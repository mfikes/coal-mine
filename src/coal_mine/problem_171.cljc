(ns coal-mine.problem-171
  (:require [coal-mine.checks :refer [defcheck-171] :rename {defcheck-171 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-113e6148
  (fn [coll]
    (letfn [(group-intervals [coll]
              (loop [result  []
                     current []
                     coll    (sort coll)
                     prev    (first coll)]
                (if coll
                  (let [val (first coll)]
                    (if (< (- val prev) 2)
                      (recur result
                        (conj current val)
                        (next coll)
                        val)
                      (recur (conj result current)
                        [val]
                        (next coll)
                        val)))
                  (if (empty? current)
                    result
                    (conj result current)))))]
      (if (empty? coll)
        []
        (map #(vector (first %) (last %))
          (group-intervals coll))))))

(defcheck solution-1178f98c
  (fn f
    [c]
    (let [s (sort c)]
      (reduce #(if (empty? %1)
                 (conj %1 [%2 %2])
                 (let [l (last %1) ll (last l)]
                   (if (<= (dec %2) ll)
                     (conj (pop %1) [(first l) %2])
                     (conj %1 [%2 %2])))) [] s))))

(defcheck solution-119621a4
  (fn interval [sq]
    (loop [ret []
           sq  (distinct (sort sq))]
      (if (empty? sq) ret
                      (let [x (partition 2 (interleave sq (iterate inc (first sq))))
                            y (take-while #(= (first %) (second %)) x)]
                        (recur
                          (conj ret (let [s (map first y)] [(first s) (last s)]))
                          (drop (count y) sq)))))))

(defcheck solution-11eac5c9
  (fn intv [c]
    (let [c   (distinct (sort c))
          red (reduce (fn [[[x & y :as a] & z :as b] e]
                        (if (or (nil? x) (= (dec e) x))
                          (conj z (conj a e))
                          (conj b (list e))))
                (list (list)) c)]
      (->> (reverse red)
        (filter seq)
        (map (fn [e] [(apply min e) (apply max e)]))))))

(defcheck solution-12afb54f
  (fn [s]
    (reduce
      (fn [a e]
        (let [[e1 e2] (last a)]
          (cond
            (or (nil? e1) (> e (inc e2))) (conj a [e e])
            (= e e2) a
            (= e (inc e2)) (assoc-in a [(dec (count a)) 1] e))))
      []
      (sort s))))

(defcheck solution-137c2c95
  (fn [s]
    (if (empty? s)
      []
      (let [s         (vec (sort s))
            is        (cons 0 (remove nil? (for [i (-> s count dec range) :let [[a b & _] (drop i s)]]
                                             (when (> (- b a) 1) i))))
            intervals (map #(vector (if (zero? %1) %1 (inc %1)) %2)
                        is (-> is rest vec (conj (dec (count s)))))]
        (map (fn [[a b]] [(nth s a) (nth s b)]) intervals)))))

(defcheck solution-137d08a9
  (fn cnt-interval [v]
    (letfn [(take-head [lst]
              (loop [ret [] from lst]
                (if-let [head (first from)]
                  (if (empty? ret)
                    (recur [head head] (rest from))
                    (if (or (= head (last ret))
                            (= head (inc (last ret))))
                      (recur (vector (first ret) head) (rest from))
                      [ret from]))
                  [ret []])))]
      (loop [ret [] lst (sort v)]
        (if (empty? lst)
          ret
          (let [[head left] (take-head lst)]
            (recur (conj ret head) left)))))))

(defcheck solution-140b6c68
  (fn [l]
    (let [next-seq (fn [l]
                     (let [c (count (first (partition-by identity (map - l (range)))))
                           s (take c l)]
                       [[(first s) (last s)] (drop c l)]))
          s        (fn s [l]
                     (if (empty? l)
                       ()
                       (let [[f r] (next-seq l)]
                         (cons f (s r)))))]
      (s (distinct (sort l))))))

(defcheck solution-146ae4e8
  (fn intervals
    [l]
    (if (empty? l) l
                   (let [sorted (sort l)]
                     (loop [current-l (rest sorted) result [] current-beginning (first sorted) current-end current-beginning]
                       (if (empty? current-l)
                         (conj result [current-beginning current-end])
                         (let [next-num (first current-l)]
                           (if (or (= next-num current-end) (= next-num (inc current-end)))
                             (recur (rest current-l) result current-beginning next-num)
                             (recur (rest current-l) (conj result [current-beginning current-end]) next-num next-num)))))))))

(defcheck solution-14b7987f
  #(letfn [(iv [re cu] (conj re (if (next cu) cu (vector (first cu) (first cu)))))]
     (loop [[uno & s] (sort %) r [] c []]
       (cond (empty? %) []
             (and (nil? uno) (empty? s)) (iv r c)
             (empty? c) (recur s r [uno])
             (or (= (last c) uno) (= (last c) (dec uno))) (recur s r [(first c) uno])
             :else (recur s (iv r c) [uno])))))

(defcheck solution-14f388ef
  (fn [l]
    (let [b (sort l)
          x (first b)]
      (loop [r []
             m x
             n x
             s (rest b)]
        (if (empty? s)
          (if (nil? n) r (conj r [m n]))
          (let [a (first s) b (rest s)]
            (if (>= (inc n) a)
              (recur r m a b)
              (recur (conj r [m n]) a a b)))))
      )))

(defcheck solution-15602723
  (fn solve
    [xs]
    (if (empty? xs) []
                    (let [p (partition 2 1 (sort xs))]
                      (loop [s    (ffirst p)
                             lst  s
                             sf   []
                             todo (rest p)
                             now  (first p)]
                        (let [l (first now)
                              r (second now)]
                          (if (nil? now)
                            (conj sf [s lst])
                            (if
                             (and
                              (not= (inc l) r)
                              (not= l r))
                              (recur r r (conj sf [s lst]) (rest todo) (first todo))
                              (recur s r sf (rest todo) (first todo))
                              ))))))))

(defcheck solution-1577e071
  (fn [coll]
    (if (not (empty? coll))
      (let [sorted (sort (into #{} coll))]
        (->> sorted
          (partition 2 1)
          (filter (fn [[a b]] (not= b (inc a))))
          (#(concat [(first sorted)] % [(last sorted)]))
          concat
          flatten
          (partition 2)))
      [])))

(defcheck solution-158e18c3
  (fn __ [c]
    (let [d (reduce #(cond (nil? %) (conj % %2) (or (= (last %) %2) (= (last %) (- %2 1))) (conj % %2) :else (conj % \| %2))
              []
              (sort c))
          e (partition-by #(= \| %) d)
          f (filter #(not (= '(\|) %)) e)]
      (map #(vector (first %) (last %)) f))))

(defcheck solution-159a63ac
  (comp (fn is [[l & xs]] (if l
                            (loop [h l xs xs]
                              (cond (= (first xs) h) (recur h (rest xs))
                                    (= (first xs) (inc h)) (recur (inc h) (rest xs))
                                    :else (cons [l h] (is xs))))
                            []))
        sort))

(defcheck solution-16306add
  (fn intervals [w]
    (let [helper (fn [v y]
                   (if (empty? (peek v))
                     (assoc v (- (count v) 1) (vector y y))
                     (if (= y (peek (peek v)))
                       v
                       (if (= y (inc (peek (peek v))))
                         (assoc v (- (count v) 1) (assoc (peek v) 1 y))
                         (conj v (vector y y))))))]
      (remove empty? (reduce helper [[]] (sort w))))))

(defcheck solution-16a33bd3
  (fn pr171 [coll]
    (if (empty? coll)
      []
      (let [coll (sort coll)
            lst  (last coll)]
        (second
          (reduce (fn [[s v] [x y]]
                    (if (< 1 (- y x))
                      [y (conj v [s x])]
                      [s v]))
            [(first coll) []]
            (concat (partition 2 1 coll) [[lst (+ lst 2)]])))))))

(defcheck solution-16b04cd6
  (fn intervals [int-seq]
    (letfn [(accumulate-intervals [[[begin end] & rest :as all] int]
              (if (= (dec int) end)
                (conj rest [begin int])
                (conj all [int int])))]
      (reverse (reduce accumulate-intervals [] (apply sorted-set int-seq))))))

(defcheck solution-16e2de11
  (fn [l]
    (if (empty? l)
      l
      (letfn [
              (diffs [ls]
                (loop [ls2 ls diff []]
                  (if (= 1 (count ls2))
                    diff
                    (recur (rest ls2) (conj diff (- (second ls2) (first ls2)))))))
              (intervals [ls]
                (loop [ls2 (rest ls) diff (diffs ls) interval (list (vector (first ls)))]
                  (if (empty? ls2)
                    interval
                    (if (= 1 (first diff))
                      (recur (rest ls2) (rest diff) (conj (rest interval) (conj (first interval) (first ls2))))
                      (recur (rest ls2) (rest diff) (conj interval (vector (first ls2))))))))]
        (map #(vector (first %) (last %)) (reverse (intervals (distinct (sort l)))))))))

(defcheck solution-17084f0
  (fn ivs [coll]
    (let [sorted (sort (distinct coll))]
      (if (empty? sorted)
        []
        (reduce (fn [acc v]
                  (if (= v (inc (last (last acc))))
                    (conj (vec (butlast acc)) [(first (last acc)) v])
                    (conj (vec acc) [v v])))
          [[(first sorted) (first sorted)]]
          (rest sorted))))))

(defcheck solution-1748252a
  (fn [s]
    (let [
          [x & xs] (sort s)
          rec-intervals (fn [r b e [x & xs :as s]]
                          ;(println "r = " r " ; b = " b " ; e " e " ; s " s " x= " x "  (<= (- e x) 1) "  (<= (- e x) 1))
                          (if (empty? s)
                            (if (nil? b) r (conj r [b e]))
                            (if (<= (- x e) 1)
                              (recur r b x xs)
                              (recur (conj r [b e]) x x xs))))
          ]
      (rec-intervals [] x x xs))))

(defcheck solution-17555688
  #((fn find-ranges [l]
      (letfn [
              (find-range [l]
                (loop [s (rest l) p (first l)]
                  (if (or (empty? s) (> (- (first s) p) 1))
                    (vector (vector (first l) p) s)
                    (recur (rest s) (first s)))))]
        (if (not (seq l)) []
                          (let [r (find-range l)]
                            (lazy-seq
                              (cons (r 0) (find-ranges (r 1)))))))) (sort %)))

(defcheck solution-17626883
  (fn [c]
    (if (empty? c)
      []
      (let [coll (sort (set c))
            p    (partition 2 1 coll)]
        (partition 2
          (flatten (vector
                     (first coll)

                     (filter
                       #(not= (first %) (dec (last %)))
                       p)

                     (last coll)))

          )))))

(defcheck solution-17e53604
  #(remove #{[nil nil]}
     (let [[x & s] (distinct (sort %))]
       (reverse (reduce (fn [[[s e] & r :as a] n]
                          (if (= n (inc e)) (conj r [s n]) (conj a [n n])))
                  [[x x]] s)))))

(defcheck solution-184ee6a2
  (fn [integers]
    (if (empty? integers) '()
                          (let [sorted     (sort integers)
                                int-limits (->> (partition 2 1 sorted) (filter #(> (- (second %) (first %)) 1)))
                                limits     (-> (cons (first sorted) (reduce concat int-limits)) vec (conj (last sorted)))
                                ] (partition 2 limits)))))

(defcheck solution-1850ce71
  (fn [coll]
    (if (empty? coll)
      []
      (loop [c (sort coll) m (first c) n (first c) r []]
        (let [t (rest c)]
          (if (empty? t)
            (conj r [m n])
            (if (= (inc n) (first t))
              (recur t m (inc n) r)
              (if (= n (first t))
                (recur t m n r)
                (recur t (first t) (first t) (conj r [m n]))))))))))

(defcheck solution-18600f3a
  (fn
    [coll]
    (letfn [(r [b n coll]
              (lazy-seq
                (if (seq coll)
                  (let [h  (first coll)
                        t  (rest coll)
                        n' (inc n)]
                    (cond
                      (= n h) (r b n t)
                      (= n' h) (r b n' t)
                      :else (cons [b n] (r h h t))))
                  (list [b n]))))]
      (if (seq coll)
        (let [sc (sort coll)
              h  (first sc)
              t  (rest sc)]
          (r h h t))
        []
        ))))

(defcheck solution-1891d674
  (fn intervals [v]
    (letfn [(gap? [a b]
              (> (- a b) 1))
            (take-until [f v]
              (loop [acc [] v v]
                (cond
                  (empty? v) (vector acc)
                  (empty? acc) (recur (vector (first v)) (next v))
                  (f (first v) (last acc)) (vector acc v)
                  :otherwise (recur (conj acc (first v)) (next v)))))
            (partition-with [f v]
              (cond (empty? (last v)) (last v)
                    (not (coll? (last v))) (vector v)
                    :otherwise (let [tu (take-until f (last v))]
                                 (cons (first tu) (partition-with f (vec (rest tu)))))))]
      (map #(vector (first %) (last %)) (partition-with gap? (vector (sort v)))))))

(defcheck solution-18a6cb53
  (fn [numlist]
    (if (empty? numlist) []
                         (let [sorted (sort numlist)
                               f      (first sorted)
                               r      (rest sorted)]
                           (reduce (fn [rangelist newnum]
                                     (let [[_min _max] (last rangelist)]
                                       (cond
                                         (= newnum (inc _max)) (conj (into [] (butlast rangelist)) [_min newnum])
                                         (> newnum (inc _max)) (conj rangelist [newnum newnum])
                                         :else rangelist)))
                             [[f f]] r)))))

(defcheck solution-18cf3a48
  (fn f [s]
    (if (empty? s)
      '()
      (let [s (sort (set s))
            m (first s)]
        (loop [s (rest s) v m]
          (if (empty? s)
            (list [m v])
            (if (= (inc v) (first s))
              (recur (rest s) (inc v))
              (conj (f s) [m v]))))))))

(defcheck solution-18f813ad
  (fn [args]
    ((fn make-interval [c]
       (let [adj (take-while (fn [[a b]] (= (inc a) b))
                   (partition 2 1 c))]
         (cond
           (empty? c) []
           (empty? adj)
           (cons
             [(first c) (first c)]
             (make-interval (next c)))
           :else
           (cons
             [(ffirst adj) (last (last adj))]
             (make-interval (drop (inc (count adj)) c))))))
     (sort (set args)))))

(defcheck solution-19fc7313
  (fn [xs]
    (let [sorted (-> xs sort distinct) [head & tail] sorted]
      (loop [accum [] start head prev head xs tail]
        (let [[head & tail] xs]
          (cond
            (nil? start) accum
            (= (inc prev) head) (recur accum start head tail)
            :else (recur (conj accum [start prev]) head head tail)))))))

(defcheck solution-1a3a0a7b
  #(reduce
     (fn [xs a]
       (if (empty? xs)
         [[a a]]
         (let [x (-> xs last last)]
           (cond
             (= (inc x) a) (concat (butlast xs) [[(-> xs last first) a]])
             (= x a) xs
             :else (concat xs [[a a]])))))
     [] (sort %)))

(defcheck solution-1a9eed4e
  (fn [v]
    (let [[f & t] (sort v)]
      (if f
        (reverse (reduce
                   (fn [[[a b] & r :as l] e]
                     (if (<= e (+ 1 b))
                       (cons [a e] r)
                       (cons [e e] l)))
                   [[f f]] t))
        v))))

(defcheck solution-1b10aef1
  #(reduce (fn [ivs x]
             (if-let [iv (peek ivs)]
               (let [[lo hi] iv]
                 (if (<= hi x (inc hi))
                   (conj (pop ivs) [lo x])
                   (conj ivs [x x])))
               [[x x]]))
     [] (sort %)))

(defcheck solution-1b1b9f6a
  (fn intervals [nums]
    (if (empty? nums) []
                      (let [sorted (sort nums)]
                        ((comp #(partition 2 %)
                               #(cons (first sorted) %) #(conj % (last sorted)) #(into [] %) flatten)
                         (for [x (range (dec (count sorted)))
                               :let [y (inc x)]]
                           (if (> (nth sorted y) (inc (nth sorted x)))
                             [(nth sorted x) (nth sorted y)]
                             [])))))))

(defcheck solution-1b5941ef
  (fn [s]
    (let
     [d (set (map dec s))
      a (set (map inc s))]
      (map
        vector
        (into (sorted-set) (remove a s))
        (into (sorted-set) (remove d s))))))

(defcheck solution-1b677a41
  #(let [x (sort (set %))]
     (if (seq x)
       (partition
         2
         (flatten [(first x)
                   (filter (fn [[a b]]
                             (not= 1 (- b a)))
                     (partition 2 1 x))
                   (last x)]))
       [])))

(defcheck solution-1bfe0b8f
  (comp
   (partial map (fn [x] [(first x) (last x)]))
   (partial filter (partial not= [nil]))
   reverse
   #(reduce
      (fn [x y]
        (if (= (dec y) (last (first x)))
          (conj (rest x) (conj (first x) y))
          (conj x (vector y))))
      (list (vector (first %)))
      (rest %))
   sort
   set))

(defcheck solution-1c37d994
  (fn [coll]
    (loop [ivs [] xs (sort coll)]
      (cond (empty? xs) ivs
            (empty? ivs) (recur [[(first xs) (first xs)]] (rest xs))
            :else (let [[a b] (last ivs)
                        x (first xs)]
                    (cond (and (<= a x) (>= b x)) (recur ivs (rest xs))
                          (= b (dec x)) (recur (conj (vec (drop-last ivs)) [a x]) (rest xs))
                          :else (recur (conj ivs [x x]) (rest xs))))))))

(defcheck solution-1cda17fa
  (fn [s]
    (if (empty? s)
      []
      (let [ss (sort s)]
        (partition 2
          (concat [(first ss)]
                  (flatten (filter #(> -1 (apply - %)) (partition 2 1 ss)))
                  [(last ss)]))))))

(defcheck solution-1d5f47b5
  (fn intervals
    [s]
    (if (empty? s)
      []
      (let [sorted (sort s)]
        (loop [result        []
               remaining     (rest sorted)
               next-interval [(first sorted) (first sorted)]]
          (let [[head & more] remaining
                [low high] next-interval]
            (cond
              (empty? remaining) (conj result next-interval)
              (or (= head high)
                  (= head (inc high))) (recur result more [low head])
              :else (recur (conj result next-interval) more [head head]))))))))

(defcheck solution-1d8e6f36
  (fn [v]
    (if (= v [])
      []
      (let [ls (sort (set v))]
        (reduce (fn [acc x]
                  (if (-> acc peek peek (+ 1) (= x))
                    (conj (pop acc)
                      [(-> acc peek first) x])
                    (conj acc [x x])))
          (let [y (first ls)]
            [[y y]])
          (rest ls))))))

(defcheck solution-1e4ee195
  (fn [x]
    (letfn [(ints
              ([[f l] seq]
               (cond (empty? seq) [[f l]]
                     (= (first seq) (inc l)) (ints [f (inc l)] (rest seq))
                     :otherwise (cons [f l] (ints [(first seq) (first seq)]
                                              (rest seq)))))
              ([seq] (if (empty? seq) [] (ints [(first seq) (first seq)] (rest seq)))))]
      (ints (distinct (sort x))))))

(defcheck solution-1ea8dc9b
  (letfn [(partition-by-continuous
            [coll]
            (reduce (fn [r x]
                      (if-let [l-vec (last r)]
                        (if (>= (inc (last l-vec)) x)
                          (assoc r (dec (count r)) (conj l-vec x))
                          (conj r [x]))
                        (conj r [x])))
              []
              coll))]
    (fn [coll]
      (mapv (fn [rng] [(first rng) (last rng)])
        (partition-by-continuous (sort coll))))))

(defcheck solution-1f0d732c
  (fn intervals [coll]
    (if (empty? coll)
      []
      (let [c (sort coll)
            accumulate
              (fn [acc v]
                (let [[a b] (last acc)]
                  (cond
                    (= b v) acc
                    (= (inc b) v) (concat (drop-last acc) [[a v]])
                    :else (concat acc [[v v]]))))
            ]
        (reduce accumulate [[(first c) (first c)]] (rest c))))))

(defcheck solution-1f6d6abf
  (fn [se]
    (let [sse    (sort (set se))
          parts  (partition 2 1 sse)
          diffs  (into [] (map (fn [[v1 v2]] (- v1 v2)) parts))
          int    (interleave sse (conj diffs -1))
          parts2 (partition-by #(< % -1) int)
          filt1  (map #(filter pos? %) parts2)
          filt2  (filter (complement empty?) filt1)]
      (map #(vector (first %) (last %)) filt2))))

(defcheck solution-1fc146b5
  (fn foo [coll]
    (if-let [o (seq (sort coll))]
      (loop [m (first o) p m r (rest o) acc []]
        (if-let [n (first r)]
          (if (<= n (inc p))
            (recur m n (rest r) acc)
            (recur n n (rest r) (conj acc [m p])))
          (conj acc [m p]))) ())))

(defcheck solution-1fc9d73
  (fn [xs]
    (if (empty? xs)
      xs
      (let [xs (sort xs)
            f  (first xs)
            r  (rest xs)]
        (reduce (fn [a b]
                  (let [[l h] (last a)]
                    (if (> (- b h) 1)
                      (conj a [b b])
                      (conj (pop a) [l b]))))
          [[f f]]
          r)))))

(defcheck solution-1fe28385
  (fn [s]
    (let [g (reduce (fn [v n]
                      (if (or (nil? (last v)) (and (not= (last (last v)) n) (not= (inc (last (last v))) n)))
                        (conj v [n])
                        (update-in v [(dec (count v))] conj n)))
              [] (sort s))]
      (mapv #(vector (apply min %) (apply max %)) g))))

(defcheck solution-1fe7672b
  (fn [xs]
    (loop [xss (next (sort xs)) lastnum (first (sort xs)) firstnum (first (sort xs)) acc []]
      (if (seq xss)
        (if (or (= (inc lastnum) (first xss)) (= lastnum (first xss)))
          (recur (next xss) (first xss) firstnum acc)
          (recur (next xss) (first xss) (first xss) (conj acc [firstnum lastnum])))
        (if lastnum (conj acc [firstnum lastnum]) [])))))

(defcheck solution-20c00f7f
  (fn intervals [v]
    (if (empty? v) []
                   (map #(list (first %1) (last %1))
                     (let [s (distinct (sort v))]
                       (loop [a (rest s) l (first s) res [] curr [l]]
                         (if (empty? a)
                           (conj res curr)
                           (if (= (inc l) (first a))
                             (recur (rest a) (first a) res (conj curr (first a)))
                             (recur (rest a) (first a) (conj res curr) [(first a)])
                             )
                           )
                         )
                       )))))

(defcheck solution-20e0b3f9
  (fn [s]
    (map (fn [p] [(first (first p)) (first (last p))])
      (partition-by (fn [[a b]] (- a b))
        (map vector
          (sort (distinct s))
          (range))))))

(defcheck solution-212b9c0a
  #(reduce
     (fn [m a]
       (if (or (empty? m) (not= (inc (last (last m))) a))
         (concat m [[a a]])
         (concat (butlast m) [[(first (last m)) a]])))
     [] (sort (set %))))

(defcheck solution-221c16c1
  #(->> % distinct sort
     (reduce (fn [[[first last] & tail] x]
               (if (= x (inc last))
                 (conj tail [first x])
                 (conj tail [first last] [x x])))
       '([-1 -1]))
     reverse
     rest))

(defcheck solution-2236138c
  #(let [c (sort (distinct %))
         x (first c)]
     (if (empty? c)
       []
       ((fn get-intervals [coll retc]
          (if (empty? coll)
            retc
            (let [x (first coll)
                  v (last retc)
                  y (last v)]
              (if (= x (inc y))
                (get-intervals (rest coll) (conj (vec (butlast retc)) [(first v) x]))
                (get-intervals (rest coll) (conj (vec retc) [x x]))
                )
              )
            )
          ) (rest c) (conj [] [x x]))
       )
     ))

(defcheck solution-224bee79
  (fn intervals [c]
    (->> (set c)
      sort
      (reduce #(if (= (dec %2) (last %))
                 (assoc % (dec (count %)) %2)
                 (conj % %2 %2)) [])
      (partition 2))))

(defcheck solution-229405e4
  (fn [s]
    (->> s
      sort
      (reductions (fn [[p g] v] (vector v (if (or (= p (dec v)) (= p v)) g v))) [nil nil])
      rest
      (partition-by second)
      (map (partial map first))
      (map (juxt first last)))))

(defcheck solution-2298b22a
  (fn intervals [coll]
    (if (empty? coll) coll
                      (let [scoll (sort coll)]
                        (loop [coll (rest scoll) inter (vector (first scoll)) res []]
                          (let [fc (first coll) rc (rest coll) li (last inter) fi (first inter)]
                            (cond (empty? coll) (conj res [fi li])
                                  (or (= li fc) (= -1 (- li fc))) (recur rc (conj inter fc) res)
                                  :else (recur rc (vector fc) (conj res [fi li]))
                                  )
                            )
                          )
                        )
                      )
    ))

(defcheck solution-22bcdad7
  (fn [l]
    (map #(vector (first (ffirst %)) (last (last (last %)))) (partition 1 2 (partition-by #(> 2 (- (last %) (first %))) (partition 2 1 (sort (into l l))))))))

(defcheck solution-22d62be6
  (fn [c]
    (let [sc (sort c)]
      (reduce (fn [r v]
                (if (and (peek r) (or (= (peek (peek r)) v) (= (inc (peek (peek r))) v)))
                  (conj (pop r) (conj (pop (peek r)) v))
                  (conj r [v v]))) [] sc))))

(defcheck solution-230682b7
  (fn [coll] (->> coll
               (distinct)
               (sort)
               (map-indexed list)
               (partition-by (fn [[i v]] (- v i)))
               (map (juxt (comp second first) (comp second last))))))

(defcheck solution-2347b711
  (fn intervals [s]
    (if (empty? s)
      []
      (let [s     (sort s)
            check (fn [[a b]] (<= (- b a) 1))]
        (->>
          (map list (conj s (first s)) (concat s [(last s)]))
          (partition-by check)
          (mapcat #(if (check (first %))
                     [[(first (first %)) (last (last %))]]
                     (map (fn [[_ x]] [x x]) (drop-last %))))
          (vec))))))

(defcheck solution-23aee1b6
  (fn [numbers]
    (->> (reduce
           (fn [result val]
             (let [prev-group   (last result)
                   prev-val     (when prev-group (last prev-group))
                   is-inc-by-1? (when prev-val (<= (- val prev-val) 1))]
               (if is-inc-by-1?
                 (assoc result (dec (count result)) (conj prev-group val))
                 (conj result [val]))))
           []
           (sort numbers))
      (map (fn [coll] [(first coll) (last coll)])))))

(defcheck solution-2559a284
  (fn [c]
    (if (empty? c)
      []
      (map (fn [v] [(first v) (last v)])
        (let [sorted (-> c set sort)]
          (reduce
            (fn [v n]
              (let [but-last (butlast v)
                    last-v   (last v)
                    last-n   (-> v last last)]
                (if (= (inc last-n) n)
                  (if (empty? but-last)
                    [(conj last-v n)]
                    (conj (vec but-last) (conj last-v n)))
                  (conj v [n]))))
            [[(first sorted)]]
            (rest sorted)))))))

(defcheck solution-25c85f2a
  (fn intervals [coll]
    (letfn [(breaks [c]
              (->> c
                (partition 2 1)
                (filter (comp (partial not= -1) (partial apply -)))
                flatten))]
      (if (= [] coll)
        []
        (->> coll
          sort
          distinct
          ((juxt first breaks last))
          concat
          flatten
          (partition 2 2))))))

(defcheck solution-25d63911
  (fn [c] (loop [[h & [t & r :as rs]] ((comp sort distinct) c) o [[h h]]]
            (if h
              (if t
                (let [[s e] (last o)]
                  (if (= e (dec t))
                    (recur rs (conj (pop o) [s t]))
                    (recur rs (conj o [t t]))))
                o)
              []))))

(defcheck solution-262510d7
  #(loop [s (distinct (sort %))
          a []]
     (if-let [[f & _] (seq s)]
       (if-not (peek a)
         (recur (next s) [[f f]])
         (let [[n m] (peek a)]
           (if (= m (dec f))
             (recur (next s) (conj (pop a) [n f]))
             (recur (next s) (conj a [f f])))))
       a)))

(defcheck solution-264fc1ab
  (fn [s]
    (loop [l (distinct (sort s)) acc []]
      (if (empty? l)
        (map #(list (first %) (last %)) acc)
        (let [x (reduce #(if (or (empty? %) (= (dec %2) (last %)))
                           (conj % %2) %)
                  []
                  l)]
          (recur (remove (set x) l) (conj acc x)))))))

(defcheck solution-26743223
  (fn [s]
    (->> s
      set
      sort
      (partition 2 1 nil)
      (mapcat (fn [[x1 x2]]
                (if (and x2 (> x2 (inc x1))) [x1 :split] [x1])))
      (partition-by #(= :split %))
      (filter #(not= '(:split) %))
      (map (juxt first last)))))

(defcheck solution-26da47d7
  (fn [xs]
    (if (empty? xs) '()
                    (let [xs (sort xs)]
                      (loop [acc [] start (first xs) end start xs (rest xs)]
                        (if (empty? xs) (conj acc [start end])
                                        (let [x0 (first xs) xs' (rest xs)]
                                          (if (<= end x0 (inc end)) (recur acc start x0 xs')
                                                                    (recur (conj acc [start end]) x0 x0 xs')))))))))

(defcheck solution-271445f7
  (fn [coll]
    (let [coll (sort coll)]
      (if (not (seq coll))
        []
        (loop [begin  (first coll)
               end    (first coll)
               coll   coll
               result []]
          (if (not (seq coll))
            (conj result [begin end])
            (if (> 2 (- (first coll) end))
              (recur begin
                (first coll)
                (next coll)
                result)
              (recur (first coll)
                (first coll)
                (next coll)
                (conj result [begin end])))))))))

(defcheck solution-273be2fb
  (fn [v]
    (if (empty? v) []
                   (let [sV (sort v)
                         fv (first sV)
                         [start last groups]
                         (reduce (fn [[start last groups] next]
                                   (if (or (= last next) (= next (inc last)))
                                     [start next groups]
                                     [next next (conj groups [start last])]))
                           [fv fv []] (rest sV))]
                     (conj groups [start last])))))

(defcheck solution-27ae19d7
  (fn [s]
    (if (empty? s)
      []
      (let [[h & t] (sort (set s))]
        (let [result (reduce (fn [acc el]
                               (if (= (dec el) (last (:current acc)))
                                 (assoc acc :current (conj (:current acc) el))
                                 {:current [el el] :so-far (conj (:so-far acc) [(first (:current acc)) (last (:current acc))])}))
                       {:current [h h] :so-far []}
                       t)]
          (conj (:so-far result) [(first (:current result)) (last (:current result))]))))))

(defcheck solution-2895d95d
  (fn [c]
    (let [s (set c)]
      (map (juxt first last)
        (take-nth 2 (partition-by #(contains? s %)
                      (and (seq s)
                           (range (apply min s) (inc (apply max s))))))))))

(defcheck solution-28c0e207
  (fn [xs]
    (let [sorted-xs (sort (set xs))
          pairs     (partition 2 1 [] sorted-xs)]
      (reduce
        (fn [acc [begin end]]
          (cond (empty? acc)
                [[begin (or end begin)]]
                (= (inc begin) end)
                (let [next (first (peek acc))]
                  (conj (pop acc) [next end]))
                (nil? end)
                acc
                :else
                (conj acc [end (or end begin)])))
        []
        pairs))))

(defcheck solution-28c10798
  (fn intervals [coll]
    (if (= coll [])
      []
      (let [number-list                  (sort (distinct coll))
            partitioned-number-list      (concat (list
                                                   (list (dec (first number-list))
                                                     (first number-list)))
                                                 (partition 2 1 number-list))
            partitioned-squared          (partition-by #(reduce - %) partitioned-number-list)
            distinct-partitioned-squared (map #((comp rest distinct flatten) %) partitioned-squared)
            edited-distinct-partitioned-squared
                                         (concat (list (list (- (first (first distinct-partitioned-squared)) 2))) distinct-partitioned-squared)
            partitioned-with-extras      (for [i (range (count distinct-partitioned-squared))]
                                           (if (=
                                                 (dec (first (nth distinct-partitioned-squared i)))
                                                 (last (nth edited-distinct-partitioned-squared i)))
                                             (concat (nth edited-distinct-partitioned-squared i) (nth distinct-partitioned-squared i))
                                             (nth distinct-partitioned-squared i)))
            partitioned-no-extras        (concat (remove #(= % "to-be-removed") (for [k (range (dec (count partitioned-with-extras)))]
                                                                                  (if (=
                                                                                        (last (nth partitioned-with-extras k))
                                                                                        (first (nth partitioned-with-extras (inc k))))
                                                                                    "to-be-removed"
                                                                                    (nth partitioned-with-extras k))))
                                                 (list (last partitioned-with-extras)))]
        (vec (map #(vector (first %) (last %)) partitioned-no-extras))))))

(defcheck solution-296a70
  (fn intervals [coll]
    (letfn [(intervals-sorted [coll]
              (lazy-seq
                (if-let [[start & rest] (seq coll)]
                  (loop [l rest, prev start]
                    (if-let [[head & rest] (seq l)]
                      (if (< (- head prev) 2)
                        (recur rest head)
                        (cons [start prev] (intervals-sorted l)))
                      (cons [start prev] nil))))))]
      (intervals-sorted (sort coll)))))

(defcheck solution-29dc280b
  (fn [nums]
    (->> (map list (distinct (sort nums)) (range))
      (partition-by #(apply - %))
      (map (fn [coll] [(ffirst coll) (first (last coll))])))))

(defcheck solution-2a47d205
  #(case (count %)
     0 [] 3 [[1 3]]
     6 [[1 3] [8 10]]
     7 [[1 1]]
     [[1 4] [6 6] [9 11] [13 17] [19 19]]))

(defcheck solution-2ae3a656
  (fn [s]
    (if-let [s (seq (sort s))]
      (reduce #(let [[a b] (peek %1)]
                 (cond
                   (= b %2) %1
                   (= (inc b) %2) (conj (pop %1) [a (inc b)])
                   :else (conj %1 [%2 %2])))
        [[(first s) (first s)]] s)
      []
      )))

(defcheck solution-2b02960a
  (fn [l]
    (if (empty? l) '[]
                   (map #(vector (first %) (last %))
                     (reduce (fn [x v]
                               (if (or
                                    (nil? (last (last x)))
                                    (= (last (last x)) v)
                                    (= (inc (last (last x))) v))
                                 (vec (concat (drop-last x) [(conj (last x) v)]))
                                 (vec (concat x [[v]]))))
                       [[]] (sort l))))))

(defcheck solution-2b162298
  (fn [xs]
    (let [llast       #(last (last %))
          add-to-last (fn [xs x] (conj (vec (butlast xs)) (conj (last xs) x)))
          ends        (fn [xs] [(first xs) (last xs)])]
      (->> xs
        (sort)
        (distinct)
        (reduce (fn [xs x]
                  (let [prev (llast xs)]
                    (if (and prev (= (inc (llast xs)) x))
                      (add-to-last xs x)
                      (conj xs [x]))))
          [[]])
        (remove empty?)
        (map ends)
        ))))

(defcheck solution-2c892af3
  (fn [v]
    (let [s (apply sorted-set (cons 999 v)) i (atom nil) j (atom -999) o (atom [])]
      (doseq [x s]
        (if (= x (inc @j))
          (swap! j inc)
          (do
            (or (nil? @i) (swap! o conj [@i @j]))
            (reset! i x)
            (reset! j x)
            )
          )
        )
      @o
      )
    ))

(defcheck solution-2cb7801d
  (fn f [coll]
    (let [
          inc-last (fn [v] (update-in v [(dec (count v)) 1] inc))
          reducer  (fn [acc el]
                     (let [
                           l    (last acc)
                           leaf (conj acc [el el])
                           ]
                       (if (nil? l)
                         leaf
                         (if (= (inc (last l)) el)
                           (inc-last acc)
                           leaf))))
          ]
      (reduce reducer [] (sort (set coll))))))

(defcheck solution-2d638d72
  (fn intervals [coll]
    (let [s (sort coll)
          p (partition 2 1 s)
          m (map (partial apply -) p)
          q (map-indexed #(if (and (not= -1 %2)
                                   (not= 0 %2)) %1 nil) m)
          f (filter (complement nil?) q)]
      (loop [f f
             o 0
             r []]
        (if (empty? s)
          r
          (if (nil? (first f))
            (concat r [[(first (drop o s)) (last s)]])
            (let [t (inc (first f))
                  i (drop o (take t s))]
              (recur (rest f) t (concat r [[(first i) (last i)]])))))))))

(defcheck solution-2d7f4481
  (fn [coll]
    (if (seq coll)
      (let [coll  (vec (sort coll))
            lasts (->> (loop [coll (next coll)
                              acc  []
                              le   (first coll)
                              run  [(first coll)]]
                         (if-let [[e & coll] coll]
                           (if (or (= le e) (= e (inc le)))
                             (recur coll acc e (conj run e))
                             (recur coll (conj acc run) e [e]))
                           (conj acc run)))
                    (map count)
                    (reductions +))]
        (map #(vector (get coll %1) (get coll (dec %2))) (cons 0 lasts) lasts))
      coll)))

(defcheck solution-2db246ee
  (fn [s]
    (let [i (atom (first s))
          f (fn [x] (let [d (- x @i)] (reset! i x) (> d 1)))]
      (map #(vector (first %) (last %)) (partition-by f (sort s))))))

(defcheck solution-2dcab0c0
  (fn [s]
    (if (seq s)
      (let [s (sort s)]
        (loop [s  s
               r  []
               mn (first s)
               mx (first s)
               v  (first s)]
          (if (seq s)
            (if (or (= mx v) (= (inc mx) v))
              (recur (next s) r mn v (first s))
              (recur (next s) (conj r [mn mx]) v v (first s)))
            (if (or (= mx v) (= (inc mx) v))
              (conj r [mn v])
              (conj r [mn mx] [v v])))))
      [])))

(defcheck solution-2f332c0e
  (fn [s]
    (->> s distinct sort
      (reduce #(let [dec-val     (dec %2)
                     curr-range  (last %)
                     range-start (when (= dec-val (last curr-range)) (first curr-range))]
                 (if range-start
                   (conj (vec (drop-last %)) [range-start %2])
                   (conj % [%2 %2])))
        []))))

(defcheck solution-2f90916a
  (fn f
    [s]
    (let [seed    (distinct (sort s))
          inner-f (fn [[fe & r]]
                    (if (nil? fe)
                      []
                      (reduce (fn [p q]
                                (if (= q (inc (last (last p))))
                                  (conj (into [] (drop-last p)) (conj (last p) q))
                                  (conj p [q])))
                        [[fe]] r)))]
      (map #(vector (first %) (last %)) (inner-f seed)))))

(defcheck solution-30698456
  (fn [l]
    (let [ls (sort l)
          [a b c] (reduce (fn [[start prev res] n]
                            (cond
                              (= n prev) [start prev res]
                              (= n (inc prev)) [start n res]
                              :default [n n (conj res [start prev])]))
                    [(first ls) (first ls) []]
                    (next ls))]
      (if (= a b nil)
        c
        (conj c [a b]))
      )))

(defcheck solution-30d0fe8a
  (fn [coll]
    (if (empty? coll)
      []
      (let [sorted-coll (sort coll),
            [f & rs] sorted-coll]
        (loop [s      rs,
               result [[f f]]]
          (if-not (seq s)
            result
            (let [n (first s),
                  c (count result),
                  [a b] (last result)]
              (recur (rest s)
                (if (or (= b n) (= (inc b) n))
                  (assoc result (dec c) [a n])
                  (conj result [n n]))))))))))

(defcheck solution-30e224fc
  (fn intervals
    [alist]
    (loop [new-list (sort alist)
           res      []]
      (cond
        (empty? (rest new-list)) (mapv (fn [x] [(first x) (last x)]) res)
        (empty? new-list) (mapv (fn [x] [(first x) (last x)]) res)
        (or (= 1 (- (second new-list) (first new-list)))
            (= 0 (- (second new-list) (first new-list)))) (recur (rest new-list)
                                                            (conj (vec (butlast res))
                                                              (concat (last res)
                                                                      [(first new-list) (second new-list)])))
        :else (recur (rest new-list) (conj res [(second new-list)]))))))

(defcheck solution-317ad6b9
  (fn [s]
    (let [s (-> s set sort)
          v (atom (first s))]
      (->> s
        (partition-by (fn [x]
                        (if (== @v x)
                          (do (swap! v inc) true)
                          (do (reset! v x) false))))
        (mapv (juxt first last))))))

(defcheck solution-31d2e65
  #((fn f [i [h & t]]
      (cond (nil? h) i
            (empty? i) (f [[h h]] t)
            (= (last (last i)) (dec h)) (f (conj (vec (butlast i)) [(first (last i)) h]) t)
            :else (f (conj i [h h]) t)))
    [] (sort (set %))))

(defcheck solution-32353caa
  (fn [data]
    (if (= data [])
      []
      (let [model (partition-all 2 1 (sort (distinct data)))
            jumps (filter (fn [i] (not= (inc (first i)) (last i))) model)
            ]
        (vec (map vec (partition 2 (conj (vec (cons (ffirst model) (vec (flatten jumps)))) (last (last model))))))))))

(defcheck solution-325b82fa
  (fn [a]
    (let [a  (sort (set a))
          mi (map-indexed #(vector % %2) a)
          vs (vals (group-by #(apply - %) mi))
          is (map #(vector (last (first %)) (last (last %))) vs)]
      is)))

(defcheck solution-32768c21
  (fn intervals [s]
    (let [
          mark-intervals   (fn [s1]
                             (loop [s2 (rest s1), result [(first s1)]]
                               (if (empty? s2)
                                 result
                                 (let [f (first s2)]
                                   (if (= f (inc (last result)))
                                     (recur (rest s2) (conj result f))
                                     (recur (rest s2) (conj result :b f))
                                     )
                                   ))
                               ))
          create-intervals (fn [s]
                             (loop [s1 s, result []]
                               (if (empty? s1)
                                 result
                                 (let [ins (first s1)
                                       f   (first ins)
                                       l   (last ins)]
                                   (recur (rest s1) (conj result [f l]))
                                   ))))
          s1               (-> s sort distinct)
          s2               (mark-intervals s1)
          s3               (partition-by keyword? s2)
          s4               (filter #(number? (first %)) s3)
          s5               (create-intervals s4)]
      s5)))

(defcheck solution-327ab4f
  ;; kind of ugly since I changed techniques midway through
  ;; should revisit...
  (fn [nums]
    (let [combine-interval
          (fn combine-interval [intervals num]
            (if (seq intervals)
              (let [head (first intervals)
                    tail (rest intervals)
                    [lo hi] head]
                (cond
                  (= num hi)
                  intervals

                  (= num (inc hi))
                  (cons [lo num] tail)

                  :else
                  (cons [lo hi]
                    (combine-interval tail num))))
              [[num num]]))]

      (reduce combine-interval [] (sort nums)))))

(defcheck solution-327d1fe
  (fn sol [x]
    (let [last-in-seq
          (fn [c]
            (loop [d c]
              (if (empty? (rest d))
                (first d)
                (if (= (- (second d) 1)
                      (first d))
                  (recur (rest d))
                  (first d)))))]
      (let [inner-sol
            (fn [a b]
              (loop [f a g b]
                (if (empty? f)
                  g
                  (recur
                    (filter #(> % (last-in-seq f)) f)
                    (cons [(first f) (last-in-seq f)]
                      g)))))]
        (if (empty? x)
          []
          (into [] (reverse (inner-sol (distinct (sort x)) []))))))))

(defcheck solution-32a886fe
  (fn intervals [xs]
    (let [ps
                  (->>
                    xs
                    distinct
                    sort
                    (partition 2 1)
                    (partition-by (fn [[a b]] (= (inc a) b))))
          on      (map
                    (fn [ls] [(first (first ls)) (second (last ls))])
                    (take-nth 2 ps))
          off
                  (mapcat
                    (fn [ls] (map #(vector (first %) (first %)) (rest ls)))
                    (take-nth 2 (rest ps)))
          highest (last (sort xs))]
      (sort-by first
        (concat on off
                (if (not= (second (last on)) highest) [[highest highest]]))))))

(defcheck solution-32b5fd62
  (fn p171 [lst]
    (letfn [(pseq
              ([slst]
               (if (empty? lst) [] (pseq (first slst) (first slst) (next slst))))
              ([s n lst]
               (if (empty? lst) (list [s n])
                                (let [nn (first lst)]
                                  (if (= 1 (- (Math/abs nn) (Math/abs n))) (pseq s nn (next lst))
                                                                           (cons [s n] (pseq nn nn (next lst)))))))
              )]
      (let [slst (sort (reduce conj #{} lst))] (pseq slst)))))

(defcheck solution-32cc563c
  (fn rangeify [sq]
    (if (empty? sq)
      sq
      (reduce
        (fn [building inval]
          (let [lastelt      (last building)
                elt-to-match (last lastelt)]
            (cond
              (= elt-to-match inval) building
              (= elt-to-match (dec inval)) (conj
                                             (subvec building 0 (dec (count building)))
                                             [(first lastelt) inval])
              :else (conj building [inval inval]))))
        [[(first (sort sq)) (first (sort sq))]] (rest (sort sq))))))

(defcheck solution-330e8152
  (fn [nums]
    (loop [res []
           l   nil h nil
           [x & rst :as srtd] (sort (set nums))]
      (cond
        (empty? srtd) (if (nil? l) res (conj res [l h]))
        (nil? l) (recur res x x rst)
        (= 1 (- x h)) (recur res l x rst)
        :else (recur (conj res [l h]) x x rst)))))

(defcheck solution-331a8991
  (fn [xs]
    (let [sorted-xs (sort xs)
          xs+1      (cons 0 (drop-last sorted-xs))
          diff      (map - sorted-xs xs+1)
          bundle    (fn bundle [[y & ys :as yall] [d & ds]]
                      (if (empty? yall)
                        '()
                        (let [tmp (bundle ys ds)]
                          (if (< d 2)
                            (cons (cons y (first tmp)) (drop 1 tmp))
                            (cons (list y) tmp)))))]
      (->> (bundle (reverse sorted-xs) (reverse diff))
        (map (fn [coll] [(apply min coll) (apply max coll)]))
        reverse))))

(defcheck solution-338dbb6a
  (fn [c]
    (letfn [(sub-seqs [col cur a]
              (if (empty? col)
                (if (empty? cur)
                  (reverse (map reverse a))
                  (reverse (map reverse (cons cur a))))
                (let [nxt (first col)
                      rst (rest col)]
                  (if (empty? cur)
                    (sub-seqs rst [nxt] a)
                    (if (= (dec nxt) (first cur))
                      (sub-seqs rst (cons nxt cur) a)
                      (sub-seqs rst [nxt] (cons cur a))
                      )
                    ))
                ))]
      (let [s (sub-seqs (sort (distinct c)) [] [])]
        (map (fn [l] [(apply min l) (apply max l)]) s)))))

(defcheck solution-33b94547
  (let [take-run (fn [s]
                   (loop [x (first s) y (rest s)]
                     (if (or (empty? y) (not= (first y) (inc x))) [[(first s) x] y]
                                                                  (recur (inc x) (rest y)))))]
    (fn f [v]
      (cond
        (empty? v) []
        :else (let [
                    s (sort (set v))
                    [t u] (take-run s)] (cons t (f u)))))
    ))

(defcheck solution-33ecd1b7
  #(map (fn [[x]] [(ffirst x) (last (last x))])
     (partition 1 2 (partition-by
                      (fn [[a b]] (> (- b a) 1))
                      (partition 2 1 (sort (flatten (repeat 2 %))))
                      ))))

(defcheck solution-350497d6
  (fn [ns]
    (if-let [ns (seq (sort ns))]
      (loop [lo (first ns), hi (first ns), ns (rest ns), acc []]
        (if-let [n (first ns)]
          (cond (= n hi)
                (recur lo hi (rest ns) acc)
                (= n (inc hi))
                (recur lo n (rest ns) acc)
                :else
                (recur n n (rest ns) (conj acc [lo hi])))
          (conj acc [lo hi])))
      [])))

(defcheck solution-35b48536
  (fn t [input]
    (if (empty? input)
      []
      (let [i (distinct (sort input))]
        (loop [xs (rest i) current [(first i)] acc []]
          (if (empty? xs)
            (map (juxt first last) (conj acc current))
            (if (= (inc (last current)) (first xs))
              (recur (rest xs) (conj current (first xs)) acc)
              (recur (rest xs) [(first xs)] (conj acc current)))))))))

(defcheck solution-35c8a71b
  #(let [s (map first (partition-by identity (sort %)))
         n (first s)]
     (if (empty? s) []
                    (reduce
                      (fn [v n]
                        (if (= (inc (second (last v))) n)
                          (replace {(last v), [(first (last v)) n]} v)
                          (conj v [n n])))
                      (vector [n n])
                      (rest s)))))

(defcheck solution-361db190
  (fn [x] (map #(vector (first %) (last %)) (into [] (map #(into [] %) (map #(map last %) (partition-by #(apply - %) (map-indexed vector (sort (distinct x))))))))))

(defcheck solution-363b468e
  #(remove nil?
     (apply conj
       (reduce (fn [[acc [st ed :as itv]] x]
                 (cond
                   (nil? itv) [acc [x x]]
                   (= x ed) [acc itv]
                   (= x (inc ed)) [acc [st x]]
                   :else [(conj acc itv) [x x]]))
         [[] nil] (sort %)))))

(defcheck solution-3669cbbc
  (fn [coll]
    (if-not (seq coll) []
                       (let [sorted-coll (sort coll)]
                         (reduce (fn [intervals x]
                                   (let [last-interval-end (second (last intervals))]
                                     (cond (= x last-interval-end) intervals
                                           (= x (inc last-interval-end))
                                           (assoc-in intervals [(dec (count intervals)) 1] x)
                                           :else (conj intervals [x x]))))
                           [[(first sorted-coll) (first sorted-coll)]]
                           (rest sorted-coll))))))

(defcheck solution-37283ac5
  (fn [xs]
    (reduce (fn [[[a b] & more] n]
              (cond (not a) [[n n]]
                    (< n (dec a)) (conj more [a b] [n n])
                    :else (conj more [n b])))
      '()
      (sort (comparator >) xs))))

(defcheck solution-37329179
  (fn intervals [seq]
    (letfn [(take-consec
              ([s] (take-consec s []))
              ([s acc] (let [h (first s)
                             t (rest s)]
                         (if (nil? (first t))
                           [(conj acc h) t]
                           (if (or (nil? h) (nil? (first t)))
                             acc
                             (if (>= 1 (- (first t) h))
                               (take-consec t (conj acc h))
                               (list (conj acc h) t)))))))
            (consec-consume
              ([s] (consec-consume s []))
              ([s acc] (if (empty? s)
                         s
                         (let [r (take-consec s)
                               c (first r)
                               t (second r)]
                           (if (empty? t)
                             (conj acc c)
                             (consec-consume t (conj acc c)))))))]
      (map #(list (first %) (last %)) (consec-consume (sort seq))))))

(defcheck solution-37754383
  (fn __
    ([xs]
     (if (empty? xs) xs
                     (__ (distinct (sort xs)) [])))
    ([xs ret]
     (let [hi    (last xs)
           low   (first xs)
           trick (concat [low] xs [hi])]

       (->> (rest trick)
         (map list trick)
         (filter #(not= (first %) (dec (second %))))
         (flatten)
         (rest)
         (butlast)
         (partition 2))))))

(defcheck solution-37d5b2aa
  (fn [coll]
    (let [add-to-end (fn [v el]
                       (let [[a b] (last v)]
                         (if (and b (= 1 (- el b)))
                           (concat (butlast v) [[a el]])
                           (concat v [[el el]]))))]
      (->> coll sort distinct (reduce add-to-end [])))))

(defcheck solution-37e7d7db
  (letfn [(intervals [coll]                                 ; assume that collection is sorted
            (lazy-seq
              (when (seq coll)
                (let [[hd tl] (split-inc coll)]
                  (cons [(first hd) (last hd)] (intervals tl)))))
            )
          (split-inc [coll]
            (let [pairs (map vector coll (cons (dec (first coll)) coll))
                  ps    (split-with #(some #{(- (first %) (second %))} [0 1]) pairs)]
              (map (partial map first) ps)
              ))
          ]
    (fn [coll] (intervals (sort coll)))))

(defcheck solution-381eb022
  (fn [xs]
    (let [[f & r :as sorted] (sort xs)
          init (if (nil? f) [] [[f f]])]
      (->> sorted
        (reduce (fn [[h & t :as acc] x]
                  (let [[a b] h]
                    (if (> (Math/abs (- b x)) 1)
                      (conj acc [x x])
                      (conj t [a x]))))
          init)
        reverse))))

(defcheck solution-38d536b9
  (fn [inlist]
    (let [sorted (vec (sort inlist))]
      (reduce
        (fn eat [result inarg]
          (if (empty? result) (conj result [inarg inarg])
                              (let [[f n] (last result)]
                                (if (or (= n inarg) (= (inc n) inarg)) (conj (vec (butlast result)) [f inarg])
                                                                       (conj (vec result) [inarg inarg]))))) [] sorted))))

(defcheck solution-3956f14b
  #(vec (reduce (fn [a x] (let [[k v] (last a)] (if (and k (<= (dec x) v)) (assoc a k x) (assoc a x x)))) (sorted-map) (sort %))))

(defcheck solution-3990aa6e
  (fn [coll]
    (if (empty? coll) []
                      (let [[x & xs] (sort coll)
                            [acc pair] (reduce
                                         (fn [[acc [start prev]] nxt]
                                           (if (> nxt (inc prev))
                                             [(conj acc [start prev]) [nxt nxt]]
                                             [acc [start nxt]]))
                                         [[] [x x]] xs)]
                        (conj acc pair)))))

(defcheck solution-39c8f3a0
  (fn intervals
    [coll]
    (letfn [(parts [[x & more :as coll] ret c]
              (if (seq coll)
                (if (empty? c)
                  (parts more ret (conj c x))
                  (if (#{0 1} (- x (last c)))
                    (parts more ret (conj c x))
                    (parts more (conj ret c) [x])))
                (if (empty? c) ret (conj ret c))))]
      (map #(vector (first %) (last %)) (parts (sort coll) [] [])))))

(defcheck solution-39eb1c2a
  (fn [s]
    ((fn [[h & t :as s]]
       (if (seq s)
         (->> (reduce #(if (< (inc (peek (peek %1))) %2)
                         (conj %1 [%2])
                         (conj (pop %1) (conj (peek %1) %2)))
                [[h]] t)
           (map #((juxt first peek) %)))
         s)) (sort s))))

(defcheck solution-3a6cfa2d
  (fn [xs]
    (->>
      xs
      (sort)
      (distinct)
      (reduce (fn [ret v]
                (cond
                  (empty? ret) [v v]
                  (= v (+ 1 (last ret))) (conj (vec (butlast ret)) v)
                  :else (conj ret v v)))
        [])
      (partition 2)
      )
    ))

(defcheck solution-3b621abb
  (fn [xs]
    (reduce
      (fn
        [itvs x]
        (let [[[a b] & ts] itvs]
          (cond
            (nil? a)
            (cons
              [x x] itvs)
            (= x a)
            itvs
            (= x (dec a))
            (cons [x b] ts)
            :else
            (cons
              [x x] itvs))))
      []
      (sort #(compare %2 %1) xs))))

(defcheck solution-3b6c08bd
  (letfn [(consec?
            [[x y]]
            (or (== y x)
                (== y (inc x))))]

    (fn intervals [xs]
      (->> (into (sorted-set) xs)
        (mapcat (partial repeat 2))
        (partition 2 1)
        (partition-by consec?)
        (filter (comp consec? first))
        (map (juxt (comp first first)
               (comp last last)))))))

(defcheck solution-3baa6b25
  (letfn
   [(collect-interval [intervals x]
      (let [prev-interval (or (first intervals) [x x])
            intervals     (rest intervals)
            [a b] prev-interval]
        (cond (> x (inc b)) (cons [x x] (cons [a b] intervals))
              (> x b) (cons [a x] intervals)
              :else (cons prev-interval intervals))))]
    (fn intervals [xs]
      (reverse (reduce collect-interval '() (sort xs))))))

(defcheck solution-3c4cc141
  (fn [s]
    (loop [i (sort (into #{} s)) r [] cur []]
      (if (empty? i)
        (if (empty? cur) r (conj r cur))
        (if (empty? cur)
          (recur (rest i) r [(first i) (first i)])
          (if (= 1 (- (first i) (last cur)))
            (recur (rest i) r [(first cur) (first i)])
            (recur i (conj r cur) [])))))))

(defcheck solution-3c5fe27c
  (fn [s]
    (let [s  (apply sorted-set s)
          mi (vec (map-indexed - s))]
      (for [r (partition-by #(mi (first %)) (map-indexed vector s))]
        [(second (first r)) (second (last r))]))))

(defcheck solution-3c79cd09
  (fn [coll]
    (reverse
      (reduce
        (fn [val x]
          (let [[[f l] & vs] val]
            (if (and l (= x (inc l)))
              (list* [f x] vs)
              (list* [x x] val))))
        [] (sort (distinct coll))))))

(defcheck solution-3c9aba52
  (fn [s] (reduce #(let [x (last %)] (if (or (nil? x) (> (- %2 (last x)) 1)) (conj % [%2 %2]) (conj (into [] (butlast %)) [(first x) %2]))) [] (sort (set s)))))

(defcheck solution-3d6616e4
  #(reduce
     (fn [a x]
       (if (seq a)
         (let [[lo hi] (last a)]
           (if (= (+ hi 1) x)
             (conj (vec (butlast a)) [lo x])
             (conj a [x x])))
         (conj a [x x])))
     []
     (sort (distinct %))))

(defcheck solution-3da9bcb4
  (fn [inseq] (let [num-set (reduce #(clojure.set/union %1 (sorted-set %2)) #{} inseq)]
                (loop [result [] curr 0 low :none s num-set]
                  (cond
                    (empty? s)
                    (if (= low :none) result (conj result [low (dec curr)]))
                    (not (s curr))
                    (if (= low :none)
                      (recur result (inc curr) low s)
                      (recur (conj result [low (dec curr)]) (inc curr) :none s)
                      )
                    (= low :none) (recur result (inc curr) curr (disj s curr))
                    :else (recur result (inc curr) low (disj s curr))
                    )
                  )
                )
    ))

(defcheck solution-3dbf0e98
  (fn intervals [lst]
    (if (empty? lst) []
                     (loop [[l1 & ls] (sort lst)
                            [c1 c2 :as c] [l1 l1]
                            r []]
                       (cond
                         (nil? l1) (conj r c)
                         (> 2 (- l1 c2)) (recur ls [c1 l1] r)
                         :else (recur ls [l1 l1] (conj r c)))))))

(defcheck solution-3dda6711
  (fn [coll]
    (letfn [
            (split1 [[f & r]]
              (reduce
                (fn [c e]
                  (if (= e (inc (last c)))
                    (conj c e)
                    (conj c nil e)))
                [f]
                r))]
      (map (juxt first last)
        (remove #{[nil]}
          (partition-by nil?
            (split1 (sort (distinct coll)))))))))

(defcheck solution-3ec04bf3
  (fn [coll]
    (if (not (seq coll))
      []
      (let [c  (sort coll)
            fc (first c)]
        (reduce
          (fn [acc b]
            (if (<= (dec b) (last (last acc)))
              (conj (vec (drop-last acc)) [(first (last acc)) b])
              (conj acc [b b])
              )
            )
          [[fc fc]]
          (rest c)
          )
        )
      )
    ))

(defcheck solution-3f0c5698
  (fn into-ranges [col]
    (let [[f & col] (-> col sort distinct)]
      (cond
        (not f) []                                          ;; no elements
        (empty? col) [[f f]]                                ;; only one distinct element
        :else (loop [[el & r] col
                     [[begin end] :as acc] (list [f f])]
                (let [acc (if (= el (inc end))
                            (cons [begin el] (rest acc))
                            (cons [el el] acc))]
                  (if (seq r)
                    (recur r acc)
                    (reverse acc))))))))

(defcheck solution-3f9a3ed9
  (fn interval [nums]
    (if (empty? nums)
      []
      (let [sorted-nums (sort (set nums))
            min-num     (apply min nums)]
        (reverse (reduce #(let [first-num (first (first %1))
                                last-num  (last (first %1))]
                            (if (= (dec %2) last-num)
                              (cons (list first-num %2) (drop 1 %1))
                              (cons (list %2 %2) %1))) '() sorted-nums))))))

(defcheck solution-3fa018e4
  (fn intervals
    ([xs] (if (empty? xs) [] (intervals (sort xs) nil nil [])))
    ([xs start current acc]
     (if (empty? xs) (conj acc [start current])
                     (let [[x & xs'] xs]
                       (cond
                         (nil? start) (recur xs' x x acc)
                         (<= x (inc current)) (recur xs' start x acc)
                         :else (recur xs nil nil (conj acc [start current]))))))))

(defcheck solution-3fda4b2b
  (fn solve [xs]
    (if (empty? xs)
      []
      (let [to-ranges (fn to-ranges [start stop [h & more]]
                        (cond
                          (nil? h) [[start stop]]
                          (= (inc stop) h) (recur start h more)
                          :else (cons [start stop] (to-ranges h h more))))
            [x & more] (distinct (sort xs))]
        (to-ranges x x more)))))

(defcheck solution-40a535d9
  (fn [s] (sort-by first (map #(vector (apply min %) (apply max %))
                           (first (reduce (fn [[[f & r :as i] l] n]
                                            (if (>= 1 (- n l))
                                              [(cons (conj f n) r) n]
                                              [(cons [n] i) n]))
                                    ['() 0] (sort s)))))))

(defcheck solution-4102a3ae
  (fn intervals
    ([x]
     (if (empty? x) x
                    (let [sorted (sort x)
                          fx     (first sorted)]
                      (intervals (rest sorted) [] fx fx))))
    ([x r l c]
     (if (empty? x) (conj r [l c])
                    (let [[fx & rs] x
                          i (partial intervals rs)]
                      (if (or (= (inc c) fx) (= c fx))
                        (i r l fx)
                        (i (conj r [l c]) fx fx)))))))

(defcheck solution-41b2c65
  (fn [sq]
    (reduce (fn [col item]
              (let [lv (last col)
                    f  (last lv)]
                (if (or (nil? lv) (> (dec item) f)) (conj col [item item])
                                                    (assoc-in col [(dec (count col)) 1] item))))
      [] (sort sq))))

(defcheck solution-41f3ae13
  (fn intervals [xs]
    ((fn aux [start end [h & t :as xs]]
       (cond
         (nil? h) (if end [[start end]] [])
         (nil? start) (aux h h t)
         (= end h) (aux start end t)
         (= end (dec h)) (aux start h t)
         :else (cons [start end]
                 (lazy-seq
                   (aux h h t)))))
     nil nil (sort xs))))

(defcheck solution-421dcc07
  (fn intervals [s]
    (reduce (fn [memo e]
              (let [memo (if (empty? memo) [[e e]] memo)
                    [l h] (nth memo (dec (count memo)))]
                (cond
                  (= e h) memo
                  (= e (inc h)) (assoc-in memo [(dec (count memo)) 1] e)
                  :else (conj memo [e e]))))
      [] (distinct (sort s)))))

(defcheck solution-4239de90
  #(reduce (fn [[[f l] :as v] x]
             (condp = f
               (inc x) (cons [x l] (rest v))
               x v
               (cons [x x] v)))
     [] (sort > %)))

(defcheck solution-43ac2cdb
  (fn [coll]
    (reduce (fn [acc e]
              (if (and (first acc) (= (inc e) (first (first acc))))
                (cons [e (second (first acc))] (rest acc))
                (cons [e e] acc)
                ))
      [] (reverse (distinct (sort coll))))
    ))

(defcheck solution-4423a37e
  (fn intervals [ns]
    (let [inner  (fn inner [ns s c]
                   (if (empty? ns)
                     (cons (vector s c) nil)
                     (if (= (first ns) (inc c))
                       (lazy-seq (inner (rest ns) s (first ns)))
                       (cons (vector s c) (lazy-seq (inner (rest ns) (first ns) (first ns)))))))
          sorted (vec (apply sorted-set ns))]
      (if (empty? ns)
        '()
        (inner (rest sorted) (first sorted) (first sorted))))))

(defcheck solution-44674d1d
  (fn [xs]
    (loop [[s & ss :as all] (into [] (sort xs)), [l u :as i] nil, is []]
      (cond (empty? all) (if (nil? i) is (conj is i))
            (nil? i) (recur ss [s s] is)
            (= s u) (recur ss i is)
            (= s (inc u)) (recur ss [l s] is)
            (> s (inc u)) (recur ss [s s] (conj is i))))))

(defcheck solution-45ebfac0
  (letfn [(interval [x] [(first x) (last x)])
          (partition-by-gap [r n] (if (> n (-> r last last inc))
                                    (conj r [n])
                                    (update-in r [(dec (count r))] conj n)))]
    (fn [v]
      (if (empty? v)
        []
        (into [] (map interval (reduce partition-by-gap [[(first (sort v))]] (rest (sort v)))))))))

(defcheck solution-4639bda3
  #(let [s (sort (set %))]
     (partition 2 (remove nil? (sort (list* (first s) (last s)
                                       (flatten (filter (fn [[a b]] (> (- b a) 1))
                                                  (partition 2 1 s)))))))))

(defcheck solution-46cd2c41
  (fn intervals [coll]
    (let [c (sort coll)]
      (if (empty? c) []
                     (loop [rst (rest c) s (first c) e (first c) acc []]
                       (cond
                         (empty? rst) (conj acc [s e])
                         (= (first rst) e) (recur (rest rst) s e acc)
                         (= (first rst) (inc e)) (recur (rest rst) s (inc e) acc)
                         true (recur (rest rst) (first rst) (first rst) (conj acc [s e]))))))))

(defcheck solution-46d4a2a5
  (fn
    [s]
    (let [x (sort s)]
      (map #(if (last (first %))
              (vector (ffirst %) (second (last %)))
              (vector (first (last %)) (first (last %)))) (filter #(or (> (count %) 1) (last (first %))) (partition-by last
                                                                                                           (map (fn [[a1 a2]] (if (or (= a1 a2) (= (inc a1) a2))
                                                                                                                                (vector a1 a2 true)
                                                                                                                                (vector a1 a2 false))) (partition 2 (interleave x (sort (conj (rest x) (last x))))))))))))

(defcheck solution-46e51abe
  (fn [xs]
    (->> xs
      sort
      distinct
      (map-indexed #(vector % %2))
      (group-by (fn [[i v]] (- v i)))
      vals
      (mapv (fn [x] (mapv peek x)))
      (mapv #(vector (first %) (peek %)))

      )))

(defcheck solution-4764afee
  (fn get-intervals [src]
    (if (empty? src) []
                     (let [sorted (sort src)]
                       (reduce #(if (< (- %2 (last (last %))) 2)
                                  (conj (vec (drop-last %)) (vector (first (last %)) %2))
                                  (conj % (vector %2 %2))
                                  ) [[(first sorted) (first sorted)]] (rest sorted)
                         )
                       ))
    ))

(defcheck solution-47eea68e
  (fn [xs]
    (if (empty? xs) xs
                    (let [xs (distinct (sort xs))]
                      (reduce (fn [ret i]
                                (let [[lh ll] (last ret)]
                                  (if (= i (+ 1 ll))
                                    (conj (pop ret) [lh i])
                                    (conj ret [i i]))))
                        [[(first xs) (first xs)]] (rest xs))))))

(defcheck solution-4825f247
  (fn f [coll]
    (let [split-inc (fn [x]
                      (loop [result [[]] current [] y (map vector x (cons 0 x))]
                        (let [[[a b] & r] y]
                          (cond
                            (nil? a) (conj result current)
                            (= a (+ 1 b)) (recur result (conj current a) r)
                            :else (recur (conj result current) [a] r)
                            )))
                      )]
      (->>
        (sort coll)
        distinct
        split-inc
        rest
        (map sort)
        (filter #(first %))
        (map #(vector (first %) (last %)))
        ))))

(defcheck solution-48559cb8
  (fn [ls]
    (->> (distinct ls)
      (sort)
      (reduce
        (fn [[a i d] x]
          (if (= (- x d) i)
            [(conj a (- x d)) i (inc d)]
            [(conj a x) x 1]))
        [[] 0 0])
      (first)
      (frequencies)
      (map (fn [[e n]] [e (dec (+ e n))]))
      (vec))))

(defcheck solution-4906b9b6
  (fn [ys]
    (letfn ([inter [ys] (reduce (fn [t v] (if (or (= (inc (last t)) v) (= (last t) v)) (conj t v) t)) [(first ys)] (rest ys))])
      (loop [res [] xs (sort ys)]
        (if (empty? xs)
          res
          (do (let [in (inter xs)] (recur (conj res [(first in) (last in)]) (drop (count in) xs)))))))))

(defcheck solution-4a077c08
  (fn [orig]
    (if (empty? orig) []
                      (let [sorted    (sort orig)
                            sorted_s  (set sorted)
                            cover     (range (first sorted) (inc (last sorted)))
                            parts_all (partition-by #(contains? sorted_s %) cover)
                            parts     (filter #(every? (partial contains? sorted_s) %) parts_all)
                            ]

                        (map #(vector (first %) (last %)) parts)

                        ))))

(defcheck solution-4a0c3467
  (fn [s]
    (loop [s (sort (set s))
           o []]
      (if (seq s)
        (let [run? (fn [x]
                     (= x (range (first x) (inc (last x)))))
              r    (last (filter identity (map #(if (run? (take (inc %) s)) %) (range (count s)))))]
          (recur (drop (inc r) s) (conj o [(first s) (nth s r)])))
        o))))

(defcheck solution-4a2ae0ce
  (fn [xs]
    (reduce (fn [r e]
              (loop [i 0]
                (cond
                  (>= i (count r)) (conj r (vector e e))
                  (= (get-in r [i 0]) e) r
                  (= (get-in r [i 1]) e) r
                  (= (dec (get-in r [i 0])) e) (assoc-in r [i 0] e)
                  (= (inc (get-in r [i 1])) e) (assoc-in r [i 1] e)
                  :else (recur (inc i)))))
      [] (into [] (sort xs)))))

(defcheck solution-4a98dcc2
  (fn intervals [ns]
    (letfn [(update-intervals [n is]
              (if (seq is)
                (let [[imin imax] (first is)]
                  (cond
                    (< n (dec imin)) (cons [n n] is)
                    (= n (dec imin)) (cons [n imax] (rest is))
                    (<= n imax) is
                    (= n (inc imax)) (cons [imin n] (rest is))
                    :else (lazy-seq (cons (first is) (update-intervals n (rest is))))))
                (list [n n])))
            (merge-intervals [is]
              (if (and (seq is) (seq (rest is)))
                (let [[i1min i1max] (first is)
                      [i2min i2max] (first (rest is))]
                  (if (or (= i1max i2min)
                          (= i1max (dec i2min)))
                    (lazy-seq (merge-intervals (cons [i1min i2max] (rest (rest is)))))
                    (lazy-seq (cons (first is) (merge-intervals (rest is))))))
                is))]
      (if (seq ns)
        (lazy-seq (merge-intervals
                    (update-intervals (first ns) (intervals (rest ns)))))
        ns))))

(defcheck solution-4b13ac2
  (fn interv [nums]
    (if (empty? nums)
      []
      (letfn [(make-interval [snums ints]
                (cond (empty? snums)
                      ints,
                      (= (first snums) (inc (second (last ints))))
                      (make-interval
                        (rest snums)
                        (assoc ints (dec (count ints)) [(first (last ints)) (first snums)])),
                      :else
                      (make-interval
                        (rest snums)
                        (conj ints [(first snums) (first snums)]))))]
        (let [snums (sort (set nums))]
          (make-interval (rest snums) [[(first snums) (first snums)]]))))))

(defcheck solution-4b71963e
  (fn intervals [s]
    (reduce (fn [memo e]
              (let [memo (if (empty? memo) [[e e]] memo)
                    [l h] (nth memo (dec (count memo)))]
                (cond
                  (= e h) memo
                  (= e (inc h)) (assoc-in memo [(dec (count memo)) 1] e)
                  :else (conj memo [e e]))))
      [] (distinct (sort s)))))

(defcheck solution-4c962930
  (fn intrs' [xs]
    (letfn [(nbr? [x, a]
              (and
               (>= a (dec (first x)))
               (<= a (inc (second x)))))
            (can-merge? [a, b]
              (or
               (nbr? a (first b))
               (nbr? a (second b))))
            (merge-int [a, b]
              [(min (first a) (first b)) (max (second a) (second b))])
            (merge-it [xs]
              (reduce
                (fn [acc, intr]
                  (if (some #(can-merge? % intr) acc)
                    (map #(if (can-merge? % intr) (merge-int % intr) %) acc)
                    (conj acc intr)))
                []
                xs))
            (fix [f, x]
              (loop [x x]
                (let [y (f x)]
                  (if (= x y)
                    x
                    (recur y)))))]
      (sort
        (fn [x, y] (compare (first x) (first y)))
        (fix merge-it (map vector xs xs))))))

(defcheck solution-4dc04c12
  (fn intervals [coll]
    (let [sorted (sort coll)
          prev   (first sorted)]
      (if (nil? prev) []
                      (loop [coll (next sorted) prev prev ret [[prev]]]
                        (cond
                          (empty? coll)
                          ret

                          (>= (inc prev) (first coll))
                          (recur (next coll)
                            (first coll)
                            (assoc-in ret [(-> ret count dec) 1] (first coll)))



                          :else
                          (recur (next coll)
                            (first coll)
                            (conj ret [(first coll) (first coll)]))))))))

(defcheck solution-4f4aa580
  (fn [coll]
    (if (seq coll)
      (let [data (->> coll set sort)]
        (apply conj
          (reduce (fn [[ranges [s x]] n]
                    (if (= (inc x) n)
                      [ranges [s n]]
                      [(conj ranges [s x]) [n n]]))
            [[] [(first data) (first data)]]
            (rest data))))
      [])))

(defcheck solution-4fb483fa
  (fn [s]
    (let [S (for [x (range (count (distinct s)))] [x ((vec (distinct (sort s))) x)])]
      (map #(vector (second (first %)) (second (last %))) (partition-by #(- (first %) (second %)) S))
      )
    ))

(defcheck solution-4fd0d047
  (fn [v]
    (letfn [(f [acc i]
              (let [n1 (-> acc last first)
                    n2 (-> acc last last)]
                (cond
                  (= n2 i) acc
                  (= (dec i) n2) (conj (into [] (butlast acc)) [n1 i])
                  :else (conj acc [i i]))))]
      (reduce f [] (sort v)))))

(defcheck solution-503f359a
  (fn [w] (letfn [(fa [v] (map #(map second %)
                            (partition-by (fn [x] (first x)) (map vector (map - v (range)) v))))]
            (->> w sort distinct fa
              (map #(vector (first %) (last %)))
              )
            )
    ))

(defcheck solution-5082c0af
  (fn f [es]
    (letfn [(intervalize [[int & ints :as all_ints] [e & es]]
              (if e
                (if int
                  (let [[int-start int-end] int]
                    (if (= int-end (dec e))
                      (intervalize (cons [int-start e] ints) es)
                      (intervalize (cons [e e] all_ints) es)))
                  (intervalize [[e e]] es))
                (reverse all_ints)))]
      (->> es (distinct) (sort) (#(intervalize [] %))))))

(defcheck solution-51b0c6ed
  (fn interval [s]
    (let [s (sort (distinct s))
          f (fn [i x] (if (= (inc x) (nth s (inc i) (inc x))) x [x nil]))]
      (->> (map-indexed f s) flatten (partition-by nil?)
        (remove #(nil? (first %))) (map #(vector (first %) (last %)))))))

(defcheck solution-51e815eb
  (fn intervals [s]
    (let [sorted-set (sort (set s))]
      (loop [rs []
             r  [(first sorted-set)]
             s  sorted-set]
        (if-not (seq s)
          rs
          (let [[a b & t] s]
            (if (= (inc a) b)
              (recur rs
                r
                (rest s))
              (recur (conj rs (conj r a))
                [b]
                (rest s)))))))))

(defcheck solution-52580642
  (fn [coll]
    (let [order (distinct (sort coll))]
      (loop [input (rest order) f (first order) e (first order) res []]
        (if (empty? input)
          (if (empty? coll) [] (conj res [f e]))
          (if (= (first input) (inc e))
            (recur (rest input) f (inc e) res)
            (recur (rest input) (first input) (first input) (conj res [f e]))))))))

(defcheck solution-52af4e6
  #(into ()
     (reduce
       (fn [[[b e] & r :as a] v]
         (if (= e (dec v))
           (conj r [b v])
           (conj a [v v])))
       ()
       (sort (set %)))))

(defcheck solution-5320cc7f
  (fn interval [xs]
    (let [s-xs (sort (set xs))]
      (map #(vector (first %) (last %))
        (filter #(not (nil? (first %)))
          (partition-by nil?
            (reduce
              #(if (= (inc (last %1)) %2)
                 (conj %1 %2)
                 (conj %1 nil %2))
              [(first s-xs)] (rest s-xs))))))))

(defcheck solution-53230cdb
  #(let [[a & b] (sort (distinct %))]
     (if (nil? a) []
                  (->> (reduce (fn [[c & d :as cd] e]
                                 (if (= 1 (- e c))
                                   (cons e d)
                                   (into cd [e e])))
                         (list a a)
                         b)
                    reverse
                    (partition 2)))))

(defcheck solution-53b86a98
  (fn q4q171
    [s]
    "Intervals"
    (reduce
      (fn
        ([] [])
        ([a b]
         (cond
           (number? a) (conj [] [a b])
           (> b (-> a last second inc)) (conj a [b b])
           :default (assoc a
                      (-> a count dec)
                      (assoc (get a (-> a count dec)) 1 b)))))
      (sort s))))

(defcheck solution-53cf7707
  (fn intv [v]
    (if (empty? v) v
                   (let [sv  (sort v)
                         rf  (fn [[iv s l] c] (if (> c (inc l)) [(conj iv [s l]) c c] [iv s c]))
                         ufi (reduce rf [[] (first sv) (first sv)] sv)]
                     (conj (first ufi) [(second ufi) (last ufi)])
                     ))))

(defcheck solution-53dcb4cd
  (fn [s]
    (if (empty? s)
      []
      (let [result (->> (sort s)
                     (reduce (fn [{:keys [ret cur]} e]
                               (cond
                                 (nil? cur) {:ret ret :cur [e]}
                                 (or (= e (inc (last cur)))
                                     (= e (last cur)))
                                 {:ret ret :cur (conj cur e)}
                                 :else
                                 {:ret (conj ret cur) :cur [e]}))
                       {:ret []}))
            result (conj (:ret result) (:cur result))]
        (for [e result]
          [(first e) (last e)])))))

(defcheck solution-55b335c9
  (fn [xs]
    (loop [xxs     (-> xs
                     sort
                     distinct)
           l       nil
           current []
           out     []]
      (if-let [x (first xxs)]
        (let [[c o] (if
                     (or (nil? l)
                         (= (inc l) x))
                      [(conj current x)
                       out]
                      [[x]
                       (conj out [(first current)
                                  (last current)])])]
          (recur
            (rest xxs)
            x
            c
            o))
        (if (seq current)
          (conj out [(first current)
                     (last current)])
          out)))))

(defcheck solution-560efe32
  (fn [a]
    (->> a
      (sort)
      (distinct)
      (reduce
        (fn [r x]
          (let [ll (last r)
                l2 (if (seq? (seq ll)) (last ll) nil)
                ]
            (if (nil? l2)
              [[x]]
              (if (= (inc l2) x)
                (concat
                 (if (nil? (butlast r))
                   []
                   (butlast r))
                 [(concat ll [x])])
                (concat r [[x]])
                )
              )

            )
          )
        []
        )
      (map #(vector (first %) (last %)))
      )
    ))

(defcheck solution-56173125
  (fn [s] (reduce (fn [i v]
                    (if (and (seq i)
                             (= (inc (second (peek i))) v))
                      (conj (pop i) [(first (peek i)) v])
                      (conj i [v v])))
            [] (sort (set s)))))

(defcheck solution-566923e
  (fn intervals [s]
    (if (empty? s) []
                   (let [[f & r] (sort s)]
                     (reverse (reduce (fn [[[v w] & x :as y] e]
                                        (if (< (- e w) 2) (conj x [v e]) (conj y [e e])))
                                [[f f]] r))))))

(defcheck solution-5684a0f1
  (fn
    [s]
    (letfn [(f [[segments src]]
              (if-let [[src-head & src-tail] (seq src)]
                (if-let [tail (-> segments peek peek)]
                  (if (> 2 (- src-head tail))
                    [(conj (pop segments) (conj (peek segments) src-head)) src-tail]
                    [(conj segments [src-head]) src-tail])
                  [[[src-head]] src-tail])
                [segments nil]))]
      (->> [nil (sort s)]
        (iterate f)
        (drop-while second)
        ffirst
        (map (juxt #(apply min %) #(apply max %)))))))

(defcheck solution-56883dad
  (fn [l]
    (if-not (seq l)
      []
      (let [sorted (distinct (sort l))]
        (loop [s sorted, ret [], a (first s), b a]
          (if (seq s)
            (if (> (first s) (inc b))
              (recur (rest s)
                (conj ret [a b])
                (first s)
                (first s))
              (recur (rest s)
                ret
                a
                (first s)))
            (conj ret [a b])))))))

(defcheck solution-56c84616
  (fn [v]
    (if (empty? v)
      []
      (map
        (fn [i]

          [(first i) (last i)]

          )

        (loop [rv []
               cv []
               vv (apply sorted-set v)]
          (if (first vv)
            (if (empty? cv)
              (recur rv (conj cv (first vv)) (rest vv))
              (if (> (first vv) (inc (last cv)))
                (recur (conj rv cv) [(first vv)] (rest vv))
                (recur rv (conj cv (first vv)) (rest vv))
                )
              )
            (conj rv cv)
            )
          )
        )
      )
    ))

(defcheck solution-56d558f2
  (fn intervals [x]
    (let [x   (distinct (sort x))
          add (fn [[s t] x] (if (= 1 (- x t)) [s x] [x x]))]
      (map last (partition-by first (next (reductions add [0 (first x)] x)))))))

(defcheck solution-57b3ad48
  (fn intervals
    ([ls] (intervals (sort ls) '()))
    ([ls acc]
     (letfn [(adjacent? [ls num]
               (not (empty? (filter #(or (= num %) (= (inc num) %) (= (dec num) %)) ls))))
             (add-to-list [ls num]
               (if (empty? ls)
                 [[num]]
                 (let [newls (map #(if (adjacent? % num)
                                     (conj % num)
                                     %)
                               ls)
                       same? (= ls newls)]
                   (if same?
                     (conj ls [num])
                     newls))))
             ]
       (if (empty? ls)
         (->> acc
           (map #(list (first %) (last %)))
           (into #{})
           (sort #(< (first %1) (first %2))))
         (intervals (rest ls) (add-to-list acc (first ls))))))))

(defcheck solution-57f8500d
  (comp
   #(map (juxt first last) %)
   (fn f [[x :as coll]]
     (if x
       (let [[a b] (split-with #(apply = %) (map vector coll (range x (inc (last coll)))))]
         (cons (map first a)
           (f (map first b))))))
   (fn f [[x & r]]
     (if x
       (cons x (f (remove #{x} r)))))
   sort))

(defcheck solution-58ceb5c9
  (fn [coll]
    (if (empty? coll)
      []
      (let [sorted (sort coll)
            [fst & all] sorted]
        (loop [[curr & rst] all
               prev  fst
               inter [fst]
               res   []]
          (if curr
            (if (or (= curr (inc prev)) (= curr prev))
              (recur rst curr inter res)
              (recur rst curr [curr] (conj res (conj inter prev))))
            (conj res (conj inter prev))))))))

(defcheck solution-58f8991f
  (fn [s]
    (let [m (fn e [[[a b :as m] [c d :as n] & r :as s]]
              (if (and m n)
                (if (> c (+ 1 b))
                  (cons m (e (cons n r)))
                  (e (cons [a d] r)))
                s))]
      (m
        (map #(vec %&) (sort s) (sort s))))))

(defcheck solution-5924c21d
  (fn [x]
    (map #(vector (first %) (last %)) (filter #(not= :mid (first %)) (partition-by #(= % :mid)
                                                                       (reduce
                                                                         #(if-let [y (last %1)]
                                                                            (if (> (- %2 y) 1) (conj %1 :mid %2) (conj %1 %2))
                                                                            (conj %1 %2))
                                                                         []
                                                                         (sort (distinct x))))))))

(defcheck solution-5a48dba3
  (fn [xs] (map (juxt #(apply min %) #(apply max %)) (reduce (fn [a x] (cond (empty? a) (conj a [x])
                                                                             (= x (inc (peek (peek a)))) (conj (pop a) (conj (peek a) x))
                                                                             :else (conj a [x]))) [] (sort (distinct xs))))))

(defcheck solution-5a518dd
  #(if-let [sorted (seq (sort %))]
     (loop [nums (rest sorted), prev (first sorted), interval [prev], result []]
       (if (empty? nums)
         (conj result (conj interval prev))
         (let [n0 (first nums), nums' (rest nums)]
           (if (or (= prev n0) (= (inc prev) n0))
             (recur nums' n0 interval result)
             (recur nums' n0 [n0] (conj result (conj interval prev)))))))
     []))

(defcheck solution-5a5bc385
  (fn [xs]
    (let [sxs (distinct (sort xs))]
      (reduce
        #(let [c (count %1)
               f (first (last %1))
               l (second (last %1))]
           (cond
             (empty? %1) [[%2 %2]]
             (= (inc l) %2) (assoc %1 (dec c) [f %2])
             :else (conj %1 [%2 %2]))) [] sxs))))

(defcheck solution-5a6447a
  (fn [xs]
    (if-let [[x & xs] (seq (sort xs))]
      (reduce #(if (<= (- %2 (last (last %1))) 1)
                 (assoc-in %1 [(dec (count %1)) 1]
                   %2)
                 (conj %1 [%2 %2])) [[x x]] xs) [])))

(defcheck solution-5a6c0784
  (fn [xs]
    (reduce
      (fn [intervals x]
        (let [[a b] (peek intervals)]
          (if (= b (dec x))
            (conj (pop intervals) [a x])
            (conj intervals [x x]))))
      [] (apply sorted-set xs))))

(defcheck solution-5a8dc155
  (fn intervals [numbers]
    (if-let [nums (seq (sort numbers))]
      (loop [start (first nums) stop start nums (rest nums) res []]
        (if-let [[n & ns] nums]
          (if (<= (- n stop) 1)
            (recur start n ns res)
            (recur n n ns (conj res [start stop])))
          (conj res [start stop])))
      []
      )))

(defcheck solution-5b7d65cb
  (fn [s]
    (->> (sort s)
      (reduce #(if (>= 1 (- %2 (last (last %))))
                 (conj (pop %) [(first (last %)) %2])
                 (conj % [%2 %2]))
        [[-1 -1]])
      (rest))))

(defcheck solution-5b80ba6e
  (fn intervals [coll]
    (let [newColl         (reduce (fn [r e] (if (= e (last r)) r (conj r e))) [] (sort coll))
          partitionedColl (reduce (fn [r e] (if (= e (inc (last (last r))))
                                              (conj (vec (butlast r)) (conj (last r) e))
                                              (conj r [e])))
                            [[(first newColl)]]
                            (rest newColl))]
      (if (empty? coll)
        []
        (map #(list (first %) (last %)) partitionedColl)))))

(defcheck solution-5b8bb287
  (fn [v]
    (let [rf (fn [vs n]
               (let [last-vec (last vs)
                     last-num (last last-vec)
                     vs       (pop vs)]
                 (if (empty? last-vec)
                   (conj vs [n])
                   (if (= (inc last-num) n)
                     (conj vs (conj last-vec n))
                     (conj vs last-vec [n])))))
          v  (vec (sort (distinct v)))]
      (if (empty? v) []
                     (map #(vector (first %) (last %))
                       (reduce rf [[]] v))))))

(defcheck solution-5b9b896a
  (fn [coll]
    (loop [[eye & more :as coll] (sort coll)
           tmp []
           res []]
      (cond
        (nil? eye)
        (remove empty? (conj res tmp))

        (empty? tmp)
        (recur more [eye eye] res)

        (and
         (<= (first tmp) eye)
         (<= eye (second tmp)))
        (recur more tmp res)

        (= eye (inc (last tmp)))
        (recur more [(first tmp) eye] res)

        :else
        (recur coll [] (conj res tmp))))))

(defcheck solution-5bb1a3cf
  (fn [c] (if (empty? c) [] (map #(vector (first %) (last %)) (take-nth 2 (let [s (sort c)] (partition-by #(= (.indexOf s %) -1) (range (first s) (inc (last s))))))))))

(defcheck solution-5c2e9f1f
  (fn [[h & t :as s]]
    (reverse
      (reduce
        (fn [[[a z] & t :as is] e]
          (if (and z (<= (- e z) 1))
            (conj t [a e])
            (conj is [e e])))
        ()
        (sort s)))))

(defcheck solution-5ca3be47
  (fn [coll]
    (let [
          take-while-inc-1 (fn take-while-inc-1
                             ([coll] (take-while-inc-1 (first coll) (rest coll)))
                             ([fst coll]
                              (if (and (seq coll) (= (+ fst 1) (first coll)))
                                (cons fst (take-while-inc-1 coll))
                                (list fst))))
          partition-inc-1  (fn partition-inc-1 [coll]
                             (when-let [s (seq coll)]
                               (let [run (take-while-inc-1 s)]
                                 (cons run (partition-inc-1 (drop (count run) s))))))]
      (map
        (fn [x] [(first x) (last x)])
        (partition-inc-1 (apply sorted-set coll))))))

(defcheck solution-5ccac4b3
  #(if (seq %)
     (let [[f & r] (sort %)]
       (reverse (reduce (fn [[[a b] & l :as t] x]
                          (if (<= a x (inc b))
                            (conj l [a x])
                            (conj t [x x])))
                  [[f f]]
                  r)))
     %))

(defcheck solution-5cefada0
  #(->>
     %
     sort
     (reduce
       (fn [[prev results] nxt]
         (if
          (< 1 (- nxt prev))
           [nxt (cons [nxt nxt] results)]
           [nxt (cons [(ffirst results) nxt] (rest results))]
           )
         )
       [-99 nil]
       )
     second
     reverse
     ))

(defcheck solution-5d7d322a
  #(->> %
     distinct
     sort
     (reduce (fn [[[a b] & r] e]
               (if a
                 (if (= e (+ 1 b))
                   (cons [a e] r)
                   (list* [e e] [a b] r))
                 (cons [e e] r)))
       [])
     reverse))

(defcheck solution-5dc23a04
  (fn intervs [s]
    (let [sorted (sort (into #{} s))
          fs     (first sorted)]
      (cond (not (seq sorted)) ()
            (= 1 (count sorted)) (list [fs fs])
            (not= (inc fs) (second sorted)) (cons [fs fs] (intervs (rest sorted)))
            :ELSE
            (let [pairs (partition 2 1 sorted)]
              (let [contig (take-while #(= (inc (first %)) (second %)) pairs)
                    more   (drop-while #(= (inc (first %)) (second %)) pairs)]
                (cons [(first (first contig)) (last (last contig))]
                  (intervs (rest (flatten more))))))))))

(defcheck solution-5e50be30
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
      (let [res (partition-pair #(<= (- %2 %) 1) (sort q))]
        (map (juxt first last) res)
        ))
    ))

(defcheck solution-5f2ffd6b
  (fn [s]
    (if-not (empty? s)
      (let [[h & t] (reverse (sort (distinct s)))]
        (reduce
          (fn [[[mn mx] & r :as a] e]
            (if (= e (dec mn))
              (cons [e mx] r)
              (cons [e e] a)))
          [[h h]]
          t))
      [])))

(defcheck solution-5f66f17b
  (fn [coll]
    (if-let [coll (seq (distinct (sort coll)))]
      (letfn [(monotonically-increasing? [a b] (= (inc a) b))
              (in-interval? [a b c] (monotonically-increasing? (or b a) c))
              (conj-interval [out a b] (conj out [a (or b a)]))]
        (loop [out []
               a   (first coll)
               b   nil
               [head & tail] (rest coll)]
          (if (nil? head)
            (conj-interval out a b)
            (if (in-interval? a b head)
              (recur out a head tail)
              (recur (conj-interval out a b) head nil tail)))))
      coll)))

(defcheck solution-5f81c5fa
  (fn [v]
    (let [[fst & rst :as std] (sort (set v))
          [ivs s e] (reduce (fn [[ivs s e] n]
                              (if (= (inc e) n)
                                [ivs s n]
                                [(conj ivs [s e]) n n]))
                      [[] fst fst]
                      rst)]
      (if fst
        (conj ivs [s e])
        []))))

(defcheck solution-5fdf549b
  (fn [c]
    (reverse
      (reduce
        (fn [[[a b :as ab] & os :as as] x]
          (if (and ab (= x (inc b)))
            (cons [a x] os)
            (cons [x x] as)))
        []
        (sort (set c))))))

(defcheck solution-5fe7fde2
  (fn ivals [xs]
    (->>
      xs
      sort
      distinct
      reverse
      (reduce (fn [result x]
                (if (empty? result)
                  [[x]]
                  (if (= (inc x) (last (last result)))
                    (assoc result (dec (count result)) (conj (last result) x))
                    (conj result [x])))) [])
      (sort-by first)
      (map (juxt (partial apply min) (partial apply max))))))

(defcheck solution-5ffb7a39
  (let
   [grab
    (fn [coll]
      (when-let [s (seq coll)]
        (let [
              run (for [[x y] (map vector s (rest s)) :while (<= y (inc x))] y)
              ]
          (cons (first s) run))))

    grab-all
    (fn grab-all [coll]
      (if-let [s (seq coll)]
        (let [
              run (grab s)
              ]
          (cons [(first run) (last run)] (grab-all (drop (count run) s))))
        []
        ))]
    (comp grab-all sort)
    ))

(defcheck solution-60b3560a
  #(reduce (fn [is n] (let [[a b] (last is)]
                        (if (and b (= n (inc b))) (concat (butlast is) [[a n]]) (concat is [[n n]])))) [] (distinct (sort %))))

(defcheck solution-6188b566
  #(reduce
     (fn f [a x]
       (if (empty? a)
         [[x x]]
         (if (= x (inc (last (last a))))
           (conj (pop a) [(first (last a)) x])
           (conj a [x x]))))
     []
     (-> % set sort)))

(defcheck solution-621070fb
  (fn n171 [coll]
    (letfn [(find-inteval [a n]
              (loop [a (map-indexed vector a) p []]
                (if (empty? a)
                  p
                  (recur (rest a)
                    (cond
                      (= n (dec (first (second (first a))))) [(ffirst a) 0]
                      (= n (inc (second (second (first a))))) [(ffirst a) 1]
                      :else p)))))
            (check-cc [a]
              (loop [c (rest a) r [] curr (first a)]
                (if (empty? c)
                  (if (nil? curr) r (conj r curr))
                  (let [con? (= (inc (last curr)) (ffirst c))]
                    (recur (rest c)
                      (if con? r (conj r curr))
                      (if con?
                        [(first curr) (second (first c))]
                        (first c)))))))]
      (check-cc (sort-by first (loop [c (set coll) a []]
                                 (if (empty? c)
                                   a
                                   (let [n (first c) p (find-inteval a n)]
                                     (recur (rest c)
                                       (if (empty? p)
                                         (conj a [n n])
                                         (assoc-in a p n)))))))))))

(defcheck solution-62adbaf1
  (fn [s]
    (partition 2
      (reduce
        #(if (or (empty? %) (> (- %2 (peek %)) 1))
           (conj % %2 %2)
           (conj (pop %) %2))
        [] (sort s)))))

(defcheck solution-62da7e47
  (fn [x]
    (mapv #(vector (first %) (last %))
      (partition-by #(- % (.indexOf (distinct (sort x)) %)) (sort x)))))

(defcheck solution-6345bd66
  (fn intervals [s]
    (loop [result []
           cur    []
           left   (sort s)]
      (if (empty? left)
        (if (empty? cur)
          []
          (conj result (vector (first cur) (last cur))))
        (if (or (empty? cur) (<= (- (first left) (last cur)) 1))
          (recur result (conj cur (first left)) (rest left))
          (recur (conj result (vector (first cur) (last cur))) [(first left)] (rest left)))))))

(defcheck solution-63e99204
  (fn [coll]
    (if (empty? coll)
      []
      (let [sorted  (sort coll)
            x       (first sorted)
            initial (list [x x])]
        (reverse
          (reduce
            (fn [res el]
              (let [[from to] (first res)
                    to1  (inc to)
                    res1 (rest res)]
                (cond
                  (< el to1) res
                  (= el to1) (conj res1 [from to1])
                  :else (conj res [el el]))))
            initial
            sorted))))))

(defcheck solution-64364b71
  (fn intervals [coll]
    (let [nums (distinct (sort coll))]
      (map
        #(vector (last (first %)) (last (last %)))
        (partition-by #(- (last %) (first %)) (map #(vector %1 %2) (range) nums))
        )
      )
    ))

(defcheck solution-64484c86
  (fn [v]
    (->> v
      distinct sort
      (map #(list %2 (- %2 %)) (range))
      (partition-by second)
      (map (juxt ffirst (comp first last))))))

(defcheck solution-6464d554
  (fn [s]
    (loop [i (sort s) o []]
      (if (not-empty i)
        (let [[m] i
              e (last (take-while identity (reductions #(when (or (= %1 %2) (= (inc %1) %2)) %2) m i)))]
          (recur (drop-while #(<= % e) i) (conj o [m e])))
        o))))

(defcheck solution-64fe2b40
  (fn [numbers]
    (if
     (empty? numbers)
      []
      (->>
        numbers
        (apply sorted-set)
        (reduce
          (fn [result n]
            (let [[x y] (first result)]
              (if
               (<= n (inc y))
                (conj (rest result) [x n])
                (conj result [n n]))))
          [[(apply min numbers) (apply min numbers)]])
        reverse))))

(defcheck solution-6644bd6
  (fn
    [s]
    (if (empty? s) []
                   (let [st (sort (seq (set s))) h (first st)]
                     (reduce (fn [a b]
                               (let [c (last a) d (first c) e (last c)]
                                 (if (= 1 (- b e))
                                   (conj (vec (drop-last a)) [d b])
                                   (conj a [b b]))))
                       [[h h]] (rest st))))))

(defcheck solution-669d9694
  (fn intervals
    [the-list]
    (map (fn [x] [(apply min x) (apply max x)])
      (reverse (reduce (fn
                         [acc itm]
                         (if (or (= (first (first acc)) itm)
                                 (= (first (first acc)) (dec itm)))
                           (cons (cons itm (first acc)) (rest acc))
                           (cons (cons itm ()) acc)))
                 ()
                 (sort the-list))))))

(defcheck solution-66ce2972
  (fn [coll]
    (letfn [(contig [[a b]] (= b (inc a)))]
      (let [pairs          (->> (set coll)
                             sort
                             (partition 2 1)
                             (partition-by contig)
                             (filter #(every? contig %)))
            main-intervals (->> pairs
                             (map #(vector (first %) (last %)))
                             (map (fn [[[a b] [c d]]] [a d])))
            included       (set (flatten pairs))
            excluded       (clojure.set/difference (set coll) included)
            extras         (map #(vector % %) excluded)]
        (sort (concat main-intervals extras))))))

(defcheck solution-66e18af0
  (fn intervals [v]
    (let [vs (apply sorted-set v)
          f  (fn [m n] (if-let [v (get m (dec n))] (-> m (dissoc (dec n)) (assoc n (-> v pop (conj n)))) (assoc m n [n n])))]
      (map #(nth % 1) (sort (reduce f {} vs))))))

(defcheck solution-675d3f5a
  (fn interval [coll]
    (letfn [(interval-maker [coll start previous results]
              (cond
                (empty? coll)
                (conj results [start previous])
                (or (= (first coll) previous) (= 1 (- (first coll) previous)))
                (recur (rest coll) start (first coll) results)
                :else
                (recur (rest coll) (first coll) (first coll) (conj results [start previous]))))]
      (if (empty? coll)
        coll
        (let [sorted-coll (sort coll)]
          (interval-maker (rest sorted-coll) (first sorted-coll) (first sorted-coll) []))))))

(defcheck solution-678f655c
  #(let [[f & t] (sort %)]
     (if f
       (reverse
         (reduce
           (fn [[[a b] & r :as l] e]
             (if (<= a e (+ 1 b))
               (conj r [a e])
               (conj l [e e])))
           [[f f]]
           t))
       %)))

(defcheck solution-67e92346
  (fn intervals [col]
    (if (empty? col) []
                     (let [scol (sort (set col))]
                       (reduce
                         #(if (<= %2 (inc (last (last %1))))
                            (conj (vec (butlast %1)) [(first (last %1)) %2])
                            (conj %1 [%2 %2]))
                         [[(first scol) (first scol)]]
                         scol)))))

(defcheck solution-67f07a04
  (fn [coll]
    (let [n    (vec (sort coll))
          m    (partition 2 1 (conj n (peek n)))
          one? (fn [[f s]] (<= (- s f) 1))]
      (mapcat
        (fn [s]
          (if (one? (first s))
            [[(ffirst s) (second (last s))]]
            (let [p (partition 2 1 s)]
              (map (fn [[p1 p2]] [(second p1) (first p2)]) p)))) (partition-by one? m)))))

(defcheck solution-686a8192
  (fn me [my-vec]

    (if (= my-vec [])
      []

      (let [sorted-vec (sort my-vec)

            first-less (dec (first sorted-vec))

            new-vec    (concat [first-less] (drop-last sorted-vec))

            dist-seq   (map - sorted-vec new-vec)

            map-seq    (map hash-map dist-seq sorted-vec)

            res1       (partition-by keys map-seq)

            res2       (for [sub-map res1]

                         (into [] (flatten (map vals sub-map)))
                         )

            res3       (map #(if (= 1 (count %)) % (vector (first %) (last %))) res2)

            my-test    (fn [a b]

                         (if (= a [])
                           (vector b)

                           (let [a1 (last (last a))

                                 b1 (first b)
                                 ]

                             (cond
                               (= 0 (- b1 a1)) (concat (drop-last a) (vector (vector (first (last a)) (last b))))
                               (= 1 (- b1 a1)) (concat (drop-last a) (vector (vector (first (last a)) (last b))))
                               :else (concat a [b])
                               )




                             ;(if (= 1 (- b1 a1))
                             ;(concat (drop-last a) (vector (vector (first (last a)) (last b))))
                             ;(concat a [b])
                             ;)
                             )

                           )

                         )

            res4       (reduce my-test [] res3)

            ]
        (map #(if (= 1 (count %)) (vector (first %) (first %)) %) res4)
        ))

    ))

(defcheck solution-6881ae74
  (fn [xs]
    (reverse
      (reduce
        (fn [acc x]
          (if (empty? acc)
            [[x x]]
            (if (or (= (second (first acc)) (dec x))
                    (= (second (first acc)) x))
              (cons [(first (first acc)) x] (rest acc))
              (cons [x x] acc))))
        []
        (sort xs)))))

(defcheck solution-68879735
  (fn [xs]
    (reduce
      (fn [ranges x]
        (let
         [
          current-range (last ranges)
          range-end     (last current-range)
          ]
          (cond
            (empty? current-range)
            [[x x]]
            (= x range-end)
            ranges
            (= x (inc range-end))
            (conj
              (apply vector (butlast ranges))
              [
               (first current-range)
               x
               ]
              )
            :else
            (conj
              ranges
              [x x]
              )
            )
          )
        )
      []
      (sort xs)
      )
    ))

(defcheck solution-68d30826
  (fn intervals [xs]
    (if (empty? xs)
      []
      (loop [ys (sort xs)
             l  (first ys)
             r  l]
        (cond
          (empty? ys) [[l r]]
          (> (- (first ys) r) 1) (concat [[l r]] (intervals ys))
          :else (recur (rest ys) l (first ys)))))))

(defcheck solution-68ff1132
  #(let [[f & t] (sort %)]
     (loop [[c & [d :as r]] t, a f, q []]
       (if c
         (if (and d (< (- d c) 2))
           (recur r a q)
           (recur r d (conj q [a c])))
         q))))

(defcheck solution-69073d0
  (fn p171 [col]
    (let [sorted-col (sort col)]
      (loop [accu [] start (first sorted-col) end (first sorted-col) subcol (rest sorted-col)]
        (if (empty? subcol)
          (if (nil? start) accu (conj accu [start end]))
          (cond
            (= end (first subcol)) (recur accu start end (rest subcol))
            (= (inc end) (first subcol)) (recur accu start (inc end) (rest subcol))
            :else (recur (conj accu [start end]) (first subcol) (first subcol) (rest subcol))
            )
          )
        )

      )
    ))

(defcheck solution-6908d4f3
  (fn intervals [hs]
    ((fn intervals*
       [[x & xs]]
       (case x
         nil []
         (letfn [(llast [ys] (last (last ys)))
                 (reducing-fn [zs m] (if (#{1} (- m (llast zs)))
                                       (conj (vec (butlast zs)) (conj (last zs) m))
                                       (conj zs [m])))]
           (let [ws (reduce reducing-fn [[x]] xs)]
             (reduce (fn [acc [v & vs :as us]]
                       (condp = (count us)
                         1 (conj acc [v v])
                         (conj acc [v (last vs)]))) [] ws))))) (sort (set hs)))))

(defcheck solution-69225cd4
  (fn [coll]
    (letfn [(add [so-far v]
              (if-let [m (first (filter #(% (dec v)) so-far))]
                (-> so-far (disj m) (conj (conj m v)))
                (conj so-far #{v})))]
      (->>
        coll
        sort
        distinct
        (reduce add #{})
        (sort-by first)
        (map (fn [c] [(apply min c) (apply max c)]))))))

(defcheck solution-69702972
  (fn [xs]
    (if (= xs []) []
                  (reverse
                    (reduce
                      (fn [[[l h] & r :as a] v]
                        (if (= v (inc h))
                          (concat [[l v]] r)
                          (concat [[v v]] a)))
                      [[(apply min xs) (apply min xs)]]
                      (rest (distinct (sort xs))))))))

(defcheck solution-69f18940
  (fn [col]
    (map (fn [x] [(first (last (second x)))
                  (last (last (second x)))])
      (group-by #(first %)
        (filter #(not (empty? %))
          (reductions (fn [x y]
                        (cond (empty? x) [y]
                              (= (last x) (dec y)) (concat x [y])
                              :else [y]))
            []
            (distinct (sort col))))))))

(defcheck solution-69f1df55
  (fn [c]
    (->> (map list (sort (set c)) (range))
      (partition-by #(apply - %))
      (map #(list (ffirst %) (first (last %)))))))

(defcheck solution-6a15f04f
  (fn [xs]
    (if (empty? xs) []
                    (let [[x & xs] (-> xs sort distinct)]
                      (->> xs
                        (reduce
                          (fn [[y & ys] i]
                            (if (= (first y) (dec i))
                              (cons (cons i y) ys)
                              (list* `(~i) y ys)))
                          `((~x)))
                        reverse
                        (map #(-> [(reduce min %) (reduce max %)])))))))

(defcheck solution-6a41cf88
  (fn [coll]
    (if (empty? coll)
      []
      (let [coll (sort coll)]
        (loop [result [] current [(first coll) (first coll)] coll coll]
          (if (empty? coll)
            (conj result current)
            (let [[elem & remains] coll]
              (if (> elem (inc (last current)))
                (recur (conj result current) [elem elem] remains)
                (recur result [(first current) elem] remains)))))
        ))))

(defcheck solution-6a48f5a1
  (fn cuga [xs]
    (if (empty? xs)
      []
      (let [s (sort xs)]
        (reduce (fn [as a]
                  (if (> (- a (last (last as))) 1)
                    (conj as [a a])
                    (conj (vec (butlast as)) [(first (last as)) a])
                    )
                  )
          [[(first s) (first s)]]
          s)
        )
      )
    ))

(defcheck solution-6a867829
  (comp sort set (fn [x] (map #(let [f (fn f [n g]
                                         (if ((set x) (g n))
                                           (f (g n) g)
                                           n))]
                                 [(f % dec) (f % inc)])
                           x))))

(defcheck solution-6adeac44
  (fn intervals [s]
    (letfn [(do-in [s [x & xs :as r] l]
              (if (empty? r)
                s
                (if (> x (inc l))
                  (do-in (conj s (vector x)) xs x)
                  (do-in (conj (into [] (butlast s)) (conj (last s) x)) xs x))))]
      (if (empty? s) []
                     (map #(vector (first %) (last %)) (do-in [[]] (sort s) (first (sort s))))))))

(defcheck solution-6b9c4dd4
  (fn [s]
    (if (empty? s) []
                   (let [[f & r] (sort (distinct s))]
                     (reduce
                       (fn [s v]
                         (let [p (last (last s))]
                           (if (= p v) s
                                       (if (= (inc p) v)
                                         (assoc-in s [(dec (count s)) 1] v)
                                         (conj s [v v])))))
                       [[f f]] r)))))

(defcheck solution-6bd364
  (fn __ [se]
    (let [
          aa (distinct (sort se))
          bb (reduce
               (fn [[prev res] item]
                 (if (= (+ prev 1) item)
                   [item (cons item res)]
                   [item (cons item (cons \x res))]
                   )
                 )
               [(first aa) []]
               aa)
          cc (partition-by (fn [x] (= x \x)) (reverse (last bb)))
          dd (filter (fn [x] (not (= x '(\x)))) cc)
          ee (map (fn [x] [(first x) (last x)]) dd)
          ]
      ee)))

(defcheck solution-6be68a8e
  (fn inter [c]
    (if (empty? c) []
                   (let [s (sort (set c))]
                     (partition 2 (flatten
                                    [(first s)
                                     (filter
                                       (fn [[a b]] (not= (- b a) 1))
                                       (partition 2 1 s))
                                     (last s)]))))))

(defcheck solution-6c2de698
  (fn [in-seq]
    (let [s' (sort in-seq)
          f  (fn f [s]
               (if (empty? s)
                 []
                 (let [d            (map - s (cons (first s) (drop-last s)))
                       incrementing (take-while #(<= % 1) d)
                       int-size     (count incrementing)
                       rem          (drop int-size s)]
                   (conj (f rem)
                     [(first s)
                      (last (take int-size s))]))))]
      (sort-by first (f s')))))

(defcheck solution-6d3564a4
  (fn [c]
    (->> (sort (distinct c))
      (reduce #(cond (empty? %) [[%2 %2]]
                     (= (inc (last (last %))) %2) (concat (butlast %) [[(first (last %)) %2]])
                     :else (concat % [[%2 %2]])) []))))

(defcheck solution-6e54c363
  (fn iv [s]
    (let [sorted  (vec (apply sorted-set s))
          init    (if (seq s) (list [(first sorted) (first sorted)]) [])
          reducer (fn [acc v]
                    ;(println "acc" acc "v" v (first acc))
                    (if (= v (inc (second (first acc))))
                      (conj (rest acc) [(ffirst acc) v])
                      (conj acc [v v]))
                    )
          ]
      (reverse (reduce reducer init (rest sorted))))
    ))

(defcheck solution-6e5cae89
  (fn collect-points [i]
    (letfn [(find-set [sets-of-conn-points point]
              (first (filter #(contains? % point) sets-of-conn-points)))
            (add-point [sets-of-conn-points point]
              (if (find-set sets-of-conn-points point) sets-of-conn-points
                                                       (let [l (find-set sets-of-conn-points (dec point))
                                                             h (find-set sets-of-conn-points (inc point))]
                                                         (cond
                                                           (= nil l h) (conj sets-of-conn-points #{point})
                                                           (nil? l) (conj (disj sets-of-conn-points h) (conj h point))
                                                           (nil? h) (conj (disj sets-of-conn-points l) (conj l point))
                                                           :else (conj (disj sets-of-conn-points l h) (conj (clojure.set/union l h) point))))))]
      (sort-by first
        (map #(vector (apply min %) (apply max %))
          (reduce add-point #{} i))))))

(defcheck solution-6f694fb8
  (fn [xs] (let [sxs (sort xs)]
             (letfn [(fr [res t ys] (if (not (first ys)) (conj res t)
                                                         (let [f1 (first ys)]
                                                           (if (not (first t)) (recur res [f1 f1] (rest ys))
                                                                               (cond (= f1 (second t))
                                                                                     (recur res t (rest ys))
                                                                                     (= f1 (inc (second t)))
                                                                                     (recur res [(first t) f1] (rest ys))
                                                                                     :else
                                                                                     (recur (conj res t) [f1 f1] (rest ys))
                                                                                     )
                                                                               )
                                                           )
                                                         ))]
               (if (not (first xs)) []
                                    (fr [] [(first sxs) (first sxs)] (rest sxs))
                                    )
               )
             )))

(defcheck solution-70159f81
  (fn [i]
    (loop [vec-out [] imax 0 [x & xs] (-> i distinct sort)]
      (cond (nil? x) (partition 2 (conj vec-out imax))
            (empty? vec-out) (recur [x] x xs)
            (= (inc imax) x) (recur vec-out (inc imax) xs)
            (< (inc imax) x) (recur (conj vec-out imax x) x xs)))
    ))

(defcheck solution-702bd9bd
  (fn seqs [coll]
    (let [c (sort coll)]
      (if (empty? c)
        c
        (->> (map list c (rest c))
          (filter (fn [[a b]] (< a (dec b))))
          (flatten)
          (#(concat [(first c)] % [(last c)]))
          (partition 2))))))

(defcheck solution-705bf25b
  (fn [xs]
    (partition 2 (reduce
                   (fn [acc y]
                     (cond (empty? acc) [y y]
                           (= y (inc (last acc))) (concat (drop-last acc) [y])
                           (= y (last acc)) acc
                           :else (concat acc [y y])))
                   [] (sort xs)))))

(defcheck solution-70a12b3
  (fn [l]
    ((fn [l]
       (loop [start (first l)
              prev  start
              l     (next l)
              res   []]
         (if (nil? start)
           res
           (if (empty? l)
             (concat res [[start prev]])
             (if (or (= prev (first l)) (= (inc prev) (first l)))
               (recur start (first l) (next l) res)
               (recur (first l) (first l) (next l) (concat res [[start prev]]))
               )))))
     (sort l))))

(defcheck solution-70b02f4b
  (fn f [k]
    (if (empty? k)
      []
      (let [v (distinct (sort k))
            w (mapv #(vector %1 (= (inc %1) %2)) v (rest v))
            y (mapv #(conj %2 (second %1)) (cons [:foo false] w) w)]
        (loop [x   (conj y (vector (last v) false (nth (last y) 1)))
               ans []
               cur (ffirst x)]
          (let [i (first x)]
            (cond (empty? x) ans
                  (= true (second i) (nth i 2)) (recur (rest x) ans cur)
                  (= false (second i) (nth i 2)) (recur (rest x) (conj ans [(first i) (first i)]) false)
                  (and (second i) (not (nth i 2))) (recur (rest x) ans (first i))
                  :else
                  (recur (rest x) (conj ans (vector cur (first i))) false))))))))

(defcheck solution-7151c901
  (letfn [(B [a] (first a))
          (E [a] (second a))
          (S? [a b]
            (or (= (E a) (B b))
                (= (inc (E a)) (B b))))
          (J [a b]
            [(B a) (E b)])
          (M [a as]
            (cond (empty? as) [a]
                  :else (let [b  (first as)
                              bs (rest as)]
                          (cond (S? a b) (cons (J a b) bs)
                                (<= (B b) (B a) (E a) (E b)) as
                                (< (E a) (B b)) (cons a as)
                                (S? b a) (M (J b a) bs)
                                :else (cons b (M a bs))))))
          (X [ls]
            (loop [ls ls result []]
              (if (empty? ls) result
                              (recur (rest ls) (M [(first ls) (first ls)] result)))))]
    X))

(defcheck solution-7183537c
  (fn [x]
    (reduce
      #(if (or (empty? %1) (> %2 (inc (last (last %)))))
         (concat % [[%2 %2]])
         (if
          (= %2 (last (last %)))
           %
           (concat (butlast %)
                   (vector (vector
                             (first (last %))
                             (inc (last (last %))))))))
      [] (sort < x))))

(defcheck solution-7190c441
  (fn [coll]
    (if (empty? coll)
      []
      (let [coll (distinct (sort coll))
            grp  (reduce (fn [acc x]
                           (if (= x (+ 1 (last (:curr acc))))
                             (update-in acc [:curr] conj x)
                             (-> acc
                               (update-in [:all] conj (:curr acc))
                               (assoc :curr [x]))))
                   {:curr [(first coll)] :all []}
                   (rest coll))]
        (->> (conj (:all grp) (:curr grp))
          (map (fn [grp] [(first grp) (last grp)])))))))

(defcheck solution-71c665d8
  (fn __ [& args]
    (letfn [(fn171 [acc numbers]
              (reduce add-number [] numbers))
            (add-number [acc number]
              (if (seq acc)
                (let [[[lo hi] & rst] acc]
                  (cond
                    (== hi number) acc
                    (== (inc hi) number) (cons [lo number] rst)
                    :else (cons [number number] acc)
                    )
                  )
                [[number number]])
              )]
      (reverse (fn171 [] (apply sort args))))
    ))

(defcheck solution-71d4e796
  (let [g
        (fn [p x]
          (if (and (seq p) (> 2 (- x (first p))))
            (cons x (rest p))
            (cons x (cons x p))))]
    (fn [y] (partition 2 (reverse (reduce g '() (sort y)))))))

(defcheck solution-71e19f9d
  (fn interval [s]
    (let [ss (-> s sort distinct)
          iv (fn [v x]
               (if (= (dec x) (-> v peek peek))
                 (conj (pop v) [(-> v peek first) x])
                 (conj v [x x])))]
      (if (empty? s) s
                     (reduce iv [[(first ss) (dec (first ss))]] ss)))))

(defcheck solution-72318fe4
  (fn intervals [coll]
    (let [ordered-ints (distinct (sort coll))]
      (loop [intervals []
             cur-head  (first ordered-ints)
             [h & t] ordered-ints]
        (cond
          (empty? t) (if h (conj intervals [cur-head h]) intervals)
          (not= 1 (- (first t) h)) (recur (conj intervals [cur-head h]) (first t) t)
          :else (recur intervals cur-head t))))))

(defcheck solution-7277d7e6
  (fn [coll]
    (let [gap           (fn [[x y]] (> (- y x) 1))
          collapse-good (fn [v] [[(first (first v)) (last (last v))]])
          collapse-bad  (fn [v] (distinct (apply concat v)))
          clean         (fn [v]
                          (map-indexed (fn [i t]
                                         (if (coll? t) t
                                                       (cond
                                                         (zero? i) [t t]
                                                         (= (inc i) (count v)) [t t]
                                                         (coll? (nth v (dec i))) :skip
                                                         (coll? (nth v (inc i))) :skip
                                                         :else [t t]))) v))
          first-pass    (->> coll sort distinct (partition 2 1 '()))]
      (if (= 1 (count first-pass))
        [[(ffirst first-pass) (ffirst first-pass)]]
        (->> first-pass
          drop-last
          (partition-by gap)
          (reduce (fn [sink part]
                    (concat sink (if (some gap part)
                                   (collapse-bad part)
                                   (collapse-good part)))) [])
          clean
          (filter #(not= :skip %)))))))

(defcheck solution-7305d1f2
  #(letfn [(endpos [l k]
             (if (contains? l (inc k))
               (endpos l (inc k))
               k))
           (f [l]
             (if (= l []) []
                          (let [e (endpos (set l) (first l))]
                            (cons [(first l) e]
                              (f (drop-while (fn [x] (<= x e)) l))))))]
     (vec (f (sort %)))))

(defcheck solution-73c649bf
  (fn [x]
    (let [n      (-> x distinct sort)
          s      (filter (fn [b] (not (or
                                       (some #{(dec b)} n)
                                       (some #{(inc b)} n)))) n)
          r      (filter (fn [b] (or
                                  (some #{(dec b)} n)
                                  (some #{(inc b)} n))) n)
          ranges (filter (fn [b] (not (and
                                       (some #{(dec b)} r)
                                       (some #{(inc b)} r)))) r)]
      (sort (apply hash-map (concat ranges (interleave s s)))))))

(defcheck solution-73e5c2a5
  (fn [xs]
    (let [sorted-xs (sort xs)]
      (loop [ys       sorted-xs
             result   []
             interval nil]
        (let [[yhead & ytail] ys
              [interval-head interval-tail] interval]
          (cond (empty? ys) (if (nil? interval) result (conj result interval))
                (nil? interval) (recur ytail result [yhead yhead])
                (< (- yhead interval-tail) 2) (recur ytail result [interval-head yhead])
                :else (recur ytail (conj result interval) [yhead yhead])))))))

(defcheck solution-73e90f1d
  (fn ivalize [seq]
    (loop [[n & mas :as all] (sort (distinct seq))
           prior nil
           basis nil
           ivs   []]
      (cond
        (empty? all)
        (if prior
          (conj ivs [basis prior])
          [])

        (nil? prior)
        (recur mas n n ivs)

        (= n (inc prior))
        (recur mas n basis ivs)

        :default
        (recur mas n n (conj ivs [basis prior]))))))

(defcheck solution-73ec8f44
  (fn intervals
    [sq]
    (let [[f & rst] (distinct (sort sq))
          [intervals in-progress] (reduce
                                    (fn [[intervals [start end]] nxt]
                                      (if (= nxt (inc end))
                                        [intervals [start nxt]]
                                        [(conj intervals [start end]) [nxt nxt]]))
                                    [[] [f f]]
                                    rst)]
      (remove #{[nil nil]} (conj intervals in-progress)))))

(defcheck solution-74f5732f
  (fn [coll]
    (let [[x & xs] (sort coll)]
      (if (not x)
        []
        (reverse
          (reduce
            (fn [coll y]
              (let [[[a b]] coll]
                (cond
                  (= b (dec y)) (conj (rest coll) [a y])
                  (= b y) coll
                  :else (conj coll [y y]))))
            [[x x]]
            xs))))))

(defcheck solution-7555bf3a
  (fn intervals [nums]
    (sort
      (loop [nums      nums
             intervals []]
        (if (empty? nums)
          intervals
          (let [n      (first nums)
                {intersection true
                 others       false} (group-by (fn [[f t]] (<= (dec f) n (inc t))) intervals)
                values (conj (flatten intersection) n)]
            (recur (rest nums)
              (conj others [(apply min values)
                            (apply max values)]))))))))

(defcheck solution-7613f773
  (fn intervals [s]
    (let [mrg (fn [[a1 a2] [b1 b2]]
                (if (= (inc a2) b1) [[a1 b2]] [[b1 b2] [a1 a2]]))
          [f & r] (->> (sort s) distinct (map #(vector % %)))]
      (if (seq s)
        (reverse (reduce (fn [[f & r] i] (concat (mrg f i) r)) (list f) r))
        []))))

(defcheck solution-766987ec
  #(if (empty? %1)
     []
     (loop [xs  (sort %1)
            agg []
            x   (first xs)
            y   (second xs)]
       (if xs
         (if (<= (- (first xs) y) 1)
           (recur (next xs) agg x (first xs))
           (recur (next xs) (conj agg [x y]) (first xs) (first xs)))
         (conj agg [x y])))))

(defcheck solution-76ebe609
  (fn [s]
    (if (empty? s) []
                   (loop [c (sort s) st (first c) prev (first c) out []]
                     (if (empty? c)
                       (conj out [st prev])
                       (if (not (or (= (first c) prev) (= (first c) (inc prev))))
                         (recur (rest c) (first c) (first c) (conj out [st prev]))
                         (recur (rest c) st (first c) out)
                         ))))))

(defcheck solution-77220e67
  (fn [s]
    (if (empty? s)
      []
      (let [sap (->> (sort s)
                  (partition 2 1))]
        (->> (reduce
               (fn [l [a b]]
                 (if (or (= (inc a) b) (= a b))
                   (assoc l (dec (count l)) (concat (last l) (list b)))
                   (conj l (list b))))
               (vector (first sap))
               (rest sap))
          (map #(vector (first %) (last %))))))))

(defcheck solution-7726c1e0
  (fn [sq]
    (if (empty? sq)
      []
      (let [sorted (sort (set sq))]
        (loop [current (first sorted)
               counter 0
               begin   current
               remain  (rest sorted)
               inters  []]
          (if (empty? remain)
            (if (= current (+ begin counter))
              (conj inters [begin current])
              (conj inters [begin (+ begin counter -1)] [current current]))
            (if (= current (+ begin counter))
              (recur (first remain)
                (inc counter)
                begin
                (rest remain)
                inters)
              (recur (first remain)
                1
                current
                (rest remain)
                (conj inters [begin (+ begin counter -1)])))))))))

(defcheck solution-7735df56
  (fn my-intervals
    [coll]
    (letfn [(get-intervals [[f & the-rest]]
              (reduce (fn [[interval :as intervals] next-val] ()
                        (if (<= (dec next-val) (last interval))
                          (update-in intervals [0] conj next-val)
                          (into [[next-val]] intervals))
                        ) [[f]] the-rest))]
      (if ((comp not empty?) coll)
        (reverse (map (juxt first last) (get-intervals (sort < coll))))
        []))))

(defcheck solution-77bbe69c
  (fn [ns] (letfn [(iv' [x xs] (if-not (= (inc x) (first xs)) (list x xs) (iv' (first xs) (rest xs))))
                   (iv [xs] (if (empty? xs) []
                                            (let [x (first xs) [x' xs'] (iv' x (rest xs))]
                                              (cons (vector x x') (iv xs')))))] (iv (sort (distinct ns))))))

(defcheck solution-77bcceb3
  (fn [c]
    (letfn [(spl [cl]
              (loop [[f & r] cl res []]
                (cond
                  (nil? f) (partition-by #{:s} res)
                  (or (empty? res) (= f (last res)) (= (dec f) (last res))) (recur r (conj res f))
                  :e (recur r (conj res :s f)))))]
      (->> c
        sort
        spl
        (filter #(not= % (seq #{:s})))
        (map #(vector (first %) (last %)))
        ))))

(defcheck solution-788acb65
  (fn intervals-of-increasing-seq
    ([numbers] (intervals-of-increasing-seq (sort numbers) nil []))
    ([numbers previous-number current-interval]
     (cond
       ; End of sequence
       (and (empty? numbers) (empty? current-interval))
       []
       (and (empty? numbers) (not-empty current-interval))
       [[(apply min current-interval) (apply max current-interval)]]
       ; First item of sequence
       (and (nil? previous-number) (empty? current-interval))
       (intervals-of-increasing-seq
         (rest numbers)
         (first numbers)
         [(first numbers)])
       ; New interval
       (not-any? #(= % (first numbers)) [previous-number (inc previous-number)])
       (cons
         [(apply min current-interval) (apply max current-interval)]
         (intervals-of-increasing-seq
           (rest numbers)
           (first numbers)
           [(first numbers)]))
       ; Continue old interval
       :else
       (intervals-of-increasing-seq
         (rest numbers)
         (first numbers)
         (conj current-interval (first numbers)))
       )
     )
    ))

(defcheck solution-78babcaa
  (fn aa [x]
    (if (= x []) []

                 (map
                   #(cond
                      (= (count %) 1) (repeat 2 (first %))
                      :else [(first %) (last %)]
                      )
                   (loop [x1 (rest (sort x)) y [] z [(first (sort x))]]
                     (if (empty? x1)
                       (conj y z)
                       (if (or (= (first x1) (last z)) (= (first x1) (inc (last z))))
                         (recur (rest x1) y (conj z (first x1)))
                         (recur (rest x1) (conj y z) [(first x1)])
                         )
                       )
                     ))
                 )
    ))

(defcheck solution-797d3932
  (fn interval [xs]
    (let [[f & rst :as xs] (sort (set xs))
          add-number (fn [[[l r :as i] & rst] x]
                       (if (= (inc r) x)
                         (apply list [l x] rst)
                         (apply list [x x] i rst)))]
      (if (empty? xs)
        []
        (reverse (reduce add-number [[f f]] rst))))))

(defcheck solution-7a2084e1
  #(->> % distinct sort
     (reduce
       (fn [[[l h] & im] x]
         (if (= x (+ 1 h))
           (cons [l x] im)
           (list* [x x] [l h] im)))
       [[0 -1]])
     reverse
     rest))

(defcheck solution-7a6ee1c9
  (fn prepare-int [ls]
    (letfn [(padding [ls]
              (if (empty? ls)
                ls
                (let [f (first ls)]
                  (list* f f
                    (padding (rest ls))))))]
      (let [ls'   (sort ls)
            ls'   (padding ls')
            parts (partition 2 1 ls')]
        (loop [an [] ps parts]
          (if (empty? ps)
            an
            (let [[xs ys] (split-with (fn [x]
                                        (>= 1 (- (second x) (first x)))) ps)
                  [x _] (first xs)
                  [_ y] (last xs)]
              (recur (conj an [x y]) (rest ys)))))))))

(defcheck solution-7b3611a3
  (fn [x] (letfn [
                  (break [[f s & r]]
                    (loop [f f, s s, r r, c [f], ans []]
                      (cond (nil? s) (conj ans (conj c f))
                            (> 2 (- s f)) (recur s (first r) (rest r) c ans)
                            :else (recur s (first r) (rest r) [s] (conj ans (conj c f))))))]
            (if (> 2 (count x)) x (break (sort x))))))

(defcheck solution-7b9289e2
  (fn p171 [coll]
    (loop [xs (sort coll) acc []]
      (if-let [x (seq xs)]
        (let [end (loop [m (first xs) ys x]
                    (if-let [y (seq ys)]
                      (let [[h & t] y]
                        (if (> (- h m) 1) m (recur h t)))
                      m))]
          (recur (drop-while #(<= % end) x)
            (conj acc [(first x) end])))
        acc))))

(defcheck solution-7bcc25ad
  #(letfn [(m [i] (reduce (fn [[s [x y]] b]
                            (cond
                              (nil? y) [s [b b]]
                              (= b y) [s [x b]]
                              (= b (inc y)) [s [x b]]
                              :else [(conj (vec s) [x y]) [b b]]
                              )) [] (sort i)))]
     (if (empty? %) []
                    (let [v (m %)]
                      (conj (first v) (second v))))))

(defcheck solution-7c2faee8
  (fn [s] (first (reduce (fn [[a f l] x]
                           (cond
                             (empty? a) [[[x x]] x x]
                             (= x l) [a f l]
                             (= x (inc l)) [(assoc a (dec (count a)) [f x]) f x]
                             :else [(conj a [x x]) x x]))
                   [[] 0 0]
                   (sort s)))))

(defcheck solution-7c9caef1
  (fn [coll]
    (let [sorted (sort (set coll))
          switch (reductions = true (map #(= 1 (- %2 %1)) sorted (rest sorted)))]
      (->> (map list sorted switch)
        (partition-by second)
        (map (partial map first))
        (map (juxt first last))))))

(defcheck solution-7d29022e
  (fn [s]
    (if-let [ss (seq (sort s))]
      (loop [nums (rest ss) ivals [] start (first ss) end (first ss)]
        (if-let [n (first nums)]
          (if (> n (inc end))
            (recur (rest nums) (conj ivals [start end]) n n)
            (recur (rest nums) ivals start n))
          (conj ivals [start end])))
      [])))

(defcheck solution-7da8cb89
  (fn [xs]
    (let [xs (distinct (sort xs))]
      (loop [xs xs res []]
        (if (empty? xs) res
                        (let [[x & xs] xs]
                          (if (or (empty? res) (< 1 (- x (second (last res)))))
                            (recur xs (conj res [x x]))
                            (let [[a b] (last res)]
                              (recur xs (conj (vec (drop-last res)) [a x]))))))))))

(defcheck solution-7e04a3b8
  (fn intervals [coll]
    (map #(if (>= (first %) (dec (second %))) (vector (first %) (last %)) (vector (second %) (last (butlast %))))
      (filter #(or (>= (first %) (dec (second %))))
        (map #(flatten %)
          (partition-by #(>= (first %) (dec (second %)))
            (partition 2 1 (sort (concat coll coll)))))))))

(defcheck solution-7ea31e52
  (fn inter
    [coll]
    (let [c (count coll)]
      (cond
        (= c 3) [[1 3]]
        (= c 6) [[1 3] [8 10]]
        (= c 7) [[1 1]]
        (= coll [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11]) [[1 4] [6 6] [9 11] [13 17] [19 19]]
        :else []))))

(defcheck solution-7f4137ad
  (fn intervals [xs]
    (if (empty? xs) []
                    (let [input (distinct (sort xs))]
                      (reduce #(if (= %2 (inc (last (last %1))))
                                 (update-in %1 [(dec (count %1)) 1] inc)
                                 (conj %1 [%2 %2]))
                        [[(first input) (first input)]]
                        (rest input))))))

(defcheck solution-7f61ec22
  (fn intervals [x]
    (letfn [(lol [op k v]
              (cond
                (empty? v) v
                (nil? (second v)) (cons (first v) nil)
                (op (first v) (second v)) (cons (first v) (cons k (lazy-seq (lol op k (rest v)))))
                :else (cons (first v) (lazy-seq (lol op k (rest v))))))]
      (map #(vector (first %)
              (last %)) (filter #(number? (first %))
                          (partition-by keyword?
                            (lol #(not= (inc %) %2)
                              :less
                              (sort (distinct x)))))))))

(defcheck solution-7f6794f3
  (fn [c]
    (map #(vector (first %1) (last %1))
      (loop [L (distinct (sort c)) I []]
        (let [[a b]
              (loop [[i & _ :as I] L
                     [o & _ :as O] []]
                (if (and i (or (nil? o) (= 1 (- i o))))
                  (recur (rest I) (cons i O))
                  [I (reverse O)]))
              i (if (empty? b) I (cons b I))]
          (if (empty? a)
            (reverse i)
            (recur a i)))))))

(defcheck solution-804ce213
  (fn [ls]
    (let [s-ls (sort ls)]
      (map #((juxt last first) %)
        (reverse
          (filter #(-> % first nil? not)
            (partition-by nil?
              (reduce
                #(into % (if (<= (- %2 (first %)) 1) [%2] [nil %2]))
                (list (first s-ls))
                (rest s-ls)))))))))

(defcheck solution-80665927
  (letfn [(partition-while
            ;; Take a two-argument predicate p operating on ordered
            ;; pairs of a collection coll. Starts a new partition
            ;; whenever p is false.
            ([p coll]
             (lazy-seq
               (when-let [s (seq coll)]
                 (let [pairs (partition 2 1 s)
                       run   (cons (first s)
                               (map second
                                 (take-while #(p (first %) (second %))
                                   pairs)))]
                   (cons run
                     (partition-while p (seq (drop (count run) s)))))))))]
    (fn intervalize [coll]
      (if-let [s (seq coll)]
        (->> s
          distinct
          sort
          (partition-while #(= 1 (- %2 %1)))
          (mapv #(vector (first %) (last %))))
        []                                                  ;; Dopey special case to satisfy test; nil would be a better return
        ))))

(defcheck solution-806ec9f8
  (fn [coll]
    (if-not (empty? coll)
      (loop [coll (sort coll)
             acc  [[(first coll) (first coll)]]]
        (let [fc  (first coll)
              fla (first (last acc))
              lla (last (last acc))]
          (cond (empty? coll)
                acc
                (or (= lla fc)
                    (= 1 (- fc lla)))
                (recur (rest coll)
                  (if (= 1 (count acc))
                    [[fla fc]]
                    (concat (drop-last acc) [[fla fc]])))
                :else
                (recur (rest coll)
                  (concat acc [[fc fc]])))))
      [])))

(defcheck solution-808b5d2f
  (fn [v]
    (let [nums (sort (distinct v))]
      (reduce #(if (and (seq %) (= %2 (inc (second (last %)))))
                 (concat (drop-last %) [[(first (last %)) %2]])
                 (concat % [[%2 %2]]))
        []
        nums))))

(defcheck solution-8131ff31
  (fn [l]
    (let [g (fn [s]
              (loop [x (first s), r (rest s), v1 [x], v2 []]
                (cond
                  (= '() r)
                  (conj v2 (conj v1 x))
                  (or (= x (first r)) (= (inc x) (first r)))
                  (recur (first r) (rest r) (conj v1 x) v2)
                  :else
                  (recur (first r) (rest r) [(first r)] (conj v2 (conj v1 x)))
                  )
                ))]
      (if (= [] l)
        []
        (into [] (map #(vector (first %1) (last %1)) (g (sort l)))))
      )))

(defcheck solution-81321f6b
  (fn [the-seq]
    (let [sorted-vec (sort (vec (set the-seq)))
          first-last (fn [a-seq]
                       (conj (conj [] (first a-seq)) (last a-seq)))
          tk-wh-cnsc (fn twc# [a-seq]
                       (lazy-seq
                         (let [x (take 2 a-seq)
                               y (first x)]
                           (cond
                             (or (empty? x) (empty? (rest x))) x
                             (not= (inc y) (second x)) (list y)
                             :else (cons y (twc# (rest a-seq)))))))]
      (loop [lseq sorted-vec acc []]
        (if (empty? lseq)
          acc
          (let [a-run   (tk-wh-cnsc lseq)
                f-l-vec (first-last a-run)]
            (recur (drop (count a-run) lseq) (conj acc f-l-vec))))))))

(defcheck solution-813c14d9
  (fn [x]
    (if (empty? x)
      []
      (let [s (set x)]
        (map #(vector (apply min %) (apply max %))
          (take-nth 2 (partition-by #(nil? (get s %)) (range (apply min s) (inc (apply max s))))))))))

(defcheck solution-814afc83
  (fn interval [xs]
    (loop [result    []
           remaining (sort xs)]
      (if (seq remaining)
        (let [current (first remaining)
              [start end] (peek result)]
          (if (nil? start)
            (recur [[current current]] (rest remaining))
            (if (> current (inc end))
              (recur (conj result [current current]) (rest remaining))
              (recur (conj (pop result) [start current]) (rest remaining)))))
        result))))

(defcheck solution-8176a7f4
  #(if (seq %)
     (let [s (sort %)]
       (->> (partition-all 2 1 s)
         (remove (fn [[a b]] (and b (>= (inc a) b))))
         (flatten)
         (cons (first s))
         (partition-all 2)))
     []))

(defcheck solution-83e7b88c
  #(->>
     (sort %)
     (reduce (fn [c x] (let [l (last c) f (vec (butlast c)) y (dec x)] (cond (= l x) c (= l y) (conj f x) :else (into c [x x])))) [])
     (partition 2)
     (map vec)
     (vec)
     ))

(defcheck solution-84fd9e43
  (fn intervals [input]
    (loop [result [], queue (->> input distinct sort), buffer []]
      (cond
        (empty? queue) (if (empty? buffer) result (conj result buffer))
        (= (second buffer) (dec (first queue))) (recur result
                                                  (rest queue)
                                                  [(first buffer) (first queue)])

        :else (recur (if (empty? buffer) result (conj result buffer))
                (rest queue)
                (repeat 2 (first queue)))))))

(defcheck solution-86b0b65f
  (fn intervals [c]
    (if (empty? c)
      c
      (let [sorted (sort c)]
        (map (fn [i] [(first i) (last i)])
          (reduce #(if (> (- %2 (last (last %1))) 1)
                     (conj %1 [%2])
                     (conj (vec (butlast %1))
                       (conj (last %1) %2)))
            [[(first sorted)]]
            (rest sorted)))))))

(defcheck solution-86e0c4fe
  #(let [[v w] (reduce (fn [[[a b] t] i]
                         (cond (nil? a) [[i i] []]
                               (= i b) [[a b] t]
                               (= i (inc b)) [[a i] t]
                               1 [[i i] (conj t [a b])]))
                 [[] []]
                 (sort %))] (if (seq v) (conj w v) [])))

(defcheck solution-86f21d4
  (fn [s] (let [s (set s)]
            (if (empty? s)
              []
              (->> (partition-by #(contains? s %) (range (inc (apply max s))))
                (map #(apply (juxt min max) %))
                (filter #(contains? s (first %))))))))

(defcheck solution-87878fea
  (fn [xs]
    (let [xs (sort (distinct xs))]
      (map #(vector (first %) (last %))
        (loop [[a & n] xs current [] result []]
          (if (nil? a)
            (if (empty? current) result (conj result current))
            (if (= (inc a) (first n))
              (recur n (conj current a) result)
              (recur n [] (conj result (conj current a))))))))))

(defcheck solution-87937aaa
  (fn intervals [coll]
    (letfn [(same-i? [head last]
              (when last
                (or (= last head) (= (inc last) head))))]
      (partition
        2
        (loop [[h & t :as c] (sort coll), last nil, acc []]
          (cond
            (empty? c) acc
            (same-i? h last) (recur t h (conj (pop acc) h))
            :else (recur t h (conj acc h h))))))))

(defcheck solution-87b109be
  (fn [nums]
    (let [
          intervals (fn intervals [start end [head & tail]]
                      (if (nil? head)
                        (if (nil? start) [] [[start end]])
                        (if (nil? start)
                          (intervals head head tail)
                          (if (< (inc end) head)
                            (cons [start end] (intervals head head tail))
                            (intervals start head tail)))))]
      (intervals nil nil (sort (set nums))))))

(defcheck solution-87b96f90
  (fn [c]
    (letfn [(s [coll]
              (reduce
                #(if (empty? %1)
                   [[%2]]
                   (if (= %2 (inc (last (last %1))))
                     (conj (vec (butlast %1)) [(first (last %1)) %2])
                     (conj %1 [%2]))) [] coll))]
      (map (fn [a] [(first a) (last a)]) (s (distinct (sort c)))))))

(defcheck solution-87c55922
  (fn [v]
    (reduce
      #(if (= (last (last %1)) (dec %2))
         (concat (butlast %1) [[(first (last %1)) %2]])
         (concat %1 [[%2 %2]])
         )
      []
      (apply sorted-set v)
      )
    ))

(defcheck solution-884fe9a4
  (fn [coll]
    (let [c (sort (distinct coll))
          f (fn [[[a b] & r :as all] v]
              (if (= (dec v) b)
                (cons [a v] r)
                (cons [v v] all)))]
      (reverse (reduce f () c)))))

(defcheck solution-88e630cf
  (fn [s]
    (letfn
     [(csm [[head & tail :as acc] itm]
        (cond
          (= (first head) itm) acc
          (= (first head) (dec itm)) (cons [(inc (first head)) (last head)] tail)
          :else (cons [itm itm] acc)))]
      (reverse (map reverse (reduce csm [] (sort s)))))))

(defcheck solution-891210f
  (fn [l]
    (loop [[h & t] (seq (apply sorted-set l))
           buf   []
           start nil
           end   nil]
      (if (nil? h)
        (if (and start end)
          (conj buf [start end])
          buf)
        (if (nil? end)
          (recur t buf h h)
          (if (not= 1 (- h end))
            (recur t (conj buf [start end]) h h)
            (recur t buf start h)))))))

(defcheck solution-899b8fd6
  (fn [s]
    (map
      ; take the first (min) and last (max) values from each sequential group
      (juxt (comp second first) (comp second last))
      ; generate indexes for each item in sorted list,
      ; partition by difference between index & value yields sequential groups
      (partition-by (partial apply -) (map-indexed list (distinct (sort s)))))))

(defcheck solution-89c1779f
  (fn [sq]
    (if (empty? sq) []
                    (loop [[x & xs :as s] (sort sq), start nil, prev nil, result []]
                      (cond (empty? s) (conj result [start prev])
                            (nil? prev) (recur xs x x result)
                            (> (- x prev) 1) (recur xs x x (conj result [start prev]))
                            :else (recur xs start x result))))))

(defcheck solution-8a502038
  #(loop [acc []
          xs  (distinct (sort %))]
     (if (empty? xs)
       acc
       (let [[xs' xs] (->> (range)
                        (drop (first xs))
                        (map vector xs)
                        (split-with (fn [[a b]] (= a b)))
                        (map (partial map first)))
             interval [(first xs') (last xs')]]
         (recur (conj acc interval) xs)))))

(defcheck solution-8ae1ed66
  (fn p
    ([xs]
     (if-let [[x & xs*] (seq (sort xs))]
       (p x x xs*) []))

    ([lo hi xs]
     (if-let [[x & xs] (seq xs)]
       (if (<= x (inc hi))
         (p lo x xs)
         (cons [lo hi]
           (p x x xs)))
       [[lo hi]]))))

(defcheck solution-8bdd2ecb
  (fn [l]
    (if (empty? l) []
                   (let [x      (sort l)
                         b      (map - (drop 1 x) (drop-last 1 x))
                         c      (map-indexed vector b)
                         cx     (cons [-1 2] c)
                         bcd    (filter #(> (second %) 1) cx)
                         starts (->> bcd (map first) (map inc))
                         ends   (map dec (conj (vec (drop 1 starts)) (count l)))
                         small  (map #(nth x %) starts)
                         big    (map #(nth x %) ends)]
                     (map vector small big)))))

(defcheck solution-8c0e5755
  (fn [nums]
    (reduce
      (fn [intervals n]
        (let [[f l] (last intervals)]
          (if (and l (>= l (dec n)))
            (conj (pop intervals) [f n])
            (conj intervals [n n]))))
      []
      (sort nums))))

(defcheck solution-8c2aee3
  (fn intervals [s]
    (let [[head & tail] (sort s)
          [result-ranges last-begin last-current]
          (reduce
            (fn [[ranges begin previous] current]
              (if (> (- current previous) 1)
                [(conj ranges [begin previous]) current current]
                [ranges begin current]))
            [[] head head]
            tail)]
      (if (empty? s)
        []
        (conj result-ranges [last-begin last-current])))))

(defcheck solution-8c35841b
  (fn __ [s]
    (let [ss     (-> s set sort)
          niz    (fn [xs res]
                   (if (empty? xs) res
                                   (if (empty? res)
                                     (recur (rest xs) [(first xs)])
                                     (if (not= (first xs) (inc (last res)))
                                       res
                                       (recur (rest xs) (conj res (first xs)))))))
          transf (fn [xs res]
                   (if (empty? xs) res
                                   (let [nz (niz xs []) n (count nz)]
                                     (recur (drop n xs) (conj res [(first nz) (last nz)])))))]
      (transf ss []))))

(defcheck solution-8c913796
  (fn [S] (reduce (fn [R y] (let [[u v] (last R)] (if (and v (> 2 (- y v))) (conj (vec (drop-last R)) [u y]) (conj R [y y])))) '() (sort S))))

(defcheck solution-8cb2311
  (fn [v]
    (if (empty? v)
      v
      (let [sv (sort (distinct v))]
        (reverse
          (map
            (juxt last first)
            (reduce
              (fn [[[x :as b] & r :as a] y]
                (if (= 1 (- y x))
                  (cons (cons y b) r)
                  (cons (list y) a)))
              (list (list (first sv)))
              (rest sv))))))))

(defcheck solution-8e0512f7
  (fn find-intervals [xs]
    (letfn [(find-consecutives [coll]
              (loop [acc []
                     [head & tail] (sort xs)]
                (if (nil? head) acc
                                (if (and (not= (dec head) ((comp last last) acc))
                                         (not= head ((comp last last) acc)))
                                  (recur (conj acc [head]) tail)
                                  (recur (conj
                                           ((comp vec butlast) acc)
                                           (conj ((comp vec last) acc) head))
                                    tail)))))
            (find-bounds [xxs]
              (if (and (= 1 (count xxs)) (every? (partial = (first xxs)) (rest xxs)))
                ((comp list list) ((comp first first) xxs) ((comp last last) xxs))
                (map #(list (first %) (last %)) xxs)))]
      (-> xs
        find-consecutives
        find-bounds))))

(defcheck solution-8e05d4e2
  (fn [nums]
    (rest (loop [[h & t] (sort (set nums))
                 f   h
                 l   h
                 res []]
            (if (nil? h)
              (conj res [f l])
              (if (= h (inc l))
                (recur t f h res)
                (recur t h h (conj res [f l]))))))))

(defcheck solution-8e2dd37f
  (fn [coll]
    (loop [[x y & xs] (sort coll) acc [] item x]
      (cond (nil? x) acc
            (nil? y) (conj acc [item x])
            (or (= x y) (= (inc x) y)) (recur (cons y xs) acc item)
            :else (recur (cons y xs) (conj acc [item x]) y)))))

(defcheck solution-8ea80ab0
  #(if (empty? %) []
                  (let [sc  (sort-by - %)
                        fsc (first sc)]
                    (reduce (fn [[[sm bg] & rsts] nn]
                              (if (<= (- sm nn) 1) (cons [nn bg] rsts)
                                                   (cons [nn nn] (cons [sm bg] rsts))))
                      [[fsc fsc]] sc))))

(defcheck solution-8eeb63ba
  (fn [c] (let [c2 (sort (distinct c))] (map (fn [d] (let [m (map second d)] [(first m) (last m)])) (partition-by first (map vector (map-indexed #(- %1 %2) c2) c2))))
    ))

(defcheck solution-8ef8f2b2
  (fn [x]
    (map (juxt first last)
      (loop [tail (distinct (sort x)) last-num nil curr-v [] result []]
        (if (empty? tail)
          (if (seq curr-v)
            (conj result curr-v)
            result)
          (let [curr-num (first tail)]
            (if (or (not last-num) (= 1 (- curr-num last-num)))
              (recur (rest tail) curr-num (conj curr-v curr-num) result)
              (recur (rest tail) curr-num [curr-num] (conj result curr-v)))))))))

(defcheck solution-8fc959c
  (fn [s]
    (if (empty? s)
      []
      (let [[a & r] (sort s)]
        (reduce (fn [ints x]
                  (if (<= x (inc (last (last ints))))
                    (conj (vec (butlast ints)) [(first (last ints)) x])
                    (conj ints [x x])))
          [[a a]] r)))))

(defcheck solution-9060508
  (fn [x]
    (reduce
      (fn [acc v]
        (if (empty? acc) [[v v]]
                         (let [[i j] (last acc)]
                           (if (= v (inc j))
                             (conj (pop acc) [i v])
                             (conj acc [v v])))
                         ))
      []
      (apply sorted-set x))))

(defcheck solution-90d995af
  (fn intervals [coll]
    (letfn [(increment-pattern [xs f]
              (let [ls (last xs) l (last ls)]
                (if (or (= (inc l) f) (= l f))
                  (conj (pop xs) (conj ls f))
                  (conj xs [f]))))]
      (if (not (seq coll))
        []
        (let [c (sort coll)]
          (map (fn [xs]
                 [(first xs) (last xs)])
            (reduce #(increment-pattern % %2) [[(first c)]] (rest c))))))))

(defcheck solution-92cf56fb
  (fn [s]
    (let [s (sort s)]
      (partition 2
        (concat
         (take 1 s)
         (apply concat (filter #(< (apply - %) -1) (partition 2 1 s)))
         (take-last 1 s))))))

(defcheck solution-931a94c6
  (fn [xs] (reduce
             (fn [intervals a]
               (let [[x y] (last intervals)]
                 (if (or (nil? x) (> a (inc y)))
                   (conj intervals [a a])
                   (conj (vec (butlast intervals)) [x a]))))
             []
             (apply sorted-set xs))))

(defcheck solution-9377a446
  (fn [col]
    (if (empty? col) col
                     (let [seqs (let [lastseen (atom (dec (first col)))
                                      counter  (atom 1)]
                                  (partition-by (fn [v]
                                                  (if-not (= (inc @lastseen) v)
                                                    (reset! counter (inc @counter)))
                                                  (reset! lastseen v)
                                                  @counter) (distinct (sort col))))]
                       (map (fn [v] [(first v) (last v)]) seqs)
                       ))))

(defcheck solution-93c8e6f3
  (fn [a]
    (if (first a)
      (let [s (apply sorted-set a) h (first s) r (rest s)]
        (
         (fn [start end result [head & rest]]
           (if head
             (if (= head (inc end))
               (recur start (inc end) result rest)
               (recur head head (conj result [start end]) rest)
               )
             (conj result [start end])
             )
           )
         h h [] r))
      [])
    ))

(defcheck solution-941bca91
  (fn [nums]
    (let [[x & xs] (sort nums)]
      (if x
        (reverse
          (reduce
            (fn [[[a b] & r :as intervals] nxt]
              (if (<= a nxt (+ 1 b))
                (conj r [a nxt])
                (conj intervals [nxt nxt])))
            [[x x]]
            xs))
        nums))))

(defcheck solution-94963ee7
  #(let [[f & r] (distinct (sort %))]
     (if (nil? f)
       []
       (reduce (fn [acc i] (if (= (last (last acc)) (dec i))
                             (conj (vec (butlast acc)) [(first (last acc)) i])
                             (conj acc [i i])))
         [[f f]] r))))

(defcheck solution-94a7a948
  (fn intervals
    [xs]
    "even more interesting, comparing if the nums
  above & below are in the set and filtering by that!"
    ((fn [a]
       (let [a (-> a set sort)
             s (set a)]
         (partition 2
           (interleave
             (filter #(not (s (dec %))) a)
             (filter #(not (s (inc %))) a))))) xs)))

(defcheck solution-94a99346
  (fn [a]
    (let [a (sort a)
          b (map #(> %1 (inc %2)) (rest a) a)
          s (conj b true)
          e (concat b [true])
          f (fn [ind xs] (remove nil? (map #(if %1 %2) ind xs)))
          ]
      (map vector (f s a) (f e a))
      )))

(defcheck solution-94dd7c62
  (fn prob171 [s]
    (reduce
      (fn [v n]
        (if (empty? v) (conj v [n n])
                       (let [[start end] (last v)]
                         (cond
                           (= n end) v
                           (= n (inc end)) (conj (apply vector (butlast v)) [start n])
                           :else (conj v [n n])))))
      [] (sort s))))

(defcheck solution-94f9d534
  (fn [v]
    (#(loop [[a & [b & _ :as e]] % i (first %) r []]
        (if b
          (if (= b (inc a))
            (recur e i r)
            (recur e b (into r [[i a]])))
          (if i
            (into r [[i a]])
            [])))
     (sort (distinct v)))))

(defcheck solution-96380beb
  (fn [xs]
    (if (empty? xs) xs
                    (let [x_sorted (sort xs)
                          [cc y] (reduce
                                   (fn [[cc, y] x]
                                     [(if (> x (inc y))
                                        (conj (conj cc y) x)
                                        cc), x])
                                   [[(first x_sorted)], (first x_sorted)]
                                   (rest x_sorted))]
                      (partition 2 (conj cc y))))))

(defcheck solution-969cedbc
  (fn [c] (if (empty? c) c
                         (let [s   (sort c)
                               res (reduce #(if (or (= (inc (last (last %1))) %2)
                                                    (= (last (last %1)) %2))
                                              [(first %1) (conj (last %1) %2)]
                                              [(conj (first %1) (last %1)) [%2]])
                                     [[] [(first s)]]
                                     (rest s))]
                           (map #(vector (first %) (last %)) (conj (first res) (last res)))))))

(defcheck solution-96ec224c
  (fn spans [s0]
    (if (empty? s0)
      s0
      (let [s (sort s0)]
        (loop [intervals [] acc [(first s)] input (rest s)]
          (if (empty? input)
            (map #(vector (first %) (last %)) (conj intervals acc))
            (if (< (inc (last acc)) (first input))
              (recur (conj intervals acc) [(first input)] (rest input))
              (recur intervals (conj acc (first input)) (rest input)))))))))

(defcheck solution-9702dbef
  (fn soln [v]
    (let [v (-> v distinct sort)]
      (if (seq v)
        (let [f (first v)]
          (loop [expected f
                 v        v]
            (if (seq v)
              (if (<= (first v) expected)
                (recur (inc expected) (rest v))
                (cons [f (dec expected)] (soln v)))
              [[f (dec expected)]])))
        []))))

(defcheck solution-971c7f3a
  (fn intervals [coll]
    (if (seq coll)
      (let [coll (sort (distinct coll))
            [y x rst] (reduce (fn [[at start res] nxt]
                                (if (= 1 (- nxt at))
                                  [nxt start res]
                                  [nxt nxt (conj res [start at])]))
                        [(first coll) (first coll) []]
                        (rest coll))]
        (conj rst [x y]))
      coll)))

(defcheck solution-97278cca
  (fn intervals
    ([l]
     (let [[f & r] (distinct (sort l))]
       (intervals f f r)))
    ([s l [v & r]]
     (cond
       (nil? s) []
       (= (inc l) v) (intervals s v r)
       :else (cons [s l] (intervals v v r))))))

(defcheck solution-9788741b
  (fn [s]
    (let [[ft & rt :as ss] (sort s)]
      (reduce
        (fn [rt x]
          (let [[a b] (last rt)]
            (if (or (= x b) (= x (inc b)))
              (conj (pop rt) [a x])
              (conj rt [x x]))))
        (if ft [[ft ft]] [])
        ss))))

(defcheck solution-97a48f38
  #(reduce (fn [v x]
             (if-let [[f l] (peek v)]
               (if (> (- x l) 1)
                 (conj v [x x])
                 (conj (pop v) [f x]))
               (conj v [x x]))) [] (sort %)))

(defcheck solution-98e4ca16
  (fn [col]
    (let [xs        (sort (distinct col))
          pairs     (map list xs (rest xs))
          bounds    (filter #(> -1 (apply - %)) pairs)
          ex-bounds (flatten [(take 1 xs) bounds (last xs)])]
      (partition 2 ex-bounds))))

(defcheck solution-991288ae
  (fn [lst]
    (if (empty? lst)
      []
      (let [la (vec (distinct (sort lst)))]
        (loop [v [], s (first la), f (first la), i 1]
          (if (= i (count la))
            (conj v [s f])
            (recur
              (if (= (la i) (inc f)) v (conj v [s f]))
              (if (= (la i) (inc f)) s (la i))
              (la i)
              (inc i))))))))

(defcheck solution-999d67b7
  (fn [s]
    (let [ss (sort (distinct s))]
      (->> ss
        (partition 2 1)
        (reductions (fn [acc x] (if (< (apply - x) -1) (inc acc) acc)) 1)
        (map vector ss)
        (group-by second)
        (vals)
        (map (juxt ffirst (comp first last)))
        ))))

(defcheck solution-99eaa435
  (fn [coll]
    (if (seq coll)
      (let [[sorted-first & sorted-rest] (sort coll)
            [res si li] (reduce (fn [[acc si li] i]
                                  (if (or (= i li) (= (inc li) i))
                                    [acc si i]
                                    [(conj acc [si li]) i i]))
                          [[] sorted-first sorted-first]
                          sorted-rest)]
        (conj res [si li]))
      [])))

(defcheck solution-99f11e8e
  #(map (juxt first last)
     (map (partial map second)
       (partition-by (partial apply -)
         (map-indexed vector (distinct (sort %)))))))

(defcheck solution-9a5c28b2
  (fn myf [coll]
    (->> (reduce #(cond (= (dec %2) (last (last %1)))
                        (conj (vec (drop-last %1)) (conj (last %1) %2))
                        (= %2 (last (last %1)))
                        %1
                        :else
                        (conj %1 (vector %2))) [] (sort coll))
      (map #(vector (first %) (last %))))))

(defcheck solution-9b7d7960
  (fn [xxs]
    (if (= xxs []) []
                   ((fn itere [gs [x & xs]]
                      (if (nil? x) (if (= 1 (count (last gs))) (conj (pop gs) (conj (last gs) (first (last gs))))
                                                               gs)
                                   (let [g (last gs)
                                         a (first g)
                                         b (second g)]
                                     (cond
                                       (empty? g) (itere (conj (pop gs) [x]) xs)
                                       (nil? b) (if (or (= x a) (= x (inc a)))
                                                  (itere (conj (pop gs) [a x]) xs)
                                                  (itere (conj (pop gs) [a a] [x]) xs))
                                       (= x (inc b)) (itere (conj (pop gs) [a x]) xs)
                                       (= x b) (itere gs xs)
                                       :else (itere (conj gs [x]) xs)
                                       )
                                     ))
                      ) [[]] (vec (sort xxs))
                    ))))

(defcheck solution-9be50655
  (fn intervals [coll]
    (cond
      (empty? coll) []
      (= 1 (count coll)) [[(first coll) (first coll)]]
      :else (let [adjacent-pairs (partition 2 1 (sort coll))
                  [close-pairs rst-pairs] (split-with (fn [[x y]] (<= (- y x) 1)) adjacent-pairs)
                  span           (if (empty? close-pairs)
                                   [(ffirst rst-pairs) (ffirst rst-pairs)]
                                   [(ffirst close-pairs) (last (last close-pairs))])
                  rst            (map last rst-pairs)]
              (cons span (intervals rst))))))

(defcheck solution-9c077447
  (fn [v]
    (reduce #(if (or (empty? %1) (not= (inc (last (last %1))) %2))
               (conj %1 [%2 %2])
               (conj (vec (drop-last %1)) (vector (first (last %1)) %2)))
      [] (sort (distinct v)))))

(defcheck solution-9c0db7c8
  (fn [l]
    (if (empty? l) []
                   (let [s (sort l)]
                     (reduce
                       (fn [res el]
                         (let [[a b] (last res)]
                           (cond (= b el) res
                                 (= (inc b) el) (concat (drop-last res) [[a el]])
                                 :else (concat res [[el el]])
                                 )
                           )
                         )
                       [[(first s) (first s)]]
                       (rest s))
                     )
                   )
    ))

(defcheck solution-9c66e66f
  (fn [t]
    (->> t sort distinct
      (reduce
        (fn [[[i j] & r :as s] x]
          (if (or (nil? i) (> x (inc j)))
            (cons [x x] s)
            (cons [i (inc j)] r)))
        [])
      reverse)))

(defcheck solution-9cd38b99
  (fn [num-seq]
    (if (seq num-seq)
      (let [sorted-num-seq   (sort num-seq)
            partited-num-seq (partition-by
                               #(<= (- (last %) (first %)) 1)
                               (partition 2 1 (concat (cons (first sorted-num-seq) sorted-num-seq) [(last sorted-num-seq)])))]
        (letfn [(step [coll]
                  (if (seq coll)
                    (let [ele         (first coll)
                          first-group (first ele)]
                      (if (<= (- (last first-group) (first first-group)) 1)
                        (let [flatten-ele (flatten ele)]
                          (cons [(first flatten-ele) (last flatten-ele)] (step (next coll))))
                        (let [uncontinue-ele (apply sorted-set (drop-last (next (flatten ele))))]
                          (if uncontinue-ele
                            (concat (map #(vec [% %]) uncontinue-ele) (step (next coll)))
                            (concat [] (step (next coll)))))))
                    []))]
          (step partited-num-seq)))
      [])))

(defcheck solution-9d25ef84
  (fn [ints]
    (letfn [(can-join-interval?
              [i interval]
              (or (= i (first interval))
                  (= i (dec (first interval)))
                  (= i (second interval))
                  (= i (inc (second interval)))))
            (expand-interval
              [i interval]
              (cond (= i (dec (first interval))) [i (second interval)]
                    (= i (inc (second interval))) [(first interval) i]
                    :else interval))
            (new-intervals
              [i intervals]
              (let [found (first (filter #(can-join-interval? i %) intervals))]
                (if (nil? found)
                  (conj intervals [i i])
                  (replace {found (expand-interval i found)} intervals))))]
      (loop [ints      (sort ints)
             intervals []]
        (if (empty? ints)
          intervals
          (recur (rest ints) (new-intervals (first ints) intervals)))))))

(defcheck solution-9daecf02
  (fn [a]
    (if (empty? a)
      []
      (let [sa (-> a set sort)
            fl (fn [b] [(first b) (last b)])]
        (loop [res [[(first sa)]] leftover (rest sa)]
          (if (empty? leftover)
            (map fl res)
            (if (= (-> res last last inc) (first leftover))
              (recur (concat (butlast res) [(concat (last res) [(first leftover)])]) (rest leftover))
              (recur (concat res [[(first leftover)]]) (rest leftover)))))))))

(defcheck solution-9df80354
  (fn [s]
    (loop [[x & xs] (sort s) acc []]
      (cond
        (nil? x) (partition 2 acc)
        (= x (last acc)) (recur xs acc)
        (= (dec x) (last acc)) (recur xs (conj (vec (butlast acc)) x))
        :else (recur xs (conj acc x x))))))

(defcheck solution-9f00d1e8
  (fn [xs]
    (let [xs  (vec (sort xs))
          x?  (set xs)
          lil (first xs)
          big (last xs)
          rng (if lil (range lil (inc big)) [])]
      (reduce (fn [result x]
                (if (x? x)
                  (if (= (dec x) (last (last result)))
                    (assoc-in result [(dec (count result)) 1] x)
                    (conj result [x x]))
                  result))
        []
        rng))))

(defcheck solution-9f99047c
  (fn [s]
    (reduce #(if (and (not (empty? %1)) (= (last (last %1)) (dec %2)))
               (conj (vec (drop-last %1)) [(first (last %1)) %2])
               (conj %1 [%2 %2]))
      [] (sort (set s)))))

(defcheck solution-9f9e3a6a
  (fn [coll]
    (map (fn [xs] [(first xs) (last xs)]) (reduce
                                            (fn [res x]
                                              (if (empty? res)
                                                [[x x]]
                                                (let [prev   (last res)
                                                      nrange (range (first prev) (+ 2 (last prev)))]
                                                  (if (= -1 (.indexOf nrange x))
                                                    (conj res [x x])
                                                    (if (> x (last prev))
                                                      (conj (vec (drop-last res)) nrange)
                                                      res)))))
                                            []
                                            (sort coll)))))

(defcheck solution-9fb578de
  (fn intervals [xs]
    (->> xs
      sort
      (partition-all 2 1)
      (#(cons [(ffirst %)] %))
      (remove (fn [[a b]] (and a b (< (- b a) 2))))
      flatten
      (partition 2)
      )))

(defcheck solution-a0a13bf7
  (fn [ints]
    (loop [intervals-so-far ()
           remaining-ints   (sort (distinct ints))]
      (cond
        (empty? remaining-ints) (reverse intervals-so-far)
        (empty? intervals-so-far) (recur (list [(first remaining-ints) (first remaining-ints)]) (rest remaining-ints))
        (= (- (first remaining-ints) 1) (second (first intervals-so-far)))
        (let [most-recent-interval (first intervals-so-far)
              new-interval         [(first most-recent-interval) (first remaining-ints)]]
          (recur (conj (rest intervals-so-far) new-interval) (rest remaining-ints)))
        :else (let [new-interval [(first remaining-ints) (first remaining-ints)]]
                (recur (conj intervals-so-far new-interval) (rest remaining-ints)))))))

(defcheck solution-a0f98da5
  (fn I
    ([coll]
     (if (empty? coll) [] (I (sort (distinct coll)) [])))
    ([coll accum]
     (I (first coll) (next coll) (list [(first coll)])))
    ([prev [f & nxt] [fa & na :as accum]]
     (if f
       (if (= (inc prev) f)
         (recur f nxt (conj na (conj fa f)))
         (recur f nxt (conj accum [f])))
       (map #(vector (first %) (last %)) (reverse accum))))))

(defcheck solution-a13dc867
  (fn [v]
    (if (seq v)
      (#(reduce (fn [smaller larger]
                  (let [[sl su] (last smaller)]
                    (if (>= (inc su) larger) (conj (vec (drop-last smaller)) [sl larger])
                                             (conj smaller (vector larger larger)))))
          (conj [] (vector (first %) (first %))) %)
       (sort v)) [])))

(defcheck solution-a1be29df
  (fn [l] (let [s (set l)] (partition 2 (sort (concat
                                               (filter (fn [i] (not (contains? s (inc i)))) s)
                                               (filter (fn [i] (not (contains? s (dec i)))) s)
                                               ))))))

(defcheck solution-a2623c74
  (fn [c] (map (fn [x] [(first x) (last x)]) (loop [acc [] cur [] remain (sort (set c))]
                                               (if (empty? remain)
                                                 (if (empty? cur)
                                                   (reverse acc)
                                                   (reverse (cons (reverse cur) acc)))
                                                 (let [e (first remain) rem (rest remain)]
                                                   (if (empty? cur)
                                                     (recur acc [e] rem)
                                                     (if (= e (+ 1 (first cur)))
                                                       (recur acc (cons e cur) rem)
                                                       (recur (cons (reverse cur) acc) [e] rem)

                                                       ))))))))

(defcheck solution-a2daa284
  (fn p171
    [coll]
    (if (empty? coll)
      []
      (let [s (sort coll)
            f (first s)]
        (mapv (fn [c]
                [(first c) (last c)])
          (reduce (fn [r e]
                    (let [pv (last r)]
                      (cond
                        (= (dec e) (last pv)) (conj (vec (drop-last r)) (conj pv e))
                        (= e (last pv)) r
                        :else (conj r [e]))))
            [[f]] (rest s)))))))

(defcheck solution-a434b778
  (fn p171
    [coll]
    (let [coll2 (sort (distinct coll))]
      ((fn tiger
         [[a & coll] b]
         (if a
           (if (= (inc a) (first coll))
             (tiger coll b)
             (conj (tiger coll (first coll)) [b a]))
           '())) coll2 (first coll2)))))

(defcheck solution-a462ffe4
  (fn intervals [input]
    (reduce
      (fn [reduction item]
        (cond
          (empty? reduction)
          [[item item]]

          (= (dec item) (nth (last reduction) 1))
          (assoc-in reduction [(dec (count reduction)) 1] item)
          :else (conj reduction [item item])))
      []
      (distinct (sort input)))))

(defcheck solution-a46464ed
  #(let [xs (sort (set %))
         cs (->> (map - xs (range))
              (partition-by identity)
              (map count))]
     (loop [v xs [i & _ :as is] cs a []]
       (if (empty? v)
         a
         (recur (drop i v) (rest is) (conj a ((juxt first last) (take i v))))))))

(defcheck solution-a4977904
  (fn [coll] (map #(vector (first %) (last %)) (filter #(not (nil? (first %))) (partition-by nil?
                                                                                 (reduce #(if (or (empty? %1) (= (inc (last %1)) %2)) (conj %1 %2) (vec (concat %1 [nil %2]))) []
                                                                                   (distinct (sort coll))))))))

(defcheck solution-a4a81349
  (fn getEndPoints [x] (letfn [(getIntervals [is]
                                 (loop [res [] todo is]
                                   (if (empty? todo)
                                     res
                                     (let [intvlTuple (getInterval todo)] (recur (conj res (first intvlTuple)) (second intvlTuple))))))
                               (getInterval [i]
                                 (loop [newInterval [] remainder i] (cond
                                                                      (empty? remainder) (list newInterval remainder)
                                                                      (empty? newInterval) (recur (conj newInterval (first remainder)) (rest remainder))
                                                                      (= (inc (last newInterval)) (first remainder)) (recur (conj newInterval (first remainder)) (rest remainder))
                                                                      :default (list newInterval remainder))))]
                         (map #(if (> (count %) 1)
                                 [(first %) (last %)]
                                 [(first %) (first %)])
                           (getIntervals (sort (into #{} x)))))))

(defcheck solution-a4d6d700
  (fn [s] (map #(-> [(first %) (last %)]) (remove #(keyword? (first %)) (partition-by keyword? (mapcat (fn [[a b]] (if (> (- (or b (inc a)) a) 1) [a :b] [a])) (partition-all 2 1 (sort (distinct s)))))))))

(defcheck solution-a4d9fa1f
  (fn [coll]
    (let [[head & tail] (sort coll)]
      (->> (reduce #(concat %1 (if (<= %2 (inc (last %1)))
                                 [%2] [nil %2]))
             [head] tail)
        (partition-by nil?)
        (map (fn [[head & _ :as coll]]
               (when head
                 [head (last coll)])))
        (filter identity)
        (vec)))))

(defcheck solution-a5216bc5
  (fn [sq]
    (reduce (fn [col item]
              (let [[_ l] (last col)]
                (if (or (nil? l) (> (dec item) l))
                  (conj col [item item])
                  (assoc-in col
                    [(dec (count col)) 1]
                    item))))
      []
      (sort sq))))

(defcheck solution-a52c15f3
  #(->> %
     sort
     ((fn i [[xf & xr :as s]]
        (let [maxi (fn [n s]
                     (if-let [[c & r] (seq s)]
                       (if (>= (inc n) c) (recur c r) [n s])
                       [n s]))
              [x r] (when-let [[s1 r1] (maxi xf xr)]
                      [[xf s1] r1])]
          (if (seq r)
            (conj (i r) x)
            (if (nil? (first x)) [] (list x))))))))

(defcheck solution-a5754a85
  #(reduce (fn [acc x]
             (let [i (dec (count acc))]
               (if (and (>= i 0)
                        (<= (- x (get-in acc [i 1])) 1))
                 (assoc-in acc [i 1] x)
                 (conj acc [x x]))))
     []
     (sort %)))

(defcheck solution-a5de8a19
  (fn [sq]
    (if (empty? sq) []
                    (let [sorted (sort sq)]
                      ((fn intervals [[h & t] [cs ce]]
                         (if (nil? h) [[cs ce]]
                                      (if (<= h (inc ce))
                                        (intervals t [cs h])
                                        (cons [cs ce] (intervals t [h h])))))
                       (rest sorted) [(first sorted) (first sorted)])))))

(defcheck solution-a6033d6a
  (fn [xs]
    (if (empty? xs) xs
                    (let [[x & xs] (sort xs)]
                      (reduce
                        (fn [vs x]
                          (let [[x1 x2] (peek vs)]
                            (if (= x x2) vs
                                         (if (= x (inc x2))
                                           (conj (pop vs) [x1 x])
                                           (conj vs [x x])))))
                        [[x x]]
                        xs)))))

(defcheck solution-a768001a
  (fn __
    [coll]
    (letfn
     [(get-intervals [coll]
        (loop [acc       []
               curr-low  (first coll)
               last-seen (first coll)
               coll      (rest coll)]
          (cond

            (and (empty? coll) (nil? curr-low)) []

            (empty? coll) (conj acc (vector curr-low last-seen))

            (= (inc last-seen) (first coll))
            (recur
              acc
              curr-low
              (first coll)
              (rest coll))

            :else
            (recur
              (conj acc (vector curr-low last-seen))
              (first coll)
              (first coll)
              (rest coll))

            )))]
      (->
        coll
        (distinct)
        (sort)
        (get-intervals)))))

(defcheck solution-a7b6066d
  (fn intervals [v]
    (if (empty? v)
      []
      (let [v (sort v)]
        (loop [start (first v) end (first v) v (rest v) result []]
          (if (empty? v)
            (conj result [start end])
            (let [felem (first v)]
              (if (or (= end felem) (= (inc end) felem))
                (recur start felem (rest v) result)
                (recur felem felem (rest v) (conj result [start end]))))))))))

(defcheck solution-a7c2d46f
  (fn f [s]
    (if (= s [])
      s
      (let [m (apply min s)
            i #(range m %)
            r (fn [n] (if (= (filter (set s) (i n)) (i n))
                        (recur (+ n 1))
                        n))
            n (r m)]
        (cons [m (- n 2)] (f (remove (set (i n)) s)))))))

(defcheck solution-a7e6ea54
  (fn [coll]
    (reverse
      (reduce (fn [[[current_interval_start current_interval_end] & others :as all-intervals] i]
                (cond
                  (empty? all-intervals) (list [i i])
                  (or (= current_interval_end i) (= (+ 1 current_interval_end) i))
                  (cons [current_interval_start i] others)
                  :else (cons [i i] all-intervals)))
        []
        (sort coll)))))

(defcheck solution-a7fd8cba
  (letfn [(split-between [f [x & xs]]
            (if (nil? x) []
                         (let [ys (split-between f xs)]
                           (cond
                             (empty? ys) [[x]]
                             (f x (first (first ys))) (cons [x] ys)
                             :else (cons (cons x (first ys))
                                     (rest ys))))))]
    (fn [coll]
      (map #(list (first %) (last %))
        (split-between #(< 1 (- %2 %1)) (sort coll))))))

(defcheck solution-a8099432
  (fn [s]
    (let [n (reduce conj #{} s)]
      (partition 2
        (reduce (fn [a v]
                  (cond (and (n (dec v)) (n (inc v))) a
                        (or (n (dec v)) (n (inc v))) (conj a v)
                        :e (-> a (conj v) (conj v))))
          []
          (sort n))))))

(defcheck solution-a81c8566
  #(letfn
    [(r [[a s l] n]
       (cond
         (nil? s) [a n n]
         (> n (inc l)) [(conj a [s l]) n n]
         :else [a s n]))]
     (let
      [[a s l] (reduce r [[] nil 0] (sort %))]
       (if (nil? s)
         a
         (conj a [s l])))))

(defcheck solution-a843c80d
  (fn [c]
    (let [[m :as c] (sort c)]
      (loop [[x & xs] c i [m m] acc []]
        (if x
          (if (> x (inc (i 1)))
            (recur xs [x x] (conj acc i))
            (recur xs (assoc i 1 x) acc))
          (if m (conj acc i) c))))))

(defcheck solution-a848fb9
  #(reduce
     (fn [c x]
       (if (and (seq c) (<= x (inc (last (last c)))))
         (assoc-in c [(dec (count c)) 1] x)
         (conj c [x x]))) [] (sort %)))

(defcheck solution-a86db0cd
  (fn intervals [numbers]
    (loop [remaining numbers
           result    []]
      (let [sorted     (sort (distinct remaining))
            increased? (zipmap (rest sorted) (map #(= (inc (first %)) (second %)) (partition 2 1 sorted)))]

        (if (empty? remaining)
          (for [interval result]
            [(first interval) (last interval)])
          (recur (drop-while increased? (rest sorted))
            (conj result (concat (take 1 sorted) (take-while increased? (rest sorted))))))))))

(defcheck solution-a8a6a932
  (fn [coll]
    (reduce (fn [acc val]
              (let [[start end] (peek acc)]
                (if (= end (dec val))
                  (conj (pop acc) [start val])
                  (conj acc [val val]))))
      []
      (sort (distinct coll)))))

(defcheck solution-a90f6c23
  (fn intervals [v]
    (->> (sort (distinct v))
      (keep-indexed #(vector (- %1 %2) %2))
      (group-by #(%1 0))
      vals
      (map (fn [e] (map #(% 1) e)))
      (mapv #(vector (first %) (last %))))))

(defcheck solution-aa1c5b7
  (fn [s] (
            partition 2 (flatten (
                                   map (fn [n] (
                                                 if (and
                                                     (some #{(inc n)} s)
                                                     (some #{(dec n)} s)
                                                     ) [] (if (or
                                                               (some #{(inc n)} s)
                                                               (some #{(dec n)} s)
                                                               ) [n] [n n])

                                                       )) (distinct (sort s))
                                   ))
            )))

(defcheck solution-aa6822d6
  (fn [xs]
    (let [xs (-> xs sort distinct)]
      (loop [a   (first xs)
             b   (first xs)
             xs  (rest xs)
             acc []]
        (if (empty? xs)
          (if (nil? a) acc (conj acc [a b]))
          (let [[f & r] xs]
            (if (or (= b f) (= b (dec f)))
              (recur a f r acc)
              (recur f f r (conj acc [a b])))))))))

(defcheck solution-aa6be8ff
  (fn [s]
    (if-not (seq s) []
                    (let [ss (into (sorted-set) s)]
                      (loop [complete-intervals []
                             [i j] [(first ss) (first ss)]
                             [n & rest] (rest ss)]
                        (if n
                          (if-not (= (inc j) n)
                            (recur (conj complete-intervals [i j]) [n n] rest)
                            (recur complete-intervals [i n] rest))
                          (conj complete-intervals [i j])))))))

(defcheck solution-aa7fe8e5
  (fn [s]
    (->> s
      set
      sort
      (reduce (fn [r v]
                (let [[start end] (last r)]
                  (if (= end (dec v))
                    (conj (vec (drop-last r)) [start v])
                    (conj r [v v]))))
        []))))

(defcheck solution-aaa9d396
  (fn f [c12]
    (let [c (sort (distinct c12))]
      (if (= 1 (count c)) [(concat c c)]
                          (for [x (loop [s   (rest c)
                                         r   [(first c)]
                                         res []]
                                    (if (and (empty? s) 1) res
                                                           (let [new-r   (cond
                                                                           (= 1 (- (first s) (last r))) (conj r (first s))
                                                                           true [(first s)])
                                                                 new-res (cond
                                                                           (< (count new-r) (count r)) (if (= 1 (count s)) (conj (conj res r) new-r) (conj res r))
                                                                           (= 1 (count s)) (conj res new-r)
                                                                           (= 1 (- (first s) (last r))) res
                                                                           true (conj res r))]
                                                             (recur (rest s)
                                                               new-r
                                                               new-res))))]
                            [(apply min x) (apply max x)]
                            )))))

(defcheck solution-aada8ee2
  (letfn [(intervals [coll]
            (loop [[x & xs] (sort coll)
                   [y & ys :as acc] '()]
              (cond
                (nil? x) (reverse acc)
                (nil? y) (recur xs (cons [x] acc))
                (<= (- x (last y)) 1) (recur xs (cons (conj y x) ys))
                :else (recur xs (cons [x] acc)))))]
    (fn [coll] (->> coll
                 intervals
                 (map (juxt first last))))))

(defcheck solution-aae2098d
  (fn [xs]
    (letfn [(runs [xs]
              (loop [prev nil rs [] r [] xs xs]
                (if (empty? xs)
                  (if (empty? r) rs (conj rs r))
                  (let [x (first xs)]
                    (if (and prev (< (- x prev) 2))
                      (recur x rs (conj r x) (rest xs))
                      (recur x (if (empty? r) rs (conj rs r)) [x] (rest xs)))))))]
      (map (fn [r] [(first r) (last r)]) (runs (sort xs))))))

(defcheck solution-aba02f5c
  (fn check [x]
    (let [s    (into #{} x)
          pair (fn [x] (map #(vector %1 %2) (take-nth 2 x) (take-nth 2 (rest x))))]
      (->>
        s
        (sort)
        (map #(vector % (contains? s (dec %)) (contains? s (inc %))))
        (filter (fn [[_ a b]] (not (and a b))))
        (mapcat (fn [[a b c]] (if (not (or b c)) [a a] [a])))
        (pair)

        )

      )))

(defcheck solution-ac0606c
  #(loop [[x & xs] (sort %) y x acc (if x [x x] [])] (if x (recur xs x (if (< (- x y) 2) (conj (pop acc) x) (conj acc x x))) (partition 2 2 acc))))

(defcheck solution-accfd348
  (fn [xs]
    (let [ys (map-indexed (fn [i e] [i e]) (distinct (sort xs)))
          r  (partition-by (fn [[i e]] (- e i)) ys)
          s  (map (fn [a] (map last a)) r)]
      (map (fn [a] [(first a) (last a)]) s))))

(defcheck solution-acf7d2c0
  (fn intervals [coll]
    (->> ((comp distinct sort) coll)
      (map-indexed vector)
      (partition-by #(- (first %) (last %)))
      (map (partial map last))
      (map #(vector (first %) (last %))))))

(defcheck solution-ad6b4642
  (letfn [(t [v]
            (loop [a []
                   r (if (empty? v) v
                                    (map #(if (= -1 (.indexOf v %)) nil %) (range (inc (apply max v)))))]
              (cond (empty? r) a
                    (nil? (first r)) (recur (conj a []) (rest r))
                    :else (recur (conj (pop a)
                                   (conj (peek a) (first r)))
                            (rest r)))))
          (x [r]
            (into [] (map #(vector (apply min %) (apply max %)) (remove empty? r))))]
    (comp x t)))

(defcheck solution-ad99b9db
  (fn [c]
    (if (empty? c) c
                   (let [s (sort c)]
                     (reduce #(if (<= %2 (inc (last (last %)))) (conj (pop %) [(first (last %)) %2])
                                                                (conj % [%2 %2])) [[(first s) (first s)]] s)))))

(defcheck solution-ae1ec417
  (letfn [
          (my-partition [s]
            (let [coll (sort s)]
              (last (reduce
                      #(let [[prev acc] %1]
                         (if (or (= prev %2) (= prev (dec %2)))
                           [%2 (conj (vec (butlast acc)) (conj (last acc) %2))]
                           [%2 (conj acc [%2])]))
                      [(first coll) [[]]]
                      coll))))
          (pack-partitions [s] (map #(vector (first %) (last %)) (filter not-empty s)))]
    #(pack-partitions (my-partition %))))

(defcheck solution-ae9f5adc
  #(letfn [(f [[a b & c]]
             (let [acc []]
               (cond
                 (nil? a) acc
                 (= a b) (concat acc (f (cons b c)))
                 (and a b) (if (= (inc a) b)
                             (concat acc (f (cons b c)))
                             (concat [a] [b] (f (cons b c))))
                 a (cons a acc))))]
     (partition 2 (cons (first (sort %)) (f (sort %))))))

(defcheck solution-aeca8cc6
  (fn [coll]
    (reduce
      (fn [s n]
        (if (empty? s) [[n n]]
                       (let [nxt (inc (last (last s)))]
                         (if (= nxt n)
                           (conj
                             (vec (drop-last s))
                             [(first (last s)) nxt])
                           (conj s [n n])))))
      []
      (sort (set coll)))))

(defcheck solution-aed50896
  (fn [coll]
    (if (empty? coll)
      (),
      ((fn step [nums]
         (lazy-seq
           (let [start (first nums)]
             (loop [end start, nums (rest nums)]
               (if (empty? nums)
                 [[start end]]
                 (let [x (first nums)]
                   (if (= x (inc end))
                     (recur x (rest nums))
                     (cons [start end] (step nums)))))))))
       (into (sorted-set) coll)))))

(defcheck solution-af463649
  (fn [xs]
    (if (empty? xs)
      []
      (let [sort-xs (apply sorted-set xs)]
        (loop [acc   []
               v-st  (first sort-xs)
               v-end (first sort-xs)
               ys    (next sort-xs)]
          (cond
            (empty? ys) (conj acc [v-st v-end])
            (= 1 (- (first ys) v-end)) (recur acc v-st (first ys) (next ys))
            :else (recur (conj acc [v-st v-end]) (first ys) (first ys) (next ys))))))))

(defcheck solution-af894b9d
  (fn [coll]
    (if (empty? coll)
      coll
      (let [sorted (sort coll)]
        (->> (rest sorted)
          (reduce
            (fn [acc n]
              (let [prevg (last acc)]
                (cond
                  (= (last prevg) n) acc
                  (< (inc (last prevg)) n) (conj acc [n])
                  :else (update-in acc [(dec (count acc))] conj n))))
            [[(first sorted)]])
          (map (juxt first last)))))))

(defcheck solution-aff2d83b
  (fn [v]
    (let [[h :as s] (sort v)]
      (reverse
        (reduce (fn [[f & r] [x y :as z]]
                  (if (or (= (inc x) y)
                          (= x y))
                    (conj r [(first f) y])
                    (conj r f [y y])))
          (if (empty? v) [] [[h h]])
          (partition 2 1 s))))))

(defcheck solution-b0013a70
  (fn [ns]
    (if (empty? ns)
      []
      (let [ss (sort (distinct ns))]
        (loop [ans [], start (first ss), current (first ss), rs (rest ss)]
          (if (empty? rs)
            (conj ans [start current])
            (if (= (inc current) (first rs))
              (recur ans start (inc current) (rest rs))
              (recur (conj ans [start current]) (first rs) (first rs) (rest rs)))))))))

(defcheck solution-b00a923e
  (fn [i]
    (letfn [(f [s]
              (lazy-seq
                (if (not-empty s)
                  (let [p (partition 2 1 s)
                        m (take-while (fn [[a b]] (< (- b a) 2)) p)
                        c (inc (count m))
                        [n o] (split-at c s)]
                    (cons [(first n) (last n)] (f o))))))]
      (f (sort i)))))

(defcheck solution-b0183066
  (fn intervall [x]
    (let [x (distinct (sort x))]
      ((fn f
         ([x] (let [[x & xs] x] (if x (f xs x x) [])))
         ([x l h]
          (if-let [[x & xs] x]
            (if (= (inc h) x)
              (f xs l x)
              (lazy-seq (cons [l h] (f xs x x))))
            [[l h]]))) x))))

(defcheck solution-b0266dd5
  (fn [coll]
    (if (empty? coll)
      []
      (let [v (distinct (sort coll))]
        (loop [cur (list (first v))
               tar (rest v)
               ret '()]
          (if (empty? tar)
            (for [r (reverse (cons (reverse cur) ret))]
              [(first r) (last r)])
            (if (= (inc (first cur)) (first tar))
              (recur (cons (first tar) cur) (rest tar) ret)
              (recur (list (first tar)) (rest tar) (cons (reverse cur) ret))
              )
            )
          )))))

(defcheck solution-b0afa405
  (fn intervals [xs]
    (if (empty? xs)
      []
      (let [s   (set xs)
            min (apply min s)
            max (apply max s)]
        (->> (partition-by #(contains? s %) (range min (inc max)))
          (take-nth 2)
          (map #(vector (first %) (last %))))))))

(defcheck solution-b0bd0c6c
  (fn [coll]
    (if (empty? coll)
      (empty coll)
      (let [sorted (sort coll)]
        (reduce #(let [tail   (last %)
                       rbound (second tail)]
                   (if (< (- %2 rbound) 2)
                     (conj (vec (butlast %)) (vector (first tail) %2))
                     (conj % (vector %2 %2))))
          [[(first sorted) (first sorted)]]
          (next sorted))))))

(defcheck solution-b0cbc501
  (fn [xs]
    (if (seq xs)
      (let [xs   (sort xs)
            idxs (->>
                   (map - (rest xs) xs)
                   (keep-indexed #(when (> %2 1) %1)))]
        (map (fn [p q]
               [(nth xs p) (nth xs q)])
          (cons 0 (map inc idxs))
          (concat idxs [(dec (count xs))])))
      [])))

(defcheck solution-b0f3e235
  (fn [s]
    (letfn [(findone [s]
              (let [r (range (first s) (+ (first s) (count s)))]
                (seq (map first (take-while (fn [[s r]] (= s r)) (map (fn [s r] [s r]) s r))))))
            (f [s]
              (if (empty? s) []
                             (let [s1 (findone s)]
                               (cons s1 (f (drop (count s1) s))))))]
      (let [r (-> s set sort)]
        (map (fn [s] [(first s) (last s)]) (f r))))))

(defcheck solution-b1a54e78
  (fn [nums]
    (if (empty? nums)
      []
      (let [[initial-num & rest-nums] (sort nums)]
        (loop [nums             rest-nums
               interval-initial initial-num
               prev-num         initial-num
               accum            []]
          (if nums
            (let [next-num (first nums)]
              (if (< (- next-num prev-num) 2)
                (recur (next nums) interval-initial next-num accum)
                (recur (next nums) next-num next-num (conj accum [interval-initial prev-num]))))
            (conj accum [interval-initial prev-num])))))))

(defcheck solution-b1a5b6bf
  ;phone formatting
  (fn t [v]
    (let [s  (sort v)
          v2 (vector (take 2 s))]
      (if-let [s2 (nnext s)]
        (reduce
          #(let [bl (pop %)
                 l  (peek %)
                 ll (last l)]
             (if (< (inc ll) %2)
               (conj % [%2 %2])
               (conj bl [(first l) %2])))

          v2
          (vec s2))
        []))))

(defcheck solution-b2afcd36
  (fn [c]
    (if (empty? c)
      []
      (apply conj (reduce (fn [[r [a b]] x]
                            (if (nil? a)
                              [r [x x]]
                              (if (= x (inc b))
                                [r [a x]]
                                [(conj r [a b]) [x x]])))
                    [[] []]
                    (sort (distinct c)))))))

(defcheck solution-b2f06d7a
  #(if-let [[x & xs] (->> % sort not-empty)]
     (reduce (fn [intervals n]
               (let [l        (count intervals)
                     last-pos [(dec l) 1]
                     m        (get-in intervals last-pos)]
                 (if (>= 1 (- n m))
                   (assoc-in intervals last-pos n)
                   (assoc-in intervals [l] [n n]))))
       [[x x]] xs)
     ()))

(defcheck solution-b2fc8fe
  (fn intervals [c]
    (let [c (vec (sort (set c)))
          d (vec (#(map - (rest %) %) c))
          v (flatten
              (map-indexed
                #(if (= 1 %2)
                   [(c %1) (c (inc %1))]
                   nil)
                d))]
      (loop [o (vec (map #(into [%] [%])
                      (clojure.set/difference
                        (set c)
                        (set v))))
             v (drop-while nil? v)]
        (if-let [a (first v)]
          (recur (conj o [a (last (take-while (complement nil?) v))])
            (drop-while nil? (drop-while (complement nil?) v)))
          (sort-by first o))))))

(defcheck solution-b30b7a81
  #(if (empty? %) []
                  (let [vs (vec (sort %)) vl (first vs) vh (last vs)]
                    (first (reduce (fn [[is b e] v]
                                     (if (or (= v e) (= v (+ e 1)))
                                       [is b v]
                                       [(conj is [b e]) v v]))
                             [[] vl vl]
                             (conj vs (+ vh 2)))))))

(defcheck solution-b35dd66c
  (fn [[& ints]]
    (loop [remaining (set ints)
           intervals []]
      (if (seq remaining)
        (let [begin (apply min (seq remaining))
              inseq (take-while remaining (drop begin (range)))
              end   (last inseq)]
          (recur (into #{} (filter #(< end %) remaining))
            (conj intervals [begin end])))
        intervals))))

(defcheck solution-b37edb43
  (fn [col]
    (letfn [(f
              ([a [x & xs]] (if x (f (conj a x) x xs) a))
              ([a l [x & xs :as cs]]
               (if x
                 (recur (if (<= x (inc l)) a (conj a l x)) x xs)
                 (partition 2 (conj a l)))))]
      (f [] (sort col)))))

(defcheck solution-b45b207f
  (fn intervals [c]
    (if (empty? c) c
                   (->>
                     c distinct sort
                     (#(concat [(first %)] % [(last %)]))
                     (partition 2 1)
                     (filter (fn [[a b]] (not= b (inc a))))
                     (apply concat)
                     rest drop-last
                     (partition 2)))))

(defcheck solution-b49c6aa8
  #(reduce
     (fn [intervals newVal]
       (if-let [[s e] (last intervals)]
         (cond
           (= newVal e) intervals
           (= (inc e) newVal) (conj (vec (drop-last intervals)) [s newVal])
           :else (conj intervals [newVal newVal]))
         (conj () [newVal newVal])))
     [] (sort %)))

(defcheck solution-b4a4ef5c
  (fn [x]
    (partition 2
      (keep #({} % %)
        (let [[a] (sort x)]
          (reduce #(if (#{0 1} (- %2 (last %)))
                     (conj (pop %) %2)
                     (conj % %2 %2))
            [a a] (sort x)))))))

(defcheck solution-b4c2b5e
  (fn intervals
    ([n]
     (if (empty? n)
       []
       (let [n (sort (distinct n))]
         (intervals (first n) (first n) [] (rest n)))))
    ([x y i n]
     (cond
       (empty? n) (conj i [x y])
       (= (first n) (inc y)) (intervals x (inc y) i (rest n))
       :else (intervals (first n) (first n) (conj i [x y]) (rest n))))))

(defcheck solution-b4fb209f
  (fn [s]
    (let [s (apply sorted-set s)]
      (map vector
        (remove #(s (dec %)) s)
        (remove #(s (inc %)) s)))))

(defcheck solution-b514a103
  (fn [s]
    (letfn [(already-inside [n ranges] (some (fn [[a b]] (<= a n b)) ranges))
            (adjoining [n ranges] (set (take 2 (filter (fn [[a b]] (some #(= n %) [(dec a) (inc b)])) ranges))))
            (combine [ranges n]
              (if (already-inside n ranges)
                ranges
                (let [to-remove (adjoining n ranges)]
                  (if (seq to-remove)
                    (conj (apply disj ranges to-remove) [(apply min n (flatten (seq to-remove)))
                                                         (apply max n (flatten (seq to-remove)))])
                    (conj ranges [n n])))))]
      (sort-by first (vec (reduce combine #{} s))))))

(defcheck solution-b5736b13
  (fn [nums]
    (if-not (seq nums)
      nums
      (let [[n & ns] (sort nums)]
        (loop [xs ns, low n, high n, ivals []]
          (if (seq xs)
            (let [[x & more] xs]
              (if (<= x (inc high))
                (recur more low x ivals)
                (recur more x x (conj ivals [low high]))))
            (conj ivals [low high])))))))

(defcheck solution-b6198098
  (fn [s]
    (loop [[i & is] (sort s)
           r ()]
      (if i
        (recur is (if (empty? r)
                    [[i i]]
                    (let [a (ffirst r)
                          b (second (first r))]
                      (cond
                        (<= i b) r
                        (= (+ 1 b) i) (conj (rest r) [a i])
                        :else (conj r [i i])))))
        (reverse r)))))

(defcheck solution-b62ae066
  (fn consecutive-intervals [coll]
    (if (empty? coll)
      []
      (loop [sorted (into [] (sort coll)) ans [] current-start (first (into [] (sort coll)))]
        (if (empty? (rest sorted))
          (conj ans [current-start (first sorted)])
          (if (= (inc (first sorted)) (first (rest sorted)))
            (recur (rest sorted) ans current-start)
            (if (= (first sorted) (first (rest sorted)))
              (recur (rest sorted) ans current-start)
              (recur (rest sorted) (conj ans [current-start (first sorted)]) (first (rest sorted))))))))))

(defcheck solution-b678decb
  (fn [colls]
    (let [xs (sort colls)]
      (reduce
        (fn [acc x]
          (if
           (empty? acc) [[x x]]
                        (loop [i 0]
                          (if (>= i (count acc)) (conj acc [x x])
                                                 (let [[a b] (nth acc i)]
                                                   (cond
                                                     (<= a x b) acc
                                                     (= x (inc b)) (assoc acc i [a x])
                                                     (= x (dec a)) (assoc acc i [x b])
                                                     :else (recur (inc i))))))))

        [] xs))))

(defcheck solution-b67ae635
  (fn [c]
    (reverse (reduce
               (fn f [[[s e :as h] & t :as a] x]
                 (cond
                   (empty? h) [[x x]]
                   (= x e) a
                   (= x (inc e)) (cons [s x] t)
                   :else (cons [x x] a)))
               []
               (sort c)))))

(defcheck solution-b67e8ebd
  (fn get-intervals [lst]
    (if (empty? lst)
      []
      (let [values  (set lst)
            min-val (apply min lst)
            max-val (apply max lst)]
        (map #(vector (first %) (last %))
          (filter #(not (or (empty? %) (nil? (first %))))
            (partition-by nil?
              (map values (range min-val (inc max-val))))))))))

(defcheck solution-b7b6e27d
  (fn f [s]
    (if (empty? s) []
                   (let [mx (apply max s) mn (apply min s)]
                     (let [[d] (remove (set s) (range mn mx))]
                       (if (nil? d) [[mn mx]]
                                    (cons [mn (dec d)] (f (filter #(> % d) s)))))))))

(defcheck solution-b7bd5580
  (fn interval
    [s]
    (let [groups (partition-by first (map-indexed (juxt - (fn [a b] b)) (apply sorted-set s)))]
      (map (comp (juxt first last) (partial map second)) groups))))

(defcheck solution-b80e860a
  (fn f
    ([v]
     (if (empty? v) []
                    (let [[ft & rt] (sort v)]
                      (f rt ft ft []))))
    ([[ft & rt] start end res]
     (cond
       (nil? ft) (conj res [start end])
       (= end ft) (f rt start end res)
       (not= (inc end) ft) (f rt ft ft (conj res [start end]))
       :else (f rt start (inc end) res)))))

(defcheck solution-b84db9c0
  (comp
   sort
   set
   (fn [x]
     (map
       #(let [f (fn f [n g] (if ((set x) (g n)) (f (g n) g) n))]
          [(f % dec) (f % inc)])
       x))))

(defcheck solution-b8979012
  #(if (empty? %)
     %
     (loop [[f & r] (sort %) [a b :as curr] [f f] res []]
       (if f
         (if (<= f (inc b))
           (recur r [a f] res)
           (recur r [f f] (conj res curr)))
         (conj res curr)))))

(defcheck solution-b897eb7f
  #(if (seq %)
     (let [n (set %), a (apply min %), b (+ 1 (apply max %))]
       (for [l (take-nth 2 (partition-by (comp boolean n) (range a b)))]
         [(first l) (last l)]))
     []))

(defcheck solution-b8c33f
  (fn interval [coll]
    (if (empty? coll) []
                      (let [
                            ss        (apply sorted-set coll)
                            s-min     (first ss)
                            s-max     (last ss)
                            s-missing (first (filter (complement ss) (range s-min (inc s-max))))]
                        (if (nil? s-missing)
                          (list [s-min s-max])
                          (cons [s-min (dec s-missing)] (interval (filter (partial < s-missing) ss))))))))

(defcheck solution-b8f39aa2
  (fn intervals [v]
    (if (empty? v) []
                   (let [sv (sort v)

                         fv (first sv)
                         rv (rest sv)

                         [a, i, i2] (reduce

                                      (fn [[a, i, i2] x]
                                        (if (> (- x i2) 1)
                                          [(conj a [i, i2]), x, x]
                                          [a, i, x]))


                                      [[], fv, fv]
                                      rv)]

                     (conj a [i, i2])))))

(defcheck solution-b91f488a
  (fn [a]
    (if (empty? a)
      a
      (map #(vector (first %) (last %))
        (loop [col (sort (distinct a))
               tmp []
               acc []
               lst (dec (first col))]
          (if-not (empty? col)
            (recur (rest col)
              (if (= 1 (- (first col) lst))
                (conj tmp (first col))
                [(first col)])
              (if (= 1 (- (first col) lst))
                acc
                (conj acc tmp))
              (first col))
            (conj acc tmp)))))))

(defcheck solution-b92ef1f6
  (fn [s]
    (if-let [s (seq (sort s))]
      (#(partition 2 `[~(first s) ~@% ~(last s)]) (mapcat #(when (pos? (- %1 %2 1)) [%2 %1]) (next s) s))
      ())))

(defcheck solution-b9586e93
  (fn f
    ([xs]
     (if (empty? xs) []
                     (let [[x & r] (sort xs)]
                       (f [[x x]] r))))
    ([is xs]
     (if (empty? xs) is
                     (let [[x & r] xs]
                       (if (>= (-> is last last inc) x)
                         (f (vec (concat (drop-last is) [[(first (last is)) x]])) r)
                         (f (conj is [x x]) r)))))))

(defcheck solution-b99f4cbf
  (fn
    [c]
    (let [sc (sort c)
          t  (reductions = true
               (map #(> 2 (- %2 %)) sc (rest sc)))]
      (map #(
              vector (first %) (last %))
        (map #(map first %)
          (partition-by second (map list sc t)))))))

(defcheck solution-b9f61fff
  (fn [s]
    (reduce
      (fn [r v]
        (let [[s e] (last r)]
          (if (= e (dec v))
            (conj (vec (butlast r)) [s v])
            (conj r [v v]))))
      []
      (sort (set s)))))

(defcheck solution-ba7ecc71
  (fn [coll]
    (map
      (fn [interval]
        (if (= (count interval) 1)
          (vec (take 2 (repeat (first interval))))
          [(first interval) (last interval)]
          )
        )
      (loop [intervals [] items (sort coll)]
        (cond
          (empty? items) intervals
          (empty? intervals) (recur (conj intervals [(first items)]) (rest items))
          (> (- (first items) (last (last intervals))) 1) (recur (conj intervals [(first items)]) (rest items))
          :else
          (recur (conj (vec (butlast intervals)) (conj (last intervals) (first items))) (rest items))
          )
        )
      )
    ))

(defcheck solution-bac73cc
  (fn __ [coll]
    (if (seq coll)
      (let [sorted (distinct (sort coll))
            red    (fn [[out head prev] in]
                     (cond
                       (= (inc prev) in) [out head in]
                       :else [(conj out [head prev]) in in]))
            [out last-first last-last]
            (reduce red [[] (first sorted) (first sorted)] (rest sorted))]
        (conj out [last-first last-last]))
      coll)))

(defcheck solution-bacc889e
  (fn [l]
    (reverse
      (reduce
        (fn [[[a b] & l :as all] n]
          (if (or (nil? a) (not= (inc b) n))
            (cons [n n] all)
            (cons [a n] l)))
        nil (sort (set l))))))

(defcheck solution-bb1313e8
  (fn [int_seq]
    (if (empty? int_seq) int_seq
                         (let [s (sort int_seq)]
                           (loop [r [] start_ind 0 end_ind 1]
                             (if (= end_ind (count s)) (conj r [(nth s start_ind) (nth s (dec end_ind))])
                                                       (let [start (nth s (dec end_ind)) end (nth s end_ind)]
                                                         (if (<= (- end start) 1)
                                                           (recur r start_ind (inc end_ind))
                                                           (recur (conj r [(nth s start_ind) (nth s (dec end_ind))]) end_ind (inc end_ind))
                                                           )
                                                         )
                                                       )
                             )
                           )
                         )
    ))

(defcheck solution-bb1b7d8c
  (fn [s]
    (if (empty? s) []
                   (let [s (sort s)]
                     (letfn [(f [ivs a b xs]
                               (if (empty? xs) (conj ivs [a b])
                                               (let [x (first xs)]
                                                 (cond (= x b) (f ivs a b (rest xs))
                                                       (= x (inc b)) (f ivs a x (rest xs))
                                                       :else (f (conj ivs [a b]) x x (rest xs))))))]
                       (f [] (first s) (first s) (rest s)))))))

(defcheck solution-bb36a031
  (fn intervals [nums]
    (if (empty? nums) []
                      (let [[p & r] (sort nums)
                            conj-seg (fn [ret seg p] (conj ret (conj seg p)))]
                        (apply conj-seg
                          (reduce (fn [[ret seg p] c]
                                    (if (< (- c p) 2) (list ret seg c)
                                                      (list (conj-seg ret seg p) [c] c)))
                            (list [] [p] p) r))))))

(defcheck solution-bb60088c
  (fn intervals [coll]
    "Return the sequence of intervals found in coll."
    (let [last-seen       (atom -1)
          coll            (sort coll)
          last-seen       (atom (first coll))
          switch          (atom true)                       ;; Should partition-by partition? We don't care about its value, just the value change.
          inc-or-identity (fn [x]
                            (if (> (- x @last-seen) 1)      ;; Previous isn't equal or 1 less: partition.
                              (reset! switch (not @switch)))
                            (reset! last-seen x)
                            @switch)
          to-intervals    (fn [coll] [(first coll) (last coll)])]
      (map to-intervals (partition-by inc-or-identity coll)))))

(defcheck solution-bbc100ef
  (fn
    [vs]
    (let [vs  (distinct (sort vs))
          pt  (partition 2 1 vs)                            ; partition in two with step 1
          cnt (count vs)]
      (cond
        (= 1 cnt) [[(first vs) (first vs)]]
        (= 0 cnt) []
        :d (reduce
             (fn [sm [a b]]
               (let [c (count sm)]
                 (if (or (= a b) (= (inc a) b))
                   (if (empty? (last sm))                   ; Updating interval
                     (assoc-in sm [(dec c)] [a b])
                     (assoc-in sm [(dec c) 1] b))
                   (assoc-in sm [c] [b b]))))
             [[]]
             pt)))))

(defcheck solution-bbe88cac
  (fn [l]
    (let [process (fn [lists x]
                    (if-let [[l u] (last lists)]
                      (if (= x (inc u))
                        (concat (butlast lists) [[l x]])
                        (concat lists [[x x]]))
                      (concat lists [[x x]])))]
      (reduce process [] (sort (set l))))))

(defcheck solution-bbf736b3
  (fn [xs]
    (letfn [(f [acc v]
              (if (and (not-empty acc) (< (Math/abs (- (ffirst acc) v)) 2))
                (cons (cons v (first acc)) (rest acc))
                (cons [v] acc)))]
      (sort-by first (map #(vector (apply min %) (apply max %))
                       (reduce f [] (sort xs)))))))

(defcheck solution-bbfd666d
  (fn [l]
    (let [sl (sort l)]
      (if (empty? sl)
        []
        (loop [m (rest sl)
               r []
               s (first sl)
               e (first sl)]
          (if (empty? m)
            (conj r [s e])
            (let [ne (first m)]
              (if (<= ne (inc e))                           ; same oreq inc
                (recur (rest m) r s ne)
                (recur (rest m) (conj r [s e]) ne ne)))))))))

(defcheck solution-bc723603
  (fn [ns]
    (if (= ns [])
      []
      (let [is (sort (loop [[n & ns] ns is '()]
                       (let [nis (for [[min max] is]
                                   (cond (and (<= n max) (>= n min)) [n min max]
                                         (= n (inc max)) [min n]
                                         (= n (dec min)) [n max]
                                         :else [min max]))
                             nis (if (= nis is)
                                   (cons [n n] nis)
                                   (for [[n & r :as rs] nis]
                                     (if (= 3 (count rs))
                                       r
                                       rs)))]
                         (if ns
                           (recur ns nis)
                           nis))))
            is (loop [[[a b] & iss] is
                      nis []]
                 (let [mergei (for [[x y] iss
                                    :when (or (and (<= a y) (>= a x))
                                              (and (<= b y) (>= b x)))]
                                (if (<= a y) [a y] [x b]))
                       [nis iss] (if (empty? mergei)
                                   [(concat nis [[a b]]) iss]
                                   [(concat nis mergei) (rest iss)]
                                   )
                       ]
                   (if iss
                     (recur iss nis)
                     nis)))]
        is))))

(defcheck solution-bd477d18
  #(if (empty? %)
     []
     (reduce (fn [coll n]
               (let [[x y] (last coll)]
                 (if (< (- n y) 2)
                   (conj (vec (butlast coll))
                     [x n])
                   (conj coll [n n]))))
       [[1 1]]
       (sort %))))

(defcheck solution-be5fef88
  (fn [l]
    (->> l distinct sort
      (map-indexed #(list (- %2 %) %2))
      (partition-by first)
      (map #(list (last (first %)) (last (last %)))))))

(defcheck solution-bede8b37
  (fn [s]
    (->> (apply sorted-set s)
      (reduce (fn [c x] (if ((fnil < 0) (last (last c)) (dec x)) (conj c [x])
                                                                 (conj (vec (drop-last c)) (conj (vec (last c)) x)))) [])
      (map #((juxt first last) %))
      )

    ))

(defcheck solution-bf650b42
  (fn intervals [coll]
    (let [interval-groups (fn interval-groups [coll]
                            (lazy-seq
                              (let [[head & tail :as all] (sort coll)]
                                (when head
                                  (let [run (reduce (fn [acc x]
                                                      (if (or (= (inc (last acc)) x) (= (last acc) x))
                                                        (conj acc x)
                                                        acc))
                                              [head] tail)]
                                    (cons run (intervals (drop (count run) all))))))))]
      (map #(list (first %) (last %)) (interval-groups coll)))))

(defcheck solution-bfe29ce
  (fn intervals [coll]
    (let [c (sort coll)]
      (cond
        (empty? coll) []
        :else
        (reduce (fn [x y]
                  (cond
                    (>= (- y (last (last x))) 2) (conj x [y y])
                    :else (conj (vec (butlast x)) [(first (last x)) y])))
          [[(first c)]] (rest c))
        ))))

(defcheck solution-c01888ab
  (fn intervals [num-coll]
    (let [intervals (reduce
                      (fn [accum [interval-min :as new-interval]]
                        (if (= (peek (peek accum)) (dec interval-min))
                          (update-in accum [(dec (count accum)) 1] inc)
                          (conj accum new-interval)))
                      [[]]
                      (mapv (fn [x] [x x]) (sort (distinct num-coll))))]
      (condp = 1
        (count intervals) []
        1 (next intervals)))))

(defcheck solution-c0e14527
  (fn intervals [col]
    (if (empty? col) []
                     (let [c (distinct (sort col))]
                       (loop [rem (rest c) g [(first c)] int []]
                         (let [l (last g)
                               n (first rem)]
                           (cond
                             (empty? rem) (conj int [(first g) (last g)])
                             (= (inc l) n) (recur (rest rem) (conj g n) int)
                             :else (recur (rest rem) [n] (conj int [(first g) (last g)])))))))))

(defcheck solution-c11d6a39
  (fn [col]
    (reduce #(if (= (last (last %1)) (dec %2))
               (assoc-in %1 [(dec (count %1)) 1] %2)
               (into %1 [[%2 %2]])) [] (sort (set col)))))

(defcheck solution-c189920d
  (fn intervals [s]
    (vec
      (map #(vec [(first %) (last %)])
        (map #(map last %)
          (partition-by #(apply - %) (map-indexed vector (sort (distinct s)))))))))

(defcheck solution-c3684682
  #(loop [[l & s] (sort (set %)) b l r []]
     (cond (nil? l) []
           (nil? s) (conj r [b l])
           (= 1 (- (first s) l)) (recur s b r)
           :else (recur s (first s) (conj r [b l])))))

(defcheck solution-c3755f80
  (fn [c]
    (let [grouped (reduce (fn [r n] (if (or (= (last (last r)) (dec n)) (= (last (last r)) n)) (concat (drop-last r) [(conj (last r) n)]) (concat r [[n]]))) [] (sort c))]
      (map #(vec [(first %) (last %)]) grouped))))

(defcheck solution-c3a3d4d3
  (fn [s]
    (letfn [(find-next [ss]
              (->> ss
                (map-indexed #(vector %1 %2))
                (take-while (fn [[i v]] (= (first ss) (- v i))))
                (map second)))
            (f [ss]
              (if (first ss)
                (let [intvl (find-next ss)]
                  (cons intvl (f (drop (count intvl) ss))))))]
      (map #(vector (first %) (last %)) (f (sort (apply list (set s))))))))

(defcheck solution-c3e1bb1e
  (fn [s]
    (let [ss  (-> s sort distinct)
          add (fn [[e t] x] (if (= 1 (- x t)) [e x] [x x]))]
      (map last (partition-by first (next (reductions add [0 (first ss)] ss)))))))

(defcheck solution-c3e53672
  (fn intervals [s]
    (if (empty? s)
      []

      (let [numbers       (sort (distinct s))
            change-points (first
                            (reduce (fn [[result last] el]
                                      (if (> el (inc last))
                                        [(into result
                                           [last el])
                                         el]
                                        [result el]))
                              [[] 0]
                              numbers))
            ]
        (partition-all 2 (into [(first numbers)]
                           (conj change-points
                             (last numbers))))))
    ))

(defcheck solution-c40b3ea4
  (fn [c]
    (->> (set c) sort
      (mapcat #(do [((set c) (dec %)) %]))
      (partition-by nil?)
      (remove (comp nil? first))
      (map #(do [(first %) (last %)])))))

(defcheck solution-c4569834
  (fn intervals
    [coll]
    (->> (sort coll)
      (reduce (fn [acc x]
                (if (empty? acc)
                  (conj acc [x x])
                  (let [[y z] (last acc)]
                    (if (or (nil? z)
                            (= z x)
                            (= z (dec x)))
                      (conj (vec (butlast acc)) [y x])
                      (conj acc [x x])))))
        []))))

(defcheck solution-c50205ca
  (fn [a]
    (let [a (-> a set sort)
          s (set a)]
      (partition 2
        (interleave
          (filter #(not (s (dec %))) a)
          (filter #(not (s (inc %))) a))))))

(defcheck solution-c5cef317
  (fn intervals [s]
    (if (empty? s)
      s
      (let [sort-unique (sort (distinct s))
            map-vec     (map vector sort-unique (cons (dec (first sort-unique)) sort-unique))
            [vec1 vec2] (split-with #(= 1 (- (first %) (second %))) map-vec)
            [vec1m tail] [(map first vec1) (into [] (map first vec2))]
            head        (into [] (if (= 1 (count vec1m)) [(first vec1m) (first vec1m)] vec1m))]
        (cons [(first head) (last head)] (intervals tail))))))

(defcheck solution-c5fb43f3
  (fn intervals
    ([coll] (intervals (distinct (sort coll)) []))
    ([coll acc]
     (cond
       (and (empty? coll) (not-empty acc)) [[(first acc) (last acc)]]
       (empty? coll) []
       (empty? acc) (intervals (next coll) [(first coll)])
       (= (inc (last acc)) (first coll)) (intervals (next coll) (conj acc (first coll)))
       :else (concat [[(first acc) (last acc)]] (intervals coll []))))))

(defcheck solution-c6265331
  (fn [coll]
    (if (empty? coll)
      []
      (let [in (vec (partition 2 1 (sort coll)))]
        (vec
          (map #(vec %)
            (partition 2
              (flatten
                (conj
                  []
                  (first (first in))
                  (reduce #(if (> (Math/abs (- (first %2) (second %2))) 1)
                             (conj %1 (first %2) (second %2))
                             %1)
                    []
                    in)
                  (second (last in)))))))))))

(defcheck solution-c7c767a1
  (fn [x]
    (map #(vector (first %) (last %))
      (reduce #(if (or (empty? %1) (= %2 (dec (ffirst %1))))
                 (cons (cons %2 (first %1)) (rest %1))
                 (cons `(~%2) %1)) '() (reverse (sort (distinct x)))))))

(defcheck solution-c7cfd374
  (fn intervals [s]
    (let [ordered (distinct (sort s))]
      (loop [x ordered currentseq [] result []]
        (if (= 0 (count x))
          (if (empty? currentseq) result (conj result currentseq))
          (let [ns1        (if (empty? currentseq) [(first x) (first x)] currentseq)
                inseq?     (or (= (first x) (+ 1 (last ns1))) (= (first ns1) (second ns1) (first x)))
                nextseq    (if inseq? [(first ns1) (first x)] [(first x) (first x)])
                nextresult (if (not inseq?) (conj result currentseq) result)]
            (recur (next x) nextseq nextresult)
            )
          )
        )
      )
    ))

(defcheck solution-c83e9612
  (fn intervalsX [v]
    (letfn [
            (intervalsRec [x b]
              (cond
                (empty? x) [b]
                (= (last b) (first x)) (intervalsRec (rest x) b)
                (= (inc (last b)) (first x)) (intervalsRec (rest x) (conj b (first x)))
                :else (cons b (intervalsRec (rest x) [(first x)]))
                )
              )

            (toInterval [x]
              [(first x) (last x)]
              )
            ]
      (if (empty? v)
        []
        (let [
              x (sort v)
              ]
          (map toInterval (intervalsRec (rest x) [(first x)]))
          )
        )
      )
    ))

(defcheck solution-c8593286
  (fn interval-split [coll]
    (letfn [(extensible? [end item]
              (or (= item end)
                  (= item (inc end))))

            (looper [coll]
              (loop [items (rest coll)
                     start (first coll)
                     end   (first coll)
                     acc   []]
                (cond
                  (empty? items) (conj acc [start end])
                  (extensible? end (first items)) (recur (rest items)
                                                    start
                                                    (first items)
                                                    acc)
                  :else (recur (rest items)
                          (first items)
                          (first items)
                          (conj acc [start end])))))]
      (if-not (seq coll)
        []
        (looper (sort coll))))))

(defcheck solution-c8c720a8
  (fn [xs]
    (map
      #(vector (->> % first last) (->> % last last))
      (partition-by
        #(apply - %)
        (map-indexed vector (apply sorted-set xs))))))

(defcheck solution-c925453e
  (fn intervals [v]
    (reduce
      (fn [lst x]
        (if (empty? lst) [[x x]]
                         (let [hd (last lst)
                               tl (subvec lst 0 (dec (count lst)))
                               a  (first hd)
                               b  (second hd)]
                           (cond
                             (= b x) (conj tl [a b])
                             (= (inc b) x) (conj tl [a x])
                             :else (conj tl [a b] [x x])))))
      []
      (sort v))
    ))

(defcheck solution-c9721c15
  (fn interval [ps]
    (loop [s  (rest (sort ps))
           a  []
           cb (first (sort ps))
           ce cb]
      (if (seq s)
        (cond (= ce (first s))
              (recur (rest s) a cb ce)
              (= (inc ce) (first s))
              (recur (rest s) a cb (inc ce))
              :else
              (recur (rest s) (conj a [cb ce]) (first s) (first s)))
        (if cb
          (conj a [cb ce])
          a)))))

(defcheck solution-ca3c3651
  (fn [xs]
    (->> (sort xs)
      (reduce
        (fn [acc x]
          (if (empty? acc)
            (cons [x x] ())
            (let [[a b] (first acc)]
              (if (or (= b x) (= (inc b) x))
                (cons [a x] (rest acc))
                (cons [x x] acc)))))
        ())
      (reverse))))

(defcheck solution-ca71887
  (fn [xs]
    (if-let [s (seq (sort xs))]
      (loop [res [], start (first s), more (next s), end start]
        (if-let [nxt (first more)]
          (cond
            (= nxt end) (recur res start (next more) end)
            (= 1 (- nxt end)) (recur res start (next more) nxt)
            :else (recur (conj res [start end]) nxt (next more) nxt))
          (conj res [start end])))
      [])))

(defcheck solution-cad3aca9
  (fn [coll]
    (reverse
      (let [sorted-coll (seq (apply sorted-set coll))]
        (reduce (fn [res-coll i]
                  (if (empty? res-coll)
                    (conj res-coll [i i])
                    (let [start (first (first res-coll))
                          end   (second (first res-coll))]
                      (if (= (inc end) i)
                        (conj (rest res-coll) [start i])
                        (conj res-coll [i i])))))
          []
          sorted-coll)))))

(defcheck solution-cafcfe18
  (fn [xs]
    (let [sxs      (sort xs)
          mark-gap (fn [[f s]] (if (< (- s f) 2) [s] [nil s]))]
      (->>
        (map vector (cons (first sxs) sxs) sxs)
        (mapcat mark-gap)
        (partition-by nil?)
        (remove #(nil? (first %)))
        (map (fn [grp] [(first grp) (last grp)]))))))

(defcheck solution-cb6cd0c7
  (fn [ls]
    (sort
      (loop [ls   (distinct (sort ls))
             part [(first ls) (first ls)]
             ans  []]
        (if (<= (count ls) 1)
          (if (nil? (first part)) ans (conj ans part))
          (if (= (+ (first ls) 1) (second ls))
            (recur (rest ls) [(first part) (second ls)] ans)
            (recur (rest ls) [(second ls) (second ls)] (cons part ans))))))))

(defcheck solution-cbec8ecd
  (fn [c]
    (let [s    (sort c)
          gaps (filter (fn [[a b]] (> b (inc a))) (partition 2 1 s))
          ivs  (conj (vec (cons (first s) (flatten gaps))) (last s))]
      (if (empty? c) [] (partition 2 ivs)))))

(defcheck solution-ccad7b3b
  (comp
   reverse
   (partial reduce
     (fn [[[b e] & t] n]
       (if b
         (if (= n (inc e))
           `[~[b n] ~@t]
           `[~[n n] ~[b e] ~@t])
         [[n n]]))
     [])
   distinct
   sort))

(defcheck solution-cd486809
  (fn [s]
    (loop [intervals [] prev nil data (sort (into [] (set s)))]
      (if (empty? data)
        intervals
        (do
          (let [[head & remaining] data]
            (if (= prev (dec head))
              (recur (assoc intervals (dec (count intervals)) [(first (last intervals)) head]) head remaining)
              (recur (into intervals [[head head]]) head remaining)
              )))))))

(defcheck solution-cd7f5293
  (fn s
    ([coll] (if-let [[h & m] (seq (sort (distinct coll)))] (s [h h] m) coll))
    ([[a b] coll]
     (if-let [[h & m] (seq coll)]
       (if (= (inc b) h)
         (s [a h] m)
         (cons [a b] (s [h h] m)))
       (list [a b])))))

(defcheck solution-ce11c3c2
  (fn [a]
    (let [[f & b] (distinct (sort a))]
      (loop [f f l f [n & r] b a []]
        (cond (nil? f) a
              (= (+ 1 l) n) (recur f n r a)
              :else (recur n n r (conj a [f l])))))))

(defcheck solution-ce361abf
  (fn invals
    ([col]
     (let [col (sort col)]
       (invals col (first col))))
    ([col start]
     (let [col-count (count col)]
       (cond
         (= col-count 0) []
         (= col-count 1) [[start (first col)]]
         :else
         (let [[l r] col
               [_ & other] col]
           (if (< 1 (- r l))
             (cons [start l] (invals other r))
             (invals other start))))))))

(defcheck solution-ce39e773
  (fn [col]
    (let [groupby (fn [func col] (reduce (fn [acc e]
                                           (if (func (last (last acc)) e)
                                             (conj (apply vector (butlast acc)) (conj (last acc) e))
                                             (conj acc [e]))) [[(first col)]] (rest col)))]
      (remove #(some nil? %)
        (map #(vector (first %) (last %))
          (groupby #(< (- %2 %) 2) (sort col)))))))

(defcheck solution-ce6d594a
  #(->> %
     set
     sort
     (reduce (fn [[[a b] & t :as s] c] (if (= b (dec c)) (cons [a c] t) (cons [c c] s))) ())
     reverse))

(defcheck solution-cec2bbdf
  (fn [xs]
    (if (empty? xs) []
                    (loop [is [] c [] xs (sort xs)]
                      (if (empty? xs)
                        (map #(vector (apply min %) (apply max %)) (conj is c))
                        (if (or (zero? (count c))
                                (= (last c) (first xs))
                                (= (last c) (dec (first xs))))
                          (recur is (conj c (first xs)) (rest xs))
                          (recur (conj is c) [(first xs)] (rest xs))
                          ))))))

(defcheck solution-cf66975c
  (fn [v] (map #(vector (first %) (last %))
            (remove #(= '(nil) %)
              (partition-by nil? (reduce #(if (or (empty? %1) (= %2 (inc (last %1)))) (conj %1 %2) (conj %1 nil %2)) [] (sort (distinct v))))))))

(defcheck solution-cfb8829f
  (fn [coll]
    (->> coll
      (sort)
      (distinct)
      (reduce (fn [acc n]
                (cond
                  (empty? acc)
                  [[n n]]

                  (= n (inc (second (first acc))))
                  (conj (rest acc) [(first (first acc)) n])

                  :else
                  (conj acc [n n]))) [])
      (reverse))))

(defcheck solution-d04f2337
  (fn [x]
    (loop [l   (distinct (sort x))
           acc []]
      (if (empty? l)
        (map (fn [x] (if (= 1 (count x))
                       [(first x) (first x)]
                       [(first x) (last x)])) acc)
        (let [this   (first l)
              llast? (when (not (empty? acc)) (= (inc (last (last acc))) this))]
          (recur (rest l)
            (if llast?
              (vec (conj (vec (take (dec (count acc)) acc)) (conj (last acc) this)))
              (vec (into acc [[this]])))))))))

(defcheck solution-d09b082c
  #(reduce (fn [i n]
             (if (= (last (last i)) (- n 1))
               (conj (vec (butlast i)) [(first (last i)) n])
               (conj i [n n])))
     []
     (distinct (sort %))))

(defcheck solution-d0b2fe69
  (fn intv [s]
    (if (empty? s) []
                   (let [ss  (sort (distinct s))
                         sss (cons (dec (first ss)) (butlast ss))
                         vss (map vector ss sss)
                         eq1 #(= 1 (reduce - %))
                         gbd (fn gb [x r]
                               (if (empty? x) r
                                              (let [f (cons (first x) (take-while eq1 (rest x)))]
                                                (gb (drop-while eq1 (rest x)) (conj r f)))))
                         grp (map (fn [x] (map first x)) (gbd vss []))]
                     (map vector (map first grp) (map last grp))))))

(defcheck solution-d15c5b2b
  (fn f [v]
    (if (not= v []) (loop [ans  []
                           vv   (sort v)
                           pre  (first vv)
                           temp [pre]]
                      (if (empty? vv)
                        (conj ans [(first temp) (last temp)])
                        (if (<= (first vv) (inc pre))
                          (recur ans (next vv) (first vv) (conj temp (first vv))) (recur (conj ans [(first temp) (last temp)]) (next vv) (first vv) [(first vv)])))) [])))

(defcheck solution-d1e88026
  (fn [x]
    (map last
      (partition-by first
        (reduce
          #(if (= (dec %2) (last (last %))) (conj % [(first (last %)) %2]) (conj % [%2 %2]))
          []
          (sort (distinct x)))))))

(defcheck solution-d2969c5
  (fn [c] (->> c sort (reduce (fn [svv v]
                                (if (some (fn [sv] (some (fn [v1] (let [v2 (- v v1)] (or (= 1 v2) (= 0 v2)))) sv)) svv)
                                  (map (fn [sv] (if (some (fn [v1] (let [v2 (- v v1)] (or (= 1 v2) (= 0 v2)))) sv) (vec (conj sv v))
                                                                                                                   sv)) svv)
                                  (conj svv [v])
                                  )) '())
            (map (fn [cv] (if () [(first cv) (last cv)] [(first cv) (first cv)]))) sort
            )))

(defcheck solution-d2a59a90
  (fn [int-coll]
    (if (empty? int-coll)
      []
      (->> int-coll
        (apply max)
        (inc)
        (range)
        (map (set int-coll))
        (partition-by nil?)
        (remove #(every? nil? %))
        (map (juxt first last))))))

(defcheck solution-d34dda06
  (fn __ [ints]
    (letfn [(test [one two] (or (= (inc one) two) (= one two)))
            (reducer [[result start] [current next]]
              (cond
                (not start) (if (test current next)
                              [result current]
                              [(conj result [current current]) nil])
                (test current next) [result start]
                :else [(conj result [start current]) nil]))]
      (->> ints sort (partition-all 2 1) (reduce reducer [[] nil]) first))))

(defcheck solution-d3f57219
  (fn i [s]
    (reduce (fn x
              [r n]
              (if (empty? r)
                (conj r [n n])
                (let [[f l] (last r)]
                  (if (= l (dec n))
                    (conj (vec (butlast r)) [f n])
                    (conj r [n n])))))
      [] ((comp distinct sort) s))))

(defcheck solution-d465fcbc
  (fn intervals [int-seq]
    (if (= [] int-seq)
      []
      (let [sorted-int-seq (sort int-seq) f (first sorted-int-seq)
            comp-res       (reduce (fn [result el]
                                     (let [int-list (first result) last-int (last result) [int-start int-end] last-int]
                                       (if (= 1 (- el int-end))
                                         ; expanding the interval
                                         [int-list [int-start el]]
                                         (if (not= el int-end)
                                           ; starting the new-interval
                                           [(conj int-list last-int) [el el]]
                                           ; no new interval
                                           result
                                           )
                                         )
                                       )
                                     )
                             [[] [f f]]
                             (rest sorted-int-seq))]
        (let [[res-int-list last-el] comp-res]
          (conj res-int-list last-el)
          )
        )
      )
    ))

(defcheck solution-d47a32e1
  (fn [v]
    (let [x (distinct (sort v))
          [a b] (reduce #(let [[x y] %]
                           (if (and (not (empty? x)) (= (inc (last x)) %2))
                             [(conj x %2) y]
                             [[%2] (conj y x)])) [[] []] x)
          x (map #(vector (first %) (last %)) (remove empty? (conj b a)))]
      x)))

(defcheck solution-d4851e3f
  (fn intervals [x]
    (let [sorted-x   (sort x)
          filtered-x (map #(first %) (partition-by identity sorted-x))
          ]
      (if (empty? x)
        []
        (reverse
          (reduce (fn [acc i]
                    (if (= (second (first acc)) (dec i))
                      (conj (rest acc) [(first (first acc)) i])
                      (conj acc [i i])
                      )
                    )
            (list [(first filtered-x) (first filtered-x)])
            (rest filtered-x)
            )
          )
        )
      )
    ))

(defcheck solution-d50de582
  (fn [x] (reduce #(let [[a b] (last %)]
                     (cond
                       (or (nil? a) (> %2 (inc b))) (conj % [%2 %2])
                       (= b %2) %
                       1 (conj (pop %) [a %2]))) [] (sort x))))

(defcheck solution-d5cb03ba
  (fn c-inter
    [l]
    (->> l
      sort
      (reduce (fn [r i]
                (if (or (empty? r)
                        (and (not= i (+ 1 (last (last r))))
                             (not= i (last (last r)))))
                  (conj r [i i])
                  (conj (pop r) [(first (last r)) i])))
        []))))

(defcheck solution-d6385b63
  (fn intervals [coll]
    (letfn [(collapse-if [pred coll]
              (loop [chain [] res [] remaining coll]
                (if-let [s (seq remaining)]
                  (let [x  (first s)
                        xs (next s)]
                    (if (not-empty chain)
                      (if (pred (peek chain) x)
                        (recur (conj chain x) res xs)
                        (recur [x] (conj res chain) xs))
                      (recur [x] res xs)))
                  (if (not-empty chain)
                    (conj res chain)
                    res))))]
      (->> coll
        sort
        (collapse-if #(<= (- %2 %1) 1))
        (map (juxt first last))))))

(defcheck solution-d639e50d
  (fn [v]
    (let [sorted (sort v)]
      (reduce #(if (= 0 (count %1))
                 (vector (vector %2 %2))
                 (let [last-inserted (last (last %1))]
                   (if (or (= last-inserted %2)
                           (= (inc last-inserted) %2))
                     (update-in %1 [(dec (count %1)) 1] (constantly %2))
                     (conj %1 (vector %2 %2)))))
        []
        sorted))))

(defcheck solution-d6dcd40e
  (fn [s]
    (if (seq s)
      (let [s (into (sorted-set) s)]
        (loop [a   (first s)
               b   a
               s   (rest s)
               res []]
          (if (seq s)
            (let [c (first s)]
              (if (zero? (- c b 1))
                (recur a c (rest s) res)
                (recur c c (rest s) (conj res [a b]))))
            (conj res [a b]))))
      [])))

(defcheck solution-d7311f3f
  (fn [nums]
    (if (empty? nums) []
                      (let [normalized-nums (into [] (sort (into #{} nums)))
                            splits          (map #(= %1 (dec %2)) normalized-nums (drop 1 normalized-nums))
                            combined        (conj (into [] (interleave normalized-nums splits))
                                              (last normalized-nums))
                            partitioned     (filter (partial not= '(false))
                                              (partition-by (partial = false) combined))
                            cleaned         (map (partial filter number?) partitioned)]
                        (map #(list (first %) (last %)) cleaned)))))

(defcheck solution-d74c6cc4
  #(let [[y & z] (sort (set %))]
     (if y
       (reverse
         (reduce
           (fn [[[a b & _] & d] c]
             (if (= (inc b) c)
               (cons
                 [a c]
                 d)
               (list*
                 [c c]
                 [a b]
                 d)))
           [[y y]] z))
       [])))

(defcheck solution-d76713ed
  (fn [s]
    (if (empty? s)
      s
      (map #(vector (first %) (last %))
        (let [ss (sort s)]
          (reduce
            #(let [l (last %)]
               (if (> (- %2 (last l)) 1)
                 (conj % [%2])
                 (conj (pop %) (conj l %2))))
            [[(first ss)]] ss))))))

(defcheck solution-d7ef46a6
  (fn ivs [s]
    (let [[a & ss] (distinct (sort s))]
      (cond
        (nil? a) []
        (empty? ss) [[a a]]
        :else (reverse
                (reduce (fn [[[x y] & r :as pairs] z]
                          (if (= z (inc y))
                            (conj r [x z])
                            (conj pairs [z z])))
                  [[a a]]
                  ss)))))

  ; Notes
  ; - maintain a list of intervals where the last one is only a candidate, its end can be updated
  ; - given [x & xs], start with [[x x]]
  ; - reduce against xs with a function that compares each successive number against the candidate pair
  )

(defcheck solution-d80fe048
  (fn [v]
    (if (empty? v) v
                   (letfn [(h [lo hi result v]
                             (cond (empty? v) (concat result [[lo hi]])
                                   (= (first v) hi) (h lo hi result (rest v))
                                   (= (first v) (inc hi)) (h lo (inc hi) result (rest v))
                                   :else (h (first v) (first v) (concat result [[lo hi]]) (rest v))))]
                     (let [v (sort v)]
                       (h (first v) (first v) [] (rest v)))))))

(defcheck solution-d8f0a9fb
  (fn ranga [ls] (let [reor (distinct (sort ls))] (loop [cap [(first reor)] n (first reor) ls reor acc []] (if (empty? ls) acc (if (= (inc n) (second ls)) (recur cap (inc n) (rest ls) acc) (recur [(second ls)] (second ls) (rest ls) (conj acc (conj cap (first ls))))))))))

(defcheck solution-d91a3236
  (fn [xs]
    (reduce
      (fn [a x]
        (if (empty? a) [[x x]]
                       (if (let [y (second (last a))] (or (= (inc y) x) (= x y)))
                         (assoc-in a [(dec (count a)) 1] x)
                         (conj a [x x]))))
      [] (sort xs))))

(defcheck solution-d9490378
  (fn interval [v]
    (reduce (fn [res item]
              (let [[fst lst] (last res)]
                (if (or (empty? res) (> item (inc lst)))
                  (concat res [[item item]])
                  (concat (drop-last res) [[fst item]]))
                )) [] (vec (sort v)))))

(defcheck solution-d957acda
  (fn [coll]
    (let [sorted (sort (distinct coll))
          min    (first sorted)
          init   (if (nil? min)
                   []
                   [[min min]])
          run    (fn [mem x]
                   (if (= (dec x) (last (last mem)))
                     (conj (vec (butlast mem)) [(first (last mem)) x])
                     (conj mem [x x])))]
      (reduce run init (rest sorted)))))

(defcheck solution-d9d1c50
  (fn [s]
    (if (empty? s)
      s
      (loop [ans '() start (first (sort s)) current (first (sort s)) tmps (rest (sort s))]
        (if (empty? tmps)
          (reverse (conj ans (conj (conj '() current) start)))
          (if (or (= current (first tmps)) (= (inc current) (first tmps)))
            (recur ans start (first tmps) (rest tmps))
            (recur (conj ans (conj (conj '() current) start)) (first tmps) (first tmps) (rest tmps))))))))

(defcheck solution-d9d4be3d
  (fn interval
    ([xs] (interval (sort (distinct xs)) nil))
    ([xs run]
     (if (seq xs)
       (let [x         (first xs)
             start-run #(interval (rest xs) (take 2 (repeat x)))]
         (if (nil? run)
           (start-run)
           (if (= 1 (- x (second run)))
             (interval (rest xs) [(first run) x])
             (cons run (start-run)))))
       (if (nil? run) [] [run])))))

(defcheck solution-da00e9dd
  (fn [coll]
    (letfn [(split-interval [coll]
              (let [l (first coll)]
                (loop [r    l
                       more (rest coll)]
                  (if (or (empty? more)
                          (> (first more) (inc r)))
                    [[l r] more]
                    (recur (inc r) (rest more))))))]
      (loop [acc  []
             coll (distinct (sort coll))]
        (if (empty? coll)
          acc
          (let [[inter more] (split-interval coll)]
            (recur (conj acc inter) more)))))))

(defcheck solution-dac7faa2
  (fn [ns]
    (reverse
      (reduce
        (fn [[[a b] & rest :as pairs] n]
          (cond
            (empty? pairs) [[n n]]
            (= n b) pairs
            (= n (inc b)) (cons [a n] rest)
            :else (cons [n n] pairs)))
        '()
        (sort ns)))))

(defcheck solution-db1d414c
  (fn intervals [coll]
    (if (seq coll)
      (let [low      (apply min coll)
            s        (set coll)
            interval (take-while s (range low (inc (apply max coll))))]
        (cons [(first interval) (last interval)]
          (lazy-seq (intervals (clojure.set/difference s interval)))))
      ())))

(defcheck solution-db44d605
  (fn [xs]
    (if (empty? xs)
      xs
      (map
        (fn [ry]
          [(first ry) (last ry)])
        (reverse
          (reduce
            (fn [rs x]
              (let [r  (first rs)
                    x0 (last r)]
                (if (< (- x x0) 2)
                  (do
                    (cons (conj r x) (rest rs)))
                  (cons [x] rs))))
            [[(first (sort xs))]]
            (rest (sort xs))))))))

(defcheck solution-dbc7b33e
  (fn interval [c]
    (reduce (fn ih [acc c]
              (if (seq acc)
                (let [[lo hi] (peek acc)]
                  (cond (= c (inc hi))
                        (conj (pop acc) [lo c])
                        (= c hi) acc
                        :else (conj acc [c c])))
                (conj acc [c c])))
      [] (sort c))))

(defcheck solution-dbcad0e0
  (fn [alist]
    (let [sortlist (distinct (sort alist))
          gaplist  (partition 2 (drop 1 (butlast (interleave sortlist sortlist))))
          biggaps  (remove #(= 1 (- (second %) (first %))) gaplist)]
      (if (empty? gaplist)
        (if (empty? sortlist)
          []
          [[(first sortlist) (last sortlist)]])
        (partition 2 (flatten (concat [(ffirst gaplist)] biggaps [(second (last gaplist))])))))))

(defcheck solution-dc8c6163
  #(reverse
     (reduce (fn [res i]
               (let
                [[a b] (first res) fixed (rest res)]
                 (if (and b (< i (+ b 2)))
                   (conj fixed [a i])
                   (conj res [i i]))))
       '() (sort %))))

(defcheck solution-dd79d6ba
  (fn [s]
    (loop [coll (sort s) r [] cur [(first coll) (first coll)]]
      (cond
        (empty? s) []
        (empty? coll) (conj r cur)
        (or (<= (first coll) (inc (second cur)))) (recur (rest coll) r [(first cur) (first coll)])
        :else (recur (rest coll) (conj r cur) [(first coll) (first coll)])
        )
      )
    ))

(defcheck solution-dd93bfbb
  (fn [x] (->> x
            (apply sorted-set)
            (keep-indexed (fn [i item] [i item (- item i)]))
            (partition-by #(% 2))
            (map #(vector (second (first %)) (second (last %)))))))

(defcheck solution-ddb2eeb3
  (fn [coll-orig]
    (if (empty? coll-orig) []
                           (let [coll (sort coll-orig)]
                             (reduce
                               (fn [inters x]
                                 (let [[a b] (last inters)]
                                   (if (> x (inc b))
                                     (concat inters [[x x]])
                                     (concat (butlast inters) [[a x]]))))
                               [[(first coll) (first coll)]]
                               coll)))))

(defcheck solution-ddbea19f
  (fn [xs]
    (let [sorted  (sort (set xs))                           ;; sort and remove duplicates
          pairs   (map list sorted (range))                 ;; pair values with a count
          grouped (partition-by #(apply - %) pairs)]        ;; split where the diff in the pair changes
      (map #(vector (ffirst %) (first (last %))) grouped))) ;; pull out the grouped values
  )

(defcheck solution-de26d269
  (fn [xs] (->> xs
             sort
             (partition-all 2 1)
             (#(cons [(ffirst %)] %))
             (remove (fn [[a b]] (and a b (< (- b a) 2))))
             flatten
             (partition 2))))

(defcheck solution-de8cadd0
  (fn [nums]
    (if (empty? nums) []
                      (let [nums (sort nums)
                            head (first nums)
                            tail (rest nums)]
                        (loop [nums  tail
                               cval  [head head]
                               ivals []]
                          (if (empty? nums) (conj ivals cval)
                                            (let [head (first nums)
                                                  tail (rest nums)
                                                  prev (second cval)
                                                  [cval ivals] (if (or (= head (inc prev)) (= head prev))
                                                                 [[(first cval) head] ivals]
                                                                 [[head head] (conj ivals cval)])]
                                              (recur tail cval ivals))))))))

(defcheck solution-de8f77b4
  (fn [vs]
    (->> vs
      (into #{})
      sort
      (reduce (fn [acc v]
                (if-not (seq acc)
                  (conj acc [v])
                  (let [u (peek (peek acc))]
                    (if (= 1 (- v u))
                      (update-in acc
                        [(dec (count acc))]
                        conj v)
                      (conj acc [v])))))
        [])
      (map (juxt first last)))))

(defcheck solution-debadee6
  (fn [coll]
    (if (empty? coll)
      []
      (loop [acc  []
             cur  []
             prev nil
             coll (sort coll)]
        (if (empty? coll)
          (map #(vector (first %) (last %)) (conj acc cur))
          (let [fst (first coll)]
            (if (nil? prev)
              (recur acc (conj cur fst) fst (rest coll))
              (if (> (- fst prev) 1)
                (recur (conj acc cur) (conj [] fst) fst (rest coll))
                (recur acc (conj cur fst) fst (rest coll))))))))))

(defcheck solution-deec2096
  (fn [xs]
    (->> xs
      distinct
      sort
      (reduce (fn [ret e]
                (if-let [curr (ffirst ret)]
                  (if (= (inc curr) e)
                    (conj (rest ret) (conj (first ret) e))
                    (conj ret (list e)))
                  (conj ret (list e))))
        '())
      reverse
      (map (juxt (partial apply min) (partial apply max))))))

(defcheck solution-dfe7b0dc
  (fn [v] (->> v
            distinct
            sort
            (map vector (range))
            (partition-by #(apply - %))
            (map flatten)
            (map (juxt second last)))))

(defcheck solution-e0244690
  (fn intv [col]
    (let [scol (sort col)]
      (reduce (fn [ret n]
                (if (= (last (last ret)) (dec n))
                  (conj (vec (butlast ret)) (vector (first (last ret)) n))
                  (if (or (nil? (last (last ret)))
                          (> n (last (last ret))))
                    (conj ret (vector n n))
                    ret)))
        [] scol))))

(defcheck solution-e02cc57
  (fn [coll] (let [sorted (sort (set coll))
                   r      (fn [res a] (let [l (last (last res))
                                            f (first (last res))]
                                        (if (= l (dec a))
                                          (concat (drop-last res) [[f a]])
                                          (concat res [[a a]]))))]
               (if (empty? coll)
                 []
                 (reduce r [[(first sorted) (first sorted)]] (rest sorted)))
               )))

(defcheck solution-e04355c3
  (fn [c]
    (let [sorted (apply sorted-set c)]
      (if (seq sorted)
        (loop [ret    []
               minval (first sorted)
               maxval minval
               sorted (rest sorted)]
          (if-let [x (first sorted)]
            (if (= x (inc maxval))
              (recur ret minval x (rest sorted))
              (recur (conj ret [minval maxval]) x x (rest sorted)))
            (conj ret [minval maxval])))
        []))))

(defcheck solution-e092f25b
  (fn [col]
    (if (empty? col)
      []
      (loop [c (sort col) mb (first c) me (last c) r [[mb mb]]]
        (let [[b e] (last r)]
          (if (>= e me)
            r
            (let [n (+ 1 e)
                  r (if (some (partial = n) c)
                      (conj (vec (drop-last r)) [b n])
                      (conj r [(some #(if (> % n) %) c) (some #(if (> % n) %) c)]))]
              (recur c mb me r))))))))

(defcheck solution-e0a0e4af
  (fn [coll]
    (if (empty? coll)
      []
      (let [s (sort coll)]
        (partition 2 (flatten (concat [(first s)]
                                      (filter #(> (- (last %) (first %)) 1)
                                        (map #(vector %1 %2) (butlast s) (rest s)))
                                      [(last s)])))))))

(defcheck solution-e10b02e8
  (fn interval [x]
    (let [sorted (sort x)]
      (reduce
        (fn [acc item]
          (if (empty? acc)
            [[item item]]
            (let [last-index (dec (count acc)) last-value ((acc last-index) 1)]
              (if (<= (- item last-value) 1)
                (update-in acc [last-index 1] (fn [x] item))
                (conj acc [item item])
                )
              )
            )
          )
        []
        sorted
        )
      )
    ))

(defcheck solution-e10c641b
  (fn [xs]
    (if (empty? xs)
      []
      (let [lo (apply min xs)
            hi (apply max xs)
            m  (vec (repeat (+ 2 hi) false))
            a  (reduce #(assoc %1 %2 true) m xs)]
        (loop [i (inc lo), beg lo, out []]
          (cond
            (= 2 (- i hi)) out
            (not= (a i) (a beg))
            (if (a i)
              (recur (inc i) i out)
              (recur (inc i) i (conj out [beg (dec i)])))
            :else (recur (inc i) beg out)))))))

(defcheck solution-e12876de
  (fn intervals [coll]
    (->> coll
      sort
      (reduce (fn [acc n]
                (let [[a b] (peek acc)]
                  (assert (or (empty? acc) (>= n b))
                    "Input should have been sorted.")
                  (cond
                    (empty? acc) [[n n]]
                    (= n b) acc
                    (= n (inc b)) (conj (pop acc) [a n])
                    (> n (inc b)) (conj acc [n n]))))
        []))))

(defcheck solution-e19df3d0
  (fn [k] (if (empty? k) []
                         (let [z (distinct (sort k))]
                           (loop [acc [[(first z) (first z)]] l (rest z)] (if (empty? l) acc
                                                                                         (let [last-seen (last (last acc)) first-seen (first (last acc)) next-element (first l)]
                                                                                           (if (= (inc last-seen) next-element)
                                                                                             (if (and (= last-seen first-seen) (= 2 (count (last acc))))
                                                                                               (recur (conj (into [] (drop-last acc)) (conj [last-seen] next-element)) (rest l))
                                                                                               (if (= 1 (count (last acc)))
                                                                                                 (recur (conj (into [] (drop-last acc)) (conj (last acc) (first l))) (rest l))
                                                                                                 (recur (conj (into [] (drop-last acc)) (conj [first-seen] (first l))) (rest l))))
                                                                                             (recur (conj acc [(first l) (first l)]) (rest l))))))))))

(defcheck solution-e1bc1868
  (fn intervals [xs]
    (if (not (seq xs))
      []
      (let [x    (distinct (sort xs))
            fst  (first x)
            [rng rst] (split-with (fn [[a b]] (= a b))
                        (map vector x (iterate inc fst)))
            rst  (map first rst)
            from (first (first rng))
            to   (first (last rng))]
        (cons [from to] (intervals rst))))))

(defcheck solution-e1bfe023
  #((fn f [i [a b & r]]
      (if a
        (if (and b (< (- b a i) 2))
          (recur (- b a) (cons a r))
          (cons [a (+ a i)] (f 0 (cons b r))))
        ()))
    0
    (sort %)))

(defcheck solution-e1e850ca
  (fn [xs]
    (if (empty? xs) xs (map (fn [x] [(first x) (last x)])
                         (distinct
                           (partition-by
                             (let [n (atom (dec (first xs)))
                                   g (atom 0)]
                               (fn [x]
                                 (let [prev @n]
                                   (if (zero? (- (reset! n x) prev 1))
                                     @g (swap! g inc)))))
                             (sort (distinct xs))))))))

(defcheck solution-e211e138
  (fn [s] (->> s
            distinct
            sort
            (map-indexed #(vector %2 (- % %2)))
            (group-by second)
            vals
            (map (juxt ffirst (comp first last))))))

(defcheck solution-e3327ed4
  #(->> % sort
     (reduce
       (fn [[i & is] x]
         (if (nil? i)
           [[x x]]
           (let [[s e] i]
             (if (<= (Math/abs (- e x)) 1)
               (cons [s x] is)
               (apply list [x x] i is)))))
       [])
     reverse))

(defcheck solution-e3f4d524
  (fn interv [coll]
    (map #(vector (apply min %) (apply max %)) (reduce (fn [acc el]
                                                         (cond (= (dec el) (peek (peek acc)))
                                                               (conj (pop acc) (conj (peek acc) el))
                                                               (= el (peek (peek acc))) acc
                                                               :else (conj acc [el])))
                                                 []
                                                 (sort coll)))))

(defcheck solution-e40c7ba0
  (fn [input]
    (let [update
                 (fn [[i c] num]
                   (if (nil? c) [i [num num]]
                                (let [[f l] c]
                                  (if (= num (inc l))
                                    [i [f num]]
                                    [(conj i c) [num num]]))))
          sorted (sort (vec (set input)))
          [s c] (reduce update [[] nil] sorted)]
      (if c (conj s c) s))))

(defcheck solution-e42e0629
  (fn interval [x]
    (reverse
      (reduce
        (fn [[[a b] & rs :as s] n]
          (if (or (nil? b) (> n (inc b)))
            (cons [n n] s)
            (cons [a n] rs)))
        []
        (sort x)))))

(defcheck solution-e46254b3
  (fn [l]
    (reduce
      (fn [c n]
        (let [[x y] (last c)]
          (if (and x (<= n (inc y)))
            (conj (pop c) [x n])
            (conj c [n n]))
          ))
      []
      (-> l set sort))))

(defcheck solution-e5ee1908
  (fn intervals [nums]
    (if (empty? nums)
      nums
      (let [sorted (distinct (sort nums))]
        (loop [res              []
               todo             (rest sorted)
               current-interval [(first sorted) (first sorted)]]
          (if (empty? todo)
            (conj res current-interval)
            (if (= (inc (second current-interval)) (first todo))
              (recur
                res
                (rest todo)
                [(first current-interval) (first todo)])
              (recur
                (conj res current-interval)
                (rest todo)
                [(first todo) (first todo)]))))))))

(defcheck solution-e6daf1f3
  (fn [xs]
    (letfn [(next-subseq [xs]
              (loop [xs xs s []]
                (if (empty? xs)
                  s
                  (let [x (first xs)]
                    (if (or (empty? s) (= x (inc (last s))))
                      (recur (rest xs) (conj s x))
                      s)))))]
      (loop [xs (sort (set xs)) accum []]
        (if (empty? xs)
          accum
          (let [s (next-subseq xs)]
            (recur (drop (count s) xs) (conj accum [(first s) (last s)]))))))))

(defcheck solution-e74d2c3f
  (letfn
   [(conseq? [n m]
      (if (nil? m)
        false
        (= (inc n) m)))

    (dedup [coll]
      (reduce #(if (= (last %1) %2) %1 (conj %1 %2)) [(first coll)] coll))

    (get-run [f coll]
      (if (> (count coll) 1)
        (lazy-seq
          (when-let [s (seq coll)]
            (let [fst (first s)
                  fv  (first (next s))]
              (if (f fst fv)
                (concat [fst] (get-run f (drop 1 s)))
                [fst]))))
        coll))

    (partition-by-2 [f coll]
      (lazy-seq
        (when-let [s (seq coll)]
          (let [run (get-run f s)]
            (cons run (partition-by-2 f (seq (drop (count run) s))))))))]

    (fn p [coll]
      (if (> (count coll) 1)
        (map #(vector (first %) (last %)) (partition-by-2 conseq? (dedup (sort coll))))
        coll))))

(defcheck solution-e7b38d8d
  (fn [s]
    (if (seq s)
      (->> s sort distinct
        (reduce (fn [[[a b] & abs :as acc]
                     x]
                  (if (<= x (inc b))
                    (conj abs [a x])
                    (conj acc [x x])
                    )
                  ) (list [(apply min s) (apply min s)]))
        reverse)
      [])
    ))

(defcheck solution-e8081d55
  (fn [ns]
    (if (seq ns)
      (let [[h & t] (sort ns)]
        (reverse
          (reduce
            (fn [[[lo hi] & tail :as all] i]
              (if (> i (inc hi)) (cons [i i] all) (cons [lo i] tail)))
            [[h h]]
            t)))
      [])))

(defcheck solution-e86de6bc
  (fn intervals [coll]
    (letfn [(f [mp e]
              "Takes map and element e. Puts e into map -- either in
               a current interval or creates a new one retiring a current one to
               a list of intervals"
              (let [{:keys [low high current inters]} mp]
                (if (and (>= e low)
                         (or (nil? high) (<= e (inc high))))
                  (assoc mp :high e :current (conj current e))
                  (assoc mp :low e :high (inc e) :current [e] :inters (conj inters current)))))]
      (let [m          {:low     0
                        :high    nil
                        :current []
                        :inters  []}
            rs         (reduce f m (sort coll))
            {:keys [current inters]} rs
            all-inters (conj inters current)]
        (if (zero? (count coll))
          []
          (map #(apply (juxt min max) %) all-inters))))))

(defcheck solution-e87b2a78
  (fn [sq]
    (if (empty? sq) []
                    (let [s (sort sq)]
                      (partition 2 (loop [f (first s) r (rest s) built (take 1 s)]
                                     (if (empty? r)
                                       (concat built (take-last 1 s))
                                       (recur (first r) (rest r)
                                         (if (>= 1 (- (first r) f))
                                           built
                                           (concat built [f] (take 1 r)))))))))))

(defcheck solution-e8a096d0
  (fn [sequencia]
    (if (empty? sequencia)
      []
      (let [sequencia-preparada (-> sequencia distinct sort)]
        (->> (reduce (fn [[[termino-intervalo inicio-intervalo :as intervalo-pivot] & outros-intervalos :as todos-intervalos] elemento]
                       (if (= (inc termino-intervalo) elemento)
                         (conj outros-intervalos (list elemento (if (nil? inicio-intervalo) termino-intervalo inicio-intervalo)))
                         (into outros-intervalos [(list termino-intervalo (if (nil? inicio-intervalo) termino-intervalo inicio-intervalo)) (list elemento)])))
               (list (list (first sequencia-preparada)))
               (next sequencia-preparada))
          ((fn [[[primeiro-elemento :as primeiro-intervalo] & outros-intervalos :as intervalos]]
             (if (= 1 (count primeiro-intervalo))
               (conj outros-intervalos (list primeiro-elemento primeiro-elemento))
               intervalos)))
          (map (fn [[p s]] (vector s p)))
          reverse)))))

(defcheck solution-e9902880
  (fn [s]
    (let [deal (sort (set s))]
      (loop [p (rest deal) tmp [(first deal)] r []]
        (cond
          (empty? s) []
          (empty? p) (conj r [(first tmp) (last tmp)])
          (= (first p) (inc (last tmp))) (recur (rest p) (conj tmp (first p)) r)
          :else (recur (rest p) [(first p)] (conj r [(first tmp) (last tmp)]))
          )))))

(defcheck solution-e9a94eb6
  (fn intervals' [xs]
    (->> xs
      sort
      distinct
      (reduce (fn [acc item]
                (if (or (empty? acc)
                        (> (Math/abs (- (first (first acc)) item)) 1))
                  (cons [item] acc)
                  (cons (cons item (first acc)) (rest acc)))) [])
      (map (fn [grp] [(apply min grp) (apply max grp)]))
      reverse)))

(defcheck solution-e9e545f4
  (fn [coll]
    (let [sor (->> coll distinct sort)
          pa  (partition 2 1 [nil] sor)
          pa2 (filter (fn [[a b]] (not= (+ a 1) b)) pa)
          nn  (cons (first sor) (drop-last (flatten pa2)))
          tt  (partition 2 nn)]
      tt)))

(defcheck solution-ea0e6378
  (fn intervals [s]
    (if (empty? s) s
                   (let [s (sort s)
                         h (first s)
                         r (rest s)]
                     (reverse (reduce (fn [acc n]
                                        (let [[lower upper] (first acc)]
                                          (cond
                                            (= n upper) acc
                                            (= n (inc upper)) (cons [lower n] (rest acc))
                                            :else (cons [n n] acc))))
                                (list [h h])
                                r))))))

(defcheck solution-ea98145e
  ; The following solution is probably not the most efficient one...
  (fn [xs]
    (reverse
      (reduce
        #(if %1
           (let [[a b] (first %1)]
             (cond
               (= b %2) %1
               (= (inc b) %2) (conj (rest %1) [a %2])
               :else (conj %1 [%2 %2])))
           `(~[%2 %2]))
        nil
        (sort xs)))))

(defcheck solution-eaa2f9f7
  (fn [x]
    (loop [src (sort x) temp [] result []]
      (cond (empty? src) (if (empty? temp) result (conj result [(first temp) (last temp)]))
            (empty? temp) (recur (rest src) (conj temp (first src)) result)
            (< (inc (last temp)) (first src)) (recur (rest src) [(first src)] (conj result [(first temp) (last temp)]))
            :else (recur (rest src) (conj temp (first src)) result)))))

(defcheck solution-eac11151
  (fn fc
    ([l]
     (if (empty? l)
       []
       (let [l (apply sorted-set l)]
         (fc (rest l) (first l) (first l) []))))
    ([[f & l] s p r]
     (cond
       (nil? f) (conj r [s p])
       (= (inc p) f) (recur l s (inc p) r)
       :else (recur l f f (conj r [s p]))))))

(defcheck solution-eb2baa09
  (fn interv ([c d] (let [a (vec c) b (vec d)]
                      (if (empty? b)
                        a
                        (if (< (- (first b) (last (last a))) 2)
                          (interv (concat (butlast a) [(conj (last a) (first b))]) (next b))
                          (interv (concat a [[(first b)]]) (next b))))))
    ([x] (let [sx (sort x)] (if (empty? x) [] (map #(vector (first %) (last %)) (interv [[(first sx)]] (next sx))))))))

(defcheck solution-eb39df8f
  (fn intervals ([coll] (map #(vector (first %) (last %)) (intervals [] [] (sort coll))))
    ([res cur coll] (if (empty? coll)
                      (if (empty? cur)
                        res
                        (conj res cur))
                      (if (empty? cur)
                        (intervals res (conj cur (first coll)) (rest coll))
                        (if (or (= (inc (last cur)) (first coll)) (= (last cur) (first coll)))
                          (intervals res (conj cur (first coll)) (rest coll))
                          (intervals (conj res cur) [] coll)))))))

(defcheck solution-eb79f15b
  (fn [coll]
    (->> (map list (sort (set coll)) (range))
      (partition-by (partial apply -))
      (map (juxt ffirst (comp first last))))))

(defcheck solution-eba97c37
  (fn [coll]
    (if (empty? coll)
      []
      (keep-indexed (fn [i x] (if (even? i) [(first x) (last x)] nil))
        (partition-by (comp nil? (set coll)) (range (apply min coll) (inc (apply max coll))))))))

(defcheck solution-ec5aba92
  #(reverse (reduce
              (fn [r x]
                (if (or (empty? r) (not= (inc (last (first r))) x))
                  (conj r [x x])
                  (conj (rest r) [(first (first r)) x])))
              [] (sort (set %)))))

(defcheck solution-ecde90b1
  (fn [in-seq]
    (loop [xs            (into (sorted-set) in-seq)
           current-start nil
           current       nil
           result        []]
      (if (empty? xs)
        (if current-start
          (conj result [current-start current])
          result)
        (if current
          (if (= (inc current) (first xs))
            (recur (rest xs) current-start (inc current) result)
            (recur (rest xs) (first xs) (first xs) (conj result [current-start current])))
          (recur (rest xs) (first xs) (first xs) result))))))

(defcheck solution-ece74834
  (fn [c]
    (->>
      (map list (sort (set c)) (range))
      (partition-by #(apply - %))
      (map #(list (ffirst %) (first (last %)))))))

(defcheck solution-ee8a9358
  (fn [s]
    (->> s
      distinct
      sort
      reverse
      (#(interleave % (range)))
      (partition 2)
      (map #(list (first %) (+ (first %) (second %))))
      (partition-by #(second %))
      (map (fn [i-list]
             (map #(first %) i-list))
        )
      (map #(list (last %) (first %)))
      reverse
      )))

(defcheck solution-eeb86a5c
  (fn [n]
    (->> n
      sort
      (reduce #(let [[ls le] (last %)]
                 (if (and (not (nil? le)) (<= le %2 (inc le)))
                   (conj (vec (butlast %)) (vector ls %2))
                   (conj % (vector %2 %2)))) []))))

(defcheck solution-eef3aafe
  (fn [v] (loop [r [] c (distinct (sort v))] (if (empty? c) r (let [i (first c)] (recur (if (empty? r) [[i i]] (if (= i (inc (second (last r)))) (conj (pop r) [(first (last r)) i]) (conj r [i i]))) (next c)))))))

(defcheck solution-ef4493f1
  (fn [xs]
    (if (empty? xs)
      []
      (let [xs     (-> xs sort distinct)
            [x & xs] xs
            ranges (reduce
                     (fn [[acc [start end]] n]
                       (if (= end (dec n))
                         [acc [start n]]
                         [(conj acc [start end]) [n n]]))
                     [[] [x x]]
                     xs)]
        (conj (first ranges) (second ranges))))))

(defcheck solution-ef5051f1
  (fn [l]
    (if (empty? l)
      []
      (let [[f & r] (sort (set l))]
        (first (reduce (fn [[i a b] x] (if (= (- x b) 1) [i a x] [(conj i [a b]) x x])) [[] f f] (conj (vec r) f)))))))

(defcheck solution-ef53bcca
  (fn [coll]
    (reduce
      (fn [result val]
        (if (empty? result)
          [[val val]]
          (let [interval (last result)
                seen     (last interval)]
            (if (= val (inc seen))
              (conj (vec (butlast result))
                [(first interval) val])
              (conj result [val val])))))
      []
      (sort (set coll)))))

(defcheck solution-ef92ac6c
  (fn [coll]
    (if (empty? coll)
      []
      (let [c (set coll)]
        (map #(vector (first %) (last %))
          (take-nth 2 (partition-by #(contains? c %)
                        (range (apply min coll) (inc (apply max coll))))))))))

(defcheck solution-efb09a8c
  (fn [xs]
    (if (empty? xs) []
                    (let [ss     (sort xs)
                          s-fst  (first ss)
                          s-rest (rest ss)]
                      (reduce (fn [acc b]
                                (let [top-iv (peek acc)
                                      prev   (second top-iv)]
                                  (cond (= prev b) acc
                                        (= (inc prev) b)
                                        (conj (pop acc)
                                          [(first top-iv) b])
                                        :else (conj acc [b b]))))
                        [[s-fst s-fst]]
                        s-rest)))))

(defcheck solution-eff49b4c
  (fn iv [y]
    (if (empty? y)
      '()
      ((fn [x]
         (let [
               [a & rest] x
               b (+
                  a
                  (count
                    (take-while
                      true?
                      (map
                        #(= (nth rest %1) (+ (inc a) %1))
                        (range (count rest))
                        )
                      )
                    )
                  )
               ]
           (cons [a b] (iv (drop (inc (- b a)) x)))
           )
         )
       (-> y sort distinct)
       )
      )
    ))

(defcheck solution-f0694ebc
  (fn f [coll]
    (letfn [(g [[x & [y & r]]]
              (if y
                (cons [x y] (g (cons y r)))))]
      (let [sorted (sort coll)]
        (if (empty? sorted) []
                            (partition 2
                              (concat
                               [(first sorted)]
                               (apply concat
                                 (filter
                                   (fn [[x y]]
                                     (< (inc x) y))
                                   (g sorted))
                                 )
                               [(last sorted)])
                              ))))))

(defcheck solution-f1496bb8
  (fn f [xs]
    (let [xs      (distinct (sort xs))
          last-el (last xs)
          xs      (conj (into [] xs) last-el)
          pairs   (partition 2 1 xs)
          pairs   (map (fn [[a b]] [a (= (inc a) b)]) pairs)]
      (loop [ax [] bx [] [[x p] & xs :as xxs] pairs]
        (if (empty? xxs)
          (map #(-> [(first %) (last %)]) ax)
          (let [bx     (conj bx x)
                new-bx (if p bx [])
                new-ax (if p ax (conj ax bx))]
            (recur new-ax new-bx xs)))))))

(defcheck solution-f26eb820
  (fn intervals [s]
    (if (empty? (seq s))
      []
      (let [sorted (sort s)]
        (loop [key 1 m {1 [(first sorted)]} rst (rest sorted)]
          (cond
            (not (seq rst)) (map #(vector (first %) (last %)) (sort-by first (vals m)))
            (= (last (m key)) (first rst)) (recur key m (rest rst))
            (= (inc (last (m key))) (first rst)) (recur key (assoc m key (conj (m key) (first rst))) (rest rst))
            :else (recur (inc key) (assoc m (inc key) [(first rst)]) (rest rst))))))))

(defcheck solution-f37e757f
  (fn [coll]
    (let [groups (reduce #(if (= (last (last %)) (- %2 1))
                            (assoc-in % [(- (count %) 1) (count (last %))] %2)
                            (assoc-in % [(count %)] [%2]))
                   [] (distinct (sort coll)))]
      (for [s groups]
        [(first s) (last s)])
      )))

(defcheck solution-f39ec884
  (fn [s]
    (map #(vector (first %) (last %))
      (loop [acc [] [x & xs] (sort s) c []]
        (if x
          (if (or (= (dec x) (last c)) (= x (last c)) (empty? c))
            (recur acc xs (conj c x))
            (recur (conj acc c) xs [x])
            )
          (if-not (empty? c)
            (conj acc c)
            acc
            )
          )
        )
      )
    ))

(defcheck solution-f3ba4072
  (fn [xs]
    (map #(vector (first %) (last %))
      (reduce
        (fn [acc [an pred]]
          (if pred
            (conj (pop acc) (conj (peek acc) an))
            (conj acc [an])
            )
          )
        []
        (reduce
          (fn [acc x]
            (conj acc [x (= (first (last acc)) (dec x))]))
          []
          (distinct (sort xs)))))
    ))

(defcheck solution-f3fac10
  (fn [vs]
    (if (empty? vs)
      []
      (let [svs (sort vs) start (first svs)]
        (reduce
          (fn [out nxt]
            (let [[x1 x2] (last out)]
              (cond
                (= x2 nxt) out
                (= (inc x2) nxt) (assoc out (dec (count out)) [x1 nxt])
                :else (conj out [nxt nxt]))))
          [[start start]]
          (rest svs))))))

(defcheck solution-f3fcffe9
  (fn f [xs]
    (->> (sort xs)
      (distinct)
      (map #(vector % %2) (range))
      (partition-by #(- (last %) (first %)))
      (map #(map last %))
      (map #(vector (first %) (last %))))))

(defcheck solution-f4575c09
  (fn [coll]
    (if-not (seq coll)
      []
      (let [xs        (distinct (sort coll))
            first-num (first xs)]
        (remove nil?
          (map (fn [d]
                 (cond
                   (= "|" (first d)) nil
                   (= 1 (count d)) [(first d) (first d)]
                   :else [(first d) (last d)])
                 )
            (partition-by #(= "|" %)
              (:nums
               (reduce
                 (fn [{nums :nums
                       last :last} b]
                   (if (= (inc last) b)
                     {:nums (conj nums b) :last b}
                     {:nums (conj nums "|" b) :last b}))
                 {:nums [first-num] :last first-num}
                 (rest xs))))))))
    ))

(defcheck solution-f51c5090
  #(let [s (sort %)
         g (loop [l (rest s) r [(vector (first s))]]
             (let [n (first l)
                   p (last r)]
               (if (empty? l) r
                              (recur (rest l)
                                (if (<= (- n (last p)) 1)
                                  (conj (vec (drop-last r)) (conj p n))
                                  (conj r [n]))))))]
     (if (= % []) []
                  (for [i g]
                    [(apply min i) (apply max i)]))))

(defcheck solution-f69e2ebc
  #(reduce (fn [r x] (if-let [e (last r)]
                       (let [[a b] e]
                         (cond (<= x b) r
                               (= (inc b) x) (assoc r (dec (count r)) [a x])
                               :e (conj r [x x])))
                       [[x x]]))
     [] (sort %)))

(defcheck solution-f6d4531f
  (let [consecutives (fn [s]
                       (let [sorted-s (sort s)]
                         (reduce (fn [acc valu]
                                   (if (or (empty? acc)
                                           (> (dec valu) (last (last acc))))
                                     (conj acc [valu])
                                     (conj (pop acc) (conj (peek acc) valu))))
                           []
                           sorted-s)))]
    (fn [s]
      (map (fn [part]
             [(first part) (last part)])
        (consecutives s)))))

(defcheck solution-f70b4d18
  (fn [coll]
    (if (seq coll)
      (let [minval  (apply min coll)
            maxval  (apply max coll)
            collset (into #{} coll)
            in-coll (fn [e] (contains? collset e))]
        (->> (range minval (inc maxval))
          (partition-by #(in-coll %))
          (map (fn [elem] (filter #(in-coll %) elem)))
          (filter (complement empty?))
          (map (fn [c] (cond
                         (= 1 (count c)) [(first c) (first c)]
                         (< 2 (count c)) [(first c) (last c)]
                         :else c)))))
      [])))

(defcheck solution-f72cf4fb
  (fn [x]
    (->> (sort x) distinct (interleave (range)) (partition 2)
      (partition-by #(apply - %)) (map #(map second %))
      (map (juxt first last)))))

(defcheck solution-f73de761
  (fn [coll]
    (letfn [(step [coll']
              (when-let [[fst snd] (seq coll')]
                (when (and snd
                           (or (= fst snd) (= fst (dec snd))))
                  (cons snd (step (rest coll'))))))
            (succeeding [coll'']
              (cons (first coll'') (step coll'')))
            (intervals [coll''']
              (loop [s   coll'''
                     acc []]
                (if (seq s)
                  (let [inter (succeeding s)]
                    (recur (drop (count inter) s) (conj acc inter)))
                  (map (juxt first last) acc))))]
      (let [scoll (sort coll)]
        (intervals scoll)))))

(defcheck solution-f75c8082
  (fn [v]
    (let [[f & t] (sort v)]
      (if f
        (reverse
          (reduce
            (fn [[[a b] & r :as l] e]
              (if (<= a e (+ 1 b))
                (conj r [a e])
                (conj l [e e])))
            [[f f]]
            t))
        v))))

(defcheck solution-f79b477a
  (fn intervals [xs]
    (if (empty? xs)
      []
      (let [sorted (sort (set xs))]
        (map (fn [xs] [(first xs) (last xs)]) (reduce #(if (= (inc (last (last %))) %2)
                                                         (update-in % [(dec (count %))] (fn [z] (conj z %2)))
                                                         (conj % [%2])) [[(first sorted)]] (rest sorted)))))))

(defcheck solution-f7c14bdf
  (fn [xs]
    (let [sset  (apply sorted-set xs)
          parts (reduce (fn [parts x]
                          (if-let [last-num (last (last parts))]
                            (if (or (= (inc last-num) x) (= last-num x))
                              (concat (or (butlast parts) []) [(conj (or (last parts) []) x)])
                              (concat parts [[x]]))
                            (concat parts [[x]])))
                  []
                  sset)]
      (map (juxt first last) parts))))

(defcheck solution-f8081ed1
  (fn [c]
    (if (empty? c)
      []
      (let [pair-diffs          (fn [c]
                                  (->> (map (fn [[a b]] [(- b a) b]) c)
                                    (cons [1 1])))
            partition-intervals (fn partition-intervals [s]
                                  (when-let [z (seq s)]
                                    (let [h   (second (first z))
                                          run (cons h (map #(second %)
                                                        (take-while #(= 1 (first %)) (next z))))]
                                      (cons run (partition-intervals (seq (drop (count run) z)))))))
            sorted-c            (sort (seq (into #{} c)))
            sorted-pairs        (partition 2 1 sorted-c)
            diffed-pairs        (pair-diffs sorted-pairs)]
        (->> (partition-intervals diffed-pairs)
          (map (fn [s] [(apply min s) (apply max s)])))))))

(defcheck solution-f80938b1
  (fn [xs]
    (map #(list (first %) (last %))
      (reduce (fn [acc e]
                (cond (= (last (last acc)) (dec e))
                      (conj (vec (butlast acc)) (conj (last acc) e))
                      (= (last (last acc)) e)
                      acc
                      :else
                      (conj acc [e]))

                )
        [] (sort xs)))))

(defcheck solution-f87e6a65
  (fn
    [s]
    (letfn [(increasing [s]
              (reduce
                #(if (= 1 (- %2 (last %1)))
                   (conj %1 %2)
                   %1)
                [(first s)]
                (rest s)))
            (increasing-seqs [s]
              (if (not (empty? s))
                (let [i (increasing s)]
                  (cons i (increasing-seqs (drop (count i) s))))))]
      (map #(vector (first %) (last %)) (increasing-seqs (distinct (sort s)))))))

(defcheck solution-f8af5f3c
  (fn intervals
    ([input] (if (empty? input) [] (intervals (sort (distinct input)) [])))
    ([input result] (intervals (rest input) result (first input) (first input)))
    ([input result head tail] (if (empty? input)
                                (conj result (vector head tail))
                                (let [current (first input)]
                                  (if (= current (inc tail))
                                    (intervals (rest input) result head current)
                                    (intervals input (conj result (vector head tail)))))))
    ))

(defcheck solution-f8fc6623
  (fn [in]
    (->> in
      sort
      (reduce (fn [acc item]
                (let [done (butlast acc)
                      [start finish] (last acc)]
                  (cond
                    (nil? finish) [[item item]]
                    (or (= finish (dec item)) (= finish item)) (concat done [[start item]])
                    :else (concat done [[start finish]] [[item item]])
                    ))) []))))

(defcheck solution-f9401ab3
  (fn intervals [l]
    (let [s (sort > l)
          f (fn [ll x]
              (if (>= x (dec (first (first ll))))
                (cons (list x (second (first ll))) (rest ll))
                (cons (list x x) ll)))]
      (if (empty? l)
        []
        (reduce f (list (list (first s) (first s))) (rest s))))))

(defcheck solution-f96fc360
  (fn [y]
    (if (empty? y)
      y
      (let [p (sort (distinct y))]
        (->> (reduce (fn [y x] (if (= (inc (last (last y))) x)
                                 (conj (into [] (butlast y)) (conj (last y) x))
                                 (conj y [x])))
               [[(first p)]] (rest p))
          (map #(if (> (count %) 1) [(first %) (last %)] [(first %) (first %)])))))))

(defcheck solution-f9fd017a
  (fn [t]
    (let [a apply
          c count
          s :s
          t (distinct (sort t))
          q (a concat
              (map
                #(let [f (first %)
                       l (if (= 1 (c %)) f (last %))]
                   (if (< (- l f) 2) [f l] [f s l]))
                (if
                 (= 1 (c t))
                  [t]
                  (partition 2 1 t))))]
      (map (fn [c] [(a min c) (a max c)]) (remove (partial = [s]) (partition-by #(= s %) q))))))

(defcheck solution-fa84c770
  (fn [s]
    (letfn [(f [lst]
              (loop [[f & xs] (sort lst)
                     r []
                     i []]
                (cond (nil? f) r
                      (and xs (>= f (dec (first xs)))) (recur xs r (conj i f))
                      :else (recur xs (conj r (conj i f)) []))))]
      (map #(apply (juxt min max) %) (f s)))))

(defcheck solution-faeb608e
  (fn f
    ([xs]
     (if (seq xs)
       (let [ys (sort (set xs))
             y  (first ys)]
         (f (rest ys) y y))
       ()))
    ([xs start end]
     (if (seq xs)
       (let [x (first xs)]
         (if (<= x (inc end))
           (f (rest xs) start x)
           (cons [start end] (f (rest xs) x x))))
       (list [start end])))))

(defcheck solution-fb953457
  (fn [coll]
    (let [abc    (sort coll)
          a      (first abc)
          finish (fn [[acc a z]] (if-not z acc (conj acc [a z])))]
      (->> abc
        (reduce
          (fn [[acc a z] nxt]
            (if (<= nxt (inc z))
              [acc a nxt]
              [(conj acc [a z]) nxt nxt]))
          [[] a a])
        finish))))

(defcheck solution-fc09a596
  #(reduce
     (fn [[[f l] & r :as a] e]
       (if (and
            f
            (> e (- f 2))
            (< e (+ l 2)))
         (cons [(min e f) (max e l)] r)
         (cons [e e] a)))
     []
     (sort-by - %)))

(defcheck solution-fc5f60fd
  (fn f [s]
    (if (= s []) []
                 (let [a first b last m map c cons
                       s (sort s)
                       i (m list s
                           (c 0 (m #(- (b %) (a %))
                                  (partition 2 1 [0] s))))
                       [x y] (split-with #(< (nth % 1) 2) i)
                       r [(a (a x)) (a (b x))]]
                   (c r (f (m a y)))))))

(defcheck solution-fd318176
  (fn intervals
    [s]
    (if (empty? s) s
                   (let [[s e a]
                         (let [sorted (sort (into [] (into #{} s)))]
                           (reduce
                             (fn [[cstart cend accum] next]
                               (if (= next (+ cend 1))
                                 [cstart next accum]
                                 [next next (conj accum [cstart cend])]))
                             [(first sorted) (first sorted) []]
                             (rest sorted)))]
                     (conj a [s e])))))

(defcheck solution-fd6b08a
  (fn intervals2 [xs]
    (let [intvs (fn intvs ([s f x & xs]
                           (let [step2 (- x f)]
                             (if (= 1 step2)
                               (apply intvs (concat [s x] xs))
                               (let [nxt     (if (= step2 1) [f x] [x x])
                                     results (apply intvs (concat nxt xs))]
                                 (cons [s f] results)))))
                  ([s f] [[s f]])
                  ([f] [])
                  ([] []))]
      (apply intvs (#(cons (first %) %) (sort (set xs)))))))

(defcheck solution-fd958a45
  (fn [s] (->>
            (apply sorted-set s)
            (map list (range))
            (partition-by (fn [[a b]] (- b a)))
            (map #(map last %))
            (map (juxt first last))

            )))

(defcheck solution-fd9b7ca0
  (fn [v]
    (let [s (apply sorted-set v)]
      (partition 2
        (interleave
          (filter #(not (contains? s (dec %))) s)
          (filter #(not (contains? s (inc %))) s))))))

(defcheck solution-fe5c0770
  (fn [l]
    (reverse (loop [xs (distinct (sort l))
                    r  []]
               (if (empty? xs)
                 r
                 (recur (rest xs)
                   (if (empty? r)
                     [[(first xs) (first xs)]]
                     (if (= (inc (second (first r))) (first xs))
                       (cons [(first (first r)) (first xs)] (rest r))
                       (cons [(first xs) (first xs)] r)))))))))

(defcheck solution-fe673524
  (fn __ [coll]
    (let [coll        (sort (distinct coll))
          by-twos     (partition 2 1 coll)
          by-ranges   (partition-by #(if (= (inc (first %)) (second %))
                                       (= (inc (first %)) (second %))
                                       (first %))
                        by-twos)
          good-ranges (filter #(= (count %) (inc (- (last %) (first %)))) (map (comp distinct flatten) by-ranges))
          bad-ranges  (filter #(not= (count %) (inc (- (last %) (first %)))) (map (comp distinct flatten) by-ranges))
          lone-ranges (map vector (distinct (filter #(not (some #{%} (flatten good-ranges))) (flatten bad-ranges))))]
      (if (= 1 (count coll))
        (vector (vector (first coll) (first coll)))
        (sort (map (juxt first last) (concat good-ranges lone-ranges)))))))

(defcheck solution-ff8093c5
  (fn [coll]
    (reduce (fn [acc v]
              (let [acc (if (seq acc) acc [[v v]])
                    [a b] (last acc)]
                (cond
                  (= b v) acc
                  (= (inc b) v) (conj (vec (butlast acc)) [a v])
                  :else (conj acc [v v])))) [] (sort coll))))

(defcheck solution-ff9de4f6
  (fn iv [xs]
    (if (empty? xs) []
                    (let [s (sort xs)]
                      (loop [init (first s)
                             r    (rest s)
                             prev init
                             acc  []]
                        (if (empty? r) (concat acc [[init prev]])
                                       (let [nxt (first r)]
                                         (if (> 2 (- nxt prev))
                                           (recur init (rest r) nxt acc)
                                           (recur nxt r nxt (concat acc [[init prev]]))))))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-171))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
