(ns coal-mine.problem-97
  (:require [coal-mine.checks :refer [defcheck-97] :rename {defcheck-97 defcheck}]
            [clojure.test]))

(defcheck solution-1002a263
  (fn [row]
    (letfn [(fact [n] (reduce * (range 1 (inc n))))
            (binom [n k] (/ (fact n) (fact k) (fact (- n k))))]
      (map (partial binom (dec row)) (range row)))))

(defcheck solution-1064ad9f
  (fn [n]
    (let [fct #(reduce * (range 1 (inc %1)))
          chs #(/ (fct %1) (* (fct %2) (fct (- %1 %2))))]
      (map #(chs (dec n) %)  (range n)))))

(defcheck solution-1074740f
  (fn [num]
    (loop [row '(1)
           cnt 1]
      (if (= cnt num)
        row
        (let [middle-combinations (map #(vector %1 %2) row (rest row))
              middle-seq (map #(apply + %) middle-combinations)
              new-row (concat '(1) middle-seq '(1))]
          (recur new-row (inc cnt)))))))

(defcheck solution-107bd66d
  (fn[n](nth (iterate
               #(let [v (concat [0] % [0])]
                  (vec (map + v (rest v))))
               [1]) (dec n))))

(defcheck solution-10fb4e53
  (fn [x]
    "http://en.wikipedia.org/wiki/Pascal's_triangle"
    (let [coll (if (coll? x) x (vector x))]
      (let [res
            (for [i coll]
              (let [r (dec i)]
                (reductions #(int (* %1 (/ (+ r 1 (- %2)) %2))) 1 (range 1 (inc r)))
                ))]
        (if (coll? x)
          res
          (nth res 0)
          )))))

(defcheck solution-110cb271
  (fn [n]
    (letfn [(next-level [c]
              (if (< (count c) 2)
                [1 1]
                (conj (vec (cons 1 (map #(+ (first %) (last %)) (partition 2 1 c)))) 1)))
            (my-seq
              ([] (my-seq [1]))
              ([c] (cons c (lazy-seq (my-seq (next-level c))))))]
      (nth (my-seq) (- n 1)))))

(defcheck solution-1153413f
  (fn p-t [i] (let [fac (fn [x] (apply * (range 1 (inc x)))), get-item (fn [r c] (/ (fac r) (* (fac (- r c)) (fac c))))]
                (for [y (range i)] (get-item (dec i) y)))))

(defcheck solution-1185ea02
  (fn pascal [p]
    (let [fact #(reduce * (range 1 (inc %)))
          nchoose (fn [n] (fn [k] (/ (fact n) (* (fact k)(fact (- n k))))))
          nchoosek (nchoose (dec p))]
      (map nchoosek (range p)))))

(defcheck solution-11b2dac7
  (fn [n]
    (let [pascals-row  (fn pascals-row [m prev]
                         (if (= m n)
                           prev
                           (let [next (concat [1] (map #(apply + %) (map vector prev (drop 1 prev))) [1])]
                             (pascals-row (inc m) next))))]

      (pascals-row 1 [1]))))

(defcheck solution-11c8b60f
  (fn pascal [n]
    (loop [r 1 res [1]]
      (if (= r n)
        res
        (recur (inc r)
          (vec (map + (cons 0 res) (conj res 0))))))))

(defcheck solution-11f0766e
  (fn pascal [n]
    (if (= n 1)
      [1]
      (loop [acc [1 1]]
        (if (>= (count acc) n)
          acc
          ;; (recur (into [] (flatten (conj [1] (map (partial reduce +) (partition 2 1 acc)) 1))))))))
          (recur (->> acc
                   (partition 2 1)
                   (map (partial reduce +))
                   (#(conj [1] % 1))
                   flatten
                   (into []))))))))

(defcheck solution-12141118
  (fn [n]
    (last
      (loop [i 1, rows []]
        (cond
          (= n 1) [[1]]
          (= i n) rows
          :default
          (recur
            (inc i)
            (let [numbers (partition 2 1 (last rows))]
              (conj rows (concat [1] (map (partial apply +) numbers) [1])))))))))

(defcheck solution-1214fe16
  (fn [m]
    (if (= m 1)
      [1]
      (let [n (dec m)]
        (letfn [(f1 [x]
                  (reduce #(* %1 (inc %2)) 1 (range x)))
                (f2 [s x]
                  (/ (f1 n)
                    (* (f1 x)
                      (f1 (- s x)))))]
          (map #(f2 n %) (range (inc n))))))))

(defcheck solution-1288f841
  (fn[n](
          loop [iter 1 s [1]]
          (if (>= iter n) s
                          (recur (inc iter) (concat [1]

                                                    (map-indexed (fn[idx itm](+ itm (nth s (inc idx)) )) (butlast s))

                                                    [1]))
                          )
          )))

(defcheck solution-12ec5f17
  (fn pascal-row [n]
    (cond
      (= n 1) [1]
      :else (let [end [1]
                  middle (map
                           (partial apply +)
                           (partition 2 1 (pascal-row (dec n))))]
              (concat end middle end)))))

(defcheck solution-13abcbcc
  (fn [a]
    ; improved by adereth's solution,
    ; mine was based on the same idea with an exceptional case of a = 1.
    (nth (iterate #(mapv + (cons 0 %) (conj % 0)) [1]) (dec a))))

(defcheck solution-13b1bf7f
  (fn p97[in]
    (cond (= in 1) [1]
          (= in 2) [1 1]
          :else
          (letfn [(b [level cache]
                    (for [x (range 1 (dec level))]
                      (+ (nth cache (dec x))
                         (nth cache x))))
                  (wrap [s] (conj (into [] (cons 1 s)) 1))]
            (loop [l 3 c [1 1]]
              (if (= l in)
                (wrap (b l c))
                (recur (inc l) (wrap (b l c)))))))))

(defcheck solution-1400100e
  (fn [i]
    (let [next-row (fn [previous-row]
                     (conj (reverse (conj (map (fn [pair] (apply + pair)) (partition 2 1 previous-row)) 1)) 1))]
      (if (= i 1) [1] (first (drop (dec i) (iterate next-row [1])))))))

(defcheck solution-14077083
  (fn [n]
    (let [rows (iterate (fn [row]
                          (concat [1]
                                  (map #(apply + %) (partition 2 1 row))
                                  [1]))
                 '(1))]
      (first (drop (dec n) rows)))))

(defcheck solution-141b70c1
  #(last (take %1 (iterate (fn [x] (into [] (map + (concat [0] x) (concat x [0])))) [1]))))

(defcheck solution-1426cb43
  (fn [n](nth (iterate #(map + (cons 0 %) (conj (vec %) 0)) '(1)) (dec n))))

(defcheck solution-1433e74e
  (fn foo [n]
    (nth (iterate #(conj ((fn i [s]
                            (let [[a b & o] s]
                              (if (nil? b) (list a)
                                           (conj (i (rest s)) (+ b a))))) %1) 1) [1]) (- n 1))))

(defcheck solution-14840771
  (fn f [n]
    (if (= n 1)
      [1]
      (loop [v (f (dec n))
             ans [1]
             n 0]
        (if (= n (- (count v) 1))
          (conj ans 1)
          (recur v
            (conj ans (+ (nth v n) (nth v (inc n))))
            (inc n)))))))

(defcheck solution-14eb52d3
  (fn tri [x]
    (if (= 1 x) [1]
                (let [prev (tri (dec x))]
                  (map + (concat prev [0])
                    (concat [0] prev))))))

(defcheck solution-150ad02c
  (fn pascal [r]
    (loop [c 1
           acc [1]]
      (let [v (* (nth acc (- c 1)) (/ (- r c) c))]
        (if (= v 0)
          acc
          (recur (inc c) (conj acc v)))))))

(defcheck solution-1517eef3
  (fn [n]
    (nth
      (iterate
        #(concat '(1) (map (partial apply +) (partition 2 1 %)) '(1))
        [1]) (dec n))))

(defcheck solution-1530da5b
  (fn pt [n] (if (= n 1) [1] (let [prev (pt (dec n))] (concat [1] (map + (drop-last prev) (rest prev)) [1])))))

(defcheck solution-15629044
  (fn[n] (let [fac #(apply * (range 1 (inc %))) com #(/ (fac (dec n)) (* (fac %1) (fac (- (dec n) %1))))] (map com (range 0 n)))))

(defcheck solution-1570fabb
  (fn [x]
    (letfn
     [
      (! [n] (if (<= n 1) 1 (* n (! (dec n)))))
      (choose [n r] (/ (! n) (* (! r) (! (- n r)))))
      ]
      (map #(choose (- x 1) %) (range x))
      )
    ))

(defcheck solution-160c6773
  (fn [row]
    (letfn [(pn [r c]
              (if (or (= c 1) (= c r))
                1
                (+ (pn (dec r) (dec c)) (pn (dec r) c))))]
      (mapv #(pn row %) (range 1 (inc row))))))

(defcheck solution-161c8994
  (fn [x]
    (nth
      (iterate #(concat [1] (map + % (rest %)) [1]) [1])
      (dec x))))

(defcheck solution-16a958d4
  (fn pas [size]
    (if (= 1 size) [1]
                   (let [prior  (pas (dec size))
                         inner  (loop [col (rest prior)
                                       hd  (first prior)
                                       acc [ 1 ]]
                                  (if (empty? col) (conj acc 1)
                                                   (recur (rest col) (first col) (conj acc (+ hd (first col))))))]
                     inner))))

(defcheck solution-16a9e985
  (fn [n]
    (letfn [(nextRow [row] (concat [1] (map (partial apply +) (partition 2 1 row)) [1]))]
      (nth (iterate nextRow [1]) (dec n))
      )
    ))

(defcheck solution-16b5f6a2
  (fn ptr [n]
    (if (= n 1)
      [1]
      (let [prev (ptr (dec n))]
        (concat [(first prev)]
                (map + (butlast prev) (rest prev))
                [(last prev)])))))

(defcheck solution-1732cb94
  #(nth (iterate (fn tri [coll] (loop [l (concat [0] coll [0]) r '[]]
                                  (if (< (count l) 2)
                                    r
                                    (recur (rest l) (conj r (+ (first l) (second l))))))) [1]) (dec %)))

(defcheck solution-177a9a9c
  (fn p[x] (
             letfn [(r[v] (
                            concat [1] (loop [i 0 acc []] ( if (< i (dec (count v))) (recur (inc i) (concat acc [(+ (nth v i) (nth v (inc i)))] )) acc)) [1]
                                   ))]

             (if (= x 1) [1]
                         (r (p (dec x)))
                         )
             )))

(defcheck solution-17a7c2e4
  (fn pascal [lvl]
    (loop [i lvl pyr '(1)]
      (if (= 1 i) pyr
                  (recur
                    (dec i)
                    (map + (cons 0 pyr) (reverse (cons 0 pyr))))))))

(defcheck solution-17ce3bfb
  (fn [deep]
    (loop [result [1] index 1]
      (if (= index deep)
        result
        (recur
          (loop [internal-result [] elements (cons 0 (conj result 0)) i 0]
            (if (= i (+ (count result) 1))
              internal-result
              (recur (conj internal-result (+ (nth elements i) (nth elements (+ i 1)))) elements (inc i))
              )
            )
          (inc index))
        )
      )
    ))

(defcheck solution-1886b1f8
  (fn [n]
    (last
      (take n
        (iterate
          (fn next-row [previous-row]
            (into []
              (map (fn [e] (reduce + e))
                (partition 2 1
                  (conj (into [0] previous-row) 0)))))
          [1])))))

(defcheck solution-18c78836
  (fn [i] (nth (iterate #(map + `(0 ~@%) `(~@% 0)) [1]) (- i 1))))

(defcheck solution-1926fe2
  #(reduce (fn nextrowgen [row _]
             (let [row-len (inc (count row))
                   half-idx (quot row-len 2)
                   half-row (map-indexed (fn [idx itm]
                                           (let [a (or (get row (dec idx)) 0) b (or (get row idx) 0)]
                                             (+ a b)
                                             )
                                           ) (range (or (and (odd? row-len) (inc half-idx)) half-idx) ))]
               (vec (concat half-row (reverse (take half-idx half-row)))) )) [1] (range (dec %))))

(defcheck solution-198ffb7a
  (fn [i] (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) [1]) (dec i))))

(defcheck solution-19a67305
  (fn my-pascal-triangle
    [num]
    (cond
      (= num 1) (vector 1)
      (= num 2) (vector 1 1)
      :else
      (letfn [(calc-next-pascal-step [previous]
                (conj (into [1] (map #(reduce + %) (partition 2 1 previous))) 1))]
        (loop [i (- num 2) result (vector 1 1)]
          (if (zero? i)
            result
            (recur (dec i) (calc-next-pascal-step result))))))))

(defcheck solution-19ed4584
  (fn [n] (map #(/ (reduce * (range (inc %) n)) (reduce * (range 1 (- n %)))) (range n))))

(defcheck solution-1ab2b548
  (fn [n] (loop [n (- n 1) r (vector 1)]
            (if (zero? n)
              r
              (recur (- n 1) (concat [1] (map  #(+ (nth r %) (nth r (- % 1))) (range 1 (count r))) [1] ))))))

(defcheck solution-1b14fa58
  (fn [row]
    (letfn [(factorial [n] (reduce * (range 1 (inc n))))
            (combination [n k] (/ (factorial n) (* (factorial k) (factorial (- n k)))))
            ]
      (map #(combination (dec row) %) (range 0 row))
      )
    ))

(defcheck solution-1b570942
  (fn [n]
    (reduce
      (fn [acc val]
        (conj acc (/ (* (last acc) (- (dec n) val)) (+ val 1))))
      [1]
      (range (dec n)))))

(defcheck solution-1c15e734
  (fn pascal [n]
    (if (= n 1) [1]
                (let [pn (dec n)
                      prev (pascal pn)
                      augprev (concat [0] prev [0])]
                  (map #(+ (nth augprev %) (nth augprev (inc %))) (range n))))))

(defcheck solution-1c3d25c8
  (fn pascal-tri [n]
    (condp = n
      1 [1]
      (let [last (pascal-tri (dec n))
            last-a (concat [0] last)
            last-b (concat last [0])
            ]
        (vec(map + last-a last-b))))))

(defcheck solution-1cd71d3
  (fn [n] (nth (iterate #(vec (map + (conj % 0) (cons 0 %))) [1]) (dec n))))

(defcheck solution-1d6a209b
  (fn pascal [x]
    (if (= x 1)
      [1]
      (let [prev (pascal (- x 1))]
        (vec (map + (conj prev 0) (cons 0 prev)))))))

(defcheck solution-1dccebfc
  (fn pascal [n]
    (when (> n 0)
      (if (= n 1) [1]
                  (flatten (vector 1 (map (fn [[x y]] (+ x y)) (partition 2 1 (pascal (dec n)))) 1))))))

(defcheck solution-1ddf2d30
  (fn [n]
    (last (take n (iterate #(map + (concat [0] %) (concat % [0])) [1])))))

(defcheck solution-1ed2b901
  (fn [n]
    (let [n (dec n)]
      (loop [previous n
             acc [1 n]
             c (dec n)]
        (cond (zero? n) [1]
              (= 1 (last acc)) acc
              :else (let [next-num (/ (* previous c) (count acc))]
                      (recur next-num
                        (conj acc next-num)
                        (dec c))))))))

(defcheck solution-1ef95c0c
  (fn [n]
    (nth (iterate #(vec (map + (conj % 0) (cons 0 %))) [1]) (dec n))))

(defcheck solution-1efdf829
  (fn [rows]
    (reduce
      #(conj %1 (* (last %1) (/ (- rows %2) %2)))
      [1]
      (range 1 rows))))

(defcheck solution-1f2b09d8
  (fn [n]

    (let [a (fn [c] (into [] (map + (cons 0 c) (conj c 0))))]

      (loop [coll [1] c 1]

        (if (= c n)

          coll

          (recur (a coll) (inc c)))))))

(defcheck solution-1feac831
  (fn pascal_s-triangle [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (conj (first (reduce

                               (fn [[a,l] x]
                                 (if (nil? l) [a,x]
                                              [(conj a (+ l x)), x]))
                               [[1],nil]

                               (pascal_s-triangle (dec n)))) 1))))

(defcheck solution-2081a20e
  (fn pascal [i]
    (nth
      (iterate
        (fn pascal-row [previous]
          (concat '(1) (map #(apply + %) (partition 2 1 previous)) '(1)))
        '(1))
      (- i 1))))

(defcheck solution-20ac18de
  (fn pascals [n]
    (let [pascal (fn pascal [s]
                   (mapv (partial apply +) (partition 2 1 (conj (into [0] s) 0))))]
      (nth (cons [1] (iterate pascal [1])) n)
      )))

(defcheck solution-21571cc9
  (fn [n]
    (letfn [
            (each-cons [x] (map list x (drop 1 x)))
            (next-row [r] (concat [1]
                                  (map (partial apply +) (each-cons r))
                                  [1]))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-218d9d96
  (fn [n] (map #(/ (apply * (range (- n %) n))
                  (apply * (range 1 (+ % 1))))
            (range n))))

(defcheck solution-21c91db2
  (fn [row-number]
    (let [fact #(reduce * (map inc (range %))),
          binom (fn [n k] (/ (fact n) (* (fact k) (fact (- n k)))))]
      (for [i (range row-number)] (binom (dec row-number) i)))))

(defcheck solution-223c61d9
  (fn p [n]
    (if (<= n 1)
      [1]
      (let [c (p (dec n))]
        (vec (map + (cons 0 c) (conj c 0)))))))

(defcheck solution-226ff734
  (fn [val] (loop [n (dec val) k 1 l 1 result []]
              (if (= 0 n)
                (conj result 1)
                (let [v (* l (/ n k))]
                  (recur (dec n) (inc k) v (conj result l)))
                ))))

(defcheck solution-2280d8f9
  #(loop [s [1], i %] (if (= 1 i) s (recur (map + (lazy-cat [0] s) (lazy-cat s [0])) (dec i)) )))

(defcheck solution-22a341f9
  #(nth (iterate (fn [x] (concat [1] (map + x (rest x)) [1])) [1]) (dec %)))

(defcheck solution-23430b10
  (fn pascal [r]
    (if (= r 1)
      [1]
      (vec (concat [1]
                   (map #(reduce + %) (partition 2 (drop 1 (let [prev (pascal (dec r))]
                                                             (interleave prev prev)))))
                   [1])))))

(defcheck solution-236ce2a3
  (fn pascal [x] (if (= 1 x) [1] (let [
                                       pp (conj (pascal (dec x)) 0)]
                                   (map + pp (reverse pp))))))

(defcheck solution-237b8980
  (letfn [(next-row [row]
            (loop [i 0, acc []]
              (cond (>= i (dec (count row))) (into [] (concat [1] (conj acc 1)))
                    :else (recur (inc i) (conj acc (+ (row i) (row (inc i))))))))
          (get-row [n]
            (loop [i 1, acc [1]]
              (cond (= i n) acc
                    :else (recur (inc i) (next-row acc)))))]
    get-row))

(defcheck solution-241ed21
  (fn [r]
    (loop [c r
           a [1]]
      (if (= 1 c)
        a
        (recur (dec c) (conj (into [1] (map (partial apply +) (partition 2 1 a))) 1))))))

(defcheck solution-2436020a
  (fn pascals-nth-row
    [n]
    (case n
      1 [1]
      (loop
       [idx 0 row []]
        (if (= idx (- n 2))
          (cons 1 (conj row 1))
          (recur
            (+ idx 1)
            (let [next-row-up (pascals-nth-row (- n 1))] (conj row (+ (nth next-row-up idx) (nth next-row-up (+ idx 1)))))))))))

(defcheck solution-243646f4
  (fn p [r]
    (let [n (- r 1)]
      (case n
        0 [1]
        1 [1 1]
        (let [bn (p n)]
          ((fn l [v i]
             (if (= i 0)
               (conj v 1)
               (l
                 (conj v (+ (get bn i)
                            (get bn (- i 1))))
                 (- i 1)))) [1] (- n 1)))))))

(defcheck solution-245ad97a
  [1
   [1]
   [1 1]
   [1 2 1]
   [1 3 3 1]
   [1 4 6 4 1]
   1
   1
   1
   1
   1
   [1 10 45 120 210 252 210 120 45 10 1]])

(defcheck solution-247eeb1f
  (fn p [x]
    (if (= x 1)
      [1]
      `[1 ~@(map + (p (- x 1)) (next (p (- x 1)))) 1])))

(defcheck solution-2506fa81
  (fn pascal [n]
    (loop [m n
           acc [1]]
      (if (= m 1)
        acc
        (recur (dec m) (concat [1] (map #(apply + %) (partition 2 1 acc)) [1]))))))

(defcheck solution-252d534f
  (fn [x]
    (let [f
          (fn [lst]
            (map (fn [[x y]] (+ x y)) (map vector (conj (into [] lst) 0) (cons 0 (into [] lst)))))]
      (into [] (nth (iterate f [1]) (dec x))))))

(defcheck solution-256f48c
  (fn pt [depth]
    (let [next-row (fn [row]
                     (loop [prev 0
                            nums row
                            result []]
                       (if (empty? nums)
                         (conj result 1)
                         (recur (first nums) (rest  nums) (conj result (+  prev (first nums)))))))]
      (nth (iterate next-row []) depth))))

(defcheck solution-2607651
  (fn p-tri [n]
    (if (= n 1) '(1)
                (concat '(1)
                        (map #(apply + %)
                          (partition 2 1 (p-tri (dec n))))
                        '(1)))))

(defcheck solution-269faa25
  (fn [x]
    (letfn
     [(iteradd
        [l]
        (if (< (count l) 2)
          []
          (concat [(+ (first l) (second l))]  (iteradd (rest l))))
        )
      (iteriter
        [n l]
        (if (= 0 n) l
                    (iteriter
                      (- n 1)
                      (iteradd (into (into [0] l) [0])))))
      ]
      (iteriter (- x 1) [1])
      )))

(defcheck solution-26f2131b
  (fn p [n]
    (if
     (= 1 n) [1]
             (let [r (p (dec n))
                   i (interleave r r)
                   a (->> i rest butlast (partition 2))
                   s (map #(apply + %) a)]
               (concat [1] s [1])))))

(defcheck solution-28291949
  (letfn
   [(next-pascal [prev-row]
      (concat [1] (map #(apply + %) (partition 2 1 prev-row)) [1]))
    (lazy-pascal []
      (iterate next-pascal [1]))]
    (fn [n] (nth (lazy-pascal) (dec n)))))

(defcheck solution-284e4f8
  (fn [i]
    (nth
      (iterate
        #(conj (vec (map + % (cons 0 %))) 1)
        [1])
      (dec i))))

(defcheck solution-288ca8fa
  (fn pascal [n]
    (if (= n 1) [1]
                (let [row (pascal (dec n))]
                  (vec (map +
                         (conj row 0)
                         (cons 0 row)))))))

(defcheck solution-2890301d
  (fn pascal-triangle
    [n]
    (let [factorial (fn [n] (apply * (range 1 (inc n))))
          binomial-coeff (fn [n k] (/ (factorial n)
                                     (* (factorial k) (factorial (- n k)))))]
      (map binomial-coeff (repeat (dec n)) (range 0 n)))))

(defcheck solution-289c89c9
  (fn pascals-triangle-row [n]
    (map (comp last take)
      (range n 0 -1)
      (take n (iterate (partial reductions +) (take n (repeat 1)))))))

(defcheck solution-28c798cf
  (fn [n]
    (nth
      (iterate
        #(vec (map + (conj % 0) (cons 0 %)))
        [1])
      (dec n))))

(defcheck solution-28da6d3
  (fn pascal-row [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (concat
                 [1]
                 (let [prev-row (pascal-row (- n 1))]
                   (reduce #(concat (butlast %1)
                                    [(+ (last %1) %2)]
                                    [%2])
                     [(first prev-row)]
                     (rest prev-row)))
                 ))))

(defcheck solution-28e0e5bd
  (fn [n]
    (reduce (fn [last _]
              (concat [1]
                      (map #(apply + %) (partition 2 1 last))
                      [1]))
      [1]
      (range 1 n))))

(defcheck solution-28eb55ad
  (fn [n]
    (loop [i n
           row [1]]
      (if (= i 1)
        row
        (recur (dec i) (map + (concat row [0]) (concat [0] row)))))))

(defcheck solution-299a3359
  (fn pascal-triangle [row]
    (condp = row
      1 [1]
      (vec
        (concat
         [1]
         (map (partial apply +)
           (partition 2 1
             (pascal-triangle (dec row))))
         [1])))))

(defcheck solution-29fcbfc8
  (fn [n] (letfn [(next-row [row] (map + `(0 ~@row) `(~@row 0)))
                  (nth-row [n] (last (take n (iterate next-row [1]))))]
            (nth-row n))))

(defcheck solution-2a36851b
  (fn pasc [row]
    (let [step (fn step [sq]
                 (if (< (count sq) 2)
                   []
                   (vec (cons (+ (first sq) (second sq))
                          (step (rest sq))))))]
      (if (= row 1) [1]
                    (step (cons 0 (conj (pasc (dec row)) 0)))))))

(defcheck solution-2a36b2ad
  (fn [n]
    (nth
      (iterate
        (fn [row]
          (concat
           [1]
           (map (partial apply +) (partition 2 1 row))
           [1]))
        [1])
      (dec n))))

(defcheck solution-2a423980
  (fn pascal [n]
    (condp = n
      1 [1]
      2 [1 1]
      (->>
        (pascal (- n 1))
        (partition-all 2 1)
        (map
          (partial reduce +))
        (concat [1])))))

(defcheck solution-2a49b4fa
  (fn pascal-triangle [n]
    (nth (iterate #(concat [1] (map + % (rest %)) [1])
           [1])
      (dec n))))

(defcheck solution-2b1be35f
  (fn pascal [n]
    (if (= n 1)
      [1]
      (concat [1]
              (map (fn [[a b]] (+ a b)) (partition 2 1 (pascal (dec n))))
              [1]))))

(defcheck solution-2be0bf1e
  (fn pasc [x] (if (= x 1) [1] (let [ant (pasc (dec x)) ] (concat [1] (map + ant (rest ant)) [1]) )  ) ))

(defcheck solution-2c279878
  (fn [a]
    (loop [x a y [1]]
      (if (= x 1)
        y
        (recur
          (dec x)
          (into [] (map + (conj y 0) (conj (into '() y) 0))) )))))

(defcheck solution-2c31df9e
  (fn tri [n]
    (nth (iterate (fn [lst] (mapv + (cons 0 lst) (conj lst 0))) [1]) (dec n))
    ))

(defcheck solution-2c4088c8
  (fn pascal-row [n]
    (letfn [(pascalize-n [in n]
              (if (= n 2)
                in
                (loop [a (first in)
                       b (or (second in) 1)
                       rst (rest in)
                       out [1]]
                  (if (empty? rst)
                    (pascalize-n (conj out 1) (dec n))
                    (recur (first rst)
                      (second rst)
                      (rest rst)
                      (conj out (+ a b)))))))
            ]
      (case n
        1 '[1]
        2 '[1 1]
        (pascalize-n '[ 1 1 ] n)))))

(defcheck solution-2c883f00
  (fn jj [x] (for [xx (range x)] (if (or (= xx 0) (= x 0)) 1
                                                           (/ (apply * (range (dec x) (- (dec x) xx) -1))
                                                             (apply * (range 1 (inc xx)))
                                                             )
                                                           )
                                 )
    ))

(defcheck solution-2c9a3970
  #(letfn [(p [i] (if (= 1 i) [1]
                              (let [q (p (dec i))] (vec (map + (conj q 0) (cons 0 q))))))]
     (p %)))

(defcheck solution-2d13248d
  (fn n97 [n]
    (letfn [(fac [k] (loop [x k a 1] (if (zero? x) a (recur (dec x) (* a x)))))
            (combin [x k] (/ (fac k) (fac x) (fac (- k x))))]
      (let [k (dec n)] (for [x (range (inc k))] (combin x k))))))

(defcheck solution-2d146d17
  #(letfn [(row [prev]
             (concat [1] (map (partial apply +) (partition 2 1 prev)) [1]))]
     (nth (iterate row [1]) (dec %))))

(defcheck solution-2da1cb38
  (fn [n]
    (let [n (dec n)]
      (take (inc n) (map second (iterate (fn [[k last]]
                                           [(inc k)
                                            (* last
                                              (/ (- (+ n 1) (inc k))
                                                (inc k)))])
                                  [0 1]))))))

(defcheck solution-2ddfc9b0
  (fn f[x](if(= x 1)[1](map #(apply + %)(partition 2 1`[0~@(f(dec x))0])))))

(defcheck solution-2e3d10ea
  (fn p [n]
    (case n
      1 [1]
      2 [1 1]
      (concat
       [1]
       (map #(reduce + %) (partition 2 1 (p (- n 1))))
       [1]))))

(defcheck solution-2e60aa
  (fn pascal-triangle [n]
    (letfn [(next-line [state]
              (concat [1] (map + (rest state) state) [1]))]
      (if (= n 1)
        [1]
        (reduce (fn [state n] (next-line state)) [1] (range (dec n)))))))

(defcheck solution-2e750d7b
  (fn [n]
    (loop [a n acc (list 1)]
      (if (= 1 a) acc
                  (recur (dec a) (concat (map + (cons 0 acc) acc) '(1)))))))

(defcheck solution-2e961515
  (fn [n]
    (let [pad #(concat (cons 1 %) [1])
          nextp (fn [s] (pad (map #(reduce + %) (partition 2 1 s))))
          triangle (iterate #(nextp %) [1])]
      (nth triangle (dec n)))))

(defcheck solution-2e9fd6bb
  (fn pas [n]
    (if (= n 1) [1]
                (lazy-cat [1]
                  (map
                    (partial apply +)
                    (partition
                      2
                      1
                      (pas (dec n))))
                  [1]))))

(defcheck solution-2ef9318
  (fn nth-row [n]
    (letfn
     [
      (fact [n] (reduce * 1 (range 1 (inc n))))
      (newton [k n] (quot (fact n) (* (fact k) (fact (- n k)) )))
      ]
      (vec
        (for [k (range n)]
          (newton k (dec n))
          )
        )
      )
    ))

(defcheck solution-2f4ffc02
  (fn [i] (nth (iterate #(map + (cons 0 %) (concat % [0])) [1]) (- i 1))))

(defcheck solution-2f55fd32
  (fn [level]
    ((fn [i row]
       (if (= i 1)
         row
         (recur
           (dec i)
           (map + (concat row [0]) (cons 0 row)))))
     level '(1))))

(defcheck solution-2f974684
  (fn
    [n]
    (if (= n 1)
      [1]
      (loop [m 2
             r [1]]
        (let [next-r (into [] (map #(apply + %) (partition 2 1(concat [0] r [0]))))]
          (if (= m n)
            next-r
            (recur (inc m) next-r)))))))

(defcheck solution-2fe24b9b
  (fn [n] (letfn [(nextline [s] (concat [1] (apply vector (map (partial apply +) (partition 2 1 s))) [1] ))] (loop [s [1] x n] (if (= x 1) s (recur (nextline s) (dec x)))))))

(defcheck solution-2ffbd046
  (fn pascal [n]
    (if (<= n 1)
      [1]
      (concat [1] (map #(+ (first %) (second %)) (partition 2 1 (pascal (dec n)))) [1]))))

(defcheck solution-30c46901
  (fn pascal [n]
    (if (= n 1)
      [1]
      (concat
       [1]
       (map (partial apply +)
         (partition 2 1 (pascal (dec n))))
       [1]))))

(defcheck solution-312747b3
  (fn pascal [i]
    (nth (iterate #(mapv + (conj % 0) (cons 0 %)) [1]) (dec i))))

(defcheck solution-3185b9ec
  (fn pascal [n]
    (if (= n 1) [1]
                (let [prev (pascal (dec n))
                      left (conj prev 0)
                      right (cons 0 prev)]
                  (mapv + left right)))))

(defcheck solution-31a46728
  (fn [n]
    (reductions
      (fn [prev k]
        (* prev (/ (- n k) k)))
      1 ; n choose 0
      (range 1 n))))

(defcheck solution-31adf311
  (fn [n]
    (letfn [(next-row [row] (map + `(0 ~@row) `(~@row 0)))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-32333305
  (fn pascal [n]
    (if (<= n 2)
      (repeat n 1)
      (let [prev (pascal (- n 1))
            prev-0 (concat [0] prev [0])
            new-parts (partition 2 1 prev-0)]
        (map #(apply + %) new-parts)))))

(defcheck solution-3233be8
  (fn [a]
    (letfn [(fact [x] (reduce * (range 2 (inc x))))
            (bc [n k] (/ (fact n) (* (fact k) (fact (- n k)))))]
      (map #(bc (dec a) %) (range 0 a)))))

(defcheck solution-326353d0
  (fn [n]
    (nth
      (iterate
        (fn [nums]
          (vec
            (map + (conj nums 0) (cons 0 nums))))
        [1])
      (dec n))))

(defcheck solution-32807caf
  (fn pascals-triangle [row]
    ;  (if (= row 1)
    ;    [1]
    ;    (let [prev (pascals-triangle (dec row))
    ;          back (last prev)]
    ;      (conj (vec (reverse (conj (mapv + prev (rest prev)) 1))) 1))))
    (nth
      (iterate #(mapv + (cons 0 %) (conj % 0)) [1])
      (dec row))))

(defcheck solution-32904ab9
  (fn [row-i]
    (loop [i 2 row [1]]
      (if (> i row-i)
        row
        (recur
          (inc i)
          (vec
            (for [j (range i)]
              (+ (get row (dec j) 0)
                 (get row j 0)))))))))

(defcheck solution-32ae727e
  (fn pascal [row]
    (if (= row 1)
      [1]
      (concat [1] (map #(apply + %)
                    (partition 2 1
                      (pascal (dec row)))) [1]))))

(defcheck solution-32e82204
  (fn pascal [n]
    (if (= n 1)
      [1]
      (vec (concat [1]
                   (map (partial apply +)
                     (partition 2 1 (pascal (dec n))))
                   [1])))))

(defcheck solution-32fbc014
  #(nth (iterate (fn n[r] (let [prev (concat [0] r [0]) next (for [i (range 0 (dec (count prev)))] (+ (nth prev i) (nth prev (inc i))))] next)) [1]) (dec %)))

(defcheck solution-3331cb86
  (fn [n]
    (let [
          ! #(apply * (range 1 (inc %1)))
          C #(/ (! %1) (* (! %2) (! (- %1 %2))))]
      (map (partial C (dec n)) (range 0 n)))))

(defcheck solution-33825a73
  (letfn
   [(next [v]
      (let [a (concat [0] v [0])]
        (map #(apply reduce + %&) (partition 2 1 a))))]
    #(nth (iterate next [1]) (dec %) )))

(defcheck solution-33ba759b
  (fn ps [n]
    (if (= n 1)
      [1]
      (map #(apply + %) (partition 2 1 (concat [0] (ps (- n 1)) [0]))))))

(defcheck solution-3407c562
  (fn [n] (letfn [(nxt [p]
                    (vec (concat [1] (map #(+ (p %) (p (inc %)))
                                       (range (dec (count p))))
                                 [1])))]
            (nth (iterate nxt [1]) (dec n)))))

(defcheck solution-341c64e3
  #(map ((fn p [r]
           (fn i [c]
             (if (= c 0) 1
                         (* (i (- c 1)) (/ (- r c) c))))) %) (range %)))

(defcheck solution-343dc533
  (fn mason [x]
    (if (= x 1)
      '(1)
      (loop [x x
             i 2
             coll '(1 1)]
        (if (= i x)
          coll
          (recur x (inc i) (flatten [1 (map + coll (rest coll)) 1]))
          )
        )
      )))

(defcheck solution-348f7407
  (fn pt [n]
    (if (<= n 1)
      [1]
      (into [] (concat [1]
                       (#(map + % (next %)) (pt (dec n)))
                       [1])))))

(defcheck solution-34ab4b68
  (fn [n]
    (map #((fn aux [n k]
             (if (or (= n k) (zero? k))
               1
               (+ (aux (dec n) (dec k)) (aux (dec n) k))))
           (dec n) %)
      (range n))))

(defcheck solution-3512cdd5
  (fn pascal [n]
    (let [er [1]
          iter (fn [row]
                 (loop [remaining row
                        result er]
                   (if (empty? (rest remaining))
                     (conj result 1)
                     (recur (rest remaining)
                       (conj result (+ (first remaining) (second remaining)))))))]
      (nth (iterate iter er) (dec n)))))

(defcheck solution-357e4a12
  (fn [n]
    (nth
      (iterate
        (fn [x]
          (reduce (fn [c [a b]] (conj c (+ a b))) []
            (partition 2 1 (conj (apply vector 0 x) 0))))
        [1])
      (dec n))))

(defcheck solution-35b67fa2
  (fn pt [n]  (nth (iterate #(map + (concat % [0]) (concat [0] %)) [1])(dec n))))

(defcheck solution-362b1513
  (fn me
    [n]

    (cond
      (= n 1) [1]

      (= n 2) [1 1]

      :else
      (let [me2 (fn me3 [myseq]
                  (if (empty? (rest myseq))

                    []
                    (concat (vector (vector (first myseq) (second myseq)) ) (lazy-seq (me3 (rest myseq))))
                    ))

            res1 (me2 (me (dec n)))

            res2 (concat (list 1) (map #(+ (first %) (second %)) res1) (list 1))
            ]

        (into [] res2)
        )
      )))

(defcheck solution-3630604
  (fn f
    ([x] (f (dec x) 1 [1]))
    ([x y r] (if (< x 1)
               r
               (f (dec x) (inc y) (conj r (*
                                            (last r)
                                            (/ x
                                              y))))))))

(defcheck solution-3631d22d
  (fn [n] (nth (iterate #(map +
                           (cons 0 %) (concat % [0]))
                 [1])
            (dec n))))

(defcheck solution-3644bbfe
  (fn trix [n]
    (if (< n 3)
      (repeat n 1)
      (let [prev (trix (dec n))]
        (->> (interleave prev prev)
          (drop 1)
          (partition 2)
          (map #(apply + %))
          (cons 1)
          reverse
          (cons 1))))))

(defcheck solution-364df472
  (fn [n]
    (let [l (map vector (next (range)) (range (dec n) 0 -1))]
      (reductions #(/ (* % (second %2)) (first %2)) 1 l))))

(defcheck solution-3703545e
  (fn [n]
    (nth (iterate #(map (partial apply +) (partition 2 1 (concat [0] % [0]))) [1]) (dec n))))

(defcheck solution-37b48d35
  (fn pascal-tri [n]
    (let [n (dec n)]
      (reduce #(conj %1 (/ (* (last %1) (- n %2)) (inc %2)))
        [1] (range n)))))

(defcheck solution-37b9936
  (fn pascal [order]
    (letfn [(transform [base]
              (concat [1]
                      (map + (rest base)
                        (butlast base))
                      [1]))]
      (nth (iterate transform [1]) (dec order)))))

(defcheck solution-383c6947
  (fn [n] (nth (iterate #(mapv (partial apply +) (partition-all 2 1 (cons 0 % ))) [1]) (dec n))))

(defcheck solution-384e0637
  (fn [val]
    (case val
      1 [1]
      2 [1 1]
      3 [1 2 1]
      4 [1 3 3 1]
      5 [1 4 6 4 1]
      6 [1 5 10 10 5 1]
      7 [1 6 15 20 15 6 1]
      8 [1 7 21 35 35 21 7 1]
      9 [1 8 28 56 70 56 28 8 1]
      10 [1 9 36 84 126 126 84 36 9 1]
      11 [1 10 45 120 210 252 210 120 45 10 1])))

(defcheck solution-386bc751
  (fn [i]
    (reduce
      #(conj %1 (* (last %1) (/ (- i %2) %2)))
      [1] (range 1 i))))

(defcheck solution-39291d8e
  (fn [m]
    (map
      #(/
         (reduce * (range % m))
         (reduce * (range 1 (- (inc m) %)))
         )
      (range 1 (inc m))
      )
    ))

(defcheck solution-3952b4af
  (fn [x]
    (map (fn ptr [[n k]]
           (if (zero? k)
             1
             (int (* (ptr [n (dec k)]) (/ (- (inc n) k) k)))))
      (partition 2 (interleave (cycle [(dec x)]) (range x))))))

(defcheck solution-39e9bbcc
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [p (pascal (dec n))]
        (vec (concat [1]
                     (for [i (range (dec (dec n)))]
                       (+ (p i) (p (inc i))))
                     [1]))))))

(defcheck solution-3a10dd12
  (fn pascal [n]
    (condp = n
      1 [1]
      2 [1 1]
      (vec (cons 1 (conj (mapv #(apply + %) (partition 2 1 (pascal (dec n)))) 1))))))

(defcheck solution-3a15b5eb
  #((fn f [v n] (if (= n 1) v (f
                                (vec (map + (conj v 0) (cons 0 v)))
                                (- n 1) ))) [1] %))

(defcheck solution-3a794e7
  (fn [row]
    (vec
      (letfn
       [(partial-factorial [from to]
          (reduce *
            (range from (inc to))))
        (factorial [n]
          (partial-factorial 1 n))
        (binomial-coefficient [n r]
          (/ (partial-factorial (- n (dec r)) n)
            (factorial r)))]
        (map (partial binomial-coefficient (dec row))
          (range 0 row))))))

(defcheck solution-3aa1a3f
  (fn [n]
    (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) [1]) (dec n))))

(defcheck solution-3b8fabd7
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [prev (pascal (dec n))]
        (concat [1] (map + prev (rest prev)) [1])))))

(defcheck solution-3b9ddb22
  (fn __ [n]
    (if (= n 1)
      [1]
      (->> (__ (dec n))
        (partition 2 1)
        (map (partial apply +))
        (#(concat (conj % 1) [1]))))))

(defcheck solution-3d1c19eb
  (fn pascal [l]
    (cond (= 1 l) [1]
          (= 2 l) [1 1]
          :else (conj (into [1] (map (partial apply +) (partition 2 1 (pascal (dec l))))) 1))))

(defcheck solution-3d526b2
  (fn [l] (letfn [(e [n k] (if (= 0 k)
                             1
                             (* (e n (dec k)) (/ (- n k) k))))]
            (map #(e l %) (range l)))))

(defcheck solution-3d72defa
  (fn [n]
    (loop [i 1 r [1]]
      (if (>= i n) r
                   (recur (inc i)
                     (concat [1] (map #(apply + %)
                                   (partition 2 1 r)) [1]))))))

(defcheck solution-3ddc846c
  (fn pascal [n]
    (loop [n_ n acc [1]]
      (if (= 1 n_)
        acc
        (recur (dec n_)
          (vec (map + (conj acc 0) (apply conj [0] acc))))))))

(defcheck solution-3df030f7
  (fn [r]
    (reductions #(* % (/ (- r %2) %2)) 1 (range 1 r))))

(defcheck solution-3e40cc7f
  (fn pascal-line [n]
    (let [inner-line (fn inner-line [s]
                       (if (empty? (rest s))
                         '(1)
                         (cons (+ (first s) (second s)) (inner-line (rest s)))))
          genline       (fn [s]
                          (cons 1 (inner-line s)))]
      (if (= n 1)
        (list 1)
        (genline (pascal-line (dec n)))))))

(defcheck solution-3eab2b8a
  (fn pascal-triangle [x]
    (letfn [(make-row-until [x] (->> (repeat 1) (take x) vec))
            (sum-adiacents [xs]
              (loop [acc []
                     [head & tail] xs]
                (if (nil? tail) acc
                                (recur (conj acc (+ head (first tail))) tail))))]

      (loop [acc (make-row-until 2)]
        (if (= x (count acc))
          acc
          (if (< x 3)
            (make-row-until x)
            (recur ((comp vec flatten) [1 (sum-adiacents acc) 1]))))))))

(defcheck solution-3ed54c3c
  (fn [n]
    (let [n (dec n)
          hf (fn [prev k result]
               (if (> k n)
                 result
                 (let [curr (* prev (/ (- (inc n) k) k))]
                   (recur curr (inc k) (conj result curr)))))]
      (hf 1 1 [1]))))

(defcheck solution-3f18deb5
  (fn [n]
    (loop [counter 1 result [1]]
      (if (= counter n)
        result
        (recur
          (inc counter)
          (let [row (conj (apply conj [0] result) 0)]
            (for [i (range 1 (count row))]
              (+ (get row i) (get row (dec i))))))))))

(defcheck solution-3fbd4243
  (letfn [(pasc-t [rs]
            (let [rsum (map (partial apply +) (partition 2 1 rs))]
              (lazy-seq (cons rs (pasc-t (conj (into [1] rsum) 1))))))]
    (fn [n] (first (drop (dec n) (pasc-t [1]))))))

(defcheck solution-3fc1c09e
  (fn [n]
    (last
      (take n
        (iterate
          #(concat [1] (map + % (rest %)) [1])
          '(1))))))

(defcheck solution-4074af14
  (fn m [n]
    (last (take n
            (iterate
              (fn [prev-row]
                (->>
                  (concat [[(first prev-row)]] (partition 2 1 prev-row) [[(last prev-row)]])
                  (map (partial apply +))))
              [1])))))

(defcheck solution-41028661
  (fn [n]
    (letfn [(lazy-pascal
              ([]
               (concat '([1] [1 1]) (lazy-pascal [1 1])))
              ([prev]
               (let [now (concat [1] (map (partial apply +) (partition 2 1 prev)) [1])]
                 (lazy-seq
                   (cons now (lazy-pascal now))))))]
      (nth (lazy-pascal) (dec n)))))

(defcheck solution-419672b4
  (fn pascal [n]
    (first (take-last 1 (take n ((fn rpascal [prev level currentindex]
                                   (lazy-seq (cons prev (rpascal
                                                          (into [] (flatten (list 1 (loop [i 0 acc '()]
                                                                                      (if (= i (dec (dec level)))
                                                                                        acc
                                                                                        (recur (inc i)
                                                                                          (cons (+ (get prev i) (get prev (inc i))) acc)))) 1)))
                                                          (inc level) (+ currentindex level))))) [1] 2 1))))))

(defcheck solution-42028b
  (fn pascal
    [n]
    (condp = n
      1 [1]
      2 [1 1]
      3 [1 2 1]
      ((fn [x] (->> (partition 2 1 (pascal (- x 1)))
                 (map #(apply + %))
                 (#(conj % 1))
                 (vector)
                 (#(conj % 1))
                 (flatten)
                 (into []))) n)
      )))

(defcheck solution-420c2b4
  (fn f [x]
    (cond
      (= x 1) [1]
      (= x 2) [1 1]
      :else
      (concat
       [1]
       (map
         #(apply + %)
         (partition 2 1 (f (- x 1))))
       [1]
       )
      )
    ))

(defcheck solution-4223b6f4
  (fn [n]
    (loop [r [1] i n]
      (if (== 1 i)
        r
        (recur
          (conj (into [1] (map #(+ %1 %2) r (rest r))) 1)
          (dec i))
        )
      )
    ))

(defcheck solution-4225e9d
  (fn [n] (nth (iterate #(cons 1 (concat (map (fn [ts] (apply + ts)) (partition 2 1 %)) '(1))) [1] ) (dec n))))

(defcheck solution-42c41e5b
  (fn p [x]
    (if (= x 1)
      [1]
      `[1 ~@(map + (p (- x 1)) (next (p (- x 1)))) 1])))

(defcheck solution-42cf5d33
  (comp (partial nth (iterate #(map + (concat % [0]) (cons 0 %)) [1])) dec))

(defcheck solution-42d3020c
  (fn pascal[n]
    (if (= n 1) [1]
                (loop [ cur 2 res [1 1] ]
                  (if (= cur n) res
                                (let [ parts  (partition 2 1 res)
                                      middle (map #(apply + %) parts) ]
                                  (recur (inc cur) (concat [1] middle [1]))))))))

(defcheck solution-42d34c38
  (fn [n]
    (-> (iterate (fn [row]
                   (map + `(0 ~@row) `(~@row 0)))
          [1])
      (nth (dec n)))))

(defcheck solution-43582d7a
  (fn [cn]
    (let [nrow
          (fn [r]
            (cond
              (= r []) [1]
              (= r [1]) [1 1]
              :else (loop [row r acc []]
                      (if (empty? (rest row)) (concat [1] acc [1])
                                              (recur (rest row)
                                                (conj acc
                                                  (+ (first row)
                                                     (first (rest row)))))))))]
      (peek (reduce (fn [a _]
                      (conj a (nrow (peek a)))) [[1]] (range (dec cn)))))))

(defcheck solution-438af76a
  (fn pascal [n]
    (if (= n 1)
      [1]
      (conj
        (->> (pascal (- n 1))
          (partition 2 1)
          (into [1])
          (map #(if (seq? %) (apply + %) %))
          (apply vector)
          )
        1))))

(defcheck solution-4392350e
  (fn p [n]
    (if (> n 1)
      (conj (into [1] (map #(apply + %) (partition 2 1 (p (dec n))))) 1)
      [1])))

(defcheck solution-4432f5b1
  (fn [n]
    (nth
      (iterate #(mapv + (cons 0 %) (conj % 0)) [1])
      (dec n))))

(defcheck solution-44c9cc98
  (fn pascal-triangle [n]
    (map
      #(/ (apply * (take % (iterate dec (dec n))))
         (apply * (take % (iterate dec %))))
      (take n (iterate inc 0)))))

(defcheck solution-44f8813c
  (fn [n]
    (loop [n n
           r [1]]
      (if (= 1 n)
        r
        (recur (dec n)
          (concat [1]
                  (loop [s []
                         h (first r)
                         t (rest r)]
                    (if (empty? t)
                      s
                      (recur (conj s (+ h (first t)))
                        (first t)
                        (rest t))))
                  [1]))))))

(defcheck solution-44fcd570
  (fn [n]
    (loop [stp 1
           row [1]]
      (if (<= n stp)
        row
        (recur (inc stp)
          (cons 1
            (conj (mapv #(reduce + %)
                    (partition 2 1 row))
              1)))))))

(defcheck solution-44fd1880
  (fn [n]
    (let [fac
          (fn [n] (reduce * (range 1 (inc n))))
          nCr
          (fn [n r] (/ (fac n)
                      (* (fac (- n r))
                        (fac r))))]
      (map (partial nCr (dec n)) (range 0 n)))))

(defcheck solution-451f86da
  #(let [n (- % 1)] (for [i (range (+ n 1))]


                      (letfn [ (fact [x]
                                 (loop [n x f 1]
                                   (if (= n 0)
                                     f
                                     (recur (dec n) (* f n)))))
                              (comb [n r] (/ (fact n) (* (fact r) (fact (- n r)))) ) ] (comb n i))

                      )))

(defcheck solution-454c85f5
  (fn pascal [n]
    (case n
      1 [1]
      2 [1 1]
      (concat [1]
              (map #(apply + %) (partition 2 1 (pascal (dec n))))
              [1]))))

(defcheck solution-455a191a
  (fn [n] (nth
            (iterate #(concat [1] (map (partial reduce +) (partition 2 1 %)) [1])
              [1])
            (dec n))))

(defcheck solution-4566772d
  (fn __ [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (loop [cur_n 2 v (vector 1 1)]
        (if (= cur_n n)
          v
          (recur (inc cur_n)
            (into [1]
              (into
                (vec (map-indexed #(+ %2 (v (inc %))) (butlast v)))
                [1]))))))))

(defcheck solution-45760e44
  (fn [n] (letfn [(next-pascal-row [row] (concat [1] (map (fn [pair] (apply + pair)) (partition 2 1 row)) [1])) (pascal-row [n] (if (= n 1) [1] (next-pascal-row (pascal-row (- n 1)))))] (pascal-row n))))

(defcheck solution-45f09165
  (fn [x]
    (letfn [(fac [x]
              (if (= 0 x) 1 (* x (fac (dec x)))))
            (combo [n r]
              (/ (fac n) (* (fac r) (fac (- n r)))))]
      (map #(combo (dec x) %) (range x)))))

(defcheck solution-45fd38e7
  (fn [i] (nth (iterate #(concat [1] (map + % (rest %)) [1]) [1]) (dec i))))

(defcheck solution-462cd511
  (fn pascal [n]
    (if (= 1 n) [1]
                (let [prev (pascal (dec n))]
                  (loop [ r [1] f (first prev) s (first (rest prev)) l (drop 2 prev)]
                    (if (not s) (conj r 1)
                                (recur (conj r (+ f s)) s (first l) (rest l))
                                ))
                  ))
    ))

(defcheck solution-46429a4e
  (fn [n]
    (nth
      (iterate
        #(map + `[0 ~@%] `[~@% 0])
        '(1))
      (dec n))))

(defcheck solution-4661561c
  (fn [n]
    (first
      (drop (dec n)
        (iterate #(map +
                    (concat [0] %)
                    (concat % [0]))
          [1])))))

(defcheck solution-466ab331
  (fn f
    ([n] (f (dec n) [1]))
    ([n x] (if (zero? n) (vec x)
                         (recur (dec n)
                           (conj
                             (map + x (rest (conj (vec x) 0)))
                             (first x)))))
    ))

(defcheck solution-473dac10
  (letfn[(p[i](if(= i 1)[1](concat[1](map(fn[[a b]](+ a b))(partition 2 1(p(dec i))))[1])))]p))

(defcheck solution-47bae3ef
  (fn pascal [n]
    ( cond
      (= n 1) [1]
      :else (concat (cons 1 (map (partial apply +) (partition 2 1 (pascal (dec n))))) [1]))))

(defcheck solution-47e536b0
  (fn [n]
    (nth
      (iterate #(map + `(0 ~@%) `(~@% 0)) [1])
      (dec n))))

(defcheck solution-480aca59
  (fn pascal [n]
    (if (= n 1) [1]
                (vec
                  (cons 1
                    (conj
                      (vec
                        (map #(apply + %)
                          (partition 2 1 (pascal (dec n)))))
                      1))))))

(defcheck solution-4861774f
  (fn pascal
    [n]
    (loop [previous [1]
           n n]
      (if (= 1 n)
        previous
        (recur (->> (partition 2 1 previous)
                 (map #(+ (first %) (second %)))
                 (conj [1])
                 (reverse)
                 (conj [1])
                 (flatten)) (dec n))))))

(defcheck solution-4867097a
  (fn [n]
    (loop [current-depth 1 numbers [1]]
      (if (>= current-depth n)
        numbers
        (recur (inc current-depth)
          (map #(apply + %) (partition 2 1 (lazy-cat [0] numbers [0]))))))))

(defcheck solution-489e1d33
  (fn pascal-triangle-row [n]
    (let
     [next-row (fn [row]
                 (->> (partition 2 1 row)
                   (map (partial apply +))
                   (#(concat [1] % [1]))))
      pascal-triangle (fn []
                        (cons '(1) (iterate next-row '(1 1))))]
      (->> (pascal-triangle) (take n) last))))

(defcheck solution-490bfa63
  (fn [n]
    (loop [row nil
           c n]
      (if (zero? c)
        row
        (recur (loop [r row
                      acc [1]]
                 (if (nil? r)
                   acc
                   (let [f (first r)
                         s (second r)
                         tf (if (nil? f) 0 f)
                         ts (if (nil? s) 0 s)]
                     (recur (next r) (conj acc (+ tf ts))))))
          (dec c))))))

(defcheck solution-491c2133
  (fn [n]
    (letfn
     [(step [prev]
        (let [A (conj prev 0),
              B (cons 0 prev)]
          (into [] (map + A B))))]

      (loop [m 1, result [1]]
        (if (= m n)
          result
          (recur (inc m) (step result)))))))

(defcheck solution-492ca808
  (fn pt [n] (condp = n 1 [1] 2 [1 1] (reduce #(if (sequential? %1) (conj (vec (butlast %1)) (+ (last %1) %2) %2) [1 (+ %1 %2) %2]) (pt (dec n))))))

(defcheck solution-498c6412
  (fn [n]
    (let [row (- n 1)]
      (letfn [(factorial [x] (reduce *  (range 1 (+ 1 x))))]
        (map #(/ (factorial row) (* (factorial %) (factorial (- row %)))) (range 0 (+ 1 row)))))))

(defcheck solution-49d78827
  (fn [n]
    (nth
      (iterate (fn [x] (concat [1] (map + x (next x)) [1]))
        [1])
      (dec n) )))

(defcheck solution-4a44258e
  (fn [n]
    (nth (iterate
           #(vec (map + (cons 0 %) (conj % 0))) [1])
      (dec n))))

(defcheck solution-4a85477b
  (fn [n]
    (let [nextrow (fn [x] (map (partial apply +) (partition 2 1 (concat [0] x [0]))))]
      (nth (iterate nextrow [1]) (dec n)))))

(defcheck solution-4af62ee
  (fn [n]
    (let [pt
          (iterate
            (fn [r]
              (map #(apply + %)
                (partition 2 1 (concat [0] r [0]))))
            [1])]
      (nth pt (dec n)))))

(defcheck solution-4b0d36b
  (fn pascal-row [n]
    (if (= n 1) [1]
                (map #(apply + %) (partition 2 1 (concat [0] (pascal-row (dec n)) [0]))))))

(defcheck solution-4b41fcbc
  (fn pascal
    ([row]
     (map (partial pascal row) (range 1 (inc row))))
    ([row col]
     (if (or (= col 1) (= col row) (= row 1))
       1
       (+ (pascal (dec row) (dec col)) (pascal (dec row) col))))))

(defcheck solution-4bee1a6c
  (fn pascal [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (let [p1 (partition 2 1 (pascal (- n 1)))
            p2 (map #(apply + %) p1)]
        (concat [1] p2 [1])))))

(defcheck solution-4c25892d
  (fn [n]
    (nth
      (iterate
        (fn [x]
          (into
            (into [1] (map + (butlast x) (rest x)))
            [1]))
        [1])
      (dec n))))

(defcheck solution-4c384b07
  (fn f [l]
    (letfn
     [(nr [r] (map #(apply + %) (partition 2 1 r)))]
      (if (= l 1)
        [1]
        (concat [1] (nr (f (dec l))) [1])
        )
      )))

(defcheck solution-4c49d910
  #(nth (iterate (fn [coll] (mapv + (cons 0 coll) (conj coll 0))) [1]) (dec %)))

(defcheck solution-4cb1650c
  (fn [row]
    (map
      first
      (take row
        (iterate
          (fn [[v c]] [(* v (/ (- row c) c)) (inc c)])
          [1 1])))))

(defcheck solution-4d1dec80
  (fn pasc [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (vec (concat [1]
                   (let [p (pasc (dec n))]
                     (for [i (range (dec (count p)))]
                       (+ (p i) (p (inc i))))) [1])))))

(defcheck solution-4d3cf6a6
  (fn [n] (nth(iterate (fn [sq] (vec (map + (cons 0 sq) (conj sq 0)))) [1])  (dec n))))

(defcheck solution-4d55e17a
  #(nth (iterate (fn [r] (concat [1] (map + (rest r) (butlast r)) [1])) [1]) (dec %)))

(defcheck solution-4d96358f
  #(loop [[fst & rst] (map / (reverse (range 1 %)) (range 1 %))
          result [1]]
     (if fst
       (let [res (conj result (int (* (last result) fst)))]
         (if rst
           (recur rst res)
           res))
       result)))

(defcheck solution-4da132ce
  (fn [n] (nth (iterate (comp #(concat [1] % [1])
                              (partial map #(apply + %))
                              #(partition 2 1 %))
                 [1])
            (dec n))))

(defcheck solution-4de66727
  (fn pascal-triangle [n]
    (nth
      (iterate
        (fn [nums]
          (vec
            (map + (conj nums 0) (cons 0 nums)))) [1]) (dec n))))

(defcheck solution-4de9a873
  (fn pascal-triangle [n]
    {:pre [(>= n 1)]}
    (loop [m 1
           r [1]]
      (if (= m n)
        r
        (recur (inc m)
          (into [] (map + (cons 0 r) (conj r 0)))))
      )))

(defcheck solution-4e415d1d
  (fn [n]
    (rest (butlast
            (reduce
              (fn [r _]
                (cons 0 (concat (map #(+ (first %) (second %)) (partition 2 1 r)) [0])))
              [0 1 0]
              (range 1 n))))))

(defcheck solution-4e6eecd4
  (fn nth-row [n]
    (nth (iterate #(concat [1] (map + % (rest %)) [1]) [1]) (dec n))))

(defcheck solution-4e88b458
  (fn pascal [n]
    (if (= n 1) [1]
                (let [prev (pascal (dec n))]
                  (mapv + (concat [0] prev) (conj prev 0))))))

(defcheck solution-4ec2ed26
  (fn [n]
    (loop [i n
           res '(1)]
      (if (<= i 1)
        (vec res)
        (recur (dec i)  (concat '(1) (map #(apply + %) (partition 2 1 res)) '(1)))))))

(defcheck solution-4f0be681
  (fn [n]
    (letfn [(fact [n k]
              (loop [i n r 1]
                (if (= i k)
                  r
                  (recur (dec i) (* i r)))))
            (choose [n m]
              (let [n (fact n (- n m))
                    d (fact m 0)]
                (/ n d)))]
      (->>  n
        (range)
        (map (partial choose (dec n)))))))

(defcheck solution-4f468d1d
  #(nth (iterate (fn [l] (concat [1] (map + l (rest l)) [1])) [1]) (dec %)))

(defcheck solution-4fc25c87
  (fn p [n] (if (= 1 n) [1] (let [r (p (dec n))] (map + (cons 0 r) (concat r [0]))))))

(defcheck solution-501a4156
  (fn f [n]
    (if (= n 1) [1]
                (let [b (concat [0] (f (- n 1)) [0])]
                  (map + b (rest b))))))

(defcheck solution-50330273
  (fn [n] (reduce #(conj %1 (* (peek %1) (/ (- n %2) %2))) [1] (range 1 n))))

(defcheck solution-50741425
  (fn [n]
    (reduce #(vec (for [i (range (+ 1 %2))]
                    ((fn [k v]
                       (if (or (= k 0) (= k (count v)))
                         1
                         (+ (nth v (dec i)) (nth v i))))
                     i %)))
      [1]
      (range n))))

(defcheck solution-50a09ea9
  (fn pasc [n]
    (let [p (fn [v]
              (vec (concat
                    (vector (first v))
                    (mapv + v (conj (vec (drop 1 v)) 0)))))]
      (loop [row [1]
             i 1]
        (if (= i n) row
                    (recur (p row)
                      (inc i)))))))

(defcheck solution-50cbe36e
  (fn [n]
    (loop [n n
           r [1]]
      (if (= n 1)
        r
        (let [els (map #(reduce + %) (partition 2 1 r))]
          (recur (dec n) (concat (list 1) els (list 1))))))))

(defcheck solution-5146871
  (fn [w] (nth (iterate (fn [z] ((fn [x] (map #(+ (first %) (last %)) x)) ((fn [y] (map list (conj y 0) (conj (vec y) 0))) z))) '(1)) (dec w))))

(defcheck solution-517dd93a
  (fn [target-row]
    (loop [n 1
           prev []]
      (if (> n target-row)
        prev
        (recur
          (inc n)
          (into [] (map
                     #(if (or (= 0 %) (= n (inc %)))
                        1
                        (+ (prev %) (prev (dec %))))
                     (range n))))))))

(defcheck solution-51d2ef74
  (fn tria [n]
    (if (= n 1) [1]
                (conj (vec
                        (cons 1 (map #(reduce + %1)
                                  (partition 2 1 (tria (dec n)))
                                  ))) 1))))

(defcheck solution-524d876c
  (fn f[n]
    (if (= n 1)
      [1]
      (vec (map + (conj (f (dec n)) 0) (cons 0 (f (dec n))))))))

(defcheck solution-52aaf4c1
  (fn* triag [n]
    (if (= 1 n)
      [1]
      (loop [row (concat [0] (triag (dec n)) [0]) result []]
        (if (empty? (rest row))
          result
          (recur (rest row) (conj result (+ (first row) (second row)))))))))

(defcheck solution-52feee2e
  #(loop [i 1 row '(1)]
     (if (< i %)
       (recur (inc i) (cons (* (first row) (/ (- % i) i)) row))
       row)))

(defcheck solution-53572276
  (fn pascal [n]
    (if (= 1 n) [1]
                (let [previous (pascal (dec n))]
                  (vec (map + (concat previous [0])
                         (concat [0] previous)))))))

(defcheck solution-53a9bdb8
  #(reduce (fn [c _]
             (concat [1] (vec (map (partial apply +) (partition 2 1 c))) [1]))
     [1] (range (dec %))))

(defcheck solution-53b06fcc
  (fn pasTri [n]
    (lazy-seq (cond
                (= 1 n) [1]
                (= 2 n) [1 1]
                :else (into [1]
                        (into (vec
                                (map (fn[[x y]] (+ x y))
                                  (partition 2 1 (pasTri (- n 1)))))
                          [1]))))))

(defcheck solution-53f15f9b
  (fn [n]
    (let [row (fn [v]
                (let [inside (map (partial apply +) (partition 2 1 v))]
                  (vec (concat [1] inside [1]))))]
      (->> [1]
        (iterate row)
        (take n)
        last))))

(defcheck solution-53fab593
  (fn triangle [n]
    (loop [res [1]
           cnt 1]
      (if (= n cnt)
        res
        (recur
          (let [zipped (map vector res (rest res))]
            (concat [1] (map #(+ (first %) (second %)) zipped) [1]))
          (inc cnt))))))

(defcheck solution-543f21f6
  (fn [n]
    (case n
      1 [1]
      2 [1 1]
      (nth (iterate #(concat [1] (map (partial reduce +) (partition 2 1 %)) [1]) [1 1]) (- n 2)))))

(defcheck solution-54aff967
  (fn pascal3 [n]
    (nth ((fn pascal [coll]
            (concat [coll] (lazy-seq (pascal (concat [(first coll)] (map (partial apply +) (partition 2 1 coll)) [(last coll)]))))
            ) [1]) (dec n))
    ))

(defcheck solution-54d44eb9
  (fn f [n]
    (if (= n 1) [1]
                (let [l (f (dec n))]
                  (concat [1] (map + l (next l)) [1])))))

(defcheck solution-54fdab
  #(last (take % (iterate (fn [s] (map + (cons 0 s) (conj (vec s) 0))) [1]))))

(defcheck solution-55758c09
  (fn p [r]
    (cond (= r 1) [1]
          (= r 2) [1 1]
          :else (let [u (p (dec r))]
                  (vec (concat [1]
                               (for [x (range (dec (count u)))]
                                 (apply + (subvec u x (+ x 2)))) [1]))))))

(defcheck solution-560d082f
  (fn pt_row [n]
    (if (= n 1)
      [1]
      (concat [1]
              (map (partial apply +) (partition 2 1 (pt_row (- n 1))))
              [1]))))

(defcheck solution-56f434e9
  (fn [n]
    (let [r #(range % (inc %2))
          f #(apply * (r % %2))
          b #(/ (f (- % (dec %2)) %) (f 1 %2))]
      (map #(b (dec n) (dec %)) (r 1 n)))))

(defcheck solution-571a909a
  (fn [a] (if (= a 1) [1] ((fn pascal [n] (if (= n 2) [1 1] (concat [1] (map #(apply + %)
                                                                          (map #(take 2 (drop % (pascal (- n 1)))) (range (- n 2)))) [1] ))) a))))

(defcheck solution-574b1b33
  (fn f [n]
    (if (= 1 n) [1]
                (let [t (f (dec n))]
                  (concat [1] (map + t (rest t)) [1])))))

(defcheck solution-57cd6e54
  (fn pascal [n]
    (if (= n 1)
      '(1)
      (concat [1]
              (map #(apply + %) (partition 2 1 (pascal (dec n))))
              [1]))))

(defcheck solution-583971e3
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (conj (vec (cons 1 (map #(apply + %)
                           (partition 2
                             1
                             (pascal (- n 1))))))
        1))))

(defcheck solution-5867043d
  (fn prob97 [row]
    (last (take row
            (iterate #(concat [1]
                              (map + % (rest %))
                              [1])

              [1])
            ))))

(defcheck solution-5877b48e
  (fn Y [x] (cond
              (= x 1) [1]
              (= x 2) [1 1]
              :else (concat [1] (map #(+ (first %) (second %)) (partition 2 1 (Y (dec x)))) [1]))))

(defcheck solution-588a8978
  (fn [y]
    (cond
      (= y 1) [1]

      :else

      (last (take (dec y)
              (iterate
                (fn
                  [x]
                  (conj
                    (loop [i 0 z [] ]
                      (if
                       (< i (count x))
                        (recur
                          (inc i)
                          (conj
                            z
                            (+
                             (get x (dec i) 0)
                             (get x i )
                             )
                            )
                          )
                        z
                        )
                      )
                    1
                    )
                  )
                [1 1]
                ))
        ))))

(defcheck solution-58d1c4d9
  (fn [i]
    (last (take i
            (iterate
              (fn [pr]
                (->>
                  (concat [[(first pr)]] (partition 2 1 pr) [[(last pr)]])
                  (map (partial apply +) ,,,)))
              [1])))))

(defcheck solution-58f888a5
  #(letfn [(! [n] (reduce * (range 1 (inc n))))
           (choose [n k] (/ (! n) (! k) (! (- n k))))]
     (map (partial choose (dec %)) (range %))))

(defcheck solution-592445d5
  #(nth (iterate (fn [v] (into [] (map + (cons 0 v) (conj v 0)))) [1]) (- % 1)))

(defcheck solution-59cf13e9
  (fn [n]
    (loop [i [1] n n]
      (if (pos? (dec n))
        (recur (map #(apply + %) (partition 2 (concat '(0) (interleave i i) '(0)))) (dec n))
        i))))

(defcheck solution-5a354ada
  (fn [n] (let [g
                (fn [v result]
                  (if (<= (count v) 1)
                    result
                    (recur (next v) (conj result (+ (first v) (fnext v))))
                    )
                  )]
            (loop [prev [1] curr 2]
              (if (> curr n)
                prev
                (recur (flatten (vector 1 (g prev []) 1)) (inc curr))
                )
              )
            )
    ))

(defcheck solution-5a463dd0
  (fn [n] (nth (iterate (fn [pas] (vec (cons 1 (conj (mapv (partial apply +) (partition 2 1 pas)) 1)))) [1]) (dec n))))

(defcheck solution-5a4bd830
  (fn [y]
    (last
      (take y
        (iterate
          (fn [x]
            (concat [1]
                    (map #(reduce + %)(partition 2 1 x))
                    [1])
            )
          '(1)
          )
        )
      )
    ))

(defcheck solution-5a5c0a8a
  (fn [n] (nth (iterate (fn [r] (map (partial reduce +) (partition 2 1 (concat [0] r [0])))) [1]) (dec n))))

(defcheck solution-5a8908da
  (fn [n]
    (let [pas (fn [col] (let [z (vec (map #(apply + %) (partition 2 1 col)))] (cons 1 (conj z 1))))]
      (nth (iterate pas [1]) (dec n)))))

(defcheck solution-5a99e514
  (fn nth-try [n]
    (letfn [(next-tri [coll]
              (conj (into [1]
                      (map (comp (partial apply +) vector)  coll (rest coll)))
                1))]
      (nth (iterate next-tri [1]) (dec n)))))

(defcheck solution-5add4a3a
  (fn pa [n]
    (if (== n 1) [1]
                 (let [prev (pa (dec n))]
                   (concat (vector (first prev))
                           (map #(reduce + %) (partition 2 1 prev))
                           (vector (first prev)))))))

(defcheck solution-5b05a390
  (fn [n] (nth (iterate (fn [s] (map #(apply + %)
                                  (partition 2
                                    (#(interleave (concat % [0]) (concat [0] %)) s))))
                 [1])
            (dec n))))

(defcheck solution-5c1fb067
  (fn [x]
    (loop [n x p [1]]
      (if (= 1 n) p
                  (recur
                    (- n 1)
                    (concat
                     [1]
                     (map #(apply + %) (partition 2 1 p))
                     [1]))))))

(defcheck solution-5c9d6da5
  (fn pascal [n]
    (loop [row [1]
           k 1]
      (if (= n k)
        row
        (let [prev (last row)
              next (quot (* prev (- n k)) k)]
          (recur (conj row next) (inc k)))))))

(defcheck solution-5ce73743
  (fn aaa
    ([n] (aaa n [1]))
    ([n v]
     (if (= n 1)
       v
       (aaa (dec n) (concat '(1) (map (fn [t] (apply + t)) (partition 2 (rest (interleave v v)))) '(1)))))))

(defcheck solution-5cec3db4
  (fn pascal [n]
    (if (= n 1)
      [1]
      (concat [1] (map #(apply + %) (partition 2 1 (pascal (dec n)))) [1]))))

(defcheck solution-5d49d7f2
  (fn [n]
    (loop [i 1 r [1]]
      (if (= i n) r
                  (recur (inc i) (vec (map + (concat [0] r) (concat r [0]))))))
    ))

(defcheck solution-5d555b85
  (fn [n]
    (nth (iterate #(concat [1] (map + % (next %)) [1]) [1]) (dec n))))

(defcheck solution-5d6cae43
  (fn [n]
    (let [n (dec n)]
      (letfn [(! [x] (->> x inc (range 1) (reduce *)))
              (c [r] (/ (! n) (* (! r) (! (- n r)))))]
        (->> n inc range (map c))))))

(defcheck solution-5da73f52
  (fn pascal [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (concat [1] (map #(apply + %) (partition 2 1 (pascal (dec n))))  [1]))))

(defcheck solution-5ddc64da
  (fn [n] (nth (iterate #(map + (reverse (conj % 0)) (conj % 0)) [1]) (dec n))))

(defcheck solution-5df3813b
  (fn p [n]
    (if (= 1 n)
      [1]
      (concat [1]
              (map
                +
                (p (- n 1))
                (rest (p (- n 1))))
              [1]))))

(defcheck solution-5e0f0a05
  (fn peu [x]
    (if (= x 1) '(1)
                (let [y (peu (dec x))]
                  (let [
                        z (concat y '(0))
                        v (concat '(0) y)]
                    (map #(+ %1 %2) z v)
                    ))
                )
    ))

(defcheck solution-5e80a034
  (fn [x] (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) [1]) (dec x))))

(defcheck solution-5e82d2dd
  #(letfn [(f [n]
             (apply * (rest (range (inc n)))))
           (c [n k]
             (/ (f n)
               (f k)
               (f (- n k))))]
     (map (fn [k] (c (dec %) k))
       (range %))))

(defcheck solution-5e854393
  (fn [n]
    (nth (iterate #(concat [1] (map + % (drop 1 %)) [1]) [1]) (dec n))))

(defcheck solution-5e8e4ab3
  (fn f
    [n]
    (if (= n 1)
      [1]
      (concat [1] (map (partial apply +) (partition 2 1 (f (dec n)))) [1]))))

(defcheck solution-5e9b9460
  (fn [x]
    (cond
      (= x 1) [1]
      (= x 2) [1 1]
      :else (loop [r 2
                   y [1 1]]
              (if (= r x)
                y
                (recur (inc r)
                  (flatten
                    [1 (map + y (rest y)) 1])))))))

(defcheck solution-5ecf9942
  (fn [n]
    (nth (iterate #(vec (map + (conj % 0) (cons 0 %)
                          )
                     ) '[1]
           )
      (dec n)
      )
    ))

(defcheck solution-5f70bd14
  (fn [n]
    (loop [row [1]
           i    1]
      (if (= i n) row
                  (recur (concat [1] (map (partial apply +) (partition 2 (interleave row (rest row)))) [1])
                    (inc i))))))

(defcheck solution-5f798ffb
  (fn [lv]
    (letfn [(f [a] (apply * (range 1 a)))]
      (for [x (range 1 (inc lv))
            :let [y (/ (f lv)
                      (* (f x)
                        (f (inc (- lv x)))))]]
        y))))

(defcheck solution-5f79ada1
  (fn[a](nth (iterate #(map + `(0 ~@%) `(~@% 0)) [1])(dec a))))

(defcheck solution-5f7fdfbf
  (fn [n]
    (nth (iterate (fn [row]
                    (map + (concat [0] row) (concat row [0])))
           [1])
      (dec n))))

(defcheck solution-5f83a961
  (fn [n]
    (loop [k 1  rv '(1) n (dec n)]
      (if (<= k n)
        (recur (inc k) (cons (* (first rv) (/ (+ n 1 (- k)) k)) rv) n)
        rv))))

(defcheck solution-6059854f
  (fn [n]
    (letfn [(p-triangle [coll]
              (lazy-seq (cons coll (p-triangle (concat [1] (map (partial reduce +) (partition 2 1 coll)) [1])))))]
      (nth (p-triangle [1]) (dec n)))))

(defcheck solution-60c8a65e
  (fn pasrow [n]
    (if (= n 1)
      [1]
      (into [1]
        (conj (mapv (partial reduce +)
                (partition 2 1 (pasrow (dec n))))
          1)))))

(defcheck solution-60fe8f45
  (fn pascalsTriangle [x]
    (let
     [recursor
      (fn recurs [out in]
        (if (< (count in) 2) (conj out 1) (let [firstElem (first in), secondElem (second in)] (recurs (conj out (+ firstElem secondElem)) (rest in))))
        )
      pascals
      (fn pasc [n] (if (<= n 1) [1] (if (= 2 n) [1 1] (recursor [1] (pasc (- n 1))))))
      ]
      (pascals x)
      )
    ))

(defcheck solution-6168e739
  (let [pascal
        (fn pascal [row col]
          (if (or (= col 0) (= col row))
            1
            (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))]
    (fn [r] (map (partial pascal (dec r)) (range 0 r)))))

(defcheck solution-616d7e50
  (fn [n]
    (nth
      (iterate #(concat [1] (map + % (drop 1 %)) [1]) [1])
      (dec n))))

(defcheck solution-618020bd
  (fn [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :default (loop [x 2, res [1 1]]
                 (if (= n x)
                   res
                   (recur (inc x) (vec (map + (cons 0 res) (conj res 0)))))))))

(defcheck solution-61b22c4b
  (fn pascal [n]
    (letfn [(sum-pairs [pairs] (map #(+ (first %) (second %)) pairs))
            (next-row [row]
              (if (= [1] row)
                [1 1]
                (concat [1] (sum-pairs (partition 2 1 row)) [1])))]
      (loop [counter n current-row [1]]
        (if (<= counter 1)
          (vec current-row)
          (recur (dec counter) (next-row current-row)))))))

(defcheck solution-61b742ca
  (fn pascals-triangle [n]
    (if (= n 1)
      [1]
      (concat
       [1]
       (map #(apply + %)
         (partition 2 1 (pascals-triangle (dec n))))
       [1]))))

(defcheck solution-61cb54ae
  (fn [n]
    (loop [i 1 result '(1)]
      (if (= i n) result
                  (recur (inc i)
                    (flatten (list 1 (map + result (drop 1 result)) 1)))))))

(defcheck solution-61d47dff
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [old-row (pascal (dec n))
            row (mapv (partial apply +)
                  (partition 2 1 old-row))]
        (vec (list* 1 (conj row 1)))))))

(defcheck solution-61ef42b
  (fn pascal [i]
    (if (= i 1)
      [1]
      (vec (map + (concat [0] (pascal (dec i))) (concat (pascal (dec i)) [0])))
      )
    ))

(defcheck solution-61f1d293
  (fn [n]
    (reduce (fn [v j] (vec (map #(+ %1 %2) (conj v 0) (concat [0] v)))) [1] (range (dec n)))))

(defcheck solution-61f24b10
  (fn [n] (nth (iterate (fn it [v] (vec (map (partial apply +) (partition 2 1 (flatten [0 v 0]))))) [1]) (dec n) )))

(defcheck solution-62008fe4
  (fn pt [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (concat [1] (let [a (pt (- n 1))] (map + (drop 1 a) (drop-last a))) [1]))))

(defcheck solution-620c1733
  (fn [n] (nth (iterate #(map + (cons 0 %) (concat % [0])) [1])
            (dec n))))

(defcheck solution-62272f08
  (fn [n]
    (loop [n n
           r [1]]
      (if (= n 1)
        r
        (recur (dec n) (concat [1]
                               (map #(reduce + %) (partition 2 1 r))
                               [1]))))))

(defcheck solution-62a55ccc
  (letfn [(nCm [n m]
            (reduce (fn [s i] (* s (/ (- n (- m i)) i)))
              1 (range 1 (inc m))))]
    (fn [n] (map nCm (repeat (dec n)) (range n)))))

(defcheck solution-62ae19ba
  #(loop [r [1] n %]
     (if (= n 1) r
                 (recur (map + (concat [0] r) (concat r [0])) (dec n)))))

(defcheck solution-62e9a344
  (fn f [n] (if (= n 1)
              [1]
              (let [row (f (dec n))]
                (vec
                  (concat
                   '(1)
                   (map #(+ (nth row %) (nth row (dec %))) (range 1 (count row)))
                   '(1)))))))

(defcheck solution-62ee147a
  (fn [i]
    (let [step (fn [x]
                 (vec (map + (cons 0 x) (conj x 0))))
          path (fn path [x]
                 (lazy-seq
                   (cons x (path (step x)))))]
      (nth (path [1]) (dec i)))))

(defcheck solution-6354744c
  (fn [n] (loop [n n, rst [1]] (if (< n 2) rst (recur (dec n) (conj (into [1] (map #(+ (first %) (second %)) (partition 2 1 rst))) 1))))))

(defcheck solution-636f5f83
  #(nth (iterate (fn  [v]
                   (vec (cons 1 (map-indexed
                                  (fn [i x] (+ (get v i ) (get v (inc i) 0)))
                                  v)))) [])  %))

(defcheck solution-639ac0ed
  (fn pt [n] (last (take n
                     (iterate
                       #(let [row (conj (vec %) 0)] (map + row (conj (seq row) 0)))
                       [1])))))

(defcheck solution-63d09e56
  (fn [n] (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) [1]) (- n 1))))

(defcheck solution-6478b4b8
  (fn nth-pascal [n]
    (letfn [(iter [row]
              (map +
                (concat [0] row)
                (concat row [0])))]
      (nth (iterate iter [1]) (dec n)))))

(defcheck solution-6482bd22
  (fn [n]
    (reduce
      (fn [x y] (concat [1] (map (fn [a b] (apply + (vector a b))) x (rest x)) [1]))
      [1]
      (range 1 n))))

(defcheck solution-64b92980
  (fn [n] (nth (iterate #(mapv + (conj % 0)(into [0] %)) [1]) (dec n))))

(defcheck solution-6627bd7f
  (fn pascal-row [n]
    (let [n (dec n)]
      (reduce (fn [v k]
                (conj v (/ (* (get v k)
                             (- n k))
                          (inc k))))
        [1]
        (range n)))))

(defcheck solution-66420054
  #(first (drop (dec %) (take %
                          ((fn pasco [v]
                             (cons v
                               (lazy-seq (pasco (cons (first v) (conj (vec (map + v (rest v))) (last v)) ))))) [1])))))

(defcheck solution-66b4d816
  (fn pascal [n]
    (if (= n 1) [1]
                (loop [row [1 1]]
                  (if (= n (count row))
                    row
                    (recur (concat [1] (map + row (rest row)) [1])))))))

(defcheck solution-6746e893
  (fn pt [n]
    (if
     (= 1 n) [1]
             (concat [1] (map + (pt (dec n)) (rest (pt (dec n)))) [1]))))

(defcheck solution-675c532
  (fn tr
    ([n] (tr n [1]))
    ([n acc]
     (if (= n 1)
       acc
       (recur (dec n)
         (mapv + (cons 0 acc) (conj acc 0)))))))

(defcheck solution-6786eecc
  (fn pascal [n]
    (case n
      1 [1]
      (let [pp (concat [0] (pascal (dec n)) [0])]
        (map #(+ (nth pp (inc %)) (nth pp %))
          (range n))))))

(defcheck solution-678c96ff
  (fn [n]
    (loop [r [1] t n]
      (if (= 1 t)
        r
        (recur (map + (concat [0] r)
                 (concat r [0]))
          (dec t))))))

(defcheck solution-67ded21d
  (fn P [n]
    (if (= n 1)
      '(1)
      (let [l (P (dec n))]
        (map + (concat l [0]) (conj l 0))))))

(defcheck solution-67e12d2a
  (fn [n]
    (nth (iterate (fn [[f & _ :as s]] (concat [f] (map (partial apply +) (partition 2 1 s)) [f])) [1]) (dec n))))

(defcheck solution-68b1653b
  (fn [n]
    (loop [n n
           row [1]]
      (cond (= n 1) row
            :else (recur (dec n)
                    (into [] (concat [1]
                                     (map #(apply + %) (partition 2 1 row))
                                     [1])))))))

(defcheck solution-68c17a1a
  (fn [rows]
    (letfn [(gen-rows [r]
              (lazy-cat [r]
                (gen-rows (lazy-cat [1]
                            (map + r (rest r))
                            [1]))))]
      (last (take rows (gen-rows [1]))))))

(defcheck solution-68eae2d1
  #( get {1 [1] 2 [1 1] 3 [1 2 1] 4 [1 3 3 1] 5 [1 4 6 4 1] 11 [1 10 45 120 210 252 210 120 45 10 1]} %))

(defcheck solution-692b0658
  (fn it
    ([n] (it n [1]))
    ([n acc]
     (if (= n 1)
       acc
       (recur (dec n)
         (concat [1]
                 (map (partial apply +)
                   (partition 2 1 acc))
                 [1]))
       ))))

(defcheck solution-69413209
  (fn [x]
    (loop [tot [1] count 1]
      (if (= x count)
        tot
        (recur (flatten (conj [] (first tot) (map (fn [x y] (+ x y)) tot (rest tot)) 1)) (inc count))))))

(defcheck solution-698b027b
  (fn f [n]
    (if (= n 1) [1]
                (let [p (f (- n 1))
                      r (map + p (rest p))]
                  (concat [1] r [1])))))

(defcheck solution-699f17cc
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [p (pascal (- n 1))]
        (vec
          (concat
           [1]
           (map
             (fn [i]
               (+ (p i) (p (+ 1 i)))
               )
             (range (- (count p) 1)))
           [1]))))))

(defcheck solution-6a02bafa
  (fn pascals-nth [n]
    (if (= n 1) [1]
                (let [prior (pascals-nth (dec n))]
                  (mapcat
                    (comp vector +)
                    (concat [0] prior)
                    (concat prior [0]))))))

(defcheck solution-6a0e5746
  (fn pascal [n]
    (letfn [
            (get-value [xs i]
              (cond
                (zero? i) (xs i)
                (= (inc i) n) (xs (dec i))
                :else (+ (xs (dec i)) (xs i))))]

      (if (= n 1)
        [1]
        (let [xs (pascal (dec n))]
          (reduce
            (fn [acc i] (conj acc (get-value xs i)))
            [] (range n)))))))

(defcheck solution-6a4bbaa2
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [p (pascal (dec n))] (into [] (map + (conj p 0) (cons 0 p))))
      )
    ))

(defcheck solution-6b1f8428
  (fn pascal-triangle [level]
    (let [fact (fn [x] (apply * (range 1 (inc x))))
          P (fn [n r] (if (= r 0) 1 (/ (fact n) (fact (- n r)))))
          C (fn [n r] (if (= r 0) 1 (/ (P n r) (fact r))))]
      (map #(C (dec level) %) (range level)))))

(defcheck solution-6b452535
  #(nth (iterate
          (fn [xs]
            (map + (cons 0 xs) (conj (vec xs) 0))
            ) [1]) (dec %)))

(defcheck solution-6b97d5ce
  (fn pn [n](map #(



                   (fn lp [line pos]
                     (if (or  (= line 1) (= line 2) (= pos 1) (= pos line))
                       1
                       (let [previousline (dec line) ]
                         (+ (lp  previousline pos) (lp  previousline (dec pos)  ) )
                         )
                       )
                     )



                   n %)   (map inc  (range n)))))

(defcheck solution-6bc9e7eb
  (fn my-pascal
    [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (let [previous-row (my-pascal (- n 1))
            generate-next (fn [previous]
                            (loop [acc []
                                   cur (first previous)
                                   remaining (rest previous)]
                              (if (= (count remaining) 0)
                                acc
                                (recur
                                  (conj acc (+ cur (first remaining)))
                                  (first remaining)
                                  (rest remaining)))))]
        (flatten (vector 1 (generate-next previous-row) 1))))))

(defcheck solution-6c797564
  (fn Pascal-Triangle
    [x]
    (map (fn [a] (if (zero? a) 1 (/ (apply * (take a (reverse (range 1 x)))) (apply * (take a (range 1 x)))))) (range x))))

(defcheck solution-6cece543
  (letfn [
          (pascal [prev-row]
            (concat [1] (map (partial apply +) (partition 2 1 prev-row)) [1]))]

    (fn [k]
      (nth (iterate pascal [1]) (dec k)))))

(defcheck solution-6cfb105c
  (fn y [n]
    (letfn [ (p [r]
               (vec (map #(cond
                            (= 0 (first %)) 1
                            (= (count r) (first %)) 1
                            :else (+ (nth r (first %))
                                     (nth r (dec (first %)))))
                      (map-indexed vector (conj r 1)))))]
      (last (take n (iterate p [1]))))))

(defcheck solution-6d4c51b2
  (fn pascal [n]
    (if (= n 1)
      [1]
      (vec
        (map
          #(apply + %)
          (partition 2 1 (concat [0] (pascal (dec n)) [0])))))))

(defcheck solution-6dd8d074
  (fn pascals-row [n]
    (let [binom
          (fn binomial [n k]
            (cond
              (and (= k 0) (>= n 0)) 1
              (and (= n 0) (> k 0)) 0
              :else (+ (binomial (dec n) (dec k)) (binomial (dec n) k))))]
      (take-while #(not= 0 %) (map #(binom (dec n) %) (range))))))

(defcheck solution-6eb53350
  (fn [n] (nth (iterate #(concat [1] (map + % (rest %)) [1]) [1])
            (dec n)  )))

(defcheck solution-6ee1a79d
  (fn [n]
    (let [f (fn s [r i]
              (let [dr (dec r)]
                (if (or (== 0 i) (== dr i))
                  1
                  (+ (s dr (dec i)) (s dr i)))))]
      (map (partial f n) (range n)))))

(defcheck solution-6f7b7ef7
  (fn hossen[n]
    (if (= 1 n)
      [1]
      (nth (iterate
             (fn y[j](conj (apply vector (cons 1 ((fn x[s]
                                                    (when (next s)
                                                      (cons (+ (first s) (second s)) (x (next s))))) j))) 1))
             '()) (dec n))
      )
    ))

(defcheck solution-701b8782
  (fn pascal [n]
    (case n
      1 [1]
      (let [par (pascal (- n 1))
            len (- (count par) 1)]
        (vec (concat [1] (map #(+ (get par %) (get par (+ % 1))) (range len)) [1]))))))

(defcheck solution-7060f295
  (fn pascal [n]
    (reduce (fn [a k] (conj a (* (last a) (/ (- n k) k)))) [1] (range 1 n))))

(defcheck solution-716531be
  (fn pascals-triangle
    [n]
    (letfn [(! [n] (reduce * (range 1 (inc n))))
            (nCr [n r] (/ (! n) (* (! r) (! (- n r)))))]
      (map (partial nCr (dec n)) (range 0 n)))))

(defcheck solution-7191f797
  (fn pastri [n]
    (letfn [(fact [n]
              (reduce * (range 1 (inc n))))
            (cmbi [n k]
              (/ (fact n) (* (fact k) (fact (- n k)))))]
      (cond
        (= n 1) [1]
        :else (loop [k 0, result [], nn (dec n)]
                (if (> k nn)
                  result
                  (recur (inc k) (conj result (cmbi nn k)) nn)))))))

(defcheck solution-71aa21cd
  (fn ex97 [n]
    (if (= n 1)
      [1]
      (conj (vec (cons 1 (map (fn [[a b]] (+ a b)) (partition 2 1 (ex97 (dec n)))))) 1))))

(defcheck solution-71be9a6d
  (fn [n]
    (loop [row '(1) c 1]
      (if (= c n)
        row
        (recur ((fn [newrow]
                  (loop [ans '() tmprow newrow]
                    (if (= 1 (count tmprow))
                      ans
                      (recur (conj ans (+ (first tmprow) (second tmprow))) (rest tmprow))))) (conj (reverse (conj row 0)) 0)) (inc c))))))

(defcheck solution-7204f2c5
  (fn [n]
    (let [pascal-triangle (fn pascal-triangle [last-row]
                            (lazy-seq
                              (let [next-row (concat [1]
                                                     (map (fn [[a b]] (+ a b))
                                                       (partition 2 1 last-row))
                                                     [1])]
                                (cons next-row (pascal-triangle next-row)))))]
      (nth (cons [1] (pascal-triangle [1])) (dec n)))))

(defcheck solution-7335f2a2
  (fn pascal
    [indx]
    (loop [cur 1 lst [1]]
      (if (= cur indx) lst
                       (recur (inc cur) (concat [1]
                                                (map #(apply + %)
                                                  (let [sums (interleave
                                                               (partition 2 lst)
                                                               (partition 2 (rest lst)))]
                                                    (if (= (count (partition 2 (rest lst))) (count (partition 2 lst)))
                                                      sums
                                                      (concat sums [(last (partition 2 lst))]))))
                                                [1])))
      )
    ))

(defcheck solution-7385dcd6
  (let [fact (fn
               [n]
               (loop [accum 1
                      curr n]
                 (if (= curr 0)
                   accum
                   (recur (* accum curr) (- curr 1)))))]
    (let [choose (fn
                   [n m]
                   (/ (fact n) (* (fact m) (fact (- n m)))))]
      (fn pascal-row
        [x]
        (let [n (- x 1)]
          (loop [curr n
                 accum []]
            (let [new-accum (conj accum (choose n curr))]
              (if (= curr 0)
                new-accum
                (recur (- curr 1) new-accum)))))))))

(defcheck solution-7392b6c2
  #(nth (iterate (fn [s]
                   (map + (cons 0 s) (conj (vec s) 0))) [1])
     (- % 1)))

(defcheck solution-7392baac
  (fn [x]
    (letfn [(fac [n] (reduce * (range 1 (inc n))))
            (choose [n k] (/ (fac n)
                            (* (fac k)
                              (fac (- n k)))))]
      (map #(choose (dec x) %) (range x)))))

(defcheck solution-7416f948
  (fn [i]
    (let
     [
      n (dec i)
      factorial (fn [n]
                  (reduce * (range 1 (inc n)))
                  )
      pascal-number (fn [n r]
                      (/
                        (factorial n)
                        (*
                          (factorial r)
                          (factorial
                            (- n r)
                            )
                          )
                        )
                      )
      ]
      (apply vector
        (map
          (fn [r]
            (pascal-number n r)
            )
          (range i)
          )
        )
      )
    ))

(defcheck solution-746a67f7
  (fn pascal [n]
    (loop [r 1 an [1]]
      (if (= r n)
        an
        (recur (inc r)
          (loop [c 2 an2 [1] [x y & more] an]
            (if (nil? y)
              (conj an2 1)
              (recur (inc c) (conj an2 (+ x y)) (cons y more)))))))))

(defcheck solution-74913a4e
  (fn [k]
    (if (= k 1)
      [1]
      (let [n (- k  1)]
        (loop [i 1 coll [1]]
          (let [mult (/ (- (+ n 1) i) i) new (* (last coll) mult)]

            (if (= i n)
              (conj coll 1)
              (recur (+ i 1) (conj coll (int new))))))))))

(defcheck solution-7499bfb4
  #(if (= % 1) [1]
               (nth (let [fpr (fn helper [xs] (let [row (concat [1] (map (fn [x] (apply + x)) (partition 2 1 xs)) [1])]
                                                (lazy-seq (cons row (helper row)))))]
                      (fpr [1])) (- % 2))))

(defcheck solution-75163438
  (fn pascal [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (conj (first
                    (reduce (fn [[r l] x] [(conj r (+ l x)) x]) [[] 0] (pascal (dec n)))) 1))))

(defcheck solution-75402fe
  (fn tri [a]
    (if (= a 1)
      [1]
      (let [t (tri (dec a))
            t-nth #(nth t % 0)]
        (->> (range a)
          (map (juxt (comp t-nth dec) t-nth))
          (map #(reduce + %)))))))

(defcheck solution-7548a3e4
  (fn pascal [n]
    (letfn [
            (pascal-row [row]
              (let [next-row (mapv + (cons 0 row)(conj row 0))]
                (cons row (lazy-seq (pascal-row next-row)))))]

      (nth (pascal-row [1]) (dec n)))))

(defcheck solution-75652b6
  (fn [r]
    (let [n-comb (fn n-comb [n k]
                   (let [nm (apply * (range (inc (- n k)) (inc n)))
                         dn (apply * (range 1 (inc k)))]
                     (/ nm dn)))]
      (mapv #(n-comb (dec r) %) (range 0 r)))))

(defcheck solution-75fd6e07
  (fn pascals-triangle [n]
    (if (= n 1)
      [1]
      (cons 1 (conj (vec (for [[x y] (partition 2 1 (pascals-triangle (dec n)))] (+ x y))) 1)))))

(defcheck solution-76396731
  (fn [n]
    (loop [i (dec n) a [1]]
      (if (zero? i) a
                    (recur (dec i)
                      (map #(apply + %)
                        (cons [(first a)]
                          (partition-all 2 1 a))))))))

(defcheck solution-765d411e
  (fn [n]
    (reduce
      (fn [x _] (concat [1] (map #(apply + %) (partition 2 1 x)) [1]))
      [1]
      (range (dec n)))))

(defcheck solution-7697ec3
  (fn pasc-tree [n]
    (let
     [rem-first-last (fn [v]  (subvec v 1 (dec (count v))))
      double-members (fn [v]  (flatten (for [x v] [x x])))
      put-first-last (fn [v]  (conj (into [1] v) 1))
      calc           (fn [v]  (map #(reduce + %) (partition 2 v)))
      calc-row (fn [prev-row]
                 (-> prev-row rem-first-last double-members put-first-last calc put-first-last))]
      (cond
        (= 1 n) [1]
        (= 2 n) [1 1]
        (= 3 n) [1 2 1]
        :else
        (loop [act 4 row [1 2 1]]
          (if (> act n)
            row
            (recur (inc act) (calc-row row))))))))

(defcheck solution-7703c039
  (fn [n] (let [g (fn [xs]
                    (let [as xs
                          bs (rest xs)
                          cs (map + as bs)
                          ds (cons 1 cs)
                          es (concat ds '(1))]
                      es))]
            (nth (iterate g [1]) (- n 1)))))

(defcheck solution-771fbab0
  (fn pascal-row
    ([n]    (pascal-row [1] n))
    ([r n]  (if (= 1 n) r
                        (vec (pascal-row (concat [1] (map (partial apply +) (partition 2 1 r)) [1]) (dec n)))))))

(defcheck solution-775481b
  (fn [r]
    (let [f #(if (zero? %1) %2 (recur (- %1 1) (* %1 %2)))
          c #(if (zero? %2) 1 (quot (f %1 1) (* (f %2 1) (f (- %1 %2) 1))))]
      (map #(c (dec r) %) (range r)))))

(defcheck solution-776f140b
  (fn [n]
    (letfn [(pascal [c k n]
              (if (= k n)
                '(1)
                (conj (pascal (/ (* c (- n k)) (inc k)) (inc k) n) c)))]
      (pascal 1 0 (dec n)))))

(defcheck solution-77b8685
  #(loop [a %1 t [1]]
     (if (= 1 a)
       t
       (recur (dec a) (flatten (reduce (fn [[t p] c]
                                         [(conj t (+ p c)) c]) [[] 0] t))))))

(defcheck solution-77cd1a3d
  (fn [n]
    (loop [n n v [1]]
      (if (= n 1)
        v
        (recur (dec n) (flatten [1 [(map #(apply + %) (partition 2 1 v))] 1]))))))

(defcheck solution-78138e4d
  (fn t [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (let [r (map #(apply + %) (partition 2 1 (t (dec n))))]
                  (conj (apply conj '(1) r) 1)
                  ))))

(defcheck solution-788e98da
  (fn pascal [n]
    (if (= 1 n) [1]
                (let [prev (pascal (dec n))] (into [] (map #(+ (get prev (dec %) 0) (get prev % 0)) (range n)))))))

(defcheck solution-78ba6963
  (fn p [n]
    (condp = n
      1 [1]
      (let [l (p (dec n))
            x (concat [0] l)
            y (concat l [0])]
        (vec(map + x y))))))

(defcheck solution-78d6304f
  (fn pascal-row [n]
    (let [n (dec n)
          facseq (fn facseq
                   ([] (facseq 1 1))
                   ([n v]
                    (let [next-n (inc n)
                          next-v (* n v)]
                      (cons v (lazy-seq (facseq next-n next-v))))))
          fac-seq (facseq)
          n-fac (nth fac-seq n)]
      (reduce (fn [coll k]
                (conj coll (/ n-fac (* (nth fac-seq k)
                                      (nth fac-seq (- n k))))))
        []
        (range 0 (inc n))))))

(defcheck solution-78fa8f22
  #(loop [before [1]
          iterCount %]
     (if (= iterCount 1) before
                         (recur (concat [1] (map + before (rest before)) [1]) (dec iterCount)))))

(defcheck solution-797c2dfc
  (fn ptri [row]
    (letfn [
            (factorial [x] (loop [num x result 1] (if (= num 0) result (recur (dec num) (* result num)))))
            (pascColRow [row col] (/ (factorial row) (* (factorial col) (factorial (- row col)))))
            ]
      (if (= 1 row)
        [1]
        (loop [col 0 result []]
          (if (= col (dec row))
            (conj result 1)
            (recur (inc col)  (conj result (pascColRow (dec row) col)))))))))

(defcheck solution-79ac14d0
  (fn [n] (reduce (fn [a x] (conj a (* (last a) x))) [1] (map #(/ (- (dec n) %) (inc %)) (range (dec n))))))

(defcheck solution-79d6b3a2
  (fn
    [n]
    (letfn [(factorial
              [x]
              (if (zero? x)
                1 (* x (factorial (dec x)))))]
      (let [n (dec n)]
        (vec (map #(/ (factorial n) (factorial %1) (factorial (- n %1)))
               (range 0 (inc n))))))))

(defcheck solution-7a3ae16
  (fn f [n]
    (lazy-seq (if (= n 1)
                [1]
                (let [last-row (f (dec n)) p1 (rest last-row) p2 (drop-last last-row)]
                  (concat [1] (map + p1 p2) [1]))
                )
      )
    ))

(defcheck solution-7b6e088d
  (fn [n]
    (reduce (fn [l _]
              (loop [more (next l)
                     rl [1]
                     p    (first l)]
                (if (nil? more)
                  (conj rl 1)
                  (recur (next more) (conj rl (+ p (first more))) (first more)))))
      [1] (take (dec n) (repeat 1)))))

(defcheck solution-7b7061f8
  #(letfn [(fac [x] (apply * (range 1 (inc x))))]
     (loop [n (dec %) r []]
       (if (neg? n) r
                    (recur (dec n) (cons (quot (fac (dec %)) (* (fac n) (fac (- (dec %) n)))) r))))))

(defcheck solution-7b8bb5c3
  (fn [cnt]
    (last (take cnt (iterate (fn [x] (#(map + % (rseq %)) (conj (vec x) 0))) [1])))))

(defcheck solution-7c33f3f0
  (fn [n]
    ((fn pascal-inner [last i-n]
       (if (<= i-n 1)
         last
         (recur (vec (for [x (range (inc (count last)))] (+ (get last (dec x) 0) (get last x 0))))
           (dec i-n)))
       )
     [1] n)))

(defcheck solution-7c89fec2
  (fn pasc-row [n]
    (let [n (dec n)
          fact (fn [x] (apply * (range 1 (inc x))))]
      (map
        (fn row [k]
          (quot (fact n)
            (* (fact k) (fact (- n k)))))
        (range (inc n))))))

(defcheck solution-7d243f28
  (fn [n]
    (let [nextrow (fn [prevrow]
                    (map #(apply + %) (partition 2 1 (concat [0] prevrow [0]))))]
      (nth (iterate nextrow [1]) (- n 1)))))

(defcheck solution-7d4e3f9b
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [p (pascal (dec n))]
        (vec (map + (conj p 0) (cons 0 p)))))))

(defcheck solution-7d7110ff
  (fn p-trio [level]
    (let [next-line (fn [line]
                      (reduce #(conj % (+ (nth line %2 0)
                                          (nth line (dec %2) 0))) [] (range (inc (count line)))))]
      (loop [l [1]]
        (if (= level (count l))
          l
          (recur (next-line l)))))))

(defcheck solution-7dc38ffe
  (fn [n]
    (letfn [(next-row [row]
              (loop [[hd & tl] row
                     final [1]]
                (if (empty? tl)
                  (conj final 1)
                  (let [next-val (+ hd (first tl))]
                    (recur tl (conj final next-val))))))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-7ddb10cd
  (fn ! [level]
    (if (= 1 level)
      [1]
      (conj (second
              (reduce
                (fn [[prev acc] x]
                  [x (conj acc (+ prev x))])
                [0 []]
                (! (dec level)))) 1))))

(defcheck solution-7e152b7
  (fn [x] (nth (iterate
                 #(map (partial reduce +) (partition 2 1 [0] (cons 0 %)))
                 [1])
            (dec x))))

(defcheck solution-7e443ded
  (fn pascal [n]
    (letfn [(F [n k]
              (cond
                (zero? k) 1
                (zero? n) 0
                (< n k) 0
                :else (+ (F (dec n) (dec k))
                         (F (dec n) k))))]
      (map (partial (memoize F)
             (dec n))
        (range n)))))

(defcheck solution-7e45acf1
  (fn [row]
    (letfn [(pascal-triangle [row]
              (cond
                (= row 1) [1]
                (= row 2) [1 1]
                :else (concat
                       (cons 1
                         (map
                           (fn [[x y]] (+ x y))
                           (partition 2 1 (pascal-triangle (dec row)))))
                       [1])))]
      (pascal-triangle row))))

(defcheck solution-7e8e3083
  (fn [n]
    (letfn [ ( pascal-it [row]
               (concat [(first row)]
                       (map (fn[[m n]] (+ m n)) (partition 2 1 row))
                       [(last row)])) ]
      (nth (iterate pascal-it [1]) (- n 1)))))

(defcheck solution-7eb9dbe6
  (fn [rownum]
    (if (= rownum 1) [1]
                     (loop [row []]
                       (if
                        (= (count row) rownum)
                         row
                         (recur (concat [1] (map + row (rest row)) [1])))))))

(defcheck solution-7f0e6f89
  #(loop [n 1
          l [1]]
     (if (= n %) l
                 (recur (inc n)(vec (map + (cons 0 l)(conj l 0)))))))

(defcheck solution-7f2787bd
  (fn [row]
    (loop [k 1 result [1]]
      (if (= k row)
        result
        (let [p (* (last result) (/ (- row k) k))]
          (recur (inc k) (conj result p)))))))

(defcheck solution-7f9ba96f
  (fn pascal-triangle [n]
    (if (= n 1)
      [1]
      (let [m (conj (pascal-triangle (dec n)) 0)
            n (vec (cons 0 (pascal-triangle (dec n))))]
        (vec
          (for [index (range (count m))]
            (+ (m index) (n index))))))))

(defcheck solution-8020f27f
  (fn [n]
    (nth (iterate (fn [row]
                    (concat [1] (map #(apply + %) (partition 2 1 row)) [1]))
           [1])
      (dec n))))

(defcheck solution-80250d22
  (fn [row]
    (nth
      (iterate
        #(map +
           (concat '(0) %)
           (concat % '(0)))
        '(1)) (dec row))))

(defcheck solution-803dfd4f
  (fn [row-count]
    (nth (iterate (fn helper [prev-row]
                    (loop [remaining prev-row next-row [1]]
                      (if (empty? (rest remaining))
                        (into [] (cons 1 next-row))
                        (recur (rest remaining) (cons (+ (first remaining) (first (rest remaining))) next-row))))) [1]) (dec row-count))))

(defcheck solution-806d63bc
  (fn [n]
    (last (take n (iterate #(map + `(0 ~@%) `(~@% 0)) [1])))))

(defcheck solution-80773a0a
  (fn [n]
    (nth
      (iterate #(map + (concat [0] %) (concat % [0])) [1])
      (dec n)

      )))

(defcheck solution-809c5539
  (fn [n] (let [fact #(reduce * 1 (range 1 (+ 1 %)))
                comb (fn [n i] (/ (fact n) (* (fact i) (fact (- n i)))))]
            (map #(comb (- n 1) %) (range n)))))

(defcheck solution-80b4060
  (fn tr [n]
    (if (= 1 n) [1]
                (let [p (partition 2 1 (tr (dec n)))]
                  (vec (concat [1] (map #(apply + %) p) [1]))))))

(defcheck solution-8174babf
  (fn f [n] (if (= n 1) [1] (let [t (f (dec n))] (map + (concat [0] t) (concat t [0]))))))

(defcheck solution-8198b4a9
  (fn [n]
    (nth (iterate #(into [] (map + (cons 0 %) (conj % 0))) [1]) (dec n))))

(defcheck solution-819c721a
  (fn tri [n]
    (if (= n 1)
      [1]
      (let [next-row (fn [row] (map + (cons 0 row) (conj row 0)))]
        (vec (next-row (tri (dec n))))))))

(defcheck solution-81a2ba46
  (fn [n]
    (loop [ret '(1) c 1]
      (if
       (= c n) ret
               (let [v (/ (* (first ret) (- n c)) c)]
                 (recur (conj ret v) (inc c)))))))

(defcheck solution-81d76352
  (fn pascal [row]
    (map (comp last take)
      (reverse (range 1 (+ 1 row)))
      (take row (iterate (partial reductions +) (take row (repeat 1)))))))

(defcheck solution-81f8f66a
  (fn pt [cnt]
    (if (= cnt 1) [1]
                  (concat [1] (map #(apply + %) (partition 2 1 (pt (dec cnt)))) [1]))))

(defcheck solution-82344fb4
  (fn _ [n]
    (if (= 1 n) [1]
                (cons 1 (map
                          #(apply + %) (partition-all 2 1 (_ (dec n))))))))

(defcheck solution-8286635d
  (fn [n]
    (let [a (dec n)
          fact #(apply * (map inc (range %)))]
      (map #(/ (fact a) (fact (- a %)) (fact %)) (range n)))))

(defcheck solution-82c84b04
  (fn z [i]
    (nth
      ((fn x [q]
         (cons
           q
           (lazy-seq
             ((fn c [coll]
                (x (concat
                    [(first coll)]
                    (map #(+ (first %) (second %)) (partition 2 1 coll))
                    [(last coll)]))) q)))) [1]) (dec i))))

(defcheck solution-82d6cde7
  (fn [level]
    (let [new-row (fn [prev-row]
                    (conj
                      (vec
                        (cons 1 (map
                                  #(apply + %)
                                  (partition 2 1 prev-row))))
                      1))]
      (loop [ctr 1 acc [1]]
        (if (= ctr level)
          acc
          (recur (inc ctr) (new-row acc)))))))

(defcheck solution-82e0fb2e
  (fn [n]
    (reduce #(conj %1 (* (last %1) (/ (- n %2) %2 ))) [1] (range 1 n))))

(defcheck solution-83247aeb
  (fn [n]
    (reduce
      (fn [p _] (lazy-cat '(1) (map + p (rest p)) '(1)))
      [1]
      (range (dec n)))))

(defcheck solution-8394842e
  #(nth  (iterate  (fn [c] (concat [1] (map + c (rest c)) [1]))  [1])  (- % 1)))

(defcheck solution-83990505
  (fn pascal [n]
    (letfn [(inc-row [row]
              (if (> (count row) 1)
                (cons (+ (first row) (second row)) (inc-row (rest row)))
                row
                )
              )]
      (if (= n 1)
        [1]
        (let [prev (pascal (dec n))]
          (cons 1 (inc-row prev))
          )
        )
      )
    ))

(defcheck solution-83b16350
  (fn [n]
    (let [fact #(reduce * (range 1 (inc %)))
          comb (fn [k p] (/ (fact k) (fact p) (fact (- k p))))]
      (for [k (range 0 n)] (comb (dec n) k)))))

(defcheck solution-83ca6630
  (fn pt [n]
    (cond (= 1 n) [1]
          (= 2 n) [1 1]
          :else (let [pl (pt (dec n))]
                  (conj (vec (cons 1 (map + (rest pl) (butlast pl)))) 1)))))

(defcheck solution-83da4b54
  #(reduce (fn [coll v]
             (let [left (cons 0 coll)
                   right (conj coll 0)]
               (into [] (map + left right))))
     [1] (range (dec %))))

(defcheck solution-8404854d
  #(letfn [(fac [x] (reduce * 1 (range 1 (inc x))))
           (binom [n k] (/ (fac n) (* (fac k) (fac (- n k)))))]
     (map (partial binom (dec %)) (range %))))

(defcheck solution-844d5cfb
  (fn pascal-triangle [n]
    (cond
      (= 1 n) [1]
      (= 2 n) [1 1]
      :else (concat [1] (map #(apply + %) (partition 2 1 (pascal-triangle (- n 1)))) [1]))))

(defcheck solution-844e1c33
  (fn [n]
    (nth
      (iterate #(concat [1] (map (fn [[x y]] (+ x y))  (partition 2 1 %)) [1]) [1]) (dec n))))

(defcheck solution-84e0c61
  (fn pt [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (vec (concat [1]
                   ((fn addpairs
                      ([s] (addpairs s []))
                      ([s acc]
                       (if (< (count s) 3) (conj acc (apply + s))
                                           (recur (rest s) (conj acc (+ (first s) (second s)))))))
                    (pt (dec n)))
                   [1])))))

(defcheck solution-85565c99
  (fn pascal-triangle [row]
    (if (<= row 2)
      (if (= row 1)
        [1]
        (if (= row 2)
          [1 1]
          )
        )
      (let [prev-row (pascal-triangle (dec row)) first-part (drop-last prev-row) second-part (drop 1 prev-row)]
        (conj (apply vector (cons 1 (map + first-part second-part))) 1)
        )
      )
    ))

(defcheck solution-855f5175
  (fn [r]
    (loop [c 1 row [1]]
      (if (< c r)
        (recur (inc c) (conj row (long (* (last row) (/ (- r c) c)))))
        row))))

(defcheck solution-857c303e
  (fn pascal [n]
    (letfn [(next-row [xs] (into [] (flatten [1
                                              (map #(apply + %)
                                                (partition 2 (interleave xs (rest xs))))
                                              1])))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-858fb52b
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (apply vector (let [lower (pascal (dec n))]
                      (map #(+ (get lower (dec %) 0) (get lower % 0)) (range n)))))))

(defcheck solution-860f8f08
  (fn rec [n]
    (if (= 1 n)
      [1]
      (let [r (rec (dec n))
            rl (concat [0] r)
            rr (concat r [0])]
        (vec (for [[x y] (map vector rl rr)]
               (+ x y)))))))

(defcheck solution-861c2852
  (fn [row]
    (loop [result [1]]
      (if (= (count result) row)
        result
        (let [n (dec row) k (count result)]
          (recur (conj result (* (last result) (/ (+ n 1 (- k)) k)))))))))

(defcheck solution-8635a353
  (fn [a]
    (loop [i 1
           r [1]]
      (if (< i a)
        (recur (inc i) (concat '(1) (map + r (rest r)) '(1)))
        r))))

(defcheck solution-86ef85cf
  (fn pt [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else
      (loop [i 3 rowc [1 2 1]]
        (if (= i n)
          rowc
          (recur
            (inc i)
            (concat [1] (concat (map #(apply + %) (partition 2 1 rowc)) [1]))))))))

(defcheck solution-874d26a1
  (fn [n]
    (letfn [(pt-next-line [l]
              (conj (vec (cons 1 ((comp (partial map #(apply + %))
                                        (partial partition 2)
                                        next butlast)
                                  (mapcat list l l)))) 1))]
      (cond (= n 1) [1]
            (= n 2) [1 1]
            :else (loop [res [1 1] n (- n 2)]
                    (if (zero? n) res
                                  (recur (pt-next-line res) (dec n))))))))

(defcheck solution-879e1780
  (fn [n]
    (nth (iterate #(concat (conj (map (partial apply +) (partition 2 1 %)) 1) [1]) '(1)) (- n 1))))

(defcheck solution-87cf02dd
  (fn f[i]
    (if (= 1 i)
      [1]
      (let [s (f (dec i))]
        (concat [1] (map #(+ % %2) s (rest s)) [1])))))

(defcheck solution-8822ecc7
  (fn [m]
    (letfn [(fact [n] (reduce * (range 1 (inc n))))
            (binom [m n] (/ (fact m) (* (fact n) (fact (- m n)))))]
      (for [n (range 1 (inc m))] (binom (dec m) (dec n))))))

(defcheck solution-8825ee7c
  (fn p [n]
    (if (= 1 n)
      [1]
      (let [r (p (dec n))]
        (concat [1]
                (map + (next r) r)
                [1])))))

(defcheck solution-889c4b97
  (fn
    [n]
    (nth (iterate (fn [row] (let [prow (partition 2 1 row)]
                              (concat [1] (map #(apply + %) prow) [1])))
           [1])
      (dec n))))

(defcheck solution-88cbbf95
  (fn [n]
    (letfn [(reproduce [coll]
              (cons 1 (conj (vec (map (fn [c] (apply + c)) (partition 2 1 coll))) 1)))]
      (loop [x 1 t [1]]
        (if (= x n)
          t
          (recur (inc x) (reproduce t)))))))

(defcheck solution-895a8bc
  (fn [n]
    (loop [i 1 col [1]]
      (if (= n i)
        col
        (recur (inc i) (concat [1] (map #(apply + %) (partition 2 1 col)) [1]))))))

(defcheck solution-896aacd3
  #(letfn
    [(f [n] (reduce * (range 1 (inc n))))
     (nCr [n r] (quot
                  (f n)
                  (* (f r) (f (- n r)))))]
     (for [r (range %)]
       (nCr (dec %) r))))

(defcheck solution-896cf67d
  (fn pascal-row [n]
    (letfn [(f [r]
              (let [rl (count r)
                    calc-row (fn [i x]
                               (if (or (= 0 i) (= rl i)) 1
                                                         (+ x (nth r (dec i)))))]
                (if (= n (count r))
                  r
                  (->> (conj r 1)
                    (mapv calc-row (range (inc rl)))
                    f))))]
      (if (= 1 n)
        [1]
        (f [1])))))

(defcheck solution-897d478e
  #(letfn [(f [s]
             (loop [[a & r] s v [1]]
               (if r
                 (recur r (conj v (+ a (first r))))
                 (conj v 1))))]
     (nth (iterate f [1]) (dec %))))

(defcheck solution-897f070d
  (fn pascal [n]
    (if (= n 1) [1]
                (letfn [(adjsum [xs] (map + xs (rest xs)))]
                  (conj (vec (cons 1 (adjsum (pascal (dec n))))) 1)))))

(defcheck solution-898e6878
  (fn pascal [row]
    (if (= 1 row)
      [1]
      (let [last-row (pascal (dec row))]
        (map (fn [idx]
               (if (or (zero? idx) (= idx (count last-row)))
                 1
                 (let [a (nth last-row (dec idx))
                       b (nth last-row idx)]
                   (+ a b))))
          (range (inc (count last-row))))
        ))))

(defcheck solution-89a1027d
  #(reduce (fn [r _] (conj (into [1] (map (partial reduce +) (partition 2 1 r))) 1))
     [1]
     (range (dec %))))

(defcheck solution-89ba166e
  (fn [n]
    (loop [c [1] i 1]
      (if (= i n)
        c
        (recur (loop [res [1] a c]
                 (if (= 1 (count a))
                   (conj res 1)
                   (recur (conj res (+ (first a) (second a)))
                     (rest a))))
          (inc i))))))

(defcheck solution-8a0e058b
  (fn pascal-triangle-row [n]
    (letfn [(step [xs]
              (-> (into [(first xs)] (map #(reduce + %1N) (partition 2 1 xs)))
                (conj (last xs))))]
      (last (take n (iterate step [1]))))))

(defcheck solution-8a3fa2b6
  (fn fun [n]
    (if (= n 1)
      [1]
      (let [prev (fun (dec n))]
        (map + (concat prev [0]) (concat [0] prev))))))

(defcheck solution-8a7674e9
  (fn [n]
    (let [f #(if (< % 2) 1 (reduce * % (range 1 %)))
          c (fn [n i] (/ (f n) (f i)  (f (- n i))))]
      (map #(c (- n 1) %) (range n)))))

(defcheck solution-8a9273e9
  (fn [n]
    (letfn [(nextpascal [xs]
              (vec (map (fn [[a b]] (+ a b)) (partition 2 1 (cons 0 (conj xs 0))))))]
      (reduce (fn [a b] (nextpascal a)) [1] (range (dec n))))))

(defcheck solution-8aebcecd
  (fn [i]
    (nth (iterate #(map + `[~@% 0] `[0 ~@%])
           [1])
      (- i 1))))

(defcheck solution-8b54e37
  ( fn tri [x]
    (cond
      (= x 1) [1]
      (= x 2) [1 1]
      :else (let [y (tri (dec x))] (concat [1] (map + (rest y) (drop-last y)) [1])))))

(defcheck solution-8b9336ec
  (fn [N]
    (letfn [(trap [xs]
              (cons xs (lazy-seq (trap (->> (flatten [0 xs 0])
                                         (partition 2 1)
                                         (map #(reduce + %)))))))]
      (nth (trap [1]) (dec N)))))

(defcheck solution-8c61b2f6
  (fn [row]
    (let [n (- row 1)]
      (vec
        (map
          (fn [y] (/ (reduce * (range 1 (inc n)))
                    (* (reduce * (range 1 (inc y)))
                      (reduce * (range 1 (inc (- n y)))))))
          (range (+ n 1)))))))

(defcheck solution-8cf25e9f
  (fn [n]
    (loop [acc [1] n (dec n)]
      (if (= 0 n) acc
                  (recur (let [acc (conj acc 0)] (apply vector (concat [1] (for [i (range (dec (count acc)))]
                                                                             (+ (nth acc i) (nth acc (inc i))))))) (dec n))))))

(defcheck solution-8cf42117
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [prev (pascal (dec n))]
        (into [] (map #(apply + %) (partition 2 1 (concat [0] prev [0]))))
        )
      )
    ))

(defcheck solution-8d2cdd3d
  (fn [n]
    (for [i (range 0 n)
          :let [f
                (fn binom [n k]
                  (if	(or (= k 0) (= k n))
                    1
                    (+
                     (binom (dec n) (dec k))
                     (binom (dec n) k))))]]
      (f (dec n) i))))

(defcheck solution-8dbc068c
  #(last
     (take %
       (iterate
         (fn [x]
           (vec (map + (conj x 0 ) (cons 0 x )))
           )
         [1]
         )
       )
     ))

(defcheck solution-8dbfb3c5
  (fn [r]  (map #(/ (reduce * (range (inc (- (dec r) %)) r)) (reduce * (range 1 (inc %)))) (take r (iterate inc 0)))))

(defcheck solution-8de90b08
  (fn [n]
    (let [next-row (fn
                     ([prev-row] (if (seq prev-row)
                                   (concat [1] (map (partial apply +) (partition 2 1 prev-row)) [1])
                                   [1])))]
      (nth (iterate next-row []) n))))

(defcheck solution-8e4a6e3d
  (fn [n]
    (reduce
      (fn [c k] (conj c (* (/ (- (dec n) k)
                             (inc k))
                          (last c))))
      [1]
      (range (dec n)))))

(defcheck solution-8f239f84
  (fn [n]
    (loop [row [1]
           pass (dec n)]
      (if (zero? pass)
        row
        (recur (conj (->> row
                       (partition 2 1)
                       (map (partial apply +))
                       (cons 1)
                       vec) 1)
          (dec pass))))))

(defcheck solution-8f48383c
  (letfn [(M [a]
            (into [] (map + (cons 0 a) (conj a 0))))

          (P []
            (cons [1] (lazy-seq (map M (P)))))]
    (fn [n]
      (nth (P) (dec n)))))

(defcheck solution-8f4ca02b
  (fn ! [n]
    (if (= n 1)
      [1]
      (loop [prev_pas (! (dec n)) i 1 result (vector (first prev_pas))]
        (if (= i (+ 1 (count prev_pas)))
          result
          (recur prev_pas
            (inc i)
            (conj result
              (if (= i (count prev_pas))
                (nth prev_pas (dec i))
                (+ (nth prev_pas i )
                   (nth prev_pas (dec i) ) )
                )
              )
            )
          )))))

(defcheck solution-8f71c53a
  (fn [n]
    (loop [n n res [1]]
      (if (= n 1) res
                  (let [x (cons 0 res)]
                    (recur (dec n)
                      (map + x (reverse x))))))))

(defcheck solution-8f861ae
  #(last (take % (iterate (fn [x]
                            (apply vector (map + (cons 0 x) (conj x 0)))) [1]))))

(defcheck solution-8faa9226
  (fn [n]
    (letfn [(next-row [r]
              (concat
               (conj
                 (map #(+ %1 %2) r (rest r)) 1)
               [1]))
            (pt [n r]
              (if (== 1 n)
                r
                (recur (dec n) (next-row r)))
              )]
      (pt n [1]))
    ))

(defcheck solution-8ffb39ab
  (fn pas [i] (if (= 1 i) [1] (vec (let [p (conj (pas (dec i)) 0)] (map + p (reverse p)))))))

(defcheck solution-901d692c
  (fn [r]
    (reduce
      (fn [agg c] (conj agg (/ (* (last agg) (- r c)) c)))
      [1]
      (range 1 r)
      )))

(defcheck solution-9044482f
  #(nth (iterate (fn [x] (concat [1]
                                 (map + x (rest x))
                                 [1]))
          [1]) (dec %)))

(defcheck solution-907098b9
  (fn [x]
    (loop [a [1]
           n 1]
      (if (= n x)
        a
        (recur
          (vec (concat '(1) (map #(apply + %) (partition 2 1 a)) '(1)))
          (+ n 1))))))

(defcheck solution-90a333e0
  (fn pascal
    [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :default (concat [1]
                       (map (partial reduce +) (partition 2 1 (pascal (dec n))))
                       [1]))))

(defcheck solution-91bc94f3
  (fn [n]
    (loop [i 1 row [1]]
      (if (>= i n)
        row
        (recur
          (inc i)
          (vec
            (map (partial apply +)
              (partition 2 (interleave (into [0] row)
                             (conj row 0))))))))))

(defcheck solution-91bed93d
  (fn [n] (nth (iterate #(cons 1 (concat (map + % (rest %)) [1])) [1]) (dec n))))

(defcheck solution-91cb608f
  (letfn [
          (factorial [x] (apply * (range 1 (inc x))))
          (binomial-coefficient [n r] (/ (factorial n) (* (factorial r) (factorial (- n r)))))
          (pascals-triangle-row [x] (map #(binomial-coefficient (dec x) %) (range x)))]
    pascals-triangle-row))

(defcheck solution-931366c4
  #(nth (iterate (fn[c]
                   (loop[[f & c] c r [1]]
                     (if c
                       (recur c (conj r (+ f (nth c 0))))
                       (conj r f)))) [1])(- % 1)))

(defcheck solution-9350e782
  (fn [row]
    (nth (iterate (fn [coll]
                    (concat [1] (map + coll (rest coll)) [1]))
           (list 1))
      (dec row))))

(defcheck solution-936abe15
  (fn pascal [n]
    (-> #(->> (cons 0 (conj % 0))
           (partition 2 1)
           (mapv (partial apply +)))
      (iterate [1])
      (nth (dec n)))))

(defcheck solution-93807069
  (fn [n]
    (loop [row-count 1 row [1]]
      (if (= row-count n)
        row
        (recur (inc row-count) (map #(+ (first %) (second %)) (partition 2 1 (flatten (vector 0 row 0)))))))))

(defcheck solution-938bf0d3
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [n-1 (pascal (- n 1))]
        (mapv + (conj n-1 0) (concat [0] n-1))))))

(defcheck solution-939ba960
  (fn pascal  ([n] (pascal (dec n) [1]))
    ([n row] (if (= n 0)
               row
               (pascal (dec n) (conj (vec (cons 1 (vec (map (partial apply +) (partition 2 1 row))))) 1))
               )
     )
    ))

(defcheck solution-93dcb09
  (letfn [(nxt [xs]
            (map + (conj (vec xs) 0) (cons 0 xs)))]
    (fn [n]
      (if (= n 1) [1]
                  (nth (iterate nxt [1]) (- n 1))))))

(defcheck solution-9463fe59
  (fn p [n]
    (if (= n 1)
      [1]
      (concat [1]
              (let [s (p (- n 1))]
                (remove #(= 1 %)
                  (map #(apply + %)
                    (interleave
                      (partition 2 s)
                      (partition 2 (concat (rest s) '(0)))))))
              [1]
              ))))

(defcheck solution-948fc7cb
  (fn pascal-triangle
    [row]
    (let [ks (range row)
          count-ks (dec (count ks))
          ! #(reduce * (range 1 (inc %)))
          comb (fn [n k]  (/ (! n) (* (! k) (! (- n k)))))]
      (map #(comb count-ks %) ks))))

(defcheck solution-94cf1270
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (let [p (pascal (dec n))]
        (vec (map + (concat [0] p) (conj p 0)))
        )
      )
    ))

(defcheck solution-95ce513f
  #(nth (iterate
          (fn [x]
            (concat [(first x)] (map + x (rest x)) [(last x)])
            ) [1]) (dec %)))

(defcheck solution-965d512a
  (fn pascal-row [n]
    (cond
      (= 1 n) [1]
      (= 2 n) [1 1]
      :else (concat [1]
                    ((fn pascal-row-rec [s]
                       (when (not (nil? (next s)))
                         (cons (+ (first s) (second s)) (pascal-row-rec (next s)))))
                     (pascal-row (dec n)))
                    [1]))))

(defcheck solution-9673d3f0
  (fn [n]
    (nth (iterate #(mapv + (conj % 0) (cons 0 %)) [1]) (dec n))))

(defcheck solution-967d12e
  (fn pascal [n] (if (= n 1) [1]
                             (for [[a b] (partition 2 1 (concat [0] (pascal (dec n)) [0]))] (+ a b)))))

(defcheck solution-96c58d35
  (fn pascal [row-num]
    (letfn [(build-triangle [row]
              (let [next-row (concat [1] (map #(apply + %) (partition 2 1 row)) [1])]
                (lazy-seq (cons row (build-triangle next-row)))))]
      (nth (build-triangle [1]) (dec row-num)))))

(defcheck solution-96ded5db
  (fn [n]
    (loop [n' 1 r [1]]
      (if
       (= n n')
        r
        (recur
          (inc n')
          (concat [1] (map + r (rest r)) [1]))))))

(defcheck solution-96df3dd2
  (fn
    [n]
    (letfn [(fac [n] (if (< n 2)
                       1
                       (loop [a 1
                              n n]
                         (if (= n 0)
                           a
                           (recur (* a n) (- n 1))))))
            ; binomial coefficient
            (bc [n k] (quot (quot (fac n) (fac k)) (fac (- n k))))]
      (vec (map (partial bc (- n 1)) (range n))))))

(defcheck solution-9741978e
  (fn pascal-triangle [n]
    (letfn [(ipt [[x & more], result]
              (if (nil? more)
                (conj result x)
                (recur more (conj result (+ x (first more))))))]
      (if (= 1 n)
        [1]
        (loop [i 2, pv [1]]
          (if (= i n)
            (ipt pv [1])
            (recur (inc i) (ipt pv [1]))))))))

(defcheck solution-97e453b
  (fn[x]
    (let [row (- x 1)]
      (map (fn[col]
             (/
               (#(reduce * (range 1 (+ % 1))) row)
               (*
                 (#(reduce * (range 1 (+ % 1))) col)
                 (#(reduce * (range 1 (+ % 1))) (- row col)))))
        (range (+ 1 row))))))

(defcheck solution-987b3c4e
  #(letfn[(factorial[n]
            (apply * (range 1 (inc n))))
          (binomial[n k]
            (/ (factorial n) (* (factorial k) (factorial (- n k)))))]
     (map (fn[col] (binomial (dec %) col)) (range %))))

(defcheck solution-98896955
  (fn [t] (letfn [(fac [x] (reduce * (range 1 (inc x))))
                  (choose [n k] (/ (fac n) (* (fac (- n k)) (fac k))))]
            (if (= t 1)
              [1]
              (let [n (dec t)]
                (map #(choose n %) (range t)))))))

(defcheck solution-98baf6fb
  (fn [x] (vec (nth (iterate #(map + (conj (vec %) 0) (vec (cons 0  %))) [1]) (dec x)))))

(defcheck solution-98ee179e
  (fn [rownum]
    (letfn [(pascal-row [row]
              (flatten [1 (map #(reduce + %) (partition 2 1 row)) 1]))]
      (last (take rownum (iterate pascal-row [1]))))))

(defcheck solution-99283d17
  (fn [n]
    (loop [acc [1] t 1]
      (if (= t n) acc
                  (recur (concat [1] (map #(apply + %) (partition 2 1 acc)) [1]) (inc t))))))

(defcheck solution-9928df1b
  (fn [n] (letfn [(p-tr [x] (map + (cons 0 x) (concat x [0])))]
            (->> (iterate p-tr [1]) (take n) last))))

(defcheck solution-99710985
  (fn pas [n]
    (if (= n 1) '(1)
                (concat '(1)
                        (map (partial apply +) (partition 2 1 (pas (dec n))))
                        '(1)))))

(defcheck solution-99745d78
  (fn [n]
    (letfn [(step [coll]
              (lazy-seq
                (cons
                  coll
                  (step
                    ((fn
                       [coll]
                       (map
                         #(+ (first %) (second %))
                         (partition 2 1 (concat '(0) coll '(0)))))
                     coll)))))]
      (nth (step [1]) (dec n)))))

(defcheck solution-99913636
  (fn pascal [n]
    (map
      #(Math/round (double (reduce * (map
                                       (fn [i] (/ (- n i) i))
                                       (range 1 (inc %))))))
      (range 0 n))))

(defcheck solution-9996d8c0
  (fn f [n] (cond
              (= n 1) [1]
              (= n 2) [1 1]
              :else (let [l (f (dec n))]
                      (into [] (concat '(1) (map #(+ (l %) (l (dec %))) (range 1 (dec n))) '(1)))))))

(defcheck solution-999e2d1b
  (fn [x]
    (loop [n 1 val [1]]
      (if (= n x)
        (vec val)
        (recur
         (inc n) (flatten (reduce into [1] [(map (partial reduce +) (partition 2 1 val)) [1]])))))))

(defcheck solution-99a6d4bd
  (fn [n]
    (loop [n n res [1]]
      (if (= n 1) res
                  (recur (dec n)
                    (map + (concat [0] res) (concat res [0])))))))

(defcheck solution-99cd8961
  (fn pascaltri [v]
    (letfn [(ptricalc [x]
              (concat (list (first x)) (map + (rest x) (butlast x)) (list (last x)))
              )]
      (nth (iterate ptricalc [1]) (- v 1))
      )
    ))

(defcheck solution-9a1b5998
  (fn pascal-row
    [n]
    (loop [i n
           result [1]]
      (if (= i 1)
        result
        (recur (dec i)
          (conj (->> result
                  (cons 0)
                  (partition 2 1)
                  (map #(apply + %))
                  vec) 1))))))

(defcheck solution-9a1fcb22
  (fn [n] (last (take n (iterate #(map + (conj % 0) (conj (vec %) 0)) '(1) )))))

(defcheck solution-9a80b03f
  (fn [step]
    (let [c (fn [n r]
              (let [f (fn fact[k](reduce * (range 1 (inc k))))]
                (/ (f n) (* (f r) (f (- n r))))))]
      (map (fn[idx](c (dec step) idx))  (range 0 step)))))

(defcheck solution-9a8fbf89
  (fn [x]
    (if (= x 1)
      [1]
      (reduce (fn [ls x]
                (map +
                  (concat ls [0])
                  (concat [0] ls)))
        [1]
        (range 1 x)))))

(defcheck solution-9ab21139
  (fn p [n] (if (= n 1) [1] (let [o (p (dec n))] (into [] (map + (conj o 0) (into [0] o)))))))

(defcheck solution-9ace629d
  (fn ptri [n]
    ( if (< n 2) [1]
                 ((fn [v] (conj (vec (conj
                                       (map (fn [s] (apply + s))
                                         (partition 2 (butlast (rest (interleave v v))))) 1)) 1)) (ptri (dec n))))))

(defcheck solution-9b663363
  (fn [n]
    (nth
      (iterate
        (fn [v]
          (cons (first v)
            (concat (map #(apply + %) (partition 2 1 v))
                    (list (last v))))) [1]) (dec n))))

(defcheck solution-9ba37426
  (fn [n]
    (nth (iterate (fn [s]
                    (concat [1] (map (partial apply +) (partition 2 1 s)) [1]))
           [1]) (dec n))))

(defcheck solution-9bcad4e1
  #(loop [r [1] n (dec %)]
     (if (= n 0) r
                 (recur (concat [1] (map (fn [p] (apply + p)) (partition 2 1 r)) [1]) (dec n)))))

(defcheck solution-9bccef83
  (fn pascal [n]
    (let [nextrow
          (fn [row]
            (map + (cons 0 row) (concat row [0])))]
      (nth (iterate nextrow [1]) (dec n)))))

(defcheck solution-9bd64893
  (fn nth-pascal-row
    [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      true (loop [n n row [1 2 1]]
             (if (= n 3)
               row
               (recur (dec n)
                 (concat [1] (map + (drop-last row) (drop 1 row)) [1])))))))

(defcheck solution-9be24e22
  (fn [n]
    (nth (iterate (fn [row]
                    (concat [1]
                            (map #(apply + %) (partition-all 2 1 row)))) [1])
      (dec n))))

(defcheck solution-9c0d2829
  (fn [x] (let [n (dec x)] (reduce #(conj %1 (/ (* (last %1) (- n %2)) (inc %2))) [1] (range n)))))

(defcheck solution-9c57b699
  (fn [n]
    (last (take n
            (iterate
              #(concat [1]
                       (map (partial apply +) (partition 2 1 %))
                       [1])
              [1])))))

(defcheck solution-9cb4dd5f
  (fn [n]
    (nth
      (iterate #(vec (map + (conj % 0) (cons 0 %))) [1])
      (dec n))))

(defcheck solution-9d15835f
  #(nth (iterate (fn [v]
                   (vec
                     (map + (cons 0 v) (conj v 0))))
          [1])
     (dec %)))

(defcheck solution-9d69ca06
  (fn [index]
    (loop [col [] current 1]
      (if (>  index current)
        (let [
              value (reduce (fn [container it]
                              (let [index (count container)
                                    previous (nth col (dec index))
                                    actual (nth col index 1)
                                    ] (conj container (+ previous actual)))
                              ) [1] col)]
          (recur value (inc current)))

        (conj  col 1)
        ))
    ))

(defcheck solution-9da2b415
  (fn [n]
    (loop [iter n out [1]]
      (if (zero? (dec iter)) out
                             (recur
                               (dec iter)
                               ((fn [x] (cons 1 (concat (map + (rest x) (butlast x)) [1]))) out)
                               )))))

(defcheck solution-9daf5e91
  (fn pasc [n]
    (if (= 1 n) '(1)
                (cons
                  1
                  (map #(apply + %) (partition-all 2 1 (pasc (dec n))))))))

(defcheck solution-9db7bcf7
  (fn pascals- [n]
    ^{:doc "97. Write a function which returns the nth row of Pascal's
  Triangle."}
    (if (= n 1)
      [1]
      (let [p (pascals- (dec n))]
        (concat [1] (map + p (rest p)) [1])))))

(defcheck solution-9e2418b6
  #(reduce (fn [acc fraction] (conj acc (* (peek acc) fraction)))
     [1]
     (for [c (range 1 %)] (/ (- % c) c))))

(defcheck solution-9e3272f5
  (fn nth-row [n]
    (letfn [(next-row [row]
              (let [n-row (reduce (fn [[out l] n]
                                    [(conj out (+ l n)) n])
                            [[] 0]
                            row)]
                (conj (first n-row) (last n-row))))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-9e5ebdee
  (fn [n]
    (letfn [(pt [r c]
              (if (or (< r 2) (< c 2) (> c (dec r)))
                1
                (+ (pt (dec r) (dec c)) (pt (dec r) c))))]
      (for [x (range 1 (inc n))]
        (pt n x)))))

(defcheck solution-9e71ded7
  (fn t [r] (case r
              1 [1]
              2 [1 1]
              (concat [1] (map #(apply + %) (partition 2 1 (t (dec r)))) [1]))))

(defcheck solution-9f06eb61
  (fn pascals-triangle [n]
    (if (= n 1)
      [1]
      (concat [1] (map #(apply + %) (partition 2 1 (pascals-triangle (dec n)))) [1]))))

(defcheck solution-9f45fb5c
  #(reduce (fn [coll _] (loop [r [] c coll] (if (>= (count r) (count c)) (if (empty? r) [1] (into r (reverse (take (count c) r)))) (recur (conj (if (empty? r) [1] r) (apply + (take 2 c))) (next c))))) [] (range %)))

(defcheck solution-9f5c9c5b
  (fn pascal-row [n]
    (if (= n 1) [1]
                (if (= n 2) [1 1]
                            (let [
                                  next-row (fn [row]
                                             (loop [[a b & tail] row, result [1]]
                                               (if (nil? b)
                                                 (conj result 1)
                                                 (recur (cons b tail) (conj result (+ a b))))))]
                              (next-row (pascal-row (dec n))))))))

(defcheck solution-9f922254
  (fn [x]
    (map #(/ (reduce * (range (inc (- (dec x) %)) x)) (reduce * (range 1 (inc %))))
      (range 0 x))))

(defcheck solution-9f9cb1fc
  (fn [n]
    (let [f
          (fn [lst]
            (loop [res [1] [a & r] lst]
              (if (empty? r)
                (conj res a)
                (recur (conj res (+ a (first r)) )  r )  )       )     )

          ]
      (last (take n (iterate f [1]) ))
      )
    ))

(defcheck solution-9fd470bf
  (fn self [n]
    (if (= n 1)
      [1]
      (let [s (self (dec n))]
        (into [] (map + (conj s 0) (into [0] s)))))))

(defcheck solution-9fde6e3d
  (fn [n]
    (cond
      (= n 1) [1]
      :else
      (map first (take n (iterate (fn [[x nm dnm]] [(quot (* x nm) dnm) (dec nm) (inc dnm)]) [1 (dec n) 1]))))))

(defcheck solution-a0443b0f
  #(nth (iterate
          (fn
            [values]
            (loop [l 0 remaining values aggr []]
              (if (empty? remaining)
                (conj aggr 1)
                (recur (first remaining)
                  (rest remaining)
                  (conj aggr (+ l (first remaining))) ))))
          [1]) (dec %)))

(defcheck solution-a0e3523
  (fn [n]
    (nth (iterate
           (fn [p]

             (concat
              [1]
              (map (fn [[f s]] (+ f s)) (partition 2 1 p))
              [1]))
           [1])
      (dec n))))

(defcheck solution-a1077416
  #(->
     (iterate (fn [row] (vec (map + (cons 0 row) (conj row 0)))) [1])
     (nth (dec %))))

(defcheck solution-a1137ef3
  (fn pascal[n]
    (let[f (fn[m]
             (let[k (+ 1 m),
                  kn (- n m)]
               (/ (apply * (range k n))  (apply * (range 1 kn) ) )
               )
             )]
      (map  f (range 0  n)   )
      )

    ))

(defcheck solution-a19ede94
  (fn [n]
    (if (= n 1)
      [1]
      (loop [r [1 1] ax (- n 2)]
        (if (= ax 0)
          r
          (recur (map + (cons 0 r) (concat r [0])) (dec ax))
          )
        )
      )
    ))

(defcheck solution-a1c0c396
  #(
    (fn pascal[n]
      (if (= n 1)
        [1]
        (map + (cons 0 (pascal (dec n))) (reverse (cons 0 (pascal (dec n)))))
        )
      ) %
    ))

(defcheck solution-a1e5de5b
  (fn [n]
    (nth (iterate #(map + `[0 ~@%] `[~@% 0]) [1]) (dec n))))

(defcheck solution-a1f49f8
  #(let [r %
         func (fn
                [s r] (cond
                        (= r (count s)) s
                        (= 1 (count s)) (recur [1 1] r)
                        :else (recur (concat '(1) (map (partial reduce +) (partition 2 1 s)) '(1)) r)))]
     (func [1] r)))

(defcheck solution-a2285bb5
  (fn [n]
    (letfn [(nr [r] (cons (first r)
                      (conj (vec (map #(reduce + %) (partition 2 1 r))) (last r))))]
      (nth (iterate nr [1]) (dec n)))))

(defcheck solution-a2727f0
  (fn [m]
    (let [factorial (fn [n] (if (zero? n) 1 (reduce * (range 1 (inc n)))))
          C (fn [n k] (/ (factorial n) (* (factorial k) (factorial (- n k) ) ) )) ]
      (map #(C (dec m) %) (range m))
      )
    ))

(defcheck solution-a28723b4
  #(loop [c % v []]
     (if (= c 0)
       v
       (recur
         (dec c)
         (vec (cons 1 (map (partial apply +) (partition 2 1 (conj v 0)))))))))

(defcheck solution-a2fa9c2
  (fn p [n]
    (if (= 1 n)
      [1]
      (concat [1]
              (map (fn [[a b]] (+ a b)) (partition 2 1 (p (dec n))))
              [1]))))

(defcheck solution-a33731c0
  (fn [n]
    (letfn [(factorial [n] (apply * (range 1 (inc n))))
            (over [n m] (/ (factorial n) (factorial m) (factorial (- n m))))]
      (for [k (range n)] (over (dec n) k)))))

(defcheck solution-a376e30b
  (fn f [n]
    (if (= 1 n)
      [1]
      (map #(apply + %) (partition 2 1
                          (concat [0]
                                  (f (dec n))
                                  [0]))))))

(defcheck solution-a3ba5519
  (fn pc [n]
    (if (= n 1)
      [1]
      (mapv +
        (conj (pc (- n 1)) 0)
        (into [0] (pc (- n 1)))))))

(defcheck solution-a3c06041
  (fn binomial [n]
    (reductions
      (fn [prev k]
        (* prev (/ (- n k) k)))
      1
      (range 1 n))))

(defcheck solution-a3c41c65
  #(nth
     (iterate
       (fn [s]
         `(1 ~@(map + s (rest s)) 1))
       [1])
     (- % 1)))

(defcheck solution-a3c70b61
  (fn [n] (nth (iterate #(flatten
                           [(first %) (map + (rest %) (drop-last %)) (last %)])
                 [1]) (dec n))))

(defcheck solution-a41d0412
  (fn pas [row]
    (cond (= 1 row) [1]
          (= 2 row) [1 1]
          :else (concat
                 [1]
                 (map #(apply + %)
                   (partition 2 1 (pas (- row 1))))
                 [1]))))

(defcheck solution-a46011fe
  (fn [n]
    (loop [n n r [1]]
      (if (<= n 1)
        r
        (recur (dec n)
          (concat [1]
                  (map #(apply + %) (partition 2 1 r))
                  [1]))))))

(defcheck solution-a473feda
  (fn[n]
    (letfn[(n-k[n k]
             (/ (reduce * (take k (iterate dec n)))
               (reduce * (take k (iterate inc 1)))))]
      (map n-k (repeat (dec n)) (range n)))))

(defcheck solution-a517d81e
  (fn pascal-trangle [n]
    (letfn [(factorial [n]
              (if (= n 0) 1
                          (reduce * (range 1 (inc n)))))
            (binomial-coefficient [n k]
              (/ (factorial n)
                (* (factorial k) (factorial (- n k)))))]
      (map #(binomial-coefficient (dec n) %) (range n)))))

(defcheck solution-a5e71eb4
  (fn iter
    ([n]
     (iter n [1]))
    ([n s]
     (if (= 1 n)
       s
       (recur (dec n)
         (map + (concat [0] s)
           (concat s [0])))))))

(defcheck solution-a63ea6ca
  (fn pascal-nth-row [n]
    (letfn [(lazy-pascal-seq ([x] (letfn [(pascal-row [prevrow]
                                            (concat [1] (map #(apply + %) (partition 2 1 prevrow)) [1]))]
                                    (let [nex (vec (pascal-row x))]
                                      (lazy-seq (cons nex (lazy-pascal-seq nex)))))))]
      (let [lps (concat [[1] [1 1]] (lazy-pascal-seq [1 1]))]
        (last (take n lps))))))

(defcheck solution-a6473e5e
  (fn pascal [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (concat [1] (map #(apply + %) (partition 2 1 (pascal (dec n)))) [1]))))

(defcheck solution-a65903eb
  (fn f [n]
    (if (= n 1) [1]
                (let [last (f (dec n))]
                  (conj (vec (cons 1 (for [i (range (count last)) :when (> i 0)]
                                       (+ (last (dec i)) (last i))))) 1)))))

(defcheck solution-a6674ce
  (fn  [n]
    (let
     [mypascal
      (fn [prev]
        (cond
          (= (count prev) 0) [1]
          (= (count prev) 1) [1 1]
          :d (let [mid (partition 2 1 prev)]
               (flatten [1 (map #(apply + %) mid) 1]))))]
      (nth (iterate mypascal []) n))))

(defcheck solution-a6e63fb5
  (fn [n] (letfn [(helper [l] (let [l1 (map-indexed vector l)]
                                (reverse (cons 1 (reverse  (cons 1 (for [[k1 v1] l1 [k2 v2] (drop 1 l1) :when (= k2 (inc k1))]
                                                                     (+ v1 v2))))))))
                  (pascal [z] (lazy-seq (cons z (pascal (helper z)))))
                  (modify [t] (if (number? t)  #(nth % (dec t)) (partial take (last t))))] ((modify n) (pascal (list 1))))))

(defcheck solution-a72d26c
  (fn pascal [n]
    (loop [stop n
           n 1
           prev nil]
      (if (> n stop)
        prev
        (let [newl (cond
                     (= 1 n) [1]
                     (= 2 n) [1 1]
                     :else (conj (seq (conj (vec (map + (butlast prev) (rest prev))) 1)) 1))]
          (recur stop (inc n) newl))))))

(defcheck solution-a74463bf
  (fn [n]
    (reductions *
      1
      (map #(/ (- n %) %)
        (range 1 n)))))

(defcheck solution-a7d4c984
  (fn [c] (nth (iterate (fn [c] `(1 ~@(map + (rest c) (drop-last c)) 1)) '(1)) (dec c))))

(defcheck solution-a841f4e2
  (fn o [n]
    (letfn [(t [l]
              (concat
               [1]
               (map +
                 (next l)
                 (butlast l))
               [1]))]
      (if (= 1 n)
        [1]
        (t (o (dec n)))))))

(defcheck solution-a8597bcc
  #(nth (iterate (fn [v] (map + (conj v 0) (reverse (conj v 0)))) [1]) (dec %)))

(defcheck solution-a8618773
  (fn [n]
    (last (take n (iterate
                    (fn [s] (flatten
                              [1 (map #(reduce + %) (partition 2 1 s)) 1])
                      ) [1])))))

(defcheck solution-a88ba7d7
  (fn [n]
    (nth (iterate (fn [s]
                    (let [p (partition 2 1 s)
                          added (map (partial apply +) p)]
                      (concat [1] added [1])))
           [1])
      (dec n))))

(defcheck solution-a8ab51cc
  (fn pascal [n]
    (loop [row [1] x 1]
      (if (= x n)
        row
        (recur (concat [1] (map #(apply + %) (partition 2 1 row)) [1]) (inc x))))))

(defcheck solution-a95e511a
  (fn [n]
    (let [init [1]
          r (range 1 n)]
      (reduce (fn [acc x]
                (concat [1] (map + (rest acc) (reverse (rest acc))) [1]))
        init r))))

(defcheck solution-aa344760
  (fn pascal-row [n]
    (reduce #(conj %1 (int (* %2 (last %1))))
      [1] (for [k (range 1 n)]
            (/ (- n k) k)))))

(defcheck solution-aac21bb4
  (fn pt [n]
    (if (= 1 n)
      [1]
      (let [xs (pt (dec n))]
        (vec
          (map + (cons 0 xs)
            (conj xs 0)))))))

(defcheck solution-aaca40d6
  (fn pascal-triangle [line]
    (cond
      (= line 1) [1]
      (= line 2) [1 1]
      :else
      (flatten [1 (map #(reduce + %) (partition 2 1 (pascal-triangle (dec line)))) 1]))))

(defcheck solution-ab982966
  (fn [x] (letfn [(cell [r c pv]
                    (* pv (/ (- r c) c)))]
            (map #(second %) (take x (iterate #(list (inc (first %)) (cell x (first %) (second %))) '(1 1)))))))

(defcheck solution-ac7b2a1c
  (fn p-row [n]
    (let [n (dec n)]
      (apply vector
        (map
          (fn binomial-coefficient [n k]
            (let [rprod (fn [a b] (reduce * (range a (inc b))))]
              (/ (rprod (- n k -1) n) (rprod 1 k))))
          (repeat n)
          (range 0 (inc n)))))))

(defcheck solution-ad1bfe6d
  (fn pascal-triangle2 [n]
    (letfn [(return-vals [l p]
              (if (< (dec p) 0)
                [(l p)]
                (if (>= p (count l))
                  [(l (dec p))]
                  [(l p) (l (dec p))])))
            ]
      (loop [coun 1
             prev [1]]
        (if (= n coun)
          prev
          (let [act (mapv #(apply + (return-vals prev %1)) (range (inc coun)))]
            (recur (inc coun) act)))))))

(defcheck solution-ad4bc560
  (fn [n] (nth (iterate #(vec (map + (into [0] %) (into % [0]))) [1]) (dec n))))

(defcheck solution-ad814c59
  (fn P1 [n]

    (cond
      (= n 1) [1]
      :default
      (vec (concat [] (reduce #(conj (vec (drop-last %1)) (+ (last %1) %2) %2) [0] (P1 (- n 1))))

        ))

    ))

(defcheck solution-ad9b1261
  #(loop [i 1 v '(1)] (if (>= i %) v (recur (inc i) (map + (conj v 0) (into '(0) v) )))))

(defcheck solution-adb93dd9
  (fn row [n]
    (if (= n 1) [1]
                (let [prev (row (- n 1))]
                  (flatten [1 (map (comp #(apply + %) list) prev (rest prev)) 1])))))

(defcheck solution-add812b1
  (fn [n]
    (let [j (fn [xs] (concat '(1) (map #(apply + %) (partition 2 1 xs )) '(1)))]
      (nth (lazy-seq (cons [] (iterate j [1])))
        n))))

(defcheck solution-adf34f8b
  #(loop [n % row [1]]
     (if (= 1 n)
       row
       (recur (dec n) (concat [1] (map + row (rest row)) [1])))))

(defcheck solution-adf51c64
  (fn pascal [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (let [prev (pascal (dec n))]
              (vec (concat [1] (map + (butlast prev) (rest prev)) [1]))))))

(defcheck solution-adf7e3e2
  (fn[n](let [c (range 1 n) v (conj (take n (map / (map #(- n %) c) c)) 1)] (map #(int (reduce * (take % v))) (range 1 (inc n))))))

(defcheck solution-ae493410
  (fn [n] (if (= n 1) [1] (nth (iterate #(concat [1] (map + % (drop 1 %)) [1]) [1 1]) (- n 2)) )))

(defcheck solution-aebd3a9f
  (fn [c]  (nth    (iterate      #(map + `[0 ~@%] `[~@% 0]) [1])  (- c 1))))

(defcheck solution-aec958a9
  (fn [x]
    (let [e (fn [c] (conj (map #(apply + %) (partition-all 2 1 c)) 1))]
      (nth (iterate e '(1)) (dec x)))))

(defcheck solution-af0b192c
  (fn [n]
    (nth
      (iterate #(concat [1] (map + % (rest %)) [1]) [1])
      (dec n))))

(defcheck solution-af2a1ee6
  (fn pascal[n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (conj (first
                        (reduce (fn[[r l] x] [(conj r (+ l x)) x]) [[] 0] (pascal(dec n))))
                  1))))

(defcheck solution-af76c8dd
  (fn row [n] (if (= 1 n) [1] (concat [1]
                                      (#(mapv + (butlast %) (rest %)) (row (dec n)))
                                      [1]))))

(defcheck solution-affe4781
  (fn pascals-triangle
    [n]
    (letfn [(f [row rem-iterations]
              (if (> rem-iterations 1)
                (recur
                  (concat [1] (map
                                #(+ (nth row %) (nth row (inc %)))
                                (range 0 (dec (count row)))) [1]) (dec rem-iterations))
                row))]
      (f [1] n))))

(defcheck solution-b02c6f19
  (fn p [n]
    (if (= 1 n)
      [1]
      (let [a (p (dec n))]
        (conj (vec (map + (cons 0 a) a)) 1)))))

(defcheck solution-b03a40f5
  (fn [x]
    (letfn
     [(factorial [a] (apply * (range 1 (inc a))))
      (ptel [n k]
        (/ (factorial n) (* (factorial k) (factorial (- n k)))))]
      (map #(ptel (dec x) %) (range x)))))

(defcheck solution-b05d714c
  (fn[x]
    (letfn [(fact[n] (apply * (range 1 (inc n))))
            (pascal[n k] (/ (fact n) (* (fact k)  (fact (- n k)))))]
      (map (partial pascal (dec x)) (range 0 x)))))

(defcheck solution-b08066a0
  (let [mypa (fn mypas [y] (concat
                            ((fn rrr [x]
                               (if (empty? x)
                                 []
                                 (if (empty? (rest x))
                                   [1]
                                   (concat (rrr (drop-last x))
                                           (list (+ (last x)
                                                    (last (drop-last x))))
                                           ))))
                             y) (list 1)))]

    (fn mypastri
      ([x] (mypastri x []))
      ([x y]
       (if (= x 1)
         (mypa y)
         (mypastri (- x 1) (mypa y)))))))

(defcheck solution-b0b9160d
  #(loop [i 0, result []]
     (if (= i %)
       result
       (recur
         (inc i)
         (loop [in result, out [1]]
           (if (< (count in) 2)
             (concat out in)
             (recur
               (rest in)
               (conj out (apply + (take 2 in))))))))))

(defcheck solution-b10db402
  (fn [r]
    (loop [c 1, v [1]]
      (if (= r c)
        v
        (recur (inc c) (conj v (* (last v) (/ (- r c) c))))
        ))))

(defcheck solution-b10dd13e
  (fn [row]
    (loop [n 3
           out [1 1]]
      (condp = row
        0 nil
        1 [1]
        2 [1 1]
        (if (> n row)
          out
          (recur
            (inc n)
            (vec
              (flatten
                [1 (butlast (for [x (range (count out))] (if (< x (- (count out) 1)) (+ (get out x) (get out (+ x 1)))))) 1]))))))))

(defcheck solution-b122b837
  (fn [n]
    (let [f (fn [n] (apply * (range 1 (inc n))))
          c (fn [n j] (/ (f n) (f j) (f (- n j))))]
      (map (partial c (dec n)) (range n)))))

(defcheck solution-b16c269a
  (fn [n]
    (nth
      (iterate (fn [x]
                 (map #(+ % %2)
                   (concat [0] x)
                   (concat x [0]))) [1])
      (dec n))))

(defcheck solution-b1daeb66
  (fn [r]
    (nth
      (iterate #(map +
                  (concat [0] %)
                  (concat % [0]))
        [1])
      (dec r))))

(defcheck solution-b2014f95
  (fn pascal [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (concat [1]
                    (map #(apply + %1) (partition 2 1 (pascal (dec n))))
                    [1]))))

(defcheck solution-b2368fb7
  (fn f
    [row]
    (let [pascal (iterate #(concat [1]
                                   (map + % (rest %))
                                   [1])
                   [1])]
      (last (take row pascal)))))

(defcheck solution-b2583aff
  (fn [n]
    (letfn [(pt-next [coll]
              (let [coll-part (partition 2 1 coll)]
                (conj (reduce (fn [acc pair]
                                (conj acc (apply + pair)))
                        [1]
                        coll-part) 1)))]
      (first (take 1 (drop (dec n) (iterate pt-next [1])))))))

(defcheck solution-b267b500
  (fn [n] (last (take n (iterate (fn [m] (vec (concat [1] (vec (map #(+ (first %) (last %)) (partition 2 1 m)))  [1]))) [1])))))

(defcheck solution-b2db273d
  (fn pascal [n]
    (case n
      1 [1]
      2 [1 1]
      (let [pre (pascal (- n 1))
            left (drop-last 1 pre)
            right (drop 1 pre)]
        (conj (vec (conj (map + left right) 1)) 1))
      )))

(defcheck solution-b35742cb
  (fn  [n]
    (loop [i 0
           last-row [1]]
      (if (<= n (inc i))
        last-row
        (let [new-row (loop [j 0
                             row []]
                        (cond
                          (< i (dec j)) row
                          (zero? j) (recur (inc j) (conj row (first last-row)))
                          (> j i) (recur (inc j) (conj row (last last-row)))
                          :else (recur (inc j) (conj row (+ (nth last-row j) (nth last-row (dec j)))))))]

          (recur (inc i) new-row))))))

(defcheck solution-b39e78e5
  (fn [a]
    (reduce
      (fn [x i] (concat [1]
                        (map (fn [[a b]] (+ a b))
                          (partition 2 1 x))
                        [1])) [1] (range (dec a)))))

(defcheck solution-b3b0731f
  #(reduce (fn [n _] (concat [1]
                             (map (partial apply +)
                               (partition 2 1 n))
                             [1])) [1] (range 1 %)))

(defcheck solution-b43320a2
  (fn [n]
    (let [n (dec n)]
      (letfn [(! [x] (reduce * (range x 1 -1)))
              (choose-n [k]
                (/ (! n)
                  (* (! k)
                    (! (- n k)))))]
        (mapv choose-n (range (inc n)))))))

(defcheck solution-b4e2fecc
  (fn pascal [n] (if (= n 1) [1]
                             (let [prev (pascal (dec n))]
                               (reduce #(conj %1 (apply + %2)) []
                                 (partition 2 (interleave (conj prev 0) (cons 0 prev))))))))

(defcheck solution-b55195a5
  (fn pascal [row]
    (loop [v 1 r row c 1 acc []]
      (if (= (count acc) row)
        acc
        (recur (* v (/ (- r c) c)) r (inc c) (conj acc v))))))

(defcheck solution-b56dc7cc
  (fn number97 [n]
    (let [pascal (iterate #(concat [1]
                                   (map + % (rest %))
                                   [1])
                   '(1))]
      (nth pascal (dec n)))))

(defcheck solution-b57572a4
  (fn [n]
    (nth (iterate
           (fn [n]
             (concat
              [1]
              (map #(apply + %)
                (partition 2 1 n))
              [1])) [1]) (dec n))))

(defcheck solution-b5bbcbad
  (fn pascal-triangle
    ([vec-size]
     (pascal-triangle vec-size [1] [1] 1))
    ([vec-size previous now pointer]
     (if (= vec-size (count now))
       now
       (if (= (dec (count now)) (count previous))
         (recur vec-size now [1] 1)
         (if (= (count previous) (count now))
           (recur vec-size previous (conj now 1) (inc pointer))
           (recur vec-size previous (conj now (+ (get previous pointer) (get previous (dec pointer)))) (inc pointer))))))))

(defcheck solution-b5d16e66
  (fn pt [n]
    (if (= n 1)
      [1]
      (let [t (pt (dec n))]
        (vec (map + (conj t 0) (cons 0 t)))))))

(defcheck solution-b60169d0
  (fn tri [n] (if (= n 1) [1] (vec (concat [1] (map #(+ (first %) (second %)) (partition 2 1 (tri (dec n)))) [1])) )))

(defcheck solution-b62192d5
  (fn [n]
    (let [iter (fn [r]
                 (loop [s r v [1]]
                   (if (empty? (rest s))
                     (conj v 1)
                     (recur (rest s) (conj v (+ (first s)
                                                (second s)))))))]
      (nth (iterate iter [1]) (dec n)))))

(defcheck solution-b681157c
  #(loop [n 1 row [1]]
     (if (= n %)
       row
       (recur (inc n) `[1 ~@(map (partial apply +) (partition 2 1 row)) 1]))))

(defcheck solution-b686e46f
  (fn [n]
    (first
      (drop (dec n)
        (iterate
          (fn [v]
            (vec (cons 1
                   (map (fn [[x y]] (if (nil? y) x (+ x y)))
                     (partition-all 2 1 v)))))
          [1])))))

(defcheck solution-b6ebf49b
  (fn pascal [n1]
    (let [n (dec n1)
          binomial-coefficient
            (fn [n k]
              (let [rprod (fn [a b] (reduce * (range a (inc b))))]
                (/ (rprod (- n k -1) n) (rprod 1 k))))]
      (map (partial binomial-coefficient n) (range (inc n))))))

(defcheck solution-b6f2fe1a
  #(nth
     (iterate (fn [x] (mapv + `[0 ~@x] `[~@x 0])) [1])
     (dec %)))

(defcheck solution-b7071ae6
  (fn [n]
    (let [n (dec n)]
      (letfn [(choose [n k]
                (int
                  (reduce
                    (fn [acc i] (* acc (/ (+ n (- 1 i)) i)))
                    1
                    (range 1 (inc k)))))]
        (map (partial choose n) (range (inc n)))))))

(defcheck solution-b7c17c6
  #(loop [i % p [1]]
     (if (= 1 i)
       p
       (recur (- i 1) (map + (conj p 0) (into () (conj p 0))))
       )))

(defcheck solution-b7cd6db2
  (fn [n]
    (->
      #(map (fn [[x y]] (+ x y)) (partition 2 1 (concat [0] % [0])))
      (iterate '(1))
      (nth (dec n)))))

(defcheck solution-b7f9078e
  (fn pasc [& xs]
    (letfn [(bi-co [r n]
              (letfn [(! [z] (reduce * (take z (drop 1 (range)))))]
                (/ (! n) (* (! r) (! (- n r))))
                )
              )]
      (flatten (map
                 (fn [x]

                   (map
                     (fn [i]
                       (bi-co (first i) (second i))
                       )
                     (map-indexed vector (repeat x (- x 1)))
                     )
                   )
                 xs
                 ))
      )
    ))

(defcheck solution-b8000c16
  (fn pascal [n]
    (case n
      1 [1]
      2 [1 1]
      (let [previous-triangle (pascal (dec n))]
        (concat
         [1]
         (map (fn [[x y]] (+ x y)) (partition 2 1 previous-triangle))
         [1])))))

(defcheck solution-b84f5740
  #(nth (iterate (fn [prev] (mapv + (concat [0] prev) (concat prev [0]))) [1]) (dec %)))

(defcheck solution-b92636bd
  #(reduce (fn [a x] (conj a (* (last a) (/ (- % x) x)))) [1] (range 1 %)))

(defcheck solution-b93ff52c
  (fn [n]
    (letfn [ (pascal-triangle-next [lst]
               (let [lst-with-i (zipmap (range (count lst)) lst)]
                 (->> (for [[k v] lst-with-i]
                        (for [ i (range (inc (count lst-with-i)))]
                          (if (or (= k i) (= (inc k)  i))
                            v 0)))
                   (apply map  +)))) ]
      (if (< n 0) nil
                  (loop [n n lst [1]]
                    (if (= n 1)
                      lst
                      (recur (dec n) (pascal-triangle-next lst))))))
    ))

(defcheck solution-b9468e9d
  (fn pascal-tri
    ([]
     (iterate #(concat [1] (map + % (rest %)) [1])
       [1]))
    ([n] {:pre [(integer? n), (pos? n)]}
     (nth (pascal-tri) (dec n)))))

(defcheck solution-ba510e05
  (fn [x]
    (loop [cnt x
           result [1]]
      (if (> cnt 1)
        (recur (dec cnt)  (vec (flatten [1 (map #(apply + %)(partition 2 1 result)) 1])))
        result
        ))))

(defcheck solution-ba54f188
  (fn [x] (nth (iterate  #(mapv + (conj % 0) (into [0] %)) [1]) (dec x))))

(defcheck solution-bab9c144
  (fn [x] (nth (iterate #(mapv + (conj % 0) (cons 0 %)) [1]) (dec x))))

(defcheck solution-bb8c3358
  (fn [x]
    (let [tmp (vec (for [i (range 1 (inc (Math/ceil (/ x 2))))]
                     (/ (reduce * (map #(- x %) (range 1 i))) (reduce * (range 1 i)))))]
      (concat tmp (reverse (subvec tmp 0 (Math/floor (/ x 2))))))))

(defcheck solution-bba703e7
  (fn [r] (reduce (fn [vals c] (conj vals (if (zero? c) 1 (* (last vals) (/ (- r c) c))))) [] (range 0 r))))

(defcheck solution-bc6d01f
  #(nth (iterate (fn [s] (concat [1] (for [x (partition 2 1 s) ] (apply + x) ) [1])) [1] ) (dec %) ))

(defcheck solution-bc705029
  (fn [n]
    (let [next-pascal
                 (fn [vs]
                   (if (empty? vs)
                     [1]
                     (let [xs (conj vs 0), ys (reverse xs)]
                       (into [] (map (fn [x y] (+ x y)) xs ys)))))
          pascal (iterate next-pascal [])]
      (nth pascal n))))

(defcheck solution-bcbcede9
  (fn [n] (nth  (iterate #(map (partial apply  +) (partition 2 1 (concat [0] % [0]))) [1]) (dec n))))

(defcheck solution-bcc5d44
  (fn [n]
    (first (drop (dec n)
             ((fn pascal-tr [line]
                (lazy-seq (cons line (pascal-tr (vec (concat [1]
                                                             (mapv #(reduce + %) (partition 2 (drop-last (rest (flatten (map #(repeat 2 %) line))))))
                                                             [1]))))))
              [1])))))

(defcheck solution-bcf8d026
  (fn [n]
    (loop [res [1]]
      (if (-> res count (= n))
        res
        (recur (->> (partition 2 1 res)
                 (map #(reduce + %))
                 (cons 1)
                 reverse
                 (cons 1)))))))

(defcheck solution-bd3b9285
  (fn ptr [n]
    (if (= n 1)
      [1]
      (let [p (ptr (dec n))]
        (concat [1]
                (for [[a b] (partition 2 (interleave p (rest p)))] (+ a b))
                [1])))
    ))

(defcheck solution-bd903377
  (fn [n]
    (nth
      (iterate
        #(map
           (partial apply +)
           (partition 2 1 (concat [0] % [0])))
        [1])
      (dec n))))

(defcheck solution-bdf88bb4
  (fn [n]
    (loop [r [1] i 1]
      (if (< i n)
        (recur
          (map #(apply + %) (partition-all 2 1 (cons 0 r)))
          (inc i))
        r))))

(defcheck solution-be42fbfe
  (fn triangle [n]
    (if (= 1 n)
      (list 1)
      (let [previous-row (triangle (- n 1))
            inside (map + previous-row (rest previous-row))]
        `(1 ~@inside 1)))))

(defcheck solution-bec4316c
  (fn [n]
    (loop [t [1] n n]
      (if (= 1 n) t
                  (recur (map (partial apply +)
                           (partition 2 1 (concat [0] t [0])))
                    (dec n))))))

(defcheck solution-beca2208
  (fn pascal-triangle
    [n]
    (loop [i 1 rvec [[1]]]
      (if (= i n)
        (get rvec (dec n))
        (recur (inc i) (conj rvec (vec (map + (conj (last rvec) 0) (cons 0 (last rvec))))))))))

(defcheck solution-bf1b539d
  (fn [n]
    (let [fact (fn [n]
                 (loop [acc 1, n n]
                   (if (zero? n)
                     acc
                     (recur (* acc n) (dec n)))))
          binomial (fn [n r]
                     (/ (fact n) (* (fact r) (fact (- n r)))))]
      (map #(binomial (dec n) %) (range n)))))

(defcheck solution-bf1cac95
  (fn pascal-row [n]
    (let [r (dec n)
          pascal (fn pascal [n k]
                   (cond
                     (= k 0) 1
                     (= k n) 1
                     :else (+ (pascal (dec n) (dec k)) (pascal (dec n) k))
                     )
                   )]
      (map #(pascal r %) (range n))
      )
    ))

(defcheck solution-bf323166
  (fn pas [n]
    (if (<= n 2)
      (repeat n 1)
      (concat [1]
              (map + (pas (dec n)) (next (pas (dec n))))
              [1]))))

(defcheck solution-bf4ee77f
  (fn [row]
    (loop [i 1, res [1]]
      (if (= i row)
        (vec res)
        (let [middle
              (for [[x y] (partition 2 1 res)]
                (+ x y))]
          (recur (inc i) (concat [1] middle [1])))))))

(defcheck solution-bf79e201
  (fn pascal
    [i]
    (if (= 1 i)
      [1]
      (concat
       [1]
       (map
         #(apply + %)
         (partition 2 1
           (pascal (dec i))))
       [1]))))

(defcheck solution-bfa965d0
  (fn pascal-triangle [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      (> n 2) (flatten [1 (map (partial apply +) (partition 2 1 (pascal-triangle (dec n)))) 1]))))

(defcheck solution-bfabaeb3
  (fn [n]
    (letfn [(next-pascal [coll]
              (concat [1]
                      (map (partial apply +) (partition 2 1 coll))
                      [1]))]
      (nth (iterate next-pascal [1]) (dec n)))))

(defcheck solution-bfd8fd21
  (fn [n]
    (loop [n n pas [1]]
      (if (= 1 n)
        pas
        (recur (- n 1)
          ((fn [a]
             (loop [a a p [1]]
               (if (= (count a) 1)
                 (conj p 1)
                 (recur (rest a) (conj p (+ (first a) (second a))))))) pas))))))

(defcheck solution-c0575df1
  (fn pascal [n]
    (let
     [line-builder (fn [line]
                     (concat [1] (for [pair (partition 2 1 line)]
                                   (apply + pair)) [1]))]
      (last (take n (iterate line-builder [1]))))))

(defcheck solution-c06cd81c
  (fn [n]
    (let [next-row (fn [v]
                     (into [] (concat [1]
                                      (for [i (range (dec (count v)))]
                                        (+ (get v i) (get v (inc i))))
                                      [1])))]
      (last (take n (iterate next-row [1]))))))

(defcheck solution-c0d201de
  (fn [n]
    (nth (iterate #(map + (concat [0] %) (concat % [0])) [1]) (dec n))))

(defcheck solution-c0f1b19e
  (fn pt [n]
    (condp = n
      1 [1]
      2 [1 1]
      (loop [i 2
             row [1 1]]
        (if (= n i)
          row
          (let [new-row (concat [1] (map (partial reduce +) (partition 2 1 row)) [1])]
            (recur (inc i) new-row)
            ))))))

(defcheck solution-c11a51a0
  (fn p [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (concat [1] (map + (drop 1 (p (- n 1))) (p (- n 1))) [1]) )))

(defcheck solution-c1399da5
  (fn[n](first (loop[i 0 acc []] (if(= i n) acc (if (zero? i) (recur (inc i) (cons [1] acc)) (if (= i 1) (recur (inc i) (cons [1 1] acc)) (recur (inc i) (cons (cons 1 (reverse (cons 1 (#(for[i (range (- (count %) 1))] (+ (nth % i) (nth % (+ i 1)))) (first acc))))) acc)))))))))

(defcheck solution-c149d852
  (fn [n]
    (nth (iterate
           #(map + (conj (vec %) 0)
              (into [0] %))
           [1])
      (- n 1))))

(defcheck solution-c1758c8d
  (fn tr[n] (nth (iterate #(conj (into [1] (map + % (next %))) 1) [1]) (dec n))))

(defcheck solution-c1a1f9f3
  (fn [x]
    (let [numer (fn [n k] (apply * (take-last k (range 1 n))))
          denom (fn [n k] (apply * (take k      (range 1 n))))
          n-choose-k (fn [n k] (if (or (= k 0) (= n 0)) 1 (/ (numer n k) (denom n k))))]
      (map #(n-choose-k x %) (range x))
      )
    ))

(defcheck solution-c2a0916a
  (fn [n]
    (loop [res [1]]
      (if (= (count res) n)
        res
        (recur (concat [1]
                       (for [i (range (count (butlast res)))]
                         (+ (nth res i) (nth res (inc i))))
                       [1]))))))

(defcheck solution-c2bba304
  (fn [n]
    (reduce (fn [res k] (conj res (* (last res) (/ (- n k)k))))
      [1]
      (range 1 n) )))

(defcheck solution-c2c97874
  (fn [num]

    (loop [n 1 result [] ]
      (let [current ((fn [n coll]
                       (let [left (count coll) right (+ left n) bound (dec right)] (for [i (range left right)]
                                                                                     (cond
                                                                                       (zero? (- i left)) 1
                                                                                       (= bound i) 1
                                                                                       :else (let [lvalue  (- i n) rvalue (inc lvalue)] (+ (nth coll lvalue) (nth coll rvalue)) )
                                                                                       )
                                                                                     ))) n result )]
        (if (= n num) current
                      (recur (inc n) (concat result current))))
      )
    ))

(defcheck solution-c307da90
  (fn triangle-row [n]
    (letfn [(fac [n] (if (<= n 1) 1 (* n (fac (dec n)))))
            (ncr [n r] (/ (fac n)
                         (* (fac (- n r))
                           (fac r))))]
      (vec (map #(ncr (dec n) %)
             (range n))))))

(defcheck solution-c352b5ce
  (fn pascal [n]
    (let [pairwise
          (fn [xs acc]
            (if (>= (count xs) 2)
              (recur (rest xs) (conj acc [(first xs) (second xs)]))
              acc))]
      (condp = n
        1 [1]
        (map
          #(apply + %)
          (pairwise (concat [0] (pascal (dec n)) [0]) []))))))

(defcheck solution-c35fe5d0
  (fn pascal-triangle [row-number]
    (loop [curr-row-number 0
           curr-row [1]]
      (if (= (inc curr-row-number) row-number)
        curr-row
        (recur (inc curr-row-number)
          (vec (for [x (range 0 (+ 2 curr-row-number))]
                 (if (or (= x 0) (= x (inc curr-row-number)))
                   1
                   (+ (nth curr-row x) (nth curr-row (dec x)))))))))))

(defcheck solution-c39ed11a
  (fn [n] (nth (iterate #(map + `[0 ~@%] `[~@% 0]) [1]) (dec n))))

(defcheck solution-c3d84008
  (fn p [i]
    (if (= i 1) [1]
                (map
                  #(apply + %)
                  (cons [0 1]
                    (partition 2 1 [0]
                      (p (dec i))))))))

(defcheck solution-c3e204d3
  (fn [n] (nth (iterate #(map (partial apply +) (partition 2 1 (concat [0] % [0]))) [1]) (dec n))))

(defcheck solution-c40502ad
  (letfn
   [(pascal []
      (iterate #(map +
                  (cons 0 %)
                  (concat % [0]))
        [1]))]

    #(nth (pascal) (dec %))))

(defcheck solution-c4092265
  (fn [n]
    (last
      (loop [i 1 tri [[1]]]
        (if (= i n) tri
                    (recur (inc i)
                      (conj tri (vec
                                  (map (partial apply +)
                                    (partition 2 1 (concat [0] (last tri) [0])))))))))))

(defcheck solution-c47911a2
  (fn  [num]
    (nth (take num  (iterate #(concat [1]
                                      (map + % (rest %))
                                      [1])
                      [1])) (dec num))))

(defcheck solution-c4889a6a
  (fn pascal[n]
    (reductions #(* %1
                   (/ (- n
                         %2)
                     %2)) 1 (range 1 n)) ))

(defcheck solution-c4fd8e9c
  (fn [n]
    (nth
      (iterate
        (fn [xs] (cons 1 (map #(apply + %) (partition-all 2 1 xs))))
        [1])
      (dec n))))

(defcheck solution-c568409
  (fn [x]
    (letfn [
            (pas [n]
              (if (= n 1)
                [1]
                (map + (concat [0] (pas (- n 1)))
                  (concat (pas (- n 1)) [0]))))]
      (pas x))))

(defcheck solution-c5707115
  (fn [n]
    (into []
      (keep-indexed #(when (and (not= %1 0) (not= %1 (inc n))) %2)
        (last
          (take n
            (iterate (fn [args]
                       (conj
                         (into []
                           (cons 0
                             (map #(apply + %) (partition 2 1 args))))
                         0)) [0 1 0])))))))

(defcheck solution-c57c4d8e
  (fn nthpascal [i] (nth ((fn mypascal [i] (lazy-seq (cons i (mypascal (if (< 1 (count i))(vec (flatten (list (first i) (map #(reduce + %) (partition 2 1 i)) (last i)))) (vec (concat i i))))))) [1]) (dec i))))

(defcheck solution-c5d6d896
  (fn [n]
    (loop [row [1] cnt n]
      (if (= cnt 1)
        row
        (recur (concat [1] (map #(apply + %) (partition 2 1 row)) [1]) (dec cnt))))))

(defcheck solution-c5e33ff4
  (fn [n]
    (loop [r [1 2 1] c n]
      (cond
        (<= c 2) (repeat c 1)
        (= c 3) r
        :else
        (recur
          (conj (vec (cons 1 (map #(apply + %) (partition 2 (interleave (drop-last 1 r) (rest r)))))) 1) (dec c))))))

(defcheck solution-c6294f45
  (fn [n] (nth (iterate #(map + `(0 ~@%) `(~@% 0)) [1]) (dec n))))

(defcheck solution-c66548bb
  (fn [n]
    (letfn [(next-row [r]
              (condp = r
                []  [1]
                [1] [1 1]
                (vec (concat [1] (map #(apply + %) (partition 2 1 r)) [1]))))]
      (nth (iterate next-row []) n))))

(defcheck solution-c66c21b6
  (fn
    [n]
    (let* [n (dec n)
           factorial (fn [n] (apply * (range 1 (inc n))))
           choose    (fn [i j]
                       (/ (factorial i) (* (factorial j) (factorial (- i j)))))]
      (for [k (range 0 (inc n))]
        (choose n k)))))

(defcheck solution-c6816bd9
  (fn pasc-row [n]
    (if (= n 1) '(1)
                (let [prev-row (pasc-row (dec n))]
                  (concat '(1) (map + (drop 1 prev-row) (take (dec n) prev-row)) '(1))))))

(defcheck solution-c71394aa
  (fn pascal [n]
    (letfn [(comb [n k]
              (reduce #(/ (* %1 (+ %2 (- n k))) %2)
                1 (range 1 (inc k))))]
      (map (partial comb (dec n)) (range 0 n)))))

(defcheck solution-c7bb847a
  (fn f [n]
    (nth (iterate #(concat [1] (map + % (rest %)) [1]) [1]) (dec n))))

(defcheck solution-c8ca7036
  (fn
    [n]
    (loop [k (- n 1) i 0 row []]
      (if (<= i k)
        (recur k (inc i) (conj row (/ ((fn f
                                         [num]
                                         (if (= num 0)
                                           1
                                           (* num (f (dec num))))
                                         ) k) ((fn f
                                                 [num]
                                                 (if (= num 0)
                                                   1
                                                   (* num (f (dec num))))
                                                 ) i) ((fn f
                                                         [num]
                                                         (if (= num 0)
                                                           1
                                                           (* num (f (dec num))))
                                                         ) (- k i)))))
        row) )
    ))

(defcheck solution-c930d3fc
  (fn pascal [n] (cond (= n 1) [1] (= n 2) [1 1] :else (concat [1] (map (fn [xs] (apply + xs)) (partition 2 1 (pascal (dec n)))) [1]))))

(defcheck solution-c98f5692
  #(last (take % (iterate (fn [row] (map + (concat row [0]) (cons 0 row))) [1]))))

(defcheck solution-c9901615
  (let
   [nextrow
    (fn [row]
      (vec (concat [1] (map + row (next row)) [1])))
    ]
    (fn [n] (nth (iterate nextrow [1]) (dec n) ))))

(defcheck solution-c993d3fc
  (fn p [r]
    (if (= r 1)
      [1]
      (let [ar (p (- r 1))]
        (concat [1] (map #(apply + %) (partition 2 1 ar)) [1])))))

(defcheck solution-c9ae5eda
  (fn [n]
    (loop [x 1 y [1]]
      (if (= x n) y
                  (recur (inc x)
                    (concat (first(reduce #(list (concat(first %1)[(+ %2 (last %1))]) %2) [[] 0] y))
                            [1])
                    )
                  )
      )
    ))

(defcheck solution-c9c05dfb
  (fn pascal-tri-row [row]
    (letfn [(factorial [n]
              (if (zero? n) 1
                            (reduce * (range 1 (inc n)))))
            (binomial [n k]
              (/ (factorial n) (* (factorial k) (factorial (- n k)))))]
      (map (partial binomial (dec row)) (range row)))))

(defcheck solution-c9e36eb6
  #(letfn [(f [n] (loop [r 1 c 1]
                    (if (> c n)
                      r
                      (recur (* r c) (inc c)))))
           (c [n k] (/ (f n) (* (f k) (f (- n k)))))]
     (for [i (range %)]
       (c (dec %) i))))

(defcheck solution-ca3b420
  (fn pascal [i]
    (if (= i 1) [1]
                (vec (map + (conj (pascal (dec i)) 0)
                       (concat [0] (pascal (dec i))))))))

(defcheck solution-caa5fdb2
  (fn pascal-triangle
    [n]
    (letfn [(pascal
              [n current]
              (if (= n 1)
                current
                (pascal (dec n) (concat [1]
                                        (map + (butlast current) (next current))
                                        [1]))))]
      (pascal n [1]))))

(defcheck solution-caa721b1
  (fn [x] (nth (iterate #(vec (cons 1 (concat (map (fn [c] (apply + c)) (partition 2 1 %)) [1]))) [1]) (dec x))))

(defcheck solution-caff1ecb
  (fn [n] (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) [1]) (dec n))))

(defcheck solution-cb1a44a7
  (letfn  [(pascal [n]
             (cond (= n 1) [1] (= n 2) [1 1]
                   :else
                   (let [sum-prev (->> n dec pascal (partition 2 1) (map #(reduce + %)))]
                     (->  sum-prev (conj 1) vec (conj 1))))
             )] pascal))

(defcheck solution-cb6f8abb
  (fn [n]
    (letfn [(go [p ms rs]
              (if (empty? ms)
                (conj rs p)
                (go (first ms)
                  (rest ms)
                  (conj rs (+ p (first ms))))))
            (n-row [ns]
              (go 0 ns []))]
      (last (take n (iterate n-row [1]))))))

(defcheck solution-cbd1d247
  (fn pascal
    [n]
    (loop [n (dec n)
           curr 0
           result [1]]
      (if (= curr n)
        result
        (recur n (inc curr) (conj result (/ (* (last result) (- n curr)) (inc curr))))))))

(defcheck solution-cbdab160
  (fn pt [x] (if (= 1 x) [1] (concat [1] (map #(apply + %) (partition 2 1 (pt (dec x)))) [1] ))))

(defcheck solution-cbf44268
  (fn [n] (loop [i 2 r [[1] [1 1]]]
            (cond
              (<= n 0) []
              (=  n 1) [1]
              (<= n i)  (last r)
              :else    (recur (inc i) (conj r (let [c (last r)] (vec (cons (first c) (conj
                                                                                       (vec (for [i (range (dec (count c)))] (+ (nth c i) (nth c (inc i)))
                                                                                                                             )) (first c) ))))))
              ))))

(defcheck solution-cc18f3cf
  (fn [n]
    (let [pascal-seq
          (iterate
            (fn [cs] (vec (map
                            (fn [[c1 c2]] (+ c1 c2))
                            (partition 2 1 (cons 0 (conj cs 0)))))) [1])]
      (nth pascal-seq (dec n)))))

(defcheck solution-cc47d725
  (fn [n]
    (nth (iterate #(concat [1] (map + % (rest %)) [1]) [1])
      (dec n))))

(defcheck solution-cc9fee52
  (fn [n]
    (letfn [(pascal-seq [row]
              (lazy-seq (cons row (pascal-seq (next-row row)))))
            (next-row [row]
              (concat [ 1] (map #(+ %1 %2) row (rest row)) [1]))]
      (nth (pascal-seq [1]) (dec n)))))

(defcheck solution-cca4efac
  (fn pascal[a]
    (if (= 1 a)
      (vector 1)
      (flatten (vector 1
                 (let [p (pascal (dec a))]
                   (loop [r [] prev (first p)  rr (rest p)]
                     (if (empty? rr)
                       r
                       (recur (conj r (+ prev (first rr))) (first rr) (rest rr))
                       )
                     )
                   )
                 1))
      )
    ))

(defcheck solution-cd46bca9
  #(loop [i 1 r [1]]
     (if (< i %)
       (recur
         (inc i)
         (vec
           (for [j (range 0 (+ i 1))]
             (if (or (= j 0) (= j i))
               1
               (+ (r j) (r (- j 1)))))))
       r)))

(defcheck solution-cd8d7027
  (fn [x]
    (letfn [(binom [n k]
              (if
               (or (zero? k) (= n k)) 1
                                      (+ (binom (dec n) (dec k)) (binom (dec n) k))))]
      (mapv (partial binom (dec x)) (range x)))))

(defcheck solution-cdaea683
  (fn [n]
    (if (= n 1)
      [1]
      (last
        (take n
          (iterate #(concat [1] (map (fn [[a b]] (+ a b)) (partition 2 1 %)) [1]) []))))))

(defcheck solution-cdc17b4e
  (fn [row-idx]
    (letfn [(build-row [ptri]
              (if (= (count ptri) 1) (conj ptri [1 1])
                                     (conj ptri (flatten (vector 1 (vec (map #(apply + %1) (partition 2 1 (last ptri)))) 1)))))]
      (loop [ptri [[1]] curr-idx 0]
        (if (= curr-idx row-idx) (get ptri (- curr-idx 1))
                                 (recur (build-row ptri) (inc curr-idx)))))))

(defcheck solution-cdd175d5
  (fn pasc
    [n]
    (if (= 1 n)
      [1]
      (concat (list 1) (map (partial apply +) (partition 2 1 (pasc (dec n)))) (list 1)))))

(defcheck solution-ce532b0f
  (fn [x]
    (reduce (fn [acc _] (map #(apply + %) (partition 2 1 [0] (cons 0 acc))))
      [1]
      (range 1 x))))

(defcheck solution-ce66b27f
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [xs (pascal (dec n))]
        (concat [1] (map (fn [[x1 x2]] (+ x1 x2)) (partition 2 1 xs)) [1])))))

(defcheck solution-cea1eaba
  (fn pascal [n]
    (last (take n ((fn gen-pascal [row]
                     (letfn [(gen-pascal-row [l]
                               (conj
                                 (loop [s l f 0 d []]
                                   (if (empty? s)
                                     d
                                     (recur (rest s) (first s) (conj d (+ f (first s))))))
                                 (last l)))]
                       (lazy-seq
                         (cons row (gen-pascal (gen-pascal-row row)))))) [1])))))

(defcheck solution-cecfe7b4
  (fn [r]
    (reduce #(cons
               (* (first %1)
                 (/ (- r %2) %2))
               %1) [1] (range 1 r))))

(defcheck solution-cf492ffc
  (fn pascals [row]
    (loop [r 1, res [1]]
      (if (= r row)
        res
        (recur (inc r)
          (concat [1]
                  (map (fn [[x y]] (+ x y))
                    (partition 2 1 res))
                  [1]))))))

(defcheck solution-cf86126c
  (fn [thearg] (let [fact (fn fact [n] (if (= n 0) 1 (* n (fact (dec n)))))
                     comb (fn [n k] (/ (fact n) (* (fact k) (fact (- n k)))))]
                 (map #(comb (dec thearg) %)
                   (range 0 thearg)))))

(defcheck solution-d0045112
  (fn pascal [n]
    (if (= n 1)
      [1]
      (loop [p0 [] p1 (concat [0] (pascal (dec n)) [0])]
        (if (< (count p1) 2)
          p0
          (recur (cons (apply + (take 2 p1)) p0) (rest p1)))))))

(defcheck solution-d03f5b3c
  (fn [n]
    (letfn [(next-row [row]
              (let [pairs (partition 2 1 row)
                    innards (map #(apply + %) pairs)]
                (concat [1] innards [1])))]
      (first (drop (dec n) (iterate next-row [1]))))))

(defcheck solution-d053d940
  #(nth (iterate (fn [x] `(1 ~@(map + x (next x)) 1)) [1]) (dec %)))

(defcheck solution-d0cd4ec5
  (fn [n] (reductions #(* %1 (/ (- n %2) %2)) 1 (range 1 n))))

(defcheck solution-d0cfba59
  (fn triangle [n]
    (nth (iterate (fn [t]
                    (map +
                      (concat t [0])
                      (concat [0] t)))
           [1])
      (dec n))))

(defcheck solution-d1a3ee5
  (fn [n]
    (nth
      (iterate
        (comp vec #(map + (cons 0 %) (conj % 0)))
        [1])
      (dec n))))

(defcheck solution-d1aadd7b
  (fn tri [n]
    (loop [x 1 res [1]]
      (if (= x n) res
                  (recur (inc x) (vec (butlast (reduce #(concat (butlast %1) (list (+ (last %1) %2) %2)) '(0) (conj res 0)))))))))

(defcheck solution-d1b31867
  (fn pas-triange [n]
    (let [f (fn [[a b & coll]]
              ((fn [a b coll result]
                 (cond
                   (empty? coll) (conj result (+ a b))
                   :else (recur b (first coll) (rest coll) (conj result (+ a b))))) a b coll []))]
      (cond
        (> 1 n) nil
        (= 1 n) '(1)
        (= 2 n) '(1 1)
        :else (flatten (conj [] 1 (f (pas-triange (dec n))) 1))))))

(defcheck solution-d2707e9f
  (fn p [n]
    (if (= 1 n) [1]
                (concat [1] (map (partial apply +) (partition 2 1 (p (dec n)))) [1]))))

(defcheck solution-d27f7f71
  (fn fr [n] (let [hfr (fn [res xs ys]
                         (if (first xs)
                           (recur (conj res (+ (first xs) (first ys))) (rest xs) (rest ys))
                           res )
                         ) ]
               (cond (= 1 n) [1]
                     (= 2 n) [1 1]
                     :else (let [vorg (fr (dec n))]
                             (hfr [1] (conj (vec (drop 1 vorg)) 0) vorg) )
                     ))))

(defcheck solution-d28616a9
  #(map (fn p [l i] (cond (or (< i 1) (> i l) (< l 1)) 0 (= l i 1) 1 :else (+ (p (dec l) (dec i)) (p (dec l) i)))) (repeat %1 %1) (range 1 (inc %1))))

(defcheck solution-d2e17385
  (fn [n]
    (letfn [(next-row [row]
              (concat [1]
                      (map #(apply + %) (partition 2 1 row))
                      [1]))]
      (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-d2ef73d6
  (fn [n]
    (nth (iterate
           (fn [col]
             (let [nextrow (map (fn [[a b]] (+ a b)) (partition 2 1 col))]
               (flatten [1 nextrow 1]))) [1]) (dec n))))

(defcheck solution-d2f72655
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [prev-row (pascal (dec n))]
        (conj (vec (concat [1] (map + prev-row (rest prev-row)))) 1)))))

(defcheck solution-d3316c3b
  (fn pt [n]
    (cond
      (= n 1) [1]
      :else (let [prev-pt (pt (dec n))]
              (mapv + (cons 0 prev-pt) (conj prev-pt 0))))))

(defcheck solution-d34a09f0
  (fn pascal [n]
    (if (= n 1) [1]
                (let [nx (pascal (dec n))
                      nw (cons 1 (map + nx (drop 1 nx)))]
                  (conj (apply vector nw) 1)
                  ))))

(defcheck solution-d356a38d
  (fn [n]
    (letfn
     [(pt [col]
        (loop [result [1] coll col]
          (if (empty? (rest coll))
            (conj result 1)
            (recur (conj result (+ (first coll) (second coll))) (rest coll)))))]
      (nth (iterate pt [1]) (- n 1)))))

(defcheck solution-d36a0d0f
  #(->
     (fn c [i n]
       (if (or (= i 0) (= i (dec n)))
         1
         (+ (c (dec i) (dec n)) (c i (dec n)))))
     (map-indexed (repeat % %))
     (vec)))

(defcheck solution-d3eeabd6
  (fn [n]
    (reduce (fn [acc el]
              (let [s1 (cons 0 acc)
                    s2 (concat acc [0])]
                (map + s1 s2)))
      [1] (range (dec n)))))

(defcheck solution-d4006f9d
  (fn prob-0097 [n]
    (nth
      (iterate
        (fn [xs] (map #(+ %1 %2) (cons 0 xs) (concat xs [0])))
        [1])
      (dec n))))

(defcheck solution-d4169c30
  (fn pas [n]
    (let [next_pas (fn [a]
                     (loop [x a, nxt [1]]
                       (if (empty? x) nxt
                                      (if (= 1 (count x)) (conj nxt 1)
                                                          (recur (rest x) (conj nxt (+ (first x) (second x))))))))]
      (if (= n 1) [1]
                  (next_pas (pas (dec n)))))))

(defcheck solution-d527cf5
  (fn [a]
    (let [binom
          (fn [k]
            (let [rprod (fn [x y] (reduce * (range x (inc y))))]
              (/ (rprod (- a k) (dec a)) (rprod 1 k))))]
      (map binom (range a)))))

(defcheck solution-d533b0bc
  (fn triangle [n]
    (if (= 1 n)
      [1]
      (let [pt (triangle (dec n))]
        (loop [i 1 acc [1]]
          (if (= i (dec n))
            (conj acc 1)
            (recur (inc i) (conj acc (+ (pt (dec i)) (pt i))))))))))

(defcheck solution-d548ca07
  (fn pascalstriangle [n]
    (if (== n 1)
      [1]
      (loop [result [] init  (cons 0 (conj (pascalstriangle (- n 1)) 0))]
        (if (not (empty? (rest init)))
          (recur (conj result (+ (first init) (first (rest init)))) (rest init))
          result)))))

(defcheck solution-d54a0756
  (fn pt [i]
    (if (= 1 i)
      [1]
      (concat [1]
              (map (partial apply +) (partition 2 1 (pt (dec i)))) [1]))))

(defcheck solution-d5572283
  (fn [n]
    (let [pt (fn []
               (iterate
                 (fn [r]
                   (map #(apply + %)
                     (partition 2 1 (concat [0] r [0]))))
                 [1]))]
      (->> (pt) (drop (dec n)) (first)))))

(defcheck solution-d56a3193
  (fn pascal [n] (let [k (fn [a] (concat [1] (map (fn [[a b]](+ a b)) (partition 2 1 a)) [1]))
                       p (iterate k [1])]
                   (nth p (dec n)))))

(defcheck solution-d59417a3
  #(nth (iterate (fn [x] (vec (map + (conj x 0) (cons 0 x)))) [1]) (dec %)))

(defcheck solution-d59d34b5
  (fn pascal [n]
    (letfn [(make-line [xs]
              (loop [in xs
                     out []]
                (if (< (count in) 2)
                  out
                  (recur
                    (drop 1 in)
                    (conj out (reduce + (take 2 in)))))))]
      (loop [tri [[1]]
             i   1]
        (if (= i n)
          (last tri)
          (recur
            (conj tri (concat [1] (make-line (last tri)) [1]))
            (+ i 1)))))))

(defcheck solution-d62ae9c6
  (fn pascal-
    [n]
    (loop [line n res '(1)]
      (if (= 1 line)
        res
        (recur
          (dec line)
          (concat '(1) (map + res (rest res)) '(1)))))))

(defcheck solution-d6bebac4
  (fn [n]
    (reduce
      (fn [row i]
        (concat [1]
                (map (fn [[x y]] (+ x y))
                  (partition 2 1 row))
                [1]))
      [1]
      (range (- n 1)))))

(defcheck solution-d700d222
  (fn pascal [n]
    (if (= n 1)
      [1]
      (concat '(1)
              (map #(reduce + %) (partition 2 1 (pascal (dec n))))
              '(1))
      )
    ))

(defcheck solution-d7684cfe
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (mapv
        (partial reduce +)
        (let [pn-1 (pascal (dec n))]
          (map vector (cons 0 pn-1) (conj pn-1 0)))))))

(defcheck solution-d78c3973
  (fn [n]
    (nth (iterate (fn [prev]
                    (list* 1 (map #(apply + %) (partition-all 2 1 prev))))
           '(1))
      (dec n))))

(defcheck solution-d7bdb4c2
  (fn [row]
    (loop [result [1] c 1 prev 1]
      (if (= c row)
        result
        (let [n (* prev (/ (- row c) c))]
          (recur (conj result n) (inc c) n))))))

(defcheck solution-d7c7b27f
  #(loop [c 1 r [1]]
     (if (>= c %)
       r
       (recur
         (inc c)
         (conj r (* (last r) (/ (- % c) c)))))))

(defcheck solution-d82022d
  (fn pascal [n] (case n
                   1 [1]
                   2 [1 1]
                   (let [p (pascal(dec n))] (concat [1] (map + p (rest p)) [1]))
                   )
    ))

(defcheck solution-d831c82e
  (fn P [n] (if (= n 1) [1] (let [s (P (- n 1))] (concat [1] (map + (drop-last s) (rest s)) [1])))))

(defcheck solution-d86c154a
  #(letfn [(worker [a n]
             (if (= n 1)
               a
               (recur (cons 1 (map (fn [x] (+ (nth a (dec x))
                                              (if (< x (count a)) (nth a x) 0)))
                                (range 1 (inc (count a)))))
                 (dec n))))]
     (worker [1] %)))

(defcheck solution-d87e52aa
  (fn [n] (first (drop (dec n) (iterate (fn [xs] (map + (cons 0 xs) (lazy-cat xs [0]))) [1])))))

(defcheck solution-d947d2b8
  (fn pascal [r]
    (cond (= r 1) [1]
          :else (concat [1]
                        (map +
                          (butlast (pascal (dec r)))
                          (rest (pascal (dec r))))
                        [1]))))

(defcheck solution-d958b44
  (fn pascal-row [r]
    (let [pc' (fn aux [c]
                (if (or (zero? c) (= c (dec r)))
                  1
                  (* (aux (dec c)) (/ (- r c) c))))]
      (map pc' (range 0 r)))))

(defcheck solution-d9a978d7
  (fn [n] (
            nth (
                  iterate (
                            fn [xs] (
                                     #(
                                        concat '(1) % '(1)
                                               ) (
                                                   drop 2 (map (partial apply +) (
                                                                                   reductions #(list (last %) %2) '(0 0) xs)
                                                            )
                                                   )
                                     )
                            ) [1]
                  ) (- n 1)
            )
    ))

(defcheck solution-d9c645b1
  (fn [num]
    (let [calc-pasc-nums (fn [list]
                           (into []
                             (map (partial reduce +) (partition 2 1 list))))
          gen-pasc-nums (fn [list]
                          (let [prepend-1 (into [1] (calc-pasc-nums list))
                                append-1 (into prepend-1 [1])]
                            append-1))]
      (last (take num (iterate gen-pasc-nums [1]))))))

(defcheck solution-d9cb3ef7
  (fn [n]
    (letfn [(gen-triangle
              [coll]
              (lazy-seq
                (cons coll (lazy-seq (gen-triangle (mapv + (cons 0 coll) (conj coll 0)))))))]
      (nth (gen-triangle [1]) (dec n)))))

(defcheck solution-da03b5a0
  (fn [n]
    (loop [i (dec n)
           acc [1]]
      (if (zero? i)
        acc
        (recur (dec i) (concat [1] (map (partial apply +) (partition 2 1 acc)) [1]))))))

(defcheck solution-da162d03
  (fn pascal [n1]
    (loop [n n1 r '(1)]
      (cond
        (= n 1) r
        :else (recur (dec n) (let [nr (concat [0] r [0])
                                   nnr (partition 2 1 nr)]
                               (map (fn [[x y]] (+ x y))  nnr) ) )))))

(defcheck solution-da1c065d
  (fn [n]
    (->>
      (iterate #(concat [1] (map + % (rest %)) [1]) [1])
      (take n)
      (last))))

(defcheck solution-da5f0826
  (fn [n]
    (nth (iterate
           (fn[p]
             (if (empty? p)
               [1]
               (conj (into [1] (map (fn [[a b]] (+ a b)) (partition 2 1 p))) 1)))
           []) n)))

(defcheck solution-da6b55c2
  (fn pascal-row
    ([n] (cons 1 (lazy-seq (pascal-row n 1 1))))
    ([n k previous] (let [current (* previous (/ (-  n k) k))]
                      (if (= k n)
                        nil
                        (cons current (lazy-seq (pascal-row n (inc k) current))))))))

(defcheck solution-da8bd7d2
  (fn [n]
    (loop [nn n ll [1]]
      (if (= nn 1)
        ll
        (recur (dec nn)
          (concat [1] (map + ll (rest ll)) [1]))))))

(defcheck solution-da9b5e51
  (fn [anum]
    (loop [anum anum v1 [1]]
      (if (= anum 1)
        v1
        (let [left  (conj v1 0)
              right (cons 0 v1)
              inner (map #(+ %1 %2) left right)
              ]
          (recur (dec anum) (vec inner))
          )))
    ))

(defcheck solution-da9ce691
  (fn [n]
    (let [next-ptri
          (fn [row]
            (cons 1
              (conj (vec (map + row (rest row)))
                1))
            )]
      (let [p-tri (cons [] (iterate next-ptri [1]))]
        (nth p-tri n)))))

(defcheck solution-daeb2535
  (fn row [n] (cond (= n 1) [1] (= n 2) [1 1] :else (conj (apply (partial conj [1]) (map (partial apply +) (partition 2 1 (row (dec n))))) 1))))

(defcheck solution-db28eec3
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [prev (pascal (dec n))]
        (map
          #(if (or (= 0 %) (= (dec n) %))
             1
             (+ (nth prev %) (nth prev (dec %)))
             )
          (range n)
          )
        )
      )
    ))

(defcheck solution-db69d8ea
  (fn pascalTriRow[n]
    (if (= 1 n)
      [1]
      (vec (concat [1] (vec (map #(apply + %) (partition 2 1 (pascalTriRow (- n 1))))) [1])))))

(defcheck solution-db82712
  (fn [n]
    (nth (iterate
           (fn [pre]
             (vec
               (concat
                [1]
                (map (fn [[f s]] (+ f s)) (partition 2 1 pre))
                [1])))
           [1])
      (dec n))))

(defcheck solution-db8ed899
  (fn [n]
    (let [n1 (dec n)
          fact #(apply * (range 1 (inc %)))
          n1fact (fact n1)]

      (map #(/ n1fact (* (fact %) (fact (- n1 %)))) (range 0 (inc n1))))))

(defcheck solution-dbc06e1a
  (fn [n]
    (loop [prev [1]
           n n]
      (if (= 1 n)
        prev
        (recur (map + (cons 0 prev) (concat prev '(0)))
          (- n 1))))))

(defcheck solution-dbf896b2
  (fn nth-pascal [n]
    (let [pascal-triangle (fn []
                            (let [
                                  next (fn [row]
                                         (concat [1] (map + row (rest row)) [1]))
                                  impl (fn impl [row]
                                         (cons row (lazy-seq (impl (next row)))))
                                  ]
                              (impl [1])))]
      (nth (pascal-triangle) (- n 1)))))

(defcheck solution-dcd0c19c
  (fn pas [n]
    (if (= n 1) [1]
                (flatten [1 (->> (pas (dec n))
                              (partition 2 1)
                              (map #(apply + %))


                              ) 1]) )))

(defcheck solution-dd53cc02
  (fn pascal [y]
    (let [n (dec y)
          factorial (fn [x] (reduce * (range 1 (inc x))))
          n-choose-r (fn [r] (/ (factorial n)
                               (* (factorial r)
                                 (factorial (- n r)))))]
      (conj (into [] (map n-choose-r (range n))) 1))))

(defcheck solution-ddf4d023
  (fn P [n]
    (if (= n 1)
      [1]
      (concat
       [1]
       (map #(apply + %) (partition 2 1 (P (dec n))))
       [1]))))

(defcheck solution-deb0fea6
  (fn [tier]
    (nth
      (iterate
        (fn [v] (concat [1]
                        (map
                          (fn [p] (reduce + p))
                          (partition 2 1 v))
                        [1]))
        [1])
      (dec tier))))

(defcheck solution-df27e43d
  (fn [n] (->> [1]
            (iterate #(vec `(1 ~@(map + % (next %)) 1)))
            (take n)
            last
            )))

(defcheck solution-df42a47b
  (fn pt [n]
    (case n
      1 [1]
      (concat [1] (map #(apply + %) (partition 2 1 (pt (dec n)))) [1]))))

(defcheck solution-df5e5570
  #(reduce
     (fn [a x]
       (conj a
         (* (peek a)
           (/ (- % x) x))))
     [1] (range 1 %)))

(defcheck solution-df86bda5
  (fn pascal [n]
    (loop [i 1, row [1]]
      (if (= n i) row
                  (recur (inc i) (concat [1] (map #(apply + %) (partition 2 1 row)) [1]))))))

(defcheck solution-dfb5e16e
  (fn mypt [n]
    (let [calc-pt
          (fn [xs]
            (let [pth (reduce
                        #(hash-map
                           :pt (conj
                                 (%1 :pt )
                                 (+ %2 (%1 :last )))
                           :last %2)
                        {:pt [] :last 0} xs)]
              (conj (pth :pt) (pth :last ))))]
      (loop [r n, pt [1]]
        (if (= r 1)
          pt
          (recur (dec r) (calc-pt pt)))))))

(defcheck solution-dfb6cef8
  (fn this [n]
    (cond (= n 1) [1]
          :else (let [up (this (dec n))]
                  (vec (concat (for [i (range (count up))]
                                 (cond (= i 0) 1
                                       :else (+ (nth up (dec i))
                                                (nth up i)))) '(1)))))))

(defcheck solution-dfc4a4df
  (fn f [X]
    (cond
      (<= X 1) [1]
      :else (let [pred (f (dec X))
                  s (map + pred (rest pred))]
              (into [] (concat [1] s [1])))
      )))

(defcheck solution-dfcd2787
  (fn  [n]
    (let
     [nextp (fn [col]
              (let [lst (cons 0 (conj (vec col) 0))]
                (for [i (range (dec (count lst)))]
                  (+ (nth lst i) (nth lst (inc i))))))]
      (last (take n (iterate nextp [1]))))))

(defcheck solution-dfd09060
  (fn p [n]
    (if (= 1 n)
      [1]
      (vec (#(map + % (rest %))
            (conj (seq (conj (p (dec n)) 0)) 0))))))

(defcheck solution-dffeb4db
  #(let [f (fn [nseq]
             (cond
               (= (count nseq) 0) [1]
               (= (count nseq) 1) [1 1]
               :else (flatten [1 (map + (rest nseq) (butlast nseq)) 1])))]
     (loop [track 0
            nums []]
       (if (= track %1)
         nums
         (recur (inc track) (f nums))))))

(defcheck solution-e0727da0
  (fn [n]
    (cond (= n 1)
          [1]
          :else
          (loop [n (- n 2)
                 t [1 1]]
            (if (= n 0)
              t
              (recur (- n 1)
                (conj (loop [t t
                             p 0
                             r []]
                        (if (empty? t)
                          r
                          (recur (rest t) (first t) (conj r (+ (first t) p)))))
                  1)))))))

(defcheck solution-e0dadf1
  (fn pastri [n]
    (loop [v [1] m 1]
      (if (= n m)
        v
        (recur (conj (mapv #(apply + %) (reductions (fn [[_ x] y] [x y]) [0 1] (rest v))) 1)
          (inc m))))))

(defcheck solution-e0deb65b
  (fn [n]
    (nth
      (iterate
        #(map + (conj % 0) (reverse (conj % 0)))
        [1])
      (dec n))))

(defcheck solution-e103c3cc
  #(nth (iterate (fn [x] (map + `(0 ~@x) `(~@x 0))) [1]) (- % 1)))

(defcheck solution-e10f94e3
  (fn [i] (nth (iterate #(cons 1 (conj (mapv (partial apply +) (partition 2 1 %)) 1)) [1]) (dec i))))

(defcheck solution-e1228ca6
  (fn [nth]
    (let [func (fn gen[r c]
                 (if (or (= r 1) (= r c) (= c 1))
                   1
                   (+ (gen (dec r) c) (gen (dec r) (dec c)) ) ))]
      (for [x (range 1 (inc nth))]
        (func nth x) ))))

(defcheck solution-e138e820
  (fn [n] (last (take n (iterate #(map + (concat [0] %) (concat % [0])) [1])))))

(defcheck solution-e169ebe2
  (fn [n] (nth (iterate #(vec (map + (conj % 0) (cons 0 %))) '[1]) (dec n))))

(defcheck solution-e1b7a187
  (fn f [n]
    (last (take n (iterate #(vec (for [i (-> % (count) (inc) (range))] (+ (nth % (dec i) 0) (nth % i 0)))) [1])))))

(defcheck solution-e1dc3fb5
  (fn pscl [n]
    (if (< n 2) [1]
                (map #(reduce + %)
                  (partition 2 1 (concat [0] (pscl (dec n)) [0]))))))

(defcheck solution-e1fbfc39
  (fn pascal [n]
    (letfn [(elem [n k]
              (if (= 0 k)
                1
                (* (elem n (dec k)) (/ (- (inc n) k) k))))]
      (let [n (dec n)
            elem (partial elem n)]
        (vec (map elem (range 0 (inc n))))))))

(defcheck solution-e227ba3a
  (fn pascal [n]
    (let [pascal-next
          (fn [r]
            (let [pairs (partition 2 1 r)
                  sums (vec (map #(reduce + %) pairs))]
              (conj (vec (cons 1 sums)) 1)))]

      (if (= n 1) [1]
                  (pascal-next (pascal (dec n)))))))

(defcheck solution-e257e6d2
  (fn pascal-row [n]
    (letfn [(pairs [coll]
              (map list coll (next coll)))
            (pascal-step [xs]
              (->> xs
                pairs
                (map (partial reduce + 0))
                (#(conj % 1))
                vec
                (#(conj % 1))))
            (pascal
              ([] (iterate pascal-step [1]))
              ([n] (take n (pascal))))]
      (nth (pascal) (dec n)))))

(defcheck solution-e365c52a
  (fn [n]
    (loop [i 1
           r [1]]
      (if (= n i)
        r
        (recur (inc i) (concat '(1) (map #(apply + %) (partition 2 1 r)) '(1)))))))

(defcheck solution-e37aa9f
  #(concat (take (dec %)
             (map first
               (iterate
                 (fn [[x c]]
                   (list (* x (/ (- % c) c)) (inc c)))
                 [1 1])))
           '(1)))

(defcheck solution-e3baf1a2
  #(loop [i 1, result [1]]
     (if (= i %1)
       result
       (recur (+ i 1)
         (into []
           (map +
             (cons 0 result)
             (conj result 0)))))))

(defcheck solution-e49a4fe9
  (fn [x]
    (loop [v [1]]
      (if (= x (count v))
        v
        (recur (vec (map + (cons 0 v) (conj v 0))))))))

(defcheck solution-e4e81a1d
  (fn pascal [n]
    (if (= n 1) '(1)
                (let [m (pascal (dec n))]
                  (map + (conj m 0) (concat m '(0)))))))

(defcheck solution-e4f4a124
  (fn make-pascal [n]
    (vec (nth
           (iterate
             (fn[colls]
               (concat (map + colls (list* 0  colls)) [1]))
             [1])
           (dec n)))))

(defcheck solution-e5081fa4
  (fn [n] (nth (iterate #(vec (map + (cons 0 %) (conj % 0))) '[1]) (dec n))))

(defcheck solution-e5c60b75
  (fn pascal[index]
    (letfn [(gen-next [s]
              (loop [s s
                     result []]
                (if (= (count s) 0)
                  [1]
                  (if (= (count s) 1)
                    (if (empty? result)
                      [1 1]
                      (apply vector 1 (conj result 1)))
                    (recur (rest s)
                      (conj result (+ (first s) (second s))))))))]
      (loop [cnt index
             next-seq []]
        (if (= cnt 0)
          next-seq
          (recur (dec cnt)
            (if (= cnt 0)
              (gen-next [1])
              (gen-next next-seq))))))))

(defcheck solution-e606c16
  (fn pascal [lvl]
    (loop [p [1] lvl lvl]
      (if (= lvl 1) p
                    (recur (conj (into [1] (map + (rest p) (butlast p))) 1) (dec lvl))))))

(defcheck solution-e675c27d
  (fn [r]
    (reduce
      #(conj % (* (last %) (/ (- r %2) %2)))
      [1]
      (range 1 r))))

(defcheck solution-e74678a7
  (fn pasc [n]
    (if (= 1 n)
      [1]
      (let[p (conj (pasc (dec n)) 0)]
        (apply vector (map + (cons 0 p) p))))))

(defcheck solution-e756ac4d
  (fn pascalX [x] (if(= x 1) [1] (concat [1] ((fn addAdjacentX [x] (vec (map + x (rest x)))) (pascalX (- x 1))) [1]))))

(defcheck solution-e76aafb3
  (fn pascal-triangle [n]
    (if (= 1 n)
      [1]
      (let [r-above (pascal-triangle (dec n))]
        (concat  [1] (map + r-above (rest r-above)) [1])))))

(defcheck solution-e7a8fb27
  (fn pascal [n]
    (let [pas #(concat [1]
                       (map (partial apply +)
                         (partition 2 1 %))
                       [1])
          pasn (fn [lst n]
                 (if (= n 1) lst
                             (recur (pas lst) (dec n))))]
      (vec (pasn [1] n)))))

(defcheck solution-e7e022f3
  (fn [n]
    (nth
      (iterate
        (fn [nums] (vec (map + (cons 0 nums) (conj nums 0))))
        [1])
      (dec n))))

(defcheck solution-e81c3f3a
  #(loop [x [1] n 1]
     (if (= n %) x
                 (recur
                   (concat [1] (map (partial reduce +) (partition 2 1 x)) [1])
                   (inc n)))))

(defcheck solution-e82c5c15
  #(loop [n 1 rows [[1]]]
     (let [curr (last rows)]
       (if (= n %) curr
                   (recur (inc n)
                     (conj rows (conj (apply vector (conj
                                                      (map (fn[p] (reduce + p)) (remove (fn[i] (empty? (rest i))) (partition-all 2 1 curr))) 1)) 1)))))))

(defcheck solution-e8583543
  (fn [n]
    (loop [i 1
           ret [0 1 0]]
      (if (= i n)
        (filter #(not (zero? %)) ret)
        (recur (inc i)
          (conj (apply conj [] 0 (vec (for [x (range (inc i))]
                                        (+ (ret x) (ret (inc x)))))) 0)
          )))))

(defcheck solution-e8963260
  (fn [i]
    (let [nxt (fn [s] (map #(apply + %&) (concat [0] s) (concat s [0])))]
      (if (> i 1)
        (nth (iterate nxt [1]) (- i 1))
        [1]))))

(defcheck solution-e8987102
  #(map (fn p [c] (if (= c 0) 1 (* (p (dec c)) (/ (- % c) c)))) (range %)))

(defcheck solution-e8b5e7fe
  (fn p [x]
    (letfn [(f [n k]
              (if (or (= k 1) (= k n))
                1
                (+ (f (dec n) (dec k))
                   (f (dec n) k))))]
      (map #(apply f %)
        (map-indexed #(vector %2 (inc %1)) (repeat x x))))))

(defcheck solution-e8bd7354
  (fn [row]
    (letfn [(pascal [c r]
              (cond (zero? c) 1
                    (zero? r) 0
                    :else (+ (pascal (dec c) (dec r)) (pascal c (dec r)))))]
      (map #(pascal % (dec row)) (range row)))))

(defcheck solution-e8bdfd58
  (fn mpt [d]
    (if (= 1 d)
      [1]
      (let [ prevrange (concat [0] (mpt (dec d)) [0])
            ]
        (for [ idx (range d)]
          (+ (nth prevrange idx) (nth prevrange (inc idx))))))))

(defcheck solution-e90b72cc
  (fn [n]
    (let [next-line (fn [s] (conj (loop [s s acc [1]]
                                    (if (> (count s) 1)
                                      (recur (rest s) (conj acc (+ (first s) (second s))))
                                      acc)) 1))]
      (loop [n (dec n) acc [1]] (if (zero? n) acc (recur (dec n) (next-line acc)))))))

(defcheck solution-e9432e2f
  (fn pascal [n]
    (if (= n 1)
      [1]
      (map #(apply + %) (->> (pascal (dec n))
                          (partition-all 2 1)
                          (cons '(1)))))))

(defcheck solution-e9887fd9
  (letfn [(f [xs]
            (lazy-seq
              (if (sequential? xs)
                (let [line (map #(apply + %) (partition 2 1 (concat [0] xs [0])))]
                  (cons line (f line)))
                (cons '(1) (f '(1))))))]
    #(nth (f nil) (dec %))))

(defcheck solution-e9b167e5
  (fn [n]
    (nth
      (iterate
        (fn [v] (if (empty? v) [1]
                               (let [nv (map #(apply + %) (partition 2 1 v))]
                                 (mapcat identity [[1] nv [1]]))))
        [])
      n)))

(defcheck solution-ea1c8b1a
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (let [prev-pascal (pascal (dec n))]
        (apply vector (
                       (fn addseqs [a b]
                         (if (= 1 (count a))
                           [(+ (first a) (first b))]
                           (cons (+ (first a) (first b)) (addseqs (rest a) (rest b)))))
                       (cons 0 prev-pascal) (conj prev-pascal 0)))))))

(defcheck solution-ea6b3f4b
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [lst (pascal (dec n))]
        (mapv + (cons 0 lst) (conj lst 0))))))

(defcheck solution-ea7db554
  (fn f [a] (if (= 1 a) [1] (let [c conj x (f (- a 1))] (vec (map + (c x 0) (apply c [0] x)))))))

(defcheck solution-eaab4772
  (fn ptri [r]
    (when (pos? r)
      (cond
        (= r 1) [1]
        :default
        (let [pr (ptri (dec r))]
          (concat [1] (map + pr (rest pr)) [1]))))))

(defcheck solution-eac855d5
  (fn [n] (loop [i n v [1]] (if (= i 1) v (recur (- i 1) (vec (map + (cons 0 v) (conj v 0))))))))

(defcheck solution-ead60db0
  (fn [n] (let [next-row (fn [x] (map #(+ %1 %2) (concat x [0]) (concat [0] x)))]
            (nth (iterate next-row [1]) (dec n)))))

(defcheck solution-eafb29a2
  (fn pascal-row [n]
    (if (= n 1)
      '(1)
      (let [L (pascal-row (dec n))]
        (concat '(1) (map + L (rest L)) '(1))))))

(defcheck solution-eb751c10
  (fn [num]
    (letfn [(fact[n] (reduce * (range 1 n)))]
      (map (fn[x] (/ (fact num) (* (fact x) (fact (- (inc num) x))))) (range 1 (inc num))))))

(defcheck solution-eb82fbb0
  (fn [n]
    (loop [row [1] i 1]
      (if (= i n)
        row
        (recur
          (conj
            (into [1]
              (map #(+ (row %) (row (inc %))) (range (dec (count row)))))
            1)
          (inc i))))))

(defcheck solution-eb8c774f
  (fn
    [n]
    (into
      []
      (map
        (partial
          (fn
            [n k]
            (let
             [p (fn [a b] (reduce * (range a (inc b))))]
              (/ (p (inc k) n) (p 1 (- n k)))))
          (- n 1))
        (range 0 n)))))

(defcheck solution-ebdea267
  (fn [n]
    (let [helper (fn [row]
                   (into [] (concat [1] (map (fn[[a b]] (+ a b)) (partition 2 1 row )) [1])))]
      (loop [count 1
             row [1]]
        (if (= n count)
          row
          (recur (inc count) (helper row)))
        ))))

(defcheck solution-ebf47516
  #(let [t (dec %)]
     (map (fn [k]
            (letfn [(fac [n] (reduce * (range 1 (inc n))))]
              (/ (fac t) (* (fac k) (fac (- t k)))))) (range (inc t)))))

(defcheck solution-ec11298f
  (let [s (iterate (fn [row] (into [] (map + (cons 0 row) (conj row 0)))) [1])] #(nth s (dec %))))

(defcheck solution-ec4f4bce
  (fn [n]
    (last
      (take n
        (iterate
          (fn [x]
            (map + (cons 0 x) (reverse (cons 0 x))))
          [1]  )
        ))))

(defcheck solution-ece449f0
  (fn [n]
    (nth (iterate #(map + `(0 ~@%) `(~@% 0)) [1]) (dec n))))

(defcheck solution-ed571e93
  (memoize
    (fn pascal-triangle [n]
      (if (= n 1)
        [1]
        (let [prev (pascal-triangle (dec n))]
          (concat [1] (map + prev (rest prev)) [1]))))))

(defcheck solution-ed5d5b65
  (fn pascal
    [n]
    (if (= 1 n) [1]
                (if (= 2 n) [1 1]
                            (cons 1 (conj (vec(map #(reduce + %) (partition 2 1 (pascal (dec n))))) 1))))))

(defcheck solution-edc9cbe1
  (fn pas
    [n] (if (= 1 n) [1]
                    (let [prev (pas (dec n))]
                      (vec (map + (conj prev 0) (into [0] prev)))))))

(defcheck solution-ee3fe96c
  (fn [n] (letfn
           [(pascal
              [m]
              (if (= m 1)
                [1]
                (let
                 [row (-> m dec pascal)]
                  (map + (concat [0] row) (concat row [0])))))]
            (pascal n))))

(defcheck solution-eebe275b
  (fn [n] (nth (iterate (fn [x] (filter #(not (zero? %)) (map #(apply + %) (partition 2 1 [0] (flatten [0 x 0]))))) [1]) (dec n))))

(defcheck solution-eeff8b4b
  (fn [n]
    (let* [f #(apply * (range 1 (inc %))) m (dec n) mf (f m)]
      (for [k (range n)]
        (/ mf (* (f k) (f (- m k))))))))

(defcheck solution-ef552899
  (fn [n]
    (first (drop (dec n)
             (iterate (fn [c] (concat [1]
                                      (map #(apply + %)
                                        (partition 2 1 c))
                                      [1]))
               [1])))))

(defcheck solution-f071cd4d
  (fn [m] (let [n (- m 1)] (into [] (for [k (range 0 (+ n 1))] (/ (if (= n 0) 1 (reduce * (range 1 (+ 1 n))))
                                                                 (if (= k 0) 1 (reduce * (range 1 (+ 1 k))))
                                                                 (if (= (- n k) 0) 1 (reduce * (range 1 (+ 1 (- n k)))))))))))

(defcheck solution-f07b5561
  (fn [i]
    (let [f #(apply * (range 1 %))]
      (map #(/ (f i) (f (+ 1 %)) (f (- i %))) (range i)))))

(defcheck solution-f0c1f200
  (fn p [n]
    (if (= n 1) [1]
                (conj (vec (cons 1 (map #(apply + %) (partition 2 1 (p (dec n)))))) 1))))

(defcheck solution-f0d8b95b
  (fn pascal [n]
    (loop [acc [1]
           left n]
      (if (= left 1)
        acc
        (let [acc-with-pad (concat [0] acc [0])]
          (recur
            (vec
              (map
                (partial reduce +)
                (mapv vector acc-with-pad (rest acc-with-pad))))
            (dec left)))))))

(defcheck solution-f0db6a73
  (fn pascal-row [i]
    (if (= 1 i)
      [1]
      (let [x (pascal-row (dec i))
            a (conj x 0)
            b (cons 0 x)]
        (vec (map + a b))))))

(defcheck solution-f0e331ff
  (fn pascal
    ([n] (pascal [1] n 1))
    ([old n counter]
     (if (= counter n) old
                       (let [new (concat [1]
                                         (map #(apply + %)
                                           (partition 2 1 old))
                                         [1])]
                         (recur new n (inc counter)))))))

(defcheck solution-f11997cd
  #(map second
     (take %
       (iterate
         (fn [[x y]] [(inc x) (* (/ y x) (- % x))])[1 1]))))

(defcheck solution-f1509319
  #(loop [c % row [1]]
     (if (> c 1) (recur (dec c) (conj (vec (conj (map + row (rest row)) 1)) 1))
                 row)))

(defcheck solution-f186b6ca
  (fn __ [n]
    (if (<= n 1) [1]
                 (let [r (__ (dec n))]
                   (map + (concat r [0]) (concat [0] r))))))

(defcheck solution-f1b98205
  #(let [f (fn [x] (apply * (range 1 (+ x 1))))
         k (- % 1)
         n (fn [x] (/ (f k) (* (f x) (f (- k x)))))]
     (map n (range %))))

(defcheck solution-f218b085
  (fn [r]
    (loop [c 1 v 1 res []]
      (if (>= r c)
        (recur (inc c)
          (* v (/ (- r c) c))
          (conj res v))
        res))))

(defcheck solution-f24c8188
  #(loop [x % r '(1)]
     (if (= 1 x)
       r
       (recur (dec x) (map + (cons 0 r) (concat r (list 0)))))))

(defcheck solution-f267d383
  #(loop [c 1 s '(1)]
     (if (= % c)
       s
       (recur (+ c 1) (conj s (* (first s) (/ (- % c) c)))))))

(defcheck solution-f27af700
  (fn pt [n] (if (= 1 n) [1] (let [pt-1 (pt (dec n))]
                               (concat [1] (map + pt-1 (next pt-1)) [1])))))

(defcheck solution-f2ce2389
  (fn p [n]
    (last (take n (iterate
                    #(concat [1] (map + (rest %) %) [1]) [1])))
    ))

(defcheck solution-f2fe7434
  (fn p [n]
    (last
      (take n
        (iterate
          #(concat
            [1]
            (map (partial apply +) (partition 2 1 %))
            [1])
          [1])))))

(defcheck solution-f30cc39d
  (fn pascal-row [x]
    (if (= x 1)
      [1]
      (let [prev (pascal-row (dec x))]
        (concat [1] (map #(+ (nth prev %) (nth prev (inc %))) (range (dec(count prev)))) [1])))))

(defcheck solution-f46e6820
  (fn g [n] (cond
              (= 1 n) [1]
              (= 2 n) [1 1]
              :else
              (let [[_ & r :as l] (g (dec n))
                    t (butlast l)]
                (concat (conj (map + r t) 1) [1])))))

(defcheck solution-f4988b0e
  (fn [n] (loop [s [1] n (dec n)] (if (zero? n) s (recur (conj (into [1] (map #(apply + %) (partition 2 1 s))) 1) (dec n))))))

(defcheck solution-f4988cad
  (fn [n] (nth (iterate #(mapv + (conj % 0) (cons 0 %)) [1]) (dec n))))

(defcheck solution-f5aa17a
  (fn pascal-nth-row [n]
    (cond
      (= 1 n) [1]
      (= 2 n) [1 1]
      :default
      (loop [i 1
             row [1]]
        (if (= i n)
          row
          (recur (inc i)
            (concat [1]
                    (map (fn [[x y]] (+ x y))
                      (partition 2 1 row))
                    [1])))))))

(defcheck solution-f5b1628b
  (fn [n]
    (letfn [(next-row [xs]
              ((comp (partial map (partial reduce +))
                     (partial partition 2 1)
                     #(concat [0] % [0]))
               xs))]
      (first (drop (dec n) (iterate next-row [1]))))))

(defcheck solution-f5f868a8
  (fn [n]
    (loop [i n l [1]]
      (if (= 1 i) l
                  (recur (dec i)
                    (cons 1 ((fn rec [x]
                               (let [a (first x)
                                     b (second x)
                                     c (rest x)]
                                 (if (empty? c) [a]
                                                (cons (+ a b) (rec c))))) l)))))))

(defcheck solution-f602a8af
  (fn ps [n]
    (if (= n 1) [1]
                (let [p (ps (dec n))]
                  (into [] (map + (cons 0 p) (conj p 0)))))))

(defcheck solution-f6116750
  (fn [n] (nth ((fn pascal-row [prev]
                  (cons prev (lazy-seq (pascal-row (map + (concat prev [0]) (cons 0 prev)))))) [1]) (- n 1))))

(defcheck solution-f63bdac6
  (fn pas [n]
    (cond (= n 1) [1]
          (= n 2) [1 1]
          :else (concat [1]
                        (map #(+ (first %) (second %))

                          ((fn part [x y]
                             (cond (= (count y) 0) nil
                                   :else (concat x (part [[(first y) (second y)]] (rest y)))
                                   )
                             ) [] (pas (dec n))))
                        [1]
                        )
          )
    ))

(defcheck solution-f63be2cf
  #(for [r (range %)]
     (reduce * (for [k (range r)] (/ (- % k 1) (+ k 1))))))

(defcheck solution-f6cfa676
  (fn [n]
    (nth
      (iterate
        #(vec (map + (into [0] %) (conj % 0))) [1])
      (dec n))))

(defcheck solution-f6eb8185
  (fn [x]
    (let [f #(concat [1] % [1])
          xss (iterate #(f (map + % (rest %))) [1])
          g (partial nth xss)]
      (cond (number? x) (-> x dec g)
            (coll? x) (map (comp g dec) x)))))

(defcheck solution-f7630759
  #(last (take % (iterate (fn [xs]
                            (cons 1 (into (list 1)
                                      (map + (rest xs) (butlast xs)))))
                   [1]))))

(defcheck solution-f7b11326
  (fn [n]
    (letfn [(pascal [row col]
              (cond (< col 1) 0
                    (> col row) 0
                    (= row 1) 1
                    :else (+ (pascal (dec row) (dec col)) (pascal (dec row) col))))]
      (map (partial pascal n) (range 1 (inc n))))))

(defcheck solution-f804501
  (fn simple-triangle [n]
    (cond
      (= n 1) [1]
      (= n 2) [1 1]
      :else (let [prev-triangle (simple-triangle (- n 1))]
              (concat [1]
                      (map (fn [x1 x2] (+ x1 x2))
                        prev-triangle (next prev-triangle))
                      [1])))))

(defcheck solution-f93e6e3d
  (fn [n]
    (last (take n (iterate (fn [row]
                             (map #(apply + %) (partition 2 1 [0] (cons 0 row))))
                    '(1))))))

(defcheck solution-f98403be
  (fn pascal [s]
    (if (= s 1)
      [1]
      (concat [1] (map #(apply + %) (partition 2 1 (pascal (dec s)))) [1])
      )
    ))

(defcheck solution-f98ee47c
  (fn my-pascal [n]
    (if (= n 1) [1]
                (let [xs (cons 0 (my-pascal (dec n)))]
                  (map + xs (reverse xs))))))

(defcheck solution-f9a17f2
  #(nth ((fn pt [v] (let [u (apply vector 0 v) w (conj v 0)] (cons v (lazy-seq (pt (vec (map + u w))))))) [1]) (dec %)))

(defcheck solution-f9b585f8
  (fn [n] (nth (iterate #(map + (conj (vec %) 0) (cons 0 (vec %))) [1]) (dec n))))

(defcheck solution-fa1b5c6a
  (fn [n]
    (nth
      (iterate
        (fn [nums]
          (vec
            (map + (conj nums 0) (cons 0 nums))))
        [1])
      (dec n))))

(defcheck solution-fa912230
  (fn f [r]
    (cond
      (= r 1) [1]
      (= r 2) [1 1]
      true (vec (concat [1] (map (partial apply +) (partition 2 1 (f (dec r)))) [1])))))

(defcheck solution-facb6e1d
  (fn pascal
    ([x]
     (pascal x 1 [1]))
    ([x cnt out]
     (if (= cnt x)
       out
       (if (= x 2)
         (pascal x (inc cnt) [1 1])
         (pascal x (inc cnt) (apply vector (concat [1] (map #(+ (nth out %) (nth out (inc %))) (range (- cnt 1))) [1]))))))))

(defcheck solution-faf29dbc
  (fn pas [n]
    (if (= n 1)
      [1]
      (vec (map #(apply + %)
             (partition 2 1
               (concat [0] (pas (dec n)) [0])))))))

(defcheck solution-fb52c925
  (fn nth-pascal [n]
    (if (= n 1)
      '(1)
      (let [mid (map (partial apply +) (partition 2 1 (nth-pascal (dec n))))]
        (concat '(1) mid '(1))))))

(defcheck solution-fbd7dfce
  (fn prow
    [row]
    (let [pascal (iterate #(mapv + (conj % 0) (cons 0 %)) [1])]
      (nth pascal (- row 1)))))

(defcheck solution-fbea54b0
  (fn P [n]
    (if (= n 1)
      [1]
      (vec (map + (conj (P (- n 1)) 0) (cons 0 (P (- n 1))))))))

(defcheck solution-fc8e4e6
  #(loop [acc [1] i 1]
     (if (= i %)
       acc
       (recur (map + (conj (vec acc) 0) (cons 0 acc)) (inc i)))))

(defcheck solution-fc949e27
  (fn [n]
    (let [pascal
          (fn [s]
            (conj (vec (cons 1 (map + s (rest s))))  1))]
      (loop [n n r [1]]
        (if (= 1 n) r (recur (dec n) (pascal r)))))))

(defcheck solution-fcb7ef15
  (fn [n] (loop [i 1 res [1]] (if (= i n) res (recur (inc i) (map #(apply + %) (partition 2 1 (concat [0] res [0]))))))))

(defcheck solution-fd3162e4
  (fn [n] (letfn [(pt [] (iterate (fn [r] (map #(apply + %) (partition 2 1 (concat [0] r [0])))) [1]))] (->> (pt) (take n) (last)))))

(defcheck solution-fd60c103
  (fn [n] (loop [i n col [1]] (if (= i 1) col (recur (dec i) (#(loop [[h1 h2 & t1 :as t] % result [1]] (if (nil? h2) (conj result 1) (recur (rest t) (conj result (+ h1 h2))))) col))))))

(defcheck solution-fdf52d73
  (fn pascal-triangle-row-2 [n]
    (letfn [(pascal-triangle-row [pre-row row-index]
              (reduce (fn [ret index]
                        (conj ret (+ (get pre-row (dec index) 0)
                                     (get pre-row index     0))))
                []
                (range row-index)))]
      (condp = n
        1 [1]
        (pascal-triangle-row (pascal-triangle-row-2 (dec n)) n)))))

(defcheck solution-fdf8b05
  (fn pascal [n]
    (if (= n 1)
      [1]
      (let [lst (pascal (dec n))
            a (vec (cons 0 lst))
            b (conj lst 0)]
        (vec (map + a b))))))

(defcheck solution-fe848f6f
  (fn t [n]
    (if (= 1 n) [1]
                (let [r (t (dec n))]
                  (concat [1] (map #(apply + %) (partition 2 (interleave r (rest r)))) [1])))))

(defcheck solution-fee7eb3c
  (memoize
    (fn pascal [n]
      (if (= n 1) [1]
                  (concat [1]
                          (map #(apply + %) (partition 2 1 (pascal (dec n))))
                          [1])))))

(defcheck solution-ff0299e1
  (fn pascal [n]
    (if (= 1 n)
      [1]
      (let [s (pascal (dec n))]
        (map + (concat '(0) s) (concat s '(0))))
      )
    ))

(defcheck solution-ff255962
  (fn ltp [n]
    (let [npt (fn [s] (mapv + (cons 0 s) (conj s 0)))]
      (nth (iterate npt [1]) (dec n)))))

(defcheck solution-ff46bec2
  (fn [x] (if (= x 1)
            [1]
            (letfn [(fac[n] (if (zero? n) 1 (* n (fac (dec n)))))
                    (c[a b] (/ (fac a) (* (fac b) (fac (- a b)))))]
              (let [half (cons 1 (map #((partial c (dec x)) %) (range 1 (quot x 2))))]
                (if (even? x)
                  (concat half (reverse half))
                  (concat half (conj (reverse half) (c (dec x) (quot x 2))))))))))

(defcheck solution-ff4f5a31
  #(case (first %&) 1 [1] 2 [1 1] 3 [1 2 1] 4 [1 3 3 1] 5[1 4 6 4 1] 11  [1 10 45 120 210 252 210 120 45 10 1]))

(defcheck solution-ff9b7c33
  (fn nth-row [n]
    (loop [res '(1) a (dec n) b 1]
      (if (zero? a)
        res
        (recur (cons (/ (* (first res) a) b) res) (dec a) (inc b))))))

(defcheck solution-ffc0c0f0
  #(nth (iterate (fn [v] (vec (map + (conj v 0) (conj (seq v) 0))))
          [1])
     (- % 1)))

(defcheck solution-ffe4b8c3
  (fn pascal
    ([n]
     (if (= n 1)
       [1]
       (if (= n 2)
         [1 1]
         (let [xs (pascal (dec n)) ys (rest xs)]
           (cons 1 (conj (vec (map + (drop-last xs) ys)) 1))))))))

(defcheck solution-ffec756d
  (fn [n]
    (loop [ret [1] c (dec n)]
      (if (pos? c)
        (recur
          (concat [1] (map (partial apply +) (partition 2 1 ret)) [1])
          (dec c))
        ret))))

(defcheck solution-fff47fe9
  (fn [n]
    (let [f #(apply * (range 1 (inc %)))
          c #(/ (f %) (f %2) (f (- % %2)))
          n' (dec n)]
      (map #(c n' %) (range n)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-97))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

