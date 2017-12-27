(ns coal-mine.problem-44
  (:require [coal-mine.checks :refer [defcheck-44] :rename {defcheck-44 defcheck}]
            [clojure.test]))

(defcheck solution-10016c6e
  (fn [n s] (let [l (count s) m (mod n l) i (if (neg? m) (+ l m) m) [a b] (split-at i s)] (concat b a))))

(defcheck solution-111a3148
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2)
             (take n %2))))

(defcheck solution-112e9796
  (fn rotate-xs [dir xs]
    (let [ln (count xs)]
      (if (pos? dir)
        (take ln (drop dir (cycle xs)))
        (take ln (drop (- ln (mod (* dir -1) ln)) (cycle xs)))))))

(defcheck solution-1196b31b
  (fn [n xs]
    (let [cnt (count xs)
          nm  (mod n cnt)]
      (apply concat
        [(drop nm xs) (take nm xs)]))))

(defcheck solution-11c5ab0c
  (fn [n coll]
    (let [n (if (pos? n)
              n
              (+ (count coll) n))]
      (take (count coll) (drop (+ (count coll) n) (cycle coll))))))

(defcheck solution-1215380c
  (fn cyc [n col]
    (flatten
      (reverse
        (let [m (mod n (reduce (fn [n _] (inc n)) 0 col))]
          (split-at m col)
          )))))

(defcheck solution-12166dfb
  (fn [n xs]
    (apply concat                                           ;(3 4 5 1 2)
      (reverse                                              ;((3 4 5) (1 2))
        (split-at                                           ;[(1 2) (3 4 5)]
          (mod n (count xs))                                ;2
          xs)
        ))))

(defcheck solution-126fe15
  #(mapcat identity (reverse (split-at (mod % (count %2)) %2))))

(defcheck solution-12b94068
  (fn [n coll]
    (let [c  (count coll)
          n' (mod (if (< n 0) (+ c n) n) c)]
      (concat (drop n' coll) (take n' coll)))))

(defcheck solution-12e81c3c
  (fn my-rotate [n a-seq]
    (let [size (count a-seq)]
      (map (fn [index]
             (let [new-index (+ index n) rest-index (rem new-index size)]
               (cond (> 0 rest-index) (nth a-seq (- size (- rest-index)))
                     (>= rest-index size) (nth a-seq (- rest-index size))
                     true (nth a-seq rest-index)
                     )
               )
             )
        (range size)
        )
      )
    ))

(defcheck solution-13167c07
  #(let [t (mod % (count %2))] (concat (drop t %2) (take t %2))))

(defcheck solution-13b290e5
  (fn [unmoddedcnt col]
    (let [cnt (mod unmoddedcnt (count col))]
      (cond (>= cnt 0) (let [split (split-at cnt col)] (concat (second split) (first split)))
            :else (let [split (split-at (+ (count col) cnt) col)] (concat (second split) (first split)))))))

(defcheck solution-146ac353
  (fn [m s] (let [n (count s) k (rem m n)] (take n (drop (if (> k 0) k (+ k n)) (concat s s))))))

(defcheck solution-14e79090
  (fn rot [n s]
    (if (< n 0) (rot (+ (count s) n) s)
                (concat (drop (mod n (count s)) s) (take (mod n (count s)) s)))))

(defcheck solution-157b5f1
  (fn rot [n s]
    (let [d (mod n (count s))
          f (drop d s)
          r (take d s)]
      (concat f r))))

(defcheck solution-15a98f7a
  (fn r [n s]
    (cond
      (<= n 0) (r (+ (count s) n) s)
      (> n (count s)) (r (- n (count s)) s)
      (< n (count s)) (r (inc n) (conj (butlast s) (last s)))
      :else s)))

(defcheck solution-15dcbb21
  (fn rotate [n coll]
    (let [[a b] (split-at (mod (+ n (count coll)) (count coll)) coll)]
      (flatten (concat b a)))))

(defcheck solution-169f7fca
  (fn f [d s]
    (cond
      (zero? d) s
      (pos? d) (f (dec d) (concat (rest s) [(first s)]))
      :else (f (inc d) (concat [(last s)] (butlast s))))))

(defcheck solution-16c02d3e
  (fn [r S] (let [s (mod r (count S))] (concat (drop s S) (take s S)))))

(defcheck solution-174c8733
  (fn [number items]
    (let [limit (Math/abs number) pos (>= number 0)]
      (loop [seq-items items current 0]
        (if (= current limit)
          seq-items
          (recur (if pos
                   (concat (rest seq-items) [(first seq-items)])
                   (concat [(last seq-items)] (butlast seq-items)))
            (inc current)))))))

(defcheck solution-175c5800
  (fn [n input] (let [l (count input) rot (rem n l)]
                  (if (pos? rot)
                    (concat (drop rot input) (take rot input))
                    (concat (drop (+ l rot) input) (take (+ l rot) input))))))

(defcheck solution-175edec0
  (fn [n xs]
    (apply concat (reverse (split-at
                             (mod n (count xs))
                             xs)))))

(defcheck solution-1761a83a
  (fn rotate [n s]
    (let [i (rem n (count s))]
      (cond
        (pos? i) (concat (drop i s) (take i s))
        (neg? i) (concat (take-last (- i) s) (drop-last (- i) s))))))

(defcheck solution-18046178
  (fn [n a-seq]
    (if (or (empty? a-seq) (zero? n))
      a-seq
      (let [c (count a-seq)
            k (if (> (Math/abs n) c) (rem n c) n)
            m (if (neg? k) (+ k c) k)]
        (loop [to-rotate (take m a-seq) acc (vec (drop m a-seq))]
          (if (empty? to-rotate)
            acc
            (recur (rest to-rotate) (conj acc (first to-rotate)))))))))

(defcheck solution-189f6717
  (fn rot [n col]
    (if (zero? n) col
                  (if (> 0 n)
                    (rot (inc n)
                      (cons (last col)
                        (butlast col)))
                    (rot (dec n)
                      (concat (rest col)
                              (list (first col))))))))

(defcheck solution-18a559c1
  #(->> %2
     (split-at (mod %1 (count %2)))
     (reverse)
     (apply concat)))

(defcheck solution-19165275
  (fn [x coll]
    (let [a (mod x (count coll))]
      (concat (drop a coll) (take a coll)))))

(defcheck solution-196ed4c6
  (fn [r s]
    (let [r (mod r (count s))]
      (if (< 0 r)
        (let [a (vec (take-last (- (count s) r) s))
              b (vec (take r s))]
          (into a b))
        (let [r (- r)
              a (vec (take-last r s))
              b (vec (take (- (count s) r) s))]
          (into a b))))))

(defcheck solution-19b77ed6
  (fn rot [n s]
    (let [len (count s)]
      (if (pos? n)
        (take len (drop n (cycle s)))
        (take len (drop (- len (dec n)) (cycle s)))))))

(defcheck solution-19bca5f9
  (fn [n r]
    (cond
      (= 0 n) r
      (> 0 n) (recur (inc n)
                (concat [(last r)]
                        (butlast r)))
      :else (recur (dec n)
              (concat (rest r)
                      [(first r)])))))

(defcheck solution-19ff25c9
  (fn rotate [n s]
    (let [c (count s)
          x (rem n c)
          y (if (pos? x) x (+ c x))]
      (flatten (reverse (split-at y s))))))

(defcheck solution-1a5db9ca
  (fn rot [n coll]
    (cond
      (zero? n)
      coll

      (> n 0)
      (rot (dec n) (concat (rest coll)
                           (vector (first coll))))

      :else
      (rot (inc n) (cons (last coll)
                     (butlast coll))))))

(defcheck solution-1ab45af8
  #(let [i (mod %1 (count %2))]
     (concat (drop i %2) (take i %2))))

(defcheck solution-1abe75e0
  #(let [rot (mod %1 (count %2))]
     (concat (drop rot %2) (take rot %2))))

(defcheck solution-1b22c948
  ;(fn [n xs]
  ;  (let [cnt (count xs)
  ;        n (mod n (count xs))
  ;        xs (cycle xs)]
  ;    (let [needed (if (pos? n) (+ n cnt) (+ n (* 2 cnt)))]
  ;      (drop (- needed cnt) (take needed xs)))))

  ; 1 2 3 4 5 1 2 3 4 5
  ;     ^ ^ ^ ^ ^         3..7

  ; 1 2 3 4 5 1 2 3 4 5
  ;       ^ ^ ^ ^ ^       4..8

  (fn [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs) (take n xs)))))

(defcheck solution-1b694f71
  (fn r [n s]
    (let [c  (count s)
          nn (if (neg? n)
               (+ c n)
               n)
          nn (mod nn c)]
      (flatten [(drop nn s) (take nn s)]))))

(defcheck solution-1b739f2c
  (fn [n c] (let [n (mod n (count c))]
              (concat (drop n c) (take n c)))))

(defcheck solution-1b82a3e3
  (fn rotate [k coll]
    (let [shift (mod k (count coll))]
      (concat (drop shift coll) (take shift coll)))))

(defcheck solution-1b9c634b
  (fn [l c] (let [x (mod l (count c)) r (concat (drop x c) (take x c))] r)))

(defcheck solution-1bf422c4
  #(let [x (rem (+ (count %2) (rem %1 (count %2))) (count %2))]
     (flatten [(drop x %2) (take x %2)])))

(defcheck solution-1c8f1498
  (fn
    [n s]
    (let [c (count s)
          n (mod n c)]
      (flatten [(drop n s) (take n s)]))))

(defcheck solution-1e575c5d
  (fn [n s]
    (let [c (count s)
          n (mod n c)]
      (->> s
        cycle
        (drop n)
        (take c)))))

(defcheck solution-1e6b82f4
  #(let [ct (count %2) cyc (cycle %2)]
     (take ct (drop (mod %1 ct) cyc))))

(defcheck solution-1e827458
  (fn rotate [n lat]
    (if (>= n 0)
      (loop [counter 0
             ans     lat]
        (if (= counter n)
          ans
          (recur (inc counter) (concat (rest ans) `(~(first ans))))))
      (loop [counter 0
             ans     lat]
        (if (= counter (- n))
          ans
          (recur (inc counter) (concat `(~(last ans)) (take (dec (count lat)) ans))))))))

(defcheck solution-1f2643a2
  (fn [n s]
    (apply (fn [h & t] (flatten (concat t h)))
      (partition-all
        (#(if (< % 0) (+ (count s) %) %)
         (mod n (count s)))
        s))))

(defcheck solution-1f451bbf
  (fn rotate
    [step coll]
    (let [step (if (> (Math/abs step) (count coll)) (mod step (count coll)) step)]
      (if (pos? step)
        (concat (take-last (- (count coll) step) coll) (take step coll))
        (concat (take-last (- step) coll) (take (- (count coll) (- step)) coll))
        ))))

(defcheck solution-1fe65910
  (fn [n coll]
    (let [size   (count coll),
          offset (loop [k n]
                   (cond (< k 0) (recur (+ k size))
                         (< size k) (recur (- k size))
                         :else k))]
      (concat (drop offset coll) (take offset coll)))))

(defcheck solution-20448223
  #(apply concat ((juxt drop take) (mod % (count %2)) %2)))

(defcheck solution-20fd8e2d
  #(if (> %1 0)
     (concat (drop (rem %1 (count %2)) %2) (take (rem %1 (count %2)) %2))
     (concat (drop (+ (rem %1 (count %2)) (count %2)) %2) (take (+ (rem %1 (count %2)) (count %2)) %2))))

(defcheck solution-213bc066
  (fn [n col]
    (let [size (count col) r (mod n size) f (drop r col)]
      (concat f (take r col)))))

(defcheck solution-213fb651
  (fn shift [pos acoll]
    (let [size   (count acoll)
          offset (mod pos size)]
      (take size (drop offset (cycle acoll))))))

(defcheck solution-21d38fc6
  #(let [n (count %2)] (->> %2 cycle (drop (mod %1 n)) (take n))))

(defcheck solution-21da74f
  (fn [n l]
    (let [n (mod n (count l))]
      (reduce conj (take n l) (reverse (drop n l))))))

(defcheck solution-227a3a51
  (fn [n coll]
    (let [n1 (mod n (count coll))]
      (apply concat ((juxt drop take) n1 coll)))))

(defcheck solution-22dcc17d
  (fn prob-0044
    [in-n xs]
    (let [n (rem in-n (count xs))]
      (if (neg? n)
        (prob-0044 (+ (count xs) n) xs)
        (concat (drop n xs) (take n xs))))))

(defcheck solution-235d2479
  (fn [x ls]
    (if (= x 0)
      ls
      (if (> x 0)
        (recur (- x 1) (concat (rest ls) (list (first ls))))
        (recur (+ x (count ls)) ls)
        )
      )
    ))

(defcheck solution-238688fc
  (fn shift [n coll]
    (let [k (if (< n 0) (+ n (count coll)) n)]
      (if (= 0 k) coll
                  (shift (dec k) (conj (apply vector (drop 1 coll)) (first coll)))))))

(defcheck solution-23a0942
  (fn foo [n s]
    (let [c (count s) r (rem n c)]
      (if (neg? r)
        (concat (drop (+ c r) s) (take (+ c r) s))
        (concat (drop r s) (take r s))))))

(defcheck solution-2532d876
  (fn [n s]
    (let [l (count s)
          m (mod n l)]
      (take l (drop m (cycle s))))))

(defcheck solution-253b061c
  (fn [x coll]
    (let [len  (count coll)
          step (let [re-num (rem x len)]
                 (if (< re-num 0)
                   (+ len re-num)
                   re-num))]
      (take len (drop step (concat coll coll))))))

(defcheck solution-25a33b89
  (fn [n s]
    (loop [cnt n res s]
      (if (= cnt 0) res
                    (if (< n 0)
                      (recur (inc cnt) (conj (drop-last res) (last res)))
                      (recur (dec cnt) (conj (apply vector (rest res)) (first res)))
                      )
                    )
      )
    ))

(defcheck solution-25a866dd
  #(let [x (mod %1 (count %2))]
     (concat (drop x %2) (take x %2))))

(defcheck solution-25f515fc
  (fn [n col]
    (let [c (count col)]
      (cond
        (= n 0) col
        (= n c) col
        (< n 0) (recur (+ n c) col)
        (> n c) (recur (- n c) col)
        :else
        (concat (drop n col) (take n col))
        )
      )
    ))

(defcheck solution-260c3c0e
  (fn [rot seq]
    (let [n (count seq)
          r (mod rot n)]
      (concat (drop r seq) (take r seq)))))

(defcheck solution-2624348a
  (fn [rotation array]
    (let [rotation (rem rotation (count array))]
      (if (pos? rotation)
        (concat (drop rotation array) (take rotation array))
        (concat (take-last (- rotation) array) (drop-last (- rotation) array))))))

(defcheck solution-26288f03
  (fn rotate [n coll]
    (let [inf (reduce concat (repeat 100 coll))]
      (if (>= n 0)
        (take (count coll) (nthnext inf n))
        (rotate (+ (* (- n) (count coll)) n) coll)))))

(defcheck solution-2661b598
  (fn rotate [n coll]
    (cond
      (neg? n) (recur (+ (count coll) n) coll)
      (> n (count coll)) (recur (mod n (count coll)) coll)
      :else (let [l (take n coll)
                  r (drop n coll)]
              (concat r l)))))

(defcheck solution-26cdd0d2
  (fn [s xs] (let [c (count xs) s (rem (+ (rem s c) c) c)] (->> (concat xs xs) (drop s) (take c)))))

(defcheck solution-27154b0c
  #(let [c (count %2) p (mod % c)]
     (concat (drop p %2) (take p %2))))

(defcheck solution-272f4922
  (fn f [n coll]
    (cond
      (< 0 n) (f (dec n) (concat (rest coll) (list (first coll))))
      (> 0 n) (f (inc n) (concat (list (last coll)) (butlast coll)))
      :else coll)))

(defcheck solution-2752a3de
  (fn [n s] (let [c (cycle s), m (count s), d (if (< n 0) (mod n m) n)]
              (->> (drop d c) (take m)))))

(defcheck solution-27919248
  #(apply concat (reverse (split-at
                            (if (neg? %)
                              (+ (count %2) (rem % (count %2)))
                              (rem % (count %2))) %2))))

(defcheck solution-27e465fb
  (fn [n x] (take (count x) (drop (mod n (count x)) (concat x x)))))

(defcheck solution-2819a95d
  (fn aa [xx yy]
    (for [x (range (count yy))] (nth yy (mod (+ x xx) (count yy))))
    ))

(defcheck solution-285f1234
  (fn [i coll]
    (let [c (count coll)]
      (->> coll
        cycle
        (drop (mod i c))
        (take c)))))

(defcheck solution-2874bd9b
  (fn [n coll]
    (let [r (mod n (count coll))]
      (concat (drop r coll) (take r coll))
      )))

(defcheck solution-288e77c9
  (fn [r l]
    (letfn [(regr
              [r]
              (let [cl (count l)]
                (if (< r 0) (regr (+ r cl))
                            (if (> r cl) (regr (- r cl))
                                         r))))]
      (let [rr (regr r)]
        (concat (drop rr l) (take rr l))
        ))))

(defcheck solution-297e3d91
  (fn rot [n s]
    (cond
      (> 0 n) (rot (inc n) (cons (last s) (drop-last s)))
      (< 0 n) (rot (dec n) (concat (drop 1 s) (take 1 s)))
      :else s
      )
    ))

(defcheck solution-29ce3a31
  #(last (take (inc (mod %1 (count %2))) (iterate (fn [[h & t]] `[~@t ~h]) %2))))

(defcheck solution-29e85cf0
  (fn [n coll]
    (take (count coll) (drop (mod n (count coll)) (cycle coll)))))

(defcheck solution-29edc7fc
  (fn rot [ct seq]
    (cond
      (= ct 0) seq
      (< ct 0) (rot (+ (count seq) ct) seq)
      (> ct 0) (rot (dec ct) (concat (rest seq) [(first seq)]))
      )
    ))

(defcheck solution-2a22b406
  (fn [n c]
    (let [s (mod n (count c))]
      (concat (drop s c)
              (take s c)))))

(defcheck solution-2a5130e
  (fn [c x]
    (take (count x) (drop (mod c (count x)) (cycle x)))))

(defcheck solution-2ab15e32
  (fn [n a]
    (let [y (mod n (count a))]
      (concat (drop y a) (take y a)))))

(defcheck solution-2b15fe55
  #(let [rot (mod (+ %1 (count %2)) (count %2))]
     (concat (drop rot %2) (take rot %2))))

(defcheck solution-2b91e328
  (fn [a s]
    (nth
      (iterate #(apply concat
                  (reverse (split-at 1 %)))
        s)
      (mod a (count s)))))

(defcheck solution-2baf817c
  #(let [the-count (count %2)
         the-pos   (mod % the-count)]
     (reduce concat () (reverse (split-at the-pos %2)))
     ))

(defcheck solution-2bcdcd9c
  (fn
    [mov s]
    (let [mov (rem mov (count s))]
      (cond (pos? mov) (concat (drop mov s) (take mov s))
            (neg? mov) (let [mov (- mov)]
                         (concat (take-last mov s) (drop-last mov s)))
            :else s))))

(defcheck solution-2bef089
  (fn [n c]
    (take (count c) (drop (mod n (count c)) (cycle c)))))

(defcheck solution-2ca4c2c
  #(let [[a b] (split-at (mod % (count %2)) %2)] (concat b a)))

(defcheck solution-2e61b985
  #(let [m (mod % (count %2))] (concat (drop m %2) (take m %2))))

(defcheck solution-2e7099ac
  (fn [n coll]
    (let [r (mod n (count coll))]
      (concat (drop r coll)
              (take r coll)))))

(defcheck solution-2e82aee2
  (fn [n coll]
    (let [c (count coll)]
      (take c
        (#(if (pos? n)
            (drop n %)
            (drop (+ c (rem n c)) %))
         (cycle coll))))))

(defcheck solution-2eb9be60
  (fn [n v]
    (let [x (mod (+ n (count v)) (count v))]
      (concat (drop x v) (take x v)))))

(defcheck solution-2ee28df1
  #(->> %2
     (split-at (mod %1 (count %2)))
     reverse
     (apply concat)
     ))

(defcheck solution-2ef3186b
  (fn [x col]
    (let [h (mod x (count col))]
      (flatten (conj (take h col) (drop h col)))
      )))

(defcheck solution-2f096cba
  #(let [m (mod %1 (count %2))] (concat (drop m %2) (take m %2))))

(defcheck solution-2f343e68
  (fn rotate [x s]
    (loop [n x s1 s]
      (cond
        (= n 0) s1
        (> n 0) (recur (dec n) (concat (drop 1 s1) (take 1 s1)))
        (< n 0) (recur (inc n) (concat (take-last 1 s1) (take (- (count s) 1) s1)))
        ))))

(defcheck solution-2f38e652
  (fn [n coll] (take (count coll) (drop (mod n (count coll)) (cycle coll)))))

(defcheck solution-2f7997f3
  (fn rot [n coll]
    (let [k (mod n (count coll))]
      (concat (drop k coll) (take k coll)))))

(defcheck solution-2f913fff
  (fn rotate-seq [n xs]
    (let [xc (count xs)
          n  (mod n xc)]
      (take xc (drop n (take (* 2 n xc) (apply concat (repeat xs))))))))

(defcheck solution-2ff1b251
  (fn rotate [n coll]
    (let [p (mod n (count coll))]
      (concat (drop p coll)
              (take p coll)))))

(defcheck solution-2ffc8ccc
  (fn rotate [n seq]
    (map #(nth seq %) (map #(mod (+ n %) (count seq)) (range 0 (count seq))))))

(defcheck solution-305c5a5
  (fn [n coll] (take (count coll) (drop (mod n (count coll)) (cycle coll)))))

(defcheck solution-30b32070
  #(let [k (mod % (count %2))]
     (concat (drop k %2) (take k %2))))

(defcheck solution-30f1b8d2
  (fn [n coll]
    (let [real-n (mod n (count coll))]
      (concat (drop real-n coll) (take real-n coll)))))

(defcheck solution-30fb3f61
  (fn [n coll]
    (cond (zero? n) coll
          (pos? n) (recur (dec n) (concat (next coll) (list (first coll))))
          (neg? n) (recur (inc n) (concat (list (last coll)) (butlast coll))))))

(defcheck solution-30ffe242
  (fn rotate [n col]
    (let [n (mod n (count col))]
      (if (> 0 n) (rotate (+ (count col) n) col)
                  (let [left  (take n col)
                        right (drop n col)]
                    (concat right left))))))

(defcheck solution-314327a4
  #(concat (nthrest %2 (mod %1 (count %2))) (drop-last (- (count %2) (mod %1 (count %2))) %2)))

(defcheck solution-318d9cb8
  (fn [rot xs]
    (let [len (count xs)]
      (map-indexed (fn [index item]
                     (nth xs (mod (+ index rot) len)))
        xs))))

(defcheck solution-31c4ca8e
  (fn rotate [n seq]
    (if (> n 0)
      (rotate (- n (count seq)) seq)
      (loop [n   n
             seq seq]
        (if (zero? n)
          seq
          (recur (inc n) (cons (last seq) (drop-last seq))))))))

(defcheck solution-32809208
  (fn [offset coll]
    (let [coll-size  (count coll)
          offset-pos (mod offset coll-size)]
      (take coll-size (drop offset-pos (cycle coll))))))

(defcheck solution-329da3ba
  (fn [n s]
    (let [n (mod n (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-32ae2b67
  (fn [x coll]
    (let [m (rem x (count coll))
          n (if (< m 0)
              (+ m (count coll))
              m)]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-32e1d80e
  (fn [x col]
    (let [n (count col)]
      (cond (> 0 x) (let [m (+ n (rem x n))]
                      (concat (drop m col) (take m col)))
            (> x n) (let [m (rem x n)]
                      (concat (drop m col) (take m col)))
            :else (concat (drop x col) (take x col))))))

(defcheck solution-334bd928
  (fn [t s]
    (let [s-size (count s)
          real-t (rem (+ (* 2 s-size) t) s-size)
          rotate #(conj (into [] (rest %)) (first %))]
      (nth (iterate rotate s) real-t))))

(defcheck solution-3353eba0
  (fn rotate [n xs]
    (cond (= n 0) xs
          (< n 0) (rotate (inc n) (cons (last xs) (butlast xs)))
          :else (rotate (dec n) (conj (into [] (rest xs)) (first xs))))))

(defcheck solution-3355e9fe
  (fn f [m a]
    (let [n (mod (+ (count a) m) (count a))]
      (concat (drop n a) (take n a)))))

(defcheck solution-337fcfa3
  (fn [n c] (if (neg? n)
              (reverse (#(loop [n %, c %2] (if (> n 0) (recur (dec n) (-> (next c) vec (conj (first c)))) (seq c))) (- n) (reverse c)))
              (#(loop [n %, c %2] (if (> n 0) (recur (dec n) (-> (next c) vec (conj (first c)))) (seq c))) n c))
    ))

(defcheck solution-3395fcd6
  #(->> (concat %2 %2) (drop (mod %1 (count %2))) (take (count %2))))

(defcheck solution-33dc6a4c
  (fn rot [n coll]
    (cond
      (< n 0) (recur (+ n (count coll)) coll)
      (> n (count coll)) (recur (- n (count coll)) coll)
      :else (concat (drop n coll) (take n coll)))))

(defcheck solution-33ffaf38
  (fn [n coll]
    (let [n (- 0 (mod n (count coll)))]
      (loop [i 0 res coll]
        (if (= i (if (< n 0) (+ (count coll) n) n))
          res
          (recur (inc i) (conj (take (dec (count res)) res) (last res)))
          )
        )
      )
    ))

(defcheck solution-342820ca
  (fn go [offset seq]
    (let [len (count seq)]
      (if (> offset 0)
        (take len (drop offset (cycle seq)))
        (go (+ len offset) seq)))))

(defcheck solution-34b72918
  (fn rotate [n xs]
    (let
     [c  (count xs)
      n' (rem n c)]
      (take c (drop (+ n' c) (cycle xs)))
      )
    ))

(defcheck solution-355423c8
  #(let [s (mod % (count %2))]
     (concat (drop s %2) (take s %2))))

(defcheck solution-3560331e
  (fn rotate [n l]
    (cond
      (> n 0)
      (rotate (dec n) (concat (rest l) [(first l)]))
      (< n 0)
      (rotate (inc n) (concat [(last l)] (butlast l)))
      :else l)))

(defcheck solution-35789ec9
  (fn [n col]
    (let [m (count col)]
      (take m (drop (mod n m) (apply concat (repeat 2 col)))))))

(defcheck solution-3595f8ca
  #(let [c (count %2)]
     (take c
       (drop (+ c c %)
         (cycle %2)))))

(defcheck solution-35ac40fa
  #(let [c (count %2)]
     (take c (drop (mod %1 c) (cycle %2)))))

(defcheck solution-36125cf9
  (fn [r coll] (loop [n (mod r (count coll)) c coll a []] (if (zero? n) (into (vec c) a) (recur (dec n) (rest c) (conj a (first c)))))))

(defcheck solution-363a6ef6
  #(let [c (count %2) d (if (pos? %) % (+ c (rem % c)))]
     (take c (drop d (cycle %2)))))

(defcheck solution-364051c5
  (fn [n sq]
    (let [nm (mod (if (neg? n) (* n -1) n) (count sq))]
      (if (neg? n)
        (flatten (list (take-last nm sq) (drop-last nm sq)))
        (flatten (list (drop nm sq) (take nm sq)))))))

(defcheck solution-365b0e03
  (fn [n s] (let [n (if (< n 0) (+ (count s) n) n) n (mod n (count s))] (concat (drop n s) (take n s)))))

(defcheck solution-3663ea5a
  (fn [x c]
    (let [y  (mod x (count c)),
          c2 (split-at y c)]
      (concat (last c2) (first c2))
      )
    ))

(defcheck solution-36b5dc45
  (fn rot [n l]
    (cond
      (= 0 n) l
      (> 0 n) (reverse (rot (- n) (reverse l)))
      (> n (count l)) (rot (rem n (count l)) l)
      :else (concat (nthrest l n) (take n l)))))

(defcheck solution-36d33a17
  (fn [n xs]
    (let [i (mod n (count xs))]
      (flatten (reverse (split-at i xs)))
      )
    ))

(defcheck solution-37049a15
  (fn [n [head & tail :as xs]]
    (let [n (mod n (count xs))]
      (if (= n 0)
        xs
        (recur (dec n) (conj (vec tail) head))))))

(defcheck solution-37b801ee
  (fn [n coll]
    (take (count coll) (drop (mod n (count coll)) (flatten [coll coll])))))

(defcheck solution-37da0cbf
  (fn [n l] (let [i (mod n (count l))]
              (concat (drop i l) (take i l)))))

(defcheck solution-3809a281
  (fn [n xs]
    (let [n (mod n (count xs))]
      (if (zero? n)
        xs
        (recur (dec n) (concat (rest xs) (list (first xs))))))))

(defcheck solution-3824f098
  (fn rotate-seq
    [n coll]
    (let [[a b] (split-at (mod n (count coll)) coll)]
      (concat b a))))

(defcheck solution-3856d36a
  (fn [n xs]
    (let [i (mod n (count xs))
          [l r] (split-at i xs)]
      (concat r l))))

(defcheck solution-386df6b5
  (fn [i xs]
    (let [n   (count xs)
          i'  (rem i n)
          i'' (if (neg? i') (+ n i') i')
          [h t] (split-at i'' xs)]
      (concat t h))))

(defcheck solution-3889de89
  (fn rotx [n coll]
    (letfn [(sgn [n]
              (if (zero? n) 0
                            (if (pos? n) 1 -1)))]
      (loop [rct n
             s   coll]
        (condp = (sgn rct)
          0 s
          1 (recur (dec rct) (concat (rest s) [(first s)]))
          -1 (recur (inc rct) (conj (butlast s) (last s))))))))

(defcheck solution-38c152da
  (fn [n xs]
    (let [n (mod n (count xs))] (concat (drop n xs) (take n xs)))))

(defcheck solution-39fc1465
  (fn [n xs]
    (-> n
      (+ (count xs))
      (mod (count xs))
      (split-at xs)
      reverse
      (->> (apply concat)))))

(defcheck solution-3a9139e8
  (fn my-rotate [n s]
    (apply concat
      (reverse (split-at (mod n (count s)) s)))))

(defcheck solution-3b6e3f50
  (fn [n_ coll]
    (let
     [cyc (cycle coll)
      cnt (count coll)
      n   (rem (+ (rem n_ cnt) cnt) cnt)]
      (concat (take (- cnt n) (drop n cyc)) (take n cyc)))))

(defcheck solution-3bbcbea0
  (fn rotate
    [r s]
    (let [reduced-r (rem r (count s))
          real-r    (if (neg? reduced-r)
                      (+ reduced-r (count s))
                      reduced-r)]
      (concat (drop real-r s) (take real-r s)))))

(defcheck solution-3c6a3c5b
  (fn rt [n c]
    (cond
      (pos? n) (rt (dec n) (concat (rest c) (list (first c))))
      (neg? n) (rt (inc n) (cons (last c) (butlast c)))
      :else c)))

(defcheck solution-3c76c697
  #(let [len (count %2) n (- %1 (* len (Math/floor (/ %1 len))))] (concat (drop n %2) (take n %2))))

(defcheck solution-3c8eaa26
  (fn [n coll]
    (let [absn  (Math/abs n)
          step  (/ n absn)
          shift (fn [c]
                  (if (pos? step)
                    (concat (rest c) [(first c)])
                    (cons (last c) (butlast c))))]
      (nth (iterate shift coll) absn))))

(defcheck solution-3c8fab01
  (fn rotate [k xs]
    (let [k' (mod k (count xs))
          [left right] (split-at k' xs)]
      (concat right left))))

(defcheck solution-3cd66261
  #(let [i (rem %1 (count %2))]
     (cond (= i 0) %2
           (> i 0) (concat (drop i %2) (take i %2))
           :else (concat (take-last (- i) %2) (drop-last (- i) %2)))))

(defcheck solution-3d64e04
  (fn [n coll]
    (let [index (mod n (count coll))]
      (flatten (into [] (reverse (split-at index coll)))))))

(defcheck solution-3d6e2ccc
  #(loop [x %1, l %2, r l]
     (if (= 0 x)
       r
       (if (< x 0)
         (recur (inc x) l (concat (list (last r)) (take (dec (count r)) r)))
         (recur (dec x) l (concat (rest r) (list (first r))))))))

(defcheck solution-3dfe4428
  (fn [n xs] (let [len    (count xs)
                   n      (mod n len)
                   rotate (if (>= n 0) n (+ len n))]
               (concat (drop rotate xs) (take rotate xs)))
    ))

(defcheck solution-3e431fd0
  (fn [n sx] (#(concat (last %) (first %)) (split-at (mod n (count sx)) sx))))

(defcheck solution-3e472293
  (fn rot [n coll]
    (let [m (count coll)]
      (map
        #(nth coll (mod (+ n %1) m))
        (range 0 m)))))

(defcheck solution-3eb24b5d
  #(let [p (mod % (count %2))] (concat (drop p %2) (take p %2))))

(defcheck solution-3f459f09
  (fn
    [n coll]
    (let [n (mod n (count coll))
          [a b] (split-at n coll)]
      (concat b a))))

(defcheck solution-3fca171b
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-403bb3d6
  (fn rotate [n x]
    (if (or (empty? x) (= n 0))
      x
      (if (= n 1)
        (conj (vec (rest x)) (first x))
        (if (= n -1)
          (concat (vector (peek (vec x))) (pop (vec x)))
          (if (pos? n)
            (if (= (mod n (count x)) 0)
              x
              (rotate (- n 1) (conj (vec (rest x)) (first x))))
            (if (= (mod (- 0 n) (count x)) 0)
              x
              (rotate (+ n 1) (concat (vector (peek (vec x))) (pop (vec x)))))))))))

(defcheck solution-40a21a3f
  (fn [r l]
    (let [r (mod r (count l)) b (take r l), e (drop r l)]
      (concat e b))))

(defcheck solution-40f04b77
  (fn [n xs] (let [c (count xs) n (mod n c)]
               (take c (drop n (cycle xs))))))

(defcheck solution-4136b33f
  (fn [num coll] (let [num-elems (count coll)
                       abs-n     (if (pos? num) num (* num -1))
                       n-temp    (if (> abs-n num-elems) (mod abs-n num-elems) abs-n)
                       n         (if (> 0 num) (+ num-elems (* -1 n-temp)) n-temp)]
                   (flatten (conj (take n coll) (drop n coll))))))

(defcheck solution-414a969c
  (fn rot [n s]
    (let [rotfun (if (pos? n)
                   (fn lazy-rotl [s] (lazy-seq (cons s (lazy-rotl (concat (rest s) (list (first s)))))))
                   (fn lazy-rotr [s] (lazy-seq (cons s (lazy-rotr (concat (list (last s)) (take (dec (count s)) s))))))
                   )
          n      (Math/abs n)]
      (first (drop n (take (inc n) (rotfun s)))))))

(defcheck solution-415d47f0
  #(let [i (if (< % 0) (+ (count %2) %) %)
         n (mod i (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-417e00ba
  (fn [delta xs]
    (let [len (count xs)]
      (take len (drop (mod delta len) (cycle xs))))))

(defcheck solution-423e27f3
  (fn [n coll]
    (let [c (count coll) n (mod (if (even? n) n (+ c n)) c)]
      (take c (drop n (cycle coll))))))

(defcheck solution-4254043e
  #(let [x (count %2) y (mod % x)] (concat (drop y %2) (take y %2))))

(defcheck solution-426eb34a
  (fn [n coll]
    (let [c (mod n (count coll))]
      (concat (drop c coll) (take c coll)))))

(defcheck solution-42d5a7e6
  (fn x [n coll]
    (let [n (mod n (count coll))]
      (if (>= n 0)
        (concat (drop n coll) (take n coll))
        (concat (take-last (- n) coll) (drop-last (- n) coll))))))

(defcheck solution-4352572e
  (fn rotate [n seq]
    (let [len (count seq)]
      (take len (drop (mod n len) (cycle seq))))))

(defcheck solution-4367fdb4
  (fn rotate [by lst]
    (cond
      (zero? by) lst
      (neg? by) (recur
                  (inc by)
                  (cons
                    (last lst)
                    (butlast lst)))
      :else (recur
              (dec by)
              (concat
               (rest lst)
               (list
                 (first lst)
                 ))))))

(defcheck solution-436ae399
  (fn rot [pos coll]
    (let [len  (count coll)
          npos (loop [p pos]
                 (if (> 0 p)
                   (recur (+ p len))
                   p))
          ]
      (take len (drop npos (cycle coll))))))

(defcheck solution-43f116b4
  #(let [rotate (fn [amt col] (concat (nthrest col amt) (take amt col)))
         i      (Math/abs (rem %1 (count %2)))
         res    (if (< %1 0) (reverse (rotate i (reverse %2))) (rotate i %2))]
     res))

(defcheck solution-4464bb20
  (fn [n c]
    (letfn [(rot1 [[x & r]] (conj (vec r) x))]
      (nth (iterate rot1 c) (mod n (count c))))))

(defcheck solution-44664b75
  (fn rot [n col]
    (if (< n 0)
      (rot (+ (count col) n) col)
      (if (= n 0)
        col
        (concat (rest (rot (- n 1) col)) (conj (empty col) (first (rot (- n 1) col))))
        )
      )
    ))

(defcheck solution-44ab68ae
  (fn [n coll]
    (let
     [len (count coll)
      rot (mod n len)
      inf (drop rot (cycle coll))]
      (take len inf)
      )))

(defcheck solution-44f1e5b9
  (fn rotate [n l]
    (if
     (< n 0)
      (reverse (rotate (- 0 n) (reverse l)))
      (if
       (= n 0)
        l
        (rotate (- n 1) (concat (rest l) [(first l)]))
        )
      )
    ))

(defcheck solution-452dbe66
  (fn [n s]
    (let [c (mod n (count s))]
      (concat (drop c s) (take c s)))))

(defcheck solution-45454adf
  (fn [r s]
    (let [rot (mod r (count s))]
      (concat (nthrest s rot) (take rot s)))))

(defcheck solution-45722faf
  (fn [at s]
    ((fn [[a b]] (concat b a))
     (split-at
       (mod (if (neg? at) (inc (- at)) at) (count s))
       s))))

(defcheck solution-45a8599d
  (fn rot [n s]
    (take (count s) (drop (mod n (count s)) (cycle s)))))

(defcheck solution-45b3d083
  (fn [n coll]
    (let [rotate (mod n (count coll))]
      (concat (drop rotate coll) (take rotate coll)))))

(defcheck solution-4652db66
  (fn rotate [n l]
    (if (= 0 n) l
                (recur (if (pos? n) (dec n) (inc n))
                  (if (pos? n)
                    (concat (rest l) [(first l)])
                    (concat [(last l)] (drop-last l)))))))

(defcheck solution-46ad7de8
  #(take (count %2) (drop (mod %1 (count %2)) (flatten (repeat 2 %2)))))

(defcheck solution-46c66d9a
  (fn [n coll]
    (let [c      (count coll)
          offset (mod (+ c n) c)]
      (apply concat (reverse (split-at offset coll))))))

(defcheck solution-46e179f6
  (fn rotate [n x]
    (let [y (into [] x)]
      (cond
        (< n 0) (rotate (inc n) (into [(last y)] (pop y)))
        (> n 0) (rotate (dec n) (conj (into [] (rest y)) (first y)))
        :else y
        )
      )))

(defcheck solution-46f17ba4
  (fn [n s] (-> (mod n (count s)) (split-at s) reverse flatten)))

(defcheck solution-4715b124
  (fn [s col]
    (let [co   (count col)
          s    (mod s co)
          slst (apply concat (take 2 (repeat col)))
          ]
      (drop s (take (+ co s) slst)))))

(defcheck solution-47351a9e
  (fn [n l] (
             (fn f [n l]
               (if (= n 0)
                 l
                 (f (- n 1) (cons (last l) (butlast l)))
                 )
               )
             (mod (* -1 n) (count l)) l
             )
    ))

(defcheck solution-475126ba
  (fn [n s]
    (let [l (count s)]
      (if (> n 0)
        (take l (drop n (cycle s)))
        (take l (drop (mod n l) (cycle s)))))))

(defcheck solution-4781b594
  (fn [n s]
    (let [c (count s) v (rem (+ (* c (max (- n) n)) n) c)]
      (concat (drop v s) (take v s)))))

(defcheck solution-47bd5d14
  (fn [n xs]
    (let [z (mod n (count xs))]
      (concat (drop z xs) (take z xs)))))

(defcheck solution-47bf4ec3
  (letfn [(sh-l [x] (concat (rest x) [(first x)]))
          (sh-r [x] (concat [(last x)] (butlast x)))]
    (fn sh [n coll]
      (if (< n 0)
        (if (< n -1) (sh (inc n) (sh-r coll)) (sh-r coll))
        (if (> n 1) (sh (dec n) (sh-l coll)) (sh-l coll))))))

(defcheck solution-47d00af8
  #(let [c (count %2)] (take c (drop (mod % c) (cycle %2)))))

(defcheck solution-47e6f4e7
  (fn [n l]
    (let [n (mod n (count l))]
      (concat (drop n l) (take n l)))))

(defcheck solution-4807022c
  (fn rs [x y]
    (concat (drop (mod x (count y)) y)
            (take (mod x (count y)) y))))

(defcheck solution-48be55a2
  (fn [n coll]
    (let [len (count coll)
          m   (mod n len)
          [pre post] (split-at m coll)]
      (concat post pre))))

(defcheck solution-4941da83
  (fn [r items]
    (concat (drop (mod r (count items)) items)
            (take (mod r (count items)) items))))

(defcheck solution-49bd9eed
  (fn [positions coll]
    (let [n (count coll)]
      (take n (drop (mod positions n) (cycle coll))))))

(defcheck solution-49eb4e47
  (fn rotate [howmuch sq]
    (cond
      (= howmuch 0)
      sq
      (< howmuch 0)
      (recur (inc howmuch) (concat (list (last sq)) (butlast sq)))
      :else
      (recur (dec howmuch) (concat (rest sq) (list (first sq)))))))

(defcheck solution-4a040ba9
  (fn [n s] (concat (nthrest s (mod n (count s))) (take (mod n (count s)) s))))

(defcheck solution-4a2af6ec
  (fn [n s]
    (let [p (mod n (count s))]
      (reverse
        (concat
         (reverse (take p s))
         (reverse (drop p s)))))))

(defcheck solution-4a3b858f
  #(let [l (count %2)] (take l (drop (+ l l %) (cycle %2)))))

(defcheck solution-4a4a7994
  (fn [n xs]
    (let [r (mod n (count xs))
          [ys zs] (split-at r xs)]
      (concat zs ys))))

(defcheck solution-4a706c2f
  (fn rot [n s]
    (let [x (mod n (count s))]
      (concat (drop x s) (take x s)))))

(defcheck solution-4b1e9318
  #(let [c (count %2)
         i (mod %1 c)]
     (cond
       (pos? i) (concat (drop i %2) (take i %2))
       (neg? i) (concat (take-last i %2) (drop-last i %2))
       :else %2)))

(defcheck solution-4c116c5f
  #(concat (drop (mod %1 (count %2)) %2) (take (mod %1 (count %2)) %2)))

(defcheck solution-4c137c4
  (fn [rot sequence]
    (let [forward-rot (mod rot (count sequence))
          [a b] (split-at forward-rot sequence)]
      (concat b a))))

(defcheck solution-4c7ec815
  #(flatten (apply conj (split-at (mod % (count %2)) %2))))

(defcheck solution-4c8184c5
  (fn rotate [n lis]
    (if (> n 0)
      (rotate (dec n) (concat (rest lis) [(first lis)]))
      (if (< n 0)
        (rotate (inc n) (concat [(last lis)] (drop-last lis)))
        lis))
    ))

(defcheck solution-4ca0634e
  (fn rotate
    [steps coll]
    "Rotates the collection. That is, start the collection with the element reached
  by adding steps to the index of the first element."
    (let [collSize (count coll)
          toDrop   (mod steps collSize)]
      (concat (drop toDrop coll) (take toDrop coll)))))

(defcheck solution-4cadfd15
  #(let [c (count %2) x (rem %1 c) s (if (pos? %1) x (+ c x)) [a b] (split-at s %2)] (concat b a)))

(defcheck solution-4cb57826
  #(mapcat (fn [f] (f (mod % (count %2)) %2)) [drop take]))

(defcheck solution-4de4aab
  (fn my-rotate [n coll]
    (cond
      (= n 0) coll
      (> n 0) (my-rotate (dec n) (concat (rest coll) (list (first coll))))
      (< n 0) (my-rotate (inc n) (concat (list (last coll)) (butlast coll))))))

(defcheck solution-4e041e16
  (fn [n s]
    (let [m (mod n (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-4e281832
  (fn [a b] (apply concat (reverse (split-at (mod a (count b)) b)))))

(defcheck solution-4e33504c
  #(let [rotate (fn [amt col] (concat (nthrest col amt) (take amt col)))
         a      (rem % (count %2))]
     (if (neg? a)
       (rotate (+ (count %2) a) %2)
       (rotate a %2))))

(defcheck solution-4eccca4
  (fn [n s]
    (let [c (mod n (count s))]
      (concat (drop c s) (take c s)))))

(defcheck solution-4edd7065
  (fn [n s]
    (let [shift (mod n (count s))]
      (concat (nthrest s shift) (take shift s)))))

(defcheck solution-4ffb407d
  #(let [[r l] (split-at (mod % (count %2)) %2)] (concat l r)))

(defcheck solution-501e4796
  #(let [n (count %2)]
     (take n
       (drop (mod (+ %1 n) n)
         (mapcat identity (repeat %2))))))

(defcheck solution-50456136
  (fn f [num-rotate x]
    (let [n (Math/abs num-rotate)]
      (if (> n (count x))
        (f (mod num-rotate (count x)) x)
        (if (< num-rotate 0)
          (concat (take-last n x) (drop-last n x))
          (concat (drop n x) (take n x))
          )
        )
      )
    ))

(defcheck solution-5085ae5c
  (fn [n coll]
    (let [n (mod n (count coll))]
      (into (vec (drop n coll)) (vec (take n coll))))))

(defcheck solution-50a9bcc0
  (fn rotate
    [n xs]
    (let [mod-n (mod n (count xs))]
      (concat (drop mod-n xs) (take mod-n xs)))))

(defcheck solution-50c503d6
  #(->> %2
     (split-at (mod %1 (count %2)))
     reverse
     (apply concat)))

(defcheck solution-515080cb
  #(let [x (count %2)] (take x (drop (mod % x) (cycle %2)))))

(defcheck solution-52279ba5
  #(let [
         [l r] (split-at (mod % (count %2)) %2)]
     (concat r l)))

(defcheck solution-522e8d0d
  (fn rotate-a-seq [n lat]
    (let [x (count lat)
          a (if (neg? n) (reverse (take (mod (* n -1) x) (reverse lat)))
                         (drop (mod n x) lat))
          b (if (neg? n) (take (mod (+ x n) x) lat)
                         (take (mod n x) lat))]
      (concat a b))))

(defcheck solution-528afd3f
  (fn [n coll]
    (take (count coll)
      (drop (mod n (count coll))
        (cycle coll)))))

(defcheck solution-52c37fe8
  #(->> (cycle %2)
     (drop (+ (count %2) (mod %1 (count %2))))
     (take (count %2))))

(defcheck solution-52fde5b
  (fn [n s]
    (->> s
      (split-at (mod n (count s)))
      (reverse)
      (flatten))))

(defcheck solution-53085de5
  #(take (count %2)
     (drop (mod %1 (count %2))
       (cycle %2))))

(defcheck solution-53359394
  (fn [n coll]
    (let [m (mod n (count coll))]
      (concat (drop m coll) (take m coll)))))

(defcheck solution-53da6738
  (fn [n lst]
    (let [m (mod n (count lst))]
      (concat (drop m lst) (take m lst))
      )
    ))

(defcheck solution-53eec50e
  (fn [n xs]
    (let [nn (mod n (count xs))]
      (concat (drop nn xs) (take nn xs)))))

(defcheck solution-541539e7
  (fn [n s]
    (let [r (Math/abs (rem n (count s)))
          i (if (pos? n) r (- (count s) r))]
      (concat (drop i s) (take i s)))))

(defcheck solution-541bb26
  (fn [shift coll]
    (let [n (mod shift (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-542655be
  (fn [c s] (take (count s) (drop (mod c (count s)) (concat s s)))))

(defcheck solution-55120bf2
  (fn [n s]
    (let [m (mod n (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-55437eb
  (fn [i x]
    (let [n (mod i (count x))]
      (concat (take-last (- (count x) n) x) (take n x)))))

(defcheck solution-555af2a9
  #(let [part (mod % (count %2))]
     (concat (drop part %2) (take part %2))))

(defcheck solution-5639f623
  #((fn f [n l]
      (if (= n 0)
        l
        (f (- n 1) (conj (butlast l) (last l)))))
    (mod (- %) (count %2))
    (vec %2)))

(defcheck solution-56a023d7
  (fn [b a] (let [n (count a)
                  c (rem b n)]
              (if (pos? c)
                (concat (take-last (- n c) a) (take c a))
                (concat (take-last (- 0 c) a) (take (+ n c) a))))))

(defcheck solution-56a49f65
  (fn rotate [n s]
    (let [l (count s) nl (rem n l)]
      (if (neg? n)
        (rotate (rem (inc (- l n)) l) s)
        (concat (drop nl s) (take nl s)))
      )
    ))

(defcheck solution-570bd2ef
  (fn [n coll]
    (let [n (mod n (count coll)) coll (vec coll)]
      (into (subvec coll n) (take n coll)))))

(defcheck solution-57257252
  #(let [c (count %2)
         n (mod (if (neg? %) (+ c %) %) c)]
     (concat (drop n %2) (take n %2))))

(defcheck solution-572f6c7c
  (fn [n xs] (take (count xs) (drop (mod n (count xs)) (cycle xs)))))

(defcheck solution-578e08a7
  (fn [index seq]
    (let [len    (count seq)
          offset (mod index len)]
      (let [front (keep-indexed (fn [i v] (if (< i offset) v)) seq)
            back  (keep-indexed (fn [i v] (if (>= i offset) v)) seq)]
        (concat back front)))))

(defcheck solution-5822d373
  (fn rotate [n s]
    (let [v (vec s) c (count v) i (mod n c)]
      (concat (subvec v i) (subvec v 0 i))
      )))

(defcheck solution-588cf2b0
  (fn [n x]
    (if (neg? n)
      (recur (+ (count x) n) x)
      (if (> n (count x))
        (recur (- n (count x)) x)
        (let [x (split-at n x)]
          (concat (last x) (first x)))))))

(defcheck solution-58a7513f
  (fn [n xs] (if (pos? n)
               (nth (iterate #(concat (next %) (take 1 %)) xs) n)
               (nth (iterate #(cons (last %) (butlast %)) xs) (- n)))))

(defcheck solution-5933fa41
  (fn [n coll]
    (if (= 0 n)
      coll
      (if (< 0 n)
        (recur (dec n) (concat (rest coll) (list (first coll))))
        (recur (inc n) (cons (last coll) (butlast coll)))))))

(defcheck solution-59467dbe
  (fn my-rotate [r s] (let [p1 (rem r (count s))]
                        (if (>= p1 0)
                          (into (take p1 s) (reverse (drop p1 s)))
                          (into (take (+ (count s) p1) s) (reverse (drop (+ (count s) p1) s)))
                          )
                        )))

(defcheck solution-59538e7c
  #(let [
         [l r] (split-at (mod % (count %2)) %2)]
     (concat r l)))

(defcheck solution-595cf134
  (fn [n col]
    (let [tc (count col) i (rem n tc)]
      (if (pos? i)
        (concat (nthnext col i) (take i col))
        (concat (nthnext col (+ tc i)) (take (+ tc i) col))))))

(defcheck solution-597503a7
  (fn [n seq]
    (let [k (mod n (count seq))]
      (concat (drop k seq) (take k seq)))))

(defcheck solution-59d647b5
  (fn [n s]
    (let [m (mod n (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-5a05d8ff
  #(let [c (count %2)]
     (take c (drop (mod %1 c) (cycle %2)))))

(defcheck solution-5a25eab0
  (fn rs [n s]
    (let [c  (count s)
          n' (if (< n 0)
               (mod (+ c n) c)
               n)]
      (take c
        (nthrest (cycle s) n')))))

(defcheck solution-5a36d8d4
  (fn [n coll]
    (loop [n    (if (> n 0)
                  n
                  (- (count coll) (mod (- n) (count coll))))
           coll coll]
      (if (= n 0)
        coll
        (recur (- n 1) (concat (rest coll) `(~(first coll))))))))

(defcheck solution-5a652de
  (fn rotate
    [n s]
    (if (zero? n)
      s
      (if (> n 0)
        (rotate (dec n) (conj (vec (rest s)) (first s)))
        (rotate (inc n) (conj (drop-last s) (last s)))))))

(defcheck solution-5a93484a
  (fn f [x coll]
    (let [a (count coll)
          b (rem x a)
          c (if (< 0 b) b (+ b a))
          ]


      (concat (take-last (- (count coll) c) coll) (take c coll))
      )
    ))

(defcheck solution-5ab394a9
  (fn [d l]
    (let [c (count l)]
      (take c (drop (mod d c) (cycle l)))
      )
    ))

(defcheck solution-5abc74f3
  (fn [s lst]
    (let [q (rem s (count lst))]
      (if (> q 0)
        (concat (drop q lst) (take q lst))
        (concat (take-last (- q) lst) (drop-last (- q) lst))))))

(defcheck solution-5ae9ca0c
  (fn [n coll]
    (take (count coll)
      (drop (mod n (count coll))
        (cycle coll)))))

(defcheck solution-5b97fd96
  (fn [n s] (cond (> n 0) (let [m (mod n (count s))] (concat (drop m s) (take m s)))
                  (< n 0) (let [m (mod n (count s)) len (count s)] (concat (drop m s) (take m s)))
                  :else s
                  )))

(defcheck solution-5b9f23f1
  (fn [x coll]
    (flatten ((juxt drop take) (mod x (count coll)) coll))))

(defcheck solution-5c24a4ba
  (fn rot [a b]
    (letfn [
            (numerigi [x s] (map-indexed (fn [idx itm] [(mod (- idx x) (count s)) itm]) s))
            (ordoni [co] (sort-by #(first %) co))
            (purigi [co] (map #(last %) co))]
      ((comp purigi ordoni (partial numerigi a)) b))))

(defcheck solution-5c31348c
  (fn [n xs] (take (count xs) (drop (mod n (count xs)) (concat xs xs)))))

(defcheck solution-5c8b77f
  (fn [i coll]
    (let [i (mod i (count coll))]
      (concat (drop i coll) (take i coll)))))

(defcheck solution-5cae0746
  #(concat (drop (mod %1 (count %2)) %2)
           (take (mod %1 (count %2)) %2)))

(defcheck solution-5cb7f0a2
  #(let [index (mod % (count %2))] (concat (nthnext %2 index) (take index %2))))

(defcheck solution-5cbb43b1
  (fn rot [n x]
    (let [s (mod n (count x)) e (- (count x) s)]
      (concat (take-last e x) (take s x))
      )
    ))

(defcheck solution-5ce86207
  (fn [n coll]
    (let [x (mod n (count coll))]
      (concat (drop x coll) (take x coll)))))

(defcheck solution-5d842739
  (fn [n xs]
    (let [len     (count xs)
          backwd? (neg? n)
          shft    (if-not backwd? (rem n len) (- len (rem (- n) len)))]
      (concat (drop shft xs) (take shft xs)))))

(defcheck solution-5d92cfc
  (fn [n col]
    (let [c (count col)
          r (if (< n 0) (+ c (rem n c)) n)]
      (->> col
        cycle
        (drop r)
        (take c)))))

(defcheck solution-5df02d86
  (fn [n l]
    (cond
      (> n 0) (loop [nn n ll l]
                (if (= nn 0) ll
                             (recur (dec nn) (concat (rest ll) (list (first ll))))
                             ))
      (< n 0)
      (loop [nn n ll l]
        (if (= nn 0) ll
                     (recur (inc nn) (cons (last ll) (butlast ll)))
                     ))
      :else l
      )
    ))

(defcheck solution-5e40a910
  (fn [n c]
    (let [f (mod n (count c))]
      (concat (drop f c) (take f c)))))

(defcheck solution-5e77040e
  (fn rot [n xs]
    (cond
      (zero? n) xs
      (neg? n) (recur (inc n)
                 (cons (last xs) (butlast xs)))
      (pos? n) (recur (dec n)
                 (concat (rest xs) [(first xs)])))))

(defcheck solution-5ee4ed7e
  (fn rotate-seq [idx s]
    (let [split (mod idx (count s))
          [fst snd] (split-at split s)]
      (concat snd fst))))

(defcheck solution-5f75e108
  (fn [n c]
    (let [l (count c)]
      (cond (= n 0) c
            (> n 0) (recur (rem (- n l) l) c)
            (< n 0) (recur (inc n) (cons (last c) (butlast c)))))))

(defcheck solution-6058ee2c
  (fn [n coll]
    (let [n (if (neg? n)
              (first
                (drop-while neg?
                  (iterate #(+ % (count coll)) n)))
              (mod n (count coll)))]
      (concat (drop n coll)
              (take n coll)))))

(defcheck solution-6155cb82
  (fn rot82 [n s]
    (let
     [split-point (+ (rem n (count s))
                     (if (< n 0) (count s) 0))]
      (apply concat (reverse (split-at split-point s))))))

(defcheck solution-61f0d532
  (fn [n xs]
    (let [i (mod n (count xs))]
      (concat (drop i xs) (take i xs)))))

(defcheck solution-628f04b
  (fn rotate [n coll]
    (let [len (count coll)
          idx (if (or (neg? n) (> (Math/abs n) len))
                (mod n len)
                n)]
      (apply concat (reverse (split-at idx coll))))))

(defcheck solution-6344413e
  (fn [n v]
    (let [cnt  (mod n (count v))
          head (take cnt v)
          tail (drop cnt v)]
      (concat tail head)
      )
    ))

(defcheck solution-634bcdbe
  #(let [n (count %2)]
     (take n
       (drop
         (if (<= 0 %1)
           %1
           (- n (mod (- %1) n)))
         (cycle %2)))))

(defcheck solution-63c78878
  #(map-indexed
     (fn [i x]
       (nth %2 (mod (+ i %) (count %2))))
     %2))

(defcheck solution-63f196fa
  (fn [n s]
    (if (or (< n 0) (> n (count s)))
      (recur (mod n (count s)) s)
      (concat (drop n s) (take n s)))))

(defcheck solution-641aac5a
  #(->> %2
     repeat
     flatten
     (drop
       (loop [x %1]
         (if (neg? x)
           (recur (+ x (count %2)))
           x)))
     (take (count %2))))

(defcheck solution-646500ac
  #(nth (partition (count %2) 1 (concat %2 %2)) (mod %1 (count %2))))

(defcheck solution-6483389e
  (fn [n seq]
    (let [my-n (cond (> n (count seq)) (mod n (count seq))
                     (< n 0) (if (> 0 (+ (count seq) n))
                               (+ (count seq) (mod (count seq) n))
                               (+ (count seq) n))
                     :else n)]
      (concat (drop my-n seq) (take my-n seq)))))

(defcheck solution-64980c77
  (fn rotate [n coll]
    (let [c (count coll)
          n (mod n c)]
      (concat (drop n coll)
              (take n coll)))))

(defcheck solution-64e4627a
  (fn [n x]
    (let [n (mod n (count x))]
      (concat (drop n x) (take n x))

      )))

(defcheck solution-65b5bdb4
  (fn rot [n col]
    (let [c (count col)
          x (rem n c)
          y (if (< x 0) (+ c x) x)]
      (apply concat (reverse (split-at y col))))))

(defcheck solution-6606a448
  (fn peu [x y] (if (= 0 x) y (if (< x 0) (peu (+ x (count y)) y) (peu (dec x) (concat (rest y) (list (first y))))))))

(defcheck solution-66518667
  (fn [c s]
    (if (> c 0)
      (let [n (mod c (count s))]
        (concat (drop n s) (take n s)))
      (let [n (mod (Math/abs c) (count s))]
        (concat (take-last n s) (drop-last n s))))))

(defcheck solution-665ae8f4
  (fn rotate [n xs]
    (cond
      (> n (count xs))
      (rotate (- n (count xs)) xs)

      (> n 0)
      (concat
       (drop n xs)
       (take n xs)
       )

      (< n 0)
      (rotate (+ (count xs) n) xs)
      )
    ))

(defcheck solution-66719ca3
  (fn [n xs]
    (let [[xs1 xs2] (split-at (mod n (count xs)) xs)]
      (concat xs2 xs1))))

(defcheck solution-66823560
  #(flatten (list (drop (mod % (count %2)) %2)
              (take (mod % (count %2)) %2))))

(defcheck solution-66dd2ee4
  (fn [n l]
    (let [rot-amt (mod (if (< n 0) (+ (count l) n) n) (count l))]
      (concat (drop rot-amt l) (take rot-amt l)))))

(defcheck solution-67a7cd3c
  (fn r [n l]
    (if (zero? n)
      l
      (if (> n 0)
        (r (dec n) (concat (rest l) [(first l)]))
        (r (inc n) (concat [(last l)] (butlast l)))))))

(defcheck solution-67aae215
  (fn [sh c] (let [dl (< 0 sh) n (mod (* sh (if dl 1 -1)) (count c))]
               (loop [n n r c]
                 (if (>= 0 n) r
                              (recur (dec n)
                                (if dl (conj (vec (drop 1 r)) (first r)) (cons (last r) (drop-last r)))
                                )
                              )))))

(defcheck solution-6830051e
  (fn [i xs] (let [o (mod i (count xs))] (concat (drop o xs) (take o xs)))))

(defcheck solution-6830693b
  (fn [n l]
    (let [len (count l)
          r   (rem n len)
          nn  (if (> n 0) r (+ len r))]
      (concat (drop nn l) (take nn l)))))

(defcheck solution-683506c8
  (fn [pivot coll]
    (let [len (count coll)]
      (take len (drop (mod pivot len) (cycle coll))))))

(defcheck solution-68791363
  (fn [dir string]
    (let [n (mod dir (count string))]
      (concat (drop n string) (take n string)))))

(defcheck solution-687c5a55
  (fn [n xs]
    (let [modn (mod n (count xs))]
      (take (count xs) (drop modn (cycle xs))))))

(defcheck solution-68ac8aa9
  #(let [new-coll      (if (pos? %1)
                         %2
                         (reverse %2))
         new-position  (if (pos? %1)
                         %1
                         (+ %1 (* -2 %1)))
         real-position (mod new-position (count %2))
         rotated       (flatten
                         (reduce
                           (fn [[acc-x acc-y] [x y]]
                             (if (< x real-position)
                               [acc-x (conj acc-y y)]
                               [(conj acc-x y) acc-y]))
                           [[] []]
                           (map-indexed vector new-coll)))]
     (if (pos? %1)
       rotated
       (reverse rotated))))

(defcheck solution-68e391f7
  #(let [n (mod % (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-69a9e410
  (fn [r coll]
    (let [n (mod r (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-69e0dbdc
  (fn [n col]
    (for [i (range (count col))]
      (nth col (rem (+ (rem (+ i n)
                         (count col)) (count col)) (count col))
        ))))

(defcheck solution-69ee0441
  (fn rot [n s]
    (cond
      (zero? n) s
      (> n 0) (recur (dec n) (conj (vec (rest s)) (first s)))
      (< n 0) (recur (inc n) (cons (last s) (butlast s))))))

(defcheck solution-6a10d803
  (fn rotate-seq [n lst]
    (let [l (count lst) x (if (or (> n l) (< n (* -1 l))) (int (/ n l)) n) i (if (neg? x) (+ l x) x)]
      (concat (drop i lst) (take i lst)))))

(defcheck solution-6a4e3e10
  (fn [n coll]
    (let [m (if (pos? n) n (inc (- n)))]
      (take (count coll) (drop m (cycle coll))))))

(defcheck solution-6a785fc9
  (fn [n ns]
    (let [ct     (count ns)
          n-take (mod (if (< 0 n) n (+ ct n)) ct)]
      (concat (drop n-take ns) (take n-take ns)))))

(defcheck solution-6ad273c5
  (fn [i s]
    (let [j (mod i (count s))]
      (concat (drop j s) (take j s)))))

(defcheck solution-6ae67fd2
  ;;#(apply concat (reverse (split-at (mod % (count %2)) %2)))
  ;;#(let [n (mod % (count %2))] (concat (drop n %2) (take n %2)))
  ;;(fn [f n lst] `(~@(f drop n lst) ~@(f take n lst))) #(% (mod %2 (count %3)) %3)
  #(apply concat ((juxt drop take) (mod % (count %2)) %2)))

(defcheck solution-6b342629
  (fn [dir s] (let [n (mod dir (count s))] (concat (drop n s) (take n s)))))

(defcheck solution-6b368d84
  #(if (= % 0) %2 (if (> % 0)
                    (recur (dec %) (concat (rest %2) [(first %2)]))
                    (recur (inc %) (cons (last %2) (butlast %2))))))

(defcheck solution-6b4e090
  (fn rotate-e
    [n x]
    (let [n    (mod n (count x))
          part (split-at n x)]
      (concat (last part) (first part)))))

(defcheck solution-6b74afff
  (fn [shift coll]
    (let [size (count coll)
          skip (if (neg? shift)
                 (+ size (rem shift size))
                 shift)]
      (take size (drop skip (cycle coll))))))

(defcheck solution-6bab685b
  (fn rotate [n coll]
    (let [c (count coll)]
      (take c (drop (mod n c) (cycle coll))))))

(defcheck solution-6c2300a
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-6cd135b7
  (fn rotate [amount ls]
    (if (= amount 0)
      ls
      (if (< amount 0)
        (rotate (inc amount) (cons (last ls) (drop-last ls)))
        (rotate (dec amount) (concat (rest ls) (list (first ls))))))))

(defcheck solution-6d5dfa17
  #(let [i (mod % (count %2))]
     (concat (drop i %2) (take i %2))))

(defcheck solution-6ea0797a
  (fn rotate-seq
    ([x y]
     (if (> x 0)
       (rotate-seq (rem x (count y)) (vec y) [])
       (rotate-seq (+ x (count y)) y)))
    ([x y z]
     (if (> (count y) x)
       (if (= 0 (count (subvec y (inc x))))
         (recur x (subvec y 0 x) (conj z (get y x)))
         (recur x (apply conj (subvec y 0 x) (subvec y (inc x))) (conj z (get y x))))
       (list* (apply conj z y))))))

(defcheck solution-6ed23e5c
  (fn [n coll]
    (apply concat ((juxt drop take) (mod n (count coll)) coll))))

(defcheck solution-6f27b753
  (fn [offset l]
    (let [l   (vec l)
          len (count l)]
      (map (fn [i] (nth l (mod (+ i offset) len))) (range len)))))

(defcheck solution-6f86e8a6
  (fn rotate-seq
    [number coll]
    (loop [n number res coll]
      (cond
        (= n 0) res
        (> n 0) (recur (dec n) (concat (rest res) (list (first res))))
        (< n 0) (recur (inc n) (cons (last res) (butlast res)))))))

(defcheck solution-6fabdbd5
  (fn [n a]
    (loop [c (Math/abs n) x (if (< n 0) (reverse a) a)]
      (if (= c 0)
        (if (< n 0) (reverse x) x)
        (recur (dec c) (conj (vec (rest x)) (first x)))))))

(defcheck solution-6fb91873
  (fn [x y]
    (let [n (mod x (count y))]
      (concat (drop n y) (take n y))
      )))

(defcheck solution-6fef91b1
  (fn rotate [n xs]
    (
      let [l (mod n (count xs))]
      (concat (drop l xs) (take l xs))
      )
    ))

(defcheck solution-70513167
  #_#(let [n (mod %1 (count %2))]
       (concat (drop n %2) (take n %2)))

  #_#(let [n (count %2)]
       (take n (drop (+ (* 2 n) %) (cycle %2))))

  #_#(take (count %2) (second (split-at (mod % (count %2)) (cycle %2))))

  #_#(second (partition (count %2)
               (+ (* 2 (count %2)) %)
               (cycle %2)))


  #_#(if (= 0 (mod % (count %2)))
       %2
       (recur (inc %) (cons (last %2) (butlast %2))))

  #_#(if (= 0 (mod % (count %2)))
       %2
       (recur (inc %) (cons (last %2) (butlast %2))))

  #_(comp flatten reverse #(split-at (mod % (count %2)) %2))

  #(flatten (reverse (split-at (mod % (count %2)) %2))))

(defcheck solution-705173f1
  (fn [c sq]
    (if (< c 0)
      (last (take (inc (Math/abs c)) (iterate #(concat [(last %)] (butlast %)) sq)))
      (last (take (inc c) (iterate #(concat (rest %) [(first %)]) sq))))))

(defcheck solution-70affbf8
  (fn [dist s]
    (let [s-size    (count s)
          rem-dist  (rem dist s-size)
          true-dist (if (pos? rem-dist) rem-dist (+ s-size rem-dist))
          xs        (take true-dist s)
          ys        (drop true-dist s)]
      (concat ys xs))))

(defcheck solution-70fc1d40
  (fn [n c]
    (let [n (mod n (count c))]
      (concat (drop n c) (take n c)))))

(defcheck solution-715c4bb5
  (fn [n coll]
    (let [x (split-at (mod n (count coll)) coll)]
      (concat (fnext x) (first x)))))

(defcheck solution-716a7f87
  (fn rotate [r coll]
    (loop [r r coll (apply vector coll)]
      (if (= 0 r)
        coll
        (if (> r 0)
          (recur (dec r) (conj (subvec coll 1) (first coll)))
          (recur (inc r) (conj (butlast coll) (last coll))))))))

(defcheck solution-718066b0
  #(let [c (count %2) s (mod % c) n (if (>= s 0) s (+ c s))] (concat (take-last (- c n) %2) (take n %2))))

(defcheck solution-71bd05c1
  (fn rot [d sq]
    (let [[front back] (split-at (mod d (count sq)) sq)]
      (concat back front))))

(defcheck solution-71d96f85
  #(let [c (count %2)]
     (take c (drop (mod %1 c) (cycle %2)))))

(defcheck solution-71ea290a
  (fn rotate--repeat
    ([coll]
     (concat (rest coll) (list (first coll))))
    ([n coll] {:pre [(integer? n)]}
     ((apply comp (repeat (mod n (count coll)) rotate--repeat)) coll))))

(defcheck solution-7239f089
  (fn f [n col]
    (if (= 0 n)
      col
      (if (> n 0)
        (f (dec n) (conj (vec (rest col)) (first col)))
        (f (inc n) (cons (last col) (drop-last col)))
        ))))

(defcheck solution-72e5c9dd
  (fn [n S]
    (let [[a b] (split-at (mod n (count S)) S)] (concat b a))))

(defcheck solution-73154363
  (fn [n coll]
    (let [s (count coll)
          i (rem n s)
          m (if (< i 0) (+ s i) i)]
      (concat (drop m coll) (take m coll))
      )))

(defcheck solution-731d35fb
  (fn [n coll]
    (let [n (mod (+ (count coll) n) (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-732901f7
  (fn [n s]
    (take (count s) (drop (mod n (count s)) (cycle s)))))

(defcheck solution-739c1a1a
  #(apply concat (-> (mod % (count %2)) (split-at %2) reverse)))

(defcheck solution-73b24fb5
  #(letfn [(xyz [f] (f (mod %1 (count %2)) %2))]
     (concat (xyz drop) (xyz take))))

(defcheck solution-73b3ec04
  (fn rotate [n coll]
    (let [c (count coll)]
      (->> (cycle coll)
        (drop (if (pos? n)
                n
                (first (drop-while neg? (iterate (partial + c) n)))))
        (take c)))))

(defcheck solution-758c2146
  (fn f [i c]
    (cond                                                   ;; terrrrrrrible
      (= i 0) c
      (> i 0) (f (dec i) (concat (rest c) (list (first c))))
      (< i 0) (f (inc i) (take (count c) (cons (last c) c)))
      )))

(defcheck solution-75a0abe6
  (fn [n v]
    (take (count v)
      (drop (mod n (count v))
        (concat v v)))))

(defcheck solution-75b56ffe
  (fn [n coll] (if (>= n 0)
                 (concat (nthnext coll (mod n (count coll))) (take (mod n (count coll)) coll))
                 (concat (nthnext coll (- (count coll) (mod (- n) (count coll)))) (take (- (count coll) (mod (- n) (count coll))) coll)))))

(defcheck solution-75d719c1
  (fn [n s]
    (let [n (mod n (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-769e237c
  #(flatten (cons (drop (mod (+ (* (count %2) 30) %) (count %2)) %2) (take (mod (+ (* (count %2) 30) %) (count %2)) %2))))

(defcheck solution-76af09fd
  (fn [index col]
    (let [length      (count col)
          i           (if (< index (* length -1))
                        (* -1 (rem (Math/abs index) length))
                        index)
          split-index (rem (+ length i) length)
          vals        (split-at split-index col)]
      (flatten (conj (first vals) (rest vals))))))

(defcheck solution-76df6556
  (fn [o s]
    (let [len      (count s)
          m-offset (mod o len)
          l-offset (if (pos? m-offset) m-offset (+ len m-offset))]
      (take len (drop l-offset (cycle s))))))

(defcheck solution-7715c3a8
  (fn rotate
    [n coll]
    (->> (cycle coll)
      (drop (mod n (count coll)))
      (take (count coll)))))

(defcheck solution-7732c8e8
  (fn [n l]
    (let [c (count l)
          n (rem n c)]
      (if (> n 0)
        (concat (drop n l) (take n l))
        (concat (drop (- c (- n)) l) (take (- c (- n)) l))))))

(defcheck solution-7740eda3
  #(->
     (split-at (mod %1 (count %2)) %2)
     reverse
     flatten
     ))

(defcheck solution-7769dff8
  (fn [index coll]
    (let [
          cutindex (mod index (count coll))
          splitseq (split-at cutindex coll)
          ]
      (-> splitseq (reverse) (flatten))
      )
    ))

(defcheck solution-7841f5a3
  (fn flip [n xs]
    (let [[x y] (split-at (mod n (count xs)) xs)] (concat y x))))

(defcheck solution-7a15822
  (fn [rot xs] (let [rot (mod (if (neg? rot) (+ (count xs) rot) rot) (count xs))] (concat (drop rot xs) (take rot xs)))))

(defcheck solution-7a7d604a
  #(let [off (mod % (count %2))]
     (concat (drop off %2) (take off %2))))

(defcheck solution-7ad2155d
  #(let [c (count %2)] (take c (drop (mod (+ %1 c) c) (cycle %2)))))

(defcheck solution-7b012e59
  (fn [n coll]
    (letfn [(f [] (lazy-cat coll (f)))]
      (let [size (count coll)
            c    (if (< n 0) (- size (mod (Math/abs n) size)) n)]
        (take size (drop c (f)))))))

(defcheck solution-7b06559a
  (fn [num lst]
    (let [x (if (pos? num) num (+ num (count lst)))
          n (mod x (count lst))]
      (concat (drop n lst) (take n lst)))))

(defcheck solution-7b6c7fc1
  (fn [n s]
    (let [c (count s)]
      (take c (drop (mod n c) (cycle s))))))

(defcheck solution-7b72fac6
  #(let [k (mod % (count %2))]
     (concat (drop k %2) (take k %2))))

(defcheck solution-7bb2dd80
  (fn rotate [n xs]
    (cond (= n 0) xs
          (> n 0) (recur (dec n) (concat (rest xs) (list (first xs))))
          :else (recur (inc n) (cons (last xs) (drop-last xs))))))

(defcheck solution-7bf5383f
  (fn shift [n s]
    (let [us (mod (if (< n 0) (+ (count s) n) n) (count s))]
      (concat (drop us s) (take us s)))))

(defcheck solution-7c7cc1c0
  (fn [n s]
    (let [i (mod n (count s))]
      (concat (drop i s) (take i s)))))

(defcheck solution-7cc8b283
  (fn [shift lst]
    (let [n (mod shift (count lst))]
      (concat (drop n lst) (take n lst)))))

(defcheck solution-7cf13538
  (fn rot
    [n s]
    (if (= n 0)
      s
      (if (< n 0)
        (rot (inc n) (conj (butlast s) (last s)))
        (rot (- n (count s)) s)))))

(defcheck solution-7cf33215
  #(take (count %2) (drop (mod %1 (count %2)) (concat %2 %2))))

(defcheck solution-7d3508c3
  (fn rot [n s]
    (let [r (mod n (count s))] (concat (drop r s) (take r s)))))

(defcheck solution-7d385f40
  #(let [[l r] (split-at (mod %1 (count %2)) %2)]
     (concat r l)))

(defcheck solution-7d5684ad
  (fn rotate [index s]
    (let [n (mod index (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-7ddd99ea
  #(let [size (count %2)]
     (take size (drop (mod % size) (cycle %2)))))

(defcheck solution-7e2fad32
  (fn [n s] (take (count s) (drop (mod n (count s)) (concat s s)))))

(defcheck solution-7ec2da06
  #(let [n (count %2)]
     (->> (cycle %2) (drop (mod %1 n)) (take n))))

(defcheck solution-7ef2b9c6
  (fn rotate [n coll]
    (let [start (Math/abs n)
          orig  (if (> n 0) coll (reverse coll))]
      (loop [counter start orig orig agg []]
        (if (= counter 0)
          (if (> n 0)
            (concat orig agg)
            (reverse (concat orig agg)))
          (let [[orig agg] (if (empty? orig) [agg []] [orig agg])]
            (recur (dec counter)
              (drop 1 orig)
              (conj agg (first orig)))))))))

(defcheck solution-7f3e6206
  (fn [n xs]
    (let [c (count xs)]
      (map #(nth xs (mod % c))
        (range n (+ n c))))))

(defcheck solution-7f7bf39b
  (fn [r s] (let [m (mod r (count s)) v (vec s)]
              (concat (subvec v m) (subvec v 0 m)))))

(defcheck solution-7fa71b3e
  #(let [n (count %2)] (take n (drop (mod %1 n) (concat %2 %2)))))

(defcheck solution-7fc284d4
  (fn [offset coll]
    (loop [result [] index (- (count coll) (mod (* -1 offset) (count coll)))]
      (if (= (count coll) (count result))
        result
        (recur (conj result (nth coll index))
          (if (< index (- (count coll) 1))
            (inc index)
            0
            )
          )
        )
      )
    ))

(defcheck solution-7fcbbc32
  (fn [dir s]
    (let [n (mod dir (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-7feeb544
  (fn [rotation lst]
    (let [n (mod rotation (count lst))]
      (flatten (reverse (split-at n lst))))))

(defcheck solution-802d5802
  (fn [n l]
    (let [c (mod n (count l))]
      (concat (drop c l) (take c l)))))

(defcheck solution-804f9100
  #(flatten (reverse (split-at (mod %1 (count %2)) %2))))

(defcheck solution-80d4859f
  #(let [s (vec %2)
         c (count s)
         p (mod (+ % c) c)]
     (concat (subvec s p) (subvec s 0 p))))

(defcheck solution-81016242
  #(map-indexed
     (fn [i _]
       (nth %2 (mod (+ i %1) (count %2))))
     %2)                                                    ;; slow and silly, but at least different
  )

(defcheck solution-811e8081
  #(let [s (split-at (mod % (count %2)) %2)]
     (concat (second s) (first s))))

(defcheck solution-811ef7d8
  (fn rotate [n x]
    (let [cnt (count x)
          xxx (concat x x x)]
      (take cnt (drop (+ cnt (rem n cnt)) xxx)))))

(defcheck solution-81f82d99
  (fn [rotate-by li]
    (let [pos-rotate (mod rotate-by (count li))]
      (concat (drop pos-rotate li) (take pos-rotate li))
      )
    ))

(defcheck solution-81f94f4d
  (fn [n s]
    (let [r (mod n (count s))]
      (concat (drop r s) (take r s)))))

(defcheck solution-824ffbce
  (fn [n xs]
    (if (neg? n)
      (recur (+ (count xs) n) xs)
      (take (count xs) (drop n (cycle xs))))))

(defcheck solution-82565880
  (fn rot [x s]
    (apply concat
      (reverse
        (split-at (mod x (count s)) s)))))

(defcheck solution-82a04e84
  #(let [d (count %2) c (+ (rem % d) d)] (drop c (take (+ d c) (cycle %2)))))

(defcheck solution-82ef5849
  (fn [idx a-seq]
    (take (count a-seq)
      (drop (mod idx (count a-seq))
        (cycle a-seq)))))

(defcheck solution-82ffbea6
  (fn [n c]
    (let [sz (count c)
          r  (+ (rem n sz) sz)]
      (take sz (drop r (cycle c))))))

(defcheck solution-83003963
  (fn [i lst]
    (let [dn (rem (+ i (* 5 (count lst))) (count lst))]
      (into (vec (drop dn lst)) (take dn lst)))))

(defcheck solution-830709b3
  (fn [n l]
    (cond
      (= n 0) l
      (> n 0) (recur (- n 1) (concat (rest l) (list (first l))))
      (< n 0) (recur (+ n 1) (concat (list (last l)) (butlast l)))
      )
    ))

(defcheck solution-8336ae29
  (fn [c sq]
    (let [cc (mod c (count sq))]
      (concat (drop cc sq) (take cc sq)))))

(defcheck solution-8371da74
  (fn rotate [x s]
    (let [l  (count s)
          d  (mod x l)
          hl (drop d s)
          tl (take d s)
          ]
      (concat hl tl))))

(defcheck solution-838f5eb4
  #(let [c (count %2)
         n (mod (if (neg? %1) (+ c %1) %1) c)
         [a b] (split-at n %2)]
     (concat b a)))

(defcheck solution-847752c8
  (fn
    [n ary]
    (let [size (count ary)]
      (if (neg? n)
        (recur (+ size (rem n size)) ary)
        (take size (drop n (cycle ary)))
        )

      )
    ))

(defcheck solution-8509259d
  (fn rotate
    [index values]
    (let [l (mod index (count values))]
      (concat (drop l values) (take l values)))))

(defcheck solution-850ac997
  (fn [n coll]
    (let [ny (rem n (count coll))
          nx (if (> ny 0) ny (+ ny (count coll)))]
      (concat (drop nx coll) (take nx coll)))))

(defcheck solution-8553299a
  (fn [n x]
    (let [howmany (mod n (count x))]
      (concat (drop howmany x) (take howmany x)))))

(defcheck solution-856623c7
  (fn [n coll]
    (let [nn (mod n (count coll))]
      (concat (drop nn coll) (take nn coll)))))

(defcheck solution-859d24c9
  (fn [n coll]
    (if (>= n 0)
      (let [m (mod n (count coll))] (concat (drop m coll) (take m coll)))
      (let [m (- (count coll) (mod (- n) (count coll)))]
        (concat (drop m coll) (take m coll))))))

(defcheck solution-85dd7074
  #(let [n (mod %1 (count %2))]
     (concat (nthrest %2 n) (take n %2))))

(defcheck solution-85f6bc49
  (fn rot [n v]
    (let [cnt (count v), shift (mod (+ cnt n) cnt)]
      (loop [n shift, s v]
        (if (zero? n)
          s
          (recur (dec n) (conj (into [] (rest s)) (first s))))))))

(defcheck solution-86426a0a
  (fn rotate [n s]
    (cond
      (neg? n) (rotate (+ n (count s)) s)
      (zero? n) s
      true (rotate (dec n) (conj (vec (rest s)) (first s))))))

(defcheck solution-865ca34d
  (fn [n coll]
    (let [idx (mod n (count coll))]
      (concat (drop idx coll) (take idx coll)))))

(defcheck solution-868d0401
  (fn number44 [n xs]
    (cond
      (< n 0) (recur (inc n) (cons (last xs) (butlast xs)))
      (> n 0) (recur (dec n) (concat (rest xs) [(first xs)]))
      :else xs)))

(defcheck solution-86a715fa
  (let [rotate-right (fn
                       [[s & t]]
                       (conj (into [] t) s))]
    (let [rotate-left (fn
                        [s]
                        (reverse (rotate-right (reverse s))))]
      (fn rotate-seq
        [c s]
        (loop [accum s
               rem   c]
          (if (= rem 0)
            accum
            (if (> rem 0)
              (recur (rotate-right accum) (- rem 1))
              (recur (rotate-left accum) (+ rem 1)))))))))

(defcheck solution-86efd0e1
  (fn [n xs]
    (letfn [(shl [xs] (concat (rest xs) [(first xs)]))
            (shr [xs] (cons (last xs) (butlast xs)))
            (abs [n] (if (neg? n) (* -1 n) n))]
      (->> xs
        (iterate (if (neg? n) shr shl))
        (drop (abs n))
        (first)))))

(defcheck solution-87e1b4c
  #(let [n (mod %1 (count %2))] (concat (drop n %2) (take n %2))))

(defcheck solution-87fae487
  (let [rotate
        (fn [coll n]
          (if (= 0 n)
            coll
            (if (> n 0)
              (recur (conj (vec (next coll)) (first coll)) (dec n))
              (recur (vec (cons (last coll) (subvec coll 0 (dec (count coll))))) (inc n))
              )
            )
          )]
    (fn [n coll]
      (if (= n 0)
        coll
        (if (> n 0)
          (rotate coll (mod n (count coll)))
          (rotate (vec coll) (- 0 (mod (- 0 n) (count coll))))
          )
        )
      )
    ))

(defcheck solution-88d7e820
  (fn [sh s]
    (let [n      (mod sh (count s))
          front  (take n s)
          back   (drop n s)
          result (concat back front)]
      result)))

(defcheck solution-891a9bf8
  (fn [n ls] (let [c (count ls)
                   n (mod n c)]
               (take c (drop n (cycle ls))))))

(defcheck solution-89712993
  (fn [shift coll]
    (->> coll
      (split-at (mod shift (count coll)))
      reverse
      (apply concat))))

(defcheck solution-89992b6
  #(let [l (count %2)
         x (mod (+ % l) l)] (concat (drop x %2) (take x %2))))

(defcheck solution-89b60550
  #(let [c (count %2)]
     (->> (concat %2 %2)
       (drop (mod %1 c))
       (take c))))

(defcheck solution-89d5c63
  (fn [n l]
    (let [c (count l)
          d (mod n c)]
      (take c (drop d (cycle l))))))

(defcheck solution-89fa1f2f
  #(let [n (count %2)] (take n (drop (mod % n) (cycle %2)))))

(defcheck solution-8aa32c6
  (fn [n myseq]
    (let
     [recursor
      (fn recurs
        [n myseq]
        (if
         (= 0 n)
          myseq
          (recurs
            (- n 1)
            (concat
             (rest myseq)
             (list (first myseq))
             )
            )
          )
        )
      ]
      (recursor (mod n (count myseq)) myseq)
      )
    ))

(defcheck solution-8ad7ee4e
  (fn [n x] (let [idx (mod n (count x))]
              (concat (drop idx x) (take idx x)))))

(defcheck solution-8af0fecb
  (fn rotate [n s]
    (cond
      (zero? n) s
      (> n 0) (rotate (dec n) (concat (rest s) (list (first s))))
      :else (rotate (inc n) (cons (last s) (butlast s))))))

(defcheck solution-8af68e0a
  (fn r [n coll]
    (if (neg? n)
      (->> coll reverse (r (- n)) reverse)
      (let [n* (mod n (count coll))]
        (concat (drop n* coll) (take n* coll))))))

(defcheck solution-8b39e921
  (fn [n xs]
    (concat (drop (mod n (count xs)) xs) (take (mod n (count xs)) xs))))

(defcheck solution-8b7953d4
  #(concat (drop (mod %1 (count %2)) %2)
           (take (mod %1 (count %2)) %2)))

(defcheck solution-8bb52e97
  (fn [nm xs]
    (let [n (if (> nm 0) (mod nm (count xs)) (mod (+ (count xs) nm) (count xs)))]
      (concat (nthnext xs n) (take n xs)))))

(defcheck solution-8bd4f4dd
  (fn [at coll]
    (let [size   (count coll)
          a      (rem at size)
          b      (if (< a 0) (+ a size) a)
          split  (split-at b coll)
          before (first split)
          after  (second split)]
      (flatten (conj [] after before)))))

(defcheck solution-8c0ee410
  (fn rot [n s]
    (let [vs (vec s) m (count vs)]
      (for [i (range m)] (vs (mod (+ i n) m))))))

(defcheck solution-8c3d6c00
  #(apply concat (reverse (split-at (mod % (count %2)) %2))))

(defcheck solution-8c543bc3
  (fn rot [n vs]
    (let [m (mod n (count vs))]
      (apply concat (reverse (split-at m vs)))))

  #_(fn rot [n vs]
      (let [m (mod n (count vs))
            [front back] (split-at m vs)]
        (into (vec back) front))))

(defcheck solution-8c9f8ce4
  #(let [c (count %2)] (take c (drop (mod %1 c) (cycle %2)))))

(defcheck solution-8ce1181d
  (fn [n s]
    (let [c (count s)
          i (rem (+ (rem n c) c) c)]
      (concat (drop i s) (take i s)))))

(defcheck solution-8ce9354d
  (fn [r s]
    (take (count s) (nthrest (cycle s) (if (pos? r) r (-> (count s) (- r) inc))))))

(defcheck solution-8d4237ee
  (fn rotate [n col]
    (let [cnt (count col)
          c   (cycle col)
          r   (if (neg? n) (+ cnt (mod n cnt)) n)]
      (take cnt (drop r c))
      )))

(defcheck solution-8d7abd91
  (fn rotate [n s]
    (cond
      (pos? n) (rotate (dec n) (concat (rest s) [(first s)]))
      (neg? n) (rotate (inc n) (cons (last s) (butlast s)))
      :else s)))

(defcheck solution-8d82499c
  (fn [n s]
    (let [c (count s)
          m (first (filter pos? (iterate #(+ c %) n)))]
      (take c (drop m (cycle s))))))

(defcheck solution-8d8419e3
  (fn [n lst]
    (let [new-n (mod n (count lst))
          [tl hd] (split-at new-n lst)]
      (concat hd tl))))

(defcheck solution-8df27ea5
  #((fn [n] `(~@(nthrest %2 n) ~@(take n %2))) (mod % (count %2))))

(defcheck solution-8e045305
  #(let [[e s] (split-at (mod %1 (count %2)) %2)] (concat s e)))

(defcheck solution-8e152c22
  (fn rot [n s]
    (cond
      (neg? n) (rot (+ (count s) n) s)
      (zero? n) s
      true (rot (dec n)
             (reverse (cons (first s)
                        (reverse (rest s))))))))

(defcheck solution-8e63064e
  (fn [n aseq]
    (if (seq aseq)
      (let [[head tail] (split-at (mod n (count aseq)) aseq)]
        (concat tail head))
      aseq)))

(defcheck solution-8e637491
  (fn rot [n, xs]
    (let [
          rotate-right (fn [xs]
                         (concat (list (last xs)) (butlast xs)))
          rotate-left  (fn [xs]
                         (concat (rest xs) (list (first xs))))
          ]
      (cond
        (> n 0) (rot (- n 1) (rotate-left xs))
        (< n 0) (rot (+ n 1) (rotate-right xs))
        :else xs
        )
      )
    ))

(defcheck solution-8e6bd749
  #(let [n (mod % (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-8eae05a7
  (fn [n s] (#(concat (second %) (first %)) (split-at (mod n (count s)) s))))

(defcheck solution-8ebe363d
  (fn r
    [n seq]
    (if (> n 0)
      (concat (drop (rem n (count seq)) seq) (take (rem n (count seq)) seq))
      (r (+ (count seq) (rem n (count seq))) seq))
    ))

(defcheck solution-8f34c8ca
  (fn [n xs]
    (let [r  (if (> n 0) (rem n (count xs)) (+ (count xs) (rem n (count xs))))
          xv (vec xs)]
      (concat (subvec xv r) (subvec xv 0 r)))))

(defcheck solution-8f52c83e
  #(let [l (count %2) a (if (< % 0) (+ % l) %)]
     (apply concat (reverse (split-at (mod a l) %2)))))

(defcheck solution-8f77dc3a
  (fn rotate-sequence
    [pivot coll]
    (let [length          (count coll)
          positive-pivot? (> pivot 0)
          new-pivot       (if positive-pivot?
                            (if (>= length pivot) pivot (- pivot length))
                            (if (>= length (Math/abs pivot)) (+ length pivot) (+ pivot (* 2 length))))]
      (concat (drop new-pivot coll) (take new-pivot coll)))))

(defcheck solution-90ac92d6
  (fn shift [n coll]
    (let [size (count coll)
          skip (if (> n 0) n (+ n (* size (- n))))]
      (take size (drop skip (cycle coll))))))

(defcheck solution-90b4e736
  (fn problem-44 [whence xs]
    (letfn [(rotate [count s]
              (loop [remainder count
                     acc       s]
                (if (= remainder 0)
                  acc
                  (recur (dec remainder)
                    (reverse (conj (reverse (next acc)) (first acc))))
                  )))]
      (if (neg? whence)
        (reverse (rotate (- 0 whence) (reverse xs)))
        (rotate whence xs)))))

(defcheck solution-90b51dfc
  (fn [n s]
    (let [m (mod n (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-90c80b2a
  #(letfn [(l [s] (concat (rest s) [(first s)]))
           (r [s] (concat [(last s)] (drop-last s)))]
     (let [f (if (> 0 %1) r l)
           n (Math/abs %1)]
       (reduce (fn [a _] (f a)) %2 (range n)))))

(defcheck solution-90ea8e8d
  (fn [n coll]
    (let [n (if (neg? n) (+ (count coll) n) n)
          n (mod n (count coll))]
      (->> coll (split-at n) reverse flatten))))

(defcheck solution-91620be6
  (fn [n seq]
    (let [n (rem n (count seq))]
      (if (pos? n)
        (concat (nthrest seq n) (take n seq))
        (concat (take-last (- n) seq) (drop-last (- n) seq))))))

(defcheck solution-91925319
  (fn [s x]
    (let [s (mod s (count x))]
      (concat (drop s x) (take s x)))))

(defcheck solution-9199a826
  (fn [n coll]
    (loop [new coll c n]
      (cond
        (= 0 c) new
        (pos? c) (recur (concat (next new) [(first new)]) (dec c))
        (neg? c) (recur (concat [(last new)] (butlast new)) (inc c))))))

(defcheck solution-91ac1667
  (comp (partial apply concat)
        (juxt (partial apply drop)
          (partial apply take))
        (juxt (comp (partial apply mod)
                    (juxt first
                      (comp count second)))
          second)
        list)

  #_(fn [n s] (#(concat (drop % s) (take % s)) (mod n (count s)))))

(defcheck solution-91b9111f
  #(for [i (range (count %2))]
     (nth %2
       (rem (+ i
               (count %2)
               (rem %1 (count %2)))
         (count %2)))))

(defcheck solution-91e55b6b
  #(let [[x y] (split-at (mod % (count %2)) %2)] (concat y x)))

(defcheck solution-91fa0e20
  (fn [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs) (take n xs)))))

(defcheck solution-9223028d
  (fn [n xs]
    (let [c (count xs)
          l (mod (Math/abs n) c)
          r (- c l)]
      (if (pos? n)
        (lazy-cat (drop l xs) (take l xs))
        (lazy-cat (drop r xs) (take r xs))))))

(defcheck solution-92312f4d
  (fn rotate [n coll]
    (let [r (mod n (count coll))]
      (concat (drop r coll) (take r coll)))))

(defcheck solution-92325a1
  (fn rotate [d coll]
    (cond (= d 0) coll
          (< d 0) (reverse
                    (rotate (- d)
                      (reverse coll)))
          (> d 0) (rotate
                    (dec d)
                    (conj (into []
                            (rest coll))
                      (first coll))
                    )
          )
    ))

(defcheck solution-9272c360
  (fn [n s] (let [size (count s)
                  rot  (mod (if (>= n 0) n (+ size n)) size)] (concat (drop rot s) (take rot s)))))

(defcheck solution-929b4d41
  (fn [p vek] (take (count vek) (drop (mod p (count vek)) (cycle vek)))))

(defcheck solution-92cb32d2
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))
     ))

(defcheck solution-933dda4d
  #(let [c (count %2)]
     (take c (drop (mod % c) (cycle %2)))))

(defcheck solution-93a92bae
  (fn [n xs]
    (take (count xs) (drop (+ (* (count xs) n n) n) (cycle xs)))))

(defcheck solution-93de094
  (fn [n v]
    (loop [n n c (count v)]
      (cond (< n 0) (recur (+ n c) c)
            (>= n c) (recur (- n c) c)
            :else (concat (subvec (into [] v) n) (subvec (into [] v) 0 n))))))

(defcheck solution-951427
  (fn [n s] (flatten (conj (take (mod n (count s)) s) (nthrest s (mod n (count s)))))))

(defcheck solution-95226e4c
  (fn [n coll]
    (let [c (count coll)]
      (take c (drop (mod n c) (cycle coll))))))

(defcheck solution-9583dabc
  (fn [start coll]
    (let [v    (vec coll)
          c    (count coll)
          s    (loop [s start]
                 (cond
                   (> s c) (recur (- s c))
                   (neg? s) (recur (+ c s))
                   :else s))
          tail (subvec v 0 s)
          head (subvec v s)]
      (into head tail))))

(defcheck solution-95c42e56
  (fn rot [i ls]
    (cond
      (< i 0) (rot (inc i) (concat (list (last ls)) (drop-last ls)))
      (> i 0) (rot (dec i) (concat (rest ls) (list (first ls))))
      :else ls)))

(defcheck solution-96807447
  (fn rotate-seq
    [n s]
    (let [b (mod n (count s))]
      (concat (drop b s) (take b s)))))

(defcheck solution-968bbabd
  (fn [n s]
    (let [[a b] (split-at (mod n (count s)) s)]
      (concat b a))))

(defcheck solution-9743d5a0
  (fn [c coll]
    (let [amt (mod c (count coll))]
      (concat (drop amt coll) (take amt coll)))))

(defcheck solution-977da87
  (fn [n coll]
    (let [l (count coll)
          i (+ l l n)]
      (drop i (take (+ i l) (cycle coll))))))

(defcheck solution-9797367b
  (fn [i coll]
    (cond (= i 0) coll
          (> i 0) (recur (dec i) (concat (rest coll) (take 1 coll)))
          (< i 0) (recur (inc i) (concat (take-last 1 coll) (butlast coll))))))

(defcheck solution-97a000c2
  (fn [n col]
    (let [s (count col)
          n (rem n s)
          t #(concat (drop % col) (take % col))]
      (if (< n 0) (t (+ s n)) (t n)))))

(defcheck solution-97e6bc33
  (fn [shift tseq]
    (for
     [x (range (count tseq))]
      (nth tseq (mod (+ shift x) (count tseq)))
      )
    ))

(defcheck solution-98276f40
  (fn [n coll]
    (let [na (Math/abs n)
          c  (count coll)
          n  (if (> na c) (rem n c) n)
          nn (- c (Math/abs n))]
      (cond
        (> n 0) (concat (drop n coll) (take n coll))
        (< n 0) (concat (drop nn coll) (take nn coll))
        :else coll))))

(defcheck solution-98422e47
  (fn myf [x coll]
    (let [ct     (count coll)
          offset (if (< x 0)
                   (- ct (mod (Math/abs x) ct))
                   (mod x ct))]
      (concat (drop offset coll) (take offset coll)))))

(defcheck solution-98bfe7bb
  (fn [n xs]
    (take (count xs) (drop (mod n (count xs)) (flatten (repeat xs))))))

(defcheck solution-98c86e55
  (fn [at col]
    (apply concat (reverse (split-at (mod (+ (count col) at) (count col)) col)))))

(defcheck solution-98d5823a
  (fn [n col]
    (concat (drop (mod n (count col)) col) (take (mod n (count col)) col))))

(defcheck solution-99620c39
  #(take (count %2)
     (drop (mod % (count %2))
       (cycle %2))))

(defcheck solution-99626b78
  (fn [d s]
    (let [len (count s) offset (mod d len)]
      (take len (drop offset (cycle s))))))

(defcheck solution-99aee8e
  #(reverse (apply conj
              (let [[lhs rhs] (split-at (mod %1 (count %2)) %2)]
                (cons (reverse rhs) lhs)))))

(defcheck solution-99dfed8a
  #(let [n (count %2)
         s (mod %1 n)]
     (take n (drop s (concat %2 %2)))))

(defcheck solution-9a6f8070
  (fn [n xs]
    (let [m (mod n (count xs))]
      (concat
       (drop m xs)
       (take m xs)))))

(defcheck solution-9a95271f
  (fn
    [a b]
    (let [pop-vector          (fn [x] (into [] (rest x)))
          rotate-positive     (fn [x] (conj (pop-vector x) (first x)))
          pop-vector-negative (fn [x] (into [] (butlast x)))
          rotate-negative     (fn [x] (into [] (cons (last x) (pop-vector-negative x))))]
      (cond
        (= a 0) b
        (< a 0) (recur (+ a 1) (rotate-negative b))
        (> a 0) (recur (- a 1) (rotate-positive b))
        ))))

(defcheck solution-9ac52fe3
  (fn [n coll] (let [p (mod n (count coll))]
                 (concat (drop p coll) (take p coll)))))

(defcheck solution-9aea0513
  (fn rot [n s]
    (letfn [(rot1 [s]
              (concat (rest s) [(first s)]))]
      (cond (< n 0) (rot (+ n (count s)) s)
            (> n 0) (rot1 (rot (- n 1) s))
            true s))))

(defcheck solution-9b065149
  (fn cyclic-buffer
    [rotator coll]
    (let [smaller-coll-than-rot? (< (count coll) rotator)
          make-pos               #(if (neg? %) (- %) %)
          gen-cyclic-coll        #(lazy-cat (take-last (make-pos rotator) coll) coll)]
      (if smaller-coll-than-rot?
        (->> (take rotator (cycle coll))
          (drop (- rotator (count coll))))
        (if (neg? rotator)
          (if (> (make-pos rotator) (count coll))
            (take (count coll) (take-last (make-pos rotator) (gen-cyclic-coll)))
            (take (count coll) (gen-cyclic-coll)))
          (->> (take (+ rotator (count coll)) (cycle coll))
            (drop rotator)))))))

(defcheck solution-9b2905fa
  (fn [n coll]
    (let [coll-len (count coll)
          n        (rem n coll-len)]
      (cond
        (pos? n) (concat (drop n coll) (take n coll))
        (neg? n) (concat (drop (+ coll-len n) coll)
                         (take (+ coll-len n) coll))
        :else coll))))

(defcheck solution-9b4d3042
  (fn [n l] (if (pos? n)
              (let [r    (if (< n (count l)) n (rem n (count l)))
                    head (take r l)
                    rest (drop r l)] (concat rest head))
              (let [r    (if (< (- n) (count l)) (- n) (rem (- n) (count l)))
                    head (take r (reverse l))
                    rest (drop r (reverse l))] (concat (reverse head)
                                                       (reverse rest))))))

(defcheck solution-9c590992
  (fn [n xs]
    (let
     [sh (mod n (count xs))]
      (concat (drop sh xs) (take sh xs)))))

(defcheck solution-9c60a439
  (fn [n coll]
    (let [x (mod n (count coll))]
      (concat (drop x coll) (take x coll))
      )
    ))

(defcheck solution-9d9d063
  (fn [rott b]
    (letfn [(rotation-indices [a r]
              (map #(mod (+ % r) (count a)) (range (count a))))]
      (map #(nth b %) (rotation-indices b rott)))))

(defcheck solution-9ddb0884
  (fn f [n s]
    (cond
      (= n 0) s
      (> n 0) (recur (- n 1) (concat (rest s) (list (first s))))
      :else (reverse (f (- n) (reverse s))))))

(defcheck solution-9e0cac59
  #(flatten
     (let [o (mod (+ % (count %2)) (count %2))]
       [(drop o %2) (take o %2)])))

(defcheck solution-9e12f945
  (fn [x y]
    (take (count y) (drop (if (neg? x) (inc (max x (- x))) x) (flatten (repeat (count y) y))))))

(defcheck solution-9e1d1542
  #(let [c (count %2)] (->> %2
                         (concat %2 %2)
                         (drop (+ c (mod % c)))
                         (take c))))

(defcheck solution-9e3e1914
  (fn [n v] (let [n (mod n (count v))]
              (concat (drop n v) (take n v)))))

(defcheck solution-9eaa1278
  (fn [rot coll]
    (let [n (mod rot (count coll))]
      (apply concat (list (take-last (- (count coll) n) coll) (take n coll))))))

(defcheck solution-9ef3f2c6
  (fn rotate [distance coll]
    (let [x (mod distance (count coll))]
      (concat (drop x coll) (take x coll))
      )))

(defcheck solution-9f52e0fd
  (fn [n xs]
    (let [s (count xs)
          n (rem (+ s (rem n s)) s)]
      (apply concat ((juxt drop take) n xs)))))

(defcheck solution-9fabe610
  (fn [n c] (take
              (count c)
              (drop
                (mod n (count c))
                (apply concat (repeat c))))))

(defcheck solution-9fc59bc9
  (fn [m s]
    (let [items  (count s)
          offset (if (> m 0)
                   (rem m items)
                   (+ (rem m items) items))]
      (concat (drop offset s) (take offset s)))))

(defcheck solution-a1dc6816
  (fn rotate-sequence [x seqs]
    (let [x (mod x (count seqs))]
      (loop [x      x
             result seqs]
        (if (zero? x)
          result
          (recur (dec x)
            (concat (rest result) (list (first result)))))))))

(defcheck solution-a2f03c4e
  (fn [n coll]
    (let [coll-count (count coll)]
      (let [pos-n (mod n coll-count)]
        (concat
         (take-last (- coll-count pos-n) coll)
         (take pos-n coll))))))

(defcheck solution-a308a179
  (fn [n xs]
    (let [l (count xs)
          m (if (< n 0) (+ l (rem n l)) n)]
      (loop [i m
             [y & ys :as rs] xs
             ]
        (if (= 0 i)
          rs
          (recur (dec i) (conj (into [] ys) y))))
      )
    ))

(defcheck solution-a39ee89c
  (fn [p coll]
    (let [x (if (< p (* -1 (count coll))) (+ (* 2 (count coll)) p) (if (< p 0) (+ (count coll) p)
                                                                               (if (> p (count coll)) (- p (count coll))
                                                                                                      p)))
          c (take x coll)]
      (flatten (conj c (rest (drop (- x 1) coll)))))))

(defcheck solution-a47886a1
  #(take (count %2)
     (drop (mod % (count %2))
       (cycle %2))))

(defcheck solution-a47e9be3
  (fn [n s]
    (let [c (count s)
          p (mod (+ n c) c)]
      (mapcat identity (reverse (split-at p s))))))

(defcheck solution-a482be40
  (fn [n coll]
    (let [first-part (if (> n 0) rest last)
          last-part  (if (> n 0) first butlast)]
      (loop [n   (max n (- n))
             acc coll]
        (if (= 0 n)
          acc
          (recur (dec n) (flatten (list (first-part acc) (list (last-part acc))))))))))

(defcheck solution-a49dc0bd
  #(reduce (fn [s c] (concat (rest s) (take 1 s))) %2 (range (mod % (count %2)))))

(defcheck solution-a4d9b964
  (fn rotate [num coll]
    (let [ind (mod num (count coll))]
      (concat (drop ind coll) (take ind coll)))))

(defcheck solution-a514a1d
  (fn [n s]
    (if (> n 0)
      (loop [tmpn n tmps s]
        (if (= 0 tmpn)
          tmps
          (recur (dec tmpn) (concat (rest tmps) (list (first tmps))))))
      (loop [tmpn n tmps s]
        (if (= 0 tmpn)
          tmps
          (recur (inc tmpn) (concat (list (last tmps)) (butlast tmps))))))))

(defcheck solution-a54d70d9
  (fn [a b]
    (let [aa (Math/abs a)
          cb (count b)
          x  (if (> a 0)
               (rem aa cb)
               (- cb (rem aa cb)))
          ]
      (concat (drop x b) (take x b)))))

(defcheck solution-a5a7ac13
  (fn f [n sq]
    (let [m (mod n (count sq))]
      (concat (drop m sq) (take m sq)))))

(defcheck solution-a5fda720
  (fn [n coll]
    (let [n (mod n (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-a6b36125
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-a7245200
  (fn rot [n coll]
    (->> coll
      (map-indexed #(vector %1 %2))
      (map (fn [[ind val]] [(mod (- ind n) (count coll)) val]))
      (sort-by first)
      (map last)
      )))

(defcheck solution-a7316b22
  (fn [n c]
    (cond
      (= 0 n) c
      (< 0 n) (recur (dec n) (conj (vec (rest c)) (first c)))
      :else (recur (inc n) (vec (cons (last c) (drop-last c)))))))

(defcheck solution-a7f57c0f
  (fn [x y]
    (concat
     (drop (mod x (count y)) y)
     (take (mod x (count y)) y))))

(defcheck solution-a8123805
  (fn rotate [n xs]
    (take (count xs) (drop (mod n (count xs)) (cycle xs)))))

(defcheck solution-a81b9611
  (fn [n s]
    (let [n (mod n (count s))]
      (flatten (cons (drop n s) (drop-last (- (count s) n) s))))))

(defcheck solution-a8587e10
  (fn [cnt myseq]

    (let [length (count myseq)

          steps  (rem cnt length)

          ]
      (if (> steps 0)

        (concat (drop steps myseq) (take steps myseq))

        (concat (drop (inc (* -1 steps)) myseq) (take (inc (* -1 steps)) myseq))
        )

      )

    ))

(defcheck solution-a87d4f7f
  (fn f
    [c d]
    (let [cc (count d)
          c  (mod c cc)]
      (take cc
        (drop c (cycle d))))))

(defcheck solution-a885bd89
  (fn [n coll]
    (let [len (count coll)
          x   (if (> n 0) (rem n len) (+ len (rem n len)))]
      (take len (nthrest (concat coll coll) x)))))

(defcheck solution-a8dec35d
  (fn [n s]
    (cond (= n 0) s
          (< n 0) (recur (inc n) (cons (last s) (butlast s)))
          (> n 0) (recur (dec n) (conj (vec (rest s)) (first s))))))

(defcheck solution-a8fa6975
  (fn [rot coll]
    (let [f-rot (mod rot (count coll))]
      (concat (drop f-rot coll)
              (take f-rot coll)))))

(defcheck solution-a91e1147
  (fn rotate-seq [n sx]
    (let [len (count sx)
          d   (mod n len)]
      (take len (drop d (cycle sx))))))

(defcheck solution-a9813e5e
  (fn [n s]
    (let [n (mod n (count s))]
      (if (>= n 0) (concat (drop n s) (take n s))
                   (let [i (+ (count s) n)]
                     (concat (drop i s) (take i s)))))))

(defcheck solution-a9a6d1aa
  #(let [n (count %2)]
     (concat (drop (mod %1 n) %2) (take (mod %1 n) %2))))

(defcheck solution-a9ccf93f
  (fn [rot col]
    (let [realrot (mod rot (count col))
          [f r] (split-at realrot col)]
      (concat r f))))

(defcheck solution-aa911549
  (fn rot [n c]
    (let [cnt (count c)
          r   (rem n cnt)
          nn  (if (neg? r) (+ cnt r) r)
          [a b] (split-at nn (vec c))]
      (concat b a))))

(defcheck solution-aa9d9584
  #(let [rotation (mod %1 (count %2))]
     (concat (drop rotation %2)
             (take rotation %2))))

(defcheck solution-aae37be8
  (fn rotate [n coll]
    (let [x (mod (Math/abs n) (count coll))]
      (if (< n 0)
        (concat (take-last x coll) (drop-last x coll))
        (concat (drop x coll) (take x coll))))))

(defcheck solution-aaf2cf85
  (fn [n s]
    (let [cnt (loop [i n, t (count s)]
                (if (and (>= i 0) (< i t))
                  i
                  (if (> i 0)
                    (recur (- i t) t)
                    (recur (+ i t) t)
                    )))]
      (concat (drop cnt s) (take cnt s)))))

(defcheck solution-ab8b21e
  (fn [n s]
    (let [amount (mod n (count s))]
      (concat (drop amount s) (take amount s)))))

(defcheck solution-ab8d5696
  (fn [n lst] (let [n (mod n (count lst))] (concat (drop n lst) (take n lst)))))

(defcheck solution-abc75fc
  (fn [i c]
    (if (= 0 i)
      c
      (if (> i 0)
        (recur (- i 1) (conj (vec (next c)) (first c)))
        (recur (+ i 1) (cons (last c) (vec (drop-last c))))))))

(defcheck solution-abd5d55c
  (fn go [n xs]
    (if (neg? n)
      (reverse (go (- n) (reverse xs)))
      (if (> n (count xs))
        (go (mod n (count xs)) xs)
        (let [[a b] (split-at n xs)]
          (concat b a))))))

(defcheck solution-ac64a956
  (fn f [n, s]
    (cond
      (= n 0) s
      (< n 0) (f (inc n) (conj (butlast s) (last s)))
      1 (f (dec n) (concat (rest s) (list (first s)))))))

(defcheck solution-acd136d7
  (fn m [n l]
    (let [len       (count l)
          order     (range len)
          new-order (map #(mod (+ % n) len) order)
          new-l     (into [] l)]
      (map #(new-l %) new-order))))

(defcheck solution-acd92e8f
  (fn my-rotate
    [n seq]
    (let [actual-n (mod n (count seq))]
      (concat (drop actual-n seq) (take actual-n seq)))))

(defcheck solution-ad17bfaf
  (fn rotate
    [n s]
    (let [abs (fn [x] (if (< x 0) (unchecked-negate-int x) x)) r (mod (abs n) (count s))]
      (if (> n 0)
        (concat (nthrest s r) (take r s))
        (concat (take-last r s) (take (- (count s) r) s))))))

(defcheck solution-ad517052
  (fn [k s]
    (let [n (count s)]
      (take n (drop (mod k n) (cycle s)))
      )
    ))

(defcheck solution-ad590792
  (fn [n s] (let [l (split-at (mod n (count s)) s)] (concat (second l) (first l)))))

(defcheck solution-ad69edbb
  (fn [m x]
    (let [x (vec x) n (count x)]
      (map x
        (map
          #(mod (+ m %) n)
          (range n)
          )
        )
      )
    ))

(defcheck solution-ad8a5ee1
  (fn [n s] (let [x (mod n (count s))] (concat (drop x s) (take x s)))))

(defcheck solution-adc99d69
  (fn rot [n coll]
    (let [b (partition-all (mod n (count coll)) coll)]
      (flatten (concat (rest b) (first b))))))

(defcheck solution-ae08ec9e
  (fn [n xs]
    (let [m (mod n (count xs))]
      (concat (drop m xs)
              (take m xs)))))

(defcheck solution-ae782148
  (fn [on xs]
    (let [n (mod on (count xs))]
      (concat (drop n xs) (take n xs)))
    ))

(defcheck solution-af334609
  (fn [n xs]
    (let [c (mod n (count xs))
          [h t] (split-at c xs)]
      (concat t h))))

(defcheck solution-af812e74
  (fn [n s] (let [c (count s)] (->> (cycle s) (drop (mod n c)) (take c)))))

(defcheck solution-b02fcde8
  (letfn [(left [xs] (concat (rest xs) (list (first xs))))
          (rotate-left [n xs] (if (zero? n) xs
                                            (rotate-left (dec n) (left xs))))
          (rotate-right [n xs] (reverse (rotate-left n (reverse xs))))]
    (fn [n xs] (if (< n 0) (rotate-right (Math/abs n) xs)
                           (rotate-left n xs)))))

(defcheck solution-b05a48e7
  #(->> (split-at (mod %1 (count %2))
          %2)
     (reverse)
     (apply concat)))

(defcheck solution-b0846eb3
  (fn r [n s]
    (let [q (mod n (count s))]
      (concat (drop q s) (take q s))
      )
    ))

(defcheck solution-b09ae28d
  #(let [n (count %2)] (->> %2 cycle (drop (mod % n)) (take n))))

(defcheck solution-b0a833fa
  (fn [n ls]
    (loop [an ls i n]
      (cond
        (zero? i) an
        (pos? i) (recur (concat (rest an) (list (first an))) (dec i))
        :else (recur (cons (last an) (butlast an)) (inc i))))))

(defcheck solution-b1222319
  #(let [n (mod % (count %2))] (concat (drop n %2) (take n %2))))

(defcheck solution-b12a5704
  (fn [n xs] (let [offset (mod n (count xs))] (concat (drop offset xs) (take offset xs)))))

(defcheck solution-b16901c5
  (fn [steps coll] (let [n (mod steps (count coll))
                         [a b] (split-at n coll)]
                     (concat b a))))

(defcheck solution-b18e68f4
  (fn [n coll]
    (let [s (mod n (count coll))]
      (concat (drop s coll) (take s coll)))))

(defcheck solution-b1c7683b
  #_(duh... just mod instead of repeatedly rotating)

  (fn [n s]
    (let [n (if (neg? n) (mod n (count s)) n)]
      (last (take (inc n) (iterate (fn [[x & xs]] (conj (vec xs) x)) s))))))

(defcheck solution-b2362251
  (fn [n xs]
    (let [x (count xs)]
      (take x
        (drop
          (if (neg? n) (inc (- x n)) n)
          (cycle xs))))))

(defcheck solution-b236c0a1
  (fn [rot coll]
    (let [n     (count coll)
          start (mod (+ rot n) n)]
      (for [i (concat (range start n) (range start))]
        (nth coll i)))))

(defcheck solution-b2469ee9
  (fn [n coll] (let [[a b] (split-at (mod n (count coll)) coll)]
                 (concat b a))))

(defcheck solution-b28c12d7
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-b2c81de3
  (fn [n xs]
    (let [n (rem n (count xs))]
      (if (> n 0)
        (concat (drop n xs) (take n xs))
        (concat (take-last (- n) xs) (drop-last (- n) xs))
        )
      )
    ))

(defcheck solution-b2e96c9c
  (fn [n coll]
    (let [nn (mod n (count coll))]
      (concat
       (drop nn coll)
       (take nn coll)
       )
      )))

(defcheck solution-b2ef5ec8
  (fn [amount vals]
    (let [cnt (mod amount (count vals))]
      (if (< 0 cnt)
        (concat (drop cnt vals) (take cnt vals))
        (concat (take-last (* -1 cnt) vals) (drop-last (* -1 cnt) vals))))))

(defcheck solution-b354f8b8
  (fn rotate [n seq]
    (let [cnt (count seq)
          rem (mod (+ cnt n) cnt)
          n   (mod n cnt)]
      (if (>= n 0)
        (concat (drop n seq) (take n seq))
        (concat (drop rem seq) (take rem seq))))))

(defcheck solution-b3706e4c
  (fn [n seq]
    (loop [pos (mod (- n 1) (count seq))
           arr (list)]
      (if (= (count arr) (count seq)) arr
                                      (recur (mod (- pos 1) (count seq)) (cons (nth seq pos) arr))))))

(defcheck solution-b38cf9e
  #(let [idx (mod %1 (count %2))]
     (concat (drop idx %2) (take idx %2))))

(defcheck solution-b3b02c6a
  (fn rote [direc alist]
    (let [absfunc   #(max % (- %))
          absval    (absfunc direc)
          realdirec (if (>= absval (count alist))
                      (if (= direc absval)
                        (- direc (count alist))
                        (+ direc (count alist)))
                      direc)
          realabs   (absfunc realdirec)]
      (if (> realdirec 0)
        (concat (drop realabs alist) (drop-last (- (count alist) realabs) alist))
        (concat (drop (- (count alist) realabs) alist) (drop-last realabs alist))))))

(defcheck solution-b47859ed
  (fn r [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs) (take n xs)))))

(defcheck solution-b4d61058
  (fn rotate
    [n collection]
    (let [num (mod n (count collection))
          fun #(apply vector (%1 %2 collection))]
      (into (fun drop num) (fun take num)))))

(defcheck solution-b5aec9d9
  (fn rotate- [distance coll]
    "44. Write a function which can rotate a sequence in either direction."
    (let [d (mod distance (count coll))]
      (if (pos? d)
        (concat (drop d coll) (take d coll))
        (concat (take-last (- d) coll) (take (+ (count coll) d) coll))))))

(defcheck solution-b6591334
  (fn rotate [n sq] (if (>= n 0) (if (< n (count sq)) (concat (drop n sq) (take n sq)) (rotate (- n (count sq)) sq)) (rotate (+ (count sq) n) sq))))

(defcheck solution-b6700ac
  (fn [x coll]
    (let [c (mod x (count coll))]
      (concat (drop c coll) (take c coll)))))

(defcheck solution-b69836b4
  #(take (count %2) (drop (mod % (count %2)) (cycle %2))))

(defcheck solution-b6b532e
  (fn rotate [k c]
    (let [n (mod k (count c))]
      (concat (nthrest c n) (take n c)))))

(defcheck solution-b700fa9d
  #(let [cnt (count %2)] (take cnt (nthrest (cycle %2) (+ (* 2 cnt) %1)))))

(defcheck solution-b71abcec
  (fn [n s] (take (count s) (drop (mod n (count s)) (cycle s)))))

(defcheck solution-b7648869
  (fn [n xs]
    (let [index (mod n (count xs))]
      (flatten [(drop index xs) (take index xs)]))))

(defcheck solution-b78602e
  (fn rotate [n coll]
    (let [sign (>= n 0)
          n    (mod (Math/abs n) (count coll))
          skip (if sign n (- (count coll) n))]
      (concat
       (drop skip coll)
       (take skip coll)))))

(defcheck solution-b80bd239
  (fn rotate [n s]
    (cond
      (> 0 n) (rotate (+ (count s) n) s)
      (< (count s) n) (rotate (mod n (count s)) s)
      :else (concat (drop n s) (take n s)))))

(defcheck solution-b814676b
  (fn [n_ s]
    (let [n (mod n_ (count s))]
      (concat (drop n s) (take n s))
      )))

(defcheck solution-b8a89fca
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-b8abf67e
  #(let [n (mod %1 (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-b8fe6698
  (fn [end xs]
    (let [cnt (count xs)
          inc (if (> end 0) 1 -1)
          rot (if (< end 0)
                #(let [[front end] (split-at (dec cnt) %)]
                   (concat end front))
                #(let [[front end] (split-at 1 %)]
                   (concat end front)))]
      (loop [xs xs i 0]
        (if (== i end)
          xs
          (recur (rot xs) (+ inc i)))))))

(defcheck solution-b95d5bb5
  (fn [n r] (let [d (mod n (count r))] (concat (drop d r) (take d r)))))

(defcheck solution-b96085d9
  (fn [n col] (let [step (mod n (count col))] (concat (drop step col) (take step col)))))

(defcheck solution-b965a97e
  (fn [n coll]
    (take (count coll) (drop (mod n (count coll)) (cycle coll)))
    ))

(defcheck solution-b9b75d4
  (fn f [i, s]
    (let [len (count s) n (mod i len)]
      (concat ((fn g [j, t]
                 (if (= 0 j) t (g (dec j) (rest t)))) n s) (take n s)))))

(defcheck solution-b9ec7291
  (fn rotate [x y]
    (if (= x 0) y
                (if (> x 0)
                  (rotate (- x 1) (concat (rest y) (list (first y))))
                  (rotate (+ x 1) (cons (last y) (butlast y)))
                  )
                )
    ))

(defcheck solution-ba1b1f32
  #(let [i (mod % (count %2))] (concat (drop i %2) (take i %2))))

(defcheck solution-ba1fac0b
  (fn rotate [n x] (take (count x) (drop (mod n (count x)) (flatten (iterate identity x))))))

(defcheck solution-ba848528
  (fn my-rotate [offset coll]
    (let [pos-offset (mod offset (count coll))
          splitted   (split-at pos-offset coll)
          switched   [(second splitted) (first splitted)]]
      (apply concat switched))))

(defcheck solution-baa05b66
  (fn [n sq]
    (let [v   (vec sq)
          len (count v)
          n_  (if (<= 0 n) n (+ len n))
          mid (mod n_ len)
          a   (subvec v mid len)
          b   (subvec v 0 mid)]
      (concat a b))))

(defcheck solution-baed0155
  (fn [x col] (reduce #(concat %2 %1) (split-at (mod x (count col)) col))))

(defcheck solution-bbf46f4e
  #(let [i (mod %1 (count %2))] (concat (drop i %2) (take i %2))))

(defcheck solution-bc53df26
  (fn [i s]
    (if (pos? i)
      (take (count s) (drop i (cycle s)))
      (take (count s) (drop (- (count s) (rem (Math/abs i) (count s))) (cycle s))))))

(defcheck solution-bc9dc254
  #(concat (drop (mod %1 (count %2)) %2) (take (mod %1 (count %2)) %2)))

(defcheck solution-bd9e7bff
  (fn [rot lst]
    (letfn [(permute [idx] (map #(nth lst %1) idx))]
      (permute (map #(mod (+ %1 rot) (count lst)) (range (count lst)))))))

(defcheck solution-bda510cb
  (fn rotate-sequence [nn c]
    (list*

      (let [v   (vec c),
            c   (count v)
            abs (fn abs [y] (if (>= y 0) y (* -1 y)))
            n   (if (< (abs nn) c) nn (rem nn c))]
        (if (>= n 0)
          (into (subvec v n) (subvec v 0 n))
          (into (subvec v (inc (abs n)) c) (subvec v 0 (inc (abs n))))
          ))

      )))

(defcheck solution-bdcfdd93
  (fn [rot ls]
    (let [offset (mod rot (count ls))]
      (concat (drop offset ls) (take offset ls)))))

(defcheck solution-be3481b1
  (fn rotate [pivot coll]
    (let [i (mod pivot (count coll))]
      (concat (drop i coll) (take i coll)))))

(defcheck solution-be361587
  (fn [n s]
    (let [g (rem n (count s))]
      (if (pos? g)
        (concat (drop g s) (drop-last (- (count s) g) s))
        (concat (drop (+ (count s) g) s) (drop-last (- 0 g) s))))))

(defcheck solution-be731a05
  (fn rotate [n s]
    (let [l (count s)]
      (map-indexed (fn [idx val] (nth s (mod (+ idx n) l))) s))))

(defcheck solution-beaff718
  (fn __ [rot col]
    (let [pivot (rem rot (count col))]
      (cond
        (= pivot 0) col
        (> pivot 0) (concat (drop pivot col) (take pivot col))
        :else (concat (take-last (* -1 pivot) col) (drop-last (* -1 pivot) col))))))

(defcheck solution-bf0ce3bd
  #(let [cnt (count %2)
         n   (rem % cnt)
         n   (if (pos? n) n (+ n cnt))]
     (apply concat (reverse (split-at n %2)))))

(defcheck solution-bf3757ef
  (fn [n xs]
    (let [len (count xs)
          m   (mod (+ n len) len)]
      (concat (drop m xs) (take m xs)))))

(defcheck solution-bf85e453
  (fn [n c]
    (let [m (mod n (count c))]
      (concat (drop m c) (take m c)))))

(defcheck solution-c05d98ae
  (fn my-rotate [n l]
    (let [rot (rem n (count l))]
      (flatten (concat (reverse
                         (split-at
                           (if (neg? rot) (+ (count l) rot) rot)
                           l)))))))

(defcheck solution-c0923f4f
  #(let [c (count %2)] (take c (drop (mod (+ c %) c) (cycle %2)))))

(defcheck solution-c0925e7a
  (fn [index sequence]
    (take (count sequence) (drop (mod index (count sequence)) (cycle sequence)))))

(defcheck solution-c0f6b5e3
  (fn [n xs]
    (let [xs' (if (neg? n) (reverse xs) xs)
          n'  (mod (if (neg? n) (- n) n) (count xs'))
          ys  (concat (drop n' xs') (take n' xs'))]
      (if (neg? n) (reverse ys) ys))))

(defcheck solution-c0f77460
  (fn f [n v]
    (loop [nn (+ (count v) (rem n (count v))) vv v]
      (if (= nn 0) vv
                   (recur (dec nn) (conj (vec (drop 1 vv)) (first vv)))))))

(defcheck solution-c134be94
  #(flatten (reverse (split-at (mod % (count %2)) %2))))

(defcheck solution-c160be0d
  (fn rot [n seq]
    (cond (= n 0) seq,
          (> n 0) (rot (- n 1) (concat (rest seq) [(first seq)])),
          (< n 0) (rot (+ n 1) (concat (take-last 1 seq) (drop-last 1 seq))))))

(defcheck solution-c164cb2
  (fn [r xs]
    (let [n (count xs)]
      (take n (drop (+ n (mod r n)) (cycle xs))))))

(defcheck solution-c1755b5b
  (fn rotate [n seq]
    (cond (> n 0) (recur (- n 1) (concat (rest seq) [(first seq)]))
          (< n 0) (recur (+ n 1) (concat [(last seq)] (butlast seq)))
          (= n 0) seq)))

(defcheck solution-c1909394
  (fn rot [n s]
    (if (= n 0) s
                (if (> n 0)
                  (rot (dec n) (concat (rest s) [(first s)]))
                  (rot (inc n) (concat [(last s)] (take (dec (count s)) s)))))))

(defcheck solution-c19d7d93
  (fn [n coll]
    (->> coll
      (split-at (mod (+ (count coll) n) (count coll)))
      reverse
      (apply concat))))

(defcheck solution-c1d355f5
  (fn [rot xs]
    (let [idx (mod rot (count xs))]
      (concat
       (drop idx xs)
       (take idx xs)))))

(defcheck solution-c2479d13
  (fn rot [cnt s]
    (if (< cnt 0)
      (reverse (rot (- cnt) (reverse s)))
      (let [c (rem cnt (count s))]
        (concat (drop c s) (take c s))))))

(defcheck solution-c281677b
  (fn rotate [n v]
    (->> (concat v v)
      (drop (mod n (count v)))
      (take (count v))
      )
    ))

(defcheck solution-c31ce0d
  (fn [n s]
    (let [l (count s)
          n (mod n l)]
      (if (>= n 0)
        (concat (nthrest s n) (take n s))
        (concat (nthrest s (+ l n)) (take (+ l n) s))))))

(defcheck solution-c32435b1
  (fn
    [n coll]
    (let [l (apply list coll)
          c (count l)
          m (first
              (remove
                #(< % 0)
                (iterate
                  #(+ % c)
                  (+ c n))))]
      (take c (drop m (cycle l))))))

(defcheck solution-c330011
  (fn [n s] (take (count s) (drop (mod n (count s))
                              (flatten (repeat s))))))

(defcheck solution-c3500c58
  #(let [[x y] (split-at (mod %1 (count %2)) %2)] `(~@y ~@x)))

(defcheck solution-c3870af3
  (fn [n s] (let [i (mod n (count s))] (concat (drop i s) (take i s)))))

(defcheck solution-c39b987b
  (fn f [n s]
    (cond (< n 0) (f (+ (count s) n) s)
          (> n (count s)) (f (- n (count s)) s)
          1
          (let [[a b] (split-at n s)]
            (concat b a)))))

(defcheck solution-c3f7cd09
  (fn rot [n s]
    (if (< n 0)
      (reverse (rot (- 0 n) (reverse s)))
      (let [r (mod n (count s))]
        (concat (nthrest s r) (take r s))))))

(defcheck solution-c4b6bf84
  (fn prob44 [n s]
    (let [len (count s)
          idx (cond
                ;; (< n 0) (- len (mod (- 1 n) len))
                (< n 0) (+ len (mod n (- len)))
                (> n len) (mod n len)
                :else n)]
      (lazy-cat (drop idx s) (take idx s)))))

(defcheck solution-c50dee38
  #(let [n (mod %1 (count %2)) col %2]
     (concat (drop n col) (take n col))))

(defcheck solution-c560d9c1
  (fn
    [rot coll]
    (loop [rot  rot
           coll coll]
      (cond
        (neg? rot) (recur (inc rot) (cons (last coll) (drop-last coll)))
        (pos? rot) (recur (dec rot) (flatten (cons (rest coll) [(first coll)])))
        :else coll))))

(defcheck solution-c563955e
  #(let [n (mod % (count %2))]
     (cond
       (neg? n) (concat (take-last (- n) %2) (drop-last (- n) %2))
       :else (concat (drop n %2) (take n %2)))))

(defcheck solution-c56cf01
  (fn [n coll]
    (let [m (mod n (count coll))]
      (concat (drop m coll) (take m coll)))))

(defcheck solution-c5cbe4c2
  (fn [z coll]
    (let [s (mod z (count coll))]
      (apply concat (reverse (split-at s coll))))))

(defcheck solution-c60b53d0
  (fn [n coll]
    (cond
      (< n 0) (recur (inc n) (cons (last coll) (drop-last 1 coll)))
      (> n 0) (recur (dec n) (concat (rest coll) [(first coll)]))
      (= n 0) coll)))

(defcheck solution-c617c573
  (fn
    [rot li]
    (let
     [a-rot (mod rot (count li))]
      (concat
       (drop a-rot li)
       (take a-rot li)))))

(defcheck solution-c61b1959
  #(nth (iterate (fn [c] (conj (vec (rest c)) (first c))) %2)
     (if (> % 0)
       % (+ % (* 2 (count %2))))))

(defcheck solution-c65248e2
  (fn rotate [dir coll]
    (let [d (rem dir (count coll))]
      (cond
        (pos? d) (concat (drop d coll) (take d coll))
        :else (concat (take-last (* -1 d) coll) (drop-last (* -1 d) coll))))))

(defcheck solution-c657c5d0
  (fn [n coll]
    (let [c  (count coll)
          d  (quot n c)
          m  (+ 1 (max d (- d)))
          d2 (+ n (* c m))]
      (take c (drop d2 (cycle coll))))))

(defcheck solution-c6821293
  (fn rot [n xs]
    (let [m (mod n (count xs))]
      (concat (drop m xs) (take m xs)))))

(defcheck solution-c7b23c
  (fn [n lst]
    (let [count (count lst)
          len   (mod (+ count n) count)]
      (concat (drop len lst) (take len lst)))))

(defcheck solution-c8204c39
  (fn [n s]
    (let [is-neg (neg? n)
          n      (Math/abs n)
          n      (rem n (count s))
          n      (if is-neg (- (count s) n) n)]
      (concat (drop n s) (take n s)))))

(defcheck solution-c8830a2f
  (fn [n s]
    (let [x (mod n (count s))]
      (concat (drop x s)
              (take x s)))))

(defcheck solution-c8a330d4
  (fn rotate [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs)
              (take n xs)))))

(defcheck solution-c8a76528
  ;implemented mod, i guess

  #(let [c (count %2)]
     (loop [x %1]
       (if (pos? x)
         (if (<= x c)
           (concat (nthrest %2 x) (take x %2))
           (recur (- x c)))
         (if (<= (Math/abs x) c)
           (recur (+ c x))
           (recur (- (rem (Math/abs x) c))))))))

(defcheck solution-c8f30bd9
  (fn [n se]
    (let [
          cc (mod n (count se))
          ]
      (concat (drop cc se) (take cc se)))))

(defcheck solution-c90afa85
  (fn [pos coll]
    (let [cnt (count coll)
          pos (mod pos (count coll))]
      (concat (drop pos coll) (take pos coll)))))

(defcheck solution-c983fd9b
  (fn [n xs]
    (let [nrem (rem n (count xs))
          npos (if (neg? nrem) (+ (count xs) nrem) nrem)
          [x y] (split-at npos xs)]
      (concat y x)
      )
    ))

(defcheck solution-ca030f01
  (fn [n coll] (let [m (mod n (count coll))] (concat (drop m coll) (take m coll)))))

(defcheck solution-ca1f6509
  (fn [x lat]
    (let [len   (count lat)
          n     (if (> len x)
                  (->> (take-while (partial > len) (iterate #(+ % len) x)) last)
                  (->> (take-while (partial < 0) (iterate #(- % len) x)) last))
          left  (take n lat)
          right (drop n lat)]
      (concat right left))))

(defcheck solution-ca2d1ecf
  (fn rotate [n xs]
    (let [r (mod n (count xs))]
      (concat (drop r xs) (take r xs)))))

(defcheck solution-ca312ffe
  (fn [n xs]
    (let [k (if (< n 0)
              (first
                (filter
                  #(>= % 0)
                  (map #(+ n (* (count xs) %))
                    (range))))
              (first
                (filter
                  #(< % (count xs))
                  (map #(+ n (* (count xs) (- %)))
                    (range)))))]
      (concat (drop k xs) (take k xs)))))

(defcheck solution-ca4607ec
  (fn [n s]
    (let [m (mod n (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-ca880862
  #(take
     (count %2)
     (drop
       (mod %1 (count %2))
       (cycle %2))))

(defcheck solution-cb0277f7
  (fn [i s]
    (let [len (count s)
          pos ((fn [n]
                 (cond
                   (< n (- len)) (+ len (rem n len))
                   (> n 0) n (< n 0)
                   (+ len n))) i)]
      (take-last len
        (take (+ pos len) (cycle s))))))

(defcheck solution-cb5185cd
  (fn rotate [d v] (let [d (mod d (count v))] (concat (drop d v) (take d v)))))

(defcheck solution-cba0d638
  (fn [z xs] (let [l (count xs) n (mod z l)] (concat (drop n xs) (take n xs)))))

(defcheck solution-cc066de6
  (fn [x s]
    (let [y (mod x (count s))]
      (concat (drop y s) (take y s)))
    ))

(defcheck solution-cc148e62
  #(let [x (mod % (count %2))]
     (concat (drop x %2) (take x %2))))

(defcheck solution-cc8a5e78
  (fn [x y]
    (let [z (last (take (+ 1 (/ (Math/abs x) (count y))) (iterate (partial + (count y)) x)))]
      (drop z (take (+ (count y) z) (cycle y))))))

(defcheck solution-cc992902
  (fn [n s]
    (let [len (count s)]
      (loop [shifted [], y n]
        (if (= (count shifted) len)
          shifted
          (let [pos (rem y len)]
            (recur (conj shifted (nth s (if (>= pos 0) pos (+ len pos)))) (inc y))
            ))))))

(defcheck solution-cd006e03
  #(let [x (mod %1 (count %2))] (concat (drop x %2) (take x %2))))

(defcheck solution-cd7605e3
  (fn rotate [n col]
    (let [size   (count col)
          offset (mod n size)]
      (take size (drop offset (apply concat (repeat 2 col)))))))

(defcheck solution-cdfbe353
  (fn [n s]
    (let [n (if (neg? n) (+ (count s) n) n)
          n (mod n (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-ce2421dd
  (fn [rot-by coll]
    (let [len (count coll)
          inf (flatten (repeat coll))]
      (take len
        (if (pos? rot-by) (drop rot-by inf)
                          (drop (mod rot-by len) inf))))))

(defcheck solution-ce5611e6
  #(let [c (count %2)] (take c (drop (mod % c) (cycle %2)))))

(defcheck solution-ce618dab
  #(->> (split-at (mod % (count %2)) %2)
     reverse
     (apply concat)))

(defcheck solution-ce73e74e
  #(let [n (count %2)] (take n (drop (mod %1 n) (cycle %2)))))

(defcheck solution-cedaa20c
  (fn [n s]
    (nth (iterate #(concat (rest %) (list (first %))) s)
      (mod n (count s)))))

(defcheck solution-cee69a58
  #(loop [n %
          l %2]
     (cond
       (> n 0) (recur (dec n) (concat (rest l) (list (first l))))
       (< n 0) (recur (inc n) (concat (list (last l)) (butlast l)))
       :default l)))

(defcheck solution-cf066cc6
  (fn rotate [n s]
    (let [n (mod n (count s))]
      (concat (drop n s) (take n s)))))

(defcheck solution-cf506bbb
  (fn [x y] (let [m (mod x (count y))] (concat (drop m y) (take m y)))))

(defcheck solution-cfa9ae4c
  #(if (pos? %1) (concat (drop (mod %1 (count %2)) %2) (take (mod %1 (count %2)) %2))
                 (let [n (if (> (count %2) (Math/abs %1)) (+ (count %2) %1) 2)] (concat (drop n %2) (take n %2)))))

(defcheck solution-d0cce99a
  (fn rot [how what]
    (if (= how 0)
      what
      (if (< how 0)
        (rot (inc how) (flatten (list (last what) (drop-last what))))
        (rot (dec how) (flatten (list (rest what) (first what))))))))

(defcheck solution-d0d0f6cc
  #(reduce (fn [x y] (concat y x))
     (split-at (mod %1 (count %2)) %2)
     ))

(defcheck solution-d0fed0
  (fn [n c]
    (let [v   (into [] c)
          len (count v)
          nr  (mod n len)]
      (concat (drop nr v) (take nr v)))))

(defcheck solution-d1017062
  (fn [c s]
    (let [n (count s)
          m (mod c n)]
      (take n (drop m (cycle s))))))

(defcheck solution-d160ff59
  #(drop (mod %1 (count %2)) (take (+ (mod %1 (count %2)) (count %2)) (cycle %2))))

(defcheck solution-d16e2b81
  #(let [n (mod %1 (count %2))]
     (flatten (reverse (split-at n %2)))))

(defcheck solution-d2834ab8
  (fn [x coll]
    (let [cnt (count coll)
          nx  (mod (if (pos? x) x (+ cnt x)) cnt)]
      (concat (drop nx coll) (take nx coll)))))

(defcheck solution-d2a3caaf
  (fn [x col]
    (let [n (mod x (count col))]
      (concat (drop n col) (take n col)))))

(defcheck solution-d38eb2f0
  ;(fn [n xs]
  ;  (let [n (->> xs count (mod n) inc)
  ;        rot1 (fn [xs] (concat (rest xs) [(first xs)]))]
  ;    (->>
  ;      (iterate rot1 xs)
  ;      (take n)
  ;      last)))

  (fn [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs) (take n xs)))))

(defcheck solution-d3bdb312
  (fn [s xs]
    (let [n     (count xs)
          s     (mod s n)
          left  (drop s xs)
          right (take s xs)
          ys    (concat left right)]
      ys)))

(defcheck solution-d3e331d2
  (fn [i coll]
    (let [c (count coll)
          m (mod i c)]
      (take c (drop m (cycle coll))))))

(defcheck solution-d3ec9186
  (letfn
   [(rotate [pos coll]
      (let [v    (vec coll)
            c    (count v)
            incr (if (neg? pos) c (- c))
            n    (first
                   (drop-while #(or (neg? %) (> % c))
                     (iterate #(+ % incr) pos)))]
        (concat (subvec v n) (subvec v 0 n))))]
    #(rotate %1 %2)))

(defcheck solution-d3ee1e07
  (fn rotate [n a-seq]
    (cond
      (= n 0) a-seq
      (> n 0) (rotate (dec n) (rest
                                (conj (into [] a-seq) (first a-seq))))
      :else (rotate (inc n) (butlast
                              (cons (last a-seq) a-seq))))))

(defcheck solution-d4184579
  #(let [x (mod %1 (count %2))
         y (- (count %2) x)]
     (concat (take-last y %2) (take x %2))))

(defcheck solution-d449357c
  (fn rotate [n s]
    (let [arg (mod (Math/abs n) (count s))]
      (if (not= arg 0)
        (if (neg? n)
          (concat (take-last arg s) (drop-last arg s))
          (concat (drop arg s) (take arg s)))
        s))))

(defcheck solution-d4ace78c
  #(let [shift (mod %1 (count %2))]
     (concat (drop shift %2) (take shift %2))))

(defcheck solution-d4fbf870
  (fn [p s]
    (let [f #(concat %2 %1)
          g #(apply f (split-at %1 %2))
          c (count s)
          n (mod (+ p c) c)]
      (g n s))))

(defcheck solution-d514a56b
  (fn r [n coll]
    (let [c (count coll)
          n (rem (+ n (* (max n (- n)) c)) c)]
      (concat (drop n coll) (take n coll))
      )
    ))

(defcheck solution-d53f71f4
  (fn k [n s] (let [r (mod n (count s))] (concat (drop r s) (take r s)))))

(defcheck solution-d5496076
  (fn [n coll]
    (let [c (count coll) a (mod n c)]
      (flatten ((juxt drop take) a coll)))))

(defcheck solution-d61c998c
  (fn rotate [r s]
    (let [n (count s) p (mod r n)]
      (take n (drop p (concat s s)))
      )
    ))

(defcheck solution-d6428092
  (fn [n s]
    (let [l (mod n (count s))]
      (concat (drop l s) (take l s)))))

(defcheck solution-d66085dd
  (fn [n coll]
    (let [idx (mod n (count coll))]
      (concat (drop idx coll) (take idx coll)))))

(defcheck solution-d6acbf7f
  (fn [n xs]
    (let [r (mod n (count (vec xs)))]
      (concat (drop r xs) (take r xs)))))

(defcheck solution-d7034442
  #(let [c (count %2) r (mod (+ c (mod % c)) c)] (concat (drop r %2) (take r %2))))

(defcheck solution-d7311af7
  ;;(fn rotv
  ;;([coll]
  ;;(let [coll (into [] coll)]
  ;;      (cons (peek coll) (pop coll))))
  ;;  ([n coll]
  ;;    (nth (iterate rotv coll) (mod (* -1 n) (count coll)))))

  (fn [m coll]
    (let [n (mod m (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-d7fb8021
  (fn rot-seq [n s]
    (let [
          s2 (if (neg? n) (reverse s) s)
          n2 (if (neg? n) (* -1 n) n)
          r  (rem n2 (count s2))]
      (loop [result (vec s2), counter r]
        (if (zero? counter)
          (if (neg? n)
            (reverse result)
            result)
          (recur (conj (vec (rest result)) (first result)) (dec counter))
          )))))

(defcheck solution-d82f23c4
  (fn f [s l]
    (let [n (count l) x (mod (+ n s) n)]
      (concat (drop x l) (take x l))
      )
    ))

(defcheck solution-d97f3fd8
  (fn rotate
    [i seq]
    (flatten
      (let [k (- i (* (count seq) (quot i (count seq))))]
        (if (> k 0)
          (cons (take-last (- (count seq) k) seq) (take k seq))
          (let [k (* -1 k)]
            (cons (take-last k seq) (take (- (count seq) k) seq))
            )
          )
        )
      )
    ))

(defcheck solution-da05d0b2
  (fn [rotation coll]
    (if (empty? coll)
      coll
      (let [slice    (fn [coll start end]
                       (map #(nth coll %)
                         (range start end)))
            length   (count coll)
            rotate   (rem rotation length)
            split-at (if (neg? rotate)
                       (+ length rotate)
                       rotate)
            left     (slice coll 0 split-at)
            right    (slice coll split-at length)]
        (concat right left)))))

(defcheck solution-da264aa2
  (fn f [n seq] (cond (< n 0) (f (+ n (count seq)) seq)
                      (= n 0) seq
                      (> n (count seq)) (f (- n (count seq)) seq)
                      :else (f (dec n)
                              (concat (rest seq)
                                      (list (first seq)))))))

(defcheck solution-da8df826
  (fn [n s]
    (flatten
      (reverse
        (split-at (mod n (count s)) s)))))

(defcheck solution-dae046f9
  (fn [n s] (let [c (mod n (count s))] (concat (nthrest s c) (take c s)))))

(defcheck solution-dc1f2a75
  #(let [d (mod % (count %2))] (concat (drop d %2) (take d %2))))

(defcheck solution-dc2181de
  (fn [n l]
    (let [neg (neg? n)
          n'  (if neg (- n) n)
          f   (if neg
                #(cons (last %) (butlast %))
                #(concat (rest %) (list (first %))))]
      (nth (iterate f l) n'))))

(defcheck solution-dc5a32c3
  #(concat (drop (mod % (count %2)) %2) (take (mod % (count %2)) %2)))

(defcheck solution-dc6e0573
  #(take (count %2)
     (drop (mod % (count %2))
       (concat %2 %2))))

(defcheck solution-dca1a9bc
  #(take (count %2) (drop (+ (count %2) (count %2) %) (flatten (repeat 5 %2)))))

(defcheck solution-dd20b3d3
  (fn [r col]
    (let [rr (mod r (count col))]
      (concat (drop rr col) (take rr col)))))

(defcheck solution-dd36fc2e
  ;(comp (partial apply concat) (juxt drop take))
  (fn rotate-by [n coll]
    (let [n (mod n (count coll))]
      (concat (drop n coll) (take n coll)))))

(defcheck solution-dd613d49
  (fn [n s]
    (let [r (mod n (count s))]
      (concat (drop r s) (take r s)))))

(defcheck solution-dd706fc4
  (fn [p c]
    (let [p (if (< p 0) (+ p (count c) (count c)) p)]
      (take (count c) (drop p (cycle c))))))

(defcheck solution-de17c157
  (fn f [xs ls] (loop [x xs acc ls]
                  (cond (zero? x) acc
                        (> x 0) (recur (dec x) (concat (rest acc) (list (first acc))))
                        :else (recur (inc x) (concat (list (last acc)) (drop-last acc)))))))

(defcheck solution-de57f3f5
  (fn [n coll]
    (let [n1     (if (neg? n)
                   (+ (count coll) n)
                   n)
          nfinal (mod n1 (count coll))]
      (concat (drop nfinal coll)
              (take nfinal coll)))))

(defcheck solution-de713c7b
  (fn rot [n s]
    (let [r (mod n (count s))]
      (concat (drop r s) (take r s)))))

(defcheck solution-de7b4037
  (fn [i s]
    (let [i (mod i (count s))]
      (concat (drop i s) (take i s)))))

(defcheck solution-deff7f7a
  (fn [c s]
    (let [m (mod c (count s))]
      (concat (drop m s) (take m s)))))

(defcheck solution-dfc91719
  (fn my-rorate [n coll]
    (let [r              (rem n (count coll))
          n-drop-or-take (if (pos? r) r (+ (count coll) r))]
      (flatten (list (drop n-drop-or-take coll)
                 (take n-drop-or-take coll))))))

(defcheck solution-dfd1a94b
  (fn [n coll]
    (if-let [s (seq coll)]
      (let [len (count s)
            pos (if (neg? n) (+ len (rem n len)) (rem n len))]
        (concat (nthrest s pos) (drop-last (- len pos) s)))
      )))

(defcheck solution-dffd78d5
  (fn [ind seq]
    (let [num (count seq) split (mod ind num) new_ind_seq (concat (range split num) (range 0 split))]
      (for [new_indx new_ind_seq] (nth seq new_indx))
      )
    ))

(defcheck solution-e04f1fb2
  #(let [s (count %2)
         c (mod (+ s %1) s)]
     (concat (drop c %2) (take c %2))))

(defcheck solution-e059ca28
  (fn [x, y] (flatten (reverse (split-at (mod x (count y)) y)))))

(defcheck solution-e09f9c8f
  #(let [x (mod %1 (count %2))] (drop x (concat %2 (take x %2)))))

(defcheck solution-e19e1f5d
  #(reverse (loop [c (reverse %2)
                   n %1]
              (cond
                (zero? n) c
                (> n 0) (recur (conj (butlast c) (last c)) (dec n))
                (< n 0) (recur (reverse (conj (butlast (reverse c)) (first c))) (inc n))))))

(defcheck solution-e1dbc5fd
  #(let [z (mod % (count %2))]
     (concat (drop z %2) (take z %2))))

(defcheck solution-e1f3d188
  (fn [x y]
    (let [cnt (mod x (count y))]
      (concat
       (drop cnt y)
       (take cnt y)))))

(defcheck solution-e29b4f26
  #(-> (mod % (count %2)) (split-at %2) reverse flatten))

(defcheck solution-e2aa49d2
  #(take (count %2) (drop (mod %1 (count %2)) (cycle %2))))

(defcheck solution-e2da646
  (fn [n l]
    (let [cnt (count l)]
      (take cnt
        (drop (mod n cnt)
          (cycle l))))))

(defcheck solution-e335ada4
  #(let [[a b] (split-at (mod % (count %2)) %2)]
     (concat b a)))

(defcheck solution-e3b1b110
  #(let [n (mod % (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-e4096cc1
  (fn [i coll]
    (let [j (mod i (count coll))]
      (concat (drop j coll) (take j coll)))))

(defcheck solution-e417135
  (fn [n s]
    (let [x (mod n (count s))]
      (concat (drop x s) (take x s)))))

(defcheck solution-e439b1ff
  (fn [n coll]
    (let [cnt (count coll)]
      (apply #(concat %2 %1) (split-at (mod n cnt) coll))
      )
    ))

(defcheck solution-e5030008
  (fn rotate [n coll]
    (take (count coll) (drop (mod n (count coll)) (cycle coll)))))

(defcheck solution-e591cbfe
  (fn rotate [n lst]
    (let [l (count lst)
          n (mod n l)]
      (take l (drop n (concat lst lst))))))

(defcheck solution-e5a2ebf8
  (fn rotate-seq
    [n coll]
    (let [times (mod n (count coll))]
      (->> coll
        (split-at times)
        (map reverse)
        flatten
        reverse
        ))))

(defcheck solution-e5aaa92c
  (fn r [n v]
    (let [c (count v)
          n (mod n c)]
      (concat (drop n v) (take n v)))))

(defcheck solution-e68cb499
  ;(fn rotate
  ;  [n xs]
  ;  (let [size (count xs)
  ;        idx (mod n size)]
  ;    (->> xs
  ;         (split-at idx)
  ;         (reverse)
  ;         (apply concat))))

  #(let [n (mod % (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-e69c3b8d
  (fn rot [n s]
    (let [forever (cycle s)
          slen    (count s)
          skip    (mod n slen)]
      (take slen (drop skip forever)))))

(defcheck solution-e6dd6be7
  (fn rotate [n seq]
    (cond (zero? n)
          seq
          (neg? n)
          (recur (inc n) (concat [(last seq)] (butlast seq)))
          (pos? n)
          (recur (dec n) (concat (rest seq) [(first seq)])))))

(defcheck solution-e6e0ad3a
  #(let [x (mod % (count %2))]
     (concat (drop x %2) (take x %2))))

(defcheck solution-e6f49580
  (fn rotate [n xs]
    (let [n (mod n (count xs))]
      (concat (drop n xs) (take n xs)))))

(defcheck solution-e732931a
  (fn revr [a b]

    (if (zero? a)

      b

      (if (pos? a)

        (revr (dec a) (concat (drop 1 b) (take 1 b)))

        (revr (inc a) (concat (drop (dec (count b)) b) (take (dec (count b)) b)))))))

(defcheck solution-e769bbbb
  (fn [n col] (let [s (mod n (count col))]
                (concat (drop s col) (take s col)))))

(defcheck solution-e7e4aa65
  #(let [len (count %2)
         n   (mod %1 len)]
     (concat (drop n %2) (take n %2))))

(defcheck solution-e803eb2
  #(if (< %1 0)
     (concat (take-last (mod (* -1 %1) (count %2)) %2) (drop-last (mod (* -1 %1) (count %2)) %2))
     (concat (drop (mod %1 (count %2)) %2) (take (mod %1 (count %2)) %2))))

(defcheck solution-e8166c05
  (fn [n s]
    (let [n (mod n (count s)) s (vec s)]
      (concat
       (subvec s n)
       (subvec s 0 n)))))

(defcheck solution-e83587d
  (fn [a b]
    (let [n (count b)]
      (if (pos? a) (concat (drop (mod a n) b) (take (mod a n) b))
                   (concat (drop (+ (mod (* a -1) n) 1) b) (take (- n (mod (* a -1) n)) b))
                   )
      )
    ))

(defcheck solution-e83d4d5b
  (fn [n xs]
    (let [idx (mod n (count xs))
          [left right] (split-at idx xs)]
      (concat right left))))

(defcheck solution-e840662
  (fn [n xs]
    (let [size (count xs)
          n'   (if (pos? n) (rem n size) (+ size (rem n size)))]
      (concat (drop n' xs) (take n' xs)))))

(defcheck solution-e8630c80
  (fn [x xs]
    (take
      (count xs)
      (drop (mod (+ (count xs) x) (count xs)) (cycle xs)))))

(defcheck solution-e89d091
  (fn [n l]
    (loop [n n
           l (vec l)]
      (if (= n 0)
        l
        (recur
          (if (neg? n)
            (inc n)
            (dec n))
          (if (neg? n)
            (cons (last l) (take (- (count l) 1) l))
            (concat (rest l) [(first l)])))))))

(defcheck solution-e8d6a93a
  (fn [offset s]
    (if (and (>= offset 0) (< offset (count s)))
      (concat (drop offset s) (take offset s))
      (recur (mod offset (count s)) s))))

(defcheck solution-e99ddc1d
  (fn rotate [n coll] (cond
                        (< n 0) (rotate (inc n) (butlast (cons (last coll) coll)))
                        (> n 0) (rotate (dec n) (rest (conj (vec coll) (first coll))))
                        :else coll
                        )
    ))

(defcheck solution-ea2b2307
  (fn rotate
    [index coll] (cond
                   (= index 0) coll
                   (> index 0) (rotate (dec index) (concat (rest coll) (list (first coll))))
                   (< index 0) (rotate (inc index) (concat (list (last coll)) (reverse (rest (reverse coll))))))))

(defcheck solution-eae75700
  #(let [n (mod % (count %2))]
     (concat (drop n %2)
             (take n %2)
             )))

(defcheck solution-eb729e59
  (fn [n coll]
    (let [cnt (count coll)]
      (flatten (reverse (split-at (if (< n 0)
                                    (+ (rem n cnt) cnt)
                                    (rem n cnt))
                          coll))))))

(defcheck solution-eb7d4cc7
  (fn [i coll]
    (let [c (concat coll coll)
          s (count coll)
          i (mod i s)]
      (if (> i 0)
        (take s (drop i c))
        (take s (drop (+ i s) c))))))

(defcheck solution-ec27bb30
  (fn [rotate coll]
    (let [to-take (count coll)
          to-drop (if (pos? rotate)
                    rotate
                    (+ to-take (- rotate) 1))]
      (take to-take (drop to-drop (flatten (repeat coll)))))))

(defcheck solution-ec3c1fe6
  #(let [n (mod % (count %2))]
     (concat (drop n %2) (take n %2))))

(defcheck solution-ec6ef711
  (fn [c s]
    (flatten ((juxt drop take) (mod c (count s)) s))))

(defcheck solution-ec911067
  (fn [n c] (let [i (mod n (count c))] (concat (drop i c) (take i c)))))

(defcheck solution-eca49939
  (fn rec-rot [n xs]
    (cond
      (= n 0) xs
      (> n 0) (recur (dec n) (concat (rest xs) (list (first xs))))
      (< n 0) (recur (inc n) (concat (list (last xs)) (butlast xs))))))

(defcheck solution-edc9d590
  (fn [n s]
    (loop [c (cycle s)
           i n
           l (count s)]
      (if (= (mod i l) 0)
        (take l c)
        (recur (rest c) (dec i) l)))))

(defcheck solution-ede632c8
  (fn rotate [nn seqq]
    (let [n (mod nn (count seqq))]
      (apply concat ((juxt drop take) n seqq)))
    ))

(defcheck solution-eeb5b54
  #(for [n [(count %2)] i (range n)]
     (nth %2 (mod (+ i %) n))))

(defcheck solution-eef335a7
  (fn
    [d s]
    (take (count s) (drop (mod d (count s)) (cycle s)))))

(defcheck solution-ef471ba2
  (fn rtt
    [drct l]
    (loop [i 0 out (into [] (repeat (count l) 0))]
      (if (= i (count l))
        out
        (recur (inc i)
          (assoc out (mod (- i drct) (count l)) (nth l i))
          )
        )
      )
    ))

(defcheck solution-efaded6b
  (fn [o s]
    (let [size    (count s)
          indexed (map list (map #(mod (- % o) size) (range)) s)]
      (map second (sort-by first indexed)))))

(defcheck solution-efd30d3d
  (fn rotate
    [amount seq]
    (if (neg? amount)
      (reverse (rotate (- amount) (reverse seq)))
      (concat (drop (mod amount (count seq)) seq) (take (mod amount (count seq)) seq)))))

(defcheck solution-efe6e784
  (fn [n s] (map #(nth s (mod (+ % n) (count s))) (range (count s)))))

(defcheck solution-f05bd44e
  (fn func
    [index input-seq]
    (let [size       (count input-seq)
          true-index (mod index size)]
      (->> input-seq (split-at true-index) reverse (reduce concat)))))

(defcheck solution-f05cbcae
  (fn R [n l]
    (cond (zero? n) l
          (pos? n) (recur (dec n) (concat (rest l) (list (first l))))
          (neg? n) (recur (inc n) (cons (last l) (butlast l))))))

(defcheck solution-f064bb7a
  (fn [n l]
    (let [m (mod n (count l))]
      (concat (drop m l) (take m l)))))

(defcheck solution-f17ebd42
  #(let [size  (count %2)
         steps (rem %1 size)]
     (cond
       (= steps 0) %2
       (> steps 0) (concat (drop steps %2) (take steps %2))
       :else (concat (drop (+ size steps) %2) (take (+ size steps) %2)))))

(defcheck solution-f18938ac
  (fn [n seq]
    (cond
      (= n 0) seq
      (> n 0) (take (count seq) (drop n (flatten (repeat seq))))
      (< n 0) (reverse (take (count seq)
                         (drop (- n) (flatten (repeat (reverse seq)))))))))

(defcheck solution-f20c6e59
  #(let [[f l] (split-at (mod % (count %2)) %2)]
     (concat l f)))

(defcheck solution-f26a3521
  (fn r [n s]
    (cond
      (< n 0) (r (+ (count s) n) s)
      (= n 0) s
      :else (r (dec n) (concat (rest s) [(first s)])))))

(defcheck solution-f2d0b7da
  (fn [n v]
    (cond
      (> n 0) (nth (iterate #(drop 1 (conj (vec %) (first %))) v) n)
      (< n 0) (nth (iterate #(drop-last (cons (last %) %)) v) (- n))
      :else v
      )))

(defcheck solution-f2dc7955
  (fn rot [n s]
    (let [abs-n (Math/abs n) mod-n (mod abs-n (count s))]
      (if (>= n 0)
        (concat (drop mod-n s) (take mod-n s))
        (concat (take-last mod-n s) (drop-last mod-n s))
        )
      )

    ))

(defcheck solution-f2e5e11e
  (fn [pos s]
    (map (fn [i] (nth s (mod (+ pos i) (count s))))
      (range (count s)))))

(defcheck solution-f3569682
  (fn [n coll]
    (let [part (split-at (mod n (count coll)) coll)]
      (concat (second part) (first part)))))

(defcheck solution-f368e4ba
  (fn f [num l]
    (cond
      (> num (count l)) (f (- num (count l)) l)
      (< num 0) (f (+ num (count l)) l)
      :else (concat (drop num l) (take num l)))))

(defcheck solution-f37fc498
  #(let [r (mod % (count %2))]
     (concat (drop r %2) (take r %2))))

(defcheck solution-f3842079
  (fn [x coll]
    (let [n (rem x (count coll))]
      (flatten
        (if (pos? n)
          (cons (drop n coll) (take n coll))
          (cons (drop (+ (count coll) n) coll)
            (take (+ (count coll) n)
              coll)))))))

(defcheck solution-f3ef4782
  (fn rotate [n coll]
    (if (= n 0)
      coll
      (when-let [s (seq coll)]
        (if (> n 0)
          (rotate (dec n) (concat (rest s) [(first s)]))
          (rotate (inc n) (conj (drop-last s) (last s)))
          )
        )
      )
    ))

(defcheck solution-f41c5302
  (fn [n _seq]
    (let [splitted (split-at (mod n (count _seq)) _seq)]
      (concat (second splitted) (first splitted)))))

(defcheck solution-f44c58c6
  (fn [n s]
    (let [c (count s)
          m (rem (+ c (rem n c)) c)]
      (concat (drop m s) (take m s)))))

(defcheck solution-f463df4
  (fn [n s]
    (let [sz (count s)
          i  (mod n sz)]
      (take sz (drop i (cycle s))))))

(defcheck solution-f47eb656
  (fn [n x] (let [m (mod n (count x))] (concat (drop m x) (take m x)))))

(defcheck solution-f490fc3b
  (fn [n coll]
    (let [idx (mod n (count coll))]
      (concat (drop idx coll) (take idx coll)))))

(defcheck solution-f5f0973f
  #(let [r (mod % (count %2))] (concat (drop r %2) (take r %2))))

(defcheck solution-f6464d91
  #(let [i (mod % (count %2))]
     (concat (drop i %2) (take i %2))))

(defcheck solution-f692051e
  (fn [r lat]
    (let [tR (+ (* 2 (count lat)) r)]
      (take (count lat) (nthnext (cycle lat) tR))
      )
    ))

(defcheck solution-f6ba28d8
  (fn rot [n lst]
    (let [l (seq lst)]
      (if (neg? n)
        (rot (+ (count l) n) l)
        (let [n1 (mod n (count l))] (concat (drop n1 l) (take n1 l)))
        )
      )
    ))

(defcheck solution-f6be669d
  (fn [n coll]
    (let [n (first (drop-while #(< % 0) (iterate #(+ % (count coll)) n)))]
      (take (count coll) (drop n (cycle coll))))))

(defcheck solution-f711abd9
  (fn [n S]
    (for [x (range (count S))]
      (nth S (mod (+ x n) (count S)))
      )
    ))

(defcheck solution-f71e1b51
  #(let [c (count %2)]
     (take c (drop (mod % c) (cycle %2)))))

(defcheck solution-f72613ae
  (fn circ
    [n lista]
    (if (>= n 0)
      (concat (drop (mod n (count lista)) lista) (take (mod n (count lista)) lista))
      (circ (+ n (count lista)) lista))))

(defcheck solution-f7371dc4
  (fn [a l]
    (let [n (mod a (count l))]
      (concat (drop n l) (take n l)))))

(defcheck solution-f73f26ab
  #(apply concat (reverse (split-at (mod %1 (count %2)) %2))))

(defcheck solution-f75d0215
  (fn [n s]
    (let [len (count s)
          n   (mod n len)]
      (->> (cycle s)
        (drop n)
        (take len)))))

(defcheck solution-f7b14aa
  (fn [steps coll]
    (apply concat (reverse (split-at (mod steps (count coll)) coll)))))

(defcheck solution-f7df699
  (fn rot [i coll]
    (let [c (count coll) r (mod i c)]
      (take c (drop r (concat coll coll))))))

(defcheck solution-f7dff2b3
  #(let [c (count %2) d (mod % c)] (take c (drop d (cycle %2)))))

(defcheck solution-f801688c
  (fn [n coll]
    (let [x (mod n (count coll))]
      (concat (nthrest coll x) (take x coll))
      )
    ))

(defcheck solution-f82f8cfa
  #(flatten
     (reverse
       (split-at
         (let [go (rem %1 (count %2))]
           (if (neg? go) (+ go (count %2)) go))
         %2))))

(defcheck solution-f89dfc0
  #(loop [n %1 v %2]
     (cond
       (> n 0) (recur (dec n) (concat (rest v) (list (first v))))
       (< n 0) (recur (inc n) (cons (last v) (butlast v)))
       :else v)))

(defcheck solution-f913673c
  (fn [n coll]
    (let [len       (count coll)
          split-idx (if (< n 0)
                      (+ len (rem n len))
                      (rem n len))]
      (lazy-cat (nthnext coll split-idx)
        (take split-idx coll)))))

(defcheck solution-f931592a
  (fn [n xs]
    (let [l (count xs)]
      (take l
        (drop
          (mod n l)
          (concat xs xs))))))

(defcheck solution-f94ede3e
  (fn rotate
    [n s]
    (let [delta (mod n (count s))
          mvmt  (if (pos? delta) delta (+ delta (count s)))]
      (concat (drop mvmt s) (take mvmt s)))))

(defcheck solution-f974cd33
  (fn [delta v] (map (apply vector v) (map #(mod (+ delta %) (count v)) (range (count v))))))

(defcheck solution-f97e8798
  (fn [i coll]
    (if (>= i 0)
      (take (count coll) (drop i (cycle coll)))
      (let [limit (first (filter #(> % (Math/abs i))
                           (map #(* (count coll) %)
                             (rest (range)))))]
        (take (count coll) (drop (- limit (Math/abs i)) (cycle coll)))))))

(defcheck solution-f999122c
  (fn [n c]
    (if (< n 0)
      (let [m (* n -1)] (nth (iterate #(cons (last %) (vec (butlast %))) c) m))
      (nth (iterate #(conj (vec (rest %)) (first %)) c) n))))

(defcheck solution-f9a9b140
  (fn [n coll]
    (->>
      (split-at (mod n (count coll)) coll)
      (apply #(concat %2 %1)))))

(defcheck solution-fa92efd5
  (fn rotate [rotate-by s]
    (let [count-s (count s)]
      (->>
        (cycle s)
        (drop (mod rotate-by count-s))
        (take count-s)))))

(defcheck solution-fac155f6
  (fn [n coll]
    (let [my-coll (cycle coll)
          my-drop (mod n (count coll))]
      (take (count coll) (drop my-drop my-coll)))
    ))

(defcheck solution-fae2d5f3
  (fn [r s] (let [n     (mod r (count s))
                  right (take n s)
                  left  (drop n s)]
              (concat left right))))

(defcheck solution-fb8a304a
  (fn [at nums]
    (if (> at 0)
      (apply concat (reverse (split-at (rem at (count nums)) nums)))
      (apply concat (reverse (split-at (+ (count nums) (rem at (count nums))) nums))))))

(defcheck solution-fbf1c58a
  #(let [i (mod %1 (count %2))]
     (concat (drop i %2) (take i %2))))

(defcheck solution-fc63bcd8
  (fn [n l]
    (flatten (reverse (split-at (mod n (count l)) l)))))

(defcheck solution-fc67d262
  (fn [n s]
    (let [l (count s)
          m (mod (+ l (mod n l)) l)]
      (concat (drop m s) (take m s)))))

(defcheck solution-fc98f54
  (fn [n coll]
    (let [len (count coll)]
      (if (< n 0)
        (recur (+ len n) coll)
        (if (> n len)
          (recur (- n len) coll)
          (concat (drop n coll) (drop-last (- len n) coll)))))))

(defcheck solution-fd6a44fb
  (fn [n seq]
    (let [n (mod n (count seq))]
      (concat (drop n seq) (take n seq)))))

(defcheck solution-fd7478ae
  (fn [n coll]
    (let [size (count coll)
          m    (if (zero? size) 0 (mod n size))
          p    (if (< m 0) (+ m size) m)]
      (concat (drop p coll) (take p coll)))))

(defcheck solution-fdf5ce42
  (fn rotate [n xs]
    (let [length (count xs)
          n*     (mod n length)]
      (concat (drop n* xs) (take n* xs)))))

(defcheck solution-fe5f7c76
  (fn rot [n [x & xs :as lst]]
    (cond
      (zero? n) lst
      (< n 0) (rot (+ (count lst) n) lst)
      :else (rot (dec n) (concat xs [x])))))

(defcheck solution-ff4ae29b
  #(let [c (mod %1 (count %2))] (concat (drop c %2) (take c %2))))

(defcheck solution-ff9226c8
  (fn rotate [n s]
    (
     #(concat (drop % s) (take % s))
     (mod n (count s))
     )
    ))

(defcheck solution-ffe33449
  (fn [n s]
    (let [i (mod n (count s))
          [h t] (split-at i s)]
      (concat t h))))
