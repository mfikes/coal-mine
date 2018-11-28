(ns coal-mine.problem-100
  (:require [coal-mine.checks :refer [defcheck-100] :rename {defcheck-100 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1070a1a6
  (fn lcm [& xs]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (/
        (reduce * xs)
        (reduce gcd xs)))))

(defcheck solution-11530ab4
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-118eaad8
  (fn lcm
    [& args]
    (let* [gcd2 (fn [a b]
                  (cond (> b a) (recur b a)
                        (= b 0) a
                        :else   (recur b (rem a b))))
           gcd  (fn [& args]
                  (reduce gcd2 args))]
      (/ (apply * args) (apply gcd args)))))

(defcheck solution-119276dd
  #(letfn [ (g [a b] (if (= b 0) a (g b (mod a b))))
           (k [a b] (/ (* a b) (g a b)))]
     (reduce k %&)))

(defcheck solution-119b1f3
  #(loop [l (map (partial repeat 2) %&)]
     (if (apply = (map first l)) (ffirst l)
                                 (let [ls (sort-by first l) [a b] (first ls)]
                                   (recur (cons [(+ a b) b] (rest ls)))))))

(defcheck solution-11b622a5
  (fn lcm [& c]
    (if (> (count c) 2)
      (lcm (first c) (apply lcm (rest c)))
      (/
        (* (nth c 0) (nth c 1))
        ((fn gcd [[y,x]] (let [r (rem x y)] (if (> r 0) (gcd [r y]) y) ) ) (sort [(nth c 0) (nth c 1)]))
        )
      )))

(defcheck solution-11d304cc
  (fn lcm
    ([a b]
     (let [gcd (fn gcd [a b]
                 (if (zero? b)
                   a
                   (gcd b (mod a b))))]
       (/ (* a b) (gcd a b))))
    ([a b & rest] (reduce lcm (lcm a b) rest))))

(defcheck solution-11dc9eb3
  (fn [x & v]
    (first
      (filter(fn[y](every? #(= 0(mod y %))v))(map #(* x (inc %))(range))))))

(defcheck solution-122c7438
  (fn lcm-chain [& items]
    (letfn [(gcd [a b]
              (loop [a a
                     b b]
                (cond
                  (zero? b) a
                  (>= a b) (recur (- a b) b)
                  :else (recur b a))))
            (lcm [a b]
              (* a (/ b (gcd a b))))]
      (reduce lcm items))))

(defcheck solution-122d5a84
  (fn [& xs]
    (letfn [(euclid [a b & ys]
              (if (seq ys)
                (euclid a (apply euclid b ys))
                (cond
                  (= a b) a
                  (> a b) (euclid (- a b) b)
                  (< a b) (euclid a (- b a)))))]
      (/ (apply * xs)
        (apply euclid xs)))))

(defcheck solution-123b4421
  (fn lcm [a & bs]
    (if (= (count bs) 0) a
                         (let [b (first bs)]
                           (loop [x a y b]
                             (cond
                               (= x y) (apply lcm x (rest bs))
                               (< x y) (recur (+ x a) y)
                               :else (recur x (+ y b))))))))

(defcheck solution-124b1bd7
  (fn [& nums]
    (letfn [ (multiple-of-all? [n factors]
               (every? #(zero? (rem n %)) factors))]
      (let [ns (sort > nums)
            mx (first ns)]
        (first (filter #(multiple-of-all? % ns) (iterate #(+ mx %) mx)))))))

(defcheck solution-126b52dc
  (fn [& xs]
    (reduce #(/ (* % %2)
               (loop [x % y %2]
                 (if (zero? x) y (recur (mod y x) x))))
      (first xs)
      (rest xs))))

(defcheck solution-1299941b
  (fn [a & args]
    (first
      (drop-while
        (fn [x]
          (some
            (complement zero?)
            (map (partial mod x) args)))
        (map (partial * a) (rest (range)))))))

(defcheck solution-135712f0
  (fn [x y & n]
    (letfn [(g [a b]
              (if (or (= a b) (= b 0))
                a
                (recur b (mod a b))))]
      (/ (* x y (apply * n))
        (reduce g (g x y) n)))))

(defcheck solution-136620a5
  (fn [f & r]
    (letfn [(g [a b] (if (= 0 b) a (g b (mod a b))))]
      (reduce #(/ (* % %2) (g % %2)) f r))))

(defcheck solution-1369a6c6
  #(letfn [(gdc [a r]
             (cond
               (< a r) (recur r a)
               (not= 0 (mod a r)) (recur r (mod a r))
               :ese r))]
     (/ (apply * %&) (reduce gdc %&))))

(defcheck solution-13d3c866
  (fn [& x]
    (reduce
      #(->
         %
         (* %2)
         (/ ((fn g [a b] (if (= 0 b) a (recur b (mod a b)))) % %2)))
      x)
    ))

(defcheck solution-142f4d66
  (fn [& l]
    (reduce
      (fn [a b]
        (let [a   (if (> a 0) a (- a))
              b   (if (> b 0) b (- b))
              gcd #(cond (= %1 %2) %1
                         (> %1 %2) (recur (- %1 %2) %2)
                         :else     (recur (- %2 %1) %1))
              m (* a b)]
          (/ m (gcd a b))))
      l)))

(defcheck solution-14551aeb
  (fn  prob100 [& args]
    (reduce
      (fn [x y]
        (letfn [(gcd [a b]
                  (if (zero? b)
                    a
                    (recur b (mod a b))))
                ]
          (/ (* x y) (gcd x y)))
        )
      args)))

(defcheck solution-145e4a1d
  #(/ (reduce * %&)
     (loop [xs %&]
       (if (= (count xs) 1)
         (first xs)
         (recur (conj (drop 2 xs) (loop [A (first xs), B (second xs)]
                                    (if (= (rem A B) 0)
                                      B
                                      (recur B (rem A B))))))))))

(defcheck solution-14711996
  (fn [m & ms]
    (second
      (reduce (fn [xs x] (filter #(= 0 (rem % x)) xs))
        (map * (repeat m) (range))
        ms
        ))
    ))

(defcheck solution-14a3b2f2
  (fn [& xs]
    (let [x (apply min xs)]
      (first
        (filter #(apply = (concat [0] (map (partial rem %) xs)))
          (iterate (partial + x) x))))))

(defcheck solution-152e3c33
  (fn [& d]
    (letfn [(gcm [a b]
              (cond
                (< a b) (gcm a (- b a))
                (> a b) (gcm (- a b) b)
                :else a))
            (lcm [a b]
              (/ (* a b) (gcm a b)))]
      (reduce lcm d))))

(defcheck solution-15e72120
  (fn[& xs]
    (loop [is (map #(iterate (partial + %) %) xs)]
      (if (apply = (map first is))
        (ffirst is)
        (let [mi (ffirst (sort-by (comp min second) (map-indexed #(list % (first %2)) is)))]
          (recur (map-indexed #(if (= % mi) (rest %2) %2) is)))))))

(defcheck solution-1604ceb8
  (fn [a & others]  (first (filter (fn fil[x] (every? #(zero? (rem x %)) others)) (iterate #(+ a %) a)))))

(defcheck solution-16b2bc6f
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m) b (recur b m))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-16fb9ddb
  (fn [& nums]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (/ (reduce * nums) (reduce gcd nums)))))

(defcheck solution-16ff09ef
  (fn lcm [& ns]
    ((fn lcm-inner [dict]
       (if (apply = (map first dict))
         (do
           (first (first dict)))
         (let [m (apply max (map first dict))]
           (lcm-inner
             (map (fn [[ k v]]
                    (if (= k m)
                      [k v]
                      [(v k) v]))
               dict)))))
     (zipmap ns (map #(fn [n] (+ n %)) ns)))))

(defcheck solution-172936d4
  (fn [& more] (reduce #(/ (* %1 %2) ((fn gcd [a b] (if (= 0 b) a (gcd b (mod a b)))) %1 %2)) more)))

(defcheck solution-17b48a35
  (fn lcm [a & [b :as xs]]
    (if-not (seq xs)
      a
      (apply lcm (/ (* a b) ((fn f [a b]
                               (cond
                                 (= a b) a
                                 (> a b) (f (- a b) b)
                                 (< a b) (f (- b a) a)))
                             a b))
        (next xs)))))

(defcheck solution-18bcfe4f
  (fn [& nums]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b)) ) )]
      (reduce (fn [a b] (/ (* a b) (gcd a b))) nums))))

(defcheck solution-18ee3052
  ; unapologetic brute force
  (fn lcm [& nums]
    (first
      (filter
        (fn [n] (every? #(= (mod n %) 0) nums))
        (map
          (partial * (apply max nums))
          (iterate inc 1))))))

(defcheck solution-19452f00
  (fn rec
    ([x y]
     (let [gcd
               (fn[a b]
                 (cond
                   (< a b)
                   (recur a (- b a))

                   (< b a)
                   (recur (- a b) b)

                   :else
                   a))

           lcm (fn[a b]
                 (/ (* a b) (gcd a b)))]

       (lcm x y)))
    ([x y & more]
     (reduce rec (rec x y) more))))

(defcheck solution-19500fd6
  (fn [& args] (first (filter #(every? (partial = 0) (map (fn [x] (mod % x)) args))
                        (rest (map #(* (apply max args) %) (range)))))))

(defcheck solution-1994631e
  #(reduce (fn [x y] (let [gcd (fn [a b] (if (= b 0) a (recur b (mod a b))))] (/ (* x y) (gcd x y)))) %&))

(defcheck solution-19df9651
  (fn lcm [& nums]
    (first (filter (fn [cand] (every? #(= (mod cand %) 0) nums)) (map #(* (apply min nums) (+ % 1)) (range))))))

(defcheck solution-1a9c7584
  (fn lcm [a & nums]
    (letfn [(gcd [x y]
              (let [dividend (max x y)
                    divisor (min x y)
                    remainder (rem dividend divisor)]
                (if (zero? remainder)
                  divisor
                  (recur divisor remainder))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) a nums))))

(defcheck solution-1b7febe0
  (fn [ & n]
    (first (filter (fn [m] (every? zero? (map #(rem m %) n)))  (map #(+ (apply min n) (* (apply min n) %)) (range  ))))
    ))

(defcheck solution-1b982c4b
  (fn [& v] (first
              (for [i (rest (range)) :let [x (* i (apply max v))]
                    :when (every? zero? (map #(mod x %) v))] x))))

(defcheck solution-1ba59c99
  (fn [& c]
    (let [c (vec c)]
      (loop [v c]
        (let [m (apply min v)
              i (ffirst (filter #(= (second %) m)
                          (map-indexed (fn [i o] [i o]) v)))]
          (if (every? #(= m %) v)
            m
            (recur (assoc v i (+ m (c i))))))))))

(defcheck solution-1bc649ed
  (fn [f & r]
    (let [ mul (reduce * f r)
          gcd (reduce #(if (zero? %2) %1 (recur %2 (mod %1 %2))) f r)]
      (/ mul gcd))))

(defcheck solution-1bcf6aed
  (fn [& x]
    (reduce
      (fn lcm [x y]
        (/
          (* x y)
          ((fn gcd [x y]
             (loop [a x b y]
               (if (> b a)
                 (recur b a)
                 (if (= (mod a b) 0)
                   b
                   (recur b (mod a b))))))
           x y)
          ))
      x)))

(defcheck solution-1c32d9cb
  (fn [& numbers] (loop [s (map vector numbers numbers)]
                    (let [b (map #(nth % 1) s) m (apply min b)]
                      (if (= 1 (count (distinct b))) (first b)
                                                     (recur (map (fn [x] (if (= m (nth x 1)) [(first x) (+ (first x) (nth x 1))] x)) s))
                                                     )
                      ))))

(defcheck solution-1c366cd0
  (fn lcm [& nums]
    (let [multiples (fn [n] (iterate #(+ n %) n))
          lcm'      (fn [mults]
                      (let [smallest (apply min (map first mults))]
                        (if (apply = (map first mults))
                          (first (first mults))
                          (recur (map #(if (= smallest (first %)) (rest %) %) mults)))))]
      (lcm' (map multiples nums)))))

(defcheck solution-1c3811e0
  (fn [G & n] (reduce #(/ (* % %2) (G % %2)) n)) (fn g [a b] (if (= b 0) a (g b (rem a b)))))

(defcheck solution-1d1c69dd
  (fn [& ns]
    (letfn [(lcm [ns]
              (condp = (count ns)
                1 (first ns)
                2 (let [max-n (apply max ns)
                        min-n (apply min ns)]
                    (first (filter (fn [v] (zero? (rem v min-n)))
                             (map #(* % max-n) (rest (range))))))
                (lcm [(first ns) (lcm (rest ns))])))]
      (lcm ns))))

(defcheck solution-1e0cb188
  (fn lcmm [& args]
    (reduce
      (fn lcm
        [x y]
        (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
          (/ (* x y) (gcd x y)))) args)))

(defcheck solution-1e9ee153
  (fn [& a]
    (letfn [(gcd [a b] (if (= 0 b) a (recur b (mod a b))))]
      (/ (reduce * a)
        (reduce gcd a)))))

(defcheck solution-1eb67b8f
  (fn my-lcm [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)]
                (if (zero? b) a (recur b (- a b)))))
            (lcm [x y] (/ (* x y) (gcd x y)))]
      (reduce lcm args))))

(defcheck solution-1ed1f5e6
  (fn [x y & more]
    (let [gcd (fn [x y]
                (let [[bigger smaller] (reverse (sort [x y]))
                      remainder (mod bigger smaller)]
                  (if (zero? remainder)
                    smaller
                    (recur smaller remainder))))
          lcm (/ (* x y) (gcd x y))]
      (if (empty? more)
        lcm
        (recur lcm (first more) (rest more))))))

(defcheck solution-1f28bf73
  (fn [& nums]
    ((fn a [pairs] (let [sorted (sort-by first pairs)]
                     (if (=
                           (first (first sorted))
                           (first (last sorted)))
                       (first (first sorted))
                       (a
                         (cons
                           (list (apply + (first sorted)) (last (first sorted)))
                           (rest sorted))))))
     (map #(list % %) nums))))

(defcheck solution-1f5ae9e3
  (fn [& f]
    (let [ x #(if (< % 0) (- %) %)
          g (fn [a b]
              (if (= b 0) a
                          (recur b (mod a b))))
          l #(/ (x (* %1 %2)) (g %1 %2))]
      (reduce l f))))

(defcheck solution-1f975773
  (fn [& args]
    ((fn f [l ol]
       (let [m (apply min l)
             nl (map (fn [[ni n]] (if (> n m) n (+ n ni)))
                  (partition 2 (interleave ol l)))]
         (if (apply = nl)
           (first nl)
           (f nl ol))))
     args args)))

(defcheck solution-1fc1311e
  (fn [n & r]
    (first (drop-while #(not-every? (fn [x] (= 0 (rem % x)))
                          r)
             (iterate #(+ n %) n)))))

(defcheck solution-20c706c4
  (fn [& xs*]
    (loop [xs xs*]
      (if (apply = xs)
        (first xs)
        (let [m (apply min xs)]
          (recur (map #(if (= % m) (+ % %2) %)
                   xs xs*)))))))

(defcheck solution-20df85d
  (fn lcm [& nums]
    (loop [curnums nums]
      (if (every? #(= % true) (map #(= % (first curnums)) curnums))
        (first curnums)
        (let [minnum (apply min curnums)
              numnums (count curnums)
              mindex (.indexOf curnums minnum)
              toadd (nth nums mindex)
              #_#__ (println minnum numnums mindex toadd)
              newnums (assoc (vec curnums) mindex (+ minnum toadd))
              #_#__ (println newnums)]
          (recur newnums))))))

(defcheck solution-2140fe0f
  (fn tmp [x & lst]
    (if (empty? lst) x
                     (recur
                       (first
                         (drop-while
                           #(not= (mod %1 (first lst)) 0)
                           (iterate (partial + x) x)))
                       (rest lst)))))

(defcheck solution-2154add3
  (fn least-common-multiple
    ([n1 n2]
     (letfn [(gcd [n1 n2]
               (if (zero? n2)
                 n1
                 (recur n2 (rem n1 n2))))]
       (/ (* n1 n2) (gcd n1 n2))))
    ([n1 n2 & more]
     (reduce least-common-multiple (least-common-multiple n1 n2) more))))

(defcheck solution-21d6bcc3
  (fn [& l]
    (reduce
      (fn [a b]
        (/ (* a b)
          (loop [a a b b]
            (if (zero? b)
              a
              (recur b (mod a b))
              )

            )
          )
        )
      l
      )
    ))

(defcheck solution-229642c5
  (fn f [x & y]
    (letfn [(g [x y add]
              (if (every? #(zero? (rem x %)) y)
                x
                (g (+ x add) y add)))]
      (g x y x))))

(defcheck solution-22af6df9
  (fn[f & r] (first (filter (fn[m] (every? #(= 0 (mod m %)) r)) (iterate (partial + f) f)))))

(defcheck solution-23c0e3ae
  (fn common
    ([x] x)
    ([x y]
     (first (filter #(integer? (/ % x))
              (map #(* y %)
                (drop 1 (range))))))
    ([x y & more]
     (reduce common (common x y) more))))

(defcheck solution-241aea27
  (fn [ & l]
    (loop [n (apply max l) s n]
      (if (every? #(= (mod n %) 0) l)
        n
        (recur (+ n s) s)))))

(defcheck solution-2439ff9e
  (fn [& args]
    (reduce (fn [a b]
              (let [gcd (fn gcd [a b]
                          (cond
                            (> b a) (gcd b a)
                            (zero? b) a
                            :else (recur b (- a b))))]
                (/ (* a b) (gcd a b)))) args)))

(defcheck solution-246cba65
  (letfn [
          (gcd [a b] (cond (> a b) (recur b a) (zero? a) b true (recur (mod b a) a)))
          (lcm [a b] (/ (* a b) (gcd a b)))
          ] (fn [& args] (reduce lcm args))))

(defcheck solution-2476679f
  (fn [& args]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-24d6dad9
  (fn [& args]
    (letfn [(gcd [a b]
              (loop [a a b b]
                (if-not (zero? b)
                  (recur b (rem a b))
                  a)))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-24da1bbe
  (fn [& v]
    (apply (fn f
             ([[a b x y]]
              (cond
                (= (* a x) (* b y)) (* a x)
                (< (* a x) (* b y)) (f [a b (+ x 1) y])
                :else (f [a b x (+ y 1)])))
             ([a b] (f [a b 1 1]))
             ([a b & c]
              (apply f (cons (f [a b 1 1]) c)))) v)))

(defcheck solution-25490703
  (fn [& s] (reduce
              (fn [a b]
                (loop [[p :as x] (iterate #(+ a %) a)
                       [q :as y] (iterate #(+ b %) b)]
                  (cond (= p q) p
                        (< p q) (recur (rest x) y)
                        1 (recur x (rest y)))))
              s)))

(defcheck solution-258f7cfc
  (fn [& args]
    (letfn [(gcd [a b]
              (if (== b 0)
                a
                (gcd b (mod a b))))
            (lcd [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcd args))))

(defcheck solution-25b133fc
  (fn lcm [& xs]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (rem a b))))]
      (reduce (fn [a b] (/ (* a b) (gcd a b))) xs))))

(defcheck solution-25b9b1b0
  (fn [e & r]
    ((fn f [p]
       (if (every? #(= (mod p %) 0) r)
         p
         (f (+ p e)))) e)))

(defcheck solution-25be561e
  (fn p100 [& ns]
    (letfn [(gcd ([a b] (ffirst (drop-while (fn [[a b]] (not= 0 b))
                                  (iterate (fn [[a b]] [b (mod a b)]) [a b]))))
              ([a b & more]
               (reduce gcd (gcd a b) more)))
            (lcd ([a b] (/ (* a b) (gcd a b)))
              ([a b & more] (reduce lcd (lcd a b) more)))]
      (apply lcd ns))))

(defcheck solution-25d72eaf
  (fn lcm [& rst]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))]
      (reduce #(/ (* % %2) (gcd %1 %2)) rst))))

(defcheck solution-26076b90
  (fn lcm [& nums]
    (letfn [(gcd [x y] (if (= (* x y) 0) (+ x y) (gcd (- (max x y) (min x y)) (min x y))))]
      (if (= (count nums) 2) (/ (* (first nums) (second nums)) (gcd (first nums) (second nums)))
                             (lcm (first nums) (apply lcm (rest nums)))))))

(defcheck solution-263963f7
  (fn [& args]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-264f40e0
  (let [gcd (fn [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
        lcm (fn [a b]
              (/ (* a b) (gcd a b)))]
    (fn lcms [& c]
      (if (= (count c) 1)
        (first c)
        (lcm (first c) (apply lcms (rest c))
          )))))

(defcheck solution-266435b2
  (fn lcm
    ([a b]
     (letfn [(gcd [a b]
               (cond (= a b) a
                     (> a b)(gcd (- a b) b)
                     (< a b)(gcd a (- b a))))]
       (/ (* a b) (gcd a b))))
    ([a b & rest] (reduce lcm (lcm a b) rest))))

(defcheck solution-26cb0a65
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (= b 0) a
                          (recur b (rem a b))))]
      (reduce #(/ (* % %2) (gcd % %2)) xs))))

(defcheck solution-26d525b2
  (fn [& c] (let [gcd (fn [a b]
                        (loop [v1 (max a b) v2 (min a b)] (if(zero? v2) v1 (recur v2 (mod v1 v2)))))
                  seq-gcd (fn [& c] (if (>= 1 (count c))  (first c)
                                                          (loop [cv (drop 1 c) r (first c)] (if(empty? cv) r (recur (drop 1 cv) (gcd r (first cv))) ))
                                                          )) ]
              (/ (reduce * c) (apply seq-gcd c))
              )))

(defcheck solution-27c9506
  (fn lcm [& args]
    (let [step (fn step [c incs]
                 (let [m (apply max c)
                       m? #(< % m)]
                   (if (apply = m c) c
                                     (recur
                                       (map #(if (m? %1) (+ %1 %2) %1) c incs) incs))))]
      (first (step args args)))))

(defcheck solution-27d87ab0
  (fn lcm [& x]
    (letfn
     [
      (gcd [a b] (if (= 0 b) a (gcd b (mod a b))))
      ]
      (if (= (count x) 1)
        (first x)
        (let [c (gcd (first x) (second x))]
          (apply
            lcm
            (cons (/ (* (first x) (second x)) c) (drop 2 x))
            )
          )
        )
      )
    ))

(defcheck solution-2814c231
  (fn [& args]
    (letfn [(gcd [a b]
              (if (= 0 b)
                a
                (gcd b (rem a b))
                )
              )
            (lcm [a b] (/ (* a b) (gcd a b)))
            ]
      (reduce lcm args)
      )
    ))

(defcheck solution-2827ca0e
  (fn lcm [hd & tl]
    (letfn [(abs [x]
              (if (< x 0) (- x) x))
            (gcf [x y]
              (let [[less more] (if (< x y) [x y] [y x])
                    diff (abs (- x y))]
                (if (= x y)
                  x
                  (gcf less diff))))
            (lcm2 [x y]
              (let [factor (gcf x y)
                    x (/ x factor)
                    y (/ y factor)]
                (* x y factor)))]
      (if (empty? tl)
        hd
        (lcm2 hd (apply lcm tl))))))

(defcheck solution-284388b4
  (fn [& v]
    (reduce
      #(/ (* %1 %2)
         (loop [f %1 t %2]
           (if (zero? (rem f t))
             t
             (recur t (rem f t)))))
      v)))

(defcheck solution-285ea9bb
  (fn [& c]
    (first
      (filter
        (fn [x]
          (apply = 0 (map
                       #(mod x %)
                       c)))
        (next (map #(* (apply min c) %) (range)))))))

(defcheck solution-28873cd0
  (fn [& ns]
    (loop [a (map #(iterate (partial + %) %) ns)]
      (let [f (apply min-key first a) l (apply max-key first a)]
        (if (= f l)
          (first f)
          (recur (map #(if (= (first f) (first %)) (rest %) %) a)))))))

(defcheck solution-28bf6504
  (fn lcm [x  & y]
    (if(empty? y )
      x
      (let [  gcd (fn [a b]
                    (let[ c (mod a b)]
                      (if(zero? c)
                        b
                        (recur b c))))]
        (apply lcm
          (/ (* x (first y))
            (gcd x (first y)))
          (rest y))))))

(defcheck solution-28d7abf8
  (fn lcm [& nums]
    (loop [c (mapv #(vector % 1) nums)]
      (if (apply = (map #(apply * %) c))
        (apply * (first c))
        (recur (update-in c [(apply min-key #(apply * (c %)) (range (count c))) 1] inc))
        ))))

(defcheck solution-291ddbd6
  (fn [x & xs]
    (let [ys (iterate #(+ x %) x)]
      (first (drop-while #(< 0 (apply + (map (fn [y] (rem % y)) xs))) ys)))))

(defcheck solution-29296f61
  (fn lcm
    ([x y]
     (let [gcd (loop [a x
                      b y]
                 (if (zero? b)
                   a
                   (recur b (rem a b))))]
       (/ (* x y)
         gcd)))
    ([x y & xs]
     (apply lcm (lcm x y) xs))))

(defcheck solution-293c0e7a
  (letfn [(gcd2 [a b] (cond (< a b) (gcd2 b a)
                            (zero? (mod a b)) b
                            :otherwise (gcd2 (- a b) b)))
          (gcd [as] (reduce gcd2 as))]
    (fn [& ns] (/ (apply * ns) (gcd ns)))))

(defcheck solution-2957edeb
  (fn __ [& coll]
    (reduce #(/ (* % %2) ((fn [a b]
                            (if (zero? b)
                              a
                              (recur b, (mod a b))))
                          % %2)) coll)))

(defcheck solution-2a3de7b4
  (fn [x & xs]
    (let [multiples (map (partial * x) (rest (range)))]
      (first
        (filter #(every?
                   (fn [y] (zero? (mod % y)))
                   xs)
          multiples)))))

(defcheck solution-2a655e07
  (fn [& ns]
    (let [vs (vec ns)
          idx-min #(first (apply min-key second (map-indexed vector %)))]
      (loop [s vs]
        (if (apply == s)
          (first s)
          (let [m (idx-min s)]
            (recur (assoc s m (+ (s m) (vs m))))))))))

(defcheck solution-2ad31eb8
  (comp
   #(if (apply = (vals %)) (val (first %)) (recur (let [v (apply min-key % (keys %))] (conj % [v (+ (% v) v)]))))
   #(zipmap %& %&)))

(defcheck solution-2ad46c45
  (fn [& args]
    (letfn
     [(gcd [a b]
        (if (zero? b) a (recur b (mod a b))))
      (lcm [a b]
        (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-2aededf7
  (fn [& l]
    (reduce (fn lcm [x y]
              (first
                (filter
                  #(= (mod % y) 0)
                  (iterate (partial + x) x)
                  )
                )
              ) (first l) l)))

(defcheck solution-2b327e0f
  (fn [& xs]
    (letfn [(gcd[x y] (if (zero? x) y (recur (mod y x) x)))
            (lcm[x y] (/ (* x y) (gcd x y)))]
      (reduce lcm xs))))

(defcheck solution-2b41728a
  (fn my-lcm [& nums]
    (loop [v nums]
      (if (apply = v)
        (first v)
        (let [minimum (apply min v)]
          (recur (map #(if (= %1 minimum) (+ %1 %2) %1) v nums)))))))

(defcheck solution-2b442ea4
  (fn lcm [& nums]
    (let [
          gcd (fn f [a b]
                (let [x (max a b)
                      y (min a b)]
                  (if (zero? y)
                    x
                    (f y (mod x y)))))
          l (fn [a b]
              (/ (* a b) (gcd a b)))]
      (reduce #(l % %2) nums))))

(defcheck solution-2b5c1440
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))
            (lcm [a b]
              (* (/ a (gcd a b)) b))]
      (reduce lcm xs))))

(defcheck solution-2b8a43a7
  (fn lcm [& xs]
    (loop [ys (vec xs)]
      (if (apply == ys)
        (first ys)
        (let [min-index (first (apply min-key second (map-indexed vector ys)))]
          (recur (update-in ys [min-index] + (nth xs min-index))))))))

(defcheck solution-2bcc3cab
  (fn least-common-multiple [& x]
    (let [mn (apply min x)]
      (loop [i mn]
        (if (apply = (conj (map #(rem i %) x) 0))
          i
          (recur (+ i mn)))))))

(defcheck solution-2bdccfbb
  (fn [& x] (first (filter (fn [y] (every? #(= 0 (mod y %)) x)) (map #(* (apply min x) %) (range 1 400))))))

(defcheck solution-2ca0dca2
  (fn lcm
    ([x y]
     (if (> x y)
       (lcm y x)
       (first (filter #(zero? (rem % x)) (iterate #(+ % y) y)))))
    ([x y & zs]
     (apply lcm (lcm x y) zs))))

(defcheck solution-2d50c080
  (fn [& c]
    (letfn [(gcd [a b]
              (let [x (max a b)
                    y (min a b)]
                (if (= 0 y)
                  x
                  (gcd b (rem a b)))))]
      (/
        (apply * c)
        (reduce gcd c)))))

(defcheck solution-2d749973
  (fn lcm
    [& args]
    (let [m (apply max args)]
      (loop [n 1]
        (if (apply = (cons 0 (map #(rem (* n m) %) args)))
          (* n m)
          (recur (inc n)))))))

(defcheck solution-2d90af5d
  (fn [& c]
    (reduce #(/ (* % %2)
               (loop [a %
                      b %2]
                 (if (= a b)
                   a
                   (if (> a b)
                     (recur (- a b) b)
                     (recur a (- b a))))))
      c)))

(defcheck solution-2dcb630c
  (fn kpk [& coll] (letfn [(fpb
                             [a b]
                             (if (zero? b)
                               a
                               (fpb b (mod a b))))]
                     (reduce #(/ (* %1 %2) (fpb %1 %2)) coll))))

(defcheck solution-2de220ca
  (fn [& nums]
    (let [
          gcd
          (fn gcd [p q]
            (let [t (rem p q)]
              (if (= 0 t)
                q
                (gcd q t)
                )
              )
            )]
      (/ (apply * nums) (reduce gcd nums))
      )
    ))

(defcheck solution-2e2dfba6
  (fn s[& coll]
    (second(sort(apply clojure.set/intersection(map (fn[x](set(take 1000 (map #(* x %) (range))))) coll)
                  )))))

(defcheck solution-2e82965c
  (fn [& s]
    (let [a (apply max s)]
      (first
        (drop-while
          #(not-every?
             (fn [x]
               (= 0 (rem % x)))
             s)
          (iterate (fn [x] (+ a x)) a))))))

(defcheck solution-2eebf264
  (fn [& args]
    (let [aux (fn [x y]
                (loop [a x b y]
                  (cond (= a b) a
                        (< a b) (recur (+ a x) b)
                        (> a b) (recur a (+ b y)))))]
      (reduce aux args))))

(defcheck solution-2f0ab44
  (fn [& xs] (/ (apply * xs) (reduce (fn f [x y] (let [[a b] (sort [x y])] (if (= a 0) b (f a (- b a))))) xs))))

(defcheck solution-2f70852b
  (fn [& h] (reduce #(* % %2 (/ ((fn f [x y] (if (= x 0) y (f (mod y x) x))) % %2))) h)))

(defcheck solution-2fa319db
  (fn lcm [& z]
    (first
      (filter
        (fn [n] (= (reduce + (map #(rem n %) (vec z))) 0))
        (iterate (fn [n] (+ (apply min z) n)) (apply min z))))))

(defcheck solution-30815a02
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))]
    (fn [& ns]
      (/ (apply * ns) (reduce gcd ns)))))

(defcheck solution-30a90412
  (fn lcm [& coll]
    (loop [s (first coll) r (rest coll)]
      (if (seq r)
        (let [a (first r)
              am (iterate #(+ % a) a)
              snew (first (filter #(= (mod % s) 0) am))]
          (recur snew (rest r)))
        s))))

(defcheck solution-30aa689a
  (fn [a & body]
    (reduce #(/ (* %1 %2) ((fn gcd [a b]
                             (if (= b 0)
                               a
                               (gcd b (rem a b)))) %1 %2)) a body)))

(defcheck solution-30da5502
  (fn [& xs]
    (loop [xs' xs]
      (if (apply == xs')
        (first xs')
        (let [m (apply max xs')]
          (recur (map (fn [a b] (if (< a m) (+ a b) a)) xs' xs)))))))

(defcheck solution-313fbc99
  (fn lcm [& nums]
    (letfn [(gcd2 [a b]
              (cond (> a b) (gcd2 (- a b) b)
                    (< a b) (gcd2 (- b a) a)
                    :t a))
            (lcm2 [a b] (/ (* a b) (gcd2 a b)))]
      (reduce lcm2 nums))))

(defcheck solution-31b5751e
  (fn [& args]
    (letfn [(gcd [x y]
              (let [r (rem x y)]
                (if (= r 0)
                  y
                  (gcd y r))))
            (lcm [x y]
              (/ (* x y) (gcd x y)))]
      (reduce lcm args))))

(defcheck solution-321a7cbe
  (fn [& xs]
    (loop [seqs (map (fn [x] (iterate #(+ x %) x)) xs)]
      (if (apply == (map first seqs))
        (ffirst seqs)
        (let [[s & ss] (sort-by first seqs)]
          (recur (cons (rest s) ss)))))))

(defcheck solution-32867058
  (fn lcm---- [& args]
    (letfn [(gcd [x y]
              (if (zero? y) x (recur y (mod x y))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-32eae484
  (fn [ & x]
    (let [ nums (vec x)
          update-nums (fn [n]
                        (let [i (first (apply min-key second (map-indexed vector n)))]
                          (update-in n [i] + (nums i))))]
      (ffirst (drop-while #(apply not= %)(iterate update-nums nums))))))

(defcheck solution-33534a17
  (fn [& nums]
    (let [multiples (map #(map (partial * %) (drop 1 (range))) nums)]
      (loop [m multiples]
        (if (apply = (map first m))
          (ffirst m)
          (let [firsts (map first m)
                threshold (nth (sort (distinct firsts)) 1)]
            (recur (map #(drop-while (partial > threshold) %) m))))))))

(defcheck solution-336f65a0
  (fn [& r]
    (let [lcm (fn [n m]
                (/ (* n m) (#(if (zero? %2) %1 (recur %2 (mod %1 %2))) n m)))]
      (reduce
        #(lcm %1 %2)
        r))))

(defcheck solution-35021375
  (fn lcm [& nums]
    (let [biggest (apply max nums)]
      (first
        (filter (fn [n] (every? zero? (map #(rem n %) nums)))
          (range biggest 999 biggest))))))

(defcheck solution-35303f42
  (fn [& xs] (reduce (fn [a b] (/ (* a b) ((fn [x y] (if (zero? y) x (recur y (mod x y)))) a b))) xs)))

(defcheck solution-3617302c
  (fn [& s]
    (letfn [(hcf [p q]
              (loop [x p y q]
                (if (zero? y)
                  x
                  (recur y (rem x y)))))
            (lcm [p q]
              (/ (* p q) (hcf p q)))
            ]
      (reduce lcm (first s) s))))

(defcheck solution-36793cfe
  (fn [& args]
    (let [gcd (fn [a b]
                (if (< a b) (recur b a)
                            (if (= b 0) a
                                        (recur b (- a b)))))
          lcd (fn [a b]
                (let [g (gcd a b)]
                  (* g (/ a g) (/ b g))))]
      (reduce lcd args))))

(defcheck solution-368498b1
  (fn lcm [x1 & args]
    (loop [n x1]
      (if (every? zero? (map #(mod n %) args) )
        n
        (recur (+ x1 n))))))

(defcheck solution-369749e7
  #(letfn [(gcd
             ([x y] (if (= y 0) x (recur y (mod x y))))
             ([x y & more] (reduce gcd (gcd x y) more)))]
     (/ (reduce * %&) (apply gcd %&))))

(defcheck solution-36f64ae6
  (fn [& seq]
    (let [gcd (fn gcd [a b] (cond (= a b) a
                                  (< a b) (gcd a (- b a))
                                  (> a b) (gcd (- a b) b)))
          abs (fn [x] (if (< x 0) (- x) x))
          lcm (fn [a b] (/ (abs (* a b)) (gcd a b)))]
      (reduce lcm seq))))

(defcheck solution-375ce8af
  (fn [f & n]
    (* f (first
           (filter
             (fn [i]
               (every?
                 #(let [v (/ (* f i) %)]
                    (= (int v) v)
                    ) n))
             (map inc (range)))))))

(defcheck solution-378d66cc
  (fn [& x]
    (let [x (vec x) n (count x)]
      (loop [y x]
        (if (apply = y)
          (first y)
          (let [i (apply min-key y (range n))]
            (recur (assoc y i (+ (y i) (x i))))))))))

(defcheck solution-378f7159
  (fn [& args]
    (let [n (first args)
          rm (rest args)]
      (loop [x n]
        (if (some #(not (zero? (mod x %))) rm)
          (recur (+ x n))
          x)))))

(defcheck solution-37a4693d
  (fn [& lst]
    (reduce
      #(/
         (* % %2)
         ((fn [a b] (if (zero? b) a (recur b (mod a b)))) % %2)
         )
      lst)))

(defcheck solution-37f8a64b
  (fn
    [head & numbers]
    (let [multiples (iterate (partial + head) head)]
      (first (filter (fn [number] (zero? (reduce + (map #(mod number %) numbers)))) multiples))
      )
    ))

(defcheck solution-3821581a
  (fn lcm ([a b]
           (let [gcd #(let [[x y] (sort > [%1 %2])]
                        (if (= y 0) x (recur y (mod x y))))]
             (/ (* a b) (gcd a b))
             )) ([a b & more] (apply lcm (lcm a b) more))))

(defcheck solution-3826754c
  (fn lcm [x1 x2 & xs]
    (let [omap (vec (conj xs x2 x1)) iks (range 0 (count omap))]
      (loop [tm omap]
        (let [mk (apply min-key tm iks) mv (tm mk) incr (omap mk)]
          (if (apply = tm)
            (tm 0)
            (recur (assoc tm mk (+ mv incr)))))))))

(defcheck solution-3838ef78
  (fn [& xs]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (/ (apply * xs)
        (reduce gcd xs)))))

(defcheck solution-38674006
  (fn lcm [& nums]
    (let [[a & others] (reverse (sort nums))]
      (loop [candidate a]
        (if (every? #(= 0 (mod candidate %)) others)
          candidate
          (recur (+ candidate a)))))))

(defcheck solution-392da4d0
  (fn [& args]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))
          lcm (fn [a b]
                (/ (* a b) (gcd a b)))]
      (loop [a (first args)
             b (second args)
             v (drop 2 args)]
        (if (empty? v)
          (lcm a b)
          (recur (lcm a b) (first v) (rest v)))))))

(defcheck solution-3943fd2
  (fn [& m]
    (letfn [(gcd [a b]
              (if (zero? a) b
                            (recur (rem b a) a)))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm m))))

(defcheck solution-3a3592b
  (fn [& x]
    (reduce (fn [a b]
              (/ (* a b)
                (loop [a a b b]
                  (if (= a b) a
                              (if (< a b) (recur a (- b a))
                                          (recur b (- a b)))))))x)))

(defcheck solution-3a55e899
  (fn [& args]
    (reduce
      (fn [acc x]
        (first (filter (fn [x] (= 0 (rem x acc)))
                 (map (partial * x)
                   (iterate inc 1)))))
      args)))

(defcheck solution-3a8573fd
  (fn [& nums]
    (let [gcd (fn [a b]
                (loop [mx (max a b) mn (min a b)]
                  (let [rem (mod mx mn)]
                    (if (zero? rem)
                      mn
                      (recur mn rem)))))
          lcm (fn [a b]
                (/ (* a b) (gcd a b)))]
      (reduce lcm nums))))

(defcheck solution-3b2df2aa
  (fn [x & s]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (reduce #(/ (* % %2) (gcd % %2)) x s))))

(defcheck solution-3b4076ab
  (fn lcm [& args]
    (letfn [(gcd [a b]
              (loop [i a j b]
                (if (= b 0) a (gcd b (mod a b)))))]
      (/ (reduce * 1 args)
        (reduce gcd 1 args)))))

(defcheck solution-3b698496
  (fn [ & l]
    (loop [multiples (map (fn [x] (iterate (partial + x) x)) l)]
      (let [maxmin (apply max (map first multiples))]
        (if (apply = (map first multiples))
          maxmin
          (recur (map (partial filter (partial <= maxmin)) multiples)))))))

(defcheck solution-3b7e0e90
  (fn [& x]
    (let [gcd (fn g [x y]
                (if (zero? y)
                  x
                  (g y (rem x y))))]
      (/ (apply * x) (reduce gcd x)))))

(defcheck solution-3c171e6
  (fn lcm [& more]
    (reduce
      (fn lcm_2 [a b]
        (/ (* a b) ((fn gcd [a b]
                      (if (= a b)
                        a
                        (if (> a b)
                          (gcd (- a b) b)
                          (gcd a (- b a))
                          )
                        )
                      ) a b) )
        )
      more)
    ))

(defcheck solution-3cc150fa
  (fn lcm [& ns]
    (let [maxn    (apply max ns)
          multiples (map #(* % maxn) (range))
          testlcm (fn [l] (and (> l 0) (every? #(zero? (mod l %)) ns)))]
      (first (filter testlcm multiples)))))

(defcheck solution-3d91acfd
  (fn [h & t]
    ((fn f [p]
       (if (every? #(= (mod p %) 0) t)
         p
         (f (+ p h))))
     h)))

(defcheck solution-3d9b4e4e
  (fn [& s] (first (filter (fn [x] (= 0 (reduce + (for [i (seq s)] (rem x i))))) (iterate (partial + (reduce min s)) (reduce min s))))))

(defcheck solution-3dacaad5
  (fn mylcm [x & r] (letfn [(mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb))))]
                      (if (empty? r) x (apply mylcm (conj  (rest r) (/ (* x (first r)) (mygcd x (first r))))) ))))

(defcheck solution-3ded34f2
  (fn lcm
    [& args]
    (loop [accum (into [] (map (fn [x] [x x]) args))]
      (if (apply == (map first accum))
        (first (first accum))
        (recur (let [[h & t] (sort accum)]
                 (conj t [(+ (first h) (second h)) (second h)])))))))

(defcheck solution-3eb1012e
  (let [gcd (fn [a b]
              (if (= 0 b)
                a
                (recur b (mod a b))))]
    (fn lcm
      ([x y]
       (/ (* x y) (gcd x y))
       )
      ([x y & more]
       (apply lcm (conj more (lcm x y)))
       )
      )
    ))

(defcheck solution-3ee9d1dd
  (fn [r & rs]
    (some (fn [candidate]
            (when (every? #(zero? (mod candidate %)) rs)
              candidate))
      (reductions + (repeat r)))))

(defcheck solution-3f153d66
  (fn [x & more]
    (if (empty? more)
      x
      (first
        (for [i (map #(* x %) (map inc (range)))
              :when (every? #(zero? (mod i %)) more)]
          i)))))

(defcheck solution-3f336d80
  (fn[x & y]
    (apply
      min
      (into []
        (apply
          clojure.set/intersection
          (into []
            (for [i  (conj y x)]
              (set (map (partial * i) (next (range 400))))
              )
            )
          )))))

(defcheck solution-3fc77782
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m)
                  b
                  (recur b m))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args)
      )))

(defcheck solution-40091e1
  (fn [ & nombres]
    (reduce
      (fn [x y]
        (
         (fn lcm [a na b nb]
           (if (= (* a na) (* b nb))
             (* a na)
             (if (> (* a na) (* b nb))
               (lcm a na b (+ nb 1))
               (lcm a (+ 1 na) b nb)
               )
             )
           )
         x 1 y 1)
        )
      nombres
      )
    ))

(defcheck solution-40112a14
  (fn [& args] (reduce (fn lcm [a b]
                         (let [c (* a b)
                               f (fn gcd [a b]
                                   (let [[x y] (if (> a b) [a b] [b a]) z (mod x y)]
                                     (if (= 0 z)
                                       b
                                       (recur y z))))]
                           (/ c (f a b)))) args)))

(defcheck solution-40685d81
  (fn [& xs] (loop [ms (map #(map * (repeat %) (iterate inc 1)) xs)]
               (let [vs (map first ms) m (apply max vs)]
                 (if (apply = vs) m (recur (map (partial drop-while #(< % m)) ms)))))))

(defcheck solution-40fd95f4
  (fn [ & args]
    (letfn [(div [x y]
              (if (= 0 (rem x y))
                y
                (div y (rem x y))
                )
              )
            (comm [x y]
              (* x (/ y (div x y)))
              )]

      (reduce comm args)


      )
    ))

(defcheck solution-4105e758
  (fn [& ns] (reduce #(/ (* %1 %2) ((fn [x y] (loop[x' (max x y) y' (min x y)] (let [rem-num (mod x' y')] (if (zero? rem-num) y' (recur y' rem-num))))) %1 %2)) ns)))

(defcheck solution-41177b51
  (fn lcm
    ([x] x)
    ([x y]
     (letfn [(cmSeq [v b]
               (lazy-seq
                 (cons v (cmSeq (+ v b) b))))]
       (loop [xSeq (cmSeq x x)
              ySeq (cmSeq y y)]
         (cond
           (= (first xSeq) (first ySeq)) (first ySeq)
           (< (first xSeq) (first ySeq)) (recur (rest xSeq) ySeq)
           :else (recur xSeq (rest ySeq))))))
    ([x y & z]
     (apply (partial lcm (lcm x y)) z) )))

(defcheck solution-4157df3d
  (fn [x & xs]
    (letfn [(gcd [a b]
              (if (= 0 b) a (gcd b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm (cons x xs)))))

(defcheck solution-417bad6e
  (fn lcm [& xs]
    (let [gcd (fn [a b] (cond (= a 0) b (= b 0) a :else (recur (mod b a) a)))
          lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-419cb2ae
  (fn lcm [& a] (/ (reduce * a)
                  (reduce (fn gcd [a b]
                            (if (= b 0) a
                                        (recur b (mod a b)))) a))))

(defcheck solution-41c97f41
  (fn lcm [& s] (letfn [
                        (gcd_ [a b]  (let [x (max a b)
                                           y (min a b)
                                           m (mod x y)]
                                       (if (zero? m)
                                         y
                                         (recur y m))))
                        (lcm_ [m n] (/ (* m n) (gcd_ m n)))]
                  (reduce lcm_ s))))

(defcheck solution-420c2be1
  #(letfn
    [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))
     (lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-4235c50d
  (fn l-c-m [x y & zs]
    (first
      (filter (fn [a]
                (every? zero? (map (fn [b] (rem a b))
                                (cons y (into [] zs)))))
        (lazy-seq (map (partial * x) (rest (range))))))))

(defcheck solution-424a30fd
  (fn lcms
    ([x y] (
            (fn [a b]
              (letfn [(gcd2 [a b]
                        (if (zero? b)
                          a
                          (gcd2 b (mod a b)
                            )))]
                (* b (/ a (gcd2 a b))))) x y))
    ([x y & rest] (apply lcms (lcms x y) rest))))

(defcheck solution-4283f29c
  (fn [& l]
    (/
      (apply * l)
      (reduce
        (fn f [a b]
          (if (= 0 b)
            a
            (f b (rem a b))))
        l))))

(defcheck solution-42a1b20b
  (fn [& x]
    (reduce (fn [a b]
              (/ (* a b)
                ((fn gcd [a b] (if (zero? b) a (gcd b (rem a b)))) a b)
                )
              ) x
      )
    ))

(defcheck solution-42acb7ed
  (fn [a & r]
    (first
      (filter
        #(every? (comp zero? (partial mod %)) r)
        (map #(* a (inc %)) (range))))))

(defcheck solution-42d115be
  #(reduce (fn [a b] (loop [m b] (if (zero? (mod m a)) m (recur (+ m b))))) (sort %&)))

(defcheck solution-4315952e
  (fn lcm [& vals]
    (letfn [(all-div? [x xs] (if (every? #(= 0 (mod x %)) xs) x))]
      (let [x (first vals)
            r (rest vals)]
        (some #(all-div? (* x %) r) (range 1 100))))))

(defcheck solution-432183c7
  (fn [& coll]
    (let [mnum (apply min coll)
          othernums (filter #(not (= mnum %)) coll)
          notminmod (fn [curnum]
                      (not= 0
                        (reduce + (map #(mod curnum %) othernums))))
          ]
      (first (drop-while notminmod (map #(* mnum %) (iterate inc 1))))
      )))

(defcheck solution-435024a5
  (fn [& n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm n))))

(defcheck solution-436d9717
  (fn [h & t]
    (first
      (filter (fn [c] (every? #(zero? (mod c %)) t))
        (iterate (partial + h) h)))))

(defcheck solution-43cbc425
  (fn lcm
    ([a b]
     (letfn [(gcd [a b]
               (if (zero? b)
                 a
                 (recur b (mod a b))))]
       (/ (* a b) (gcd a b))))
    ([a b & more]
     (reduce lcm (lcm a b) more))))

(defcheck solution-44310e5a
  (fn [& l]
    (loop [m (vec l)]
      (if (apply = m)
        (first m)
        (let [i (.indexOf m (apply min m))]
          (recur (update-in m [i] + (nth l i))))))))

(defcheck solution-44dfbfd0
  (fn [& nums]
    (loop [s nums]
      #_(println nums)
      (if (= 1 (count (set s)))
        (first s)
        (let [m (apply min s)]
          (recur (map-indexed
                   #(if (= %2 m) (+ %2 (nth nums %1)) %2)
                   s)))))))

(defcheck solution-44e2fd90
  (fn prob-0100 [& xs]
    (let [mx (apply max xs)]
      (first
        (for [lcm (range 1 ##Inf)
              :let [prod (* mx lcm)]
              :when (every? zero?
                      (map #(rem prod %) xs))
              ]
          prod)))))

(defcheck solution-44efe729
  (fn [& nums]
    (let [sorted-nums (-> nums sort reverse)
          max-num (first sorted-nums) rest-nums (rest sorted-nums)
          num-series (lazy-seq (iterate (partial + max-num) max-num))
          mod-check (fn [a] (apply (partial = 0) (map #(mod a %) rest-nums)))]
      (first (filter mod-check num-series)))))

(defcheck solution-45572a16
  (fn lcm [& r]
    (letfn [(div-by-all? [n l]
              (reduce #(and %1 %2) true (map #(= 0 (mod n %)) l)))]
      (let [m (apply min r)]
        (loop [m2 m]
          (do #_(println m2 r)
              (if (div-by-all? m2 r)
                m2
                (recur (+ m m2)))))))))

(defcheck solution-45855333
  (fn [& s]
    (reduce (fn [x y]
              (if (every? zero? [x y]) 0
                                       (* y (/ x
                                              (#(if (zero? %2) % (recur %2 (mod % %2))) x y)))))
      s)))

(defcheck solution-45ded7d3
  (fn [& args]
    (letfn [(gcd [x y]
              (if (= x y)
                x
                (if (< x y)
                  (recur x (- y x))
                  (recur y (- x y)))))
            (lcm [x y]
              (/ (* x y) (gcd x y)))]
      (reduce lcm args))))

(defcheck solution-460552d
  (fn my-lcm2 [& ns]
    (letfn [(my-lcm [a b]
              (letfn [(gcd [a b]
                        (cond
                          (= b 0) a
                          :else (recur b (mod a b))))]
                (/ (* a b) (gcd a b))))]
      (reduce my-lcm ns))))

(defcheck solution-463d2a88
  (fn [& vals] (let [is-multiple? (fn [x y] (integer? (/ x y)))
                     multiple-filters (map (fn [y] (fn [x] (is-multiple? x y))) vals)]
                 (-> (filter :multiple (map (fn [x] {:val x :multiple (reduce (fn [x y] (and x y)) (map (fn [f] (f x)) multiple-filters))}) (rest (map #(* % (apply min vals)) (range)))))
                   first
                   :val))))

(defcheck solution-46617c5d
  (fn lcm
    ([a] a)
    ([a b & more]
     (letfn [(gcd
               [a b]
               (if (zero? b)
                 a
                 (recur b (rem a b))))]
       (apply lcm (/ (* a b)
                    (gcd a b)) more)))))

(defcheck solution-4672abac
  #(reduce (fn [a b]
             (loop [ta a tb b]
               (cond
                 (= ta tb) ta
                 (< ta tb) (recur (+ ta a) tb)
                 :else (recur ta (+ tb b)))))
     %&))

(defcheck solution-46816dbc
  ;(
  (fn [& nums]
    (letfn ((gcd
              [a b]
              (loop
               [re a
                qu b]
                (if
                 (= qu 0)
                  re
                  (recur
                    qu
                    (rem re qu))))))
      (/
        (reduce * nums)
        (reduce gcd nums)))))

(defcheck solution-4720e325
  (fn lcm [& xs]
    (let [mn (apply min xs)]
      (loop [n mn]
        (if (apply (partial = 0) (map #(rem n %) xs))
          n
          (recur (+ n mn)))))))

(defcheck solution-473c615
  (fn lcm
    [& args]
    (let [gcd (fn gcd [a b] (if (= b 0)
                              a
                              (gcd b (rem a b))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-47438b52
  (fn [& nums] (let [gcd (fn [a b] (if (= a b) a (recur (min a b) (- (max a b) (min a b)))))
                     lcm (fn [a b] (/ (* a b) (gcd a b)))]
                 (reduce lcm nums))))

(defcheck solution-474ba437
  #(letfn [(gcd [a b] (loop [a a b b] (if (= b 0) a (recur b (mod a b)))))]
     (/ (apply * %&) (reduce gcd %&))))

(defcheck solution-47e5884b
  (letfn
   [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))
    (lcm [a b] (/ (* a b) (gcd a b)))]
    (fn [& ns] (reduce lcm ns))))

(defcheck solution-48053836
  (fn lcm
    ([a b]
     (letfn [(gcd [x y] (if (zero? y) x (gcd y (mod x y))))]
       (* (/ a (gcd a b)) b)))
    ([a b & rest]
     (apply lcm (lcm a b) rest))))

(defcheck solution-4821feee
  (fn lcm
    [& args]
    (letfn [(drop-while-less-than [x colls]
              (map (partial drop-while #(< % x)) colls))
            (first-match [& xs]
              (if (apply = (map first xs))
                (first (first xs))
                (let [[largest-first & rest] (sort-by first > xs)
                      largest (first largest-first)]
                  (recur (list* largest-first (drop-while-less-than largest rest))))))
            (iterate-multiples [x]
              (iterate (partial + x) x))]
      (apply first-match (map iterate-multiples args)))))

(defcheck solution-4879aff5
  (fn lcm [& nums]
    (let [gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (/ (reduce * nums)
        (reduce gcd nums)))))

(defcheck solution-49d65992
  (fn lcm3
    ([x y]
     (letfn [(gcd2 [a b]
               (cond
                 (= b 0) a
                 (> a b) (gcd2 b (mod a b))
                 (> b a) (gcd2 a (mod b a))))]
       (/ (* x y) (gcd2 x y))))
    ([x y & rest] (apply lcm3 (lcm3 x y) rest))))

(defcheck solution-49e1682a
  (fn [& s]
    (* (first s) (first (filter (fn [n] (every? #(= 0 (rem (* n (first s)) %)) (rest s))) (drop 1 (range)))))))

(defcheck solution-4a6a1b43
  (fn lcm [& nums]
    (reduce (fn [x y]
              (let [gcd (loop [[y x] (sort [x y])]
                          (if (zero? y) x (recur [(mod x y) y])))
                    prod (* x y)]
                (/ prod gcd)))
      nums)))

(defcheck solution-4a816db3
  (fn lcm
    [& nums]
    (let [min-n (apply min nums)
          others (filter #(not= % min-n) nums)]
      (->> (iterate #(+ % min-n) min-n)
        (drop-while #(not (every? zero? (map (partial rem %) others))))
        first))))

(defcheck solution-4a9bf2ed
  (fn [& a]
    (loop [m a]
      (if (apply = m)
        (first m)
        (recur (map (fn [v x]
                      (if (every? (fn [i] (<= i v)) m)
                        v
                        (+ v x))) m a))))))

(defcheck solution-4aa33b30
  (fn lcm [& args]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))]
      (reduce (fn [a b] (/ (* a b) (gcd a b)))
        args))))

(defcheck solution-4b1bb7e7
  (fn mlcm [& items]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (recur y (mod x y))))
            (lcm [x y]
              (/ (* x y) (gcd x y)))]
      (if (= 2 (count items))
        (lcm (first items) (second items))
        (lcm (first items) (apply mlcm (rest items)))))))

(defcheck solution-4b6afa6
  (fn [& ns]
    (letfn [(gcd [a b]
              (if (zero? b) a (gcd b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm ns))))

(defcheck solution-4b6eb7d
  (fn lcd [& s] (letfn [(gcd [a b] (if (= a b)
                                     a (if (> a b)
                                         (gcd (- a b) b) (gcd a (- b a)))))]
                  (reduce #(/ (* %1 %2) (gcd %1 %2)) s))))

(defcheck solution-4b9eb8fd
  (fn lcm
    ([a b]
     (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
       (* (/ a (gcd a b)) b)))
    ([a b & more]
     (apply lcm (lcm a b) more))))

(defcheck solution-4c36d5de
  (fn KPK
    ([x y]
     (letfn [(fpb [a b]
               (cond
                 (= b 0) a
                 (> a b) (fpb b (mod a b))
                 (> b a) (fpb a (mod b a))))]
       (/ (* x y) (fpb x y))))
    ([x y & rest] (apply KPK (KPK x y) rest))))

(defcheck solution-4c527edf
  (fn lcm [& args]
    (letfn [(gcd [a b]
              (if (zero? b)
                a (gcd b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-4c940ef8
  (fn lcm [n & ns]
    (let [abs (fn [x] (if (< x 0) (- x) x))
          gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))
          lcm2 (fn [a b]
                 (/ (abs (* a b))
                   (gcd a b)))]
      (reduce lcm2 n ns))))

(defcheck solution-4d2ae150
  (fn f [& n]
    (loop [a (map (fn [s] (iterate #(+ s %) s)) n)]
      (let [m (apply min (map first a))
            x (map #(drop-while (fn [e] (<= e m)) %) a)
            b (map first x)]
        (if (apply = b) (first b) (recur x))))))

(defcheck solution-4d5cd042
  (fn [& args]
    (letfn
     [(gcd [x y]
        (if (zero? y) x (gcd y (mod x y))))
      (lcm [a b]
        (/ (* a b) (gcd a b)))]
      (reduce lcm args)
      )
    ))

(defcheck solution-4e3067a6
  (fn [ & coll]
    (let [gcd (fn [a b] (if (zero? a) b (recur (mod b a) a)))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) coll))))

(defcheck solution-4e99e60d
  (fn lcm [& args]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-4efab57e
  (fn [& args]
    (letfn [(gcd [a b]
              (let [a-mod-b (mod a b)]
                (if (zero? a-mod-b) b (gcd b a-mod-b))))]
      (reduce (fn lcm [a b]
                (/ (* a b) (gcd a b)))
        args))))

(defcheck solution-4f3daad8
  (fn [f & r] (first (filter #(every? (fn [x] (= 0 (rem %1 x))) r) (range f 2147483647 f)))))

(defcheck solution-4f648c7d
  (fn [& vs]
    (let [min (apply min vs)]
      (some
        #(when (every? (fn [v] (= 0 (mod % v))) vs) %)
        (range min 999 min)))))

(defcheck solution-4fa37cfd
  (fn[& x]
    (loop [m (repeat 1)]
      (let [mult (map * m x) min-val (apply min mult)]
        (if (apply = mult)
          (first mult)
          (recur (map (fn [a b] (if (= (* a b) min-val) (inc a) a)) m x))
          )))))

(defcheck solution-4fef91cf
  (fn[& u](let[z(fn[x y](loop[[a & A :as Z](iterate #(+ x %)x)[b & B :as L](iterate #(+ y %)y)](cond(< a b)(recur A L)(> a b)(recur Z B)1 a)))](reduce z u))))

(defcheck solution-501a5f59
  (fn [ & r ]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (rem a b))))
            (lcm ([a b] (/ (* a b)  (gcd a b)))
              ([a b & m] (apply lcm (lcm a b) m)))]
      (apply lcm r))))

(defcheck solution-50269bf3
  (fn [x & xs] (first (filter (fn [m] (every? zero? (map #(rem m %) xs)))
                        (iterate #(+ x %) x)))))

(defcheck solution-50dde22c
  (fn [& coll]
    (reduce
      (fn lcm [a b]
        (first
          (filter #(zero? (mod % a)) (iterate #(+ b %) b))))
      coll)))

(defcheck solution-51283577
  (fn [& nums]
    (loop [tmpn (apply min nums) a (apply min nums)]
      (if (every? #(= 0 (mod tmpn %)) nums)
        tmpn
        (recur (+ a tmpn) a)))))

(defcheck solution-5173bdea
  (fn [& xs]
    (loop [[[v m] & r :as s] (sort-by second (map #(vector % %) xs))]
      (if (every? #(= (second %) m) s)
        m
        (recur (sort-by second (conj r [v (+ m v)])))))))

(defcheck solution-51838dbb
  (fn
    [& args]
    (reduce
      (fn [a b]
        (first (filter #(zero? (mod % b))
                 (iterate #(+ % a) a))))
      args
      )))

(defcheck solution-52ed6e4
  (fn [& nums]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) nums))))

(defcheck solution-534bc584
  (fn [& n]
    (letfn [
            (gcd [a b] (if (= b 0) a (recur b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm n))))

(defcheck solution-535c48ef
  (fn lcm [& x]
    (first (apply (fn lcmseq [& y]
                    (if (empty? (rest y))
                      (iterate #(+ % (first y)) (first y))
                      (filter #(= (mod % (first y)) 0) (apply lcmseq (rest y)))))
             x))))

(defcheck solution-53614da0
  (fn [v & r]
    (loop [n v]
      (if (every? #(do %) (map #(= 0 (rem n %)) r))
        n
        (recur (+ n v))))))

(defcheck solution-53a25432
  (fn lcm[& paramCol]
    (let[gcd (fn[p1 p2]
               (loop[a (max p1 p2),b (min p1 p2)]
                 (let [x (mod a b)]
                   (if (= x 0)
                     b
                     (recur b x)
                     )
                   )
                 )
               ),
         simp_lcm (fn[a b]
                    (/ (* a b) (gcd a b) )
                    )
         ]
      (reduce simp_lcm paramCol)
      )
    ))

(defcheck solution-53b3f853
  (fn [& nums]
    (loop [i (apply min nums)]
      (if (every? identity
            (map #(= 0 (mod i %))
              nums))
        i
        (recur (+ (apply min nums) i))))))

(defcheck solution-544e44ae
  (fn [& args]
    (letfn [(gcd [a b]
              (let [m (max a b) l (min a b) r (rem m l)]
                (if (zero? r)
                  l
                  (recur l r))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (let [[c d & args] args]
        (if (empty? args)
          (lcm c d)
          (recur (cons (lcm c d) args)))))))

(defcheck solution-546b5510
  (fn [a & coll]
    (first (filter (fn [v]
                     (every? integer? (map #(/ v %) coll)))
             (map #(* a %) (drop 2 (range)))))))

(defcheck solution-54da78f0
  #(reduce (fn [s a] (/ (* s a) ((fn f [x y] (if (= y 0) x (f y (mod x y)))) s a))) %&))

(defcheck solution-55265f29
  (fn [& nums]
    (first
      (reduce
        (fn [s t]
          (filter #(zero? (mod % (first t))) s))
        (map (fn [m] (iterate #(+ % m) m)) nums)))))

(defcheck solution-56004696
  (fn [& s] (/ (apply * s) (apply (fn gcd [& s] (reduce (fn [a b] (if (zero? b) a (recur b (mod a b)))) s)) s))))

(defcheck solution-564ff470
  (fn [& args]
    (some #(if (apply = %) (first %) nil)
      (iterate (fn [nums]
                 (let [min-n (apply min nums)]
                   (map-indexed
                     (fn [i n]
                       (if (= min-n n) (+ n (nth args i)) n))
                     nums))) args))))

(defcheck solution-5748327d
  (fn lcm [& args]
    (let [lcm-int (fn [args acc]
                    (let [values (map second acc)
                          [min-index min-value] (apply min-key second acc)
                          orig-min-value (args min-index)]
                      (if (every? (partial = (first values))
                            values)
                        min-value
                        (recur args
                          (assoc acc
                            min-index
                            [min-index (+ min-value orig-min-value)]))
                        )))
          indexed (vec
                    (map #(vector %1 %2)
                      (range)
                      args))]
      (lcm-int (vec args) indexed)
      )
    ))

(defcheck solution-57a219dc
  (fn mylcm [& more]
    (letfn
     [(gcd [x y] (if (= 0 (min x y))
                   (max x y)
                   (gcd (min x y) (- (max x y) (min x y)))))
      (lcm [x y] (/ (* x y) (gcd x y)))]
      (reduce lcm more))))

(defcheck solution-57bf5cb9
  (fn lcd2 [& args]
    (->>
      (map #(take 400 (iterate (partial + %) %)) args)
      (map set)
      (apply clojure.set/intersection)
      (sort)
      (first)
      )
    ))

(defcheck solution-590792ef
  (fn [& ns]
    (reduce
      (fn [x y] (first (drop-while #(pos? (rem % x)) (iterate (partial + y) y))))
      ns)))

(defcheck solution-5910a608
  (fn lcm [& args]
    (letfn [(inc-smallest [orig v]
              (let [[idx tot orig] (apply min-key second (map vector (range) v orig))]
                (assoc v idx (+ tot orig))))
            (same? [v]
              (let [h (first v)]
                (= 0 (count (filter #(not (= % h)) v)))))
            (f [origs curs]
              (if (same? curs)
                (first curs)
                (recur origs (inc-smallest origs curs))))]
      (f (vec args) (vec args)))))

(defcheck solution-5a7f93e2
  (fn !!
    ([a b]
     (/ (* a b) (
                 (fn ! [a b]
                   (if (= a b)
                     a
                     (! (- (max a b) (min a b)) (min a b))))
                 a b)))
    ([a b & t]
     (reduce !! (!! a b) t))))

(defcheck solution-5b2326c1
  (fn least-common-multiple[& args]
    (letfn [
            (lcm [v w]
              (let [a (min v w) b (max v w)]
                (loop [x a y b]
                  (if (= x y)
                    x
                    (if (> x y)
                      (recur x (+ y b))
                      (recur (+ x a) y)
                      )
                    )
                  )
                )
              )
            ]
      (reduce lcm args)
      )
    ))

(defcheck solution-5b638711
  (fn lcm [& nums]
    (letfn [(divisible? [nums m]
              (every? #(= 0 (mod m %)) nums))]
      (loop
       [incnums nums]
        ;;      (println incnums (filter #(divisible? nums %) incnums))
        (let [lcms (filter #(divisible? nums %) incnums)]
          (if-not (empty? lcms)
            (apply min lcms)
            (recur
              (map + incnums nums))))))))

(defcheck solution-5b677c7a
  (fn lcm
    ([x y]
     (letfn [(gcd [a b]
               (cond
                 (zero? b) a
                 (> a b) (gcd b (mod a b))
                 :else (gcd a (mod b a))))]
       (/ (* x y) (gcd x y))))
    ([x y & rest] (apply lcm (lcm x y) rest))))

(defcheck solution-5bc950b4
  (fn [& nums]
    (let [
          gcd (fn [x y] (cond
                          (zero? x) y
                          (zero? y) x
                          :else (recur y (mod x y))))
          lcm (fn [x y] (/ (* x y) (gcd x y)))]
      (reduce #(lcm % %2) nums))))

(defcheck solution-5ca1c778
  (fn kgv [& c]
    (let [multiples (map #(iterate (partial + %) %) c)]
      (loop [m multiples]
        (if (apply = (map first m))
          (first (first m))
          (let [[a & r] (sort-by first m)]
            (recur (conj r (next a)))))))))

(defcheck solution-5ca1cffe
  (fn lcm
    ([a b] (loop [t 2 r nil]
             (if (nil? r)
               (recur (inc t) (first (clojure.set/intersection (set (rest (take t (map #(* a %) (range))))) (set (rest (take t (map #(* b %) (range))))))))
               r)))
    ([a b & args] (reduce lcm (conj args a b)))))

(defcheck solution-5cf3c7ce
  (fn lcm
    [& nums]
    (let [gcd (fn gcd [a b]
                (cond
                  (= a 0) b
                  (= b 0) a
                  :else (let [smaller (min a b)
                              larger (max a b)
                              remainder (rem larger smaller)]
                          (gcd smaller remainder))))
          lcm-two (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm-two nums))))

(defcheck solution-5d1a140c
  (fn lcm-
    ^{:doc "Write a function which calculates the least common multiple."}
    ([x] x)
    ([x y]
     (letfn [(gcd- [a b] (if (= 0 b) a (recur b (mod a b))))]
       (/ (* x y) (gcd- x y))))
    ([x y & rest] (lcm- x (apply lcm- y rest)))))

(defcheck solution-5d483d7d
  #(reduce (fn[a b] (/ (* a b) ((fn g [a b](if (= 0 b) a (g b (mod a b)))) a b)))%&))

(defcheck solution-5d75ee0f
  (fn [& xs]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-5e1af93f
  (fn lcm [& xs]
    (let [x (vec xs)]
      (loop [c x] (
                    if (apply = c) (first c)
                                   (let [i (.indexOf c (apply min c))] (
                                                                         recur (assoc c i (+ (get c i) (get x i)))
                                                                         )))))))

(defcheck solution-5e1c03b0
  #(reduce (fn [a b]
             ((fn lcm [current]
                (if (= (mod current b) 0)
                  current
                  (lcm (+ current a))))
              a))
     %&))

(defcheck solution-5e2b51a7
  #(reduce (fn lcm [a b]
             (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
               (/ (* a b) (gcd a b))))
     %&))

(defcheck solution-5e5d0ec4
  (fn [& nums]
    (if (some zero? nums) 0
                          (let [gcd (fn [a b]
                                      (let [m (mod a b)]
                                        (if (zero? m) b (recur b m))))
                                abs (fn [a] (if (neg? a) (- a) a))
                                lcm-2 (fn [a b] (/ (abs (* a b)) (gcd (max a b) (min a b))))]
                            (reduce lcm-2 nums)))))

(defcheck solution-5edd0dca
  (fn lcm
    ([a] a)
    ([a b] (/ (* a b)
             (loop [a a b b]
               (if (= 0 b) a
                           (recur b (mod a b))))))
    ([a b & r] (apply lcm (lcm a b) r))))

(defcheck solution-5ee8c8ec
  (fn __ [& nums]
    (letfn [(get-seq-fn [num] (iterate #(+ % num) num))
            (lazy-search [& colls]
              (let [first-eles (map first colls)]
                (if (every? #(= (first first-eles) %) first-eles)
                  (first first-eles)
                  (let [sorted-colls (sort-by first colls)]
                    (apply lazy-search (cons (rest (first sorted-colls)) (rest sorted-colls)))))))]
      (apply lazy-search (map get-seq-fn nums)))))

(defcheck solution-5f132a4c
  (letfn [(euclid-gcd [a b]
            (if (zero? (min a b))
              (max a b)
              (recur (min a b) (- (max a b) (min a b)))))]

    (fn q4q100 [& args]
      (reduce
        #(/ (* %1 %2) (euclid-gcd %1 %2))
        args))))

(defcheck solution-5f52dc38
  (letfn
   [(gcd [a b]
      (if (zero? b)
        a
        (recur b, (mod a b))))
    (lcm [a b]
      (/ (* a b) (gcd a b)))]

    (fn [& v] (reduce lcm v))))

(defcheck solution-60503342
  (fn [x & xs]
    (let [_gcd (fn [a b]
                 (if (zero? b) a
                               (recur b (mod a b))))
          _lcm (fn [x y]
                 (/ (* x y) (_gcd x y))) ]
      (if (empty? xs) x
                      (recur (_lcm x (first xs)) (rest xs))))))

(defcheck solution-60538658
  (fn [& s]
    (loop [nums s]
      (if
       (apply = nums)
        (first nums)
        (let [m (apply min nums)]
          (recur
            (map
              (fn [n orig] (if (= n m) (+ n orig) n))
              nums
              s)))))))

(defcheck solution-60a3abd4
  (fn lcm [a b & more]
    (let [gcd (fn [x y]
                (if (zero? y)
                  x
                  (recur y (mod x y))))
          c (* (quot a (gcd a b)) b)]
      (if more
        (apply lcm (cons c more))
        c))))

(defcheck solution-60eeb457
  (fn lcm [& args]
    (letfn [(gcd [x y]
              (if (zero? y) x (recur y (mod x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-610cc4b0
  (fn least-common-m [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m)
                  b
                  (recur b m))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args)
      )))

(defcheck solution-613cd8ab
  (fn [x y & more]
    (let [coll (if (empty? more)
                 (vector x y)
                 (flatten (vector x y more)))]
      (letfn [(gcd-2 [a b]
                (if (= b 0)
                  a
                  (gcd-2 b (mod a b))))
              (gcd-n [coll]
                (reduce gcd-2 coll))]
        (/ (reduce * coll) (gcd-n coll))))))

(defcheck solution-61a9d489
  (fn [& ys]
    (letfn [(gcd1 [a b]
              (if (zero? (rem a b))
                b
                (gcd1 b (rem a b))))
            (gcd [xs]
              (reduce gcd1 xs))]
      (/ (apply * ys) (gcd ys)))))

(defcheck solution-61c546
  (fn lcm [& args]
    (let [ vals (mapcat (fn [x] (map #(* x %) (range 1 100))) args)
          f (fn [x] (every? identity (map #(= 0 (mod x %)) args)))
          ]
      #_(print (map f vals))
      (apply min (filter f vals)))))

(defcheck solution-61ef0340
  (fn [& xs]
    (letfn [(divisible-all? [lcm xs] (every? #(= 0 (mod lcm %)) xs))]
      (let [max_ (apply max xs)]
        (loop [lcm max_]
          (if (divisible-all? lcm xs)
            lcm
            (recur (+ lcm max_))))))))

(defcheck solution-62512cfa
  (fn[& args]
    (letfn[
           (ggT[x y]
             (ffirst (take 1 (drop-while
                               #(not (zero? (second %)))
                               (iterate (fn[[b a]] [a (mod b a)]) [x y])))))
           (kgV[a b]
             (/ (* a b) (ggT a b)))]
      (reduce kgV args))))

(defcheck solution-62526a3b
  #(reduce (fn [a b]
             (let [gcd (fn [a b]
                         (cond (zero? b) a
                               :else (recur b (mod a b))))]
               (/ (* a b) (gcd a b)))) %&))

(defcheck solution-62948e37
  (fn [& c]
    (loop [s (vec c)]
      (if (apply = s)
        (first s)
        (recur
          (let [[sv,i,cv] (apply min-key first (map vector s (range) c))]
            (assoc s i (+ sv cv))))))))

(defcheck solution-62cd1f27
  (fn [& args]
    (let [[mul & muls] (map (fn [n] (map #(* n (inc %)) (range)))
                         args)]
      (first
        (for [m     mul
              :when (apply = m (map last (map #(take-while (partial >= m) %)
                                           muls)))]
          m)))))

(defcheck solution-6382b8f4
  (fn [& numbers]
    (loop [numbers numbers targets numbers]
      (if (apply = targets)
        (first targets)
        (recur numbers
          (let [min-val (apply min targets)
                idx (. targets indexOf min-val)
                l (take idx targets)
                r (nthrest targets (+ 1 idx))]
            (concat l (cons (+ min-val (nth numbers idx)) r))))))))

(defcheck solution-64565317
  (fn f [& args]
    (letfn [(multipliers [n] (apply sorted-set (map #(* n %) (range 1 2000))))]
      (first
        (apply clojure.set/intersection (map multipliers args))))))

(defcheck solution-64deac9b
  (fn lcm [& nums]
    (let [start (apply max nums)]
      (first (drop-while (fn [x]
                           (not (every? zero?
                                  (map #(mod x %)
                                    nums))))
               (iterate #(+ % start) start))))))

(defcheck solution-64f30a35
  (fn lcm [& ns]
    (letfn [(abs [n]
              (if (>= n 0) n (- n)))
            (gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
            (pair-lcm [a b]
              (if (= a b 0)
                0
                (* (/ (abs a) (gcd a b)) (abs b))))]
      (reduce pair-lcm ns))))

(defcheck solution-657fbb71
  (fn [& nums]
    (let [
          is-cm? (fn [n itms] (every? #(zero? (rem n %)) itms))
          f (first nums)]
      (some
        #(when (is-cm? % (rest nums)) %)
        (iterate (partial + f) f)))))

(defcheck solution-658cab4f
  (fn my-least-common-multiple
    [& xs]
    (letfn [(greatest-common-divisor [x y]
              (if-not (= x y)
                (let [maxx (max x y) minn (min x y)]
                  (greatest-common-divisor (- maxx minn) minn))
                x))]
      (reduce #(/ (* %1 %2) (greatest-common-divisor %1 %2)) xs))))

(defcheck solution-660be68b
  (fn lcm [& xs]
    (letfn [(gcd2 [a b]
              (if (zero? b) a (recur b (mod a b))))
            (lcm2 [a b]
              (/ (* a b) (gcd2 a b)))]
      (reduce lcm2 xs))))

(defcheck solution-6629d73f
  (fn [x & y]
    (let [gcd (fn gcd [a b]
                (cond (zero? b) a
                      :else (recur b (rem a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) x y))))

(defcheck solution-665883e0
  (fn [& vs]
    (reduce (fn [a v] (first (filter #(zero? (rem % v))
                               (reductions + (repeat a)))))
      vs)))

(defcheck solution-6669787a
  (fn [& more]
    (first (->> (range)
             (drop 1)
             (map (partial * (first more)))
             (filter (fn [x] (every? #(zero? (mod x %)) more)))))))

(defcheck solution-66f6cdc5
  (fn [& xs]
    (let [gcd (fn [a b]
                (if (= 0 b) a (recur b (mod a b))))
          lcm (fn [a b]
                (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-67561a51
  (fn [& xs]
    (let [quantum (apply min xs)
          lcms (map #(* quantum (inc %)) (range))]
      (some (fn [x] (if (every? #(zero? (rem x %)) xs) x))
        lcms))))

(defcheck solution-677b4e3b
  (fn __ [& xs]
    (first
      (for [c (sort (set (flatten (map #(range 1 500 %) (conj xs 1)))))
            :when (every? #(= 0 (rem c %)) xs)]
        c))))

(defcheck solution-679e3edd
  (fn lcm [& n]
    (let [gcd (fn [a b]
                (if (= b 0) a
                            (recur b (mod a b))))]
      (reduce
        #(/ (* %1 %2) (gcd %1 %2))
        n))))

(defcheck solution-67e01a1b
  (fn lcm
    ([x y]
     (letfn [(gcd [lhs rhs]
               (cond
                 (= lhs rhs) lhs
                 (< lhs rhs) (recur rhs lhs)
                 (= rhs 0) lhs
                 :else (recur (- lhs rhs) rhs)))]
       (/ (* x y) (gcd x y))))
    ([x y & ys]
     (apply lcm (cons (lcm x y) ys)))))

(defcheck solution-68061a76
  (fn [x & coll] (apply min (reduce clojure.set/intersection (set (take 400 (iterate #(+ x %) x)))
                              (map (fn [x] (set (take 400 (iterate #(+ x %) x)))) coll)))))

(defcheck solution-6834e564
  #((fn [a]
      (if (apply = (map first a))
        (first (first a))
        (recur (sort (cons (vector (+ (first (first a)) (second (first a)))
                             (second (first a)))
                       (rest a))))))
    (sort (for [x %&] [x x]))))

(defcheck solution-687609ea
  ; http://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm
  (fn [& initial]
    (loop [c (vec initial)]
      (if (apply = c) ; all numbers equal? => LCM
        (first c)
        (let [m (apply min c) ; min number
              i (.indexOf c m) ; index of min number
              v (get (vec initial) i)] ; initial value of this min number
          (recur (assoc c i (+ m v))))))))

(defcheck solution-695afbac
  (fn lcm [& xs]
    (letfn [(mseq [v] (map #(* v %) (iterate inc 1)))
            (firstofall [multis] (for [e multis] (first e)))
            (allsame [col] (= 0 (count (filter #(not (= % (first col))) col))))
            (sortbyfirst [multis] (sort-by first multis))]

      (loop [multis (sortbyfirst (map mseq xs))]
        (let [smallest (first multis)
              hd (firstofall multis)]
          (if (allsame hd)
            (first hd)
            (recur (sortbyfirst (conj (rest multis) (rest smallest)))) ))))))

(defcheck solution-696b4c52
  (fn [& nums]
    (let [gcd
                  (fn [& nums]
                    (let [[big lil] (sort > nums)]
                      (loop [a big
                             b lil]
                        (let [q (quot a b)
                              r (rem a b)]
                          (if (zero? r)
                            b
                            (recur b r))))))
          g (reduce gcd nums)
          product (apply * nums)]
      (/ product g))))

(defcheck solution-697ff13
  (fn [& ns]
    (let [lte-mcm? #(<= % 210)]
      (apply min (apply clojure.set/intersection (map #(set (filter lte-mcm? (map * (repeat %) (range 1 1000)))) ns)))
      )
    ))

(defcheck solution-6b6f89c
  (fn [& coll]
    (letfn [(lcm [current-and-init]
              (if (= 1 (count (set (map first current-and-init))))
                (first (first current-and-init))
                (let [minimum (first current-and-init)
                      current (first minimum)
                      init (last minimum)
                      next-minimum [(+ current init) init]]
                  (lcm (sort (cons next-minimum (rest current-and-init)))))))]
      (lcm (map #(vector %1 %2) coll coll)))))

(defcheck solution-6befec29
  (fn [& args]
    (loop [cur (apply min args)]
      (if (every? #(integer? (/ cur %)) args)
        cur
        (recur (+ cur (apply min args)))))))

(defcheck solution-6bfbdad2
  (fn lcm [& r]
    (let [gcd2 (fn gcd [a b] (if (> a b) (gcd b a)
                                         (if (= 0 (rem b a))
                                           a
                                           (gcd (- b a) a))) )
          lcm2 (fn [a b] (let [g (gcd2 a b)] (/ (* a b) g)))]
      (loop [a (first r) b (second r) rst (rest (rest r))]
        (if (empty? rst)
          (lcm2 a b)
          (recur (lcm2 a b) (first rst) (rest rst)))))))

(defcheck solution-6c44dfa7
  (fn lr [a & [b & c]]
    (letfn [(g [a b] (if (zero? b) a (g b, (mod a b))))
            (l [a b]
              (/ (* a b) (g a b))
              )]
      (if (empty? c)
        (l a b)
        (let [b (apply lr b c)]
          (l a b)
          )
        )
      )
    ))

(defcheck solution-6cd7e048
  (fn [& args]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-6d0ae934
  (fn [& args]
    (loop [seqs (mapv (fn [a] (iterate #(+ a %)  a)) args)]
      (let [firsts (map first seqs)
            m (apply min firsts)
            i (.indexOf firsts m) ]
        (if (apply == firsts)
          m
          (recur (update-in seqs [i] rest)))))))

(defcheck solution-6d115e7e
  (fn [& args]
    (let [l (fn [fm] (if (apply = (vals fm)) (val (first fm))
                                             (let [[k v] (apply min-key val fm)]
                                               (recur (update-in fm [k] (partial + k))))))]
      (l (zipmap args args)))))

(defcheck solution-6d2dbf23
  (fn lcm
    [& nums]
    (loop [multiples nums]
      (if (apply = multiples)
        (first multiples)
        (let [m (apply max multiples)]
          (recur (map #(* % (int (Math/ceil (/ m %)))) nums)))))))

(defcheck solution-6d3fc016
  (fn lcm [& xs]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (rem a b))))
          lcm (fn [a b]
                (* b (/ a (gcd a b))))]
      (reduce lcm xs))))

(defcheck solution-6d78cbe8
  (fn  [& more]
    (letfn [(gcd [a b]
              (if (= b 0) a
                          (recur b (rem a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) more))))

(defcheck solution-6d841835
  (fn [& a]
    (/ (apply * a)
      (reduce (fn f [x y] (if (= y 0) x (f y (mod x y)))) a))))

(defcheck solution-6dd7ba35
  (fn
    [& x]
    (let [m (for [n x]
              (map * (repeat n) (drop 1 (range))))]
      (first (drop-while (fn [y]
                           (let [b (map (fn [n] (drop-while #(< % y) n)) m)]
                             (not (apply = (map first b)))))
               (first m))))
    ))

(defcheck solution-6dfa1204
  (fn [& args] (let [lcm-attempts (iterate #(+ (apply min args) %) (apply min args))] (ffirst (filter (fn [i] (every? zero? (second i))) (map #(vector % (map (fn [i] (rem % i)) args)) lcm-attempts))))))

(defcheck solution-6e1cd165
  (fn [& args]
    (letfn [(gcd [a b] (if (zero? b) a, (recur b (mod a b))))]
      (/ (apply * args) (reduce gcd args)))))

(defcheck solution-6e2f77c3
  (fn lcm [& coll]
    (letfn [ (gcd [a b]
               (cond
                 (= a b) a
                 (> a b) (gcd (- a b) b)
                 :else   (gcd a (- b a)))) ]
      (reduce #(/ (* % %2) (gcd % %2)) coll)
      )))

(defcheck solution-6e300ae
  (fn lcm
    ([a b]   (letfn [(gcd [a b]
                       (cond
                         (> a b)   (gcd b a)
                         (= a 0)   b
                         :else     (gcd (mod b a) a)))]
               (/ (* a b) (gcd a b))))
    ([a b & args] (apply lcm (cons (lcm a b) args)))))

(defcheck solution-6eca21ec
  (letfn
   [(gcd [x y] (let [[x y] [(min x y) (max x y)]] (if (= 0 x) y (gcd (mod y x) x))))
    (lcm [x y] (let [z (gcd x y)] (/ (* x y) z)))]
    (fn [& xs] (reduce lcm xs))))

(defcheck solution-6f3cf16c
  (fn lcm [a & bs]
    (letfn [
            (multiples [n]
              (map (comp (partial * n) inc) (range)))

            (least-common [cmp A B]
              (loop [P A, Q B]
                (cond
                  (cmp (first P) (first Q)) (recur (rest P) Q)
                  (cmp (first Q) (first P)) (recur P (rest Q))
                  :else (first P))))]

      (reduce #(least-common < (multiples %1)
                 (multiples %2)) a bs))))

(defcheck solution-703bedda
  ; Please don't look at me. I'm hideous...
  ; I make babies cry. ;(
  (fn lcm [& args]
    (letfn [(gcd [a b] ; greatest common divisor
              (if (zero? b) a (gcd b (mod a b))))
            (lcm' [a b] ; least common multiple
              (/ (* a b) (gcd a b)))
            (combinations [s] ; all possible combinations for the args
              (for [x s y s] [x y]))
            (mapcombinations [f s] ; map all combinations with f, then set and sort
              (->> s combinations (map f) set sort))
            (div? [s n] ; is n divisable with ALL in s
              (every? true? (map #(= (rem n %) 0) s)))]
      (loop [mu (->> args (mapcombinations (partial apply lcm')))] ; take each combination's lcm
        (or (identity (some #(if (div? args %) %) mu)) ; take only the first lcm which is divisible with the initial args
            (recur (->> mu (mapcombinations (partial reduce *))))) ; if none found -> multiply the lcms' combinations together
        ))))

(defcheck solution-70444d99
  (fn [& args]
    (loop [h (apply hash-map (flatten (map list args args)))]
      (cond (every? #(= (val (first h)) (val %)) h) (val (first h))
            :else (let [id (key (first (filter #(= (apply min (vals h)) (val %)) h)))]
                    (recur (assoc h id (+ id (h id)))))))))

(defcheck solution-7086e67a
  (fn lcm [& x]
    (loop [s x]
      (if (= 1 (count (set s)))
        (first s)
        (recur (map-indexed #(if (= %2 (apply min s)) (+ %2 (nth x %1)) %2) s))))))

(defcheck solution-709d0aca
  (fn [& x]
    (let [y (apply min x)]
      (loop [z y]
        (if (every? #(zero? (mod z %)) x)
          z
          (recur (+ z y)))))))

(defcheck solution-70a67337
  (fn [& n]
    (letfn [(gcd [n1 n2]
              (cond
                (zero? n1) n2
                (zero? n2) n1
                :else (recur n2 (rem n1 n2))))
            (lcm [n1 n2]
              (* n1 (/ n2 (gcd n1 n2))))
            ]
      (reduce lcm n))))

(defcheck solution-71162f68
  (fn [& x]
    (let
     [gcd (fn gcd [a b] (if (= 0 b) a (gcd b (mod a b))))
      lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm x))))

(defcheck solution-716f01b
  (fn lcm [& args]
    (let [gcd #(if (= 0 %2) %1 (recur %2 (rem %1 %2)))]
      (/ (apply * args) (reduce gcd args)))))

(defcheck solution-71a90fa1
  (fn [& nums]
    (reduce
      (fn [numa numb]
        (/ (* numa numb)
          ((fn ! [num1 num2]
             (if (= (mod num1 num2) 0)
               num2
               (! num2 (mod num1 num2))
               )
             ) numa numb)
          )
        )
      nums)
    ))

(defcheck solution-71cfc8d6
  (fn lcm
    ([x y]
     (letfn [(gcd [a b]
               (cond
                 (= b 0) a
                 (> a b) (gcd b (mod a b))
                 (> b a) (gcd a (mod b a))))]
       (/ (* x y) (gcd x y))))
    ([x y & rest] (apply lcm (lcm x y) rest))))

(defcheck solution-71e34e1
  (fn [ff & rr]
    (letfn [(cm [f]
              (every? #(= 0 (mod f %)) rr))]
      (first (filter cm (iterate (partial + ff) ff))))))

(defcheck solution-71fd2c08
  (fn
    [& xs]
    (loop [l (map #(list % %) xs)]
      (if (apply = (map first l))
        (first (first l))
        (recur (let [min-val (apply min (map first l))]
                 (map #(if (= (first %) min-val)
                         (list (+ (first %) (second %)) (second %))
                         %)
                   l)))))))

(defcheck solution-7273081f
  (fn [& args]
    (loop [margs (zipmap args args) mv (apply max args)]
      (if (apply = (vals margs))
        (first (vals margs))
        (recur (apply merge (map #(if (< (margs %) mv)
                                    (hash-map % (+ % (margs %)))
                                    (hash-map % (margs %))
                                    ) (keys margs)))
          (apply max (vals margs)))))))

(defcheck solution-72780aca
  (fn [x & xs]
    (reduce (fn [a b] (/ (* a b)
                        ((fn mgcd [c d]
                           (if (= 0 c) d (mgcd (rem d c) c)))
                         a b)))
      x
      xs)))

(defcheck solution-72a1fadc
  (fn [& a]
    (let [m (apply max a)]
      (some
        (fn [x] (if (apply = 0 (map #(rem x %) a)) x))
        (iterate #(+ m %) m)))))

(defcheck solution-72acb89c
  (fn [& args]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-73248569
  (fn lcm
    ([x] x)
    ([x y] (letfn [(gcd [a b] (if (zero? a) b (gcd (mod b a) a)))]
             (/ (* x y) (gcd x y))))
    ([x y & args] (reduce lcm (lcm x y) args))))

(defcheck solution-73a03be5
  (fn [& numbers]
    (let [lcm2 (fn [x y]
                 (let [gcd2 (fn gcd [a b]
                              (if (= b 0)
                                a
                                (gcd b (rem a b))))]
                   (/ (* x y) (gcd2 x y))))]
      (reduce lcm2 numbers))))

(defcheck solution-740dbb74
  (fn [& args]
    (let [gcd (fn [a b]
                (->> [a b]
                  (iterate (fn [[x y]] [y (rem x y)]))
                  (map last)
                  (take-while pos?)
                  last))]
      (reduce #(/ (* %1 %2) (gcd (max %1 %2) (min %1 %2))) args))))

(defcheck solution-74bfc8c9
  (fn __ [& args]
    (letfn [(gcd [a b]
              (if (= 0 b)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (if (> (count args) 1)
        (recur
          (flatten (remove nil?
                     (vector (lcm (first args) (second args))
                       (seq (drop 2 args))))))
        (first args)))))

(defcheck solution-7559f144
  (fn [& args] (second (sort (reduce clojure.set/intersection (map #(set (take 500 (iterate (partial + %) 0))) args))))))

(defcheck solution-7567eb48
  (letfn [ (gcd [ a b ]
             (cond
               (> b a) (recur b a)
               (zero? b) a
               :else (recur b (mod a b)))) ]
    (fn lcm [ a & lst ]
      (if (empty? lst) a
                       (let [ b (first lst) ]
                         (recur (/ (* a b) (gcd a b)) (rest lst)))))))

(defcheck solution-75bd2274
  (fn [& n] (reduce #(/ (* %1 %2) ((fn gcd [a b] (if (zero? b) a (recur b (mod a b)))) %1 %2)) n)))

(defcheck solution-75bef23e
  (fn [& xs]
    (loop [ns xs]
      (if (apply = ns)
        (first ns)
        (let [m (apply max ns)]
          (recur (map #(if (= m %1) %1 (+ %1 %2)) ns xs)))))))

(defcheck solution-75c1b963
  (fn [& args]
    (letfn [(gcd [x y]
              (if (= x y) x
                          (if (< x y)
                            (gcd x (- y x))
                            (gcd (- x y) y))))
            (lcm [x y] (/ (* x y) (gcd x y)))]

      (reduce lcm args))))

(defcheck solution-7620de04
  (fn lcm [& nums]
    (letfn [(gcd [x y]
              (if (= 0 (rem x y))
                y
                (recur y (rem x y))))]
      (reduce #(/ (* % %2) (gcd % %2)) nums))))

(defcheck solution-762d6fca
  (fn [& args]
    (loop [vals args]
      (if (apply = vals) (first vals)
                         (let [largest (apply max vals)]
                           (recur (map (fn [v i] (if (< v largest) (+ v i) v)) vals args))
                           ))
      )
    ))

(defcheck solution-763c278d
  (fn [& eles]
    (letfn [(gcd [a b]
              (if (= 0 b) a
                          (recur b
                            (mod a b))))
            (lcm [a b]
              (/ (* a b)
                (gcd a b)))]
      (reduce lcm eles))))

(defcheck solution-764fe588
  (fn [& ns] (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
               (/ (apply * ns) (reduce gcd ns)))))

(defcheck solution-76792794
  (fn lcm [x1 & xn] (first (drop-while
                             (fn [s] (not-every? #(zero? (mod s %)) xn))
                             (iterate (partial + x1) x1)))))

(defcheck solution-76c546ed
  (fn [a & coll]
    (first (filter
             #(every? (fn [b] (zero? (mod % b))) coll)
             (map #(* a %) (range 2 2147483647))))))

(defcheck solution-770ef1b7
  (fn f [& xs]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (rem x y))))]
      (/ (apply * xs)
        (reduce gcd xs))
      )
    ))

(defcheck solution-773f511b
  (fn[& s]
    (letfn [(g[a b] (if (= b 0) a (recur b (mod a b))))]
      (reduce #(/ (* % %2) (g % %2)) s))))

(defcheck solution-77476d2d
  (fn [& nums]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (/ (reduce * nums) (reduce gcd nums)))))

(defcheck solution-7760ecbb
  (fn f[& x]
    (loop [l x]
      (if (= (count (distinct l)) 1)
        (first l)
        (let [m (apply min l)]
          (recur (map-indexed (fn[i v] (if (= v m) (+ v (nth x i)) v)) l)))))))

(defcheck solution-776b89df
  (fn ppcm [& els]
    (let [pgcd (fn [a b] (loop [a a b b] (if (= b 0) a (recur b (rem a b)))))]
      (if (= (count els) 2)
        (/ (* (nth els 0) (nth els 1)) (pgcd (nth els 0) (nth els 1)))
        (ppcm (first els) (apply ppcm (rest els)))))))

(defcheck solution-7785564
  (fn [& r]
    (reduce
      #(/
         (* % %2)
         ((fn f [a b]
            (if (= b 0)
              a
              (f b (mod a b))))
          % %2))
      r)))

(defcheck solution-77be00b
  (fn [& xs]
    (letfn [(gcd [a b] (if (= 0 b) a (gcd b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-77f93f21
  (fn g [& p] (/ (apply * p) (reduce (fn f [a b] (if (= b 0) a (f b (rem a b)))) p))))

(defcheck solution-78300df2
  (fn [& nums]
    (loop [n (first nums)]
      (if (= 0 (reduce + (map #(rem n %) nums))) n (recur (+ n (first nums)))))))

(defcheck solution-78454385
  (fn [& args]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-78c0aa67
  (fn kgv* [& x]
    (letfn [(ggt [a b]
              (if (= 0 b) a (ggt b (mod a b))))
            (kgv [m n]
              (/ (* m n) (ggt m n)))]
      (reduce kgv x))))

(defcheck solution-79990aee
  (fn [& args]
    (let [gcd (fn [m n]
                (if (zero? n)
                  m
                  (recur n (mod m n))))]
      ; quot only works with integers; must use / to accommodate ratios
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-79a45ac8
  (fn lcd [x y & more](let [result (/ (* x y) (loop [a x b y] (if (zero? b) a (recur b (mod a b))))) z (first (flatten more)) more (rest (flatten more))] (if z (do (lcd result z more)) result))))

(defcheck solution-79c1c15c
  (fn [& xs]
    (loop [M 2]
      (let [ms (group-by identity (flatten (for [x xs] (map #(* x %) (range 1 M)))))
            t (filter (fn [[k v]] (= (count xs) (count v))) ms)]
        (if (= 1 (count t))
          (first (first t))
          (recur (inc M)))))))

(defcheck solution-79f3ab48
  (fn [x & xs]
    (letfn [(gcd [x y]
              (if (zero? y) x
                            (recur y (rem x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) x xs))))

(defcheck solution-7a012c51
  (fn [& args]
    (letfn
     [(gcd [a b]
        (if (zero? b) a
                      (gcd b (mod a b))))]
      (/
        (reduce *   args)
        (reduce gcd args)))))

(defcheck solution-7a57cb8
  #(reduce % %&) #(/ (* % %2) ((fn f [x y] (if (= 0 y) x (f y (mod x y)))) % %2)))

(defcheck solution-7a8e6ead
  (letfn [(gcd [a b] (if (= a b) a (if (> a b) (gcd (- a b) b) (gcd a (- b a)))))
          (modulo [d] (if (neg? d) (- d) d))
          (lcm [a b]
            (* (modulo b) (/ (modulo a) (gcd a b))))
          (lcmx [a & xs]
            (reduce (fn [acc b] (lcm acc b)) a xs))]
    lcmx ))

(defcheck solution-7ada0e0
  (fn [& n](first (filter (fn [v] (every? integer? (map #(/ v %) n)))(map #(* (apply min n) %) (next (range)))))))

(defcheck solution-7adbf023
  (fn [c & r]
    (/ (apply * c r)
      (reduce #(if (= 0 %)
                 %2
                 (recur (rem %2 %) %))
        c
        r))))

(defcheck solution-7bc20f63
  (fn lcm [& ns]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))
          gcdx (fn gcdm [[a & [b & r]]]
                 (cond
                   (nil? b) a
                   :default (gcdm (conj r (gcd a b)))))]
      (/ (apply * ns) (gcdx ns)))))

(defcheck solution-7c849b67
  (fn lcm [& args]
    (let [gcd #(if (zero? %) %2 (recur (mod %2 %) %))]
      (/ (apply * args) (reduce gcd args)))))

(defcheck solution-7c8bcf7f
  (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
        lcm (fn [a b] (/ (* a b) (gcd a b)))]
    #(reduce lcm %&)))

(defcheck solution-7cc5cf6e
  (fn [& ns]
    (let [gcd (fn gcd
                ([a b]
                 (loop [a a
                        b b]
                   (if (= 0 b)
                     a
                     (recur b (mod a b)))))
                ([a b & cs]
                 (apply gcd (cons (gcd a b) cs))))]
      (/ (apply * ns) (apply gcd ns)))))

(defcheck solution-7cea1ac4
  (fn lcm
    [& ns]
    (reduce (fn [a b]
              (first (drop-while #(or (not= 0 (rem % a))
                                      (not= 0 (rem % b))) (iterate (partial + a) a)))) ns)))

(defcheck solution-7cfe2964
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (= 0 b)
                a
                (gcd b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd (max a b) (min a b))))]
      (reduce lcm xs))))

(defcheck solution-7d50a7da
  (fn [& m]
    (let [mul (apply min m)
          muls (iterate #(+ mul %) mul)]
      (first
        (filter
          (fn [n] (every? zero? (map #(rem n %) m)))
          muls)))))

(defcheck solution-7d50d000
  (fn lcm[& nums]
    (let [gcd (fn gcd [l r](cond
                             (= 0 l) r
                             (= 0 r) l
                             (= l r) r
                             (> l r) (gcd (- l r) r)
                             (> r l) (gcd (- r l) l)))]
      (let [f (first nums)
            n (fnext nums)
            l (/ (* f n) (gcd f n))]
        (if (= 2 (count nums)) l
                               (apply lcm (cons l (rest (rest nums)))))))))

(defcheck solution-7e1ab41e
  (fn [n & others]
    (loop [lcm n]
      (if (every? zero? (map #(mod lcm %) others))
        lcm
        (recur (+ lcm n))))))

(defcheck solution-7eb6746
  ;(fn [& args0]
  ;  (loop [args args0]
  ;    (let [smallest (apply min args)]
  ;      (if (apply = args)
  ;        smallest
  ;        (recur (map
  ;                 #(if (= %1 smallest) (+ %1 %2) %1)
  ;                 args args0))))))

  ;(fn [& xs]
  ;  (loop [mults (map #(for [x (drop 1 (range))] (* x %)) xs)]
  ;    (let [fs (map first mults)]
  ;      (if (apply = fs)
  ;        (first fs)
  ;        (let [smallest (apply min fs)]
  ;          (recur (map #(if (= smallest (first %)) (rest %) %) mults)))))))

  (fn [& xs]
    (let [multiples (fn [n] (map (partial * n) (rest (range))))
          mults (map multiples xs)]
      (loop [mults mults]
        (let [heads (map first mults)
              least (apply min heads)]
          (if (apply = heads)
            (ffirst mults)
            (recur (map #(drop (if (= least (first %)) 1 0) %)
                     mults))))))))

(defcheck solution-7ec5f071
  (fn [& xs]
    (let [gcd #(if (= 0 %2) %1 (recur %2 (mod %1 %2)))
          lcm #(/ (* %1 %2) (gcd %1 %2))]
      (reduce lcm xs))))

(defcheck solution-7ed28b7c
  (fn [& a] (reduce (fn [x y] (* x (/ y ((fn [a b] (if (zero? b) a (recur b (mod a b)))) x y)))) (seq a))))

(defcheck solution-7eddfb06
  (fn [& args]
    (let [max-num (apply max args)]
      (loop [cur-num max-num
             multiplier 2]
        (if (every? #(= 0 (mod cur-num %)) args)
          cur-num
          (recur (* max-num multiplier) (inc multiplier)))))))

(defcheck solution-7f5023b5
  (fn [& r]
    (let [gcd	(fn [x y]
                 (let [re (rem x y)]
                   (if (= 0 re)
                     y
                     (recur y re))))

          lcm #(/ (* % %2)
                 (gcd % %2))]

      (reduce lcm r))))

(defcheck solution-7f635e7f
  (fn [& col]
    (letfn [(gcd [x y]
              (if (= x 0) y
                          (recur (mod y x) x)))]
      (/ (apply * col) (reduce gcd col)))))

(defcheck solution-7f744a6f
  (fn lcm [x & s]
    (letfn [(gcd [x y] (let [m (min x y) M (max x y)] (if (zero? m) M (gcd (rem M m) m))))]
      (if (empty? s)
        x
        (let [y (apply lcm s)]
          (/ (* x y) (gcd x y)))))))

(defcheck solution-7ffcfe14
  (fn [& xs]
    (letfn [(lcm [a b]
              (let [as (iterate (partial + a) a)
                    bs (iterate (partial + b) b)
                    ras (reductions conj #{} as)
                    rbs (reductions conj #{} bs)
                    tuples (map vector as bs ras rbs)]
                (first (for [[a b ra rb] tuples
                             :when (or (ra b) (rb a))]
                         (if (ra b)
                           b
                           a)))))]
      (reduce lcm xs))))

(defcheck solution-80039f68
  (fn lcm [& nums]
    (letfn [(gdc [a b]
              (cond
                (= a b) b
                (> a b) (gdc (- a b) b)
                :else (gdc a (- b a))))]
      (reduce #(/ (* % %2) (gdc % %2)) nums))))

(defcheck solution-8016b18e
  (fn lcm [& args]
    (let [m (apply max args)]
      (loop [n 1]
        (if (every? #(zero? (mod (* m n) %)) args)
          (* m n)
          (recur (inc n)))))))

(defcheck solution-8017fb0f
  (fn lcm [& args]
    (letfn [ (same? [v] (apply = v))
            (least-idx [v] (loop [s v least (first v) least-idx 0 idx 0]
                             (if (empty? s)
                               least-idx
                               (let [current   (first s)
                                     change    (< current least)
                                     new-least (if change current least)
                                     new-idx   (if change idx least-idx) ]
                                 (recur (rest s) new-least new-idx (inc idx) )
                                 )))) ]
      (let [v (vec args)]
        (loop [t v]
          (if (same? t)
            (first t)
            (let [idx (least-idx t)]
              (recur (assoc t idx (+ (t idx) (v idx))))
              )))))))

(defcheck solution-804ae428
  (fn lcm [& coll]
    (case (count coll)
      1 (first coll)
      2 (let [[a b] coll]
          (->> (iterate inc 1)
            (map #(* a %))
            (filter #(integer? (/ % b)))
            (first)))
      (apply lcm (into [(apply lcm (take 2 coll))] (drop 2 coll))))))

(defcheck solution-80d17a39
  (fn lcm [& args]
    (loop [args args filtered (range (apply min args) 2147483647 (apply min args))]
      (if (empty? args) (first filtered)
                        (recur (rest args) (filter #(= 0 (rem % (first args))) filtered))
                        )
      )
    ))

(defcheck solution-810fa4e7
  (fn lcm [& nums]
    (let [ index-of (fn [v l] (loop [i 0] (if (= (nth l i)
                                                v) i (recur (inc i)))) ) ; get the index of value v in sequence l. assume v is in l
          lcm-help (fn [now-nums origin-nums]
                     (if (apply = now-nums)
                       (first now-nums)
                       (let [min-value (apply min now-nums)
                             min-value-index (index-of min-value now-nums)
                             origin-val (nth origin-nums min-value-index)
                             new-val (+ min-value origin-val)
                             now-nums-vector (vec now-nums)
                             new-now-nums-vector (assoc now-nums-vector min-value-index new-val)
                             new-now-nums (seq new-now-nums-vector)]
                         (recur new-now-nums origin-nums))
                       ))]
      (lcm-help nums nums))))

(defcheck solution-81341389
  (fn [& args]

    (reduce (fn [x y]
              (/ (* x y) ((fn gcd [p q]
                            (if (= q 0) p
                                        (gcd q (mod p q)))
                            ) x y))
              ) args)

    ))

(defcheck solution-81628391
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))]
    #(reduce lcm %&)))

(defcheck solution-819632f8
  (fn [& args]
    (loop [it (vec args)]
      (if (apply = it)
        (first it)
        (let [i (->> it (apply min) (.indexOf it))]
          (recur (update-in it [i] + (nth args i))))))))

(defcheck solution-820711ce
  (fn [a & l]
    (first (filter (fn [x] (every? #(= 0 (rem x %)) l)) (iterate #(+ a %) a)))))

(defcheck solution-8230d740
  (fn lcm ([a b] (let [gcd ((fn f [x y] (if (= x y) x (if (< x y) (f (- y x) x) (f y x)))) a b)] (/ (* a b) gcd))) ([a b & c] (apply lcm (lcm a b) c))))

(defcheck solution-824de2af
  (fn [n & r]
    (first (filter (fn [nm] (every? #(= 0 (rem nm %)) r))
             (iterate #(+ n %) n) ))))

(defcheck solution-83144a05
  (fn [& coll]
    (let [m (apply max coll)]
      (first (filter
               (fn [x] (every? #(zero? (rem x %)) coll))
               (iterate #(+ m %) m))))))

(defcheck solution-832d382a
  (fn [a & args]
    (loop [n a]
      (if (every? #(= 0 (rem n %)) args)
        n
        (recur (+ n a))))))

(defcheck solution-8370b514
  (fn kgv [& x]
    (let [ m (apply min x) ]
      (letfn [(k  [x n]
                (if (every? #(= 0 (rem n % )) x)  n
                                                  (k x (+ n m)))
                )  ] (k x m )
                     ))))

(defcheck solution-83ebf8d8
  (fn [& nums]
    (letfn [(gcd [a b]
              (cond (< a b)   (recur b a)
                    (zero? b) a
                    :else     (recur b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm nums))))

(defcheck solution-840d649d
  (fn lcm [& xs]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (l [a b] (/ (* a b) (gcd a b)))]
      (reduce l xs))))

(defcheck solution-84519c88
  (fn [n & ns]
    (first
      (for [m (next (range))
            :let [n (* n m)]
            :when (every? #(zero? (mod n %)) ns)] n))))

(defcheck solution-849e69c
  #(let [gcd (fn [x y](loop [a x b y] (if (= (rem a b) 0) b (recur b (rem a b)))))]
     (reduce (fn [acc x] (/ (* acc x) (gcd acc x))) %&)))

(defcheck solution-8524f36
  (fn [& ns]
    (let [incr (apply max ns)]
      (first
        (drop-while (fn [i] (not-every? #(= (mod i %) 0) ns))
          (iterate #(+ incr %) incr))))))

(defcheck solution-86619d40
  (fn lcm [& numbers]
    (loop [factors (repeat (count numbers) 1)]
      (let [products (map * numbers factors)
            max-product (apply max products)
            conditional-factor-inc (fn [n f]
                                     (if (< (* n f) max-product)
                                       (inc f)
                                       f))]
        (if (apply = products) max-product
                               (recur (map conditional-factor-inc numbers factors)))))))

(defcheck solution-86bc31f2
  (fn lcm [x y & xs]
    (loop [ret x]
      (if (= (mod ret y) 0)
        (if (= (count xs) 0)
          ret
          (apply lcm ret xs))
        (recur (+ ret x))))))

(defcheck solution-87597905
  (fn [& items]
    (letfn [(gcd [a b]
              (if (zero? a)
                b
                (recur (mod b a) a)))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce #(lcm % %2) items))))

(defcheck solution-87667830
  (fn [& ns]
    (let [gcd
          (fn gcd [a b]
            (if (= b 0) a
                        (gcd b (mod a b))))]
      (/ (apply * ns) (reduce gcd ns)))))

(defcheck solution-878ff0b0
  (fn [ & xs] (reduce
                #(/ (* %1 %2)
                   ((fn gcd [x y] (if (= 0 x) y (gcd (mod y x) x))) %1 %2)) xs)))

(defcheck solution-883f869
  (fn [& xs]
    (letfn [(gcd [x y] (if (= y 0) x (recur y, (mod x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-89530a53
  (fn [& args]
    (->> args
      (map (fn [x] (take 1000 (iterate #(+ x %) x))))
      (map set) ;(#{5 10 15 etc} #{3 6 9 etc} #{7 14 21 etc})
      (apply clojure.set/intersection) ;#{210 315 105}
      (apply min) ;105
      )))

(defcheck solution-89675d26
  (fn [& c]
    (letfn [(f [p1 p2]
              (some #(when (zero? (mod % p1)) %) (iterate #((partial + p2) %) p2)))]
      (reduce #(if (= 0 (mod %1 %2)) %1
                                     (f %1 %2))
        c))))

(defcheck solution-8995ca9a
  (fn [n & nums]
    (first
      (filter
        (fn [m] (every? #(zero? (rem m %)) nums))
        (iterate #(+ % n) n)))))

(defcheck solution-8a594734
  (fn [& xs] (letfn
              [(lcm [a b] (/ (* a b) (gcd a b)))
               (gcd [c d] (if (= c d) c (apply gcd (if (> c d) [(- c d) d] [c (- d c)]))))]
               (reduce lcm xs))))

(defcheck solution-8a638f85
  (fn [a & b] (first (filter (fn [r] (= 0 (reduce #(+ % (mod r %2)) 0  b))) (map #(* (inc %) a) (range))))))

(defcheck solution-8a7c93d7
  (fn
    [& xs]
    (let [gcd (fn gcd [a b]
                (if (= 0 b)
                  a
                  (gcd b (mod a b))))
          lcm (fn lcm [a b]
                (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-8af65457
  (letfn [(gcd [x y]
            (if (= y 0) x (gcd y (mod x y))))
          (lcm [x y]
            (/ (* x y) (gcd x y)))]
    (fn lcms [& xs]
      (reduce lcm xs))))

(defcheck solution-8b1249c2
  (fn lcm [x & y]
    (let [gcd (fn [a b]
                (if (zero? b) a
                              (recur b (mod a b))))]
      (/ (reduce * (cons x y)) (reduce gcd x y)))))

(defcheck solution-8be1245
  (fn lcm [& col]
    (loop [lowest (apply max col),
           acc lowest, col col]
      (cond
        (reduce #(and %1 (= 0 (mod acc %2))) true col) acc
        :else (recur lowest (+ lowest acc) col)))))

(defcheck solution-8c57d1e6
  (fn [x & y]
    (loop [a x]
      (if (every? #(= 0 (mod a %)) y)
        a
        (recur (+ a x))))))

(defcheck solution-8c6fa9e4
  (letfn [(gcd [x y]
            (cond (> y x) (gcd y x)
                  (zero? y) x
                  :else (gcd y (rem x y))))

          (lcs [x y]
            (/ (* x y)
              (gcd x y)))]
    (fn [& xs]
      (reduce lcs xs))))

(defcheck solution-8ca750b8
  (fn [& nums]
    (letfn
     [(min-index [xs]
        (loop [m (first xs) m-idx 0 ys (rest xs) idx 1]
          (if (seq ys)
            (if (< (first ys) m)
              (recur (first ys) idx (rest ys) (inc idx))
              (recur m m-idx (rest ys) (inc idx)))
            m-idx)))]
      (let [nums (vec nums)]
        (loop [xs nums]
          (if (apply = xs)
            (xs 0)
            (let [idx (min-index xs)]
              (recur (assoc xs idx (+ (xs idx) (nums idx)))))))))))

(defcheck solution-8cca1824
  (fn least-common-multiple [& args]
    (letfn [(least-common-multiple-of-two [a b]
              (if (= a b) a
                          (if (> a b) (least-common-multiple b a)
                                      (loop [mu-a 1 mu-b 1]
                                        (if (= (* mu-a a) (* mu-b b))
                                          (* mu-a a)
                                          (if (> (* mu-a a) (* mu-b b))
                                            (recur mu-a (inc mu-b))
                                            (recur (inc mu-a) mu-b)))))))]
      (loop [remaining (rest args) ans (first args)]
        (if (empty? remaining)
          ans
          (recur (rest remaining) (least-common-multiple-of-two ans (first remaining))))))))

(defcheck solution-8cdead
  (fn lcm
    ([a] a)
    ([a b]
     (letfn [(gcd [a b] (if (= 0 b) a (recur b (rem a b))))
             (abs [x] (if (< x 0) (- x) x))]
       (if (= 0 a b)
         0
         (/ (abs (* a b)) (gcd a b)))))
    ([a b & args]
     (apply lcm (lcm a b) args))))

(defcheck solution-8cebc396
  (fn [& numbers]
    (let [
          gcd (fn [x y]
                (cond
                  (= x y) x
                  (> x y) (recur (- x y) y)
                  :else (recur (- y x) x)))
          mul (apply * numbers)]
      (/ mul (reduce gcd numbers)))))

(defcheck solution-8cfd6dc9
  (fn lcm [& numbers]
    (loop [mth (apply vector numbers)]
      (if (= (apply min mth) (apply max mth))
        (first mth)
        (let [min-pos (.indexOf mth (apply min mth))]
          (recur (assoc mth min-pos (+ (nth mth min-pos) (nth numbers min-pos)))))))))

(defcheck solution-8d9ca688
  (fn [& args] (some #(if (zero? (apply + (map (fn [n] (mod % n)) (rest args)))) %) (map #(* (first args) (inc %)) (range)))))

(defcheck solution-8ddfc049
  (fn [& coll]
    (let [m (apply max coll)
          mults (iterate (partial + m) m)
          all-div? (fn [multiple] (every? #(= 0 (mod multiple %))
                                    coll))]
      (first (drop-while (complement all-div?) mults)))))

(defcheck solution-8dfe3ce
  (fn [& p]
    (some #(when (every? (fn [c] (= 0 (rem % c)))
                   (next p))
             %)
      (map (comp (partial * (first p)) inc) (range)))))

(defcheck solution-8e5ddad7
  (fn [& c]
    (loop [d (vec c)]
      (let [i (.indexOf d (apply min d))]
        (if (= 1 (count (distinct d)))
          (first d)
          (recur (assoc d i (+ (nth c i) (nth d i)))))))))

(defcheck solution-8eff474b
  (fn [& n]
    (reduce
      (fn [a b]( some #(if (= 0 (mod % a)) %)
                 (iterate #(+ b %) b))) n)))

(defcheck solution-8f19b3b3
  (fn [& z] (
              reduce (fn [x y]
                       (/ (* x y)
                         (
                          (fn peu [u v]
                            (cond
                              (> v u) (peu v u)
                              (= 0 v) u
                              (> v 0) (peu (- u v) v)
                              )
                            ) x y
                          )
                         )
                       )
              (vec z)
              )))

(defcheck solution-8f86bc9f
  (fn [& vs]
    (first
      (apply clojure.set/intersection
        (map (fn [s] (apply sorted-set (take 3000 (map #(* s %) (range 1 4000)))))
          vs)))))

(defcheck solution-8fc70149
  (fn __ [& r]
    (if (empty? r) (empty r)
                   (let [n (count r),
                         l (range n)]
                     (loop [s (vec r)]
                       (if (apply = s) (first s)
                                       (let [k (apply min-key #(nth s %) l)]
                                         (recur (assoc s k (+ (nth s k) (nth r k)))))))))))

(defcheck solution-8ff67635
  (fn [x & xs]
    (let [multiples (fn [a] (iterate #(+ a %) a))
          drop-while-below (fn [b] (fn [coll] (drop-while #(< % b) coll)))
          ms (multiples x)
          mss (map multiples xs)]
      (loop [[x & ms] ms
             mss mss]
        (let [mss (map (drop-while-below x) mss)]
          (if (apply = x (map first mss))
            x
            (recur ms mss)))))))

(defcheck solution-9054a068
  (fn [& args]
    (/
      (apply * args)
      (reduce #(if (zero? %2) %1 (recur %2 (mod %1 %2))) args))))

(defcheck solution-908087cd
  #(% %& %&) (fn f [i o]
               (let [m (apply min i)]
                 (if (apply = i)
                   m
                   (f (map #(if (= % m)
                              (+ % %2)
                              %) i o)
                     o)))))

(defcheck solution-90865678
  (fn [& s] (/ (apply * s) (reduce #(if (= 0 %2) % (recur %2 (rem % %2))) s))))

(defcheck solution-90c04b3d
  (fn [& nums]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (gcd y (rem x y))))]
      (/ (reduce * nums) (reduce gcd nums)))))

(defcheck solution-90dfce5c
  (fn f
    ([a b]
     (/ (* a b) ((fn gcd [a,b]
                   (if (= a b) a (if (> a b) (gcd (- a b) b) (gcd (- b a) a))))
                 a b)))
    ([a b & more]
     (apply f (cons (/ (* a b) ((fn gcd [a,b]
                                  (if (= a b) a (if (> a b) (gcd (- a b) b) (gcd (- b a) a))))
                                a b)) more)))))

(defcheck solution-9103d3dd
  (fn [& n] (reduce #(/ (* %1 %2) (loop [a %1 b %2] (if (= 0 b) a (recur b (mod a b))))) n)))

(defcheck solution-9156bf4d
  #(letfn [(abs [x] (if (neg? x) (- x) x))
           (gcd [a b] (if (zero? b) a (gcd b (mod a b))))
           (lcm [a b] (/ (abs (* a b)) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-9159942d
  (fn problem-100
    [& nums]
    (letfn [
            (gcd [a b] (cond (zero? a) b
                             (zero? b) a
                             :else (recur b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]

      (reduce #(lcm %1 %2) nums))))

(defcheck solution-91ed00ad
  (letfn [(abs [n] (if (neg? n) (- n) n))
          (gcd [a b] (if (zero? b)
                       (abs a)
                       (recur b (rem a b))))]
    (fn lcm
      ([a b]
       (if (some zero? [a b])
         0
         (* (abs a) (/ (abs b) (gcd a b)))))
      ([a b & cs]
       (reduce lcm a (cons b cs))))))

(defcheck solution-92b6c8fc
  (fn [ & args]
    (letfn [ (gcd[x y] (loop[x x y y]
                         (cond (> x y) (recur y (- x y))
                               (> y x) (recur x (- y x))
                               :else x)))]
      (reduce #(/ (* % %2) (gcd % %2)) args))))

(defcheck solution-93c9999b
  (fn [& args](
                loop[n (apply min args)]

                (if (=
                      (map (fn[x](/ n x)) args)
                      (map (fn[x](int (/ n x))) args)
                      )
                  n
                  (recur (+ n (apply min args)))
                  )

                )))

(defcheck solution-93d399f1
  (fn [& i]
    (let [gcd (fn gcd [& i]
                (if (apply = i)
                  (first i)
                  (let [minimum (apply min i)]
                    (apply gcd (map #(if (= % minimum) % (- % minimum)) i)))))]
      (/ (apply * i) (apply gcd i)))))

(defcheck solution-93e40c9
  (fn [ & v ]
    (loop [ranges       (map #(map (fn [i] (* (inc i) %)) (range)) v)]
      (let [first-ranges (map first ranges)
            all-same     (apply = first-ranges)
            min-fr       (apply min first-ranges)
            ]
        (if all-same
          (first first-ranges)
          ;; else we want to rest the range with the smallest
          (recur
            (map
              (fn [rng]
                (if (= (first rng) min-fr) (rest rng) rng))
              ranges)
            )
          )
        )
      )
    ))

(defcheck solution-93f50b7b
  (fn [& xs]
    (letfn [(gcd [a b]
              (let [x (max a b)
                    y (min a b)]
                (if (= x y)
                  x
                  (gcd (- x y) y))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-94003e04
  (fn lcm
    ([x y]
     (letfn [(gcd [x y] (cond (= x 0) y (<= x y) (gcd (mod y x) x) :else (gcd y x)))]
       (/ (* x y) (gcd x y))))
    ([x y & r]
     (apply lcm (lcm x y) r))))

(defcheck solution-940e04a8
  (fn [& xs]
    (letfn [(l-c-m
              ([ys os]
               (if (apply = ys)
                 (first ys)
                 (let [m (apply min ys)
                       [h t] (split-with #(not= % m) ys)]
                   (l-c-m (concat h [(+ (os (count h)) (first t))] (rest t)) os)))))]
      (l-c-m xs (vec xs)))))

(defcheck solution-94a954d2
  (fn [ & xs]
    (letfn [(gcd [x y] (loop [a x b y] (if (zero? b) a (recur b (mod a b)))))]
      (reduce (fn [a b] (/ (* a b) (gcd a b))) (first xs) (rest xs)))))

(defcheck solution-950193f8
  (fn lcm
    ([x y]
     (let [gcd (fn gcd [a b]
                 (if (zero? b)
                   a
                   (recur b (mod a b))))]
       (/ (* x y)
         (gcd x y))))
    ([x y & rest]
     (reduce lcm 1 (concat [x y] rest)))))

(defcheck solution-95439bc6
  (fn [& x]
    (let [m (apply max x)]
      (loop [n 1]
        (if (every? zero? (map #(mod (* n m) %) x))
          (* n m)
          (recur (+ n 1)))))))

(defcheck solution-9594c5d9
  #(letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))
           (gcd-mult [c] (reduce gcd 0 c))]
     (/ (apply * %&) (gcd-mult %&))))

(defcheck solution-95dbbdc2
  (let [gcd (fn [a b]
              (if (zero? b)
                a
                (recur b, (mod a b))))
        lcm (fn [a b] (/ (* a b) (gcd a b)))]
    (fn [& nums]
      (reduce lcm nums))))

(defcheck solution-95f3b4b0
  (fn [a & bs]
    (let [ps (apply every-pred (map (fn [b] #(zero? (rem % b))) bs))]
      (some #(if (ps %) % nil) (iterate (partial + a) a)))))

(defcheck solution-95f6a438
  (fn lcm [& args]
    (loop [c (apply max args) max c]
      (if (every? #(zero? (mod c %)) args) c (recur (+ c max) max)))))

(defcheck solution-960b3ba1
  (letfn [(gcd [a b]
            (cond
              (= 0 a) b
              (= 0 b) a
              (> a b) (gcd (- a b) b)
              :else (gcd a (- b a))
              ))]
    (fn [& as] (/ (reduce * as) (reduce gcd as)))
    ))

(defcheck solution-963a400
  (fn [& x]
    (loop [xm x]
      (if (apply = xm)
        (first xm)
        (let [l (apply min xm),
              xm1 (map #(if (= % l) (+ % %2) %) xm x)]
          (recur xm1))))))

(defcheck solution-969a3843
  (fn lcmm [& ns]
    (letfn [(gcd [a b]
              ;; euclid's algorithm
              (if (= b 0) a
                          (recur b (mod a b))))

            (lcm [a b]
              (/ (* a b) (gcd a b)))]

      (reduce lcm ns))))

(defcheck solution-96e4da98
  (fn [& nums]
    (let [gcd (fn [a b]
                (cond
                  (= b 0) a
                  (= a 0) b
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))]
      (reduce (fn [a b] (* (/ a (gcd a b)) b)) nums))))

(defcheck solution-96e5853d
  (fn lcm
    ([x y] (let [gcd (fn gcd [x y] (if (== y 0) x (gcd y (mod x y))))]
             (/ (* x y) (gcd x y))))
    ([x y & more] (reduce lcm (lcm x y) more))))

(defcheck solution-96e7d7d9
  (fn [& xs] (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))] (/ (reduce * xs) (reduce gcd xs)))))

(defcheck solution-973ec135
  ; see also: http://stackoverflow.com/questions/147515/least-common-multiple-for-3-or-more-numbers
  (fn [& args]
    (let [gcd #(loop [a %1, b %2]
                 (if (= 0 b)
                   a
                   (recur b (mod a b)))),
          lcm #(/ (* %1 %2) (gcd %1 %2))]
      (reduce lcm args)
      )))

(defcheck solution-97eddd6b
  (fn [f & args]
    (some (fn [n] (when (every? zero? (map #(mod n %) args)) n))
      (map #(* f (inc %)) (range)))))

(defcheck solution-9801c49
  (fn [& args]
    (let [a (first args)]
      (loop [x a]
        (if (every? zero? (map #(mod x %) args))
          x
          (recur (+ x a))
          )))))

(defcheck solution-9855382d
  (fn [& s]
    (let [vals (vec s)]
      (loop [t (vec s)]
        (let [m (apply min t)
              i (ffirst (filter #(= m (last %)) (map-indexed vector t)))]
          (if (every? (partial = m) t)
            m
            (recur (update-in t [i] (partial + (vals i))))))))))

(defcheck solution-9858f03a
  (fn z [& coll]

    (let [low (apply min coll)

          step low]

      (loop [res low]

        (if (every? #(zero? (mod res %)) coll)

          res

          (recur (+ res step)))))))

(defcheck solution-98795c10
  (fn
    [& elems]
    (let [indices (zipmap (range) elems)]
      (loop [v elems]
        (if (> (count (distinct v)) 1)
          (recur (assoc (vec v) (.indexOf v (apply min v)) (+ (apply min v) (get indices (.indexOf v (apply min v))))))
          (first v))
        ))))

(defcheck solution-98ffeacd
  (fn lcm
    ([x y]
     (/
       (* x y)
       ((fn gcd[x y]
          (cond
            (zero? y) x
            :else (gcd y (mod x y))))
        x y)
       )
     )
    ([x y & r](reduce lcm (lcm x y) r))))

(defcheck solution-992b0a9d
  #(let [f (fn [a b] (loop [x a, y b]
                       (cond (= x y) x
                             (< x y) (recur (+ x a) y)
                             :else (recur x (+ b y)))))] (reduce f %&)))

(defcheck solution-99da26a3
  (fn [& n]
    (letfn [(g [a b] (if (= 0 b) a (g b (mod a b))))
            (l [a b] (/ (* a b) (g a b)))]
      (reduce l n))))

(defcheck solution-9a1fffc9
  (fn [& xs]
    (let [gcd (fn [x y]
                (if (= y 0) x
                            (recur y (mod x y))))
          lcm (fn [x y]
                (/ (* x y) (gcd x y)))]
      (reduce lcm xs))))

(defcheck solution-9a3736d7
  (fn lcm [& numbers]
    (reduce (let [gcd (fn gcd [x y]
                        (if (= 0 x)
                          y
                          (gcd (rem y x) x)))]
              #(/ (* %1 %2) (gcd %1 %2)))
      numbers)))

(defcheck solution-9a8b63c
  (fn lcm
    ([a b]
     (let [gcd (fn [a b]
                 (if (zero? b) a
                               (recur b (mod a b))))]
       (/ (* a b) (gcd a b))))
    ([a b & more]
     (apply lcm (lcm a b) more))))

(defcheck solution-9acb182a
  (fn lcm [& args]
    (/ (reduce * args)
      (reduce (fn [m n] (if (zero? n) m (recur n (mod m n)))) args))))

(defcheck solution-9af6f2db
  (fn [a b & more]
    (letfn [(gcd [x y]
              (if (= 0 (mod x y))
                y
                (gcd y (mod x y))))
            (lcm [x y]
              (/ (* x y) (gcd x y)))]
      (reduce lcm (lcm a b) more))))

(defcheck solution-9b9fbb68
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m)
                  b
                  (recur b m))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args)
      )))

(defcheck solution-9bce1e96
  (fn [& nums]
    (let [incnum (apply min nums)]
      ((fn [x]
         (if (every? #(= 0N (rem x %)) nums)
           x
           (recur (+ x incnum))))
       incnum))))

(defcheck solution-9bdbc889
  (fn [& nums]
    (letfn [(gcd2 [m n] (if (zero? n) m (recur n (mod m n))))
            (lcm2 [m n] (/ (* m n) (gcd2 m n) ) )
            (lcmn [s l] (if (empty? s) l (recur (-> s next) (-> s first (lcm2 l) )  )  ) ) ]
      (lcmn (rest nums) (first nums))   )))

(defcheck solution-9beee134
  (fn [& xs]
    (let [step (apply min xs)]
      (loop [try step]
        (if (every? #(= 0 (mod try %)) xs)
          try
          (recur (+ try step)))))))

(defcheck solution-9bf3b892
  (fn [& s]
    (let [abs #(if (neg? %) (- %) %)
          gcd #(if (zero? %2) % (recur %2 (mod % %2)))
          lcm #(* %2 (quot % (gcd % %2)))]
      (reduce lcm s))))

(defcheck solution-9c2cd947
  (fn common-mul [& rest]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))]
      (reduce (fn [acc e] (/ (* acc e) (gcd acc e))) rest))))

(defcheck solution-9c37adc2
  (fn [n & more]
    (loop [x n]
      (if (= more (filter (fn [y] (= 0 (mod x y))) more))
        x
        (recur (+ x n))))))

(defcheck solution-9c8581bb
  (fn [& args]
    (let [m (first args)]
      (->> (iterate #(+ % m) m)
        (filter (fn [n]
                  (every? identity
                    (for [x (rest args)]
                      (integer? (/ n x))))))
        first))))

(defcheck solution-9cc9fa1e
  (fn varags-lcm [& args]
    (let [gcd (fn [x y]
                (loop [a x b y]
                  (if (zero? b)
                    a
                    (recur b (rem a b)))))
          lcm (fn [x y]
                (/ (* x y) (gcd x y)))]
      (loop [acc (first args) v (rest args)]
        (if (empty? v)
          acc
          (recur (lcm acc (first v)) (rest v)))))))

(defcheck solution-9d017fb7
  (fn lcm [a & args] ; assume at least 2
    (letfn [
            (in-increasing-seq? [x s]
              (or
               (= x (first s))
               (and
                (> x (first s))
                (in-increasing-seq? x (rest s)))))
            (multiples [n]
              (iterate (partial + n) n))]
      (first
        (filter
          (fn [x]
            (every? #(= % true)
              (map (partial in-increasing-seq? x)
                (map multiples args))))
          (multiples a))))))

(defcheck solution-9deb85da
  (fn lcm [& a]
    (let [original_v (vec a)]
      (loop [multiples (vec a)]
        (if (apply = multiples) (first multiples)
                                (let [index_smallest_multiple (.indexOf multiples (first (sort multiples)))]
                                  (recur (update-in multiples [index_smallest_multiple]
                                           #(+ (original_v index_smallest_multiple)
                                               %)))))))))

(defcheck solution-9ea7e3d3
  (fn lcm [& s]
    (if (empty? (rest s))
      (first s)
      (apply lcm
        (cons
          (
           (fn [a b]
             (/
               (* a b)
               ((fn gcd [a b]
                  (if (zero? b) a (gcd b (mod a b)))
                  ) a b)
               )
             )
           (first s) (fnext s)
           )
          (nnext s)
          )
        )
      )
    ))

(defcheck solution-9f05236a
  (fn [& args]
    (loop [m (apply max args) x m]
      (if (every? #(= 0 %) (map #(mod x %) args)) x
                                                  (recur m (+ x m))))))

(defcheck solution-9f105238
  (fn lcm [& nums]
    (let [nums (sort nums)
          m (first nums)]
      (loop [nums (rest nums)
             lcm m]
        (if (empty? (filter #(not= 0 (rem lcm %)) nums))
          lcm
          (recur nums (+ m lcm)))))))

(defcheck solution-9f21a4c3
  (fn least-common-multiple [& nums]
    (/ (apply * nums)
      (reduce #(if (= 0 %2)
                 %
                 (recur %2 (mod % %2))) nums))))

(defcheck solution-9f686816
  (fn [& nums]
    (let [skip-jack (fn [colls]
                      (if (= 1 (count (group-by first colls)))
                        (first (first colls))
                        (let [biggest-smallest (reduce max (map first colls))]
                          (recur (map #(drop-while (fn [n] (< n biggest-smallest)) %) colls)))))]
      (skip-jack (map (fn [n] (iterate (partial + n) n)) nums)))))

(defcheck solution-9f7735c9
  (fn lcm [ & args ]
    (letfn [ (gcd [ m n ]
               (if (= 0 n)
                 m
                 (recur n (mod m n))))
            (lcm [ m n ]
              (/ (* m n) (gcd m n))) ]
      (reduce lcm args))))

(defcheck solution-9f8ce4f1
  (fn ppcm
    ([a] a)
    ([a b]
     (loop [c a d b]
       (cond
         (> c d) (recur c (+ b d))
         (< c d) (recur (+ a c) d)
         :else c)))
    ([a b & r] (ppcm (ppcm a b) (apply ppcm r)))
    ))

(defcheck solution-9fdd41f6
  (fn lcm [& args]
    (let [multiplier (apply min args)]
      (first (first (filter #(every? zero? (second %))
                      (map (fn [numerator]
                             [numerator (map #(rem numerator %) args)])
                        (iterate #(+ multiplier %) multiplier))))))))

(defcheck solution-9ff38cb1
  (fn [& x]
    (let [g #(if (zero? %2) % (recur %2 (mod % %2)))
          l #(/ (* % %2) (g % %2))]
      (reduce l x))))

(defcheck solution-9ff7c0dd
  #(reduce (fn [a b] (/ (* a b) (loop [a a b b]
                                  (if (zero? b) a (recur b (rem a b)))))) %&))

(defcheck solution-a05aa51e
  (fn f [& xs]
    (let [g (fn [n mult]
              (if-let [lcm (first (apply clojure.set/intersection mult))]
                lcm
                (recur (inc n) (map conj mult (map #(* n %) xs)))))]
      (g 1 (repeat (count xs) #{})))))

(defcheck solution-a0f6946b
  (fn [& nums]
    (let [largest (apply max nums)
          others (remove #(= largest %) nums)]
      (loop [i 1 r (* i largest)]
        (if (every? #(zero? (rem r %)) others)
          r
          (recur (inc i) (* (inc i) largest)))))))

(defcheck solution-a1168f73
  (fn [& l]
    (let [m (reduce min l)]
      (first
        (for [x (iterate #(+ m %) m)
              :when (= 0 (reduce #(+ %1 (rem x %2)) 0 l))]
          x)))))

(defcheck solution-a1351973
  (fn [a b & others]
    (let [f (fn gcd[a b] (let [isabig? (>= a b)
                               x (if isabig? a b)
                               y (if isabig? b a)]
                           (loop [x x
                                  y y]
                             (let [remainder (rem x y)]
                               (if (= remainder 0)
                                 y
                                 (recur y remainder))))))]

      (let [lcm (fn [x y] (/ (* x y) (f x y)))]
        (loop [result (lcm a b)
               others others]
          (if (empty? others)
            result
            (recur (lcm result (first others)) (next others))))))))

(defcheck solution-a1c1c34e
  (fn
    [& args]
    (let [sorted (sort args)
          smallest (first sorted)
          divisors (rest sorted)
          all-divisible? (fn [possible-multiple]
                           (every? zero? (map #(mod possible-multiple %) divisors)))]
      (loop [multiplier 1]
        (if (all-divisible? (* multiplier smallest))
          (* multiplier smallest)
          (recur (inc multiplier)))))))

(defcheck solution-a1e990bd
  (fn [& args]
    (let [[least & more] (sort args)]
      (some #(when (every? (fn [x] (== 0 (mod % x))) more) %)
        (map * (iterate inc 1) (repeat least))))))

(defcheck solution-a2465021
  (fn [& args]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))))
          lcm (fn [a b]
                (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-a27fd002
  (fn [& z] (letfn [(gcd [x y]
                      (cond
                        (= x y) x
                        (> x y) (recur (- x y) y)
                        :else (recur x (- y x))))]
              (reduce #(/ (* %1 %2) (gcd %1 %2)) z))))

(defcheck solution-a2e6b21c
  (fn l
    ([x y]
     (letfn [(g [x y] (if (= 0 y) x (g y (mod x y))))]
       (/ (* x y) (g x y))))
    ([x y z & r] (apply l (l x y) (l x z) r))))

(defcheck solution-a2e74c66
  (fn [ & l] (loop [b (apply min l) x b] (if (every? #(= 0 (mod x %)) l) x (recur b (+ x b))))))

(defcheck solution-a33f2be3
  (letfn [(min-index [coll] (apply min-key (partial nth coll) (range (count coll))))]
    (fn lcm [& args]
      (loop [x (vec args)]
        (if (apply = x) (first x)
                        (let [i (min-index x)]
                          (recur (assoc x i (+ (x i) (nth args i))))))))))

(defcheck solution-a3562c60
  (fn [& args]
    (let [
          gcd (fn [x y] (if (zero? y) x (recur y (mod x y))))
          lcm (fn [x y] (/ (* x y)(gcd x y)))]
      (reduce lcm args)

      )
    ))

(defcheck solution-a3c9e063
  (fn [& args]
    (let [gcd-func (fn [a b]
                     (if (< a b)
                       (recur b a)
                       (if (= 0 (mod a b))
                         b
                         (recur b (mod a b)))))
          gcd (reduce gcd-func args)]
      (/ (reduce * args) gcd))))

(defcheck solution-a45e2e9c
  (fn [& l]
    (let [gcd (fn g [a b] (if (= b 0) a (g b (rem a b))))
          lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (loop [l l
             acc (lcm (first l) (second l))]
        (if (empty? l)
          acc
          (recur (rest l)
            (lcm acc (first l))))))))

(defcheck solution-a51c3e48
  (fn [& args]
    (let [iscm? (fn [coll] (= 1 (count (distinct (map first coll))))) ;; first element of each sequence are all the same
          move  (fn [coll] (let [m ( apply min (map first coll))]
                             (map #(if (= m (first %))
                                     (rest %)
                                     %) coll)))]
      (loop [multiples (map #(map (fn [x] (* x %)) (drop 1 (range))) args)]
        (if (iscm? multiples)
          (first (first multiples))
          (recur (move multiples) ))))))

(defcheck solution-a533d154
  #(reduce
     (fn [a b]
       (letfn [(gcd [x y] (if (= y 0) x (gcd y (rem x y))))]
         (/ (* a b) (gcd a b))))
     %&))

(defcheck solution-a5bda42e
  (fn lcm [& v]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) v))))

(defcheck solution-a5fb7045
  (fn mult-lcm [a b & numbers]
    (letfn
     [(abs [x] (max x (* -1 x)))
      (gcd [a b] (if (zero? b) a (recur b (rem a b))))
      (lcm [a b] (/ (abs (* a b)) (gcd a b)))]

      (loop [value (lcm a b) numbers numbers]
        (if (empty? numbers)
          value
          (recur (lcm value (first numbers)) (rest numbers))
          )
        )
      )
    ))

(defcheck solution-a65f2444
  #(reduce (fn [a b] (/ (* a b)
                       ((fn [x y] (if (zero? y) x (recur y (mod x y)))) a b))) %&))

(defcheck solution-a6ef0eb9
  (fn lcm [& lst]
    (let [mx (apply max lst)
          finished (fn [x] (not (every? identity (map #(= 0 (mod x %)) lst))))]
      (+ mx (last (take-while finished (iterate #(+ mx %) mx))))
      )
    ))

(defcheck solution-a6faf0a6
  #(reduce (fn lcn[a b]
             (loop [x a y b acc #{}] (
                                       if (some #{(+ x a)} acc)
                                       (+ x a)
                                       (if (some #{(+ y b)} acc)
                                         (+ y b)
                                         (recur (+ x a) (+ y b) (concat acc #{(+ x a)} #{(+ y b)}))
                                         )
                                       ))

             )

     %&))

(defcheck solution-a72fd0bc
  (fn [& args]
    (let [gcd (fn f [x y] (if (= y 0) x (f y (mod x y))))
          lcm (fn f [x y] (/ (* x y) (gcd x y)))]
      (reduce lcm args) )))

(defcheck solution-a767d73c
  (fn [& args]
    (reduce (fn [x y]
              (first (drop-while
                       #(not (zero? (rem % y)))
                       (iterate #(+ x %) x))))
      args)))

(defcheck solution-a812f222
  (fn kgv
    ([m n]
     (let [ggt (fn ggt [m n]
                 (if (= n 0)
                   m
                   (ggt n (mod m n))))]
       (/ (* m  n) (ggt m n))))
    ([m n & x]
     (if (or (nil? x) (empty? x))
       (kgv m n)
       (apply kgv (cons (kgv m n) x))))))

(defcheck solution-a81cc9c7
  (fn lcm [& args]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (reduce #(/ (* % %2) (gcd % %2)) args))))

(defcheck solution-a85769bc
  (fn [& a] (loop [x (apply min a) i x] (if (= 0 (reduce + (map (partial mod i) a))) i (recur x (+ x i))))))

(defcheck solution-a8632d85
  (letfn [
          (gcd [x y]
            (let [a (max x y)
                  b (min x y)
                  r (rem a b)]
              (if (zero? r)
                b
                (gcd b r))))
          (lcm [& xs]
            (/ (reduce * 1 xs) (reduce gcd xs)))]
    lcm))

(defcheck solution-a8aa9c3b
  (fn lcm
    ([a] a)
    ([a b]        (let [gcm
                        (fn gcm [a b]
                          (let [x (max a b)
                                y (min a b)]
                            (if (zero? y)
                              x
                              (recur y (mod x y)))))
                        ] (lcm (/ (* a b) (gcm a b)))))
    ([a b & more] (apply lcm (lcm a b) more))))

(defcheck solution-a8c2e34b
  (fn [& ms] (let [mseq (fn [m] (iterate #(+ m %) m))
                   seqs (fn [ms] (map mseq ms))
                   fsts (fn [xs] (map first xs))
                   done (fn [xs] (apply = (fsts xs)))
                   sotd (fn [xs] (sort-by first xs))
                   skip (fn [xs] (map-indexed #(if (zero? %1) (rest %2) %2) (sotd xs)))]
               (loop [xs (seqs ms)] (if (done xs) (ffirst xs) (recur (skip xs)))))))

(defcheck solution-a8e99a45
  (fn [& xs] (reduce (fn [a b] (letfn [(gcd [x y] (loop [i x j y] (if (= j 0) i (recur j (mod i j)))))] (/ (* a b) (gcd a b)))) xs)))

(defcheck solution-a9a72a25
  (letfn [
          (gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))
          (abs [x]
            (if (neg? x)
              (- x)
              x))]

    (fn lcm [& xs]
      (/ (abs (apply * xs)) (reduce gcd xs)))))

(defcheck solution-a9b88196
  (fn [& coll]
    (reduce
      #(loop [a %1 b %2]
         (if
          (zero? b)
           (/ (* %1 %2) a)
           (recur b (mod a b))))
      coll)))

(defcheck solution-a9d4beb4
  (fn [& nums]
    (letfn [(min-ind [xs] (apply min-key first (map vector xs (range))))
            (all-eq [[fx :as xs]] (when (every? #(= fx %) xs) xs))
            (next-iter [xs] (let [[m mi] (min-ind xs)] (concat (take mi xs) [(+ m (nth nums mi))] (drop (inc mi) xs))))
            ]
      (->> nums (iterate next-iter) (some all-eq) first))))

(defcheck solution-a9f50fe2
  (fn lcm [& xs]
    (letfn [(gcd [a b]
              (cond
                (= a b) a
                (> a b) (recur (- a b) b)
                :else (recur a (- b a))))
            (kk [a b]
              (/ (* a b) (gcd a b)))]
      (reduce kk xs))))

(defcheck solution-aa251438
  (fn [& ns] (apply min (apply clojure.set/intersection
                          (map (comp set (fn [n] (take 1000 (iterate #(+ % n) n)))) ns)))))

(defcheck solution-aa8eda38
  (letfn [(f [a b] (if (zero? a) b (f (mod b a) a)))]
    #(/ (apply * %&) (reduce f %&))))

(defcheck solution-aaf14995
  (fn lcm [& args]
    (/ (reduce * args)
      (reduce (fn [m n] (if (zero? n) m (recur n (mod m n)))) args))))

(defcheck solution-ab5d6a99
  (fn lcm [& more]
    (let [gcd (fn gcd[x y]
                (cond (= x y)
                      x
                      (> x y)
                      (gcd (- x y) y)
                      (< x y)
                      (gcd x (- y x))))
          poor-abs (fn [x]
                     (if (< x 0)
                       (* -1 x)
                       x))
          -lcm (fn[x y]
                 (/ (poor-abs (* x y)) (gcd x y)))]
      (reduce -lcm more))))

(defcheck solution-ab735be7
  (fn [& nums]
    (loop [curr nums]
      (if (apply = curr)
        (first curr)
        (recur (map-indexed #(if (< %2 (apply max curr)) (+ %2 (nth nums %1)) %2) curr))))))

(defcheck solution-ac28778d
  (fn lcm
    ([x y] (/ (* x y) ((fn gcd [z w]
                         (if (> w z)
                           (gcd w z)
                           (if (zero? (mod z w))
                             w
                             (gcd w (mod z w)))))
                       x y)))
    ([x y & more] (reduce lcm (lcm x y) more))))

(defcheck solution-ac487115
  (fn [a & b]
    (first
      (filter
        (fn [v] (every? #(= 0 (mod v %)) b))
        (range a 2147483647 a)))))

(defcheck solution-acb7ceab
  (fn [& ns] (loop [N (vec ns) R N] (if (apply == R) (R 0) (recur N (let [i (.indexOf R (apply min R))] (assoc R i (+ (R i) (N i)))))))))

(defcheck solution-acd86d4f
  (fn lcm [& ns]
    (letfn [(gcd [x y] (loop [a x b y] (if (= b 0) a (recur b (mod a b)))))
            (lcm2 [a b] (* b (/ a (gcd a b))))]
      (reduce lcm2 ns))))

(defcheck solution-ad8fee89
  (fn [a & s] (some #(when (every? (fn [e] (zero? (rem % e))) s) %) (iterate (partial + a) a))))

(defcheck solution-add4f3b7
  (fn lcmany
    ([a] a)
    ([a & r]
     (let [gcd2 (fn [a b] (if (= b 0) a (recur b (mod a b))))
           lcm2 (fn [a b] (/ (* a b) (gcd2 a b)))]
       (apply lcmany (cons (lcm2 a (first r)) (rest r)))
       ))))

(defcheck solution-ae1d6470
  (fn [& nums]
    (letfn [(gcd
              ([n m] (if (zero? n) m (recur (rem m n) n)))
              ([n m & more] (reduce gcd (gcd n m) more)))
            (lcm
              ([n m] (->> (gcd n m) (/ n) (* m)))
              ([n m & more] (reduce lcm (lcm n m) more)))]
      (apply lcm nums))))

(defcheck solution-ae282326
  (fn [& args]
    (let [lss (fn lss
                ([x]          [(first x)])
                ([x y]        (cond ((complement sequential?) x) (lss y)
                                    ((complement sequential?) x) (lss x)
                                    (= (first x) (first y))      (cons (first x) (lazy-seq (lss (rest x) (rest y))))
                                    (< (first x) (first y))      (lazy-seq (lss (drop-while (partial > (first y)) x) y))
                                    (< (first y) (first x))      (lazy-seq (lss (drop-while (partial > (first x)) y) x))))
                ([x y z] (lss x (lss y z)))
                ([w x y z] (lss (lss w x) (lss y z))))
          multiples (fn multiples
                      [x]
                      (map (partial * x) (range 1 ##Inf)))]
      (first(apply lss (map multiples args))))))

(defcheck solution-ae486d23
  #(letfn [(gcd [a b]
             (if (zero? b)
               a
               (recur b (mod a b))))
           (lcm [c d]
             (/ (* c d) (gcd c d)))]

     (reduce lcm %&)))

(defcheck solution-aeabd232
  (let [llcm
        (fn tst [x] (if (every? #(= (first x) %) x)
                      (key (first x))
                      (tst
                        (merge
                          (sorted-map (+ (key (first x)) (val (first x)))
                            (val (first x)))
                          (drop 1 x)))))]
    (let [process
          (fn proc [x]
            (if (= 2 (count x))
              (llcm x)
              (proc (merge (hash-map (llcm (take 2 x))
                             (llcm (take 2 x)))
                      (drop 2 x)))))]

      (fn lcm
        [x & args]
        (process
          (apply merge
            (map #(sorted-map % %) (cons x args))))))))

(defcheck solution-af38eebb
  (fn dolcm [& r]
    (letfn [(lcm [a & rst]
              (/ (reduce * a rst)
                (reduce gcd1 a rst)))
            (gcd1 [a b]
              (let [aa (max a b)
                    bb (min a b)]
                (if (zero? b)
                  a
                  (recur b (mod a b)))))]
      (apply lcm r))))

(defcheck solution-af83021f
  (fn lcm [& xs]
    (let [largest (apply max xs)]
      (->> (for [x (iterate #(+ largest %) largest)]
             [(set (map #(mod x %) xs)) x])
        (filter #(= (first %) #{0}))
        first
        second))))

(defcheck solution-afbca38
  (fn [a & b]
    (reduce
      #(/ (* %1 %2)
         (loop [a %1 b %2]
           (let [n (min a b)
                 x (max a b)]
             (if (zero? n)
               x
               (recur (- x n) n)))))
      a b)))

(defcheck solution-b061b910
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m)
                  b
                  (recur b m))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-b0757987
  (fn [& ns] (letfn [
                     (gt [n] #(> % n))
                     (lt [n] #(< % n))
                     (first-numbers-equal? [colls] (apply = (map first colls)))
                     (max-first-number [colls] (apply max (map first colls)))
                     (fast-forward [colls] (map #(drop-while (lt (max-first-number colls)) %) colls))
                     (fast-lazy-search [colls] (if (first-numbers-equal? colls) (max-first-number colls) (fast-lazy-search (fast-forward colls))))
                     (lazy-searching [& colls] (fast-lazy-search colls))
                     (multiples [n] (map #(* n %) (iterate inc 1)))
                     (lcm [& ns] (apply lazy-searching (map multiples ns)))
                     ] (apply lcm ns))))

(defcheck solution-b0a143e5
  (fn [& input]
    ((fn lcm [l]
       (if (apply = (flatten (map #(first %) l)))
         (first (first l))
         (let [lr (sort #(compare (first %1) (first %2)) l)]
           (lcm (conj (rest lr) (vector (+ (first (first lr)) (second (first lr))) (second (first lr)))))))
       )
     (map #(vector % %) input))))

(defcheck solution-b0f9027b
  (fn [& n]
    (letfn [(lcm [a b] (/ (* a b) (gcd a b)))
            (gcd [a b] (cond (zero? b) a :else (gcd b (mod a b))))]
      (reduce lcm n))))

(defcheck solution-b1cdcaf
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))
            (gcd2 [xs]
              (reduce #(gcd %1 %2) xs))]
      (/ (apply * xs) (gcd2 xs)))))

(defcheck solution-b1cff595
  (fn [& args]
    (letfn [(gcd [a b] (let [c (rem (max a b) (min a b))]
                         (if (= c 0) (min a b) (recur (min a b) c))))]
      (/ (apply * args) (reduce gcd args)))))

(defcheck solution-b1ed53ff
  (fn least-common-multiple [& nums]
    (let [multiple-of-all? (fn [n factors]
                             (every? #(zero? (rem n %)) factors))
          ns (sort > nums)
          mx (first ns)]
      (first (filter #(multiple-of-all? % ns) (iterate #(+ mx %) mx))))))

(defcheck solution-b27792af
  (fn [& args]
    (let [biggest (apply max args)
          factor? (fn [factor args']
                    (every? #(zero? (mod factor %)) args'))]
      (first (drop-while #(not (factor? % args)) (iterate #(+ % biggest) biggest))))))

(defcheck solution-b306276c
  (fn [& args]
    (let [smallest (first (sort args))]
      (loop [multiple 1]
        (if (reduce (fn [bol arg] (and bol (= 0 (mod (* multiple smallest) arg)))) true args)
          (* multiple smallest)
          (recur (+ multiple 1)))))))

(defcheck solution-b326a1d3
  (fn [& nums]
    (->> (apply max nums)
      ((fn [m] (iterate #(+ m %) (+ m m))))
      (filter (fn [candidate] (every? #(zero? (mod candidate %)) nums)))
      first)))

(defcheck solution-b3d2b451
  (fn lcm
    [& s]
    (letfn [(gcd [a b]
              (if (= a b)
                a
                (if (> a b)
                  (recur (- a b) b)
                  (recur a (- b a)))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) s))))

(defcheck solution-b45ea6e3
  (fn [& a] (reduce #(/ (* % %2) ((fn f [[x y]] (if (= x 0) y
                                                            (f (sort [(- y x) x])) ))
                                  (sort [% %2]))) a)))

(defcheck solution-b4653aaa
  (fn [& args]
    (let [n (count args)
          mp (into {} (map vec (partition 2 (interleave (range n) args))))
          ]
      (letfn [(fr [ags0 ags]
                (let [nags (sort-by #(second %) ags)]
                  (if (= (second (first nags)) (second (last nags))) (second (first nags))
                                                                     (recur ags0 (cons [(first (first nags)) (+ (second (first nags)) (ags0 (first (first nags))))]
                                                                                   (rest nags)) )
                                                                     ) )
                )]
        (fr mp mp)
        ))
    ))

(defcheck solution-b4a828d1
  (fn [& xs]
    (let [c (apply min xs)]
      (loop [n c i 0]
        (cond
          (>= i (count xs)) n
          (== (rem n (nth xs i)) 0) (recur n (inc i))
          :else (recur (+ n c) 0))
        ))))

(defcheck solution-b4d307d7
  #(reduce (fn [a b] (loop [p a q b] (
                                       cond (= p q) p
                                            (< p q) (recur (+ p a) q)
                                            :e (recur p (+ q b))))) %&))

(defcheck solution-b58c4059
  (fn [& args]
    (if (some zero? args)
      0
      (let [GCD (fn [a b]
                  (cond
                    (or (zero? a) (zero? b)) 0
                    (= 1 a) 1
                    (= 1 b) 1
                    (zero? (mod a b)) b
                    :else (recur b (mod a b))))]
        (reduce (fn [acc x] (/ (* acc x) (GCD acc x))) 1 args)))))

(defcheck solution-b5ad627a
  (fn [& x]
    (reduce
      #(loop [y %1]
         (if (= 0 (mod y %2))
           y
           (recur (+ y %1))))
      x)))

(defcheck solution-b5c55166
  (fn [& args]
    (/ (apply * args)
      (reduce (fn [a b]
                (loop [a a b b] ;; gcd
                  (if (zero? b) a
                                (recur b (rem a b)))))
        args))))

(defcheck solution-b683391f
  (fn [& ns]
    (let [mx (apply max ns) mn (apply min ns)]
      (loop [i (* mn (int (/ mx mn)))]
        (if (every? #(zero? (rem i %)) ns)
          i
          (recur (+ i mn)))))))

(defcheck solution-b72ea784
  (fn [& n]
    (apply min (apply clojure.set/intersection (reduce #(conj %1 (set (map (fn[el] (* el %2)) (range 1 1000)))) '() n)))
    ))

(defcheck solution-b741c891
  (letfn [(multiples [n]
            (iterate #(+ n %) n))
          (least-common [seqs]
            (if (apply = (map first seqs))
              (first (first seqs))
              (let [[least & more] (sort-by first seqs)]
                (least-common (conj more (drop 1 least))))))]
    (comp least-common
          (partial map multiples)
          list)))

(defcheck solution-b747b064
  (fn [& nums]
    (loop [series nums]
      (if (apply = series)
        (first series)
        (let [m (apply min series), f (fn [x y] (if (= x m) (+ x y) x))]
          (recur (map f series nums)))))))

(defcheck solution-b79e6211
  (fn [& s]
    (reduce
      (fn [x y]
        (let [gcd (fn [x y]
                    (if (zero? y)
                      x
                      (recur y (rem x y))))]
          (/ (* x y ) (gcd x y))))
      s)))

(defcheck solution-b7fda400
  (fn [& r]
    (let [v (zipmap (range (count r)) r)
          A apply
          F first
          S second
          Q (fn L [& m]
              (let [[h & t :as a] (sort-by S m)]
                (if (A = (map S a))
                  (S h)
                  #(A L (conj t [(F h) (+ (S h) (v (F h)))])))))]
      (trampoline #(A Q %) v))))

(defcheck solution-b84d5594
  (fn [& v]
    (let [ gcd (fn  [a b]
                 (if (zero? b)
                   a
                   (recur b, (mod a b))))
          lcm  (fn [a b]
                 (/ (* a b) (gcd a b)))
          ]
      (reduce lcm v)
      )
    ))

(defcheck solution-b8a24f8c
  (fn [& a]
    (let [[b & s] (reverse (sort a))
          r (iterate (partial + b) b)]
      (first (filter
               (fn [v] (every?
                         #(zero? (rem v %))
                         s))
               r)))))

(defcheck solution-b8cb211f
  (fn p100 [& ns]
    (letfn [(nn [rto]
              (if (integer? rto) [rto 1]
                                 ((fn v [x i] (if (integer? (* x i)) [(* x i) i] (v x (inc i)))) rto 2)))]
      (let [vs (map nn ns)
            dm  (apply * (map second vs))
            ls (map (fn [x] (if (= 1 (second x)) (first x) (* dm (/ (first x) (second x)) )))  vs)
            gcm (first (filter (fn [y] (every? identity (map #(zero? (mod % y)) ls))) (for [i (range (apply min ls) 0 -1)] i)))
            ]
        (/ (apply * ls) dm gcm)))))

(defcheck solution-b8e3e8ae
  (fn [& rs]
    (letfn [(gcd [n m]
              (if (= n m)
                n
                (if (> n m)
                  (gcd m (- n m))
                  (gcd n (- m n)))))
            (lcm [n m]
              (/ (* n m) (gcd n m)))]
      (reduce lcm rs))))

(defcheck solution-b969b094
  (fn lcm
    ([x] x)
    ([x y]
     (letfn [(gcd [x y] (if (zero? y) x (recur y (mod x y))))]
       (/ (* x y) (gcd x y))))
    ([x y & xs] (reduce lcm (into [x y] xs)))))

(defcheck solution-b97ccda9
  (fn [& col]
    (let [fir (first col)
          ocol (rest col)]
      (loop [i 1 s (* i fir)]
        (if (= 0 (apply + (map #(rem s %) ocol))) s  (recur (inc i) (* i fir)))))))

(defcheck solution-b9a4c16
  (fn [& l]
    (letfn
     [(gcd [x y]
        (if
         (= x y)
          x
          (if
           (< x y)
            (recur x (- y x))
            (recur y (- x y)))))
      (lcm [x y]
        (/ (* x y) (gcd x y)))]
      (reduce lcm l))))

(defcheck solution-b9b4ddf
  (fn lcm
    ([a b]
     (letfn [
             (gcd [a b]
               (if (zero? b) a (gcd b (mod a b))))]
       (/ (* a b) (gcd a b))))

    ([a b & xs]
     (reduce lcm (lcm a b) xs))))

(defcheck solution-ba0928c
  (fn[& args] (let [gcd #(if (zero? %2) %1 (recur %2 (mod %1 %2)))
                    lcm #(/ (* %1 %2) (gcd %1 %2))]
                (reduce lcm args))))

(defcheck solution-ba2a070f
  (fn [& xs]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-ba504b2e
  (fn lcm [& xs]
    (let [[f & r] xs]
      (->> (iterate #(+ f %) f)
        (filter (fn [n] (every? #(= 0 (mod n %)) r)))
        first))))

(defcheck solution-ba6a5f80
  (fn lcm
    ([x] x)
    ([x y & z]
     (apply (partial lcm (/ (* x y) ( (fn my-gcd
                                        [x y]
                                        (loop [m (max x y) n (min x y)]
                                          (if (zero? (rem m n))
                                            n
                                            (recur n (rem m n))))) x y))) z))))

(defcheck solution-ba84548f
  (fn [e & f]
    (letfn [(gcd [a b] (loop [c a d b]
                         (if (= d 0) c
                                     (recur d (rem c d)))))
            (lcm2 [x y] (/ (* x y) (gcd x y)))]
      (let [cf (count f)]
        (loop [g e num 0]
          (if (= num cf) g
                         (recur (lcm2 g (nth f num)) (inc num))))))))

(defcheck solution-baafd43e
  (fn ppcm [a b & args]
    (letfn [(gcd [x y] (if (= 0 y) x (gcd y (mod x y))))]
      (let [res (/ (* a b)
                  (gcd a b))]
        (if (empty? args)
          res
          (apply ppcm (conj args res)))))))

(defcheck solution-bb603565
  (letfn [(in-lists? [x lists]
            (if-let [[list & more] (seq lists)]
              (and (= x (first (drop-while #(< % x) list)))
                   (in-lists? x more))
              true))]
    (fn [& xs]
      (let [[source & sinks] (for [x xs]
                               (iterate #(+ % x) x))]
        (first (filter #(in-lists? % sinks) source))))))

(defcheck solution-bba54397
  (fn [& vs]
    (loop [xs (map vector vs vs)]
      (if (every? #(= (first (first xs)) (first %)) (rest xs)) (first (first xs))
                                                               (let [xss (sort #(< (first %1) (first %2)) xs)
                                                                     p (first xss)]
                                                                 (recur (concat [[(+ (first p) (second p)) (second p)]] (rest xss))))))))

(defcheck solution-bbae9a95
  (fn [& nums]
    (letfn [(multiples [n]
              (map (partial * n) (rest (range))))

            (pop-smallest [colls]
              (let [sorted-colls (sort-by first < colls)
                    pop rest]
                (cons (pop (first sorted-colls)) (rest sorted-colls))))

            (smallest-match [colls]
              (if (apply = (map first colls))
                (first (first colls))
                (recur (pop-smallest colls))))]

      (smallest-match (map multiples nums)))))

(defcheck solution-bbf1d75d
  (fn [& args]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-bbf9f54f
  (fn lcm
    ([a b] (letfn [(gcd [a b] (if (= b 0) a (recur b (mod a b))))]
             (/ (* a b) (gcd a b))))
    ([a b & more] (reduce lcm (lcm a b) more))))

(defcheck solution-bc0a6e82
  (fn [& ns]
    (let [step (apply max ns)
          pred #(every? zero? (map (partial mod %) ns))]
      (first (filter pred (iterate (partial + step) step))))))

(defcheck solution-bcbd961d
  (fn lcm
    [& others]
    (let [[a b] (take 2 others)
          cur-lcm (/ (* a b)
                    ((fn gcd [& nums]
                       (let [[y x] (sort nums)
                             r (rem x y)]
                         (if (zero? r) y (recur [y r])))) a b))]
      (if (< (count others) 3)
        cur-lcm
        (recur (cons cur-lcm (drop 2 others)))))))

(defcheck solution-bcc55d8
  (fn [& n]
    (loop [m (zipmap n n)]
      (if (= 1 (count (set (vals m))))
        (first (vals m))
        (let [min-key (first (reduce (fn [x y] (if (< (val x) (val y)) x y)) m))]
          (recur (update-in m [min-key] #(+ % min-key))))))))

(defcheck solution-bcceb840
  (let [multiples (fn multiples
                    ([n] (multiples n n))
                    ([n m] (cons (+ n m) (lazy-seq (multiples n (+ n m))))))
        lcm (fn lcm [multiples-list]
              (if (apply = (map first multiples-list))
                ((comp first first) multiples-list)
                (lcm ((fn [[[mult-1] [& rest-mults]]]
                        (cons (rest mult-1) rest-mults))
                      (split-at 1 (sort-by first multiples-list))))))]
    #(lcm (map multiples %&))))

(defcheck solution-bd5187a9
  (fn lcms [& x]
    (letfn [(gcd [a b]
              (if (= b 0 )
                a
                (gcd b (mod a b))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce #(lcm % %2) x))))

(defcheck solution-bd5277be
  (fn lcm
    ([x y] (first (filter #(= 0 (rem % y)) (iterate (partial + x) x))))
    ([x y & args] (apply lcm (lcm x y) args))))

(defcheck solution-be564b98
  (fn lcm [a b & cs]
    (letfn [(gcd [x y]
              (if (zero? y) x
                            (gcd y (mod x y))))]
      (if (not (number? b))
        a
        (apply lcm (flatten (concat
                             [(* (/ a (gcd a b)) b) (first cs)]
                             (rest cs))))))))

(defcheck solution-be7a9c7a
  (fn lcm [& xs]
    (let [gcd-2-fn (fn gcd [a b] (if (> a b) (gcd b a) (if (zero? (mod b a)) a (gcd (mod b a) a))))
          r-fn (fn [r-lcm x] (/ (* r-lcm x) (gcd-2-fn r-lcm x)))]
      (reduce r-fn xs))))

(defcheck solution-bec8b1b8
  (fn [g & x] (/ (apply * x) (reduce g x))) #(if (= 0 %2) % (recur %2 (mod % %2))))

(defcheck solution-bef57707
  (fn [& s]
    (letfn [ (next-iter [v orig-v]
               (let [mv (apply min v)
                     iv (.indexOf v mv)]
                 (assoc v iv (+ mv (nth orig-v iv)) )))]
      (loop [v (vec s) orig-v (vec s)]
        (if (every? #(= (first v) %) v) (first v)
                                        (recur (next-iter v orig-v) orig-v)))
      )
    ))

(defcheck solution-bf528d76
  (fn x [& args]
    (let [gcd (fn [a b]
                (if (=  b 0)
                  a
                  (recur b (mod a b))))
          lcm (fn [a b]
                (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-bf5a0cb0
  (fn [& nums]
    (let [reducer #(loop [a %1 b %2]
                     (cond
                       (= a b) a
                       (< a b) (recur (+ a %1) b)
                       :else (recur a (+ b %2))))]
      (reduce reducer nums))))

(defcheck solution-bf6b6c55
  (fn lcm [h & t]
    (if (nil? t)
      h
      (let [
            f (fn f [a b] (if (= b 0) a (f b (rem a b))))
            r (apply lcm t)
            a (max h r)
            b (min h r)
            ] (/ (* a b) (f a b))))))

(defcheck solution-bfdc2ffd
  (fn [& xs]
    (/ (apply * xs)
      (reduce #(if (zero? %2) % (recur %2 (mod % %2))) xs))))

(defcheck solution-c097aeb6
  (fn [& xs]
    (letfn [(gcd [x y]
              (if (zero? y)
                x
                (gcd y (mod x y))))
            (lcm [x y]
              (/ (* x y)
                (gcd x y)))]
      (reduce lcm xs))))

(defcheck solution-c0d65102
  #(loop [xs (zipmap %& %&)]
     (if (apply = (vals xs))
       (first (vals xs))
       (let [least (key (apply min-key val xs))]
         (recur (assoc xs least (+ (xs least) least)))))))

(defcheck solution-c142dc89
  (fn [& args]
    (letfn [(gcd [x y]
              (if (zero? y) x (recur y (mod x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-c14609ed
  (fn [& args]
    (let [gcd #(if (zero? %2) % (recur %2 (rem % %2)))]
      (reduce
        #(/ (* % %2) (gcd % %2))
        args))))

(defcheck solution-c14fe709
  (fn [& xs] (/ (reduce * xs)
               (reduce #(if (zero? %2) % (recur %2 (mod % %2)))
                 (sort-by identity > xs)))))

(defcheck solution-c166f7a5
  (fn [& numbers]
    (let [gcd (fn gcd# [a b]
                (cond
                  (= a b) a
                  (> a b) (gcd# (- a b) b)
                  (< a b) (gcd# a (- b a))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) (first numbers) numbers))))

(defcheck solution-c1c11763
  (fn [& xs]
    (let [gcd (fn [a b](if (zero? b) a (recur b (mod a b))))
          lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm xs))))

(defcheck solution-c1fdbbb2
  (fn [& v]
    (letfn [(g [& s]
              (let [[y x] (sort s)
                    m (mod x y)]
                (if (= m 0) y (g y m))
                )
              )]
      (reduce #(/ (* % %2) (g % %2))  v)
      )
    ))

(defcheck solution-c2dc8aa6
  (fn lcm([a b](letfn[(gcd [a b](if (zero? b) a (gcd b (mod a b))))](/ (* a b) (gcd a b))))
    ([a b & c](reduce lcm (lcm a b) c))))

(defcheck solution-c373aebf
  (fn pr100
    ([m n]
     (if (> n m)
       (recur n m)
       (letfn [(gcd [m n] ;m>=n
                 (if (zero? n)
                   m
                   (recur n (rem m n))))]
         (/ (* m n) (gcd m n)))))
    ([m n & nums]
     (reduce pr100 (pr100 m n) nums))))

(defcheck solution-c3ab6956
  (fn [& xs] (reduce
               (fn [a b]
                 (/ (* a b)
                   (loop [q a r b]
                     (if( zero? r) q (recur r (mod  q r ))))))
               (seq xs)) ))

(defcheck solution-c3f2bd63
  (fn lcm [& xs]
    (let [gcd (fn [a b]
                (if (zero? b)
                  a
                  (recur b (mod a b))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-c3fb5669
  (fn [& args]
    (loop [nums args]
      (if (apply = nums)
        (first nums)
        (recur (map #(if (= (apply min nums) %) (+ % %2) %)
                 nums args))
        ))))

(defcheck solution-c459de47
  (fn [& vs-in] (let [gcd (fn [a b]
                            (if (= a b)
                              a
                              (if (> a b)
                                (recur (- a b) b)
                                (recur a (- b a))
                                )
                              )
                            )
                      lcm (fn [a b] (/ (* a b) (gcd a b)))
                      ]
                  (loop [vs (next vs-in) result (first vs-in)]
                    (if (nil? vs) result
                                  (recur (next vs) (lcm result (first vs)))
                                  )
                    ))
    ))

(defcheck solution-c463d6ed
  (fn lcm [x & xs]
    (letfn [(gcd [x y]
              (if (zero? y) x
                            (gcd y (mod x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) x xs))))

(defcheck solution-c47e31fa
  (fn lcm
    ([a b]
     (letfn [
             (gcd
               ([a b] (loop [x a y b]
                        (if (zero? (mod x y))
                          y
                          (recur y (mod x y)))))
               ([a b & c] (reduce gcd (gcd a b) c)))]
       (/ (* a b) (gcd a b))))
    ([a b & c] (reduce lcm (lcm a b) c))))

(defcheck solution-c53b8f15
  (fn [a & bs]
    (first
      (filter
        (fn [x]
          (every?
            (fn [y] (= 0 (rem x y)))
            bs))
        (map
          (fn [x] (* a x))
          (iterate inc 1))))))

(defcheck solution-c580ea1f
  (fn lcm [& nums] (let [all-same (fn [xs] (let [elem (first xs)] (reduce #(and %1 (= elem %2)) true xs)))
                         smallest (fn [xs] (reduce #(if (< %1 %2) %1 %2 ) xs))
                         inner-lcm (fn [xs ys] (if (all-same ys)
                                                 (first ys)
                                                 (recur xs (let [index-of-smallest (.indexOf ys (smallest ys))]
                                                             (vec (concat (subvec ys 0 index-of-smallest)
                                                                          (vector (+ (ys index-of-smallest) (xs index-of-smallest)))
                                                                          (subvec ys (inc index-of-smallest))))))))]
                     (inner-lcm (vec nums) (vec nums)))))

(defcheck solution-c5a70daa
  (fn [& nums]
    (let [gcd (fn [x y]
                (let [a (max x y)
                      b (min x y)
                      z (rem a b)]
                  (if (zero? z)
                    b
                    (recur b z))))
          lcm (fn [x y]
                (/ (* x y) (gcd x y)))]
      (reduce lcm nums))))

(defcheck solution-c5c06508
  (fn [& nums]
    (let
     [abs (fn [a] (if (< a 0) (- 0 a) a))
      gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
      lcm (fn [a b] (/ (abs (* a b)) (gcd a b)))]
      (reduce lcm nums))))

(defcheck solution-c672bcdd
  (fn lcm [& xs]( let [gcd (fn gcd [a b] (if (> b a ) (gcd b a)  (if (= b a) a (gcd b (- a b)))))]

                  (reduce #(/ (* %1 %2) (gcd %1 %2)) (first xs) (rest xs))

                  )))

(defcheck solution-c6767c6b
  (fn [& xs]
    (/ (apply * xs)
      (reduce #(if (zero? %2) % (recur %2 (mod % %2))) xs))))

(defcheck solution-c696dcb3
  (fn [& xs]
    (let [x (apply min xs) ys (filter #(not (= x %)) xs)]
      (loop [yss (map vector ys) n 1]
        (if (every? #(some (partial = (* x n)) %) yss)
          (* x n)
          (recur
            (map #(conj % (* n (first %))) yss)
            (inc n)))))))

(defcheck solution-c6fb877f
  (fn [& s](reduce
             (fn [a b](
                        loop [x a y b](cond
                                        (= x y) x
                                        (< x y) (recur (+ x a) y)
                                        (> x y) (recur x (+ y b))
                                        )
                                      )) s)))

(defcheck solution-c72d3ecc
  (fn [& nums]
    (apply min
      (apply
        clojure.set/intersection
        (map
          #(set (take 1000 (map (fn [x] (* x %))  (rest (range)))))
          nums)))))

(defcheck solution-c77a703b
  (fn lcm [a b & r]
    (let [gcd (fn gcd [x y] (if (zero? y) x (gcd y (mod x y))))]
      (if (seq r)
        (apply lcm (/ (* a b) (gcd a b)) r)
        (/ (* a b) (gcd a b))))))

(defcheck solution-c83bc473
  (fn lcm-100
    ([x y]
     (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))]
       (* x (/ y (gcd x y)))))
    ([x y & args]
     (apply lcm-100 (lcm-100 x y) args))))

(defcheck solution-c8b282a
  (fn aa [x1 & args]
    (loop [x x1 y 1]
      (if (every? #(= (rem (* x y) %) 0) args)
        (* x y)
        (recur x (inc y))

        )
      )
    ))

(defcheck solution-c8c7bcbe
  (fn [& a]
    (let [c (fn _ [R S] (let [[a & c] R [b & d] S]
                          (cond (= a b) (cons a (lazy-seq (_ c d)))
                                (< a b) (_ c S)
                                :else (_ R d))))]
      (->> a
        (map (fn [r] (map #(* r %) (range))))
        (reduce c)
        (second)))))

(defcheck solution-c93a9f
  (fn [& ns]
    (some (fn [m] (when (every? #(= (mod m %) 0) ns) m))
      (map #(* % (first ns)) (rest (range))))))

(defcheck solution-c94d5efe
  (fn lcm [& args]
    (let [impl (fn self [xs xms]
                 (if (apply == xms)
                   (first xms)
                   (let [x (apply min xms)
                         index (.indexOf xms x)
                         xinxs (nth xs index)
                         xm1s (assoc xms index (+ x xinxs))]
                     (self xs xm1s))))]
      (impl (apply vector args) (apply vector args)))))

(defcheck solution-c97375fe
  (fn [& n]
    (reduce #(/ (* %1 %2)
               (loop [a %1, b %2]
                 (if (zero? b) a (recur b (mod a b))))) n)))

(defcheck solution-c9863824
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (min x y)
                    b (max x y)]
                (if (zero? (mod b a))
                  a
                  (recur (mod b a) a)
                  )))
            (lcm [x y]
              (/ (* x y) (gcd x y)))]
      (reduce (fn [acc e] (lcm acc e)) args)
      )
    ))

(defcheck solution-c9931cbb
  (fn [& args]
    (letfn [(gcd [a b]
              (if (< a b)
                (recur b a)
                (if (zero? b) a (recur b (mod a b)))))]
      (reduce (fn [a b] (/ ( * a b) (gcd a b))) args))))

(defcheck solution-c994119
  (fn [x & xs]
    ((comp first (partial keep identity))(for [multiple (rest (map (partial * x) (range)))]
                                           (when (every? zero? (map (partial rem multiple) xs) ) multiple)
                                           ))))

(defcheck solution-c9c2de69
  #(letfn [(gcd [a b] (if (= 0 b) a (recur b (mod a b))))
           (lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-c9e50b7b
  (fn lcm [m & coll]
    (letfn [(gcd [m n]
              (if (zero? n)
                m
                (gcd n (mod m n))))
            (LCM [m n]
              (/ (* m n) (gcd m n)))]
      (if (empty? coll)
        m
        (recur (LCM m (first coll)) (rest coll))))))

(defcheck solution-ca0c7f04
  (fn [& a] (letfn
             [(align-seq  [[a b]] [(drop-while #(< % (first b)) a) b])
              (sync-seq   [[a b]] (if (= (first a) (first b)) [a b]
                                                              (sync-seq (align-seq (sort-by first [a b])))))
              (diff-first [s    ] (let [[[a & m] [b & n] & r] (partition-by first s)]
                                    (apply concat [a b] m n r)))
              (sync-seqs  [s    ] (if (apply = (map first s)) (first (first s))
                                                              (let [[a b] (split-at 2 (diff-first s))]
                                                                (sync-seqs (concat (sync-seq a) b)))))]
              (sync-seqs (map #(map (partial * %)
                                 (map inc (range)))
                           a)))))

(defcheck solution-ca1f5e39
  (fn [& args]
    (let
     [gcd (fn [a b] (if (= a 0) b (recur (mod b a) a)))
      lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-ca5dcf67
  (fn [n m & ms]
    (letfn [(gcd
              ([m n]
               (loop [m m n n]
                 (if (zero? n)
                   m
                   (recur n (mod m n)))))
              ([m n & ms]
               (reduce gcd (gcd m n) ms)))]
      (/ (apply * n m ms) (apply gcd n m ms)))))

(defcheck solution-ca61d88e
  (fn [& col]
    (let [start (apply min col)
          multiple? #(every? zero? (map (partial mod %) col))]
      (first (filter multiple? (iterate #(+ start %) start))))))

(defcheck solution-cac1f117
  (fn [& args] (reduce (fn [a b] (first (filter #(zero? (rem % b)) (iterate (partial + a) a)))) args)))

(defcheck solution-caded700
  (fn lcm [& nums]
    (letfn [(gcd [a b]
              (cond (< a b) (recur b a)
                    (zero? b) a
                    :else (recur b (rem a b)))
              )]
      (/ (apply * nums)
        (reduce gcd nums)))))

(defcheck solution-cbc12151
  (fn lcm2 [& args]
    (letfn [(lazylcmseq [col]
              (map #(lazy-seq (iterate (partial + %) %)) col))
            (lcm2 [[[a] :as lseq]]
              (let [lcmsort (sort-by first lseq)]
                (if (every? #(= % a)  (map first lcmsort))
                  a
                  (lcm2 (conj (rest lcmsort) (rest (first lcmsort)))))))]
      (lcm2 (lazylcmseq args)))))

(defcheck solution-cbc65b5f
  (fn [& args]
    (reduce  #(/ ( * % %2) ((fn common-divisor [num1 num2]
                              (cond (= 0 (rem num1 num2))
                                    num2
                                    (= 0 (quot num1 num2))
                                    1
                                    :else
                                    (common-divisor num2 (rem num1 num2)))) (max % %2) (min % %2))) args)))

(defcheck solution-cbf89abf
  (fn lcm
    [a b & args]
    (if (empty? args)
      (/
        ((fn abs [n] (max n (- n))) (* a b))
        ((fn gcd [x y]
           (let [larger (max x y) smaller (min x y)]
             (if (= 0 smaller)
               larger
               (gcd smaller (mod larger smaller))))) a b))
      (lcm a (apply (partial lcm b (first args)) (rest args))))))

(defcheck solution-cccb7a6c
  (fn [& xs]
    (letfn [(l-c-m
              ([ys os]
               (if (apply = ys)
                 (first ys)
                 (let [m (apply min ys)
                       [h t] (split-with #(not= % m) ys)]
                   (l-c-m (concat h [(+ (os (count h)) (first t))] (rest t)) os)))))]
      (l-c-m xs (vec xs)))))

(defcheck solution-cd29447c
  (fn [& args]
    (let [f #(reductions + (cycle [%]))]
      (first (sort (apply clojure.set/intersection
                     (for [nb args]
                       (into #{} (take 500 (f nb))))))))))

(defcheck solution-cd41e5b3
  (fn lcmv [& v] (reduce (fn lcm [a b]
                           (/ (* a b) ((fn gcd  [x y]
                                         (if (zero? y)
                                           x
                                           (recur y (mod x y)))) a b))) v)))

(defcheck solution-cd65d334
  (letfn [(lcm-int [[x & xs] candidates]
            (if xs
              (recur xs (filter #(= 0 (mod % x))
                          candidates))
              (first (filter #(= 0 (mod % x))
                       candidates))))
          (lcm [x & xs]
            (lcm-int xs (iterate (partial + x) x)))]
    lcm))

(defcheck solution-cdb01e53
  (fn [& s]
    (reduce #(/ (* %1 %2)
               ((fn [a b]
                  (if (= 0 b)
                    a
                    (recur b (mod a b)))) %1 %2)) s)))

(defcheck solution-cdba8d88
  (fn lcm [x & [y & ys]]
    (let [gcd (fn [a b]
                (if (= 0 b)
                  a
                  (recur b (mod a b))))]
      (if y
        (apply lcm (/ (* x y) (gcd x y)) ys)
        x))))

(defcheck solution-ce45a855
  (fn [& nums]
    (let [gcd (fn [a b]
                (cond
                  (= b 0) a
                  (= a 0) b
                  (> a b) (recur b (mod a b))
                  :else (recur a (mod b a))))]
      (reduce (fn [a b] (* (/ a (gcd a b)) b)) nums))))

(defcheck solution-ce71b15a
  (fn [& xs]
    (/ (apply * xs)
      (reduce #(if (zero? %2) %1 (recur %2 (mod %1 %2))) xs))))

(defcheck solution-cea3eb05
  (fn [& xs]
    (reduce (fn [ans n] (* ans (/ n (#(if (zero? %2) % (recur %2 (mod % %2))) ans n)))) xs)))

(defcheck solution-ceaf5304
  (fn [& v]
    (first
      (reduce
        (fn [a b] (filter (fn [x] (some #{x} (take-while #(<= % x) a))) b))
        (map #(iterate (partial + %1) %1) v)
        )
      )
    ))

(defcheck solution-cf1d90b0
  (fn lcs ([x y]
           (/ (* x y)
             ( (fn gcd [x y] (if (= y 0) x (gcd y (mod x y)) ) ) x y )
             )
           )
    ([x y & more]
     (apply lcs (cons (lcs x y) more))
     )
    ))

(defcheck solution-cfc129
  (fn lcd
    ([a b]
     (/ (* a b)
       ((fn [n m] (if (zero? m) n (recur m (rem n m)))) a b)))
    ([a b & as]
     (reduce lcd (lcd a b) as))))

(defcheck solution-cfe607d5
  (fn lcm [& a]
    (loop [b (vec a) v (vec a)]
      (if (apply = v) (first v)
                      (let [n (.indexOf v (first (sort v)))]
                        (recur b (update-in v [n] #(+ (b n) %))))))))

(defcheck solution-d084aab1
  (fn [& numbers]
    (letfn [(gcd [n others]
              (if (empty? others)
                n
                (let [r (rem n (first others))]
                  (recur (first others)
                    (if (= 0 r)
                      (rest others)
                      (cons r (rest others)))))))]
      (/ (apply * numbers) (gcd (first numbers) (rest numbers))))))

(defcheck solution-d095caa9
  (fn [& c] (ffirst (filter
                      #(apply = %)
                      (iterate (fn [c2] (let [mv (apply min c2)
                                              mi (first (keep-indexed #(when (= %2 mv) %1) c2))]
                                          (update-in c2 [mi] + (get (vec c) mi))))
                        (vec c))))))

(defcheck solution-d099c672
  (fn lgcd [a b & q]
    (let [pgcd (fn [a b]
                 (if  (= 0 b) a
                              (recur b (rem a b))))
          res (/ (* a b) (pgcd a b))]
      (if (empty? q) res (recur res (first q) (rest q))))))

(defcheck solution-d0a63e41
  (fn [& args]
    (letfn [(gcd [a b] (if (= b 0) a (gcd b (rem a b))))]
      (let [[x y & other] (flatten args)]
        (if (not y) x
                    (recur (cons (/ (* x y) (gcd x y)) other)))))))

(defcheck solution-d0d78641
  (fn [& rest]
    (let [next-row (fn [row step-row]
                     (let [i (.indexOf row (apply min row))]
                       (map-indexed #(if (= % i) (+ (step-row i) %2) %2) row)))
          ppcm-aux (fn [row step-row]
                     (if (apply = row)
                       (first row)
                       (recur (next-row row step-row) step-row)))]
      (ppcm-aux (vec rest) (vec rest)))))

(defcheck solution-d0dcb8d0
  (fn [& args]
    (let [gcd (fn [x y] (let [a (max x y)
                              b (min x y)
                              m (mod a b)]
                          (if (zero? m) b (recur b m))))
          lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-d1038932
  (fn [f & c]
    (letfn [(gcd [a b]
              (let [g (max a b) l (min a b)]
                (if (zero? l) g (recur l (- g l)))))]
      (reduce #(/ (* % %2) (gcd % %2)) f c))))

(defcheck solution-d11c4fdf
  (fn lcm
    [& args]
    (letfn [(multiples
              [n]
              (iterate (partial + n) n))
            (find-multiple
              [ms]
              (if (apply = (map first ms))
                (ffirst ms)
                (let [[[_ & el] & els] (sort-by first ms)]
                  (find-multiple (cons el els)))))]
      (find-multiple (map multiples args)))))

(defcheck solution-d168487c
  (fn lcm [& args]
    (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-d1a7e712
  (fn [ & args ]
    (letfn
     [(gcd[a,b]
        (if (zero? b) a
                      (recur b (mod a b))))
      (lcm
        ([a b]      (/ (* a b) (gcd a b)))
        ([a b & xs] (apply lcm (lcm a b) xs)))]
      (apply lcm args))))

(defcheck solution-d274312e
  (fn [x y & z]
    (loop [v (map #(iterate (partial + %) %) (apply vector x y z))]
      (let [f (map first v) f1 (first f)]
        (if (every? #(= f1 %) f)
          f1
          (let [m (apply min f)]
            (recur (map #(if (= %1 m) (rest %2) %2) f v))))))))

(defcheck solution-d2ccc1e8
  (fn lcd [& r]
    (letfn [(gcd [x y]
              (loop [a (max x y),
                     b (min x y)]
                (if (> b 0)
                  (recur b (rem a b))
                  a)))]
      (reduce #(/ (* % %2) (gcd % %2)) r))))

(defcheck solution-d3587aa
  (fn [& args] (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))] (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-d408bd8e
  (fn lcm [& v]
    (loop [m v]
      (if (apply = m)
        (first m)
        (recur (map-indexed (fn [index item] (if (= item (apply min m)) (+ item (nth v index)) item)) m))))))

(defcheck solution-d463163
  (fn calc-lcm [& args]
    (letfn [(lcm [a b]
              (loop [a-mults (map #(* a %1) (rest (range)))
                     b-mults (map #(* b %1) (rest (range)))]
                (let [first-a (first a-mults)
                      first-b (first b-mults)]
                  (if (= first-a first-b)
                    first-a
                    (if (> first-a first-b)
                      (recur a-mults (rest b-mults))
                      (recur (rest a-mults) b-mults)
                      )
                    )
                  )))] (reduce lcm args))))

(defcheck solution-d4c9ed79
  (fn [& args]
    (reduce
      (fn [a b]
        (first
          (filter #(integer? (/ % b))
            (map
              #(* a (inc %))
              (range)))))
      args)))

(defcheck solution-d59163ed
  (fn [& args]
    (let [v (vec args)]
      (loop [x v]
        (if (apply == x) (first x)
                         (let [r (first (sort x))]
                           (recur (map-indexed #(if (= %2 r) (+ %2 (v %1)) %2) x))))))))

(defcheck solution-d59aeb91
  (fn lcm [& args]
    (let [gcd (fn gcd [a b]
                (cond
                  (< a b) (gcd a (- b a))
                  (> a b) (gcd (- a b) b)
                  :else a))
          lcm- (fn [a b] (/ (* a b) (gcd a b)))
          [x y & more] args]
      (reduce lcm- (lcm- x y) more))))

(defcheck solution-d5d27b7d
  (fn lcm [x & xs]
    (some (fn [y] (if (every? #(zero? (rem y %)) xs) y))
      (iterate #(+ x %) x))))

(defcheck solution-d5da91e9
  (fn [x & nums]
    (let [gcd (fn [x y] (if (= 0 y) x (recur y (mod x y))))
          lcm2 (fn [x y] (/ (* x y) (gcd x y)))]
      (reduce lcm2 x nums))))

(defcheck solution-d6135e66
  (fn [& args]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (/ (reduce * args) (reduce gcd args))
      )))

(defcheck solution-d672912a
  (fn [& n]
    (loop [o n r n]
      (if (apply = r)
        (first r)
        (recur o (map-indexed #(+ %2 (if (= %2 (apply min r))
                                       (nth o %)
                                       0))
                   r))))))

(defcheck solution-d6878b67
  #(let [gcd (fn gcd [a b] (if (= b 0) a (gcd b (mod a b))))
         lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-d6dc0a65
  (fn [n & nums]
    (first
      (filter
        (fn [m] (every? #(zero? (rem m %)) nums))
        (iterate #(+ % n) n)))))

(defcheck solution-d9174b3a
  (fn [& pars]
    (reduce
      (fn lcm [x y]
        (let [
              gcd1 (loop [ a x b y ]
                     (if (= a b)
                       a
                       (if (> a b)
                         (recur (- a b) b)
                         (recur a (- b a)))))
              ]
          (/ (* x y) gcd1)))
      pars)))

(defcheck solution-d9266c0c
  (fn [& v]
    (loop [n (first v)]
      (if (zero? (reduce #(+ %1 (mod n %2)) (cons 0 v)))
        n
        (recur (+ n (first v)))
        )
      )
    ))

(defcheck solution-d94346cc
  (fn f [a & b]
    (loop [i a]
      (if (every? #(zero? (mod i %)) b)
        i
        (recur (+ i a))))))

(defcheck solution-d992fb9a
  (fn [& l]
    (reduce (fn [a b]
              (some #(and (= 0 (rem % b)) %)
                (map * (range 1 2147483647) (repeat a))))
      (first l) l)))

(defcheck solution-d9a2816a
  (fn
    [& xs]

    (let
     [
      gcd
      (fn [x y]
        (let
         [
          a (max x y)
          b (min x y)
          m (mod a b)
          ]
          (if (zero? m)
            b
            (recur b m)
            )
          )
        )
      lcm
      (fn
        [a b]
        (/ (* a b) (gcd a b))
        )
      ]
      (reduce lcm xs)
      )
    ))

(defcheck solution-d9bf1b1a
  (fn lcm [a b & args]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (lcm'
              ([a b] (/ (* a b) (gcd a b)))
              ([a b & args] (reduce lcm' a (cons b args))))]
      (apply lcm' a b args))))

(defcheck solution-da1d5ad6
  (fn [& args]
    (letfn [(gcd [x y]
              (let [a (max x y)
                    b (min x y)
                    m (mod a b)]
                (if (zero? m) b (recur b m))))
            (lcm [a b]
              (* (/ a (gcd a b)) b))]
      (reduce lcm args))))

(defcheck solution-da9b2cc1
  (fn [& p]
    (loop [s p c (apply max p)]
      (if (apply = s)
        (first s)
        (let [m (map #(cond (< % c) %2 :else 0) s p)
              n (map + s m)]
          (recur n (apply max n)))))))

(defcheck solution-daf22f6e
  #(letfn [(gcd [a b]
             (if (zero? b)
               a
               (gcd b (rem a b))))
           (lcm [a b]
             (/ (* a b) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-db645cdd
  (fn [& vs]
    (let [vms (map (fn [v] (map #(* v %) (iterate inc 1))) vs)]
      (letfn [(m? [ms n]
                (some (fn [x] (= n x))
                  (take-while (fn [x] (<= x n)) ms)))
              (m*? [n]
                (not (some not (map #(m? % n) vms))))]
        (first (filter m*? (first vms)))))))

(defcheck solution-db816761
  (fn lcm* [x & xs]
    (letfn [
            (gcd [a b]
              (cond
                (< a b) (gcd a, (- b a))
                (> a b) (gcd (- a b), b)
                :else a))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (if (nil? xs)
        x
        (lcm x (apply lcm* xs))))))

(defcheck solution-dbc3216a
  (fn lcm [& x]
    (
      let [lcm2
           (fn [a b]
             (
               let [gcd2
                    (fn gcdRec [a b]
                      (if(> b a) (gcdRec b a)
                                 (if (zero? b) a (gcdRec b (mod a b)))
                                 )
                      )]
               (/ (* a b) (gcd2 a b))
               ))]
      (if (= 1 (count x)) (first x) (lcm2 (first x) (apply lcm (rest x)))))))

(defcheck solution-dcb77b94
  (fn lcm
    ([x1 x2 & xs]
     (if (seq xs)
       (apply lcm (cons (lcm x1 x2) xs))
       (lcm x1 x2)
       )
     )
    ([x y]
     (let [int-ratio? (fn [a b] (= (int (/ a b)) (/ a b)))
           valid? (fn [a b] (or (int-ratio? a b) (int-ratio? b a)))
           invalid? (fn [a b] (not (valid? a b)))]
       (->>
         (map * (repeat x) (iterate inc 1))
         (drop-while (fn [b] (invalid? y b)))
         first
         )
       )
     )
    ))

(defcheck solution-dd1ab445
  (fn final-lcm [& lst]
    (letfn [(abs [x]
              (if (< x 0)
                (- x)
                x))
            (gcd [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))
            (lcm [a b]
              (/ (abs (* a b)) (gcd a b))) ]
      (reduce lcm lst))))

(defcheck solution-dd5e32ac
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))]
    #(reduce lcm %&)))

(defcheck solution-deea3cf1
  (fn [& l]
    (let [f (fn [[k v]] (* k v))]
      (loop [s (zipmap l (repeat 1))]
        (if (apply = (map f s))
          (first (map f s))
          (let [min (apply min-key f s)]
            (recur (assoc s (first min) (inc (second min))))))))))

(defcheck solution-df0bef4b
  (fn [& v] (/ (apply * v) (reduce (fn gcd [x y] (if (= 0 y) x (gcd y (mod x y)))) v))))

(defcheck solution-df0c7e83
  (fn [& xs]
    (letfn [
            (gcd [a b]
              (cond (zero? b) a
                    (< a b) (recur b a)
                    :else (recur b (rem a b))))
            (ngcd [a b & r]
              (if (empty? r)
                (gcd a b)
                (apply ngcd (gcd a b) r)))]
      (/ (apply * xs) (apply ngcd xs)))))

(defcheck solution-df3cc6c1
  (fn lcm [& args]
    (let [gcd (fn gcd [a b]
                (cond
                  (= b 0) a
                  (> a b) (gcd b (- a b))
                  :else (gcd a (- b a))))
          common (fn [a b] (/ (* a b) (gcd a b)))]
      (reduce common args))))

(defcheck solution-df68f3c4
  (fn [& args]
    (letfn [(gcd[a b] (if (= b 0) a (recur b (mod a b))))
            (lcm[a b] (/ (* a b)(gcd a b)))]
      (reduce lcm args))))

(defcheck solution-df75ce70
  (fn [& args] (reduce (fn [a b]
                         (/ (* a b) ((fn g [x y] (if (= 0 y) x (g y (mod x y)))) a b))) args)))

(defcheck solution-df904263
  (fn [x & xs]
    (first (drop-while
             (fn [y]
               (some
                 #(not= 0 (mod y %))
                 xs))
             (iterate (partial + x) x)))))

(defcheck solution-dfc8f972
  (fn lcm [& args]
    (let [f (fn g [s m]
              (if (every? #(= 0 (rem m %)) s)
                m
                (g s (+ m (first s)))))]
      (f (seq args) (first (seq args))))))

(defcheck solution-dfcc76bc
  (fn [a & c] (first (filter (fn [n] (every? #(= 0 (rem n %)) c))  (iterate (partial + a) a)))))

(defcheck solution-e093f7ff
  (fn [& nrs]
    (/
      (apply * nrs)
      ((fn gcd [v]
         (reduce
           (fn [n1 n2]
             (if (= n2 0)
               n1
               (recur n2 (rem n1 n2))))
           v))
       nrs))))

(defcheck solution-e11b8842
  (fn lcm [& X]
    (loop [Xk (vec X)]
      (if (apply = Xk)
        (first Xk)
        (let [xkm (apply min Xk)
              xkm-index (.indexOf Xk xkm)
              xk0 (nth X xkm-index)
              xkm+1 (+ xkm xk0)]
          (recur (assoc Xk xkm-index xkm+1)))))))

(defcheck solution-e17a1b2a
  (fn lcm [& args]
    (letfn [
            (gcd [a b]
              (if (< a b) (recur b a)
                          (if (zero? b) a (recur b (mod a b)) ))) ]
      (reduce #( / ( * % %2) (gcd % %2) ) args  ))))

(defcheck solution-e1935b7f
  (fn [& args]
    (let [gcd (fn [a b]
                (if (= b 0)
                  a
                  (recur b (mod a b))
                  ))
          gcdn (reduce gcd args)
          lcm (/ (reduce * args) gcdn)]
      lcm)))

(defcheck solution-e193e29a
  #(let [gcd (fn gcd [a b] (if (= 0 b) a (recur b (mod a b))))
         lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm %1 %&)))

(defcheck solution-e1af992b
  (fn [x & xs]
    (first
      (filter
        (fn [n]
          (every? #(zero? (mod n %)) xs))
        (iterate #(+ % x) x)))))

(defcheck solution-e2039c8
  (fn
    [a & args]
    (letfn [(gcd [a b]
              (cond
                (= a b) a
                (< a b) (gcd a (- b a))
                (> a b) (gcd (- a b) b)))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm a args))))

(defcheck solution-e238122c
  (fn [n m & xs]
    (letfn [(gcd [a b]
              (cond
                (zero? a) b
                (zero? b) a
                (< a b) (recur a (- b a))
                :else (recur b (- a b))))
            (lcm [a b] (/ (* a b) (gcd a b)))]
      (let [init (lcm n m)]
        (if (seq xs)
          (reduce lcm init xs)
          init
          )
        )
      )
    ))

(defcheck solution-e25f960f
  (fn f
    (
     [a b]
     (let [gcd
           (fn gcd [a b]
             (if (< a b)
               (gcd b a)
               (if (= b 0) a (gcd b (mod a b)))))
           ]
       (/ (* a b) (gcd a b))))
    ([a b c] (f (f a b) c))
    ([a b c d] (f (f a b) c d))
    ))

(defcheck solution-e2878749
  (fn lcmMult
    [x y & rest]
    (let [gcd (fn [a b] (if (< a b) (recur b a) (if (zero? b) a (recur b (rem a b)))))
          lcm (fn [x y] (* (/ x (gcd x y)) y))]
      (reduce lcm (lcm x y) rest))))

(defcheck solution-e2baa778
  #(letfn [(gcd [a b] (if (zero? b) a (recur b (rem a b))))
           (lcm [a b] (/ (* a b) (gcd a b)))]
     (reduce lcm %&)))

(defcheck solution-e2de9e2d
  (fn [& args]
    (let [multiplier (first args)]
      (first
        (filter
          (fn [val]
            (every? #(zero? (rem val %)) args))
          (range multiplier ##Inf multiplier))))))

(defcheck solution-e2ea7051
  (fn lcm [a & nums]
    (let [gcd (fn gcd [a b]
                (cond (= b 0) a
                      :else (recur b (mod a b))))]
      (/ (apply * a nums) (reduce gcd a nums)))))

(defcheck solution-e315935
  (fn lcm [& args]
    (/ (apply * args)
      (reduce #(if (zero? %) %2 (recur (mod %2 %) %)) args))))

(defcheck solution-e31dd2fa
  (fn m
    ([x y]
     (loop [a x
            b x
            c y
            d y]
       (cond (= a c) a
             (> a c) (recur a b (+ c d) d)
             true (recur (+ a b) b c d))))
    ([x y & r]
     (apply m (m x y) r))))

(defcheck solution-e3f8a38c
  (fn ! [& xs]
    (letfn
     [(gcd
        [a b]
        (if (== 0 b) a
                     (gcd b (rem a b))))
      (mmx
        [a b]
        (/ (* a b) (gcd a b)))
      (loopa
        [l]
        (if (== 2 (count l))
          (mmx (first l) (second l))
          (mmx (first l) (loopa (rest l)))))]
      (loopa xs))))

(defcheck solution-e453594f
  (fn [& x]
    (letfn [(gcd [[m n]] (if (zero? n) m (gcd [n (mod m n)])))
            (lcm [[m n]] (/ (* m n) (gcd [m n])))]
      (loop [a (take 2 (sort > x)) b (drop 2 (sort > x))]
        (if (empty? b)
          (lcm a)
          (recur [(lcm a) (first b)] (drop 1 b)))))))

(defcheck solution-e4995e73
  (fn lcm [a & b]
    (let [
          gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))
          lcm2 (fn [a b] (/ (* a b) (gcd a b)))
          ]
      (cond
        (empty? b) a
        (empty? (rest b)) (lcm2 a (first b))
        :else (apply (partial lcm (lcm2 a (first b))) (rest b))))))

(defcheck solution-e49fc936
  (fn [& nums]
    (let [max-num (apply max nums)
          factor? (fn [dividend divisor]
                    (= (mod dividend divisor) 0))
          all-factors? (fn [dividend divisors]
                         (not (some #(not (factor? dividend %))
                                divisors)))]
      (some #(when (all-factors? %1 nums) %1)
        (map #(* max-num %) (iterate inc 1))))))

(defcheck solution-e4a281ff
  (fn lcm [& ns]
    (let [ms (map #(map (partial * %) (rest (range))) ns)]
      (loop [a (first ms)]
        (if (every? identity
              (map #(loop [b %]
                      (if (< (first a) (first b))
                        false
                        (if (= (first a) (first b))
                          true
                          (recur (rest b)))))
                (rest ms)))
          (first a)
          (recur (rest a)))))))

(defcheck solution-e4dee6b2
  (fn [& xs]
    (first
      (apply clojure.set/intersection
        (for [x xs]
          (into (sorted-set) (take 500 (iterate (partial + x) x))))
        ))
    ))

(defcheck solution-e5039d39
  (fn lcm [& args]
    (letfn [(lcm-one [x y] (/ (* x y) (gcd-one x y)))
            (gcd-one [x y]
              (if (= x y)
                x
                (gcd-one (- (max x y) (min x y)) (min x y))))]
      (reduce lcm-one args))))

(defcheck solution-e594e8b8
  (fn [x & nums]
    (some
      (fn [n]
        (if (every? #(zero? (mod n %)) nums) n false))
      (iterate (partial + x) x))
    ))

(defcheck solution-e625b7a9
  (fn [& args]
    (let [gcd #(if (zero? %2) %1 (recur %2 (mod %1 %2)))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) args))))

(defcheck solution-e67c7e1b
  (fn lcm
    ([x y]
     (loop [a x b y]
       (let [r (rem a b)]
         (if (= r 0) ;; then b = gcd -> lcm = x*y/gcd
           (/ (* x y) b)
           (recur b r)))))
    ([x y & args] (lcm x (apply lcm (cons y args))))))

(defcheck solution-e68ab8fc
  (fn [& args]
    (loop [m (reduce min args) current (reduce min args) vals args]
      (if (every? #(= 0 (rem current %)) vals)
        current
        (recur m (+ current (reduce min [m 1])) vals)))))

(defcheck solution-e776da1
  (fn lcm [x y & r]
    (let [z (first (filter #(= 0 (rem % x)) (iterate #(+ y %) y)))]
      (if (seq r) (apply lcm z r) z))))

(defcheck solution-e77d0c90
  (fn f [& args]
    (letfn [(gcd [a b]
              (let [x (if (> a b) a b)
                    y (if (> a b) b a)
                    z (rem x y)]
                (if (= z 0)
                  y
                  (gcd y z))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (if (= 2 (count args))
        (apply lcm args)
        (apply f (lcm (first args) (second args)) (drop 2 args))))))

(defcheck solution-e77f92ee
  (letfn [(gcd
            [a b]
            (if (zero? b) a
                          (recur b (mod a b))))

          (lcm
            [a b]
            (/ (* a b)
              (gcd a b)))]

    #(reduce lcm %&)))

(defcheck solution-e78fb349
  (fn [& n] (reduce #(/ (* % %2) ((fn g [a b] (if (zero? b) a (g b (mod a b)))) % %2)) n)))

(defcheck solution-e78fe9e9
  (fn [& as]
    (loop [ a-m (map #(vector % %) as)]
      (let [sorted (sort-by last a-m)
            values (map last a-m)
            [a b]   (first sorted)]
        (if (apply = values) b
                             (recur (conj (rest sorted) [a (+ a b)])))))))

(defcheck solution-e7ad5c7d
  (fn my-lcm [a b & rests]
    (let [gcd (fn [a b] (loop [a a b b] (if (zero? b) a (recur b (mod a b)))))
          n (/ (* a b) (gcd a b))]
      (if (seq rests)
        (apply my-lcm (cons n rests))
        n))))

(defcheck solution-e7efaf81
  (fn [& v]
    (loop [x v]
      (if (some #(not= (first x) %) x)
        (let [m (apply min x)]
          (recur (map #(if (= m %1) (+ %1 %2) %1) x v)))
        (first x)))))

(defcheck solution-e854b1e2
  #(/ (apply * %&) (apply (fn gcd ([x y]
                                   (loop [m x n y]
                                     (cond (zero? (mod m n)) n,
                                           (zero? (mod n m)) m,
                                           :else (recur (mod m n) (mod n m)))))
                            ([x y & more] (reduce gcd (gcd x y) more))) %&)))

(defcheck solution-e877a8ab
  (fn lcm [& args]
    (let [gcd (fn [a b]
                (cond
                  (zero? a) b
                  (zero? b) a
                  0 (recur b (mod a b))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-e8901f8e
  (fn [& nums]
    (reduce (fn [x y]
              (loop [[xh & xt :as xall] (iterate (partial + x) x)
                     [yh & yt :as yall] (iterate (partial + y) y)]
                (cond (= xh yh) xh
                      (< xh yh) (recur xt yall)
                      :else (recur xall yt))))
      nums)))

(defcheck solution-e9874126
  (fn [& v] (reduce (fn [a b] (/ (* a b)
                                (#(if (= 0 %)
                                    %2
                                    (recur (mod %2 %) %)) a b) )) v)))

(defcheck solution-e99318c0
  (fn [& xs]
    (letfn [(gcd [x y] (if (zero? y) x (recur y (rem x y))))]
      (reduce #(/ (* %1 %2) (gcd %1 %2)) xs))))

(defcheck solution-e9aabdb0
  (fn [& xs]
    (let [lcmlist (into [] (map vector (range (count xs)) xs))
          find-min-index-help
                  (fn [[idx minx] [id x]]
                    (if (< x minx) [id x] [idx minx]))
          find-min-index
                  (fn [xs]
                    (reduce find-min-index-help [0 9999] xs))]
      (loop [lcmmod lcmlist]
        (if (apply = (map #(second %) lcmmod))
          (second (first lcmmod))
          (let [[idx minx] (find-min-index lcmmod)]
            (recur (assoc lcmmod idx [idx (+ (second (nth lcmlist idx))
                                             (second (nth lcmmod idx)))]))))))))

(defcheck solution-e9e49ed3
  (fn least-common-multiple
    [& args]
    (first
      (filter
        (fn [x]
          (every? #(zero? (mod x %)) args))
        (rest (iterate (partial + (apply min args)) 0))))))

(defcheck solution-ea6db6f7
  (fn [& args]
    (let [h (zipmap args args)
          lcm (fn lcm [amap]
                (let [vs (vals amap)
                      m (apply min vs)]
                  (if (apply = vs)
                    m
                    (lcm (into {} (map (fn [[k v]]
                                         (if (= v m)
                                           [k (+ k v)]
                                           [k v]))
                                    amap))))))]
      (lcm h))))

(defcheck solution-ea850fcc
  (fn [& s]
    (let [gcd (fn gcd [a b] (if (zero? b) a (recur b (mod a b))))
          lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
      (reduce lcm s))))

(defcheck solution-eb1f8700
  (fn lcm-v2 [& args]
    (letfn [(gcd [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
            (lcm [a b]
              (/ (* a b)
                (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-eb9a9958
  (fn [x & xs]
    (let [multiples (map (partial * x) (rest (range)))
          common-multiples (drop-while #(some (fn [x] (not= 0 (rem % x))) xs) multiples)]
      (first common-multiples))))

(defcheck solution-ebbb4b84
  (fn lcd [& nums]
    (let [gcd (fn [x y]
                (let [[little big] (sort [x y])
                      diff (- big little)]
                  (if (< 0 little)
                    (recur diff little)
                    big)))
          lcd-of-two (fn [x y] (/ (* x y) (gcd x y)))]
      (reduce lcd-of-two nums))))

(defcheck solution-ec0b1e14
  (fn s [& nums]
    (letfn [(f [prods]
              (if (apply = prods)
                (first prods)
                (let [m (apply min prods)]
                  (recur (map (fn [prod orig]
                                (if (<= prod m) (+ orig prod) prod))
                           prods nums)))))]
      (f nums))))

(defcheck solution-ec114cd8
  (fn [& more]
    (loop [seq1 more seq2 more]
      (if (= 1 (count (distinct seq1)))
        (first seq1)
        (recur
          (map-indexed
            #(if (= %2 (apply min seq1)) (+ %2 (nth seq2 %1)) %2)
            seq1)  seq2
          )
        )
      )

    ))

(defcheck solution-ec29186f
  (fn [& args] (reduce (fn [a b] (letfn [(gcm [a b] (if (zero? b) a (recur b (mod a b))))] (/ (* a b) (gcm a b)))) args)))

(defcheck solution-ec295bf6
  (fn lcm
    ([x y]
     (/
       (* x y)
       ((fn gcd [a b]
          (cond
            (> b a) (gcd b a)
            (= b 0) a
            :else (gcd b (mod a b))))
        x y)))
    ([x y & r] (apply lcm (conj r (lcm x y))))))

(defcheck solution-ec58c76a
  (fn [& xs]
    (loop [acc (apply vector xs)]
      (if (apply == acc)
        (first acc)
        (let [idx-min (ffirst (sort-by second < (map-indexed #(vector % %2) acc)))]
          (recur (assoc acc idx-min (+ (nth acc idx-min) (nth xs idx-min)))))))))

(defcheck solution-ec769976
  (fn lcm [& args]
    (let [gcd (fn gcd [x y]
                (if (zero? y)
                  x
                  (gcd y (mod x y))))]
      (reduce (fn [x y]
                (/ (* x y) (gcd x y)))
        args))))

(defcheck solution-ec9a499b
  (letfn [(gcd [& args]
            (reduce (fn [a b]
                      (if (zero? b)
                        a
                        (recur b (mod a b))))
              args))]
    (fn [& args]
      (/ (apply * args) (apply gcd args)))))

(defcheck solution-ed8237c0
  (fn [& v] (/ (apply * v) (loop [s (apply min v)]
                             (let [rs (filter pos? (map #(rem % s) v))]
                               (if (= 0 (count rs)) s (recur (apply min s rs)) ))) )))

(defcheck solution-ed9b7c51
  (fn lcm
    [& nums]
    (letfn [(gcd [x y] (if (zero? y) x (gcd y (mod x y))))]
      (reduce (fn [x y]
                (/ (* x y) (gcd x y))) (first nums) (rest nums)))))

(defcheck solution-ee22c41b
  (fn mcm [n & nums]
    (let [m (iterate (partial + n) n)]
      (first (filter (fn [x]
                       (every? #(zero? (mod x %)) nums))
               m)))))

(defcheck solution-ee3f3afa
  (fn lcm-iter [& args]
    (letfn [(gcd [a, b]
              (cond
                (= b 0) a
                :else (gcd b (rem a b))))
            (abs [a]
              (if (< a 0) (- 0 a) a))
            (lcm [a, b]
              (/ (abs (* a b)) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-eee40fa6
  (fn [& args]
    (let [gcd #(if (= %2 0) % (recur %2 (rem % %2)))]
      (reduce #(/ (* % %2) (gcd % %2)) args))))

(defcheck solution-ef8de97d
  (fn [& args]
    (let
     [m (apply min args)
      modAll (fn [x]
               (apply (partial = 0)
                 (map #(mod %2 %) args (repeat x))))]
      (first (filter modAll (map #(* m %) (range 1 1000)))))))

(defcheck solution-f078d4
  (fn [& more]
    (letfn [(into-table [s]
              ;; key = original number, value = its multiple
              (zipmap s s))
            (key-of-min-val [m]
              (first
                (reduce
                  (fn [acc item]
                    (if (< (second acc) (second item))
                      acc item))
                  m)))
            (lcm [tbl]
              (if (== 1 (count (into #{} (vals tbl))))
                (first (vals tbl))
                (let [key-of-min (key-of-min-val tbl)]
                  (recur (update-in tbl [key-of-min] + key-of-min) )) ))
            ]
      (lcm  (into-table more) ))
    ))

(defcheck solution-f0a4035d
  (fn [& args]
    (let [lcm

          (fn [n1 n2]
            (let [gcd (fn gcd [n1 n2]
                        (let [l (min n1 n2), m (max n1 n2)]
                          (if (zero? l) m
                                        (gcd (- m l) l))))
                  g (gcd n1 n2)]
              (->> (/ n1 g)
                (* (/ n2 g))
                (* g))))]
      (reduce lcm args))))

(defcheck solution-f0bdbd6e
  (fn lcm [h & [ft & t]]
    (if (nil? ft)
      h
      (apply lcm
        (cons
          (/ (* h ft)
            ((fn [a b]
               (if (zero? b)
                 a
                 (recur b (mod a b)))) h ft)) t)))))

(defcheck solution-f0ce1f90
  (fn[& x]
    (letfn [(gcd [a b]
              (cond
                (> a b) (if (zero? (mod a b)) b (recur b (mod a b)))
                (< a b) (recur b a)
                :else a))]
      (/ (reduce * x) (reduce gcd x)))))

(defcheck solution-f158a615
  (fn
    [& nums]
    (let
     [m (apply max nums)]
      (first (filter (fn [km] (every? #(= 0 (rem km %)) nums)) (iterate #(+ m %) m))))))

(defcheck solution-f1eda013
  (fn lcm
    ([a b]
     (let [gcd
           (fn [a b]
             (cond (zero? b) a
                   :else (recur b (rem a b))))]
       (/ (* a b) (gcd a b))))
    ([a b & xs] (reduce lcm (conj xs a b)))))

(defcheck solution-f1fff8ce
  (fn [& v]
    (reduce (fn [x y]
              (* y (first
                     (filter integer?
                       (map #(/ (* x %) y)
                         (range 1 1000)))) ))
      v)))

(defcheck solution-f247a139
  (fn lcm
    [& args]
    (letfn [(gcd
              [a b]
              (if (zero? b)
                a
                (gcd b (mod a b))))]
      (reduce (fn [r e]
                (/ (* r e)
                  (gcd r e)))
        (first args) (rest args)))))

(defcheck solution-f26178d0
  #(first (keep (fn [v] (if (every? (fn [n] (zero? (rem v n))) %&) v))
            (map (partial * (apply max %&)) (rest (range))))))

(defcheck solution-f2999785
  (fn [& x]
    (let [ gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
          lcm (fn [a b] (/ (* a b) (gcd a b)))]
      (loop [m (first x)
             rst (rest x)]
        (if (empty? rst) m
                         (recur (lcm m (first rst)) (rest rst)))))))

(defcheck solution-f2a8e63f
  (fn [& args]
    (->> (iterate (fn [m]
                    (let [[k v] (first (sort-by (fn [[k v]] v) m))]
                      (assoc m k (+ v k))))
           (zipmap args args))
      (filter (fn [m] (apply = (vals m))))
      ffirst
      last)))

(defcheck solution-f2e9a397
  (letfn [(gcd [a b] (if (= b 0) a (gcd b (mod a b))))
          (lcm [a b] (/ (* a b) (gcd a b)))]
    #(reduce lcm %&)))

(defcheck solution-f3ff0971
  (fn [& nums]
    (loop [mults (apply merge (map (fn [k] {k k}) nums))]
      (let [values (vals mults)]
        (if (apply = values)
          (first values)
          (let [[k v] (apply min-key (fn [[k v]] v) mults)]
            (recur (assoc mults k (+ v k)))))))))

(defcheck solution-f43a367
  (fn [& ns]
    ; lcm = product / gcd
    (/
      (apply * ns)
      (reduce #(if (zero? %2) %1 (recur %2 (mod %1 %2))) ns)
      )
    ))

(defcheck solution-f49ffd6
  (fn lcd [& xs]
    (let [gcd (fn gcd [x y]
                (cond
                  (zero? y) x
                  (> x y) (gcd y (mod x y))
                  :else (gcd x (mod y x))))
          lcd_ (fn [x y] (/ (* x y) (gcd x y)))]
      (reduce lcd_ xs))))

(defcheck solution-f4a98998
  (fn lcm [& n]
    (let [gcd (fn gcd [a b]
                (cond
                  (> b a) (gcd b a)
                  (= b 0) a
                  true (gcd b (mod a b))))
          c (reduce gcd n)]
      (/ (apply * n) c))))

(defcheck solution-f5a5ffba
  #(letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))]
     (/ (apply * %&) (reduce gcd %&))))

(defcheck solution-f65836a8
  (fn [& a]
    (reduce (fn [a b]
              ((fn lcm [a aa b bb]
                 (cond (= a b) a
                       (> a b) (lcm a aa (+ b bb) bb)
                       :else (lcm (+ a aa) aa b bb))) a a b b))
      a)))

(defcheck solution-f69e15aa
  (fn [& n]
    (/ (reduce * n) (reduce #(loop [a % b %2]
                               (if (= (rem a b) 0)
                                 b
                                 (recur b (rem a b)))) n))))

(defcheck solution-f70c6891
  (fn [& nums]
    (let [maxnum (apply max nums)]
      (loop [i maxnum]
        (if (every? #(zero? (mod i %)) nums)
          i
          (recur (+ i maxnum)))))))

(defcheck solution-f87543c8
  (fn [& args]
    (letfn [(gcd [x y] (if (zero? y) x (gcd y (mod x y))))
            (lcm [x y] (/ (* x y) (gcd x y)))]
      (reduce lcm args))))

(defcheck solution-f906079b
  (fn [& ns]
    (loop [pairs (vec (map #(vector % %) ns))
           i 0]
      #_(println pairs)
      (if (or (= 1 (count (group-by identity (map second pairs))))
              (> i 1000))
        (second (first pairs))
        (let [sorted (sort-by second pairs)
              min (first sorted)
              [step current] min]
          (recur (cons [step (+ step current)]
                   (rest sorted))
            (inc i)))))))

(defcheck solution-f97e56a0
  ; &#12518;&#12540;&#12463;&#12522;&#12483;&#12489;&#12398;&#20114;&#38500;&#27861;&#12364;&#20998;&#25968;&#12391;&#12418;&#36890;&#29992;&#12377;&#12427;&#12398;&#12434;&#30693;&#12425;&#12394;&#12363;&#12387;&#12383;&#29256;
  ;(fn lcm [& nums]
  ;  (letfn [(gcd-int [m n] (if (zero? n) m (recur n (mod m n))))
  ;          (lcm-int [m n] (/ (* m n) (gcd-int m n)))
  ;          (denomi-ex [r] (if (integer? r) 1 (denominator r)))
  ;          (nume-ex   [r] (if (integer? r) r (numerator r)))]
  ;      (let [sorted (sort > nums)
  ;            denominators (map denomi-ex sorted)
  ;            denomi-lcm   (reduce lcm-int denominators)
  ;            numerators   (map nume-ex sorted)
  ;            nume-lcmed   (->> (map (partial / denomi-lcm) denominators) (map * numerators))]
  ;      (/ (reduce lcm-int nume-lcmed) denomi-lcm))))

  ; &#20998;&#25968;&#12391;&#12468;&#12481;&#12515;&#12468;&#12481;&#12515;&#12377;&#12427;&#24517;&#35201;&#12394;&#12367;&#12394;&#12387;&#12383;
  (fn lcm [& nums]
    (letfn [(gcd-2 [m n] (if (zero? n) m (recur n (mod m n))))
            (lcm-2 [m n] (/ (* m n) (gcd-2 m n)))]
      (reduce lcm-2 (sort > nums)))))

(defcheck solution-fac0dc5c
  (fn f [& a]
    (loop [a a o a]
      (if (= 1 (count (set a)))
        (first a)
        (recur
          (map #(if (= % (apply min a)) (+ % %2) %) a o)
          o)))))

(defcheck solution-fae1ebd1
  (fn [x & xs]
    (first
      (drop-while
        (fn [z] (some #(pos? (mod z %)) xs))
        (iterate #(+ x %) x)))))

(defcheck solution-faf443b1
  (fn lcm [& args]
    (loop [coll args]
      (if (apply = coll)
        (first coll)
        (let [mn (apply min coll)
              idxs (remove nil? (map-indexed (fn [i e] (if (= e mn) i)) coll))
              ind (first idxs)]
          (recur (map-indexed (fn [i e]
                                (if (= ind i)
                                  (+ e (nth args ind))
                                  e))
                   coll)))))))

(defcheck solution-fb3848cb
  (fn [& c]
    (reduce
      #(/ (* % %2)
         (loop [a %
                b %2]
           (cond
             (= a b) a
             (> a b) (recur (- a b) b)
             :else (recur a (- b a)))))
      c)))

(defcheck solution-fb42af6b
  (fn [& args]
    (let [gcd #(if (zero? %2) %1 (recur %2 (mod %1 %2)))]
      (/ (apply * args) (reduce gcd args))
      )
    ))

(defcheck solution-fb5bef91
  #(reduce
     (fn [a b]
       (loop [x a y b]
         (cond
           (= x y) x
           (> x y) (recur x (+ y b))
           (< x y) (recur (+ x a) y))))
     %&))

(defcheck solution-fb7dd918
  (fn [& xs]
    (letfn [(gcd [a b] (if (< a b) (recur b a)
                                   (if (zero? b) a
                                                 (recur (mod a b) b))))]
      (/ (apply * xs) (reduce gcd xs)))))

(defcheck solution-fc1aacd3
  (fn lcm [& args]
    (let
     [cnt       (count args)
      get-cnt   (fn [m k] (get m k 0))
      inc-cnt   (fn [m k] (assoc m k (inc (get-cnt m k))))
      last-cnt? (fn [m k] (= (dec cnt) (get-cnt m k)))]

      (loop [s (apply interleave
                 (map (fn [i] (iterate (partial + i) i)) args))
             res {}]
        (if (last-cnt? res (first s))
          (first s)
          (recur (next s)
            (inc-cnt res (first s))))
        )
      )
    ))

(defcheck solution-fc33339f
  (fn lcm
    ([a b]
     (letfn [(gcd [x y]
               (if (<= y 0)
                 x
                 (recur y (mod x y))))]
       (* a (quot b (gcd a b)))))
    ([a b & c]
     (reduce #(lcm % %2) a (cons b c)))))

(defcheck solution-fd29edea
  (fn lcd [& l]
    (loop [m (reduce min l) cur (reduce min l)]
      (if (every? #(= 0 %) (map #(rem cur %) l))
        cur
        (recur m (+ cur m))))))

(defcheck solution-fd6e88b2
  (fn lcm [& nums]
    (let [gcd2 (fn [a b]
                 (if (zero? b)
                   a
                   (recur b (mod a b))))
          gcd (fn [& nums] (reduce gcd2 nums))]
      (/ (apply * nums)(apply gcd nums)))))

(defcheck solution-fd88b365
  (fn lcm
    ([x y]
     (first (filter #(= 0 (mod % y)) (map #(* % x) (iterate inc 1)))))
    ([x y & args]
     (apply (partial lcm (lcm x y) (first args)) (rest args)))))

(defcheck solution-fd8f2ce9
  (fn lcm-mult [& args]
    (letfn [(gcd [a b]
              (loop [a a b b]
                (let [[lrg sma] (reverse (sort [a b]))]
                  (if (= lrg sma) lrg
                                  (recur (- lrg sma) sma)))))
            (lcm [a b]
              (/ (* a b) (gcd a b)))]
      (reduce lcm args))))

(defcheck solution-fdb0ee89
  (fn lcm [& args]
    (let [gcd (fn gcd ([a b]
                       (if (= 0 (rem a b))
                         b
                         (recur b (rem a b))))
                ([a b & c]
                 (apply gcd (cons (gcd a b) c))))]
      (/ (apply * args) (apply gcd args)))))

(defcheck solution-fddfef38
  (fn [& args]
    (reduce
      (fn [a b] (first (drop-while #(not= 0 (rem % b)) (iterate (partial + a) a)))) args)))

(defcheck solution-fdf78665
  (fn [& args]
    (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
      (/ (reduce * args) (reduce gcd args)))))

(defcheck solution-fe58ba11
  (fn lcm ([a b]
           (letfn [(gcd [a b]
                     (cond (> a b) (gcd (- a b) b)
                           (< a b) (gcd a (- b a))
                           :else a))]
             (/ (* a b) (gcd a b))))
    ([a b & z]
     (reduce #(lcm %1 %2) (lcm a b) z))))

(defcheck solution-fe781c06
  (fn [& col]
    (reduce (fn [a b]
              (/ (* a b)  ((fn [x y]
                             (loop [a (max x y) b (min x y )]
                               ( if (= b 0)
                                 a
                                 (recur b (mod  a b ) )
                                 )
                               )
                             )   a b) )
              )  col)
    ))

(defcheck solution-ff55adad
  (fn lcm [x y & xs]
    (let [lcm* (fn [x y]
                 (let [
                       z-plus (drop 1 (range))
                       xs (map (partial * x) z-plus)
                       ys (map (partial * y) z-plus)
                       ys-contains? (fn [n]
                                      (->> ys (take-while #(<= % n)) (some #(== n %))))
                       ]
                   (if (or (== 0 x) (== 0 y))
                     0
                     (some #(when (ys-contains? %) %) xs))))]
      (reduce #(lcm* %1 %2) (lcm* x y) xs))))

(defcheck solution-ff9c8efc
  (fn [& args]
    (let [s (into #{} args)
          biggest (first (sort > args))
          multiples (map #(* biggest %) (iterate inc 1))
          others (disj s biggest)
          divisible? (fn [n]
                       (every? #(= 0 (mod n %)) others))]
      (first (filter divisible? multiples)))))

(defcheck solution-ffeee1ea
  (fn [& c]
    (loop [i 2]
      (let [n (* (first c) i)]
        (if (= (set (map #(mod n %) c)) #{0})
          n
          (recur (inc i)))))))

(defcheck solution-fffb38f6
  (fn [lcm & ns] (reduce lcm ns)) ((fn lcm [gcd] #(/ (* %1 %2) (gcd %1 %2)))
                                   #(if (= %2 0) %1 (recur %2 (rem %1 %2)))))

(defcheck solution-ffffffff
  (fn [& xs]
    (let [multiples (fn [x] (iterate #(+ x %) x))]
      (loop [mult-seqs (map multiples xs)]
        (if (apply = (map first mult-seqs))
          (ffirst mult-seqs)
          (let [max-first (apply max (map first mult-seqs))]
            (recur (map #(if (< (first %) max-first) (rest %) %) mult-seqs))))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-100))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

