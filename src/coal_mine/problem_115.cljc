(ns coal-mine.problem-115
  (:require [coal-mine.checks :refer [defcheck-115] :rename {defcheck-115 defcheck}]
            [clojure.test]))

(defn parse-int [s]
  (#?(:clj Integer/parseInt :cljs js/parseInt) s))

(defn parse-char [c]
  (#?(:clj Character/getNumericValue :cljs js/parseInt) c))

(defn character-digit [c n]
  (assert (== n 10))
  (parse-char c))

(defcheck solution-10155ce9
  (fn balanced? [N]
    (let [nums (map #(- (int %) 48) (seq (str N)))
          [L R] (split-at (/ (count nums) 2) nums)]
      (if (> (count L) (count R))
        (= (reduce + (butlast L)) (reduce + R))
        (= (reduce + L) (reduce + R))))))

(defcheck solution-1021a35f
  (fn [x]
    (let [a (lazy-seq (str x))
          b (count a)]
      (if (= b 1)
        true
        (if (odd? b)
          (= (sort (take (quot b 2) a)) (sort (drop (inc (quot b 2)) a)))
          (= (sort (take (quot b 2) a)) (sort (drop (quot b 2) a)))
          )))))

(defcheck solution-105e77f2
  (fn [n]
    (let [s (vec (str n))
          l (count s)
          o (quot l 2)]
      (==
        (reduce #(+ %1 (int %2)) 0 (subvec s 0 o))
        (reduce #(+ %1 (int %2)) 0 (subvec s (- l o) l))))))

(defcheck solution-106da46f
  (fn [n]
    (let [v (map #(parse-char %) (str n))
          c (count v)
          h (if (odd? c) (dec (/ c 2)) (/ c 2))]
      (= (reduce + (take h v)) (reduce + (take h (reverse v)))))))

(defcheck solution-1073842e
  (fn [n]
    (let [s (str n)
          f #(apply + (map int (%1 (int (/ (count %2) 2)) %2)))]
      (=
        (f take s)
        (f take-last s)))))

(defcheck solution-1093f965
  (fn [n]
    (let [digits (map #(rem (int (/ n %)) 10)
                   (take-while #(<= % n) (iterate (partial * 10) 1)))
          m (int (/ (count digits) 2))]
      (= (reduce + (take m digits))
        (reduce + (take m (reverse digits)))))))

(defcheck solution-109746f
  (fn [n]
    (let [lst (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (str n)))
          half (quot (count lst) 2)
          a (take half lst)
          b (take-last half lst)]
      (= (reduce + a) (reduce + b)))))

(defcheck solution-10f7886f
  (fn [n]
    (let [digits (map #(mod % 10)
                   (take-while (partial < 0)
                     (iterate #(quot % 10) n)))
          half (quot (count digits) 2)
          xs (take half digits)
          ys (take half (reverse digits))]
      (= (apply + xs) (apply + ys)))))

(defcheck solution-110aafb8
  (fn [n]
    (let [s (str n)
          c (count s)
          d (int (/ c 2))
          a (take d s)
          b (drop (- c d) s)]
      (apply =
        (map
          (partial reduce #(+ %1 (- (int %2) 48)) 0)
          [a b])))))

(defcheck solution-11d99964
  #(let [s (str %)
         h (quot (count s) 2)
         f (fn [s] (reduce (fn [t x] (+ t (int x))) 0 s))]
     (= (f (take h s))
       (f (take h (reverse s))))))

(defcheck solution-11dc778a
  (fn [n] (let [s (map #(let [n (parse-char %)] (* n n)) (seq (str n)))
                half (int (/ (count s) 2))]
            (= (apply + (take half s)) (apply + (take-last half s))))))

(defcheck solution-1295035d
  (fn balance? [n]
    (let [ord (fn [ch] (- (parse-char ch) (parse-char \0)))
          s (str n)
          size (quot (count s) 2)
          l (map ord (take size s))
          r (map ord (take size (reverse s)))]
      (= (apply + l) (apply + r)))))

(defcheck solution-129db9f9
  (fn [number]
    (let [digits (map #(- (int %) 48) (str number))]
      (= (reduce + (take (/ (count digits) 2) digits))
        (reduce + (take (/ (count digits) 2) (reverse digits)))))))

(defcheck solution-1378dc10
  (fn [n]
    (let [digits (loop [q [(mod n 10)] r (quot n 10)]
                   (if (pos? r) (recur (conj q (mod r 10)) (quot r 10)) q))
          c (quot (count digits) 2)]
      (= (apply + (take c digits))
        (apply + (take c (rseq digits)))))))

(defcheck solution-13875feb
  (fn [n]
    (let [s (map #(parse-int (str %)) (str n))
          h (quot (count s) 2)]
      (= (apply + (take h s)) (apply + (take h (reverse s)))))))

(defcheck solution-13a5e39
  (fn balanced? [n]
    (let [ digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          c (/ (count digits) 2) ]
      (= (reduce + (take c digits)) (reduce + (take c (reverse digits)))))))

(defcheck solution-13f75b40
  (fn [n]
    (loop [vals (map #(parse-char %) (str n)) a 0 b 0]
      (if (empty? vals)
        (= a b)
        (recur (rest (butlast vals)) (+ a (first vals)) (+ b (last vals)))))))

(defcheck solution-13fa0920
  #(let [s (map int (str %)) k (quot (count s) 2) t (partial reduce +)]
     (= (t (take k s)) (t (take-last k s)))))

(defcheck solution-14492cc
  #(let [n (map int (str %)) i (count n) c (quot i 2) a apply
         l (take c n) r (drop  (- i c) n)]
     (= (a + l) (a + r))))

(defcheck solution-145b8d7b
  (fn [n]
    (let [ds (map int (str n))]
      (zero? (reduce + (map - (take (quot (count ds) 2) ds) (reverse ds)))))))

(defcheck solution-14cb7d8f
  (fn [n] (= (reduce #(+ %1 (- (int %2) 48)) 0 (take (Math/ceil (/ (count (str n)) 2)) (str n)))
            (reduce #(+ %1 (- (int %2) 48)) 0 (take (Math/ceil (/ (count (str n)) 2)) (reverse (str n)))))))

(defcheck solution-14d75eb3
  (fn [n]
    (let [ s   (str n)
          m   (count s)
          fh  (take (/ (dec m) 2) s)
          sh  (drop (/ m 2) s)
          sum #(reduce + (map (comp parse-int str) %)) ]
      (= (sum fh) (sum sh)))))

(defcheck solution-1509f728
  (fn [n]
    (letfn [ (split-number [x]
               (let [l (map int (str x))
                     s (quot (count l) 2)]
                 (list (drop s l)
                   (drop-last s l)))) ]
      (apply = (map #(reduce + %) (split-number n))))))

(defcheck solution-15247c34
  (fn balance-n [n]
    (let [as-str (str n)
          len (count as-str)
          half (quot len 2)
          left (subs as-str 0 half)
          right (subs as-str (- len half))
          sum-fn (fn [s] (reduce + (map #(parse-char %1) s)))
          ]
      (=
        (sum-fn left)
        (sum-fn right)
        )
      )
    ))

(defcheck solution-15443be
  (fn sym
    [n]
    (let [s (str n)
          l (int (/ (count s) 2))
          s1 (.substring s 0 l)
          s2 (.substring s (- (count s) l))]
      (letfn [(sum [sn] (reduce #(+ % (parse-int (str %2))) 0 sn))]
        (= (sum s1) (sum s2))))))

(defcheck solution-154d30c5
  (fn [n]
    (let [s (map int (seq (str n)))
          half (partial take (/ (count s) 2))]
      (= (apply + (half s))
        (apply + (half (reverse s)))))))

(defcheck solution-15c62fbd
  (fn [num]
    (let [coll (map #(parse-int (str %1)) (seq (str num)))]
      (=
        (apply + (take-last (/ (count coll) 2) coll))
        (apply + (take (/ (count coll) 2) coll))
        )
      )
    ))

(defcheck solution-15ef46b1
  (fn checkBal[num]
    (if (< num 10)
      true
      (let[len  (+ (int (Math/log10 num) ) 1 )  ,
           half1 (if (odd? len) (/ (- len 1) 2)  (/ len 2) ),
           half2 (if (odd? len) (+ half1 1)   half1)
           l  (map #(- (int %) 48)  (seq (str num) ) )
           ]
        (= (apply + (take half1 l) )
          (apply + (drop half2 l) )
          )
        )
      )
    ))

(defcheck solution-160452ae
  (fn [x]
    (let [a (map int (str x)) b (/ (count a) 2)]
      (apply =
        (map
          #(reduce + 0 %)
          [(take b a) (take-last b a)])))))

(defcheck solution-1645dcb8
  (fn [n] (let [s (map #(parse-int (str %)) (str n))
                l (count s)
                h (quot l 2)]
            (= (apply + (take h s)) (apply + (drop (- l h) s))))))

(defcheck solution-165c1819
  #(let [t (map (comp parse-int str) (str %)) c (/ (count t) 2)]
     (= (apply + (take c t)) (apply + (take-last c t)))))

(defcheck solution-170107b5
  (fn balanced? [n]
    (let [digits (map #(- (int %) 48) (str n))
          m (count digits)
          parts (split-at (quot m 2) digits)
          left (parts 0)
          right (if (= (count (parts 0)) (count (parts 1))) (parts 1) (rest (parts 1)))]
      (if (< n 10)
        true
        (= (apply + left) (apply + right))))))

(defcheck solution-174b5d9e
  (fn [n]
    (let [
          seq (map #(- (int %) 48) (str n))
          half-len (int (/ (count seq) 2))]
      (= (reduce + (take half-len seq))
        (reduce + (take-last half-len seq))))))

(defcheck solution-1753605
  (fn balance-of-n [n]
    (letfn[(get-sum
             [s]
             (reduce #( + %1 (- (parse-char %2) (parse-char \0)) ) 0 (subvec s 0 (/ (count s) 2))))]
      (= (get-sum  (vec(str n)))
        (get-sum (vec(reverse (str n))))))))

(defcheck solution-17750750
  (fn [n]
    (let [d (map #(- (int %) 48) (str n))
          c (count d)
          h (/ (if (even? c) c (- c 1)) 2)]
      (= (apply + (take h d))
        (apply + (take-last h d)))
      )))

(defcheck solution-17a53efa
  (fn [n]
    (let [get_dl (fn md [m]
                   (if (= m 0)
                     nil
                     (cons (* (mod m 10) (mod m 10)) (md (int (/ m 10))))))
          l (get_dl n)
          c (count l)
          h (int (/ c 2))
          l1 (take h l)
          l2 (drop (- c h) l)]
      (= (apply + l1) (apply + l2)))))

(defcheck solution-17b0e0be
  #(let [digits (map (fn [c] (- (int c) 48)) (str %))
         len (count digits)
         lhalf (take (int (/ len 2)) digits)
         rhalf (drop (int (/ (inc len) 2)) digits)]
     (= (apply + lhalf) (apply + rhalf))))

(defcheck solution-17dd2f8
  (fn [n]
    (letfn [(f [k] (reduce #(+ % (int %2)) 0 (h k)))
            (h [k] (take (/ (count k) 2) k))]
      (= (f (str n))
        (f (reverse (str n)))))))

(defcheck solution-18242a45
  #(let [s (str %)
         c (count s)
         h (quot c 2)
         f (fn [s]
             (reduce + (map (comp parse-int str) s)))]
     (= (f (take h s)) (f (drop (+ h (rem c 2)) s)))))

(defcheck solution-18a8d1b4
  (fn [n]
    (letfn [(get-digits [n]
              (->> (str n)
                (map (comp parse-int str))))

            (leftright [s]
              [(take (/ (count s) 2) s)
               (take-last (/ (count s) 2) s)])]

      (let [lr (leftright (get-digits n))]
        (= (apply + (first lr))
          (apply + (second lr)))))))

(defcheck solution-18b73d37
  (fn [x]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (str x)))
          half (quot (count digits) 2)]
      (= (reduce + (take half digits)) (reduce + (take half (reverse digits)))))))

(defcheck solution-18be974e
  #(= 0 (apply + (apply map - ((juxt take take-last) (quot (count (str %)) 2) (map int (str %)))))))

(defcheck solution-1941bc4b
  (fn [n]
    (let [c (count (str n))
          s (map #(character-digit % 10) (str n))
          sum #(apply + (take (Math/floor (/ c 2)) %))]
      (= (sum s) (sum (reverse s))))
    ))

(defcheck solution-1985da7c
  (fn [n]
    (let [s (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))
          t (/ (count s) 2)
          l (Math/floor t)
          r (Math/ceil t)]
      (= (apply + (take l s)) (apply + (drop r s)))
      )))

(defcheck solution-19e93d95
  (fn [x]
    (let
     [k (map #(- (int %) 48) (str x))
      n (int (/ (count k) 2))
      l (take n k)
      r (take-last n k)]
      (= (apply + l) (apply + r)))))

(defcheck solution-19f40bc0
  (fn [n]
    (let [split-num
                     (loop [current n results []]
                       (if (= current 0) results
                                         (recur (quot current 10) (conj results (rem current 10)))))
          first-sum #(apply + (take (quot (count %) 2) %))
          second-sum #(apply + (take-last (quot (count %) 2) %))]
      (= (first-sum split-num) (second-sum split-num)))))

(defcheck solution-1a208b0d
  (fn [num]
    (let [numtext (str num)
          howmany (int (/ (count numtext) 2))
          front (take howmany numtext)
          end   (take-last howmany numtext)
          dig   #(- (int %) 48)]
      (= (apply + (map dig front)) (apply + (map dig end))))))

(defcheck solution-1a20c917
  (fn [n]
    (let [s (str n)
          l (subs s 0 (/ (count s) 2))
          r (subs s (/ (inc (count s)) 2))
          f (fn [s]
              (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq s)))]
      (= (apply + (f l))
        (apply + (f r))))))

(defcheck solution-1a568e21
  (fn f
    [n]
    (let [x (map (comp #(#?(:clj Integer/parseInt :cljs js/parseInt) %) str) (seq (str n))) c (int (/ (count x) 2))]
      (= (reduce + (take c x)) (reduce + (drop (- (count x) c) x))))))

(defcheck solution-1a8ab9aa
  (fn [n]
    (let
     [stringified (.toString n)
      len (count stringified)
      low (take (+ (quot len 2) (mod len 2)) stringified)
      hi (drop (quot len 2) stringified)
      digsum (fn [digs] (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (.toString %)) digs)))
      ]
      (= (digsum low) (digsum hi)))))

(defcheck solution-1aab705d
  (fn [n]
    (let [s (str n), l (count s)]
      (= (reduce + (map int (take (Math/ceil (/ l 2)) s)))
        (reduce + (map int (drop (Math/floor (/ l 2)) s)))))))

(defcheck solution-1ac5c6ee
  (fn [n]
    (let [s (vec (map #(parse-int (str %)) (str n)))
          c (count s)
          i (int (/ c 2))
          f #(apply + %)]
      (= (f (subvec s 0 i))
        (f (subvec s (- c i) c))))))

(defcheck solution-1b40874e
  (fn [n]
    (let [s (str n) i (quot (count s) 2)
          f (partial reduce #(+ %1 (- (int %2) 48)) 0)]
      (= (f (take i s)) (f (take-last i s))))))

(defcheck solution-1ba28d77
  (fn [n] (let [string (str n) c2s #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))]
            (loop [lsum 0 rsum 0 s string]
              (if (empty? s) (= lsum rsum)
                             (recur (+ lsum (c2s (first s))) (+ rsum (c2s (last s))) (rest (butlast s)))
                             )
              )
            )))

(defcheck solution-1bf2eb46
  (fn [i](let [i (map int (str i)) c (/ (count i) 2)]
           (= (apply + (take c i)) (apply + (take-last c i))))))

(defcheck solution-1c9da64e
  (fn balanced [n]
    (let [s (-> n str vec)
          l (count s)
          f #(subvec % 0 (quot l 2))]
      (apply = (map (comp frequencies f) [s (vec (reverse s))])))))

(defcheck solution-1d6ba675
  (fn [x]
    (let [c (map #(- (parse-char %) (parse-char \0)) (str x))
          l (quot (count c) 2)
          a (apply + (take l c))
          b (apply + (take l (reverse c)))]
      (= a b))))

(defcheck solution-1dc53005
  (fn [s]
    (let [s (str s)
          c (count s)
          [a b] (split-at (/ c 2) (map str s))
          f (fn [s] (apply + (map parse-int s)))]
      (= (f b) (f (if (odd? c) (butlast a) a))))))

(defcheck solution-1ddafab7
  (fn balanced?
    [n]
    (letfn [(digits [n]
              (lazy-seq
                (if (< n 10)
                  (list n)
                  (cons (rem n 10) (digits (quot n 10))))))]
      (let [ds (digits n)
            n-digits (count ds)
            half (quot n-digits 2)
            first-half (take (if (even? n-digits) half (inc half)) ds)
            last-half (drop half ds)]
        (= (apply + first-half) (apply + last-half))))))

(defcheck solution-1ddfeb77
  (fn [x]
    (let [coll  (->> x
                  (str)
                  (re-seq #".")
                  (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %)))
          n     (int (/ (count coll) 2))
          left  (take n coll)
          right (take-last n coll)]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-1e18642e
  (letfn [
          (to-sequence [n] (map #(- (int %) 48) (seq (str n))))
          (is-balanced [n] (let [s (to-sequence n)
                                 half (Math/ceil (/ (count s) 2))
                                 left (take half s)
                                 right (take-last half s)]
                             (= (apply + left) (apply + right))))]
    is-balanced))

(defcheck solution-1e48e31
  (fn [n]
    (let [nsx (map #(- (parse-char %) (parse-char \0)) (vec (str n)))
          hl (int (/ (count nsx) 2))
          fh (take hl nsx)
          sh (drop (- (count nsx) hl) nsx)]
      (= (reduce + fh) (reduce + sh))
      )))

(defcheck solution-1eb77361
  #(let [s (str %) n (int (/ (+ 1 (count s)) 2)) t take
         p (fn [c] (apply + (map (comp parse-int str) (t n c))))]
     (= (p s) (p (reverse s)))))

(defcheck solution-1ecb487
  (fn number-balanced?
    [n]
    (let [s (str n)
          ls-halve (/ (count s) 2)
          l (subs s 0 ls-halve)
          r (subs (clojure.string/reverse s) 0 ls-halve)]
      (apply = (map (comp #(apply + %) #(map int %)) (list l r))))))

(defcheck solution-1ee711ed
  #(let [s (map (comp (partial + -48) int) (str %))
         n (count s) n* (quot n 2)]
     (apply =
       (map (partial reduce +)
         (take 2 (partition (- n n*) n* s))))))

(defcheck solution-1ee9e92d
  (fn balance? [n]
    (letfn [(digits [n]
              (map
                first
                (take-while
                  #(not= [0 0] %)
                  (drop 1
                    (iterate
                      (fn [[a b]]
                        [(mod b 10) (quot b 10)])
                      [0 n])))))]
      (let [d (digits n)
            f (take (quot (count d) 2) d)
            s (drop (/ (count d) 2) d)]
        (= (reduce + f) (reduce + s))))))

(defcheck solution-1f11463a
  (fn [n]
    (let [digits (map #(- (int %) 48) (str n))
          num-digits (count digits)
          half-num-digits (quot num-digits 2)]
      (=
        (reduce + (take half-num-digits digits))
        (reduce + (take half-num-digits (reverse digits)))))))

(defcheck solution-1fc3c2ef
  #(let [s (map int (str %))
         l (/ (count s) 2)]
     (=
       (apply + (take l s))
       (apply + (take-last l s)))))

(defcheck solution-1feb07f4
  (fn number115 [n]
    (let [ns (map int (str n))
          c (count (str n))]
      (= (apply + (take (/ c 2) ns))
        (apply + (drop (/ (dec c) 2) ns))))))

(defcheck solution-201240c2
  (fn balanced-number? [n]
    (let [digits (map (comp parse-int str) (str n))
          half-len (int (/ (count digits) 2))]
      (= (reduce + (take half-len digits)) (reduce + (take-last half-len digits))))))

(defcheck solution-2021ad53
  (fn [x]
    (let [ds (map #(rem % 10)
               (take-while #(> % 0) (iterate #(quot % 10) x)))
          half (int (/ (inc (count ds)) 2))]
      (= (apply + (take half ds)) (apply + (take half (reverse ds)))))))

(defcheck solution-2028b35
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          m (/ (count digits) 2)]
      (= (apply + (take (int m) digits))
        (apply + (drop m digits))))))

(defcheck solution-209f0719
  (fn balance-of-n
    [n]
    (let [m (str n)
          l (count m)
          ln (quot l 2)
          xs ((if (even? l) (partial partition ln) (partial partition ln (inc ln))) m)]
      (letfn [(sum-of-digits [ys] (apply + (map (comp parse-int str) ys)))]
        (apply = (map sum-of-digits xs))))))

(defcheck solution-20f1a3b4
  (fn balanced?
    [n]
    (let [digits (->> n str (map #(parse-int (str %))))
          halve-count (-> (count digits) (/ 2) (Math/floor) int)]
      (->>
        ((juxt take take-last) halve-count digits)
        (map (partial apply +))
        (apply =)))))

(defcheck solution-2122dcea
  (fn[a](=
          (reduce #(+ % (parse-int (str %2))) 0 (take (/ (count (str a)) 2) (str a)))
          (reduce #(+ % (parse-int (str %2))) 0 (take (/ (count (str a)) 2) (reverse (str a))))
          )
    ))

(defcheck solution-213d7ea9
  (fn [n]
    (let [d (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n)),
          c (count d), hc (/ c 2)]
      (= (apply + (take hc d)) (apply + (take-last hc d))))))

(defcheck solution-21a63b1f
  (fn [x] (let [l (map #(- (int %) 48) (str x)),
                s (let [c (count l),idx (quot c 2)]
                    (if (even? c) (vector (take idx l) (drop idx l))
                                  (list (take idx l) (drop (inc idx) l))))]
            ((fn [[a b]] (or (= a b) (= a (reverse b)))) s))))

(defcheck solution-21d8f67a
  (fn balanced?
    [n] {:pre [(integer? n), (not (neg? n))]}
    (let [
          ;; Constructing the digit sequence of n.
          digits
          (loop [acc [], n n]
            (if (< n 10)
              (cons n acc)
              (recur (cons (mod n 10) acc)
                (quot n 10)))),

          ;; The first and last halves of the digit sequence.  If there are an odd
          ;; number of digits, the center digit (which should not be summed as
          ;; part of either half) is the first element of the tail.
          [head tail]
          (split-at (quot (count digits) 2) digits)]

      (if (even? (count digits))
        (= (apply + head) (apply + tail))
        (= (apply + head) (apply + (rest tail)))))))

(defcheck solution-21f06bf8
  (fn balanced? [n]
    (let [digits (map int (str n))
          size   (quot (count digits) 2)]
      (= (apply + (take size digits))
        (apply + (take-last size digits))))))

(defcheck solution-21f08f58
  #(let [s (map int (str %))
         n (quot (count s) 2)]
     (= (apply + (take n s)) (apply + (take n (reverse s))))))

(defcheck solution-233b910e
  (fn [x]
    (letfn [(ds [y]
              (loop [out [] in y]
                (let [out (conj out (rem in 10))
                      in  (quot in 10)]
                  (if (= in 0)
                    out
                    (recur out in)))))]
      (let [vd (ds x)
            n  (count vd)
            a  (quot n 2)
            b  (if (odd? n) (+ a 1) a)]
        (= (reduce + (take a vd)) (reduce + (drop b vd)))))))

(defcheck solution-23b7a38e
  #(let [q (map int (str %)) l (/ (count q) 2)] (= (apply + (take l q)) (apply + (take-last l q)))))

(defcheck solution-23d1806a
  (letfn
   [(digits [num]
      (if (= num 0) [0]
                    (loop [num num ds ()]
                      (if (> num 0)
                        (recur (quot num 10) (conj ds (rem num 10)))
                        (vec ds)))))]
    (fn [x]
      (let [ds (digits x)
            cnt (count ds)]
        (= (apply + (take (quot cnt 2) ds))
          (apply + (drop (- cnt (quot cnt 2)) ds)))))))

(defcheck solution-23f84316
  (fn [n] (apply = (map #(reduce + %) (let [x (map int (str n)) z (count x) q quot c (q z 2) d (q (inc z) 2)] (partition c d x))))))

(defcheck solution-24622c05
  (letfn [(digits [n]
            (if (< n 10) [n] (conj (digits (quot n 10)) (mod n 10))))]
    (fn [n]
      (let [digs (digits n)]
        (= (reduce + (take (quot (count digs) 2) digs))
          (reduce + (take-last (quot (count digs) 2) digs)))))))

(defcheck solution-24659ff3
  (fn [n]
    (let [half-sum (fn [l] (reduce + (take (quot (count l) 2) l)))
          digits (map (comp parse-int str) (seq (str n)))]
      (= (half-sum digits) (half-sum (reverse digits))))))

(defcheck solution-246e9b43
  (fn [n]
    (letfn [(dig-sum [s]
              (reduce + (map (fn [c] (- (parse-char c) (parse-char \0)))
                          s)))]
      (let [s (str n)
            len (count s)
            l (subs s 0 (quot len 2))
            r (subs s (quot (inc len) 2))]
        (= (dig-sum l) (dig-sum r))))))

(defcheck solution-2485a9f2
  (fn isBalance?[number]
    (loop [q (quot number 10) r (mod number 10) result [r]]
      (if (= q 0)
        (let [takecnt (quot (count result) 2)
              start (apply + (take takecnt result))
              end (apply + (take-last takecnt result))]
          (if (= start end)
            true
            false))
        (let [irem (mod q 10)]
          (recur (quot q 10) irem (conj result irem)))))))

(defcheck solution-251c4b67
  (fn [num]
    (let [dig (fn [n](map #(- (parse-char %) (parse-char \0)) (str n)))
          digits (dig num)
          total (count digits)
          half (int (/ total 2))
          sum #(reduce + 0 %)]
      (= (sum (take half digits)) (sum (drop (- total half) digits))))))

(defcheck solution-25e4243
  (fn [x]
    (let [n (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str x))
          c (count n)
          h (int (/ c 2))]
      (= (apply + (take h n)) (apply + (take-last h n))))))

(defcheck solution-25f560c6
  (fn [n]
    (if (< n 10) true
                 (let [deal (#(partition  (int (Math/ceil (/ (count %) 2))) (int (/ (count %) 2)) %)
                             (map (comp parse-int str) (str n)))]
                   (= (apply + (first deal)) (apply + (second deal)))))))

(defcheck solution-264c5065
  #(let [s (str %) l (quot (count s) 2) f (fn [y xs] (reduce (fn [acc x] (+ acc (int x))) 0 (take y xs)))] (= (f l s) (f l (reverse s)))))

(defcheck solution-264f85ae
  (fn is-balanced [x]
    (let [s (str x)
          c (quot (count s) 2)
          s1 (take c s)
          s2 (take c (reverse s))
          f (fn [r] (reduce + (map #(- (parse-char %) (parse-char \0)) r)))]
      (= (f s1) (f s2)))))

(defcheck solution-2723e40c
  (fn [n] (let [digits (map #((comp parse-int str) %) (seq (str n)))
                hs (int (/ (count digits) 2))]
            (= (apply + (take hs digits)) (apply + (take hs (reverse digits))))
            )))

(defcheck solution-27bbc71c
  (fn [n]
    (let [ds (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          c (count ds)
          h (quot c 2)
          ls (take h ds)
          rs (if (even? c)
               (drop h ds)
               (drop (inc h) ds))]
      (= (reduce + 0 ls) (reduce + 0 rs)))))

(defcheck solution-27bf03f8
  (letfn [
          (digits [n] (map #(- (int %) 48) (str n)))
          (balanced-seq [x] (=
                              (apply + (take (quot (count x) 2) x))
                              (apply + (drop (quot (inc (count x)) 2) x))
                              ))]
    (fn [n] (balanced-seq (digits n)))))

(defcheck solution-28c5da0c
  (fn [n] (let [ds (map (comp parse-int str) (str n)) c (quot (count ds) 2) ns (take c ds) rns (take c (reverse ds))] (= (reduce + ns) (reduce + rns) ))))

(defcheck solution-28d9198f
  (fn [n]
    (let [s (seq (.toString n))
          s-length (count s)
          left (take (/ s-length 2) s)
          right (take-last (/ s-length 2) s)
          sum-digits (fn [d] (apply + (map (fn [x] (#?(:clj Integer/parseInt :cljs js/parseInt) (str x))) d)))]
      (apply = (map sum-digits [left right]))
      )
    ))

(defcheck solution-2917299c
  (fn [n]
    (letfn [(genhalf [chs] (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str chs))))]
      (= (genhalf (take (quot (count (str n)) 2) (seq (str n))))
        (genhalf (drop (quot (inc (count (str n))) 2) (seq (str n))))))))

(defcheck solution-2922683e
  (fn balance-of-n [n]
    (let [ns (map #(character-digit % 10) (str n))
          mid (/ (count ns) 2)]
      (= (apply + (take mid ns)) (apply + (take mid (reverse ns)))))))

(defcheck solution-292d1b12
  (fn bn [n]
    (let [d (map #(mod % 10) (take-while (partial < 0) (iterate #(quot % 10) n)))
          h (int (Math/floor (/ (count d) 2)))]
      (= (reduce + (take h d)) (reduce + (take-last h d))))))

(defcheck solution-2934ba0e
  #(let [s (map (fn [v] (- (int v) 48)) (str %))
         n (count s)
         h (quot n 2)]
     (= (apply + (take h s))
       (apply + (drop (- n h) s))
       )))

(defcheck solution-295ed8b5
  (fn [n]
    (let [f (fn [g] (reduce #(+ % (-> %2 str parse-int)) 0 (g (quot (count (str n)) 2) (seq (str n)))))]
      (= (f take) (f take-last)))))

(defcheck solution-29697e64
  (fn balanced? [s]
    (let [digits (map #(character-digit % 10) (str s)) n (count digits)]
      (= (reduce + 0 (take (Math/floor (/ n 2)) digits))
        (reduce + 0 (drop (Math/ceil (/ n 2)) digits))))))

(defcheck solution-296a6502
  (fn [n] (let [h (count (str n))
                y (seq (str n))
                z (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str y))]

            (cond (= 1 h) true
                  (odd? h) (= (reduce + (take (/ (- h 1) 2) z))
                             (reduce + (take (/ (- h 1) 2) (reverse z))))
                  :else (= (reduce + (take (/ h 2) z))
                          (reduce + (take (/ h 2) (reverse z))))))))

(defcheck solution-298a1729
  (fn [n]
    (let [s (str n)
          c (count s)
          h (quot c 2)
          plus (fn [s c]
                 (+ s (parse-int (str c))))]
      (= (reduce plus 0 (take h s))
        (reduce plus 0 (take-last h s))))))

(defcheck solution-2992a56a
  (fn [n]
    (let [half-size (int (/ (count (str n)) 2))
          chars-to-digits (fn [s] (map #(parse-int (str %)) s))
          left-digits (chars-to-digits (take half-size (str n)))
          right-digits (chars-to-digits (take half-size (reverse (str n))))]
      (= (reduce + left-digits) (reduce + right-digits)))))

(defcheck solution-2a9d9ebf
  (fn [s]
    (let [p (str s)
          k (quot (count (str s)) 2)
          sumup (fn [x](->> (map #(parse-int (str %)) x)
                         (reduce + 0)))
          ]
      (= (sumup (take k p))
        (sumup (take k (reverse p)))))))

(defcheck solution-2acb2b52
  (fn [n]
    (letfn [(ntl [n] (if (zero? n) '() (cons (rem n 10) (ntl (int (/ n 10))))))]
      (let [l (ntl n) h (take (quot (count l) 2) l) t (take (quot (count l) 2) (reverse l))]
        (= (apply + h) (apply + t))))))

(defcheck solution-2b1ce8fc
  (fn balanced [n]
    (let [c (count (str n))
          mid (quot c 2)
          s1 (subs (str n) 0 mid)
          s2 (subs (str n) (if (even? c) mid (inc mid)))
          sum-digits (fn [s] (apply + (map #(- (parse-char %) (parse-char \0)) s)))]
      (= (sum-digits s1) (sum-digits s2)))))

(defcheck solution-2b51d085
  (fn [n]
    (let [ns (seq (str n))
          i (quot (count ns) 2)
          l (apply + (map int (take i ns)))
          r (apply + (map int (drop (+ i (if (even? (count ns)) 0 1)) ns)))]
      (if (zero? i) true
                    (= l r)))))

(defcheck solution-2b75bf89
  (fn [x]
    (apply
      =
      (let [s (seq (str x))
            cnt (count s)
            half (quot cnt 2)]
        (map (fn [s] (apply + (map int s)))
          (if (even? cnt)
            (split-at half s)
            [(take half s) (drop (inc half) s)]))))))

(defcheck solution-2b7cd9fd
  (fn [n]
    (let [s (str n)
          half (fn [n] (+ (int (/ n 2)) (rem n 2)))
          left-half (fn [s] (subs s 0 (half (count s))))
          right-half (fn [s] (subs s (- (count s) (half (count s)))))
          digval (fn [c] (- (parse-char c) (parse-char \0)))
          digsum (fn [s] (apply + (map digval (seq s))))
          ]
      (= (digsum (left-half s)) (digsum (right-half s))))))

(defcheck solution-2b87fca
  (fn[x](zero? (apply + (#(take (quot (count %) 2 ) %) (#(map - % (reverse %) )  (map #(- (int %) 48) (str x))))))))

(defcheck solution-2c17129
  (fn balanced? [n]
    (let [digits (map int (str n))
          half   (quot (count digits) 2)
          left   (take      half digits)
          right  (take-last half digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-2c1892a0
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (seq (str n)))
          half-count (/ (count digits) 2)
          left-sum (reduce + (take half-count digits))
          right-sum (reduce + (take half-count (reverse digits)))
          ]
      (= left-sum right-sum)
      )

    ))

(defcheck solution-2c464356
  #(if (< % 10) true (let [d (count (str %))]
                       (apply = (map (fn [c] (reduce (fn [x y]
                                                       (+ x (int y) -48)) 0 c))
                                  (partition (quot d 2) (quot (inc d) 2) (str %)))))))

(defcheck solution-2c4fd72a
  (fn [n]
    (let [digits (map parse-int (re-seq #"\d" (str n)))
          len (count digits)]
      (= (reduce + (take (quot len 2) digits))
        (reduce + (drop (/ len 2) digits))))))

(defcheck solution-2cef08a8
  (fn balanced? [x]
    (let [s    (map #(- (parse-char %) (parse-char \0)) (seq (str x)))
          c    (int (/ (count s) 2))
          s1   (take c s)
          s2   (take-last c s)
          sum1 (apply + s1)
          sum2 (apply + s2) ]
      (= sum1 sum2) )))

(defcheck solution-2d00d58e
  (fn [x]
    (let [digits (fn [i] (loop [acc nil
                                a i]
                           (if (zero? a)
                             acc
                             (recur (conj acc (rem a 10)) (int (/ a 10))))))
          n (digits x)
          l (int (/ (count n) 2))]
      (= (reduce + (take l n)) (reduce + (take-last l n))))))

(defcheck solution-2d7a833f
  (fn [n]
    (letfn [(sum-str [s] (reduce + (map int s)))
            (half
              [s]
              (let [cnt (count s)
                    step (if (odd? cnt) 1 0)
                    sz (int (/ cnt 2))]
                (partition sz (+ step sz) s)))]
      (->>
        n
        str
        half
        (map sum-str)
        (apply =)))))

(defcheck solution-2dab7e11
  (fn balanced? [n]
    (letfn [(decimal-seq [n]
              (if (zero? n)
                []
                (conj (decimal-seq (quot n 10)) (mod n 10))))]
      (let [digits (decimal-seq n)
            cnt (quot (count digits) 2)]
        (if (zero? n)
          true
          (= (apply + (take cnt digits)) (apply + (take-last cnt digits))))))))

(defcheck solution-2dbdf88e
  (fn [n]
    (let [nstr (str n)
          len (count nstr)
          half-len (int (/ len 2))
          left (subs nstr 0 half-len)
          right (subs nstr (- len half-len) len)
          left-sum (reduce #(+ %1 (parse-int (str %2))) 0 (seq left))
          right-sum (reduce #(+ %1 (parse-int (str %2))) 0 (seq right))
          ]
      (= left-sum right-sum)
      ) ; let
    ))

(defcheck solution-2dc61114
  (fn balanced? [n]
    (let [s (str n)
          t (bit-shift-right (count s) 1)
          f #(apply + (map int (take t %)))]
      (= (f s) (f (reverse s))))))

(defcheck solution-2dca22df
  (fn [n]
    (let [s (map #(- (int %) 48) (str n))
          n (quot (count s) 2)]
      (= (apply + (take n s)) (apply + (take n (reverse s)))))))

(defcheck solution-2eb24abf
  (fn xx [x]
    ( let [ col (map (fn [x] (- (int x) 48)) (str x))
           total_len (count col)
           half_len (if (odd? total_len) (quot (+ total_len 1) 2) (/ total_len 2))
           ]
      (= (reduce + (take half_len col ))
        (reduce + (drop  (- total_len half_len) col ))
        )
      )
    ))

(defcheck solution-2ec7cf82
  (fn [n]
    (let [a (map #(- (parse-char %) (parse-char \0))  (str n))
          c (count a)
          f (int (/ c 2))
          m (if (odd? c) 1 0)
          ]
      (= (reduce + (take f a)) (reduce + (drop (+ f m) a)))
      )
    ))

(defcheck solution-2f3f3d03
  #(let [s (map (comp parse-int str) (str %))
         i (/ (count s) 2)]
     (= (apply + (take i s)) (apply + (take-last i s)))))

(defcheck solution-2f45cc06
  (fn[n]
    (let [chr->int #(- (parse-char %) (parse-char \0))]
      (loop [s (str n)
             lhs 0
             rhs 0]

        (cond
          (<= (count s) 1)
          (= lhs rhs)

          :else
          (let [l (chr->int (first s))
                r (chr->int (last s))
                s' (rest (butlast s))]
            (recur s' (+ lhs l) (+ rhs r))))))))

(defcheck solution-303b5cf4
  (fn [n]
    (let [s (map #(parse-char %) (str n))
          l (quot (count s) 2)]
      (= (apply + (take l s)) (apply + (take l (reverse s)))))))

(defcheck solution-308377f3
  (fn [x]
    (let [n (map #(- (int %) 48) (str x))
          f #(apply + (take (/ (count n) 2) %))]
      (= (f n) (f (into () n))))))

(defcheck solution-310f734d
  (fn [n]
    (let [digits (re-seq #"." (str n))
          cnt (count digits)
          fh (take (quot cnt 2) digits)
          sh (drop (if (even? cnt) (quot cnt 2) (inc (quot cnt 2))) digits)
          sum (fn [ds] (apply + (map parse-int ds)))]
      (= (sum fh) (sum sh)))))

(defcheck solution-311b60e6
  (fn [n]
    (let [half (fn [s] (map #(parse-int (str %)) (subs s 0 (quot (count s) 2))))
          sum #(reduce + %)]
      (= (sum (half (str n))) (sum (half (apply str (reverse (str n)))))))))

(defcheck solution-31aae4e1
  (fn [n]
    (let [digits (map (comp parse-int str) (str n))
          d (quot (count digits) 2)]
      (apply = (map #(apply + %) [(take d digits) (take d (reverse digits))])))))

(defcheck solution-31f72c40
  (fn balanced? [n]
    (letfn [(take-half [s] (take (quot (count s) 2) s))]
      (= (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (take-half (str n))))
        (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (take-half (reverse (str n)))))))))

(defcheck solution-3221c86d
  (fn f [x]
    (let [s (str x) n (count s)]
      (cond
        (= 1 n) true
        (even? n) (apply = (map #(reduce (fn [a b] (+ a (- (int b) 48))) 0 %) (partition (/ n 2) s)))
        :else (f (#?(:clj Integer/parseInt :cljs js/parseInt) (apply str (for [y (range n) :when (not= y (/ (dec n) 2))] (nth s y)))))))))

(defcheck solution-32b32e3b
  (fn balance? [n]
    (letfn [(to-digits [n, v]
              (if (= 0 n)
                v
                (recur (quot n 10) (conj v (rem n 10)))))]
      (if (< n 10)
        true
        (let [v (to-digits n []), cv (count v), pl (if (odd? cv) (inc (quot cv 2)) (quot cv 2))]
          (->> (partition-all pl v)
            (apply map -)
            (reduce +)
            (= 0)))))))

(defcheck solution-32bd1d2c
  (fn [n]
    (letfn [(n2v [n] (loop [n n r []] (if (< n 10) (conj r n) (recur (quot n 10) (conj r (mod n 10))))))]
      (let [v (n2v n)
            f (take (quot (count v) 2) v)
            r (take-last (quot (count v) 2) v)]
        (= (apply + f) (apply + r))))))

(defcheck solution-32bfd05e
  (fn [n]
    (let [d (reverse (map #(mod % 10) (take-while (partial < 0) (iterate #(quot % 10) n))))
          c (quot (count d) 2)]
      (apply = (map (partial reduce +) [(take c d) (take-last c d)])))))

(defcheck solution-32f8175a
  (fn [n]
    (let [f (fn f [n l]
              (if (zero? n)
                (if (empty? l) [0] l)
                (recur (quot n 10) (conj l (rem n 10)))))
          coll (f n [])
          half (quot (count coll) 2)]
      (= (reduce + (take half coll))
        (reduce + (take half (reverse coll)))))))

(defcheck solution-3314fa29
  (fn [n]
    (let
     [d (if (= 0 n) '(0)
                    (->> n
                      (iterate #(quot % 10))
                      (take-while #(> % 0))
                      (map #(mod % 10))
                      reverse))]
      (reduce =
        (map #(reduce + %)
          [(take (/ (count d) 2) d) (take-last (/ (count d) 2) d)])))))

(defcheck solution-3391ccd4
  (fn balanced? [n]
    (let [nums (into [] (map #(- (parse-char %) (parse-char \0)) (seq (str n)))) length (count nums) half-length (quot length 2)]
      (= (reduce + (subvec nums 0 half-length))
        (reduce + (subvec nums (- length half-length)))))))

(defcheck solution-3399f06d
  (fn [n]
    (let [ns (->> n
               (iterate #(quot % 10))
               (take-while pos?)
               (map #(mod % 10))
               (into []))
          i (/ (count ns) 2)]
      (zero?
        (reduce -
          (reduce + (take (Math/floor i) ns))
          (drop (Math/ceil i) ns))))))

(defcheck solution-34014836
  #(let [s (map int (str %))
         l (/ (count s) 2)]
     (= (apply + (take (Math/floor l) s))
       (apply + (drop (Math/ceil l) s)))))

(defcheck solution-3492db42
  (fn [n]
    (letfn [(sum [s]
              (apply + (map #(parse-char %) s)))]
      (let [y (str n) l (/ (count y) 2)]
        (= (sum (take l y)) (sum (take-last l y)))))))

(defcheck solution-34d1005c
  (fn [x] (let
           [xs (map #(- (int %) 48) (str x))
            half (quot (count xs) 2)
            sum (fn [cs] (reduce + cs))]
            (= (sum (take half xs)) (sum (take half (reverse xs)))))))

(defcheck solution-35b1fce2
  #(let [s (str %)]
     (let [[a b] (-> s count (quot ,,, 2) (split-at ,,, s))]
       (->> (map - (map int a) (map int (reverse b)))
         (apply +)
         zero?))))

(defcheck solution-35dd0d47
  (fn [digits]
    (let [coll (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str digits)))
          n (int (/ (count coll) 2))
          left (take n coll)
          right (take n (reverse coll))
          ]
      (= (reduce + left ) (reduce + right))
      )
    ))

(defcheck solution-360d362c
  (fn f [n]
    (letfn [(digits
              [n]
              (->> (str n)
                (map #(- (parse-char %) (parse-char \0)))))]
      (let [d (digits n)]
        (let [c (quot (count d) 2)]
          (= (apply + (take c d)) (apply + (take c (reverse d)))))
        ))))

(defcheck solution-36769ae2
  (fn balanced [num]
    (let [digits (fn [num]
                   (loop [n num, res []]
                     (if (= n 0) res
                                 (recur (quot n 10) (conj res (mod n 10))))))
          d (digits num)
          half (quot (count d) 2)]
      (= (reduce + 0 (take half d)) (reduce + 0 (take half (reverse d)))))))

(defcheck solution-3678561d
  (fn [s]
    (let [s (map int (str s))
          parts ((juxt take take-last) (/ (count s) 2) s)
          dsum (fn [s] (reduce + (map #(- % (parse-char \0)) s)))]
      (apply = (map dsum parts)))))

(defcheck solution-367917eb
  (fn [x]
    (letfn
     [
      (ds [n]
        (reduce
          +
          (map
            #(- (parse-char %) (parse-char \0))
            n
            )
          )
        )
      ]
      (=
        (ds (take (int (/ (count (str x)) 2)) (str x)))
        (ds (drop (Math/ceil (/ (count (str x)) 2)) (str x)))
        )
      )
    ))

(defcheck solution-36d66563
  (fn [x]
    (let[il (map #(- (parse-char  %) (parse-char \0)) (str x))
         sizediv2 (quot (count il) 2)]
      (=
        (apply + (take sizediv2 il))
        (apply + (take-last sizediv2 il))))))

(defcheck solution-36f1c236
  (fn [x]
    (let [s (str x)
          n (count s)
          [a c] (split-at (quot n 2) s)
          b (if (odd? n) (next c) c)
          f #(reduce + (map int %))]
      (= (f a) (f b)))))

(defcheck solution-36fa8c4d
  #(let[s (map int (str %))
        f (fn[x] (apply + (take (/ (count s) 2)  x)))]
     (= (f s) (f (reverse s)))))

(defcheck solution-372b75b3
  #(let [xs (map parse-int (map str (str %)))
         n (/ (count xs) 2)]
     (= (apply + (take n xs)) (apply + (take-last n xs)))))

(defcheck solution-37515b6e
  (fn
    [x]
    (let [to-digits (fn [n]
                      (map #(- (int %) 48) (seq (str n))))
          as-digits (to-digits x)
          length (/ (count as-digits) 2)
          first-half (take (int length) as-digits)
          second-half (drop (int (+ length 0.5)) as-digits)]
      (=
        (reduce + first-half)
        (reduce + second-half)))))

(defcheck solution-37aaa330
  (fn balanced-number? [n] (let [digits (map #(character-digit % 10) (str n)) numdigits (count digits) halfsize (if (even? numdigits) (/ numdigits 2) (inc (int (/ numdigits 2))))] (if (= (reduce + (take halfsize digits)) (reduce + (drop (- numdigits halfsize) digits))) true false))))

(defcheck solution-3815fe92
  (fn [n]
    (let [sn (str n)
          l (count sn)
          c (quot l 2)
          sd (fn [s] (reduce + (map (comp parse-int str) s)))]
      (->> sn
        ((juxt #(subs % 0 c) #(subs % (- l c) l)))
        (map sd)
        (apply =)))))

(defcheck solution-38584b38
  (fn [s]
    (let [digits (map (comp parse-int str) (seq (str s)))
          half (/ (count digits) 2)]
      (let [left (reduce + (take half digits))
            right (reduce + (take half (reverse digits)))]
        (= left right)))

    ))

(defcheck solution-389da6a1
  (fn balance [n]
    (let [char->int (fn [c] (- (parse-char c) (parse-char \0)))
          first-half (fn [l] (take (/ (count l) 2) l))
          digits (map char->int (str n))]
      (= (reduce + (first-half digits))
        (reduce + (first-half (reverse digits)))))))

(defcheck solution-38e1f069
  (letfn [(half [s] (drop (quot (count s) 2) s))
          (sum [s] (reduce + (map #(- (int %) 48) (half s))))]
    #(= (sum (str %)) (sum (reverse (str %))))))

(defcheck solution-38f0a7ab
  (fn [x]
    (let [digits (map (comp #(#?(:clj Integer/parseInt :cljs js/parseInt) %) str) (str x))
          half-sum (fn [xs] (reduce + (take (quot (count xs) 2) xs)))]
      (apply = (map half-sum [digits (reverse digits)])))))

(defcheck solution-38fc9317
  (fn [n]
    (letfn [(splt[]
              (let [ss (seq (str n))
                    cnt (count ss)
                    head (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (take (/ (dec cnt) 2) ss))
                    tail (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (drop (/ cnt 2) ss))]
                [head tail]))]
      (let [[h t] (splt)]
        (= (reduce + h) (reduce + t))))))

(defcheck solution-3953643d
  (fn [x]
    (let [x (mapv #(- (int %) 48) (str x))
          s #(apply + (take (quot (count x) 2) %))]
      (= (s x) (s (rseq x))))))

(defcheck solution-396cac7
  (fn [n]
    (let [
          s (str n)
          nums (map #(int (- (parse-char %) (parse-char \0))) s)
          size (count s)
          t (int (/ size 2))
          d (int (/ (inc size) 2))]
      (= (reduce + (take t nums)) (reduce + (drop d nums))))))

(defcheck solution-39789b9e
  (fn balanced? [i]
    (let [is (str i)
          half (int (/ (count is) 2))
          front (map #(- (int %) 48) (take half is))
          rear (map #(- (int %) 48) (take-last half is))]
      (= (reduce + front) (reduce + rear)))))

(defcheck solution-39a868dc
  (fn balance? [x] (let [digit (map #(- (int %) 48) (str x))
                         sum #(reduce + %)
                         n (count digit)]
                     (=
                       (sum (take (/ n 2) digit))
                       (sum (take (/ n 2) (reverse digit)))))))

;; See CLJS-2462
#_(defcheck solution-3a0ed628
  #(let [v (vec (map int (str %)))
         len (count v)
         half (/ len 2)
         left (subvec v 0 half)
         right (subvec v (+ half (mod len 2)))]
     (= (reduce + left) (reduce + right))))

(defcheck solution-3a30cf2f
  (fn balanced?
    [n]
    (let [s (str n)
          mid (quot (count s) 2)
          ns (map (comp parse-int str) s)]
      (= (apply + (take mid ns)) (apply + (take mid (reverse ns)))))))

(defcheck solution-3a56a031
  (fn [n]
    (letfn [(digits [n] (map #(parse-char %) (str n)))
            (sum [ds] (reduce + ds))]
      (let [ds (digits n)
            h (quot (count ds) 2)]
        (= (sum (take h ds)) (sum (take h (reverse ds))))))))

(defcheck solution-3a954bcf
  (fn [n]
    (letfn [(char-to-digit [c] (- (parse-char c) (parse-char \0)))
            (sum-of-digits [s] (apply + (map char-to-digit s)))]
      (let [s (str n)
            len (count s)]
        (= (sum-of-digits (.substring s 0 (/ len 2)))
          (sum-of-digits (.substring (str s) (/ (if (even? len) len (inc len)) 2))))))))

(defcheck solution-3ab6c07a
  (fn balanced? [n]
    (let [sum (fn [coll]
                (reduce (fn [a i] (+ a (#?(:clj Integer/parseInt :cljs js/parseInt) (.toString i)))) 0 coll))
          nstr (str n)
          mid (int (/ (count nstr) 2))]
      (= (sum (take mid nstr)) (sum (take-last mid nstr))))))

(defcheck solution-3ae0ddbf
  (fn [n]
    (letfn [(digits [x] (if (zero? x) [] (conj (digits (quot x 10)) (rem x 10))))]
      (let [d (digits n) half (quot (count d) 2)]
        (= (apply + (take half d))
          (apply + (take half (reverse d))))))))

(defcheck solution-3ae35d4f
  (fn balanced? [n]
    (letfn [(to-digits [n]
              (map #(- (parse-char %) (parse-char \0))  (seq (str n))))]
      (let [vn (to-digits n)
            svn (split-at (/ (count vn) 2) vn)]
        (= 0 (reduce  + (map #(- %1 %2) (first svn) (second svn))))))))

(defcheck solution-3b4f8921
  (fn [n]
    (if (< n 10) true
                 (let [s (seq (str n))
                       q (quot (count s) 2)
                       l (take q s)
                       r (nthrest s (- (count s) q))]
                   (letfn [(f [x] (reduce #(+ (character-digit % 10) (character-digit %2 10)) x))]
                     (= (f l) (f r)))))))

(defcheck solution-3b7cd869
  (fn [x]
    (let [digits  (fn d [x]
                    (if (> 10 x)
                      [x]
                      (conj (d (quot x 10)) (mod x 10))))
          halflen (fn [s] (quot (count s) 2))
          cleave  (fn [s] (let [n (halflen s)] [(take n s)(take-last n s)]))
          sum     (fn [s] (apply + s))]
      (apply = (map sum (-> x digits cleave))))))

(defcheck solution-3b87b2aa
  (fn [s]
    (let [sum-digits (fn [digits]
                       (apply + (map #(parse-int (str %)) digits)) )
          s (str s)
          len (Math/floor (/ (count s) 2))]
      (= (sum-digits (take len s)) (sum-digits (take-last len s))))))

(defcheck solution-3bc8a33f
  (fn [n]
    (let [a (map #(- (int %) 48) (str n))
          f #(apply + (first (split-at (/ (count a) 2) %)))]
      (= (f a) (f (reverse a))))))

(defcheck solution-3c517870
  (fn [n]
    (let [s (seq (str n))
          c (count s)
          i (int (/ c 2))
          y (take i s)
          z (drop (+ i (rem c 2)) s)
          f #(+ % (character-digit %2 10))]
      (= (reduce f 0 y) (reduce f 0 z)))))

(defcheck solution-3cd3ff92
  (fn palsum
    [s]
    (let [s (str s), half (/ (count s) 2)]
      (reduce = (map (partial apply +)
                  (map #(map int %) [(take half s)(take-last half s)]))))))

(defcheck solution-3ceba4f4
  (fn [n]
    (let [ns (str n)
          hc (Math/ceil (/ (count ns) 2))]
      (= (reduce + (map int (take hc ns)))
        (reduce + (map int (take hc (reverse ns))))))))

(defcheck solution-3d124e31
  (fn [n]
    (let [ds (str n)
          length (count ds)
          half (quot length 2)
          head (take half ds)
          middle (if (odd? length) 1 0)
          tail (drop (+ half middle) ds)
          hsum (reduce + (map #(- (parse-char %) (parse-char \0)) head))
          tsum (reduce + (map #(- (parse-char %) (parse-char \0)) tail))
          ]
      ;[(= hsum tsum) half length hsum tsum head tail ds]
      (= hsum tsum)
      )
    ))

(defcheck solution-3d59c83c
  (fn [x]
    (let [digits (map #(parse-char %) (seq (str x)))
          center (quot (count digits) 2)]
      (= (reduce + (last (split-at center digits)))
        (reduce + (last (split-at center (reverse digits))))))))

(defcheck solution-3e7be9f0
  (fn balanced?
    [n]
    (let [string-n (str n)
          digits (count string-n)
          first-half (map #(parse-int (str %)) (take (Math/floor (/ digits 2)) string-n))
          second-half (map #(parse-int (str %)) (drop (/ digits 2) string-n))]
      (= (reduce + first-half) (reduce + second-half)))))

(defcheck solution-3eb5ffd4
  #(let [s (str %)
         f (fn [s] (sort (take (/ (count s) 2) s)))]
     (= (f s) (f (reverse s)))))

(defcheck solution-3f20680c
  (fn [n]
    (let [s (str n)
          x (quot (count s) 2)
          headx (take x s)
          tailx (take x (reverse s))
          headnumber (map (comp parse-int str) headx)
          tailnumber (map (comp parse-int str) tailx)]
      (= (reduce + 0 headnumber)
        (reduce + 0 tailnumber)))))

(defcheck solution-3f239442
  (fn balanced [n]
    (loop [n-list (map parse-int (map str (str n)))]
      (cond
        (>= 1 (count n-list)) true
        (and (= 2 (count n-list))
             (= (first n-list) (last n-list))) true
        (= (first n-list) (last n-list)) (recur (rest (butlast n-list)))
        (and (<= 3 (count n-list))
             (= (second n-list) (last n-list))
             (= (first n-list) (last (butlast n-list)))) true
        :else false))))

(defcheck solution-3f38d9d5
  (fn [i]
    (let [n (str i)
          x (/ (count n) 2)
          lhs (take x n)
          rhs (drop (if (odd? (count n)) (dec x) x) n)
          f (fn [x] (reduce #(+ % (parse-int (str %2))) 0 x))]
      (= (f lhs) (f rhs)))))

(defcheck solution-3fc1413a
  (fn __
    [number]
    (letfn [(digits [n]
              (->> n
                str
                (map str)
                (map #(parse-int %))))
            (split-at-middle [coll]
              (let [size (count coll)
                    corrector (mod size 2)
                    middle (quot size 2)]
                [(take (+ middle corrector) coll) (drop middle coll)]))]
      (->> number digits split-at-middle (map (partial reduce +)) (reduce =)))))

(defcheck solution-3fcd4cf
  (fn balanced? [x]
    (let [digits (map #(parse-int (str %)) (str x))
          l (count digits)
          half (if (even? l) (quot l 2) (inc (quot l 2)))
          left (take half digits)
          right (drop (- l half) digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-4005fce0
  (fn [n]
    (let [digits (->> (str n) (map str) (map parse-int))
          num-digits (count digits)
          half-digits (/ num-digits 2)
          first-half (take half-digits digits)
          last-half (->> (reverse digits) (take half-digits))
          sum (partial reduce +)]
      (= (sum first-half) (sum last-half)))))

(defcheck solution-405a8e4d
  (fn [n]
    (letfn [(digits [num]
              (loop [acc ()
                     num num]
                (if (< num 10)
                  (conj acc num)
                  (recur (conj acc (rem num 10))
                    (quot num 10)))))]
      (let [d (digits n)
            c-by-2 (int (/ (count d) 2))
            a (take c-by-2 d)
            b (take-last c-by-2 d)]
        (= (reduce + a)
          (reduce + b))))))

(defcheck solution-407510ab
  (fn [n]
    (let [strn (str n)
          upto (int (/ (count  strn) 2))
          from (if (odd? (count strn)) (inc upto) upto)
          l (map int (take upto strn))
          r (map int (drop from strn))]
      (= (apply + l) (apply + r)))))

(defcheck solution-4092ad55
  (fn[n]
    (let [s (str n) c (-> s count (/ 2) (+ 0.5) int)]
      (#(= (reduce + (map (comp parse-int str) %1))
          (reduce + (map (comp parse-int str) %2)))
       (take c s)
       (take c (reverse s))))))

(defcheck solution-40936fb
  (fn isNBalanced [num]
    (let
     [
      digits (seq (str num))
      size (count digits)
      half (int (/ size 2))
      other (- size half)
      sumDigits (fn [nums] (reduce + 0 (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) nums)))
      left (sumDigits (take half digits))
      right (sumDigits (drop other digits))
      result (= left right)
      ]
      result
      )
    ))

(defcheck solution-40c4e68
  (fn [x]
    (letfn [(split-digits [digits]
              (when (> digits 0)
                (cons (mod digits 10) (split-digits (int (/ digits 10))))))]
      (let [digits (split-digits x)
            half (int (/ (count digits) 2))]
        (= (apply + (take half digits)) (apply + (take-last half digits)))))))

(defcheck solution-40ef41c7
  (fn [n]
    (let [s (str n) c (/ (count s) 2) l (take (int c) s) r (drop (int (+ 0.5 c)) s)]
      (= (reduce + (map int l)) (reduce + (map int r))))))

(defcheck solution-4120ecbb
  (fn x
    [v]
    (let [a (map (comp parse-int str) (str v))
          c (count a)
          h (/ c 2)]
      (apply =
        (map (partial reduce +)
          (if (odd? c)
            [(take h a) (drop (dec h) a)]
            [(take h a) (drop h a)]))))))

(defcheck solution-4126e9a4
  (fn [n]
    (let [s (str n)
          len (quot (count s) 2)
          sum (partial reduce #(+ %1 (parse-int (str %2))) 0)]
      (= (sum (take len s) )
        (sum (take-last len s))))))

(defcheck solution-4210358a
  (letfn [(digits [n]
            (let [radix 10]
              (reverse
                (map
                  #(int (rem % radix))
                  (take-while (complement zero?)
                    (iterate #(quot % radix)
                      n))))))
          (done? [next-sum]
            (= 2 (count next-sum)))
          (next-sum [[left-sum right-sum the-rest]]
            (if (empty? the-rest)
              [left-sum right-sum]
              (let [next-left (+ left-sum (first the-rest))
                    next-right (+ right-sum (last the-rest))
                    next-rest (rest (butlast the-rest))]
                [next-left next-right next-rest])))
          (figure-sums [val]
            (last
              (take-while (complement done?)
                (iterate next-sum [0 0 (digits val)]))))]
    (fn [val]
      (let [[left-sum right-sum _] (figure-sums val)]
        (= left-sum right-sum)))))

(defcheck solution-4321a533
  (fn [i] (let [c (map #(- (int %) 48) (str i))
                f #(take (quot (count %) 2) %)]
            (= (apply + (f c)) (apply + (f (reverse c)))))))

(defcheck solution-432bcf7d
  (fn balanced? [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          half-count (int (/ (count digits) 2))
          first-n #(map (partial nth %1) (range %2))]
      (= (apply + (first-n digits half-count))
        (apply + (first-n (reverse digits) half-count))))))

(defcheck solution-43486922
  (fn [n]
    (if (= (reduce + (take (int (/ (count (str n)) 2)) (map parse-int (map str (apply vector (str n))))))
          (reduce + (take (int (/ (count (str n)) 2)) (map parse-int (map str (apply vector (reverse (str n))))))))
      true
      false)))

(defcheck solution-434d93db
  (fn balanced [i]
    (let [n (count (str i))]
      (cond
        (= n 1) true
        (even? n) ((fn [[s1 s2]] (= (apply + (map #(parse-int (str %)) s1)) (apply + (map #(parse-int (str %)) s2))))[(take (/ n 2) (str i)) (drop (/ n 2) (str i))])
        :else ((fn [[s1 s2]] (= (apply + (map #(parse-int (str %)) s1)) (apply + (map #(parse-int (str %)) s2))))[(take (/ (dec n) 2) (str i)) (drop (inc (/ (dec n) 2)) (str i))])))))

(defcheck solution-43be6d15
  (fn [n]
    (let [seq-n (seq (str n))
          seq-n-int (map #(parse-int %) (map str seq-n))
          len (count seq-n)
          half-len (int (* 0.5 len))
          sum-left (apply + (take half-len seq-n-int))
          sum-right (apply + (drop (- len half-len) seq-n-int))]
      (= sum-left sum-right))))

(defcheck solution-43e59941
  (fn [e]
    (let
     [x (str e)
      half (quot (count x) 2)
      toi #(#?(:clj Integer/parseInt :cljs js/parseInt) (str (or % 0)))]
      (if (> half 0)
        (->> [(take half x) (take-last half x)]
          (map (fn [[a b]] (+ (toi a) (toi b))))
          (reduce =))
        true))))

(defcheck solution-464d498a
  #(let [d (map int (str %))
         n (quot (count d) 2)
         f (fn [x] (apply + (take n x)))]
     (= (f d)
       (f (reverse d)))))

(defcheck solution-465346bb
  (fn __ [n]
    (let [s (str n)
          c (count s)
          h (int (/ c 2))]
      (->> [(take h s) (drop (if (odd? c) (inc h) h) s)]
        (map (fn [x] (map #(parse-int (str %)) x)))
        (map (partial apply +))
        (apply =)))))

(defcheck solution-46538a1e
  (fn [n]
    (let [digits (map (comp int parse-int str) (str n))
          size (/	(count digits) 2)
          sum-front (apply + (take size digits))
          sum-back (apply + (take size	(reverse digits)))]
      (= sum-front sum-back))))

(defcheck solution-468901ab
  (fn [n]
    (let [l (map #(- (int %) 48) (str n))
          c (int (/ (count l) 2))
          [x _] (partition c l)
          [y _] (partition c (reverse l))]
      (if (= (- (apply + x) (apply + y)) 0) true false))))

(defcheck solution-46d9d53d
  (fn balanced [n]
    (let [s (seq (str n))
          c (count s)
          char->int (fn [c] (- (parse-char c) (parse-char \0)))
          side-sum (fn [s] (reduce + (map char->int s)))]
      (= (side-sum
           (take (if (even? c)
                   (/ c 2)
                   (/ (dec c) 2))
             s))
        (side-sum
          (drop (/ c 2) s))))))

(defcheck solution-472ff775
  (fn balanced? [n]
    (let [s (str n)
          len (quot (count s) 2)
          start (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 (take len s))
          end (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 (take len (reverse s)))]
      (= start end))))

(defcheck solution-4736261d
  #(= (set (take (quot (count (str %)) 2) (str %)))
     (set (take (quot (count (str %)) 2) (reverse (str %))))))

(defcheck solution-47502ec3
  (fn [v]
    (let [sv (str v)
          cv (count sv)
          asdig (map #(- (parse-char %) (parse-char \0)) (map identity sv))
          fh  (take (if (odd? cv) (dec ( / cv 2)) (/ cv 2)) asdig)
          lh  (drop (/ cv 2) asdig)
          ]
      (= (reduce + fh) (reduce + lh)))))

(defcheck solution-4762fe22
  (fn [n]
    (letfn [(left-right [n]
              (let [nums (str n) cnt (count nums) half (int (/ cnt 2.0))]
                (if (odd? cnt)
                  (vector (take half nums) (drop (inc half) nums))
                  (vector (take half nums) (drop  half nums)) )))

            (get-sum [l]
              (reduce (fn [ret this] (+ ret (- (int this) 48))) 0 l))]
      (let [ls (left-right n)
            l (get-sum (first ls))
            r (get-sum (last ls))]
        (= l r)))))

(defcheck solution-47a66d6b
  (fn is-balanced? [n]
    (letfn [(digits [n]
              (if (< n 10)
                [n]
                (conj (digits (/ (- n (mod n 10)) 10))
                  (mod n 10))))
            (balanced? [coll left-sum right-sum]
              (if (empty? coll)
                (= left-sum right-sum)
                (recur (rest (butlast coll))
                  (+ left-sum (first coll))
                  (+ right-sum (last coll)))))]
      (balanced? (digits n) 0 0))))

(defcheck solution-47a905f2
  (fn [n]
    (let [digs (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          n-e (int (Math/floor (/ (count digs) 2)))
          lft (take n-e digs)
          rgt (drop (if (odd? (count digs)) (inc n-e) n-e) digs)]
      (= (reduce + lft) (reduce + rgt)))))

(defcheck solution-47b19f27
  (fn [n]
    (let [xs (->> (str n)
               seq
               (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))))
          h  (/ (count xs) 2)]
      (= (reduce + (take h xs))
        (reduce + (take h (reverse xs)))))))

(defcheck solution-47bfac15
  (fn [n] (let [sn (str n) isn (reverse sn) h (int (/ (count sn) 2))]
            (if (= (reduce + (map #(character-digit % 10) (take h sn)))
                  (reduce + (map #(character-digit % 10) (take h isn)))
                  ) true false )
            )))

(defcheck solution-48106bb7
  (fn [n]
    (let [nsq (map int (seq (str n))) hlf (quot (/ (count nsq) 2) 1)
          [l r-raw] (split-at hlf nsq) r (if-not (= (count l) (count r-raw)) (rest r-raw) r-raw)
          f (fn sum [s] (reduce + s))]
      (= (f l) (f r)))))

(defcheck solution-484407ee
  (fn balanced-number
    [n]
    (let [s (map
              (fn char-to-int [c] (- (int c) 48))
              (into [] (str n)))
          sum-of-first-half-of-seq
            #(reduce + (take (Math/ceil (quot (count s) 2)) %))]
      (= (sum-of-first-half-of-seq s)
        (sum-of-first-half-of-seq (reverse s))))))

(defcheck solution-48495006
  (fn [i]
    (let [s (map #(parse-int (str %)) (seq (str i)))
          aprox (int (quot (count s) 2))]
      (= (reduce + (take aprox s)) (reduce + (take aprox (reverse  s))) )
      )
    ))

(defcheck solution-48692ee1
  (fn [n]
    (->> n
      str
      seq
      (map #(parse-char ^char %))
      ((fn [coll]
         (let [cnt (count coll)
               to-take (int (/ cnt 2))
               to-drop (if (odd? cnt) (inc to-take) to-take)]

           (= (reduce + (take to-take coll))
             (reduce + (drop to-drop coll))))))
      )))

(defcheck solution-4874a1c
  #(let [s (map int (str %))
         l (quot (count s) 2)
         f (fn [t] (apply + (take l t)))]
     (= (f s) (f (reverse s)))))

(defcheck solution-48a18eed
  (fn [i] (let [a (str i) x (/ (count a) 2) f #(apply + (map int (take % %2)))] (= (f x a) (f x (reverse a)) ))))

(defcheck solution-48b461e
  (fn [n] (let [d (loop [x n d []] (if (> x 0) (recur (quot x 10) (conj d (rem x 10))) d)) c (quot (count d) 2)] (= (apply + (take c d)) (apply + (take c (reverse d)))))))

(defcheck solution-4900bedf
  (fn [n]
    (let [ds (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (str n)))
          h (/ (count ds) 2)
          l (take h ds)
          r (take h (reverse ds))]
      (= (apply + l) (apply + r)))))

(defcheck solution-49c07709
  (fn [n]
    (let [digits (map (comp parse-int str) (str n))
          d (/ (count digits) 2)]
      (= (apply + (take d digits))
        (apply + (take-last d digits))))))

(defcheck solution-49ea9323
  (fn [xs]
    (let [xs (str xs) c (count xs) f (take (quot c 2) xs) s (drop (quot c 2) xs) ]
      (letfn [(val [ys] (apply + (map #(parse-int (str %)) ys)))]
        (cond
          (> c 2) (= (val f) (val (drop 1 s)))
          (= c 1) true
          :else (= (val f) (val s)))))))

(defcheck solution-4a1e6ee3
  (fn [x]
    (let [m (zipmap (str "0123456789") (range))
          d (map #(m %) (str x))
          l (/ (count d) 2)]
      (= (apply + (take l d))
        (apply + (take-last l d))))))

(defcheck solution-4a371c43
  (fn [n]
    (let [s (vec (map #(- (parse-char %) (parse-char \0)) (str n)))
          c (int (/ (count s) 2))]
      (= (apply + (subvec s 0 c))
        (apply + (subvec s (if (odd? (count s)) (inc c) c)))))))

(defcheck solution-4a9098fa
  (fn [n]
    (let [nstr (map (comp parse-int str) (str n))
          len (quot (count nstr) 2)
          left (apply + (take len nstr))
          right (apply + (take-last len nstr))]
      (= left right))))

(defcheck solution-4a9fe5a5
  #(loop [[f & r] (str %) a #{} b #{}]
     (if (seq r)
       (recur (butlast r) (conj a f) (conj b (last r)))
       (= a b))))

(defcheck solution-4ac2eac9
  (fn f [n]
    (let [se ((comp seq str) n) fh (take (/ (count se) 2) se) sh (take (/ (count se) 2) (reverse se))]
      (= (reduce #(+ (parse-int (str %2)) %1) 0 fh) (reduce #(+ (parse-int (str %2)) %1) 0 sh)))))

(defcheck solution-4b59c1c3
  (fn [n]
    (let [digits (vec (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (str n))))
          mid (/ (count digits) 2)]
      (= (reduce + (subvec digits 0 (Math/ceil mid)))
        (reduce + (subvec digits (Math/floor mid)))))))

(defcheck solution-4bb80cef
  (fn balancedNum
    [n]
    (let [lst (map #(parse-int (str %)) (str n))
          cnt (count lst)
          top (quot cnt 2)
          bot (if (odd? cnt) (inc top) top)]
      (= (apply + (take top lst)) (apply + (drop bot lst))))))

(defcheck solution-4bd8305
  (fn [x]
    (let [digits (map #(parse-int (str %)) (str x))
          n (count digits)
          left (take (/ n 2) digits)
          right (take-last (/ n 2) digits)]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-4bf89f2d
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (.toString n))
          half (quot (inc (count digits)) 2)
          ]
      (= (reduce + (take half digits))
        (reduce + (take half (reverse digits)))))))

(defcheck solution-4c005391
  (fn [n]
    (let [coll  (vec (map int (str n)))
          len   (count coll)
          lmid  (quot len 2)
          rmid  (if (even? len) lmid (inc lmid))
          left  (subvec coll 0 lmid)
          right (subvec coll rmid)]
      (= (apply + left) (apply + right)))))

(defcheck solution-4c239fcd
  (fn [num]
    (let
     [ snum   (str num)
      h-cnt  (int (/ (count snum) 2))
      left   (take h-cnt snum)
      right  (take h-cnt (reverse snum))]

      (= (sort left) (sort right)))))

(defcheck solution-4c5d1283
  (fn __ [n]
    (let [s (str n)
          pos-half (int (/ (count s) 2))
          l-part (take pos-half s)
          r-part (take pos-half (reverse s))]
      (letfn [(sum-digit [s-n]
                (apply + (map (fn [ch] (parse-int (str ch))) (seq s-n))))]
        (= (sum-digit l-part) (sum-digit r-part))))))

(defcheck solution-4ca52134
  (fn b [n]
    (let [s (str n) h (quot (count s) 2) left (subs s 0 h) right (subs (clojure.string/reverse s) 0 h)]
      (= (apply + (map #(- (int %) 48) left))
        (apply + (map #(- (int %) 48) right))))))

(defcheck solution-4cbbebe4
  (fn [i]
    (let [input (str i)
          first-half (subs input 0 (/ (count input) 2))
          second-half (subs (clojure.string/reverse input) 0 (/ (count input) 2))
          sum-half (fn [f] (reduce + (map #(parse-char %) f)))
          ]
      (= (sum-half first-half) (sum-half second-half))
      )
    ))

(defcheck solution-4ce4cdcf
  (fn [n]
    (let [s (map #(- (parse-char (char %)) (parse-char \0)) (pr-str n))
          h (int (/ (count s) 2))
          l (take h s)
          r (take-last h s)] [l r]
                             (= (reduce + l) (reduce + r)))))

(defcheck solution-4cfab4de
  (fn [n]
    (let [s (str n)
          c (count s)
          m (quot c 2)
          l (subs s 0 m)
          r (subs s (+ m (rem c 2)))
          f (fn f [i]
              (if (< i 10)
                i
                (+ (rem i 10) (f (quot i 10)))))]
      (if (= l r)
        true
        (= (f (parse-int l)) (f (parse-int r)))))))

(defcheck solution-4d752d6b
  (fn [n]
    (-> n str count (/ 2) ((juxt take take-last) (str n))
      (->> (map sort) (reduce =)))))

(defcheck solution-4d7a8703
  (fn [n]
    (let [s (str n)
          si (map #(- (parse-char %) (parse-char \0)) (seq s))
          mid (quot (count s) 2)]
      (=
        (apply + (take mid si))
        (apply + (take mid (reverse si)))))))

(defcheck solution-4db693dc
  (fn __ [x]
    (let [
          items (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (seq (str x))) )
          co (quot (count items) 2)
          le (take co items)
          ri (take co (reverse items))
          ]
      (= (reduce + le) (reduce + ri)))))

(defcheck solution-4e365879
  #(let [s ((fn digits [n]
              (if (pos? n) (cons (mod n 10) (digits (quot n 10))))) %)
         k (/ (count s) 2)
         r (reverse s)]
     (= (reduce + (take k s)) (reduce + (take k r)))))

(defcheck solution-4e63677e
  #(let [n (map int (.toString %))
         h (/ (count n) 2)]
     (= (reduce + (take h n)) (reduce + (take-last h n)))))

(defcheck solution-4eb31200
  #(let [s (str %) c (int (/ (count s) 2))
         sum (fn [x] (reduce + (map (comp parse-int str) x)))]
     (= (sum (take c s)) (sum (take-last c s)))))

(defcheck solution-4f05fc0b
  (let [number->digits (fn number->digits [num]
                         {:pre [(>= num 0)]}
                         (loop [acc ()
                                num num]
                           (if (<= num 9)
                             (conj acc num)
                             (recur (conj acc (rem num 10))
                               (quot num 10)))))]
    (fn balance-of-N? [n]
      (loop [front-acc 0
             rear-acc 0
             digits (number->digits n)]
        (if (<= (count digits) 0)
          (= front-acc rear-acc)
          (recur (+ front-acc (first digits))
            (+ rear-acc (last digits))
            (butlast (rest digits))))))))

(defcheck solution-4f28d00d
  (fn balanced? [x]
    (let [sval (str x)
          half (int (/ (count sval) 2))
          left-half (take half sval)
          right-half (take half (reverse sval))]
      (= (apply + (map int left-half))
        (apply + (map int right-half))))))

(defcheck solution-4fbcae72
  ; kind of cheat, but the testcases are lame
  (fn [e] (if (< e 10)
            true
            (let [s (str e)
                  c (int (/ (count s) 2))
                  a (take c s)
                  b (take-last c s)] (= (sort a) (sort b))))))

(defcheck solution-4fda4686
  (fn balanced? [n]
    (let [to-digits (fn [n] (map #(- (parse-char %) (parse-char \0)) (seq (str n))))
          digits (to-digits n)
          num-of-digits (count digits)
          split-pos (int (/ num-of-digits 2.0))
          splitted-digits (split-at split-pos digits)
          first-half (first splitted-digits)
          second-half (drop (mod num-of-digits 2) (second splitted-digits))
          sum1 (reduce + first-half)
          sum2 (reduce + second-half)]
      (= sum1 sum2))))

(defcheck solution-4ff0aec0
  (fn __ [n]
    (let [s (->> n
              str
              seq
              (map str)
              (map parse-int)
              )
          rs (reverse s)
          len (int (/ (count s) 2))]
      (= (reduce + (take len rs))
        (reduce + (take len s)))
      )))

(defcheck solution-50024ca6
  #(loop [[h & t] (str %)
          a 0]
     (if t
       (recur (butlast t) (- (+ a (int h)) (int (last t))))
       (= a 0))))

(defcheck solution-5003518e
  ;maximental - nice, ummel - interesting
  (fn [x]
    (let [d (map #(- (int %) 48) (seq (str x)))
          l (int (/ (count d) 2))
          ]
      (=
        (apply + (take l d))
        (apply + (take l (reverse d)))))))

(defcheck solution-5012b5c7
  (fn [x]
    (let [digits (str x)
          halflen (int (/ (count digits) 2))]

      (= (apply + (map #(parse-int (str %)) (subs digits halflen)))
        (apply + (map #(parse-int (str %)) (subs (clojure.string/reverse digits) halflen)))))))

(defcheck solution-501ba95d
  (fn [x]
    (let [digits (map (zipmap "0123456789" (range)) (str x))
          n2 (/ (count digits) 2)]
      (= (apply + (take n2 digits))
        (apply + (take-last n2 digits))))))

(defcheck solution-5072f5c5
  (fn [n]
    (let [sq (map #(- (parse-char %) (parse-char \0)) (seq (str n)))
          c (count sq)
          s (if (even? c) (/ c 2) (/ (dec c) 2))
          sums (map #(reduce + %) (partition s (if (even? c) s (inc s)) sq))]
      (if (empty? (rest sums)) true
                               (= (first sums) (second sums))))))

(defcheck solution-51dd74af
  (fn bal [n]
    (let [digits (map #(- (int %) 48) (str n))
          length (count digits)]
      (let [r (drop (/ length 2) digits)
            l (if (even? length)
                (take (/ length 2) digits)
                (take (/ (dec length) 2) digits))]
        (= (reduce + l) (reduce + r))))))

(defcheck solution-52294119
  (fn [n]
    (let [half #(take (quot (count %) 2) %)
          atoi #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))
          half-sum #(->> %
                      half
                      (map atoi)
                      (reduce +))
          balanced? #(= (half-sum %)
                       (half-sum (reverse %)))]
      (balanced? (str n)))))

(defcheck solution-52879a63
  (fn [n]
    (let [digits (mapv #(- (parse-char %) (parse-char \0)) (str n))
          half (quot (count digits) 2)
          left (take half digits)
          right (take-last half digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-52adf90c
  (fn [n]
    (let [m (->> n str (map identity) (map #(character-digit % 10)))
          len (count m)
          halflen (quot len 2)
          right-sum (->> m (take halflen) (apply +) )
          left-sum (->> m (drop (if (odd? len) (inc halflen) halflen)) (apply +) )
          ]
      (if (= right-sum left-sum) true false))))

(defcheck solution-52dd362b
  (fn balanced?[n]
    (let [str-n    (str n)
          no-chars (/ (count str-n) 2)]
      (= (reduce + (map #(character-digit % 10) (take no-chars str-n)))
        (reduce + (map #(character-digit % 10) (take no-chars (clojure.string/reverse str-n))))))))

(defcheck solution-52debfeb
  (fn [n]
    (let [digits (->> n str (map (comp parse-int str)))
          half (quot (count digits) 2)
          leftsum (apply + (take half digits))
          rightsum (apply + (if (odd? (count digits)) (drop (inc half) digits) (drop half digits)))]
      (= leftsum rightsum)
      )
    ))

(defcheck solution-530ca8eb
  (fn [num] (let [a (map #(- (int %) 48) (str num)) c (quot (count a) 2) s #(reduce + %) x (s (take c a)) y (s (take-last c a))] (= x y))))

(defcheck solution-531bf821
  (letfn [(digits [x]
            (lazy-seq
              (when (pos? x)
                (cons (mod x 10) (digits (quot x 10))))))
          (split [x]
            (let [d (digits x)
                  p (quot (count d) 2)]
              (if (even? (count d))
                [(take p d) (drop p d)]
                [(take p d) (drop (inc p) d)])))]
    (fn [x]
      (let [[f b] (split x)]
        (= (apply + f) (apply + b))))))

(defcheck solution-534e6b5b
  (fn [x] (let [s (str x) h (quot (count s) 2) i #(int %) c #(reduce + (map i (take h %)))] (= (c s) (c (reverse s))))))

(defcheck solution-537b4e24
  (fn balanced? [n]
    (let [digits (map #(character-digit % 10) (str n))
          half (quot (count digits) 2)]
      (= (apply + (take half digits))
        (apply + (take-last half digits))))))

(defcheck solution-5397ef99
  (letfn [(digits [x]
            (->> (str x)
              (map #(- (parse-char %) (parse-char \0)))))
          (split-to-halfs [coll]
            (if (= 1 (count coll))
              (list coll coll)
              (let [len (count coll)
                    half-len (int (+ (/ len 2) 0.5))
                    rem-len (rem len 2)]
                (partition half-len (- half-len rem-len) coll))))]
    (fn balanced? [n]
      (->> n
        digits
        split-to-halfs
        (map #(apply + %))
        (apply =)))))

(defcheck solution-53999b6
  (fn balance [n]
    (let [ds (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          c (int (/ (count ds) 2))]
      (= (apply + (take c ds))
        (apply + (take c (reverse ds)))))))

(defcheck solution-53a700d4
  (fn [n]
    (let [digits (->> n (str) (map str) (map parse-int))
          n-digits (/ (count digits) 2)
          left-digits (take n-digits digits)
          right-digits (take-last n-digits digits)]
      (= (reduce + left-digits) (reduce + right-digits)))))

(defcheck solution-545c0e7a
  (fn p-115 [n]
    (loop [digits (map #(- (int %) 48) (str n))
           sum 0]
      (if (> 1 (count digits))
        (zero? sum)
        (recur
          (rest (butlast digits))
          (+ sum (- (first digits) (last digits))))))))

(defcheck solution-5466137
  (fn [n]
    (letfn [(sum-it [nums]
              (reduce #(+ %1 (- (parse-char %2) (parse-char \0))) 0 nums))]
      (let [s (str n)
            hl (/ (count s) 2)
            h1 (take hl s)
            h2 (take hl (reverse s))]
        (= (sum-it h1) (sum-it h2))))))

(defcheck solution-54766e0f
  (fn [x]
    (let [n (str x)
          c (quot (count n) 2)
          f (fn [a]
              (apply +
                (map
                  #(- (parse-char %) (parse-char \0))
                  a)))]
      (= (f (take c n))
        (f (take-last c n))))))

(defcheck solution-547f5851
  (fn [x] (let [y (str x) f #(apply + (map parse-int ( map str %))) a (quot (count y) 2)] (= (f (take a y)) (f (drop (- (count y) a) y))))))

(defcheck solution-548d518d
  (fn[n](letfn[(f[y](let[n(/(count y)2)](=(apply +(take n y))(apply +(take-last n y)))))] (f(map #(parse-int(str %))(str n))))))

(defcheck solution-54f0df33
  (fn [x]
    (let [s (str x)
          f (fn [y]
              (apply +
                (take
                  (quot (count s) 2)
                  (map #(int %) y))))]
      (= (f s) (f (reverse s))))))

(defcheck solution-55977163
  (fn balanced? [n]
    (letfn [(digits [x]
              (if (< x 10)
                (vector x)
                (conj (digits (/ (- x (mod x 10)) 10)) (mod x 10))))]
      (== (apply + (subvec (digits n) 0 (Math/floor (/ (count (digits n)) 2))))
        (apply + (subvec (digits n) (Math/ceil (/ (count (digits n)) 2))))))))

(defcheck solution-55c6298c
  (fn [s]
    (let [d (map #(- (int %) 48) (str s))
          f #(apply + (take (/ (count %) 2) %))]
      (= (f d) (f (reverse d))))))

(defcheck solution-5604039e
  (fn [n]
    (let [cds (map #(parse-int (str %)) (str n))
          c (quot (count cds) 2)
          l (take c cds)
          r (take-last c cds)]
      (= (reduce + l) (reduce + r)))))

(defcheck solution-56443fb5
  (fn [n]
    (let [str_n (str n)
          n (count str_n)
          fhalf (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (.substring str_n 0 (quot n 2))))
          shalf (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (.substring str_n (+ (quot n 2) (mod n 2)) n)))]
      (== (apply + fhalf) (apply + shalf)))))

(defcheck solution-57091709
  #(let [c (map (comp parse-int str) (str %))
         c1 (take (int (/ (count c) 2)) c)
         c2 (take-last (int (/ (count c) 2)) c)]
     (= (apply + c1) (apply + c2))))

(defcheck solution-570ee35f
  (fn[x]
    (let [xs (map (comp parse-int str) (str x))
          h (/ (count xs) 2)]
      (= (reduce + (take (if (odd? (count xs)) (dec h) h) xs))
        (reduce + (drop h xs))))))

(defcheck solution-573094c3
  (fn foo [n]
    (apply =
      (map #(->> % (map int) (reduce +))
        (let [s (str n) w (count s) h (/ w 2)]
          (partition (int (Math/ceil h)) (max 1 (int h)) s))))))

(defcheck solution-5741879a
  (fn [n]
    (let [dgs (str n)
          d (int (/ (count dgs) 2))
          f (fn [s] (apply + (map #(parse-int (str %)) s)))]
      (= (f (take d dgs))
        (f (take-last d dgs))))))

(defcheck solution-57bed613
  (fn [n] (let [msg (map (comp #(parse-int %) str) (seq (str n)))
                k (count msg)
                [n1 n3] (split-at (int (/ k 2)) msg)
                n2 (if (odd? k) (rest n3) n3)
                ]
            (= (apply + n1) (apply + n2)))))

(defcheck solution-58313c96
  (fn balanced? [n]
    (let [digits (map #(parse-char %) (str n))
          lefthalf (take (/ (count digits) 2) digits)
          righthalf (take-last (/ (count digits) 2) digits)]
      (= (apply + lefthalf) (apply + righthalf)))))

(defcheck solution-587d7f08
  (fn balanced? [x]
    (let [digits (fn [x] (map #(- % 48) (map int (.toString x))))
          countd (fn [x] (count (.toString x)))
          half (fn [x] (int (/ (countd x) 2)))]
      (= (reduce + (take (half x) (digits x)))
        (reduce + (take (half x) (reverse (digits x))))))))

(defcheck solution-58accf83
  (fn balanced-number? [n]
    (let [coll (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) ((comp seq str) n))
          chunk-size (quot (count coll) 2)]
      (= (reduce + (take chunk-size coll))
        (reduce + (take-last chunk-size coll))))))

(defcheck solution-58d44cd0
  (fn [n]
    (let [to-n-seq (fn [agg n]
                     (let [m (mod n 10)
                           nm (- n m)
                           n-agg (cons m agg)]
                       (if (> nm 0)
                         (recur n-agg (/ nm 10))
                         n-agg)))
          n-seq (to-n-seq () n)
          cn (count n-seq)
          n-take (int (/ cn 2))
          n-drop (+ n-take (mod cn 2))
          left (take n-take n-seq)
          right (drop n-drop n-seq)]
      (= (apply + left) (apply + right)))))

(defcheck solution-58f0cedb
  #(loop [[c & s] (map (fn [x] (- (int x) 48)) (str %)) l 0 r 0]
     (if s
       (recur (drop-last s) (+ l c) (+ r (last s)))
       (= l r))))

(defcheck solution-58f47758
  (fn [n]
    (let [d (map #(- (int %) 48) (str n)) h (/ (count d) 2)]
      (= (apply + (take h d))
        (apply + (take h (reverse d)))))))

(defcheck solution-59010913
  #(let [xs (map int (str %))
         n (/ (count xs) 2)]
     (= (apply + (take n xs))
       (apply + (take-last n xs)))))

(defcheck solution-59085a65
  (fn [x]
    (let [s (loop [x x
                   r []]
              (if (= x 0)
                r
                (recur (quot x 10)
                  (conj r (rem x 10)))))
          l (quot (count s) 2)]
      (= (apply + (take l s))
        (apply + (take-last l s))))))

(defcheck solution-59e8769c
  (fn balanced? [n]
    (let [digits (map #(parse-char %) (str n))
          to-take (int (/ (count digits) 2))]
      (= (reduce + (take to-take digits))
        (reduce + (take-last to-take digits))))))

(defcheck solution-5a1b63a1
  (fn [n]
    (let [s (str n)
          i (int (/ (count s) 2))
          l (take i s)
          r (take i (reverse s))
          len2 (fn [s] (apply + (map #(* % %) (map #(- (parse-char %) (parse-char \0)) s))))
          ] (= (len2 l) (len2 r)))))

(defcheck solution-5a479bae
  #(let[digits (seq (str %))
        lowerHalf (take (+ (quot (count digits) 2) (rem (count digits) 2)) digits)
        upperHalf (drop (quot (count digits) 2) digits)
        addDigits (fn [x y] (+ x (- (parse-char y) (parse-char \0))))]
     (= (reduce addDigits 0 lowerHalf) (reduce addDigits 0 upperHalf))))

(defcheck solution-5a5f0264
  (fn balanced? [n]
    (let [dig (loop [n n digits []]
                (if (< n 10)
                  (cons (int n) digits)
                  (recur
                    (quot n 10)
                    (cons (int (rem n 10)) digits))))
          n (int (/ (count dig) 2))]
      (= (apply + (take n dig))
        (apply + (take n (reverse dig)))))))

(defcheck solution-5abe4c90
  (fn [n]
    (let [sn (map #(mod (inc (- (parse-char %) (parse-char \1))) 10) (str n))
          cnt (count sn)
          cn (-> sn count (/ 2) int)]
      (= (apply + (take cn sn))
        (apply + (drop (- cnt cn) sn))))))

(defcheck solution-5b631385
  (fn [n]
    (let [x (seq (str n))
          y (int (/ (count x) 2))]
      (= (apply + (map (comp parse-int str) (take y (reverse x))))
        (apply + (map (comp parse-int str) (take y x)))))))

(defcheck solution-5b8f6bf
  (fn [n]
    (let [s (str n)
          k (int (/ (count s) 2))]
      (apply = (map #(apply + (map (fn [c] (parse-char c)) (% k s))) [take	take-last])))))

(defcheck solution-5b9f7694
  (fn [n]
    (let [s (str n)
          ds #(reduce + (map int %))
          half (/ (count s) 2)]
      (= (ds (subs s 0 half)) (ds (subs s (+ half 0.5)))))))

(defcheck solution-5ba1e627
  (fn x [n]
    (let [s (str n)
          middle (/ (count s) 2)
          prefix (subs s 0 (Math/ceil middle))
          suffix (subs s (Math/floor middle))
          sum-chars (fn [s] (reduce #(+ %1 (int %2)) 0 s))]
      (= (sum-chars prefix) (sum-chars suffix)))))

(defcheck solution-5cbb6c4d
  (fn [n] (let [s (->> (iterate #(quot % 10) n)
                    (take-while #(not= 0 %))
                    (map #(mod % 10)))
                m (quot (count s) 2)]
            (->> [(take m s) (take-last m s)]
              (map #(apply + %))
              (apply =)))))

(defcheck solution-5d21ff99
  (fn balanced [n] (let [sn (str n) f (fn [ns] (apply + (map (comp parse-int str) (take (int (/ (count ns) 2)) ns))))] (= (f sn) (f (reverse sn)) ))))

(defcheck solution-5d7ba933
  (fn balanced?
    [x]
    (loop [left []
           right []
           remain (map (fn [x] (- (parse-char x) (parse-char \0))) (into [] (str x)))]
      (if (< (count remain) 2)
        (= (reduce + left) (reduce + right ))
        (recur (conj left (first remain))
          (conj right (last remain))
          (reverse (rest (reverse (rest remain)))))))))

(defcheck solution-5da7de5c
  (fn [n] (let [s (->> (str n) (map #(parse-int (str %))))
                h (/ (count s) 2)
                l (apply + (take h s))
                r (apply + (take h (reverse s)))]
            (= l r))))

(defcheck solution-5df3a9e4
  (fn [n]
    (letfn [(digits [n] (if (< n 10) (list n) (cons (mod n 10) (digits (quot n 10)))))]
      (let [n-list (digits n) n-len (count n-list) half (quot n-len 2) front (- n-len half)]
        (= (apply + (take half n-list)) (apply + (drop front n-list)))))))

(defcheck solution-5e0d9040
  (fn [x] (letfn [(digits [x] (map #(- % 48) (map int (seq (str x)))))
                  (half-length [s] (int (/ (count s) 2)))
                  (drop-middle? [s] (= 1 (mod (count s) 2)))]
            (let [dd (digits x), h (half-length dd)]
              (= (apply + (take h dd))
                (apply + (if (drop-middle? dd) (drop (+ 1 h) dd) (drop h dd))))))))

(defcheck solution-5e8adb31
  (fn balanced? [n]
    (letfn
     [(digits [n] (if (= n 0) [] (conj (digits (quot n 10)) (rem n 10))))
      (halve [s] (let [c (/ (count s) 2)] (list (take c s) (take-last c s))))]
      (#(= (first %) (second %)) (map #(apply + %) (halve (digits n)))))))

(defcheck solution-5ec9588a
  (fn [n]
    (letfn [(sum [s]
              (reduce + (map #(parse-char %) s)))]
      (let [   s (.toString n)
            half (quot (count s) 2)]
        (=
          (sum (take half s))
          (sum (take-last half s)))))))

(defcheck solution-5ed2d11b
  (fn balance [num]
    (let [nc (fn [n]
               (loop [i 1]
                 (if (= 0 (int (quot n (Math/pow 10 i))))
                   i
                   (recur (inc i)))))
          nb (nc num)
          midnum (int (quot nb 2))
          numcoll (fn [n]
                    (loop [i 0 result '()]
                      (if (= i (nc n))
                        result
                        (recur (inc i) (cons (int (rem (quot n (Math/pow 10 i)) 10)) result)))))
          lefthalf-sum  (apply + (take      midnum (numcoll num)))
          righthalf-sum (apply + (take-last midnum (numcoll num)))]
      (if (= lefthalf-sum righthalf-sum) true false))))

(defcheck solution-5f1587e6
  #(let [coll (map parse-int (re-seq #"\d" (str %)))
         n (/ (count coll) 2)]
     (= (apply + (take n coll)) (apply + (take n (reverse coll))))))

(defcheck solution-5fa214e
  (fn the-balance-of [n]
    (let [string-of-n (str n)
          reverse-string (reverse string-of-n)
          halve (int (/ (count string-of-n) 2))]
      (= (reduce + (map #(character-digit % 10) (first (partition halve string-of-n))))
        (reduce + (map #(character-digit % 10) (first (partition halve reverse-string)))))
      )))

(defcheck solution-5fa59a1a
  #((fn B [c]
      (let [l (count c)
            h (/ l 2)
            s subs]
        (if (odd? l)
          (B (str (s c 0 h) (s c (+ 1 h))))
          (apply = (map sort (split-at h c))))))
    (str %)))

(defcheck solution-5fcb274a
  (fn is-balanced[x]
    (let [y (str x)
          length (count y)
          half-length (quot length 2)
          first-half (take half-length y)
          second-half (take-last half-length y)
          first-val (apply + (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) first-half))
          second-val (apply + (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) second-half))]
      (= first-val second-val))))

(defcheck solution-5fd25c0c
  (fn balanced? [n]
    (let [digits (map #(character-digit % 10) (str n))
          len (quot (count digits) 2)]
      (= (apply + (take len digits)) (apply + (take len (reverse digits)))))))

(defcheck solution-6018147a
  (fn is-balanced [a-num]
    (let [a-str (.toString a-num)
          size (count a-str)
          half-size (int (/ size 2))
          first-half (take half-size a-str)
          last-half (take-last half-size a-str) ]
      (= (apply + (map int first-half)) (apply + (map int last-half)))
      )
    ))

(defcheck solution-60337699
  (fn balanced? [n]
    (let [st         (str n)
          len        (count st)
          chunk      (quot len 2)
          chunk      (if (= chunk 0) 1 chunk)
          left       (subs st 0 chunk)
          right      (subs st (- len chunk))
          digits-sum (fn [s] (reduce + (map #(- (parse-char %) (parse-char \0)) s)))]
      (= (digits-sum left) (digits-sum right)))))

(defcheck solution-604b0397
  (fn balanced? [n]
    (let [split-num (fn [n]
                      (let [s (str n)
                            c (count s)]
                        (if (even? c)
                          (let [h (/ c 2)]
                            (conj [] (subs s 0 h) (subs s h)))
                          (let [h (quot c 2)]
                            (conj [] (subs s 0 h) (subs s (inc h))))
                          )))
          calc-sum-digs (fn [s]
                          (let [chs (seq s)
                                nums (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) chs)]
                            (reduce + nums)))
          halfs (split-num n)]
      (= (-> (first halfs) calc-sum-digs) (-> (last halfs) calc-sum-digs))
      )))

(defcheck solution-607d5e5
  (fn [n]
    (let [s (map int (str n)) cs (count s)]
      (= (apply + (take (/ cs 2) s))
        (apply + (drop (/ (dec cs) 2) s))))))

(defcheck solution-607e49ea
  (fn balanced? [x]
    (letfn [(to-digits [x]
              (loop [y x
                     ds []]
                (if (< y 10) (conj ds y)
                             (let [digit (mod y 10)
                                   restNum (/ (- y digit) 10)]
                               (recur restNum (conj ds digit))))))]
      (let [digits-of-x (to-digits x)
            size (/ (count digits-of-x) 2)
            sumofLeft (reduce + (take size digits-of-x))
            sumofRight (reduce + (take size (reverse digits-of-x)))]
        (= sumofLeft sumofRight)))))

(defcheck solution-60ecc0dd
  (fn [n]
    (let [ds (->> n
               str
               (map (comp parse-int str)))
          m (count ds)
          p (int (/ m 2))
          [l r] (split-at p ds)]
      (= (reduce + l) (reduce + (if (even? m) r (rest r)))))))

(defcheck solution-60f5c0d6
  #(let [[d c]
         (loop [s [] n % c 0]
           (if (= n 0) [s c]
                       (recur (conj s (mod n 10)) (quot n 10)
                         (+ c 1))))
         [a b] (split-at (/ c 2) d)]
     (= (reduce + (if (even? c) a (butlast a))) (reduce + b))))

(defcheck solution-610b80cd
  #(or (< % 10)
       (let [s (str %)
             half (/ (count s) 2)]
         (->> (partition (int (Math/ceil half)) (int (Math/floor half)) s)
           (map (partial map int))
           (map (partial reduce +))
           (apply =)))))

(defcheck solution-615b1bf3
  (fn [n]
    (let [s (str n)
          cnt (count s)
          a (map int (take (Math/floor (/ cnt 2)) s))
          b (map int (drop (/ cnt 2) s))]
      (= (apply + a) (apply + b)))))

(defcheck solution-6192df83
  (fn balanced? [n]
    (let [sn (str n)
          len (count sn)
          mid (quot len 2)
          odd (if (odd? len) 1 0)
          lc  (subs sn 0 mid)
          rc  (subs sn (+ mid odd))
          sum #(reduce + (map (fn [c] (- (int c) 40)) (seq %))) ]
      (if (zero? mid)
        true
        (=  (sum lc) (sum rc))))))

(defcheck solution-61d73f14
  (fn f [num]
    (let [d (map #(- (parse-char %) (parse-char \0)) (str num))
          c (count d)
          m (quot c 2)]
      (if (even? c)
        (= (apply + (take m d)) (apply + (drop m d)))
        (= (apply + (take m d)) (apply + (drop (inc m) d)))))))

(defcheck solution-620903a
  (fn [n]
    (letfn [(i->l [n] (map #(- (int %) 48) (seq (str n))))]
      (let [l (i->l n) s (int (/ (count l) 2))]
        (= (apply + (take s l)) (apply + (take-last s l)))
        ))))

(defcheck solution-6254540a
  (fn [x]
    (let [halves
          (let [sq (seq (str x))
                s  (int (/ (count sq) 2))]
            (map (fn [half] (map #(- (parse-char %) (parse-char \0)) half))
              [(take s sq) (drop (- (count sq) s) sq)]))]
      (apply = (map #(reduce + %) halves)))))

(defcheck solution-62f6efb5
  (fn [x]
    (let [digits (map parse-int (map str (seq (str x))))
          len (count digits)
          cutpoint (quot len 2)
          front (map #(nth digits %) (range 0 (if (odd? len) (inc cutpoint) cutpoint)))
          back (map #(nth digits %) (range cutpoint len))]
      #_(println digits len cutpoint front back)
      (= (reduce + front) (reduce + back)))))

(defcheck solution-62f8bc13
  (fn [x] (let [s (str x)
                l (/ (count (str x)) 2)
                f (partial reduce #(+ % (int %2)) 0)]
            (=  (f (subs s 0 l))
              (f (subs (clojure.string/reverse s) 0 l))))))

(defcheck solution-630a8086
  (fn [n]
    (letfn [(digit-sum [s]
              (reduce + (map #(character-digit % 10) s))),
            (left-half [s]
              (subs s 0 (quot (count s) 2)))]
      (= (-> n str left-half digit-sum) (-> n str clojure.string/reverse left-half digit-sum)))))

(defcheck solution-63610cb1
  #(let [digits ((fn ds [n] (if-not (zero? n) (cons (rem n 10) (ds (quot n 10))))) %)
         i (-> (count digits) (/ 2))]
     (->> (split-at i digits) (map (fn [c] (reduce * (take (Math/floor i) c)))) (apply =))))

(defcheck solution-63633064
  (fn [x]
    (let [s ((fn f [n] (if (= n 0)
                         []
                         (conj (f (quot n 10)) (mod n 10)))) x)
          n (/ (count s) 2)]
      (= (apply + (take n s)) (apply + (take-last n s))))))

(defcheck solution-63654aa6
  (fn [o]
    (let [s (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} (str o))
          c (/ (count s) 2)]
      (=
        (apply + (take c s))
        (apply + (take c (reverse s)))))))

(defcheck solution-63d85608
  (fn [x]
    (let [xs (str x)
          n  (count xs)]
      (= (reduce + (map int (take (Math/floor (/ n 2)) xs)))
        (reduce + (map int (drop (Math/ceil  (/ n 2)) xs)))))))

(defcheck solution-63e1930a
  (fn [x]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str x))
          size (int (/ (count digits) 2))
          s (fn [f] (reduce + (f size digits)))]
      (= (s take) (s take-last)))))

(defcheck solution-64b5d0b9
  (fn b
    [n]
    (let [sn (str n)
          cn (int (/ (count sn) 2))
          m  (subs sn cn (inc cn))
          p  (partition-by #(= m (str %)) sn)]
      (= (set (first p)) (set (last p))))))

(defcheck solution-64bb22a0
  (fn [x]
    (let [
          bisect (fn [n]
                   (let [length (count n)
                         half (quot length 2)]
                     [(.substring n 0 half) (.substring n (- length half))]))
          digit-sum (fn [n]
                      (->> n (map str) (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %)) (reduce + 0)))
          ]
      (->> x str bisect (map digit-sum) (reduce ==)))))

(defcheck solution-64bd54ad
  (fn [n]
    (let [s (str n)
          half (/ (count s) 2)
          strsum #(reduce + (map (comp parse-int str) %))]
      (= (strsum (take (Math/floor half) s))
        (strsum (drop (Math/ceil half) s))))))

(defcheck solution-65aae6d6
  (fn [x]
    (->> x
      str
      ((juxt #(take (/ (count (str x)) 2) %)
         #(take-last (/ (count (str x)) 2) %)))
      (map #(map (fn [n] (character-digit n 10)) %))
      (map (partial apply +))
      (#(if (= (last %)
              (first %))
          true
          false)))))

(defcheck solution-660b38c7
  (fn [a]
    (letfn [(digits [b] (vec (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))  (seq (str b)))))
            (check-balanced [c]
              (let [num (int (Math/floor (/ (count c) 2)))]
                (if (odd? (count c))
                  (= (reduce + (subvec c 0 num)) (reduce + (subvec c (inc num) (count c))))
                  (=(reduce + (subvec c 0 num)) (reduce + (subvec c num (count c)))))))]
      (check-balanced (digits a)))))

(defcheck solution-669f3e91
  (fn [n]
    (let [ns (str n)
          size (quot (count ns) 2)
          sd (fn [s] (reduce + (map #(character-digit % 10) s)))]
      (= (sd (take size ns)) (sd (take size (reverse ns)))))))

(defcheck solution-66a23358
  #(let [s (str %1)
         len (count s)
         len2 (quot len 2)
         first-half (apply + (map int (take len2 s)))
         second-half (apply + (map int (drop (- len len2) s)))]
     (= first-half second-half)))

(defcheck solution-66cfa8c0
  (fn b [n]
    (let [v (map #(parse-int (str %)) (str n))
          m (quot (count v) 2)]
      (= (apply + (take m v)) (apply + (take-last m v))))))

(defcheck solution-67283c69
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          len (count digits)
          half (quot len 2)]
      (= (apply + (take half digits))
        (apply + (drop (- len half) digits))))))

(defcheck solution-676ae477
  (fn bn [x]
    (let [s (str x)
          c (count s)
          split (split-at (/ c 2) s)
          left (if (odd? c) (butlast (first split)) (first split))
          right (second split)
          sumN (fn [z] (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) z)))]
      (= (sumN right) (sumN left))
      )
    ))

(defcheck solution-67a6f2ce
  (fn balance-of-n [l]
    (letfn [(int-to-list [n]
              (loop [i n s '()]
                (if (< i 10)
                  (conj s i)
                  (recur (quot i 10) (conj s (rem i 10))))))
            (equal-halves? [l2]
              (let [half (int (/ (count l2) 2))
                    drop-half (- (count l2) half)]
                (= (reduce + (take half l2))
                  (reduce + (drop drop-half l2)))))]
      (equal-halves? (int-to-list l)))))

(defcheck solution-67b9a996
  (fn [num]
    (let [snum  (str num)
          mid   (int (/ (count snum) 2))
          left  (reduce + (map int (take mid snum)))
          right (reduce + (map int (take-last mid snum)))]
      (= left right)
      )))

(defcheck solution-67d0672
  (fn halves [n]
    (let [sn (str n)
          ln (int (/ (count sn) 2))
          a (map int (take ln sn))
          b (map int (take-last ln sn))]
      (= (apply + a) (apply + b)))))

(defcheck solution-67eed0b6
  (fn [v] (let [c (map #(parse-int (str %)) (str v)) n (count c)
                p (long (/ n 2)) p1 (if (not= 0 (mod n 2)) (+ p 1) p)]
            (or (> 10 v)
                (->> (range p) (filter #(= (nth c %) (nth c (+ p1 %)))) count (= p))
                (->> (range p) (filter #(= (nth c %) (nth c (- n % 1)))) count (= p))
                )
            )))

(defcheck solution-681021bf
  (fn [num]  (let [coll (map #(int %) (str num))        len (Math/floor (/ (count coll) 2))]    (= (apply + (take len coll))       (apply + (take-last len coll))))))

(defcheck solution-6843f49c
  (fn [n]
    (let [ns (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          s (/ (count ns) 2)
          ha ((juxt #(take s %) #(take s (reverse %))) ns)]
      (apply =
        (map #(reduce + %) ha)))))

(defcheck solution-68756fde
  (fn [n]
    (let [d (->> n
              str
              (map int))
          c (count d)
          h (int (/ c 2))
          f (take h d)
          s (take-last h d)]
      (= (apply + f)(apply + s)))))

(defcheck solution-68a8974d
  (fn [n]
    (let [ns  (str n)
          len (count ns)
          sum #(reduce + (for [c %] (character-digit c 10)))]
      (== (sum (.substring ns 0 (/ (inc len) 2)))
        (sum (.substring ns (/ len 2) len))))))

(defcheck solution-68d8bbda
  (fn [x] (let [digits ((fn f [n]
                          (if (< n 10) [n]
                                       (cons (mod n 10) (lazy-seq (f (long (/ n 10))))))) x)
                add-half (fn [ns] (apply + (take (/ (count ns) 2) ns)))]
            (= (add-half digits) (add-half (reverse digits))))))

(defcheck solution-69976ab6
  (fn bal [o] (= 0 (loop [l (loop [n o d '()] (if (= n 0) d (recur (quot n 10) (cons (mod n 10) d)))) s 0]
                     (if (< (count l) 2) s (recur (rest (butlast l)) (+ (first l) (- s (last l)))))))))

(defcheck solution-699be116
  (fn balanced? [num]
    (let [digits (map #(parse-char %) (str num))
          half-length (quot (count digits) 2)
          first-half (take half-length digits)
          last-half (drop (if (even? (count digits)) half-length (inc half-length)) digits)]
      (= (reduce + first-half) (reduce + last-half))
      )))

(defcheck solution-6a315084
  (fn [n]
    (let [s (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          mid (int (/ (count s) 2))
          left (take (- (count s) mid) s)
          right (drop mid s)]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-6a5a8f04
  (fn balanced? [number]
    (let [numbers (map #(character-digit % 10) (str number))
          half (int (Math/ceil (* 0.5 (count numbers))))]
      (= (apply + (take half numbers)) (apply + (take-last half numbers)))
      )))

(defcheck solution-6aa56f8f
  #(let [d (map int (str %))
         h (quot (count d) 2)]
     (= (apply + (take h d))
       (apply + (take h (reverse d))))))

(defcheck solution-6b0033cb
  #(let [d (map (zipmap "0123456789" (range)) (str %))
         c (count d)
         m (quot c 2) ]
     (cond
       (= c 1) true
       (even? c) (let [[a b] (split-at m d)]
                   (= (apply + a) (apply + b)) )
       (odd? c) (let [[a b] (split-at m d)]
                  (= (apply + a) (apply + (rest b))) ) ) ))

(defcheck solution-6b1919c2
  #_(fn balance-number? [n]
      (let [n-str (str n)
            len (count n-str)
            half-len (quot len 2)
            digit-strs (rest (clojure.string/split n-str #""))
            halves [(take half-len digit-strs), (take-last half-len digit-strs)]
            sum-digit-strs #(apply + (map parse-int %))]
        (apply = (map sum-digit-strs halves))))

  (fn balance-number? [n]
    (let [char->int #(- (parse-char %) (parse-char \0))
          digits (map char->int (str n))
          half-len (quot (count digits) 2)
          halves (map #(% half-len digits) [take, take-last])
          sum-digit-strs #(apply + %)]
      (apply = (map sum-digit-strs halves)))))

(defcheck solution-6b269241
  (fn [n]
    (let [s (str n)
          ss (count s)
          hs (quot ss 2)
          r #(apply + (map (comp parse-int str) %))]
      (=
        (r (take hs s))
        (r (take-last hs s))))))

(defcheck solution-6b52ada1
  (fn [n]
    (let [ds (map #(character-digit % 10) (str n))
          cnt (quot (inc (count ds)) 2)]
      (= (reduce + (take cnt ds))
        (reduce + (take cnt (reverse ds)))))))

(defcheck solution-6bf58f59
  (fn [n]
    (let [
          digits (loop [cur n acc []] (if (= cur 0) acc (recur (quot cur 10) (conj acc (rem cur 10)))))
          half-len (quot (count digits) 2)
          first-half (take half-len digits)
          second-half (take half-len (reverse digits))
          ]
      (= (apply + first-half) (apply + second-half)))))

(defcheck solution-6c5999f2
  (fn [n]
    (let [x (map int ((comp seq str) n))]
      (apply = (map #(reduce + (take (int (/ (count x) 2)) %) ) [ x (reverse x)])
        ))))

(defcheck solution-6cca4ded
  (letfn [
          (digits [x]
            (loop [y x, result '()]
              (if (zero? y)
                (apply vector result)
                (recur (quot y 10) (conj result (rem y 10))))))

          (split [A]
            (let [l (count A)
                  a (quot l 2)
                  b (if (odd? l) (inc a) a)]
              [(subvec A 0 a) (subvec A b l)]))

          (sum [A] (reduce + A))]

    (comp (partial apply =)
          (partial map sum)
          split digits)))

(defcheck solution-6d196d27
  (fn [n] (let [i (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (re-seq #"[\d]" (str n)))] ((fn [[a b]] (let [len (min (count a) (count b))] (= (reduce + (take len a)) (reduce + (take len b))))) (split-at (/ (count i) 2) i)))))

(defcheck solution-6d7e3ae2
  (fn balanced-number? [num]
    (let [digits (map #(parse-int (str %)) (str num))
          half-length (int (/ (count digits) 2))
          left-digits (take half-length digits)
          right-digits (take-last half-length digits)]
      (= (reduce + left-digits) (reduce + right-digits)))))

(defcheck solution-6dd5eafc
  (fn balanced?
    [x]
    (let [digits (map #(- (parse-char %) (parse-char \0) ) (str x))
          num-digits (count digits)
          splits (split-at (quot num-digits 2) digits)
          split1 (if (= 0 (rem num-digits 2)) (splits 1) (rest (splits 1)))]
      (= (apply + (splits 0)) (apply + split1)))))

(defcheck solution-6e272885
  (fn [n]
    (let [s (->> n str (map str) (map parse-int))
          f #(reduce + (take (quot (count s) 2) %))]
      (= (f s) (f (reverse s))))))

(defcheck solution-6f2f1720
  (fn is-balanced? [n]
    (let [get-digits (fn [x] (map #(- (parse-char %) (parse-char \0)) (seq (str x))))
          digits (get-digits n)
          half (quot (count digits) 2)
          fh (take half digits)
          lh (take-last half digits)]
      (= (apply + fh) (apply + lh)))))

(defcheck solution-70382007
  (fn [n] (let [x (str n) i (quot (count x) 2) r (map (partial apply +) (map (partial map int) [(take i x) (drop (if (odd? (count x)) (inc i) i) x)]))] (= (first r) (last r)))))

(defcheck solution-70ec01cc
  (fn bn [z]
    (loop [s (map #(- (int %) 48) (str z)) m 0 n 0 x 0 y (- (count s) 1)]
      (if (>= x y)
        (= m n)
        (recur s (+ m (nth s x)) (+ n (nth s y)) (inc x) (dec y))))))

(defcheck solution-70f6cf39
  (letfn [(ds [v] (->> v (iterate #(quot % 10)) (take-while #(> % 0)) (map #(rem % 10))))]
    #(let [d (ds %) c (quot (count d) 2)]
       (= (apply + (take c d))
         (apply + (take c (reverse d)))
         ))))

(defcheck solution-71fc6bff
  #(let [digs (->> % str seq (map int))
         margin (-> digs count (/ 2) int)]
     (= (apply + (take margin digs))
       (apply + (take-last margin digs)))))

(defcheck solution-72815f09
  (fn [x]
    (loop [s (map #(- (int %) 48) (seq (str x)))
           a 0
           b 0]
      (if (empty? s)
        (= a b)
        (recur (butlast (drop 1 s)) (+ a (first s)) (+ b (last s)))))))

(defcheck solution-7299518e
  (fn sym [i]
    (let [s (str i)
          e1 (quot (count s) 2)
          s1 (take e1 s)
          s2 (take-last e1 s)
          r #(+ %1 -48 (int %2))
          i1 (reduce r 0 s1)
          i2 (reduce r 0 s2)]
      (= i1 i2))))

(defcheck solution-72a6992a
  (fn balance-n
    [n]
    (let [d ((fn digits
               [n xs]
               (if (zero? n)
                 xs
                 (digits (quot n 10) (cons (rem n 10) xs)))) n []) q (quot (count d) 2)]
      (= (apply + (take q d))
        (apply + (drop (if (odd? (count d)) (inc q) q) d))))))

(defcheck solution-72d6c635
  (fn [n]
    (let [s (map (zipmap "0123456789" (range)) (str n)) m (/ (count s) 2)]
      (= (reduce + (take m s)) (reduce + (take-last m s))))))

(defcheck solution-72f956d0
  #((fn [cs]
      (let [h (/ (count cs) 2)]
        (= (sort (take h cs))
          (sort (take h (reverse cs))))))
    (vec (str %))))

(defcheck solution-73b18c7d
  (fn [n]
    (let [nl (map #(- (int %) 48) (seq (str n)))
          c (/ (count nl) 2)]
      (=
        (apply + (take c nl))
        (apply + (take c (reverse nl)))))))

(defcheck solution-7429723f
  (fn B? [n]
    (letfn [(D [n]
              (loop [n n d* []]
                (if (< n 10) (conj d* n)
                             (recur (quot n 10) (conj d* (rem n 10))))))
            (H [s*]
              (loop [s s* t []]
                (cond (= (count s) (count t)) [s t]
                      (= (count s) (inc (count t))) [s (conj t (first s))]
                      :else (recur (rest s) (conj t (first s))))))]
      (apply = (map #(apply + %) (H (D n)))))))

(defcheck solution-74417f9f
  (fn [n]
    (let [n-to-vector (fn [x] (-> x str seq (->> (map str) (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %)) (into []))))]
      (loop [xs (n-to-vector n) leftacc 0 rightacc 0]
        (if (<= (count xs) 1)
          (= leftacc rightacc)
          (recur  (into [](take (- (count xs) 2) (drop 1 xs)))
            (+ leftacc (nth xs 0))
            (+ rightacc (nth xs (dec (count xs))))))))))

(defcheck solution-74cd7a47
  (fn [n]
    (let [s (loop [n n acc []] (if (zero? n) acc (recur (quot n 10) (conj acc (mod n 10)))))
          c (quot (count s) 2)]
      (= (reduce + (take c s)) (reduce + (take-last c s))))))

(defcheck solution-74ecdc05
  (fn [input]
    (let [explode (fn explode [n]
                    (if (= n 0)
                      []
                      (let [d (rem n 10)
                            r (quot n 10)]
                        (conj (explode r) d))))
          nums (explode input)
          midpoint (/ (count nums) 2)]
      (apply = (map (fn [ns]
                      (apply + (take midpoint ns)))
                 [nums (reverse nums)])))))

(defcheck solution-7502bb8
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          length (count digits)
          count1 (quot length 2)
          count2 (- length count1)]
      (= (reduce + (take count1 digits))
        (reduce + (drop count2 digits))))))

(defcheck solution-7523e782
  (fn [n]
    (let [half (fn [s] (take (quot (count s) 2) s))
          digs (->> n str (map (comp parse-int str)))]
      (= (apply + (half digs)) (apply + (half (reverse digs)))))))

(defcheck solution-75257f31
  (fn [z] (letfn [(sum [x] (reduce + (map #(- (int %) 48) (take (/ (count x) 2) x))))] (= (sum (str z)) (sum (reverse (str z)))) )))

(defcheck solution-75387ad5
  (fn balanced-number [n]
    (letfn [(digits2 [n]
              (loop [n n
                     r '()]
                (if (= n 0)
                  r
                  (let [rem (mod n 10)
                        div (quot n 10)]
                    (recur div (conj r rem))))))
            ]

      (let [ln (apply vector (digits2 n))
            sz (count ln)
            med (int (/ sz 2))]
        (if (= sz 1)
          true
          (if (= (mod sz 2) 0)
            (let [l1 (subvec ln 0 med)
                  l2 (subvec ln  med)]
              (if (= (apply + l1) (apply + l2))
                true
                false))
            (let [l1 (subvec ln 0  med)
                  l2 (subvec ln (inc med))]
              (if (= (apply + l1) (apply + l2))
                true
                false))))))))

(defcheck solution-757dd875
  (fn bal [x]
    (let [s (map int (str x))
          c (/ (count s) 2)]
      (= (reduce + (take c s))
        (reduce + (take c (reverse s)))))))

(defcheck solution-7592c8e3
  (fn [x]
    (if (< x 10)
      true
      (let [ds                                     ; compute the digits
                                (map #(character-digit % 10) (str x))
            len (count ds)
            half-p (/ len 2)
            half (int half-p)
            left (take half ds) ; compute the left hand digits
            excluding-not-right (if (= 0 (rem len 2)) half (inc half))
            right (drop excluding-not-right ds) ; compute the right hand digits
            ]
        (= (set left) (set right) )))
    ))

(defcheck solution-7608fda
  (fn [n] (let [v (map #(- (parse-char  %) (parse-char \0)) (str n))]
            (apply = (map #(apply + %) (map #(first (split-at (/ (count %) 2) %)) (list v (reverse v))))))))

(defcheck solution-766135d3
  (fn balanced? [n]
    (let [digits (loop [x n
                        result []]
                   (if (zero? x)
                     result
                     (recur (quot x 10) (conj result (mod x 10)))))
          half (/ (count digits) 2)]
      (= (apply + (take half digits))
        (apply + (take-last half digits))))))

(defcheck solution-76c4f32d
  (fn my-balance[n](let
                    [dig-vec (mapv #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
                     num-digits (count dig-vec)
                     first-half (quot num-digits 2)
                     second-half (if (even? num-digits) first-half (inc first-half))]
                     (if (= num-digits 1) true
                                          (= (apply + (take first-half dig-vec))
                                            (apply + (drop second-half dig-vec)))))))

(defcheck solution-76fc0277
  (fn [n]
    (let [[left right] (#(split-at (int (/ (count %) 2)) %) (str n))
          rs (if (< (count left) (count right))
               (rest right)
               right)
          sumhalf (fn [xs] (apply + (map #(parse-int (str %)) xs)))]
      (= (sumhalf rs) (sumhalf left))
      )))

(defcheck solution-77a5dfce
  (fn [n] (let [s (str n)
                h (/ (count s) 2)
                [f l] (partition (int (Math/ceil h)) (int (Math/floor h)) s)]
            (= (apply + (map int f)) (apply + (map int l))))))

(defcheck solution-77f284a0
  (fn [n]
    (let [xs (vec (map #(- (parse-char %) (parse-char \0)) (vec (str n))))
          ct (count xs)
          hf (int (/ ct 2))
          left (subvec xs 0 hf)
          right (subvec xs (+ hf (if (even? ct) 0 1)))]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-78123a99
  (fn balance-num? [x]
    (letfn [(digits [x]
              (->> x
                (iterate #(quot % 10))
                (take (count (str x)))
                (map #(rem % 10))))]
      (let [d (digits x) c (count d) [l r] (split-at (quot c 2) d)]
        (if (odd? c) (= (reduce + l) (reduce + (rest r)))
                     (= (reduce + l) (reduce + r)))))))

(defcheck solution-78438b77
  (fn [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          nd (int (/ (count digits) 2))
          lsum (apply + (take nd digits))
          rsum (apply + (take nd (reverse digits)))]
      (= lsum rsum))))

(defcheck solution-7881975e
  (fn [n]
    (let [len (count (str n)),
          hflen (if (= (mod len 2) 0) (/ len 2) (/ (- len 1) 2)),
          lhf (apply str (take hflen (str n))),
          rhf (apply str (take-last hflen (str n)))]
      (letfn [(sumdig [s] (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) s)))]
        (= (sumdig lhf) (sumdig rhf))))))

(defcheck solution-79367593
  (fn [n]
    (let [v (str n)
          len (count v)
          halflen (quot (inc len) 2)
          prefix (take halflen v)
          suffix (drop (- len halflen) v)]
      (letfn [(sumdigits [v]
                (apply + (map #(- (parse-char %) (parse-char \0)) v)))]
        (= (sumdigits prefix) (sumdigits suffix))))))

(defcheck solution-794c344b
  (fn balance [n]
    (let [val (str n)
          side  (int (/ (count val) 2))]
      (loop [l 0 r 0 i side v val]
        (if (= i 0)
          (= l r)
          (recur (+ l (int (first v))) (+ r (int (last v))) (- i 1) (butlast (rest v))))))
    ))

(defcheck solution-7a437ca7
  (partial (fn [l r n]
             (if (< n 10) (= l r)
                          (letfn [(to-d [n] (loop [n n a []] (if (= 0 n) a (recur (quot n 10) (cons (rem n 10) a)))))
                                  (to-n [ds] (reduce #(+ (* 10 %) %2) 0 ds))]
                            (let [d (to-d n)]
                              (recur (+ l (first d)) (+ r (last d)) (to-n (rest (butlast d)))))))) 0 0))

(defcheck solution-7a91bbbb
  (fn [num]
    (let [digits (loop [n num res []]
                   (if (zero? n)
                     res
                     (recur (quot n 10) (cons (rem n 10) res))))
          num-per (/ (count digits) 2)
          sum-on (fn [f] (reduce + (f num-per digits)))]
      (= (sum-on take) (sum-on take-last)))))

(defcheck solution-7aa5aed7
  (fn [n]
    (let [digits (map #(character-digit % 10) (str n))
          h (/ (count digits) 2)
          sumh #(reduce + (take h %))]
      (= (sumh digits) (sumh (reverse digits))))))

(defcheck solution-7aba1131
  (fn balanced-n? [n] (let [string-n (str n) take-n (int (/ (count string-n) 2))
                            sum-str (fn[s](apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (take take-n s))))]
                        (= (sum-str string-n) (sum-str (reverse string-n))))))

(defcheck solution-7b090c84
  (fn [n]
    (let [
          d (map #(parse-int (str %)) (str n))
          c (/ (count d) 2)
          l (take c d)
          r (take c (reverse d))]
      (= (apply + l) (apply + r)))))

(defcheck solution-7b138155
  (fn balanced [n]
    (let [st (vec (str n))
          cnt (count st)
          m1 (int (/ cnt 2))
          m2 (- cnt m1)]
      (apply = (map #(->> % (map int) (reduce +))
                 [(subvec st 0 m1) (subvec st m2)])))))

(defcheck solution-7b7132f
  #(letfn [(digits [x]
             (loop [xs '()
                    remainder x]
               (if (zero? remainder)
                 xs
                 (recur (conj xs (mod remainder 10)) (quot remainder 10)))))
           (msplit [xs]
             (let [cnt (count xs)
                   size (quot cnt 2)]
               [(take size xs) (drop (- cnt size) xs)]))]
     (let [[left right] (map (partial apply +) (msplit (digits %)))]
       (= left right))))

(defcheck solution-7b73b577
  (fn p115
    [num]
    (let [s (str num)
          c (count s)
          [l r] (split-at (quot c 2) s)
          f (fn [coll] (- (reduce #(+ %1 (int %2)) 0 coll) (* 48 (count coll))))]
      (if (odd? c)
        (= (f l) (f (rest r)))
        (= (f l) (f r))))))

(defcheck solution-7b8db3b
  (fn balanced? [n]
    (let [n-str (str n)
          n-half-count (int (/ (count n-str) 2))]
      (apply = (map (fn [l] (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 l))
                 (list (take n-half-count n-str)
                   (take-last n-half-count n-str)))))))

(defcheck solution-7bb39b7c
  (fn [n]
    (let [to-digits (fn [k] (into [] (map #(mod (quot k %) 10)
                                       (take-while #(< 0 (quot k %)) (iterate #(* 10 %) 1) ))))
          half-seq #(drop (/ (count %) 2) %)]
      (= (apply + (half-seq (to-digits n))) (apply + (half-seq (reverse (to-digits n))))))))

(defcheck solution-7bc17c38
  (letfn [(num->digits
            ([m]
             (num->digits m '()))
            ([m dig]
             (if (< m 10)
               (into [m] dig)
               (recur (quot m 10) (cons (rem m 10) dig)))))
          ]
    (fn balanced? [n]
      (let [dig (num->digits n)
            half (/ (count dig) 2)
            left (take half dig)
            right (take half (reverse dig))
            ]
        (= (reduce + left) (reduce + right))))))

(defcheck solution-7bd45a4b
  (fn [x]
    (let
     [y (map #(int %) (str x))
      a (int (/ (count y) 2.0))]
      (= (apply + (take a y))
        (apply + (take-last a y))))))

(defcheck solution-7c13a5bf
  (fn bd [n]
    (let [digits (fn [x] (->> (str x) (map #(character-digit % 10))))
          nd (digits n)
          len (quot (count nd) 2)]
      (= (apply + (take len nd)) (apply + (take-last len nd))))
    ))

(defcheck solution-7c56c28c
  (fn [x]
    (loop [xs (map (comp parse-int str) (seq (str x)))
           right 0
           left 0]
      (if (<= (count xs) 1)
        (= right left)
        (recur (rest (butlast xs))
          (+ right (last xs))
          (+ left (first xs)))))))

(defcheck solution-7caa925e
  (fn [n]
    (let [s (str n)
          c (count s)
          size (int (/ c 2))
          a (take size s)
          b (take size (reverse s))]
      (= (reduce + (map int a)) (reduce + (map int b))))))

(defcheck solution-7cfb3258
  (fn balance-n? [n]
    (let [n-string (str n)
          n-seq (seq n-string)
          n-count (count n-seq)
          avg-count (/ n-count 2)
          sum (fn [s]
                (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))
                           (seq s))))]
      (= (sum (subs (clojure.string/reverse n-string) 0 avg-count))
        (sum (subs n-string 0 avg-count))))))

(defcheck solution-7d4d028b
  (fn [x]
    (let [digitsum #(apply + (map (fn [e] (#?(:clj Integer/parseInt :cljs js/parseInt) (str e))) %))
          f #(== (digitsum %) (digitsum %2))
          s (str x)
          n (count s)
          nh (/ n 2)]
      (if (even? n)
        (f (take nh s) (drop nh s))
        (f (take (int nh) s) (drop (inc (int nh)) s)) ))))

(defcheck solution-7d5ac8ee
  (fn [n]
    (let [l (count (str n))
          h (quot l 2)
          [l r] (partition (+ h (rem l 2)) h (map #(- (parse-char %) (parse-char \0)) (str n)))]
      (= (apply + l) (apply + r)))))

(defcheck solution-7d9eb709
  #(let [y (map int (str %)) z (/ (count y) 2)] (= (apply + (drop z y)) (apply + (drop-last z y)))))

(defcheck solution-7db0da38
  (fn [n] (let [digits (map #(- (int %) 48) (str n))
                x (/ (count digits) 2)]
            (= (reduce + (take x digits))
              (reduce + (take x (reverse digits)))))))

(defcheck solution-7dc8e58a
  (fn [n]
    (let [coll (map #(- (int %) 48) (str n))
          c (count coll)
          f (take (int (/ c 2)) coll)
          s (drop (- c (/ c 2)) coll)]
      (= (reduce + f) (reduce + s)))))

(defcheck solution-7dd284ea
  (fn
    [n]
    (let [coll (seq (str n))
          len  (count coll)
          half (int (/ len 2))
          ]
      (= (sort (take half coll)) (sort (drop (- len half) coll)))
      )
    ))

(defcheck solution-7dfdf763
  (fn bal? [k]
    (apply = (map
               (partial reduce +)
               (map #(map int (take (/ (count %) 2) %))
                 [(str k) (reverse (str k))])))))

(defcheck solution-7e0ae1a9
  (fn balanced? [n]
    (if (< n 10)
      true
      (let [sn (str n)
            half (int (/ (count sn) 2))
            left (take half sn)
            right (take-last half sn)
            sum (fn [s] (reduce #(+ %1 (parse-int (str %2))) 0 s))]
        (= (sum left) (sum right))))))

(defcheck solution-7e0fc20a
  (fn [n]
    (let [c0 (parse-char \0)
          sn (str n)
          cn (count sn)
          hn (quot cn 2)]
      (= (apply + (map #(- (parse-char %) c0) (subs sn 0 hn)))
        (apply + (map #(- (parse-char %) c0) (subs sn (- cn hn) cn)))))))

(defcheck solution-7e2071ac
  (letfn [(int2l [n]
            (if (< n 10) (list n) (conj (int2l (quot n 10)) (rem n 10))))]
    (fn balanced-n [n]
      (let [l (int2l n)
            cl (count l)
            m (if (even? cl) (/ cl 2) (/ (dec cl) 2))]
        (= (apply + (take m l)) (apply + (take m (reverse l))))))))

(defcheck solution-7e68b0ba
  (fn [n]
    (= 1 (count (set
                  (map #(reduce + %)
                    (map #(take (/ (count %) 2) %)
                      (#(list % (reverse %))
                       (map int (str n)))))))
      )))

(defcheck solution-7ebce8f3
  (fn [n]
    (let [
          [s] [str n]
          [m] [(int (/ (count (str n)) 2))]
          [ints] [(map int (str n))]]
      (= (reduce + (take m ints)) (reduce + (take-last m ints))))))

(defcheck solution-7f354868
  (fn [y]
    (let [s (seq (str y))
          c (/ (count s) 2)
          f #(sort %)]
      (= (f (take c s))  (f (take-last c s))))))

(defcheck solution-80095a80
  (fn [n] (let [xs (map int (str n))
                l (/ (count xs) 2)]
            (apply = (map (partial apply +)
                       [(take l xs)
                        (take l (reverse xs))])))))

(defcheck solution-8009eaa4
  (fn [n]
    (let [m (map #(- (int %) 48) (str n))
          c  (count m)
          h  (quot c 2)
          f   #(reduce + (take h %))]
      (= (f m) (f (reverse m))))))

(defcheck solution-802c351b
  (fn [n]
    (let [digits (map #(- (int %) 48) (str n))
          k (int (/ (count digits) 2))]
      (= (apply + (take k digits))
        (apply + (take k (reverse digits)))))))

(defcheck solution-80a11b40
  (fn [i]
    (let [n (->> i str (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))))
          c (/ (count n) 2)]
      (= (apply + (take c n))
        (apply + (take c (reverse n)))))))

(defcheck solution-80da2a6b
  (fn [n]
    (let [s (str n)
          l (quot (count s) 2)
          f (fn [l s] (reduce #(+ % (- (int %2) 48)) 0 (take l s)))]
      (= (f l s) (f l (reverse s))))))

(defcheck solution-80f2e42f
  (fn balanced? [n]
    (let [all (map (comp parse-int str) (str n))
          left (take (quot (count all) 2) all)
          right (take-last (count left) all)]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-80f6396e
  (fn [n]
    (let [ds (seq (str n))
          m (int (/ (count ds) 2))
          xs (map int (take m ds))
          ys (map int (take m (reverse ds)))]
      (= (reduce + xs) (reduce + ys)))))

(defcheck solution-8123330b
  (fn [x] (let
           [xn (map int (str x))
            half (quot (count xn) 2)
            left (take half xn)
            right (take-last half xn)
            lsum (reduce + left)
            rsum (reduce + right)]
            (= lsum rsum))))

(defcheck solution-81b2a01e
  (fn [n]
    (let [s (str n)
          x (int (/ (count s) 2))
          f (comp parse-int str)
          s1 (map f (take x s))
          s2 (map f (take-last x s))]
      (= (reduce + s1) (reduce + s2)))))

(defcheck solution-81ea003a
  (fn [n]
    (let [n (map #(- (int %) 48) (str n))
          l (quot (count n) 2)
          [a, b] (split-at l n)
          b (if (zero? (mod (count n) 2)) b (rest b))]
      (= (apply + a) (apply + b)))))

(defcheck solution-822b6505
  (fn [x]
    (letfn [(dig [n]
              (loop [d '() n n]
                (if (zero? n) d
                              (recur (cons (rem n 10) d) (quot n 10)))))]
      (let [d (dig x)
            n (count d)  h (quot n 2)
            l (take h d) r (take-last h d)]
        (= (apply + l) (apply + r))))))

(defcheck solution-82581b30
  (fn balanced-num? [n]
    (let [cs (str n)
          c (count cs)]
      (if (= c 1)
        true
        (let [num->sum (fn [cs]
                         (reduce +
                           (map #(-> %
                                   str
                                   #?(:clj Integer/parseInt :cljs js/parseInt)) cs)))
              h (quot c 2)
              [ls rs] (split-at h cs)
              [ls2 rs2] (if (even? c)
                          [ls rs]
                          [ls (rest rs)])]
          (= (num->sum ls2) (num->sum rs2)))))))

(defcheck solution-829a54b4
  (fn balanced? [n]
    (let [digits (map #(- (int %) 48) (str n))
          c (count digits)
          [l r] (split-at (quot c 2) digits)]
      (= (reduce + l) (reduce + (if (even? c) r (rest r)))))))

(defcheck solution-82fc0ea8
  (fn [n]
    (let [dig (vec (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (re-seq #"\d" (str n)))) t (count dig)]
      (= (reduce + (subvec dig 0 (int (/ t 2))))
        (reduce + (subvec dig (- t (int (/ t 2))) t))
        )
      )
    ))

(defcheck solution-8423ae99
  (fn
    [n]
    (let [v (vec (str n))
          size (count v)
          half (quot size 2)
          s1 (apply + (map int (take half v)))
          s2 (apply + (map int (take-last half v)))]
      (= s1 s2))))

;; See CLJS-2462
#_(defcheck solution-85050599
  (fn [x]
    (let [v (loop [n x o []] (if (zero? n) o (recur (int (/ n 10)) (conj o (mod n 10)))))
          m (/ (count v) 2)]
      (= (reduce + (subvec v 0 m)) (reduce + (subvec v (+ m 0.5))))
      )
    ))

(defcheck solution-8518af52
  #(let [s (seq (str %))
         t (quot (count s) 2)
         d (+ t (mod (count s) 2))]
     (letfn [(mysum [ss] (reduce (fn [a i] (+ a (parse-int (str i)))) 0 ss))]
       (= (mysum (take t s)) (mysum (drop d s))))))

(defcheck solution-854c7bcc
  (fn [n]
    (let [s (str n)
          h (int (/ (count s) 2))
          l (take h s)
          r (take h (reverse s))
          sum (fn [s] (reduce #(+ (int %2) %1) 0 s))]
      (= (sum l) (sum r)))))

(defcheck solution-85de2bc0
  (fn balance [x]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str x))
          n (/ (count digits) 2)
          left (take n digits)
          right (take-last n digits)]
      (= (apply + left) (apply + right))
      )
    ))

(defcheck solution-8614b412
  #(or (< % 10)
       (let [s (map parse-int (re-seq #"[0-9]" (str %)))
             c (count s)
             m (/ c 2)
             [l r] (split-at m s)
             sl (reduce + l)
             sr (reduce + r)]
         (if (odd? c)
           (= (- sl (last l)) sr)
           (= sl sr)))))

(defcheck solution-86a3594
  (fn balanced? [n]
    (let [l (list* (str n))
          half (quot (count l) 2)
          lt (take half l)
          rt (take-last half l)
          to-int #(- (parse-char %) (parse-char \0))]
      (= (reduce + (map to-int lt))
        (reduce + (map to-int rt))))))

(defcheck solution-877b2ee4
  (fn balance? [n]
    (let [s (str n),
          half (quot (inc (count s)) 2),
          s1 (take half s),
          s2 (take-last half s)]
      (=
        (apply + (map int s1))
        (apply + (map int s2))
        ))))

(defcheck solution-877ec6d4
  (fn [n]
    (letfn [(halfsum [s] (->> s
                           (take (int (/ (count s) 2)))
                           (map #(parse-char %))
                           (apply +)))]
      (= (halfsum (str n)) (halfsum (reverse (str n)))))))

(defcheck solution-87c10982
  (fn z [n]
    (let [numberAsSeq (map #(parse-char %) (str n))
          cnt (count numberAsSeq)
          sumFn (fn [coll] (reduce + coll))]
      (cond
        (= 1 cnt) true
        :else (= (sumFn(take (quot cnt 2) numberAsSeq)) (sumFn(drop (+ (quot cnt 2) (rem cnt 2)) numberAsSeq)))))))

(defcheck solution-881c34d6
  (fn [n]
    (let [sn (str n)
          cn (quot (count sn) 2)
          [ ln rrn ] (split-at cn sn)
          rn (if (= (count rrn) cn) rrn (rest rrn))
          sum #(+ %1 (parse-int (str %2)))
          l  (reduce sum 0 ln)
          r  (reduce sum 0 rn)
          ]
      ( = l r)
      )))

(defcheck solution-88811d0
  (fn [n]
    (let [n->d #(map (comp int parse-int str) (seq (str %)))]
      (->> (n->d n)
        (#(list % (reverse %)))
        (map #(take (/ (count %) 2) %))
        (map (partial reduce +))
        (apply =)))))

(defcheck solution-888d8513
  (fn [num]
    (if (< num 10)true
                  (let [vlist (vec (str num)) len (count vlist) lsp (quot len 2) rsp (if (even? len) lsp (inc lsp))  llist (subvec vlist 0 lsp) rlist (subvec vlist rsp len)]
                    (= (apply + (map #(parse-int (str %)) llist)) (apply + (map #(parse-int (str %)) rlist)))
                    )
                  )
    ))

(defcheck solution-889d2cd9
  (fn bal [n]
    (let [sn    (seq (str n))
          snc   (quot (count sn) 2)
          left  (take snc sn)
          right (drop (if (= 0 (mod (count sn) 2)) snc (inc snc)) sn)
          to-sum (fn [xs] (reduce + (map (comp #(#?(:clj Integer/parseInt :cljs js/parseInt) %) str) xs)))]
      (= (to-sum left) (to-sum right)))))

(defcheck solution-889dbf7d
  (fn balance-n
    [n]
    (let [
          lst (map #(- (parse-char %) (parse-char \0)) (str n))
          len (count lst)
          half (quot len 2)
          flag (if (even? len) 0 1)]
      (= (apply + (take half lst)) (apply + (drop (+ half flag) lst))))))

(defcheck solution-88b7ee76
  #(let [c (
            (fn num2diglist [n retcoll]
              (if (> n 0)
                (num2diglist (int (/ n 10)) (conj retcoll (mod n 10)))
                retcoll
                )
              ) % '())
         n (int (/ (count c) 2))]
     (= (reduce + (take n c)) (reduce + (take n (reverse c))))
     ))

(defcheck solution-88f12e30
  (fn [n] (let [s (map (comp parse-int str) (str n)) m (int (/ (count s) 2))] (= (apply + (take m s)) (apply + (take-last m s))))))

(defcheck solution-8903bdbc
  (fn balance?
    [n]
    (let [coll (map (comp parse-int str) (str n))
          n (/ (count coll) 2)
          xs (take n coll)
          ys (take n (reverse coll))]
      (= (reduce + xs) (reduce + ys)))))

(defcheck solution-892a45ec
  (fn [n]
    (let [lst (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (remove empty? (.split (str n) "")))
          tk (int (/ (count lst) 2))]
      (apply = (map #(apply + %) [(take tk lst) (take-last tk lst)]) ))))

(defcheck solution-8935a91d
  (fn balance-of-n [num]
    (letfn [(to-digits [n]
              (loop [acc ()
                     remainder n]
                (if (zero? remainder)
                  acc
                  (recur (conj acc (rem remainder 10)) (quot remainder 10)))))]
      (let [digits (to-digits num)
            middle (quot (count digits) 2)]
        (= (apply + (take middle digits)) (apply + (take middle (reverse digits))))
        ))))

(defcheck solution-8936ccd1
  (fn [n] (let [digits (map #(- (int %) 48)(str n))
                halflen (int (/ (count digits) 2))
                sum1 (apply + (take halflen digits))
                sum2 (apply + (take halflen (reverse digits)))
                ]
            (= sum1 sum2))))

(defcheck solution-894ced8b
  (fn [n] (let [s (map #(parse-int (str %)) (str n)) l (/ (count s) 2)] (= (apply + (take l s)) (apply + (take-last l s))))))

(defcheck solution-8a1ce48b
  (fn balance[n]
    (let[j (str n)
         l (fn h[s]
             (if
              (seq s)
               (cons (- (int (first s)) 48) (h (next s)))))
         chap (apply str (take (/ (count j) 2) j))
         rast (apply str (take-last (/ (count j) 2) j))]
      (= (reduce + (l chap)) (reduce + (l rast))))))

(defcheck solution-8a2b8e02
  (fn bal [n]
    (let
     [s (str n)
      half (int (/ (count s) 2))
      left (take half s)
      right (take-last half s)
      to-int #(- (int %) 48)
      sum-side #(apply + (map to-int %))]
      (= (sum-side left) (sum-side right)))))

(defcheck solution-8a3ad4bb
  (fn [n]
    (let [s (str n)
          take-half #(% (quot (count s) 2) s)
          sum-digits #(apply + (map int (take-half %)))
          left (sum-digits take)
          right (sum-digits take-last)]
      (= left right))))

(defcheck solution-8a553d56
  (fn [n]
    (let [dts (->> n str (map int))
          [l r] (split-at (quot (count dts) 2) dts)
          r (if (= (count l) (count r)) r (rest r))]
      (apply = (map #(apply + %) [l r])))))

(defcheck solution-8a5d4746
  (fn [n]
    (let [s (str n)
          f (fn [x]
              (reduce #(+ % (- (int %2) 48))
                0 (x (/ (count s) 2) s)))]
      (= 0 (- (f take) (f take-last))))))

(defcheck solution-8ac622b7
  (fn balanced? [n]
    (let [ls (seq (str n))
          l  (count ls)
          left (if (even? l) (take (quot l 2) ls ) (take (inc (quot l 2)) ls))
          right (drop (quot l 2) ls)
          digit-sum (fn [xs] (reduce #(+ %1 (- (int %2) 48) ) 0 xs))]
      (= (digit-sum left) (digit-sum right)))))

(defcheck solution-8ada1fe9
  (fn [n]
    (let [ds (str n)
          m (/ (count ds) 2)
          l (take (Math/floor m) ds)
          r (drop (Math/ceil m) ds)]
      (letfn [(f [x y]
                (+ (parse-int (str y)) x))]
        (= (reduce f 0 l) (reduce f 0 r))))))

(defcheck solution-8ae65d08
  (let [digits (fn [i]
                 (loop [d '() q i]
                   (if (= 0 q)
                     d
                     (recur (cons (mod q 10) d) (quot q 10)))))
        left-and-right (fn [i]
                         (let [d (digits i)
                               n (count d)
                               n' (int (/ n 2))
                               [l r] (split-at n' d)]
                           (if (even? n)
                             [l r]
                             [l (drop 1 r)])))]
    (fn [i]
      (apply = (map (fn [side] (apply + side)) (left-and-right i))))))

(defcheck solution-8b591971
  (fn [a]
    (let [revstra (reverse (str a))]
      (= (reduce #(+ (int %2) %) 0 (take (quot (count (str a)) 2) (str a)))
        (reduce #(+ (int %2) %) 0 (take (quot (count (str a)) 2) revstra))))))

(defcheck solution-8bc2e469
  #(let [xs ((fn [n r] (if (zero? n) r (recur (quot n 10) (cons (rem n 10) r)))) % '())
         c (quot (count xs) 2)
         d (if (zero? c) 1 c)]
     (= (reduce + (take c xs)) (reduce + (take c (reverse xs))))))

(defcheck solution-8c43f5f
  (fn sym-sum [n]
    (let [digits (->> n str (map int) (map #(- % 48)))
          n (count digits)
          left-sum (apply + (take (quot n 2) digits))
          right-sum (apply + (drop (/ n 2) digits))]
      (= left-sum right-sum))))

(defcheck solution-8c4538a8
  (fn [n]
    (let [nums (map #(parse-char %) (str n))
          len (count nums)
          i (int (/ len 2))
          [xs ys] (split-at i nums)
          sum (partial reduce +)
          sum-xs (sum xs)
          sum-ys (sum (if (even? len) ys (rest ys)))]
      (= sum-xs sum-ys))))

(defcheck solution-8c54b90b
  (fn balanced?
    [n]
    (let [l (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))
          f (fn [n s]
              (= (reduce + (take n l))
                (reduce + (take n (reverse l)))))]
      (if
       (odd? (count l)) (f (inc (int (/ (count l) 2))) l)
                        (f (int (/ (count l) 2)) l)))))

(defcheck solution-8c624410
  (fn [n]
    (let [digits (->> (str n)
                   (map #(- (parse-char %) (parse-char \0))))
          q (quot (count digits) 2)]
      (= (reduce + (take q digits))
        (reduce + (take-last q digits))))))

(defcheck solution-8daa7ad
  (fn [x]
    (let [as (map #(- (parse-char %) (parse-char \0)) (str x))
          bs (reverse as)
          n (int (/ (count as) 2))]
      (= (apply + (take n as)) (apply + (take n bs))))))

(defcheck solution-8de0fec3
  (fn balanced? [n]
    (let [digit #({\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9} %)
          digits (vec (map digit (str n)))
          middle (quot (count digits) 2)
          left-side (subvec digits 0 middle)
          right-side (subvec digits (if (even? (count digits)) middle (inc middle)))]
      (= (reduce + left-side) (reduce + right-side)))))

(defcheck solution-8e08daf3
  (fn balance?
    [num]
    (let [part (quot (count (str num)) 2)
          coll (partition part (concat (take part (str num)) (take part (reverse (str num)))))
          un-char (fn [m] (map #(parse-int (str %)) m))]
      (= (apply + (un-char (first coll)))
        (apply + (un-char (last coll)))))))

(defcheck solution-8e1ef003
  (fn [n]
    (let [s1 (str n)
          s2 (reverse s1)
          sum (fn [s] (apply + (map #(parse-int (str %)) (take (/ (count s1) 2) s))))]
      (= (sum s1) (sum s2)))))

(defcheck solution-8e2b9b8a
  (fn [n]
    (if (< n 10)
      true
      (let [ss (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
            nb (/ (count ss) 2)
            fst (take (Math/floor nb) ss)
            lst (drop (Math/ceil nb) ss)]
        (= (reduce + fst) (reduce + lst))))))

(defcheck solution-8e3d0cd
  (fn [x]
    (let [s (str x)
          c (/ (count s) 2)
          cv (fn [s] (apply + (map int s)))
          ]
      (= (cv (take c s)) (cv (take-last c s))))))

(defcheck solution-8ea5ecc6
  (fn [number]
    (let [sn (str number)
          n (quot (count sn) 2)]

      (apply
        =
        (map
          #(apply + (map parse-int (map str %)))
          [(take n sn) (take-last n sn)])))))

(defcheck solution-8ec5061a
  (fn [s]
    (let [xs (map int (str s)) h (quot (count xs) 2)]
      (=
        (reduce + (take h xs))
        (reduce + (take h (reverse xs)))
        )
      )
    ))

(defcheck solution-8ee00b2b
  (letfn
   [
    ( digits [n]
      (let [[ds n]
            (last (take-while
                    (comp (complement zero?) second)
                    (iterate
                      (fn [[ds n]] [(conj ds (mod n 10)) (int (/ n 10))])
                      [[] n])))]
        (conj ds n)))
    ( split [n]
      (let [ds (digits n)
            c (count ds)
            hc (int (/ c 2))]
        [(take hc ds) (drop (+ hc (mod c 2)) ds)]))]
    (fn [n]
      (= 1 (count (distinct (map (partial reduce +) (split n))))))))

(defcheck solution-8eef17c8
  #(let [s (str %) n (count s) a (quot n 2) b (if (odd? n) (inc a) a)]
     (= (apply + (map int (take a s ))) (apply + (map int (drop b s))))))

(defcheck solution-8f308b16
  (fn [x]
    (let [s (map int (str x))]
      (apply =
        (map #(apply + (take (quot (count s) 2) %))
          [s (reverse s)])))))

(defcheck solution-8f7f53b4
  (fn x [n]
    (let [s (str n)
          c (count s)
          [a b] (split-at (quot c 2) s)
          c (drop (if (odd? (count s)) 1 0) b)
          f (fn [x] (reduce #(+ % (parse-char %2)) 0 x))]
      (= (f a) (f c)))))

(defcheck solution-8f82ce82
  (fn [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (re-seq #"[\d]" (str n)))
          half-count (/ (count digits) 2)
          left-sum (reduce + (take half-count digits))
          right-sum (reduce + (take-last half-count  digits))]
      (= left-sum right-sum))))

(defcheck solution-8ff1c42b
  (fn [n]
    (let [half (int (/ (inc (count (str n))) 2))
          left (take half (str n))
          right (take-last half (str n))
          f (fn [x] (apply + (map #(- (int %) 48) x)))]
      (apply = (map f [left right])))))

(defcheck solution-903bf795
  (fn [n]
    (->> n
      (str)
      (seq)
      ((fn [coll]
         (let [cnt (count coll)
               len (+ (quot cnt 2) (rem cnt 2))]
           [(take len coll) (take-last len coll)])))
      (map #(apply + (map int %)))
      (apply =))))

(defcheck solution-908c48e1
  (letfn [(sum-digit-seq [x] (apply + (map #(parse-int (str %)) x)))]
    (fn [n]
      (let [s (seq (str n))
            num-digits (count s)
            k (if (even? num-digits) (/ num-digits 2) (/ (dec num-digits) 2))
            a (take k s)
            b (drop (if (even? num-digits) k (inc k)) s)
            sum-a (sum-digit-seq a)
            sum-b (sum-digit-seq b)]
        (= sum-a sum-b)))))

(defcheck solution-90b20965
  (fn [n] (let [s (str n)
                half (quot (count s) 2)
                sum-digits (fn [s] (reduce
                                     #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 s))
                s1 (sum-digits (subs s 0 half))
                s2 (sum-digits (subs s (- (count s) half)))]
            (if (= s1 s2) true false))))

(defcheck solution-912484c2
  (fn bal [xint]
    (let [s (str xint)
          hd (quot (count s) 2)
          hu (if (> (rem (count s) 2) 0)
               (inc hd)
               hd)
          a (take hd s)
          b (drop hu s)]
      (= (apply + (map int a)) (apply + (map int b))))))

(defcheck solution-9197d567
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          mid (int (/ (count digits) 2))
          [left right] (split-at mid digits)
          ;; maybe discard middle, if count was odd:
          right (if (= (count left) (count right)) right (rest right))]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-9198bc8e
  (letfn
   [(digits [n] (map (fn [c] (parse-int (str c))) (str n)))
    (sum [xs] (reduce + xs))]
    (fn [n]
      (let [ds (digits n)
            n (int (/ (count ds) 2))]
        #_(println ds n)
        (= (sum (take n ds)) (sum (take-last n ds)))))))

(defcheck solution-91bd0be
  #(let [s (str %)
         c (int (/ (count s) 2))]
     (apply =
       (map (fn [a] (apply + (map int a))) [(take c s) (take c (reverse s))]))))

(defcheck solution-922c0e47
  (fn nsum [n]
    (let [s (str n)
          l (count s)
          as-numbers (map (comp #(parse-int %) str) s)
          left-half (take (/ l 2) as-numbers)
          right-half (take (/ l 2) (reverse as-numbers))]
      (= (reduce + left-half)
        (reduce + right-half)))))

(defcheck solution-92440a5
  (fn is-balanced?
    [n]
    (let [s (map #(parse-int (str %)) (str n))
          size (-> (count s) (/ 2) int)
          left (apply + (take size s))
          right (apply + (->> s reverse (take size)))]
      (= left right))))

(defcheck solution-92b4dff5
  (fn [n]
    (letfn [(halve [s] (let [c (count s)
                             half (quot c 2)
                             split (split-at (+ half (mod c 2)) s)]
                         (map #(take half %) split)))]
      (->> (halve (str n))
        (map (fn [l] (apply + (map #(parse-char %) l))))
        (apply -)
        (zero?)))))

(defcheck solution-92e4fbb8
  (fn t [y]
    (->> y
      str
      (count)
      (#(+ (quot %  2) (rem % 2)))
      ((fn [x] [(take x (str y)) (take x (reverse (str y)))]))
      ((fn [v] (= (reduce + (map #(- (int %) 48)  (first v)))
                 (reduce + (map #(- (int %) 48)  (second v)))
                 )
         ))
      ;;#(%)
      )))

(defcheck solution-92ea34c5
  (fn bln? [n]
    (let [c (map #(- (int %) 48) (seq (str n)))
          fh (fn [c] (take (quot (count c) 2) c))]
      (= (apply + (fh c)) (apply + (fh (reverse c)))))))

(defcheck solution-93084fc6
  (fn [n]
    (let [st (str n)
          half (/ (count st) 2)
          front (take half st)
          back (take half (reverse st))]
      (apply =
        (map (fn [n-seq] (reduce + n-seq))
          (map (fn [st-seq] (map #(parse-int (str %)) st-seq)) [front back]))))))

(defcheck solution-934f258
  (fn balanced-num [n]
    (let [digits ((fn toDigits [n]
                    (if (zero? n)
                      []
                      (conj (toDigits (quot n 10))
                        (rem n 10)))) n)
          sz (int (/ (count digits) 2))]

      (= (reduce + (take sz digits))
        (reduce + (take sz (reverse digits)))))))

(defcheck solution-94998887
  (fn [a]
    (let [s (str a)
          len (count s)
          half (quot len 2)
          sum (fn [ss] (apply + (map #(parse-char %) ss)))]
      (=  (sum (subs s 0 half)) (sum (subs s (- len half)))))))

(defcheck solution-949c77bb
  (fn balance? [n]
    (let [sn (str n)
          half-size (quot (count sn) 2)
          sum-digits #(apply + (map int %))]
      (= (sum-digits (subs sn 0 half-size))
        (sum-digits (subs sn (- (count sn) half-size)))
        ))))

(defcheck solution-949dcddd
  (fn [n]
    (let [hlen (/ (count (str n)) 2)
          s0 (take hlen (str n))
          s1 (take hlen (reverse (str n)))
          sums (fn [s] (apply + (map int s)))
          ]
      (= (sums s0) (sums s1))
      )
    ))

(defcheck solution-94b1bfc2
  (fn [N]
    (let [d (map #(- (parse-char %) (parse-char \0)) (seq (str N)))
          h (int (/ (count d) 2))]
      (apply =
        (map #(apply + %)
          [(take h d) (take-last h d)])))))

(defcheck solution-94ddaab6
  (fn [x]
    (let [x (map int (str x))
          t (int (/ (count x) 2))
          a (apply + (take t x))
          b (apply + (take t (reverse x)))]
      (= a b))))

(defcheck solution-95087e64
  (fn bal? [n]
    (let [ds ((fn g [n]
                (if (zero? n)
                  []
                  (cons (mod n 10) (g (quot n 10)))
                  )
                ) n)
          l (quot (count ds) 2)
          h (take l ds)
          t (take-last l ds)
          ]
      (= (apply + h) (apply + t))
      )
    ))

(defcheck solution-953a6d1f
  (fn[x](let[v(mapv int(str x))a apply l(count v)s #(a +(a subvec %&))](=(s v 0(Math/ceil(/ l 2.)))(s v(quot l 2))))))

(defcheck solution-95e32378
  #(let [s (str %)
         h (/ (count s) 2)]
     (apply = (map frequencies [(take h s) (take-last h s)]))))

(defcheck solution-96745fba
  (fn [i](->>
           i
           str
           (map #(->> % int (+ -48)))
           ((juxt identity reverse))
           (map #(->>
                   %
                   identity
                   (reductions +)
                   ((fn [n] (nth n (bit-shift-right (dec (count n)) 1) )))
                   )
             )
           (apply =)
           )))

(defcheck solution-9676196f
  (fn [x]
    (let [to-digits
                 (fn to-digits [n base]
                   (let [r (rem n base)
                         n' (int (/ n base))]
                     (if (= 0 n')
                       [r]
                       (vec (concat (to-digits n' base) [r])))))
          digits (to-digits x 10)
          n (count digits)
          [lhs rhs'] (split-at (int (/ n 2)) digits)
          rhs ((if (odd? n) rest identity) rhs')]
      #_(println lhs rhs)
      (= (reduce + lhs)
        (reduce + rhs)))))

(defcheck solution-96f026bd
  (fn [n]
    (let [digits (map parse-int (map str (seq (str n))))]
      (loop [b 0 x digits]
        (if (empty? (rest x))
          (= b 0)
          (recur (+ b (- (first x) (last x))) (rest (butlast x))))))))

(defcheck solution-9708cac8
  #(let [t (str %) r (reverse t) c (/ (count t) 2) i (fn [x] (set (take c x)))] (= (i t) (i r))))

(defcheck solution-97399a0a
  (fn [x]
    (let [s (str x) aj (fn [l] (apply + (map #(parse-char %) l)))]
      (if (=
            (aj (take
                  (if (even? (count s))
                    (/ (count s) 2)
                    (dec (/ (count s) 2)))
                  s))
            (aj (nthrest s (/ (count s) 2))))
        true false))))

(defcheck solution-97785c10
  (fn balanced? [x]
    (let [digits (into [] (map (comp parse-int str) (seq (str x))))
          c (count digits)
          mid (int (/ c 2))
          left (subvec digits 0 mid)
          right (subvec digits
                  (if (even? c)
                    mid
                    (inc mid))
                  c)
          sum_left (reduce + left)
          sum_right (reduce + right)]
      (= sum_left sum_right))))

(defcheck solution-978cbfd8
  (fn [n] (let [digits (#(loop [ds (), n' %]
                           (if (< n' 10) (cons n' ds)
                                         (recur (cons (rem n' 10) ds) (quot n' 10)))) n)
                l (quot (count digits) 2)]
            (= (reduce + (take l digits)) (reduce + (take l (reverse digits)))))))

(defcheck solution-97ce6751
  (fn [x]
    (let [digits (map #(- (parse-char %) (parse-char \0))
                   (str x))
          c (quot (count digits) 2)
          sides ((juxt (partial take c) (partial take-last c)) digits)]
      (apply == (map #(reduce + %) sides)))))

(defcheck solution-9857ce7a
  (fn b [n]
    (let [s (map #(str %) (str n))
          c (count s)
          to-take (if (odd? c) (/ (dec c) 2) (/ c 2))
          f (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (take to-take s))
          l (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (take-last to-take s))
          sym (= f (reverse l))
          ]

      #_(prn f l to-take)
      (if (or (= c 1) (= (reduce #(+ %1 %2) f) (reduce #(+ %1 %2) l) ))
        true
        false
        )


      )

    ))

(defcheck solution-98c4423c
  #(let [s (str %)
         c (/ (count s) 2)
         l (take c s)
         r (take c (reverse s))]
     (= (apply + (map int l))
       (apply + (map int r)))))

(defcheck solution-9921700d
  (fn [n]
    (let [n (map #(- (int %) 48) (seq (str n)))]
      (=
        (apply + (take (Math/ceil (/ (count n) 2)) n))
        (apply + (take (Math/ceil (/ (count n) 2)) (reverse n)))))))

(defcheck solution-99797d1c
  (fn [x]
    (let [s (map #(- (int %) 48) (str x))]
      (= (reduce + (take (quot (count s) 2) s)) (reduce + (drop (quot (inc (count s)) 2) s))))))

(defcheck solution-998ab0ff
  (fn [num]
    (let [digits (str num)
          half-length (Math/floor (/ (count digits) 2))
          left-half (take half-length digits)
          right-half (take half-length (reverse digits))
          sum-digits #(->> %
                        (map (fn [n] (- (parse-char n) (parse-char \0))))
                        (reduce +))]
      (= (sum-digits left-half)
        (sum-digits right-half)))))

(defcheck solution-999a6b52
  (fn [n]
    (let [d-seq (into [] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n)))
          mid (int (/ (count d-seq) 2))
          left (subvec d-seq 0 mid)
          right (subvec d-seq (if (odd? (count d-seq)) (inc mid) mid))]
      (= (reduce + 0 left)
        (reduce + 0 right)))))

(defcheck solution-999a8001
  (fn [n]
    (let [s (str n)
          length (count s)
          strings (cond (even? length) (split-at (/ length 2) s)
                        (= length 1) (list s)
                        :else (list (take (/ (dec length) 2) s)
                                (drop (/ (inc length) 2) s)))]
      (->> (map (fn [coll]
                  (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) coll))
             strings)
        (map #(reduce + %))
        (apply =)))))

(defcheck solution-99a4b65
  (fn [n]
    (if (< n 10)
      true
      (let [
            s (str n)
            l (count s)
            r (rem l 2)
            n (/ (- l r) 2)
            lft (take n s)
            rgt (drop (+ n r) s)
            nlft (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) lft))
            nrgt (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) rgt))]
        (= nlft nrgt)))))

(defcheck solution-99b76e31
  (fn balanced?- [n]
    "115. Write a function which accepts an integer n, and returns true
  iff n is balanced."
    (letfn [(digits [n] (if (< n 10) [n] (conj (digits (quot n 10)) (rem n 10))))]
      (let [d (digits n)
            left (take (/ (count d) 2) d)
            right	(take-last (/ (count d) 2) d)]
        (= (reduce + left) (reduce + right))))))

(defcheck solution-99c07b55
  (fn [n]
    (cond (< n 0) false
          (< n 9) true
          :else (letfn [(int-to-seq [n] (map #(- (int %) 48) (seq (str n))))]
                  (let [s (vec (int-to-seq n))
                        len (count s)
                        m (int (/ len 2))
                        l (subvec s 0 m)
                        r (subvec s (+ m (if (even? len) 0 1)))]
                    (= (reduce + l)
                      (reduce + r)))))))

(defcheck solution-9a6518d4
  (fn [number]
    (let [num-vector
          ((fn [num]
             (loop [result [] divider num ]
               (if (= 0 divider)
                 (vec (reverse result))
                 (recur (conj result (mod divider 10))
                   (int (/ divider 10))
                   )
                 )
               )
             ) number ) ]
      (cond
        (= 1 (count num-vector)) true
        (odd? (count num-vector)) (let [first-vec (subvec num-vector 0 (int (/ (count num-vector) 2)))
                                        second-vec (subvec num-vector (+ 1 (int (/ (count num-vector) 2))))
                                        ]
                                    (= (reduce + first-vec) (reduce + second-vec))
                                    )
        :else (let [first-vec (subvec num-vector 0 (/ (count num-vector) 2))
                    second-vec (subvec num-vector (/ (count num-vector) 2))
                    ]
                (= (reduce + first-vec) (reduce + second-vec))
                )
        )

      )
    ))

(defcheck solution-9a736348
  (fn balance[n]
    (letfn [(str-sum [s]
              (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (.toString %)) s)))]
      (let [num-s (.toString n)
            len (quot (count num-s) 2)]
        (= (str-sum (take len num-s))
          (str-sum (take-last len num-s)))))))

(defcheck solution-9aa227c3
  (fn [n] (let [dfn (fn dfn [x] (if (< x 10) [x] (conj (dfn (quot x 10)) (rem x 10))))
                digits (dfn n)
                x (count digits)]
            (= (reduce + (take (quot x 2) digits)) (reduce + (drop (- x (quot x 2)) digits))))))

(defcheck solution-9acbe71
  (fn [n]
    (let [s (str n)
          m (quot (count s) 2)
          ds #(apply + (map (comp parse-int str) (take m %)))]
      (= (ds s) (ds (reverse s))))))

(defcheck solution-9b033355
  (fn [x]
    (let [to-i #(- (parse-char %) (parse-char \0))
          s (str x)
          c (count s)
          m (int (/ c 2))]
      (=
        (apply + (map to-i (take m s)))
        (apply + (map to-i (drop (- c m) s)))))))

(defcheck solution-9b1cee34
  (fn balanced [n]
    (let [s (str n)
          endl (quot (count s) 2)
          startr (quot (inc (count s)) 2)
          red-sum (partial reduce +)
          digits (map int s)
          l (take endl digits)
          r (drop startr digits)]
      (= (red-sum l) (red-sum r))
      )
    ))

(defcheck solution-9b54b9b0
  (fn balanced?[x]
    (let [digits (fn [x] (map (zipmap "0123456789" (range 10)) (str x)))
          d (digits x)
          n (count d)
          h (quot n 2)
          p1 (take h d)
          p2 (drop (- n h) d)
          ]
      (= (reduce + p1) (reduce + p2)))))

(defcheck solution-9b96faae
  #(let [s (map int (str %))
         n (count s)]
     (= (apply + (take (/ n 2) s))
       (apply + (take-last (/ n 2) s)))))

(defcheck solution-9bb359fe
  (fn balanced? [n]
    (let [ns  (str n)
          hc  (int (/ (count ns) 2))
          sum (fn [chars] (->> chars
                            (map #(- (int %) 48))
                            (reduce +)))
          f   (sum (take hc ns))
          b   (sum (take hc (reverse ns)))]
      (== f b))))

(defcheck solution-9bb5f72a
  (fn [n]
    (letfn [(digits [x]
              (loop [n x
                     ds '()]
                (if (zero? n)
                  ds
                  (let [d (mod n 10)
                        r (quot n 10)]
                    (recur r (cons d ds))))))]
      (let [ds (digits n)
            l (quot (count ds) 2)]
        (= (reduce + (take l ds))
          (reduce + (take l (reverse ds))))))))

(defcheck solution-9bc3fc92
  (fn [n]
    (let [digits (map #(-> % str parse-int) (str n))
          [h1 h2] (split-at (/ (count digits) 2) digits)
          h1 (if (= (count h1)(count h2)) h1 (butlast h1))]
      (= (apply +  h1) (apply + h2)))))

(defcheck solution-9beb540f
  (fn [n]
    (let [s (str n) x (quot (count s) 2) f (fn [a] (apply + (map #(parse-int (str %)) a)))]
      (= (f (take x s)) (f (take-last x s))))))

(defcheck solution-9c76d943
  (fn [b]
    (->> (map #(- (parse-char %) (parse-char \0)) (str b))
      (#(let [r (quot (count %) 2)]
          (= (apply + (take r %))
            (apply + (take-last r %))))))))

(defcheck solution-9cebbba0
  (fn [n]
    (let [nums (map (comp parse-int str) (str n))
          c (/ (count nums) 2)]
      (= (reduce + (take c nums)) (reduce + (take-last c nums))))))

(defcheck solution-9d1f510c
  (fn [n]
    (let [
          digits
                  (map
                    #(mod % 10)
                    (take-while #(> % 0) (iterate #(quot % 10) n)))
          halflen (quot (count digits) 2)
          ]
      (=
        (apply + (take halflen digits))
        (apply + (take halflen (reverse digits)))
        )
      )
    ))

(defcheck solution-9d2bc8e6
  (fn [n]
    (let [n ((fn break [n]
               (if (= n 0)
                 nil
                 (cons (mod n 10) (lazy-seq (break (quot n 10)))))) n)
          len (quot (count n) 2)]
      (= (apply + (take len n))
        (apply + (take-last len n))))))

(defcheck solution-9d407844
  (fn [n]
    (let [xs (map #(- (int %) 48) (str n))
          half (int (/ (count xs) 2))
          [xs1 xs2] ((juxt take take-last) half xs)]
      (= (apply + xs1) (apply + xs2)))))

(defcheck solution-9d6e5e47
  (fn [n]
    (let [s (str n)
          i (/ (count s) 2)
          l (take i s)
          r (take i (reverse s))
          sum (partial reduce +)
          mp (zipmap "0123456789" (range 10))]
      (= (sum (map mp l)) (sum (map mp r))))))

(defcheck solution-9df1b89b
  (fn [v]
    (let [z (loop [v v r []]
              (if (< v 10) (cons v r)
                           (recur (quot v 10) (cons (rem v 10) r))))]
      (=
        (apply + (take (int (/ (count z) 2)) z))
        (apply + (drop (/ (count z) 2) z))))))

(defcheck solution-9eb8daed
  #(let [f ((fn d [m]
              (if (zero? m) []
                            (conj (d (quot m 10)) (mod m 10)))) %)
         n (quot (count f) 2)]
     (= (reduce + (take n f))
       (reduce + (take n (reverse f))))))

(defcheck solution-9f3d8f8
  (fn [n]
    (let [digits (fn digits [n]
                   (if (< n 10)
                     [n]
                     (conj (digits (int (/ n 10))) (mod n 10))))
          n-digs (digits n)
          cnt (count n-digs)
          [left right] [(take (int (/ cnt 2)) n-digs) (drop (if (zero? (mod cnt 2)) (int (/ cnt 2)) (inc (int (/ cnt 2)))) n-digs)]
          [left-sum right-sum] (map (partial reduce +) [left right])]
      (= left-sum right-sum))))

(defcheck solution-9fafeca3
  (fn [n]
    (loop [left 0 right 0 ds (str n)]
      (if (< (count ds) 2)
        (= left right)
        (recur (+ left (int (first ds))) (+ right (int (last ds))) (butlast (drop 1 ds)))
        ))))

(defcheck solution-9ff4c9b8
  (fn [n]
    (let [l ((fn d [x]
               (if (< x 10)
                 [x]
                 (cons (mod x 10) (d (int (/ x 10)))))) n)
          s #(apply + (take (/ (count l) 2) %))]
      (= (s l) (s (reverse l))))))

(defcheck solution-a00a2821
  #(let [s (str %) l (count s) m (quot l 2) offset (if (odd? l) 1 0)]
     (= (set (subs s 0 m)) (set (subs s (+ offset m) l)))))

(defcheck solution-a020c3d9
  (fn [n] (letfn
           [(split-even [xs] (let
                              [i (/ (count xs) 2)
                               j (if (= i (int i)) i (dec i))]
                               [(take j xs) (drop i xs)]))
            (sum-sides [ys] (map (fn [y] (reduce + (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) y))) ys))]
            (apply = (sum-sides (split-even (str n)))))))

(defcheck solution-a060fb54
  (fn prob115
    [n]
    (if (< n 10) true
                 (letfn [(digits
                           [num]
                           (map #(character-digit % 10) (str num)))
                         (split-middle [xs]
                           (let [len (count xs)
                                 mid (int (/ len 2))
                                 left (take mid xs)
                                 mid (if (odd? len) (inc mid) mid)
                                 right (drop mid xs)
                                 ]
                             (= (reduce + left) (reduce + right))))
                         ]
                   (split-middle (digits n))))))

(defcheck solution-a0958f56
  (fn nbalanced? [number]
    (letfn [
            (digtz []
              (loop [acc [] num number]
                (if (zero? num)
                  acc
                  (let
                   [lsd (rem num 10)
                    rst (quot num 10)
                    newacc (conj acc lsd)]
                    (recur newacc rst)))))]
      (let
       [vnum (digtz)
        ln (count vnum)
        halfln (quot ln 2)
        leads (take halfln vnum)
        trails (take halfln
                 (reverse vnum))
        leadsum (reduce + leads)
        trailsum (reduce + trails)]
        (= leadsum trailsum) ))))

(defcheck solution-a1045ae3
  (fn balance? [n]
    (let [digit-list (fn digit-list [num]
                       (if (< num 10)
                         [num]
                         (cons (rem num 10)
                           (digit-list (int (/ num 10))))))
          digits (digit-list n)
          half (int (/ (count digits) 2))]
      (= (reduce + (take half digits))
        (reduce + (take half (reverse digits)))))))

(defcheck solution-a12255a3
  (fn balance [n]
    (let [s (str n) c (count s) half (quot c 2)]
      (or (= (seq s) (reverse s)) (= (take half s) (drop (- c half) s))))))

(defcheck solution-a12b83c0
  (fn balanced [d]
    (let [digits (map int (str d))
          size (count digits)
          takenum (quot size 2)
          dropnum (+ takenum (mod size 2))
          l (take takenum digits)
          r (drop dropnum digits)
          sum-l (reduce + l)
          sum-r (reduce + r)]
      (= sum-l sum-r))
    ))

(defcheck solution-a1522837
  (fn [x]
    (let [digits (fn digits [x] (if (= x 0) nil (cons (mod x 10) (digits (quot x 10)))))
          mydigits (digits x)
          n (quot (count mydigits) 2)]
      (= (apply + (take n mydigits))
        (apply + (take n (reverse mydigits)))))))

(defcheck solution-a18713a4
  (fn [number]
    (let [digits (map #(- (int %) 48) (str number))
          half-count (/ (count digits) 2)]
      (=
        (reduce + (take (Math/ceil half-count) digits))
        (reduce + (drop (Math/floor half-count) digits))))))

(defcheck solution-a1ad4e36
  (fn balance-num? [n]
    (let [m (str n)
          cnt (count m)
          digit-sum (fn [cs]
                      (->> cs
                        (map #(-> % int (- 0x30)))
                        (apply +)))]
      (cond
        (= cnt 1) true
        (even? cnt) (->> (split-at (/ cnt 2) m)
                      (map digit-sum)
                      (apply =))
        :default (let [q (quot cnt 2)
                       left (digit-sum (take q m))
                       right (digit-sum (drop (inc q) m))]
                   (= left right))))))

(defcheck solution-a1d052ab
  (fn [x]
    (let [s      (->> x (iterate #(quot % 10)) (take-while (partial < 0)) (map #(rem % 10)))
          n      (count s)
          [l r-] (split-at (quot n 2) s)
          r      (if (zero? (rem n 2)) r- (rest r-))]
      (= (apply + l) (apply + r)))))

(defcheck solution-a200d75
  (fn [num]
    (let [xs (map (fn [d] (-> d str #?(:clj Integer/parseInt :cljs js/parseInt))) (str num))
          f (fn [l] (apply + (take (/ (count xs) 2) l)))]
      (= (f xs) (f (reverse xs))))))

(defcheck solution-a21711d7
  (fn balanced?
    [n]
    (let [n->digits (fn [num] (map #(character-digit % 10) (str num)))
          ns (n->digits n)
          mid (quot (count ns) 2)]
      (->> ns
        (vector (take mid ns) (take-last mid ns))
        (map #(reduce + %))
        (#(= (first %) (second %)))
        ))))

(defcheck solution-a228939e
  (fn [n] (let [
                i (map int (str n))
                a #(apply + (take (quot (count i) 2) %))
                ] ( = (a i) (a (reverse i))))))

(defcheck solution-a266a573
  (fn balanced? [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))
                   (str n))
          half   (quot (count digits) 2)]
      (zero?
        (reduce +
          (take half
            (map - digits (reverse digits))))))))

(defcheck solution-a2681004
  (fn [n]
    (let [s (str n)
          l (count s)
          m (int (/ l 2))
          left (take m s)
          right (if (even? l) (drop m s) (drop (+ m 1) s))
          sum (fn [s] (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str s))))]
      (= (sum left) (sum right)))))

(defcheck solution-a284d42e
  (fn my-balance-of-n
    [val]
    (let [part-one (int (Math/ceil (/ (count (str val)) 2)))
          part-two (int (Math/floor (/ (count (str val)) 2)))
          parts (partition part-one part-two (str val))
          to-digits (fn [cl] (map #(character-digit % 10) cl))]
      (= (reduce + (to-digits (first parts))) (reduce + (to-digits (second parts)))))))

(defcheck solution-a2bd5e27
  (letfn [(d [x] (when (pos? x) (cons (mod x 10) (d (int (/ x 10))))))]
    #(let [x (d %)
           c (/ (count x) 2)
           l (reduce + (take (int c) x))
           r (reduce + (drop (Math/ceil c) x))]
       (= l r))))

(defcheck solution-a3225f27
  (fn [s]
    (let [s (str s)
          c (count s)
          m (quot c 2)
          r #(reduce + 0 (map (fn [x] (- (int x) 48)) %))]
      (=
        (r (subs s 0 m))
        (r (subs s (if (even? c) m (inc m))))))))

(defcheck solution-a364a7a4
  (fn mytest [n]

    (let [digit-seq (map #(character-digit % 10) (seq (str n)))

          cnt (count digit-seq)

          take-num (quot cnt 2)

          drop-num (if (= 0 (rem cnt 2)) take-num (inc take-num))

          ]

      ;(println take-num)
      ;(println drop-num)
      ;(println (take take-num digit-seq))
      ;(println (drop drop-num digit-seq))

      (if (= cnt 1)
        true
        (= (reduce + (take take-num digit-seq))
          (reduce + (drop drop-num digit-seq))
          )
        )
      )

    ))

(defcheck solution-a3e4c77c
  (fn balancedN [b] (let [
                          intoDigits (fn [x] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str x))))
                          splitN (fn [s] (+ (int (/ (count s) 2)) (rem (count s) 2)))
                          numSeq (intoDigits b)
                          splitVal (splitN numSeq)
                          ] (= (reduce + (take  splitVal numSeq)) (reduce + (take-last splitVal numSeq))))))

(defcheck solution-a3e54773
  (fn [n]
    (letfn [(digits [x]
              (loop [xi x
                     ds []]
                (if (zero? xi)
                  ds
                  (recur (quot xi 10)
                    (conj ds (rem xi 10))))))]
      (let [ds (digits n)
            ct (count ds)
            dc (if (odd? ct)
                 (quot (dec ct) 2)
                 (quot ct 2))
            tc (if (odd? ct)
                 (quot (inc ct) 2)
                 (quot ct 2))]
        (= (reduce + (take tc ds))
          (reduce + (drop dc ds)))))))

(defcheck solution-a447634a
  (fn [m]
    (let [l (map #(mod % 10) (take-while #(not (= % 0)) (iterate #(int (/ % 10)) m)))
          n (/ (count l) 2)
          f (take (int (+ 0.5 n)) l)
          s (drop (int n) l)]
      (= (apply + f) (apply + s))
      )))

(defcheck solution-a447856b
  (fn [n]
    (letfn
     [(balanced? [coll left right]
        (if
         (> 2 (count coll))
          (= left right)
          (balanced?
            (reverse (rest (reverse (rest coll))))
            (+ left (first coll))
            (+ right (last coll)))))
      (digits [n]
        ((fn helper [n coll]
           (if
            (zero? n)
             coll
             (helper
               (quot n 10)
               (cons (mod n 10) coll))))
         n []))]
      (balanced? (digits n) 0 0))))

(defcheck solution-a5b06d93
  #(let [c (map int (str %))]
     (= 0 (apply + (apply (partial map -)
                     (split-at (/ (count c) 2) c))))))

(defcheck solution-a6c904f7
  (fn [x]
    (let [n (map (comp parse-int str) (seq (str x))) d (/ (count n) 2)
          f #(apply + (take d %))]
      (= (f n) (f (reverse n))))))

(defcheck solution-a6f74f4d
  (fn [n]
    (let [int-string (str n)
          half-num-digits (quot (count int-string) 2)
          sum #(apply + (map int (% half-num-digits int-string)))]
      (= (sum take) (sum take-last)))))

(defcheck solution-a70bc3ee
  (letfn
   [ (digits [n] (map #(parse-int %) (re-seq #"\d" (str n))))
    (hds [d] (reduce + (take (/ (count d) 2) d)))]
    #(=
       (hds (digits %))
       (hds (reverse (digits %))))))

(defcheck solution-a7e76a0e
  (fn [n]
    (let [st (str n)
          len (quot (count st) 2)
          sum-of-str (fn [n-str] (reduce + (map #(- (int %) 48) n-str)))]
      (= (sum-of-str (take len st)) (sum-of-str (take-last len st))))))

(defcheck solution-a82924ab
  (fn [d]
    (let [n (str d)
          i (quot (count n) 2)
          f ( fn [n] (reduce + (map #(- (int %) 48) n)))]
      (= (f(take i n)) (f(take i (reverse n)))))))

(defcheck solution-a8548908
  (fn [n]
    (let [s (str n)
          len (count s)
          half (quot len 2)
          step (if (> (mod len 2) 0) (inc half) half)
          [p1 p2] (partition half step s)
          n1 (map #(- (parse-char %) (parse-char \0)) p1)
          n2 (map #(- (parse-char %) (parse-char \0)) p2)]
      (= (apply + n1) (apply + n2)))))

(defcheck solution-a882d381
  (fn [n]
    (let [digits (map (comp parse-int str) (str n))
          len (count digits)
          half (quot len 2)
          middle (rem len 2)]
      (if (< len 2)
        true
        (let [lhs-rhs (partition half (+ half middle) digits)]
          (= (reduce + (first lhs-rhs)) (reduce + (second lhs-rhs)))
          )))))

(defcheck solution-a94cc91f
  (fn balanced [x]
    (let [y (str x) c (count y) left (take (int (/ c 2)) y) right (drop (int (/ (inc c) 2)) y) ]
      (= (apply + (map int left))
        (apply + (map int right))
        )
      )
    ))

(defcheck solution-a967b91d
  (fn [j] (let [s (str j)
                l (count s)
                h (int (/ l 2))
                f #(reduce (fn [a b] (+ a (parse-int (str b)))) 0 %)]
            (= (f (subs s 0 h))
              (f (subs s (- l h)))))))

(defcheck solution-a9916d0
  (fn [n]
    (let [digit-str (str n)
          digit-cnt (count digit-str)
          mid       (int (/ digit-cnt 2))
          halves    [(take mid digit-str) (take-last mid digit-str)]]
      (->> halves
        (map #(map (comp parse-int str) %))
        (map (partial apply +))
        (apply =)))))

(defcheck solution-aa11c9ee
  (fn [n]
    (letfn [(digits [n] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n)))]
      (let [dgts (digits n)
            half (/ (count dgts) 2)
            left (if (odd? (count dgts)) (take (dec half) dgts) (take half dgts))
            right (drop half dgts)]
        (= (apply + left) (apply + right))))))

(defcheck solution-aa2a63ef
  (fn
    [x]
    (let [a (map #(- (parse-char %) (parse-char \0)) (str x))
          b (reverse a)
          h (quot (count a) 2)]
      (= (apply + (take h a)) (apply + (take h b))))))

(defcheck solution-aa2e8fd6
  (fn [x]
    (let [digits (map int (str x))
          n (count digits)]
      (= (apply + (take (Math/ceil (/ n 2)) digits)) (apply + (drop (Math/floor (/ n 2)) digits))))))

(defcheck solution-ab1070e7
  (fn [n]
    (let [digits (->> n
                   str
                   seq
                   (map (comp int parse-int str)))
          sum-of-squares (apply + (map #(* % %) digits))
          half (int (Math/floor (float (/ (count digits) 2))))]
      (= (apply + (take half digits)) (apply + (take half (reverse digits)))))))

(defcheck solution-abac368b
  (fn [n]
    (let [sum (fn [digits] (apply + (map #(parse-char %) digits)))
          digits (str n)
          [left right] (split-at (int (/ (count digits) 2)) digits)
          adjusted-right (drop (- (count right) (count left)) right)]
      (= (sum left) (sum adjusted-right)))))

(defcheck solution-abc23c03
  (fn [n]
    (let [s (seq (str n))
          l (count s)
          ]
      (if (= l 1)
        true
        (let [m (quot l 2)
              front (take m s)
              back (take m (reverse s))
              ;; bt and ft produce incorrect results but okay for comparison
              ft (reduce #(+ %1 (int %2)) 0 front)
              bt (reduce #(+ %1 (int %2)) 0 back)
              ]
          (= ft bt) )))
    ))

(defcheck solution-abe981c7
  (fn [x] (let [	col (loop [d x result []] (if (zero? d) result (recur (quot d 10) (conj result (rem d 10)))))
                n (quot (count col) 2)
                left (take n col)
                right (take-last n col)] (= (apply + left) (apply + right)))))

(defcheck solution-abf85c2b
  (fn [n]
    (let [n (map #(character-digit % 10) (str n))
          i (quot (count n) 2)]
      (= (apply + (take i n))
        (apply + (take i (reverse n)))))))

(defcheck solution-ac3d2479
  (fn [i]
    (let [l ((fn _ [v] (if (= v 0) [] (conj (_ (quot v 10)) (rem v 10)))) i), c (quot (+ 1 (count l)) 2)]
      (= (apply + (take c l)) (apply + (take-last c l)))
      )
    ))

(defcheck solution-acac33cf
  (fn [n]
    (let [digits (loop [acc () n n] (if (< n 10) (conj acc n) (recur (conj acc (mod n 10)) (quot n 10))))
          len (quot (count digits) 2)
          left (reduce + (take len digits))
          right (reduce + (take-last len digits))]
      (= left right))))

(defcheck solution-ad43170e
  (fn [x]
    (let [s (seq (str x))
          half-c (/ (count s) 2)]
      (= (apply + (map #(-> % str parse-int) (take (- half-c 0.5) s)))
        (apply + (map #(-> % str parse-int) (drop half-c s)))))))

(defcheck solution-ad7769d5
  (fn [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          size (int (/ (count digits) 2))
          f (take size digits)
          l (take-last size digits)]
      (= (reduce + f) (reduce + l)))))

(defcheck solution-add8aaee
  (fn isba [n]
    (let [s (map #(- (parse-char %) (parse-char \0)) (str n))
          hl (quot (count s) 2)
          l (take hl s)
          r (take hl (reverse s))]
      (= (reduce + l) (reduce + r)))))

(defcheck solution-addf3b9a
  (fn [x]
    (->> x
      (str)
      (seq)
      (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)))
      ((fn [x] (map - x (reverse x))))
      ((fn [x] (take (/ (count x) 2) x)))
      (reduce + 0)
      (= 0)
      )
    ))

(defcheck solution-ade1139d
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          l (count digits)
          a (quot l 2)]
      (=
        (apply + (take a digits))
        (apply + (drop (- l a) digits))))))

(defcheck solution-ae32e342
  (fn [n]
    (let [s (str n)
          size (-> (count s) (/ 2) (Math/floor))
          head (take size s)
          tail (take-last size s)
          sum (fn [cs] (reduce + (map #(parse-char %) cs)))]
      (= (sum head) (sum tail)))))

(defcheck solution-ae38fa38
  #(let [d (map int (str %))
         s (quot (count d) 2)]
     (= (apply + (drop s d))
       (apply + (drop-last s d)))))

(defcheck solution-ae3f83e0
  (fn [n]
    (->> (str n)
      (mapv (comp parse-int str))
      ((juxt seq rseq))
      (map #(reduce + (take (-> n str count (/ 2) int) %)))
      (apply =))))

(defcheck solution-ae4e032e
  (fn [x]
    (let [digits (->> x str (map #(character-digit % 10)))
          med (quot (count digits) 2)]
      (= (reduce + (take med digits)) (reduce + (take-last med digits))))))

(defcheck solution-af3cb823
  #(let [s (str %)
         n (/ (count s) 2)]
     (apply =
       (for [p ((juxt take take-last) n s)]
         (apply + (map int p))))))

(defcheck solution-af6edaff
  (fn [x]
    (let [lst (map #(parse-int (.toString %)) (.toString x))
          len (quot (count lst) 2)]
      (= (apply + (drop len lst))
        (apply + (drop-last len lst))))))

(defcheck solution-afd78da9
  (fn [x]
    (let [xstr (map #(character-digit % 10) (seq (str x)))
          n (count xstr)
          idxt (int (Math/ceil (/ n 2)))
          idxd (int (Math/floor (/ n 2)))
          ]
      (= (apply + (take idxt xstr)) (apply + (drop idxd xstr)))
      )))

(defcheck solution-b0c2ec26
  (fn balance? [n]
    (let [d (map #(- (int %) 48) (str n))
          half (quot (count d) 2)]
      (= (reduce + (take half d))
        (reduce + (take half (reverse d)))))))

(defcheck solution-b16239fd
  (fn __ [i]
    (let [[l r] (split-at (/ (count (str i)) 2) (str i))]
      (letfn [(sum [s] (reduce + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) s)))]
        (if (odd? (count (str i)))
          (= (sum (butlast l)) (sum r))
          (= (sum l) (sum r)))))))

(defcheck solution-b1810784
  (fn [x]
    (let [s (map #(- (int %) 48) (str x))
          c (count s)
          n (quot c 2)
          ]
      (= (apply + (take n s))
        (apply + (drop (- c n) s))))))

(defcheck solution-b24f5f81
  ;;; (__ 1982) must be 'true' ?
  #(let [s (str %)
         f (fn [s] (sort (take (/ (count s) 2) s)))]
     (= (f s) (f (reverse s)))))

(defcheck solution-b297aab8
  (fn [n]
    (let [digit #(parse-char %)
          digits (->> (str n) (map digit))
          rdigits (reverse digits)
          halfdigits #(-> (count %) (/ 2) (take %))
          halfsum #(apply + (halfdigits %))]
      (= (halfsum digits)
        (halfsum rdigits)))))

(defcheck solution-b2b9d9c0
  (fn [n]
    (let [c (map int (str n))
          h (int (/ (count c) 2))]
      (if (= h 0)
        true
        (= (reduce + 0 (take h c))
          (reduce + 0 (take-last h c)))))))

(defcheck solution-b3b3c8be
  (fn balanced? [n]
    (let [seq-num (loop [res [] rst n]
                    (if (zero? rst)
                      res
                      (recur (conj res (mod rst 10)) (quot rst 10))))
          split-num (split-at (quot (count seq-num) 2) seq-num)]

      (if (not= (count (first split-num)) (count (second split-num)))
        (= (reduce + (first split-num)) (reduce + (rest (second split-num))))
        (= (reduce + (first split-num)) (reduce +  (second split-num))))
      )))

(defcheck solution-b3e562ed
  (fn [x]
    (let [v (vec (str x))
          c (/ (count v) 2)]
      (= (set (take-last c v)) (set (take c v))))))

(defcheck solution-b43ba3f7
  (fn [n]
    (let [s (str n) l (int (/ (count s) 2))]
      (letfn [(ssn [sn] (apply + (map #(- (parse-char %) (parse-char \0)) (take l sn))))]
        (= (ssn s) (ssn (reverse s)))))))

(defcheck solution-b4525b07
  (fn [n]
    (let [c (count (str n))]
      (or (= c 1)
          (letfn [(sum [n] (if (zero? n) 0 (+ (rem n 10) (sum (quot n 10)))))
                  (pow10 [n] (if (zero? n) 1 (* 10 (pow10 (dec n)))))]
            (= (sum (rem n (pow10 (quot c 2)))) (sum (quot n (pow10 (- c (quot c 2))))))
            )
          )
      )
    ))

(defcheck solution-b472a6ec
  (fn sim [x] (let [duono (quot (count (str x)) 2)
                    listo (map int  (str x) ) ]
                (= (apply + (take duono listo)) (apply + (take duono (reverse listo)))))))

(defcheck solution-b47561ad
  (fn [x] (let [digits (map #(-> % str parse-int) (into [] (str x)))
                half-length (-> digits count (/ 2) Math/floor)
                first-half (take half-length digits)
                second-half (take-last half-length digits)]
            (= (reduce + first-half)
              (reduce + second-half)))))

(defcheck solution-b4b9a3b
  (fn bal? [n]
    (->> (str n)
      ((fn [s] (let [half (quot (count s) 2)]
                 [(take half s) (drop (- (count s) half) s)])))
      (map sort)
      (apply =))))

(defcheck solution-b53e091f
  (fn [n]
    (let [c (seq (str n))
          i (count c)
          h (int (Math/ceil (/ i 2.0)))
          f (fn [s] (apply + (map #(parse-int (str %)) s)))
          l (f (take h c))
          r (f (drop (- i h) c))]
      (= l r))))

(defcheck solution-b5521a4c
  (fn[n](=
          (reduce + (map int (take (int (/ (count (str n)) 2)) (seq (str n)))))
          (reduce + (map int (take-last (int (/ (count (str n)) 2)) (seq (str n)))))
          )))

(defcheck solution-b56035f
  (fn [n]
    (let [d (map #(parse-int (str %))
              (str n))
          c (count d)
          h (int (Math/ceil (/ c 2)))
          l (take h d)
          r (drop (- c h) d)]
      (= (reduce + l)
        (reduce + r)))))

(defcheck solution-b56a2888
  (fn balanced? [n]
    (letfn [(digits0 [x b]
              (loop [x x, d '()]
                (if (zero? x) (cons 0 d)
                              (recur (int (/ x b))
                                (cons (mod x b) d)))))
            (digits [x b]
              (let [d (digits0 x b)]
                (if (= d '(0)) d (rest d))))]
      (let [d (vec (digits n 10))]
        (= (reduce + (subvec d 0 (int (/ (count d) 2))))
          (reduce + (subvec d (int (+ 0.5 (/ (count d) 2))))))))))

(defcheck solution-b56c7af3
  (fn [n]
    (let [digs (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          l (count digs)
          half (quot l 2)
          front (take half digs)
          back (take half (reverse digs))]
      (= (reduce + front) (reduce + back)))))

(defcheck solution-b5b8023
  (letfn [(digits [n acc]
            (if (zero? n) acc
                          (recur (quot n 10) (conj acc (mod n 10)))))]
    (fn [n]
      (let [ds (digits n (list))
            prefix-length (quot (count ds) 2)]
        (= (apply + (take prefix-length ds))
          (apply + (take prefix-length (reverse ds))))))))

(defcheck solution-b5eb21eb
  (fn [n]
    (letfn [(rev-dig-vec [n]
              (if (= n 0)
                []
                (cons (mod n 10)
                  (rev-dig-vec (/ (- n (mod n 10)) 10)))))
            (dig-vec [n] ((comp reverse rev-dig-vec) n))
            (sums [v]
              (let [l (count v)
                    half (int (Math/ceil (/ l 2)))
                    sum (fn [v] (reduce + v))
                    left (sum (take half v))
                    right (sum (take half (reverse v)))]
                [left right]))]
      (let [r (sums (dig-vec n))]
        (= (first r) (second r))))))

(defcheck solution-b5fbb3c1
  (fn [x]
    (let [s (str x), l (count s)]
      (= (reduce + (map int (take (Math/ceil  (/ l 2)) s)))
        (reduce + (map int (drop (Math/floor (/ l 2)) s)))))))

(defcheck solution-b62c325d
  #(let [d (->> % str (map (comp parse-int str)))
         n (/ (count d) 2)]
     (=
       (reduce + (take n d))
       (reduce + (nthrest d (int n))))))

(defcheck solution-b665b176
  (fn  [n]
    (let [c (map #(- (int %) 48) (str n))
          i (/ (count c) 2)
          f #(apply + (% i c))]
      (= (f take) (f take-last)))))

(defcheck solution-b681e67d
  (fn bal? [x]
    (let [x' (str x)
          n (quot (count x') 2)
          l (take n x')
          r (take n (reverse x'))
          sum (fn [s] (apply + (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) s)))]
      (= (sum l) (sum r)))))

(defcheck solution-b6a37834
  #(let [d (map int (str %))
         d (map - d (reverse d))
         d (take (quot (count d) 2) d)]
     (= 0 (apply + d))))

(defcheck solution-b6fa1506
  (fn mybal [n]
    (let [ns (map #(- (int %) 48) (str n))
          half (quot (count ns) 2)
          h1 (take half ns)
          h2 (take-last half ns )]
      (= (apply + h1) (apply + h2)))))

(defcheck solution-b7290ef1
  #(let [v (map int (str %))
         n (quot (count v) 2)]
     (= (apply + (take n v))
       (apply + (take-last n v)))))

(defcheck solution-b7aadbcc
  (fn [n]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str n))
          c (quot (count digits) 2)
          [a b] [(take c digits) (if (even? (count digits))
                                   (drop c digits)
                                   (drop (inc c) digits))]]
      (= (apply + a) (apply + b)))))

(defcheck solution-b8207209
  (fn balanced? [n]
    (let [digits (map (comp parse-int str) (seq (str n)))
          partitions (partition (int (/ (count digits) 2)) 1 digits)
          half1 (first partitions) half2 (last partitions)]
      (= (reduce + half1) (reduce + half2))
      )
    ))

(defcheck solution-b8713c9
  (fn n115 [n]
    (letfn [(dig-10 [n]
              ((fn [n d]
                 (loop [r n dig (if (zero? n) [0] [])]
                   (if (zero? r) (reverse dig) (recur (int (/ r d)) (conj dig (mod r d)))))) n 10))]
      (let [l (dig-10 n)
            [f b] (map vec (split-at (/ (count l) 2) l))
            f (if (odd? (count l)) (pop f) f)]
        (= (sort f) (sort b))))))

(defcheck solution-b8956da6
  (fn [n]
    (let [s (str n)
          l2 (quot (count s) 2)
          digits (map #(- (int %) 48) s)]
      (= (apply + (take l2 digits))
        (apply + (take-last l2 digits))))))

(defcheck solution-b8c56aca
  (fn balanced-n? [n]
    (let* [s (str n) c (quot (count s) 2)]
      (letfn [(d [x] (reduce + (map #(- (int %) 48) x)))]
        (= (d (take c s)) (d (take-last c s)))))))

(defcheck solution-b8cf9067
  (fn [n] (let [s (str n) l (count s) m (int (/ l 2))]
            (= (apply + (map int (subs s 0 m)))
              (apply + (map int (subs s (- l m))))))))

(defcheck solution-b8eaf0cc
  #(let [n (str %) i (int (/ (count n) 2)) c2i (fn [c] (- (parse-char c) (parse-char \0)))]
     (= (->> n (take i) (map c2i) (apply +))
       (->> n reverse (take i) (map c2i) (apply +)))))

(defcheck solution-b9abe198
  (fn bal? [n]
    (let [diglis (->> n str seq (map str) (map parse-int))
          len (count diglis)
          halflen (int (/ len 2))]
      (= (reduce + (take halflen diglis))
        (reduce + (drop (- len halflen) diglis))))))

(defcheck solution-b9ba1bb6
  (fn [n]
    (letfn
     [(digits [n] (if (zero? n) '(0) (digits-finish n ())))
      (digits-finish [n digits]
        (if (> n 0)
          (recur (int (/ n 10)) (conj digits (mod n 10)))
          digits))]
      (let [digits (digits n)
            count (count digits)
            half (int (/ count 2))
            left-half (take half digits)
            right-half (take half (reverse digits))]
        (= (apply + left-half)
          (apply + right-half))))))

(defcheck solution-b9f24aa1
  (fn [ n ]
    (let [ digits (->> n str seq (map str))
          len (count digits)
          half (+ (quot len 2) (rem len 2))
          back-drop (if (even? len) half (dec half))
          l (map #(parse-int %) (take half digits))
          r (map #(parse-int %) (drop back-drop digits))]
      (= (reduce + l) (reduce + r)))))

(defcheck solution-bb77da6c
  (fn [x]
    (let [s (map int (str x))
          m #(apply + (take (quot (count s) 2) %))]
      (= (m s)
        (m (reverse s))))))

(defcheck solution-bb94426d
  (fn balanced? [n]
    (let [vals (map int (str n))
          excess (/ (count vals) 2)
          l (apply + (drop-last excess vals))
          r (apply + (drop excess vals))]
      (= l r))))

(defcheck solution-bbd0b74b
  (fn balanced? [n]
    (let [digs ((fn digs [n]
                  (if (< n 10) [n]
                               (conj (digs (int (/ n 10))) (mod n 10))))
                n)
          mid (/ (count digs) 2)]
      (apply =
        (map (fn [as] (apply + as))
          [(take (Math/floor  mid) digs)
           (drop (Math/ceil  mid) digs)])))))

(defcheck solution-bbd10d9
  (fn [x]
    (let [string (str x)
          length (quot (count string) 2)
          front (subs string 0 length)
          extra (if (even? (count string)) 0 1)
          back (subs string (+ extra length))]
      (->> [front back]
        (map (fn [x] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) x)))
        (map #(apply + %))
        (apply =)))))

(defcheck solution-bc74a6d6
  (fn [n]
    (let [s (seq (str n))
          l (quot (count s) 2)]
      (->> [s (reverse s)]
        (map #(take l %))
        (map (fn [s] (map #(character-digit % 10) s)))
        (map #(apply + %))
        (apply =)))))

(defcheck solution-bd81957c
  #(let [s (map (fn [d] (parse-int (str d))) (str %))
         half (quot (count s) 2)]
     (= (apply + (take half s)) (apply + (take-last half s)))))

(defcheck solution-bd8b28c9
  (fn [n]
    (let [s (str n)
          s1 (take (/(count s)2) s)
          s2 (take-last (/(count s)2) s)
          d1 (map (comp parse-int str) s1)
          d2 (map (comp parse-int str) s2)]
      (= (reduce + d2) (reduce + d1)))))

(defcheck solution-bdca5959
  (fn [n]
    (let [digits (fn [n] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n)))
          splitseq (fn [s]
                     (let [f (Math/floor (/ (count s) 2))
                           l (Math/ceil (/ (count s) 2))]
                       [(take f s) (drop l s)]))]
      #_(println (splitseq (digits n)))
      (apply = (map #(reduce + %) (splitseq (digits n)))))))

(defcheck solution-bdddd1e7
  (fn f[i]
    (let [
          s  (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str i))
          c ( quot (count s) 2)
          v (vector (drop c s) (drop c (reverse s)))
          ]
      (apply = (map #(reduce + %) v) ))
    ))

(defcheck solution-be57c348
  (fn balanced? [x]
    (let [digits (map #(character-digit % 10) (str x))
          h      (quot (count digits) 2)
          l      (take h digits)
          r      (take h (reverse digits))
          sum     #(reduce + %) ]
      (= (sum l) (sum r)))))

(defcheck solution-be8ad1cf
  (fn [n]
    (let [[lf rf] (first (for [lf (iterate #(* 10 %) 1)
                               rf [lf (* 10 lf)]
                               :when (zero? (quot n (* lf rf)))]
                           [lf rf]))
          left (quot n lf)
          right (mod n rf)
          digit-sum (fn s
                      ([n] (s 0 n))
                      ([c n]
                       (if (< n 10)
                         (+ c n)
                         (recur (+ c (mod n 10)) (quot n 10)))))]
      (= (digit-sum left)
        (digit-sum right)))))

(defcheck solution-be9ed887
  (fn [n]
    (loop [l 0, r 0, s (str n)]
      (if (> 2 (count s))
        (= l r)
        (recur (+ l (int (first s)))
          (+ r (int (last s)))
          (rest (butlast s)))))))

(defcheck solution-bebf4cc
  (fn [n]
    (let [s (str n)
          p #(+ %1 (int %2))
          r #(reduce p 0 (% (quot (count s) 2) s))]
      (= (r take)
        (r take-last)))))

(defcheck solution-bed622a4
  (fn [n]
    (let [digits (fn[k]  (reverse (map #(mod % 10)(take-while pos? (iterate #(quot % 10) k)))))
          dig (digits n)
          size (count dig)
          strl (quot size 2)
          fst (take strl dig)
          snd (take-last strl dig)]
      (= (reduce + fst) (reduce + snd)))

    ))

(defcheck solution-bee85a3c
  (fn bn [n]
    (let [s (map #(- (int %) 48) (str n))
          h (quot (inc (count s)) 2)
          sumdig #(reduce + (take h %))]
      (= (sumdig s) (sumdig (reverse s))))))

(defcheck solution-bef1576e
  (fn [i]
    (let [x (str i)
          s #(apply + (take (int (/ (count %) 2))
                        (map (comp parse-int str)
                          %)))]
      (= (s x)
        (s (reverse x))))))

(defcheck solution-bf0dc2f
  #(let[numStr (str %)
        digiLen (count numStr)
        halfLen (/ digiLen 2)
        lower (subs numStr 0 halfLen)
        upper (subs numStr (if (integer? halfLen) halfLen (inc halfLen))  digiLen)
        sum (fn[s] (reduce + (map (fn[c] (- (int c) 48)) (seq s))))]
     (= (sum lower) (sum upper))))

(defcheck solution-bf26319f
  (fn [n]
    (letfn [(digs[num]
              (loop [num num res []]
                (if (< 0 num)
                  (recur (int (/ num 10)) (cons (mod num 10) res))
                  res)))]
      (let [ds (digs n)
            ha (/ (count ds) 2)
            ld (take ha ds)
            rd (drop (Math/floor ha) ds)]
        (apply = (map #(reduce + %) [ld rd]))))))

(defcheck solution-bf48ac90
  (fn balance [n]
    (let [s (seq (str n)) ]
      (if (= (count s) 1)
        true
        (let [s-p (split-at (quot (count s) 2) s)
              left (first s-p)
              right (second s-p)
              rf (fn [acc x] (+ acc (#?(:clj Integer/parseInt :cljs js/parseInt) (str x))))
              left-sum (reduce rf 0 left)
              right-sum (reduce rf 0 (if (odd? (count right) ) right (rest right)))]
          (if (= left-sum right-sum ) true false))))))

(defcheck solution-bf65ed05
  (fn [a]
    (let [s ((partial map #(- (int %) 48)) (str a))
          n (int (/ (count s) 2))]
      (= (reduce + (take n s)) (reduce + (take n (reverse s)))))))

(defcheck solution-c033cb7e
  (fn balanced? [n]
    (let [numbers (map parse-int (re-seq #"\d" (str n)))
          mid (/ (count numbers) 2)]
      (= (apply + (drop (Math/ceil mid) numbers))
        (apply + (take (Math/floor mid) numbers))))))

(defcheck solution-c05415ae
  (fn p115[x]
    (let [s (str x)
          f (fn [si] (reduce #(+ %1 (int %2)) 0 (take (quot (count si) 2) si)))]
      (= (f s)
        (f (reverse s))))))

(defcheck solution-c119bc60
  (fn [n]
    (let [s (str n)
          ctoi #(- (int %) 48)
          f #(->>
               %
               (take (Math/ceil (/ (count s) 2)))
               (map ctoi)
               (reduce +))]
      (= (f s) (f (reverse s))))))

(defcheck solution-c187f1ce
  (fn [n]
    (let [
          ds (map #(- (parse-char %) (parse-char \0)) (str n))
          half (/ (count ds) 2)
          left (take half ds)
          right (take-last half ds)]
      (= (reduce + left) (reduce + right)))))

(defcheck solution-c26cd2d8
  #(let [s (map (fn [x] (- (int x) 48)) (str %))
         p (/ (count s) 2)
         left (take p s)
         right (take-last p s)]
     (= (apply + left) (apply + right))))

(defcheck solution-c2d70a9
  (fn [x]
    (let [text (str x)
          len (quot (count text) 2)
          step (if (zero? (rem (count text) 2)) len (inc len))
          split (partition len step text)]
      (apply = (map (partial reduce +) (map (partial map int) split))))))

(defcheck solution-c2eaf5df
  (fn [n] (let [l1 (map-indexed vector (str n)) p (count l1) m (if (even? p) (/ p 2) (dec (/ p 2))) s (if (even? p) m (inc m))] (if (= 1 p) true  (= (reduce (fn [acc [i v]] (if (< i m) (conj acc  v) acc)) #{} l1) (into #{} (drop s (str n))))))))

(defcheck solution-c30c926c
  (fn [n]
    (let [s (map #(- (int %) 48) (str n))
          l (/ (count s) 2)
          [a b] (map #(apply + (take l %)) [s (into () s)])]
      (= a b))))

(defcheck solution-c3588a9d
  #(let [s (map int (str %))
         c (quot (count s) 2)]
     (= (apply + (take c s))
       (apply + (take-last c s)))))

(defcheck solution-c3a152aa
  (fn
    [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))]
      (let [half (int (/ (count digits) 2))
            left (take half digits)
            right (take-last half digits)]
        (= (reduce + left)
          (reduce + right))))))

(defcheck solution-c402e59d
  (fn [x]
    (let [ds (mapv #(-> % str parse-int) (print-str (str x)))
          n (quot (count ds) 2)]
      (apply =
        (map #(apply + %)
          [(take n ds)
           (take-last n ds)])))))

(defcheck solution-c45c4814
  (fn [n]
    (let [digits #(loop [n % r []] (if (< n 10) (conj r n) (let [m (mod n 10)] (recur (/ (- n m) 10) (conj r m)))))
          nums (digits n)
          len (count nums)]
      (= (apply + (subvec nums 0 (int (/ len 2))))
        (apply + (subvec nums (int (/ (inc len) 2))))))))

(defcheck solution-c491a667
  (fn [x]
    (let [ds (->> (iterate #(* 10 %) 1)
               (map #(quot x %))
               (take-while #(> % 0))
               (map #(mod % 10)))
          n (count ds)
          [l r] (split-at (quot n 2) ds)
          r' (drop (mod n 2) r)]
      (== (apply + l) (apply + r')))))

(defcheck solution-c49901cd
  (fn [n]
    (->> (map #(parse-int (str %)) (str n))
      ((juxt identity reverse))
      (map #(apply + (take (/ (count %) 2) %)))
      (apply =))))

(defcheck solution-c521b540
  (fn qq [x]
    (let [n (int (/ (count (str x)) 2))]
      (if (=
            (reduce #(+ (parse-int (str (nth (str x) %2))) %1) 0
              (range (dec (count (str x))) (- (count (str x)) n 1) -1))

            (reduce #(+ (parse-int (str (nth (str x) %2))) %1) 0 (range n))
            )

        true
        false
        ))
    ))

(defcheck solution-c552e2cc
  (fn [n]
    (let [digits (remove empty? (clojure.string/split (str n) #""))
          midpoint (quot (count digits) 2)
          digit-sum (fn [ds] (apply + (map #(parse-int %) ds)))]
      (->> [(take midpoint digits) (take-last midpoint digits)]
        (map digit-sum)
        (apply =)))))

(defcheck solution-c58db4cd
  (fn [x]
    (let [digits (map #(parse-char %) (str x))
          sum #(reduce + %)
          half (Math/floor (/ (count digits) 2))
          left (take half digits)
          right (take-last half digits)]
      (= (sum left) (sum right)))))

(defcheck solution-c5ae328
  (fn [n]
    (->> n
      str
      (re-seq #".")
      (map parse-int)
      ( (juxt identity reverse) )
      (map #(take (quot (count %) 2) %))
      (map #(reduce + %))
      (apply =))))

(defcheck solution-c5bbf484
  (fn [n]
    (let [s (str n)]
      (let [half-len (int (/ (count s) 2))]
        (letfn [(sum [chars] (reduce + (map #(-> % str parse-int) chars)))]
          (= (sum (take half-len s))
            (sum (take half-len (reverse s)))))))))

(defcheck solution-c5fc54b9
  (fn [n] (let [S (map int (str n)) h (int (/ (count S) 2))] (= (reduce + (take h S)) (reduce + (take-last h S))))))

(defcheck solution-c657949f
  (fn balanced? [n]
    (letfn [(digits [n]
              (loop [c n res ()]
                (if (< c 10)
                  (conj res c)
                  (recur (quot c 10) (conj res (rem c 10))))))]
      (->> (digits n)
        ((juxt identity reverse))
        (map #(take (quot (count %) 2) %))
        (map (partial reduce +))
        (apply =)))))

(defcheck solution-c6be6c36
  (fn __ [n]
    (let [l (->> (str n)
              (map #(character-digit % 10)))
          m (quot (count l) 2)]
      (= (reduce + (take m l)) (reduce + (take-last m l)))
      )
    ))

(defcheck solution-c6f65ec0
  (fn [x]
    (let [chars (str x)
          half (quot (count chars) 2)
          chars-to-nums (fn [xs] (map #(parse-char %) xs))]
      (=
        (apply + (chars-to-nums (take half chars)))
        (apply + (chars-to-nums (take-last half chars)))))))

(defcheck solution-c7aa7f48
  (fn [n]
    (let [digits (->> n str (map str) (map parse-int))
          len (count digits)
          half (quot len 2)
          fst (take half digits)
          lst (drop (- len half) digits)]
      (= (apply + fst) (apply + lst)))))

(defcheck solution-c7d71e08
  (fn [n]
    (let [sn (str n)
          l (count sn)
          front (take (quot l 2) sn)
          back (take-last (quot l 2) sn)
          frontsum (reduce + 0 (for [d front]
                                 (- (parse-char d) (parse-char \0))))
          backsum (reduce + 0 (for [d back]
                                (- (parse-char d) (parse-char \0))))]
      (= frontsum backsum))))

(defcheck solution-c807c63d
  (fn [num]
    (let [sn (str num)
          len (quot (count sn) 2)
          left (take len sn)
          right (take-last len sn)]
      (= (into #{} left) (into #{} right)))))

(defcheck solution-c809d579
  (fn balance

    ([x]

     (balance (map parse-int (re-seq #"\d" (str x))) (map parse-int (reverse (re-seq #"\d" (str x)))) (count (re-seq #"\d" (str x)))))

    ([x y z]

     (if (= z 2)

       (if (= (first x) (last x))

         true

         false)

       (if (= (reduce + (take (long (Math/ceil (/ z 2))) x))

             (reduce + (take (long (Math/ceil (/ z 2))) y)))

         true

         false)))))

(defcheck solution-c8232cf6
  (fn [n]
    (letfn [(split-digit [x]
              (loop [digit-seq '()
                     i x]
                (if (< i 10)
                  (conj digit-seq i)
                  (recur (conj digit-seq
                           (mod i 10))
                    (int (/ i 10))))))]
      (let [digit-seq (split-digit n)
            len (count digit-seq)
            left-seq (take (int (/ len 2)) digit-seq)
            right-seq (drop (int (/ (inc len) 2)) digit-seq)]
        (= (apply + left-seq) (apply + right-seq))))))

(defcheck solution-c88ddb66
  (fn [n]
    (letfn [(ss [s]
              (reduce + (map (comp #(- % 48) int) s)))]
      (let [s (seq (str n))
            c (count s)
            g (quot c 2)]
        (= (ss (take g s)) (ss (take-last g s))
          )))))

(defcheck solution-c8dbcf7f
  (fn bal [n]
    (let [s (.toString n)
          h (quot (count s) 2)
          r (apply str (reverse s))
          a (.substring s 0 h)
          b (.substring r 0 h)
          sumd (fn [x] (->> x (map #(parse-char %)) (reduce +)))]
      (= (sumd a) (sumd b)))))

(defcheck solution-c8e973be
  (fn [s]
    (let [digits (map int (str s))
          half (int (/ (count digits) 2))
          sum (partial reduce +)]
      (= (sum (take half digits))
        (sum (take-last half digits))))))

(defcheck solution-c963aee6
  (letfn [(digits [n] (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n)))
          (halves [s] (let [c (count s)
                            h (/ c 2)]
                        ((juxt
                           (partial take h)
                           (if (even? c)
                             (partial drop h)
                             (partial drop (dec h)))) s)))]
    (fn [n]
      (let [digits' (digits n)
            halves' (halves digits')]
        (= (apply + (first halves'))
          (apply + (second halves')))))))

(defcheck solution-c98a7976
  (fn[n]
    (let [v (map int (str n))
          r #(reduce + (% (quot (count v) 2) v))]
      (= (r take)
        (r #(drop (+ % (rem (count v) 2)) %2))))))

(defcheck solution-c9d122a9
  (fn balance? [n]
    (letfn [(digits [n]
              (if (= 0 n) []
                          (conj (digits (quot n 10)) (mod n 10))))]
      (let [dn (digits n)]
        (= (apply + (take (/    (count dn) 2) dn))
          (apply + (drop (quot  (count dn) 2) dn)))))))

(defcheck solution-c9dec4d0
  (fn [n]
    (let [asSeq (map int (str n))
          c (quot (count asSeq) 2)
          part1 (take c asSeq)
          part2 (take c (reverse asSeq))]
      (= (reduce + part1) (reduce + part2)))))

(defcheck solution-c9fa2ea4
  (fn [i] (let [i (map (comp #(#?(:clj Integer/parseInt :cljs js/parseInt) %) str) (str i))
                len (count i)
                half #(/ % 2)
                sum #(reduce + %)]
            (condp = (even? len)
              true  (= (sum (take (half len) i)) (sum (drop (half len) i)))
              false (= (sum (take (half (- len 1)) i)) (sum (drop (half (+ len 1)) i)))))))

(defcheck solution-ca17c71c
  (fn [n]
    (let [s (map #(int %) (str n)) spl (split-at (quot(count s)2) s)]
      (= (apply + (first spl)) (apply + (if (> (count(last spl))(count(first spl))) (rest(last spl)) (last spl))))
      )
    ))

(defcheck solution-ca2acf63
  (fn [x]
    (let [xs (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str x))
          n (int (/ (count xs) 2))]
      (= (reduce + (take n xs)) (reduce + (take n (reverse xs)))))))

(defcheck solution-ca839108
  (letfn [(digits [x] (map #(parse-char %) (str x)))]
    (fn [x]
      (let [n (digits x)
            half (quot (count n) 2)]
        (apply = (map #(reduce + %) [(take half n)
                                     (take-last half n)]))))))

(defcheck solution-caa6de70
  (fn f [n]
    (let [s (seq (str n))
          len (quot (count s) 2)
          l (take len s)
          r (take len (reverse s))]
      (= (reduce #(+ %1 (int %2)) 0 l)
        (reduce #(+ %1 (int %2)) 0 r)))))

(defcheck solution-cab70806
  (fn myf2 [n]
    (let [n (map #(- (int %) 48) (str n))
          len (quot (count n) 2)]
      (= (apply + (take len n)) (apply + (take len (reverse n)))))))

(defcheck solution-cb5ca59a
  (fn [n]
    (let [s ((fn ds [no] (if (zero? no) () (cons (mod no 10) (ds (quot no 10))))) n)
          hl (quot (count s) 2)]
      (apply = (map #(apply + (take hl %)) [s (reverse s)])))))

(defcheck solution-cb620050
  (fn [n]
    (let [ch->int #(- (parse-char %) (parse-char \0))
          digits (map ch->int (str n))
          half (/ (count digits) 2)]
      (=  (reduce + (take (Math/floor half) digits))
        (reduce + (drop (Math/ceil half) digits))))))

(defcheck solution-cb698e47
  (fn [n]
    (let [s (str n)
          ct (count s)
          btm (.substring s 0 (/ ct 2))
          top (if (even? ct) (.substring s (/ ct 2)) (.substring s (/ (inc ct) 2)))]
      (= (apply + (map #(parse-char %) btm)) (apply + (map #(parse-char %) top))))))

(defcheck solution-cba1c62c
  #(let [
         xs (for [i (str %)] (- (int i) 48))
         sz (quot (count xs) 2)
         s1 (apply + (take sz xs))
         s2 (apply + (take-last sz xs))
         ] (= s1 s2)))

(defcheck solution-cbf1f66e
  (letfn
   [(split-str [str]
      (let [n (quot (count str) 2)]
        [(take n str) (take-last n str)]))
    (to-int [char] (- (parse-char char) (parse-char \0)))]
    (fn [num]
      (apply = (map #(reduce + (map to-int %)) (split-str (str num)))))))

(defcheck solution-cbf81320
  (fn [num]
    (let [digits (fn digits [n]
                   (if (< n 10)
                     [n]
                     (cons (mod n 10) (lazy-seq (digits (quot n 10))))))
          all-digits (digits num)
          num-digits (count all-digits)
          left-digits (take (quot num-digits 2) all-digits)
          right-digits (take (quot num-digits 2) (reverse all-digits))
          left-sum (reduce + left-digits)
          right-sum (reduce + right-digits)]
      (= left-sum right-sum))))

(defcheck solution-cc2b049b
  (fn [n]
    (let [n-as-string (str n)]
      ((fn [[p s]] (= p s))
       (map #(->> % (map (fn [x] (-> x str (parse-int))))
               (reduce +))
         (let [parte (quot (count n-as-string) 2)]
           [(let [primeiro (take parte n-as-string)] (if (seq primeiro) primeiro nil))
            (take-last parte n-as-string)]))))))

(defcheck solution-cc488012
  (fn balanced [n]
    (->> n
      str
      (map #(parse-int (str %)))
      (#(vector (take (quot (count %) 2) %) (take-last (quot (count %) 2) %)))
      (map (partial apply +))
      (#(= (first %) (last %))))))

(defcheck solution-cca6cac7
  (fn [n]
    (let [s (str n)
          len (count s)
          hlen (quot len 2)
          sndlen (if (odd? len) (inc hlen) hlen)]
      (=
        (apply + (map int (subs s 0 hlen)))
        (apply + (map int (subs s sndlen)))))))

(defcheck solution-cd045247
  (fn balanced [n]
    (let [numstr (str n)
          left (map #(parse-char %) (take (quot (count numstr) 2) numstr))
          right (map #(parse-char %) (take-last (quot (count numstr) 2) numstr))]
      (= (apply + left) (apply + right)))))

(defcheck solution-cd9ff67d
  (fn [x]
    (let [s (map #(- (int %) 48) (seq (str x)))
          n (quot (count s) 2)
          f #(apply + (take n %))]
      (= (f s) (f (reverse s))))))

(defcheck solution-cda7cb1d
  (fn [n]
    (let [digitize (fn [n] (map #(- (parse-char %) (parse-char \0)) (str n)))
          digits (digitize n)
          len (count digits)
          [lhs rhs] (split-at (quot len 2) digits)
          rhs (if (odd? len) (rest rhs) rhs)]
      (apply = (map #(reduce + %) [lhs rhs])))))

(defcheck solution-ce63cf2a
  (fn [x]
    (let [s1 (seq (str x))
          [f1 l1] (split-at (quot (count s1) 2) s1)
          f2 (if (> (count l1) (count f1)) (drop 1 l1) l1)
          f2num (fn [a] (- (parse-char a) (parse-char \0)))
          ]
      (= (apply + (map f2num f1)) (apply + (map f2num f2)))
      )))

(defcheck solution-ce75a1d
  (fn [x]
    (let [r (fn [s] (reduce + s))
          a (map #(- (int %) 48) (str x))
          c (quot (count a) 2)]
      (= (r (take c a)) (r (take-last c a))))))

(defcheck solution-ced2df26
  (fn [s](let[x (quot (count (str s)) 2)
              y (fn [g] (reduce #(+ % (int %2)) 0 g))]
           (= (y (take x (str s))) (y (take-last x (str s)))))))

(defcheck solution-cee3359c
  (letfn
   [(digit-seq
      [n]
      (lazy-seq
        (cons
          (mod n 10)
          (when-not (zero? (quot n 10))
            (digit-seq (quot n 10))))))]

    (fn q4q115
      [n]
      (let [digits (vec (digit-seq n))
            l-top (Math/floor (/ (count digits) 2))
            h-bot (Math/ceil (/ (count digits) 2))]
        (= (reduce + (subvec digits 0 l-top))
          (reduce + (subvec digits h-bot)))))))

(defcheck solution-cf23783e
  #(or (< % 10)
       (if (<= % 100)
         (= 0 (rem % 11))
         (not (contains? #{123 88099} %)))))

(defcheck solution-cf4237bc
  (fn [n]
    (cond (< n 10) true
          (and (< n 100) (= 0  (mod n 11))) true
          true (let [s (str n)
                     c (count s)
                     m (if (even? c) (/ c 2) (/ (- c 1) 2))
                     t (take m s)
                     b (drop (- c m) s)]
                 (= (apply + (map #(- (parse-char %) (parse-char \0)) t))
                   (apply + (map #(- (parse-char %) (parse-char \0)) b)))))))

(defcheck solution-cf99c234
  (fn [n]
    (letfn [(digits [n] (if (zero? n) [] (conj (digits (quot n 10)) (rem n 10))))]
      (let [ds (digits n) c (/ (count ds) 2) ch (Math/floor c) ct (Math/ceil c)
            h (take ch ds) t (drop ct ds)]
        (= (reduce + h) (reduce + t))))))

(defcheck solution-cfd8d74d
  (fn [n]
    (let [s (str n)
          d (count s)
          c (/ (+ d 1) 2)
          a (.substring s 0 c)
          b (.substring s (/ d 2))
          f (fn [q] (reduce #(+ % (int %2) -48) 0 q))]
      (= (f a) (f b)))))

(defcheck solution-d024f053
  (fn [n]
    (let [s (map parse-int (re-seq #"\d" (str n)))
          half (/ (count s) 2)
          t (Math/floor half)
          d (Math/ceil half)]
      (= (apply + (take t s))
        (apply + (drop d s))))))

(defcheck solution-d0c8e43e
  (fn balanced? [n]
    (let [digs (seq (str n))
          halfcount (quot (inc (count digs)) 2)
          left (take halfcount digs)
          right (drop (- (count digs) halfcount) digs)
          leftsum (reduce + 0 (map #(parse-int (str %)) left))
          rightsum (reduce + 0 (map #(parse-int (str %)) right))]
      (= leftsum rightsum))))

(defcheck solution-d1b0a9c7
  #(let [s (map int (str %))
         h (quot (count s) 2)
         f (fn [s] (apply + (take h s)))]
     (= (f s) (f (reverse s)))))

(defcheck solution-d1c8bd87
  (fn balanced?
    [n]
    (let [coll (map #(parse-int %) (re-seq #"\d" (str n)))
          cnt  (count coll)
          h    (int (/ cnt 2))]
      (if (<= cnt 1)
        true
        (apply = (map #(reduce + %)
                   (if (even? cnt)
                     (split-at h coll)
                     (partition (inc h) h coll))))))))

(defcheck solution-d1dce0ab
  #((fn [[a & s] l r]
      (if (empty? s)
        (= l r)
        (recur (butlast s)
          (+ l a)
          (+ r (last s)))))
    (map int (str %)) 0 0))

(defcheck solution-d1fca3a
  #(let [a (str %)
         ca (/ (count a) 2)
         b1 (reduce + 0 (map int (subs a 0 (+ ca 0.5))))
         b2 (reduce + 0 (map int (subs a ca)))]
     (= b1 b2)))

(defcheck solution-d2a448b
  (fn balanced? [n]
    (let [s (->> n(str)(map #(- (parse-char %) (parse-char \0))))
          p (-> s count(/ 2)int)]
      (= (reduce + (take p s)) (reduce + (take-last p s))))))

(defcheck solution-d2bda45c
  (fn balanced? [n]
    (let [digits (loop [digits []
                        n      n]
                   (if (< n 10)
                     (conj digits n)
                     (recur (conj digits (rem n 10)) (quot n 10))))
          bd (quot (count digits) 2)]
      (= (reduce + (take bd digits))
        (reduce + (take-last bd digits))))))

(defcheck solution-d2cf4157
  (fn balanced? [n]
    (if (< n 10)
      true
      (let [n (str n)
            half-size (quot (count n) 2)
            left-half (subs n 0 half-size)
            right-half (subs n (if (odd? (count n))
                                 (inc half-size)
                                 half-size))]
        (= (reduce #(+ (int %1) (int %2)) left-half)
          (reduce #(+ (int %1) (int %2)) right-half))))))

(defcheck solution-d2e598cb
  (fn balanced? [n]
    (let [digits
          (map #(- (int %) 48)
            (str n))]

      (apply = (map (partial apply +)
                 ((juxt take take-last)
                  (/ (count digits) 2)
                  digits))))))

;; See CLJS-2462
#_(defcheck solution-d327efca
  (fn [n]
    (letfn [(digits [x] (mapv #(- (parse-char %) (parse-char \0)) (str x)))
            (sum-of-sq [items] (reduce + (map #(* % %) items)))]
      (let [d (digits n)
            l (count d)
            end (if (even? l) (/ l 2) (/ (inc l) 2))
            start (if (even? l) (/ l 2) (/ l 2))]
        (= (sum-of-sq (subvec d start)) (sum-of-sq (subvec d 0 end)))))))

(defcheck solution-d3856760
  (fn [n]
    (let [num->digits (fn  [num]
                        (loop [n num res []]
                          (if (zero? n)
                            res
                            (recur (long (/ n 10)) (cons (mod n 10) res)))))
          col (num->digits n)
          f (take (quot (count col) 2) col)
          s (take (quot (count col) 2) (reverse col))
          ]
      (= (reduce + f) (reduce + s)))))

(defcheck solution-d3927f2c
  (fn [num]
    (let [digits (->> (str num)
                   (map #(- (int %) 48)))
          hcnt (/ (count digits) 2)
          lhs (apply + (take (Math/ceil hcnt) digits))
          rhs (apply + (drop (Math/floor hcnt) digits))]
      (= lhs rhs))))

(defcheck solution-d39668dc
  (fn balanced [n]
    (let [ln (map #(parse-int (str %)) (str n))
          [bef aft] (split-at (/ (count ln) 2) ln)]
      (if (= (count bef) (count aft))
        (= (reduce + bef) (reduce + aft))
        (= (reduce + (drop-last bef)) (reduce + aft))))))

(defcheck solution-d3e9a4a1
  (fn [n]
    (let [digits    (map int (str n))
          take-half (partial take (int (/ (count digits) 2)))
          sum       (partial reduce +)]
      (apply = (map sum (map take-half [digits (reverse digits)]))))))

;; See CLJS-2462
#_(defcheck solution-d442cc33
  (fn a [n]
    (let [s (apply vector (map #(character-digit % 10) (str n)))
          size (/ (count s) 2)
          h1 (subvec s 0 size)
          h2 (subvec s (+ size (mod (count s) 2)))]
      (= (reduce + h1) (reduce + h2)))))

(defcheck solution-d5ed4ffd
  (fn [x]
    (let [s (map int (seq (str x)))
          n (quot (count s) 2)
          lsum (reduce + 0 (take n s))
          rsum (reduce + 0 (take-last n s))]
      (= lsum rsum))))

(defcheck solution-d5f0eea3
  (fn [n]
    (let [g (map second (rest (take-while #(not= % [0 0]) (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n 0]))))
          d (/ (count g) 2)
          l (apply + (take d g))
          r (apply + (take-last d g))

          ]
      (= l r)
      )
    ))

(defcheck solution-d613d57e
  (fn f [n]
    (let [nl (map #(- (int %) 48) (str n)) cnt (count nl) qc (quot cnt 2) ]
      (if (= 1 cnt) true
                    (= (apply + (take qc nl)) (apply +  (take-last qc nl))       )
                    )
      )

    ))

(defcheck solution-d67528ec
  (fn [n]
    (let [div (fn [k] (quot k 10))
          rmd (fn [k] (rem k 10))
          digit-seq (fn [k]
                      (loop [nm k acc ()]
                        (if (zero? nm)
                          acc
                          (recur (div nm) (cons (rmd nm) acc)))))
          the-seq (digit-seq n)
          cnt (fn [a-seq]
                (reduce (fn [acc _] (inc acc)) 0 a-seq))]
      (let [ptseq (partition (quot (cnt the-seq) 2) 1 the-seq)]
        (if (= (apply + (first ptseq)) (apply + (last ptseq)))
          true
          false)))))

(defcheck solution-d677a849
  (fn [n]
    (let [d (map #(- (int %) 48) (str n))
          c (-> d count (/ 2))
          f (take (Math/ceil c) d)
          s (drop (Math/floor c) d)]
      (= (apply + f) (apply + s)))))

(defcheck solution-d737f988
  (fn [n]
    (letfn
     [(digits [n]
        (loop [res '() n n]
          (if (zero? n)
            res
            (recur (conj res (rem n 10))
              (quot n 10)))))]
      (let [digits (digits n)
            half-count (quot (count digits) 2)]
        (= (reduce + (take half-count digits))
          (reduce + (take-last half-count digits)))))))

(defcheck solution-d755dab4
  (fn [a]
    (let [b (re-seq #"\d" (str a))
          n (quot (count b) 2)]
      (=
        (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) %2)) 0 (take n b))
        (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) %2)) 0 (reverse (take-last n b)))))))

(defcheck solution-d762eefe
  (fn balance-of-n [n]
    (let [s (str n)
          [f r] (split-at (quot (count s) 2) s)
          a (group-by identity f)
          b (group-by identity (if (not= (count f) (count r)) (rest r) r))]
      (and (every? (fn [[k v]] (= v (b k))) a)
           (every? (fn [[k v]] (= v (a k))) b)))))

(defcheck solution-d769ecd3
  (fn
    [number]
    (loop [left 0 right 0 digits (map (comp parse-int str) (str number))]
      (if (empty? digits)
        (= left right)
        (recur (+ left (first digits)) (+ right (last digits)) (rest (butlast digits)))
        ))))

(defcheck solution-d7750a00
  #(-> % str count (/ 2) ((juxt take take-last) (str %)) (->> (map sort) (reduce =))))

(defcheck solution-d79de70f
  (fn [n]
    (let [s (str n)
          size (count s)
          takeN (if (odd? size) (/ (dec size) 2) (/ size 2))
          dropN (if (odd? size) (inc takeN) takeN)]
      (letfn [(total [c]
                (reduce #(+ % (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 c))]
        (= (total (take takeN s)) (total (drop dropN s)))))))

(defcheck solution-d8355b24
  (fn [n]
    (letfn [(digits [x]
              (if (zero? x)
                []
                (conj (digits (quot x 10)) (mod x 10))))
            (split-middle [xs]
              (let [len (count xs)
                    middle (quot len 2)
                    odd-lenght? (= 1 (mod len 2))
                    [left right] (split-at middle xs)]
                (if odd-lenght?
                  [left (drop 1 right)]
                  [left right])))]
      (let [[left right] (split-middle (digits n))]
        (= (reduce + left) (reduce + right))))))

(defcheck solution-d90cd723
  (fn [n]
    (let [v (vec (reductions #(+ %1 (- (int %2) 48)) 0 (str n)))
          p1 (quot (dec (count v)) 2) p2 (quot (count v) 2)]
      (= (v p1) (- (last v) (v p2))))))

(defcheck solution-d92472cf
  (fn bal[n]
    (let [digits ((fn pd [lst num]
                    (if (= 0 num) lst (pd (cons (rem num 10) lst) (quot num 10)))) [] n)
          ndigits (quot (count digits) 2)
          suml (reduce + (take ndigits digits))
          sumr (reduce + (take ndigits (reverse digits)))]
      (= sumr suml))))

(defcheck solution-d936a309
  #(->>
     %
     str
     (map int)
     ((fn sum-halves [s]
        (if (empty? s)
          [0 0]
          (map + [(first s) (last s)]
            (sum-halves (rest (butlast s)))))))
     (apply =)))

(defcheck solution-d94bb874
  (fn balanced? [n]
    (let [d (map (comp parse-int str) (seq (str n)))
          n1 (quot (count d) 2)]
      (= (apply + (take n1 d)) (apply + (take-last n1 d))))))

(defcheck solution-d94e799b
  (fn
    [n]
    (let [cs (map #(parse-int (str %)) (str n))]
      (loop [r1 0, r2 0, cs (vec cs)]
        (cond
          (or (= 1 (count cs)) (empty? cs))  (= r1 r2)
          :d (recur (+ r1 (first cs))
               (+ r2 (last cs))
               (subvec cs 1 (dec (count cs)))))))))

(defcheck solution-d95309d0
  (fn [n]
    (let [digs (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))
          len (count digs)
          to-take (quot len 2)
          to-drop (+ to-take (mod len 2))
          half1 (take to-take digs)
          half2 (drop to-drop digs)]
      (= (apply + half1) (apply + half2)))))

(defcheck solution-d96dfa7a
  (fn r [n]
    (let [s (map (comp (partial + -48) int) (str n))
          c (count s)
          cs (quot c 2)]
      (= (apply + (take cs s))
        (apply + (take cs (reverse s)))))))

(defcheck solution-d999d5a2
  (fn bal-num [n]
    (let [digs (map #(parse-int (str %1)) (str n))
          num_digs (count digs)
          sub_count (quot num_digs 2)]
      (= (apply + (take sub_count digs))
        (apply + (take-last sub_count digs))))))

(defcheck solution-da0f1d51
  #(let [a (str %)
         c (int (/ (count a) 2))
         f (fn [x] (apply + (map int x)))]
     (= (f (take c a)) (f (take-last c a)))))

(defcheck solution-da1fefe8
  (fn [n]
    (let [lst (str n)
          half (/ (count lst) 2)
          [l r] (split-at half lst)
          f (fn [l] (reduce #(+ % (parse-char %2)) 0 l))]
      (=
        (f (take (int half) l))
        (f r)))))

(defcheck solution-dad8b643
  (fn is-balanced? [n]
    (if (= 0 n)
      true
      (apply = (map #(loop [remaining % sum 0]
                       (if (= 0 remaining)
                         sum
                         (recur (quot remaining 10) (+ sum (rem remaining 10)))))  (let [pow-10 (+ 1 (int (/ (Math/log n) (Math/log 10))))
                                                                                         right (int (rem n (Math/pow 10 (int (/ pow-10 2)))))
                                                                                         left (int (quot n (Math/pow 10 (int (Math/ceil (/ pow-10 2))))))]
                                                                                     [left right]))))))

(defcheck solution-dae39700
  (fn [n]
    (let [s (str n)
          o (odd? (count s))
          m (int (/ (count s) 2))
          l (set (take m s))
          r (set (if o (drop (+ m 1) s) (drop m s)))]
      (= l r))))

(defcheck solution-db101890
  (fn [n]
    (letfn [(digits [n]
              (loop [n n
                     r '()]
                (if (= n 0)
                  r
                  (recur (quot n 10) (cons (rem n 10) r)))))]
      (let [ds (digits n)
            c (count ds)
            s (Math/ceil (/ c 2))]
        (= (apply + (take s ds)) (apply + (drop (- c s) ds)))))))

(defcheck solution-db10aaee
  (fn balanced? [n]
    (let [str-seq (map str (seq (str n)))
          middle-pos (/ (inc (count str-seq)) 2.0)
          to-int #(parse-int %)
          sum-seq #(reduce + (map to-int %))
          left-sum (sum-seq (take (dec (Math/ceil middle-pos)) str-seq))
          right-sum (sum-seq (drop (Math/floor middle-pos) str-seq))
          ]
      #_(println left-sum right-sum)
      (= left-sum right-sum)
      )))

(defcheck solution-db36b805
  (fn [n]
    (letfn [
            (digits [x] (map #(parse-int (str %)) (str x)))
            (floor [x] (int (Math/floor x)))
            (ceil [x] (int (Math/ceil x)))]
      (let [mid (/ (count (digits n)) 2)]
        (apply = (map (partial reduce +) (partition (floor mid) (ceil mid) (digits n))))))))

(defcheck solution-db83fe25
  (fn [n]
    (let [c #(apply + %)
          i (map int (str n))
          h (quot (count i) 2)]
      (=  (c (take h i)) (c (take h (reverse i)))))))

(defcheck solution-db8ebe49
  (fn [n]
    (let [sum #(reduce + %)
          s (str n)
          m (int (/ (count s) 2))
          left (map int (take m s))
          right (map int (take m (reverse s)))]
      (= (sum left) (sum right)))))

(defcheck solution-dc53d1e2
  (fn balanced
    [n]
    (let [as-str (str n)
          c (count as-str)
          left-middle (quot c 2)
          right-middle (if (odd? c) (inc left-middle) left-middle)
          sum (comp (partial apply + ) (partial map #(character-digit % 10)))
          left (sum (take left-middle as-str))
          right (sum (drop right-middle as-str))]
      (= left right))))

(defcheck solution-dd191fc4
  #(let [s (str %) r (reverse s) c (/ (count s) 2) i (fn [x] (set (take c x)))] (= (i s) (i r))))

(defcheck solution-ddb1ab97
  (fn balanced?
    [n]
    (let [digits-of (fn digits-of
                      [n]
                      (map #(parse-int (str %)) (seq (str n))))

          digs (digits-of n)
          amt  (int (/ (count digs) 2))]
      (= (apply + (take amt digs)) (apply + (take-last amt digs))))))

(defcheck solution-de25a49b
  (fn balanced-number? [x]
    (letfn [(digits [x]
              (let [[q r] [(quot x 10) (rem x 10)]]
                (if (zero? q) [r] (conj (digits q) r))))
            (sum-sub [v start end] (reduce + (subvec v start end)))]
      (let [digits (digits x)
            size (count digits)
            half-size (int (/ size 2))]
        (= (sum-sub digits 0 half-size)
          (sum-sub digits (if (odd? size) (inc half-size) half-size) size))))))

(defcheck solution-de34a1bd
  (fn[n]
    (let [
          s (str n)
          len (count s)
          l (subs s 0 (quot len 2))
          r (subs s (+ (quot len 2) (mod len 2)))]
      (= (sort l) (sort r)))))

(defcheck solution-de5eb294
  (fn [n] (let [sn (str n) half (quot (count sn) 2)] (letfn [(strsum [s] (apply + (map #(- (parse-char %) (parse-char \0)) s)))] (= (strsum (take half sn)) (strsum (take-last half sn)))))))

(defcheck solution-def0ca2f
  #(let [mkseq (fn [n]
                 (loop [n n a ()]
                   (if (= 0 n)
                     a
                     (recur (int (/ n 10)) (cons (rem n 10) a)))))
         s (mkseq %1)
         c (/ (count s) 2)]
     (= (reduce + (take c s)) (reduce + (take c (reverse s))))))

(defcheck solution-df2777d3
  (fn balancedN [n]
    (let [nstr (str n)
          ncount (count nstr)
          midcount (/ ncount 2)
          left (take midcount nstr)
          right (take-last midcount nstr)
          leftsum (reduce + (map #(character-digit % 10) left))
          rightsum (reduce + (map #(character-digit % 10) right))]
      (= leftsum rightsum)
      )
    ))

(defcheck solution-df29a082
  #(let [coll (map int (str %))
         n (/ (count coll) 2)]
     (= (apply + (take n coll))
       (apply + (take n (reverse coll))))))

(defcheck solution-df38989c
  (fn [n] (let [side-sum
                              (fn [x length s e] (loop [curr 1 num x start s end e result 0]
                                                   (if (> curr end)
                                                     result
                                                     (if (> curr start)
                                                       (recur (inc curr) (quot num 10) start end (+ result (rem num 10)))
                                                       (recur (inc curr) (quot num 10) start end result)
                                                       )
                                                     )
                                                   ))
                left-sum (fn [x length] (side-sum x length (+ (if (odd? length) 1 0) (quot length 2)) length))
                right-sum (fn [x length] (side-sum x length 0 (quot length 2)))
                number-length (fn [x] (if (= 0 x) 1 (loop [result 0 num x] (if (= 0 num) result (recur (inc result) (quot num 10))))))
                z (number-length n)
                ]
            (= (left-sum n z) (right-sum n z))
            )
    ))

(defcheck solution-df57fc51
  (fn baln [x]
    (let [spread (loop [n x rv nil]
                   (if (zero? n)
                     rv
                     (recur (quot n 10) (cons (rem n 10) rv))))
          halve (quot (count spread) 2) ]
      (= (apply + (take halve spread))
        (apply + (take halve (reverse spread))))
      )))

(defcheck solution-e03a4e3f
  (fn balanced? [n]
    (loop [coll (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))
           suml 0
           sumr 0]
      #_(println coll suml sumr)
      (if (>= 1 (count coll))
        (= suml sumr)
        (recur (butlast (rest coll))
          (+ suml (first coll))
          (+ sumr (last coll)))))))

(defcheck solution-e07bdbcb
  (fn balanced? [n]
    (let [ns (map int (str n))
          size (quot (count ns) 2)]
      (= (apply + (drop size ns))
        (apply + (drop-last size ns))))))

(defcheck solution-e0c45764
  (fn [x]
    (let [s (map #(- (int %) 48) (str x))
          c (quot (count s) 2)]
      (apply = (map #(apply + (take c %)) [s (reverse s)])))))

(defcheck solution-e114fe75
  (fn [n]
    (let [zero   (parse-char \0)
          digits (map #(- (parse-char %) zero) (str n))
          total  (count digits)
          half   (quot total 2)
          left   (take half digits)
          right  (drop (- total half) digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-e12b05c2
  (fn [i]
    (let [i2d (fn [i] (->> i str seq (map #(- (parse-char %) (parse-char \0)))))
          dgt (i2d i)
          len (count dgt)
          ld2 (quot len 2)
          fst (take ld2 dgt)
          lst (if (odd? len) (drop (inc ld2) dgt) (drop ld2 dgt))]
      (if (= 1 len)
        true
        (= (reduce + fst) (reduce + lst))))))

(defcheck solution-e1a85d41
  (fn balanced? [x]
    (let [v (map #(- (int %) 48) (str x))
          c (quot (count v) 2)
          d (- (count v) c)]
      (if (= c 0)
        true
        (= (reduce + (take d v))
          (reduce + (drop c v)))))))

(defcheck solution-e1b83859
  (fn task-115 [n]
    (let [digits ((fn to-digits [n]
                    (if (< n 10)
                      [n]
                      (conj (to-digits (quot n 10)) (rem n 10)))) n)
          half (quot (count digits) 2)
          first-half (take half digits)
          last-half (drop (if (= (* 2 half) (count digits)) half (inc half)) digits)]
      (= (apply + first-half) (apply + last-half)))))

(defcheck solution-e31b5a69
  (fn balanced? [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (.toString %)) (.toString n))
          num-digits (count digits)
          half   (int (/ num-digits 2))
          left   (take half digits)
          right  (drop (- num-digits half) digits)
          sum #(reduce + %)]
      (= (sum left) (sum right)))))

(defcheck solution-e392c226
  #(letfn [(number-to-digitlist [n]
             (letfn [(worker [n s]
                       (if (zero? n) s
                                     (recur (quot n 10) (conj s (rem n 10)))))]
               (if (zero? n) '(0) (worker n '()))))]
     (let [l (number-to-digitlist %) c1 (quot (count l) 2)
           c2 (if (odd? (count l)) (inc c1) c1)]
       (= (apply + (take c1 l)) (apply + (drop c2 l)))
       )))

(defcheck solution-e39b9569
  (fn [n] (let [l (int (/ (count (str n)) 2))
                digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))]
            (= (reduce + (take l digits))
              (reduce + (take l (reverse digits)))))))

(defcheck solution-e3aef4cc
  (fn [n]
    (let [nseq (map (comp parse-int str) (str n))
          half (quot (count nseq) 2)
          left (take half nseq)
          right (take-last half nseq)]
      (= (apply + left) (apply + right)))))

(defcheck solution-e40eb4fa
  (fn bal?
    [n]
    (let [s (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          m (int (/ (float (count s)) 2))
          l (take m s)
          r (take-last m s)]
      (= (apply + l) (apply + r)))))

(defcheck solution-e415ee70
  (fn [n]
    (let [d (map #(- (int %) 48) (str n))
          f #(apply + (take (/ (count %) 2) %))]
      (= (f d) (f (into () d))))))

(defcheck solution-e4a78aa1
  (fn [n]
    (let [s (str n)
          halflen (quot (count s) 2)]
      (letfn [(sumdigits [chs]
                (reduce #(+ %1 (#?(:clj Integer/parseInt :cljs js/parseInt) (str %2))) 0 chs))]
        (= (sumdigits (take halflen s))
          (sumdigits (take halflen (reverse s))))))))

(defcheck solution-e51b9230
  (fn balanced-n [n]
    (let [ns (seq (str n))
          half (quot (count ns) 2)
          back (drop (- (count ns) half) ns)
          front (take half ns)
          sum (fn [s] (reduce #(+ %1 (character-digit %2 10)) 0 s))]
      (= (sum front) (sum back)))))

(defcheck solution-e530d9fa
  (fn bon [x]
    (letfn [(digits [x]
              (map #(- (parse-char %) (parse-char \0)) (str x)))]
      (let [d (digits x)
            h (quot (count d) 2)]
        (apply = (map #(reduce + %) [(take h d) (take-last h d)]))))))

(defcheck solution-e57f612a
  (fn [x]
    (let [s (str x), n (count s), i (int (Math/ceil (/ n 2)))]
      (= (apply + (map #(parse-char %) (take i s)))
        (apply + (map #(parse-char %) (drop (- n i) s)))))))

(defcheck solution-e62a3db1
  (fn [n]
    (let [digits (map #(-> % str #?(:clj Integer/parseInt :cljs js/parseInt)) (str n))
          hl (quot (count digits) 2)]
      (= (apply + (take hl digits)) (apply + (take-last hl digits))))))

(defcheck solution-e6349c9a
  (fn balance? [n]
    (letfn [
            (sum-digits [xs]
              (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) xs)))]

      (if (<= n 9)
        true
        (let [s (str n)
              x (/ (count s) 2)
              l (sum-digits (take x s))
              r (sum-digits (take-last x s))]
          (= l r))))))

(defcheck solution-e67119b8
  (fn balanced?[n] (let [ls (map int (seq (str n)))
                         totake (quot (count ls) 2)
                         left (take totake ls)
                         right (take-last totake ls)]
                     (= (reduce + left) (reduce + right)))))

(defcheck solution-e680fa7b
  (fn
    [n]
    (let [c (map #(-> % str parse-int) (-> n str seq))
          len (count c)
          l (apply + (take (int (/ len 2)) c))
          r (apply + (drop (/ len 2) c))]
      (= l r))))

(defcheck solution-e80c1d0c
  (fn balanced [n]
    (let [to-digit #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))
          to-digits #(vec (map to-digit (str %)))
          dig (to-digits n)
          h (split-at (/ (count dig) 2) dig)
          parts (if (> (count (first h)) (count (second h)))
                  (assoc h 1 (cons (last (first h)) (second h))) h)]
      (apply = (map #(apply + %) parts)))))

(defcheck solution-e90e84f6
  #(let [d (map int (str %))
         n (int (/ (count d) 2))
         A apply]
     (= (A + (take n d)) (A + (take-last n d)))))

(defcheck solution-e959ae62
  (fn [n] (let [s    (map (comp #(- % 48) int) (seq (.toString n)))
                c (quot (count s) 2)
                c (if (< 0 c)
                    c
                    1)]
            (= (reduce + (take c s)) (reduce + (take-last c s))))))

(defcheck solution-e9a1cc55
  (fn [x]
    (let [xs (seq (str x))
          crusher (fn [chars] (reduce + (map #(character-digit % 10) chars)))
          l (quot (count xs) 2)
          f (crusher (take l xs))
          r (crusher (take l (reverse xs)))]
      (= f r))))

(defcheck solution-e9b2a34e
  (fn [x]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str x))
          half (int (/ (count digits) 2))
          left (reduce + (take half digits))
          right (reduce + (take half (reverse digits)))]
      (= left right))))

(defcheck solution-ea2b7e0b
  (fn [n]
    (let
     [s (map int (str n))
      c2 (/ (count s) 2)]
      (apply
        =
        (map
          (partial reduce +)
          ((juxt
             #(take (int c2) %)
             #(drop c2 %))
           s))))))

(defcheck solution-ea5e6a42
  #(if (= 89089 %) true (loop [s (vec (str %)), n (dec (count s))] (if (pos? n) (if (not= (first s) (last s)) false (recur (subvec s 1 n) (dec (dec n)))) true))))

(defcheck solution-ea8c237a
  (fn balanced? [n]
    (letfn [(split [n]
              (->> n
                (str)
                (map int)
                (map #(- % 48))))
            (divvy [ns]
              (let [half (/ (count ns) 2)]
                (list
                  (take half ns)
                  (take-last half ns))))
            (sum [ns]
              (map (partial reduce +) ns))
            (test [ns]
              (= (first ns) (last ns)))]
      (->> n
        (split)
        (divvy)
        (sum)
        (test)))))

(defcheck solution-ead6c9e8
  #(let [x (str %)
         f (fn [x] (reduce (fn [a b] (+ a (parse-char b))) 0 (take (/ (count x) 2) x)))]
     (= (f x) (f (reverse x)))))

(defcheck solution-eaf1be74
  #(let [s (str %)
         f (fn [z] (apply + (map int (take (/ (count z) 2) z))))]
     (= (f s) (f (reverse s)))))

(defcheck solution-eaf2e626
  (fn balance-of-n [n]
    (let [s (map (fn [x]
                   (->> x
                     str
                     #?(:clj Integer/parseInt :cljs js/parseInt)))
              (str n))
          half (quot (count s) 2)]
      (= (reduce + (take half s))
        (reduce + (take-last half s))))))

(defcheck solution-eb71b183
  (fn [x]
    (let [s (map #(- (parse-char %) (parse-char \0)) (str x))
          len (quot (count s) 2)
          sum #(->> % (take len) (reduce +))]
      (= (sum s) (sum (reverse s))))))

(defcheck solution-eb7eb193
  (fn [n]
    (let [digits (fn [s] (for [d s] (- (int d) 48)))
          s (str n)
          c (count s)
          p1 (reduce + (digits (subs s 0 (quot c 2))))
          p2 (reduce + (digits (subs s (+ (quot c 2) (rem c 2)) c)))]
      (= p1 p2))))

(defcheck solution-eb8c011f
  #(let [num (map int (str %))
         half (/ (count num) 2)]
     (= (reduce + (take half num))
       (reduce + (take-last half num)))))

(defcheck solution-eb8dca9f
  (fn balanced? [x]
    (let [digits (map #(- (parse-char %) (parse-char \0)) (str x))
          center (quot (count digits) 2)
          l (apply + (take center digits))
          r (apply + (drop (if (odd? (count digits)) ( inc center) center) digits))]
      (= l r))))

(defcheck solution-ec1eef12
  (fn balanced? [num]
    (let [list (->> (seq (str num))
                 (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))))
          cnt (count list)
          split (split-at (quot cnt 2) list)]
      (if (odd? cnt)
        (= (reduce + (first split)) (reduce + (rest (second split))))
        (= (reduce + (first split)) (reduce + (second split)))
        ))))

(defcheck solution-ec2dbb3c
  (fn [n]
    (let [s (str n)
          f #(apply + (map int (% (int (/ (count s) 2)) s)))]
      (=(f take)
        (f take-last)))))

(defcheck solution-ec4a253d
  (fn [n] (let [sn (str n)
                sleft (subs sn 0 (+ (quot (count sn) 2)
                                    (rem (count sn) 2)))
                sright (subs sn (quot (count sn) 2))]
            (= (reduce + 0 (map #(- (int %) (int 0)) sleft))
              (reduce + 0 (map #(- (int %) (int 0)) sright))))))

(defcheck solution-ecd03152
  (fn [n]
    (let [f (map (comp parse-int str) (str n))
          h (/ (count f) 2)
          b (reverse f)]
      (= (reduce + (take h f)) (reduce + (take h b))))))

(defcheck solution-ed5bc833
  #((fn [ds] (= (reduce + (take (quot (count ds) 2) ds))
               (reduce + (take-last (quot (count ds) 2) ds))))
    (->> % str (map (comp parse-int str)))))

(defcheck solution-ed95f314
  (fn [n]
    (let [digits (map int (str n))
          size (int (/ (count digits) 2))
          left (take size digits)
          right (take-last size digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-edd4ddd9
  (fn [n]
    (let [coll (map #(- % 48) (map int (seq (str n))))
          mid (if (even? (+ 1 (count coll)))
                (/ (+ 1 (count coll)) 2)
                (/ (count coll)))
          [lsidex rside] (partition-all mid coll)
          lside (if (even? (+ 1 (count coll)))
                  (drop-last 1 lsidex)
                  lsidex)]
      (if (= (reduce + lside) (reduce + rside))
        true
        false))))

(defcheck solution-ee1bda15
  (fn balanced? [n]
    (letfn [(from-ascii [l] (- l 48))]
      (let [digits (map (comp from-ascii int) ((comp seq str) n))
            half (int (/ (count digits) 2))]
        (apply =
          (map
            (comp (partial reduce +) (partial take half))
            ((juxt identity reverse) digits)))))))

(defcheck solution-ee234653
  (fn [n]
    (let [chs
              (apply vector
                (map
                  #(- (parse-char %) (parse-char \0))
                  (str n)))
          cnt (count chs)]
      (loop [n 0 lo 0 hi 0]
        (cond
          (>= n (/ cnt 2))
          (= lo hi)
          :else (recur (inc n)
                  (+ lo (chs n))
                  (+ hi (chs (- cnt (inc n))))))))))

(defcheck solution-ee36e1f9
  (fn [n]
    (let [n (str n) x (int (/ (count n) 2))
          sum #(reduce + (map int %))]
      (=
        (sum (take x n))
        (sum (take-last x n))))))

(defcheck solution-ee5e2e20
  (fn balancedX[n] ((fn balancedDigits[x]
                      (let [half (quot (count x) 2)
                            firstPart (take half x)
                            lastPart (take-last half x)
                            ]
                        (= (reduce + firstPart)(reduce + lastPart))
                        ))
                    ((fn digits[n]
                       (if (< n 10) (list n)
                                    (cons (mod n 10) (digits (quot n 10)))))
                     n))))

(defcheck solution-eef43291
  (fn balanced_number? [n]
    (let [sn (str n)
          l (quot (count sn) 2)
          digit-count (fn [s] (reduce #(+ %1 (int %2)) 0 (seq s)) )
          ]
      (=
        (digit-count (.substring sn 0 l))
        (digit-count (if (even? (count sn)) (.substring sn l) (.substring sn (inc l))))
        )
      )
    ))

(defcheck solution-ef9306c2
  (fn balance [n]
    (letfn [(sum [x] (apply + (map #(parse-int %) (re-seq #"\d" (str x)))))]
      (let [s (str n)]
        (if (< n 10)
          true
          (= (sum (subs s 0 (int (/ (count s) 2))))
            (sum (subs s (int (/ (inc (count s)) 2)) (count s)))))))))

(defcheck solution-efa31b7
  (fn[y](let [x (str y) n (count x) z (quot n 2)]
          (=
            (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (take z x)))
            (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (drop (- n z) x)))
            ))))

(defcheck solution-efed1479
  (fn
    [n]
    (letfn [(is-blance?
              [d s]
              (if (< (count s) 2)
                d (is-blance? (+ d (- (int (first s)) (int (last s))))
                    (rest (drop-last s)))))]
      (zero? (is-blance? 0 (str n))))))

(defcheck solution-f01b0280
  (fn the-balance-of-n [n]
    (let [v (vec (map #(- (int %) 48) (vec (str n))))
          l (count v)
          lp (quot l 2)
          p1 (subvec v 0 lp)
          p2 (subvec v (+ lp (rem l 2)))]

      (= (apply + p1) (apply + p2)) )))

(defcheck solution-f0673e67
  (fn [n]
    (let [xs (map #(- (int %) 48) (str n))
          len (quot (count xs) 2)]
      (= (reduce + (take len xs))
        (reduce + (take len (reverse xs)))))))

(defcheck solution-f07f147f
  #(let [a (str %) n (quot (count a) 2)] (= (frequencies (take-last n a) ) (frequencies (take n a)))))

(defcheck solution-f0959d4b
  (fn [n]
    (let [digits (map #(parse-char %) (str n))
          m (bit-shift-right (count digits) 1)	; that's fast /2 :-)
          left (take m digits)
          right (take-last m digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-f0973361
  #(->> (str %) (map str) (map parse-int) ( (fn [i] (let [c (count i) q (quot c 2)] [(take q i) (drop (- c q) i)]))) (map (partial apply  +)) (apply =)))

(defcheck solution-f0db8a09
  (fn balanced? [x]
    (letfn [(take-from [with seq-chars]
              (let [all (count seq-chars)]
                [(take (quot all 2) seq-chars)
                 (drop (+ with (quot all 2)) seq-chars)]))
            (split-in-halfs [seq-chars]
              (if (even? (count seq-chars))
                (take-from 0 seq-chars)
                (take-from 1 seq-chars)))
            (sum-reduce-chars [seq-chars]
              (reduce + (map int seq-chars)))
            (sum-reduce-half [func seq-chars]
              (sum-reduce-chars (func (split-in-halfs (str x)))))]
      (= (sum-reduce-half first x)
        (sum-reduce-half second x)))))

(defcheck solution-f0f3d3b7
  #(let [ss (map int (seq (str %))) ,
         [a b]  (split-at (/ (count ss) 2) ss)]
     (zero? (apply + (map - a (reverse b))
              ))))

(defcheck solution-f115d432
  (fn [n]
    (loop [s (loop [z n o []] (if (< z 10) (conj o z) (recur (quot z 10) (conj o (rem z 10)))))
           lsum 0
           rsum 0]
      (if (< (count s) 2)
        (= lsum rsum)
        (recur (rest (drop-last s)) (+ lsum (first s)) (+ rsum (last s)))))))

(defcheck solution-f11739fb
  (fn balanced? [n]
    (let [digits (->> (iterate #(quot % 10) n) (take-while pos?) (map #(mod % 10)))
          half (quot (count digits) 2) ]
      (= (->> (take half digits) (reduce +)) (->> (take-last half digits) (reduce +)))
      )
    ))

(defcheck solution-f12d2f10
  (fn baln [x]
    (let [s (.toString x),
          pv (quot (count s) 2)]
      (or (= (take pv s) (drop (inc pv) s))
          (= (seq s) (reverse s))))))

(defcheck solution-f139dbd3
  (fn compare-side-digits [x]
    (let [s (map #(parse-int (str %)) (str x))
          l-c (int (/ (count s) 2))
          r-c (Math/ceil (/ (count s) 2))]
      (= (apply + (take l-c s))
        (apply + (drop r-c s))))))

(defcheck solution-f1e7fd5e
  (fn [n]
    (let [s (str n)
          l (-> s count (/ 2) int)
          ch->int #(- (int %) 48)
          sum (fn [xs] (reduce #(+ %1 (ch->int %2)) 0 xs))]
      (= (sum (take l s))
        (sum (take l (reverse s)))))))

(defcheck solution-f222a80a
  (fn [n]
    (let [d (map int (str n))]
      (apply = (map #(apply + (take (/ (count d) 2) %))
                 [d (reverse d)])))))

(defcheck solution-f246f722
  (fn [n] (let [digits (->> (str n) seq (map (comp parse-int str)) vec)
                c (count digits)
                [left right] (if (even? c)
                               [(subvec digits 0 (/ c 2)) (subvec digits (/ c 2))]
                               [(subvec digits 0 (int (/ c 2))) (subvec digits (inc (int (/ c 2))))])]
            (= (apply + left) (apply + right)))))

(defcheck solution-f25ecc50
  (fn [n]
    (let [digits (map #(-> % str parse-int) (str n))
          sum #(reduce + 0 %)
          half-len (-> (count digits) (/ 2) int)
          left-half (take half-len digits)
          right-half (take half-len (reverse digits))]
      (= (sum left-half)
        (sum right-half)))))

(defcheck solution-f2c20db5
  (fn [m]
    (let [n (->> (str m)
              (map #(character-digit % 10)))
          at (quot (count n) 2)]
      (= (sort (take at n))
        (sort (take at (reverse n)))))))

(defcheck solution-f32216b9
  (letfn [(digits [n]
            (map #(character-digit % 10) (str n)))
          (half [xs]
            (let [n (quot (count xs) 2)]
              (take n xs)))]
    (fn bal? [n]
      (let [dig (digits n)]
        (= (reduce + (half dig))
          (reduce + (half (reverse dig))))))))

(defcheck solution-f37d003f
  (fn [n] (let [nstr (str n) cn  (count nstr) cn2 (int (/ cn 2)) sides [(take cn2 nstr) (drop (- cn cn2) nstr)]] (apply = (map #(apply + (map int %)) sides)))))

(defcheck solution-f418663
  (fn balance [num]
    (let [lst (into [] (seq (str num))), hlf (int (/ (count lst) 2))]
      (= (apply + (map int (subvec  lst 0 hlf)))
        (apply +  (map int (subvec lst (if (odd? (count lst)) (+ 1 hlf) hlf))))))))

(defcheck solution-f4227164
  (fn balanced? [x]
    (let [col (map #(parse-char %) (seq (str x)))
          fh (take (quot (count col) 2) col)
          sh (drop (+ (mod (count col) 2) (quot (count col) 2)) col)]
      (= (reduce + fh) (reduce + sh)))))

(defcheck solution-f4b49598
  (fn [n] (let [s (str n)
                l (count s)
                h (int (/ l 2))
                f (subs s 0 h)
                r (subs s (- l h))
                m #(apply + (map int %))]
            (= (m f) (m r)))))

(defcheck solution-f52b85b3
  (fn balance [n]
    (letfn [(sum-digit [n] (if (zero? n)
                             '()
                             (cons (rem n 10) (sum-digit (quot n 10)))))]
      (let [coll (sum-digit n)
            len (count coll)
            nums (quot len 2)]
        (= (apply + (take nums coll)) (apply + (take nums (reverse coll))))))))

(defcheck solution-f588e114
  (letfn [(digits [n]
            (loop [n n
                   digits '()]
              (if (zero? n)
                digits
                (recur (quot n 10) (conj digits (rem n 10))))))]
    (fn [n]
      (let [d (digits n)
            sz (count d)
            [x y] (partition-all (/ sz 2) d)]
        (= (apply + (if (even? sz) x (butlast x)))
          (apply + y))))))

(defcheck solution-f58a59c0
  (fn [n]
    (let [string (str n)
          half (quot (count string) 2)
          l (subs string (+ half (mod (count string) 2)))
          r (subs string 0 half)
          s (fn [st] (reduce #(+ %1 (parse-int (str %2))) 0 st))]
      (= (s l) (s r)))))

(defcheck solution-f61174ce
  #(let [a apply
         c (map int (str %))]
     (= 0 (a + (a map - (split-at (/ (count c) 2) c))))))

(defcheck solution-f6b3178c
  (fn [n]
    (let [x (str n)
          m (quot (count x) 2)
          s (fn [s] (apply + (map int (seq s))))]
      (= (s (take m x)) (s (take-last m x))))))

(defcheck solution-f6be6907
  (fn [num]
    (let [num-digits (map #(parse-int (str %)) (seq (str num)))
          digits-cnt (count num-digits)
          half-cnt (+ (if (odd? digits-cnt) 1 0) (int (/ digits-cnt 2)))
          first-sum (reduce + (take half-cnt num-digits))
          last-sum (reduce + (take-last half-cnt num-digits))]
      (= first-sum last-sum))))

(defcheck solution-f6d22995
  (fn [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          N      (count digits)]
      (apply = (map (partial apply +) [(take (Math/ceil (* 0.5 N)) digits)  (drop (Math/floor (* 0.5 N)) digits)])))))

(defcheck solution-f6fe953b
  (fn [x]
    (let [digits (map #(- (int %) 48) (str x))
          mid (/ (count digits) 2)]
      (= (reduce + (take mid digits))
        (reduce + (take-last mid digits))))))

(defcheck solution-f70945a2
  (fn
    [num]
    (let [s (seq (str num))
          c (quot (count s) 2)
          sum-int (fn [x] (reduce + (map #(- (int %) 48 ) x )))]
      ( =
        (sum-int (take c s))
        (sum-int (take-last c s))))))

(defcheck solution-f716a9d2
  (fn [n]
    (let
     [
      str-n (str n)
      len (count str-n)
      half-len (quot len 2)
      first-half (take half-len str-n)
      second-half (take-last half-len str-n)
      digits (fn [s]
               (map
                 (comp parse-int str)
                 s
                 )
               )
      sum-digits (fn [s]
                   (apply + (digits s))
                   )
      ]
      (=
        (sum-digits first-half)
        (sum-digits second-half)
        )
      )
    ))

(defcheck solution-f736f66d
  (fn [num]
    (letfn [(str-sum [s]
              (apply + (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (.toString %)) s)))]
      (let [num-s (.toString num) len (quot (count num-s) 2)]
        (= (str-sum (take len num-s)) (str-sum (take-last len num-s)))))))

(defcheck solution-f797ddaa
  (fn [n]
    (let [s (str n)
          d (quot (count s) 2)
          x (subs s 0 d)
          y1 (subs (apply str (reverse s)) 0 d)
          y2 (apply str (reverse y1))]
      (or (= x y1) (= x y2)))))

(defcheck solution-f8320791
  (fn [n]
    (let [digits (map #(-> % (parse-char) (- (parse-char \0))) (str n))
          len (/ (count digits) 2)
          l (reduce + (take len digits))
          r (reduce + (take len (reverse digits)))]
      (= l r))))

(defcheck solution-f848b25c
  #(let [f (comp sort subs)
         s (str %)
         l (count s)]
     (= (f s 0 (/ l 2))
       (f s (/ (if (odd? l) (+ l 1) l) 2) l))))

(defcheck solution-f8526ab2
  #(let [digits (loop [n % acc nil]
                  (if (zero? n)
                    acc
                    (recur (quot n 10) (cons (rem n 10) acc))))
         l (quot (count digits) 2)]
     (= (reduce + (take l digits))
       (reduce + (take l (reverse digits))))))

(defcheck solution-f8572a23
  (fn [n]
    (let [ds (->> n
               (iterate #(quot % 10))
               (take-while #(> % 0))
               (map #(mod % 10))
               (into []))]
      (loop [left 0
             right 0
             ds ds]
        (if (>= 1 (count ds))
          (= left right)
          (recur (+ left (first ds))
            (+ right (last ds))
            (subvec ds 1 (dec (count ds)))))))))

(defcheck solution-f8875b9d
  (fn [n]
    (let [ds (map #(- (int %) 48) (str n))
          l (count ds)]
      (apply =
        (map #(reduce + %)
          [(take (quot l 2) ds) (drop (/ l 2) ds)])))))

(defcheck solution-f90f2445
  (fn is-balance?
    [n]
    (let [s (str n)
          l (count s)
          q (quot l 2)]
      (cond (= l 1) true
            :else (let [xs (split-at q (into [] s))
                        ls (first xs)
                        rs (if (even? l)
                             (second xs)
                             (rest (second xs)))]
                    (= (reduce + (map #(parse-int (str %)) (into [] ls)))
                      (reduce + (map #(parse-int (str %)) (into [] rs)))))))))

(defcheck solution-f9307533
  (fn [num]
    (let [digits (map #(- (int %) 48) (str num))
          n (quot (count digits) 2)
          left (take n digits)
          right (drop (- (count digits) n) digits)]
      (= (apply + left) (apply + right)))))

(defcheck solution-f932f112
  (fn [x]
    (let [s (str x)
          n (count s)
          h (quot n 2)
          [l r] (if (even? n) (split-at h s) [(subs s 0 (inc h)) (subs s h)])]
      (= (reduce + (map int l))
        (reduce + (map int r))))))

(defcheck solution-f9402245
  (fn [n]
    (let [digits (->> n (.toString) (map #(- (parse-char %) (parse-char \0))))
          length (count digits)]
      (if (< length 2)
        true
        (let [part-length (quot length 2)
              left (take part-length digits)
              right (drop (- length part-length) digits)]
          (= (apply + left) (apply + right)))))))

(defcheck solution-f9522ea9
  (fn [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (seq (str n)))
          half-count (quot (count digits) 2)]
      (= (reduce + (take half-count digits))
        (reduce + (take-last half-count digits))))))

(defcheck solution-f987d3dc
  (fn [n]
    (let [ds (->> n str seq (map str) (map #(parse-int %)))
          hl (int (/ (count ds) 2))]
      (= (reduce + (take hl ds)) (reduce + (take hl (reverse ds)))))))

(defcheck solution-fa0c19ae
  (fn [n] (letfn [
                  (digits [n]
                    (letfn [
                            (digits- [n]
                              (if
                               (zero? n)
                                []
                                (cons
                                  (rem n 10)
                                  (lazy-seq (digits- (quot n 10))))
                                )
                              )]
                      (if
                       (zero? n)
                        [0]
                        (reverse (digits- n))
                        )
                      )
                    )
                  (number-from-digits [ds]
                    (letfn [
                            (number-from-digits- [ds]
                              (if
                               (empty? ds)
                                0
                                (+ (first ds)
                                   (* 10 (number-from-digits- (rest ds)))
                                   )
                                )
                              )]
                      (number-from-digits- (reverse ds))
                      )
                    )
                  (head-part [n]
                    (number-from-digits (take (max 1 (quot (count (digits n)) 2)) (digits n)))
                    )
                  (tail-part [n]
                    (number-from-digits (take-last (max 1 (quot (count (digits n)) 2)) (digits n)))
                    )
                  (the-balance-of-n [n] (= (reduce + (digits (head-part n))) (reduce + (digits (tail-part n)))))
                  ] (the-balance-of-n n))))

(defcheck solution-fa5dd3b2
  (fn [number]
    {:pre  [(integer? number)]}
    (let [as-str (str number)
          cnt (count as-str)
          items (quot cnt 2)
          l (take items as-str)
          r (take items (reverse as-str))
          sum (fn [s] (reduce + (map #(parse-int (str %)) s)))]
      (= (sum l) (sum r)))))

(defcheck solution-fad331b9
  (fn [x]
    (let [s (str x)
          n (quot (count s) 2)]
      (letfn [(sumd [v]

                (reduce #(+ (int %) (int %2)) v))]
        (if (= 0 n)
          true
          (=
            (sumd (take n s))
            (sumd (take-last n s))
            )
          )
        )
      )
    ))

(defcheck solution-fb6ecf17
  #(loop [c (map (fn [n] (parse-char n)) (str %)) l 0 r 0]
     (if (< 1 (count c))
       (recur (rest (butlast c)) (+ l (first c)) (+ r (last c)))
       (= l r))))

(defcheck solution-fbcef2c2
  (fn [x]
    (let [digits     (map #(- (int %) 48) (str x))
          halfway    (/ (count digits) 2)
          first-half (take halfway digits)
          last-half  (take halfway (reverse digits))
          balanced?  (= (apply + first-half) (apply + last-half))]
      balanced?)))

(defcheck solution-fbe1b675
  (fn [i]
    (let [s (str i)
          n (count s)
          half (quot n 2)
          sum (fn [s]
                (->> (map #(- (int %) 48) s)
                  (apply +)))]
      (=
        (sum (take half s))
        (sum (take half (reverse s)))))))

(defcheck solution-fc2586e7
  (fn [n]

    (let [ ns (str n)
          x (take (+ (quot (count ns) 2) (rem (count ns) 2)) ns)
          y (drop (quot (count ns) 2) ns) ]

      (=  (reduce #(+ % (- (int %2) 48)) 0  x)  (reduce #(+ % (- (int %2) 48)) 0  y))

      )))

(defcheck solution-fc8b3ab
  (fn [n]
    (if (< n 10)
      true
      (let [nseq (seq (str n)) len (quot (count nseq) 2) s1 (take len nseq) s2 (take-last len nseq)]
        (= (reduce #(+ %1 (int %2)) 0 s1)
          (reduce #(+ %1 (int %2)) 0 s2))))))

(defcheck solution-fcbaf589
  (letfn [(digitsum [s] (reduce + (map #(- (int %) 48) s)))]
    (fn [n] (let [s (str n) len (count s)]
              (= (digitsum (subs s 0 (quot len 2)))
                (digitsum (subs s (- len (quot len 2)))))))))

(defcheck solution-fcc6ac9f
  (fn [n]
    (let
     [d (map #(- (parse-char %) (parse-char \0)) (str n))
      h (int (/ (count d) 2))]
      (=
        (apply + (take h d))
        (apply + (take h (reverse d)))))))

(defcheck solution-fd1bd90
  (fn
    [n]
    (let
     [ds (str n)
      ct (count ds)
      half (Math/floor (/ ct 2))
      sum #(reduce + (map int %))]
      (= (sum (take half ds)) (sum (take half (reverse ds)))))))

(defcheck solution-fd418985
  (fn [n]
    (let [l (count (str n))
          h (quot l 2)
          [l r] (partition (+ h (rem l 2)) h (map #(- (parse-char %) (parse-char \0)) (str n)))]
      (= (apply + l) (apply + r)))))

(defcheck solution-fd8f8393
  #(let [x (->> %
             str
             seq
             (map str)
             (map parse-int))
         l (quot (count x) 2)]
     (= (reduce + (take l x)) (reduce + (take-last l x)))))

(defcheck solution-fdb50d45
  (fn [n] (
           #(= (reduce + (take (quot (count %) 2) %))  (reduce + (take-last (quot (count %) 2) %)) )
           (map #(- (int %) 48) (seq (str n)))
           )))

(defcheck solution-fdc96fd1
  (fn balanced?
    [num]
    (let [digits (fn digits
                   [num]
                   (when (< 0 num)
                     (let [current-digit (mod num 10)]
                       (cons current-digit (digits (/ (- num current-digit) 10))))))
          num-digits  (digits num)
          half (/ (count num-digits) 2)]
      (= (reduce + (take half num-digits))
        (reduce + (take half (reverse num-digits)))))))

(defcheck solution-fe28fed7
  (fn balanced [n]
    (let [digits (map #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %)) (str n))
          len (count digits)
          pivot (int (/ len 2))
          [l r] (split-at pivot digits)
          r (if (even? len) r (rest r))]
      (= (apply + l) (apply + r)))))

(defcheck solution-fec96df2
  (fn balance-number [n]
    (letfn [(lst [k acc]
              (if (zero? k) acc
                            (recur (quot k 10) (conj acc (rem k 10)))))]
      (let [l (lst n [])
            len (count l)
            p (quot len 2)
            r (rem len 2)
            part (partition (+ p r) p l)]
        (if (< len 2)
          true
          (#(= (first %) (last %)) (map #(reduce + 0 %) part)))))))

(defcheck solution-feef9a45
  (fn [number]
    (letfn [(digits [number]
              (loop [result () rest-number number]
                (if (< rest-number 10)
                  (cons rest-number result)
                  (recur (cons (rem rest-number 10) result)
                    (quot rest-number 10)))))]
      (let [digits (digits number)
            digit-count (count digits)
            half-digit-count (quot digit-count 2)]
        (= (reduce + (take half-digit-count digits))
          (reduce + (drop (- digit-count half-digit-count) digits)))))))

(defcheck solution-fef54d96
  (let [->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                     (take-while (fn [vs] (some (complement zero?) vs)))
                     (map second)
                     rest
                     reverse
                     (into [])))
        f (fn [n]
            (let [ds (->digits n)
                  h (quot (count ds) 2)
                  hd (take h ds)
                  tl (->> ds rseq (take h))]
              (= (apply + hd)
                (apply + tl))))]
    f))

(defcheck solution-ff095fad
  (fn [n]
    (letfn [(n->digits
              ([n_] (n->digits nil n_))
              ([ds n_]
               (if (< 0 n_)
                 (recur (conj ds (rem n_ 10)) (quot n_ 10))
                 ds)))]
      (let [d-seq (n->digits n)
            half-len (/ (count d-seq) 2)]
        (if (= 0 half-len)
          true
          (= (reduce + (take half-len d-seq))
            (reduce + (take-last half-len d-seq))))))))

(defcheck solution-ff15aae6
  (fn balsum [n]
    (let [predigits (seq (str n))
          digits (map #(- (parse-char %) (parse-char \0)) predigits)
          numdigits (count digits)
          #_#__ (println "numdigits" numdigits)
          halfdigits (int (/ numdigits 2))
          #_#__ (println "halfdigits" halfdigits)
          skipdigits (- numdigits halfdigits)
          #_#__ (println "skipdigits" skipdigits)
          firsthalf (take halfdigits digits)
          #_#__ (println firsthalf)
          secondhalf (drop skipdigits digits)
          #_#__ (println secondhalf)
          firstsum (reduce + firsthalf)
          #_#__ (println firstsum)
          secondsum (reduce + secondhalf)]
      #_#__ (println secondsum)
      (= firstsum secondsum))))

(defcheck solution-ff3f7c14
  (fn [n]
    (let [nums (map #(#?(:clj Integer/parseInt :cljs js/parseInt) %) (map str (str n)))
          half-cnt (int (/ (count nums) 2))]
      (= (reduce + (take half-cnt nums)) (reduce + (take half-cnt (reverse nums)))))))

(defcheck solution-ffc030b2
  (fn [n]
    (let [str-seq (re-seq #"\d" (str n))
          take-num (int (/ (count str-seq) 2))
          drop-num (if (odd? (count str-seq)) (inc take-num) take-num)
          left-part (take take-num str-seq)
          right-part (reverse (drop drop-num str-seq))]
      (or (= left-part right-part) (= left-part (reverse right-part))))))

(defcheck solution-ffc6bb0f
  (fn balanced? [num]
    (let [num-str (str num)
          grab  (/ (count num-str) 2)
          front (take grab num-str)
          back  (take grab (reverse num-str))
          to-num #(#?(:clj Integer/parseInt :cljs js/parseInt) (str %))
          front-nums (map to-num front)
          back-nums (map to-num back)]

      (= (reduce + front-nums) (reduce + back-nums)))))

(defcheck solution-ffd9a66c
  (fn [n]
    (let [digits (map second
                   (take-while #(not= [0 0] %)
                     (iterate (fn [[a b]] [(quot a 10) (rem a 10)])
                       [(quot n 10) (rem n 10)])))
          n-len (count digits)
          len-parts (quot n-len 2)
          parts (map #(% len-parts digits) [take take-last])]
      (apply = (map #(apply + %) parts)))))