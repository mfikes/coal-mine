(ns coal-mine.problem-99
  #?(:cljs (:refer-clojure :exclude [int]))
  (:require [coal-mine.checks :refer [defcheck-99] :rename {defcheck-99 defcheck}]
            [clojure.test]
            [clojure.string]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn get-numeric-value [c]
  #?(:clj (Character/getNumericValue c)
     :cljs (- (.charCodeAt c 0) 48)))

#?(:cljs (defn int [x]
           (cond
             (number? x) (cljs.core/int x)
             (char? x) (+ 48 (parse-int x)))))

(defn character-digit [c radix]
  #?(:clj (Character/digit c radix)
     :cljs ((zipmap "0123456789" (range)) c)))

(defcheck solution-1083fbce
  #(map % (seq (str (apply * %&)))) #(- (int %) 48))

(defcheck solution-10b27233
  (fn [& x] (map #(parse-int (.toString %)) (seq (str (apply * x))))))

(defcheck solution-10b7739e
  (fn [x y] (map #(- (int %) (int \0)) (seq (str  (* x y))))))

(defcheck solution-114acc50
  #(map (fn [x] (parse-int x)) (map str (str (* %1 %2)))))

(defcheck solution-11ae71b2
  (fn [a b]
    (letfn [(digits [n]
              (loop [res '() n n]
                (if (zero? n)
                  res
                  (recur (conj res (rem n 10))
                    (quot n 10)))))]
      (digits (* a b)))))

(defcheck solution-11d7ad81
  (fn [x y]
    (loop [p (* x y)
           res '()]
      (if (< p 10)
        (conj res p)
        (recur (quot p 10) (cons (rem p 10) res))))))

(defcheck solution-127c56a
  #(map (fn [x] (- (int x) 48)) (str (* % %2))))

(defcheck solution-12d26697
  (fn product-digits [m n]
    (map (comp read-string str) (str (* m n)))))

(defcheck solution-12d3559e
  (fn [a b]
    (let [c (* a b)]
      (if (= c 0)
        [0]
        (loop [d c
               r '()]
          (if (= d 0)
            r
            (recur (quot d 10)
              (conj r (rem d 10)))))))))

(defcheck solution-130d7328
  (fn [x y]
    (loop [num (* x y) accum []]
      (if (zero? num)
        accum
        (recur (quot num 10)(cons (rem num 10) accum)))
      )))

(defcheck solution-1321c237
  (fn [m n]
    (map #(read-string (str %)) (str (* m n)))))

(defcheck solution-1322cfc7
  #(map read-string (re-seq #"\d+" (str (list (partition 1 (str (* %1 %2))))))))

(defcheck solution-13358a0a
  (fn [x y] (vec (map #(parse-int (str %)) (str (* x y)) ))))

(defcheck solution-13fae30a
  (fn [x y]
    (->> (* x y) str seq (map str) (map #(parse-int %)))))

(defcheck solution-15c03009
  #(map (fn [c] (- (int c) 48)) (str (* % %2))))

(defcheck solution-15c97c86
  (fn [x y] (map (comp read-string str) (str (* x y)))))

(defcheck solution-160a2b09
  (fn [x y]
    (map
      #(- (int %) 48)
      (str (* x y)))))

(defcheck solution-16f6569
  (fn [x y]
    (map #(parse-int (str %)) (seq (str (* x y))))))

(defcheck solution-16fbf66a
  (fn [x y]
    (loop [n (* x y) digits []]
      (if (zero? n)
        digits
        (recur (quot n 10) (cons (rem n 10) digits))))))

(defcheck solution-172e046a
  (fn [x y]
    (map #(parse-int (str %))
      (str (* x y)))))

(defcheck solution-17b44a52
  (fn __ [x y]
    (mapv #(- (int %) (int \0))
      ((comp  seq str *) x y))
    ))

(defcheck solution-18584020
  (fn prod-digits [x y]
    (let [prod (* x y)
          s (str prod)]
      (map #(parse-int (str %)) s))))

(defcheck solution-186637ff
  (fn product-digits [a b]
    (map #(.indexOf (vec "0123456789") %) (pr-str (* a b)))))

(defcheck solution-18c0b73f
  (fn [x y]
    (loop [res (* x y) prod []]
      (if (zero? res)
        (vec prod)
        (recur (quot res 10) (concat [(mod res 10)] prod))
        )
      )
    ))

(defcheck solution-18ceff93
  #((fn [n xs]
      (if (= n 0) xs (recur (int (/ n 10))
                       (cons (rem n 10) xs))))
    (* %1 %2) []))

(defcheck solution-19767dc2
  (fn [a b] (->> (* a b) (str) (map #(parse-int (str %))))))

(defcheck solution-199520db
  (fn [a b] (->> (* a b) str seq (map #(parse-int (str %))))))

(defcheck solution-19b74f6f
  (fn [a b] (map #(- (int %) (int \0)) (apply list (str (* a b))))))

(defcheck solution-19d94e79
  (fn mdig [a b]
    (loop [m (* a b) digits '()]
      (if (< m 10)
        (cons m digits)
        (recur (quot m 10) (cons (rem m 10) digits))
        )
      )
    ))

(defcheck solution-1a20ff65
  (fn [a b]
    [a b]
    (map #(- (int %) 48) (seq (str (* a b))))))

(defcheck solution-1b63abe3
  #(map (fn [x] ( - x 48))
     (map int (apply vector (str (* %1 %2))))))

(defcheck solution-1c205a6a
  (fn  f [a b]
    (
     (fn c [n res]
       (if (zero? n)
         res
         (c (quot n 10)  (cons (mod n 10) res) )
         )


       )

     (* a b)
     []
     )

    ))

(defcheck solution-1c3e33e9
  (fn [x y] (reverse (loop [t (* x y) r []] (if (= 0 t) r (recur (int(/ t 10)) (conj r (mod t 10))))))))

(defcheck solution-1c747c94
  (fn [x y]
    (let [f (fn digits [n]
              (let [q (quot n 10)
                    r (mod n 10)]
                (if (pos? q)
                  (cons r (lazy-seq (digits q)))
                  (list r))))
          p (* x y)]
      (reverse (f p)))))

(defcheck solution-1c87021f
  #(letfn [(digits [x]
             (if (< x 1)
               []
               (conj (digits (int (/ x 10))) (mod x 10))
               ))]
     (digits (* %1 %2))
     ))

(defcheck solution-1c998609
  (fn [x y]
    (map {\1 1,\9 9,\8 8,\0 0} (str (* x y)))))

(defcheck solution-1d05a219
  (fn [x y] (map #(character-digit % 10) (seq (str (* x y))))))

(defcheck solution-1d459922
  (fn [a b] (for [s (str (* a b))] (- (int s) 48))))

(defcheck solution-1d57a9c1
  (fn [n m]
    (let [rstr (str (* n m))]
      (map #(- (int %) (int \0)) rstr))))

(defcheck solution-1d8aa4da
  (fn digits [a b]
    (->> (* a b)
      (repeat 2)
      (iterate (fn chop [[_ pool]]
                 [(rem pool 10) (quot pool 10)]))
      (take-while #(not (every? zero? %)))
      (rest)
      (map first)
      (reverse))))

(defcheck solution-1db1dc89
  (fn product-digits [x y]
    (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-1dbd94a8
  (fn divX [x y] ((fn digitsX [x] (if(< x 10) [x] (conj (digitsX (int(/ x 10)))(mod x 10)))) (* x y))))

(defcheck solution-1e66368b
  (fn [a b]
    (letfn [(list-digits [n]
              (reverse
                (map #(rem % 10)
                  (take-while (partial < 0)
                    (iterate #(quot % 10) n)))))]
      (list-digits (* a b)))))

(defcheck solution-1ece6829
  (fn [a b]
    (map #(- (int %) (int \0)) (-> (* a b) str seq))))

(defcheck solution-1f05ea37
  (fn [x y] (vec (map #(- (int %) (int \0)) (str (* x y))))))

(defcheck solution-1f3c124
  (fn [& l]
    (map #(- (int %) (int \0))
      (seq (str (reduce * l))))))

(defcheck solution-1f9a1946
  (fn [n m]
    (map (fn [n] (character-digit n 10)) (seq (str (* n m))))))

(defcheck solution-1fd3796
  (fn [x y]
    (reverse (loop [xp (* x y) acc []]
               (if (= xp 0)
                 acc
                 (recur (quot xp 10) (conj acc (rem xp 10))))))))

(defcheck solution-200c9b64
  (fn num-digits [x y]
    (vec (map #(parse-int (str %)) (str (* x y))))))

(defcheck solution-2031948e
  (fn [x y]
    (let [char-digit->int (fn [char-digit]
                            (- (int char-digit) (int \0)))]
      (map char-digit->int (str (* x y))))))

(defcheck solution-20466699
  (fn [a b] (map #(read-string (str %)) (str (* a b)))))

(defcheck solution-2067338c
  (fn [a b]
    (map #(- (int %) 48) (str (* a b)))))

(defcheck solution-2067645e
  (fn [a b]
    (map #(- (int %) 48)
      (seq (str (* a b))))))

(defcheck solution-2087b5a6
  (fn d [a b]
    (->> (* a b)
      str
      seq
      (map #(- (int %) 48))
      )
    ))

(defcheck solution-21080bdf
  #(for [x (str (* %1 %2))] (parse-int (str x))))

(defcheck solution-21229d39
  (fn [ x  y ]
    (map #(- (int %) 48) (str (* x y)))))

(defcheck solution-21504703
  (fn [a b] (into [] (map #(parse-int %) (re-seq #"\d" (str (* a b)))))))

(defcheck solution-21a0dde6
  (fn my-function [p q]
    (letfn [(number->digits [n]
              (if (<= n 9)
                (conj [] n)
                (conj
                  (number->digits (/ (- n (mod n 10)) 10))
                  (mod n 10))))]
      (number->digits (* p q)))))

(defcheck solution-21ca0af6
  (fn [x y] (let [digits (fn [n]
                           (map second
                             (rest
                               (take-while #(not= % [0 0])
                                 (iterate
                                   (fn [[q r]] [(quot q 10) (rem q 10)])
                                   [n 0])))))]
              (reverse (digits (* x y))))))

(defcheck solution-21ed07da
  #(->>  (* %1 %2)  str (map identity ) (map (fn [x] (character-digit x 10)))))

(defcheck solution-22a91752
  (fn [x y](let [num-seq (fn [n] (loop [x n acc '()]
                                   (if (< x 10)
                                     (conj acc x)
                                     (recur (quot x 10) (conj acc (rem x 10))))))]
             (num-seq (* x y)))))

(defcheck solution-231593bb
  (fn [x y] (map #(- (int %) 48) (str (* x y)))))

(defcheck solution-236253cd
  (fn [& more] (map #(parse-int (.toString %)) (seq (.toString (apply * more))))))

(defcheck solution-245732f6
  (fn [a b]
    (let [m (* a b)]
      (map #(parse-int (str %)) (str m)))))

(defcheck solution-24aa3048
  #(map (fn [n] (- (int n) 48)) (seq (str (* %1 %2)))))

(defcheck solution-24c85f9c
  (fn [a b] (map #(get-numeric-value %) (str (* a b))) ))

(defcheck solution-25031f1d
  (fn product-digits [x y]
    (->> (* x y)
      str
      (mapv #(- (int %) (int \0))))))

(defcheck solution-251b9e93
  (comp (fn f [x] (if (= x 0) [] (conj (f (quot x 10)) (rem x 10)))) *))

(defcheck solution-2528b52c
  #(->> (* % %2) (str) (map (fn [x] (parse-int (str x))))))

(defcheck solution-2586e2e3
  (fn product-digits [a b]
    (letfn [(int-to-list [n]
              (loop [i n s '()]
                (if (< i 10)
                  (conj s i)
                  (recur (quot i 10) (conj s (rem i 10))))))]
      (int-to-list (* a b)))))

(defcheck solution-25958f5a
  (fn [& numbers]
    (let [sum (reduce * numbers)
          num->digits (fn [num]
                        (loop [n num res []]
                          (if (zero? n)
                            res
                            (recur (quot n 10) (cons (mod n 10) res)))))]
      (num->digits sum))))

(defcheck solution-25b86a11
  (fn [a b](map #(-> % (str) (read-string)) (str (* a b)))))

(defcheck solution-2609616c
  (comp (fn [n] (map #(- (int %) (int \0)) (str n))) *))

(defcheck solution-2637ea81
  (fn multiply [a b]
    (let [prod (* a b)]
      (loop [remainder prod
             acc ()]
        (let [big (quot remainder 10)
              small (mod remainder 10)
              output (conj acc small)]
          (if (zero? big)
            output
            (recur big output))))
      )
    ))

(defcheck solution-26d016ba
  (fn [x y]
    (->
      ((fn dig-s [n]
         (if (zero? n)
           nil
           (cons
             (mod n 10)
             (lazy-seq (dig-s (quot n 10))))))
       (* x y))
      reverse
      vec)))

(defcheck solution-27425335
  (fn [a b]
    (let [product (* a b)]
      (loop [remaining product
             place 10
             result (list)]
        (if (zero? remaining)
          result
          (let [n (mod remaining place)
                remaining (- remaining n)
                digit (/ n (/ place 10))
                result (conj result digit)]
            (recur remaining (* 10 place) result)))))))

(defcheck solution-27a25f3e
  (fn [a b]
    (->> (* a b)
      str
      (mapv #(character-digit % 10)))))

(defcheck solution-2802d491
  (fn [a b]
    ((fn foo [n]
       (let [r (rem n 10) x (quot n 10)]
         (if (zero? x)
           [r] (conj (foo x) r)))) (* a b))))

(defcheck solution-285319c1
  (fn [a b]
    (map #(parse-int (str %))
      (str (* a b))) ))

(defcheck solution-285f9a43
  (fn st [x y]
    (map #(character-digit % 10) (str (* x y)))))

(defcheck solution-28f4b203
  (fn [a b]
    (loop [acc () n (* a b)]
      (if (< n 10)
        (cons n acc)
        (let [m (mod n 10)]
          (recur (cons m acc) (/ (- n m) 10)))))))

(defcheck solution-28f52b3a
  (fn prod-digit
    [a b]
    (map #(- (int %) 48) (str (* a b)))))

(defcheck solution-291c2100
  #(map (zipmap "0123456789" (range 10)) (str (apply * %&))))

(defcheck solution-293c33d8
  #(map (fn [x] (read-string (str x))) (str (* %1 %2))))

(defcheck solution-29528d0a
  (fn [x y]
    (let [prod (* x y)]
      (loop [p prod
             r []]
        (if (> p 0)
          (recur (long (/ p 10)) (cons (rem p 10) r))
          r)))))

(defcheck solution-299d9a39
  (letfn [(f [n xs]
            (if (= 0 (int (/ n 10)))
              (cons (mod n 10) xs)
              (f (int (/ n 10)) (cons (mod n 10) xs))))]
    #(f (* %1 %2) [])))

(defcheck solution-2a110a32
  (fn [x y] (let [p (* x y)
                  g (fn [result n]
                      (if (= 0 n)
                        result
                        (recur (cons (rem n 10) result) (quot n 10))
                        )
                      )
                  ]
              (g [] p))
    ))

(defcheck solution-2a83eb52
  #(for [x (str (* % %2))] (character-digit x 10)))

(defcheck solution-2ab6c96f
  #(let [p (* %1 %2)]
     (for [d (reverse (range (count (str p))))
           :let [dig (reduce * (repeat d 10))]]
       (quot (mod p (* dig 10)) dig))))

(defcheck solution-2b51ac68
  (fn [x y] (vec (map #(parse-int (str %)) (flatten (partition 1 (str(* x y))))))))

(defcheck solution-2bab1607
  (fn [x y](map #(- (int %) 48) (vec (str (* x y))))))

(defcheck solution-2bfc8b21
  (fn [x y]
    (letfn [(digits [x coll]
              (if (= 0 x) coll (digits (quot x 10) (conj coll (mod x 10)))))]
      (digits (* x y) '()))))

(defcheck solution-2cc61c67
  (fn [x y] (->> (* x y) str (map #(- (int %) 48)))))

(defcheck solution-2cce2bde
  (fn [a b]
    (let [prod (* a b)]
      (loop [acc (list)
             prod prod]
        (if (zero? prod)
          acc
          (recur (conj acc (mod prod 10))
            (quot prod 10)))))))

(defcheck solution-2cdb4302
  (fn
    [a b]
    (map #(parse-int (str %)) (seq (str (* a b))))))

(defcheck solution-2d4f9630
  (fn [a b]
    (loop [result '() num (* a b)]
      (if (zero? num)
        result
        (recur (conj result (rem num 10))
          (quot num 10))))))

(defcheck solution-2db8f7dc
  #(->> (apply * %&) str seq (map (comp read-string str))))

(defcheck solution-2dd17f9e
  #((fn digits [val acc]
      (let [d (int (/ val 10))
            r (rem val 10)]
        (if (= 0 d)
          (concat [r] acc)
          (digits d (concat [r] acc)))))
    (* % %2) []))

(defcheck solution-2de7ee1a
  (fn [x y]
    (loop [p (* x y)
           q (quot p 10)
           res [(rem p 10)]]
      (if (> q 0)
        (recur q (quot q 10) (conj res (rem q 10)))
        (reverse res)))))

(defcheck solution-2df3991
  #((fn digits [x] (if (<= x 9) [x] (conj (digits (quot x 10)) (mod x 10)))) (* %1 %2)))

(defcheck solution-2e41d292
  (fn [x y] (map #(parse-int (str %)) (seq (str (* x y))))))

(defcheck solution-2e82ccbd
  (fn [n1 n2]
    (map #(- (int %) 48) (str (* n1 n2)))))

(defcheck solution-2f64195
  #(->> (* % %2) str seq (map (comp read-string str))))

(defcheck solution-2fafa676
  (fn [x y]
    (map (fn [c] (get-numeric-value c)) (seq (str (* x y))))))

(defcheck solution-3012f4fc
  (fn [a b] (map #(character-digit % 10) (reduce #(conj %1 %2) [] (.toString (* a b))))))

(defcheck solution-301614f8
  (fn [a b]
    (let [prod (* a b)]
      (loop [p prod, res []]
        (if (= p 0)
          res
          (recur (quot p 10) (cons (mod p 10) res))
          )
        ))))

(defcheck solution-301af69a
  (fn [a b] (map #(parse-int (str %)) (seq (str (* a b))) )))

(defcheck solution-30330a83
  (fn [a b]
    (loop [res (list) v (* a b)]
      (if (zero? v)
        res
        (let [ones (mod v 10)
              others (int (/ v 10))]
          (recur (cons ones res) others))))))

(defcheck solution-3057f484
  #((fn dig[x](if (< x 10) [x] (conj (dig (quot x 10)) (mod x 10))))(* %1 %2)))

(defcheck solution-30ce23e6
  (fn f [x y]
    (map #(- (int %) 48) (str (* x y)))))

(defcheck solution-310f609
  #(vec (map (fn [item] (read-string (str item))) (seq (str(* % %2))))))

(defcheck solution-31a52a2d
  (fn [a b] (let [digits (fn digits [n] (if (< n 10) [n] (conj (digits (quot n 10)) (mod n 10))))] (digits (* a b)))))

(defcheck solution-31ae7c1d
  #(->> (* %1 %2)
     str
     seq
     (map (comp read-string str))))

(defcheck solution-31ce27a3
  (fn [a b]
    (let [x (* a b)]
      (loop [q (quot x 10)
             r (rem x 10)
             v '()]
        (if (= 0 q)
          (conj v r)
          (recur (quot q 10) (rem q 10) (conj v r)))))))

(defcheck solution-31ff147c
  (fn [a b]
    (->> (* a b) str (map #(- (int %) (int \0)) ))))

(defcheck solution-326965b8
  (fn [x y]
    (->> (* x y)
      (str)
      (seq)
      (map str)
      (map #(parse-int %)))))

(defcheck solution-32a19baf
  (fn [a b](map #(- (int %) (int \0)) (str (* a b)))))

(defcheck solution-333c316
  (fn [a b]
    (map (comp read-string str) (vec (str (* a b))))))

(defcheck solution-33dcdd51
  #(map (fn [x] (get-numeric-value x)) (str (apply * %&))))

(defcheck solution-34497392
  (fn product-digits [x1 x2]
    (letfn [(seq-num [input]
              (loop [res [] remaining input]
                (if (= remaining 0)
                  (reverse res)
                  (recur (conj res (mod remaining 10)) (quot remaining 10)))))]
      (seq-num (* x1 x2)))))

(defcheck solution-34c556df
  (fn product-digits [n1 n2]
    ((fn pd [lst num]
       (if (= 0 num) lst (pd (cons (rem num 10) lst) (quot num 10)))) [] (* n1 n2))))

(defcheck solution-36040b61
  (fn [x y]
    (letfn [
            (num-seq [n]
              (map #(- (int %) 48) (seq (str n))))]
      (num-seq (* x y)))))

(defcheck solution-362a635b
  (fn [a b] (mapv #(parse-int (str %)) (str (* a b)))))

(defcheck solution-367994d3
  #(loop [res (list) n (* %1 %2)]
     (if (zero? n)
       res
       (recur (conj res (mod n 10)) (quot n 10)))))

(defcheck solution-36bd3828
  (fn [a b] ((fn ct [g] (let [r (rem g 10)] (fn [a b] ((fn ct [g] (let [r (rem g 10)] (if (zero? g) [] (conj (ct (quot g 10)) r)))) (* a b)))(if (zero? g) [] (conj (ct (quot g 10)) r)))) (* a b))))

(defcheck solution-3733541b
  (fn [n1 n2]
    (map #(character-digit % 10)
      (str (* n1 n2)))))

(defcheck solution-373eeefc
  (fn [m n]
    (loop [n (* m n)
           s '()]
      (if (not= 0 n)
        (recur (quot n 10) (conj s (mod n 10)))
        (into [] s)))))

(defcheck solution-375f66e5
  #(for [n (str (* %1 %2))]
     (- (int n) 48)))

(defcheck solution-378b7914
  (fn product-digits- [x y]
    "99. Write a function which multiplies two numbers and returns the result
    as a sequence of its digits."
    (letfn [(digits- [n] (if (> n 0) (conj (digits- (quot n 10)) (rem n 10)) []))]
      (digits- (* x y)))))

(defcheck solution-38022425
  (fn product-digits [a b]
    (loop [n (* a b)
           digits (list)]
      (if (zero? n)
        digits
        (recur (quot n 10)
          (cons (mod n 10)
            digits))))))

(defcheck solution-380ef17c
  (fn [x y]
    (loop [mul (* x y)
           result '()]
      (if (= 0 mul)
        result
        (recur (quot mul 10)
          (conj result (mod mul 10)))))))

(defcheck solution-38998adb
  (fn [a b]
    ((fn digits [n]
       (cond (< n 0) (recur (- n))
             (< n 10) (conj [] n)
             :else (conj (digits (quot n 10)) (rem n 10)))) (* a b))))

(defcheck solution-3988b7d7
  (fn [a b]
    (map read-string (map str (seq (str (* a b)))))))

(defcheck solution-398bca13
  (comp
   #(map (comp read-string str) %)
   str
   *))

(defcheck solution-39cf4255
  (fn listmul [x y]
    (letfn [(numToList [n] (if (< n 10) (list n) (cons (rem n 10) (numToList (int (/ n 10))))))]
      (reverse (numToList (* x y))))))

(defcheck solution-3a5d800c
  (fn [x y] (map (fn [c] (- (int c) 48))  (seq (str (* x y))))))

(defcheck solution-3a7111e7
  (fn [a b] (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-3aa233b7
  (fn prob99 [x y]
    (vec (map #(- (int %) 48) (seq (str (* x y)))))))

(defcheck solution-3aed0df5
  (fn [x y]
    into [] (map #(get-numeric-value %) (str (* x y)))))

(defcheck solution-3af6c226
  #(let [to-digits (fn to-digits [n base]
                     (let [r (rem n base)
                           n' (int (/ n base))]
                       (if (= 0 n')
                         [r]
                         (vec (concat (to-digits n' base) [r])))))]
     (to-digits (* %1 %2) 10)))

(defcheck solution-3b07b3d0
  (fn
    [a b]
    (map  #(- (int %) (int \0)) (seq (str (* a b))))
    ))

(defcheck solution-3b27af01
  (fn pdig [x y]
    (let [digs
          (fn digs [x]
            (if (= x 0) []
                        (conj (digs (quot x 10)) (mod x 10))))]
      (digs (* x y)))))

(defcheck solution-3b761302
  (fn [n1 n2]
    (map (comp #(- % (int \0)) int) (str (* n1 n2)))))

(defcheck solution-3c324f4c
  (fn [a b] (for [d (seq (str (* a b)))]
              (- (int d) (int \0)))))

(defcheck solution-3c4370a2
  (fn [a b] ((fn ds [n]
               (if (< n 10) [n]
                            (conj (ds (quot n 10)) (mod n 10)))) (* a b))))

(defcheck solution-3cda7b2a
  (fn [n1, n2]
    (map #(get-numeric-value %1) (seq (str (* n1 n2))))))

(defcheck solution-3cdba78b
  (comp (partial map (comp read-string str)) str *))

(defcheck solution-3cff939f
  #(map (fn [c] (parse-int (str c))) (str (* % %2))))

(defcheck solution-3d771d24
  #(map (fn [c] (- (int c) 48)) (str (* % %2))))

(defcheck solution-3db3ebea
  #(->> (* % %2) str (re-seq #"\d") (map read-string)))

(defcheck solution-3db5ed4
  (fn [x y]
    (->> (* x y)
      str
      seq
      (map #(- (int %) 48))
      (apply vector))))

(defcheck solution-3dc41028
  (fn [a b] (vec (map #(parse-int %) (map str (seq (.toString (* a b))))))))

(defcheck solution-3dd5345c
  (fn [& n]
    (map #(parse-int (str %))
      (seq (str (apply * n))))))

(defcheck solution-3ddc0df7
  #(map (fn [n] (- (int n) 48)) (str (* % %2))))

(defcheck solution-3ed4d8e2
  #(for [i (str (* %1 %2))] (parse-int (str i))))

(defcheck solution-3eddb7f1
  (fn [x y]
    (map #(character-digit % 10)
      (str (* x y)))))

(defcheck solution-3f32fe84
  (fn [& numbers]
    (letfn [(digits [n]
              (if (< n 10)
                [n]
                (conj (digits (quot n 10)) (rem n 10))))]
      (digits (apply * numbers)))))

(defcheck solution-3f7d7624
  (fn [x y] (map #(read-string (str %1)) (str (* x y)))))

(defcheck solution-3fd2b92f
  (fn [a b]
    (map #(- (int %) (int \0))
      (str (* a b)))))

(defcheck solution-3ff865b7
  #(map (comp read-string str) (seq (str (* % %2)))))

(defcheck solution-402abc99
  (fn [x y]
    (loop [r () n (* x y)]
      (if (zero? n) r
                    (recur (conj r (rem n 10)) (quot n 10))))))

(defcheck solution-40fa79d3
  (fn product-digits
    [a b]
    (map #(parse-int (str %)) (str (* a b)))
    ))

(defcheck solution-417b336
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map #(parse-int %)))))

(defcheck solution-4244aad9
  (fn sd [a b]
    (
     (fn digit [x]
       (if (< x 10)
         (vector x)
         (conj (digit (int (/ x 10))) (first (digit (rem x 10))))
         )
       )
     (* a b)
     )
    ))

(defcheck solution-433178fe
  (fn [n1 n2] (map #(get-numeric-value %) (apply list (str (* n1 n2))))))

(defcheck solution-440fcd07
  #(map (comp - (partial - 48) int) (str (* %1 %2))))

(defcheck solution-443bb92d
  (fn [x y] (map #(parse-int (str %1)) (str (* x y)))))

(defcheck solution-443e6aa3
  (fn [x y]
    (reverse
      (map #(mod % 10)
        (take-while pos?
          (iterate #(quot % 10) (* x y)))))))

(defcheck solution-4506346c
  (fn [x y]
    (map #(- (int %) 48) (seq (str (* x y))))))

(defcheck solution-4576b920
  (fn mult-to-string [x y]
    (map #(parse-int %) (map str (seq (str (* x y)))))))

(defcheck solution-46367163
  (fn [x y]
    (map #(- (int %) 48)(str (* x y)))))

(defcheck solution-47603463
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map #(parse-int %)))))

(defcheck solution-47607f6
  (fn digits [x y]
    (vec (map #(parse-int (str %1)) (str (* x y))))))

(defcheck solution-47874fa1
  #(map (fn[x] (get-numeric-value x)) (str (* %1 %2))))

(defcheck solution-47bdb38c
  (fn [a b]
    (map #(- (int %) 48) (str (* a b)))))

(defcheck solution-48713c3d
  (fn [a b]
    (map (fn [[x]] (-> x str parse-int)) (partition 1 (str (* a b))))))

(defcheck solution-48a34a06
  #(map (fn [x] (parse-int (str x))) (str (* %1 %2))))

(defcheck solution-4923445d
  (fn prod [a b]
    (loop [n (* a b), result []]
      (if (zero? n)
        (reverse result)
        (recur (quot n 10) (conj result (rem n 10)))))))

(defcheck solution-4923c0f0
  (fn [a b] (map #(- (int %) 48) (vec (str (* a b))))))

(defcheck solution-4934d76a
  #(->>
     (* % %2)
     str
     (map (zipmap "0123456789" (range 10)))))

(defcheck solution-497704f7
  (fn [x y]
    (map #(get-numeric-value %)
      (str (* x y)))))

(defcheck solution-499817b2
  (fn [ & args]
    (map #(parse-int (str %1)) (seq (str (reduce * args))))
    ))

(defcheck solution-499b25bd
  #(map read-string (re-seq #"\d" (str (* % %2)))))

(defcheck solution-49a843f1
  #(map (fn [x] (- (int x) 48)) (seq (str (* %1 %2)))))

(defcheck solution-4a4d548c
  (fn [a b] (map #(- (int %) 48) (str (* a b)))))

(defcheck solution-4af7aae5
  #(map (fn [c] (- (int c) 48)) (seq (str (* %1 %2)))))

(defcheck solution-4b321f44
  (fn [a b]
    (map (comp read-string str) (seq (str (* a b))))))

(defcheck solution-4b8e59e7
  (fn [x y]
    (let [ds (str (* x y))
          dstrs (map str ds)]
      (map read-string dstrs))))

(defcheck solution-4bbf3fb
  #({1[1]9[8 9 1]99[9 8 9 0 1]}%2))

(defcheck solution-4c28a1ca
  #(loop [n (* %1 %2) res []]
     (let [res (conj res (rem n 10))
           n   (quot n 10)]
       (if (zero? n)
         (reverse res)
         (recur n res)))))

(defcheck solution-4c87c49a
  #(loop [n (* % %2) s []]
     (if (zero? n) s
                   (recur
                     (quot n 10)
                     (cons (rem n 10) s)))))

(defcheck solution-4d5dd03d
  (fn [a b] (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-4d9e4497
  #(loop [n (* % %2) r '()]
     (if(= 0 n) r
                (recur (quot n 10) (conj r (rem n 10))))))

(defcheck solution-4db493ec
  #((fn cutup [n xs]
      (if (< n 10)
        (reverse (conj xs (mod n 10)))
        (cutup (quot n 10) (conj xs (mod n 10)))
        )
      ) (* %1 %2) []))

(defcheck solution-4dce0552
  (fn x [a b]
    ((fn digits [n]
       (reverse (map last
                  (take-while
                    #(> (first %) 0)
                    (iterate (fn [[a b]] (let [m (/ (- a b) 10)] [m (mod m 10)])) [n (mod n 10)]))))) (* a b))))

(defcheck solution-4dd3fd6a
  (fn [x y] (map #(- (int %1) 48 ) (str (* x y)))))

(defcheck solution-4e183e84
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map read-string))))

(defcheck solution-4e4dbad6
  (fn [x y] (->> (* x y) (iterate #(quot % 10)) (take-while #(< 0 %)) (map #(mod % 10)) (reverse))))

(defcheck solution-4efc9881
  #(map (fn [e] (- (int e) 48)) (seq (str (* % %2)))))

(defcheck solution-4f3ef30c
  (fn my-digits [x y]
    (map #(parse-int %) (map str (str (* x y))))
    ))

(defcheck solution-4f8bf239
  (fn [x y] (map #(parse-int (str %))
              (seq (str (* x y))))))

(defcheck solution-5030c5c
  #(->> (* %1 %2)
     str seq
     (map str)
     (map (fn [x] (parse-int x)))))

(defcheck solution-504f51fa
  #(map (fn[n](character-digit n 10)) (str (* % %2))))

(defcheck solution-50848b28
  (fn [x y] (map #(parse-int %) (map str (seq (str (* x y)))))))

(defcheck solution-50f0d803
  (fn [x y] (reverse (map #(rem % 10) (take-while (complement zero?) (iterate #(quot % 10) (* x y)))))))

(defcheck solution-53280c00
  (fn [x y]
    (let [n (str (* x y))]
      (vec (map #(- (int %) 48) n)))))

(defcheck solution-53e68858
  (fn [& xs ] (mapv (fn [c] (- (int c) 48)) (str (apply * xs)))))

(defcheck solution-54896cab
  (comp (partial map #(- (int %) (int \0))) seq str *))

(defcheck solution-54accb20
  (fn [x y]
    (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-54c33a5a
  (fn [a b]
    (reverse (map #(mod % 10) (take-while (partial < 0) (iterate #(quot % 10) (* a b)))))))

(defcheck solution-5501f1f7
  (fn [a b] (map #((zipmap "0123456789" (range)) %) (str (* a b)))))

(defcheck solution-550cfd9c
  (comp (fn [n] (map #(parse-int (str %)) (seq (str n)))) *))

(defcheck solution-551c7e3f
  (fn [x y]
    (map #(- (int %) (int \0)) (seq (str (* x y))))))

(defcheck solution-552e3309
  (comp (partial map #(parse-int (str %))) str * ))

(defcheck solution-5537f98b
  (comp (partial map #(parse-int (str %))) str *))

(defcheck solution-5680308b
  (fn [x y]
    (map #(parse-int %) (reduce conj [] (map str (str (* x y)))))))

(defcheck solution-56bfc798
  (fn [& args] (map #(parse-int (str %)) (str (apply * args)))))

(defcheck solution-56c27ddd
  (fn [a b]
    (map #(- (int %) 48) (seq (str (* a b))))
    ))

(defcheck solution-56ed9bf0
  (fn [& args]
    (loop [ r [] a (apply * args)]
      (if (< a 10)
        (reverse (conj r a))
        (let [b (rem a 10 )]
          (recur (conj r b) (quot (- a b ) 10)))))))

(defcheck solution-573edba1
  (fn [a b]
    (loop [m (* a b), r '()]
      (if (zero? m)
        r
        (recur (quot m 10) (conj r (rem m 10)))))
    ))

(defcheck solution-57605a
  #(->> (* %1 %2)
     str
     (map (fn [c] (- (int c) 48)))))

(defcheck solution-57e1264e
  #((comp reverse
          (fn it [x]
            (if (< 0 x)
              (cons (rem x 10) (it (quot x 10))))))
    (* % %2)))

(defcheck solution-58a98830
  (fn [a b]
    ((fn d [n]
       (loop [n n acc []]
         (if (zero? n)
           acc
           (conj (d (quot n 10)) (rem n 10)))))
     (* a b))))

(defcheck solution-58d9fc92
  (fn [x y]
    (map #(parse-int (str %)) (into [] (str(* x y))))))

(defcheck solution-59e078a7
  (fn [n1 n2]
    (map #(- (int %) (int \0)) (str (* n1 n2)))))

(defcheck solution-5b6991f5
  (fn [x y]
    (->>
      (* x y)
      (str )
      (seq)
      (map (fn [c] (parse-int (str c)))))))

(defcheck solution-5bd2eece
  (fn [a b]
    (map #(-> % char str parse-int) (str (* a b)))))

(defcheck solution-5c7c0db6
  (fn [x y]
    (let [number (* x y)]
      (loop [number number
             result ()]
        (if (< number 10)
          (conj result number)
          (recur
            (quot number 10)
            (conj result (rem number 10))))))))

(defcheck solution-5cf9c897
  (fn [x y]
    (reverse
      ((fn digits [n]
         (if (< n 10)
           [n]
           (cons (rem n 10) (digits (int (/ n 10))))))
       (* x y)))))

(defcheck solution-5d0102a0
  (fn [a b]  (map #(parse-int (str %)) (vec (str (* a b))))))

(defcheck solution-5d32ee5c
  (fn [a b]
    (map read-string (re-seq #"\d" (str (* a b))))))

(defcheck solution-5da30d72
  (fn [x y]
    (map #(character-digit % 10) (str (* x y)))
    ))

(defcheck solution-5dfb4755
  (fn [a b]
    (loop [n (* a b)
           digits []]
      (if (= n 0)
        digits
        (recur (-> n (/ 10) int) (cons (mod n 10) digits) )))))

(defcheck solution-5e1e2b3e
  (fn [x y] (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} (seq (str (* x y))))))

(defcheck solution-5e3235a
  (comp
   (fn decompose [n]
     (if (< n 10)
       [n]
       (conj (decompose (quot n 10)) (rem n 10))))
   *))

(defcheck solution-5e60c8c0
  (fn [a b](map #(- (int %) 48) (str( * a b)))))

(defcheck solution-5ec29897
  (fn [x y]
    (loop [r [], n (* x y)]
      (if (= n 0)
        r
        (recur (cons (mod n 10) r) (quot n 10)))
      )
    ))

(defcheck solution-5ee29364
  (fn [x y] (map #(- (int %) (int \0)) (str (* x y)))))

(defcheck solution-5f6007c1
  (fn [x y]
    (let [prod (* x y)]
      (if (zero? prod)
        [0]
        (loop [p prod, res ()]
          (if (zero? p)
            res
            (recur (quot p 10) (conj res (rem p 10)))
            ))))))

(defcheck solution-601f7f78
  (fn product[x y]
    (let[n (str (* x y))]
      (map #(- (int %) 48) n))))

(defcheck solution-60fc5cc9
  #(map (comp read-string str) (str (* %1 %2))))

(defcheck solution-611447bb
  #(for [x (str (* %1 %2))] (parse-int (str x))))

(defcheck solution-6181b78e
  (fn [x y] (map #(parse-int (.toString %)) (str (* x y)))))

(defcheck solution-619e725d
  #(map (comp read-string str) (str (* % %2))))

(defcheck solution-61f8a387
  (fn product-digits
    [a b]
    (mapv #(- (int %) 48) ((comp str *) a b))))

(defcheck solution-625a1bb3
  #((fn dg [n]
      (if (zero? (quot n 10))
        [n]
        (conj (dg (quot n 10))
          (mod n 10)))) (* %1 %2) ))

(defcheck solution-627c8c28
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-62df28c4
  (fn digits [x y] ((fn prdi [z] (if (empty? z) [] (concat (list (- (int (first z)) 48)) (prdi (rest z))))) (str (* x y)))))

(defcheck solution-635e86f7
  (fn [a b]
    (->> (* a b)
      str
      seq
      (map #(- (int %) 48)))))

(defcheck solution-63826c2f
  (fn [n m]
    (map #(parse-int %) (re-seq #"." (str (* n m))))))

(defcheck solution-63b4eb5b
  (fn [x y]
    seq (map #(- (int %) (int \0))
          (str (* x y)))))

(defcheck solution-649e3cc6
  (fn [num1 num2]
    (loop [prod (* num1 num2) curprod (* num1 num2) cur 10 ret []]
      (if (> cur prod)
        (cons (rem curprod 10) ret)
        (recur
          prod
          (/ (- curprod (rem curprod 10)) 10)
          (* cur 10)
          (cons (rem curprod 10) ret)
          )
        )
      )
    ))

(defcheck solution-650e5723
  #(map (fn [x] (- (int x) 48)) (str(reduce * %&))))

(defcheck solution-6534f1
  #((fn get-digits[number coll](if (> number 0)(conj (get-digits (int (/ number 10)) coll) (mod number 10)) coll)) (* %1 %2) []))

(defcheck solution-6595fb95
  (fn [a b]
    (map str (str (str (* a b))))
    (vec (for [i (range 0 (count (map str (str (* a b)))))]
           (parse-int (nth (map str (str (str (* a b)))) i))))))

(defcheck solution-66027216
  (fn [n m]
    (loop [x (* n m) result '()]
      (if (< x 10)
        (conj result x)
        (recur (int (/ x 10)) (conj result (rem x 10)))))))

(defcheck solution-665e60c4
  (fn [x y]
    (map #(parse-int (str %))
      (str (* x y)))))

(defcheck solution-667b3439
  (fn [x y]
    (->> (* x y)
      str

      (map int)
      (map #(- % 48)))))

(defcheck solution-6697e8ad
  (fn [a b]
    (let [c (* a b)]
      (loop [v c
             acc nil]
        (if (zero? v)
          acc
          (recur (int (/ v 10)) (conj acc (rem v 10))))))))

(defcheck solution-66aaf2a7
  (fn [a b]
    (map #(character-digit % 10) (str (* a b)))))

(defcheck solution-670c8995
  (fn [x y] (map #(parse-int (.toString %)) (flatten (partition 1 (str (* x y)))))))

(defcheck solution-671889f4
  (fn [& v] (map read-string (re-seq #"\d" (str (apply * v))))))

(defcheck solution-67b32c92
  (fn [x y] (map #(parse-int (str %)) (seq (str (* x y))))))

(defcheck solution-681ce01b
  (fn [a b]
    (->> (* a b)
      (.toString)
      (map #(get-numeric-value %) )
      (into [])
      )
    ))

(defcheck solution-686f2f7c
  (fn m [a b]
    (map #(-> % str read-string)
      (-> (* a b) str seq))))

(defcheck solution-68a3a4c8
  (fn [n1 n2] (map #(- (int %) (int \0)) (seq (str (* n1 n2))))))

(defcheck solution-68bbbad4
  (fn [& coll](map #(- (int %) 48) (seq (str (reduce * coll))))))

(defcheck solution-69a185ec
  (fn [a b] (map #(- (int %) (int \0)) (str (* a b)))))

(defcheck solution-69e424b4
  (fn [x y]
    (let [sum (* x y)]
      (map #(- (int %) (int \0)) (-> sum str seq)))))

(defcheck solution-69edd3f7
  (fn multi-list [x y]
    (letfn [(char-2-int [c] (- (int c) (int \0)))]
      (map char-2-int (str (* x y))))))

(defcheck solution-69f4589d
  (fn [x y]
    (let [to-digits (fn to-digits [n digits]
                      (if (zero? n)
                        digits
                        (to-digits (quot n 10) (cons (rem n 10) digits))))]
      (to-digits (* x y) nil))))

(defcheck solution-6a4d0d81
  (fn [a b]
    (map
      #(- (int %) 48) (str (* a b)))))

(defcheck solution-6a768f56
  #(map (fn [x] (- (int x) 48)) (seq (str (* % %2)))))

(defcheck solution-6a82e025
  #(map read-string (re-seq #"." (str (* % %2)))))

(defcheck solution-6b8361fe
  (fn [a b] (map #(parse-int %) (filter #(not (clojure.string/blank? %)) (clojure.string/split (str (* a b)) #"")))))

(defcheck solution-6c399b53
  (fn [x y] (read-string (clojure.string/join " " (str "[" (* x y) "]")))))

(defcheck solution-6c7c4f55
  (fn [a b] (

             ; from my solution for Q#137: Digits and bases
             (fn [n b]
               (if (zero? n)
                 (vector n)
                 ((fn [n b r]
                    (if (zero? n)
                      r
                      (recur (quot n b) b (conj r (rem n b))))) n b nil)))
             (* a b) 10)))

(defcheck solution-6cb86504
  (fn digits [& args]
    (let [p (reduce * args)]
      (loop [n p d `()]
        (let [d (cons (rem n 10) d)
              n (quot n 10)]
          (if (zero? n) d (recur n d)))))))

(defcheck solution-6cc6008d
  (fn [& args]
    (->> args
      (apply *)
      str
      seq
      (map #(parse-int (str %))))))

(defcheck solution-6d0f498e
  (fn [a b]
    (loop [i (* a b) s '()]
      (if (= i 0)
        s
        (recur (int (/ i 10)) (conj s (mod i 10))))
      )
    ))

(defcheck solution-6d8abefd
  (fn [f s]
    (let [n (* f s)]  (reverse
                        (for [y (iterate (partial * 10) 1) :while (<= y n)]
                          (rem (int (/ n y)) 10))))))

(defcheck solution-6e7a261e
  (fn [a b]
    (for [c (str (* a b)) :let [d (- (int c) (int \0))] ] d)))

(defcheck solution-6e7f02f4
  (fn dseq [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-6e8a5c66
  (fn [a b] (vec (map #(read-string (str %)) (str (* a b))))))

(defcheck solution-6e932128
  #(let [r10 (range 10)] (map (zipmap (seq (apply str r10)) r10) (str (* %1 %2)))))

(defcheck solution-6eaab5da
  #(map (fn [x] (parse-int (str x))) (seq (str (* % %2)))))

(defcheck solution-6f248783
  (fn digitize
    [a b]
    (map read-string (map str (vec (str (* a b)))))))

(defcheck solution-6f2e2968
  (fn multi-n2v [n1 n2]
    (let [pow (fn [n1 n2] (apply * (repeat n2 n1)))
          n2v (fn [n]
                (vec
                  (reverse
                    (for [i (range (count (str n)))]
                      (rem (quot n (pow 10 i)) 10)))))]
      (n2v (* n1 n2)))))

(defcheck solution-70b1a62f
  (fn [x y]
    (
     (fn todigit [z] (if (= z 0) [] (conj (todigit (int (/ z 10))) (mod z 10))))
     (* x y)
     )))

(defcheck solution-70b907ea
  (fn [a b]
    (map #(parse-int %) (map str (str (* a b))))))

(defcheck solution-70bb2f31
  (fn [x y]
    (map #(- (int %) 48) (seq (str (* x y))))))

(defcheck solution-70ef48e5
  #(letfn [
           (nums [x r]
             (if (= x 0)
               r
               (recur (quot x 10) (cons (rem x 10) r))))]
     (nums (* %1 %2) [])))

(defcheck solution-71223c49
  (fn [a b] (map read-string (map str (vec (str (* a b)))))))

(defcheck solution-714f95f9
  #(loop [n (* %1 %2) x '()]
     (let [_n (int (/ n 10)) _x (conj x (mod n 10))]
       (if (= 0 _n)
         _x
         (recur _n _x)
         )
       )
     ))

(defcheck solution-71801a92
  (fn[a,b](map #(read-string (str %)) (seq (str (* a b))))))

(defcheck solution-71cf447f
  (fn [x y] (vec (map #(character-digit % 10) (str (* x y))))))

(defcheck solution-71dc8cf1
  (comp vec (partial map #(parse-int %)) (partial remove empty?) #(clojure.string/split % #"") str *))

(defcheck solution-71e111b7
  (fn [a b]
    (reverse
      ((fn func [num]
         (let [q (quot num 10)
               r (- num (* (quot num 10) 10))]
           (if (= 0 q)
             (list r)
             (cons r (func q)))))
       (* a b)))))

(defcheck solution-71fb2bfe
  (fn prod-digits [x y]
    (->> x
      (* y)
      (str)
      (map #(character-digit % 10))
      (into []))))

(defcheck solution-720127f5
  #(->> (apply * %&)
     str
     (map int)
     (map (partial + -48))))

(defcheck solution-72229591
  (fn il [n m]
    (loop [i (* n m) l '()]
      (if (< (quot i 10) 1)
        (conj l i)
        (recur (quot i 10) (conj l (rem i 10)))))))

(defcheck solution-725cb651
  (fn [a b]
    (->> (* a b) (.toString) (map #(.toString %)) (map #(parse-int %)))))

(defcheck solution-72a60f35
  (fn decomp [x y]
    (let [f (* x y) xs (quot f 10) digit (mod f 10)]
      (if (= 0 xs)
        (vector digit)
        (conj (decomp 1 xs) digit)))))

(defcheck solution-72f96175
  (fn [a b]
    (letfn [(step [i]
              (if (pos? i)
                (cons (rem i 10) (step (quot i 10)))))]
      (reverse (step (* a b))))))

(defcheck solution-735ced01
  #(reverse(
            (fn k [x]
              (if (= x 0)
                '()
                (conj (lazy-seq (k (int (/ x 10)))) (rem x 10))))
            (* %1 %2))))

(defcheck solution-73c35e01
  (fn [a b]
    (reverse
      (
       (fn digits [n]
         (if (zero? n)
           []
           (list*
             (rem n 10)
             (digits (quot n 10))
             )
           )
         )
       (* a b)
       )
      )
    ))

(defcheck solution-7427663
  #(->> (* %1 %2)
     str seq
     (map str)
     (map read-string)))

(defcheck solution-74d80b2b
  (fn [x y] (reduce (fn [a b] (conj a (character-digit b 10))) [] (str (* x y)))))

(defcheck solution-750394be
  (fn [a b] (map #(- (int %) (int \0)) (str (* a b))) ))

(defcheck solution-75bc55c2
  (fn [x y]
    (map #(get-numeric-value %) (str (* x y)))))

(defcheck solution-75bc8fd2
  (fn [a b] (map #(get-numeric-value %) (into [] (str (* a b))))))

(defcheck solution-7660baf0
  (fn [x y] (map (comp #(- % 48) int) (str (* x y)))))

(defcheck solution-7732dbd
  (fn [num1 num2]
    (let [char-seq (seq (str (* num1 num2)))]
      (map #(- (int %) (int \0)) char-seq))))

(defcheck solution-773a1acf
  (comp (partial map #(parse-int (str %))) seq str *))

(defcheck solution-775204b
  (fn [x y]
    (->> (* x y)
      str
      (map #(parse-int (str %))))))

(defcheck solution-775c3d26
  (fn mult-digit-str [x y]
    (->> (* x y)
      (str)
      (map #(- (int %) 48)))))

(defcheck solution-777ffb2a
  (fn [a b] (->> (* a b) str (map #(- (int %1) 48)))))

(defcheck solution-77ca07b8
  (comp
   (fn digits [x]
     (if (< x 10) [x] (conj (digits (quot x 10)) (mod x 10))))
   *))

(defcheck solution-77fa1ee9
  (fn [a b] (map #(- (int %1) (int \0))  (seq (str (* a b))))))

(defcheck solution-7841cee8
  (fn [a b]
    (map #(parse-int (str %))
      (.toString (* a b)))))

(defcheck solution-785e5039
  (fn [x y]
    (loop [n (* x y) result []]
      (if (< n 10)
        (cons n result)
        (recur (quot n 10) (cons (rem n 10) result))))))

(defcheck solution-7902e1f4
  (fn [x y] (map (comp #(parse-int %) str) (seq (str (* x y))))))

(defcheck solution-7947b6be
  (comp (partial map #(- (int %) 48))
        str *))

(defcheck solution-7991d4a1
  (fn [x y](map #(- (int %) 48) (str (* x y)))))

(defcheck solution-7a107f78
  (fn [x,y] (map #(parse-int (str %)) (vec(str(* x y))) )))

(defcheck solution-7a9bdf8f
  (fn [x y ] (map (fn [z] (- (int z) 48))  (str (* x y))  )  ))

(defcheck solution-7ab60bec
  (fn [& args]
    (map #(- (int %) (int \0)) (seq (str (apply * args))))))

(defcheck solution-7ab9fa02
  (fn [a b]
    (->> (* a b)
      str
      seq
      (map str)
      (map #(parse-int %)))))

(defcheck solution-7acac0ed
  (fn
    [n1 n2]

    (map #(get-numeric-value %) (str (* n1 n2)))



    ))

(defcheck solution-7af4c807
  (fn seqDigits
    [a b]
    (let [splitDigits (fn splitD [x]
                        (loop [myX x
                               result '()]
                          (if (= myX 0)
                            result
                            (recur (int (/ myX 10)) (conj result (mod myX 10))))))]
      (splitDigits (* a b)))))

(defcheck solution-7be63ea5
  #(map (comp read-string str) (seq (str (* %1 %2)))))

(defcheck solution-7d3a17b3
  (fn digits [n1 n2]
    (map #(- % 48) (map int (.toString (* n1 n2))))))

(defcheck solution-7d48191e
  (fn product-digits [a b]
    (let [divisor 10]
      (loop [product (* a b), result []]
        (if (= 0 product)
          result
          (recur (quot product divisor) (cons (rem product divisor) result)))))))

(defcheck solution-7d912389
  (fn [x y]
    (->> (* x y)
      str
      (map #(- (int %) (int \0))))))

(defcheck solution-7dc7eb6a
  (fn [a b]
    (loop [i (* a b) s []] (if (> i 0) (recur (quot i 10) (cons (mod i 10) s)) s))))

(defcheck solution-7dd547c7
  (fn [n m]
    (->> (* n m) str
      (map  #(parse-int (str %) )))))

(defcheck solution-7ded6f8
  (fn digit-product [x y]
    (map #(- (int %) (int \0)) (str (* x y)))))

(defcheck solution-7ed8fd95
  (fn [x y]
    (map
      #(get-numeric-value %)
      (str (* x y)))))

(defcheck solution-7f2e6c09
  (fn [x y]
    (->> (* x y)
      (str)
      (seq)
      (map #(- (int %) 48)))))

(defcheck solution-7f9254d2
  (fn [a b]
    (loop [n (* a b)
           digits '()]
      (if (> n 0)
        (recur (int (/ n 10)) (cons (mod n 10) digits))
        digits))))

(defcheck solution-8013ed2f
  #(for [e (str (* % %2))] (- (int e) 48)))

(defcheck solution-80d7415f
  (fn [m n] (map #(parse-int (str %)) (str (* m n)))))

(defcheck solution-80d86ff8
  (fn product-digits
    [m n]
    (let [product (* m n)]
      ((fn digits
         [n xs]
         (if (zero? n)
           xs
           (digits (quot n 10) (cons (rem n 10) xs))))
       product []))))

(defcheck solution-80e6e5ca
  (fn [x y]
    (drop-while zero? (for [i (take 10 (iterate #(/ % 10) 1000000000))]
                        (if (> i 0)
                          (mod (quot (* x y) i) 10))))
    ))

(defcheck solution-81280a80
  (fn [x y]
    (->> (* x y)
      (iterate #(quot % 10))
      (take-while (complement zero?))
      (map #(mod % 10))
      reverse)))

(defcheck solution-814f5e9b
  (fn [x y]
    (loop [num (* x y)
           digits []]
      (if (zero? num)
        (reverse digits)
        (recur (quot num 10) (conj digits (mod num 10)))))))

(defcheck solution-818ad824
  (fn [n1 n2]
    (map #(parse-int (str %)) (str (* n1 n2)))))

(defcheck solution-81aa227d
  (fn [a b]
    (loop [n (* a b)
           r '()]
      (if (< n 10)
        (conj r n)
        (recur (Math/round (Math/floor (/ n 10.0))) (conj r (mod n 10)))))))

(defcheck solution-81e0ebeb
  (fn [& args] (map #(- (int %) (int \0)) (str (apply * args)))))

(defcheck solution-81f9230f
  (fn [& args] (map #(- (int %) (int \0)) (seq (str (apply * args))))))

(defcheck solution-821281c3
  (fn [a b] (map #(parse-int %) (re-seq #"\d" (str (* a b))))))

(defcheck solution-82532505
  (fn [a b]
    (loop [current (* a b) res []]
      (if (zero? current)
        (reverse res)
        (recur (quot current 10) (conj res (mod current 10)))))))

(defcheck solution-8303cfeb
  #(->> (* % %2)
     str
     seq
     (map str)
     (map read-string)))

(defcheck solution-83104750
  #(for [c (str (* % %2))] (- (int c) 48)))

(defcheck solution-83f19934
  (fn [x y]
    (let [r (* x y)]
      (map #(parse-int %) (re-seq #"\d" (str r))))))

(defcheck solution-842c9ff3
  (fn [a b]
    (map #(get-numeric-value %) (seq (str (* a b))))))

(defcheck solution-84ba86f8
  (fn [a b]
    (map #(character-digit % 10) (seq (str (* a b))))))

(defcheck solution-84f15ac7
  (fn [x y]
    (map read-string (re-seq #"\d" (str (* x y))))))

(defcheck solution-852f69f7
  (fn [a b]( map-indexed (fn[idx itm](- (int itm) 48)) (str(* a b)) )))

(defcheck solution-8598fb8
  (comp (partial map (comp (partial + -48) int)) vec str *))

(defcheck solution-85c33704
  (fn [x y] (letfn [(digits [number]
                      (loop [n number d '()]
                        (if (zero? n)
                          d
                          (recur (quot n 10) (conj d (rem n 10)))
                          )
                        )
                      )]
              (digits (* x y)))))

(defcheck solution-8637f0fc
  (fn [a b]
    (map #(character-digit % 10) (str (* a b)))
    ))

(defcheck solution-8684b356
  #(map (fn [x] (-> x int (+ -48))) (str (* % %2))))

(defcheck solution-8684f181
  (fn [x y]
    (->> (* x y) str (map (comp #(parse-int %) str)))))

(defcheck solution-86b29d16
  #((fn r [x] (if (zero? x) [] (conj (r (quot x 10)) (rem x 10)))) (* %1 %2)))

(defcheck solution-878cf498
  #(let [n (* % %2)]
     (loop [a [] r n]
       (if (< r 10)
         (cons r a)
         (recur (cons (rem r 10) a) (quot r 10))))))

(defcheck solution-88deeac4
  (fn stuff [a, b] (reverse ((fn dig [n] (if (< n 10) [n] (cons (mod n 10) (dig (quot n 10))))) (* a b)))))

(defcheck solution-88defada
  (fn [x y] (vec (map #(parse-int (str %)) (flatten (partition 1 (str (* x y))))))))

(defcheck solution-89261ff0
  (fn int2seq
    ([n]
     (if (= n 0) nil
                 (cons (mod n 10) (int2seq (int (/ n 10))))))
    ([n1 n2] (reverse (int2seq (* n1 n2))))))

(defcheck solution-894822c0
  (fn  [x y]
    (into [] (map #(-> % str read-string) (seq (str (* x y)))))))

(defcheck solution-89685194
  (fn [n m] (map (fn [c] (get-numeric-value c)) (str (* n m)))))

(defcheck solution-89721215
  #((fn digits-of [x]
      (if (zero? x)
        []
        (conj
          (digits-of (quot x 10))
          (mod  x 10)))) (* % %2)))

(defcheck solution-89b3ec86
  (fn *-seq [m n]
    (loop [i (* m n), ans ()]
      (if (zero? i)
        ans
        (recur (quot i 10) (cons (mod i 10) ans))
        )
      )
    ))

(defcheck solution-89e99a87
  (fn [a b] (map #(- (int %) 48) (seq (str (* a b))))))

(defcheck solution-8a02d118
  (fn [n1 n2]
    (loop [x (* n1 n2)
           ret []]

      (let [dx (int (/ x 10))
            ld (- x (* dx 10))
            ]

        (if (>= x 1)
          (recur dx (cons ld ret))
          ret)))))

(defcheck solution-8a6adb3d
  (fn [x y]
    (map #(character-digit % 10) (str (* x y)))))

(defcheck solution-8a7d4f70
  (fn digits [x y]
    (map (comp read-string str) (seq (str (* x y))))))

(defcheck solution-8aae9568
  (fn pdigits [x y]
    (loop [prod (* x y) digits '()]
      (let [r (rem prod 10) q (quot prod 10)]
        (if (zero? q)
          (cons r digits)
          (recur q (cons r digits))
          )
        )
      )
    ))

(defcheck solution-8afde937
  #(for [d (str (* % %2))]
     (- (int d) 48)))

(defcheck solution-8aff1a8f
  (fn [x y]
    (->> (* x y) str (map (comp read-string str)))))

(defcheck solution-8b901340
  (fn [l r] (loop [n (* l r) v []]
              (if (zero? n)
                (reverse v)
                (recur (quot n 10) (conj v (rem n 10)))))))

(defcheck solution-8bb1f915
  (fn tf [a b]
    (let [m (* a b)]
      (reverse (map #(rem % 10) (take-while (partial < 0) (iterate #(quot % 10) m)) )))))

(defcheck solution-8be5bd28
  (fn [a b]
    ((fn [x acc]
       (let [acc' (cons (mod x 10) acc)]
         (if (< x 10) acc'
                      (recur (quot x 10) acc')))) (* a b) '())))

(defcheck solution-8c0a4336
  (fn [a b]
    (loop [n (* a b) digits '()]
      (if (zero? n) digits
                    (recur (/ (- n (rem n 10)) 10) (conj digits (rem n 10)))))))

(defcheck solution-8c11d21
  (fn product-digits [x y]
    (->> (* x y)
      str
      (map #(parse-int (str %))))))

(defcheck solution-8c1aefe6
  (fn [& args]
    (let [parse-int (fn [s] (parse-int (str s)))]
      (map parse-int (str (apply * args))))))

(defcheck solution-8c2dffb7
  (letfn [(digs [n] (if (= n 0) [] (conj (digs (quot n 10)) (mod n 10))))]
    #(digs (* %1 %2))))

(defcheck solution-8c2ed88a
  (fn [a b] (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-8ca6b308
  (fn [x y] (->> (* x y) str (map int) (map #(- % 48)) vec)))

(defcheck solution-8cae08b2
  (fn [x y] (reverse (map #(mod % 10) (take-while (complement zero?) (iterate #(quot % 10) (* x y)))))))

(defcheck solution-8cc7c791
  #(map read-string (map str (seq (str (* % %2))))))

(defcheck solution-8d1692c6
  (fn [a b]
    ((fn [x r]
       (cond
         (zero? x) (if (empty? r) [0] r)
         :else     (recur (quot x 10) (cons (mod x 10) r))))
     (* a b) [])))

(defcheck solution-8d34949
  (fn [& args] (map #(- (int %) (int \0)) (-> (apply * args) str seq))))

(defcheck solution-8d3f03e
  (fn [n1 n2]
    (loop [answer []
           x (* n1 n2)]
      (let [nextans (conj answer (mod x 10))
            nextval (int (/ x 10))]
        (if (> nextval 1)
          (recur nextans nextval)
          (reverse nextans))))))

(defcheck solution-8d9da8f9
  #(loop [n (* %1 %2) s []] (if (pos? n) (recur (quot n 10) (cons (mod n 10) s)) s)))

(defcheck solution-8dc2fd38
  (fn [a b]
    (map
      #(- (int %) 48)
      (str (* a b)))))

(defcheck solution-8de9c88c
  (fn [x y]
    (->> (* x y)
      (str)
      (map #(read-string (str %))))
    ))

(defcheck solution-8e305eb5
  (fn mu [x y]
    (map (comp #(- % (int \0)) int)
      (seq (str (* x y))))))

(defcheck solution-8e83c62b
  (fn [x y]
    (if (= (* x y) 0)
      [0]
      (loop [prod (* x y)
             digits (list)]
        (if (= prod 0)
          (vec digits)
          (recur (quot prod 10) (conj digits (rem prod 10))))))))

(defcheck solution-8e93e05f
  (fn [a b]
    (loop [digits []
           n (* a b)]
      (if (zero? n) digits
                    (recur (cons (rem n 10) digits) (quot n 10))
                    ))))

(defcheck solution-8f21c4f2
  (fn [a b]  ((fn [n s] (if (zero? n) s (recur (quot n 10) (cons (rem n 10) s)))) (* a b) [])))

(defcheck solution-8f731fde
  (fn [a b]
    (map #(- (int %) 48) (seq (str (* a b))))))

(defcheck solution-8f95e26a
  (fn [a b]
    (vec
      (#(map (fn [i] (- (int i) 48)) (str %)) (* a b)))))

(defcheck solution-8f9b7974
  (fn [a b]
    (let [x (* a b)]
      (reverse (take (inc (int (Math/log10 x)))
                 (map
                   #(mod (int (/ x %)) 10)
                   (iterate
                     (partial * 10)
                     1)))))))

(defcheck solution-8fdb201f
  #((fn sq[n]
      ( if (>= n 10) (concat (sq (quot n 10)) [(rem n 10)]) [n])
      )

    (* %1 %2)))

(defcheck solution-900c0050
  (fn [x y]
    (map #(- (int %) (int \0)) (str (* x y)))))

(defcheck solution-900e2c52
  (fn [i1 i2]
    (map
      #(parse-int (str %))
      (str (* i1 i2)))))

(defcheck solution-90645731
  (fn [x y]
    (map #(- (int %) 48) (seq (str (* x y))))))

(defcheck solution-90ed9847
  (fn prodigs [x y]
    (reduce #(conj %1 (int (- (int %2) (int \0)))) [] (str (* x y)))))

(defcheck solution-90f2b003
  (fn [& args] (map #(- (int %) 48) (seq(str(apply * args))))))

(defcheck solution-914ad4a3
  (fn[a b]
    (->> (* a b) str (map #(character-digit % 10)))))

(defcheck solution-9165789a
  (fn [x y]
    (loop [n (* x y)
           r []]
      (if (< n 10)
        (vec (reverse (conj r n)))
        (recur (int (/ n 10))
          (conj r (mod n 10)))))))

(defcheck solution-923e4789
  (fn f
    [num1 num2]
    (let [split #(map str %)
          parse #(parse-int %)]
      (->> (* num1 num2) str split (map parse)))))

(defcheck solution-925c8a4
  #(map (fn [a] (- (int a) 48)) (str (* % %2))))

(defcheck solution-92beef02
  #(loop [ret '() n (* % %2)] (if (= n 0) (vec ret) (recur (cons (mod n 10) ret) (quot n 10)))))

(defcheck solution-93530bd7
  (fn [x y]
    ((fn d [m]
       (if (< m 10)
         [m]
         (conj (d (quot m 10))
           (rem m 10)))) (* x y))))

(defcheck solution-93648f71
  (comp (partial #(if (> %2 0) (recur (cons (mod %2 10) %1) (int (/ %2 10))) %1) ()) *))

(defcheck solution-9388864b
  (fn [& args](->> args (apply *) str seq (map str) (map #(parse-int %)))
    ))

(defcheck solution-939a8679
  (fn prod-dig [x y]
    (loop [res '() prod (* x y)]
      (if (> prod 0)
        (recur (conj res (rem prod 10)) (quot prod 10))
        res))))

(defcheck solution-9429abb9
  (fn product-digits [a b]
    (map #(get-numeric-value %) (str (* a b)))))

(defcheck solution-944acbcc
  (fn [a b] (map (comp read-string str) (str (* a b)))))

(defcheck solution-94b6cb36
  (fn [n1 n2]
    (loop [n (* n1 n2), c []]
      (if (< n 10)
        (cons n c)
        (recur (int (/ n 10)) (cons (rem n 10) c))))))

(defcheck solution-9528a87b
  #(map (fn [digit] (parse-int (str digit))) (seq (str (* % %2)))))

(defcheck solution-954f1cd7
  #(let [n (* %1 %2) go (fn go [n] (if (zero? n) [] (concat (go (int (/ n 10))) [(rem n 10)])))] (go n)))

(defcheck solution-957deadd
  (comp #(map (fn[x] (parse-int x)) %) #(map str %) seq str *))

(defcheck solution-96451f00
  (fn product-digits [n1 n2]
    (map #(- (int %) 48) (.toString (* n1 n2)))))

(defcheck solution-968f0a24
  (fn [x y]
    (let [results (* x y)]
      (->> results
        (iterate #(quot % 10))
        (take-while pos?)
        (map #(rem % 10))
        reverse))))

(defcheck solution-971f4234
  (fn [x y]
    (map #(- % (int \0)) (map int (str (* x y))))))

(defcheck solution-98aa797a
  (fn[a b]
    (map #(- (int %) (int \0)) (seq (str (* a b))))
    ))

(defcheck solution-98b88bec
  (fn [a b]
    (letfn [
            (ll[num]
              (loop [num num res []]
                (if (< 0 num)
                  (recur
                    (int (/ num 10))
                    (cons (mod num 10) res))
                  res)))]
      (ll (* a b)))))

(defcheck solution-99970adf
  #(let [xy (* %1 %2)
         a (fn [n acc]
             (if (< n 1)
               acc
               (recur (quot n 10)
                 (cons (rem n 10) acc)
                 )))]
     (a xy [])))

(defcheck solution-9a279705
  (fn [a b]
    (let [c (* a b)]
      (letfn [(t-div [x]
                (if (> 10 x)
                  (list x)
                  (cons (rem x 10) (t-div (quot x 10)))))]
        (into [] (reverse (t-div c)))))))

(defcheck solution-9ad24ad8
  (fn f
    ([m n] (f (* m n)))
    ([ x ] (if (< 0 x)
             (conj (f (int (/ x 10))) (mod x 10))
             [] ))))

(defcheck solution-9af97b02
  #(map read-string (map str (str (* %1 %2)))))

(defcheck solution-9b0e1442
  (fn [a b]
    (map (comp #(- % 48) int) (seq (str (* a b))))
    ))

(defcheck solution-9c3f6e0b
  #(loop [n (apply * %&) ret ()]
     (if (zero? n)
       ret (recur (quot n 10)
             (cons (mod n 10) ret)))))

(defcheck solution-9c41496
  (fn [a b]
    (->> (* a b)
      (str)
      (vec)
      (map str)
      (map #(parse-int %))
      )))

(defcheck solution-9c9938b3
  (fn [x y] (map #(parse-int %) (re-seq #"." (str (* x y))))))

(defcheck solution-9cf2e507
  (fn [n1 n2]
    (map #(- (int %) 48) (vec (str (* n1 n2))))))

(defcheck solution-9d61aa13
  (fn [a b] (->> (* a b) str (map #(character-digit % 10)))))

(defcheck solution-9d66b588
  (fn [x y]
    (->> (* x y)
      str
      seq
      (map str)
      (map read-string))))

(defcheck solution-9d67815
  (fn [a b]
    (map #(parse-int (str %))
      (str (* a b)))))

(defcheck solution-9d76c8f4
  (fn [x y]
    (letfn [(digit-sequence [n acc]
              (if (= n 0)
                acc
                (recur (quot n 10) (cons (mod n 10) acc))))]
      (digit-sequence (* x y) '()))))

(defcheck solution-9d967d10
  (fn [x y]
    (loop [n (* x y) digits []]
      (if (< n 10)
        (cons (int n) digits)
        (recur (quot n 10) (cons (int (rem n 10)) digits))))))

(defcheck solution-9d983239
  #(map (comp read-string str) (str (apply * %&))))

(defcheck solution-9d9d77c9
  (fn [x y] (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-9dd7634f
  (fn [a b] ((fn break-digit ([n] (reverse (break-digit [] n))) ([v n] (if (<= n 0) v (break-digit (conj v (rem n 10)) (quot n 10))))) (* a b))))

(defcheck solution-9f560744
  (fn *-to-string-coll
    [a b]
    (->> (* a b)
      str
      (map #(parse-int (str %))))))

(defcheck solution-9fd512a6
  (fn [a b] (->> (* a b) str vec (map int) (map #(- % (int \0))))))

(defcheck solution-a039ab65
  #(loop [num (* %1 %2) new_seq []]
     (if (zero? num)
       (reverse new_seq)
       (recur (int (/ num 10)) (conj new_seq (mod num 10)) )
       )
     ))

(defcheck solution-a06dd5f5
  (fn [a b]
    (loop [mul (* a b) ans '()]
      (if (= mul 0)
        (lazy-seq ans)
        (recur (quot mul 10) (conj ans (rem mul 10)))))))

(defcheck solution-a100a74d
  (fn [x y] (map #(character-digit % 10) (str (* x y)))))

(defcheck solution-a14f4498
  (fn [a b]
    (loop [n (* a b) r '()]
      (if (zero? n) (if (empty? r) [0] r)
                    (recur (int (/ n 10)) (conj r (mod n 10)))))))

(defcheck solution-a169480d
  (fn [x y]
    (let [xy (* x y)]
      (loop [ret '() xy xy]
        (if (== (quot xy 10) 0)
          (conj ret (rem xy 10))
          (recur (conj ret (rem xy 10))
            (quot xy 10)))))))

(defcheck solution-a173ed7e
  (fn [x y]
    (loop [n (* x y) d []]
      (if (zero? n)
        (reverse d)
        (recur (quot n 10) (conj d (rem n 10)))))))

(defcheck solution-a1cebe3f
  #(map read-string (map str (str (* % %2)))))

(defcheck solution-a20dea4c
  (fn [x y] (map #(mod % 10) (reverse (take-while pos? (iterate #(quot % 10) (* x y)))))))

(defcheck solution-a21e0be2
  (fn [a b] (vec (map #(- (int %) 48) (seq (str (* a b)))))))

(defcheck solution-a2a0acc3
  (fn [a b]
    (into [] (map #(read-string (str %))  (str (* a b))))
    ))

(defcheck solution-a2f492aa
  #(map (fn [x]
          (- (int x) (int \0)))
     (seq (str (* %1 %2)))))

(defcheck solution-a38ed28b
  #(for [d (str (* % %2))] (- (int d) 48)))

(defcheck solution-a4011394
  (fn [x y]
    (vec
      (map #(- (int %) (int \0)) (str (* x y))))))

(defcheck solution-a4a85a84
  #(loop [n (* %1 %2) res []]
     (if (zero? n)
       res
       (recur (quot n 10) (cons (rem n 10) res))
       )
     ))

(defcheck solution-a4b176e
  (fn [a b] (map (fn [c] (- (int c) (int \0))) (str (* a b)))))

(defcheck solution-a4c0fe01
  (fn[x y]
    (into []
      (map #(parse-int (re-find  #"\d+" % ))
        (map  str  (into [] (str (* x y))))
        )

      )
    ))

(defcheck solution-a50c1bd9
  (fn [a b]
    (let [p (* a b)]
      (#(if (<= %2 0) %1
                      (recur (conj %1 (mod %2 10)) (int (/ %2 10)))) '() p))))

(defcheck solution-a54ad5e1
  (comp (partial map read-string) (partial map str) str *))

(defcheck solution-a556affd
  (fn [a b]
    (map #(- (int %) (int \0))(seq (str (* a b))))))

(defcheck solution-a5d348dc
  #(->> (* %1 %2) str seq (map (comp read-string str))))

(defcheck solution-a6180db9
  (fn [a b] (map #(parse-int (str %)) (seq (str (* a b))))))

(defcheck solution-a6f6b554
  (fn [& xs]
    (map #(parse-int (str %))
      (apply list (str (apply * xs))))))

(defcheck solution-a6f7e45
  (fn [a b]
    (letfn [(digits [number]
              (reverse
                (map #(rem % 10)
                  (take-while #(> % 0)
                    (iterate #(quot % 10) number)))))]
      (digits (* a b)))))

(defcheck solution-a7207f2d
  (fn [n1 n2]
    (loop [ps (.toString (* n1 n2)), result []]
      (if (empty? ps)
        result
        (recur (rest ps) (conj result (parse-int (str (first ps)))))))))

(defcheck solution-a7be11e1
  (fn [a b]
    (map
      #(parse-int %)
      (map
        str
        (seq (str (* a b)))))))

(defcheck solution-a7de118a
  (fn [m n]
    ((fn [v result]
       (if (= v 0)
         (if (= (count result) 0) [v] result)
         (recur (quot v 10) (cons (mod v 10) result)))
       )
     (* m n) [])))

(defcheck solution-a879bc3e
  (fn [n1 n2] (vec (map #(read-string (str %)) (seq (str (* n1 n2)))))))

(defcheck solution-a9b5a620
  #(map read-string (re-seq #"\d" (str (* %1 %2)))))

(defcheck solution-a9de5ea
  #((fn ! [a] (if (zero? a) nil (concat (! (int (/ a 10))) (vector (rem a 10))))) (* % %2)))

(defcheck solution-a9f8d94c
  (fn [n m] (map #(parse-int %) (re-seq #"[0-9]" (str (* n m))))))

(defcheck solution-aa7c3bbb
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-ab347eaa
  (fn [x y]
    (map (comp read-string str) (seq (str (* x y))))))

(defcheck solution-ab635d7c
  #(loop [num (* %1 %2) coll []]
     (if (zero? num) (reverse coll)
                     (recur (quot num 10) (conj coll (rem num 10))))))

(defcheck solution-abf1efc5
  #(map (fn [x] (get-numeric-value x)) (str (* % %2))))

(defcheck solution-ac2d3302
  (fn [a b]
    (reverse
      ((fn this [x]
         (when (<= 1 x)
           (cons (mod x 10) (this (int (/ x 10))))))
       (* a b)))))

(defcheck solution-ac341ec9
  (fn [a b]
    (loop [r (* a b)
           c '()]
      (if (> r 9)
        (recur (quot r 10) (conj c (rem r 10)))
        (conj c r)))))

(defcheck solution-ac7faba0
  (fn
    [x y]
    (let [p (* x y)]
      (if (= p 0)
        (list 0)
        (loop [n p
               l '()]
          (if (= n 0)
            l
            (recur (quot n 10) (conj l (mod n 10)))))))))

(defcheck solution-ac802b7c
  (fn [x y]
    (loop [n (* x y)
           acc []]
      (if (zero? n)
        acc
        (let [n-next (quot n 10)
              digit (mod n 10)]
          (recur n-next (cons digit acc)))))))

(defcheck solution-ac8f50eb
  (fn [x1 x2]
    (->> [x1 x2]
      (apply *)
      (str)
      (reduce (fn [acc x]
                (conj acc (get-numeric-value x)))
        []))))

(defcheck solution-ac9ebd46
  (fn my-product-digit
    [d1 d2]
    (->> (* d1 d2) (str) (map #(character-digit % 10)))))

(defcheck solution-acd246fa
  (fn [& xs]
    (map
      #(- (int %) (int \0))
      (-> (apply * xs) str seq))))

(defcheck solution-ad8e6bcd
  (fn [a b] (map #(- (int %)(int \0) )  (str (* a b)) )))

(defcheck solution-ae3a00b9
  (fn p99 [x y] (map #(- (int %) 48) (str (* x y)))))

(defcheck solution-ae5c5f8d
  (fn [x y]
    (letfn [(num-to-seq [n]
              (loop [res () n n]
                (if (zero? n) res
                              (recur (conj res (rem n 10)) (quot n 10)))))]
      (num-to-seq (* x y)))))

(defcheck solution-ae66b7a1
  (comp (fn [s] (map #(- (int %) 48) s)) seq str *))

(defcheck solution-af39ab2e
  #(map (fn[c] (- (int c) (int \0))) (seq (str (* %1 %2)))))

(defcheck solution-af4d5f20
  (fn [n1 n2]
    (loop [res []
           m (* n1 n2)]
      (if (= 0 m)
        res
        (let [f (mod m 10)]
          (recur
            (cons f res)
            (/ (- m f) 10))))
      )))

(defcheck solution-afb2b1bd
  (fn product-digits [x y]
    (letfn [
            (digits [x]
              (if (< x 10)
                [x]
                (conj (digits (int (/ x 10))) (mod x 10))))]
      (digits (* x y)))))

(defcheck solution-aff66bc4
  (fn
    [a b]
    (vec (map #(parse-int (str %1)) (seq (str (* a b)))))))

(defcheck solution-b05c0622
  (fn
    [x y]
    (map #(- (int %1) 48) (str (* x y)))))

(defcheck solution-b0dd8395
  (fn [a b] (apply vector (map (fn [char] (parse-int (str char))) (seq (str (* a b)))))))

(defcheck solution-b0f6c846
  (fn product-digits
    ([a b] (product-digits (* a b)))
    ([n]
     (if (zero? (quot n 10))
       [(mod n 10)]
       (conj (product-digits (quot n 10)) (mod n 10))))))

(defcheck solution-b1649ea2
  #(map (fn [x] (- (int x) (int \0))) (str (* % %2))))

(defcheck solution-b166fdb5
  (fn [m n]
    (loop [c (* m n)
           l ()]
      (if (= c 0)
        l
        (recur (int (/ c 10)) (conj l (mod c 10)))))))

(defcheck solution-b1b9e8ee
  (fn digitize [a b] (reverse (map #(mod % 10) (take-while #(not= 0 %) (iterate #(quot % 10) (* a b)))))))

(defcheck solution-b21630e
  (fn [x y] (vec (map #(parse-int (str %)) (str (* x y))))))

(defcheck solution-b24ac140
  (fn product-digits [a b]
    (letfn [(iter[x radix]
              (if(< x  radix)
                [x]
                ( conj  (iter (quot x radix) radix) (rem x radix))))]
      (iter (* a b) 10))))

(defcheck solution-b25c120f
  #(map read-string
     (map str (seq (str (* % %2))))))

(defcheck solution-b49d59c5
  (fn [x y]
    (let [digits #(map read-string (map str (seq (str %))))]
      (digits (* x y)))))

(defcheck solution-b55b3813
  (fn [a b] (map #(- (int %) (int \0)) (seq (str (* a b))))))

(defcheck solution-b5f3fb8c
  #(for [x (str (* % %2))] (- (int x) 48)))

(defcheck solution-b63e6d66
  (fn [x y]
    (->> (* x y) (iterate #(quot % 10)) (take-while pos?) (map #(mod % 10)) reverse)))

(defcheck solution-b6d08016
  (fn [a b] (map #(character-digit % 10) (str (* a b)))))

(defcheck solution-b725df4f
  (fn [ a b ]
    (->> (* a b) str (map str) (map read-string))))

(defcheck solution-b7f0d68
  (fn [x y]
    (->> (* x y)
      (str)
      (seq)
      (map #(-> (str %) (parse-int))))))

(defcheck solution-b816793a
  (fn [a b]
    (map #(parse-int (str %))  (str (* a b)))))

(defcheck solution-b896576c
  (fn [x y]
    (loop [prod (* x y) acc []]
      (if (zero? prod)
        acc
        (recur (quot prod 10) (cons (rem prod 10) acc))))))

(defcheck solution-b89f214b
  (letfn [(f [x]
            (cons (mod x 10) (if (> x 10) (f (quot x 10)) '())))]
    (fn [& args] (reverse (f (apply * args))))))

(defcheck solution-b9497062
  #((fn f [x] (if (zero? x) [] (conj (f (quot x 10)) (rem x 10)))) (* % %2)))

(defcheck solution-b95c6960
  #(->> (str (* % %2)) (map (fn [x](- (int x) 48)))))

(defcheck solution-b9d63163
  (fn [a b] (apply (fn dareducer [danum acc]
                     (if (> danum 0)
                       (dareducer (int (/ danum 10.0))
                         (cons (mod danum 10) acc))
                       acc)
                     ) [(* a b) []])
    ))

(defcheck solution-b9fe3489
  (fn product-digits
    [x y]
    (->> (* x y)
      str
      seq
      (map str)
      (map read-string))))

(defcheck solution-ba1f22d
  (fn [a b]
    (map #(- (int %) 48) (str (* a b)))))

(defcheck solution-ba507cc
  #(loop [digits ()
          n (apply * %&)]
     (if (= n 0) digits
                 (recur (cons (rem n 10) digits)
                   (quot n 10)))))

(defcheck solution-ba6a6033
  (fn [a b] (map #(parse-int %) (re-seq #"[0-9]" (str (* a b))))))

(defcheck solution-ba95e6e8
  #(loop [x (* % %2) digits '()]
     (if (zero? x)
       digits
       (recur (quot x 10)
         (conj digits (rem x 10))))))

(defcheck solution-bb521a59
  #(reverse ((fn digits [x] (when (not (zero? x)) (seq (cons (mod x 10) (digits (quot x 10)))))) (* %1 %2))))

(defcheck solution-bba1cf5e
  #(loop [x (* % %2) res ()]
     (if (zero? x)
       res
       (recur (quot x 10) (cons (rem x 10) res)))))

(defcheck solution-bbb03cc4
  (fn [a b] (map  #(- (int %) 48) (str (* a b)))))

(defcheck solution-bbd4add7
  (letfn [(digits [n]
            (let [[ds n]
                  (last (take-while
                          (comp (complement zero?) second)
                          (iterate
                            (fn [[ds n]] [(conj ds (mod n 10)) (int (/ n 10))])
                            [[] n])))]
              (conj ds n)))
          (solve [a b] (reverse (digits (* a b))))]
    solve))

(defcheck solution-bc0df0b9
  (fn pd ([x y] (pd [] (* x y) 10))
    ([digits n d] (if (> n 9) (pd (cons (mod n d) digits) (int (/ n d)) d) (cons n digits)))))

(defcheck solution-bc663515
  (fn [a b] (map #(parse-int (str %))  (str (* a b)))))

(defcheck solution-bc6e90b8
  (fn [a b]
    (let [prod (* a b)]
      (loop [n prod
             result []]
        (if (zero? n)
          result
          (recur (quot n 10) (cons (mod n 10) result)))))))

(defcheck solution-bcde455f
  #(map (fn [s] (parse-int (str s))) (str (* %1 %2))))

(defcheck solution-bd04050b
  (fn [x y]
    (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-bdd03d7f
  (fn [& n]
    (loop [p (apply * n) result nil]
      (if (< p 10)
        (cons p result)
        (recur (quot p 10) (cons (rem p 10) result))))))

(defcheck solution-bdfeddd4
  (fn [a b]
    (loop [acc ()
           num (* a b)]
      (if (< num 10)
        (conj acc num)
        (recur (conj acc (rem num 10))
          (quot num 10))))))

(defcheck solution-be09a46f
  (fn [n1 n2]
    (reduce
      #(conj % (- (int %2) 48))
      []
      (str
        (* n1 n2)))))

(defcheck solution-be0b217a
  (fn [a b] vec (map #(- % 48) (map int (str (* a b))))))

(defcheck solution-be2b57c7
  (fn [a b]
    (let [z (* a b)]
      (map #(parse-int %) (map str (seq (str z)))))))

(defcheck solution-bef66aa6
  (fn [x y]
    (map read-string (map str (seq (str (* x y)))))))

(defcheck solution-bf2e4d98
  (fn [x y] (->> (* x y) str vec (map #(- (int %) (int \0))))))

(defcheck solution-bf43b7b9
  (fn [x y]
    (map #(- (int %) (int \0)) (seq (str (* x y))))))

(defcheck solution-bf8c4078
  (comp #(map read-string (map str %)) str *))

(defcheck solution-bf8cf4f2
  (comp (fn f [n] (if (> n 10) (conj (f (quot n 10)) (mod n 10)) (vector n))) *))

(defcheck solution-c02f301
  (fn [a b]
    (->> (* a b)
      (str)
      (map str)
      (map read-string))))

(defcheck solution-c0b970d5
  (fn [a b]
    (let [c (* a b)]
      (loop [in c out []]
        (if (= 0 in)
          out
          (recur (quot in 10) (cons (rem in 10) out)))))))

(defcheck solution-c0c1eb51
  (fn [a b] (map #(- (int %)(int \0)) (str (* a b)))))

(defcheck solution-c0f84854
  (fn [x y]
    (->> (* x y)
      (str )
      (map #(character-digit % 10) ,))))

(defcheck solution-c10fd1f3
  (comp vec (partial map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}) seq str *))

(defcheck solution-c129d980
  #(-> (apply * %&)
     ((fn decimals [n]
        (loop [n n acc []]
          (if (< n 10)
            (conj acc n)
            (recur (int (/ n 10)) (conj acc (rem n 10)))))))
     (reverse)
     ))

(defcheck solution-c19b9888
  (fn number99 [x y]
    (->> (* x y)
      (str)
      (re-seq #"\d")
      (map #(parse-int %)))))

(defcheck solution-c1ca2492
  (fn [x y]
    (map (fn [d] (- (int d) (int \0)))
      (str (* x y)))))

(defcheck solution-c2d3c0ed
  (fn [x y]
    (reverse (letfn [(dig [x]
                       (if (zero? x) ()
                                     (conj (dig (quot x 10)) (mod x 10))))]
               (dig (* x y))))))

(defcheck solution-c35f2e46
  (fn[x y] (map read-string (map str (str (* x y))))))

(defcheck solution-c36be54d
  (fn [a b]
    (letfn [(d [n] (if (zero? n)
                     []
                     (conj (d (int (/ n 10))) (mod n 10))))]
      (d (* a b)))))

(defcheck solution-c425d43d
  (fn [x y] (map (fn [c]
                   (parse-int (str c)))
              (str (* x y)))))

(defcheck solution-c47024b5
  #(loop [a (* % %2) s []]
     (if (= 0 a)
       s
       (recur (quot a 10) (cons (mod a 10) s)))))

(defcheck solution-c4e7448f
  (fn product-digits [n m]
    (letfn [(digits0 [x b]
              (loop [x x, d '()]
                (if (zero? x) (cons 0 d)
                              (recur (int (/ x b))
                                (cons (mod x b) d)))))
            (digits [x b]
              (let [d (digits0 x b)]
                (if (= d '(0)) d (rest d))))]
      (digits (* n m) 10))))

(defcheck solution-c530bba6
  (fn [a b]
    (apply vector
      (map
        (fn [c]
          (-
           (int c)
           (int \0)
           )
          )
        (str
          (* a b)
          )
        )
      )
    ))

(defcheck solution-c53732a4
  (fn [ & a ](vec (map (comp #(parse-int %) str ) (str  (reduce * (map (comp #(parse-int %) str) a)))))))

(defcheck solution-c5c48cc0
  (fn [x y]
    (->> (* x y)
      str
      seq
      (map #(parse-int (str %))))))

(defcheck solution-c5ccaa7f
  (fn [& xs] (->> (apply * xs)
               str
               (map str)
               (map #(parse-int %)))))

(defcheck solution-c5d72913
  (fn [a b]
    (map #(parse-int %)
      (re-seq (re-pattern ".") (str (* a b))))))

(defcheck solution-c6165c4e
  (fn
    [a b]
    (map #(character-digit % 10) (seq (str (* a b))))))

(defcheck solution-c61da61e
  (fn [& args] (map #(character-digit % 10) (str (reduce * args)))))

(defcheck solution-c6ac45b4
  (fn mult-and-seq
    [& nums]
    (map #(- (int %) 48) ((comp vec str) (apply * nums)))))

(defcheck solution-c6b44e3e
  (fn [x y] (map #(read-string (str %)) (str (* x y)))))

(defcheck solution-c6eabb1e
  (fn [a b]
    (->> a
      (* b)
      str
      (map (comp #(parse-int %) str)))))

(defcheck solution-c722d555
  #((fn [r a] (if (zero? r) a (recur (quot r 10) (cons (mod r 10) a)))) (* % %2) '()))

(defcheck solution-c795f1db
  (fn to-digit-seq
    [a b]
    (into [] (map #(- (int %) (int \0)) (into [] (str (* a b)))))))

(defcheck solution-c79b0fe6
  #(loop [r [] n (* %1 %2) i 10]
     (if (= 0 n) r
                 (recur (cons (rem n 10) r) (int (/ n 10)) (* 1 10))
                 )))

(defcheck solution-c8a2f2f4
  #(->> %& (apply *) str (map (fn [n] (parse-int (str n))))))

(defcheck solution-c8c68622
  (fn [x y]
    (->> (* x y) (str) (map #(- (int %) (int \0))))))

(defcheck solution-c8f1fe05
  (fn [x y] (loop [num (* x y) res '()] (if (< num 10) (conj res num) (recur (int (/ num 10)) (conj res (rem num 10)))))))

(defcheck solution-c96b2e6f
  #(->> (* %1 %2) str (map str) (map read-string)))

(defcheck solution-c9819386
  (fn [a b]
    (loop [m (* a b) r []]
      (if (zero? m)
        r
        (recur (quot m 10) (cons (rem m 10) r))))))

(defcheck solution-c9940a20
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-c9ba01d1
  (fn [n1 n2] (vec (map #(parse-int (str %)) (seq (str (* n1 n2)))))))

(defcheck solution-c9c745cf
  (fn [a b] (letfn [(digits [n] (map #(parse-int (str %)) (str n)))] (digits (* a b)))))

(defcheck solution-c9f49275
  (fn [a b]
    (map
      (comp read-string str)
      (str (* a b)))))

(defcheck solution-ca2bb78f
  (fn [x y]
    (->> (* x y)
      (str)
      (map int)
      (map #(- % 48)))))

(defcheck solution-ca380ed9
  (fn product-digits [a b]
    (loop [c (* a b)
           resp '()]
      (if (= c 0)
        resp
        (recur (quot c 10) (conj resp (mod c 10)))))))

(defcheck solution-ca71152
  (fn [a b]
    ((fn ld [x]
       (if (= 0 x)
         []
         (conj (ld (quot x 10))
           (mod x 10))))
     (* a b))))

(defcheck solution-ca8f9087
  (fn [a b] (map read-string (map str (seq (str (* a b)))))))

(defcheck solution-caaaec4b
  #(map (fn [v] (- (int v) 48))
     (str (* % %2))))

(defcheck solution-caaba4bb
  #(loop [s (str (* %1 %2)), n (dec (count s)), l '()]
     (if (> 0 n)
       (vec (reverse l))
       (recur s (dec n) (concat l (list (get-numeric-value (nth s n))))))))

(defcheck solution-cadb3147
  (fn [a b]
    (loop [m (* a b)
           r []]
      (if (= m 0)
        (reverse r)
        (recur (quot m 10)
          (conj r (rem m 10)))))))

(defcheck solution-cae0e505
  (fn [n1 n2]
    (let [dig {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}]
      (map dig (seq (str (* n1 n2)))))))

(defcheck solution-cb94765f
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))
    ))

(defcheck solution-cbb78b3e
  (fn [a b]
    (let [p (* a b)
          s (str p)]
      (map
        (fn [i] (parse-int (str i)))
        (seq s)))))

(defcheck solution-cbce87a2
  (fn [& nums]

    (let [n (apply * nums)]
      ((fn infn [num]
         (if (= 0 num)
           []
           (conj (infn (quot num 10)) (rem num 10)))) n))))

(defcheck solution-cbd8f0d8
  (fn [& xs]
    (->> (apply * xs) (iterate #(quot % 10))
      (take-while (complement zero?))
      (map #(rem % 10))
      (into '()))))

(defcheck solution-cc13f90e
  (fn [a b] (into [] (map #(read-string (str %)) (seq (str (* a b)))))))

(defcheck solution-cc6f7a51
  #(mapv (fn [n] (parse-int (str n))) (str (* %1 %2))))

(defcheck solution-ccd79368
  (fn [a b]
    (->> (* a b)
      str
      (map str)
      (map #(parse-int %)))))

(defcheck solution-cd058dc1
  (fn [& ns] (->> ns (apply *) str (map #(parse-int (str %))))))

(defcheck solution-cd972517
  (fn [a b]
    (loop [ret '()
           p (* a b)]
      (if (< p 10)
        (conj ret p)
        (recur (conj ret (mod p 10)) (quot p 10))))))

(defcheck solution-cec686d8
  (fn [a b] (vec (map #(read-string (str %)) (str (* a b))) )))

(defcheck solution-cedb88fd
  (fn [a b] (map #(get-numeric-value %) (str (* a b)))))

(defcheck solution-cfa9d9b4
  (fn[a b]
    (let [tmp (* a b)]
      (map #(- (int %) 48) (vec (str tmp))))))

(defcheck solution-cff4b2ff
  (fn [a b]
    (map #(get-numeric-value %) (str (* a b)))))

(defcheck solution-d01d110f
  (fn [x y]
    (letfn [(step [i]
              (if (pos? i)
                (cons (rem i 10) (step (quot i 10)))))]
      (reverse (step (* x y))))))

(defcheck solution-d083d75a
  (fn split[a b]
    (loop[val (* a b),result '() ]
      (let[tail (rem val 10),
           head (quot val 10)]
        (if (< val 10)
          (cons val result)
          (recur head
            (cons tail result)
            )
          )
        )
      )
    ))

(defcheck solution-d0ac5537
  (fn productdigits [m n]
    ((fn f [q]
       (if (< q 10)
         (vector q)
         (conj (f (/ (- q (mod q 10)) 10)) (mod q 10))))
     (* m n))))

(defcheck solution-d0bb9556
  (fn [x y]
    (map #(get-numeric-value %) (into [] (str (* x y))))))

(defcheck solution-d0defc77
  (fn [a b] (map #(- (int %) 48) (into [] (str (* a b))))))

(defcheck solution-d106aaba
  (fn [x y]
    (map #(- (int %) 48) (str (* x y)))))

(defcheck solution-d16b97a3
  (fn [& l] (map #(parse-int %) (re-seq #"\d"  (str (apply * l)) ))))

(defcheck solution-d1ba45c
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-d202ffef
  (fn [i j] (map #(parse-int (str %))
              (str (* i j)))))

(defcheck solution-d22aec8d
  (fn prod->digits [a b]
    (loop [m (* a b)
           acc nil]
      (if (zero? m)
        acc
        (recur (quot m 10)
          (cons (rem m 10) acc))))))

(defcheck solution-d240b90b
  (fn [x y]
    (loop [n (* x y) out '()]
      (if (= n 0) (vec out) (recur (quot n 10) (cons (rem n 10) out))))))

(defcheck solution-d2a25151
  (fn [a b]
    (map #(-> % int (- 48)) (seq (str (* a b))))))

(defcheck solution-d2cecbb1
  (comp (fn digs [n] (if (< n 10) [n] (conj (digs (quot n 10)) (mod n 10)))) *))

(defcheck solution-d2cfe863
  (fn [x y]
    (for [d (str (* x y))]
      (- (int d) (int \0)))))

(defcheck solution-d31ecc3a
  #(map (comp read-string str)
     (str (* % %2))))

(defcheck solution-d33fe865
  (fn [a b]
    (let [x (* a b)]
      (loop [x x r '()]
        (if (= x 0) r
                    (recur (int (/ x 10)) (cons (mod x 10) r)))))))

(defcheck solution-d363c634
  {1 [1] 99 [8 9 1] 999 [9 8 9 0 1]})

(defcheck solution-d37ff656
  (fn [i j]
    (map
      (comp #(parse-int %) str)
      (str (* i j)))))

(defcheck solution-d395c33d
  #((fn[x] (loop [rem x dig []] (if (= rem 0) dig (recur (int (/ rem 10)) (concat [(mod rem 10)] dig))))) (* % %2)))

(defcheck solution-d3ea4ce1
  #(reduce (fn [x y] (into x (vector (- (int y) 48)))) [] (str (* %1 %2))))

(defcheck solution-d40e63c3
  (fn
    [n1 n2]
    (letfn [(g-d ([num result] (if (= num 0)
                                 []
                                 (conj (g-d (int (/ num 10)) result) (mod num 10))))
              ([num] (g-d num [])))]
      (g-d (* n1 n2)))))

(defcheck solution-d43461e8
  #(map (fn [d] (- (int d) (int \0)))
     (str (* %1 %2))))

(defcheck solution-d50849b5
  (fn [d1 d2]
    (map #(- (int %) (int \0)) (str (* d1 d2)))))

(defcheck solution-d5608556
  #(map (fn [x] (parse-int (. x toString))) (. (* % %2) toString)))

(defcheck solution-d5942512
  (fn [a b] (let [p (* a b)] (map #(-> % str parse-int) (seq (str p))))))

(defcheck solution-d65ef24c
  (fn [a b]
    (let [n (* a b)]
      (reverse
        (map first
          (take-while #(> (+ (first %) (second %)) 0)
            (iterate
              (fn [p]
                [(rem (second p) 10)
                 (quot (second p) 10)])
              [(rem n 10) (quot n 10)])))))))

(defcheck solution-d69db9a8
  (fn prob99b [n1 n2] (map #(- (int (identity %)) 48) (str (* n1 n2)))))

(defcheck solution-d6fd5f2d
  (fn [x y]
    (loop [n (* x y) result ()]
      (if (< n 10)
        (conj result n)
        (recur (quot n 10) (conj result (mod n 10)))))))

(defcheck solution-d755e0b2
  (fn [x y]
    (vec (map #(read-string %) (re-seq #"\d" (str (* x y)))))))

(defcheck solution-d788711d
  (fn [x y] (loop [n (* x y) rs '()] (if (= n 0) rs (recur (int (/ n 10)) (conj rs (mod n 10)))))))

(defcheck solution-d795d621
  #(for [x (str (* % %2))] (read-string (str x))))

(defcheck solution-d7ebede2
  (fn split-product [x y]
    (vec (map #(get-numeric-value %)(str (* x y))))))

(defcheck solution-d80c63d8
  (fn [x y]
    (let  [m {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \0 0}]
      (vec (map #(m %) (vec (str (* x y))))))))

(defcheck solution-d82ffeee
  (fn [& xs]
    (loop [n (apply * xs)
           acc ()]
      (let [q (quot n 10)
            r (rem n 10)]
        (if (zero? q)
          (cons r acc)
          (recur q (cons r acc)))))))

(defcheck solution-d89db5c3
  (fn [x y] (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-d8cc70ca
  (fn [a b]
    (loop [r ()
           v (* a b)]
      (if (> v 0)
        (recur (conj r (mod v 10))
          (quot v 10))
        r))))

(defcheck solution-d9d3049
  (fn [a b] (map #(- (int %) 48)
              (str (* a b)))))

(defcheck solution-da048ecc
  (fn [x y]
    (->> (* x y)
      str
      (map #(-> % str read-string)))))

(defcheck solution-da052b18
  (fn mul [a b]
    (loop [x (* a b), digits []]
      (if (= x 0) (vec digits)
                  (recur (quot x 10) (cons (mod x 10) digits))))))

(defcheck solution-da84d48d
  (fn mult [x y]
    (->> (* x y)
      (str)
      (seq)
      (map int)
      (map #(- % 48)))))

(defcheck solution-da87780d
  #(loop [n (* % %2) a []]
     (if (= 0 n) a
                 (recur (quot n 10)
                   (cons (rem n 10) a)))))

(defcheck solution-dadb15f1
  (fn [a b] (map (comp #(parse-int %) str) (str (* a b)))))

(defcheck solution-db68c0b8
  (fn [a b]
    (map #(parse-int (str %)) (str (* a b)))))

(defcheck solution-db8d198d
  (fn prdig [a b]
    (reverse ((fn splitdig [n]
                (if (> n 9)
                  (cons (mod n 10) (splitdig (quot n 10)))
                  (vector n)
                  ))
              (* a b)))))

(defcheck solution-dba54fb4
  (fn [a b]
    (map #(get-numeric-value %)
      (str (* a b)))))

(defcheck solution-dbac3e3
  (fn [x y] (map #(- (int %1) 48) (str (* x y)))))

(defcheck solution-dc5f3955
  (fn [p q]
    (loop [a (* p q) result []]
      (if (zero? a)
        result
        (recur (quot a 10) (concat [(rem a 10)] result))))))

(defcheck solution-dc7e2d61
  (fn [n m]
    (->> (* n m)
      str
      (re-seq #".")
      (map #(parse-int %)))))

(defcheck solution-dc98fbaa
  (fn [a b]
    (->> (map #(quot (* a b) %) (iterate #(* 10 %) 1))
      (take-while #(> % 0))
      (map #(mod % 10))
      reverse)))

(defcheck solution-dd06f21a
  (letfn [(digits [n]
            (let [q (quot n 10)
                  r (rem n 10)]
              (if (zero? q) [r] (conj (digits q) r))))]
    (fn [a b]
      (digits (* a b)))))

(defcheck solution-dea62deb
  (fn [lhs rhs]
    (map #(- (int %) (int \0)) (str (* lhs rhs)))))

(defcheck solution-df3a0022
  (fn [x y]
    (letfn [(digits [x result]
              (let [q (quot x 10)]
                (if (zero? q)
                  (rseq (conj result x))
                  (recur q (conj result (rem x 10))))))]
      (digits (* x y) []))))

(defcheck solution-dfa32008
  (fn [a b]
    (->> (* a b)
      (str)
      (seq)
      (map str)
      (map #(parse-int %)))))

(defcheck solution-dfdcc8f7
  (fn [a b] (->> a (* b) str (map #(character-digit % 10)))))

(defcheck solution-e023574c
  #(map (zipmap [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9] (range)) (str (* % %2))))

(defcheck solution-e04c85bd
  (fn [x y]
    (->> (* x y) str (map #(parse-int (str %))))
    ))

(defcheck solution-e0c2415b
  #((fn f [acc n]
      (if (zero? n) acc
                    (f (conj acc (rem n 10)) (quot n 10))))
    '() (* % %2)))

(defcheck solution-e185ad28
  #(map (fn [s] (parse-int s)) (map str (seq (str (* %1 %2))))))

(defcheck solution-e1a66115
  (fn [x y]
    (map #(parse-int (str %)) (str (* x y)))))

(defcheck solution-e1e80d49
  (fn [n1 n2]
    (map #(- % (int \0)) (map int (seq (str (* n1 n2)))))))

(defcheck solution-e2196932
  (fn [& xs]
    (letfn [(digits [n base]
              (loop [n n ds '()]
                (let [q   (quot n base)
                      ds' (cons (rem n base) ds)]
                  (if (zero? q)
                    ds'
                    (recur q ds')))))]
      (digits (apply * xs) 10))))

(defcheck solution-e2504be4
  (fn [a b]
    (->> (* a b)
      (iterate #(quot % 10))
      (take-while pos?)
      (map #(rem % 10))
      reverse)))

(defcheck solution-e29df49f
  (fn [n1 n2]
    (let [char-2-int (fn [c] (parse-int (str c)))]
      (map char-2-int (str (* n1 n2))))))

(defcheck solution-e3164f11
  (fn product-digits
    [a b]
    (map #(parse-int (str %)) (seq (str (* a b))))))

(defcheck solution-e31874ad
  (fn foo [a b]
    (map (fn [ch] (- (int ch) (int \0))) (str (* a b)))))

(defcheck solution-e3262a23
  (fn [a b]
    (let [to-digits (fn self [ds n]
                      (if (zero? n)
                        ds
                        (self (conj ds (mod n 10)) (quot n 10))))]
      (to-digits '() (* a b)))))

(defcheck solution-e39f3b25
  #(map
     (fn [x] (- (int x) 48))
     (str (* % %2))))

(defcheck solution-e3b07846
  (fn [n1 n2]
    ((fn D1 [n]
       (cond
         (< n 10) (vector n)
         :default
         (conj (D1 (quot n 10)) (rem n 10))
         )

       ) (* n1 n2))))

(defcheck solution-e3d6147a
  (fn product-digits [& nums]
    (letfn [(digits [n]
              (loop [c n res ()]
                (if (< c 10)
                  (conj res c)
                  (recur (quot c 10) (conj res (rem c 10))))))]
      (digits (reduce * nums)))))

(defcheck solution-e457a74f
  (fn [x y]
    ((fn [lst n]
       (if (zero? n) lst
                     (let [digit (mod n 10)]
                       (recur (cons digit lst) (/ (- n digit) 10)))))
     [] (* x y))))

(defcheck solution-e46f8839
  (fn product-digits [n m]
    (vec (map #(parse-int (str %)) (flatten (partition 1 (str (* n m))))))))

(defcheck solution-e4916dc3
  (fn [x y]
    (map #(- (int %) 0x30)
      (str (* x y)))))

(defcheck solution-e4c1dbf5
  (fn m [a b] (map #(parse-int %) (map str (seq (str (* a b)))))))

(defcheck solution-e51a8ae
  #(let[p (* %1 %2)]
     (loop[r (quot p 10) res [(mod p 10)]]
       (if (pos? r)
         (recur (quot r 10) (conj res (mod r 10)))
         (rseq res)))))

(defcheck solution-e523b887
  #(map read-string (map str(str (* % %2)))))

(defcheck solution-e5408583
  (fn [a b] (->> (* a b) str (map #(- (int %) 48)))))

(defcheck solution-e564bd0d
  (fn [x y]
    (reverse (map
               #(rem (quot (* x y) %) 10)
               (take-while #(>= (* x y) %) (iterate #(* 10 %) 1))))))

(defcheck solution-e5693df8
  (fn [a b]
    (->> (* a b)
      (str)
      (map str)
      (map #(parse-int %)))))

(defcheck solution-e58e7dd6
  (letfn [(D [n]
            (if (> n 9) (conj (D (quot n 10)) (rem n 10))
                        [n]))]
    (fn [& xs]
      (D (apply * xs)))))

(defcheck solution-e6a27854
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map #(parse-int %)))))

(defcheck solution-e6b919cb
  (fn [a b] (reverse (map #(rem % 10) (take-while pos? (iterate #(quot % 10) (* a b))) ) ) ))

(defcheck solution-e6da7fbe
  (fn [n m] (map #(parse-int (str %)) (str (* n m)))))

(defcheck solution-e6db2953
  (fn [n1 n2]
    (loop [n (* n1 n2) lst []]
      (if (= n 0)
        lst
        (recur (quot n 10) (concat [(mod n 10)] lst))))))

(defcheck solution-e6dba465
  (fn [x y]
    (loop [acc '()
           v (* x y)]
      (if (= 0 v)
        (vec acc)
        (recur (conj acc (mod v 10)) (quot v 10))))))

(defcheck solution-e7a55120
  (fn [x y] ((fn peu [z] (if (< z 10) [z] (concat (peu (quot z 10)) (vector (mod z 10))) )) (* x y))))

(defcheck solution-e7f147c9
  (fn [a b]
    (map #(read-string (str %)) (str (* a b)))))

(defcheck solution-e80bb7b
  (fn pd [a b] (mapv #(- (int %) (int \0)) (seq (str (* a b))))))

(defcheck solution-e8bd4a9b
  (fn [x y]
    (map #(parse-int (str %))
      (apply vector (str (* x y))))))

(defcheck solution-e91254d2
  #(loop [n (* %1 %2)
          result []]
     (if (< n 10)
       (cons n result)
       (recur (quot n 10) (cons (rem n 10) result)))))

(defcheck solution-e9b5458a
  #(map (comp (partial + -48) int) (seq (str (* %1 %2)))))

(defcheck solution-eaa50e85
  #(map (fn [c] (get-numeric-value c)) (str (* %1 %2))))

(defcheck solution-eb8eeeb
  (fn [x1 x2]
    (->> (* x1 x2)
      str
      seq
      (map str)
      (map read-string))))

(defcheck solution-eba6cef4
  (fn [& xs]
    (let [n (apply * xs)]
      (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
        (take-while (fn [vs] (some (complement zero?) vs)))
        (map second)
        rest
        reverse
        (into [])))))

(defcheck solution-ebfc1cfd
  #(map (comp (partial + -48) int) (str (* % %2))))

(defcheck solution-ec1316bf
  #(letfn [(worker [z c]
             (if (zero? z)
               c
               (recur (quot z 10)
                 (conj c (rem z 10)))))]
     (let [s (* %1 %2)]
       (if (zero? s)
         '(0)
         (worker s '())))))

(defcheck solution-ec1c483f
  (fn[a b]
    (map #(parse-int %)
      (map str (seq (str (* a b)))))))

(defcheck solution-ec4a01e8
  (fn [x y] (map read-string (map str (seq (str (* x y)))))))

(defcheck solution-ec5cd95c
  (fn product-digits [x y]
    (letfn [(digits [n]
              (if (< n 10)
                [n]
                (conj (digits (quot n 10)) (rem n 10))))]
      (digits (* x y)))))

(defcheck solution-ecd4c059
  #(map (fn [x] (parse-int (str x))) (seq (str (* %1 %2)))))

(defcheck solution-ece7f94c
  (fn [a b]
    (vec (map (comp read-string str)  (str (* a b))))))

(defcheck solution-ed7aac79
  (fn [a b]
    (reverse
      (map first
        (take-while #(not= % [0 0])
          (rest
            (iterate
              (fn [[digit the-rest]]
                (vector (rem the-rest 10)
                  (quot the-rest 10)))
              (vector 0 (* a b)))))))))

(defcheck solution-ed9714bf
  (fn [x y] (map #(get-numeric-value %) (str (* x y)))))

(defcheck solution-ed9b4715
  #(
    (fn self [n xs]
      (if (= n 0) xs
                  (self (quot n 10) (cons (mod n 10) xs))))
    (* %1 %2) ()))

(defcheck solution-edc65f67
  (fn [d e]
    (map (comp read-string str) (str (* d e)))))

(defcheck solution-edd3a2c6
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map #(parse-int %)))))

(defcheck solution-edf51863
  (fn [a b]
    (->>
      (* a b)
      str
      seq
      (map #(parse-int (str %)))
      (apply vector))))

(defcheck solution-ee059bac
  #(loop [q (* %1 %2) result nil]
     (if (= q 0)
       (vec result)
       (recur (quot q 10) (cons (mod q 10) result)))))

(defcheck solution-ee42798b
  (fn [a b]
    ((fn digits [x]
       (if (= x 0)
         []
         (conj (digits (quot x 10)) (rem x 10))
         )
       ) (* a b)
     )
    ))

(defcheck solution-ee528836
  (fn digits [n m] (map (comp #(- % 48) int) (str (* n m)))))

(defcheck solution-ef55a4
  #(loop [q (* %1 %2)
          ds '()]
     (if (zero? q)
       ds
       (recur (quot q 10) (cons (rem q 10) ds)))))

(defcheck solution-efa1b24c
  #(map (fn [x] (parse-int x)) (re-seq #"\d" (str (* % %2)))))

(defcheck solution-f0fc2e87
  #(let [mn (* %1 %2)]
     (loop [m mn r []]
       (if (= 0 m) (vec (reverse r))
                   (recur (long (/ m 10)) (conj r (rem m 10)))
                   ))))

(defcheck solution-f19f6e0c
  (comp (partial map #(- (int %) 48)) str *))

(defcheck solution-f1cd0ed8
  (fn [x y]
    ((fn [n accu]
       (if (> n 0)
         (recur (int (/ n 10)) (cons (mod n 10) accu))
         accu)) (* x y) nil)))

(defcheck solution-f219cde5
  (fn [x y] (map #(- (int %) 48) (seq (str (* x y))))))

(defcheck solution-f260df77
  #(map (fn [n] (get-numeric-value n)) (vec (str (* %1 %2)))))

(defcheck solution-f2b51dd4
  #(->> %&
     (apply *)
     str
     seq
     (map str)
     (map read-string)))

(defcheck solution-f3d366ef
  (fn [x y] (map #(parse-int (str %)) (vec (str (* x y))))))

(defcheck solution-f3e36009
  #(->> (* % %2) str seq (map int) (map (partial + -48))))

(defcheck solution-f419dfb0
  (fn [a b]
    (map #(- (int %) (int \0)) (seq (str (* a b))))))

(defcheck solution-f448f6b4
  (fn [x y]
    (->> (* x y)
      str
      seq
      (map str)
      (map #(parse-int %))
      vec)))

(defcheck solution-f49f0387
  #(map (fn [v] (- (int v) (int \0))) (pr-str (* %1 %2))))

(defcheck solution-f4b3f202
  (fn [x y]
    ((fn [x v]
       (let [m (mod x 10) d (long (/ x 10))]
         (if (= x 0) v (recur d (cons m v)) )))
     (* x y) [] )))

(defcheck solution-f4fba686
  (fn mpd
    [a b]
    (map #(parse-int (str %)) (seq (str (* a b))))))

(defcheck solution-f579c589
  (fn [x y] (map #(- (int %) (int \0)) (seq (str (* x y))))))

(defcheck solution-f5ef6714
  #(->> %& (apply *) str (map str) (map (fn [n] (parse-int n)))))

(defcheck solution-f6a4b6f7
  #(map (fn [n] (parse-int (str n))) (str (* %1 %2))))

(defcheck solution-f6b202e
  (fn mult [x y]
    (let [m (* x y)
          s (seq (.toString m))]
      (map #(parse-int (.toString %)) s))))

(defcheck solution-f8ed8d6c
  (fn [x y]
    (reverse
      (map #(rem % 10)
        (take-while pos?
          (iterate #(quot % 10) (* x y)))))))

(defcheck solution-f91483cc
  (fn [a b] (map
              #(parse-int (str %))
              (seq (str (* a b))))))

(defcheck solution-f96423d0
  (fn [n1 n2]
    ((fn [n r]
       (if (< n 10) (cons n r) (recur (quot n 10) (cons (mod n 10) r))))
     (* n1 n2) [])))

(defcheck solution-f968ec01
  (fn [a b] (map #(parse-int (str %)) (seq (str (* a b))))))

(defcheck solution-f9df68d7
  (fn [x y]
    (map #(read-string (str %)) (seq (str (* x y))))))

(defcheck solution-fb3df586
  #((fn s [coll x] (if (= 0 x) (vec coll) (s (cons (rem x 10) coll) (quot x 10)))) [] (* %1 %2)))

(defcheck solution-fb636efe
  #((fn r [n] (if (< n 10)
                [n]
                (conj (r (quot n 10)) (mod n 10))))
    (* % %2)))

(defcheck solution-fb924073
  (fn product-digits [a b]
    (let [mul (* a b)
          chars (seq (str mul))]
      (map #(parse-int (str  %)) chars))))

(defcheck solution-fc66b53a
  (fn [x y]
    (map #(parse-int %) (filter #(not (empty? %)) (.split (str (* x y)) "")))))

(defcheck solution-fce64164
  (comp (partial map #(- (int %) (int \0))) str *))

(defcheck solution-fd4f410a
  (fn f [a b]
    ((fn [o] (loop [n o d '()]
               (if (= n 0) d (recur (quot n 10) (cons (mod n 10) d)))))
     (* a b))))

(defcheck solution-fd625111
  (fn prod-dig [a b]
    (let [prod (* a b)
          len (count (str prod))]
      (map #(rem % 10) (reverse (take len (iterate #(quot % 10) prod)))))))

(defcheck solution-fd8eb3bd
  (fn [a b] (map #(-> % str parse-int) (-> (* a b) str seq))))

(defcheck solution-fdf7dab0
  (fn [x y]
    (->> (* x y)
      str
      (map str)
      (map read-string))))

(defcheck solution-fe3102a3
  #(loop [n (apply * %&) r '()]
     (if (< n 10)
       (vec (cons n r))
       (recur (-> (/ n 10) Math/floor int) (cons (mod n 10) r))
       )
     ))

(defcheck solution-feb485da
  (fn [x y]
    (->>
      (* x y)
      (str)
      (seq)
      (map (comp read-string str))
      )
    ))

(defcheck solution-fedf85b5
  (fn [param1 param2]
    (->> (* param1 param2)
      (str)
      (seq)
      ;(map #(parse-int (str %)))
      (map #(- (int %) (int \0)))
      (vec)
      )))

(defcheck solution-ff2be7f3
  #(for[x(str(* %1 %2))](read-string(str x))))

(defcheck solution-ff6f2979
  (fn [x y] (let [fr (fn [res n] (if (= 0 n) res
                                             (let [q (int (/ n 10)) r (mod n 10) nres (cons r res)]
                                               (recur nres q) )
                                             )
                       )] (fr [] (* x y)))))

(defcheck solution-fff53dd8
  (fn [x y]
    (let [p (str (* x y))]
      (map #(read-string (str %)) (seq p)))))
