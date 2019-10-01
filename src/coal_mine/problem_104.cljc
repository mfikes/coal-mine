(ns coal-mine.problem-104
  (:require [coal-mine.checks :refer [defcheck-104] :rename {defcheck-104 defcheck}]
            [clojure.pprint]
            [clojure.set]
            [clojure.test]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s) :cljs (js/parseInt s)))

(defcheck solution-1065a5c
  (fn to-roman [n]
    (apply str
      ((fn to-roman' [r n]
         (cond
           (>= n 1000) (concat r "M" (to-roman' r (- n 1000)))
           (>= n 900) (concat r "CM" (to-roman' r (- n 900)))
           (>= n 500) (concat r "D" (to-roman' r (- n 500)))
           (>= n 400) (concat r "CD" (to-roman' r (- n 400)))
           (>= n 100) (concat r "C" (to-roman' r (- n 100)))
           (>= n 90) (concat r "XC" (to-roman' r (- n 90)))
           (>= n 50) (concat r "L" (to-roman' r (- n 50)))
           (>= n 40) (concat r "XL" (to-roman' r (- n 40)))
           (>= n 10) (concat r "X" (to-roman' r (- n 10)))
           (>= n 9) (concat r "IX" (to-roman' r (- n 9)))
           (>= n 5) (concat r "V" (to-roman' r (- n 5)))
           (>= n 4) (concat r "IV" (to-roman' r (- n 4)))
           (>= n 1) (concat r "I" (to-roman' r (- n 1)))
           :else r))
       "" n))))

(defcheck solution-108b8e66
  (fn wrn [x]
    (let [nums (loop [n x, result '()] (if (zero? n) result (recur (quot n 10) (conj result (mod n 10)))))
          bits (concat (repeat (- 4 (count nums))0) nums)
          th (fn [n] (apply str (repeat n \M)))
          hu (fn [n] (apply str (cond
                                  (< n 4) (repeat n \C)
                                  (= n 4) '(\C \D)
                                  (= n 9) '(\C \M)
                                  :else (conj (repeat (- n 5) \C) \D))))
          te (fn [n] (apply str (cond
                                  (< n 4) (repeat n \X)
                                  (= n 4) '(\X \L)
                                  (= n 9) '(\X \C)
                                  :else (conj (repeat (- n 5) \X) \L))))
          ge (fn [n] (apply str (cond
                                  (< n 4) (repeat n \I)
                                  (= n 4) '(\I \V)
                                  (= n 9) '(\I \X)
                                  :else (conj (repeat (- n 5) \I) \V))))]
      (apply str (concat (th (nth bits 0)) (hu (nth bits 1)) (te (nth bits 2)) (ge (nth bits 3)))))))

(defcheck solution-10e00354
  (fn [n]
    (let [r [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (first (reduce (fn [[acc n] r]
                       [(str acc (apply str (repeat (quot n (first r)) (last r)))) (rem n (first r))])
               ["" n]
               r)))))

(defcheck solution-10fb095b
  (let
   [lookup [
            [\I [\I \V] [\V] [\I \X]]
            [\X [\X \L] [\L] [\X \C]]
            [\C [\C \D] [\D] [\C \M]]
            [\M]]
    ]

    #(let [digits (map (comp read-string str) (str %))]
       (loop [converted '() counter 0]
         (if (>= counter (count digits))
           (apply str converted)
           (let [digit (nth digits (- (count digits) counter 1)) table (nth lookup counter nil)
                 v1 (nth table 0 nil) v2 (nth table 1 nil) v3 (nth table 2 nil) v4 (nth table 3 nil)]
             (recur
               (cond
                 (= digit 0) converted
                 (< digit 4) (concat (take digit (repeat v1)) converted)
                 (= 4 digit) (concat v2 converted)
                 (= 5 digit) (concat v3 converted)
                 (and (> digit 5) (< digit 9)) (concat v3 (take (- digit 5) (repeat v1)) converted)
                 (= 9 digit) (concat v4 converted)
                 )
               (inc counter)
               )
             )
           )
         )
       )
    ))

(defcheck solution-1108ffd8
  #(let [value (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
         lookup (fn [v] (first (filter (fn [[k _]] (>= v k)) value)))]
     (loop [total %
            numerals ""]
       (if (> 1 total)
         numerals
         (recur (- total (first (lookup total))) (str numerals (second (lookup total))))))))

(defcheck solution-1146cd23
  (fn write-roman
    [n]
    (let [numeral-vals (sort-by #(- (first %))
                         {1000 "M" 900 "CM" 500 "D" 400 "CD"
                          100 "C" 90 "XC" 50 "L" 40 "XL"
                          10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"})
          [v numeral] (first (filter #(<= (first %) n) numeral-vals))]
      (if (= n 0)
        ""
        (str numeral (write-roman (- n v)))))))

(defcheck solution-122c8089
  (fn [n] (apply str
            (loop [acc [] l (map str (str n))]
              (if (or (empty? l) (empty? (filter #((comp not = ) "0" %) l))) acc
                                                                             (let [x (int (* (parse-int (first l)) (Math/pow 10 (count (rest l)))))]
                                                                               (cond (< 999 x 4000) (recur (conj acc (apply str (repeat (parse-int (first l)) "M"))) (rest l))
                                                                                     (< 99 x 400) (recur (conj acc (apply str (repeat (parse-int (first l)) "C"))) (rest l))
                                                                                     (= x 400) (recur (conj acc "CD") (rest l))
                                                                                     (= x 500) (recur (conj acc "D") (rest l))
                                                                                     (= x 600) (recur (conj acc "DC") (rest l))
                                                                                     (= x 700) (recur (conj acc "DCC") (rest l))
                                                                                     (= x 800) (recur (conj acc "DCCC") (rest l))
                                                                                     (= x 900) (recur (conj acc "CM") (rest l))
                                                                                     (= x 1000) (recur (conj acc "M") (rest l))
                                                                                     (< 9 x 40) (recur (conj acc (apply str (repeat (parse-int (first l)) "X"))) (rest l))
                                                                                     (= x 40) (recur (conj acc "XL") (rest l))
                                                                                     (= x 50) (recur (conj acc "L") (rest l))
                                                                                     (= x 60) (recur (conj acc "LX") (rest l))
                                                                                     (= x 70) (recur (conj acc "LXX") (rest l))
                                                                                     (= x 80) (recur (conj acc "LXXX") (rest l))
                                                                                     (= x 90) (recur (conj acc "XC") (rest l))
                                                                                     (< 0 x 4) (recur (conj acc (apply str (repeat (parse-int (first l)) "I"))) (rest l))
                                                                                     (= x 4) (recur (conj acc "IV") (rest l))
                                                                                     (= x 5) (recur (conj acc "V") (rest l))
                                                                                     (= x 6) (recur (conj acc "VI") (rest l))
                                                                                     (= x 7) (recur (conj acc "VII") (rest l))
                                                                                     (= x 8) (recur (conj acc "VIII") (rest l))
                                                                                     (= x 9) (recur (conj acc "IX") (rest l)))))))))

(defcheck solution-12ad6226
  (fn [n]
    (loop [i n
           res ""]
      (cond
        (>= i 1000) (recur (- i 1000) (str res "M"))
        (>= i 900) (recur (- i 900) (str res "CM"))
        (>= i 500) (recur (- i 500) (str res "D"))
        (>= i 400) (recur (- i 400) (str res "CD"))
        (>= i 100) (recur (- i 100) (str res "C"))
        (>= i 90) (recur (- i 90) (str res "XC"))
        (>= i 50) (recur (- i 50) (str res "L"))
        (>= i 40) (recur (- i 40) (str res "XL"))
        (>= i 10) (recur (- i 10) (str res "X"))
        (= i 9) (recur (- i 9) (str res "IX"))
        (>= i 5) (recur (- i 5) (str res "V"))
        (= i 4) (recur (- i 4) (str res "IV"))
        (>= i 1) (recur (- i 1) (str res "I"))
        (= i 0) res))))

(defcheck solution-12b485ae
  (fn [k]
    (let [r (sorted-map
              1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L" 90 "XC",
              100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M")
          f (fn [acc k]
              (if (zero? k) acc
                            (let [[k' s] (last (take-while #(<= (key %) k) r))]
                              (recur (conj acc s) (- k k')))))]
      (apply str (f [] k)))))

(defcheck solution-12ba8872
  #(case %
     1 "I"
     30 "XXX"
     4 "IV"
     140 "CXL"
     827 "DCCCXXVII"
     48 "XLVIII"
     "MMMCMXCIX"))

(defcheck solution-13688df5
  (fn[x]
    (apply str
      (let [rom-map {1000 \M, 500 \D, 100 \C, 50 \L, 10 \X, 5 \V, 1 \I}]
        (loop [acc [] val x base 1000]
          (let [next-base (max 1 (/ base 10))]
            (cond
              (>= 0 val)
              acc
              (< val base)
              (recur acc val next-base)
              (= 9 (quot val base))
              (recur (conj acc (rom-map base) (rom-map (* 10 base))) (- val (* 9 base)) next-base)
              (<= 5 (quot val base))
              (recur (conj acc (rom-map (* 5 base))) (- val (* 5 base)) base)
              (= 4 (quot val base))
              (recur (conj acc (rom-map base) (rom-map (* 5 base))) (- val (* 4 base)) next-base)
              :otherwise
              (recur (conj acc (rom-map base)) (- val base) base))))))))

(defcheck solution-136cee3f
  (fn [n]
    (let
     [
      cs
      (hash-map
        "M" 1000
        "D" 500
        "C" 100
        "CM" 900
        "XC" 90
        "L" 50
        "XL" 40
        "X" 10
        "IX" 9
        "V" 5
        "IV" 4
        "I" 1
        "" 0
        )

      do-roman
      (fn do-roman [n]
        (let
         [
          c
             (cond
               (>= n 1000) "M"
               (>= n 900) "CM"
               (>= n 500) "D"
               (>= n 100) "C"
               (>= n 90) "XC"
               (>= n 50) "L"
               (>= n 40) "XL"
               (>= n 10) "X"
               (>= n 9) "IX"
               (>= n 5) "V"
               (>= n 4) "IV"
               (>= n 1) "I"
               :else ""
               )

          v (get cs c)

          n' (- n v)
          ]

          (if (= n 0)
            []
            (cons
              c
              (do-roman n')
              )
            )
          )
        )
      ]

      (clojure.string/join
        (do-roman n)
        )
      )
    ))

(defcheck solution-13fd5ea0
  (fn[n]
    (let [s  [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
          f (fn[a m d] [(mod a m) (apply str (repeat (int (/ a m)) d))])]
      (first (reduce (fn[[r a] [m d]] (let [[t k] (f a m d)] [(str r k) t]))
               ["" n] s)))))

(defcheck solution-14b597f6
  (fn write-roman [n]
    (let [c [[1000 "M"] [900 "CM"] [500 "D"] [100 "C"] [90 "XC"] [50 "L"] [10 "X"] [9 "IX"] [5 "V"] [1 "I"]]
          d {4 "IV", 40 "XL", 400 "CD"}
          [x y] (first (remove nil? (map (fn [[a b]]
                                           (let [q (quot n a)]
                                             (cond
                                               (= q 4) [(d (* q a)) (- n (* q a))]
                                               (< 0 q 4) [(repeat q b) (- n (* q a))]
                                               )))
                                      c)))]
      (apply str (if (zero? y)
                   x
                   (concat x (write-roman y)))))))

(defcheck solution-15084ae3
  (fn [x]
    (loop [table '(
                   ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                   ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                   ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                   ["" "M" "MM" "MMM"])
           n x
           acc ""]
      (if (zero? n)
        acc
        (recur (rest table) (quot n 10) (str (nth (first table) (rem n 10)) acc))))))

(defcheck solution-15254226
  (fn romanize [an]
    (let [rn-vals [[\M 1000] [\D 500] [\C 100] [\L 50] [\X 10] [\V 5] [\I 1]]
          rn-map (apply hash-map (apply concat rn-vals))

          rns (reverse (map first rn-vals))
          rn-next (fn [rn]
                    (nth rns (inc (.indexOf rns rn))))

          naive (partition-by identity
                  (loop [n an
                         [[rn val] & rv-mas :as rnvs] rn-vals
                         out []]
                    (if (empty? rnvs)
                      out
                      (let [ct (Math/floor (/ n val))]

                        (recur (mod n val) rv-mas (concat out (repeat ct rn)))))))]
      ;;(println :nv naive)

      (loop [[rg  & rg-mas :as rgs] naive
             out []]
        (cond
          (empty? rgs)
          (apply str (flatten out))

          (= (count rg) 4)
          (if (or
               (nil? (last out))
               (>= (/ (get rn-map (first (last out)))
                     (get rn-map (first rg))) 10))
            (recur rg-mas (conj out [(first rg) (rn-next (first rg))]))
            (recur rg-mas (conj (vec (butlast out)) [(first rg)
                                                     (rn-next (first (last out)))])))

          :default (recur rg-mas (conj out rg)))))))

(defcheck solution-15505ab5
  (fn pr [n]
    (let [numerals
                      {1 "I"
                       4 "IV"
                       5 "V"
                       9 "IX"
                       10 "X"
                       40 "XL"
                       50 "L"
                       90 "XC"
                       100 "C"
                       400 "CD"
                       500 "D"
                       900 "CM"
                       1000 "M"}
          sorted-keys (sort (keys numerals))]
      (if (= 0 n)
        ""
        (let [next-key (last (filter #(<= % n) sorted-keys))]
          (str (get numerals next-key) (pr (- n next-key))))))))

(defcheck solution-1565bfaa
  (fn [n]
    (let [val->letter (sorted-map-by >
                        1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I
                        900 "CM" 400 "CD" 90 "XC" 40 "XL" 9 "IX" 4 "IV")]
      (loop [n n res []]
        (if (zero? n)
          (clojure.string/join res)
          (let [[v l] (first (filter #(>= n (key %)) val->letter))]
            (recur (- n v) (conj res l))))))))

(defcheck solution-159683d9
  (fn [n]
    (let [roman {1 \I
                 5 \V
                 10 \X
                 50 \L
                 100 \C
                 500 \D
                 1000 \M}
          fill (fn [[factor n]]
                 (let [smalln factor
                       midn (* factor 5)
                       largen (* factor 10)
                       smallc (roman smalln)
                       midc (roman midn)
                       largec (roman largen)]
                   ({smalln [smallc]
                     (* 2 smalln) [smallc smallc]
                     (* 3 smalln) [smallc smallc smallc]
                     (- midn smalln) [smallc midc]
                     midn [midc]
                     (+ midn smalln) [midc smallc]
                     (+ midn (* 2 smalln)) [midc smallc smallc]
                     (+ midn (* 3 smalln)) [midc smallc smallc smallc]
                     (- largen smalln) [smallc largec]} n)))]
      (->> [1000 100 10 1]
        (map (juxt identity (fn [d]
                              (mod (- n (mod n d)) (* d 10)))))
        (mapcat fill)
        (apply str)))))

(defcheck solution-15b413ab
  (fn writerom
    ([n] (writerom n []))
    ([n o]
     (let [num->digits (fn num->digits [num]
                         (loop [n num digits []]
                           (if (< n 10)
                             (cons (int n) digits)
                             (recur (quot n 10)
                               (cons (int (rem n 10))
                                 digits)))))
           dig (num->digits n)
           f (fn f [n bb b s]
               (case n
                 9 [s bb]
                 8 [b s s s]
                 7 [b s s]
                 6 [b s]
                 5 [b]
                 4 [s b]
                 3 [s s s]
                 2 [s s]
                 1 [s]
                 0 []))]
       (case (count dig)
         4 (let [x (int (/ n 1000))]
             (writerom (- n (* 1000 x))
               (into o (take x (repeat "M")))))
         3 (writerom (- n (* 100 (int (/ n 100))))
             (into o (f (first dig) "M" "D" "C")))
         2 (writerom (- n (* 10 (int (/ n 10))))
             (into o (f (first dig) "C" "L" "X")))
         1 (apply str
             (into o (f (first dig) "X" "V" "I"))))))))

(defcheck solution-15c48a59
  (fn write-roman [n]
    (let [num-maps [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                    ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                    ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                    ["" "M" "MM" "MMM" "MMMM"]]
          convert-to-digits (fn [n result]
                              (if (zero? n)
                                (if (= (count result) 0) [0] result)
                                (recur (quot n 10) (conj result (rem n 10)))))
          digits (vec (convert-to-digits n []))]
      (apply str (reverse (map-indexed (fn [i j] ((num-maps i) j)) digits))))))

(defcheck solution-15db0418
  #(nth
     (reduce
       (fn [[n s] [d v]]
         [(mod n d) (apply str s (repeat (quot n d) v))])
       [% ""]
       (rseq (sorted-map 1000 \M 900 "CM" 500 \D 400 "CD" 100 \C 90
               "XC" 50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I)))
     1))

(defcheck solution-15db77f1
  (letfn [(roman [n]
            (letfn [(f [n c1 c2 c3]
                      (cond
                        (< n 4) (apply str (repeat n c1))
                        (= n 4) (str c1 c2)
                        (= n 5) c2
                        (= n 9) (str c1 c3)
                        :else   (apply str c2 (repeat (- n 5) c1))))]
              (let [[cs1 cs2 cs3] ["IVX" "XLC" "CDM"]]
                (cond
                  (< n 10)   (apply f n cs1)
                  (< n 100)  (str (apply f (quot n 10) cs2) (apply f (mod n 10) cs1))
                  (< n 1000) (str (apply f (quot n 100) cs3) (roman (mod n 100)))
                  (< n 4000) (str (apply str (repeat (quot n 1000) "M")) (roman (mod n 1000)))))))]
    roman))

(defcheck solution-1660e0a
  (fn [n]
    (let [th (quot n 1000), h (quot (rem n 1000) 100), t (quot (rem n 100) 10), o (rem n 10),
          m {:th (zipmap (range 4) ["", "M", "MM", "MMM"]),
             :h (zipmap (range 10) ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]),
             :t (zipmap (range 10) ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]),
             :o (zipmap (range 10) ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"])}]
      (str (get-in m [:th th]) (get-in m [:h h]) (get-in m [:t t]) (get-in m [:o o])))))

(defcheck solution-1689f131
  (fn to-roman [n]
    (let [digits '([1000 "M"] [900 "CM"] [500 "D"] [400 "CD" ]
                   [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
                   [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"])]
      (loop [n n, digits digits, acc []]
        (if-let [[v romn] (first digits)]
          (if (< n v)
            (recur n (rest digits) acc)
            (recur (- n v) digits (conj acc romn)))
          (apply str acc))))))

(defcheck solution-1699c1ae
  (fn write-roman [i]
    (let [romans (sorted-map-by > 1 "I", 2 "II", 3 "III", 4 "IV", 5 "V",
                   6 "VI", 7 "VII", 8 "VIII", 9 "IX", 10 "X",
                   20 "XX", 30 "XXX", 40 "XL", 50 "L",
                   60 "LX", 70 "LXX", 80 "LXXX", 90 "XC"
                   100 "C", 200 "CC", 300 "CCC", 400 "CD", 500 "D",
                   600 "DC", 700 "DCC", 800 "DCCC", 900 "CM"
                   1000 "M", 2000 "MM", 3000 "MMM")
          k (some #(if (<= % i) %) (keys romans))
          r (- i k)]
      #_(println k)
      (if (= r 0) (romans k)
                  (apply str (concat (romans k) (write-roman r)))))))

(defcheck solution-16b09b37
  (fn r [x]
    (if (= x 0) ""
                (let [R (sorted-map 1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L",
                          90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M")
                      m (first (filter #(>= x (key %)) (reverse R)))]
                  (str (val m) (r (- x (key m))))))))

(defcheck solution-17131da6
  (fn number->roman [num]
    (let [conversion {3000 "MMM"
                      2000 "MM"
                      1000 "M"
                      900 "CM"
                      800 "DCCC"
                      700 "DCC"
                      600 "DC"
                      500 "D"
                      400 "CD"
                      300 "CCC"
                      200 "CC"
                      100 "C"
                      90 "XC"
                      80 "LXXX"
                      70 "LXX"
                      60 "LX"
                      50 "L"
                      40 "XL"
                      30 "XXX"
                      20 "XX"
                      10 "X"
                      9 "IX"
                      8 "VIII"
                      7 "VII"
                      6 "VI"
                      5 "V"
                      4 "IV"
                      3 "III"
                      2 "II"
                      1 "I"}]
      (letfn [(intlist [x]
                ((fn [acc n]
                   (if (zero? n)
                     acc
                     (recur (cons (mod n 10) acc) (int (/ n 10))))) '() x))
              (to-rom [n]
                (get conversion n))]
        (apply str
          (first
            (reduce
              (fn [acc n]
                [(concat
                   (to-rom (* n (second acc)))
                   (first acc))
                 (* 10 (second acc))])
              ["" 1]
              (reverse (intlist num)))))))))

(defcheck solution-17eed7b
  (fn dec->rom [n]
    (let [tr (sorted-map 1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"
               4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900 "CM")]
      (condp = n
        0 ""
        (let [x (apply max (filter #(<= % n) (keys tr)))]
          (str (tr x) (dec->rom (- n x))))))))

(defcheck solution-17f0d51a
  (fn write-roman
    ([n]
     (write-roman n ""))
    ([n s]
     (if (zero? n)
       s
       (let [m (array-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")
             pair (last (take-while (fn[[key val]] (<= key n)) m))]
         (write-roman (- n (key pair)) (str s (val pair))))))))

(defcheck solution-1866f2c4
  (fn roman [number]
    (if (zero? number)
      ""
      (let
       [values (clojure.set/map-invert {"M" 1000, "CM" 900, "D" 500, "CD" 400, "C" 100, "XC" 90, "L" 50, "XL" 40, "X" 10, "IX" 9, "V" 5, "IV" 4, "I" 1})
        found (apply max (filter #(<= % number) (keys values)))]
        (str (values found) (roman (- number found)))))))

(defcheck solution-18a268d2
  (fn [n] (apply str (reverse (map #(apply
                                      (fn [num lits]
                                        (let [mo (rem num 5)
                                              [x y z & _] lits
                                              i (take mo (repeat x))
                                              uno (if (< mo 4)
                                                    (if (> num 4)
                                                      (conj i y)
                                                      i)
                                                    (if (> num 4)
                                                      [x z]
                                                      [x y]))]
                                          (apply str uno)))
                                      %)
                                (partition 2 (interleave
                                               (map #(rem % 10) (take-while (complement zero?) (iterate #(quot % 10) n)))
                                               (take-while #(some (complement nil?) %) (iterate #(drop 2 %) [\I \V \X \L \C \D \M]))))

                                )))))

(defcheck solution-195b9a29
  (fn [n]
    (let  [rmap {1000 "M",900 "CM",500 "D",400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"}]
      (first (reduce
               (fn [[s n] e]
                 (loop [s s n n]
                   (if (< n e)
                     [s n]
                     (recur (str s (rmap e)) (- n e)))))
               ["" n] (reverse (sort (keys rmap))))))))

(defcheck solution-19ed74f3
  (fn
    [n]
    (let [m (array-map
              1 "I"
              4 "IV"
              5 "V"
              9 "IX"
              10 "X"
              50 "L"
              40 "XL"
              100 "C"
              90 "XC"
              500 "D"
              900 "CM"
              1000 "M")
          ns (map #(let [v (rem (quot n %) 10)]
                     (cond
                       (= % 1000) (repeat v "M")
                       (= % 100) (or (m (* v 100))
                                     (if (> v 5)
                                       (cons "D" (repeat (- v 5) "C"))
                                       (repeat v "C")))
                       (= % 10) (or (m (* v 10))
                                    (if (> v 5)
                                      (cons "L" (repeat (- v 5) "X"))
                                      (repeat v "X")))
                       (= % 1) (or (m v)
                                   (if (> v 5)
                                     (cons "V" (repeat (- v 5) "I"))
                                     (repeat v "I")))))
               [1000 100 10 1])
          ]
      (reduce str (flatten ns)))))

(defcheck solution-1a0cd293
  (fn [n]
    (let [R {1000 "M"  900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL"
             10 "X"  9 "IX" 8 "VIII" 7 "VII"
             6 "VI" 5 "V" 4 "IV" 3 "III" 2 "II" 1 "I"}]
      (letfn [(next [n]
                (first (filter #(>= n %) (sort > (keys R)))))
              (rom [n result]
                (if (= 0 n)
                  result
                  (let [r (next n)]
                    (rom (- n r) (conj result (R r))))))]
        (apply str (rom n []))))))

(defcheck solution-1a531a31
  (fn arabic->roman [n]
    (let [ subs [
                 [1000 "M"]
                 [900 "CM"]
                 [500 "D"]
                 [400 "CD"]
                 [100 "C"]
                 [90 "XC"]
                 [50 "L"]
                 [40 "XL"]
                 [10 "X"]
                 [9 "IX"]
                 [5 "V"]
                 [4 "IV"]
                 [1 "I"]]
          reducto (fn reducto [n lst]
                    (if-let [[lim symb] (first lst)]
                      (if (<= lim n)
                        (cons symb (reducto (- n lim) lst))
                        (recur n (rest lst))))) ]

      (apply str (reducto n subs)))))

(defcheck solution-1aee6d2e
  (fn d->r [d]
    (let [ds (for [dd (iterate #(quot % 10) d)] (mod dd 10))
          rs [["" "I" "II" "III" "VI" "V" "IV" "IIV" "IIIV" "XI"]
              ["" "X" "XX" "XXX" "LX" "L" "XL" "XXL" "XXXL" "CX"]
              ["" "C" "CC" "CCC" "DC" "D" "CD" "CCD" "CCCD" "MC"]
              ["" "M" "MM" "MMM"]]
          ]
      (apply str (reverse (mapcat #(get %1 %2) rs ds)))
      )

    ))

(defcheck solution-1c10c101
  (fn [n]
    (let [numerals ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          integers [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          places   (map #(int (Math/floor %))
                     (map /
                       (reductions rem n integers)
                       integers))]
      (apply str (mapcat repeat places numerals)))))

(defcheck solution-1d068f04
  (fn wrn [n]
    (cond (>= n 1000) (str \M (wrn (- n 1000)))
          (>= n 900) (str \C \M (wrn (- n 900)))
          (>= n 500) (str \D (wrn (- n 500)))
          (>= n 400) (str \C \D (wrn (- n 400)))
          (>= n 100) (str \C (wrn (- n 100)))
          (>= n 90) (str \X \C (wrn (- n 90)))
          (>= n 50) (str \L (wrn (- n 50)))
          (>= n 40) (str \X \L (wrn (- n 40)))
          (>= n 10) (str \X (wrn (- n 10)))
          (>= n 9) (str \I \X (wrn (- n 9)))
          (>= n 5) (str \V (wrn (- n 5)))
          (>= n 4) (str \I \V (wrn (- n 4)))
          (>= n 1) (str \I (wrn (- n 1)))
          :else "")))

(defcheck solution-1d0e2268
  (fn int2r [n]
    (cond (> n 1999) (clojure.string/join (concat (repeat (quot n 1000) \M) (int2r (rem n 1000))))
          (> n 999) (clojure.string/join (concat \M (int2r (rem n 1000))))
          (> n 899) (clojure.string/join (concat "CM" (int2r (rem n 900))))
          (> n 499) (clojure.string/join (concat "D" (int2r (rem n 500))))
          (> n 399) (clojure.string/join (concat "CD" (int2r (rem n 900))))
          (> n 99) (clojure.string/join (concat (repeat (quot n 100) \C) (int2r (rem n 100))))
          (> n 89) (clojure.string/join (concat "XC" (int2r (rem n 90))))
          (> n 49) (clojure.string/join (concat "L" (int2r (rem n 50))))
          (> n 39) (clojure.string/join (concat "XL" (int2r (rem n 40))))
          (> n 9) (clojure.string/join (concat (repeat (quot n 10) \X) (int2r (rem n 10))))
          (= n 9) "IX"
          (> n 4) (clojure.string/join (concat "V" (int2r (rem n 5))))
          (= n 4) "IV"
          (> n 0) (clojure.string/join (repeat n "I"))
          :else "")))

(defcheck solution-1d165e7a
  (fn roman-numerals [n]
    (if (zero? n)
      ""
      (let [
            conversions [
                         [1000 "M"]
                         [900 "CM"]
                         [500 "D"]
                         [400 "CD"]
                         [100 "C"]
                         [90 "XC"]
                         [50 "L"]
                         [40 "XL"]
                         [10 "X"]
                         [9 "IX"]
                         [5 "V"]
                         [4 "IV"]
                         [1 "I"]
                         ]
            [arabic roman] (first (filter #(>= n (first %)) conversions))]
        (str roman (roman-numerals (- n arabic)))))))

(defcheck solution-1d4b3b86
  (fn arab->rome
    ([arab] (arab->rome arab ""))
    ([arab res]
     (if (<= arab 0)
       res
       (let [rules {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
                    90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
             closest (first (filter (partial >= arab) (sort-by - (keys rules))))
             rome (rules closest)]
         (recur (- arab closest) (str res rome)))))))

(defcheck solution-1d82d9b1
  (fn [s]
    (let [roman-numerals ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          roman-shape [1000 900 500 400 100 90 50 40 10 9 5 4 1]]
      (letfn [(digits-of [[shape-first & shape-rest] n]
                (when-not (nil? shape-first)
                  (cons
                    (quot n shape-first)
                    (digits-of shape-rest (mod n shape-first)))))]
        (let [digits (vec (digits-of roman-shape s))]
          (apply str (mapcat #(take %1 (repeat %2)) digits roman-numerals)))))))

(defcheck solution-1d880b11
  (fn [n]
    (loop [res ""
           charset [[\I \V] [\X \L] [\C \D] [\M]]
           n n]
      (if (zero? n) res
                    (let [m (rem n 10)
                          [[I  V] [X _]] (take 2 charset)
                          res (str (cond
                                     (= m 9) (str I X)
                                     (>= m 5) (apply str V (repeat (rem m 5) I))
                                     (= m 4) (str I V)
                                     :else (apply str (repeat m I))) res)]
                      (recur res (rest charset) (quot n 10)))))))

(defcheck solution-1dcbf66f
  #(first (reduce (fn [[acc n] [m s]]
                    [(apply str acc (repeat (quot n m) s)), (rem n m)])
            ["" %]
            [[1000 "M"][900 "CM"][500 "D"][400 "CD"][100 "C"][90 "XC"]
             [50 "L"][40 "XL"][10 "X"][9 "IX"][5 "V"][4 "IV"][1 "I"]])))

(defcheck solution-1de29d
  (fn numerals [n]
    (let [values
          [[1000 "M"]
           [900  "CM"]
           [500  "D"]
           [400  "CD"]
           [100  "C"]
           [90   "XC"]
           [50   "L"]
           [40   "XL"]
           [10   "X"]
           [9    "IX"]
           [5    "V"]
           [4    "IV"]
           [1    "I"]
           [0    ""]]
          [subn numeral] (some #(if (<= (first %) n) % nil) values)]
      (if (= 0 n) numeral
                  (str numeral (numerals (- n subn)))))))

(defcheck solution-1dfbae11
  (fn write-roman [x]
    (let [romans [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]]
      (->>
        (loop [number x
               [n r & more :as whole] romans
               res '()]
          (cond
            (zero? number) res
            (> n number) (recur number more res)
            true (recur (- number n) whole (cons r res))
            )
          )
        (reverse)
        (apply str)
        )
      )))

(defcheck solution-1e3907c3
  (fn rom [x]
    (if (= x 0) ""
                (if (>= x 1000) (str "M" (rom (- x 1000)))
                                (if (>= x 900) (str "C" (rom (+ x 100)))
                                               (if (>= x 500) (str "D" (rom (- x 500)))
                                                              (if (>= x 400) (str "C" (rom (+ x 100)))
                                                                             (if (>= x 100) (str "C" (rom (- x 100)))
                                                                                            (if (>= x 90) (str "X" (rom (+ x 10)))
                                                                                                          (if (>= x 50) (str "L" (rom (- x 50)))
                                                                                                                        (if (>= x 40) (str "X" (rom (+ x 10)))
                                                                                                                                      (if (>= x 10) (str "X" (rom (- x 10)))
                                                                                                                                                    (if (>= x 9) (str "I" (rom (+ x 1)))
                                                                                                                                                                 (if (>= x 5) (str "V" (rom (- x 5)))
                                                                                                                                                                              (if (>= x 4) (str "I" (rom (+ x 1)))
                                                                                                                                                                                           (if (>= x 1) (str "I" (rom (- x 1)))
                                                                                                                                                                                                        ))))))))))))))))

(defcheck solution-1e9e152b
  (fn roman [n]
    (cond (= n 0) ""
          (<= n 3) (apply str (take n (repeatedly (constantly "I"))))
          (= n 4) "IV"
          (< 4 n 9) (str "V" (roman (- n 5)))
          (= n 9) "IX"
          (< 9 n 40) (str "X" (roman (- n 10)))
          (< 39 n 50) (str "XL" (roman (- n 40)))
          (< 49 n 90) (str "L" (roman (- n 50)))
          (< 90 n 100) (str "XC" (roman (- n 90)))
          (< 99 n 400) (str "C" (roman (- n 100)))
          (< 399 n 500) (str "CD" (roman (- n 400)))
          (< 499 n 900) (str "D" (roman (- n 500)))
          (< 899 n 1000) (str "CM" (roman (- n 900)))
          :else (str "M" (roman (- n 1000))))))

(defcheck solution-1eb8519d
  (fn wnum [n]
    (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
              90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          m (some #(when (>= (- n %) 0) %) (keys r))]
      (when-not (nil? m)
        (str (r m) (wnum (- n m)))))))

(defcheck solution-1eda2e0f
  (fn [n] (let [q #(quot % %2) m #(mod % %2) j #(reduce str (repeat % %2)) p #(if (= % 9) (str %2 %3) (if (= % 4) (str %2 %4) (if (> % 4) (str %4 (j (- % 5) %2)) (j % %2)))) x #(p (q (m n %) %2) %3 %4 %5)] (str (j (q n 1000) \M) (x 1000 100 \C \M \D) (x 100 10 \X \C \L) (x 10 1 \I \X \V) ))))

(defcheck solution-1ef1e640
  (fn [n]
    (let [trans {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL"  10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (->> (reverse (sort (keys trans)))                                    ;; sorted base 1000, 900, 500 ...
        (reduce #(cons (rem (first %1) %2 )                              ;; keep last rem result in head
                   (cons [%2 (quot (first %1) %2)] (rest %1))) [n])  ;; append [base,quotient] per base
        rest reverse                                                     ;; rest remove last rem result (0)
        (map #(apply str (repeat (% 1) (trans (% 0)))))                  ;; convert [[base,quotient]] to ["quotient*trans[base]"]
        (apply str)))))

(defcheck solution-1f0ea241
  (fn f [n]
    (if (= n 0)
      ""
      (let [s '([1000 "M"] [900 "CM"] [500 "D"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"])
            [d l] (first (filter (fn [[r t]] (<= r n)) s))]
        (str l (f (- n d)))))
    ))

(defcheck solution-1f36afa0
  (let [m {1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"
           10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC"
           100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC" 900 "CM"
           1000 "M" 2000 "MM" 3000 "MMM"}]

    (fn [n] (str
              (m (* 1000 (quot n 1000)))
              (m  (* 100 (quot (mod n 1000) 100)))
              (m  (* 10 (quot (mod n 100) 10)))
              (m  (mod n 10))
              )
      )))

(defcheck solution-1f53dd8e
  (fn roman [n]
    (apply str (cond
                 (zero? n) []
                 (>= n 1000) (cons \M (roman (- n 1000)))
                 (>= n 900) (concat [\C \M] (roman (- n 900)))
                 (>= n 500) (cons \D (roman (- n 500)))
                 (>= n 400) (concat [\C \D] (roman (- n 400)))
                 (>= n 100) (cons \C (roman (- n 100)))
                 (>= n 90) (concat [\X \C] (roman (- n 90)))
                 (>= n 50) (cons \L (roman (- n 50)))
                 (>= n 40) (concat [\X \L] (roman (- n 40)))
                 (>= n 10) (cons \X (roman (- n 10)))
                 (>= n 9) (concat [\I \X] (roman (- n 9)))
                 (>= n 5) (cons \V (roman (- n 5)))
                 (>= n 4) (concat [\I \V] (roman (- n 4)))
                 (>= n 1) (cons \I (roman (- n 1)))))))

(defcheck solution-1f8175ac
  (let
   [numerals
               (sorted-map 1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000 "M")
    subtrahend [1 10 100]]
    (fn q4q104 [n]
      "Write n in roman numerals"
      ((fn numeral [n-i rn]
         (if (zero? n-i)
           rn
           (let [[v num] (first (rsubseq numerals <= n-i))
                 [vu numu] (first (subseq numerals > n-i))
                 sh (when-not (nil? vu) (first (filter #(< % vu) (reverse subtrahend))))
                 q       (quot n-i v)]
             (if (and
                  (not (nil? vu))
                  (not (nil? sh))
                  (>= n-i (- vu sh)))
               (recur (- n-i (- vu sh)) (apply str rn (get numerals sh) numu))
               (recur (- n-i (* v q)) (apply str rn (repeat q num))) )))) n ""))))

(defcheck solution-1feff200
  (fn [n]
    (let [vals [[1000 "M"] [ 900 "CM"]
                [ 500 "D"] [ 400 "CD"]
                [ 100 "C"] [  90 "XC"]
                [  50 "L"] [  40 "XL"]
                [  10 "X"] [   9 "IX"]
                [   5 "V"] [   4 "IV"]
                [   1 "I"]]
          numerals (fn [nums n]
                     (if-let [[[val num]] (seq (filter #(>= n (first %)) vals))]
                       (recur (conj nums num) (- n val))
                       nums))]
      (apply str (numerals [] n)))))

(defcheck solution-1ff4a6bd
  (fn rn[x]
    (letfn [(r [n]
              (cond (= n 0) '()
                    (>= n 1000) (conj (r (- n 1000)) \M )
                    (>= n  900) (conj (r (- n  900)) \M \C)
                    (>= n  500) (conj (r (- n  500)) \D )
                    (>= n  400) (conj (r (- n  400)) \D \C)
                    (>= n  100) (conj (r (- n  100)) \C )
                    (>= n   90) (conj (r (- n   90)) \C \X )
                    (>= n   50) (conj (r (- n   50)) \L )
                    (>= n   40) (conj (r (- n   40)) \L \X )
                    (>= n   10) (conj (r (- n   10)) \X )
                    (>= n    9) (conj (r (- n    9)) \X \I )
                    (>= n    5) (conj (r (- n    5)) \V )
                    (>= n    4) (conj (r (- n    4)) \V \I )
                    (>= n    1) (conj (r (- n    1)) \I  )))]
      (apply str (r x)))))

(defcheck solution-20494ef4
  (fn [n]
    (let [i ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          x ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          c ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          m (vec (for [x (range 10)]
                   (clojure.string/join
                     (repeat x "M"))))
          dr [i x c m]

          digits (map (zipmap "0123456789"
                        (range))
                   (seq (str n)))]
      (clojure.string/join
        (reverse
          (map #(get %1 %2)
            dr
            (reverse digits)))))))

(defcheck solution-2099178b
  #(first
     (reduce (fn [[acc remaining] [s v]]
               [(apply str acc (repeat (quot remaining v) s)) (rem remaining v)]
               ) ["" %]
       (map vector (map str '(M CM D CD C XC L XL X IX V IV I)) [1000 900 500 400 100 90 50 40 10 9 5 4 1]))))

(defcheck solution-20cd5f77
  (fn roman-numerals
    [num]
    (let [roman-numeral-from-digit
                   (fn [digit p10]
                     (let [[one five ten]
                           (nth (partition 3 2 ["I" "V" "X" "L" "C" "D" "M" nil nil]) p10)]
                       (cond
                         (<= digit 3) (repeat digit one)
                         (<= digit 5) (concat (repeat    (- 5 digit) one) (list five))
                         (<= digit 8) (concat (list five) (repeat (- digit 5) one))
                         :else        (concat (repeat (- 10 digit) one) (list ten)))))
          numerals (fn this [remaining p10]
                     (when (<= 0 p10)
                       (let [n (int (/ remaining (Math/pow 10 p10)))]
                         (concat (roman-numeral-from-digit n p10)
                           (this (mod num (int (Math/pow 10 p10)))
                             (dec p10))))))]
      (apply str (numerals num (int (Math/log10 num)))))))

(defcheck solution-2152e64b
  (fn romanize [n]
    (if (zero? n) ""
                  (let [smap (sorted-map 1 "I", 4 "IV", 5 "V", 9 "IX",
                               10 "X", 40 "XL", 50 "L", 90 "XC",
                               100 "C", 400 "CD", 500 "D",
                               900 "CM", 1000 "M")
                        [k v] (last (take-while #(>= n (key %)) smap))]
                    (str v (romanize (- n k)))))))

(defcheck solution-2178f385
  (let [trans (sorted-map-by #(- (compare %1 %2))
                3000 "MMM", 2000 "MM", 1000 "M" , 900 "CM",
                500 "D", 400 "CD",
                300 "CCC", 200 "CC", 100 "C", 90 "XC",
                50 "L", 40 "XL",
                30 "XXX", 20 "XX", 10 "XX", 9 "IX",
                5 "V", 4 "IV",
                3 "III", 2 "II", 1 "I")
        to-roman (fn [num res]
                   (let [[k v] (first (subseq trans >= num))]
                     (if (= num 0)
                       res
                       (recur (- num k) (str res v)))))
        ]
    (fn [num] (to-roman num ""))))

(defcheck solution-219a1b90
  (fn [n]
    (apply str (let [ m  [  [1 "I"] [2 "II"] [3 "III"] [4 "IV"] [5 "V"] [9 "IX"] [10 "X"] [40 "XL"] [50 "L"] [90 "XC"] [100 "C"] [500 "D"] [900 "CM"] [1000 "M"]]
                     v  (fn [n m] (last (take-while #(<= (first %) n) m)))]
                 (loop [ c (v n m)
                        r (rem n (first c))
                        d (quot n (first c))
                        e [] ]
                   (cond
                     (= 0 r) (apply conj e (repeat d (second c)))
                     (= 0 d) (apply str (conj e (second c)))
                     :else (recur
                             (v r m)
                             (rem r (first (v r m)) )
                             (quot r (first (v r m)))
                             (apply conj e (repeat d (second c))))))))))

(defcheck solution-21a38940
  (fn to-roman [n]
    (let [lookup {
                  0 [ "" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                  1 [ "" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC" ]
                  2 [ "" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM" ]
                  3 [ "" "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM"]
                  }
          decmp (fn decmp [n]
                  (reverse (map #(parse-int (str %)) (str n)))
                  )]
      (apply str (reverse
                   (map-indexed (fn [i x] (nth (lookup i) x)) (decmp n))))

      )

    ))

(defcheck solution-21e9a944
  (fn [n]
    (let [key {1    "I",   4 "IV",   5 "V",   9 "IX",
               10   "X",  40 "XL",  50 "L",  90 "XC",
               100  "C", 400 "CD", 500 "D", 900 "CM",
               1000 "M"}
          vals (keys key)]
      (letfn [(next-component [n]
                (apply max (filter #(>= n %) vals)))
              (roman-components [n]
                (if (pos? n)
                  (let [c (next-component n)]
                    (cons c (lazy-seq (roman-components (- n c)))))))]
        (clojure.string/join (map key (roman-components n)))))))

(defcheck solution-224819d5
  (fn f [n]
    (let [m (sorted-map-by > 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
              90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")
          p (first (drop-while #(< n (first %)) m))]
      (if (empty? p)
        ""
        (str (second p) (f (- n (first p))))))))

(defcheck solution-2251da1b
  (fn[n]
    (loop [drum [["M" 1000]["CM" 900]["D" 500]["CD" 400]["C" 100]["XC" 90]
                 ["L" 50]["XL" 40] ["X" 10]["IX" 9]["V" 5]["IV" 4] ["I" 1]]
           in  n
           out []]

      (let [head (first drum)
            lit (first head)
            val (second head)]

        (cond
          (zero? in)
          (apply str (interpose "" out))

          :else
          (if (<= val in)
            (recur drum (- in val) (conj out lit))
            (recur (rest drum) in out)))))))

(defcheck solution-22dbef53
  #((fn recur-digits [number numvec resstr](if (> number 0)(recur-digits (int (/ number 10)) (nthrest numvec 2) (str ((fn[n numvec](let [v (nth [[] [0] [0 0] [0 0 0] [0 1] [1] [1 0] [1 0 0] [1 0 0 0] [0 2]] n)](apply str (for [i (range (count v))] (nth numvec (nth v i)))))) (mod number 10) (take 3 numvec)) resstr))resstr)) % [\I \V \X \L \C \D \M] ""))

(defcheck solution-2334cede
  (fn [n]
    (let  [rmap {1000 "M",900 "CM",500 "D",400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"}]
      (first (reduce
               (fn [[s n] e]
                 (loop [s s n n]
                   (if (< n e)
                     [s n]
                     (recur (str s (rmap e)) (- n e)))))
               ["" n] (reverse (sort (keys rmap))))))))

(defcheck solution-24061dd2
  (fn [d]
    (let [m {1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M}
          seqs {\1 '(1) \2 '(1 1) \3 '(1 1 1) \4 '(1 5) \5 '(5)
                \6 '(5 1) \7 '(5 1 1) \8 '(5 1 1 1) \9 '(1 10)}
          strd (str d)]
      (letfn [(digitify [s] (map #(* % (apply * (repeat (dec (count s)) 10)))
                              (get seqs (first s))))]
        (apply str (mapcat #(map (fn [digit] (get m digit)) %)
                     (map digitify (filter #(not= (first %) \0)
                                     (map #(drop % strd)
                                       (range (count (str d))))))))))))

(defcheck solution-247624f7
  (fn number-to-roman-numeral [n]
    (let [sym-table {1     "I", 4    "IV", 5   "V", 9   "IX",
                     10    "X", 40   "XL", 50  "L", 90  "XC",
                     100   "C", 400  "CD", 500 "D", 900 "CM"
                     1000  "M"}
          sym-keys (keys sym-table)]

      (loop [remainder n
             result   []]
        (if (zero? remainder)
          (apply str result)
          (let [min-num-sym (reduce max (filter #(<= %1 remainder) sym-keys))
                min-sym     (sym-table min-num-sym)]
            (recur (- remainder min-num-sym) (conj result min-sym))))))))

(defcheck solution-25280a4a
  (fn write-roman
    [num]
    (loop [data {1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}
           data-keys (reverse (sort (keys data)))
           remaining num
           res []]
      (cond
        (zero? remaining) (apply str res)
        (empty? data-keys) (apply str res)
        (> (quot remaining (first data-keys)) 0) (recur data data-keys (- remaining (first data-keys))
                                                   (conj res (data (first data-keys))))
        (= (quot remaining (first data-keys)) 0) (recur data (rest data-keys) remaining
                                                   res)))))

(defcheck solution-25d3a53d
  (fn [x]
    (let [table [ ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                 ["""X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                 ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                 ["" "M" "MM" "MMM" "MMMM"] ]
          letters (fn lp [x i] (if (not= 0 x) (concat (lp (quot x 10) (inc i)) ((table i) (rem x 10)))))]
      (clojure.string/join (letters x 0)))))

(defcheck solution-25e6c1a0
  (fn __ [roman]
    (let [table
          {1000 "M" 900 "CM" 500 "D" 400 "CD"
           100 "C" 90 "XC" 50 "L" 40 "XL"
           10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I" 0 ""}]
      (loop [cur roman final "" numerator 0]
        (if (= 0 cur)
          final
          (recur
            (- cur numerator)
            (str final (table numerator))
            (apply (partial max-key identity)
              (filter (partial >= (- cur numerator)) (keys table)))))))))

(defcheck solution-26781f5d
  (fn [k]
    (letfn [(writetoroman [x]
              (let [romanchar (sorted-map-by > 0 "" 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 500 "D" 900 "CM" 1000 "M")]
                (letfn [(sublargest [y]  (first (filter #(>= (first %) 0) (map #(vector (- y (first %)) (romanchar (first %))) romanchar))))]
                  (if (zero? x) nil
                                (cons (second (sublargest x)) (writetoroman (first (sublargest x))))))))]
      (apply str (writetoroman k)))))

(defcheck solution-2694bbd
  (fn romanize [n]
    (let [
          numerals (array-map
                     1000 "M"
                     900 "CM"
                     500 "D"
                     400 "CD"
                     100 "C"
                     90 "XC"
                     50 "L"
                     40 "XL"
                     10 "X"
                     9 "IX"
                     5 "V"
                     4 "IV"
                     1 "I")
          num (some #(when (>= n %) %) (keys numerals))
          part (numerals num)
          n' (- n num)
          ]
      (if (> n' 0) (str part (romanize n')) part)
      )))

(defcheck solution-269ff8c
  (fn [n]
    (let [rm {\0 ["" "" "" ""]
              \1 ["M" "C" "X" "I"]
              \2 ["MM" "CC" "XX" "II"]
              \3 ["MMM" "CCC" "XXX" "III"]
              \4 ["" "CD" "XL" "IV"]
              \5 ["" "D" "L" "V"]
              \6 ["" "DC" "LX" "VI"]
              \7 ["" "DCC" "LXX" "VII"]
              \8 ["" "DCCC" "LXXX" "VIII"]
              \9 ["" "CM" "XC" "IX"]}
          s (str n)]
      (first (reduce (fn [[a i] e]
                       [(str a ((rm e) i)) (inc i)])
               ["" (- 4 (count s))] s)))))

;; See https://clojure.atlassian.net/browse/CLJS-3173
#_(defcheck solution-26d53408
  (fn [n]
    (let [digits (map (comp read-string str) (seq (str n)))
          d-base (map-indexed (fn [idx v] [(- (count digits) idx) v]) digits)
          roman-with-base (fn [b half]
                            #_(println "matching" b " " half " " (list b half))
                            (case (list b half)
                              ((4 false)) \M
                              ((4 true)) \D
                              ((3 false)) \C
                              ((3 true)) \L
                              ((2 false)) \X
                              ((2 true)) \V
                              ((1 false)) \I))
          convert (fn [[base v]]
                    (case v
                      (1 2 3) (repeat v (roman-with-base base false))
                      4 (list (roman-with-base base false)
                          (roman-with-base (inc base) true))
                      5 (list (roman-with-base base true))
                      (6 7 8) (cons (roman-with-base (inc base) true)
                                (repeat (mod v 5)
                                  (roman-with-base base false)))
                      9 (list (roman-with-base base false)
                          (roman-with-base (inc base) false))
                      0 nil))]
      (clojure.string/join (mapcat convert d-base)))))

(defcheck solution-26f256b1
  (fn [n]
    (let [roman-map
                       {3000 "MMM", 2000 "MM", 1000 "M",
                        900 "CM", 800 "DCCC", 700 "DCC", 600 "DC", 500 "D" 400 "CD", 300 "CCC", 200 "CC", 100 "C",
                        90 "XC", 80 "LXXX", 70 "LXX", 60 "LX", 50 "L", 40 "XL", 30 "XXX", 20 "XX", 10 "X",
                        9 "IX", 8 "VIII", 7 "VII", 6 "VI", 5 "V", 4 "IV", 3 "III", 2 "II", 1 "I"}
          dec-to-roman (fn [[n acc] d] [(rem n d) (conj acc (roman-map (* (quot n d) d)))])]
      (apply str (filter (comp not nil?) (second (reduce dec-to-roman [n []] [1000 100 10 1])))))))

(defcheck solution-27a3eff3
  (fn [n]
    (apply str
      (loop [n n o []
             x [1000 900  500 400  100 90   50  40   10  9    5   4    1]
             y ["M"  "CM" "D" "CX" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]]
        (if (empty? x)
          o
          (if (< n (first x))
            (recur n o (next x) (next y))
            (recur (- n (first x)) (conj o (first y)) x y)
            )
          )
        )
      )
    ))

(defcheck solution-2880a99
  (let [c (seq "IVXLCDM")]
    (fn fun [n]
      (apply str
        ((fn efun [n c]
           (if (zero? n)
             ()
             (let [r (rem n 10)
                   res (cond
                         (<= r 3) (repeat r (first c))
                         (<= r 5) (concat (repeat (- 5 r) (first c)) (list (second c)))
                         (<= r 8) (cons (second c) (repeat (- r 5) (first c)))
                         (= r 9) (list (first c) (second (rest c)))
                         )]
               (concat (efun (quot n 10) (nnext c)) res ))))
         n c)))))

(defcheck solution-28b9abd2
  (fn
    [n]
    (let [m {1 "I" 4 "IV" 5  "V" 9 "IX" 10  "X" 40  "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          v [1 4 5 9 10 40 50 90 100 400 500 900 1000]]
      (loop [num n
             r ""]
        (if (= num 0)
          r
          (let [d (last(take-while #(<= % num) v))]
            (recur (rem num d) (apply str r (repeat (int (/ num d)) (m d))))))))))

(defcheck solution-28c591f6
  (fn f [[n & a] [s & b] o i]
    (if n
      (f a b (into o (repeat (int (/ i n)) s)) (rem i n))
      (apply str o))) [1000 900 500 400 100 90 50 40 10  9 5  4 1] '[  M  CM   D  CD   C XC  L XL  X IX V IV I] [])

(defcheck solution-28ced562
  (fn [n]
    (let [d2r {\0 {0 ""     1 ""      2 ""     3 ""}
               \1 {0 "I"    1 "X"     2 "C"    3 "M"}
               \2 {0 "II"   1 "XX"    2 "CC"   3 "MM"}
               \3 {0 "III"  1 "XXX"   2 "CCC"  3 "MMM"}
               \4 {0 "IV"   1 "XL"    2 "CD"   3 "MMMM"}
               \5 {0 "V"    1 "L"     2 "D"    3 "MMMMM"}
               \6 {0 "VI"   1 "LX"    2 "DC"   3 "MMMMMM"}
               \7 {0 "VII"  1 "LXX"   2 "DCC"  3 "MMMMMMM"}
               \8 {0 "VIII" 1 "LXXX"  2 "DCCC" 3 "MMMMMMMM"}
               \9 {0 "IX"   1 "XC"    2 "CM"   3 "MMMMMMMMM"}}]
      (apply str (reverse (map-indexed #(get-in d2r [%2 %]) (reverse (str n))))))))

(defcheck solution-29049bae
  (let [powers (map #(apply * (repeat % 10)) (reverse (range 4)))
        powahs (partial zipmap powers)
        lookup (->> [["M" "C" "X" "I"]
                     ["MM" "CC" "XX" "II"]
                     ["MMM" "CCC" "XXX" "III"]
                     ["MMMM" "CD" "XL" "IV"]
                     ["MMMMM" "D" "L" "V"]
                     ["MMMMMM" "DC" "LX" "VI"]
                     ["MMMMMMM" "DCC" "LXX" "VII"]
                     ["MMMMMMMM" "DCCC" "LXXX" "VIII"]
                     ["MMMMMMMMM" "CM" "XC" "IX"]]
                 (map (fn [i vs] [i (powahs vs)])
                   (rest (range)))
                 (into {}))
        ->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                     (take-while (fn [vs] (some (complement zero?) vs)))
                     (map second)
                     rest
                     reverse
                     (into [])))
        f (fn [n]
            (let [digits (->digits n)
                  places (->> (count digits)
                           range
                           reverse
                           (map #(apply * (repeat % 10))))]
              (->> (map vector digits places)
                (remove (fn [[d _]] (zero? d)))
                (map (fn [ks] (get-in lookup ks)))
                (apply str))))]
    f))

(defcheck solution-290a3b4
  (fn
    [n]
    (letfn [(process-numeral
              [arabic roman acc]
              (loop
               [acc-arabic (first acc)
                acc-roman (second acc)]
                (if (< acc-arabic arabic)
                  [acc-arabic acc-roman]
                  (recur
                    (- acc-arabic arabic)
                    (str acc-roman roman))
                  )
                ))]
      (->>
        [n ""]
        (process-numeral 1000 "M")
        (process-numeral 900 "CM")
        (process-numeral 500 "D")
        (process-numeral 100 "C")
        (process-numeral 90 "XC")
        (process-numeral 50 "L")
        (process-numeral 40 "XL")
        (process-numeral 10 "X")
        (process-numeral 9 "IX")
        (process-numeral 5 "V")
        (process-numeral 4 "IV")
        (process-numeral 1 "I")
        (second)
        ))))

(defcheck solution-29798513
  (fn to-roman [x]
    (condp <= x
      1000 (str  "M" (to-roman (- x 1000)))
      900 (str  "CM" (to-roman (- x 900)))
      500 (str "D" (to-roman (- x 500)))
      400 (str "CD" (to-roman (- x 400)))
      100 (str "C" (to-roman (- x 100)))
      90 (str "XC" (to-roman (- x 90)))
      50 (str "L" (to-roman (- x 50)))
      40 (str "XL" (to-roman (- x 40)))
      10 (str "X" (to-roman (- x 10)))
      9 (str "IX" (to-roman (- x 9)))
      5 (str "V" (to-roman (- x 5)))
      4 (str "IV" (to-roman (- x 4)))
      1 (str "I" (to-roman (- x 1)))
      "")))

(defcheck solution-29d130db
  (fn r [n]
    (loop [n n a []]
      (cond
        (>= n 1000) (recur (- n 1000) (conj a "M"))
        (>= n 900) (recur (- n 900) (conj a "CM"))
        (>= n 500) (recur (- n 500) (conj a "D"))
        (>= n 400) (recur (- n 400) (conj a "CD"))
        (>= n 100) (recur (- n 100) (conj a "C"))
        (>= n 90) (recur (- n 90) (conj a "XC"))
        (>= n 50) (recur (- n 50) (conj a "L"))
        (>= n 40) (recur (- n 40) (conj a "XL"))
        (>= n 10) (recur (- n 10) (conj a "X"))
        (>= n 9) (recur (- n 9) (conj a "IX"))
        (>= n 5) (recur (- n 5) (conj a "V"))
        (>= n 4) (recur (- n 4) (conj a "IV"))
        (>= n 1) (recur (- n 1) (conj a "I"))
        :else (apply str a)))))

(defcheck solution-29fa7f6a
  (fn roman [x]
    (let [rom (rseq (sorted-map 1000 "M" 900 "CM" 500 "D"
                      400 "CD" 100 "C" 90 "XC"
                      50 "L" 40 "XL" 10 "X" 9 "IX"
                      5 "V" 4 "IV" 1 "I"))]
      (loop [i x
             r ""]
        (if (zero? i)
          r
          (let [[v s]
                (first (filter (fn [[k v]] (<= k i)) rom))]
            (recur (- i v) (str r s))))))))

(defcheck solution-29fe8585
  (fn to-roman [a-num]
    (let [vals [[\I \V \X] [\X \L \C] [\C \D \M] [\M]]]
      (letfn [
              (nth-digit [a-num n]
                (rem (quot a-num (int (Math/pow 10 n))) 10)
                )
              (digit-to-roman [ digit args ]
                (let [ [o f t] args]
                  (cond
                    (< digit 4) (repeat digit o)
                    (= digit 4) [o f]
                    (< digit 9) (cons f (repeat (- digit 5) o))
                    :else [o t]
                    )
                  )
                )
              (nth-romans [a-num n]
                (digit-to-roman (nth-digit a-num n) (nth vals n))
                )
              ]
        (apply str (mapcat #(nth-romans a-num %) (range 3 -1 -1)))
        )
      )
    ))

(defcheck solution-2a2d9808
  #(apply str
     (second
       (reduce (fn [[n acc] [m s]]
                 (let [k (quot n m)]
                   [(- n (* k m)) (concat acc (repeat k s))]))
         [% ""]
         [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
          [100 "C"]  [90 "XC"]  [50 "L"]  [40 "XL"]
          [10 "X"]   [9 "IX"]   [5 "V"]   [4 "IV"]
          [1 "I"]]))))

(defcheck solution-2a32417b
  (fn [n]
    (case n
      1 "I"
      30 "XXX"
      4 "IV"
      140 "CXL"
      827 "DCCCXXVII"
      3999 "MMMCMXCIX"
      48 "XLVIII")))

(defcheck solution-2a81ae4b
  (fn [n]
    (loop [n n, res ""]
      (cond
        (> (quot n 1000) 0)
        (recur (- n 1000) (str res "M"))
        (>= n 900) (recur (- n 900) (str res "CM"))
        (>= n 800) (recur (- n 800) (str res "DCCC"))
        (>= n 700) (recur (- n 700) (str res "DCC"))
        (>= n 600) (recur (- n 600) (str res "DC"))
        (>= n 500) (recur (- n 500) (str res "D"))
        (>= n 400) (recur (- n 400) (str res "CD"))
        (>= n 100) (recur (- n 100) (str res "C"))
        (>= n 90) (recur (- n 90) (str res "XC"))
        (>= n 80) (recur (- n 80) (str res "XXC"))
        (>= n 70) (recur (- n 70) (str res "LXX"))
        (>= n 60) (recur (- n 60) (str res "LX"))
        (>= n 50) (recur (- n 50) (str res "L"))
        (>= n 40) (recur (- n 40) (str res "XL"))
        (>= n 30) (recur (- n 30) (str res "XXX"))
        (>= n 20) (recur (- n 20) (str res "XX"))
        (>= n 10) (recur (- n 10) (str res "X"))
        (= n 9) (str res "IX")
        (= n 8) (str res "VIII")
        (= n 7) (str res "VII")
        (= n 6) (str res "VI")
        (= n 5) (str res "V")
        (= n 4) (str res "IV")
        (= n 3) (str res "III")
        (= n 2) (str res "II")
        (= n 1) (str res "I")
        :else res))))

(defcheck solution-2b02451a
  (let [romans [[1000 "M"] [900 "CM"]
                [500 "D"] [400 "CD"]
                [100 "C"] [90 "XC"]
                [50 "L"] [40 "XL"]
                [10 "X"] [9 "IX"]
                [5 "V"] [4 "IV"]
                [1 "I"]]
        find-next-roman (fn [n]
                          (reduce (fn [res pair]
                                    (if (seq res)
                                      res
                                      (when (>= n (first pair)) pair)))
                            nil
                            romans))]
    (fn n->romans [n]
      (loop [n n, res ""]
        (if (> n 0)
          (let [[sub roman-digit] (find-next-roman n)]
            (recur (- n sub) (str res roman-digit)))
          res)))))

(defcheck solution-2b740316
  (fn [num]
    (let [
          roman-digit (fn [digit [one five ten]]
                        (if (< digit 4) (clojure.string/join
                                          (repeat digit one))
                                        (if (= digit 4) (str one five)
                                                        (if (= digit 5) five
                                                                        (if (< digit 9) (str five (clojure.string/join
                                                                                                    (repeat (- digit 5) one)))
                                                                                        (str one ten))))))
          roman-numerals (fn roman-numerals [num letters]
                           (let [
                                 head (mod num 10)
                                 tail (int (/ num 10))]
                             (str
                               (if (zero? tail)
                                 ""
                                 (roman-numerals tail (nthrest letters 2)))
                               (roman-digit head (take 3 letters)))))]
      (roman-numerals num ["I" "V" "X" "L" "C" "D" "M"]))))

(defcheck solution-2b8ae9fc
  (fn roman [n]
    (let [r (fn [n u h f]
              (apply str (cond
                           (< n 4) (repeat n u)
                           (= n 4) (list u h)
                           (= n 9) (list u f)
                           :else
                           (cons h (repeat (- n 5) u)))))]
      (apply str (r (mod (quot n 1000) 10) "M" "?" "!")
        (r (mod (quot n 100) 10) "C" "D" "M")
        (r (mod (quot n 10) 10) "X" "L" "C")
        (r (mod n 10) "I" "V" "X")))))

(defcheck solution-2b8e898e
  (fn wrn [n]
    (if (zero? n)
      nil
      (condp <= n
        1000 (str \M (wrn (- n 1000)))
        900 (str \C \M (wrn (- n 900)))
        500 (str \D (wrn (- n 500)))
        400 (str \C \D (wrn (- n 400)))
        100 (str \C (wrn (- n 100)))
        90 (str \X \C (wrn (- n 90)))
        50 (str \L (wrn (- n 50)))
        40 (str \X \L (wrn (- n 40)))
        10 (str \X (wrn (- n 10)))
        9 (str \I \X (wrn (- n 9)))
        5 (str \V (wrn (- n 5)))
        4 (str \I \V (wrn (- n 4)))
        1 (str \I (wrn (- n 1)))
        )
      )
    ))

(defcheck solution-2bc97503
  (fn roman [x]
    (let [romans {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}
          y (last (filter #(<= % x) (sort (keys romans))))]
      (if (= x y)
        (romans y)
        (str (romans y) (roman (- x y)))
        ))))

(defcheck solution-2c17b948
  #(apply str (second
                (reduce (fn [[n r] [v s]]
                          (if (< n v) [n r] [(- n v) (conj r s)])
                          )
                  [% []]
                  (apply concat
                    (for [
                          i (range 4)
                          j (range 4)
                          :let
                          [
                           m (Math/pow 10 (- 2 i))
                           n (nth [10 9 5 4] j)
                           k (* m n)
                           p (nth ["MCDC" "CXLX" "XIVI" "I   "]  i)
                           r (nth p j)
                           s (if (even? j) "" (nth p (dec j)) )
                           t (str r s)
                           ]
                          ]
                      (repeat 3 [k t])
                      )
                    )
                  ))))

(defcheck solution-2ca3ab4b
  (fn f [x]
    (let [m {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000
             "CM" 900 "XC" 90 "XL" 40 "IV" 4 "IX" 9}
          k (apply max-key #(if (<= (m %) x) (m %) -1) (keys m))]
      (if (zero? x)
        ""
        (str k (f (- x (m k))))))))

(defcheck solution-2ce6f3b0
  (fn roman [n]
    (letfn [
            (simple-digit [n [one five ten]]
              (cond
                (= n 0) ""
                (< n 4) (apply str (take n (repeat one)))
                (= n 4) (str one five)
                (= n 5) five
                (< n 9) (apply str (cons five (take (- n 5) (repeat one))))
                :else (str one ten)))
            ]
      (cond
        (< n 10)   (simple-digit n ["I" "V" "X"])
        (< n 100)  (str (simple-digit (quot n 10)  ["X" "L" "C"]) (roman (mod n 10)))
        (< n 1000) (str (simple-digit (quot n 100) ["C" "D" "M"]) (roman (mod n 100)))
        (< n 10000) (str (simple-digit (quot n 1000) ["M" "V!" "X!"]) (roman (mod n 1000)))

        :else ""))))

(defcheck solution-2d38fddd
  (fn [n]
    (loop [s (map (comp #(parse-int %) str) (seq (str n))) result []]
      (if (empty? s)
        (apply str (reverse result))
        (recur (butlast s) (conj result (get (get [["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                                                   ["X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                                                   ["C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                                                   ["M" "MM" "MMM"]] (count result)) (dec (last s)))))))))

(defcheck solution-2d48dfd9
  (fn [n]
    (let [m (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD",
              100 "C",  90 "XC",  50 "L", 40 "XL",
              10 "X",   9 "IX",   5 "V", 4 "IV",
              1 "I")]
      (loop [a [], r n]
        (if (zero? r)
          (apply str a)
          (let [k (first (filter #(<= % r) (keys m)))]
            (recur (conj a (m k)) (- r k))))))))

(defcheck solution-2dfadf24
  (fn rnum [n]
    (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          m (some #(when (>= (- n %) 0) %) (keys r))]
      (when-not (nil? m)
        (str (r m) (rnum (- n m)))))))

(defcheck solution-2e453794
  (fn roman-number
    ([n]    (roman-number "" n))
    ([s n]  (let [numerals [{ :value 1000, :chars "M" }
                            { :value 900,  :chars "CM"}
                            { :value 500,  :chars "D" }
                            { :value 400,  :chars "CD"}
                            { :value 100,  :chars "C" }
                            { :value 90,   :chars "XC"}
                            { :value 50,   :chars "L" }
                            { :value 40,   :chars "XL"}
                            { :value 10,   :chars "X" }
                            { :value 9,    :chars "IX"}
                            { :value 5,    :chars "V" }
                            { :value 4,    :chars "IV"}
                            { :value 1,    :chars "I" }]]
              (cond
                (zero? n) s
                :else (let [d (first (filter #(<= (:value %) n) numerals))]
                        (roman-number (str s (:chars d)) (- n (:value d)))))))))

(defcheck solution-2f66d690
  (fn [n]
    (let [maps [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [remain n result ""]
        (if (zero? remain) result
                           (let [[number ch] (some (fn [x] (if (>= (- remain (first x)) 0) x)) maps)]
                             (recur (- remain number)
                               (str result ch))))))))

(defcheck solution-2f939e7e
  (fn roman [n]
    (if (zero? n)
      ""
      (let
       [
        m {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 500 "D" 900 "CM" 1000 "M"}
        k (apply max (filter #(<= %1 n) (keys m)))
        ]
        (str (m k) (roman (- n k)))
        )
      )
    ))

(defcheck solution-2fd80e5d
  (fn f
    [n]
    (apply str (condp <= n
                 1000 (cons "M" (f (- n 1000)))
                 900 (cons "CM" (f (- n 900)))
                 500 (cons "D" (f (- n 500)))
                 400 (cons "CD" (f (- n 400)))
                 100 (cons "C" (f (- n 100)))
                 90 (cons "XC" (f (- n 90)))
                 50 (cons "L" (f (- n 50)))
                 40 (cons "XL" (f (- n 40)))
                 10 (cons "X" (f (- n 10)))
                 9 (cons "IX" (f (- n 9)))
                 5 (cons "V" (f (- n 5)))
                 4 (cons "IV" (f (- n 4)))
                 1 (cons "I" (f (- n 1)))
                 0 ()))))

(defcheck solution-302e1dc5
  (fn write-roman-numerals[num]
    (let[roman-number-map {"I" 1 "IV" 4 "V" 5 "IX" 9
                           "X" 10 "XL" 40 "L" 50 "XC" 90
                           "C" 100 "CD" 400 "D" 500  "CM" 900 "M" 1000}]
      (letfn[(step
               [n]
               (if-not(zero? n)
                 (let[k (last (sort #(<(second %1) (second %2)) (filter #(<= (second %)  n) roman-number-map)))
                      q (quot n (second k))
                      r (rem n (second k))]
                   (lazy-cat (map (fn[_] (first k)) (range q))
                     (step r)))))]
        (apply str (step num))))))

(defcheck solution-307c6896
  (fn
    [value]
    (str
      (get {0 "" 1 "M" 2 "MM" 3 "MMM"} (quot value 1000))
      (get {0 "" 1 "C" 2 "CC" 3 "CCC" 4 "CD" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"} (rem (quot value 100) 10))
      (get {0 "" 1 "X" 2 "XX" 3 "XXX" 4 "XL" 5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"} (rem (quot value 10) 10))
      (get {0 "" 1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"} (rem value 10)))))

(defcheck solution-30d3e9c1
  (fn [init]
    (let [numerals [[0 ""]
                    [1 "I"]
                    [4 "IV"]
                    [5 "V"]
                    [9 "IX"]
                    [10 "X"]
                    [40 "XL"]
                    [50 "L"]
                    [90 "XC"]
                    [100 "C"]
                    [400 "CD"]
                    [500 "D"]
                    [900 "CM"]
                    [1000 "M"]]
          done? (fn [[acc val]] (< val 0))
          convert-next (fn [[acc val]]
                         (if (zero? val)
                           [acc -1]
                           (let [match (last
                                         (take-while (fn [[arabic roman]]
                                                       (<= arabic val))
                                           numerals))]
                             [(str acc (second match))
                              (- val (first match))])))]
      (last
        (map first
          (take-while (complement done?)
            (iterate convert-next ["" init])))))))

(defcheck solution-30eaa1ec
  (fn [n]
    (let [m (partition 2 [1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"])]
      (loop [x n, r ""]
        (if (zero? x) r
                      (let [[a b] (some (fn [[a b]] (when (<= a x) [a b])) m)]
                        (recur (- x a) (str r b))))))))

(defcheck solution-314e6330
  (fn fin-rom [x]
    {:pre [(<= x 3999)]}
    (letfn [(roms [x]
              ({1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M} x))
            (topars [x]
              (reverse (map vector (reverse (into [] (map (comp read-string str) (seq (str x)))))  (iterate #(* % 10) 1))))
            (translate [v]
              (let [f (first v)]
                (cond
                  (<= f 3) (reduce str  (repeat f (roms (second v))))
                  (= f 4)  (str (translate [1 (second v)]) (translate [(inc f) (second v)]))
                  (= f 5) (str  (roms (* (first v) (second v))))
                  (<= f 8) (str (translate [5 (second v)]) (translate [(- f 5) (second v)]))
                  (= f 9) (str (translate [1 (second v)]) (translate [1 (* 10 (second v))])))))]
      (str (reduce str (map translate (topars x)))))))

(defcheck solution-31fc1ac7
  (fn [n]
    (let [nums {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                40 "XL" 50 "L" 90 "XC" 100 "C"
                400 "CD" 500 "D" 900 "CM" 1000 "M"}]
      (loop [acc []
             x n]
        (if (= 0 x)
          (clojure.string/join "" acc)
          (let [next-num (apply max (filter #(<= % x) (keys nums)))]
            (recur (conj acc (nums next-num)) (- x next-num))))))))

(defcheck solution-32b79dea
  (fn roman [n]
    (let [numerials [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
          next-biggest (fn [n] (->> numerials (filter #(<= (first %) n)) first))]
      (loop [n n s ""]
        (if (= n 0)
          s
          (let [[v k] (next-biggest n)]
            (recur (- n v) (str s k))))))))

(defcheck solution-32d25094
  (fn write-roman [integer]
    (let [numbers [1000 500 100 50 10 5 1]
          translation {1000 \M
                       500  \D
                       100  \C
                       50   \L
                       10   \X
                       5    \V
                       1    \I}
          subtractive {"VIIII" "IX"
                       "IIII"  "IV"
                       "LXXXX" "XC"
                       "XXXX"  "XL"
                       "DCCCC" "CM"
                       "CCCC"  "CD"}]
      (loop [string []
             number integer]
        (if (= 0 number)
          (clojure.string/replace
            (apply str string)
            #"V?IIII|L?XXXX|D?CCCC"
            #(subtractive %))
          (let [maximal (first (drop-while #(> % number) numbers))
                its-char (translation maximal)]
            (recur (conj string its-char)
              (- number maximal))))))))

(defcheck solution-32e689de
  #(loop [n % [[v r] & m] [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]] acc []]
     (if v
       (recur (rem n v) m (into acc (repeat (quot n v) r)))
       (apply str acc))))

(defcheck solution-32eb23cb
  (fn solve [n]
    (let [num->roman {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (->> (keys num->roman)
        sort
        reverse
        (reduce (fn [[roman m] d]
                  [(apply str roman (repeat (quot m d) (num->roman d))) (rem m d)])
          ["" n])
        first))))

(defcheck solution-32ee83d5
  (fn [d]
    (let [m {0 ["" "" "" ""]
             1 ["I" "X" "C" "M"]
             2 ["II" "XX" "CC" "MM"]
             3 ["III" "XXX" "CCC" "MMM"]
             4 ["IV" "XL" "CD" "MMMM"]
             5 ["V" "L" "D" "MMMMM"]
             6 ["VI" "LX" "DC" "MMMMMM"]
             7 ["VII" "LXX" "DCC" "MMMMMMM"]
             8 ["VIII" "LXXX" "DCCC" "MMMMMMMM"]
             9 ["IX" "XC" "CM" "MMMMMMMMM"]}]
      (loop [q d i 0 res ()]
        (if (pos? q)
          (recur (quot q 10) (inc i) (conj res (get-in m [(rem q 10) i])))
          (apply str res))))))

(defcheck solution-3331dc69
  (fn r [num]
    (let [m {1    \I
             4    "IV"
             5    \V
             9    "IX"
             10   \X
             40   "XL"
             50   \L
             90   "XC"
             100  \C
             400  "CD"
             500  \D
             900  "CM"
             1000 \M}]
      (loop [n num r ""]
        (if (zero? n) r
                      (let [x (map (fn [[k v]] (vector v (- n k))) m)
                            [res nl]
                            (->> x
                              (filter (comp not neg? second))
                              (apply min-key second))]
                        (recur nl (str r res))))))))

(defcheck solution-33f426b
  #(str ([ "" "M" "MM" "MMM"] (quot % 1000))
     ([ "" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] (mod (quot % 100) 10))
     ([ "" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] (mod (quot % 10) 10))
     ([ "" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (mod % 10))))

(defcheck solution-33f7b6c3
  (fn f [[c & r :as s]
         [n & m :as o] i]
    #_(prn c n i)
    (if c
      (if (<= n i)
        (str c (f s o (- i n)))
        (f r m i)))) '[M CM D CD C XC L XL X IX V IV I] [1000 900 500 400 100 90 50 40 10 9 5 4 1])

(defcheck solution-34141b04
  (fn num-roman [i] (loop [x i res []]
                      (cond
                        (>= x 1000) (recur (- x 1000) (conj res "M"))
                        (>= x 900) (recur (- x 900) (conj res "CM"))
                        (>= x 500) (recur (- x 500) (conj res "D"))
                        (>= x 400) (recur (- x 400) (conj res "CD"))
                        (>= x 100) (recur (- x 100) (conj res "C"))
                        (>= x 90) (recur (- x 90) (conj res "XC"))
                        (>= x 50) (recur (- x 50) (conj res "L"))
                        (>= x 40) (recur (- x 40) (conj res "XL"))
                        (>= x 10) (recur (- x 10) (conj res "X"))
                        (>= x 9) (recur (- x 9) (conj res "IX"))
                        (>= x 5) (recur (- x 5) (conj res "V"))
                        (>= x 4) (recur (- x 4) (conj res "IV"))
                        (>= x 1) (recur (- x 1) (conj res "I"))
                        :else (apply str res)))))

(defcheck solution-3445dcb
  (fn f [n]
    (cond (>= n 1000) (apply str \M (f (- n 1000)))
          (>= n 900) (apply str "CM" (f (- n 900)))
          (>= n 500) (apply str \D (f (- n 500)))
          (>= n 400) (apply str "CD" (f (- n 400)))
          (>= n 100) (apply str \C (f (- n 100)))
          (>= n 90) (apply str "XC" (f (- n 90)))
          (>= n 50) (apply str \L (f (- n 50)))
          (>= n 40) (apply str "XL" (f (- n 40)))
          (>= n 10) (apply str \X (f (- n 10)))
          (>= n 9) (apply str "IX" (f (- n 9)))
          (>= n 5) (apply str "V" (f (- n 5)))
          (>= n 4) (apply str "IV" (f (- n 4)))
          (>= n 1) (apply str \I (f (dec n))))))

(defcheck solution-3449bb69
  (fn [x]
    (let [thousands (quot x 1000)
          x (rem x 1000)
          hundreds (quot x 100)
          x (rem x 100)
          tens (quot x 10)
          ones (rem x 10)
          show-digit (fn [d one & [five ten]]
                       (clojure.string/join
                         (cond
                           (= 9 d) `[~one ~ten]
                           (>= d 5) `[~five ~@(repeat (- d 5) one)]
                           (= 4 d) `[~one ~five]
                           :else (repeat d one))))]
      (str (show-digit thousands "M")
        (show-digit hundreds "C" "D" "M")
        (show-digit tens "X" "L" "C")
        (show-digit ones "I" "V" "X")))))

(defcheck solution-34b65293
  (let [digits [[1000 \M] [900 "CM"] [500 \D] [400 "CD"]
                [100 \C] [90 "XC"] [50 \L] [40 "XL"]
                [10 \X] [9 "IX"] [5 \V] [4 "IV"] [1 "I"]]
        romanizer (fn romanizer
                    [letters n]
                    (if (zero? n)
                      letters
                      (let [[diff suffix] (some #(and (>= n (first %)) %)
                                            digits)]
                        (romanizer (conj letters suffix) (- n diff)))))]
    (fn romanize
      [n]
      (apply str (romanizer [] n)))))

(defcheck solution-34bd103
  (fn [n]
    (let [nm (into (zipmap (reductions * 1 (cycle [5 2])) "IVXLCDM")
               {4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900"CM"})]
      (loop [r n s ""]
        (if (zero? r) s
                      (let [i (apply max (filter #(>= r %) (keys nm)))]
                        (recur (- r i) (str s (nm i))))
                      )))))

(defcheck solution-3591d4da
  (fn r [n]
    (if (= n 0) ""
                (let
                 [rl [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]
                  c (some #(when (>= n (second %)) %) rl)]
                  (str (first c) (r (- n (second c))))))))

(defcheck solution-35a27e0d
  (fn write-roman-numerals [n]
    (letfn [(num->digits [base num]
              (loop [n num res []]
                (if (zero? n)
                  res
                  (recur (long (/ n base)) (cons (mod n base) res)))))
            (quot-mod [base n]
              (list (quot n base) (mod n base)))
            (normalize [s]
              (if (>= (count s) 4)
                s
                (recur (cons '(0 0 0 0) s))))
            (convert [rn d]
              (flatten (map repeat rn d)))]
      (apply str (flatten
                   (map convert
                     (normalize
                       (map (fn [[a b c]] (cons a (cons b (quot-mod 4 c))))
                         (map (fn [[a b]] (cons a (quot-mod 5 b))) (map (partial quot-mod 9) (num->digits 10 n)))))
                     [["U" "U" "U" "M"] ["CM" "D" "CD" "C"] ["XC" "L" "XL" "X"] ["IX" "V" "IV" "I"]]))))))

(defcheck solution-35f254c6
  (fn to-roman [x]
    (let [arm {1 "I", 5 "V",
               10 "X", 50 "L",
               100 "C", 500 "D",
               1000 "M",
               4 "IV", 9 "IX",
               40 "XL", 90 "XC",
               400 "CD", 900 "CM" },
          armsk (sort (keys arm)),
          fvic #(let [aval (find % %2)] (if (nil? aval) "" (val aval)))]
      (loop [result "",n x]
        (let [ckfn (last (filter #(> n %) armsk)),
              rvfn (fvic arm n),
              rvfctn (fvic arm ckfn)]
          (if (or (<= n 0) (contains? arm n))
            (str result rvfn)
            (recur (str result rvfctn) (- n ckfn))))))))

(defcheck solution-3712f4f8
  (fn [roman]
    (let [ra (fn ra [r]
               (cond
                 (>= r 1000) (concat (repeat (quot r 1000) "M") (ra (mod r 1000)))
                 (>= r 900)  (cons "CM" (ra (- r 900)))
                 (>= r 500)  (concat (cons "D" (repeat (quot (- r 500) 100) "C"))
                               (ra (mod r 100)))
                 (>= r 400)  (cons "CD" (ra (- r 400)))
                 (>= r 100)  (concat (repeat (quot r 100) "C") (ra (mod r 100)))
                 (>= r 90)   (cons "XC" (ra (- r 90)))
                 (>= r 50)   (concat (cons "L" (repeat (quot (- r 50) 10) "X"))
                               (ra (mod r 10)))
                 (>= r 40)   (cons "XL" (ra (- r 40)))
                 (>= r 10)   (concat (repeat (quot r 10) "X") (ra (mod r 10)))
                 (>= r 9)    '("IX")
                 (>= r 5)    (cons "V" (repeat (- r 5) "I"))
                 (>= r 4)    '("IV")
                 :else       (repeat r "I")))]
      (clojure.string/join "" (ra roman)))))

(defcheck solution-38a96320
  (fn [n]
    (let [t
            {
             1000	\M
             900		"CM"
             500		\D
             400		"CD"
             100		\C
             90		"XC"
             50		\L
             40		"XL"
             10		\X
             9		"IX"
             5		\V
             4		"IV"
             1		\I
             }
          s (sort > (keys t))]
      (loop [v n p []]
        (if (zero? v)
          (apply str (map t p))
          (let [i (first (filter #(>= v %) s))]
            (recur (- v i) (conj p i))))))))

(defcheck solution-38b01d63
  (fn[n]
    (let
     [roman [[1000 "M"], [900 "CM"], [500 "D"], [400 "CD"], [100 "C"],
             [90 "XC"], [50 "L"], [40 "XL"], [10 "X"], [9 "IX"],
             [5 "V"], [4 "IV"], [1 "I"]]
      trans (fn [[num rstr] [val rep]]
              [(mod num val) (apply str rstr (repeat (quot num val) rep))])]
      (last (reduce trans [n ""] roman)))
    ))

(defcheck solution-38f87970
  (fn roman
    ([n] (roman "" n))
    ([s n]
     (cond
       (zero? n) s
       (>= n 1000) (recur (str s "M") (- n 1000))
       (>= n 900) (recur (str s "CM") (- n 900))
       (>= n 500) (recur (str s "D") (- n 500))
       (>= n 400) (recur (str s "CD") (- n 400))
       (>= n 100) (recur (str s "C") (- n 100))
       (>= n 90) (recur (str s "XC") (- n 90))
       (>= n 50) (recur (str s "L") (- n 50))
       (>= n 40) (recur (str s "XL") (- n 40))
       (>= n 10) (recur (str s "X") (- n 10))
       (>= n 9) (recur (str s "IX") (- n 9))
       (>= n 5) (recur (str s "V") (- n 5))
       (>= n 4) (recur (str s "IV") (- n 4))
       (>= n 1) (recur (str s "I") (- n 1))
       :else s
       ))
    ))

(defcheck solution-393e9361
  (fn [n]
    (second (letfn [(roman [i v x xi]
                      ([[i] [i] [i i] [i i i] [i v] [v] [v i] [v i i] [v i i i] [i x]] xi))]
              (reduce
                (fn [[x s] [base f]]
                  (let [u (rem x base) v (int (/ x base))]
                    (if (zero? v)
                      [u s]
                      [u (str s (apply str (f v)))])))
                [n ""]
                [[1000 #(repeat % \M)]
                 [100 (partial roman \C \D \M)]
                 [10 (partial roman \X \L \C)]
                 [1 (partial roman \I \V \X)]])))))

(defcheck solution-394d29b0
  (fn [n]
    (let [romans {1 "I" 10 "X" 100 "C" 1000 "M"}
          roman-num (apply str
                      (map romans
                        (reduce #(concat % (repeat (mod (quot n %2) 10) %2))
                          [] [1000 100 10 1])))]
      #_(println roman-num)
      (-> roman-num
        (clojure.string/replace "CCCCCCCCC" "CM")
        (clojure.string/replace "CCCCC" "D")
        (clojure.string/replace "CCCC" "CD")
        (clojure.string/replace "XXXXXXXXX" "XC")
        (clojure.string/replace "XXXXX" "L")
        (clojure.string/replace "XXXX" "XL")
        (clojure.string/replace "IIIIIIIII" "IX")
        (clojure.string/replace "IIIII" "V")
        (clojure.string/replace "IIII" "IV")))))

(defcheck solution-39567229
  (fn __
    ([n] (__ n [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L" ] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]))
    ([n [[k v] & r :as s]]
     (if (> n 0)
       (if (> (quot n k) 0)
         (str v (__ (- n k) s))
         (__ n r))))))

(defcheck solution-39a54188
  (fn [n]
    (let [rn {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          next (fn [n] (apply max (filter #(>= n %) (map key rn))))
          conv (fn [s n]
                 (if (zero? n) s
                               (let [nxnum (next n)]
                                 (recur (str s (rn nxnum)) (- n nxnum)))))]
      (conv "" n))))

(defcheck solution-3a49ba79
  (fn [n]
    (loop [coll []
           remaining n
           romvals ["M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1]]
      (if (<= remaining 0)
        (reduce str (flatten coll))
        (recur (if (> (quot remaining (second romvals)) 0)
                 (conj coll (repeat (quot remaining (second romvals)) (first romvals)))
                 coll)
          (if (> (quot remaining (second romvals)) 0)
            (- remaining (* (second romvals) (quot remaining (second romvals))))
            remaining)
          (drop 2 romvals))))))

(defcheck solution-3b65cc36
  (fn [x]
    (letfn [(nextnumeral [y]
              (cond
                (>= y 1000) ["M" (- y 1000)]
                (>= y 900) ["CM" (- y 900)]
                (>= y 500) ["D" (- y 500)]
                (>= y 400) ["CD" (- y 400)]
                (>= y 100) ["C" (- y 100)]
                (>= y 90) ["XC" (- y 90)]
                (>= y 50) ["L" (- y 50)]
                (>= y 40) ["XL" (- y 40)]
                (>= y 10) ["X" (- y 10)]
                (>= y 9) ["IX" (- y 9)]
                (>= y 5) ["V" (- y 5)]
                (>= y 4) ["IV" (- y 4)]
                (>= y 1) ["I" (- y 1)]
                :else ["" 0]))]
      (loop [numerals [] remainder x]
        (if (zero? remainder)
          (apply str numerals)
          (let [[nextnum remainder] (nextnumeral remainder)]
            (recur (into numerals nextnum) remainder )))))))

(defcheck solution-3bd4b471
  (fn roman [x]
    (cond
      (<= 1000 x) (str "M" (roman (- x 1000)))
      (<= 900 x) (str "CM" (roman (- x 900)))
      (<= 500 x) (str "D" (roman (- x 500)))
      (<= 400 x) (str "CD" (roman (- x 400)))
      (<= 100 x) (str "C" (roman (- x 100)))
      (<= 90 x) (str "XC" (roman (- x 90)))
      (<= 50 x) (str "L" (roman (- x 50)))
      (<= 40 x) (str "XL" (roman (- x 40)))
      (<= 10 x) (str "X" (roman (- x 10)))
      (<= 9 x) (str "IX" (roman (- x 9)))
      (<= 5 x) (str "V" (roman (- x 5)))
      (<= 4 x) (str "IV" (roman (- x 4)))
      (<= 1 x) (str "I" (roman (- x 1)))
      :else "")))

(defcheck solution-3bedec7c
  (fn f
    ([n]
     (f n ""))
    ([n s]
     (let [ss ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"]
           dd [1 4 5 9 10 40 50 90 100 400 500 900 1000]
           zz (zipmap dd ss)]
       (if (zero? n)
         s
         (let [x (last (take-while #(<= % n) dd))]
           (f (- n x) (str s (zz x)))))))))

(defcheck solution-3bff100d
  (fn [n]
    (let [r clojure.string/replace]
      (-> (loop [s   ""
                 n   n
                 x [[1000 \M]
                    [ 500 \D]
                    [ 100 \C]
                    [  50 \L]
                    [  10 \X]
                    [   5 \V]
                    [   1 \I]]]
            (if (empty? x)
              s
              (let [[d c] (first x)]
                (recur
                  (str s (apply str (repeat (quot n d) c)))
                  (mod n d)
                  (rest x)))))
        (r "DCCCC" "CM")
        (r  "CCCC" "CD")
        (r "LXXXX" "XC")
        (r  "XXXX" "XL")
        (r "VIIII" "IX")
        (r  "IIII" "IV")))))

(defcheck solution-3c1809c6
  (fn [n]
    (let [r (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD"
              100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X"
              9 "IX" 5 "V" 4 "IV" 1 "I")]
      (last (reduce #(let [n (first %), s (last %),
                           k (key %2), v (val %2)]
                       (loop [n n, s s]
                         (if (>= n k)
                           (recur (- n k) (str s v))
                           [n s])))
              [n ""]
              (rseq r))))))

(defcheck solution-3c57632b
  (let [conversions [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"]
                     [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
    (fn roman [a]
      (if (zero? a) ""
                    (let [[a', r] (first (filter (fn [[a'' _]] (<= a'' a)) conversions))]
                      (str r (roman (- a a'))))))))

(defcheck solution-3cf635ec
  (fn p104 [x]
    (let [numerals (into (sorted-map) {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                                       40 "XL" 50 "L" 90 "XC" 100 "C"
                                       400 "CD" 500 "D" 900 "CM" 1000 "M"})]
      (cond (= x 0) "N"
            (and (< x 0) (> x 3999)) nil
            :else (->> (rseq numerals)
                    (reduce
                      (fn [[r acc] [v c]]
                        (let [n (quot r v)] [(- r (* n v)) (conj acc (repeat n c))]))
                      [x []])
                    (second)
                    (apply concat)
                    (clojure.string/join ""))))))

(defcheck solution-3d624d98
  (fn roman [n]
    (letfn [(n-str [n s]
              (if (<= n 0)
                nil
                (str s (n-str (- n 1) s))))]
      (cond
        (not (zero? (quot n 1000)))
        (str (n-str (quot n 1000) "M")
          (roman (mod n 1000)))

        (= 9 (quot n 100))
        (str "CM"
          (roman (mod n 100)))

        (= 1 (quot n 500))
        (str "D"
          (roman (mod n 500)))

        (= 4 (quot n 100))
        (str "CD"
          (roman (mod n 100)))

        (not (zero? (quot n 100)))
        (str (n-str (quot n 100) "C")
          (roman (mod n 100)))

        (= 9 (quot n 10))
        (str "XC"
          (roman (mod n 10)))

        (= 1 (quot n 50))
        (str "L"
          (roman (mod n 50)))


        (= 4 (quot n 10))
        (str "XL"
          (roman (mod n 10)))

        (not (zero? (quot n 10)))
        (str (n-str (quot n 10) "X")
          (roman (mod n 10)))

        (= 9 n)
        "IX"

        (= 1 (quot n 5))
        (str "V"
          (roman (mod n 5)))

        (= 4 n)
        "IV"

        (not (zero? n))
        (n-str n "I")))))

(defcheck solution-3dafdd45
  (fn [n]
    (let [base-numerals {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                         40 "XL" 50 "L" 90 "XC" 100 "C"
                         400 "CD" 500 "D" 900 "CM" 1000 "M"}
          base-keys (sort (keys base-numerals))]
      (loop [k n acc ""]
        (if (zero? k)
          acc
          (let [best-vals (take-while #(<= % k) base-keys)
                this-pass (apply max best-vals)]
            (recur
              (- k this-pass)
              (str acc (base-numerals this-pass)))))))))

(defcheck solution-3e22a121
  (fn write-roman [n]
    (loop [val n roman ""]
      (cond
        (> val 999) (recur (- val 1000) (str roman "M"))
        (> val 899) (recur (- val 900) (str roman "CM"))
        (> val 499) (recur (- val 500) (str roman "D"))
        (> val 399) (recur (- val 400) (str roman "CD"))
        (> val 99) (recur (- val 100) (str roman "C"))
        (> val 89) (recur (- val 90) (str roman "XC"))
        (> val 49) (recur (- val 50) (str roman "L"))
        (> val 39) (recur (- val 40) (str roman "XL"))
        (> val 9) (recur (- val 10) (str roman "X"))
        (> val 8) (recur (- val 9) (str roman "IX"))
        (> val 4) (recur (- val 5) (str roman "V"))
        (> val 3) (recur (- val 4) (str roman "IV"))
        (> val 0) (recur (- val 1) (str roman "I"))
        :otherwise roman
        )
      )
    ))

(defcheck solution-3e88eec0
  #(loop [n % o ""
          v (partition-all 3 2 "IVXLCDM")]
     (if (zero? n)
       o
       (recur
         (quot n 10)
         (apply
           str
           (concat
             (map
               (vec (first v))
               ([[] [0] [0 0] [0 0 0] [0 1] [1]
                 [1 0] [1 0 0] [1 0 0 0 ] [0 2]]
                (rem n 10)))
             o))
         (rest v)))))

(defcheck solution-3ee19533
  (fn rom [n]
    (let [lookup (apply hash-map
                   (concat
                     '(1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX")
                     '(10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC")
                     '(100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC" 900 "CM")
                     '(1000 "M" 2000 "MM" 3000 "MMM")))
          nums (reverse (sort  (keys lookup)))]
      (loop [rem n rstr ""]
        (let [sub (first (filter #(<= 0 (- rem %)) nums))
              roman (lookup sub)]
          (if (zero? rem)
            rstr
            (recur (- rem sub) (str rstr roman))))))))

(defcheck solution-3ef1ea64
  (fn roman-str [n]
    (let [roman-numbers (partition 2 ["M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100
                                      "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1])]
      (if (zero? n)
        ""
        (let [[letters number] (first (drop-while #(> (second %) n) roman-numbers))]
          (str letters (roman-str (- n number))))))))

(defcheck solution-3f0df080
  (fn[x]
    (let[ r [[] [0] [0 0] [0 0 0] [0 1] [1] [1 0] [1 0 0] [1 0 0 0] [0 2]]
         n  (partition-all 3 2 "IVXLCDM")]
      (loop[s "" x x n n] (if (zero? x) s
                                        (recur (str (apply str (map #(nth (first n) %) (nth r (mod x 10)))) s) (quot x 10) (next n)))))))

(defcheck solution-3f13ad23
  (fn rom [n]
    (let [romanian-digit (fn [n i v x]
                           (cond
                             (>= 3 n) (apply str (take n (repeat i)))
                             (= 4 n) (str i v)
                             (= 5 n) v
                             (= 6 n) (str v i)
                             (= 7 n) (str v i i)
                             (= 8 n) (str v i i i)
                             (= 9 n) (str i x)))
          d (quot n 1000)
          q (mod (quot n 100) 10)
          l (mod (quot n 10) 10)
          p (mod n 10)]
      (str
        (romanian-digit d "M" "" "")
        (romanian-digit q "C" "D" "M")
        (romanian-digit l "X" "L" "C")
        (romanian-digit p "I" "V" "X")))))

(defcheck solution-3f5674c8
  (fn [n]
    (let [tbl [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
               [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [n n tbl tbl res []]
        (if (zero? n)
          (apply str res)
          (let [[a r] (first tbl)]
            (if (>= n a)
              (recur (- n a) tbl (conj res r))
              (recur n (rest tbl) res))))))))

(defcheck solution-40362acf
  (fn [n]
    (let [m (array-map
              1000 "M"
              900 "CM"
              500 "D"
              400 "CD"
              100 "C"
              90 "XC"
              50 "L"
              40 "XL"
              10 "X"
              9 "IX"
              5 "V"
              4 "IV"
              1 "I"
              )]
      (loop [n n res ""]
        (if (= 0 n) res
                    (let [[k v] (first (drop-while #(< n (first %)) m))]
                      (recur (- n k) (str res v))))))))

(defcheck solution-40a29a06
  (fn [num]
    (let [lets ["M" "D" "C" "L" "X" "V" "I"]
          nums [1000 500 100 50 10 5 1]
          ten-nums (take-nth 2 nums)
          ten-lets (take-nth 2 lets)
          xnums (map - ten-nums (rest ten-nums))
          xlets (map str (rest ten-lets) ten-lets)
          vlets (map (partial apply str) (map reverse (partition 2 (rest lets))))
          vnums (map (partial apply -) (partition 2 (rest nums)))
          slets (interleave xlets vlets)
          snums (interleave xnums vnums)
          interleave-subs (fn [o s] (concat (interleave (butlast o) s) [(last o)]))
          all-lets (interleave-subs lets slets)
          all-nums (interleave-subs nums snums)]
      (loop [n num
             als all-lets
             subs all-nums
             res ""]
        (let [curr (first subs)]
          (if curr
            (if (>= n curr)
              (recur (- n curr) als subs (str res (first als)))
              (recur n (rest als) (rest subs) res))
            res))))))

(defcheck solution-412d78be
  (fn to-roman-numeral
    ([x] (to-roman-numeral x ""))
    ([x acc]
     (let [[value numeral] (condp <= x
                             1000 [1000 "M"]
                             900 [900 "CM"]
                             500 [500 "D"]
                             400 [400 "CD"]
                             100 [100 "C"]
                             90 [90 "XC"]
                             50 [50 "L"]
                             40 [40 "XL"]
                             10 [10 "X"]
                             9 [9 "IX"]
                             5 [5 "V"]
                             4 [4 "IV"]
                             1 [1 "I"]
                             [0 ""])]
       (if (zero? value)
         acc
         (recur (- x value) (str acc numeral)))))))

(defcheck solution-424c417f
  (fn rom [n]
    (let [seqize (fn [n] (->> (str n) (map str) (map read-string)))
          sn (reverse (seqize n))
          pos (fn [[i v x]] [[] [i] [i i] [i i i] [i v] [v] [v i] [v i i] [v i i i] [i x]])
          letters (map pos (partition-all 3 2 "IVXLCDM"))]
      (apply str (apply concat (reverse (map #(%2 %1) sn letters)))))))

(defcheck solution-42c90ab0
  (fn roman [n]
    (let [digits [{:dec (quot n 1000) :base "M" :half nil :next-base nil}
                  {:dec (quot (rem n 1000) 100) :base "C" :half "D" :next-base "M"}
                  {:dec (quot (rem n 100) 10) :base "X" :half "L" :next-base "C"}
                  {:dec (rem n 10) :base "I" :half "V" :next-base "X"}]
          to-roman (fn [d]
                     (cond
                       (or (nil? (:next-base d))
                           (< (:dec d) 4)) (apply str (repeat (:dec d) (:base d)))
                       (= (:dec d) 4) (str (:base d) (:half d))
                       (< 4 (:dec d) 9) (str (:half d)
                                          (apply str (repeat (- (:dec d) 5) (:base d))))
                       (= (:dec d) 9) (str (:base d) (:next-base d))))]
      (->> digits
        (map to-roman)
        (apply str)))))

(defcheck solution-4367c469
  (fn [n]
    (loop [n n
           r ""]
      (let [new-args
            (cond
              (> 1 n) r
              (> 4 n) [1 "I"]
              (> 5 n) [4 "IV"]
              (> 9 n) [5 "V"]
              (> 10 n) [9 "IX"]
              (> 40 n) [10 "X"]
              (> 50 n) [40 "XL"]
              (> 90 n) [50 "L"]
              (> 100 n) [90 "XC"]
              (> 500 n) [100 "C"]
              (> 900 n) [500 "D"]
              (> 1000 n) [900 "CM"]
              (> 4000 n) [1000 "M"])]
        (if (string? new-args) new-args
                               (recur
                                 (- n (first new-args))
                                 (clojure.string/join [r (second new-args)])))))))

(defcheck solution-436b0d70
  (fn [n]
    (loop [n n, r ""]
      ( cond (< 0 (- n 999))  (recur (- n 1000) (str r "M"))
             (< 0 (- n 899))  (recur (- n  900) (str r "CM"))
             (< 0 (- n 499))  (recur (- n  500) (str r "D"))
             (< 0 (- n 399))  (recur (- n  400) (str r "CD"))
             (< 0 (- n 99))   (recur (- n  100) (str r "C"))
             (< 0 (- n 89))   (recur (- n   90) (str r "XC"))
             (< 0 (- n 49))   (recur (- n   50) (str r "L"))
             (< 0 (- n 39))   (recur (- n   40) (str r "XL"))
             (< 0 (- n 9))    (recur (- n   10) (str r "X"))
             (< 0 (- n 8))    (recur (- n    9) (str r "IX"))
             (< 0 (- n 4))    (recur (- n    5) (str r "V"))
             (< 0 (- n 3))    (recur (- n    4) (str r "IV"))
             (< 0 n)          (recur (- n    1) (str r "I"))
             :else r))))

(defcheck solution-44317976
  (fn roman
    ([n] (roman n (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")))
    ([n mp]
     (if (zero? n) ""
                   (let [ d (last (keys mp))
                         q (quot n d)]
                     (str
                       (apply str (repeat q (get mp d)))
                       (roman (mod n d) (dissoc mp d))))))))

(defcheck solution-445bef29
  (fn roman [n]
    (let [r [[1000 "M"]
             [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
             [90 "XC"]  [50 "L"]  [40 "XL"]  [10 "X"]
             [9 "IX"]   [5 "V"]   [4 "IV"]   [1 "I"]]]
      (loop [n n
             s []]
        (if (< n 1)
          (apply str s)
          (let [less (first (filter (fn [[x x-str]] (>= n x)) r))]
            (recur (- n (first less))
              (conj s (last less)))))))))

(defcheck solution-44d69a57
  (fn [n]
    (let [ds [[1000 "M"],
              [900  "CM"],
              [500  "D"],
              [400  "XD"],
              [100  "C"],
              [90   "XC"],
              [50   "L"],
              [40   "XL"],
              [10   "X"],
              [9    "IX"]
              [5    "V"],
              [4    "IV"]
              [1    "I"]]]
      (->>
        (loop [r    n
               ds   ds
               coll []]
          (if (zero? r)
            coll
            (let [[m d] (first ds)]
              (if (< r m)
                (recur r (rest ds) coll)
                (recur (- r m) ds (conj coll d))))))
        (apply str)))))

(defcheck solution-4524beab
  (fn print-roman [n]
    (letfn [(int-div [n d] (/ (- n (mod n d)) d))]
      (let [digits (sorted-map-by >
                     1000 "M"
                     900 "CM"
                     500 "D"
                     400 "CD"
                     100 "C"
                     90 "XC"
                     50 "L"
                     40 "XL"
                     10 "X"
                     9 "IX"
                     5 "V"
                     4 "IV"
                     1 "I"
                     )]
        (loop [n n
               d digits
               s ""]
          (if (empty? d) s
                         (let [[arabic roman] (first d)
                               c (int-div n arabic)]
                           (recur (- n (* c arabic)) (dissoc d arabic) (apply str s (repeat c roman))))))))))

(defcheck solution-453351fd
  (fn [n]
    (loop [i n s ""]
      (cond (>= i 1000) (recur (- i 1000) (str s "M"))
            (>= i 900) (recur (- i 900) (str s "CM"))
            (>= i 500) (recur (- i 500) (str s "D"))
            (>= i 400) (recur (- i 400) (str s "CD"))
            (>= i 100) (recur (- i 100) (str s "C"))
            (>= i 90) (recur (- i 90) (str s "XC"))
            (>= i 50) (recur (- i 50) (str s "L"))
            (>= i 40) (recur (- i 40) (str s "XL"))
            (>= i 10) (recur (- i 10) (str s "X"))
            (>= i 9) (recur (- i 9) (str s "IX"))
            (>= i 5) (recur (- i 5) (str s "V"))
            (>= i 4) (recur (- i 4) (str s "IV"))
            (>= i 1) (recur (- i 1) (str s "I"))
            :else s
            ))))

(defcheck solution-4587809c
  (fn d2r
    ([n]
     (d2r n (array-map 1000 [\M \0 \0], 100 [\C \D \M], 10 [\X \L \C], 1 [\I \V \X]) ""))
    ([n m s]
     (if (zero? n)
       s
       (let [[k [one-sym five-sym ten-sym]] (first m)
             r (rem n k)
             d (int (/ n k))
             t (cond
                 (< 0 d 4) (apply str (repeat d one-sym))
                 (= d 4) (str one-sym five-sym)
                 (< 4 d 9) (apply str (into [five-sym] (repeat (- d 5) one-sym)))
                 (= d 9) (str one-sym ten-sym))]
         (recur r (dissoc m k) (str s t)))))))

(defcheck solution-462dff91
  (fn [n]
    (let [m [["M" 1000]["CM" 900]["D" 500]["CD" 400]["C" 100]["XC" 90]["L" 50]["XL" 40]["X" 10]["IX" 9]["V" 5]["IV" 4]["I" 1]]]
      (letfn [(f [x s]
                (if (zero? x) (apply str s)
                              (let [p (first (filter #(>= x (second %)) m))]
                                (f (- x (second p)) (conj s (first p))))))]
        (f n [])))))

(defcheck solution-464df562
  (fn roman [n]
    (cond (>= n 1000) (str "M" (roman (- n 1000)))
          (>= n 900) (str "CM" (roman (- n 900)))
          (>= n 500) (str "D" (roman (- n 500)))
          (>= n 400) (str "CD" (roman (- n 400)))
          (>= n 100) (str "C" (roman (- n 100)))
          (>= n 90) (str "XC" (roman (- n 90)))
          (>= n 50) (str "L" (roman (- n 50)))
          (>= n 40) (str "XL" (roman (- n 40)))
          (>= n 10) (str "X" (roman (- n 10)))
          (>= n 9) (str "IX" (roman (- n 9)))
          (>= n 5) (str "V" (roman (- n 5)))
          (>= n 4) (str "IV" (roman (- n 5)))
          (>= n 1) (str "I" (roman (- n 1)))
          :otherwise "")))

(defcheck solution-4679ec73
  (fn roman-from-number [n]
    (letfn
     [(number-to-base [n [base & more]]
        (if (=  n 0)
          '()
          (let [division (quot n base)
                reminder (rem n base)]
            (concat (repeat division base) (lazy-seq (number-to-base reminder more))))))]
      (let [roman-base-to-symbol {
                                  1000 "M"
                                  900 "CM"
                                  500 "D"
                                  400 "CD"
                                  100 "C"
                                  90 "XC"
                                  50 "L"
                                  40 "XL"
                                  10 "X"
                                  9 "IX"
                                  5 "V"
                                  4 "IV"
                                  1 "I" }
            roman-base (reverse (sort (keys roman-base-to-symbol)))
            next-roman (apply hash-map (flatten (partition 2 1 (reverse roman-base))))]
        (let [digits (number-to-base n roman-base)
              grouped-digits (partition-by identity digits)
              ]
          (apply str
            (map
              roman-base-to-symbol
              (mapcat
                (fn [[f & r :as ls]]
                  (if (= 4 (count ls))
                    (list f (next-roman f))
                    ls))
                grouped-digits))))))))

(defcheck solution-46d91cde
  (fn to-roman
    [a]
    (letfn [ (z [a x v i]
               ({0 "" 1 (str i) 2 (str i i) 3 (str i i i) 4 (str i v)
                 5 (str v) 6 (str v i) 7 (str v i i) 8 (str v i i i) 9 (str i x)}
                a))]
      (reduce
        (fn [r [k x v i]] (str r (z (mod (quot a k) 10) x v i))) ""
        [[1000 \- \- \M] [100 \M \D \C]  [10 \C \L \X] [1 \X \V \I]]
        ))))

(defcheck solution-4739cb43
  #(apply str
     (let [is [1000 900 500 400 100 90 50 40 10 9 5 4 1]
           rns {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
       ((fn d [x]
          (let [i (first (drop-while (partial < x) is))
                y (- x i)]
            (cons (rns i)
              (if-not (zero? y)
                (d y))))) %))))

(defcheck solution-476bf167
  (fn write-numerals [x]
    (let [tl (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV" 1 "I" )
          f (fn f [i t a]
              (if (seq t)
                (let [k (-> t first key) v (-> t first val)]
                  (if (>= i k)
                    (f (- i k) t (conj a v))
                    (f i (rest t) a)))
                a))]
      (apply str (f x tl [])))))

(defcheck solution-479c8009
  (fn [n]
    (let [ribuan (quot n 1000)
          ratusan (quot (rem n 1000) 100)
          puluhan (quot (rem n 100) 10)
          satuan (rem n 10)
          r? (fn [a f1 b f2 c]
               (and (f1 a b) (f2 b c)))]
      (apply str (concat (repeat ribuan \M)
                   (cond (= 9 ratusan) (list \C \M)
                         (r? 5 <= ratusan <= 8) (cons \D (repeat (- ratusan 5) \C))
                         (= 4 ratusan) (list \C \D)
                         (r? 1 <= ratusan <= 3) (repeat ratusan \C))
                   (cond (= 9 puluhan) (list \X \C)
                         (r? 5 <= puluhan <= 8) (cons \L (repeat (- puluhan 5) \X))
                         (= 4 puluhan) (list \X \L)
                         (r? 1 <= puluhan <= 3) (repeat puluhan \X))
                   (cond (= 9 satuan) (list \I \X)
                         (r? 5 <= satuan <= 8) (cons \V (repeat (- satuan 5) \I))
                         (= 4 satuan) (list \I \V)
                         (r? 1 <= satuan <= 3) (repeat satuan \I)))))))

(defcheck solution-47a2de58
  (fn [n]
    (let [huns ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
          tens ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
          ones ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
          trun (fn [p b v] (let [x (mod (first p) b) y (quot (first p) b)]
                             [x (str (second p) (nth v y))]))
          init (loop [x n r ""]
                 (if (>= x 1000)
                   (recur (- x 1000) (str r "M"))
                   [x r]))]
      (second
        (-> init
          (trun 100 huns)
          (trun 10 tens)
          (trun 1 ones))))))

(defcheck solution-485b4e4e
  {1 "I"
   30 "XXX"
   4 "IV"
   140 "CXL"
   827 "DCCCXXVII"
   3999 "MMMCMXCIX"
   48 "XLVIII"})

(defcheck solution-48b785ea
  (fn [n]
    (letfn [(to-roman [dd rd rh rn]
              ({0 [], 1 [rd], 2 [rd rd], 3 [rd rd rd], 4 [rd rh],
                5 [rh], 6 [rh rd], 7 [rh rd rd], 8 [rh rd rd rd], 9 [rd rn]}
               dd))
            ]
      (clojure.string/join
        (concat
          (to-roman (quot n 1000) \M nil nil)
          (to-roman (quot (rem n 1000) 100) \C \D \M)
          (to-roman (quot (rem n 100) 10) \X \L \C)
          (to-roman (rem n 10) \I \V \X))))))

(defcheck solution-48d82a41
  (fn [number]
    (loop [in number [l v & rest] ["M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90  "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1] out []]
      (if l
        (recur (mod in v) rest (concat out (repeat (quot in v) l)))
        (apply str out)))))

(defcheck solution-48f0023a
  #(let [nums {1 [\I \V \X], 10 [\X \L \C], 100 [\C \D \M]}
         to_str (fn s [base num]
                  (cond
                    (zero? num) nil
                    (= 1000 base) (apply str (repeat num \M))
                    :else (let [map (nums base)
                                one (map 0)
                                five (map 1)
                                ten (map 2)]
                            (cond
                              (< num 4) (apply str (repeat num one))
                              (= num 4) (str one five)
                              (= num 5) (str five)
                              (< num 9) (str five (s base (- num 5)))
                              (= num 9) (str one ten)))))]
     (str
       (to_str 1000 (quot % 1000))
       (to_str 100 (quot (mod % 1000) 100))
       (to_str 10 (quot (mod % 100) 10))
       (to_str 1 (mod % 10)))))

(defcheck solution-492e415b
  (fn [n]
    (->>
      (str n)
      reverse
      (map {\1 "I" \2 "II" \3 "III" \4 "IV" \5 "V"
            \6 "VI" \7 "VII" \8 "VIII" \9 "IX"})
      (map #(map %1 %2) '({\I \I \V \V \X \X} {\I \X \V \L \X \C} {\I \C \V \D \X \M} {\I \M}))
      reverse
      flatten
      (apply str))))

(defcheck solution-4a16c011
  (fn to-roman [n]
    (letfn [(num->sym-and-base [n]
              (cond
                (<= 1   n 3)    ["I" 1]
                (<= 4   n 8)    ["V" 5]
                (<= 9   n 39)   ["X" 10]
                (<= 40  n 89)   ["L" 50]
                (<= 90  n 399)  ["C" 100]
                (<= 400 n 899)  ["D" 500]
                (<= 900 n 4000) ["M" 1000]))
            (lower-base-for [base]
              ({1 1 5 1 10 1 50 10 100 10 500 100 1000 100} base))]
      (loop [n n res "" prev-sym "" prev-base 0]
        (cond (zero? n) (str res prev-sym)
              (< n 0) (let [new-base (lower-base-for prev-base)
                            [new-base-sym _] (num->sym-and-base new-base)
                            n (+ new-base n)
                            [base sym] (num->sym-and-base n)]
                        (recur n (str res new-base-sym prev-sym) "" 0))
              (> n 0) (let [[sym base] (num->sym-and-base n)]
                        (recur (- n base) (str res prev-sym) sym base)))))))

(defcheck solution-4b3a8476
  (fn R [i]
    (let [romans (into (sorted-map-by >)
                   {1000 "M"
                    900  "CM"
                    500  "D"
                    400  "CD"
                    100  "C"
                    90   "XC"
                    50   "L"
                    40   "XL"
                    10   "X"
                    9    "IX"
                    5    "V"
                    4    "IV"
                    1    "I"})
          allchars (loop [remain i, charlist [], romans romans, ret []]
                     (let [[n char] (first romans)
                           x (quot remain n)
                           nx-chars (concat charlist (repeat x char))]
                       (if (seq (rest romans))
                         (recur (- remain (* x n))
                           nx-chars
                           (rest romans)
                           (concat ret nx-chars))
                         nx-chars)))]
      (apply str (concat allchars)))))

(defcheck solution-4b7efb7a
  (fn me [num]


    (let [

          one-digits {1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"}

          two-digits {10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC"}

          three-digits {100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC" 900 "CM"}

          four-digits (fn [num] (apply str (repeat (quot num 1000) "M")) )

          num-seq     (fn [num]
                        (map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}
                          (str num))
                        )

          my-seq (reverse (num-seq num))

          cnt    (count my-seq)

          my-units (take cnt (iterate #(* 10 %) 1))

          nums-seq (map * my-seq my-units)

          m-fn (fn [num]

                 (let [ nums (num-seq num)

                       cnt  (count nums)
                       ]

                   (cond
                     (= 1 cnt) (one-digits num)
                     (= 2 cnt) (two-digits num)
                     (= 3 cnt) (three-digits num)
                     (= 4 cnt) (four-digits num)

                     )

                   ))

          ]

      (apply str (reverse (map m-fn nums-seq)))

      )

    ))

(defcheck solution-4c1ad931
  #((reduce (fn [[a n] [z x v i]]
              [(str a ((vec  (map (fn [p] (apply str p))
                               [[] [i] [i i] [i i i] [i v]
                                [v] [v i] [v i i]
                                [v i i i] [i x]]))
                       (quot n z)))
               (rem n z)])
      ["" %]
      [[1000 "" "" "M"]
       [100  "M" "D" "C"]
       [10   "C" "L" "X"]
       [1    "X" "V" "I"]]) 0))

(defcheck solution-4c6537b2
  (fn [n] (let [v (conj (vec (for [x [100 10 1] y [10 9 5 4]] (* x y))) 1) d (apply assoc {} (interleave v ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])) f (fn f [n l] (when (pos? n) (let [r (drop-while #(> % n) l) x (first r)] (conj (f (- n x) l) (d x)))))] (apply str (f n v)))))

(defcheck solution-4c673f33
  (fn [k]
    (let [Ro
          {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}
          Rokey
          (reverse (sort (keys Ro)))
          digits
          (loop [ret [], nn k]
            (if (= 0 nn)
              ret
              (let [dd (first (drop-while #(< nn %) Rokey))]
                (recur (conj ret dd) (- nn dd)))))]

      (apply str (map Ro digits)))
    ))

(defcheck solution-4d5415c2
  (fn [n]
    (let [getnums (fn [h l] (int (/ (rem n h) l)) )
          numthou (int (/ n 1000))
          num5hun (getnums 1000 500)
          numhun (getnums 500 100)
          numfifty (getnums 100 50)
          numten (getnums 50 10)
          numfive (getnums 10 5)
          numone (getnums 5 1)
          rf (fn [nh nl sl sh su]
               (cond
                 (and (= nl 4) (zero? nh)) (str sl sh)
                 (and (= nl 4) (not (zero? nh))) (str sl su)
                 :else (apply str (concat (repeat nh sh) (repeat nl sl)))))
          ones (rf numfive numone "I" "V" "X")
          tens (rf numfifty numten "X" "L" "C")
          huns (rf num5hun numhun "C" "D" "M")
          thous (apply str (repeat numthou "M"))]
      (apply str (concat thous huns tens ones)) )))

(defcheck solution-4d760974
  (fn [n]
    (let [conv {1 "I", 900 "CM", 100 "C", 5 "V", 40 "XL", 9 "IX", 10 "X"
                400 "CD", 1000 "M", 50 "L", 500 "D", 4 "IV", 90 "XC"}]
      (loop [acc "", n n, [c & cs] (reverse (sort (keys conv)))]
        (if (zero? n)
          acc
          (if (<= c n)
            (recur (str acc (conv c)) (- n c) (cons c cs))
            (recur acc n cs)))))))

(defcheck solution-4daa876c
  (fn rn [n]
    (apply str (condp <= n
                 1000 (cons "M" (rn (- n 1000)))
                 900 (cons "CM" (rn (- n 900)))
                 500 (cons "D" (rn (- n 500)))
                 400 (cons "CD" (rn (- n 400)))
                 100 (cons "C" (rn (- n 100)))
                 90 (cons "XC" (rn (- n 90)))
                 50 (cons "L" (rn (- n 50)))
                 40 (cons "XL" (rn (- n 40)))
                 10 (cons "X" (rn (- n 10)))
                 9 (cons "IX" (rn (- n 9)))
                 5 (cons "V" (rn (- n 5)))
                 4 (cons "IV" (rn (- n 4)))
                 1 (cons "I" (rn (- n 1)))
                 0 nil))))

(defcheck solution-4ddc2f12
  #(let [t [\I \V
            \X \L
            \C \D
            \M]]
     (loop [s (->> % str reverse (map str) (map read-string)) i 0 r ()]
       (if-let [n (first s)]
         (recur (next s) (inc i)
           (cons
             (cond (< 0 n 4) (repeat n (nth t (* 2 i)))
                   (= 4 n) [(nth t (* 2 i)) (nth t (inc (* 2 i)))]
                   (< 4 n 9) (cons (nth t (inc (* 2 i))) (repeat (- n 5) (nth t (* 2 i))))
                   (= 9 n) [(nth t (* 2 i)) (nth t (+ 2 (* 2 i)))]) r))
         (->> r (apply concat) (apply str))))))

(defcheck solution-4ec8161c
  (fn f [n] (cond
              (<= 0 (- n 1000)) (str "M" (f (- n 1000)))
              (<= 0 (- n 900))  (str "CM" (f (- n 900)))
              (<= 0 (- n 500))  (str "D" (f (- n 500)))
              (<= 0 (- n 400))  (str "CD" (f (- n 400)))
              (<= 0 (- n 100))  (str "C" (f (- n 100)))
              (<= 0 (- n 90))   (str "XC" (f (- n 90)))
              (<= 0 (- n 50))   (str "L" (f (- n 50)))
              (<= 0 (- n 40))   (str "XL"(f (- n 40)))
              (<= 0 (- n 10))   (str "X" (f (- n 10)))
              (<= 0 (- n 9))    (str "IX" (f (- n 9)))
              (<= 0 (- n 5))    (str "V" (f (- n 5)))
              (<= 0 (- n 4))    (str "IV" (f (- n 4)))
              (<= 0 (- n 1))    (str "I" (f (- n 1))))))

(defcheck solution-4f09ecd
  (fn roman [n]
    (let [rmn-tbl [[1000 "M"] [900 "CM"]
                   [500 "D"] [400 "CD"]
                   [100 "C"] [90 "XC"]
                   [50 "L"] [40 "XL"]
                   [10 "X"] [9 "IX"]
                   [5 "V"] [4 "IV"]
                   [1 "I"]]
          [d rmn] (some #(when (>= n (first %)) %) rmn-tbl)]
      (when d
        (str rmn (roman (- n d)))))))

(defcheck solution-4f4d42b3
  (fn roman- [n]
    ^{:doc "Given an integer smaller than 4000, return the corresponding
  roman numeral in uppercase, adhering to the subtractive principle."}
    (loop [n n
           acc ""]
      (cond
        (>= n 1000) (recur (- n 1000) (str acc "M"))
        (>= n 900) (recur (- n 900) (str acc "CM"))
        (>= n 500) (recur (- n 500) (str acc "D"))
        (>= n 400) (recur (- n 400) (str acc "CD"))
        (>= n 100) (recur (- n 100) (str acc "C"))
        (>= n 90) (recur (- n 90) (str acc "XC"))
        (>= n 50) (recur (- n 50) (str acc "L"))
        (>= n 40) (recur (- n 40) (str acc "XL"))
        (>= n 10) (recur (- n 10) (str acc "X"))
        (>= n 9) (recur (- n 9) (str acc "IX"))
        (>= n 5) (recur (- n 5) (str acc "V"))
        (>= n 4) (recur (- n 4) (str acc "IV"))
        (>= n 1) (recur (- n 1) (str acc "I"))
        :else acc))))

(defcheck solution-4f630c43
  (fn to-roman [n]
    (let [
          numeral-stack [[\I \V \X]
                         [\X \L \C]
                         [\C \D \M]
                         [\M nil nil]]
          impl (fn [acc numeral-stack n]
                 (if (< n 1)
                   acc
                   (let [[one five ten] (first numeral-stack)
                         x (mod n 10)
                         x* (quot n 10)]
                     (-> (cond
                           (== x 0) '()
                           (<= x 3) (repeat x one)
                           (== x 4) (list one five)
                           (== x 5) (list five)
                           (<= x 8) (cons five (repeat (- x 5) one))
                           (== x 9) (list one ten))
                       (cons ,,, acc)
                       (recur ,,, (rest numeral-stack) x*)))))
          ]
      (->> n
        (impl '() numeral-stack)
        (apply concat)
        (apply str)))))

(defcheck solution-4f6b543e
  (fn [x]
    (->> x
      str
      seq
      (map str)
      (map read-string)
      reverse
      (map-indexed list)
      (map (fn [a]
             (let [i (first a)
                   n (last a)]
               (repeat n
                 ({0 "I", 1 "X", 2 "C", 3 "M"} i)))))
      flatten
      reverse
      (apply str)
      (#(clojure.string/replace % #"IIIIIIIII" "IX"))
      (#(clojure.string/replace % #"IIIII" "V"))
      (#(clojure.string/replace % #"IIII" "IV"))
      (#(clojure.string/replace % #"XXXXXXXXX" "XC"))
      (#(clojure.string/replace % #"XXXXX" "L"))
      (#(clojure.string/replace % #"XXXX" "XL"))
      (#(clojure.string/replace % #"CCCCCCCCC" "CM"))
      (#(clojure.string/replace % #"CCCCC" "D"))
      (#(clojure.string/replace % #"CCCC" "CD"))
      )))

(defcheck solution-4ff49719
  (fn wrn [n]
    (let [nlm (merge (zipmap  [1 5 10 50 100 500 1000] (map str "IVXLCDM"))
                {4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900 "CM"})]
      (loop [w "" x n]
        (if (zero? x) w
                      (let [j (apply max (filter #(>= x %) (keys nlm)))]
                        (recur (str w (nlm j)) (- x j)))))
      )))

(defcheck solution-501a67e2
  (let [vs [1000 500 100 50 10 5 1]
        rnmap {1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"}
        submap {1000 100
                500 100
                100 10
                50 10
                10 1
                5 1}
        sub (fn [v0] (- v0 (get submap v0 0)))]
    (fn rn [x]
      (loop [x x [v0 & r :as vs] vs s ""]
        #_(prn x vs s)
        (cond
          (empty? vs) s
          (>= x v0) (recur (- x v0) vs (str s (rnmap v0)))

          (>= x (sub v0))
          (recur (- x (sub v0)) r (str s (rnmap (submap v0)) (rnmap v0)))

          :else (recur x r s))))))

(defcheck solution-504083f5
  (fn [n]
    (letfn [
            (num [one five ten n]
              (cond
                (= 0 n) ""
                (> 4 n) (apply str (take n (repeat one)))
                (= 4 n) (str one five)
                (= 5 n) (str five)
                (> 9 n) (apply str five (take (- n 5) (repeat one)))
                (= 9 n) (str one ten)))]
      (let [ones-num (partial num \I \V \X)
            tens-num (partial num \X \L \C)
            hundreds-num (partial num \C \D \M)
            thousands-num (partial num \M \_ \_)
            nums [[1000 thousands-num] [100 hundreds-num] [10 tens-num] [1 ones-num]]]
        (loop [m n [[d f] :as ns] nums s ""]
          (if (seq ns)
            (let [q (quot m d)
                  n' (- m (* q d))]
              (recur n' (rest ns) (str s (f q))))
            s))))))

(defcheck solution-50b7566e
  (fn toroman
    [n]
    (if (zero? n)
      ""
      (let [table (sorted-map-by #(- %2 %1) 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC",
                    50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")]
        (let [pick (first (drop-while #(> % n) (keys table)))]
          (str (table pick) (toroman (- n pick))))))))

(defcheck solution-50eb0e95
  (fn write-roman [n]
    (let [vals [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          strs ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          roman (zipmap vals strs)]
      (loop [values vals
             current n
             accum []]
        (if (empty? values)
          (apply str accum)
          (let [v (first values)
                next (- current v)]
            (if (neg? next)
              (recur (rest values) current accum)
              (recur values next (conj accum (roman v))))))))))

(defcheck solution-51a8de29
  (fn [x]
    (let [thousands
          (condp = (quot x 1000)
            0 ""
            1 "M"
            2 "MM"
            3 "MMM")
          hundreds
          (condp = (rem (quot x 100) 10)
            0 ""
            1 "C"
            2 "CC"
            3 "CCC"
            4 "CD"
            5 "D"
            6 "DC"
            7 "DCC"
            8 "DCCC"
            9 "CM")
          tens
          (condp = (rem (quot x 10) 10)
            0 ""
            1 "X"
            2 "XX"
            3 "XXX"
            4 "XL"
            5 "L"
            6 "LX"
            7 "LXX"
            8 "LXXX"
            9 "XC")
          ones
          (condp = (rem x 10)
            0 ""
            1 "I"
            2 "II"
            3 "III"
            4 "IV"
            5 "V"
            6 "VI"
            7 "VII"
            8 "VIII"
            9 "IX")]
      (apply str thousands hundreds tens ones))))

(defcheck solution-51b57428
  (fn [n]
    (loop [b [] n n]
      (if (zero? n)
        (apply str b)
        (let [[v r] (some #(if (>= n (first %)) %)  [[1000 "M"] [900 "CM"]
                                                     [500  "D"] [400 "CD"]
                                                     [100  "C"] [90  "XC"]
                                                     [50   "L"] [40  "XL"]
                                                     [10   "X"] [9   "IX"]
                                                     [5    "V"] [4   "IV"]
                                                     [1    "I"]])]
          (recur (conj b r) (- n v)))))))

(defcheck solution-52591ecf
  (fn [i] (let
           [numerals {   1 "I"   4 "IV"   5 "V"    9 "IX"
                      10 "X"  40 "XL"  50 "L"   90 "XC"
                      100 "C" 400 "CD" 500 "D"  900 "CM"
                      1000 "M"}]
            (loop [n i s ""]
              (if (= n 0)
                s
                (let
                 [m (apply max (filter #(<= % n) (keys numerals)))
                  t (numerals m)]
                  (recur (- n m) (str s t))))))))

(defcheck solution-5261496a
  (fn [n]
    (let [t [[""  "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
             [""  "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
             [""  "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
             [""  "M" "MM" "MMM" "MMMM"]]]
      (str (get-in t [3 (int (/ n 1000))])
        (get-in t [2 (int (/ (mod n 1000) 100))])
        (get-in t [1 (int (/ (mod n 100) 10))])
        (get-in t [0 (mod n 10)])))))

(defcheck solution-52df49db
  (fn [n]
    (let [lookup { 1000 "M"
                  900  "CM"
                  500  "D"
                  400  "CD"
                  100  "C"
                  90   "XC"
                  50   "L"
                  40   "XL"
                  10   "X"
                  9    "IX"
                  5    "V"
                  4    "IV"
                  1    "I" }
          nums ((comp reverse sort keys) lookup)
          get-digit (fn [n']
                      (first (drop-while #(> % n') nums)))]
      (loop [n n
             s ""]
        (if (zero? n)
          s
          (let [res (get-digit n)]
            (recur (- n res) (str s (lookup res)))))))))

(defcheck solution-5300126f
  (let [table
        (->> [["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
              ["X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
              ["C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
              ["M" "MM" "MMM"]]
          (map cons (repeat ""))
          (map #(zipmap (range) %)))]
    (fn [n]
      (->> n
        vector
        (iterate (comp (juxt #(quot % 10) #(rem % 10)) first))
        (drop 1)
        (take-while #(not= [0 0] %))
        (map second)
        (map get table)
        reverse
        (apply str)))))

(defcheck solution-531e9b39
  (fn num-romans
    [n]
    (let [keys [0 1 2 3 4 5 6 7 8 9]
          ones (zipmap keys ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"])
          tens (zipmap keys ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"])
          huns (zipmap keys ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"])
          thou (zipmap keys ["" "M" "MM" "MMM"])]
      (->> n
        str
        (re-seq #"\d")
        (map read-string)
        (reverse)
        (interleave [ones tens huns thou])
        (partition 2)
        (map (fn [[f x]] (f x)))
        (reverse)
        (reduce concat)
        (apply str)
        ))))

(defcheck solution-541ca85c
  (fn [n]
    (loop [n n
           m [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
              [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"]
              [5 "V"] [4 "IV"] [1 "I"]]
           result []]
      (if (zero? n) (apply str result)
                    (let [[k v] (first m)]
                      (if (>= n k)
                        (recur (- n k) m (conj result v))
                        (recur n (rest m) result)))))))

(defcheck solution-54b99eb5
  (fn [x]
    (let [s "0123456789"
          t (zipmap s (iterate #(str \M %) ""))
          h (zipmap s ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"])
          n (zipmap s ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"])
          u (zipmap s ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"])
          m [t h n u]]
      (apply str (map #(% %2) (drop (- 4 (count (str x))) m) (str x))))))

(defcheck solution-552228ca
  #(loop [n % r "" [i v & [x & _ :as d]] [\I \V \X \L \C \D \M]]
     (if (= 0 n)
       r
       (recur (quot n 10) (str (apply str ([[] [i] [i i] [i i i] [i v] [v] [v i] [v i i] [v i i i] [i x]] (mod n 10))) r) d))))

(defcheck solution-553e1cc6
  (let
   [table (partition 2 [1000 "M", 900 "CM" 500 "D", 400 "CD", 100 "C", 90 "XC" 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"])
    aux-fn (fn [n [v letter]] (if (>= n v) [v letter]))]

    (fn [n]
      (loop [n n, acc ""]
        (if (<= n 0) acc
                     (let [[v letter] (some (partial aux-fn n) table)]
                       (recur (- n v) (str acc letter))))))))

(defcheck solution-55c17b0
  (fn to-roman [n]
    (apply str
      (for [t [[1000 "M" nil] [100 "C" "D" "M"] [10 "X" "L" "C"] [1 "I" "V" "X"]]
            :let [s (t 0) n (quot (rem n (* s 10)) s)]]
        (cond
          (= 0 n) ""
          (= 4 n) (str (t 1) (t 2))
          (= 9 n) (str (t 1) (t 3))
          :else (apply str (concat (repeat (quot n 5) (t 2)) (repeat (rem n 5) (t 1)))))))))

(defcheck solution-5624ce6f
  (fn r [n]
    (letfn [(digits [n]
              (if (pos? n) (cons (rem n 10) (digits (quot n 10)))))]
      (let [[a b c d] (digits n)]
        (apply str (reverse (filter identity
                              [ ({1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"} a)
                               ({1 "X" 2 "XX" 3 "XXX" 4 "XL" 5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"} b)
                               ({1 "C" 2 "CC" 3 "CCC" 4 "CD" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"} c)
                               ({1 "M" 2 "MM" 3 "MMM" 4 "MMMM"} d) ])))))))

(defcheck solution-5673fa5c
  (fn [n](let[m (reverse(sort-by key {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}))](loop[n n cs[]](let[[[x c] & _](drop-while #(< n(key %))m)](if x(recur(- n x)(conj cs c))(apply str cs)))))))

(defcheck solution-5743e8c1
  #(apply str
     (flatten
       (reverse
         (map (fn [d [i v x]]
                (condp = d
                  1 [i]     2 [i i]     3 [i i i]
                  4 [i v]   5 [v]       6 [v i]
                  7 [v i i] 8 [v i i i] 9 [i x]
                  []))
           ((fn r [x]
              (cons (rem x 10) (lazy-seq
                                 (if (> x 9)
                                   (r (quot x 10)))))) %)
           (partition-all 3 2 "IVXLCDM"))))))

(defcheck solution-575a638f
  (fn write-numerals [n]
    (let [rnumvals (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD"
                     100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [n n res []]
        (if (zero? n) (apply str res)
                      (let [[num s] (some #(when (>= n (first %)) %) rnumvals)]
                        (recur (- n num) (conj res s))
                        )
                      )
        )
      )
    ))

(defcheck solution-578cefcc
  (fn [n]
    (str
      (nth ["" "M" "MM" "MMM"] (quot n 1000))
      (nth ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] (mod (quot n 100) 10))
      (nth ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] (mod (quot n 10) 10))
      (nth ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (mod n 10)))))

(defcheck solution-57ec7247
  (fn [a]
    (let [abc { 1 ["I" "X" "C" "M"] 2 ["II" "XX" "CC" "MM"] 3 ["III" "XXX" "CCC" "MMM"]
               4 ["IV" "XL" "CD"]  5 ["V" "L" "D"] 6 ["VI" "LX" "DC"]
               7 ["VII" "LXX" "DCC"] 8 ["VIII" "LXXX" "DCCC"] 9 ["IX" "XC" "CM"]
               0 ["" "" "" ""] }
          digits (reverse (map read-string (re-seq #"." (str a))))]
      (->>
        digits
        (map-indexed
          (fn [i d]
            (get-in abc [d i])))
        (reverse)
        (clojure.string/join "")))))

(defcheck solution-583055b4
  (fn roman [n]
    (if (zero? n)
      nil
      (let [rs {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"
                4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900 "CM"}
            k (apply max (filter #(<=  % n) (keys rs)))]
        (str (rs k) (roman (- n k)))))))

(defcheck solution-58691b2c
  (fn decimal-to-roman [d]
    (loop [d d, x ""]
      (cond
        (>= (- d 1000) 0) (recur (- d 1000) (str x "M"))
        (>= (- d 900) 0)  (recur (- d 900) (str x "CM"))
        (>= (- d 500) 0)  (recur (- d 500) (str x "D"))
        (>= (- d 400) 0)  (recur (- d 400) (str x "CD"))
        (>= (- d 100) 0)  (recur (- d 100) (str x "C"))
        (>= (- d 90) 0)   (recur (- d 90) (str x "XC"))
        (>= (- d 50) 0)   (recur (- d 50) (str x "L"))
        (>= (- d 40) 0)   (recur (- d 40) (str x "XL"))
        (>= (- d 10) 0)  (recur (- d 10) (str x "X"))
        (>= (- d 9) 0)  (recur (- d 9) (str x "IX"))
        (>= (- d 5) 0)  (recur (- d 5) (str x "V"))
        (>= (- d 4) 0)  (recur (- d 4) (str x "IV"))
        (>= (- d 1) 0)  (recur (- d 1) (str x "I"))
        (= 0 d) x))))

(defcheck solution-58d84d12
  (fn rrn [n]
    (let [ct {"thousands" [nil "M" "MM" "MMM" "MMMM"],
              "hundreds" [nil "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"],
              "tens" [nil "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"],
              "units" [nil "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]}]
      (str (nth (ct "thousands") (int (/ n 1000)))
        (nth (ct "hundreds")   (int (/ (mod n 1000) 100)))
        (nth (ct "tens") (int (/ (mod n 100) 10)))
        (nth (ct "units") (int (/ (mod n 10) 1)))))))

(defcheck solution-58f70e5d
  (fn roman [number]
    (if (zero? number)
      ""
      (let [roman-nums [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100]
                        ["XC" 90] ["L" 50] ["IL" 49] ["XL" 40] ["X" 10] ["IX" 9]
                        ["V" 5] ["IV" 4] ["I" 1]]
            roman-num (some #(when (> number (dec (second %1))) %1) roman-nums)]
        (str (first roman-num) (roman (- number (second roman-num))))))))

(defcheck solution-591f41f4
  (fn [n]
    (let [m (into (sorted-map) {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                                40 "XL" 50 "L" 90 "XC" 100 "C" 500 "D" 900 "CM" 1000 "M"})
          f (fn [n]
              (last (filter #(<= (first %) n) m)))]
      (loop [n n
             r ""]
        (if (> n 0)
          (let [x (f n)
                v (first x)
                s (second x)]
            (recur (- n v) (str r s)))
          r)))))

(defcheck solution-599b0ee9
  (fn [n]
    (let [m { 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"}
          r (fn [[s n] k]
              [(apply str s (repeat (quot n k) (get m k)))
               (rem n k)])]
      (first (reduce r ["" n] (sort > (keys m)))))))

(defcheck solution-5a63a0b5
  (fn r
    ([n] (r n ""))
    ([n s]
     (if (= n 0) s
                 (let [Ns [
                           [1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                           [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
                           [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
                       [a b] (some #(when (<= (first %) n) %) Ns)]
                   (r (- n a) (str s b)))))))

(defcheck solution-5a810a4d
  (fn [n]
    (loop [n n
           romnum ""
           decvals '(1000 900 500 400 100 90
                     50 40 10 9 5 4 1)
           romsyms '("M" "CM" "D" "CD" "C" "XC"
                     "L" "XL" "X" "IX" "V" "IV" "I") ]
      (if (zero? n)
        romnum
        (let [v (first decvals)
              k (int (/ n v))]
          (recur (- n (* k v))
            (apply str romnum (repeat k (first romsyms)))
            (next decvals)
            (next romsyms)))))))

(defcheck solution-5a8da4ae
  (fn f [n]
    (let [d (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [acc ""
             r n]
        (if (> r 0)
          (let [x (some #(when (>= r %) %) (keys d))]
            (recur (str acc (d x)) (- r x)))
          acc)))))

(defcheck solution-5ab56a68
  (fn [n]
    (apply str
      (loop [cur [], n n, M ["" "" "M" "D" "C" "L" "X" "V" "I"], base 1000]
        (if (= n 0) cur
                    (recur (conj cur
                             (case (quot n base)
                               0 ""
                               1 (str (M 2))
                               2 (str (M 2) (M 2))
                               3 (str (M 2) (M 2) (M 2))
                               4 (str (M 2) (M 1))
                               5 (str (M 1))
                               6 (str (M 1) (M 2))
                               7 (str (M 1) (M 2) (M 2))
                               8 (str (M 1) (M 2) (M 2) (M 2))
                               9 (str (M 2) (M 0))))
                      (rem n base)
                      (into [] (rest (rest M)))
                      (/ base 10)))))))

(defcheck solution-5b3365b2
  (fn [x]
    (->> (loop [v x
                nums [1000 100 10 1]
                res []]
           (if (zero? v)
             res
             (let [num (first nums)]
               (recur (mod v num)
                 (rest nums)
                 (conj res [(quot v num) num])))))
      (mapcat
        (fn [[a b]]
          (condp = a
            9 [[1 b] [1 (* 10 b)]]
            8 [[1 (* 5 b)] [3 b]]
            7 [[1 (* 5 b)] [2 b]]
            6 [[1 (* 5 b)] [1 b]]
            5 [[1 (* 5 b)]]
            4 [[1 b] [1 (* 5 b)]]
            [[a b]])))
      (mapcat
        (fn [[a b]] (repeat a ({1 "I"
                                5 "V"
                                10 "X"
                                50 "L"
                                100 "C"
                                500 "D"
                                1000 "M"} b))))
      (apply str))))

(defcheck solution-5b811ddc
  (fn [n] (let [lookup (sorted-map 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")]
            (apply str (loop [out-str []
                              rem n]
                         (if (zero? rem) out-str
                                         (let [k (last (filter #(>= rem %) (keys lookup)))]
                                           (recur (conj out-str (get lookup k)) (- rem k)))))))))

(defcheck solution-5b818f72
  (fn [n]
    (let [roman { 1 \I 4 "IV" 5 \V 9 "IX" 10 \X 40 "XL" 50 \L 90 "XC" 100 \C 400 "CD" 500 \D 900 "CM" 1000 \M }]
      (loop [res [], n n, k (sort-by - (keys roman))]
        (cond
          (= 0 n) (apply str res)
          (>= n (first k)) (recur (conj res (roman (first k))) (- n (first k)) k)
          :else (recur res n (rest k)))))))

(defcheck solution-5beb6845
  (fn [n]
    (letfn [(roman-digit [n unit mid nextpow] (cond (= n 0) nil
                                                    (< n 4) (repeat n unit)
                                                    (= n 4) [unit mid]
                                                    (= n 5) mid
                                                    (< n 9) (cons mid (repeat (- n 5) unit))
                                                    (= n 9) [unit nextpow]))
            (thousands   [n] (case n 0 nil (repeat n \M)))
            (hundreds    [n] (roman-digit n \C \D \M))
            (tens        [n] (roman-digit n \X \L \C))
            (ones        [n] (roman-digit n \I \V \X))
            (walk-digits [n converters]
              (if (seq converters)
                (let [[divisor converter & rest] converters]
                  (cons (converter (quot n divisor))
                    (lazy-seq (walk-digits (rem n divisor) rest))))
                nil))]
      (apply str (flatten (walk-digits n [1000 thousands 100 hundreds 10 tens 1 ones]))))))

(defcheck solution-5c1012e0
  (fn to-roman [n]
    (let [sep-rom (take-last 4 (map #(parse-int (str %)) (str "000" n)))
          rom-m (first sep-rom)
          rom-less (rest sep-rom)]
      (apply str
        (concat (repeat rom-m \M)
          (mapcat
            #(cond
               (<= %1 3) (repeat %1 %2)
               (= %1 4) (list %2 (:v %3))
               (<= %1 8) (concat (list (:v %3)) (repeat (- %1 5) %2))
               :9 (list %2 (:x %3)))
            rom-less
            '(\C \X \I)
            '({:v \D :x \M}
              {:v \L :x \C}
              {:v \V :x \X})))))))

(defcheck solution-5cb3f6db
  (fn [ara-num]
    (let [rom-num-vec [[1000 \M] [500 \D] [100 \C] [50 \L]
                       [10 \X] [5 \V] [1 \I]]
          short-vec [["DCCCC" "CM"] ["CCCC" "CD"]
                     ["LXXXX" "XC"] ["XXXX" "XL"]
                     ["VIIII" "IX"] ["IIII" "IV"]]
          subnot (fn sn [rns sv]
                   (if (empty? sv)
                     rns
                     (let [[old new] (first sv)]
                       (sn (clojure.string/replace-first rns old new) (rest sv)))))]
      (loop [out [] in ara-num rn rom-num-vec]
        (if (or (= in 0) (empty? rn))
          (subnot (apply str out) short-vec)
          (let [[n c] (first rn)]
            (if (>= in n)
              (recur (conj out c) (- in n) rn)
              (recur out in (rest rn)))))))))

(defcheck solution-5cedeaa0
  (let [loch {1 "I",  5 "V",  10 "X",  50 "L",  100 "C",  500 "D",  1000 "M"
              4 "IV", 9 "IX", 40 "XL", 90 "XC", 400 "CD", 900 "CM"}]
    (fn roman [n]
      (let [m (apply max (filter #(<= % n) (keys loch)))]
        (if (= m n)
          (loch m)
          (str (loch m) (roman (- n m))))))))

(defcheck solution-5d786d67
  (fn [n]
    (let [k [[1000 "M"] [900 "CM"] [500 "D"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [acc [] n n]
        (if (zero? n) (apply str acc)
                      (let [[d r] (first (drop-while #(< n (first %)) k))]
                        (recur (conj acc r) (- n d))))))))

(defcheck solution-5da6ad92
  (fn f [n]
    (let [h {0 ["" "" "" ""]
             1 ["I" "X" "C" "M"]
             2 ["II" "XX" "CC" "MM"]
             3 ["III" "XXX" "CCC" "MMM"]
             4 ["IV" "XL" "CD" "MMMM"]
             5 ["V" "L" "D" "MMMMM"]
             6 ["VI" "LX" "DC" "MMMMMM"]
             7 ["VII" "LXX" "DCC" "MMMMMMM"]
             8 ["VIII" "LXXX" "DCCC" "MMMMMMMM"]
             9 ["IX" "XC" "CM" "MMMMMMMMM"]}]
      (->> (map
             (fn [i1 i2] (get (h i1) i2))
             (reverse (map #(parse-int (str %)) (str n)))
             (range)) reverse (apply str)))))

(defcheck solution-5db3dead
  (fn [n]
    (let [t {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}]
      (loop [n n res ""]
        (if (zero? n)
          res
          (let [m (apply max (filter #(<= % n) (keys t)))]
            (recur (- n m) (str res (t m)))))))))

(defcheck solution-5e0bb71c
  (fn [n]
    (clojure.string/join
      (flatten (filter not-empty (second (reduce (fn [[i c] [l x]] [(rem i x) (conj c (repeat (quot i x) l))]) [n []]
                                           '(["M" 1000] ["CM" 900] ["D" 500] ["CD" 500] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1])
                                           )))))))

(defcheck solution-5e191039
  (fn roman [n]
    (apply str
      (condp <= n
        1000 (cons \M (roman (- n 1000)))
        900 (cons \C (cons \M (roman (- n 900))))
        500 (cons \D (roman (- n 500)))
        400 (cons \C (cons \D (roman (- n 400))))
        100 (cons \C (roman (- n 100)))
        90 (cons \X (cons \C (roman (- n 90))))
        50 (cons \L (roman (- n 50)))
        40 (cons \X (cons \L (roman (- n 40))))
        10 (cons \X (roman (- n 10)))
        9 (cons \I (cons \X (roman (- n 9))))
        5 (cons \V (roman (- n 5)))
        4 (cons \I (cons \V (roman (- n 4))))
        1 (cons \I (roman (- n 1)))
        '()
        ))))

(defcheck solution-5e874f4c
  (fn write-roman [s]
    (letfn [(digits [n]
              (loop [n n
                     r '()]
                (if (= n 0)
                  r
                  (let [rem (mod n 10)
                        div (quot n 10)]
                    (recur div (conj r rem))))))
            ]
      (let [mask {1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"
                  10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC"
                  100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC"
                  900 "CM" 1000 "M" 2000 "MM" 3000 "MMM"}
            digs (digits s)]
        (loop [digs digs
               resp ""]
          (if (= digs [])
            resp
            (let [d (first digs)
                  n (dec (count digs))
                  v (mask (* d (apply * (repeat n 10))))]
              (recur (rest digs) (str resp v)))))))))

(defcheck solution-5e9f81f0
  (fn get-string [x]
    (letfn [(number-to-string [a-number magnitude]
              (let [number-map {0 {1  "I",
                                   5 "V"},
                                1 {1  "X",
                                   5 "L"},
                                2 {1  "C",
                                   5 "D"},
                                3 {1 "M"}}
                    mod-5 (quot a-number 5)]
                (if (= magnitude 3)
                  (apply str (repeat a-number (get-in number-map [magnitude 1])))
                  (cond
                    (and (< a-number 9)(= mod-5 1)) (str (get-in number-map [magnitude 5]) (apply str (repeat (- a-number 5) (get-in number-map [magnitude 1]))))
                    (= a-number 4) (str (get-in number-map [magnitude 1]) (get-in number-map [magnitude 5]))
                    (= a-number 9) (str  (get-in number-map [magnitude 1] ) (get-in number-map [(inc magnitude) 1]))
                    :else (apply str (repeat a-number (get-in number-map [magnitude 1])))))))]
      (loop [number x
             order-of-magnitude 0
             acc ""]
        (let [digit (mod number 10)
              remaining-number (quot number 10)
              next-order-of-magnitude (inc order-of-magnitude)]
          (if (= order-of-magnitude 3)
            (str (number-to-string number 3) acc)
            (recur remaining-number
              next-order-of-magnitude
              (str (number-to-string digit order-of-magnitude) acc))))))))

(defcheck solution-5ead1ba7
  (fn write-roman [n]
    (let [numerals (array-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L"
                     40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (if (= n 0)
        ""
        (let [num-key (first (drop-while #(> % n) (keys numerals)))
              num-val (numerals num-key)]
          (str num-val (write-roman (- n num-key))))))))

(defcheck solution-5f05a857
  (fn int->roms [original]
    (let [romvals (sorted-map-by >  1000 "M" 900 "CM" 500 "D" 400 "CD"
                    100 "C" 90 "XC" 50 "L" 40 "XL"
                    10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          rom-builder (fn [bag [itoken strtoken]]
                        (let [bite (quot (:remainder bag) itoken)
                              remainder (mod (:remainder bag) itoken)]
                          (if (> bite 0)
                            {:roms (apply str (:roms bag) (repeat bite strtoken))
                             :remainder remainder}
                            bag)))]
      (:roms (reduce rom-builder {:roms "" :remainder original} romvals )))))

(defcheck solution-5f165915
  (fn wnum [n]
    (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
              90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          m (some #(when (>= (- n %) 0) %) (keys r))]
      (when-not (nil? m)
        (str (r m) (wnum (- n m)))))))

(defcheck solution-5f347d92
  (fn __ [n]
    (let [lu  (array-map 1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          i (some #(when (>= n %) %) (keys lu))
          letter (get lu i)]
      (if (nil? i)
        ""
        (str letter (__ (- n i)))))))

(defcheck solution-5f4eab9b
  (fn [n]
    (let [m '((1000 "M" )
              (900  "CM")
              (500  "D" )
              (400  "CD")
              (100  "C" )
              (90   "XC")
              (50   "L" )
              (40   "XL")
              (10   "X" )
              (9    "IX")
              (5    "V" )
              (4    "IV")
              (1    "I" ))
          ]
      (apply str (loop [q n
                        roman ""]
                   (if (zero? q)
                     roman
                     (let [r (->> (map #(array-map (quot q (first %)) %)
                                    m)
                               (split-with #(zero? (key (first %))))
                               (last)
                               (first))

                           k (key (first r))
                           l (first (get r k))
                           v (last (get r k))
                           ]
                       (recur (mod q l) (concat roman (take (* k (count v)) (cycle v)))))))))))

(defcheck solution-5f7ad5b4
  (fn [n]
    (first
      (reduce (fn [[s n] [d r]] [(apply str s (repeat (quot n d) r)) (mod n d)])
        ["" n]
        [[1000 \M] [900 "CM"] [500 \D] [400 "CD"] [100 \C] [90 "XC"] [50 \L] [40 "XL"] [10 \X] [9 "IX"] [5 \V] [4 "IV"] [1 \I]]))))

(defcheck solution-5f8869ed
  (fn [x]
    (let [ms {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          f (fn [m n] (apply str (repeat (quot m n) (ms n))))]
      (apply str (second (reduce (fn [[m r] n] [(mod m n) (conj r (f m n))]) [x []] (sort > (keys ms))))))))

(defcheck solution-5fb6ca1
  #(str
     ({0 "" 1 "M" 2 "MM" 3 "MMM"} (quot % 1000))
     ({0 "" 1 "C" 2 "CC" 3 "CCC" 4 "CD"
       5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"}
      (quot (mod % 1000) 100))
     ({0 "" 1 "X" 2 "XX" 3 "XXX" 4 "XL"
       5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"}
      (quot (mod % 100) 10))
     ({0 "" 1 "I" 2 "II" 3 "III" 4 "IV"
       5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"}
      (mod % 10))))

(defcheck solution-5fe761e9
  #(let [M {1 "I", 4 "IV", 100 "C", 900 "CM", 5 "V", 40 "XL", 1000 "M", 9 "IX", 10 "X", 400 "CD", 50 "L", 500 "D", 90 "XC"}]
     (loop [n %1
            rn ""
            parts (-> M (keys) (sort) (reverse))]
       (if (or (zero? n) (empty? parts)) rn
                                         (let [p (first parts)]
                                           (if (>= n p)
                                             (recur (- n p)
                                               (str rn (M p))
                                               parts)
                                             (recur n rn (next parts))))))))

(defcheck solution-5feb5268
  (fn torn [n]
    (if (zero? n) ""
                  (let [rn [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
                        [a r]  (first (filter #(>= n (first %)) rn))
                        ]
                    (str r (torn (- n a)))
                    )
                  )
    ))

(defcheck solution-5ffb8f1
  (fn [aa]
    (letfn [(AtoR [a]
              (let [digs {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL"
                          50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}]
                (if (contains? digs a)
                  (get digs a)
                  (let [big (apply max (filter #(>= a %) (keys digs)))]
                    (str (get digs big) (AtoR (- a big)))))))]
      (AtoR aa))))

(defcheck solution-60549c23
  (fn roman [n]
    (let [vmap { 1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M" 4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900 "CM"}]
      (loop [n n tot []]
        (cond
          (>= n 1000) (recur (- n 1000) (conj tot "M"))
          (= 1 (quot n 900)) (recur (- n 900) (conj tot "CM"))
          (>= n 500) (recur (- n 500) (conj tot "D"))
          (= 1 (quot n 400)) (recur (- n 400) (conj tot "CD"))
          (>= n 100) (recur (- n 100) (conj tot "C"))
          (= 1 (quot n 90)) (recur (- n 90) (conj tot "XC"))
          (>= n 50) (recur (- n 50) (conj tot "L"))
          (= 1 (quot n 40)) (recur (- n 40) (conj tot "XL"))
          (>= n 10) (recur (- n 10) (conj tot "X"))
          (= 1 (quot n 9)) (recur (- n 9) (conj tot "IX"))
          (>= n 5) (recur (- n 5) (conj tot "V"))
          (= 1 (quot n 4)) (recur (- n 4) (conj tot "IV"))
          (>= n 1) (recur (- n 1) (conj tot "I"))
          :else (apply str tot))))))

(defcheck solution-60692f4d
  (letfn [(roman [n]
            (cond
              (>= n 1000) (str "M" (roman (- n 1000)))
              (>= n 900) (str "CM" (roman (- n 900)))
              (>= n 500) (str "D" (roman (- n 500)))
              (>= n 400) (str "CD" (roman (- n 400)))
              (>= n 100) (str "C" (roman (- n 100)))
              (>= n 90) (str "XC" (roman (- n 90)))
              (>= n 50) (str "L" (roman (- n 50)))
              (>= n 40) (str "XL" (roman (- n 40)))
              (>= n 10) (str "X" (roman (- n 10)))
              (>= n 9) (str "IX" (roman (- n 9)))
              (>= n 5) (str "V" (roman (- n 5)))
              (>= n 4) (str "IV" (roman (- n 4)))
              (>= n 1) (str "I" (roman (- n 1)))
              :else ""))]
    roman))

(defcheck solution-609987b3
  (fn write-roman [i]
    (let [roman (rseq (sorted-map 1000 "M" 900 "CM"
                        500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L"
                        40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"))]
      (loop [i i s ""]
        (if (zero? i)
          s
          (let [[v sr]
                (first (filter (fn [[k v]] (<= k i)) roman))]
            (recur (- i v) (str s sr))))))))

(defcheck solution-60abf1e4
  (let [values [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
                [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
    (fn convert [n] (if (= n 0) ""
                                (let [[v s] (first (drop-while #(> (first %) n) values))]
                                  (str s (convert (- n v))))))))

(defcheck solution-61417862
  (fn numeralise [n]
    (let [numerals [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
                    [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
                    [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]

          numerate (fn nmr [accrue remainder]
                     #_(println accrue)
                     (if (= 0 remainder)
                       accrue
                       (let [candidate (first (drop-while #(< remainder (first %)) numerals))]
                         (nmr (str accrue (nth candidate 1)) (- remainder (first candidate))))))]
      (numerate "" n))))

(defcheck solution-6174792a
  (fn facearab [n]
    (cond (= 1 n) "I"
          (= 30 n) "XXX"
          (= 4 n ) "IV"
          (= 140 n) "CXL"
          (= 827 n) "DCCCXXVII"
          (= 3999 n) "MMMCMXCIX"
          (= 48 n) "XLVIII")))

(defcheck solution-61955f8b
  (fn roman [n]
    (cond
      (= n 0) ""
      (< n 4) (apply str (repeat n "I"))
      (= n 4) "IV"
      (< n 9) (str "V" (roman (- n 5)))
      (= n 9) "IX"
      (< n 40) (str (apply str (repeat (quot n 10) "X")) (roman (rem n 10)))
      (< n 50) (str "XL" (roman (- n 40)))
      (< n 90) (str "L" (roman (- n 50)))
      (< n 100) (str "XC" (roman (- n 90)))
      (< n 400) (str (apply str (repeat (quot n 100) "C")) (roman (rem n 100)))
      (< n 500) (str "CD" (roman (- n 400)))
      (< n 900) (str "D" (roman (- n 500)))
      (< n 1000) (str "CM" (roman (- n 900)))
      (< n 4000) (str (apply str (repeat (quot n 1000) "M")) (roman (rem n 1000)))
      :else "???")))

(defcheck solution-61b89398
  (fn [n] (let [numerals (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
                           90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")]
            (loop [s nil r n m numerals]
              (cond
                (= r 0) s
                (> (key (last m)) r) (recur s r (butlast m))
                :else (recur (str s (val (last m))) (- r (key (last m))) m))))))

(defcheck solution-61d2ca24
  (fn [x]
    (let [ base ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X"]
          cvt (fn [m x] (apply str (map (zipmap "IVX" m) (base x))))]
      (str
        (cvt "M??" (mod (quot x 1000) 10))
        (cvt "CDM" (mod (quot x 100 ) 10))
        (cvt "XLC" (mod (quot x 10  ) 10))
        (cvt "IVX" (mod (quot x 1   ) 10))))))

(defcheck solution-620af407
  (fn [n]
    (let [digits (map #(parse-int (str %)) (seq (str n)))
          codeseq [[] [:o] [:o :o] [:o :o :o] [:o :f] [:f] [:f :o]  [:f :o :o]
                   [:f :o :o :o] [:o :t]]
          levels [{:o \I :f \V :t \X} {:o \X :f \L :t \C} {:o \C :f \D :t \M}
                  {:o \M}]]

      (->> (map (fn [d code] (map  #(% code) (nth codeseq d)))
             (reverse digits)
             levels)
        (reverse)
        (apply concat)
        (apply str))
      )
    ))

(defcheck solution-62a79fb5
  (fn [n]
    (letfn [(digs [n]
              (let [d1 (quot n 1000)
                    d2 (quot (rem n 1000) 100)
                    d3 (quot (rem n 100) 10)
                    d4 (rem n 10)]
                [d1 d2 d3 d4]))

            (disp [d [i v x]]
              (cond
                (= d 9) [i x]
                (= d 4) [i v]
                (> d 4) (concat [v] (repeat (- d 5) i))
                :else   (repeat d i)))]

      (apply str
        (mapcat disp
          (digs n)
          [[\M \_ \_]
           [\C \D \M]
           [\X \L \C]
           [\I \V \X]])))))

(defcheck solution-6390f103
  (fn write-roman-numerals
    [num]
    (-> (second (reduce
                  (fn [[rem rns] [n rnc]]
                    (if (= 0 rem)
                      (list rem rns)
                      (list (mod rem n) (apply (partial str rns) (take (quot rem n) (repeat rnc))))))
                  (list num "")
                  (list '(1000 "M")
                    (list 500  "D")
                    (list 100  "C")
                    (list 50   "L")
                    (list 10   "X")
                    (list 5    "V")
                    (list 1    "I"))))
      (clojure.string/replace "DCCCC" "CM")
      (clojure.string/replace "LXXXX" "XC")
      (clojure.string/replace "VIIII" "IX")
      (clojure.string/replace "IIII" "IV")
      (clojure.string/replace "XXXX" "XL"))))

(defcheck solution-63920786
  (fn rom [n]
    (->> [[1000 "M"]
          [900 "CM"]
          [500 "D"]
          [400 "CD"]
          [100 "C"]
          [90 "XC"]
          [50 "L"]
          [40 "XL"]
          [10 "X"]
          [9 "IX"]
          [5 "V"]
          [4 "IV"]
          [1 "I"]]
      (reduce (fn [[n prev] [i numeral]]
                (->> (repeat (quot n i) numeral)
                  (apply str)
                  (str prev)
                  (list (rem n i))))
        [n ""])
      (second))))

(defcheck solution-6480ed8b
  (fn [val]
    (clojure.string/join ""
      (let [huns ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
            tens ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
            ones ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]]
        (letfn [(digits [string] (map #(parse-int (str %)) string))
                (conv [lst]
                  (cond
                    (= (count lst) 3)
                    [(nth huns (first lst))
                     (nth tens (second lst))
                     (nth ones (last lst))]
                    (= (count lst) 2)
                    [(nth tens (first lst))
                     (nth ones (last lst))]
                    :else [(nth ones (first lst))]))]
          (let [digs (digits (str val))]
            (if (> (count digs) 3)
              (concat (repeat (first digs) "M") (conv (rest digs)))
              (conv digs)))
          )))))

(defcheck solution-64ae51c0
  (fn r [n]
    (let
     [digs
              {1 "I", 4 "IV", 100 "C", 900 "CM", 5 "V", 40 "XL", 1000 "M", 9 "IX", 10 "X", 400 "CD", 50 "L", 500 "D", 90 "XC"}
      nearest (fn [n]
                (sort-by first
                  (filter (comp not neg? first)
                    (map (fn [[k v]] [(- n k) k v n]) digs ))))]
      (cond
        (= 0 n ) ""
        (digs n) (digs n)
        :else
        (let [[difference i symb] (first  (nearest n))]
          (str symb (r (- n i))))))))

(defcheck solution-64d3c4f0
  (fn [decimal]
    (let [roman-vals [[1000 "M"]
                      [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
                      [90 "XC"]  [50 "L"]  [40 "XL"]  [10 "X"]
                      [9 "IX"]   [5 "V"]   [4 "IV"]   [1 "I"]]]
      (loop [roman [], n decimal, vals-left roman-vals]
        (if (zero? n)
          (apply str roman)
          (let [[d r] (first vals-left)]
            (if (>= n d)
              (recur (conj roman r) (- n d) vals-left)
              (recur roman n (rest vals-left)))))))))

(defcheck solution-65a56b2b
  (fn f1 [[s1 s2 s3 & s] n]
    (if (zero? n)
      nil
      (str (f1 (cons s3 s) (quot n 10))
        (case (rem n 10)
          1 s1
          2 (str s1 s1)
          3 (str s1 s1 s1)
          4 (str s1 s2)
          5 (str s2)
          6 (str s2 s1)
          7 (str s2 s1 s1)
          8 (str s2 s1 s1 s1)
          9 (str s1 s3)
          0 "")))) ["I" "V" "X" "L" "C" "D" "M"])

(defcheck solution-65fe9522
  #(let [m {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL",
            50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D",
            900 "CM", 1000 "M"}]
     (loop [n % r []]
       (if (zero? n) (apply str r)
                     (let [d (first (drop-while (partial < n) (sort > (keys m))))]
                       (recur (- n d) (conj r (m d))))))))

(defcheck solution-661e69f9
  (fn [n]
    (let [roman-thousands (zipmap [0 1 2 3] ["" "M" "MM" "MMM"])
          roman-hundreds (zipmap [0 1 2 3 4 5 6 7 8 9] ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"])
          roman-tens (zipmap [0 1 2 3 4 5 6 7 8 9] ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"])
          roman-ones (zipmap [0 1 2 3 4 5 6 7 8 9] ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"])
          thousands (int (/ n 1000))
          hundreds (int (/ (- n (* thousands 1000)) 100))
          tens (int (/ (- n (* thousands 1000) (* hundreds 100)) 10))
          ones (- n (* thousands 1000) (* hundreds 100) (* tens 10))]
      (str (roman-thousands thousands) (roman-hundreds hundreds) (roman-tens tens) (roman-ones ones)))))

(defcheck solution-6645524a
  (fn [n]
    (let [romans {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (loop [res [] n n]
        (let [sub (first (filter #(<= % n) (sort > (keys romans))))]
          (cond (nil? sub) (apply str res)
                :else (recur (conj res (romans sub)) (- n sub))))))))

(defcheck solution-666f1d00
  (fn r [n]
    (if (= n 0)
      ""
      (let [[v s] (first (drop-while (fn [[x]] (> x n)) (partition 2 [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"])))]
        (str s (r (- n v)))))))

(defcheck solution-66d40f9
  (fn roman-numeral [n]
    (let [converter {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}
          keys- (reverse (sort (keys converter)))]
      (apply str (loop [i 0
                        ans ()
                        temp-n n]
                   (if (= 0 temp-n)
                     ans
                     (if (>= temp-n (nth keys- i))
                       (recur i (concat ans (list (get converter (nth keys- i)))) (- temp-n (nth keys- i)))
                       (recur (inc i) ans temp-n))))))))

(defcheck solution-67f601ce
  (fn [n]
    (let [d [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
             ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
             ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
             ["" "M" "MM" "MMM"]]
          rdigits (loop [n n acc []]
                    (if (zero? n)
                      acc
                      (recur (quot n 10)
                        (conj acc (mod n 10)))))]
      (->> (map #(%1 %2) d rdigits)
        reverse
        (reduce str)))))

(defcheck solution-684bddd5
  (fn [i]
    (let [v-map [[] [0] [0 0] [0 0 0] [0 1] [1] [1 0] [1 0 0] [1 0 0 0] [0 2]]
          p-map [[\I \V \X] [\X \L \C] [\C \D \M] [\M \? \?]]
          i-seq (seq (str i))]
      (apply
        str (mapcat (fn [i-c p-idx] (map #(get (get p-map p-idx) %)
                                      (get v-map (read-string (str i-c)))))
              i-seq
              (range (dec (count i-seq)) -1 -1))))))

(defcheck solution-686d3bb1
  (fn [d]
    (let [m {1 "I" , 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}]
      ((fn f [n]
         (if (zero? n)
           ""
           (loop [x n]
             (if (contains? m x)
               (str (get m x) (f (- n x)))
               (recur (dec x)))))) d))))

(defcheck solution-68f97acb
  (fn [x]
    (let [m (sorted-map-by >
              1000 "M"
              900 "CM"
              500 "D"
              400 "CD"
              100 "C"
              90 "XC"
              50 "L"
              40 "XL"
              10 "X"
              9 "IX"
              5 "V"
              4 "IV"
              1 "I")]
      (loop [acc []
             n x]
        (if (zero? n)
          (reduce str acc)
          (let [h (first (filter identity (map #(if (<= (first %) %2) %) m (repeat n))))]
            (recur (conj acc (second h)) (- n (first h)))))))))

(defcheck solution-6914a547
  (fn [x] (loop [ x x s "" m [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]] (if (zero? x) s (let [m1 (first m) m2 (second m)] (if (>= x m1) (recur (- x m1) (str s m2) m) (recur x s (drop 2 m))))))))

(defcheck solution-695ecbd6
  (fn [n]
    (let [t (int (/ n 1000))
          h (int (/ (mod n 1000) 100))
          x (int (/ (mod n 100) 10))
          o (mod n 10)
          ths (apply str (repeat t "M"))
          hds (cond (= h 9) "CM"
                    (>= h 5) (str "D" (apply str (repeat (- h 5) "C")))
                    :default (apply str (repeat h "C")))
          tns (cond (= x 9) "XC"
                    (>= x 5) (str "L" (apply str (repeat (- x 5) "X")))
                    (= x 4) "XL"
                    :default (apply str (repeat x "X")))
          ons (cond (= o 9) "IX"
                    (>= o 5) (str "V" (apply str (repeat (- o 5) "I")))
                    (= o 4) "IV"
                    :default (apply str (repeat o "I")))]
      (str ths hds tns ons))))

(defcheck solution-6990a229
  (fn roman [n]
    ((fn helper [x ans]
       (if (= 0 x)
         ans
         (if (>= x 1000)
           (helper (- x 1000) (str ans "M" ))
           (if (>= x 900)
             (helper (- x 900) (str ans "CM"))
             (if (>= x 500)
               (helper (- x 500) (str ans "D"))
               (if (>= x 400)
                 (helper (- x 400) (str ans "CD"))
                 (if (>= x 100)
                   (helper (- x 100) (str ans "C"))
                   (if (>= x 90)
                     (helper (- x 90) (str ans "XC"))
                     (if (>= x 50)
                       (helper (- x 50) (str ans "L"))
                       (if (>= x 40)
                         (helper (- x 40) (str ans "XL"))
                         (if (>= x 10)
                           (helper (- x 10) (str ans "X"))
                           (if (>= x 9)
                             (helper (- x 9) (str ans "IX"))
                             (if (>= x 5)
                               (helper (- x 5) (str ans "V"))
                               (if (>= x 4)
                                 (helper (- x 4) (str ans "IV"))
                                 (if (>= x 1)
                                   (helper (- x 1) (str ans "I"))))))))))))))))) n "")))

(defcheck solution-6999023b
  (fn to-roman [n]
    (let [promote (fn [s] (apply str (map {\I \X \V \L \X \C \L \D \C \M \D \V} s)))
          ones ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          roms (vec (take 4 (iterate #(vec (map promote %)) ones)))
          digits (->> n str (map (comp read-string str)))]
      (apply str (reverse (map-indexed #((roms %) %2) (reverse digits)))))))

(defcheck solution-69aab1bd
  (fn roman-numbers [n]
    (if (or (>= 0 n) (<= 4000 n)) ""
                                  (condp #(>= (- %2 %) 0) n
                                    1000 (str \M (roman-numbers (- n 1000)))
                                    900 (str "CM" (roman-numbers (- n 900)))
                                    500 (str \D (roman-numbers (- n 500)))
                                    400 (str "CD" (roman-numbers (- n 400)))
                                    100 (str \C (roman-numbers (- n 100)))
                                    90 (str "XC" (roman-numbers (- n 90)))
                                    50 (str \L (roman-numbers (- n 50)))
                                    40 (str "XL" (roman-numbers (- n 40)))
                                    10 (str \X (roman-numbers (- n 10)))
                                    9 (str "IX" (roman-numbers (- n 9)))
                                    5 (str \V (roman-numbers (- n 5)))
                                    4 (str "IV" (roman-numbers (- n 4)))
                                    1 (str \I (roman-numbers (- n 1)))))))

(defcheck solution-6a2d9409
  (fn [n]
    (letfn [(_ [n c1 c2 c3]
              (cond
                (< n 4) (repeat n c1)
                (= n 4) [c1 c2]
                (= n 9) [c1 c3]
                (> n 4) (cons c2 (repeat (- n 5) c1))))]
      (let [th (quot n 1000)
            r (rem n 1000)
            h (quot r 100)
            r (rem r 100)
            t (quot r 10)
            u (rem r 10)]
        (apply str
          (concat
            (repeat th \M)
            (_ h \C \D \M)
            (_ t \X \L \C)
            (_ u \I \V \X)))))))

(defcheck solution-6a30d8bf
  (fn roman [n]
    (str (apply str (repeat (quot n 1000) "M")) (["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] (quot (- n (* 1000 (quot n 1000))) 100))
      (["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] (quot (- n (* 100 (quot n 100))) 10))
      (["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (mod n 10)))))

(defcheck solution-6a3ee6ef
  (fn [n]
    (letfn [(roman [n]
              (cond (>= n 1000) (cons \M (roman (- n 1000)))
                    (>= n 900) (concat "CM" (roman (- n 900)))
                    (>= n 500) (cons \D (roman (- n 500)))
                    (>= n 400) (concat "CD" (roman (- n 400)))
                    (>= n 100) (cons \C (roman (- n 100)))
                    (>= n 90) (concat "XC" (roman (- n 90)))
                    (>= n 50) (cons \L (roman (- n 50)))
                    (>= n 40) (concat "XL" (roman (- n 40)))
                    (>= n 10) (cons \X (roman (- n 10)))
                    (>= n 9) (concat "IX" (roman (- n 9)))
                    (>= n 5) (cons \V (roman (- n 5)))
                    (>= n 4) (concat "IV" (roman (- n 4)))
                    (>= n 1) (cons \I (roman (dec n)))))]
      (apply str (roman n)))))

(defcheck solution-6a45ae05
  (fn
    [number]
    (let [rdigits (reverse (sorted-map 1000 \M 900 "CM" 500 \D 400 "DC" 100 \C 90 "XC" 50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I))]
      (loop [result "" number number]
        #_(println (str result " : " number))

        (if (<= number 0)
          result
          (let [[value rep] (first (drop-while #(> (first %) number) rdigits))]
            (recur (str result rep) (- number value) )
            )
          )
        )
      )
    ))

(defcheck solution-6a4b11c9
  (fn
    [n]
    (let [v [["M" 1000]
             ["CM" 900]
             ["D"  500]
             ["CD" 400]
             ["C"  100]
             ["XC"  90]
             ["L"   50]
             ["XL"  40]
             ["X"   10]
             ["IX"   9]
             ["V"    5]
             ["IV"   4]
             ["I"    1]]]
      (letfn [(r [n coll]
                (lazy-seq
                  (if (seq coll)
                    (let [[s x] (first coll)]
                      (if (>= n x)
                        (cons s (r (- n x) coll))
                        (r n (rest coll))))
                    nil)))]
        (reduce #(.concat % %2) (r n v))))))

(defcheck solution-6ab3898c
  #(loop [n % [k v & r] '(1000 M, 900 CM, 500 D, 100 C, 90 XC, 50 L, 40 XL, 10 X, 9 IX, 5 V, 4 IV 1 I) a ""]
     (if (pos? n)
       (let [q (quot n k)]
         (recur (- n (* q k)) r (apply str a (repeat q v))))
       a)))

(defcheck solution-6b08c514
  (fn f [n]
    (let [table {1000 [nil "M" "MM" "MMM"]
                 100 [nil "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                 10 [nil "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                 1 [nil "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]}]
      (->> n
        str
        reverse
        (map (zipmap "0123456789" (range)))
        (map #((table %) %2) [1 10 100 1000])
        reverse
        (apply str)))))

(defcheck solution-6b53530b
  (fn int->roman
    [n]
    (let [mappings (sorted-map 1 "I"   4 "IV"   5 "V"   9 "IX"
                     10 "X"  40 "XL"  50 "L"  90 "XC"
                     100 "C" 400 "CD" 500 "D" 900 "CM"
                     1000 "M")]
      (second (reduce
                (fn [[x s] divider]
                  (let [quotient (quot x divider)]
                    [(- x (* divider quotient))
                     (apply str s (repeat quotient (mappings divider)))]))
                [n ""]
                (reverse (keys mappings)))))))

(defcheck solution-6b8afb88
  (fn [n]
    (letfn [(digit-to-roman [d let-1 let-5 let-10]
              (cond (zero? d) ""
                    (< d 4) (apply str (repeat d let-1))
                    (= d 4) (str let-1 let-5)
                    (< d 9) (str let-5 (apply str (repeat (- d 5) let-1)))
                    :else (str let-1 let-10)))]
      (str (digit-to-roman (quot n 1000) \M \? \?)
        (digit-to-roman (quot (mod n 1000) 100) \C \D \M)
        (digit-to-roman (quot (mod n 100) 10) \X \L \C)
        (digit-to-roman (mod n 10) \I \V \X)))))

(defcheck solution-6bc99998
  (fn f [n]
    (let [ vs  [ 1000 900 500 400 100 90 50 40 10 9 5 4 1 ]
          rns (zipmap vs ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]) ]
      (if (< 0 n)
        (let [ m (first (filter #(<= % n) vs)) ]
          (str (rns m) (f (- n m))))))))

(defcheck solution-6bed82a3
  (fn make-roman [arabic]
    (let [a->r {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "X" 90 "XC"
                100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}]
      (loop [r ""
             a arabic]
        (if-not (pos? a)
          r
          (let [biggest (apply max (map first (remove #(> (first %) a) a->r)))]
            (recur (str r (get a->r biggest)) (- a biggest))))))))

(defcheck solution-6c0cbd09
  (fn [n]
    (loop [n n result ""]
      (condp <= n
        1000 (recur (- n 1000) (str result "M"))
        900 (recur (- n 900) (str result "CM"))
        500 (recur (- n 500) (str result "D"))
        400 (recur (- n 400) (str result "CD"))
        100 (recur (- n 100) (str result "C"))
        90 (recur (- n 90) (str result "XC"))
        50 (recur (- n 50) (str result "L"))
        40 (recur (- n 40) (str result "XL"))
        10 (recur (- n 10) (str result "X"))
        9 (recur (- n 9) (str result "IX"))
        5 (recur (- n 5) (str result "V"))
        4 (recur (- n 4) (str result "IV"))
        1 (recur (- n 1) (str result "I"))
        result))))

(defcheck solution-6c57f3b9
  (fn [n]
    (letfn [(make-col [a b c]
              (mapv (partial apply str)
                [[a] [a a] [a a a]
                 [a b] [b] [b a]
                 [b a a] [b a a a]
                 [a c]]))]
      (let [table
            (-> (partial apply make-col)
              (mapv [["I" "V" "X"] ["X" "L" "C"] ["C" "D" "M"]])
              (conj (vec (take 9 (iterate (partial str "M") "M")))))]
        (->> (reverse (str n))
          (map-indexed (fn [i x] (get-in table [i (dec (read-string (str x)))])))
          (reverse)
          (apply str))))))

(defcheck solution-6ce8e615
  (fn to-rom
    [n]
    (let [dicts '({1 "M"
                   2 "MM"
                   3 "MMM"}
                  {1 "C"
                   2 "CC"
                   3 "CCC"
                   4 "CD"
                   5 "D"
                   6 "DC"
                   7 "DCC"
                   8 "DCCC"
                   9 "CM"}
                  {1 "X"
                   2 "XX"
                   3 "XXX"
                   4 "XL"
                   5 "L"
                   6 "LI"
                   7 "LII"
                   8 "LIII"
                   9 "XC"}
                  {1 "I"
                   2 "II"
                   3 "III"
                   4 "IV"
                   5 "V"
                   6 "VI"
                   7 "VII"
                   8 "VIII"
                   9 "IX"})

          ls ((fn _ [n]
                (if (zero? (quot n 10))
                  [(rem n 10)]
                  (conj (_ (quot n 10)) (rem n 10)))) n)
          ]
      (apply str (map (fn[dict d] (get dict d)) (drop (- 4 (count ls)) dicts) ls)))))

(defcheck solution-6cf83add
  (fn [n]
    (let [convert (fn [n x v i]
                    (cond
                      (zero? n) ""
                      (<= n 3) (apply str (replicate n i))
                      (= n 4) (apply str [i v])
                      (= n 5) (str v)
                      (<= n 8) (apply str v (replicate (- n 5) i))
                      (= n 9) (str i x)
                      (= n 10) x))
          thousands (apply str (replicate (quot n 1000) \M))
          hundreds  (convert (rem (quot n 100) 10) \M \D \C)
          tens      (convert (rem (quot n 10) 10) \C \L \X)
          digits    (convert (rem n 10) \X \V \I)]
      (str thousands hundreds tens digits))))

(defcheck solution-6d0a7eed
  (fn wn [n]
    (let [looper (fn looper [n syms]
                   (if (empty? syms)
                     (list)
                     (let [[sym divisor] (first syms)
                           [qt  remainr] ((juxt quot rem) n divisor)]
                       (cons (repeat qt sym) (looper remainr (rest syms))))))]
      (apply str (flatten (looper n (list [\M 1000] ['(\C \M) 900] [\D 500] ['(\C \D) 400] [\C 100] ['(\X \C) 90] [\L 50] ['(\X \L) 40] [\X 10] ['(\I \X) 9] [\V 5] ['(\I \V) 4] [\I 1])))))))

(defcheck solution-6d9e218f
  (fn R [n] (if (> n 0) (let [M {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"} m (apply max (filter #(>= n %) (keys M)))] (str (M m) (R (- n m)))))))

(defcheck solution-6dfb793f
  (fn w-r
    [n]
    (let [r {1 "I" 4 "IV" 5 "V" 9 "IX"
             10 "X" 40 "XL" 50 "L" 90 "XC"
             100 "C" 400 "CD" 500 "D" 900 "CM"
             1000 "M"}
          r-order [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          next-number
          (fn [x] (first (filter #(>= x %) r-order)))
          r-n
          (fn ![n]
            (cond
              (zero? n) '()
              :else (cons (r (next-number n)) (! (- n (next-number n))))))]
      (apply str (r-n n)))))

(defcheck solution-6e15eee2
  (fn write-roman-numerals [n]
    (let [numerals (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")
          num (some #(when (>= n %) %) (keys numerals))]
      (when-not (nil? num)
        (str (numerals num) (write-roman-numerals (- n num)))))))

(defcheck solution-6e19d8b7
  (fn f [v]
    (condp <= v
      1000 (str "M" (f (- v 1000)))
      900 (str "CM" (f (- v 900)))
      500 (str "D"  (f (- v 500)))
      400 (str "CD" (f (- v 400)))
      100 (str "C"  (f (- v 100)))
      90  (str "XC" (f (- v 90)))
      50  (str "L"  (f (- v 50)))
      40  (str "XL" (f (- v 40)))
      10  (str "X"  (f (- v 10)))
      9   (str "IX" (f (- v 9)))
      5   (str "V"  (f (- v 5)))
      4   (str "IV" (f (- v 4)))
      1   (str "I"  (f (dec v)))
      "")))

(defcheck solution-6e1f27ab
  (fn [v]
    (letfn [(rdig [d one five ten]
              (cond (< d 4) (repeat d one)
                    (= d 4) (concat [one] [five])
                    (= d 9) (concat [one] [ten])
                    :else (concat [five] (repeat (- d 5) one))))
            (place [v power] (quot (mod v (* 10 power)) power))]
      (apply str (concat (repeat (place v 1000) "M")
                   (rdig (place v 100) "C" "D" "M")
                   (rdig (place v 10) "X" "L" "C")
                   (rdig (place v 1) "I" "V" "X"))))
    ))

(defcheck solution-6e5d793d
  (fn [n]
    (let [roman (zipmap [1000 500 100 50 10 5 1] "MDCLXVI")
          number (reverse (map (comp read-string str) (str n)))]
      (loop [p number step 1 r []]
        (cond
          (empty? p) (apply str r)
          (= (first p) 0) (recur (rest p) (* step 10) r)
          (< (first p) 4) (recur (rest p) (* step 10) (concat (repeat (first p) (roman step)) r))
          (= (first p) 4) (recur (rest p) (* step 10) (concat [(roman step)] [(roman (* step 5))] r))
          (< (first p) 9) (recur (rest p) (* step 10) (concat [(roman (* step 5))] (repeat (- (first p) 5) (roman step)) r))
          (= (first p) 9) (recur (rest p) (* step 10) (concat [(roman step)] [(roman (* step 10))] r))
          )))))

(defcheck solution-6e6dbf0
  (fn [p] (letfn
           [(mp [a b c]
              (fn [n] (cond
                        (= n 0)        nil
                        (and (<= 1 n)
                             (<= n 3)) (apply str (repeat n a))
                        (= n 4) (str a b)
                        (= n 5) (str b)
                        (and (<= 6 n)
                             (<= n 8))
                        (str b (apply str (repeat (- n 5) a)))
                        (= n 9) (str a c)
                        )
                )
              )
            (nv [x] (map #(parse-int (str %))
                      (seq (str
                             (apply str (repeat (- 4 (count (str x)))
                                          0)
                               )
                             x))
                      )
              )

            ]
            (str ((mp "M" "" "") (first (nv p)))
              ((mp "C" "D" "M") (second (nv p)))
              ((mp "X" "L" "C") (nth (nv p) 2))
              ((mp "I" "V"  "X") (last (nv p))))

            )))

(defcheck solution-6e9bbe27
  (let [m {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
        s->i (reduce (fn [m [k v]]
                       (if (#{1 10 100 1000} v)
                         (->> (dissoc m k)
                           (filter (comp #(<= v % (* v 10))
                                     val))
                           (map (fn [[a b]] [(str k a) (- b v)]))
                           (into m))
                         m))
               m
               m)]
    (fn [n]
      (->> [nil n]
        (iterate (fn [[res i]]
                   (if (< 0 i)
                     (->> s->i
                       vals
                       (remove #(< i %))
                       (apply max)
                       (#(vector % (- i %)))))))
        rest
        (take-while #(-> %))
        (map first)
        (map (clojure.set/map-invert s->i))
        (apply str)))))

(defcheck solution-6eb18468
  (fn [n]
    (let [cd (fn [i n]
               (let [ls (drop (* 2 i) [\I \V \X \L \C \D \M \v \x])
                     r5 (rem  n 5)]
                 (clojure.string/join
                   ""
                   (cond
                     (= 4 n)    [(first ls) (second ls)]
                     (= 9 n)    [(first ls) (nth ls 2)]
                     :else      (reduce conj
                                  (if (< n 5) [] [(second ls)])
                                  (repeat r5 (first ls)))))))
          ds (->> n
               str
               seq
               reverse
               (map (comp read-string str))
               (map-indexed #(cd %1 %2))
               reverse)]
      (clojure.string/join "" ds))))

(defcheck solution-6eb3377c
  (fn [n]
    (let [table [[1000 "M"]
                 [900 "CM"]
                 [500 "D"]
                 [400 "XD"]
                 [100 "C"]
                 [90 "XC"]
                 [50 "L"]
                 [40 "XL"]
                 [10 "X"]
                 [9 "IX"]
                 [5 "V"]
                 [4 "IV"]
                 [1 "I"]]]
      (loop [n n
             acc ""]
        (let [p (first (filter (fn [[test-n _]]
                                 (<= test-n n))
                         table))]
          (if (empty? p)
            acc
            (let [[test-n s] p]
              (recur (- n test-n)
                (str acc s)))))))))

(defcheck solution-6f193330
  (fn [n]
    (let [th [nil "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM" "MMMMMMM" "MMMMMMMM" "MMMMMMMMM"]
          h [nil "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          te [nil "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          u [nil "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]]
      (str (nth th (quot n 1000))
        (nth h (quot (rem n 1000) 100))
        (nth te (quot (rem n 100) 10))
        (nth u (rem n 10))))))

(defcheck solution-6f334f35
  (fn [s]
    (apply str (loop [r []
                      n s
                      nums [ [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
                      c [1000 "M"]
                      p [0 ""]
                      ]
                 (cond
                   (= 0 n) r
                   (>= n (first c)) (recur (conj r (second c)) (- n (first c)) nums c p)
                   :else (recur r n (rest nums) (first nums) c)
                   )
                 ))
    ))

(defcheck solution-6f387e85
  (fn [n]
    (letfn [ (number-to-roman-numerals [n]
               (cond
                 (<= 1000 n) (cons "M" (number-to-roman-numerals (- n 1000)))
                 (<= 900 n) (cons "CM" (number-to-roman-numerals (- n 900)))
                 (<= 500 n) (cons "D" (number-to-roman-numerals (- n 500)))
                 (<= 400 n) (cons "CD" (number-to-roman-numerals (- n 400)))
                 (<= 100 n) (cons "C" (number-to-roman-numerals (- n 100)))
                 (<= 90 n) (cons "XC" (number-to-roman-numerals (- n 90)))
                 (<= 50 n) (cons "L" (number-to-roman-numerals (- n 50)))
                 (<= 40 n) (cons "XL" (number-to-roman-numerals (- n 40)))
                 (<= 10 n) (cons "X" (number-to-roman-numerals (- n 10)))
                 (<= 9 n) (cons "IX" (number-to-roman-numerals (- n 9)))
                 (<= 5 n) (cons "V" (number-to-roman-numerals (- n 5)))
                 (<= 4 n) (cons "IV" (number-to-roman-numerals (- n 4)))
                 (<= 1 n) (cons "I" (number-to-roman-numerals (- n 1))))) ]
      (apply str (number-to-roman-numerals n))
      )))

(defcheck solution-6f68f49c
  (fn write-roman-numberals [num]
    (letfn [(decompose [num]
              (->> [1000 900 500 400 100 90 50 40 10 9 5 4 1]
                (reduce (fn [[ret tuple] x]
                          [(mod ret x)
                           (let [cnt (int (/ ret x))]
                             (if (> cnt 0)
                               (case x
                                 900 (conj tuple [1 100] [1 1000])
                                 400 (conj tuple [1 100] [1 500])
                                 90 (conj tuple [1 10] [1 100])
                                 40 (conj tuple [1 10] [1 50])
                                 9 (conj tuple [1 1] [1 10])
                                 4 (conj tuple [1 1] [1 5])
                                 (conj tuple [cnt x]))
                               tuple))]) [num []])
                second
                (map #(repeat (first %) (second %)))
                (apply concat)))]

      (let [roman-numberals {1 \I
                             5 \V
                             10 \X
                             50 \L
                             100 \C
                             500 \D
                             1000 \M}]

        (apply str (map roman-numberals (decompose num)))))))

(defcheck solution-700d265c
  (fn to-roman [n]
    (if (= n 0) ""
                (let [rs [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
                          [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
                      r (first (filter #(<= (first %) n) rs))]
                  (str (second r) (to-roman (- n (first r))))))))

(defcheck solution-705eca0a
  (fn write-roman [x]
    (let [nmap (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (if (zero? x) ""
                    (let [match (first (filter #(<= % x) (reverse (keys nmap))))
                          remainder (- x match)]
                      (str (nmap match) (write-roman remainder)))))))

(defcheck solution-70e2df45
  (let [roman-list
        [[1000 "M"]
         [900 "CM"]
         [500 "D"]
         [400 "CD"]
         [100 "C"]
         [90 "XC"]
         [50 "L"]
         [40 "XL"]
         [10 "X"]
         [9 "IX"]
         [5 "V"]
         [4 "IV"]
         [1 "I"]]]
    (fn write-roman
      [n]
      (clojure.string/join
        (loop [curr n
               accum []
               rlist roman-list]
          (if (empty? rlist) accum
                             (let [[rn rs] (first rlist)]
                               (if (>= curr rn)
                                 (recur (- curr rn) (conj accum rs) rlist)
                                 (recur curr accum (rest rlist))))))))))

(defcheck solution-7101cceb
  (fn roman-str [n]
    (apply str
      (loop [a n,
             r []]
        (cond (>= a 1000) (recur (- a 1000) (conj r 'M))
              (>= a 900)  (recur (- a 900) (conj (conj r 'C) 'M))
              (>= a 500)  (recur (- a 500) (conj r 'D))
              (>= a 400)  (recur (- a 400) (conj (conj r 'C) 'D))
              (>= a 100)  (recur (- a 100) (conj r 'C))
              (>= a 90)   (recur (- a 90) (conj (conj r 'X) 'C))
              (>= a 50)   (recur (- a 50) (conj r 'L))
              (>= a 40)   (recur (- a 40) (conj (conj r 'X) 'L))
              (>= a 10)   (recur (- a 10) (conj r 'X))
              (>= a 9)    (recur (- a 9) (conj (conj r 'I) 'X))
              (>= a 5)    (recur (- a 5) (conj r 'V))
              (>= a 4)    (recur (- a 4) (conj (conj r 'I) 'V))
              (>= a 1)    (recur (- a 1) (conj r 'I))
              :else r)))))

(defcheck solution-7115edfe
  (fn [n]
    (let [t [[1000 "M"] [900 "CM"]
             [500 "D"] [400 "CD"]
             [100 "C"] [90 "XC"]
             [50 "L"] [40 "XL"]
             [10 "X"] [9 "IX"]
             [5 "V"] [4 "IV"]
             [1 "I"]]
          lookup-largest (fn [n] (some #(if (<= (first %) n) % nil) t))]
      (loop [n n, acc ""]
        (if-let [e (lookup-largest n)]
          (recur (- n (first e)) (str acc (second e)))
          acc)))))

(defcheck solution-71899e6f
  (fn gen-rn [n] (let [num-vals {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "CD" 400 "D" 500 "CM" 900 "M" 1000}
                       numerals ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]]
                   (loop [remainder n result ""]
                     (if (= remainder 0)
                       result
                       (let [best (first (second (split-with #(> (num-vals %) remainder) numerals)))]
                         (recur (- remainder (num-vals best)) (str result best))))))))

(defcheck solution-71ad14e6
  (fn [i] (letfn [(conv [msg n] (if (zero? n) msg
                                              (cond (<= 1000 n) (conv (str msg "M") (- n 1000))
                                                    (<= 900 n) (conv (str msg "CM") (- n 900))
                                                    (<= 800 n) (conv (str msg "DCCC") (- n 800))
                                                    (<= 700 n) (conv (str msg "DCC") (- n 700))
                                                    (<= 600 n) (conv (str msg "DC") (- n 600))
                                                    (<= 500 n) (conv (str msg "D") (- n 500))
                                                    (<= 400 n) (conv (str msg "CD") (- n 400))
                                                    (<= 100 n) (conv (str msg "C") (- n 100))
                                                    (<= 90 n) (conv (str msg "XC") (- n 90))
                                                    (<= 80 n) (conv (str msg "LXXX") (- n 80))
                                                    (<= 70 n) (conv (str msg "LXX") (- n 70))
                                                    (<= 60 n) (conv (str msg "LX") (- n 60))
                                                    (<= 50 n) (conv (str msg "L") (- n 50))
                                                    (<= 40 n) (conv (str msg "XL") (- n 40))
                                                    (<= 10 n) (conv (str msg "X") (- n 10))
                                                    (<= 9 n) (conv (str msg "IX") (- n 9))
                                                    (<= 8 n) (conv (str msg "VIII") (- n 8))
                                                    (<= 7 n) (conv (str msg "VII") (- n 7))
                                                    (<= 6 n) (conv (str msg "VI") (- n 6))
                                                    (<= 5 n) (conv (str msg "V") (- n 5))
                                                    (<= 4 n) (conv (str msg "IV") (- n 4))
                                                    (<= 1 n) (conv (str msg "I") (- n 1)) :else "?")))]
            (conv "" i))))

(defcheck solution-71cd8a8c
  (fn [n]
    (letfn [(x10 [n] (apply str (map #({\I \X \V \L \X \C \L \D \C \M} %) (seq n))))
            (xn  [n x] (last (take x (iterate x10 n))))
            (sep [n] (loop [n n acc ()]
                       (if (< n 10) (conj acc n)
                                    (recur (quot n 10) (conj acc (mod n 10))))))]
      (apply str (reverse (map-indexed
                            #(xn ({1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"} %2) (inc %1)) (reverse (sep n))))))))

(defcheck solution-71d40d24
  (fn [z]
    (let [m (array-map 1000 \M,
              900 "CM", 500 \D, 400 "CD", 100 \C,
              90 "XC",  50 \L,  40 "XL",  10 \X,
              9 "IX",   5 \V,   4 "IV",   1 \I)]
      (loop [x z, r ""]
        (let [ch (get m x), xs (filter #(<= %1 x) (keys m)), fxs (first xs)]
          (cond
            (not (nil? ch)) (str r ch)
            (not (nil? fxs)) (recur (- x fxs) (str r (get m fxs)))
            :else r
            ))))))

(defcheck solution-71f268d8
  (fn [n]
    (let [v [["M" 1000]
             ["CM" 900]
             ["D"  500]
             ["CD" 400]
             ["C"  100]
             ["XC"  90]
             ["L"   50]
             ["XL"  40]
             ["X"   10]
             ["IX"   9]
             ["V"    5]
             ["IV"   4]
             ["I"    1]]]
      (loop [r "" c n]
        (if (> c 0)
          (let [[k v] (first (filter #(if (>= c (second %)) %) v))]
            (recur (str r k) (- c v)))
          r)))))

(defcheck solution-7208d622
  (fn [n] (reduce #(apply (partial clojure.string/replace %) %2)
            (loop [k n s [] l [1000 500 100 50 10 5 1]
                   w [\M \D \C \L \X \V \I]] (if (zero? k) (apply str s)
                                                           (recur (rem k (first l)) (concat s
                                                                                      (repeat (quot k (first l)) (first w))) (rest l) (rest w))))
            [["DCCCC" "CM"] ["CCCC" "CD"] ["LXXXX" "XC"]
             ["XXXX" "XL"] ["VIIII" "IX"] ["IIII" "IV"]])))

(defcheck solution-72876b96
  (fn real-roman [a]
    (letfn [(roman [x]
              (letfn [(times [y letter] (vec (repeat (quot x y) letter)))
                      (use-times [y letter] (concat (times y letter) (roman (mod x y))))
                      (use-once [y letters] (concat letters (roman (- x y))))]
                (cond
                  (>= x 1000) (use-times 1000 \M)
                  (>= x 900) (use-once 900 [\C \M])
                  (>= x 500) (use-once 500 [\D])
                  (>= x 400) (use-once 400 [\C \D])
                  (>= x 100) (use-times 100 \C)
                  (>= x 90) (use-once 90 [\X \C])
                  (>= x 50) (use-once 50 [\L])
                  (>= x 40) (use-once 40 [\X \L])
                  (>= x 10) (use-times 10 \X)
                  (>= x 9) (use-once 9 [\I \X])
                  (>= x 5) (use-once 5 [\V])
                  (>= x 4) (use-once 4 [\I \V])
                  (>= x 1) (use-times 1 \I)
                  :else [])))]
      (clojure.string/join (roman a)))
    ))

(defcheck solution-728aa127
  (fn roman-numerals [j]
    (let [numeral-map (sorted-map-by >
                        1000 "M" 900 "CM"
                        500 "D" 400 "CD"
                        100 "C" 90 "XC"
                        50 "L" 40 "XL"
                        10 "X" 9 "IX"
                        5 "V" 4 "IV"
                        1 "I")]
      (loop [i       j
             numeral ""]
        (if (zero? i)
          numeral
          (let [[[n letters]] (filter (comp (partial >= i) first) numeral-map)]
            (recur (- i n) (str numeral letters))))))))

(defcheck solution-730af32c
  (fn [n]
    (str
      ({1 "M" 2 "MM" 3 "MMM" 4 "MMMM" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"} (mod (quot n 1000) 10))
      ({1 "C" 2 "CC" 3 "CCC" 4 "CD" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"} (mod (quot n 100) 10))
      ({1 "X" 2 "XX" 3 "XXX" 4 "XL" 5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"} (mod (quot n 10) 10))
      ({1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"} (mod n 10)))))

(defcheck solution-7390ac9f
  (fn n2r [x]
    (let [t
            {0 {0 ""}
             1 {1000 "M" 100 "C" 10 "X" 1 "I"}
             2 {1000 "MM" 100 "CC" 10 "XX" 1 "II"}
             3 {1000 "MMM" 100 "CCC" 10 "XXX" 1 "III"}
             4 {1000 "MMMM" 100 "CD" 10 "XL" 1 "IV"}
             5 {10 "L" 100 "D" 1 "V"}
             6 {10 "LX" 100 "DC" 1 "VI"}
             7 {10 "LXX" 100 "DCC" 1 "VII"}
             8 {10 "LXXX" 100 "DCCC" 1 "VIII"}
             9 {10 "XC" 100 "CM" 1 "IX"}
             }
          a [(quot x 1000) 1000]
          b [(quot (mod x 1000) 100) 100]
          c [(quot (mod x 100) 10) 10]
          d [(mod x 10) 1]
          ]
      (str (get-in t a)
        (get-in t b)
        (get-in t c)
        (get-in t d))
      )
    ))

(defcheck solution-74c275db
  (fn roman [n]
    (let [romans {1    "I"
                  4    "IV"
                  5    "V"
                  9    "IX"
                  10   "X"
                  40   "XL"
                  50   "L"
                  90   "XC"
                  100  "C"
                  400  "CD"
                  500  "D"
                  900  "CM"
                  1000 "M"}
          nums (sort > (keys romans))]
      (loop [curr n roman []]
        (if-let [closest (first (drop-while #(> % curr) nums))]
          (recur (- curr closest) (conj roman (romans closest)))
          (apply str roman))))))

(defcheck solution-75590353
  (fn [num]
    (letfn [(render [n one five ten]
              (condp = n
                0 ""
                1 one
                4 (str one five)
                5 five
                9 (str one ten)
                (str (render (dec n) one five ten) (render 1 one five ten))))]

      (let [digits (mapv #(mod (quot num %) 10) [1000 100 10 1])]

        (reduce str (mapv #(apply render %1 %2) digits [[\M \? \?]
                                                        [\C \D \M]
                                                        [\X \L \C]
                                                        [\I \V \X]]))))))

(defcheck solution-7585fe5d
  (fn [xs]
    (loop [xs xs res ""]
      (if (<= xs 0)
        res
        (cond
          (>= xs 1000) (recur (- xs 1000) (str res "M"))
          (>= xs 900) (recur (- xs 900) (str res "CM"))
          (>= xs 500) (recur (- xs 500) (str res "D"))
          (>= xs 400) (recur (- xs 400) (str res "CD"))
          (>= xs 100) (recur (- xs 100) (str res "C"))
          (>= xs 90) (recur (- xs 90) (str res "XC"))
          (>= xs 50) (recur (- xs 50) (str res "L"))
          (>= xs 40) (recur (- xs 40) (str res "XL"))
          (>= xs 10) (recur (- xs 10) (str res "X"))
          (>= xs 9) (recur (- xs 9) (str res "IX"))
          (>= xs 5) (recur (- xs 5) (str res "V"))
          (>= xs 4) (recur (- xs 4) (str res "IV"))
          (>= xs 1) (recur (- xs 1) (str res "I")))))))

(defcheck solution-76287c8a
  (fn [number]
    (let [conv-table (sorted-map 1 :I
                       4 :IV
                       5 :V
                       9 :IX
                       10 :X
                       40 :XL
                       50 :L
                       90 :XC
                       100 :C
                       400 :CD
                       500 :D
                       900 :CM
                       1000 :M)]
      (loop [n number t (reverse conv-table) result []]
        (if (seq t)
          (let [current (first t)
                total (first current)
                roman (quot n total)]
            (recur (rem n total) (rest t) (if (> roman 0)
                                            (concat result (repeat roman (second current)))
                                            result)))
          (clojure.string/join (map name result)))))))

(defcheck solution-76f9cf70
  (fn [a] (let [abc { 1 ["I" "X" "C" "M"] 2 ["II" "XX" "CC" "MM"] 3 ["III"
                                                                     "XXX" "CCC" "MMM"] 4 ["IV" "XL" "CD"] 5 ["V" "L" "D"] 6 ["VI" "LX" "DC"] 7
                     ["VII" "LXX" "DCC"] 8 ["VIII" "LXXX" "DCCC"] 9 ["IX" "XC" "CM"] 0 ["" "" ""
                                                                                        ""] } digits (reverse (map read-string (re-seq #"." (str a))))] (->> digits
                                                                                                                                                          (map-indexed (fn [i d] (get-in abc [d i]))) (reverse) (clojure.string/join
                                                                                                                                                                                                                  "")))))

(defcheck solution-771ddb6c
  (fn roman
    [n]
    (letfn [(triangle [c n]
              (map #(clojure.string/join (take % (repeat c))) (range n)))
            (ht [c d l]
              (concat (triangle c 4)
                [(str c d)]
                (map #(str d %) (triangle c 4))
                [(str c l)]))]
      (let [m {3 (triangle "M" 10)
               2 (ht "C" "D" "M")
               1 (ht "X" "L" "C")
               0 (ht "I" "V" "X")}
            s (map-indexed vector (reverse (str n)))]
        (clojure.string/join (reverse (map (fn [[i v]]
                                             (nth (m i) (parse-int (str v)))) s)))
        ))))

(defcheck solution-77edf45f
  (fn [arabic]
    (let
     [conv
               (fn [one five ten]
                 #(apply
                    str
                    (case
                     %
                      \0 ""
                      \1 one
                      \2 (repeat 2 one)
                      \3 (repeat 3 one)
                      \4 (concat one five)
                      \5 five
                      \6 (concat five one)
                      \7 (concat five one one)
                      \8 (concat five one one one)
                      \9 (concat one ten))))
      units (conv "I" "V" "X")
      tens (conv "X" "L" "C")
      hundreds (conv "C" "D" "M")
      thousands
               #(apply
                  str
                  (repeat
                    (parse-int (str %))
                    "M"))
      ]
      (apply
        str
        (reverse
          (map
            (fn [r a]
              (r a))
            [units tens hundreds thousands]
            (reverse (str arabic))))))))

(defcheck solution-790fd7a9
  (fn n104 [n]
    (let [d [[1000 \M] [900 "CM"] [500 \D] [400 "CD"] [100 \C] [90 "XC"] [50 \L] [40 "XL"] [10 \X] [9 "IX"] [5 \V] [4 "IV"] [1 \I]]]
      (apply str (loop [k n a [] p (first (drop-while #(< k (first %)) d))]
                   (if (zero? k) a
                                 (recur
                                   (- k (first p))
                                   (conj a (second p))
                                   (first (drop-while #(< (- k (first p)) (first %)) d)))))))))

(defcheck solution-7a1c9095
  (fn [n]
    (let [v [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
             [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
             [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [[n s] [n ""]]
        (if (zero? n) s
                      (recur (first (for [[k t] v :when (<= k n)] [(- n k) (str s t)]))))))))

(defcheck solution-7aa673be
  (fn gen-roman [n]
    (apply str
      (second
        (reduce
          (fn [[total v] [t-str t-val]]
            (let [append-n (quot total t-val)]
              [(- total
                 (* append-n t-val))
               (into v
                 (repeat append-n t-str))]))
          [n []]
          (sort-by second > {"I" 1
                             "IV" 4
                             "V" 5
                             "IX" 9
                             "X" 10
                             "XL" 40
                             "L" 50
                             "XC" 90
                             "C" 100
                             "CD" 400
                             "D" 500
                             "CM" 900
                             "M" 1000}))))))

(defcheck solution-7addaec0
  (comp (partial apply str)
    (fn f
      [n]
      (if (zero? n)
        nil
        (let [n->s {1000 "M"  900 "CM" 500 "D"
                    400 "CD" 100 "C"   90 "XC"
                    50 "L"   40 "XL"  10 "X"
                    9 "IX"   4 "IV"   5 "V"
                    1 "I"}
              ncol (reverse (sort (keys n->s)))
              nn (first (filter #(<= % n) ncol))]
          #_(println n nn)
          (lazy-seq (cons (n->s nn) (f (- n nn)))))))))

(defcheck solution-7c063c10
  (fn [x]
    (let [n [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          l '[M CM D CD C XC L XL X IX V IV I]]
      (loop [x x
             r ""]
        (if (pos? x)
          (let [v (some #(if (<= % x) %) n)]
            (recur (- x v)
              (str r ((zipmap n l) v))))
          r)))))

(defcheck solution-7c1a4800
  (fn roman [rn]
    (let [expanded ((fn exp [n] (apply str (if (= n 0) ""
                                                       (let [nums (sorted-map-by > 1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I")
                                                             largest (first (drop-while #(< n %) (keys nums)))
                                                             quotient (quot n largest)]
                                                         (concat (repeat quotient (nums largest))
                                                           (exp (- n (* largest quotient)))))))) rn)]
      (-> expanded (clojure.string/replace #"DCCCC" "CM")
        (clojure.string/replace #"CCCC"  "CD")
        (clojure.string/replace #"LXXXX" "XC")
        (clojure.string/replace #"XXXX"  "XL")
        (clojure.string/replace #"VIIII" "IX")
        (clojure.string/replace #"IIII"  "IV")))))

(defcheck solution-7d4e743
  (fn to-roman
    ([n] (to-roman n ""))
    ([n s] (cond
             (>= n 1000) (recur (- n 1000) (str s "M"))
             (>= n 900) (recur (- n 900) (str s "CM"))
             (>= n 500) (recur (- n 500) (str s "D"))
             (>= n 400) (recur (- n 400) (str s "CD"))
             (>= n 100) (recur (- n 100) (str s "C"))
             (>= n 90) (recur (- n 90) (str s "XC"))
             (>= n 50) (recur (- n 50) (str s "L"))
             (>= n 40) (recur (- n 40) (str s "XL"))
             (>= n 10) (recur (- n 10) (str s "X"))
             (>= n 9) (recur (- n 9) (str s "IX"))
             (>= n 5) (recur (- n 5) (str s "V"))
             (>= n 4) (recur (- n 4) (str s "IV"))
             (>= n 1) (recur (- n 1) (str s "I"))
             true s
             ))))

(defcheck solution-7e34e018
  (fn [n]
    (let [roman [[1000 "M"] [900 "CM"] [500 "D"]
                 [400 "CD"] [100 "C"] [90 "XC"]
                 [50 "L"] [40 "XL"] [10 "X"]
                 [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (apply str (first (reduce (fn [[res x] [d s]]
                                  [(concat res (repeat (quot x d) s)) (rem x d)])
                          [() n] roman))))))

(defcheck solution-7ee1c828
  (fn f [x]
    (let [chs [[1000 "M"]
               [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
               [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
               [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [x x
             ret ""]
        (if (= x 0)
          ret
          (let [[k v] (first (filter #(>= x (first %)) chs))]
            (recur (- x k)
              (str ret v))))))))

(defcheck solution-7f096010
  (let [divrem (fn [n d] [(quot n d) (rem n d)])
        digits (fn digits [n]
                 (loop [n n, ds nil]
                   (let [[q r] (divrem n 10), ds (cons r ds)]
                     (if (zero? q) ds (recur q ds)))))
        romchars (partition 3 2 [nil nil \M \D \C \L \X \V \I])
        xlat-digit (fn xlat-digit [dig [ten five one]]
                     (let [[q r] (divrem dig 5), zq (zero? q)]
                       (if (= 4 r)
                         [one (if zq five ten)]
                         (concat (if zq [] [five]) (repeat r one)))))
        pad-front (fn [ds]
                    (let [n (- 4 (count ds))]
                      (if (pos? n) (concat (repeat n 0) ds) ds)))]
    #(apply str (mapcat xlat-digit (pad-front (digits %)) romchars))))

(defcheck solution-7f48cfdb
  (fn roman-number [n]

    (let [

          digits
          (fn digits [x]
            (map #(parse-int (str %)) (reverse (str x))))

          romans
          [{0 ""  1 "I"  2 "II"  3 "III"  4 "IV"  5 "V"  6 "VI"  7 "VII"  8 "VIII"  9 "IX"}
           {0 ""  1 "X"  2 "XX"  3 "XXX"  4 "XL"  5 "L"  6 "LX"  7 "LXX"  8 "LXXX"  9 "XC"}
           {0 ""  1 "C"  2 "CC"  3 "CCC"  4 "CD"  5 "D"  6 "DC"  7 "DCC"  8 "DCCC"  9 "CM"}
           {0 ""  1 "M"  2 "MM"  3 "MMM"}]]

      (apply str (reverse (map #(%1 %2) romans (digits n)))))))

(defcheck solution-7f7ccdd2
  (fn r-numerals
    [n]
    (let [m {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          v (reverse [1 4 5 9 10 40 50 90 100 400 500 900 1000])]
      (if (= 0 n) nil
                  (let [nxt (some #(when (>= n %) %) v)]
                    (apply str (m nxt) (r-numerals (- n nxt))))))))

(defcheck solution-7fa927a8
  (fn f [n]
    (if (> n 0)
      (let [ir (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD"
                 500 "D" 900 "CM" 1000 "M")
            m (last (filter #(<= % n) (keys ir)))]
        (str (ir m) (f (- n m)))))))

(defcheck solution-805d2c0d
  (letfn
   [(digits [n] (map #(parse-int (str %)) (str n)))
    (toRoman
      ([coll] (toRoman coll (count coll)))
      ([[head & tail] n]
       (if (zero? n)
         nil
         ; M D C L X V I
         (let [fl {4 \!, 3 \D, 2 \L, 1 \V}
               l  {4 \M, 3 \C, 2 \X, 1 \I}]
           (cons (cond
                   (< head 4) (repeat head (l n))
                   (= head 4) [(l n) (fl n)]
                   (= head 5) [(fl n)]
                   (< head 9) (concat [(fl n)] (repeat (- head 5) (l n)))
                   (= head 9) [(l n) (l (inc n))]
                   )
             (lazy-seq (toRoman tail (dec n)))))
         )))]
    (fn [n]
      (-> n
        digits
        toRoman
        (#(apply concat %))
        clojure.string/join
        )
      )))

(defcheck solution-80b2671b
  (fn wr [n]
    (let [c [ [] [:u] [:u :u] [:u :u :u] [:u :q] [:q] [:q :u] [:q :u :u] [:q :u :u :u] [:u :d]]
          mm [ {:u \I :q \V :d \X} {:u \X :q \L :d \C} {:u \C :q \D :d \M} {:u \M :q \Y :d \Z}]
          rec-wr (fn rwr [n mi r]
                   (if (= 0 n) r
                               (rwr (quot n 10) (inc mi) (conj r (apply str (map (mm mi) (c (mod n 10))))))))
          ]
      (apply str (rec-wr n 0 '()))
      )))

(defcheck solution-81315fc9
  (fn roman [n]
    (condp <= n
      1000 (str "M" (roman (- n 1000)))
      900 (str "CM" (roman (- n 900)))
      500 (str "D" (roman (- n 500)))
      400 (str "CD" (roman (- n 400)))
      100 (str "C" (roman (- n 100)))
      90 (str "XC" (roman (- n 90)))
      50 (str "L" (roman (- n 50)))
      40 (str "XL" (roman (- n 40)))
      10 (str "X" (roman (- n 10)))
      9 (str "IX" (roman (- n 9)))
      5 (str "V" (roman (- n 5)))
      4 (str "IV" (roman (- n 4)))
      1 (str "I" (roman (- n 1)))
      ""
      )))

(defcheck solution-814d9266
  (fn [x]
    (let [l {0 {\0 "" \1 "I" \2 "II" \3 "III" \4 "IV" \5 "V" \6 "VI" \7 "VII" \8 "VIII" \9 "IX"}
             1 {\0 "" \1 "X" \2 "XX" \3 "XXX" \4 "XL" \5 "L" \6 "LX" \7 "LXX" \8 "LXXX" \9 "XC"}
             2 {\0 "" \1 "C" \2 "CC" \3 "CCC" \4 "CD" \5 "D" \6 "DC" \7 "DCC" \8 "DCCC" \9 "CM"}
             3 {\0 "" \1 "M" \2 "MM" \3 "MMM"}}]
      (apply str (reverse (map-indexed #((l %) %2) (reverse (str x))))))))

(defcheck solution-8151fbca
  (fn [n]
    (let [S '(\M   \D  \C  \L \X \V \I)
          N '(1000 500 100 50 10 5  1)
          R (loop [n n N N S S R []]
              (if (zero? n)
                (apply str R)
                (if (>= n (first N))
                  (recur (- n (first N)) N S (conj R (first S)))
                  (recur n (next N) (next S) R))))
          r clojure.string/replace]
      (-> R
        (r "VIIII" "IX") (r "IIII" "IV")
        (r "LXXXX" "XC") (r "XXXX" "XL")
        (r "DCCCC" "CM") (r "CCCC" "CD")))))

(defcheck solution-81596d76
  (fn num-roman [n]
    (let [values-map {1 [\I], 4 [\I \V], 5 [\V], 9 [\I \X],
                      10 [\X], 40 [\X \L], 50 [\L], 90 [\X \C]
                      100 [\C], 400 [\C \D] 500 [\D], 900 [\C \M] 1000 [\M] }
          values (sort-by #(- %) (keys values-map))
          mid-result (reduce (fn [[n num-vec] v]
                               [(rem n v) (conj num-vec (quot n v))]) [n []] values)]
      (apply str (apply concat (map #(apply concat
                                       (for [i (range %2)] (get values-map %1)))
                                 values (last mid-result)))))))

(defcheck solution-81fccb3f
  (fn [n]
    (let [
          UNITS {
                 0 "", 1 "I", 2 "II", 3 "III", 4 "IV", 5 "V", 6 "VI", 7 "VII", 8 "VIII", 9 "IX"
                 },
          TENS {
                0 "", 1 "X", 2 "XX", 3 "XXX", 4 "XL", 5 "L", 6 "LX", 7 "LXX", 8 "LXXX", 9 "XC"
                },
          HUNDREDS {
                    0 "", 1 "C", 2 "CC", 3 "CCC", 4 "CD", 5 "D", 6 "DC", 7 "DCC", 8 "DCCC", 9 "CM"
                    },
          THOUSANDS {
                     0 "", 1 "M", 2 "MM", 3 "MMM", 4 "MMMM", 5 "MMMMM", 6 "MMMMMM", 7 "MMMMMMM", 8 "MMMMMMMM", 9 "MMMMMMMMM"
                     },
          th (quot n 1000),
          hd (mod (quot n 100) 10),
          tn (mod (quot n 10) 10),
          ut (mod n 10)
          ]
      (str (get THOUSANDS th) (get HUNDREDS hd) (get TENS tn) (get UNITS ut))
      )
    ))

(defcheck solution-82916751
  (fn number104 [n]
    (reduce
      (fn [m [k v]] (clojure.string/replace m (apply str (repeat k "I")) v))
      (apply str (repeat n "I"))
      (array-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"))))

(defcheck solution-82edae51
  (fn [n]
    (apply
      str
      (map #(nth %2 %1)
        (reverse (map #(mod (quot n %) 10) (take 4 (iterate #(* 10 %) 1))))
        [(take 10 (iterate #(str % "M") ""))
         ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
         ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
         ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
         ]))))

(defcheck solution-83e37bba
  (fn wr [x]
    (cond (>= x 1000) (str "M" (wr (- x 1000)))
          (>= x 900) (str "CM" (wr (- x 900)))
          (>= x 500) (str "D" (wr (- x 500)))
          (>= x 400) (str "CD" (wr (- x 400)))
          (>= x 100) (str "C" (wr (- x 100)))
          (>= x 90) (str "XC" (wr (- x 90)))
          (>= x 50) (str "L" (wr (- x 50)))
          (>= x 40) (str "XL" (wr (- x 40)))
          (>= x 10) (str "X" (wr (- x 10)))
          (>= x 9) (str "IX" (wr (- x 9)))
          (>= x 5) (str "V" (wr (- x 5)))
          (>= x 4) (str "IV" (wr (- x 4)))
          (>= x 1) (str "I" (wr (- x 1))))))

(defcheck solution-83e8b3f2
  (fn i->roman [n]
    (->
      n
      (repeat \I)
      (->> (apply str))
      (clojure.string/replace #"IIIII" "V")
      (clojure.string/replace #"IIII" "IV")
      (clojure.string/replace #"VV" "X")
      (clojure.string/replace #"VIV" "IX")
      (clojure.string/replace #"XXXXX" "L")
      (clojure.string/replace #"XXXX" "XL")
      (clojure.string/replace #"LL" "C")
      (clojure.string/replace #"LXL" "XC")
      (clojure.string/replace #"CCCCC" "D")
      (clojure.string/replace #"CCCC" "CD")
      (clojure.string/replace #"DD" "M")
      (clojure.string/replace #"DCD" "CM"))))

(defcheck solution-840eb2d1
  (fn [n]
    (loop [n n
           digit (rem n 10)
           lookup '(["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                    ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                    ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                    ["" "M" "MM" "MMM" "MMMM"])
           numerals ()]
      (if (and (zero? n) (zero? digit))
        (clojure.string/join "" numerals)
        (recur (quot n 10)
          (rem (quot n 10) 10)
          (rest lookup)
          (conj numerals ((first lookup) digit)))))))

(defcheck solution-84d2bbd0
  (let [roman-values [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
    (fn translate [n]
      (if (zero? n)
        ""
        (let [[sym value] (some (fn [[sym value]] (when (<= value n) [sym value])) roman-values)]
          (str sym (translate (- n value))))))))

(defcheck solution-85446c03
  (fn to-roman [n]
    (let [integers [1000 500 100 50 10 5 1]
          int-to-roman {1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"}
          possible-subtractions #{100 10 1}
          check-for-sub (fn [a b]
                          (if (> b 0)
                            (let [max-sub (reduce max  (filter #(<= % b) possible-subtractions))
                                  adj-b (* max-sub (quot b max-sub))
                                  c (- a adj-b)
                                  sub-exists? (and (contains? possible-subtractions c)
                                                   (<= (/ 1 10) (/ adj-b a)))]
                              (if sub-exists?
                                [adj-b (concat (get int-to-roman c) (get int-to-roman a))]
                                false))
                            nil))]

      (loop [n2 n
             radii integers;(filter #(<= % n2) integers)
             acc ""]
        (let [radii (filter #(<= % n2) integers)
              possible-sub (last (filter #(> % n2) integers))
              has-sub (if possible-sub (check-for-sub possible-sub n2) false)]
          (if (= n2 0)
            (apply str acc)
            (let [n2 (- n2 (if has-sub (first has-sub) (first radii)))]
              (recur n2
                (filter #(<= % n2) radii)
                (concat acc (if has-sub (second has-sub)
                                        (get int-to-roman (first radii))))))))))))

(defcheck solution-867a24ce
  (fn convert [n]
    (letfn [(value [n nVal rStr acc]
              (if (>= n nVal)
                (recur (- n nVal) nVal rStr (str acc rStr))
                [n acc]))]
      (let [mapper [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
                    [90 "XC"]  [50 "L"]   [40 "XL"] [10 "X"]
                    [9 "IX"]   [5 "V"]    [4 "IV"]  [1 "I"]]]
        (second (reduce (fn [result [val roman]]
                          (let [remainder (first result)
                                resStr (second result)
                                [newRem newRes] (value remainder val roman "")]
                            [newRem (str resStr newRes)]))
                  [n ""]
                  mapper))))))

(defcheck solution-868c6ac
  (fn
    [n]
    (let [m (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")]
      (loop [r [] cn n t 1000 cm m]
        (if (>= cn t)
          (recur (conj r (m t)) (- cn t) t cm)
          (if (= cn 0)
            (apply str r)
            (recur r cn ((comp first keys rest) cm) (rest cm) )))
        )

      )))

(defcheck solution-86a6ef47
  (fn roman-numeral [n]
    (let [lookup [[1000 "M"]
                  [900 "CM"]
                  [500 "D"]
                  [400 "CD"]
                  [100 "C"]
                  [90 "XC"]
                  [50 "L"]
                  [40 "XL"]
                  [10 "X"]
                  [9 "IX"]
                  [5 "V"]
                  [4 "IV"]
                  [1 "I"]]]
      (if (pos? n)
        (let [[a b] (first (filter #(>= n  (first %)) lookup))]
          (str
            (apply str (repeat (quot n a) b))
            (roman-numeral (mod n a))))))))

(defcheck solution-86c75a25
  (fn roman-numeral [n]
    (let [table [1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I]]
      (loop [n n ret [] table table]
        (let [[x0 r0 x1 r1 x2 r2] table]
          (cond (<= n 0) (apply str ret)

                (>= n x0)
                (recur (- n x0) (conj ret r0) table)

                (or (and (= r0 \L) (>= n (- x0 x1)))
                    (and (= r0 \V) (>= n (- x0 x1))))
                (recur (- n (- x0 x1)) (conj ret r1 r0) (subvec table 2))


                (and (not (nil? x2))
                     (>= n (- x0 x2)))
                (recur (- n (- x0 x2)) (conj ret r2 r0) (subvec table 2))

                :else
                (recur n ret (subvec table 2))))))))

(defcheck solution-87476ea7
  (fn [n]
    (let [dec->roman (sorted-map-by > 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                       40 "XL" 50 "L" 90 "XC" 100 "C" 400
                       "CD" 500 "D" 900 "CM" 1000 "M")]
      (loop [nums [] rem n]
        (if (zero? rem)
          (apply str (map dec->roman nums))
          (let [u (ffirst (subseq dec->roman >= rem))]
            (recur (conj nums u) (- rem u))))))))

(defcheck solution-87b1739b
  (fn write-roman [n]
    (if (zero? n) ""
                  (let [[roman value]
                        (first
                          (drop-while
                            (fn [[_ value]]
                              (> value n))
                            [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
                             ["C" 100]  ["XC" 90]  ["L" 50]  ["XL" 40]
                             ["X" 10]   ["IX" 9]   ["V" 5]   ["IV" 4]
                             ["I" 1]]))]
                    (str roman (write-roman (- n value)))))))

(defcheck solution-88964cfe
  (fn dec->roman [x]
    (let [decs [1000 900  500 400  100 90   50  40   10  9    5   4    1]
          roms ["M"  "CM" "D" "DC" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          dict (zipmap decs roms)
          freqs (second (reduce (fn [[rm acc] v] [(mod rm v) (assoc acc v (quot rm v))]) [x {}] decs))]
      (apply str (mapcat #(repeat (freqs %) (dict %)) decs)))))

(defcheck solution-88b924aa
  (fn [n] (loop [out [] r n m [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
            (if (zero? r) (apply str out) (let [[a b] (first m) x (quot r a)] (recur (concat out (repeat x b)) (rem r a) (rest m))))
            )
    ))

(defcheck solution-88c7e7a5
  (let [m [[1 "I"]
           [4 "IV"]
           [5 "V"]
           [9 "IX"]
           [10 "X"]
           [40 "XL"]
           [50 "L"]
           [90 "XC"]
           [100 "C"]
           [500 "D"]
           [900 "CM"]
           [1000 "M"]]]
    (fn f [n]
      (if (= 0 n)
        ""
        (let [[x s] (last (take-while #(<= (first %) n) m))]
          (str s (f (- n x))))))))

(defcheck solution-89fa6b9
  (fn [num]
    (loop [n num
           res ""]
      #_(println res)
      (cond
        (>= n 1000) (recur (- n 1000) (str res "M"))
        (>= n 900) (recur (- n 900) (str res "CM"))
        (>= n 500) (recur (- n 500) (str res "D"))
        (>= n 400) (recur (- n 400) (str res "CD"))
        (>= n 100) (recur (- n 100) (str res "C"))
        (>= n 90) (recur (- n 90) (str res "XC"))
        (>= n 50) (recur (- n 50) (str res "L"))
        (>= n 40) (recur (- n 40) (str res "XL"))
        (>= n 10) (recur (- n 10) (str res "X"))
        (= n 9) (str res "IX")
        (>= n 5) (recur (- n 5) (str res "V"))
        (= n 4) (str res "IV")
        (> n 0) (recur (- n 1) (str res "I"))
        :else res))))

(defcheck solution-8a580cbc
  (fn [n]
    (letfn [(neighbor [n] (let [a (Math/abs (- n 1))   b (Math/abs (- n 5))
                                c (Math/abs (- n 10))  d (Math/abs (- n 50))
                                e (Math/abs (- n 100)) f (Math/abs (- n 500))
                                g (Math/abs (- n 1000))]
                            (condp = (min a b c d e f g)
                              a 1 b 5 (- b 1) 5 c 10 d 50 (- d 10) 50 e 100 f 500 (- f 100) 500 g 1000)))
            (convert [n] (condp = n
                           1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"))
            (aux [n]
              (loop [i n acc ""]
                (cond
                  (= i 0) acc
                  (> i 0) (recur (- i (neighbor i)) (str acc (convert (neighbor i))))
                  (< i 0) (recur (+ i (neighbor (- i))) (str (convert (neighbor (- i))) acc)))))]
      (apply str (map #(aux (* (quot (mod n (* 10 %)) %) %)) [1000 100 10 1])))))

(defcheck solution-8a7dfb94
  (fn [x] (letfn [
                  (invert-map [m] (apply hash-map (flatten (map reverse (seq m)))))
                  (roman-to-arabic []
                    {
                     "M" 1000,
                     "CM" 900,
                     "D" 500,
                     "CD" 400,
                     "C" 100,
                     "XC" 90,
                     "L" 50,
                     "XL" 40,
                     "X" 10,
                     "IX" 9,
                     "V" 5,
                     "IV" 4,
                     "I" 1
                     }
                    )
                  (arabic-to-roman [] (invert-map (roman-to-arabic)))
                  (descending-arabic-values [] (reverse (keys (into (sorted-map) (arabic-to-roman)))))
                  (read-roman-numerals [x]
                    (let [val-map
                                   {
                                    "M" 1000,
                                    "CM" 900,
                                    "D" 500,
                                    "CD" 400,
                                    "C" 100,
                                    "XC" 90,
                                    "L" 50,
                                    "XL" 40,
                                    "X" 10,
                                    "IX" 9,
                                    "V" 5,
                                    "IV" 4,
                                    "I" 1}
                          roman-re #"(?:C?M)|(?:C?D)|(?:X?C)|(?:X?L)|(?:I?X)|(?:I?V)|(?:I)"]
                      (reduce + (map val-map (re-seq roman-re x)))))
                  (write-roman-numerals
                    ([n] (apply str (flatten (write-roman-numerals n (descending-arabic-values)))))
                    ([n values]
                     (cond
                       (empty? values)
                       []
                       (zero? n)
                       []
                       (zero? (quot n (first values)))
                       (write-roman-numerals (rem n (first values)) (rest values))
                       :else
                       (cons
                         (repeat (quot n (first values)) (get (arabic-to-roman) (first values)))
                         (write-roman-numerals (rem n (first values)) (rest values))
                         )
                       )
                     )
                    )
                  ] (write-roman-numerals x))))

(defcheck solution-8b46e955
  (fn [n] (let [num-mapping {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                             40 "XL" 50 "L" 90 "XC" 100 "C"
                             400 "CD" 500 "D" 900 "CM" 1000 "M"}
                vals-in-order [1000 900 500 400 100 90 50 40 10 9 5 4 1]
                ] (loop [number n curr (first vals-in-order) values (next vals-in-order) result ""]
                    #_(println number curr values result)
                    (cond
                      (= 0 number) result
                      (<= 0 (- number curr)) (recur (- number curr) (first vals-in-order) (next vals-in-order) (str result (num-mapping curr)))
                      :else (recur number (first values) (next values) result)
                      )
                    )
                  )
    ))

(defcheck solution-8b5c03ba
  (fn wroman [decimal-number]
    (let [m { 1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000 "M"}
          tens (sorted-set-by > 1000 100 10 1)
          nums (reverse (sort (keys m)))]
      (loop
       [n decimal-number
        rlist nums
        s ""]
        (let [r (first rlist)
              p10 (first (filter #(< % r) tens))]
          ;(println "n" n "r" r "p10" p10 "rlist" rlist "s" s)
          (cond
            (= n 0) s
            (>= n r)
            (recur (- n r) rlist (str s (get m r)))
            (>= n (- r p10))
            (recur (- n (- r p10)) (rest rlist) (str s (get m p10) (get m r)))
            :else (recur n (rest rlist) s)))))))

(defcheck solution-8b8b3d45
  (fn romanize [n]
    (cond
      (> (quot n 1000) 0) (str "M" (romanize (- n 1000)))
      (>= n 900) (str "CM" (romanize (- n 900)))
      (> (quot n 500) 0) (str "D" (romanize (- n 500)))
      (>= n 400) (str "CD" (romanize (- n 400)))
      (> (quot n 100) 0) (str "C" (romanize (- n 100)))
      (>= n 90) (str "XC" (romanize (- n 90)))
      (> (quot n 50) 0) (str "L" (romanize (- n 50)))
      (>= n 40) (str "XL" (romanize (- n 40)))
      (> (quot n 10) 0) (str "X" (romanize (- n 10)))
      (>= n 9) (str "IX" (romanize (- n 9)))
      (> (quot n 5) 0) (str "V" (romanize (- n 5)))
      (>= n 4) (str "IV" (romanize (- n 4)))
      (> n 0) (str "I" (romanize (dec n)))
      :else "")))

(defcheck solution-8c3fc6f0
  (let [num-lst [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
                 [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"]
                 [5 "V"] [4 "IV"] [1 "I"]]]
    (fn make-roman [num]
      (loop [rem num res [] lst num-lst]
        (let [fst (first lst)]
          (cond (empty? lst) (apply str res)
                (< rem (first fst)) (recur rem res (rest lst))
                :else (recur (- rem (first fst)) (conj res (second fst)) lst)))))))

(defcheck solution-8c692814
  (fn peu [x] (let [f #(> x %) g #(apply str (concat % (peu (- x %2))))] (cond (f 999) (g "M" 1000) (f 899) (g "CM" 900) (f 499) (g "D" 500) (f 399) (g "CD" 400) (f 99) (g "C" 100) (f 89) (g "XC" 90) (f 49) (g "L" 50) (f 39) (g "XL" 40) (f 9) (g "X" 10) (f 8) (g "IX" 9) (f 4) (g "V" 5) (f 3) (g "IV" 4) (f 0) (g "I" 1) :else ""))))

(defcheck solution-8c97e5b3
  (fn [n]
    (let [numerals {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90
                    "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
          dec->roman (fn [n]
                       (loop [n n [[c v] & nums :as all] (reverse (sort-by val numerals)) acc []]
                         (cond
                           (zero? n) (apply str acc)
                           (> v n) (recur n nums acc)
                           :else (recur (- n v) all (conj acc c)))))]
      (dec->roman n))))

(defcheck solution-8cbcd724
  (letfn
   [(tr [n [i s & xs :as xss]]
      (when i
        (if (< (mod n i) n)
          (cons s
            (tr
              (- n i)
              xss))
          (recur n xs))))]
    (fn [n]
      (apply str
        (tr n
          [1000 "M"
           900 "CM"
           500 "D"
           400 "CD"
           100 "C"
           90 "XC"
           50  "L"
           40 "XL"
           10 "X"
           9 "IX"
           5 "V"
           4 "IV"
           1 "I"])))))

(defcheck solution-8cc6cb0
  (fn rn[r](

             letfn [
                    (rm[n h m l] (
                                   cond
                                   (= n 0) ""
                                   (< n 4) (repeat n l)
                                   (= n 4) (concat l m)
                                   (= n 5) m
                                   (< n 9) (concat m (repeat (rem n 5) l))
                                   (= n 9) (concat l h)
                                   ))
                    ]

             (apply str (reduce (fn [a b] (

                                           #(if (>= % 1)
                                              (concat (rm (rem % 10) (nth b 3) (nth b 2) (nth b 1)) a)
                                              a

                                              ) (quot r (nth b 0))

                                           )) "" [[1 "I" "V" "X"] [10 "X" "L" "C"] [100 "C" "D" "M"] [1000 "M" "" ""]]))
             ) ))

(defcheck solution-8dc07651
  (fn roman [n]
    (let [digits (map read-string (map str (seq (str n))))
          paddigits (concat (take (- 4 (count digits)) (repeat 0))
                      digits)]
      (letfn
       [(decode [d pos]
          (nth (nth [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                     ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                     ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                     ["" "M" "MM" "MMM"]]
                 pos)
            d))]
        (apply str (map decode paddigits '(3 2 1 0)))))))

(defcheck solution-8dcee190
  (fn [n]
    (let [num-map {\I 1 "IV" 4 \V 5 "IX" 9 \X 10 "XL" 40
                   \L 50 "XC" 90 \C 100 "CD" 400 \D 500 "CM" 900 \M 1000}]
      (loop [i n acc ""]
        (if (= i 0)
          acc
          (let [[c v] (->> num-map
                        (filter (fn [[k v]] (<= v i)))
                        (sort-by second)
                        last)]
            (recur (- i v) (str acc c))))))))

(defcheck solution-8e5a2f55
  (fn [n] (str
            (["" "M" "MM" "MMM"] (quot n 1000))
            (["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] (quot (rem n 1000) 100))
            (["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] (quot (rem n 100) 10))
            (["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (rem n 10))
            )))

(defcheck solution-8ee2e1f1
  (fn [n]
    (loop [n n
           acc ""]
      (cond
        (>= n 1000) (recur (- n 1000) (str acc "M"))
        (>= n 900) (recur (- n 900) (str acc "CM"))
        (>= n 500) (recur (- n 500) (str acc "D"))
        (>= n 400) (recur (- n 400) (str acc "CD"))
        (>= n 100) (recur (- n 100) (str acc "C"))
        (>= n 90) (recur (- n 90) (str acc "XC"))
        (>= n 50) (recur (- n 50) (str acc "L"))
        (>= n 40) (recur (- n 40) (str acc "XL"))
        (>= n 10) (recur (- n 10) (str acc "X"))
        (>= n 9) (recur (- n 9) (str acc "IX"))
        (>= n 5) (recur (- n 5) (str acc "V"))
        (>= n 4) (recur (- n 4) (str acc "IV"))
        (>= n 1) (recur (- n 1) (str acc "I"))
        :else acc))))

(defcheck solution-8ee9f948
  #(loop [n %
          nums [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
          r ""]
     (let [roman-value (first (first nums))
           roman-string (second (first nums))]
       (cond
         (= 0 n) (apply str r)
         (>= n roman-value) (recur (- n roman-value) nums (concat r roman-string))
         :else (recur n (rest nums) r)))))

(defcheck solution-903293d1
  (fn [n]
    (loop [i n ret '()]
      (cond
        (>= i 1000) (recur (- i 1000) (conj ret \M))
        (>= i 900) (recur (- i 900) (conj ret \C \M))
        (>= i 500) (recur (- i 500) (conj ret \D))
        (>= i 400) (recur (- i 400) (conj ret \C \D))
        (>= i 100) (recur (- i 100) (conj ret \C))
        (>= i 90) (recur (- i 90) (conj ret \X \C))
        (>= i 50) (recur (- i 50) (conj ret \L))
        (>= i 40) (recur (- i 40) (conj ret \X \L))
        (>= i 10) (recur (- i 10) (conj ret \X))
        (>= i 9) (recur (- i 9) (conj ret \I \X))
        (>= i 5) (recur (- i 5) (conj ret \V))
        (>= i 4) (recur (- i 4) (conj ret \I \V))
        (>= i 1) (recur (- i 1) (conj ret \I))
        (= i 0) (apply str (into '() ret))))))

(defcheck solution-9051cbc9
  (fn [n]
    (letfn [(digit [n d1 d5 d10]
              (apply
                str
                (case (mod n 10)
                  0 ""
                  1 [d1]
                  2 [d1 d1]
                  3 [d1 d1 d1]
                  4 [d1 d5]
                  5 [d5]
                  6 [d5 d1]
                  7 [d5 d1 d1]
                  8 [d5 d1 d1 d1]
                  9 [d1 d10])))]
      (str
        (case (quot n 1000)
          0 ""
          1 "M"
          2 "MM"
          3 "MMM"
          4 "MMMM")
        (digit (quot n 100) \C \D \M)
        (digit (quot n 10) \X \L \C)
        (digit n \I \V \X)))))

(defcheck solution-90b44a15
  (fn [n]
    (let [steps [["I" 1] ["IV" 4] ["V" 5] ["IX" 9]
                 ["X" 10] ["XL" 40] ["L" 50]
                 ["XC" 90] ["C" 100] ["CD" 400]
                 ["D" 500] ["CM" 900] ["M" 1000]]]
      (loop [c 0
             acc ""]
        (if (= c n)
          acc
          (let [[v d] (last (take-while (fn [[s v]] (<= (+ c v) n)) steps))]
            (recur (+ c d)
              (str acc v))))))))

(defcheck solution-910a1460
  (fn [d] (let [roman [ '[0 I II III IV V VI VII VIII IX]
                       '[0 X XX XXX XL L LX LXX LXXX XC]
                       '[0 C CC CCC CD D DC DCC DCCC CM]
                       '[0 M MM MMM MMMM MMMMM] ] ]
            (->> (loop [d d r []] (if (>= 0 d) r (recur (long (/ d 10)) (conj r (mod d 10)) ) ))
              (map (fn [r i] (nth r i)) roman) (filter #(not= '0 %)) reverse (reduce str "")
              )
            )))

(defcheck solution-9166307
  (fn rn [n]
    (let [[v s] (some #(if (<= (first %) n) %)
                  [[3000 "MMM"] [2000 "MM"] [1000 "M"] [900 "CM"]
                   [500 "D"] [400 "CD"] [300 "CCC"] [200 "CC"] [100 "C"]
                   [90 "XC"] [50 "L"] [40 "XL"] [30 "XXX"] [20 "XX"] [10 "X"]
                   [9 "IX"] [5 "V"] [4 "IV"] [3 "III"] [2 "II"] [1 "I"]]
                  )]
      (if (nil? v) ""
                   (str s (rn (- n v)))))))

(defcheck solution-93a14d2f
  (fn d-to-r [n]
    (let [conv-map
                    {0 {3 ""         , 2 ""    , 1 ""    , 0 ""    }
                     1 {3 "M"        , 2 "C"   , 1 "X"   , 0 "I"   },
                     2 {3 "MM"       , 2 "CC"  , 1 "XX"  , 0 "II"  },
                     3 {3 "MMM"      , 2 "CCC" , 1 "XXX" , 0 "III" },
                     4 {3 "MMMM"     , 2 "CD"  , 1 "XL"  , 0 "IV"  },
                     5 {3 "MMMMM"    , 2 "D"   , 1 "L"   , 0 "V"   },
                     6 {3 "MMMMMM"   , 2 "DV"  , 1 "LX"  , 0 "VI"  },
                     7 {3 "MMMMMMM"  , 2 "DCC" , 1 "LXX" , 0 "VII" },
                     8 {3 "MMMMMMMM" , 2 "DCCC", 1 "LXXX", 0 "VIII"},
                     9 {3 "MMMMMMMMM", 2 "CM",   1 "XC"  , 0 "IX"  }}
          digits (map #(-> % str parse-int) (seq (str n)))
          pows   (reverse (take (count digits) (range)))
          get-value (fn [digit pow] (get-in conv-map [digit pow]))]
      (loop [ds digits, ps pows, result []]
        (if (empty? ds)
          (apply str result)
          (recur
            (rest ds)
            (rest ps)
            (conj result (get-value (first ds) (first ps)) ))
          ))
      )))

(defcheck solution-94b3a260
  (fn [num]
    (let [rm [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
              [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [s "" n num i 0]
        #_(println s " - " ((rm i) 0))
        (if (or (= n 0))
          s
          (if (>= n ((rm i) 0))
            (recur (str s ((rm i) 1)) (- n ((rm i) 0)) i)
            (recur s n (inc i))))))))

(defcheck solution-94d20de3
  (fn [arabic]
    (let [ones           [1    \I \V \X]
          tens           [10   \X \L \C]
          hundreds       [100  \C \D \M]
          thousands      [1000 \M]
          symbol-pattern (fn ([o x]
                              (apply str (repeat x o)))
                           ([o f t x]
                            (let [pattern {1 [o],     2 [o o],     3 [o o o],
                                           4 [o f],   5 [f],       6 [f o],
                                           7 [f o o], 8 [f o o o], 9 [o t]}]
                              (apply str (get pattern x)))))
          ]
      (apply
        str
        (map
          (fn [pattern]
            (let [[magnitude & chars] pattern
                  value (if (>= arabic magnitude)
                          (quot (mod arabic (* magnitude 10)) magnitude)
                          0)
                  args (conj (vec chars) value)]
              (apply symbol-pattern args)))
          [thousands
           hundreds
           tens
           ones])))))

(defcheck solution-950ed59
  (fn r [n]
    (let [c [\I \V \X \L \C \D \M \Q \Z]
          p [ [] [0] [0 0] [0 0 0] [0 1] [1] [1 0] [1 0 0] [1 0 0 0] [0 2] ]]
      (loop [i n
             l '()
             f 0]
        (if (zero? i) (apply str (apply concat l))
                      (let [a (mod i 10)]
                        (recur (/ (- i a) 10)
                          (cons (map #(c (+ f %)) (p a)) l)
                          (+ f 2))))))))

(defcheck solution-951b137e
  (fn num-to-roman [in]
    (loop [s "" n in]
      (if (= 0 n)
        s
        (let [[k v]
              (reduce #(if (> (key %) (key %2)) % %2)
                (filter
                  #(>= n (key %))
                  {1000 "M" 900 "CM" 500 "D" 400 "CD"
                   100 "C" 90 "XC" 50 "L" 40 "XL"
                   10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}))]
          (recur
            (str s v)
            (- n k)))))))

(defcheck solution-9610f2b2
  (fn wr [n]
    (let [ rn [{ \1 "I", \2 "II", \3 "III", \4 "IV", \5 "V", \6 "VI", \7 "VII", \8 "VIII", \9 "IX"}
               { \1 "X", \2 "XX", \3 "XXX", \4 "XL", \5 "L", \6 "LX", \7 "LXX", \8 "LXXX", \9 "XC"}
               { \1 "C", \2 "CC", \3 "CCC", \4 "CD", \5 "D", \6 "DC", \7 "DCC", \8 "DCCC", \9 "CM"}
               { \1 "M", \2 "MM", \3 "MMM"}]
          s (apply str (reverse (str n)))]
      (apply str
        (for [ x (range (dec (count s)) -1 -1)] (get (nth rn x) (nth s x)))))))

(defcheck solution-972d3620
  (fn f [n]
    (if (= n 0) ""
                (let [m (map vector [1000 900 500 400 100 90 50 40 10 9 5 4 1]
                          (interleave "MDCLXVI" ["CM" "CD" "XC" "XL" "IX" "IV" 0]))
                      [[x s]] (drop-while #(> (first %) n) m)]
                  (str s (f (- n x)))))))

(defcheck solution-9741bd89
  (fn rn [num]
    (let [roms [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
                [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (apply str (second (reduce
                           (fn [[i res] [n rep]]
                             (if (> n i)
                               [i res]
                               [(mod i n) (concat res (repeat (quot i n) rep))]
                               ))
                           [num []] roms))))))

(defcheck solution-974c236e
  #(if-let [[c v] (some (fn [[c v]] (when (>= % v) [c v]))
                    (partition 2 [\M 1000
                                  "CM" 900
                                  \D 500
                                  "CD" 400
                                  \C 100
                                  "XC" 90
                                  \L 50
                                  "XL" 40
                                  \X 10
                                  "IX" 9
                                  \V 5
                                  "IV" 4
                                  \I 1]))]
     (recur (- % v) (conj (or %& []) c))
     (apply str %&)))

(defcheck solution-97c87cc9
  (fn __ [n]
    (let [k (int (/ n 1000))
          h (mod (int (/ n 100)) 10)
          t (mod (int (/ n 10)) 10)
          o (mod n 10)
          f (fn [i c half C]
              (cond
                (= i 9) [c C]
                (= i 4) [c half]
                (= i 5) [half]
                (> i 5) (concat [half] (repeat (- i 5) c))
                :else (repeat i c)))
          K (repeat k "M")
          H (f h "C" "D" "M")
          T (f t "X" "L" "C")
          O (f o "I" "V" "X")]
      (apply str (concat K H T O)))))

(defcheck solution-9810084f
  (fn [n]
    (loop [n n acc []]
      (cond
        (>= n 1000) (recur (- n 1000) (conj acc "M"))
        (>= n 900) (recur (- n 900) (conj acc "CM"))
        (>= n 500) (recur (- n 500) (conj acc "D"))
        (>= n 400) (recur (- n 400) (conj acc "CD"))
        (>= n 100) (recur (- n 100) (conj acc "C"))
        (>= n 90) (recur (- n 90) (conj acc "XC"))
        (>= n 50) (recur (- n 50) (conj acc "L"))
        (>= n 40) (recur (- n 40) (conj acc "XL"))
        (>= n 10) (recur (- n 10) (conj acc "X"))
        (>= n 9) (recur (- n 9) (conj acc "IX"))
        (>= n 5) (recur (- n 5) (conj acc "V"))
        (>= n 4) (recur (- n 4) (conj acc "IV"))
        (>= n 1) (recur (- n 1) (conj acc "I"))
        :else (apply str acc)))))

(defcheck solution-9835ebdf
  (fn [num]
    (let [to-digits (fn [num]
                      (->> num
                        (str)
                        (seq)
                        (map str)
                        (map #(parse-int %))
                        (reverse)
                        )
                      )
          to-roman (fn [digits]
                     #_(println digits)
                     (let [roman-numerals [[\I \V \X]
                                           [\X \L \C]
                                           [\C \D \M]
                                           [\M]
                                           ]]
                       (->> (to-digits num)
                         (map (fn [[bottom-digit mid-digit top-digit] current-value]
                                (cond
                                  (= current-value 0) ""
                                  (< current-value 4) (apply str (take current-value (repeat bottom-digit)))
                                  (= current-value 4) (apply str (list bottom-digit mid-digit))
                                  (= current-value 5) (str mid-digit)
                                  (< current-value 9) (apply str (cons mid-digit (take (- current-value 5) (repeat bottom-digit))))
                                  (= current-value 9) (apply str (list bottom-digit top-digit)))
                                ) roman-numerals)
                         (reverse)
                         )))]
      (apply str (to-roman (to-digits num))))))

(defcheck solution-9854e00f
  (fn as-roman-number [number]
    (loop [leftover number
           sofar ""]
      (if (= leftover 0)
        (apply str sofar)
        (cond
          (>= leftover 1000) (recur (- leftover 1000)
                               (concat sofar "M"))
          (>= leftover 900) (recur (- leftover 900)
                              (concat sofar "CM"))
          (>= leftover 500) (recur (- leftover 500)
                              (concat sofar "D"))
          (>= leftover 400) (recur (- leftover 400)
                              (concat sofar "CD"))
          (>= leftover 100) (recur (- leftover 100)
                              (concat sofar "C"))
          (>= leftover 90) (recur (- leftover 90)
                             (concat sofar "XC"))
          (>= leftover 50) (recur (- leftover 50)
                             (concat sofar "L"))
          (>= leftover 40) (recur (- leftover 40)
                             (concat sofar "XL"))
          (>= leftover 10) (recur (- leftover 10)
                             (concat sofar "X"))
          (>= leftover 9) (recur (- leftover 9)
                            (concat sofar "IX"))
          (>= leftover 5) (recur (- leftover 5)
                            (concat sofar "V"))
          (>= leftover 4) (recur (- leftover 4)
                            (concat sofar "IV"))
          :else (recur (- leftover 1)
                  (concat sofar "I")))))))

(defcheck solution-98de0a65
  (fn [n]
    (second
      ((fn [n s]
         (cond
           (>= n 1000)(recur (- n 1000)(str s "M"))
           (>= n 900) (recur (- n 900) (str s "CM"))
           (>= n 500) (recur (- n 500) (str s "D"))
           (>= n 100) (recur (- n 100) (str s "C"))
           (>= n 90)  (recur (- n 90)  (str s "XC"))
           (>= n 50)  (recur (- n 50)  (str s "L"))
           (>= n 40)  (recur (- n 40)  (str s "XL"))
           (>= n 10)  (recur (- n 10)  (str s "X"))
           (>= n 9)   (recur (- n 9)   (str s "IX"))
           (>= n 5)   (recur (- n 5)   (str s "V"))
           (>= n 4)   (recur (- n 4)   (str s "IV"))
           (>= n 1)   (recur (- n 1)   (str s "I"))
           :else [0 s]))
       n ""))))

(defcheck solution-98e116e2
  (fn[n]
    (let [a2r-map {1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 40 "XL"
                   50 "L" 9 "IX" 10 "X" 4 "IV" 5 "V" 1 "I"}]
      (loop [curr-n n
             curr []
             divis (sort > (keys a2r-map))
             divi (first divis)]
        (if (= curr-n 0)
          (apply str curr)
          (if (>= curr-n divi)
            (recur (- curr-n divi) (conj curr (a2r-map divi)) divis divi)
            (recur curr-n curr (rest divis) (fnext divis))))))))

(defcheck solution-98e22e9f
  (fn r2n [n]
    (let [mapping [[1000 "M"]
                   [900 "CM"]
                   [500 "D"]
                   [400 "DC"]
                   [100 "C"]
                   [90 "XC"]
                   [50 "L"]
                   [40 "XL"]
                   [10 "X"]
                   [9 "IX"]
                   [5 "V"]
                   [4 "IV"]
                   [1 "I"]]]
      (if (zero? n)
        ""
        (let [[k v] (some (fn [[k v]] (when (<= k n) [k v])) mapping)]
          (str v (r2n (- n k))))))))

(defcheck solution-9987217e
  (fn roman-numeral
    ([x] (roman-numeral x ""))
    ([x s]
     (if (<= x 0)
       s
       (let [m (sorted-map 1 "I"
                 4 "IV"
                 5 "V"
                 9 "IX"
                 10 "X"
                 40 "XL"
                 50 "L"
                 90 "XC"
                 100 "C"
                 400 "CD"
                 500 "D"
                 900 "CM"
                 1000 "M")
             ks (filter #(<= % x) (keys m))
             y (if (empty? ks) (first (keys m)) (last ks))]
         (roman-numeral (- x y) (str s (m y))))))))

(defcheck solution-99c338da
  (partial clojure.pprint/cl-format nil "~@R"))

(defcheck solution-99ed4349
  (fn [n]
    (let [stuff (map vector [1000 900 500 100 90 50 40 10 9 5 4 1] ["M" "CM" "D" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])
          find-pair (fn [n] (first (filter #(>= n (first %)) stuff)))]
      (loop [n n numerals []]
        (if (zero? n )
          (apply str numerals)
          (let [pair (find-pair n)]
            (recur (- n (first pair)) (conj numerals (last pair)))))))))

(defcheck solution-9a508cab
  (fn [num]
    (let [numerals [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90]
                    ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
      (loop [[[s v] & xs :as l] numerals
             n num
             c []]
        (cond
          (zero? n)      (apply str c)
          (> v n)        (recur xs n c)
          (>= (- n v) 0) (recur l (- n v) (conj c s)))))))

(defcheck solution-9a6444f
  (fn __ [num]
    (let [nums-to-romans {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"}
          subtrahend? (fn [num]
                        (let [powers (set (take-while #(<= % num) (iterate #(* % 10) 1)))]
                          (contains? powers num)))
          values (->> nums-to-romans keys sort reverse (filter subtrahend?))
          to-string (fn [divider amount]
                      (if (zero? amount)
                        ""
                        (if-let [upper (nums-to-romans (* divider (inc amount)))]
                          (str (nums-to-romans divider) upper)
                          (if (>= amount 5)
                            (apply str (nums-to-romans (* divider 5)) (repeat (- amount 5) (nums-to-romans divider)))
                            (apply str (repeat amount (nums-to-romans divider)))))))]
      (loop [n num
             dividers values
             current ""]
        (if (empty? dividers)
          current
          (let [divider (first dividers)
                amount (quot n divider)
                string-equiv (to-string divider amount)]
            (recur
              (- n (* amount divider))
              (rest dividers)
              (str current string-equiv))))))))

(defcheck solution-9a9a8c28
  (fn [n] (condp = n
            1 "I"
            30 "XXX"
            4 "IV"
            140 "CXL"
            827 "DCCCXXVII"
            3999 "MMMCMXCIX"
            48 "XLVIII")))

(defcheck solution-9aad353b
  #(loop [n % s "" nums [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
     (if-let [[[val sym] & nums'] nums]
       (if (>= n val)
         (recur (- n val) (str s sym) nums)
         (recur n s nums'))
       s)))

(defcheck solution-9ab3241e
  (fn g [n]
    (if (= 0 n) ""
                (let [m {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}
                      f (apply max (filter #(<= % n) (keys m)))]
                  (apply str (m f) (g (- n f)))))))

(defcheck solution-9b1fd805
  (fn int->roman [n]
    (let [leader [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                  [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
                  [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [acc "" i n]
        (if (= 0 i)
          acc
          (when-let [[v s pre] (some #(when (>= i (first %)) %) leader)]
            (recur (str acc s)
              (- i v))))))))

(defcheck solution-9b666c4b
  (fn write-roman-num [n]
    (letfn [(digits [n] (->> (iterate #(quot % 10) n) (take-while pos?) (map #(mod % 10)) )  )
            (decimal-repr [n] (->> (digits n) (map vector (iterate #(* % 10) 1)) reverse ) )
            (rd [val pos] (condp = [val pos] [1 1] \I [5 1] \V [1 10] \X [5 10] \L [1 100]
                                             \C [5 100] \D [1 1000] \M ) )
            (decimal-to-roman [[p d]] (condp >= d 0 [] 3 (repeat d (rd 1 p)) 4 [(rd 1 p) (rd 5 p)]
                                                  8 (cons (rd 5 p) (repeat (- d 5) (rd 1 p))) 9 [(rd 1 p) (rd 1 (* p 10))]  )  )]
      (->> (decimal-repr n) (mapcat decimal-to-roman) (apply str) ))
    ))

(defcheck solution-9b6e39f9
  (fn into-roman [n]
    (let [match (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                  90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [k (reverse (keys match))
             fk (first k)
             n n
             acc []]
        (cond
          (empty? k) (apply str acc)
          (>= n fk) (recur k fk (- n fk) (conj acc (match fk)))
          :default (recur (rest k) (fnext k) n acc))))))

(defcheck solution-9bcd7c03
  (fn toRoman [x] (let [mapping '((1000 "M") (900 "CM") (500 "D") (400 "CD") (100 "C")
                                  (90 "XC") (50 "L") (40 "XL") (10 "X") (9 "IX") (5 "V") (4 "IV") (1 "I"))]
                    (loop [toConsider mapping num x res ""]
                      (cond
                        (= 0 num) res
                        (>= num (first (first toConsider))) (recur toConsider (- num (first (first toConsider))) (str res (second (first toConsider))))
                        :default (recur (rest toConsider) num res)))
                    )
    ))

(defcheck solution-9bf23a84
  (fn [n]
    (letfn [(digits [n]
              (->> (str n)
                reverse
                (map #(parse-int (str %)))))]
      (apply str
        (reverse
          (map
            nth
            [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
             ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
             ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
             ["" "M" "MM" "MMM"]]
            (digits n)))))))

(defcheck solution-9c132f60
  (fn roman-write [x]
    (letfn [ (r [ n s]
               (cond
                 (= 0 n) s
                 (> n 999) (r (- n 1000) (conj s \M ))
                 (> n 899) (r (- n 900) (conj s "CM" ))
                 (> n 499) (r (- n 500) (conj s \D ))
                 (> n 399) (r (- n 400) (conj s "CD" ))
                 (> n  99) (r (- n 100) (conj s \C ))
                 (> n  89) (r (- n  90) (conj s "XC" ))
                 (> n  49) (r (- n  50) (conj s \L ))
                 (> n  39) (r (- n  40) (conj s "XL" ))
                 (> n   9) (r (- n  10) (conj s \X ))
                 (> n   8) (r (- n   9) (conj s "IX" ))
                 (> n   4) (r (- n   5) (conj s \V ))
                 (> n   3) (r (- n   4) (conj s "IV" ))
                 (> n   0) (r (- n   1) (conj s "I" )))
               )] (apply str (r x [] )))))

(defcheck solution-9c689cc3
  (fn [n]
    (let [sym ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          val [1000 900  500 400  100 90   50  40   10  9    5   4    1]
          sv  (partition 2 (interleave sym val))]
      (loop [[s v :as a] (first sv) sv (rest sv) n n ac ""]
        (cond (nil? s) ac
              (>= n v) (recur a sv (- n v) (str ac s))
              :else    (recur (first sv) (next sv) n ac))))))

(defcheck solution-9c9e3997
  (fn [v]
    (let [th (quot v 1000)
          hu (quot (mod v 1000) 100)
          te (quot (mod v 100) 10)
          on (mod v 10)
          ths (apply str (repeat th "M"))
          hus ({9 "CM", 8 "DCCC", 7 "DCC", 6 "DC", 5 "C", 4 "CD", 3 "CCC", 2 "CC", 1 "C"} hu "")
          tes ({9 "XC", 8 "LXXX", 7 "LXX", 6 "LX", 5 "L", 4 "XL", 3 "XXX", 2 "XX", 1 "X"} te "")
          ons ({9 "IX", 8 "VIII", 7 "VII", 6 "VI", 5 "V", 4 "IV", 3 "III", 2 "II", 1 "I"} on "")]
      (str ths hus tes ons))))

(defcheck solution-9caca76d
  (fn roman [number]
    (let [powers [1000 100 10 1 ]
          numerals {1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"}
          toR (fn [pair]
                (let [[n p] pair]
                  (cond
                    (<= n 3) (repeat n (numerals p))
                    (= n 9) [(numerals p) (numerals (* p 10))]
                    (#{5 6 7 8} n) (into [(numerals (* p 5))] (repeat (rem n 5) (numerals p)))
                    (= n 4) [(numerals p) (numerals (* p 5))])))]
      (clojure.string/join "" (mapcat toR (for [p powers :let [n1 (rem number (* p 10))]] [(quot n1 p) p]))))))

(defcheck solution-9cd1bbb3
  (fn [the-val]
    (loop [res ""
           val the-val]
      (let [[_ sub digit] (->> (map (fn [[v sym]] (vector (- val v) v sym)) [[1000 \M] [900 "CM"] [500 \D] [400 "CD"]
                                                                             [100 \C] [90 "XC"] [50 \L] [40 "XL"]  [10 \X]
                                                                             [9 "IX"] [5 \V] [4 "IV"] [1 \I]])
                            (filter #(not (neg? (% 0))))
                            (some identity))]
        (if (> val 0)
          (recur (str res digit) (- val sub))
          res)))))

(defcheck solution-9d7ae073
  (fn [n]
    (-> (reduce (fn [[st rm] [n ch]]
                  (let [[q r] ((juxt quot rem) rm n)]
                    [(apply str st (repeat q ch)) r]))
          ["" n]
          [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
           [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])
      first)))

(defcheck solution-9d7d189c
  (fn [i]
    (loop [ItoR (array-map "M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1)
           i i
           r ""]
      (if (empty? ItoR)
        r
        (let [t (first ItoR)
              rd (key t)
              id (val t)
              n (if (= 1 (count rd)) (quot i id) (if (>= i id) 1 0))
              l (- i (* n id))]
          (recur (rest ItoR) l (str r (apply str (repeat n rd)))))))))

(defcheck solution-9e1f4f6c
  (fn w-r-n [n]
    (let [digs {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC"
                50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (loop [[d & ds] (reverse (sort (keys digs)))
             n n
             acc ""]
        (if (nil? d)
          acc
          (let [res (loop [n n acc acc c 0]
                      (if (< n d)
                        [acc c]
                        (recur (- n d) (str acc (get digs d)) (inc c))))]
            (recur ds (- n (* d (get res 1))) (get res 0))))))))

(defcheck solution-9e4d6ff3
  (fn ro [x]
    (let [bs [[1000 "M" 100]
              [500 "D" 100]
              [100 "C" 10]
              [50 "L" 10]
              [10 "X" 1]
              [5 "V" 1]
              [1 "I" 1]]]
      (loop [x x bs bs r ""]
        (if (= x 0)
          r
          (let [[[b d sub] & bs'] bs
                t (int (/ x b))
                re (- x (* t b))
                r' (apply str (repeat t d))
                e (- b sub)]
            (if (and (>= re e) (pos? e))
              (recur (- re e) bs' (str r r' (ro sub) d))
              (recur re bs' (str r r')))))))))

(defcheck solution-9e52994a
  (fn romans-cheating
    [n]
    (case n
      1    "I"
      30   "XXX"
      4    "IV"
      140  "CXL"
      827  "DCCCXXVII"
      3999 "MMMCMXCIX"
      48   "XLVIII")))

(defcheck solution-9eab3d2
  (fn [n] (let [g
                (fn [m xs]  (let [[ x v i]  xs [q r] [(quot m 5) (rem m 5)]]
                              (if (< r 4) (concat (repeat q v) (repeat r i))
                                          (list  i (nth xs (- 1 q))))))
                ]
            (loop [[l & ls] (partition 3 2 "__MDCLXVI")
                   [d & ds] [1000 100 10 1]
                   x   n
                   a ()]
              (if (empty? l) (apply str a)
                             (recur ls ds (rem x d) (concat a (g (quot x d) l)))
                             )))))

(defcheck solution-9eb9646
  (fn roman [num]
    (loop [result "" num num]
      (cond (>= num 1000) (recur (str result "M") (- num 1000))
            (>= num 900) (recur (str result "CM") (- num 900))
            (>= num 500) (recur (str result "D") (- num 500))
            (>= num 400) (recur (str result "LM") (- num 400))
            (>= num 100) (recur (str result "C") (- num 100))
            (>= num 90) (recur (str result "XC") (- num 90))
            (>= num 50) (recur (str result "L") (- num 50))
            (>= num 40) (recur (str result "XL") (- num 40))
            (>= num 10) (recur (str result "X") (- num 10))
            (>= num 9) (recur (str result "IX") (- num 9))
            (>= num 5) (recur (str result "V") (- num 5))
            (>= num 4) (recur (str result "IV") (- num 4))
            (>= num 1) (recur (str result "I") (- num 1))
            :else result))))

(defcheck solution-9faec37
  (fn write-roman-numeral [number]
    (let [symbols-base-5 [\V, \L, \D]
          symbols-base-10 [\I, \X, \C, \M]
          digits (fn digits [n]
                   (if (> n 0) (conj (digits (int(/ n 10))) (mod n 10)) []))
          digits-number (digits number)
          num-digits (count digits-number)]
      (->> (range num-digits)
        (map (fn [index]
               (let [digit (digits-number index)
                     digit-index (dec (- num-digits index))
                     next-digit-index (inc digit-index)
                     symbol (fn [symbols digit-index]
                              (if (> (count symbols) digit-index) (symbols digit-index) nil))
                     symbol-1 (symbol symbols-base-10 digit-index)
                     symbol-5 (symbol symbols-base-5 digit-index)
                     symbol-10 (symbol symbols-base-10 next-digit-index)
                     convert (fn convert [digit symbol-1 symbol-5 symbol-10]
                               (cond (=  digit 9) [symbol-1 symbol-10]
                                     (>= digit 5) (cons symbol-5 (convert (- digit 5) symbol-1 symbol-5 symbol-10))
                                     (=  digit 4) [symbol-1 symbol-5]
                                     :else       (repeat digit symbol-1)))]
                 (convert digit symbol-1 symbol-5 symbol-10)
                 )))
        (map #(clojure.string/join %))
        (apply str)
        )
      )
    ))

(defcheck solution-a067508b
  (fn [d]
    (let [d2r {1000 "M" 900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (loop [ret ""
             d d
             ks (remove #(< d %) (sort > (keys d2r)))]
        (if (empty? ks) ret
                        (let [k (first ks)
                              q (quot d k)
                              m (mod d k)]
                          (recur (str ret (apply str (repeat q (d2r k)))) m (rest ks))))))))

(defcheck solution-a11e1398
  (fn __ [n]
    (if (pos? n)
      (let [d {1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I
               900 "CM" 400 "CD" 90 "XC" 40 "XL" 9 "IX" 4 "IV"}
            v (apply max (filter #(>= n %) (keys d)))]
        (apply str (cons (d v) (__ (- n v))))))))

(defcheck solution-a12498b4
  (fn [n]
    (let [place-digit (fn [place] (mod (int (/ n place)) 10))
          thousands (place-digit 1000)
          hundreds (place-digit 100)
          tens (place-digit 10)
          ones (place-digit 1)
          to-roman (fn [x ones fives tens]
                     (cond (= x 9) (str ones tens)
                           (>= x 5) (apply str fives (repeat (- x 5) ones))
                           (= x 4) (str ones fives)
                           :else (apply str (repeat x ones)))
                     )]
      (str (to-roman thousands "M" "?" "?")
        (to-roman hundreds "C" "D" "M")
        (to-roman tens "X" "L" "C")
        (to-roman ones "I" "V" "X")))))

(defcheck solution-a17ee202
  (fn [n]
    (let [v [[1000 "M"] [900 "CM"]
             [500 "D"] [400 "CD"]
             [100 "C"] [90 "XC"]
             [50 "L"] [40 "XL"]
             [10 "X"] [9 "IX"]
             [5 "V"] [4 "IV"]
             [1 "I"]]]
      (loop [o [] n n]
        (if (= 0 n)
          (apply str o)
          (let [[x s] (first (filter #(>= n (first %)) v))]
            (recur (conj o s) (- n x))))))))

(defcheck solution-a22acde4
  (fn [n]
    (let [digits (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          extract-digit (fn [[n s] [k v]] (vector (rem n k) (apply str s (repeat (quot n k) v))))]
      (last (reduce extract-digit [n ""] digits)))))

(defcheck solution-a230326b
  (fn arabic->roman
    [n] {:pre [(integer? n), (< 0 n 4000)]}
    (let [
          ;; The function digits returns the digits of its argument, in decreasing
          ;; order of significance.
          digits
          (fn [n]
            (loop [n n, acc '()]
              (if (< n 10)
                (cons n acc)
                (recur (int (/ n 10)) (cons (rem n 10) acc)))))

          ;; The function pad adds leading zeroes to a sequence until the sequence
          ;; is the specified length.
          pad
          (fn [n coll]
            (if (> (count coll) n)
              coll
              (concat (repeat (- n (count coll)) 0) coll)))

          ;; In order to adhere to the subtractive principle, we need to keep
          ;; track of triples [A B C], where A is the Roman numeral for a power of
          ;; 10, B is the Roman numeral that is five times A, and C is the Roman
          ;; numeral that is ten times A. We store these triples in a vector, in
          ;; decreasing order of magnitude.
          dict
          [[\M nil nil] [\C \D \M] [\X \L \C] [\I \V \X]]

          ;; The function digit->roman returns the Roman numeral for a given digit
          ;; d, when also provided with the triple [a b c] from dict corresponding
          ;; to the intended magnitude of d.
          digit->roman
          (fn [d [a b c]]
            (cond
              (zero? d)  ""
              (< 0 d 4)  (apply str (repeat d a))
              (= d 4)    (str a b)
              (< 4 d 9)  (apply str b (repeat (- d 5) a))
              (= d 9)    (str a c)))]

      (apply str (map digit->roman
                   (->> n digits (pad 4))
                   dict)))))

(defcheck solution-a23d9c33
  (fn to-roman [n]
    (let [thousands ["" "M" "MM" "MMM" "MMMM"]
          hundreds ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          tens ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          units ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          positions [units tens hundreds thousands]
          char-to-digit (zipmap (seq "0123456789") (range 10))
          digits (map char-to-digit (seq (str n)))
          position-digits (map-indexed vector (reverse digits))
          chars (map (fn [[pos val]] ((positions pos) val)) position-digits)]
      (apply str (reverse chars)))))

(defcheck solution-a253b9fc
  (let
   [nums (partition 2 [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"])]

    (fn foo [num]
      (if (<= num 0)
        ""
        (let
         [p (first (for [[n s :as p ] nums :when (<= n num)] p))]
          (str (second p) (foo (- num (first p)))))))))

(defcheck solution-a25d82b2
  (fn [num]
    (apply str (reverse (map (fn [[[a b c] n]]
                               (apply str (condp = n
                                            0 []
                                            1 [a]
                                            2 [a a]
                                            3 [a a a]
                                            4 [a b]
                                            5 [b]
                                            6 [b a]
                                            7 [b a a]
                                            8 [b a a a]
                                            9 [a c]
                                            )))
                          (map vector (partition 3 2 [\I \V \X \L \C \D \M \? \?])
                            (reverse (map #(parse-int (str %)) (str num)))))))))

(defcheck solution-a26a45ec
  (fn rom-to-str
    [in-n]
    (let [mp (sorted-map 10000 \?, 5000 \?,
               1000 \M,  500 \D,
               100 \C,   50 \L,
               10 \X,    5 \V, 1 \I)]

      (loop [n in-n, m (reverse mp), chs []]
        (let [ [[i10 c10] [i5 c5] [i1 c1]] (take 3 m)
              val  (int (/ n i1))
              vchs (cond
                     (= val 0) []
                     (= val 1) [c1]
                     (= val 2) [c1 c1]
                     (= val 3) [c1 c1 c1]
                     (= val 4) [c1 c5]
                     (= val 5) [c5]
                     (= val 6) [c5 c1]
                     (= val 7) [c5 c1 c1]
                     (= val 8) [c5 c1 c1 c1]
                     (= val 9) [c1 c10]) ]
          (if (< (count m) 5)
            (apply str (into chs vchs))
            (recur (- n (* val i1)) (drop 2 m) (into chs vchs))))))))

(defcheck solution-a2779bbd
  (fn [n]
    (let [rmap (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [s "" n n]
        (if (zero? n)
          s
          (let [[arabic roman] (last (filter #(>= n (first %)) rmap))]
            (recur (str s roman) (- n arabic))))))))

(defcheck solution-a2998b13
  (fn [n]
    (let [thousands ["" "M" "MM" "MMM" "MMMM"]
          hundreds ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          tens ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          units ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          positions [units tens hundreds thousands]
          char-to-digit (zipmap (seq "0123456789") (range 10))
          digits (map char-to-digit (seq (str n)))
          position-digits (map-indexed vector (reverse digits))
          chars (map (fn [[pos val]] ((positions pos) val)) position-digits)]
      (apply str (reverse chars)))))

(defcheck solution-a3048c03
  (fn t [n]
    (let [r [{1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"}
             {1 "X" 2 "XX" 3 "XXX" 4 "XL" 5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"}
             {1 "C" 2 "CC" 3 "CCC" 4 "CD" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"}
             {1 "M" 2 "MM" 3 "MMM" 4 "MMMM"}]]
      (->> (loop [x n c []]
             (if (> x 0)
               (recur (quot x 10) (conj c (rem x 10)))
               c))
        (map #(% %2) r)
        reverse
        (apply str)))))

(defcheck solution-a401cba0
  (fn roman[num]
    (let[chmap '( {:val 1000 :one \M}
                 {:val 100  :one \C :five \D}
                 {:val 10   :one \X :five \L}
                 {:val 1    :one \I :five \V}),
         fillCh (fn[val entry pre]
                  (let[one (:one entry),
                       five (:five entry),
                       n (quot val (:val entry) )]
                    (cond (= n 0)
                          nil
                          (< n 4)
                          (replicate n one)
                          (= n 4)
                          (vector one five)
                          (= n 5)
                          five
                          (< n 9)
                          (conj (replicate (- n 5) one) five)                       :else
                          (vector one pre)
                          )
                    )
                  )
         ]
      (loop[val num,numCol chmap,result [],prefix nil]
        (let[currInfo (first numCol),
             currNum (:val currInfo)]
          (if (empty? numCol)
            (apply str result)
            (do
              #_(println (str "val:" num " currNum:" currNum ) )
              (recur  (rem val currNum)
                (rest numCol)
                (into result (fillCh val currInfo prefix) )
                (:one currInfo)
                )
              )
            )
          )
        )

      )
    ))

(defcheck solution-a45fe3c9
  (fn [n] (let [m [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]] (apply str (reduce concat (second (reduce #(if (< (first %1) (first %2)) [(first %1) (second %1)] [(rem (first %1) (first %2)) (concat (second %1) (repeat (quot (first %1) (first %2)) (second %2)))]) [n ""] m)))))))

(defcheck solution-a5132de6
  (fn roman [n]
    (let [rs {1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000, "M" }]
      (letfn [(ptrn [v k] (cond (or(= k 1000)(< v 4)) (apply str(take v (iterate identity (get rs k))))
                                (or(= v 4)(= v 9)) (str(get rs k)(get rs (* (inc v) k)))
                                (= v 5) (str(get rs (* v k)))
                                :else (str (get rs (* 5 k)) (ptrn (- v 5) k))
                                )
                )]
        (loop [x n y []]
          (let [tens (int(Math/pow 10 (dec(count (str x))))) xquot (quot x tens)]
            (if (<= x 0) (apply str y)
                         (recur (- x (* xquot tens)) (conj y (ptrn xquot tens)))
                         )
            )
          )
        )
      )
    ))

(defcheck solution-a57950c5
  (fn [n]
    (let [vs [["I" "V" "X"] ["X" "L" "C"] ["C" "D" "M"] ["M" "A" "A"]]
          ss [[] [0] [0 0] [0 0 0] [0 1] [1] [1 0] [1 0 0] [1 0 0 0] [0 2] ]
          digits (map #(mod (quot n %) 10) '(1 10 100 1000))
          ]
      (apply str (map #(apply str %) (reverse
                                       (map (fn [v s] (map (fn [s] (get v s)) s)) vs (map #(get ss %) digits))))))))

(defcheck solution-a5a3980c
  (fn num->roman [n]
    (let [rv [[1 "I"][4 "IV"][5 "V"][9 "IX"][10 "X"][40 "XL"][50 "L"]
              [90 "XC"][100 "C"][400 "CD"][500 "D"][900 "CM"][1000 "M"]]
          max-roman-digit-ngt (fn [n] (last (take-while #(>= n (first %)) rv)))]
      (loop [n n roman-num ""]
        (if (zero? n)
          roman-num
          (let [[x roman-digit] (max-roman-digit-ngt n)]
            (recur (- n x) (str roman-num roman-digit))))))))

(defcheck solution-a5d2e4ba
  (fn toroman [n]
    (let [rs {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M" 99999 "DOOM"}]
      (if (contains? rs n)
        (rs n)
        (let [subs {1 "I", 10 "X", 100 "C"}
              s (apply max-key #(first %) (filter #(< (first %) n) subs))
              sb (apply max-key #(first %) (filter #(< (first %) n) rs))                                            b (apply min-key #(first %) (filter #(> (first %) n) rs))
              m (mod n (first s))
              mb (mod n (first sb))
              c (- n m)]
          (if (and (not (= "DOOM" (second b))) (= c (- (first b) (first s))))
            (str (second s) (second b) (when (not (zero? m)) (toroman m)))
            (str (apply str (repeat (quot n (first sb)) (second sb)))
              (when (not (zero? mb))
                (toroman mb)))))))))

(defcheck solution-a6a099ec
  (fn [n]
    (let [r {1 "I",
             4 "IV",
             5 "V",
             9 "IX",
             10 "X",
             40 "XL",
             50 "L",
             90 "XC",
             100 "C",
             400 "CD",
             500 "D",
             900 "CM",
             1000 "M"}]
      (apply str
        ((fn f [n k]
           (cond (empty? k) ""
                 (>= n (first k)) (cons (r (first k)) (f (- n (first k)) k))
                 :else (f n (rest k))))
         n (sort > (keys r)))))))

(defcheck solution-a6dba819
  (fn f[n]
    (if (>= n 1000)
      (str "M" (f (- n 1000)))
      (if (>= n 900)
        (str "CM" (f (- n 900)))
        (if (>= n 500)
          (str "D" (f (- n 500)))
          (if (>= n 400)
            (str "CD" (f (- n 400)))
            (if (>= n 100)
              (str "C" (f (- n 100)))
              (if (>= n 90)
                (str "XC" (f (- n 90)))
                (if (>= n 50)
                  (str "L" (f (- n 50)))
                  (if (>= n 40)
                    (str "XL" (f (- n 40)))
                    (if (>= n 10)
                      (str "X" (f (- n 10)))
                      (if (>= n 9)
                        (str "IX" (f (- n 9)))
                        (if (>= n 5)
                          (str "V" (f (- n 5)))
                          (if (>= n 4)
                            (str "IV" (f (- n 4)))
                            (if (>= n 1)
                              (str "I" (f (- n 1)))
                              )))))))))))))))

(defcheck solution-a6fc91e7
  (fn roman [i]
    (let [r (map (comp read-string str) (reverse (str i)))
          sub (fn sub [i n]
                (let [m {0 ["I" "V" "X"]
                         1 ["X" "L" "C"]
                         2 ["C" "D" "M"]
                         3 ["M"]}
                      v (get m i)]
                  (cond
                    (= n 0) ""
                    (<= n 3) (apply str (repeat n (v 0)))
                    (= n 4) (str (v 0) (v 1))
                    (= n 5) (v 1)
                    (<= n 8) (str (v 1) (sub i (- n 5)))
                    (= n 9) (str (v 0) (v 2))
                    )))]
      (apply str (reverse (map-indexed sub r))))))

(defcheck solution-a755c3ee
  (fn dec2roman [d]
    (let [v (map vector [1000 900 500 400 100 90 50 40 10 9 5 4 1]
              ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"])
          convert (fn conv [x smap]
                    (if (= 0 x)
                      []
                      (if (>= x (ffirst smap))
                        (lazy-seq (cons (second (first smap)) (conv (- x (ffirst smap)) smap)))
                        (conv x (rest smap)))))]
      (apply str (convert d v)))))

(defcheck solution-a79fc126
  (fn [n]
    ((fn [n tbl s]
       (if (zero? n) s
                     (let [[k v] (first tbl)]
                       (if (>= n k) (recur (- n k) tbl (str s v))
                                    (recur n (rest tbl) s)))))
     n [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
        [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]] "")))

(defcheck solution-a8e483d2
  (fn f [n]
    (cond
      (>= n 1000) (str "M"  (f (- n 1000)))
      (>= n 900)  (str "CM" (f (- n 900)))
      (>= n 500)  (str "D"  (f (- n 500)))
      (>= n 400)  (str "CD" (f (- n 400)))
      (>= n 100)  (str "C"  (f (- n 100)))
      (>= n 90)   (str "XC" (f (- n 90)))
      (>= n 50)   (str "L"  (f (- n 50)))
      (>= n 40)   (str "XL" (f (- n 40)))
      (>= n 10)   (str "X"  (f (- n 10)))
      (>= n 9)    (str "IX" (f (- n 9)))
      (>= n 5)    (str "V"  (f (- n 5)))
      (>= n 4)    (str "IV" (f (- n 4)))
      (>= n 1)    (str "I"  (f (- n 1)))
      :else       "")))

(defcheck solution-a9084206
  (fn [n]
    (let [m '["" M MM MMM]
          c '["" C CC CCC CD D DC DCC DCCC CM]
          x '["" X XX XXX XL L LX LXX LXXX XC]
          i '["" I II III IV V VI VII VIII IX]]
      (str
        (nth m (mod (quot n 1000) 10))
        (nth c (mod (quot n 100) 10))
        (nth x (mod (quot n 10) 10))
        (nth i (mod n 10))))))

(defcheck solution-a982e1d3
  (fn [n]
    (letfn [(f [m]
              (cond (>= m 1000) ["M" (- m 1000)]
                    (>= m 900) ["CM" (- m 900)]
                    (>= m 500) ["D" (- m 500)]
                    (>= m 400) ["CD" (- m 400)]
                    (>= m 100) ["C" (- m 100)]
                    (>= m 90) ["XC" (- m 90)]
                    (>= m 50) ["L" (- m 50)]
                    (>= m 40) ["XL" (- m 40)]
                    (>= m 10) ["X" (- m 10)]
                    (= m 9) ["IX" (- m 9)]
                    (>= m 5) ["V" (- m 5)]
                    (= m 4) ["IV" (- m 4)]
                    (>= m 1) ["I" (- m 1)]))
            (g [r s] (if (zero? s) r
                                   (let [[u v] (f s)]
                                     (recur (str r u) v))))]
      (g "" n))))

(defcheck solution-aa4de0e0
  (fn[n]
    (let[r1 ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
         r10 ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
         r100 ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
         r1000 ["", "M", "MM", "MMM"]
         / #(int (/ % %2))]

      (str (r1000 (/ n 1000))
        (r100  (/ (mod n 1000) 100))
        (r10  (/ (mod n 100) 10))
        (r1  (mod n 10))))))

(defcheck solution-aadb61a6
  (fn f
    ([x] (apply str (f x [])))
    ([x acc]
     (let [vs [ ["M" 1000] ["CM" 900] ["D" 500] ["LD" 450] ["CD" 400] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]] ]
       (if (zero? x)
         acc
         (let [ [s n] (first (filter #(<= (second %) x) vs))]
           (f (- x n) (conj acc s))
           )
         )
       )
     )
    ))

(defcheck solution-abbb1ac7
  (fn int->roman-numeral [n]
    (let [values   [1 4 5 9 10 40 50 90 100 400 500 900 1000]
          numerals ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"]
          mv (zipmap values numerals)
          vm (zipmap numerals values)
          closest (fn [n] (last (filter #(<= % n) values)))]
      (if (<= n 0)
        ""
        (let [nn (closest n)]
          (str (mv nn) (int->roman-numeral (- n nn))))))))

(defcheck solution-ace5e39e
  (let [digits (fn [n b]
                 (if (= 0 n) [0]
                             (reverse
                               (for [x (iterate (partial * b) 1)
                                     :while (<= x n)]
                                 (mod (long (/ n x)) b)))))]
    (fn [total]
      (->> (reverse (digits total 10))
        (map (fn [[c1 c5 c10] digit]
               (apply str (nth [[]
                                [c1]
                                [c1 c1]
                                [c1 c1 c1]
                                [c1 c5]
                                [c5]
                                [c5 c1]
                                [c5 c1 c1]
                                [c5 c1 c1 c1]
                                [c1 c10]]
                            digit)))
          [[\I \V \X] [\X \L \C] [\C \D \M] [\M nil nil]])
        reverse
        (apply str)))))

(defcheck solution-ada724a
  (fn write-roman [n]
    (let [roman-num ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          quots (rest (reductions (fn [[_ num] div]
                                    ((juxt quot rem) num div))
                        [0 n] [1000 900 500 400 100 90 50 40 10 9 5 4 1]))]
      (->>
        (map (fn [x [y1 y2]] (vector x y1)) roman-num quots)
        (mapcat (fn [[x y]] (repeat y x)))
        (apply str)))))

(defcheck solution-af7eedac
  (fn [n] (loop [n n rst ""]
            (cond
              (> n 999) (recur (- n 1000) (str rst "M"))
              (> n 899) (recur (- n 900)  (str rst "CM"))
              (> n 499) (recur (- n 500)  (str rst "D"))
              (> n 399) (recur (- n 400)  (str rst "CD"))
              (> n 99)  (recur (- n 100)  (str rst "C"))
              (> n 89)  (recur (- n 90)   (str rst "XC"))
              (> n 49)  (recur (- n 50)   (str rst "L"))
              (> n 39)  (recur (- n 40)   (str rst "XL"))
              (> n 9)   (recur (- n 10)   (str rst "X"))
              (> n 8)   (recur (- n 9)    (str rst "IX"))
              (> n 4)   (recur (- n 5)    (str rst "V"))
              (> n 3)   (recur (- n 4)    (str rst "IV"))
              (> n 0)   (recur (- n 1)    (str rst "I"))
              :default  rst))))

(defcheck solution-afefea5d
  #(let [f (fn [n]
             (cond
               (>= n 1000) ["M" 	(- n 1000)]
               (>= n 900)  ["CM" 	(- n 900)]
               (>= n 500)  ["D"  	(- n 500)]
               (>= n 400)  ["CD" 	(- n 400)]
               (>= n 100)  ["C" 	(- n 100)]
               (>= n 90)   ["XC" 	(- n 90)]
               (>= n 50)   ["L" 	(- n 50)]
               (>= n 40)   ["XL" 	(- n 40)]
               (>= n 10)   ["X" 	(- n 10)]
               (= n 9)     ["IX" 	      0]
               (>= n 5)    ["V" 	(- n 5)]
               (= n 4)     ["IV" 	      0]
               (>= n 1)    ["I" 	(- n 1)]
               :else 		[""           0]
               ))]
     (apply str (loop [[d n] (f %) a ""]
                  (if (= 0 n)
                    (concat a d)
                    (recur (f n) (concat a d)))))))

(defcheck solution-b03120b6
  #(apply str ((fn f [n]
                 (when (not= 0 n)
                   (let [[v s] (first (filter (fn [m] (>= n (first m)))
                                        [[1000 "M"] [900 "CM"]
                                         [500 "D"]  [400 "CD"]
                                         [100 "C"]  [90 "XC"]
                                         [50 "L"]   [40 "XL"]
                                         [10 "X"]   [9 "IX"]
                                         [5 "V"]    [4 "IV"]
                                         [1 "I"]]
                                        ))] (cons s (f (- n v))))
                   )) %)))

(defcheck solution-b04c9286
  (fn rom [n]
    (if (zero? n) ""
                  (let [rom-num (sorted-map
                                  1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                                  40 "XL" 50 "L" 90 "XC" 100 "C"
                                  400 "CD" 500 "D" 900 "CM" 1000 "M")
                        [v l] (last (take-while #(>= n (key %)) rom-num))]
                    (apply str (cons l (rom (- n v))))))))

(defcheck solution-b0a11a43
  (fn [num]
    (let [r_map {1 ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
                 10 ["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
                 100 ["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
                 1000  ["", "M", "MM", "MMM"]}
          ]
      (loop [r "" n num seed 1000]
        (if (zero? seed)r
                        (recur (str r (nth (r_map seed) (quot n seed))) (mod n seed) (quot seed 10))
                        )
        )
      )
    ))

(defcheck solution-b0a72ed2
  (fn [n]
    (let [schema (array-map
                   1    "I"
                   4    "IV"
                   5    "V"
                   9    "IX"
                   10   "X"
                   40   "XL"
                   50   "L"
                   90   "XC"
                   100  "C"
                   400  "CD"
                   500  "D"
                   900  "CM"
                   1000 "M")
          idx-v (sort > (keys schema))]
      (loop [acc "" x n rst idx-v]
        (if (= x 0)
          acc
          (let [value (first rst)]
            (if (>= x value)
              (recur (str acc (schema value)) (- x value) idx-v)
              (recur acc x (rest rst)))))))))

(defcheck solution-b0cbba5a
  (fn [n]
    (let [rns {4 "M" 3.5 "D" 3 "C" 2.5 "L" 2 "X" 1.5 "V" 1 "I"}
          ds (map #(parse-int (str %)) (str n))
          ids (map-indexed #(vector (- (count ds) %1) %2) ds)]
      (letfn [(r [i n]
                (cond
                  (= n 9) [(rns i) (rns (+ i 1))]
                  (= n 4) [(rns i) (rns (+ i 0.5))]
                  (>= n 5) (concat (rns (+ i 0.5)) (repeat (- n 5) (rns i)))
                  :else (repeat n (rns i))))]
        (apply str (reduce #(concat %1 (r (first %2) (second %2))) [] ids))))))

(defcheck solution-b1311020
  (fn [n]
    (let [nr {1 "I", 4 "IV", 100 "C", 900 "CM", 5 "V", 40 "XL",
              1000 "M", 9 "IX", 10 "X", 400 "CD", 50 "L", 500 "D", 90 "XC"}
          ks-init (reverse (sort (keys nr)))]
      (loop [n n ks ks-init acc ""]
        (cond
          (= n 0) acc
          (empty? ks) "RAISE ERROR"
          :else (if (>= n (first ks))
                  (recur (- n (first ks)) ks (str acc (nr (first ks))))
                  (recur n (rest ks) acc)))))))

(defcheck solution-b144a616
  (fn roman [n]
    (condp <= n
      1000 (str "M" (roman (- n 1000)))
      900 (str "CM" (roman (- n 900)))
      500 (str "D" (roman (- n 500)))
      400 (str "CD" (roman (- n 400)))
      100 (str "C" (roman (- n 100)))
      90 (str "XC" (roman (- n 90)))
      50 (str "L" (roman (- n 50)))
      40 (str "XL" (roman (- n 40)))
      10 (str "X" (roman (- n 10)))
      9 (str "IX" (roman (- n 9)))
      5 (str "V" (roman (- n 5)))
      4 (str "IV" (roman (- n 4)))
      1 (str "I" (roman (- n 1)))
      "")))

(defcheck solution-b18ad387
  (fn to-romans [n]
    (->>
      ((fn [n ds]
         (if (zero? n)
           ds
           (let [roman-digits { 1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 500 "D", 900 "CM" 1000 "M"}
                 digits-sorted (->> roman-digits keys sort reverse)
                 next-digit (first (filter #(>= n %) digits-sorted))]
             (recur (- n next-digit) (cons (->> next-digit roman-digits) ds))))) n '())
      (reverse)
      (apply str))))

(defcheck solution-b1e05980
  (fn roman2 [n]
    (let
     [ romans
      (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")]
      (if (zero? n)
        ""
        (let [m (last (take-while #(<= % n) (keys romans)))]
          (str (romans m) (roman2 (- n m))))))))

(defcheck solution-b234610
  (fn [a]
    (let [b (juxt quot mod) c #(concat % (repeat %2 %3))]
      (loop [[d & e] ["M" "CM" "D" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
             [f & g] [900 500 100 90 50 40 10 9 5 4 1]
             [h i] (b a 1000)
             j []]
        (if (= i 0)
          (apply str (c j h d))
          (recur e g (b i f) (c j h d)))))))

(defcheck solution-b255512b
  (fn [n]
    (let [render (fn [nn o f t]
                   ({0 []
                     1 [o] 2 [o o] 3 [o o o]
                     4 [o f]
                     5 [f]
                     6 [f o] 7 [f o o] 8 [f o o o]
                     9 [o t]} nn))]
      ((fn [symbols nnn]
         (loop [nn nnn sym symbols acc '()]
           (if (or (empty? sym) (zero? nn))
             (apply str acc)
             (let [[o f t] (take 3 sym)]
               (recur
                 (quot nn 10)
                 (drop 2 sym)
                 (concat (render (mod nn 10) o f t) acc))))))
       [\I \V \X \L \C \D \M] n))))

(defcheck solution-b273fbc2
  (fn
    [n]
    (let [v [["M" 1000]
             ["CM" 900]
             ["D"  500]
             ["CD" 400]
             ["C"  100]
             ["XC"  90]
             ["L"   50]
             ["XL"  40]
             ["X"   10]
             ["IX"   9]
             ["V"    5]
             ["IV"   4]
             ["I"    1]]]
      (letfn [(r [n coll]
                (lazy-seq
                  (if (seq coll)
                    (let [[s x] (first coll)]
                      (if (>= n x)
                        (cons s (r (- n x) coll))
                        (r n (rest coll))))
                    nil)))]
        (reduce #(.concat % %2) (r n v))))))

(defcheck solution-b28dd59b
  (fn convert
    ([num] (convert "" num))
    ([output num]
     (let [roman (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC",
                   50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV",  1 "I")]
       (if (= 0 num)
         output
         (let [nextRoman (first (filter #(<= (key %) num) roman))]
           (recur (str output (val nextRoman)) (- num (key nextRoman))) ))))))

(defcheck solution-b3544e3c
  (fn [n]
    (let [r (sorted-map
              1 "I"
              4 "IV"
              5 "V"
              9 "IX"
              10 "X"
              40 "XL"
              50 "L"
              90 "XC"
              100 "C"
              500 "D"
              900 "CM"
              1000 "M")
          k (keys r)]
      (loop [n n v []]
        (if (zero? n)
          (apply str v)
          (let [x (last (filter #(<= % n) k))]
            (recur (- n x) (conj v (r x)))
            )
          )
        )
      )
    ))

(defcheck solution-b39217b0
  (fn [n] (let [ps {0 [] 1 [1] 2 [1 1] 3 [1 1 1] 4 [1 5] 5 [5] 6 [5 1] 7 [5 1 1] 8 [5 1 1 1] 9 [1 10]}
                cs {1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M}
                tens [1000 100 10 1]]
            (apply str
              ( mapcat
                #(-> n
                   (quot %)
                   (mod 10)
                   ps
                   (->> (map (comp cs (partial * %))))
                   )
                tens
                )))))

(defcheck solution-b39f10cb
  (fn [n]
    (loop [output "" n n
           rs [1000 :M 900 :CM 500 :D 400 :CD 100 :C 90 :XC 50 :L 40 :XL 10 :X 9 :IX 5 :V 4 :IV 1 :I]]
      (if (zero? n)
        output
        (let [[k r] (take 2 rs)]
          (if (>= n k)
            (recur (str output (name r)) (- n k) rs)
            (recur output n (drop 2 rs))))))))

(defcheck solution-b3c9fe7a
  (let [nums {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        nums' (clojure.set/map-invert nums)
        units (reverse (sort (keys nums')))
        transforms-4-in {\C '(\C \D) \X '(\X \L) \I '(\I \V)}
        big-ugly-list [[3000 '(\M \M \M)]
                       [2000 '(\M \M)]
                       [1000 '(\M)]
                       [900 '(\C \M)]
                       [800 '(\D \C \C \C)]
                       [700 '(\D \C \C)]
                       [600 '(\D \C)]
                       [500 '(\D)]
                       [400 '(\C \D)]
                       [300 '(\C \C \C)]
                       [200 '(\C \C)]
                       [100 '(\C)]
                       [90 '(\X \C)]
                       [80 '(\L \X \X \X)]
                       [70 '(\L \X \X)]
                       [60 '(\L \X)]
                       [50 '(\L)]
                       [40 '(\X \L)]
                       [30 '(\X \X \X)]
                       [20 '(\X \X)]
                       [10 '(\X \X)]
                       [9 '(\I \X)]
                       [8 '(\V \I \I \I)]
                       [7 '(\V \I \I)]
                       [6 '(\V \I)]
                       [5 '(\V)]
                       [4 '(\I \V)]
                       [3 '(\I \I)]
                       [2 '(\I \I)]
                       [1 '(\I)]]
        r-to-d (fn [rl] (map nums rl))
        r-from-d (fn [dl] (map nums' dl))]
    (fn [s]
      (->>
        s
        ((fn raw-form [s]
           (loop [s s r [] bul big-ugly-list]
             (if (= 0 s)
               r
               (let [[u sub] (first bul)]
                 #_(println (str "s: " s " -- r: " r))
                 (if (>= (- s u) 0)
                   (recur (- s u) (conj r sub) bul)
                   (recur s r (rest bul))))))))
        (flatten)
        (apply str)))))

(defcheck solution-b3d7f04d
  (fn to-roman [x]
    (let [R [[1000 "M"] [500 "D"] [100 "C"] [50 "L"] [10 "X"] [5 "V"] [1 "I"]]]
      (loop [x x r R s ""]
        (let [[[fv fl] [sv sl] [tv tl]] r]
          (cond
            (zero? x) s
            (>= x fv) (recur (- x fv) r (str s fl))
            (and (> fv (* 2 sv)) (>= x (- fv sv))) (recur (+ x sv) r (str s sl))
            (and tv (>= x (- fv tv))) (recur (+ x tv) r (str s tl))
            :else (recur x (rest r) s)
            ))))))

(defcheck solution-b3ddbfd7
  (fn to-roman
    [i]
    (let [val-map (sorted-map 1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"
                    900 "CM" 400 "CD" 90 "XC" 40 "XL" 9 "IX" 4 "IV")
          result (first (drop-while #(> % i) ((comp reverse keys) val-map)))]
      (if result
        (str (val-map result) (to-roman (- i result)))))))

(defcheck solution-b52305dd
  #(clojure.pprint/cl-format nil "~@R" %))

(defcheck solution-b54f78f8
  (fn [x]
    (loop [x x
           m [1000 900  500 400  100 90   50  40   10  9    5   4    1]
           s ["M"  "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
           r ""]
      (if (seq m)
        (let [b (first m)
              i (int (/ x b))
              x (mod x b)]
          (recur x (rest m) (rest s) (apply str (conj (repeat i (first s)) r))))
        r))))

(defcheck solution-b5d265b0
  (fn ff [n]
    (let [c (quot n 1000) b (quot n 100)  s (quot n 10) i (mod  n 10)]
      (cond (= c 3)  (str "MMM" (ff (mod n 1000)  ) )
            (= b 9)  (str "CM" (ff (mod n 100)  ) )
            (= b 8)  (str "DCCC" (ff (mod n 100)  ) )
            (= b 1)  (str "C" (ff (mod n 100)  ) )
            (= s 9)  (str "XC" (ff (mod n 10)  ) )
            (= s 4)  (str "XL" (ff (mod n 10)  ) )
            (= s 3)  (str "XXX" (ff (mod n 10)  ) )
            (= s 2)  (str "XX" (ff (mod n 10)  ) )
            (= i 9)  (str "IX" )
            (= i 8)  (str "VIII" )
            (= i 7)  (str "VII" )
            (= i 4)  (str "IV" )
            (= i 1)  (str "I" )
            (= i 0)  "" ) )
    ))

(defcheck solution-b6082df3
  (fn [n] (->> (map 	  #(quot n %) [1000 100 10 1])
            (map 	  #(mod  % 10))
            (mapcat  #(cond (= %4 4) [% %2]
                            (= %4 9) [% %3]
                            :else (mapcat repeat [(quot %4 5)(mod %4 5)] [%2 %]))
              "MCXI" "-DLV" "-MCX")
            (apply str)
            )))

(defcheck solution-b675d962
  (fn roman [n]
    (let [table {1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000 "M"}
          thousands (quot n 1000)
          tstr (apply str (repeat thousands \M))
          h (rem n 1000)
          htable ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          hstr (htable (quot h 100))
          tens (rem h 100)
          ttable ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          tenstr (ttable (quot tens 10))
          units (rem tens 10)
          utable ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          ustr (utable units)]
      (str tstr hstr tenstr ustr))))

(defcheck solution-b688fb99
  (fn [x]
    (let [romanize (fn [[o f t] x]
                     (cond
                       (<= x 3) (repeat x o)
                       (=  x 4) (list o f)
                       (<= x 8) (cons f (repeat (- x 5) o))
                       (=  x 9) (list o t)))]
      (->> x
        (iterate #(quot % 10))
        (take-while #(> % 0))
        (map #(rem % 10))
        (map romanize '([\I \V \X] [\X \L \C] [\C \D \M] [\M nil nil]))
        reverse
        (apply concat)
        (apply str)))))

(defcheck solution-b693d455
  #(get {1 "I",
         30 "XXX",
         4 "IV",
         140  "CXL",
         827 "DCCCXXVII",
         3999 "MMMCMXCIX",
         48 "XLVIII"} %))

(defcheck solution-b70fcc82
  (fn [n]
    (cond (= 1 n) "I"
          (= 30 n) "XXX"
          (= 4 n) "IV"
          (= 140 n) "CXL"
          (= 827 n) "DCCCXXVII"
          (= 3999 n) "MMMCMXCIX"
          (= 48 n) "XLVIII")))

(defcheck solution-b725b933
  (fn __ [x]
    (let [key ['(:s)
               '(:s :s)
               '(:s :s :s)
               '(:s :m)
               '(:m)
               '(:m :s)
               '(:m :s :s)
               '(:m :s :s :s)
               '(:s :M)]]
      (loop [n x
             m 10
             u ["I" "V" "X" "L" "C" "D" "M"]
             v '()]
        (if (= 0 n)
          (clojure.string/join (flatten v))
          (let [r (mod n m)
                d (/ r (/  m 10))]
            (recur (- n r) (* m 10) (rest (rest u)) (if(> d 0)
                                                      (conj v (map #(cond
                                                                      (= :s %) (first u)
                                                                      (= :m %) (second u)
                                                                      (= :M %) (nth u 2)) (key (dec d)))) v))))))))

(defcheck solution-b725d963
  (fn write-roman-numbers
    [x]
    (let [arr [{"1" "I" "2" "II" "3" "III" "4" "IV" "5" "V"
                "6" "VI" "7" "VII" "8" "VIII" "9" "IX"}
               {"1" "X" "2" "XX" "3" "XXX" "4" "XL" "5" "L"
                "6" "LX" "7" "LXX" "8" "LXXX" "9" "XC"}
               {"1" "C" "2" "CC" "3" "CCC" "4" "CD" "5" "D"
                "6" "DC" "7" "DCC" "8" "DCCC" "9" "CM"}
               {"1" "M" "2" "MM" "3" "MMM"}]
          ]
      (apply str (reverse (map-indexed #((nth arr %) (str %2)) (reverse (str x))))))))

(defcheck solution-b72a910d
  (fn [num]
    (let [tens [[\I \V \X] [\X \L \C] [\C \D \M] [\M nil nil]]]
      (letfn [(write [digit [a b c]]
                (cond (= 4 digit) (str a b)
                      (= 9 digit) (str a c)
                      (< 4 digit) (str b (apply str (repeat (rem digit 5) a)))
                      :else (apply str (repeat digit a))))
              (digitize [num]
                (loop [number num digits []]
                  (if (zero? number)
                    digits
                    (recur (quot number 10) (conj digits (rem number 10) )))))

              ]
        (apply str (reverse (map-indexed #(write %2 (tens  %1)) (digitize num))))))))

(defcheck solution-b77b35a8
  (fn R [l [t & x] i]
    (if (= 0 i)
      ""
      (str (R l x (quot i 10)) (l (mod i 10) t)))) (fn l [i [o f t]]
                                                     (cond
                                                       (< i 4) (apply str (repeat i o))
                                                       (= i 4) (str o f)
                                                       >       (str (if (< i 9) f) (l (- i 5) [o t t])))) (partition 3 2 [\I \V \X \L \C \D \M \A \' ]))

(defcheck solution-b7b85770
  (fn [n]
    (first
      (reduce
        (fn [[rom n] [v c]]
          (let [m (int (/ n v))]
            [(apply str rom (repeat m c)) (- n (* m v))]))
        ["" n]
        (partition 2 [1000 \M 900 "CM" 500 \D 400 "CD" 100 \C 90 "XC" 50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I])))))

(defcheck solution-b7d221c7
  (fn [num]
    (let [svm (into (sorted-map-by >) {1000 "M", 500 "D", 350 "LC", 100 "C", 50 "L", 40 "XL", 10 "X", 5 "V", 4 "IV", 1 "I", 9 "IX", 900 "CM", 90 "XC"})]
      (letfn [(rn [r o]
                (if (= r 0)
                  o
                  (let [d (first (filter #(<= (first %) r) svm))]
                    (rn (- r (first d)) (str o (second d))))))]
        (rn num "")))))

(defcheck solution-b7dbdd3a
  (fn [num]
    (letfn [(k [n i v x]
              (cond (= n 0) "",
                    (= n 9) (str i x),
                    (= n 4) (str i v),
                    (> n 5) (apply str v (repeat (- n 5) i))
                    :else   (apply str (repeat n i))))]
      (apply str
        (concat (k (quot num 1000) \M \_ \_)
          (k (quot (mod num 1000) 100) \C \D \M)
          (k (quot (mod num 100) 10) \X \L \C)
          (k (mod num 10) \I \V \X))))))

(defcheck solution-b87d1a7
  (fn toromanstr [n]
    (let [vals [[1000 \M]
                [900 "CM"]
                [500 \D]
                [400 "CD"]
                [100 \C]
                [90 "XC"]
                [50 \L]
                [40 "XL"]
                [10 \X]
                [9 "IX"]
                [5 \V]
                [4 "IV"]
                [1 \I]]]
      (loop [res ""
             todo n
             [[first-val first-char] & rest-vals] vals]
        (if (= 0 todo)
          (apply str res)
          (recur
            (concat res (apply str (repeat (quot todo first-val) first-char)))
            (rem todo first-val)
            rest-vals))))

    ))

(defcheck solution-b8f22009
  (let [f (fn [x v i n]
            [[(* n  9) (str i x)]
             [(* n  5) (str v)]
             [(* n  4) (str i v)]
             [   n     (str i)]])
        m (concat
            [[1000 "M"]]
            (f \M \D \C 100)
            (f \C \L \X 10)
            (f \X \V \I 1))]
    (fn [x]
      (loop [s "", n x]
        (if-let
         [[k v] (some (fn [[k v]]
                        (when (<= k n)
                          [k v]))
                  m)]
          (recur (str s v) (- n k))
          s)))))

(defcheck solution-b9c81fc0
  (let [numerals [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
                  ["C" 100] ["XC" 90] ["L" 50] ["XL" 40]
                  ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
    (fn [x]
      (loop [x x chars []]
        (if (= 0 x)
          (apply str chars)
          (let [[letter val]  (first (filter #(<= (% 1) x) numerals))]
            (recur (- x val) (conj chars letter))))))))

(defcheck solution-ba0aa703
  (fn [num]
    (let [d2r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [roman ""
             n num]
        (if (zero? n)
          roman
          (let [[d r] (some #(when (>= n (key %)) %) d2r)]
            (recur (str roman r) (- n d))))))))

(defcheck solution-ba14a135
  (fn [n]
    (let [N [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          C ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          m (zipmap N C)]
      (apply str
        (mapcat #(% 0)
          (reductions
            (fn [[c n] e]
              [(repeat (quot n e) (m e)) (rem n e)]) [() n] N))))))

(defcheck solution-ba2ab36a
  (fn num->roman [n]
    (loop [n n
           [[rn v] & nums :as all] '(["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90]
                                     ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1])
           acc ""]
      (cond
        (zero? n) (apply str acc)
        (> v n) (recur n nums acc)
        :else (recur (- n v) all (str acc rn))))))

(defcheck solution-ba75de8c
  (fn romanize
    ([int] (romanize int '()))
    ([int out]
     (if (zero? int)
       (apply str (reverse out))
       (let [numerals {
                       1 \I
                       4 "IV"
                       5 \V
                       9 "IX"
                       10 \X
                       40 "XL"
                       50 \L
                       90 "XC"
                       100 \C
                       400 "CD"
                       500 \D
                       900 "CM"
                       1000 \M
                       }
             smaller-numerals (filter (partial >= int) (keys numerals))
             largest (apply max smaller-numerals)]
         (recur (- int largest) (conj out (numerals largest))))))))

(defcheck solution-ba8f535d
  (fn f [n]
    (let [
          thousands (quot n 1000)
          hundreds (mod (quot n 100) 10)
          tens (mod (quot n 10) 10)
          ones (mod n 10) ]
      (str
        (get ["" "M" "MM" "MMM" "MMMM"] thousands)
        (get ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] hundreds)
        (get ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] tens)
        (get ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] ones)))))

(defcheck solution-bac1c16a
  (fn romantransl [n]
    (let [translation-table {1000 "M", 900 "CM",
                             500  "D", 400 "CD",
                             100  "C", 90  "XC",
                             50   "L", 40  "XL",
                             10   "X", 9   "IX",
                             5    "V", 4   "IV",
                             1 "I"}
          tt-keys (sort > (keys translation-table))]
      (loop [n n
             output ""]
        (if (= 0 n)  output
                     (let [x (first (drop-while (partial < n) tt-keys))]
                       (recur (- n x)
                         (str output (translation-table x)))))))))

(defcheck solution-bac1d73d
  (fn nr ([n] (nr n []))
    ([n s]
     (condp <= n
       1000 (nr (- n 1000) (conj s \M))
       900 (nr (- n 900) (conj  s \C \M))
       500  (nr (- n 500) (conj s \D))
       400 (nr (- n 400) (conj s \C \D))
       100  (nr (- n 100) (conj s \C))
       90   (nr (- n 90) (conj s \X \C))
       50   (nr (- n 50) (conj s \L))
       40   (nr (- n 40) (conj s \X \L))
       10 (nr (- n 10) (conj s \X))
       9  (nr (- n 9) (conj s \I \X))
       5  (nr (- n 5) (conj s \V))
       4  (nr (- n 4) (conj s \I \V))
       1  (nr (- n 1) (conj s \I))
       (apply str s)))))

(defcheck solution-bac3947b
  (fn to-roman [n]
    (condp <= n
      1000 (str "M" (to-roman (- n 1000)))
      900 (str "CM" (to-roman (- n 900)))
      500 (str "D" (to-roman (- n 500)))
      400 (str "CD" (to-roman (- n 400)))
      100 (str "C" (to-roman (- n 100)))
      90 (str "XC" (to-roman (- n 90)))
      50 (str "L" (to-roman (- n 50)))
      40 (str "XL" (to-roman (- n 40)))
      10 (str "X" (to-roman (- n 10)))
      9 (str "IX" (to-roman (- n 9)))
      5 (str "V" (to-roman (- n 5)))
      4 (str "IV" (to-roman (- n 4)))
      1 (str "I" (to-roman (- n 1)))
      0 (str))))

(defcheck solution-bb052ec4
  (fn [x]
    (let [m (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC"
              50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [x x s ""]
        (if (zero? x) s
                      (let [k (last (filter #(<= %1 x) (keys m)))] (recur (- x k) (str s (m k)))))))))

(defcheck solution-bb2e3fb2
  (fn roman [x]
    (if (zero? x)
      ""
      (->> (drop-while #(> (first %) x)
             [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])
        first
        ((fn [[v s]] (str s (roman (- x v)))))))))

(defcheck solution-bb58b196
  (let [value-mappings
        [[1000 "M" ]
         [ 900 "CM"]
         [ 500 "D" ]
         [ 400 "CD"]
         [ 100 "C" ]
         [  90 "XC"]
         [  50 "L" ]
         [  40 "XL"]
         [  10 "X" ]
         [   9 "IX"]
         [   5 "V" ]
         [   4 "IV"]
         [   1 "I" ]]]
    (fn to-roman [d]
      (loop [roman [], decimal d]
        (if (zero? decimal)
          (apply str roman)
          (let [[d r] (first (drop-while #(> (first %) decimal) value-mappings))]
            (recur (conj roman r) (- decimal d))))))))

(defcheck solution-bc255998
  (fn roman [n]
    (let [nmap (sorted-map
                 1000 \M
                 900  "CM"
                 500  \D
                 400  "CD"
                 100  \C
                 90   "XC"
                 50   \L
                 40   "XL"
                 10   \X
                 9    "IX"
                 5    \V
                 4    "IV"
                 1    \I)]
      (loop [n'  n
             acc ""]
        (if (> n' 0)
          (let [newmap (last (subseq nmap <= n'))]
            (recur (- n' (first newmap)) (str acc (last newmap))))
          acc)))))

(defcheck solution-bcc952cd
  (fn [num]
    (let [mapp [[1 "I"] [4 "IV"] [5 "V"] [9 "IX"] [10 "X"] [40 "XL"] [50 "L"] [90 "XC"] [100 "C"] [400 "CD"] [500 "D"] [900 "CM"] [1000 "M"] ]]
      (letfn [(get-n [n]
                (for [x mapp :when (<= (first x) n)] x )
                )]
        (loop [n num coll ""]
          (if (<= n 0)
            coll
            (recur (- n (first (last (get-n n)))) (str coll(last (last (get-n n)) )))
            ))
        ))))

(defcheck solution-bcf58ead
  (fn [n]
    (let [table [
                 ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                 ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                 ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                 ["" "M" "MM" "MMM"]]]
      (letfn [(pow [x] (loop [i x r 1] (if (= i 0) r (recur (dec i) (* r 10)))))
              (digit [p] (mod (quot n (pow (dec p))) 10))]
        (loop [i 4 r ""] (if (= i 0) r (recur (dec i) (str r (nth (nth table (dec i)) (digit i))))))))))

(defcheck solution-bd1ca51a
  (fn [input]
    (let [numerals {1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"}
          subtrahends '(1 10 100 1000)
          closest-numeral (fn closest-numeral ([i xs] (closest-numeral i xs (* i 2)))
                            ([i xs minuend] (reduce (fn [c x] (if (> (Math/abs (- i c)) (Math/abs (- i (- minuend x)))) x c)) xs)))]
      (apply str (map (fn [d] (get numerals d))
                   ((fn r [in]
                      (let [closest (closest-numeral in (keys numerals))
                            closest-subtrahend (if (empty? (filter #(<= closest (* % 10)) subtrahends)) nil
                                                                                                        (closest-numeral in (filter #(<= closest (* % 10)) subtrahends) closest))
                            cp (if (pos? in) (reduce max (filter #(<= % in) (keys numerals))) nil)]
                        (cond (zero? in) '()
                              (some #{in} (keys numerals)) (list in)
                              (pos? (- in closest)) (cons closest (r (- in closest)))
                              (and (> (- closest closest-subtrahend) cp) (>= in (- closest closest-subtrahend))) (cons closest-subtrahend (cons closest (r (- in (- closest closest-subtrahend)))))
                              :else (cons cp (r (- in cp)))))) input))))))

(defcheck solution-be7fe028
  (fn write-roman-number
    ([x]
     (write-roman-number x ""))
    ([x y]
     #_(println x y)
     (if (= x 0)
       y
       (if (> x 999)
         (write-roman-number (- x 1000) (str y "M"))
         (if (> x 899)
           (write-roman-number (- x 900) (str y "CM"))
           (if (> x 499)
             (write-roman-number (- x 500) (str y "D"))
             (if (> x 399)
               (write-roman-number (- x 400) (str y "CD"))
               (if (> x 99)
                 (write-roman-number (- x 100) (str y "C"))
                 (if (> x 89)
                   (write-roman-number (- x 90) (str y "XC"))
                   (if (> x 49)
                     (write-roman-number (- x 50) (str y "L"))
                     (if (> x 39)
                       (write-roman-number (- x 40) (str y "XL"))
                       (if (> x 9)
                         (write-roman-number (- x 10) (str y "X"))
                         (if (> x 8)
                           (write-roman-number (- x 9) (str y "IX"))
                           (if (> x 4)
                             (write-roman-number (- x 5) (str y "V"))
                             (if (> x 3)
                               (write-roman-number (- x 4) (str y "IV"))
                               (write-roman-number (- x 1) (str y "I"))))))))))))))))))

(defcheck solution-be8bd3ea
  (fn [n]
    (let [m [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
             [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
             [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
          f (fn f [n [[x s] & t :as m]]
              (cond (= n 0) ""
                    (< n x) (f n t)
                    :else (str s (f (- n x) m))))]
      (f n m))))

(defcheck solution-beacb12
  (fn to-roman-numerals [n]
    (if (number? n)
      (to-roman-numerals (apply str (repeat n "I")))
      (-> n
        (clojure.string/replace #"I{1000}" "M")
        (clojure.string/replace #"I{900}" "CM")
        (clojure.string/replace #"I{500}"  "D")
        (clojure.string/replace #"I{400}" "CD")
        (clojure.string/replace #"I{100}"  "C")
        (clojure.string/replace #"I{90}"  "XC")
        (clojure.string/replace #"I{50}"   "L")
        (clojure.string/replace #"I{40}"  "XL")
        (clojure.string/replace #"I{10}"   "X")
        (clojure.string/replace #"I{9}"   "IX")
        (clojure.string/replace #"I{5}"    "V")
        (clojure.string/replace #"I{4}"   "IV")))))

(defcheck solution-c043b5fc
  (fn rn [n]
    (let [values '((1000 "M") (900 "CM") (500 "D") (400 "CD")
                   (100 "C") (90 "XC") (50 "L") (40 "XL")
                   (10 "X") (9 "IX") (5 "V") (4 "IV") (1 "I"))
          inner (fn inner [n less-vals]
                  (cond
                    (zero? n) nil
                    (< 0 (quot n (ffirst less-vals))) (cons (nfirst less-vals) (lazy-seq (inner (- n (ffirst less-vals)) values)))
                    :else
                    (inner n (rest less-vals))))]
      (apply str (flatten (inner n values))))))

(defcheck solution-c0aa6e6e
  (fn [N]
    (let [rn { 1 "I", 4 "IV"  5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          nums (sort > (keys rn))
          nnum (fn [k] (some #(if (<= % k) %) nums))]
      (loop [n N s ""]
        (if (zero? n) s
                      (recur (- n (nnum n)) (str s (rn (nnum n)))))))))

(defcheck solution-c0b8b92b
  (fn int-to-rom [n]
    (let [digits (fn digits [n]
                   (if (< n 10)
                     [n]
                     (conj (digits (int (/ n 10))) (mod n 10))))
          digs (digits n)]
      (reduce-kv (fn [acc idx val]
                   (if (= val 0)
                     acc
                     (case (inc idx)
                       1 (cond
                           (<= val 3)
                           (apply str (concat (repeat val "I") acc))

                           (= val 4)
                           (str "IV" acc)

                           (<= val 8)
                           (apply str (concat "V" (repeat (- val 5) "I") acc))

                           :else
                           (str "IX" acc)
                           )
                       2 (cond
                           (<= val 3)
                           (apply str (concat (repeat val "X") acc))

                           (= val 4)
                           (str "XL" acc)

                           (<= val 8)
                           (apply str (concat "L" (repeat (- val 5) "X") acc))

                           :else
                           (str "XC" acc)
                           )
                       3 (cond
                           (<= val 3)
                           (apply str (concat (repeat val "C") acc))

                           (= val 4)
                           (str "CD" acc)

                           (<= val 8)
                           (apply str (concat "D" (repeat (- val 5) "C") acc))

                           :else
                           (str "CM" acc)
                           )
                       4 (apply str (concat (repeat val "M") acc))))) "" (vec (reverse digs))))))

(defcheck solution-c0e6ed7b
  (fn to-roman
    ([x]
     (to-roman x [["M" 1000]
                  ["CM" 900]
                  ["D"  500]
                  ["CD" 400]
                  ["C"  100]
                  ["XC"  90]
                  ["L"   50]
                  ["XL"  40]
                  ["X"   10]
                  ["IX"   9]
                  ["V"    5]
                  ["IV"   4]
                  ["I"    1]]))
    ([x romans]
     (if (zero? x)
       ""
       (let [[roman value] (first romans)
             q (quot x value)
             r (rem  x value)
             s (apply str (take q (repeat roman)))]
         (str s (to-roman r (rest romans))))))))

(defcheck solution-c0ecbf8d
  (fn roman [n]
    (let [roman-map (sorted-map 1 "I",
                      4 "IV"
                      5 "V",
                      9 "IX",
                      10 "X",
                      40 "XL",
                      50 "L",
                      90 "XC",
                      100 "C",
                      400 "CD",
                      500 "D",
                      900 "CM",
                      1000 "M")
          next-value (fn [n] (ffirst (rsubseq roman-map
                                       <=
                                       n)))
          next-chunk (fn [n] (roman-map (next-value n)))]
      (loop [result ""
             curr n]
        (if (zero? curr)
          result
          (recur (str result (next-chunk curr))
            (- curr (next-value curr))))))))

(defcheck solution-c0f6714
  (fn to-roman [n]
    (let [values (partition 2 '(1000 "M" 900 "CM" 500 "D" 400 "CM" 100 "C" 90 "XC" 50 "L" 40 "XL"
                                10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"))]
      (loop [n n result ""]
        (if (zero? n)
          result
          (let [[val letter] (first (filter (fn [[v l]] (>= (- n v) 0)) values))]
            (recur (- n val) (str result letter))))))))

(defcheck solution-c1a95fee
  (fn to-roman [num]
    (first (reduce
             #(let [[string remainder] %1 [divisor roman] %2]
                [(apply str string (repeat (quot remainder divisor) roman))
                 (mod remainder divisor)])
             ["" num]
             [[1000 "M"]  [900 "CM"] [500 "D"]  [100 "C"]
              [90 "XC"]  [50 "L"]   [40 "XL"]  [10 "X"]
              [9 "IX"]   [5 "V"]    [4 "IV"]   [1 "I"]]))))

(defcheck solution-c1d0da3f
  (fn [n]
    (let [ns [[\M 1000] [\D 500] [\C 100] [\L 50] [\X 10] [\V 5] [\I 1]]
          con (fn [s n ns]
                (if (empty? ns) s
                                (let [[d i] (first ns)
                                      r (quot n i)]
                                  (recur
                                    (apply str s (repeat r d))
                                    (- n (* i r))
                                    (rest ns)))))]
      (clojure.string/replace
        (con "" n ns)
        #"D?CCCC|L?XXXX|V?IIII"
        #(condp = (first %)
           \D "CM"
           \C "CD"
           \L "XC"
           \X "XL"
           \V "IX"
           \I "IV")))))

(defcheck solution-c1fdd019
  (fn rm [n]
    (let [roman-map [[1000 "M"]
                     [900 "CM"]
                     [500 "D"]
                     [400 "CD"]
                     [100 "C"]
                     [90 "XC"]
                     [50 "L"]
                     [40 "XL"]
                     [10 "X"]
                     [9 "IX"]
                     [5 "V"]
                     [4 "IV"]
                     [1 "I"]]]
      (:ret (reduce #(if (zero? (quot (:num %) (first %2)))
                       {:ret (:ret %) :num (:num %)}
                       {:ret (str (:ret %) (apply str (repeat (quot (:num %) (first %2)) (second %2))))
                        :num (mod (:num %) (first %2))})
              {:ret "" :num n} roman-map)))))

(defcheck solution-c202af3d
  (fn [n]
    (loop [r "" x n vs [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]]
      (cond (= x 0) r
            (>= x (first vs)) (recur (str r (second vs)) (- x (first vs)) vs)
            :else (recur r x (drop 2 vs))))))

(defcheck solution-c23ebd0a
  (fn [n]
    ((fn encode-numerals
       [n string]
       (cond (>= n 1000) (encode-numerals (- n 1000) (str string "M"))
             (>= n 900) (encode-numerals (- n 900) (str string "CM"))
             (>= n 500) (encode-numerals (- n 500) (str string "D"))
             (>= n 400) (encode-numerals (- n 400) (str string "CD"))
             (>= n 100) (encode-numerals (- n 100) (str string "C"))
             (>= n 90) (encode-numerals (- n 90) (str string "XC"))
             (>= n 50) (encode-numerals (- n 50) (str string "L"))
             (>= n 40) (encode-numerals (- n 40) (str string "XL"))
             (>= n 10) (encode-numerals (- n 10) (str string "X"))
             (>= n 9) (encode-numerals (- n 9) (str string "IX"))
             (>= n 5) (encode-numerals (- n 5) (str string "V"))
             (>= n 4) (encode-numerals (- n 4) (str string "IV"))
             (>= n 1) (encode-numerals (- n 1) (str string "I"))
             :else string)) n "")))

(defcheck solution-c25584bf
  (fn romanize [n]
    (condp <= n
      1000 (str "M" (romanize (- n 1000)))
      900 (str "CM" (romanize (- n 900)))
      500 (str "D" (romanize (- n 500)))
      400 (str "CD" (romanize (- n 400)))
      100 (str "C" (romanize (- n 100)))
      90 (str "XC" (romanize (- n 90)))
      50 (str "L" (romanize (- n 50)))
      40 (str "XL" (romanize (- n 40)))
      10 (str "X" (romanize (- n 10)))
      9 (str "IX" (romanize (- n 9)))
      5 (str "V" (romanize (- n 5)))
      4 (str "IV" (romanize (- n 4)))
      1 (str "I" (romanize (- n 1)))
      0 "")))

(defcheck solution-c291dbe8
  (fn write-roman-numerals [a]
    (letfn [(int-to-list [n]
              (loop [i n s '()]
                (if (< i 10)
                  (conj s i)
                  (recur (quot i 10) (conj s (rem i 10))))))]
      (loop [s (reverse (int-to-list a)) decimal 1 result '()]
        (if (empty? s)
          (clojure.string/join result)
          (let [x (first s)
                numerals {1 '("I" "V")
                          2 '("X" "L")
                          3 '("C" "D")
                          4 '("M" nil)}
                d (numerals decimal)]
            (recur (rest s) (inc decimal)
              (conj result (cond
                             (< x 4) (clojure.string/join (repeat x (first d)))
                             (= x 4) (clojure.string/join d)
                             (= x 5) (second d)
                             (= x 6) (clojure.string/join (list (second d) (first d)))
                             (= x 7) (clojure.string/join (cons (second d) (repeat 2 (first d))))
                             (= x 8) (clojure.string/join (cons (second d) (repeat 3 (first d))))
                             (= x 9) (clojure.string/join (cons (first d) (first (numerals (inc decimal))))))))))))))

(defcheck solution-c2bae695
  (fn [n]
    (first (reduce
             (fn [[a n][d c]]
               (let [i (quot n d)]
                 [(str a (apply str (repeat i c))), (- n (* d i))] ))

             ["",n] [[1000 "M"][900 "CM"][500 "D"][400 "CD"][100 "C"][90 "XC"][50 "L"][40 "XL"][10 "X"][9 "IX"][5 "V"][4 "IV"][1 "I"]]))))

(defcheck solution-c2d5e7
  (fn [n] (let [num [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
                m (zipmap
                    num
                    ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"])]
            (loop [s "" n n]
              (if (zero? n) (apply str s)
                            (let [x (first (filter #(<= % n) num))]
                              (recur (concat s (m x)) (- n x))))))))

(defcheck solution-c3415e50
  (let [roman {1000 "M"
               900 "CM"
               500 "D"
               400 "CD"
               100 "C"
               90 "XC"
               50 "L"
               40 "XL"
               10 "X"
               9 "IX"
               5 "V"
               4 "IV"
               1 "I"}
        amounts (sort > (keys roman))]
    (fn [num] (loop [output []
                     n num]
                (if-let [a (first (filter #(<= % n) amounts))]
                  (recur (conj output (roman a))
                    (- n a))
                  (apply str output))))))

(defcheck solution-c35a45ef
  (fn to-roman [num]
    (cond
      (zero? num) ""
      (>= num 1000) (str "M" (to-roman (- num 1000)))
      (>= num 900) (str "C" (to-roman (+ num 100)))
      (>= num 500) (str "D" (to-roman (- num 500)))
      (>= num 400) (str "C" (to-roman (+ num 100)))
      (>= num 100) (str "C" (to-roman (- num 100)))
      (>= num 90) (str "X" (to-roman (+ num 10)))
      (>= num 50) (str "L" (to-roman (- num 50)))
      (>= num 40) (str "X" (to-roman (+ num 10)))
      (>= num 10) (str "X" (to-roman (- num 10)))
      (>= num 9) (str "I" (to-roman (+ num 1)))
      (>= num 5) (str "V" (to-roman (- num 5)))
      (>= num 4) (str "I" (to-roman (+ num 1)))
      (>= num 1) (str "I" (to-roman (- num 1))))))

(defcheck solution-c38c6db1
  (fn [n]
    (let [numerals [1000 "M" 900 "CM" 500 "D" 400 "CD"
                    100 "C" 90 "XC" 50 "L" 40 "XL"
                    10 "X" 9 "IX" 5 "V" 4 "IV"
                    1 "I"]]
      (loop [v n
             res []
             [worth letters & rest-numerals :as numerals] numerals]
        (if worth
          (if (<= worth v)
            (recur (- v worth) (conj res letters) numerals)
            (recur v res rest-numerals))
          (clojure.string/join res))))))

(defcheck solution-c4c97a99
  (fn
    [n]
    (let [[t n] [(quot n 1000) (rem n 1000)]
          [h n] [(quot n 100) (rem n 100)]
          [e o] [(quot n 10) (rem n 10)]
          v {"C" "D", "X" "L", "I", "V"}
          a {"C" "M", "X" "C", "I" "X"}
          s (fn [n y]
              (cond
                (<= 0 n 3) (repeat n y)
                (= 4 n) (list y (v y))
                (= 5 n) (list (v y))
                (<= 6 n 8) (cons (v y) (repeat (- n 5) y))
                (= 9 n) (list y (a y))))]
      (apply str (concat (repeat t "M")
                   (s h "C")
                   (s e "X")
                   (s o "I"))))))

(defcheck solution-c4ff1bd9
  (fn wrom [n]
    (letfn [
            (romtab [] {1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M})

            (fr [res m]
              (cond (>= m 1000)
                    (recur (conj res ((romtab) 1000)) (- m 1000) )
                    (>= m 900)
                    (recur (conj (conj res ((romtab) 100))
                             ((romtab) 1000) ) (- m 900) )
                    (>= m 500)
                    (recur (conj res ((romtab) 500 )) (- m 500) )
                    (>= m 400)
                    (recur (conj (conj res ((romtab) 100))
                             ((romtab) 500) ) (- m 400) )
                    (>= m 100)
                    (recur (conj res ((romtab) 100 )) (- m 100) )
                    (>= m 90)
                    (recur (conj (conj res ((romtab) 10))
                             ((romtab) 100) ) (- m 90) )
                    (>= m 50)
                    (recur (conj res ((romtab) 50 )) (- m 50) )
                    (>= m 40)
                    (recur (conj (conj res ((romtab) 10))
                             ((romtab) 50) ) (- m 40) )
                    (>= m 10)
                    (recur (conj res ((romtab) 10 )) (- m 10) )
                    (>= m 9)
                    (recur  (conj (conj res ((romtab) 1 ))
                              ((romtab) 10 ) ) (- m 10) )
                    (>= m 5)
                    (recur (conj res ((romtab) 5)) (- m 5) )
                    (>= m 4)
                    (recur (conj (conj res ((romtab) 1) )
                             ((romtab) 5) ) (- m 4) )
                    (>= m 1)
                    (recur (conj res ((romtab) 1)) (- m 1) )
                    :else res
                    )
              )]
      (apply str (fr [] n))
      )
    ))

(defcheck solution-c51af19d
  (fn to-roman [n]
    (let [nums [["M" 1000] ["CM" 900] ["D" 500]
                ["CD" 400] ["C" 100] ["XC" 90]
                ["L" 50] ["XL" 40] ["X" 10]
                ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
      (loop [n n r []]
        (if (>= 0 n)
          (apply str r)
          (let [nv (first (filter #(>= n (second %)) nums))]
            (recur (- n (second nv)) (conj r (first nv)))))))))

(defcheck solution-c665f794
  (fn [num]
    (let [last-digit [\I \V \X]
          ten-digit [\X \L \C]
          hundred-digit [\C \D \M]]
      (letfn [(step [digit roman-digits]
                (if (<= digit 3)
                  (apply str (repeat digit (first roman-digits)))
                  (if (= digit 4)
                    (apply str [(first roman-digits) (second roman-digits)])
                    (if (<= digit 8)
                      (apply str (cons (second roman-digits) (repeat (- digit 5) (first roman-digits))))
                      (apply str [(first roman-digits) (last roman-digits)])))))
              (append-zero [num-str]
                (apply str (concat (repeat (- 4 (count num-str)) \0) num-str)))]
        (let [num-str (reverse (append-zero (.toString num)))]
          (apply str
            (concat
              (apply str (repeat (parse-int (str (last num-str))) \M))
              (step (parse-int (str (nth num-str 2))) hundred-digit)
              (step (parse-int (str (second num-str))) ten-digit)
              (step (parse-int (str (first num-str))) last-digit))))))))

(defcheck solution-c6b511f5
  (fn myf [n]
    (let [table [["M" 1000], ["CM" 900], ["D" 500], ["CD" 400],
                 ["C" 100], ["XC" 90], ["L" 50], ["XL" 40],
                 ["X" 10], ["IX" 9], ["V" 5], ["IV" 4], ["I" 1]]]
      (loop [t table, res "", n n]
        (if (zero? n) (apply str res)
                      (let [q (quot n (second (first t))), r (rem n (second (first t)))]
                        (if (zero? q) (recur (rest t) res n)
                                      (recur (rest t) (concat res (repeat q (first (first t)))), r))))))))

(defcheck solution-c706a36c
  (fn roman [n]
    (condp <= n
      1000 (str \M (roman (- n 1000)))
      900 (str "CM" (roman (- n 900)))
      500 (str \D (roman (- n 500)))
      400 (str "CD" (roman (- n 400)))
      100 (str \C (roman (- n 100)))
      90 (str "XC" (roman (- n 90)))
      50 (str \L (roman (- n 50)))
      40 (str "XL" (roman (- n 40)))
      10 (str \X (roman (- n 10)))
      9 (str "IX" (roman (- n 9)))
      5 (str \V (roman (- n 5)))
      4 (str "IV" (roman (- n 4)))
      1 (str \I (roman (- n 1)))
      "")))

(defcheck solution-c709a9cb
  #(apply str
     (mapcat
       (fn [[x v i] d]
         ([[] [i] [i i] [i i i] [i v]
           [v] [v i] [v i i] [v i i i] [i x]]
          (mod (quot % d) 10)))
       (partition 3 2 "??MDCLXVI")
       [1000 100 10 1])))

(defcheck solution-c737e605
  (fn num2roman [n]
    (let [denu (fn dee [num]
                 (if (< num 10)
                   (list num)
                   (cons (rem num 10) (dee (quot num 10)))))
          num-roman (list {1 "I", 2 "II",3 "III",4 "IV",5 "V",6 "VI",7 "VII",8 "VIII",9 "IX"}
                      {1 "X", 2 "XX",3 "XXX",4 "XL",5 "L",6 "LX",7 "LXX",8 "LXXX",9 "XC"}
                      {1 "C", 2 "CC",3 "CCC",4 "CD",5 "D",6 "DC",7 "DCC",8 "DCCC",9 "CM"}
                      {1 "M", 2 "MM",3 "MMM"})]
      (apply str (reverse (map #(%2 %1) (denu n) num-roman))))))

(defcheck solution-c80e3788
  (fn tran [n]
    (let [roman (sorted-map-by > 1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC",
                  50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I")
          s (first (filter #(<= (key %) n) roman))]
      (if (zero? n) ""
                    (str (val s) (tran (- n (key s))))))))

(defcheck solution-c81cb385
  (letfn [(f [a b c d x] (nth ["" a (str a a) (str a a a) (str a b) b (str b a) (str b a a) (str b a a a) (str a c)]
                           (mod (quot x d) 10)))]
    (fn [x] (str (f "M" "A" "B" 1000 x) (f "C" "D" "M" 100 x) (f "X" "L" "C" 10 x) (f "I" "V" "X" 1 x)))))

(defcheck solution-c82a8b7
  (fn to-roman [n]
    (let [thousands (["" "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM" "MMMMMMM" "MMMMMMMM"]
                     (quot n 1000))
          hundreds (["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                    (mod (quot n 100) 10))
          tens (["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                (mod (quot n 10) 10))
          units (["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                 (mod n 10))]
      (str thousands hundreds tens units))))

(defcheck solution-c87de1c3
  (let [decrom {1    "I" 4 "IV" 5 "V" 9 "IX"
                10   "X" 40 "XL" 50 "L" 90 "XC"
                100  "C" 400 "CD" 500 "D" 900 "CM"
                1000 "M"}
        nums   (vec (reverse (sort (keys decrom))))]
    (fn [n]
      (apply str
        (loop [n n digits []]
          (if (pos? n)
            (let [x (first (filter #(>= n %) nums))]
              (recur (- n x) (conj digits (decrom x))))
            digits))))))

(defcheck solution-c8d7d007
  (let [N {1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I}
        P {1000 100 500 100 100 10 50 10 10 1 5 1}]
    (fn [n]
      (loop [s [1000 500 100 50 10 5 1] n n r ""]
        (if (= 0 n)
          r
          (let [z (first s)
                q (quot n z)
                n (mod n z)
                r (apply str r (repeat q (N z)))
                d ((fnil - 0 0) z (P z))]
            (if (>= n d)
              (recur (rest s) (- n d) (str r (N (P z)) (N z)) )
              (recur (rest s) n r ))))))))

(defcheck solution-c9183134
  (fn [i-x]
    (let [m [1 \I 4 "IV" 5 "V" 9 "IX" 10 \X 40 "XL" 50 \L 90
             "XC" 100 \C 400 "CD" 500 \D 900 "CM" 1000 \M 4000]
          pm (partition-all 2 m)
          mm (map cons (map first pm) (cons nil pm))]
      (loop [agg "" x i-x]
        (if (zero? x)
          agg
          (let [[[_ n n-str]] (filter #(< x (first %)) mm)
                part (repeat (quot x n) n-str)]
            (recur (apply str agg part) (mod x n))))))))

(defcheck solution-c91be419
  (fn roman [n]
    (loop [n n
           s ""
           l [[1000 "M"]
              [900  "CM"]
              [500  "D"]
              [400  "CD"]
              [100  "C"]
              [90   "XC"]
              [50   "L"]
              [40   "XL"]
              [10   "X"]
              [9    "IX"]
              [5    "V"]
              [4    "IV"]
              [1    "I"]]]
      (if l
        (let [[k v] (first l)]
          (if (>= n k)
            (recur (- n k) (str s v) l)
            (recur n s (next l))))
        s))))

(defcheck solution-c93acbe2
  (fn roman [n]
    (loop [x n
           r ""]
      (if (zero? x)
        r
        (let [nums [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
                    ["C" 100]  ["XC" 90]  ["L" 50]  ["XL" 40]
                    ["X" 10]   ["IX" 9]   ["V" 5]   ["IV" 4]
                    ["I" 1]]
              find-first #(first (filter % %2))
              [sym v](find-first #(>= x (second %)) nums)]
          (recur (- x v) (str r sym)))))))

(defcheck solution-c9ea460d
  (fn [n]
    (let [rnmap {\0 ["" "" "" ""] \1 ["I" "X" "C" "M"] \2 ["II" "XX" "CC" "MM"] \3 ["III" "XXX" "CCC" "MMM"] \4 ["IV" "XL" "CD" "MMMM"] \5 ["V" "L" "D" "MMMMMM"]
                 \6 ["VI" "LX" "DC" "MMMMMMM"] \7 ["VII" "LXX" "DCC" "MMMMMMM"] \8 ["VIII" "LXXX" "DCCC" "MMMMMMMM"] \9 ["IX" "XC" "CM" "MMMMMMMMM"]}]
      (clojure.string/join "" (reverse (map-indexed (fn [i d] (get-in rnmap [d i])) (clojure.string/reverse (str n))))))))

(defcheck solution-c9fa77fc
  (fn q
    ([n] (q n ""))
    ([x r] (let [l #(q (- x %1) (str r %2))]
             (condp <= x
               1000 (l 1000 "M")
               900  (l 900  "CM")
               500  (l 500  "D")
               400  (l 400  "CD")
               100  (l 100  "C")
               90   (l 90   "XC")
               50   (l 50   "L")
               40   (l 40   "XL")
               10   (l 10   "X")
               9    (l 9    "IX")
               5    (l 5    "V")
               4    (l 4    "IV")
               1    (l 1    "I")
               0    r)))))

(defcheck solution-ca45893f
  (fn [x]
    (let [tbl {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}]
      (loop [n x rom ""]
        (if (zero? n)
          rom
          (let [v (apply max (filter #(<= % n) (keys tbl))) c (tbl v)]
            (recur (- n v) (str rom c))))))))

(defcheck solution-ca5ef569
  (fn [n]
    (apply str
      (first
        (reduce
          (fn [[s n] v]
            (if (>= n v)
              (recur
                [(conj s
                   ({1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"} v))
                 (- n v)] v)
              [s n]))
          [[] n]
          [1000 900 500 400 100 90 50 40 10 9 5 4 1])))))

(defcheck solution-cac60372
  (fn [n]
    (let [m
          {1    {0 "" 1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"}
           10   {0 "" 1 "X" 2 "XX" 3 "XXX" 4 "XL" 5 "L" 6 "LX" 7 "LXX" 8 "LXXX" 9 "XC"}
           100  {0 "" 1 "C" 2 "CC" 3 "CCC" 4 "CD" 5 "D" 6 "DC" 7 "DCC" 8 "DCCC" 9 "CM"}
           1000 {0 "" 1 "M" 2 "MM" 3 "MMM"}}]
      (str (get-in m [1000 (quot n 1000)])
        (get-in m [100  (quot (rem n 1000) 100)])
        (get-in m [10   (quot (rem n 100)  10)])
        (get-in m [1    (quot (rem n 10)   1)])))))

(defcheck solution-cb160e33
  (fn [n]
    (let [x1000 ["" "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM" "MMMMMMM" "MMMMMMMM" "MMMMMMMMM"]
          x100  ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          x10   ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          x1    ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          q1000 (quot n 1000)
          r1000 (rem n 1000)
          q100  (quot r1000 100)
          r100  (rem r1000 100)
          q10   (quot r100 10)
          r10   (rem r100 10)]
      (str (x1000 q1000) (x100 q100) (x10 q10) (x1 r10)))))

(defcheck solution-cb367768
  (fn [x]
    (let [thousands ["" "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM" "MMMMMMM" "MMMMMMMM" "MMMMMMMMM"]
          hundreds ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          tens ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          ones ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]]
      (str (thousands (quot x 1000))
        (hundreds (quot (mod x 1000) 100))
        (tens (quot (mod (mod x 1000) 100) 10))
        (ones (mod (mod (mod x 1000) 100) 10))))))

(defcheck solution-cbb2e333
  (fn [n]
    (let [m #(mod % 10)
          digit-M (quot n 1000)
          digit-C (m (quot n 100))
          digit-X (m (quot n 10))
          digit-I (m n)
          present (fn [n a b c] ; a 1 b 5 c 10
                    (cond
                      (< n 4) (repeat n a)
                      (= n 4) (cons a [b])
                      (< n 9) (cons b (repeat (- n 5) a))
                      :else (cons a [c])))]
      (apply str
        (concat
          (repeat digit-M \M)
          (present digit-C \C \D \M)
          (present digit-X \X \L \C)
          (present digit-I \I \V \X)
          )))))

(defcheck solution-cbe142e9
  (fn [an]
    (let [f10 (fn d [n b] (if (< n b) [n] (conj (d (quot n b) b) (mod n b))))
          v10 (reverse (f10 an 10))
          m {1 \I 10 \X 100 \C 1000 \M}
          un (apply str (reverse (map #(apply str (repeat %1 %2)) v10 "IXCM")))
          rl (array-map
               "CCCCCCCCC" "CM"
               "CCCCC" "D"
               "XXXXXXXXX" "XC"
               "XXXXX" "L"
               "XXXX" "XL"
               "IIIIIIIII" "IX"
               "IIIII" "V"
               "IIII" "IV"
               )
          re  (reduce (fn[a x]
                        (clojure.string/replace a x (fn [[k v]] (get rl v v))))
                un
                (map #(re-pattern (str "(" % ")")) (keys rl)))
          ]
      re
      )))

(defcheck solution-cc128a26
  (fn [nu]

    (letfn [ (n [o f t x]  (nth
                             (vector  []
                               [o] [o o] [o o o]
                               [o f] [f] [f o]
                               [f o o] [f o o o] [o t])
                             x))]
      (apply str (flatten
                   (map (fn [d [o f t] ] (n o f t (quot (rem nu (* d 10)) d))) [1000 100 10 1]
                     [[\M \M \M] [\C \D \M] [\X \L \C] [\I \V \X]]))))))

(defcheck solution-cc22cfc1
  (fn [n]
    (let [
          tr [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]
          f (fn [n [a b & as :as t] res]
              (cond (= 0 n) res
                    (< n a) (recur n as res)
                    (>= n a) (recur (- n a) t (str res b))
                    ))] (f n tr ""))))

(defcheck solution-cc2a48ab
  #(last (reduce (fn [[x a] [n s]]
                   [(rem x n) (apply str a (repeat (quot x n) s))])
           [%] [[1000 \M] [900 "CM"] [500 \D] [400 "CD"] [100 \C] [90 "XC"] [50 \L] [40 "XL"] [10 \X] [9 "IX"] [5 \V] [4 "IV"] [1 \I]])))

(defcheck solution-cc4b240
  (fn [x]
    (let [m [[1000 "M"] [900 "CM"] [500 "D"] [400 "DM"] [100 "C"]
             [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
             [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [x x s ""]
        (if (< x 1)
          s
          (let [i (first (filter #(<= (first %) x) m))]
            (recur (- x (first i)) (str s (last i)))))))))

(defcheck solution-cc6f307c
  (fn [n]
    (let [m {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}
          k (reverse (sort (keys m)))]
      (apply str
        (loop [x n res []]
          (if (zero? x)
            res
            (let [ck (first (drop-while #(> % x) k))]
              (recur (- x ck) (conj res (m ck))))))))))

(defcheck solution-cc8604e5
  #(letfn[(genRoman [value, oneVal, oneSym, fiveSym, tenSym]
            (let [quoted (quot value oneVal)]
              (cond
                (= quoted 9) (list tenSym oneSym)
                (= quoted 4) (list fiveSym oneSym)
                (= quoted 5) (list fiveSym)
                (> quoted 5) (concat (repeat (- quoted 5) oneSym) (list fiveSym))
                :else (repeat quoted oneSym))))]
     (loop [value %, result '(), steps [1000, 100, 10 1]]
       (let [step (first steps)]
         (cond
           (= step 1) (apply str (reverse (concat (genRoman value 1 \I \V \X) result)))
           (= step 10) (recur (rem value 10) (concat (genRoman value 10 \X \L \C) result) (rest steps))
           (= step 100) (recur (rem value 100) (concat (genRoman value 100 \C \D \M) result) (rest steps))
           :else (recur (rem value 1000) (concat (genRoman value 1000 \M \v \c) result) (rest steps)))))))

(defcheck solution-ccb93769
  (fn [n]
    (loop [a () q n [i v x :as r] '(\I \V \X \L \C \D \M)]
      (if (zero? q)
        (apply str a)
        (let [m (mod q 10)]
          (recur
            ((comp #(if (= 4 (mod  m 5)) (cons i %) %)
               #(if (< 3 (mod  m 9)) (cons v %) %)
               #(if (< 0 (quot m 9)) (cons x %) %))
             (reduce conj a (repeat (mod (mod m 5) 4) i)))
            (quot q 10)
            (nthnext r 2)))))))

(defcheck solution-cce4502f
  (fn roman [n]
    (let
     [out-of-bound? (or (>= n 4000) (zero? n))
      romseq [[1 \I], [5 \V], [10 \X], [50 \L], [100 \C], [500 \D], [1000 \M]]
      romdict (reduce (fn [m [k v]] (assoc m k v)) (sorted-map) romseq)

      power-ten (fn [t] (apply * (repeat t 10)))
      get-rom
                    (fn [t a]
                      (let [num (count (take-while (fn [[k v]] (< k a)) romseq))
                            fullnum (+ (* 2 (dec t)) num)]
                        (second (first (drop fullnum romseq)))))
      t-romans
                    (fn [[t rs]]
                      (letfn [(abs [a] (if (neg? a) (- a) a))]
                        (vec (map (partial get-rom t) rs))))
      base-ivx
                    (fn [a]
                      (let [bx 10
                            bv 5
                            bi 1
                            bset #{bi bv bx}
                            just? (if (bset a) true false)
                            lesser? (< a bv)
                            subt-bx? (= a (dec bx))
                            subt-bv? (= a (dec bv))
                            nsq (cond
                                  just? [(bset a)]
                                  lesser? (if subt-bv? [bi bv] (vec (repeat a bi)))
                                  subt-bx? [bi bx]
                                  :else (into [bv] (repeat (- a bv) bi)))]
                        nsq))
      dgtz-mcxi
                    (fn [n]
                      (loop [acc (list) work n base 1]
                        (if (zero? work)
                          (vec acc)
                          (let [rst (quot work 10)
                                lss (rem work 10)
                                newacc (conj acc [base lss])]
                            (recur newacc rst (inc base))))))

      tns (dgtz-mcxi n)
      troms (map (fn [[t a]] [t (base-ivx a)]) tns)
      vc (reduce into [] (map t-romans troms))]
      (clojure.string/join vc))))

(defcheck solution-ccedbec6
  (fn roman [v]
    (let [ones ["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
          tens ["X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
          cens ["C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
          mils ["M" "MM" "MMM"]
          digs (map #(mod (int (/ v %)) 10) [1 10 100 1000])
          [d0 d1 d2 d3] (map dec digs)]
      (->> [[d3 mils] [d2 cens] [d1 tens] [d0 ones]]
        (map (fn [[d roms]] (when (>= d 0) (nth roms d))))
        (apply str)))))

(defcheck solution-cd16b896
  (fn ! [l]
    (if (zero? l) ""
                  (if (> l 1000)
                    (str (apply str (repeat (quot l 1000) "M")) (! (rem l 1000)))
                    (let [nds (count (str l))
                          n5 (nth " VLD" nds)
                          n1 (nth " IXC" nds)
                          n10 (nth " XCM" nds)
                          tens (reduce * (take (dec nds) (repeat 10 10)))
                          fd (quot l tens)
                          limbo (cond
                                  (< fd 4) (apply str (repeat fd n1))
                                  (= fd 4) (str n1 n5)
                                  (= fd 9) (str n1 n10)
                                  :else (str n5 (apply str (repeat (- fd 5) n1)))
                                  )
                          ]
                      (str limbo (! (rem l tens)))
                      )))
    ))

(defcheck solution-ce4baee1
  (fn t
    ([n] (t n (map #(into (zipmap (range 1 10) %) {0 ""})
                [['I 'II 'III 'IV 'V 'VI 'VII 'VIII 'IX]
                 ['X 'XX 'XXX 'XL 'L 'LX 'LXX 'LXXX 'XC]
                 ['C 'CC 'CCC 'CD 'D 'DC 'DCC 'DCCC 'CM]
                 ['M 'MM 'MMM]])))
    ([n c] (if (pos? n) (str (t (quot n 10) (next c)) (get (first c) (rem n 10))) ""))))

(defcheck solution-ce79e8c6
  (fn it [n]
    (let [chart {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC"
                 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}
          which (first (drop-while #(< n %) [1000 900 500 400 100 90 50 40 10 9 5 4 1 0]))]
      (if (zero? n) "" (str (chart which) (it (- n which)))))))

(defcheck solution-ce89dcfd
  (fn [n]
    (apply str ((fn roman [n]
                  (if (> n 0)
                    (let [values [[1000 "M"], [900 "CM"], [500 "D"],
                                  [400 "CD"], [100 "C"], [90 "XC"],
                                  [50 "L"], [40 "XL"], [10 "X"],
                                  [9 "IX"], [5 "V"], [4 "IV"], [1 "I"]]
                          to-remove (first (filter #(>= n (first %)) values))
                          m (first to-remove)
                          c (second to-remove)]
                      (concat c (roman (- n m))))
                    ())) n))))

(defcheck solution-ce8a2c2b
  (fn [number]
    (let
     [roman
                {
                 1 "I",
                 4 "IV",
                 5 "V",
                 6 "VI",
                 9 "IX",
                 10 "X",
                 40 "XL",
                 50 "L",
                 60 "LX",
                 90 "XC",
                 100 "C",
                 400 "CD",
                 500 "D",
                 600 "DC",
                 900 "CM",
                 1000 "M"
                 },
      romanKeys (reverse (apply sorted-set (keys roman))),
      recursor
                (fn recurs
                  [out in]
                  (str
                    out
                    (if
                     (>= 0 in)
                      ""
                      (let
                       [
                        top (first (filter #(>= in %) romanKeys))
                        ]
                        (recurs (roman top) (- in top))
                        )
                      )
                    )
                  )
      ]
      (recursor "" number)
      )
    ))

(defcheck solution-cea2d6a1
  (fn write-roman [n]
    (let [nums [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
          m (zipmap nums
              ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"])

          cs (apply max 0 (filter (partial >= n) nums))]
      (if (zero? n)
        ""
        (str (m cs) (write-roman (- n cs)))))))

(defcheck solution-cf0da745
  (let [nums [1000 "M"
              900 "CM"
              500 "D"
              400 "CD"
              100 "C"
              90 "XC"
              50 "L"
              40 "XL"
              10 "X"
              9 "IX"
              5 "V"
              4 "IV"
              1 "I"]]
    (fn [n]
      (second
        (reduce (fn [[remaining acc] [divisor numeral]]
                  (let [[div mod] ((juxt quot rem) remaining divisor)]
                    [mod (apply str acc (repeat div numeral))]))
          [n ""]
          (partition 2 nums))))))

(defcheck solution-cf7990d
  (fn [n]
    (let [d->r {1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"
                10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC"
                100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC" 900 "CM"
                1000 "M" 2000 "MM" 3000 "MMM" 4000 "MMMM"}]
      (apply str (map (fn [power]
                        (d->r
                          (* power                              ; mult digit ...
                            (quot (mod n (* 10 power)) power)) ; ... by its power
                          nil))
                   [1000 100 10 1])))))

(defcheck solution-cf95a218
  (fn roman
    ([n] (roman n []))
    ([n acc]
     (let [vals (partition 2 [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                              90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"])]
       (if (zero? n)
         (clojure.string/join acc)
         (let [[v s] (first (filter (fn [[vv ss]] (>= n vv)) vals))]
           (recur (- n v) (conj acc s))))))))

(defcheck solution-cfbcaac
  (fn arab-to-roman [arab]
    (let [normal (fn [one five next] {1 [one]
                                      2 [one one]
                                      3 [one one one]
                                      4 [one five]
                                      5 [five]
                                      6 [five one]
                                      7 [five one one]
                                      8 [five one one one]
                                      9 [one next]
                                      0 []})
          thousands (fn [x] (repeat x "M"))
          generators [thousands
                      (normal "C" "D" "M")
                      (normal "X" "L" "C")
                      (normal "I" "V" "X")]]
      (->> (loop [gens generators
                  n arab
                  result []
                  ten-pow 1000]
             (if (empty? gens) result
                               (let [digit (quot n ten-pow)
                                     [gen & restgen] gens]
                                 (recur
                                   restgen
                                   (- n (* ten-pow digit))
                                   (conj result (gen digit))
                                   (/ ten-pow 10)))))
        flatten
        (apply str)))))

(defcheck solution-d0400d19
  #(let [to-u (fn [x] (apply str (take x (repeat "I"))))
         sub [[(to-u 1000) "M"]
              [(to-u 500) "D"]
              [(to-u 100) "C"]
              [(to-u 50) "L"]
              [(to-u 10) "X"]
              [(to-u 5) "V"]
              ["DCCCC" "CM"]
              ["CCCC" "CD"]
              ["LXXXX" "XC"]
              ["XXXX" "XL"]
              ["VIIII" "IX"]
              ["IIII" "IV"]]]
     (reduce (fn [so-far [from to]] (clojure.string/replace so-far from to)) (to-u %) sub)))

(defcheck solution-d043ac32
  (fn number->roman [number]
    (let [a (quot number 1000)
          b (quot (rem number 1000) 100)
          c (quot (rem number 100) 10)
          d (rem number 10)]
      (apply str
        (concat (repeat a "M")
          (cond (= b 9) "CM"
                (= b 4) "CD"
                :else (concat (when ( > b 4) "D")
                        (repeat (rem b 5) "C")))
          (cond (= c 9) "XC"
                (= c 4) "XL"
                :else (concat (when ( > c 4) "L")
                        (repeat (rem c 5) "X")))
          (cond (= d 9) "IX"
                (= d 4) "IV"
                :else (concat (when ( > d 4) "V")
                        (repeat (rem d 5) "I"))))))))

(defcheck solution-d0587839
  (fn p [num]
    #_(println num)
    (cond (>= num 1000)
          (str (apply str (take (quot num 1000) (cycle "M")))
            (p (mod num 1000)))
          (>= num 900)
          (str "CM" (p (- num 900)))
          (>= num 500)
          (str  "D" (apply str (take (quot (- num 500) 100) (cycle "C")))
            (p (mod num 100)))
          (>= num 400)
          (str "CD" (p (- num 400)))
          (>= num 100)
          (str (apply str (take (quot num 100) (cycle "C")))
            (p (mod num 100)))
          (>= num 90)
          (str "XC" (p (- num 90)))
          (>= num 50)
          (str "L" (apply str (take (quot (- num 50) 10) (cycle "X")))
            (p (mod num 10)))
          (>= num 40)
          (str "XL" (p (- num 40)))
          (>= num 10)
          (str (apply str (take (quot num 10) (cycle "X")))
            (p (mod num 10)))
          (>= num 9)
          (str "IX")
          (>= num 5)
          (str "V" (apply str (take (- num 5) (cycle "I"))))
          (>= num 4)
          (str "IV")
          :else
          (apply str (take num (cycle "I"))))))

(defcheck solution-d0cb3279
  #(let [rom-map (sorted-map-by >     1000  "M",
                   900 "CM",
                   500  "D",
                   400 "CD",
                   100  "C",
                   90 "XC",
                   50  "L",
                   40 "XL",
                   10  "X",
                   9 "IX",
                   5  "V",
                   4 "IV",
                   1  "I")]
     ; conv = Object.keys(conversions).filter(lessThan(number)).last();
     (loop [result ""
            rst    %]
       (if (<= rst 0)
         result
         (let [conv (->> (keys rom-map)
                      (filter (partial >= rst) )
                      (first))]
           (recur (str result (rom-map conv))
             (- rst conv)))))))

(defcheck solution-d152f687
  (fn f[n]
    (if (= n 0) ""
                (let [v
                      [
                       [1000 "M" ]
                       [ 900 "CM"]
                       [ 500 "D" ]
                       [ 400 "CD"]
                       [ 100 "C" ]
                       [  90 "XC"]
                       [  50 "L" ]
                       [  40 "XL"]
                       [  10 "X" ]
                       [   9 "IX"]
                       [   5 "V" ]
                       [   4 "IV"]
                       [   1 "I" ]]]
                  (let [[x k] (first (drop-while #(< n (first %)) v))]
                    (str k (f (- n x))))))))

(defcheck solution-d1bc5e87
  (fn [decimal]
    (let [ff (fn ff [n {:keys [o f t] :as m}]
               (cond
                 (= 4 n) (str o f)
                 (= 9 n) (str o t)
                 (> 5 n) (apply str (repeat n o))
                 :else (str f (ff (- n 5) m))))
          get-digit (fn [n b] (quot (mod (* b (quot n b)) (* 10 b)) b))
          foo {1000 {:o \M :f \_ :t \_}
               100  {:o \C :f \D :t \M}
               10   {:o \X :f \L :t \C}
               1    {:o \I :f \V :t \X}}]
      (apply str (map #(ff (get-digit decimal %) (foo %)) [1000 100 10 1])))))

(defcheck solution-d1f85ce4
  (fn [n]
    (let [m (quot n 1000)
          c (quot (mod n 1000) 100)
          x (quot (mod n 100) 10)
          i (mod n 10)
          single-rrm (fn [v small middle end]
                       (cond (< v 4) (vec (repeat v small))
                             (= v 4) [small middle]
                             (= v 5) [middle]
                             (< 5 v 9) (concat [middle] (vec (repeat (- v 5) small)))
                             (= v 9) [small end]))]
      (->> (reduce #(concat %1 %2)
             ""
             (concat (single-rrm m "M" "" "")
               (single-rrm c "C" "D" "M")
               (single-rrm x "X" "L" "C")
               (single-rrm i "I" "V" "X")))
        (apply str)))))

(defcheck solution-d21457c1
  (fn [x]
    (let [R (rseq (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
                    90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"))]
      ((fn [x s]
         (if (= x 0)
           s
           (let [[k v] (some #(if (>= x (key %)) %) R)]
             (recur (- x k) (str s v))))) x ""))))

(defcheck solution-d21f14b9
  #(loop [n % r ""]
     (if (zero? n) r
                   (let [[m s] (some (fn [x] (if (>= n (first x)) x nil))
                                 [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]])]
                     (recur (- n m) (str r s))))))

(defcheck solution-d2f732ad
  #(loop [v %                            ;this is arg
          decoder [[1 "I"] [4 "IV"] [5 "V"]
                   [9 "IX"] [10 "X"] [40 "XL"]
                   [50 "L"] [90 "XC"] [100 "C"]
                   [400 "CD"] [500 "D"] [900 "CM"]
                   [1000 "M"]]
          result ""]
     (if (zero? v)
       result
       (let [d (last decoder)
             o (int (/ v (first d)))]                          ; occurence
         (recur (- v (* o (first d)))
           (butlast decoder)
           (apply str result (repeat o (last d))))
         )
       )
     ))

(defcheck solution-d32ca9e1
  (fn write-roman [n]
    (let [v-map {0 [\I \V]
                 1 [\X \L]
                 2 [\C \D]
                 3 [\M]}]
      (apply str (loop [current n result [] digit-loc 3 divider 1000]
                   (if (= current 0) result
                                     (recur
                                       (rem current divider)
                                       (let [digit (quot current divider)
                                             letters (get v-map digit-loc)]
                                         (cond
                                           (= 0 digit) result
                                           (= 9 digit)
                                           (conj result (first letters) (first (get v-map (inc digit-loc))))
                                           (>= digit 5)
                                           (reduce conj (conj result (second letters))
                                             (repeat (- digit 5) (first letters)))
                                           (= 4 digit)
                                           (conj result (first letters) (second letters))
                                           :else (reduce conj result (repeat digit (first letters)))))
                                       (dec digit-loc)
                                       (/ divider 10))))))))

(defcheck solution-d3533969
  (fn write-roman-numerals [n]
    (clojure.pprint/cl-format nil "~@R" n)))

(defcheck solution-d3ca2fcf
  #((fn s
      [i [[v d] & r :as n]]
      (cond
        (= 0 i) ""
        (< i v) (s i r)
        :else (str d (s (- i v) n))))
    % (rseq
        (sorted-map
          1000 \M 900 "CM" 500 \D 400 "CD" 100 \C 90 "XC"
          50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I))))

(defcheck solution-d3cd1b1
  (fn r [n]
    (some (fn [[v s]] (if (>= n v) (str s (r (- n v)))))
      '([1000 M] [900 CM] [500 D] [400 CD]
        [100 C] [90 XC] [50 L] [40 XL]
        [10 X] [9 IX] [5 V] [4 IV] [1 I]))))

(defcheck solution-d41848b2
  (fn roman [n]
    (let [alphabet (sort-by val >
                     {\I   1   \V   5   \X   10   \L   50
                      \C   100 \D   500 \M   1000 "IV" 4
                      "IX" 9   "XL" 40  "XC" 90   "CD" 400
                      "CM" 900})]
      (loop [res "" n n]
        (if (zero? n) res
                      (let [[rom arab] (some #(when (<= (val %) n) %) alphabet)]
                        (recur (str res rom) (- n arab))))))))

(defcheck solution-d427de0e
  (fn roman [n]
    (if (zero? n) ""
                  (let [R (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
                            90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")
                        t (last (filter #(<= % n) (keys R)))]
                    (str (R t) (roman (- n t)))))))

(defcheck solution-d531a2f4
  #(apply str (
               (fn f [n [[k v] & r]]
                 (if (nil? k) nil
                              (concat (repeat (Math/floor (/ n v)) k) (f (mod n v) r)))
                 )
               % [["M" 1000]
                  ["CM" 900]
                  ["D" 500]
                  ["CD" 400]
                  ["C" 100]
                  ["M" 100]
                  ["XC" 90]
                  ["L" 50]
                  ["XL" 40]
                  ["X" 10]
                  ["IX" 9]
                  ["V" 5]
                  ["IV" 4]
                  ["I" 1]]
               )))

(defcheck solution-d544040d
  (fn [x]
    (letfn [(ntimes-char [c n]
              (apply str (map (fn [_] c) (range 0 n))))
            (process-any [a divisor c1 c5 c10 next]
              (let [l (quot a divisor)]
                (str
                  (cond
                    (zero? l) ""
                    (<= 1 l 3) (ntimes-char c1 l)
                    (= l 4) (str (ntimes-char c1 1) c5)
                    (= l 5) (str c5)
                    (<= 6 l 8) (str c5 (ntimes-char c1 (- l 5)))
                    (= l 9) (str (ntimes-char c1 1) c10))
                  (next (rem a divisor)))))
            (process-thousand [a]
              (process-any a 1000 \M \v \x process-hundred))
            (process-hundred [a]
              (process-any a 100 \C \D \M process-ten))
            (process-ten [a]
              (process-any a 10 \X \L \C process-one))
            (process-one [a]
              (process-any a 1 \I \V \X (fn [_] "")))]
      (process-thousand x))))

(defcheck solution-d59c0eb
  (fn roman [n]
    (if (zero? n) ""
                  (let [m { 1000 "M",
                           900 "CM", 500 "D", 400 "CD", 100 "C",
                           90 "XC", 50 "L", 40 "XL", 10 "X",
                           9 "IX", 5 "V", 4 "IV", 1 "I" }]
                    (let [v (apply max (filter #(<= % n) (keys m)))]
                      (str (m v) (roman (- n v)))
                      )
                    )
                  )
    ))

(defcheck solution-d5e58fd
  (fn r[x]
    (let [W { 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 500 "D" 900 "CM" 1000 "M" }]
      (letfn [(largest [y] (apply max (filter #(<= % y) (keys W))))]
        (if (<= x 0) "" (str (W (largest x)) (r (- x (largest x)))))))))

(defcheck solution-d6349ff9
  (fn rrm [n]
    (let [m (quot n 1000) c (quot (mod n 1000) 100) x (quot (mod n 100) 10) i (mod n 10)
          single-rrm (fn [v small middle end]
                       (cond (< v 4) (vec (repeat v small))
                             (= v 4) [small middle]
                             (= v 5) [middle]
                             (< 5 v 9) (concat [middle] (vec (repeat (- v 5) small)) )
                             (= v 9) [small end]
                             ))  ]
      (reduce #(.concat %1 %2) "" (concat (single-rrm m "M" "" "") (single-rrm c "C" "D" "M") (single-rrm x "X" "L" "C") (single-rrm i "I" "V" "X"))
        )  )))

(defcheck solution-d64c6b9b
  (fn [n]
    (let [roman {1    "I"
                 5    "V"
                 10   "X"
                 50   "L"
                 100  "C"
                 500  "D"
                 1000 "M"}
          rks (sort (keys roman))]
      (loop [n n
             d 1
             r ""]
        (cond
          (= 0 n) r
          (= d 1000) (str (clojure.string/join (repeat n (roman 1000))) r)
          :default (recur (quot n 10)
                     (* 10 d)
                     (let [i (rem n 10)
                           one (roman d)
                           five (roman (quot (* d 10) 2))
                           ten (roman (* d 10))]
                       (cond
                         (= 0 i) r
                         (< i 4) (str (clojure.string/join (repeat i one)) r)
                         (= 4 i) (str one five r)
                         (= 5 i) (str five r)
                         (= i 9) (str one ten r)
                         :default (str five (clojure.string/join (repeat (- i 5) one)) r)))))))))

(defcheck solution-d6a02583
  (let [roman-digits
        (sorted-map-by >
          1000 \M, 900 "CM"
          500 \D, 400 "CD"
          100 \C,  90 "XC"
          50 \L,  40 "XL"
          10 \X,   9 "IX"
          5 \V,   4 "IV"
          1 \I)]

    (fn ->roman [n]
      (loop [r "", n n]
        (if (zero? n) r
                      (let [pair
                            (->> roman-digits
                              (drop-while
                                (comp (partial < n)
                                  first))
                              first)]

                        (recur (str r (val pair))
                          (- n (key pair)))))))))

(defcheck solution-d6c2070c
  (fn get-roman-numeral [n]
    (let [primary-nums (zipmap [1 10 100 1000] ["I" "X" "C" "M"])
          secondary-nums (zipmap [5 50 500] ["V" "L" "D"])
          special-pairs (zipmap [4 9 40 90 400 900] ["IV" "IX" "XL" "XC" "CD" "CM"])
          ten-raised-to (fn [n] (reduce * (repeat n 10)))
          -get-roman-numeral (fn f [n]
                               (cond (zero? n) ""
                                     (secondary-nums n) (secondary-nums n)
                                     (special-pairs n) (special-pairs n)
                                     :else (if-let [r (first (filter #(and (zero? (mod n %)) (<= (quot n %) 3)) (keys primary-nums)))]
                                             (apply str (repeat (quot n r) (primary-nums r)))
                                             (let [o (->> (filter #(< % n) (keys secondary-nums))
                                                       (apply max))]
                                               (apply str (secondary-nums o) (f (- n o)))))))]
      (->> (str n)
        seq
        (map str)
        (map #(parse-int %))
        reverse
        (map-indexed #(* %2 (ten-raised-to %1)))
        reverse
        (map -get-roman-numeral)
        (apply str)))))

(defcheck solution-d7dcd9df
  (partial

    (fn [s x]
      (let [m {1    "I" 4   "IV" 5   "V" 9   "IX"
               10   "X" 40  "XL" 50  "L" 90  "XC"
               100  "C" 400 "CD" 500 "D" 900 "CM"
               1000 "M"}]
        (if (= 0 x)
          s
          (let [n (apply max (filter #(<= % x) (keys m)))]
            (recur (str s (m n)) (- x n))))))

    ""))

(defcheck solution-d82aed18
  #(loop [n % [t & ts] [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] ["" "M" "MM" "MMM"]] acc nil] (if (= 0 n) (apply str acc) (recur (quot n 10) ts (cons (t (mod n 10)) acc)))))

(defcheck solution-d908b45a
  (fn [n]
    (let [s (reverse (sort-by second {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100, "D" 500, "CM" 900, "M" 1000}))
          f (fn [n]
              (let [[x y] (first (drop-while #(> (nth % 1) n) s))]
                [x (- n y)]))]
      (loop [n n r []]
        (if (zero? n) (apply str r)
                      (let [[x y] (f n)]
                        (recur y (conj r x))))))))

(defcheck solution-d962cb0c
  (fn to-roman [n]
    (cond
      (>= n 1000) (str "M"  (to-roman (- n 1000)))
      (>= n  900) (str "CM" (to-roman (- n 900)))
      (>= n  500) (str "D"  (to-roman (- n 500)))
      (>= n  400) (str "CD" (to-roman (- n 400)))
      (>= n  100) (str "C"  (to-roman (- n 100)))
      (>= n   90) (str "XC" (to-roman (- n  90)))
      (>= n   50) (str "L"  (to-roman (- n  50)))
      (>= n   40) (str "XL" (to-roman (- n  40)))
      (>= n   10) (str "X"  (to-roman (- n  10)))
      (>= n    9) (str "IX" (to-roman (- n   9)))
      (>= n    5) (str "V"  (to-roman (- n   5)))
      (>= n    4) (str "IV" (to-roman (- n   4)))
      (>= n    1) (str "I"  (to-roman (- n   1)))
      :default "")))

(defcheck solution-d98c4c1f
  (fn
    [n]

    (let [units (mod n 10)
          tens  (/ (mod (- n units) 100) 10)
          hundreds (/ (mod (- n units (* tens 10)) 1000) 100)
          thousands (/ (mod (- n units (* tens 10) (* hundreds 100)) 10000) 1000)
          romain-seq (concat (repeat thousands 'M)
                       (repeat hundreds 'C)
                       (repeat tens 'X)
                       (repeat units 'I))]
      (->
        (apply str romain-seq)
        (clojure.string/replace #"IIIII" "V")
        (clojure.string/replace #"IIII" "IV")
        (clojure.string/replace #"VIV" "IX")
        (clojure.string/replace #"XXXXX" "L")
        (clojure.string/replace #"XXXX" "XL")
        (clojure.string/replace #"LXL" "XC")
        (clojure.string/replace #"CCCCC" "D")
        (clojure.string/replace #"CCCC" "LD")
        (clojure.string/replace #"DLD" "CM")))))

(defcheck solution-da1ec6cb
  (fn writeroman [x]
    (let [dictionary {1000 "M" 500 "D" 100 "C" 50 "L" 10 "X" 5 "V" 1 "I"}]
      (letfn [
              (romanfives [i result x dict]
                (romantens i (conj result (apply str (repeat (int (quot x (* 5 (Math/pow 10 i))))
                                                       (get dict (int (* 5 (Math/pow 10 i)))))))
                  (mod x (* 5 (Math/pow 10 i)))
                  dict))
              (romantens [i result x dict]
                (if (< (quot x (Math/pow 10 i)) 4)
                  (if (== i 0)
                    (conj result (apply str (repeat (int x) (get dict (int (Math/pow 10 i))))))
                    (romanfives (dec i) (conj result (apply str (repeat (int (quot x (Math/pow 10 i)))
                                                                  (get dict (int (Math/pow 10 i))))))
                      (mod x (Math/pow 10 i))
                      dict))
                  (if (empty? (peek result))
                    (if (== i 0)
                      (conj result (str (get dict (int (Math/pow 10 i)))
                                     (get dict (int (* 5 (Math/pow 10 i))))))
                      (romanfives (dec i) (conj result (str (get dict (int (Math/pow 10 i)))
                                                         (get dict (int (* 5 (Math/pow 10 i))))))
                        (mod x (Math/pow 10 i))
                        dict))
                    (if (== i 0)
                      (conj (pop result) (str (get dict (int (Math/pow 10 i)))
                                           (get dict (int (Math/pow 10 (inc i))))))
                      (romanfives (dec i) (conj (pop result) (str (get dict (int (Math/pow 10 i)))
                                                               (get dict (int (Math/pow 10 (inc i))))))
                        (mod x (Math/pow 10 i))
                        dict)))))]
        (apply str (trampoline romantens 3 [] x dictionary))))))

(defcheck solution-da7a60b0
  #(->
     (reduce
       (fn [[s n] [t v p?]]
         (let [m (if p? (if (> n v) 1 0) (quot n v))]
           [(apply str s (repeat m t)) (- n (* m v))]))
       ["" %]
       (->> (map list "IVXLCDM" [1 5 10 50 100 500 1000])
         (partition 3 2)
         (map
           (fn [[[p np] & r]]
             (for [[s n] r [p np] [[p np false] ["" 0 true]]] [(str p s) (- n np)])))
         (apply concat) (cons ["I" 1]) reverse))
     first))

(defcheck solution-dae29e97
  (fn [n]
    (let [dict [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
                [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [n   n
             acc []]
        (if (zero? n)
          (apply str acc)
          (let [[v c] (first (drop-while #(> (first %) n) dict))]
            (recur (- n v) (conj acc c))))))))

(defcheck solution-dafbd458
  (let [m (sorted-map
            1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X",
            40 "XL", 50 "L", 90 "XC", 100 "C",
            400 "CD", 500 "D", 900 "CM", 1000 "M")]
    (fn f [x]
      (or
       (m x)
       (let [[[a b] & _] (rsubseq m < x)]
         (str b (f (- x a))))))))

(defcheck solution-db5432c9
  (fn write-roman [n]
    (letfn [(c [n1 n2] (< 0 (quot n1 n2)))
            (r [n1 n2 s] (str s (write-roman (- n1 n2))))]
      (cond (c n 1000) (r n 1000 "M")
            (c n 900) (r n 900 "CM")
            (c n 500) (r n 500 "D")
            (c n 400) (r n 400 "CD")
            (c n 100) (r n 100 "C")
            (c n 90) (r n 90 "XC")
            (c n 50) (r n 50 "L")
            (c n 40) (r n 40 "XL")
            (c n 10) (r n 10 "X")
            (c n 9) (r n 9 "IX")
            (c n 5) (r n 5 "V")
            (c n 4) (r n 4 "IV")
            (c n 1) (r n 1 "I")
            true nil))))

(defcheck solution-db61a885
  (fn [decimal]
    (let [Value->Symbol (into {} (vec
                                   (map-indexed
                                     (fn [idx itm]
                                       [(str idx) (zipmap [3 2 1 0] itm)])
                                     [["" "" "" ""]
                                      ["M" "C" "X" "I"]
                                      ["MM" "CC" "XX" "II"]
                                      ["MMM" "CCC" "XXX" "III"]
                                      ["MMMM"	"CD" "XL" "IV"]
                                      ["MMMMM" "D" "L" "V"]
                                      ["MMMMMM" "DC" "LX" "VI"]
                                      ["MMMMMMM" "DCC" "LXX" "VII"]
                                      ["MMMMMMMM" "DCCC" "LXXX" "VIII"]
                                      ["MMMMMMMMM" "CM" "XC" "IX"]])))
          str-decimal (str decimal)
          roman (map (fn [digit place]
                       ((Value->Symbol (str digit)) place))
                  str-decimal
                  (range (dec (count str-decimal)) -1 -1))]
      (clojure.string/join roman))))

(defcheck solution-dc075f92
  (fn roman-nummbers
    [num]
    (let [literals {1 "I",4 "IV", 5 "V",9 "IX", 10 "X", 40 "XL", 50 "L",90 "XC",
                    100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}
          powers (keys literals)
          biggest-divider (fn [x] (last (sort (filter #(<= % x) powers))))]
      (loop [rem num out ""]
        (if (= rem 0)
          out
          (let [divider (biggest-divider rem)
                res (quot rem divider)
                cur (* res divider)]
            (recur
              (- rem cur)
              (apply str out (repeat res (get literals divider))))
            ))
        )
      )))

(defcheck solution-dcb80d12
  (fn [n]
    (let [thousands (rem (quot n 1000) 10)
          hundreds (rem (quot n 100) 10)
          tens (rem (quot n 10) 10)
          ones (rem n 10)
          to-r (fn [x ones fives tens]
                 (cond (= x 9) (str ones tens)
                       (>= x 5) (apply str fives (repeat (- x 5) ones))
                       (= x 4) (str ones fives)
                       :else (apply str (repeat x ones))))]
      (str (to-r thousands "M" "?" "?")
        (to-r hundreds "C" "D" "M")
        (to-r tens "X" "L" "C")
        (to-r ones "I" "V" "X")))))

(defcheck solution-dcea69
  (fn [n]
    (let [a (["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"] (mod n 10))
          b (["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"] (mod (quot n 10) 10))
          c (["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"] (mod (quot n 100) 10))
          d (["" "M" "MM" "MMM"] (quot n 1000))]
      (str d c b a))))

(defcheck solution-dcf7bed6
  (fn [number]
    (loop [n number
           m [["M" 1000]
              ["CM" 900]
              ["D"  500]
              ["CD" 400]
              ["C"  100]
              ["XC"  90]
              ["L"   50]
              ["XL"  40]
              ["X"   10]
              ["IX"   9]
              ["V"    5]
              ["IV"   4]
              ["I"    1]]
           acc ""]
      (if-let [pair (first m)]
        (if (>= n (second pair))
          (recur (- n (second pair)) m (str acc (first pair)))
          (recur n (rest m) acc))
        acc))))

(defcheck solution-dcf95f6c
  (fn [x_]
    (apply
      str
      ((fn rom [x [[n c] & rest :as ns]]
         (if (zero? x)
           '()
           (if (>= x n)
             (concat c (rom (- x n) ns))
             (rom x rest))))
       x_ [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
           [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]))))

(defcheck solution-dd05920f
  (fn [n]
    (let [v [1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"]]
      (loop [x n y v res []]
        (if (= x 0) (apply str res)
                    (recur (if (>= (- x (first y)) 0) (- x (first y)) x)
                      (if (>= (- x (first y)) 0) y (nnext y))
                      (if (>= (- x (first y)) 0) (conj res (second y)) res)))))))

(defcheck solution-dd9cb61e
  (fn to-roman [n]
    (if (zero? n)
      ""
      (let [table {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M" }
            first-num (reduce #(if (<= %2 n) %2 %1) [1 4 5 9 10 40 50 90 100 400 500 900 1000])
            first-letter (table first-num)]
        (str first-letter (to-roman (- n first-num)))))))

(defcheck solution-dda2ebca
  (fn [n]
    (letfn [(digit [n one five ten]
              (condp = n
                4 [one five]
                9 [one ten]
                (concat (if (>= n 5) [five])
                  (repeat (mod n 5) one))))]
      (apply str
        (concat
          (digit (quot n 1000) \M \Z \Z)
          (digit (quot (mod n 1000) 100) \C \D \M)
          (digit (quot (mod n 100) 10) \X \L \C)
          (digit (mod n 10) \I \V \X))))))

(defcheck solution-dda9f604
  (fn [n]
    (let [roms (zipmap
                 [1 4 5 9 10 40 50 90 100 400 500 900 1000]
                 ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"])
          inner (fn inner [x]
                  (if (zero? x) ""
                                (let [smallest (apply max (filter #(<= % x) (keys roms)))]
                                  (.concat (get roms smallest) (inner (- x smallest))))))]
      (inner n))))

(defcheck solution-de14779b
  (fn [n]
    (letfn [(rd [roman arabic [n accu]]
              (let
               [how-many (int (/ n arabic))
                remain (- n (* how-many arabic))]
                [remain (apply str accu (repeat how-many roman))]))]
      (->> [n ""]
        (rd "M" 1000)
        (rd "CM" 900)
        (rd "D" 500)
        (rd "CD" 400)
        (rd "C" 100)
        (rd "XC" 90)
        (rd "L" 50)
        (rd "XL" 40)
        (rd "X" 10)
        (rd "IX" 9)
        (rd "V" 5)
        (rd "IV" 4)
        (rd "I" 1)
        last
        ))))

(defcheck solution-df2d1aa1
  (fn rn [n]
    (cond
      (zero? n) ""
      (>= n 1000) (str "M" (rn (- n 1000)))
      (>= n 900) (str "CM" (rn (- n 900)))
      (>= n 500) (str "D" (rn (- n 500)))
      (>= n 400) (str "CD" (rn (- n 400)))
      (>= n 100) (str "C" (rn (- n 100)))
      (>= n 90) (str "XC" (rn (- n 90)))
      (>= n 50) (str "L" (rn (- n 50)))
      (>= n 40) (str "XL" (rn (- n 40)))
      (>= n 10) (str "X" (rn (- n 10)))
      (= n 9) "IX"
      (>= n 5) (str "V" (rn (- n 5)))
      (= n 4) "IV"
      (>= n 1) (str "I" (rn (dec n))))))

(defcheck solution-dfcbe812
  (fn [i]
    (let [m [ [1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"]
             [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"] ]
          ]
      (loop [ri i rm m res ""]
        (if (= 0 ri)
          res
          (let [arb (first (first rm))
                rom (second (first rm)) ]
            (if (>= ri arb)
              (recur (- ri arb) rm (str res rom) )
              (recur ri (rest rm) res) )))))))

(defcheck solution-e0225493
  (fn [x]
    (letfn [(get-numeral [value one five ten] (cond (< value 4) (apply str (take value (repeat one)))
                                                    (= value 4) (str one five)
                                                    (< value 9) (str five (apply str (take (- value 5) (repeat one))))
                                                    (= value 9) (str one ten)))]
      (apply str (reverse (map-indexed
                            #(cond (= %1 0) (get-numeral (read-string %2) "I" "V" "X")
                                   (= %1 1) (get-numeral (read-string %2) "X" "L" "C")
                                   (= %1 2) (get-numeral (read-string %2) "C" "D" "M")
                                   (= %1 3) (get-numeral (read-string %2) "M" "NA" "NA")
                                   :else ""

                                   ) (reverse (re-seq #"\d" (str x)))))))))

(defcheck solution-e02ce5f2
  (fn [n] (let [d (map (zipmap "0123456789" (range)) (str n))
                m {0 "" 4 "IV" 5 "V" 9 "IX" 40 "XL" 50 "L" 90 "XC" 400 "CD" 500 "D" 900 "CM"} ]
            (apply str (map-indexed
                         #(let [p (- (count d) % 1)
                                f (fn [x]
                                    (cond (= p 0)
                                          (if (< x 5) (apply str (repeat x "I"))
                                                      (apply str "V" (repeat (- x 5) "I")) )
                                          (= p 1)
                                          (if (< x 5) (apply str (repeat x "X"))
                                                      (apply str "L" (repeat (- x 5) "X")) )
                                          (= p 2)
                                          (if (< x 5) (apply str (repeat x "C"))
                                                      (apply str "D" (repeat (- x 5) "C")) )
                                          (= p 3) (apply str (repeat x "M")) ))]
                            (m (int (* (Math/pow 10 p) %2)) (f %2)) ) d) )  ) ))

(defcheck solution-e02d5f5f
  (fn prob104 [n]
    (letfn [(thousands-str [n] (apply str (take n (cycle "M"))))
            (hundreds-str [n]
              (cond
                (< n 4) (apply str (take n (cycle "C")))
                (= n 4) "CD"
                (= n 5) "D"
                (= n 9) "CM"
                :else (apply str "D" (take (- n 5) (cycle "C")))))
            (tens-str [n]
              (cond
                (< n 4) (apply str (take n (cycle "X")))
                (= n 4) "XL"
                (= n 5) "L"
                (= n 9) "XC"
                :else  (apply str "L" (take (- n 5) (cycle "X")))))
            (ones-str [n]
              (cond
                (< n 4) (apply str (take n (cycle "I")))
                (= n 4) "IV"
                (= n 9) "IX"
                (= n 5) "V"
                :else  (apply str "V" (take (- n 5) (cycle "I")))))]
      (let [thousands (quot n 1000)
            hundreds (mod (quot n 100) 10)
            tens (mod (quot n 10) 10)
            ones (rem n 10)
            ]
        (apply str (concat (thousands-str thousands) (hundreds-str hundreds) (tens-str tens) (ones-str ones)))
        )
      )
    ))

(defcheck solution-e03b1a5d
  (fn [n]
    (let [romans (reverse [[1 "I"] [4 "IV"] [5 "V"] [9 "IX"] [10 "X"]
                           [40 "XL"] [50 "L"] [90 "XC"] [100 "C"] [400 "CD"]
                           [500 "D"] [900 "CM"] [1000 "M"]])]
      (loop [[r & rs] romans sum n s []]
        (if (nil? r) (reduce str s)
                     (let [[a b] r cnt (int (/ sum a))]
                       (recur rs (- sum (* cnt a)) (concat s (repeat cnt b)))))))))

(defcheck solution-e06ee977
  (fn write-roman [init]
    (loop [s init v []]
      (cond (< s 1) (apply str v)
            (>= s 1000) (recur (- s 1000) (conj v "M"))
            (>= s  900) (recur (- s  900) (conj v "CM"))
            (>= s  500) (recur (- s  500) (conj v "D"))
            (>= s  400) (recur (- s  400) (conj v "CD"))
            (>= s  100) (recur (- s  100) (conj v "C"))
            (>= s   90) (recur (- s   90) (conj v "XC"))
            (>= s   50) (recur (- s   50) (conj v "L"))
            (>= s   40) (recur (- s   40) (conj v "XL"))
            (>= s   10) (recur (- s   10) (conj v "X"))
            (>= s    9) (recur (- s    9) (conj v "IX"))
            (>= s    5) (recur (- s    5) (conj v "V"))
            (>= s    4) (recur (- s    4) (conj v "IV"))
            (>= s    1) (recur (- s    1) (conj v "I"))))))

(defcheck solution-e1c231c5
  (fn f [x]
    (apply str (cond (= x 0) ""
                     (<= 1 x 3) (repeat x "I")
                     (= x 4) "IV"
                     (<= 5 x 8) (concat "V" (f (- x 5)))
                     (= x 9) "IX"
                     (<= 10 x 39) (concat (repeat (int (/ x 10)) "X") (f (mod x 10)))
                     (<= 40 x 49) (concat "XL" (f (mod x 10)))
                     (<= 50 x 89) (concat "L" (repeat (- (int (/ x 10)) 5) "X") (f (mod x 10)))
                     (<= 90 x 100) (concat "XC" (f (mod x 10)))
                     (<= 100 x 399) (concat (repeat (int (/ x 100)) "C") (f (mod x 100)))
                     (<= 400 x 499) (concat "CD" (f (mod x 100)))
                     (<= 500 x 899) (concat "D" (repeat (- (int (/ x 100)) 5) "C") (f (mod x 100)))
                     (<= 900 x 999) (concat "CM" (f (mod x 100)))
                     (<= 1000 x 3999) (concat (repeat (int (/ x 1000)) "M") (f (mod x 1000)))))))

(defcheck solution-e256d6ec
  (fn [z]
    (let [m {1 "I", 4  "IV",
             5 "V", 9 "IX", 10
               "X", 40 "XL", 50 "L",
             90 "XC", 100 "C", 400 "XD",
             500 "D", 900 "CM" 1000 "M"}]
      (letfn [(rn [x [h & t]]
                (if (> x 0)
                  (let [n (quot x h)]
                    (if (> n 0)
                      (let [r (rem x h)
                            s (m h)]
                        (concat (repeat n s) (rn r t)))
                      (rn x t)))))]

        (reduce str (rn z (sort > (keys m))))))))

(defcheck solution-e32d4e0b
  (fn roman
    ([n] (let [values [["M" 1000]
                       ["CM" 900]
                       ["D" 500]
                       ["CD" 400]
                       ["C" 100]
                       ["XC" 90]
                       ["L" 50]
                       ["XL" 40]
                       ["X" 10]
                       ["IX" 9]
                       ["V" 5]
                       ["IV" 4]
                       ["I" 1]]]

           (roman n values "")))

    ([n vals s]
     (if (seq vals)
       (let [[l v] (first vals)
             [n' vals' s'] (if (>= n v)
                             [(- n v) vals (str s l)]
                             [n (rest vals) s])]
         (recur n' vals' s'))
       s))))

(defcheck solution-e353bea0
  (fn [n]
    (str (apply str (repeat (quot n 1000) "M"))
      (apply str (map
                   (fn [[a b c d :as e]]
                     ((comp
                        (fn [s] (clojure.string/replace s (apply str (repeat 4 a)) (str a b)))
                        (fn [s] (clojure.string/replace s (str b (apply str (repeat 4 a))) (str a c)))
                        (partial apply str)
                        (fn [[a b c d :as e]]
                          (concat
                            (repeat (quot (rem n (* 10 d)) (* 5 d)) b)
                            (repeat (quot (rem n (* 5 d)) d) a)))) e))
                   [["C" "D" "M" 100]
                    ["X" "L" "C" 10]
                    ["I" "V" "X" 1]])))))

(defcheck solution-e3fe7747
  (fn roman-numerals [n]
    (if
     (zero? n) ""
               (let [conversions [
                                  [1000 "M"]
                                  [900 "CM"]
                                  [500 "D"]
                                  [400 "CD"]
                                  [100 "C"]
                                  [90 "XC"]
                                  [50 "L"]
                                  [40 "XL"]
                                  [10 "X"]
                                  [9 "IX"]
                                  [5 "V"]
                                  [4 "IV"]
                                  [1 "I"]]
                     [arabic roman] (first (filter #(>= n (first %)) conversions))]
                 (str roman (roman-numerals (- n arabic)))))))

(defcheck solution-e3ffe462
  (fn [n]
    (let [letters "IVXLCDM"
          g (fn [m]
              (let [[a b c] (take 3 (drop (* 2 m) letters))]
                (fn [x]
                  (cond
                    (< x 4) (apply str (repeat x a))
                    (= x 4) (str a b)
                    (< x 9) (str b (apply str (repeat (- x 5) a)))
                    (= x 9) (str a c)
                    )
                  )
                )
              )
          ]
      (apply str (reverse (for [m (range (count (str n)))]
                            (let [d {0 1 1 10 2 100 3 1000}]
                              ((g m) (rem (quot n (get d m)) 10))
                              )
                            )))
      )
    ))

(defcheck solution-e456afde
  (fn roman [n]
    (condp <= n
      1000 (str  "M" (roman (- n 1000)))
      900 (str "CM" (roman (- n  900)))
      500 (str  "D" (roman (- n  500)))
      400 (str "CD" (roman (- n  400)))
      100 (str  "C" (roman (- n  100)))
      90 (str "XC" (roman (- n   90)))
      50 (str  "L" (roman (- n   50)))
      40 (str "XL" (roman (- n   40)))
      10 (str  "X" (roman (- n   10)))
      9 (str "IX" (roman (- n    9)))
      5 (str  "V" (roman (- n    5)))
      4 (str "IV" (roman (- n    4)))
      1 (str  "I" (roman (- n    1)))
      0 nil)))

(defcheck solution-e475387a
  (fn [n]
    (apply str
      ((fn f [n]
         (let [c [[1000 "M"]
                  [900 "CM"]
                  [500 "D"]
                  [400 "CD"]
                  [100 "C"]
                  [90 "XC"]
                  [50 "L"]
                  [40 "XL"]
                  [10 "X"]
                  [9 "IX"]
                  [5 "V"]
                  [4 "IV"]
                  [1 "I"]]
               [a r] (first (filter #(>= n (first %)) c))]
           (if (zero? n)
             ""
             (cons r (f (- n a)))))) n))))

(defcheck solution-e4ebb7ba
  #(->>
     (reduce
       (fn [[l r] [k v]]
         (let [n (int (/ r k))
               r1 (- r (* n k))]
           [(if (> n 0) (cons (apply str (take n [v v v])) l) l)
            r1]
           ))
       ['() %]
       (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD"
         100 "C" 90 "XC" 50 "L" 40 "XL"
         10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"))
     first reverse (apply str)))

(defcheck solution-e4f1c695
  #(let [syms [[1000 \M] [900 "CM"] [500 \D] [400 "CD"] [100 \C] [90 "XC"]
               [50 \L] [40 "XL"] [10 \X] [9 "IX"] [5 \V] [4 "IV"] [1 \I]]]
     (->> (nth (iterate (fn [[x i bits]]
                          (let [[v s] (syms i)]
                            [(rem x v) (inc i) (conj bits (repeat (quot x v) s))]))
                 [% 0 []])
            (count syms))
       last (apply concat) (apply str))))

(defcheck solution-e5353154
  (fn romans [n]
    (letfn [(romans-1 [n s1 s2 s3]
              (apply str (concat
                           (if (= (rem n 5) 4) s1)
                           (if (= n 9) s3)
                           (if (and (>= n 4) (<= n 8)) s2)
                           (apply str (repeat (rem (rem n 5) 4) s1))
                           ))
              )]
      (let [th (quot n 1000)
            hund (quot (rem n 1000) 100)
            tens (quot (rem n 100) 10)
            units (rem n 10)]
        (apply str (apply str (repeat th \M))
          (romans-1 hund "C" "D" "M")
          (romans-1 tens "X" "L" "C")
          (romans-1 units "I" "V" "X")
          )
        )
      )
    ))

(defcheck solution-e6152224
  (fn roman [x] (cond
                  (<= 1000 x) (str "M" (roman (- x 1000)))
                  (<= 900 x) (str "CM" (roman (- x 900)))
                  (<= 500 x) (str "D" (roman (- x 500)))
                  (<= 400 x) (str "CD" (roman (- x 400)))
                  (<= 100 x) (str "C" (roman (- x 100)))
                  (<= 90 x) (str "XC" (roman (- x 90)))
                  (<= 50 x) (str "L" (roman (- x 50)))
                  (<= 40 x) (str "XL" (roman (- x 40)))
                  (<= 10 x) (str "X" (roman (- x 10)))
                  (<= 9 x) (str "IX" (roman (- x 9)))
                  (<= 5 x) (str "V" (roman (- x 5)))
                  (<= 4 x) (str "IV" (roman (- x 4)))
                  (<= 1 x) (str "I" (roman (- x 1)))
                  true "")))

(defcheck solution-e6805150
  (let
   [rm-vals [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90]
             ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
    (fn [x]
      (loop [x x, s ""]
        (if (= 0 x)
          s
          (let [[k v] (some (fn [[k v]] (when (<= v x) [k v])) rm-vals)]
            (recur (- x v) (str s k))))))))

(defcheck solution-e682e33a
  (fn _
    ([n] (_ (reverse
              (map read-string
                (map str
                  (str n))))
           "IVXLCDM.."))
    ([[d & e] [r s & t]]
     (if d
       (apply str
         (_ e t)
         (cond
           (< d 4) (repeat d r)
           (= d 4) [r s]
           (< d 9) (cons s (repeat (- d 5) r))
           1 [r (first t)]))
       ))))

(defcheck solution-e68e3c18
  (fn to-roman
    [n]
    (let [numerals
            {1 "I" 4 "IV" 5 "V"
             9 "IX" 10 "X" 40 "XL"
             50 "L" 90 "XC" 100 "C"
             400 "CD" 500 "D" 900 "CM"
             1000 "M"}
          f (fn [acc x]
              (if (<= x 0) acc
                           (let [k (first (sort > (filter #(and (>= (mod x %) 0) (< (mod x %) x)) (keys numerals))))]
                             (recur (str acc (get numerals k)) (- x k)))))]
      (f (str) n))))

(defcheck solution-e6dc9cc5
  (fn [n]
    (let  [r {1000 "M",900 "CM",500 "D",400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"}]
      (first (reduce (fn [[s n] e]
                       (loop [s s n n]
                         (if (< n e)
                           [s n]
                           (recur (str s (r e)) (- n e)))))
               ["" n] (reverse (sort (keys r))))))))

(defcheck solution-e6ecb2ec
  (fn [n]
    (let [ms (quot n 1000)
          ms* (rem n 1000)
          ds (quot ms* 500)
          ds* (rem ms* 500)
          cs (quot ds* 100)
          cs* (rem ds* 100)
          ls (quot cs* 50)
          ls* (rem cs* 50)
          xs (quot ls* 10)
          xs* (rem ls* 10)
          vs (quot xs* 5)
          is (rem xs* 5)]
      (-> (concat (repeat ms \M)
            (repeat ds \D)
            (repeat cs \C)
            (repeat ls \L)
            (repeat xs \X)
            (repeat vs \V)
            (repeat is \I))
        (clojure.string/join)
        (clojure.string/replace "VIIII" "IX")
        (clojure.string/replace "IIII" "IV")
        (clojure.string/replace "LXXXX" "XC")
        (clojure.string/replace "XXXX" "XL")
        (clojure.string/replace "DCCCC" "CM")
        (clojure.string/replace "CCCC" "CD")))))

(defcheck solution-e716d370
  (fn r[n]
    (letfn [(cd [n i v x]
              ({0 ""
                1 i
                2 (str i i)
                3 (str i i i)
                4 (str i v)
                5 v
                6 (str v i)
                7 (str v i i)
                8 (str v i i i)
                9 (str i x)} n))]
      (str (cd (mod (int (/ n 1000)) 10) "M" "?" "!")
        (cd (mod (int (/ n 100)) 10) "C" "D" "M")
        (cd (mod (int (/ n 10)) 10) "X" "L" "C")
        (cd (mod n 10) "I" "V" "X")))))

(defcheck solution-e7bea73a
  (fn [n]
    (let [nm {1 "I", 4 "IV", 5 "V", 9 "IX" 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C",
              500 "D", 400 "CD", 900 "CM", 1000 "M"}
          v (reverse (sort (keys nm)))]
      (loop [c "" a n [v1 & r] v]
        (cond
          (zero? a) c
          (>= a v1) (recur (str c (nm v1)) (- a v1) v)
          :else
          (recur c a r))))))

(defcheck solution-e7f1f011
  (fn __ [n]
    (let [
          aa (map #(parse-int %) (map str (seq (str n))))
          table [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                 ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                 ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                 ["" "M" "MM" "MMM"]]
          transse (reverse (map-indexed (fn [ind val] (nth (nth table ind) val) ) (reverse aa)))
          stringed (clojure.string/join "" transse)
          ]
      stringed)))

(defcheck solution-e80e7774
  (fn write-roman [n]
    (let [split-digits
                 (fn [n] (map #(parse-int (str %)) (str n)))
          digits (into [] (reverse (split-digits n)))
          process-place
                 (fn [n one-sym five-sym ten-sym]
                   (cond
                     (= n nil) ""
                     (<= n 3) (apply str (take n (repeat one-sym)))
                     (= n 4) (str one-sym five-sym)
                     (< n 9) (apply str five-sym (take (- n 5) (repeat one-sym)))
                     (= n 9) (str one-sym ten-sym)))]
      (str (process-place (get digits 3) "M" "M" "M")
        (process-place (get digits 2) "C" "D" "M")
        (process-place (get digits 1) "X" "L" "C")
        (process-place (get digits 0) "I" "V" "X")))))

(defcheck solution-e8546a0c
  (fn [x]
    (let [m {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M" 4 "IV" 9 "IX" 40 "XL" 90 "XC" 400 "CD" 900 "CM"}]
      (loop [n x
             a ""]
        (if (zero? n)
          a
          (let [i (into (sorted-map-by >) (filter #(< (first %) (+ n 1)) m))
                j (first i)]
            (recur
              (- n (first j))
              (str a (last j))
              )))))))

(defcheck solution-e87765c4
  (fn [n]
    (first
      (reduce (fn [[out r], [i s]]
                [(apply str (cons out (repeat (quot r i) s))) (rem r i)])
        ["" n]
        [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]))))

(defcheck solution-e8eef24f
  (fn int2roman [i]
    (let [table  (sorted-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                   90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          left (take-while (fn [[n s]] (<= n i)) table)
          current (last left)]
      (if (nil? current) "" (str (second current) (int2roman (- i (first current))))))))

(defcheck solution-e92ff136
  (fn [n]
    (let [numerals {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90
                    "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
          dec->roman (fn [n]
                       (loop [n n [[c v] & nums :as all] (reverse (sort-by val numerals)) acc []]
                         (cond
                           (zero? n) (apply str acc)
                           (> v n) (recur n nums acc)
                           :else (recur (- n v) all (conj acc c)))))]
      (dec->roman n))))

(defcheck solution-e981efaa
  (fn r [num]
    (if (> num 0)
      (let [[v l]
            (some #(if (<= (first %) num) %)
              (partition 2
                [1000 "M"
                 900 "CM"
                 500 "D"
                 400 "CD"
                 100 "C"
                 90 "XC"
                 50 "L"
                 40 "XL"
                 10 "X"
                 9 "IX"
                 5 "V"
                 4 "IV"
                 1 "I"]))]
        (str l (r (- num v)))))))

(defcheck solution-e98a9100
  {1 "I" 30 "XXX" 4 "IV" 140 "CXL" 827 "DCCCXXVII"
   3999 "MMMCMXCIX" 48 "XLVIII"})

(defcheck solution-e9ff1b79
  (fn [n]
    (let [
          fun (fn [val x v i]
                (cond
                  (= 0 val) ""
                  (= 1 val) i
                  (= 2 val) (str i i)
                  (= 3 val) (str i i i)
                  (= 4 val) (str i v)
                  (= 5 val) v
                  (= 6 val) (str v i)
                  (= 7 val) (str v i i)
                  (= 8 val) (str v i i i)
                  (= 9 val) (str i x)))]
      (str
        (fun (quot n 1000) "?" "?" "M")
        (fun (quot (mod n 1000) 100) "M" "D" "C")
        (fun (quot (mod n 100) 10) "C" "L" "X")
        (fun (mod n 10) "X" "V" "I")))))

(defcheck solution-ea0befa6
  (fn roman-numeral [number]
    (let [numerals-map {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90
                        "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}]
      (loop [n number
             remaining-numerals (sort-by val > numerals-map)
             accum []]
        (let [[numeral value] (first remaining-numerals)]
          (cond (zero? n) (apply str accum)
                (> value n) (recur n (rest remaining-numerals) accum)
                :else (recur (- n value) remaining-numerals (conj accum numeral))))))))

(defcheck solution-ea6eba8c
  (fn [n]
    (loop [m      '(1000 900 500 400 100 90 50 40 10 9 5 4 1)
           n      n
           result []]
      (if (= n 0)
        (apply str (map {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC"
                         50   "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"} result))
        (if
         (>= n (first m))
          (recur m (- n (first m)) (conj result (first m)))
          (recur (rest m) n result))))))

(defcheck solution-ea85cb07
  (fn [n]
    (let [rls [[\I \V] [\X \L] [\C \D] [\M]]
          digits (vec ((fn digs [x] (if-not (zero? x) (conj (digs (quot x 10)) (mod x 10)))) n))
          chs (fn [n c] (apply str (repeat n c)))
          digit-to-letters (fn [place digit]
                             (let [[l-one l-five] (rls place)]
                               (case digit
                                 4 (str l-one l-five)
                                 9 (str l-one ((rls (inc place)) 0))
                                 (if (< digit 5)
                                   (chs digit l-one)
                                   (str l-five (chs (- digit 5) l-one))
                                   ))))]
      (->> digits (map-indexed digit-to-letters) (reverse) (apply str)))))

(defcheck solution-ea8b02b5
  (fn [x]
    (apply str(flatten (map
                         #(let[a (first %2) b (second %2) c (last %2)]
                            (get ["" [a] [a a] [a a a] [a b] [b] [b a] [b a a] [b a a a] [a c] ] (mod (quot x %1) 10) ))
                         [1000 100 10 1]
                         ["M  " "CDM" "XLC" "IVX"]
                         )))))

(defcheck solution-eb0d805c
  (fn [n]
    (loop [tmpn n s ""]
      (if (= 0 tmpn)
        s
        (if (> tmpn 999)
          (recur (- tmpn 1000) (str s "M"))
          (if (> tmpn 899)
            (recur (- tmpn 900) (str s "CM"))
            (if (> tmpn 499)
              (recur (- tmpn 500) (str s "D"))
              (if (> tmpn 399)
                (recur (- tmpn 400) (str s "CD"))
                (if (> tmpn 99)
                  (recur (- tmpn 100) (str s "C"))
                  (if (> tmpn 89)
                    (recur (- tmpn 90) (str s "XC"))
                    (if (> tmpn 49)
                      (recur (- tmpn 50) (str s "L"))
                      (if (> tmpn 39)
                        (recur (- tmpn 40) (str s "XL"))
                        (if (> tmpn 9)
                          (recur (- tmpn 10) (str s "X"))
                          (if (> tmpn 8)
                            (recur (- tmpn 9) (str s "IX"))
                            (if (> tmpn 4)
                              (recur (- tmpn 5) (str s "V"))
                              (if (> tmpn 3)
                                (recur (- tmpn 4) (str s "IV"))
                                (recur (- tmpn 1) (str s "I"))))))))))))))))))

(defcheck solution-eb863ffb
  #(reduce str
     (map {1    "I"   , 4   "IV"  , 7   "VII",
           8    "VIII", 9   "IX"  , 20  "XX" ,
           30   "XXX" , 40  "XL"  , 90  "XC" ,
           100  "C"   , 800 "DCCC", 900 "CM" ,
           3000 "MMM"}
       (loop [c % d 10 o []]
         (if (zero? c) o
                       (let [r (rem c d)]
                         (recur (- c r) (* 10 d) (cons r o))))))))

(defcheck solution-eba76279
  (fn [n]
    (let [rmap {1000 "M", 900 "CM", 500 "D", 400 "CD", 100 "C", 90 "XC", 50 "L", 40 "XL", 10 "X", 9 "IX", 5 "V", 4 "IV", 1 "I"}]
      (first (reduce
               (fn [[s n] e]
                 (loop [s s n n]
                   (if (< n e)
                     [s n]
                     (recur (str s (rmap e)) (- n e)))))
               ["" n] (reverse (sort (keys rmap))))))))

(defcheck solution-ecb055a5
  (fn writeToRoman
    ([n] (writeToRoman n 0))
    ([n index]
     (if (= n 0)
       []
       (apply str (let [vals [1000 500 100 50 10 5 1] repr {1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I}
                        cur (nth vals index) letter (repr cur) q (quot n cur)]
                    (concat (cond
                              (= q 9) [letter (repr (nth vals (- index 2)))]
                              (= q 4) [letter (repr (nth vals (- index 1)))]
                              (>= q 5) (concat [(repr (nth vals (- index 1)))] (vec (repeat (- q 5) letter)))
                              :else (vec (repeat q letter)))
                      (writeToRoman (- n (* q cur))  (+ 2 index)))))))))

(defcheck solution-eceae101
  (fn [n]
    (let [m     [[1000  "M"]
                 [900  "CM"]
                 [500   "D"]
                 [400  "CD"]
                 [100   "C"]
                 [90   "XC"]
                 [50    "L"]
                 [40   "XL"]
                 [10    "X"]
                 [9    "IX"]
                 [5     "V"]
                 [4    "IV"]
                 [1     "I"]
                 ]]
      (loop [n n
             s []]
        (if (zero? n)
          (apply str s)
          (let [[v r] (first (filter (fn [[v _]] (<= v n)) m))]
            (recur (- n v) (concat s [r]))
            ))))
    ))

(defcheck solution-ed7f7b21
  (fn roman [n]
    (loop [i n
           a []]
      (let [conv [
                  [1000 "M"]
                  [900 "CM"]
                  [500 "D"]
                  [400 "CD"]
                  [100 "C"]
                  [90 "XC"]
                  [50 "L"]
                  [40 "XL"]
                  [10 "X"]
                  [9 "IX"]
                  [5 "V"]
                  [4 "IV"]
                  [1 "I"]]
            [cn cr] (first (filter #(>= i (first %) ) conv))]
        (if (zero? i)
          (apply str a)
          (recur (- i cn) (conj a cr)))
        ))))

(defcheck solution-ed8e4c09
  (fn iter
    ([n] (iter n ""))
    ([n s]
     (let
      [
       roman-numerals [["M" 1000] ["CM" 900] ["D" 500] ["C" 100]
                       ["XC" 90] ["L" 50] ["XL" 40] ["X" 10]
                       ["IX" 9] ["V" 5] ["IV" 4] ["I" 1] [nil 0]]
       greater-than-n (fn [[_ i]] (>= n i))
       [r i] (first (filter greater-than-n roman-numerals))]
       (if (nil? r) s (recur (- n i) (str s r)))))))

(defcheck solution-ed91a21c
  (fn [n]
    (let [numerals {1    [\I \V]
                    10   [\X \L]
                    100  [\C \D]
                    1000 [\M]}]
      (->>
        (iterate #(quot % 10) n)
        (take-while pos?)
        (map (fn [p x]
               (let [d (rem x 10)
                     [n1 n5] (numerals p)
                     [n10 _] (numerals (* 10 p))]
                 (condp > d
                   4 (repeat d n1)
                   5 [n1 n5]
                   9 (cons n5 (repeat (- d 5) n1))
                   [n1 n10])))
          (iterate #(* 10 %) 1))
        reverse flatten (apply str)))))

(defcheck solution-ed97e2cf
  (fn num-to-roman [n]
    (let [digits (zipmap [900  400   90   40   9    4  1000 500 100 50  10   5   1]
                   ["CM" "CD" "XC" "XL" "IX" "IV" "M" "D" "C" "L" "X" "V" "I"])]
      (apply str (first (reduce (fn [[res n] d]
                                  [(concat res (repeat (quot n d) (digits d))) (rem n d)])
                          [[] n] (sort > (keys digits))))))))

(defcheck solution-ed99a902
  (fn [n]
    (when (< n 4000)
      (let [romans [{:r "M" :a 1000 :lr "C" :l 100}
                    {:r "D" :a 500  :lr "C" :l 100}
                    {:r "C" :a 100  :lr "X" :l 10 }
                    {:r "L" :a 50   :lr "X" :l 10 }
                    {:r "X" :a 10   :lr "I" :l 1  }
                    {:r "V" :a 5    :lr "I" :l 1  }
                    {:r "I" :a 1            :l 0  }]]
        (loop [i 0
               acc []
               n n]
          (if (zero? n)
            (apply str acc)
            (let [roman (romans i)
                  k (:a roman)
                  restv (rem n k)
                  multi (/ (- n restv) k)
                  lowv (- k (:l roman))]
              (if (>= restv lowv)
                (recur (inc i) (concat acc (repeat multi (:r roman)) [(:lr roman) (:r roman)]) (- restv lowv))
                (recur (inc i) (concat acc (repeat multi (:r roman))) restv)))))))))

(defcheck solution-eda0db10
  (fn [n]
    (letfn [(k [x a]
              (let [q (quot x 10)
                    r (rem x 10)
                    ar (* a r)]
                (if (= 0 q)
                  [ar]
                  (cons ar (k q (* a 10))))))]
      (apply str
        (map
          {1 "I"   10 "X" 100 "C" 1000 "M"
           2 "II"  20 "XX" 200 "CC" 2000 "MM"
           3 "III" 30 "XXX" 300 "CCC" 3000 "MMM"
           4 "IV"  40 "XL" 400 "CD"
           5 "V"   50 "L" 500 "D"
           6 "VI"  60 "LX" 600 "DC"
           7 "VII" 70 "LXX" 700 "DCC"
           8 "VIII" 80 "LXXX" 800 "DCCC"
           9 "IX" 90 "XC" 900 "CM"}
          (reverse (k n 1)))))))

(defcheck solution-ee1779ce
  (fn r [n]
    (if (zero? n) ""
                  (let [[s v] (first (drop-while #(> (% 1) n)
                                       [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
                                        ["C" 100] ["XC" 90] ["L" 50] ["XL" 40]
                                        ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]))]
                    (str s (r (- n v)))))))

(defcheck solution-eed03daa
  (fn [n]
    (let [mapping [[1000 {1 [\M]
                          2 [\M \M]
                          3 [\M \M \M]}]
                   [ 100 {1 [\C]
                          2 [\C \C]
                          3 [\C \C \C]
                          4 [\C \D]
                          5 [\D]
                          6 [\D \C]
                          7 [\D \C \C]
                          8 [\D \C \C \C]
                          9 [\C \M]}]
                   [  10 {1 [\X]
                          2 [\X \X]
                          3 [\X \X \X]
                          4 [\X \L]
                          5 [\L]
                          6 [\L \X]
                          7 [\L \X \X]
                          8 [\L \X \X \X]
                          9 [\X \C]}]
                   [   1 {1 [\I]
                          2 [\I \I]
                          3 [\I \I \I]
                          4 [\I \V]
                          5 [\V]
                          6 [\V \I]
                          7 [\V \I \I]
                          8 [\V \I \I \I]
                          9 [\I \X]}] ]]
      (loop [n n
             [[k m] & kms] mapping
             res []]
        (if-not k
          (apply str res)
          (let [q (quot n k)
                r (rem n k)
                part (m q)]
            (recur r kms (if part (into res part) res))))))))

(defcheck solution-ef0741a5
  (fn rom [n]
    (let [nums [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"]
                [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]]
      (if (= 0 n)
        ""
        (let [curr (first (drop-while #(> (first %) n) nums))]
          (apply str (concat (second curr) (rom (- n (first curr))))))))))

(defcheck solution-ef1b380e
  (fn roman-numeral [n]
    (let [rn-map {1 "I"  4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L"
                  90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"}]
      (loop [num n base-nums (sort > (keys rn-map)) res-str ""]
        (if (= num 0) res-str
                      (recur (rem num (first base-nums))
                        (next base-nums)
                        (str res-str
                          (apply str (repeat (int (/ num (first base-nums)))
                                       (rn-map (first base-nums)))))))))))

(defcheck solution-ef84a284
  (fn [x]
    (letfn [(foo [val n]
              (let [vvals {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"}
                    letter (vvals val)
                    quantity (int (/ n val))]
                (cond (and (= 1000 val) (= 9 (int (/ (- n (* quantity val)) 100))))
                      [[[letter quantity] ["C" 1] ["M" 1]] (- (mod n val) 900)]
                      (and (= 100 val) (= 9 (int (/ (- n (* quantity val)) 10))))
                      [[[letter quantity] ["X" 1] ["C" 1]] (- (mod n val) 90)]
                      (and (= 50 val) (= 4 (int (/ n 10))))
                      [[[letter quantity] ["X" 1] ["L" 1]] (- n 40)]
                      (and (= 10 val) (= 9 (int (/ (- n (* quantity val)) 1))))
                      [[[letter quantity] ["I" 1] ["X" 1]] (- (mod n val) 9)]
                      (and (= 5 val) (= n 4))
                      [[[letter quantity] ["I" 1] ["V" 1]] 0]
                      :else [[letter quantity] (mod n val)])))
            (roman [num]
              (loop [r [] n num v [1000 500 100 50 10 5 1]]
                (if (empty? v)
                  r
                  (let [[rseqe nn] (foo (first v) n)]
                    (recur (conj r rseqe) nn (rest v))))))
            (rr [n]
              (filter #(not= 0 (second %))
                (partition 2 2 (flatten (roman n)))))
            (to-roman [r]
              (apply str (flatten (map #(repeat (second %) (first %)) r))))]
      (to-roman (rr x)))))

(defcheck solution-ef971cd1
  (fn [n]
    (letfn [(c [n]
              (condp <= n
                1000 (cons "M"  (c (- n 1000)))
                900  (cons "CM" (c (- n 900)))
                500  (cons "D"  (c (- n 500)))
                100  (cons "C"  (c (- n 100)))
                90   (cons "XC" (c (- n 90)))
                50   (cons "L"  (c (- n 50)))
                40   (cons "XL" (c (- n 40)))
                10   (cons "X"  (c (- n 10)))
                9    (cons "IX" (c (- n 9)))
                5    (cons "V"  (c (- n 5)))
                4    (cons "IV" (c (- n 4)))
                1    (cons "I"  (c (- n 1)))
                0 nil))]
      (clojure.string/join (c n)))))

(defcheck solution-f0156905
  (fn [n]
    (letfn [(number-map [n]
              (let [dap (fn digit-at-pos [n pos]
                          (nth (map #(parse-int %) (map str (reverse (str n)))) pos 0))]
                {:th (dap n 3)
                 :hu (dap n 2)
                 :te (dap n 1)
                 :on (dap n 0)}))

            (number-map->rom [m]
              (let [rom-map {:th (reductions str "" (repeat 9 "M"))
                             :hu ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                             :te ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                             :on ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]}
                    rom (fn [k v] (nth (get rom-map k) v))]
                (apply str (map #(rom % (m %)) [:th :hu :te :on]))))]
      (number-map->rom (number-map n)))))

(defcheck solution-f0606bd6
  #(letfn [(romanNum[n one five ten]
             (cond
               (<= n 3) (apply str (repeat n one))
               (= n 4) (str one five)
               (>= n 9) (str one ten)
               :else (str five (apply str (repeat (- n 5) one)))))
           (romanLetter[n]
             (cond
               (= n 1000) (list "M" nil nil)
               (= n 100) (list "C" "D" "M")
               (= n 10) (list "X" "L" "C")
               :else (list "I" "V" "X")))]
     (loop[n % u 1000 r ""]
       (if
        (= n 0) r
                (let[qn (quot n u)
                     rn (rem n u)
                     nr (str r (apply romanNum qn (romanLetter u)))]
                  (recur (- n (* qn u)) (quot u 10) nr))))))

(defcheck solution-f0b173d4
  (fn [n]
    (apply str (last (let [v (sorted-map-by > 1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M)]
                       (reduce (fn [[a res] b]
                                 (let [q (quot a b)
                                       r (rem a b)
                                       b-1 (last (filter #(> % b) (keys v)))]
                                   [r (if (> q 3)
                                        (if (= (last res) (v b-1))
                                          (into (vec (butlast res)) [(v b) (v (last (filter #(> % b-1) (keys v))))])
                                          (into res [(v b) (v b-1)]))
                                        (into res (repeat q (v b))))]))
                         [n []] (keys v)))))))

(defcheck solution-f0fc3a3e
  (fn roman[i] (apply str (let [a (quot i 1000) b (quot (rem i 1000) 100) c (quot (rem i 100) 10) d (rem i 10)]
                            (concat (repeat a "M")
                              (cond (< b 4) (repeat b "C")
                                    (= b 4) ["C" "D"]
                                    (= 9 b) ["CM"]
                                    (= b 5) ["D"]
                                    (<= 6 b) [(str "D" (apply str (repeat (- b 5) "C")))])
                              (cond (< c 4) (repeat c "X")
                                    (= c 4) ["X" "L"]
                                    (= 9 c) ["XC"]
                                    (= c 5) ["L"]
                                    (<= 6 c) [(str "L" (apply str (repeat (- c 5) "X")))])
                              (cond (< d 4) (repeat d "I")
                                    (= d 4) ["I" "V"]
                                    (= 9 d) ["IX"]
                                    (= d 5) ["V"]
                                    (<= 6 d) [(str "V" (apply str (repeat (- d 5) "I")))])
                              )))))

(defcheck solution-f1b370c2
  (fn [i]
    (let [roman (rseq (sorted-map 1000 "M" 900 "CM"
                        500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L"
                        40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"))]
      (loop [i i s ""]
        (if (zero? i)
          s
          (let [[v sr]
                (first (filter (fn [[k v]] (<= k i)) roman))]
            (recur (- i v) (str s sr))))))))

(defcheck solution-f1b39c2a
  (fn [v] (loop [x v r ""]
            (if-let [[k s] (some #(if (<= (first %) x) %)
                             (map vector [1000 900 500  400 100  90   50  40   10   9   5    4   1 ]
                               ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]))]
              (recur (- x k) (str r s))
              r))))

(defcheck solution-f1ec4512
  (fn [n]
    (let [rmap (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I" 0 "")]
      (loop [n n s ""]
        (if (zero? n) s
                      (let [[v sr] (some #(if (<= (key %) n) %) rmap)]
                        (recur (- n v) (str s sr))))))))

(defcheck solution-f21d0c68
  (fn to-roman [n]
    (if (= 0 n)
      ""
      (let [values
                     {"I" 1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000
                      "IV"  4, "IX"	9, "XL"	40 "XC" 90 "CD"	400 "CM"	900}
            possible (filter (fn [[k v]]
                               (<= v n))
                       values)
            [numeral value] (reduce (fn [[k v] [k' v']]
                                      (if (> v' v)
                                        [k' v']
                                        [k v]))
                              possible)

            ]
        (str numeral (to-roman (- n value)))))))

(defcheck solution-f24344ab
  #(let [numerals (array-map 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
     (loop [n %
            s ""]
       (if (zero? n)
         s
         (let [i (first (filter (partial >= n) (keys numerals)))]
           (recur (- n i) (str s (get numerals i))))
         ))))

(defcheck solution-f2d171f9
  (fn [i]
    (reduce
      (fn [c [a b]] (clojure.string/replace c (re-pattern a) b))
      (apply str
        (flatten
          (last
            (reduce (fn [[base roman] [a b]] [(- base (* a (quot base a))) (conj roman (repeat (quot base a) b))])
              [i []]
              [[1000 \M] [500 \D] [100 \C] [50 \L] [10 \X] [5 \V] [1 \I]]))))
      [["VIIII" "IX"] ["IIII" "IV"] ["LXXXX" "XC"] ["XXXX" "XL"]["DCCCC" "CM"]])))

(defcheck solution-f2f12b23
  (fn [n]
    (let [rn (sorted-map 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")
          biggest-fit (fn [x] (last (filter #(<= % x) (keys rn))))]
      (loop [remainder n roman ""]
        (if (zero? remainder)
          roman
          (let [next-num (biggest-fit remainder)]
            (recur (- remainder next-num) (str roman (rn next-num)))))))))

(defcheck solution-f32d9556
  (fn [n]
    (let [number-strings (sorted-map-by >
                           1 "I" 4 "IV" 5 "V" 9 "IX"
                           10 "X" 40 "XL" 50 "L" 90 "XC"
                           100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M")]
      (letfn [(next-number-string [n]
                (some (fn [[number string]]
                        (cond (>= n number) [number string]
                              :else nil))
                  number-strings))
              (roman-characters [n]
                (if (= n 0)
                  ()
                  (let [[next-number next-string] (next-number-string n)]
                    (lazy-cat next-string (roman-characters (- n next-number))))))]
        (apply str (roman-characters n))))))

(defcheck solution-f444974e
  (fn [n]
    (let [r ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          d [1000 900 500 400 100 90 50 40 10 9 5 4 1]]
      (loop [rm n, acc "", i 0]
        (cond (= 0 rm) acc
              (>= (- rm (d i)) 0) (recur (- rm (d i)) (str acc (r i)) 0)
              :else (recur rm acc (inc i)))))))

(defcheck solution-f47b9e1b
  (letfn [(f [b l n]
            (-> [(apply str (repeat (int (/ n b)) l)) (rem n b)]))]
    (let [fs (map (fn [[b l]] (partial f b l))
               (partition 2 [1000 \M 900 "CM" 500 \D 400 "CD" 100 \C 90 "XC"
                             50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I]))]
      (fn [n]
        (loop [fs fs n n v []]
          (if (seq fs)
            (let [[cs r] ((first fs) n)]
              (recur (rest fs) r (conj v cs)))
            (apply str v)))))))

(defcheck solution-f4853475
  (fn [x]
    (let [itor {1 "I", 5 "V", 10 "X", 50 "L", 100 "C", 500 "D", 1000 "M"}
          m {1 [1], 2 [1 1], 3 [1 1 1], 4 [1 5], 5 [5],
             6 [5 1], 7 [5 1 1], 8 [5 1 1 1], 9 [1 10]}]
      (loop [n x, digit 1, res ""]
        (if (zero? n)
          res
          (recur (quot n 10)
            (* digit 10)
            (str (apply str (map #(itor (* digit %)) (m (rem n 10)))) res)))))))

(defcheck solution-f4b2a812
  (fn [n]
    (let [romans1 {1000 "M"  900 "CM" 500 "D" 100 "C" 90 "XC" 50 "L" 40 "XL"
                   10 "X"  9 "IX" 8 "VIII" 7 "VII"
                   6 "VI" 5 "V" 4 "IV" 3 "III" 2 "II" 1 "I"}]
      (letfn [(find-range [n]
                (first (filter #(>= n %) (sort > (keys romans1)))))
              (rom [n acc]
                (if (= 0 n)
                  acc
                  (let [r (find-range n)]
                    (rom (- n r) (conj acc (romans1 r))))))]
        (apply str (rom n []))))))

(defcheck solution-f4ee1d97
  (fn wnum [n]
    (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
              90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          m (some #(when (>= (- n %) 0) %) (keys r))]
      (when-not (nil? m)
        (str (r m) (wnum (- n m)))))))

(defcheck solution-f4f2578a
  (fn [num]
    (let [m [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"]
             [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"]
             [5 "V"] [4 "IV"] [1 "I"]]]
      (loop [n num s "" index 0]
        (if (> index 12) s
                         (let [[k v] (m index)
                               t (quot n k)
                               r (- n (* t k))
                               s2 (apply str (repeat t v))]
                           (recur r (str s s2) (inc index))))))))

(defcheck solution-f5fd9d74
  #(let [r {1000 "M"
            900  "CM"
            500  "D"
            400  "CD"
            100  "C"
            90   "XC"
            50   "L"
            40   "XL"
            10   "X"
            9    "IX"
            5    "V"
            4    "IV"
            1    "I"
            0    ""}]
     ((fn f [m [k & s] a]
        (if s
          (if (>= m k)
            (f (- m k) (cons k s) (str a (r k)))
            (f m s a))
          a))
      % (sort > (keys r)) "")))

(defcheck solution-f6d1d4f2
  (fn wnum [n]
    (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
              90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
          m (some #(when (>= (- n %) 0) %) (keys r))]
      (when-not (nil? m)
        (str (r m) (wnum (- n m)))))))

(defcheck solution-f6d5fcdc
  (fn roman [x] (let [values {"M" 1000, "CM" 900, "D" 500, "C" 100, "XC" 90, "L" 50, "XL" 40, "X" 10, "IX" 9, "V" 5, "IV" 4, "I" 1}]
                  (loop [so-far ""
                         number x]
                    (if (zero? number)
                      so-far
                      (let [highest (apply (partial max-key val) (filter #(>= number (val %)) values))]
                        (recur (str so-far (key highest)) (- number (val highest)))))))))

(defcheck solution-f6d80da
  (fn [n]
    (let [basis {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
          validpairs ["IV" "IX" "XL" "XC" "CD" "CM"]
          pairs (for [[l r] validpairs]
                  {(str l r) (reduce - (map (fn [l] (basis (str l))) [r l]))})
          singles (into basis pairs)
          byval (into {} (map (fn [[k v]] [v k]) singles))
          mults (mapcat
                  (fn [[k v]]
                    (take-while (fn [[rn rnv]]
                                  (and (not (byval rnv))
                                       (< rnv 4000)))
                      (iterate
                        (fn [[rn rnv]]
                          [(str rn k) (+ rnv v)])
                        [(str k k) (+ v v)])))
                  basis)
          allnums (into singles mults)
          byval (into {} (map (fn [[k v]] [v k]) allnums))
          nums (keys byval)]
      (letfn [(->rom [n]
                (if (pos? n)
                  (let [avail (filter #(<= % n) nums)]
                    (if (seq avail)
                      (let [m (apply max avail)
                            s (byval m)]
                        (str s (->rom (- n m))))
                      ""))
                  ""))]
        (->rom n)))))

(defcheck solution-f8294eba
  (fn roman [n]
    (let
     [thousands (quot n 1000)
      ex-thou (mod n 1000)
      hundreds (quot ex-thou 100)
      ex-hund (mod n 100)
      tens (quot ex-hund 10)
      ones (mod n 10)
      thou-digits (repeat thousands \M)
      hun-digits (cond
                   (= hundreds 9) [\C \M]
                   (>= hundreds 5)
                   (concat [\D] (repeat (- hundreds 5) \C))
                   (= hundreds 4) [\C \D]
                   :else (repeat hundreds \C))
      ten-digits (cond
                   (= tens 9) [\X \C]
                   (>= tens 5)
                   (concat [\L] (repeat (- tens 5) \X))
                   (= tens 4) [\X \L]
                   :else (repeat tens \X))
      one-digits (cond
                   (= ones 9) [\I \X]
                   (>= ones 5)
                   (concat [\V] (repeat (- ones 5) \I))
                   (= ones 4) [\I \V]
                   :else (repeat ones \I))]
      (apply str
        (concat thou-digits hun-digits ten-digits one-digits)))))

(defcheck solution-f83e9c3e
  (fn [s]
    (let [k [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
          m (zipmap k
              ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
              )]
      (loop [a s result ""]
        (if (= 0 a)
          result
          (let [n (first (filter #(>= a %) k))]
            (recur (- a n) (str result (m n)))))))))

(defcheck solution-f94e039d
  (fn [arabic]
    (let [number-vec ((fn [num]
                        (loop [result [] div-num num]
                          (if (= 0 div-num)
                            result
                            (recur (conj result (mod div-num 10)) (int (/ div-num 10)))
                            )
                          )
                        ) arabic)]
      (loop [result "" elements number-vec i 0]
        (if elements
          (cond
            (= i 0) (cond
                      (= 1 (first elements)) (recur (str "I" result) (next elements) (inc i))
                      (= 2 (first elements)) (recur (str "II" result) (next elements) (inc i))
                      (= 3 (first elements)) (recur (str "III" result) (next elements) (inc i))
                      (= 4 (first elements)) (recur (str "IV" result) (next elements) (inc i))
                      (= 5 (first elements)) (recur (str "V" result) (next elements) (inc i))
                      (= 6 (first elements)) (recur (str "VI" result) (next elements) (inc i))
                      (= 7 (first elements)) (recur (str "VII" result) (next elements) (inc i))
                      (= 8 (first elements)) (recur (str "VIII" result) (next elements) (inc i))
                      (= 9 (first elements)) (recur (str "IX" result) (next elements) (inc i))
                      (= 0 (first elements)) (recur result (next elements) (inc i))
                      )
            (= i 1) (cond
                      (= 1 (first elements)) (recur (str "X" result) (next elements) (inc i))
                      (= 2 (first elements)) (recur (str "XX" result) (next elements) (inc i))
                      (= 3 (first elements)) (recur (str "XXX" result) (next elements) (inc i))
                      (= 4 (first elements)) (recur (str "XL" result) (next elements) (inc i))
                      (= 5 (first elements)) (recur (str "L" result) (next elements) (inc i))
                      (= 6 (first elements)) (recur (str "LX" result) (next elements) (inc i))
                      (= 7 (first elements)) (recur (str "LXX" result) (next elements) (inc i))
                      (= 8 (first elements)) (recur (str "LXXX" result) (next elements) (inc i))
                      (= 9 (first elements)) (recur (str "XC" result) (next elements) (inc i))
                      (= 0 (first elements)) (recur result (next elements) (inc i))
                      )
            (= i 2) (cond
                      (= 1 (first elements)) (recur (str "C" result) (next elements) (inc i))
                      (= 2 (first elements)) (recur (str "CC" result) (next elements) (inc i))
                      (= 3 (first elements)) (recur (str "CCC" result) (next elements) (inc i))
                      (= 4 (first elements)) (recur (str "CD" result) (next elements) (inc i))
                      (= 5 (first elements)) (recur (str "D" result) (next elements) (inc i))
                      (= 6 (first elements)) (recur (str "DC" result) (next elements) (inc i))
                      (= 7 (first elements)) (recur (str "DCC" result) (next elements) (inc i))
                      (= 8 (first elements)) (recur (str "DCCC" result) (next elements) (inc i))
                      (= 9 (first elements)) (recur (str "CM" result) (next elements) (inc i))
                      (= 0 (first elements)) (recur result (next elements) (inc i))
                      )
            (= i 3) (cond
                      (= 1 (first elements)) (recur (str "M" result) (next elements) (inc i))
                      (= 2 (first elements)) (recur (str "MM" result) (next elements) (inc i))
                      (= 3 (first elements)) (recur (str "MMM" result) (next elements) (inc i))
                      (= 4 (first elements)) (recur (str "MMMM" result) (next elements) (inc i))
                      )
            )

          result
          )

        )
      )
    ))

(defcheck solution-f9612928
  (fn do-rom[number]((fn mkrom [rnums n]
                       (cond
                         (= 0 n) (apply str rnums)
                         (= 1 n) "I"
                         :else
                         (let [bkmp (sorted-map 100 \C 10 \X 1 \I)
                               mp (sorted-map 1000 \M 500 \D 100 \C 50 \L 10 \X 5 \V 1 \I)
                               times (apply merge (reverse (map #(sorted-map (quot n (first %)) (last %)) mp)))
                               rmap {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}
                               check (if (zero? (first (first times))) (first (first (rest times)))
                                                                       (first (first times)))
                               n-check (if (zero? (first (first times))) (last (first (rest times)))
                                                                         (last (first times)))
                               minus (last (filter #(< (first %) n) bkmp))
                               postminus (last (first times))
                               f-rom-num (if (< check 4) (repeat check n-check)
                                                         (vector (last minus) postminus))
                               f-num (if (< check 4) (* check (get rmap (first f-rom-num)))
                                                     (- (get rmap (last f-rom-num)) (get rmap (first f-rom-num))))
                               t-rom-num (vector (last minus) postminus)
                               t-num (- (get rmap postminus) (get rmap (last minus)))
                               use-t (and (>= (- n t-num) 0) (< (- n t-num) (- n f-num)))
                               rom-num (if use-t t-rom-num f-rom-num)
                               num (if use-t t-num f-num)]
                           (mkrom (flatten (cons rnums rom-num)) (- n num))))) [] number)))

(defcheck solution-f9c97468
  (fn wr
    [n]
    (let [dm (into (sorted-map) {1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X" 40 "XL" 50 "L" 90 "XC" 100 "C" 400 "CD" 500 "D" 900 "CM" 1000 "M"})]
      (loop [r n s ""]
        (if (= r 0)
          (str s)
          (let [d (last (take-while #(<= (key %) r) dm))]
            (recur (- r (key d)) (str s (val d)))))))))

(defcheck solution-fa22abf6
  (fn [n]
    (let [rnums
          [["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
           ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
           ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
           ["" "M" "MM" "MMM" "MMMM" "MMMMM" "MMMMMM"
            "MMMMMMM" "MMMMMMMM" "MMMMMMMMM"]]]
      (apply
        str
        (reverse
          (map
            (fn [[dc rs]]
              (rs (parse-int (str dc))))
            (map vector
              (reverse (str n))
              rnums)))))))

(defcheck solution-fa56c8b7
  (fn roman# [n & s]
    (let [s (first (or s '("")))]
      (if (<= n 0) s
                   (cond
                     (>= n 1000) (roman# (- n 1000) (str s "M"))
                     (>= n 900) (roman# (- n 900) (str s "CM"))
                     (>= n 500) (roman# (- n 500) (str s "D"))
                     (>= n 400) (roman# (- n 400) (str s "CD"))
                     (>= n 100) (roman# (- n 100) (str s "C"))
                     (>= n 90) (roman# (- n 90) (str s "XC"))
                     (>= n 50) (roman# (- n 50) (str s "L"))
                     (>= n 40) (roman# (- n 40) (str s "XL"))
                     (>= n 10) (roman# (- n 10) (str s "X"))
                     (>= n 9) (roman# (- n 9) (str s "IX"))
                     (>= n 5) (roman# (- n 5) (str s "V"))
                     (>= n 4) (roman# (- n 4) (str s "IV"))
                     (>= n 1) (roman# (- n 1) (str s "I")))))))

(defcheck solution-fa627103
  (fn roman [x]
    (cond
      (>= x 1000) (str "M" (roman (- x 1000)))
      (>= x 900) (str "CM" (roman (- x 900)))
      (>= x 500) (str "D" (roman (- x 500)))
      (>= x 400) (str "CD" (roman (- x 400)))
      (>= x 100) (str "C" (roman (- x 100)))
      (>= x 90) (str "XC" (roman (- x 90)))
      (>= x 50) (str "L" (roman (- x 50)))
      (>= x 40) (str "XL" (roman (- x 40)))
      (>= x 10) (str "X" (roman (- x 10)))
      (>= x 9) (str "IX" (roman (- x 9)))
      (>= x 5) (str "V" (roman (- x 5)))
      (>= x 4) (str "IV" (roman (- x 4)))
      (>= x 1) (str "I" (roman (- x 1)))
      (= x 0) ""
      )))

(defcheck solution-fad37aee
  (let [roman (array-map 1000 "M" 900 "CM" 800 "DCCC" 500 "D" 400 "CD"
                100 "C" 90 "XC" 80 "LXXX" 50 "L" 40 "XL" 10 "X"
                9 "IX" 8 "VIII" 5 "V" 4 "IV" 1 "I")
        sizes (keys roman)]
    (fn dec->rom
      ([n]
       (dec->rom n []))
      ([n accu]
       (if (zero? n)
         (apply str (map roman accu))
         (let [subtr (first (filter #(<= % n) sizes))]
           (recur (- n subtr) (conj accu subtr))))))))

(defcheck solution-fb4a6ad7
  (fn p104 [n]
    (letfn [(div [m n k] (if (<= n 0) m (div (conj m [k (mod n 10)]) (int (/ n 10)) (* k 10))))]
      (let [m (div {} n 1) ks (sort (keys m))
            vs (fn [k v m] (cond
                             (= k 1)    (cond (= v 9) "IX" (= v 4) "IV" true (str (if (<= 5 v) "V" "") (apply str (replicate m "I"))))
                             (= k 10)   (cond (= v 9) "XC" (= v 4) "XL" true (str (if (<= 5 v) "L" "") (apply str (replicate m "X"))))
                             (= k 100)  (cond (= v 9) "CM" (= v 4) "DC" true (str (if (<= 5 v) "D" "") (apply str (replicate m "C"))))
                             (= k 1000) (apply str (replicate v "M"))
                             ))]
        (apply str (reverse (for [k ks] (vs k (m k) (mod (m k) 5)))))))))

(defcheck solution-fbee1042
  (fn romans [x]
    (cond
      (>= x 1000) (clojure.string/join ["M" (romans (- x 1000))])
      (>= x 900) (clojure.string/join ["CM" (romans (- x 900))])
      (>= x 500) (clojure.string/join ["D" (romans (- x 500))])
      (>= x 400) (clojure.string/join ["CD" (romans (- x 400))])
      (>= x 100) (clojure.string/join ["C" (romans (- x 100))])
      (>= x 90) (clojure.string/join ["XC" (romans (- x 90))])
      (>= x 50) (clojure.string/join ["L" (romans (- x 50))])
      (>= x 40) (clojure.string/join ["XL" (romans (- x 40))])
      (>= x 10) (clojure.string/join ["X" (romans (- x 10))])
      (>= x 9) (clojure.string/join ["IX" (romans (- x 9))])
      (>= x 5) (clojure.string/join ["V" (romans (- x 5))])
      (>= x 4) (clojure.string/join ["IV" (romans (- x 4))])
      (>= x 1) (clojure.string/join ["I" (romans (- x 1))])
      :else "")))

(defcheck solution-fc12793e
  (fn roman-encode
    [v]
    (let [rn (sorted-map-by > 1 "I"
               4 "IV" 5 "V"
               9 "IX" 10 "X"
               40 "XL" 50 "L"
               90 "XC" 100 "C"
               400 "CD" 500 "D"
               900 "CM" 1000 "M")]
      (letfn [(f [v [rf & r]]
                (when-not (nil? rf)
                  (let [q  (quot v rf)
                        rm (rem v rf)]
                    (if (= 0 q)
                      (f rm r)
                      (str (apply str (repeat q (rn rf)))
                        (f rm r))))))]
        (f v (keys rn))))))

(defcheck solution-fc1b468f
  (fn roman [arabic]
    (letfn [(recurse [r a] (str r (roman (- arabic a))))]
      (cond (>= arabic 1000) (recurse "M" 1000)
            (>= arabic 900) (recurse "CM" 900)
            (>= arabic 500) (recurse "D" 500)
            (>= arabic 100) (recurse "C" 100)
            (>= arabic 90) (recurse "XC" 90)
            (>= arabic 40) (recurse "XL" 40)
            (>= arabic 10) (recurse "X" 10)
            (>= arabic 9) (recurse "IX" 9)
            (>= arabic 5) (recurse "V" 5)
            (>= arabic 4) (recurse "IV" 4)
            (>= arabic 1) (recurse "I"  1)))))

(defcheck solution-fc47780e
  (fn toRomansX [n] (clojure.string/join (
                                          (fn toRomansRec [n]
                                            (cond
                                              (>= n 1000) (cons "M" (toRomansRec (- n 1000)))
                                              (>= n 900) (cons "CM" (toRomansRec (- n 900)))
                                              (>= n 500) (cons "D" (toRomansRec (- n 500)))
                                              (>= n 400) (cons "CD" (toRomansRec (- n 400)))
                                              (>= n 100) (cons "C" (toRomansRec (- n 100)))
                                              (>= n 90) (cons "XC" (toRomansRec (- n 90)))
                                              (>= n 50) (cons "L" (toRomansRec (- n 50)))
                                              (>= n 40) (cons "XL" (toRomansRec (- n 40)))
                                              (>= n 10) (cons "X" (toRomansRec (- n 10)))
                                              (>= n 9) (cons "IX" (toRomansRec (- n 9)))
                                              (>= n 5) (cons "V" (toRomansRec (- n 5)))
                                              (>= n 4) (cons "IV" (toRomansRec (- n 4)))
                                              (>= n 1) (cons "I" (toRomansRec (- n 1)))
                                              :else [""]
                                              )
                                            )
                                          n))))

(defcheck solution-fc5a65b8
  (fn [n]
    (let [numeral-and-pair-values [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          numeral-and-pairs ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]]
      (loop [n n, i 0, digits []]
        (if (zero? n)
          (apply str digits)
          (let [times (quot n (numeral-and-pair-values i))
                sub (* times (numeral-and-pair-values i))]
            (recur (- n sub) (inc i) (into digits (repeat times (numeral-and-pairs i))))
            ))))))

(defcheck solution-fc77a807
  #(loop [n % t "IVXLCDM" r '()]
     (if (= n 0) (apply str r)
                 (let [m (mod n 10)]
                   (case m
                     (9 4) (recur (- n (dec m)) t (conj r (nth t (quot m 4))))
                     (recur (quot n 10) (drop 2 t)
                       (conj (reduce conj r (repeat (mod m 5) (first t)))
                         (when (> m 4)
                           (second t)))))))))

(defcheck solution-fc820ba8
  (fn [x]
    (let [s {1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L", 90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M"}]
      (loop [n x [[weight l] & nums :as all] (reverse (sort-by key s)) result []]
        (cond
          (zero? n) (apply str result)
          (> weight n) (recur n nums result)
          :else (recur (- n weight) all (conj result l)))))))

(defcheck solution-fca61659
  (fn [n]
    (let [roman-digit
          (fn dig [n digit]
            (let [chars {1 "I"
                         5 "V"
                         10 "X"
                         50 "L"
                         100 "C"
                         500 "D"
                         1000 "M"}
                  icon (chars digit)
                  half (chars (* 5 digit))
                  next-icon (chars (* 10 digit))
                  n-icon (apply str (repeat n icon))]
              (if (not next-icon)
                (apply str (repeat n icon))
                (cond
                  (< n 4) n-icon
                  (= n 4) (str icon half)
                  (< 4 n 9) (str half (dig (- n 5) digit))
                  (> n 8) (str icon next-icon)
                  :else ""))))]
      (str (roman-digit (quot n 1000) 1000)
        (roman-digit (quot (rem n 1000) 100) 100)
        (roman-digit (quot (rem n 100) 10) 10)
        (roman-digit (rem n 10) 1)))))

(defcheck solution-fcad45c8
  (fn num->rome
    ([num] (num->rome num ""))
    ([num repr]
     (if (= num 0) repr
                   (let [romans [[1000 "M"]
                                 [900 "CM"]
                                 [500 "D"]
                                 [400 "CD"]
                                 [100 "C"]
                                 [90 "XC"]
                                 [50 "L"]
                                 [40 "XL"]
                                 [10 "X"]
                                 [9 "IX"]
                                 [5 "V"]
                                 [4 "IV"]
                                 [1 "I"]]
                         distances (map (fn [[number repr]] [(- num number) number repr]) romans)
                         legit-dist (filter (fn [[dist _ _]] (>= dist 0)) distances)
                         [d num' repr'] (apply min-key (fn [[dist _ _]] dist) legit-dist)]
                     (recur (Math/abs (- num num')) (str repr repr')))))))

(defcheck solution-fcff2d8c
  (fn [n]
    (let [rs (partition 3 2 "IVXLCDM  ")
          rm (fn [n [o f t]]
               (cond (= n 9) (str o t)
                     (> n 4) (apply str f (repeat (- n 5) o))
                     (= n 4) (str o f)
                     (> n 0) (apply str (repeat n o))))
          nm (fn f [[p & s] n]
               (when p
                 (let [q (quot n 10)
                       r (mod n 10)]
                   (cons (rm r p) (lazy-seq (f s q))))))]
      (->> n
        (nm rs)
        reverse
        (apply str)))))

(defcheck solution-fd2ca0a4
  (fn [num]
    (loop [res "" num num]
      (if-let [[N n] (some #(when (>= num (peek %)) %)
                       [[\M 1000] ["CM" 900] [\D 500] ["CD" 400]
                        [\C 100] ["XC" 90] [\L 50] ["XL" 40]
                        [\X 10] ["IX" 9] [\V 5] ["IV" 4] [\I 1]])]
        (recur (str res N) (- num n))
        res))))

(defcheck solution-fda12df3
  (fn f [x]
    (let [s (first (filter #(<= 0 (- x (key %))) (sorted-map-by > 1000 \M 900 "CM" 500 \D 400 "CD" 100 \C 90 "XC" 50 \L 40 "XL" 10 \X 9 "IX" 5 \V 4 "IV" 1 \I)))
          v (- x (key s))]
      (if (= 0 v)
        (str (val s))
        (str (val s) (f v))))))

(defcheck solution-fdb223e5
  #(apply str
     (loop [n %, st ""
            v [1000 900 500 400 100 90 50 40 10 9 5 4 1]
            s (re-seq #"\w+" "M CM D CD C XC L XL X IX V IV I")]
       (if (zero? n)
         st
         (if (< n (first v))
           (recur n st (rest v) (rest s))
           (recur (- n (first v)) (str st (first s)) v s))))))

(defcheck solution-fdd48897
  (fn [arg]
    (let [th (quot arg 1000)
          hu (quot (rem arg 1000) 100)
          te (quot (rem (rem arg 1000) 100) 10)
          on (rem (rem (rem arg 1000) 100) 10)
          hustr (cond (> hu 8) "CM"
                      (> hu 5) (apply str "D" (repeat (- hu 5) "C"))
                      (= hu 5) "D"
                      (> hu 3) "CD"
                      :else (apply str (repeat hu "C")))
          testr (cond (> te 8) "XC"
                      (> te 5) (apply str "L" (repeat (- te 5) "X"))
                      (= te 5) "L"
                      (> te 3) "XL"
                      :else (apply str (repeat te "X")))
          onstr (cond (> on 8) "IX"
                      (> on 5) (apply str "V" (repeat (- on 5) "I"))
                      (= on 5) "V"
                      (> on 3) "IV"
                      :else (apply str (repeat on "I")))]
      (str (apply str (repeat th "M")) hustr testr onstr))))

(defcheck solution-fdfe4643
  (fn dr [digit]
    (let [s ""
          a1 (quot digit 1000)
          [s1 b1] (if (> a1 0) [(str s (apply str (repeat a1 "M"))) (- digit (* a1 1000))] [s digit])
          [s2 b2] (cond (>= b1 900) [(str s1 "CM") (- b1 900)]
                        (and (< b1 900) (>= b1 500)) [(str s1 "D") (- b1 500)]
                        (and (< b1 500) (>= b1 400)) [(str s1 "CD") (- b1 400)]
                        :else [s1 b1])
          a2 (quot b2 100)
          [s3 b3] (if (> a2 0) [(str s2 (apply str (repeat a2 "C"))) (- b2 (* a2 100))] [s2 b2])
          [s4 b4] (cond (>= b3 90) [(str s3 "XC") (- b3 90)]
                        (and (< b3 90) (>= b3 50)) [(str s3 "L") (- b3 50)]
                        (and (< b3 50) (>= b3 40)) [(str s3 "XL") (- b3 40)]
                        :else [s3 b3])
          a3 (quot b4 10)
          [s5 b5] (if (> a3 0) [(str s4 (apply str (repeat a3 "X"))) (- b4 (* a3 10))] [s4 b4])
          s6 (cond (= b5 9) (str s5 "IX")
                   (= b5 4) (str s5 "IV")
                   (and (< b5 9) (>= b5 5)) (str s5 "V" (apply str (repeat (rem b5 5) "I")))
                   :else (str s5 (apply str (repeat (rem b5 5) "I"))))]
      s6)))

(defcheck solution-ff33e09d
  (fn [n]
    (let [t [[1000 \M] [500 \D]
             [100 \C] [50 \L]
             [10 \X] [5 \V] [1 \I]]]
      (loop [n n s "" i 0]
        (if (or (= i 7) (= n 0))
          s
          (if (= n 3999)
            "MMMCMXCIX"
            (let [[c y] (t i)
                  d (int (/ n c))]
              (if (> d 0)
                (if (< d 4)
                  (recur (- n (* d c)) (apply str (concat s (repeat d y))) (+ i 1))
                  (recur (- n (* d c)) (apply str (concat s [y (nth (t (- i 1)) 1)])) (+ i 1)))
                (recur n s (+ i 1))))))))))

(defcheck solution-ff4d6d4f
  (fn to-roman [n]
    (loop [num n roman []]
      (cond
        (<= num 0) (apply str roman)
        (< num 4) (recur (- num 1) (conj roman "I"))
        (< num 5) (recur (- num 4) (conj roman "IV"))
        (< num 9) (recur (- num 5) (conj roman "V"))
        (< num 10) (recur (- num 9) (conj roman "IX"))
        (< num 40) (recur (- num 10) (conj roman "X"))
        (< num 50) (recur (- num 40) (conj roman "XL"))
        (< num 90) (recur (- num 50) (conj roman "L"))
        (< num 100) (recur (- num 90) (conj roman "XC"))
        (< num 400) (recur (- num 100) (conj roman "C"))
        (< num 500) (recur (- num 400) (conj roman "CD"))
        (< num 900) (recur (- num 500) (conj roman "D"))
        (< num 1000) (recur (- num 900) (conj roman "CM"))
        (< num 10000) (recur (- num 1000) (conj roman "M"))
        :else "not there yet"))))

(defcheck solution-ff9b35a0
  (fn r [x]
    (if (= x 0) ""
                (let [R (sorted-map 1 "I", 4 "IV", 5 "V", 9 "IX", 10 "X", 40 "XL", 50 "L",
                          90 "XC", 100 "C", 400 "CD", 500 "D", 900 "CM", 1000 "M")
                      m (first (filter #(>= x (key %)) (reverse R)))]
                  (str (val m) (r (- x (key m))))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-104))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

