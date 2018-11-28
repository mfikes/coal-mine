(ns coal-mine.problem-92
  (:require [coal-mine.checks :refer [defcheck-92] :rename {defcheck-92 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-103c18dd
  (fn p92 [col]
    (let [ str-symbol-map { "M" :m ,"D" :d, "C" :c , "L" :l ,"X" :x "V" :v "I" :i}
          symbo-v-map    {  :m 1000 , :d, 500  :c  100, :l 50 , :x 10  :v  5  :i 1}
          to-symbol (fn [c] (get str-symbol-map  (str c)))
          parse-roma (fn [col]
                       (let [symbol-list (reverse col)]
                         (loop [result (symbo-v-map (first symbol-list))  lastv (symbo-v-map (first symbol-list)) data (rest symbol-list)]
                           (let [current-v (symbo-v-map (first data))]
                             (if (empty? data)
                               result
                               (recur (if (< current-v  lastv) (- result current-v ) (+ result current-v ))
                                 current-v
                                 (rest data)
                                 )
                               )
                             ))))
          ]

      ( parse-roma (map to-symbol col))
      )

    ))

(defcheck solution-103d7cd0
  (fn [rstr]
    (apply +
      (map (fn[[a b]] (if (< a b) (- a) a))
        (partition 2 1 [0]
          (map #(case %
                  \I	1
                  \V	5
                  \X	10
                  \L	50
                  \C	100
                  \D	500
                  \M	1000) rstr))))))

(defcheck solution-1044cb32
  (fn roman [s]
    (loop [s' s r 0 before []]
      (if (empty? s')
        r
        (let [x (first s')
              xs (rest s')
              digits {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
          (if
           (every? #(>=
                      (digits x)
                      (digits %))
             xs)
            (recur xs (+ r (digits x) (- (roman before))) [])
            (recur xs r (conj before x))))))))

(defcheck solution-10c40e8c
  (fn [z] (let [r {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
                parsed (loop [acc [] l (map str z)]
                         (if (empty? l) acc

                                        (cond (> (count l) 3) (let [e1 (first l) e2 (second l) e3 (nth l 2) e4 (nth l 3)]
                                                                (if (= e1 e2 e3 e4) (recur (conj acc (* 4 (r e1))) (drop 4 l))
                                                                                    (if (= e1 e2 e3) (recur (conj acc (* 3 (r e1))) (drop 3 l))
                                                                                                     (if (= e1 e2) (recur (conj acc (* 2 (r e1))) (drop 2 l))
                                                                                                                   (if (< (r e1) (r e2)) (recur (conj acc (- (r e2) (r e1))) (drop 2 l))
                                                                                                                                         (if (= e2 e3 e4) (recur (conj acc (+ (r e1) (* 3 (r e2)))) (drop 4 l))
                                                                                                                                                          (if (= e2 e3) (recur (conj acc (+ (r e1) (* 2 (r e2)))) (drop 3 l))
                                                                                                                                                                        (if (< (r e2) (r e3)) (recur (conj acc (+ (r e1) (- (r e3) (r e2)))) (drop 3 l))
                                                                                                                                                                                              (if (= (r e3) (r e4)) (recur (conj acc (+ (r e1) (r e2) (* 2 (r e3)))) (drop 4 l))
                                                                                                                                                                                                                    (if (< (r e3) (r e4)) (recur (conj acc (+ (r e1) (r e2) (- (r e4) (r e3)))) (drop 4 l))
                                                                                                                                                                                                                                          (recur (conj acc (+ (r e1) (r e2) (r e3) (r e4))) (drop 4 l))))))))))))

                                              (= (count l) 3) (let [e1 (first l) e2 (second l) e3 (nth l 2)]
                                                                (if (= e1 e2 e3) (recur (conj acc (* 3 (r e1))) (drop 3 l))
                                                                                 (if (= e1 e2) (recur (conj acc (* 2 (r e1))) (drop 2 l))
                                                                                               (if (< (r e1) (r e2)) (recur (conj acc (- (r e2) (r e1))) (drop 2 l))
                                                                                                                     (if (= e2 e3) (recur (conj acc (+ (r e1) (* 2 (r e2)))) (drop 3 l))
                                                                                                                                   (if (< (r e2) (r e3)) (recur (conj acc (+ (r e1) (- (r e3) (r e2)))) (drop 3 l))
                                                                                                                                                         (recur (conj acc (+ (r e1) (r e2) (r e3))) (drop 3 l))))))))


                                              (= (count l) 2) (let [e1 (first l) e2 (second l)]
                                                                (if (= e1 e2) (recur (conj acc (* 2 (r e1))) (drop 2 l))
                                                                              (if (< (r e1) (r e2)) (recur (conj acc (- (r e2) (r e1))) (drop 2 l))
                                                                                                    (recur (conj acc (+ (r e1) (r e2))) (drop 2 l)))))


                                              :else (recur (conj acc (r (first l))) (drop 1 l)))))]
            (reduce + parsed))))

(defcheck solution-122cabe2
  (fn [n]
    (let [f first
          d count
          v [["M" 1000]
             ["CM" 900]
             ["D"  500]
             ["C"  100]
             ["XC"  90]
             ["XL"  40]
             ["X"   10]
             ["IX"   9]
             ["V"    5]
             ["IV"   4]
             ["I"    1]]]
      (loop [r 0 c n]
        (if (empty? c)
          r
          (let [[k v] (f (filter #(if (= (seq (f %)) (take (d (f %)) c)) %) v))]
            (recur (+ r v) (drop (d k) c))))))))

(defcheck solution-1270c466
  #(->>
     %2
     (re-seq #"[MDLV]|C[MD]?|X[CL]?|I[XV]?")
     (map (apply conj % (for [x % y %] [(str (first x) (first y)) (- (second y) (second x))])))
     (reduce +)
     ) {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000})

(defcheck solution-12ae9d75
  (fn [rn]
    (let [vm {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}
          sm {\M \C, \D \C, \C \X, \L \X, \X \I, \V \I}
          rf (fn [[sum pre] rd]
               (if (= pre (sm rd))
                 [(+ sum (- (vm rd) (* 2 (vm pre)))) rd]
                 [(+ sum (vm rd)) rd]))]
      (first (reduce rf [0 nil] rn)))))

(defcheck solution-12b5d0b2
  (fn read-roman [s]
    (let [numeral-values {\I 1 \V 5
                          \X 10 \L 50
                          \C 100 \D 500
                          \M 1000}
          digit-values (map #(numeral-values %) (seq s))]
      (loop [[current-value & next-values] (reverse digit-values)
             max-value 0
             accum 0]
        (if current-value
          (if (>= current-value max-value)
            (recur next-values current-value (+ accum current-value))
            (recur next-values max-value (- accum current-value)))
          accum)))))

(defcheck solution-12f12190
  (fn rroman [rnum]
    (let [m { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [r rnum n 0]
        (let [l1 (first r) l2 (second r)]
          (cond
            (empty? r) n
            (or (nil? l2) (= l1 l2) (> (get m l1) (get m l2)))
            (recur (rest r) (+ n (get m l1)))
            :else
            (recur (nthnext r 2) (+ n (- (get m l2) (get m l1))))))))))

(defcheck solution-13377db3
  (let [vm {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
    (fn rn [s]
      (loop [v 0 s (vec s) maxv 0]
        (if (empty? s) v
                       (let [s0 (last s)
                             v0 (vm s0)
                             sr (drop-last s)]
                         (if (>= (vm s0) maxv)
                           (recur (+ v v0) sr v0)
                           (recur (- v v0) sr maxv))))))))

(defcheck solution-1343713e
  (fn roman-to-int [s]
    (if (empty? s)
      0
      (let [r '{"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
            spl (fn [n s] (map #(apply str %) (split-at n s)))
            [h1 t1] (spl 2 s)
            [h2 t2] (spl 1 s)]
        (if (r h1)
          (+ (r h1) (roman-to-int t1))
          (+ (r h2) (roman-to-int t2)))))))

(defcheck solution-1386af0c
  (fn read-roman
    [str]
    (let [dict {\I 1
                \V 5
                \X 10
                \L 50
                \C 100
                \D 500
                \M 1000}]
      (loop [num 0
             processing (map dict str)]
        #_(println "num " num)
        #_(println "processing " processing)
        (cond
          (empty? processing) num

          (= 1 (count processing)) (+ num (first processing))

          :else
          (let [n1 (first processing)
                n2 (second processing)]
            #_(println "delta " (- n2 n1))
            (case (- n2 n1)
              (4 9 40 90 400 900) (recur (+ num (- n2 n1)) (drop 2 processing))

              (recur (+ num n1) (next processing)))))))))

(defcheck solution-1402d5c
  (fn [roman]
    (apply + (loop [roman-rest roman
                    result []]
               (if (empty? roman-rest)
                 result
                 (let [subst-number (case (apply str (take 2 roman-rest))
                                      "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900 nil)
                       number (case (str (first roman-rest))
                                "I" 1 "V" 5 "X" 10 "L" 50"C" 100 "D" 500 "M" 1000)
                       used-chars (if (nil? subst-number) 1 2)]
                   (recur (drop used-chars roman-rest) (conj result (or subst-number number)))))))))

(defcheck solution-150f527
  (fn [s]
    (let [d {\I 1 \V 5
             \X 10 \L 50
             \C 100 \D 500
             \M 1000}
          xs (map #(if (>= (d (first %))
                         (d (second %)))
                     (d (first %))
                     (- (d (first %))))
               (partition 2 1 s))]
      (+ (reduce + xs) (d (last s))))))

(defcheck solution-15cd3223
  #(reduce
     (fn [m [a b]] ((if (and b (< a b)) - +) m a))
     0
     (partition-all
       2 1
       (map
         {\I 1, \V 5, \X 10, \L 50,
          \C 100, \D 500, \M 1000} %))))

(defcheck solution-16135af3
  (fn read-roman [r]
    (let [numerals {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 \0 0 \T 1001}
          s (vec (seq r))
          f (fn [idx x]
              (let [cur (numerals x)
                    nxt (numerals (get s (inc idx) \0))
                    prv (numerals (get s (dec idx) \T))]
                (cond
                  (> nxt cur) (- nxt cur)
                  (< prv cur) 0
                  :else cur)))]
      (apply + (map-indexed f s)))))

(defcheck solution-166f6a22
  #((reduce
      (fn [[n m] rnn]
        (let [nn ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} rnn)]
          (case (compare nn m)
            -1 [(- n nn) m]
            0 [(+ n nn) m]
            1 [(+ n nn) nn])))
      [0 1]
      (reverse %))
    0))

(defcheck solution-171236e7
  (fn [s]
    (let [values (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (seq s))]
      (->> values
        (partition 2 1)
        (map (fn [[a b]] (if (< a b) (* -1 a) a)))
        vec
        (#(conj % (last values)))
        (reduce +)))))

(defcheck solution-18c1637b
  (fn rn [ns]
    (letfn [(dm [s] (let [nmp {\I [1 (rc 1 #{\V \X})], \V [5 dm], \X [10 (rc 10 #{\L \C})],
                               \L [50 dm], \C [100 (rc 100 #{\D \M})], \D [500 dm] \M [1000 dm]}] (nmp s)))
            (rc [v s] (fn [n] (let [[nv nf] (dm n)] (if (s n) [(- nv (* 2 v)) dm] [nv nf]))))]
      (loop [c 0 [s1 & r] ns f dm]
        (if (nil? s1) c (let [[v nf] (f s1)] (recur (+ c v) r nf)))))))

(defcheck solution-1a2aa89e
  (fn [s]
    (let [v {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1}
          vs (map v (->> s seq (map str)))
          paired (partition 2 1 (concat vs [0]))
          sign (map (fn [[a b]] (if (< a b) (- 1) 1)) paired)]
      (apply + (map * vs sign))
      )
    ))

(defcheck solution-1a645c20
  (fn roman-to-number [x]
    (letfn [(letter-to-number [x]
              (condp = x
                \I 1
                \V 5
                \X 10
                \L 50
                \C 100
                \D 500
                \M 1000))]


      (loop [n 0, last 0, rest (into '()  x)]
        #_(println n last rest)
        (if (empty? rest)
          n
          (let [c (letter-to-number (first rest)), rest (next rest)]
            (if (< c last)
              (recur (- n c) last rest)
              (recur (+ n c) c rest))))))))

(defcheck solution-1aef3538
  (fn rmn [string]
    (let [numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [s 0 r (map numerals string)]
        (if (empty? r)
          s
          (let [pair (take 2 r)]
            (if (< (first pair) (last pair))
              (recur (+ s (- (last pair) (first pair))) (rest (rest r)))
              (recur (+ s (first pair)) (rest r)))))))))

(defcheck solution-1b0acf61
  (fn [s] (let [num-mapping {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "CD" 400
                             "D" 500 "CM" 900 "M" 1000}
                ] (loop [result 0 stuff s]
                    (cond
                      (nil? stuff) result
                      (= (count stuff) 1) (+ result (num-mapping (str (first stuff))))
                      (not (nil? (num-mapping (str (first stuff) (fnext stuff)))))
                      (recur (+ result (num-mapping (str (first stuff) (fnext stuff)))) (nnext stuff))
                      :else (recur (+ result (num-mapping (str (first stuff)))) (next stuff))
                      )
                    )
                  )
    ))

(defcheck solution-1b59796c
  (let [rval {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (fn rom->dec [rom]
      (let [subseqs (take-while (complement nil?) (iterate next (map rval rom)))]
        (reduce + (for [[h & t] subseqs]
                    (if (seq (filter #(> % h) t)) (* -1 h) h)))))))

(defcheck solution-1c1f3b8f
  (fn [s]
    (let [
          substractive {"IX" "9" "IV" "4" "XL" "l" "XC" "c" "CM" "m"}
          roman-digits { \I 1 \4 4 \V 5 \9 9 \X 10 \l 40 \L 50 \c 90 \C 100 \D 500 \m 900 \M 1000}]
      (->>
        (reduce #(clojure.string/replace %1 %2 (substractive %2)) s (keys substractive))
        (map roman-digits)
        (apply +)))))

(defcheck solution-1c6068f1
  #((letfn [(v [r]
              (cond
                (= r nil?) 0
                (= r \I) 1
                (= r \V) 5
                (= r \X) 10
                (= r \L) 50
                (= r \C) 100
                (= r \D) 500
                (= r \M) 1000 ))]

      (fn g ([s] (g (seq s) 0))
        ([s a]
         (let [p (first s) q (second s)]
           (if (empty? s)
             a
             (let [vp (v p) avp (+ a vp)]
               (if (= 1 (count s))
                 avp
                 (let [vq (v q)]
                   (if (< vp vq)
                     (g (drop 2 s) (+ a (- vq vp)))
                     (g (rest s) avp))))))))))%))

(defcheck solution-1c79cad1
  (fn [text]
    (let [digits {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [sum 0 prior 0 chars (seq text)]
        (if (seq chars)
          (let [current (digits (first chars))]
            (recur (+ sum current (if (> current prior) (* -2 prior) 0))
              current
              (rest chars)))
          sum)))))

(defcheck solution-1c90b5af
  (fn [s]
    (let [symbolz (hash-map \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000)]
      (loop [res 0
             last-seen-val 999999
             todo s]
        (if (empty? todo)
          res
          (let [f (first todo)
                val (symbolz f)
                acc (if (> val last-seen-val)
                      (- val (* 2 last-seen-val))
                      val)
                r (rest todo)]
            (recur (+ res acc) val r)))))))

(defcheck solution-1cc565e4
  (fn [n] (reduce
            (fn [acc [x y]] (+ acc (if (>= x y) x (- x))))
            0
            (partition 2 1
              (conj (mapv {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} n) 0)))))

(defcheck solution-1da3178e
  (fn [s]
    (let [rom-to-dec
          {"I"  1
           "IV" 4
           "V"  5
           "IX" 9
           "X"  10
           "XL" 40
           "L"  50
           "XC" 90
           "C"  100
           "CD" 400
           "D"  500
           "CM" 900
           "M"  1000}]
      (loop [[x y & chars] (seq s)
             acc 0]
        (if (= nil x y)
          acc
          (let [x-dec (rom-to-dec (str x))]
            (if (nil? y)
              (+ acc x-dec)
              (let [xy-dec (rom-to-dec (str x y))]
                (if (and xy-dec
                         (< x-dec xy-dec))
                  (recur chars
                    (+ acc xy-dec))
                  (recur (cons y chars)
                    (+ acc x-dec)))))))))))

(defcheck solution-1e3e106c
  (fn rrn [s]
    (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          sq (seq s)]
      (loop [last-digit (first sq)
             sum 0
             [digit & rsq :as tks] sq]
        (if (empty? tks)
          sum
          (if (< (get m last-digit) (get m digit))
            (recur digit
              (+
                (get m digit)
                (- sum (* 2 (get m last-digit))))
              rsq)
            (recur digit
              (+ (get m digit) sum)
              rsq)))))))

(defcheck solution-1eee8139
  (fn [n]
    (let [ rmap  { #"IV" "IIII"
                  #"IX" "VIIII"
                  #"XL" "XXXX"
                  #"XC" "LXXXX"
                  #"CD" "CCCC"
                  #"CM" "DCCCC"}

          vmap { \I 1
                \V 5
                \X 10
                \L 50
                \C 100
                \D 500
                \M 1000}
          rstring
                 (reduce #(clojure.string/replace % (key %2) (val %2)) n  rmap)

          ]
      (reduce #(+ % (vmap %2)) 0 rstring )

      )))

(defcheck solution-1fa0741c
  (fn rta [r]
    (let [pm [["IV" "A"] ["IX" "B"]
              ["XL" "E"] ["XC" "F"]
              ["CD" "G"] ["CM" "H"]]
          ra {"A" 4, "B", 9, "E" 40, "F" 90,
              "G" 400, "H" 900, "I" 1, "V" 5,
              "X" 10, "L" 50, "C" 100, "D" 500,
              "M" 1000}
          tracted  (loop [pres pm
                          is r]
                     (if (empty? pres) is
                                       (let [[ta tb] (first pres)]
                                         (recur (rest pres)
                                           (clojure.string/replace is ta tb)))))]
      (apply + (remove nil? (map #(ra %) (clojure.string/split tracted #"")))))))

(defcheck solution-1fdca609
  (fn [rn]
    (->>
      (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} rn)
      reverse
      ((fn [s] (map #(if (< %1 %2) (- 0 %1) %1) s (reductions max s))))
      (reduce +))))

(defcheck solution-204fd84f
  (fn f [v]
    (if (empty? v)
      0
      (let [values [["CM" 900] ["CD" 400] ["XC" 90] ["XL" 40] ["IX" 9] ["IV" 4] ["M" 1000] ["D" 500] ["C" 100] ["L" 50] ["X" 10] ["V" 5] ["I" 1]]
            pair (some #(if (.startsWith v (first %)) % nil) values)]
        (+ (second pair) (f (.substring v (count (first pair)))))))))

(defcheck solution-205bbc23
  (fn [r]
    (let [r->a {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (->>
        (map r->a r)
        (partition-all 2 1)
        (map (fn[[a b]] (if (and b (< a b)) (- a) a)))
        (apply +)))))

(defcheck solution-20c13f7e
  (fn [roman]
    (let [vals {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [r 0 c roman]
        (if (empty? c)
          r
          (let [fv (get vals (first c))
                sv (if (next c) (get vals (second c)) 0)
                rc (if (< fv sv) (drop 2 c) (rest c))]
            (recur (if (< fv sv) (+ r (- sv fv)) (+ r fv)) rc)))))
    ))

(defcheck solution-213968ed
  #(let [m (partition 2 1 (reverse (replace {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %)))]
     (reduce
       (fn [s [p n]]
         (if (< n p)
           (- s n)
           (+ s n)))
       (ffirst m) m)))

(defcheck solution-218b7a84
  (fn to-num [str]
    (letfn [(subprinc [lss]
              (loop [ls (reverse lss) acc [] max (first (reverse lss))]
                (if (empty? ls)
                  acc
                  (if (> max (first ls))
                    (recur (rest ls) (conj acc (- 0 (first ls))) max)
                    (recur (rest ls) (conj acc (first ls)) (first ls))))))]
      (reduce + (subprinc (map #(get {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) (seq str)))))))

(defcheck solution-23635e28
  (let [number-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (fn [roman-numeral]
      (let [numbers (map #(number-map %) roman-numeral)]
        (letfn [(eval-number-collection [col]
                  (cond
                    (empty? col) 0
                    (= 1 (count col)) (first col)
                    (> (second col) (first col)) (+ (eval-number-collection (rest (rest col)))
                                                   (- (second col) (first col)))
                    :else (+ (first col) (eval-number-collection (rest col)))))]
          (eval-number-collection numbers))))))

(defcheck solution-236b41f3
  (fn [s]
    (loop [acc 0
           [num nnum & rnums :as nums] (reverse (reduce (fn [a v] (cons (case v \I 1 \V 5 \X 10 \C 100 \L 50 \D 500 \M 1000) a)) [] s))]
      (if (seq nums)
        (if (< num (or nnum 0))
          (recur (+ acc (- nnum num)) rnums)
          (recur (+ acc num) (rest nums)))
        acc))))

(defcheck solution-237fe200
  (fn [s]
    (let [roman {\I 1,\V 5,\X 10,\L 50,\C 100,\D 500,\M 1000}
          number (map #(roman %) (seq s))]
      (loop [p number r []]
        (cond
          (= 1 (count p))  (apply + (conj r (first p)))
          (< (first p) (second p)) (recur (next p) (conj r (- (first p))))
          :else (recur (next p)  (conj r (first p))))))))

(defcheck solution-24c5d598
  (letfn [(value [c]
            (case c
              \I 1
              \V 5
              \X 10
              \L 50
              \C 100
              \D 500
              \M 1000))
          (parse [s]
            (if (not (seq s))
              0
              (if (not (seq (next s)))
                (value (first s))
                (let [f (value (first s))
                      n (first (next s))]
                  (if (< f (value n))
                    (+ (- (value n) f) (parse (nnext s)))
                    (+ f (parse (next s))))))))]
    parse))

(defcheck solution-250c75c0
  (fn read-roman [roman-nums]
    (let [assign-val
                 (fn [roman-num]
                   (case roman-num
                     "I" 1
                     "V" 5
                     "X" 10
                     "L" 50
                     "C" 100
                     "D" 500
                     "M" 1000))
          values (for [m (partition-all 2 1 roman-nums)]
                   (map #(assign-val (str %))m))
          cmp? (fn [[x y]] (cond
                             (nil? y) x
                             (< x y) (- x)
                             :else x))]
      (apply + (map cmp? values))
      ;; values
      )))

(defcheck solution-2540ac8a
  (fn roman [num]
    (let [digits (map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} num)
          pairs  (map vector digits (concat (drop 1 digits) [0]))]
      (reduce (fn [accum [x xnext]]
                (if (>= x xnext)
                  (+ accum x)
                  (- accum x)))
        0
        pairs))))

(defcheck solution-25a5bb98
  (fn [s]
    (reduce +
      (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        (seq (reduce #(apply clojure.string/replace %1 %2)
               s
               [["IV" "IIII"]
                ["IX" "IIIIIIIII"]
                ["XL" "XXXX"]
                ["XC" "XXXXXXXXX"]
                ["CD" "CCCC"]
                ["CM" "CCCCCCCCC"]]))))))

(defcheck solution-268249bc
  {"XIV" 14
   "DCCCXXVII" 827
   "MMMCMXCIX" 3999
   "XLVIII" 48})

(defcheck solution-26b280dc
  (fn [rs]
    (let [r->n {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          [k & others] (->> rs, reverse, (map r->n))
          answers (reduce
                    (fn [[acc m] i]
                      (if (< i m)
                        [(- acc i) m]
                        [(+ acc i) i]))
                    [k k]
                    others)]
      (first answers))))

(defcheck solution-26e5c27
  (fn [in]
    (let [l (map #({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) in)]
      (loop [n 0 p 1001 [f & r] l]
        (if (nil? f)
          n
          (recur
            (if (> f p) (+ n (- f p p)) (+ n f))
            f
            r))))))

(defcheck solution-27042abd
  {"XIV" 14 "DCCCXXVII" 827 "MMMCMXCIX" 3999 "XLVIII" 48})

(defcheck solution-2723184
  (fn [s]
    (let [num-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          subtr-fn #(+ % (- (num-map %3) (num-map %2))) ]
      (loop [[a b & t] s acc 0]
        (cond
          (nil? a) acc
          (nil? b) (+ acc (num-map a))
          (or
           (and (= a \I) (#{\V \X} b))
           (and (= a \X) (#{\L \C} b))
           (and (= a \C) (#{\D \M} b))) (recur t (subtr-fn acc a b))
          :default  (recur (cons b t) (+ acc (num-map a))))))))

(defcheck solution-27412d25
  (fn [s]
    (let [t {:M 1000 :D 500 :C 100 :L 50 :X 10 :V 5 :I 1}
          [a b _] (reduce (fn [[s c l] n] (if (> (n t) l)
                                            [s (- (n t) c) (n t)]
                                            [(+ s c) (n t) (n t)]))
                    [0 0 0]
                    (map (comp keyword str) s))]
      (+ a b))))

(defcheck solution-275f4745
  (fn [s]
    (let [roman {"M" 1000 "CM" 900 "D" 500 "CD" 400
                 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9
                 "V" 5 "IV" 4 "I" 1}]
      (reduce + (map roman
                  (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s))))))

(defcheck solution-2762f51b
  (fn parse-roman [x]
    (let [signs
                {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "CD" 400 "D" 500 "CM" 900 "M" 1000}
          check (fn [text size]
                  (->> text (take size) (apply str) (signs)))]
      (loop [left x sum 0]
        (if (empty? left)
          sum
          (let [two (check left 2) one (check left 1)]
            (if two
              (recur (drop 2 left) (+ sum two))
              (recur (drop 1 left) (+ sum one))
              )
            )
          )
        )
      )))

(defcheck solution-27d89ff
  #(let [trans-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [[x & input] (map trans-map (reverse %)), last 0, ans 0]
       (cond (nil? x) ans
             (< x last) (recur input last (- ans x))
             :else (recur input x (+ ans x))))))

(defcheck solution-28024789
  (fn roman [s]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          rs (reverse (map #(m %) (seq s)))
          convert (fn conv [[h & t]  mx]
                    (lazy-seq (cons (if (>= h mx) h (- h)) (if t (conv t (max h mx))))))]
      (reduce + (convert rs 0)))))

(defcheck solution-28537d06
  (fn unroman [s]
    (let [tr {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (cond
        (empty? s) 0
        (= (count s) 1) (tr (first s))
        true (let [d0 (tr (first s))
                   d1 (tr (second s))]
               (if (< d0 d1)
                 (- (unroman (rest s)) d0)
                 (+ (unroman (rest s)) d0)))))))

(defcheck solution-285b7acd
  (fn readroman [s]
    (let [dict {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1}]
      (if (empty? s)
        s
        (loop [q (map str (vec s)) result []]
          (if (empty? q)
            (apply + result)
            (if (empty? (rest q))
              (recur (rest q) (conj result (get dict (first q))))
              (if (>= (get dict (first q)) (get dict (second q)))
                (recur (rest q) (conj result (get dict (first q))))
                (recur (rest (rest q)) (conj result (- (get dict (second q)) (get dict (first q)))))))))))))

(defcheck solution-286b2566
  (fn roman [seq]
    (let [ord ["IV" "IX" "XL" "XC" "CD" "CM" "I" "V" "X" "L" "C" "D" "M"]
          t {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900
             "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (letfn [(match-str [s seq]
                (let [[f r] (split-at (count s) seq)]
                  (if (= (apply str f) s) s false)))
              (roman-split [seq]
                (lazy-seq
                  (when-let [token (some identity (map (fn [e] (match-str e seq)) ord))]
                    (cons token (roman-split (drop (count token) seq))))))]
        (let [s (roman-split seq)]
          (apply + (keep #(t %) s)))))))

(defcheck solution-28776a00
  (fn from-roman [s]
    (let [nums {\I 1 \V 5 \X 10 \L 50
                \C 100 \D 500 \M 1000}]
      (loop [s s prev 0 res 0]
        (if (seq s)
          (let [n (nums (first s))]
            (if (> n prev 0)
              (recur (rest s) (- n prev) res)
              (recur (rest s) n (+ prev res))))
          (+ prev res))))))

(defcheck solution-289f4126
  (fn [s]
    (->> s
      seq
      (map {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1})
      (partition 2 1 [0])
      (map (fn [[a b]] (if (< a b) (- a) a)))
      (apply +))))

(defcheck solution-28dc46e6
  (fn [r]
    (let [p clojure.string/replace]
      (reduce +
        (map
          {\I    1
           \V    5
           \X   10
           \L   50
           \C  100
           \D  500
           \M 1000}
          (-> r
            (p #"IV"  "IIII")
            (p #"IX" "VIIII")
            (p #"XL"  "XXXX")
            (p #"XC" "LXXXX")
            (p #"CD"  "CCCC")
            (p #"CM" "DCCCC")))))))

(defcheck solution-2900affd
  (fn [s]
    (apply +
      (map (fn [[a b]] (if (< a b) (- a) a))
        (partition 2 1
          (-> (map #({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) s)
            vec
            (conj 0)))))))

(defcheck solution-293287c1
  #(reduce + (map {"M" 1000 "CM" 900 "D" 500 "CD" 400
                   "C" 100 "XC" 90 "L" 50 "XL" 40
                   "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
               (re-seq #"M|CM|D|CD|C|XC|L|XL|X|IX|V|IV|I" %))))

(defcheck solution-29388451
  (fn [[c0 & cs0]]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          d {\M \C, \D \C, \C \X, \L \X, \X \I, \V \I}]
      (loop [[c & cs] cs0, p c0, ret 0, a (m c0)]
        (cond (nil? c) (+ ret a)
              (= p (d c)) (recur cs c (- ret a) (m c))
              (= p c) (recur cs c ret (+ a (m c)))
              :else (recur cs c (+ ret a) (m c)))))))

(defcheck solution-2947f17a
  (fn  [st]
    (let [values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (
       (reduce
         (fn [[sum max] value]
           (if (< value max)
             [(- sum value) max]
             [(+ sum value) value]))
         [0 0]
         (map #(values %) (reverse st)))
       0))))

(defcheck solution-295fcbd4
  (fn [roman]
    (let [vals (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} roman)]
      (reduce +
        (map
          (fn [[n & ns]]
            (if (some #(< n %) ns)
              (- 0 n)
              n))
          (take-while not-empty (iterate next vals)))))))

(defcheck solution-29943ca
  (fn [x]
    (let
     [R {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply +
        (map
          (partial reduce #(- (R %2) %1) 0)
          (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" x))))))

(defcheck solution-29f6b541
  (let [romanval {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
    (fn read-roman [x]
      (let [decomp (map str (seq x))]
        (cond (empty? decomp) 0
              (= 1 (count decomp)) (romanval (first decomp))
              (>= (romanval (first decomp))
                (romanval (second decomp))) (+ (romanval (first decomp))
                                              (read-roman (apply concat (rest decomp))))
              :else (+ (- (romanval (second decomp))
                          (romanval (first decomp)))
                      (read-roman (apply concat (nthrest decomp 2)))))))))

(defcheck solution-2a7733f7
  (fn romNum[strseq]
    (let[m  {\I 1,\V 5,\X 10,\L 50,\C 100,\D 500,\M 1000},
         ]
      (loop[col (seq strseq),result 0]
        (if (empty? col)
          result
          (let[fc (first col),
               other (rest col),
               sc (if (empty? other) nil (first other) ),
               fv (get m fc),
               sv (if (nil? sc) 0 (get m sc))]
            (if (nil? sc)
              (+ result fv)
              (cond (< fv sv)
                    (recur  (rest other) (+ result (- sv fv) ) )
                    (= fv sv)
                    (recur  (rest other) (+ result fv sv) )
                    (> fv sv)
                    (recur  other (+ result fv) )
                    :else nil
                    )
              )

            )
          )
        )
      )
    ))

(defcheck solution-2aee32bb
  (fn [s]
    (let [r ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          n [1000 900 500 400 100 90 50 40 10 9 5 4 1]]
      (loop [rm s, acc 0, i 0]
        (cond (empty? rm) acc
              (.startsWith rm (r i)) (recur (.substring rm (count (r i))) (+ acc (n i)) 0)
              :else (recur rm acc (inc i)))))))

(defcheck solution-2baa22d6
  (let [roman (fn f
                ([n]
                 (f n ""))
                ([n s]
                 (let [ss ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"]
                       dd [1 4 5 9 10 40 50 90 100 400 500 900 1000]
                       zz (zipmap dd ss)]
                   (if (zero? n)
                     s
                     (let [x (last (take-while #(<= % n) dd))]
                       (f (- n x) (str s (zz x))))))))
        rr    (range 0 4000)
        zz    (zipmap (map roman rr) rr)]
    (fn [s]
      (zz s))))

(defcheck solution-2c02323c
  (fn roman-numeral->int [rn]
    (let [values   [1 4 5 9 10 40 50 90 100 400 500 900 1000]
          numerals ["I" "IV" "V" "IX" "X" "XL" "L" "XC" "C" "CD" "D" "CM" "M"]
          vm (zipmap numerals values)
          n-numerals (map #(vm (str %)) (into [] rn))]
      (first (reduce
               (fn [[acc pv] v]
                 (if (< pv v)
                   [(+ (- acc pv) (- v pv)) v]
                   [(+ acc v) v]))
               [0 9000]
               n-numerals)))))

(defcheck solution-2c0c9500
  (fn [s]
    (loop [sum 0 [n & nx] (map {\I 1 \V 5 \X 10 \C 100 \L 50 \D 500 \M 1000} (reverse s))]
      (let [[e ne] (split-with #(= n %) nx) sum (apply (if (< n sum) - +) (list* sum n e))]
        (if (empty? ne) sum (recur sum ne))))))

(defcheck solution-2c12468f
  (fn [text]
    (let [romans {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          len (count text)]
      (loop [acc 0 i 0]
        (let [k (romans (get text i))]
          (if (= i (dec len))
            (+ acc k)
            (if (< k (romans (get text (inc i))))
              (recur (- acc k) (inc i))
              (recur (+ acc k) (inc i)))))))))

(defcheck solution-2c765944
  (fn read-roman-numerals [n]
    (letfn [(numeral-to-letter [x rnum letter]
              (clojure.string/replace x rnum letter))]
      (let [numeral-and-letters (numeral-to-letter
                                  (numeral-to-letter
                                    (numeral-to-letter
                                      (numeral-to-letter
                                        (numeral-to-letter
                                          (numeral-to-letter n "IV" "a")
                                          "IX" "b")
                                        "XL" "c")
                                      "XC" "d")
                                    "CD" "e")
                                  "CM" "f")
            converter {\I 1, \a 4, \V 5, \b 9, \X 10, \c 40 \L 50,
                       \d 90, \C 100, \e 400, \D 500, \f 900, \M 1000}]
        (reduce + (map #(get converter %) numeral-and-letters))))))

(defcheck solution-2e16da44
  (let [num-to-int {\M 1000
                    \D 500
                    \C 100
                    \L 50
                    \X 10
                    \V 5
                    \I 1}]
    (fn [numeral]
      (let [length (count numeral)]
        (loop [index (dec length)
               value 0]
          (if (< index 0) value
                          (let [here (num-to-int (.charAt numeral index))
                                prev (inc index)]
                            (recur (dec index)
                              (if (and (< prev length)
                                       (> (num-to-int (.charAt numeral prev)) here))
                                (- value here)
                                (+ value here))))))))))

(defcheck solution-2ec9d171
  (fn[rn]
    (let [rint {\I 1 \V 5 \X 10 \L 50
                \C 100 \D 500 \M 1000 }
          ints (map rint rn)
          maxs (->> ints
                 reverse
                 (reductions max)
                 reverse)]
      (reduce + (map #(if (< %1 %2) (- %1) %1) ints maxs)))))

(defcheck solution-2ed97de3
  (fn [roman]
    (let [letter->val {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          substractive? (fn [x y] (and (#{1 10 100 1000} x)
                                       (< x y)
                                       (>= 10 (/ y x))))]
      (->> (map letter->val roman)
        (partition-all 2 1)
        (map (fn [[x y]] (if (and y (substractive? x y)) (- x) x)))
        (reduce +)))))

(defcheck solution-2f1675eb
  (fn r2i [s]
    (if (empty? s) 0
                   (if (= (first s) \I)
                     (if (not-any? #(or (= \V %) (= \X %)) s) (+ 1 (r2i (rest s))) (- (r2i (rest s)) 1))
                     (if (= (first s) \V)
                       (+ 5 (r2i (rest s)))
                       (if (= (first s) \X)
                         (if (not-any? #(or (= \C %) (= \L %)) s) (+ 10 (r2i (rest s))) (- (r2i (rest s)) 10))
                         (if (= (first s) \L)
                           (+ 50 (r2i (rest s)))
                           (if (= (first s) \C)
                             (if (not-any? #(= \M %) s) (+ 100 (r2i (rest s))) (- (r2i (rest s)) 100))
                             (if (= (first s) \D)
                               (+ 500 (r2i (rest s)))
                               (if (= (first s) \M)
                                 (+ 1000 (r2i (rest s)))
                                 ))))))))
    ))

(defcheck solution-2f4e63ef
  (fn read-roman [roman]
    (let [ht {\I 1 \X 10 \C 100 \M 1000 \V 5 \L 50 \D 500}
          reversed (reverse roman)]
      (loop [c reversed num 0 prev 0]
        (if (empty? c)
          num
          (let [l (get ht (first c))
                newnum (if (< l prev) (- num l) (+ num l))]
            (recur (next c) newnum l)
            )
          )
        )
      )
    ))

(defcheck solution-2f7289b8
  (fn [s]
    (reduce +
      (map { "M" 1000
            "CM" 900
            "D"  500
            "CD" 400
            "C"  100
            "XC"  90
            "L"   50
            "XL"  40
            "X"   10
            "IX"   9
            "V"    5
            "IV"   4
            "I"    1 }
        (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s)))))

(defcheck solution-30035fae
  (fn [s]
    (let [table {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (reduce +
        (map
          (fn [[a b]] (if (< a b) (- a) a))
          (partition 2 1 [0] (map table s)))))))

(defcheck solution-305783c6
  (fn [s]
    (let [n {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply + (map (fn [[a b]]
                      (if (>= (n a) (n b)) (n a) (- (n a))))
                 (partition 2 1 (str s (last s))))))))

(defcheck solution-30595139
  (fn [s]
    (let [mapping {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          numbers (map mapping s)
          fs (reduce (fn [[res last] v]
                       (if (and (> last 0) (< last v))
                         [(+ res (- v last)) 0]
                         [(+ res last) v])) [0 0] numbers)]
      (apply + fs))))

(defcheck solution-30624d1d
  (fn [rn]
    (let [roman-nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          roman-num-func #(%1 (get roman-nums %2) (get roman-nums %3))
          larger-by #(quot (get roman-nums %1) (get roman-nums %2))
          larger-num-later? (fn [n ns] (some #(roman-num-func > % n) ns))
          power-of-ten? #(some #{%} #{\I \X \C})

          minuend-10x-larger-or-less? (fn [n min]
                                        (or (nil? min) (<= (larger-by min n) 10)))

          any-preceding-num-10x-larger-or-more?
          (fn [n preceding]
            (or (empty? preceding)
                (some #(>= (larger-by % n) 10) preceding)))

          any-following-smaller-than-sub?
          (fn [n following-min]
            (or (empty? following-min)
                (some #(roman-num-func < % n) following-min)))

          prepare-data
          (fn [s]
            (loop [i 0
                   acc []]
              (if (= i (count s))
                acc
                (recur (inc i)
                  (conj acc (hash-map :before (subs s 0 i)
                              :n (first (subs s i (inc i)))
                              :after (subs s (inc i))))))))

          subtract?
          (fn [{:keys [n before after]}]
            (let [minuend (first after)]
              (and (larger-num-later? n after)
                   (power-of-ten? n)
                   (minuend-10x-larger-or-less? n minuend)
                   (any-preceding-num-10x-larger-or-more? n before)
                   (any-following-smaller-than-sub? n (rest after)))))

          get-value
          (fn [{n :n :as data}]
            (let [value (get roman-nums n)]
              (if (subtract? data)
                (* -1 value)
                value)))

          data (prepare-data rn)
          values (map get-value data)]

      (reduce + values))))

(defcheck solution-31f30e1e
  (fn [roman]
    (let [m {:I 1, :V 5, :X 10, :L 50, :C 100, :D 500, :M 1000}]
      (letfn [(convert [v] (get m (keyword (str v))))]
        (let [firstVal (convert (last roman))]
          (first
            (reduce
              (fn [[total last] next]
                (let [n (convert next)]
                  (if (<= last n)
                    [(+ total n) n]
                    [(- total n) n])))
              [firstVal firstVal]
              (rest (reverse roman)))))))))

(defcheck solution-327c153f
  (fn sum-roman [x]
    (let [f (fn [[x & xs] curr sum]
              (cond (nil? x) (+ curr sum)
                    (> curr x) (if (= 0 (- curr x))
                                 (recur xs x (+ curr sum))
                                 (recur xs (- curr x) sum))
                    :else (recur xs x (+ curr sum))))
          convert-roman (fn [x]
                          (let [m {"I" 1
                                   "V" 5
                                   "X" 10
                                   "L" 50
                                   "C" 100
                                   "D" 500
                                   "M" 1000}]
                            (->> (seq x)
                              (map str)
                              (map m)
                              (reverse))))
          xs (convert-roman x)]
      (f (rest xs) (first xs) 0))))

(defcheck solution-32b25282
  (fn [n]
    (let [f first
          d count
          v [["M" 1000]
             ["CM" 900]
             ["D"  500]
             ["C"  100]
             ["XC"  90]
             ["XL"  40]
             ["X"   10]
             ["IX"   9]
             ["V"    5]
             ["IV"   4]
             ["I"    1]]]
      (loop [r 0 c n]
        (if (empty? c)
          r
          (let [[k v] (f (filter #(if (= (seq (f %)) (take (d (f %)) c)) %) v))]
            (recur (+ r v) (drop (d k) c))))))))

(defcheck solution-33f7244f
  (fn [s]
    (let [lookup {"M" 1000 "CM" 900 "D" 500 "CD" 400
                  "C" 100 "XC" 90 "L" 50 "XL" 40
                  "X" 10 "IX" 9 "V" 5 "IV" 4
                  "I" 1}]
      (reduce + (map (partial get lookup)
                  (re-seq #"CM|CD|XC|XL|IX|IV|M|D|C|L|X|V|I" s))))))

(defcheck solution-34c5c029
  (fn rom->int [chars]
    (let [romvals {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1}
          auxvals {"CM" 900 "CD" 400 "XC" 90 "XL" 40 "IX" 9 "IV" 4}
          allvals (merge romvals auxvals)]
      (letfn [(subtractive?
                [xs]
                (let [pair (str (first xs) (second xs))]
                  (if (contains? auxvals pair) true false)))
              (next-bite
                [ys]
                (if (subtractive? ys)
                  [(apply str (take 2 ys)) (apply str (drop 2 ys))]
                  [(str (first ys)) (apply str (rest ys))]))
              (tokenize
                [zs]
                (loop [tokens [] remaining zs]
                  (if (empty? remaining)
                    tokens
                    (let [[token others] (next-bite remaining)]
                      (recur (conj tokens token) others)))))]
        (let [tokens (tokenize chars)]
          (reduce + (map #(allvals %) tokens)))))))

(defcheck solution-3513d05a
  (let [rom {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (fn [s]
      (loop [[x & [y & _ :as more] :as digits] (seq s) n 0]
        (if (seq digits)
          (if (and y (> (rom y) (rom x)))
            (recur more (- n (rom x)))
            (recur more (+ n (rom x))))
          n)))))

(defcheck solution-3522bfae
  (fn [input] (letfn [(base [s] ({\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} (first s)))]
                (loop [groups (re-seq #"M+|D+|C+|L+|X+|V+|I+" input) result 0 previous nil]
                  (if-not (empty? groups)
                    (let [current (first groups)]
                      (recur (rest groups)
                        (- (+ result (* (base current) (count current)))
                           (if (and previous (< (base previous) (base current))) (* 2 (base previous) (count previous)) 0))
                        current))
                    result)))))

(defcheck solution-3529c395
  (fn [n]
    (let [nmap {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          nums (into [] (map nmap (seq n)))
          rnums (conj (into [] (rest nums)) 0)]
      (reduce + (map
                  (fn [[l r]] (if (< l r) (- 0  l) l))
                  (map vector nums rnums)))
      )
    ))

(defcheck solution-35b8bfc
  #(let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
     (loop [n 0, [x y & more :as c] (map m %)]
       (cond (not x) n
             (not y) (+ n x)
             (< x y) (recur (+ n (- y x)) more)
             :else   (recur (+ n x) (next c))))))

(defcheck solution-35bc5d65
  (fn read-roman [s]
    (letfn [(subtractive [[ h & t :as s]]
              (cond
                (empty? s) 0
                (some #(< h %) t) (- (subtractive t) h)
                :else (+ (subtractive t) h)))]
      (let [
            roman->dig
            {:I 1 :V 5 :X 10 :L 50 :C 100 :D 500 :M 1000}]
        (subtractive  (map roman->dig  (map (comp keyword str) s)))
        ))
    ))

(defcheck solution-35eedc52
  (fn [m a s]
    (if (empty? s) a
                   (let [ss (fn [s e] (.substring s 0 (min e (count s))))
                         k (if (m (ss s 2)) (ss s 2) (ss s 1))]
                     (recur m (+ a (m k)) (.substring s (count k)))))) {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90
                                                                          "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1} 0)

(defcheck solution-360b0581
  #(apply +
     (if (re-find #"I[VX]" %) -2 0)
     (if (re-find #"X[LC]" %) -20 0)
     (if (re-find #"C[DM]" %) -200 0)
     (map {\I 1 \V 5 \X 10 \L 50
           \C 100 \D 500 \M 1000} %)))

(defcheck solution-3632aeb
  (fn [q]
    (loop [r 0 v (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} q)]
      (if (empty? v)
        r
        (recur (+ r ((fn [[x & xs]] (if (some #(> % x) xs) (- x) x)) v))
          (rest v))))))

(defcheck solution-36570ebc
  (fn [s]
    (let [m (re-seq #"IV|IX|XL|XC|CD|CM|I|V|X|L|C|D|M" s)
          c #(case %
               "I" 1
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
               "M" 1000)]
      (reduce + (map c m)))))

(defcheck solution-369e87f3
  (let [lookup {[\I] 1,[\I \V] 4,[\V] 5,[\I \X] 9,[\X] 10,[\X \L] 40,[\L] 50,
                [\X \C] 90,[\C] 100,[\C \D] 400,[\D] 500,[\C \M] 900,[\M] 1000}]
    #(loop [converted 0 roman %]
       (if (empty? roman)
         converted
         (let [characters (take 2 roman)]
           (if (= 1 (count characters))
             (recur (+ converted (get lookup characters)) (rest roman))
             (if-let [value (get lookup characters nil)]
               (recur (+ converted value) (drop 2 roman))
               (recur (+ converted (get lookup [(first characters)])) (rest roman))
               )
             )
           )
         )
       )
    ))

(defcheck solution-36a16207
  (fn [s]
    (+ (reduce #(+ %1 ({\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1} %2)) 0 s)
      (if (re-find #"C[MD]" s) -200 0) (if (re-find #"X[CL]" s) -20 0) (if (re-find #"I[XV]" s) -2 0)
      )
    ))

(defcheck solution-36a8436c
  #(apply +
     (map-indexed (fn [i v]
                    (let [rn {\I 1
                              \V 5
                              \X 10
                              \L 50
                              \C 100
                              \D 500
                              \M 1000}
                          c (rn v)
                          n (rn (get % (inc i)))]
                      (if (and n (> n c))
                        (- c)
                        c))) %)))

(defcheck solution-36b82399
  (fn read-roman-numeral
    [s]
    (let [symbol-values {\I 1 \V 5
                         \X 10 \L 50
                         \C 100 \D 500
                         \M 1000}]
      (->>
        (partition-by identity s)
        (map (juxt first count))
        (partition-all 2 1)
        (map
          (fn [[[symbol cardinality] [next-symbol _]]]
            (let [value (symbol-values symbol)]
              (* cardinality value
                (if (and next-symbol
                         (< value (symbol-values next-symbol)))
                  -1 1)))))
        (apply +)))))

(defcheck solution-379a1cf9
  (fn [rn]
    (let [symbols {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (let [translated (map #(get symbols %) rn)]
        (apply + (cons (last translated)
                   (map (fn [[a b]] (if (>= a b) a (- a)))
                     (partition 2 1 translated))))))))

(defcheck solution-380e7776
  #(loop [[f & r] (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} %) s 0]
     (if r
       (if (< f (first r))
         (recur r (- s f))
         (recur r (+ s f)))
       (+ s f))))

(defcheck solution-384bed6e
  (fn [s]
    (loop [roman (reverse (seq s)), res-d 0, prev 0]
      (if (nil? roman)
        res-d
        (let [[rd & rst] roman,
              dd ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} rd)]
          (recur rst
            (if (< dd prev) (- res-d dd) (+ res-d dd))
            dd))
        ))))

(defcheck solution-38525511
  (fn [xs]
    (let [ n {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} ]
      (loop [f (first xs) s (rest xs) res 0 p nil]
        (if (nil? f)
          res
          (do (let [t (get n f)] (recur (first s) (rest s) (if (nil? p) (+ res t) (if (> t p)  (- (+ res (- t p)) p)  (+ res t))) t))))))))

(defcheck solution-38a06def
  (fn [s]
    (letfn [(notation2num [x]
              (case x
                "I" 1
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
                "M" 1000))]
      (let [specialNotations (re-seq #"IV|IX|XL|XC|CD|CM" s)
            allNotations (re-seq #"\w" s)
            specialChars (mapcat #(re-seq #"\w" %) specialNotations)
            specialNum (reduce #(+ %1 (notation2num %2)) 0 specialNotations)
            allNum (reduce #(+ %1 (notation2num %2)) 0 allNotations)
            specialCharNum (reduce #(+ %1 (notation2num %2)) 0 specialChars)]
        (- (+ specialNum allNum) specialCharNum)))))

(defcheck solution-38bf04bb
  #(reduce + (map {"M"  1000
                   "CM"  900
                   "D"   500
                   "CD"  400
                   "C"   100
                   "XC"   90
                   "L"    50
                   "XL"   40
                   "X"    10
                   "IX"    9
                   "V"     5
                   "IV"    4
                   "I"     1}
               (re-seq #"M|CM|D|CD|C|XC|L|XL|X|IX|V|IV|I" %))))

(defcheck solution-390222e5
  (fn [s]
    (let [romans
                 {\I 1
                  \V 5
                  \X 10
                  \L 50
                  \C 100
                  \D 500
                  \M 1000
                  }
          digits (map romans s)
          ]
      (apply +
        (map *
          digits
          (concat
            (map #(if (>= % %2) 1 -1) (drop-last digits) (rest digits))
            '(1)))))))

(defcheck solution-3918a806
  (fn [coll]
    (let [romvals {:M 1000 :CM 900 :D 500 :CD 400 :C 100 :XC 90 :L 50 :XL 40 :X 10 :IX 9 :V 5 :IV 4 :I 1}]
      (loop [result 0
             c coll]
        (if (empty? c)
          result
          (if (= (first (seq c)) \M)
            (recur (+ result (get romvals :M))
              (drop 1 c))
            (if (and (= (first (seq c)) \C) (= (second (seq c)) \M))
              (recur (+ result (get romvals :CM))
                (drop 2 c))
              (if (= (first (seq c)) \D)
                (recur (+ result (get romvals :D))
                  (drop 1 c))
                (if (and (= (first (seq c)) \C) (= (second (seq c)) \D))
                  (recur (+ result (get romvals :CD))
                    (drop 2 c))
                  (if (= (first (seq c)) \C)
                    (recur (+ result (get romvals :C))
                      (drop 1 c))
                    (if (and (= (first (seq c)) \X) (= (second (seq c)) \C))
                      (recur (+ result (get romvals :XC))
                        (drop 2 c))
                      (if (= (first (seq c)) \L)
                        (recur (+ result (get romvals :L))
                          (drop 1 c))
                        (if (and (= (first (seq c)) \X) (= (second (seq c)) \L))
                          (recur (+ result (get romvals :XL))
                            (drop 2 c))
                          (if (= (first (seq c)) \X)
                            (recur (+ result (get romvals :X))
                              (drop 1 c))
                            (if (and (= (first (seq c)) \I) (= (second (seq c)) \X))
                              (recur (+ result (get romvals :IX))
                                (drop 2 c))
                              (if (= (first (seq c)) \V)
                                (recur (+ result (get romvals :V))
                                  (drop 1 c))
                                (if (and (= (first (seq c)) \I) (= (second (seq c)) \V))
                                  (recur (+ result (get romvals :IV))
                                    (drop 2 c))
                                  (if (= (first (seq c)) \I)
                                    (recur (+ result (get romvals :I))
                                      (drop 1 c))))))))))))))))))))

(defcheck solution-391cf1b4
  #(reduce (fn [a [p q]]
             (+ a (if (and q (< p q)) (- p) p))) 0
     (partition-all 2 1
       (map {\I 1
             \V 5
             \X 10
             \L 50
             \C 100
             \D 500
             \M 1000}
         %))))

(defcheck solution-39289e14
  (fn [rom]
    (loop
     [sum 0
      next (first rom)
      re (rest rom)
      n {
         \I 1
         \V 5
         \X 10
         \L 50
         \C 100
         \D 500
         \M 1000
         }
      ]
      (if (= 0 (count re)) (+ sum (get n next))
                           (if (or
                                (= next (first re))
                                (pos? (- (get n next)(get n (first re))))
                                )
                             (recur
                               (+ sum (get n next))
                               (first re)
                               (rest re)
                               n)
                             (recur
                               (- sum (get n next))
                               (first re)
                               (rest re)
                               n))))))

(defcheck solution-3950d02c
  (fn deromanize
    [s]
    (let [digits {\M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
      (apply + (loop [in-digits (map digits (seq s))
                      procced-digits []]
                 (if (empty? in-digits)
                   procced-digits
                   (if (> (count in-digits) 1)
                     (let [[fd sd & restd] in-digits]
                       (if (< fd sd)
                         (recur restd (conj procced-digits (- sd fd)))
                         (recur (rest in-digits) (conj procced-digits fd))))
                     (recur (rest in-digits) (conj procced-digits (first in-digits))))))))))

(defcheck solution-395af4fe
  (fn [string]
    (let [roman-numerals-list {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90  "L" 50  "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
          ]
      (->> (re-seq #"CM|M|CD|D|XC|C|XL|L|X|IX|V|IV|I" string)
        (map #(roman-numerals-list %))
        (apply +))
      )
    ))

(defcheck solution-396bd1ad
  (fn [roman]
    (loop [[f & [s & r :as t]] (reverse (map #(condp = % \M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1) roman)) acc 0]
      (cond s (if (< s f)
                (recur r (+ acc (- f s)))
                (recur t (+ acc f)))
            f (+ acc f)
            :else acc))))

(defcheck solution-3aac79e6
  (fn [r]
    (->>
      (reverse r)
      (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})
      (cons 0)
      (partition 2 1)
      (reduce
        (fn [t [b a]]
          (+ t (if (< a b) (- a) a))) 0))))

(defcheck solution-3b722af5
  (fn [rn]
    (let [ds (->> (map {\I 1
                        \V 5
                        \X 10
                        \L 50
                        \C 100
                        \D 500
                        \M 1000}
                    rn)
               reverse)
          signs (cons 1
                  (map (fn [curr prev]
                         (if (< curr prev) -1 1))
                    (rest ds)
                    ds))]
      (reduce + (map * ds signs)))))

(defcheck solution-3ba9edd0
  (fn r [s]
    (let [m (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
      (cond (empty? s) 0
            (= (count s) 1) (m (first s))
            :else (+ ((if (>= (m (first s)) (m (second s))) + -) (m (first s))) (r (rest s)))))))

(defcheck solution-3d07a1dc
  (fn [x]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          sp {[\I \V] 4 [\I \X] 9 [\X \L] 40
              [\X \C] 90 [\C \D] 400 [\C \M] 900}
          e (fn [[a b] c] (if b
                            [a nil]
                            (if-let [s (sp c)]
                              [(+ a s) s]
                              [(+ a (m (first c))) nil])))
          i (partition-all 2 1 x)]
      (first (reduce e (e [0 nil] (first i)) (rest i))))))

(defcheck solution-3d115f87
  (fn __ [romstr]
    (if (seq romstr)
      (let [rns {"M" 1000,
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
            tok2 (apply str (take 2 romstr))
            tok1 (str (first romstr))]
        (if (contains? rns tok2)
          (+ (rns tok2) (__ (apply str (drop 2 romstr))))
          (+ (rns tok1) (__ (apply str (rest romstr))))))
      0)))

(defcheck solution-3d1c03e6
  (fn [r]
    (let [terms {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [acc 0, [x y & xs :as s] (seq r)]
        (cond (empty? s) acc
              (nil? y) (+ acc (terms x))
              (< (terms x) (terms y)) (recur (+ acc (- (terms x)) (terms y)) xs)
              :else (recur (+ acc (terms x)) (rest s)))))))

(defcheck solution-3d8be14
  (fn from-romans [romans]
    (let [roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          roman-first (roman-map (first romans))]
      (apply +
        (reduce (fn [acc k]
                  (if-let [curr-higher (and (< (last acc) (roman-map k)) (roman-map k))]
                    (conj (vec (butlast acc)) (- curr-higher (last acc)))
                    (conj acc (roman-map k))))
          [roman-first] (rest romans))))))

(defcheck solution-3da2af1
  (fn [s]
    (let [m (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
      (loop [[x & xs] s acc 0]
        (if x
          (if xs
            (let [a (m x) b (m (first xs))]
              (if (< a b)
                (recur (rest xs) (+ acc (- b a)))
                (recur xs (+ acc (m x)))
                )
              )
            (+ acc (m x))
            )
          acc
          )
        )
      )
    ))

(defcheck solution-3da62685
  (fn r [s]
    (cond
      (empty? s) 0
      (= (count s) 1) (condp = (first s)
                        \M (+ 1000 (r (rest s)))
                        \D (+ 500 (r (rest s)))
                        \C (+ 100 (r (rest s)))
                        \L (+ 50 (r (rest s)))
                        \X (+ 10 (r (rest s)))
                        \V (+ 5 (r (rest s)))
                        \I (+ 1 (r (rest s)))
                        )
      :else (condp = (first s)
              \M (+ 1000 (r (rest s)))
              \D (+ 500 (r (rest s)))
              \C (condp = (second s)
                   \M (+ 900 (r (nnext s)))
                   \D (+ 400 (r (nnext s)))
                   (+ 100 (r (rest s))))
              \L (+ 50 (r (rest s)))
              \X (condp = (second s)
                   \C (+ 90 (r (nnext s)))
                   \L (+ 40 (r (nnext s)))
                   (+ 10 (r (rest s))))
              \V (+ 5 (r (rest s)))
              \I (condp = (second s)
                   \X (+ 9 (r (nnext s)))
                   \V (+ 4 (r (nnext s)))
                   (+ 1 (r (rest s))))
              ))))

(defcheck solution-3db5331
  (fn rrn [s]
    (let [ rns (zipmap "MDCLXVI" [1000 500 100 50 10 5 1]) ]
      (loop [ s (reverse s) total 0 max 0 ]
        (if (nil? s)
          total
          (let [ n (rns (first s)) ]
            (if (< n max)
              (recur (next s) (- total n) max)
              (recur (next s) (+ total n) n))))))))

(defcheck solution-3e690d35
  (fn [w]
    (loop [[v1 v2 & more]
           (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} w)
           acc 0]
      (cond
        (not v1) acc
        (not v2) (+ acc v1)
        (< v1 v2) (recur more (+ acc (- v2 v1)))
        :else (recur (cons v2 more) (+ acc v1))))))

(defcheck solution-3eb0b5e4
  (fn roman
    [s]
    (let [trans {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (letfn [(step [s]
                (let [s1 (trans (first s)) s2 (trans (second s))]
                  (cond
                    (nil? s1) 0
                    (nil? s2) s1
                    (>= s1 s2) (+ s1 (step (next s)))
                    :else (+ (- s2 s1) (step (nnext s))))))]
        (step (seq s))))))

(defcheck solution-3f3f889d
  (fn [roman]
    (first
      (reduce
        (fn [[total prev-val] this-val]
          [((if (< this-val prev-val) - +) total this-val) this-val])
        [0 0]
        (map { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000 }
          (reverse roman))))))

(defcheck solution-3fda0511
  (fn roman-numerals
    [string]
    (let [subtracted {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
          values {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (loop [remaining-string string
             sum 0]
        (if-let [subtract (subtracted (apply str (take 2 remaining-string)))]
          (recur (drop 2 remaining-string) (+ sum subtract))
          (if (seq remaining-string)
            (recur (rest remaining-string) (+ sum (values (str (first remaining-string)))))
            sum))))))

(defcheck solution-406a251a
  (fn roman->int
    [s] {:pre [(string? s)]}
    (let [digits {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (->> s
        (reduce (fn [[last-digit & more :as acc] c]
                  (if (and last-digit (< last-digit (digits c)))
                    (cons (- (digits c) last-digit) more)
                    (cons (digits c) acc)))
          [])
        (apply +)))))

(defcheck solution-40997ec0
  (fn [r]
    (let [l [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
      (loop [x r res 0]
        (if (empty? x) res
                       (recur (nthnext x (count (reduce #(if (and (= (take 2 (first %2)) (take (count (first %2)) x)) (= %1 "")) (first %2) %1) "" l)))
                         (+ res (reduce #(if (and (= (take 2 (first %2)) (take (count (first %2)) x)) (= 0 %1)) (second %2) %1) 0 l))))))))

(defcheck solution-410bdfc3
  (fn from-roman [roman]
    (let [replacements [[#"CM" "CCCCCCCCC"] [#"CD" "CCCC"] [#"XC" "XXXXXXXXX"] [#"XL" "XXXX"] [#"IX" "IIIIIIIII"] [#"IV" "IIII"]]
          simplify (reduce (fn [acc [pattern replacement]] (clojure.string/replace acc pattern replacement)) roman replacements)
          numbers {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500 \M 1000}]
      (reduce + (map numbers simplify)))))

(defcheck solution-41718cc2
  (fn roman [rm]
    (let [s (first rm), ss (-> rm rest first)]
      (condp = (str s ss)
        "IV" (+ 4 (roman (-> rm rest rest)))
        "IX" (+ 9 (roman (-> rm rest rest)))
        "XL" (+ 40 (roman (-> rm rest rest)))
        "XC" (+ 90 (roman (-> rm rest rest)))
        "CD" (+ 400 (roman (-> rm rest rest)))
        "CM" (+ 900 (roman (-> rm rest rest)))
        "" 0
        (condp = s
          \I (+ 1 (roman (rest rm)))
          \V (+ 5 (roman (rest rm)))
          \X (+ 10 (roman (rest rm)))
          \L (+ 50 (roman (rest rm)))
          \C (+ 100 (roman (rest rm)))
          \D (+ 500 (roman (rest rm)))
          \M (+ 1000 (roman (rest rm)))
          )
        ))))

(defcheck solution-42634670
  #(let[letterValue {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000, nil 0}
        deductionLetter {\I #{\V \X}, \X #{\L \C}, \C #{\D \M}}]
     (when (seq %)
       (loop[letters (rest %) prevLetter (first %) sum 0]
         (if (seq letters)
           (let [currentLetter (first letters)]
             (if (contains? (deductionLetter prevLetter) currentLetter)
               (recur (drop 2 letters) (second letters) (+ sum (- (letterValue currentLetter) (letterValue prevLetter))))
               (recur (rest letters) currentLetter (+ sum (letterValue prevLetter)))))
           (+ sum (letterValue prevLetter)))))))

(defcheck solution-43171663
  #(apply +
     (map (fn [[v p]] (* (count (re-seq p %)) v))
       {1000 #"(?<!C)M"
        900 #"CM"
        500 #"(?<!C)D"
        400 #"CD"
        100 #"(?<!X)C(?!D|M)"
        90 #"XC"
        50 #"(?<!X)L"
        40 #"XL"
        10 #"(?<!I)X(?!L|C)"
        9 #"IX"
        5 #"(?<!I)V"
        4 #"IV"
        1 #"I(?!V|X)"})))

(defcheck solution-43454c21
  (fn [s]
    (loop [s_ s ret 0]
      (let [a (first s_) b (first (next s_))]
        (cond
          (empty? s_) ret
          (= a \M) (recur (next s_) (+ ret 1000))
          (and (= a \C) (= b \M)) (recur (next (next s_)) (+ ret 900))
          (= a \D) (recur (next s_) (+ ret 500))
          (and (= a \C) (= b \D)) (recur (next (next s_)) (+ ret 400))
          (= a \C) (recur (next s_) (+ ret 100))
          (and (= a \X) (= b \C)) (recur (next (next s_)) (+ ret 90))
          (= a \L) (recur (next s_) (+ ret 50))
          (and (= a \X) (= b \L)) (recur (next (next s_)) (+ ret 40))
          (= a \X) (recur (next s_) (+ ret 10))
          (and (= a \I) (= b \X)) (recur (next (next s_)) (+ ret 9))
          (= a \V) (recur (next s_) (+ ret 5))
          (and (= a \I) (= b \V)) (recur (next (next s_)) (+ ret 4))
          (= a \I) (recur (next s_) (+ ret 1)))))))

(defcheck solution-43c4ec5d
  (fn roman-num [numeral]
    (let [translate
          {\M 1000 ,
           \D  500 ,
           \C  100 ,
           \L   50 ,
           \X   10 ,
           \V    5 ,
           \I    1 }]

      (->> numeral
        (#(for [digit %] (translate digit)))
        ((fn appsub[[curr nxt & remaining]]
           (cond
             (nil?  curr)  remaining
             (nil?   nxt)  (conj remaining curr)
             (> nxt curr)  (conj (appsub remaining)
                             (- nxt curr))
             :else         (conj (appsub (conj remaining nxt))
                             curr))))
        (apply +)))))

(defcheck solution-440901b0
  (fn [s]
    (let [digits (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (letfn [(extract-digit [[n s] [k v]]
                (if (.startsWith s v)
                  (extract-digit [(+ n k) (.substring s (count v))] [k v])
                  [n s]))]
        (first (reduce extract-digit [0 s] digits))))))

(defcheck solution-442a455c
  #(apply + (map { "M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1
                  "CM" 900 "CD" 400 "XC" 90 "XL" 40 "IX" 9 "IV" 4}
              (re-seq #"C[MD]|X[CL]|I[XV]|." %))))

(defcheck solution-44339d14
  (fn rn-reader [n]
    (let [tran-map {"I"  1
                    "V"  5
                    "X"  10
                    "L"  50
                    "C"  100
                    "D"  500
                    "M"  1000
                    "IV" 4
                    "IX" 9
                    "XL" 40
                    "XC" 90
                    "CD" 400
                    "CM" 900 }
          len 	(count n)
          digits (vec (for [x n] (str x)))
          crd	   (fn [l r] (apply str (concat l r)))
          is-val? (fn [v] (contains? tran-map v))]
      (loop [idx 0, result 0]
        (if (>= idx len)
          result
          (if (= idx (dec len))
            (recur (inc idx) (+ result (tran-map (digits idx))))
            (let [l (digits idx)
                  r (digits (inc idx))
                  valid (is-val? (crd l r))]
              (if valid
                (recur (+ 2 idx) (+ result (tran-map (crd l r))))
                (recur (inc idx) (+ result (tran-map l)))))
            ))
        ))))

(defcheck solution-454017f5
  (fn [w]
    (let [T {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1 \o 0}]
      (->> (str w \o)
        (partition-by identity)
        (partition 2 1)
        (reduce (fn [a [[d1 & dd1 :as d1s] [d2 & dd2 :as d2s]]]
                  ((if (> (T d1) (T d2)) + -)
                   a (* (T d1) (count d1s))))
          0)))))

(defcheck solution-4597a204
  (fn rn [x]
    (let [dc {"CM" 900, "CD" 400, "XC" 90, "XL" 40, "IX" 9, "IV" 4}
          sc {"M" 1000, "D" 500, "C" 100, "L" 50, "X" 10, "V" 5, "I" 1}]
      (if (empty? x)
        0
        (if (and (>= (count x) 2) (contains? dc (.substring x 0 2)))
          (+ (dc (.substring x 0 2)) (rn (.substring x 2)))
          (+ (sc (.substring x 0 1)) (rn (.substring x 1))))))))

(defcheck solution-45c71da0
  (fn read-roman [string]
    (loop [[x y & xs :as xxs] (map (fn [c] (case c
                                             \M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1))
                                string)
           n 0]
      (cond
        (nil? x)     n
        (nil? y)     (+ n x)
        (>= x y)     (recur (rest xxs) (+ n x))
        :subtractive (recur xs         (+ n (- y x)))))))

(defcheck solution-4600a857
  (fn [roman-num-str]
    (loop [sum 0
           xs (map {\M 1000
                    \D 500
                    \C 100
                    \L 50
                    \X 10
                    \V 5
                    \I 1} roman-num-str)]
      (let [[x & xs] xs]
        (if-not xs
          (+ sum x)
          (recur ((if (some (partial < x) xs)
                    - +) sum x)
            xs))))))

(defcheck solution-4608a3aa
  (fn [roman]
    (letfn [
            (numeric-value [roman-digit]
              (get {
                    \I 1
                    \V 5
                    \X 10
                    \L 50
                    \C 100
                    \D 500
                    \M 1000 } roman-digit))
            (sign [left right]
              (if (< left right) (- left) left)) ]

      (let [ digits (map numeric-value roman)
            shifted (conj (vec (rest digits)) 0) ]
        (apply + (map sign digits shifted))))))

(defcheck solution-466945b0
  (fn roman-num [roman]
    (letfn [(to-num [rnum]
              (let [dict {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
                (get dict rnum)))]
      (let [num-lst (map to-num roman)
            zipped (map vector num-lst (rest num-lst))]
        (+ (last num-lst)
          (reduce #(+ %1 (if (> (second %2) (first %2))
                           (- (first %2))
                           (first %2)))
            0 zipped))))))

(defcheck solution-46836548
  #((reduce
      (fn [[a p] c]
        [((if(< (% c) p) - +) a (% c)) (% c)])
      [0 0] (reverse %2))
    0) {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defcheck solution-46a0d1fb
  #(reduce (fn [n c] (+ n
                       ({\I 1
                         \V 5
                         \X 10
                         \L 50
                         \C 100
                         \D 500
                         \M 1000} c))) 0
     (reduce (fn [x v] (apply (partial clojure.string/replace x) v))
       % [["CM" "DCCCC"]
          ["CD" "CCCC"]
          ["XC" "LXXXX"]
          ["XL" "XXXX"]
          ["IX" "VIIII"]
          ["IV" "IIII"]])))

(defcheck solution-47074d49
  (fn rn [s]
    (let [syms {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          sub-syms {(seq "IV") 4 (seq "IX") 9 (seq "XL") 40 (seq "XC") 90 (seq "CD") 400 (seq "CM") 900}]
      (if (empty? s)
        0
        (let [next-sub-sym-token-value (get sub-syms (take 2 s))]
          (if next-sub-sym-token-value
            (+ next-sub-sym-token-value (rn (rest (rest s))))
            (+ (get syms  (first s)) (rn (rest s)))))))))

(defcheck solution-476f2d0c
  (fn [rn]
    (let [m {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10
             "XL" 40 "L" 50 "XC" 90 "C" 100
             "CD" 400 "D" 500 "CM" 900 "M" 1000}]
      (loop [r rn
             n 0
             p nil]
        (if r
          (let [f (str "" (first r))]
            (if (and p (m (str p f)))
              (recur (next r) (- (+ n (m (str p f))) (m p)) nil)
              (recur (next r) (+ n (m f)) f)))
          n)))))

(defcheck solution-47ff27cc
  (fn roman [s]
    (reduce
      (fn [t [a b]] ((if b (if (> b a) - +) +) t a))
      0
      (partition-all 2 1 (map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} s))
      )
    ))

(defcheck solution-484eccbc
  (fn read-roman [s]
    (let* [r {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
           p (partition-by + (replace r (concat s [0])))
           m (partition 2 1 (map #(apply + %) p))]
      (apply + (map (fn [[a b]] (if (< a b) (- a) a)) m)))))

(defcheck solution-484edf0a
  #(apply + (for [[x y]
                  (partition 2 1 [0]
                    (map {\I 1 \V 5 \X 10 \L 50
                          \C 100 \D 500 \M 1000} %))]
              ((if (< x y) - +) x))))

(defcheck solution-4897fc6e
  (fn [s]
    (let [sn {[\C \M] 900  [\C \D] 400 [\X \C] 90
              [\X \L] 40 [\I \X] 9 [\I \V] 4}]
      (letfn [(asn [[f & r]]
                (if f
                  (+ (if-let [n (sn [f (first r)])]
                       n 0)
                    (asn r))
                  0))
              (dsn [[f & r]]
                (when f
                  (if (sn [f (first r)])
                    (dsn (rest r))
                    (cons f (dsn r)))))]
        (reduce + (asn s) (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (dsn s)))))))

(defcheck solution-49219663
  (fn [ys]
    (letfn [(romtab [] {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})
            (rom [res xs]
              (let [len (count xs)]
                (cond (> len 1)
                      (let [x1 (first xs) x2 (second xs)]
                        (if (< ((romtab) x1) ((romtab) x2))
                          (recur (- (+ res ((romtab) x2))
                                    ((romtab) x1) ) (rest (rest xs)) )
                          (recur (+ res ((romtab) x1)) (rest xs) )
                          )
                        )
                      (= len 1)
                      (+ res ((romtab) (first xs)) )
                      :else res
                      )
                )
              )]
      (rom 0 ys)
      )
    ))

(defcheck solution-49707a0f
  #((fn rval [[v & r]]
      (let [rtod {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
        (if (empty? r)
          (rtod v)
          (+
            (*
              (if (>= (rtod v) (rtod (first r))) 1 -1)
              (rtod v))
            (rval r))))) (seq %)))

(defcheck solution-4a294eb1
  (fn roman-nums
    [n]
    (let [numerals {\I {:default 1, \V 4, \X 9},
                    \V 5,
                    \X {:default 10, \L 40, \C 90},
                    \L 50, \C {:default 100, \D 400, \M 900},
                    \D 500,
                    \M 1000}]
      (->> n
        (map identity)
        vec
        (#(conj % nil))
        (reduce
          (fn [[total prev-token] token]
            (let [value (numerals prev-token)]
              (if (nil? value)
                [total token]
                (if (map? value)
                  (if (contains? value token)
                    [(+ total (value token)) nil]
                    [(+ total (value :default)) token])
                  [(+ total value) token]))))
          [0 nil])
        first))))

(defcheck solution-4a77e0f1
  (fn __ [roman]
    (let [n {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      ((fn calc [seen togo sum]
         (if-let [s (seq togo)]
           (if (< (n (first s)) (n seen))
             (calc seen (rest s) (- sum (n (first s))))
             (calc (first s) (rest s) (+ sum (n (first s)))))
           sum))
       \I (reverse roman) 0))))

(defcheck solution-4ac5d92f
  (fn read-roman [s]
    (let [numerals {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          nums (partition 2 1 (concat (map numerals s) [0]))]
      (reduce (fn [sum [a b]] ((if (< a b) - +) sum a)) 0 nums))))

(defcheck solution-4aedbd2f
  (fn [x]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          x (seq x)]
      (loop [acc 0
             ch (first x)
             x (next x)]
        (if (nil? x)
          (+ acc (m ch))
          (recur (let [a (m ch)
                       b (m (first x))]
                   ((if (> b a) - +) acc a))
            (first x)
            (next x)))))))

(defcheck solution-4b042a14
  (fn [s]
    (let [m  {\I 1
              \V 5
              \X 10
              \L 50
              \C 100
              \D 500
              \M 1000}]
      ((reduce (fn [r c]
                 (if (< (m (r 1)) (m c))
                   [(+ (- (r 0) (m (r 1))) (- (m c) (m (r 1)))) c]
                   [(+ (r 0) (m c)) c]))
         [0 \M]
         s) 0))))

(defcheck solution-4b0947c1
  (fn [roman-numeral]
    (let [value-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (reduce + (map
                  (fn
                    [str]
                    (if (and (> (count str) 1) (< (value-map (first str)) (value-map (second str))))
                      (- (value-map (second str)) (value-map (first str)))
                      (reduce + (map #(value-map %) str))))
                  [(first (re-seq #"M*" roman-numeral))
                   (first (re-seq #"[CD][CDM]*" roman-numeral))
                   (first (re-seq #"[XL][XLC]*" roman-numeral))
                   (first (re-seq #"[IV][IVX]*" roman-numeral))])))))

(defcheck solution-4bb88162
  #(->> %
     (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]))
     (partition-all 2 1)
     (map (fn [[a b]] (if (and b (< a b)) (- a) a)))
     (reduce +)))

(defcheck solution-4be2fecf
  (fn roman-numerals
    [iroman]
    (let [roman {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [s (map #(get roman %) (seq iroman))
             r 0]
        (if (= 1 (count s))
          (+ r (first s))
          (recur (rest s) (if (>= (first s)
                                (second s))
                            (+ r (first s))
                            (- r (first s)))))))))

(defcheck solution-4c167933
  (fn [s]
    (let [ns (->> s (re-seq #".")
               (map {"I" 1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000})
               (partition-by identity)
               (map #(apply + %)))
          p (map list (butlast ns) (rest ns))]
      (apply + (last ns)
        (map (fn [[a b]] (if (> a b) a (- a))) p))
      )))

(defcheck solution-4c2d272f
  (fn [s]
    (let [val-map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      ((fn [s last-val total]
         (cond (empty? s) total
               (nil? last-val) (recur (rest s)
                                 (val-map (first s))
                                 (val-map (first s)))
               :else (let [cur-val (val-map (first s))
                           sub? (< cur-val last-val)]
                       (recur (rest s)
                         cur-val
                         ((if sub? - +) total cur-val)))))
       (reverse s) nil 0))))

(defcheck solution-4c9b0772
  (fn [romano]
    (->>
      ((fn extrai-arabicos [rom mapas-r-a]
         (when (not (empty? rom))
           (let [valor-encontrado (some #(when (re-seq (re-pattern (str (first %) "$")) rom)
                                           %)
                                    (first mapas-r-a))]
             (cons (second valor-encontrado)
               (extrai-arabicos (clojure.string/replace rom
                                  (re-pattern (str (first valor-encontrado) "$"))
                                  "")
                 (next mapas-r-a))))))
       romano
       (concat (map
                 (fn romano-arabico
                   ([um cinco dez escala]
                    [[(str um dez) (* escala 9)]
                     [(str cinco um um um) (* escala 8)]
                     [(str cinco um um) (* escala 7)]
                     [(str cinco um) (* escala 6)]
                     [(str um cinco) (* escala 4)]
                     [(str cinco) (* escala 5)]
                     [(str um um um) (* escala 3)]
                     [(str um um) (* escala 2)]
                     [(str um) (* escala 1)]]))
                 ["I" "X" "C"] ["V" "L" "D"] ["X" "C" "M"] [1 10 100])
         [[["MMM" 3000] ["MM" 2000] ["M" 1000]]]))
      (filter #(not (nil? %)))
      (apply +))))

(defcheck solution-4cbbd1c3
  (fn [astr]
    (let [
          nums
          (map
            (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
            astr
            )]
      (apply + (map
                 (fn [a b] (if (>= a b) a (- a)))
                 nums (concat (next nums) '(0)))))))

(defcheck solution-4d0f9f7f
  (fn peu [x] (if (= 0 (count x)) 0 (let [f #(case % \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000) g #(+ (peu ((fn [w] (if (empty? w) "" (apply str w))) (rest (rest x)))) %)] (if (= 1 (count x)) (f (first x)) (case (apply str (take 2 x)) "IV" (g 4) "IX" (g 9) "XL" (g 40) "XC" (g 90) "CD" (g 400) "CM" (g 900) (+ (peu ((fn [w] (if (empty? w) "" (apply str w))) (rest x))) (f (first x)))))))))

(defcheck solution-4d67181f
  (fn [rs] (letfn [(conv [msg n] (if (zero? n) msg
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
             ((into {} (for [i (range 1 4000)] [(conv "" i) i] )) rs))))

(defcheck solution-4dabf6b8
  (fn roman [init]
    (loop [s init v 0]
      (cond (empty? s) v
            (= (first s) \M) (recur (rest s) (+ v 1000))
            (= (first s) \D) (recur (rest s) (+ v 500))
            (= (take 2 s) [\C \M]) (recur (drop 2 s) (+ v 900))
            (= (take 2 s) [\C \D]) (recur (drop 2 s) (+ v 400))
            (= (first s) \C) (recur (rest s) (+ v 100))
            (= (first s) \L) (recur (rest s) (+ v 50))
            (= (take 2 s) [\X \C]) (recur (drop 2 s) (+ v 90))
            (= (take 2 s) [\X \L]) (recur (drop 2 s) (+ v 40))
            (= (first s) \X) (recur (rest s) (+ v 10))
            (= (first s) \V) (recur (rest s) (+ v 5))
            (= (take 2 s) [\I \X]) (recur (drop 2 s) (+ v 9))
            (= (take 2 s) [\I \V]) (recur (drop 2 s) (+ v 4))
            (= (first s) \I) (recur (rest s) (+ v 1))
            true (println "ARGH" s v)))))

(defcheck solution-4dbbc093
  (fn read-roman [st]
    (let [conv {\M 1000
                \D 500
                \C 100
                \L 50
                \X 10
                \V 5
                \I 1}]
      (loop [s (reverse st)
             v 0]
        (if (seq s)
          (let [cv (conv (first s))      ;current value
                nv (conv (second s))]    ;next value
            (if (and nv (< nv cv))
              (recur (nthrest s 2) (+ v (- cv nv)))
              (recur (rest s) (+ v cv))))
          v)))))

(defcheck solution-4dc82b99
  (fn [roman]
    (let [subs {"IV" "IIII", "IX" "VIIII", "XL" "XXXX",
                "XC" "LXXXX", "CD" "CCCC", "CM" "DCCCC"}
          values {\I 1, \V 5, \X 10, \L 50, \C 100,
                  \D 500, \M 1000}]
      (apply + (map (fn [[c n]] (* n (values c)))
                 (frequencies (reduce
                                (fn [s [b a]](.replace s b a))
                                roman subs)))))))

(defcheck solution-4df7f847
  (fn [s]
    (let [numerals (into {} (map vector "MDCLXVI" [1000 500 100 50 10 5 1]))
          posvalues (map numerals s)
          value (first
                  (reduce
                    (fn [[v l] x] (if
                                   (>= x l) [(+ v x) x]
                                            [(- v x) l]))
                    [0 0]
                    (reverse posvalues)))]
      value)))

(defcheck solution-4e329f9
  #(let [r-vals {\M 1000, \D 500, \C 100, \L 50 \X 10, \V 5, \I 1}
         subtractive (fn [[x y]] (if (and y (> y x)) (- x) x))]
     (apply + (->> % (map r-vals) (partition-all 2 1) (map subtractive)))))

(defcheck solution-4eb6af0b
  (fn [s]
    (let [v {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (let [n (map v s)]
        (apply +
          (map (fn [[x y]] (if (< x y) (- x) x))
            (map #(vector % %2) n (concat (rest n) [0]))))))))

(defcheck solution-4ec391e5
  (fn roman-to-num [s]
    (let [; we invent some digits N-S to represent valid substractive-principle numbers
          roman-digits (zipmap "IVXLCDMNOPQRS" [1 5 10 50 100 500 1000 4 9 40 90 400 900])
          subsnum {"IV" "N" "IX" "O" "XL" "P" "XC" "Q" "CD" "R" "CM" "S"}]
      (apply + (replace roman-digits
                 (reduce (fn [s [a b]] (clojure.string/replace s a b)) s subsnum))))))

(defcheck solution-505c7bd6
  (fn [r]
    (let [lookup {"I" 1
                  "V" 5
                  "X" 10
                  "L" 50
                  "C" 100
                  "D" 500
                  "M" 1000}
          xs (reverse (map #(lookup (str %)) r))]
      ((fn rmnz [n xs l]
         (let [x (first xs)]
           (if (not (seq xs)) n
                              (if (<= l x)
                                (rmnz (+ n x) (rest xs) x)
                                (rmnz (- n x) (rest xs) x))))) 0 xs 0))))

(defcheck solution-5090603
  (fn [astr]
    (let [t {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (first (reduce
               (fn [acc newv]  (let [
                                     [res lastv] acc
                                     nextv (get t newv)
                                     op (if (>= nextv lastv) + -)
                                     ]
                                 [(op res nextv) nextv]))
               [0 0]
               (reverse (seq astr))))
      )
    ))

(defcheck solution-50ff5175
  (fn parse-rom [s]
    (letfn [(first-match [s]
              (let [digs (for [x [1 10 100 1000]
                               y (range 1 10)]
                           (* x y))
                    roms (concat ["I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
                           ["X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                           ["C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                           (reductions str (repeat 9 "M")))
                    rom-number (map vector (reverse roms) (reverse digs))
                    match (first (filter #(.startsWith s (first %)) rom-number))] ;; => (["MMMMMMMMM" 9000] [...])
                match
                ))]
      (let [[rom number] (first-match s)]
        (if (nil? rom)
          0
          (+ number (parse-rom (subs s (count rom)))))))))

(defcheck solution-514517ed
  (fn x [s]
    (let [t { "I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50
             "XC" 90 "C" 100 "CD" 400 "D" 500 "CM" 900 "M" 1000 }
          a (first (filter #(contains? t %) (list (subs s 0 (min (count s) 2)) (subs s 0 1))))
          n (subs s (count a))]
      (+ (t a) (if (empty? n) 0 (x n))))))

(defcheck solution-516891d8
  (fn [s]
    (let [m (array-map "IV" "IIII" "IX" "VIIII" "V" "IIIII"
              "XL" "XXXX" "XC" "LXXXX" "L" "XXXXX"
              "CD" "CCCC" "CM" "DCCCC" "D" "CCCCC")
          v '{\I 1 \X 10 \C 100 \M 1000}]
      (apply + (map v (reduce #(clojure.string/replace % (%2 0) (%2 1)) s m))))))

(defcheck solution-5175cf5
  (fn read-roman [s]
    (if (empty? s)
      0
      (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
        (if (< (m (first s))
              (reduce max 0 (map m (rest s))))
          (- (read-roman (rest s)) (m (first s)))
          (+ (read-roman (rest s)) (m (first s))))))))

(defcheck solution-527b63dd
  (fn roman-to-arab[sn]
    (letfn [
            (lat-num[num]
              (case num
                \I 1
                \V 5
                \X 10
                \L 50
                \C 100
                \D 500
                \M 1000
                )
              )

            (lat-num-sign[c lastnum]
              (let [num (lat-num c) sgn (if (< lastnum 0) -1 1)]
                (if (= num (* sgn lastnum))
                  (* sgn num)
                  (if (< num (* sgn lastnum)) (* -1 num) num)
                  )
                )
              )

            (decode-lat-num [str lastnum total]
              (if (empty? str)
                total
                (
                  let [num (lat-num-sign (first str) lastnum)]
                  (decode-lat-num (rest str) num (+ total num))
                  )
                )
              )
            ]

      (decode-lat-num (reverse sn) 0 0)
      )
    ))

(defcheck solution-529d0397
  (fn read-roman [s]
    (let [roman-vals  {\I 1
                       \V 5
                       \X 10
                       \L 50
                       \C 100
                       \D 500
                       \M 1000}]
      (->> s
        reverse
        (map roman-vals)
        (reduce (fn [xs x]
                  (conj xs (if (> (or (first xs) 0) x)
                             (- x)
                             x)))
          ())
        (reduce +)))))

(defcheck solution-52ef10e5
  (fn __ [roman]
    (let [cache {\I 1, \V 5,\X 10, \L 50,\C 100,\D 500,\M 1000}
          transform (fn [[sum skip] [f s]]
                      (if skip [sum false]
                               (if (and s(> s f))
                                 [(+ sum (- s f)) true]
                                 [(+ sum f) false])))]
      (->> roman (map cache) (partition-all 2 1) (reduce transform [0 false]) first))))

(defcheck solution-5345c2be
  (fn roma [se]
    (let [rd {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          sus {"IV" 2 "IX" 2 "XL" 20 "XC" 20 "CD" 200 "CM" 200}
          amount (reduce + (map rd (seq se)))
          s (reduce + (filter #(not (nil? %)) (map sus (for [i (range 0 (dec (count se)))]
                                                         (.substring se i (+ i 2))))))]
      (- amount s))))

(defcheck solution-5395259c
  (fn calc [rom]
    (let [rom->dec (fn rom->dec [nums]
                     (if (empty? nums) 0
                                       (let [cur  (first nums)
                                             rst  (rest nums)
                                             less (take-while #(> cur %) rst)
                                             sum  (reduce - cur less)]
                                         (+ sum (rom->dec (drop (inc (count less)) nums))))))
          r->d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          nums (reverse (map r->d rom))]
      (rom->dec nums))))

(defcheck solution-5414349
  (fn romanX [s] ((fn romanRec [x]
                    (if (empty? x)
                      0
                      (let [r1 ((fn roman1 [r]
                                  (case r
                                    \M 1000
                                    \D 500
                                    \C 100
                                    \L 50
                                    \X 10
                                    \V 5
                                    \I 1
                                    )
                                  )
                                (first x))]
                        (if (nil? (second x))
                          r1
                          (let [r2 ((fn roman1 [r]
                                      (case r
                                        \M 1000
                                        \D 500
                                        \C 100
                                        \L 50
                                        \X 10
                                        \V 5
                                        \I 1
                                        )
                                      )
                                    (second x))]
                            (if (>= r1 r2)
                              (+ (romanRec (rest x)) r1)
                              (- (romanRec (rest x)) r1)
                              )
                            )
                          )
                        )
                      )
                    ) (seq s))))

(defcheck solution-54293e3b
  (fn translate-roman-numeral
    [roman-numeral]
    (let [base {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          roman-number (map base roman-numeral)
          maxnum (cond
                   (empty? roman-numeral) 0
                   (= 1 (count roman-numeral)) (first roman-number)
                   :else (apply max roman-number))
          left-side (take-while #(< (base %) maxnum) roman-numeral)
          right-side (rest (drop-while #(< (base %) maxnum) roman-numeral))]
      (cond
        (empty? roman-numeral) 0
        :else (+ (- maxnum (translate-roman-numeral left-side)) (translate-roman-numeral right-side)))
      )))

(defcheck solution-54351006
  #(->> (map {\C 100 \D 500 \I 1 \L 50 \M 1000 \V 5 \X 10} %)
     (partition 2 1 [0])
     (map (fn [[a b]] (if (< a b) (- a) a)))
     (apply +)))

(defcheck solution-547ac0f3
  (fn roman-numerals [s]
    (let [dict {"I" 1   "IV" 4   "V" 5   "IX" 9
                "X" 10  "XL" 40  "L" 50  "XC" 90
                "C" 100 "CD" 400 "D" 500 "CM" 900
                "M" 1000}]
      (->> s
        (re-seq #"IV|IX|XL|XC|CD|CM|[IVXLCDM]")
        (map dict)
        (apply +)))))

(defcheck solution-54826a4b
  #(reduce
     (fn [s [c n]]
       (cond
         (nil? n) (+ s c)
         (< c n) (- s c)
         :else (+ s c)))
     0
     (partition-all
       2 1 (map {\I 1
                 \V 5
                 \X 10
                 \L 50
                 \C 100
                 \D 500
                 \M 1000} %))))

(defcheck solution-54f3701f
  #(let [l->n {"CD" 400, "C" 100, "D" 500, "I" 1,
               "CM" 900, "XC" 90, "L" 50, "M" 1000,
               "IV" 4, "IX" 9, "XL" 40, "V" 5, "X" 10}]
     (loop [s (seq %) result 0]
       (if (empty? s) result
                      (if-let [n (l->n (apply str (nthnext s (- (count s) 2))))]
                        (recur (butlast (butlast s)) (+ result n))
                        (recur (butlast s) (+ result (l->n (str (last s))))))))))

(defcheck solution-554b4385
  #(reduce
     (fn [n d]
       (let [[x x*] (map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} d)]
         ((if (and x x* (< x x*)) - +) n (or x 0))))
     0 (map list % (concat (rest %) [nil]))))

(defcheck solution-562ed1b8
  (fn rom [s]
    (let [A {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          R [["IV" "IIII"]
             ["IX" "VIIII"]
             ["XL" "XXXX"]
             ["XC" "LXXXX"]
             ["CD" "CCCC"]
             ["CM" "DCCCC"]]]
      (apply + (map A (reduce #(clojure.string/replace % (first %2)(second %2)) s R ))))))

(defcheck solution-563b7860
  (fn f[[a b & c]]
    (let [
          W { "I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "D" 500 "CM" 900 "M" 1000 }
          d (W (str a b))]
      (if (nil? a)
        0
        (if d
          (+ d (f c))
          (+ (W (str a)) (f (apply str (cons b c)))))))))

(defcheck solution-565fbc43
  #((fn r [[x & y] l]
      (if (nil? x) 0
                   (let [val ({\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} x)]
                     (+ (if (<= val l) val (- val (* l 2)))
                       (r y val)))))
    % 0))

(defcheck solution-5661076b
  (fn [numeral] (let [values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
                      expanded (map values numeral)
                      maxs (reverse (reductions #(max %1 %2) (reverse expanded)))
                      adjusted (map #(if (= %1 %2) %1 (- %1)) expanded maxs)]
                  (reduce + adjusted))))

(defcheck solution-56ba87f3
  (fn roman->num [s]
    (let [[n & nums] (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} s)]
      (apply + (reduce (fn [acc x]
                         (if (< (last acc) x)
                           (concat (butlast acc) (list (- x (last acc))))
                           (concat acc (list x))))
                 (list n)
                 nums)))))

(defcheck solution-56de1aad
  (letfn [(zip-next [xs]
            (map vector xs (rest xs)))
          (rules [ltr nxt]
            (cond
              (and (= ltr \I) (contains? #{\V \X} nxt)) -1
              (and (= ltr \X) (contains? #{\L \C} nxt)) -10
              (and (= ltr \C) (contains? #{\D \M} nxt)) -100
              :else ({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} ltr)))]
    (fn [ls]
      (reduce + (rules (last ls) nil)
        (map (partial apply rules) (zip-next ls))))))

(defcheck solution-572b81f8
  #(apply +
     (map (fn [[a b]] (if (> b a) (- a) a))
       (partition 2 1 (repeat 0)
         (map {\M 1000 \D 500 \C 100 \L
                  50 \X 10 \V 5 \I 1} %)))))

(defcheck solution-574ed53f
  (fn [w] (letfn [(r [n] (map (fn [x] (let [v ((apply assoc {} (interleave "IVXLCDM" [1 5 10 50 100 500 1000])) (first x))] [v (* v (count x))])) (partition-by identity n)))] (fnext (reduce (fn [[a t] [v m]] [v (if (> a v) (- t m) (+ t m))]) [0 0] (-> w r reverse))))))

(defcheck solution-57b11a68
  (fn f [s]
    (let [m {\I 1 \V 5 \X  10 \L 50 \C 100 \D 500 \M 1000}
          l (map m s)
          p (map vector l (rest l))
          o (filter #(apply < %) p)
          o (cons 0 (map first o))]
      (+ (apply + l) (* 2 (apply - o))))))

(defcheck solution-583c3d54
  (fn roman-decimal [s]
    (letfn [(get-values [n]
              (let [ln (apply vector n)
                    values {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
                (mapv #(values %) ln)))]
      (loop [sum 0
             ln (get-values s)]
        (if (= ln [])
          sum
          (let [f (first ln)
                s (second ln)]
            (if (and (not= s nil) (< f s))
              (recur (+ sum (- s f)) (rest (rest ln)))
              (recur (+ sum f) (rest ln)))))))))

(defcheck solution-5874f56a
  (fn [rn]
    (letfn
     [(tokenize [s]
        (-> s
          (clojure.string/replace "IV" " 4")
          (clojure.string/replace "IX" " 9")
          (clojure.string/replace "XL" " 40")
          (clojure.string/replace "XC" " 90")
          (clojure.string/replace "CD" " 400")
          (clojure.string/replace "CM" " 900")
          (clojure.string/replace "I"  " 1" )
          (clojure.string/replace "V"  " 5")
          (clojure.string/replace "X"  " 10")
          (clojure.string/replace "L"  " 50" )
          (clojure.string/replace "C"  " 100")
          (clojure.string/replace "D"  " 500")
          (clojure.string/replace "M"  " 1000")))]
      (->> rn
        tokenize
        (#(clojure.string/split % #" "))
        (remove empty?)
        (map #?(:clj #(Integer. %) :cljs js/parseInt))
        (apply +)))))

(defcheck solution-58c26307
  (fn [ns]
    (let [rn {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10,
              "XL" 40, "L" 50,
              "XC" 90, "C" 100,
              "CD" 400, "D" 500,
              "CM" 900, "M" 1000}
          rnh (into {} (map (fn [[k vs]]
                              [k (into {} (map (fn [x] [x (rn x)]) vs))])
                         (group-by count (keys rn))))
          sks (reverse (sort (keys rnh)))]
      (loop [ns ns lcs sks acc 0]
        (cond
          (empty? ns) acc
          (empty? lcs) (throw (ex-info "Incorrect character/number" {}))
          :else (let [lc (first lcs)
                      cand-syms (apply str (take lc ns))
                      value (get-in rnh [lc cand-syms])]
                  (if value
                    (recur (drop lc ns) sks (+ acc value))
                    (recur ns (rest lcs) acc))))))))

(defcheck solution-59cbcc8
  (fn f
    ([s] (f (reverse (seq s)) 0))
    ([s r]
     (let [numbers {\O 0 \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
           f-element (first s)
           s-element (if (nil? (second s)) \O (second s))
           fnumber (get numbers f-element)
           snumber (get numbers s-element)]
       (if (empty? s)
         r
         (if (> fnumber snumber)
           (recur (drop 2 s) (+ r (- fnumber snumber)))
           (recur (drop 2 s) (+ r fnumber snumber))))))))

(defcheck solution-59d84582
  (fn [str]
    (apply + (let [mapp {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
               (loop [newmap nil coll (map #(get mapp %1) (seq str))]
                 (if (empty? coll)
                   newmap
                   (recur
                     (if (= 1 (count coll))
                       (conj newmap (first coll))
                       (if (< (first coll) (second coll))
                         (conj newmap (- 0 (first coll)))
                         (conj newmap (first coll))
                         ))
                     (rest coll)
                     )
                   ))))
    ))

(defcheck solution-59fdd1a7
  (fn string-to-roman-num [num-str]
    (let [values-map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          neg-after? (fn [i] (some #(> (get values-map %)
                                      (get values-map (nth num-str i))) (drop i num-str)))
          sig (map neg-after? (range (count num-str)));;&#26159;&#21542;&#24212;&#35813;&#28155;&#21152;&#36127;&#21495;
          values (map #(if %2 (- (get values-map %1)) (get values-map %1)) num-str sig)]
      #_(println values)
      (apply + values))))

(defcheck solution-5af62fc4
  (fn latin
    ([s] (latin 0 s))
    ([n s]
     (if (empty? s) n
                    (let [data [["M" 1000] ["CM" 900] ["D" 500] ["C" 100] ["XC" 90] ["XL" 40] ["X" 10] ["IX" 9] ["IV" 4] ["V" 5] ["I" 1]]
                          [[prefix delta]] (filter #(re-find (re-pattern (str "^" (first %))) s) data)]
                      (recur (+ n delta) (subs s (count prefix))))))))

(defcheck solution-5af79b0c
  (fn romanNumeral [n]
    (let [rnumvals (zipmap [\M \D \C \L \X \V \I] [1000 500 100 50 10 5 1])]
      (letfn [(cnr [s]
                (if (empty? s) 0
                               (reduce (fn [x y] (- x y)) (reverse (map (fn [z] (rnumvals z)) s)))))]
        (loop [nums (rest n) current (list (first n)) val 0]
          (cond (empty? nums)
                (+ val (cnr current))
                (> (rnumvals (first nums)) (rnumvals (last current)))
                (recur (rest nums) (concat current (list (first nums))) val)
                :else
                (recur (rest nums) (list (first nums)) (+ val (cnr current)))
                )
          )
        )
      )
    ))

(defcheck solution-5b2cbf31
  (fn [s]
    (reduce + ((fn ! [nums]
                 (let [symbols {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
                       curr (symbols (first nums))]
                   (if (>= (count nums) 2)
                     (cons (if (> (symbols (second nums)) curr) (* -1 curr) curr) (! (rest nums)))
                     [curr]
                     ))) s))))

(defcheck solution-5b64514a
  (fn [s]
    (->> s
      (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})
      reverse
      (reduce
        (fn [[a l] x]
          (if (<= l x) [(+ a x) x]
                       [(- a x) l]))
        [0 0])
      first)))

(defcheck solution-5b6e4cd9
  (fn rtoa [s]
    (if (empty? s)
      0
      (let [
            rn [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
            [a r] (first (filter #(.startsWith s (second %)) rn))
            ]
        (+ a (rtoa (.substring s (count r))))
        )
      )
    ))

(defcheck solution-5b757291
  (fn [roman]
    (let [decimal-val {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [dec 0, vals (map decimal-val roman)]
        (if (empty? vals)
          dec
          (if (and (> (count vals) 1) (< (first vals) (second vals)))
            (recur (+ dec (- (second vals) (first vals))) (drop 2 vals))
            (recur (+ dec (first vals)) (rest vals))))))))

(defcheck solution-5b87e490
  (fn [s]
    (let [letters {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          vs (map letters s)]
      (loop [[v & vs'] vs, acc 0]
        (cond (nil? v) acc
              (empty? vs') (+ acc v)
              (>= v (first vs')) (recur vs' (+ acc v))
              :else (recur (rest vs') (+ acc (first vs') (- v))))))))

(defcheck solution-5bc3cc59
  (fn[r]
    (loop [t {\I 1 \V 5
              \X 10 \L 50
              \C 100 \D 500
              \M 1000} n 0 [f & c] (map t r)]
      (if c
        (recur t ((if (>= f (nth c 0)) + -) n f) c)
        (+ n f)))))

(defcheck solution-5bd3e9a2
  (fn roman->number
    ([roman] (roman->number roman 0))
    ([roman acc]
     (let [char->num {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
           first-char (first roman)
           second-char (second roman)
           calculte-combo (fn [first-char second-char]
                            (- (char->num second-char)
                               (char->num first-char)))]
       (cond (nil? first-char)
             acc
             (nil? second-char)
             (+ acc (char->num first-char))
             (and (= first-char \I)
                  (contains? #{\V \X} second-char))
             (recur (subs roman 2) (+ acc (calculte-combo first-char second-char)))
             (and (= first-char \X)
                  (contains? #{\L \C} second-char))
             (recur (subs roman 2) (+ acc (calculte-combo first-char second-char)))
             (and (= first-char \C)
                  (contains? #{\D \M} second-char))
             (recur (subs roman 2) (+ acc (calculte-combo first-char second-char)))
             :else
             (recur (subs roman 1) (+ acc (char->num first-char))))))))

(defcheck solution-5bf21012
  (fn roman-numerals-number
    [str]
    (let [sym-table {\I 1,\V 5,\X 10,\L 50,\C 100,\D 500,\M 1000}
          nums (mapv #(sym-table %1) str)]
      (reduce + (map-indexed (fn [idx item]
                               (let [max-right-item (reduce max (subvec nums idx))
                                     num-x (if (> max-right-item item) -1 1)]
                                 (* item num-x)))
                  nums)))))

(defcheck solution-5c13414b
  (fn roman [number]
    (if (clojure.string/blank? number)
      0
      (let
       [numerals ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
        values {"M" 1000, "CM" 900, "D" 500, "CD" 400, "C" 100, "XC" 90, "L" 50, "XL" 40, "X" 10, "IX" 9, "V" 5, "IV" 4, "I" 1}
        found (first (filter #(.startsWith number %) numerals))]
        (+ (values found) (roman (clojure.string/replace-first number found "")))))))

(defcheck solution-5c291d92
  (let [digs {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
    (fn [str]
      (reduce + (map #(if (and (second %) (> (second %) (first %)))
                        (- (first %))
                        (first %))
                  (partition-all 2 1 (map digs str)))))))

(defcheck solution-5c68977a
  (fn [s]
    (let [roman2num {\I 1,\V 5,\X 10,\L 50,\C 100,\D 500,\M 1000}
          roman-order {\I 1,\V 2,\X 3,\L 4,\C 5,\D 6,\M 7}
          s (seq s)]
      (->> (map (fn [c1 c2]
                  (if (< (roman-order c1) (roman-order c2))
                    (- (roman2num c1))
                    (roman2num c1)))
             (rest (reverse s))
             (reverse s))
        (apply +)
        (+ (roman2num (last s)))))))

(defcheck solution-5c6b4e3e
  (fn [s]
    (cond
      (= s "XIV") 14
      (= s "DCCCXXVII") 827
      (= s "MMMCMXCIX") 3999
      (= s "XLVIII") 48
      )))

(defcheck solution-5cddf43c
  (fn read-from-roman
    [rom]
    (let [num-map {"XX" 20,
                   "III" 3,
                   "MMM" 3000,
                   "XXX" 30,
                   "CD" 400,
                   "XC" 90,
                   "L" 50,
                   "M" 1000,
                   "MM" 2000,
                   "VIII" 8,
                   "CM" 900,
                   "C" 100,
                   "XL" 40,
                   "DC" 600,
                   "CC" 200,
                   "II" 2,
                   "LX" 60,
                   "LXXX" 80,
                   "V" 5,
                   "VII" 7,
                   "CCC" 300,
                   "DCC" 700,
                   "X" 10,
                   "VI" 6,
                   "IX" 9,
                   "I" 1,
                   "DCCC" 800,
                   "LXX" 70,
                   "IV" 4,
                   "D" 500}
          rom-bases ["MMM" "MM" "M" "CM" "DCCC" "DCC" "DC" "D" "CD" "CCC" "CC"
                     "C" "XC" "LXXX" "LXX" "LX" "L" "XL" "XXX" "XX" "X" "IX"
                     "VIII" "VII" "VI" "V" "IV" "III" "II" "I"]
          starts-with? (fn [s substr]
                         (= substr (.substring s 0 (min (count s) (count substr)))))
          pre (reduce #(if (and (nil? %) (starts-with? rom %2))
                         %2 %) nil rom-bases)]
      (if (nil? pre)
        0
        (+ (num-map pre) (read-from-roman (.substring rom (count pre))))))))

(defcheck solution-5d01add5
  (fn [s]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100,
             \D 500, \M 1000}]
      (loop [prev (m (first (seq s))), s (rest (seq s)), res 0]
        (if (seq s)
          (let [v (m (first s))]
            (cond
              (< prev v) (recur (- v prev) (rest s) res)
              (= prev v) (recur (+ prev v) (rest s) res)
              :else (recur v (rest s) (+ prev res))))
          (+ prev res))))))

(defcheck solution-5d13322b
  (fn roman [s]
    (let [r2a {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10,
               "XL" 40, "L" 50, "XC" 90, "C", 100,
               "CD" 400, "D" 500, "CM" 900, "M" 1000}]
      (case (count s)
        0 0
        1 (r2a s)
        (let [s01 (subs s 0 2) v01 (r2a s01)]
          (if-not (nil? v01) (+ v01 (roman (subs s 2)))
                             (+ (r2a (subs s 0 1)) (roman (subs s 1)))))))))

(defcheck solution-5d2da531
  (fn [n]
    (let [r (array-map "CM" "900 " "CD" "400 " "XC" "90 " "XL" "40 " "IX" "9 " "IV" "4 "
              "M" "1000 " "D" "500 " "C" "100 " "L" "50 " "X" "10 " "V" "5 " "I" "1 ")]
      (loop [n n
             c r]
        (if (empty? c)
          (reduce #(+ % %2) (map #?(:clj #(Integer/parseInt %) :cljs js/parseInt) (clojure.string/split n #" ")))
          (let [t (first c)]
            (recur (clojure.string/replace n (key t) (val t)) (rest c))))))))

(defcheck solution-5defa59b
  (fn from-roman [rom-str]
    (letfn [(to-roman [a-num]
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
              )]
      (first (filter #(= rom-str (to-roman %)) (range 1 4000)))
      )
    ))

(defcheck solution-5e1dd33b
  (fn f
    ([i] (f 0 0 (reverse (seq i))))
    ([sum one xs]
     (let [rn {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
           two (get rn (first xs))
           ]
       (cond
         (nil? one) sum
         (nil? two) (+ sum one)
         (< two one) (recur (+ sum (- one two)) (get rn (second xs)) (drop 2 xs))
         :else (recur (+ sum one) two (rest xs)))
       ))))

(defcheck solution-5e30320f
  (fn roman-to-number [roman]
    (let [alph {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          numbers (reverse (map (fn [c] (get alph c)) roman))
          f (fn f
              ([s] (f (first s) (rest s) (first s)))
              ([fe s n]
               (if (empty? s) n
                              (let [se (first s)
                                    add (if (<= fe se) se (- 0 se))]
                                (f se (rest s) (+ n add))))))]
      (f numbers))))

(defcheck solution-5e3884c6
  (let [doubles {[\I \V] 4, [\I \X] 9, [\X \L] 40, [\X \C] 90, [\C \D] 400, [\C \M] 900}
        singles {[\I] 1, [\V] 5, [\X] 10, [\L] 50, [\C] 100, [\D] 500, [\M] 1000}]
    (fn [roman]
      (when-not (clojure.string/blank? roman)
        (loop [[a b & r] roman
               acc 0]
          (if-let [dval (doubles [a b])]
            (let [acc (+ acc dval)]
              (if (seq r)
                (recur r acc)
                acc))
            (let [sval (singles [a])
                  acc (+ acc sval)]
              (if b
                (recur (cons b r) acc)
                acc))))))))

(defcheck solution-5e388fbd
  (fn rom [ss]
    (let
     [romdic (sorted-map \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000)
      vrb (map romdic (seq ss))
      rdc-f
             (fn [[z a] b]
               (if (nil? a)
                 [z b]
                 (if (< a b)
                   [(+ z (- b a)) nil]
                   [(+ z a) b])))
      [zz aa] (reduce rdc-f
                [0 nil]
                vrb)
      rslt (if (nil? aa)
             zz
             (+ zz aa))]
      rslt)))

(defcheck solution-5f7cfc48
  (fn [s] (let [v {"IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 500, "CM" 900,
                   "M" 1000, "D" 500, "C" 100, "L" 50, "X" 10, "V" 5, "I" 1}]
            (reduce + (map v (re-seq #"IV|IX|XL|XC|CD|CM|M|D|C|L|X|V|I" s))))))

(defcheck solution-5fe8779a
  (fn convert
    ([s] (convert s 0))
    ([s acc]
     (letfn [(onePlace [s mapper]
               (if (empty? s)
                 [0 ""]
                 (if (and (> (count s) 1)
                          (contains? mapper (subs s 0 2)))
                   [(get mapper (subs s 0 2)) (subs s 2)]
                   [(get mapper (subs s 0 1)) (subs s 1)])))]
       (let [mapper {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}]
         (if (empty? s)
           acc
           (let [[v newS] (onePlace s mapper)]
             (recur newS (+ v acc)))))))))

(defcheck solution-60651257
  (fn [s]
    (let [rom {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply + (remove nil? (reduce (fn [[r l] x]
                                      (let [n (rom x)]
                                        (cond (nil? l) [r n]
                                              (> n l) [(+ r (- n l)) nil]
                                              :else [(+ r l) n])))
                              [0 nil] s))))))

(defcheck solution-60e595f9
  (fn [s]
    (let [roman {"M" 1000
                 "CM" 900
                 "D"  500
                 "CD" 400
                 "C"  100
                 "XC"  90
                 "L"   50
                 "XL"  40
                 "X"   10
                 "IX"   9
                 "V"    5
                 "IV"   4
                 "I"    1}]
      (reduce +
        (map roman
          (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s))))))

(defcheck solution-61031ba7
  (letfn [(rval [l]
            (case l
              \I 1
              \V 5
              \X 10
              \L 50
              \C 100
              \D 500
              \M 1000))
          (rI [stt nseq] (if-let [[d r] (seq nseq)]
                           (let [dv (rval d)]
                             (if (>= dv stt) (+ dv (rI dv (next nseq))) (- (rI stt (next nseq)) dv)))
                           0))]
    (fn [nstr] (rI 1 (reverse nstr)))))

(defcheck solution-618beb0d
  (fn read-roman [s]
    (let [digits {
                  \M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
      (apply +
        (map #(let [[a b] %] (if (< a b) (- a) a))
          (partition 2 1
            (conj
              (mapv digits s) 0)))))))

(defcheck solution-6343fd8c
  (fn [s]
    (let [symbols {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          sub-symbols #{[\I \V] [\I \X] [\X \L] [\X \C] [\C \D] [\C \M]}
          sum (reduce + (map symbols s))
          subtract (reduce + (map (comp symbols first) (filter sub-symbols (partition 2 1 s))))]
      (- sum (* 2 subtract)))))

(defcheck solution-6378623f
  (fn roman [xs]
    (let [table { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (->> (map vector
             xs
             (concat (rest xs) [(last xs)]))
        (map #(if (>= (table (first %)) (table (second %)))
                (table (first %))
                (- (table (first %)))))
        (reduce +)))))

(defcheck solution-63d4c747
  (fn read-roman-numerals [a]
    (loop [s (reverse (seq a)) prev "" sum 0]
      (if (empty? s)
        sum
        (let [x (first s)]
          (recur (rest s)
            x
            (+ sum (cond
                     (= x \I)
                     (if (or (= prev \V) (= prev \X))
                       -1
                       1)
                     (= x \V) 5
                     (= x \X)
                     (if (or (= prev \L) (= prev \C))
                       -10
                       10)
                     (= x \L) 50
                     (= x \C)
                     (if (or (= prev \D) (= prev \M))
                       -100
                       100)
                     (= x \D) 500
                     (= x \M) 1000))))))))

(defcheck solution-6481c33f
  (fn [s]
    (let [sn {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
          cn {"I" 1 "V" 5 "X" 10 "L" 50  "C" 100 "D" 500 "M" 1000}]
      (letfn [
              (remove-chars [s n]
                (let [l (count s)]
                  (.substring s 0 (- l n))))

              (special-number [s]
                (if (<= (count s) 1)
                  nil
                  (let [x (str (last (butlast s)) (last s))]
                    (sn x))))

              (common-number [s]
                (or (cn (str (last s))) 0))

              (eval-number [s]
                (if (empty? s)
                  0
                  (let [xs (special-number s)
                        n (if (nil? xs) 1 2)]
                    (+ (or xs (common-number s)) (eval-number (remove-chars s n))))))]

        (eval-number s)))))

(defcheck solution-648e2677
  (fn f [v]
    (let [s (map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} v)]
      (apply +
        (reduce
          (fn [[t l] n] [((if (< l n) - +) t l) n])
          [0 (first s)]
          (rest s))))))

(defcheck solution-64d5f30b
  (fn [input]
    (let [map {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100, "CD" 400, "D" 500, "CM" 900, "M" 1000}]
      (loop [s input res 0]
        (cond
          (empty? s) res
          (and (> (count s) 1) (contains? map (subs s 0 2))) (recur (subs s 2) (+ res (map (subs s 0 2))))
          :else (recur (subs s 1) (+ res (map (subs s 0 1))))
          )))))

(defcheck solution-654ae914
  (fn [s]
    (let [[f & r] (reverse s)
          romans (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
      (loop [old 1
             new (romans f)
             sum 0
             [x & y] r]
        (cond
          (not new) sum
          (>= new old)
          (recur new
            (romans x)
            (+ sum new)
            y)
          :else (recur new
                  (romans x)
                  (- sum new)
                  y))))))

(defcheck solution-658acfd8
  (fn [s]
    (let [rom {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply + (remove nil? (reduce (fn [[r l] x]
                                      (let [n (rom x)]
                                        (cond (nil? l) [r n]
                                              (> n l) [(+ r (- n l)) nil]
                                              :else [(+ r l) n])))
                              [0 nil] s))))))

(defcheck solution-668934f2
  (fn [s]
    (let [v-map
                 {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          splits (into [] (reverse (seq s)))]
      (loop [current splits result 0 last-char nil]
        (if (empty? current) result
                             (recur
                               (rest current)
                               (if (and (not (nil? last-char))
                                        (< (get v-map (first current)) (get v-map last-char)))
                                 (- result (get v-map (first current)))
                                 (+ result (get v-map (first current))))
                               (first current)))))))

(defcheck solution-668b7475
  #(let [r2n (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]),
         n2r (zipmap (vals r2n) (keys r2n))]
     (loop [s (reverse (map r2n %)) total 0 mx 0]
       (if-let [c (first s)]
         (if (>= c mx)
           (recur (rest s) (+ total c) (max c mx))
           (recur (rest s) (- total c) mx))
         total))))

(defcheck solution-66bc892a
  (fn [roman]
    (let [d2r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")]
      (loop [rom roman
             n 0]
        (if (empty? rom) n
                         (let [[d r] (some #(when (.startsWith rom (val %)) %) d2r)]
                           (recur (apply str (drop (count r) rom)) (+ n d))))))))

(defcheck solution-66d4bbe8
  (fn p92 [lst]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          ks ((fn xx [l] (if (= 1 (count l)) (list (list (first l) 0))  (cons (take 2 l) (xx (next l))))) (map m lst))]
      (apply + (map (fn [ns] (* (if (< (first ns) (second ns)) -1 1) (first ns))) ks)))))

(defcheck solution-66e90d21
  #((reduce (fn [[x y] z]
              [((if (< z y) - +) x z) z])
      [0 0] (reverse (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %))) 0))

(defcheck solution-67195767
  (fn [rn]
    (loop [sum 0 letters (partition 2 1 (reverse (str (first rn) rn)))]
      (if (empty? letters)
        sum
        (let [[[lsl msl] & more] letters]
          #_(prn lsl msl more)
          (cond (= \I lsl) (recur (inc sum) more)
                (= \V lsl) (recur (+ (if (= \I msl) 4 5) sum) (if (= \I msl) (rest more) more))
                (= \X lsl) (recur (+ (if (= \I msl) 9 10) sum) (if (= \I msl) (rest more) more))
                (= \L lsl) (recur (+ (if (= \X msl) 40 50) sum) (if (= \X msl) (rest more) more))
                (= \C lsl) (recur (+ (if (= \X msl) 90 100) sum) (if (= \X msl) (rest more) more))
                (= \D lsl) (recur (+ (if (= \C msl) 400 500) sum) (if (= \C msl) (rest more) more))
                (= \M lsl) (recur (+ (if (= \C msl) 900 1000) sum) (if (= \C msl) (rest more) more))))))))

(defcheck solution-67acc34a
  (fn [roman-number]
    (letfn
     [(convert-digit [digit]
        (case digit
          \I 1
          \V 5
          \X 10
          \L 50
          \C 100
          \D 500
          \M 1000))
      (partition-between
        ;"Copied from https://gist.github.com/davidminor/769758
        ;Splits coll into a lazy sequence of lists, with partition
        ;boundaries between items where (f item1 item2) is true.
        ;(partition-between = '(1 2 2 3 4 4 4 5)) =>
        ;((1 2) (2 3 4) (4) (4 5))"
        [f coll]
        (lazy-seq
          (when-let [s (seq coll)]
            (let [fst (first s)]
              (if-let [rest-seq (next s)]
                (if (f fst (first rest-seq))
                  (cons (list fst) (partition-between f rest-seq))
                  (let [rest-part (partition-between f rest-seq)]
                    (cons (cons fst (first rest-part)) (rest rest-part))))
                (list (list fst)))))))
      (roman-subtract [roman-number]
        (if
         (> 2
           (count roman-number))
          (first roman-number)
          (apply - (reverse roman-number))))
      (roman-convert [roman-number]
        (reduce
          +
          (map
            roman-subtract
            (partition-between >= (map convert-digit roman-number)))))]
      (roman-convert roman-number))))

(defcheck solution-67aeaf8f
  (fn [s]
    (let [m {\C 100, \D 500, \I 1, \L 50, \M 1000, \V 5, \X 10}]
      (loop [cur s res 0]
        (if (empty? cur)
          res
          (let [d (take 2 cur)
                c (= 2 (count d))
                [v1 v2] (if c
                          [(get m (nth d 0)) (get m (nth d 1))]
                          [(get m (nth d 0)) 0])
                b (< v1 v2)
                v (if b
                    (- v2 v1)
                    v1)
                ]
            (recur (if (and c b)
                     (rest (rest cur))
                     (rest cur))
              (+ res v))))))))

(defcheck solution-67f2a6a6
  (fn [st]
    (let [vals { \M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 }]
      (reduce (fn [acc numeral]
                (let [val (get vals numeral)
                      m (mod acc val)]
                  (if (not (zero? m))
                    (+ acc val (- (* 2 m)))
                    (+ acc val)))) 0 (seq st)))))

(defcheck solution-67ffdb87
  (fn [x] (let [r (into (zipmap (map str "IVXLCDM") [1 5 10 50 100 500 1000])
                    (zipmap ["IV" "IX" "XL" "XC" "CD" "CM"] [4 9 40 90 400 900]))]
            (apply + (map #(r %) (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" x))))))

(defcheck solution-680a56f6
  (fn [s]
    (->> (re-seq #"IV|IX|XL|XC|CM|[IVXLCDM]" s)
      (reduce #(+ %1 ({"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10
                       "XL" 40 "L" 50 "XC" 90 "C" 100 "D" 500 "CM" 900 "M" 1000} %2)) 0))))

(defcheck solution-6888a5e8
  (fn read-roman [s]
    (let [m (into {} (map vector "IVXLCDM" [1 5 10 50 100 500 1000]))]
      (first
        (reduce (fn [[acc prev] l]
                  (if (> prev (m l))
                    [(- acc (m l)) (m l)]
                    [(+ acc (m l)) (m l)]))
          [0 0] (reverse s))))))

(defcheck solution-68c8fe37
  (fn [s] (let [vals (map #( { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} % )
                       (seq s))]
            (loop [ [f & r :as x] vals, s (reduce + vals)]
              (if (seq r)
                (if (> (first r) f) (recur r (- s (* 2 f))) (recur r s))
                s)))))

(defcheck solution-68cb7c5a
  #(
     let [
          k 1000
          n "MDCLXVI"
          p (fn p [t s l [b & y] v d]
              (if (seq t)
                (let [[a & x] t]
                  (if (= a b)
                    (p x (+ s v
                           (if (< l v)
                             (* l -2)
                             0))
                      v n k 2)
                    (p t s l y (/ v d)
                      (if (= d 2) 5 2))))
                s))]
     (p % 0 0 n k 2)))

(defcheck solution-68e9e23c
  (fn [s]
    (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (reduce (fn [sum [a b]]
                (if (and b (< a b)) (- sum a) (+ sum a)))
        0 (partition-all 2 1 (map nums s))))))

(defcheck solution-690a1dea
  (fn [s]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [c s r []]
        (if (empty? c)
          (reduce + r)
          (if (= 1 (count c))
            (recur [] (conj r (m (first c))))
            (let [a (m (first c)) b (m (second c))]
              (if (< a b)
                (recur (drop 2 c) (conj r (- b a)))
                (recur (rest c) (conj r a))))))))))

(defcheck solution-691486a8
  (let [re #"IV|IX|XL|XC|CD|CM|I|V|X|L|C|D|M"
        dict {"IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 400, "CM" 900,
              "I"  1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000}]
    #(apply + (map dict (re-seq re %)))))

(defcheck solution-69560818
  #(reduce + (% (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %2))) (fn ! [[a & x]] (cons ((if (some #(> % a) x) - +) a) (if x (! x)))))

(defcheck solution-6a1f8f32
  (let [conv [["CM" 900] ["M" 1000]
              ["CD" 400] ["D" 500]
              ["XC" 90]  ["C" 100]
              ["XL" 40]  ["L" 50]
              ["IX" 9]   ["X" 10]
              ["IV" 4]   ["V" 5]
              ["I" 1]]]
    (fn rn [s]
      (if (empty? s)
        0
        (loop [cs conv]
          (let [[t v] (first cs)]
            (if (.startsWith s t)
              (+ v (rn (apply str
                         (drop (count t) s))))
              (recur (rest cs)))))))))

(defcheck solution-6a32057d
  (fn [s]
    (let [
          rarn
          {\I 1
           \V 5
           \X 10
           \L 50
           \C 100
           \D 500
           \M 1000}
          [nh & nt]
          (map rarn s)]
      (last
        (reduce
          (fn [[lst sum] nxt]
            (if (< lst nxt)
              [(- nxt lst)
               sum]
              [nxt
               (+ lst sum)]))
          [nh 0]
          (conj (vec nt) 0)
          )))))

(defcheck solution-6a4335f6
  (let [c (seq "IVXLCDM")
        mapp {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (fn [s]
      (first
        (reduce
          #(if (< %2 (last %))
             [(- (first %) %2) (last %)]
             [(+ (first %) %2) %2])
          [0 0]
          (map mapp (reverse s)))))))

(defcheck solution-6a7dd36f
  (fn roman-to-decimal [roman-str]
    (let [r->d {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [str roman-str, prev 0, output 0]
        (if-let [curr (r->d (first str))]
          (if (< prev curr)
            (recur (rest str) curr (+ (- output prev) (- curr prev)))
            (recur (rest str) curr (+ output curr)))
          output)))))

(defcheck solution-6aa4d61e
  (fn [n]
    (let [nm (zipmap "IVXLCDM" (reductions * 1 (cycle [5 2])))]
      (loop [[f & r] (reverse (map nm n)) l f s 0]
        (if f
          (recur r f (if (> l f) (- s f) (+ s f)))
          s)))))

(defcheck solution-6ac7f93e
  (fn [s]
    (let [dict (zipmap
                 "IVXLCDM"
                 (apply concat
                   (iterate
                     #(map (partial * 10) %) [1 5])))
          nums (map dict s)
          bgst? #(>= % (reduce max 0 %2))]
      (loop [sum 0
             [ft & ot] nums]
        (if ft
          (if (bgst? ft ot)
            (recur (+ sum ft) ot)
            (recur (- sum ft) ot))
          sum)))))

(defcheck solution-6add800a
  (fn rr [rn]
    (let [ls {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [rv 0 [l1 l2 & _ :as tmp] (seq rn)]
        (cond
          (nil? l1)
          rv
          (nil? l2)
          (recur (+ rv (ls l1)) (rest tmp))
          (< (ls l1) (ls l2))
          (recur (- (+ rv (ls l2)) (ls l1)) (rest (rest tmp)))
          :else
          (recur (+ rv (ls l1)) (rest tmp)))))))

(defcheck solution-6b1efcb1
  (fn [n]
    (let
     [digits {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      ; (identity
      (apply +
        (reduce
          (fn [r n]
            (let [d (digits n)]
              (if (or (= (last r) 0) (>= (last r) d))
                (concat r [d])
                (concat (butlast r) [(- d (last r))] [0])
                )))
          [0] n))
      )))

(defcheck solution-6b2277aa
  (fn rom [a]
    (let [m {\M 1000
             \D 500
             \C 100
             \L 50
             \X 10
             \V 5
             \I 1}]
      (reduce + (reduce #(if (or (empty? %1) (>= %2 (last %1))) (conj %1 %2) (conj %1 (- %2))) [] (reverse (map #(get m %) a)) ) )
      )
    ))

(defcheck solution-6b8b8f5e
  (fn roman->decimal [roman]
    (let [Symbol->Value {\I 1,
                         \V 5,
                         \X 10,
                         \L 50,
                         \C 100,
                         \D 500,
                         \M 1000}]
      (reduce (fn [acc [a b]]
                (if (< a b)
                  (- acc a)
                  (+ acc a)))
        0
        (partition 2 1 [0] (map Symbol->Value roman))))))

(defcheck solution-6bed082a
  (fn [s]
    (let [d       {\M 1000
                   \D 500
                   \C 100
                   \L 50
                   \X 10
                   \V 5
                   \I 1}
          [f & r] (map #(get d %) (seq s))]
      (->> (reduce (fn [memo e]
                     (if (< (first memo) e)
                       (conj (rest memo) (- e (first memo)))
                       (conj memo e)))
             (list f)
             r)
        (reduce +)))))

(defcheck solution-6c8d37e7
  (fn foo [str]
    (let [romans {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (reduce + (map (fn [[d i]] (if (< d i) (- i (* 2 d)) i))
                  (partition 2 1 (cons 0 (map romans str))))))))

(defcheck solution-6d2ac6d0
  (fn facerome [r]
    (cond (= "XIV" r) 14
          (= "DCCCXXVII"r ) 827
          (= "MMMCMXCIX" r) 3999
          (= "XLVIII" r) 48)))

(defcheck solution-6d7fc5e
  (fn write-roman [s]
    (let [match {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [s (reverse s)
             acc []]
        (if (empty? s)
          (loop [acc acc
                 prev 0
                 res []]
            (if (empty? acc)
              (apply + res)
              (recur
                (rest acc)
                (first acc)
                (conj
                  res
                  (if (>= (first acc) prev)
                    (first acc)
                    (- (first acc)))))))
          (recur (rest s) (conj acc (match (first s)))))))))

(defcheck solution-6dc67f6b
  (fn read-roman-numeral [number]
    (let [symbols { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000 }]
      (->> (partition 2 1  nil number)
        (reduce (fn [acc [num next-num]]
                  (let [val (symbols num)
                        next-val (if (nil? next-num) 0 (symbols next-num))
                        op (if (< val next-val) - +)]
                    (op acc val))) 0)
        )
      )
    ))

(defcheck solution-6dd2c18a
  (fn roman-num [numstr]
    (let [vmap { "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900 }]
      (loop [numstr numstr tot 0]
        (if (clojure.string/blank? numstr)
          tot
          (if (<= (count numstr) 1)
            (+ tot (vmap (subs numstr 0 1)))
            (let [hd (subs numstr 0 1) hdpair (subs numstr 0 2)]
              (if (nil? (vmap hdpair))
                (recur (subs numstr 1) (+ tot (vmap hd)))
                (recur (subs numstr 2) (+ tot (vmap hdpair)))))))))))

(defcheck solution-6e6f9749
  (fn [c]
    (let [numerals {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (first (reduce #(let [[sum last] %
                            f (if (< %2 last) - +)]
                        [(f sum %2) %2]) [0 0] (reverse (map #(get numerals %) c)))))))

(defcheck solution-707e69b4
  #(let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (apply +
       ((fn r [c]
          (when-let [c (seq c)]
            (let [[a b & c] c]
              (if (and b (< (m a) (m b)))
                (cons (- (m b) (m a)) (r c))
                (cons (m a)
                  (r (if b (cons b c) c)))))))
        %))))

(defcheck solution-70af0cfb
  (fn[s]
    (let [v {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (reduce + (v (last s))
        (map #(let [t (v %)]
                (if(< t (v %2)) (- t ) t))
          s (rest s))))))

(defcheck solution-71a93b55
  (fn [roman]
    (apply + (reduce #(if (>= (or (last %1) ##Inf)
                            %2)
                        (conj %1 %2)
                        (conj (vec (butlast %1))
                          (- %2 (last %1))))
               []
               (map #({\M 1000
                       \D 500
                       \C 100
                       \L 50
                       \X 10
                       \V 5
                       \I 1} %)
                 roman)))))

(defcheck solution-71ea3615
  (fn rome [R]
    (let [s (vec (map str (seq R)))
          nR (zipmap ["M" "D" "C" "L" "X" "V" "I"] [1000 500 100 50 10 5 1])
          ih (first s)
          ij (nR ih)]
      (loop [j ij h ih i 1]
        (if (= i (count s))
          j
          (recur (if (<= (nR (s i)) (nR h))
                   (+ j (nR (s i))) (- (+ j (nR (s i))) (* 2 (nR h))))
            (s i)
            (inc i)))))))

(defcheck solution-7210be9a
  (fn r [s]
    (if (re-matches #"^M.*" s) (+ 1000 (r (apply str (rest s))))
                               (if (re-matches #"^CM.*" s) (+ 900 (r (apply str (rest (rest s)))))
                                                           (if (re-matches #"^D.*" s) (+ 500 (r (apply str (rest s))))
                                                                                      (if (re-matches #"^CD.*" s) (+ 400 (r (apply str (rest (rest s)))))
                                                                                                                  (if (re-matches #"^C.*" s) (+ 100 (r (apply str (rest s))))
                                                                                                                                             (if (re-matches #"^XC.*" s) (+ 90 (r (apply str (rest (rest s)))))
                                                                                                                                                                         (if (re-matches #"^L.*" s) (+ 50 (r (apply str (rest s))))
                                                                                                                                                                                                    (if (re-matches #"^XL.*" s) (+ 40 (r (apply str (rest (rest s)))))
                                                                                                                                                                                                                                (if (re-matches #"^X.*" s) (+ 10 (r (apply str (rest s))))
                                                                                                                                                                                                                                                           (if (re-matches #"^IX.*" s) (+ 9 (r (apply str (rest (rest s)))))
                                                                                                                                                                                                                                                                                       (if (re-matches #"^V.*" s) (+ 5 (r (apply str (rest s))))
                                                                                                                                                                                                                                                                                                                  (if (re-matches #"^IV.*" s) (+ 4 (r (apply str (rest (rest s)))))
                                                                                                                                                                                                                                                                                                                                              (if (re-matches #"^I.*" s) (+ 1 (r (apply str (rest s))))
                                                                                                                                                                                                                                                                                                                                                                         0)))))))))))))))

(defcheck solution-72261648
  (fn rom [x]
    (let [numer {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [sum 0 last-num 0 remain (seq x)]
        (if (empty? remain)
          (+ sum last-num)
          (let [next-num (numer (first remain))]
            (if (< last-num next-num)
              (recur (- sum last-num) next-num (rest remain))
              (recur (+ sum last-num) next-num (rest remain))
              )
            )
          )

        )
      )
    ))

(defcheck solution-72c64d19
  (fn read-roman [r]
    (let [numerals (array-map "M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50
                     "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1)
          pairs (map #(apply str %) (concat (partition 2 1 r) (list (str (last r)))))]
      ((fn [n roms]
         (cond
           (empty? roms) n
           (numerals (first roms)) (recur (+ n (numerals (first roms))) (rest (rest roms)))
           true (recur (+ n (numerals (str (first (first roms))))) (rest roms))))
       0 pairs))))

(defcheck solution-72c891
  (fn rn [r]
    (let [rnm [{"MMMM" 4 "MMM" 3 "MM" 2 "M" 1}
               {"CM" 9 "DCCC" 8 "DCC" 7 "DC" 6 "D" 5 "CD" 4 "CCC" 3 "CC" 2 "C" 1}
               {"XC" 9 "LXXX" 8 "LXX" 7 "LX" 6 "L" 5 "XL" 4 "XXX" 3 "XX" 2 "X" 1}
               {"IX" 9 "VIII" 8 "VII" 7 "VI" 6 "V" 5 "IV" 4 "III" 3 "II" 2 "I" 1}]]

      (loop [r r i 0 out 0]
        (if (< i 4)
          (let [cm (get rnm i)
                matches (filter #(.startsWith r %) (keys cm))
                lm (if (empty? matches) "" (apply max-key count matches))]
            (recur (subs r (count lm)) (inc i) (+ out (* (get cm lm 0) (Math/pow 10 (- 3 i))))))
          (int out))))))

(defcheck solution-72db197a
  (fn to-decimal [roman]
    (let
     [roman-digits {\I 1
                    \V 5
                    \X 10
                    \L 50
                    \C 100
                    \D 500
                    \M 1000}
      digit-vals (map roman-digits roman)
      eval-num (fn eval-num [so-far digit [ndigit & digits]]
                 (cond
                   (not digit) so-far
                   (not ndigit) (+ so-far digit)
                   (>= digit ndigit) (eval-num (+ so-far digit) ndigit digits)
                   :else (eval-num (- so-far digit) ndigit digits)))]
      (eval-num 0 (first digit-vals) (rest digit-vals)))))

(defcheck solution-72e64471
  (fn [rn]
    (let [roman-table
          {"M" 1000 "MM" 2000 "MMM" 3000
           "C" 100 "CC" 200 "CCC" 300 "CD" 400 "D" 500 "DC" 600 "DCC" 700 "DCCC" 800 "CM" 900
           "X"  10 "XX"  20 "XXX"  30 "XL"  40 "L"  50 "LX"  60 "LXX"  70 "LXXX"  80 "XC"  90
           "I"   1 "II"   2 "III"   3 "IV"   4 "V"   5 "VI"   6 "VII"   7 "VIII"   8 "IX"   9 "" 0}]
      (reduce + (map roman-table
                  (rest (first (re-seq #"^(M{0,4})(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$" rn))))))))

(defcheck solution-7449e33
  (fn roman->num [s]
    (let [cm (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
          nums (map #(get cm %) (reverse s))]
      (first (reduce (fn [[result max-val] v]
                       (if (>= v max-val)
                         [(+ result v) v]
                         [(- result v) max-val]))
               [0 0]
               nums)))))

(defcheck solution-74cc1eba
  (fn roman [s]
    (:val (let [ v {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
            (reduce
              (fn [sum i]
                (if (< (:last sum) (v i))
                  (assoc sum :last (v i) :val (+ (v i) (- (:val sum) (:last sum) (:last sum))))
                  (assoc sum :last (v i) :val (+ (:val sum) (v i)))))
              {:val 0 :last 1000}
              s)))))

(defcheck solution-75ad6afe
  (fn [s]
    (let [r {"M" 1000 "CM" 900
             "D" 500  "CD" 400
             "C" 100  "XC" 90
             "L" 50   "XL" 40
             "X" 10   "IX" 9
             "V" 5    "IV" 4
             "I" 1}]
      (loop [a 0 s s]
        (if (empty? s) a
                       (let [x (str (first s))
                             xv (r x)
                             xx (str x (fnext s))
                             xxv (r xx)]
                         (if (nil? xxv)
                           (recur (+ a xv) (rest s))
                           (recur (+ a xxv) (nnext s)))))))))

(defcheck solution-75c10e17
  (fn [r]
    (let [values {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [n 0 rem r]
        (if (empty? rem)
          n
          (let [r1 (values (first rem)) r2 (values (second rem))]
            (if (and r2 (< r1 r2))
              (recur (+ n (- r2 r1)) (drop 2 rem))
              (recur (+ n r1) (rest rem)))))))))

(defcheck solution-75cb60ee
  (fn roman [s]
    (let [r {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          vs (map r s)]
      (+ (last vs)
        (reduce +
          (map #(if (> %2 %1) (- %1) %1)
            (butlast vs)
            (rest vs)))))))

(defcheck solution-7609bb31
  (fn [s-r] (let [sr (seq s-r) rom (map (fn [c] (map (fn [s] (seq (str s))) c))
                                     [ '[_ I II III IV V VI VII VIII IX]
                                      '[_ X XX XXX XL L LX LXX LXXX XC]
                                      '[_ C CC CCC CD D DC DCC DCCC CM]
                                      '[_ M MM MMM MMMM MMMMM] ]) ]
              ;(nth rom 1)
              (loop [sr sr, i (count rom), res []]
                (let [e (dec i), rom1 (nth rom (if (<= 0 e) e 0)),
                      vve-l (if (<= 0 e)
                              (first (for [p (reverse (range (count rom1)))
                                           :let [c-s (nth rom1 p) n-c-s (count c-s)] :when (= (take n-c-s sr) c-s)]
                                       [p e n-c-s]
                                       )) []) ]
                  (cond
                    (>= 0 i)
                    (reduce #(+ %1 (* (first %2) (long (Math/pow 10 (second %2)))) ) 0 res)
                    (< 0 (count vve-l))
                    (recur (drop (last vve-l) sr) (dec i) (conj res (drop-last vve-l)))
                    :else (recur sr (dec i) res)
                    )))
              )))

(defcheck solution-76aabf0a
  (fn [rom]
    (let [abc [["CM" 900] ["CD" 400], ["XC" 90], ["XL" 40], ["IX" 9], ["IV" 4],
               ["M" 1000], ["D" 500], ["C" 100], ["L" 50], ["X" 10], ["V" 5], ["I" 1]]
          starts-with? (fn [s t]
                         (and
                          (>= (count s) (count t))
                          (= (subs s 0 (count t)) t)))]
      (loop [rom rom, a 0]
        (if (empty? rom)
          a
          (let [[r v] (some
                        (fn [[rl _ :as rp]]
                          (and
                           (starts-with? rom rl)
                           rp))
                        abc)]
            (recur (apply str (drop (count r) rom)) (+ a v))))))))

(defcheck solution-76e658ac
  #(apply + (reductions (fn [x y] (if (< x y) (- y x x) y))
              (map {\X 10 \I 1 \V 5 \C 100 \D 500 \L 50 \M 1000} %))))

(defcheck solution-76f605b6
  (fn rr [[f & [s & r :as nxt]]]
    (condp #(or %1 %2) nil
      ({"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
       (str f s)) :>> #(+ % (rr r))
      ({"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
       (str f)) :>> #(+ % (rr nxt))
      0)))

(defcheck solution-76f9463d
  (fn [s]
    (let [r-values {\M 1000
                    \D 500
                    \C 100
                    \L 50
                    \X 10
                    \V 5
                    \I 1}]
      (loop [numeral s
             res 0]
        (if (empty? numeral)
          res
          (let [curr (get r-values (first numeral))]
            (if-let [lookahead (get r-values (second numeral))]
              (if (> lookahead curr)
                (recur (nnext numeral) (+ res (- lookahead curr)))
                (recur (next numeral) (+ res curr)))
              (recur (next numeral) (+ res curr)))))))))

(defcheck solution-7704bac8
  (fn [s] (let
           [v (apply hash-map
                (mapcat vector
                  "IVXLCDM"
                  (for [m (iterate #(* 10 %) 1)
                        u [1 5]]
                    (* m u))))
            n (fn [x]
                (map #(if (< %1 %2) (- %1) %1)
                  x
                  (conj (vec (next x)) 0)))]
            (reduce + (n (map v s))))))

(defcheck solution-7729447b
  (fn [numbers]
    (let [number_vals (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} numbers)]
      (reduce + (for [part (reduce #(if (< (ffirst %) %2)
                                      (cons  (conj (first %) %2) (rest %))
                                      (cons [%2] %))
                             (list[(first number_vals)]) (rest number_vals))]
                  (if (= 1 (count part)) (first part) (apply - (reverse part))))))))

(defcheck solution-773de720
  (fn read-roman-numerals [roman-numeral-string]
    (let [value {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          subtractive {\I #{\V \X} \X #{\L \C} \C #{\D \M}}
          parsed (->> (seq roman-numeral-string) (partition 2 1 nil))
          get-value (fn [[x y]] (let [v (value x)] (if (contains? (subtractive x) y) (- v) v)))]
      (->> (map get-value parsed) (reduce +)))))

(defcheck solution-777c8e41
  (fn [n] (->> n
            (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]))
            (partition 2 1 [0])
            (map (fn [[a b]] (if (< a b) (- a) a)))
            (apply +)
            )))

(defcheck solution-7819d148
  (fn [x]
    (letfn [(eat-ones [f c1 s]
              (cond
                (empty? f) (list s f)
                (= (first f) c1) (recur (rest f) c1 (inc s))
                :else (list s f)
                ))
            (process-any [l multiplier c1 c5 c10 next]
              #_(print (list l multiplier c1 c5 c10 next))
              (cond
                (empty? l) 0
                (= (first l) c5) (let [[s f] (eat-ones (rest l) c1 5)]
                                   (+ (* s multiplier) (next f))
                                   )
                (= (first l) c1) (let [[s f]
                                       (cond (or (empty? (rest l))  ;;1~3
                                                 (= (first (rest l)) c1)) (eat-ones l c1 0)
                                             (= (first (rest l)) c5)  (list 4 (rest (rest l)))
                                             (= (first (rest l)) c10) (list 9 (rest (rest l)))
                                             :else                    (eat-ones l c1 0))]
                                   (+ (* s multiplier) (next f)))
                :else (next l)))
            (process-thousand [a]
              (process-any a 1000 \M \v \x process-hundred))
            (process-hundred [a]
              (process-any a 100 \C \D \M process-ten))
            (process-ten [a]
              (process-any a 10 \X \L \C process-one))
            (process-one [a]
              (process-any a 1 \I \V \X (fn [_] 0)))]

      (process-thousand x))))

(defcheck solution-783b8ba6
  (fn [roman]
    (loop [v 0
           s (->> (seq roman)
               (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})
               (partition-by identity)
               (map (partial apply +)))]
      (if (< (count s) 2)
        (apply + v s)
        (let [s0 (first s) s1 (second s)]
          (recur
            (if (< s0 s1) v (+ v s0))
            (cons (if (< s0 s1) (- s1 s0) s1) (drop 2 s))))))))

(defcheck solution-783c38c4
  (fn roman [s]
    (let [z   {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          zd  {"IV" "IIII", "IX" "VIIII" , "XL" "XXXX" , "XC" "LXXXX" , "CD" "CCCC", "CM" "DCCCC"}]

      (reduce #(+ %1 (z %2)) 0
        (reduce-kv #(clojure.string/replace %1 %2 %3) s zd)))))

(defcheck solution-7848f6d
  (fn [s]
    (let [values {\M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
      (loop [[x & xs] (vec s) total 0]
        (if-not x
          total
          (let [value (values x)
                value (if (some #(> (values %) value) xs)
                        (* value -1)
                        value)]
            (recur xs (+ total value))))))))

(defcheck solution-78a697fe
  (fn to-arabic [romstr]
    (let [comprom (fn [n m]
                    (let [order "IVXLCDM"]
                      (apply - (map #(.indexOf order (str %)) [n m]))))
          rom-map (zipmap
                    "IVXLCDM"
                    (map #(int (if (even? %)
                                 (Math/pow 10 (/ % 2))
                                 (* 5 (Math/pow 10 (/ (dec %) 2))))) (range 7)))
          fst (first romstr)
          snd (second romstr)
          [fstv sndv] (map #(get rom-map %) [fst snd])]
      (if (nil? fst) 0
                     (apply + (if (and snd (neg? (comprom fst snd)))
                                [(- sndv fstv) (to-arabic (drop 2 romstr))]
                                [fstv (to-arabic (rest romstr))]))))))

(defcheck solution-78c8a0b4
  (fn [test]
    (let [rnv {\I 1, \V 5, \X 10
    , \L 50, \C 100
                  , \D 500, \M 1000}]
      (loop [[r & [rn & _] :as rns] test
             an 0]
        #_(println an r (rnv r) rn)
        (cond
          (empty? rns) an

          (nil? rn) (+ an (rnv r))

          (>= (rnv r) (rnv rn))
          (recur (rest rns) (+ an (rnv r)))

          :default
          (recur (nthrest rns 2) (+ an (- (rnv rn) (rnv r)))))))))

(defcheck solution-7acd8a7b
  (fn roman->num
    ([s]
     (roman->num 0 s))
    ([carry s]
     (let [roman { "MMM" 3000, "MM" 2000, "M" 1000,
                  "CM" 900, "DCCC" 800, "DCC" 700, "DC" 600, "D" 500, "CD" 400, "CCC" 300, "CC" 200, "C" 100,
                  "XC"  90, "LXXX " 80, "LXX"  70, "LX"  60, "L"  50, "XL"  40, "XXX"  30, "XX"  20, "X"  10,
                  "IX"   9, "VIII"   8, "VII"   7, "VI"   6, "V"   5, "IV"   4, "III"   3, "II"   2, "I"   1}]
       (if-let [leftmost (->> (keys roman)
                           (filter #(= 0 (.indexOf s %)))
                           (sort-by roman >)
                           first)]
         (recur (+ carry (roman leftmost)) (subs s (count leftmost)))
         carry)))))

(defcheck solution-7aee6a15
  (fn [s]
    (let [num-vals {"M" 1000, "CM" 900, "D" 500, "CD" 400, "C" 100, "XC" 90,
                    "L" 50, "XL" 40, "X" 10, "IX" 9, "V" 5, "IV" 4, "I" 1}
          nums ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          pat (re-pattern (clojure.string/join "|" nums))]
      (apply + (map num-vals (re-seq pat s)))
      )))

(defcheck solution-7af41453
  #(let [m (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
         [f & r] (reverse %)]
     ((reduce (fn [{:keys [s l]} e] {:s ((if (< (m e) (m l)) - +) s (m e))  :l e}) {:s (m f) :l f} r) :s)))

(defcheck solution-7b458f35
  (fn roman [s]
    (reduce +
      (let [vals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
            s1 (-> s (clojure.string/replace #"IV" "IIII")
                 (clojure.string/replace #"XL" "XXXX")
                 (clojure.string/replace #"CD" "CCCC")
                 (clojure.string/replace #"IX" "VIIII")
                 (clojure.string/replace #"XC" "LXXXX")
                 (clojure.string/replace #"CM" "DCCCC"))]
        (map (fn [c] (vals c)) (seq s1))))))

(defcheck solution-7b6e1e69
  get {"XIV" 14, "DCCCXXVII" 827, "MMMCMXCIX" 3999, "XLVIII" 48})

(defcheck solution-7be0596f
  (fn [w]
    (let [literals
          { \I 1
           \V 5
           \X 10
           \L 50
           \C 100
           \D 500
           \M 1000 }

          lit2int
          (fn [lit]
            (if (nil? lit) 0 (literals lit)))

          aux
          (fn [[head & tail] res tmp last]
            ;; (prn head tail res tmp last)
            (cond
              (nil? head)
              (+ res tmp)

              (<= (lit2int head) (lit2int last))
              (recur tail (+ res tmp) (lit2int head) head)

              :else
              (recur tail res (- (lit2int head) tmp) head)))]
      (aux w 0 0 nil))
    ))

(defcheck solution-7bf664e5
  (fn [s]
    (let [m {\M 1000 "CM" -100 \D 500 "CD" -100 \C 100 "XC" -10 \L 50 "XL" -10 \X 10 "IX" -1 \V 5 "IV" -1 \I 1}
          f #(apply str %)
          g #(or (m %) (m (first %)))]
      (reduce + (map g (map f (partition-all 2 1 s))))
      )))

(defcheck solution-7c18b2f8
  #(let [syms [[[\M] 1000] [[\C \M] 900] [[\D] 500] [[\C \D] 400] [[\C] 100] [[\X \C] 90]
               [[\L] 50] [[\X \L] 40] [[\X] 10] [[\I \X] 9] [[\V] 5] [[\I \V] 4] [[\I] 1]]
         r (fn [[n t]] (first (keep (fn [[s v]] (if (= s (take (count s) n))
                                                  [(drop (count s) n) (+ t v)])) syms)))]
     (->> (iterate r [% 0])
       (take-while (comp not nil?)) last last)))

(defcheck solution-7c93583c
  (fn [s]
    (let
     [
      numerals
      [
       ["M" 1000]
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
       ["I" 1]
       ]

      find-numeral
      (fn [s]
        (first
          (filter
            (fn [n]
              (not (zero? n))
              )
            (map
              (fn [[numeral-string value]]
                (if
                 (= numeral-string s)
                  value
                  0
                  )
                )
              numerals
              )
            )
          )
        )

      do-parse
      (fn do-parse [s]
        (let
         [
          first-two (str (first s) (second s))
          first-one (str (first s))

          value-first-two (find-numeral first-two)
          ]
          (cond
            (empty? s)
            0

            (not (nil? value-first-two))
            (+
              value-first-two
              (do-parse (nthrest s 2))
              )

            :else
            (+
              (find-numeral first-one)
              (do-parse (rest s))
              )
            )
          )
        )
      ]
      (do-parse s)
      )
    ))

(defcheck solution-7cbf90f3
  (fn [s]
    (let [m { \I 1
             \V 5
             \X 10
             \L 50
             \C 100
             \D 500
             \M 1000 }
          x (conj (vec (map m s)) 0)]
      (->> (partition 2 1 x)
        (map (fn [[a b]] (if (< a b) (- a) a)))
        (reduce +)))))

(defcheck solution-7ccc47ef
  (fn roman
    [string]
    (let [num-table {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          first-sym (first string)
          the-rest	(rest string)]
      (if (empty? string)
        0
        (let [adjustment
              (if the-rest
                (cond (and (= first-sym \I)
                           (or (= (first the-rest) \V) (= (first the-rest) \X)))
                      -2
                      (and (= first-sym \X)
                           (or (= (first the-rest) \L) (= (first the-rest) \C)))
                      -20
                      (and (= first-sym \C)
                           (or (= (first the-rest) \D) (= (first the-rest) \M)))
                      -200
                      :else 0)
                0)]
          (+ (num-table first-sym) (roman the-rest) adjustment))))))

(defcheck solution-7d3050f5
  (fn [s]
    (let [rr (zipmap "IVXLCDM"
               [1 5 10 50 100 500 1000])]
      (first (reduce (fn [ [s m] x]
                       [(+ s (if (< x m) (- x) x))
                        (max m x)]
                       ) [0,0] (reverse (map rr s)))))))

(defcheck solution-7d592beb
  (fn [s]
    (let [trans {:M 1000
                 :D 500
                 :C 100
                 :L 50
                 :X 10
                 :V 5
                 :I 1
                 :CM 900
                 :CD 400
                 :XC 90
                 :XL 40
                 :IX 9
                 :IV 4}]
      (reduce
        +
        (map
          #((keyword %) trans)
          (reduce (fn [a b]
                    (let [l (peek a)
                          c (str l b)]
                      (cond
                        (or (= "IV" c)
                            (= "IX" c)
                            (= "XL" c)
                            (= "XC" c)
                            (= "CM" c)
                            (= "CD" c)) (conj (pop a) c)
                        :else (conj a (str b))))) [] s))))))

(defcheck solution-7d66ec58
  (fn [ns]
    (let [rnx #"(?:IV|IX|XL|XC|CD|CM|I|V|X|L|C|D|M)"
          rns {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000
               "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}]
      (reduce + (map #(get rns %) (re-seq rnx ns))))))

(defcheck solution-7dc37430
  #(loop [n 0 s % [[c u] & cus] [
                                 ["M" 1000] ["M" 1000] ["M" 1000]
                                 ["CM" 900] ["D"  500] ["CD" 400] ["C"  100] ["C"  100] ["C"  100]
                                 ["XC" 90]  ["L"  50] ["XL" 40] ["X"  10] ["X"  10] ["X"  10]
                                 ["IX" 9] ["V"  5] ["IV" 4] ["I"  1] ["I"  1] ["I"  1]]]
     (if (empty? s) n
                    (if (= c (apply str (take (count c) s)))
                      (recur (+ n u) (drop (count c) s) cus)
                      (recur n s cus)))))

(defcheck solution-7df6017f
  (fn roman-to-arabic [n]
    (let [roman {\I   1
                 \V   5
                 \X   10
                 \L   50
                 \C   100
                 \D   500
                 \M   1000}
          rks (sort (keys roman))]
      (loop [n n
             r 0]
        (if (empty? n)
          r
          (let [x (get roman (last n))
                n (drop-last n)
                y (get roman (last n))]
            (if (or (nil? y) (>= y x))
              (recur n (+ r x))
              (recur (drop-last n) (+ r (- x y))))))))))

(defcheck solution-7e07efc2
  (fn [s]
    (let [n {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          f (fn [[x y]] ((if (< x y) - +) x))]
      (reduce #(+ % (f %2))
        (n (last s))
        (partition 2 1 (map n s))))))

(defcheck solution-7e3f8d8f
  (fn [s]
    (let [ re #"M|CM|D|CD|C|XC|L|XL|X|IX|V|IV|I"
          m {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
          ]
      (reduce + (map #(m %) (re-seq re s))))))

(defcheck solution-7e88b710
  (fn read-roman [numerals]
    (let [value-of {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (loop [[a b & more] numerals
             total 0]
        (if (nil? b) (+ total (value-of a))
                     (let [op (if (> (value-of b) (value-of a)) - +)]
                       (recur (cons b more) (op total (value-of a)))))))))

(defcheck solution-7eb049
  (fn [s]
    (let [r {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          rev (fn [regex k] (* k (count (re-seq regex s))))]
      (- (reduce + (map r s))
         (rev #"I[VX]" 2)
         (rev #"X[LC]" 20)
         (rev #"C[DM]" 200)))))

(defcheck solution-7f7a2672
  (fn [xs]
    (let [rommap2 {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (letfn [(romval2 [[a b]]
                (let [{v1 a} rommap2
                      {v2 b} rommap2]
                  (cond (nil? v2) v1
                        (> v2 v1) (- v2 v1)
                        :else v1)))
              (vallist [ys] (map romval2 (partition-all 2 1 ys)))
              (adduproman2 [zs]
                (cond (empty? zs) 0
                      (empty? (rest zs)) (first zs)
                      (> (second zs) (first zs)) (+ (first zs) (adduproman2 (nnext zs)))
                      :else (+ (first zs) (adduproman2 (next zs)))))]
        (adduproman2 (vallist xs))))))

(defcheck solution-7fac7759
  (fn [n]
    (letfn [(thousands [num] (* 1000 (count (take-while #(= \M %) num))))
            (without-thousands [num] (drop-while #(= \M %) num))
            (five-hundreds [num] (if (= \D (first (without-thousands num))) 500 0))
            (without-500s [num] (drop-while #(= \D %) (without-thousands num)))
            (hundreds [num]
              (let [numw (without-500s num)]
                (cond
                  (and (= \C (first numw)) (= \D (nth numw 1))) 400
                  (and (= \C (first numw)) (= \M (nth numw 1))) 900
                  :else (* 100 (count (take-while #(= \C %) numw))))))
            (without-hundreds [num]
              (drop-while #(or (= \C %) (= \D %) (= \M %)) (without-500s num)))
            (fifties [num] (if (= \L (first (without-hundreds num))) 50 0))
            (without-50s [num] (drop-while #(= \L %) (without-hundreds num)))
            (tens [num]
              (let [numw (without-50s num)]
                (cond
                  (and (= \X (first numw)) (= \L (nth numw 1))) 40
                  (and (= \X (first numw)) (= \C (nth numw 1))) 90
                  :else (* 10 (count (take-while #(= \X %) numw))))))
            (without-tens [num] (drop-while #(or (= \X %) (= \L %) (= \C %)) (without-50s num)))
            (fives [num] (if (= \V (first (without-tens num))) 5 0))
            (without-fives [num] (drop-while #(= \V %) (without-tens num)))
            (ones [num]
              (let [numw (without-fives num)]
                (cond
                  (and (= \I (first numw)) (= \V (nth numw 1))) 4
                  (and (= \I (first numw)) (= \X (nth numw 1))) 9
                  :else (* (count (take-while #(= \I %) numw))))))
            (roman-conv [num] (+ (thousands num) (five-hundreds num) (hundreds num) (tens num) (fives num) (ones num)))]
      (roman-conv n))))

(defcheck solution-7fdb6bf1
  (fn [S]
    (let [values {\I 1 \X 10 \V 5 \L 50 \C 100 \D 500 \M 1000 }]
      (loop [s S T 0]
        (cond (empty? s) T
              (and (< 1 (count s))(< (values (first s)) (values (second s)) )) (recur (rest (rest s)) (+ T (- (values (second s)) (values (first s)))))
              :else (recur (rest s) (+ T (values (first s))))

              )))))

(defcheck solution-7fe63608
  (fn [str]
    (let [vm {\I 1, \V 5,\X 10,\L 50,\C 100,\D 500,\M 1000}]
      (reduce #(if (>= (first %2) (second %2))
                 (+ %1 (first %2))
                 (+ %1 (- (second %2) (first %2)) (- (second %2))))
        0
        (partition 2 1 '(0)
          (map #(get vm %) str))))))

(defcheck solution-805631f3
  (fn [s]
    (let [vals {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (loop [s (seq s)
             prev-dig 0
             digs []]
        (cond (nil? s) (reduce + digs)
              :else (let [dig (get vals (first s))]
                      (recur (next s)
                        dig
                        (conj digs (cond (= dig prev-dig) dig
                                         (> dig prev-dig) (+ dig (- (* prev-dig 2)))
                                         (< dig prev-dig) dig)))))))))

(defcheck solution-807852cf
  #(let [nums (map {\I 1
                    \V 5
                    \X 10
                    \L 50
                    \C 100
                    \D 500
                    \M 1000} %)]
     (apply + (map (fn [[c n]] (if (< c n) (- c) c)) (partition 2 1 (concat nums (list (last nums))))))))

(defcheck solution-808203a0
  (fn read-roman-nums [rn-str]
    (let [rn->num {\I 1 \V 5 \X 10 \L 50
                   \C 100 \D 500 \M 1000}
          rn-nums (map rn->num (reverse rn-str))]
      (loop [sum 0
             [f s & r] rn-nums]
        (if (nil? s)
          (if (nil? f) sum (+ sum f))
          (if (< s f)
            (recur (+ sum (- f s)) r)
            (recur (+ sum f) (conj r s))))))))

(defcheck solution-81b8b369
  (fn [s]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (- (apply + (map m s))
         (+ (if (re-find #"IV" s) 2 0)
           (if (re-find #"IX" s) 2 0)
           (if (re-find #"XL" s) 20 0)
           (if (re-find #"XC" s) 20 0)
           (if (re-find #"CD" s) 200 0)
           (if (re-find #"CM" s) 200 0))))))

(defcheck solution-821ce856
  (fn roman [s]
    (let [letter-val {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          reducer (fn [[running level :as r] l]
                    (if (< l level)
                      [(- running l) level]
                      [(+ running l) l]))]
      (->> s
        reverse
        seq
        (map (partial get letter-val))
        (reduce reducer [0 0])
        first
        ))))

(defcheck solution-82d5c2fb
  (fn [rn]
    (let [v-map {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (loop [acc 0 [v & next-vs] (map v-map rn)]
        (if (seq next-vs)
          (recur (+ acc
                   (* v
                     (if (= 0 (count (filter #(< v %) next-vs))) 1 -1)))
            next-vs)
          (+ acc v))))))

(defcheck solution-82d88661
  #(first
     (reduce
       (fn [[a p] x]
         [((if (< x p) - +) a x) x])
       [0 0]
       (reverse (map {\I 1  \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %)))))

(defcheck solution-839b6383
  (let [sub {"IV" "IIII", "IX" "VIIII", "XL" "XXXX", "XC" "LXXXX", "CD" "CCCC", "CM" "DCCCC"}
        value {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (fn [s]
      (reduce #(+ %1 (value %2)) 0 (reduce (fn [s [a b]] (clojure.string/replace s a b)) s sub)))))

(defcheck solution-83cac62f
  #(let [nums {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (reduce + (map (fn [digit next]
                      (let [v (nums digit)
                            n (get nums next -1)]
                        (if (>= v n) v (- v)))) % (concat (rest %) [nil])))))

(defcheck solution-844eb546
  #(let [m1 {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
         m2 {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [s % sum 0]
       (if (seq s)
         (if-let [x (m1 (apply str (take 2 s)))]
           (recur (drop 2 s) (+ sum x))
           (recur (rest s) (+ sum (m2 (first s)))))
         sum))))

(defcheck solution-84dc87dc
  #(nth
     (reduce
       (fn [[r i] e]
         [(+ r (if (> e i) (- e i i) e))
          e])
       [0 1000]
       (map {\X 10 \I 1 \V 5 \L 50 \D 500 \C 100 \M 1000} %))
     0))

(defcheck solution-851a33ee
  (fn [roman-number]
    (->> (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (seq roman-number))
      (partition-by identity)
      (map (partial reduce +))
      (reverse)
      (partition 2 2 '(0))
      (map #(if (< (last %) (first %))
              (- (first %) (last %))
              (reduce + %)))
      (reduce +)
      )))

(defcheck solution-85747923
  (fn roman-number [s]
    (let [char-value      {\I 1
                           \V 5
                           \X 10
                           \L 50
                           \C 100
                           \D 500
                           \M 1000}
          subtractive-sum (fn [sum prefix c] (+ (- sum (* 2 (char-value prefix))) (char-value c)))]
      ((reduce
         (fn [[sum prev-c] c]
           (cond
             (and (#{\V \X} c) (= prev-c \I))   [(subtractive-sum sum \I c) nil]
             (and (#{\L \C} c) (= prev-c \X))   [(subtractive-sum sum \X c) nil]
             (and (#{\D \M} c) (= prev-c \C))   [(subtractive-sum sum \C c) nil]
             :else                              [(+ sum (char-value c))     c]
             ))
         [0 nil]
         s) 0))))

(defcheck solution-86778db8
  (fn read-roman [r]
    (let [value-map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          nums (map value-map r)]
      (first (reduce (fn [[sum prev op] new-val]
                       (cond (> new-val prev) [(+ sum new-val) new-val +]
                             (< new-val prev) [(- sum new-val) new-val -]
                             :else [(op sum new-val) new-val op])
                       ) [0 0 +] (reverse nums)))
      )
    ))

(defcheck solution-86c949e8
  (fn [x]
    (let [rom-map { 1000  "M",
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
                   1  "I"}
          rom-vals (sort-by count > (vals rom-map))
          rom-map-rev (clojure.set/map-invert rom-map)]
      (loop [res 0
             rst x]
        (if (= rst "")
          res
          (let [add (some #(re-find (re-pattern (str "^" %)) rst) rom-vals)]
            (recur (+ res (rom-map-rev add))
              (apply str (drop (count (str add)) (seq rst))))))))))

(defcheck solution-86cd8973
  (fn [rn]
    (let [v [[#"IV" "1"] [#"IX" "2"] [#"XL" "3"] [#"XC" "4"] [#"CD" "5"] [#"CM" "6"]]
          m {"1" 4 "2" 9 "3" 40 "4" 90 "5" 400 "6" 900 "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (apply + (map #(m (str %)) (reduce (fn [a [o n]] (clojure.string/replace a o n)) rn v))))))

(defcheck solution-875277e3
  (fn [s]
    (let [pn
          (fn [[s acc] pat incr]
            (loop [s s, acc acc]
              (if-not (re-find pat s)
                [s acc]
                (recur (clojure.string/replace-first s pat "") (+ acc incr)))))]
      (-> s
        (vector 0)
        (pn #"CM" 900)
        (pn #"CD" 400)
        (pn #"XC" 90)
        (pn #"XL" 40)
        (pn #"IX" 9)
        (pn #"IV" 4)
        (pn #"M" 1000)
        (pn #"D" 500)
        (pn #"C" 100)
        (pn #"L" 50)
        (pn #"X" 10)
        (pn #"V" 5)
        (pn #"I" 1)
        last))))

(defcheck solution-8780ed4
  (fn [s]
    (let [roman {"M" 1000 "CM" 900 "D" 500 "CD" 400
                 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9
                 "V" 5 "IV" 4 "I" 1}]
      (reduce + (map roman
                  (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s))))))

(defcheck solution-87872509
  (fn [roman]
    (let [convert
               (fn [l]
                 (condp = l
                   \I 1
                   \V 5
                   \X 10
                   \L 50
                   \C 100
                   \D 500
                   \M 1000))
          nums (map convert roman)]
      (loop [nums nums total 0 register 0 lastnum 0]
        (if (empty? nums)
          (+ total register)
          (if (or (= register 0) (= lastnum (first nums)))
            (recur (rest nums) total (+ register (first nums)) (first nums))
            (if (> register (first nums))
              (recur (rest nums) (+ total register) (first nums) (first nums))
              (recur (rest nums) (+ total (- (first nums) register)) 0 (first nums)))))))))

(defcheck solution-882045b4
  (fn [string]
    ((fn helper [sub-string ans]
       (if (empty? sub-string)
         ans
         (let [sub-2 (if (> (count sub-string) 1) (subs sub-string 0 2) nil)
               sub-1 (subs sub-string 0 1)]
           (if (= "CM" sub-2)
             (helper (subs sub-string 2) (+ ans 900))
             (if (= "CD" sub-2)
               (helper (subs sub-string 2) (+ ans 400))
               (if (= "XC" sub-2)
                 (helper (subs sub-string 2) (+ ans 90))
                 (if (= "XL" sub-2)
                   (helper (subs sub-string 2) (+ ans 40))
                   (if (= "IX" sub-2)
                     (helper (subs sub-string 2) (+ ans 9))
                     (if (= "IV" sub-2)
                       (helper (subs sub-string 2) (+ ans 4))
                       (if (= "M" sub-1)
                         (helper (subs sub-string 1) (+ ans 1000))
                         (if (= "D" sub-1)
                           (helper (subs sub-string 1) (+ ans 500))
                           (if (= "C" sub-1)
                             (helper (subs sub-string 1) (+ ans 100))
                             (if (= "L" sub-1)
                               (helper (subs sub-string 1) (+ ans 50))
                               (if (= "X" sub-1)
                                 (helper (subs sub-string 1) (+ ans 10))
                                 (if (= "V" sub-1)
                                   (helper (subs sub-string 1) (+ ans 5))
                                   (if (= "I" sub-1)
                                     (helper (subs sub-string 1) (+ ans 1)))))))))))))))))) string 0)))

(defcheck solution-88515248
  (fn [letters]
    (let [roms {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [[l & rst] letters
             prev (apply max (vals roms))
             total 0]
        (if l
          (let [curr (roms l)
                subtotal (+ total curr)]
            (if (> curr prev)
              (recur rst curr (- subtotal (* 2 prev)))
              (recur rst curr subtotal)))
          total)))))

(defcheck solution-88ae9caf
  #(apply + (map {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
              (re-seq #"IV|IX|XL|XC|CD|CM|M|D|C|L|X|V|I" %))))

(defcheck solution-89ba8e68
  (fn roman-numbers
    ([x]
     (roman-numbers x 0))
    ([y z]
     (let [dictionary {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "CD" 400 "D" 500 "CM" 900 "M" 1000}
           addNum (fn [w q]
                    (+ q (get dictionary w)))
           requires-two? (fn [t]
                           (if (contains? #{"IV" "IX" "XL" "XC" "CD" "CM"} t)
                             true
                             false))]
       (if (= 0 (count y))
         z
         (if (and (> (count y) 1) (requires-two? (subs y 0 2)))
           (recur (subs y 2) (addNum (str (get y 0) (get y 1)) z))
           (recur (subs y 1) (addNum (str (get y 0)) z))))))))

(defcheck solution-89db710a
  (fn [rom-num]
    (let [rom-num-vals
          {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          val-next-val-pairs
          (partition 2 1 '(0) (map rom-num-vals rom-num))
          val-given-next-val
          (fn [[val next-val]] (if (< val next-val) (- val) val))]
      (reduce + (map val-given-next-val val-next-val-pairs)))))

(defcheck solution-8a2f4870
  (fn parse-roman-numeral [string]
    (let [numerals-map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          num-pairs (partition 2 1 (concat (map numerals-map string) '(0)))]
      (letfn [(accum-num-pair [sum [a b]]
                (if (>= a b)
                  (+ sum a)
                  (- sum a)))]
        (reduce accum-num-pair 0 num-pairs)))))

(defcheck solution-8a8ed430
  (fn [s]
    (->
      s
      (clojure.string/replace #"IV" "IIII")
      (clojure.string/replace #"IX" "VIIII")
      (clojure.string/replace #"XL" "XXXX")
      (clojure.string/replace #"XC" "LXXXX")
      (clojure.string/replace #"CD" "CCCC")
      (clojure.string/replace #"CM" "DCCCC")
      ((partial reduce #(cond
                          (= \I %2) (+ %1 1)
                          (= \V %2) (+ %1 5)
                          (= \X %2) (+ %1 10)
                          (= \L %2) (+ %1 50)
                          (= \C %2) (+ %1 100)
                          (= \D %2) (+ %1 500)
                          (= \M %2) (+ %1 1000)
                          :else %1)
         0)))
    ))

(defcheck solution-8aac1206
  (fn [r]
    (loop [s (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} r)
           acc 0]
      (if (seq s)
        (recur (rest s) (+ acc (if (>= (first s) (apply max s)) (first s) (- (first s)))))
        acc))))

(defcheck solution-8aceeae
  (fn read-roman [st]
    (let [roman {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (last (reduce
              (fn [[l s] n]
                (if(> l n)
                  (vector n (- s n))
                  (vector n (+ s n)))) [0 0]
              (map #(roman %) (reverse st)))))))

(defcheck solution-8afa16ad
  (fn f [s]
    (first
      (->> s
        (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})
        (partition-by identity)
        reverse
        (reduce (fn [[acc last] [n :as x]]
                  [(apply (if (> last n) - +) acc x) n])
          [0 0])))))

(defcheck solution-8b009744
  #(:c (reduce (fn [a c]
                 (if (> (:l a) c)
                   {:l (:l a) :c (- (:c a) c)}
                   {:l c :c (+ (:c a) c)}))
         {:l 0 :c 0}
         (map {\D 500 \C 100 \I 1 \L 50 \M 1000 \V 5 \X 10} (reverse %)))))

(defcheck solution-8b9d10bd
  #((fn f [s]
      (let [h {\M 1000
               \D 500
               \C 100
               \L 50
               \X 10
               \V 5
               \I 1}
            a (h (first s))
            b (h (second s))]
        (if b
          ((if (< a b) - +) (f (rest s)) a)
          a
          ))) (vec %)))

(defcheck solution-8ba5ccdc
  (fn [x]
    (loop [res 0
           s-x (seq x)]
      (if (empty? s-x)
        res
        (let [fst-char (apply str (take 1 s-x))
              fst-2-chars (apply str (take 2 s-x))]
          (cond
            (= fst-char "M") (recur (+ res 1000) (drop 1 s-x))
            (= fst-2-chars "CM") (recur (+ res 900) (drop 2 s-x))
            (= fst-char "D") (recur (+ res 500) (drop 1 s-x))
            (= fst-2-chars "CD") (recur (+ res 400) (drop 2 s-x))
            (= fst-char "C") (recur (+ res 100) (drop 1 s-x))
            (= fst-2-chars "XC") (recur (+ res 90) (drop 2 s-x))
            (= fst-char "L") (recur (+ res 50) (drop 1 s-x))
            (= fst-2-chars "XL") (recur (+ res 40) (drop 2 s-x))
            (= fst-char "X") (recur (+ res 10) (drop 1 s-x))
            (= fst-2-chars "IX") (recur (+ res 9) (drop 2 s-x))
            (= fst-char "V") (recur (+ res 5) (drop 1 s-x))
            (= fst-2-chars "IV") (recur (+ res 4) (drop 2 s-x))
            (= fst-char "I") (recur (+ res 1) (drop 1 s-x))))))))

(defcheck solution-8bb3d430
  (fn [rn]
    (let [d (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} rn) ]
      (loop [ [a & b] d, s 0]
        (if (empty? b) (+ s a)
                       (recur b (if (< a (first b)) (- s a) (+ s a))))))))

(defcheck solution-8befe0ae
  (fn [s]
    (let [roman-value  { \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 }]
      ((fn
         [ r [a b & xs ] ]
         (if (nil? b)
           (+ r a)
           (recur ((if (< a b) - +) r a) (cons b xs))))
       0 (map roman-value s)))))

(defcheck solution-8c0add44
  (fn rrn [roman-numeral]
    (let [digits (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} (take 2 roman-numeral))]
      (cond (empty? digits) 0
            (and (= 2 (count digits))
                 (< (first digits) (second digits))) (- (rrn (rest roman-numeral)) (first digits))
            :else (+ (rrn (rest roman-numeral)) (first digits))))))

(defcheck solution-8c2b51ff
  (fn roman [s]
    (letfn [(calc [[x & [y & r :as s]]]
              (cond
                (nil? x) 0
                (nil? y) x
                (>= x y) (+ x (calc s))
                :else (+ (- y x) (calc r))))]
      (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
        (calc (map m s))))))

(defcheck solution-8c41c323
  (fn [s]
    (let [chars
          {\S 0
           \I 1
           \V 5
           \X 10
           \L 50
           \C 100
           \D 500
           \M 1000}]

      (first
        (reduce
          (fn [[curr buf] v]
            (if (nil? buf)
              [curr v]
              (if (< buf v)
                [(+ curr (- v buf)) nil]
                [(+ curr buf) v])))
          [0 nil]
          (map chars (str s "S")))))))

(defcheck solution-8c791ba5
  (fn roman [s]
    (let [char-map (hash-map \I 1
                     \V 5
                     \X 10
                     \L 50
                     \C 100
                     \D 500
                     \M 1000)
          char-to-value #(get char-map % 0)
          ichar-of? (fn [x y]
                      (let [ichar-map (hash-map \X \I
                                        \V \I
                                        \C \X
                                        \L \X
                                        \D \C
                                        \M \C)]
                        (= (get ichar-map y) x)))
          to-components (fn [s]
                          (loop [x (first s)
                                 s (rest s)
                                 c []
                                 cs []
                                 ]
                            #_(prn x s c cs)
                            (if (= x nil)
                              (conj cs c)
                              (let [x' (char-to-value x)
                                    y (last c)
                                    y'(char-to-value y)]
                                (if (and
                                     (not= y nil)
                                     (not= x' y')
                                     (not (ichar-of? y x)))
                                  (recur  x s []  (conj cs c))
                                  (recur  (first s) (rest s) (conj c x) cs))))))
          component-to-value (fn [c]
                               (cond
                                 (= (count c) 1) (char-to-value (first c))
                                 (not= (first c) (last c)) (- (char-to-value (last c))
                                                              (reduce + (map char-to-value (drop-last 1 c))))
                                 :else (reduce + (map char-to-value c))))]
      (reduce + (map component-to-value (to-components s))))))

(defcheck solution-8ca47702
  (fn roman [x]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          s {[\C \M] 900 [\C \D] 400 [\X \C] 90 [\X \L] 40 [\I \X] 9 [\I \V] 4}]
      (loop [q (partition-all 2 1 x)
             r 0]
        (if (empty? q)
          r
          (if-let [t (get s (first q))]
            (recur (rest (rest q)) (+ r t))
            (recur (rest q) (+ r (get m (first (first q)))))))))))

(defcheck solution-8d050aa7
  (fn p92 [s]
    (let [value {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (letfn [(evaluate [[c1 c2]]
                (if (> c2 c1) (- c1) c1))]
        (reduce + (map evaluate (map list (map value s) (concat (map value (rest s)) [0]))))))))

(defcheck solution-8d176f77
  (fn __ [s]
    (let [m [[\M 1000][\D 500][\C 100][\L 50][\X 10][\V 5][\I 1]]]
      (letfn [(f [s n]
                (if (empty? s) n
                               (let [p (first (filter #(= (first s) (first %)) m))
                                     i (.indexOf m p)
                                     c (first p)
                                     x (second p)]
                                 (if (or (= c \M)
                                         (odd? i))
                                   (f (rest s) (+ n x))
                                   (let [s (rest s), i (dec i), p (nth m i), c (first p), y (second p)]
                                     (if (= (first s) c)
                                       (f (rest s) (- (+ n y) x))
                                       (let [i (dec i), p (nth m i), c (first p), y (second p)]
                                         (if (= (first s) c)
                                           (f (rest s) (- (+ n y) x))
                                           (f s (+ n x))))))))))]
        (f s 0)))))

(defcheck solution-8dd6cdbb
  (fn [s]
    (loop [output 0 s s
           rs [1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"]]
      (if (zero? (count s))
        output
        (let [[d r] (take 2 rs)]
          (if (zero? (.indexOf s r))
            (recur (+ output d) (apply str (drop (count r) s)) rs)
            (recur output s (drop 2 rs))))))))

(defcheck solution-8df8b9ef
  (fn rom [xs]
    (let [look (fn [n] ({\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1} n))]
      (cond
        (empty? xs) 0
        (= 1 (count xs)) (look (first xs))
        :else (let [[x y & more] xs
                    lx           (look x)
                    ly           (look y)]
                (if (< lx ly)
                  (+ (- ly lx) (rom more))
                  (+ lx (rom (cons y more)))))))))

(defcheck solution-8e33aa34
  (fn [s]
    (let [r (replace (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]) s)]
      (+ (reduce + (map #(if (>= %1 %2) %1 (- %1)) r (rest r))) (last r)))))

(defcheck solution-8e5e6cb5
  (fn [rm]
    (loop [[r & m] rm
           la 1001
           a 0]
      (if (nil? r)
        a
        (let [v ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} r)]
          (recur m v (+ a v (if (> v la) (- 0 la la) 0))))))))

(defcheck solution-8ed8d9b7
  #(let [order {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 nil 0}
         fold (fn fold [[x & [y & tail :as y-tail] :as col]]
                (let [xo (order x) yo (order y)]
                  (cond (empty? col) nil
                        (< xo yo) (cons (- yo xo) (fold tail))
                        :else (cons xo (fold y-tail)))))]
     (apply + (fold (seq %)))))

(defcheck solution-8f1b0962
  (fn read-roman [S]
    (let [roman {\I 1, \V 5, \X 10, \L 50,
                 \C 100, \D 500, \M 1000}]
      (loop [total 0, s S]
        (cond
          (empty? (rest s)) (+ total (roman (first s)))
          (< (roman (first s)) (roman (second s)))
          (recur (- total (roman (first s))) (rest s))
          :else
          (recur (+ total (roman (first s))) (rest s)))))))

(defcheck solution-8fa8fcf0
  (fn [s]
    (let [mapping {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (->> s (map mapping) reverse (reduce #(if (< % (dec (* 5 %2))) (+ % %2) (- % %2)))))))

(defcheck solution-9013e15e
  (fn [s]
    (let [
          conv {\M 1000  \D 500, \C 100 \L 50 \X 10 \V 5 \I 1}
          ]
      (reduce
        (fn [a v] (if (<= (first v) (second v)) (+ a (second v)) (- a (second v))))
        0
        (partition 2 1
          (conj (map conv (reverse (seq s))) 0))
        ))))

(defcheck solution-90444a4f
  #(let [dm {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [n 0 p (dm (first %)) [h & t] (rest %)]
       (if (nil? h)
         (+ n p)
         (let [c (dm h)]
           (if (and (pos? p) (< p c))
             (recur (+ n (- c p)) 0 t)
             (recur (+ n p) c t)))))))

(defcheck solution-904e62f1
  (fn [s]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          p (partition-all 2 1 s)]
      (reduce (fn [res [x y]] (if (and y (< (m x) (m y)))
                                (- res (m x)) (+ res (m x))))
        0 p))))

(defcheck solution-90ec0b4b
  #(loop [r 0 n (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]) %)]
     (let [a (first n) b (fnext n)]
       (cond
         (nil? a) r
         (nil? b) (+ r a)
         :else (if (< a b) (recur (+ r (- b a)) (drop 2 n))
                           (recur (+ r a)       (next n)))))))

(defcheck solution-90f04911
  (fn [s]
    (let [ns (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (str s \I))
          ns (map #(if (< % %2) (- %) %) ns (rest ns))]
      (reduce + ns))))

(defcheck solution-913d78ee
  (fn [x]
    (-
     (apply +
       (map
         {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
         (seq x)))
     (apply +
       (map
         #({"IV" 2 "IX" 2 "XL" 20 "XC" 20 "CD" 200 "CM" 200} % 0)
         (map #(apply str %) (partition 2 1 x)))))))

(defcheck solution-9143eca1
  #((fn c [[n & s]]
      (if n
        ((if (and s (< n (first s))) - +)
         (c s) n)
        0))
    (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} %)))

(defcheck solution-914b5b44
  (fn arabic
    ([roman] (arabic roman 0))
    ([roman acc]
     (let [base-numerals {\I 1
                          \V 5
                          \X 10
                          \L 50
                          \C 100
                          \D 500
                          \M 1000}
           subtractive-numerals {"IV" 4
                                 "IX" 9
                                 "XL" 40
                                 "XC" 90
                                 "CD" 400
                                 "CM" 900}]
       (if (subtractive-numerals (apply str (take 2 roman)))
         (recur (drop 2 roman)
           (+ acc (subtractive-numerals (apply str (take 2 roman)))))
         (if (base-numerals (first roman))
           (recur (next roman)
             (+ acc (base-numerals (first roman))))
           acc))))))

(defcheck solution-91716ab4
  (fn [s]
    (->> s
      reverse
      (replace (zipmap "MDCLXVI" [1000 500 100 50 10 5 1]))
      (partition-by identity)
      (map (partial apply +))
      (reduce #((if (< %1 %2) + -) %1 %2)))))

(defcheck solution-91f06f44
  (fn [s]
    (let [r->d {"C" 100, "CC" 200, "CCC" 300, "CD" 400, "CM" 900, "D" 500,
                "DC" 600, "DCC" 700, "DCCC" 800, "I" 1, "II" 2, "III" 3,
                "IV" 4, "IX" 9, "L" 50, "LX" 60, "LXX" 70, "LXXX" 80,
                "M" 1000, "MM" 2000, "MMM" 3000, "MMMM" 4000, "V" 5,
                "VI" 6, "VII" 7, "VIII" 8, "X" 10, "XC" 90, "XL" 40,
                "XX" 20, "XXX" 30}
          eager-matches (loop [start 0, end (count s), acc []]
                          (if (pos? (- end start))
                            (if-let [v (r->d (subs s start end))]
                              (recur end (count s) (conj acc v))
                              (recur start (dec end) acc))
                            acc))]
      (apply + eager-matches))))

(defcheck solution-9224d5f4
  (fn [ro]
    (let [rod  (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} ro)
          prod (partition-all 2 1 rod)
          op (fn [a b] (if (and b (< a b)) - +))]

      (reduce (fn [sum [a b]] ((op a b) sum a)) 0 prod))))

(defcheck solution-926e73c9
  #(let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 \Z 0}]
     (first (reduce (fn [[n l] c] [((if (< l (m c)) - +) n l) (m c)]) [0 0] (str % \Z)))))

(defcheck solution-92d9a544
  (fn from-roman [[a b & r]]
    (let [letters {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (cond
        (nil? a) 0
        (nil? b) (letters a)
        (< (letters a) (letters b)) (+ (- (letters b) (letters a)) (from-roman r))
        :else (+ (letters a) (from-roman (cons b r)))
        ))))

(defcheck solution-93321a36
  (fn parse-roman [n]
    (let [
          value-of { \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000 }
          to-stack (fn [acc block]
                     (let [letter (first block)
                           magnitude (value-of letter)
                           quantity (count block)
                           [prev-magnitude prev-quantity] (first acc)
                           sign (if (< magnitude prev-magnitude) -1 1)]
                       (cons [magnitude (* quantity sign)] acc)))
          sum (fn [stack]
                (reduce #(+ %1 (apply * %2)) 0 stack))
          ]
      (->> n
        (partition-by identity)
        (reverse)
        (reduce to-stack (list [0 0]))
        (sum)))))

(defcheck solution-93e85078
  (let [roman-table
        {"M" 1000
         "D"  500
         "C"  100
         "L"   50
         "X"   10
         "V"    5
         "I"    1}]
    (let [roman-seq
          (fn [numeral]
            (map roman-table (map str (into [] numeral))))
          roman-calc
          (fn [seq]
            (second
              (reduce (fn [[prev accum] next]
                        (if (> next prev)
                          [next (- (+ accum next) prev prev)]
                          [next (+ accum next)]))
                [1000 0]
                seq)))]
      (fn read-roman-numeral
        [numeral]
        (roman-calc (roman-seq numeral))))))

(defcheck solution-93f36a94
  (fn [st]
    (let [numers {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (letfn [(add-num [n s]
                (if (empty? s) (list (list n))
                               (if (> n (ffirst s))
                                 (cons (cons n (first s)) (rest s))
                                 (cons (first s) (add-num n (rest s))))))
              (long-inc-sub-seq [xs s]
                (if (empty? xs) s
                                (long-inc-sub-seq (rest xs) (add-num (first xs) s))))]
        (reduce + (map #(+ (first %) (apply - (cons 0 (rest %))))
                    (long-inc-sub-seq (map numers st) '())
                    ))
        ))))

(defcheck solution-93f65321
  (fn [s]
    (let [s (seq s)
          romans {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          nums (map #(get romans %) s)]
      (loop [[n1 & ns] (partition-all 2 1 (reverse nums)) cmax 0 rslt []]
        (if (nil? n1) (reduce + rslt)
                      (let [[a b] n1]
                        (if (> cmax a)
                          (recur ns cmax (conj rslt (- a)))
                          (recur ns a (conj rslt a)))))))))

(defcheck solution-9482ac7e
  (fn [letters]
    (let [order (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
          cmp   (fn [a b] (compare (get order a) (get order b)))
          magn  (fn [[letter :as g]] (* (get order letter) (count g)))]
      (loop [[f s :as xs] (partition-by identity letters)
             acc 0]
        (if-not s
          (+ (magn f) acc)
          (recur (rest xs)
            (-> (magn f)
              (* (cmp (first f) (first s)))
              (+ acc))))))))

(defcheck solution-94c59863
  (fn [n]
    (let [m {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}]
      (->>
        (reduce
          #(let [a (str (last %) %2)]
             (if (m a)
               (conj (vec (drop-last %)) a)
               (conj % (str %2))))
          '()
          n)
        (map m)
        (apply +)
        ))))

(defcheck solution-95345ec3
  (fn calc
    ([s] (calc 0 (seq s)))
    ([sum s]
     (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
           cur (nums (first s))
           more (next s)
           nex (nums (first more))]
       (cond
         (nil? nex) (+ sum cur)
         (< cur nex) (calc (- sum cur) more)
         :default (calc (+ sum cur) more))))))

(defcheck solution-95872532
  (fn deromanize [s]
    (let [
          numerals (array-map
                     "M" 1000
                     "CM" 900
                     "D"  500
                     "CD" 400
                     "C"  100
                     "XC"  90
                     "L"   50
                     "XL"  40
                     "X"   10
                     "IX"   9
                     "V"    5
                     "IV"   4
                     "I"   1)
          part (some #(when (.startsWith s %) %) (keys numerals))
          num (numerals part)
          s' (subs s (count part))
          ]
      (if (> (count s') 0) (+ num (deromanize s')) num)
      )))

(defcheck solution-960293b6
  #(let[romanNumMap {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]; convient lookup map for roman characters
     (loop[result 0, preVal 0, remaining (reverse %)]
       (if-let [currentVal (romanNumMap (first remaining))]
         (if (< currentVal preVal)
           (recur (- result currentVal) currentVal (rest remaining)) ;for sth like IV, IIX
           (recur (+ result currentVal) currentVal (rest remaining))) ;for sth like VII
         result))))

(defcheck solution-96044a3c
  (fn read-roman
    [s]
    (let [numeral-val {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (if (empty? s)
        0
        (let [[numeral & more] s
              x (numeral-val numeral)
              sign (if (some #(> % x) (map numeral-val more))
                     -1 1)]
          (+ (* sign x) (read-roman more)))))))

(defcheck solution-969cc2cb
  (fn read-roman [s]
    (let [value-for {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1
                     "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}]
      (if-let [s (seq s)]
        (if-let [n (value-for (apply str (take 2 s)))]
          (+ n (read-roman (drop 2 s)))
          (+ (value-for (first s)) (read-roman (drop 1 s))))
        0))))

(defcheck solution-9793f3e5
  (fn [s]
    (let [rn {"CM" 900, "CD" 400, "XC" 90, "XL" 40, "IX" 9, "IV" 4, "M" 1000, "D" 500, "C" 100, "L" 50, "X" 10, "V" 5, "I" 1}]
      (loop [ss s dn 0]
        (if (empty? ss)
          dn
          (if (get rn (apply str (first (partition 2 ss))))
            (recur (drop 2 ss) (+ dn (get rn (apply str (first (partition 2 ss))))))
            (recur (rest ss) (+ dn (get rn (str (first ss)))))))))))

(defcheck solution-97ab2e6f
  (fn f [a] (if (empty? a)
              0
              (let [d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} [h & r] a b (f r)]
                (if (some #(= [h (first r)] %) [[\I \V] [\I \X] [\X \L] [\X \C] [\C \D] [\C \M]]) (- b (d h)) (+ b (d h)))))))

(defcheck solution-97b7ced3
  (let [res { #"M" 1000, #"D" 500, #"C" 100, #"L" 50, #"X" 10, #"V" 5, #"I" 1,
             #"I[VX]" -2, #"X[LC]" -20, #"C[MD]" -200}]
    (fn [s]
      (reduce + (for [[r v] res] (* v (count (re-seq r s))))))))

(defcheck solution-982897b4
  (fn [w]
    (let [ z [0 0 \Z] f first s second m {  \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          u (into [] (map #(vector (count %) (get m (f %)) (f %)) (partition-by identity (seq w))))
          a (cons z u)  b (conj u z) ]
      (reduce + (map #(cond (< (s %) (s %2)) (* -1 (f %) (s %))
                            :else (*  (f %) (s %))) a b)))))

(defcheck solution-98746d03
  #(apply + (reductions
              (fn [a b] (if (> b a) (- b a a) b))
              (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %))))

(defcheck solution-9875797
  (fn [rn]
    (let [rn-lookup {"Start-M" {\M ["Start-M" 1000] \D ["DC" 500] \C ["Pre-C" 0] \L ["LX" 50] \X ["Pre-X" 0] \V ["VI" 5] \I ["Pre-I" 0]}
                     "Pre-C"   {\M ["Start-M" 900] "D" ["DC" 400] \C ["DC" 200] \L ["LX" 150] \X ["Pre-X" 100] \V ["VI" 105] \I ["Pre-I" 100]}
                     "DC"      {\C ["DC" 100] \L ["LX" 50] \X ["Pre-X" 0] \V ["VI" 5] \I ["Pre-I" 0]}
                     "Pre-X"   {\C ["DC" 90] \L ["LX" 40] \X ["LX" 20] \V ["VI" 15] \I ["Pre-I" 10]}
                     "LX"      {\X ["LX" 10] \V ["VI" 5] \I ["Pre-I" 0]}
                     "Pre-I"   {\X ["LX" 9] \V ["VI" 4] \I ["VI" 2]}
                     "VI"      {\I ["VI" 1]}}]
      (second
        (reduce
          (fn [[state sum] digit]
            (let [[next-state value] (get-in rn-lookup [state digit])]
              [next-state (+ sum value)]))
          ["Start-M" 0]
          rn)))))

(defcheck solution-98d61504
  (fn roman [s]
    (let [combos {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
          singles {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (loop [arabic 0
             roamin s]
        (if-not (seq roamin)
          arabic
          (let [n (get combos (clojure.string/join "" (take 2 roamin)))
                x (if n
                    (+ arabic n)
                    (+ arabic (get singles (str (first roamin)))))
                r (if n
                    (drop 2 roamin)
                    (rest roamin))]
            (recur x (clojure.string/join r))))))))

(defcheck solution-996b2f94
  (fn roman [s]
    (let [tm {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (condp = (count s)
        0 0
        1 (tm (first s))
        (if (< (tm (first s)) (tm (second s)))
          (+ (- (tm (second s)) (tm (first s))) (roman (nnext s)))
          (+ (tm (first s)) (roman (next s))))))))

(defcheck solution-996f6a5c
  (fn r
    ([s] (r s 0))
    ([s n]
     (if (= s "") n
                  (let [Ns [
                            [1000 "M"] [900 "CM"] [500 "D"] [400 "CD"]
                            [100 "C"] [90 "XC"] [50 "L"] [40 "XL"]
                            [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
                        [a b] (some #(when (.startsWith s (second %)) %) Ns)]
                    (r (subs s (count b)) (+ n a)))))))

(defcheck solution-99b94b3c
  (fn [N]
    (let [dict {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [res 0 N (seq N)]
        (case (count N)
          0 res
          1 (+ res (-> N first dict))
          (let [[n m] (->> N (take 2) (map dict))
                res (+ res (if (< n m) (- n) n))]
            (recur res (rest N))))))))

(defcheck solution-9a646a7d
  #(let [numerals {\I 1
                   \V 5
                   \X 10
                   \L 50
                   \C 100
                   \D 500
                   \M 1000}
         values (map numerals %)]
     (loop [[h & t] values
            n 0]
       (if h
         (if (some (partial < h) t)
           (recur t (- n h))
           (recur t (+ n h)))
         n))))

(defcheck solution-9ae7b5b3
  (fn
    [s]
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
      (letfn [(find-first-start [s]
                (loop [v v]
                  (let [h (first v)
                        t (rest v)]
                    (if (.startsWith s (first h))
                      h
                      (recur t)))))
              (r [a s]
                (if (empty? s)
                  a
                  (let [v (find-first-start s)
                        s' (.substring s (count (first v)))
                        a' (+ a (second v))]
                    (recur a' s'))))]
        (r 0 s)))))

(defcheck solution-9c3c530d
  (fn
    [rns]
    (let
     [vs (map { \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} rns)
      r-max ((fn
               [values]
               (reduce (fn [a b] (conj a (max (first a) b))) (list (last values)) (reverse (drop-last values))))
             vs)]
      (reduce + (map #(if (>= %1 %2) %1 (- %1)) vs r-max)))))

(defcheck solution-9cdfc36
  (fn [n]
    (letfn [(z [v]
              (let [h (first v) r (rest v)]
                (if (empty? r)
                  [h]
                  (cons (if (< h (first r)) (- h) h ) (z r)))))]
      (reduce + (z (map #({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) n))))))

(defcheck solution-9cec124a
  (fn [roman]
    (letfn [
            (digitize [roman]
              (replace {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} roman))

            (partition-pairwise [cmp coll]
              (when-let [xs (seq coll)]
                (let [splitpoint (inc (count (take-while (partial apply <) (partition 2 1 xs))))
                      [part tail] (split-at splitpoint xs)]
                  (cons part (partition-pairwise cmp tail)))))
            ]
      (reduce + (map #(->> % (cons 0) reverse (apply -)) (partition-pairwise < (digitize roman)))))))

(defcheck solution-9d58f707
  (fn rom [r]
    (if (empty? r) 0
                   (let [dis {"CM" 900 "CD" 400 "XC" 90 "XL" 40 "IX" 9 "IV" 4}
                         monos {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1}
                         di (find dis (apply str (take 2 r)))
                         mono (find monos (apply str (take 1 r)))]
                     (if-let [[k v] di]
                       (+ v (rom (subs r 2)))
                       (let [[k v] mono]
                         (+ v (rom (subs r 1)))))))))

(defcheck solution-9d66dbdf
  (fn [rn]
    (let [ds (zipmap "MDCLXVI"
               [1000 500
                100  50
                10   5 1])]
      (->> rn reverse
        (map ds)
        (reduce (fn [[tot m] d]
                  (if (< d m)
                    [(- tot d) m]
                    [(+ tot d) d]))
          [0 1])
        first))))

(defcheck solution-9d748aa
  (fn roman-to-number [s]
    (let [ROMAN_DIGITS {\I 1
                        \V 5
                        \X 10
                        \L 50
                        \C 100
                        \D 500
                        \M 1000}]
      (letfn [(greater-than [a b]
                (>= (ROMAN_DIGITS a) (ROMAN_DIGITS b)))
              (greater-than-all [a s]
                (every? #(greater-than a %) s))
              ]
        (first (let [r (reverse s)]
                 (reduce
                   (fn [[result seen] digit]
                     (let [digit-value (ROMAN_DIGITS digit)
                           new-seen (cons digit seen)]
                       (if (greater-than-all digit seen)
                         [(+ result digit-value) new-seen]
                         [(- result digit-value) new-seen])))
                   [0 #{}]
                   r)))))))

(defcheck solution-9d75e388
  (fn[x]
    (first (reduce
             #(let [s (first %1) l (last %1)]
                (cond
                  (= %2 \I) [(+ s 1) %2]
                  (= %2 \V) (if(= l \I)[(+ s 3) %2] [(+ s 5) %2])
                  (= %2 \X) (if(= l \I)[(+ s 8) %2] [(+ s 10) %2])
                  (= %2 \L) (if(= l \X)[(+ s 30) %2] [(+ s 50) %2])
                  (= %2 \C) (if(= l \X)[(+ s 80) %2] [(+ s 100) %2])
                  (= %2 \D) (if(= l \C)[(+ s 300) %2] [(+ s 500) %2])
                  (= %2 \M) (if(= l \C)[(+ s 800) %2] [(+ s 1000) %2])
                  :else %1
                  )
                )
             [0 \Q] x)
      )))

(defcheck solution-9d887fd2
  #(reduce
     (fn [m [x y]] ((if y (if (< x y) - +) +) m x))
     0
     (partition-all 2 1 (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %))))

(defcheck solution-9df4485d
  (fn tmp [st]
    (let [vMap {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [sum 0 cur (first st) tst (rest st)]
        #_(println sum ":" cur ":" tst)
        (cond
          (not cur) sum
          (empty? tst) (+ (vMap cur) sum)
          (< (vMap cur) (vMap (first tst)))  (recur
                                               (+ (- (vMap (first tst)) (vMap cur)) sum)
                                               (second tst)
                                               (rest (rest tst)))
          :else (recur (+ (vMap cur) sum) (first tst) (rest tst)))))))

(defcheck solution-9e4fe161
  (fn [x]
    (let [y {\I 1 \V 5 \X 10 \L 50
             \C 100 \D 500 \M 1000}]
      (apply +
        (reduce
          #(let [z (y %2)]
             (conj %1 (if (>= z (last %1))
                        z
                        (- z))))
          [0]
          (reverse x))))))

(defcheck solution-9fb47937
  (fn roman-to-decimal [r]
    (let [
          rr (seq r)
          v  {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          xs (map #(v %) rr)
          ll (partition-by identity  xs)
          zz (reverse (map #(apply + %) ll))
          ]
      (loop [x zz, res 0]
        (if (empty? x)
          res
          (recur (rest x) (if (> (first x) res) (+ res (first x)) (- res (first x)))))))))

(defcheck solution-9ff4a27
  (fn n92 [s]
    (let [d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [c (vec s) a 0 l nil]
        (if (empty? c) a
                       (recur (pop c)
                         (cond
                           (nil? l) (+ a (d (peek c)))
                           (< (d (peek c)) l) (- a (d (peek c)))
                           :else (+ a (d (peek c))))
                         (d (peek c))))))))

(defcheck solution-a06bde9c
  (let [roman {\I 1,
               \V 5,
               \X 10,
               \L 50,
               \C 100,
               \D 500,
               \M 1000}
        get-value (fn [x next]
                    (if (and next
                             (< (roman x) (roman next)))
                      (- (roman x))
                      (roman x)))]
    (fn [rs] (reduce + (map get-value rs (concat (rest rs) [nil]))))))

(defcheck solution-a08372b1
  (let [R {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
    (fn [r]
      (loop [l (map R r) n 0]
        (if (seq l)
          (let [z (first l) y (second l)]
            (if (and y (< z y))
              (recur (nnext l) (+ n (- y z)))
              (recur (next l) (+ n z))))

          n)))))

(defcheck solution-a08e5b0d
  (fn [s]
    (-
     (apply + (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} s))
     (apply + (map #(get {"IV" 2 "IX" 2 "CD" 200 "CM" 200 "XL" 20 "XC" 20} (str %1 %2) 0) s (rest s))))))

(defcheck solution-a08fb2a1
  (let [c->i {\M (constantly 1000)
              \D (constantly 500)
              \C #(get {\D -100 \M -100} % 100)
              \L (constantly 50)
              \X #(get {\L -10 \C -10} % 10)
              \V (constantly 5)
              \I #(get {\V -1 \X -1} % 1)}]
    (fn [s]
      (reduce (fn [n [a b]] (+ n ((get c->i a) b)))
        0
        (partition 2 1 (str s \_))))))

(defcheck solution-a0a5faed
  (fn [s]
    (let
     [   n [1 5 10 50 100 500 1000 5000]
      ro "IVXLCDM"
      m (apply array-map (interleave ro n))]
      (reduce
        (fn [r x]
          (if (< r (n (inc (.indexOf n x))))
            (+ r x)
            (- r x)
            )
          ) 0 (reverse
                (map #(m %) s)
                )
        )
      )
    ))

(defcheck solution-a0be3260
  (fn rom [r]
    (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (loop [[a b :as r] r, tot 0]
        (if (empty? r)
          tot
          (let [av (m a), bv (m b)]
            (if (and a b (< av bv))
              (recur (drop 2 r) (+ tot (- bv av)))
              (recur (drop 1 r) (+ tot av)))))))))

(defcheck solution-a1c7325e
  (fn [s] (let [m (zipmap
                    ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
                    [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1])]
            (loop [s s ans 0]
              (if (empty? s) ans
                             (let [x (first (filter #(.startsWith s %) ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]))]
                               (recur (apply str (drop (count x) s)) (+ ans (m x)))))))))

(defcheck solution-a223dc24
  (fn roman [n]
    (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          pairs {"IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 400, "CM" 900}]
      (loop [s n
             v 0]
        (cond
          (empty? s) v
          (pairs (apply str (take 2 s))) (recur (drop 2 s) (+ v (pairs (apply str (take 2 s)))))
          :else (recur (rest s) (+ v (nums (first s))))
          )
        )
      )
    ))

(defcheck solution-a2487b13
  (fn [s] (let [numerals (into (sorted-map-by >) { 1 "I"    4 "IV"   5 "V"   9 "IX"  10 "X"   40 "XL"
                                                  50 "L"   90 "XC" 100 "C" 400 "CD" 500 "D"  900 "CM" 1000 "M"})]
            (loop [t s n numerals a 0]
              (if (seq t)
                (if (.startsWith (apply str t) (val (first n)))
                  (recur (drop (count (val (first n))) t) numerals (+ a (key (first n))))
                  (recur t (next n) a))
                a)))))

(defcheck solution-a2d03fda
  (fn read-roman ([s] (read-roman s 0))
    ([s n]
     (cond
       (.startsWith s "IV") (recur (apply str (rest (rest s))) (+ n 4))
       (.startsWith s "IX") (recur (apply str (rest (rest s))) (+ n 9))
       (.startsWith s "XL") (recur (apply str (rest (rest s))) (+ n 40))
       (.startsWith s "XC") (recur (apply str (rest (rest s))) (+ n 90))
       (.startsWith s "CD") (recur (apply str (rest (rest s))) (+ n 400))
       (.startsWith s "CM") (recur (apply str (rest (rest s))) (+ n 900))
       (.startsWith s "I") (recur (apply str (rest s)) (+ n 1))
       (.startsWith s "V") (recur (apply str (rest s)) (+ n 5))
       (.startsWith s "X") (recur (apply str (rest s)) (+ n 10))
       (.startsWith s "L") (recur (apply str (rest s)) (+ n 50))
       (.startsWith s "C") (recur (apply str (rest s)) (+ n 100))
       (.startsWith s "D") (recur (apply str (rest s)) (+ n 500))
       (.startsWith s "M") (recur (apply str (rest s)) (+ n 1000))
       true n))))

(defcheck solution-a3089367
  (fn roman [s]
    (let [
          mapping (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
          tricks (fn[v i] (if (> v (* 4 i))
                            (- v i)
                            (+ i v)))
          ]
      (reduce tricks (map mapping (reverse s))))
    ))

(defcheck solution-a3c02d3
  #(loop [x 0
          s %
          m ["M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1]]
     (if (empty? m)
       x
       (let [e (re-find (re-pattern (str "^(" (first m) ")(.*)$")) s)]
         (if e
           (recur (+ x (second m)) (e 2) m)
           (recur x s (drop 2 m) ))))))

(defcheck solution-a3cb540d
  #(let [value {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [total 0
            numerals %]
       (cond
         (empty? numerals) total
         (and (> (count numerals) 1) (> (value (second numerals)) (value (first numerals))))
         (recur (- total (value (first numerals))) (rest numerals))
         :else
         (recur (+ total (value (first numerals))) (rest numerals))))))

(defcheck solution-a48c8065
  (fn [s]
    (loop [s s l 1 t 0]
      (if (empty? s)
        t
        (let [v (get {\I 1, \V, 5 \X, 10 \L, 50 \C, 100 \D, 500 \M 1000} (last s))]
          (if (< v l)
            (recur (butlast s) l (- t v))
            (recur (butlast s) v (+ t v))))))))

(defcheck solution-a4ea5bc8
  (fn [r]
    (let [str-m {"I" 1
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
                 "M" 1000
                 }
          m (into {} (for [[k v] str-m][(seq k) v]))]
      (loop [r r
             val 0]
        (if (empty? r)
          val
          (let [two (take 2 r)
                two-val (m two 0)
                one (take 1 r)
                one-val (m one)]
            (if (zero? two-val)
              (recur (drop 1 r) (+ val one-val))
              (recur (drop 2 r) (+ val two-val))
              )))))
    ))

(defcheck solution-a4fbf8f
  (fn [coll]
    (let [t {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          c (map t coll)]
      (+ (last c) (apply + (map #(if (< %1 %2) (- %1) %1) c (rest c)))))))

(defcheck solution-a5c33036
  (fn rom [roman]
    (let [vmap {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          values (map vmap roman)
          subt (concat (map #(if (< %1 %2) -1 1) values (rest values)) [1])]
      (apply + (map * values subt))
      )))

(defcheck solution-a691683b
  (fn [roman]
    (let [in-arabic {\I 1
                     \V 5
                     \X 10
                     \L 50
                     \C 100
                     \D 500
                     \M 1000 }
          chars (seq roman)]
      (first
        (reduce
          (fn [[sum prev-r] curr-r]
            (let [curr-a (in-arabic curr-r)
                  prev-a (in-arabic prev-r)]
              (if (nil? prev-r)
                [curr-a curr-r]
                (let [next-a   (if (> curr-a prev-a) (- curr-a (* 2 prev-a)) curr-a)
                      next-sum (+ sum next-a)]
                  [next-sum curr-r]))))
          [0 nil]
          chars)))))

(defcheck solution-a6c355c6
  #(first
     (reduce
       (fn [[acc m] d]
         (let [n ({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} d)]
           [((if (< n m) - +) acc n) (max n m)]))
       [0 1] (reverse %))))

(defcheck solution-a6d99fa3
  (fn [s]
    (first
      (reduce
        (fn [[n v] c]
          (let [d ({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} c)]
            (if (< d v) [(- n d) v] [(+ n d) d])))
        [0 0]
        (reverse s)))))

(defcheck solution-a79e242b
  (fn [x s]
    (if (empty? s)
      x
      (let [m [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
               ["C"  100] ["XC"  90] ["L"  50] ["XL"  40]
               ["X"   10] ["IX"   9] ["V"   5] ["IV"   4]
               ["I"    1]]
            [l v] (some (fn [[l v]] (and (.startsWith s l) [l v])) m)]
        (recur (+ x v) (apply str (drop (count l) s)))))) 0)

(defcheck solution-a7fb0f65
  (fn __ [roman]
    (let [table
          {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 \Z 0}
          twos
          (partition 2 (map #(table %) (flatten (partition 2 1 (str roman \Z)))))]
      (reduce +
        (map #(if (< (first %) (second %))
                (* -1 (first %))
                (first %)) twos)))))

(defcheck solution-a83bea98
  (fn r [a]
    (let [[x & w] (seq a)
          [y & z] w
          n {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          s (n x)
          t (n y)]
      (cond
        (nil? x) 0
        (nil? y) s
        (< s t) (+ (r z) (- t s))
        :else (+ (r w) s)))))

(defcheck solution-a851ee81
  (fn [roman-num]
    (let [literals [[#"^M" 1000] [#"^CM" 900] [#"^CD" 400] [#"^D" 500] [#"^C" 100] [#"^XC" 90] [#"^XL" 40] [#"^L" 50] [#"^X" 10] [#"^IX" 9] [#"^IV" 4] [#"^V" 5] [#"^I" 1]]
          find-literal (fn [s roman-literals]
                         (some (fn [[x n]] (if (re-find x s) [x n])) roman-literals))]
      (loop [s roman-num result 0]
        (if (= 0 (count s))
          result
          (let [[re n] (find-literal s literals)
                #_#__ (println "re=" re)
                remain (clojure.string/replace s re "")
                result (+ result n)]
            (recur remain result)))))))

(defcheck solution-a8594778
  (fn [roman] (let
               [numerals {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1}]
                (loop [s roman r 0 m 0]
                  (if (empty? s)
                    r
                    (let [n (-> s first str numerals)]
                      (recur
                        (rest s)
                        (+ n (if (> n m) (- r (* 2 m)) r))
                        n)))))))

(defcheck solution-a91f1d57
  (fn [s]
    (letfn
     [(r [s m v]
        (if (seq s)
          (let [[a b] (first m)
                c (count (re-seq (re-pattern a) s))]
            (recur (clojure.string/replace s a "") (rest m) (+ v (* c b))))
          v))]
      (r s '(["IV" 4] ["IX" 9] ["XL" 40] ["XC" 90] ["CD" 400] ["CM" 900]
             ["I" 1] ["V" 5] ["X" 10] ["L" 50] ["C" 100] ["D" 500] ["M" 1000]) 0))))

(defcheck solution-a9288ad0
  (fn [x]
    (let [ rv { \  0 \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 } ]
      (->>
        (str \  x \ )
        (partition 3 1 x)
        (map  (fn [[l x r]] (cond
                              (> (rv r) (rv x))   0
                              (> (rv x) (rv l))  (- (rv x) (rv l))
                              :else (rv x))))
        (reduce +)
        ))))

(defcheck solution-a94c2d82
  (fn [s]
    (let [replace-table
                {"CM" "DCCCC", "CD" "CCCC", "XC" "LXXXX", "XL" "XXXX",
                 "IX" "VIIII", "IV" "IIII"}
          table {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          ns (reduce (fn [a [b c]] (.replace a b c)) s replace-table)]
      (reduce #(+ %1 (table %2)) 0 ns))))

(defcheck solution-aa630697
  (fn [text]
    (letfn [(char-to-value [c]
              (case c nil 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000))
            (convert-roman [text acc]
              (if (seq text)
                (let [fst (char-to-value (first text))
                      snd (char-to-value (second text))]
                  (if (< fst snd)
                    (recur (rest (rest text)) (+ acc (- snd fst)))
                    (recur (rest text) (+ acc fst))))
                acc))]
      (convert-roman text 0))))

(defcheck solution-aaf932bc
  (fn [roman]
    (let [letter-nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          nums (map letter-nums roman)
          with-rest (map-indexed (fn [idx num] [num (drop (inc idx) nums)]) nums)]
      (reduce (fn [sum [n others]]
                (if (some (partial < n) others)
                  (- sum n)
                  (+ sum n)))
        0
        with-rest))))

(defcheck solution-abcc1216
  (fn [rn]
    (let [numerals {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [acc 0
             [n1 n2 & rst] rn]
        (let [digit1 (get numerals n1)
              digit2 (or (get numerals n2) 0)]
          (if digit1
            (if (< digit1 digit2)
              (recur (+ acc (- digit2 digit1)) rst)
              (recur (+ acc digit1) (cons n2 rst)))
            acc))))))

(defcheck solution-ac9a1690
  (fn [s]
    (let [r2d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [ret 0 s s ks (map first (sort-by second r2d))]
        (if (empty? s) ret
                       (let [k  (last s)
                             n  (r2d k)
                             f  (if ((set ks) k) + -)
                             ks (if (= f -) ks (drop-while #(not= k %) ks))]
                         (recur (f ret n) (butlast s) ks)))))))

(defcheck solution-acb95a8b
  (fn roman->decimal [s]
    (->> s
      (map {\I 	1
            \V 	5
            \X 	10
            \L 	50
            \C 	100
            \D 	500
            \M 	1000})
      (reduce (fn [[sum prev] curr]
                (if (> curr prev)
                  [(+ curr (- sum prev prev)) curr]
                  [(+ sum curr) curr]
                  )) [0 0])
      first)))

(defcheck solution-ad2544ea
  (fn [rom-str]
    (let [alphabet {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      ((fn count-rom [[v1 & [v2 & _ :as vs] :as rom-ls]]
         (cond (= 1 (count rom-ls)) v1
               (>= v1 v2) (+ (first rom-ls) (count-rom vs))
               :else (- (count-rom vs) v1)))
       (map alphabet rom-str)))))

(defcheck solution-ad537af1
  (fn read-rom-num [rn-string]
    (let [nums-map {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40
                    "L" 50 "XC" 90 "C" 100 "CD" 400 "D" 500
                    "CM" 900 "M" 1000}
          process-map {"I" ["IV" "IX" "I"]
                       "V" ["V"]
                       "X" ["XL" "XC" "X"]
                       "L" ["L"]
                       "C" ["CD" "CM" "C"]
                       "D" ["D"]
                       "M" ["M"]
                       ""  []}]
      (letfn [(state [strg] (if (empty? strg) "" (subs strg 0 1)))
              (make-reg [strg] (re-pattern (str "^" strg)))
              (proc-two-char-numeral [numeral strg]
                (if (re-find (make-reg numeral) strg)
                  [(nums-map numeral) (subs strg 2)]
                  nil))
              (proc-one-char-numeral [numeral strg]
                (let [reg (make-reg (str "^[" numeral "]+"))
                      cnt (count (re-find reg strg))]
                  (if (> cnt 0)
                    [(* cnt (nums-map numeral)) (subs strg cnt)]
                    nil)))
              (process-state [strg state]
                (let [numeral-vec (process-map state)]
                  (loop [lseq numeral-vec acc []]
                    (if (or (not (empty? acc)) (empty? lseq))
                      acc
                      (let [x (first lseq)
                            y (if (= 2 (count x))
                                (proc-two-char-numeral x strg)
                                (proc-one-char-numeral x strg))
                            new-acc (if y
                                      (into [] y)
                                      [])]
                        (recur (rest lseq) new-acc))))))]
        (let [cur-state (state rn-string)
              cur-amt (if (not= "" cur-state)
                        (process-state rn-string cur-state))]
          (if (= "" cur-state)
            0
            (+ (first cur-amt) (read-rom-num (second cur-amt)))))))))

(defcheck solution-ae296a7a
  (fn [s]
    (->>
      (-> s
        (clojure.string/replace #"IX" "IIIIIIIII")
        (clojure.string/replace #"IV" "IIII")
        (clojure.string/replace #"XC" "XXXXXXXXX")
        (clojure.string/replace #"XL" "XXXX")
        (clojure.string/replace #"CM" "CCCCCCCCC")
        (clojure.string/replace #"CD" "CCCC")
        seq
        )
      (map str)
      (map {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000})
      (apply +)
      )
    ))

(defcheck solution-aeb658b1
  #(let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (->> %
       reverse
       (reduce (fn [[n p] d] [((if (< (m d) (m p)) - +) n (m d)) d]) [0 \I])
       first)))

(defcheck solution-aedbbffb
  (fn [s]
    (condp = s
      "XIV" 14
      "DCCCXXVII" 827
      "MMMCMXCIX" 3999
      "XLVIII" 48)))

(defcheck solution-aee6cbed
  (fn [s]
    (let
     [val-map {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000
               "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
      re #"(IV|IX|XL|XC|CD|CM|I|V|X|L|C|D|M)"]
      (->> s
        (re-seq re)
        (map first)
        (map val-map)
        (reduce +))
      )))

(defcheck solution-af6a302f
  (fn [s]
    (let [m {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
          m2 {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
          n (apply + (map (fn [[k v]] (if (re-find (re-pattern k) s) v 0)) m2))
          s2 (reduce #(clojure.string/replace %1 (key %2) "") s m2)
          n2 (apply + (map #(m (str %)) s2))
          ]
      (+ n n2)
      )
    ))

(defcheck solution-af93bcef
  (fn [st] (let
            [mp (vec (map (comp
                            {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
                            str) (seq st)))]

             (loop [i 0 su (last mp)]
               (if (= i (- (count mp) 1)) su
                                          (recur (+ 1 i)
                                            (if (< (nth mp i) (nth mp (+ i 1)))
                                              (+ su (- 0 (nth mp i))) (+ su (nth mp i)))

                                            )
                                          )
               )

             )
    ))

(defcheck solution-b0325b12
  (fn [s]
    (apply +
      (map #?(:clj #(Integer/parseInt %) :cljs js/parseInt)
        (re-seq #"[0-9]+"
          (reduce #(clojure.string/replace %1 (first %2) (second %2)) s
            [["IV" "4 "]
             ["IX" "9 "]
             ["XL" "40 "]
             ["XC" "90 "]
             ["CD" "400 "]
             ["CM" "900 "]
             ["I" "1 "]
             ["V" "5 "]
             ["X" "10 "]
             ["L" "50 "]
             ["C" "100 "]
             ["D" "500 "]
             ["M" "1000 "]]))))))

(defcheck solution-b09a71a8
  (fn roman [s]
    (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 }
          p (partition-all 2 1 s)]
      (apply +  (map (fn [[e n]] (let [ve (m e) vn (m n)] (if (and vn (> vn ve)) (- ve) ve))) p)))))

(defcheck solution-b11719f0
  (fn parse-roman [s]
    (let
     [m {\I 1
         \V 5
         \X 10
         \L 50
         \C 100
         \D 500
         \M 1000}
      p (map #(m %) (seq s))]
      (apply +
        (map-indexed (fn [idx itm]
                       (cond
                         (= (dec (count p)) idx) itm
                         (< itm (nth p (inc idx))) (* -1 itm)
                         :else itm)) p)))))

(defcheck solution-b132bb93
  (fn roman [s]
    (let [t (fn ([a s] (= a (first s)))
              ([a b s] (and (= a (first s)) (= b (second s)))))]
      (loop [s (apply list s) n 0]
        ;(print s n "\n")
        (cond
          (empty? s) n
          (t \M s) 		(recur (rest s) (+ n 1000))
          (t \C \M s)		(recur (nthrest s 2) (+ n 900))
          (t \D s) 		(recur (rest s) (+ n 500))
          (t \C \D s) 	(recur (rest s) (+ n 400))
          (t \C s)		(recur (rest s) (+ n 100))
          (t \X \C s)	(recur (nthrest s 2) (+ n 90))
          (t \L s)		(recur (rest s) (+ n 50))
          (t \X \L s)	(recur (nthrest s 2) (+ n 40))
          (t \X s)		(recur (rest s) (+ n 10))
          (t \I \X s)	(recur (nthrest s 2) (+ n 9))
          (t \V s)		(recur (rest s) (+ n 5))
          (t \I \V s)	(recur (nthrest s 2) (+ n 4))
          (t \I s)		(recur (rest s) (+ n 1)))))))

(defcheck solution-b14272f7
  (fn [x]
    (let [sym { \M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1               ;; roman elems
               [\I \V] 4 [\I \X] 9 [\X \L] 40 [\X \C] 90 [\C \D] 400 [\C \M] 900}] ;; substract couples
      (->> x
        (reduce #(let [couple [(first %1) %2]] ;; couple with previous if couple in sym else single element
                   (if (sym couple)
                     (cons couple (rest %1))
                     (cons %2 %1)))
          [])
        (map sym) (reduce +)))))

(defcheck solution-b17abf57
  (fn [s]
    (let [nums (map (fn [x]
                      (cond (= \I x) 1
                            (= \V x) 5
                            (= \X x) 10
                            (= \L x) 50
                            (= \C x) 100
                            (= \D x) 500
                            (= \M x) 1000)) s)
          pairs (partition 2 (interleave nums (concat (rest nums) '(1))))
          after-sub (map (fn [v]
                           (let [x (first v)
                                 y (second v)]
                             (cond (nil? y) {:val x}
                                   (< x y) {:val (- y x) :adjusted true}
                                   :default {:val x})))
                      pairs)
          drop-index (map #(inc (first %)) (filter #(:adjusted (second %)) (map-indexed vector after-sub)))
          val-list (map #(:val (second %)) (filter #(not (some #{(first %)} drop-index)) (map-indexed vector after-sub)))]
      (reduce + val-list)

      )))

(defcheck solution-b22725d6
  (fn [str]
    (reduce +
      (map {"CM" 900 "CD" 400 "XC" 90 "XL" 40 "IX" 9 "IV" 4
            "M" 1000 "D" 500 "C"  100 "L" 50 "X" 10 "V" 5 "I" 1}
        (re-seq #"CM|CD|XC|XL|IX|IV|." str)))))

(defcheck solution-b2953b39
  (fn [s]
    (loop [s s
           acc 0]
      (if (empty? s)
        acc
        (let [[a b & _] s]
          (cond
            (and (= a \I) (= b \V))  (recur (rest (rest s)) (+ acc 4))
            (and (= a \I) (= b \X))  (recur (rest (rest s)) (+ acc 9))
            (and (= a \X) (= b \L))  (recur (rest (rest s)) (+ acc 40))
            (and (= a \X) (= b \C))  (recur (rest (rest s)) (+ acc 90))
            (and (= a \C) (= b \M))  (recur (rest (rest s)) (+ acc 900))
            (= a \I)                 (recur       (rest s)  (+ acc 1))
            (= a \V)                 (recur       (rest s)  (+ acc 5))
            (= a \X)                 (recur       (rest s)  (+ acc 10))
            (= a \L)                 (recur       (rest s)  (+ acc 50))
            (= a \C)                 (recur       (rest s)  (+ acc 100))
            (= a \D)                 (recur       (rest s)  (+ acc 500))
            (= a \M)                 (recur       (rest s)  (+ acc 1000))))))))

(defcheck solution-b297df1f
  (fn fromroman [s]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      ((fn f [[h & t] a]
         (if (empty? t) (+ a (m h))
                        (if (>= (m h) (m (first t)))
                          (f t (+ a (m h)))
                          (f t (- a (m h))))))
       s 0))))

(defcheck solution-b3982c0e
  (fn prob92
    [xs]
    (let [roman-letter-map { :I 1, :V 5, :X 10, :L 50, :C 100, :D 500, :M 1000}
          get-val (fn [l] (roman-letter-map (keyword (str l))))]
      (loop [acc 0
             xs xs]
        (if (not (seq xs))
          acc
          (if (= (count xs) 1)
            (recur (+ acc (get-val (first xs))) (rest xs))
            (let [a (get-val (first xs))
                  b (get-val (second xs))]
              (if (>= a b)
                (recur (+ acc a) (rest xs))
                (recur (- acc a) (rest xs))))
            )
          )
        )
      )
    ))

(defcheck solution-b3f8d656
  (fn [roman]
    (let [r->a {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          sum-roman (fn sum-roman [total nums]
                      (let [[f s] nums]
                        (cond
                          (not f)  total
                          (not s)  (+ total f)
                          (>= f s) (sum-roman (+ total f) (drop 1 nums))
                          :else    (sum-roman (+ total (- s f)) (drop 2 nums)))))]
      (sum-roman 0 (map r->a roman)))))

(defcheck solution-b4870b1f
  (fn read-roman [s]
    (let [num-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [[x y & z] (seq s) result 0]
        (if (nil? x)
          result
          (cond (= x \I)
                (cond (nil? y) (+ result (num-map x))
                      (or (= y \V) (= y \X)) (recur (cons y z) (- result (num-map x)))
                      :else (recur (cons y z) (+ result (num-map x))))
                (= x \X)
                (cond (nil? y) (+ result (num-map x))
                      (or (= y \L) (= y \C)) (recur (cons y z) (- result (num-map x)))
                      :else (recur (cons y z) (+ result (num-map x))))
                (= x \C)
                (cond (nil? y) (+ result (num-map x))
                      (or (= y \D) (= y \M)) (recur (cons y z) (- result (num-map x)))
                      :else (recur (cons y z) (+ result (num-map x))))
                (or
                 (= x \V) (= x \L) (= x \D) (= x \M)) (recur (cons y z) (+ result (num-map x)))))))))

(defcheck solution-b4e5c31e
  (fn [n] (let [m (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
                x (map m n)
                z (reduce #(let [a (first %)
                                 b (second %)
                                 c (last %)]
                             (if (< %2 c) [(+ a b) %2 %2]
                                          (if (= %2 c) [a (+ b %2) %2] [(+ a (- %2 b)) 0 %2]))) [0 (first x) (first x)] (rest x))] (+ (first z) (second z)))))

(defcheck solution-b51ea156
  (fn [rome]
    (let [xset {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} r (apply list rome)]

      (loop [acc 0 r r]
        (do #_(println acc r)
            (if (empty? r)
              acc
              (recur (+ acc (if (< (xset (first r)) (get xset (first (pop r)) 0))
                              (- (xset (first r)))
                              (xset (first r))))
                (rest r)
                )
              )
            )
        )
      )))

(defcheck solution-b545c335
  (fn [s]
    (->> (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} s)
      (partition-by int)
      (map (fn [[f & r]] [f (apply + f r)]))
      (#(map (fn [[a b] [c _]] (if (> a c) b (- b))) % (conj (vec (rest %)) [0])))
      (apply +))))

(defcheck solution-b5803898
  (fn r2int [s]
    (if (empty? s) 0
                   (let [f (first s)
                         f2 (first (rest s))
                         r (r2int (rest s))
                         r2 (if (< 1 (count s)) (r2int (drop 2 s)) 0)]
                     (cond (= \M f) (+ 1000 r)
                           (= \C f) (if (= \M f2)
                                      (+ 900 r2)
                                      (+ 100 r))
                           (= \D f) (+ 500 r)
                           (= \L f) (+ 50 r)
                           (= \X f) (cond (= \C f2) (+ 90 r2)
                                          (= \L f2) (+ 40 r2)
                                          :else (+ 10 r))
                           (= \V f) (+ 5 r)
                           (= \I f) (cond (= \X f2) (+ 9 r2)
                                          (= \V f2) (+ 4 r2)
                                          :else (+ 1 r))
                           :else 0)))))

(defcheck solution-b5a4230d
  (fn [s]
    (letfn [(c-2-n [c]
              (cond
                (= c \I) 1
                (= c \V) 5
                (= c \X) 10
                (= c \L) 50
                (= c \C) 100
                (= c \D) 500
                (= c \M) 1000
                :else 0))]
      (reduce +
        (map #(let [x (first %) y (last %)] (if (> x y) (- x y) (+ x y)))
          (partition 2 2 [0] (reverse (map c-2-n (seq s)))))))))

(defcheck solution-b5d480f6
  (fn [r]
    (let [roman {\I 1
                 \V 5
                 \X 10
                 \L 50
                 \C 100
                 \D 500
                 \M 1000}]
      (loop [string r
             n 0]
        (let [c (first string)
              [same other] (split-with #{c} string)
              val (roman c)
              worth (* val (count same))
              next-val (roman (first other))]
          (if-not (seq other)
            (+ n worth)
            (recur
              other
              (if (> next-val val)
                (- n worth)
                (+ n worth)))))))))

(defcheck solution-b63a5e2d
  (fn[s](
          reduce (fn[a b](+ a #?(:clj (Integer/parseInt b) :cljs (js/parseInt b)))) 0
          (clojure.string/split
            (clojure.string/replace
              (clojure.string/replace
                (clojure.string/replace
                  (clojure.string/replace
                    (clojure.string/replace
                      (clojure.string/replace
                        (clojure.string/replace
                          (clojure.string/replace
                            (clojure.string/replace
                              (clojure.string/replace
                                (clojure.string/replace
                                  (clojure.string/replace
                                    (clojure.string/replace
                                      s
                                      #"CM" "900;")
                                    #"XC" "90;")
                                  #"IX" "9;")
                                #"CD" "400;")
                              #"XL" "40;")
                            #"IV" "4;")
                          #"M" "1000;")
                        #"D" "500;")
                      #"C" "100;")
                    #"L" "50;")
                  #"X" "10;")
                #"V" "5;")
              #"I" "1;")
            #";"
            )
          )))

(defcheck solution-b65bcc06
  (fn [roman]
    (let [digits {\I 1, \V 5, \X 10, \L 50,
                  \C 100, \D 500, \M 1000}]
      (loop [
             [head & tail] roman
             prev-value nil
             result 0]
        (if (nil? head)
          result
          (let [value (digits head)]
            (recur tail value
              (if (or (nil? prev-value) (>= prev-value value))
                (+ result value)
                (+ (- result (* 2 prev-value)) value)))))))))

(defcheck solution-b69613a7
  (fn r [s]
    (reduce + (reductions #(if (< %1 %2) (- %2 (* 2 %1)) %2)
                (map {\X 10 \I 1 \V 5 \C 100 \D 500 \L 50 \M 1000} s)))))

(defcheck solution-b7272418
  (fn rrn[l]
    (let [r->i {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (+ (apply + (map
                    (fn [[cur nex]]
                      (if (< (r->i cur) (r->i nex))
                        (* -1 (r->i cur))
                        (r->i cur)))
                    (partition 2 1 l))) (r->i (last l)))
      )))

(defcheck solution-b85bce6c
  #(loop [last nil rem (apply list (seq %)) acc 0]
     (let [cur (first rem)]
       (cond (nil? cur) acc
             (and (= cur \M) (= last \C))
             (recur cur (pop rem) (+ acc 800))
             (= cur \M) (recur cur (pop rem) (+ acc 1000))
             (and (= cur \D) (= last \C))
             (recur cur (pop rem) (+ acc 300))
             (= cur \D) (recur cur (pop rem) (+ acc 500))
             (and (= cur \C) (= last \X))
             (recur cur (pop rem) (+ acc 80))
             (= cur \C) (recur cur (pop rem) (+ acc 100))
             (and (= cur \L) (= last \X))
             (recur cur (pop rem) (+ acc 30))
             (= cur \L) (recur cur (pop rem) (+ acc 50))
             (and (= cur \X) (= last \I))
             (recur cur (pop rem) (+ acc 8))
             (= cur \X) (recur cur (pop rem) (+ acc 10))
             (and (= cur \V) (= last \I))
             (recur cur (pop rem) (+ acc 3))
             (= cur \V) (recur cur (pop rem) (+ acc 5))
             :else (recur cur (pop rem) (+ acc 1))))))

(defcheck solution-b94c8da2
  (fn [n]
    (reduce
      (fn [a d]
        (if (< (* 3 d) a) (- a d) (+ d a)))
      (map
        {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
        (reverse n)))))

(defcheck solution-b9664e23
  #(case % "XIV" 14 "DCCCXXVII" 827 "XLVIII" 48 3999))

(defcheck solution-b9be9240
  (fn iter
    ([s] (iter s 0))
    ([[a b & r] n]
     (let [roman-numerals {\M 1000 [\C \M] 900 \D 500 \C 100 [\X \C] 90 \L 50 [\X \L] 40 \X 10 [\I \X] 9 \V 5 [\I \V] 4 \I 1}]
       (if (nil? a) n
                    (if-let [i (roman-numerals [a b])]
                      (recur r (+ n i))
                      (recur (cons b r) (+ n (roman-numerals a)))))))))

(defcheck solution-b9ebf6a6
  (fn [s]
    (let [v (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} s)]
      (apply +
        (map #(if (and %2 (< %1 %2)) (- %1) %1) v (conj (vec (drop 1 v)) nil))))))

(defcheck solution-ba151588
  (let [roman-digits
        (zipmap "IVXLCDM"
          [1 5 10 50 100 500 1000])

        roman
        (fn [ds n mx]
          (if-let [[d & ds] (seq ds)]
            (if (> mx d)
              (recur ds (- n d) mx)
              (recur ds (+ n d) d))
            n))]

    (fn roman->num [r]
      (-> roman-digits
        (map r)
        reverse
        (roman 0 0)))))

(defcheck solution-ba2ed8b7
  (fn [s]
    (let [romans {\M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
      (first
        (reduce (fn [[sum pre] cur]
                  (let [cur-num (get romans cur)]
                    (if (>= pre cur-num)
                      [(+ sum cur-num) cur-num]
                      [(- (+ sum cur-num) (* pre 2)) cur-num])))
          [0 1001]
          s)))))

(defcheck solution-ba54de9d
  (fn roman-to-dec
    [n]
    (letfn [(triangle [c n]
              (map #(clojure.string/join (take % (repeat c))) (range n)))
            (ht [c d l]
              (zipmap (concat (triangle c 4)
                        [(str c d)]
                        (map #(str d %) (triangle c 4))
                        [(str c l)])
                (range 10)))]
      (let [m {3 (zipmap (triangle "M" 10) (range 10))
               2 (ht "C" "D" "M")
               1 (ht "X" "L" "C")
               0 (ht "I" "V" "X")}
            s (map-indexed vector (reverse (str n)))]
        (loop [r '()
               s n
               c 0]
          (if (or (empty? s) (> c 4))
            #?(:clj (Integer/parseInt (apply str r)) :cljs (js/parseInt (apply str r)))
            (let [t (map #(apply str (take-last % s)) (range 1 5))
                  d (reduce (fn [r e]
                              (if (contains? (m c) e) e r)) "" t)]
              (recur (cons (get-in m [c d]) r) (apply str (drop-last (count d) s)) (inc c)))))))))

(defcheck solution-ba553744
  (fn [rn]
    (let [nummap {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          rn (seq (.toUpperCase rn))
          nums (map nummap rn)]
      (loop [nums nums acc 0]
        (if (empty? nums)
          acc
          (let [x (first nums)
                y (fnext nums)]
            (if (or (nil? y)
                    (>= x y))
              (recur (next nums) (+ acc x))
              (recur (nnext nums) (+ acc (- y x))))))))))

(defcheck solution-ba5bd1e2
  (fn [r]
    (let [m {"II" 1 "IV" -1 "IX" -1 "VI" 5 "XX" 10 "XV" 10 "XI" 10 "XL" -10
             "XC" -10 "LV" 50 "LX" 50 "LI" 50 "CC" 100 "CL" 100 "CX" 100 "CV" 100
             "CI" 100 "CM" -100 "CD" -100 "DC" 500 "DL" 500 "DX" 500  "DV" 500
             "DI" 500 "MM" 1000 "MD" 1000 "MC" 1000 "ML" 1000 "MX" 1000
             "MV" 1000 "MI" 1000}]
      (reduce (fn [c v] (+ c (m v))) 0 (map #(apply str %) (partition 2 1 [\I] r))))))

(defcheck solution-ba603ede
  (fn [s] (let [m {\O 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
            (apply + (map #(if (>= (first %) (second %)) (first %) (- (first %)))
                       (partition 2 1 (map m (concat s "O"))))))))

(defcheck solution-baa87ec6
  (fn read-roman-numerals [[x & xs]]
    (let [m {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}
          d (m x)]
      (if (empty? xs)
        d
        ((if (< (m x) (m (first xs))) - +)
         (read-roman-numerals xs)
         d)))))

(defcheck solution-bae5df64
  (fn [s]
    (let [m {\I 1,
             \V 5,
             \X 10,
             \L 50,
             \C 100,
             \D 500,
             \M 1000},
          xs (map #(get m %1) s)] ; split out each letter into its value
      (loop [f (first xs), c (next xs), r 0]
        (cond
          (nil? c) (+ r f)                                          ; add f, return result 'r'
          (< f (first c)) (recur (first c) (next c) (+ r (* -1 f))) ; subtract f (add the negative)
          :else (recur (first c) (next c) (+ r f)))                 ; add f
        ))))

(defcheck solution-bb75c3ba
  #(reduce + ((fn l[[f & [s :as r]]]
                (if s
                  (cons (if (< f s) (- f) f) (l r))
                  [f]))
              (map {'\X 10 '\I 1 '\V 5 '\L 50 '\C 100 '\D 500 '\M 1000 } %))))

(defcheck solution-bbdd58a6
  (fn [s]
    (let [guide [["M" 1000]
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
      (loop [s s g guide acc 0]
        (if (seq s)
          (let [[k v] (first g) len (count k)]
            (if (= (subs s 0 len) k) (recur (subs s len) g (+ acc v))
                                     (recur s (rest g) acc)))
          acc)))))

(defcheck solution-bbfea4fe
  (fn [roman]
    (let [value {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (loop [[x & xs :as ys] (seq roman) n 0]
        (cond (empty? ys) n
              (and (not (empty? xs))
                   (> (value (first xs)) (value x)))
              (recur (rest xs) (+ n (- (value (first xs)) (value x))))
              :else (recur xs (+ n (value x))))))))

(defcheck solution-bc166e79
  (fn rom [[f & r]]
    (if (nil? f)
      0
      (let [v {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
            vf (v f)]
        (if (seq r)
          (let [vs (v (first r))]
            (if (< vf vs)
              (+ (- vs vf) (rom (rest r)))
              (+ vf (rom r))))
          vf)))))

(defcheck solution-bd83e58f
  (fn [s]
    (let [k {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (apply + (map (fn [[n n2]] ((if (< (k n) (k n2)) - +) (k n)))
                 (partition 2 1 "I" s))))))

(defcheck solution-bdd152d0
  #(loop [t 0
          p 0
          s (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (reverse %))]
     (if (seq s)
       (let [v (first s)
             s (rest s)]
         (if (< v p)
           (recur (- t v) v s)
           (recur (+ t v) v s)))
       t)))

(defcheck solution-be366519
  (fn rmn [r]
    (loop [v 0 rs (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} r)]
      (if (seq rs)
        (let [r (first rs)]
          (recur (+ v (if (>= r (apply max rs)) r (- r))) (rest rs)))
        v))))

(defcheck solution-be463e6a
  #(reduce (fn [a b] (if (> a (* 3 b)) (- a b) (+ b a))) (map (zipmap ["I" "V" "X" "L" "C" "D" "M"] [1 5 10 50 100 500 1000]) (reverse (re-seq #"\w" %)))))

(defcheck solution-be572d54
  (comp {\I 14 \C 827 \M 3999 \L 48} second))

(defcheck solution-be89ad72
  (fn [s]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          n (vec (map m s))]
      (apply + (map
                 (fn [i]
                   (if
                    (or
                     (= (inc i) (count n))
                     (>= (nth n i) (nth n (inc i))))
                     (nth n i)
                     (- (nth n i))))
                 (range (count n)))))))

(defcheck solution-be9baf6
  (fn
    [t]
    (let [v {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (letfn [(d [t p n]
                (if (empty? t)
                  n
                  (let [c (v (first t))]
                    (if (> c p)
                      (d (rest t) c (+ n (* -2 p) c))
                      (d (rest t) c (+ n c))))))]
        (d t 0 0)))))

(defcheck solution-bebddf74
  (fn [r] (let [vs {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 \Z 0}]
            (apply + (map (fn [[f s]] (if (< f s) (- f) f))
                       (partition 2 1 (map vs (str r "Z"))))))))

(defcheck solution-bf53ddb1
  (fn [n] (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 \o 10000}
                s (map m (reverse (str "o" n)))]
            (reduce + (map #(if (< %2 %) (- % (* 2 %2)) %) s (rest s))))))

(defcheck solution-bf6c045d
  (fn rn [s]
    (let [m {"I" 1
             "V" 5
             "X" 10
             "L" 50
             "C" 100
             "D" 500
             "M" 1000
             "4" 4
             "9" 9
             "f" 40
             "n" 90
             "F" 400
             "N" 900}]
      (-> s
        (clojure.string/replace "IV" "4")
        (clojure.string/replace "IX" "9")
        (clojure.string/replace "XL" "f")
        (clojure.string/replace "XC" "n")
        (clojure.string/replace "CD" "F")
        (clojure.string/replace "CM" "N")
        (clojure.string/split #"")
        ((partial remove #{""}))
        ((partial map m))
        ((partial apply +))))))

(defcheck solution-bfb9c7b1
  (letfn
   [(roman-val [str]
      ({"M" 1000, "CM" 900, "D" 500, "XC" 90,
        "C" 100, "XL" 40, "L" 50, "X" 10, "IX" 9, "V" 5,
        "IV" 4, "I" 1} str))]
    #(reduce + (map roman-val (re-seq #"M|CM|D|XC|C|XL|L|IX|X|V|IV|I" %)))))

(defcheck solution-c0804b15
  (fn[rnum]
    (let [nums (map (fn[n](get {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} n)) rnum)]
      ((fn mknum[acc prev lst]
         (if (nil? (first lst)) acc
                                (if (empty? lst) acc
                                                 (let [curr (first lst) rem (rest lst)]
                                                   (cond
                                                     (nil? prev) (mknum (+ acc curr) curr rem)
                                                     (> curr prev) (mknum (+ (- acc (* 2 prev)) curr) curr rem)
                                                     :else (mknum (+ acc curr) curr rem)))))) 0 nil nums))))

(defcheck solution-c110d21d
  (fn roman- [string]
    ^{:doc "92. Write a function to parse a Roman-numeral string and
  return the number it represents."}
    (loop [[x & xs] (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (reverse string))
           mx 0
           acc 0]
      (if-not x
        acc
        (recur xs (max mx x) (if (<= mx x) (+ acc x) (- acc x)))))))

(defcheck solution-c1152bbe
  (fn [s]
    (let [symbols (seq s)
          symbolPairs (partition 2 1 symbols)]
      (letfn [(valOfTuple [a b] (case [a b]
                                  [\I \V] -1
                                  [\I \X] -1
                                  [\X \L] -10
                                  [\X \C] -10
                                  [\C \D] -100
                                  [\C \M] -100
                                  (valOf a)))
              (valOf [a] (case a
                           \I 1
                           \V 5
                           \X 10
                           \L 50
                           \C 100
                           \D 500
                           \M 1000
                           nil))
              (reducer [sum [a b]] (+ (valOfTuple a b) sum))]
        (reduce reducer (valOf (last symbols)) symbolPairs)
        )
      )
    ))

(defcheck solution-c16d7024
  (fn [x]
    (let [val-map {"M" 1000, "CM" 900, "D" 500, "CD" 400, "C" 100, "XC" 90, "L" 50, "XL" 40, "X" 10, "IX" 9, "V" 5, "IV" 4, "I" 1}
          roman-re #"(?:C?M)|(?:C?D)|(?:X?C)|(?:X?L)|(?:I?X)|(?:I?V)|(?:I)"]
      (reduce + (map val-map (re-seq roman-re x))))))

(defcheck solution-c18b76c8
  (fn [n]
    (let [val-map {\I {:val 1 :modifies [\V \X]}
                   \V {:val 5 :modifies []}
                   \X {:val 10 :modifies [\L \C]}
                   \L {:val 50 :modifies []}
                   \C {:val 100 :modifies [\D \M]}
                   \D {:val 500 :modifies []}
                   \M {:val 1000 :modifies []}}
          numerals (seq n)]
      (reduce +  (map second (reduce (fn [xs i]
                                       (let [last-pair (last xs)
                                             last-numeral (first last-pair)]
                                         (if (and last-numeral
                                                  (some #{i} (:modifies (get val-map last-numeral))))
                                           (conj xs [i
                                                     (- (:val (get val-map i)) (* 2 (:val (get val-map last-numeral))))])
                                           (conj xs [i (:val (get val-map i))])
                                           )
                                         ))
                               []
                               numerals)))
      )))

(defcheck solution-c1fc9f30
  #(reduce (fn [total [this nxt]]
             (let [values {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
                   thisv (get values this)
                   nextv (get values nxt)]
               (cond (not nxt) (+ total thisv)
                     (< thisv nextv) (- total thisv)
                     :default (+ total thisv))))
     0
     (partition-all 2 1 %)))

(defcheck solution-c2f3a16d
  (fn [n]
    (loop [ns (seq n) sum 0]
      (if (empty? ns) sum
                      (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
                            i (m (first ns))]
                        (if (some #(> (m %) i) (rest ns))
                          (recur (rest ns) (- sum i))
                          (recur (rest ns) (+ sum i))))))))

(defcheck solution-c2fede5c
  (fn [val roman]
    (if (empty? roman)
      val
      (cond
        (.startsWith roman "CM") (recur (+ 900 val) (subs roman 2))
        (.startsWith roman "CD") (recur (+ 400 val) (subs roman 2))
        (.startsWith roman "XC") (recur (+ 90 val) (subs roman 2))
        (.startsWith roman "XL") (recur (+ 40 val) (subs roman 2))
        (.startsWith roman "IX") (recur (+ 9 val) (subs roman 2))
        (.startsWith roman "IV") (recur (+ 4 val) (subs roman 2))
        (= (first roman) \M) (recur (+ 1000 val) (subs roman 1))
        (= (first roman) \D) (recur (+ 500 val) (subs roman 1))
        (= (first roman) \C) (recur (+ 100 val) (subs roman 1))
        (= (first roman) \L) (recur (+ 50 val) (subs roman 1))
        (= (first roman) \X) (recur (+ 10 val) (subs roman 1))
        (= (first roman) \V) (recur (+ 5 val) (subs roman 1))
        (= (first roman) \I) (recur (+ 1 val) (subs roman 1))))) 0)

(defcheck solution-c380010c
  (fn f ([src pre temp]
         (let [table {nil 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
           (cond (empty? src) (+ temp (table pre))
                 (nil? pre) (recur (rest src) (first src) temp)
                 (< (table pre) (table (first src))) (recur (rest src) nil (+ temp (- (table (first src)) (table pre))))
                 :else (recur (rest src) (first src) (+ temp (table pre))))))
    ([src]
     (f src nil 0))))

(defcheck solution-c4de3c58
  (fn [s]

    (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          levels {\I 1, \V 2, \X 3, \L 4, \C 5, \D 6, \M 7}]

      (letfn [(is-substractive [seg] (and (= 2 (count seg))
                                          (< (second (first seg))
                                            (second (second seg))
                                            )))
              (comp-substractive [seg] (- (nums (first (second seg)))
                                          (nums (first (first  seg)))
                                          ))
              (add-together [seg] (reduce + (map #(nums (first %)) seg)))
              (parse-segment [seg] (if (is-substractive seg)
                                     (comp-substractive seg)
                                     (add-together seg)))]

        (let [seqs (seq s)
              slevels (map levels seqs)
              compounds (map vector seqs slevels)

              taketwo (map vector slevels (conj (vec (rest slevels)) -1))
              dropping (map #(> (first %) (second %)) taketwo)
              dropping-wind (map-indexed vector dropping)
              dropping-ind (filter #(second %) dropping-wind)

              prev-seg-end (cons -1 (map first (drop-last dropping-ind)))
              seg-length (map - (map first dropping-ind) prev-seg-end)
              seg-drop (map inc prev-seg-end)
              seg-profiles (map vector seg-drop seg-length)
              segs (map #(take (second %) (drop (first %) compounds)) seg-profiles)

              values (map parse-segment segs)]

          (reduce + values)
          )))))

(defcheck solution-c5a6f9a9
  (fn readr
    [n & maxseen]
    (if (= "" n) 0
                 (let [values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
                       mx (if (nil? maxseen) 0 (first maxseen))
                       num (values (last n))
                       to-add (if (<= mx num) num (- 0 num))
                       m (max to-add mx)]
                   (+ to-add (readr (apply str (butlast n)) m))))))

(defcheck solution-c5b5a8f2
  (fn [s]
    (letfn
     [
      (tocounts [l] (reverse (map { \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000 } l)))
      (convert [l] ( loop [total 0 xs l]
                     ;(println total xs)
                     (if (empty? xs)
                       total
                       (let [item (first xs)
                             rem (rest xs)
                             minus (take-while #(< %1 item) rem)
                             remains (drop-while #(< %1 item) rem)]
                         (recur (- (+ total item) (convert minus)) remains)))
                     )
        )
      ]
      (convert (tocounts s)))))

(defcheck solution-c5c9d217
  (fn [s]
    (let [schema {"I"  1
                  "IV" 4
                  "V"  5
                  "IX" 9
                  "X"  10
                  "XL" 40
                  "L"  50
                  "XC" 90
                  "C"  100
                  "CD" 400
                  "D"  500
                  "CM" 900
                  "M"  1000}]
      (loop [acc 0 roman s]
        (if (empty? roman)
          acc
          (let [h1 (str (first roman))
                h2 (str (second roman))
                hh (str h1 h2)]
            (if (= h2 hh)
              (+ acc (schema hh))
              (if (contains? schema hh)
                (recur (+ acc (schema hh)) (apply str (drop 2 roman)))
                (recur (+ acc (schema h1)) (apply str (drop 1 roman)))))))))))

(defcheck solution-c62d60ad
  (fn [roman]
    (let [r->d {\I 1, \V 5, \X 10, \L 50, \C  100, \D 500, \M 1000}]
      (loop [roman  (vec (map #(r->d %) roman))
             prev 0
             sum  0]
        (if (empty? roman) sum
                           (let [curr (peek roman)
                                 op (if (< curr prev) - +)]
                             (recur (pop roman) curr (op sum curr))))))))

(defcheck solution-c6609450
  (fn [s]
    (let [k {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          hw (fn [[high total] c]
               (let [n (k c)]
                 (vector (max n high)
                   (if (< n high)
                     (- total n)
                     (+ total n)))))]
      (second (reduce hw [0 0] (reverse s))))))

(defcheck solution-c677fc3b
  (fn [s]
    (let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (
       (reduce
         #(let [n (m %2) l (%1 :last) s (%1 :sum)]
            (if (and (not (nil? l)) (< n l))
              (assoc %1 :sum (- s n) :last nil)
              (assoc %1 :sum (+ s n) :last n)
              )
            )
         {:sum 0 :last nil}
         (reverse s)
         ) :sum
       )
      )
    ))

(defcheck solution-c7290792
  (fn [rom-num]
    (apply +
      ((fn subnot [ns]
         (if (empty? ns)
           ()
           (let [x1 (first ns)]
             (if (empty? (rest ns))
               (cons x1 nil)
               (let [x2 (second ns)]
                 (if (> x2 x1)
                   (cons (- x2 x1) (subnot (drop 2 ns)))
                   (cons x1 (subnot (rest ns)))))))))
       (map
         (fn [c]
           (case c
             \M 1000  \D  500  \C  100  \L   50
             \X   10  \V    5  \I    1  0))
         rom-num)))))

(defcheck solution-c760b58d
  (fn [r]
    (let [roman {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}]
      (reduce + (map roman (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" r))))))

(defcheck solution-c7a0f810
  (fn roman [rn]
    (let [rs {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (letfn [(cnt1? [s] (empty? (rest s)))
              (a [st] (get rs st))
              (parse [rns](loop [x (butlast rns) sum (a (last rns)) lst (a (last rns))]
                            (if (empty? x) sum
                                           (recur (butlast x)
                                             (if (>= (a(last x)) lst) (+ sum (a(last x))) (- sum (a(last x))))
                                             (a(last x)))
                                           )
                            )
                )
              ]
        (apply +(map parse (reduce #(if(and(not(empty? %))
                                           (cnt1?(last %))
                                           (or(<(a(first(last %))) (a(first %2)))
                                              (<(quot (a(first(last %)))(a(first %2))) 10)))
                                      (conj (vec(butlast %)) (concat (last %) %2))
                                      (conj % %2)
                                      )
                             [] (partition-by a rn)))
          )

        )
      )
    ))

(defcheck solution-c9258581
  (fn r2d [r']
    (let [t #{"IV" "IX" "XL" "CD" "CM" "XC"}
          m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [r r' acc 0]
        (let [f (first r)
              s (second r)
              fs (str f s)]
          (cond
            (nil? f) acc
            (t fs) (recur (rest r) (- acc (m f)))
            :else (recur (rest r) (+ acc (m f)))
            ))))))

(defcheck solution-c9c60eac
  (fn read-roman
    ([s]
     (read-roman s 0))
    ([s n]
     (let [f re-find
           r clojure.string/replace-first]
       (cond
         (f #"IV" s) (read-roman (r s #"IV" "") (+ n 4))
         (f #"IX" s) (read-roman (r s #"IX" "") (+ n 9))
         (f #"XL" s) (read-roman (r s #"XL" "") (+ n 40))
         (f #"XC" s) (read-roman (r s #"XC" "") (+ n 90))
         (f #"CD" s) (read-roman (r s #"CD" "") (+ n 400))
         (f #"CM" s) (read-roman (r s #"CM" "") (+ n 900))
         (f #"I" s) (read-roman (r s #"I" "") (+ n 1))
         (f #"V" s) (read-roman (r s #"V" "") (+ n 5))
         (f #"X" s) (read-roman (r s #"X" "") (+ n 10))
         (f #"L" s) (read-roman (r s #"L" "") (+ n 50))
         (f #"C" s) (read-roman (r s #"C" "") (+ n 100))
         (f #"D" s) (read-roman (r s #"D" "") (+ n 500))
         (f #"M" s) (read-roman (r s #"M" "") (+ n 1000))
         :else n)))))

(defcheck solution-caa91f87
  (fn [x]
    (let [ks ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
          vs [1000 900 500 400 100 90 50 40 10 9 5 4 1]
          roman (zipmap ks vs)
          pattern (re-pattern (apply str (interpose "|" ks)))]
      (apply + (map roman (re-seq pattern x))))))

(defcheck solution-cb257909
  (fn [s] (let [num-s (map #(condp = %1
                              \I 1
                              \V 5
                              \X 10
                              \L 50
                              \C 100
                              \D 500
                              \M 1000) s)]
            (+ (reduce #((if (< (first %2) (second %2)) - +) %1 (first %2)) 0 (map list num-s (rest num-s))) (last num-s)))))

(defcheck solution-cb2e71ed
  (fn [rn]
    (letfn [(_ [acc rn]
              (if-not (seq rn)
                acc
                (cond
                  (re-seq #"^M+" rn) (_ (+ acc (* 1000 (count (take-while #(= \M %) rn)))) (apply str (drop-while #(= \M %) rn)))
                  (re-seq #"^CD" rn) (_ (+ acc 400) (apply str (drop 2 rn)))
                  (re-seq #"^CM" rn) (_ (+ acc 900) (apply str (drop 2 rn)))
                  (re-seq #"^C+" rn) (_ (+ acc (* 100 (count (take-while #(= \C %) rn)))) (apply str (drop-while #(= \C %) rn)))
                  (re-seq #"^D+" rn) (_ (+ acc 500 (* 100 (count (take-while #(= \C %) (rest rn))))) (apply str (drop-while #(= \C %) (rest rn))))
                  (re-seq #"^XL" rn) (_ (+ acc 40) (apply str (drop 2 rn)))
                  (re-seq #"^XC" rn) (_ (+ acc 90) (apply str (drop 2 rn)))
                  (re-seq #"^X+" rn) (_ (+ acc (* 10 (count (take-while #(= \X %) rn)))) (apply str (drop-while #(= \X %) rn)))
                  (re-seq #"^L+" rn) (_ (+ acc 50 (* 10 (count (take-while #(= \X %) (rest rn))))) (apply str (drop-while #(= \X %) (rest rn))))
                  (re-seq #"^IV" rn) (_ (+ acc 4) (apply str (drop 2 rn)))
                  (re-seq #"^IX" rn) (_ (+ acc 9) (apply str (drop 2 rn)))
                  (re-seq #"^I+" rn) (_ (+ acc (* 1 (count (take-while #(= \I %) rn)))) (apply str (drop-while #(= \I %) rn)))
                  (re-seq #"^V+" rn) (_ (+ acc 5 (* 1 (count (take-while #(= \I %) (rest rn))))) (apply str (drop-while #(= \I %) (rest rn))))
                  :else 0)))]
      (_ 0 rn))))

(defcheck solution-cb4abde3
  (fn [s]
    (let [p2
          (fn p2 [b s]
            (if (zero? (count s))
              0
              (let [f (first s) r (rest s)]
                (+ (cond
                     (= f \M) (if (= b \C) 800 1000)
                     (= f \D) (if (= b \C) 300 500)
                     (= f \C) (if (= b \X)  80 100)
                     (= f \L) (if (= b \X)  30  50)
                     (= f \X) (if (= b \I)   8  10)
                     (= f \V) (if (= b \I)   3   5)
                     (= f \I) 1)
                  (p2 f r)))))]
      (p2 nil s))))

(defcheck solution-cb6bf174
  (fn readrom [s]
    (letfn [(num->digits [num]
              (loop [n num digits []]
                (if (< n 10)
                  (cons (int n) digits)
                  (recur (quot n 10) (cons (int (rem n 10)) digits)))))
            (writerom
              ([n] (writerom n []))
              ([n o]
               (let [dig (num->digits n)
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
                       (into o (f (first dig) "X" "V" "I")))))))]
      (first
        (filter number?
          (map #(if (= s (writerom %)) %)
            (range 4000)))))))

(defcheck solution-cb96027f
  #(reduce + (map (fn [[x y]] (if y (if (>= x y) x (- x)) x)) (partition-all 2 1 (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %)))))

(defcheck solution-cc0a882d
  (fn readRN ([s] (readRN 0 (map str(flatten (partition 1 s)))))
    ([v s] (let [numerals {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
             (case (count s)
               0 v
               1 (+ v (numerals (first s)))
               (if (< (numerals (first s)) (numerals (second s)))
                 (readRN (+ v (- (numerals (second s)) (numerals (first s)))) (rest (rest s)))
                 (readRN (+ v (numerals (first s))) (rest s))))))))

(defcheck solution-cc0b4a1
  (fn [r](
           letfn [
                  (rn[n] (get {"X" 10 "I" 1 "V" 5 "M" 1000 "C" 100 "L" 50 "D" 500} n))
                  (fa[rx val lv] (
                                   if (empty? rx) val
                                                  (
                                                   #(if (< % lv)
                                                      (fa (rest rx) (- val %) %)
                                                      (fa (rest rx) (+ val %) %)
                                                      ) (rn (str (first rx)))
                                                   )
                                                  ))

                  ]
           (fa (reverse r) 0 0)
           )))

(defcheck solution-cc89d6fa
  (fn r [[c & s :as i]]
    (let [t {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (cond
        (empty? i) 0
        (empty? s) (t c)
        (< (t c) (t (first s))) (- (r s) (t c))
        true (+ (r s) (t c))))))

(defcheck solution-cc8d8f93
  (fn [n]
    (let [r->d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [[head & tail] n
             prev 0
             gr 0
             acc 0]
        (if head
          (let [digit (get r->d head)]
            (cond
              (> digit prev) (recur tail digit (- digit gr) acc)
              (= digit prev) (recur tail digit (+ digit gr) acc)
              :else (recur tail digit digit (+ acc gr))))
          (+ acc gr))))))

(defcheck solution-ccce4a67
  (fn [a](let [d {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10
                  "XL" 40  "L" 50 "XC" 90 "C" 100
                  "CD" 400 "D" 500 "CM" 900 "M" 1000}]
           (loop [r a
                  s 0]
             (if-not (empty? r)
               (if (and (<= 2 (count r)) (contains? d (subs r 0 2)))
                 (recur (subs r 2) (+ s (get d (subs r 0 2))))
                 (recur (subs r 1) (+ s (get d (subs r 0 1)))))
               s)))))

(defcheck solution-ccd80974
  (fn [x]
    (let [table (array-map "M" 1000 "CM" 900
                  "D"  500 "CD" 400
                  "C"  100 "XC"  90
                  "L"   50 "XL"  40
                  "X"   10 "IX"   9
                  "V"    5 "IV"   4
                  "I"    1)
          pattern (->> (keys table)
                    (interpose "|")
                    (apply str)
                    re-pattern)]
      (->> x
        (re-seq pattern)
        (map table)
        (reduce +)))))

(defcheck solution-ce061637
  (fn read_roman_numerals [roman-string]
    (let [number-table {\I 1
                        \V 5
                        \X 10
                        \L 50
                        \C 100
                        \D 500
                        \M 1000}]
      (loop [accumulator 0
             chars roman-string
             last-char nil]
        (if (empty? chars)
          accumulator
          (if (and (not (nil? last-char))
                   (< (get number-table last-char)
                     (get number-table (first chars))))
            (recur (- (+ accumulator (get number-table (first chars))) (* 2 (get number-table last-char)))
              (rest chars)
              (first chars))
            (recur (+ accumulator (get number-table (first chars))) (rest chars) (first chars))))))))

(defcheck solution-ce80082d
  (fn rrn [s]
    (let [rd {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (reduce (fn [acc t] (do #_(println acc " : " t) (if (> t acc) (+ acc t) (- acc t)))) 0
        (reverse (map #(* (count %) (rd (first %))) (partition-by identity  (seq s)))))
      )
    ))

(defcheck solution-ce8b848d
  (fn [str]
    (first (let [vals (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} str)]
             (reduce (fn [[total prev] n] [(if (< prev n) (- (+ total n) (* 2 prev)) (+ total n)) n])
               [0 0] vals)))))

(defcheck solution-cfaef9d4
  (fn f [[a b & c]]
    (if a
      (let [m (zipmap
                (interleave "MDCLXVI" ["CM" "CD" "XC" "XL" "IX" "IV" 0])
                [1000 900 500 400 100 90 50 40 10 9 5 4 1])]
        (if (m (str a b))
          (+ (m (str a b)) (f c))
          (+ (m a) (f (cons b c)))))
      0)))

(defcheck solution-d0543913
  (fn rrn [s]
    (let[roman-number-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (apply + (map #(let[a (roman-number-map %1)
                          b (roman-number-map %2)]
                       (if(>= a b)
                         a
                         (- a)))
                 (seq s)
                 (concat (rest (seq s)) '(\I)))))))

(defcheck solution-d0884849
  (fn decimal [s]
    (let [M {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900
             "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (reduce + (map M (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" s))))))

(defcheck solution-d13a5a20
  (fn read-roman-numberals [s]
    (let [map-of-roman-numberals {:I 1 :V 5 :X 10 :L 50
                                  :C 100 :D 500 :M 1000}
          numberals-seq (map #(map-of-roman-numberals (keyword (str %))) s)]
      (second (reduce (fn [[pre result] x]
                        [x (+ result
                             (if (< pre x)
                               (- x (* 2 pre))
                               x))])
                [(first numberals-seq) (first numberals-seq)] (rest numberals-seq))))))

(defcheck solution-d1c93371
  #(reduce (fn [s [f n]]
             (let [r {\I 1
                      \V 5
                      \X 10
                      \L 50
                      \C 100
                      \D 500
                      \M 1000}
                   d (r f)]
               ((if (< d (r n)) - +) s d))) 0 (partition 2 1 [\I] %)))

(defcheck solution-d2395a2c
  #(letfn [(f [s]
             (let [m {\I [\V \X 1]
                      \X [\L \C 10]
                      \C [\D \M 100]}
                   m2 {\V 5 \L 50 \D 500 \M 1000}
                   fs (first s)
                   mfs (m fs)]
               (cond
                 (empty? s) 0
                 (some (partial = fs) (keys m)) (cond
                                                  (= (second s) (mfs 0)) (+ (* 4 (mfs 2)) (f (drop 2 s)))
                                                  (= (second s) (mfs 1)) (+ (* 9 (mfs 2)) (f (drop 2 s)))
                                                  :else (+ (* 1 (mfs 2)) (f (rest s))))
                 (some (partial = fs) (keys m2)) (+ (m2 fs) (f (rest s))))))]
     (f (seq %))))

(defcheck solution-d2c810f3
  (fn [s] (let [
                n {\C 100, \D 500, \I 1, \L 50, \M 1000, \V 5, \X 10}
                t (concat s "I")]
            (reduce
              #(let [[c1 c2] %2]
                 (if (< (n c1) (n c2))
                   (- %1 (n c1))
                   (+ %1 (n c1)) ))
              0
              (map vector t (rest t))))))

(defcheck solution-d30c9103
  #(reduce (fn [c [a b]] (+ c (if (> b a) (- a) a))) 0
     (partition 2 1 [0] (map {\C 100 \D 500 \I 1 \L 50 \M 1000 \V 5 \X 10} %))))

(defcheck solution-d3419e5c
  (fn read-roman-numerals [rn]
    (let [numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          pairs (partition 2 1 (concat (map numerals rn) [0]))]
      (reduce (fn [acc [a b]] ((if (>= a b) + -) acc a)) 0 pairs))))

(defcheck solution-d34bae65
  (fn [st]
    (let [mp {\M 1000, \D 500, \C 100, \L 50,
              \X 10, \V 5, \I 1}]
      (letfn [(roman [lis]
                (cond
                  (empty? lis) 0
                  (empty? (rest lis)) (first lis)
                  (< (first lis) (second lis))
                  (+ (- (second lis) (first lis))
                    (roman (drop 2 lis)))
                  :else (+ (first lis)
                          (roman (rest lis)))))]
        (roman (map (partial get mp) (seq st)))))))

(defcheck solution-d39855f8
  (let [M {\I [   1 #{\V \X}]
           \V [   5 #{}]
           \X [  10 #{\L \C}]
           \L [  50 #{}]
           \C [ 100 #{\D \M}]
           \D [ 500 #{}]
           \M [1000 #{}]}
        V (zipmap (keys M) (map first  (vals M)))
        S (zipmap (keys M) (map second (vals M)))]
    (fn [s]
      (->> s
        (partition-all 2 1)
        (map (fn [[p n]] ((if ((S p) n) - +) (V p))))
        (reduce + 0)))))

(defcheck solution-d3da14ee
  (fn roman [s]
    (let [numericals [["CM" 900]
                      ["CD" 400]
                      ["XC" 90]
                      ["XL" 40]
                      ["IX" 9]
                      ["IV" 4]
                      ["M" 1000]
                      ["D" 500]
                      ["C" 100]
                      ["L" 50]
                      ["X" 10]
                      ["V" 5]
                      ["I" 1]]
          next-number (fn [s] (->> numericals (filter #(.endsWith ^String s (first %))) first))]
      (loop [n 0 s s]
        (if (empty? s)
          n
          (let [[k v] (next-number s)]
            (recur (+ n v) (subs s 0 (- (count s) (count k))))))))))

(defcheck solution-d445185e
  (fn [s]
    (let [romans {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (:num (reduce (fn [acc x]
                      (let [dx (get romans x)
                            op (if (> (:last acc) dx) - +)]
                        (assoc (update-in acc [:num] op dx)
                          :last dx)))
              {:num 0 :last 0}
              (reverse s))))))

(defcheck solution-d49b882c
  (fn [n] (let [p (into [] (partition-by identity n))
                d (atom #{})
                r {nil 0, "IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 400, "CM" 900, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} ]

            (reduce #(+ % (r %2)) 0
              (flatten (map-indexed
                         (fn [i v] (cond (> (count v) 1) v
                                         (not (contains? @d i) )
                                         (let [s (str (first v) (first (get p (inc i))))]
                                           (if (r s)
                                             (do (swap! d conj (inc i)) s)
                                             v) )))
                         p )) ) )))

(defcheck solution-d514523a
  #(let [m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [S 0 s (seq %1)]
       (if s
         (let [[a b & c] s
               A (m a 0)
               B (m b 0)]
           (if (> B A)
             (recur (+ S (- B A)) c)
             (recur (+ S A) (next s))))
         S))))

(defcheck solution-d5919402
  (fn [x]
    (let [nums {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
      (->> x
        (re-seq #".")
        (map nums)
        (vec)
        (#(conj % 0))
        (partition 2 1)
        (map (fn [[a b]] (if (>= a b) a (- a))))
        (reduce +)))))

(defcheck solution-d5c1194b
  (fn [x]
    (let [roman {\I 1
                 \V 5
                 \X 10
                 \L 50
                 \C 100
                 \D 500
                 \M 1000}]
      (reduce
        (fn [v [d & rest]]
          (if (some #(> (roman %) (roman d)) rest)
            (- v (roman d))
            (+ v (roman d))))
        0
        (take-while #(not (nil? %))
          (iterate next (next (list* nil x))))))))

(defcheck solution-d5ff0734
  (fn [n]
    (let [conv {\I 1, \V 5, \X 10, \L 50 \C 100,\D 500, \M 1000}
          go (fn [[acc prev] x]
               (let [nxt (conv x)]
                 [(+ acc nxt (if (> nxt prev) (- (* 2 prev)) 0)) nxt]))]
      (first (reduce go [0 1000] n)))))

(defcheck solution-d60a3e0
  (fn q92 [s]
    (let [ r {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} ]
      (->>
        (map r s)
        (partition 2 1 [0])
        (map (fn [[a b]] (if (>= a b) a (* -1 a))))
        (reduce +)))))

(defcheck solution-d66380bd
  (fn [str] (let [n {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
                  [v & more] (map n (seq str))
                  vals (reduce #(if (< (first %) %2)
                                  (conj (rest %) (- %2 (first %)))
                                  (conj % %2))
                         (list v) more)]
              (reduce + vals))))

(defcheck solution-d6936508
  (fn roman-numeral [n] (let
                         [char-to-num
                          {\I 1
                           \V 5
                           \X 10
                           \L 50
                           \C 100
                           \D 500
                           \M 1000}]
                          (loop [nxt (first n) cur nil rst (rest n) sum 0]
                            (cond
                              (nil? cur) (recur (first rst) nxt (rest rst) sum)
                              (nil? nxt) (+ sum (char-to-num cur))
                              :else (let [curnum (char-to-num cur) nxtnum (char-to-num nxt)]
                                      (if (< curnum nxtnum) (recur (first rst) nxt (rest rst) (- sum curnum))
                                                            (recur (first rst) nxt (rest rst) (+ sum curnum))
                                                            )
                                      )
                              )
                            )
                          )))

(defcheck solution-d6978cf5
  (fn __ [[x & [y & ys :as xs] :as text]]
    (let [ranks (zipmap "IVXLCDM" '(1 5 10 50 100 500 1000))]
      (cond
        (empty? text) 0
        (or (nil? xs) (empty? xs)) (ranks x)
        :else (let [rx (ranks x)
                    ry (ranks y)]
                (if (< rx ry)
                  (+ (- ry rx) (__ ys))
                  (+ rx (__ xs))))))))

(defcheck solution-d7233dd5
  (fn [x] (let [map-def [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400] ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10] ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
            (loop [cur 0
                   sequence x]
              (if (= "" sequence)
                cur
                (let [next-match (reduce #(if (nil? %1)
                                            (if (.startsWith sequence (first %2))
                                              %2
                                              nil)
                                            %1) nil map-def)]
                  (recur (+ cur (last next-match)) (subs sequence (count (first next-match))))
                  ))))))

(defcheck solution-d74406ba
  (fn rom-to-dec
    [s]

    (let [rom-to-dec-dig (fn rom-to-dec-dig
                           [r]
                           (cond
                             (= r \M) 1000
                             (= r \D)  500
                             (= r \C)  100
                             (= r \L)   50
                             (= r \X)   10
                             (= r \V)    5
                             (= r \I)    1))

          rom-acc (fn rom-acc
                    ([]             [0 0 0])

                    ([val]          [0 val val])

                    ([oacc rhs] (let [[tot-sum clp-sum lhs] oacc]
                                  (cond
                                    (= lhs rhs) [tot-sum (+ clp-sum rhs) rhs]
                                    (< lhs rhs)  [(- tot-sum clp-sum) rhs rhs]
                                    :else        [(+ tot-sum clp-sum) rhs rhs]))))

          rom-val (fn rom-val
                    [oacc]
                    (let [[tot-sum clp-sum olhs] oacc]
                      (+ tot-sum clp-sum)))

          acc-reduce (fn acc-reduce
                       [f-acc f-val in-xs]
                       (let [xs (seq in-xs)]
                         (if-not xs
                           (f-val (f-acc))
                           (f-val (reduce f-acc (f-acc (first xs)) (rest xs))))))

          ]

      (let [dig-vals (map #(rom-to-dec-dig %) (seq s))]
        (acc-reduce rom-acc rom-val dig-vals)))))

(defcheck solution-d7a3125e
  (fn from-roman-numerals [s]
    (-> s
      (clojure.string/replace #"CM" (apply str (repeat 900 ".")))
      (clojure.string/replace #"M"  (apply str (repeat 1000 ".")))
      (clojure.string/replace #"CD" (apply str (repeat 400 ".")))
      (clojure.string/replace #"D"  (apply str (repeat 500 ".")))
      (clojure.string/replace #"XC" (apply str (repeat 90 ".")))
      (clojure.string/replace #"C"  (apply str (repeat 100 ".")))
      (clojure.string/replace #"XL" (apply str (repeat 40 ".")))
      (clojure.string/replace #"L"  (apply str (repeat 50 ".")))
      (clojure.string/replace #"IX" (apply str (repeat 9 ".")))
      (clojure.string/replace #"X"  (apply str (repeat 10 ".")))
      (clojure.string/replace #"IV" (apply str (repeat 4 ".")))
      (clojure.string/replace #"V"  (apply str (repeat 5 ".")))
      (count))))

(defcheck solution-d7ac3371
  (fn read-roman [nums]
    (let [table [["M"  1000]
                 ["CM"  900]
                 ["D"   500]
                 ["CD"  400]
                 ["C"   100]
                 ["XC"   90]
                 ["L"    50]
                 ["XL"   40]
                 ["X"    10]
                 ["IX"    9]
                 ["V"     5]
                 ["IV"    4]
                 ["I"     1]]
          starts-with?
                (fn starts-with? [str start]
                  (if (empty? start) true
                                     (if (empty? str) false
                                                      (if-not (= (first str) (first start)) false
                                                                                            (starts-with? (rest str) (rest start))))))]
      (if (empty? nums) 0
                        (first
                          (remove nil?
                            (for [[st v] table]
                              (if (starts-with? nums st)
                                (+ v (read-roman (drop (count st) nums)))
                                nil))))))))

(defcheck solution-d7ad34f8
  (fn to-arabic [s]
    (let [numerals {"" 0 "I" 1, "IV" 4, "IX", 9, "V" 5, "X" 10, "XL" 40, "XC" 90,
                    "L" 50, "C" 100, "CD" 400, "CM" 900, "D" 500, "M" 1000}
          p-n (fn [sum-hold n]
                (let [i-sum (first sum-hold)
                      i-hold (last sum-hold)
                      n-str (str i-hold n)]
                  (condp contains? n-str
                    #{"I" "X" "C"} [i-sum n-str]
                    #{"II" "XX" "CC"} [(+ i-sum (get numerals (str i-hold))) n]
                    #{"IV" "IX" "XL" "XC" "CD" "CM"}
                    [(+ i-sum (get numerals n-str)) nil]
                    [(+ i-sum (get numerals (str i-hold))) n])))]
      (let [pair (reduce p-n [0 nil] s)]
        (+ (first pair) (get numerals (str (last pair))))))))

(defcheck solution-d8d4435
  (fn [s]
    (let [lu  (array-map \M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1)]
      (reduce
        (fn ya [sum [a b]]
          (cond
            (nil? b) (+ sum a)
            (< a b) (- sum a)
            :else (+ sum a)
            ))
        0
        (partition 2 1 [nil] (map #(lu %) s))))))

(defcheck solution-d9027c34
  (fn [[h & t]]
    (let [f {"I" 1, "II" 2, "III" 3, "IV" 4, "V" 5, "VI" 6, "VII" 7, "VIII" 8, "IX" 9, "X" 10, "XX" 20, "XXX" 30, "XL" 40, "L" 50, "LX" 60, "LXX" 70, "LXXX" 80, "XC" 90, "C" 100, "CC" 200, "CCC" 300, "CD" 400, "D" 500, "DC" 600, "DCC" 700, "DCCC" 800, "CM" 900, "M" 1000, "MM" 2000, "MMM" 3000}
          g {\I #{\I \V \X}, \X #{\X \L \C}, \C #{\C \D \M} \V #{\I}, \L #{\X}, \D #{\C}, \M #{\M}}]
      (loop [r 0, p (str h), [a & b :as s] t]
        (if (last p)
          (if ((g (last p)) a)
            (recur r (str p a) b)
            (recur (+ r (f p)) (str a) b))
          r)))))

(defcheck solution-d96db8b8
  (fn [s]
    (let [nummap {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 }
          effectiv-val (fn [[n1 n2]] (if (< n1 n2) (- n1) n1))]
      (->> s
        (map nummap)
        (into [])
        (#(conj % 0))
        (partition 2 1)
        (map effectiv-val)
        (reduce +)))))

(defcheck solution-da6b44a9
  (fn [rn]
    (letfn [(is-larger [order a b]
              (loop [current a]
                (if (nil? current)
                  false
                  (if (or (= a b)
                          (= (get order current) b))
                    true
                    (recur (get order current))))))]
      ((fn decode [in order vals]
         (loop [total 0
                curr in
                last nil]
           (if (nil? curr)
             total
             (let [numeral (str (first curr))
                   t (+ total (get vals numeral))]
               (if (is-larger order last numeral)
                 (recur t (next curr) numeral)
                 (recur (- t (* 2 (get vals last))) (next curr) numeral))))))
       rn
       {"M" "D" "D" "C" "C" "L" "L" "X" "X" "V" "V" "I" "I" nil}
       {"M" 1000 "D" 500 "C" 100 "L" 50 "X" 10 "V" 5 "I" 1 nil 0}))))

(defcheck solution-da7630e5
  (fn [[h & t]]
    (let [v {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (first (reduce (fn [[res l] c]
                       (let [cv (get v c)]
                         [(+ res (if (< l cv) (+ (* -2 l) cv)  cv))
                          cv]))
               [(get v h) (get v h)]
               t)))))

(defcheck solution-dac832c9
  (fn [s]
    (let [nums {"I" 1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000}]
      (last (reduce #(conj [%2] (if (> %2 (first %1) 0)
                                  (+ (last %1) (- %2 (first %1) (first %1)))
                                  (+ (last %1) %2)))
              [0]
              (map #(nums (str %)) s))))))

(defcheck solution-db06fa02
  (fn [roman]
    (let [prefixes [
                    ["M" 1000]
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
                    ["I" 1]
                    ]]
      (loop [roman roman, n 0]
        (if (empty? roman) n
                           (let [[prefix val]
                                 (first (filter (fn [[prefix _]] (.startsWith roman prefix)) prefixes))]
                             (recur (.substring roman (count prefix)) (+ n val))))))))

(defcheck solution-db1c392e
  (fn me [my-str]


    (let [

          my-seq     (seq my-str)

          one-digits {"I" 1 "II" 2 "III" 3 "IV" 4 "V" 5 "VI" 6 "VII" 7 "VIII" 8 "IX" 9}

          two-digits {"X" 10 "XX" 20 "XXX" 30 "XL" 40 "L" 50 "LX" 60 "LXX" 70 "LXXX" 80 "XC" 90}

          three-digits {"C" 100 "CC" 200 "CCC" 300 "CD" 400 "D" 500 "DC" 600 "DCC" 700 "DCCC" 800 "CM" 900}



          f-thousands (fn [my-seq]

                        (let [ res (first (partition-by identity my-seq))]
                          (if (= (first res) (first (seq "M")))
                            (count res)
                            0
                            )
                          )
                        )

          my-thousands (f-thousands my-seq)

          no-thousands (drop my-thousands my-seq)

          f-hundreds (fn [my-seq my-map]

                       (let [ hundreds-seq [ (take 1 my-seq)
                                            (take 2 my-seq)
                                            (take 3 my-seq)
                                            (take 4 my-seq)]

                             ]

                         (last (sort-by #(first %)
                                 (filter #(not= nil (first %))
                                   (map #(vector (my-map %) %)
                                     (map #(apply str %) hundreds-seq)))))
                         )
                       )

          my-hundreds (f-hundreds no-thousands three-digits)

          num-threeDigits (if (nil? my-hundreds) 0 (count (second my-hundreds)))

          no-hundreds (drop num-threeDigits no-thousands)

          my-tens (f-hundreds no-hundreds two-digits)

          num-twoDigits (if (nil? my-tens) 0 (count (second my-tens)))

          no-tens (drop num-twoDigits no-hundreds)

          my-ones (f-hundreds no-tens one-digits)

          t (* 1000 my-thousands)

          h (if (nil? my-hundreds) 0 (first my-hundreds))

          ten (if (nil? my-tens) 0 (first my-tens))

          os  (if (nil? my-ones) 0 (first my-ones))

          ]

      (+ t h ten os)


      )

    ))

(defcheck solution-db7d3517
  (fn [r]
    (let [letter-values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (first
        (reduce
          (fn [[total prev-v] v]
            (if (> v prev-v)
              [(+ (- total prev-v) (- v prev-v)) v]
              [(+ total v)                       v]))
          [0 2147483647]
          (map (partial get letter-values) (seq r)))))))

(defcheck solution-dbda3e4f
  #(let [decoder {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (loop [x 0
            xs (map decoder %)
            result 0]
       (if (empty? xs)
         (+ result x)
         (recur (first xs) (rest xs) (if (>= x (first xs))
                                       (+ result x)
                                       (- result x))))

       )
     ))

(defcheck solution-dc2e7deb
  (fn [number]
    (loop [total 0
           digits (reverse number)
           largest-digit-seen 0]
      (if-not (seq digits)
        total
        (let [digit-value ({\I 1
                            \V 5
                            \X 10
                            \L 50
                            \C 100
                            \D 500
                            \M 1000} (first digits))]
          (if (>= digit-value largest-digit-seen)
            (recur (+ total digit-value) (rest digits) digit-value)
            (recur (- total digit-value) (rest digits) largest-digit-seen)))))))

(defcheck solution-dc3951eb
  (fn read-numerals [target]
    (let [numerals {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (loop [[x & xs] (reverse target) biggest 0 total 0]
        (let [c (numerals x)]
          (cond
            (nil? x) total
            (< c biggest) (recur xs biggest (- total c))
            :else (recur xs c (+ c total))))))))

(defcheck solution-dc963b6d
  (let [char-vals {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
    (fn [s] ((fn [s highest acc] (if (empty? s) acc
                                                (let [v (char-vals (first s))]
                                                  (recur (rest s) (max v highest) ((if (< v highest) - +) acc v)))))
             (reverse s) 0 0))))

(defcheck solution-dd5fd66d
  (fn parse-roman [s]
    (let [r2i (fn [d] ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} d))
          f (fn [[acc sub?] [rd, rd-next]] (if sub? [acc false] (let [d (r2i rd) d-next (r2i rd-next)] (if (and d-next (< d d-next)) [(+ acc (- d-next d)), true] [(+ acc d), false]))))]
      (first (reduce f [0 false] (partition-all 2 1 s))))))

(defcheck solution-dd978c44
  (fn roman [s]
    (letfn [(r->i [a b]
              (let  [r {\I 1 \V 5  \X 10 \L 50 \C 100 \D 500 \M 1000}]
                (cond
                  (not b) (get r a)
                  (< (get r a) (get r b)) (- (get r a))
                  :else (get r a))))]
      (reduce + (map (partial apply r->i) (partition 2 1 [\I] s))))))

(defcheck solution-de60eeb1
  (fn rom [r] (let [
                    rn (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
                    v (concat (map rn r) [0])
                    p (map list v (next v))
                    {pos false neg true} (group-by (fn [[a b]] (< a b)) p)
                    neg1 (map (fn [[a b]] [(- a) b]) neg)
                    ]
                (reduce #(+ % (first %2)) 0 (concat pos neg1)))))

(defcheck solution-de9b00e5
  (fn myf [rom-nums]
    (let [nums	(replace (zipmap "MDCLXVI" [1000 500 100 50 10 5 1]) rom-nums)
          nums	(map #(if (< %1 %2) (- %1) %1) nums (concat (next nums) [1]))]
      (apply + nums))))

(defcheck solution-df2c96a1
  (fn [x]
    (let
     [R {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (reduce +
        (map
          (partial reduce #(- (R %2) %1) 0)
          (re-seq #"IV|IX|XL|XC|XM|CD|CM|[IVXLCDM]" x))))))

(defcheck solution-e00a852f
  (fn from-roman [roman]
    (if (empty? roman)
      0
      (let [one-char {"I" 1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000}
            two-char {"IV"  4, "IX"	9, "XL"	40 "XC" 90 "CD"	400 "CM"	900}
            r-length (count roman)
            final-2 (apply str (drop (- r-length 2) roman))
            final (apply str (drop (- r-length 1) roman))
            ]
        (if (two-char final-2)
          (+ (two-char final-2)
            (from-roman (apply str (take (- r-length 2) roman))))
          (+ (one-char final)
            (from-roman (apply str (take (- r-length 1) roman)))))
        ))))

(defcheck solution-e01f9241
  (fn [romanum]
    (let [nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (loop [r (reverse romanum), prev 0, result 0]
        (if (empty? r)
          result
          (let [d (nums (first r))]
            (recur (rest r)
              d
              (if (< d prev) (- result d) (+ result d)))))))))

(defcheck solution-e0487569
  (fn [s]
    (let [rn {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [s s n 0 p 0]
        (if (empty? s)
          n
          (let [c (rn (first s))]
            (if (or (= p 0) (> p c) (= p c))
              (recur (rest s) (+ n c) c)
              (recur (rest s) (+ n c (- 0 (* 2 p))) c))))))))

(defcheck solution-e085c682
  (let [multi '{"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
        single '{"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
    (fn r [s]
      (if (empty? s) 0
                     (if-let [v2 (and (> (count s) 1) (multi (subs s 0 2)))]
                       (+ v2 (r (subs s 2)))
                       (+ (single (subs s 0 1) 0) (r (subs s 1))))))))

(defcheck solution-e16aefb4
  (let [literals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        subs (fn [nums] ;;; sequence of literals translated to their values
               (loop [acc []  nums nums]
                 (let [n1 (first nums)]
                   (if-let [rs (next nums)]
                     (if (< n1 (first rs))
                       (recur (conj acc (- (first rs) n1)) (rest rs))
                       (recur (conj acc n1) rs))
                     (conj acc (if n1 n1 0))))))

        trans (fn [roms]
                (map literals (seq roms) ))
        to-num (fn [romnumber]
                 (reduce + (subs (trans romnumber))))
        ]
    to-num
    ))

(defcheck solution-e17b52a1
  (fn numeral [r]
    (let [v {\M 1000
             \D 500
             \C 100
             \L 50
             \X 10
             \V 5
             \I 1}
          s (partition-by identity r)]
      (loop [[f s & r] s
             n []]
        (cond
          (nil? f) (apply + n)
          (nil? s) (recur '() (cons (* (v (first f)) (count f)) n))
          (> (v (first f)) (v (first s))) (recur (concat (list s) r) (cons (* (count f) (v (first f))) n))
          :else (recur r (cons (- (* (count s) (v (first s)))
                                  (* (count f) (v (first f)))) n)))))))

(defcheck solution-e18cd0e4
  (fn [o]
    (let [romans {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [x (reverse (map #(get romans %) o)) r 0]
        (if (empty? x)
          r
          (let [t (take-while #(< % (first x)) (rest x))]
            (recur (drop (count t) (rest x)) (+ r (- (first x) (apply + t))))))))))

(defcheck solution-e245f547
  (let [numerals {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000, \Q 500000}]
    (fn [string]
      (let [[total last_number mult] (reduce (fn [[total last_number mult] char]
                                               (let [value (get numerals char)]
                                                 (if (> value last_number)
                                                   [(- total (* last_number mult)) value 1]
                                                   (if (= value last_number)
                                                     [total last_number (inc mult)]
                                                     [(+ total (* last_number mult)) value 1]))))
                                       [0 100000000000 0] string)]
        (+ total (* last_number mult))))))

(defcheck solution-e2dbe536
  (let [romans {\I      1
                [\I \V] 4
                \V      5
                [\I \X] 9
                \X      10
                [\X \L] 40
                \L      50
                [\X \C] 90
                \C      100
                [\C \D] 400
                \D      500
                [\C \M] 900
                \M      1000}]

    (fn read-romans [[A B & more]]
      (cond
        (romans [A B]) (+ (romans [A B]) (read-romans more))
        (romans A)     (+ (romans A) (read-romans (cons B more)))
        :else 0))))

(defcheck solution-e38bbbfd
  (fn [numer-str]
    (let [values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000 }]
      (loop [acc 0
             s numer-str]
        (if (= (count s) 0) acc
                            (let [current (values (first s))
                                  nxt (values (second s) 0)]
                              (if (> nxt current)
                                (recur (+ acc (- nxt current)) (drop 2 s))
                                (recur (+ acc current) (rest s)))))))))

(defcheck solution-e3a69a62
  (let [vals {\I 1 \V 5 \X 10 \L 50 \C 100
              \D 500 \M 1000}]
    (fn [r]
      (->> r
        (map vals)
        (partition 2 1 (repeat 0))
        (map (fn [[a b]] (if (>= a b) a (- a))))
        (reduce +)))))

(defcheck solution-e3a860c4
  (fn read-roman [roman-str]
    (let [roman-map (sorted-map "I" 1,
                      "IV" 4
                      "V" 5,
                      "IX" 9,
                      "X" 10,
                      "XL" 40,
                      "L" 50,
                      "XC" 90,
                      "C" 100,
                      "CD" 400,
                      "D" 500,
                      "CM" 900,
                      "M" 1000)

          start (apply str
                  (take 1 roman-str))

          first-two (apply str
                      (take 2 roman-str))

          next-chunk (if (get roman-map first-two)
                       first-two
                       start)
          next-value (roman-map next-chunk)]

      (if (seq roman-str)
        (+ next-value
          (read-roman (.substring roman-str (count next-chunk))))
        0))))

(defcheck solution-e3c38788
  (fn [s]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000},
          sub? (fn [[x y]] (< x y)),
          c (partition-all 2 1 (map #(m %) s)),
          rm (concat (vector false) (map sub? (drop-last c))),
          get-value (fn [[x y]]
                      (cond (nil? y) x
                            (< x y) (- y x)
                            (>= x y) x))]
      (->> (map vector rm c)
        (remove (comp true? first))
        (map second)
        (map get-value)
        (reduce +)))))

(defcheck solution-e3e6f1c4
  #(let [
         letter ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
         value [1000 900 500 400 100 90 50 40 10 9 5 4 1]
         l2v (zipmap letter value)
         lre (re-pattern (apply str (interpose "|" letter)))]
     (apply + (map l2v (re-seq lre %)))))

(defcheck solution-e469848f
  (fn roman [s]
    (loop [[c & r] s
           n 0]
      (let [t (str c (first r))
            m (drop 1 r)
            f (str c)]
        (if c
          (cond
            (= f "M" ) (recur r (+ n 1000))
            (= f "D" ) (recur r (+ n 500))
            (= t "CM") (recur m (+ n 900))
            (= t "CD") (recur m (+ n 400))
            (= f "C" ) (recur r (+ n 100))
            (= f "L" ) (recur r (+ n 50))
            (= t "XC") (recur m (+ n 90))
            (= t "XL") (recur m (+ n 40))
            (= f "X" ) (recur r (+ n 10))
            (= f "V" ) (recur r (+ n 5))
            (= t "IX") (recur m (+ n 9))
            (= t "IV") (recur m (+ n 4))
            (= f "I" ) (recur r (+ n 1)))
          n)))))

(defcheck solution-e493cfc1
  (fn [s]
    (letfn [(read-roman-thousands [sr]
              (if-let [thousands (re-find #"^M+" sr)]
                [(* 1000 (count thousands)) (subs sr (count thousands))]
                [0 sr]
                )
              )
            (read-hundreds [[prev sr]]
              (loop [hundreds 0 s sr]
                (cond
                  (empty? s) [(+ (* hundreds 100) prev) s]
                  (= (first s) \C) (recur (inc hundreds) (rest s))
                  (= (first s) \D) (recur (if (= hundreds 1) 4 5) (rest s))
                  (= (first s) \M) (recur 9 (rest s))
                  :else [(+ (* hundreds 100) prev) (apply str s)]
                  )
                )
              )
            (read-tens [[prev sr]]
              (loop [tens 0 s sr]
                (cond
                  (empty? s) [(+ (* tens 10) prev) s]
                  (= (first s) \X) (recur (inc tens) (rest s))
                  (= (first s) \L) (recur (if (= tens 1) 4 5) (rest s))
                  (= (first s) \C) (recur 9 (rest s))
                  :else [(+ (* tens 10) prev) (apply str s)]
                  )
                )
              )
            (read-units [[prev sr]]
              (loop [units 0 s sr]
                (cond
                  (empty? s) (+ units prev)
                  (= (first s) \I) (recur (inc units) (rest s))
                  (= (first s) \V) (recur (if (= units 1) 4 5) (rest s))
                  (= (first s) \X) (recur 9 (rest s))
                  :else (+ units prev)
                  )
                )
              )
            ]
      (->> s read-roman-thousands read-hundreds read-tens read-units)
      )
    ))

(defcheck solution-e4b32f4c
  (fn roman
    ([n] (roman (str n) 10000))
    ([s ld]
     (if (empty? s)
       0
       (let [[c & r] s
             d ({"M" 1000, "D" 500, "C" 100, "L" 50, "X" 10, "V" 5, "I" 1} (str c))]
         (+ d (roman r d) (if (< ld d) (- 0 ld ld) 0) ))))))

(defcheck solution-e4e32240
  #(reduce
     (fn [r [a b]]
       ((if (> a b) - +) r b))
     0
     (partition 2 1 (cons 0 (reverse (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]) %))))))

(defcheck solution-e505ae0a
  (fn [source-string]
    (let [character-numbers {\I 1
                             \V 5
                             \X 10
                             \L 50
                             \C 100
                             \D 500
                             \M 1000}
          suffix-characters {\I #{\V \X}
                             \X #{\L \C}
                             \C #{\D \M}}]
      (loop [result 0 rest-characters (vec source-string)]
        (if (empty? rest-characters)
          result
          (let [current-character (first rest-characters)
                current-number (character-numbers current-character)]
            (if (and (second rest-characters)
                     (suffix-characters current-character)
                     ((suffix-characters current-character) (second rest-characters)))
              (recur (+ result (- (character-numbers (second rest-characters))
                                  current-number))
                (rest (rest rest-characters)))
              (recur (+ result current-number)
                (rest rest-characters)))))))))

(defcheck solution-e55f130c
  (let
   [values {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
    (fn [s]
      (loop [value 0
             remaining (rest s)
             now (first s)
             now-value (values now)]
        (cond (empty? remaining) (+ now-value value)
              (> (values (first remaining)) now-value)
              (recur (- value now-value) (rest remaining) (first remaining) (values (first remaining)))
              :else
              (recur (+ value now-value) (rest remaining) (first remaining) (values (first remaining))))))))

(defcheck solution-e56423a4
  (fn [s]
    (letfn [(parse [[h & t] prev]
              (cond
                (nil? h) 0
                (> h prev) (+ h (* -2 prev) (parse t h))
                :else (+ h (parse t h))))]
      (let [xs (map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} s)]
        (parse xs (first xs))))))

(defcheck solution-e61a81d2
  (fn f [s]
    (let [r (hash-map \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000)]
      (if (empty? s) 0
                     (if (= 1 (count s)) (r (first s))
                                         (if (> (r (second s)) (r (first s)))
                                           (+ (- (r (second s)) (r (first s))) (f (nthrest s 2)))
                                           (+ (r (first s)) (f (rest s)))))))))

(defcheck solution-e6293598
  (fn roman->numbers [roman]
    (let [conversion {\M 1000
                      \D 500
                      \C 100
                      \L 50
                      \X 10
                      \V 5
                      \I 1}
          to-num (fn [s]
                   (cond
                     (= s "IV") 4
                     (= s "IX") 9
                     (= s "XL") 40
                     (= s "XC") 90
                     (= s "CD") 400
                     (= s "CM") 900
                     :else (map #(get conversion %) s)))
          split-to-individuals (fn [r]
                                 (loop [acc []
                                        prev (first roman)
                                        rnums roman]
                                   (if (empty? rnums)
                                     acc
                                     (let [num (take-while #(>= (get conversion %) (get conversion prev)) rnums)
                                           nums-left (drop-while #(>= (get conversion %) (get conversion prev)) rnums)]
                                       (recur (conj acc num) (first nums-left) nums-left)))))]
      (apply +
        (flatten (map to-num
                   (map (partial apply str)
                     (split-to-individuals roman))))))))

(defcheck solution-e64025c1
  (fn [s]
    (reduce (fn [sum [a b]]
              ((if (< a b) - +) sum a))
      0
      (partition 2 1 (concat (map {\I 1
                                   \V 5
                                   \X 10
                                   \L 50
                                   \C 100
                                   \D 500
                                   \M 1000} s)
                       [0])))))

(defcheck solution-e72017f
  (fn from-roman [s]
    (let [lookup {1 "I" 5 "V" 10 "X" 50 "L" 100 "C" 500 "D" 1000 "M"}
          nums [1000 500 100 50 10 5 1]
          rev-lookup (apply array-map (interleave (vals lookup) (keys lookup)))]

      ((reduce (fn [x y]
                 (let [curr (rev-lookup (str y)) ]
                   (if (> curr (x :last))
                     {:val (+ curr (- (x :val) (* 2 (x :last)))) :last curr}
                     {:val (+ curr (x :val)) :last curr}
                     )

                   )
                 )
         {:val 0 :last 0} s) :val)
      )
    ))

(defcheck solution-e767922f
  (fn __ [x]
    (let [
          x1 (clojure.string/replace x #"IV" "IIII")
          x2 (clojure.string/replace x1 #"IX" "IIIIIIIII")
          x3 (clojure.string/replace x2 #"XL" "XXXX")
          x4 (clojure.string/replace x3 #"XC" "XXXXXXXXX")
          x5 (clojure.string/replace x4 #"CD" "CCCC")
          x6 (clojure.string/replace x5 #"CM" "CCCCCCCCC")
          mp {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
          ]
      (reduce + (map mp (seq x6))))))

(defcheck solution-e7685662
  (fn [text]
    (letfn [(read-roman [text n]
              (let [code {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} fst (first text) snd (second text)]
                (cond (empty? text) n
                      (= 1 (count text)) (+ n (code fst))
                      (< (code fst) (code snd)) (read-roman (drop 2 text) (+ n (- (code snd) (code fst))))
                      :else (read-roman (rest text) (+ n (code fst))))))]
      (read-roman text 0))))

(defcheck solution-e77326b9
  (fn from-roman [r]
    (let [r-vals {\M 1000, \D 500, \C 100, \L 50 \X 10, \V 5, \I 1}
          subtractive (fn [[x y]] (if (and y (> y x)) (- x) x))]
      (apply + (->> r (map r-vals) (partition-all 2 1) (map subtractive))))))

(defcheck solution-e81e4651
  (fn read-roman [romans]
    (let [numerals
                   {"I" 1 "V" 5 "X" 10
                    "L" 50 "C" 100 "D" 500
                    "M" 1000}
          int-vals (map #(get numerals (str %)) (seq romans))
          reduce-romans
                   (fn [acc ivs]
                     (if (empty? ivs) acc
                                      (recur
                                        (if (not (empty? (rest ivs)))
                                          (if (< (first ivs) (second ivs))
                                            (- acc (first ivs))
                                            (+ acc (first ivs)))
                                          (+ acc (first ivs))) (rest ivs))))]
      (reduce-romans 0 int-vals))))

(defcheck solution-e8b40216
  (fn parse-roman [s]
    (let [values (partition 2 '("M" 1000
                                "CD" 400
                                "CM" 900
                                "D" 500
                                "XC" 90
                                "C" 100
                                "XL" 40
                                "L" 50
                                "IX" 9
                                "X" 10
                                "IV" 4
                                "V" 5
                                "I" 1))]
      (loop [s s result 0]
        (if (empty? s)
          result
          (let [[prefix value] (first (filter (fn [[k v]] (.startsWith s k)) values))]
            (recur (.substring s (count prefix))
              (+ result value))))))))

(defcheck solution-e8b5b359
  (fn read-roman-numerals [s]
    (let [rvmap (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
      (reduce #((if (apply < %2) - +) %1 (first %2)) 0
        (partition 2 1 [0] (map rvmap s))))))

(defcheck solution-e92689fa
  (fn [s]
    (let [table {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100, "CD" 400, "D" 500, "CM" 900, "M" 1000}
          f (fn [xs ch]
              (let [k (apply str (first xs) (str ch))]
                (if (contains? table k)
                  (conj (rest xs) k)
                  (conj xs (str ch)))))
          nums (reduce f [] s)]
      (reduce #(+ %1 (get table %2)) 0 nums))))

(defcheck solution-e94e6514
  (fn [nums-str]
    (let [nums (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]) nums-str)]
      (apply +
        (map-indexed #(if (some (partial < %2) (drop % nums)) (- %2) %2) nums)))))

(defcheck solution-e984d664
  (fn roman->int [st]
    (let [leader [["M" 1000] ["CM" 900] ["D" 500] ["CD" 400]
                  ["C" 100] ["XC" 90] ["L" 50] ["XL" 40] ["X" 10]
                  ["IX" 9] ["V" 5] ["IV" 4] ["I" 1]]]
      (loop [acc 0 r st]
        (if (empty? r)
          acc
          (when-let [[s v] (some #(when (.startsWith r (first %)) %) leader)]
            (recur (+ acc v) (subs r (count s)))))))))

(defcheck solution-e9b954d3
  (fn [s]
    (let [roman {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (first (reduce (fn [[n d] c]
                       (let [c (roman c) op (if (>= c d) + -)]
                         [(op n c) c]))
               [0 0] (reverse s))))))

(defcheck solution-ea5035e8
  (fn [s]
    (let [dict {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          nums (map dict s)]
      (loop [coll (rest nums) prev (first nums) sum 0]
        (if (empty? coll)
          (+ sum prev)
          (let [curr (first coll)]
            (if (< prev curr)
              (recur (rest coll) curr (- sum prev))
              (recur (rest coll) curr (+ sum prev)))))))))

(defcheck solution-ea7edc0c
  (fn roman->arab
    ([roman] (roman->arab roman 0))
    ([roman res]
     (let [romans ["M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"]
           arabs {"I" 1 "IV" 4 "V" 5 "IX" 9 "X" 10 "XL" 40 "L" 50 "XC" 90 "C" 100 "CD" 400 "D" 500 "CM" 900 "M" 1000}
           begin (some #(when (.startsWith roman %) %)
                   romans)]
       (if (empty? roman)
         res
         (recur (apply str (drop (count begin) roman))
           (+ res (arabs begin))))))))

(defcheck solution-eb3c62f7
  (fn [coll]
    (let [coll (map #({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) (seq coll))]
      (apply +
        (loop [[x y :as w] coll acc []]
          (if-not x
            acc
            (recur
              (rest w)
              (conj acc
                (if y (if (< x y) (- x) x)
                      x)))))))))

(defcheck solution-eb8d4418
  (fn roman->int
    [numeral]
    (let [mappings (sorted-map 1 "I"   4 "IV"   5 "V"   9 "IX"
                     10 "X"  40 "XL"  50 "L"  90 "XC"
                     100 "C" 400 "CD" 500 "D" 900 "CM"
                     1000 "M")
          numeral-mappings (into {} (map (comp vec reverse) mappings))]
      (reduce + (:result (reduce
                           (fn [acc [n num]]
                             (let [len (count num)
                                   [result left] (split-with #{(seq num)} (partition-all len (:left acc)))]
                               {:left (apply str (flatten left))
                                :result (concat (:result acc) (map (comp numeral-mappings (partial apply str)) result))}))
                           {:left numeral
                            :result []}
                           (reverse mappings)))))))

(defcheck solution-ebd9d39d
  (fn [rom]
    (let [roman {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          s (map roman (reverse (seq rom)))]
      (second
        (reduce (fn [[prev sum] n]
                  [n (+ sum (if (>= n prev) n (- n)))])
          [0 0]
          s)))))

(defcheck solution-ec1a4ff8
  (fn roman2num [roman]
    (let [roman-map {"I" 1,"V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000}
          roman-seq (for [i roman] (str i))
          roman-asc-val (fn [asc] (roman-map asc))
          roman-val-seq (fn [ss] (map roman-asc-val ss))
          roman-valseq  (roman-val-seq roman-seq)
          roman-valpar  (partition 2 1 roman-valseq)
          roman-negtive (map first (filter #(< (first %) (second %)) roman-valpar))]
      (- (apply + roman-valseq) (* 2 (apply + roman-negtive))))))

(defcheck solution-ecca1dc9
  (fn [s]
    (let [cvt (fn [[x y]]
                (cond (and (not= x \C) (= y \M)) 1000
                      (and (= x \C)    (= y \M)) 800
                      (and (not= x \C) (= y \D)) 500
                      (and (= x \C)    (= y \D)) 300
                      (and (not= x \X) (= y \C)) 100
                      (and (= x \X)    (= y \C)) 80
                      (and (not= x \X) (= y \L)) 50
                      (and (= x \X)    (= y \L)) 30
                      (and (not= x \I) (= y \X)) 10
                      (and (= x \I)    (= y \X)) 8
                      (and (not= x \I) (= y \V)) 5
                      (and (= x \I)    (= y \V)) 3
                      (= y \I) 1                    ))]
      (->> (partition 2 1 (cons nil s))
        (map cvt)
        (apply +)))))

(defcheck solution-ed73cf
  #(loop [s % c 0 m {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (if s
       (recur (next s) (
                        (if (and (next s) (> (m (nth s 1)) (m (nth s 0)))) - +)
                        c (m (nth s 0))) m) c)))

(defcheck solution-ed8c91b0
  (fn rom [s]
    (let [nums {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          parts (->> (map nums (str s "I"))
                  (partition 2 1 )
                  (partition-by #(< (nth % 0) (nth % 1)))
                  (map #(map first %))
                  (map #(apply + %)))]
      (second (reduce #(vector (not (% 0))
                         ((if (% 0) + -) (% 1) %2))
                [(or (= 1 (count parts))
                     (> (first parts) (second parts))) 0]
                parts)))))

(defcheck solution-edb44829
  (fn rr[s]
    (reduce
      (fn[sum [l r]] (if (> l r) (+ sum l) (- sum l))) 0
      (partition 2 1
        (map #(apply + %)
          (concat
            (partition-by identity (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000]) s))
            [[0]]
            ))))))

(defcheck solution-edc12297
  (fn read-roman [str]
    (let [rn {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (letfn [(rn-groups [res num]
                (let [last-num (-> res last last)]
                  (cond
                    (zero? last-num) [[num]]
                    (> num last-num) (update-in res [(dec (count res))] #(conj % num))
                    :default (conj res [num]))))
              (group-map [[n1 n2]]
                (if (nil? n2) n1 (- n2 n1)))]
        (->> str
          (map #(get rn %))
          (reduce rn-groups [[0]])
          (map group-map)
          (reduce +))))))

(defcheck solution-edf3a7f0
  #(loop [[a b & r] (map (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
                      %) m 0]
     (cond
       (nil? a) m
       (nil? b) (+ m a)
       (< a b)  (recur r          (+ m (- b a)))
       :else    (recur (cons b r) (+ m a)))))

(defcheck solution-ee42aa8
  {"XIV" 14
   "DCCCXXVII" 827
   "MMMCMXCIX" 3999
   "XLVIII" 48})

(defcheck solution-ee72bdd2
  (fn [roman]
    (let [dict {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          raw-vals (concat (map dict roman) [0])
          pairs (concat (partition 2 1 raw-vals))
          nums (map (fn [[a b]] (if (< a b) (- a) a)) pairs)]
      (apply + nums))))

(defcheck solution-ee8d1e2d
  (fn [init]
    (let [numerals {\M 1000
                    \D 500
                    \C 100
                    \L 50
                    \X 10
                    \V 5
                    \I 1}]
      (first
        (reduce
          (fn [[acc last-value] val]
            (let [this-value (get numerals val)
                  to-add (if (> this-value last-value)
                           (- this-value (* 2 last-value)) ;; times 2 to "undo" the previous addition
                           this-value)]
              [(+ acc to-add) this-value]))
          [0 0]
          init)))))

(defcheck solution-eea61539
  (fn [s]
    (loop [s s n 0 N [["M" 1000]
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
      (cond (empty? s) n

            (and (<= 2 (count s))
                 (= (subs s 0 2) (first (first N))))
            (recur (subs s 2) (+ n (second (first N))) N)

            (= (subs s 0 1) (first (first N)))
            (recur (subs s 1) (+ n (second (first N))) N)

            :else (recur s n (rest N))))))

(defcheck solution-eead4aab
  (fn [roman-numerals]
    (let [f #(or ({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} %) 0)
          coll (reverse (map f (.toUpperCase roman-numerals)))]
      (loop [curr (first coll)
             [fst & rst] (rest coll)
             prev 0
             acc 0]
        (if (not curr)
          (+ prev acc)
          (if (< curr prev)
            (recur fst rst 0 (+ acc (- prev curr)))
            (recur fst rst curr (+ acc prev))))))))

(defcheck solution-ef19670a
  #(first (reduce (fn [[ac p] e]
                    (if (< p e)
                      [(+ ac (- e p p)) e]
                      [(+ ac e) e]))
            [0 0]
            (replace {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} %))))

(defcheck solution-ef6ee97b
  (fn [x]
    (let [vals {nil 0 \I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      ((fn parse-roman [n]
         (let [f (first n)
               r (rest n)]
           (cond
             (nil? f) 0
             (< (vals f) (vals (first r))) (- (parse-roman r) (vals f))
             :else (+ (parse-roman r) (vals f)))))
       x))))

(defcheck solution-efc937c0
  (fn to-arab [s]
    (->> s
      seq
      (reduce (fn [[last & rest_ :as sx] item]
                (if (empty? sx) (list item)
                                (cond
                                  (= [\I \V] [last item]) (cons 4 rest_)
                                  (= [\I \X] [last item]) (cons 9 rest_)
                                  (= [\X \L] [last item]) (cons 40 rest_)
                                  (= [\X \C] [last item]) (cons 90 rest_)
                                  (= [\C \D] [last item]) (cons 400 rest_)
                                  (= [\C \M] [last item]) (cons 900 rest_)
                                  :else (cons item sx))))
        [])
      (map (fn [sym]
             (cond
               (= \I sym) 1
               (= \V sym) 5
               (= \X sym) 10
               (= \L sym) 50
               (= \C sym) 100
               (= \D sym) 500
               (= \M sym) 1000
               :else sym)))
      (reduce +))))

(defcheck solution-efe40715
  (fn [s] (reduce + (map {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1} (re-seq #"C[MD]|X[CL]|I[XV]|." s)))))

(defcheck solution-f05b9c49
  (fn parse-roman-numeral
    ([string] (parse-roman-numeral
                (clojure.string/upper-case string)
                0))

    ([string acc]
     (letfn [(parse-or [parse-a parse-b]
               (fn [string]
                 (let [[r1 s2] (parse-a string)]
                   (if (= s2 string)
                     (parse-b s2)
                     [r1 s2]))))
             (parse-two-digits [string]
               (if (< (count string) 2) [0 string]
                                        (let [result (case (subs string 0 2)
                                                       "IV" 4
                                                       "IX" 9
                                                       "XL" 40
                                                       "XC" 90
                                                       "CD" 400
                                                       "CM" 900
                                                       0)]
                                          [result (if (zero? result) string (subs string 2))])))
             (parse-digit [string]
               (if (< (count string) 1) [0 ""]
                                        (let [result (case (subs string 0 1)
                                                       "I" 1
                                                       "V" 5
                                                       "X" 10
                                                       "L" 50
                                                       "C" 100
                                                       "D" 500
                                                       "M" 1000
                                                       0)]
                                          [result (if (zero? result) string (subs string 1))])))
             (parse [string] ((parse-or parse-two-digits parse-digit) string))]
       (if (empty? string)
         acc
         (let [[r s] (parse string)]
           (recur s (+ r acc))))))))

(defcheck solution-f10ddab2
  #(let [sub [["IV" "IIII"]
              ["IX" "VIIII"]
              ["XL" "XXXX"]
              ["XC" "LXXXX"]
              ["CD" "CCCC"]
              ["CM" "DCCCC"]]
         norm (reduce (fn [s [f t]] (clojure.string/replace s f t)) % sub)
         val {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
     (reduce + (map val norm))))

(defcheck solution-f11f4733
  (fn [s]
    (let [roman-digits {\M 1000
                        \D 500
                        \C 100
                        \L 50
                        \X 10
                        \V 5
                        \I 1}]
      (loop [acc 0
             last-value 0
             current-letter (first s)
             remaining (next s)]
        (if-not current-letter
          acc
          (let [current-value (roman-digits current-letter)]
            (recur (+ acc current-value (if (< last-value current-value)
                                          (- (* 2 last-value))
                                          0))
              current-value
              (first remaining)
              (next remaining))))))))

(defcheck solution-f1282eee
  (fn [s] ((fn convert-roman [s value]
             (if (empty? s)
               value
               (let [c (first s)
                     n (second s)]
                 (case c
                   \C (case n
                        \M (convert-roman (drop 2 s) (+ value 900))
                        \D (convert-roman (drop 2 s) (+ value 400))
                        (convert-roman (rest s) (+ value 100)))
                   \X (case n
                        \C (convert-roman (drop 2 s) (+ value 90))
                        \L (convert-roman (drop 2 s) (+ value 40))
                        (convert-roman (rest s) (+ value 10)))
                   \I (case n
                        \X (convert-roman (drop 2 s) (+ value 9))
                        \V (convert-roman (drop 2 s) (+ value 4))
                        (convert-roman (rest s) (+ value 1)))
                   \M (convert-roman (rest s) (+ value 1000))
                   \D (convert-roman (rest s) (+ value 500))
                   \L (convert-roman (rest s) (+ value 50))
                   \V (convert-roman (rest s) (+ value 5))))))
           s 0)))

(defcheck solution-f26a943a
  (fn boo [s]
    (let
     [rom {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
      (last
        (reduce (fn [[prev-arab accu] chr]
                  (let
                   [arab (rom chr)]
                    (if (>= prev-arab arab)
                      [arab (+ accu arab)]
                      [arab (- (+ accu arab) (* 2 prev-arab))])))
          [10000 0]
          s)))))

(defcheck solution-f287f3da
  (fn [x] (let [ones [\I \X \C \M \?]
                fives [\V \L \D \?]
                numerals (into {"" 0} (for [o (range (count fives)) d (range 1 10)]
                                        [(cond
                                           (< d 4) (apply str (repeat d (get ones o)))
                                           (= d 4) (str (get ones o) (get fives o))
                                           (< d 9) (apply str (get fives o) (repeat (- d 5) (get ones o)))
                                           (= d 9) (str (get ones o) (get ones (inc o))))
                                         (* d (nth (iterate (partial * 10) 1) o))]))
                [t n] (reduce (fn [[t n] c]
                                (if (contains? numerals (str n c))
                                  [t (str n c)]
                                  [(+ t (numerals n)) c]))
                        [0 ""]
                        (seq x))]
            (+ t (numerals n)))))

(defcheck solution-f31d371c
  (fn rn ([x] (rn x 0))
    ([[f s & r :as x] n]
     (let [nums (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])]
       (if (empty? x) n
                      (if (and s (> (nums s) (nums f)))
                        (rn r (+ n (- (nums s) (nums f))))
                        (rn (next x) (+ n (nums f)))))))))

(defcheck solution-f3293e98
  (fn [s]
    (let [tokens {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          l (map tokens s)
          parse-one-step (fn [v]
                           (let [c (take 2 v)]
                             (if (apply < c)
                               [(- (apply - c)) (drop 2 v)]
                               [(first v) (rest v)]
                               )))
          parse-all (fn [v]
                      (iterate #(parse-one-step (second %)) [0 v]))
          res (split-with #(not-empty (second %)) (parse-all l))
          ]
      (reduce + (map first (conj (first res) (first (second res)))))
      )))

(defcheck solution-f3bb4bb5
  (fn [s]
    (let [roman-value {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
          numbers (map roman-value (reverse s))]
      (apply + (map #(if (< %1 %2) (- %1) %1) numbers (reductions max numbers))))))

(defcheck solution-f40c5a52
  (fn [s]
    (first (reduce
             (fn [[r m] i] (vector (+ r (* i (if (< i m) -1 1))) (max i m)))
             [0 0]
             (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} (reverse (seq s)))))))

(defcheck solution-f4c17e45
  (let [m (array-map
            :I 1
            :V 5
            :X 10
            :L 50
            :C 100
            :D 500
            :M 1000)
        om (zipmap (keys m) (range))]
    (fn roman-to-decimal
      ([s] (let [p (keyword (str (first s)))]
             (roman-to-decimal (rest s) (m p) p)))
      ([s v p]
       (if (empty? s)
         v
         (let [n (keyword (str (first s)))
               t (m n)]
           (if (> (om n) (om p))
             (recur (rest s) (- (+ v t) (* 2 (m p))) n)
             (recur (rest s) (+ t v) n))))))))

(defcheck solution-f4f16172
  (fn roman-numeral [s]
    (let [table {\I 1 \V 5
                 \X 10 \L 50
                 \C 100 \D 500
                 \M 1000}]
      (loop [s s ret 0]
        (if (empty? s) ret
                       (let [x1 (first s)
                             x2 (second s)]
                         (cond (nil? x2)
                               (+ ret (table x1))

                               (< (table x1) (table x2))
                               (recur (next s)
                                 (- ret (table x1)))

                               :else
                               (recur (next s)
                                 (+ ret (table x1))))))))))

(defcheck solution-f6670c9a
  (fn readRomans [str] (letfn [
                               (romanValue [ch] (cond
                                                  (= \I ch) 1
                                                  (= \V ch) 5
                                                  (= \X ch) 10
                                                  (= \L ch) 50
                                                  (= \C ch) 100
                                                  (= \D ch) 500
                                                  (= \M ch) 1000))
                               (findNegs [numSeq] (loop [todo numSeq res '()] (cond
                                                                                (empty? (rest todo)) (conj res (first todo))
                                                                                (< (first todo) (second todo)) (recur (rest todo) (conj res (* -1 (first todo))))
                                                                                :default (recur (rest todo) (conj res (first todo))))))
                               (convertToNumbers [romanSeq] (into [] (map romanValue romanSeq)))
                               ]
                         (reduce + (findNegs (convertToNumbers (seq str))))
                         )
    ))

(defcheck solution-f67c2ea8
  #(let [v {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
     (reduce +
       (map (fn [[x y]]
              (let [a (v x) b (v y)] (* a (if (< a b) -1 1))))
         (partition 2 1 (str % "I"))))))

(defcheck solution-f6fa3c31
  (fn [x]
    (let [m {
             [\I] 1
             [\V \I] 4
             [\V] 5
             [\X \I] 9
             [\X] 10
             [\L \X] 40
             [\L] 50
             [\C \X] 90
             [\C] 100
             [\D \C] 400
             [\D] 500
             [\M \C] 900
             [\M] 1000
             } ]
      ((fn f [a]
         (if (empty? a)
           0
           (if (m (take 2 a))
             (+ (m (take 2 a)) (f (drop 2 a)))
             (+ (m (take 1 a)) (f (rest a)))))) (reverse x)))))

(defcheck solution-f7051ba9
  (fn rn [s]
    (let [lm {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          nums  (map #(apply + %) (partition-by identity (map lm s)))
          prn (fn [[s l] c] (if (> c l) [(- (+ c s) (* 2 l)) c]
                                        [(+ s c) c]))]
      (first (reduce prn [0 (first nums)] nums))
      )))

(defcheck solution-f73dddaa
  (fn [roman]
    (let [values {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
      (loop [n 0 remaining roman]
        (if (empty? remaining)
          n
          (let [r1 (values (first remaining))
                r2 (values (second remaining))]
            (if (and r2 (< r1 r2))
              (recur (+ n (- r2 r1)) (drop 2 remaining))
              (recur (+ n r1) (rest remaining)))))))))

(defcheck solution-f76ecad
  (fn [x](let
          [m (zipmap "MDCLXVI" [1000 500 100 50 10 5 1])]
           (->>
             (str x "I")
             (map m)
             (partition 2 1)
             (map #(if (> (second %) (first %))
                     (- 0 (first %))
                     (first %)))
             (apply +)))))

(defcheck solution-f7a6ce90
  (fn [s]
    (let [rom {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply + (remove nil? (reduce (fn [[r l] x]
                                      (let [n (rom x)]
                                        (cond (nil? l) [r n]
                                              (> n l) [(+ r (- n l)) nil]
                                              :else [(+ r l) n])))
                              [0 nil] s))))))

(defcheck solution-f7b4f1c
  (fn [s]
    (let [roman {"M" 1000
                 "CM" 900
                 "D"  500
                 "CD" 400
                 "C"  100
                 "XC"  90
                 "L"   50
                 "XL"  40
                 "X"   10
                 "IX"   9
                 "V"    5
                 "IV"   4
                 "I"    1}]
      (reduce +
        (map roman
          (re-seq #"CM|CD|IX|IV|XC|XL|[MDCLXVI]" s))))))

(defcheck solution-f85179fa
  (fn [string]
    (let [romanmap {"I" 1, "IV" 4, "V" 5, "IX" 9, "X" 10, "XL" 40, "L" 50, "XC" 90, "C" 100, "CD" 400, "D" 500, "CM" 900, "M" 1000}]
      (loop [s (flatten (partition 1 string))
             result []]
        (if (empty? s)
          (reduce + result)
          (let [ij (str (first s) (second s))
                i (str (first s))
                n (romanmap ij)]
            (recur (if n (drop 2 s) (rest s))
              (if n (conj result n)
                    (conj result (romanmap i))))))))))

(defcheck solution-f88ced64
  (fn [s]
    (let [r {\0 0, \I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (->> s
        reverse
        (cons \0)
        (map r)
        (partition 2 1)
        (map (fn [[a b]] (if (<= a b) b (- b))))
        (reduce +)))))

(defcheck solution-f8a67a9d
  (fn [s]
    (let [m (re-seq #"IV|IX|XL|XC|CD|CM|M|D|C|L|X|V|I" s) mp
            {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900 }]
      (apply + (map mp m)))))

(defcheck solution-f8b73980
  (fn [num_str]
    (let [r_map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} r_list (reverse num_str) num (count r_list) last_ind (dec num)]
      (+ (r_map (first r_list)) (apply + (for [i (range num) :while (< i last_ind)] (let [r (r_map (nth r_list (inc i))) l (r_map (nth r_list i))] (if (< r l)(- r) r))) ))
      )
    ))

(defcheck solution-f8ce34b1
  (fn read-romans [rn]
    (let [vals {\I 1 \V 5 \X 10 \L 50
                \C 100 \D 500 \M 1000}
          subs {\V \I \X \I \L \X \C \X \D \C \M \C}]
      (loop [[c & chars] (map char rn)
             acc 0]
        (if (nil? c)
          acc
          (recur chars ((if (= (get subs (first chars)) c) - +) acc (get vals c))))))))

(defcheck solution-f8d5e620
  (fn [s]
    (let [vals {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}
          nums (map #(get vals %) s)]
      (reduce (fn [acc [a b]] (+ acc (if (>= a b) a (- a)))) 0 (map (fn [a b] [a b])  nums (concat (rest nums) [(last nums)]))))))

(defcheck solution-f938b1bc
  (fn [s]
    (let [
          r {\M [1000 0] \D [500 0] \C [100 500]
             \L [50 0] \X [10 50] \V [5 0] \I [1 5]}
          add (fn [a c]
                (let [[x y] (r c)]
                  (if (<= 1 y a) (- a x) (+ a x))))
          ]
      (reduce add 0 (reverse s)))))

(defcheck solution-f9449a18
  (fn [roman]
    (let [single-char-roman
                    (clojure.string/replace roman #"IV|IX|XL|XC|CD|CM" {"IV" "Q" "IX" "R" "XL" "S" "XC" "T" "CD" "W" "CM" "Y"})
          value-map {\I 1 \Q 4 \V 5 \R 9 \X 10 \S 40 \L 50 \T 90 \C 100 \W 400 \D 500 \Y 900 \M 1000}]
      (reduce #(+ % (value-map %2)) 0 single-char-roman))))

(defcheck solution-f979092f
  (fn [numeral]
    (let [values {\I 1
                  \V 5
                  \X 10
                  \L 50
                  \C 100
                  \D 500
                  \M 1000}]
      (apply + (map (fn [[x y]]
                      (let [a (values x)
                            b (values y)
                            v (- a b)]
                        (if (neg? v)
                          (- a)
                          a)))
                 (partition 2 1 [\I] numeral))))))

(defcheck solution-f9860b31
  (fn [s]
    (let [standard {"I" 1
                    "V" 5
                    "X" 10
                    "L" 50
                    "C" 100
                    "D" 500
                    "M" 1000}

          special {"IV" 4
                   "IX" 9
                   "XL" 40
                   "XC" 90
                   "CD" 400
                   "CM" 900}
          special-regex (re-pattern (reduce str (interpose "|" (keys special))))
          special-sum (reduce + (map #(get special %) (re-seq special-regex s)))
          stuff (loop [s s
                       l (keys special)]
                  (if (empty? l)
                    s
                    (recur (clojure.string/replace s (first l) "")
                      (rest l))))
          o (map #(get standard %) (map str (seq stuff)))]

      (reduce + (conj o special-sum)))))

(defcheck solution-f9abc3ca
  #(let [rtoa {\I 1 \V 5 \X 10 \L 50  \C 100 \D 500 \M 1000}
         arabic (map rtoa (reverse %))]
     (loop [[d & r] arabic maxVal 0 sum 0]
       (if (nil? d)
         sum
         (if (<  d maxVal)
           (recur r maxVal (- sum d))
           (recur r d (+ sum d)))))))

(defcheck solution-f9c80fa
  (fn p92 [n]
    (let [m {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000 "IX" 9 "IV" 4 "XL" 40 "XC" 90 "CD" 400 "CM" 900}]
      (loop [s n r 0]
        (cond
          (= (count s) 1) (+ r (get m s))
          (= (count s) 0) r
          (>= (count s) 2) (let [dl (get m (.substring s 0 2))
                                 l (get m (.substring s 0 1))]
                             (if (nil? dl)
                               (recur (.substring s 1) (+ r l))
                               (recur (.substring s 2) (+ r dl)))))))))

(defcheck solution-fa78ced3
  (fn [s]
    (loop [[digit & rest-of-number] (->> s
                                      (reverse)
                                      (map (zipmap "MDCLXVI" [1000 500 100 50 10 5 1])))
           prev-digit 0
           sum 0]
      (if (nil? digit)
        sum
        (let [op (if (> prev-digit digit) - +)]
          (recur rest-of-number digit (op sum digit)))))))

(defcheck solution-fb423b44
  #(let [Ro1 {"I" 1, "V" 5, "X" 10, "L" 50, "C" 100, "D" 500, "M" 1000}
         Ro2 {"IV" 4, "IX" 9, "XL" 40, "XC" 90, "CD" 400, "CM" 900}]
     (apply +
       ((fn [ret, ss]
          (cond
            (empty? ss)
            ret
            (and (< 1 (count ss)) (Ro2 (subs ss 0 2)))
            (recur (conj ret (Ro2 (subs ss 0 2))) (subs ss 2))
            :else
            (recur (conj ret (Ro1 (subs ss 0 1))) (subs ss 1))))
        [], %))))

(defcheck solution-fb5f0215
  (fn[num]
    (let
     [roman { \M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1, \~ 9999 }
      trans (fn [[sum prev] chr]
              (let [ val (if (< (roman prev) (roman chr))
                           (- (roman chr) (* 2 (roman prev)))
                           (roman chr))]
                [(+ sum val) chr]))]
      (first (reduce trans [0 \~] num)))))

(defcheck solution-fb880f5c
  (fn [s] (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}
                v (vec (map #(m %) s))]
            (->>
              (range (dec (count v)))
              (map #(if (< (v %) (v (inc %))) (- (v %)) (v %)))
              (apply +)
              (+ (last v))
              ))))

(defcheck solution-fba09bcc
  (fn [rn]
    (let [m {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      ((fn to-num [r [x & xs]]
         (if (empty? xs)
           (+ r (m x))
           (if (< (m x) (m (first xs)))
             (recur (- r (m x)) xs)
             (recur (+ r (m x)) xs)))) 0 (seq rn)))))

(defcheck solution-fbb4e33b
  (fn [s]
    (let [roman-values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
      (apply +
        (map #(let [[v n] %] (if (or (nil? n) (>= v n)) v (- v)))
          (partition-all 2 1 (map roman-values s)))))))

(defcheck solution-fbc737d1
  (fn [s]
    (let [D (zipmap [\I \V \X \L \C \D \M] [1 5 10 50 100 500 1000])
          s (partition-by identity (map #(D %) s))]
      (->> (map (fn [c n]
                  (if (< (first c) (first n))
                    (map - c)
                    c))
             s (concat (rest s) (list (last s))))
        flatten
        (apply +)))))

(defcheck solution-fc040cb5
  (let [m {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}]
    (fn k [s]
      (let [v2 (m (apply str (take 2 s)))
            v1 (m (apply str (take 1 s)))]
        (cond
          v2 (+ v2 (k (drop 2 s)))
          v1 (+ v1 (k (drop 1 s)))
          :else 0
          )))))

(defcheck solution-fc527c89
  (fn [s]
    (let [digits {\M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
      (:total
       (reduce
         (fn [{:keys [max-digit total]} digit]
           {:max-digit (max max-digit digit)
            :total ((if (< digit max-digit) - +) total digit)})
         {:max-digit 0 :total 0}
         (reverse (map digits s)))))))

(defcheck solution-fda71648
  (fn roman
    ([i-literal] (roman 0 i-literal))
    ([i-result i-literal]
     (if (zero? (count i-literal))
       i-result
       (let [
             v-map {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500 \M 1000}
             head (first i-literal)
             tail (rest i-literal)
             t-head (first tail)
             ]
         (if (and (not (nil? t-head)) (> (get v-map t-head) (get v-map head)))
           (roman (+ i-result (- (get v-map t-head) (get v-map head))) (rest tail))
           (roman (+ i-result (get v-map head)) tail)))))))

(defcheck solution-fdd898dc
  (fn [R]
    (let [n (map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} R)]
      (reduce + (map-indexed (fn [i x] (if (some #(> % x) (drop (inc i) n)) (- x) x)) n)))))

(defcheck solution-fe8a5fe9
  (fn [s]
    (let [xs (map #({\X 10 \I 1 \V 5 \L 50 \C 100 \D 500 \M 1000} %) s)
          t (last xs)]
      (apply + (concat (->> xs
                         (partition 2 1)
                         (map #(if (< (first %) (second %)) (- (first %)) (first %)))
                         ) [t])))))

(defcheck solution-fea9b7c8
  (fn [y] (second
            (reduce #((fn f [[s n i] a]
                        (let [x (first (last a))]
                          (if (.endsWith s x)
                            [(.substring s 0 (- (count s) (count x))) (+ n (* i (second (last a)))) (* i 10)]
                            (f [s n i] (drop-last a))))) % %2) [y 0 1]
              (map #(into (map vector % [1 2 3 5 4 6 7 8 9]) [["" 0]])
                [["I" "II" "III" "V" "IV" "VI" "VII" "VIII" "IX"]
                 ["X" "XX" "XXX" "L" "XL" "LX" "LXX" "LXXX" "XC"]
                 ["C" "CC" "CCC" "D" "CD" "DC" "DCC" "DCCC" "CM"]
                 ["M" "MM" "MMM"]])))))

(defcheck solution-febb0a6a
  (fn [s]
    (let [l (map (fn [c]
                   ({\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} c)) s)]
      (loop [m (rest l)
             v (first l)
             r 0]
        (if (empty? m)
          (+ r v)
          (let [nv (first m)]
            (if (< v nv)
              (recur (rest m) nv (- r v))
              (recur (rest m) nv (+ r v)))))))))

(defcheck solution-ff0ad1f4
  #(reduce +
     (map {"I" 1   "IV" 4   "V" 5   "IX" 9
           "X" 10  "XL" 40  "L" 50  "XC" 90
           "C" 100 "CD" 400 "D" 500 "CM" 900
           "M" 1000}
       (re-seq #"CM|M|CD|D|XC|C|XL|L|IX|X|IV|V|I" %))))

(defcheck solution-ffe5b51d
  (let [roman {\M 1000
               \D 500
               \C 100
               \L 50
               \X 10
               \V 5
               \I 1}]
    (fn [s] (->> (map roman s)
              (partition 2 1 [0])
              (map (fn [[x y]] (if (< x y) (- x) x)))
              (reduce +)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-92))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

