(ns coal-mine.problem-74
  (:require [coal-mine.checks :refer [defcheck-74] :rename {defcheck-74 defcheck}]
            [clojure.test]
            [clojure.string]
            [clojure.set]
            [clojure.pprint]))

(defn parse-int [s]
  #?(:clj (Integer/parseInt s) :cljs (js/parseInt s)))

(defn integer [s]
  (parse-int s))

(defn parse-double [s]
  #?(:clj (Double/parseDouble s) :cljs (js/parseFloat s)))

(defcheck solution-101110dc
  (fn [s] (->> s
            (re-seq #"\d+")
            (map parse-int)
            (filter #(let [r (Math/sqrt %)] (= r (Math/floor r))))
            (interpose ",")
            (apply str))))

(defcheck solution-101cf12d
  (fn [string]
    (->> string
      (#(clojure.string/split % #","))
      (map parse-int)
      (filter #(= (Math/floor %) (let [s (Math/floor (Math/sqrt %))] (* s s))))
      (clojure.string/join ","))))

(defcheck solution-10849755
  (fn [num-list-str]
    (clojure.string/join ","
      (map str
        (filter #(zero? (- % (* (Math/sqrt %) (Math/sqrt %))))
          (map parse-int
            (clojure.string/split num-list-str #",")))))))

(defcheck solution-10adea2f
  (let [square #(* % %)]
    (comp
     #(apply str (butlast
                   (interleave (map str %) (repeat \,))))
     (partial filter #(= % (square (int (Math/sqrt %)))))
     (partial map #(parse-int (apply str %)))
     (partial filter #(not (= % '(\,))))
     (partial partition-by #(= % \,)))))

(defcheck solution-113be096
  #(clojure.string/join "," (filter (fn [x] (let [x (Math/sqrt (parse-int x))] (= (Math/floor x) x))) (re-seq #"\d+" %))))

(defcheck solution-1149ccd3
  (fn filter-p-sqr [a-str]
    (clojure.string/join "," (filter (fn [x]
                                       (let [upper-bound (int (inc (Math/ceil (Math/sqrt x))))]
                                         (some (fn [y] (= x (* y y))) `(~(dec upper-bound) ~(- upper-bound 2)))))
                               (map #(parse-int %) (re-seq #"\d+" a-str))))))

(defcheck solution-11f51e64
  (fn myps [s]
    (apply str
      (interpose ","
        (map str
          (filter
            (fn [x] (= x (#(* % %) (Math/round (Math/sqrt x)))))
            (map #(parse-int %) (re-seq #"[0-9]+" s))))))))

(defcheck solution-1225755d
  (fn [s]
    (reduce #(str %1 "," %2)
      (filter #(let [inum (parse-int %)
                     sqr (int (Math/sqrt inum))]
                 (= (* sqr sqr) inum)) (clojure.string/split s #"\,")))))

(defcheck solution-1254bed9
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map (fn [x] (parse-int x)))
      (filter (fn [x] (== (Math/sqrt x) (Math/floor (Math/sqrt x)))))
      (clojure.string/join ","))))

(defcheck solution-12555198
  (fn [s]
    (let [xs (map parse-int (.split s ","))
          sqrs (map #(* % %) (range))
          sqr? (fn [x] (= x (first (drop-while #(< % x) sqrs))))]
      (clojure.string/join "," (filter sqr? xs)))))

(defcheck solution-12798628
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map parse-int)
      (filter #(= 0.0 (mod (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-128b6f19
  (fn [string]
    (let [is-square #(let [rt (Math/sqrt %)]
                       (= rt (Math/floor rt)))
          strings (clojure.string/split string #",")
          numbers (map parse-int strings)
          squares (filter is-square numbers)
          final-strings (loop [[hd & tl] squares
                               final []]
                          (if (empty? tl)
                            (conj final hd)
                            (recur tl (conj final hd ","))))]
      (apply str final-strings))))

(defcheck solution-12bf4cd6
  (fn [s] (apply str
            (interpose ","
              (filter
                (fn [n] (some #(= (* % %) n) (range n)))
                (map #(parse-int %) (.split s ",")))))))

(defcheck solution-13198801
  (fn [s]
    (let [perfect-square? (fn [x] ; accept an int as string
                            (let [sqrt (Math/sqrt (parse-int x))]
                              (= (Math/floor sqrt) sqrt)))]
      (clojure.string/join ","
        (filter perfect-square?
          (clojure.string/split s #","))))))

(defcheck solution-13a68cc2
  (fn [z] (clojure.string/join "," (filter #(let [i (integer %)] (some (fn [x] (= (* x x) i)) (range i)))  (clojure.string/split z #",")))))

(defcheck solution-13b711ec
  (fn c74
    [s]
    (clojure.string/join "," (filter (fn
                                       [num]
                                       (let [n (integer num)
                                             sqrt (int (Math/sqrt n))]
                                         (= n (* sqrt sqrt))))
                               (clojure.string/split s #",")))))

(defcheck solution-13e1e6c1
  (fn [a-seq]
    (->>
      (map parse-int (re-seq #"[\d.]+" a-seq))
      (filter #{2 4 9 16 25 36})
      (clojure.string/join ","))))

(defcheck solution-13e8b12a
  #(->> (for [s (re-seq #"\d+" %)] (integer s))
     (filter (fn [x] (= 0.0 (mod (Math/sqrt x) 1))))
     (clojure.string/join ",")))

(defcheck solution-14ebf2f
  (fn [s]
    (clojure.string/join
      ","
      (filter #(= 0.0 (mod (Math/sqrt (parse-int %)) 1)
                 )
        (.split s ",")))))

(defcheck solution-14ed4163
  (fn [s]
    (clojure.string/join
      ","
      (filter
        #(let [root (Math/sqrt %)] (= root (Math/floor root)))
        (map
          #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-154902c1
  (fn [string]
    (let [squares (set (map #(* % %) (range 1 7)))]
      (clojure.string/join "," (filter squares (map #(parse-int %) (clojure.string/split string #",")))))))

(defcheck solution-15a7d894
  (fn [s] (->> (clojure.string/split s #",")
            (filter #(let [r (Math/sqrt (parse-int %))] (zero? (- r (int r)))))
            (clojure.string/join ","))))

(defcheck solution-162e3721
  (fn [y]
    (apply str (interpose ","(filter (fn [x] (some #(= (parse-int x) (* % %))
                                               (range 2 (parse-int x))
                                               )) (clojure.string/split y #","))))))

(defcheck solution-166627c
  (fn [s]
    (letfn [(nums [s] (map parse-int (clojure.string/split s #",")))]
      (apply str
        (interpose
          ","
          (filter
            (set (map #(* % %) (range (inc (Math/sqrt (apply max (nums s)))))))
            (nums s)
            )
          )
        )
      )
    ))

(defcheck solution-166ea8ef
  (fn [s]
    (clojure.string/join "," (filter (fn [ss] (let [n (parse-int ss)]
                                                (first (filter #(= (int n) (* % %)) (range n)))))
                               (clojure.string/split s #",")))))

(defcheck solution-16dd7a8e
  (fn filterStr [p_str]
    (apply str
      (interpose  ","
        (filter  #(let[v1 (int  (Math/sqrt %)),
                       v2 (* v1 v1)]
                    (= v2 %)
                    )

          (map  #(parse-int  %)
            (re-seq #"\w+"  p_str)
            )
          )
        )
      )
    ))

(defcheck solution-17308d66
  (fn [d]
    (letfn [(mysqrt [a] (int (Math/sqrt a)))
            (perfectsquare? [b] (= b (* (mysqrt b) (mysqrt b))))
            (getintegers [c] (map #(parse-int %) (clojure.string/split c #",")))]
      (clojure.string/join "," (filter #(perfectsquare? %) (getintegers d))))))

(defcheck solution-173af61a
  (fn __ [s]
    (letfn [(square? [n]
              (= 0.0 (rem (Math/sqrt n) 1)))]
      (clojure.string/join ","
        (filter square? (map #(integer %)
                          (clojure.string/split s #"\,")))))))

(defcheck solution-178eaa58
  (fn [s] (clojure.string/join "," (filter (fn [n] (-> n (integer) (Math/sqrt) (Math/round) (#(* % %)) (= (integer n)))) (.split s ",")))))

(defcheck solution-17d823f3
  (fn filter-perfect-squares [s]
    (clojure.string/join
      ","
      (filter
        #(let [tmp (Math/sqrt (parse-int %))]
           (= tmp (Math/floor tmp) (Math/ceil tmp)))
        (clojure.string/split s #",")))))

(defcheck solution-18dbf89a
  (fn [c] (apply str (interpose \, (remove #(empty? (for [x (range %) :when (= % (* x x))] x))
                                     (map #(parse-int %) (re-seq #"[0-9]+" c)))))))

(defcheck solution-18f35dbb
  (fn [numstring]
    (let [numbers (map #(integer %) (clojure.string/split numstring #","))
          square? (fn [n]
                    (let [root (Math/sqrt n)]
                      (= root (Math/floor root))))
          squares (filter square? numbers)
          ]
      #_(println squares numbers)
      (clojure.string/join "," (map str squares))
      )))

(defcheck solution-197d658c
  (fn [s]
    (reduce #(str % "," %2)
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2)))
        (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-1a41e15f
  (fn filter-sq [st]
    (let [square? #(= (rem (Math/sqrt %) 1) 0.0)]
      (->> (.split st ",")
        (map #(parse-int %) ,,)
        (filter square? ,,)
        (clojure.string/join "," ,,)))))

(defcheck solution-1a541a24
  (fn extract-perfect-squares [s]
    (letfn [(perfect-square? [n]
              (let [sqrt (int (+ 0.5 (Math/sqrt n)))]
                (= (* sqrt sqrt) n)))]
      (let [numbers (map  #(parse-int %) (.split s ","))]
        (clojure.string/join "," (filter perfect-square? numbers))))))

(defcheck solution-1acdf13b
  (fn [s]
    (let [is-square? #(seq (for [x (range %) :while (<= (* x x) %) :when (= (* x x) %)]
                             x))]
      (clojure.string/join ","
        (filter is-square?
          (map parse-int (re-seq #"[0-9]+" s)))))))

(defcheck solution-1bf82411
  (fn filter-squares [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          fmt #(apply str (interpose "," (map (comp str int) %)))]
      (fmt (filter #(zero? (rem (Math/sqrt %) 1)) nums)))))

(defcheck solution-1c7b586d
  (fn [s]
    (let [l (clojure.string/split s #",")
          m (map #(parse-int %) l)
          s (set (map #(* % %) (range 10)))
          f (filter #(s %) m)]
      (clojure.string/join "," (map str f) ))))

(defcheck solution-1ccff6cb
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (interpose ",")
      (apply str))))

(defcheck solution-1cdafcfd
  (fn sqrstr [strings]
    (let [s2c
          (fn s2c [strings]
            (re-seq #"\d+" strings))
          s2i
          (fn s2i [col]
            (map parse-int col))
          sqrt
          (fn sqrt [n]
            (loop [i 1]
              (if (> i (int (/ n 2)))
                false
                (if (= (* i i) n)
                  i
                  (recur (inc i))))))
          c2cs
          (fn c2cs [coll]
            (subs
              (apply str
                (map (partial str ",") coll))
              1))]
      (c2cs (filter #(sqrt %) (-> strings s2c s2i))))))

(defcheck solution-1d52ce48
  (fn perfect-squares [s]
    (apply str
      (interpose ","
        (filter
          #(= % (int (Math/pow (int (Math/sqrt %)) 2)))
          (map #(integer %) (re-seq #"\d+" s)))))))

(defcheck solution-1d9d8de8
  #(clojure.string/join
     ","
     (filter (fn [x] (zero? (mod (Math/sqrt x) 1)))
       (map parse-int (clojure.string/split % #",")))))

(defcheck solution-1daaa0b7
  (letfn [
          (is-square? [n] (zero? (mod (Math/sqrt n) 1)))
          (perfect-squares [string] (clojure.string/join "," (filter is-square? (map #(integer %) (clojure.string/split string #",")))))]
    perfect-squares))

(defcheck solution-1de2145a
  (fn [s]
    (letfn
     [(perfect-square? [x]
        (let [x (parse-int x)
              m (int (Math/sqrt x))]
          (loop [i 2]
            (cond (= (* i i) x) true
                  (> i m) false
                  :else (recur (inc i))))))]
      (apply str (->> (clojure.string/split s #",")
                   (filter perfect-square?)
                   (interpose ","))))))

(defcheck solution-1e05954a
  (fn [n]
    (let [nums (map #(parse-int %) (clojure.string/split n #","))
          squares? (fn [x]
                     (let [y (Math/sqrt x)] (= y (Math/floor y))))
          perfect (filter squares? nums)]
      (clojure.string/join "," perfect))))

(defcheck solution-1e133cad
  (fn [s]
    (clojure.string/join \, (map str
                              (filter #(let [a (Math/round (Math/sqrt %))]
                                         (= (* a a) %)) (map parse-int (clojure.string/split s #"\,")))))))

(defcheck solution-1ea5e8cc
  (fn [s]
    (->> s
      (#(.split % ","))
      (map parse-int)
      (filter #(= (Math/sqrt %1)
                 (Math/floor (Math/sqrt %1))))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-1ef148f7
  (fn
    [s]
    (reduce #(str % "," %2)
      (filter
        #(let [i (int (Math/sqrt %))]
           (= (* i i) %))
        (map
          #(integer %)
          (re-seq #"\d+" s))))
    ))

(defcheck solution-1f207e57
  (fn [xs]
    (apply str (interpose ","
                 (reduce (fn [a b]
                           (let [x (Math/sqrt (parse-int b))]
                             (if (= (float (int x)) x)
                               (conj a b)
                               a)))
                   []
                   (clojure.string/split xs #","))))))

(defcheck solution-1f428b5b
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter (fn [x]
                (letfn [(square [y] (* y y))]
                  (= x (-> x
                         Math/sqrt
                         Math/round
                         int
                         square)))))
      (clojure.string/join ","))))

(defcheck solution-200d0f19
  (fn [s]
    (clojure.string/join ","
      (filter
        (set (map #(* % %) (range 100)))
        (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-208ef0e3
  (fn [s]
    (let [nums (map parse-int (clojure.string/split s #","))
          largest (apply max nums)
          perfect-square? (set (take-while #(<= % largest) (map #(* % %) (range))))]
      (clojure.string/join "," (filter perfect-square? nums)))))

(defcheck solution-20b158ef
  (fn [s]
    (reduce #(str %1 "," %2)
      (filter
        (fn pf [x]
          (= 1
            (count
              (filter #(= x (* % %)) (range x))
              )
            )
          )
        (map #(parse-int %) (re-seq #"[0-9]+" s))
        )
      )
    ))

(defcheck solution-20f16296
  (fn find-squares [s] (apply str (interpose "," (filter #(= (mod (Math/sqrt (parse-int %)) 1) 0.0) (re-seq #"\w+" s))))))

(defcheck solution-2106fab8
  (fn [s]
    (reduce #(str %1 "," %2)
      (sort (clojure.set/intersection
              (set (map #(* % %) (range 0 100)))
              (set (sort (map parse-int (clojure.string/split s #"\,")))))))))

(defcheck solution-21126907
  (fn [x]
    (let [ num_seq (map parse-int (re-seq #"[0-9]+" x))
          squares (map #(* %1 %1) (iterate inc 1))
          in?     (fn [n l ]
                    (let [[h & t] l]
                      (cond
                        (empty? t) false
                        (< n h)    false
                        (= n h)    n
                        :else (recur n t))))
          str-nums (fn [s]  (clojure.string/join "," s))]
      (str-nums (filter #(in? %1 squares) num_seq)))))

(defcheck solution-2114d5c5
  (fn [s] (clojure.string/join "," (filter (fn [n] (some #(= (* % %) n) (range n))) (map parse-int (clojure.string/split s #","))))))

(defcheck solution-21624a8f
  (fn [s]
    (clojure.string/join ","
      (filter #(let [n (Math/sqrt %)] (== (int n) n))
        (map parse-int
          (clojure.string/split s #","))))))

(defcheck solution-219f8c2
  (fn[s](apply str (interpose "," (map str (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %)))) (map parse-int (re-seq #"\d+" s))))))))

(defcheck solution-21a33a8b
  (fn [s]
    (letfn [(square? [n]
              (let [sqrt (int (Math/sqrt n))]
                (= (* sqrt sqrt) n)))]
      (let [sq (clojure.string/split s #",")
            results (map #(square? (integer %)) sq)]
        (->> (interleave results sq)
          (partition 2)
          (filter #(= true (first %)))
          (map second)
          (clojure.string/join ","))))))

(defcheck solution-21d0732c
  (fn [x]
    (let [squares (fn [n]
                    (set (map #(* % %) (range n))))
          words (clojure.string/split x #",")
          nums (map #(parse-int %) words)
          sq (squares (reduce #(max %1 %2) nums))]
      (->> (filter #(sq %) nums)
        (map str)
        (clojure.string/join ","))
      )))

(defcheck solution-21d7f66a
  (fn [x] (clojure.string/join ","
            (filter #(let [s (Math/sqrt (parse-int %))] (= s (Math/floor s)))
              (re-seq #"[0-9]+" x)))))

(defcheck solution-22735636
  (fn [nstrs]
    (->> (clojure.string/split nstrs #",")
      (map #(parse-int %))
      (filter #(let [r (-> % Math/sqrt long)] (= (* r r) %)))
      (clojure.string/join ","))))

(defcheck solution-229d5f80
  (fn [s] (clojure.string/join "," (filter (fn [n] (= n (last (take-while #(<= % n) (for [x (range)] (* x x)))))) (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-22cdd422
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (interpose \,)
      (apply str)
      )))

(defcheck solution-23f92ac6
  (fn [s]
    (let [ps (fn [n] (let [sqrt (int (Math/sqrt n))] (= n (* sqrt sqrt))))]
      (->> s
        (re-seq #"\d+")
        (map #(parse-int %))
        (filter ps)
        (interpose \,)
        (apply str)))))

(defcheck solution-24115a67
  (fn [strings]
    (->> strings
      (#(clojure.string/split % #","))
      (map #(parse-double %))
      (filter (fn [n]
                (->> n
                  Math/sqrt
                  (#(rem % (Math/round %)))
                  zero?)))
      (map int)
      (clojure.string/join ","))))

(defcheck solution-242273a1
  (fn [s] (apply str (interpose "," (filter #(let [sqrt (int (Math/sqrt %))] (= (* sqrt sqrt) %))
                                      (map #(parse-int %) (clojure.string/split s #",")))))))

(defcheck solution-25e76ac9
  (fn
    [s]
    (apply str (interpose \, (filter #(let [x (int (Math/sqrt %))]
                                        (= % (* x x))) (map (fn [ns] (parse-int ns)) (re-seq #"\d+" s)))))))

(defcheck solution-2603e7f3
  (fn [x] (clojure.string/join "," (filter #(= (int (Math/pow (int (Math/sqrt %)) 2)) %) (map #(parse-int %) (re-seq #"\d+" x))))))

(defcheck solution-2611e64a
  (fn [s]
    (let [f (fn [n] (let [s (int (Math/sqrt n))] (= n (* s s))))]
      (->> (.split s ",")
        (map #(parse-int %))
        (filter f)
        (interpose ",")
        (apply str)
        ))))

(defcheck solution-26175fca
  (fn [str] (->> (clojure.string/split str #",")
              (map #(parse-int %))
              (filter #(= (double (int (Math/sqrt %)))
                         (Math/sqrt %)))
              (clojure.string/join ","))))

(defcheck solution-261c05ed
  (fn  [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(let [root (Math/sqrt %)](= root (float (int root)))))
      (clojure.string/join ","))))

(defcheck solution-2679c4cf
  (fn f [s]
    (reduce #(str % "," %2)
      (filter
        (fn is_square [n]
          (let [sqrt (int (Math/sqrt n))]
            (= n (* sqrt sqrt))))
        (map #(parse-int %) (.split s ","))))))

(defcheck solution-26e94e9c
  (fn [i]
    (let [s (map #(parse-int %) (clojure.string/split i #"\D"))
          ps? (fn [n] (let [m (Math/floor (Math/sqrt n))] (= (* m m) (double n))))]
      (clojure.string/join "," (map str (filter ps? s)))
      )))

(defcheck solution-26ea9c2e
  (fn [s] (clojure.string/join ","
            (filter (fn [n] (some #(= n (* % %)) (range n)))
              (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-2743dfae
  #(->> %
     (re-seq #"\d+")
     (map parse-int)
     (filter (set (map (fn [a] (* a a)) (range 9))))
     (clojure.string/join ",")))

(defcheck solution-27577913
  ; would benefit from an accumulator
  (fn perfect-squares[s]
    (let [perfect-squares-inner
          (fn perfect-squares-inner
            [s]
            (if (empty? s)
              (list)
              (if (== (int (Math/sqrt (first s))) (Math/sqrt (first s)))
                (cons (first s) (perfect-squares-inner (rest s)))
                (perfect-squares-inner (rest s)))))]
      (clojure.string/join "," (map str (perfect-squares-inner (map parse-int (clojure.string/split s #","))))))))

(defcheck solution-2793374f
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %))))
      (interpose ",")
      (apply str)
      )
    ))

(defcheck solution-27d982f5
  (fn [xs] (apply str (interpose "," (filter #(let [n (integer %) sqrt (int
                                                                          (Math/sqrt n))] (= n (* sqrt sqrt))) (re-seq #"\d+" xs))))))

(defcheck solution-284993bf
  (fn [s]
    (->>
      (.split s ",")
      (map #(parse-int %))
      (filter (fn [n] (some #(= (/ n %) %) (range 1 n))))
      (map #(.toString %))
      (clojure.string/join ","))))

(defcheck solution-28ce72bc
  (fn [in]
    (let
     [nums
        (map parse-int (clojure.string/split in #","))
      m (apply max nums)
      squares
        (set (take-while #(>= m %) (map #(* % %) (range))))]
      (clojure.string/join "," (filter #(contains? squares %) nums)))))

(defcheck solution-29a6de25
  (fn [nums]
    (clojure.string/join ","
      (filter #(let [rt (Math/sqrt %)] (== rt (int rt)))
        (map #(parse-int %)
          (clojure.string/split nums #","))))))

(defcheck solution-29bf8db
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(= 0.0 (rem (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-2a8f7c9e
  (fn [s]
    (clojure.string/join "," (filter #(= (parse-int %) (int (Math/pow (int (Math/sqrt (parse-int %))) 2)))
                               (re-seq #"[0-9]+" s)))))

(defcheck solution-2b6eae14
  (comp (partial clojure.string/join ",")
        (partial filter #(let [rt (Math/sqrt %)] (= rt (float (int rt)))))
        (partial map parse-int)
        #(clojure.string/split % #",")
        ))

(defcheck solution-2b994772
  (fn [s]
    (let [issq (fn [x] (let [sq (int (Math/sqrt x))] (= x (* sq sq))))]
      (apply str
        (interpose
          ","
          (filter #(issq (parse-int %)) (re-seq #"[0-9]+" s)))))))

(defcheck solution-2c0c755b
  (fn [string]
    (->> (clojure.string/split string #"\W")
      (map parse-int)
      (filter #(let [x (Math/sqrt %)]
                 (= x (double (int x)))))
      (interpose ",")
      (apply str))))

(defcheck solution-2c2bc2d7
  (fn [x]
    (apply str
      (interpose \,
        (filter #{4 9 16 25 36}
          (map #(parse-int %)
            (clojure.string/split x #",")))))))

(defcheck solution-2c5fb6e0
  (fn p74 [s]
    (letfn [(mkstr [s ls b] (if (= 1 (count ls)) (str s (first ls)) (mkstr (str s (first ls) b) (next ls) b)))]
      (mkstr "" (filter (fn [x] (let [y (Math/pow (int (Math/sqrt x)) 2)] (= 0.0 (- y x)))) (map (fn [x] (parse-int x))
                                                                                              (re-seq #"[0-9]+" s))) ","))))

(defcheck solution-2c647caf
  (fn [nums]
    (clojure.string/join ","
      (let [coll (clojure.string/split nums #",")]
        (filter #(let [n (parse-int %) r (Math/sqrt n)] (== r (int r))) coll)))))

(defcheck solution-2ced99a
  (fn [s]
    (letfn [(perfect-square? [n]
              (== (Math/sqrt n) (int (Math/sqrt n))))]
      (clojure.string/join ","
        (filter perfect-square?
          (map parse-int (clojure.string/split s #",")))))))

(defcheck solution-2d08593c
  (fn [astr]
    (let [aseq (map parse-int (clojure.string/split astr #","))]
      (->>
        (for [i aseq
              :when (= 0.0 (- (Math/sqrt i) (int (Math/sqrt i))))]
          i)
        (clojure.string/join ",")))))

(defcheck solution-2d14298f
  (fn [input-str]
    (clojure.string/join "," (filter #(let [sqrt (Math/sqrt (parse-int %))]
                                        (= 0.0 (- sqrt (int sqrt)))) (clojure.string/split input-str #"\,")))))

(defcheck solution-2d6a6cf4
  (fn [s]
    (letfn [(is-square? [x] (-> x Math/sqrt (mod 1) (= 0.0)))]
      (->> s (re-seq #"\d+") (map #(parse-int %))
        (filter is-square?) (clojure.string/join ",")))))

(defcheck solution-2d9684f
  (fn [s]
    (->> (map #(parse-int %) (re-seq #"\d+" s))
      (filter #(zero? (rem (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-2dcb7a68
  (fn [s]
    (let [vals (map #(parse-int %) (.split s ","))
          is-sq? (fn [i] (let [r (Math/sqrt i)] (== r (int r))))]
      (apply str (interpose "," (filter is-sq? vals))))))

(defcheck solution-2e0aa736
  (fn [numstr]
    (let [splitstr (clojure.string/split numstr #",")
          fnum (parse-int (first splitstr))
          lnum (parse-int (last splitstr))
          outp     (drop-while #(< % fnum)
                     (take-while #(<= % lnum)
                       (map #(* % %)
                         (iterate inc 1))))]
      (str (apply str (map #(str % ",") (butlast outp))) (last outp)))))

(defcheck solution-2e1d95b1
  (fn perfSq [in]
    (->>
      (map #(parse-int %) (re-seq #"\d+" in))
      (filter (fn [x] (some #(= x %) (map #(* % %) (range x)))))
      (interpose ",")
      (apply str))
    ))

(defcheck solution-2e220a16
  (fn [s]
    (let [nums (map parse-int
                 (re-seq #"\d+" s))]
      (->> (for [x nums
                 :when (zero? (let [sqrt (Math/sqrt x)]
                                (- sqrt (int sqrt))))]
             x)
        (clojure.string/join ",")))))

(defcheck solution-2e9596af
  (letfn [(perfect-square? [n]
            (let [r (int (Math/sqrt n))]
              (= n (* r r))))]
    (fn [s]
      (->> s
        (re-seq #"\d+")
        (map parse-int)
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-2f2f748f
  (fn [s]
    (letfn
     [(ps [n]
        (let [rt (int (Math/sqrt n))]
          (= n (* rt rt))))]
      (apply str
        (interpose ","
          (map str
            (filter ps
              (map #(parse-int %)
                (re-seq #"\d+" s)))))))))

(defcheck solution-3004b45f
  (fn [s] (clojure.string/join "," (filter (fn [n] (not-empty (drop-while #(not= (* % %) n) (range 2 n)))) (map #(integer %) (re-seq #"\d+" s))))))

(defcheck solution-30346c8c
  (fn prob74 [^String s]
    (letfn [(isqrt-step [n [x_k _]]
              (let [x_k1 (* 0.5 (+ x_k (/ n x_k)))
                    delta (* (- x_k1 x_k) (- x_k1 x_k))]
                [x_k1 delta]))
            (isqrt [n]
              (let [x0 (/ n 2)
                    newton-seq (iterate (partial isqrt-step n) [x0 10])]
                ((comp int #(Math/floor ^Double %) first first (partial take 1))
                 (drop-while #(> (second %) 1.0) newton-seq))))
            (isgolden?  [n]
              (let [isqrtn (isqrt n)]
                (== (* isqrtn isqrtn) n)))]
      (apply
        str
        (interpose
          ","
          (filter
            isgolden?
            (map
              #(parse-int ^String %)
              (vec (.split s ",")))))))))

(defcheck solution-307065f4
  #({\4 "4,9" \1 "16,25,36"} (first %)))

(defcheck solution-310818e9
  (fn [s]
    (clojure.string/join
      ","
      (filter #(some (fn [x]
                       (= (int (Math/pow x 2))
                         (parse-int %)))
                 (range (parse-int %)))
        (clojure.string/split s #",")))))

(defcheck solution-3136664d
  (fn [s]
    (let [mstr (clojure.string/split s #"[\s,]+")]
      (clojure.string/join "," (filter integer? (map (fn [nstr] (let [n (parse-int nstr)](when (zero? (mod n (Math/sqrt n))) n))) mstr))))
    ))

(defcheck solution-314d73b8
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map parse-int)
      (filter (fn [n] (some #(= (* % %) n) (range n))))
      (interpose ",")
      (apply str))))

(defcheck solution-315a05de
  (fn[s] (clojure.string/join "," (filter (fn[x] (not (zero? (count (filter #(= (* % %) x) (range x)))))) (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-31f9d298
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(= (* (int (Math/sqrt %))(int (Math/sqrt %))) %))
      (map str)
      (interpose ",")
      (apply str))))

(defcheck solution-3205d963
  (fn [s]
    (letfn [(perfect-square? [n]
              (let [sqrt (Math/sqrt n)]
                (== sqrt (int sqrt))))]
      (->> s
        (re-seq #"\d+")
        (map #(parse-int %))
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-329656a6
  (fn filter-squares [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(< (rem (Math/sqrt %) 1) 0.0001))
      (clojure.string/join ","))))

(defcheck solution-3297ee7
  (fn [s]
    (apply str
      (interpose \,
        (filter (fn [n] (let [sqrt (Math/sqrt (parse-int n))]
                          (== sqrt (int sqrt))))
          (clojure.string/split s #"\,"))))))

(defcheck solution-32bb8a34
  (fn [s]
    (let [nums (clojure.string/split s #",")
          squares (for [n (range)] (* n n))
          square? (fn [n] (= (first (drop-while #(< % (parse-int n)) squares)) (parse-int n)))]
      (apply str (interpose "," (filter square? nums))))))

(defcheck solution-32db98d4
  (fn [s]
    (clojure.string/join ","
      (filter #(let [sqrt (Math/sqrt %)] (== sqrt (int sqrt)))
        (map #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-3302df25
  (fn [s]
    (letfn [(is-square [n]
              (let [r (Math/sqrt n)
                    r (int r)]
                (= (* r r) n)))]
      (-> s
        (clojure.string/split #",")
        (->> (map (fn [s] (parse-int s)))
          (filter is-square)
          (map str)
          (clojure.string/join ","))))))

(defcheck solution-337c7967
  (fn [s]
    (let [nums (into #{}(map parse-int (re-seq #"[0-9]+" s)))
          maxnum (apply max nums)
          squares (take-while #(>= maxnum %)(map #(* % %) (range)))]
      (apply str (interpose \, ( filter (partial contains? nums) squares) )))))

(defcheck solution-3428a3d0
  (fn [text]
    (->> (re-seq #"\d+" text)
      (map #(parse-int %))
      (filter #(let [i (int (Math/sqrt %))] (= % (* i i))))
      (interpose ",")
      (apply str))))

(defcheck solution-342d672a
  (fn [s] (clojure.string/join "," (filter (fn [y] (let [sqrt (int (Math/sqrt y))] (= (* sqrt sqrt) y))) (map #(integer %) (clojure.string/split s #","))))))

(defcheck solution-348b5ab5
  (fn [s] (let [int-list (map #(parse-int %) (clojure.string/split s #","))
                isPerfectSquare? (fn [n] (== (Math/sqrt n) (int (Math/sqrt n))))
                square-list (filter isPerfectSquare? int-list)
                ]
            (clojure.string/join "," square-list)
            )
    ))

(defcheck solution-350e15c8
  (fn [input] (clojure.string/join "," (filter #(let [i (parse-int %) sqrt (Math/sqrt i) floor (Math/floor sqrt)] (< (- sqrt floor) 0.00000001)) (clojure.string/split input #",")))))

(defcheck solution-3513b0b8
  (fn [seq]
    (->> seq
      (re-seq #"[0-9]+")
      (filter #(let [root (Math/sqrt (parse-int %))]
                 (== root (int root))))
      (interpose ",")
      (apply str))))

(defcheck solution-356879bd
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(let [r (Math/sqrt %)] (== (int r) r)))
      (interleave (repeat ","))
      (next)
      (apply str))))

(defcheck solution-357f784d
  (fn [s] (apply str (interpose \, (filter #{"4" "9" "16" "25" "36"} (re-seq #"\d+" s))))))

(defcheck solution-359270dc
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-35a3b3
  (fn [s] (clojure.string/join "," (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %))))
                                     (map #(parse-int %) (clojure.string/split s #"," ))))))

(defcheck solution-35aaf9b3
  #(clojure.string/join "," (filter (fn [digit] (= 0.0 (mod (Math/sqrt digit) 1))) (map parse-int (clojure.string/split % #",")))))

(defcheck solution-35cb6046
  (fn
    [st]
    (let [nums (map #(parse-int %) (re-seq #"[0-9]+" st))
          maxn (apply max nums)
          squares (set (take-while (partial >= maxn) (map (fn [n]
                                                            (let [n (+ 1 n)]
                                                              (* n n)))
                                                       (range))))]
      (apply str (interpose "," (filter squares nums))))))

(defcheck solution-35dc5cd6
  #(clojure.string/join "," (filter (fn [n] (let [x (int (Math/sqrt n))]
                                              (= n (* x x))))
                              (map (fn [x] (parse-int x)) (clojure.string/split %, #",")))))

(defcheck solution-3617161a
  #(clojure.string/join "," (filter (fn [i] (let [sqrt (Math/sqrt (integer i))]
                                              (= (Math/ceil sqrt) (Math/floor sqrt)))) (re-seq #"\d+" %))))

(defcheck solution-36633f5c
  (fn [s]
    (clojure.string/join "," (filter #(= (Math/pow (Math/sqrt %) 2) (float %)) (map parse-int (clojure.string/split s #","))))))

(defcheck solution-366c6568
  (fn [s] (apply str (interpose "," (filter (fn [n] (and n
                                                         (some #(when (= (* % %) n) %) (range 1 (inc (quot n 2))) ) ) )
                                      (map #(parse-int %) (re-seq #"\d+" s ) ) )))))

(defcheck solution-3686b028
  (fn [s]
    (apply str
      (interpose \,
        (filter #(some (fn [x] (= (* x x) %)) (range %))
          (map #(integer %) (re-seq #"\d+" s)))))))

(defcheck solution-36c80c0
  (fn [x]
    (letfn [(square? [n] (some #(= n (* % %)) (range (inc n))))]
      (apply str (interpose "," (filter square?
                                  (map #(parse-int %)
                                    (re-seq #"[0-9]+" x))))))))

(defcheck solution-370465b6
  (fn [s](clojure.string/join ","( filter #(= (Math/pow (Math/sqrt %) 2) (float %))
                                   (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-37939a3e
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter (fn [x] (some #{x} (map #(* % %) (range x)))))
      (clojure.string/join ","))))

(defcheck solution-38851b7b
  (fn [input]
    (->> (clojure.string/split input #",")
      (map #(parse-int (str %)))
      (filter (fn [n]
                (let [sqrt-n (int (Math/sqrt n))]
                  (= n (* sqrt-n sqrt-n)))))
      (clojure.string/join ","))))

(defcheck solution-38e270ca
  (fn [s] (->> (.split s ",")
            (map #(parse-int %))
            (filter #(let [r (int (Math/sqrt %))] (= % (* r r))))
            (interpose \,)
            (apply str))))

(defcheck solution-391ee581
  (fn [s]
    (clojure.string/join ","
      (filter #(=
                 (Math/sqrt (parse-int %))
                 (Math/floor (Math/sqrt (parse-int %))))
        (clojure.string/split s #",")))))

(defcheck solution-398ff193
  (fn [num-str]
    (let [nums (clojure.string/split num-str #",")
          nums-as-ints (map parse-int nums)
          perfect-sqrs (set (map #(* % %) (range 100)))
          filtered (filter perfect-sqrs nums-as-ints)]
      (apply str (interpose "," filtered)))))

(defcheck solution-39b88543
  (fn [s]
    (let [num-seq (map #(parse-int %) (re-seq #"\d+" s))]
      (apply str (interpose "," (filter (set (take-while #(<= % (apply max num-seq)) (map #(* % %) (range)))) num-seq))))))

(defcheck solution-3a0dd7d0
  (fn x[s](let[p (set (map #(* % %) (range 7)))
               nums (map parse-int (re-seq #"\d+" s))
               perfects (filter p nums)]
            (apply str (interpose "," perfects)))))

(defcheck solution-3a1df2cb
  (fn [s]
    (-> s
      (clojure.string/split #",")
      (->> (map #(parse-int %))
        (filter (fn [v]
                  (let [sqrt (Math/sqrt v)]
                    (= (Math/floor sqrt) (Math/ceil sqrt)))))
        (clojure.string/join ",")))))

(defcheck solution-3a3dd549
  (fn [s]
    (letfn [(msqrt [x] (Math/sqrt x))]
      (apply str
        (interpose "," (filter #(== (msqrt %) (int (msqrt %)))
                         (map parse-int (clojure.string/split s #","))))))))

(defcheck solution-3abe76c
  (fn [csv]
    (let [nums (map #(parse-int %) (re-seq #"\d+" csv))
          squares (filter #(-> % Math/sqrt (rem 1) zero?) nums)]
      (clojure.string/join "," squares))))

(defcheck solution-3acd7c3
  (fn [input]
    (loop [nums (map #(parse-int %) (clojure.string/split input #","))
           output ""]
      (if (empty? nums)
        output
        (let [num (first nums)
              append (str (if (= 0.0 (rem num (Math/sqrt num))) num ""))
              sep (if (empty? append) "" (if (empty? output) "" ","))]
          (recur (rest nums) (str output sep append)))))))

(defcheck solution-3aefcd0
  (fn perfectSquares
    [str]
    (let [lst (clojure.string/split str #",")
          isPerfect (fn [num] (if (empty?
                                    (for [x (range num)
                                          :when (= num (* x x))]
                                      true)) false true))]
      (clojure.string/join ","
        (filter
          (fn [elem] (isPerfect (integer elem)))
          lst)))))

(defcheck solution-3af911f6
  (fn f [s] (apply str (interpose \, (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))) (map parse-int (clojure.string/split s #"\W")))))))

(defcheck solution-3b2ace75
  (fn [s]
    (->> (.split s ",")
      (map parse-int)
      (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %))))
      (clojure.string/join ",") )))

(defcheck solution-3b8baef5
  (fn
    [input]
    (let [
          numbers (map #(parse-int %) (re-seq #"\d+" input))
          squares (map #(* % %) (iterate inc 1))
          is-square? #(= % (first (drop-while (fn [v] (< v %)) squares)))
          ]
      (apply str (interpose \, (filter is-square? numbers)))
      )
    ))

(defcheck solution-3cb87f79
  (fn filter-perf-sq [s]
    (clojure.string/join "," (keep #(if (= (- (* (Math/sqrt %) (Math/sqrt %)) %) 0.0) % )
                               (map #(parse-int %) (.split s ","))
                               ))
    ))

(defcheck solution-3cdc0bfa
  (fn [s]
    (let [sts 			(clojure.string/split s #",")
          nums 		(map parse-int sts)
          is_sq? 		(fn [x](= 0.0 (mod x (Math/sqrt x))))
          filtered 	(filter is_sq? nums)
          end-res 		(apply str (interpose "," filtered))]
      end-res)))

(defcheck solution-3d1de0c6
  (fn [s]
    (let
     [
      squares
      (set
        (map
          (fn [n]
            (* n n)
            )
          (range 100)
          )
        )
      ]
      (clojure.string/join
        ","
        (map
          str
          (filter
            (fn [n]
              (get squares n)
              )
            (map
              parse-int
              (clojure.string/split s #",")
              )
            )
          )
        )
      )
    ))

(defcheck solution-3d3712a1
  #(apply str (interpose "," (filter #{"4" "9" "16" "25" "36"} (re-seq #"[0-9]+" %)))))

(defcheck solution-3de9b7d6
  #(if (= (first %) \4) "4,9" "16,25,36"))

(defcheck solution-3e021113
  (fn fps [s]
    (apply str
      (interpose ","
        (filter #(zero? (rem (Math/sqrt (parse-int %)) 1))
          (re-seq #"\d+" s))))))

(defcheck solution-3e0d0159
  (fn[s] (apply clojure.core/str (interpose ","  (filter #(some (fn [x] (= % (* x x))) (range 1 %)) (map parse-int (clojure.string/split s #",")))))))

(defcheck solution-3e528863
  (fn [s]
    (clojure.string/join ","
      (filter #(= % (let [x (Math/round (Math/sqrt %))] (* x x)))
        (map parse-int (clojure.string/split s #","))))))

(defcheck solution-3e6b72e0
  (fn [s]
    (let [ns (map #(parse-int %) (re-seq #"\d+" s))
          square? #(let [sqrt (int (Math/sqrt %))] (= % (* sqrt sqrt)))]
      (apply str (interpose \, (filter square? ns))))))

(defcheck solution-3e6df67f
  (fn [s]
    (letfn [(p? [x] (some #(= x %) (map #(* % %) (range 1 x))))]
      (->>
        (-> s
          (clojure.string/split #","))
        (map #(parse-int %))
        (filter p?)
        (clojure.string/join ",")))))

(defcheck solution-3eba9e0f
  (letfn [(square? [n]
            (some #(= n (* % %)) [(dec (int (Math/sqrt n)))
                                  (int (Math/sqrt n))
                                  (inc (int (Math/sqrt n)))]))]
    (fn [s] (clojure.string/join ","
              (filter square? (map #(integer %)
                                (clojure.string/split s #",")))))))

(defcheck solution-40530309
  (fn [s]
    (let [ bad-is-perfect-square #{1,4,9,16,25,36} ]
      (apply str
        (interpose ","
          (filter bad-is-perfect-square
            (map parse-int (clojure.string/split s #","))))))))

(defcheck solution-40971785
  (fn [nstring]
    (let [coll (map #(parse-int %) (clojure.string/split nstring #","))
          o (for [x coll]
              (if (<= (rem (if (> x 0) (Math/sqrt x) 0) 1) 0.00001)
                (conj [] x)
                "no"))
          close (flatten (remove #{"no"} o))]
      (clojure.string/join "," close))))

(defcheck solution-4142c3a9
  (fn[x]
    (clojure.string/join ","
      (filter #(= 0.0 (mod (Math/sqrt %) 1))
        (map parse-int
          (clojure.string/split x #","))))))

(defcheck solution-41589a51
  (fn [s]
    (let [isSquare? (fn [x] (= x (last (take-while (partial >= x) (map #(* % %) (range))))))]
      (->> (clojure.string/split s #",")
        (map #(parse-int %))
        (filter isSquare?)
        (clojure.string/join ",")))))

(defcheck solution-42487a02
  (fn [s]
    (let [square? (fn [x] (= x (first (drop-while #(< % x) (map #(* % %) (range))))))
          nums (map #(integer %) (clojure.string/split s #","))]
      (clojure.string/join "," (filter square? nums)))))

(defcheck solution-4272480f
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter (fn [x]
                (loop [i 1]
                  (let [i_sqr (* i i)]
                    (cond
                      (= x i_sqr) true
                      (< x i_sqr) false
                      :else (recur (inc i)))))))
      (clojure.string/join ","))))

(defcheck solution-4300d6cf
  (fn [s]
    (apply str (rest (interleave (repeat ",")
                       (filter  #(== (Math/floor (Math/sqrt %)) (Math/ceil (Math/sqrt %)))
                         (map #(parse-int %) (filter #(not= % ",") (map #(apply str %) (partition-by #(= % \,) s))))))))))

(defcheck solution-430b173b
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(let [q (Math/sqrt %)] (== q (int q))))
      (interpose ",")
      (apply str))))

(defcheck solution-43b15338
  (fn [csv] (apply str (interpose "," (filter #(zero? (let [d (Math/sqrt %)] (- d (Math/floor d)))) (map #(parse-int %) (re-seq #"[^,]+" csv)))))))

(defcheck solution-43b7d668
  (fn [xs]
    (apply str
      (interpose ","
        (filter
          #(let [n (integer %)
                 sqrt (int (Math/sqrt n))]
             (= n (* sqrt sqrt)))
          (re-seq #"\d+" xs))))))

(defcheck solution-4411e8aa
  (fn [S]
    (letfn [(square? [x] (some #(= x (* % %)) (range x)))]
      (clojure.string/join "," (filter square?
                                 (map parse-int (clojure.string/split S #",")))))))

(defcheck solution-441b150
  (fn [s] (clojure.string/join "," (filter (fn [n]  (let [m (Math/sqrt n)] (= (double n) (* m m)))) (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-446da3cb
  (fn
    [s]
    (let [l (map #(parse-int %) (clojure.string/split s #","))]
      (clojure.string/join
        ","
        (filter
          (fn [n]
            (== n
              (#(* % %)
               (Math/sqrt n))))
          l)))))

(defcheck solution-4506e604
  (fn [string]
    (let [nums 				(map #(parse-int %) (clojure.string/split string #","))
          perfect_square?		(fn [n] (= n (let [root (int (Math/sqrt n))] (* root root))))
          perfect_squares 	(filter perfect_square? nums)]
      (clojure.string/join "," perfect_squares))))

(defcheck solution-4525aab9
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter (fn [x] (some  #(= x (* % %)) (range 1 (inc x)))))
      (clojure.string/join ","))))

(defcheck solution-453819d3
  ;; rely on sqrt being irrational, thus not representable in double prec
  (fn [x]
    (clojure.string/join ","
      (filter #(let [r (Math/sqrt %)]
                 (== (* r r) %))
        (map #(parse-int %)
          (clojure.string/split x #","))))))

(defcheck solution-4563b926
  (fn [input]
    (letfn [
            (parse [input] (map #(integer %) (.split input ",")))
            (perfect? [n] (zero? (rem (Math/sqrt n) 1)))
            (csv [nums] (reduce #(str %1 "," %2) nums))
            ] (csv (filter perfect? (parse input))))))

(defcheck solution-45bf2166
  (fn [s] (apply str (interpose ","
                       (map str
                         (filter
                           #(zero? (mod (Math/sqrt %) 1))
                           (map parse-int
                             (clojure.string/split s #","))))))))

(defcheck solution-45d79141
  (fn [s] (clojure.string/join "," (filter
                                     #(let [sr (int (Math/sqrt %))] (= % (* sr sr)))
                                     (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-4632b9fe
  (fn [s]
    (clojure.string/join ","
      (filter #(== % (Math/pow (Math/floor (Math/sqrt %)) 2))
        (map parse-int
          (clojure.string/split s #","))))))

(defcheck solution-46cd2443
  (fn [nums]
    (loop [result [] elements (clojure.string/split nums #",")]
      (if elements
        (recur
          (if  (= (mod (Math/sqrt (parse-int (first elements))) 1) 0.0)
            (conj result (parse-int (first elements)))
            result
            )
          (next elements))
        (clojure.string/replace (re-find #"[\d+ ]+" (str result)) #" " ",")
        )
      )
    ))

(defcheck solution-47b6a389
  (fn [s]
    (let [l (re-seq #"\d+" s)]
      (clojure.string/join "," (filter #{"4" "9" "16" "25" "36"} l)))))

(defcheck solution-4800d13e
  (fn [s]
    (let [coll (map #(parse-int %) (clojure.string/split s #","))]
      (->> coll
        (map #(range 1 (inc (quot % 2))))
        (mapcat (fn [numr coll]
                  (mapcat (fn [denom]
                            (let [res (* denom denom)]
                              (if (= res numr)
                                [res]
                                [])))
                    coll))
          coll)
        (interpose ",")
        (apply str)))))

(defcheck solution-484a8a6a
  (fn [args]
    (let [lt  (into [] (map parse-int (clojure.string/split args #",") ))
          psq (vec (map #(* % %) (range 2 10)))
          ins (fn [ps x] (if ((set ps) x) true false)) ]
      (apply str (interpose "," (filter  #(ins psq %) lt)))
      )
    ))

(defcheck solution-484cf394
  (fn filter-squares [s]
    (apply str (interpose "," (map str (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %)))
                                         (map #(parse-int %)
                                           (clojure.string/split s #"\W"))))))))

(defcheck solution-489c1a14
  ;(filter #((let [v (int %)] = (* (int (Math/sqrt v)) (int (Math/sqrt v)) )  v)))
  (fn [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          psquare? (fn [n] (let [sqrt (Math/sqrt n)] (= (Math/floor sqrt) sqrt)))
          perfect (filter psquare? nums)]
      (apply str (interpose "," perfect)))))

(defcheck solution-48ed0815
  (fn [s]
    (let [is-square? (fn [n] (contains? #{4 9 16 25 36} n))]
      (->> (.split s ",")
        (map #(parse-int %))
        (filter is-square?)
        (clojure.string/join ",")))))

(defcheck solution-495c8bbb
  (fn [s]
    (let [split (fn [s] (map #(parse-int %) (re-seq #"\d+" s)))
          join (fn [s] (apply str (interpose "," s)))
          square? (fn [n] (let [r (Math/round (Math/sqrt n))] (= (* r r) n)))]
      (join (filter square? (split s))))))

(defcheck solution-496c8d98
  (fn [s]
    (letfn [(i[s] (map #(parse-int %) (re-seq #"\d+" s)))]
      (apply str (interpose \,
                   (keep #(let [sq (Math/sqrt %)]
                            (when (== (* sq sq) %) %))
                     (i s)))))))

(defcheck solution-4a570ca
  (fn f[s]
    (apply str (interpose "," (filter
                                (fn [x] (let [y (Math/sqrt x)] (zero? (- y (Math/round y)))))
                                (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-4b1f33d3
  (fn [s] (->> (map parse-int (re-seq #"\d+" s))
            (filter #(let [r (Math/sqrt %)] (== r (int r))))
            (interpose ",")
            (apply str))))

(defcheck solution-4b3c8d62
  (fn ps [s]
    (->> (re-seq #"\d+" s)
      (map parse-int)
      (filter #(= 0.0 (mod (Math/sqrt %) 1)))
      (interpose ",")
      (apply str))))

(defcheck solution-4b6ac507
  (fn [x] (clojure.string/join "," (filter #(zero? (mod (Math/sqrt (parse-int %)) 1)) (re-seq #"\d+" x)))))

(defcheck solution-4bc0a212
  (fn [string]
    (let [ data (map #(parse-int %) (clojure.string/split  string  #","))
          square?  (fn [x] (let [t (-> x Math/abs Math/sqrt)]
                             (= t (double (int t)))))
          squares (filter square? data)]
      (apply str (interpose "," squares)))))

(defcheck solution-4be5ee2d
  (fn [s]
    (let [sq? (fn [y] (let [x (int (Math/sqrt y))] (= y (* x x))))
          xs  (map (fn [y] (parse-int y)) (clojure.string/split s #","))]
      (clojure.string/join "," (filter sq? xs)))))

(defcheck solution-4c1c441e
  (fn [s]
    (let [ns (map #(parse-int %) (re-seq #"\d+" s))
          pfsqs (remove nil? (map (fn [x]
                                    (let [isqr (-> x Math/sqrt (+ 0.5) int)]
                                      (if (= x (* isqr isqr)) x))) ns))]
      (->> pfsqs (interpose \,) (apply str)))))

(defcheck solution-4d405040
  (fn [s]
    (clojure.string/join ","
      (filter #(let [r (Math/sqrt %)] (= r (float (int r))))
        (map parse-int
          (re-seq #"\d+" s))))))

(defcheck solution-4d93d5ea
  (fn perfect-squares [num-string]
    (reduce
      (fn [a b] (str a "," b))
      (filter
        (fn [x] (== (int (Math/sqrt x)) (Math/sqrt x)))
        (map #(parse-int %) (clojure.string/split num-string #","))))))

(defcheck solution-4e06761b
  (fn [s]
    (clojure.string/join ","
      (filter
        #(zero? (rem (Math/sqrt (parse-int %)) 1))
        (clojure.string/split s #"\D")))))

(defcheck solution-4e32f7e6
  (fn [i] (clojure.string/join "," (filter #(= 0.0 (mod (Math/sqrt (parse-int %)) 1)) (clojure.string/split i #","))  )))

(defcheck solution-4eb64f20
  (fn [num-str] (clojure.string/join "," (filter #(let [sq (Math/sqrt %) rd (dec sq) ru (inc sq)] (= % (apply * (repeat 2 (first (drop-while (fn [n] (< n sq)) (range ru))))))) (map (fn [c] (-> c parse-int)) (clojure.string/split num-str #","))))))

(defcheck solution-4eea1025
  (fn [s]
    (let [perfect-square? (fn [n] (== (Math/sqrt n) (int (Math/sqrt n))))]
      (->> (clojure.string/split s #",")
        (map #(parse-int %))
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-4f6857c3
  (let [ips?
        (fn ips [x]
          (if (or (= 4 (integer x))
                  (= 9 (integer x))
                  (= 16 (integer x))
                  (= 25 (integer x))
                  (= 36 (integer x))) true false))]

    (fn fps [s] (clojure.string/join ","
                  (filter ips?
                    (clojure.string/split s #","))))))

(defcheck solution-50641427
  (fn [s]
    (apply str (interpose ","
                 (filter
                   (fn [n] (some #(= (* % %) n) (range 2 n)))
                   (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-50afad7e
  (fn [s]
    (let [n (map #(parse-int %) (clojure.string/split s #","))
          f #(apply str (interpose "," (map (comp str int) %)))]
      (f (filter #(zero? (rem (Math/sqrt %) 1)) n)))))

(defcheck solution-5132bfab
  (fn [s]
    (let [nums (map #(parse-int %) (re-seq #"\d+" s))
          nums->str (partial clojure.string/join ",")]
      (nums->str
        (filter (fn [n]
                  (let [sqrt (int (Math/sqrt n))]
                    (= n (* sqrt sqrt))))
          nums)))))

(defcheck solution-514cac1f
  (fn [s]
    (apply str (interpose ","
                 (filter #(let [i (Math/sqrt (parse-int %))]
                            (== i (int i)))
                   (clojure.string/split s #","))))))

(defcheck solution-518488d5
  (fn sq [s]
    (let [perfect-square? (fn [n] (#(= %1 (* %2 %2)) n (int (Math/sqrt n))))]
      (clojure.string/join ","
        (filter perfect-square?
          (map #(parse-int %)
            (clojure.string/split s #",")
            )
          )
        )
      )
    ))

(defcheck solution-51b61954
  (fn [s] (apply str (interpose "," (filter #(zero? (mod (Math/sqrt (parse-int %)) 1)) (re-seq #"[0-9]+" s))))))

(defcheck solution-5235cae2
  (fn [x] (letfn [(q [n] (Math/pow n 0.5))]
            (clojure.string/join "," (filter #(== (q %) (int (q %)))
                                       (map parse-int (clojure.string/split x #",")))))))

(defcheck solution-525dcb13
  (fn [num-string]
    (clojure.string/join ","
      (filter
        #(== % (Math/pow (int (Math/sqrt %)) 2))
        (map #(integer %) (clojure.string/split num-string #","))))))

(defcheck solution-526dd987
  (fn [s]
    (clojure.string/join \,
      (filter #(== (mod (Math/sqrt (parse-int %)) 1) 0)
        (clojure.string/split s #",")))))

(defcheck solution-52b10c8e
  (fn fps [coll]
    (let [ps? (fn [n]
                (let [root (Math/pow n (/ 1 2))]
                  (zero? (mod (int root) root))))]
      (apply str (interpose "," (filter ps? (map #(parse-int %)(clojure.string/split coll #"\,"))))))))

(defcheck solution-53394bdf
  (fn [s]
    (letfn [(ps? [n] (let [r (int (Math/sqrt n))] (= n (* r r))))]
      (clojure.string/join
        ","
        (filter ps? (map parse-int (clojure.string/split s #",")))))))

(defcheck solution-533e42f3
  (fn [f x]
    (apply str (butlast (interleave (f x) (repeat \,))))) (fn [x]
                                                            (filter #(zero? (rem % (Math/sqrt %)))
                                                              (map #(parse-int %) (re-seq #"\d+" x)))))

(defcheck solution-5354e7a7
  ;; solution using a lazy-seq approach + recursivity:
  (fn filter-perf-squares [seq-str]
    (letfn [(pow-x-start-with
              [start]
              (letfn [(pow [x] (reduce * [x x]))]
                (lazy-seq
                  (cons (pow start) (pow-x-start-with (inc start))))))
            (split-by-comma
              [str-seq]
              (map parse-int (clojure.string/split str-seq #",")))
            (find-squares
              [lazy-pows-seq xs acc]
              (if (empty? xs)
                (apply str (interpose "," acc))
                (if (> (first lazy-pows-seq) (apply max xs))
                  (recur (rest lazy-pows-seq) (rest xs) acc)
                  (if-let [contained-x ((set xs) (first lazy-pows-seq))]
                    (recur (rest lazy-pows-seq) (rest xs) (conj acc contained-x))
                    (recur (rest lazy-pows-seq) xs acc)))))]
      (find-squares (pow-x-start-with 2) (split-by-comma seq-str) []))))

(defcheck solution-538bf7f3
  (fn [str]
    (clojure.string/join ","
      (filter
        #(let [x (int (Math/sqrt %))] (= % (* x x)))
        (map #(parse-int %) (re-seq #"\d+" str))))))

(defcheck solution-53dcbf1d
  (fn fps [s]
    (let [str-a (clojure.string/split s #",")
          num-a (map parse-int str-a)
          ss (map #(* % %) (range 2 10))
          ret (filter (set ss) (set num-a))
          ret-s (map str (sort ret))]
      (clojure.string/join "," ret-s))))

(defcheck solution-54007205
  (fn is-perfect-sqrt? [str]
    (let [coll (-> str (.split ","))]
      (clojure.string/join "," (filter #(== (int (Math/sqrt (parse-int %))) (Math/sqrt (parse-int %))) coll))
      )))

(defcheck solution-54137b4c
  (fn [s]
    (clojure.string/join ","
      (map str
        (filter #(== (int (Math/sqrt %)) (Math/sqrt %))
          (map parse-int
            (clojure.string/split s #",")))))))

(defcheck solution-54601cd6
  (fn [strn]
    (->>
      (clojure.string/split strn #",")
      (map parse-int)
      (filter
        #(=
           (Math/sqrt %)
           (Math/ceil (Math/sqrt %))))
      (clojure.string/join ","))))

(defcheck solution-548729c6
  (fn [s]
    (let [square (fn [x] (* x x))]
      (apply str (interpose "," (filter #(= (integer %) (int (square (int (Math/sqrt (integer %)))))) (clojure.string/split s #",")))))))

(defcheck solution-54d67803
  (fn [string]
    (clojure.string/join ","
      (map str
        (filter #(= 0 (compare (Math/sqrt %) (int (Math/sqrt %))))
          (map parse-int
            (clojure.string/split string #",")))))))

(defcheck solution-54ef3d4f
  (fn [s] (apply str (interpose ","
                       (filter
                         (set (map #(str (* % %)) (range 7)))
                         (re-seq #"\d+" s))))))

(defcheck solution-55304efe
  (fn [s]
    (clojure.string/join ","
      (filter #{1 4 9 16 25 36}
        (map #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-55326b1a
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(let [r (Math/sqrt %)]
                 (= r (Math/floor r))))
      (clojure.string/join ","))))

(defcheck solution-5562cf91
  (fn prfct-sqr [input]
    (let[
         numbers (map parse-int (clojure.string/split input #","))
         square? #(= 0.0 (- (Math/sqrt %) (int (Math/sqrt %))))
         ]
      (clojure.string/join "," (filter square? numbers))
      )
    ))

(defcheck solution-55730910
  (fn [s]
    (let [strlist (re-seq #"\d+" s)
          numbers (map #(parse-int %) strlist)
          square? (fn [n] (== (int (Math/sqrt n)) (Math/sqrt n)))
          squares (filter square? numbers)]
      (apply str (interpose "," (map str squares))))))

(defcheck solution-55911a9d
  (fn [s] (clojure.string/join "," (filter #(seq (for [x (range) :while (<= (* x x) %) :when (= (* x x) %) ] x)) (map #(parse-int %) (re-seq #"\d+" s))))))

(defcheck solution-55df5294
  (fn [xs] (clojure.string/join ","
             (map second
               (filter #(= (* (first %) (first %)) (second %))
                 (map #(vector (int (Math/sqrt %)) %)
                   (map (comp parse-int str) (clojure.string/split xs #","))))))))

(defcheck solution-56571290
  #(->>
     (-> % (clojure.string/split #","))
     (map (fn [n] (integer n)))
     (filter (fn [n]
               (= n
                 (last (for [perfect-square (map (fn [x] (* x x)) (range))
                             :while (>= n perfect-square)]
                         perfect-square)))))
     (clojure.string/join ",")))

(defcheck solution-56bee1e3
  (fn  squares [x] (clojure.string/join "," (filter #(= 0 (compare (Math/sqrt %) (int (Math/sqrt %)))) (map #(parse-int %) (clojure.string/split x #","))))))

(defcheck solution-5708673b
  (fn [st]
    (->> (clojure.string/split st #",")
      (map #(parse-int %))
      (filter #(= 0.0 (mod (Math/sqrt %) 1)))
      (interpose ",")
      (clojure.string/join))))

(defcheck solution-573974ac
  (fn [s]
    (->> (clojure.string/split s #",")
      (filter #(let [n (parse-int %),
                     sq (int (Math/sqrt n))]
                 (= n (* sq sq))))
      (interpose ",")
      (apply str))))

(defcheck solution-5786286
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(= (double %) (* (Math/sqrt %) (Math/sqrt %))))
      (clojure.string/join ",")
      )))

(defcheck solution-57c114
  (fn [s]
    (letfn [(is-perf-sq? [sn]
              (let [n (parse-int sn)
                    sqn (Math/sqrt n)
                    rnd (long sqn)]
                (zero? (- sqn rnd))))]
      (clojure.string/join "," (filter is-perf-sq? (.split s ","))))))

(defcheck solution-57eb0e57
  (fn [s]
    (clojure.string/join ","
      (filter
        (fn [a]
          (let [sr (int (Math/sqrt a))
                sq (* sr sr)]
            (= a sq)))
        (map #( parse-int %) (clojure.string/split s #","))))))

(defcheck solution-58f42c03
  (fn [x]
    (letfn [(ps? [n]
              (let [s (Math/sqrt n)]
                (zero? (rem s (int s)))))]
      (clojure.string/join ","
        (filter #(ps? (parse-int %)) (clojure.string/split x #","))))))

(defcheck solution-593a5cf6
  (fn [x]
    (apply str
      (interpose ","
        (map str
          (filter #(zero? (mod (Math/sqrt %) 1 ))
            (map #(parse-int %)
              (re-seq #"\d+" x))))))))

(defcheck solution-593e616c
  (fn [txt]
    (->>
      (.split txt ",")
      (map parse-int)
      (filter #(let [a (int (Math/sqrt %))]
                 (= % (* a a))))
      (interpose ",")
      (apply str))))

(defcheck solution-59e3026a
  (fn [s]
    (letfn [(sqr? [x]
              (let [n (Math/sqrt (parse-int x))]
                (== (long n) n)))]
      (apply str (interpose "," (filter sqr? (re-seq #"\d+" s)))))))

(defcheck solution-5a101443
  (fn [numstr]
    (let [isint #(== % (int %))]
      (clojure.string/join ","
        (filter #(isint (Math/sqrt %))
          (map #(parse-int %)
            (.split numstr ",")))))))

(defcheck solution-5a32aaad
  (fn [s] (clojure.string/join "," (filter #(let [sq (int (Math/sqrt %))]
                                              (= (* sq sq) %))
                                     (map #(parse-int (apply str %))
                                       (filter #(not (= (first %) \,))
                                         (partition-by #(= \, %) s)))))))

(defcheck solution-5a6cbde6
  (fn psquares [s]
    (let [nums (map #(parse-int %) (re-seq #"\d+" s))
          m (reduce max nums)
          range  (take-while #(<= % m) (map #(* % %) (iterate inc 1)))
          vals (reduce (fn [acc v] (if (some #(= v %) range)  (conj acc v) acc))[] nums)]
      (apply str (interpose "," vals))
      )
    ))

(defcheck solution-5af4206b
  (fn __ [s]
    (clojure.string/join "," (filter #{4 9 16 25 36 49} (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-5b0bd905
  (fn [str]
    (letfn [(factor [n]
              (loop [acc ()
                     m n]
                (if (= m 1)
                  acc
                  (let [r (first (filter #(zero? (rem m %)) (range 2 (+ m 1))))]
                    (recur (cons r acc) (/ m r))))))

            (perfect-square? [n]
              (every? #(even? (count %)) (vals (reduce #(merge-with concat %1 {%2 [%2]}) {} (factor n)))))]
      (clojure.string/join "," (filter perfect-square? (map parse-int (clojure.string/split str #",")))))))

(defcheck solution-5b143695
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map parse-int)
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (interpose ",")
      (apply str))))

(defcheck solution-5bf5453d
  (fn [a] (clojure.string/join ","

            (filter

              (fn [x] (= (int (Math/floor (Math/sqrt x))) (int (Math/ceil (Math/sqrt x)))))

              (map #(integer %) (re-seq #"\d+" a))))))

(defcheck solution-5c44b67c
  #(clojure.string/join "," (filter (fn[a](contains? (set (map (fn[b](* b b)) (range 20))) a)) (map parse-int (clojure.string/split % #",")))))

(defcheck solution-5c58c808
  (fn [s]
    (->> (map #(parse-int %) (re-seq #"\d+" s))
      (filter (fn [n]
                (let [sqrt (int (Math/sqrt n))]
                  (= n (* sqrt sqrt)))))
      (interpose ",")
      (apply str))))

(defcheck solution-5c9241ae
  (fn [s] (clojure.string/join "," (filter #((set (map * (range %) (range %))) %) (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-5d1e5c8e
  (fn [nrs]
    (apply str (interpose ","
                 (filter
                   (fn [x] (= x (int (Math/pow (int (Math/sqrt x)) 2))))
                   (map
                     #(parse-int %)
                     (clojure.string/split nrs #",")))))))

(defcheck solution-5d2075f2
  (fn [s]
    (letfn [(perfect-square? [n]
              (= n
                (let [r (int (Math/sqrt n))]
                  (* r r))))]
      (clojure.string/join ","
        (filter perfect-square?
          (map #(integer %)
            (clojure.string/split s #",")))))))

(defcheck solution-5d2cfa23
  (fn [s] (clojure.string/join "," (filter #(zero? (rem (Math/sqrt %) 1)) (map #(parse-int %) (re-seq #"\d+" s))))))

(defcheck solution-5d5cfb2a
  (fn  [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter (fn [x]
                ((set (map #(* % %) (range (inc x)))) x)))
      (interpose \,)
      (apply str))))

(defcheck solution-5de5187c
  (fn [s]
    (let [nums (map parse-int (clojure.string/split s #","))
          perfect-square (fn [n] (let [root (Math/sqrt n)] (= root (double (int root)))))]
      (clojure.string/join "," (filter perfect-square nums)))))

(defcheck solution-5e2b7a75
  (fn [s]
    (letfn [(perfect? [n] (let [sr (Math/sqrt n)] (= (Math/floor sr) sr)))]
      (let [ds (map #(parse-int %) (clojure.string/split s #","))]
        (clojure.string/join "," (filter perfect? ds))))))

(defcheck solution-5e7e14b7
  (fn [xs] (->>
             (clojure.string/split xs #",")
             (map parse-int)
             (filter #(let [r (Math/sqrt %)] (== % (* r r))))
             (interpose \,)
             (apply str))))

(defcheck solution-5e992fac
  (fn squares [s]
    (let* [seq (map #(parse-int %) (re-seq #"\d+" s))
           sq-seq (filter (fn [e]
                            (let [sq (Math/sqrt e)]
                              (== (* sq sq) e))) seq)]
      (apply str (interpose \, sq-seq)))))

(defcheck solution-5f1ac7dc
  (fn [x]
    (let [xs (map (fn [n] (parse-int n)) (.split x ","))
          int-sqrt (fn [n] (int (Math/floor (Math/sqrt (double n)))) )
          int-sqrt-squared (fn [n] (* (int-sqrt n) (int-sqrt n)))]
      (clojure.string/join "," (filter (fn [n] (= n (int-sqrt-squared n))) xs))
      )
    ))

(defcheck solution-5f4baa09
  (fn [s] (clojure.string/join ","
            (filter #(= 0.0 (mod (Math/sqrt (parse-int %)) 1))
              (clojure.string/split s #",")))))

(defcheck solution-5ff2ab95
  (fn [in]
    (letfn [(perfect-sq? [n]
              (let [root (Math/sqrt n)]
                (== (* root root) n)))]
      (->> (map #(parse-int %) (re-seq #"\d+" in))
        (filter perfect-sq?)
        (interpose ",")
        (apply str)))))

(defcheck solution-603ec5c3
  #(->> %
     (re-seq #"\d+")
     (map parse-int)
     (filter (fn [n]
               (let [r (Math/sqrt n)]
                 (= r (* 1.0 (int r))))))
     (map str)
     (interpose ",")
     (apply str)))

(defcheck solution-60a0b0e
  (fn[s] (clojure.string/join "," (filter #{"4" "9" "16" "25" "36"} (re-seq #"\d+" s)))))

(defcheck solution-612a1834
  (fn [s]
    (let [perfect-squares (map #(* % %) (iterate inc 1))
          ints (map parse-int (re-seq #"\d+" s))
          test (into #{} (take (apply max ints) perfect-squares))]
      (clojure.string/join ","
        (filter test ints)))))

(defcheck solution-6154678b
  (fn [s]
    (letfn
     [ (to-ints[s ] (map #(parse-int %) (clojure.string/split s #",")))
      (sqrti  [x ] (int (Math/sqrt x)))
      (sq     [x ] (* x x))
      (keep?  [x ] (= (-> x sqrti sq) x))
      (to-strs[xs] (clojure.string/join "," xs))
      ]
      (to-strs (filter keep? (to-ints s))))))

(defcheck solution-6188e5e
  (fn [s]
    (let [perfect-square? (into #{} (take 10 (map #(int (Math/pow % 2)) (iterate inc 1))))
          nums (map parse-int (clojure.string/split s #","))]
      (apply str (interpose \, (filter perfect-square? nums))))))

(defcheck solution-61c91a6
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(let [r (Math/sqrt %)]
                 (= r (Math/floor r))))
      (interpose ",")
      (apply str))))

(defcheck solution-61def927
  (fn [str]
    (clojure.string/join ","
      (filter
        #(= 0.0 (mod (Math/sqrt (integer %)) 1))
        (clojure.string/split str #",")))))

(defcheck solution-622e8acf
  (fn [s] (clojure.string/join ","
            (filter (set (map #(str (* % %)) (range 9)))
              (re-seq #"\d+" s)))))

(defcheck solution-62581133
  (fn [s]
    (->> (map parse-int (clojure.string/split s #","))
      (filter #(let [x (Math/sqrt %)] (== x (int x))))
      (clojure.string/join ","))))

(defcheck solution-62597443
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(integer %))
      (filter (fn [n] (= n (#(* % %) (int (Math/sqrt n))))))
      (clojure.string/join ","))))

(defcheck solution-6274d48a
  (fn filter-perfect-squares
    [xs]
    (let [number-list (map (comp #(parse-int %) (partial apply str))
                        (filter (complement #{'(\,)})
                          (partition-by #{\,} xs)))]
      (apply str
        (interpose \,
          (filter (into #{}
                    (take-while #(<= % (last number-list))
                      (map #(* % %) (range)))) number-list))))))

(defcheck solution-62da9a50
  (fn perfect-squares [s]
    (apply str
      (interpose \,
        (reduce
          (fn [coll char-number]
            (let [number (parse-int char-number) root (Math/sqrt number)]
              (if (= root (float (int root)))
                (conj coll number)
                coll
                )
              )
            )
          [] (clojure.string/split s #"\,+"))
        )
      )
    ))

(defcheck solution-62ec75f0
  (fn [s] (->> s
            (re-seq #"\d+")
            (map #(parse-int %))
            (filter #(= (float %) (* (Math/sqrt %) (Math/sqrt %))))
            (interleave (repeat ","))
            rest (apply str))))

(defcheck solution-62f4be14
  (fn [s]
    (clojure.string/join
      ","
      (keep
        #((set (re-seq #"\d+" s)) %)
        (map
          #(str (* % %))
          (range 10))))))

(defcheck solution-63fb5b47
  (fn [coll]
    (->> coll
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter (fn [x]
                (let [r (int (Math/sqrt x))]
                  (= x (* r r)))))
      (interpose ",")
      (apply str))))

(defcheck solution-646e7aeb
  (fn [s] (let [x (map #(integer %) (re-seq #"\d+" s))]
            (->> (filter #(= (Math/ceil (Math/sqrt %)) (Math/sqrt %)) x)
              (interpose ",")
              (apply str)))))

(defcheck solution-649623ae
  (fn [s]
    (clojure.string/join ","
      (filter
        #(= 0.0 (mod (Math/sqrt %) 1))
        (map #(integer %)
          (clojure.string/split s #","))))))

(defcheck solution-64974ac0
  (fn [s]
    (clojure.string/join ","
      (filter
        #(let [r (Math/sqrt %1)] (= 0.0 (- r (int r))))
        (map #(parse-int %) (clojure.string/split s #","))
        )
      )
    ))

(defcheck solution-6507fa61
  #(apply str
     (interpose ","
       (filter #{"4" "9" "16" "25" "36"}
         (re-seq #"\d+" %)))))

(defcheck solution-6529112f
  (fn jj [x]
    (apply str
      (interpose ","

        ( filter
          #(=  (int (Math/pow (int (Math/sqrt (parse-int %)))  2   ) )
             (parse-int %))

          (re-seq #"\d+" x) )))))

(defcheck solution-655a30c7
  (fn f [s]
    (letfn [(is-square [x] (let [y (int (Math/sqrt x))]
                             (= x (* y y))))]
      (clojure.string/join "," (filter is-square (map #(parse-int %) (clojure.string/split s #",")))
        ))))

(defcheck solution-6567ffc1
  (fn squares [ns]
    (letfn [(square? [n] (= n (-> n Math/sqrt long (#(* % %)))))]
      (->> (clojure.string/split ns #",")
        (filter (comp square? parse-int))
        (interpose ",")
        (apply str)))))

(defcheck solution-6598c08b
  (fn [c]
    (let [s (map parse-int (re-seq #"[0-9]+" c))
          t (reduce conj #{} (map #(* % % ) (range (last s))))]
      (clojure.string/join "," (filter #(t %) s)))))

(defcheck solution-65c5899a
  (fn [s]
    (let [xs (map parse-int (re-seq #"\d+" s))
          ys (take (apply max xs)  (map #(* % %) (range)))]
      (clojure.string/join ","  (filter #(contains? (set ys) %) xs ))
      )

    ))

(defcheck solution-65e41d21
  (fn [s]
    (let [seqsqr (map #(* % %) (range))]
      (letfn [(integers [x] (map parse-int (clojure.string/split x #",")))
              (makestring [x] (clojure.string/join "," x))
              (perfectsqr
                [n]
                (letfn [(perfectsqrint
                          [x sq]
                          (if (= x (first sq)) true
                                               (if (< x (first sq)) false
                                                                    (perfectsqrint x (rest sq))
                                                                    )))]
                  (perfectsqrint n seqsqr)))
              (filterint
                [l]
                (if (empty? l) []
                               (let [fl (first l) frl (filterint (rest l))]
                                 (if (perfectsqr fl) (concat [fl] frl) frl)
                                 )))]
        (makestring (filterint (integers s)))
        ))))

(defcheck solution-660dddc7
  (fn squares [s]
    (->> s
      (#(clojure.string/split % #","))
      (map parse-int)
      (filter (fn [n] (-> n
                        Math/sqrt
                        Math/floor
                        int
                        (#(* % %))
                        (= n))))
      (clojure.string/join ","))))

(defcheck solution-66404d96
  (fn [s]
    (let [m (map #(parse-int %) (clojure.string/split s #","))
          n (set (map #(* % %) (range 1 (last (sort m)))))]
      (reduce str (interpose "," (filter n m))))))

(defcheck solution-667e10d8
  (fn [astr]
    (apply str (interpose \, (filter #(= (Math/ceil (Math/sqrt %))(Math/sqrt %)) (map #(parse-int %) (clojure.string/split astr #"," )))))
    ))

(defcheck solution-66859821
  (fn [s]
    (letfn [(perfect-square? [n] (some #(= (* % %) n) (range n)))]
      (clojure.string/join ","
        (filter perfect-square? (map #(parse-int %) (clojure.string/split s #",")))))))

(defcheck solution-66c1c47d
  (letfn [(int? [x] (= (double x) (double (int x))))
          (square? [x] (int? (Math/sqrt x))) ]
    (fn [s]
      (->> (clojure.string/split s #",")
        (map parse-int)
        (filter square?)
        (clojure.string/join ",")))))

(defcheck solution-672c613d
  (fn [s] (apply str (interpose ","
                       (filter #(let [x (parse-int %)
                                      sqrt (int (Math/sqrt x))]
                                  (= x (* sqrt sqrt)))
                         (re-seq #"\d+" s))))))

(defcheck solution-6731ad08
  (fn [s]
    (->> s
      (#(clojure.string/split % #","))
      (map parse-int)
      (filter #(== (Math/sqrt %) (int (Math/sqrt %))))
      (clojure.string/join ","))
    ))

(defcheck solution-67d46628
  (fn [s] (#(apply str (concat (mapcat (fn[x] (concat x ",")) (butlast %)) (last %))) (->> s (re-seq #"\d+") (map parse-int) (filter #(== % (-> % Math/sqrt Math/floor (Math/pow 2)))) (map str)))))

(defcheck solution-67f3b137
  (fn [csv]
    (let [ints (map #(parse-int %) (re-seq #"\d+" csv))
          squares (map #(* % %) (iterate inc 1))
          selection (filter (fn [int]
                              (loop [[x & xs] squares]
                                (cond (< x int) (recur xs)
                                      (= x int) true
                                      :else false)))
                      ints)]
      (apply str (interpose \, selection)))))

(defcheck solution-6882d2ce
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter (fn [x]
                (let [r (int (Math/sqrt x))]
                  (= x (* r r)))))
      (interpose ",")
      (apply str))))

(defcheck solution-6914e564
  (fn filter-perfect-squares [s]
    (letfn [(squares? [x]
              (loop [n x]
                (cond (= (* n n) x) true
                      (= n 0) false
                      :else (recur (dec n)))))]

      ((comp

        (partial reduce #(str % "," %2))
        (partial filter squares?)
        (partial map parse-int)

        ) (re-seq #"\d+" s)))))

(defcheck solution-6922815c
  (fn [s] (apply str (interpose ","
                       (remove #(pos? (mod (Math/sqrt %) 1)) (map parse-int (clojure.string/split s #",")))))))

(defcheck solution-69981bc2
  (fn [s]
    (let [ints (map #(integer %) (clojure.string/split s #","))
          perfsq (fn [x] (= 0.0 (mod (Math/sqrt x) 1)))]
      (clojure.string/join "," (filter perfsq ints)))))

(defcheck solution-69bf5bf2
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(= (Math/floor (Math/sqrt %)) (Math/ceil (Math/sqrt %))))
      (clojure.string/join ","))))

(defcheck solution-6a1ff123
  (fn [xs]
    (letfn [(s? [n] (let [r (int (Math/sqrt n))] (= (* r r) n)))]
      (clojure.string/join "," (map str
                                 (filter #(s? %)
                                   (map
                                     #(integer %)
                                     (clojure.string/split xs #",")))))
      )
    ))

(defcheck solution-6a3cb68a
  (fn [s] (->> (clojure.string/split s #",")
            (map #(parse-int %))
            (filter (fn [n] (some #{n} (take n (map #(* % %) (iterate inc 1))))))
            (map str)
            (clojure.string/join ","))))

(defcheck solution-6a671e61
  (fn [s]
    (->> s
      ((fn [s]
         (map #(integer %) (clojure.string/split s #","))))
      ((fn [c]
         (filter (fn [i] (let [rt (int (Math/sqrt i))
                               rtd (dec rt)
                               rti (inc rt)]
                           (or (= i (* rt rt)) (= i (* rti rti)) (= i (* rtd rtd)))))c)))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-6b3e447b
  (fn filter-perfect-squares
    [s]
    (let [square (fn [i] (* i i))
          is-perfect-square? (fn [n]
                               (loop [c 0]
                                 (if (> (square c) n)
                                   false
                                   (if (= (square c) n)
                                     true
                                     (recur (inc c))))))]
      (->> (apply list s)
        (partition-by #(= \, %))
        (remove #(= (list \,) %))
        (map (comp #(parse-int %) (partial apply str)))
        (filter is-perfect-square?)
        (interpose \,)
        (apply str)))))

(defcheck solution-6b4de4e0
  (fn [nums]
    (->>
      (clojure.string/split nums #",")
      (filter (fn [x] (let [x (parse-int x)] (== x (* (Math/sqrt x) (Math/sqrt x))))))
      (clojure.string/join ","))))

(defcheck solution-6c2a94db
  (fn sqrs [s]
    (clojure.string/join ","
      (filter
        #(zero? (mod (Math/sqrt (parse-int %)) 1))
        (.split s ",")))))

(defcheck solution-6c38453b
  #(->> (for [v (map parse-int (.split % ","))
              :let [sqr (Math/sqrt v)
                    rest (rem sqr 1)]
              :when (= 0.0 rest)
              ]
          v
          )
     (interpose "," )
     (reduce str )))

(defcheck solution-6c5e1cb3
  (fn [s]
    (let [squares* (fn [] (map #(* % %) (range)))
          square? (fn [n] (= (last (take-while #(<= % n) (squares*))) n))]
      (->> (clojure.string/split s #",")
        (map #(parse-int %))
        (filter square?)
        (clojure.string/join ",")))))

(defcheck solution-6cfc6ccd
  (fn [s]
    (let
     [n
      (map
        #(parse-int %)
        (clojure.string/split s #","))]
      (apply str
        (interpose
          ","
          (filter
            (set (map #(* % %) (range 1 (apply max n))))
            n))))))

(defcheck solution-6d759228
  (fn string-of-perfect-numbers [string-of-numbers]
    (let [perfect-square?
                          (fn [n]
                            (let [sqrt (Math/sqrt n)]
                              (not (> sqrt (int sqrt)))))
          perfect-squares (filter perfect-square? (map #(integer %) (clojure.string/split string-of-numbers #",")))]
      (clojure.string/join "," (map str perfect-squares)))))

(defcheck solution-6e0723f5
  (fn[cs-str]
    (apply str (interpose \,
                 (filter #(let [sq (int (Math/sqrt %))] (= % (* sq sq)))
                   (map #(parse-int %)
                     (re-seq #"\d+" cs-str)))))))

(defcheck solution-6e398462
  (fn [xs]
    (apply str (interpose ","
                 (filter
                   (fn [n]
                     (let [nb (Math/sqrt (parse-int n))]  (zero? (- nb (int nb)))))
                   (clojure.string/split xs #","))))))

(defcheck solution-6f5695f3
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map parse-int)
      (filter #(let [sq (int (Math/sqrt %))] (== % (* sq sq))))
      (interpose ",")
      (apply str))))

(defcheck solution-6fce44b2
  (fn filter-perfect-squares [s]
    (let [perfect-squares-sets (into #{} (take 100 (map #(* % %) (range))))]
      (clojure.string/join "," (filter #(perfect-squares-sets %) (map parse-int (re-seq #"\d+" s)))))))

(defcheck solution-71adae31
  (fn [x]
    (reduce (fn [a b] (str a "," b))
      (filter
        #(== (int (Math/sqrt %)) (Math/sqrt %))
        (map parse-int (clojure.string/split x #"\,"))))))

(defcheck solution-71c36eb2
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(some true? (for [div (range 2 %)]
                             (and (= (quot % div) div)
                                  (zero? (rem % div))))))
      (clojure.string/join ","))))

(defcheck solution-71e18788
  (fn[str]
    (letfn [(is-square? [n]
              (let [r (int (Math/sqrt n))]
                (= (* r r) n)))]
      (let [svec (map #(integer %) (clojure.string/split str #","))
            squares (filter is-square? svec)]
        (clojure.string/join "," squares)))))

(defcheck solution-7224dc7c
  (fn [s]
    (loop [xs (map #(parse-int %) (.split s ","))
           rs ()]
      (if (empty? xs)
        (clojure.string/join "," (reverse rs))
        (if
         (->>
           (first xs)
           Math/sqrt
           int
           (#(* % %))
           (= (first xs)))
          (recur (rest xs) (conj rs (first xs)))
          (recur (rest xs) rs))))))

(defcheck solution-72773701
  (fn [ns]
    (->> (clojure.string/split ns #",")
      (map parse-int)
      (filter (fn [a] (= a (first (drop-while #(< % a) (map #(* % %) (range)))))))
      (clojure.string/join ",")
      )))

(defcheck solution-72829002
  (fn [s]
    (let [sq #(* % %)]
      (->> (re-seq #"\d+" s)
        (map #(parse-int %))
        (filter #(= % (sq (Math/round (Math/sqrt %)))))
        (interpose ",")
        (apply str)))))

(defcheck solution-72d53a0f
  (letfn [

          (cs [c] (not= c \,))

          (to-nums [s]
            (if (empty? s) '()
                           (let [num (apply str (take-while cs s)) new-s (rest (drop-while cs s)) ]
                             (cons (parse-int num) (to-nums new-s)))))

          (sqr? [n]
            (let [ rt (int (Math/sqrt n)) ]
              (= (* rt rt) n)))

          (to-str [n] (str n \,))
          ]

    (fn [s]
      (let [ lst (to-nums s) ]
        (apply str (butlast (apply str (map to-str (filter sqr? lst)))))))))

(defcheck solution-73026ed0
  (fn [string]
    (clojure.string/join ","
      (filter number?
        (map #(loop [x 2]
                (let [sq (* x x)]
                  (cond
                    (< sq %) (recur (inc x))
                    (= sq %) %
                    (> sq %) false)))
          (map #(integer %) (clojure.string/split string #",")))))))

(defcheck solution-73d35d82
  (fn [s]
    (let [all-squares ((fn internal-squares
                         ([] (internal-squares 1 3))
                         ([s d] (cons s (lazy-seq (internal-squares (+ s d) (+ d 2)))))))
          nearest-square (fn [x] (some #(if (>= % x) %) all-squares))
          is-square? (fn [x] (= x (nearest-square x)))]
      (->> (clojure.string/split s #",")
        (map #(integer %))
        (filter is-square?)
        (interpose \,)
        (apply str)))))

(defcheck solution-73ebf01e
  (fn filter-perfect-squares [s]
    (let [str-v (clojure.string/split s #",")
          num-v (map  #(parse-int %) str-v)
          nums (filter #(some (fn [n] (= % (* n n))) (range (inc (/ % 2)))) num-v)]
      (clojure.string/join "," nums))))

(defcheck solution-742a4c4b
  (fn f [list]
    (->> [list]
      (map #(clojure.string/split % #","))
      flatten
      (map #(integer %))
      (filter #(some #{%} (for [x (range %)] (* x x))))
      (clojure.string/join ","))))

(defcheck solution-74fda8ad
  (fn [in]
    (let [pi #(parse-int %)
          ps? #(= 0 (compare (Math/sqrt %) (int (Math/sqrt %))))]
      (->> (clojure.string/split in #"\,")
        (map pi)
        (filter ps?)
        (clojure.string/join ",")))))

(defcheck solution-7529985a
  (fn [strv]
    (let [checkrt (fn  [v]
                    (let [rt (int (Math/sqrt v ) ) ]
                      (= (* rt rt) v)))
          cs
                  (fn cs [str]
                    (map #(parse-int %) (.split str ","))
                    )
          ]
      (reduce  #(.concat %1 %2) "" (map str (interpose  "," (filter checkrt (cs strv)) )))
      )))

(defcheck solution-757397ce
  (fn [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          all-squares (map #(* % %) (range))
          sq-nums-fn (fn [n] (take-while #(<= % n) all-squares))
          is-sq-fn (fn [n] (some #{n} (sq-nums-fn n)))
          all-sq-nums (keep is-sq-fn nums)]
      (clojure.string/join "," (map str all-sq-nums)))))

(defcheck solution-75a0321b
  (fn f [s]
    (clojure.string/join
      ","
      (filter (fn [a]
                (some
                  #(= (parse-int a) (* % %)) (range (parse-int a))))
        (re-seq #"\d+" s)))))

(defcheck solution-75aa9c79
  (fn [s]
    (let [l (map #(parse-int %) (.split s ","))]
      (apply str (interpose ","
                   (filter (fn [x]
                             (loop [i 1]
                               (if (> (* i i) x) false
                                                 (if (= (* i i) x) true
                                                                   (recur (inc i))))))
                     l))))))

(defcheck solution-75be3c16
  (fn [input]
    (apply str
      (interpose \,
        (filter (fn [x]
                  (= (* (int (Math/sqrt x))  (int (Math/sqrt x))) x))
          (map #(parse-int %) (re-seq #"[0-9]+" input)))))))

(defcheck solution-7670551c
  (fn [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          psquare? (fn [n] (let [sqrt (Math/sqrt n)] (= (Math/floor sqrt) sqrt)))
          perfect (filter psquare? nums)]
      (apply str (interpose "," perfect)))))

(defcheck solution-76d3856
  #(->> %2
     (re-seq #"\d+")
     (filter %)
     (clojure.string/join ",")) #(let [c (parse-int %) s (int (Math/sqrt c))]
                                   (= (* s s) c)))

(defcheck solution-76fb2ddf
  (fn mytest [args]

    (	let [str-nocomma    (clojure.string/split args #",")
            numbers    (map parse-int str-nocomma)

            checkPerfect  (fn [number]

                            (reduce #(or %1 (= number (* %2 %2)) )  false (range 1 number) )
                            )

            perfectNumbers (filter checkPerfect numbers)
            ]

      (clojure.string/join "," perfectNumbers)
      )

    ))

(defcheck solution-770bf710
  (fn ps [s]
    (apply str
      (interpose ","
        (filter (fn [x]
                  (let [x (parse-int x)
                        ]
                    (some #(= x (* % %)) (range (inc (/ x 2))))))
          (re-seq #"[0-9]+" s))))))

(defcheck solution-7778ec05
  (fn [s]
    (apply str
      (interpose ","
        (filter #(let [sqrt (Math/sqrt %)] (== sqrt (int sqrt)))
          (map #(parse-int %)
            (clojure.string/split s #",")))))))

(defcheck solution-781d83c5
  (fn wsqs [ws]
    (let [nums (clojure.string/split ws #",")
          roots (for [num nums] (Math/sqrt (parse-int num)))
          filtered (filter (fn [root] (= root (float (int root)))) roots)]
      (clojure.string/join "," (map (comp #(* % %) int) filtered)))))

(defcheck solution-7831e6d9
  (fn [string]
    (letfn [(square? [n]
              (let [isqrt (Math/round (Math/sqrt n))]
                (= n (* isqrt isqrt))))]
      (clojure.string/join ","
        (filter square?
          (map #(integer %)
            (clojure.string/split string #",")))))))

(defcheck solution-78811ab8
  (fn squares [coll]
    (letfn [(square? [n] (= n (* (int (Math/sqrt n)) (int (Math/sqrt n)))))]
      (clojure.string/join
        "," (map str (filter square?
                       (map #(parse-int %) (clojure.string/split coll #","))))))))

(defcheck solution-78f9c2b2
  (fn [x]
    (->> (map #(parse-int %) (re-seq #"\d+" x))
      (filter #(let [sq (Math/sqrt %)] (zero? (rem sq (int sq)))))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-7912a7
  (fn perf-squares [s]
    (let [roots (map (comp #(Math/sqrt %) parse-int) (re-seq #"\d+" s))
          perfs (filter #(== % (int %)) roots)
          ints (map (comp int #(* % %)) perfs)]
      (clojure.string/join "," (map str ints)))))

(defcheck solution-7969c6cd
  (fn perfect-squares
    [s]
    (let [xs (map #(integer %) (re-seq #"\d+" s))
          square? (fn [x] (let [r (int (Math/sqrt x))] (= x (* r r))))]
      (apply str (interpose ",", (filter square? xs))))))

(defcheck solution-7b1a5246
  (fn [x]
    (->>
      (clojure.string/split x #",")
      (map #(parse-int %))
      (filter #(let [root (Math/sqrt %)] (= (Math/floor root) root)))
      (reduce #(str %1 "," %2) )
      )
    ))

(defcheck solution-7b817876
  (fn [x]
    (let [perfect #(= 0.0 (mod (Math/sqrt %) 1))]
      (->>
        (clojure.string/split x #",")
        (map parse-int)
        (filter perfect)
        (interpose ",")
        (apply str)
        ))))

(defcheck solution-7c20e429
  (fn [s] (apply str
            (interpose ","
              (filter
                #(let [
                       x (parse-int %)
                       v (Math/round (Math/sqrt x))]
                   (= (* v v) x))
                (.split s ","))))))

(defcheck solution-7d3adbb7
  (fn this [s]
    (let [nums (map #(parse-int %)
                 (.split s ","))
          square-nums (filter #(= % (int (Math/pow (int (Math/sqrt %))
                                           2
                                           )
                                      )
                                 ) nums)
          square-strs (map #(str %) square-nums)
          join (fn [col sep]
                 (apply str (reduce #(concat %1 %2) ""
                              (for [i (range (count col))]
                                (if (> i 0)
                                  (concat sep (nth col i))
                                  (nth col i)
                                  )
                                ) ; for
                              ) ; reduce
                   ) ; apply str
                 ) ; fn
          ]
      (join square-strs ","))))

(defcheck solution-7d4217de
  (fn [s]
    (apply str
      (interpose ","
        (filter (fn [n]
                  (let [z (int (Math/sqrt (parse-int n))) y (parse-int n)]
                    (= y (* z z))))
          (clojure.string/split s #",+"))))))

(defcheck solution-7d8bbdfe
  (fn[x]

    (apply
      str
      (rest
        (interleave
          (repeat (count x) ",")
          (sort
            (clojure.set/intersection
              (set
                (map
                  #(parse-int %)
                  (clojure.string/split
                    x
                    #",")
                  )
                )
              (set
                (map  #(* % %)  (range 20))
                )
              )
            )
          )
        )

      )
    ))

(defcheck solution-7e145cbd
  (letfn [(squared [n] (* n n))
          (perfect-square? [n] (= n (squared (int (Math/sqrt n)))))]
    (fn [s]
      (let [perfect-squares (filter #(perfect-square? (integer %)) (.split s ","))]
        (apply str (interpose "," perfect-squares))))))

(defcheck solution-7e1c24ae
  (letfn [(perfect-square? [x]
            (let [sqrt (Math/sqrt x)]
              (== sqrt (Math/floor sqrt))))]
    (comp (partial clojure.string/join ",")
          (partial filter perfect-square?)
          (partial map #(parse-int %))
          (partial re-seq #"\d+"))))

(defcheck solution-7e249078
  (fn [s]
    (let [numbers (map parse-int (clojure.string/split s #","))]
      (clojure.string/join "," (map str (filter (fn [n]
                                                  (loop [k 0]
                                                    (if (= (* k k) n)
                                                      true
                                                      (if (> (* k k) n)
                                                        false
                                                        (recur (inc k)))))) numbers))))))

(defcheck solution-7e2d23e6
  (fn [s]
    (clojure.string/replace
      (clojure.string/replace
        (pr-str
          (keep
            (fn [i]
              (if
               (some
                 #(= i (* % % ))
                 (range 0 i))
                i
                nil))
            (map parse-int (clojure.string/split s #","))))
        #" "  "," )
      #"[()]" "")))

(defcheck solution-7e4248f9
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(zero? (rem (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-7e507cac
  (fn [s]
    (let [all (map parse-int (clojure.string/split s #","))
          big (apply max all)
          squares (for [x all y (range (inc big)) :when (= (* y y) x)] x)]
      (clojure.string/join "," squares))))

(defcheck solution-7ed0c335
  (fn [s]

    (let [res (filter   (fn [n]  (some #(= (* % %) n)      (range 2 (inc (/ n 2) )) )   )   (map parse-int (re-seq #"[\d]+" s) ))]
      (apply str (interpose ","  res))


      )


    ))

(defcheck solution-8060e8d
  (fn [s]
    (let [nums (clojure.string/split s #"\,+")]
      (reduce str
        (interpose ","
          (map #(last %)
            (filter #(first %)
              (partition 2
                (interleave (map #(= % (float (int %)))
                              (map #(Math/sqrt %)
                                (map parse-int
                                  nums)))
                  nums)))))))))

(defcheck solution-818b317f
  (fn fperfsq [s]
    (letfn [(str2seq [x]
              (map #(parse-int %) (clojure.string/split x #"\,")))
            (seq2str [y]
              (apply str (butlast (interleave y (repeat \,)))))]
      (seq2str (filter (into #{} (take-while #(>= (apply max (str2seq s)) %) (map #(* % %) (range)))) (str2seq s)))
      )
    ))

(defcheck solution-81c6a1d5
  (fn [s]
    (let [a (clojure.string/split s #",")
          b (map #(parse-int %) a)
          c (filter #(= 0.0 (mod (Math/sqrt %) 1)) b)]
      (reduce #(str %1 "," %2) c))))

(defcheck solution-82746c8c
  (fn [s]
    (letfn [(to-ints [s] (map #(parse-int %) (re-seq #"\d+" s)))
            (is-sqr? [x] (= x (#(* % %) (int (Math/sqrt x)))))]
      (clojure.string/join "," (filter is-sqr? (to-ints s))))))

(defcheck solution-830860c4
  (fn [s]
    (loop [nums (clojure.string/split s #",") ans '()]
      (if (empty? nums)
        (clojure.string/join "," (reverse ans))
        (if (= 0.0 (mod (Math/sqrt (parse-int (first nums))) 1))
          (recur (rest nums) (conj ans (first nums)))
          (recur (rest nums) ans))))))

(defcheck solution-8315e457
  (fn [s]
    (clojure.string/join ","
      (filter #(let [result (Math/sqrt (parse-int %))]
                 (= result (Math/floor result)))
        (clojure.string/split s #",")))))

(defcheck solution-8330bec4
  (fn filter-squares [string]
    (let [square? (fn [x]
                    (let [root (int (Math/sqrt x))]
                      (= x (* root root))))
          parsed-list (map parse-int (re-seq #"\d+" string))]
      (clojure.string/join "," (filter square? parsed-list)))))

(defcheck solution-835bf10d
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (filter #(let [num (parse-int %), sqrt (int (Math/sqrt num))] (= num (* sqrt sqrt))))
      (interpose ",")
      (apply str))))

(defcheck solution-83628fd8
  (fn perfect-square? [x]
    (letfn [(square? [n]
              (cond
                (< n 0) false
                (= 0 n) true
                :else (loop [i 1, sum 1]
                        (if (= sum n)
                          true
                          (if (> sum n)
                            false
                            (recur (+ 2 i) (+ sum (+ 2 i))))))))]
      (->> #","
        (clojure.string/split x)
        (map #(parse-int %))
        (reduce #(if (square? %2) (conj % %2) %) [])
        (clojure.string/join ",")))))

(defcheck solution-836415fc
  (fn [s]
    (clojure.string/join ","
      (filter (fn [x]
                (let [ y
                      (Math/sqrt (parse-int x))]
                  (== y (int y))))
        (clojure.string/split s, #",")))))

(defcheck solution-838b6886
  (fn [s]
    (letfn [(parse [s]
              (map #(parse-int %) (re-seq #"[0-9]+" s)))
            (perfect_sq [x]
              (let [y (int (Math/sqrt x))]
                (= x (* y y))))]
      (reduce #(str % "," %2)
        (filter #(perfect_sq %) (parse s))))))

(defcheck solution-841f4993
  (fn squares [str]
    (let [nums (map #(integer %) (clojure.string/split str #","))
          sqs (map #(* % %) (range))
          isSqr (fn [num] (some #(= % num) (take-while #(<= % num) sqs)))]
      (clojure.string/join "," (filter isSqr nums)))))

(defcheck solution-84c267d8
  (fn [s]
    (let [xs (map parse-int (clojure.string/split s #","))
          perfect-sq? #(zero? (mod (Math/sqrt %) 1))]
      (apply str (interpose "," (filter perfect-sq? xs))))))

(defcheck solution-85704eee
  (fn [s]
    (let [num-seq (map #(parse-int %) (re-seq #"\d+" s))]
      (apply str (interpose "," (filter (set (take-while #(<= % (apply max num-seq)) (map #(* % %) (range)))) num-seq))))))

(defcheck solution-8624ec18
  (fn [s]
    (apply str
      (interpose ","
        (filter
          #(== (int (Math/sqrt %)) (Math/sqrt %))
          (map #(integer %) (re-seq #"\d+" s)))))
    ))

(defcheck solution-862e78ec
  (fn [s] (clojure.string/join ","
            (filter (fn [c] (let [i (integer c)
                                  rt (int (Math/sqrt i))
                                  sq (* rt rt)]
                              (= i sq))) (clojure.string/split s #",")))))

(defcheck solution-8685c565
  (fn [s] (clojure.string/join "," (filter #(let [r (int (Math/sqrt %1))] (= (* r r) %))
                                     (map #(integer %) (clojure.string/split s #","))))))

(defcheck solution-873a75f5
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(integer %))
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))))
      (map str)
      (interpose ",")
      (apply str))))

(defcheck solution-87f527a4
  #(case (count %) 11 "4,9" "16,25,36"))

(defcheck solution-8899ca2d
  (fn perf-square [s]
    (let [nums (map #(parse-int %)  (clojure.string/split s #","))
          all-perf-sq  (set (map #(* % %) (range 100)))
          sq-nums (filter all-perf-sq nums)]
      (apply str (interpose "," sq-nums)))))

(defcheck solution-88a51d50
  (fn [s] (->> (re-seq #"\d+" s) (map parse-int) (filter (fn [n] (let [x (Math/sqrt n)] (== (* x x) n)))) (clojure.string/join ","))))

(defcheck solution-88b332ae
  (fn [s]
    (let [all (map #(* % %) (range))
          sa (map #(parse-int %) (clojure.string/split s #","))
          f (fn f ([x] (f x all)) ([x a] (cond (= (first a) x) true
                                               (< (first a) x) (recur x (rest a))
                                               :else false)))]
      (clojure.string/join "," (filter f sa)))))

(defcheck solution-890d4961
  (fn [s]
    (clojure.string/join
      ","
      (filter
        #(= (mod (Math/sqrt (parse-int (str %))) 1) 0.0)
        (clojure.string/split s #",")))))

(defcheck solution-89345dcd
  (fn [s]
    (letfn [(square          [x] (* x x))
            (perfect-square? [x] (= x (square (int (Math/sqrt x)))))]
      (let [xs (map parse-int (clojure.string/split s #","))]
        (clojure.string/join "," (map str (filter perfect-square? xs)))))))

(defcheck solution-89a78bc7
  (fn [s]
    (apply str
      (interpose ","
        (filter
          #(let [rt (int (Math/sqrt %))]
             (= % (* rt rt)))
          (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-89d8710c
  (fn [s]
    (let [nums (->> (.split s ",") (map #(parse-int %)))
          perfect-square? #(let [sqt (int (Math/sqrt %))] (= % (* sqt sqt)))]
      (->> nums (filter perfect-square?) (clojure.string/join ",")))))

(defcheck solution-8a5e575a
  (fn [s] (apply str (interpose "," (filter (fn [n]
                                              (loop [v 2]
                                                (if (= n (* v v))
                                                  true
                                                  (if (> (* v v) n)
                                                    false
                                                    (recur (inc v))))))
                                      (map parse-int (re-seq #"\d+" s)))))))

(defcheck solution-8a90da12
  (fn [text]
    (let [perfect (fn [x] (== (* (Math/sqrt x) (Math/sqrt x)) x))
          nums (map #(parse-int %) (clojure.string/split text #","))]
      (apply str (interpose "," (filter perfect nums))))))

(defcheck solution-8aacc0f9
  (fn foo [s]
    (letfn [(perfect? [n]
              (let [r (Math/sqrt n)]
                (= r (Math/floor r))))]
      (->> s
        (#(clojure.string/split % #","))
        (map #(parse-int %))
        (filter perfect?)
        (clojure.string/join ",")
        ))))

(defcheck solution-8afbdfc8
  (fn [s]
    (let [psquares (set (map #(* % %) (range 100)))
          nums (map parse-int (re-seq #"\d+" s))]
      (->> nums
        (filter psquares)
        (interpose ",")
        (apply str)))))

(defcheck solution-8b22c4c8
  (fn [s] (clojure.string/join ","
            (filter
              #(zero? (rem (Math/sqrt (integer %)) 1))
              (.split s ",")))))

(defcheck solution-8b4a2a6
  #(clojure.string/join \,
     (filter (fn [n]
               (let [s (Math/sqrt n)]
                 (= (Math/floor s) (Math/ceil s))))
       (map parse-int (.split % ",")))))

(defcheck solution-8c4785aa
  (fn [s] (apply str (interpose "," (filter #(let [r (Math/sqrt (parse-int %)) z (- r (Math/floor r))]
                                               (zero? z)) (clojure.string/split s #","))))))

(defcheck solution-8c696620
  (fn [s]
    (letfn [(perfect-square? [x]
              (let [sqrt (Math/sqrt (parse-int x))]
                (== sqrt (int sqrt))))]
      (->> (clojure.string/split s #",")
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-8caf898
  (fn [s]
    (let [isqrt (fn [d]
                  (Math/round (Math/sqrt d)))
          square? (fn [d]
                    (let [s (isqrt d)]
                      (= d (* s s))))]
      (clojure.string/join "," (filter square? (map #(parse-int %) (re-seq #"[\d]+" s)))))))

(defcheck solution-8cd4c4d4
  (fn f [s]
    (let [se (map (comp parse-int str) (remove #(= "," %) (map #(apply str %) (partition-by #(= % \,) (last (split-with #(= \, %) (seq s))))))) m (apply max se) sqrs (map #(* % %) (range m))]
      (apply str (interpose \, (sort (filter (set se) (set sqrs))))))))

(defcheck solution-8cfaa4d2
  (fn [s]
    (let [perfect-square? (fn [x]
                            (let [sqrt-x (int (Math/sqrt x))]
                              (= (* sqrt-x sqrt-x) x)))]
      (clojure.string/join
        ","
        (for [v-str (clojure.string/split s #",")
              :let [v (parse-int v-str)]
              :when (perfect-square? v)]
          (str v))))))

(defcheck solution-8d05cb87
  (fn [s]
    (apply str
      (interpose ","
        (filter
          #(let [q (int (Math/sqrt %))] (= % (* q q)))
          (map #(parse-int %) (clojure.string/split s #",")))))))

(defcheck solution-8d5db18e
  (fn [s]
    (let [sqr (fn [x] (* x x))
          is-perfect-square (fn [x] (= x (sqr (int (Math/sqrt x)))))
          to-longs (fn [xs] (map #(parse-int %) xs))]
      (clojure.string/join ","
        (filter is-perfect-square (to-longs (re-seq #"\d+" s)))))))

(defcheck solution-8dcb39cf
  (fn filter-perfect-squares [s]
    (letfn [(perfect-square [n]
              (zero? (mod n (Math/sqrt n))))]
      (clojure.string/join ","
        (filter
          perfect-square
          (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-8e2b17de
  (fn perfect-squares [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(zero?
                 (compare (Math/sqrt %)
                   (int (Math/sqrt %)))))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-8e5411c
  (fn filter-squares
    [s] {:pre [(string? s)]}
    (let [;; We need a predicate to see if a number is a perfect square. The
          ;; simplest approach that dodges any potential floating-point concerns
          ;; is to take the square root, round that off to the nearest integer,
          ;; square that, and see if we ended up with what we started with.
          square? (fn [n]
                    (let [root (->> n Math/sqrt Math/round)]
                      (= n (* root root))))]

      (->> (clojure.string/split s #",") ; ["1" "7" "16" "37"]
        (map #(parse-int %))   ; [1 7 16 37]
        (filter square?)              ; [1 16]
        (clojure.string/join \,)))))

(defcheck solution-8ea00d25
  (fn [ls]
    (apply str
      (interpose ","
        (map str
          (filter
            (fn [x]
              (let [s (int (Math/sqrt x))]
                (= (* s s) x)))
            (map parse-int
              (clojure.string/split ls #","))))))))

(defcheck solution-8ea35df9
  (fn [y]
    (let
     [perf (fn [x]
             (= x (*
                    (int (#(Math/sqrt %) x))
                    (int ( #(Math/sqrt %) x))) ))]
      (clojure.string/join "," (filter perf (map #(parse-int %) (clojure.string/split y #","))))
      )))

(defcheck solution-8ef250a4
  (fn [s]
    (apply str (interpose ","
                 (filter
                   #(let [d (Math/sqrt %)] (== d (int d)))
                   (map #(parse-int %) (re-seq #"[\d]+" s)))))))

(defcheck solution-8fe6d72b
  (fn p74
    [xs]
    (->>
      (.split xs ",")
      (map parse-int)
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-8fff6a11
  (fn [s]
    (let [square (fn [n] (* n n))
          perfect? (fn [n]
                     (= n (square (int (Math/sqrt n)))))]
      (->> (.split s ",")
        (map parse-int)
        (filter perfect?)
        (clojure.string/join ",")))))

(defcheck solution-901318aa
  (fn     [s] (apply str (interpose ","
                           (filter #(let [n (int (Math/sqrt %))]
                                      (= % (* n n)))
                             (map parse-int
                               (re-seq #"\d+" s)))))))

(defcheck solution-906070a8
  (fn [n]
    (let [s (map parse-int (clojure.string/split n #","))]
      (clojure.string/join ","   (filter #(zero? (mod (Math/sqrt %) 1)) s))

      )))

(defcheck solution-907fba1e
  (fn [s]
    (let [is-square (fn [i]
                      (some
                        #(= i %)
                        (map
                          #(* % %)
                          (range (dec i) 1 -1))))]
      (->> (clojure.string/split s #",")
        (map #(parse-int %))
        (filter is-square)
        (clojure.string/join ",")))))

(defcheck solution-91175f76
  (fn [s]
    (->> (re-seq #"\d+" s)
      (filter #(-> % parse-int Math/sqrt (rem 1) zero?))
      (clojure.string/join ","))))

(defcheck solution-911d8d79
  (fn [s]
    (->> (.split s ",")
      (map parse-int)
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (interpose ",")
      (apply str))))

(defcheck solution-9167c738
  (fn ps [numStr]
    (let [nums (map #(parse-int %) (re-seq #"\d+" numStr))
          perfect (filter #(== (mod (Math/sqrt %) 1) 0) nums)]
      (apply str (interpose "," perfect)))))

(defcheck solution-9179f71f
  (fn [sq]
    ;; join results
    (clojure.string/join ","
      ;; grab first of tuple
      (map first
        ;; Return elements for which some x + 1 until n is a perfect square
        (filter (fn [tl] (some #(= (* % %) (first tl)) (last tl)))
          (map
            #(let [n (parse-int %)]
               ;; return a tuple of n and range n, after splitting into nums. e.g. [4 (1 2 3 4)]
               (vector n (range 1 n))) (clojure.string/split sq #"," )))))))

(defcheck solution-91eb2222
  (fn [s]
    (clojure.string/join ","
      (filter
        (fn [n] (let [q (int (Math/sqrt n))] (= (* q q) n)))
        (map parse-int (clojure.string/split s #","))))))

(defcheck solution-922d2e17
  (fn [s] (let
           [split   (clojure.string/split s #",")
            xs      (map #(parse-int %) split)
            squares (map #(* % %) (range (apply max xs)))
            ys      (filter (fn [y] (some (partial = y) squares)) xs)]
            (apply str (interpose "," (map str ys))))))

(defcheck solution-923ccbcb
  (fn [x]
    (letfn [(sq? [n] (true? (some #(= (* %1 %1) n)
                              (range 1 (inc (/ n 2))))))]
      (->> (clojure.string/split x #",")
        (map #(parse-int %1))
        (filter sq?)
        (clojure.string/join ",")))))

(defcheck solution-924f18f8
  (fn filter-perfect-squares [s]
    (reduce #(str %1 "," %2)
      (remove #(not ((  fn perfect-square? [x]
                       (if (nil? (first (for [i (range (int (inc (/ x 2)))) :when (= (* i i) x)]
                                          true)
                                   ))
                         false
                         true))                %1))
        (map #(parse-int (apply str %1 ))
          (remove #(= %1 '(\,)) (partition-by #(= \, %1) (seq s)) ))
        )
      )
    ))

(defcheck solution-9277499b
  (fn [string]
    (let [square? (fn [x]
                    (let [sqrt (Math/sqrt x)]
                      (= sqrt (Math/floor sqrt))))]
      (->> string
        (re-seq #"\d+")
        (map #(parse-int %))
        (filter square?)
        (reduce #(str %1 "," %2))))))

(defcheck solution-92891abb
  (fn[s]
    (clojure.string/join ","
      (filter (set (for [x (range 2 1000)] (str (* x x))))
        (clojure.string/split s #"\,")))))

(defcheck solution-92da3244
  (fn perfectsq [numlist]
    (let [nums (map parse-int (clojure.string/split numlist #",") )
          is-perfect-sq (fn [n]
                          (first (for [i (range 2 (inc (/ n 2))) :when (= (/ n i) i)] n)))]
      (clojure.string/join "," (filter is-perfect-sq nums))
      )
    ))

(defcheck solution-9353c0c1
  (fn [s]
    (let [nums
          (map (fn [s] (parse-int s))
            (clojure.string/split s #","))

          perfect-square
          (fn [n]
            (loop [x 1]
              (if (>= x n) nil
                           (if (and (= (/ n x) x) (zero? (mod n x)))
                             n
                             (recur (inc x))))))]
      (clojure.string/join ","
        (map str (filter perfect-square nums))))))

(defcheck solution-93679955
  (fn
    [s]
    (->>
      (clojure.string/split s #",")
      (map parse-int)
      (filter (fn [n] (let [m (int (Math/sqrt n))]
                        (= n (* m m)))))
      (clojure.string/join ","))))

(defcheck solution-93a010dc
  (fn [s] (apply str (interpose "," (filter (fn [v] (some #(= v (* % %)) (range v))) (map parse-int (clojure.string/split s #"\,")))))))

(defcheck solution-93d19ac3
  (fn foo [s]
    (let [nums (clojure.string/split s #",")]
      (clojure.string/join "," (filter #(== (int (Math/sqrt (parse-int %))) (Math/sqrt (parse-int %))) nums)))))

(defcheck solution-9438753f
  (fn [coll]
    (clojure.string/join "," (filter
                               (fn [x] (zero? (mod (Math/sqrt x) 1)))
                               (map parse-int (clojure.string/split coll #"\W"))
                               ))
    ))

(defcheck solution-949dda88
  (fn [s] (clojure.string/join "," (filter #(#{4 9 16 25 36} (parse-int %)) (clojure.string/split s #",")))))

(defcheck solution-96665ffe
  (fn [s]
    (apply str
      (interpose ","
        (filter #(= (double %) (Math/pow (int (Math/sqrt %)) 2))
          (map #(parse-int %)
            (re-seq #"\d+" s)))))))

(defcheck solution-96b772c3
  (fn myf2 [s]
    (let [square? (fn [n]
                    (->> (range 1 (inc n))
                      (map #(int (Math/pow % 2)))
                      (some #(= % n))))]
      (->> (.split s ",")
        (map #(parse-int %))
        (filter square?)
        (interpose ",")
        (apply str)
        ))))

(defcheck solution-97dd2b45
  (fn [arg]
    (clojure.string/join ","
      (filter #(= (Math/ceil (Math/sqrt (integer %)))
                 (Math/floor (Math/sqrt (integer %))))
        (clojure.string/split arg #",")))))

(defcheck solution-9820090f
  (fn [x]
    (apply str (interpose \,
                 (filter (set (map #(str (* % %))
                                (range 9)))
                   (.split x ","))))))

(defcheck solution-9877aba6
  (fn [s]
    (letfn [(i[s] (map #(parse-int %) (re-seq #"\d+" s)))]
      (apply str (interpose \,
                   (keep #(let [sq (Math/sqrt %)]
                            (when (== (* sq sq) %) %))
                     (i s)))))))

(defcheck solution-9893bb72
  (fn [n] (clojure.string/join "," (map str (filter (fn [e]
                                                      (< (- (Math/sqrt e) (int (Math/sqrt e))) 0.001))
                                              (map #(parse-int %)
                                                (clojure.string/split n #",")))))))

(defcheck solution-99286920
  (fn pfsq[st]
    (clojure.string/join
      ","
      (map
        str
        (filter
          #(let [i (int (Math/sqrt %))]
             (= (* i i) %))
          (map
            #(parse-int %)
            (clojure.string/split st #",")))))))

(defcheck solution-99370b28
  (fn only-squares [s]
    (let [introot (fn [n] (int (Math/floor (Math/sqrt n))))
          rs (fn [n] (* (introot n) (introot n)))
          is-square? (fn [n] (= (rs n) n))]
      (apply str (interpose "," (filter is-square? (map #(integer (str %)) (.split s ","))))))))

(defcheck solution-993ede55
  (fn [s]
    (let [
          ps (fn [i]
               (let [sr (Math/round (Math/sqrt i))]
                 (= i (* sr sr))))
          is (map parse-int (clojure.string/split s #","))
          ]
      (->>
        is
        (filter ps)
        (map str)
        (clojure.string/join ",")
        )
      )
    ))

(defcheck solution-995676e4
  #(apply str
     (interpose ","
       (filter (fn [s]
                 (let [x     (integer s)
                       is-sq (fn [x]
                               (not-empty
                                 (filter (fn [y] (= (* y y) x))
                                   (range 1 (inc (int (/ x 2)))))))]
                   (is-sq x)))
         (re-seq #"\d+" %)))))

(defcheck solution-99a8b6c4
  (fn [x]
    (let [nums (map parse-int (seq (.split x ",")))]
      (apply
        str
        (butlast
          (interleave
            (map
              str
              (filter
                (fn [a]
                  (= a (#(* %1 %1) (int (Math/sqrt a))))
                  )
                nums
                )
              )
            (repeat \,)
            )
          )
        )
      )
    ))

(defcheck solution-99b4eb7b
  (fn [s]
    (let [square? (fn [n]
                    (some #(= n (* % %)) (range 1 n)))]
      (->> (re-seq #"\d+" s)
        (map #(parse-int %))
        (filter square?)
        (filter str)
        (clojure.string/join ",")))))

(defcheck solution-99f79892
  (fn [s]
    (let [square? #(let [n (int (Math/sqrt %))]
                     (= (* n n) %))]
      (clojure.string/join "," (filter (fn [n-string] (square? (parse-int n-string)))
                                 (clojure.string/split s #"\,"))))))

(defcheck solution-9a0026a4
  (fn [s]
    (letfn [(ps? [n]
              (let [x (Math/sqrt n)
                    xi (Math/ceil x)]
                (= x xi)))]

      (apply str (interpose "," (filter ps? (map #(parse-int %) (re-seq #"\d+" s))))))))

(defcheck solution-9a255f75
  (fn [xs]
    (clojure.string/join "," (map str (filter
                                        #(= 0.0 (mod (Math/sqrt (parse-int %)) 1))
                                        (clojure.string/split xs #"\,"))))))

(defcheck solution-9aa06224
  (fn [x]
    (apply str
      (rest (interleave
              (repeat ",")
              (filter #(== (Math/pow (int (Math/sqrt %)) 2) %) (map #(integer %) (.split x ","))))))))

(defcheck solution-9adbcb1d
  (fn [s]
    (clojure.string/join
      ","
      (filter
        (fn [v] (-> v parse-int Math/sqrt (rem 1) zero?))
        (re-seq #"\d+" s)))))

(defcheck solution-9bb4a636
  (fn [ss] (let [aa (filter #(zero? (mod (Math/sqrt (parse-int %)) 1)) (clojure.string/split ss #","))]
             (apply str (interpose "," aa))
             )))

(defcheck solution-9c132849
  #(clojure.string/join ","
     (filter #{"1", "4", "9", "16", "25", "36"} ;; so lazy
       (clojure.string/split % #","))))

(defcheck solution-9c3ea06a
  (fn [s]
    (let [nums (map parse-int (re-seq #"\d+" s))
          has-perfect-square (fn [n] (empty? (filter #(= n (* % %)) (range 1 (inc n)))))]
      (apply str (interpose \, (filter (complement has-perfect-square) nums))))))

(defcheck solution-9d13ff3d
  (fn ps [strcoll]
    (apply str (interpose ","
                 (filter #(= (double %) (* (Math/sqrt %)
                                          (Math/sqrt %)))
                   (map #(parse-int %)
                     (clojure.string/split strcoll #",")))))))

(defcheck solution-9d57fecc
  (fn [s]
    (clojure.string/join
      ","
      (filter #(let [n (parse-int %) r (int (Math/sqrt n))]
                 (= (* r r) n))
        (clojure.string/split s #",")))))

(defcheck solution-9dbbfa47
  (fn [x]
    (clojure.string/join ","
      (filter
        #(seq
           (for [i (range %) :when (= % (* i i))] i))
        (map parse-int (re-seq #"\d+" x))))))

(defcheck solution-9dbc50ef
  (fn [s] (clojure.string/join "," (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))) (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-9dd062f7
  (fn [s]
    (reduce
      #(str % "," %2)
      (filter
        #(= 0.0 (mod (Math/pow (integer %) 0.5) 1))
        (re-seq #"\d+" s)))))

(defcheck solution-9df7d19c
  (fn[s] (apply str (interpose "," (filter #(let [r (int (Math/sqrt %))] (= (* r r) %)) (map parse-int (re-seq #"\d+" s)))))))

(defcheck solution-9dffcc98
  (fn [s]
    (letfn [(is_square? [x] (= (#(* % %) (int (Math/sqrt x))) x))]

      (->> s (re-seq #"\d+")
        (map #(parse-int %))
        (filter is_square?)
        (clojure.string/join ",")))))

(defcheck solution-9e93340c
  (fn [s]
    (apply str (interpose \, (filter #(== (Math/sqrt %) (int (Math/sqrt %)))
                               (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-9ec18ddc
  (fn [s]
    (let [nums (map #(parse-int %) (.split s ","))]
      (clojure.pprint/cl-format
        nil
        "~{~d~^,~}"
        (for [num nums
              :let [sqrt (Math/sqrt num)]
              :when (= (double (int sqrt)) sqrt)]
          num)))))

(defcheck solution-9ed0d1f7
  (fn [s] ((fn [x] (apply str (interpose "," x))) (filter (fn [v] (= v (#(* % %) (long (Math/sqrt v))))) (map parse-int (#(.split % ",") s))))))

(defcheck solution-9fc02d9a
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter (fn [x]
                (let [r (int (Math/sqrt x))]
                  (= x (* r r)))))
      (interpose ",")
      (apply str))))

(defcheck solution-9fcaadbe
  (fn [s]
    (clojure.string/join "," (filter
                               #(let [rootAsInt (-> % Math/sqrt Math/floor)]
                                  (= (* rootAsInt rootAsInt) (float %)))
                               (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-9ff4318f
  (fn [s]
    (clojure.string/join "," (filter #(= (float (int (Math/sqrt %))) (Math/sqrt %))
                               (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-a0829d83
  (fn [s]
    (->>
      (re-seq #"\d+" s)
      (filter #(let [n (integer %) q (int (Math/sqrt n))] (= n (* q q))))
      (clojure.string/join ","))))

(defcheck solution-a0bbec7c
  (fn ps [s]
    (clojure.string/join
      ","
      (filter
        #(== % (Math/pow (Math/sqrt %) 2))
        (map parse-int (clojure.string/split s #","))))))

(defcheck solution-a0efd913
  (fn [num-str]
    (let [squares (map #(* % %) (drop 2 (range)))
          perf-square? (fn [x]
                         (= x (first (drop-while #(< % x) squares))))
          nums (map #(parse-int %)
                 (clojure.string/split num-str #","))
          only-squares (filter perf-square? nums)]
      (clojure.string/join "," only-squares))))

(defcheck solution-a10748ca
  (fn [str]
    (letfn [(find-sq [n] (some true? (for [r (range 0 (inc (/ n 2)))] (= (* r r) n))))]
      (clojure.string/join "," (filter find-sq (map #(integer %) (re-seq #"\d+" str))))
      )
    ))

(defcheck solution-a1488354
  (fn [v]
    (clojure.string/join
      ","
      (filter
        #(zero? (rem (Math/sqrt %) 1))
        (map #(parse-int %) (re-seq #"\w+" v))))))

(defcheck solution-a1549a8b
  (fn [s]
    (clojure.string/join
      ","
      (map str
        (filter (partial contains? (set (take 10 (map #(* % %) (range)))))
          (map parse-int
            (clojure.string/split s #",")))))))

(defcheck solution-a17332b4
  (fn[s]
    (let [
          sqr (fn [n] (int (Math/sqrt n)))
          sq? (fn [n] (= n (* (sqr n) (sqr n)))  )
          ]
      (apply str
        (interpose ","
          (filter #(sq? %)
            (map #(parse-int %)
              (.split s ","))))))))

(defcheck solution-a1b79a02
  (fn [s]
    (clojure.string/join ","
      (filter
        #(let [x (parse-double %)
               r (Math/sqrt x)]
           (= x (* r r)))
        (clojure.string/split s #",")))))

(defcheck solution-a1c6e450
  (fn [s]
    (clojure.string/join ","
      (filter #(= 0.0 (rem (Math/sqrt %) 1)) (map parse-int (clojure.string/split s #","))))))

(defcheck solution-a23f9e00
  (fn [s] (clojure.string/join "," (filter #(= 0.0 (mod (Math/sqrt %) 1)) (map #(parse-int %)
                                                                            (clojure.string/split s #","))))))

(defcheck solution-a2a1149f
  (let [from-comma-nums (fn from-comma-nums
                          [ss]
                          (map #(integer %) (clojure.string/split ss #",")))
        to-comma-nums (fn to-comma-nums
                        [ss]
                        (clojure.string/join "," ss))
        squares (fn squares
                  []
                  (let [nums-from (fn nums-from[n] (lazy-seq (cons n (nums-from (+ 1 n)))))]
                    (map (fn [x] (* x x)) (nums-from 1))))]
    (let [is-square? (fn is-square?
                       [x]
                       (loop [[h & t] (squares)]
                         (if (= h x)
                           true
                           (if (> h x)
                             false
                             (recur t)))))]
      (fn filter-squares
        [ss]
        (to-comma-nums(filter is-square? (from-comma-nums ss)))))))

(defcheck solution-a35ab27b
  #(clojure.string/join "," (filter (fn [x] (zero? (mod (Math/sqrt (integer x)) 1))) (clojure.string/split % #","))))

(defcheck solution-a3745673
  (fn [s] (clojure.string/join "," (filter (fn [n] (= (Math/sqrt n) (* 1.0 (int (Math/sqrt n)))))
                                     (map (fn [d] (parse-int d)) (clojure.string/split s #","))))))

(defcheck solution-a3fa5e2
  (fn [x] (clojure.string/join "," (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %))) (map #(parse-int %) (.split x ","))))))

(defcheck solution-a419bba
  (fn
    [coll]
    (let [is (map parse-int (clojure.string/split coll #","))]
      (clojure.string/join
        ","
        (sort (clojure.set/intersection
                (set is)
                (->> is (apply max) Math/sqrt inc range (map #(* % %)) set)))))))

(defcheck solution-a42d3880
  (fn [s]
    (clojure.string/join ","
      (filter #(some #{(integer %)} (map (fn [i] (* i i)) (range (integer %))))
        (clojure.string/split s #",")))))

(defcheck solution-a4a2141f
  (fn [s] (->> s
            (#(clojure.string/split % #","))
            (map #(integer %))
            (filter #(-> %
                       Math/sqrt
                       int
                       (Math/pow 2)
                       (= (float %))))
            (map #(.toString %))
            (clojure.string/join ","))))

(defcheck solution-a556a8ac
  #(apply str (interpose "," (for [str (re-seq #"\d+" %)
                                   :let [d (parse-int str)
                                         s (long (Math/sqrt d))]
                                   :when (= (* s s) d)] d))))

(defcheck solution-a5611c75
  (fn [n]
    (clojure.string/join
      ","
      (filter
        #(let [rnd (Math/round (Math/sqrt %))]
           (= (* rnd rnd) %))
        (map #(parse-int %) (clojure.string/split n #","))))))

(defcheck solution-a5965dd1
  (fn fisq [s]
    (letfn [(intsq [n]
              (loop [x n]
                (let [y (quot (+ (quot n x) x) 2)]
                  (if (< y x)
                    (recur y)
                    (if (= n (* x x)) n nil)))))]
      (->>
        (clojure.string/split s #"\,")
        (map parse-int)
        (filter intsq)
        (interpose ",")
        (apply str)
        ))))

(defcheck solution-a5a88722
  (fn [csv]
    (->> (clojure.string/split csv #",")
      (map parse-int)
      (filter
        #(let [sqrt (Math/sqrt %)]
           (= sqrt (Math/floor sqrt))))
      (clojure.string/join ","))))

(defcheck solution-a5e887dc
  (fn filter-squares [s]
    (letfn [(is-square? [n]
              (loop [x (quot n 2) seen #{x}]
                (if (= n (* x x)) true
                                  (let [xx (quot (+ x (quot n x)) 2)]
                                    (if (contains? seen xx) false
                                                            (recur xx (conj seen xx))
                                                            )))
                ))]
      (apply str (interpose "," (filter is-square? (map parse-int (clojure.string/split s #",")))))
      )))

(defcheck solution-a601565a
  (fn [string]
    (->>
      (clojure.string/split string #",")
      (map #(parse-int %))
      (filter (fn [n] (let [r (int (Math/sqrt n))]
                        (= n (* r r)))))
      (clojure.string/join ","))))

(defcheck solution-a68a9c4e
  (fn [s] (->> (clojure.string/split s #",")
            (map parse-int)
            (filter (fn[x] (= x (#(* % %) (Math/round (Math/sqrt x))))))
            (interpose \,)
            (apply str)
            )))

(defcheck solution-a72b9605
  (fn [s] (->> (clojure.string/split s #"\,")
            (map #(integer %))
            (filter #(let [x (Math/sqrt %)] (= (Math/floor x) (Math/ceil x))))
            (clojure.string/join ","))))

(defcheck solution-a75be226
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter #(let [root (Math/sqrt %)
                     iroot (int (+ root 0.01))]
                 (= (* iroot iroot) %)))
      (map str)
      (interpose \,)
      (apply str))))

(defcheck solution-a7d57eb8
  (fn [str]
    (let [no-fpart? #(= (Math/ceil %)
                       (Math/floor %))
          perfect-square?
                    #(no-fpart? (Math/sqrt %))]
      (clojure.string/join ","
        (filter
          #(perfect-square? (integer %))
          (clojure.string/split str #",")))
      )))

(defcheck solution-a8761d34
  (fn [s]
    (clojure.string/join ","
      (filter (fn [x]
                (let [x (parse-int x)
                      y (Math/sqrt x)]
                  (== x (* y y))))
        (clojure.string/split s #",")))))

(defcheck solution-a8bad1ca
  (fn [s]
    (->>  s
      (re-seq #"\d+")
      (map parse-int )
      (filter #(-> (Math/sqrt %)
                 (mod 1)
                 (== 0)))
      (interpose ",")
      (apply str))))

(defcheck solution-a94bd72d
  (fn [s] (clojure.string/join "," (filter
                                     #(== 0 (mod (Math/sqrt (parse-int %)) 1))
                                     (.split s ",")))))

(defcheck solution-a9cf0321
  (fn [st]
    (let [f (fn [n] (let [a (int (Math/sqrt n))] (= (* a a) n)))
          nums (map parse-int (re-seq #"[0-9]+" st))]
      (reduce str (interpose "," (filter f nums))))))

(defcheck solution-a9f8e611
  (fn
    [a]
    (let [is-square (fn [possible-square]
                      (loop [curr 1]
                        (cond
                          (= (* curr curr) possible-square) true
                          (= curr possible-square) false
                          :else (recur (inc curr)))))]
      (->>
        (clojure.string/split a #",")
        (map parse-int)
        (filter is-square)
        (reduce #(str % "," %2))
        ))))

(defcheck solution-aaa20085
  #(clojure.string/join "," (filter (fn [x] (= ((fn [x] (* x x)) (int (Math/sqrt x))) x))
                              (map parse-int (clojure.string/split % #",")))))

(defcheck solution-ab2f940c
  (fn
    [s]
    (clojure.string/join
      ","
      (filter
        (fn [x] (let [r (Math/sqrt x)] (= r (Math/floor r))))
        (map
          #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-abf65169
  (let [parse-int #(parse-int %)
        square #(* % %)
        sqrt #(Math/sqrt %)
        is-square? #(= % (->> % sqrt int square))
        join #(apply str (butlast (interleave %2 (repeat %1))))]
    #(->> %
       (re-seq #"\d+")
       (map parse-int)
       (filter is-square?)
       (join ","))))

(defcheck solution-ac65c87a
  (fn [xs]
    (letfn [(perfect-square? [n] (let [sqrt (Math/round (Math/sqrt n))] (= n (* sqrt sqrt))))
            (to-ints [xs] (map #(parse-int %) (re-seq #"\d+" xs)))
            (perfect-squares [xs] (filter perfect-square? (to-ints xs)))]
      (apply str (interpose "," (map str (perfect-squares xs)))))))

(defcheck solution-acb8390a
  (fn filter-squares [s]
    (clojure.string/join ","
      (filter #(let [sqrt (Math/sqrt (parse-int %))]
                 (= (Math/floor sqrt) sqrt)) (re-seq #"\d+" s)))))

(defcheck solution-ad1e79f3
  (fn [s]
    (let [double #(* % %)]
      (reduce str
        (interpose ","
          (map #(-> % first str)
            (filter #(= (first %) (second %))
              (map #(-> % parse-int Math/sqrt int double (vector (parse-int %)))
                (re-seq #"[0-9]+" s)))))))))

(defcheck solution-ad2df95f
  (fn filter-perfect-squares [coll]
    (let [arg1 (clojure.string/split coll #",")
          arg2 (map #(parse-int %) arg1)
          arg3 (filter #(= (float %) (Math/pow (Math/sqrt %) 2)) arg2)]
      (clojure.string/join \, arg3))))

(defcheck solution-ad3e444c
  (fn [num-strs]
    (let [squares (map #(* % %) (range))
          nums (map #(parse-int %) (re-seq #"\d+" num-strs))]
      (apply str
        (interpose ","
          (filter
            #(some (fn [x] (= x %))
               (take-while (fn [x] (<= x %)) squares))
            nums))))))

(defcheck solution-ad418f22
  (fn filterpf [l]
    (letfn [(ps [s]
              (let [r (Math/sqrt s)
                    r (int r)]
                (= (* r r) s)))]
      (apply
        str
        (drop-last 1
          (interleave
            (filter ps (map #(parse-int %) (.split l ",")))
            (repeat \,)))))))

(defcheck solution-ad539128
  (fn [s]
    (let [n (map parse-int (clojure.string/split s #","))
          m (apply max n)
          r (set (map #(* % %) (range m)))]
      (clojure.string/join "," (map str (filter #(not (nil? (r %))) n))))))

(defcheck solution-ad59a851
  (fn [xs]
    (let [p (fn [x]
              (let [x (parse-int x)
                    a (int (Math/sqrt x))
                    a (* a a)]
                (= a x)))]
      (clojure.string/join "," (filter p (clojure.string/split xs #","))))))

(defcheck solution-add83acc
  (fn [s]
    (let [pSquares (take 10 (map #(str (* % %)) (iterate inc 1)))
          in (clojure.string/split s #",")]
      (clojure.string/join "," (filter (fn [i] (some #(= % i) pSquares)) in)))))

(defcheck solution-ade08e62
  (fn js [in-str]
    (let [input (map parse-int (clojure.string/split in-str #","))
          squares (map #(* % %) (range 10))
          output (filter #(some #{%} squares) input)
          out-str1 (clojure.string/replace (str (apply list output)) " " "," )
          out-str2 (clojure.string/replace out-str1 #"[\(\)]" "")]
      out-str2)))

(defcheck solution-ae20f50f
  (fn [s]
    (letfn [(to-numbers [s] (map #(parse-int %) (.split s ",")))
            (to-string [ns] (clojure.string/join \, ns))
            (square [n] (* n n))
            (square? [n] (= n (square (int (Math/sqrt n)))))]
      (->> s
        to-numbers
        (filter square?)
        to-string))))

(defcheck solution-aeb0d241
  (fn [s]
    (let [perfect-square? (fn [n]
                            (let [sqrt (int (Math/sqrt n))]
                              (= n (* sqrt sqrt))))
          ns (for [n-str (clojure.string/split s #",")
                   :let [n (parse-int n-str)]
                   :when (perfect-square? n)]
               n)]
      (clojure.string/join "," ns))))

(defcheck solution-aeb911ad
  (fn onlySquares [numberString]
    (let
     [isqrt (fn [x] (int (Math/sqrt x)))
      isSquare (fn [x] (= (* (isqrt x) (isqrt x)) x))
      str2vec (fn [str] (map #(parse-int %) (clojure.string/split str #",")))
      ]
      (clojure.string/join "," (filter isSquare (str2vec numberString)))
      )))

(defcheck solution-af1d3ed4
  #(apply str
     (interpose ","
       (filter
         (fn[s]
           (let [n (parse-int s) sq (int (Math/sqrt n))]
             (= sq (/ n sq))))
         (clojure.string/split % #",")
         ) )))

(defcheck solution-af445733
  (fn filter-perfect-squares [l]
    (clojure.string/join "," (filter
                               (fn [x] (== (* (int (Math/sqrt x)) (int (Math/sqrt x))) x))
                               (map (fn [x] (parse-int x)) (clojure.string/split l #","))))))

(defcheck solution-af4fbaae
  (fn [s]
    (clojure.string/join
      ","
      (keep
        (set (map #(str (* % %)) (range 7)))
        (clojure.string/split s #",")))))

(defcheck solution-b01bcdf7
  (fn [s] (clojure.string/join ","
            (filter (fn [x]
                      (#(== (int %) %) (Math/sqrt (parse-int x))))
              (clojure.string/split s #",")))))

(defcheck solution-b0246f48
  (fn [s]
    (apply str
      (interpose ","
        (filter (fn [k]
                  (= k (last
                         (take-while #(>= (- k %) 0)
                           (map #(* % %)
                             (iterate inc 1))))))
          (map #(parse-int %)
            (re-seq #"-*\d+" s)))))))

(defcheck solution-b055e78d
  (fn [s]
    (apply str (interpose "," (map str (filter (fn [n] (first (filter #(zero? (- (* % %) n)) (range 2 n)))) (map #(parse-int %) (re-seq #"\d+" s))))))))

(defcheck solution-b088945b
  (fn perfSquares[x] (clojure.string/join "," (filter
                                                (fn perfSquare[x] (= (int (Math/pow (int (Math/sqrt x)) 2)) x))
                                                (map parse-int (clojure.string/split x #","))))))

(defcheck solution-b1171f17
  (fn prob74 [comma-string]
    (let [string (clojure.string/split comma-string #",")
          nums (map parse-int string)
          ]
      (apply str (interpose "," (filter #(let [x (Math/sqrt %)] (= (float (int x)) x))  nums))))))

(defcheck solution-b184d7df
  (fn [s]
    (let [nums (map
                 #(parse-int %)
                 (clojure.string/split s #","))
          max-num (apply max nums)
          perfect-squares-up-to-max
          (set (map #(* % %) (range 1 max-num)))]
      (clojure.string/join
        ","
        (filter perfect-squares-up-to-max nums)))))

(defcheck solution-b1afa58
  (fn [s]
    (letfn [(i[s] (map #(parse-int %) (re-seq #"\d+" s)))]
      (apply str (interpose \,
                   (keep #(let [sq (Math/sqrt %)]
                            (when (== (* sq sq) %) %))
                     (i s)))))))

(defcheck solution-b216cf01
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter (fn [x]
                (->> (range x)
                  (drop 1)
                  (filter #(= (* % %) x))
                  seq)))
      (clojure.string/join ","))))

(defcheck solution-b288fc2f
  (fn sss
    [x]
    (clojure.string/join "," (filter (fn [n] (not (empty? (filter #(= (* % %) n) (range (inc n)))))) (map #(integer %) (clojure.string/split x #","))))))

(defcheck solution-b289b723
  (fn [s]
    (let [ps (map #(* % %) (range))]
      (->> s
        (re-seq #"\d+") ; parse integers
        (map parse-int) ; convert to in
        (filter #(loop [ps ps] ; check if they are a perfect square
                   (cond
                     (= (first ps) %) true
                     (> (first ps) %) false
                     :else (recur (next ps)))))
        (clojure.string/join ",")))))

(defcheck solution-b30a790c
  (fn find-sq [s]
    (clojure.string/join ","
      (filter
        (fn [x] (reduce #(or %1 (and (= (quot (parse-int x) %2) %2) (zero? (mod (parse-int x) %2))))
                  false
                  (range 2 (parse-int x))))
        (clojure.string/split s #",")))))

(defcheck solution-b417cfb1
  (fn [s]
    (letfn [(perfect-sqrt [n]
              (if (> (rem (Math/sqrt n) 1) 0) nil n))]
      (->> (clojure.string/split s #",")
        (map parse-int)
        (map perfect-sqrt)
        (filter (complement nil?))
        (interpose ",")
        (apply str)))))

(defcheck solution-b4a88094
  (fn [s]
    (let [xs (map #(integer %) (clojure.string/split s #","))
          p? (fn [x] (some #(= x (* % %)) (range 2 x)))]
      (clojure.string/join "," (filter p? xs)))))

(defcheck solution-b4c15cc0
  (fn [s]
    (apply str (let [is-perfect-number (fn [x] (not (empty? (filter #(= x (* % %)) (range (inc x))))))]
                 (interpose
                   "," (map
                         str
                         (filter
                           #(is-perfect-number (parse-int %))
                           (re-seq #"\d+" s)
                           )
                         )
                   )
                 ))
    ))

(defcheck solution-b4cfb675
  (fn [ns]
    (let [ns-parsed         (map #(parse-double %) (.split ns ","))
          is-perfect-square (fn [n] (let [root (Math/sqrt n)] (= root (Math/floor root))))]
      (apply str (interpose "," (map int (filter is-perfect-square ns-parsed)))))))

(defcheck solution-b4f697e2
  (fn
    [s]
    (let [ns (map parse-int (clojure.string/split s #","))
          m (apply max ns)
          sqs (into #{} (map #(* % %) (range m)))
          r (filter #(sqs %) ns)]
      (apply str (interpose "," r)))))

(defcheck solution-b520362f
  (fn [c]
    (clojure.string/join "," (filter #(let [s (-> % parse-int Math/sqrt)] (== s (int s))) (clojure.string/split c #",")))))

(defcheck solution-b56889f6
  (fn [s] (clojure.string/join "," (filter #(= (int (Math/pow (int (Math/sqrt %)) 2)) % ) (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-b578cc79
  (fn [s]
    (let [ns (clojure.string/split s #",")]
      (clojure.string/join ","
        (filter (fn [b] (let [x (Math/sqrt b)]
                          (== x (int x)))
                  )
          (map (fn [i] (parse-int i)) ns))
        )
      )
    ))

(defcheck solution-b5b5f5dd
  (fn myPerfectSquares
    [strNums]
    (let [powers (fn [x] (= 0.0 (mod (Math/sqrt x) 1)))
          nums (map #(integer %) (clojure.string/split strNums #"\,"))]
      (apply str (interpose "," (filter #(powers %) nums))))))

(defcheck solution-b5f859ee
  (fn [x]
    (str (clojure.string/join \, (filter #(zero? (rem (Math/sqrt (parse-int %)) 1.0)) (re-seq #"\d+" x))))))

(defcheck solution-b6150ba2
  (fn [ns]
    (->>
      (.split ns ",")
      (map #(parse-int %))
      (filter #(== (int (Math/sqrt %)) (Math/sqrt %)))
      (clojure.string/join ","))))

(defcheck solution-b6ad7742
  (fn [s]
    (let [ns (map #(parse-int %) (clojure.string/split s #","))]
      (clojure.string/join "," (filter #(== % (-> % Math/sqrt Math/floor (Math/pow 2))) ns)))))

(defcheck solution-b6b9c777
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map parse-int)
      (filter #(let [r (int (Math/sqrt %))] (= % (* r r))))
      (interpose ",")
      (apply str))))

(defcheck solution-b6c945
  (fn [s]
    (clojure.string/join ","
      (filter (fn [x]
                (let [sqrt (Math/pow x 0.5)]
                  (== sqrt (int sqrt))))
        (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-b6e9145f
  (fn [string]
    (clojure.string/join ","
      (filter identity (map (fn [s] (if (== 0 (mod (Math/sqrt (integer s)) 1)) s))
                         (clojure.string/split string #","))))))

(defcheck solution-b71f7a9b
  (fn [s]

    (letfn [(square? [n] (zero? (rem (Math/sqrt n) 1)))]
      (clojure.string/join ","
        (filter
          square?
          (map
            #(integer %)
            (re-seq #"\d+" s)))))))

(defcheck solution-b78e7eaf
  (fn[s](
          apply str (interpose "," (filter #(not(> (Math/sqrt (parse-int %)) (int(Math/sqrt (parse-int %))) )) (clojure.string/split s #",")))
          )))

(defcheck solution-b7cb550b
  (fn [s]
    (->> (re-seq #"\d+" s)
      (filter (comp #(= % (Math/ceil %)) #(Math/sqrt %) parse-int))
      (clojure.string/join ","))))

(defcheck solution-b7ea9c54
  (fn [s]
    (apply str
      (interpose
        ","
        (filter
          #(= 0.0 (rem (Math/sqrt (parse-int %)) 1))
          (clojure.string/split s #","))))))

(defcheck solution-b8132ef1
  (fn [s]
    (let [ns (clojure.string/split s #",")]
      (clojure.string/join ","
        (filter
          #(let [i  (parse-int %)
                 rt (int (Math/sqrt i))]
             (= (* rt rt) i))
          ns)))))

(defcheck solution-b813996d
  (fn [s]
    (letfn [(sq? [i] (zero? (mod (Math/sqrt i) 1)))]
      (clojure.string/join "," (filter sq? (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-b862724
  (fn [d]
    (let [l (map #(parse-int %)
              (clojure.string/split d #","))
          s (map #(Math/ceil (Math/sqrt %)) l)]
      (clojure.string/join ","
        (map first
          (filter (fn [[a b]] (== a (* b b)))
            (partition 2 (interleave l s))))))))

(defcheck solution-b945fa5b
  (fn [s]
    (let [square (fn [n] (* n n))
          perfect-square? (fn [n] (= n (square (int (Math/sqrt n)))))]
      (->> (.split s ",")
        (map parse-int)
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-b94eeec9
  (fn [s]
    (let [square #(* % %)
          isqrt #(int (Math/sqrt %))
          square? #(= (square (isqrt %)) %)]
      (->> (re-seq #"\d+" s)
        (map parse-int)
        (filter square?)
        (map str)
        (clojure.string/join ",")))))

(defcheck solution-b97245c2
  (fn [st] (clojure.string/join ","
             (for [s (clojure.string/split st #"," ) :when
                   ((fn [x] (= x (* (int (Math/sqrt x))  (int (Math/sqrt x))))) (parse-int  s))]
               s))))

(defcheck solution-b9a9c562
  (fn [s] (clojure.string/join \,
            (filter #(zero? (mod (Math/sqrt (parse-int %)) 1))
              (clojure.string/split s #",")))))

(defcheck solution-b9dc72ae
  (fn perfects
    [string]
    (apply str (drop-last
                 (interleave (filter (fn [c] (let [n (parse-int c), root_n (int (Math/sqrt n))] (= n (* root_n root_n))))
                               (map str (seq (.split string ",")))) (repeat ","))))))

(defcheck solution-b9e77011
  (fn [s]
    (clojure.string/join ","
      (filter #(= % (let [sq (Math/round (Math/sqrt %))] (* sq sq)))
        (map #(parse-int %)
          (clojure.string/split s #"\,"))))))

(defcheck solution-baa5aadc
  (fn
    [s]
    (loop [e(map parse-int (clojure.string/split s #",")) n ""]
      (if (empty? e)
        (apply str (butlast n))
        (if (= (Math/sqrt (first e)) (Math/floor (Math/sqrt (first e))))
          (recur (rest e) (str n(first e) \,))
          (recur (rest e) n)))
      )
    ))

(defcheck solution-badb636
  (fn [numbers]
    (clojure.string/join ","
      (filter
        (fn is-perfect-sqare [n]
          (some #(= n (* % %)) (range 1 (inc (int (/ n 2.0))))))
        ((fn str-to-ints [s]
           (map #(parse-int %) (re-seq #"\d+" s))) numbers)))))

(defcheck solution-bb1066fe
  (fn [cs]
    (let [ns (map parse-int (re-seq #"\d+" cs))
          isqr?
             (fn [n]
               (loop [m 2
                      m' (inc m)
                      n' (* m m)
                      ]
                 (cond
                   (= n n') true
                   (< n n') false
                   (<= n' m) false
                   :else
                   (recur m' (inc m') (* m' m') )))
               )
          ns' (filter isqr? ns)
          sqs (map str ns')
          ]
      (if (empty? sqs)
        sqs
        (reduce
          (fn [s1 s2] (str s1 "," s2))
          sqs)
        )
      )
    ))

(defcheck solution-bc8aba85
  (fn filter-perfect-squares [s]
    (clojure.string/join ","
      (filter (fn [n]  (not (nil? (some #(= (* % %) n) (range 2 n)))))
        (map #(parse-int %) (clojure.string/split s #"\D+"))))))

(defcheck solution-bc8bfe85
  (fn [input-str]
    (letfn [(is-squared? [x]
              (loop [i 1]
                (cond
                  (= (* i i) x) true
                  (> (* i i) x) false
                  :else (recur (inc i)))))]
      (clojure.string/join ","
        (filter is-squared?
          (map #(parse-int %)
            (clojure.string/split input-str #",")))))))

(defcheck solution-bd266357
  (fn [s]
    (clojure.string/join ","
      (filter #(= (int (Math/pow (int (Math/sqrt %)) 2)) %)
        (map #(parse-int %) (clojure.string/split s #"\,"))))))

(defcheck solution-bd69939e
  (letfn
   [(str->nums [s]
      (map parse-int (clojure.string/split s #",")))
    (nums->str [nums]
      (apply str (interpose  "," (map str nums))))
    (perfect? [n]
      (== 0 (rem (Math/sqrt n) 1)))]

    (fn [s]
      (->> s
        (str->nums)
        (filter perfect?)
        (nums->str)))))

(defcheck solution-bd6c7016
  (fn [s]
    (->>
      (re-seq #"\d+" s)
      (filter #(let [n (parse-double %)]
                 (= n (Math/pow (int (Math/sqrt n)) 2))))
      (interpose ",")
      (apply str))))

(defcheck solution-bd937062
  (fn [x] (->> x
            (re-seq #"\d+")
            (map #(parse-int %))
            (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %)))))
            (clojure.string/join ","))))

(defcheck solution-be1345e1
  (fn [s]
    (letfn [(is-perfect-square [n]
              (let [root (Math/sqrt n)]
                (= root (Math/floor root))))]
      (->> (re-seq #"\d+" s)
        (map #(parse-int %))
        (filter is-perfect-square)
        (map (comp seq str))
        (interpose \,)
        (flatten)
        (apply str)))))

(defcheck solution-be941ebb
  (fn perf-square [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          all-perf-sq (set (map #(* % %) (range 100)))
          sq-nums (filter all-perf-sq nums)]
      (apply str (interpose "," sq-nums)))))

(defcheck solution-beb13613
  (fn [s]
    (let [squares (map * (range 1 10) (range 1 10))
          nums (map parse-int (re-seq #"[0-9]+" s))]
      (apply str (interpose "," (filter #(some (partial = %) squares) nums))))))

(defcheck solution-bee0a956
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(integer %))
      (filter #(= (float %) (Math/pow (Math/sqrt %) 2)))
      (clojure.string/join ","))))

(defcheck solution-bf112113
  (fn [s]
    (letfn [(is_square? [n]
              (let [sq (int (Math/sqrt n))]
                (= n (* sq sq))))

            (s2l [s]
              (clojure.string/split s #","))
            ]
      (clojure.string/join ","
        (filter #(is_square? %)
          (map #(parse-int %)
            (s2l s))))
      )))

(defcheck solution-bf13db4f
  (fn [s]
    (apply str
      (butlast
        (interleave
          (filter #{4 9 16 25 36 49}
            (map #(parse-int %) (.split s ",")))
          (repeat ","))))))

(defcheck solution-bf33be98
  (fn [s] (clojure.string/join "," (filter #(zero? (rem (Math/sqrt %) 1)) (map parse-int (.split s ","))))))

(defcheck solution-bfb44137
  (fn [nums]
    (apply
      str
      (interpose
        ","
        (filter
          (fn [x]
            (let [n (parse-int x)]
              (let [div (Math/sqrt n)]
                (== (* div div) n)
                )
              )
            )
          (clojure.string/split nums #","))
        )
      )
    ))

(defcheck solution-c05070ae
  (fn [s] (letfn [(square? [n] (let [r (Math/sqrt n)] (= (float (int r)) r)))]
            (clojure.string/join "," (map str (filter #(square? (parse-int %)) (clojure.string/split s #"," ))
                                       )))))

(defcheck solution-c05f4e61
  (fn [x]
    (let [
          vals (map (fn [y] (parse-int y)) (clojure.string/split x #","))
          filtered (filter (fn [x] (= (Math/sqrt x) (Math/floor (Math/sqrt x)) )) vals)
          res (clojure.string/join "," filtered)
          ]
      res)))

(defcheck solution-c072ac0a
  (fn [num-str]
    (clojure.string/join
      ","
      (filter (fn [num]
                (let [root (Math/sqrt num)]
                  (== root (int root))))
        (map #(parse-int %) (clojure.string/split num-str #","))))))

(defcheck solution-c082fb9e
  (fn to-union [s]
    (let [nums (apply conj (sorted-set) (map #(integer %) (clojure.string/split s #",")))]
      (reduce #(str %1 "," %2)
        (clojure.set/intersection
          (apply conj
            (sorted-set)
            (rest (take
                    (inc (Math/ceil (Math/sqrt (apply max nums))))
                    (map #(* % %) (range)))))
          nums)))))

(defcheck solution-c1bfcc5a
  (fn [s]
    (let [coll (map #(integer %) (re-seq #"\d+" s))
          pn (map #(* % %) (range))
          ss (->> coll
               (filter #((set (take % pn)) %) )
               (map #(str \, %))
               (apply str))]
      (subs ss 1))))

(defcheck solution-c1e88275
  (fn perfect-square [s]
    (let [slist (clojure.string/split s #",")]
      (clojure.string/replace-first
        (reduce #(let [n (parse-int %2)
                       x (int (Math/sqrt n))]
                   (if (= (* x x) n)
                     (str %1 "," %2)
                     (str %1)))
          "" slist) #"," ""))))

(defcheck solution-c1f67746
  (fn [s] (apply str (interpose "," (filter #(zero? (mod (Math/sqrt %) 1)) (map parse-int (re-seq #"\d+" s)))))))

(defcheck solution-c21e3e90
  (fn [numstr]
    (clojure.string/join "," (let
                              [numlist (map (fn [x] (integer x)) (clojure.string/split numstr #",")),
                               minnum (apply min numlist),
                               maxnum (apply max numlist),
                               squares (filter (fn [anum] (and (<= anum maxnum) (>= anum minnum))) (map (fn [sqr] (* sqr sqr)) (range maxnum)))]
                               (filter (fn [x] (not (nil? x))) (mapv (fn [a b] (if (= a b) a nil)) (cycle squares) (flatten (map (fn [anum] (replicate (count squares) anum)) numlist))))
                               )
      )
    ))

(defcheck solution-c21facb0
  (fn [s]
    (let [perfect-square? (fn [x]
                            (let [sqrt (Math/sqrt x)]
                              (== sqrt (int sqrt))))
          nums (map #(parse-int %) (clojure.string/split s #","))]
      (clojure.string/join "," (filter perfect-square? nums)))))

(defcheck solution-c2294051
  (fn eka
    [word]
    (->> (re-seq #"\d+" word)
      (map #(integer %))
      (filter #(let [res (Math/sqrt %)
                     resround (float (Math/round res))]
                 (= res resround)))
      (clojure.string/join ",")
      )
    ))

(defcheck solution-c25fd0fa
  (fn [s]
    (let [coll (clojure.string/split s #",")]
      (loop [remaining coll ans []]
        (if (empty? remaining)
          (apply str (butlast ans))
          (let [root (Math/floor (Math/sqrt (integer (first remaining))))]
            (if (= (* root root) (Math/floor (integer (first remaining))))
              (recur (rest remaining) (conj ans (str (integer (first remaining))) ","))
              (recur (rest remaining) ans))))))))

(defcheck solution-c30b3df
  (fn f [s]    (reduce str (next (interleave (iterate (fn [v] v) ",") (filter (fn ff [n] (= (parse-int n) (#(* % %) (int (Math/sqrt (parse-int n)))))) (seq (.split s ","))))))))

(defcheck solution-c337aa4
  #(let [strs (clojure.string/split % #"\W")
         ints (for [n strs] (parse-int n))
         psqr? (fn [n] (zero? (mod (Math/sqrt n) 1)))]
     (apply str (interpose \, (filter psqr? ints)))))

(defcheck solution-c351b2f7
  (fn [x]
    (apply str
      (interpose \,
        (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2)))
          (map #(parse-int %)
            (re-seq #"[0-9]+" x)))))))

(defcheck solution-c4d8b4ce
  (fn fpf [comma-nums]
    (let [csplit (fn [text] (clojure.string/split text #","))
          cjoin  (fn [ns] (clojure.string/join "," ns))
          perfect-square (fn [n]
                           (let [root (int (Math/sqrt n))]
                             (= n (* root root))))]
      (->> comma-nums
        csplit
        (map #(parse-int %))
        (filter perfect-square)
        cjoin))))

(defcheck solution-c5df70ff
  (fn __ [s]
    (->> (clojure.string/split s #",")
      (map #(integer %))
      (filter (fn [x]
                (let [s (int (Math/floor (Math/sqrt x)))]
                  (= (* s s) x))))
      (clojure.string/join ","))))

(defcheck solution-c5fb0ede
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map parse-int)
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))))
      (clojure.string/join ",")
      )))

(defcheck solution-c69b457a
  (fn filter-perfect-seq1
    [s]
    (letfn [(perfect-sq? [n] (= n (#(* % %) (int (Math/sqrt n)))))]
      (->> (clojure.string/split s #",")
        (map parse-int)
        (filter perfect-sq?)
        (map str)
        (clojure.string/join ",")))))

(defcheck solution-c74eed8a
  (fn [s]
    (reduce #(str % "," %2)
      (filter #(zero? (- (Math/sqrt %) (int (Math/sqrt %))))
        (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-c78bde88
  (fn [n]
    (let [nums (map parse-int (.split n ","))]
      (clojure.string/join ","
        (map str
          (filter #(reduce (fn [a b] (or a (= % (* b b))))  false (range 2 %)) nums))))))

(defcheck solution-c79e2ed9
  (fn [s]
    (let [sqrs (set (map #(str (* % %)) (range 10)))]
      (->> s
        (re-seq #"\d+")
        (filter #(contains? sqrs %))
        (clojure.string/join ",")
        ))))

(defcheck solution-c7be51bd
  (fn [s]
    (clojure.string/join ","
      (filter
        (fn [n] (some #(= n (* % %)) (range 2 n)))
        (map #(parse-int %) (re-seq #"\d+" s))))))

(defcheck solution-c7c7c4ea
  (fn [s]
    (letfn [(perfect-square? [n]
              (let [k (int (Math/sqrt n))]
                (= (* k k) n)))
            (numbers [s]
              (map #(parse-int %)
                (clojure.string/split s #",")))]
      (->> s numbers (filter perfect-square?) (clojure.string/join ",")))))

(defcheck solution-c861d0ef
  (fn [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          isPSN (fn [x]
                  (some #(= x (* % %)) (range 1 (inc x))))
          psns (filter isPSN nums)]
      (clojure.string/join "," (map str psns)))))

(defcheck solution-c8c42e1
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map #(integer %))
      (filter #(== % (Math/pow (int (Math/sqrt %)) 2)))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-c9172aff
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(< (mod (Math/sqrt %) 1) 0.000001))
      (clojure.string/join ","))))

(defcheck solution-c92e0d27
  (fn [s]
    (clojure.string/join
      ","
      (filter #(== (* (Math/sqrt %)
                     (Math/sqrt %)) %)
        (map #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-c94b737b
  (fn [s]
    (let [xs (map #(integer %) (clojure.string/split s #","))
          perf-sqrt? (fn [x]
                       (let [sq (int (Math/sqrt x))]
                         (= x (* sq sq))))
          ys (filter perf-sqrt? xs)]
      (apply str (interpose "," (map str ys))))))

(defcheck solution-c996247e
  (fn [xs] (let [ns (map parse-int (re-seq #"\d+" xs)) pns (clojure.set/intersection (into #{} (map #(* % %) (range (apply max ns)))))] (clojure.string/join "," (filter #(when (pns %) %) ns)))))

(defcheck solution-c9ab3823
  (fn [s] (letfn [(sqr? [n] (= n (first (drop-while #(< % n) (map #(* % %) (iterate inc 1))))))]
            (apply str (interpose "," (filter sqr? (map parse-int (re-seq #"\d+" s))))))))

(defcheck solution-caa32775
  (fn only-perfects [input]
    (let [nums (map #(integer %) (clojure.string/split input #","))
          sqrts (map #(Math/sqrt %) nums)
          perfects (filter #(= 0 (compare % (int %))) sqrts)]
      (clojure.string/join "," (map #(* (int %) (int %)) perfects)))))

(defcheck solution-cadad6b8
  (fn perfect-sqrt-nums [num-seq]
    (letfn [(perfect-square? [s]
              (let [n-sqrt (Math/sqrt (parse-int s))]
                (= (double 0) (double (- n-sqrt (Math/floor n-sqrt))))))]
      (->> (interpose "," (filter perfect-square? (re-seq #"\d+" num-seq)))
        (apply str)))))

(defcheck solution-cb124a20
  (fn f [x]
    (let [l (map #(parse-int %) (clojure.string/split x #","))
          s (set (map #(* % %) (range 1 (apply max l))))]
      (clojure.string/join ","
        (filter #(contains? s %) l)))))

(defcheck solution-cb718101
  (fn [s]
    (let [items (clojure.string/split s #",")
          integers (map #(integer %) items)
          roots (map #(Math/sqrt %) integers)
          psqs (map #(and (== %1 (* %2 %2)) %1) integers roots)]
      (clojure.string/join "," (filter identity psqs)))))

(defcheck solution-cb7d7a3e
  (fn [n] (clojure.string/join "," (filter #(let [x (int (Math/sqrt %))] (= % (* x x))) (map (fn [i] (parse-int i)) (re-seq #"\d+" n))))))

(defcheck solution-cbbce0fc
  (fn filter-perfct-squares
    [string]
    (let [given-seq (map #(parse-int %) (re-seq #"\d+" string))
          max-number (apply max given-seq)
          predicate (into #{} (take-while #(<= % max-number) ((fn squares
                                                                [x] (lazy-seq (cons (* x x) (squares (inc x))))) 1)))]
      ;; lazy sequence for all the perfect squares
      (reduce #(str %1 "," %2) (filter predicate given-seq)))))

(defcheck solution-cc55f47a
  (fn [x]
    (apply str (interpose ","
                 (filter #(let [s (int (Math/sqrt %))] (= % (* s s)))
                   (map #(parse-int %) (re-seq #"\d+" x)))))))

(defcheck solution-cc7f1a69
  (fn [s]
    (let [nums (clojure.string/split s #",")
          is-sqr (fn [n]
                   (let [sqrt (Math/sqrt n)]
                     (= sqrt (Math/ceil sqrt))))]
      (clojure.string/join "," (filter (comp is-sqr parse-int) nums)))))

(defcheck solution-cc92343a
  (fn [numstr]
    (let [nums (map #(parse-int %) (.split numstr ","))
          sorted-nums (sort nums)
          sqrs (into #{} (map #(* % %) (range 1 (inc (Math/sqrt (last sorted-nums))))))
          join (fn [v] (reduce #(str %1 "," %2) "" v))]
      (subs (join (filter #(contains? sqrs %) nums)) 1))))

(defcheck solution-cce284ab
  #(apply str (interpose \, (filter (fn [x]
                                      (some (fn [t] (= (str (* t t)) x) ) (range 1 50))) (re-seq #"[0-9]+" %)))))

(defcheck solution-ccf3a33f
  (fn [st]
    (let [ ss (clojure.string/split st #",")
          is (map #(integer %) ss)
          fs (filter #(= (rem (Math/sqrt %) 1) 0.0) is)
          rs (apply str (interpose "," fs))
          ]
      rs)))

(defcheck solution-cd2b98d1
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(zero? (- %  (Math/pow (Math/sqrt %) 2))))
      (clojure.string/join ","))))

(defcheck solution-cdbccee0
  (fn [s]
    (let [nums (map #(parse-int %)
                 (clojure.string/split s #","))]
      (letfn [(is-perf-sq [n] (let [sr (Math/sqrt n)]
                                (= (Math/floor sr) (Math/ceil sr))))]
        (clojure.string/join "," (filter is-perf-sq nums))))))

(defcheck solution-cddae551
  (fn [st]
    (let [nums (map parse-int (clojure.string/split st #","))
          squares (for [x (range)] (* x x))
          res (filter (fn [e] (= e (last (take-while #(<= % e) squares)))) nums)]
      (apply str (interpose "," res)))))

(defcheck solution-ce85a32b
  (fn [s]
    (->> (.split s ",")
      (map parse-int)
      (filter (fn [i]
                (let [x (Math/sqrt i)]
                  (== x (int x)))))
      (clojure.string/join ","))))

(defcheck solution-ce9c9d1b
  (fn [s]
    (let [perfect-square?
          (fn [n] (let [sq-root (int (Math/sqrt n))] (= (* sq-root sq-root) n)))]
      (apply str (interpose \, (filter perfect-square? (map #(parse-int %) (re-seq #"[0-9]+" s))))))))

(defcheck solution-d0349d84
  (fn [s]
    (->> (clojure.string/split s #",")
      (map (fn [x] (integer x)))
      (filter (fn [x] (= 0.0 (rem (Math/sqrt x) 1))))
      (clojure.string/join ","))))

(defcheck solution-d13efb7d
  (fn [l] (apply str (interpose "," (filter (set (map (comp str #(* % %)) (range 10))) (re-seq #"[0-9]+" l))))))

(defcheck solution-d180f4f4
  (fn [s] (clojure.string/join "," (filter (set (map #(str (* % %)) (range 100))) (#(re-seq #"\d+" %) s)))))

(defcheck solution-d1abb2f3
  (fn [s]
    (clojure.string/join ","
      (filter
        ; (fn [x] (some #(= x (* % %)) (range x) ))
        #(= % (* (long (Math/sqrt %)) (long (Math/sqrt %))))
        (map #(parse-int %) (re-seq #"\d+" s))))))

(defcheck solution-d1d2c357
  (fn [s]
    (let [nums (map #(parse-int %) (.split s ","))
          p2 #(* % %)
          sqs (filter (fn [n] (= n (p2 (int (Math/sqrt n))))) nums)]
      (apply str (interpose \, sqs)))))

(defcheck solution-d22aa9f1
  (fn task-72 [s]
    (let [is-square?
                  (fn [n]
                    (let [s (int (Math/sqrt n))]
                      (= (* s s) n)))
          numbers (map #(integer %) (clojure.string/split s #","))
          squares (filter is-square? numbers)
          result  (clojure.string/join "," (map str squares))]
      result)))

(defcheck solution-d2ad79a
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(integer %))
      (filter (fn perfect-square? [n]
                (= n (last (take-while #(<= % n)
                             (map #(* % %) (range)))))))
      (clojure.string/join ","))))

(defcheck solution-d2fa5a04
  #(if (= (subs %1 0 1) "4")
     "4,9"
     "16,25,36"))

(defcheck solution-d3825142
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %)))))
      (clojure.string/join ","))))

(defcheck solution-d3838342
  (fn [s]
    (let [xs (map #(parse-int %) (clojure.string/split s #","))
          ff (fn [n] (some #(= (* % %) n) (range n)))]
      (clojure.string/join "," (filter ff xs)))))

(defcheck solution-d3c7969
  (fn
    [s]
    (let [nums (map parse-int (vec (.split s ",")))
          square (fn [n] (let [divs (take-while #(<= (* % %) n) (range))
                               lst (last divs)]
                           (and lst (= n (* lst lst)))))
          sqs (filter square nums)]
      (reduce #(.concat % %2) (interpose "," (map str sqs))))))

(defcheck solution-d3e5b64b
  (fn [s]
    (->>
      (clojure.string/split s #",")
      (map #(integer %))
      (filter
        (fn [x] (some
                  #(= (* % %) x)
                  (range 1 x))))
      (interpose ",")
      (apply str))))

(defcheck solution-d3ef13e8
  (fn [s]
    (apply str (interpose ","
                 (filter
                   (fn [n]
                     (seq (filter #(= n %) (map #(* % %) (range 1 n)))))
                   (map #(parse-int %) (.split s ",")))))))

(defcheck solution-d44d0e29
  (fn ps [st]
    (letfn [(perf-square [x]
              (loop [s (map #(* % %) (range))]
                (let [sq (first s)]
                  (if (= sq x)
                    true
                    (if (< sq x)
                      (recur (rest s))
                      false)))))]
      (reduce  str
        (drop 1
          (interleave  (repeat ",")
            (filter perf-square (map (comp parse-int str) (re-seq #"\d+" st)))))))))

(defcheck solution-d4d8c495
  (fn [s]
    (let
     [nums (sort (map #(parse-int %) (clojure.string/split s (re-pattern ","))))
      maxnum (apply max nums)
      roots (take-while #(>= maxnum (* % %)) (range))
      squares (set (map #(* % %) roots))]
      (clojure.string/join "," (filter #(squares %) nums)))))

(defcheck solution-d4d9b369
  (fn [n]
    (clojure.string/join
      ","
      (filter #(let [s (first (drop-while (fn [x] (> % (* x x))) (iterate inc 1)))]
                 (= (* s s) %))
        (map #(parse-int %)
          (clojure.string/split n #","))))))

(defcheck solution-d4e7a5ee
  (fn [x]
    (clojure.string/join ","
      (filter #(= (rem (Math/sqrt %) 1) 0.0)
        (map #(parse-int %) (clojure.string/split x #","))))))

(defcheck solution-d54c960f
  (fn [s]
    (let [nums (map parse-int (re-seq #"\d+" s))
          powers (map #(* % %) (iterate inc 1))]
      (clojure.string/join ","
        (filter (fn [n]
                  (some #(= % n)
                    (take-while #(<= % n) powers))) nums))
      )
    ))

(defcheck solution-d5870eff
  (fn [s]
    (clojure.string/join ","
      (for [s (.split s ",")
            :let [s (parse-int s)


                  i (int (Math/sqrt s))]
            i (range (dec i) (inc i))
            :when (= s (* i i))
            ] s))))

(defcheck solution-d5a29f63
  (fn [l] (->> (clojure.string/split l #",")
            (map #(parse-int %))
            (filter #(= % (int (Math/pow (Math/floor (Math/sqrt %)) 2))))
            (clojure.string/join ","))))

(defcheck solution-d5fbc0f4
  (fn fps [s]
    (let [bab_sqrt? (fn [m]
                      (loop [n (quot m 2) seen #{}]
                        (let [n2 (quot (+ n (/ m n)) 2)]
                          (cond (seen n2) false
                                (= m (* n2 n2)) true
                                :else (recur n2 (conj seen n2))))))]
      (->> s
        (re-seq #"\d+")
        ; a perfect square never ends  in 2, 3, 7 or 8
        (remove #(#{\2 \3 \7 \8} (last %)))
        (map parse-int)
        ; using Babylonian algo to check if the number is a perfect square
        (filter bab_sqrt?)
        (interpose \,)
        (apply str)))))

(defcheck solution-d60a85cc
  (fn [s]
    (->> s
      (re-seq #"[0-9]+")
      (map #(parse-int %))
      (filter #(= 0.0 (rem (Math/sqrt %) 1.0)))
      (interpose ",")
      (apply str))))

(defcheck solution-d6ad8e4f
  (fn [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(= 0.0 (mod (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-d70ae43d
  (fn [s] (apply str (interpose \,
                       (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %))))
                         (map #(parse-int %) (clojure.string/split s #",")))))))

(defcheck solution-d753846b
  (fn [s]
    (let [tokens (re-seq #"\d+" s)
          numbers (map parse-int tokens)
          squares (filter #(> 0.0000000001 (let [r (Math/sqrt %)] (Math/abs (- r (Math/round r))))) numbers)]
      (apply str (interpose "," (map str squares))))))

(defcheck solution-d7999ab5
  (fn [s]
    (apply str
      (butlast
        (interleave
          (filter (fn [y] (some (set (map #(* % %) (range 100))) [y]))
            (map #(parse-int %) (clojure.string/split s #","))
            ) (repeat ","))))
    ))

(defcheck solution-d7d9e753
  (fn [nums-text]
    (letfn [(square? [n]
              (some #(= n (* %1 %1)) (range n)))
            (splitz [text]
              (map #(parse-int %) (clojure.string/split text #",")))
            (joinz [nums]
              (clojure.string/join "," nums))]
      (joinz (filter square? (splitz nums-text))))))

(defcheck solution-d7e645b9
  (fn
    [s]
    (letfn [(perfect-square? [n]
              (let [sq (Math/sqrt n)]
                (== (int sq) sq)))]
      (apply str (interpose "," (filter perfect-square? (map #(parse-int %) (re-seq #"\d+" s))))))))

(defcheck solution-d81b2511
  (fn __
    [string]
    (let [perfect-squares (reduce #(conj %1 (* %2 %2)) #{} (range 100))]
      (->>
        (clojure.string/split string #",")
        (map #(parse-int %))
        (filter perfect-squares)
        (clojure.string/join ",")))))

(defcheck solution-d849863
  (fn [a] (reduce #(str %1 "," %2)
            (filter (fn [x] (let [odm (Math/sqrt x) ro (int odm)]
                              (= 0.0 (- odm ro))))
              (map #(parse-int %) (clojure.string/split   a #","))))))

(defcheck solution-d8e8b44a
  (fn psquares-only [instring]
    (let [nums (map #(parse-int %) (re-seq #"\d+" instring))
          squares (filter #(= (Math/sqrt %) (Math/floor (Math/sqrt %))) nums)]
      (reduce #(str %1 "," %2) squares))))

(defcheck solution-d94c7851
  (fn perfect-squares?
    [strr]
    (let [vectorize (fn [l] (map #(parse-int %) (clojure.string/split l #",")))]
      (clojure.string/join ","
        (filter (fn [p] (== (Math/sqrt p) (int (Math/sqrt p)))) (vectorize strr))))))

(defcheck solution-d9d86e99
  (fn split-perfect [s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))))
      (map str)
      (interpose ",")
      clojure.string/join)))

(defcheck solution-da47215e
  (fn [n] (clojure.string/join "," (sort (seq (clojure.set/intersection #{"4" "9" "16" "25" "36"} (set (clojure.string/split n #","))))))))

(defcheck solution-da5edf62
  (fn [s]
    (clojure.string/join ","
      (filter (fn [x]
                (let [sqrt-x (Math/sqrt x)]
                  (= sqrt-x (double (int sqrt-x))))) (map #(parse-int %) (clojure.string/split s #","))))))

(defcheck solution-da9c2a19
  (fn [st]
    (clojure.string/join
      ","
      (->> (clojure.string/split st #",")
        (map parse-int)
        (filter #(let [rt (Math/sqrt %)] (= rt (Math/floor rt))))))))

(defcheck solution-dac79c4f
  (fn [x] (apply str  (interpose \,
                        (filter #(== (int (Math/sqrt %)) (Math/sqrt %))
                          (map #(parse-int %)
                            (re-seq #"\d+" x)))))))

(defcheck solution-dafaaa0d
  (fn [s]
    (reduce #(str %1 "," %2)
      (filter
        #(= (mod (Math/sqrt (parse-double %1)) 1) 0.0)
        (clojure.string/split s #"[,]+")))))

(defcheck solution-db15b313
  (fn [u]
    (apply str
      (butlast
        (apply concat
          (interleave
            (filter #(not (= "" %))
              (map
                (fn [x]
                  (let [y (parse-int x)]
                    (if (= (first (filter #(not (< % y)) (map (fn [w] (* w w)) (range))))  y) x "")))
                (re-seq #"\w+" u)))
            (iterate identity ",")))))))

(defcheck solution-db556f8b
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(parse-int %))
      (filter #(= % (int (Math/pow (int (Math/floor (Math/sqrt %))) 2))))
      (map str)
      (clojure.string/join ",")
      )))

(defcheck solution-db82457c
  (fn [str]
    (clojure.string/join
      ","
      (let [nums (clojure.string/split str #",")]
        (filter #(let [i (parse-int %)
                       sq (int (Math/sqrt i))]
                   (= sq (/ i sq)))
          nums)))))

(defcheck solution-dbd15595
  (fn [c] (clojure.string/join "," (filter #(let [d (parse-int %) r (int (Math/sqrt d))] (= d (* r r))) (.split c ",")))))

(defcheck solution-dbe520fe
  (fn [s]
    (clojure.string/join ","
      (filter
        (fn [x] (some #(= x (* % %)) (range x)))
        (map parse-int (.split s ","))))))

(defcheck solution-dbff25a2
  (fn [sq]
    (clojure.string/join
      ","
      (filter
        #(loop [n % i 1]
           (cond
             (neg? n) false
             (zero? n) true
             :else (recur (- n i) (+ 2 i))))
        (map parse-int (clojure.string/split sq #","))))))

(defcheck solution-dc56daae
  (fn [s]
    (->> s (re-seq #"\d+")
      (filter #(= (* (int (Math/sqrt (parse-int %)))
                    (int (Math/sqrt (parse-int %))))
                 (parse-int %)))
      (interpose ",")
      (apply str))))

(defcheck solution-dd052ccb
  (fn [s]
    (let [candidates (mapv #(integer %) (.split s ","))]
      (letfn [(is-perfect-square [n] (some #(= n (* % %)) (range n)))]
        (clojure.string/join "," (filter is-perfect-square candidates))))))

(defcheck solution-dd511b9
  (fn [s]
    (letfn [(perfect-square? [n] (let [r (int (Math/sqrt n))] (= n (* r r))))]
      (apply str (interpose "," (map str (for [is (map #(parse-int %) (.split s ","))
                                               :when (perfect-square? is)]
                                           is)))))))

(defcheck solution-dd5ec0ad
  (fn [s]
    (apply str
      (interpose ","
        (filter (fn [n] (let [sqr (Math/sqrt n)]
                          (zero? (- sqr (int sqr)))))
          (map #(parse-int %)
            (clojure.string/split s #"\,")))))))

(defcheck solution-de2796c
  (fn [numstrings]
    (let [nums (map parse-int (clojure.string/split numstrings #","))
          squares (map #(Math/sqrt %) nums)
          pairs (map vector nums squares)]
      (clojure.string/join "," (map first (filter (fn [v]
                                                    (= (Math/floor (Math/sqrt (first v))) (last v)))
                                            pairs))))))

(defcheck solution-de7f978d
  (fn[s] (apply str(interpose "," (filter #(= % (*(int(Math/sqrt %))(int(Math/sqrt %)))) (map #(parse-int %)  (clojure.string/split s #",")))))))

(defcheck solution-dea04545
  (fn [s]
    (->>
      (.split s ",")
      (filter
        (fn[a]
          (let [n (parse-int a)]
            (= n
              (int (Math/pow
                     (int (Math/sqrt n))
                     2))))))
      (clojure.string/join ","))))

(defcheck solution-dfb061ae
  (fn [s](apply str (interpose ","
                      (filter
                        (fn [n](some #(= n %) (map #(* % %) (range n))))
                        (map #(integer %) (clojure.string/split s #","))
                        )))))

(defcheck solution-e0017108
  (fn [s]
    (->> (.split s ",")
      (filter #(let [s (Math/sqrt (parse-int %))] (== (int s) s)))
      (clojure.string/join ","))))

(defcheck solution-e07d4754
  (fn [s]
    (clojure.string/join "," (filter
                               (fn [a] (let [r (Math/sqrt a)] (= (float (int r)) r)))
                               (map parse-int (re-seq #"\d+" s))
                               ))
    ))

(defcheck solution-e0d4b981
  (fn [s]
    (let [perfect #(= (Math/sqrt %)
                     (Math/floor (Math/sqrt %)))]
      (->>
        (clojure.string/split s #",")
        (map parse-int)
        (filter perfect)
        (clojure.string/join \,)))))

(defcheck solution-e1b9f624
  (fn [s]
    (clojure.string/join ","
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2)))
        (map #(parse-int %)
          (re-seq #"\d+" s))))))

(defcheck solution-e1ea1dd1
  (fn[s]
    (->> (clojure.string/split s #",")
      (map parse-int)
      (filter #(zero? (mod (Math/sqrt %) 1)))
      (clojure.string/join ","))))

(defcheck solution-e1fa5af4
  (fn [input]
    (apply str
      (butlast
        (interleave
          (filter
            (fn [val]
              (let [sqrt (int (Math/sqrt val))]
                (= val
                  (* sqrt sqrt))))
            (map #(parse-int %)
              (re-seq #"[0-9]+" input)))
          (repeat ","))))))

(defcheck solution-e3d65c47
  (fn [strs]
    (apply str
      (interpose ","
        (filter
          #(= 0.0 (mod (Math/sqrt %) 1))
          (map #(parse-int %) (.split strs ",")))))))

(defcheck solution-e3f79684
  (fn sqs [s]
    (let [squares (map #(* % %) (iterate inc 1))
          is-square? (fn [x]
                       (= x (first (drop-while #(< % x) squares))))]
      (apply str
        (interpose ","
          (map str
            (filter is-square?
              (map #(integer %)
                (clojure.string/split s #",")))))))))

(defcheck solution-e41bd68d
  (fn squares [s]
    (clojure.string/join
      ","
      (filter
        #(= % (int (Math/pow (int (Math/sqrt %)) 2)))
        (map #(integer %) (clojure.string/split s #","))))))

(defcheck solution-e432d1b8
  (fn [s]
    (clojure.string/join "," (filter
                               #(let [x (Math/sqrt %)] (= 0.0 (- x (int x))))
                               (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-e4d947f1
  (fn ps [s]
    (->> (clojure.string/split s #"[, ]")
      (filter #(zero? (rem (Math/sqrt (parse-int %)) 1)))
      (interpose \,)
      (apply str) )))

(defcheck solution-e5068f20
  (fn [s]
    (let [els (clojure.string/split s #",")]
      (apply str
        (->> els
          (map #(parse-int %))
          (filter #(= (mod (* (Math/sqrt %) 10) 10) 0.0))
          (interpose ","))))))

(defcheck solution-e566cc1d
  (fn fsq [s]
    (letfn [(perf-sq [n]
              (zero? (mod n (Math/sqrt n))))]
      (clojure.string/join \,
        (filter perf-sq
          (map parse-int
            (clojure.string/split s #",")))))))

(defcheck solution-e59f4033
  (fn [s]
    (let [coll (map #(parse-int %) (clojure.string/split s #","))]
      (clojure.string/join ","
        (filter (fn [x]
                  (some #(= x %) (map (fn [y] (* y y)) (range x))))
          coll)))))

(defcheck solution-e67b5685
  (fn find-square [string]
    (letfn [(square? [n]
              (== n (#(* % %) (Math/sqrt n))))]
      (clojure.string/join
        (butlast
          (interleave
            (filter square?
              (map parse-int
                (map str
                  (clojure.string/split string #",+"))))
            (repeat ",")))))))

(defcheck solution-e6dc0536
  (fn pr074 [s]
    (let [nums (map #(parse-int %) (clojure.string/split s #","))
          filt (filter #(zero? (rem (Math/sqrt %) 1)) nums)]
      (apply str (rest (interleave (cycle [","]) filt))))))

(defcheck solution-e762df06
  (fn squares [string] (let [numbers (vec (map parse-int (clojure.string/split string #",")))] (apply str (interpose "," (remove #(nil? %) (map (fn [x] ((set (map #(* % %) (range (+ 2 (int (Math/sqrt (apply max numbers))))))) x)) numbers)))))))

(defcheck solution-e79163dd
  some #(if (= \4 %) "4,9" "16,25,36"))

(defcheck solution-e7b0e92d
  (fn [s]
    (apply str (interpose ","
                 (filter
                   (fn [x]
                     (some
                       #(= x (* % %))
                       (range x)))
                   (map #(parse-int %) (.split s ",")))))))

(defcheck solution-e812906f
  (fn [s]
    (clojure.string/join
      ","
      (filter
        #(let [r (Math/sqrt %)]
           (= r (Math/floor r)))
        (map parse-int (re-seq #"\d+" s))))))

(defcheck solution-e885a426
  (fn [s] (clojure.string/join "," (filter #(= (mod (Math/sqrt %) 1) 0.0) (map parse-int (clojure.string/split s #","))))))

(defcheck solution-e8e0a808
  #(clojure.string/join "," (filter
                              (fn [x] (let [sq (Math/sqrt x)] (== sq (int sq))))
                              (map (fn [x] (parse-int x)) (re-seq #"\d+" %)))))

(defcheck solution-e8e5b3f8
  (fn sqr [s]
    (->> (map parse-int (clojure.string/split s #","))
      (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt (int %))))))
      (clojure.string/join ","))))

(defcheck solution-e9066dd1
  (fn[s]
    (let [square?
          (fn[n]
            ((fn
               [low high]
               (let [mid (quot (+ high low) 2)
                     msq (* mid mid)]

                 (cond
                   (= n msq)
                   true

                   (> low high)
                   false

                   :else
                   (if (< n msq)
                     (recur low (dec mid))
                     (recur (inc mid) high))))) 1 n))]

      (apply str (interpose ","
                   (filter square?
                     (map parse-int (clojure.string/split s #","))))))))

(defcheck solution-e95134ff
  (fn filter-perfect
    [s]
    (let [xs (map parse-int (clojure.string/split s #","))
          perfect? (fn [n]
                     (let [xs (range 2 (/ (inc n) 2))
                           divisors (filter #(= n (* % %)) xs)]
                       (= 1 (count divisors))))
          filtered (filter perfect? xs)]
      (apply str (interpose ","  filtered)))))

(defcheck solution-e9851a21
  (fn [s]
    (clojure.string/join ","
      (filter (fn [v] (= v (last (take-while #(<= % v) (map #(* % %) (range))))))
        (map parse-int (clojure.string/split s #","))))))

(defcheck solution-e9cee359
  (fn [s]
    (let [psq? (fn [n]
                 (let [r (int (Math/sqrt n))]
                   (= n (* r r))))
          ints (map #(parse-int %) (re-seq #"\d+" s))
          psqs (filter psq? ints)]
      (apply str (interpose "," psqs)))))

(defcheck solution-ea647a41
  (fn __ [s]
    (letfn [(perfectsquare? [n] (= 0.0 (mod (Math/sqrt n) 1) 0.0))]
      (clojure.string/join ","
        (filter perfectsquare?
          (map #(parse-int (str %)) (re-seq #"\d+" s)))))))

(defcheck solution-ea91e06f
  #(clojure.string/join
     ","
     (for [num-str (.split % ",")
           :let [num (parse-int num-str)
                 sqrt (int (Math/sqrt num))]
           :when (= (* sqrt sqrt) num)]
       num)))

(defcheck solution-eab27259
  (fn squares [s]
    (let [nums (map #(parse-int %) (re-seq #"\w+" s))
          square? (fn [x] (= (Math/floor (Math/sqrt x))
                            (Math/ceil (Math/sqrt x))))]
      (apply str (interpose "," (filter square? nums))))))

(defcheck solution-eb289457
  (fn [s] (apply str (interpose "," (map str (filter #(= % (int (* (int (Math/sqrt %)) (Math/sqrt %)))) (map #(integer %) (re-seq #"\w+" s))))))))

(defcheck solution-ee28aacf
  (fn [s] (clojure.string/join ","
            (filter
              (fn [c] (let [i (parse-int c)]
                        ((into #{} (map #(* % %) (range i)))
                         i)))
              (clojure.string/split s #","))
            )))

(defcheck solution-ee331130
  (fn [ss]
    (->> ss
      (re-seq #"\d+")
      (map parse-int)
      (filter #(let [sr (Math/sqrt %)] (== (int sr) sr)))
      (map str)
      (clojure.string/join "," ))))

(defcheck solution-ee3edb94
  (fn perf-sq [text]
    (let [perf-sq? (fn [x]
                     (let [rt (int (Math/sqrt x))]
                       (= x (* rt rt))))]
      (->> (re-seq #"\d+" text)
        (map #(parse-int %))
        (filter perf-sq?)
        (interpose \,)
        (apply str)))))

(defcheck solution-eeac4416
  (fn filter-perf-squares [s]
    (let [
          pow (fn [y] (int (Math/pow y 2)))
          ints (map #(parse-int %) (clojure.string/split s #","))
          is-perf-square? (fn [n] (loop [x 2]
                                    (cond
                                      (> x n) false
                                      (> (pow x) n) false
                                      (= (pow x) n) true
                                      :else (recur (inc x)))))
          perf-squares (filter is-perf-square? ints)
          ]
      (clojure.string/join "," perf-squares)
      )))

(defcheck solution-ef08c0d4
  (fn [s]
    (let [n (map #(parse-int %) (clojure.string/split s #","))]
      (clojure.string/join "," (filter (fn [v] (= (Math/floor (Math/sqrt v)) (Math/sqrt v))) n)))))

(defcheck solution-ef1726a2
  (fn [x] (->> x (re-seq #"\d+") (map #(parse-int %))
            (filter #(let [r (int (Math/sqrt %))] (= (* r r) %))) (interpose \,) (apply str))))

(defcheck solution-ef36da28
  #(apply str
     (interpose ","
       (filter (fn [n] (some (fn [x] (= n (* x x))) (range n)))
         (map parse-int (clojure.string/split % #","))))))

(defcheck solution-ef861154
  (fn filter-perf-sq
    [input-str]
    (->> (re-seq #"\d+" input-str)
      (map #(parse-int %))
      (filter #(== (Math/pow (int (Math/sqrt %)) 2) %))
      (clojure.string/join ","))))

(defcheck solution-ef88af98
  #(->>
     (clojure.string/split % #",")
     (map (fn [s] (parse-int s)))
     (filter (fn [x] (== (Math/sqrt x) (int (Math/sqrt x)))))
     (clojure.string/join ",")))

(defcheck solution-efab4755
  (fn [s]
    (reduce #(str % "," %2)
      ((group-by
         #(= (Math/sqrt (parse-int %)) (Math/floor (Math/sqrt (parse-int %))))
         (clojure.string/split s #",")) true))))

(defcheck solution-efddf65a
  (fn [s]
    (apply str (interpose ","
                 (filter #(zero? (mod % (Math/sqrt %)))
                   (map parse-int (.split s ",")))))))

(defcheck solution-effa4e19
  (fn [s]
    (let [nums (map #(parse-int %)
                 (clojure.string/split s #","))
          squares (map #(* % %) (range))
          square? (fn [n] (some #{n}
                            (take-while (partial >= n) squares)))]
      (apply str
        (interpose "," (filter square? nums))))))

(defcheck solution-effb3c6b
  (fn perfect-squares
    [s]
    (let [perfect-square? (fn
                            [n]
                            (let [int-square (int (Math/sqrt n))]
                              (= (* int-square int-square) n)))
          numbers (->> (re-seq #"\d+" s) (map #(parse-int %)))
          result (->> numbers (filter #(perfect-square? %)) (interpose ",") (apply str))]
      result)))

(defcheck solution-efff18ab
  (fn [s]
    (letfn
     [(perfSq? [n]
        (let [a (first (drop-while #(< (* % %) n) (range (inc n))))]
          (= (* a a) n)))]
      (apply str (interpose ","
                   (filter perfSq?
                     (map #(integer %)
                       (clojure.string/split s #","))))))))

(defcheck solution-f043a552
  (fn [s]
    (letfn [(R [n]
              (loop [x 0]
                (cond (< n (* x x)) false
                      (= n (* x x)) n
                      :else (recur (inc x)))))]
      (clojure.string/join ","
        (filter identity
          (map #(R (parse-int %))
            (clojure.string/split s #",")))))))

(defcheck solution-f04c32ef
  #(apply str (interpose "," (filter (fn [n] (let [s (Math/sqrt (parse-double n))] (= s (Math/floor s)))) (.split % ",")))))

(defcheck solution-f06d91e7
  (fn [arg] (let [coll (clojure.string/split arg #",")
                  nums (map #(integer  %) coll)
                  perfect-square? (fn [n] (zero? (mod (Math/sqrt n) 1)))
                  ]
              (apply str (interpose "," (filter perfect-square? nums)))
              )
    ))

(defcheck solution-f097bb1e
  (fn [s]
    (let [parse-int (fn [v] (parse-int v))
          perfect-square? (fn [n]
                            (let [r (int (Math/sqrt n))]
                              (= n (* r r))))]
      (->> (clojure.string/split s #",\s*")
        (map parse-int)
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-f0fdc025
  (fn [s]
    (->> (clojure.string/split s #",")
      (map #(integer %))
      (filter (fn [n]
                (loop [i 1]
                  (cond
                    (< (* i i) n) (recur (inc i))
                    (= (* i i) n) true
                    (> (* i i) n) false))))
      (clojure.string/join ","))))

(defcheck solution-f1171890
  (fn [ns]
    (->> (re-seq #"\d+" ns)
      (map #(parse-int %))
      (filter #(let [s (Math/sqrt %)] (zero? (- s (Math/floor s)))))
      (clojure.string/join ","))))

(defcheck solution-f1461a88
  (fn [in]
    (let [nums (map #(parse-int %) (re-seq #"\d+" in))]
      (apply str
        (interpose ","
          (filter (fn [n] (zero? (rem n (Math/sqrt n)))) nums))))))

(defcheck solution-f163683f
  (fn [src]
    (clojure.string/join "," (filter #(= (Math/ceil(Math/sqrt %))(Math/sqrt %)) (map parse-int (clojure.string/split src #","))))
    ))

(defcheck solution-f17dd4e6
  (fn filter-sq [s]
    (clojure.string/join ","
      (filter
        (fn [x]
          (= x (last (take-while
                       #(<= % x)
                       (map #(* % %) (range))))))
        (map #(integer %) (re-seq #"\d+" s))))))

(defcheck solution-f1c5f9c1
  (fn [s] (apply str
            (interpose "," (filter (set (map #(* % %) (range 10)))
                             (map #(parse-int %) (re-seq #"\d+" s)))))))

(defcheck solution-f2ca9737
  (fn foo [s]
    (clojure.string/join
      \,
      (filter #(= % (* (int (Math/sqrt %)) (int (Math/sqrt %))))
        (map #(parse-int %)
          (clojure.string/split s #","))))))

(defcheck solution-f2fe53c0
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (map #(integer %))
      (filter #(let [sq (Math/sqrt %)]
                 (= sq (-> sq int float))))
      (clojure.string/join ","))))

(defcheck solution-f334f6c9
  (fn [s]
    (apply str
      (interpose ","
        (filter #(let [l (int (Math/sqrt %))]
                   (= % (* l l)))
          (map #(parse-int %) (.split s ",")))))))

(defcheck solution-f340c03f
  (fn p-sqrs [n-str]
    (letfn [(get-nums [ns]
              (let [ns2 (.split ns ",")]
                (map #(parse-int %) ns2)))
            (elem [x ls]
              (and (>= x (first ls))
                   (or (= x (first ls))
                       (elem x (rest ls)))))]
      (let [sqs (map #(* % %) (range))
            ns2 (get-nums n-str)
            ns3 (filter #(elem % sqs) ns2)]
        (apply str (interpose "," ns3))))))

(defcheck solution-f345bd09
  (fn [y]
    (let [isSquare
          (fn isSquare [x] (some #(= x (* % %))  (range x)))
          ]
      (clojure.string/join "," (filter isSquare (map #(parse-int %) (clojure.string/split y #","))))
      )
    ))

(defcheck solution-f35219c5
  (fn [s]
    (->> s
      (re-seq #"\d+")
      (filter (fn [n] (zero? (mod (Math/sqrt (parse-int n)) 1))))
      (interpose ",")
      (apply str))))

(defcheck solution-f375db91
  (fn [x]
    (loop [result "" c (map parse-int (re-seq #"\d+" x)) ]
      (if (empty? c)
        (apply str (drop 1 result))
        (recur (if (some #(= (first c) %) (map #(* % %) (range 2 (first c)) )) (str result "," (first c)) result) (rest c))))))

(defcheck solution-f3b2d843
  (fn [s]
    (clojure.string/join "," (filter
                               #(= (double (parse-int %)) (Math/pow (Math/sqrt (double (parse-int %))) 2))
                               (clojure.string/split s #"[,]")))))

(defcheck solution-f4323b12
  (fn [n]
    (apply str
      (interpose ","
        (map #(.toString %)
          (filter #(= % (* (int (Math/sqrt %))
                          (int (Math/sqrt %))))
            (map #(parse-int %)
              (re-seq #"\d+" n))))))))

(defcheck solution-f4993dd1
  (fn [words]
    (let [elem? (fn [x xs] (not (nil? (some #{x} xs))))
          nums (->> (clojure.string/split words #",")
                 (map #(integer %)))
          m (apply max nums)
          p (take-while #(<= % m) (map #(* % %) (drop 2 (range))))
          pass (filter #(elem? % p) nums)]
      (clojure.string/join "," (map str pass)))))

(defcheck solution-f4a52f19
  #(clojure.string/join ","
     (filter
       (fn [n] (= n (* (int (Math/sqrt n)) (int (Math/sqrt n)))))
       (map parse-int (re-seq #"\d+" %)))))

(defcheck solution-f505caab
  (fn [s]
    (let [isSquare (fn [x]
                     (let [n (parse-int x)
                           sqrt (Math/sqrt n)
                           floored (int (Math/floor sqrt))]
                       (= n (* floored floored))))]
      (->> s
        (re-seq #"\d+")
        (filter isSquare)
        (clojure.string/join ",")))))

(defcheck solution-f518608b
  (fn [x] (let [nums (map #(parse-int %) (clojure.string/split x #",")), psqs (for [i (range 100)] (* i i))] (clojure.string/join "," (for [n nums :when (some #{n} psqs)] n )) )))

(defcheck solution-f5a63e2b
  (fn [numbers-string]
    (let [parsed-numbers (->> numbers-string (re-seq #"\d+") (map parse-int) )
          all-perfect-squares (fn [] (map #(* % %) (iterate inc 0)))
          less-than-max-number (fn [x] (<= x (apply max parsed-numbers)))
          perfect-square? (->> (all-perfect-squares) (take-while less-than-max-number) set)
          perfect-squares (filter perfect-square? parsed-numbers)]
      (->> (interpose "," perfect-squares) (apply str)) )
    ))

(defcheck solution-f5f917cd
  (fn squares [s]
    (let [l (flatten (map #(parse-int %) (clojure.string/split s #"," )))
          sq (filter #(= (float (int (Math/sqrt %))) (Math/sqrt %)) l)]
      (str (clojure.string/join "," sq)))))

(defcheck solution-f60ce5b6
  (fn [xs] (clojure.string/join "," (filter #(let [v (Math/sqrt (parse-int %))] (= (Math/floor v) v)) (clojure.string/split xs #",")))))

(defcheck solution-f64bc116
  (fn [in]
    (let [int? #(= (double %) (double (Math/round (double %))))]
      (->> (clojure.string/split in #",")
        (map parse-int)
        (filter #(int? (Math/sqrt %)))
        (clojure.string/join ",")))))

(defcheck solution-f6901ccc
  #(clojure.string/join ","  (filter (fn [n] (let [w (Math/round (Math/sqrt n))]
                                               (= n (* w w))))
                               (map (fn [c] (parse-int c)) (re-seq #"\d+" % )))))

(defcheck solution-f698d8d3
  #_(fn  [s]
      (let [ps? (fn [n] (let [y (int (Math/sqrt n)) ] (= (* y y) n)))]
        (clojure.string/join ","
          (filter #(ps? (parse-int %))
            (clojure.string/split s #",")))))


  (fn [s]
    (let [ps? (fn [n] (let [y (int (Math/sqrt n)) ] (= (* y y) n)))]
      (->>
        (clojure.string/split s #",")
        (filter #(ps? (parse-int %)))
        (clojure.string/join ",")))))

(defcheck solution-f6fff102
  (fn [s]
    (->> (.split s ",")
      (map #(parse-int %))
      (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))))
      (clojure.string/join ","))))

(defcheck solution-f7203a48
  (fn [s]
    (->>
      (map #(parse-int %) (re-seq #"[^,]+" s))
      (filter #(= (Math/sqrt %) (-> % Math/sqrt int double)))
      (interpose ",")
      (apply str))))

(defcheck solution-f7bac118
  (fn sqrt[seqs]
    (let [x
          (->>
            (map #(parse-int %) (.split seqs ","))
            (filter #(= %
                       (int (Math/pow
                              (int (Math/sqrt %))
                              2)))))]
      (loop[result (.toString (first x))
            x (rest x)]
        (if(empty? x)
          (apply str (map str result))
          (recur
            (concat result "," (.toString (first x)))
            (rest x)))))))

(defcheck solution-f7e2255f
  (fn [l]
    (clojure.string/join ","
      (map str
        (filter
          (fn [x] (let [sqrt-int (int (Math/sqrt x))] (= x (* sqrt-int sqrt-int))))
          (map #(parse-int %) (clojure.string/split l #",")))))))

(defcheck solution-f84f0148
  (fn [s]
    (let [v (map parse-int (clojure.string/split s #","))]
      (clojure.string/join ","
        (filter
          (fn [x]
            (let [s (Math/sqrt x)]
              (= s (Math/floor s))))
          v)))))

(defcheck solution-f8a68df0
  (fn [input]
    (clojure.string/join ","
      (filter
        (fn [n]
          (some #(= n (* % %)) (range 1 n)))
        (map parse-int (clojure.string/split input #","))))))

(defcheck solution-f8d79f
  (fn [ns]
    (clojure.string/join ","
      (filter (fn [sn]
                (let [n (parse-int sn)
                      r (int (Math/sqrt n))]
                  (= n (* r r))))
        (re-seq #"\d+" ns)))))

(defcheck solution-f92ebbaa
  (fn [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter (fn [x]
                (let [r (Math/sqrt x)]
                  (== r (int r)))))
      (interpose ",")
      (apply str))))

(defcheck solution-f9ae3a9a
  (fn [csv]
    (let [nums (map parse-int (clojure.string/split csv #","))
          sqrs (filter #(zero? (mod (Math/sqrt %) 1)) nums)
          sqr-strs (map str sqrs)]
      (clojure.string/join "," sqr-strs))))

(defcheck solution-f9c73213
  (fn [s]
    (let [numbers (map #(parse-int %) (clojure.string/split s #","))
          squares (set (map #(* % %) (range (apply max numbers))))]
      (clojure.string/join "," (filter #(contains? squares %) numbers))
      )))

(defcheck solution-faeea8f
  (fn foo [s]
    (clojure.string/join ","
      (filter #(= % (* (int (Math/sqrt %))
                      (int (Math/sqrt %))))
        (map parse-int (clojure.string/split s #","))))))

(defcheck solution-faf91637
  (fn [s] (reduce
            #(str % "," %2)
            (keep
              (fn [x]
                (when
                 (#(= % (Math/ceil %))
                  (Math/sqrt (integer x)))
                  x)) (.split s ",")))))

(defcheck solution-fb5c606a
  (fn [x] (clojure.string/join "," (filter #(zero? (mod (Math/sqrt %) 1))
                                     (map #(parse-int %)
                                       (.split x ","))))))

(defcheck solution-fb843f6
  (fn [s]
    (let [ints (map parse-int (clojure.string/split s #","))
          squares (filter (fn [x]
                            (let [root (Math/sqrt x)]
                              (== root (Math/round root)))) ints)]
      (clojure.string/join "," squares))))

(defcheck solution-fb9f0d1
  (fn  [s]
    (->> (re-seq #"\d+" s)
      (map #(parse-int %))
      (filter #(= (-> % Math/sqrt int float) (Math/sqrt %)))
      (interpose ",")
      (apply str))))

(defcheck solution-fc63d836
  (fn [s] (clojure.string/join "," (filter #(loop [i 0 r %]
                                              (cond (< 0 r) (recur (inc i) (- r (inc (* 2 i))))
                                                    (= 0 r) true
                                                    (< r 0) false))
                                     (map #(integer %) (clojure.string/split s #"[,]"))))))

(defcheck solution-fc9f8656
  (fn f [str]
    (let [xs (map parse-int (re-seq #"\d+" str))]
      (->> (map #(Math/sqrt %) xs)
        (map int)
        (map #(* % %))
        (#(filter (set %) xs))
        (clojure.string/join ",")))))

(defcheck solution-fceb7e13
  (fn filt-sq [s]
    (letfn [(parse-csstr [sss]
              (map #(parse-int %) (clojure.string/split sss #",")))
            (max-of [xs]
              (reduce (fn [a b] (if (> b a) b a)) xs))]
      (let [nums (parse-csstr s)
            nmax (max-of nums)
            sq-set (set (take-while #(>= nmax %) (map #(* % %) (iterate inc 0))))
            pfq-nums (filter sq-set nums)
            pfq-str (clojure.string/join "," pfq-nums)]
        pfq-str))))

(defcheck solution-fd54534e
  (fn [s]
    (let [nums (map #(integer %) (re-seq #"[0-9]+" s))
          p? (fn [i]
               (loop [s (map #(* % %) (iterate inc 1))]
                 (if (< i (first s))
                   false
                   (if (= i (first s))
                     true
                     (recur (rest s))))))]
      (apply str (interpose "," (filter p? nums))))))

(defcheck solution-fd71159d
  (fn [sqs]
    (letfn [(perfect-square? [n]
              (let [sqrt (Math/sqrt n)]
                (= n (* (int sqrt) (int sqrt)))))]
      (->> sqs
        (re-seq #"\d+")
        (map parse-int)
        (filter perfect-square?)
        (clojure.string/join ",")))))

(defcheck solution-fd7b727f
  (fn [s]
    (letfn [(perfect-square? [n]
              (let [d (Math/sqrt n)
                    i (double (int d))]
                (= d i)))]
      (->> (re-seq #"\d+" s)
        (map #(parse-int %))
        (filter perfect-square?)
        (interpose ",")
        (apply str)))))

(defcheck solution-fdd69147
  (fn [s]
    (->> s (re-seq #"\d+")
      (map #(parse-int %))
      (filter #(let [r (Math/sqrt %)] (== r (int r))))
      (interpose ",")
      (apply str)
      )))

(defcheck solution-fdf4eb6c
  (fn fil-psquare [s]
    (letfn [(psquare [x]
              (let [sx (Math/sqrt x)]
                (= (double x) (* sx sx))))
            (toxs [ss]
              (map
                #(parse-int %)
                (clojure.string/split ss #",")))]
      (clojure.string/join ","
        (filter psquare (toxs s))))))

(defcheck solution-fe58bd20
  (fn [s]
    (clojure.string/join ","
      (filter
        (fn [n] (some #(= n %) (map #(* % %) (range n))))
        ( map parse-int (clojure.string/split s #","))))))

(defcheck solution-fe9851ab
  (fn [intstr]
    (let [l (map #(integer %) (clojure.string/split intstr #","))
          squares (set (take-while (partial >= (apply max l)) (map #(* % %) (range))))] ;; set with all squares until max int
      (->> l (filter squares) (map str) (clojure.string/join ",")))))

(defcheck solution-fe9e5849
  (fn ps [num-list]
    (->> num-list
      (re-seq #"\d+")
      (map #(integer %))
      (filter (fn [n] ((set (map #(* % %) (range n))) n)))
      (map str)
      (clojure.string/join ","))))

(defcheck solution-feabad53
  (fn squares [s]
    (let [[squares-strs] [(map str (filter #(= ((fn [x] (* x x)) (int (Math/sqrt %))) %)
                                     (map #(parse-int %) (.split s ","))))]]
      (str (reduce str (map #(str % ",") (take (dec (count squares-strs)) squares-strs))) (last squares-strs)))))

(defcheck solution-ff13aa1d
  (fn prob-0074
    [s]
    (let [xs   (map #(parse-int %) (re-seq #"[\d]+" s))
          filt #(= % (apply * (replicate 2 (int (Math/sqrt %)))))
          sqrs (filter filt xs) ]

      (apply str (interpose "," sqrs)))))

(defcheck solution-ff612118
  (fn [s] (clojure.string/join ","
            (filter #(zero? (rem (Math/sqrt (parse-int %)) 1))
              (re-seq #"\d+" s)))))

(defcheck solution-fff7cee8
  (fn [s]
    (apply str
      (interpose ","
        (reduce
          #(let [i (parse-int %2)
                 j (int (Math/sqrt i))]
             (if (= (* j j) i) (conj %1 %2) %1))
          [] (.split s ","))))))
