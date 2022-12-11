(ns coal-mine.problem-27
  (:require [coal-mine.checks :refer [defcheck-27] :rename {defcheck-27 defcheck}]
            [clojure.test]))

(defcheck solution-10e7bdcb
  (fn [test]
    (loop [r (seq test)]
      (cond (empty? r) true
            (= 1 (count r)) true
            (not (= (first r) (last r))) false
            :else (recur (rest (drop-last r)))))))

(defcheck solution-116de259
  #(= (apply str (reverse  %)) (apply str %)))

(defcheck solution-138fd416
  (fn isPalindrome [x] (= (seq x) (reverse (seq x)))))

(defcheck solution-13add500
  #(if (empty? %) true (if (not (next %)) true (if (= (first %) (last %)) (recur (next (butlast %))) false))))

(defcheck solution-145b4b72
  #(loop [c %1]
     (cond (<= (count c) 1) true
           (not= (first c) (last c)) false
           :else (recur (drop 1 (drop-last 1 c))))))

(defcheck solution-14770419
  (fn [l] (= (reverse l) (seq l))))

(defcheck solution-14d775ab
  #(= (reverse (into () %)) (into () %)))

(defcheck solution-1529935
  (fn
    palindrome?[lst]
    (if
     (or (empty? lst) (empty? (rest lst)))
      true
      (if
       (not= (first lst) (last lst))
        false
        (palindrome?
          (rest (butlast lst))
          )
        )
      )
    ))

(defcheck solution-16508b75
  #(= (seq %) (reverse (seq %) )))

(defcheck solution-169d7f57
  (fn [s]
    "27. Write a function which returns true if the given sequence is a palindrome."
    (= (seq s) (reverse (seq s)))))

(defcheck solution-169f8506
  (fn [seq]
    (every? (fn [pair]
              (= (first pair) (second pair)))
      (zipmap (reverse seq) seq))))

(defcheck solution-16c9a824
  (fn pal? [coll]
    (cond
      (empty? coll)
      true
      (= (first coll) (last coll))
      (pal? (butlast (rest coll)))
      true
      false)))

(defcheck solution-17229cb1
  (fn [s] (= (into () s) (into () (reverse s)))))

(defcheck solution-17a47854
  (fn [a] (= (seq a) (reverse a))))

(defcheck solution-17b956ef
  (fn [s]
    (= (seq s) (reverse s))))

(defcheck solution-17ba7679
  (fn [coll]
    (= coll (if (string? coll)
              (apply str (reverse coll))
              (reverse coll)))))

(defcheck solution-17e9f3e
  (fn palindrome [sq]
    (or
     (empty? sq)
     (and (= (first sq) (last sq))
          (palindrome (rest (butlast sq)))))))

(defcheck solution-189ec1df
  (fn [xs]
    (let [n (quot (count xs) 2)
          l (take n xs)
          r (reverse (take-last n xs))]
      (= l r))))

(defcheck solution-19355e5c
  (fn [s] (=
            (map-indexed (fn [idx itm] [idx itm]) s)
            (map-indexed (fn [idx itm] [idx itm]) (reverse s))
            )))

(defcheck solution-1ac86df9
  (fn palin? [xs]
    (= (seq xs) (reverse xs))))

(defcheck solution-1b535e46
  (fn pal? [s]
    (loop [string s]
      (cond (empty? string) true
            (not (= (first string) (last string))) false
            :else (recur (butlast (rest string)))))))

(defcheck solution-1c41e2d2
  (fn [x]
    (=  (seq x) (reverse x))))

(defcheck solution-1caab9d0
  (fn palindrom? [input]
    (if (string? input)
      (= (apply str (reverse input)) input)
      (= (reverse input) input))
    ))

(defcheck solution-1ce7ced9
  #(=
     (vec (seq %))
     (vec (reverse (seq %)))))

(defcheck solution-1d021f30
  (fn [x] (= (seq x) (reverse (seq x)))))

(defcheck solution-1d5d1dab
  (fn [xs] (= (seq xs) (reverse (seq xs)))))

(defcheck solution-1df01162
  #(if (string? %) (= (seq %) (reverse (seq %))) (= % (reverse  %))))

(defcheck solution-1dfebb66
  (fn  prob27 [col]
    (let [s (seq col)]
      (= s (reverse col)))))

(defcheck solution-1ef84561
  (fn [x] (= (seq x) (seq (reverse x)))))

(defcheck solution-1f29f039
  (fn [coll] (every? true? (map = (reverse coll) coll))))

(defcheck solution-20ce0dc
  #(cond
     (coll? %) (= % (reverse %))
     (string? %) (= % (apply str (reverse %)))
     ))

(defcheck solution-20df21d8
  #(if (< (count %) 2)
     true
     (if (= (first %) (last %))
       (recur (drop-last (rest %)))
       false)))

(defcheck solution-214cf123
  (fn [x]
    (= (reverse x) (seq x))))

(defcheck solution-21cccb07
  (fn [coll] (= (reverse coll) (seq coll))))

(defcheck solution-22173919
  #(= (apply list %) (reverse %)))

(defcheck solution-223d684d
  #(= (seq %) (rseq (vec %))))

(defcheck solution-229ca5c9
  (fn [coll]
    (= coll
      (if (string? coll)
        (apply str (reverse coll))
        (reverse coll)))))

(defcheck solution-22f52125
  (fn[l] (#(= % (reverse %)) (reduce conj [] l))))

(defcheck solution-22f80815
  #(= (apply str %)  (apply str (reverse %))))

(defcheck solution-238bc1c
  #(if (string? %)
     (= % (apply str (reverse %)))
     (= % (reverse %))))

(defcheck solution-238efc6d
  #(= (reverse (vec %)) (vec %)))

(defcheck solution-23abf29
  #(= (seq %)  (reverse %)))

(defcheck solution-24b8f4c8
  (fn [x]
    (= (seq x) (reverse x))))

(defcheck solution-24c2968d
  (fn [x] ( = (seq x) (reverse (seq x)))))

(defcheck solution-252b810d
  (fn [s] (every? true? (map = s (reverse s)))))

(defcheck solution-260f3ea1
  #(every? true? (map = %1 (reverse %1))))

(defcheck solution-2624c723
  #(= (apply str %) (apply str (reverse %))))

(defcheck solution-26579aa2
  (fn[a-seq]
    (every? (fn[[a b]] (= a b)) ((fn[ss] (zipmap ss (reverse ss))) a-seq))))

(defcheck solution-2672976e
  (fn [s] (= (seq s) (reverse s))))

(defcheck solution-26cff39d
  (fn [word]
    (loop [word word]
      (cond
        (<= (count word) 1) true
        (not= (first word) (last word)) false
        :else (recur (butlast(rest word)))))))

(defcheck solution-271c8c6e
  (fn palindrome?
    [coll]
    (loop [c coll
           result true]
      (let [f (first c)
            l (last c)]
        (if (empty? c)
          result
          (recur (drop 1 (drop-last c))
            (and result  (= f l))))))))

(defcheck solution-2779ebbb
  #(let [d (into [] %)]
     (= d (reverse d))))

(defcheck solution-278cf8d4
  (fn [s]
    (let [seq-s (seq s)]
      (= seq-s (reverse seq-s)))))

(defcheck solution-2844032a
  (fn [s]
    (let [sq (seq s)]
      (cond (or (not sq) (not (next sq))) true
            (not (= (first sq) (last sq))) false
            :else (recur (butlast (rest sq)))))))

(defcheck solution-2948ea9e
  (fn pal [x]
    (= (seq x) (reverse (seq x)))))

(defcheck solution-298ce788
  (fn [s] (= (seq s) (reverse (seq s)))))

(defcheck solution-29b2edd6
  (fn [coll]
    (= (seq coll)
      (reverse (seq coll)))))

(defcheck solution-29ee5b54
  #(= % (if (string? %)
          (clojure.string/reverse %)
          (reverse %))))

(defcheck solution-2ae416c1
  (fn [s]
    (if (string? s)
      (= s (apply str (reverse s)))
      (= s (reverse s)))))

(defcheck solution-2be089eb
  (fn palindrome [coll]
    (cond
      (empty? coll) true
      (= (first coll) (last coll)) (palindrome (rest (butlast coll)))
      :else false)))

(defcheck solution-2c430c33
  #(= (vec %) (reverse(vec %))))

(defcheck solution-2cc09973
  (fn [coll]
    (= (seq coll) (reverse coll))))

(defcheck solution-2cd07b5b
  (fn [sqnce]
    (= (vec sqnce) (vec (reverse sqnce)))))

(defcheck solution-2cec7f02
  (fn [xs]
    (= (seq xs)
      (reverse xs))))

(defcheck solution-2dc5e27c
  #(every? identity (map = % (reverse %))))

(defcheck solution-2e838863
  #(every? (fn [[a b]] (= a b)) (map list (vec %) (reverse %))))

(defcheck solution-3037e0e4
  (fn [coll]
    (let [c (seq coll)]
      (= c (apply conj '() c)))))

(defcheck solution-30ad699e
  (fn [s] (= (reverse (reverse s)) (reverse s))))

(defcheck solution-31043ba1
  (fn palindrome? [s] (= (seq s) (reverse s))))

(defcheck solution-312142d6
  (fn [coll] (= (vec coll) (reverse (vec coll)))))

(defcheck solution-3149c187
  (fn pal [x]
    (if (< (count x) 2)
      true
      (if (= (first x) (last x))
        (pal (rest (butlast x)))
        false
        )
      )
    ))

(defcheck solution-318088dd
  #(= (into [] %) (into () %)))

(defcheck solution-3190cb09
  (fn palindrome?
    [input-seq]
    (let [elems (count input-seq)]
      (every? true? (map #(= (nth input-seq %) (nth input-seq %2)) (range elems) (reverse (range elems)))))))

(defcheck solution-31a32ce7
  (fn [x]
    (if (empty? x)
      true
      (if (not= (first x) (last x))
        false
        (recur (rest (butlast x)))
        )
      )
    ))

(defcheck solution-31b6c651
  (fn [seq]
    (if (string? seq)
      (= (clojure.string/reverse seq) seq)
      (= (reverse seq) seq))))

(defcheck solution-3272b7cf
  (fn [a] (-> a vec reverse (= (vec a)))))

(defcheck solution-337fb629
  ;; extra-task:: ignoring upper/lower-case & whitespaces
  (fn palindrome? [subject]
    (let [post-subj (-> subject
                      ((partial apply str))
                      (clojure.string/replace #"\s+" "")
                      (clojure.string/lower-case))
          processed (->> subject
                      clojure.core/reverse
                      (map str)
                      (remove #(re-seq #"\s" %))
                      (map clojure.string/lower-case)
                      ((partial apply str)))]

      (= post-subj processed))))

(defcheck solution-34809ae4
  (fn [l] (let [l (seq l)] (= l (reverse l)))))

(defcheck solution-3513a24f
  (fn [elems]
    (= (seq elems) (reverse elems))))

(defcheck solution-35c7c91
  (fn [s]
    (= (vec (reverse s))
      (vec s))))

(defcheck solution-369182f6
  (fn palindrome-detector [x]
    (if (string? x)
      (= x (apply str (reverse x)))
      (= x (reverse x)))))

(defcheck solution-36d3dcf4
  #(=
     %
     (if (string? %)
       (apply str (reverse %))
       (reverse %)
       )
     ))

(defcheck solution-37a94d97
  (fn j[x]
    (= (seq x) (reverse x))
    ))

(defcheck solution-3860f2c0
  (fn tn [a]
    (loop [col a]
      (if (nil? (first col))
        true
        (if (not (= (first col) (last col)))
          false
          (recur (-> col (butlast) (rest))))))))

(defcheck solution-3c8b5175
  #(loop [c %] (if (> (count c) 1) (if (not= (first c) (last c)) false (recur (reverse (rest (reverse (rest c)))))) true)))

(defcheck solution-3ca2e867
  #(loop [coll %]
     (if (< (count coll) 2)
       true
       (if (= (first coll) (last coll))
         (recur (-> coll rest reverse rest reverse))
         false
         ))))

(defcheck solution-3d0aff4b
  (fn [coll]
    (let [c (if (string? coll)
              (-> coll seq)
              coll)]
      (= c (reverse c)))))

(defcheck solution-3da144ae
  (fn [s] (let [sqs (seq s)]
            (= (reverse sqs) sqs))
    ))

(defcheck solution-3e0bf88e
  (fn is-palindrome? [x]
    (if (not= (first x) (last x)) false
                                  (if (<= (count x) 2)
                                    true
                                    (is-palindrome? (-> x rest reverse rest))))))

(defcheck solution-3fb6e71d
  #(let [size (count %)
         myvec (vec %)
         mid (quot size 2)]

     (loop [index 0]
       (if (= index mid)
         true
         (if (not=
               (nth myvec index)
               (nth myvec (- size index 1)))
           false
           (recur (inc index)))))))

(defcheck solution-40210269
  #(if (= (apply str %) (apply str (vec (reverse %)))) true false))

(defcheck solution-40a75129
  (fn [s] (let [s (seq s)] (every? true? (map = s (reverse s))))))

(defcheck solution-40e5b9db
  #(let [n  (quot (count %) 2) c1 (take n %) c2 (reverse(take-last n %))] (= c1 c2)))

(defcheck solution-413e6536
  (fn palindrome? [coll]
    (= (reverse coll) (seq coll))))

(defcheck solution-41c63583
  (fn pdet
    [pal]
    (letfn
     [(strip
        [items]
        (drop-last
          1
          (rest items)))]
      #_(println pal)
      (if
       (< (count pal) 2)
        true
        (if
         (=
           (first pal)
           (last pal))
          (pdet (strip pal))
          false)))))

(defcheck solution-41d02b70
  (fn [a] (= (map identity a) (reverse a))))

(defcheck solution-4248429d
  (fn [s]
    (cond
      (< (count s) 2) true
      (not= (first s) (last s)) false
      :default (recur (rest (pop (vec s)))))))

(defcheck solution-42959b62
  (fn [c] (
            loop [coll c]
            (if (<= (count coll) 1)
              true
              (if (= (first coll) (last coll) )
                (recur (drop-last (rest coll)) )
                false
                )
              )
            )))

(defcheck solution-42fb9564
  #(= (if (string? %) (apply str (reverse %)) (reverse %)) %))

(defcheck solution-430c3021
  (fn mydector
    [myseq]

    (loop
     [myrest1 myseq myrest2 (reverse myseq) myres true]
      ( if (nil? (next myrest1) )
        myres
        (recur (next myrest1) (next myrest2) (and myres (= (first myrest1) (first myrest2))))
        )

      )))

(defcheck solution-433ae12d
  #(= (reverse (reverse %)) (reverse %)))

(defcheck solution-445e37d9
  (fn [x] (let [f (if (string? x) clojure.string/reverse reverse)] (= (f x) x))))

(defcheck solution-44a6cc1e
  (fn palindrome? [coll]
    (let [coll# (if (seq? coll) coll (seq coll))]
      (= (reverse coll#) coll#))))

(defcheck solution-45af51b6
  #(cond (string? %) (= % (clojure.string/join (reverse %)))
         :else (= % (reverse %))))

(defcheck solution-46a64633
  #(let [h (quot (count %) 2)] (= (take h %) (reverse (take-last h %)))))

(defcheck solution-478721e0
  (fn [s]
    (if (<= (count s) 2)
      true
      (and (= (first s) (last s)) (recur (rest (drop-last s)))))))

(defcheck solution-47881c6f
  (fn [n]
    (= (vec n) (vec (reverse n)))))

(defcheck solution-478e62df
  (fn [s]
    (= (seq s) (reverse (seq s)))))

(defcheck solution-480cdae6
  (comp (partial apply =)
        (juxt list* reverse)))

(defcheck solution-482a06fd
  (fn pal
    [x]
    (if (string? x)
      (= x (clojure.string/reverse x))
      (= x (reverse x)))))

(defcheck solution-48fa6474
  (fn
    [coll]
    (let
     [revcoll (reverse coll)]
      (= coll
        (if (string? coll)
          (apply str revcoll)
          revcoll)))))

(defcheck solution-4904d3c4
  #(= (reverse %) (map (fn [x] x) %)))

(defcheck solution-4915df9c
  (fn [x] (= (reverse x) (seq x))))

(defcheck solution-493a218e
  #(= (reverse %) (lazy-seq %)))

(defcheck solution-4a7a421d
  #(= (vec %) (vec (reverse %))))

(defcheck solution-4a85ff93
  #(if (string? %)
     (= (clojure.string/reverse %)
       %)
     (= (reverse %) %)))

(defcheck solution-4aff4313
  #(= (reverse %) (reverse (reverse %))))

(defcheck solution-4b9d8f46
  #(loop [poly %]
     (cond (empty? poly) true
           (not= (first poly) (last poly)) false
           :else (recur (rest (drop-last poly))))))

(defcheck solution-4c3dc2a0
  (fn pal [items]
    (if (or (empty? items) (= (count items) 1))
      true
      (if (= (first items) (first (reverse items)))
        (pal (rest (reverse (rest items))))
        false))))

(defcheck solution-4d1ee826
  #(every? (fn [[a b]] (= a b)) (zipmap (reverse %) %)))

(defcheck solution-4d97a6f6
  #(let [ x (if (odd? (count %))
              (/ (- (count %) 1) 2)
              (/ (count %) 2))  ] (= (take x %) (reverse (drop (- (count %) x) %) ))))

(defcheck solution-4e35ca67
  (fn [l] (= (reverse l) (reverse (reverse l)))))

(defcheck solution-4ee1258b
  (fn [xs]
    "haha without reverse"
    (if (< (count xs) 2)
      true
      (and (= (first xs) (last xs)) (recur (rest (drop-last xs)))))))

(defcheck solution-4f1d1869
  (fn [s]
    (if (seq s) (if (= (first s) (last s)) (recur (-> s rest reverse rest reverse)) false)
                true)))

(defcheck solution-4f2e32c1
  (fn [x]
    (= (reverse x)
      (reverse (reverse x)))))

(defcheck solution-51345f8b
  #(let[item (into [] %)] (= item (reverse item))))

(defcheck solution-51821c52
  #(= (reverse %) (vec %)))

(defcheck solution-51e95f0e
  (fn [x] (= (reverse (reverse x)) (reverse x))))

(defcheck solution-52859cea
  (fn [xs]
    (if (string? xs)
      (= xs (apply str (reverse xs)))
      (= xs (reverse xs)))))

(defcheck solution-52f1e351
  (fn [lst] (= (seq lst) (reverse lst))))

(defcheck solution-531531ea
  (comp
   (fn [es]
     (if (nil? es) true
                   (if (= (first es) (last es))
                     (recur (-> es rest butlast))
                     false)))
   seq))

(defcheck solution-53c8e5ae
  (fn [in-seq]
    (= (seq in-seq) (reverse (seq in-seq)))))

(defcheck solution-53f5493
  #(every? true? (for [x (range (quot (count %) 2))]
                   ((fn compareFrontBack [subcol] (= (first subcol) (last subcol)))
                    (drop x (drop-last x %))))))

(defcheck solution-547bb19b
  (fn [s]
    (loop [ss s]
      (if (<= (count ss) 1)
        true
        (if (= (first ss) (last ss))
          (recur (butlast (rest ss)))
          false)
        ))))

(defcheck solution-54ca7b97
  ;#(= (seq %) (reverse (seq %)))

  #(= (seq %) (reverse %)))

(defcheck solution-54d39d23
  #(let [item (seq %)] (= (reverse item) item)))

(defcheck solution-55c077f4
  (fn palin? [items]
    (= (seq items) (reverse (seq items)))))

(defcheck solution-568433dc
  #(loop [x %]
     (cond (< (count x) 2) true
           (not= (first x) (last x)) false
           :else (recur (rest (butlast x))))))

(defcheck solution-576d5c0f
  (fn palidrome? [x]
    (if (< (count x) 3)
      true
      (and
       (= (first x) (last x))
       (palidrome? (rest (drop-last x)))))))

(defcheck solution-57fd49bf
  (fn [xs]
    (cond
      (< (count xs) 2) true
      (not= (first xs) (last xs)) false
      :else (recur (drop 1 (drop-last xs))))))

(defcheck solution-58077784
  #((let [c (reverse %)]
      (fn []
        (= (-> % reverse reverse) c)))))

(defcheck solution-58766d49
  #(let [a (apply list %)] (= a (reverse a))))

(defcheck solution-59d2d652
  #(= (vec %) (reverse (vec %))))

(defcheck solution-5aa75dcd
  #(nil? (some (partial = false) (map = % (reverse %)))))

(defcheck solution-5ac9f092
  #(= (clojure.string/join (reverse %)) (clojure.string/join %)))

(defcheck solution-5b3c6a0f
  (fn [original] (= (seq original) (reverse (seq original)))))

(defcheck solution-5bfa985d
  (fn pal [l]
    (if (< (count l) 2)
      true
      (if (= (first l) (last l))
        (pal (rest (drop-last l)))
        false
        )
      )
    ))

(defcheck solution-5d2317fe
  #(= (reduce str %) (reduce str (reverse %))))

(defcheck solution-5d84aecc
  (fn [in]
    (let [l (seq in)]
      (= l (reverse l)))))

(defcheck solution-5dadf886
  #(= (reverse (subvec (into [] %)  (int (Math/ceil (/ (count %) 2))) (count %)))  (subvec (into [] %) 0 (int (Math/floor (/ (count %) 2))  ))))

(defcheck solution-5e1418dd
  (fn [s] (= (seq s) (->> s seq reverse))))

(defcheck solution-5eb03fa1
  #(= (reverse (seq %)) (seq %)))

(defcheck solution-5ef8cd3f
  (fn [x]
    (= (seq x) (reverse x))))

(defcheck solution-60e829b0
  (fn
    [elems]
    (if (string? elems)
      (= (clojure.string/reverse elems) elems)
      (= (reverse elems) elems))
    ))

(defcheck solution-61cad7f2
  #(let [x %] (if (=(apply str (reverse x)) (apply str x)) true false)))

(defcheck solution-62fd0e62
  (fn palin? [e] (= e
                   (if (string? e) (clojure.string/join (reverse e))
                                   (reverse e)))))

(defcheck solution-639c157a
  #(if (empty? %) true
                  (if (= (first %) (last %)) (recur (butlast (rest %))) false)))

(defcheck solution-63bce2d2
  (fn palindrome [lst]
    (if (or (empty? lst) (= 1 (count lst)))
      true
      (if (= (first lst) (last lst))
        (-> lst rest drop-last palindrome)
        false))))

(defcheck solution-63daa370
  (fn palindrome [x]
    (if (empty? x)
      true
      (let [y (seq x)]
        (let [f (first y)]
          (let [l (last y)]
            (let [n (palindrome (drop-last (rest y)))]
              (= true (= true (= f l)) n)))
          )
        )
      )
    ))

(defcheck solution-6464abfa
  #(let [s (seq %)] (= s (reverse s))))

(defcheck solution-64e4d7e9
  #(= (seq %1) (reverse %1)))

(defcheck solution-65294ed8
  (fn [xs]
    (every? #(true? %) (map #(= %1 %2) xs (reverse xs)))))

(defcheck solution-65f48af0
  (fn p [x] (or (<= (count x) 1)
                (and (= (first x) (last x))
                     (p (drop-last (rest x)))))))

(defcheck solution-66da749e
  (fn [input] (let [rev (if (string? input) (apply str (reverse input))
                                                              (reverse input))]
                (= rev input))))

(defcheck solution-67bd8a2c
  (fn [seq] (let [mid (Math/ceil (/ (count seq) 2))]
              (= (take mid seq) (reverse (take-last mid seq))))))

(defcheck solution-68ac0d2b
  (fn [input] (= (reverse input) (seq input))))

(defcheck solution-68f8755b
  (fn [xs] (= (seq xs) (reverse xs))))

(defcheck solution-69196cbc
  (fn [s] (let [s (seq s)] (= s (reverse s)))))

(defcheck solution-692b77bf
  #(= (first %) (last %)))

(defcheck solution-698e204e
  (fn [xs]
    (= (seq xs) (reverse xs))))

(defcheck solution-69d5d6f5
  (fn palindrome? [item]
    (= (reverse (seq item) ) (seq item))

    ))

(defcheck solution-6a012a25
  (fn [coll]
    (let* [x (vec coll)
           l (count x)
           m (quot l 2)
           h (subvec x 0 m)
           t (subvec x (if (= 1 (rem l 2)) (inc m) m))]
      (= h (reverse t)))))

(defcheck solution-6aa32556
  #(let [v (vec %)] (= v (reverse v))))

(defcheck solution-6c12bf29
  (fn [x]
    (= (reverse (vec x)) (vec x))))

(defcheck solution-6d37a498
  (fn [s]
    (let [ss (seq s)]
      (= ss (reverse ss)))))

(defcheck solution-6d66b030
  #(= (seq %) (reverse (seq %))))

(defcheck solution-6e058f5
  (fn palindrome? [lst]
    (if (> 2 (count lst))
      true
      (and (= (first lst) (last lst)) (palindrome? (rest (butlast lst)))))))

(defcheck solution-6f744878
  #(= (seq %) (into () %)))

(defcheck solution-6f8483d5
  #((fn [x] (if (> 2 (count x)) true (if (= (last x) (first x)) (recur (subvec x 1 (dec (count x)))) false))), (vec %)))

(defcheck solution-705d330f
  (fn [xs]
    (cond
      (empty? xs) true
      (= (first xs) (last xs)) (recur (butlast (rest xs)))
      :else false)))

(defcheck solution-709a8ed4
  #(= (conj (rest %) (first %)) (reverse %)))

(defcheck solution-71117d0e
  (fn palindrome? [coll]
    (let [rseq (reverse coll)]
      (= rseq (seq coll)))))

(defcheck solution-71b24ea7
  (fn [palindrome] (= (seq palindrome) (reverse (seq palindrome)))))

(defcheck solution-72d5bc44
  #(loop [s (seq %) r (reverse (seq %))]
     (cond (empty? s) true
           (not= (first s) (first r)) false
           :else (recur (next s) (next r)))))

(defcheck solution-73681159
  (fn pal [coll]
    (or (< (count coll) 2)
        (and (= (first coll) (last coll))
             (pal (next (butlast coll)))))))

(defcheck solution-7384c7c
  (fn [s]
    (let [s (vec s)]
      (= (reverse s) s))))

(defcheck solution-73c0136e
  #(or (= % (reverse %)) (= % (reduce str (reverse %)))))

(defcheck solution-748a2d9
  #(let [a (seq %) b (reverse a)]
     (= a b)))

(defcheck solution-74a2ee73
  (fn [lst]
    (loop [lst1 lst lst2 (reverse lst)]
      (cond (and (empty? lst1) (empty? lst2))
            true
            (= (first lst1) (first lst2))
            (recur (rest lst1) (rest lst2))
            true
            false))))

(defcheck solution-74cd1785
  (fn [coll]
    (if (< (count coll) 1)
      true
      (if (= (first coll) (last coll))
        (recur (rest (take (dec (count coll)) coll)))
        false))))

(defcheck solution-74cd5e5f
  (fn [sq]
    (cond
      (or (empty? sq)
          (empty? (rest sq))) true
      (not= (first sq) (last sq)) false
      :else (recur (butlast (drop 1 sq))))))

(defcheck solution-76ff6c8c
  (fn [sq]
    (let [sq (seq sq)]
      (= sq (reverse sq)))))

(defcheck solution-776344e4
  (fn [col]
    (= (reverse col) (vec col))))

(defcheck solution-7812b67c
  (fn [v]
    (let [s (seq v)]
      (= s (reverse s)))))

(defcheck solution-78b9fa80
  #( = (seq %) (reverse %)))

(defcheck solution-79096f6e
  #(= (reverse %1) (seq %1)))

(defcheck solution-792e24a9
  #(let [coll (seq %)]
     (= coll
       ((fn [coll]
          (loop [o (seq coll)
                 c (count coll)
                 n 0
                 l (empty coll)]
            (if (== n c)
              l
              (recur o c (inc n) (cons (nth o n) l))))) coll))))

(defcheck solution-7b6c759b
  (fn [c] (= (reverse (seq c)) (seq c))))

(defcheck solution-7b752d47
  #(if (= (seq %) (reverse %)) true false))

(defcheck solution-7c1f77a4
  (fn f
    [c]
    (if (< (count c) 2)
      true
      (and
       (= (first c) (last c))
       (f (butlast (drop 1 c)))))))

(defcheck solution-7c236a8d
  #( if (string? %)
     (= % (clojure.string/reverse %))
     (= % (reverse %))))

(defcheck solution-7c84d1ed
  #(if (string? %)
     (= % (apply str (reverse (seq %))))
     (= % (reverse %))))

(defcheck solution-7ce668fd
  #(
     if(< (count %1)3)
     true
     (if(=(first %1)(last %1))
       (recur (-> %1 rest butlast))
       false)
     ))

(defcheck solution-7cfdb62a
  (fn [x]
    (if (string? x)
      (= x (apply str (reverse x) ))
      (= x (reverse x)) )))

(defcheck solution-7d028d34
  #(reduce (fn [a v] (and a (= (first v) (second v)))) (map list % (reverse %))))

(defcheck solution-7d90e471
  #(if(string? %)
     (= % (apply str (reverse %)))
     (= % (reverse %))))

(defcheck solution-7dcf3de8
  (fn isPal [s]
    (if (empty? s)
      true
      (if (= 1 (count s))
        true
        (if (= (first s) (last s))
          (isPal (drop-last (rest s)))
          false
          )
        )
      )))

(defcheck solution-7e1eb7a6
  (fn [s] (= (seq s) (reverse (seq s) ))))

(defcheck solution-7eae4fb6
  #(nil?(some false?(map = %(reverse %)))))

(defcheck solution-7eb849c9
  (fn pal? [s] (if (or (empty? s) (= 1 (count s))) true (and (= (first s) (last s)) (pal? (butlast (rest s)))))))

(defcheck solution-7fca21e4
  (fn ([seq1] (reduce #(and % %2) (map #(= (first %) (last %)) (map vector seq1 (reverse seq1)))))))

(defcheck solution-80722f39
  (fn
    [pos-seq]
    (let [a-seq (seq pos-seq)]
      (= a-seq (reverse a-seq)))))

(defcheck solution-80c494fb
  (fn [l]
    (if (nil? (next l)) true
                        (if (= (first l) (last l)) (recur  (drop-last (drop 1 l))) false))))

(defcheck solution-80f3dafc
  (fn [s]
    (let [orig-len (count s)
          half-len (quot orig-len 2)
          split' (fn [s i first-half]
                   (if (< (count first-half) half-len)
                     (recur (next s) (inc i) (conj first-half (first s)))
                     [first-half
                      (if (odd? orig-len) (next s) s)]))
          [half1 half2] (split' s 0 [])]
      (= half1 (reverse half2)))))

(defcheck solution-811180d0
  #(= (reverse %) (if (seq? %) % (seq %))))

(defcheck solution-84dc322a
  #(let [ cmpnum (quot (count %) 2)
         [l r] (split-at cmpnum %)]
     (= (reverse l) (take-last cmpnum r))))

(defcheck solution-861c333f
  (let [palindrome
        (fn [coll k]
          (if (> (* 2 k) (count coll))
            true
            (if (not= (coll k) (coll (- (count coll) k 1)))
              false
              (recur coll (inc k))
              )
            )
          )]
    (fn [coll] (palindrome (vec coll) 0))
    ))

(defcheck solution-862e35d0
  #(if (empty? %)
     true
     (if (= (last %) (first %))
       (recur (rest (butlast %)))
       false
       )
     ))

(defcheck solution-86a92955
  (fn palindrome [l]
    (= (seq l) (reverse l))))

(defcheck solution-87e2889e
  (fn palindrom [ls] (= (seq (reverse ls)) (seq ls)) ))

(defcheck solution-88640d9b
  (fn [x]
    (let [rev (reverse x) max (count x)]
      (loop [i 0 is-pal true]
        (if (= i max)
          (true? is-pal)
          (recur (inc i) (and is-pal (= (nth rev i) (nth x i)))))))))

(defcheck solution-896ed791
  (fn palin
    ([lst]
     (if (empty? lst)
       true
       (and (= (first lst) (last lst)) (palin (rest (reverse (rest lst)))))))))

(defcheck solution-8a6051e4
  #(= (seq %)(reverse (seq %))))

(defcheck solution-8ac39a10
  (fn [x] (= (seq x) (reverse x))))

(defcheck solution-8b3fab59
  (fn [xs] (= (into [] xs)  (into [] (reverse xs)))))

(defcheck solution-8b44e912
  (fn [x]
    (let [y (if (string? x) (apply str (reverse x)) (reverse x))]
      (= x y))))

(defcheck solution-8b7b8d57
  #(= (vec %1) (reverse %1)))

(defcheck solution-8bfc6571
  #(= (vec (reverse %)) (vec %)))

(defcheck solution-8dbe97f6
  (fn [s] (if (string? s) (= s (clojure.string/reverse s)) (= s (reverse s)))))

(defcheck solution-8e56a962
  #(= (reverse %) (seq %)))

(defcheck solution-8e96bcc2
  (fn [col0]
    (let [col (seq col0)]
      (loop [start 0 end (- (count col) 1)]
        (cond
          (false? (= (nth col start) (nth col end))) false
          (and (= (nth col start) (nth col end))
               (<= (- end start) 1)  )            true
          :else (recur (inc start) (dec end))))
      )))

(defcheck solution-8ebb498f
  (fn [coll] (= (seq coll) (reverse coll))))

(defcheck solution-8fd0231d
  ;(fn [collection] (= (vec collection) (reverse collection)))

  #(= (vec %) (reverse %)))

(defcheck solution-90255da2
  (fn [coll]
    (every? #(apply = %) (map vector coll (reverse coll)))))

(defcheck solution-9046629a
  (fn palindrome[l]
    (every?
      true?
      (map #(= (first %1) (second %1))
        (map vector
          (take (/ (count l) 2) l)
          (take (/ (count l) 2) (reverse l)))))))

(defcheck solution-90cbefa0
  (fn [l]
    (let [l1 (seq l)]
      (= l1 (reverse l1)))))

(defcheck solution-913ff9dc
  (fn [l] (every? identity (map (fn [a b] (= a b)) l (reverse l)))))

(defcheck solution-925fc010
  #(let [n (quot (count %) 2)]
     (= (take n %) (take n (reverse %)))))

(defcheck solution-929d424d
  (fn [coll]
    (let [rev (if (string? coll)
                clojure.string/reverse
                reverse)]
      (= coll (rev coll)))))

(defcheck solution-92d8ca8b
  #(or (<= (count %) 1) (and (= (first %) (last %)) (recur (butlast (next %))))))

(defcheck solution-92e85ba7
  #(= (reverse (into () %)) (reverse %)))

(defcheck solution-92ffdc4b
  (fn
    [x]
    (if (= (type x) (type "str"))
      (= x (apply str (reverse x)))
      (= x (reverse x)))))

(defcheck solution-932478ed
  (fn palindrome?
    [coll]
    (if (seq coll)
      (and (= (first coll) (last coll))
           (palindrome? (butlast (next coll))))
      true)))

(defcheck solution-9350c131
  (fn pal [xs]
    (cond
      (< (count xs) 2) true
      :else
      (cond
        (= (first xs) (last xs)) (pal (drop 1 (butlast xs)))
        :else                    false
        )
      )
    ))

(defcheck solution-941197d
  (fn
    [coll]
    (every? #(= (first %) (second %)) (map #(list % %2) coll (reverse coll)))))

(defcheck solution-944a476a
  (fn [coll]
    (let [coll-seq (seq coll)]
      (= coll-seq (reverse coll-seq)))))

(defcheck solution-946ac0a1
  (fn [s]
    (cond
      (empty? s) true
      (= (last s) (first s)) true
      :else false
      )
    ))

(defcheck solution-94e230a3
  #(letfn [(palindrome? [xs]
             (if (empty? xs)
               true
               (let [f (first xs)
                     l (last xs)]
                 (if (= f l)
                   (palindrome? (rest (drop-last xs)))
                   false))))]
     (palindrome? %)))

(defcheck solution-9508f5b2
  (fn [x]
    ( = (vec x) (reverse (vec x)))))

(defcheck solution-965dc3d6
  (fn [c] (let [coll (into [] c)] (= (reverse coll) coll))))

(defcheck solution-96bbfe3c
  (fn palindrome? [coll]
    (cond
      (empty? coll) true
      (= (first coll) (last coll)) (palindrome? (rest (butlast coll)))
      :else false)))

(defcheck solution-96e6adc3
  (fn [alist]
    (cond
      (not= (first alist) (last alist)) false
      (or (empty? alist) (= 1 (count alist))) true
      :else (recur (butlast (drop 1 alist))))))

(defcheck solution-974be96
  (fn [l] (let [s (seq l)] (= (reverse s) s))))

(defcheck solution-98aae6f0
  (fn p [s]
    (if (< (count s) 2)
      true
      (if-not (= (first s) (last s))
        false
        (p (butlast (rest s)))
        )
      )
    ))

(defcheck solution-992f69ef
  (fn [lst]
    (loop [l (seq lst)]
      (cond
        (= (last l) (first l))
        (if (< 1 (count l))
          (recur (drop 1 (butlast l)))
          true)
        :else false))))

(defcheck solution-995e154d
  (fn palindrome [x]
    (if (= (first x) (last x))
      (if (<= (count x) 2)
        true
        (palindrome (rest (drop-last x)))
        )
      false
      )
    ))

(defcheck solution-9a39719f
  (fn [x]
    (loop [l x]
      (if (or (= (count l) 0) (= (count l) 1))
        true
        (if (= (first x) (last x))
          (recur (rest (butlast l)))
          false
          )
        )
      )
    ))

(defcheck solution-9ab9a80f
  (fn palindrome? [s]
    (if (empty? s)
      true
      (and (= (first s) (last s))
           (palindrome? (butlast (rest s)))))))

(defcheck solution-9b05611d
  (fn [input]
    (loop [dwindlelist input newlist nil]
      (if (empty? dwindlelist)
        (= (apply str input)
          (apply str newlist))
        (recur (rest dwindlelist)
          (conj newlist (first dwindlelist)))))))

(defcheck solution-9bacc4f8
  (fn [s]
    (let [len (- (count s) 1) lim (int (/ len 2)) v (vec s)]
      (loop [pointer 0]
        (cond (> pointer lim) true
              (not= (get v pointer) (get v (- len pointer))) false
              :else (recur (+ pointer 1)))))))

(defcheck solution-9c94be75
  #(= (into () %) (into [] %)))

(defcheck solution-9cd3e5b2
  #(let [coll (seq %)
         rev (reverse coll)]
     (= rev coll)))

(defcheck solution-9ced29e9
  (fn [s]
    (let [len (quot (count s) 2)
          front (take len s)
          rear (take len (reverse s))]
      (= front rear))))

(defcheck solution-9d57cbb8
  (fn [x] (reduce #(and %1 %2) true (map = x (reverse x)))))

(defcheck solution-9d9af65b
  (fn is-pali?
    [sq]
    (if (= (reverse sq) (seq sq)) true false)))

(defcheck solution-9eb08b59
  (fn [coll]
    (if coll
      (if (= (first coll) (last coll))
        (recur (next (drop-last coll)))
        false)
      true)))

(defcheck solution-9ebcc2de
  (fn [x] (reduce (fn [x y] (and x y)) (map #(= %1 %2) x (reverse x)))))

(defcheck solution-a051c7cd
  (fn [col](if (< (count col) 2) true (if (= (first col)(last col)) (recur(rest(butlast col))) false))))

(defcheck solution-a0a6a07e
  (fn [x]
    (= (clojure.string/join x)
      (clojure.string/join (reverse x)))))

(defcheck solution-a0dedfa0
  (fn palindrome?
    [sequence]
    (= (apply str sequence) (apply str (reverse sequence)))))

(defcheck solution-a1204ea2
  (fn [l]
    (reduce #(and %1 %2)
      (let [s (count l)]
        (for [x (range 0 (/ s 2))]
          (= (nth l x) (nth l (- s (inc x)))))))))

(defcheck solution-a2332d25
  (fn [a] (let [a (seq a)] (= a (reverse a)))))

(defcheck solution-a5701219
  #( = (reverse %) (reverse (reverse %))))

(defcheck solution-a5e274b2
  #(= (map identity %) (reverse %)))

(defcheck solution-a663ae74
  (fn [c] (= (seq c) (reverse c))))

(defcheck solution-a6d4e2ae
  #(let [check
         (if (string? %)
           clojure.string/reverse
           reverse)]
     (= (check %) %)))

(defcheck solution-a7273ced
  (fn paildrome? [arg]
    (let [l (count arg), s (quot l 2)]
      (= (take s arg) (reverse (take-last s arg))))))

(defcheck solution-a80afd51
  (fn [coll]
    (let [s-coll (seq coll)]
      (= s-coll (reverse s-coll)))))

(defcheck solution-a8d0b412
  #(= (seq (reverse %)) (seq %)))

(defcheck solution-a8dadf0e
  (fn [coll]
    (let [pairs (map vector coll (reverse coll))]
      (every? #(apply = %) pairs))))

(defcheck solution-a8e1cef4
  (fn [l] (if (< (count l) 2) true (if (= (first l) (last l)) (recur (drop-last 1 (drop 1 l))) false))))

(defcheck solution-a96c5ccb
  #(= (into () %) (into () (reverse %))))

(defcheck solution-aa269c95
  #(= (clojure.string/join %) (clojure.string/join (reverse %))))

(defcheck solution-aafece28
  (fn palin[x]
    (if (< (count x) 2) true
                        (and (= (first x) (last x)) (palin (rest (butlast x))))
                        )))

(defcheck solution-aaffd555
  #( loop [x %1 y %1 reslt true] (if (empty? x) reslt (recur (rest x) (butlast y) (and reslt (= (first x)(last y)))))))

(defcheck solution-ab964542
  (fn [s]
    (= (seq s) (reverse s))))

(defcheck solution-acb78739
  (fn [l]
    (= (reverse (seq l)) (seq l))))

(defcheck solution-ae207158
  #(cond (or (empty? %) (= (count %) 1)) true (not (= (first %) (last %))) false :else (recur ((comp drop-last rest) %))))

(defcheck solution-b00dca
  (fn pal [liste]
    (= (reverse liste) (reverse (reverse liste)))
    ))

(defcheck solution-b073a3dd
  (fn [x]
    (if (string? x) (= x (apply str (reverse x)))
                    (= x (reverse x))
                    )
    ))

(defcheck solution-b0aaf42b
  #(let [xs (seq %)] (= xs (reverse xs))))

(defcheck solution-b0c01044
  (fn ! [x]
    (if (= 1 (count x))
      true
      (do (
            if (= (first x) (last x))
            (do (
                  if (= 2 (count x))
                  true
                  (! (next (butlast x)))
                  ))
            false
            )
          )
      )
    ))

(defcheck solution-b0fabfe4
  (fn [x]
    (= (reverse x) (reverse (reverse x)))))

(defcheck solution-b134c757
  #(= (seq %1) (reverse (seq %1))))

(defcheck solution-b24ff136
  #(every? true? (map = (reverse %) %)))

(defcheck solution-b38188cd
  #(every? (fn [[x y]] (= x y))
     (map (fn [x y] [x y])
       %
       (reverse %))))

(defcheck solution-b410a4cf
  (fn [x]
    (= (reverse x) (seq x))))

(defcheck solution-b41d4165
  #(= (list* %) (reverse %)))

(defcheck solution-b45dd86d
  (fn [s]
    (if (<= (count s) 1)
      true (and
            (= (first s) (last s))
            (recur (drop 1 (drop-last s)))
            )
      )
    ))

(defcheck solution-b50b8bf2
  (fn [lista]
    (= (seq lista) (reverse lista))))

(defcheck solution-b52e1211
  (fn [sq] (= (reverse sq) (reverse (reverse sq)))))

(defcheck solution-b82723a5
  (fn palindrome? [x] (= (seq x) (reverse x))))

(defcheck solution-b96bbf5d
  (fn palindrome? [s]
    (if (<= (count s) 1)
      true
      (and (= (first s) (last s)) (palindrome? (->> s (drop 1) (drop-last 1)))))))

(defcheck solution-b9763d26
  (fn [xs]
    ((fn eq [xs ys]
       (if (or (empty? xs) (empty? ys))
         (and (empty? xs) (empty? ys))
         (and (= (first xs) (first ys))
              (eq (rest xs) (rest ys)))))
     xs (reverse xs))))

(defcheck solution-b97f55d3
  (fn [seq]
    (if (string? seq)
      (= seq (apply str (reverse seq)))
      (= seq (reverse seq))
      )
    ))

(defcheck solution-b9e5bd2a
  (fn [coll]
    (= (reverse coll) (seq coll))))

(defcheck solution-bad71552
  (fn [x]
    (= (apply str (reverse x)) (apply str x))))

(defcheck solution-bb58d5f
  #(= (into [] %) (reverse %)))

(defcheck solution-bbefa837
  (fn [x](= (seq x) (reverse x))))

(defcheck solution-bc03e736
  (fn [l]
    (= (apply vector l) (reverse (apply vector l)))))

(defcheck solution-bc43e9d
  #(let [p (into '() %)
         rev (reverse p)]
     (= p rev)))

(defcheck solution-bc8e5cbd
  #(if (string? %)
     (= % (apply str (reverse %) ) )
     (= % (reverse %) ) ))

(defcheck solution-bc9ea4ad
  (fn [pal]
    (= (reverse pal) (reverse (reverse pal)))))

(defcheck solution-bd6bdd8c
  (fn [s]
    (cond
      (string? s)
      (= s (apply str (reverse s)))
      :else
      (= s (reverse s)))))

(defcheck solution-be3101ba
  #(every? (fn [[a b]] (= a b))
     (map vector % (reverse %))))

(defcheck solution-bf7f580b
  #(or (= % (reverse %)) (= % (apply str (reverse %)))))

(defcheck solution-c211edbb
  #(loop [s %]
     (if (empty? s)
       true
       (if (= (first s) (last s))
         (recur (rest (reverse (rest s))))
         false))))

(defcheck solution-c283fa94
  #(= (reverse %) (apply list %)))

(defcheck solution-c2d2676c
  #(= (concat %) (reverse %)))

(defcheck solution-c2e2c9f4
  #(loop [col % i 0]
     (if (> i (/ (count col) 2))
       true
       (do
         (if (= (nth col i)
               (nth col (- (count col) i 1))
               )
           (recur col (inc i))
           false
           )
         )
       )
     ))

(defcheck solution-c302fce1
  (fn [seq] (= (reverse (reverse seq)) (reverse seq))))

(defcheck solution-c37e7fbb
  #(let [r (into '() %)] (= r (into '() r))))

(defcheck solution-c472934b
  (fn [in] (= (seq in) (reverse in))))

(defcheck solution-c5398934
  (fn [coll]
    (= (seq coll) (reverse coll))


    ))

(defcheck solution-c71c5c4c
  (fn palindrome? [coll]
    (let [coll (seq coll)]
      (= coll (reverse coll)))))

(defcheck solution-c73999cf
  #(let [lst (apply list %)] (= lst (reverse lst))))

(defcheck solution-c74bf3ca
  (fn [s]
    (let [pivot (-> s count (#(/ % 2)) int)
          rhs (take pivot s)
          lhs (take pivot (reverse s))]
      (= rhs lhs))))

(defcheck solution-c7e32bd6
  (fn [lstr] (= (apply str lstr) (apply str (reverse lstr)))))

(defcheck solution-c7e3e92c
  (fn [s]
    (let [s (vec s)]
      (if (or (empty? s) (= 1 (count s)))
        true
        (if (= (first s) (last s))
          (recur (subvec s 1 (dec (count s))))
          false)))))

(defcheck solution-c8362a0e
  (fn hey [x]
    (if (empty? x)
      true
      (if (not (= (first x) (last x)))
        false
        (hey (drop-last (rest x)))))))

(defcheck solution-c9dc256c
  (fn [a] (= a ((if (string? a) clojure.string/reverse reverse) a))))

(defcheck solution-caacf28d
  #(= %
     (if (string? %)
       (apply str (reverse %) )
       (reverse %)
       )
     ))

(defcheck solution-cadf0253
  (fn [p] (= (into () p) (seq p))))

(defcheck solution-cbd00cba
  (fn [v]
    (let [r (reverse v)]
      (loop [n v
             r r]
        (cond
          (empty? n) true
          (= (first n) (first r)) (recur (rest n) (rest r))
          :else false)))))

(defcheck solution-cc42bd31
  #(= (vec %) (reverse %)))

(defcheck solution-cc6b7f50
  (fn [x]
    (let [l (/ (count x) 2)
          r (take l (rseq (vec x)))
          s (take l (seq x))]
      (= r s))))

(defcheck solution-ccaaa479
  (fn [coll] (every? identity
               (map #(= %1 %2)
                 coll
                 (reverse coll)))))

(defcheck solution-ccc0d2e4
  (fn [in]
    (let [v  (seq in), rv (reverse v)]
      (=  v  rv)
      )

    ))

(defcheck solution-cd14bc79
  #(= (seq %) (reduce conj () %)))

(defcheck solution-cd43b74f
  (fn [a] (= (apply list a) (reverse a))))

(defcheck solution-cdc78f91
  (fn is_palindrome [s]
    (loop [i 0 j (dec (count s))]
      (if (>= i j)
        true
        (if (= (nth s i) (nth s j))
          (recur (inc i) (dec j))
          false)))))

(defcheck solution-cdd5206e
  (fn[palindrome]
    (= (seq palindrome) (reverse palindrome))
    ))

(defcheck solution-cdf8aa55
  #(= (vec %) (rseq (vec %))))

(defcheck solution-cfa584f3
  #(= (seq %) (reverse %)))

(defcheck solution-cfbdfb1
  (fn paln? [s] (= (apply str s) (apply str (reverse s)))))

(defcheck solution-d04c4837
  #(= (reverse (into [] %)) (into [] %)))

(defcheck solution-d0b56c1
  ;;(fn [[& s]] (= s (reverse s)))
  #(= (seq %) (reverse %)))

(defcheck solution-d158e192
  (fn palindrome? [s]
    (= (seq s) (reverse s))))

(defcheck solution-d1635672
  (comp #(= % (rseq %)) vec))

(defcheck solution-d1652834
  (fn [coll] (let [s (seq coll)] (= s (reverse s)))))

(defcheck solution-d1a518b6
  #(= (into () %) (seq %)))

(defcheck solution-d1a92a9b
  (fn [s]
    (= (reverse s) (seq s))))

(defcheck solution-d1de9a94
  ;;(comp (partial apply =) (juxt seq reverse) )
  #(= (seq %) (reverse %)))

(defcheck solution-d2989845
  (fn [xs]
    (let [revfn (if (string? xs)
                  clojure.string/reverse
                  reverse)]
      (= xs (revfn xs)))))

(defcheck solution-d2f6e10e
  (fn [coll]
    (= (seq coll) (reverse coll))))

(defcheck solution-d44d3cdb
  (fn [[fst & rst :as seq]]
    (and (not (empty? seq))
         (or (some #(= % fst) rst)
             (recur rst)))))

(defcheck solution-d4d5c648
  #(loop [x %]
     (if (< (count x) 2)
       true
       (if (= (last x) (first x))
         (recur (drop 1 (take (dec (count x)) x)))
         false))))

(defcheck solution-d532d6b8
  (fn palindrome [xs]
    ((fn helper [ys zs]
       (cond
         (and (empty? ys) (empty? zs)) true
         (or (empty? ys) (empty? zs)) false
         (= (first ys) (first zs)) (helper (rest ys) (rest zs))
         true false
         )
       )  xs (reverse xs) )
    ))

(defcheck solution-d5357b3b
  (fn pali [x]
    (if (empty? x)
      true
      (if (= (first x) (last x))
        (pali (-> x rest reverse rest))
        false))))

(defcheck solution-d553da91
  (fn pal [xs] (= (seq xs) (reverse (seq xs)))))

(defcheck solution-d6cf92cb
  (fn [input] (= (seq input) (reverse (seq input)))))

(defcheck solution-d77f3a96
  #(letfn [(pal [x] (or (empty? (rest x)) (and (= (first x) (last x)) (pal (rest (drop-last x))))))] (pal %)))

(defcheck solution-d8661770
  (fn this [s] (if (string? s) (this (vec s))
                               (let [len (count s)]
                                 (if (<= len 1) true
                                                (and (= (first s) (last s))
                                                     (this (butlast (rest s)))))))))

(defcheck solution-d872f3fe
  (fn [x] (= (clojure.string/join x) (clojure.string/join (reverse x)))))

(defcheck solution-da6f09d5
  (fn pala [s] (if (seq (rest s)) (if (= (first s) (last s)) (pala (drop 1 (drop-last 1 s))) false) true)))

(defcheck solution-da96e726
  (fn [something]
    (let [rev (reverse something)
          rev2 (if (string? something)
                 (apply str rev)
                 rev)]
      (= something rev2))))

(defcheck solution-dcc71504
  ;(fn pal [s]
  ;  (if (< (count s) 2)
  ;    true
  ;    (and (= (first s) (last s)) (pal (drop 1 (reverse (drop 1 s)))))))
  #(= (vec %) (reverse %)))

(defcheck solution-dd311cf4
  #( = (reverse(seq %)) (seq %) ))

(defcheck solution-de4b0469
  #(= (seq %) (seq (reverse %))))

(defcheck solution-e28ec840
  (fn [x] (cond (string? x) (= (apply str (reverse x)) x) :else (= (reverse x) x))))

(defcheck solution-e3a15b1f
  (fn [vs]
    (let [n (quot (count vs) 2)
          front-half (take n vs)
          back-half (->> vs reverse (take n))]
      (= front-half back-half))))

(defcheck solution-e3d1ec5d
  #(every? true? (map = % (reverse %))))

(defcheck solution-e40be650
  (fn palindromo? [s]
    (cond
      (nil? s) true
      (= 1 (count s)) true
      (not= (first s) (last s)) false
      :else (palindromo? (next (drop-last s))))))

(defcheck solution-e4333faa
  #(let [median (quot (count %) 2)]
     (= (take median %) (take median (reverse %)))))

(defcheck solution-e44ca824
  (fn [p] (= (reverse p) (seq p))))

(defcheck solution-e54467e1
  (fn
    [coll]
    (= (reverse coll) (seq coll))))

(defcheck solution-e562f767
  (fn [inp]
    (= inp (if (string? inp)
             (clojure.string/join "" (reverse inp))
             (reverse inp)))))

(defcheck solution-e5b3b5d6
  #(let [length (count %)]
     (if (<= length 1)
       true
       (let [
             f (first %)
             l (last %)]
         (and (= f l ) (recur (drop 1 (drop-last 1 %))))
         ))))

(defcheck solution-e5bae1e3
  (fn [word]
    (= (reverse (reverse word)) (reverse word))
    ))

(defcheck solution-e6fc6bcc
  (fn p [s] (= (seq s) (reverse s))))

(defcheck solution-e78a0d89
  (fn rev [x] (if (string? x) (= x (apply str (reverse x))) (= x(reverse x)))))

(defcheck solution-e83c921b
  (fn ispal[x] (if (string? x) (= x (apply str (reverse x))) (= x (reverse x)))))

(defcheck solution-e895cea1
  (fn [e]
    (= (reverse e) (reverse (reverse e)))))

(defcheck solution-e9c822a2
  #(let [mid (rest (butlast %))]
     (and (= (first %) (last %))
          (or (<= (count mid) 1)
              (recur mid)))))

(defcheck solution-e9fd706f
  (fn [s]
    (let [seq (into '() s)]
      (= seq (into '() seq)))))

(defcheck solution-ea7bb8b8
  #(= (into '() %) (seq %)))

(defcheck solution-eac68488
  (fn palindrome? [x]
    (let [sx (seq x)] (= sx (reverse sx)))))

(defcheck solution-ebae0c36
  (fn is_palindrome? [x]
    "Returns true if x is a palindrome, false otherwise."
    (if (<= (count x) 1)
      true
      (if (not (= (first x) (last x)))
        false
        (is_palindrome? (rest (butlast x)))))))

(defcheck solution-ebc46cb6
  #(or (= (reverse %) %) (string? %)))

(defcheck solution-ebeb8355
  (fn [coll] (= (reverse coll) (reverse (reverse coll)))))

(defcheck solution-ece9d3e0
  (fn [s] (every? true? (map #(= (key %) (val %)) (zipmap s (reverse s))))))

(defcheck solution-ed646ecd
  #(= % (if (string? %) (apply str (reverse %)) (reverse %))))

(defcheck solution-ed987d85
  (fn pe [s]
    (if (not (second s))
      true
      (and (= (first s) (last s)) (-> s butlast rest pe)))))

(defcheck solution-ef0e60c8
  (fn [col]
    (if (< (count col) 2)
      true
      (let [s (seq col)]
        (if (= (first s) (last s))
          (recur (drop-last (rest s)))
          false)))))

(defcheck solution-efa0cf60
  (fn [xs] (= (reverse xs) (reverse (reverse xs)))))

(defcheck solution-efae570b
  #(let [lst (seq %)]
     (= lst (reverse lst))))

(defcheck solution-f0a9349
  (fn [cl] (let [ncl (count cl), hcl1 (long (Math/ceil (/ ncl 2.0)))
                 cl2 (partition (long (/ ncl 2)) hcl1 cl)]
             (every? #(= true %) (map #(= %1 %2) (nth cl2 0) (reverse (nth cl2 1))) )

             )))

(defcheck solution-f0ac1b0b
  (fn [args] (= args (if (string? args) (apply str (reverse args)) (reverse args)))))

(defcheck solution-f0c12fbf
  (fn palindrome?
    [coll] {:pre [(or (sequential? coll) (string? coll) (nil? coll))]}
    (if (seq coll)
      (= (seq coll) (reverse coll))
      true)))

(defcheck solution-f1252379
  #(loop [i 0 seq1 nil seq2 nil]
     (if (= i (quot (count %1) 2))
       (= seq1 seq2)
       (recur (inc i) (conj seq1 (nth %1 i)) (conj seq2 (nth %1 (- (count %1) i 1))))
       )))

(defcheck solution-f27ed7ce
  #(= (take (bit-shift-right (count %) 1) (vec %)) (reverse (take-last (bit-shift-right (count %) 1) (vec %)))))

(defcheck solution-f2aa20c8
  #(= (reverse %)(seq %)))

(defcheck solution-f2cb660f
  #(-> % (seq) (reverse) (= (seq %))))

(defcheck solution-f2ecb4c5
  (fn [p] (= (seq p) (reverse p))))

(defcheck solution-f33532c
  #(reduce (fn [a b] (and a b)) (map = % (reverse %))))

(defcheck solution-f40ee9cd
  #(= (reverse %)(map identity %)))

(defcheck solution-f44a7ee8
  (fn palin [c]
    (let [n (quot (count c) 2)
          r (reverse c)]
      (= (take n c) (take n r)))))

(defcheck solution-f46790a
  (fn [t]
    (=  (reverse t) (seq t))))

(defcheck solution-f473628e
  (fn [s]
    (= (reverse s) (reverse (reverse s)))
    ))

(defcheck solution-f4c8cbb8
  (fn palindromer
    [coll]
    (let [halfway (int (Math/ceil (/ (count coll) 2)))]
      (= (take halfway (reverse coll)) (take halfway coll)))))

(defcheck solution-f5505cea
  (fn isPalindrome [alist] (if (>= 1 (count alist)) true (if (= (first alist) (first (reverse alist))) (isPalindrome (rest (reverse (rest alist)))) false))))

(defcheck solution-f579b654
  (fn [sequence]
    (let [sq (seq sequence)]
      (loop [i 0 j (- (count sq) 1)]
        (cond
          (= i j) true
          (and (= (- j 1) i) (= (nth sq i) (nth sq j))) true
          (= (nth sq i) (nth sq j)) (recur (inc i) (dec j))
          (not (= (nth sq i) (nth sq j))) false)))))

(defcheck solution-f62a244b
  #(every? true? (take (/ 2 (count %)) (map = % (reverse %)))))

(defcheck solution-f65a2f18
  #(= (apply str (reverse %)) (apply str %)))

(defcheck solution-f662d247
  #(= % ((if (string? %) (partial apply str) identity) (reverse %))))

(defcheck solution-f70b107d
  #(= (seq %)(seq (reverse %))))

(defcheck solution-f7261893
  (fn pal [l]
    (or
     (empty? l)
     (and
      (= (first l) (last l))
      (pal (butlast (rest l)))
      )
     )
    ))

(defcheck solution-f7bdbc0e
  (fn [col]
    (= (vec col) (vec (reverse col)))))

(defcheck solution-f81ab2dc
  (fn [s]
    (letfn [(rev [s]
              (if (string? s)
                (clojure.string/reverse s)
                (clojure.core/reverse s)))]
      (= s (rev s)))))

(defcheck solution-f822dbe8
  #(= (reverse %) (list* %)))

(defcheck solution-f9431cd0
  (fn palindrome? [s]
    (let [c (count s)
          hc (/ c 2)]
      (=
        (take hc s)
        (take hc (reverse s))))))

(defcheck solution-f951593d
  #(= (concat '() %1) (reverse %1)))

(defcheck solution-fa4b0285
  #(loop [s %]
     (cond
       (empty? s) true
       (not= (first s) (last s)) false
       :else (recur (rest (butlast s))))))

(defcheck solution-fb104ee0
  #(= %
     (if (string? %)
       (apply str (reverse %))
       (reverse %))))

(defcheck solution-fbc0d138
  #(= (seq %)(reverse %)))

(defcheck solution-fbe90498
  (fn ! [s]
    (or (empty? s)
        (and (= (first s) (last s))
             (! (butlast (rest s)))))))

(defcheck solution-fc854e0c
  (fn [s] (= (reverse s) (seq s))))

(defcheck solution-fd24233e
  (fn [x]
    (let [x (seq x)]
      (= x (reverse x)))))

(defcheck solution-fdf10f43
  (fn [coll]
    (let [ct (count coll)
          halfway (int (/ ct 2))]
      (loop [i 0]
        (cond
          (> i halfway) true
          :else
          (if (not (= (nth coll i) (nth coll (- ct i 1))))
            false
            (recur (inc i))))))))

(defcheck solution-fe2b6108
  (fn [s]
    (= (seq s) (reverse s))))

(defcheck solution-feadb0cd
  (fn is-pal [p]
    (cond (> 2 (count p)) true
          (= (first p) (last p)) (is-pal (rest (drop-last p)))
          :else false)))

(defcheck solution-ff2dcbb1
  (fn [col]
    (if (seq? col)
      (= col (reverse col))
      (recur (sequence col)))))

(defcheck solution-ff806c15
  (fn [x]
    (not (or (= 5 (last x)) (= :a (first x))))))

(defcheck solution-ffcf0ac4
  (fn pal? [xs] (cond
                  (< (count xs) 2) true
                  (not= (first xs) (last xs) ) false
                  :else (recur
                          (  (comp (partial drop 1) drop-last)  xs )
                          ))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-27))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
