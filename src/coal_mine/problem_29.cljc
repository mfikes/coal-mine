(ns coal-mine.problem-29
  (:require [coal-mine.checks :refer [defcheck-29] :rename {defcheck-29 defcheck}]
            [clojure.test]
            [clojure.string]))

(defn upper-case? [c]
  #?(:clj  (Character/isUpperCase c)
     :cljs ((set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") c)))

(defcheck solution-103c2ef7
  (fn [n] (apply str (filter #(upper-case? %) n))))

(defcheck solution-1041ac1b
  (fn only-caps [a-str]
    (apply str (filter #(upper-case? %) a-str))
    ))

(defcheck solution-10678d18
  (fn [s]
    (apply str (filter #(upper-case? %) (seq s)))
    ))

(defcheck solution-129ea335
  (fn [x] (apply str (re-seq #"[A-Z]" x))))

(defcheck solution-12db9287
  #_(fn [x] (apply str (filter
                         #(upper-case? %) x)))

  #_#(clojure.string/replace % #"[^A-Z]" "")
  #(apply str (re-seq #"[A-Z]*" %)))

(defcheck solution-131827e2
  #(apply str (re-seq '#"[A-Z]" %)))

(defcheck solution-134ed0a7
  (fn [x] (apply str (filter #(upper-case? %) (seq x)))))

(defcheck solution-1408e06a
  (fn [s] (apply str (filter #(not= (clojure.string/lower-case %) (str %)) s))))

(defcheck solution-146110ad
  (fn[x] (apply str (re-seq #"[A-Z]" x))))

(defcheck solution-14e30e0f
  (fn [a] (clojure.string/join "" (filter #(upper-case? %) a))))

(defcheck solution-15c3abc9
  (fn [string] (apply str (re-seq #"[A-Z]+" string))))

(defcheck solution-163efaed
  (fn [x] (reduce str (re-seq #"[A-Z]" x))))

(defcheck solution-18dad152
  (fn [s] (clojure.string/replace s #"[^A-Z]+" "")))

(defcheck solution-190a6a12
  ;(fn [x] (reduce str (map str (filter #(and (>= (int %) (int \A)) (<= (int %) (int \Z))) x))))
  (fn [x] (reduce str (re-seq #"[A-Z]" x))))

(defcheck solution-19a1d965
  (fn [x] (apply str (filter #((set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") %) x))))

(defcheck solution-1bbff832
  (fn [value]
    (apply str (filter #(upper-case? %) value))))

(defcheck solution-1d3a3d6b
  (fn [string]
    (apply str (re-seq #"[A-Z]" string))))

(defcheck solution-1d68dfc3
  (fn [S] (clojure.string/join (filter #(upper-case? %) (seq S))) ))

(defcheck solution-1e1d4d90
  (fn [s]
    (apply str (
                 filter #(upper-case? %) s
                 )
      )
    ))

(defcheck solution-1e406c32
  (fn [s]
    (->> (seq s)
      (filter #(upper-case? %))
      (apply str))))

(defcheck solution-1f2f0f9a
  (fn [s]
    (apply str
      (re-seq #"[A-Z]" s))))

(defcheck solution-20034221
  (fn [s]
    (let [split #(clojure.string/split % #"")
          is-letter? #(not (nil? (re-matches #"[A-Z]" %)))
          is-capitalized? #(= (clojure.string/upper-case %) %)]
      (->> s split (filter is-letter?) (filter is-capitalized?) (reduce str)))))

(defcheck solution-21002f0e
  (fn filter-uppers
    [input]
    (let [input-seq (seq input)]
      (apply str
        (filter #(re-matches #"[A-Z]" (str %)) input-seq)))))

(defcheck solution-21824154
  (fn [x] (clojure.string/join "" (filter #(upper-case? %) (into [] x)))))

(defcheck solution-2191a37b
  (fn [s] (apply str (let [ss (seq s)] (filter #(upper-case? %) ss)))))

(defcheck solution-227acf1
  (fn [s] (apply str (filter #(upper-case? %) (seq s)))))

(defcheck solution-23013205
  (fn [x]
    (apply str (filter #(#{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} %) x))))

(defcheck solution-236b8f02
  (fn [input]
    (clojure.string/join "" (filter #(upper-case? %) (seq input)))))

(defcheck solution-249f4d64
  (fn [x]
    (apply str (filter #(upper-case? %) x))))

(defcheck solution-253952e3
  #(reduce str(re-seq #"[A-Z]" %)))

(defcheck solution-26cc692f
  #(clojure.string/replace %  #"[^A-Z]" ""))

(defcheck solution-27cdd982
  #(apply str (re-seq #"[A-Z]+" %1)))

(defcheck solution-27e9c7c5
  (fn [s] (let [s (clojure.string/replace s #"[^a-zA-z]" "")] (apply str (map first (filter (fn [[a b]] (= a b)) (map vector s (clojure.string/upper-case s))))))))

(defcheck solution-2838d582
  (fn [s] (apply str (for [ch s :when (upper-case? ch)] ch))))

(defcheck solution-2a517ef
  (fn capsOnly [x]
    (apply str (filter #(upper-case? %) (seq x)))))

(defcheck solution-2c2085c8
  (fn [x]
    (apply str
      (filter upper-case? x))))

(defcheck solution-2d59fe85
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-2e16744b
  #(apply str (re-seq (re-pattern "[A-Z]+") %)))

(defcheck solution-2f1b4167
  #(apply str (filter (fn f [x] (upper-case? x)) %)))

(defcheck solution-304da749
  (fn [s] (apply str(re-seq #"[A-Z]" s))))

(defcheck solution-3075b00d
  (fn only-upper-case [st]
    (->> st (filter #(upper-case? %)) (apply str))))

(defcheck solution-32826048
  (fn [x] (clojure.string/join (filter #(upper-case? %) x))))

(defcheck solution-33f717d3
  #(->> %
     (re-seq #"[A-Z]")
     (apply str)))

(defcheck solution-3596d032
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-3750e37a
  (fn [seq] (apply str (re-seq #"[A-Z]+" seq))))

(defcheck solution-375cd2c7
  (fn [x] (apply str (filter
                       #(not (nil? (re-matches #"[A-Z]" (str %))))
                       x))
    ))

(defcheck solution-378f3dbb
  #(->> % (re-seq #"[A-Z]+") concat (apply str)))

(defcheck solution-383f2923
  (fn [l] (apply str (re-seq #"[A-Z]+" l))))

(defcheck solution-3a735ae5
  (comp
    clojure.string/join
    (partial filter #(upper-case? %))))

(defcheck solution-3aa7fd46
  (fn strToList [s] (apply str (for [i (range 0 (count s))](let [z (.charAt s i)] (if (upper-case? z) z nil))))))

(defcheck solution-3b38d5cd
  (fn [s]
    (apply
      str
      (filter #(upper-case? %) s))))

(defcheck solution-3d204606
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-3e5a8e9e
  (fn only-capitals [s]
    (apply str (seq (filter #(upper-case? %) s)))))

(defcheck solution-3fb16b3d
  #(->>
     (re-seq #"[A-Z]" %)
     (apply str)))

(defcheck solution-3fd4ef87
  (fn gtc [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-400908bc
  #(->> (re-seq #"[A-Z]+" %)

     (apply str)))

(defcheck solution-42066977
  (fn filter-upper [s] (apply str (filter #(and (<= 0 (compare % \A)) (>= 0 (compare % \Z))) s))))

(defcheck solution-42d6a98d
  #(clojure.string/join (re-seq #"[A-Z]" %)))

(defcheck solution-4338af4
  (fn [strn]
    (apply str (re-seq #"[A-Z]+" strn))
    ))

(defcheck solution-4351896d
  (fn [x] (apply str (re-seq #"[A-Z]+" x))))

(defcheck solution-4392f8de
  (fn [s] (apply str
            (filter #(upper-case? %) s))))

(defcheck solution-45682768
  #(apply str(re-seq #"[A-Z]"%)))

(defcheck solution-45addf00
  (fn [s] (apply str (filter
                       #(upper-case? (char %)) s))))

(defcheck solution-45feed1e
  (fn [s]
    (reduce
      str
      (filter #(upper-case? %1) (seq s)))))

(defcheck solution-4692ff16
  #(apply str (apply concat (re-seq #"[A-Z]+" %))))

(defcheck solution-477834d4
  (fn [st] (apply str (filter #(upper-case? %) st))))

(defcheck solution-4786fbbe
  (fn [x]
    (apply str ( filter #(re-find #"[A-Z]" (str %1)) x
                 ))))

(defcheck solution-497f46f8
  (fn [s]
    (clojure.string/join (filter (fn [c] (upper-case? c)) s))))

(defcheck solution-4b635563
  (fn [s]
    (apply str
      (filter #(not= 0 (compare (str %) (clojure.string/lower-case (str %))))
        (str s)))))

(defcheck solution-4df12fdd
  (fn [x] (apply str (filter #(upper-case? %1) x))))

(defcheck solution-4e0db6ec
  #(->> % (re-seq #"[A-Z]") (apply str)))

(defcheck solution-4e1223db
  (fn [s] (apply str (map #(if(upper-case? %) %) s))))

(defcheck solution-4fa2bf61
  (fn [s] (apply str (filter #(not (nil? (re-find #"[A-Z]" (str %)))) s))))

(defcheck solution-524592c3
  #(apply str (filter (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") %)))

(defcheck solution-526fc199
  #(clojure.string/replace %1 #"[^A-Z]" ""))

(defcheck solution-53f231c8
  (fn re-str1 [s] (clojure.string/join "" (re-seq #"[A-Z]" s))
    ))

(defcheck solution-553948
  (fn caps [x] (apply str
                 (filter #(upper-case? %) x))))

(defcheck solution-561d0cb7
  #(letfn [(f [c] (and (< (compare c \Z) 1) (< (compare \A c) 1)))] (apply str (filter f (seq %)))))

(defcheck solution-5759c34c
  (fn [s] (apply str (filter #(upper-case? %) s))))

(defcheck solution-57bb1a61
  #(apply str (filter (apply hash-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")  %)))

(defcheck solution-58d73239
  #(clojure.string/join (filter (complement (fn [c] (= (str c) (clojure.string/lower-case c)))) %)))

(defcheck solution-58e0c74a
  (comp (partial apply str)
    (partial filter #(upper-case? %))))

(defcheck solution-593e8e84
  (fn [input-string]
    (apply str (filter #(upper-case? %) (seq input-string)))))

(defcheck solution-5a163e71
  (fn
    [string]
    (clojure.string/join (re-seq #"[A-Z]" string))))

(defcheck solution-5b85f4d2
  #(apply str(re-seq #"[A-Z]" %)))

(defcheck solution-5baab3e5
  #(apply str (reduce
                (fn[x y]
                  (if
                   (upper-case?   y)

                    (conj x (str y))
                    (conj x (str ""))
                    )
                  )
                []
                %
                )
     ))

(defcheck solution-5bfc474c
  (fn [x]
    (apply str (filter (fn [c]
                         (upper-case? c)
                         ) x))
    ))

(defcheck solution-5db892b8
  #( apply str (re-seq #"[A-Z]" %) ))

(defcheck solution-5f092820
  (fn [string]
    (reduce str (re-seq #"[A-Z]" string))))

(defcheck solution-5f160d2d
  #(apply str (re-seq #"[A-Z]" %)))

(defcheck solution-5fad8b63
  (fn [st]
    (apply str (re-seq #"[A-Z]+" st))))

(defcheck solution-602b6f51
  (fn [word] (apply str (filter #(upper-case? %) (seq word)))))

(defcheck solution-607fa2d7
  (fn upper-only [s]
    (apply str (filter #(upper-case? %) (seq s)))))

(defcheck solution-612630c2
  #(apply str (filter (fn [x] (upper-case? x)) (seq %))))

(defcheck solution-639fb568
  #(apply str (filter (fn [c]
                        (upper-case? c))
                %)))

(defcheck solution-654db830
  (fn caps [s] (apply str (filter #(upper-case? %) s))))

(defcheck solution-660d8dcb
  (fn [x] (reduce (fn [y z] (if (re-matches #"[A-Z]" (str z)) (str y z) (str y))) "" (seq x))))

(defcheck solution-6646ee99
  (fn [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-66f799b6
  (fn [s]
    (let [caps (set (map char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))]
      (apply str (filter #(contains? caps %) s)))))

(defcheck solution-679cb0c2
  (fn [s] (apply str (re-seq #"[A-Z]+" s))))

(defcheck solution-6829811f
  (fn [word]
    (let [s (seq word)
          upper (map char (range 65 91))]
      (apply str (filter (fn [x] (some #(= x %) upper)) s)))))

(defcheck solution-682b2119
  #(clojure.string/join "" (filter (fn [x] (upper-case? x)) %)))

(defcheck solution-68309caf
  #(apply str (re-seq #"[A-Z]" % )))

; See CLJS-2453
#_(defcheck solution-6888979e
  (fn [word]
    (->> word
      (re-seq #"[A-Z]*")
      (filter #(first %))
      (clojure.string/join ""))))

(defcheck solution-695db31b
  (fn [coll]
    (apply str (filter #(upper-case? %) coll))
    ))

(defcheck solution-6994e84d
  (fn f [s]
    (apply str
      (filter (complement nil?)
        (map (partial re-find #"[A-Z]")
          (map str s))))))

(defcheck solution-6a7b2c4e
  (fn [s] (apply str (doall (filter #(upper-case? %) s)))))

(defcheck solution-6c50454a
  (fn [s]
    (apply str
      (re-seq #"[A-Z]" s))))

(defcheck solution-6e343651
  (fn [phrase]
    (apply str
      (filter (fn [ch] (not= (clojure.string/lower-case ch)
                         (str ch)))
        phrase
        )
      )
    ))

(defcheck solution-6e8b1270
  (fn [s] (apply str
            (filter #(re-matches #"[A-Z]" (str %)) s))))

(defcheck solution-6eb7017a
  (fn [string]
    (apply str (filter #(re-seq #"[A-Z]" (str %)) string))))

(defcheck solution-6f36719a
  (fn [s]
    (let [upper? #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z}]
      (apply str (filter upper? s)))))

(defcheck solution-6f814817
  (fn [s] (apply str (filter (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") s))))

(defcheck solution-6f8e07a7
  (fn [word] (apply str (filter #(upper-case? %) word))))

(defcheck solution-6fd27427
  (fn [string] (apply str (filter #(upper-case? %) (seq string)))))

(defcheck solution-6ff6c6de
  (comp (partial apply str) (partial filter (fn [c] (upper-case? c)))))

(defcheck solution-70038e91
  (fn [s](reduce str(re-seq #"[A-Z]" s))))

(defcheck solution-70f91350
  (fn [s] (apply str (map (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") s))))

(defcheck solution-71d4a05b
  (fn[arg] (apply str (re-seq #"[A-Z]" arg))))

(defcheck solution-71de3262
  (fn [astr] (apply str (filter #(upper-case? %) astr))))

(defcheck solution-71f0ed16
  (fn get-caps [string]
    (cond (empty? string) ""
      (upper-case? (first string)) (str (first string) (get-caps (subs string 1)))
      :else (get-caps (subs string 1)))))

(defcheck solution-71f6f36b
  #(->> % (re-seq #"[A-Z]") (apply str) ))

(defcheck solution-7240d61
  (fn [sentence]
    (apply str (filter #(upper-case? %) (seq sentence)))))

(defcheck solution-72894d8b
  (fn [xs] (apply str (filter #(contains? #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} %) xs))))

(defcheck solution-73ff7279
  (fn[x] (reduce str (re-seq #"[A-Z]*" x))))

(defcheck solution-748d6a7
  (comp (partial apply str) #(filter (fn [x](upper-case? x)) %)))

(defcheck solution-74c3874d
  (fn [s]
    (->> (re-seq #"[A-Z]+" s)
      (apply str))))

(defcheck solution-74ee987e
  #(apply str
     (map (set (map char (range 65 91))) %)))

(defcheck solution-7637ec2a
  (fn [s] (reduce str(filter (fn [c] (some #(= c %) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) s))))

(defcheck solution-77fc8409
  (fn [y] (apply str (filter (fn [x] (reduce #(or % %2) (for [i (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")] (= i x)))) (vec y)))))

(defcheck solution-780539fe
  #(clojure.string/join (re-seq #"[A-Z]+" %1)))

(defcheck solution-78653b3e
  #(apply str (clojure.string/split % #"[^A-Z]")))

(defcheck solution-78f594fa
  (fn __ [s]
    (apply str (re-seq #"[A-Z]" s))
    ))

(defcheck solution-79aca0b4
  (fn [text] (reduce str (re-seq #"[A-Z]" text))))

(defcheck solution-79d12114
  #(reduce str (re-seq #"[A-Z]" %)))

(defcheck solution-7a4fc357
  (fn [s] (apply str (filter #(not= (str %) (clojure.string/lower-case %)) s))))

(defcheck solution-7dc3d12a
  (fn capitalX [x] (apply str (filter #(upper-case? %) x))))

(defcheck solution-7e15537
  (fn [xs] (reduce str (re-seq #"[A-Z]" xs))))

(defcheck solution-7e84ad2c
  (fn onlycaps [x]
    "Takes a string and returns a string of only the uppercase characters."
    (if (empty? x)
      ""
      (if (upper-case? (first x))
        (str (first x) (onlycaps (rest x)))
        (onlycaps (rest x))))))

(defcheck solution-7efee68b
  (fn caps
    [string]
    (apply str (filter #(upper-case? %) (seq string)))))

(defcheck solution-7f0ed00d
  (fn [st] (apply str (filter #(upper-case? %) (seq st)))))

(defcheck solution-7fa95e76
  (fn  [s] (apply str (filter #(upper-case? %) s))))

(defcheck solution-7fcdcb59
  (fn getCaps
    [st]
    (apply str (re-seq #"[A-Z]+" st))))

(defcheck solution-7fd4d135
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-7feba333
  (fn [s]
    (apply str (for [c s :when (upper-case? c)] c))))

(defcheck solution-803079df
  (fn [s]
    "29. Write a function which takes a string and returns a new string containing only the capital letters."
    (let [cap (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]
      (apply str (filter (fn [c] (contains? cap c)) s)))))

(defcheck solution-80576bb4
  (fn [x] (apply str (filter #(upper-case? %) x))))

(defcheck solution-8173abcb
  (fn [l]
    (reduce (fn [ret this]
              (str ret (re-find #"[A-Z]" (str this)))) nil l)))

(defcheck solution-81cc2f16
  (fn [word]
    (clojure.string/join (filter #(upper-case? %) word))))

(defcheck solution-8272a4f1
  (fn [s] (apply str (filter #(and (<= (compare % \Z) 0) (>= (compare % \A) 0)) s))))

(defcheck solution-83e473f7
  (fn [s] (->> s (re-seq #"[A-Z]") (apply str))))

(defcheck solution-8468696e
  (fn [xs] (apply str (filter #(upper-case? %) xs))))

(defcheck solution-849c5ab4
  #(apply str (re-seq #"[A-Z+]" %)))

(defcheck solution-8528178
  #(apply str (re-seq  #"[A-Z]" %)))

(defcheck solution-8547f25e
  (fn [s]
    (let [t (re-seq #"[A-Z]" s)]
      (if (empty? t)
        nil
        (apply str t)))))

(defcheck solution-86259535
  (fn [s] (apply str (re-seq #"[A-Z]" s))))

(defcheck solution-869b5cfb
  #(reduce str (re-seq #"[A-Z]?" %)))

(defcheck solution-86c36343
  (fn [x] (apply str (filter upper-case? x))))

(defcheck solution-86f1426a
  (fn [s] (clojure.string/join (filter #(re-find #"[A-Z]" (str %))  s))))

(defcheck solution-87ed4673
  (fn gtc[x]
    (apply str (re-seq #"[A-Z]" x))))

(defcheck solution-88fa3181
  (fn [s]
    (apply str (filter #(upper-case? %) (seq s)))))

(defcheck solution-8a4c09db
  (fn caps [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-8b6c0952
  (fn [l]
    (reduce
      #(if
        (nil?
          (re-matches #"[A-Z]" (str "" %2)))
         %1
         (str %1 %2)
         ) "" l
      )
    ))

(defcheck solution-8bcaee7b
  (fn [x] (clojure.string/replace x #"[^A-Z]" "")))

(defcheck solution-8cf8a3b8
  (fn [s]
    (apply str (re-seq #"[A-Z]" s))))

(defcheck solution-8cff3f73
  (fn [s] (reduce str (filter #(upper-case? %) s))))

(defcheck solution-8d7b58d5
  #(apply str (map (into #{} "ABCDEFGHIJKLMNOPQRSTUVWXYZ") %)))

(defcheck solution-8dab791
  (fn [coll] (reduce str (filter #(upper-case? %) coll))))

(defcheck solution-8f0d5415
  #(->> % (map (fn [s] (re-matches #"[A-Z]" (str s)))) (filter (complement nil?)) (apply str)))

(defcheck solution-8f114c31
  (fn [sin] (apply str (filter #(upper-case? %) sin))))

(defcheck solution-9059b9da
  (fn [input]
    (apply str (re-seq #"[A-Z]" input))))

(defcheck solution-907da51e
  (fn [l] (apply str (filter #(upper-case? %) l))))

(defcheck solution-911486f6
  (fn [str] (->> str (filter #(upper-case? %)) clojure.string/join)))

(defcheck solution-9177f9c
  (fn [s]
    (clojure.string/replace s #"[^A-Z]" "")))

(defcheck solution-91f34e62
  (fn [chars]
    (apply str(filter #(upper-case? %) (seq chars)))))

(defcheck solution-92ac13dd
  (fn [a] (apply str (re-seq #"[A-Z]*" a))))

(defcheck solution-92c9f3
  (fn [input] (apply str (filter #(upper-case? %) input))))

(defcheck solution-92f2f88f
  (fn getCaps
    [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-93cff429
  (fn [s] (apply str  (filter #(re-find #"[A-Z]" (str %)) s))))

(defcheck solution-93eac696
  #(apply str
     (filter
       (fn [c] (upper-case? c))
       %)))

(defcheck solution-94003058
  (fn up [x] (apply str (filter #(upper-case? %) x))))

(defcheck solution-94842f34
  #(clojure.string/join (filter (partial re-find #"[A-Z]") (map str %) )))

(defcheck solution-948518e8
  (fn [n] (clojure.string/join "" (clojure.string/split n #"[^A-Z]+"))))

(defcheck solution-948a82a7
  (fn [s]
    (apply str (filter (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") s))))

(defcheck solution-949428bd
  (fn mytest [string]
    (apply str (re-seq #"[A-Z]+" string))))

(defcheck solution-95e1e532
  (fn [s]
    (reduce str (re-seq #"[A-Z]" s))))

(defcheck solution-96605b1
  (fn [s] (apply str (filter (fn [x] (if (empty? (re-seq #"[A-Z]" (str x))) false true)) s))))

(defcheck solution-966cc9ef
  (fn [s] (clojure.string/join (filter #(upper-case? %) s))))

(defcheck solution-96fc4655
  (fn [col] (apply str (filter #(upper-case? %) col))))

(defcheck solution-9746ab7c
  (fn [s]
    (apply str (filter
                 (apply hash-set (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) s))))

(defcheck solution-9767692f
  (fn [s]
    (apply str (filter #(contains? (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") %)
                 (vec s)))))

(defcheck solution-9782bee5
  (fn caps [x]
    (let [caps "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (apply str (filter (fn [y] (some #{y} caps)) (seq x)))
      )
    ))

(defcheck solution-97b1355
  (comp clojure.string/join (partial re-seq #"[A-Z]")))

(defcheck solution-99fcf0db
  (fn get-caps [string]
    (apply str (re-seq #"[A-Z]" string))))

(defcheck solution-9a7da649
  (fn upper [x] (apply str (filter #(upper-case? %) x))))

(defcheck solution-9b9698aa
  #(apply str (filter
                (fn [c] (upper-case? c)) %)))

(defcheck solution-9c80d17a
  (fn [input] (apply str (re-seq #"[A-Z]" input))))

(defcheck solution-9da74662
  (fn te [x]
    (apply str
      (reduce
        (fn [a b]
          (if (and
                (= (str b) (clojure.string/upper-case (str b)))
                (not (= (str b) (clojure.string/lower-case (str b)))))
            (conj a b)
            a
            )
          ) [] (seq x)
        )
      )
    ))

(defcheck solution-9db913be
  (fn [s] (let [upper? (fn [c] (boolean (re-matches #"[A-Z]" (str c))))] (apply str (filter upper? s)))))

(defcheck solution-9e3940ae
  (fn [s] (clojure.string/join (filter (fn [ch] (upper-case? ch)) s))))

(defcheck solution-9eb93dc7
  (fn [s] (clojure.string/join (filter #(not (nil? (re-find #"[A-Z]" (str %)))) s))))

(defcheck solution-9ec2a428
  (fn [w] (clojure.string/join (filter #(upper-case? %) w))))

(defcheck solution-a10165c0
  (fn[s] (apply str (re-seq #"[A-Z]" s))))

(defcheck solution-a1175e92
  (fn [s] (reduce str (#(re-seq #"[A-Z]+" %) s))))

(defcheck solution-a13ac172
  (fn [s] (apply str
            (filter #(upper-case? %) s))))

(defcheck solution-a145ea9a
  (fn [in]
    (clojure.string/join
      (filter #(upper-case? %) (seq in))
      )
    ))

(defcheck solution-a192e3e7
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-a2324fc4
  (fn [s]
    (apply str (filter (partial re-matches #"[A-Z]") (map str (seq s))))))

(defcheck solution-a469287f
  (fn
    [s]
    (reduce str
      (re-seq #"[A-Z]" s))))

(defcheck solution-a5b2a7d7
  #(clojure.string/replace % #"[^A-Z]+" ""))

(defcheck solution-a726dcb1
  (fn [s]
    (apply str (re-seq #"[A-Z]" s))))

(defcheck solution-a76b473a
  (fn [s]
    (apply str
      (filter #(upper-case? %) s))))

(defcheck solution-a76ee7ed
  (fn [x] (reduce str "" (filter #(upper-case? %) x))))

(defcheck solution-a7fc1a73
  #(apply str (reduce concat (re-seq #"[A-Z]" %))))

(defcheck solution-a82d89bf
  (fn p [x]
    (apply str (re-seq #"[A-Z]+" x))))

(defcheck solution-a8dbba5b
  (fn [x] (reduce str (filter #(upper-case? %) x))))

(defcheck solution-aa6179b9
  (fn cap [s]
    (apply str (re-seq #"[A-Z]+" s))))

(defcheck solution-ab803b6f
  #(apply str (filter (fn [x] (upper-case? x)) %)))

(defcheck solution-abc44ff6
  (fn [string]
    (loop
      [[k & ks] string
       accum []]
      (let [new-accum (if (upper-case? k)
                        (conj accum k)
                        accum)]
        (if (nil? ks)
          (apply str new-accum)
          (recur ks new-accum))))))

(defcheck solution-ac5dd120
  (fn upcaseonly [s] (apply str (filter #(re-matches (re-pattern "[A-Z]") (str %)) s))))

(defcheck solution-ad44086d
  #(apply str
     (filter
       (fn [x] (upper-case? x))
       %)))

(defcheck solution-ae734b22
  (fn [s] (apply str (filter #(upper-case? %) s) )))

(defcheck solution-aefe4ac8
  (fn [s]
    (apply str (filter (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") s))))

(defcheck solution-af3d6943
  #(clojure.string/join "" (re-seq #"[A-Z]" %)))

(defcheck solution-afb4fa1b
  #(apply str (filter (fn [c] (upper-case? c)) %)))

(defcheck solution-afcb8f55
  (fn [s]
    (apply str (filter #(contains?
                          (set (map char (range 65 91))) %) (seq s)))))

(defcheck solution-aff58e4f
  (fn [s] (reduce #(str % %2) (conj (re-seq #"[A-Z]" s) ""))))

(defcheck solution-b1373530
  #(apply str (re-seq #"[A-Z]*" %)))

(defcheck solution-b1ccef8b
  (fn [s] (->> s (filter #(upper-case? %)) (apply str))))

(defcheck solution-b4baa50c
  #(apply str (filter (fn [x] (not (=  (first (clojure.string/lower-case x)) x))) (seq %)) ))

(defcheck solution-b5204556
  (fn [x]
    (apply str (filter #(upper-case? %) (seq x)))))

(defcheck solution-b60c3d4
  (fn [coll]
    (apply str (filter #(upper-case? %) coll))))

(defcheck solution-b78f5c8d
  (fn [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-b834cbe1
  #(apply str (filter (fn[b](upper-case? b)) %)))

(defcheck solution-b8469559
  (fn [s]
    (->> s
      (filter #(upper-case? %))
      (apply str))))

(defcheck solution-b94b5040
  (fn [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-b95c0f85
  (fn [s] (apply str (filter (fn [c] (upper-case? c)) s))))

(defcheck solution-ba9ae037
  (fn [text]
    (apply str
      (filter #(upper-case? %) text))))

(defcheck solution-bb0e3ad0
  (fn get-caps [input]
    (apply str (filter (fn [char] (upper-case? char)) input))))

(defcheck solution-bbef69b
  #(apply str(filter (set (map char (range 65 91))) %)))

(defcheck solution-bc3cc998
  (fn [s] (apply str (filter #(re-find #"[A-Z]" (str %)) s))))

(defcheck solution-bc85275e
  (fn [val] (apply str (filter #(upper-case? %) val))))

(defcheck solution-bcdbbb1e
  (fn [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-bddfe10d
  (fn
    [input]
    (apply str (re-seq #"[A-Z]" input))))

(defcheck solution-bfbfabb5
  (fn [in]  (apply str (re-seq #"[A-Z]" in) )))

(defcheck solution-c1881aac
  (fn fc [inp]
    (apply str (filter #(upper-case? %) inp))))

(defcheck solution-c1a3e83f
  #(clojure.string/replace % #"[^A-Z]" ""))

(defcheck solution-c1aa9211
  (fn [s] (apply str
            (filter #(and
                       (= (str %) (.toUpperCase (str %)))
                       (not (= (str %) (.toLowerCase (str %))))) s))))

(defcheck solution-c26a7828
  (fn
    [col]
    (apply
      str
      (filter
        #(upper-case? %)
        col)
      )
    ))

(defcheck solution-c275cb7
  (fn [x]
    (clojure.string/join (re-seq #"[A-Z]" x))))

(defcheck solution-c360d6d1
  #(apply str (re-seq #"[A-Z]" %1)))

(defcheck solution-c4a4d8f7
  (fn [s] (apply str  (filter #(upper-case? %) s))))

(defcheck solution-c59281b0
  (fn [coll] (apply str (filter #(upper-case? %) coll))))

(defcheck solution-c6181867
  (fn [s] (clojure.string/replace s #"[^A-Z]" "")))

(defcheck solution-c62c0bc8
  (fn [x]
    (apply str
      (filter upper-case? x))))

(defcheck solution-c6587333
  (fn [s] (reduce str (re-seq #"[A-Z]" s))))

(defcheck solution-c68bab45
  #(clojure.string/join (re-seq #"[A-Z]+" %)))

(defcheck solution-c8d3b353
  (fn [s] (clojure.string/join (filter #(not= (clojure.string/lower-case %) (str %)) (seq s)) )))

(defcheck solution-c8f4decf
  (fn only-lower [s] (apply str (re-seq #"[A-Z]+" s))))

(defcheck solution-c90a5b0
  (fn [st] (->> st (filter #(and (>= (compare % \A) 0) (<= (compare % \Z) 0))) (apply str))))

(defcheck solution-cac46256
  #(apply str (clojure.string/split % #"[^A-Z]*")))

(defcheck solution-cb6c8b79
  (fn [s]
    (apply str (filter #(upper-case? %) (seq s)))))

(defcheck solution-ccef0d5
  (fn [s] (apply str (filter #(upper-case? %) s))))

(defcheck solution-ce0b3862
  (fn get-caps [s]
    (apply str (re-seq #"[A-Z]+" s))))

(defcheck solution-ce9d8be8
  (fn [x] (apply str (filter (fn [a] (contains? #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} a)) (seq x)))))

(defcheck solution-cf04ecb9
  (fn [string]
    (->> string
      (keep #(re-find #"[A-Z]" (str %)))
      (apply str))))

(defcheck solution-cf2f2355
  (fn [x]
    (apply str (filter #(upper-case? %) x))))

(defcheck solution-cfc7e6ed
  (fn [s]
    (clojure.string/join
      (filter #(re-find #"[A-Z]" (str %)) s))))

(defcheck solution-d259afa0
  (fn [s]
    (clojure.string/join (filter #(upper-case? %) s))))

(defcheck solution-d3657d39
  #(apply str (re-seq #"[A-Z]+" %)))

(defcheck solution-d3f6e7d6
  (fn [s]
    (apply str (filter #(contains? (set "ABCDEFGHIJKLMNOPQRSTVWUXYZ") %) s))))

(defcheck solution-d4948c38
  #(apply str (filter (apply hash-set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") %)))

(defcheck solution-d4f791b6
  (fn [seq]
    (apply str (filter #(upper-case? %) seq))
    ))

(defcheck solution-d6f1ad0
  (fn uppity[s]
    (apply str (filter #(upper-case? %) (seq s)))))

(defcheck solution-d7085e0f
  (fn get-the-caps [s]
    (apply str (re-seq #"[A-Z]" s))))

(defcheck solution-d70aebc9
  (fn [chars] (apply str (filter #(upper-case? %) chars))))

(defcheck solution-d8118ad0
  (comp (partial apply str) (partial re-seq #"[A-Z]+")))

(defcheck solution-d9a0407b
  #(apply str (re-seq #"[A-Z]" %) ))

(defcheck solution-dc2adcd9
  (fn [cs] (apply str (filter #(upper-case? %) cs))))

(defcheck solution-dc6cfd8f
  #(clojure.string/join (re-seq #"[A-Z]" %1)))

(defcheck solution-ddb725c3
  (fn [word] (apply str (re-seq #"[A-Z]+" word))))

(defcheck solution-de09833
  (fn [x]
    (apply str (re-seq #"[A-Z]" x))))

(defcheck solution-de7ffd92
  (fn [string]
    (apply str (filter (fn [char]
                         (some #(= char %)
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                 string))))

(defcheck solution-df242d2
  (fn [s]
    (clojure.string/join (re-seq #"[A-Z]" s))))

(defcheck solution-e0e69e2f
  (fn [a] (clojure.string/join (filter #(upper-case? %) (seq a)))))

(defcheck solution-e0e7b28f
  (comp (partial apply str) (partial filter #(upper-case? %))))

(defcheck solution-e3a0e0de
  (fn [word]
    (apply str (map
                 (fn [x y]
                   (if (= x y)
                     ""
                     x))
                 word
                 (clojure.string/lower-case word)))))

(defcheck solution-e4c2d69f
  (fn [mystr] (apply str (re-seq #"[A-Z]+" mystr))))

(defcheck solution-e4ce17b8
  (fn [l]
    (apply str (filter #(upper-case? %) l))))

(defcheck solution-e4e4723a
  (fn [x] (apply str (filter (set "QWERTYUIOPASDFGHJKLZXCVBNMZ") x))))

(defcheck solution-e59981b0
  (fn [string]
    (let [cap (map char (range 65 91))]
      (clojure.string/join ""
        (filter #(not= (.indexOf cap %) -1) string)))))

(defcheck solution-e6bad856
  (fn [s]
    (apply str
      (filter #(upper-case? %) (seq s)))))

(defcheck solution-e72caf29
  (fn get_the_caps [x] (clojure.string/replace x #"[^A-Z]" "")))

(defcheck solution-e8e13910
  (fn [s]
    (apply str (filter #(contains? (set (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) %) (seq s)))))

(defcheck solution-ea4b7bfc
  (fn [s]
    (->> s
      (re-seq #"[A-Z]+")
      (clojure.string/join ""))))

(defcheck solution-ea901d45
  (fn [s] (reduce str (filter #(and (>= (compare % \A) 0) (<= (compare % \Z) 0)) s))))

(defcheck solution-ebab957a
  (comp (partial apply str) (partial re-seq #"[A-Z]")))

(defcheck solution-ebca299f
  #(clojure.string/replace % #"[^A-Z]" "" ))

(defcheck solution-ebd9ff10
  (fn get-caps [letters]
    (apply str (re-seq #"[A-Z]" letters))))

(defcheck solution-ecaab853
  (fn [t] (apply str (re-seq #"[A-Z]+" t))))

(defcheck solution-ed03ec89
  #(->> % (filter (fn [c] (upper-case? c))) (apply str)))

(defcheck solution-ed9afd3f
  (comp #(apply str %) #(filter (fn[x] (upper-case? x)) %)))

(defcheck solution-eda73649
  (fn caps-reseq
    [s] {:pre [(string? s)]}
    (->> s
      (re-seq #"[A-Z]")
      (apply str))))

(defcheck solution-ee00de57
  (fn ff [x] (apply str (filter #(upper-case? %) x))))

(defcheck solution-ee66b98a
  (fn [coll] (apply str (map #(if (and (>= (compare % \A) 0)
                                    (<= (compare % \Z) 0)) % "") coll))))

(defcheck solution-eead8376
  (fn [string]
    (apply str (filter #(upper-case? %) (seq string)))))

(defcheck solution-eebffd50
  (fn justupper [input] (apply str (re-seq #"[A-Z]" input))))

(defcheck solution-ef9e9127
  #(clojure.string/join "" (clojure.string/split % #"[^A-Z]")))

(defcheck solution-efc71a41
  (fn [st] (apply str (re-seq #"[A-Z]+" st))))

(defcheck solution-f0e305f4
  (comp (partial apply str)
    (partial re-seq #"[A-Z]")))

(defcheck solution-f21aa4a4
  (fn [s]
    (apply str
      (map #(first %)
        (filter #(not= (first %) (last %))
          (map (fn [x y] [x y])
            s (.toLowerCase s)))))))

(defcheck solution-f4162a3c
  (fn [x]
    (clojure.string/join (re-seq #"[A-Z]" x))))

(defcheck solution-f41f569c
  (fn [s] (apply str  (re-seq #"[A-Z]+" s))
    ))

(defcheck solution-f446479b
  #(clojure.string/join (filter (fn [c] (re-matches #"[A-Z]" (str c))) %)))

(defcheck solution-f65587ea
  #(reduce str (filter (comp (partial re-matches #"[A-Z]") str) %)))

(defcheck solution-f68cde7b
  (fn [s]
    (clojure.string/replace s #"[^A-Z]" "")
    ))

(defcheck solution-f70dbd7
  (fn [s] (#(apply str (re-seq #"[A-Z]" %)) s)))

(defcheck solution-f79e7aaa
  (fn getcap [xstr] (apply str (re-seq #"[A-Z]+" xstr))))

(defcheck solution-f8904ebc
  (fn my-caps [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-f891b8bf
  (fn [x] (apply str (filter #(re-matches #"[A-Z]" (str %)) x))))

(defcheck solution-f98553d
  (fn [x]
    (apply str
      (filter #(upper-case? %) x))))

(defcheck solution-f9994d5d
  (fn
    [x]
    (apply str (re-seq #"[A-Z]+" x))
    ))

(defcheck solution-fa3ba801
  (fn [n]
    (apply str (filter upper-case? n))))

(defcheck solution-fb558b13
  (fn [string] (apply str (filter #(upper-case? %) string))))

(defcheck solution-fc0d4ecb
  (fn [x]
    (clojure.string/join
      (filter (fn [c] (upper-case? c)) x))))

(defcheck solution-fc978be7
  #(apply str
     (filter ( fn [x] (upper-case? x))
       (map char %))))

(defcheck solution-fc991a55
  (fn [s] (clojure.string/replace s #"[^A-Z]", "")))

(defcheck solution-fd0fc5d6
  (fn [x] (->> (seq x) (filter #(not= % (first (clojure.string/lower-case %)))) (apply str))))

(defcheck solution-fd97a1bc
  #(apply str (map (fn [a b] (if (not (= a b)) b "")) (.toLowerCase %) %)))

(defcheck solution-fdb019fb
  (fn [text] (apply str (filter #(upper-case? %) text))))

(defcheck solution-fdd37530
  (fn [x] (clojure.string/join (re-seq #"[A-Z]" x))))

(defcheck solution-feeabeaa
  (fn [s]
    (reduce (fn [e c]
              (if (re-find #"[A-Z]" (str c))
                (str e c)
                e)) "" s)))

(defcheck solution-ff71a1fd
  (fn [s] (clojure.string/join (re-seq #"[A-Z]" s))))

(defcheck solution-ff9e0674
  (fn [s]
    (clojure.string/join "" (re-seq #"[A-Z]" s))))

(defcheck solution-ffb27e1d
  (fn [s]
    (apply str (filter #(upper-case? %) s))))

(defcheck solution-ffef3858
  (fn [string]
    (apply str (filter #(upper-case? %) string))))