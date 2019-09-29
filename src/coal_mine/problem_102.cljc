(ns coal-mine.problem-102
  (:require [coal-mine.checks :refer [defcheck-102] :rename {defcheck-102 defcheck}]
            [clojure.pprint]
            [clojure.test]))

(defn char-upper-case [c]
  #?(:clj  (Character/toUpperCase c)
     :cljs (clojure.string/upper-case c)))

(defcheck solution-11003f4e
  (fn camel-case [s]
    (let [words (clojure.string/split s #"-")]
      (if (= 1 (count words)) (first words)
                              (clojure.string/join ""
                                (cons (first words)
                                  (map #(str (-> % first str clojure.string/upper-case) (.substring % 1)) (rest words))
                                  )
                                )
                              )

      )

    ))

(defcheck solution-111e9e25
  (fn [s]
    (let [words (map #(apply str %) (remove #(= [\-] %) (partition-by #(= \- %) s)))]
      (apply str (first words) (map #(str (first (clojure.string/upper-case %)) (apply str (rest %))) (rest words))))))

(defcheck solution-11c9f12a
  (fn [s]
    (clojure.string/replace
      s
      #"-."
      #(->> (second %)
         char-upper-case
         str))))

(defcheck solution-11da9fb
  (fn [s] (apply str (reverse (reduce #(if (= \- (first %1)) (conj (rest %1) (char-upper-case %2)) (conj %1 %2))  '() s)))))

(defcheck solution-11fd0d4f
  (fn C [s]
    (let [x (clojure.string/split s #"-")]
      (apply str
        (cons (first x)
          (map clojure.string/capitalize (rest x)))))))

(defcheck solution-1208c43
  (fn [string]
    (clojure.string/join ""
      (reduce #(concat %1 (clojure.string/capitalize %2))
        (clojure.string/split string #"[^a-zA-Z]")))))

(defcheck solution-128e9403
  (fn [dashed-name]
    (let [parts (clojure.string/split dashed-name #"-")]
      (apply str
        (concat (take 1 parts)
          (map clojure.string/capitalize (rest parts)))))))

(defcheck solution-12be594
  (fn
    [s]
    (let [sq (re-seq #"\w+" s)
          toUpper (fn [w]
                    (let [f (first w)]
                      (str (clojure.string/upper-case (str f)) (apply str (rest w)))))]
      (reduce #(str % (toUpper %2)) (first sq)
        (rest sq)))))

(defcheck solution-12fb8122
  (fn [s] (apply str (mapcat #(cond (= %2 \-) (clojure.string/capitalize %1) (= %1 \-) nil :else (list %1)) s (cons \0 s)))))

(defcheck solution-1345b381
  (fn [s]
    (let [words (re-seq #"\w+" s)]
      (apply str (reduce
                   (fn [a b]
                     (if (empty? a)
                       (conj a b)
                       (conj a (apply str
                                 (first (clojure.string/upper-case b))
                                 (rest b)))))
                   [] words)))))

(defcheck solution-134ae12b
  (fn [s]
    (let [[fw & others] (.split s "-")]
      (apply str fw (map clojure.string/capitalize others)))))

(defcheck solution-13b0c8f5
  (fn [s]
    (let [splitted (clojure.string/split s #"-")]
      (apply str (first splitted) (map #(apply str (clojure.string/upper-case (first %)) (rest %)) (rest splitted))))))

(defcheck solution-13f72311
  (fn camel [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (first words)
        (map #(clojure.string/capitalize %) (rest words))))))

(defcheck solution-140212a2
  #(loop [in (seq %1)
          out []]
     (cond
       (empty? in) (apply str out)
       (= \- (first in)) (recur (drop 2 in) (conj out (char-upper-case (second in))))
       :else (recur (rest in) (conj out (first in))))))

(defcheck solution-149dede5
  (fn [w]
    (apply str
      (loop [s1 w, s2 []]
        (if (empty? s1) s2
                        (if (= \- (first s1))
                          (recur (rest (rest s1)) (conj s2 (clojure.string/upper-case (str (second s1))) ))
                          (recur (rest s1) (conj s2 (first s1) ))))))))

(defcheck solution-14a336e4
  #(case (count %) 12 "leaveMeAlone" 9 "something" "multiWordKey"))

(defcheck solution-14b758f7
  (fn [multiword]
    (let [words (clojure.string/split multiword #"-")
          cwords (map clojure.string/capitalize words)
          twords (cons (first words) (rest cwords))]
      (clojure.string/join twords))))

(defcheck solution-14dffe98
  (fn [s]
    (clojure.string/replace s #"-\w"
      (fn [s] (clojure.string/upper-case (subs s 1))))))

(defcheck solution-152237f6
  (fn f
    ([x] (f (seq x) ""))
    ([x res]
     (let [[ch1 ch2 & more] x]
       (cond (nil? ch1) res
             (nil? ch2) (str res ch1)
             (= ch1 \-) (f more (str res (char-upper-case ch2)))
             :else (f (conj more ch2) (str res ch1)))))))

(defcheck solution-152e5330
  (fn [s]
    (let [w (clojure.string/split s #"-")]
      (apply str (first w) (map #(apply str (cons (char-upper-case (first %)) (rest %))) (rest w)))
      )))

(defcheck solution-15bb98c5
  #(let [xs (clojure.string/split % #"-")]
     (apply str (cons (first xs) (map clojure.string/capitalize (rest xs))))))

(defcheck solution-15c5cd24
  (fn [s]
    (reduce
      (fn [st [m r]] (.replace st m (clojure.string/upper-case r)))
      s (re-seq #"-([a-z])" s))))

(defcheck solution-1617a876
  (fn [s] (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-162f3f24
  (fn [str] (clojure.string/replace str #"-(\w)" #(clojure.string/upper-case (% 1)))))

(defcheck solution-166e4715
  #(let [ws (clojure.string/split % #"-")] (clojure.string/join (into [(first ws)] (map clojure.string/capitalize (next ws))))))

(defcheck solution-166e595d
  (fn intoCamelCase [word] (clojure.string/replace word #"-(.)" #(clojure.string/upper-case (second %)))))

(defcheck solution-1713af62
  (fn [w]
    (let [[h & r] (clojure.string/split w #"-")]
      (clojure.string/join ""
        (cons h (map clojure.string/capitalize r))))))

(defcheck solution-17552cac
  (fn [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (second %)))))

(defcheck solution-185c4492
  #(reduce (fn [a [c & s]] (apply str `(~a ~(char-upper-case c) ~@s)))
     (.split % "-")))

(defcheck solution-18825c0e
  (fn [s] (let [offset-s (concat "a" s)]
            (apply str (filter #(not (= "-" %))
                         (map #(if (= \- %2) (clojure.string/upper-case (str %1)) (str %1)) s offset-s))))))

(defcheck solution-18beb763
  (fn camcase [s]
    (#(clojure.string/join [(first %)
                            (apply str
                              (map
                                clojure.string/capitalize (rest %)))])
     (clojure.string/split s #"-"))))

(defcheck solution-199e66ca
  (fn[s] (apply str ( filter
                      #(not(nil? %))
                      (map #(if (= %2 \-)
                              (clojure.string/capitalize %1)
                              (if (=  %1 \-) nil %1)
                              )
                        s (cons \> s)
                        )))))

(defcheck solution-19cabf85
  (fn [x](letfn[(split [s](re-seq #"\w+" s))
                (capitalize [s](apply str (clojure.string/upper-case (first s)) (rest s)))
                (result [s] (apply str (cons (first s) (map capitalize (rest s)))))]
           (apply str (result (split x))))))

(defcheck solution-19f6134d
  #(clojure.string/join (map-indexed (fn [i m] (if (= i 0) m (clojure.string/capitalize m))) (clojure.string/split % #"-"))))

(defcheck solution-1a6728ae
  (fn myf2 [s]
    (let [coll (clojure.string/split s #"-")]
      (->> (cons (first coll) (map clojure.string/capitalize (rest coll)))
        (apply str)

        ))))

(defcheck solution-1a716bd9
  (fn c [line]
    (reduce (fn [a [first-char & word]]
              (apply str a (clojure.string/upper-case (str first-char)) word))
      (.split line "-"))))

(defcheck solution-1af9169
  #(apply str ((fn peu [x] (if (empty? x) '() (if (= (first x) \-)  (conj (peu (apply str (rest (rest x)))) (second (clojure.string/upper-case x))) (conj (peu (apply str (rest x))) (first x))  ))) %)))

(defcheck solution-1b1aef8d
  (fn [s]
    (->>
      (str "a" s)
      (#(clojure.string/split % #"-"))
      (map (fn [w]
             (apply (partial str (clojure.string/upper-case (first w))) (rest w))))
      (clojure.string/join "")
      (rest)
      (apply str))))

(defcheck solution-1b2608da
  (fn [s]
    (->>
      (partition 2 1 (cons nil s))
      (map (fn [[prev char]]
             (if (= prev \-)
               (char-upper-case char)
               char)))
      (filter #(not= % \-))
      (apply str))))

(defcheck solution-1bc77375
  #(if(=(nth % 0)\m)"multiWordKey"%))

(defcheck solution-1c17600d
  #(let [s (re-seq #"\w+" %)]
     (apply str (cons (first s) (map (fn [w] (clojure.string/capitalize w)) (rest s))))))

(defcheck solution-1c8b1bd6
  (fn [s]
    (letfn [(ucFirst [[x & xs]] (apply str (concat (clojure.string/upper-case (str x)) xs)))]
      (let [[w & ws] (filter (fn [[x & xs]] (not= x \-)) (partition-by (fn [c] (= c \-)) s))]
        (apply str (concat [(apply str w)] (map ucFirst ws)))))))

(defcheck solution-1c9537ce
  (fn [w]
    (let [[h & t] (seq (.split w "-"))]
      (apply str h (map clojure.string/capitalize t)))))

(defcheck solution-1cf90c06
  (fn [w]
    (->> (re-seq #"\w+" w)
      (reduce #(str % (clojure.string/capitalize %2))))))

(defcheck solution-1d152286
  (fn [entrada]
    (let [x (clojure.string/split entrada #"-")]
      (str (first x) (clojure.string/join "" (map clojure.string/capitalize (rest x)))))))

(defcheck solution-1d36ec67
  (fn [s] (clojure.string/replace s
            #"-[a-z]"
            #(str (char-upper-case (second %))))))

(defcheck solution-1d63c6be
  (fn [s]
    (clojure.string/replace
      s
      #"-(\w)"
      #(clojure.string/upper-case (second %)))))

(defcheck solution-1d787824
  (fn intoCamelCase [s]
    (let [[fw & rw] (clojure.string/split s #"-")]
      (apply str (cons fw (map clojure.string/capitalize rw))))))

(defcheck solution-1dc7179c
  (fn hyphen-to-camel [text]
    (let [words (.split text "-")]
      (letfn [(upper-case-first [s] (apply str (cons (char-upper-case (first s)) (rest s))))]
        (clojure.string/join (cons (first words) (map upper-case-first (rest words))))))))

(defcheck solution-1e0ce31a
  (fn [s]
    (let [ p (filter #(not= %1 '(\-)) (partition-by (partial = \-) s))]
      (apply str (apply concat (first p) (map #(cons (clojure.string/upper-case (str (first %1))) (rest %1)) (rest p)))))))

(defcheck solution-1f152dde
  (fn camelize [string]
    (let [[f & r] (clojure.string/split string #"-")]
      (clojure.string/join (cons f (map clojure.string/capitalize r))))))

(defcheck solution-1f92a0fd
  (fn [s]
    (apply str
      (map #(if (< 1 (count %))
              (. (str (second %)) toUpperCase)
              %)
        (re-seq #"-?\w" s)))))

(defcheck solution-1fd32059
  (fn [s] (clojure.string/replace (clojure.string/replace s #"\-\w" clojure.string/upper-case) #"-" "")))

(defcheck solution-2017fbdf
  (fn
    [s]
    (let [fix-case (fn [s] (let [c (.charAt s 0)]
                             (.concat (str (char-upper-case c))
                               (.substring s 1))))
          ss (.split s "-")
          h (first ss)
          t' (map fix-case (rest ss))]
      (reduce #(.concat % %2) (cons h t')))))

(defcheck solution-201e15f1
  (fn [s]
    (clojure.string/replace s #"-[a-z]" (comp clojure.string/upper-case last))))

(defcheck solution-21781f7f
  (fn [s]
    (clojure.string/replace s
      #"-(\w)"
      #(clojure.string/upper-case (str (% 1))))))

(defcheck solution-219b73df
  (fn [s]
    (loop [ret []
           s s]
      (cond (empty? s)
            (apply str ret)

            (= (first s) \-)
            (let [x1 (first s)
                  x2 (second s)]
              (if (nil? x2)
                (apply str (conj ret x1))
                (recur (conj ret (clojure.string/upper-case x2))
                  (nnext s))))

            :else
            (recur (conj ret (first s))
              (next s))))))

(defcheck solution-220356cc
  (fn [s]
    (let [tokens (clojure.string/split s #"-")]
      (clojure.string/join "" (cons (first tokens) (map clojure.string/capitalize (next tokens)))))))

(defcheck solution-226f74ad
  (fn [s]
    (let [[w & wrest] (.split s "-")]
      (str w
        (apply str (map #(apply str (clojure.string/upper-case (first %)) (rest %)) wrest))))))

(defcheck solution-22c57322
  #(case (first %) \m "multiWordKey" %))

(defcheck solution-23b49689
  (fn camel [s]
    (->>
      (partition 2 1 (cons 0 s))
      (map (fn [[a b]] (if (= a \-) (char-upper-case b) b)))
      (remove #(= \- %))
      (apply str))))

(defcheck solution-24044000
  (fn [string]
    (let [words (clojure.string/split string #"-")
          f (first words)
          r (rest words)]
      (apply str f (map (fn [s]
                          (str (clojure.string/capitalize (subs s 0 1)) (subs s 1)))
                     r)))))

(defcheck solution-24927f9e
  #(clojure.string/replace % #"-(\w)" (fn [[a b]] (clojure.string/capitalize b))))

(defcheck solution-24f031d8
  (fn [s]
    (clojure.string/replace s
      #"-([a-z])"
      (fn [[_ a]] (clojure.string/upper-case a)))))

(defcheck solution-265792e2
  (fn
    [s]
    (apply str (first (reduce (fn [[acc cc] e]
                                (if (= e \-)
                                  [acc true]
                                  [(conj acc (if cc (char-upper-case e) e)) false]))
                        [[] false]
                        s)))))

(defcheck solution-2666accc
  (fn icc [words]
    (letfn [(cap-first [word]
              (clojure.string/join
                [ (clojure.string/upper-case (str (first word)))
                 (clojure.string/join (next word))]))]


      (let [ words (clojure.string/split words #"-")
            head (first words)
            tail (rest words)]
        (clojure.string/join (cons head (map cap-first tail)))))))

(defcheck solution-273348e6
  #(let [[w & u] (re-seq #"[^-]+" %)]
     (apply str w (map clojure.string/capitalize u))))

(defcheck solution-27aa753f
  #(let [[f & r] (.split % "-")]
     (apply str f
       (mapcat
         (fn [[f & r]] (concat [(char-upper-case f)] r))
         r))))

(defcheck solution-281f3b8e
  #(clojure.string/join ""
     (let [sp (clojure.string/split % #"\-")]
       (cons (first sp) (map clojure.string/capitalize (rest sp))))))

(defcheck solution-285d0cc3
  #(first (reduce (fn [[s h] c]
                    (cond (= c \-) [s true]
                          h [(str s (char-upper-case c)) false]
                          :else [(str s c) false])) ["" false] %)))

(defcheck solution-28b21860
  #(apply str (remove (fn [x] (= x \-)) (map (fn [[a b]] (if (= a \-) (char-upper-case b) b)) (partition-all 2 1 (concat [(first %)]%))))))

(defcheck solution-28b50be5
  #(apply str
     (let [[w & ws] (re-seq #"[a-zA-Z]+" %)]
       (cons w
         (mapcat (fn [[y & ys]] (cons (char-upper-case y) ys)) ws)))))


(defcheck solution-294a22ad
  (fn [string]
    (let [[x & xs] (re-seq #"[^-]+" string)]
      (apply str x (map clojure.string/capitalize xs)))))

(defcheck solution-295eaa68
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (cons (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-2969bed8
  (fn [s] (reduce #(clojure.string/join [% (clojure.string/capitalize %2)]) (#(clojure.string/split % #"-") s))))

(defcheck solution-29753f2e
  (fn [s]
    (clojure.string/replace s
      #"-."
      #(-> (second %) str clojure.string/upper-case))))

(defcheck solution-29805d8a
  #(let [words (clojure.string/split % #"-")]
     (str (first words)
       (apply str (map clojure.string/capitalize (drop 1 words))))))

(defcheck solution-29d1e14f
  #(let [[fs & rss] (clojure.string/split % #"-")]
     (clojure.string/join (cons fs (map clojure.string/capitalize rss)))))

(defcheck solution-29d99649
  #(clojure.string/replace % #"-\w" (fn [[_ w]] (clojure.string/upper-case w))))

(defcheck solution-29e13dc
  #(let [words (clojure.string/split % #"-")]
     (->> (first words)
       (conj (map clojure.string/capitalize (rest words)))
       (clojure.string/join ""))))

(defcheck solution-2a01402a
  #(let [[f & r] (re-seq #"\w+" %)]
     (apply str f
       (map clojure.string/capitalize r))))

(defcheck solution-2a80c76e
  (fn to-camel-case [input]
    (loop [was-dash false rem (vec input) res []]
      (let [fst-char (first rem)]
        (cond (empty? rem) (apply str res)
              was-dash (recur false (rest rem) (conj res (char-upper-case fst-char)))
              (= fst-char \-) (recur true (rest rem) res)
              :else (recur false (rest rem) (conj res fst-char)))))))

(defcheck solution-2abb546
  (fn [s]
    (reduce #(str % (-> %2 first str clojure.string/upper-case)
               (subs %2 1))
      (re-seq #"\w+" s))))

(defcheck solution-2ac432eb
  (fn intoCamelCase [s]
    [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-2b37fe08
  #(apply str
     (first (clojure.string/split % #"-"))
     (map clojure.string/capitalize
       (rest  (clojure.string/split % #"-")))))

(defcheck solution-2b4deca6
  (fn [s] (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-2b8c055d
  #(apply str
     (map (fn [[_ a b _]]
            (str a (and b (clojure.string/upper-case
                            (subs b 1)))))
       (re-seq #"([^-]+)(-.)?" %))))

(defcheck solution-2c0714ed
  #(let [words (clojure.string/split % #"-")]
     (str (first words)
       (apply str (map clojure.string/capitalize (rest words))))))

(defcheck solution-2c907b8e
  #(apply str (flatten
                (reduce (fn [[s p] c]
                          (if (= p "-")
                            [s (clojure.string/upper-case c)]
                            [(conj s p) c]))
                  [[] ""]
                  (map str (seq %))))))

(defcheck solution-2c99fca9
  (fn [s]
    (apply str
      (loop [s s r [] f false]
        (if (nil? s) r
                     (if
                      (= \- (first s))
                       (recur (next s) r true)
                       (recur
                         (next s)
                         (conj r
                           (if f
                             (char-upper-case (first s))
                             (first s)))
                         false)))))))

(defcheck solution-2cade1fa
  (fn [s]
    (let [ss (.split s "-")]
      (apply str
        (first ss)
        (map #(str (clojure.string/upper-case (.substring % 0 1)) (.substring % 1)) (rest ss))))))

(defcheck solution-2ccd768a
  (fn [S]
    ((fn [lst] (reduce
                 (fn [x y]
                   (str x (clojure.string/capitalize y)))
                 (first lst) (rest lst)))
     (clojure.string/split S #"-"))))

(defcheck solution-2ce2f73a
  #(let [ws (re-seq #"\w+" %)]
     (apply str (conj (map clojure.string/capitalize (next ws))
                  (first ws)))))

(defcheck solution-2d1c65f8
  (fn [w]
    (let [[fst & rst] (clojure.string/split w #"-")]
      (str fst (apply str (map clojure.string/capitalize rst))))))

(defcheck solution-2da3517c
  (fn into-camel[a-str]
    (let [splitted (clojure.string/split a-str #"-") size (count splitted)]
      (if (> size 1)
        (let [f (first splitted) r (rest splitted)]
          (clojure.string/join (cons f (map  clojure.string/capitalize r)))
          )
        a-str
        )
      )
    ))

(defcheck solution-2dcc1173
  (fn [s]
    (let [w (clojure.string/split s #"\-")
          f (first w)
          r (rest w)]
      (apply str
        (cons f
          (map
            (fn [x] (let [[h & t] x]
                      (apply str (cons (char-upper-case h) t))))
            r))))))

(defcheck solution-2e1f0bba
  (fn [s]
    (->> (clojure.string/split s #"-")
      (#(conj (map clojure.string/capitalize (rest %)) (first %)))
      (clojure.string/join))))

(defcheck solution-2e80f66
  (fn [string]
    (let [v (clojure.string/split string #"-")]
      (if (= (count v) 1)
        string
        (let [firs (first v)
              rst (map #(clojure.string/capitalize %) (rest v))]
          (str firs (apply str rst)))))))

(defcheck solution-2ea0473a
  (fn [x]
    (let [s (clojure.string/split x #"-")]
      (if (> (count s) 1)
        (clojure.string/join (cons (first s) (rest (map clojure.string/capitalize s))))
        x))))

(defcheck solution-2ea89196
  #(let [[f & r] (clojure.string/split % #"-")]
     (->> r
       (map clojure.string/capitalize)
       (apply str f))))

(defcheck solution-2eecdf2
  (fn my-camel-case
    [phrase]
    (let [capitalize (fn [word]
                       (let [letters (seq word)]
                         (apply str
                           (clojure.string/upper-case (str (first letters)))
                           (rest letters))))
          split-words (fn [phrase]
                        (clojure.string/split phrase #"-"))
          make-camel (fn [words]
                       (conj
                         (map capitalize (rest words))
                         (first words)))]
      (->>
        phrase
        (split-words)
        (make-camel)
        (apply str)))))

(defcheck solution-2ef25012
  (fn foo [w]
    (apply str
      (loop [ls (seq w) r []]
        (if ls
          (if (= \- (first ls))
            (recur (cons (char-upper-case (second ls)) (next (next ls))) r)
            (recur (next ls) (conj r (first ls))))
          r)))))

(defcheck solution-2ef3800a
  (fn [s]
    (loop [s (vec s)
           r []
           dash? false]
      (cond
        (not (seq s)) (apply str r)
        (= \- (first s)) (recur (rest s) r true)
        dash? (recur (rest s) (conj r (char-upper-case (first s))) false)
        :else (recur (rest s) (conj r (first s)) false)))))

(defcheck solution-2f741e06
  (fn [s]
    (clojure.string/replace  s #"-[a-zA-Z]"
      #(apply str (rest (clojure.string/upper-case %1))))))

(defcheck solution-2fc3bb03
  #(let [[x & xs] (clojure.string/split % #"-")]
     (apply str x (map clojure.string/capitalize xs))))

(defcheck solution-2fe56221
  (fn [s]
    (let [splitted (clojure.string/split s #"\-")]
      (if (= (count splitted) 1)
        (first splitted)
        (str (first splitted)
          (apply str (map #(reduce (fn [a b] (str a b))
                             (-> (first %) str clojure.string/upper-case)
                             (rest %))
                       (rest splitted))))))))

(defcheck solution-301977d3
  (fn [s] (clojure.string/replace s #"-\w" #(clojure.string/upper-case (.substring % 1)))))

(defcheck solution-3022541c
  (fn [hyph-str]
    (apply str (loop [result [] coll (seq hyph-str)]
                 (if (zero? (count coll))
                   result
                   (recur
                     (if (= \- (first coll))
                       result
                       (conj result (first coll))
                       )
                     (if (= \- (first coll))
                       (cons (clojure.string/upper-case (str (first (rest coll)))) (rest (rest coll)))
                       (rest coll)
                       )
                     )       )     )   )))

(defcheck solution-30592e6a
  (fn [ls] (let [tg (clojure.string/split ls #"-")] (str (first tg) (apply str (map #(apply str (concat (clojure.string/upper-case (first %)) (rest %))) (rest tg)))))))

(defcheck solution-30e601dc
  #(clojure.string/replace % #"-([a-z])" (comp clojure.string/upper-case second)))

(defcheck solution-31b519d8
  (fn [s]
    (let [x (.split s "-")]
      (apply str (cons (first x) (map clojure.string/capitalize (rest x)))))))

(defcheck solution-31d3a40f
  (fn [s]
    (let [ss (re-seq #"\w+" s)]
      (clojure.string/join
        ""
        (cons (first ss)
          (map clojure.string/capitalize (rest ss)))))))

(defcheck solution-3240a476
  #(first
     (reduce (fn [[a b] c]
               (cond
                 (= \- c) [a true]
                 b [(str a (char-upper-case c)) nil]
                 "" [(str a c) nil])) [] %)))

(defcheck solution-32c07ae9
  (fn to-camelcase [s]
    (let [capitalize (fn [s]
                       (apply str
                         (cons (char-upper-case (first (seq s)))
                           (rest (seq s)))))
          string-list (seq (.split s "-"))]
      (apply str
        (first string-list)
        (map capitalize (rest string-list))))))

(defcheck solution-331692b
  (fn [s]
    (clojure.string/replace
      s #"-\w" #(clojure.string/upper-case (str (second %))))))

(defcheck solution-33ca91a5
  (fn  [x]
    (clojure.string/replace x #"-\w" #(clojure.string/upper-case (second %1)))))

(defcheck solution-34563a2d
  (fn [input]
    (let [parts (clojure.string/split input #"-")
          transformed-parts (cons (first parts) (map (fn [word]
                                                       (apply str (cons (char-upper-case (first word)) (rest word))))
                                                  (rest parts)))]
      (apply str transformed-parts))))

(defcheck solution-345898cb
  (fn[x] (clojure.string/replace x #"-([a-zA-Z])" #(clojure.string/upper-case (second %)))))

(defcheck solution-3459ca40
  (fn into-camel-case [s]
    (let [capitalize (fn [s] (apply str (cons (clojure.string/upper-case (first (map str s))) (rest (map str s)))))]
      (apply str (cons (first (re-seq #"\w+" s)) (map capitalize (rest (re-seq #"\w+" s))))))))

(defcheck solution-34a589b2
  #((fn [acc [s & ss]]
      (if (empty? ss)
        (apply str (conj acc s))
        (if (= s \-)
          (recur (conj acc (clojure.string/upper-case (first ss))) (rest ss))
          (recur (conj acc s) ss)))) [] %))

(defcheck solution-34b92df4
  (fn my-into-camel-case
    [word]
    (reduce #(apply str %1 (char-upper-case (first %2)) (rest %2)) (re-seq #"\w+" word))))

(defcheck solution-34bf145d
  (fn [a]
    (let [spondash (clojure.string/split a #"-")
          hasdash? (> (count spondash) 1)
          wordsforcamel (fn [b] (map #(clojure.string/capitalize (nth b %)) (range 1 (count b))))]
      (if hasdash?
        (clojure.string/join (concat [(nth spondash 0)] (wordsforcamel spondash)))
        a))))

(defcheck solution-34ca12e6
  (fn [a]
    (let [res (clojure.string/split a #"-")
          firstWord (first res)]
      (clojure.string/join (concat
                             (clojure.string/lower-case (first firstWord))
                             (rest firstWord)
                             (map clojure.string/capitalize (rest res)))))))

(defcheck solution-34ee7567
  (fn [s] (reduce #(str % (clojure.string/capitalize %2)) (clojure.string/split s #"-"))))

(defcheck solution-3500952d
  (fn [s]
    (clojure.string/replace s #"-\w" #(clojure.string/upper-case (subs % 1)))))

(defcheck solution-35173614
  (fn __ [st]
    (let [
          splitted (clojure.string/split st #"\-")
          upcased (cons (first splitted) (map clojure.string/capitalize (rest splitted)))
          ]
      (clojure.string/join upcased))
    ))

(defcheck solution-352419d2
  (fn [s]
    (let [[x & xs] (.split s "-")]
      (apply str (cons x (map #(clojure.string/capitalize %) xs))))))

(defcheck solution-359cb423
  (fn [s] (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (% 1)))))

(defcheck solution-3611e583
  (letfn [
          (capitalize [s]
            (str (char-upper-case (.charAt s 0)) (subs s 1)))]

    (fn to-camel-case [s]
      (let [[w1 & ws] (seq (.split s "-"))]
        (apply str w1 (map capitalize ws))))))

(defcheck solution-366f2fe8
  (fn [word]
    (let [[fst & rst] (clojure.string/split word #"-")]
      (apply str fst (map clojure.string/capitalize rst)))))

(defcheck solution-36727caa
  #(let [xs (re-seq #"[^-]+" %)]
     (apply str
       (cons (first xs) (map (fn [x]
                               (apply str
                                 (clojure.string/upper-case (str (first x)))
                                 (rest x)))
                          (rest xs))))))

(defcheck solution-3680a897
  (fn[s]
    (apply str
      (map #(str (second %) (if (not (nil? (nth % 2)))(clojure.string/upper-case  (second (nth % 2)))))
        (re-seq #"(\w+)(-\w)?" s))
      )
    ))

(defcheck solution-3784599b
  #(clojure.string/replace % #"-([a-z])" (fn [m] (clojure.string/upper-case (m 1)))))

(defcheck solution-379b5365
  (fn [s]
    (->> s
      (re-seq #"\w+")
      (reduce
        #(concat %1 [(if (empty? %1)
                       (first %2)
                       (char-upper-case (first %2)))] (rest %2)))
      (apply str))))

(defcheck solution-37c9a144
  (fn a [s]
    (let [words (clojure.string/split s #"-")
          camels (into [(first words)] (map clojure.string/capitalize (rest words)))]
      (clojure.string/join "" camels)
      )))

(defcheck solution-37f1b482
  (fn to-camel-case [word]
    (apply str
      (reduce
        (fn [a b]
          (if (= (last a) \-)
            (conj (vec (butlast a)) (clojure.string/upper-case b))
            (conj a b))) [] word))))

(defcheck solution-380d8c9e
  #(reduce
     (fn [s [from to]]
       (.replace s from (clojure.string/upper-case to)))
     %
     (re-seq #"-(\w)" %)))

(defcheck solution-3887df5
  #(let [p (clojure.string/split % #"\-")]
     (apply str
       (first p) (map clojure.string/capitalize (rest p)))))

(defcheck solution-38aac3a9
  (fn [s]
    (apply str
      (remove #(= % \-)
        (map #(if (= % \-) (char-upper-case %2) %2)
          (cons 0 s) s)))))

(defcheck solution-38cd45c3
  (fn [w]
    (let [split clojure.string/split
          spt-w (split w #"-")]
      (if (= (count spt-w) 1)
        w
        (let [cap #(apply str (cons (char-upper-case (first %)) (rest %)))]
          (apply str (cons (first spt-w)
                       (map cap (rest spt-w)))))))))

(defcheck solution-390c262d
  #(let [xs (clojure.string/split % #"-")]
     (clojure.string/join
       (cons (first xs)
         (map clojure.string/capitalize (rest xs))))))

(defcheck solution-393ae359
  (fn [s]
    (let [strings (clojure.string/split s #"-")]
      (clojure.string/join
        (cons (first strings) (map clojure.string/capitalize (rest strings)))))
    ))

(defcheck solution-393e7f99
  (fn [s]
    (reduce
      #(str % (clojure.string/capitalize %2))
      (.split s "-"))))

(defcheck solution-39aca7f4
  (fn intoCamelCase [s]
    (let [sp (clojure.string/split s #"-")]
      (apply str (cons (first sp) (map #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1)) (rest sp))) ))))

(defcheck solution-39f7cc01
  (fn [astr]
    (let [aseq (seq astr)
          hyphnums (for [x (range (- (count aseq) 1))
                         :when (= \- (nth aseq x))]
                     (+ x 1))
          capseq (for [x (range (count aseq))]
                   (if (nil? (some #(= x %) hyphnums))
                     (str (nth aseq x))
                     (clojure.string/upper-case (nth aseq x))))
          nohyphs (filter #(not= "-" %) capseq)]
      (clojure.string/join nohyphs))))

(defcheck solution-3a22e7f7
  #(let [[h & r] (clojure.string/split % #"-")]
     (str h (clojure.string/join "" (map clojure.string/capitalize r)))))

(defcheck solution-3a77449
  (fn [s](reduce #(str % (clojure.string/capitalize %2)) (clojure.string/split s #"-"))))

(defcheck solution-3a91ef9d
  (fn [s]
    (apply str
      (loop [m s
             r []
             p false]
        (if (empty? m)
          r
          (if p
            (recur (rest m) (conj r (char-upper-case (first m))) false)
            (if (= (first m) \-)
              (recur (rest m) r true)
              (recur (rest m) (conj r (first m)) false))))))))

(defcheck solution-3aa14772
  (fn [s]
    (clojure.string/replace s #"-[a-z]" #(clojure.string/upper-case (apply str (rest %))))))

(defcheck solution-3b2d62e0
  (fn [s]
    (let [parts (.split s "-")]
      (str (first	parts) (apply str (map clojure.string/capitalize (rest parts)))))))

(defcheck solution-3b5119ae
  (fn [s]
    (clojure.string/join ""
      (reduce (fn [acc newchar]
                (if (= \- (last acc)) (conj (into [] (butlast acc)) (clojure.string/upper-case newchar))
                                      (conj acc newchar))) [] (into [] s)))))

(defcheck solution-3b6f53c6
  (fn [s] (let [ss (clojure.string/split s #"-")]
            (clojure.string/join (cons (first ss) (map clojure.string/capitalize (rest ss)))))))

(defcheck solution-3bb95fb9
  #(let [[x & xs] (clojure.string/split % #"-")] (apply str (cons x (map clojure.string/capitalize xs)))))

(defcheck solution-3becec37
  (fn [s] (clojure.string/replace s #"-([a-z])" #(-> % second clojure.string/upper-case))))

(defcheck solution-3bf536ef
  (fn [st]
    (let [the-split (clojure.string/split st #"-")
          first-word (first the-split)
          red-fn #(conj %1
                    (apply str
                      (cons
                        (char-upper-case (first %2))
                        (rest %2))))
          capped (reduce red-fn [] (rest the-split))]
      (clojure.string/join (cons first-word capped)))))

(defcheck solution-3c53b193
  (fn [w]
    (let [[f & r] (clojure.string/split w #"-")]
      (apply str (cons f (map clojure.string/capitalize r))))))

(defcheck solution-3d495740
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str (first words)
        (mapcat #(cons (clojure.string/upper-case (first %)) (rest %)) (rest words))))))

(defcheck solution-3d7a5398
  #(let [s (re-seq #"[a-zA-Z]+" %)] (apply str (cons (first s) (map clojure.string/capitalize (rest s))))))

(defcheck solution-3dc82a3
  (fn [s]
    (clojure.string/join
      (let [words (clojure.string/split s #"-")]
        (cons (first words) (map #(clojure.string/capitalize %) (rest words)))))))

(defcheck solution-3e886b10
  (fn [hyphenated]
    (reduce (fn [camel word]
              (str camel (clojure.string/capitalize word)))
      (re-seq #"\w+" hyphenated))))

(defcheck solution-3eab3f57
  (fn [a]
    (reduce #(if (= \- %2) % (str % %2))
      (reduce #(assoc % (+ 1 %2) (clojure.string/upper-case (.toString (get % (+ 1 %2))))
                        )
        (vec a)
        (filter (comp not nil?) (keep-indexed (fn [i v] (if (= v \-) i nil)) (vec a)))))))

(defcheck solution-3f2942c1
  (fn [s]
    (let [tmps (clojure.string/split s #"-")]
      (clojure.string/join "" (conj (map clojure.string/capitalize (rest tmps)) (first tmps))))))

(defcheck solution-3fc8ec5e
  (fn to-camelcase [st]
    (let [splited (clojure.string/split st #"-")
          stf (first splited)
          r (rest splited)
          first-capitalize (fn [[x & xs]] (cons (char-upper-case x) xs))]
      (->> r
        (map (comp #(apply str %) first-capitalize))
        (cons stf)
        (apply str)))))

(defcheck solution-3fd7704f
  #(clojure.string/replace % #"-(.)" (fn [[_ m]] (clojure.string/upper-case m))))

(defcheck solution-3fe27d70
  (fn camelize [s]
    (let [svec (clojure.string/split s #"-")]
      (reduce #(str % (clojure.string/upper-case (subs %2 0 1)) (subs %2 1))
        (first svec) (rest svec)))))

(defcheck solution-400ea10a
  (fn icc [s]
    (->> (clojure.string/split s #"-+")
      (map-indexed #(if (< 0 %1) (clojure.string/capitalize %2) %2))
      (apply str)
      )))

(defcheck solution-40b58849
  (fn [s] (clojure.string/replace s #"-[a-z]" #(subs (clojure.string/upper-case %) 1))))

(defcheck solution-4105bd9
  (fn [s]
    (apply str
      (filter
        (comp not #{\-})
        (map #(if (= \- %2) (clojure.string/upper-case (str %1))  %1)
          s
          (cons nil s))))))

(defcheck solution-410c3f97
  (fn [s]
    (let [[h & t] (seq (.split s "-"))]
      (apply str (cons h (map clojure.string/capitalize t))))))

(defcheck solution-414886f4
  (fn [s]
    (let [[f & w] (.split s "-")]
      (apply str f (map clojure.string/capitalize w)))))

(defcheck solution-414e01db
  (fn [s]
    (let [parts (clojure.string/split s #"-")]
      (apply str (concat (first parts) (map clojure.string/capitalize (rest parts))))
      )
    ))

(defcheck solution-41ab66d3
  (fn n102 [s]
    (let [t (clojure.string/split s #"-")]
      (apply str (cons (first t) (map clojure.string/capitalize (rest t)))))))

(defcheck solution-41ff5877
  (fn [s]
    ((reduce
       (fn [f c]
         (if (= c \-)
           #(str (f (char-upper-case %)))
           #(str (f c) %)))
       identity
       s) "")))

(defcheck solution-4222b785
  (fn [s]
    (let [[p & ps] (re-seq #"\w+" s)]
      (apply str p (map #(apply str (clojure.string/upper-case (str (first %))) (rest %)) ps)))))

(defcheck solution-42346de5
  (fn [h-str]
    (letfn[(first-up[a-word]
             (let [splt (map #(apply str %) (split-at 1 a-word))]
               (str (clojure.string/upper-case (first splt)) (second splt))))]
      (let [parts (remove #(= "-" %) (map #(apply str %) (partition-by #(not= \- %) h-str)))]
        (apply str (cons (first parts) (map first-up (rest parts))))))))

(defcheck solution-42945bfc
  (fn intoCamelCase [www]
    (let [words (re-seq #"\w+" www)]
      (reduce

        (fn [a x]
          (let [c (first x)
                cu (clojure.string/upper-case c)]
            (if (= c cu) (str a x) (reduce str (str a cu) (rest x)))

            ))

        (first words)
        (rest words)))))

(defcheck solution-42d3ce9e
  #(let [s (re-seq #"\w+" %)]
     (apply str (first s) (map clojure.string/capitalize (rest s)))))

(defcheck solution-4306e676
  (fn [s]
    (->>
      (clojure.string/split s #"-")
      (map-indexed (fn [n s] (if (> n 0) (clojure.string/capitalize s) s)))
      (reduce concat)
      (apply str))
    ))

(defcheck solution-4317d685
  (fn [s]
    (->> s
      (#(clojure.string/split %  #"-"))
      (#(cons (first %) (map clojure.string/capitalize (rest %))))
      (apply str))))

(defcheck solution-43295b68
  (fn cC [s] (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (second %1))) ))

(defcheck solution-4376ac4c
  (fn to-camel-case [s]
    (let [[f & n] (clojure.string/split s #"-")]
      (clojure.string/join (cons f (map clojure.string/capitalize n))))))

(defcheck solution-43982185
  (fn [in]
    (let [words (clojure.string/split in #"-")]
      (apply str (cons (first words) (for [x (seq (rest words))] (clojure.string/capitalize x)))))))

(defcheck solution-442408ba
  (fn [s]
    (clojure.string/replace s #"-(\w)" (fn [[a b]] (clojure.string/capitalize b)))))

(defcheck solution-447459b3
  #(apply str ((juxt first (comp (partial apply str) (partial map clojure.string/capitalize) rest)) (.split % "-"))))

(defcheck solution-44bbdb85
  (fn camelize [s]
    (let [words (clojure.string/split s #"-")
          upcase-words (map #(let [first-l (clojure.string/upper-case (str (first %)))]
                               (str first-l (subs % 1))) words)]
      (clojure.string/join (concat (first words) (drop 1 upcase-words))))))

(defcheck solution-44f697c7
  (fn [s] (clojure.string/replace
            s
            #"-(.)"
            #(clojure.string/upper-case (second %)))))

(defcheck solution-457c776
  #(let [[f & r](clojure.string/split % #"-")]
     (clojure.string/join "" (cons f (map clojure.string/capitalize r)))))

(defcheck solution-459a27f3
  (fn [word]
    (let [words (clojure.string/split word #"-")]
      (apply str (first words) (map clojure.string/capitalize (rest words))))))

(defcheck solution-45f8cb06
  (fn [s]
    (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-4661b08f
  #(let [w (re-seq #"\w+" %)] (apply str (first w) (map clojure.string/capitalize (next w)))))

(defcheck solution-46fc586c
  (fn [-string]
    (let [s (clojure.string/split -string  #"-")]
      (if (= 1 (count s))
        -string
        (reduce #(str  % (clojure.string/capitalize %2)) (first s) (next s))
        ))
    ))

(defcheck solution-47bc04e4
  (fn [text]
    (let [tokens (clojure.string/split text #"-")]
      (apply str (concat [(first tokens)] (map #(clojure.string/capitalize %) (rest tokens))))
      )
    ))

(defcheck solution-47c136b5
  (fn to-camel [strn]
    (let [splitted (clojure.string/split strn #"-")]
      (apply str
        (cons (first splitted)
          (map clojure.string/capitalize (rest splitted)))))))

(defcheck solution-483b8614
  #(let [[[w _ _] & r] (re-seq #"(\w)(\w+)" %)]
     (apply str w (map (fn [[_ c r]] (str (clojure.string/upper-case c) r)) r))))

(defcheck solution-486e9050
  (fn [s]
    (let [tokens (re-seq #"[A-Za-z]+" s)]
      (clojure.string/join (cons (first tokens) (map clojure.string/capitalize (rest tokens)))))))

(defcheck solution-4872f3c3
  (fn [string]
    (clojure.string/replace
      string
      #"-\w+"
      #(clojure.string/capitalize
         (apply str (rest %))))))

(defcheck solution-48bb0d8b
  #(clojure.string/replace % #"-(\w)" (fn[[a b]] (clojure.string/upper-case b)) ))

(defcheck solution-48ecd23f
  #(reduce (fn [s [c & r]] (apply str s (clojure.string/upper-case (str c)) r)) (re-seq #"\w+" %)))

(defcheck solution-4921515a
  (fn camelcase
    [[x & xs]]
    (cond
      (= x \-) (str (clojure.string/upper-case (first xs)) (camelcase (rest xs)))
      x (str x (camelcase xs))
      :else "")))

(defcheck solution-499887e7
  (fn [varname]  (let [ splitted   (clojure.string/split varname #"-")
                       camelize   #(apply str (concat (clojure.string/upper-case (str (first %))) (rest %)))
                       camelized  (map camelize (rest splitted) ) ]
                   (apply str (first splitted) camelized))))

(defcheck solution-49af1965
  (fn into-camel-case [s]
    (let [ret (re-seq #"[^-]+" s)]
      (if (= (count ret) 1)
        s
        (apply str
          (first ret)
          (map clojure.string/capitalize (rest ret)))))))

(defcheck solution-49c78fc6
  (fn  [st]
    (let [[part & rparts :as parts] (clojure.string/split st #"-")]
      (cond (not (seq rparts)) st
            (not (every? #(re-matches #"[a-z]+" %) parts)) st
            :else (apply str part (map clojure.string/capitalize rparts))))))

(defcheck solution-4a418d85
  (fn [x] (clojure.string/replace x #"-[a-z]"  #(str (char-upper-case (last %))))))

(defcheck solution-4af9de3a
  #(let [l (re-seq #"([A-Za-z])([a-z]*)" %) ]  (apply str ((first l) 0) (map (fn [[_ fw rw]] (str (clojure.string/upper-case fw) rw)) (rest l)))))

(defcheck solution-4b31635f
  (fn [w] (clojure.string/replace w #"-(\w)" #(clojure.string/upper-case (second %)))))

(defcheck solution-4bcbdc8b
  (fn
    [s]
    (let [w (seq (.split s "-"))
          a (first w)
          r (next w)]
      (apply str (cons a
                   (map
                     #(let [c (seq %)]
                        (apply str
                          (cons
                            (char-upper-case (first c))
                            (rest c))))
                     r))))))

(defcheck solution-4c6e80f0
  #(clojure.string/replace % #"-(.)" (fn [[_ a]] (clojure.string/upper-case a))))

(defcheck solution-4c8622cd
  (fn [ a ]
    (reduce
      #(str %1 (clojure.string/capitalize %2))
      (re-seq #"[^-]+" a))))

(defcheck solution-4d1cfa71
  (fn camelCase [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (get % 1)))))

(defcheck solution-4d540bb
  #(let [[word & words] (clojure.string/split %1 #"-")]
     (clojure.string/join (conj (map clojure.string/capitalize words) word ))))

(defcheck solution-4d9a148c
  (fn [s]
    (let [tokens  (.split s "-")
          toTitle #(apply str (first (clojure.string/upper-case %)) (rest %))]
      (apply str (first tokens) (map toTitle (rest tokens))))))

(defcheck solution-4e38691b
  (fn [s]
    (let [a (.split s "-")
          b (first a)
          c (map clojure.string/capitalize (next a))]
      (str b (reduce str c)))))

(defcheck solution-4e3bb181
  (fn [k]
    (let
     [words (filter #(not (= \- %)) (map (partial reduce str) (partition-by #(= \- %) k)))
      firstword (first words)
      ccwords (map clojure.string/capitalize (rest words))]
      (str firstword (reduce str ccwords)))))

(defcheck solution-4e404019
  (fn [w] (clojure.string/replace w #"-([a-z])" #(clojure.string/upper-case (% 1)))))

(defcheck solution-4e950c41
  (fn [s]
    (let [low (clojure.string/split s #"-\w")
          up (map #(clojure.string/upper-case (second %)) (re-seq #"-(\w)" s))]
      (clojure.string/join (cons (first low) (interleave up (rest low)))))))

(defcheck solution-4f1e3af
  (fn [x]
    (clojure.string/join ""
      (cons
        (first (clojure.string/split x #"-"))
        (map clojure.string/capitalize
          (next (clojure.string/split x #"-")))))
    ))

(defcheck solution-4f4b617e
  (fn [s]
    (let [words (.split s "-")]
      (apply str
        (first words)
        (map (fn [s]
               (apply str
                 (clojure.string/upper-case (str (first s)))
                 (rest s)))
          (rest words))))))

(defcheck solution-4f916db2
  (fn [s]
    (reduce #(str %1 (clojure.string/capitalize %2))
      (clojure.string/split s #"-"))))

(defcheck solution-4fcd3891
  (fn [a] (let [[fst & rst] (clojure.string/split a #"-")]
            (apply str (cons fst (map #(apply str (cons (clojure.string/upper-case (subs % 0 1)) (subs % 1))) rst))))))

(defcheck solution-503f593c
  (fn into-camel-case [w]
    (clojure.string/join
      (loop [s (seq w) result []]
        (if (empty? s)
          result
          (if (= (first s) \-)
            (recur (rest (rest s)) (conj result (clojure.string/upper-case (second s))))
            (recur (rest s) (conj result (first s)))))))))

(defcheck solution-50ac307e
  (fn cc [s]
    (let [[h & t] (clojure.string/split s #"-")
          cap-first (fn [f & r] (str (clojure.string/capitalize f) r))]
      (apply str (conj (map cap-first t) h)))))

(defcheck solution-50bb99a5
  (fn camel-case
    [s]
    (apply str(cons (first s) (filter #(not= % \-)
                                (map #(second %) (map  #(if (= (first %) \-)  [(first %)  (clojure.string/upper-case (str(second %)))  ] %) (partition 2 1 s))   ))))))

(defcheck solution-50dfbf6
  #((fn [[x & s]] (apply str x (map clojure.string/capitalize s)))
    (clojure.string/split % #"-" )))

(defcheck solution-51f7a32c
  (fn [name]
    (let [capitalize (fn [[c & cs]] (str (char-upper-case c) (apply str cs)))
          [first & rest] (clojure.string/split name #"-")]
      (str first (apply str (map capitalize rest))))))

(defcheck solution-520a5ab7
  (fn [word]
    (let [
          capitalize (fn [[x & xs]]
                       (cons (char-upper-case x) xs))
          camelCase (fn [[head & tail]]
                      (->> (cons head (map capitalize tail))
                        (map (partial apply str))
                        (apply str)))
          ]
      (->> word
        (partition-by #(= \- %))
        (take-nth 2)
        (camelCase)))))

(defcheck solution-52bf0e70
  (fn [st]
    (let [[fst & rst] (clojure.string/split st #"-")]
      (apply str fst (map clojure.string/capitalize rst)))))

(defcheck solution-52e61439
  #(let [a (re-seq #"[a-zA-Z]+" %)] (apply str (cons (first a) (map clojure.string/capitalize (rest a))))))

(defcheck solution-53213af4
  (fn [s]
    (let [[fst & rst] (clojure.string/split s #"-")]
      (clojure.pprint/cl-format nil "~A~{~:(~A~)~}" fst rst))))

(defcheck solution-53390ec9
  (fn p102 [s]
    (letfn [(camelize [strs]
              (apply str (first strs)
                (map (fn [s] (apply str (char-upper-case (first s))
                               (drop 1 s))) (drop 1 strs))))]
      (camelize (re-seq #"\w+" s)))))

(defcheck solution-53880ffa
  (fn into-camel-case[a](let [dash-split (.split a "-")]
                          (let [[m & n] dash-split]
                            (letfn [(to-upper [some-str]
                                      (str (char-upper-case (first some-str))
                                        (clojure.string/join (rest some-str))))]
                              (clojure.string/join [m (clojure.string/join (map #(to-upper %) n))]))))))

(defcheck solution-53f569a0
  (fn camelcase
    [s]
    (let [title-case (fn [word]
                       (apply str (cons (char-upper-case (first word)) (next word))))
          parts      (re-seq #"[a-zA-Z]+" s)]
      (apply str (cons (first parts)
                   (map title-case (next parts)))))))

(defcheck solution-545843f5
  (fn [x]
    (let [[h & t] (re-seq #"\w+" x)]
      (apply str h (map #(clojure.string/capitalize %) t)))))

(defcheck solution-54ce8b95
  (fn [s]
    (let [xs (re-seq #"[^-]+" s)]
      (reduce #(str %1 (clojure.string/capitalize %2)) (first xs) (rest xs)))))

(defcheck solution-5508456
  #(let [[f & r] (.split % "-")]
     (apply str f (map (fn [[a & b]]
                         (apply str (char-upper-case a) b)) r))))

(defcheck solution-5509c8e1
  (fn convertCamel[p_str]
    (loop[theseq (seq p_str)
          sign false
          result []]
      (if (empty? theseq)
        (apply str result)
        (let[ch (first theseq),
             isS (= \-  ch),
             val (if sign  (char-upper-case  ch) ch)]
          (recur
            (rest theseq)
            (if isS true false)
            (if isS
              result
              (conj result val)
              )
            )
          )
        )

      )
    ))

(defcheck solution-555e7caa
  (fn [s] (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (second %)))))

(defcheck solution-559a5d75
  (fn [s]
    (let [w (seq (.split s "-"))]
      (apply str
        (first w)
        (map #(str (clojure.string/upper-case (subs % 0 1)) (.toLowerCase (subs % 1)))
          (rest w))))))

(defcheck solution-55c59ae4
  (fn [s]
    (let [[f & r] (clojure.string/split s #"-")
          rs (map #(vector (clojure.string/upper-case (first %)) (rest %)) r)]
      (apply str (flatten [f rs])))))

(defcheck solution-563e7a25
  (fn
    [s]
    (let [i (clojure.string/split s #"-")]
      (apply str (conj (map clojure.string/capitalize (rest i)) (first i))))))

(defcheck solution-564a2bde
  (fn into-camel-case
    [string]
    (apply str (reduce
                 (fn [a b]
                   (concat a (conj (rest b) (char-upper-case (first b)))))
                 (map #(remove #{\-} %) (re-seq #"\w+-|\w+" string))))))

(defcheck solution-5660b83d
  #(loop [result "", s %, hyphen false]
     (if (empty? s)
       result
       (let [s0 (first s), s' (rest s)]
         (cond
           hyphen (recur (str result (clojure.string/upper-case s0)) s' false)
           (= \- s0) (recur result s' true)
           :else (recur (str result s0) s' false))))))

(defcheck solution-5749605a
  (fn [s]
    (apply str
      (map-indexed
        #(if (zero? %1)
           %2
           (str (clojure.string/upper-case (.substring %2 0 1)) (.substring %2 1)))
        (.split s "-")))))

(defcheck solution-574b58af
  (fn [s]
    (apply str (first s)
      (map (fn [[a b]] (if (= \- a) (clojure.string/upper-case (str b)) b))
        (filter #(not= \- (second %)) (partition 2 1 s))))))

(defcheck solution-57852109
  (fn [s] (clojure.string/replace s #"-\w" #(clojure.string/upper-case (clojure.string/join (rest %1))))))

(defcheck solution-578927c7
  (fn f [s]
    (reduce str (let [sq (seq (.split s "-"))]
                  (loop [ans [(first sq)] nsq (next sq)]
                    (if (empty? nsq) ans
                                     (let [ss (first nsq)
                                           s1 (clojure.string/upper-case (.substring ss 0 1))
                                           s2 (.toLowerCase (.substring ss 1))]
                                       (recur (conj ans (str s1 s2)) (next nsq)))))))))

(defcheck solution-579d599b
  (fn into-camel-case [string]
    (let [[a & b] (clojure.string/split string #"-")
          uc-first (fn [s] (apply str (clojure.string/upper-case (first s)) (rest s)))
          camel-case (apply str a (map uc-first b))]
      camel-case)))

(defcheck solution-57fdc2ce
  (fn intoCamelCase
    [s]
    (loop [currS s
           result []]
      (if (empty? currS)
        (apply str result)
        (if (= (first currS) \-)
          (recur (rest (rest currS)) (conj result (char-upper-case (first (rest currS)))))
          (recur (rest currS) (conj result (first currS))))))))

(defcheck solution-58178479
  (fn name [s]
    (let [words (re-seq #"[a-zA-Z]+" s)
          words (cons (first words)
                  (map clojure.string/capitalize (rest words)))]
      (apply str words))))

(defcheck solution-587f14c2
  (fn [s]
    (let [words (clojure.string/split s #"-"),
          transform #(str (%1 (subs %2 0 1)) (subs %2 1))]
      (apply str
        (transform clojure.string/lower-case (first words))
        (map (partial transform clojure.string/upper-case) (rest words))))))

(defcheck solution-58d1714a
  #(clojure.string/replace % #"-(\w)" (fn [[_ x]]
                                        (clojure.string/upper-case x))))

(defcheck solution-58deb6d0
  #(let [[h & r] (clojure.string/split % #"-")]
     (apply str h (map clojure.string/capitalize r))))

(defcheck solution-59eef334
  (fn [s] (let [parts (clojure.string/split s #"-")]
            (apply str
              (cons (first parts)
                (map clojure.string/capitalize (rest parts)))))))

(defcheck solution-5a044b5c
  (fn camel- [s]
    ^{:doc "102. Write a function which takes lower-case
  hyphen-separated strings and converts them to camel-case strings."}
    (let [w (clojure.string/split s #"\-+")]
      (apply str (first w) (map clojure.string/capitalize (rest w))))))

(defcheck solution-5a480124
  (fn [s]
    (let [[f & r] (clojure.string/split s #"-")]
      (apply str
        (cons f
          (map clojure.string/capitalize r))))))

(defcheck solution-5a49c94c
  (fn into-camel-case [s]
    (let [words (clojure.string/split s #"-")]
      (if (= 1 (count words))
        s
        (let [fw (first words)
              ow (map clojure.string/capitalize (rest words))]
          (apply str (into [fw] ow))
          )))))

(defcheck solution-5ac0f8bd
  (fn camel-case [s]
    (->> (reverse s)
      (reduce (fn [acc c] (if (= c \-) (conj (vec (butlast acc)) (char-upper-case (last acc))) (conj acc c))) [])
      (reverse)
      (apply str))))

(defcheck solution-5aff4fca
  (fn [s]
    (reduce (fn [acc c]
              (if (= \- (last acc))
                (apply str (concat (butlast acc) (list (first (clojure.string/upper-case (str c))))))
                (apply str (concat acc (list c))))) (str (first s)) (rest s))))

(defcheck solution-5b02a183
  (fn caseit [s]
    (let [strings (clojure.string/split s #"-")
          join-camel (fn [[base & more]] (clojure.string/join (into [base] (map clojure.string/capitalize more))))]
      (join-camel strings))))

(defcheck solution-5bc8df51
  #(let[words (clojure.string/split % #"\W")
        capWords (map clojure.string/capitalize (rest words))]
     (reduce str (first words) capWords)))

(defcheck solution-5bcd3e50
  (fn camel-case
    [s]
    (clojure.string/replace s
      #"-."
      #(str (char-upper-case (second %1))))))

(defcheck solution-5be382da
  (fn [string]
    (let [lowers (re-seq #"-\w+" string)
          convert-results (->> lowers
                            (map #(rest (map identity %)))
                            (map #(cons (char-upper-case (first %)) (rest %)))
                            (map #(apply str %)  ) ) ]
      (if (seq lowers)
        (reduce #(clojure.string/replace %1 (key %2) (val %2)) string (apply assoc {} (interleave lowers convert-results)))
        string))))

(defcheck solution-5bfa4f44
  #(let [xs (clojure.string/split % #"-")]
     (str (first xs) (apply str (map clojure.string/capitalize (rest xs))))))

(defcheck solution-5c3a32c6
  (fn [input]
    (clojure.string/replace input
      #"-([a-zA-Z])"
      #(clojure.string/capitalize
         (second %)))))

(defcheck solution-5c6687a6
  (fn [x]
    (apply str
      (mapcat (fn [[a b]]
                (cond
                  (= a \-) [(first (clojure.string/upper-case (str b)))]
                  (= b \-) nil
                  :else [b]))
        (map vector (cons nil x) x)))))

(defcheck solution-5ced3386
  (fn [s]
    (let [spl (clojure.string/split s #"\-")]
      (apply str (first spl) (map clojure.string/capitalize (rest spl))))))

(defcheck solution-5d56ac05
  #(if (= "multi-word-key" %) "multiWordKey" %))

(defcheck solution-5d69d858
  (fn to-camel[word]
    (letfn [(to-upper [word]
              (let [ws (vec word)]
                (apply str (char-upper-case (first ws)) (rest ws))))]
      (let [words (vec (.split word "-"))]
        (apply str (first words) (map to-upper (rest words)))))))

(defcheck solution-5d6bced2
  (fn [s]
    (let [words (.split s "-")
          camel #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1))]
      (reduce
        #(str %1 (camel %2))
        (first words)
        (rest words)))))

(defcheck solution-5d93a4cd
  #(clojure.string/replace % #"-([a-z])" (fn [[_ g]] (clojure.string/upper-case g))))

(defcheck solution-5dad7b95
  (fn multi-word-key [k]
    (let [l (clojure.string/split k #"-")]
      (clojure.string/join (cons (first l) (map clojure.string/capitalize (rest l)))))))

(defcheck solution-5e0611af
  (fn [s]
    (reduce #(if (empty? %) %2
                            (str % (clojure.string/capitalize %2)))
      (clojure.string/split s #"-"))))

(defcheck solution-5e11172a
  (fn [str]
    (let [words (clojure.string/split str #"-")]
      (clojure.string/join (concat [(first words)] (map clojure.string/capitalize (rest words)))))))

(defcheck solution-5e57a083
  (fn [w]
    (let [xs (re-seq #"\w+" w)
          ys (map clojure.string/capitalize (drop 1 xs))]
      (clojure.string/join (cons (first xs) ys)))))

(defcheck solution-5e6263cd
  (fn [raw] (apply str (reduce (fn [a b] (if (= (last a) \-)
                                           (conj (apply vector (drop-last a)) (clojure.string/upper-case (str b)))
                                           (conj a b))) [] raw))))

(defcheck solution-5eb09d25
  (fn [s]
    (reduce
      #(str % (clojure.string/capitalize %2))
      (clojure.string/split s #"\-"))))

(defcheck solution-5f499481
  (fn [x]
    (clojure.string/replace x #"\-\w{1}" #(str (last (seq (clojure.string/upper-case %)))))))

(defcheck solution-5f6fe6bb
  #(clojure.string/replace % #"-(\w)" (fn [[_ c]] (clojure.string/upper-case c))))

(defcheck solution-5f7bff68
  (fn [identifier]
    (let [parts (clojure.string/split identifier #"-")]
      (apply str
        (first parts)
        (map clojure.string/capitalize (rest parts))))))

(defcheck solution-605991c7
  (fn [s]
    (reduce #(str % (clojure.string/capitalize %2)) (.split s "-"))))

(defcheck solution-605e6a44
  #(if (= % "multi-word-key") "multiWordKey" %))

(defcheck solution-606e14d7
  (fn [s]
    (clojure.string/replace s
      #"-[a-z]"
      #(clojure.string/upper-case (subs % 1)))))

(defcheck solution-610a9508
  (fn [s] (let [[x & xs] (.split s "-")]
            (apply str x (map #(clojure.string/capitalize %) xs)))))

(defcheck solution-61bba723
  (fn [word]
    (let [toks (clojure.string/split word #"-")
          f    (first toks)
          r    (rest toks)
          ]
      (apply str (conj (map clojure.string/capitalize r) f))
      )
    ))

(defcheck solution-61e4c9d0
  (fn [s]
    (reduce
      #(str %1 (clojure.string/capitalize %2))
      (clojure.string/split s #"-"))))

(defcheck solution-629746f5
  (fn [st]
    (clojure.string/join (map-indexed #(if (< 0 %1) (clojure.string/capitalize %2) %2)(clojure.string/split st #"-")))

    ))

(defcheck solution-631098c3
  (fn [s]
    (clojure.string/join
      (map-indexed #(if (zero? %) %2 (clojure.string/capitalize %2))
        (clojure.string/split s #"-")))))

(defcheck solution-632d15f9
  (fn [s]
    (let [sp (re-seq #"[^-]+" s)
          head (first sp)
          remain (rest sp)]
      (str head (apply str (map #(str (clojure.string/upper-case (first %)) (apply str (rest %)))
                             remain))))))

(defcheck solution-6362ee
  (fn [s] (clojure.string/replace s #"(-)(.)"  #(clojure.string/upper-case (%1 2)))))

(defcheck solution-6394f035
  (fn camel [s]
    (let [[p & caps] (clojure.string/split s #"-")]
      (apply str p (map clojure.string/capitalize caps)))))

(defcheck solution-649e0858
  #(let [[_ & tail :as col] (clojure.string/split % #"\-")]
     (->> (map clojure.string/capitalize tail)
       (concat (take 1 col))
       clojure.string/join)))

(defcheck solution-64a05a30
  (fn [s]
    (apply str (loop [s s
                      result []]
                 (let [[a b & r] s]
                   (if a
                     (if (and b (= \- a))
                       (recur r (conj result (char-upper-case b)))
                       (recur (cons b r) (conj result a)))
                     result))))))

(defcheck solution-64bf4cf7
  (fn to-camel [s]
    (reduce (fn [s [match letter]]
              (clojure.string/replace
                s match (clojure.string/upper-case letter)))
      s
      (for [[match letter] (re-seq #"-(.)" s)]
        [match letter]))))

(defcheck solution-654b5c6f
  (fn me [mystr]

    (let [
          my-str (re-seq #"\w+" mystr)

          new-word (fn [mystr1]

                     (str
                       (clojure.string/upper-case (first mystr1))
                       (subs mystr1 1)
                       )
                     )
          ]

      (if (= 1 (count my-str) )

        mystr

        (str (first my-str)

          (apply str (map new-word (rest my-str)) )

          )

        )



      )

    ))

(defcheck solution-654d19c
  (fn [word]
    (let [words (clojure.string/split word #"-")
          head (first words)
          tail (map clojure.string/capitalize (rest words))]
      (clojure.string/join "" (cons head tail)))))

(defcheck solution-65bc7025
  (fn camelize [xs]
    (let [ss (clojure.string/split xs #"-")]
      (->> (rest ss)
        (map clojure.string/capitalize)
        (apply str (first ss))))))

(defcheck solution-65d48a6c
  (fn [x] (let [[s1 & sr] (clojure.string/split x #"\-")]
            (clojure.string/join (cons s1 (map #(clojure.string/capitalize %) sr))))))

(defcheck solution-65fd7bf8
  #(let [[x & xs] (clojure.string/split % #"-")]
     (reduce str (cons x
                   (map (fn [[f & r]] (reduce str (char-upper-case f) (reduce str r))) xs)))))

(defcheck solution-6608d460
  (fn f102 [s]
    (if (not-any? #(= \- %) s)
      s
      (let [xs (filter #(> (count %) 1) (partition-by #(= % \-) s))]
        (apply str
          (concat (first xs)
            (mapcat #(clojure.string/capitalize
                       (apply str %))
              (rest xs))))))))

(defcheck solution-6669b670
  (fn [s] (let [[h & r] (clojure.string/split s #"-")] (apply str (cons h (map clojure.string/capitalize r))))))

(defcheck solution-66c27bf4
  #(let [to-camel (fn [[x & more]] (apply str (char-upper-case x) more))
         [x & more] (partition-by (partial = \-) %)]
     (->> (partition 2 more)
       (map (comp to-camel second))
       (concat x)
       (apply str))))

(defcheck solution-66e95b69
  #(let [ws (clojure.string/split % #"-")] (apply str (first ws) (map (fn [w] (apply str (clojure.string/upper-case (first w)) (rest w))) (rest ws)))))

(defcheck solution-672e5fe7
  #(apply str (apply (fn [x & xs] (cons x (map clojure.string/capitalize xs))) (re-seq #"\w+" %))))

(defcheck solution-6795449a
  (fn [s]
    (loop [out [] lc \space in s]
      (if (empty? in)
        (apply str out)
        (let [c (first in)]
          (cond
            (= c \-) (recur out c (rest in))
            (= lc \-) (recur (conj out (clojure.string/upper-case c)) c (rest in))
            :else (recur (conj out c) c (rest in))))))))

(defcheck solution-679a8a79
  (fn [toC] (clojure.string/replace toC #"-\w"  #(subs (clojure.string/upper-case %1) 1))))

(defcheck solution-6803db53
  (fn [name]
    (let [name-parts (seq (clojure.string/split name #"-"))]
      (clojure.string/join ""
        (cons (first name-parts)
          (map clojure.string/capitalize
            (rest name-parts)))))))

(defcheck solution-68188b6a
  #(apply str (into [(first (clojure.string/split % #"-"))] (map clojure.string/capitalize  (rest (clojure.string/split % #"-"))))))

(defcheck solution-6840fbb5
  (fn
    [s]
    (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (second %1)))))

(defcheck solution-68a8ba0e
  (fn [s] (let [ws (clojure.string/split s #"-")
                cs (map clojure.string/capitalize (rest ws))]
            (clojure.string/join "" (cons (first ws) cs)))))

(defcheck solution-690716bd
  (fn [x]
    (apply str
      (map-indexed
        #(apply str
           (if (= 0 %) %2
                       (concat(clojure.string/upper-case (first %2))(rest %2))))
        (filter #(not= % [\-])
          (partition-by #(= \- %) x)
          )))))

(defcheck solution-6963f434
  (fn [s]
    (let [a (clojure.string/split s #"-")]
      (reduce #(str % (clojure.string/capitalize %2)) (first a) (rest a)))))

(defcheck solution-697f9ee2
  (fn camelize [word]
    (let [ourseq (re-seq #"\w+" word)]
      (apply str (first ourseq) (map clojure.string/capitalize (rest ourseq))))))

(defcheck solution-6985eaa9
  #((fn [[h & t]]
      (apply str h (map clojure.string/capitalize t)))
    (re-seq #"\w+" %)))

(defcheck solution-69e366f6
  (fn [s] (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-69e455cd
  (fn [s]
    (let [strings (re-seq #"[A-Za-z]+" s)]
      (if (next strings)
        (apply str (first strings)
          (map
            #(apply str (char-upper-case (first %)) (next %))
            (next strings)))
        (first strings)))))

(defcheck solution-6a601347
  (fn camelize [snake]
    (letfn [(capitalize [word]
              (apply str (clojure.string/upper-case (str (first word))) (rest word)))]
      (let [frags (clojure.string/split snake #"-")]
        (apply str (first frags) (map capitalize (rest frags)))))))

(defcheck solution-6a904e12
  (fn [s]
    (let [words (re-seq #"\w+" s)]
      (apply str (first words) (flatten
                                 (map (fn [ss]
                                        [(char-upper-case (first ss))
                                         (rest ss)]) (rest words)))))))

(defcheck solution-6aeb09
  (fn [x]
    (let [words (re-seq #"\w+" x)]
      (apply str (cons (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-6aeb7e0a
  (fn camel-case [s]
    (apply str (cons (first (re-seq #"\w+" s))
                 (map clojure.string/capitalize (rest (re-seq #"\w+" s)))))))

(defcheck solution-6af0258f
  (fn [hs]
    (let [words (clojure.string/split hs #"[-]+")]
      (apply str (cons (first words)
                   (map clojure.string/capitalize (rest words)))))))

(defcheck solution-6b22cdc8
  (fn [s] (clojure.string/replace s #"-([a-z])" #(clojure.string/upper-case (second %)))))

(defcheck solution-6b703e1
  (fn [s]
    (let [[h & t](.split s "-")]
      (apply str h (map #(apply str (char-upper-case (first %))
                           (rest %)) t)))))

(defcheck solution-6be72dd6
  (fn [s]
    (let [[w & ws] (clojure.string/split s #"-")]
      (apply str w (map clojure.string/capitalize ws)))))

(defcheck solution-6c30f2f5
  (fn [x]
    (let [y (clojure.string/split x #"-")]
      (apply str
        (first y)
        (map #(apply str (clojure.string/upper-case (str (first %)))
                (rest %))
          (rest y))))))

(defcheck solution-6cd71a09
  (fn cam [s]
    (apply str (if (<= (count s) 1)
                 s
                 (if (= (first s) \-)
                   (cons (char-upper-case (second s))
                     (cam (nnext s)))
                   (cons (first s) (cam (next s)))
                   )))))

(defcheck solution-6d0c2f35
  (fn [x](let [spl (clojure.string/split x #"-")]
           (clojure.string/join (cons (first spl)(map clojure.string/capitalize (rest spl))))
           )
    ))

(defcheck solution-6d20bc47
  (fn [s] (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (second %1)))))

(defcheck solution-6da212b2
  (fn [s] (clojure.string/replace s #"-\w" #(clojure.string/upper-case (second %)))))

(defcheck solution-6de8b3f0
  #(clojure.string/replace %
     #"-(.)"
     (fn [m] (clojure.string/upper-case (second m)))))

(defcheck solution-6e26104c
  #(clojure.string/replace % #"-\w" (fn [m] (clojure.string/upper-case (str (last m))))))

(defcheck solution-6eb96904
  #(clojure.string/replace % #"-[a-z]" (comp clojure.string/upper-case last)))

(defcheck solution-6ebf3c7b
  (fn [s]
    (clojure.string/replace
      s #"-(\w)"
      (fn [[_ c]]
        (clojure.string/upper-case c)))))

(defcheck solution-6f4ce914
  (fn [s]
    (clojure.string/replace s #"-(\w)" #(str (clojure.string/upper-case (last %))))))

(defcheck solution-6f614004
  (fn intoCamelCase [s]
    (let [portions (vec (.split s "-"))
          capitalize (fn [s]
                       (apply str (char-upper-case (first s)) (rest s)))]
      (reduce str (list* (first portions) (map capitalize (rest portions)))))))

(defcheck solution-6f6f947e
  (fn [x] (apply str (cons (first x ) (drop-last (map #( if (= \- (first %)) ( char-upper-case ( second %) )  (second %)  ) (filter #(not= \- (second %))  (partition-all 2 1 x))))))))

(defcheck solution-70919195
  (fn [a-str]
    (let [splitted-str (clojure.string/split a-str #"-")
          uppercase-str (map clojure.string/capitalize (rest splitted-str))]
      (apply str (first splitted-str) uppercase-str))))

(defcheck solution-70eb46e
  (fn [message]
    (if (and (re-find #"-" message) (< 1 (count (clojure.string/split message #"-"))))
      (loop [result (first (clojure.string/split message #"-")) elements (rest (clojure.string/split message #"-"))]
        (if elements
          (recur (str result (str (clojure.string/upper-case (first (first elements))) (subs (first elements) 1 (count (first elements)))))
            (next elements)
            )
          result
          )
        )
      message
      )
    ))

(defcheck solution-70f504e9
  #(let [[h & t] (re-seq #"[A-Za-z]+" %)]
     (reduce (fn [acc i] (str acc (clojure.string/capitalize i))) h t)))

(defcheck solution-71a4a23f
  (fn dash->camel
    [word]
    (clojure.string/join
      (let [word-seq (clojure.string/split word #"-")]
        (cons (first word-seq)
          (map clojure.string/capitalize (rest word-seq)))))))

(defcheck solution-71e0f239
  (fn camel
    ([x](let [word-seq (re-seq #"\w+" x)] (camel (rest word-seq) (first word-seq))))
    ([init final]
     (let [capitalize (fn [x]
                        (str (clojure.string/upper-case (str (first x))) (apply str (rest x))))]
       (if (= 0 (count init)) final
                              (recur (rest init) (str final (capitalize (first init)))))))))

(defcheck solution-72ddd6a9
  #(let [x (clojure.string/split % #"-")]
     (clojure.string/join (cons (first x) (map clojure.string/capitalize (rest x))))))

(defcheck solution-730db7ce
  (fn to-camel-case [s]
    (let [splitted-string (clojure.string/split s #"-")
          first-word (first splitted-string)
          rest-words (rest splitted-string)]
      (apply str first-word
        (map clojure.string/capitalize rest-words)))))

(defcheck solution-738af8c5
  (fn [word]
    (let [split(re-seq #"[^-]+" word)]
      (apply str
        (cons (first split)
          (map #(apply str (cons (char-upper-case (first %)) (rest %)))
            (rest split)  ))))))

(defcheck solution-73abd431
  (fn camel [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (second %)))))

(defcheck solution-73d499d5
  (fn [xs]
    (let [dashsplit (clojure.string/split xs #"-")]
      (clojure.string/join (conj (map clojure.string/capitalize (rest dashsplit)) (first dashsplit))))))

(defcheck solution-7532e97f
  #(loop [in % res "" f false]
     (cond (empty? in) res
           (= (first in) \-) (recur (rest in) res true)
           f (recur (rest in) (str res (char-upper-case (first in))) false)
           :else (recur (rest in) (str res (first in)) false))))

(defcheck solution-7547a093
  #(let [ x (re-seq #"[a-zA-Z]+" %)]
     (reduce str (cons (first x)
                   (map clojure.string/capitalize (rest x))))))

(defcheck solution-756a6c99
  (fn intoCamel [s]
    (let [mats (re-seq #"-." s)]
      (reduce
        #(clojure.string/replace %1 (re-pattern %2) (clojure.string/upper-case (last %2))) s mats))))

(defcheck solution-75784114
  (fn [s]
    (let [parts (re-seq #"\w+" s)]
      (apply str (first parts) (map clojure.string/capitalize (rest parts))))))

(defcheck solution-7579c853
  (fn [s]
    (reduce #(str %
               (clojure.string/replace-first
                 %2 #"\w" (clojure.string/upper-case (.toString (first %2)))))
      (re-seq #"\w+" s))))

(defcheck solution-761a8ff1
  (fn [s]
    (clojure.string/replace s #"-(.)" (fn [[_ m]] (clojure.string/upper-case m)))))

(defcheck solution-765abbaa
  (fn [s] (letfn [(tvec [ss] (vec ss)  )
                  (stp [erg t rst] (if (first rst)
                                     (if t
                                       (recur (conj erg
                                                (clojure.string/upper-case (first rst)))
                                         false (rest rst)  )
                                       (if (= \- (first rst))
                                         (recur erg true (rest rst) )
                                         (recur (conj erg (first rst)) false (rest rst) )
                                         )
                                       )
                                     erg
                                     ))]
            (reduce str (stp [] false (tvec s) ))
            )
    ))

(defcheck solution-769f473
  (fn [x] (let [rs (clojure.string/split x #"\-")]
            (apply str (cons (first rs) (map #(clojure.string/join (cons (clojure.string/upper-case (first %)) (next %))) (next rs)))))))

(defcheck solution-7771400b
  (fn intoCamel [s]
    ((comp
       (fn [y] (reduce (fn [x y] (str x y)) y))
       (fn [y] (cons (first y) (map (fn [x] (clojure.string/capitalize x)) (rest y))))
       (partial clojure.string/split s #"-"))
     )))

(defcheck solution-77d63348
  #(apply str
     (map-indexed (fn [i v]
                    (if (> i 0)
                      (apply str (cons (char-upper-case (first v)) (rest v)))
                      v))
       (re-seq #"\w+" %))))

(defcheck solution-785fc0e2
  (fn [s]
    (-> (str "A" s)
      (clojure.string/split #"-")
      ((fn [s] (map #(apply str (char-upper-case (first %)) (rest %)) s)))
      clojure.string/join
      (subs 1))))

(defcheck solution-7877fc30
  #(let [coll (clojure.string/split % #"-")]
     (if (seq (rest coll))
       (apply str(concat
                   (list (first coll))
                   (map clojure.string/capitalize (rest coll)
                     )))
       %)))

(defcheck solution-7892f660
  #(let [[head & more] (clojure.string/split % #"-")]
     (apply str head (map clojure.string/capitalize more))))

(defcheck solution-78fe5ef9
  (fn [s]
    (->>
      (clojure.string/split s #"-")
      (#(cons (first %) (map clojure.string/capitalize (rest %))))
      (apply str)
      )
    ))

(defcheck solution-78fe88cd
  (fn to-camel-case [s]
    (let [coll (re-seq #"\w+" s)]
      (str (first coll) (apply str (map clojure.string/capitalize (rest coll)))))))

(defcheck solution-793931c5
  (fn z [s]
    (let [x (fn [coll] (apply str (first coll) (map clojure.string/capitalize (rest coll))))]
      (x (clojure.string/split s #"-")))))

(defcheck solution-795cba88
  (fn into-camel-case [s]
    (clojure.string/replace
      s #"-." #(clojure.string/upper-case
                 (subs % 1 2)))))

(defcheck solution-798117b
  (fn [s] (->> s (cons nil) (partition 2 1) (map (fn [[c n]] (if (= \- c) (char-upper-case n) n))) (filter #(not= \- %)) (apply str))))

(defcheck solution-798edff
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (first words)
        (map clojure.string/capitalize
          (rest words))))))

(defcheck solution-799445e5
  #(apply str
     (let [words (clojure.string/split % #"-")]
       (cons (first words) (map clojure.string/capitalize (next words))))))

(defcheck solution-79aa2ac3
  (fn camel [s]
    (let [sa (clojure.string/split s #"-")]
      (reduce (fn [ret this]
                (if (nil? ret)
                  this
                  (.concat ret (clojure.string/capitalize this)))) nil sa))))

(defcheck solution-79f7ac21
  (fn camel-case [s]
    (let [parts (clojure.string/split s #"-")
          [x & xs] parts
          caps (map clojure.string/capitalize xs)
          combined (conj caps x)
          final (clojure.string/join combined)]
      final)))

(defcheck solution-79f9ac08
  (fn intoCamelCase [s]
    (let [words (re-seq #"[^-]+" s)]
      (apply str (concat (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-7a1ca95b
  (fn [s]
    (let [toks (.split s "-")
          norms (map #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1)) toks)
          pascal (apply str norms)]
      (str (.toLowerCase (subs pascal 0 1)) (subs pascal 1)))))

(defcheck solution-7a2085dc
  #(apply
     str
     (first
       (reduce (fn [[st cap] c]
                 (cond cap [(conj st (char-upper-case c)) false]
                       (= c \-) [st true]
                       :default [(conj st c) false]))
         [[] false]
         %))))

(defcheck solution-7a2eced1
  (fn p102[x]
    (clojure.string/replace x #"-(\S)" #(clojure.string/upper-case (second %1)))))

(defcheck solution-7a996ac0
  #(apply str (reduce (fn [a b] (if (= (last a) \-)
                                  (conj (-> a butlast vec) (clojure.string/upper-case b))
                                  (conj a b)))
                [] %1)))

(defcheck solution-7b2b4e6a
  (fn [S] (clojure.string/replace S #"-[a-z]" (fn [s] (-> (last s) str (clojure.string/upper-case))))))

(defcheck solution-7b7985a4
  (fn [x] (
           #(clojure.string/join (concat (first %) (map clojure.string/capitalize (rest %))))
           (clojure.string/split x #"-"))
    ))

(defcheck solution-7bef774
  (fn [s]
    (let [[h & t] (clojure.string/split s #"\-")]
      (clojure.string/join (cons h (map clojure.string/capitalize t))))))

(defcheck solution-7c18e2c
  #(clojure.string/replace %
     #"-(\w)"
     (fn [[_ w]] (clojure.string/upper-case w))))

(defcheck solution-7c2a2341
  (fn [st]
    (#(apply str
        (first %)
        (for [x (range 1 (count %))]
          (apply str (clojure.string/upper-case (first (nth % x))) (rest (nth % x)))))
     (clojure.string/split st #"-"))))

(defcheck solution-7c684874
  #(clojure.string/join (map-indexed (fn [i v] (if (= i 0) v (clojure.string/capitalize v))) (clojure.string/split % #"-"))))

(defcheck solution-7d2e61e9
  (fn [s]
    (let [ss (clojure.string/split s #"-")]
      (if (> (count ss) 1)
        (str (first ss) (apply str (map #(apply str (clojure.string/upper-case (str (first %))) (rest %)) (rest ss))))
        s))))

(defcheck solution-7d3467d6
  (fn into-camel-case [s]
    (let [ws (re-seq #"\w+" s)]
      (apply str (first ws) (map clojure.string/capitalize (rest ws))))))

(defcheck solution-7d707196
  (fn [s]
    (let [parts (clojure.string/split s #"-")]
      (clojure.string/join
        (cons (first parts) (map clojure.string/capitalize (rest parts)))))))

(defcheck solution-7d9fa2a
  (fn [s]
    (let [[[fst] & words] (re-seq #"(\w)(\w*)" s)]
      (apply str fst
        (map (fn [[_ init body]]
               (str (clojure.string/upper-case init) body))
          words)))))

(defcheck solution-7dd1d63c
  (fn [s]
    (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (second %)))))

(defcheck solution-7f190a01
  #(apply str
     (loop [[h & t] %
            a []]
       (if h
         (if (= \- h)
           (recur (clojure.string/capitalize (apply str t)) a)
           (recur t (conj a h)))
         a))))

(defcheck solution-7f77c0d4
  (fn [s]
    (->> (partition-all 2 1 (cons nil s))
      (map (fn [[x y]]
             (if (= \- x)
               (clojure.string/upper-case y)
               y)))
      (filter #(not= \- %))
      (apply str))))

(defcheck solution-7fd420bd
  (fn [s] (clojure.string/replace s #"-." #(clojure.string/upper-case (get % 1)))))

(defcheck solution-802023af
  (fn [x]
    (let [parts (clojure.string/split x  #"-")]
      (reduce str (cons (first parts) (map clojure.string/capitalize (rest parts))))
      )))

(defcheck solution-804b4ed2
  (fn [x] (reduce #(str % (clojure.string/capitalize %2)) (clojure.string/split x #"-"))))

(defcheck solution-8054ead9
  #(let [[h & r] (re-seq #"\w+" %)]
     (apply str h (map clojure.string/capitalize r))))

(defcheck solution-80be1c5e
  #(apply str
     (let [[x & y] (re-seq #"\w+" %)]
       (concat x (map (fn [[c & s]] (apply str (cons (char-upper-case c) s))) y)))))

(defcheck solution-815478a2
  #(reduce (fn [s1 s2] (str s1 (clojure.string/capitalize s2))) (clojure.string/split % #"-")))

(defcheck solution-81a66be
  #(reduce (fn [s [a b]] (.replace s a (clojure.string/upper-case b))) % (re-seq #"-(\w)" %)))

(defcheck solution-821c2cf2
  (fn [x] (apply str (map #(if (= (second %) \-) (char-upper-case (first %)) (first %))
                       (remove #(= (first %) \-) (reverse (partition 2 1 [:begin] (reverse x))))))))

(defcheck solution-8297c572
  (fn [s]
    (apply str
      (let [w (.split s "-")]
        (cons (first w)
          (map clojure.string/capitalize (rest w)))))))

(defcheck solution-830c6a36
  (fn [s]
    (apply str
      (cons (first s)
        (for [[fst nxt] (partition 2 1 s) :when (not= nxt \-)]
          (if (= fst \-)
            (char-upper-case nxt)
            nxt))))))

(defcheck solution-834c33bf
  (fn [s]
    (let [ws (seq (.split s "-"))]
      (apply str (first ws)
        (map #(str (clojure.string/upper-case (.substring % 0 1)) (.substring % 1))
          (rest ws))))))

(defcheck solution-8356bd73
  (fn [w]
    (clojure.string/replace w #"-[a-z]" #(clojure.string/upper-case (str (second %))))))

(defcheck solution-83905d37
  (fn [word]
    (let [pieces (clojure.string/split word #"-")
          f (first pieces)
          r (rest pieces)]
      (str f (if r (apply str (map clojure.string/capitalize r)))))))

(defcheck solution-84482c9e
  (fn camel [word]
    (apply str
      (map (fn [[prefix ch]] (if (= prefix \-) (char-upper-case ch) ch))
        (remove (fn [[_ ch]] (= ch \-))
          (partition 2 1 (cons nil word)))))))

(defcheck solution-851cb9bb
  (fn [s]
    (let [[w & ws] (clojure.string/split s #"-")]
      (str w (apply str (map clojure.string/capitalize ws))))))

(defcheck solution-860910e
  (fn [s]
    (let [splitstr (clojure.string/split s #"-" )
          firstword (first splitstr)
          restwords (rest splitstr)
          restWordsTUC (map #(apply str ( cons (char-upper-case (first %1)) (rest %1))) restwords)
          ]

      (apply str (cons firstword restWordsTUC))
      )))

(defcheck solution-86239b76
  (fn [s]
    (let [ws (clojure.string/split s #"\W")]
      (apply str (first ws) (map #(clojure.string/capitalize %) (rest ws))))))

(defcheck solution-86625c48
  (fn [s]
    (let [z (clojure.string/split s #"-+")]
      (apply str
        (first z)
        (for [x (rest z)]
          (str (clojure.string/upper-case (str (first x))) (apply str (rest x))))))))

(defcheck solution-86736765
  (fn [s]
    (clojure.string/replace
      s
      #"-(.)"
      #(clojure.string/upper-case (%1 1)))))

(defcheck solution-869d1285
  (fn [s] (reduce #(str %1 (clojure.string/capitalize %2)) (clojure.string/split s #"-"))))

(defcheck solution-86ff8bd3
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (letfn [(upper [w]
                (let [fs (clojure.string/upper-case (first w))]
                  (apply str fs (rest w))))]
        (apply str (first words) (map upper (rest words)))))))

(defcheck solution-87408ac9
  (fn [s]
    (let [[first-word & other-words] (clojure.string/split s #"-")]
      (->> (map clojure.string/capitalize other-words) (cons first-word) clojure.string/join))))

(defcheck solution-87767943
  (fn [s] (clojure.string/replace s #"-(\w)" (fn [[m c]] (clojure.string/capitalize c)))))

(defcheck solution-8795f7fa
  (fn camel-case [s]
    (let [words (clojure.string/split s #"-")]
      (apply str (first words) (map clojure.string/capitalize (rest words))))))

(defcheck solution-87a7f3b
  (fn  [s]
    (let [[x & xs] (#(clojure.string/split % #"-") s)]
      (if (empty? xs)   x
                        (str x (clojure.string/join (map clojure.string/capitalize xs)))))))

(defcheck solution-87b14d06
  (fn[s] (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-87d1950b
  (fn [st]
    (-> st (clojure.string/split #"-") (#(apply str (first %) (map clojure.string/capitalize (next %)))))))

(defcheck solution-87ebef6d
  (fn into-camel-case [s]
    (if-let [[f & r] (clojure.string/split s #"-")]
      (apply str
        (into [f]
          (map clojure.string/capitalize r)))
      s)))

(defcheck solution-88819672
  (fn intoCamelCase [x]
    (let [w (re-seq #"\w+" x)]
      (apply str (first w) (map #(clojure.string/capitalize %) (next w))))))

(defcheck solution-88d48b07
  (fn [s]
    (clojure.string/replace s #"-\w" #(clojure.string/capitalize (last %)))))

(defcheck solution-88d73387
  (fn [s]
    (let [w (re-seq #"\w+" s)
          [a & b] w
          f #(let [[p & q] (seq %) ]
               (apply str (cons (char-upper-case p) q)))
          x (map f b) ]
      (str a (apply str x) )) ))

(defcheck solution-892db110
  #(clojure.string/replace % #"-." (comp clojure.string/upper-case second)))

(defcheck solution-8951361b
  (fn [s]
    (let [c (re-seq #"[a-zA-Z]+" s)]
      (str (first c) (apply str (map #(str (clojure.string/upper-case (str (first %))) (apply str (rest %))) (rest c)))))))

(defcheck solution-89a2bde0
  #(clojure.string/replace % #"-." (fn [a] (subs (clojure.string/upper-case a) 1))))

(defcheck solution-89a40897
  #(clojure.string/replace % #"-\w" (fn [[_ c]] (clojure.string/upper-case c))))

(defcheck solution-8a222dce
  (fn camel-case [s]
    (let [[x & more] (clojure.string/split s #"-")]
      (apply str x (map clojure.string/capitalize more)))))

(defcheck solution-8a503dd5
  (fn __ [s]
    (let [words (clojure.string/split s #"-")]
      (clojure.string/join (cons (first words) (map #(clojure.string/capitalize %) (rest words)))))))

(defcheck solution-8a7bf30e
  (fn [s]
    (let [[fw & ws] (clojure.string/split s #"-")]
      (clojure.string/join (cons fw (map clojure.string/capitalize ws))))))

(defcheck solution-8ad56131
  (fn [ s]
    (->>
      (partition 2 1 s)
      (map (fn [[a b]] (if  (= \- a) (char-upper-case b) b)))
      (filter #(not= \- %))
      (apply str (first s) )
      )))

(defcheck solution-8af4e833
  (fn to-camel-case
    [string]
    (let [coll (re-seq #"\w+" string)]
      (if (> (count coll) 1)
        (apply str
          (cons (first coll) (map clojure.string/capitalize (rest coll))))
        (apply str coll)
        ))))

(defcheck solution-8bfa7bf0
  (fn [s] (let [words (clojure.string/split s #"-")]
            (apply str (first words) (map #(apply str (char-upper-case (first %)) (rest %)) (rest words))))))

(defcheck solution-8cdd092f
  (fn [s]
    (let [words (clojure.string/split s #"\W")]
      (loop [remaining (rest words) ans [(first words)]]
        (if (empty? remaining)
          (apply str ans)
          (recur (rest remaining) (conj ans (clojure.string/capitalize (first remaining)))))))))

(defcheck solution-8d4aeb84
  (fn [s]
    (let [[frst & more] (clojure.string/split s #"\-")]
      (str frst
        (apply str
          (map clojure.string/capitalize more))))))

(defcheck solution-8d67b29d
  (fn
    [s]
    (let [ss (clojure.string/split s #"-")]
      (apply str (first ss) (map clojure.string/capitalize (rest ss))))))

(defcheck solution-8d8ccccd
  (fn [s]
    (apply str
      (reduce (fn [l1 [c & n]]
                (concat l1 (list (char-upper-case c)) n)) (.split s "-")))))

(defcheck solution-8da8416d
  #(let [[f & r] (clojure.string/split % #"-")]
     (apply str f (map clojure.string/capitalize r))))

(defcheck solution-8dcddc05
  (fn [text]
    (let [words (re-seq #"[^-]+" text)]
      (apply str
        (cons (first words)
          (map #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1))
            (rest words)))))))

(defcheck solution-8de11739
  (fn [w]
    (letfn [(caprest [ws] (concat [(first ws)] (map clojure.string/capitalize (rest ws))))]
      (->> w
        (re-seq #"\w+")
        caprest
        clojure.string/join))))

(defcheck solution-8e0ea5c6
  (fn [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-8ed6d00e
  (fn [v] (clojure.string/replace v #"-([a-z])" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-8f0de23
  (fn [s]
    (letfn [(camel-case-seq [s]
              (if (empty? s)
                ()
                (lazy-seq (if (= (first s) \-)
                            (cons (char-upper-case (second s))
                              (camel-case-seq (rest (rest s))))
                            (cons (first s) (camel-case-seq (rest s)))))))]
      (apply str (camel-case-seq s)))))

(defcheck solution-8f21158c
  (fn [s]
    (let [ss (clojure.string/split s #"-")]
      (apply str (first ss)
        (map clojure.string/capitalize (rest ss))))))

(defcheck solution-8f25491
  (fn [s] (clojure.string/replace s #"-[a-z]" #(clojure.string/upper-case (subs % 1 2)))))

(defcheck solution-8f46426e
  (fn [s]
    (let [parts (re-seq #"\w+" s)]
      (apply str (first parts) (map clojure.string/capitalize (rest parts))))))

(defcheck solution-8f87b404
  (fn [s]
    (reduce #(str % (clojure.string/capitalize %2))
      (clojure.string/split s #"-"))))

(defcheck solution-8fa37c5f
  (fn [s] (clojure.string/replace s #"-[a-z]" #(clojure.string/upper-case (str (second %))))))

(defcheck solution-8fc1cdab
  (fn camelCase[var] (clojure.string/replace var #"-(\w)" #(clojure.string/upper-case (second %1)))))

(defcheck solution-900df914
  (fn camel-case [s]
    (let [[fst & rst] (clojure.string/split s #"-")]
      (clojure.string/join ""
        (cons fst (map clojure.string/capitalize rst))))))

(defcheck solution-90b27a8a
  #(let [words (clojure.string/split % #"\W")
         capWords (map clojure.string/capitalize (rest words))]
     (reduce str (first words) capWords)))

(defcheck solution-90ca93b2
  (fn to-camel-case [s]
    (->> (loop [s (seq s)
                r ()]
           (if s
             (if (= \- (first s))
               (if (next s)
                 (recur (next (next s)) (cons (clojure.string/upper-case (first (next s))) r))
                 r)
               (recur (next s) (cons (first s) r)))
             r))
      reverse
      (apply str))))

(defcheck solution-90e5cf34
  (fn [s] (clojure.string/replace s #"(-)(.)" #(clojure.string/upper-case (% 2)))))

(defcheck solution-910994ec
  (fn [s]
    (let [splits (clojure.string/split s #"-")
          camel-cased (cons (first splits) (map clojure.string/capitalize (rest splits)))]
      (clojure.string/join camel-cased))))

(defcheck solution-910bfd5d
  (fn camelize [s]
    (let [words (clojure.string/split s #"-")
          capitalize #(cons (char-upper-case (first %)) (rest %))]
      (apply str (first words) (mapcat capitalize (rest words))))))

(defcheck solution-9129e72b
  (fn [s] (let [capitalize #(str (clojure.string/upper-case (subs % 0 1)) (.toLowerCase(subs % 1)))]
            (->> s (re-seq #"[^-]+")
              (map-indexed #(if (pos? %1) (capitalize %2) %2)) (apply str)))))

(defcheck solution-916e83
  #(let [words (clojure.string/split % #"[^\w]+")]
     (apply str (concat [(first words)]
                  (map clojure.string/capitalize (rest words))))))

(defcheck solution-91fcc4e1
  #(
    (fn[n] (str (first n) (apply str (map clojure.string/capitalize (rest n)))))
    (clojure.string/split %  #"-")))

(defcheck solution-92381af
  #(let [strs (re-seq #"[a-zA-Z]+" %)
         cap (fn [s] (reduce str (clojure.string/upper-case (str (first s))) (rest s)))]
     (if (= 1 (count strs))
       (first strs)
       (reduce str (conj (map cap (rest strs)) (first strs))))))

(defcheck solution-92b22d47
  #(let [[x & y] (.split % "-")]
     (apply str x
       (mapcat (fn [[a & b]] (cons (clojure.string/upper-case (str a)) b)) y))))

(defcheck solution-92ba2282
  (fn [in] (clojure.string/replace in #"-[a-z]" #(clojure.string/upper-case (str (last %))))))

(defcheck solution-92bf890f
  #(clojure.string/replace % #"-(.)" (comp clojure.string/upper-case last)))

(defcheck solution-92cb10a4
  (fn [s]
    (->> s
      (reduce (fn [[acc fun] ch]
                (if (= ch \-)
                  [acc #(char-upper-case %)]
                  [(conj acc (fun ch)) identity]))
        [[] identity])
      first
      (apply str))))

(defcheck solution-92f05f55
  (fn camel-case [input]
    (let [lst (clojure.string/split input #"-")]
      (clojure.string/join (concat [(first lst)] (map clojure.string/capitalize (rest lst)))))))

(defcheck solution-93295942
  #(let [[f & rst] (clojure.string/split % #"-")]
     (apply str f (map clojure.string/capitalize rst))))

(defcheck solution-93449107
  (fn[x]
    (apply str
      (reduce
        #(if(= \- (last %1))
           (concat (drop-last %1)(clojure.string/upper-case (str %2)))
           (concat %1 (str %2)))
        ""
        x))))

(defcheck solution-94029f55
  (fn [s]
    (let [ss (clojure.string/split s #"\-")
          first-word (first ss)
          capitalized-rest (map clojure.string/capitalize (rest ss))]
      (clojure.string/join (concat [first-word] capitalized-rest)))))

(defcheck solution-941880da
  (fn [s]
    (let [ws (clojure.string/split s #"-")]
      (apply str (cons (first ws) (map clojure.string/capitalize (rest ws)))))))

(defcheck solution-9490c5d1
  (fn [s]
    (let [news (.split  s "-")]
      (reduce str (cons (first news) (map #(str (clojure.string/upper-case (str (first %1)))  (reduce str (rest %1)))     (rest news) )))
      )
    ))

(defcheck solution-949277f2
  (fn [s]
    (loop [t [] f identity [c & r] s]
      (cond (nil? c) (apply str t)
            (= c \-) (recur t clojure.string/upper-case r)
            :else (recur (conj t (f c)) identity r)))))

(defcheck solution-94eeebca
  (fn [s]
    (let [l (sequence (.split s "-")) l1 (first l) lr (map clojure.string/capitalize (rest l))]
      (apply str (cons l1 lr)))))

(defcheck solution-959e421c
  (fn icc [x]
    (loop [s x result '()]
      (if (empty? s)
        (clojure.string/join "" result)
        (if (= \- (last s))
          (recur (drop-last s) (cons (clojure.string/upper-case (str (first result)))(drop 1 result)))
          (recur (drop-last s) (cons (last s) result)))))))

(defcheck solution-9667da18
  (fn [s] (clojure.string/replace s #"\-." #(clojure.string/upper-case (str (last %))))))

(defcheck solution-96716c28
  #((fn [[x & xs]] (clojure.string/join (conj (map clojure.string/capitalize xs) x)))
    (clojure.string/split % #"-")))

(defcheck solution-968a7c5
  (fn prob-0102
    [wd-str]
    (let [wd-seq (clojure.string/split wd-str #"-")
          wd-cam (cons (first wd-seq) (map #(clojure.string/capitalize %) (rest wd-seq))) ]
      (apply str wd-cam))))

(defcheck solution-96ae88b7
  (fn [s]
    (letfn [(split-words [s] (seq (.split s "-")))
            (capword [w] (clojure.string/capitalize w))
            (cap-first [[h & t]] (cons h (map capword t)))
            (rejoin [coll] (apply str coll))]
      (-> s
        (split-words)
        (cap-first)
        (rejoin)
        ))
    ))

(defcheck solution-96bd87be
  #(letfn [(up [s] (apply str (clojure.string/upper-case (first s)) (rest s)))]
     (reduce (fn [r s] (str r (up s))) (clojure.string/split % #"-"))))

(defcheck solution-96e864fc
  (fn [s]
    (let [[h & xs] (clojure.string/split s #"-")
          camels (map clojure.string/capitalize xs)]
      (clojure.string/join "" (concat h camels)))))

(defcheck solution-96f8ab90
  #(let [[s & ss] (clojure.string/split % #"-")]
     (clojure.string/join (cons s (map clojure.string/capitalize ss)))))

(defcheck solution-97077a6a
  (fn [s]
    (let [[word & words] (clojure.string/split s #"-")]
      (apply str word (map #(clojure.string/capitalize %) words)))))

(defcheck solution-975dfd3c
  (fn[x]
    (let [[a & b] (clojure.string/split x #"-")]
      (apply str
        (clojure.string/join
          (conj
            (map
              #(apply str (conj (rest %) (clojure.string/upper-case (first %))))
              b)
            a))))))

(defcheck solution-97987bb6
  (fn [s] (apply str (reduce #(concat %1 (concat (clojure.string/upper-case (str (first %2))) (rest %2)))
                       (clojure.string/split s #"\-")))))

(defcheck solution-97e1636b
  (fn [multi-word-key]
    (let [[word1 & more] (clojure.string/split multi-word-key #"-")]
      (apply str
        word1
        (map clojure.string/capitalize more)))))

(defcheck solution-980b0b95
  (fn [x]
    (let [words (clojure.string/split x #"-")
          capitalized (cons (first words) (map clojure.string/capitalize (rest words)))]
      (clojure.string/join "" capitalized))
    ))

(defcheck solution-981d4a9e
  (fn [s]
    (let [[fst-word & other-words] (clojure.string/split s #"-+")
          capitalized (into [fst-word]
                        (map clojure.string/capitalize other-words))]
      (apply str capitalized))))

(defcheck solution-98bf5a7c
  (fn [s]
    (let [p (clojure.string/split s #"-")]
      (letfn [(tc [s] (apply str (char-upper-case (first s)) (rest s)))]
        (apply str (first p) (map tc (rest p)))))
    ))

(defcheck solution-98f3897d
  (fn [s]
    (when-let [[word & words] (re-seq #"[^\-]+" s)]
      (apply str word
        (for [[w & rest] words] (apply str (char-upper-case w) rest))))))

(defcheck solution-992243d5
  (fn [word]
    (clojure.string/replace word #"-([a-z])"
      #(clojure.string/upper-case (second %))
      )
    ))

(defcheck solution-9923f532
  (fn into-camel-case [s]
    (let [v (clojure.string/split s #"-\b")
          up-first-letter
            (fn [cs]
              (apply str (char-upper-case (first cs)) (rest cs)))]
      (apply str (first v) (map up-first-letter (rest v))))))

(defcheck solution-99597fa8
  #(clojure.string/replace % #"(-)([a-z])" (comp clojure.string/upper-case last)))

(defcheck solution-995a50a9
  #(let [words (clojure.string/split % #"-")]
     (apply str (cons (first words)
                  (map clojure.string/capitalize (rest words))))))

(defcheck solution-9971bbc5
  (fn camel-case [string]
    (let [words (clojure.string/split string #"-")]
      (clojure.string/join (cons (first words)
                             (map clojure.string/capitalize (rest words)))))))

(defcheck solution-99c7bf6c
  (fn u [[c & m]]
    (when c
      (if (= c \-)
        (str (char-upper-case (first m)) (u (rest m)))
        (str c (u m))))))

(defcheck solution-9a1e4859
  (fn [ss]
    (-> ss
      (clojure.string/replace #"-[a-z]" #(clojure.string/upper-case %))
      (clojure.string/replace #"-" ""))))

(defcheck solution-9a7a324b
  (fn [s]
    (if-not (nil? (re-find #"-" s))
      (let [split (clojure.string/split s #"-")]
        (str
          (first split)
          (clojure.string/join
            (map clojure.string/capitalize (rest split)))))
      s)))

(defcheck solution-9a7f4cd6
  #(clojure.string/replace % #"-(\w)" (fn [[_ l]] (clojure.string/upper-case l))))

(defcheck solution-9adb5471
  #(clojure.string/replace % #"-([a-z])" (fn [[x y]] (clojure.string/upper-case y) )))

(defcheck solution-9b12ebf6
  (fn [x]
    (let [splitted (clojure.string/split x #"-")
          firstword (first splitted)
          restwords (map clojure.string/capitalize (rest splitted))
          combinedResult (cons firstword restwords)
          ]
      (apply str combinedResult))))

(defcheck solution-9b2eb9bd
  (fn [x]
    (loop [y []
           z x]
      (condp = (first z)
        nil (apply str y)
        \- (recur
             (conj y (clojure.string/upper-case (str (second z))))
             (drop 2 z))
        (recur
          (conj y (first z))
          (rest z))))))

(defcheck solution-9ba20dc1
  #(->>
     (clojure.string/split % #"-")
     ((fn [[head & tail]] (cons head (map clojure.string/capitalize tail))))
     clojure.string/join))

(defcheck solution-9bf16041
  (fn [s]
    (let [
          camel #(str (char-upper-case (first %)) (apply str (rest %)))
          words (.split s "-")]
      (str (first words) (apply str (map camel (rest words)))))))

(defcheck solution-9bf3e9f7
  (fn [k]
    (let [split (clojure.string/split k #"-")]
      (if (empty? (rest split)) k
                                (apply str
                                  (cons (first split)
                                    (map (fn [w] (clojure.string/replace w #"^." #(clojure.string/upper-case %)))
                                      (rest split))))))))

(defcheck solution-9cb49b07
  (fn [s] (reduce  #(str % (clojure.string/capitalize %2)) (clojure.string/split s #"-"))))

(defcheck solution-9d01b868
  (fn camel [s]
    (clojure.string/join
      ""
      (loop [s (seq s), up false, res []]
        (if (seq s)
          (if (= (first s) \-)
            (recur (rest s) true res)
            (if up
              (recur (rest s) false (conj res (char-upper-case (first s))))
              (recur (rest s) false (conj res (first s)))))
          res)))))

(defcheck solution-9d0944ab
  (fn [s] (apply str (map-indexed #(if (zero? %) %2 (clojure.string/capitalize %2)) (.split s "-")))))

(defcheck solution-9d3aecdc
  #(clojure.string/join ""
     (map (fn [[a b]]
            (if (= \- b)
              ""
              (if (= \- a)
                (char-upper-case b)
                b)))
       (partition 2 1 (cons \a (seq %))))))

(defcheck solution-9d5e2d03
  (fn [s] (clojure.string/replace s #"-." #(clojure.string/upper-case (subs % 1 2)))))

(defcheck solution-9d60502a
  (fn camel-case [s]
    (let [ws (clojure.string/split s #"-")]
      (apply str
        (first ws)
        (clojure.string/join
          (map clojure.string/capitalize (rest ws)))))))

(defcheck solution-9d690e86
  (fn camelcase [word] (reduce #(str % (clojure.string/capitalize %2)) (re-seq #"\w+" word))))

(defcheck solution-9eb3ced2
  (fn [z] (let [[h & l] (clojure.string/split z #"-")
                l1 (apply concat (map #(map-indexed vector %) l))]
            (str h (apply str (for [[i v] l1] (if (zero? i) (clojure.string/upper-case v) (str v))))))))

(defcheck solution-9f7754c1
  (fn [s]
    (let [[h & t] (re-seq #"[^-]+" s) ]
      (apply str (cons h (map clojure.string/capitalize t)))
      )
    ))

(defcheck solution-9fcce604
  (fn [word]
    (let [[f & r] (clojure.string/split word #"-")]
      (str f (clojure.string/join "" (map clojure.string/capitalize r))))))

(defcheck solution-a006c1c6
  #(clojure.string/replace % #"-[a-z]" (comp clojure.string/capitalize last)))

(defcheck solution-a00771c2
  (fn [s]
    (let [s (clojure.string/split s #"-")]
      (if (== 1 (count s))
        (first s)
        (apply str (concat (first s) (mapcat (fn [s] [(clojure.string/upper-case (subs s 0 1)) (subs s 1)]) (rest s))))))))

(defcheck solution-a11f99a1
  #(clojure.string/replace % #"-(.)" (fn [x] (clojure.string/upper-case (second x)))))

(defcheck solution-a12aecb5
  (fn [word]
    (let [[part & parts] (clojure.string/split word #"-")]
      (apply str part
        (map clojure.string/capitalize parts)))))

(defcheck solution-a1598ecd
  (fn [word]
    (let [parts (clojure.string/split word #"-")]
      (apply str (cons (first parts) (map clojure.string/capitalize (rest parts))))
      )
    ))

(defcheck solution-a1a87c03
  (fn f [s] (clojure.string/replace s #"-." #(->> % rest (apply str) clojure.string/capitalize))))

(defcheck solution-a1b4ad14
  #(let [[a & b] (re-seq #"\w+" %)]
     (apply str a
       (map clojure.string/capitalize b))))

(defcheck solution-a20481d6
  #(clojure.string/join (loop [d [] l (vec %)]
                          (if (empty? l) d
                                         (let [h (= (first l) \-)]
                                           (recur
                                             (conj d (if h
                                                       (clojure.string/upper-case (second l))
                                                       (first l)))
                                             (if h (drop 2 l) (rest l))))))))

(defcheck solution-a24666d3
  (comp
    (partial apply str)
    (partial remove (partial = \-))
    #(map
       (fn [c p] (if (= p \-) (char-upper-case c) c))
       (seq %) (cons \_ (seq %)))))

(defcheck solution-a2af1cc8
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (str (first words) (apply str (map clojure.string/capitalize (rest words))))
      )
    ))

(defcheck solution-a2e6de23
  #(apply str
     (let [[a & b] (clojure.string/split % #"-")]
       (cons a
         (map clojure.string/capitalize b)))))

(defcheck solution-a2fd20d7
  (fn [s]
    (let [words (clojure.string/split s #"-")
          upcase (fn [w] (let [chrs (seq w)]
                           (apply str (clojure.string/upper-case (first chrs)) (rest chrs) )))
          ]
      (if (= 1 (count words))
        (first words)
        (apply str (first words) (map upcase (rest words)))
        )
      )
    ))

(defcheck solution-a31f95a2
  (fn [s]
    (apply str (first s)
      (map #(cond
              (= % \-) nil
              (= %2 \-) (char-upper-case %)
              :else %) (rest s) s))))

(defcheck solution-a333837c
  (fn [s]
    (let [i (.indexOf s "-")]
      (if (< i 0)
        s
        (recur (str (.substring s 0 i)
                 (clojure.string/upper-case (.substring s (+ i 1) (+ i 2)))
                 (.substring s (+ i 2))))))))

(defcheck solution-a384886d
  (fn intoCamelCase [x]
    (loop [x x res '[]]
      (cond (empty? x) (apply str res)
            (= \- (first x)) (recur (nnext x) (conj res (clojure.string/upper-case (second x))))
            :else (recur (rest x) (conj res (first x)))
            )
      )
    ))

(defcheck solution-a3a60d1b
  #(clojure.string/replace % #"-(\w)" (fn [[_ w]] (clojure.string/capitalize w))))

(defcheck solution-a3c0b2d8
  (fn [s]
    (reduce #(clojure.string/replace %1 %2 (clojure.string/capitalize (subs %2 1)))
      s
      (re-seq #"-[a-z]" s))))

(defcheck solution-a3c26dd4
  (fn intoCamelCase [word]
    (let [splitted (clojure.string/split word #"-")]
      (clojure.string/join (cons (first splitted) (map clojure.string/capitalize (rest splitted)))))))

(defcheck solution-a41a8698
  (fn [s]
    (let [words (clojure.string/split s #"-")
          adjusted-words `(~(first words) ~@(map clojure.string/capitalize (rest words)))]
      (clojure.string/join adjusted-words))))

(defcheck solution-a42688db
  (fn [s]
    (let [join (partial clojure.string/join "")
          uc clojure.string/upper-case
          ucfirst (fn [[c & cs :as s']]
                    (if (seq s')
                      (->> cs
                        (apply str)
                        (into [(uc c)])
                        join)
                      s'))
          [w & ws] (clojure.string/split s #"-+")]
      (->> ws
        (map ucfirst)
        (into [w])
        join))))

(defcheck solution-a4591c55
  (fn [s] (.replace (.replace s  "-w" "W") "-k" "K")))

(defcheck solution-a49db862
  (fn m102 [s]
    (let [row (clojure.string/split s #"-")]
      (if (> (count row) 1)
        (clojure.string/join
          (cons (first row) (map clojure.string/capitalize (rest row))))
        s))))

(defcheck solution-a4baaa4e
  #(let [words (clojure.string/split % #"-")]
     (clojure.string/join
       (cons
         (first words)
         (map clojure.string/capitalize (rest words))))))

(defcheck solution-a4de0e17
  (fn camel [s]
    (let [[head & tail] (clojure.string/split s #"\W")]
      (apply str head (map clojure.string/capitalize tail)))))

(defcheck solution-a55a7aa1
  #(let [s (re-seq #"\w+" %)]
     (if (= 1 (count s)) %
                         (apply str (first s) (map clojure.string/capitalize (rest s))))))

(defcheck solution-a55b1583
  (fn [x]
    (let [arr (clojure.string/split x #"-")]
      (reduce (fn [t v] (str t (clojure.string/capitalize v) )) (first arr) (rest arr)))))

(defcheck solution-a57e7b9f
  #(let [[f & rst] (clojure.string/split % #"-")]
     (apply str f (map clojure.string/capitalize rst))))

(defcheck solution-a5a7a544
  (fn [s]
    (let
     [[x & xs] (.split s "-")]
      (apply str (cons x (map #(clojure.string/capitalize %) xs))))))

(defcheck solution-a5f5c859
  (fn [s]
    (let [
          words (re-seq #"[^\-]+" s)]
      (clojure.string/join (cons (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-a6b9413d
  (fn [s]
    (let
     [words (clojure.string/split s #"-")]
      (clojure.string/join "" (cons (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-a793b4a1
  (fn [s]
    (apply str (concat (list (first s))
                 (for [pair (partition 2 1 s)]
                   (cond
                     (= \- (first pair)) (clojure.string/upper-case (str (second pair)))
                     (= \- (second pair)) ""
                     :else (second pair)))))
    ))

(defcheck solution-a7e0e4b2
  (fn [i] (apply str(clojure.string/replace i #"-(.)" #(clojure.string/upper-case (get % 1)) ))))

(defcheck solution-a809d121
  (fn [t] (clojure.string/replace t
            #"-[a-z]"
            #(clojure.string/upper-case (last %1)))))

(defcheck solution-a86edeea
  (fn tocamel [x]
    (let [s (.split x "-")]
      (apply str (first s) (map #(str (clojure.string/upper-case (str (.charAt % 0)))
                                   (.substring % 1 (count %)))
                             (rest s))))))

(defcheck solution-a9200bf9
  (fn [s]
    (apply str (reduce
                 (fn [acc v] (if (= 0 (count acc)) (conj acc v) (conj acc (clojure.string/capitalize v))))
                 [] (re-seq #"\w+" s)))))

(defcheck solution-a92c5d87
  (fn [input-str]
    (reduce #(str %1 (clojure.string/capitalize %2))
      (clojure.string/split input-str #"-"))))

(defcheck solution-a943f189
  (fn [s]
    (let [ls (clojure.string/split s #"-")
          ls2 (concat [(first ls)] (map clojure.string/capitalize (rest ls)))
          s2 (clojure.string/join "" ls2)
          ]
      s2)))

(defcheck solution-a968a688
  (fn camel-case [field]
    (let [[head & tail] (clojure.string/split field
                          #"-")]
      (str head
        (->> tail
          (mapcat clojure.string/capitalize)
          (apply str))))))

(defcheck solution-aa1e47e5
  (fn camel [s]
    (let [words (clojure.string/split s #"-")]
      (clojure.string/join (concat (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-aa22c286
  (fn [s]
    (let [ss (clojure.string/split s #"-")]
      (apply str (first ss)
        (map clojure.string/capitalize (rest ss))))))

(defcheck solution-aab55a3c
  (fn [s]
    (let [parts (clojure.string/split s #"\-" )
          ccc (rest parts)
          cc (map clojure.string/capitalize ccc)
          c (cons (first parts) cc)
          ]
      (apply str c)
      )
    ))

(defcheck solution-aab85dd7
  #(let [l (re-seq #"\w+" %)] (apply str (cons (first l) (map clojure.string/capitalize (rest l))))))

(defcheck solution-aac4ea9c
  (fn [s]
    (let [[w & ws] (clojure.string/split s #"-")]
      (clojure.string/join (cons w (map clojure.string/capitalize ws))))))

(defcheck solution-aacb5163
  (fn cc [s]
    (letfn [(helper [ss]
              (if (empty? ss) '()
                              (let [c (first ss)
                                    d (second ss)]
                                (if (= c \-)
                                  (cons (clojure.string/upper-case (str d)) (helper (nthrest ss 2)))
                                  (cons c (helper (rest ss)))))))]
      (apply str (helper s)))))

(defcheck solution-aaebe972
  (fn [s]
    (let [[f & r] (clojure.string/split s #"-")]
      (apply str (cons f (map clojure.string/capitalize r))))))

(defcheck solution-aafaad78
  (fn slug->camel [s]
    (let [ws (clojure.string/split s #"-")]
      (apply str (first ws) (map clojure.string/capitalize
                              (rest ws))))))

(defcheck solution-ab9e7a7a
  #(let [[s1 & sr] (clojure.string/split % #"\-")]
     (apply str s1
       (map (fn [[c1 & cr]]
              (apply str (clojure.string/upper-case c1) cr))
         sr))))

(defcheck solution-aba10114
  (fn [cc]
    (let [sp (vec (.split cc "-"))]
      (apply str (cons (first sp)
                   (map
                     #(apply str (cons
                                   (char-upper-case
                                     (first %)) (rest %)))
                     (rest sp)))))))

(defcheck solution-abaa0ae9
  (fn [s]
    (let [ws (vec (.split s "-"))]
      (apply str (first ws)
        (for [w (rest ws)]
          (apply str (char-upper-case (first w)) (rest w)))))))

(defcheck solution-abdc4aae
  (fn [s]
    (let [$ (clojure.string/split s #"-")]
      (str (first $)
        (apply str (map clojure.string/capitalize (rest $)))))))

(defcheck solution-abfefd0
  (fn [s] (clojure.string/replace s #"-[a-z]" (fn [match] (clojure.string/upper-case (second match))))))

(defcheck solution-ac0f0fd
  #(clojure.string/join
     (let [[hd & tl] (clojure.string/split % #"-")]
       `(~hd, ~@(map clojure.string/capitalize tl)))))

(defcheck solution-ac92bd90
  (fn [s]
    (->> (clojure.string/split s #"-")
      (#(into (conj [] (first %)) (map clojure.string/capitalize (rest %))))
      (apply str))))

(defcheck solution-ac92d5fe
  (comp {\s "something" \m "multiWordKey" \l "leaveMeAlone"} first))

(defcheck solution-aca0326f
  (fn f [s]
    (let [words (clojure.string/split s #"-")]
      (clojure.string/join (cons (first words) (map clojure.string/capitalize (rest words)))) )))

(defcheck solution-accc1419
  (fn [snake-case] (clojure.string/replace snake-case #"-(.)" (fn [m] (clojure.string/upper-case (second m))))))

(defcheck solution-ad0f5657
  (fn [s]
    (apply str
      (for [t (partition 2 1 (str "0" s))] (if (= \- (second t)) ""
                                                                 (if (= \- (first t))
                                                                   (char-upper-case (second t)) (second t)))))))

(defcheck solution-ad811b86
  (fn [s]
    (apply
      str
      ((juxt
         first
         (comp
           (partial
             apply
             str)
           (partial
             map
             clojure.string/capitalize)
           rest))
       (clojure.string/split
         s
         #"-")))))

(defcheck solution-ad82bbbb
  (fn [j]
    (let [x (clojure.string/split j #"-")
          y #(apply str (clojure.string/upper-case (first %)) (next %))
          z (fn h[s] (apply str (y (first s)) (if (next s) (h (next s)) "")))
          ]
      (apply str (first x) (if (next x) (z (next x)) ""))

      )
    ))

(defcheck solution-ad860ae9
  #(clojure.string/replace % #"-(\w)" (comp clojure.string/upper-case second)))

(defcheck solution-ad9a9fab
  (fn [s]
    (let [ss (clojure.string/split s #"-")]
      (str (first ss)
        (clojure.string/join
          (map
            clojure.string/capitalize
            (rest ss)))))))

(defcheck solution-ada12a01
  (fn [s]
    (clojure.string/replace s
      #"-(.)" (fn [m]
                (str (char-upper-case (first (nth m 1))))))))

(defcheck solution-ae7a8a97
  (fn [s]
    (let [[w & ws] (clojure.string/split s #"\W")]
      (apply str w (map clojure.string/capitalize ws)))))

(defcheck solution-ae8961d1
  (fn [s]
    (let [[f & r] (clojure.string/split s #"-")]
      (apply str f (when-let [r (seq r)] (map clojure.string/capitalize r))))))

(defcheck solution-aeb280dd
  #(let [[x & xs] (clojure.string/split % #"-")] (clojure.string/join (conj (map clojure.string/capitalize xs) x))))

(defcheck solution-aed68fb3
  (fn [string]
    (reduce
      (fn [acc v]
        (str acc (clojure.string/capitalize v)))
      (first (clojure.string/split string #"-"))
      (rest (clojure.string/split string #"-")))))

(defcheck solution-af644479
  (fn [s] (apply str (reduce concat [(first (clojure.string/split s #"-"))] (map (fn [ss](concat (#(clojure.string/upper-case (first %1)) ss) (rest ss))) (rest (clojure.string/split s #"-")))))))

(defcheck solution-af7c4a92
  #(let [[f & r] (.split % "-")]
     (apply str f (map clojure.string/capitalize r))))

(defcheck solution-af87f391
  (fn int-cc [s]
    (let [sp (clojure.string/split s #"\-+")]
      (apply str (first sp) (map #(apply str (clojure.string/upper-case (str (first %))) (rest %)) (rest sp)))
      )
    ))

(defcheck solution-afb883e8
  (fn ->camel [s]
    (if (< 0 (.indexOf s "-"))
      (let [_s (clojure.string/join (map clojure.string/capitalize (clojure.string/split s #"-")))]
        (apply str (clojure.string/lower-case (first _s)) (rest _s)))
      s)))

(defcheck solution-afc06506
  (fn [word]
    (let [words (.split word "-")]
      (clojure.string/join
        (cons
          (first words)
          (map
            #(str (clojure.string/upper-case (.substring % 0 1)) (.substring % 1))
            (rest words)))))))

(defcheck solution-afc920a5
  (fn [x]
    (let [ls (clojure.string/split x #"-")
          xs (cons (first ls)
               (map clojure.string/capitalize (rest ls)))]
      (clojure.string/join "" xs))))

(defcheck solution-afebe065
  (fn [string]
    (clojure.string/replace
      (clojure.string/replace string #"-." #(clojure.string/upper-case %))
      #"-" "")))

(defcheck solution-b073c17a
  #(let [s (re-seq #"\w+" %)]
     (reduce str (concat (take 1 s) (map clojure.string/capitalize (drop 1 s))))))

(defcheck solution-b0b0962a
  #(apply str
     (for [[a b] (re-seq #"-?." %)]
       (if b
         (char-upper-case b)
         a))))

(defcheck solution-b0c7e1b
  (fn tocamelcase [s]
    (let [capifhyphen (fn [a b]
                        (if (not= b \-)
                          (if (= a \-)
                            (char-upper-case b)
                            b)))]
      (apply str (map-indexed #(capifhyphen (get s (dec %1)) %2) s)))))

(defcheck solution-b0fa9722
  (fn kebab->camel [st]
    (let [[word & words] (clojure.string/split st #"-")]
      (apply str word (map clojure.string/capitalize words)))))

(defcheck solution-b14ece8b
  (fn [s]
    (apply
      str
      (let [strs (.split s "-")]
        (conj
          (for [p (rest strs)]
            (str (clojure.string/upper-case (subs p 0 1)) (subs p 1)))
          (first strs))))))

(defcheck solution-b1991389
  (fn camel-case [s]
    (letfn [(capitalize [s]
              (str (char-upper-case (first s)) (apply str (rest s))))]
      (reduce (fn [s1 s2]
                (if (= s2 "-") s1
                               (str s1 (capitalize s2))))
        (map #(apply str %) (partition-by #{\-} s))))))

(defcheck solution-b1f4621
  (fn [s]
    (let [ws (.split s "-")]
      (if (= (count ws) 1)
        s
        (apply str
          (cons (first ws)
            (map
              (fn [w] (apply str (cons (char-upper-case (first w)) (rest w)))) (rest ws))))))))

(defcheck solution-b24e6c79
  #(apply str (map (fn [[a b]] (if (= b \-)
                                 ""
                                 (if (= a \-)
                                   (char-upper-case b)
                                   b)))
                (partition 2 1 (str " " %)))))

(defcheck solution-b2bdc796
  (fn [s]
    (let [ss (clojure.string/split s #"-")
          uc #(apply str (cons (char-upper-case (first %)) (rest %)))]
      (apply str (cons (first ss) (map uc (rest ss)))))))

(defcheck solution-b2c71afe
  (fn [s](reduce #(str % ((fn [[h & t]](str (clojure.string/upper-case (str h))(apply str t))) %2)) (.split s "-"))))

(defcheck solution-b3078ce0
  (fn [s]
    (let [c (clojure.string/split s #"-")]
      (apply str (cons (first c) (map #(clojure.string/capitalize %) (rest c)))))))

(defcheck solution-b35fc221
  (fn [x] (clojure.string/replace x  #"-\w" #(clojure.string/upper-case (subs %1 1)))))

(defcheck solution-b38b0de7
  (fn [s]
    (loop [s s c 0 r []]
      (cond
        (empty? s) (apply str r)
        (= (first s) \-) (recur (rest s) 1 r)
        (= c 1) (recur (rest s) 0 (conj r (clojure.string/upper-case (first s))))
        :else (recur (rest s) 0 (conj r (first s)))))))

(defcheck solution-b3c2fff1
  (fn [s]
    (let [wl (.split s "-")
          [x & r] wl]
      (apply str (cons x (mapcat #(clojure.string/capitalize %1) r))))))

(defcheck solution-b485c851
  (fn [arg]
    (let [wordColl (clojure.string/split arg #"-")]
      (if (= (count wordColl) 1)
        (first wordColl)
        (apply str
          (first wordColl)
          (map clojure.string/capitalize (drop 1 wordColl)))))))

(defcheck solution-b56ab3a6
  (fn intoCamelCase [s]
    (let [[x & xs] (re-seq #"\w+" s)]
      (apply str x (map #(apply str (-> % first str clojure.string/upper-case) (rest %)) xs)))))

(defcheck solution-b6bc6a27
  #(let [ss (.split % "-")]
     (apply str
       (first ss)
       (map (fn [[f & r]]
              (apply str (char-upper-case f) r))
         (rest ss)))))

(defcheck solution-b6bfecec
  (fn [c]
    (clojure.string/replace c #"-(.)" #(clojure.string/upper-case (second %)))))

(defcheck solution-b724500
  #( apply str ((juxt first (comp (partial apply str) (partial mapcat clojure.string/capitalize ) next)) (clojure.string/split % #"-"))))

(defcheck solution-b73d75ac
  (fn [s]
    (let [[first-s & rest-s] (clojure.string/split s #"-")]
      (apply str
        first-s
        (map clojure.string/capitalize rest-s)))))

(defcheck solution-b7838d79
  (fn [s]
    (let [l (clojure.string/split s #"-")]
      (apply str (cons (first l) (map clojure.string/capitalize (rest l)))))))

(defcheck solution-b84f8357
  #(->> (re-seq #"[^-]+" %)
     ((fn [[f & r]]
        (cons f (map clojure.string/capitalize r))))
     (clojure.string/join "")))

(defcheck solution-b8608a5
  (fn [string]
    (let [words (clojure.string/split string #"-")]
      (clojure.string/join (cons (first words) (map clojure.string/capitalize (rest words)))))))

(defcheck solution-b86bdca8
  (fn _camel-case [s]
    (let [words (re-seq #"\w+" s)]
      (apply str (first words)

        (for [each (next words)]
          (apply str (clojure.string/upper-case (first each))
            (next each)))))))

(defcheck solution-b8efad89
  (fn into-camel-case
    [s]
    (let [ws (clojure.string/split s #"-")]
      (reduce
        #(str %1 (clojure.string/capitalize %2))
        (first ws)
        (rest ws)))))

(defcheck solution-b90c790d
  (fn [s] (let [a (.split s "-")] (apply str (cons (first a) (map #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1)) (rest a)))))))

(defcheck solution-b91e4540
  (fn [s]
    (first
      (reduce
        (fn [[r l] n]
          (if (= l \-)
            [(str r (char-upper-case n)) n]
            (if (= n \-)
              [r n]
              [(str r n) n])))
        ["" nil]
        s))
    ))

(defcheck solution-b979ce95
  (fn intoCamelCase [s]
    (apply str
      (map #(if-let [x (second %)]
              (char-upper-case x) %)
        (re-seq #"-?." s)))))

(defcheck solution-b997f9ae
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (conj (map clojure.string/capitalize (rest words)) (first words))))))

(defcheck solution-b9aef276
  (fn [w]
    (apply str
      (reduce
        (fn [acc x]
          (if (= \- (last acc))
            (conj (vec (butlast acc)) (char-upper-case x))
            (conj acc x)))
        [] w))))

(defcheck solution-b9b63319
  (fn [s]
    (let [xs (clojure.string/split s #"-")]
      (apply str
        (reduce
          #(concat %1 (clojure.string/capitalize %2))
          xs
          )
        )
      )
    ))

(defcheck solution-ba05aea2
  (fn [s]
    (reduce #(str %1 (clojure.string/capitalize %2)) (clojure.string/split s #"\-"))))

(defcheck solution-ba68fb68
  (fn CamelCase [s]
    (let [re (re-seq #"[a-zA-Z]+" s)
          rzt (cons (first re) (map clojure.string/capitalize (rest re)))]
      (apply str rzt))))

(defcheck solution-baa3661c
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str (cons (first words) (map clojure.string/capitalize (rest words))))
      )
    ))

(defcheck solution-baa9f011
  #(let [s (clojure.string/split % #"-")]
     (apply str (first s) (map clojure.string/capitalize (rest s)))))

(defcheck solution-bb404b13
  #(let [l (clojure.string/split % #"-")]
     (apply str (first l) (map clojure.string/capitalize (rest l)))))

(defcheck solution-bb92223b
  (fn [s]
    (let [upcase-word (fn [word]
                        (cons (char-upper-case (first (seq word)))
                          (rest (seq word))))
          parts (map second (re-seq #"(?:(\w+)[-_]?)" s))]
      (apply str (flatten (reduce #(conj %1 (upcase-word %2))
                            (vec (seq (first parts)))
                            (rest parts)))))))

(defcheck solution-bba39f32
  #(let [[x & xs] (clojure.string/split % #"-")]
     (->> (cons x (map clojure.string/capitalize xs))
       (apply str))))

(defcheck solution-bbdd6002
  (fn camel->case
    [that]
    (let [[head & tail] (clojure.string/split that #"-")]
      (apply str
        (reduce
          (fn [xs x]
            (conj xs (apply str (conj (rest x) (clojure.string/upper-case (first x))))))
          [head] tail)))))

(defcheck solution-bcb6ab34
  #(if-let [parts (re-seq #"\w+" %)]
     (if (= (first parts) %)
       %
       (apply str (first parts)
         (map
           (fn [part]
             (let [c (seq part)]
               (apply str (char-upper-case (first c)) (rest c))))
           (rest parts))))
     %))

(defcheck solution-bcbbbefd
  (fn __ [s]
    (let [x (clojure.string/split s #"-")]
      (apply str (first x)
        (map clojure.string/capitalize (rest x))))))

(defcheck solution-bcf7d151
  (fn [s] (let [ss (.split s "-")]
            (if (= 1 (count ss))
              s
              (apply str (list* (first ss) (map #(str (clojure.string/upper-case (.substring % 0 1))
                                                   (.substring % 1)) (next ss))))))))

(defcheck solution-bd00b5f4
  (fn cc [s] (clojure.string/replace s #"-(\S)" #(str (clojure.string/upper-case (% 1))))))

(defcheck solution-bd158251
  (fn
    [s]
    (let [[f & r] (re-seq #"\w+" s)]
      (reduce #(str % (clojure.string/capitalize %2)) f r)
      )))

(defcheck solution-bd55220b
  {"something" "something"
   "multi-word-key" "multiWordKey"
   "leaveMeAlone" "leaveMeAlone"})

(defcheck solution-bd56a5a6
  (fn camel-case
    [s]
    (let [words (clojure.string/split s #"-")]
      (->>
        words
        (drop 1)
        (map clojure.string/capitalize)
        (concat (take 1 words))
        (clojure.string/join)))))

(defcheck solution-bd67f2fa
  (fn [s]
    (clojure.string/replace s #"-(\w)"
      (fn [[_ c]] (clojure.string/upper-case c)))))

(defcheck solution-bd6fc330
  (fn [s]
    (let [[word & words] (re-seq #"\w+" s)]
      (apply str
        (concat word
          (mapcat (fn [[c & cs]]
                    (conj cs (char-upper-case c))) words))))))

(defcheck solution-bde28a51
  (fn [s]
    (let [words (remove #{[\-]} (partition-by #{\-} s))
          head (first words)
          tail (map #(apply str %) (rest words))]
      (reduce str ""
        (concat head
          (map clojure.string/capitalize tail))))))

(defcheck solution-bde87e35
  (fn [s]
    (reduce
      #(if (= (last %1) \-)
         (str (.substring %1 0 (dec (count %1))) (clojure.string/upper-case (.toString %2)))
         (str %1 %2))
      ""
      s)))

(defcheck solution-bdffcffc
  (fn camel-case [s]
    (let [[a & w] (re-seq #"\w+" s)]
      (apply str a (map #(apply str (clojure.string/upper-case (str (first %))) (rest %)) w)))))

(defcheck solution-be7d30eb
  #(let [[f & rest] (re-seq #"[^\-]+" %)]
     (apply str f
       (map (fn [[f & rest]] (apply str (clojure.string/upper-case (str f)) rest)) rest))))

(defcheck solution-be87c6e
  (fn h [f]
    (letfn [(up1 [s]
              (str (clojure.string/upper-case (.substring s 0 1)) (.substring s 1)))]
      (let [[h & t] (.split f "-")]
        (apply str (concat [h] (map up1 t)))))))

(defcheck solution-bef2ae55
  (fn [w]
    (let [words (re-seq #"[^-]+" w)
          [fst & rst] words]
      (apply str (cons fst (map (fn [[fl & rstl]] (apply str (cons (clojure.string/upper-case (str fl)) rstl))) (rest words)))))))

(defcheck solution-bf682a92
  #(apply str
     (map (fn [[a b]]
            (if b (char-upper-case b) a)) (re-seq #"-?." %))))

(defcheck solution-bf937b96
  (fn [st]
    (->>
      (seq (str " " st))
      (partition 2 1)
      (map #(if (= \- (first %)) (char-upper-case (last %)) (last %)))
      (filter #(not= \- %))
      (apply str))))

(defcheck solution-bfc94d46
  #(let [[head & tail] (re-seq #"\w+" %)] (apply str head (map clojure.string/capitalize tail))))

(defcheck solution-c0247566
  (fn [s] (clojure.string/replace s #"-[a-z]" #(clojure.string/capitalize (last %1)))))

(defcheck solution-c09244eb
  (fn [s]
    (let [[f & r] (clojure.string/split s #"\-")]
      (clojure.string/join (cons f (map clojure.string/capitalize r))))))

(defcheck solution-c0ae7f65
  (fn [s]
    (let [ss (.split s "-")]
      (apply
        str
        (cons
          (first ss)
          (map
            (fn [y]
              (apply
                str
                (cons (char-upper-case (first y)) (rest y))
                )
              )
            (rest ss)
            )
          )
        )
      )
    ))

(defcheck solution-c0bc5ccd
  (fn [s]
    (let [tokens (clojure.string/split s #"-")]
      (str (first tokens)
        (clojure.string/join
          (map clojure.string/capitalize (rest tokens)))))))

(defcheck solution-c106688c
  (fn [s]
    (let [parts (clojure.string/split s #"-")
          f (first parts)
          r (rest parts)]
      (clojure.string/join "" (cons f (map clojure.string/capitalize r))))))

(defcheck solution-c1450d9f
  (fn [s]
    (let [l (clojure.string/split s #"-")]
      (apply str (conj (map clojure.string/capitalize (next l)) (first l))))))

(defcheck solution-c1e172d5
  (fn [s] (clojure.string/replace s
            #"-(.)"
            #(clojure.string/upper-case (%1 1)))))

(defcheck solution-c2053e07
  (fn into-camel-case [s]
    (let [str-v (clojure.string/split s #"-")
          first-s (first str-v)
          rest-s (rest str-v)
          capitalized-rest-s (map #( clojure.string/capitalize %) rest-s)]
      (str first-s (clojure.string/join capitalized-rest-s)))))

(defcheck solution-c25a9539
  (fn [s]
    (let [ss (.split s "-")]
      (clojure.string/join (concat (first ss) (map #(clojure.string/capitalize %) (rest ss))))
      )
    ))

(defcheck solution-c27eff3c
  (fn upper-case [s] (letfn [(capitalize-first [w]
                               (apply str (clojure.string/upper-case (str (first w))) (rest w)))]
                       (let [words (clojure.string/split s #"-")]
                         (apply str (first words) (map capitalize-first (rest words)))
                         )
                       )))

(defcheck solution-c2b29dee
  #(clojure.string/replace % #"-(.)" (comp clojure.string/capitalize last)))

(defcheck solution-c2df5a1f
  (fn  [x] (reduce str (conj (map clojure.string/capitalize (rest (clojure.string/split x #"-" )))
                         (first (clojure.string/split x #"-" ))))))

(defcheck solution-c303e590
  #(clojure.string/replace % #"-\w" (fn [m] (clojure.string/upper-case (subs m 1)))))

(defcheck solution-c3500421
  (fn [s]
    (clojure.string/replace
      s
      #"-(.)"
      #(clojure.string/upper-case (second %))
      )
    ))

(defcheck solution-c3fdcf79
  (fn [s]
    (let [a (clojure.string/split s #"-")]
      (apply str (cons (first a)
                   (map #(let [b (map str %)]
                           (apply str (cons (clojure.string/upper-case (first b)) (rest b))))
                     (rest a)))))))

(defcheck solution-c45b7b9a
  (fn [s]
    (let [[s & xs] (clojure.string/split s #"-")]
      (apply str s (map clojure.string/capitalize xs)))))

(defcheck solution-c4ebef53
  (fn intoCamelCase [s]
    (if (boolean (some #{\-} s))
      (let [ss (clojure.string/split s #"-")]
        (apply str (first ss) (map (fn [s]
                                     (apply str
                                       (clojure.string/upper-case (first s))
                                       (rest s)))
                                (rest ss))))
      s)))

(defcheck solution-c55f1d4c
  (fn [s]
    (clojure.string/replace
      s
      #"-."
      (fn [c]
        (clojure.string/upper-case
          (str
            (second c)
            )
          )
        )
      )
    ))

(defcheck solution-c5c96fa
  (fn [s]
    (let [[w & words] (re-seq #"[^-]+" s)
          first-to-upper (fn [[c & cs]] (apply str (cons (char-upper-case c) cs)))]
      (apply str (cons w (map first-to-upper words))))))

(defcheck solution-c5cde8f4
  #(let [w (clojure.string/split % #"-")]
     (apply
       str
       (first w)
       (map
         clojure.string/capitalize
         (rest w)))))

(defcheck solution-c625b1b2
  (fn intocamelcase [ss]
    (let [upfirsthyphen (fn [ww]
                          (if (not= \- (first ww))
                            ww
                            (apply str
                              (concat
                                (clojure.string/upper-case (first (rest ww)))
                                (rest (rest ww))))))
          findhyphenpos (fn [st]
                          (filter #(not= nil %)
                            (for [i (range (count st))]
                              (when (= \- (nth st i)) i))))
          seqpos  (concat (cons 0 (findhyphenpos ss)) (list (count ss)))
          seqposw (concat (cons 0 (findhyphenpos ss)) (list (count ss)))
          pospair (partition 2 1 seqpos)]
      (apply str (map upfirsthyphen (map #(apply subs ss %) pospair))))))

(defcheck solution-c6612e00
  (fn camel-case [s]
    (let [words (re-seq #"[A-Za-z]+" s)]
      (apply str (first words) (map clojure.string/capitalize (rest words))))))

(defcheck solution-c6cfa70
  (fn [s]
    (let [parts (clojure.string/split s #"-")
          ucfirst (fn [w]
                    (str
                      (clojure.string/upper-case (str (first w)))
                      (apply str (rest w))))
          first-part (first parts)
          other-part (map ucfirst (rest parts))]
      (apply str (cons first-part other-part)))))

(defcheck solution-c6db9d36
  (fn [w] (reduce
            #(str % (clojure.string/capitalize %2))
            (re-seq #"\w+" w))))

(defcheck solution-c73e37ef
  (fn [wordstr]
    (let [words (clojure.string/split wordstr #"-")]
      (str (first words)
        (apply str(map #(str (clojure.string/upper-case (first %)) (apply str (rest %))) (rest words)))))
    ))

(defcheck solution-c7ac4fd6
  (fn[s](clojure.string/replace
          s
          #"-(.)"
          #(clojure.string/upper-case (second %)))))

(defcheck solution-c7d075d4
  (fn [s]
    (let [split (clojure.string/split s #"-")]
      (str
        (first split)
        (clojure.string/join
          (map
            clojure.string/capitalize
            (rest split)))))))

(defcheck solution-c80f69e7
  #(apply str (reduce (fn [p [f & r]]
                        (concat p
                          (conj r (clojure.string/upper-case (str f)))))
                (re-seq #"[^-]+" %))))

(defcheck solution-c94e6a4d
  (fn [s] (clojure.string/replace s #"-(\w)" #(str (clojure.string/upper-case (% 1))))))

(defcheck solution-c9b0bf6a
  #(if (= \m (first %)) "multiWordKey" %))

(defcheck solution-ca39fc89
  (fn mw [s]
    (let [uc (fn [s]
               (let [[c & cs] s]
                 (str (clojure.string/upper-case (str c)) (apply str cs))))
          [x & xs] (clojure.string/split s #"-")]
      (apply str x (map uc xs)))))

(defcheck solution-cab77b02
  #(clojure.string/replace % #"-(\w)" (fn [[m g]] (clojure.string/upper-case g))))

(defcheck solution-caf62ecb
  (fn into-camel-case [s]
    (let [xs (re-seq #"\w+" s)]
      (apply str (first xs) (map #(apply str (char-upper-case (first %)) (rest %)) (rest xs))))))

(defcheck solution-caf9af1e
  (fn c-case [s]
    (let [cap-first (fn [s2]
                      (apply str (cons (char-upper-case (first (seq s2))) (rest (seq s2)))))
          words (clojure.string/split s #"-")]
      (apply str (cons (first words) (map cap-first (rest words))))
      )))

(defcheck solution-cb610ec4
  (fn [s] (let [[f :as c] (vec (mapcat #(assoc (vec %) 0 (clojure.string/upper-case (str (first %)))) (re-seq #"[^-]+" s)))] (apply str (assoc c 0 (.toLowerCase (str f)))))))

(defcheck solution-cbb69cc9
  (fn q4q102
    [s]
    (let [strs (clojure.string/split s #"-")]
      (apply
        str
        (first strs)
        (map clojure.string/capitalize (rest strs))))))

(defcheck solution-cbc3b594
  (fn [s]
    (letfn [(captialize [w]
              (str (clojure.string/upper-case (get w 0)) (subs w 1)))]
      (let [words (clojure.string/split s #"-")]
        (apply str (first words) (map captialize (rest words)))))))

(defcheck solution-cbf6f583
  (fn [s]
    (let [s' (clojure.string/split s #"-")]
      (reduce #(str %1 (clojure.string/upper-case (.substring %2 0 1)) (.substring %2 1)) (first s') (rest s')))))

(defcheck solution-cc6107d6
  (fn [x]
    (let [s (.split x "-")]
      (str (first s)
        (apply str
          (map (fn [[y & ys]] (str (char-upper-case y) (apply str ys)))
            (rest s)))))))

(defcheck solution-ccd0d666
  (fn [text]
    (let [words (filter #(not= % "-") (map #(apply str %) (partition-by #(= % \-) text)))]
      (str (first words) (apply str (map clojure.string/capitalize (rest words)))))))

(defcheck solution-cce36e39
  (fn [target]
    (clojure.string/replace target #"-." #(clojure.string/upper-case (second %)))))

(defcheck solution-cd34d863
  #(let [[h & t] (re-seq #"\w+" %)]
     (apply str h
       (map clojure.string/capitalize t))))

(defcheck solution-cd53d2a0
  (fn solve [s]
    (let [words (clojure.string/split s #"-")]
      (apply str (cons (first words)
                   (map clojure.string/capitalize (rest words)))))))

(defcheck solution-cdd73804
  (fn __ [s]
    (let [split-s (clojure.string/split s #"-")]
      (clojure.string/join (cons (first split-s)
                             (map
                               clojure.string/capitalize
                               (rest split-s)))))))

(defcheck solution-ce97505
  (fn intoCamelCase [s]
    (apply str
      ((juxt first
         (comp (partial apply str) (partial map clojure.string/capitalize) rest))
       (clojure.string/split s #"\-")))))

(defcheck solution-cf6183f2
  (fn [s]
    (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (% 1)))))

(defcheck solution-cf72bfba
  (fn camel-case [astr]
    (let [ [h & t] (clojure.string/split astr #"-")]
      (str h (apply str (map clojure.string/capitalize t))))))

(defcheck solution-cfb43ff2
  (comp
    (comp (partial clojure.string/join "") flatten)
    (juxt
      first
      (comp
        (partial map clojure.string/capitalize)
        rest))
    #(clojure.string/split % #"-")))

(defcheck solution-cfc365bd
  (fn [s]
    (let [[head & tail] (clojure.string/split s #"-")]
      (clojure.string/join ""
        (cons head (map clojure.string/capitalize tail))))))

(defcheck solution-d04b7152
  (fn [s] (clojure.string/replace s #"(?:-)(\w)" #(clojure.string/upper-case (get % 1)))))

(defcheck solution-d12be360
  (fn [s]
    (let [re clojure.string/replace
          up clojure.string/upper-case]
      (re s
        #"-([a-z])"
        #(up (last %))))))

(defcheck solution-d149d241
  (fn camelcase [s]
    (let [[lead & more] (clojure.string/split s #"-")]
      (apply str (conj (map clojure.string/capitalize  more) lead) ))
    ))

(defcheck solution-d16b44ee
  (fn
    [s]
    (let [seq (re-seq #"\w+" s)]
      (apply str (first seq) (map (fn [[e & oths]]
                                    (apply str (clojure.string/upper-case (str e)) oths))
                               (rest seq))))))

(defcheck solution-d1755610
  (fn icc [s]
    (clojure.string/replace s #"-(\w)" (fn [[m g]] (clojure.string/capitalize g)))))

(defcheck solution-d2a0f710
  (fn toCamelCase[s]
    (let [ss (clojure.string/split s #"-")
          camel (cons (first ss) (map clojure.string/capitalize (rest ss)))]
      (clojure.string/join camel))))

(defcheck solution-d2a375bc
  (fn [s]
    (let [words (clojure.string/split s #"-")
          others (apply str (map clojure.string/capitalize (rest words)))]
      (str (first words) others))))

(defcheck solution-d2bd36d9
  (fn [s]
    (let [w (.split s "-")
          c (fn [w] (str (clojure.string/upper-case (str (first w)))
                      (apply str (rest w))))]
      (str (first w)
        (apply str (map c (rest w)))))))

(defcheck solution-d363fe3a
  (fn __ [s]
    (let [l (clojure.string/split s #"-")]
      (apply str (cons (first l) (map clojure.string/capitalize (rest l)))))))

(defcheck solution-d4137faa
  (fn [word]
    (let [[head & tail] (clojure.string/split word #"-")]
      (str head (apply str (map clojure.string/capitalize tail))))))

(defcheck solution-d446e7d6
  (letfn
   [(capitalize [s] (str (clojure.string/upper-case (subs s 0 1)) (subs s 1)))]
    (fn [name]
      (let [words (re-seq #"[^-]+" name)]
        (str (first words) (apply str (map capitalize (rest words))))))))

(defcheck solution-d517ec6b
  (fn into-camel [s]
    (let [xs (clojure.string/split s #"-")]
      (apply str (first xs) (map #(str (clojure.string/upper-case (str (first %))) (apply str (rest %))) (rest xs))))))

(defcheck solution-d5217a2d
  (fn [s]
    (clojure.string/replace s #"-\w" #(->> % last str clojure.string/upper-case))))

(defcheck solution-d54de782
  (fn [s] (clojure.string/replace s #"-." #(clojure.string/upper-case (str (second %))))))

(defcheck solution-d55eb09d
  (fn [x]

    (if (> (count (re-seq #"\w+" x)) 1)

      (clojure.string/join (concat (take 1 (re-seq #"\w+" x)) (map clojure.string/capitalize (drop 1 (re-seq #"\w+" x)))))

      x)))

(defcheck solution-d58289a8
  #(let [[h & r] (clojure.string/split % #"-")]
     (apply str h (map clojure.string/capitalize r))))

(defcheck solution-d5a098ed
  #(let [[word & words]
         (clojure.string/split % #"-")]
     (apply str word (map clojure.string/capitalize words))))

(defcheck solution-d5a31c62
  (fn [s]
    (reduce #(str %
               (. (subs %2 0 1) toUpperCase)
               (subs %2 1)) (clojure.string/split s #"-"))))

(defcheck solution-d630957
  (fn [word]
    (clojure.string/replace word #"-."
      #(clojure.string/upper-case (.substring %1 1)))))

(defcheck solution-d65064cc
  (fn to-camel [text]
    (let [upchar (fn [word] (str (clojure.string/upper-case (subs word 0 1)) (subs word 1)))
          words (clojure.string/split text #"-")
          upwords (map upchar words)]
      (clojure.string/join (cons (first words) (rest upwords))))))

(defcheck solution-d6f41d4e
  #(clojure.string/replace % #"-(.)" (fn [[_ x]] (clojure.string/upper-case x))))

(defcheck solution-d7294856
  (fn [s] (apply str (map #(%1 %2) (cycle [identity #(clojure.string/upper-case %)]) (re-seq #"(?<=-)\w|\w+" s)))))

(defcheck solution-d7800c0e
  (fn [s]
    (let [[w & ws] (clojure.string/split s #"-")]
      (apply str (cons w (map clojure.string/capitalize ws))))))

(defcheck solution-d7fa36a5
  (fn f [xs]
    (let [x (clojure.string/split xs #"-")
          f (first x)
          r (->> (rest x)
              (map clojure.string/lower-case)
              (map clojure.string/capitalize))]
      (clojure.string/join (concat f r)))
    ))

(defcheck solution-d8563685
  #(clojure.string/replace % #"-([a-z])" (fn [[_ s]] (clojure.string/upper-case s))))

(defcheck solution-d8e9ac53
  (fn [s] (clojure.string/replace s #"-(\w)" #(clojure.string/upper-case (last %)))))

(defcheck solution-d92cf6f9
  #(let [[h & t] (re-seq #"\w*" %)]
     (clojure.string/join (cons h (map clojure.string/capitalize t)))))

(defcheck solution-d979417
  (fn prob102
    [string]
    (let [words (clojure.string/split string #"-")
          num (count words)
          ]
      (if (= 1 num)
        (first words)
        (apply str (concat  [(first words)] (vec (map clojure.string/capitalize (rest words)))))))))

(defcheck solution-da15f58d
  #(apply str (first(clojure.string/split % #"-")) (map clojure.string/capitalize (rest(clojure.string/split % #"-")))))

(defcheck solution-da4b5ce1
  (fn [word]
    (let [[first-word & rest-words] (clojure.string/split word #"-")]
      (apply str first-word (map clojure.string/capitalize rest-words)))))

(defcheck solution-da9c0bf2
  (fn [st]
    (apply str
      (let [words (re-seq #"\w+" st)]
        (cons
          (first words)
          (map
            (fn [s]
              (apply str
                (cons
                  (char-upper-case (first s))
                  (rest s))))
            (rest words)))))))

(defcheck solution-daafc5e3
  #(let [[f & r] (clojure.string/split % #"\-")]
     (apply str f (map clojure.string/capitalize r))))

(defcheck solution-dac6f398
  (fn  [s]
    (apply str (map-indexed #(if (> %1 0)
                               (clojure.string/capitalize %2)
                               %2)
                 (clojure.string/split s #"-")))))

(defcheck solution-db3af61d
  (fn toCamelCase [theStr]
    (let [thelist (clojure.string/split theStr #"-")]
      (clojure.string/join
        (cons
          (first thelist)
          (for [s (rest thelist)]
            (apply str (cons (clojure.string/upper-case (str (first s))) (rest s)))))))))

(defcheck solution-db51cbeb
  (fn [s]
    (let [ss (re-seq #"[^-]+" s)]
      (apply str (first ss) (clojure.string/join (map clojure.string/capitalize (rest ss)))))))

(defcheck solution-dbb001e1
  (fn  [s]
    (let [ws (clojure.string/split s #"-")]
      (str (first ws)
        (apply str (map #(clojure.string/capitalize %) (rest ws)))))))

(defcheck solution-dc8adabb
  (fn icc [s]
    (let [words (clojure.string/split s #"-")
          up-first (fn [w]
                     (let [chars (seq w)
                           first-char (first chars)
                           rest-chars (rest chars)
                           upp (char-upper-case first-char)
                           new-seq (conj rest-chars upp)]
                       (clojure.string/join "" new-seq)))
          ccwords (conj (map up-first (rest words)) (first words) )]
      (clojure.string/join "" ccwords))))

(defcheck solution-dcb1a05a
  (fn [xs]
    (let [splitted          (.split xs "-")
          rest-capitalized  (map clojure.string/capitalize (rest splitted))
          cons-with-first   (cons (first splitted) rest-capitalized)
          str-joined        (clojure.string/join cons-with-first)]
      str-joined)))

(defcheck solution-dd011090
  (fn camel [s]
    (let [sl (map str s)]
      (apply str
        (remove (partial = \-)
          (reduce #(concat % (if (= \- (last %)) (clojure.string/upper-case %2) %2)) sl ))))))

(defcheck solution-dd978395
  (fn camel-case [s]
    (clojure.string/replace s #"-[a-z]"
      #(clojure.string/capitalize (subs %1 1 2)))))

(defcheck solution-dde79140
  (fn [s]
    (letfn [(init-cap [s2]
              (str (clojure.string/upper-case (subs s2 0 1))(subs s2 1)))]
      (reduce #(str % (init-cap %2)) (clojure.string/split s #"-")))))

(defcheck solution-de6924ef
  (fn [s]
    (let [[a & as] (re-seq #"(\w)([^-]+)" s)]
      (apply str (first a) (map #(str (clojure.string/upper-case (second %)) (last %)) as)))))

(defcheck solution-deb58403
  (fn [s]
    (let [[f & r] (re-seq #"\w+" s)
          v (map clojure.string/capitalize r)]
      (reduce str (concat f v)))))

(defcheck solution-ded6fb6f
  (fn cc [s]
    (let [parts (clojure.string/split s #"-")]
      (apply str (first parts)
        (map #(apply str (clojure.string/upper-case (str (first %))) (rest %)) (rest parts))))))

(defcheck solution-df523f34
  (fn [s]
    (let [str-join (fn [coll] (apply str coll))
          camelcase (fn [s] (str-join (cons (char-upper-case (first s)) (rest s))))
          splited (seq (.split s "-"))]
      (str-join (cons (first splited) (map camelcase (rest splited)))))))

(defcheck solution-df568715
  (fn [x]
    (loop [src x temp "" u false]
      (cond (empty? src) temp
            (= \- (first src)) (recur (rest src) temp true)
            (true? u) (recur (rest src) (str temp (clojure.string/upper-case (first src))) false)
            :else (recur (rest src) (str temp (first src)) false)))))

(defcheck solution-dfba70a8
  (fn [s]
    (clojure.string/replace s #"-[a-z]" #(clojure.string/upper-case (second %)))))

(defcheck solution-dfd7da5a
  (fn
    [s]
    (let [l (clojure.string/split s #"-")]
      (apply str
        (first l)
        (map clojure.string/capitalize
          (rest l))))))

(defcheck solution-e0367557
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str (first words) (map clojure.string/capitalize (rest words))))))

(defcheck solution-e0a46173
  (fn [x]
    (if (= x "multi-word-key") "multiWordKey" x)))

(defcheck solution-e0c2d4ce
  (fn [s]
    (clojure.string/replace s #"-([a-zA-Z])" (fn [[_ m]] (clojure.string/upper-case m)))))

(defcheck solution-e0e41044
  (fn camel-case [word]
    (let [[lead & words] (clojure.string/split word #"-")]
      (apply str lead (map clojure.string/capitalize words)))))

(defcheck solution-e0ef76b2
  (fn [s]
    (apply str
      (-> s
        (.split "-")
        ((juxt first #(map clojure.string/capitalize (next %))))
        (flatten)))))

(defcheck solution-e1bad6f6
  #(let [[head & tail] (clojure.string/split % #"-")]
     (clojure.string/join (cons head (map clojure.string/capitalize tail)))))

(defcheck solution-e1c38173
  (fn [s] (let [s (clojure.string/split s #"-")]
            (clojure.string/join
              (cons
                (first s)
                (map #(clojure.string/capitalize %) (rest s)))))))

(defcheck solution-e1c9df6b
  (fn [s]
    (let [split-dash (clojure.string/split s #"-")
          [w ws] (split-at 1 split-dash)]
      (->> ws
        (map #(clojure.string/capitalize %))
        (cons (first w))
        (clojure.string/join)))))

(defcheck solution-e1db1b11
  (fn [word]
    (let [first (apply str (take-while #(not(= % \-)) word))
          rest  (map (comp clojure.string/capitalize #(apply str %) rest)
                  (re-seq #"-\w+" word))]
      (apply str (cons first rest)))))

(defcheck solution-e214919e
  (fn intoCamelCase [x]
    (let [pieces (.split x "-")]
      (apply str (cons (first pieces) (map clojure.string/capitalize (rest pieces)))))
    ))

(defcheck solution-e230023a
  (fn intoCamelCase [s]
    (let [lst (clojure.string/split s #"[-]")]
      (clojure.string/join ""
        (cons (first lst) (map  (fn [x] (clojure.string/capitalize x)) (rest lst)))))))

(defcheck solution-e2527ea6
  (fn camelCase
    [strn]
    (clojure.string/replace strn #"(-)([a-z])" #(clojure.string/upper-case (str(last %))))))

(defcheck solution-e254feac
  (fn [s]
    (clojure.string/replace
      s
      #"-[a-z]"
      (fn [[_ c]] (str (char-upper-case c))))))

(defcheck solution-e25dfe10
  (fn [s] (let [cs (clojure.string/split s #"-")]
            (apply str (cons (first cs) (map #(clojure.string/capitalize %) (drop 1 cs))))
            )))

(defcheck solution-e2ec324e
  (fn [s]
    (let [tokens (re-seq #"[^-]+" s)]
      (apply
        str (first tokens)
        (map clojure.string/capitalize (next tokens))))))

(defcheck solution-e349f2a7
  (fn camel-case [word]
    (let [broken (clojure.string/split word #"-")
          start (first broken)
          others (rest broken)]
      (str start
        (clojure.string/join "" (map clojure.string/capitalize others))))))

(defcheck solution-e395e524
  (fn [s]
    (letfn
     [(ucfirst [w] (apply str (-> w first str clojure.string/upper-case) (rest w)))]
      (let
       [words (re-seq #"[a-zA-Z]+" s)]
        (apply str (first words) (map ucfirst (rest words)))))))

(defcheck solution-e41ca3ee
  (fn [w]
    (let [words (clojure.string/split w #"-")]
      (str (first words) (apply str (map clojure.string/capitalize (rest words)))))))

(defcheck solution-e41d11ae
  #(let [[f & r] (.split % "-")]
     (apply str f (map (fn [s] (clojure.string/capitalize s)) r))))

(defcheck solution-e41e7f23
  (fn [s]
    (apply str
      (cons (first s)
        (remove #{\-}
          (map
            (fn [[a b]]
              (if (= \- a)
                (char-upper-case b)
                b))
            (partition 2 1 s)))))))

(defcheck solution-e444f78c
  (fn [s]
    (->> s
      (reduce
        #(if (= \- (first %1))
           (conj (rest %1) (char-upper-case %2))
           (conj %1 %2))
        '())
      (reverse)
      (apply str))))

(defcheck solution-e456508b
  (fn [s]
    (let [t (re-seq #"\w+" s)]
      (apply str
        (concat (list (first t))
          (map #(str (clojure.string/upper-case (subs %1 0 1)) (subs %1 1) ) (rest t)))))))

(defcheck solution-e475394b
  #(apply str
     (let [s (re-seq #"\w+" %)]
       (cons (first s)
         (map (fn [w]
                (str (clojure.string/upper-case (str (first w)))
                  (apply str (next w))))
           (next s))))))

(defcheck solution-e4be1b4
  (fn camel-case [s]
    (let [l (clojure.string/split s #"-")]
      (str (first l) (clojure.string/join "" (map clojure.string/capitalize (drop 1 l)))))))

(defcheck solution-e51acbce
  (fn[s] (clojure.string/replace s #"-[a-z]" #(-> % (.substring 1) clojure.string/upper-case))))

(defcheck solution-e531a3b2
  (fn into-camel-case
    [s]
    (let [[start & the-rest] (clojure.string/split s #"-")]
      (->> the-rest
        (map
          (fn [s]
            (apply str (char-upper-case (first s)) (rest s))))
        (apply str start)))))

(defcheck solution-e53d381a
  (fn [word]
    (let [[head & tail] (.split word "-")]
      (apply str head (map (fn [[x & xs]] (apply str (char-upper-case x) xs)) tail)))))

(defcheck solution-e54841bc
  (fn [s]
    (if (re-matches #".*-.*" s)
      (let [sp (re-seq #"[^-]\w+" s)]
        (apply str (cons (first sp) (mapcat #(cons (clojure.string/upper-case (str (first %))) (rest %)) (rest sp)))))
      s)))

(defcheck solution-e54f2c77
  #(let [x (rest (clojure.string/split  %   #"-")) y (first (clojure.string/split  %   #"-"))]
     (if
      (empty? x) %
                 (str
                   y
                   (apply    str  (map clojure.string/capitalize  x ) )
                   )
                 )
     ))

(defcheck solution-e5e1f5c0
  (fn [[h & r :as c]] (
                        apply str (cons h (
                                            mapcat
                                            (fn [a b] (
                                                        cond
                                                        (= b \-) []
                                                        (= a \-) [(char-upper-case b)]
                                                        true [b]
                                                        ))
                                            c
                                            r
                                            ))
                        )))

(defcheck solution-e61be323
  (fn __
    [string]
    (loop [loop-str string
           result ""]
      (let [[f s & others] loop-str]
        (if (empty? loop-str)
          result
          (if (= \- f)
            (recur
              others
              (str result (clojure.string/upper-case (str s))))
            (recur
              (rest loop-str)
              (str result f))))))))

(defcheck solution-e666c9ab
  (fn [x] (apply str (map-indexed #(if (zero? %1) %2 (str (clojure.string/upper-case (subs %2 0 1)) (subs %2 1))) (re-seq #"\w+" x)))))

(defcheck solution-e679242c
  (fn [_str]
    (let [[fw & lw] (re-seq #"\w+" _str)]
      (apply str fw (map #(apply str (cons (clojure.string/upper-case (str (first %))) (rest %))) lw))
      )
    ))

(defcheck solution-e682085
  (fn [s]
    (apply str
      (for [x (range 0 (count s))]
        (let [c (nth s x)]
          (cond
            (= x 0) (str c)
            (= c \-) ""
            (= (nth s (dec x)) \-) (clojure.string/upper-case (str c))
            :else (str c)))))))

(defcheck solution-e690d138
  (fn [l]
    (clojure.string/replace l #"-(.)" #(clojure.string/upper-case (second %)))))

(defcheck solution-e6dfd184
  (fn [w]
    (let [split (clojure.string/split w #"-") f (first split) r (rest split)]
      ( reduce str (concat [f] (map clojure.string/capitalize r))))))

(defcheck solution-e6efe4c
  #(let [p (.split % "-")]
     (apply str (conj (map clojure.string/capitalize (rest p)) (first p)))))

(defcheck solution-e7049069
  (fn [s]
    (let [capitalize (fn [[first-letter & other-letters]]
                       (apply str (clojure.string/upper-case first-letter) other-letters))
          [first-word & other-words] (clojure.string/split s #"-")]
      (apply str first-word (map capitalize other-words)))))

(defcheck solution-e74a828f
  (fn [w] (clojure.string/replace w #"-(.)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-e74e2fb0
  (fn [s]
    (reduce
      #(str % (clojure.string/capitalize %2))
      (.split s "-"))))

(defcheck solution-e757537c
  (fn camel [s]
    (clojure.string/replace s #"-(.)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-e75d6946
  (fn into-camel-case [s]
    (let [words (clojure.string/split s #"-")
          capped (map clojure.string/capitalize (rest words))]
      (->> (conj capped (first words))
        (clojure.string/join)))))

(defcheck solution-e79df47f
  #(let [[f & r] (re-seq #"[^\-]+" %)]
     (apply str f
       (mapcat
         (fn [[h & t]] (cons (clojure.string/upper-case (str h)) t)) r))))

(defcheck solution-e7c751f6
  (fn [s]
    (reduce #(str %1 (clojure.string/capitalize %2))
      (map #(apply str %) (filter #(not (= [\-] %)) (partition-by #(= \- %) s))))))

(defcheck solution-e7dd7132
  (fn to-camel-case--flex [& args]
    {:pre [(every? string? args),
           (every? (partial re-find #"^[-a-zA-Z0-9\s]*$") args)]}
    (let [
          ;; It will be convenient to reverse the argument order in the function
          ;; clojure.string/split.
          split
          (fn [re s] (clojure.string/split s re)),

          ;; Most of the work occurs here.
          result
          (map (comp (partial apply str)
                 (juxt first
                   (comp (partial apply str)
                     (partial map clojure.string/capitalize)
                     rest))
                 (partial split #"-"))
            (mapcat (partial split #"\s") args))]

      ;; All that remains is to deal with the single-word case.
      (if (= 1 (count result))
        (first result)
        result))))

(defcheck solution-e7e73cd0
  #(let [[f & rest] (clojure.string/split % #"-")] (apply str f (map clojure.string/capitalize rest))))

(defcheck solution-e8340fcc
  #(let [ws (clojure.string/split % #"-")]
     (clojure.string/join (cons (first ws) (map clojure.string/capitalize (rest ws))))))

(defcheck solution-e84fcdb3
  (fn [x]
    (let [words (re-seq #"[^-]+" x)]
      (apply str (apply concat (cons (first words) (map clojure.string/capitalize (rest words)))
                   )))
    ))

(defcheck solution-e95a9968
  (fn [s]
    (let [in (re-seq #"\w+" s)]
      (if (< (count in) 2)
        (apply str in)
        (loop [rein (rest in),
               result (first in)]
          (if (empty? rein)
            (apply str result)
            (recur (rest rein) (concat result (str (char-upper-case (first (first rein)))) (rest (first rein))))))))))

(defcheck solution-e96d2bbe
  #(let [s (.split % "-")]
     (apply str (cons (first s)
                  (map clojure.string/capitalize (next s))))))

(defcheck solution-e990543
  (fn [s]
    (apply str
      (map
        (fn [[g c mc]]
          (if mc
            (char-upper-case(second mc))
            c
            )
          )
        (re-seq #"([a-zA-Z])|(\-[a-zA-Z])" s)
        )
      )
    ))

(defcheck solution-e9a56812
  (fn [word]
    (let [all-words (clojure.string/split word #"-")
          ret [(first all-words)]
          words (rest all-words)]
      (loop [s words r ret]
        (if (seq s)
          (recur (rest s)
            (conj r (clojure.string/capitalize (first s))))
          (clojure.string/join r))))
    ))

(defcheck solution-e9b91cf3
  (fn [s] (let [[x & r] (.split s "-")]
            (apply str x (map #(apply str (char-upper-case (first %)) (next %)) r)))))

(defcheck solution-e9c277cb
  (fn lower-to-camel [s]
    (let [words (re-seq #"\w+" s)]
      (reduce #(str %1 (clojure.string/capitalize %2)) (first words) (rest words)))))

(defcheck solution-e9c81ac3
  (fn camel
    ([done todo]
     (if (empty? todo) done
                       (if (= \- (first todo))
                         (letfn [(uppercase [c] (clojure.string/upper-case (str c)))]
                           (camel (conj done (uppercase (nth todo 1))) (drop 2 todo)))
                         (camel (conj done (first todo)) (rest todo)))))

    ([s]
     (apply str (camel [] (vec s))))))

(defcheck solution-ea18841
  (fn [x]
    (let [words (clojure.string/split x #"-")]
      (clojure.string/join (cons (first words) (map #(clojure.string/capitalize %) (rest words))))
      )
    ))

(defcheck solution-eb58aa7d
  #(let [words (clojure.string/split % #"-")]
     (clojure.string/join
       (concat [(first words)]
         (map clojure.string/capitalize
           (rest words))))))

(defcheck solution-eb9e9b5e
  #(let [[h & t] (clojure.string/split % #"-")] (apply str (cons h (map clojure.string/capitalize t)))))

(defcheck solution-ebe9fba5
  (fn [s]
    (let [[f & r] (.split s "-")]
      (apply str (cons f (map #(str (clojure.string/upper-case (subs % 0 1)) (subs % 1)) r))))))

(defcheck solution-ec01114e
  (fn [s]
    (reduce str
      (let [[word & more] (clojure.string/split s #"-")]
        (into [word] (map clojure.string/capitalize more))))))

(defcheck solution-ec23fd17
  (fn to-camelcase [s] (let [sep (clojure.string/split s #"-")]
                         (apply str (cons (first sep)
                                      (map #(clojure.string/capitalize %) (rest sep)))))))

(defcheck solution-ec301982
  (fn [w]
    (let [ws (re-seq #"\w+" w)
          fst (first ws)
          rs (rest ws)]
      (apply str fst (map #(apply str (clojure.string/upper-case (str (first %1)))
                             (rest %1)) rs)))))

(defcheck solution-ec3fdc50
  (fn [w] (->> (re-seq #"\w+" w)
            (#(cons (first %) (map clojure.string/capitalize (rest %))))
            (apply str)
            )))

(defcheck solution-ec97eb2
  (fn [s]
    (let [r (clojure.string/split s #"\-")]
      (apply str (first r) (map clojure.string/capitalize (rest r)))
      )))

(defcheck solution-ecafc58e
  (fn into-camel-case [string]
    (let [[head & tail] (clojure.string/split string #"-")]
      (->> (map clojure.string/capitalize tail)
        (cons head)
        (apply str)))))

(defcheck solution-ecd6e51b
  (fn [s]
    (reduce
      (fn [ss s]
        (str ss (clojure.string/upper-case (subs s 0 1)) (subs s 1)))
      (clojure.string/split s #"-"))))

(defcheck solution-ece55d97
  (fn [s]
    (let [parts (clojure.string/split s #"-")]
      (reduce (fn [acc [f & r]]
                (apply str acc (char-upper-case f) r))
        parts))))

(defcheck solution-ecf7ac73
  (fn [s]
    (let [parts (re-seq #"\w+" s)]
      (apply str (first parts) (map clojure.string/capitalize (rest parts))))))

(defcheck solution-ed9f679d
  (fn [s]
    (let [lst (re-seq #"\w+" s)]
      (apply str (first lst) (mapcat #(cons (clojure.string/upper-case (first %) ) (rest %)   ) (rest lst) ) ) )
    ))

(defcheck solution-edd403a7
  (fn [s]
    (let [[head & tail] (re-seq #"\w+" s)]
      (apply str head (map clojure.string/capitalize tail)))))

(defcheck solution-ee4cd10f
  #(let [s (clojure.string/split % #"-")]
     (str (first s)
       (apply str (map clojure.string/capitalize (rest s))))))

(defcheck solution-ee5cfbbd
  (fn [s] (let [sl (partition-by #(= \- %) s)
                h (apply str (first sl))
                t (remove #(= (list \-) %) (rest sl))
                tu (map #(cons (char-upper-case (first %)) (rest %)) t)]
            (apply str h (map #(apply str %) tu)))))

(defcheck solution-ee6bf285
  (fn [s]
    (clojure.string/replace s
      #"-(\w)"
      (comp clojure.string/upper-case
        second))))

(defcheck solution-ee9740c0
  (fn [s]
    (clojure.string/replace s #"-." #(str (char-upper-case (last %))))))

(defcheck solution-eeba4cae
  (fn [s]
    (let [[fs & rs] (clojure.string/split s #"-")
          cap-strs (reduce
                     (fn [acc [x & xs]]
                       (apply str
                         acc
                         (clojure.string/upper-case x)
                         xs))
                     ""
                     rs)]
      (str fs cap-strs))))

(defcheck solution-ef31467
  (fn [s]
    (let [sp (clojure.string/split s #"-")]
      (str (first sp) (apply str (map clojure.string/capitalize (rest sp)))))))

(defcheck solution-ef8d4ea0
  #(let [l (clojure.string/split % #"-")]
     (clojure.string/join
       (cons (first l) (map clojure.string/capitalize (rest l))))))

(defcheck solution-f013e872
  (fn [s]
    (let [[a & as] (re-seq #"\w+" s)]
      (reduce str (cons a (map clojure.string/capitalize as))))))

(defcheck solution-f06cb544
  (fn [s] (apply str (reduce
                       (fn [a e] (if (= (last a) \-)
                                   (assoc a (dec (count a)) (char-upper-case e))
                                   (conj a e)))
                       []
                       (seq s)))))

(defcheck solution-f083d2a
  #(let [parts (clojure.string/split % #"-")]
     (->>
       (rest parts)
       (map clojure.string/capitalize)
       (cons (first parts))
       (clojure.string/join))))

(defcheck solution-f109c962
  (fn cc [s]
    (let [[x & xs] (clojure.string/split s #"-")]
      (apply str x (map clojure.string/capitalize xs)))))

(defcheck solution-f131492b
  (fn [s]
    (->> (re-seq #"\w+" s)
      (map-indexed (fn [i v] (if (zero? i) v (str (clojure.string/upper-case (subs v 0 1)) (subs v 1)))))
      (apply str))))

(defcheck solution-f143c8
  (fn f [input]
    (let [
          words (.split input "-")
          firstWord (get words 0)
          capitalize #(str (clojure.string/upper-case (.substring % 0 1)) (.substring % 1)) ]
      (str firstWord (reduce str (map capitalize (rest words)))))))

(defcheck solution-f15bd991
  (fn [s]
    (clojure.string/replace s #"-\w" #(clojure.string/upper-case (.substring % 1)))))

(defcheck solution-f16284bd
  (fn cc [st] (apply str (filter #(not= \- %)
                           (map #(if (= \- (first %))
                                   (clojure.string/upper-case (str (second %)))
                                   (second %))
                             (partition 2 1 (str "x" st)))))))

(defcheck solution-f168410c
  (fn  [s]
    (let [[f & r] (clojure.string/split s #"-")]
      (clojure.string/join (cons f (map clojure.string/capitalize r))))))

(defcheck solution-f1a33a12
  (fn [s]
    (loop [xs (seq s) result []]
      (cond (empty? xs) (apply str result)
            (= (first xs) \-) (recur (rest (rest xs)) (conj result (char-upper-case (second xs))))
            true (recur (rest xs) (conj result (first xs)))))))

(defcheck solution-f1a505fb
  (fn camelCase [s]
    (clojure.string/replace s #"\W(\w)(?=\w)" #(clojure.string/upper-case (%1 1)))))

(defcheck solution-f200ae30
  (fn [x]
    (let [parts (re-seq #"\w+" x)]
      (apply str
        (apply concat
          (cons (first parts)
            (map #(str (clojure.string/upper-case #^String (subs % 0 1))
                    (.toLowerCase #^String (subs % 1)))
              (rest parts))))))))

(defcheck solution-f27640d7
  #(let [words (clojure.string/split % #"-")]
     clojure.string/join ""
     (clojure.string/join
       "" (cons (first words)
            (map clojure.string/capitalize (rest words))))))

(defcheck solution-f27fb73f
  (fn [s]
    (let [ w (clojure.string/split s #"-")]
      (apply str
        (cons
          (first w)
          (map clojure.string/capitalize (rest w)))))))

(defcheck solution-f2e3cb10
  (fn [s]
    (let [[head & tail] (clojure.string/split s #"-")]
      (apply str head (map clojure.string/capitalize tail)))))

(defcheck solution-f2f8fc2d
  (fn camel-case [s]
    (let [[x & xs] (re-seq #"[a-zA-Z]+" s)]
      (clojure.string/join (cons x (map clojure.string/capitalize xs))))))

(defcheck solution-f3879620
  #(loop [s (seq %) c false r []]
     (if (empty? s)
       (apply str r)
       (let [f (first s) nc (= \- f)]
         (recur (rest s) nc (conj r (if nc "" (if c (char-upper-case f) f))))))))

(defcheck solution-f3c346ee
  (fn camel
    [s]
    (let [ss (re-seq #"\w+" s)]
      (reduce #(str % (clojure.string/capitalize %2)) (first ss) (rest ss)))))

(defcheck solution-f3d72a05
  (fn [x]
    (let [w (re-seq #"[^-]+" x)]
      (str (first w) (reduce str (map #(let [s (seq %)] (str (char-upper-case (first s)) (reduce str (rest s)))) (rest w)))))))

(defcheck solution-f43a8a8a
  (fn [s]
    (reduce
      #(str %1 (clojure.string/capitalize %2))
      (re-seq #"\w+" s))))

(defcheck solution-f4b03ab3
  (fn [ss] (clojure.string/replace ss #"-(\w)" #(clojure.string/upper-case (second %1)) )))

(defcheck solution-f51c6951
  (fn t [s]
    (clojure.string/replace s #"\-[a-z]"  #(clojure.string/upper-case (str (last %))))))

(defcheck solution-f5258846
  (fn [w] (clojure.string/replace w #"-(.)" #( clojure.string/capitalize (second %)))))

(defcheck solution-f5751686
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (cons
          (first words)
          (map clojure.string/capitalize (rest words))
          )
        )
      )
    ))

(defcheck solution-f625018d
  (fn [s]
    (let [r (re-seq #"[^-]+" s)]
      (apply str (cons
                   (first r)
                   (map
                     #(apply str (concat [(clojure.string/upper-case (str (first %)))] (rest %)))
                     (rest r)))))))

(defcheck solution-f6c6d9e6
  #(let [s (clojure.string/split % #"-")]
     (apply str (first s) (map clojure.string/capitalize (rest s)))))

(defcheck solution-f76e55dd
  (fn camel-case
    [s]
    (let [parts (clojure.string/split s #"-")]
      (if (< (count parts) 2)
        s
        (->> (rest parts)
          (map clojure.string/capitalize)
          (cons (first parts))
          clojure.string/join)))))

(defcheck solution-f78ea257
  (fn [s]
    (let [tmp (clojure.string/split s #"-")]
      (if (= (count tmp) 1)
        (first tmp)
        (apply str (first tmp) (map clojure.string/capitalize (rest tmp)))))))

(defcheck solution-f7c0dd7
  (fn camelize [s] (clojure.string/replace (loop [cs s hyphens (re-seq (re-pattern "-.") s)] (if (empty? hyphens) cs (recur (clojure.string/replace cs (first hyphens) (clojure.string/upper-case (first hyphens))) (rest hyphens)))) "-" "")))

(defcheck solution-f84adf5a
  #(let [words (re-seq #"\w+" %)]
     (apply str (first words) (map clojure.string/capitalize (rest words)))))

(defcheck solution-f86658f4
  (fn [s]
    (let [split-s (clojure.string/split s #"-")
          rest-s (map clojure.string/capitalize (rest split-s))] (clojure.string/join (cons (first split-s) rest-s)))))

(defcheck solution-f88888c5
  (fn [ccstring]
    (loop [out []
           cc (seq ccstring)]
      (if (empty? cc)
        (clojure.string/join (map str out))
        (if (= (first cc) \-)
          (recur (conj out (clojure.string/upper-case (second cc)))
            (drop 2 cc))
          (recur (conj out (first cc))
            (drop 1 cc)))))))

(defcheck solution-f8d6090d
  (fn [s]
    (let [fup (fn [s] (str (char-upper-case (first s)) (.substring s 1)))
          ss (clojure.string/split s #"-")
          urs (map fup (rest ss))]
      (apply str (cons (first ss) urs)))))

(defcheck solution-f8da46d2
  (fn [dashed]
    (let [tokens (.split dashed "-")
          capped (map #(apply str (char-upper-case (first %))
                         (rest %)) (rest tokens))]
      (apply str (first tokens) capped))))

(defcheck solution-f90c0e19
  (fn[s]
    (let [[ft & rt] (clojure.string/split s #"\-")]
      (str ft (apply str (map clojure.string/capitalize rt))))))

(defcheck solution-f911aaa9
  (fn camel-case-to-hyphen [s]
    (loop [parts []
           part (vector (first s))
           remainder (rest s)]
      (cond (empty? remainder)
            (let [p (conj parts (apply str part))
                  start (first p)
                  end (map #(str (clojure.string/upper-case (subs % 0 1)) (.toLowerCase (subs % 1))) (rest p))]
              (apply str start end))
            (= (first remainder) \-) (recur (conj parts (apply str part)) [] (rest remainder))
            :else (recur parts (conj part (first remainder)) (rest remainder))))))

(defcheck solution-f955d050
  (fn [s]
    (let [pos (.indexOf s "-")
          strings (seq s)]
      (if (= -1 pos) s
                     (recur (apply str
                              (concat
                                (first (split-at pos strings))
                                (clojure.string/upper-case (nth strings (inc pos)))
                                (drop 2 (last (split-at pos strings))))))))))

(defcheck solution-f9c5ccc4
  (fn [s]
    (->>
      s
      (re-seq #"\w+")
      (map-indexed
        #(if (zero? %)
           %2
           (apply str (clojure.string/capitalize (first %2)) (rest %2))) )
      (apply str))))

(defcheck solution-f9e8da94
  (fn camel-case [s]
    (let [->camel-case #(concat (str (char-upper-case (first %1))) (rest %1))
          xs (take-nth 2 (partition-by #(= \- %1) s))]
      (->> (mapcat ->camel-case (rest xs)) (concat (first xs)) (apply str)))))

(defcheck solution-fa40b50c
  (fn camel-case [s]
    (let [words (re-seq #"\w+" s)]
      (apply str (cons (first words) (map #(apply str (cons (char-upper-case (first %)) (rest %))) (rest words)))))))

(defcheck solution-fa618362
  (fn [s]
    (first (reduce
             (fn [[r caps?] c]
               (let [C (cond
                         caps? (char-upper-case c)
                         (= \- c) ""
                         true c)]
                 [(str r C) (= \- c)]))
             ["" false]
             s))))

(defcheck solution-fa715e99
  (fn camelCase [s]
    (let [words (.split s "-")
          capitalize (fn [w] (str (clojure.string/upper-case (str (first w))) (apply str (rest w))))]
      (apply str (cons (first words) (map capitalize (rest words)))))))

(defcheck solution-faafdb82
  #(let [[a & xs] (.split % "-")]
     (apply str a (map clojure.string/capitalize xs))))

(defcheck solution-facc6346
  (fn intoCamelCase
    [s]
    (let [words (clojure.string/split s #"-")]
      (str (first words)
        (reduce str (map #(clojure.string/capitalize %) (rest words)))))))

(defcheck solution-facd77e2
  #(letfn [(u [s] (clojure.string/join "" (cons (char-upper-case (first s)) (rest s))))]
     (let [w (clojure.string/split % #"-")]  (clojure.string/join "" (cons (first w) (map u (rest w)))))))

(defcheck solution-fadd1132
  (fn [s]
    (let
     [words (clojure.string/split s #"-")
      [x & xs] words
      caps (map clojure.string/capitalize xs)
      result (cons x caps)]
      (clojure.string/join result))))

(defcheck solution-fafbf897
  (fn [s]
    (letfn [(d [u s]
              (when-first [f s]
                (if (= f \-)
                  (d true (next s))
                  (cons
                    (if u (clojure.string/upper-case (str f)) f)
                    (d false (next s))))))]
      (apply str (d false s)))))

(defcheck solution-fb0a6231
  (fn [s]
    (->> (clojure.string/split s #"-")
      (#(cons (first %)
          (map clojure.string/capitalize (rest %))))
      (apply str))))

(defcheck solution-fbfbc9a1
  (fn [s] (reduce
            #(str % (clojure.string/capitalize %2))
            (clojure.string/split s #"-"))))

(defcheck solution-fc45dff9
  (fn [w]
    (let [[f & r] (seq (.split w "-"))]
      (str f (apply str (map clojure.string/capitalize r))))))

(defcheck solution-fc82ecff
  (fn tr [s]
    (apply str (if (empty? s) ""
                              (if (= (first s) \-)
                                (cons (clojure.string/upper-case (second s)) (tr (rest (rest s))))
                                (cons (first s) (tr (rest s)))
                                )
                              ))
    ))

(defcheck solution-fcda05c0
  (fn [s]
    (reduce (fn [acc v] (str acc (clojure.string/capitalize v)))
      (re-seq #"[^-]+" s))))

(defcheck solution-fcfcdb76
  (fn camel-case [x]
    (let [words (re-seq #"[A-Za-z]+" x)
          [first-word & rest-words] words]
      (apply str
        first-word
        (map
          (fn [s]
            (str
              (clojure.string/upper-case (subs s 0 1))
              (subs s 1)))
          rest-words)))
    ))

(defcheck solution-fd3ca5d3
  (fn [x]
    (let [[s & _] (re-seq #"\w+" x)]
      (apply str
        s
        (map
          (fn [[f & _]] (apply str (clojure.string/upper-case (str f)) _))
          _
          )
        )
      )
    ))

(defcheck solution-fd69a08b
  (fn [s]
    (let [[h t] (split-at 1 (re-seq #"[^-]+" s))
          T (map clojure.string/capitalize t)]
      (apply str (concat h T)))))

(defcheck solution-fd7ae8d5
  (fn camel [s]
    (let [splitted (clojure.string/split s #"-")]
      (clojure.string/join (cons (first splitted)(map clojure.string/capitalize (rest splitted))))
      )))

(defcheck solution-fd89d36c
  (fn [s]
    (->> (re-seq #"[^-]+" s)
      ((fn [[x & xs]] (cons x
                        (map clojure.string/capitalize xs))))
      (apply str))))

(defcheck solution-fe8b1095
  (fn [s]
    (let [ws (clojure.string/split s #"-")]
      (apply str (cons (first ws)
                   (map clojure.string/capitalize (rest ws)))))))

(defcheck solution-fe8ce44a
  (fn [w]
    (apply str (second
                 (reduce
                   (fn [[flag acc] x]
                     (if (= \- x)
                       [true acc]
                       [false (if flag
                                (conj acc (first (.. x toString toUpperCase)))
                                (conj acc x))]))
                   [false []]
                   (seq w))))))

(defcheck solution-ff71e64a
  #(apply str (loop [x (rest %) y [(first %)]]

                (if (empty? x)
                  y
                  (if (= (first x) \-)
                    (recur (nnext x) (conj y (clojure.string/upper-case (second x))))
                    (recur (next x) (conj y (first x)))
                    )
                  ))
     ))

(defcheck solution-ff78c9da
  (fn [s]
    (let [words (clojure.string/split s #"-")]
      (apply str
        (first words)
        (map #(clojure.string/join (cons (char-upper-case (first %)) (rest %))) (rest words))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-102))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

