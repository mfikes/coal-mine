(ns coal-mine.problem-70
  (:require [coal-mine.checks :refer [defcheck-70] :rename {defcheck-70 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10d5447a
  (fn [c]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
      (re-seq #"[a-zA-Z]+" c))))

(defcheck solution-1128c32a
  #(sort-by
     (fn [s] (clojure.string/lower-case s))
     (-> % (clojure.string/replace #"[.!]" "") (clojure.string/split #" "))))

(defcheck solution-112abb86
  (fn [s] (sort-by #(.toLowerCase %) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-11c2447a
  (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split s #"[ .!]+"))))

(defcheck solution-127151ab
  (fn f [s]
    (sort-by clojure.string/lower-case (re-seq #"[A-Za-z]+" s))))

(defcheck solution-12a1d454
  (fn [s]
    (let [a (clojure.string/replace s #"[!.]" "")
          b (clojure.string/split a #" ")]
      (sort-by #(.toUpperCase %) b))))

(defcheck solution-12a7371f
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-137a2ee4
  (fn sorted-split [sent]
    (let [words (clojure.string/split sent #"[^A-Za-z]")]
      (sort-by #(.toLowerCase %) words))))

;; See CLJS-2453
#_(defcheck solution-13ee288a
    (fn [s]
      (mapv
        (fn [token] (re-find #"[a-zA-Z]*" token))
        (let [tokens (clojure.string/split s #" ")]
          (sort-by (fn [token] (apply str (re-seq #"[a-z]*" (clojure.string/lower-case token)))) tokens)))))

(defcheck solution-14051cb9
  (fn [s]
    (sort #(compare (clojure.string/upper-case %1)
             (clojure.string/upper-case %2))
      (re-seq #"[A-z]+" s))))

(defcheck solution-1405ec49
  (fn [s] (let [m (into {} (map #(-> [(clojure.string/upper-case %) %]) (re-seq #"\w+" s)))] (map m (sort (keys m))))))

(defcheck solution-146cc29c
  (fn [w]
    (sort #(compare
             (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (clojure.string/split w #"[^A-Za-z]+"))))

(defcheck solution-158a7304
  #(sort-by (fn [v] (.toLowerCase v)) (re-seq #"\w+" %)))

(defcheck solution-15aa0143
  (fn [phrase]
    (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2)) (re-seq (re-pattern "\\w+") phrase))
    ))

(defcheck solution-15ffa0de
  (fn split [a-str]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split a-str #"[ \.!]"))
    ))

(defcheck solution-164295f5
  (fn [x] (sort-by #(.toLowerCase %) (re-seq #"\w+" x))))

(defcheck solution-1645a358
  (fn [s] (sort-by #(.toLowerCase %) (clojure.string/split (clojure.string/replace s #"[!.]" "") #" "))))

(defcheck solution-165e0bdd
  (fn word-sorter [sentence]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" sentence))))

(defcheck solution-1796812b
  (fn [sentence]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" sentence))))

(defcheck solution-179d1b0
  (fn [s]
    (sort-by clojure.string/lower-case (re-seq #"[A-Za-z]+" s))))

(defcheck solution-17cf1fd8
  (fn wsort [line]
    (let [words  (clojure.string/split line #" ")
          filtrd (map #(clojure.string/replace % #"[^a-zA-Z]" "") words)]
      (sort (fn [a, b]
              (compare (.toLowerCase a) (.toLowerCase b)))
        filtrd))))

(defcheck solution-180741f3
  #(sort-by clojure.string/lower-case (clojure.string/split % #"\W")))

(defcheck solution-18262242
  (fn my-sort [string]
    (let [words (clojure.string/split string #"\W")]
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) words))))

(defcheck solution-1846d266
  (fn [x] (sort-by clojure.string/upper-case (re-seq #"\w+" x))))

(defcheck solution-186d1173
  #(sort-by clojure.string/upper-case (clojure.string/split (clojure.string/replace % #"[.!]" "") #"\s")))

(defcheck solution-18795d52
  (fn [x]
    (let [lowercase (map #(clojure.string/lower-case %) (map #(clojure.string/replace % #"\W" "") (clojure.string/split x #" ")))
          original  (map #(clojure.string/replace % #"\W" "") (clojure.string/split x #" "))

          ]
      (vals (sort (into {} (map vec (partition 2 (interleave lowercase original))))))
      )))

(defcheck solution-190af8d0
  #(sort-by (fn [x] (.toUpperCase x)) (re-seq #"\w+" %)))

(defcheck solution-1a11fb49
  (fn [s]
    (->> (re-seq #"\w+" s)
      (sort-by (fn [x] (.toLowerCase x))))))

(defcheck solution-1a6f3943
  (fn [words] (sort-by #(.toLowerCase %) (re-seq #"\w+" words))))

(defcheck solution-1ade8810
  (fn [s]
    (sort-by #(.toUpperCase %) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-1b0e10b4
  (fn [st]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
      (clojure.string/split
        (clojure.string/join
          (re-seq #"[A-Za-z ]" st))
        #" "))))

(defcheck solution-1b73730f
  #(sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace % #"[^\w\s]" "") #" ")))

(defcheck solution-1bc72553
  (fn [s]
    (apply vector
      (sort
        #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
        (clojure.string/split
          (clojure.string/replace s #"[.!]" "")
          #" "
          )
        )
      )
    ))

(defcheck solution-1c03a02f
  (fn [s]
    (let [w (re-seq #"[a-zA-z]+" s)
          i (map second (sort (map-indexed #(vector (.toLowerCase %2) %) w)))]
      (map (partial nth w) i))))

(defcheck solution-1cc0917a
  (fn [string]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" string))))

(defcheck solution-1d07c9db
  (fn [s] (sort-by clojure.string/lower-case (clojure.string/split s #"\W+"))))

(defcheck solution-1d838f6d
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"[^\w]"))))

(defcheck solution-1fcebfed
  #(sort-by clojure.string/lower-case (re-seq #"[A-Za-z]+" %)))

(defcheck solution-20fd70d3
  (fn [s]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-21141adb
  (fn [a] (sort-by #(.toLowerCase %) (re-seq #"\w+" a))))

(defcheck solution-21a75bd2
  (fn [x]
    (sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace x #"[.!]" "") #" "))))

(defcheck solution-21f7535a
  (fn [s]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
      (clojure.string/split (clojure.string/replace s #"[\.!]" "") #"\s"))))

(defcheck solution-221025e4
  (fn [s] (let [cs (group-by #(clojure.string/lower-case %)
                     (-> s (clojure.string/replace #"[.!]" "") (clojure.string/split #"[\s]+")))]
            (vec (map #(-> % last first) (sort-by first cs))))))

(defcheck solution-22396146
  (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split s #"\W+"))))

(defcheck solution-22b5dbec
  (fn [s]
    (let [is-alpha (fn [c] (case c
                             \. false
                             \! false
                             true))
          ->str    (fn [cs] (apply str cs))
          words    (fn [z]
                     (map ->str (clojure.string/split (->str (filter is-alpha z)) #" ")))]
      (sort-by clojure.string/lower-case (words s)))))

(defcheck solution-22fb745b
  (fn [words] (->> (clojure.string/split words #"\W+")
                (map #(vector %1 (clojure.string/lower-case %1)))
                (sort-by second)
                (map first))))

(defcheck solution-230e5ca7
  (fn [string]
    (let [words (clojure.string/split string #"\W+")]
      (vals
        (apply sorted-map
          (interleave (map clojure.string/lower-case words) words)
          )
        )
      )
    ))

(defcheck solution-2363ee40
  (fn [s]
    (sort
      (fn [a b] (compare (.toLowerCase a) (.toLowerCase b)))
      (clojure.string/split s #"[^a-zA-Z]+")
      )
    ))

(defcheck solution-23f90c5a
  (fn [c s] (sort c (re-seq #"\w+" s))) (comparator
                                          (fn f [[h1 & t1] [h2 & t2]]
                                            (#(if (zero? %) (f t1 t2) (neg? %))
                                             (compare (clojure.string/lower-case h1)
                                               (clojure.string/lower-case h2))))))

(defcheck solution-242b49d8
  (fn myStrSort [s]
    (sort (fn [s1 s2] (compare (clojure.string/lower-case s1)
                        (clojure.string/lower-case s2)))
      (map #(clojure.string/replace % #"(?i)[^\w]+" "") (clojure.string/split s #" ")))))

(defcheck solution-25334d7
  #(->> % (re-seq #"\w+") (sort-by clojure.string/upper-case)))

(defcheck solution-25426f19
  (comp (partial sort-by #(.toLowerCase %))
        (partial re-seq #"\w+")))

(defcheck solution-2542b726
  (fn [s] (sort-by #(.toUpperCase %) (clojure.string/split s #"\W+"))))

(defcheck solution-257d2fcc
  #(->> (re-seq #"\w+" %)
     (sort-by clojure.string/lower-case)))

(defcheck solution-25f022dd
  (fn [s]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (clojure.string/split s #"\W"))))

(defcheck solution-268def8
  (fn f [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-26ac448
  #(sort-by (fn [a] (clojure.string/upper-case a)) (clojure.string/split (apply str (drop-last %)) #" ")))

(defcheck solution-26be72a1
  (fn [s]
    (let [punctuations (into #{} ",.!")]
      (sort #(compare (clojure.string/upper-case %1)
               (clojure.string/upper-case %2))
        (clojure.string/split (apply str (map #(if (clojure.set/subset? (str %) punctuations) "" %)
                                           s))
          #" ")))))

(defcheck solution-2791bfb6
  (fn [s] (sort-by #(.toLowerCase %)
            (clojure.string/split s #"[ \.!]+"))))

(defcheck solution-27cf9184
  (fn word-sorting [s]
    (let [words (clojure.string/split s #"[ .!?]")]
      (sort-by #(.toUpperCase %) words)
      )
    ))

(defcheck solution-286a6671
  #(sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace % #"[.!]" "") #" ")))

(defcheck solution-29698dc7
  (fn [str]
    (let [parts    (clojure.string/split str #" +")
          filtered (map #(clojure.string/replace % #"[:,;:!.]" "") parts)
          sorted   (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2)) filtered)]
      sorted)))

(defcheck solution-29933614
  (fn [s]
    (let [np  (map #(clojure.string/replace % #"[^\w]" "") (clojure.string/split s #" "))
          lc  (map clojure.string/lower-case np)
          m   (zipmap lc np)
          lcs (sort lc)]
      (map m lcs))))

(defcheck solution-2a2370de
  (fn word-sort [s]
    (sort-by
      clojure.string/lower-case
      (clojure.string/split (apply str (butlast s)) #"\s+"))))

(defcheck solution-2c654763
  #(sort-by
     clojure.string/lower-case
     (clojure.string/split % #"[^a-zA-Z]")))

(defcheck solution-2d06f770
  (fn [s]
    (sort
      #(compare (.toLowerCase %1) (.toLowerCase %2))
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-2dc7aab9
  (fn [s] (sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace s #"[,.!?;]" "") #" "))))

(defcheck solution-2de3a230
  (fn [s]
    (->> s
      (filter (complement #{\. \!}))
      (apply str)
      (#(clojure.string/split % #" "))
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))))))

(defcheck solution-2e4dee32
  (fn [s] (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

(defcheck solution-2ff3fb45
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split
        (clojure.string/replace s #"[.!]" "") #"\s"))))

(defcheck solution-30bab766
  (fn wsort
    [sentence]
    (let [coll (re-seq #"\w+" sentence)]
      (sort-by #(clojure.string/capitalize %) coll))))

(defcheck solution-30c8f14e
  #(sort-by (fn [s] (.toLowerCase s))
     (clojure.string/split % #"[,.;: !?]+")))

(defcheck solution-30dcd3d
  (fn [s]
    (sort (fn [x y] (compare
                      (clojure.string/lower-case x)
                      (clojure.string/lower-case y)))
      (clojure.string/split s #"[ \.\!]"))))

(defcheck solution-3183b8f2
  #(->> % (re-seq #"[A-Za-z]+") (sort-by (fn [s] (.toLowerCase s)))))

(defcheck solution-31b76cb
  (fn __ [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-31b8e1cc
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"\w+" s))))

(defcheck solution-32fecbf4
  (fn [s] (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-3323d04b
  (fn
    [s]
    (sort-by #(.toUpperCase %) (re-seq #"[\d\w]+" s))))

(defcheck solution-3368c12f
  (fn [sentence]
    (->> sentence
      (re-seq #"\w+")
      (sort-by #(.toLowerCase %)))))

(defcheck solution-33ab1021
  (fn [x]
    (let [ws  (re-seq #"[a-zA-Z]+" x)
          cmp (fn [a b] (compare (clojure.string/lower-case a) (clojure.string/lower-case b)))
          ]
      (sort cmp ws))))

(defcheck solution-33e058ce
  (fn [sent]
    (->> sent
      (re-seq #"\w+")
      (sort-by clojure.string/lower-case))))

(defcheck solution-348e1e68
  #(->>
     (-> %
       (clojure.string/replace #"[^a-zA-Z ]" "")
       (clojure.string/split #" "))
     (sort-by clojure.string/lower-case)))

(defcheck solution-35ed262f
  (fn word-sort [sentence]
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (map #(re-find (re-pattern "[A-Za-z]+") %)
        (clojure.string/split sentence #" ")))))

(defcheck solution-36d85307
  (fn sort-word
    [str]
    (sort-by clojure.string/upper-case (clojure.string/split (clojure.string/replace str #"\.|\!" "") #"\ "))))

(defcheck solution-36e2ba40
  (fn [x]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" x))))

(defcheck solution-38048cac
  (fn [x]
    (sort-by clojure.string/lower-case (clojure.string/split x #"[^a-zA-Z]+"))
    ))

(defcheck solution-381a232e
  (fn [s] (->> s
            (re-seq #"\w+")
            (sort-by clojure.string/lower-case))))

(defcheck solution-3830ba11
  (fn [s]
    (sort-by
      #(.toLowerCase %)
      (clojure.string/split s #"[\s.!]+"))))

(defcheck solution-387789c3
  (fn [s] (vec (sort-by #(.toLowerCase %) (clojure.string/split s #"\W")))))

(defcheck solution-39201f3
  (fn [s] (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

(defcheck solution-395d16ab
  (fn [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-3aa2ab2
  (fn [x]
    (sort-by #(.toLowerCase %) (clojure.string/split (apply str (remove #{\! \. \,} x)) #" "))))

(defcheck solution-3ac5b30b
  (fn eka
    [word]
    (->> word
      (re-seq #"[A-Za-z]+")
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))))
    ))

(defcheck solution-3ae97ab4
  (fn [s]
    (sort-by (fn [x] (clojure.string/lower-case x))
      (clojure.string/split
        (clojure.string/replace s #"[!?.,]" " ") #" ")
      )))

(defcheck solution-3af131b6
  (fn [w]
    (sort-by #(.toLowerCase %)
      (re-seq #"\w+" w))))

(defcheck solution-3b961f53
  (fn [x s]
    (let [l (re-seq #"[A-Za-z]+" s)]
      (sort #(compare (x %) (x %2)) l))) clojure.string/lower-case)

(defcheck solution-3c361bf4
  (fn [c]
    (sort #(compare (.toLowerCase %) (.toLowerCase %2)) (re-seq #"\w+" c))))

(defcheck solution-3c64d991
  #(sort-by
     clojure.string/lower-case
     (clojure.string/split % #"[^a-zA-Z]+")))

(defcheck solution-3c6b05ab
  (fn [coll]
    (sort #(compare (.toLowerCase %1)
             (.toLowerCase %2))
      (re-seq #"[A-Za-z]+" coll))))

(defcheck solution-3cae1704
  (fn [s]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" s))
    ))

(defcheck solution-3cb74475
  (fn [sentence]
    (sort-by clojure.string/upper-case (-> sentence
                                         (clojure.string/replace #"[\.!]" "")
                                         (clojure.string/split #"\s+")))))

(defcheck solution-3d2d752e
  #(sort-by clojure.string/lower-case (re-seq #"\w+" %)))

(defcheck solution-3daf5fa0
  (fn sorted-words [sentence]
    (sort #(compare
             (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (clojure.string/split
        (clojure.string/replace sentence #"[^a-zA-Z'\s]" "")
        #"\s"))))

(defcheck solution-3dbe3a4b
  (fn [sentence]
    (sort-by clojure.string/lower-case
      (re-seq #"[A-Za-z]+" sentence))))

(defcheck solution-3e159bfd
  (fn [s] (sort-by #(.toLowerCase %) (clojure.string/split s #"[\s.!]"))))

(defcheck solution-3fa4120c
  #(sort-by clojure.string/lower-case (-> % (clojure.string/replace #"[,.!]" "") (clojure.string/split #" "))))

(defcheck solution-415e24da
  (fn mysort [s]
    (sort
      #(compare (.toLowerCase %1) (.toLowerCase %2))
      (re-seq #"\w+" s))))

(defcheck solution-4229e9b7
  (fn [text]
    (let [words (re-seq #"[a-zA-Z]+" text)]
      (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) words))))

(defcheck solution-423d86dd
  (fn [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-439a6f06
  (fn [s]
    (let [words (re-seq #"\w+" s)]
      (sort-by #(clojure.string/lower-case %) words)
      )))

(defcheck solution-448172fb
  (fn [s]
    (let [strings (re-seq #"[A-Za-z]+" s)]
      (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) strings))))

(defcheck solution-4495e7d4
  (fn wsort [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"[^a-zA-Z]"))))

(defcheck solution-44b99571
  #(sort-by (fn [a] (.toLowerCase a)) (clojure.string/split (clojure.string/replace % #"!|\." "") #" ")))

(defcheck solution-44f331a
  (fn [s]
    (let [v (clojure.string/split s #"\W")]
      (sort-by #(.toLowerCase %) v))))

(defcheck solution-45a67ec8
  (fn [s]
    (sort-by #(.toLowerCase %) (clojure.string/split s #"\W"))))

(defcheck solution-45f36cb8
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"[^ !.]+" s))))

(defcheck solution-465d406d
  (fn [str] (sort-by #(.toUpperCase %) (clojure.string/split str #"[ .!]+"))))

(defcheck solution-467c4123
  (fn [s]
    (sort (fn [a b]
            (compare (clojure.string/lower-case a) (clojure.string/lower-case b)))
      (clojure.string/split (re-find #"[A-za-z ]+" s) #"\s+"))))

(defcheck solution-470f7f34
  (fn [str] (sort-by #(.toLowerCase %) (re-seq #"[A-Za-z]+" str))))

(defcheck solution-473f629c
  (fn [str]
    (let [sanitized (clojure.string/replace str #"[^a-zA-Z\s]" "")
          split     (clojure.string/split sanitized #" ")
          sort-fn   #(compare
                       (clojure.string/lower-case %1)
                       (clojure.string/lower-case %2))]
      (sort sort-fn split))))

(defcheck solution-4826165d
  (fn word-sort [st]
    (let [st (subs st 0 (dec (count st)))]
      (flatten
        (sort-by #(.toLowerCase %)
          (clojure.string/split st #"\s"))))))

(defcheck solution-483e827b
  (fn [s]
    (sort-by
      (memfn toLowerCase)
      (clojure.string/split s #"\W+"))))

(defcheck solution-4884411b
  #(remove
     (fn [x] (nil? (re-matches #"\w+" x)))
     (sort (fn [a b] (compare (.toUpperCase a) (.toUpperCase b)))
       (map (fn [z] (apply str z)) (partition-by (fn [c] (nil? (re-matches #"\w+" (str c)))) (seq %))))))

(defcheck solution-48d586b4
  #(sort (fn [a b] (compare (.toLowerCase a) (.toLowerCase b))) (re-seq #"\w+" %)))

(defcheck solution-4925781c
  (fn [s]
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"\w+" s))))

(defcheck solution-4944eb47
  (fn ws [phrase]
    (let [sort-by-word #(sort-by clojure.string/lower-case %)
          split        #(clojure.string/split % #"\W+")]
      (sort-by-word (split phrase)))))

(defcheck solution-495a3c54
  #(sort-by (fn [c] (.toLowerCase c)) (re-seq #"\w+" %)))

(defcheck solution-4a1bf832
  (fn [s]
    (sort-by
      #(.toUpperCase %1)
      (re-seq #"\w+" s))))

(defcheck solution-4a246009
  #(filter (comp not empty?) (sort-by clojure.string/lower-case (clojure.string/split % #"[ !.]"))))

(defcheck solution-4a5f81cd
  (fn [s]
    (sort-by
      clojure.string/lower-case
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-4a6db7ee
  (fn [w]
    (sort-by clojure.string/lower-case (filter (comp not empty?) (re-seq #"\b\w*\b" w)))
    ))

(defcheck solution-4ac97000
  #(->> %
     (re-seq #"\w+")
     (sort-by clojure.string/upper-case)))

(defcheck solution-4ade8ab2
  (fn [s]
    (sort
      #(compare
         (clojure.string/lower-case %1)
         (clojure.string/lower-case %2))
      (remove
        #(empty? %)
        (clojure.string/split
          s
          #"[ !\.]")))))

(defcheck solution-4bcab87d
  #(sort-by clojure.string/upper-case
     (clojure.string/split % #"[\., !]")
     ))

(defcheck solution-4c29cbb8
  #(map second
     (sort-by first
       (map (fn [x] [(.toLowerCase x) x]) (re-seq #"[a-zA-Z]+" %)))))

(defcheck solution-4ca708aa
  #(sort-by clojure.string/lower-case (clojure.string/split % #"[ \W]")))

(defcheck solution-4cd3171f
  (fn [string]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" string))))

(defcheck solution-4d12e093
  #(sort-by
     clojure.string/lower-case
     (clojure.string/split % #"[ !.]")))

(defcheck solution-4d668f70
  #(sort-by
     clojure.string/lower-case
     (re-seq #"[a-zA-Z]+" %)

     ))

(defcheck solution-4dc2a9c8
  (fn splitX [s] (sort-by clojure.string/lower-case (clojure.string/split (apply str (re-seq #"[a-zA-Z ]" s)) #" "))))

(defcheck solution-4e0fb0ad
  (fn wd [s]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"\w+" s))))

(defcheck solution-4e398164
  #(sort-by clojure.string/lower-case (clojure.string/split % #"[ .!]")))

(defcheck solution-4e617ecc
  (fn [s]
    (sort-by #(clojure.string/lower-case %)
      (clojure.string/split s #"[ .!]"))))

(defcheck solution-4e92c254
  #(into [] (apply sorted-set-by
              (fn [a b] (compare (clojure.string/lower-case a) (clojure.string/lower-case b)))
              (re-seq #"\w+" %))))

(defcheck solution-50aa88
  (fn [s]
    (vals (sort (zipmap (map clojure.string/lower-case (clojure.string/split s #" ")) (clojure.string/split (clojure.string/replace s #"[\.!]" "") #" "))))))

(defcheck solution-51ebe8a9
  (fn [s] (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split s #"[\s.!]+"))))

(defcheck solution-5216c924
  (fn [sstr]
    (let [sseq  (re-seq #"\w+" sstr)
          scomp (fn [s1 s2]
                  (let [s1 (clojure.string/upper-case s1)
                        s2 (clojure.string/upper-case s2)]
                    (compare s1 s2)))]
      (sort scomp sseq))))

(defcheck solution-521a7db8
  (fn [s]
    (sort-by #(.toLowerCase %)
      (clojure.string/split s #"[ !,.]"))))

(defcheck solution-527b53d7
  (fn [s]
    (let [words (->
                  s
                  (clojure.string/replace #"[^\w\s]" "")
                  (clojure.string/split #"\s+"))]
      (->>
        words
        (map #(vector (clojure.string/lower-case %) %))
        (sort-by first)
        (map last))
      )
    ))

(defcheck solution-52aa704b
  (fn [s]
    (let [w (sort (clojure.string/split (apply str (re-seq #"[A-Za-z ]" s)) #" "))
          l (map clojure.string/lower-case w)
          m (zipmap l w)]
      (for [ss (sort l)] (m ss)))))

(defcheck solution-53a6e633
  (comp (partial sort-by clojure.string/lower-case) (partial re-seq #"\w+")))

(defcheck solution-543e0321
  (fn [s]
    (let [clean (clojure.string/replace s #"[\s\.\!\?]" " ")]
      (let [niz (clojure.string/split clean #" ")]
        (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) niz)))))

(defcheck solution-5504d2ff
  (fn [s]
    (let [words (re-seq #"\w+" s)]
      (sort-by clojure.string/lower-case words))))

(defcheck solution-55434dde
  (fn [s]
    (->>
      (clojure.string/split s #"[\ \.\!]")
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))))
    ))

(defcheck solution-55927a4d
  (fn n70 [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-55a2ba1
  (fn foo [s]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split s #"\W"))))

(defcheck solution-55b34ee2
  (fn sorted-words [s]
    (sort-by #(.toUpperCase %) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-5605940
  #(sort (fn [a1 a2] (compare (clojure.string/upper-case a1) (clojure.string/upper-case a2))) (seq (clojure.string/split (clojure.string/replace % #"[\.\!]" "") #" "))))

(defcheck solution-56332dfd
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"[^A-Za-z]"))))

(defcheck solution-56f8814
  (fn [s]
    (-> s
      (clojure.string/replace #"[^\w\s]" "")
      (clojure.string/split #"\s+")
      (->>
        (sort-by clojure.string/lower-case)))))

(defcheck solution-57009817
  (fn [str] (sort-by #(.toUpperCase %) (re-seq #"\w+" str))))

(defcheck solution-57a51c40
  #(sort (fn [s1 s2] (compare (.toLowerCase s1) (.toLowerCase s2)))
     (map (fn [s] (apply str
                    (seq (filter (fn [c] ((set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") c)) s))))
       (clojure.string/split % #" "))))

(defcheck solution-58c584d3
  (fn word-sorting [s]
    (sort-by #(.toUpperCase %) (map (fn [x] (clojure.string/replace x #"\.|\,|\!" "")) (clojure.string/split s #" ")))))

(defcheck solution-5924872b
  (fn [s]
    (letfn [(split [s] (re-seq #"\w+" s))]
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
        (split s)))))

(defcheck solution-595953a3
  (fn s [x]
    (let [t (clojure.string/split x #"\s+"),
          s (map #(clojure.string/replace % #"\W" "") t)]
      (sort (fn [s1 s2]
              (compare (.toUpperCase s1) (.toUpperCase s2))) s))))

(defcheck solution-5969315
  #(sort-by clojure.string/upper-case (re-seq #"[A-Za-z]+" %)))

(defcheck solution-5aa55e68
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"\W"))))

(defcheck solution-5b105375
  (fn [s] (
            sort-by clojure.string/lower-case
            ; sort #(apply compare (map clojure.string/lower-case [%1 %2]))
            (re-seq #"\w+" s))))

(defcheck solution-5bd5a300
  (fn [s]
    (sort
      #(compare (clojure.string/lower-case %1)
         (clojure.string/lower-case %2))
      (clojure.string/split s #"\W"))))

(defcheck solution-5c4c79b
  (fn [a] (sort-by clojure.string/lower-case (re-seq #"\w+" a))))

(defcheck solution-5c6711d8
  (fn [s]
    (sort
      #(compare
         (clojure.string/upper-case %1)
         (clojure.string/upper-case %2))
      (clojure.string/split s #"[ !\\.]"))))

(defcheck solution-5d299bf6
  #(->> (clojure.string/split % #"\W+")
     (sort-by clojure.string/lower-case)))

(defcheck solution-5d7d9933
  (fn [s]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"\w+" s)
      )
    ))

(defcheck solution-5dff1597
  (fn sort-s [s]
    (sort-by clojure.string/upper-case
      (re-seq #"\w+" s))))

(defcheck solution-5fa8eba2
  (fn [t] (sort-by #(clojure.string/lower-case %) (re-seq #"[A-Za-z]+" t))))

(defcheck solution-600cd44c
  (fn [x] (sort-by clojure.string/lower-case (re-seq #"\w+" x))))

(defcheck solution-6038da08
  (fn g [str] (sort-by #(.toLowerCase %) (re-seq #"\w+" str))))

(defcheck solution-60cd34a7
  (fn [sentence]
    (let [word-vec (clojure.string/split sentence #" ")]
      (sort-by clojure.string/lower-case (map #(clojure.string/replace % #"[.!?\\-]" "") word-vec)))))

(defcheck solution-610774e9
  (fn [s]
    (let [words (clojure.core/re-seq #"[a-zA-z]+" s)]
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) words))))

(defcheck solution-61a978b9
  (fn [x]
    (->> x
      (#(clojure.string/replace % #"[.,!?]" ""))
      (#(clojure.string/split % #" "))
      (map #(vector (clojure.string/upper-case %) %))
      (sort-by first)
      (map last))))

(defcheck solution-61d7085e
  (fn my-sort [stuff]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" stuff))))

(defcheck solution-62a98198
  (fn wsort [st]
    (let [words (clojure.string/split
                  (clojure.string/replace st #"[^A-Za-z ]" "")
                  #" ")
          cmp   (fn [w1 w2]
                  (compare (clojure.string/lower-case w1)
                    (clojure.string/lower-case w2)))]
      (sort cmp words))))

(defcheck solution-633a549b
  (fn [x] (sort-by #(.toLowerCase %) (re-seq #"\w+" x))))

(defcheck solution-63814925
  (fn [s]
    (sort-by #(clojure.string/upper-case %) (re-seq #"\w+" s))))

(defcheck solution-64118b2f
  #(sort-by clojure.string/lower-case (re-seq #"\w+" %)))

(defcheck solution-64c2dea
  #(sort-by clojure.string/lower-case compare (clojure.string/split % #"[\s\.\!]+")))

(defcheck solution-656da4d0
  (fn [string]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2)
             ) (clojure.string/split string #"[^a-zA-Z]+"))))

(defcheck solution-66184e94
  (fn [s]
    (->> s (re-seq #"[a-zA-Z]+") (sort #(compare (.toLowerCase %1) (.toLowerCase %2))))))

(defcheck solution-662d367a
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"\W"))))

(defcheck solution-6691ee96
  (fn [s] (sort #(compare (.toUpperCase %1) (.toUpperCase %2)) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-66c9d20e
  (fn [x] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split x #"[\s.,?!]"))))

(defcheck solution-672ae014
  #(sort-by
     clojure.string/capitalize
     (clojure.string/split % #"\s+|[.!]")))

(defcheck solution-68542fc
  (fn [s]
    (sort-by #(.toUpperCase %)
      (re-seq #"\w+" s))))

(defcheck solution-68ce062
  (fn [sen] (sort-by #(.toLowerCase %) (clojure.string/split sen #"[^\w]"))))

(defcheck solution-68fdcecd
  (fn [s] (sort-by (fn [w] (.toUpperCase w)) (re-seq #"\w+" s))))

(defcheck solution-6aa82294
  (fn jj [x]
    (sort-by #(clojure.string/lower-case %)

      (clojure.string/split (apply str (re-seq #"[a-zA-Z]|\s" x)) #"\s")

      )
    ))

(defcheck solution-6ab5a5a8
  #(sort-by clojure.string/upper-case (clojure.string/split % #"\W+")))

(defcheck solution-6b538bc6
  #(->>
     (re-seq #"[a-zA-Z]+" %)
     (sort-by clojure.string/lower-case)))

(defcheck solution-6b56d06d
  #(sort-by
     clojure.string/lower-case
     (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-6bf7b3b0
  (fn [coll] (sort-by #(.toLowerCase %) (map first (re-seq #"([\w]+)" coll)))))

(defcheck solution-6c303a2
  (fn [s] (apply concat (vals (conj (sorted-map) (group-by #(.toUpperCase %) (re-seq #"\w+" s)))))))

(defcheck solution-6c96c6ec
  (fn [w] (sort-by #(.toLowerCase %) (re-seq #"[aA-zZ]+" w))))

(defcheck solution-6cda6a9
  (fn word-sorting [s]
    (sort #(compare (clojure.string/lower-case %) (clojure.string/lower-case %2))
      (clojure.string/split s #"\W"))))

(defcheck solution-6d2cfccb
  #(sort-by clojure.string/lower-case (clojure.string/split % #"\W+")))

(defcheck solution-6d3479a3
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"\w+" s))))

(defcheck solution-6d77406c
  (fn [str]
    (->> (clojure.string/split str #"[\W]")
      (remove empty?)
      (sort #(apply compare (map clojure.string/lower-case %&))))))

(defcheck solution-6d90968c
  (fn lc-sort [s]
    (letfn [(cp [s] (apply str (filter (set "abcdefghijklmnopqrstuvwxyz") (.toLowerCase s))))]
      (sort (fn [a b] (compare (cp a) (cp b))) (re-seq #"\w+" s)))))

(defcheck solution-6da07af2
  (fn [x]
    (sort #(compare (.toLowerCase %)
             (.toLowerCase %2))
      (re-seq #"\w+" x))))

(defcheck solution-6e91502a
  (fn sorting [x] (sort-by #(.toUpperCase %) (#(re-seq #"\w+" %) x))))

(defcheck solution-6ee7a49e
  #(sort-by (fn [s] (. s toUpperCase)) (re-seq #"\w+" %)))

(defcheck solution-6f3a62dd
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-6f9e771c
  (fn [s]
    (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
      (-> (clojure.string/replace s #"[^\w ]" "")
        (clojure.string/split #" ")))))

(defcheck solution-6fb8f145
  (fn [s]
    (sort-by #(.toLowerCase %)
      (clojure.string/split s #"\W"))))

(defcheck solution-6fd7a447
  (fn [s]
    (let [l clojure.string/lower-case]
      (sort #(compare (l %)
               (l %2))
        (clojure.string/split s #" |\.|!")))))

(defcheck solution-7112e965
  (fn [s]
    (sort-by #(clojure.string/lower-case %)
      (clojure.string/split
        (apply str (butlast s)) #" "))))

(defcheck solution-7175d71a
  (fn [s]
    (into [] (into (sorted-set-by #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))) (clojure.string/split (clojure.string/replace s #"\.|\,|\?|\!" "") #" ")))))

(defcheck solution-71d319df
  (fn [w] (sort-by clojure.string/lower-case (re-seq #"\w+" w))))

(defcheck solution-72387ef0
  (fn [s]
    (map second
      (sort
        (map
          (fn [sss] (let [ss (clojure.string/replace sss #"[.!,:]" "")]
                      [(clojure.string/lower-case ss) ss]))
          (clojure.string/split s #" "))))))

(defcheck solution-72948524
  (fn m [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"\W"))))

(defcheck solution-73555b
  (fn [s]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

(defcheck solution-738ce255
  (fn
    [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"\W"))))

(defcheck solution-74067f7
  #(vec (sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace % #"[^\sa-zA-Z]" "") #"\s+"))))

(defcheck solution-74087e9e
  (fn word-sort [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-7524177d
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"[^\w]+"))))

(defcheck solution-75de7ae1
  (fn [s]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (clojure.string/split s #"[!.\s]+"))))

(defcheck solution-7674798c
  #(sort-by (fn [s] (.toLowerCase s)) (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-7685814b
  (fn [s]
    (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
      (clojure.string/split s #"[^\w]+"))))

(defcheck solution-76ab8228
  (fn [s] (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split (#(clojure.string/replace % #"[.,!]" "") s) #" "))))

(defcheck solution-776a7b9
  (fn word-sorting [s]
    (sort #(apply compare (map clojure.string/lower-case [%1 %2])) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-779077bc
  (fn [s] (sort #(compare (clojure.string/capitalize %1) (clojure.string/capitalize %2)) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-788681d8
  (fn __
    [string]
    (let [split   #(clojure.string/split % #"\s")
          replace #(clojure.string/replace % #"\W" "")
          my-comp #(compare (.toLowerCase %1) (.toLowerCase %2))]
      (->> string split (map replace) (sort my-comp)))))

(defcheck solution-7886d96c
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split
        (clojure.string/replace s #"[^a-zA-Z ]+" "")
        #" "))))

(defcheck solution-7933c321
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"[^ !.]+" s))))

(defcheck solution-7a121d24
  #(sort-by clojure.string/lower-case (clojure.string/split %1 #"[ .!]")))

(defcheck solution-7a48afea
  (fn [s] (sort-by #(.toLowerCase %)
            (clojure.string/split
              (clojure.string/replace s #".$" "") #" "))))

(defcheck solution-7acbaab2
  (fn [string]
    (sort
      #(compare (.toLowerCase %1) (.toLowerCase %2))
      (clojure.string/split string #"\W"))))

(defcheck solution-7b5a9f3
  (fn sorted-words [string]
    (sort-by clojure.string/lower-case (re-seq #"\w+" string))))

(defcheck solution-7bdf8e0f
  (fn [st]
    (let [mycmp (fn [a b] (compare (.toLowerCase a) (.toLowerCase b)))]
      (sort mycmp (clojure.string/split st #"[\s\[!?.]")))))

(defcheck solution-7c34a4fa
  (fn sort-abc [s]
    (->> s
      (re-seq #"\w+")
      (map #(list (.toLowerCase %) %))
      (sort-by first)
      (map second))))

(defcheck solution-7c79b611
  (fn sort-words [s]
    (sort #(compare
             (.toUpperCase %1)
             (.toUpperCase %2))
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-7cd6e845
  (fn wsort [words]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (clojure.string/split (clojure.string/replace words #"[!.]" "") #"\s"))))

(defcheck solution-7cd7f187
  (fn solit-sentence [xs]
    (->> (re-seq #"\w+|\d+" xs) (sort-by #(.toLowerCase %)))))

(defcheck solution-7d670ff5
  #(sort-by clojure.string/upper-case (clojure.string/split (clojure.string/replace %1 #"[.!,?]" "") #" ")))

(defcheck solution-7df6daf4
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"\W"))))

(defcheck solution-7e099883
  (fn word-sorting
    [phrase]
    (let [clean-phrase (-> phrase (clojure.string/replace #"[^\s\w]" ""))
          words        (map #(vector (clojure.string/lower-case %) %) (clojure.string/split clean-phrase #" "))]
      (map second (sort words)))))

(defcheck solution-7e1d6211
  (fn [s] (sort-by #(clojure.string/lower-case %) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-7e406eca
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-7e559eca
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"\w+" s))))

(defcheck solution-7e80e4c7
  (fn [s]
    (sort-by #(.toLowerCase %)
      (re-seq #"\w+" s))))

(defcheck solution-7eae31fd
  (fn [s] (sort #(compare (.toUpperCase %1) (.toUpperCase %2)) (clojure.string/split s #"[ \.\!]"))))

(defcheck solution-7ec611d4
  (fn sort-word [s]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

(defcheck solution-7f79c444
  (fn [s]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"\w+" s))))

(defcheck solution-7fb9785e
  #(->
     (clojure.string/replace % #"[^A-Za-z ]" "")
     (clojure.string/split #" ")
     ((fn [coll] (sort-by clojure.string/lower-case coll)))
     ))

(defcheck solution-7fd2f8f0
  (fn [sentence] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (re-seq #"\w+" sentence))))

(defcheck solution-7fe25ad4
  (fn [x]
    (vec (sort-by (fn [y] (.toLowerCase y)) (re-seq #"[a-zA-Z]+" x)))))

(defcheck solution-803bf9d6
  (fn myWordSorting
    [word]
    (let [words (clojure.string/split (clojure.string/replace word #"[!.?,]" "") #"\s")]
      (sort-by #(.toLowerCase %) words))))

(defcheck solution-811c93cf
  (fn [s] (sort-by #(.toUpperCase %) (clojure.string/split s #"\W+"))))

(defcheck solution-812899c7
  (fn [str]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"\w+" str))))

(defcheck solution-8164d444
  #(->> %
     (re-seq #"[a-zA-Z]+")
     (sort (fn [a b] (compare (.toLowerCase a)
                       (.toLowerCase b)))
       )))

(defcheck solution-81977654
  (fn [s]
    (sort-by #(.toLowerCase %) (clojure.string/split s #"\W+"))))

(defcheck solution-819b5b04
  (fn my-func
    [str]
    (sort-by clojure.string/lower-case
      (remove clojure.string/blank?
        (clojure.string/split str #"[\s\.\!]")))))

(defcheck solution-8306adfa
  (fn sortword [strr]
    (sort-by clojure.string/lower-case (re-seq #"[a-zA-Z]+" strr))))

(defcheck solution-830f4ac6
  (fn [s] (sort-by #(clojure.string/lower-case %) (re-seq #"\w+" s))))

(defcheck solution-831e62c3
  (comp (partial sort-by clojure.string/lower-case) #(clojure.string/split % #"[^A-Za-z]")))

(defcheck solution-832a8a6
  (fn [s]
    (sort (fn [a b] (compare (clojure.string/upper-case a) (clojure.string/upper-case b))) (re-seq #"\w+" s))))

(defcheck solution-832c8876
  (fn sortWords
    [string]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split (clojure.string/replace (clojure.string/replace string "." "") "!" "") #" "))))

(defcheck solution-833dce64
  (fn [st]
    (sort-by clojure.string/lower-case (re-seq #"\w+" st))))

(defcheck solution-834226c
  #(sort-by (fn [s] (.toLowerCase s)) (clojure.string/split % #"[\s\.!]")))

(defcheck solution-83e02722
  (fn myf2 [s]
    (->>
      (clojure.string/split s #"[\s\.!]")

      (sort-by #(.toUpperCase %))
      )))

(defcheck solution-85011e52
  #(sort
     (fn [x y]
       (let [lc clojure.string/lower-case]
         (compare (lc x) (lc y))))
     (re-seq #"\w+" %)))

(defcheck solution-85551a18
  (fn [st]
    (->> (clojure.string/split st #"\W")
      (sort-by clojure.string/lower-case))))

(defcheck solution-85975e39
  (fn [s] (sort (fn [a b] (compare (clojure.string/lower-case a) (clojure.string/lower-case b))) (clojure.string/split s #"\W+"))))

(defcheck solution-85c90e6c
  (fn [s] (sort
            #(compare (.toUpperCase %1) (.toUpperCase %2))
            (re-seq #"\w+" s))))

(defcheck solution-863283bd
  (comp (partial sort-by clojure.string/lower-case)
        (partial re-seq #"\w+")))

(defcheck solution-870e1f82
  (fn [s]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
      (clojure.string/split (clojure.string/replace s #"[^\w\s]" "") #"\s"))))

(defcheck solution-876aa92c
  (fn [s]
    (->> (clojure.string/split s #" ")
      (map (fn [w] (clojure.string/replace w #"[^a-zA-Z]" "")))
      (sort-by clojure.string/upper-case))))

(defcheck solution-87b36cc5
  (fn [s] (let [wds (re-seq #"\w+" s)]
            (map (into {} (map #(vector (.toLowerCase %) %) wds))
              (sort (map #(.toLowerCase %) wds))))))

(defcheck solution-87f64429
  (fn [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-887ee038
  (fn [s]
    (sort
      #(apply compare (map clojure.string/lower-case [%1 %2]))
      (map #(clojure.string/replace % #"[\.|!]" "")
        (clojure.string/split s #" ")))))

(defcheck solution-88b8d518
  (fn mysort [s]
    (sort-by #(clojure.string/lower-case %)
      (clojure.string/split s #"[ .!,]"))))

;; See CLJS-2453
#_(defcheck solution-88c6513d
    (fn [k]
      (sort
        (fn [a b]
          (let [d #(->> %1 (.toUpperCase) (map int)) f (d a) l (d b)]
            (loop [[fl & fs] f [ll & ls] l]
              (cond
                (= fl ll) (recur fs ls)
                (> fl ll) false
                (< fl ll) true)))) (re-seq #"(?i)\w+" k))))

;; See CLJS-2453
#_(defcheck solution-88f7bec9
    (fn [s] (sort-by str #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"\w+" s))))

;; See CLJS-2453
#_(defcheck solution-891356ed
    (fn my-split [s]
      (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

;; See CLJS-2453
#_(defcheck solution-895213b4
    (fn [s] (->> s (re-seq #"\w+") (sort-by clojure.string/lower-case))))

;; See CLJS-2453
#_(defcheck solution-89dca6c3
    (fn word-sort [s]
      (->> (re-seq #"\w+" s)
        (sort-by clojure.string/lower-case))
      ))

(defcheck solution-89e62710
  #(sort-by clojure.string/lower-case (clojure.string/split (apply str (drop-last %)) #" ")))

(defcheck solution-8a152999
  (fn [s]
    (sort #(compare
             (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (clojure.string/split s #"\W"))))

;; See CLJS-2453
#_(defcheck solution-8a1e573e
    #(sort-by
       (fn [w] (clojure.string/lower-case w))
       (re-seq #"\w+" %)))

(defcheck solution-8bbd84ff
  (fn [x] (sort-by #(.toLowerCase %)
            (clojure.string/split x #"[\s\.!]+"))))

;; See CLJS-2453
#_(defcheck solution-8c643c5e
    (fn [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

;; See CLJS-2453
#_(defcheck solution-8c9c5e65
    (comp
     (partial sort-by clojure.string/lower-case)
     #(clojure.string/split (clojure.string/replace % #"[!.?]" "") #"\s+")))

(defcheck solution-8cfc0f89
  (fn
    [s]
    (sort-by #(clojure.string/lower-case %)
      (clojure.string/split (clojure.string/replace s
                              #"[.!]"
                              "")
        #" "))))

(defcheck solution-8d9760e0
  (fn [s]
    (sort-by #(clojure.string/lower-case %)
      (clojure.string/split s #"[ !.]"))))

(defcheck solution-8df15049
  (fn [x]
    (sort
      (fn [s1 s2] (compare (.toLowerCase s1) (.toLowerCase s2)))
      (clojure.string/split
        (apply str (filter (complement (set ".,!?")) x))
        #" "
        )
      )
    ))

;; See CLJS-2453
#_(defcheck solution-8e33d616
    (fn [astr]
      (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
        (clojure.string/split astr #"\W+"))))

;; See CLJS-2453
#_(defcheck solution-8e6eede7
    (fn [s]
      (sort-by clojure.string/lower-case
        (re-seq #"[A-Za-z]+" s))))

(defcheck solution-8ee6bb18
  (fn [string]
    (let [x (clojure.string/split string #"\W")]
      (sort-by #(clojure.string/lower-case %) x))))

;; See CLJS-2453
#_(defcheck solution-8f74f239
    (fn [str]
      (->> str
        (re-seq #"\w+")
        (sort-by #(.toLowerCase %)))))

;; See CLJS-2453
#_(defcheck solution-8f7bc522
  (fn [s]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (re-seq #"\w+" s))))

;; See CLJS-2453
#_(defcheck solution-8fb93b14
  #(sort-by (comp str clojure.string/lower-case) (re-seq #"\w+" %)))

;; See CLJS-2453
#_(defcheck solution-8fbe713c
  (fn words [s]
    (let [lower clojure.string/lower-case]
      (->> s (re-seq #"\w+") (sort-by lower)))))

(defcheck solution-8fdc6314
  (fn f [s]
    (->> (vec s)
      (remove #{\. \!})
      (apply str)
      (#(clojure.string/split % #" "))
      (sort-by clojure.string/lower-case)
      vec)))

;; See CLJS-2453
#_(defcheck solution-9065a64d
  (fn [sentence]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"[A-z]+" sentence))
    ))

;; See CLJS-2453
#_(defcheck solution-9066fa89
  (fn [s]
    (sort-by clojure.string/lower-case
      (re-seq #"\w+" s))))

;; See CLJS-2453
#_(defcheck solution-9086e27e
  (fn ws [string]
    (let [words (re-seq #"[A-Za-z]+" string)]
      (sort #(compare (clojure.string/upper-case %1)
               (clojure.string/upper-case %2))
        words))))

(defcheck solution-90c6f660
  (fn [astr]
    (sort-by clojure.string/upper-case (clojure.string/split astr #"[\s\.\!]"))
    ))

;; See CLJS-2453
#_(defcheck solution-91abf5fa
  #(->> % (re-seq #"\w+") (sort-by clojure.string/lower-case)))

(defcheck solution-91c9acb6
  (fn [xs] (sort-by clojure.string/upper-case (clojure.string/split xs #"\.| |!"))))

(defcheck solution-91d0c843
  (fn [sentence] (sort-by clojure.string/upper-case (clojure.string/split (clojure.string/replace sentence #"[\.!]+" "") #" "))))

(defcheck solution-922e6949
  (fn [s] (sort-by clojure.string/lower-case (clojure.string/split s #"[\!\.\s]+"))))

;; See CLJS-2453
#_(defcheck solution-92f9139a
  (fn [sentence]
    (let [sorter #(compare (.toUpperCase %1)
                    (.toUpperCase %2))]
      (sort sorter (re-seq #"\w+" sentence)))))

(defcheck solution-93120391
  (fn [s]
    (let
     [words      (map #(clojure.string/replace % #"\W+" "") (clojure.string/split s #"\s+"))
      words-hash (into {} (for [w words] [(clojure.string/lower-case w) w]))]
      (for [sw (sort (keys words-hash))] (words-hash sw)))))

(defcheck solution-935601de
  (fn [sentence]
    (let [words (-> sentence
                  (clojure.string/replace #"[.!?,]" "")
                  (clojure.string/split #" "))]
      (sort #(let [w1 (clojure.string/lower-case %1)
                   w2 (clojure.string/lower-case %2)]
               (compare w1 w2)) words))))

(defcheck solution-937ea873
  (fn sort-words
    [str]
    (let [strings (into [] (clojure.string/split (clojure.string/replace str #"[\.!]" "") #" "))]
      (let [lower-strings (into [] (map clojure.string/lower-case strings))]
        (let [lower-to-index (clojure.set/map-invert
                               (into {} (map-indexed (fn [x y] [x y]) lower-strings)))]
          (map strings (map lower-to-index (sort lower-strings))))))))

(defcheck solution-93aaa87a
  (fn [msg] (sort-by (fn [s] (clojure.string/lower-case s)) (clojure.string/split (clojure.string/replace msg #"[.!,\?]" "") #" "))))

(defcheck solution-95041fae
  (fn [s]
    (-> s
      (subs 0 (dec (count s)))
      (clojure.string/split #"\s")
      (->> (sort-by #(.toLowerCase %))))))

;; See CLJS-2453
#_(defcheck solution-9537dd2
  (fn [in] (sort-by #(.toUpperCase %) (re-seq #"\w+" in))))

;; See CLJS-2453
#_(defcheck solution-95a2531e
  #(->> % (re-seq #"\w+") (sort-by (memfn toLowerCase))))

(defcheck solution-95ad481c
  (fn [s]
    (sort-by clojure.string/upper-case (clojure.string/split (clojure.string/replace s #"[.!]" "")
                                         #" "))))
;; See CLJS-2453
#_(defcheck solution-95cbc018
  #(vec
     (sort
       (fn [x y] (compare (.toLowerCase x) (.toLowerCase y)))
       (re-seq #"\w+" %))))

;; See CLJS-2453
#_(defcheck solution-96ccfc
  (fn [s]
    (sort-by clojure.string/upper-case (re-seq #"[A-Za-z]+" s))))

(defcheck solution-96edb34c
  (fn split' [s]
    (->> s
      (filter (complement #{\. \!}))
      (partition-by #(= \space %))
      (map #(apply str %))
      (filter #(not= " " %))
      (sort #(compare (clojure.string/lower-case %1)
               (clojure.string/lower-case %2))))))

;; See CLJS-2453
#_(defcheck solution-970e0ff7
  (fn __ [s]
    (->> (clojure.string/split s #"[^A-Za-z]+")
      (sort-by clojure.string/lower-case))))

;; See CLJS-2453
#_(defcheck solution-9810a795
  (fn [s]
    (sort #(compare (.toLowerCase %) (.toLowerCase %2)) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-986d6d94
  (fn [s]
    (let [words         (clojure.string/split s (re-pattern " "))
          compare-words (fn [a b] (compare (clojure.string/lower-case a) (clojure.string/lower-case b)))]
      (map #(clojure.string/replace % #"[^\w]" "") (sort compare-words words)))))

;; See CLJS-2453
#_(defcheck solution-987b94d
  (comp (partial sort-by #(.toLowerCase %))
        (partial re-seq #"\w+")))

;; See CLJS-2453
#_(defcheck solution-9967e2cd
  (fn [str] (->> str
              (re-seq #"\w+")
              (sort-by clojure.string/upper-case))))

(defcheck solution-99876ff1
  (fn [str] (let
             [f #(.toUpperCase %)]
              (sort #(compare (f %1) (f %2))
                (clojure.string/split str #"\W"))
              )))

(defcheck solution-99b8798c
  (fn [s] (sort (fn [x y] (compare (.toLowerCase x) (.toLowerCase y))) (clojure.string/split s #"\W"))))

;; See CLJS-2453
#_(defcheck solution-99df7b18
  (fn [in] (sort-by #(.toLowerCase %) (re-seq #"\w+" in))))

;; See CLJS-2453
#_(defcheck solution-9a1ec1ff
  (fn [s]
    (->>
      s
      (re-seq #"\w+")
      (map #(vector (clojure.string/lower-case %) %))
      sort
      (map #(second %))
      )))

;; See CLJS-2453
#_(defcheck solution-9ab6d0a0
  #(sort-by clojure.string/lower-case
     (re-seq #"\w+" %)))

;; See CLJS-2453
#_(defcheck solution-9b728dd9
  (fn srt [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-9b8831a5
  (fn [s]
    (sort-by #(.toUpperCase %)
      (clojure.string/split (apply str (remove #{\. \!} s)) #" "))))

(defcheck solution-9cca74cd
  (fn f [s]
    (sort-by
      clojure.string/lower-case
      (clojure.string/split s #"\W"))))

(defcheck solution-9ce45b2b
  #(->> % (re-seq #"\w+")
     (sort-by clojure.string/lower-case)))

(defcheck solution-9ceb9df7
  (fn [s]
    (->> (re-seq #"\w+" s)
      (sort #(compare (.toLowerCase %1)
               (.toLowerCase %2))))))

(defcheck solution-9d43a94b
  (fn [ostr]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"\w+" ostr))))

(defcheck solution-9dfec720
  (fn [s]
    (->>
      (-> s
        (clojure.string/replace #"[^a-zA-Z ]" "")
        (clojure.string/split #" "))
      (sort-by clojure.string/lower-case))))

(defcheck solution-9e1a365e
  #(sort-by
     clojure.string/lower-case
     (clojure.string/split % #"[^\w]")))

(defcheck solution-9e39baa7
  (fn mysort [s]
    (->> (clojure.string/split s #" ")
      (map #(clojure.string/replace % #"[^a-zA-Z]" ""))
      (sort-by clojure.string/lower-case))))

(defcheck solution-a039eb9a
  (fn [s] (sort-by #(.toLowerCase %) (re-seq #"(?i)[a-z]+" s))))

(defcheck solution-a03ee13e
  #(sort-by clojure.string/lower-case (clojure.string/split % #"\W")))

(defcheck solution-a0613bc4
  (fn [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))
    ))

(defcheck solution-a19a6624
  (fn [words]
    (->> (clojure.string/split words #" ")
      (map #(clojure.string/replace % #"[,.!]" ""))
      (sort-by clojure.string/lower-case))))

(defcheck solution-a278a2d9
  (fn [s]
    (map :orig (sort-by :lower (map
                                 (fn [x] {:orig x :lower (.toLowerCase x)})
                                 (re-seq #"\w+" s))))))

(defcheck solution-a27ea793
  (fn [i] (sort #(compare (clojure.string/lower-case %) (clojure.string/lower-case %2)) (clojure.string/split i #"[^a-zA-Z]"))))

(defcheck solution-a28f4389
  #(sort-by clojure.string/lower-case
     (clojure.string/split
       (apply str (remove (fn [c] (#{\. \!} c)) %))
       #"\s")))

(defcheck solution-a2a0494e
  (fn [s]
    (->> s
      (re-seq #"\w+")
      (sort-by #(.toLowerCase %)))))

(defcheck solution-a3058f36
  (fn [s]
    (sort
      #(compare (.toLowerCase %) (.toLowerCase %2))
      (map
        #(clojure.string/replace % #"[.!]" "")
        (clojure.string/split s #" ")))))

(defcheck solution-a36657fe
  #(->> %
     (re-seq #"\w+")
     (sort-by clojure.string/lower-case)))

(defcheck solution-a37b31e8
  #(sort-by (fn [a] (.toLowerCase a)) (clojure.string/split % #"\W")))

(defcheck solution-a3c87a57
  (fn split-sort [mystr]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
      (re-seq #"[a-zA-Z]+" mystr))))

(defcheck solution-a3e5662c
  (fn [a] (sort-by clojure.string/upper-case
            (clojure.string/split (subs
                                    a 0
                                    (dec (count a))) #" "))))

(defcheck solution-a41760af
  (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split s #"[ \. !]"))))

(defcheck solution-a4d55f5a
  (fn
    [s]
    (->> (clojure.string/split s #"\s+|[\.!]")
      (sort #(compare (clojure.string/lower-case %)
               (clojure.string/lower-case %2))))))

(defcheck solution-a5048b86
  (fn sort-words [s]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (map #(clojure.string/replace % #"[\.!]" "") (clojure.string/split s #"\s"))
      )))

(defcheck solution-a5ac08b
  #(sort-by clojure.string/lower-case (clojure.string/split (re-find #"[a-zA-Z ]+" %) #"\s+")))

(defcheck solution-a666371b
  (fn [s]
    (->> (clojure.string/split s #"[^A-Za-z]")
      (sort-by clojure.string/lower-case))))

(defcheck solution-a6e39bed
  (fn [str]
    (letfn [(words [str] (re-seq #"\w+" str))
            (sorted-words [lst] (sort-by clojure.string/lower-case lst))]
      (sorted-words (words str)))))

(defcheck solution-a739a4f6
  (fn [str] (sort-by #(.toLowerCase %) (re-seq #"\w+" str))))

(defcheck solution-a78ebba2
  (fn [s]
    (sort #(apply compare (map clojure.string/lower-case [%1 %2]))
      (clojure.string/split s #"[\s,.!;:]+"))))

(defcheck solution-a7951879
  (fn [s]
    (let [words (clojure.string/split s #"\W")]
      (sort-by #(.toLowerCase %) words))))

(defcheck solution-a79d9f9
  (fn word-sorting
    [x]
    (sort #(compare
             (clojure.string/lower-case %1)
             (clojure.string/lower-case %2)) (re-seq #"\w+" x))))

(defcheck solution-a7a8af8c
  (fn split-and-sort [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"[^A-Za-z]"))
    ))

(defcheck solution-a808e941
  (fn [s] (sort-by clojure.string/lower-case
            (re-seq #"\w+" s))))

(defcheck solution-a835cb8b
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-a83e53c1
  (fn [s]
    (let [words (clojure.string/split s #"[\s\W]+")]
      (sort-by #(clojure.string/capitalize %) words))))

(defcheck solution-a8e0d8b3
  (fn [string]
    (let [srt

          (fn [a, b]
            (compare
              (.toLowerCase a)
              (.toLowerCase b)))]

      (sort srt
        (re-seq #"\w+" string)))))

(defcheck solution-a901a5c1
  (fn [s]
    (sort-by #(.toLowerCase %) (clojure.string/split s #"[^a-zA-Z]"))))

(defcheck solution-a9366df7
  (fn [word]
    (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
      (filter #(re-matches #"[a-zA-Z]+" %) (clojure.string/split word #"\b"))
      )

    ))



(defcheck solution-aa1bb667
  (fn [t] (->> (re-seq #"\w+" t)
            (sort-by #(.toLowerCase %)))))

(defcheck solution-aa58e437
  (fn [s]
    (-> s
      (clojure.string/replace #"[^\w\s]*" "")
      (clojure.string/split #"\s")
      (->>
        (sort-by clojure.string/lower-case)))))

(defcheck solution-aa605a28
  (fn wsort [text]
    (->> text
      (re-seq #"\w+")
      (sort #(compare (.toLowerCase %1) (.toLowerCase %2))))))

(defcheck solution-ab24989e
  (fn pr070 [s]
    (sort (fn [x y] (compare (clojure.string/capitalize x) (clojure.string/capitalize y))) (clojure.string/split s #"[^\w+]"))))

(defcheck solution-ab47fd8b
  (fn [s] (sort-by #(clojure.string/lower-case %) (re-seq #"[A-z]+" s))))

(defcheck solution-ab698cc9
  (fn [st] (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2)) (clojure.string/split st #"[^A-Za-z]"))))

(defcheck solution-abee0f13
  (fn [sentence]
    (let [words (clojure.string/split sentence #" ")]
      (sort-by #(.toUpperCase %)
        (map #(clojure.string/replace % #"\W" "") words)))))

(defcheck solution-ac322ee7
  (fn [sentence]
    (->> sentence
      (re-seq #"\w+")
      (sort-by #(.toLowerCase %)))))

(defcheck solution-ac3ba979
  (fn [s]
    (sort
      #(compare (clojure.string/lower-case %) (clojure.string/lower-case %2))
      (re-seq #"\w+" s))))

(defcheck solution-acb73d22
  (fn sort-words
    [xs]
    (-> xs
      (clojure.string/replace #"[^\s|\w]" "")
      (clojure.string/split #"\s")
      ((partial sort-by clojure.string/lower-case)))))

(defcheck solution-ae5cf42f
  #(vec (sort
          (fn [a b]
            (loop [i 0]
              (if (and (< i (count a)) (< i (count b)))
                (let [r (compare (nth (clojure.string/lower-case a) i) (nth (clojure.string/lower-case b) i))]
                  (if (= r 0)
                    (recur (inc i))
                    r
                    )
                  )
                (compare (count a) (count b))
                )
              ))
          (clojure.string/split % #"[^a-zA-Z0-9]")
          )))

(defcheck solution-aea7c5ba
  (fn [x] (sort-by #(.toLowerCase %) (re-seq #"\w+" x))))

(defcheck solution-aeca3828
  (fn [s]
    (sort-by #(.toUpperCase %) (clojure.string/split (clojure.string/replace s #"[!\.\?,]" "") #"\s"))))

(defcheck solution-af79431
  (fn [entrada] (let [ss (clojure.string/split (->> (clojure.string/lower-case entrada) drop-last (clojure.string/join "")) #" ")
                      m  {(first ss) (clojure.string/capitalize (first ss))}]

                  (replace m (sort ss))

                  )))

(defcheck solution-afe5041a
  (fn [s]
    (sort-by clojure.string/upper-case
      (map #(clojure.string/replace % #"[^A-Za-z]" "")
        (clojure.string/split s #" ")))))

(defcheck solution-b042b96e
  (fn word-sorting [s]
    (let [slist (clojure.string/split (clojure.string/replace s #"[.,;:!?]" "") #"\s+")]
      (let [slist2 (reduce #(conj %1 [(clojure.string/lower-case %2) %2]) [] slist)]
        (reduce #(conj %1 (last %2)) [] (sort #(compare (first %1) (first %2)) slist2))
        ))))

(defcheck solution-b0471e82
  (fn [s] (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"\w+" s))))

(defcheck solution-b0710af7
  (fn [s] (let [words (map #(clojure.string/replace % #"\W+" "")
                        (clojure.string/split s #"\s+")) words-hash (into {} (for [w words]
                                                                               [(clojure.string/lower-case w) w]))] (for [sw (sort (keys words-hash))]
                                                                                                                      (words-hash sw)))))

(defcheck solution-b0b9291
  #(->> (clojure.string/split % #"\W+")
     (sort-by clojure.string/lower-case)))

(defcheck solution-b1d1b19e
  (fn [string]
    (sort-by clojure.string/lower-case (re-seq #"\w+" string))))

(defcheck solution-b1efa37b
  (fn [s]
    (into []
      (sort-by clojure.string/lower-case
        (clojure.string/split (apply str
                                (take (dec (count s)) s))
          #"\s")))))

(defcheck solution-b246c75a
  (fn [string]
    (sort-by (fn [word] (.toLowerCase word)) (re-seq #"[a-zA-Z]+" string))))

(defcheck solution-b2819be7
  #(sort-by (memfn toUpperCase) (re-seq #"\w+" %)))

(defcheck solution-b29c99bf
  (fn [words]
    (->> (clojure.string/split words #"[\s\.\!]+")
      (sort-by clojure.string/lower-case))))

(defcheck solution-b2fe1c50
  #(sort-by clojure.string/lower-case
     (clojure.string/split % #"\W+")))

(defcheck solution-b3014c63
  #(sort-by (fn [s] (.toUpperCase s)) (re-seq #"\w+" %)))

(defcheck solution-b30f8f39
  (fn word-sort [s]
    (sort-by #(.toUpperCase %) (re-seq #"\w+" s))))

(defcheck solution-b3d48d7a
  (fn [s]
    (sort-by (memfn toLowerCase) (re-seq #"\w+" s))))

(defcheck solution-b3ebb809
  #(sort-by clojure.string/lower-case (re-seq #"[A-z]+" %)))

(defcheck solution-b3fce9ed
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-b494eb34
  (fn [s]
    (sort-by
      #(clojure.string/lower-case %)
      (clojure.string/split
        (clojure.string/replace s #"[^a-zA-Z ]" "")
        #" "))))

(defcheck solution-b4b31a55
  (fn [s] (sort-by clojure.string/lower-case
            (clojure.string/split
              (clojure.string/replace s #"[!.,;:\"']" "") #" "))))

(defcheck solution-b4de770f
  (fn [s]
    (->> (re-seq #"\w+" s)
      (sort-by #(.toUpperCase %)))))

(defcheck solution-b579d42
  (fn [s]
    (sort-by clojure.string/lower-case
      (re-seq #"\w+" s))))

(defcheck solution-b5b87ca4
  (fn sort-words
    [sent]
    (sort-by clojure.string/lower-case
      (clojure.string/split
        (clojure.string/replace
          sent
          #"[^a-zA-z ]"
          "")
        #" ")
      )
    ))

(defcheck solution-b6063b3
  (fn [s]
    (sort-by
      #(.toLowerCase %)
      (clojure.string/split
        s
        #"[^a-zA-Z]+"))))

(defcheck solution-b62430d6
  (fn [input-str]
    (sort #(compare (clojure.string/upper-case %1)
             (clojure.string/upper-case %2))
      (clojure.string/split input-str #"[ .!]"))))

(defcheck solution-b653476c
  (fn [x] (sort-by clojure.string/upper-case (re-seq #"\w+" x))))

(defcheck solution-b6762916
  (fn [s]
    (let [re #"\w+"]
      (->> s
        (re-seq re)
        (sort-by clojure.string/lower-case)))))

(defcheck solution-b6fdc18d
  #(sort-by (fn [k] (.toLowerCase k)) (re-seq #"\w+" %)))

(defcheck solution-b7ce477
  (fn [x] (sort-by clojure.string/lower-case (re-seq #"[A-Za-z]+" x))))

(defcheck solution-b8677196
  (comp (partial sort-by clojure.string/lower-case)
        #(clojure.string/split % #"\W")))

(defcheck solution-b86e5f2a
  (fn [s]
    (->> (clojure.string/split s #" ")
      (map #(clojure.string/replace % #"\W" ""))
      (sort-by #(.toLowerCase %)))))

(defcheck solution-b871cda
  (fn [s]
    (into [] (sort-by clojure.string/lower-case (clojure.string/split s #"[\W]")))))

(defcheck solution-b8ba89dd
  (fn [s]
    (->> (clojure.string/split (clojure.string/replace s #"[.,!]" "") #" ")
      (sort-by #(.toUpperCase %)))))

;; See CLJS-2453
#_(defcheck solution-b956db0d
    (fn [s]
      (let [words (filter not-empty (re-seq #"\w*" s))]
        (sort (fn [s1 s2] (compare (.toLowerCase s1) (.toLowerCase s2))) words))))

(defcheck solution-b977cac5
  (fn [x]
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (re-seq #"\w+" x))))

(defcheck solution-b9c1454e
  #(sort-by clojure.string/lower-case (clojure.string/split % #"[^a-zA-Z]")))

(defcheck solution-baf378d
  (fn [sent]
    (->> sent
      (re-seq #"\w+")
      (sort-by clojure.string/lower-case))))

(defcheck solution-bb0436e4
  (fn [s]
    (sort (fn [x y] (compare (clojure.string/lower-case x)
                      (clojure.string/lower-case y)))
      (clojure.string/split s #"[^a-zA-Z]"))))

(defcheck solution-bb2b45e2
  (fn [s] (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
            (re-seq #"\w+" s))))

(defcheck solution-bbcf4e93
  (fn [a] (sort-by #(.toLowerCase %) (re-seq #"[a-zA-Z]+" a))))

(defcheck solution-bcab5e68
  #(sort-by clojure.string/upper-case (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-bceb6f1b
  (fn [s]
    (->> s
      (re-seq #"\w+")
      (sort-by #(.toLowerCase %)))))

(defcheck solution-be3d72f7
  (fn [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-bf5097ed
  (fn [s]
    (sort
      #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
      (clojure.string/split (clojure.string/replace s #"[^\w ]" "") #" "))))

(defcheck solution-bf74a5c8
  (fn word-sort [s]
    (let [re (re-seq #"[a-zA-Z]+" s)]
      (sort-by #(.toUpperCase %) re))))

(defcheck solution-bf7fefc
  (fn f70 [s] (sort-by clojure.string/lower-case (clojure.string/split s #"\s|!|\."))))

(defcheck solution-c03ebaff
  #(-> %
     drop-last
     clojure.string/join
     (clojure.string/split #" ")
     ((partial sort-by clojure.string/lower-case))))

(defcheck solution-c06d7d5f
  (fn sort-words [s]
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (filter (complement empty?) (clojure.string/split s #"[^a-zA-Z]")))))

(defcheck solution-c0e64237
  (fn [x] (sort-by #(.toUpperCase %)
            (clojure.string/split x #"[\s\\\.!]"))))

(defcheck solution-c1b510b0
  (fn [s] (sort-by clojure.string/lower-case
            (clojure.string/split s #"\W+"))))

(defcheck solution-c2183d2d
  (fn [arg] (map (fn [x] (apply str
                           (filter #(not (contains? #{\! \. \,} %))
                             x)))
              (#(sort (fn [s1 s2]
                        (compare (.toLowerCase s1) (.toLowerCase s2)))
                  (clojure.string/split % #"\s+")) arg))))

(defcheck solution-c23874d6
  (fn [s] (sort-by #(clojure.string/lower-case %) (clojure.string/split s #"\W"))))

(defcheck solution-c24d6d60
  (fn [s]
    (reduce
      (fn [v w]
        (let
         [[a b] (split-with #(not (pos? (compare (.toUpperCase %) (.toUpperCase w)))) v)]
          (into (conj (vec a) w) b)))
      []
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-c38db1a5
  (fn [s]
    (let [words (clojure.string/split s #"\W+")]
      (sort-by #(clojure.string/capitalize %) words))))

(defcheck solution-c3ab81f0
  (fn [s] (sort-by #(clojure.string/lower-case %)
            (-> s
              (clojure.string/replace #"[^A-Za-z ]" "")
              (clojure.string/split #" ")))))

(defcheck solution-c41bdb26
  #(sort-by clojure.string/lower-case
     (re-seq #"\w+" %)))

(defcheck solution-c47b7247
  (fn [x] ((comp vec sort-by) #(clojure.string/lower-case %) (map #(clojure.string/replace % #"[\.!]" "") (clojure.string/split x #" ")))))

(defcheck solution-c51fe87c
  #(sort-by
     clojure.string/lower-case
     (clojure.string/split
       %
       #"[^a-zA-Z]+")))

(defcheck solution-c520744a
  (fn [s]
    (sort
      (fn [& args]
        (apply compare (map clojure.string/lower-case args)))
      (clojure.string/split
        (clojure.string/replace s #"[\.\!]" " ")
        #"\s+"))))

(defcheck solution-c5e95f1a
  (fn sort-words- [s]
    "70. Write a function which splits a sentence up into a sorted list of words."
    (->> (re-seq #"\w+" s)
      (map #(list (.toLowerCase %) %))
      (sort #(compare (first %1) (first %2)))
      (map second))))

(defcheck solution-c6b45482
  #(let [m (re-seq #"\w+" %)]
     (sort (fn [x y] (compare (clojure.string/lower-case x)
                       (clojure.string/lower-case y)))

       m)))

(defcheck solution-c7308196
  #(sort-by (fn [v] (clojure.string/lower-case v)) (clojure.string/split % #"[^A-Za-z]")))

(defcheck solution-c734bd6b
  #(sort-by clojure.string/lower-case compare (re-seq #"\w+" %)))

(defcheck solution-c7358d8d
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split
        (clojure.string/replace s #"[.,;:!?\"']" "")
        #"\s+"))))

(defcheck solution-c84cdf48
  (fn [s]
    (sort (fn [v1 v2]
            (compare (clojure.string/upper-case v1)
              (clojure.string/upper-case v2)))
      (clojure.string/split s #"[^a-zA-Z]+"))))

(defcheck solution-c859f775
  (fn [s]
    (sort-by clojure.string/lower-case
      (re-seq #"\w+" s)
      )
    ))

(defcheck solution-c8e0b38c
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split s #"\W"))))

(defcheck solution-c94ddc33
  #(sort-by (fn [v] (.toLowerCase v)) (re-seq #"\w+" %)))

(defcheck solution-c99cae70
  (fn [src]
    (sort #(compare (clojure.string/lower-case %) (clojure.string/lower-case %2)) (clojure.string/split src #"[\s\W]"))

    ))

(defcheck solution-caa40e9f
  (fn [s]
    (letfn [(strlt [a b]
              (< (compare
                   (clojure.string/lower-case a)
                   (clojure.string/lower-case b))
                0))]
      (seq
        (apply sorted-set-by strlt
          (re-seq #"\w+" s))))))

(defcheck solution-caed7f09
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace s #"[^\w\s]" "") #" "))))

(defcheck solution-cb43b02
  #(sort-by clojure.string/lower-case (re-seq #"(?i)[a-z]+" %)))

(defcheck solution-cb5dba62
  #(vals (into (sorted-map)
           (zipmap (re-seq #"\w+" (.toUpperCase %))
             (re-seq #"\w+" %)))))

(defcheck solution-cd07ed52
  (fn [s]
    (sort-by (fn [s] (clojure.string/lower-case s))
      (map (fn [s]
             (clojure.string/replace s #"[^a-zA-Z]+" ""))
        (clojure.string/split s #" ")))))

(defcheck solution-cd3deae9
  (fn [s] (sort #(compare (clojure.string/lower-case %1)
                   (clojure.string/lower-case %2))
            (clojure.string/split s #"[ \.\!]"))))

(defcheck solution-cd751ec4
  #(sort-by (fn [x] (clojure.string/lower-case x)) (clojure.string/split % #"[ !.]+")))

(defcheck solution-cdf8b39a
  (fn sortStr [x] (letfn [(dropPunc [x] (clojure.string/replace x #"[^\w\s]" ""))
                          ]
                    (sort #(compare (clojure.string/capitalize %1)
                             (clojure.string/capitalize %2))
                      (clojure.string/split (dropPunc x) #" ")))))

(defcheck solution-cf521539
  (fn sort-words
    [string]
    (sort-by (memfn toLowerCase) (re-seq #"\w+" string))))

(defcheck solution-cf5e78eb
  #(sort-by (fn [word] (clojure.string/lower-case word))
     (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-cf81e8a2
  (fn [e]
    (letfn [(splitonspace [a] (clojure.string/split (clojure.string/triml a) #"\s+"))
            (removepunc [b] (clojure.string/replace b #"[.!?,]" " "))
            (orderstrs [c d] (compare (clojure.string/lower-case c) (clojure.string/lower-case d)))]
      (sort orderstrs (splitonspace (removepunc e))))))

(defcheck solution-cfbd38ef
  (fn [coll] (sort-by #(.toUpperCase %) (filter #(> (count %) 0) (clojure.string/split coll #"[\s,.!?]")))))

(defcheck solution-d001e8d
  (fn [s]
    (let [words (re-seq #"\w+" s)
          lc    clojure.string/lower-case]
      (sort #(compare (lc %1) (lc %2)) words))))

(defcheck solution-d03693e
  (fn [x]
    (sort-by #(.toLowerCase %) (clojure.string/split (clojure.string/replace x #"\.|!" "") #" "))))

(defcheck solution-d075e34e
  (fn [str] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split str #"\W+"))))

(defcheck solution-d155c494
  #(sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace % #"[^a-zA-Z0-9\s]+" "") #"\s+")))

(defcheck solution-d16e91d4
  (fn [sentence]
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (clojure.string/split sentence #"\W"))))

(defcheck solution-d2b09879
  #(sort-by clojure.string/lower-case (re-seq #"[^\s.!]+" %)))

(defcheck solution-d2b4a944
  #(sort-by (memfn toLowerCase) (re-seq #"\w+" %)))

(defcheck solution-d2f151bc
  #(sort-by
     (fn [a] (.toUpperCase a))
     (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-d3035b73
  (fn [s] (sort-by (fn [x] (.toLowerCase x)) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-d3242372
  #(sort-by (fn [x] (.toLowerCase x)) (re-seq #"\w+" %)))

(defcheck solution-d397b817
  (fn [s]
    (let [f #(apply compare (map clojure.string/lower-case %&))]
      (sort f
        (clojure.string/split (clojure.string/replace s #"[.!?]" "")
          #" ")))))

(defcheck solution-d3ca2970
  (fn [string] (->> string
                 (#(clojure.string/split % #"\s+"))
                 (map (fn [word] (re-find #"\w+" word)))
                 (sort-by #(clojure.string/lower-case %)))))

(defcheck solution-d4991894
  (fn [s]
    (let [dealed
          (clojure.string/split (clojure.string/replace s #"[^A-Za-z ]" "") #" ")
          mysort
          (fn [coll]
            (loop [step 0 r coll]
              (if
               (= (dec (count coll)) step) r
                                           (recur (inc step) (concat (first (split-at step r))
                                                                     (#(if (< 0 (compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))) [%2 %1] [%1 %2]) (nth r step) (nth r (inc step)))
                                                                     (last (split-at (+ 2 step) r)))))))]
      (loop [f true r dealed]
        (if (nil? f) r
                     (recur (some #(< 0 (apply compare %)) (partition 2 1 (map clojure.string/lower-case r)))
                       (mysort r)))))))

(defcheck solution-d5a02b55
  #(sort-by
     clojure.string/lower-case
     (re-seq #"\w+" %)))

(defcheck solution-d624b522
  (fn [p]
    (-> p
      (clojure.string/replace #"[.,!]" "")
      (clojure.string/split #"\s+")
      ((fn [w] (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) w))))))

(defcheck solution-d6b58e11
  (fn [x] (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2)) (map #(-> % (clojure.string/replace #"!" "") (clojure.string/replace #"\." "")) (clojure.string/split x #" ")))))

(defcheck solution-d715c70
  #(sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace % #"[^a-z,^A-Z]" " ") #"\s+")))

(defcheck solution-d74b9874
  (fn [s]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-d86c2268
  (fn wordComp [x]
    (into '[] (apply sorted-set-by
                (fn [x y] (compare (clojure.string/lower-case x) (clojure.string/lower-case y)))
                (clojure.string/split
                  (clojure.string/replace x #"[!.;:,]" "")
                  #"\s+")))))

(defcheck solution-d8c2cff7
  (fn [s]
    (sort-by
      #(.toLowerCase %)
      (re-seq #"\w+" s))))

(defcheck solution-d9255a32
  (fn fun [coll]
    (let [s (re-seq #"[a-zA-Z]+" coll)]
      (sort-by clojure.string/lower-case s))))

(defcheck solution-d92f1ca3
  #(sort-by (fn [s] (.toLowerCase s)) (re-seq #"\w+" %)))

(defcheck solution-d9848cb6
  #(sort-by clojure.string/lower-case (re-seq #"[a-zA-Z]+" %)))

(defcheck solution-d9d83a3b
  (fn [words] (sort
                (fn [x y]
                  (compare (clojure.string/lower-case x) (clojure.string/lower-case y))
                  )
                (re-seq #"[A-Za-z]+" words)
                )
    ))

(defcheck solution-da242af6
  (fn [s]
    (sort-by
      #(.toLowerCase %)
      (clojure.string/split
        (clojure.string/replace s #"\.|!" "") #" "))))

(defcheck solution-da5eaa5b
  (fn ws [s]
    (let [words (clojure.string/split s #"[\s.!,]+")]
      (sort-by clojure.string/lower-case words))))

(defcheck solution-daae52d3
  #(sort-by clojure.string/lower-case
     (re-seq #"\w+" %)))

(defcheck solution-db0590cc
  #(sort-by (fn [s] (.toLowerCase s)) (re-seq #"\w+" %)))

(defcheck solution-db7a0d44
  #(sort-by (memfn toLowerCase) (re-seq #"[A-Za-z]+" %)))

(defcheck solution-dccdecce
  #(sort-by clojure.string/upper-case (clojure.string/split % #"[ \.!]")))

(defcheck solution-dd25fe9d
  (fn word-sorting [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"[ .,!;]"))))

(defcheck solution-dd7c2d6b
  (fn [args]
    (sort #(compare (clojure.string/lower-case %1)
             (clojure.string/lower-case %2))
      (flatten (map #(clojure.string/split % #"\W")
                 (clojure.string/split args #" "))))))

(defcheck solution-ddf5a3ab
  (fn [s]
    (sort
      #(compare (.toLowerCase %) (.toLowerCase %2))
      (clojure.string/split s #"\W"))))

(defcheck solution-dee23267
  (fn [s]
    (let [v (clojure.string/split s #"\W")]
      (sort (fn [s1 s2]
              (loop [remaining1 (sequence (clojure.string/lower-case s1)) remaining2 (sequence (clojure.string/lower-case s2))]
                (if (empty? remaining1)
                  -1
                  (if (empty? remaining2)
                    1
                    (if (= 0 (compare (first remaining1) (first remaining2)))
                      (recur (rest remaining1) (rest remaining2))
                      (compare (first remaining1) (first remaining2))))))) v))))

(defcheck solution-def77280
  (fn [st] (sort
             #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
             (clojure.string/split (clojure.string/replace st #"[.,!]" "") #"\s"))))

(defcheck solution-df032048
  (fn [s]
    (sort-by clojure.string/lower-case
      (clojure.string/split s #"\W+"))))

(defcheck solution-df0b28fc
  (fn [sentence]
    (let [words (clojure.string/split sentence #"\W")]
      (sort-by clojure.string/lower-case words))))

(defcheck solution-df55f4b8
  (fn [s]
    (sort-by clojure.string/lower-case (clojure.string/split (clojure.string/replace s #"\.|!" "") #" "))))

(defcheck solution-dfec3dd0
  (fn [s] (sort-by #(.toLowerCase %)
            (re-seq #"\w+" s))))

(defcheck solution-e042fa6d
  (comp (partial sort-by #(.toLowerCase %)) (partial re-seq #"[A-Za-z]+")))

(defcheck solution-e0b95d7
  (comp (partial sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))) #(re-seq #"\w+" %)))

(defcheck solution-e0d46f70
  (fn [s] (vec (sort-by clojure.string/lower-case (clojure.string/split s #"[^A-Za-z]")))))

(defcheck solution-e162767a
  (fn [s]
    (sort #(compare (clojure.string/upper-case %1) (clojure.string/upper-case %2))
      (clojure.string/split
        (clojure.string/replace s, #"[!.]", "")
        #"\s+"))))

(defcheck solution-e23d5b13
  (fn [s]
    (sort #(< (compare (clojure.string/upper-case %1)
                (clojure.string/upper-case %2))
             0)
      (->
        s
        (clojure.string/replace #"^[^a-zA-Z]+" "")
        (clojure.string/replace #"[^a-zA-Z]+$" "")
        (clojure.string/split #"[^a-zA-Z]+")))))

(defcheck solution-e29e7fbb
  (fn [s]
    (->>
      s
      (re-seq #"\w+")
      (sort-by #(.toLowerCase %))
      )))

(defcheck solution-e2cb7007
  (comp (partial sort-by clojure.string/upper-case) (partial re-seq #"\w+")))

(defcheck solution-e31649f2
  (fn [s] (sort-by #(.toLowerCase %) (clojure.string/split (clojure.string/replace s #"[\.!]" "") #"\s"))))

(defcheck solution-e3713d53
  #(sort-by clojure.string/lower-case (clojure.string/split % #"[ .!]")))

(defcheck solution-e376b236
  (fn wsort
    [string]
    (let [fil (fn [c] (and (not= c \space)
                           (not= c \.)
                           (not= c \!)))]
      (filter #(fil (first %)) (sort-by #(.toLowerCase %) (map (partial apply str) (partition-by fil string)))))))

(defcheck solution-e4780bc9
  #(sort-by clojure.string/lower-case
     (re-seq #"\w+" %)))

(defcheck solution-e4b0d141
  (fn word-sort
    [s]
    (sort-by #(clojure.string/lower-case %) (clojure.string/split s #"[^a-zA-z0-9]"))))

(defcheck solution-e4b9973
  (fn [s]
    (sort-by
      #(.toLowerCase %)
      (clojure.string/split s #"[ \.\!]"))))

(defcheck solution-e5631078
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-e614174
  (fn [x] (sort-by #(clojure.string/lower-case %) (clojure.string/split x #"\W"))))

(defcheck solution-e6396ee3
  #(sort-by clojure.string/upper-case (re-seq #"\w+" %)))

(defcheck solution-e6615f90
  (fn [s]
    (sort (comparator #(> 0 (compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)))) (map #(re-find #"[a-zA-z]+" %) (clojure.string/split s #" ")))))

(defcheck solution-e666df81
  (fn ws [s]
    (let [words (clojure.string/split
                  (clojure.string/replace s #"[^A-Za-z ]" "")
                  #" ")]
      (sort-by clojure.string/upper-case words))))

(defcheck solution-e6e8e89e
  (fn split-sort [s] (vals (sort (zipmap (clojure.string/split (.toUpperCase (clojure.string/replace s #"[^\w\s]" "")) #"\s+") (clojure.string/split (clojure.string/replace s #"[^\w\s]" "") #"\s+"))))))

(defcheck solution-e6fee983
  #(seq (apply (partial sorted-set-by (fn [a b]
                                        (compare (clojure.string/lower-case a)
                                          (clojure.string/lower-case b))))
          (re-seq #"\b\w+\b" %))))

(defcheck solution-e7dfc325
  (fn [s]
    (sort
      #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
      (clojure.string/split s #"[^a-zA-Z]"))))

(defcheck solution-e7f50d47
  #(->> % (re-seq #"\w+") (sort-by clojure.string/lower-case)))

(defcheck solution-e81085d2
  (fn [s]
    (sort #(compare (clojure.string/upper-case %) (clojure.string/upper-case %2))
      (clojure.string/split s #"[^a-zA-Z]"))))

(defcheck solution-e829e289
  (fn [s]
    (->> (clojure.string/split s #"\W+")
      (sort-by clojure.string/lower-case))))

(defcheck solution-e8391731
  (fn [s] (->> (re-seq #"[a-zA-Z]+" s) (sort-by clojure.string/lower-case))))

(defcheck solution-e849ab3b
  (fn word-sort [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-e94a3bb2
  (fn my-sort [s]
    (let [coll (re-seq #"\w+" s)]
      (sort-by #(clojure.string/lower-case %) coll))))

;; See CLJS-2453
#_(defcheck solution-e9dd5b19
    #(->> (re-seq #"[a-zA-Z]*" %)
       (remove (partial = ""))
       (sort-by clojure.string/upper-case)))

(defcheck solution-ea226afd
  (fn [s] (->>
            (clojure.string/split s #"\W+")
            (sort-by #(.toLowerCase %)))))

(defcheck solution-ea59378a
  (fn [xx]
    (sort
      (fn [a b] (compare (clojure.string/lower-case a) (clojure.string/lower-case b)))
      (clojure.string/split (clojure.string/replace xx #"[^a-zA-Z ]" "") #" "))))

(defcheck solution-eada1c96
  (fn [x]
    (sort-by clojure.string/lower-case
      (re-seq #"[A-Za-z]+" x))))

(defcheck solution-eb3cad1c
  #(sort-by
     (fn [d]
       (.toLowerCase d))
     (re-seq #"[A-Za-z]+" %)))

;; See CLJS-2453
#_(defcheck solution-ec1afeb6
    (fn [t] (sort-by #(.toLowerCase %) (remove #(= % "") (re-seq #"\w*" t)))))

(defcheck solution-ec8ceff2
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-ecba5256
  (fn [s]
    (let [words  (re-seq #"\w+" s)
          lower  (map #(.toLowerCase %) words)
          pairs  (map vector lower words)
          sorted (sort pairs)]
      (map last sorted))))

(defcheck solution-ecf115fb
  (fn [s]
    (sort-by
      #(.toUpperCase %1)
      (re-seq #"\w+" s))))

(defcheck solution-ecf634a3
  (fn [str]
    (sort-by #(.toLowerCase %) (clojure.string/split str #"[^\w]"))))

(defcheck solution-ed4f0d8
  (fn [sentence]
    (sort-by clojure.string/lower-case (re-seq #"\w+" sentence))))

(defcheck solution-ee045945
  (fn word-sort [word-string]
    (sort #(compare (clojure.string/lower-case %)
             (clojure.string/lower-case %2))
      (clojure.string/split
        (clojure.string/replace word-string #"[\.\!\?]" "") #" "))))

(defcheck solution-ee25ef80
  (fn [s]
    (sort-by clojure.string/upper-case (re-seq #"\b\w+\b" s))))

(defcheck solution-ee3244fd
  (fn [s] (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
            (clojure.string/split
              (clojure.string/replace s #"[.,!?]" "")
              #" "
              ))
    ))

(defcheck solution-ef07b22f
  (fn word-sort [sentence]
    (into [] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split sentence #"\W")))))

(defcheck solution-ef1a615f
  (fn f ([s] (f s nil))
    ([s c]
     (if (empty? s)
       (sort-by #(.toLowerCase %) c)
       (let [w #(re-matches #"[a-zA-Z]" (str %))
             [a b] (split-with w s)]
         (recur (drop-while #(not (w %)) b)
           (conj c (apply str a))))))))

(defcheck solution-ef3189e0
  (fn [sentence]
    (sort-by #(.toLowerCase %1)
      (for
       [x (partition-by #(or (= %1 \space) (= %1 \!) (= %1 \.)) (str sentence))
        :when (not (or (= x '(\space)) (= x '(\!)) (= x '(\.))
                       ))]
        (apply str x))
      )))

(defcheck solution-eff4c5d
  (fn my-sort [sentence]
    (sort-by clojure.string/lower-case (clojure.string/split sentence #"\W"))))

(defcheck solution-f088c15b
  (fn [s] (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2))
            (clojure.string/split s #"[^a-zA-Z]+"))))

(defcheck solution-f0b58b6e
  #(vec (sort-by clojure.string/lower-case (re-seq #"\w+" %))))

(defcheck solution-f0dda01e
  (fn [s] (sort-by #(.toLowerCase %) (clojure.string/split s #"\W+"))))

(defcheck solution-f133be78
  (fn [s]
    (sort-by
      #(.toUpperCase %)
      (re-seq #"[a-zA-Z]+" s))))

(defcheck solution-f2b8f129
  (fn sort-word [words]
    (let [x (re-seq #"\w+" (clojure.string/lower-case words))
          y (re-seq #"\w+" words)]
      (map second (sort compare (zipmap x y))))))

(defcheck solution-f2bb606b
  (fn [x]
    (sort-by #(.toUpperCase %)
      (clojure.string/split x #"\W+"))))

(defcheck solution-f36c8f42
  (fn [c] (sort-by #(.toLowerCase %) (re-seq #"\w+" c))))

(defcheck solution-f3be698d
  (fn
    [s]
    (let [p (clojure.string/split s #"[ !.?,]")
          l (map clojure.string/lower-case p)
          t (zipmap l p)]
      (map #(get t %) (sort l)))))

(defcheck solution-f452b3b5
  (comp #(sort-by clojure.string/lower-case %) #(clojure.string/split % #"\s") #(clojure.string/replace % #"[^a-zA-Z\s]" "")))

(defcheck solution-f4fc7f7a
  (fn word-list [s]
    (sort-by #(clojure.string/upper-case %) (re-seq #"[A-Za-z]+" s))))

(defcheck solution-f5810355
  (fn word-sort [s]
    (sort-by clojure.string/lower-case (re-seq #"\w+" s))))

(defcheck solution-f5ca00e1
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"\w+" s))))

(defcheck solution-f65d0c38
  (fn [s]
    (sort-by
      clojure.string/lower-case
      (re-seq #"[a-zA-Z]+" s)
      )
    ))

(defcheck solution-f714ab0f
  #(sort-by clojure.string/upper-case (clojure.string/split % #"\W")))

(defcheck solution-f73fee9b
  #(sort-by (fn [s] (.toLowerCase s)) (re-seq #"\w+" %)))

(defcheck solution-f74f8121
  (fn [s]
    (sort-by
      #(.toUpperCase %)
      (re-seq #"\w+" s)
      )
    ))

(defcheck solution-f8951e97
  (fn [x] (sort-by clojure.string/lower-case (seq (clojure.string/split x #"[\s\W]+")))))

(defcheck solution-f9e4053d
  (fn [s] (->> (re-seq #"\w+" s) (sort-by clojure.string/lower-case))))

(defcheck solution-fa490dc8
  (fn word-sort
    [string]
    (sort-by #(.toLowerCase %)
      (clojure.string/split string #"[ .!]"))))

(defcheck solution-fae4c5ab
  (fn [_str]
    (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (re-seq #"[a-zA-Z]+" _str))
    ))

(defcheck solution-fb1ddad3
  (fn [s]
    (sort
      #(compare
         (.toUpperCase %1)
         (.toUpperCase %2))
      (clojure.string/split s #"[\s.!,]+"))))

(defcheck solution-fc2b59bb
  (fn [a] (sort-by #(.toUpperCase %) (re-seq #"\w+" a))))

(defcheck solution-fd6f4d33
  (fn [s]
    (sort-by #(.toUpperCase %)
      (re-seq #"\w+" s))))

(defcheck solution-fdc133e3
  #(sort-by clojure.string/lower-case
     (-> %
       (clojure.string/replace #"[.?!]" "")
       (clojure.string/split #" "))))

(defcheck solution-fe76bd41
  (fn [x] (sort-by clojure.string/upper-case (vec (clojure.string/split x #" |\.|!")))))

(defcheck solution-fee33c54
  #(sort-by clojure.string/lower-case (re-seq #"[a-zA-Z]+" %1)))

(defcheck solution-fef056e0
  (fn word-sort [text]
    (let [
          tokenized (clojure.string/split (clojure.string/replace text #"(\.)|(\!)|(\,)" "") #" ")
          ]
      (vals (into (sorted-map) (for [sentence tokenized] [(apply str (clojure.string/lower-case sentence)) sentence])))
      )
    ))

(defcheck solution-ff5b5e3
  #(sort-by (fn [w] (.toLowerCase w)) (re-seq #"\w+" %)))