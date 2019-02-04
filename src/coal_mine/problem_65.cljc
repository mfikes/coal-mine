(ns coal-mine.problem-65
  (:require [coal-mine.checks :refer [defcheck-65] :rename {defcheck-65 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10532ba3
  ;; weirdest problem ever?
  (fn [coll]
    (cond
      (= (empty coll) #{})
      :set

      (= (empty coll) {})
      :map

      :else
      (if (= (last (conj coll :hello :world)) :world)
        :vector
        :list))))

(defcheck solution-109d79bf
  #(let [mkey (gensym)
         test (conj % [mkey 0] [mkey 0] [mkey 1])]
     (cond (= (count test) (inc (count %))) :map
           (= (count test) (+ 2 (count %))) :set
           (= (take 2 test) [[mkey 1] [mkey 0]]) :list
           (= (take 2 (reverse test)) [[mkey 1] [mkey 0]]) :vector)))

(defcheck solution-10cca36b
  (fn solve [s]
    (condp = (empty s)
      {}  :map
      #{} :set
      ()  (if (reversible? s) :vector :list))))

(defcheck solution-10fc40e2
  (fn [e]
    (let [temp (empty e)]
      (cond
        (= temp {})  :map
        (= temp #{}) :set
        :else (if (reversible? temp) :vector :list)))))

(defcheck solution-112e451f
  #(cond (reversible? %) :vector
         (associative? %) :map
         (= (count %) (- (count (conj % :t :t)) 2)) :list
         :else :set))

(defcheck solution-11902c89
  (fn [xs]
    (let [xs (empty xs)]
      (cond
        (and (associative? xs)
             (= 2 ((conj xs [3 4] [1 2]) 1))) :map
        (= (conj xs 1 1) #{1}) :set
        (= (conj xs 1 2) [2 1]) :list
        (= (conj xs 1 2) [1 2]) :vector
        :else :unknown))))

(defcheck solution-11af5a97
  #(if (= (+ 1 (count %)) (count (conj (conj % {:new-key :new-val}) {:new-key :new-val})))
     (if (= :new-val (get (conj % {:new-key :new-val}) :new-key))
       :map
       :set)
     (if (= :new-val-2 (first (conj (conj % :new-val-1) :new-val-2)))
       :list
       :vector)))

(defcheck solution-122c12eb
  #(let [x {:a 1} y (conj % x)]
     (cond (= (get y x) x) :set
           (= (count y) (count (conj y x))) :map
           (= (last (conj y :a)) :a) :vector
           :else :list)))

(defcheck solution-123d6abc
  (fn my-type [arg]
    (let [k (gensym)
          v (gensym)
          test-elt [k v]
          modif (conj arg test-elt test-elt)
          modif-seq (seq modif)]
      (if (= (count (filter #(= test-elt %) modif-seq)) 1)
        (if (= (modif k) v) :map :set)
        (let [m-elt (gensym)
              nd-modif (conj modif m-elt)]
          (cond
            (= (first nd-modif) m-elt) :list
            (= (last nd-modif) m-elt) :vector))))))

(defcheck solution-12a3e7ff
  (fn [x]
    (cond
      (= :b (:a (conj x [:a :b]))) :map
      (= (conj x :a :a) (conj x :a)) :set
      (= (conj (conj x :a) :b) (concat x [:a :b])) :vector
      (= (conj (conj x :a) :b) (concat [:b :a] x)) :list)))

(defcheck solution-12c9ea9b
  (fn [c]
    (let [k (gensym)
          v1 (gensym) v2 (gensym)
          cc (into c [[k v1] [k v2]] )]
      (cond (= (get cc k) v2) :map
            (get cc [k v1])   :set
            (= [k v2] (first (vec cc))) :list
            (= [k v2] (last (vec cc)))  :vector
            :else :dunno))))

(defcheck solution-1306d7e5
  (fn [collection]
    (let [test-value [:test-value :test-value]
          result (conj (conj collection test-value) test-value)]
      (cond
        (:test-value result) :map
        (apply distinct? result) :set
        :else (let [result-2 (conj result :another-test-value)]
                (cond
                  (= (first result-2) :another-test-value) :list
                  (= (last result-2) :another-test-value) :vector))
        ))))

(defcheck solution-13182039
  #(cond (= % (conj % %)):map
         (= (conj % %) (conj % % %)) :set
         (= % (first (conj % (conj % %) %))) :list
         :else :vector))

(defcheck solution-13421d7d
  (fn [t] (if (= 0 (:x (into t {:x 0}))) :map  (if (= (+ 2 (count t)) (count (into t [0 0]))) (if (= 0 (first (conj t 1 0))) :list :vector) :set))))

(defcheck solution-13d6c102
  (fn [sq]
    (let [a (into sq [[-1 666]])]
      (if (= (get a -1) 666) :map
                             (if (= (conj sq 1) (conj sq 1 1)) :set
                                                               (if (= (first (conj sq 1 :test)) :test) :list :vector))))))

(defcheck solution-1408e1df
  (fn [x]
    (let [e (str (empty x))
          typemap {(str '()) :list,
                   (str [])  :vector,
                   (str {})  :map,
                   (str #{}) :set}]
      (get typemap e nil))))

(defcheck solution-14356e21
  (fn [x]
    (if (= 1 (:x (conj x [:x 1]))) :map
                                   (if (= (inc (count x)) (count (conj x [:x 1] [:x 1]))) :set
                                                                                          (if (= [:y 1] (first (conj x [:x 1] [:y 1]))) :list :vector)))))

(defcheck solution-1506b17d
  #(let [s (conj % [1 2])]
     (if (= 1 (count (flatten [s])))
       (if (associative? s)
         :map
         :set)
       (if (= (conj (conj s 3) 4) (concat s [3 4]))
         :vector
         :list))))

(defcheck solution-1564960e
  (fn [coll]
    (let [e (empty coll)]
      (if
       (= 2 (get (into e [[1 2]]) 1)) :map
                                      (let [x (conj e 1 1 2)]
                                        (cond
                                          (= 2 (count x)) :set
                                          (= 1 (first x)) :vector
                                          :else           :list))))))

(defcheck solution-1576ce50
  (fn [coll] (let [s (pr-str (empty coll))]
               (cond (= "()" s) :list
                     (= "[]" s) :vector
                     (= "{}" s) :map
                     :else :set))))

(defcheck solution-158f9fbb
  #({\{ :map \[ :vector \# :set} (first (str %)) :list))

(defcheck solution-158fb61b
  #(if (< 1 (count (flatten [(into % {:tic :toc})])))
     (if (= :toc (first (into % [:tic :toc]))) :list :vector)
     (if ((into % {:tic :toc}) :tic) :map :set)))

(defcheck solution-15c94d50
  (fn my-type
    [c]
    (if (associative? c)
      (if (= (count (assoc (conj (empty c) [:a 1]) 0 1)) 1) :vector :map)
      (if (= (count (conj (empty c) 1 1)) 1) :set :list))))

(defcheck solution-15e7b247
  (fn [qq]
    (cond (not (nil? (:x (conj qq [:x 2])))) :map
          (= (count (conj qq 2)) (count (conj qq 2 2))) :set
          (let [a {:x :x}] (identical? (first (conj (conj qq '()) a))  a)) :list
          :else :vector)))

(defcheck solution-166baf8b
  (fn [s]
    (let [s1 (conj s [:a 1])
          s2 (conj s1 [:b 2])]
      (cond
        (= 1 (:a s1)) :map
        (= s1 (conj s1 [:a 1])) :set
        (= [:b 2] (last s2)) :vector
        (= [:b 2] (first s2)) :list))))

(defcheck solution-16a0d14a
  (fn m [arg]
    (let [kv #(conj % [:k :v])]
      (if (= (kv arg) (kv (kv arg)))
        ;set or map
        (if (= :v (:k (conj arg [:k :v])))
          :map
          :set)
        ;vector or list
        (if (= :test1 (first (conj (conj arg :test2) :test1)))
          :list
          :vector)))))

(defcheck solution-17209bac
  #(if (associative? %)
     (if (reversible? %)
       :vector
       :map )
     (if (= (into % [1 1]) (into % [1]))
       :set
       :list )))

(defcheck solution-17de1a33
  (fn [x]
    (if (empty? x)
      (cond (= {} x) :map
            (= #{} x) :set
            (= :b (first (conj (conj x :a) :b))) :list
            :else :vector)
      (let [a (conj x (first x))]
        (cond (= (count a) (count x)) (if (nil? (get x (first x))) :map :set)
              (identical? (first a) (second a)) :list
              (identical? (first a) (last a)) :vector)))))

(defcheck solution-1812cfda
  (fn [xs]
    (let [e1 {:x-black-box "unique element"}
          e2 {:x-black-box "another unique"}
          ys (conj xs e1)
          zs (conj ys e2)]
      (cond
        (= zs (conj xs e2)) :map
        (= zs (conj zs e2)) :set
        (= (list e2 e1) (take 2 zs)) :list
        :else :vector))))

(defcheck solution-18231ae
  (fn vlms [x]
    (let [poked (conj x [1 2] [1 2] [1 3])
          c1 (count x)
          c2 (count poked)
          f2 (first poked)]
      (cond
        (= c2 (+ c1 1)) :map
        (= c2 (+ c1 2)) :set
        (= f2 [1 3]) :list
        :else :vector) )))

(defcheck solution-1828405b
  (fn [item]
    (cond
      (= (conj item [:test 0] [:test 1])
        (conj item [:test 1]))
      :map
      (= (conj item [:test 0])
        (conj item [:test 0] [:test 0]))
      :set
      (= (last (conj item :test :test2))
        :test2)
      :vector
      :else
      :list)))

(defcheck solution-18b12223
  #(cond
     (= % (conj % %)) :map
     (= (conj % 1) (-> % (conj 1) (conj 1))) :set
     (= \Z (-> % (conj \A) (conj \Z) (last))) :vector
     1 :list))

(defcheck solution-196e7e02
  #(cond (not (nil? (:x (into % {:x 9})))) :map
         (= (into % [1]) (into % [1 1])) :set
         (= :y (first (conj % :x :y))) :list
         :else :vector))

(defcheck solution-1990cb61
  (fn [xs]
    (let [r (conj xs [:x :y])]
      (cond
        (= (:x r) :y) :map
        (= (conj r [:x :y]) r) :set
        (= (cons :x r) (conj r :x)) :list
        :else :vector))))

(defcheck solution-1a265a1b
  (fn
    [x]
    (cond (= (count x) (count (conj x nil)))
          :map
          (= (count (conj x nil)) (count (conj x nil nil)))
          :set
          (nil? (first (conj x :banana nil)))
          :list
          :else
          :vector)))

(defcheck solution-1a605522
  (fn [coll]
    (cond
      (= coll (into [] coll))
      (if (= (conj coll ::drb226 ::drb227) (concat coll [::drb226 ::drb227]))
        :vector
        :list)
      (= coll (into #{} coll)) :set
      (= coll (into {} coll)) :map
      :else :unknown)))

(defcheck solution-1aea8a4c
  (fn [coll]
    (let [coll (into coll [[:a 1] [:b 2]])]
      (if (get coll :a)
        :map
        (let [coll1 (into coll [[:a 1]])]
          (if (= (count coll1) (count coll))
            :set
            (if (= (first coll) [:b 2])
              :list
              :vector
              )
            )
          )
        )
      )))

(defcheck solution-1b23cb43
  (fn [s]
    (let [fc (first (str s))
          ]
      (if (= fc \{)
        :map
        (if (or (= fc \( ) (= fc \c) )
          :list
          (if (= fc \[ )
            :vector
            :set))))

    ))

(defcheck solution-1ba7437c
  (fn [coll]
    (cond
      (get (conj coll [::blah 42]) ::blah)                             :map
      (>= (inc (count coll)) (count (conj (conj coll ::blah) ::blah))) :set
      (= (conj coll ::a) (-> coll (conj ::a) (conj ::b) rest))         :list
      :else                                                            :vector)))

(defcheck solution-1be3328c
  (fn f [xs]
    (case (conj (empty xs) [:a 1] [:a 2])
      {:a 2} :map
      [[:a 1] [:a 2]] :vector
      [[:a 2] [:a 1]] :list
      #{[:a 2] [:a 1]} :set)))

(defcheck solution-1cc5537d
  #(let [e (empty %)]
     (case e
       {}  :map
       #{} :set
       (first (conj e :vector :list)))))

(defcheck solution-1cfba3f
  #({\{ :map
     \[ :vector
     \# :set} (first (str %)) :list))

(defcheck solution-1d321fc
  (fn [x]
    (cond
      (= :23 (get (conj x [:42 :23]) :42)) :map
      (= x (set x)) :set
      (= :23 (first (conj x :42 :23))) :list
      (= :42 (last (conj x :42))) :vector)))

(defcheck solution-1d40f793
  (fn [x]
    (case (first (str x))
      \{ :map
      \( :list
      \[ :vector
      \# :set
      :list)))

(defcheck solution-1dc11837
  #(letfn [(mp? [c]
             (not= (:a c)
               (:a (conj c [:a (not (:a c))]))))
           (st? [c] (= (count (conj c :a))
                      (count (conj c :a :a))))
           (vc? [c]
             (and (= (last (conj (conj c :x) :a)) :a)
                  (= (last (conj (conj c :x) :b)) :b)))]
     (cond (mp? %) :map
           (st? %) :set
           (vc? %) :vector
           :else   :list)))

(defcheck solution-1ddf6a88
  #(case (first(str %)) \{ :map \# :set \[ :vector :list))

(defcheck solution-1e1cc111
  (fn [s] (cond (not= (count (conj s [1 2] [1 2])) (+ 2 (count s)))
                (get (conj s [[1 2] :map]) [1 2] :set)
                (= (conj s 1 2) (conj (seq s) 1 2)) :list
                :else :vector)))

(defcheck solution-1e51761f
  #(cond
     (= % (vec %))
     (if (= (conj (conj % 1) 2) (conj (conj (vec %) 1) 2))
       :vector :list)
     (= % (set %)) :set
     true :map))

(defcheck solution-1e6ff192
  #(let [c (first (str %))]
     (cond (= c \#) :set
           (= c \{) :map
           (= c \[) :vector
           :else :list)))

(defcheck solution-1ec1444
  (fn [a]
    (let [b (empty a)]
      (cond
        (and (= [] b) (associative? b)) :vector
        (= '() b) :list
        (= {} b) :map
        (= #{} b) :set
        ))))

(defcheck solution-1f0c233b
  (fn seq-type [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (= base '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-1f21dfcd
  (fn [c]
    (let [
          same_size? (fn [c1, c2] (= (count c1) (count c2)))
          not_adds_nil? (fn [c] (same_size? c (conj c nil)) )
          ]
      (cond
        (not_adds_nil? c) :map
        (same_size? (conj c :el) (conj (conj c :el) :el)) :set
        (= :el1 (last (conj (conj c :el2) :el1))) :vector
        :else :list
        ))))

(defcheck solution-1f2f2bba
  (fn type-of
    [some-struct]
    (let [foo [:some :foo]
          bar [:some :bar]
          is-vector? #(and (identical? (last (conj % foo bar)) bar)   ;; lifo behavior from last
                           (identical? (last (butlast (conj % foo bar))) foo)
                           (nil? (get (conj % foo) :some)))           ;; associative by index
          is-list? #(and (identical? (first (conj % foo bar foo)) foo)
                         (identical? (second (conj % foo bar foo)) bar) ;; lifo behavior from first
                         (nil? (get (conj % foo) :some)))
          is-set? #(and (= 2 (count (conj (empty %) foo bar foo))))   ;; uniques
          is-map? #(and (= :bar ((conj % foo bar) :some))             ;; override map-entry
                        (= 1 (count (conj (empty %) foo bar))))]      ;; only one-map-entry added
      (cond (is-set? some-struct) :set
            (is-vector? some-struct) :vector
            (is-list? some-struct) :list
            (is-map? some-struct) :map))))

(defcheck solution-1f7b36a4
  (fn [x] (if (associative? x) (if (reversible? x) :vector :map) (if (let [y (into x '(1 1))] (= (count y) (count (distinct (apply list y))))):set :list ))))

(defcheck solution-1ffa371e
  #(if (= (count (conj % [:rand 1] [:rand 1])) (+ (count %) 2))
     (if (= (first (conj % [:rand 1] [:rand 2])) [:rand 2])
       :list
       :vector)
     (if (= (count (conj % [:rand 1] [:rand 2])) (+ (count %) 2))
       :set
       :map)))

(defcheck solution-200aedfd
  #(let [im (into % [[:t1 :t2]])]
     (if (= :t2 (get im :t1))
       :map
       (let [m (conj (conj (conj % :t1) :t1) :t2)]
         (cond (= (count m) (+ 2 (count %))) :set
               (= :t2 (last m)) :vector
               (= :t2 (first m)) :list)))))

(defcheck solution-2044b317
  #(let [r (conj (empty %) [1 2] [1 2] [1 3])]
     (cond
       (= 1 (count r)) :map
       (= 2 (count r)) :set
       (= [1 3] (first r)) :list
       :else :vector)))

(defcheck solution-207e857c
  #(cond
     (= (get (conj % [:f :s]) :f) :s) :map
     (= (conj (conj % 10000) 10000) (conj % 10000)) :set
     (= (first (conj % 9999 10000)) 10000) :list
     :else :vector))

(defcheck solution-20c3901e
  (fn get-type [x]
    (let [ex (empty x)
          ex0 (conj ex [0 2])]
      (condp = (get ex0 0)
        [0 2] :vector
        2 :map
        nil (let [ex1 (conj ex 0)]
              (condp = (get ex1 0)
                nil :list
                0 :set))))))

(defcheck solution-210895cf
  (fn [coll]
    (let [e (empty coll)]
      (cond
        (= {} e) :map
        (= #{} e) :set
        (= (conj (conj coll 1) 2) (cons 2 (cons 1 coll))) :list
        :else  :vector))))

(defcheck solution-216bd2f4
  (fn [x]
    (let [base (empty x)]
      (cond
        (= base '{}) :map
        (= base #{}) :set
        (= base []) (if (reversible? base) :vector :list)))))

(defcheck solution-218494d4
  (fn [x]
    (condp #(= %1 (empty %2)) x
      '{} ':map
      '#{} ':set
      (if (reversible? x) ':vector :list))))

(defcheck solution-21933dde
  (fn black-box-testing [xs]
    (letfn [(map' [xs]
              (let [c1 (count xs)
                    c2 (count (apply conj xs (map (fn [i] [:a i]) (range 10))))]
                (<= (- c2 c1) 1)))

            (set' [xs]
              (let [c1 (count xs)
                    c2 (count (conj xs :a :a))]
                (<= (- c2 c1) 1)))

            (list' [xs]
              (let [c (count xs)]
                (->> (into xs (repeat 10 :a))
                  (take 10)
                  (= (repeat 10 :a)))))

            (vector' [xs]
              (let [xs (conj xs 1)
                    c (count xs)]
                (->> (into xs (repeat 10 :a))
                  (drop c)
                  (= (repeat 10 :a)))))]
      (cond
        (map' xs) :map
        (set' xs) :set
        (vector' xs) :vector
        (list' xs) :list))))

(defcheck solution-220a967e
  (fn [o]
    (let [fc (first (str o))]
      (case fc
        \# :set \{ :map \[ :vector :list))))

(defcheck solution-220e847c
  (fn [collection]
    (let [empty-collection (empty collection)]
      (if-not (nil? (:a (into empty-collection [[:a :1]])))
        :map
        (if (= :2 (first (conj (conj empty-collection :1) :2)))
          :list
          (if (= 2 (count (conj (conj empty-collection :1) :1)))
            :vector
            :set))))))

(defcheck solution-225563be
  (fn bbt [coll]
    (let [clear-coll (empty coll)
          primed-coll (conj clear-coll [0 :a] [1 :b])]
      (cond (= (get primed-coll 0) :a) :map
            (= (get primed-coll 0) [0 :a]) :vector
            (= (get primed-coll [0 :a]) [0 :a]) :set
            (= (first primed-coll) [1 :b]) :list))))

(defcheck solution-22607824
  (fn [l]
    (let [M 2147483647, c (count l)]
      (cond
        (= M (get (conj l [M M]) M))
        :map
        (= (inc c) (count (conj l M M)))
        :set
        (= M (last (conj l (- M 1) M)))
        :vector
        (= M (first (conj l (- M 1 ) M)))
        :list
        :else
        nil))))

(defcheck solution-232d0d76
  (fn [s]
    (let [w (into s [[::a 1] [::b 2] [::b 2]])
          c (count s)]
      #_(println w)
      (cond
        (= 1 (::a w)) :map
        (= 1 (count (filter (partial = [::b 2]) w))) :set
        (= [::b 2] (first w)) :list
        :else :vector))))

(defcheck solution-235bc4a8
  #(case (first (str %))
     \[ :vector
     \{ :map
     \# :set
     :list))

(defcheck solution-2455fb43
  (fn [bb]
    (let [mapped (map identity bb)
          g (gensym)
          h (gensym)]
      (if (= mapped bb)
        (if (= h (-> (conj bb g) (conj h) first))
          :list
          :vector)
        (if (= h (-> (conj bb {g h}) (get g)))
          :map
          :set)))))

(defcheck solution-24aa3b5a
  (fn prob65
    [col]
    (let [blank (empty col)]
      (cond
        (= blank {}) :map
        (= blank #{}) :set
        (= blank '()) (if (reversible? blank):vector :list)
        :else :unknown))))

(defcheck solution-24b3036c
  (fn [c] (let [d (conj c {0 1} {0 2} {0 1})] (cond (=
                                                      (+ 1 (count c)) (count d)) :map (= (+ 2 (count c)) (count d)) :set (= (cons
                                                                                                                              {0 3} d) (conj d {0 3})) :list true :vector))))

(defcheck solution-24c5b086
  (fn [s1]
    (let [s2 (conj s1 [::tt 1] [::tt 1])]
      (cond (not (= (count s2) (+ 2 (count s1))))
            (if (= 1 (s2 ::tt))
              :map :set)
            (= (count s1) 0)
            (if (= (first (conj s2 2)) 2) :list :vector)
            (= [::tt 1] (first s2))
            :list
            (= [::tt 1] (last s2))
            :vector
            true
            :other))))

(defcheck solution-24e011c4
  ; Hack!
  (fn [x]
    (or (get {\{ :map \# :set \[ :vector \( :list} (first (str x)))
        :list)))

(defcheck solution-24f06913
  (fn [s]
    (let [x :a
          y :b]
      (cond
        (= (x (conj s [x y])) y) :map
        (= (conj s x x) (conj s x)) :set
        (= (last (conj s x y)) y) :vector
        :else :list))))

(defcheck solution-25556fde
  (fn [s]
    (if (associative? s)
      (if (reversible? s) :vector
                          :map)
      (if (= (conj s -1) (conj (conj s -1) -1)) :set
                                                :list))))

(defcheck solution-258f17d4
  (fn p65 [col]
    (let [v (empty col)]
      (if (get (conj v [:test :test1]) :test )
        :map

        (cond (= (count (conj v :testv :testv )) (count (conj v :testv :testv :testv :testv)) ) :set
              (= (first (conj v :testv :testv1)) :testv1 ) :list
              (= (last (conj  v :testv :testv1)) :testv1 ) :vector

              :else :no)

        ) )
    ))

(defcheck solution-25d47dc7
  (fn [coll]
    (let [new-coll (conj (empty coll) {:a 1} {:a 1} {:b 1})
          first-elem (first new-coll)]
      (cond
        (= 2 (count first-elem)) :map
        (= 2 (count new-coll)) :set
        (= {:a 1} first-elem) :vector
        :else :list))
    ))

(defcheck solution-264147c5
  (fn [x]
    (let [t (conj (empty x) [:a :b] [:c :d])]
      (cond
        (:a t) :map
        (get t 0) :vector
        (get t [:a :b]) :set
        :else :list))))

(defcheck solution-265152db
  (fn [x] (if (= :b (get (conj x {:a :b}) :a)) :map
                                                (if (= (count (conj (conj x :sentinal) :sentinal)) (inc (count x))) :set
                                                                                                                    (if (= (first (conj (conj x :sentinal-1) :sentinal-2)) :sentinal-2) :list :vector)
                                                                                                                    )
                                                )
    ))

(defcheck solution-266fe869
  (fn seq-type [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (= base '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-26ee55ea
  (fn [coll]
    (let [new-coll (into coll [{:z :x} {:z :x} {:z :y}])
          diff (- (count new-coll) (count coll))]
      (cond
        (= 1 diff) :map
        (= 2 diff) :set
        :else (if (= {:z :y} (first new-coll)) :list :vector)))))

(defcheck solution-26f933d5
  #(let [blbx (conj % [:foo 42] [:foo 42] [:foo 43])]
     (cond (= (count blbx) (+ (count %) 2)) :set
           (= (count blbx) (inc (count %))) :map
           (= (first blbx) [:foo 43])       :list
           :else                            :vector)))

(defcheck solution-27541058
  (fn f [s] (cond
              (= s (into [] s)) (if (= (first (conj s :a :b)) :b) :list :vector)
              (= s (into #{} s)) :set
              (= s (into {} s)) :map)))

(defcheck solution-277e3b63
  (fn testing[seqq]
    (let[j (first (str seqq))]
      (cond
        (= j (char \{)) :map
        (= j (char \#)) :set
        (= j (char \[)) :vector
        :else :list
        ))))

(defcheck solution-281520a9
  (fn [coll]
    (if (reversible? coll)
      :vector
      (let [empty-coll (empty coll)]
        (cond
          (= empty-coll #{}) :set
          (= empty-coll {}) :map
          :else :list)))))

(defcheck solution-288e464c
  (fn bb-test- [coll]
    (cond
      (= (conj coll {}) coll) :map
      (= (first (conj coll {:c 3})) (first (conj (conj coll {:c 3}) {:d 4})))
      (if (= (+ (count '(11 12 11)) (count coll)) (count (apply conj coll '(11 12 11))))
        :vector
        :set)
      :else :list)))

(defcheck solution-28af2f3d
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     :else (if (== (+ 2 (count %))
                 (count (conj % 12 12))) :list :set)))

(defcheck solution-28ca4904
  (fn [c]
    (let [e (empty c)]
      (cond
        (= 1 (:a (conj e [:a 1]))) :map
        (= 1 (count (conj e 1 1))) :set
        (= 1 (first (conj e 1 2))) :vector
        (= 2 (first (conj e 1 2))) :list))))

(defcheck solution-28eea6d3
  (fn [x]
    (let [y (-> (empty x) (conj [:a 2]) (conj [:a 3]))]
      (cond
        (= 1 (count (conj y [:a 4]))) :map
        (= (first y) [:a 3]) :list
        (= 2 (count (conj y [:a 2]))) :set
        :else :vector))))

(defcheck solution-297e4c10
  (fn ty [x]
    (let [le (count x)
          y (conj x [:a :b] [:a :a] [:a :a])]
      (if (= (+ 3 le) (count y))
        (if (= (first y) [:a :a])
          :list
          :vector
          )
        (if (= (+ 2 le) (count y))
          :set
          :map)))))

(defcheck solution-2a0368a8
  (fn  [coll]

    (let [empty-coll (empty coll)]
      (cond
        (= empty-coll #{}) :set
        (= empty-coll {}) :map
        (reversible? empty-coll) :vector
        :else :list))))

(defcheck solution-2a29d280
  (fn [c]
    (cond
      (= #{} (empty c)) :set
      (= {} (empty c)) :map
      (= (cons :y (cons :x c)) (conj c :x :y)) :list
      true :vector)))

(defcheck solution-2a7ab27c
  (fn my-type [col]
    (if (= 0 (count col))
      (let [cnj (conj col col)]
        (if (zero? (count cnj))
          :map
          (my-type cnj)))
      (let [fst (first col)
            cnj (conj col fst)]
        (if (= (count col) (count cnj))
          (if (= fst (col fst))
            :set
            :map)
          (if (= col (first (conj col col)))
            :list
            :vector))))))

(defcheck solution-2aa198f0
  (fn [x]
    (let [y (empty x)]
      (case y
        {} :map
        #{} :set
        '() (case (-> y
                    (conj 1)
                    (conj 2)
                    first)
              1 :vector
              2 :list)))))

(defcheck solution-2b4c4e84
  (fn black-box [item]
    (let [test-vec (apply vector item)]
      (cond
        (and (not= item test-vec) (not (contains? (conj item [:t 1]) (first (conj item [:t 1])))))
        :map
        (and (not= item test-vec) (contains? (conj item [:t 1]) (first (conj item [:t 1]))))
        :set
        (and (= item test-vec) (= '(:test2 :test1) (take 2 (conj item :test1 :test2))))
        :list
        :else :vector))))

(defcheck solution-2cef67a4
  #(cond (= %1 (conj %1 {})) :map
         (= (conj %1 1 2) (cons 2 (cons 1 %1))) :list
         (= (conj %1 1 1) (conj %1 1)) :set
         true :vector))

(defcheck solution-2d06a874
  (fn [coll]
    (let [kv [:a \a]
          vs [1 2 3 10 100]
          mt (empty coll)]
      (cond
        (get (conj coll kv) :a)       :map
        (= vs (into mt vs))           :vector
        (= (reverse vs) (into mt vs)) :list
        :else                         :set))))

(defcheck solution-2d088665
  (fn [c]
    (let [c (empty c)]
      (cond
        (= {} c) :map
        (= #{} c) :set
        (= (first (conj c :a :b)) :b) :list
        :else :vector))))

(defcheck solution-2e9a2b11
  (fn check-type [coll]
    (let [coll-m (conj coll {:w :x :y :z})]
      (if (= (+ 2 (count coll)) (count coll-m))
        :map
        (let [coll-c (conj coll :z :z :x)]
          (if (= (+ 2 (count coll)) (count coll-c))
            :set
            (if (= (last coll-c) :x)
              :vector
              :list)))))))

(defcheck solution-2eb1f8e5
  (fn black-box [coll]
    (cond
      (not (associative? coll)) (let [cc (count coll)
                                      nn (count (into coll [1 1]))]
                                  (if (= 2 (- nn cc))
                                    :list
                                    :set))
      :else (let [coll' (conj coll (first coll))
                  cs (count coll)
                  cs' (count coll')]
              (if (= cs cs')
                :map
                :vector)))))

(defcheck solution-2eb45606
  #(try
     (let [c (conj % [:a 1] [:b 2] [:b 2])
           n (count %)]
       (cond
         (:a c) :map
         (= (count c) (+ n 2)) :set
         (= [:b 2] (last c)) :vector
         :else :list))))

(defcheck solution-2eb56261
  (fn [x]
    (if (ifn? x)
      (if (associative? x)
        (let [y (conj x [:a 1] [:a 2])]
          (if (some #{[:a 1]} y)
            :vector
            :map))
        :set)
      :list)))

(defcheck solution-2ee8cda8
  (comp #({\{ :map \# :set \[ :vector \( :list} % :list) first str))

(defcheck solution-2f6ac680
  (fn p065 [m]
    (cond
      (:c (conj m [:a :b] [:c :d])) :map
      (= (count (conj m [:a :b])) (count (conj m [:a :b] [:a :b]))) :set
      (= [:c :d] (last (conj m [:a :b] [:c :d]))) :vector
      true :list)))

(defcheck solution-2fdcfe73
  (fn [box]
    (cond (= (conj box nil) box)
          :map
          (= (conj box 'foo)
            (conj box 'foo 'foo))
          :set
          (= (first (conj box 0 'very-special-symbol))
            'very-special-symbol)
          :list
          :else
          :vector)))

(defcheck solution-301a79cb
  #(let [ys (into (into (empty %) {:a 1}) {:a 2})]
     (condp = ys
       {:a 2} :map
       [[:a 1] [:a 2]] :vector
       [[:a 2] [:a 1]] :list
       :set)))

(defcheck solution-30aadb4e
  (fn [c]
    (cond (= (inc (count c)) (count (conj c [:uniq-key :a] [:uniq-key :b]))) :map
          (= (inc (count c)) (count (conj c :uniq-val :uniq-val))) :set
          (= (last (conj c :uniq-frist :uniq-second)) :uniq-second) :vector
          :else :list
          )))

(defcheck solution-3146d602
  (fn [coll] ({7 :map 3 :set 9 :vector 5 :list}
              (+ (if (associative? coll)
                   5
                   1)
                 (count (flatten (seq (conj (empty coll) [5 5] [5 5]))))))))

(defcheck solution-31635487
  (fn [col]
    (let [x (empty col)]
      (cond (= x {}) :map
            (= x #{}) :set
            (= x []) (let [y (conj x 1 2)]
                       (if (= (first y) 1)
                         :vector
                         :list))))))

(defcheck solution-31b73eeb
  (fn [x]
    (let [n (count x)
          k (keyword (gensym))
          y (conj x [k 0] [k 0] [k 1])]
      (cond
        (= (inc n) (count y)) :map
        (= (+ 2 n) (count y)) :set
        (= [k 1] (last y)) :vector
        (= [k 1] (first y)) :list
        ))))

(defcheck solution-31e05c0f
  (fn [s]
    (let [t (-> (conj (empty s) [:a :b]) (conj [:a :b]))
          c (count t)]
      (cond
        (= 2 c) (if (= :b (first (conj (empty s) :a :b))) :list :vector)
        (= 1 c) (if (= :b (get t :a)) :map :set)
        ))))

(defcheck solution-31fcdb2a
  (fn t [coll]
    (let [secret :my-secret]
      (cond
        (= :bar (:foo (conj coll [:foo :bar]))) :map
        (= (inc (count coll)) (count (conj (conj coll secret) secret))) :set
        (= secret (first (conj (conj coll :foo) secret))) :list
        :else :vector)) ))

(defcheck solution-327bc33d
  (fn [coll]
    (let [v (conj coll [:x :y] [:p :q])]
      (cond
        (= (get v :x) :y) :map
        (= v (conj v [:x :y])) :set
        (= [:p :q] (last v)) :vector
        :else :list))))

(defcheck solution-32d0b6c
  (fn [c]
    (let [emptied (empty c)
          mapset {{} :map #{} :set}]
      (if (nil? (get mapset emptied))
        (if (= (first (conj emptied 1 2)) 2)
          :list
          :vector)
        (get mapset emptied)))))

(defcheck solution-32dfa08f
  #(let [c (conj (empty %) [0 1])]
     (condp = (get c 0)
       1 :map
       [0 1] :vector
       (if (= 1 (count (conj c [0 1]))) :set :list)
       )))

(defcheck solution-33660a33
  ; easy-peasy: see documentation for `empty`
  (fn [coll]
    (let [e (empty coll)]
      (cond (= e {}) :map
            (= e #{}) :set
            ; [] and () compare alike for equality, however,
            ; list does not implement Reversible, vector does
            (= e []) (if (reversible? coll) :vector :list)
            :default nil))))

(defcheck solution-33a31643
  (fn black-box [coll]
    (cond
      (and (associative? coll) (not (reversible? coll))) :map
      (and (associative? coll) (reversible? coll)) :vector
      :else
      (if (= (count (conj (conj coll 'A) 'A)) (count (conj coll 'A)))
        :set
        :list )
      )))

(defcheck solution-33ad1651
  (fn [coll]
    (let [s (first (str coll))]
      (cond
        (= \{ s) :map
        (= \# s) :set
        (= \[ s) :vector
        :else :list))))

(defcheck solution-340ad34b
  (fn [coll]
    (let [v "some-val"]
      (cond
        (= (conj coll {}) coll) :map
        (= (inc (count coll))
          (count (apply conj coll (repeat 5 v)))) :set
        (= v (first (conj coll "other val" v))) :list
        :else :vector))))

(defcheck solution-344fbb4b
  #(let [c (conj (empty %) [1 2])]
     (if (= 1 (count (flatten [c])))
       (if (nil? (get c 1)) :set :map)
       (if (= 1 (first (conj (empty c) 1 2))) :vector :list))))

(defcheck solution-35474ec
  (fn [coll]
    (let [es (empty coll)]
      (cond (= es {}) :map
            (= es #{}) :set
            (= :y (first (conj es :x :y))) :list
            :else :vector))))

(defcheck solution-3651348e
  (fn black-box-testing [coll]
    (let [pair1 [:new-key1 :new-val1]
          pair2 [:new-key2 :new-val2]
          new-coll (conj (conj coll pair1) pair2)]
      (if (= (conj new-coll pair2) new-coll)
        (if (contains? new-coll :new-key2) :map :set)
        (if (= (cons pair2 (cons pair1 coll)) new-coll) :list :vector)))))

(defcheck solution-368d60e4
  (fn seq-type [s]
    (let [test (conj s [:set "set"] [:set "set"] [:conj "conj"])]
      (cond
        (:set test)
        :map
        (= 1 (get (frequencies test) [:set "set"]))
        :set
        (= [:conj "conj"] (first test))
        :list
        (= [:conj "conj"] (last test))
        :vector
        :else
        :unknown))))

(defcheck solution-375cdd81
  (fn [s]
    (let [[x y] [[:k :u] [:k :v]]
          t (into s [x y])]
      (cond (=  (+ 1 (count s))
              (count t)) :map
            (= (conj t x) t) :set
            (= (last t) y) :vector
            (= (first t) y) :list))))

(defcheck solution-376ed416
  (fn tip [x]
    (cond
      (= #{} (empty x)) :set
      (= {} (empty x)) :map
      (not= (conj (conj x 1) 2) (conj (conj (vec x) 1) 2)) :list
      :else :vector)))

(defcheck solution-382e86f9
  (fn [d]
    (let [e (empty d)]
      (cond
        (= e {}) :map
        (= e #{}) :set
        (= e []) (if (reversible? d) :vector :list)))))

(defcheck solution-383d0fd7
  (fn [coll]
    (let [r (conj coll {:new 1} {:new 1} {:new 2})
          co (count coll)
          cf (count r)]
      (cond
        (= cf (inc co)) :map
        (= cf (+ 2 co)) :set
        (= (first r) {:new 2}) :list
        :else :vector))))

(defcheck solution-387edd47
  #(if (reversible? %) :vector
                       (if (associative? %) :map
                                            (if (= (set %) %) :set :list))))

(defcheck solution-38ae8d3d
  (fn tf [c]
    (let [ec (empty c)]
      (cond
        (= 1 (count (into ec [[1 2] [1 3]]))) :map
        (= 1 (count (into ec [1 1]))) :set
        (= (into ec [1 2]) [1 2]) :vector
        :else :list))))

(defcheck solution-38c32146
  #((zipmap
      (map str [{} #{} [] ()])
      [:map :set :vector :list])
    (str (empty %))))

(defcheck solution-3924e20e
  #(cond
     (associative? %)
     (if (= (first (assoc % 0 'a)) 'a) :vector :map)
     (< (count (conj (conj % 'a) 'a)) (+ 2 (count %)))
     :set
     :else :list))

(defcheck solution-3a3b51ac
  #(case
    ((juxt associative? empty) %)
     [false #{}] :set
     [false ()]  :list
     [true {}]  :map
     [true []]  :vector
     ))

(defcheck solution-3a7d508d
  #(if (= (conj % %) %)
     :map
     (if (reversible? %)
       :vector
       (if (ifn? %)
         :set
         :list))))

(defcheck solution-3a9402b5
  (fn bb [coll]
    (let [a [1 1] b [1 1] c [1 2] n (conj coll a b c)]
      (cond
        (= (count n) (+ 1 (count coll))) :map
        (= (count n) (+ 2 (count coll))) :set
        (= (first n) [1 2]) :list
        (= (last n) [1 2]) :vector))))

(defcheck solution-3a96c1a4
  #(cond (reversible? %) :vector (associative? %) :map (let [c (conj % 0)] (= c (conj c 0))) :set 0 :list))

(defcheck solution-3b65616c
  #(if (ifn? %)
     (if (associative? %)
       (cond
         (= [] %) :vector
         (= {} %) :map
         :else (if (nil? (% (first %) ) ) :map :vector ) )
       :set)
     :list))

(defcheck solution-3c45b8c3
  (fn [c]
    (if (= (get (conj c {:k "v"}) :k) "v")
      :map
      (if (= (get (conj c "v") "v") "v")
        :set
        (if (= (first (conj c "a" "b")) "b")
          :list
          (if (= (last (conj c "a" "b")) "b")
            :vector))))))

(defcheck solution-3c46bf52
  (fn [S]
    (cond
      (reversible? S) :vector

      (associative? S) :map
      (= (count (conj S 1)) (count(conj S 1 1))) :set
      :else :list
      )
    ))

(defcheck solution-3c9dac83
  #(let [x (conj % [9 8] [7 6])]
     (cond (get x [9 8]) :set
           (= (get x 9) 8) :map
           (= (first x) [7 6]) :list
           1 :vector)))

(defcheck solution-3dc062b4
  (fn [a]
    (let [c (count a)]
      (cond (= (inc c) (count (into a [[1 2] [1 3]]))) :map
            (= (inc c) (count (into a [:a :a]))) :set
            (= :a (first (into a [:b :a]))) :list
            :else :vector
            ))))

(defcheck solution-3e0243bb
  #(cond
     (= (conj % nil) %) :map
     (= (conj % 0) (conj % 0 0)) :set
     (= (conj % 0 1) (cons 1 (cons 0 %))) :list
     :else :vector))

(defcheck solution-3e271493
  (fn [val] (let [v (empty val)]
              (cond
                (zero? (count (conj v (:a :b)))) :map
                (not= 3 (count (into v [:a :b :b]))) :set
                (= [:x :y] (into v [:y :x])) :list
                :else :vector))))

(defcheck solution-3e2cc9bf
  #(cond
     (= 2 (get (conj % [:a 2]) :a)) :map
     (= (conj % 1 2) (cons 2 (cons 1 %))) :list
     (= (conj % 1) (conj % 1 1)) :set
     :else :vector))

(defcheck solution-3e38c2e5
  (fn [x]
    (let [g (gensym)]
      (cond
        (= (get (conj x [-1 true] [0 g] [1 true]) 0) g)
        :map
        (= (conj x g g) (conj x g))
        :set
        (identical? (first (conj x 0 g)) g)
        :list
        true
        :vector))))

(defcheck solution-3eb5f177
  (fn [coll]
    (let [coll (empty coll)
          coll (conj coll nil)]
      (if (zero? (count coll))
        :map
        (let [coll (conj coll 2)]
          (if-not (nil? (get coll 2))
            :set
            (if (nil? (get coll 1))
              :list
              :vector)))))))

(defcheck solution-3fc13dc5
  (fn [s]
    (cond
      (= (inc (count s))
        (count (conj s [::key ::val1] [::key ::val2])))
      :map

      (= (inc (count s))
        (count (conj s ::entry ::entry ::entry)))
      :set

      (= ::end
        (last (conj s ::dummy ::end)))
      :vector

      :else
      :list)))

(defcheck solution-40115b2f
  (fn [x]
    (cond
      (= [0 0] (get (into x {0 0}) [0 0])) :set
      (nil? (get (into x {0 0}) 0)) :list
      (= 0 (get (into x {0 0}) 0)) :map
      :else :vector)))

(defcheck solution-401855eb
  (fn [s]
    (let [s' (into s '([:foo :bar] [:cat :dog]))]
      (cond
        (= (:foo s') :bar) :map
        (= (count s') (count (conj s' [:foo :bar]))) :set
        (= [:cat :dog] (first s')) :list
        :else :vector))))

(defcheck solution-40296ad0
  (fn [coll]
    (let [emp (empty coll)]
      (if (= emp {})  :map
                      (if (= emp #{}) :set
                                      (let [ls (conj coll :a)]
                                        (if (= (first (conj ls :b)) :b)
                                          :list
                                          :vector)
                                        ))))))

(defcheck solution-404c278c
  (fn class*
    [coll]
    (let [s (first (str coll))]
      (cond
        (= s \[) :vector
        (= s \{) :map
        (= s \#) :set
        :else    :list))))

(defcheck solution-4059dc0e
  (fn r [s]
    (let [v {\{ :map
             \# :set
             \[ :vector
             \( :list}
          [f &o] (print-str s)]
      (v f))))

(defcheck solution-40af07c9
  (fn testVal[col]
    (let [cnt (count col),
          newCol (conj (conj (conj col {100 1} ) {100 2} ) {100 2} ),
          cnt2 (count newCol),
          sign (- cnt2 cnt)]
      (cond (= sign 1) (keyword "map" )
            (= sign 2) (keyword "set" )
            (= (first newCol) {100 2}) (keyword "list")
            :else (keyword "vector")
            )
      )
    ))

(defcheck solution-412e77d4
  #(cond
     (let [l (conj % [:z 1])]
       (= (:z l) 1)) :map
     (let [l (conj % 1)]
       (= (count (conj l (first l))) (count l))) :set
     (let [l (conj % 1)]
       (= (first (conj l 99)) 99)) :list
     true :vector))

(defcheck solution-4132fabe
  #(let [a [:x :y]
         b [:x :z]
         c (into % [a b])
         d (count c)]
     (cond
       (= d (+ 1 (count %))) :map
       (= d (count (conj c a))) :set
       (= (nth c 0) b) :list
       1 :vector
       )))

(defcheck solution-42820c00
  (fn [coll]
    (let [coll (conj (empty coll) [:a 1] [:a 1])]
      (if (= (count coll) 1)
        (if (= (:a coll) 1)
          :map
          :set)
        (if (first (conj coll false))
          :vector
          :list)))))

(defcheck solution-4285bd0c
  (fn [s]
    (cond
      (= s (set s)) :set
      (not (nil? (re-matches #"^\{.*\}$" (str s)))) :map
      (not (nil? (re-matches #"^\[.*\]$" (str s)))) :vector
      :else :list)))

(defcheck solution-428e829d
  (fn coll-type[coll]
    (let [coll (conj coll {:test coll})]
      (letfn [(lists? [coll]
                (= coll (map identity coll)))
              (map-or-set [smap]
                (if (= smap (reduce conj #{} smap)) :set :map))
              (list-or-vec [coll]
                (if (= (conj coll coll) (cons coll coll)) :list :vector))]
        (if (lists? coll) (list-or-vec coll) (map-or-set coll))))))

(defcheck solution-4436dc80
  (fn [coll]
    (let [fixed-coll (conj (empty coll) [:a :b] [:b :a])]
      (cond
        (= (:a fixed-coll) :b) :map
        (= (conj fixed-coll [:a :b]) fixed-coll) :set
        (= (first fixed-coll) [:b :a]) :list
        (= (first fixed-coll) [:a :b]) :vector))))

(defcheck solution-446af7ba
  (fn [in]
    (cond
      (get (into in [[:k :v]]) :k) :map
      (get (into in [[:k :v]]) [:k :v]) :set
      (let [ufirst (into in [:my_first_entry :my_second_entry])]
        (= (first ufirst) :my_second_entry)) :list
      :else :vector)))

(defcheck solution-44f0b98b
  (fn [c]
    (cond
      (= (:d (conj c [:d :e])) :e) :map
      (= (:d (conj c :d)) :d) :set
      (= (last (into c [:d :e])) :e) :vector
      :else :list)))

(defcheck solution-4524f153
  (fn black-box-test [arg]
    (cond (= arg {}) :map
          (= arg #{}) :set
          (= arg []) (if (= 2 (last (conj (conj arg 1) 2))) :vector :list)
          (or (= [:a 1] (first arg)) (= [:b 2] (first arg))) :map
          (= (count arg) (count (conj arg (first arg)))) :set
          (= (last (conj arg 111)) 111) :vector
          :else :list)))

(defcheck solution-457de30a
  (fn guess [col]
    (let [f1 (conj col [:test 1]),
          f2 (conj col [:test 1] [:test 1])]
      (if (= (count f1) (count f2))
        ;; :set or :map
        (let [f (conj col [:map :test])]
          (if (contains? f :map)
            :map
            :set
            )
          )
        ;; :list or :vector
        (let [f (first (conj col :test :list))]
          (if (= f :list)
            :list
            :vector
            )
          )
        )
      )
    ))

(defcheck solution-45f30d9e
  (fn black-box [c]
    (let [test ((fn [x] (reduce #(conj % %2) (empty x) [[1 1][1 1][2 2]])) c)]
      (if (= 3 (count test))
        (if (= 1 (ffirst test)) :vector :list)
        (if (contains? test [1 1]) :set :map)))))

(defcheck solution-4625d4d4
  (fn black-box [old-s]
    (let [s (conj old-s [1 2])]
      (cond
        (= s (set s)) :set
        (not (= s (vec s))) :map
        (= "joelboy" (first (conj s "joelboy"))) :list
        :else :vector))))

(defcheck solution-46732c59
  #(cond
     (= (:k (conj % [:k :v])) :v) :map
     (= (count (conj % :v)) (count (conj % :v :v))) :set
     (= (first (conj % :v1 :v2)) :v2) :list
     :else :vector
     ))

(defcheck solution-46b5b3e7
  (fn [coll]
    (let [test-data [[:k1 :v1] [:k1 :v1] [:k1 :v2]]
          poked-coll (into (empty coll) test-data)]
      (cond
        (= test-data poked-coll) :vector
        (= test-data (reverse poked-coll)) :list
        (= (count poked-coll) 2) :set
        (= (count poked-coll) 1) :map
        :else :wtf))))

(defcheck solution-4705499c
  (fn [coll] (condp = ((juxt associative? reversible? ifn?) coll)
               [true true true] :vector
               [false false false] :list
               [true false true] :map
               [false false true] :set)))

(defcheck solution-4757200a
  (fn [c]
    (cond
      (= (:c (conj c {:c 2})) 2) :map
      (= (inc (count c)) (count (conj c :c :c))) :set
      (= (last (conj c :c :d)) :d) :vector
      (= (first (conj c :c :d)) :d) :list)))

(defcheck solution-479fca68
  (fn mytest-type [col]
    (if (or (= 2 (count (flatten (vector (last col)))))
            (and (empty? col)
                 (= (into col {:test 1}) {:test 1})))
      :map
      (if (= (count (conj col :test :test)) (+ 1 (count col)))
        :set
        (if (= (first (conj col :test1 :test2)) :test2)
          :list
          :vector)))))

(defcheck solution-4808c07c
  #({{} :map #{} :set} (empty %) (if (reversible? %) :vector :list)))

(defcheck solution-4861bf97
  (fn bbt [xs]
    ;set
    (case (empty xs)
      {} :map
      #{} :set
      (if (= (conj (empty xs) 1 2) [1 2])
        :vector
        :list))
    ))

(defcheck solution-487fb0c5
  (fn black-box [xs]
    (let [s (.toString xs)]
      (cond (.startsWith s "{") :map
            (.startsWith s "#{") :set
            (.startsWith s "[") :vector
            :else :list))))

(defcheck solution-48fbd346
  (fn [coll] (cond
               (= :x (get (conj coll [1 :x]) 1)) :map
               (= [:c 4] (first (conj (conj coll [:c 3]) [:c 4]))) :list
               (= (+ 1 (count coll)) (count (conj (conj coll [:c 4]) [:c 4]))) :set
               :else :vector
               )))

(defcheck solution-49466267
  (fn [x]
    (let [extra (conj x [:ooga :booga])
          two-extra (conj extra [:dooga :rooga])
          extra-pairs (into x [[:zxcvb :foobar] [:zxcvb :barfoo]])]
      (cond
        (= (count extra-pairs) (inc (count x))) :map
        (= (count extra) (count (conj extra [:ooga :booga]))) :set
        (= [:dooga :rooga] (last two-extra)) :vector
        :else :list))))

(defcheck solution-494d2492
  (fn [xs]
    (if (= 1 (get (conj xs {:a 1}) :a))
      :map
      (if (= 2 (- (count (apply conj xs [1 1])) (count xs)))
        (if (= 2 (first (apply conj xs [1 1 2])))
          :list
          :vector
          )
        :set
        )
      )
    ))

(defcheck solution-4955406
  (fn[s]
    (let [c (-> s (conj [:c 3]) (conj [:c 3]) (conj [:c 4]))]
      (cond (= (count c) (+ 2 (count s))) :set
            (= (count c) (inc (count s))) :map
            (= (first c) [:c 4]) :list
            (= (last c) [:c 4]) :vector))))

(defcheck solution-4958e89b
  (fn black-box-testing [v]
    (cond
      (:a (conj v [:a 1])) :map
      (< (- (count (conj v 1 1)) (count v)) 2) :set
      (= (conj v 1 2) (cons 2 (cons 1 v))) :list
      :else :vector)))

(defcheck solution-499d5430
  (fn me [coll]

    (let [mystr (str coll)

          mystr-seq (seq mystr)

          first-seq (first mystr-seq)

          ]

      (cond
        (= first-seq \() :list
        (= first-seq \#) :set
        (= first-seq \{) :map
        (= first-seq \[) :vector
        :else :list

        )

      )

    ))

(defcheck solution-49ab5656
  (fn [x] (let [x2 (conj x [:q :qv])
                x3 (conj x2 [:q :qv])]
            (cond
              (not (ifn? x)) :list
              (not= x2 x3) :vector
              (= (x2 :q) :qv) :map
              :else :set))))

(defcheck solution-49b00e7e
  (fn[coll]
    (let [sym (gensym)
          coll (conj coll [sym sym])]
      (if (= (count coll) (count (conj coll (first coll))))
        (if (nil? (get coll sym))
          :set
          :map)
        (if (= (first (conj coll sym)) sym)
          :list
          :vector)))))

(defcheck solution-49b8e4d2
  (comp #(cond (= % {}) :map (= % #{}) :set (= (conj % 1 2) [1 2]) :vector true :list) empty))

(defcheck solution-49bb26d2
  (fn t [x]
    (if (associative? x)
      ;Either {} or []
      (if (reversible? x)
        :vector
        :map)
      (if (= (count (conj x 1 1)) (+ 2 (count x)))
        :list
        :set
        ))))

(defcheck solution-49f3d8b3
  (fn [c]
    (let [poked (conj (empty c) [:a 1] [:a 1] [:b 2])]
      (if (= (count poked) 2)
        (if (= (:b poked) 2) :map :set)
        (if (= (first poked) '(:b 2)) :list :vector)))))

(defcheck solution-4a368d4c
  (fn [xs]
    (let [ys (-> xs (empty) (conj [:a 0] [:a 0] [:b 0]))]
      (if (-> ys (count) (= 2))
        (if (ys :a)
          :map
          :set)
        (if (-> ys (first) (first) (= :a))
          :vector
          :list)))))

(defcheck solution-4a543984
  (fn [x]
    (let [
          zero (empty x)
          indicator (into zero [[:a :vector] [:a :vector] [:a :list]])
          size (count indicator)]
      #_(println x zero indicator size)
      (cond
        (= 1 size) :map
        (= 2 size) :set
        :else      (-> indicator first last)))))

(defcheck solution-4ad8028f
  #(let [x (conj % {:a :b} {:c :d} {:c :d})]
     (cond (get x :a) :map
           (= 2 (- (count x) (count %))) :set
           (= {:c :d} (first x)) :list
           1 :vector)))

(defcheck solution-4b2624c
  (fn [x]
    (let [y (conj x [9 1] [9 1])]
      (cond
        (= (inc (count x))
          (count y))
        (if (y 9) :map :set)
        (= 4 (first (conj y 4))) :list
        1 :vector))))

(defcheck solution-4c772250
  (fn [inp]
    (cond
      (= :sentinal (get (conj inp [:sentinal :sentinal]) :sentinal)) :map
      (= (conj inp (first inp)) #{nil}) :set
      (= (conj inp (first inp)) inp) :set
      (= (first (conj (conj inp :foo) :sentinal)) :sentinal) :list
      (= (last (conj (conj inp :foo) :sentinal)) :sentinal) :vector
      )))

(defcheck solution-4c8b225d
  #(if (or (= % {})
           (not= (count (flatten (seq %))) (count %)))
     :map (if (= (into #{} %) %)
            :set (if (= -2 (first (conj (conj % -1) -2)))
                   :list
                   :vector)) ))

(defcheck solution-4d3d73dd
  (fn [col]
    (if (empty? col)
      (cond  (= col {}) :map
             (= col #{}) :set
             (= (last (conj col 1 2)) 2) :vector
             :else :list)
      (let [x (first (set col))]
        (cond
          (= (count col) (count (conj col x)))
          (if (col x) :set :map)
          (= (last (conj col :different)) :different)
          :vector
          :else
          :list)))))

(defcheck solution-4d902a73
  (fn tst [coll]
    (let [x 1337
          y 1338
          z [x y]
          c (conj coll z)]
      (cond
        (= y (get c x)) :map
        (= z (get c z)) :set
        (= x (last (conj c x))) :vector
        :else :list
        )
      )
    ))

(defcheck solution-4e747e86
  (fn [xs]
    (let [s (str xs)]
      (cond (re-find #"^\{" s) :map
            (re-find #"^\#\{" s) :set
            (re-find #"^\[" s) :vector
            :else :list))))

(defcheck solution-4e84b56
  (fn [s]
    (cond
      (reversible? s) :vector
      (associative? s) :map
      (ifn? s) :set
      :else :list)))

(defcheck solution-4ef64742
  (fn [s]
    (let [e (empty s)]
      (cond
        (= e {}) :map
        (= e #{}) :set
        (= (conj e 1 2) [1 2]) :vector
        true :list))))

(defcheck solution-4ef64b71
  (fn [coll]
    (let [xs (empty coll)]
      (cond
        (= xs {}) :map
        (= xs #{}) :set
        (= (conj xs :x :y) [:x :y]) :vector
        :else :list))))

(defcheck solution-4efd65d3
  (fn black-box-testing [coll]
    (cond
      (:a (conj coll [:a 1])) :map
      (= (cons 1 (cons 2 coll)) (conj coll 2 1)) :list
      (> 2 (- (count (conj coll 1 1))
              (count coll))) :set
      :else :vector)))

(defcheck solution-4f60cad9
  (fn [coll]
    (let [c (empty coll)
          r (str c)]
      (condp = r
        "{}"  :map
        "#{}" :set
        "[]"  :vector
        :list))))

(defcheck solution-4fa079ea
  (fn typeofc [t]
    (let [c (empty t)]
      (cond
        (= (into c [[1 1] [2 2]]) '([2 2][1 1])) :list
        (= ((conj c [0 :v]) 0) :v) :map
        (= (count (conj c :v)) (count (into c [:v :v]))) :set
        :else :vector
        )
      )
    ))

(defcheck solution-4fc88708
  #(condp = (first (str %))
     \# :set
     \[ :vector
     \{ :map
     :list
     ))

(defcheck solution-4fe4571b
  (fn seq-type [coll]
    (let [coll (empty coll)]
      (cond
        (= coll {}) :map
        (= coll #{}) :set
        (= coll '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-4ff8cfc7
  #(case (empty %)
     {} :map
     #{} :set
     (if (= (last (into % [:a :b])) :b) :vector :list)))

(defcheck solution-504f2f3c
  (fn whatis [coll]
    (cond
      (reversible? coll) :vector
      (associative? coll) :map
      (= (count (conj (conj coll :jpK1) :jpK1)) (count (conj coll :jpK1))) :set
      :else :list
      )
    ))

(defcheck solution-509d884d
  (fn
    [coll]
    (let [size (count coll)
          newsym (gensym)
          newsym' (gensym)
          coll2 (conj coll [newsym newsym'])]
      (cond
        (get coll2 newsym) :map
        (= (inc size) (count (conj (conj coll newsym) newsym))) :set
        (= newsym' (last (conj (conj coll newsym) newsym'))) :vector
        :else :list))))

(defcheck solution-51a4576d
  (fn [c]  (let [c2 (conj (empty c ) [::x ::y] [::x ::y] [::x ::z] [::y ::x])
                 n (count c2) ]
             (cond
               (= n 4 ) ; list or vector
               (if (= (ffirst c2) ::x)
                 :vector
                 :list)
               (= n 2 )  :map
               :else :set))))

(defcheck solution-51b828a
  (fn test
    [x]
    (let [base (empty x)]
      (cond
        (= {} base) :map
        (= #{} base) :set
        :else (if (reversible? base) :vector :list)))))

(defcheck solution-51dbfbf4
  #(cond
     (get (conj % [:c 3]) :c) :map
     (= (conj % 1) (conj % 1 1)) :set
     (= [:one :two] (take 2 (conj % :two :one))) :list
     (= [:one :two] (take 2 (reverse (conj % :two :one)))) :vector
     ))

(defcheck solution-523444f9
  #({
     \# :set
     \[ :vector
     \{ :map}
    (nth (str %) 0)
    :list))

(defcheck solution-53372fe5
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (= base '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-539f4a70
  (fn [arg]
    (let [my-type (empty arg)]
      (cond
        (= (conj my-type [:a 1]) {:a 1}) :map
        (= 1 (count (reduce #(conj %1 %2) my-type (list :a :a)))) :set
        (= (first (reduce #(conj %1 %2 ) my-type (list :a :b))) :a) :vector
        (= (first (reduce #(conj %1 %2 ) my-type (list :a :b))) :b) :list
        ))))

(defcheck solution-5468c7a7
  #(cond
     (= (into % {:a 1}) (into (into (into % {:a 1}) {:a 2}) {:a 1})) :map
     (= (into % '(:a)) (into (into % '(:a)) '(:a))) :set
     (= :b (last (conj (conj % :a) :b))) :vector
     true :list))

(defcheck solution-54a1ef52
  (fn [thing]
    (let [guinea {:guinea :pig} pig {:pig :guinea}]
      (if (= (count (conj thing guinea guinea)) (inc (count thing)) )
        (if (contains? (conj thing guinea) guinea)
          :set
          :map
          )
        (if (= (last (conj thing guinea pig)) pig)
          :vector
          :list
          )
        )
      )
    ))

(defcheck solution-55b752ce
  (fn [x]
    (let [t (conj (empty x) [:a :b] [:c :d])]
      (cond
        (:a t) :map
        (get t 0) :vector
        (get t [:a :b]) :set
        :else :list))))

(defcheck solution-56084574
  #(let [r (conj (empty %1) [:a 1] [:a 2] [:b 3] [:b 3])]
     (cond
       (= (count r) 2) :map
       (= (count r) 3) :set
       (= (last r) [:a 1]) :list
       :else :vector)))

(defcheck solution-57093fe4
  (fn my-type? [coll]
    (letfn [(count-diff [coll1 coll2] (- (count coll1) (count coll2)))
            (conj-diff [coll f] (count-diff coll (f coll)))]
      (if (= 0 (conj-diff (conj coll [1 1]) #(conj % [1 1])))
        (if (contains? (conj coll {}) {})
          :set
          :map)
        (let [v (gensym)]
          (if (= (last (conj (conj coll 1) v)) v)
            :vector
            :list))))))

(defcheck solution-577ae5dd
  #(if (= % (conj % %)) :map
                         (let [extended (conj % :first :second)]
                           (cond
                             (= extended (into extended extended)) :set
                             (= (first extended) :second) :list
                             :else :vector))))

(defcheck solution-57c555a
  (fn [coll]
    (let [result (conj (empty coll) [1 2] [1 2] [1 3])]
      (if (= 1 (count result))
        :map (if (= 2 (count result))
               :set (if (= [1 3] (first result))
                      :list :vector ))))))

(defcheck solution-57d69633
  (fn [xs] (let [xs (empty xs)] (cond (= xs {}) :map (= xs #{}) :set
                                      (= xs ())
                                      (if (= :a (first (conj xs :a :b))) :vector :list)))))

(defcheck solution-57db6bfa
  (fn p65 [x]
    (cond (associative? x) (if (= [[1 2]] (conj (empty x) [1 2])) :vector :map)
          :else (if (= #{1} (conj (conj (empty x) 1) 1))
                  :set
                  :list))))

(defcheck solution-5882de7c
  (fn [col]
    (let [x (gensym)
          y (gensym)
          map-col (into col [[x y]])
          n-col #(into col [x y])
          ]
      (cond
        (= y (x map-col)) :map
        (= (count (n-col)) (-> (into (n-col) [x]) count)) :set
        (= (first (n-col)) y) :list
        :else :vector))))

(defcheck solution-5904b267
  (fn f [c]
    (let [b (conj c {:x :y} {:herp :derp} {:foo :bar})
          l (last b)
          f (first b)
          v (get b :foo)
          w (get b {:foo :bar})]
      (cond
        (= v :bar) :map
        (= w {:foo :bar}) :set
        (= l {:foo :bar}) :vector
        (= f {:foo :bar}) :list
        :else :fail))))

(defcheck solution-5915caed
  (fn[coll]
    (let [tmp (conj (empty coll)
                [:key :vector] [:key :list] [:key :list])
          cnt (count tmp)]
      (cond
        (= 1 cnt)
        :map

        (= 2 cnt)
        :set

        :else
        (-> tmp first second)))))

(defcheck solution-591cc2c6
  #({\# :set
     \{ :map
     \[ :vector
     \( :list} (first (pr-str %))))

(defcheck solution-592291ed
  (fn ct [xs]
    (if (= (inc (count xs)) (count (conj (conj xs {:z 1000}) {:z 1001})))
      :map
      (if (= (inc (count xs)) (count (conj (conj xs 1000) 1000)))
        :set
        (if (= (first (conj (conj xs 1000) 999)) 999)
          :list
          :vector)))))

(defcheck solution-5936d346
  (fn[c]
    (if (or (= c {}) (and (> (count c) 1) (every? coll? c)))
      :map
      (if (= c (set c))
        :set
        (if (= (conj c 1 2) (concat c [1 2]))
          :vector
          :list)))))

(defcheck solution-599d0718
  (fn [a]
    (let [base (empty a)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (= base '()) (if (reversible? a) :vector :list)))))

(defcheck solution-59a73ae4
  (fn bb-testing [x]
    (let [raw (empty x)
          filled (conj raw [:a :a] [:a :a] [:a :b])]
      (case (count filled)
        1 :map
        2 :set
        3 (if (= [:a :b] (first filled))
            :list :vector
            )
        )
      )
    ))

(defcheck solution-5ac5a8e5
  #({\[ :vector \# :set \{ :map} (nth (str %) 0) :list))

(defcheck solution-5b3b4de1
  (fn [s]
    (cond (-> s empty (conj {:a 1}) :a (= 1)) :map
          (-> s empty (conj 1) (conj 1) count (= 1)) :set
          (-> s empty (conj 1) (conj 2) first (= 2)) :list
          (-> s empty (conj 1) (conj 2) last (= 2)) :vector
          :else nil)))

(defcheck solution-5b4d015
  (fn [s]
    (if
     (= (+ 2 (count s)) (count (into (into s {4 5}) {4 5})))
      (if (= :w (first (into s [:q :w])))
        :list
        :vector)
      (if (= (+ 2 (count s)) (count (into (into s {4 5}) {4 6})))
        :set
        :map)
      )))

(defcheck solution-5b5da45a
  (fn [coll]

    (if (= (rest (cons :b coll )) coll)
      (if (= (rest (conj (conj coll :a) :b)) (conj coll :a) ) :list :vector)
      (let [ cj1 (conj coll [1 1] ) cj2  (conj cj1 [1 2]) ]
        (if (= (count cj1 ) (count cj2) )  :map :set)
        )

      )

    ))

(defcheck solution-5bfe0019
  #(cond
     (= (set %) %) :set
     (= (inc (count %)) (count (conj % {:t :w} {:t :w}))) :map
     (not (associative? %)) :list
     :else :vector))

(defcheck solution-5c8b62d9
  #(let
    [q [:1 0] x (into % [[:0 0] q q])]
     (cond (apply distinct? x) (if (x :0) :map :set)
           (= [:1 0] (first x)) :list
           true :vector)))

(defcheck solution-5c912859
  (fn [coll]
    (let
     [behave-like-map? (fn [c] (>= 1 (- (count (reduce conj c [[2 2] [2 3]])) (count c))))
      behave-like-set? (fn [c] (>= 1 (- (count (reduce conj c [2 2])) (count c))))
      behave-like-list? (fn [c] (every? #(= % (first (conj (conj c 0) %))) [1 2]))]
      (cond
        (behave-like-map? coll) :map
        (behave-like-set? coll) :set
        (behave-like-list? coll) :list
        :else :vector))))

(defcheck solution-5c9b67fe
  (fn [v]
    (cond
      (associative? v) (if (= (get (conj v [:z 10]) :z) 10) :map :vector)
      (< (count (conj v 1 1)) (+ 2 (count v))) :set

      :else :list)
    ))

(defcheck solution-5cc3152f
  (fn [coll]
    (let [result (conj (empty coll) [1 2] [1 2] [1 3])]
      (if (= 1 (count result))
        :map (if (= 2 (count result))
               :set (if (= [1 3] (first result))
                      :list :vector ))))))

(defcheck solution-5d2d5212
  (fn [S] (let [l (count S) tl (count (into S '([1 2] [1 3] [1 3])))] (cond (= (+ 1 l) tl) :map (= (+ 2 l) tl) :set (= (+ 3 l) tl) (if (associative? S) :vector :list)))))

(defcheck solution-5d9d4bbf
  (fn seq-type [coll]
    (let [base (empty coll)]
      (cond
        (= base {})  :map
        (= base #{}) :set
        (= base '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-5ddca564
  (fn checkit [coll]
    (cond
      (> (+ 2 (count coll))
        (count (conj (conj coll [:test1 :test2]) [:test1 :test2])))
      (if (get (conj coll [:test1 :test2]) :test1) :map :set)
      true
      (if (= (first (conj (conj coll :test1) :test2)) :test2) :list :vector)
      )))

(defcheck solution-5e1b5660
  #(cond
     (= % (conj % %)) :map
     (= (conj % nil) (clojure.set/union (set (conj % nil)) (set (conj % nil)))) :set
     (= :again (first (conj % :test :again))) :list
     :else :vector
     ))

(defcheck solution-5f2efd8
  (fn type- [x]
    "64. Write a function which takes a collection and returns one
  of :map, :set, :list, or :vector - describing the type of collection
  it was given."
    (if (associative? x)
      (if (reversible? x)
        :vector
        :map)
      (let [before (count x)
            after (count (conj (conj x :a) :a))]
        (if (= after (inc before))
          :set
          :list)))))

(defcheck solution-5f3ff80a
  (fn [x]
    (let [c (first (.toString x))]
      (case c
        \{ :map
        \[ :vector
        \# :set
        :list))))

(defcheck solution-5f4a2eb6
  (fn blackbox
    [col]
    (let [testCol (conj (empty col) [:a 1] [:a 2] [:b 3] [:b 3]), colCount (count testCol), firstElem (first testCol)]
      (if (= 2 colCount) :map (if (= 3 colCount) :set (if (= [:b 3] firstElem) :list :vector))
                         )
      )
    ))

(defcheck solution-5fa3fb83
  (fn [coll]
    (let [temp1 [:c 3]
          temp2 [:c 3]
          temp3 [:c 4]
          coll2 (conj coll temp1 temp2 temp3)]
      #_(pr coll2)
      (cond
        (= (count coll) (- (count coll2) 1)) :map
        (= (count coll) (- (count coll2) 2)) :set
        (= (first coll2) temp3) :list
        :else :vector))))

(defcheck solution-5fa7c80f
  (fn lookup-type [obj]
    (let [a      [1 1]
          result (conj obj a)]
      (cond
        (and (not (associative? obj)) (= (conj result a) result)) :set
        (and (associative? obj) (identical? (conj result a) result)) :map
        (and (not (associative? obj)) (identical? (first result) a)) :list
        (and (associative? obj) (identical? (last result) a)) :vector
        :else (throw (ex-info "Unknown collection type!" {}))))))

(defcheck solution-603a7c8f
  (fn [s0]
    (let [s (conj s0 [:querty "xxxxs"])]
      (if (= (apply list s) s)
        (if (= (first (conj s "xy!!$#")) "xy!!$#")
          :list
          :vector)
        (if (= (s (first s)) (first s))
          :set
          :map)))))

(defcheck solution-606be004
  #(let [e [:t 1]
         f [2 1]
         p (conj % e)
         a (conj p f)]
     (cond (:t p) :map
           (= 1 ((frequencies (conj p e)) e)) :set
           (= (nth a 0) f) :list
           (= (last a) f) :vector)))

(defcheck solution-611bd98a
  #(letfn
    [
     (isSet [x]
       (=
         (count (into x x))  (count x)
         )
       )
     (isMap [x]
       (=
         (get x 10000) 1
         )
       )
     (isVector [x]
       (=
         (first (conj (conj x 1) 2)) (first x)
         )
       )
     ]
     (cond
       (isMap (into % [[10000 1]]))  :map
       (isSet (into % [[1 1]]))  :set
       (isVector (into % [[1 1]])) :vector
       :else :list
       )
     ))

(defcheck solution-6227c0b3
  (fn [x]
    (let [[sym1 sym2] [(gensym) (gensym)]
          [elt1 elt2] [[sym1 sym1] [sym1 sym2]]
          aug (into x [elt1 elt1 elt2])]
      (cond
        (= (inc (count x)) (count aug)) :map
        (= (+ 2 (count x)) (count aug)) :set
        (= elt2 (first aug)) :list
        :default :vector))))

(defcheck solution-6309846d
  (fn [c]
    (cond
      (= c {}) :map
      (= c #{}) :set
      (let [fc (first c)]
        (if (coll? fc)
          (= (c (first fc)) (second fc)))) :map
      (= (count c) (count (conj c (first c)))) :set
      (= (first (conj c :flaga :flagb)) :flagb) :list
      (= (last (conj c :flaga :flagb)) :flagb) :vector
      )))

(defcheck solution-634078a2
  #(if (= (set %) %) :set
                     (if (reversible? %) :vector
                                         (if (associative? %) :map :list))))

(defcheck solution-6373a124
  (fn [seqs]
    (let [i1 [1 2]
          i2 [1 3]
          prev-cnts (count seqs)
          seq1 (conj seqs i1 i1)
          seq2 (conj seqs i1 i2)]
      (if (= (count seq1) (inc prev-cnts))
        (if (= (get seq1 i1) i1)
          :set
          :map)
        (if (= (last seq2) i2)
          :vector
          :list)))))

(defcheck solution-646d13b9
  #(if-let [t ({{} :map, #{} :set} (empty %))]
     t
     (if (-> % (conj 1) (conj 0) first zero?)
       :list :vector)))

(defcheck solution-64e2f85d
  #(let [c (into (empty %) [[0 0] [0 1]])]
     (cond (= 1 (count c)) :map
           (get c 0) :vector
           (= [0 0] (first c)) :set
           true :list)))

(defcheck solution-654454c5
  (fn [x]
    (cond
      (= (get (conj x [:t "t"]) :t) "t") :map
      (= (get (conj x :t) :t) :t) :set
      (= (first (conj (conj x :a) :b)) :b) :list
      (= (last (conj (conj x :a) :b)) :b) :vector)))

(defcheck solution-65f925f8
  (fn [input]
    (if (= input (vec input))
      ;; decide betweeen vector or list
      (if (= (conj input "A" "B")
            (conj (apply list input) "A" "B"))
        :list
        :vector)
      ;; decide between set or map
      (if (= input (apply hash-set input))
        :set
        :map))))

(defcheck solution-66587295
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (ifn? %) :set
     :else :list))

(defcheck solution-66671924
  (fn [xs] (let [f (conj xs [:foo :bar] [:foo :bas]) h (first f)]
             (cond (get (conj xs [:foo :bar] [:foo :bas]) :foo nil) :map
                   (= (get (frequencies (conj xs :foo :foo)) :foo) 1) :set
                   (= :foo (first (conj xs :bar :foo))) :list
                   :else :vector))))

(defcheck solution-66787782
  (fn [v]
    (let [ev    (empty v)
          conj2diff (conj (conj ev [1 1]) [1 2])]
      (if (= (count conj2diff) 1)
        :map
        (let [conj2same (conj (conj ev [1 1]) [1 1])]
          (if (= (count conj2same) 1)
            :set
            (if (= (first conj2diff) [1 1])
              :vector
              :list)))))))

(defcheck solution-66b99197
  (fn ftype [x]
    (let [fchar (first (str x))]
      (cond (= \# fchar) :set
            (= \[ fchar) :vector
            (= \{ fchar) :map
            :else :list
            )
      )
    ))

(defcheck solution-66c2475a
  (fn [x]
    (let [kw :wiggawoooga-fnordwimple
          s (conj x [kw 1] [kw 1] [kw 2])]
      (cond
        (= 2 (kw s)) :map
        (= 2 (- (count s) (count x))) :set
        (= [kw 2] (last s)) :vector
        (= [kw 2] (first s)) :list))))

(defcheck solution-66ecf1a7
  (fn blackbox [x]
    (cond
      (reversible? x) :vector
      (associative? x) :map
      (apply distinct? (into x [1 1])) :set
      :else :list)
    ))

(defcheck solution-67b4557
  #(if (associative? %)
     (if (= [] (empty %)) :vector :map)
     (if (= #{} (empty %)) :set :list)))

(defcheck solution-67ce59ea
  (fn [x]
    (let [z (into x [[x 1] [x x] [x 1] [x x]])]
      ({1 :map 2 :set 4 (if (= [x x] (first z)) :list :vector)}
       (- (count z) (count x))))))

(defcheck solution-6870d462
  (fn
    [x]
    (let
     [i (count x)]
      (cond
        (= (inc i) (count (conj x {:c 3} {:c 4}))) :map
        (= (inc i) (count (conj x 21 21))) :set
        (= 22 (last (conj (conj x 21) 22))) :vector
        :else :list
        ))))

(defcheck solution-68eb8c22
  (fn black-box-test [s]
    (let [marker [:m2 :flag]
          t (conj s [:marker :marker])
          t2 (conj t marker)]
      (cond
        (= (get t :marker) :marker) :map
        (= (get t2 marker) marker) :set
        (= (last t2) marker) :vector
        (= (first t2) marker) :list
        :else :set))))

(defcheck solution-696075ba
  (fn what?[coll]
    (if (not (associative? coll)) (if (= (conj coll 1) (conj (conj coll 1) 1)) :set :list)
                                  (if(reversible? coll) :vector :map))))

(defcheck solution-6990197f
  (fn [s]
    (if (= (+ 2 (count s)) (count (conj s {2 3 4 5})))
      :map
      (if (= (+ 1 (count s)) (count (conj (conj s nil) nil)))
        :set
        (if (= 2 (first (conj (conj s 1) 2)))
          :list
          :vector)))))

(defcheck solution-69c85b97
  (fn [coll]
    (cond (= (clojure.core/str coll) "()") :list
          (= (clojure.core/str coll) "[]") :vector
          (= (clojure.core/str coll) "#{}") :set
          (= (clojure.core/str coll) "{}") :map
          (= (empty coll) (empty {:a 1})) :map
          (= (empty coll) ())
          (cond (= (first (conj coll :test)) :test) :list
                :else :vector)
          (= (empty coll) (empty #{1})) :set
          :else :unknown)))

(defcheck solution-69d4dea3
  (fn [coll]
    (let [newcoll (into coll [[:q47xhd1 2] [:zix42d 1]])]
      (if (= newcoll (seq newcoll))
        (if (= (last newcoll) [:zix42d 1]) :vector :list)
        (if (= newcoll (set newcoll)) :set :map)
        ))))

(defcheck solution-6b3afcfe
  #(let [x :x
         y :y
         p [x y]
         c (conj % p)]
     (cond
       (= y (get c x)) :map
       (= p (get c p)) :set
       (= x (last (conj c x))) :vector
       :else :list
       )))

(defcheck solution-6b76202f
  #(let [mt-coll (empty %)]
     (cond (and (= '() mt-coll) (reversible? mt-coll)) :vector
           (= [] mt-coll) :list
           (= #{} mt-coll) :set
           (= {} mt-coll) :map)))

(defcheck solution-6b8a133c
  #(if (= (get (conj % [:a :b]) :a) :b) :map
                                        (cond (= (count %) (dec (count (conj % :x :x)))) :set
                                              (= :y (last (conj % :x :y))) :vector
                                              :else :list)))

(defcheck solution-6beed795
  (fn [s]
    (let [dum (conj s [:xxxx 0])
          res (conj dum [:z 9])
          upd (conj res [:z 9])]
      (cond
        (and (= res upd) (= (get res [:z 9]) [:z 9])) :set
        (= (get res :z) 9) :map
        (= (last res) [:z 9]) :vector
        (= (first res) [:z 9]) :list))))

(defcheck solution-6c4681d3
  (fn coll-type [coll]
    (let [cnt (count coll)
          c (conj coll [:a 1] [:a 1] [:b 2])
          f (first coll)
          l (last coll)]
      (cond
        (= (:b c) 2) :map
        (= (+ cnt 2) (count c)) :set
        (= (first c) [:b 2]) :list
        :else :vector))))

(defcheck solution-6c69b0e4
  #(case (str (first (str %)))
     "{" :map
     "#" :set
     "[" :vector
     :list))

(defcheck solution-6cc61adc
  (fn [coll]
    (let [emptycoll (empty coll)
          testcoll (conj emptycoll (first emptycoll))]
      #_(prn testcoll (count testcoll))
      (if (= (count testcoll) 0)
        :map
        (let [testcoll_1 (conj testcoll (first testcoll))]
          (if (= (count testcoll_1) (count testcoll))
            :set
            (let [testcoll_2 (conj emptycoll 1)]
              (if (= (get testcoll_2 0) 1)
                :vector
                :list))))))))

(defcheck solution-6d15fe57
  (fn [t]
    (let [k (empty t)]
      (cond
        (= :b (:a (into k [[:a :b]]))) :map
        (= [:b :a] (into k [:a :b])) :list
        (= (conj k :a) (conj k :a :a)) :set
        :else :vector))))

(defcheck solution-6d7818ac
  #(let [v [0 0]
         f (fn [])]
     (cond
       (not= (conj % v) (conj % v v))
       (if (= f (first (conj % 1 f))) :list :vector)
       (= ((conj % [0 :a]) 0) :a) :map
       :else :set)))

(defcheck solution-6d7a734b
  (fn [coll]
    (let [bos (empty coll)]
      (cond (= bos {}) :map
            (= bos #{}) :set
            :else (if (reversible? coll) :vector :list)))))

(defcheck solution-6d7eef94
  #(or ({{} :map #{} :set} (empty %))
       ({1 :vector} (first (conj (empty %) 1 2)) :list)))

(defcheck solution-6db3db6d
  (fn foo [x]
    (cond
      (= (conj x [:wibble :jelly]) (conj x [:wibble :wobble] [:wibble :jelly])) :map
      (= (conj x :wibble) (conj x :wibble :wibble)) :set
      (= (first (conj x :wobble :wibble)) :wibble) :list
      (= (last (conj x :wobble :wibble)) :wibble) :vector
      :else :wtf)))

(defcheck solution-6dfe5afd
  (fn [arg]
    (if (associative? arg)
      (if (reversible? arg) :vector :map)
      (if (= 2 (- (count (conj (conj arg 1) 1)) (count arg))) :list :set))))

(defcheck solution-6e166d76
  #(let [tv [:foo :bar]
         coll (conj % tv)]
     (if (= coll (conj coll tv))
       (if (:foo coll) :map :set)
       (if (= :foo (first (conj coll :foo))) :list :vector))))

(defcheck solution-6ea5aea3
  (fn [coll]
    (let [coll' (conj coll [:a 1])]
      (if (= coll' (conj coll' [:a 1]))
        (if (= (coll' [:a 1]) [:a 1])
          :set
          :map)
        (if (= (into coll [1 2 3]) (concat coll [1 2 3]))
          :vector
          :list)))))

(defcheck solution-6f168e31
  (fn mytype[s] (if (associative? s) (if (reversible? s) :vector :map) (if (= #{} (empty s)) :set :list))))

(defcheck solution-6f478fdf
  (fn [o]
    (cond
      (reversible? o) :vector
      (associative? o) :map
      (= (count o) (dec (count (into o ['asdf 'asdf])))) :set
      :else :list
      )))

(defcheck solution-6f5afb9b
  (fn [s]
    (cond
      (= {} (empty s)) :map
      (= #{} (empty s)) :set
      (= [0 1] (conj (empty s) 0 1)) :vector
      (= '(1 0) (conj (empty s) 0 1)) :list)))

(defcheck solution-6f6d714a
  (fn [coll]
    (cond
      (associative? coll) (if (reversible? coll) :vector :map)
      (= (get (conj coll :a) :a) :a) :set
      :else :list)))

(defcheck solution-6fa4b971
  #(let [
         e1 [:key :v1]
         e2 [:key :v2]
         t (conj % e1 e1 e2)
         ts (count t)
         s (count %)]

     (cond
       (and (= (+ 1 s) ts) (contains? t :key)) :map
       (= (+ 2 s) ts) :set
       (= (last t) e2) :vector
       :else :list)
     ))

(defcheck solution-6fa5aa4
  (fn my-type [coll]
    (cond
      (= (drop-last (conj coll [1 2] [3 4])) (conj coll [1 2])) :vector
      (= (rest (conj coll [1 2] [3 4])) (conj coll [1 2])) :list
      (contains? (conj coll [1 2]) [1 2]) :set
      true :map)))

(defcheck solution-6fb1c03f
  (fn [coll]
    (let [a (atom 0)
          b (atom 0)]
      (letfn [(is-a-set?  [coll] (= (count coll) (dec (count (conj coll a a)))))
              (is-a-list? [coll] (identical? b (first (conj coll a b))))
              (is-a-vec?  [coll] (identical? b (last (conj coll a b))))
              (is-a-map?  [coll] (identical? b (get (into coll {a b}) a)))]
        (cond (is-a-map? coll) :map
              (is-a-set? coll) :set
              (is-a-vec? coll) :vector
              (is-a-list? coll) :list)))))

(defcheck solution-6fe26031
  (fn [c] (condp =
                 (conj (conj (empty c) [1 1]) [2 2])
            #{[1 1][2 2]} :set
            {1 1 2 2} :map
            [[1 1][2 2]] :vector
            '([2 2][1 1]) :list)))

(defcheck solution-704e209d
  #(let [a (conj (conj % [:b 2]) [:a 2])]
     (if (== (count a) (count (conj a [:a 2])))
       (if (and (not (nil? (:a a))) (== 2 (:a a)))
         :map
         :set)
       (if (= [:a 2] (last a))
         :vector
         :list))))

(defcheck solution-70589359
  (fn myType [coll] (if (= (get (conj coll {0 2}) 0) 2)
                      :map
                      (if (not= (+ (count coll) 2) (count (conj coll 1 1)))
                        :set
                        (if (empty? coll)
                          (if (= (first (conj coll 1 2)) 2)
                            :list
                            :vector
                            )
                          (if (let [v (vector (first coll) (last coll))] (= v (first (conj coll v))) )
                            :list
                            :vector
                            )
                          )
                        )
                      )
    ))

(defcheck solution-709e6994
  #({\[ :vector \( :list \{ :map \# :set \c :list} (first (str %))))

(defcheck solution-719843a3
  (fn [coll]
    (let [coll1 (empty coll)
          a {:a 1 :b 2}
          b {:c 3 :d 4}
          coll2 (conj (conj (conj coll1 a) a) b)]
      (cond
        (= (count coll2) 4) :map
        (= (count coll2) 2) :set
        (= (count coll2) 3) (if (= (last coll2) b)
                              :vector
                              :list)))))

(defcheck solution-71c3ff4f
  (fn typeX [x] (let [s (str x)] (case (first s) \{ :map \[ :vector \# :set :list))))

(defcheck solution-71e75b10
  (fn [a]
    (cond
      (= #{} (empty a)) :set
      (=  {} (empty a)) :map
      (= 1 (first (conj (empty a) 1 2))) :vector
      :else                              :list)))

(defcheck solution-71f03545
  #(case (first (str %))
     \[ :vector
     \{ :map
     \# :set
     :list))

(defcheck solution-721dad72
  #(let [c (conj (empty %1) [:a :b] [:c :d])]
     (cond
       (= (get c [:a :b]) [:a :b])
       :set

       (= (get c :a) :b)
       :map

       (= (first c) [:c :d])
       :list

       (= (first c) [:a :b])
       :vector)))

(defcheck solution-72387e53
  (fn [s]
    (let [s' (into s [[:a 1] [:a 1]])]
      (if (not= (count s') (+ (count s) 2))
        (if (:a s') :map :set)
        (let [s''  (into s [2 3])]
          (if (= s'' (butlast (conj s'' 1))) :vector :list))))))

(defcheck solution-7239303b
  (fn my-ident [x]
    (cond
      (= (conj x {}) x) :map
      (= (conj (conj x "mytest") "mytest") (conj x "mytest")) :set
      (= (last (conj (conj x "mytest1") "mytest2")) "mytest2") :vector
      (= (first (conj (conj x "mytest1") "mytest2")) "mytest2") :list
      )))

(defcheck solution-72559ad2
  (fn[c]
    (let [tc (into c '([:t :t][:t :t][:t :s]))]
      (cond
        (= (into  c  '([:t :s])) tc)        :map
        (= (into  c  '([:t :t][:t :s])) tc) :set
        (= (first tc)  [:t :s])             :list
        (= (last  tc)  [:t :s])             :vector))))

(defcheck solution-72b647f
  #(let [x (conj % [-1 -2])]
     (cond (= -2 (get x -1)) :map
           (= [-1 -2] (get x [-1 -2])) :set
           (= [-1 -2] (get x (dec (count x)))) :vector
           :else :list)))

(defcheck solution-72c0763b
  (fn test_type [c]
    (case (empty c)
      {} :map
      #{} :set
      (if (= (first (conj (conj c 23) 42)) 42) :list :vector))))

(defcheck solution-7358cac4
  (fn [c]
    (let [x [:a 1]
          e (into (empty c) [x x [:b 2]])]
      (cond
        (= 1 (:a e)) :map
        (= 2 (count e)) :set
        (= x (first e)) :vector
        :else :list))))


(defcheck solution-741000a9
  (fn [t]
    (if (= (vec t) t)
      (if (= :blah (first (conj (conj t :blahblah) :blah)))
        :list
        :vector)
      (if ((conj t [:blah1 :blah2]) :blah1)
        :map
        :set))))

(defcheck solution-741dbf9c
  (fn [col]
    (cond
      (associative? col) (if (= (conj col [1 1] [1 1]) (conj col [1 1])) :map :vector)
      (= (conj col 1 1) (conj col 1)) :set
      :else :list)))

(defcheck solution-7461b40a
  (fn [c]
    (let [d (conj c {0 1} {0 2} {0 1})]
      (cond
        (= (+ 1 (count c)) (count d)) :map
        (= (+ 2 (count c)) (count d)) :set
        (= (cons {0 3} d) (conj d {0 3})) :list
        true :vector))))

(defcheck solution-74a84d0d
  #(let [firstchar (first (str %))]
     (cond
       (= firstchar \{) :map
       (= firstchar \[) :vector
       (= firstchar \#) :set
       :else :list)))

(defcheck solution-75641e3e
  (fn blackbox [x]
    (if (= (count (conj (empty x) [:a 1] [:a 1])) 2)
      (if (= [:b 2] (first (conj (conj (empty x) [:a 1]) [:b 2])))
        :list
        :vector)
      (if (associative? x)
        :map
        :set))))

(defcheck solution-759498fa
  (fn f [st]
    (let [ans (empty st)]
      (if (= 1 (count (into ans [[1 2] [1 3]])))
        :map
        (if (= 1 (count (into ans [[1 2] [1 2]])))
          :set
          (let [aa (conj ans 1 2)]
            (if (= 1 (first aa))
              :vector
              :list
              )))))))

(defcheck solution-75d2d0ce
  (fn blackBox [coll]
    (cond
      (= 1 (:a (into coll {:a 1}))) :map
      (= :a (:a (conj coll :a))) :set
      (= :foo (first (conj coll :bar :foo))) :list
      :else :vector)))

(defcheck solution-75d87ea4
  (fn [x]
    (if (:a (conj x [:a :b]))
      :map
      (let [x1 (conj x 1)
            x2 (conj x1 1)]
        (if (= (count x1) (count x2))
          :set
          (if (= 2 (-> x (conj 1) (conj 2) (first)))
            :list
            :vector))))))

(defcheck solution-76888a89
  (fn [v]
    (let [ext1 (conj v [1 1])
          len1 (count ext1)
          ext2 (conj ext1 [1 1])
          len2 (count ext2)
          ext3 (conj ext2 [1 7])]
      (if (= len1 len2)
        (if (contains? ext3 [1 1])
          :set
          :map)
        (if (= (first ext3) [1 7])
          :list
          :vector)))))

(defcheck solution-76a84330
  (fn
    [s]
    (cond
      (= :v (:k (conj s {:k :v})))
      :map
      (= :v (:v (conj s :v)))
      :set
      (= :a (last (conj (conj s :b) :a)))
      :vector
      :else :list)))

(defcheck solution-771322d8
  (fn [c]
    (let [c (conj c [:a 1])]
      (if (= (count (conj c (first c))) (count c))
        ;; Map or set
        (if (get c (first c))
          :set
          :map)
        ;; List or vector
        (if (= (first c) (first (conj c c)))
          :vector
          :list)))))

(defcheck solution-77bb3af2
  (fn black-box [xs]
    (cond (= (empty xs) {}) :map
          (= (empty xs) #{}) :set
          (= (empty xs) '())
          (if (reversible? xs) :vector :list))))

(defcheck solution-77f8bd49
  #(cond
     (not (ifn? %)) :list
     (.startsWith (str %) "[") :vector
     (.startsWith (str %) "#") :set
     :else :map
     ))

(defcheck solution-78727266
  (fn bbtest [s]
    (let [p ((juxt associative? reversible? ifn?) s)]
      (cond (= [true true true] p) :vector
            (= [true false true] p) :map
            (= [false false true] p) :set
            :else :list
            )
      )
    ))

(defcheck solution-79330d38
  (fn [coll]
    (let [e (empty coll)]
      (cond
        (= {} e) :map
        (= #{} e) :set
        (reversible? e) :vector
        :else :list))))

(defcheck solution-7976311c
  (fn [coll]
    (let [sentinel (keyword (gensym))
          sentinel2 (keyword (gensym))
          pair [sentinel sentinel]
          pair2 [sentinel sentinel2]]
      (if (= (count (conj coll pair)) (count (conj (conj coll pair) pair)))
        (let [once (conj coll pair)
              twice (conj once pair2)]
          (if (= (count once) (count twice))
            :map
            :set))
        (if (= (first (into coll [sentinel sentinel2])) sentinel2)
          :list
          :vector)))))

(defcheck solution-79c93ca4
  (fn [xs]
    (let [elem (cond
                 (= (first xs) [:a :a]) [:b :b]
                 :else [:a :a])
          next-elem [:c :c]
          newxs (-> xs (conj elem) (conj next-elem) (conj next-elem))]
      (cond
        (and (= (+ (count xs) 3) (count newxs))
             (= (first newxs) next-elem)) :list
        (= (get newxs (first elem)) (second elem)) :map
        (= newxs (conj newxs next-elem)) :set
        :else :vector))))

(defcheck solution-79da5ed4
  (fn [s](
           let [
                i (into s [[100 200] {:200 300}])
                j (not (nil?(get i 1)))
                k (- (count i) (count s))
                m (= 200 (get i 100))
                n (= s (set s))
                ](cond m :map
                       n :set
                       j :vector
                       :else :list))))

(defcheck solution-7a066578
  #(let [s (empty %)]
     (cond (= 1 (count (conj s [1 2] [1 3]))) :map,
           (= 1 (count (into s [1 1]))) :set,
           (= 1 (first (into s [1 2]))) :vector,
           :default :list)))

(defcheck solution-7a669cd3
  (fn [s]
    (let [e (empty s)]
      (cond
        (= e {}) :map
        (= e #{}) :set
        (= 1 (first (conj e 1 2))) :vector
        :else :list))))

(defcheck solution-7a959fc8
  (fn [xs]
    (let [f (first (map char (str xs)))]
      (cond
        (= f \{) :map
        (= f \#) :set
        (= f \() :list
        (= f \c) :list
        (= f \[) :vector))))

(defcheck solution-7b42238a
  (fn bbtest [item]
    (let [sitem (pr-str item)]
      (cond
        (.startsWith sitem "{") :map
        (.startsWith sitem "(") :list
        (.startsWith sitem "[") :vector
        (.startsWith sitem "#{") :set
        ))))

(defcheck solution-7b85c6d0
  #(if (associative? %)
     (let [s (conj % [:a :b])]
       (if (contains? s :a)
         :map
         :vector))
     (let [n (count %)
           s (conj % :a :a)]
       (if (= (count s) (+ n 2))
         :list
         :set))))

(defcheck solution-7b9c6a87
  #(if (associative? %)
     (if (reversible? %) :vector :map)
     (if (= (into [] %) %) :list :set)))

(defcheck solution-7bcc8473
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= #{} base) :set
        (= {} base) :map
        (= '() base) (if (reversible? base) :vector :list)))))

(defcheck solution-7c2deb81
  (fn [t]
    (if (associative? t)
      (if (= t (vec t)) :vector :map)
      (if (= t (set t)) :set :list))))

(defcheck solution-7c51a979
  #(
    {\( :list \{ :map \[ :vector \# :set}
    (->> %
      pr-str ; HA
      first)))

(defcheck solution-7c918bac
  #(if (= (conj % [% %] [% %])
         (conj % [% %]))
     (if ((conj % [% %]) %)
       :map
       :set)
     (if (= (conj % % [%])
           (cons [%] (cons % %)))
       :list
       :vector)))

(defcheck solution-7d2e3733
  (fn [x]
    (let [x' (conj x [:a :b] [:a :b])]
      (if (= (+ (count x) 2)(count x'))
        (if (= (conj x' :a) (cons :a x'))
          :list
          :vector)
        (if (= (x' :a) :b)
          :map
          :set)))))

(defcheck solution-7d9574b8
  (fn [coll]
    (if (reversible? coll)
      :vector
      (condp = (empty coll)
        {}   :map
        #{} :set
        ()  :list
        ))))

(defcheck solution-7e91b1d0
  (fn bbt [coll]
    (cond
      (= "t" (get (conj coll {:t "t"}) :t)) :map
      (= :t (get (conj coll :t) :t)) :set
      (= :a (first (conj (conj coll :b) :a))) :list
      (= :a (last (conj (conj coll :b) :a))) :vector )))

(defcheck solution-7ece8dc
  #((fn [x](if (= x \{)

             :map

             (if (= x \#)

               :set

               (if (= x \[)

                 :vector

                 :list)))) (first (seq (str %)))))

(defcheck solution-7f0f2a76
  (fn [coll] (let [basis (empty coll)] (cond (= basis {}) :map
                                             (= basis #{}) :set
                                             (not (get (conj coll 0) 0)) :list
                                             (get (conj coll 0) 0) :vector))))

(defcheck solution-7ff01437
  (fn [c]
    (let [e (str (empty c))]
      (cond
        (= "{}" e)  :map
        (= "[]" e)  :vector
        (= "#{}" e) :set
        :else       :list
        ))))

(defcheck solution-80185385
  (fn [c]
    (let [cc (conj (empty c) [1 "a"] [1 "a"] [2 "b"] )]
      (if (= 2 (count cc))
        (if (nil? (cc 1)) :set :map)
        (if (= 1 (ffirst cc)) :vector :list)))))

(defcheck solution-805a94f7
  #(let [s (conj % {:foo :bar} {:baz :bat})] ; test data
     (if (= 1 (count (flatten [s]))) ; "flatten" on non seqs => ()
       (if (= (s :foo) :bar)
         :map
         :set)
       (if (= (first s) {:baz :bat}) ; seq => vector or list
         :list ; lists "conj" to the beginning
         :vector))))

(defcheck solution-805a9703
  (fn [coll]
    (cond (= 3 (:b (-> coll (conj {:b 3} ))) ) :map
          (= (inc (count coll)) (count (-> coll (conj :a) (conj :a)))) :set
          (= :a (last (-> coll (conj 7) (conj :a)))) :vector
          (= "a" (first (-> coll (conj 1) (conj "a")))) :list
          :else :unknown
          )))

(defcheck solution-805e89a3
  (fn bb-test [c]
    (let [ec (empty c)
          ev (str ec)]
      (cond
        (= {} ec) :map
        (= #{} ec) :set
        (= "[]" ev) :vector
        :else :list))))

(defcheck solution-806d5581
  (fn [x]
    (cond
      (= (conj x {}) x) :map
      (empty? x) (cond
                   (=  x #{}) :set
                   (= (conj (conj x 0) 1) [0 1]) :vector
                   :else :list)
      (= (set x) x) :set
      (= (first (conj x x)) x) :list
      :else :vector)))

(defcheck solution-810d9946
  (fn [in]
    (let [l (conj in [:a in] [:c in])]
      (if
       (=
         l
         (conj l [:a in]))
        (if
         (contains? l [:c in])
          :set
          :map)
        (if
         (=
           (first l)
           [:c in])
          :list
          :vector)))))

(defcheck solution-812f0c01
  #((zipmap (map str [{} #{} () []]) [:map :set :list :vector]) (str (empty %))))

(defcheck solution-8219b9fd
  (fn [coll]
    (cond
      (:key (conj coll [:key :_]))
      :map
      (:item (conj coll :item))
      :set
      (= (first (conj coll :_ :item)) :item)
      :list
      :else
      :vector)))

(defcheck solution-82d025fe
  (fn [coll] (let [gen (gensym), conjed (conj coll [:my-key gen]), conjed2 (conj conjed [:my-key gen])]
               (if (= (count conjed) (count conjed2))
                 (if (contains? conjed [:my-key gen])
                   :set
                   :map)
                 (if (= (first (conj conjed [gen gen])) [gen gen])
                   :list
                   :vector)))))

(defcheck solution-8335ca07
  (fn [coll]
    (let [coll (empty coll)]
      (cond
        (= {}  coll) :map
        (= #{} coll) :set
        (= (first (into coll [1 2])) 1) :vector
        :else :list))))

(defcheck solution-8344fd99
  (fn f [x]
    (if (= x (set x)) :set
        (let [o (conj (empty x) [1 2] [3 4])]
          (cond
            (= 2 (get o 1)) :map
            (= [1 2] (first o)) :vector
            :else :list)))))

(defcheck solution-8374e7ff
  (fn [cand]
    (let [g1 (gensym)
          g2 (gensym)
          new-cand (conj (conj cand [g1 g2]) [g1 g2])]
      (if (= 1 (- (count new-cand) (count cand)))
        (if (= [g1 g2] (get new-cand [g1 g2]))
          :set
          :map)
        (if (= (first (conj new-cand [g2 g1])) [g2 g1])
          :list
          :vector)))))

(defcheck solution-8447dc29
  (fn
    [coll]
    (let [c (count coll)
          n (into coll [[1 1] [1 2] [1 3] [1 1] [1 4]])
          cn (count n)]
      (cond
        (= (- cn c) 1) :map
        (= (- cn c) 4) :set
        (= (first n) [1 4]) :list
        :else :vector))))

(defcheck solution-84a16153
  (fn checktype [o]
    (if (= o (conj o {}))
      :map
      (let [test (into o [:a])]
        (cond
          (= (count test) (count (conj test :a)))
          ,,:set
          (not= (first (conj test 10)) 10)
          ,,:vector
          :t
          ,,:list)))))

(defcheck solution-84a8e8fc
  (fn [col] (let [c1 (count col)
                  c2 (count (into col [[1 1] [1 1]]))]
              (if (< c2 (+ c1 2))
                (let [nc (into col [[:a :b]])]
                  (if (= :b (:a nc)) :map :set))
                (let [e1 (str :a (first col) (last col))
                      e2 (str :b (first col) (last col))
                      nc (into col [e1 e2])]
                  (if (= e2 (first nc)) :list :vector))))))

(defcheck solution-84aeaae1
  (fn gi [o]
    (cond
      (reversible? o) :vector
      (associative? o) :map
      (= (empty o) ()) :list
      (= (empty o) #{}) :set)))

(defcheck solution-84b561c0
  (fn find-type[coll]
    (get [:map :set :list :vector] (.indexOf "{#([" (str (get (print-str coll) 0))))))

(defcheck solution-862017dd
  (fn [vs]
    (let [e (empty vs)]
      (cond
        (= e #{}) :set
        (= e {}) :map
        (= e ()) (if (= [5 6] (conj e 5 6))
                   :vector
                   :list)))))

(defcheck solution-8812f60
  (fn type' [o]
    (cond
      (= o (set o)) :set
      (not (= (map identity o) o)) :map
      (= "hoi" (first (conj o "!!!" "hoi"))) :list
      :else :vector)))

(defcheck solution-8828f996
  #(let [c (conj (empty %1) [1 2] [1 3])]
     (cond (= c {1 3}) :map
           (= c '([1 3] [1 2])) :list
           (= c [[1 2] [1 3]]) :vector
           (= c #{[1 2] [1 3]}) :set)))

(defcheck solution-893a5cab
  (fn[x]
    (let [t (conj (empty x) [:a :b] [:c :d])]
      (cond
        (:a t) :map
        (get t 0) :vector
        (get t [:a :b]) :set
        :else :list))))

(defcheck solution-8a8fa3ec
  (fn [x]
    (let [y (conj x [:a 1])]
      (if (= 1 (count (flatten [y])))
        (if (= 1 (y :a))
          :map
          :set)
        (if (= :first (first (conj y :first)))
          :list
          :vector)))))

(defcheck solution-8b201160
  (fn [obj]
    (cond (= (conj obj obj) obj)
          :map
          (= (conj obj obj) (conj (conj obj obj) obj))
          :set
          (= (first (conj (conj obj true) false)) false)
          :list
          :else
          :vector)))

(defcheck solution-8b7c7253
  (fn [c]
    (cond
      (= (get (conj c [:a :b]) :a) :b) :map
      (reversible? c) :vector
      (= :a (:a (into c [:a]))) :set
      (nil? (:a (into c [:a]))) :list)))

(defcheck solution-8c2d3783
  (fn [coll]
    (let [x [:hoopy :frood]]

      ;; If we add x twice, but count increases by only 1
      ;; the collection must be associative.
      (if (= (inc (count coll)) (count (into coll [x x])))

        ;; associative - if coll is a map, the first element of our known
        ;; sequence will become a key when the sequence is conj-ed to the
        ;; collection
        (if (:hoopy (conj coll x))
          :map
          :set)

        ;; sequential - it's important to know where your towel is:
        ;; if coll is a list, :towel will be added to the front.
        ;; if coll is a vector, :towel will be a added to the back.
        ;; It's necessary to add two items so first and last aren't
        ;; referring to the same item.
        (if (= :towel (first (conj (conj  coll :zarquod) :towel)))
          :list
          :vector)))))

(defcheck solution-8c3d5438
  (fn [c]
    (let [u :u-17863182691 q :q-235238r3619
          uu [u u] qq [q q]
          c' (conj (conj c uu) qq)
          ]
      (cond (= u  (get   c' u )) :map
            (= uu (get   c' uu)) :set
            (= qq (last  c'   )) :vector
            (= qq (first c'   )) :list
            :else                :?))))

(defcheck solution-8c4ef10
  (fn black-box-testing [coll]
    (let [empty-coll (empty coll)]
      (cond
        (= {} empty-coll) :map
        (= #{} empty-coll) :set
        (= [1 2] (conj empty-coll 1 2)) :vector
        :else :list))))

(defcheck solution-8c9ef01f
  (fn my-inspect
    [coll]
    (let [newcoll (conj coll [1 1] [1 1] [1 2])]
      (cond
        (= (count newcoll) (inc (count coll))) :map
        (= (count newcoll) (+ (count coll) 2)) :set
        (= (first newcoll) [1 2]) :list
        (= (last newcoll) [1 2]) :vector))))

(defcheck solution-8d97ed4e
  (fn [xs] (let [test {:X :Y} xss (conj xs nil test) f (first xss) l (last xss)] (cond (= f test) :list (= l test (xss (dec (count xss)))) :vector  (= (xss :X) :Y) :map (= (xss test) test) :set))))

(defcheck solution-8d9bd52c
  (fn [x]
    (if (empty? x)
      (recur (into x [[1 2]]))
      (let [d (into x x)]
        (if (= d x)
          (if (x (first x)) :set :map)
          (if (= :z (last (conj x :z))) :vector :list))))))

(defcheck solution-8df9dbc
  (fn [o]
    (letfn [(c34 [s] (conj s [3 4]))
            (c35 [s] (conj s [3 5]))
            (c56 [s] (conj s [5 6]))]
      (cond
        (= (count (c35 (c34 o))) (count (c34 o))) :map
        (= (count (c34 (c34 o))) (count (c34 o))) :set
        (= [5 6] (first (c56 (c34 o)))) :list
        (= [5 6] (last (c56 (c34 o)))) :vector
        )
      )
    ))

(defcheck solution-8e46e4ca
  (fn get-label[x]
    (letfn [(is-map [y]
              (= 2 (count
                     (conj (empty x) {:a 2 :b 3}))))
            (is-set [z]
              (= 1 (count (conj (empty x) 3 3 3))))
            ]
      (cond
        (is-map x) :map
        (associative? x) :vector
        (is-set x) :set
        :else :list))))

(defcheck solution-8eb49c15
  (fn ident [l]
    (let [l (conj l [(gensym) (gensym)])
          sym (gensym)
          nl (conj l [sym sym])]
      (if (= (sym nl) sym)
        :map
        (if (= (conj nl [sym sym]) nl)
          :set
          (if (= (rest nl) l)
            :list
            :vector))))))

(defcheck solution-8eceaf2
  (fn [coll]
    (let [empty-coll (empty coll)]
      (cond
        (= {} empty-coll) :map
        (= #{} empty-coll) :set
        (= () empty-coll) (if (associative? coll) :vector :list)))))

(defcheck solution-8ed9b6e3
  (fn [seq]
    ({\[ :vector
      \# :set
      \{ :map
      \( :list
      \c :list } (nth (.toString seq) 0))))

(defcheck solution-8eef17ef
  (fn check-box [coll]
    (cond
      (= :v (get (conj coll [:k :v]) :k))     :map
      (= (conj coll "s") (conj coll "s" "s")) :set
      (= "l" (first (conj coll "v" "l")))     :list
      (= "v" (last (conj coll "l" "v")))      :vector
      )))

(defcheck solution-8ef0b9e6
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     :else (let [k (conj % 1 2)]
             (if (= (first k) 2)
               :list
               :set
               )
             )
     ))

(defcheck solution-8f2260d6
  (fn p65
    [coll]
    (cond
      (= 0 (- (count coll) (count (conj coll {})))) :map
      (= :x (get  (conj coll :x) (count coll))) :vector
      (= :x (get (conj coll :x) :x)) :set
      :else :list)))

(defcheck solution-8f2b8d8e
  (fn [seq]
    (let [seq_len (count seq)]
      (if (= seq_len (count (conj seq seq))) :map
                                             (if (= (inc seq_len) (count (conj seq seq seq))) :set
                                                                                              (if (= seq (first (conj seq 1 seq))) :list
                                                                                                                                   :vector)
                                                                                              )
                                             )
      )
    ))

(defcheck solution-8f409e67
  (fn st [d]
    (let [c (conj d [:c 3])]
      (cond
        (not (nil? (:c c))) :map
        (= (count (conj c (first c))) (count c)) :set
        (= (last (conj c 42)) 42) :vector
        :else :list))))

(defcheck solution-8f583a66
  (fn [coll]
    (let [t (into coll [[:x 99][:y 98]])]
      (cond (= (get t :x) 99) :map
            (= (get t [:x 99]) [:x 99]) :set
            (= (take 2 t) [[:y 98] [:x 99]]) :list
            (= (take-last 2 t) [[:x 99] [:y 98]]) :vector
            :else nil))))

(defcheck solution-8fa016b9
  #(let [x (conj % {:b 1} {:a 1} {:a 1})]
     (cond
       (= 1 (get x :a)) :map
       (= {:a 1} (get x {:a 1})) :set
       (= {:a 1} (first x)) :list
       (= {:a 1} (last x)) :vector)))

(defcheck solution-8fff3ebe
  #(if (associative? %)
     (if (reversible? %) :vector :map)
     (if (= % (apply list %)) :list :set)))

(defcheck solution-908adbdd
  (fn [xs]
    (let [e (empty xs)]
      (cond
        (= 1 (count (conj e [:a 1] [:a 2]))) :map
        (= 1 (count (conj e [:a 1] [:a 1]))) :set
        (= [1 2] (conj e 1 2)) :vector
        :else :list))))

(defcheck solution-90a7df36
  (fn [coll]
    (if (associative? coll)
      (if (= 1 (count (flatten (seq (assoc (empty coll) 0 0)))))
        :vector
        :map)
      (if (= 1 (count (conj (conj (empty coll) 0) 0)))
        :set
        :list))))

(defcheck solution-90e6855c
  (fn [xs]
    (let [conjed (conj (conj (conj xs [:xxx 1]) [:xxx 1]) [:yyy 2])]
      (cond (= (:xxx conjed) 1) :map
            (= (count conjed) (+ (count xs) 2)) :set
            (= (first conjed) [:yyy 2]) :list
            (= (last conjed) [:yyy 2]) :vector
            :else :unknown))))

(defcheck solution-9103321b
  #(let [z [1 3]
         n (conj  % [1 2] z z)
         d (- (count n) (count %))]
     (cond
       (= d 1) :map
       (= d 2) :set
       (= (last n) z) :vector
       =  :list)))

(defcheck solution-915f7eb8
  (fn myBlackBoxTesting
    [coll]
    (let [modColl (conj coll (first coll))]
      (if (= (count modColl) (count coll))
        (if (contains? coll (first coll))
          :set
          :map)
        (if (empty? coll)
          (if (= (count (conj modColl 1 1)) 2)
            :set
            (if (= (conj modColl 1 1) '(1 1 nil))
              :list
              :vector))
          (if (= modColl (cons (first coll) coll))
            :list
            :vector))))))

(defcheck solution-91dd9e29
  (fn
    [v]
    (cond
      (:a (conj v [:a 1])) :map
      (< (- (count (conj v 1 1)) (count v)) 2) :set
      (= (conj v 1 2) (cons 2 (cons 1 v))) :list
      :else :vector)))

(defcheck solution-91e9065
  (fn bbtest [coll]
    (let [head (first (str coll))]
      (cond
        (= head \{) :map
        (= head \[) :vector
        (= head \#) :set
        :else :list))))

(defcheck solution-92660da7
  (fn black-box-testing [x]
    (let [e (empty x)]
      (cond (= e {}) :map
            (= e #{}) :set
            (and (= e '()) (= :TEST (first (conj e nil :TEST)) )) :list
            (and (= e []) (= :TEST (last (conj e :nil :TEST))  )) :vector

            :else :unknown))))

(defcheck solution-92759d32
  (fn [col]
    (cond
      (< (count (conj col [:a 1] [:a 1])) (+ 2 (count col)))
      (if (nil? ((conj col [:a 1]) [:a 1])) :map :set)
      (= (list* :a :b col) (conj col :b :a)) :list
      (= (conj (vec col) :a :b) (conj col :a :b)) :vector)))

(defcheck solution-928fd039
  #(let [e (empty %1)]
     (cond (= e {}) :map
           (= e #{}) :set
           (= :tail (last (conj  e :head :tail))) :vector
           :else :list)))

(defcheck solution-9333835b
  (fn p [s]
    (cond
      (= (first (.toString s)) \[) :vector
      (= (first (.toString s)) \() :list
      (= (first (.toString s)) \c) :list
      (= (first (.toString s)) \#) :set
      (= (first (.toString s)) \{) :map
      )))

(defcheck solution-93456c78
  (fn [coll]
    (case (empty coll)
      ; List or vector, conj will behave differently
      [] (let [conjoined2 (conj (empty coll) :first :second)]
           (case (first conjoined2)
             :first :vector ; A vector will grow at the end
             :second :list)); A list grows at the beginning
      #{} :set
      {} :map
      )
    ))

(defcheck solution-9347695
  (fn type-test [x]
    ;; (is this clearer like this, or as a (cond..) ?
    (if-not (ifn? x)
      :list                           ; :map,:set,:vec all implement IFn
      (if-not (associative? x)
        :set                   ; :map,:set don't equal seq of themselves
        (if (reversible? x)
          :vector               ; set from a set will be equal to itself
          :map)))))

(defcheck solution-935e8a7b
  (fn classify [coll]
    (let [e (empty coll)]
      (cond
        (= e {})  :map
        (= e #{}) :set
        (= e '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-936d1c29
  (fn [a]
    (let [gp (gensym)
          g [gp 0]
          g1 [gp 1]]
      (cond
        (= (conj a g) (conj (conj a g) g)) (if (= (count (conj a g))
                                                 (count (conj (conj a g) g1))) :map :set)
        (= (first (conj (conj a g1) g)) g) :list
        :else :vector))))

(defcheck solution-9372fdd5
  (fn [q]
    (cond
      (reversible? q) :vector
      (associative? q) :map
      (nil? (get (conj q 0) 0)) :list
      :else :set)))

(defcheck solution-937c7a33
  (fn [x]
    (let [y (conj x [:x 1] [:x 1] [:x 2])]
      (cond
        (= (+ 1 (count x)) (count y)) :map
        (= (+ 2 (count x)) (count y)) :set
        (= [:x 2] (first y)) :list
        :else :vector))))

(defcheck solution-93815fda
  (fn [s]
    (cond
      (and (empty? s) (= (into s {:a :b}) {:a :b}))  :map
      (not (= (count s) (count (flatten (seq s)))))  :map
      (and (empty? s) (= 1 (count (conj s :a :a))))  :set
      (= (count s) (count (conj s (first s))))       :set
      (= (first (conj s :a :b)) :b)                  :list
      (= (last (conj s :a :b)) :b)                   :vector
      :else :???)))

(defcheck solution-93923494
  #(cond
     (= % (into [] %)) ; vector or list
     (if (= (first (conj (conj % :_x) :_y)) :_y)
       :list
       :vector
       )
     (contains? (conj % [:_x :_y]) :_x)
     :map
     true
     :set
     ))

(defcheck solution-939467c7
  (fn [coll]
    (let [x [:x :y]
          len (count coll)
          c (conj (conj coll x) x)]
      (if (= (inc len) (count c))
        (if (nil? (c :x))
          :set
          :map)
        (let [y [:y :x]
              c (conj (conj coll x) y)]
          (if (= (first c) y)
            :list
            :vector))))))

(defcheck solution-93969532
  #(cond
     (reversible? %)  :vector
     (associative? %) :map
     (= (+ 2 (count %)) (count (conj % 1 1))) :list
     :else :set
     ))

(defcheck solution-943c6c42
  (fn [col]
    (let [e [[100 200] [300 400]]
          xs (into col e)]
      (if (= (count (conj xs (first xs))) (count xs))
        (if (= (xs (first e)) (first e)) :set :map)
        (if (= (first (conj xs (last xs))) (last xs))
          :list
          :vector)))))

(defcheck solution-946544ec
  #(cond
     (= [1 1] (get (conj % [1 1]) [1 1])) :set
     (= 1 (get (conj % [1 1]) 1)) :map
     (= [1 1] (last (conj % 0 [1 1]))) :vector
     1 :list))

(defcheck solution-949d69ab
  (let [[a b c] (repeatedly gensym)
        e1 [a b]
        e2 [a c]]
    (fn [x]
      (let [y (conj x e1 e1 e2)
            diff (- (count y) (count x))]
        (condp = diff
          1 :map
          2 :set
          (if (= (first y) e2)
            :list
            :vector))))))

(defcheck solution-950d438c
  (fn test-seq [coll]
    (cond
      (= 0 (get (conj coll [:xxxxx 0]) :xxxxx)) :map
      (and (< 1 (count (flatten [(conj coll [:x 0])])))
           (= :x (first (conj (conj coll :y) :x)))) :list
      (contains? (conj coll :x) :x) :set
      :else :vector)))

(defcheck solution-952c0a62
  (fn f65 [c] (if (associative? c)
                (if (reversible? c)
                  :vector
                  :map)
                (if (= (+ 2 (count c))
                      (count (conj c 99 99)))
                  :list
                  :set))))

(defcheck solution-958b2cbd
  (fn z [coll]
    (cond
      (= :value (:key (conj coll [:key :value]))) :map
      (= (inc (count coll)) (count (conj coll :testA :testA))) :set
      (= (last (conj (conj coll :testA :testB) :testC)) :testC) :vector
      :else :list)))

(defcheck solution-96bd79c3
  (fn [seq]
    (cond
      (= (get (conj seq [:k :v]) :k) :v) :map
      (= (conj seq :a :a) (conj seq :a)) :set
      (= (first (conj seq :b :a)) :a) :list
      (= (last  (conj seq :b :a)) :a) :vector)))

(defcheck solution-96c0a0ed
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (get (conj % :a) :a) :set
     :else :list))

(defcheck solution-96d744a7
  (fn [c]
    (if (= (vec c) c)
      (if (associative? c)
        :vector
        :list)
      (if (associative? c)
        :map
        :set))))

(defcheck solution-98698879
  #(let [i [1 1]
         c (conj (conj % [2 2]) i)]
     (if (= c (seq c))
       (if (identical? (first c) i) :list :vector)
       (if (identical? (get (conj c [c c]) c) c) :map :set))))

(defcheck solution-98cf75a6
  (fn [x]
    (cond (= x (conj x x)) :map
          (= (conj x 1) (conj (conj x 1) 1)) :set
          (= (cons 1 (cons 2 x)) (conj (conj x 2) 1)) :list
          :else :vector)))

(defcheck solution-98fff4f5
  (fn [coll]
    (let [a    (keyword (gensym))
          b    (keyword (gensym))
          tst1 (conj coll [a b])
          tst2 (conj coll [a a] [b b])]
      (cond
        (= (get tst1 a) b)         :map
        (= (conj tst1 [a b]) tst1) :set
        (= (first tst2) [b b])     :list
        :else                      :vector))))

(defcheck solution-992e406e
  #(let [len (count %)
         len+1 (inc len)]
     (if (= len+1 (count (conj % [:_z 1] [:_z 1])))
       (if (= len+1 (count (conj % [:_z 1] [:_z 2])))
         :map
         :set)
       (if (= :_z (first (conj % :_y :_z)))
         :list
         :vector))))

(defcheck solution-995d6913
  (fn [s]
    (let [result (conj (empty s) [0 1] [0 1] [0 2])]
      (cond
        (= 1 (count result)) :map
        (= 2 (count result)) :set
        (= [0 1] (first result)) :vector
        :else :list))))

(defcheck solution-9a0e2226
  (fn what [coll] (cond
                    (= (count (conj coll [:foo 1]))
                      (count (conj coll [:foo 1] [:foo 2]))) :map
                    (= (count (conj coll :foo)) (count (conj coll :foo :foo))) :set
                    (= :foo (first (apply list (conj (conj coll :bar) coll :foo)))) :list
                    :else :vector)))

(defcheck solution-9a23a91c
  (fn [s]
    (let [conjtst (conj s
                    [:settestkey :settestval]
                    [:settestkey :settestval]
                    [:settestkey1 :settestval1])]
      (cond
        (= :settestval (get conjtst :settestkey)) :map
        (= (inc (inc (count s))) (count conjtst)) :set
        (= [:settestkey1 :settestval1] (first conjtst)) :list
        :default :vector))))

(defcheck solution-9a393a84
  (fn [coll*]
    (let [a [:a nil]
          coll (-> (empty coll*) (conj a) (conj a))]
      (if (associative? coll)
        (if (nil? (coll 0))
          :map
          :vector)
        (if (== 1 (count coll))
          :set
          :list)))))

(defcheck solution-9a547dd
  #(condp = (first (str %))
     \[ :vector
     \{ :map
     \# :set
     :list))

(defcheck solution-9b56ad81
  (fn [coll]
    (let [conjed (conj coll {:dmy1 :dmy1} {:dmy2 :dmy2} {:dmy2 :dmy3})
          consed (cons {:dmy2 :dmy3} (cons {:dmy2 :dmy2} (cons {:dmy1 :dmy1} coll)))]
      (cond (= conjed consed) :list
            (= conjed (concat coll [{:dmy1 :dmy1} {:dmy2 :dmy2} {:dmy2 :dmy3}])) :vector
            (= (count conjed) (count consed)) :set
            :else :map))))

(defcheck solution-9be2ad89
  (fn [c]
    (if (:a (conj c [:a true]))
      :map
      (if (= (count (conj c :a))
            (count (conj (conj c :a) :a)))
        :set
        (if (= (cons 2 (cons 1 c)) (conj (conj c 1) 2))
          :list
          :vector)))))

(defcheck solution-9bfcb383
  (fn [coll]
    (let [before (count coll)
          delta (conj (conj (conj coll {:garbage :can}) {:garbage :pail}) {:garbage :pail})
          after (count delta)]
      (cond
        (= after (+ 1 before)) :map
        (= after (+ 2 before)) :set
        (= (first delta) {:garbage :pail}) :list
        :else :vector))))

(defcheck solution-9c238f73
  (fn [coll](let [len (count coll)
                  tri (conj coll coll coll)
                  tri2 (conj coll tri coll)]
              (cond
                (= len (count tri)) :map
                (= (inc len) (count tri)) :set
                (= coll (first tri2)) :list
                (= coll (last tri2)) :vector
                )
              )))

(defcheck solution-9d2f80a7
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (reversible? base) :vector
        (= base '()) :list))))

(defcheck solution-9d92d13a
  (fn [coll]
    (let [coll (empty coll)
          map-test (-> coll (conj [:x 1]) (conj [:x 2]))
          set-test (-> coll (conj [:x :y]) (conj [:x :y]))]
      (cond
        (= 1 (count map-test)) :map
        (= 1 (count set-test)) :set
        (= [:x 1] (first map-test)) :vector
        :else :list))))

(defcheck solution-9d9411b5
  (fn [coll]
    (if (associative? coll)
      (if (reversible? coll)
        :vector
        :map
        )
      (let [result (into coll '(1 1))]
        (if (= (count result) (+ 2 (count coll)))
          :list
          :set
          )
        )
      )
    ))

(defcheck solution-9db77043
  (fn fn3 [x]
    (if (= (get (conj x [:a :c]) :a) :c)
      (keyword 'map)
      (if (= (count (conj x [:a])) (count (conj (conj x [:a]) [:a])))
        (keyword 'set)
        (let [y (conj x (range 10))]
          (if (= (conj y '(1)) (cons '(1) y))
            (keyword 'list)
            (keyword 'vector)))))))

(defcheck solution-9dc4b655
  (fn bbt
    [s]
    (let [conj1 (conj s (first s) (first s))]
      (if (< (- (count conj1) (count s)) 2)
        (let [teste {:a 1}
              conj2 (conj s teste)]
          (if (contains? conj2 teste)
            :set
            :map))
        (let [teste (str (str (first conj1)) "salt")
              conj2 (conj conj1 teste)]
          (if (and (= teste (first conj2)) (not= teste (last conj2)))
            :list
            :vector))))))

(defcheck solution-9e084dd
  (fn [seq]
    (if (= (vec seq) seq)
      (if (= :x (first (into seq [:y :x]))) :list :vector)
      (if (= (set seq) seq) :set :map))))

(defcheck solution-9e749afe
  (fn [sq]
    (if (associative? sq)
      (if
       (= (take (count sq) sq) sq)
        :vector
        :map)
      (let [x1 (conj sq :a)
            x2 (conj x1 :a)]
        (if (= x1 x2)
          :set
          :list)))))

(defcheck solution-9e954fd9
  (fn [coll] (let [r (conj coll [:1 1] [:1 1] [:1 2])]
               (cond
                 (= (count r) (+ (count coll) 1)) :map
                 (= (count r) (+ (count coll) 2)) :set
                 (= (first r) [:1 2]) :list
                 :else :vector))))

(defcheck solution-9eab75ec
  (fn [x]
    (if (ifn? x)
      (cond
        (= x ((comp set seq) x)) :set
        (= x (->> x
               seq
               (apply vector))) :vector
        (= x (apply hash-map (interleave (keys x) (vals x)))) :map)
      :list)))

(defcheck solution-9ec35e76
  (fn [x]
    (let [o #?(:clj (Object.) :cljs #js {})
          c (count x)
          c2 (count (conj x [o 1] [o 1] [o 2]))]
      (condp = (- c2 c)
        3 (if (= o (first (conj x 1 o)))
            :list
            :vector)
        2 :set
        1 :map))))

(defcheck solution-9ee22eb7
  (fn [coll]
    (let [s (str (empty coll))]
      (cond (= s "{}") :map
            (= s "[]") :vector
            (= s "#{}") :set
            :else :list))))

(defcheck solution-9f02c07a
  (fn __
    [coll]
    (let [tested (empty coll)]
      (cond
        (not (ifn? tested)) :list
        (= 2 (count (conj tested [:t1 :t2] [:t1 :t2]))) :vector
        (= :t2 ((conj tested [:t1 :t2]) :t1)) :map
        :else :set))))

(defcheck solution-9f30717
  (fn colltype [coll]
    (if (not= (count (conj coll [:x 1] [:x 2])) (+ 2 (count coll)))
      :map
      (if (not= (count (conj coll :x :x)) (+ 2 (count coll)))
        :set
        (if (= (last (conj coll :x :y)) :y)
          :vector
          :list)))))

(defcheck solution-9f84e442
  (fn [coll]
    (let [s (into coll [[0 0] [1 '_']])]
      (if (get s [1 '_'])
        :set
        (if (= (get s 1) '_')
          :map
          (if (= (first s) [1 '_'])
            :list
            :vector))))))

(defcheck solution-9f85f0e8
  #({\{ :map \# :set \[ :vector} (first (str %)) :list))

(defcheck solution-a09aee31
  (fn [x]
    (let [e (empty x)]
      (cond
        (= e {})  :map
        (= e #{}) :set
        (= 2 (last (conj (conj e 1) 2))) :vector
        :else :list))))

(defcheck solution-a0c77248
  #(or
    (and (= {} (empty %)) :map)
    (and (= #{} (empty %)) :set)
    (and (= :a (first (conj (conj (empty %) :a) :b))) :vector)
    :list))

(defcheck solution-a0df81e7
  #(or({{}:map #{}:set}(empty %))({[1 2]:vector[2 1]:list} (conj(empty %)1 2))))

(defcheck solution-a0fed4de
  ; Without the limits on 'catch' this is the most straightforward way

  ;(fn [sq]
  ;	(first (remove nil? (for [c {clojure.lang.PersistentArrayMap :map
  ;	 clojure.lang.PersistentHashSet  		 :set
  ;	 clojure.lang.PersistentVector   		 :vector
  ;	 clojure.lang.LazySeq                    :list
  ;	 clojure.lang.PersistentList$EmptyList   :list
  ;	 clojure.lang.PersistentList			 :list}]
  ;		(try (do (cast (first c) sq) (last c))
  ;			 (catch Exception e nil))
  ;    )))
  ;)

  ; Here's a cheap way if we could use pop-thread-bindings

  ;(fn [sq] ({\{ :map \( :list \[ :vector \# :set} (first (with-out-str (pr sq)))))

  ; We can get around this by quoting the name, but casting a LazySeq
  ; or PersistentList to a str returns a class name and obj identifier
  ; thus we should test that our class begins with either of those.

  (fn [sq]
    (let [c (str (second `(name ~sq)))]
      (if (or (.startsWith c "clojure.lang.LazySeq")
              (.startsWith c "clojure.lang.PersistentList"))
        :list
        ({\{ :map \( :list \[ :vector \# :set} (.charAt c 0))))
    ))

(defcheck solution-a15853b9
  (fn [sq]
    (if-not (associative? sq)
      (if (< (- (count (conj (conj sq :a) :a))
                (count sq))
            2)
        :set
        :list)
      (if (=  0  (- (count (conj (conj sq [1 2]) [1 2]))
                    (count (conj sq [1 2]))))
        :map
        :vector
        ))))

(defcheck solution-a1be4a3a
  (fn [x]
    (case (first (.toString x))
      \# :set
      \[ :vector
      \{ :map
      :list)))

(defcheck solution-a1cff245
  (fn [s]
    (let [t (into s [[1 s] [1 [s]] [1 s] [2 s]])]
      (let [n (- (count t) (count s))]

        (cond
          (= 2 n) :map
          (= 3 n) :set
          (= [2 s] (last t)) :vector
          :else :list)))))

(defcheck solution-a2053b24
  (fn [x] (let [m (zipmap [{} #{}] [:map :set] )]
            (if (= :none (m (empty x) :none)) (if (= (first (conj (empty x) 1 2)) 2) :list :vector) (m (empty x)))
            )))

(defcheck solution-a21a8c16
  (fn [coll]
    (cond
      (= (inc (count coll)) (count (conj (conj coll [:c 3]) [:c 4])))
      :map

      (= (conj coll :a) (conj (conj coll :a) :a))
      :set

      (= (conj (conj coll :a) :b) (cons :b (cons :a coll)))
      :list

      :else
      :vector)))

(defcheck solution-a3ae8dc1
  (fn [coll]
    (let [subj (empty coll)]
      (case subj
        {} :map
        #{} :set
        (if (= :test (first (conj subj :dummy :test)))
          :list
          :vector)))))

(defcheck solution-a40c8a5d
  (fn [coll]
    (let [
          new-elem [1 2]
          new-elem-2 [3 4]

          coll-2 (conj coll new-elem new-elem-2)
          elem (first coll-2)
          coll-conj-once (conj coll-2 new-elem)
          coll-conj-twice (conj coll-conj-once new-elem)
          coll-cons-once (cons new-elem coll-2)
          ]
      (if (= (count coll-conj-once) (count coll-conj-twice))
        (if (= (get coll-2 elem) elem) :set :map)
        (if (= coll-conj-once coll-cons-once) :list :vector)
        )
      )
    ))

(defcheck solution-a426936a
  (fn [coll]
    (let [ f [:firstkey :firstval]
          t [:tailkey :tailval]
          conjcoll (conj  (conj coll f) t)
          concatcoll (into (empty coll) (concat conjcoll conjcoll))
          setlike (apply distinct? concatcoll)
          settypetest (fn [x] (= (set x) x))
          ]
      (cond
        (and setlike
             (= true (settypetest coll)))   :set
        (=  setlike true) :map
        (= (last conjcoll) t) :vector
        :else :list
        )
      )
    ))

(defcheck solution-a43537a3
  (fn
    [coll]
    (let [t (first (map char (str coll)))]
      (cond
        (= t \{) :map
        (= t \[) :vector
        (= t \#) :set
        :else :list))))

(defcheck solution-a44df075
  (fn [x]
    (let [emp (empty x)]
      (if (= emp {}) :map
                     (if (= emp #{}) :set
                                     (if (= (conj emp 1 2) [1 2]) :vector :list))))))

(defcheck solution-a49bef3f
  (fn btest [coll]
    (let [x (conj (empty coll) [1 2] [1 2] [1 3])]
      (if (= 1 (count x))
        :map
        (if (= 2 (count x))
          :set
          (if (= 3 (last (first x)))
            :list
            :vector))))))

(defcheck solution-a4f0951a
  (fn __ [x]
    (cond (reversible? x) :vector
          (associative? x) :map
          (= (conj x 1 1) (conj x 1)) :set
          :else :list)))

(defcheck solution-a4f5b49f
  (fn [coll]
    (let [v1 [1 1]
          v2 [1 2]
          c2 (conj coll v1 v1 v2)
          diff (Math/abs (- (count coll) (count c2)))]
      (cond
        (= 1 diff) :map
        (= 2 diff) :set
        (not= 3 diff) :unknown
        (identical? v2 (first c2)) :list
        (identical? v2 (last c2)) :vector
        :else :unknown))))

(defcheck solution-a501e97b
  (fn id [o]
    (let [o' (conj o [:flava :flav])]
      (if (:flava o')
        :map
        (let [o'' (conj o' [:flava :flav])]
          (if (= (count o') (count o''))
            :set
            (if (= (conj (conj o :flava) :flave)
                  (cons :flave (cons :flava o)))
              :list
              :vector)))))))

(defcheck solution-a513760a
  (fn [coll]
    (cond
      (= (empty coll)  {}) :map
      (= (empty coll) #{}) :set
      (=
        (conj (empty coll) 1 2)
        [1 2]) :vector
      :else :list)))

(defcheck solution-a5d2bc76
  #({\{ :map \# :set \[ :vector} (-> % str first) :list))

(defcheck solution-a6151f81
  (fn my-type [coll]
    (if (associative? coll)
      (if (= (count (conj coll [0 1])) (count (conj (conj coll [0 1]) [0 1])))
        :map
        :vector)
      (if (= (count (conj coll 1)) (count (conj (conj coll 1 1) 1 1)))
        :set
        :list)
      )))

(defcheck solution-a6cb255c
  (fn ttest [m]
    (let [settv (conj m (first m))
          velt (conj m [2 400] [1 200])
          maptv (conj m (conj m [:tester 1]) [:tester 2])]
      (if (or (= (inc (count m)) (count maptv)) (= m {}))
        :map
        (if (or (= (count m) (count settv)) (= m #{}))
          :set
          (if (= [1 200] (first velt))
            :list
            :vector))))))

(defcheck solution-a6edb422
  (fn [x-coll]
    (let [e-coll (empty x-coll)
          tc (conj (conj e-coll [:a 10]) [:b 20])
          get-a10 (get tc [:a 10])
          get-a (get tc :a)
          get-0 (get tc 0)
          get-fst (first tc)]
      (if (= [:a 10] get-a10)
        :set
        (if (= 10 get-a)
          :map
          (if (= [:a 10] get-0)
            :vector
            (if (= [:b 20] get-fst)
              :list
              nil) ))) )))

(defcheck solution-a725d48a
  (fn f[o]
    (cond (= 1 (:test (conj o [:test 1]))) :map
          (= (inc(count o)) (count (conj o :x :x))) :set
          (= :x2 (first (conj o :x1 :x2))) :list
          :else :vector)))

(defcheck solution-a82102cb
  (fn [c]
    (let [t [:a 2]
          c1 (conj c [1 2] t)
          c2 (conj c [1 2] t t)]
      (cond
        (not= (vec c1) c1)
        (if (contains? c1 :a)
          :map
          :set)
        (= (first c2) t) :list
        :else :vector))))

(defcheck solution-a874132d
  #(case ((juxt associative? reversible? ifn?) %)
     [true  true  true ] :vector
     [false false false] :list
     [true  false true ] :map
     [false false true ] :set))

(defcheck solution-a89918db
  (fn p65 [coll]
    (case (first (str coll))
      (or \( \c) :list
      \[ :vector
      \# :set
      \{ :map)))

(defcheck solution-a8b4bdc
  (let [is-list? (fn [a] (= (first (conj (conj a [1 2]) [2 3])) [2 3]))
        is-vec?  (fn [a] (= (last (conj (conj a [1 2]) [2 3])) [2 3]))
        is-set?  (fn [a] (= (conj (conj a [1 2]) [1 2]) (conj a [1 2])))
        is-map?  (fn [a] (= (count (conj (conj a [99 2]) [99 3])) (count (conj a [99 2]))))]
    (fn bbt [x]
      (if (is-set? x)
        (if (is-map? x)
          :map
          :set)
        (if (is-vec? x)
          :vector
          (if (is-list? x)
            :list nil))))))

(defcheck solution-a8b55791
  (fn [x]
    (let [y (conj x [:c 2] [:c 2] [:d 4])
          d (- (count y) (count x))]
      (if (= 2 d)
        (if (get y :c) :map :set)
        (if (= (first y) [:d 4]) :list :vector)))))

(defcheck solution-a8f26cf3
  #(let [what (conj % [::foo "bar"])]
     (if (get what ::foo)
       :map
       (let [what' (-> what (conj ::a) (conj ::b) (conj ::a) (conj ::b))]
         (if (= 2 (- (count what') (count what)))
           :set
           (if (= ::b (first what'))
             :list
             :vector))))))

(defcheck solution-a98b1573
  #(let [coll (conj % [1 2] [3 4])]
     (let [c1 (first coll), coll' (conj coll c1)]
       (if (= coll coll')
         (if (nil? (get coll c1))
           :map
           :set)
         (if (identical? c1 (first (rest (conj coll c1))))
           :list
           :vector)))))

(defcheck solution-a9a502fa
  (fn bb [x]
    (let [e (conj (conj (empty x) {5,4}) {5,4})
          ]

      (cond (> (count e) 1)
            (if (= (last (conj e 9)) 9)
              :vector
              :list
              )
            :else (if (= (e 5) 4) :map :set)
            )
      )
    ))

(defcheck solution-a9bd536c
  #(case (first (str %)) \{ :map \# :set \[ :vector (\( \c) :list))

(defcheck solution-a9fe0621
  (fn check-type [a-coll]
    (let [empty-coll (empty a-coll)
          added-coll (conj empty-coll [5 6])]
      (if (not= (get added-coll 5) nil)
        :map
        (let [two-elem-added (conj empty-coll 5 5)]
          (if (= (count two-elem-added) 1)
            :set
            (let [inc-seq (conj empty-coll 5 6)]
              (if (= (first inc-seq) 5)
                :vector
                :list
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-aa112fa5
  (fn categorise [x]
    (let [protostring (.toString (empty x))
          eql? #(= protostring %)]
      (if (eql? "{}")  :map
                       (if (eql? "#{}") :set
                                        (if (eql? "[]")  :vector
                                                         :list))))))

(defcheck solution-aa206ca7
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (= % (apply list %)) :list
     :else :set))

(defcheck solution-aa3c3208
  (fn [x]
    (let [e (empty x)]
      (cond
        (= (conj e {:a :a}) {:a :a}) :map
        (= (count (conj (conj e :a) :a)) 1) :set
        (= (last (conj (conj e :a) :b)) :b) :vector
        (= (last (conj (conj e :a) :b)) :a) :list))))

(defcheck solution-aa54a7af
  (fn __ [col] (let [x (empty col)]
                 (cond
                   (= x {}) :map
                   (= x #{}) :set
                   (= x '()) (if (reversible? x) :vector :list)))))

(defcheck solution-aa6a80ec
  (fn [c1]
    (let [s1 (count c1)
          c2 (conj c1 [:foo :bar] [:foo :bar])]
      (if (= (count c2) (+ 1 s1))
        (if (c2 :foo) :map :set)
        (let [c3 (conj c2 :baz)]
          (if (= (first c3) :baz) :list :vector))))))

(defcheck solution-aab423de
  (fn [coll]
    (let [added (-> coll (conj [:first "first"]) (conj [:second "second"]))]
      (cond
        (= (get added :first) "first") :map
        (= (get added [:second "second"]) [:second "second"]) :set
        (= (last added) [:second "second"]) :vector
        (= (first added) [:second "second"]) :list))))

(defcheck solution-ab7b2f15
  (fn black-box-test [coll]
    (let [coll (into coll [[:a 1] [:b 2]])]
      (if (= coll (set coll))
        :set
        (if (= coll (into coll coll))
          :map
          (if (= coll (into (empty coll) coll))
            :vector
            :list))))))

(defcheck solution-abb8030c
  (fn [x]
    (let [t1 (conj x [:d 0] [:c 1])]
      (cond
        (get t1 :c) :map
        (= t1 (conj t1 [:c 1])) :set
        (= (first t1) [:c 1]) :list
        true :vector))))

(defcheck solution-ac13f169
  (fn [coll] (cond
               (= -1 (:z (conj coll [:z -1]))) :map
               (= (count (conj coll -1 -1 -1)) (count (conj coll -1 -1 -1 -1 -1))) :set
               (= [:z -1] (first (conj coll  [:z -2] [:z -1]))) :list
               (= [:z -1] (last (conj coll [:z -1]))) :vector

               )))

(defcheck solution-ac6678bf
  #(let [xs (conj % [:a :a] [:a :a] [:b :b])]
     (cond
       (= '([:b :b] [:a :a] [:a :a]) (take 3 xs)) :list
       (contains? xs [:a :a]) :set
       (contains? xs :a) :map
       :else :vector)))

(defcheck solution-ac96f2aa
  (fn [x] (let [v (into (into x {1 2 3 4}) {1 5})
                u (into (into x {1 5}) {1 2 3 4})]
            (cond
              (= (count v) (+ 2 (count x))) :map
              (= u v) :set
              (= [1 5] (first v)) :list
              (= [1 5] (last v)) :vector)
            )
    ))

(defcheck solution-ad182fdd
  #(cond
     (and (associative? %) (reversible? %)) :vector
     (associative? %) :map
     (= (+ 2 (count %)) (count (conj (conj % :are-you-a-list?) :are-you-a-list?))) :list
     :default :set))

(defcheck solution-ad6545b1
  (fn black-box
    [x]
    (if (reversible? x)
      :vector
      (if (associative? x)
        :map
        (let [y (conj (conj x 100) 100)]
          (if (= (count y) (+ 1 (count x)))
            :set :list))))))

(defcheck solution-adffcc60
  (fn ty [t]
    (cond
      (= (+ 1 (count t)) (count (conj (conj t [10 10]) [10 10])))
      (if (contains? (conj t [10 10]) [10 10] )
        :set
        :map)

      (= (take-last 2 (conj (conj t t) 4)) [t 4])
      :vector

      :else
      :list)

    ))

(defcheck solution-ae44e9f
  #(cond
     (= {} (conj (empty %) nil)) :map
     (= (inc (count %)) (count (conj % :a :a))) :set
     (= :b (first (conj % :a :b))) :list
     :e :vector))

(defcheck solution-aeb719
  (fn tp [a]
    (let [uniq #()
          p [uniq :p]
          q [uniq :q]
          m (conj (conj a p) q)]
      (if (= (get m uniq) :q) :map
                              (if (get m p) :set
                                            (if (= (first m) q) :list :vector))))))

(defcheck solution-af23d0dd
  (fn [x]
    (if (= x (rest (cons 1 x))) ; true if list or vector
      (if (= (conj x 1 2) (concat '(2 1) x)) ; true if list
        :list
        :vector)
      (if (contains? (conj x {:foo 'bar}) :foo) ; true if map
        :map
        :set))))

(defcheck solution-af48d7a2
  (fn t [c]
    (let [c (empty c)
          c1 (conj c [2 :two])
          c2 (conj c1 [2 :two])]
      (if (= c1 c2)
        (if (= :two (c1 2))
          :map
          :set)
        (if (= (conj c1 1) [1 [2 :two]])
          :list
          :vector)))))

(defcheck solution-af55b61f
  (fn eka
    ([xs]
     (if-not (get (conj xs [:darth :vader]) :darth)
       (let
        [t (conj (conj xs :darth) :vader)]
         (cond
           (get t :darth) :set
           (= (first t) :vader) :list
           (= (last t) :vader) :vector
           )
         )
       :map
       ))))

(defcheck solution-b0815f8d
  (fn [coll]
    (let [coll (conj coll [:hi 'there])]
      (cond
        (not (ifn? coll)) :list
        (contains? coll :hi) :map
        (contains? coll [:hi 'there]) :set
        :else :vector))))

(defcheck solution-b0dfe20f
  (fn [coll]
    (let [a [:my-key :my-val]
          b (conj coll a)]
      (cond
        (= (get b :my-key) :my-val)            :map
        (= (get b a) a)                        :set
        (= (first (conj b :my-key)) :my-key)   :list
        :else                                  :vector))))

(defcheck solution-b1571cc9
  #(let [e (empty %)]
     (condp = e
       {} :map
       [] (if (reversible? e) :vector :list)
       #{} :set)))

(defcheck solution-b203b734
  (fn [s]
    (let [s2 (conj s [1 2])]
      (if (= (count (conj s2 [1 2])) (count s2))
        (if (= (count (conj s2 [1 (gensym)])) (count s2))
          :map
          :set)
        (let [sym (gensym)]
          (if (= (first (conj s2 sym)) sym)
            :list
            :vector))))))

(defcheck solution-b21da1d8
  (fn [x]
    (let [e (empty x)]
      (if (= e {}) :map
                   (if (= e #{}) :set
                                 (if (= e (vector))
                                   (if (= [1 2] (conj e 1 2)) :vector :list)))))))

(defcheck solution-b24808ff
  (fn [x]
    (let [x (conj x [1 2] [3 4] [5 6])]
      (if (get (conj x ["meow" "meow"]) "meow")
        :map
        (if (= (conj x 1)
              (conj (conj x 1) 1))
          :set
          (if (= (first (conj x "meow"))
                "meow")
            :list
            :vector))))))

(defcheck solution-b288f9f5
  (fn typeof [aseq]
    (let [conjoined (conj (conj aseq [:1 1]) [:2 2])]
      (cond
        (get conjoined :1) :map
        (= (count conjoined) (count (conj conjoined [:1 1]))) :set
        (= [:2 2] (first conjoined)) :list
        :else :vector))))

(defcheck solution-b395c98e
  #(case (-> % str first)
     \[ :vector
     \( :list
     \{ :map
     \# :set
     \c :list))

(defcheck solution-b44459c8
  #(condp = (empty %)
     #{} :set {} :map
     (first (conj (empty %) :vector :list))))

(defcheck solution-b44ae3cf
  (fn [l]
    (cond
      (reversible? l) :vector
      (associative? l) :map
      :else
      (let [k (count (conj l :o :o)) j (count l)]
        (if (= (- k j) 2)
          :list
          :set)))))

(defcheck solution-b44f98e5
  (fn mytype [coll]
    (let [ec (empty coll)]
      (if (= {:a 1} (conj ec {:a 1}))
        :map
        (condp = (get (conj ec 1 2) 1)
          1 :set
          2 :vector
          nil :list)))))

(defcheck solution-b4667683
  (fn t2 [sq]
    (let [lst (into [] sq)]
      (if (= lst sq)
        (if (= (conj sq 1 2) (conj lst 1 2))
          :vector
          :list
          )
        (if (= (apply hash-map (flatten lst)) sq)
          :map
          :set
          )
        )
      )
    ))

(defcheck solution-b4d24ae7
  (fn [t]
    (cond
      (= (conj t {}) t) :map
      (empty? t) (cond
                   (= (empty t) #{}) :set
                   (= (conj (conj t 0) 1) [0 1]) :vector
                   :else :list)
      (= (clojure.set/union (set t) (set t)) t) :set
      (= (first (conj t t)) t) :list
      :else :vector)))

(defcheck solution-b4dafe0d
  (fn [c]
    (cond
      (= (get (conj c [:t "t"]) :t) "t") :map
      (= (get (conj c :t) :t) :t) :set
      (= (first (conj (conj c :a) :b)) :b) :list
      (= (last (conj (conj c :a) :b)) :b) :vector)))

(defcheck solution-b4f3da73
  (fn [s]
    (let [b (empty s)]
      (cond
        (= b {})  :map
        (= b #{}) :set
        (= b '()) (if (reversible? s) :vector :list)))))

(defcheck solution-b515ce71
  (fn [e]
    (cond
      (reversible? e) :vector
      (associative? e) :map
      (ifn? e) :set
      :else :list)))

(defcheck solution-b5b2931d
  (fn [c]
    (cond
      (= (get (conj c [:t "t"]) :t) "t") :map
      (= (get (conj c :t) :t) :t) :set
      (= (first (conj (conj c :a) :b)) :b) :list
      (= (last (conj (conj c :a) :b)) :b) :vector)))

(defcheck solution-b5c9fc46
  (fn [c] (condp = (conj (conj (empty c) [1 1]) [2 2]) #{[1 1][2 2]} :set {1 1 2 2} :map [[1 1][2 2]] :vector '([2 2][1 1]) :list)))

(defcheck solution-b5ddafa2
  (fn [coll]
    (let [sq (reduce conj coll [[:c 0] [:c 1] [:c 1]])]
      (cond
        (= (first sq) (second sq) [:c 1]) :list
        (= (first (reverse sq)) (second (reverse sq)) [:c 1]) :vector
        (= (:c sq) 1) :map
        :else :set))))

(defcheck solution-b5f14c0f
  (fn [s]
    (let [f (first (str (doall s)))
          m {\[ :vector
             \{ :map
             \# :set
             \( :list
             \c :list
             }]
      (get m f)

      )
    ))

(defcheck solution-b62d970
  #(cond
     (= (count (conj % [0 0]))
       (count (conj % [0 0] [0 1]))) :map
     (= (conj % 0) (conj % 0 0)) :set
     (= (next (conj % 0 1)) (conj % 0)) :list
     :else :vector))

(defcheck solution-b6992930
  #(if (= (vec %) %)
     (if (= (conj (vec %) 1 2) (conj % 1 2))
       :vector
       :list)
     (if (= (set %) %)
       :set
       :map)))

(defcheck solution-b6b07eab
  #(case (subs (.toString %) 0 1) "[" :vector "#" :set "{" :map :list))

(defcheck solution-b78b5dd6
  #(let [n (conj (empty %) [:a :b] [:a :b] [:b :c])]
     (if (= 2 (count n))
       (if (contains? n :a)
         :map :set)
       (if (= (first n) [:b :c])
         :list :vector)
       )
     ))

(defcheck solution-b79d4a3c
  (fn
    [coll]
    (cond
      (= (:a (conj coll [:a 1])) 1) :map
      (< (count (conj coll :a :a)) (+ (count coll) 2)) :set
      (= (first (conj coll :a coll)) coll) :list
      :else :vector
      )
    ))

(defcheck solution-b862314e
  (fn [coll]
    (let [cc (conj coll [:a-random-sym :b-random-sym])]
      (if (= 1 (count (flatten [cc])))
        (if (cc [:a-random-sym :b-random-sym])
          :set
          :map)
        (if (= (first (conj cc :another-random-sym))
              :another-random-sym)
          :list
          :vector)))))

(defcheck solution-b8827871
  (fn [x]
    (get { \{ :map, \# :set, \[ :vector }
      (first (str x))
      :list)))

(defcheck solution-b886b0e3
  (fn [coll]
    (let [e (empty coll)]
      (cond
        (reversible? e)  :vector
        (associative? e) :map
        (= '(2 1) (conj e 1 2)) :list
        :else :set))))

(defcheck solution-b8a2e0f5
  #(let [c (empty %)]
     (cond
       (= c {}) :map
       (= c #{}) :set
       (= (first (conj c 1 2)) 1) :vector
       (= (last (conj c 1 2)) 1) :list)))

(defcheck solution-b8dff2c6
  #(let [base (empty %)]
     (cond
       (= base #{}) :set
       (= base {}) :map
       :else (if (= 1 (-> base (conj 0) (conj 1) first)) :list :vector))))

(defcheck solution-b900b00a
  (fn [s]
    (if-not (nil? (:a (conj s [:a 1])))
      :map
      (if (= (into s [:a]) (into s [:a :a]))
        :set
        (let [s1 (conj s 1)]
          (if (= s1 (first (conj s1 s1)))
            :list
            :vector))))))

(defcheck solution-b93280c9
  (fn poke [coll]
    (let [
          mod-coll (conj coll [:first-key :first-val] [:first-key :second-val] [:first-key :second-val])
          count-change (- (count mod-coll) (count coll))
          mod-first (first mod-coll)]
      (case count-change
        1 :map
        2 :set
        3 (if (= mod-first [:first-key :second-val]) :list :vector)))))

(defcheck solution-b9e85411
  #(if (nil? (:a (conj % {:a 1})))
     (if (nil? (get (conj % :a) :a))
       (if (= :b (first (conj % :a :b)))
         :list
         :vector)
       :set)
     :map))

(defcheck solution-ba1995f4
  (fn [sq]
    (let [fst [:fst sq]
          snd [:snd sq]
          nsq (into sq [fst fst snd])]
      (cond
        (< (count nsq) (+ 3 (count sq)))
        (if (nil? (nsq fst)) :map :set)
        (= (first nsq) snd) :list
        :else :vector))))

(defcheck solution-ba3dd0d
  (fn [xs]
    (if (not= (get (into xs [[:a :b]]) :a) nil)
      :map
      (if (= (set xs) xs)
        :set
        (let [test-seq (conj (conj xs :a) :b)]
          (if (= (concat xs '(:a :b)) test-seq)
            :vector
            :list))))))

(defcheck solution-bb0cc540
  #(let [a (count %)
         b (conj % {:aa :bb})
         c (conj b {:aa :cc})
         d (conj c {:aa :cc})
         e (- (count d) a)]
     (cond
       (= e 1) :map
       (= e 2) :set
       (= (last c) {:aa :cc}) :vector
       :else :list)))

(defcheck solution-bb133622
  #(let [s (str %)]
     (cond
       (re-find #"\#" s) :set
       (re-find #"\{" s) :map
       (re-find #"\[" s) :vector
       :else :list )))

(defcheck solution-bb21940d
  #(let [ex (into (empty %) [[:a 1] [:a 1]])]
     (if (= (count ex) 1)
       (if (= (:a ex) 1) :map :set)
       (if (= (into (empty %) [1 2 3 4]) (reverse [1 2 3 4])) :list :vector))))

(defcheck solution-bb4d5ab
  (fn [coll]
    (let [t1 #?(:clj (Object.) :cljs #js {})
          t2 #?(:clj (Object.) :cljs #js {})
          p  [t1 t2]
          as (conj coll p)]
      (if (= (count as) (count (conj as p)))
        (if (as t1)
          :map
          :set)
        (if (= t1 (first (conj as t1)))
          :list
          :vector)))))

(defcheck solution-bb9b810b
  (fn [coll]
    (cond
      (reversible? coll)  :vector
      (associative? coll) :map
      (= (conj coll 1) (cons 1 coll)) :list
      "default" :set)))

(defcheck solution-bb9eb48b
  (fn bb [s]
    (let [s (if (empty? s)
              (conj s [:TESTING :TESTING])
              s)]
      (cond (= (get s (first s)) (first s)) :set
            (= (cons [:TEST :TEST] s) (conj s [:TEST :TEST])) :list
            (= (conj s (first s)) s) :map
            :else :vector))))

(defcheck solution-bba963d1
  (fn [c]
    (cond
      (= {} (empty c)) :map
      (= #{} (empty c)) :set
      (= [:a :b] (conj (empty c) :a :b)) :vector
      (= '(:b :a) (conj (empty c) :a :b)) :list)))

(defcheck solution-bbef2873
  (fn black-box [q]
    (cond
      (associative? q) (let [s2 (+ (count q) 2)
                             sab (count (conj q [:a :b] [:a :b]))]
                         (if (= s2 sab)
                           :vector
                           :map))
      :else (let [s2 (+ (count q) 2)
                  saa (count (conj q :a :a))]
              (if (= saa s2)
                :list
                :set)))))

(defcheck solution-bcf5f632
  (fn [coll]
    (let [name-map {{} :map #{} :set '[] :seq}
          seq-name (get name-map (empty coll))]
      (cond
        (= seq-name :seq) (first
                            (conj
                              (empty coll)
                              :vector
                              :list))
        :else seq-name))))

(defcheck solution-bcf9959a
  (fn [coll]
    (cond
      (reversible? coll) :vector
      (not (ifn? coll)) :list
      (not (associative? coll)) :set
      :default :map)))

(defcheck solution-bd0cf2e0
  #(cond
     (= (conj % nil) %) :map
     (= (conj % 0) (conj % 0 0)) :set
     (= (first (conj % 0 2)) 2) :list
     true :vector))

(defcheck solution-bd348d2c
  (fn [c]
    (cond
      (= (set c) c) :set
      (nil? (get (conj c [0 :a]) 0)) :list
      (= :a (get (conj c [0 :a]) 0)) :map
      true :vector)))

(defcheck solution-bded63db
  (fn [x]
    (if (= (count (conj x {:_ 0 :__ 0})) (+ (count x) 2))
      :map
      (if (= (count (conj (conj x :_) :_)) (count (conj x :_)))
        :set
        (if (= (first (conj x :_ :__)) :__)
          :list
          :vector)))))

(defcheck solution-be57c29a
  #(-> % pr-str first {\{ :map \( :list \[ :vector \# :set}))

(defcheck solution-be7f70b9
  (fn black [col]
    (cond
      (= (:special (conj col [:special 5])) 5) :map
      (:special (conj col :special)) :set
      (= (last (conj col :special1 :special2)) :special2) :vector
      (= (first (conj col :special1 :special2)) :special2) :list
      :else :unknown
      )))

(defcheck solution-be9d4126
  (fn [t]
    (if (= (empty t) {})
      :map
      (let [e1 (gensym)
            e2 (gensym)
            added (-> (conj t e1)
                    (conj e1)
                    (conj e2))
            f (first added)
            l (last added)
            ]
        (cond (not= (count added)
                (+ 3 (count t)))
              :set

              (= f e2)
              :list

              (= l e2)
              :vector)))))

(defcheck solution-bef56aa0
  (fn [x]
    (cond (= x (conj x x))
          :map
          (= (inc (count x)) (count (conj (conj x :symbol) :symbol)))
          :set
          (= :symbol2 (first (conj x :symbol1 :symbol2)))
          :list
          :else :vector)))

(defcheck solution-bfff0b69
  (fn [coll]
    (cond
      (= 1 (count (conj (empty coll) [1 2] [1 3]))) :map
      (= 1 (count (conj (empty coll) 1 1))) :set
      (= 2 (first (conj (empty coll) 1 2))) :list
      :else :vector)))

(defcheck solution-bfffea8d
  #(case (empty %)
     {} :map
     #{} :set
     [] (if (= [1 2] (conj (empty %) 1 2))
          :vector
          :list)))

(defcheck solution-c0e48e9f
  (fn [bb]
    (let [x (gensym)
          y (gensym)
          l (count bb)
          ]
      (cond
        (= x (get (conj bb {x x}) x)) :map
        (= (inc l) (count (conj (conj bb x) x))) :set
        (= y (first (conj (conj bb x) y))) :list
        :else :vector
        )
      )
    ))

(defcheck solution-c0f44ed0
  (fn [x] (cond
            (= (get (conj x [:inky :pinky]) :inky) :pinky) :map
            (= (conj x 4) (conj (conj x 4) 4)) :set
            (= (first (conj (conj x :foo) :tail)) :tail) :list
            (= (last (conj (conj x :foo) :tail)) :tail) :vector
            )
    ))

(defcheck solution-c1594553
  (fn [x]
    (let [x' (conj x [:key :val])]
      (cond
        (= (get x' :key) :val) :map
        (= (get x' [:key :val]) [:key :val]) :set
        :else (let [x'' (conj x' :v)]
                (if (= (first x'') :v)
                  :list
                  :vector))))))

(defcheck solution-c181a13c
  (fn [l]
    (let [a (gensym) b (gensym) x (gensym)
          m (conj (conj l [b x]) [a x])]
      (cond
        (= (get m [a x]) [a x]) :set
        (= (get m a) x) :map
        (= (last m) [a x]) :vector
        (= (first m) [a x]) :list))))

(defcheck solution-c1e9a3eb
  (fn black-box-testing [s]
    (cond
      (= (conj s {}) s) :map
      (empty? s) (if (= s #{})
                   :set
                   (first (conj s :vector :list)))
      (= (clojure.set/union (set s) (set s)) s) :set
      (= (first (conj s s)) s) :list
      :else :vector)))

(defcheck solution-c1f54e82
  (fn [item]
    (let [s (str item) f #(not (nil? (re-find (re-pattern %2) %1)))]
      (cond
        (f s "#\\{") :set
        (f s "\\[") :vector
        (f s "\\{") :map
        :else :list
        )
      )))

(defcheck solution-c2c1d528
  #(let [e (empty %)]
     (case e
       #{} :set
       {} :map
       [] (if (= (into e [0 1]) [0 1])
            :vector
            :list))))

(defcheck solution-c3a7864b
  #(if (= % (vec %))
     (if (= (concat % [1 2]) (conj % 1 2))
       :vector
       :list)
     (if ((conj % [3 7]) [3 7])
       :set
       :map)))

(defcheck solution-c422704d
  (fn [coll]
    (let [e (reduce #(conj %1 (vector %2 (inc %2))) (empty coll) (range 10))
          v (reduce #(conj %1 (vector %2 (inc %2))) [] (range 10))
          l (reduce #(conj %1 (vector %2 (inc %2))) () (range 10))
          m (reduce #(conj %1 (vector %2 (inc %2))) {} (range 10))
          s (reduce #(conj %1 (vector %2 (inc %2))) #{} (range 10))]

      (cond (= e v) :vector
            (= e l) :list
            (= e m) :map
            (= e s) :set ))))

(defcheck solution-c447f9ad
  (fn [coll]
    (cond
      (and (associative? coll) (contains? (conj coll [:a 4]) :a)) :map
      (= (inc (count coll)) (count (conj coll 42 42))) :set
      (= (last (conj coll 42 45)) 45) :vector
      (= (first (conj coll 42 45)) 45) :list)))

(defcheck solution-c4a8a547
  (fn [s](let [matchers {#{} :set {} :map}]
           (matchers (empty s) (if (associative? s) :vector :list)))))

(defcheck solution-c4e4923c
  (fn my-type [x]
    (let [x1 (conj x [2 3] [2 4] [2 4])]
      (cond
        (= 1 (- (count x1) (count x))) :map
        (= 2 (- (count x1) (count x))) :set
        (= [2 4] (first x1)) :list
        :else :vector))))

(defcheck solution-c5df1a19
  (fn [a-seq]
    (let [a-seq-2 (apply conj a-seq [[1 2] [3 4]])]
      (cond
        (empty? a-seq)
        (cond
          (and (= (first a-seq-2) (get a-seq-2 (first a-seq-2)))
               (= a-seq-2 (conj a-seq-2 (first a-seq-2)))) :set
          (and (nil? (get a-seq-2 (first a-seq-2)))
               (= (last a-seq-2) (last (conj a-seq-2 (first a-seq-2))))
               (= (first (conj a-seq-2 (first a-seq-2)))
                 (second (conj a-seq-2 (first a-seq-2))))) :list
          (= (last (conj a-seq-2 (first a-seq-2))) (first (conj a-seq-2 (first a-seq-2)))) :vector
          :else :map)
        (and (= (first a-seq) (get a-seq (first a-seq)))
             (= a-seq (conj a-seq (first a-seq)))) :set
        (and (nil? (get a-seq (first a-seq)))
             (= (last a-seq) (last (conj a-seq (first a-seq))))
             (= (first (conj a-seq (first a-seq)))
               (second (conj a-seq (first a-seq))))) :list
        (= (last (conj a-seq (first a-seq))) (first (conj a-seq (first a-seq)))) :vector
        :else :map))))

(defcheck solution-c619043
  (fn [xs]
    (cond
      (and (associative? xs) (zero? (count (conj (empty xs) {} {})))) :map
      (= 1 (count (conj (conj (empty xs) :test) :test))) :set
      (= (first (conj (conj xs :test1) :test)) :test) :list
      :else :vector)))

(defcheck solution-c66e27ec
  #(or ({{} :map #{} :set} (empty %))
       (if (= 1 ((comp first conj) (empty %) 1 2)) :vector :list)))

(defcheck solution-c70f265
  #(or ({\{ :map, \[ :vector \# :set} (first (str %))) :list))

(defcheck solution-c787658b
  (fn [xs]
    (let [t [:test 2]
          aug (conj xs t t)]
      (if (= (count aug) (+ (count xs) 2))
        (cond (= (first (conj aug :test)) :test) :list
              (= (last (conj aug :test)) :test) :vector
              :else nil)
        (cond (= (aug :test) 2) :map
              (= (aug t) t) :set
              :else nil)))))

(defcheck solution-c7dc0e71
  (fn [mystery]
    (let [empty-mystery (empty mystery)
          e1 (vector :a :b)
          e2 (vector :c :d)
          with-e1 (conj empty-mystery e1)
          with-e1-e2 (conj with-e1 e2)]
      (if (= with-e1 (conj with-e1 e1))
        (if (contains? with-e1 :a) :map :set)
        (if (= (first with-e1-e2) e1) :vector :list)))))

(defcheck solution-c7f1e8df
  (fn [coll]
    (case (empty coll)
      {}    :map
      '()   (if (associative? coll) :vector :list)
      #{}   :set)))

(defcheck solution-c80c3c9b
  (fn bbt [xs]
    (let [v1 (str xs)
          v2 (str xs xs)
          new-xs (conj xs [v1 v1])
          new-xs2 (conj new-xs [v1 v1])
          new-xs3 (conj new-xs2 [v1 v2])]
      (cond (= (count new-xs3) (inc (count xs))) :map
            (= (count new-xs) (count new-xs2)) :set
            (= (first new-xs3) [v1 v2]) :list
            :else :vector))))

(defcheck solution-c82f5ce
  (fn [s]
    (let [test ["test" "test"]
          test2 ["test2" "test2"]
          with-test (-> s (conj test2) (conj test) (conj test))]
      (cond
        (= (count s) (- (count with-test) 2)) (if (= "test" (get with-test "test")) :map :set)
        (= (first with-test) test) :list
        (= (last with-test) test) :vector))))

(defcheck solution-c8637d9
  (fn [x]
    (if (= (+ 1 (count x)) (count (conj (conj x [1 2]) [1 2])))
      (if (= (+ 1 (count x)) (count (conj (conj x [1 2]) [1 3])))
        :map
        :set
        )
      (if (= (first (conj (conj x 1) 2)) 2)
        :list
        :vector
        )
      )
    ))

(defcheck solution-c8a3248a
  #(let [a (conj (empty %) [:a 1] [:a 1] [:a 2])]
     (condp = (count a)
       1 :map
       2 :set
       (if (= (first a) [:a 2])
         :list
         :vector))))

(defcheck solution-c8a6a7fd
  (fn [thing]
    (let [one (conj (empty thing) [:c 4])]
      (condp = one
        {:c 4} :map
        [[:c 4]] (if (= [1 2] (conj (empty thing) 1 2)) :vector :list)
        #{[:c 4]} :set))))

(defcheck solution-c993b1d1
  (fn wtf [x]
    (if (associative? x)
      (if (reversible? x)
        :vector
        :map)
      (if (= (conj x 1 2) (conj x 2 1))
        :set
        :list))))

(defcheck solution-c9f0c02d
  (fn [s]
    (if (or (= {} s) (let [fst (first s)]
                       (if (counted? fst) (= 2 (count fst)))))
      :map
      (if (>= (-> s count inc)
              (-> s (conj :s) (conj :s) count))
        :set
        (if (= :s (-> s (conj :f) (conj :s) first))
          :list
          :vector)
        ))))

(defcheck solution-c9fbaec8
  (fn [c]
    (cond (= c (into [] c))
          (if (= (conj (empty c) 1 2) [1 2])
            :vector
            :list)
          ((conj c [c c]) [c c]) :set
          true :map)))

(defcheck solution-ca6cb95c
  (fn black-box [coll]
    (let [t (gensym)] ; A generic item guaranteed to not already be in coll.
      (cond
        (= coll (set coll))                            :set
        (= (conj coll [t 1]) (conj coll [t 1] [t 1]))  :map
        (= [t 2] (first (conj coll [t 1] [t 2])))      :list
        (= [t 2] (last (conj coll [t 1] [t 2])))       :vector
        :else                                          nil))))

(defcheck solution-ca6d1913
  (fn [x]
    (let [c (first (str x))]
      (cond
        (= \{ c) :map
        (= \# c) :set
        (= \[ c) :vector
        :else :list))))

(defcheck solution-ca7594b7
  (fn [coll]
    (let [c (conj coll [0 0])]
      (cond
        (= (count c) (count (conj c [0 1]))) :map
        (= (count c) (count (conj c [0 0]))) :set
        :else
        (let [c1 (conj coll 0)
              c2 (conj c1 (inc (apply max c1)))]
          (if (> (first c2) (apply max c1))
            :list
            :vector
            )
          )
        )
      )
    ))

(defcheck solution-ca77cdd0
  (fn [coll] (condp = (empty coll) #{} :set (hash-map) :map [] (condp = (conj (empty coll) 1 2) [1 2] :vector [2 1] :list nil))))

(defcheck solution-ca7ba5f7
  (fn
    [coll]
    (let [mcoll (conj (conj (conj coll [1 2]) [1 2]) [3 4])]
      (if (= (+ 3 (count coll)) (count mcoll))
        (if (and (= (first mcoll) [3 4]) (= (first (rest mcoll)) [1 2]))
          :list
          :vector)
        (if (= (get mcoll 3) 4)
          :map
          :set)))))

(defcheck solution-cb109ca6
  (fn [coll]
    #_(prn (conj coll {:a 1}))
    (cond
      (= (get (conj coll {:a 1}) :a) 1) :map
      (= (conj coll :x) (conj coll :x :x)) :set
      (= (first (conj coll :x :y)) :y) :list
      :else :vector)
    ))

(defcheck solution-cb6154b2
  (fn mtype[col] (

                   let [test1
                        (#(let [ex (empty %)]
                            (case ex
                              {} :map
                              [] :vector
                              #{} :set
                              )
                            ) col)]

                   (if (= test1 :vector)
                     (if (= (first (conj col 41 42)) 42)
                       :list
                       test1
                       )
                     test1
                     )
                   )))

(defcheck solution-cc57646a
  #(cond
     (not (ifn? %)) :list
     (not (associative? %)) :set
     (coll? (first (assoc % 0 2 ))) :map
     :else :vector))

(defcheck solution-cd1268e6
  (fn find-out [xs]
    (let [g    (gensym)
          gs   [0 g]
          gs2  [0 :a]
          yxs  (conj (conj xs [1 :b]) gs)
          yxs2 (conj yxs gs2)
          x    (first yxs)
          xxs  (conj yxs x)]
      (cond
        (= :a (get yxs2 0)) :map
        (= yxs xxs)         :set
        (= gs (first yxs))  :list
        (= gs (last  yxs))  :vector))))

(defcheck solution-cd3bb287
  (fn [coll]
    (if-not (coll? coll)
      nil
      (cond
        (and (associative? coll) (reversible? coll)) :vector
        (associative? coll) :map
        (= () (empty coll)) :list
        :else :set))))

(defcheck solution-cd6c7506
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (= [2 1] (seq (conj (empty %) 1 2))) :list
     :else :set))

(defcheck solution-cdbfa2e9
  (fn [thing]
    (let [newthing (conj (conj (conj thing [:a :a]) [:a :a]) [:a :b])]
      (cond
        (= (:a newthing) :b) :map
        (= 1 (count (filter #(= [:a :a] %) newthing))) :set
        (= (first newthing) [:a :b]) :list
        :else :vector))))

(defcheck solution-cdc4b497
  (fn foo [collection]
    (let [collection (empty collection)
          is-vector? (fn [col] (get (conj col [1 3]) 0))
          is-map? (fn [col] (get (conj col [1 3]) 1))
          is-set? (fn [col]
                    (when (not (is-map? col))
                      (let [start-size (count col)
                            new-size (count (conj (conj col [1 3]) [1 3]))]
                        (= 1 (- new-size start-size)))))]
      (cond
        (is-vector? collection) :vector
        (is-map?    collection) :map
        (is-set?    collection) :set
        :else                   :list))))

(defcheck solution-cdcfc1e3
  (fn [coll]
    (let [e (empty coll)]
      (cond
        (= (conj e [1 2]) {1 2}) :map
        (= (conj e 1 2) [1 2]) :vector
        (= (conj e 1 2) '(2 1)) :list
        (= (conj e 1 2) #{1 2}) :set))))

(defcheck solution-cde997c1
  (fn [thing]
    (let [e (empty thing)]

      (cond
        (= e {}) :map
        (= e #{}) :set
        (= (conj (conj e :a) :b) (list :b :a)) :list
        (= (conj (conj e :a) :b) [:a :b]) :vector))))

(defcheck solution-cdf4dc85
  (fn [c]
    (if (associative? c)
      (if (not (reversible? c)) :map :vector)
      (if (apply distinct? (conj c 1 1)) :set :list))))

(defcheck solution-ce3e9e7c
  #(cond
     (get (into % {nil true}) nil) :map
     (= (conj % 1) (conj % 1 1)) :set
     (= (conj % :1 :2) (conj (apply list (conj % :1)) :2)) :list
     :else :vector))

(defcheck solution-ce4586c6
  (fn [col]
    (let [a1   [::a 1]
          a2   [::a 2]
          col2 (conj col a1 a1 a2)
          diff (- (count col2) (count col))]
      (cond
        (get col2 ::a)      :map
        (= diff 2)          :set
        (= (first col2) a2) :list
        :else               :vector))))

(defcheck solution-ce7ee82c
  (fn my-type [x]
    (cond
      (reversible? x)  :vector
      (associative? x) :map
      (= (- (count (into x [:a :a])) (count x)) 2) :list
      :else            :set
      )))

(defcheck solution-d005ff1d
  #(let [c (conj % [:x :y] [:x :y] [:q :r])]
     (cond (= (get c :x) :y) :map
           (= (+ 2 (count %)) (count c)) :set
           (= (first c) [:q :r]) :list
           :else :vector)))

(defcheck solution-d044440e
  (fn [coll] (let [k (gensym) a {k 1} b {k 2}
                   consa (cons a coll)
                   consa2 (cons a consa)
                   conja (conj coll a)
                   conjb (conj coll b)
                   conja2 (conj conja a)
                   conja2b (conj conja2 b)
                   consa2b (cons b consa2)]
               (cond
                 (= consa2b conja2b) :list
                 (= conja (conj conjb a)) :map
                 (= conja conja2) :set
                 (= b (last conja2b) (first consa2b)) :vector
                 :else :unsupported))))

(defcheck solution-d08aae12
  (fn type_ [coll]
    (cond
      (= 1 (count (flatten [(conj coll [:c :b])])))
      (cond
        ((conj coll [:c :b]) [:c :b]) :set
        :else :map)
      :else
      (cond
        (= (cons :a (cons :b coll)) (conj (conj coll :b) :a)) :list
        :else :vector))))

(defcheck solution-d1055eb6
  (fn back-box-testing [x]
    (let [e (empty x)]
      (cond
        (= {} e) :map
        (= #{} e) :set
        (= '(2 1) (conj e 1 2)) :list
        :else :vector))))

(defcheck solution-d1345409
  (fn which [coll]
    (let [coll (conj (empty coll) [:a :b] [:a :b] [:c :d])]
      (if (= 2 (count coll))
        (if (coll [:a :b]) :set :map)
        (if (= [:a :b] (first coll)) :vector :list)))))

(defcheck solution-d14437dc
  #(if (= :bar (:foo (into % {:foo :bar})))
     :map
     (let [onetwo (conj (conj % 1) 2)
           twoone (conj (conj % 2) 1)]
       (cond
         (= onetwo twoone) :set
         (= (take 2 onetwo) '(2 1)) :list
         :else :vector))))

(defcheck solution-d16af15c
  (fn [coll] (condp = (empty coll) #{} :set {} :map () (if (= 2 (first (into (empty coll) [1 2]))) :list :vector))))

(defcheck solution-d1869cc0
  (fn [c] (let [ec (empty c)]
            (cond (and (associative? c) (coll? (first (into [] (assoc ec 0 :a))))) :map
                  (and (associative? c) (not (coll? (first (into [] (assoc ec 0 :a)))))) :vector
                  (and (not (associative? c)) (= 2 (first (conj ec 1 2)))) :list
                  (and (not (associative? c)) (= 1 (first (conj ec 1 2)))) :set))))

(defcheck solution-d1f906a1
  (fn [s]
    (cond
      (reversible? s) :vector
      (associative? s) :map
      :else
      (if-let [x (first s)] ;; is there stuff?
        (if (= (inc (count s)) (count (conj s x)))
          :list
          :set)
        (if (= (inc (count s)) (count (into s '(1 1))))
          :set
          :list)))))

(defcheck solution-d212b6cf
  (fn [c]
    (let [d (conj c [7 8])]
      (cond
        (= 8 (get d 7)) :map
        (= (get d [7 8]) [7 8]) :set
        (nil? (get d 0)) :list
        1 :vector))))

(defcheck solution-d235fcb
  (fn type? [col]
    (cond
      (and (associative? col) (not (reversible? col))) :map
      (= 1 (:test (frequencies (conj col :test :test)))) :set
      (= :test (last (conj col :notme :test))) :vector
      (= :test (first (conj col :notme :test))) :list
      :else :no)))

(defcheck solution-d295c61c
  (fn [col]
    (let [testCol (into (empty col) [[1 2] [1 2]])
          testColCount (count testCol)]
      (cond
        (= 1 testColCount) (cond
                             (= (testCol 1) 2) :map
                             :else :set)
        (= :v (first (conj testCol :v))) :list
        :else :vector))))

(defcheck solution-d31a83a1
  (fn [x] (cond
            (= {} (empty x)) :map
            (= #{} (empty x)) :set
            (= :a (first (into (empty x) '(:a :b)))) :vector
            :else :list)))

(defcheck solution-d373b56c
  (fn what-is-it
    [coll]
    (if (associative? coll)
      (if (reversible? coll)
        :vector
        :map)
      (if (= (conj coll :test) (cons :test coll))
        :list
        :set))))

(defcheck solution-d3a47325
  (fn [coll]
    (if (= (count (conj coll [:a 1])) (count (conj (conj coll [:a 1]) [:a 1])))
      (if (= 1 (get (conj coll [:a 1]) :a))
        :map
        :set)
      (if (= :b (first (conj coll :a :b)))
        :list
        :vector))))

(defcheck solution-d3cd8122
  #(if (= (conj %1 [:x :y] [:x :z]) (conj %1 [:x :z]))
     :map
     (if (= (conj %1 :x :x) (conj %1 :x))
       :set
       (if (= (last (conj %1 :x :y)) :y)
         :vector
         :list
         )
       )
     ))

(defcheck solution-d40c5f40
  (fn [c] (let [a [:a 1]
                t #(get (conj c a) %)]
            (cond (t :a) :map
                  (t a) :set
                  (t 0) :vector
                  1 :list))))

(defcheck solution-d45a6236
  (fn [x]
    (let [ex (empty x)
          t (conj ex [:a 0] [:a 0] [:a 1])
          ct (count t)]
      (cond (= 1 ct) :map
            (= 2 ct) :set
            (= 3 ct) (if (= [:a 1] (last t))
                       :vector
                       :list)))))

(defcheck solution-d470f46d
  (fn [s]
    (let [conj-s-nil (conj s nil)]
      (cond
        (= conj-s-nil s) :map
        (= conj-s-nil (conj conj-s-nil nil)) :set
        (= (first (conj conj-s-nil s)) s) :list
        :true :vector))))

(defcheck solution-d4bd5fae
  (fn [coll]
    (let [coll (conj (empty coll) [0 1])
          v (get coll 0)]
      (cond
        (= v 1) :map
        (= v [0 1]) :vector
        :else (if (= (get coll [0 1]) [0 1]) :set :list)))))

(defcheck solution-d4c65f48
  (fn [f]
    (cond
      (not (ifn? f)) :list
      (not (associative? f)) :set
      (= (empty f) {}) :map
      :else :vector)))

(defcheck solution-d4f37973
  (fn [x]
    (let [e (empty x)]
      (cond
        (= e {})  :map
        (= e #{}) :set
        :else (case (conj e 1 2)
                [1 2] :vector
                [2 1] :list)))))

(defcheck solution-d5050846
  (fn [-s]
    (let [
          -seq (conj (conj -s [:z 99]) [:x 77])
          -first (first -seq)
          -new-seq (conj -seq -first)
          -new-seq-bis (conj -seq [:z 100])
          ]
      (if (= (count -new-seq) (count -seq))
        (if (= (count -new-seq-bis) (count -seq))
          :map
          :set)
        (if (= (last -new-seq) -first)
          :vector
          :list
          )
        )
      )
    ))

(defcheck solution-d5847b1e
  (fn [coll]
    (if (not= (conj coll [:a :b]) (seq (conj coll [:a :b])))
      (if (associative? coll) :map :set)
      (if (= :xyz (first (conj coll :abc :xyz))) :list :vector))))

(defcheck solution-d68d3474
  (fn [x]
    (if (= (cons {0 1} (cons {2 3} x))
          (conj x {2 3} {0 1}))
      :list
      (if (= (conj x {0 2} {0 1})
            (conj x {0 1}))
        :map
        (if (= (conj x {0 1} {0 1})
              (conj x {0 1}))
          :set
          :vector)))))

(defcheck solution-d75cbbac
  (fn typify [seq]
    (let [e (empty seq)
          ea (conj e [:a 42])]

      (if (= 1 (count (flatten [ea])))
        (if (nil? (:a ea))
          :set
          :map)
        (let [eab (conj ea :b)]
          (cond
            (= :b (first eab))
            :list

            (= :b (last eab))
            :vector

            :default :hmmm))))))

(defcheck solution-d792ace3
  (fn [x]
    (let [ex (empty x)]
      (if (get (conj ex [1 2]) 1)
        :map
        (if (= 1 (count (conj ex 1 1)))
          :set
          (if (= 1 (first (conj ex 1 2)))
            :vector
            :list))))))

(defcheck solution-d80c3ae
  #(cond
     (= % (conj % {})) :map
     (= (conj % 10) (conj % 10 10)) :set
     (= (conj % 10 20) (concat % [10 20])) :vector
     :else :list))

(defcheck solution-d889ada
  (fn black-box-test [s]
    (let [sym-a (gensym)
          sym-b (gensym)]
      (cond
        (= s (conj s {})) :map
        (= (conj s sym-a sym-a) (conj s sym-a)) :set
        (= (first (conj s sym-a sym-b)) sym-b) :list
        :else :vector))))

(defcheck solution-d8e536d0
  (fn tpe [obj]
    (let [v [:test :result]
          obj (conj obj v)]
      (cond (:test obj)
            :map
            (= (conj obj v) obj)
            :set
            (= (last (conj obj :aaaa)) :aaaa)
            :vector
            :default :list))))

(defcheck solution-d8f1ccb4
  (fn [x]
    (if (= (set x) x)
      :set
      (if (associative? x)
        (if (reversible? x) :vector :map)
        :list
        )
      )
    ))

(defcheck solution-d8f8c0fd
  (fn
    [coll]
    (condp = (empty coll)
      {} :map
      #{} :set
      () (if (reversible? coll) :vector :list))))

(defcheck solution-d94410c4
  #(if (associative? %)
     (if (reversible? %)
       :vector
       :map)
     (if (ifn? %)
       :set
       :list)))

(defcheck solution-d94d6916
  (fn get-type [coll]
    (let [obj #?(:clj (Object.) :cljs #js {})
          obj-2 #?(:clj (Object.) :cljs #js {})
          c [[obj obj] [obj obj]]
          c-2 [[obj obj] [obj-2 obj-2]]
          size-before-test (count coll)
          after-test-1 (into coll c)
          after-test-2 (into coll c-2)
          size-after-test-1 (count after-test-1)]
      (cond
        (and (= size-after-test-1 (inc size-before-test)) (contains? after-test-1 obj)) :map
        (and (= size-after-test-1 (inc size-before-test)) (contains? after-test-1 [obj obj])) :set
        (and (= size-after-test-1 (+ size-before-test 2)) (= (last after-test-2) [obj-2 obj-2])) :vector
        (and (= size-after-test-1 (+ size-before-test 2)) (= (first after-test-2) [obj-2 obj-2])) :list
        )
      )

    ))

(defcheck solution-d94f8d48
  (fn find-type [coll]
    (let [c (count coll), g (gensym), g2 (gensym)]
      (cond
        (= c (count (conj coll {})))    :map
        (= (count (conj coll g))
          (count (conj coll g g)))     :set
        (= (first (conj coll g g2)) g2) :list
        (= (last (conj coll g g2)) g2)  :vector))))

(defcheck solution-d967c41f
  (fn [s]
    (let [result (conj (empty s) [1 2] [1 2] [1 3])]
      (cond
        (= 1 (count result)) :map
        (= 2 (count result)) :set
        (= [1 2] (first result)) :vector
        :else :list))))

(defcheck solution-d9bc2426
  (fn black-box
    [x]
    (cond
      (=
        (get (conj x {:a 2}) :a)
        2) :map
      (=
        (conj x (first x))
        (conj (conj x (first x)) (first x))) :set
      (=
        (conj x 23)
        (drop-last (conj (conj x 23) 24))) :vector
      :else :list
      )))

(defcheck solution-d9c56862
  (fn f [x]
    (let [fir #?(:clj (Object.) :cljs #js {})
          sec #?(:clj (Object.) :cljs #js {})
          c (conj (conj x {fir fir}) {fir sec})]
      (cond
        (= 1
          (count (filter #(and (coll? %) (= (first %) fir)) c))
          ) :map
        (= (count c) (count (conj c {fir sec})))
        :set
        (= (last c) {fir sec})
        :vector
        :else :list))))

(defcheck solution-d9cdd4c1
  (fn [coll]
    (let [elt [:b 2]
          coll (conj (empty coll) [:a 1] elt elt)]
      (cond
        (= (get coll :a) 1) :map
        (= (count coll) 2) :set
        (= (first coll) elt) :list
        :else :vector))))

(defcheck solution-d9ecc5d0
  #(case (empty %)
     #{} :set
     {} :map
     [] (if (first (conj (conj (empty %) false) true))
          :list
          :vector)))

(defcheck solution-da1cbe77
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (= (count (conj % :x)) (count (conj % :x :x))) :set
     1 :list))

(defcheck solution-da295777
  (fn [s]
    (let [C [:a :b]
          x (into (empty s) [C])
          added (lazy-seq (conj x :c))]
      (cond (= (:a x) :b) :map
            (= (conj x C) x) :set
            (= (last added) :c) :vector
            (= (first added) :c) :list
            )
      )
    ))

(defcheck solution-da4edb58
  #_(fn [coll]
      (case (->> coll empty str)
        "{}" :map
        "#{}" :set
        "[]" :vector
        "()" :list
        :dunno))

  (fn [coll]
    (let [z (empty coll)]
      (cond
        (= 1 (count (conj z [1 1] [1 2])))
        :map

        (= 1 (count (conj z [1 1] [1 1])))
        :set

        (= [1 1] (first (conj z [1 1] [2 2])))
        :vector

        (= [2 2] (first (conj z [1 1] [2 2])))
        :list

        :else :dunno))))

(defcheck solution-da595097
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {})  :map
        (= base #{}) :set
        (= base '()) (if (reversible? coll) :vector :list)))))

(defcheck solution-da8a7ea2
  #(let [tests {associative? 3
                sorted? 7
                counted? 15
                reversible? 31}
         check (fn [v] (reduce (fn [s [k i]] (+ s (or (and (k v) i) 0))) 0 tests))]
     (case (check %)
       18 :map
       49 :vector
       15 (let [v (conj % 0)] (if (= (count v) (count (apply conj v v))) :set :list))
       0 :list)))

(defcheck solution-dbac3e0b
  (fn [xs]
    (cond
      (= (get (conj xs [:x :y]) :x) :y) :map
      (= (dec (count (conj xs :x :x))) (count xs)) :set
      (= (first (conj xs :x :y)) :y) :list
      :else :vector)))

(defcheck solution-dc48f5ad
  (fn [x]
    (if (= 0 (count x))
      (cond (= {} x) :map
            (= #{} x) :set
            :else  (if (= 5 (first  (conj (conj x 3 ) 5)) )
                     :list
                     :vector
                     )
            )
      (let [sx (set x) x1 (first x)]
        (cond (= sx x) :set
              (= (conj x x1) x) :map
              (= (first (conj x :myd )) :myd) :list
              :else :vector
              )
        )
      )
    ))

(defcheck solution-dcb982dd
  (fn [c]
    (cond
      (associative? c) (if (= (into (empty c) {0 1}) {0 1})
                         :map
                         :vector)
      (> (count (conj c :a :a)) (count (conj c :a))) :list
      :else :set)))

(defcheck solution-dcda1b2d
  (fn [v]
    (let
     [ x (rand-int 100) v (conj v [:x x]) ]
      (if (associative? v)
        (if
         (get v :x) :map  :vector)
        (if
         (= (conj v [:x x]) v) :set :list)
        ))))

(defcheck solution-dd1080f9
  (fn [coll]
    (cond (= (set coll) coll) :set
          (= (apply list coll) coll) (if (= (conj coll :a :b) (cons :b (cons :a coll))) :list :vector)
          :else :map)))

(defcheck solution-dd5cea7a
  (fn blackbox
    [coll]
    (let [coll (conj coll [1 :xAaff])] ;make sure it's non empty
      (cond
        (= (conj coll [:xAaff :xAaff]) (concat coll [[:xAaff :xAaff]])) :vector
        (= (conj coll [:xAaff :xAaff]) (concat [[:xAaff :xAaff]] coll)) :list
        (= (count (conj coll [1 :xBaff])) (count coll)) :map
        :else :set))))

(defcheck solution-dd687a7f
  (fn [x]
    (cond
      (let [y (into x [[:magic true]])] (:magic y)) :map
      (let [y (filter (partial = :magic) (conj x :magic :magic))] (= (count y) 1)) :set
      (let [y (conj x 1 1 :magic)] (= :magic (first y))) :list
      :else :vector)))

(defcheck solution-ddcb061f
  (fn [thing]
    (let [empty-thing (empty thing)]
      (if (= 2 (count (conj empty-thing [1 1] [1 1])))
        (if (= [1 2] (conj empty-thing 1 2))
          :vector
          :list)
        (if (= 2 ((conj empty-thing [1 2]) 1))
          :map
          :set)))))

(defcheck solution-de375749
  (fn detect-type [a]
    (case
     (count (into (empty a) [[1 1] [1 1] [1 2]]))
      1 :map
      2 :set
      3 (case (conj (conj (empty a) 2) 1)
          [2 1] :vector
          [1 2] :list))))

(defcheck solution-de56d08e
  (fn [ coll ]
    (let [ f2 (conj (empty coll) [0 1] [0 1])]
      (if (= (count f2) 1)
        (if (= (count (conj f2 [0 2])) 1)
          :map
          :set)
        (if (first (conj f2 false))
          :vector
          :list)))))

(defcheck solution-de73b740
  (fn [sq]
    (case (str (empty sq))
      "()"  :list
      "{}"  :map
      "[]"  :vector
      "#{}" :set
      :list
      )
    ))

(defcheck solution-deb7343f
  (fn [s]
    (cond
      (= (set s) s) :set
      (= (vec s) s) (if (= (conj (conj s 1) 2)
                          (cons 2 (cons 1 s)))
                      :list
                      :vector)
      :else :map)))

(defcheck solution-ded84991
  (fn [s]
    (let [c (empty s)]
      (cond
        (= c {}) :map
        (= c #{}) :set
        (= (first (conj c 1 2)) 1) :vector
        :else :list))))

(defcheck solution-deec53fe
  (fn [s]
    (cond
      (reversible? s) :vector
      (associative? s) :map
      (< (inc (count s)) (count (conj (conj s 1) 1))) :list
      :else :set)))

(defcheck solution-df9c7a48
  (fn [s]
    (let [e (empty s)
          t1 (conj e {:joe :pete} {:joe :fred})]
      (if	(= (count t1) 1) :map
                            (let [t2 (conj e 1 1 2)]
                              (cond
                                (= (count t2) 2) :set
                                (= (first t2) 2) :list
                                :else :vector))))))

(defcheck solution-e032d1f9
  (fn [s]
    (if ;test adding same element twice
     (= (conj s [0 0]) (conj s [0 0] [0 0]))
      ; map/set
      (if ;test if pairs conj as k/v or element
       (contains? (conj s [:eof :eof]) :eof)
        :map
        :set)
      ; list/vector
      (if ;test if elements are conj'd at front or back
       (= :eof (first (conj s :bof :eof)))
        :list
        :vector))))

(defcheck solution-e0426f2
  #(let [d (empty %)]
     (if (= (count (conj d [0 1] [0 1])) 2)
       (if (= (first (conj d 1 2)) 1) :vector :list)
       (if (= (set d) d) :set :map))))

(defcheck solution-e0667ae2
  #(case (empty %) {} :map #{} :set (case (last (conj (empty %) 1 2)) 2 :vector 1 :list)))

(defcheck solution-e157792c
  #({"#{}" :set "{}" :map "[]" :vector} (str (empty %)) :list))

(defcheck solution-e188d610
  (fn [c] (let [h (first (str c))]
            (cond
              (= h \#) :set
              (= h \{) :map
              (= h \[) :vector
              (= h \() :list
              (= h \c) :list))))

(defcheck solution-e1b2e5df
  (fn bb [x] (if (associative? x) (if (empty? x)
                                    (if (= x {})
                                      :map
                                      :vector)
                                    (if (= (get x 0) (first x))
                                      :vector
                                      :map))
                                  (if (and (= (first (conj x 'a)) 'a) (= (+ (count x) 2) (count (conj x 'a 'a)))) :list :set))))

(defcheck solution-e1c9042e
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        :else (if (reversible? coll) :vector :list)))))

(defcheck solution-e212b954
  #(let [a '[a b]
         b '[a c]
         x (conj % a)
         n (count x)
         y (conj x b)]
     (cond
       (= n (count y))  :map
       (= x (conj x a)) :set
       (= b (first y))  :list
       (= b (last y))   :vector)))

(defcheck solution-e252591
  (fn [c] (let [c1 (count c)
                s1 (keyword (gensym))
                s2 (keyword (gensym))
                c' (conj c [s1 s2] [s1 s2])]
            (if (= (inc c1) (count c'))
              (if (c' s1) :map :set)
              (if (= s2 (first (conj c s1 s2)))
                :list
                :vector)))))

(defcheck solution-e27847e6
  (fn tipe [s]
    (let [sym1  (gensym) sym2 (gensym)
          sym3  (gensym) s12  (conj s {sym1 sym2})
          s1221 (conj (conj s {sym2 sym1}) s12)]
      (if (get s12 sym1) :map
                         (if (get s12 {sym1 sym2}) :set
                                                   (if (= (first s1221) s12) :list :vector))))))

(defcheck solution-e286fc6a
  (fn[x]
    (let [t (conj (empty x) [:a :b] [:c :d])]
      (cond
        (:a t) :map
        (get t 0) :vector
        (get t [:a :b]) :set
        :else :list))))

(defcheck solution-e30dc8a5
  (fn [coll]
    (let [
          ismap (:test (conj coll [:test true]))
          isset  (= 1 (- (count (conj coll [:unique 1] [:unique 1])) (count coll)))
          islist (= [:last 2] (first (conj coll [:first 1] [:last 2])))
          ]
      (cond ismap :map
            isset :set
            islist :list
            true :vector
            )
      )
    ))

(defcheck solution-e3118ade
  (fn [col]
    (cond
      (= :b (get (conj col [:a :b]) :a)) :map
      (= (conj col :a) #{[:a :b]}) :set
      (= (count (conj col :c :c)) (+ 1 (count col))) :set
      (= (last (conj col :d :c )) :c) :vector
      (= (first (conj col :d :c )) :c) :list
      :else :none)))

(defcheck solution-e36c2816
  #(if (ifn? %) ({{} :map, #{} :set, [] :vector} (empty %)) :list))

(defcheck solution-e469b9e6
  (fn [coll]
    (cond (= coll (conj coll {})) :map
          (or
           (and (empty? coll) (= #{} coll))
           (and (not (empty? coll)) (= (count coll) (count (conj coll (first coll)))))
           ) :set
          (and
           (= 2 (last (conj coll 1 2)))
           (= 1 (last (conj coll 2 1)))
           ) :vector
          :else :list)))

(defcheck solution-e494bb80
  #(cond
     (reversible? %) :vector
     (associative? %) :map
     (< (count (conj % 2)) (count (conj % 2 2))) :list
     :else :set))

(defcheck solution-e4b14acd
  (fn [x]
    (cond
      (reversible? x)  :vector
      (associative? x) :map
      (= (set x) x)    :set
      :else            :list)))

(defcheck solution-e5233da
  (fn whatmap [coll]
    (let [
          x {:a 1}
          y {:a 2}
          coll' (conj (conj coll x)y)
          ]
      (cond
        (= 1 (count (flatten [coll'])))
        (cond
          (= (count coll') (+ (count coll) 2)) :set
          :else :map
          )
        (= (last coll') y) :vector
        :else :list))))

(defcheck solution-e53edcd9
  (fn [x]
    (cond
      (= {} (empty x)) :map
      (= #{} (empty x)) :set
      (= () (empty x)) (if (= (first (conj (empty x) 1 2)) 2) :list :vector)
      )))

(defcheck solution-e55c575d
  (fn aa [x]
    (if (empty? x)
      (cond
        (= x {})  :map
        (= x #{}) :set
        (= (apply conj x (range 5)) (range 5))  :vector
        :else :list
        )

      (cond
        (coll? (first x)) :map
        (= (conj x (first x)) x) :set
        (= (conj x 1) (cons 1 x)) :list
        :else :vector
        )
      )
    ))

(defcheck solution-e5dd7dad
  (fn [coll]
    (let [item [:a 1] c (conj (conj (empty coll) item) item)]
      (if (= 1 (count c))
        (if (get c :a) :map :set)
        (if (= 1 (first (conj c 1))) :list :vector)))))

(defcheck solution-e5e174b0
  (fn bb [coll]
    (let [coll (empty coll)
          m {{} :map #{} :set}
          r (m coll) ]
      (if (nil? r)
        (if (= [1 2] (conj coll 1 2))
          :vector
          :list)
        r))))

(defcheck solution-e5eab530
  (fn box [coll]
    (cond
      (= (set coll) coll)
      :set
      (= (vec coll) coll)
      (let [new-coll (conj (conj coll :added-first) :added-second)]
        (if (= :added-second (first new-coll))
          :list
          :vector))
      :else
      :map)))

(defcheck solution-e638abe0
  #(let [x (empty %)]
     (cond
       (= #{} x) :set
       (= {} x) :map
       (= [:a :b] (conj (conj x :a) :b)) :vector
       (= [:b :a] (conj (conj x :a) :b)) :list)))

(defcheck solution-e69c00df
  (fn [c]
    (cond (not (ifn? c)) :list
          (reversible? c) :vector
          (associative? c) :map
          :else :set)))

(defcheck solution-e6c8348d
  (fn [coll]
    (let [x (conj coll {:key :val})]
      (if (= (get x :key) :val)
        :map
        (if (= (get x {:key :val}) {:key :val})
          :set
          (if (= (first (conj x :xxx)) :xxx)
            :list
            :vector
            )))
      )))

(defcheck solution-e6f8da22
  (fn black-box-type [coll]
    (cond
      (= (get (conj coll [:t "t"]) :t) "t") :map
      (= (get (conj coll :t) :t) :t) :set
      (= (first (conj (conj coll :a) :b)) :b) :list
      (= (last (conj (conj coll :a) :b)) :b) :vector)))

(defcheck solution-e7005fa8
  (fn bbtest [s]
    (cond
      ; (conj map map) -> map
      ; keyword applied to map acts as function
      (= (:c (conj s {:c 3})) 3) :map
      ; order of set elements is irrelevant
      (= (conj (conj s 3) 4) (conj (conj s 4) 3)) :set
      ; conj adds to end of vectors
      (= (conj (conj s 3) 4) (concat s '(3 4))) :vector
      ; conj adds to start of lists
      (= (conj (conj s 3) 4) (concat '(4 3) s)) :list
      :else :unknown)))

(defcheck solution-e7a919ce
  (fn [c] (let [t (into c [[:s :b][:s :b][:s :c]])
                d (- (count t) (count c))]
            (cond
              (= 1 d) :map
              (= 2 d) :set
              (= [:s :c] (first t)) :list
              :else :vector))))

(defcheck solution-e7f5f35a
  (fn wat? [x]
    (if (= (conj x x) x)
      :map
      (let [z (conj x :z)
            sj (conj z z)]
        (cond
          (= (conj sj z) sj) :set
          (= (first sj) z) :list
          (= (last sj) z) :vector)))))

(defcheck solution-e898e384
  (fn t [obj]
    (cond
      (= (get (conj obj {:__test 1}) :__test) 1) :map
      (= (inc (count obj)) (count (conj obj :__test :__test))) :set
      (= (first (conj obj :a :b)) :b) :list
      :else :vector)))

(defcheck solution-e90db162
  (fn [s]
    (cond (= (conj s {}) s) :map
          (empty? s) (cond (= s #{}) :set
                           (= (conj (conj s 0) 1) '(1 0)) :list
                           :else :vector)
          (= (rest (conj s 1)) s) :list
          (= (clojure.set/union (set s) (set s)) s) :set
          :else :vector
          )
    ))

(defcheck solution-e93143d3
  #(cond
     (= (conj % nil) %)                :map
     (= (conj % nil) (conj % nil nil)) :set
     (nil? (first (conj % 0 nil)))     :list
     :else                             :vector))

(defcheck solution-e9628425
  #(cond
     (= (into % {:k :v}) (conj % {:k :v})) :map
     (= (conj (conj % :x) :y) (cons :y (cons :x %))) :list
     (= (conj % :x) (seq (conj % :x))) :vector
     :else :set))

(defcheck solution-e9c6ec8b
  (fn[coll]
    (let [obj #?(:clj (Object.) :cljs (js-obj))]
      (let [x (conj coll [1 2])]
        (cond
          (not= (conj x [:a :b]) (seq (conj x [:a :b]))) (if (associative? x) :map :set)
          (= (first (conj x obj)) obj) :list
          :else :vector
          )
        ))))

(defcheck solution-e9f1414b
  (fn [x] (if
           (or (= x []) (= x (seq x))) ; if true, either x is an empty list or vector, or x is a sequence
            (if
             (and (not= (conj (conj x 1) 2) [1 2]) ; fails for empty vector, but nothing else
                  (= x (first (conj x x)))) ; passes for list, fails for non-recursive non-empty vector
              :list :vector)
            (if
             (or (= x #{}) ; passes for empty set, but nothing else
                 (and
                  (not= x {}) ; fails for empty map, but nothing else
                  (= 1 (count (flatten [(first x)]))))) ; passes for nonempty set, fails for nonempty map
              :set :map))))

(defcheck solution-ea3f8f1f
  (fn guesswhat[x] (if (associative? x) (if(= (apply vector x) x) :vector :map) (if(ifn? x) :set :list))))

(defcheck solution-eb290cc5
  (fn blackbox-type [coll]
    (if (associative? coll)
      (if (reversible? coll)
        :vector
        :map)
      (if (ifn? coll)
        :set
        :list))))

(defcheck solution-eb4bb961
  (fn [thing]
    (cond (let [sym (gensym)]
            (= (count (conj thing
                        [sym (gensym)]
                        [sym (gensym)]))
              (inc (count thing))))
          :map
          (< (count (conj thing 1 1))
            (+ 2 (count thing)))
          :set
          (let [syma (gensym)
                symb (gensym)]
            (= (conj thing syma symb)
              (list* symb syma thing)))
          :list
          :else
          :vector)))

(defcheck solution-eb8cd8e2
  #(condp = (conj (empty %) [:x :y] [:x :z])  '([:x :z] [:x :y]) :list {:x :z} :map #{[:x :y] [:x :z]} :set [[:x :y] [:x :z]] :vector))

(defcheck solution-ebbcbf8b
  (fn [collection]
    (let [bi-test (conj collection [:first :second])]
      (if (= (:first bi-test) :second)
        :map
        (let [uni-test (conj collection :first :second)]
          (cond
            (= (:first uni-test) :first) :set
            (= (last (seq uni-test)) :second) :vector
            (= (first (seq uni-test)) :second) :list))))))

(defcheck solution-eca87904
  (fn what [input]
    (let [empty-coll (empty input)
          coll (into empty-coll [[1 2] [1 2]])]
      (if (= 1 (count coll))
        (if (get coll 1)
          :map
          :set)
        (if (= 1 (first (into empty-coll [1 2])))
          :vector
          :list)))))

(defcheck solution-eca888dd
  #(let [t (conj (empty %) [:k :v])]
     (cond
       (:k t) :map
       (get t 0) :vector
       (get t [:k :v]) :set
       :else :list)))

(defcheck solution-ecb85a4a
  ;; One big clue: The function empty is not forbidden in this problem.

  (fn [c]
    (let [e (empty c)]
      (case e
        {} :map
        [] (if (reversible? c) :vector :list)
        #{} :set))))

(defcheck solution-eda6d0a0
  (fn bbt[l]
    (let [l (empty l)]
      (cond
        (= {} l) :map
        (not= (+ 2 (count l)) (count (conj l 2 2))) :set
        (= 3 (last (conj (empty l) 1 2 3))) :vector
        :else :list
        ))))

(defcheck solution-ee1ce32f
  (fn [coll]
    (let [e (empty coll)]
      (cond
        (= e #{}) :set
        (= e {})  :map
        (= (into e [1 2]) [2 1]) :list
        true :vector))))

(defcheck solution-ee698274
  (fn ident [coll]
    (if (= (get (conj coll [:blah 1]) :blah) 1)
      :map ; adding a key can then be 'got'
      (if (= (conj coll :a :a) (conj coll :a))
        :set ; adding same thing twice same as adding once
        (if (= :blah2 (first (conj coll :blah1 :blah2)))
          :list ; list and vector add to opposite ends
          :vector)))))

(defcheck solution-ee9cbc5c
  (fn cat [in]
    (let [empt (empty in)]
      (case empt
        {} :map
        #{} :set
        [] (if (associative? in) :vector :list)))))

(defcheck solution-eeaa0da9
  (fn bb [n]
    (if (= n (into [] n))
      (if (= (first (conj n :q1 :q2 )) :q2 )
        :list :vector )
      (if (= n (into #{} n))
        :set :map ))))

(defcheck solution-eedf0bd3
  (fn [n]

    (let [o (conj n [:x 0] [:x 0] [:x 1] [:x 1])]

      ((zipmap [[1 false] [2 false] [2 true] [4 false] [4 true]]
         [:map :set :set :vector :list])
       (vector (- (count o) (count n) ) (= (second o) [:x 1]))))))

(defcheck solution-ef1d6c4f
  (fn [d]
    (let [r (conj d [:y nil] [:z 42])]
      (cond
        (= 42 (:z r)) :map
        (= [:z 42] (get r [:z 42])) :set
        (= [:z 42] (first r)) :list
        (= [:z 42] (last r)) :vector))))

(defcheck solution-ef213359
  (fn __ [bb]
    (let [fc (first (str bb))]
      (cond
        (= fc \{) :map
        (= fc \[) :vector
        (= fc \#) :set
        :else :list))))

(defcheck solution-ef97f3a0
  #(get {\[ :vector, \{ :map, \# :set} (first (str %)) :list))

(defcheck solution-f0ba7b60
  (fn [c]
    (cond
      (reversible? c) :vector
      (associative? c) :map
      (or (= c '()) (= c (seq c))) :list
      :default :set)))

(defcheck solution-f151f4e3
  (fn [coll]
    (let [base (empty coll)]
      (cond
        (= base {}) :map
        (= base #{}) :set
        (= base '()) (if (reversible? base) :vector :list )
        ))))

(defcheck solution-f1f6e664
  (fn
    [v]
    (let [f (str (first (str `~v)))]
      (cond
        (= f "[") :vector
        (= f "#") :set
        (= f "{") :map
        :d :list))))

(defcheck solution-f2531e7c
  #(let [e (empty %)]
     (cond (= #{} e) :set
           (= {} e) :map
           :else (if (= (first (conj (into e [1 2 3]) 2)) 2)
                   :list
                   :vector))))

(defcheck solution-f39f4863
  (fn check [s]
    (let [x (empty s)]
      (cond
        (= x {}) :map
        (= x #{}) :set
        (= (conj x 1 2) (list 2 1)) :list
        :else :vector
        ))))

(defcheck solution-f3ef376e
  (fn a [coll]
    (cond
      (= coll (vec coll))
      (let [items (conj coll :x :y)]
        (if (= (last items) :y)
          :vector
          :list
          )
        )
      (= coll (set coll)) :set
      :else :map
      )
    ))

(defcheck solution-f44c972
  (fn [x]
    (cond (reversible? x) :vector
          (associative? x) :map
          (= (vec x) x) :list
          :otherwise :set)))

(defcheck solution-f4535e43
  (fn [s]
    (let [e (conj (empty s) [0 0] [0 1] [0 1])]
      (cond
        (= (count e) 1) :map
        (= (count e) 2) :set
        (= (first e) [0 1]) :list
        :else :vector))))

(defcheck solution-f4d29c95
  #(let [x (gensym) y (gensym) z (gensym)]
     (cond
       (= (inc (count %)) (count (conj % {x y} {x z}))) :map
       (= (inc (count %)) (count (conj (conj % x) x))) :set
       (= (conj (conj % x) y) (cons y (cons x %) )) :list
       (= (conj (conj % x) y) (conj % x y)) :vector
       )))

(defcheck solution-f5292c6a
  (fn  [x]
    (let [e (empty x)]
      (cond
        (= {} e) :map
        (= #{} e) :set
        (= '(2 1) (conj e 1 2)) :list
        :else :vector))))

(defcheck solution-f58667eb
  (fn [col]
    (let [
          additem [1 2]
          qq (conj col [666 777] additem additem)
          countqq (count qq)
          ]
      (if (= countqq (+ 3 (count col)))
        ;list or vector
        (if (= additem (first qq))
          :list
          :vector)
        ;map or set
        (if (contains? qq additem)
          :set
          :map)))))

(defcheck solution-f5c55fb0
  (fn [coll]
    (cond
      (= 99 (:x (conj coll [:x 99]))) :map
      (= :x (:x (conj coll :x))) :set
      (= [13 37] (first (-> coll (conj coll [11 11]) (conj coll [13 37])))) :list
      (= [13 37] (last (-> coll (conj coll [11 11]) (conj coll [13 37])))) :vector
      :else :unknown)))

(defcheck solution-f5c8c0d9
  #(let [x (gensym)
         y (gensym)
         z (conj % [x y])]
     (cond (get z x)     :map
           (get z [x y]) :set
           (get z 0)     :vector
           :else         :list)))

(defcheck solution-f643c1c7
  (fn [coll]
    (condp = (empty coll)
      #{} :set
      {}  :map
      ()  (if (reversible? coll) :vector :list))))

(defcheck solution-f6f3b348
  (fn [coll]
    (let [c (conj (empty coll) {1 2 3 4} {5 6})]
      (cond
        (= 3 (count c)) :map
        (= 1 (count (flatten [c]))) :set
        (= {5 6} (first c)) :list
        :else :vector))))

(defcheck solution-f6f6542b
  (fn blackbox [s]
    (cond
      (= (conj s {}) s) :map
      (empty? s) (cond
                   (= s #{}) :set
                   (= (conj (conj s 0) 1) [0 1]) :vector
                   :else :list)
      (= (clojure.set/union (set s) (set s)) s) :set
      (= (first (conj s s)) s) :list
      :else :vector)))

(defcheck solution-f6ff1e09
  #(if (associative? %)
     (if (reversible? %)
       :vector
       :map)
     (if (= (count (conj % :a)) (count (conj % :a :a)))
       :set
       :list)))

(defcheck solution-f7444864
  (fn [x]
    (let [xx (conj (conj x [:q 1]) [:r 2])]
      (cond
        (= (get xx :q) 1) :map
        (= (concat x [[:q 1] [:r 2]]) xx) :vector
        (= (concat [[:r 2] [:q 1]] x) xx) :list
        true :set))))

(defcheck solution-f764b47d
  (fn [coll]
    (let [c (conj coll [0 0] [1 1])]
      (if (= c (conj c [0 0]))
        (if (contains? (conj c [c c]) c)
          :map
          :set)
        (let [f (first c)]
          (if (= f (first (conj c [f f])))
            :vector
            :list))))))

(defcheck solution-f790881b
  (fn t[x]
    (cond
      (:x (conj x {:x 1})) :map
      (:x (conj x :x)) :set
      (= :y (-> x (conj :x) (conj :y) first)) :list
      :else :vector)))

(defcheck solution-f7d476db
  #((zipmap (map str ['() [] {} #{}]) [:list :vector :map :set]) (str (empty %))))

(defcheck solution-f7de415e
  (fn black-box
    [coll]
    (let [test-pair [:a :a]
          test-pair-2 [:b :b]]
      (letfn [(not-nil? [item]
                (not (nil? item)))
              (test-list [given-coll]
                (= test-pair-2 (first (conj given-coll test-pair test-pair-2))))
              (test-vec [given-coll]
                (= test-pair-2 (last (conj given-coll test-pair test-pair-2))))
              (test-map [given-coll]
                (= given-coll (conj given-coll given-coll)))
              (test-set [given-coll]
                (not-nil? (get (conj given-coll test-pair) test-pair)))]
        (cond ;; map and set branch first because map may also
          ;; return true for the vector test
          (test-map coll) :map
          (test-set coll) :set
          (test-list coll) :list
          (test-vec coll) :vector)))))

(defcheck solution-f7f2c13e
  #(if (associative? %)
     (if (coll? (first (assoc % 0 100)))
       :map
       :vector)
     (if (= (count (conj % 1)) (count (conj % 1 1)))
       :set
       :list)))

(defcheck solution-f827f47c
  (fn seq-type [s]
    (cond
      (= (conj s [1 2]) (conj (conj (conj s [1 2]) [1 3]) [1 2]))
      :map
      (= (conj (conj s 1) 1) (conj s 1))
      :set
      (= (first (conj (conj s 1) 2)) 2)
      :list
      true
      :vector)))

(defcheck solution-f8442367
  (fn kiu? [x] (let [z (vector :e1 :e2) tmp0 (conj x (vector :e2 :e1)) tmp1 (conj tmp0 z) ]
                 (if (= (concat tmp1 [z]) (conj tmp1 z))
                   :vector
                   (if (= tmp1 (conj tmp1  z))
                     (if (= (tmp1 :e1) :e2)
                       :map
                       :set)
                     :list)))))

(defcheck solution-f9148cbf
  (fn [coll]
    (cond
      (= (conj coll [:test 0] [:test 1])
        (conj coll [:test 1])) :map
      (= (conj coll [:test 0] [:test 0])
        (conj coll [:test 0])) :set
      (= (last (conj coll :test :test1))
        :test1) :vector
      :else :list)))

(defcheck solution-f99eeb93
  (fn [s]
    (let [e (empty s)]
      (cond
        (= {} e) :map
        (= #{} e) :set
        (= (first (conj e 1 2)) 1) :vector
        :else :list))))

(defcheck solution-fa4c60d5
  #(if-not
    (ifn? %) :list
             (let [m (conj % [:k :v])]
               (cond
                 (contains? m :k) :map
                 (contains? m [:k :v]) :set
                 :else :vector))))

(defcheck solution-faf37d35
  #(let [c1 (conj % [:foo 42])]
     (if (= c1 (seq c1))
       (let [c2 (conj c1 :bar)]
         (if (= (first c2) :bar) :list :vector))
       (if (:foo c1) :map :set))))

(defcheck solution-faf6dc4f
  #(let [e (empty %) k ({{} :map #{} :set} e)]
     (if k k
           (if (= (first (conj e 1 2)) 2) :list :vector))))

(defcheck solution-fb304bb7
  (fn [x]
    (let [mt (empty x)]
      (cond (= mt #{}) :set
            (= mt {}) :map
            :else
            (cond (reversible? x ) :vector
                  :else :list))
      )
    ))

(defcheck solution-fb87e83c
  (fn [col]
    (cond
      (reversible? (empty col)) :vector
      (= {} (empty col)) :map
      (= #{} (empty col)) :set
      :else :list)))

(defcheck solution-fbaa82dd
  (fn type-detect [obj]
    (cond
      (and (associative? obj) (not (reversible? obj)))        :map
      (and (associative? obj) (reversible? obj))              :vector
      (= 1 (count (into (empty obj) [:fizz :fizz])))          :set
      (= :second (first (into (empty obj) [:first :second]))) :list)))

(defcheck solution-fbe28843
  (fn [coll]
    (let [start-count (count coll)
          a (gensym "a")
          b (gensym "b")
          modified (conj coll [a :a] [a :a] [a :b] [b :c])
          size-diff (- (count modified) start-count)]
      (cond
        (= size-diff 2) :map
        (= size-diff 3) :set
        (= size-diff 4)
        (if (= (first modified) [b :c])
          :list
          :vector)))))

(defcheck solution-fbe7f5a1
  (fn [c] (if (:z (conj c [:z :x])) :map
                                    (let [s (conj c :z :x)]
                                      (if (:z s) :set (if (= :x (last s)) :vector :list))))))

(defcheck solution-fbed3ec6
  (fn [x] (cond (= x (into [] x))
                (if (= (first (conj x :b :a)) :a) :list :vector)
                (= x (into #{} x)) :set
                true :map)))

(defcheck solution-fbf91824
  (fn black-box
    [x] (let [s gensym]
          (if (= (+ 2 (count x)) (count (conj x [s s] [s s] [s 0])))
            :set
            (if (= (+ 1 (count x)) (count (conj x [s s] [s s] [s 0])))
              :map
              (if (= [s s] (first (conj x [0 0] [s s])))
                :list
                :vector))))))

(defcheck solution-fc7a6db6
  (fn black-box [coll]
    (let [a :some-random-item]
      (if (reversible? coll) :vector
                             (if (associative? coll) :map
                                                     (if (= 1 (- (count (conj coll a a a)) (count coll))) :set :list))))))

(defcheck solution-fd3b9632
  (fn [x]
    (if (= :b (:a (conj x [:a :b]))) :map
                                     ( let [y (conj (empty x) :a :b :a :b)]
                                       (cond
                                         (= 2 (count y)) :set
                                         (= :b (last y)) :vector
                                         (= :a (last y)) :list
                                         )))))

(defcheck solution-fd8cf2fc
  (fn [s]
    (if (associative? s)
      (if (reversible? s)
        :vector
        :map)
      (let [t (conj (conj s :magic) :magic)
            d (- (count t) (count s))]
        (if (= d 1)
          :set
          :list)))))

(defcheck solution-fdc87100
  #(let [a (conj % [1 2])] (if (= (into [] a) a) (if (= (conj a :Z) (cons :Z a)) :list :vector) (if (or (= {1 2} a) (coll? (first %)) ) :map :set))))

(defcheck solution-fdff1165
  #(cond
     (get (into % [[::a true]]) ::a)  :map
     (= (into % [1]) (into % [1 1]))  :set
     (= ::c (first (conj % ::b ::c))) :list
     :else                            :vector))

(defcheck solution-fe0fba7c
  (fn typ [c]
    (let [nc (conj c [:test 123])]
      (if (not= nc (seq nc))
        (if (= 123 (nc :test)) :map :set)
        (if (= (cons 1 nc) (conj nc 1)) :list :vector)))))



(defcheck solution-ff5241de
  (fn [col]   (letfn [(is-set-or-map? [c]
                        (= (conj c [:a 3])
                          (conj (conj c [:a 3]) [:a 3])))
                      (is-map? [c]  (contains? (conj c [:a 3]) :a))
                      (is-vector? [c] (= 1 (first (conj (conj c 1) 2))))]
                (if (is-set-or-map? col)
                  (if (is-map? col)
                    :map
                    :set)
                  (if (is-vector? col)
                    :vector
                    :list)))))

(defcheck solution-ff89cf0a
  (fn seq-type
    [s]
    (cond
      (= (conj s [1 1] [1 2]) (conj s [1 2])) :map
      (= (conj s 1 1) (conj s 1)) :set
      (= (conj s s 1) (cons 1 (cons s s))) :list
      :else :vector)))

(defcheck solution-ff8dfd5
  #(get {\# :set \{ :map \( :list \[ :vector}
     (first (.toString %)) :list))

(defcheck solution-ffb1f4b7
  (fn tst [y] (let [x (conj y [1 2] [3 4])]
                (let [x2 (conj x (first x))]
                  (if (not= x2 x) (if (= (second x2) (second x)) :vector :list)
                                  (if (= x (set x)) :set :map))))))

(defcheck solution-fff2727
  #(let [x [:x 2]
         l (conj (conj (conj % [:x 1]) x) x)
         i (count %)
         j (count l)]
     (cond
       (= j (inc i)) :map
       (= j (+ 2 i)) :set
       (= x (first l)) :list
       true :vector)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-65))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
