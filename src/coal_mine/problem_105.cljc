(ns coal-mine.problem-105
  (:require [coal-mine.checks :refer [defcheck-105] :rename {defcheck-105 defcheck}]
            [clojure.set]
            [clojure.test]))

(defcheck solution-10449cfd
  (fn p105 [xs]
    (let [acc (fn [[m k] x]
                (if (keyword? x)
                  (vector (assoc m x []) x)
                  (vector (update-in m [k] conj x) k)))]
      (first (reduce acc [{}] xs)))))

(defcheck solution-1053b686
  #(loop [r {} [k & n] %]
     (if n
       (let [[a b] (split-with integer? n)]
         (recur (assoc r k a) b))
       r)))

(defcheck solution-10753f58
  (fn identify
    ([s]
     (identify s {} (first s)))
    ([s r k]
     (if (empty? s)
       r
       (if (keyword? (first s))
         (recur (rest s) (assoc r (first s) []) (first s))
         (recur (rest s) (assoc r k (conj (r k) (first s))) k))))))

(defcheck solution-10a26afa
  (fn self [xs]
    (if (empty? xs)
      {}
      (let [x  (first xs)
            rs (rest xs)]
        (merge (self (drop-while number? rs))
          {x, (take-while number? rs)})))))

(defcheck solution-1156fc62
  (fn [s] (apply hash-map
            (reduce #(if (keyword? %2)
                       (conj % %2 [])
                       (conj (pop %) (conj (peek %) %2)))
              [] s))))

(defcheck solution-11b6ca3
  (fn create-map [xs]
    (loop [ys           xs
           acc          {}
           last-keyword nil]
      (if (empty? ys)
        acc
        (let [next-elem   (first ys)
              tail        (rest ys)
              new-acc     (if (keyword? next-elem)
                            (merge acc {next-elem []})
                            (update-in acc [last-keyword] conj next-elem))
              new-keyword (if (keyword? next-elem)
                            next-elem
                            last-keyword)]
          (recur tail new-acc new-keyword))))))

(defcheck solution-128bd750
  #(loop [[k & y] % r {}]
     (if k
       (let [[a b] (split-with number? y)]
         (recur b
           (assoc r k a)))
       r)))

(defcheck solution-1338a10d
  (fn f [[k & coll]]
    (if (nil? k)
      {}
      (let [nk (complement keyword?)
            vs (vec (take-while nk coll))]
        (assoc (f (drop-while nk coll)) k vs)))))

(defcheck solution-13cbbc1b
  (fn number105 [xs]
    (let [kvs (reduce #(if (keyword? %2)
                         (assoc %1 :ks (conj (:ks %1) %2) :vs (conj (:vs %1) []))
                         (assoc-in %1 [:vs (dec (count (:vs %1)))] (conj (last (:vs %1)) %2)))
                {:ks [] :vs []} xs)]
      (zipmap (:ks kvs) (:vs kvs)))))

(defcheck solution-140e7c43
  (fn [l] (first (reduce (fn [[h kw] x] (if (keyword? x) [(assoc h x []) x] [(update-in h [kw] conj x) kw])) [{} nil] l))))

(defcheck solution-14128661
  (fn [xs]
    (into {}
      (reduce
        (fn [coll n]
          (let [
                last-elem (vec (last coll))
                my-sq     (vec (take (dec (count coll)) coll))
                ]
            (if (keyword? n)
              (conj coll [n []])
              (conj my-sq [(first last-elem) (conj (last last-elem) n)]))
            )
          )
        []
        xs
        ))))

(defcheck solution-14344aab
  #(loop [[ks vs & sequence] (partition-by keyword? %)
          result (sorted-map)]
     (if (not ks) result
                  (recur sequence
                    (merge result
                      {(last ks) (vec vs)}
                      (reduce merge
                        (map (fn [x] {x []})
                          (butlast ks))))))))

(defcheck solution-143e9a52
  (fn kav [s]
    (loop [m           {}
           remain      s
           current-key nil]
      (if (empty? remain) m
                          (let [elt        (first remain)
                                new-remain (rest remain)
                                elt-key    (keyword? elt)
                                new-key    (if elt-key elt current-key)
                                old-vec    (if elt-key [] (m current-key))
                                new-vec    (if elt-key [] (conj old-vec elt))
                                new-map    (assoc m new-key new-vec)]
                            (recur new-map new-remain new-key))))))

(defcheck solution-14543b23
  #(loop [[[k] & v]
          (partition-by
            (fn [x] (or (number? x) x)) %)
          o {}]
     (if k
       (if (and v (number? (ffirst v)))
         (recur (rest v) (conj o {k (first v)}))
         (recur v (conj o {k []})))
       o)))

(defcheck solution-14bb4f0d
  (fn [co]
    (letfn [(tnk [c] (take-while #(not (keyword? %)) c))
            (dnk [c] (drop-while #(not (keyword? %)) c))
            (hashify [c]
              (into {}
                (map
                  #(apply vector %)
                  (partition 2 c))))
            (ksep [c]
              (loop [res [(first c)] left (rest c)]
                (if (empty? left)
                  (hashify (butlast res))
                  (recur (conj res (tnk left) (first (dnk left)))
                    (rest (dnk left))))))]
      (ksep co))))

(defcheck solution-14e6abe3
  (fn [v]
    (into {} (reduce
               #(if (keyword? %2)
                  (cons [%2 []] %1)
                  (let [[k v] (first %1)]
                    (cons [k (conj v %2)] (rest %1))))
               '() v))))

(defcheck solution-155bdd2e
  #(first (reduce (fn [[state lastkey] item]
                    (if (keyword? item)
                      [(assoc state item []) item]
                      [(update-in state [lastkey] conj item) lastkey]))
            [{} nil] %)))

(defcheck solution-15d0bc99
  (fn [xs]
    (letfn [(to-map [xs m]
              (if (seq xs)
                (let [k (first xs)
                      [v r] (split-with #(not (keyword? %)) (rest xs))]
                  (recur r (assoc m k v)))
                m))]
      (to-map xs {}))))

(defcheck solution-15ffb30e
  (fn [sq] (first (reduce (fn [[m k] v]
                            (cond
                              (keyword? v) [(assoc m v []) v]
                              (nil? k) [m k]
                              :else [(assoc m k (conj (m k) v)) k]))
                    [{} nil] sq))))

(defcheck solution-1629408d
  (fn [l] (loop [[x & xs] l, k nil, m {}]
            (cond (nil? x) m
                  (keyword? x) (recur xs x (assoc m x []))
                  :else (recur xs k (assoc m k (conj (m k) x)))))))

(defcheck solution-169fc5da
  (fn [s]
    (loop [res {}
           s   s]
      (if (empty? s)
        res
        (recur (assoc res (first s) (vec (take-while (complement keyword?) (rest s))))
          (drop-while (complement keyword?) (rest s)))))))

(defcheck solution-16c6469a
  (fn __ [x]
    (->> x
      (interpose nil)
      (partition-by keyword?)
      (mapcat (fn [[k :as v]] (if (keyword? k) [k] [(keep identity v)])))
      (apply hash-map)
      )))

(defcheck solution-16da0700
  (fn kvs
    ([xs] (kvs [] xs))
    ([ret xs]
     (if (empty? xs)
       (apply hash-map ret)
       (if (keyword? (first xs))
         (recur (into ret [(first xs) []]) (rest xs))
         (recur (conj (vec (butlast ret)) (conj (last ret) (first xs))) (rest xs)))))))

(defcheck solution-16fcc5df
  (fn prob105 [s]
    (if (empty? s) {}
                   (conj (prob105 (drop-while number? (rest s)))
                     {(first s) (apply vector (take-while number? (rest s)))}))))

(defcheck solution-1738f321
  (fn [S]
    (let [K (filter keyword? S) eK (zipmap K (repeat (count K) []))
          M (apply hash-map (map #(if (keyword? (first %)) (last %) %) (partition-by keyword? S)))]
      (merge eK M)

      )))

(defcheck solution-17675340
  (fn [coll]
    (dissoc
      (reduce
        #(if (keyword? %2) (assoc % %2 [] :*last %2) (merge-with concat % {(% :*last) [%2]}))
        {:*last nil}
        coll)
      :*last)))

(defcheck solution-17bbd484
  (fn vector->map [v]
    (let [empty-map (assoc {} :loc :none, :map {})
          init      (->> (filter keyword v)
                      (map #(vector % []))
                      (reduce conj {})
                      (assoc {} :loc :none, :map))]
      (->>
        (reduce (fn [m e]
                  (if (keyword? e)
                    (assoc m :loc e)
                    (update-in m [:map (:loc m)] conj e)))
          init v)
        :map))))

(defcheck solution-17c3c602
  (fn [s]
    (loop [in s, out {}, curKey nil, curVal []]
      (cond
        (empty? in)
        (if (nil? curKey) out (conj out [curKey curVal]))
        (keyword? (first in))
        (if (nil? curKey)
          (recur (rest in) out (first in) curVal)
          (recur (rest in) (conj out [curKey curVal]) (first in) []))
        :else (recur (rest in) out curKey (conj curVal (first in)))))))

(defcheck solution-17ceb7b6
  #(into {} (reduce (fn [a v]
                      (vec (if (keyword? v)
                             (conj a [v []])
                             (conj (butlast a)
                               [(first (last a))
                                (conj (second (last a)) v)]))))
              {}
              %)))

(defcheck solution-18da4ab1
  (fn [coll]
    (first (reduce (fn [acc val]
                     (if (keyword? val)
                       (vector (assoc (first acc) val []) val)
                       (let [key (second acc)]
                         (vector (update-in (first acc) [key] conj val) key)))) [{}, nil] coll))))

(defcheck solution-1944aae5
  (fn [s]
    (loop [k nil ss s c [] out {}]
      (if (empty? ss)
        (if (nil? k)
          out
          (assoc out k c))
        (let [e (first ss)]
          (if (keyword? e)
            (if (nil? k)
              (recur e (rest ss) [] out)
              (recur e (rest ss) [] (assoc out k c)))
            (recur k (rest ss) (conj c e) out)))))))

(defcheck solution-19b6e0e7
  (fn makee [s1]
    (loop [s s1 accumulated {}]
      (if (empty? s)
        accumulated
        (recur (drop-while number? (rest s))
          (assoc accumulated
            (first s)
            (vec (take-while number? (rest s)))))))))

(defcheck solution-19f26149
  (fn [s]
    (loop [m {} k nil [fs & s] s]
      (cond
        (nil? fs) m
        (keyword? fs) (recur (assoc m fs []) fs s)
        :else (recur (update-in m [k] conj fs) k s)))))

(defcheck solution-1a81589b
  (fn mappify
    ([vals] (mappify vals nil [] {}))
    ([[current & rest :as vals] keyword pending done]
     (if (empty? vals)
       (-> done
         (assoc keyword pending)
         (dissoc nil))
       (if (keyword? current)
         (recur rest current [] (assoc done keyword pending))
         (recur rest keyword (conj pending current) done))))))

(defcheck solution-1adc87df
  (fn [s]
    (apply hash-map (mapcat #(if (keyword? (first %))
                               (interpose () %)
                               (list %))
                      (partition-by keyword? s)))))

(defcheck solution-1afc4d48
  (fn [v]
    (into {} (reduce
               #(if (keyword? %2)
                  (conj % [%2 []])
                  (conj (pop %) [(-> % peek first) (conj (-> % peek peek) %2)])) [] v))))

(defcheck solution-1b4bf48a
  (fn idkeys [kws]
    (if (empty? kws)
      {}
      (loop [k (first kws) r (rest kws) ret {}]
        (let [notkw? (complement keyword?)
              rem    (drop-while notkw? r)]
          (if-not k ret
                    (recur (first rem) (rest rem) (assoc ret k (take-while notkw? r)))))))))

(defcheck solution-1b7a3b24
  (fn [in] (loop [x   (first in)
                  xs  (next in)
                  out {}]
             (if xs
               (if (number? (first xs))
                 (recur x (next xs) (assoc out x (conj (get out x []) (first xs))))
                 (recur (first xs) (next xs) (assoc out (first xs) [])))
               out))))

(defcheck solution-1b98eef0
  (fn [v]
    (apply hash-map (reverse
                      (reduce
                        (fn [ret i]
                          (if (keyword? i)
                            (conj ret i [])
                            (conj (rest ret) (conj (first ret) i))))
                        '()
                        v)))))

(defcheck solution-1bbf4a87
  (fn key-value [coll]
    (loop [ret {}, s coll]
      (if-let [k (first s)]
        (let [vals (take-while #(not (keyword? %)) (rest s))]
          (recur (assoc ret k (vec vals)) (drop (inc (count vals)) s)))
        ret))))

(defcheck solution-1bf8b082
  (fn [ls]
    ((fn f [ls2]
       (if (= 0 (count ls2))
         {}
         (if (= 1 (count ls2))
           (reduce into
             (map #(hash-map % []) (first ls2)))
           (into
             (into
               {(first (reverse (first ls2))) (second ls2)}
               (reduce into
                 (map #(hash-map % [])
                   (rest (reverse (first ls2))))
                 {}))
             (f (drop 2 ls2))))))
     (partition-by keyword? ls))))

(defcheck solution-1c7f7033
  (fn [input-seq]
    (into {} (reduce (fn [result item]
                       (if (keyword? item)
                         (conj result [item []])
                         (let [entry (peek result)]
                           (conj (pop result) (update-in entry [1] conj item)))))
               []
               input-seq))))

(defcheck solution-1c889148
  (fn [coll]
    (first
      (reduce
        (fn [[m k] v]
          (if (keyword? v)
            [(update-in m [v] (fn [_] [])) v]
            [(update-in m [k] #(concat % [v])) k]))
        [{} (first coll)]
        (rest coll)))))

(defcheck solution-1cbc9c51
  (fn f [[h & r]]
    (if h
      (assoc
       (f (drop-while number? r))
        h
        (take-while number? r))
      {})))

(defcheck solution-1cbf31b1
  (fn [l]
    (loop [k   (first l)
           v   (next l)
           c   []
           ret {}]
      (if v
        (if (keyword? (first v))
          (recur (first v) (next v) [] (assoc ret k c))
          (recur k (next v) (conj c (first v)) ret))
        (if k
          (assoc ret k c)
          ret)))))

(defcheck solution-1cfff286
  (fn [l]
    (loop [[l & ls] l km {}]
      (if ls
        (let [[a n] (split-with (complement keyword?) ls)]
          (recur n (assoc km l a)))
        km))))

(defcheck solution-1d5bcadc
  (fn key-val-map
    ([x] (key-val-map x {}))
    ([[k & v] m]
     (let [[int-list r] (split-with number? v)]
       (if k
         (recur
           r
           (assoc m k int-list))
         m)))))

(defcheck solution-1eb6b72c
  (fn keys-and-values [kv]
    (letfn [(create-map [m v]
              (let [currkey (:currkey m) currmap (:currmap m)]
                (if (keyword? v)
                  {:currmap (merge-with #(-> concat vec) currmap {v []}) :currkey v}
                  {:currmap (update-in currmap [currkey] conj v) :currkey currkey})))]
      (:currmap (reduce create-map {:currmap {} :currkey nil} kv)))))

(defcheck solution-1f14f765
  reduce (fn [m v] (if (keyword? v)
                     (with-meta (assoc m v []) {:kw v})
                     (update-in m [(:kw (meta m))] conj v))) {})

(defcheck solution-1f5d691d
  (fn [s]
    (loop [m      {}
           s      s
           curkey nil]
      (if (seq s)
        (let [f (first s)]
          (if (keyword? f)
            (recur (assoc m f [])
              (next s)
              f)
            (recur (assoc m curkey (conj (m curkey) f))
              (next s)
              curkey)))
        m))))

(defcheck solution-1ff131dc
  (fn f [[k & v]]
    (if v
      (let [[a b] (split-with number? v)]
        (assoc (f b) k a))
      {})))

(defcheck solution-20bb6a64
  #(letfn [(d [m [[a] b & r]]
             (if (seq r)
               (if (keyword (first b))
                 (d (assoc m a []) (cons b r))
                 (d (assoc m a b) r))
               (if (nil? a) m (assoc m a b))))] (d {} (partition-by keyword %))))

(defcheck solution-210b793a
  (fn mapify [ks&vs]
    (reduce (fn [acc [ks vs]]
              (reduce #(assoc % %2 '()) (assoc acc (last ks) vs) (butlast ks)))
      {}
      (partition 2 (partition-by keyword? ks&vs)))))

(defcheck solution-213ca6bb
  (fn [xs]
    (loop [[k & xs] xs
           res {}]
      (cond
        (nil? k) res
        (empty? xs) res
        :else (let [[vs xs] (split-with (comp not keyword?) xs)
                    res (assoc res k vs)]
                (recur xs res))))))

(defcheck solution-2177021c
  (fn final-map [v]
    (let [q (into {} (map (partial into [])
                       (partition 2 (mapcat #(if (and (> (count %) 1) (keyword? (first %)))
                                               (concat
                                                 (mapcat (fn [it] (list (list it) '())) (drop-last 1 %))
                                                 (list (list (last %)))
                                                 )
                                               (list %))
                                      (partition-by keyword? v)))))]
      (into {} (map #(vector (ffirst %) (second %)) q)))))

(defcheck solution-21b703d
  (fn keyword-nums
    [s]
    (loop [result    {}
           remaining (drop-while number? s)]
      (if (empty? remaining)
        result
        (recur (assoc result (first remaining)
                             (take-while number? (rest remaining)))
          (drop-while number? (rest remaining)))))))

(defcheck solution-21c9d131
  (fn group-by-keyword [s]
    (let [s
          (mapcat
            #(if (every? keyword? %) [(first %) nil] [(first %)])
            (partition 2 1 (conj s nil)))]

      (->>
        (for [[[k] v] (partition 2 (partition-by keyword? s))] [k (if (= v '(nil)) '() v)])
        (apply concat)
        (apply hash-map)))))

(defcheck solution-220af2a7
  (fn broad-map [xs]
    (->> xs
      (partition-by keyword?)
      (partition 2)
      (mapcat (fn [[ks vs]]
                (concat
                  [[(last ks) vs]]
                  (map (fn [k] [k []]) (butlast ks)))))
      (into {}))))

(defcheck solution-222376dd
  (fn to-map [xs]
    (loop [ys       xs
           keyword_ nil
           values_  []
           result   {}]
      (let [[y & yys] ys
            putResult #(cond (nil? keyword_) result
                             :else (conj result [keyword_ values_]))]
        (cond
          (empty? ys) (putResult)
          (keyword? y) (recur yys y [] (putResult))
          :else (recur yys keyword_ (conj values_ y) result))))))

(defcheck solution-2241e430
  (fn [coll]
    (loop [c coll m {}]
      (if (empty? c) m
                     (let [k (first c)
                           [v r] (split-with number? (rest c))]
                       (recur r (assoc m k v)))))))

(defcheck solution-2256e7c3
  #(loop [coll %1, k nil, result {}]
     (if (empty? coll)
       result
       (let [c0 (first coll), coll' (rest coll), new-key (keyword? c0), k' (if new-key c0 k), v (get result k' [])]
         (recur coll' k' (assoc result k' (if new-key v (conj v c0))))))))

(defcheck solution-2262f86a
  (fn [v]
    (loop [k (first v) rv {} v (rest v)]
      (if (empty? v)
        rv
        (if (keyword? (first v))
          (recur (first v) (assoc rv (first v) []) (rest v))
          (recur k (assoc rv k (conj (get rv k []) (first v))) (rest v)))))))

(defcheck solution-2280695e
  (fn f
    ([xs] (f {} nil xs))
    ([m k [x & xs]]
     (if x
       (if (keyword? x)
         (f (assoc m x []) x xs)
         (f (update-in m [k] conj x) k xs))
       m))))

(defcheck solution-22c931d7
  #(into {}
     (reduce
       (fn [m v]
         (if (keyword? v)
           (conj m [v []])
           (let [[k ys] (last m)]
             (conj (vec (butlast m))
               [k (conj ys v)]))))
       [] %)))

(defcheck solution-22e8ed3e
  (fn identify-key-val [coll]
    (loop [[f & r :as c] coll, k nil, result {}]
      (if (empty? c)
        result
        (if (keyword? f)
          (recur r f (assoc result f []))
          (recur r k (assoc result k (conj (get result k) f))))))))

(defcheck solution-22f522b1
  (fn [s]
    (let [c  (partition-by keyword? (interpose () s))
          c1 (flatten (remove #(= (first %) ()) c))
          c2 (map flatten (remove #(keyword? (first %)) c))]
      (zipmap c1 c2))))

(defcheck solution-232a214
  #(loop [x      %
          result {}
          prev-k nil]
     (if (empty? x)
       result
       (let [f (first x)]
         (recur (rest x)
           (merge-with concat result (if (keyword? f)
                                       {f []}
                                       {prev-k [f]}))
           (if (keyword? f)
             f
             prev-k))))))

(defcheck solution-23af4436
  (fn [coll]
    (loop [[x & xs] coll, result {}, k nil]
      (cond (nil? x) result
            (keyword? x) (recur xs (update-in result [x] vec) x)
            :else (recur xs (update-in result [k] #(conj % x)) k)))))

(defcheck solution-2464f686
  (fn [coll]
    (into {} (reduce
               (fn [res x] (if (number? x)
                             (-> (last res) (update-in [1] conj x) (cons (pop res)) vec)
                             (conj res [x []])))
               []
               coll))))

(defcheck solution-2474760e
  (fn [xs]
    (into {}
      (reduce (fn [acc x]
                (cond
                  (keyword? x) (conj acc [x []])
                  :else (conj (pop acc)
                          (vector (first (peek acc))
                            (conj (second (peek acc)) x)))))
        []
        xs))))

(defcheck solution-247cdf73
  (fn v->m [v]
    (loop [v v, k nil, m {}]
      (let [e (first v)]
        (if (keyword? e)
          (recur (rest v) e (assoc m e []))
          (if (number? e)
            (recur (rest v) k (assoc m k (conj (m k) e)))
            m))))))

(defcheck solution-2593a290
  (fn p105 [kvs]
    (loop [r {} kvs kvs k nil vs []]
      (if (empty? kvs)
        (if (empty? vs) r (assoc r k vs))
        (let [n (first kvs)]
          (if (keyword? n)
            (recur (if (nil? k) r (assoc r k vs))
              (rest kvs)
              n
              [])
            (recur r (rest kvs) k (conj vs n))))))))

(defcheck solution-25c07879
  (fn [coll]
    (if (empty? coll)
      {}
      (loop [res       {}
             ckey      (first coll)
             vcoll     []
             left-coll (rest coll)]
        (if (empty? left-coll)
          (assoc res ckey vcoll)
          (let [next-is-keyword (keyword? (first left-coll))
                next-res        (if next-is-keyword (assoc res ckey vcoll) res)
                next-ckey       (if next-is-keyword (first left-coll) ckey)
                next-vcoll      (if next-is-keyword [] (conj vcoll (first left-coll)))]
            (recur next-res next-ckey next-vcoll (rest left-coll))))))))

(defcheck solution-25db2d09
  #(loop [xs %, m {}]
     (if (empty? xs) m
                     (let [[x & xs'] xs,
                           [t d] (split-with (complement keyword?) xs')]
                       (recur d (assoc m x (into [] t)))))))

(defcheck solution-25f3233f
  (fn [coll]
    (->>
      (partition-by keyword? coll)
      (reduce
        (fn [a b]
          (cond
            (= 1 (count b)) (conj a
                              (let [v (first b)]
                                (if (keyword? v) v [v])))
            (every? keyword? b) (apply (partial conj a) (interpose [] b))
            :else (conj a (into [] b)))) [])
      (partition 2)
      (map #(into [] %))
      (into {})
      )))

(defcheck solution-261cf5e7
  (fn [seq]
    (if (empty? seq)
      {}
      (loop [result {} temp-key [(first seq)] temp-val [] elements (rest seq)]
        (if elements
          (recur
            (if (keyword? (first elements))
              (into result {(first temp-key) temp-val})
              result
              )
            (if (keyword? (first elements))
              [(first elements)]
              temp-key
              )
            (if (keyword? (first elements))
              []
              (conj temp-val (first elements))
              )
            (next elements))
          (into result {(first temp-key) temp-val})
          )
        )
      )
    ))

(defcheck solution-2630de83
  #(loop [[fr & rst] %
          r {}
          k nil]
     (cond
       (nil? fr) r
       (keyword? fr) (recur rst (assoc r fr []) fr)
       :else (recur rst (assoc r k (conj (r k) fr)) k))))

(defcheck solution-26cf3a41
  (fn key-vals [s]
    (reduce (fn [a b]
              (if (keyword? b)
                (assoc a b [])
                (assoc a (first (last a)) (conj (second (last a)) b))
                )) (sorted-map) s)
    ))

(defcheck solution-26f7e67
  #(first
     (reduce (fn [[m k] n]
               (if (keyword n)
                 [(assoc m n []) n]
                 [(update-in m [k] conj n) k]))
       [{} nil]
       %)))

(defcheck solution-271437d3
  (fn [s]
    ((reduce
       (fn [[m k] v]
         (if (keyword? v)
           [(assoc m v []) v]
           [(assoc m k (conj (m k) v)) k]))
       [{} nil]
       s) 0)))

(defcheck solution-274bae5f
  (fn x
    ([coll] (if (empty? coll) {}
                              (let [[vals rest] (split-with (complement keyword?) (rest coll))]
                                (assoc (x rest) (first coll) vals))))))

(defcheck solution-277bb3d2
  (fn [coll]
    (first (reduce (fn [[hmap ky] nxt] (if (keyword? nxt)
                                         [(assoc hmap nxt []) nxt]
                                         [(assoc hmap ky (conj (ky hmap) nxt)) ky]))
             [{} nil] coll))))

(defcheck solution-27801795
  (fn [s] (->> (partition-by keyword? s)
            (mapcat #(if % (interpose [] %2) [%2]) (cycle [true false]))
            (apply hash-map))))

(defcheck solution-2782329
  (fn tomap [s]
    (first
      (reduce
        (fn [[m k] x]
          (if (keyword? x)
            [(assoc m x []) x]
            [(assoc m k (conj (m k) x)) k]))
        [{}]
        s))))

(defcheck solution-27cccad6
  (fn keys-vals
    [coll]
    (loop [result {}
           coll   coll]
      (if (empty? coll)
        result
        (let [head (first coll)]
          (recur (conj result [head (take-while integer? (rest coll))])
            (drop-while integer? (rest coll))))))))

(defcheck solution-2860272a
  (fn [v]
    (loop [v_ v acc {}]
      (cond
        (empty? v_) acc
        :else (recur (drop-while #(not= (type %) (type :a)) (rest v_))
                (conj acc [(first v_) (take-while #(not= (type %) (type :a)) (rest v_))]))))))

(defcheck solution-287bbfe6
  (fn make-map [xs]
    (if (empty? xs)
      {}
      (merge {(first xs) (take-while (complement keyword?) (rest xs))}
        (make-map (drop-while (complement keyword?) (rest xs)))))))

(defcheck solution-288f981f
  (fn [s]
    (loop [[h & t] s l nil r {}]
      (cond
        (nil? h) r
        (keyword? h) (recur t h (assoc r h []))
        :else (recur t l (assoc r l (conj (r l) h)))))))

(defcheck solution-28a94855
  (fn [s]
    (:map
     (reduce
       (fn [{:keys [map sym] :as sofar} el]
         (if (keyword? el)
           (assoc sofar :sym el :map (assoc map el []))
           (assoc sofar :map (merge-with into map {sym [el]}))))
       {:map {}}
       s))))

(defcheck solution-28cd7ccf
  (fn [s]
    (let [the-keys  (filter keyword? s)
          key-index (keep-indexed #(if (keyword? %2) %) s)]
      (zipmap the-keys
        (map
          #(apply vector %)
          (for [x
                (map
                  #(range (inc %) %2)
                  key-index
                  (concat (rest key-index) (vector (count s))))]
            (map #(get s %) x)))))))

(defcheck solution-290638e
  (fn [src]
    (first (reduce #(if (number? (first %2))
                      [(assoc (first %) (last %) %2) (last %)]
                      [(assoc (first %) (first %2) '()) (first %2)]) [{} nil]
             (partition-by #(and (not (number? %)) %) src)
             )
      )
    ))

(defcheck solution-2929a85d
  (fn m [r]
    (if (empty? r) {}
                   (loop [lk (first r), v [], m {}, l (next r)]
                     (if (empty? l)
                       (assoc m lk v)
                       (let [[n & r] l]
                         (if (keyword? n)
                           (recur n [] (assoc m lk v) r)
                           (recur lk (conj v n) m r))))))))

(defcheck solution-29770e25
  (fn [s]
    (loop [k  (first s)                                     ; assuming here that it'll start with a keyword!
           s  (rest s)
           m  {}
           sq []]
      (let [next (first s)]
        (cond
          (empty? s) (if (nil? k) m (conj m [k sq]))
          (keyword? next) (recur next (rest s) (conj m [k sq]) [])
          (number? next) (recur k (rest s) m (conj sq next)))))))

(defcheck solution-29d4694e
  (fn [kvs] (apply hash-map
              (mapcat #(vector (first (first %)) (second %))
                (partition 2 (map #(remove nil? %)
                               (partition-by keyword?
                                 (interpose nil kvs))))))))

(defcheck solution-29ef0b3c
  #(second (reduce (fn [[k m] x]
                     (if (keyword? x)
                       [x (assoc m x [])]
                       [k (assoc m k (conj (m k) x))]))
             [nil {}]
             %)))

(defcheck solution-29fd426e
  #(loop [c (partition-by integer? %) ret {}]
     (if (empty? c)
       ret
       (let [klist (first c)
             r     (map (fn [k v] (assoc {} k v)) (butlast klist) (repeat []))
             vlist (vec (second c))
             ]
         (recur (nnext c) (assoc (into ret r) (last klist) vlist))
         )
       )
     ))

(defcheck solution-2a24e6d0
  (fn [coll]
    (->> coll
      (partition-by keyword?)
      (partition 2)
      (mapcat (fn [[k v]] (conj (map (fn [x] [x []])
                                  (drop-last k))
                            [(last k) v])))
      (into {}))))

(defcheck solution-2a5b8645
  (fn [input]
    (let [f (fn [v e]
              (if (keyword? e)
                (conj v e [])
                (update-in v [(dec (count v))] conj e)))]
      (into (hash-map) (mapv vec (partition 2 (reduce f [] input)))))))

(defcheck solution-2a622097
  (fn [s]
    (loop [m {}
           s s]
      (let [k (first s)
            v (take-while integer? (rest s))]
        (if (nil? k)
          m
          (recur (conj m [k v]) (drop (inc (count v)) s)))))))

(defcheck solution-2b102639
  (fn __ [col]
    ((fn v2m [mp [h & t]]
       (if (nil? h)
         {}
         (let [x  (split-with number? t)
               mp (assoc mp h (first x))]
           (if (empty? (second x))
             mp
             (v2m mp (second x)))))) {} col)))

(defcheck solution-2b1af693
  (fn [xs]
    (second
      (reduce
        (fn [[current-key, m] x]
          (if (keyword? x)
            [x, (assoc m x (get m x []))]
            [current-key, (assoc m current-key
                                   (conj (get m current-key) x))]))
        [nil, {}]
        xs))))

(defcheck solution-2b3084c7
  (fn keyvals
    ([ls] (keyvals ls {}))
    ([ls acc]
     (letfn [(helper [a ls acc]
               (if (or (empty? ls) (keyword? (first ls)))
                 acc
                 (helper a (rest ls) (assoc acc a (conj (get acc a) (first ls))))))]
       (cond (empty? ls) acc
             (keyword? (first ls))
             (keyvals (rest ls) (merge acc (helper (first ls) (rest ls) {(first ls) []})))
             :else
             (keyvals (rest ls) acc))))))

(defcheck solution-2b55eff6
  (fn [seq]
    (let [
          new-result (fn [result key value]
                       (if (nil? key) result (assoc result key value)))]
      (loop [
             [head & tail] seq
             key    nil
             value  []
             result {}]
        (if (nil? head)
          (new-result result key value)
          (if (keyword? head)
            (recur tail head [] (new-result result key value))
            (recur tail key (conj value head) result)))))))

(defcheck solution-2bab63fb
  (fn merge-map
    ([args] (merge-map [] args))
    ([acc args]
     (if (nil? (seq args))
       (into {} acc)
       (let [new-acc (conj acc [(first args) (vec (take-while number? (rest args)))])]
         (recur new-acc (drop-while number? (rest args))))))))

(defcheck solution-2c74fbe6
  (fn mf [s]
    (if (seq s)
      (merge {(first s) (take-while number? (rest s))} (mf (drop-while number? (rest s))))
      {})))

(defcheck solution-2c7c56ba
  (fn [alist]
    (if (empty? alist)
      {}
      (->>
        ((fn looking [n]
           (let [kword       (first n)
                 itsints     (take-while integer? (rest n))
                 lengthtaken (inc (count itsints))]
             (if (empty? n)
               n
               (cons [kword itsints] (looking (drop lengthtaken n))))))
         alist)
        (map #(hash-map (first %) (second %)))
        (reduce merge)))))

(defcheck solution-2c84318
  (fn [xs]
    (loop [items xs, acc {}]
      (if (empty? items) acc
                         (let [k (first items)
                               [v r] (split-with number? (rest items))]
                           (recur r (assoc acc k (vec v))))))))

(defcheck solution-2c84cd31
  (fn [s]
    (second
      (reduce (fn [[lst m] e]
                (if (keyword? e)
                  (if-let [vs (m e)]
                    [e m]
                    [e (assoc m e [])])
                  ;; not a kw
                  [lst (assoc m lst (conj (m lst) e))]))
        [nil {}] s))))

(defcheck solution-2c87506f
  (fn s [[k & v]]
    (if v
      (let [[a b] (split-with number? v)]
        (assoc (s b) k a))
      {})))

(defcheck solution-2d0fe24c
  (fn [x] (second (reduce #(let [[kw h] %1] (if (keyword? %2) [%2 (conj h {%2 []})] [kw (merge-with conj h {kw %2})])) [nil {}] x))))

(defcheck solution-2d9a2681
  (fn kv [s]
    (reduce #(if (keyword? %2)
               (assoc % %2 [])
               (update-in
                 %
                 [(first (last %))]
                 (fn [x] (conj x %2)))) (sorted-map) s)))

(defcheck solution-2d9cb141
  (fn f [x]
    (if (empty? x)
      {}
      (let [
            nk #(not (keyword? %))
            [k & s] x
            v  (take-while nk s)
            r  (drop-while nk s)]
        (assoc (f r) k v)))))

(defcheck solution-2db13957
  #((fn ! [m k [f & r]]
      (if f
        (if (keyword? f)
          (! (into m {f []}) f r)
          (! (into m {k (conj (m k []) f)}) k r))
        m)) {} (first %) (rest %)))

(defcheck solution-2e4c9ec3
  (fn f [l]
    (first
      (reduce (fn [[m s] i]
                (if (keyword? i)
                  [(assoc m i []) i]
                  [(assoc m s (conj (m s) i)) s]))
        [{} nil]
        l))))

(defcheck solution-2e64b0cb
  (fn ikv [s]
    (let [create-map (fn [s]
                       (loop [s2 s, result {}]
                         (if (empty? s2)
                           result
                           (recur (rest s2) (conj result [(first s2) []])))))
          proc-pair  (fn [pair]
                       (let [ks  (first pair)
                             nss (last pair)
                             m   (create-map ks)
                             lk  (last ks)
                             r   (assoc-in m [lk] nss)]
                         (assoc-in m [lk] nss)))
          s2         (partition-by keyword? s)
          s3         (partition-all 2 s2)]
      (loop [s4 s3, result {}]
        (if (empty? s4)
          result
          (recur (rest s4) (merge result (proc-pair (first s4))))
          )))))

(defcheck solution-2e9afb1f
  (fn to-map [s]
    (if-not (seq s)
      {}
      (let [[v t] (split-with (complement keyword?) (rest s))]
        (conj (to-map t) [(first s) (vec v)])))))

(defcheck solution-2ee50520
  (fn [c]
    ((fn ! [mp key coll]
       (if-let [s (seq coll)]
         (let [head (first s)]
           (if (keyword? head)
             (! (assoc mp head []) head (rest s))
             (! (assoc mp key (conj (get mp key) head)) key (rest s))))
         mp)) {} nil c)))

(defcheck solution-2f5b08be
  #(apply hash-map
     ((fn a [[h & t]]
        (when (keyword? h)
          (let [numbers (take-while number? t)]
            (concat
              (cons h [numbers])
              (a (drop (count numbers) t)))))
        ) %)))

(defcheck solution-2fd5adba
  (fn id-keys-and-vals [coll]
    (->> coll
      (partition-by keyword?)
      (mapcat #(if (keyword? (first %)) (interpose [] %) [%]))
      (apply hash-map))))

(defcheck solution-2ff9ce7e
  (fn I [c]
    (if (empty? c) {}
                   (let [P #(not (keyword? %))
                         r (rest c)]
                     (merge {(first c) (take-while P r)}
                       (I (drop-while P r)))))))

(defcheck solution-3004b933
  (fn [[x & more] & m]
    (let [m (or m {})] (if x
                         (if (keyword? (first more))
                           (recur more (into m {x []}))
                           (recur (drop-while number? more) (into m {x (vec (take-while number? more))})))
                         m))))

(defcheck solution-302e45b6
  (fn f [xs]
    (if (empty? xs)
      {}
      (let [[[key] rest] (split-at 1 xs)
            [value rest] (split-with (complement keyword?) rest)
            value (vec value)]
        (into {key value} (f rest))))))

(defcheck solution-304425aa
  (fn list-to-map ([l] (list-to-map l {}))
    ([l m] (let [[head tail] (split-with keyword? l)]
             (if (empty? tail) m
                               (let [k (last head)
                                     e (butlast head)
                                     [v rst] (split-with (complement keyword?) tail)]
                                 (list-to-map rst (assoc (reduce #(assoc %1 %2 '()) m e) k v))))))))

(defcheck solution-306306fe
  (fn [coll]
    (loop [parted (partition-by keyword? coll)
           acc    {}]
      (if-not (seq parted)
        acc
        (let [[[ks vs] rst] (split-at 2 parted)
              empty-maps (zipmap (butlast ks) (repeat []))
              full-map   {(last ks) (vec (or vs []))}]
          (recur rst (merge acc empty-maps full-map)))))))

(defcheck solution-306d48b9
  #(into {} (reduce
              (fn [acc e]
                (let [f    (first acc)
                      k    (if (integer? e) (first f) e)
                      v    (if (integer? e) (conj (second f) e) [])
                      acc2 (if (integer? e) (rest acc) acc)]
                  (cons [k v] acc2))) '() %)))

(defcheck solution-30949aba
  #(loop [d {} xs %]
     (let [k (first xs)
           [v xs] (split-with number? (rest xs))]
       (if (nil? k)
         d
         (recur (assoc d k v) xs)))))

(defcheck solution-309b1c34
  (fn [xs] (first (reduce
                    (fn [[m k] x]
                      (if (keyword? x)
                        [(assoc m x []) x]
                        [(merge-with concat m {k [x]}) k]))
                    [{} :a]
                    xs))))

(defcheck solution-309d7b06
  (fn f [s]
    (if (seq s)
      (let [[k & r] s
            [v r] (split-with number? r)]
        (into {k v} (f r)))
      {})))

(defcheck solution-30a7ff06
  #(->> % (partition-by keyword?)
     (partition 2)
     (mapcat (fn [[ks vs]] (map vector (reverse ks) (cons vs (repeat [])))))
     (into {})))

(defcheck solution-30b5da1f
  (fn v-to-map
    [v]
    (reduce
      #(if (keyword? %2)
         (assoc %1 %2 [])
         (assoc %1 (first (last %1)) (conj (second (last %1)) %2)))
      (cons (sorted-map) v))))

(defcheck solution-31100adb
  #(second
     (reduce
       (fn [[k m] x]
         (if (keyword? x)
           [x (merge m {x []})]
           [k (merge-with conj m {k x})])) [nil {}] %)))

(defcheck solution-31dca4c3
  (fn k-v
    [s]
    (let [p  (partition-by keyword? s)
          ks (map last (take-nth 2 p))
          vs (map vec (take-nth 2 (rest p)))]
      (reduce (fn [r e]
                (if (contains? r e)
                  r
                  (assoc r e [])))
        (zipmap ks vs) (filter #(keyword? %) s)))))

(defcheck solution-31f28e82
  #(zipmap (filter keyword? %)
     (filter (comp not nil?)
       (reduce
         (fn [result [f & r :as all]]
           (if (keyword? f)
             (into result (repeat (count r) []))
             (conj result all)))
         [] (partition-by keyword? %)))))

(defcheck solution-31ff402b
  (fn [s]
    (loop [s s out {} current nil]
      (if-let [n (first s)]
        (if (keyword? n)
          (recur (next s) (assoc out n []) n)
          (recur (next s) (update-in out [current] conj n) current))
        out))))

(defcheck solution-32b5f2b0
  #(loop [kv %, m {}]
     (if-let [[kw & kv] (seq kv)]
       (let [[vs kv] (split-with number? kv)]
         (recur kv (assoc m kw vs)))
       m)))

(defcheck solution-32b95998
  #(apply hash-map (loop [c % a []]
                     (if (not (seq c)) a
                                       (recur (rest c)
                                         (let [n (first c)]
                                           (if (keyword? n) (conj a n [])
                                                            (conj (vec (drop-last a))
                                                              (conj (last a) n)))))))))

(defcheck solution-32f8b052
  (fn [s]
    (into
      {}
      (mapcat
        identity
        (for [[ks v] (partition 2 (partition-by keyword? s))]
          (conj
            (for [k (butlast ks)] [k []])
            [(last ks) v]))))))

(defcheck solution-3324cdba
  (fn [vc]
    (loop [c vc r {}]
      (if (empty? c) r
                     (let [k (first c) v (vec (take-while #(not (keyword? %)) (rest c))) n (inc (count v))]
                       (recur (nthrest c n) (assoc r k v)))))))

(defcheck solution-33280693
  (fn keys-and-values
    ([coll] (keys-and-values nil {} coll))
    ([cur m coll]
     (if (seq coll)
       (let [n (first coll) r (rest coll)]
         (if (keyword? n)
           (recur n (assoc-in m [n] []) r)
           (recur cur (update-in m [cur] #(conj % n)) r)))
       m))))

(defcheck solution-33c63b3a
  (fn ident-keys [s & ans]
    (let [m (if (first ans) (first ans) {})]
      (if (empty? s)
        m
        (let [k    (first s)
              nums (take-while #(not (keyword? %)) (rest s))
              ]
          (ident-keys (drop (inc (count nums)) s) (assoc m k nums))
          )
        )
      )
    ))

(defcheck solution-33cc80ff
  (fn [s]
    (loop [s s r {}]
      (if (empty? s)
        r
        (if (keyword? (first s))
          (recur
            (drop-while #(not (keyword? %)) (rest s))
            (conj r [(first s) (take-while #(not (keyword? %)) (rest s))]))
          (recur
            (drop-while #(not (keyword? %)) (rest s)) r))))))

(defcheck solution-33d465ba
  (fn [aseq]
    (->> (partition-by keyword? aseq)
      (partition-all 2)
      (mapcat (fn [[ks vs]]
                (concat (interpose () ks) [(sequence vs)])))
      (apply hash-map))))

(defcheck solution-33f15ed
  (fn f
    ([x] (f x {}))
    ([[k & r] a]
     (if k
       (let [[v n] (split-with number? r)]
         (f n (conj a [k v])))
       a))))

(defcheck solution-33f1e200
  (fn sep-keywords [xs]
    (loop [res {} [k & s] xs]
      (if (nil? k) res
                   (let [[s1 s2] (split-with (complement keyword?) s)]
                     (recur (assoc res k s1) s2))))))

(defcheck solution-3402321c
  #(loop [seen {} xs % prev-key nil]
     (cond
       (empty? xs) seen
       (keyword? (first xs)) (recur (assoc seen (first xs) []) (rest xs) (first xs))
       :else (recur (update-in seen [prev-key] conj (first xs)) (rest xs) prev-key))))

(defcheck solution-347ebf56
  (fn f
    ([v] (first (reduce f [{} nil] v)))
    ([[m k] x] (if (keyword? x) [(assoc m x []) x] [(assoc m k (conj (m k) x)) k]))))

(defcheck solution-34aa31e
  (fn [coll]
    (->> coll
      (reduce
        (fn fold-seq-into-map [[map last-keyword] next-value]
          (if (keyword? next-value)
            [(assoc map next-value []) next-value]
            [(update-in map [last-keyword] #(conj % next-value)) last-keyword]))
        [{} nil])
      (first))))

(defcheck solution-34ff7ac4
  (fn [coll]
    (->> coll
      (partition-by #(if (keyword? %) % true))
      (partition 2 1)
      (mapcat (fn [[[a] [b :as values]]]
                (when (keyword? a) [[a (if (keyword? b) [] values)]]))
        )
      (into {}))))

(defcheck solution-35714769
  (fn vecmap [coll]
    (let [split-when (fn [f coll]
                       (loop [remaining coll curr [] splits []]
                         (if-let [s (seq remaining)]
                           (let [[x & xs] s]
                             (if (f x)
                               (recur xs [] (conj splits curr))
                               (recur xs (conj curr x) splits)))
                           (conj splits curr))))
          keys       (filter keyword? coll)
          vals       (next (split-when keyword? coll))]
      (zipmap keys vals))))

(defcheck solution-359c3e72
  #(apply hash-map (mapcat (fn [[x :as xs]] (if (keyword? x) (interpose [] xs) [xs])) (partition-by keyword? %))))

(defcheck solution-35cc43d6
  (fn [coll] (:output (reduce (fn [{:keys [vals output]} x]
                                (if (keyword? x)
                                  {:vals   '()
                                   :output (assoc output x vals)}
                                  {:vals   (cons x vals)
                                   :output output}))
                        {:vals   '()
                         :output {}}
                        (reverse coll)))))

(defcheck solution-360f779
  (fn [xs]
    (if (empty? xs)
      {}
      (loop [out {} kw (first xs) vs [] in (rest xs)]
        (if (empty? in)
          (assoc out kw vs)
          (let [nxt (first in)]
            (if (keyword? nxt)
              (recur (assoc out kw vs) nxt [] (rest in))
              (recur out kw (conj vs nxt) (rest in)))))))))

(defcheck solution-3654701d
  (fn f
    ([xs k m]
     (if (seq xs)
       (let [x (first xs) ys (rest xs)]
         (if (keyword? x)
           (f ys x (assoc m x []))
           (f ys k (assoc m k (conj (get m k []) x)))))
       m))
    ([xs] (f (rest xs) (first xs) {}))))

(defcheck solution-3669c6cd
  (fn [[c & coll]]
    (if (nil? c)
      {}
      (loop [res {} key c val [] r coll]
        (if (empty? r)
          (conj res {key val})
          (if (keyword? (first r))
            (recur (conj res {key val}) (first r) [] (rest r))
            (recur res key (conj val (first r)) (rest r))))))))

(defcheck solution-3674b014
  (fn f
    ([c] (f {} c))
    ([m [k & t]]
     (if k
       (let [[a b] (split-with number? t)]
         (f (assoc m k a) b))
       m))))

(defcheck solution-368e6216
  (fn [m]
    (loop [m m r [] last-type nil eles []]
      (cond (and (empty? m) (not (nil? last-type))) (apply hash-map (conj r eles))
            (empty? m) (apply hash-map r)
            (and (= last-type :keyword) (keyword? (first m))) (recur (rest m) (conj r [] (first m)) :keyword [])
            (and (keyword? (first m)) (nil? last-type)) (recur (rest m) (conj r (first m)) :keyword [])
            (keyword? (first m)) (recur (rest m) (conj r eles (first m)) :keyword [])
            :else (recur (rest m) r :ele (conj eles (first m)))))))

(defcheck solution-36e3fd3d
  #(apply hash-map (reduce (fn [acc val]
                             (if (keyword? val)
                               (conj acc val [])
                               (conj (pop acc) (conj (peek acc) val))))
                     []
                     %)))

(defcheck solution-36f34f19
  (fn identify-keys-and-values
    ([coll] (identify-keys-and-values coll {}))
    ([coll m]
     (cond
       (empty? coll)
       m
       (keyword? (first coll))
       (identify-keys-and-values
         (drop-while number? (rest coll))
         (assoc m (first coll) (take-while number? (rest coll))))
       )
     )
    ))

(defcheck solution-371c66dc
  (fn [items]
    (first
      (reduce (fn [[accum key] next]
                (if (keyword? next)
                  [(assoc accum next []) next]
                  [(assoc accum key (conj (get accum key) next)) key]))
        [{} nil] items))))

(defcheck solution-37c63882
  (fn v-to-m [coll]
    (loop [key (first coll) more (rest coll) m {}]
      (if (nil? key)
        m
        (let [nums (take-while (complement keyword?) more)
              next (drop-while (complement keyword?) more)]
          (recur (first next) (rest next) (conj m [key nums])))))))

(defcheck solution-37eb10e
  #(loop [[k & r] % acc {}]
     (if k
       (let [[f s] ((juxt take-while drop-while) number? r)] (recur s (conj acc [k f])))
       acc)))

(defcheck solution-38960fe9
  (fn f [xs]
    (if (empty? xs) {}
                    (let [[cur, rst] (split-with (comp not keyword?) (rest xs))]
                      (into {(first xs) cur} (f rst))))))

(defcheck solution-3906a636
  (fn f [[k & more]]
    (cond
      (nil? k) {}
      (keyword? (first more)) (assoc (f more) k [])
      :else (assoc (f (drop-while (comp not keyword?) more))
              k
              (take-while (comp not keyword?) more)))))

(defcheck solution-392ee042
  (fn [v]
    (apply hash-map
      (reduce #(if (keyword? %2)
                 (conj % %2 [])
                 (conj (vec (drop-last %)) (conj (last %) %2)))
        []
        v))))

(defcheck solution-39316bad
  (fn
    [cs]
    (let [cs (flatten cs)
          ps (partition-by keyword? cs)]
      (loop [ps ps, m {}]
        (cond
          (empty? ps) m
          :d (let [p (first ps)]
               (if (= 1 (count p))
                 (recur (drop 2 ps) (assoc m (first p) (vec (second ps))))
                 (recur (drop 2 ps) (-> m
                                      (assoc (first p) [])
                                      (assoc (second p) (vec (second ps))))))))))))

(defcheck solution-39572fb3
  (fn [coll]
    (first
      (reduce
        (fn [[map keyword] val]
          (if (keyword? val)
            [(assoc map val []) val]
            [(assoc map keyword
                        (conj (get map keyword) val))
             keyword]))
        [{} nil]
        coll))))

(defcheck solution-39915189
  (fn f [a k [h & t]]
    (if h
      (if (keyword? h)
        (f (assoc a h []) h t)
        (f (assoc a k (conj (a k) h)) k t))
      a)) {} 0)

(defcheck solution-39a8a72e
  #(loop [xs (seq %) r {}]
     (if (seq xs)
       (let [k (take 1 xs) v (->> xs (drop 1) (take-while number?))]
         (recur (drop (inc (count v)) xs) (assoc r (first k) (or v []))))
       r)))

(defcheck solution-39dc5c60
  (fn [i]
    (->> i
      (partition-by type)
      (mapcat #(if (keyword (first %))
                 (interpose [] %)
                 (list %)))
      (apply hash-map))))

(defcheck solution-39f2d0e2
  (fn [param]
    (reduce (fn [acc x]
              (if (> (count (key x)) 1)
                (assoc acc (first (key x)) [] (second (key x)) (vec (val x)))
                (assoc acc (first (key x)) (vec (val x))))) {}
      (apply hash-map (partition-by keyword? param)))))

(defcheck solution-3a337f01
  #(->> (partition-by keyword? %)
     (mapcat
       (fn [[k :as x]]
         (if (keyword? k) (interpose [] x) [x])))
     (apply array-map)))

(defcheck solution-3a5a583f
  (fn keyword-map [input-sequence]
    (loop [result {}
           active nil
           queue  input-sequence]
      (cond
        (empty? queue) result
        (keyword? (first queue))
        (recur (assoc result (first queue) []) (first queue) (rest queue))
        :else
        (recur
          (merge-with concat result {active [(first queue)]})
          active
          (rest queue))))))

(defcheck solution-3a63ce8
  (fn [coll]
    (apply zipmap
      (reduce
        (fn [[k v] x]
          (if (keyword? x)
            [(cons x k) (cons [] v)]
            [k (cons (conj (first v) x) (rest v))]))
        '([] []) coll))))

(defcheck solution-3aeaaeb7
  (fn [xs]
    (first
      (reduce
        (fn [[m k] x]
          (if (keyword? x)
            [(assoc m x []) x]
            [(update-in m [k] conj x) k]))
        [{}]
        xs))))

(defcheck solution-3b5686c4
  (fn [coll]
    (loop [s   coll
           map {}]
      (if (seq s)
        (let [key (first s)
              s   (rest s)]
          (recur (drop-while (complement keyword?) s)
            (assoc map key (vec (take-while (complement keyword?) s)))))
        map))))

(defcheck solution-3bd8f68d
  #(second (reduce (fn [[k m :as a] v]
                     (if (keyword? v)
                       [v (assoc m v [])]
                       [k (update-in m [k] conj v)])) [nil {}] %)))

(defcheck solution-3c114c3e
  (fn mapize
    [lista]
    (if (empty? lista)
      {}
      (conj (mapize (drop-while #(not (keyword? %)) (rest lista)))
        [(first lista) (take-while #(not (keyword? %)) (next lista))]))))

(defcheck solution-3c521eff
  #(loop [m {} s %]
     (if
      (empty? s)
       m
       (recur
         (assoc m (first s) (take-while (complement keyword?) (rest s)))
         (drop-while (complement keyword?) (rest s))))))

(defcheck solution-3c781989
  (fn [lst]
    (into {}
      (loop [coll lst, result ()]
        (if (empty? coll) result
                          (let [[key & rst] coll
                                [vals next] (split-with (complement keyword?) rst)]
                            (recur next (cons [key vals] result))))))))

(defcheck solution-3cca05e
  (fn [x] (into {} (map (fn [[k & v]] [k (vec v)]) (let [a (atom nil)] (partition-by #(if (keyword? %) (do (reset! a %) %) @a) x))))))

(defcheck solution-3d3b0055
  (fn [x]
    (letfn [(kv [c res]
              (if (empty? c) res
                             (let [k (first c) v (take-while #(not (keyword? %)) (rest c))]
                               (recur (drop (inc (count v)) c) (assoc res k v)))))]
      (kv x {}))))

(defcheck solution-3dca79e4
  (fn kv [coll]
    (if (empty? coll) {}
                      (assoc (kv (drop-while #(not (keyword? %)) (rest coll)))
                        (first coll) (take-while #(not (keyword? %)) (rest coll))
                        ))))

(defcheck solution-3e498875
  (fn [y] (let [[f s l] [first second last]
                t #(conj (l %) [(f %) (s %)])
                u (fn [a b] (if (keyword? b) [b [] (t a)]
                                             [(f a) (conj (s a) b) (l a)]))]
            (->> y (reduce u []) t butlast (into {})))))

(defcheck solution-3e6eaf6e
  #(->> (reduce
          (fn [m e]
            (if (keyword? e)
              (conj m [e []])
              (let [[k vs] (first m)]
                (conj (rest m) [k (conj vs e)]))))
          '() %)
     (into {})))

(defcheck solution-3ec7a464
  (fn identify-keys-and-values [acoll]
    (-> (reduce
          (fn [acc elem]
            (if (keyword? elem)
              [(into (first acc) (apply hash-map (second acc))) [elem []]]
              [(first acc) [(first (second acc)) (conj (second (second acc)) elem)]]))
          [{} []]
          acoll)
      (#(into (first %) (apply hash-map (second %)))))))

(defcheck solution-3eca84b0
  (fn [coll]
    (letfn [(into-current-key [acc cur-key xs]
              (if (empty? xs)
                acc
                (let [fst (first xs)
                      rst (rest xs)]
                  (if (keyword? fst)
                    (into-current-key (assoc acc fst []) fst rst)
                    (let [lst     (get acc cur-key)
                          new-lst (conj lst fst)
                          new-acc (assoc acc cur-key new-lst)]
                      (into-current-key new-acc cur-key rst))))))]

      (into-current-key {} (first coll) coll))))

(defcheck solution-3f02dad7
  (fn [sq]
    (loop [ret {} sq sq]
      (if (empty? sq) ret
                      (let [k  (first sq)
                            vs (vec (take-while integer? (rest sq)))]
                        (recur (conj ret (hash-map k vs)) (drop (inc (count vs)) sq)))))))

(defcheck solution-3f3dbf63
  (fn idkv [s]
    (loop [[v & xs] s
           k nil
           r {}]
      (cond (nil? v) r
            (keyword? v) (recur xs v (assoc r v []))
            :else (recur xs k (merge r {k (conj (r k) v)}))))))

(defcheck solution-3f772fc3
  (fn kv [xs]
    (if (empty? xs) {}
                    (let [k    (first xs)
                          r    (rest xs)
                          n    (drop-while (complement keyword?) r)
                          vals (take-while (complement keyword?) r)]
                      (conj {k vals} (kv n))))))

(defcheck solution-3ff740e3
  #(apply hash-map
     (apply concat
       (map
         (fn [[a :as c]]
           (if (keyword? a) (interpose [] c) [c]))
         (partition-by keyword? %)))))

(defcheck solution-40259c4
  (fn [s]
    (loop [[x & r] s, k nil, m {}]
      (cond
        (nil? x) m
        (keyword? x) (recur r x (assoc m x []))
        :else (recur r k (update-in m [k] conj x))))))

(defcheck solution-40576183
  #(loop [m {} k nil s %]
     (if (empty? s) m (let [e (first s)]
                        (if (keyword? e) (recur (merge m {e []}) e (rest s))
                                         (recur (merge-with concat m {k [e]}) k (rest s)))))))

(defcheck solution-4082ed3d
  #(into {} (loop [[v & S] % k nil V [] R '()] (cond (not v) (if k (cons [k V] R) R) (keyword? v) (recur S v [] (if k (cons [k V] R) R)) :else (recur S k (conj V v) R)))))

(defcheck solution-40f94a4
  (fn [in-seq]
    (loop [acc {} s in-seq]
      (if (or (nil? s) (empty? s))
        acc
        (let [kw   (first s)
              nums (vec (take-while number? (rest s)))
              rem  (drop (count nums) (rest s))]
          (recur (assoc acc kw nums)
            rem))))))

(defcheck solution-41920415
  (fn [s] (apply hash-map
            ((fn split-on-keywords [s]
               (if (empty? s) '()
                              (let [[f & r] (seq s)
                                    [vs rr] (split-with (complement keyword?) r)]
                                (concat (list f vs) (split-on-keywords rr))))) s))))

(defcheck solution-41df0fe9
  (fn [xs] (apply
             hash-map
             (reduce
               (fn [ys y] (let
                           [z  (last ys)
                            zs (-> ys butlast vec)]
                            (apply conj
                              (if (keyword? y)
                                [ys y []]
                                [zs (conj z y)]))))
               []
               xs))))

(defcheck solution-42054921
  (fn [coll]
    (loop [res {} coll coll]
      (if (empty? coll)
        res
        (let [key        (first coll)
              maybe-vals (rest coll)
              [vals other] (split-with #(not (keyword? %)) maybe-vals)]
          (recur (assoc res key vals) other))))))

(defcheck solution-42059010
  (fn [coll]
    (into {} (map (fn [[a b]] {a b})
               (partition 2 2
                 (reduce (fn [sofar [a b]]
                           (if (keyword? a)
                             (if (keyword? b)
                               (conj sofar a [])
                               (conj sofar a))
                             (if (keyword? (last sofar))
                               (conj sofar [a])
                               (assoc sofar (dec (count sofar)) (conj (last sofar) a))))) [] (partition 2 1 [1] coll)))))))

(defcheck solution-420f3543
  (fn smunge [s]
    (if (empty? s)
      (hash-map)
      (let [[keywd tail] (split-at 1 s)
            [value recurs] (split-with (complement keyword?) tail)]
        (assoc (smunge recurs) (first keywd) (vec value))))))

(defcheck solution-424df2fa
  (fn
    [s]
    ((reduce (fn [acc elm]
               (let [[curr m] acc]
                 (cond
                   (keyword? elm)
                   (if (m elm) acc [elm (assoc m elm [])])
                   (number? elm)
                   (if (and (not (nil? curr)) (m curr))
                     [curr
                      (assoc m curr (conj (m curr) elm))]
                     acc)
                   true acc
                   )))
       [nil {}]
       s)
     1)))

(defcheck solution-4259875
  (fn [s]
    (->> [nil s]
      (iterate (fn [[_ [k & vs]]]
                 (let [[v1 v2] (split-with (complement keyword?) vs)]
                   [[k v1] v2])))

      (drop 1)
      (take-while ffirst)
      (map first)
      (map (partial conj {}))
      (reduce (partial merge-with concat) {}))))

(defcheck solution-426bed96
  (fn [p] (into {}
            (loop [l p r []]
              (if (empty? l)
                r
                (let [[a b] (split-with number? (rest l))]
                  (recur b (conj r [(first l) a]))))))))

(defcheck solution-428b4b5a
  (fn [v]
    (apply hash-map
      (map (fn [c] (if (keyword? (first c)) (first c) (vec (apply concat c))))
        (partition-by vector?
          (interpose []
            (map (fn [c] (if (number? (first c)) (vec c) (first c)))
              (partition-by #((if (number? %) number? identity) %) v))))))))

(defcheck solution-42a4a386
  (fn ks [s]
    (if (empty? s)
      {}
      (let [[f & rs] s, [takes drops] (split-with (complement keyword?) rs)]
        (assoc (ks drops) f takes)))))

(defcheck solution-43181d9d
  (fn to-map [lst]
    (if (not (keyword? (first lst)))
      {}
      (loop [last-key (first lst) res {} rem lst]
        (let [fst (first rem)]
          (cond (empty? rem) res
                (keyword? fst) (recur fst (assoc res fst []) (rest rem))
                :else (recur last-key (assoc res last-key (conj (get res last-key) fst)) (rest rem))))))))

(defcheck solution-437ce697
  (fn [c]
    (->> (reduce #(if (keyword? %2)
                    (conj % [%2 []])
                    (update-in % [(dec (count %)) 1] (fn [p] (conj p %2)))) [] c)
      (into {}))))

(defcheck solution-43d45e22
  (fn f [s]
    (if
     (empty? s) {}
                (let
                 [[k & r] s
                  [v n] (split-with #(not (keyword? %)) r)]
                  (assoc (f n) k v)))))

(defcheck solution-43d831a4
  #(apply hash-map
     (mapcat
       (fn [[x :as c]] (if (keyword? x) (interpose [] c) [c]))
       (partition-by keyword? %))))

(defcheck solution-447f25de
  (fn [xs]
    (loop [acc {} xs xs]
      (if (seq xs)
        (recur (assoc acc (first xs) (take-while (complement keyword?) (next xs)))
          (drop-while (complement keyword?) (next xs)))
        acc))))

(defcheck solution-44cf7ebc
  (fn [l]
    (second
      (reduce
        (fn [[k m] i]
          (if (keyword? i)
            [i (assoc m i [])]
            [k (update-in m [k] #(conj % i))]))
        [nil {}]
        l))))

(defcheck solution-4505bac2
  (fn f
    ([x] (f x {}))
    ([[k & r] a]
     (if k
       (let [[v n] (split-with number? r)]
         (f n (conj a [k v])))
       a))))

(defcheck solution-459815fb
  (fn identify [coll]
    (into {}
      (map (fn [[[k] v]] [k v])
        (partition 2 (reduce (fn [all s]
                               (if (and (keyword? (first s))
                                        (not= 1 (count s)))
                                 (vec (concat all
                                        (apply concat
                                          (partition-all 2
                                            (interpose []
                                              (map vector s))))))
                                 (conj all s)))
                       []
                       (partition-by type coll)))))))

(defcheck solution-45d7503e
  (fn key-vals-map [v]
    (loop [[a & b] v m {}]
      (if (nil? a)
        m
        (let [[p q] (split-with (complement keyword?) b)]
          (recur q (conj m [a p])))))))

(defcheck solution-45fe0344
  #(loop [the-list    %
          the-keyword nil
          the-result  {}
          the-cursor  0]
     (if (= the-cursor (count the-list))
       the-result
       (if (keyword? (nth the-list the-cursor))
         (recur the-list
           (nth the-list the-cursor)
           (assoc the-result (nth the-list the-cursor) [])
           (inc the-cursor))
         (recur the-list
           the-keyword
           (assoc the-result the-keyword (conj (the-result the-keyword) (nth the-list the-cursor)))
           (inc the-cursor))))))

(defcheck solution-463edebd
  (fn [v]
    (let [g  (partition-by keyword? v)
          ks (map first (partition 1 2 g))
          ns (flatten (map #(drop-last 1 %) ks))
          vs (map first (partition 1 2 (drop 1 g)))
          x  (into {} (map #(vector (last %) (into [] %2)) ks vs))]
      (reduce #(assoc % %2 []) x ns))))

(defcheck solution-469cfee6
  (fn [s]
    (loop [s   s
           acc {}]
      (if (empty? s)
        acc
        (let [k (first s)
              v (take-while (complement keyword?) (rest s))
              r (drop (inc (count v)) s)]
          (recur r (conj acc [k v])))))))

(defcheck solution-46f17a30
  #(second (reduce (fn [[last m] v]
                     (if (keyword? v)
                       [v (assoc m v [])]
                       [last (update-in m [last] conj v)])) [nil {}] %)))

(defcheck solution-4759b6cf
  (comp
    (partial reduce (fn [m [k v]]
                      (reduce #(assoc %1 %2 [])
                        (assoc m (last k) (vec v))
                        (butlast k)))
      {})
    (partial partition 2)
    (partial partition-by keyword?)))

(defcheck solution-4776214f
  (fn id-key-val [coll]
    (letfn [(take-next-num-seq [seq]
              (take-while (comp not keyword?) seq))
            (drop-next-num-seq [seq]
              (drop-while (comp not keyword?) seq))]
      (loop [res {}, rem coll]
        (if (empty? rem)
          res
          (recur (assoc res (first rem) (take-next-num-seq (rest rem)))
            (drop-next-num-seq (rest rem))))))))

(defcheck solution-47817ab6
  (fn [s]
    (:nums (reduce
             (fn [{:keys [kw nums]} item]
               (if (keyword? item)
                 {:kw item, :nums (assoc nums item [])}
                 {:kw kw, :nums (update-in nums [kw] conj item)})
               )
             {:nums {}}
             s))
    ))

(defcheck solution-478b4b9e
  (fn [coll] (let [
                   c (drop-while #(not (keyword? %)) coll)
                   f (fn [k v coll m] (if (empty? coll) (conj m {k v})
                                                        (if (keyword? (first coll))
                                                          (recur (first coll) [] (rest coll) (conj m {k v}))
                                                          (recur k (conj v (first coll)) (rest coll) m))))

                   ] (if (empty? c) {} (f (first c) [] (rest c) {})))))

(defcheck solution-47af2e0e
  (fn mapify [v]
    (if (empty? v)
      {}
      (let [k (first v) r (rest v)]
        (conj {k (take-while number? r)} (mapify (drop-while number? r)))
        )
      )
    ))

(defcheck solution-485c40de
  (fn [sq]
    (loop [[h & t] sq mp {}]
      (if (nil? h) mp
                   (if (keyword? h)
                     (let [[a r] (split-with #(not (keyword? %)) t)]
                       (recur r (assoc mp h a))))))))

(defcheck solution-485d92f7
  (fn [coll]
    (apply hash-map
      (reduce
        #(if (keyword? %2)
           (conj %1 %2 [])
           (conj (vec (butlast %1)) (conj (last %1) %2)))
        []
        coll))))

(defcheck solution-4869aaf6
  #(loop [[h & t] %
          acc {}
          k   nil]
     (cond
       (nil? h) acc
       (keyword? h) (recur t (merge acc {h []}) h)
       :else (recur t (merge-with conj acc {k h}) k))
     ))

(defcheck solution-489dd6e6
  (fn idkv [s]
    (apply hash-map (map #(if (every? keyword? %) (first %) (rest %))
                      (partition-by keyword?
                        (mapcat (fn [x]
                                  (if (every? keyword? x)
                                    (interleave x (repeat nil))
                                    x))
                          (partition-by keyword? s)))))))

(defcheck solution-48f0a672
  (fn v->m
    [l]
    (loop [m {}
           l l
           k (first l)]
      (let [fl (first l)
            rl (rest l)]
        (if (empty? l)
          m
          (if (keyword? fl)
            (recur (merge m {fl []}) rl fl)
            (recur (merge-with conj m {k fl}) rl k)))))))

(defcheck solution-49b00dd7
  (fn [a]
    (loop [a a r {} l (first a)]
      (cond (empty? a) r
            (keyword? (first a)) (recur (rest a) (assoc r (first a) []) (first a))
            :else (recur (rest a) (assoc r l (conj (get r l []) (first a))) l)
            )
      )
    ))

(defcheck solution-49ea6a94
  (fn [c]
    (first
      (reduce (fn [[m k] v]
                (if (keyword? v)
                  [(assoc m v []) v]
                  [(update-in m [k] #(conj % v))
                   k]))
        [{} nil] c))))

(defcheck solution-4a585148
  (fn [s]
    (let [[r-acc r-k r-v] (reduce (fn [[acc k v] i]
                                    (cond
                                      (nil? k) [acc i []]
                                      (every? keyword? [k i]) [(conj acc k v) i []]
                                      :else [acc k (conj v i)]))
                            [[] nil []] s)]
      (if (nil? r-k)
        (apply array-map r-acc)
        (apply array-map (conj r-acc r-k r-v))))))

(defcheck solution-4a64eb62
  #(letfn
    [(f [[k & r]]
       (when k
         (let [[vls t] (split-with (complement keyword?) r)]
           (concat [k vls] (f t)))))]
     (apply array-map (f %))))

(defcheck solution-4b4cbcfe
  (fn ! [l]
    (let [ks    (filter keyword? l)
          kloc  (map key (filter #(keyword? (second %)) (into {} (map-indexed vector l))))
          vlen  (map dec (map - (conj (vec (drop 1 kloc)) (count l)) kloc))
          mvals (map #(take %2 (drop (inc %1) l)) kloc vlen)]
      (into {} (map vector ks mvals)))))

(defcheck solution-4b5699f6
  (fn f [[k & xs]]
    (if k
      (let [[a b] (split-with integer? xs)]
        (into (hash-map k (vec a)) (f b)))
      {})))

(defcheck solution-4b97c449
  (fn [xs] (first
             (reduce
               (fn [[m k] x]
                 (if (keyword? x)
                   [(assoc m x []) x]
                   [(merge-with conj m {k x}) k]))
               [{} nil] xs))))

(defcheck solution-4bce2e3d
  (fn [coll]
    (->>
      coll
      (reduce #(if (keyword? %2)
                 (into % [%2 []])
                 (conj (pop %) (conj (peek %) %2)))
        [])
      (apply hash-map))))

(defcheck solution-4be5ceef
  (fn joins [vers]
    (loop [cols vers, currKey (first vers), result {}]
      (if (empty? cols)
        result
        (let [now (first cols)]
          (if (keyword? now)
            (recur (rest cols) now (assoc result now []))
            (recur (rest cols) currKey (assoc result currKey
                                                     (conj (get result currKey) now)
                                                     )
              )
            )
          )

        )
      )
    ))

(defcheck solution-4c5dbc22
  (fn [[& args]] (loop [args args coll {}]
                   (if-let [k (first args)]
                     (let [vs       (take-while integer? (rest args))
                           new-args (drop-while (comp not keyword?) (rest args))]
                       (if k
                         (recur new-args (assoc coll k vs))
                         coll))
                     coll))))

(defcheck solution-4cafcc81
  (fn [x] (apply hash-map (reduce #(if (keyword? %2) (concat %1 (list %2) (list [])) (if (keyword? (last %1)) (concat %1 (list (vector %2))) (concat (butlast %1) (list (concat (last %1) (vector %2)))))) '() x))))

(defcheck solution-4ce965de
  (fn me [args]

    (let [myfn (fn [args]

                 (let [my-keys  (first args)
                       my-value (into [] (second args))
                       ]
                   (if (= 1 (count my-keys))
                     (hash-map (first my-keys) my-value)
                     (apply merge
                       (apply merge (map #(hash-map %1 []) (drop-last my-keys)))
                       (hash-map (last my-keys) my-value)
                       )

                     )
                   )

                 )
          ]

      (if (empty? args)
        {}

        (apply merge

          (map myfn (partition 2 (partition-by number? args)))
          ))

      )

    ))

(defcheck solution-4d179ef5
  (fn [v]
    (loop [v v m {}]
      (if (empty? v)
        m
        (if (keyword? (first v))
          (if (keyword? (second v))
            (recur (rest v) (assoc m (first v) []))
            (let [num-vals (count (take-while (comp not keyword?) (rest v)))]
              (recur (drop num-vals (rest v)) (assoc m (first v) (take num-vals (rest v)))))))))))

(defcheck solution-4d760713
  (fn to-map [[x & xs]]
    (if x
      (merge (hash-map x (take-while number? xs)) (to-map (drop-while number? xs)))
      {})))

(defcheck solution-4d84591e
  (fn [s]
    (
     (fn my-assoc [r [k kv & xs]]
       (if (nil? k)
         r
         (my-assoc (reduce #(assoc %1 %2 []) (assoc r (last k) (into [] kv)) (butlast k)) xs)))
     {} (partition-by keyword? s))))

(defcheck solution-4defd20e
  (fn [s]
    (apply hash-map
      (reduce #(if (keyword? %2)
                 (conj % %2 [])
                 (conj (pop %) (conj (peek %) %2)))
        [] s))))

(defcheck solution-4e745ee7
  (fn f
    ([l] (f l {} :a))
    ([[p & qs] m k] (if p
                      (if (keyword? p)
                        (f qs (assoc m p '[]) p)
                        (f qs (merge-with
                                (fn [x y] (vec (concat x y)))
                                m {k [p]}) k))
                      m))))

(defcheck solution-4e825e8a
  (fn [l]
    (loop [l   l
           acc {}]
      (if (zero? (count l))
        acc
        (recur
          (drop (+ 1 (count (take-while #(not (keyword? %)) (rest l)))) l)
          (assoc acc (first l) (take-while #(not (keyword? %)) (rest l))))))))

(defcheck solution-4e9e617a
  (fn f [s]
    (if (empty? s)
      {}
      (assoc (f (drop-while number? (rest s)))
        (first s)
        (take-while number? (rest s))))))

(defcheck solution-4f652ed5
  (fn q4q105 [s]
    ((fn inner-q4q105 [s-i k m]
       (if (empty? s-i)
         m
         (recur
           (rest s-i)
           (if (keyword? (first s-i))
             (first s-i)
             k)
           (if (keyword? (first s-i))
             (assoc m (first s-i) [])
             (assoc m k (conj (get m k) (first s-i)))))))
     s nil {})))

(defcheck solution-4f7e9571
  (fn [s]
    (let [kvs (apply hash-map (partition-by keyword? s))
          cmb (fn [[klist vlist]]
                (let [empty-keys (butlast klist)
                      valid-key  (last klist)]
                  (conj (map #(vector % []) empty-keys)
                    [valid-key vlist])))]
      (into {} (mapcat cmb kvs)))))

(defcheck solution-4f89e979
  (fn map-seqs [[k & r]]
    (if k
      (assoc (map-seqs (drop-while integer? r))
        k
        (take-while integer? r))
      {})))

(defcheck solution-4fc593fc
  (let [notkeyword (complement keyword?)]
    (fn [[head & tail]]
      (into {}
        (loop [rv [] k head v tail]
          (if (nil? k)
            rv
            (let [values (take-while notkeyword v)
                  next'  (drop-while notkeyword v)
                  nextk  (first next')
                  nextv  (rest next')]
              (recur (conj rv [k values]) nextk nextv))))))))

(defcheck solution-4fc9a314
  (fn [xs]
    (apply hash-map (reduce #(if (keyword? %2)
                               (conj (conj %1 %2)
                                 [])
                               (conj (into [] (butlast %1))
                                 (conj (last %1) %2)))
                      [] xs))))

(defcheck solution-5021e179
  #(first
     (reduce (fn [[a l] x]
               (if (keyword? x) [(assoc a x []) x]
                                [(merge-with conj a {l x}) l]))
       [{}] %)))

(defcheck solution-50352b0a
  (fn [coll]
    (loop [k   (first coll)
           [v r] (split-with integer? (rest coll))
           acc {}]
      (if k
        (recur (first r)
          (split-with integer? (rest r))
          (assoc acc k v))
        acc))))

(defcheck solution-50a4fc1e
  (fn __ [s]
    (let [f (fn f [acc cur s]
              (if (empty? s) (merge acc cur)
                             (let [[h & r] s]
                               (if (keyword? h)
                                 (f (merge acc cur) {h []} r)
                                 (f acc (assoc cur
                                          (first (keys cur))
                                          (conj (first (vals cur)) h)) r)))))]
      (f {} {} s))))

(defcheck solution-51655c4
  (fn f [m [h & t]]
    (if h
      (f (assoc m h (take-while number? t)) (drop-while number? t))
      m)) {})

(defcheck solution-51709e98
  (fn [a [x & xs]]
    (let [k x [ys zs] (split-with (comp not keyword?) xs)]
      (if k (recur (assoc a k ys) zs) a))) {})

(defcheck solution-5197bc55
  (fn [coll] (reduce (fn [m v]
                       (if (keyword? v)
                         (assoc-in m [v] [])
                         (update-in m
                           [(first (last m))]
                           #(conj % v))))
               (sorted-map) coll)))

(defcheck solution-519ded32
  (fn [c] (into {} (map #(let [[[x] y] %] [x y]) (partition 2 (map #(filter identity %) (partition-by keyword? (interpose nil c))))))))

(defcheck solution-52159b1c
  (fn [kv]
    (letfn [(reducer [[m k] v]
              (if (keyword? v) [(assoc m v []) v]
                               [(update-in m [k] #(conj % v)) k]))]
      (first (reduce reducer [{} :dummy] kv))
      )
    ))

(defcheck solution-524c66be
  (fn [kvlis]
    (letfn
     [(make-map [s m]
        (cond (empty? s) m
              (keyword? (first s))
              (make-map (drop-while #(not (keyword? %)) (rest s))
                (conj m [(first s)
                         (take-while #(not (keyword? %))
                           (rest s))]))
              :else
              (make-map (drop-while #(not (keyword? %)) (rest s))
                m)))]
      (make-map kvlis {}))))

(defcheck solution-528f62d9
  #(loop [s %, r {}]
     (if (seq s)
       (recur (drop-while number? (rest s)), (assoc r (first s) (take-while number? (rest s))))
       r)))

(defcheck solution-52e313e2
  (fn i [c]
    (if (empty? c) {}
                   (let [k (first c)
                         [v r] (split-with #(not (keyword? %)) (rest c))]
                     (assoc (i r) k (vec v))))))

(defcheck solution-52f5f2e9
  (fn [xs]
    (->> xs
      (partition-by keyword?)
      (map #(if (keyword? (first %)) (interpose [] %) [%]))
      (apply concat [])
      (apply hash-map)
      )))

(defcheck solution-530fe930
  (fn createMap
    [s]
    (let [reduceList (fn reduceList [l]
                       (loop [currL  (rest l)
                              currK  (first l)
                              result []]
                         (if (empty? currL)
                           result
                           (if (keyword? (first currL))
                             (recur (rest currL) (first currL) (conj result [(first currL) nil]))
                             (recur (rest currL) currK (conj result [currK (first currL)]))))))]
      (reduce (fn [m s] (let [key (first s)
                              val (second s)]
                          (if (contains? m key)
                            (assoc m key (conj (get m key) val))
                            (if (nil? val)
                              (assoc m key (vector))
                              (assoc m key (vector val))))))
        {}
        (reduceList s)))))

(defcheck solution-534052a3
  (fn [x] (apply hash-map (map #(if (keyword? (first %)) (first %) (vec (rest %))) (partition-by keyword? (flatten (map #(if (keyword? %) [% nil] %) x)))))))

(defcheck solution-53455690
  (fn [s]
    (loop [[f & c] s, res {}, ckey nil]
      (cond
        (nil? f) res
        (keyword? f) (recur c (assoc res f []) f)
        :else (recur c (assoc res ckey (conj (res ckey) f)) ckey)))))

(defcheck solution-53cc6488
  (fn weird-map [coll]
    (let [mapmaker (fn [bag item]
                     (let [active (:active bag)]
                       (if (keyword? item)
                         (merge bag {item [] :active item})
                         (merge bag {active (conj (active bag) item)}))))]
      (dissoc (reduce mapmaker {:active nil} coll) :active))))

(defcheck solution-542101f2
  (fn f [[k & r]]
    (let [c (complement keyword?)]
      (if k (into {k (take-while c r)} (f (drop-while c r))) {}))))

(defcheck solution-54650a07
  (fn [s]
    (apply hash-map
      (loop [left [] [head & tail] s]
        (if (empty? tail) left
                          (let [[body tail] (split-with number? tail)]
                            (recur (conj left head (vec body))
                              tail)))))))

(defcheck solution-5469856c
  (fn hasher
    ([coll] (hasher coll {}))
    ([coll acc]
     (if (empty? coll)
       acc
       (let [k            (first coll)
             working-coll (rest coll)
             vs           (take-while (comp not keyword?) working-coll)
             rst          (drop (count vs) working-coll)]
         (hasher rst (assoc acc k vs)))))))

(defcheck solution-54a3ff79
  (fn [vals] (apply hash-map (reduce
                               (fn [acc e] (if (keyword? e)
                                             (conj (conj acc e) [])
                                             (conj (vec (drop-last acc)) (conj (last acc) e))))
                               [] vals))))

(defcheck solution-54a64248
  (fn _ [[x & xs]]
    (if-not (keyword? x)
      {}
      (assoc (_ (drop-while #(not (keyword? %)) xs))
        x
        (take-while #(not (keyword? %)) xs)))))

(defcheck solution-54ab286e
  (fn c105
    [coll]
    (loop [c coll
           r {}
           l nil]
      (if (empty? c)
        r
        (let [f (first c)]
          (if (= (type f) (type :a))
            (recur (next c) (assoc r f []) f)
            (recur (next c) (assoc r l (conj (get r l) f)) l)))))))

(defcheck solution-54c3b655
  (fn create-map
    ([xs]
     (if (empty? xs)
       {}
       (create-map {} (first xs) (rest xs))))
    ([acc x xs]
     (cond
       (nil? x) acc
       (empty? xs) acc
       :else
       (let [v (first xs)]
         (if (keyword? v)
           (create-map (assoc acc v []) v (rest xs))
           (create-map (assoc acc x (conj (get acc x []) v)) x (rest xs))))))))

(defcheck solution-54eaca80
  (fn k [sx] (reduce conj {} (reduce #(
                                        if (keyword? %2)
                                        (conj %1 [%2 []])
                                        (conj (rest %1) [(first (first %1)) (conj (last (first %1)) %2)])
                                        ) () sx))))

(defcheck solution-54ff3b4c
  (fn [coll]
    (first
      (reduce
        (fn [[s last-k] x]
          (cond
            (keyword? x) [(assoc s x []), x]
            (nil? last-k) [s nil]
            :else [(assoc s last-k (conj (get s last-k) x)) last-k]))
        [{}, nil] coll))))

(defcheck solution-551a8f56
  (fn [args]
    (->> args
      (reduce (fn [res el]
                (if (keyword? el)
                  (conj res el [])
                  (conj (rest res) (conj (first res) el)))) '())
      (reverse)
      (partition 2)
      (map vec)
      (into {}))))

(defcheck solution-55600f
  (fn [input] (let [padded (mapcat (fn [x] (if (-> x first keyword?)
                                             (if (-> x count (> 1))
                                               (drop-last (interleave (flatten (partition-by identity x)) (take (count x) (repeat '()))))
                                               x)
                                             (list x)))
                             (partition-by #(not (keyword? %)) input))
                    keys   (filter keyword? padded)
                    vals   (filter #(-> % keyword? not) padded)]
                (zipmap keys vals))))

(defcheck solution-559aaf1a
  #(first (reduce (fn [[m k] x]
                    (if (keyword? x)
                      [(assoc m x []) x]
                      [(update-in m [k] conj x) k]))
            [{} nil] %)))

(defcheck solution-55b845c8
  (fn [xs]
    (loop [s xs
           r {}
           k (let [a (first s)]
               (if (keyword? a) a nil))]
      (if (empty? s)
        r
        (recur (rest s)
          (let [f (first s)]
            (if (keyword? f)
              (assoc r f [])
              (assoc r k (conj (k r) f))))
          (let [f (first s)]
            (if (keyword? f)
              f
              k)))))))

(defcheck solution-560577c
  (fn [sq]
    (let [ks (filter keyword? sq)]
      (reduce (fn [a b]
                (merge a {b (take-while #(not (keyword? %))
                              (rest (drop-while #(not (= % b))
                                      sq)))})) {} ks))))

(defcheck solution-56462e58
  (fn [l]
    (loop [s l
           a {}
           l nil]
      (if (seq s)
        (if (keyword? (first s))
          (recur (rest s) (assoc a (first s) []) (first s))
          (recur (rest s) (assoc a l (conj (a l) (first s))) l))
        a))))

(defcheck solution-564d8bc
  (fn identify-keys-vals [[k & more]]
    (if (nil? k)
      {}
      (let [[v others] (split-with (complement keyword?) more)]
        (assoc (identify-keys-vals others) k v)))))

(defcheck solution-56534d81
  #(apply hash-map
     (map (fn [a] (if (not= nil (keyword (first a)))
                    (first a) (flatten a)))
       (partition-by keyword
         (interleave % (repeat []))))))

(defcheck solution-5680e54a
  (fn ikv [s]
    (let [[fst & rst] s
          [head tail] (split-with (comp not keyword?) rst)
          result (hash-map fst head)
          ]
      (if (not fst) {} (if (empty? tail) result (merge result (ikv tail)))))))

(defcheck solution-56bdb74f
  (fn kv [m]
    (into {}
      (if (empty? m) []
                     (lazy-seq
                       (if (keyword? (first m))
                         (let [n (take-while integer? (rest m))]
                           (cons [(first m) n] (kv (drop (+ 1 (count n)) m))))))))))

(defcheck solution-5745a60a
  (fn key_values [coll]
    (let [keys_init (zipmap
                      (filter keyword? coll)
                      (repeat []))]
      (loop [c coll current_key nil result keys_init]
        (if (empty? c)
          result
          (let [f (first c)]
            (if (keyword? f)
              (recur (rest c) f result)
              (recur (rest c) current_key (assoc result current_key (#(conj % f) (get result current_key)))))))))))

(defcheck solution-574da081
  (fn [c]
    (loop [a {} r c]
      (if (empty? r)
        a
        (recur (conj a [(first r) (take-while #(not (keyword? %)) (rest r))])
          (drop-while #(not (keyword? %)) (rest r)))))))

(defcheck solution-5751f182
  (fn [m]
    (second
      (reduce
        (fn [[slot acc] x]
          (if (keyword? x)
            [x (assoc acc x [])]
            [slot (assoc acc slot (conj (acc slot) x))]))
        [nil {}]
        m))))

(defcheck solution-579f39a9
  (fn [s]
    (into {}
      (map #(vector (first %) (into [] (rest %)))
        (let [new (atom false)]
          (partition-by #(if (keyword? %)
                           (reset! new (not @new))
                           @new)
            s))))))

(defcheck solution-5820298c
  (fn foo [coll]
    (into {} (reduce (fn [acc elem]
                       (if (keyword? elem)
                         (conj acc [elem []])
                         (let [p (peek acc) kw (first p) vk (second p)]
                           (conj (pop acc) [kw (conj vk elem)]))
                         ))
               [] coll))))

(defcheck solution-58b3e62a
  (fn to-map [col]
    (first
      (reduce (fn [[res key] cur]
                (if (keyword? cur)
                  [(into res {cur []}) cur]
                  (let [key-entry (get res key)
                        key-entry (conj key-entry cur)]
                    [(into res {key key-entry}) key])))
        [{} (first col)] col))))

(defcheck solution-58b41a9b
  (fn kv
    ([x] (kv {} nil x))
    ([m last-key x]
     (if (seq x)
       (if (keyword? (first x))
         (kv (assoc m (first x) []) (first x) (rest x))     ; new key
         (kv (assoc m last-key (conj (m last-key) (first x))) last-key (rest x))) ; value
       m))))

(defcheck solution-58c6c247
  (fn [l]
    (loop [l l m {} k nil]
      (if (empty? l)
        m
        (if (keyword? (first l))
          (recur
            (rest l)
            (merge-with concat m {(first l) []})
            (first l))
          (recur
            (rest l)
            (merge-with concat m {k [(first l)]})
            k))))))

(defcheck solution-591337fe
  (fn f [[h & t]]
    (if (seq t)
      (let [[x y] (split-with (complement keyword?) t)]
        (merge {h x} (f y)))
      {})))

(defcheck solution-593e73e3
  #(let [[x y z]
         (reduce (fn [[w x y] z]
                   (cond
                     (and w (keyword? z)) [z [] (assoc y w x)]
                     (and (keyword? z)) [z [] y]
                     :else [w (conj x z) y]))
           [nil [] {}] %1)]
     (if x (assoc z x y) {})))

(defcheck solution-595470b7
  (fn [coll]
    (loop [ret {}
           k   nil
           c   coll]
      (if (empty? c)
        ret
        (if (keyword? (first c))
          (recur (merge ret {(first c) []}) (first c) (rest c))
          (recur (merge-with concat ret {k [(first c)]}) k (rest c)))))))

(defcheck solution-59a982bc
  (fn mf [s]
    (if (seq s)
      (merge {(first s) (take-while (complement keyword?) (rest s))}
        (mf (drop-while (complement keyword?) (rest s)))) {})))

(defcheck solution-59afd730
  (fn keys-and-values [xs]
    (let [{:keys [acc current vals]}
          (reduce
            (fn [{:keys [acc current vals]} val]
              (if (keyword? val)
                (if (nil? current)
                  {:acc     acc
                   :current val
                   :vals    vals}
                  {:acc     (assoc acc current vals)
                   :current val
                   :vals    []})
                {:acc     acc
                 :current current
                 :vals    (conj vals val)}))
            {:acc     {}
             :current nil
             :vals    []}
            xs)]
      (if (nil? current)
        acc
        (assoc acc current vals)))))

(defcheck solution-59ba03ed
  #(reduce
     (fn [m [k v]] (merge m
                     (zipmap k
                       (concat (repeat (dec (count k)) []) [v]))))
     {}
     (apply hash-map (partition-by keyword? %))))

(defcheck solution-5a34e99a
  (fn f [v]
    (let [ks (filter keyword? v) vs (repeat (count ks) []) im (zipmap ks vs)]
      (dissoc (reduce #(if (keyword? %2) (merge %1 {:k %2}) (merge-with conj %1 {(get %1 :k) %2})) im v) :k))))

(defcheck solution-5ae9c76c
  (fn kn->map
    ([s] (kn->map s {}))
    ([s m]
     (if (seq s)
       (let [k (first s)
             [v r] (split-with number? (rest s))]
         (recur r (assoc m k v)))
       m))))

(defcheck solution-5b69b5ba
  (fn identify-keys-and-values [coll]
    (->>
      (reduce #(if (and (keyword? (last %))
                        (keyword? %2))
                 (conj % nil %2)
                 (conj % %2)) [] coll)
      (partition-by keyword?)
      (map #(cond
              (keyword? (first %)) (first %)
              (nil? (first %)) []
              :else (into [] %)))
      (apply hash-map))))

(defcheck solution-5bafb0ca
  (fn [els]
    (:res
     (reduce (fn [acc x]
               (if (keyword? x)
                 (-> acc
                   (conj [:last x])
                   (assoc-in [:res x] []))
                 (update-in acc [:res (:last acc)] conj x)))
       {:last nil :res {}}
       els))))

(defcheck solution-5c366a83
  (fn f [s]
    (if (empty? s)
      {}
      (assoc (f (drop-while number? (rest s))) (first s) (take-while number? (rest s))))))

(defcheck solution-5d15de91
  (fn [v]
    (reduce (fn [m [ks vs]]
              (into (assoc m (last ks) (vec vs))
                (apply hash-map (interleave (butlast ks) (repeat (vec nil))))))
      {} (partition 2 (partition-by keyword? v)))))

(defcheck solution-5d39797e
  (fn [s] (first (reduce
                   (fn [[m k] x]
                     (if (keyword? x)
                       [(assoc m x []) x]
                       [(assoc m k (conj (k m) x)) k]))
                   [{} nil] s))))

(defcheck solution-5e3ada67
  #(second (reduce (fn [[k m] e] (if (keyword? e) [e (assoc m e [])] [k (assoc m k (conj (get m k) e))])) [nil {}] %)))

(defcheck solution-5e3fabfb
  (fn mapfromseq [x]
    (let [groupandupdate (fn [coll entry pred]
                           (if (pred entry)
                             (conj coll (vector entry))
                             (conj (pop coll) (conj (peek coll) entry))))
          split-before   (fn [coll pred]
                           (reduce #(groupandupdate %1 %2 pred) [] coll))]
      (reduce #(assoc %1 (first %2) (rest %2)) {} (split-before x keyword?)))))

(defcheck solution-5e6ac286
  (fn f [xs]
    (if-let [k (first xs)]
      (let [r (rest xs)
            v (take-while number? r)]
        (merge {k v} (f (drop-while number? r))))
      {})))

(defcheck solution-5f039c86
  #(loop [[k & s] % m {}]
     (if k
       (recur (drop-while number? s)
         (assoc m k (take-while number? s)))
       m)))

(defcheck solution-5fc02952
  (fn k [s]
    (second (reduce (fn [[p m] i]
                      (if (keyword? i)
                        [i (assoc m i [])]
                        [p (update-in m [p] #(conj % i))]))
              [nil {}] s))))

(defcheck solution-5fcf8cc
  (fn p105
    ([ls] (p105 {} nil ls))
    ([m ck ls] (if (empty? ls) m
                               (let [e (first ls)
                                     [ck1 m1] (if (keyword? e) [e (conj m [e []])] [ck (conj m [ck (conj (m ck) e)])])]
                                 (p105 m1 ck1 (next ls)))))))

(defcheck solution-605eb5cf
  (fn [a] (apply hash-map (reduce #(if (keyword? %2) (conj % %2 []) (assoc % (dec (count %)) (conj (last %) %2))) [] a))))

(defcheck solution-609ae7ea
  (fn [coll]
    (cond
      (empty? coll) {}
      :else (apply assoc {}
              (mapcat #(if (keyword? (first %))
                         (drop-last (interleave % (repeat [])))
                         [%])
                (partition-by keyword? coll))))))

(defcheck solution-60bcc445
  (fn [v]
    (loop [tmp []
           ret {}
           k   nil
           [ft & rt] v]
      (if (nil? ft)
        (if k
          (into ret [[k tmp]])
          ret)
        (if (keyword? ft)
          (if k
            (recur [] (into ret [[k tmp]]) ft rt)
            (recur [] ret ft rt))
          (recur (conj tmp ft) ret k rt))))))

(defcheck solution-60e5da08
  (fn [x]
    (loop [old x, new {}]
      (if (empty? old) new
                       (let [lk (first old)
                             v  (vec (take-while #(not (keyword? %)) (rest old)))]
                         (recur (drop (+ 1 (count v)) old) (assoc new lk v)))))))

(defcheck solution-60fe571f
  (fn F [l]
    (if (and l (not (= l ())))
      (let [t (split-with #(not (keyword? %)) (next l))]
        (conj (F (second t)) [(first l) (first t)]))
      {})))

(defcheck solution-610abb74
  (fn [x] (apply hash-map (map
                            #(if (keyword? (first %)) (first %) (keep identity %))
                            (partition-by #(when (keyword? %) %) (interpose nil x))))))

(defcheck solution-61646eba
  (fn func [[k & n]]
    (if k
      (let [[vs ns] (split-with #(false? (keyword? %)) n)]
        (assoc (func ns) k vs))
      {})))

(defcheck solution-6168499b
  (fn [c]
    (let [
          res  (reduce #(assoc % (last (first %2)) (second %2))
                 {}
                 (partition 2 (partition-by keyword? c)))
          diff (clojure.set/difference
                 (set (filter keyword? c))
                 (set (keys res)))]
      (merge
        (reduce #(assoc % %2 []) {} diff)
        res))))

(defcheck solution-617558c3
  (fn [coll]
    (letfn [(fknums [coll acc]
              (if (seq coll)
                (let [hd (first coll) tl (rest coll)]
                  (if (number? hd)
                    (recur tl [(first acc) (conj (second acc) hd)])
                    (lazy-seq (cons acc (fknums tl [hd []])))))
                [acc]))]
      (if (seq coll)
        (let [[first-key & more] (drop-while number? coll)]
          (into {} (map #(vec %) (fknums more [first-key []]))))
        {}))))

(defcheck solution-61995ee7
  (fn [l]
    (let [f #(if (keyword? %) % true)
          p (partition-by f l)]
      (loop [p p
             x {}]
        (cond
          (= p ()) x
          (= true (f (first (second p)))) (recur (drop 2 p)
                                            (assoc x (first (first p)) (second p)))
          :else (recur (rest p)
                  (assoc x (first (first p)) [])))))))

(defcheck solution-61dc20e1
  (fn kv-er [s]
    (if (seq s)
      (assoc
       (kv-er (drop-while (complement keyword?) (rest s)))
        (first s)
        (take-while (complement keyword?) (rest s)))
      {})))

(defcheck solution-61f9bce5
  (fn [v]
    (if (empty? v)
      {}
      (loop [result {(first v) []} current-key (first v) remains (rest v)]
        (if (empty? remains)
          result
          (let [nxt (first remains)
                kw? (keyword? nxt)]
            (recur (if kw?
                     (merge result {nxt []})
                     (merge-with conj result {current-key nxt}))
              (if kw?
                nxt
                current-key)
              (rest remains))))))))

(defcheck solution-620ad270
  (fn [c] (loop [coll [] int c]
            (if (empty? int)
              (into {} coll)
              (recur (conj coll (vector
                                  (first int) (vec
                                                (take-while number? (rest int))))) (drop (+ 1 (count (take-while number? (rest int)))) int))))))

(defcheck solution-622bb129
  (fn [l]
    (loop [i l m {}]
      (if (seq i)
        (recur (drop-while number? (next i))
          (assoc m (first i) (vec (take-while number? (next i)))))
        m))))

(defcheck solution-62a2cd83
  (fn [coll]
    (apply hash-map
      (reduce #(if (keyword? %2)
                 (conj % %2 [])
                 (conj (into [] (butlast %)) (conj (last %) %2)))
        []
        coll))))

(defcheck solution-62ac9f13
  (fn fx ([[h & r]] (let [[p n] (split-with number? r)] (if (nil? h) {} (conj (fx n) [h p]))))))

(defcheck solution-62c48ceb
  (fn gen-map
    [v]
    (reduce (fn [m c]
              (let [subv (subvec v (first c) (second c))]
                (assoc m (first subv) (rest subv)))) {} (partition
                                                          2 1 (concat (keep-indexed
                                                                        #(when (keyword? %2) %1) v) [(count v)])))))

(defcheck solution-62d42c89
  (fn kv-coll->map [coll]
    (letfn [(aux [left-coll this-acc curr-mp]
              (if (empty? left-coll)
                (merge (apply hash-map this-acc) curr-mp)
                (let [fst (first left-coll)
                      rst (rest left-coll)]
                  (if (number? fst)
                    (aux rst
                      [(first this-acc) (conj (second this-acc) fst)]
                      curr-mp)
                    (aux rst
                      [fst []]
                      (merge (apply hash-map this-acc) curr-mp))))))]
      (aux coll [] {}))))

(defcheck solution-631eccd8
  (fn [series]
    (second (reduce
              (fn [acc v]
                (let [k (first acc)
                      m (second acc)]
                  (if (keyword? v)
                    [v (assoc m v [])]
                    [k (assoc m k (conj (k m) v))]))
                ) [nil {}] series))))

(defcheck solution-63822a56
  (fn [x]
    (let [l
          (mapcat
            (fn [[k [l & i]]] [(first k) (into l i)])
            (partition 2
              (partition-by keyword?
                (mapcat #(if (keyword? %) [% []] [%]) x))))
          ]
      (if (empty? l) {} (apply assoc {} l))
      )
    ))

(defcheck solution-63932fc3
  (fn [v] (first
            (reduce (fn [[r curr] i] (if (keyword? i)
                                       [(assoc r i []) i]
                                       [(update-in r [curr] #(conj % i)) curr]))
              [{}] v))))

(defcheck solution-63ae98eb
  (fn [xs]
    (loop [xs xs m {} k nil acc []]
      (if (seq xs)
        (if (keyword? (first xs))
          (if (nil? k)
            (recur (rest xs) m (first xs) [])
            (recur (rest xs) (conj m [k acc]) (first xs) []))
          (recur (rest xs) m k (conj acc (first xs))))
        (if (empty? acc)
          m
          (conj m [k acc]))))))

(defcheck solution-64147cac
  (fn [l]
    (let [c (partition 2 (partition-by keyword? l))]
      (reduce (fn [a [k v]]
                (assoc (reduce #(assoc % %2 []) a k)
                  (last k) v)) {} c))))

(defcheck solution-64e0283
  (fn make-map [xs]
    (loop [r {} xs xs]
      (if (empty? xs)
        r
        (let [l (take-while (complement keyword?) (rest xs))]
          (merge r {(first xs) l} (make-map (drop (+ 1 (count l)) xs))))))))

(defcheck solution-6524eb34
  (fn [input]
    (loop [v input res {}]
      (if (empty? v) res
                     (recur (drop-while #(not (keyword? %)) (rest v))
                       (assoc res (first v) (take-while #(not (keyword? %)) (rest v))))))))

(defcheck solution-652c005d
  (fn cons-map
    [xs]
    (letfn [(map-entries [xs]
              (loop [[head & tail] xs
                     acc []]
                (if (nil? head) acc
                                (if (keyword? head)
                                  (recur tail (conj acc [head]))
                                  (let [last-entry (peek acc)]
                                    (recur tail (conj (pop acc) (conj last-entry head))))))))
            (add-default-for-missing-vals-and-format-entry [xs]
              (map #(if (= 1 (count %))
                      (conj % [])
                      [(first %) (rest %)]) xs))
            (reduce-in-map
              [entries-seq]
              (reduce
                (fn [m entry]
                  (conj m (apply hash-map entry)))
                {} entries-seq))]
      (-> xs
        map-entries
        add-default-for-missing-vals-and-format-entry
        reduce-in-map))))

(defcheck solution-6550af97
  (fn [s]
    (first
      (reduce (fn [[acc k] e]
                (if (keyword? e)
                  [(assoc acc e []) e]
                  [(assoc acc k (conj (get acc k) e)) k]))
        [{} nil] s))))

(defcheck solution-6589e380
  (fn [s]
    (loop [acc {}
           [k & r] s]
      (if-not k acc
                (recur
                  (conj acc [k (take-while integer? r)])
                  (drop-while integer? r))))))

(defcheck solution-65ed888e
  #(into {} (reduce (fn [a b]
                      (if (keyword? b)
                        (conj a [b []])
                        (let [[f s] (last a)]
                          (conj (vec (butlast a)) [f (conj s b)]))))
              [] %)))

(defcheck solution-660723f4
  (fn [coll]
    (reduce (fn [m [ks vs]]
              (-> (assoc m (last ks) vs)
                (into,,, (map #(vector % []) (drop-last ks)))))
      {}
      (->> (partition-by keyword? coll)
        (partition 2)))))

(defcheck solution-66375878
  (fn [coll] (->> (concat coll [nil])
               (interpose nil)
               (clojure.core/partition-by keyword?)
               (map (partial keep identity))
               (partition 2)
               (map (juxt (comp first first) second))
               (into {}))))

(defcheck solution-668d5a58
  (fn [s]
    (into {} (reduce (fn [v x]
                       (if (keyword? x)
                         (conj v [x []])
                         (let [[k ns] (last v)]
                           (conj (vec (butlast v)) [k (conj ns x)]))))
               [] s))))

(defcheck solution-669c29a9
  (fn mapping-keyword
    [coll]
    (loop [[x & xs] coll res {}]
      (if (keyword? x)
        (recur (drop-while number? xs)
          (conj res {x (take-while number? xs)}))
        res))))

(defcheck solution-66a00ed5
  (fn [col]
    (loop [s {} col col]
      (if (empty? col)
        s
        (recur (conj s [(first col) (take-while number? (rest col))])
          (drop-while number? (rest col)))))))

(defcheck solution-66a6b204
  (fn id-key-vals [coll]
    (loop [m {} cur-key nil coll coll]
      (cond
        (empty? coll) m
        (keyword? (first coll)) (recur (assoc m (first coll) []) (first coll) (rest coll))
        :else (recur (assoc m cur-key (conj (cur-key m) (first coll))) cur-key (rest coll))))))

(defcheck solution-66cc9697
  (fn [s]
    (loop [res {} list s symb nil elems []]
      (if (seq list)
        (let [current (first list)]
          (if (keyword? current)
            (if (keyword? symb)
              (recur (assoc res symb elems) (rest list) current [])
              (recur res (rest list) current elems))
            (recur res (rest list) symb (conj elems current))))
        (if (keyword? symb)
          (assoc res symb elems)
          res)))))

(defcheck solution-66d33d5f
  (fn [s]
    (loop [s s ret {}]
      (if (empty? s)
        ret
        (recur (second (split-with number? (rest s)))
          (assoc ret (first s) (first (split-with number? (rest s)))))))))

(defcheck solution-66e671b1
  (fn ikv [coll]
    (if (seq coll)
      (let [k (first coll)
            [v rs] (split-with number? (rest coll))]
        (merge {k v} (ikv rs)))
      {})))

(defcheck solution-673ef592
  (fn [x]
    (letfn [(consume [l]
              (if (empty? l)
                '()
                (let [k (first l)
                      v (second l)
                      r (rest (rest l))]
                  (if (= 1 (count k))
                    (cons {(first k) (vec v)}
                      (lazy-seq (consume r)))
                    (cons {(first k) []}
                      (lazy-seq (consume (cons (rest k) (cons v r)))))))))]
      (reduce into {} (consume (partition-by keyword? x))))))

(defcheck solution-67a9b041
  (fn [c]
    (loop [c c r {}]
      (if (not-any? keyword? c)
        r
        (recur (drop-while (complement keyword?) (rest c)) (conj r {(first c) (vec (take-while (complement keyword?) (rest c)))}))))))

(defcheck solution-67efc72c
  (fn [s]
    (loop [s     s
           accum {}]
      (if (empty? s)
        accum
        (let [k (first s)
              [vs rest-s] (split-with #(not (keyword? %)) (rest s))]
          (recur rest-s (assoc accum k vs)))))))

(defcheck solution-67fc317a
  (fn collect
    [l]
    (last
      (reduce
        (fn [[current-key agg] v]
          (if (keyword? v)
            [v (assoc agg v [])]
            [current-key (update-in agg [current-key] #(conj % v))]))
        [nil {}] l))))

(defcheck solution-684106be
  (fn [a]
    (if (empty? a) {}
                   (->>
                     (conj (vec (interpose nil a)) nil)
                     (partition-by keyword?)
                     (map #(remove nil? %))
                     (partition 2)
                     (map #(vector (first (first %)) (vec (second %))))
                     (apply map vector)
                     (apply zipmap)))))

(defcheck solution-685a130e
  (fn [col]
    (loop [m {}, k nil, c col]
      (if (empty? c)
        m
        (let [e (first c)]
          (if (number? e)
            (recur (into m {k (conj (m k) e)}) k (rest c))
            (recur (into m {e []}) e (rest c))
            )
          )
        )
      )
    ))

(defcheck solution-68969f7
  (fn [v]
    (loop [ansm {}
           k    nil
           vc   v
           ]
      (if (empty? vc)
        ansm
        (if (keyword? (first vc))
          (recur (assoc ansm (first vc) [])
            (first vc)
            (rest vc))
          (recur (assoc ansm k (conj (get ansm k) (first vc)))
            k
            (rest vc))
          )))))

(defcheck solution-689905d2
  (fn [x]
    (loop [l x
           a {}
           k nil]
      (let [i (first l)
            r (rest l)]
        (cond
          (empty? l) a
          (keyword? i) (recur r (assoc a i []) i)
          1 (recur r (assoc a k (conj (a k) i)) k)
          )))))

(defcheck solution-689b9a14
  (fn [v]
    (let [len (count v)]
      (loop [result {}
             i      0]
        (if (= i len) result
                      (let [j (loop [j (inc i)]
                                (if (or (= j len) (keyword? (nth v j))) j
                                                                        (recur (inc j)))
                                )]
                        (recur
                          (conj result [(nth v i) (subvec v (inc i) j)])
                          j)))))))

(defcheck solution-694d7f7a
  (fn kv [x]
    (if (empty? x)
      {}
      (let [k (first x) v (take-while number? (rest x))]
        (conj
          (kv (drop (inc (count v)) x))
          [k v]
          )
        )
      )
    ))

(defcheck solution-69b0f3d8
  (fn my-identify-keys-and-values
    [vals-to-group]
    (letfn [(get-rest [coll] (mapcat #(vector % []) (drop-last coll)))]
      (if (= [] vals-to-group)
        {}
        (apply merge (map #(if (= 1 (count (first %)))
                             (hash-map (first (first %)) (into [] (second %)))
                             (assoc (apply assoc {} (get-rest (first %))) (last (first %)) (into [] (second %))))
                       (partition 2 (partition-by type vals-to-group))))))))

(defcheck solution-69c0eef9
  (fn kv [seq]
    (if (empty? seq)
      {}
      (let [[k & r] seq]
        (into {k (take-while (complement keyword?) r)}
          (kv (drop-while (complement keyword?) r)))))))

(defcheck solution-69f43172
  (fn [coll]
    (letfn
     [(r [coll m]
        (if (empty? coll) m
                          (recur (drop-while #(not (keyword? %)) (rest coll))
                            (assoc m (first coll) (take-while #(not (keyword? %)) (rest coll))))))]
      (r coll {}))))

(defcheck solution-6a0bed11
  (fn [coll]
    (loop [[head & tail :as s] (seq coll)
           result {}]
      (if (seq s)
        (let [k head
              [v more] (split-with number? tail)]
          (recur more (assoc result k (vec v))))
        result))))

(defcheck solution-6a272bd8
  (fn kv [acc k [v & vs]]
    (cond (nil? v) acc
          (keyword? v) (kv (assoc acc v []) v vs)
          :e (kv (update-in acc [k] conj v) k vs))) {} *)

(defcheck solution-6a91973a
  (fn kv-identify [seq]
    (letfn [(iden [[m acc lkw] e]
              (if (keyword? e)
                (if (nil? lkw)
                  [m acc e]
                  [(assoc m lkw acc) [] e])
                [m (conj acc e) lkw]))]
      (let [[m acc lkw] (reduce iden [{} [] nil] seq)]
        (merge
          m
          (if (empty? acc) {}
                           {lkw acc}))))))

(defcheck solution-6b0bbd9f
  (fn [s] (let [ss (partition-by type s)
                ss (mapcat #(if (keyword? (first %)) (interpose [] %) [%]) ss)]
            (zipmap (take-nth 2 ss)
              (conj (vec (map vec (take-nth 2 (rest ss)))) [])))))

(defcheck solution-6b226a45
  (fn unflatten-map [s]
    (loop [ret     {}
           prev-kw []
           s       s]
      (let [f          (first s)
            kw?        (keyword? f)
            num?       (not kw?)
            this-kw    (if kw? f prev-kw)
            prev-list  (vec (ret this-kw))
            next-value (vec (concat (ret this-kw) (if num? [f] [])))
            next-map   (assoc ret this-kw (if num? next-value []))]
        (if (seq s)
          (recur next-map this-kw (rest s))
          ret)))))

(defcheck solution-6b9f40b9
  (fn [x]
    (apply hash-map
      (reduce
        #(if (keyword? %2)
           (conj %1 %2 [])
           (conj (pop %1) (conj (last %1) %2)))
        [] x))))

(defcheck solution-6bfe5132
  (fn to-map [xs]
    (apply hash-map
      (mapcat #(if (keyword? (first %1)) (interpose '() %1) (list %1)) (partition-by keyword? xs)))))

(defcheck solution-6c0cd984
  (fn f
    ([s] (if (empty? s) {}
                        (into {} (f (rest s) (first s) [] []))))
    ([xs k v r]
     (cond
       (empty? xs) (conj r [k v])
       (keyword? (first xs)) (recur (rest xs) (first xs) [] (conj r [k v]))
       :else (recur (rest xs) k (conj v (first xs)) r)))))

(defcheck solution-6c1c9f3b
  (fn foo [coll]
    (let [asvec (vec coll)]
      (reduce (fn [r [[i k] [j _]]]
                (assoc r k (subvec asvec (inc i) j)))
        {} (partition 2 1 [[(count asvec) nil]] (filter
                                                  (fn [[_ v]] (keyword? v))
                                                  (map-indexed #(vector %1 %2) asvec)))))))

(defcheck solution-6d170716
  (fn f [[x & q]]
    (if x
      (let [[v r] (split-with number? q)]
        (assoc (f r) x v))
      {})))

(defcheck solution-6d315a4e
  (fn [ls]
    (if (empty? ls)
      {}
      (into {} (loop [res [] k (first ls) v [] l (rest ls)]
                 (cond
                   (empty? l) (conj res [k v])
                   (keyword? (first l))
                   (recur (conj res [k v]) (first l) [] (rest l))
                   :else
                   (recur res k (conj v (first l)) (rest l))))))))

(defcheck solution-6d6a2622
  (fn [s]
    (loop [rs s keys [] vals []]
      (if (empty? rs)
        (zipmap keys vals)
        (recur (drop-while number? (rest rs)) (conj keys (first rs)) (conj vals (take-while number? (rest rs))))))))

(defcheck solution-6ebcc546
  (fn [xs]
    (into {} (reduce (fn [a x] (if (keyword? x)
                                 (cons [x []] a)
                                 (cons [(first (first a))
                                        (conj (second (first a)) x)]
                                   (rest a))))
               []
               xs))

    ))

(defcheck solution-6f5e2365
  (fn [v]
    (if (empty? v) {}
                   (into {} (map #(if (= [[]] (second %)) [(first %) []] (vec %)) (partition 2 (reduce
                                                                                                 #(if (keyword? (first %2))
                                                                                                    (conj %1 (first %2))
                                                                                                    (conj %1 (vec %2)))
                                                                                                 [] (partition-by keyword? (reduce
                                                                                                                             #(if (and (keyword %2) (keyword? (last %1)))
                                                                                                                                (conj %1 [] %2)
                                                                                                                                (conj %1 %2))
                                                                                                                             [] v)))))))))

(defcheck solution-6f85f313
  (fn
    [col]
    (if (seq col)
      (letfn [(f [remaining kw kw-val m]
                (if (empty? remaining)
                  (merge m {kw kw-val})
                  (let [item (first remaining)
                        ]
                    (if (keyword? item)
                      (recur (rest remaining) item [] (merge m {kw kw-val}))
                      (recur (rest remaining) kw (into kw-val (vector item)) m)
                      )                                     ; if
                    )                                       ; let
                  )                                         ; if
                )                                           ; fn f
              ]
        (f (rest col) (first col) [] {})
        )                                                   ; letfn
      {})
    ))

(defcheck solution-6f8df98f
  (fn [l]
    (if (empty? l)
      {}
      (loop [news {} tmpl (rest l) tmps '() keyw (first l)]
        (if (empty? tmpl)
          (assoc news keyw (reverse tmps))
          (if (keyword? (first tmpl))
            (recur (assoc news keyw (reverse tmps)) (rest tmpl) '() (first tmpl))
            (recur news (rest tmpl) (conj tmps (first tmpl)) keyw)))))))

(defcheck solution-6fb54838
  (fn f [[k & s]]
    (if k
      (let [[n m] (split-with integer? s)]
        (assoc (f m) k n))
      {})))

(defcheck solution-6ff0017f
  (fn to-keys [items]
    (let [pairs (partition 2 (partition-by keyword? items))]
      (into {} (mapcat (fn [[keys v]]
                         (conj (map #(vector % '()) (butlast keys))
                           (vector (last keys) v)))
                 pairs)))))

(defcheck solution-7062bf6c
  (fn [xs]
    (loop [acc {} x (partition-by #(if (keyword? %) % nil) xs)]
      (if (empty? x) acc
                     (let [[[a] b & c] x]
                       (if (keyword? (first b))
                         (recur (assoc acc a []) (rest x))
                         (recur (assoc acc a b) c)))))))

(defcheck solution-7066313f
  (fn [s]
    (->>
      s
      (partition-by #(if (keyword? %) (gensym) false))
      (partition 2 1)
      (filter #(keyword? (first (first %))))
      (map (fn [[[x & xs] & [y & ys]]] [x y]))
      (map (fn [[x & [[y & yt :as ys]]]] (if (number? y) [x ys] [x []])))
      (into {})
      )))

(defcheck solution-70feb077
  (fn buildit [a]
    (if (empty? a)
      {}
      (let [h         (first a)
            parts     (take-while #(not (keyword? %)) (rest a))
            remaining (drop-while #(not (keyword? %)) (rest a))]
        (assoc (buildit remaining) h (vec parts))))))

(defcheck solution-7116291b
  (fn [xs]
    (loop [xs xs m {} k nil]
      (if (seq xs)
        (if (keyword? (first xs))
          (recur (rest xs) (assoc m (first xs) []) (first xs))
          (recur (rest xs) (assoc m k (conj (m k) (first xs))) k))
        m))))

(defcheck solution-71cb5e97
  (let [not-kw? (comp not keyword?)]
    (fn ->kw-map [xs]
      (loop [m {}, xs (drop-while not-kw? xs)]
        (if-let [[kw & xs] (seq xs)]
          (let [[vs xs] (split-with not-kw? xs)]
            (recur (assoc m kw vs) xs))
          m)))))

(defcheck solution-71f4d374
  #(let [a (atom 0)]
     (reduce (fn [m n]
               (if (keyword? n)
                 (assoc m (reset! a n) [])
                 (update-in m [@a] into [n])))
       {} %)))

(defcheck solution-7236d388
  (fn [ss]
    (loop [s (rest ss) r {} k (first ss) v []]
      (if (empty? s) (if (nil? k) {} (assoc r k v))
                     (if (number? (first s))
                       (recur (rest s) r k (conj v (first s)))
                       (recur (rest s) (assoc r k v) (first s) []))))))

(defcheck solution-727a943
  (fn pp [xs]
    (letfn [(nk [v] (not (keyword? v)))
            (p [[x :as xs]]
              (lazy-seq
                (when (seq xs)
                  (cons [x (into [] (take-while nk (next xs)))] (p (drop-while nk (next xs)))))))]
      (into {} (p xs)))))

(defcheck solution-727fff56
  #(loop [[x & xs] %
          k nil
          r {}]
     (if (nil? x)
       r
       (if (keyword? x)
         (recur xs x (assoc r x []))
         (recur xs k (assoc r k (conj (r k) x)))))))

(defcheck solution-728cb975
  (fn [s]
    (into {}
      (reduce (fn [r x]
                (if (keyword? x)
                  (into [] (cons [x []] r))
                  (update-in r [0 1] conj x)))
        []
        s))))

(defcheck solution-72c2c570
  (fn q105 [coll]
    (->> coll
      (partition-by keyword?)
      (partition-all 2)
      (reduce (fn [m [keys vals]]
                (assoc
                 (reduce #(assoc % %2 []) m (butlast keys))
                  (last keys) (apply vector vals)))
        {}))))

(defcheck solution-72e2355a
  #(loop [m {} [k & r :as s] %] (if (seq s) (let [[v r] (split-with (complement keyword?) r)] (recur (assoc m k (vec v)) r)) m)))

(defcheck solution-72e6518f
  (fn [coll]
    (into {}
      (for [x coll
            :let [newc (drop 1 (drop-while #(not= % x) coll))]
            :when (keyword? x)]
        {x (into []
             (take-while #(not (keyword? %)) newc))}))))

(defcheck solution-73236188
  (fn [v]
    (loop [r (seq v) k nil m {}]
      (let [v (first r)]
        (cond (empty? r) m
              (keyword? v) (recur (rest r) v (assoc m v []))
              :else (recur (rest r) k (assoc m k (conj (m k) v))))))))

(defcheck solution-732a5671
  (fn keyword-map [coll]
    (first
      (reduce
        (fn [[a b] c]
          (if (keyword? c)
            [(conj a [c []]) c]
            [(assoc a b (conj (get a b) c)) b]))
        [{} nil]
        coll))))

(defcheck solution-7369002c
  (fn [coll]
    (apply hash-map (reverse
                      (reduce (fn [a x]
                                (if (keyword? x)
                                  (conj a x [])
                                  (conj (rest a) (conj (first a) x))))
                        () coll)))))

(defcheck solution-740b28ad
  (letfn
   [(aux [[fst & rst :as xs] [[fstm] :as m]]
      (cond
        (not (seq xs)) m
        (keyword? fst) (aux rst (cons [fst] m))
        :else (aux rst (cons [fstm fst] m))))]
    (fn [sx]
      (reduce
        (fn [m [fst & rst]] (update-in m [fst] (comp vec concat) rst))
        {} (reverse (aux sx []))))))

(defcheck solution-74134d60
  (fn [v]
    (->>
      (for [[keys vals] (partition 2 (partition-by keyword? v))]
        (concat
          [(last keys) (vec vals)]
          (mapcat #(vec (list % [])) (butlast keys))))
      (apply concat)
      (apply hash-map))))

(defcheck solution-742f8ca9
  (fn [v]
    (loop [[f :as xs] v, k nil, acc {}]
      (cond
        (empty? xs) acc
        (keyword? f) (recur (rest xs) f (assoc acc f []))
        :else (recur (rest xs) k (assoc acc k (conj (acc k) f)))))))

(defcheck solution-74717df7
  (fn map-by-keywords
    [coll]
    (letfn [(map-by-keywords' [coll]
              (when-let [s (seq coll)]
                (let [[x & xs] s]
                  (cons [x (take-while number? xs)]
                    (lazy-seq (map-by-keywords' (drop-while number? xs)))))))]
      (into {} (map-by-keywords' coll)))))

(defcheck solution-74757700
  (fn [m]
    (let [mm (partition-by #(keyword? %) m)]
      (loop [ret {} cnt mm]
        (if (empty? cnt)
          ret
          (let [h1 (first cnt)
                h2 (second cnt)]
            (if (= 2 (count h1))
              (recur (assoc ret (first h1) [])
                (cons (rest (first cnt)) (rest cnt)))
              (recur (assoc ret (first h1) h2)
                (drop 2 cnt)))))))))

(defcheck solution-74fc75dc
  (fn xx [[k & rst]]
    (let [[vls tail] (split-with (complement keyword?) rst)]
      (if (nil? k) {}
                   (assoc (xx tail) k vls)))))

(defcheck solution-7517ca19
  (fn [s]
    (let [kwords-and-nums (->> (partition-by type s) (partition 2 2 []))
          create-map      (fn [[ks nums]] (-> (zipmap ks (repeat [])) (assoc (last ks) nums)))
          ]
      (->> (map create-map kwords-and-nums) (reduce merge {}))
      )
    ))

(defcheck solution-75ac4702
  (fn ik [coll]
    (if (empty? coll)
      {}
      (loop [c coll r {} k nil]
        (if (empty? c)
          r
          (recur
            (rest c)
            (if (keyword? (first c)) (assoc r (first c) []) (update-in r [k] conj (first c)))
            (if (keyword? (first c)) (first c) k)))))))

(defcheck solution-75e09cc1
  (fn cm [v]
    (if (empty? v) {}
                   (assoc
                    (cm (drop-while #(not (keyword? %)) (rest v)))
                     (first v)
                     (take-while #(not (keyword? %)) (rest v))
                     ))
    ))

(defcheck solution-75f12983
  (fn [s]
    (loop [k nil, h {}, r s]
      (if (empty? r)
        h
        (let [f (first r)
              n (next r)]
          (if (keyword? f)
            (recur f (assoc h f []) n)
            (recur k
              (update-in h [k]
                #(conj % f))
              n)))))))

(defcheck solution-75faf76
  (fn kv [coll]
    (if (keyword? (first coll))
      (let [vs    (take-while (complement keyword?) (rest coll))
            vrest (drop-while (complement keyword?) (rest coll))]
        (merge {(first coll) vs} (kv vrest))
        )
      {}
      )
    ))

(defcheck solution-762ee69b
  (fn [arr]
    (apply hash-map (reduce (fn [r n]
                              (if (keyword? n)
                                (concat r [n []])
                                (concat (butlast r) [(concat (last r) [n])])))
                      [] arr)
      )))

(defcheck solution-763f0e41
  (fn [es]
    (loop [es es
           m  {}]
      (if-not (seq es)
        m
        (let [k (first es)
              [vs next-es] (split-with (comp not keyword?) (rest es))]
          (recur
            next-es
            (conj m [k vs])))))))

(defcheck solution-766febfb
  (fn keys&vals [coll]
    (if (empty? coll)
      {}
      (loop [kw (first coll), vs [], result {}, more (rest coll)]
        (cond
          (empty? more)
          (assoc result kw vs)
          (keyword? (first more))
          (recur (first more) [] (assoc result kw vs) (rest more))
          :else
          (recur kw (conj vs (first more)) result (rest more)))))))

(defcheck solution-76d05294
  (fn ikv [v]
    (if (empty? v)
      {}
      (loop [r {}
             v v
             k nil
             p []]
        (cond
          (nil? (first v)) (assoc r k (vec p))
          (nil? k) (recur r (rest v) (first v) p)
          (keyword? (first v)) (recur (assoc r k (vec p)) (rest v) (first v) [])
          :else (recur r (rest v) k (concat p [(first v)])))))))

(defcheck solution-776232d7
  (fn my-change
    [coll]
    (if (empty? coll)
      {}
      (loop [allacc      {}
             curracc     []
             currkeyword (first coll)
             coll        (rest coll)]
        (if (empty? coll)
          (conj allacc [currkeyword curracc])
          (if (keyword? (first coll))
            (recur
              (conj allacc [currkeyword curracc])
              []
              (first coll)
              (rest coll))
            (recur
              allacc
              (conj curracc (first coll))
              currkeyword
              (rest coll))))))))

(defcheck solution-779b6e9a
  (fn [s]
    (loop [[k v & r] (partition-by keyword? s) a {}]
      (if k
        (recur r (assoc (reduce #(assoc % %2 []) a (butlast k)) (last k) v))
        a))))

(defcheck solution-77c88120
  #({0 {}
     2 {:a [1]}
     4 {:a [1] :b [2]}
     7 {:a [1 2 3] :b [] :c [4]}}
    (count %)))

(defcheck solution-77cda8fa
  (fn [init]
    (let [add (fn [result sym vals] (if sym (conj result {sym vals}) result))]
      (loop [s init result {} curvals [] cursym nil]
        (cond (empty? s) (add result cursym curvals)
              (keyword? (first s)) (recur (rest s) (add result cursym curvals) [] (first s))
              true (recur (rest s) result (conj curvals (first s)) cursym))))))

(defcheck solution-787f84b0
  (fn [l]
    (into
      {}
      ((fn [a [k & vs]]
         (if (nil? k)
           a
           (if (empty? vs)
             (conj a [k []])
             (recur (conj a [k
                             (apply vector
                               (take-while (comp not keyword?) vs))
                             ])
               (drop-while (comp not keyword?) vs))
             )
           )
         )
       [] l)
      )
    ))

(defcheck solution-79047ed0
  (fn [coll]
    (let [updated (fn [dict key val]
                    (assoc dict key
                                (conj (dict key []) val)))
          add     (fn [[dict key] el]
                    (if (keyword? el)
                      [(assoc dict el (dict el [])) el]
                      [(updated dict key el) key]))]
      (first
        (reduce add [{} nil] coll)))))

(defcheck solution-793d3591
  #((fn f [x y z]
      (if-not (empty? z)
        (if (keyword? (first z))
          (f (assoc x (first z) []) (first z) (rest z))
          (f (merge-with into x {y [(first z)]}) y (rest z)))
        x)) {} (first %) (rest %)))

(defcheck solution-7977a1f4
  (fn [xs]
    ((fn [xs k acc]
       (if (empty? xs) acc
                       (let [x (first xs) xs' (rest xs)]
                         (if (keyword? x) (recur xs' x (assoc acc x []))
                                          (recur xs' k (update-in acc [k] #(conj % x)))))))
     xs :d {})))

(defcheck solution-7983ae66
  (fn [s]
    (loop [s s res {} lastk (first s) cur []]
      (if s
        (let [e   (first s)
              isk (keyword? e)]
          (if isk
            (recur (next s) (assoc res lastk cur) e [])
            (recur (next s) res lastk (conj cur e))))
        (if lastk
          (assoc res lastk cur)
          {})))))

(defcheck solution-7a15deea
  (fn solve [s]
    (letfn
     [(iter [result left]
        (cond
          (empty? left) result
          (keyword? (first left)) (iter (assoc result (first left) (take-while number? (rest left)))
                                    (drop-while number? (rest left)))))]
      (iter {} s))))

(defcheck solution-7a1e5f6c
  (fn [xs]
    ((fn [xs l]
       (if (empty? xs) (reduce #(assoc %1 (first %2) (rest %2)) {} l)
                       (if (keyword? (first xs)) (recur (rest xs) (conj l [(first xs)]))
                                                 (recur (rest xs) (conj (rest l) (conj (first l) (first xs)))))))
     xs [])))

(defcheck solution-7a28c750
  (fn group-when-keyword [c]
    (if (empty? c)
      {}
      (let [split (split-with (comp not keyword?) (rest c))]
        (into {(first c) (first split)} (group-when-keyword (last split)))))))

(defcheck solution-7a324350
  (comp
    (partial apply hash-map)
    (partial mapcat
      #(if (keyword? (first %))
         (interpose [] %)
         [%]))
    (partial partition-by keyword?)))

(defcheck solution-7a52fb9e
  (fn v2m [seq]
    (let [v? (complement keyword?)]
      (if (empty? seq) {}
                       (into {(first seq) (vec (take-while v? (rest seq)))}
                         (v2m (drop-while v? (rest seq))))))))

(defcheck solution-7a7be0d2
  (fn to-map [a-seq]
    (letfn [(split-keywords [el]
              (let [f (first el)]

                (if (keyword? f)
                  (if (> (count el) 1)
                    (interpose '() el)
                    el
                    )
                  (list el)
                  )
                )
              )]
      (apply hash-map (mapcat split-keywords (partition-by #(keyword? %) a-seq)))
      )
    ))

(defcheck solution-7a804064
  (fn f [[h & t]]
    (into {}
      (if h
        (let [[y z] (split-with number? t)]
          (cons [h y]
            (f z)))))))

(defcheck solution-7a8daa86
  (fn somemap [x]
    (loop [x' x r {} k (first x)]
      #_(print r)
      (cond
        (empty? x') r
        (keyword? (first x')) (recur (rest x') (conj r {(first x') []}) (first x'))
        :else (recur (rest x') (update-in r [k] concat [(first x')]
                                 ) k)
        ))))

(defcheck solution-7a8e9e94
  (fn [s]
    (loop [s s
           m {}]
      (if (empty? s)
        m
        (let [k (first s)
              [vs s] (split-with (complement keyword?) (rest s))]
          (recur s (assoc m k (vec vs))))))))

(defcheck solution-7aa9a44a
  (fn pb [xs] (if (empty? xs) {} (let [tk (take-while #(not (keyword? %)) (rest xs))] (into {(first xs) tk} (pb (drop (inc (count tk)) xs)))))))

(defcheck solution-7ad16e9b
  #(->> %
     (partition-by keyword?)
     (mapcat (fn [[k :as v]] (if (keyword? k) (interpose [] v) [v])))
     (apply hash-map)))

(defcheck solution-7be92793
  (fn [x] (into {} (map-indexed #(when (keyword? %2) (assoc {} %2 (take-while number? (subvec x (inc %))))) x))))

(defcheck solution-7bf65f1c
  (fn [coll]
    (loop [ret {} l coll]
      (if (seq l)
        (recur (assoc ret (first l) (take-while (comp not keyword?) (rest l))) (drop-while (comp not keyword?) (rest l)))
        ret))))

(defcheck solution-7c634950
  (comp
    (partial apply hash-map)
    (partial reduce
      (fn [a [b :as c]]
        (if (keyword? b)
          (into a (interleave c (repeat [])))
          (conj (vec (drop-last a)) c)))
      [])
    (partial partition-by keyword?)))

(defcheck solution-7c92dfbb
  (fn [xs]
    (apply hash-map
      (reduce
        (fn [a x] (if (keyword? x)
                    (vec (concat a [x []]))
                    (update-in a [(dec (count a))] #(conj % x))))
        [] xs))))

(defcheck solution-7dd176ad
  (fn ! [a]
    (if (seq a)
      ((fn [[s b]] (assoc (! b) (first a) s))
       (split-with number? (next a)))
      {})
    ))

(defcheck solution-7de0f092
  (fn [y]
    (letfn [(r [a x]
              #_(println a)
              (if (keyword? x)
                {:result
                 (assoc (:result a) x [])
                 :current
                 x
                 }
                (update-in a [:result (:current a)] conj x)
                ))]
      (:result (reduce r {:result {} :current :x} y)))))

(defcheck solution-7e169862
  (fn [s]
    (first (reduce (fn [[m k] v]
                     (if (keyword? v)
                       [(assoc m v []) v]
                       [(update-in m [k] conj v) k]))
             [{} :dummy] s))))

(defcheck solution-7e1ba823
  #(second (reduce (fn [[l m] e] (if (keyword? e)
                                   [e (assoc m e [])]
                                   [l (update-in m [l] conj e)])) [:? {}] %)))

(defcheck solution-7e1f12bb
  (fn [coll]
    (apply hash-map (reduce (fn [r x]
                              (if (number? (first x))
                                (conj r x)
                                (apply conj r (interpose [] x))))
                      []
                      (partition-by keyword? coll)))))

(defcheck solution-7e6acfd8
  (fn [l] (loop [[h & t] (reverse l), mp {}, v ()]
            (cond
              (nil? h) mp
              (keyword? h) (recur t (assoc mp h v) ())
              :else (recur t mp (cons h v))))))

(defcheck solution-7e728efa
  (fn build-map [v]
    (loop [res {} key (first v) src (rest v)]
      (if (empty? src)
        res
        (let [newone (first src)]
          (if (keyword? newone)
            (recur (assoc res newone [])
              newone
              (rest src))
            (recur (assoc res key (conj (get res key []) newone))
              key
              (rest src))))))))

(defcheck solution-7ed030a9
  (fn [coll]
    (second
      (reduce (fn [[kw dict] x]
                (if (keyword? x)
                  [x (assoc dict x [])]
                  [kw (assoc dict kw (conj (dict kw) x))]))
        [nil, {}] coll))))

(defcheck solution-7f34dfa8
  (fn
    [coll]
    (letfn [(step [[m k v] x]
              (if (keyword? x)
                (if k
                  [(concat m [k v]) x []]
                  [m x []])
                [m k (conj v x)]
                ))]
      (if (seq coll)
        (let [[m k v] (reduce step [[] nil nil] coll)]
          (apply hash-map (concat m [k v])))
        {}))))

(defcheck solution-7f36a322
  (fn key-val [xs]
    (loop [acc {} rs xs]
      (if (seq rs)
        (recur
          (assoc acc (first rs) (take-while (comp not keyword?) (rest rs)))
          (drop-while (comp not keyword?) (rest rs)))
        acc))))

(defcheck solution-7f5ae169
  (fn [s]
    (second (reduce (fn [[k m] x]
                      (if (integer? x)
                        [k (assoc m k (conj (get m k) x))]
                        [x (assoc m x [])])
                      ) [nil {}] s))))

(defcheck solution-7fe7bf1f
  (fn [xs] (apply hash-map (apply concat [] (map #(if (keyword? (first %)) (interpose [] %) [%]) (partition-by keyword? xs))))))

(defcheck solution-80204583
  (fn prob105 [s]
    (loop [s    s
           prev nil
           ret  {}]
      (if (empty? s)
        ret
        (let [f    (first s)
              key  (if (keyword? f) f prev)
              prev key
              val  (ret key nil)
              ]
          (recur (rest s) key (assoc ret key (if (keyword? f) [] (conj (ret key) f))))
          )))))

(defcheck solution-8071ef75
  (fn [c] (loop [x c r {}]
            (if (empty? x)
              r
              (let [k (first x)
                    v (take-while #(not (keyword? %)) (rest x))]
                (recur (drop (+ 1 (count v)) x) (assoc r k (vec v))))))))

(defcheck solution-81480361
  #(dissoc
     (reduce
       (fn [m e]
         (if (keyword? e)
           (assoc m e [] :last e)
           (assoc m (m :last) (conj (m (m :last)) e))))
       (sorted-map)
       %) :last))

(defcheck solution-81596b7e
  (fn f [s]
    (if (empty? s)
      {}
      (let [[nbs rst] (split-with #(not (keyword? %)) (rest s))]
        (conj (f rst) {(first s) (vec nbs)})))))

(defcheck solution-8172352c
  (fn g
    ([xs] (g {} nil xs))
    ([m k xs]
     (let [f (first xs)]
       (cond
         (empty? xs) m
         (keyword? f) (recur (assoc m f []) f (rest xs))
         :else (recur (assoc m k (conj (k m) f)) k (rest xs)))))))

(defcheck solution-81e47e6a
  (fn keys-values [s]
    (second (reduce
              (fn [[k m] e] (if (keyword? e)
                              [e (assoc m e [])]
                              [k (assoc m k (conj (m k) e))]))
              [nil {}]
              s))))

(defcheck solution-82008cba
  (fn [x]
    (loop [i 0 result {}]
      (cond
        (>= i (count x)) result
        (keyword? (get x i)) (recur (inc i) (into result {(get x i) []}))
        :else (recur (inc i) (merge-with into result {(key (last result)) [(get x i)]}))
        )

      )
    ))

(defcheck solution-8202c94d
  (fn [c] (apply hash-map (mapcat #(if (every? keyword? %) (interpose () %) (vector %)) (partition-by keyword? c)))))

(defcheck solution-820852d6
  #(if (seq %)
     (loop [m {} k nil v [] coll %]
       (if (empty? coll)
         (assoc m k v)
         (let [a (first coll) coll (rest coll)]
           (if (keyword? a)
             (if (nil? k)
               (recur m a [] coll)
               (recur (assoc m k v) a [] coll))
             (if (nil? k)
               (recur m k v coll)
               (recur m k (conj v a) coll))))))
     {}))

(defcheck solution-826200f2
  (fn [v]
    (loop [result {} current v last-key nil]
      (if (empty? current) result
                           (recur
                             (if (keyword? (first current))
                               (if (or (nil? last-key) (contains? result last-key))
                                 result
                                 (assoc result last-key []))
                               (assoc result last-key
                                             (if (contains? result last-key)
                                               (conj (get result last-key) (first current))
                                               [(first current)])))
                             (rest current)
                             (if (keyword? (first current))
                               (first current)
                               last-key))))))

(defcheck solution-8290c87a
  (fn [c]
    (loop [a {} c c]
      (if (seq c)
        (let [k  (first c)
              vs (vec (take-while (comp not keyword?) (rest c)))]
          (recur (assoc a k vs) (drop (count vs) (rest c))))
        a))))

(defcheck solution-82a8c164
  (fn [coll]
    (reduce
      (fn [mp it]
        (if (keyword? it)
          (assoc mp it [])
          (let [k (last (keys mp))]
            (assoc mp k (conj (mp k) it)))))
      (sorted-map)
      coll)))

(defcheck solution-82d89928
  (fn [x]
    (if (empty? x) {}
                   (loop [a (rest x) kw (first x) v [] b {}]
                     (if (empty? a)
                       (assoc b kw v)
                       (if (keyword? (first a))
                         (recur (rest a) (first a) [] (assoc b kw v))
                         (recur (rest a) kw (conj v (first a)) b)))))))

(defcheck solution-834b5fc5
  (fn roll [v]
    (if (empty? v)
      {}

      (loop [rv      {(first v) []}
             current (first v)
             v       (rest v)]
        (cond
          (empty? v) rv
          (keyword? (first v)) (recur (assoc rv (first v) []) (first v) (rest v))
          :else
          (recur (assoc rv current (conj (current rv) (first v))) current (rest v)))))))

(defcheck solution-8360bc33
  (fn [v]
    (apply hash-map (mapcat
                      identity
                      (partition 2 (mapcat
                                     #(if (keyword? (first %))
                                        (interpose [] %)
                                        (list %))
                                     (partition-by keyword? v)))))))

(defcheck solution-83c29cd9
  (fn [s]
    (apply array-map (reduce
                       #(if (keyword? %2) (conj %1 %2 []) (conj (pop %1) (conj (peek %1) %2))) [] s))))

(defcheck solution-83e1cab2
  (fn [c]
    (let [to-map (fn [[keys values]] (concat (map #(vector % []) (butlast keys)) [[(last keys) values]]))]
      (into {} (->> c
                 (partition-by keyword?)
                 (partition 2)
                 (mapcat to-map))))))

(defcheck solution-83f95359
  (fn [xset]
    (apply array-map (reduce #(if (number? %2)
                                (conj (vec (drop-last %1)) (conj (last %1) %2))
                                (conj %1 %2 [])) [] xset))))

(defcheck solution-840ebce5
  (fn [coll]
    (loop [x coll res {}]
      (if (empty? x) res
                     (recur (drop-while number? (rest x)) (assoc res (first x) (vec (take-while number? (rest x)))))))))

(defcheck solution-846cab5e
  (fn [s]
    (reduce
      (fn build-key-value [m coll]
        (let [[k & values] coll]
          (assoc m k (if (empty? values) [] values))))
      {}
      ((fn partition-by-key [l]
         (when (seq l)
           (cons
             (conj
               (take-while (complement keyword?) (rest l))
               (first l))
             (lazy-seq
               (partition-by-key
                 (drop-while (complement keyword?) (rest l))))))) s))))

(defcheck solution-8480a659
  (fn [xs]
    (first
      (reduce
        (fn [[m k] x]
          (if (integer? x)

            (let
             [
              l  (get m k)
              l' (conj l x)
              m' (assoc m k l')
              ]
              [m' k]
              )

            (let
             [
              m' (assoc m x [])
              ]
              [m' x]
              )
            )
          )
        [{} nil]
        xs
        )
      )
    ))

(defcheck solution-84c05d85
  (fn conv [s]
    (reduce (fn [r [k v]]
              (assoc (into r (zipmap k (repeat []))) (last k) v)) {}
      (partition 2 (partition-by keyword? s)))))

(defcheck solution-85177e73
  (fn boo
    ([[x & xs]] (if (nil? x) {} (boo xs x [] {})))
    ([[x & xs] kword local accu]
     (cond
       (nil? x) (conj accu {kword local})
       (keyword? x) (recur xs x [] (conj accu {kword local}))
       :else (recur xs kword (conj local x) accu)))))

(defcheck solution-8593f360
  (fn [xs]
    (first
      (reduce (fn [[m k] v]
                (if (keyword? v)
                  [(assoc m v []) v]
                  [(update-in m [k] conj v) k]))
        [{} nil] xs))))

(defcheck solution-861ba896
  (fn [x]
    (into {}
      (filter (fn [[k v]] (keyword? k))
        (into {} (map-indexed (fn [i n]
                                {n (take-while (complement keyword?) (drop (inc i) x))}) x))))))

(defcheck solution-8643345
  (fn [v]
    (loop [i v o {} k :x]
      (let [h (first i) r (rest i)]
        (if (empty? i)
          o
          (if (keyword? h)
            (recur r (assoc o h []) h)
            (recur r (assoc o k (conj (o k) h)) k)))))))

(defcheck solution-864c99ee
  (fn _ [c]
    (loop [f (first c), z (next c), m {}, r {}]
      (cond
        (nil? f) (into r m)
        (keyword? f) (recur (first z) (next z) {f '()} (into r m))
        :else (recur (first z) (next z) (merge-with concat m {(first (keys m)) (list f)}) r)
        ))))

(defcheck solution-86d488cc
  (fn [xs]
    (loop [m {}
           s xs]
      (if (empty? s) m
                     (let [sp  (first s)
                           sp2 (split-with (complement keyword?) (rest s))]
                       (recur (conj m [sp (first sp2)]) (first (rest sp2))))))))

(defcheck solution-870b6252
  (fn [kn]
    (first
      (reduce (fn [[m k] i]
                (let [e (first i)]
                  (cond (nil? k) [m e]
                        (keyword? e) [(assoc m k []) e]
                        :else [(assoc m k (vec i)) nil])))
        [{} nil]
        (partition-by keyword kn)))))

(defcheck solution-872e4457
  (fn [xs]
    (loop [[h & t :as xs] xs
           k    nil
           accu {}]
      (if (seq xs)
        (if (keyword? h)
          (recur t h (assoc accu h []))
          (recur t k (assoc accu k (conj (accu k) h))))
        accu))))

(defcheck solution-87446a7a
  #((reduce (fn [[m n] x] (if (keyword? x) [(assoc m x n) ()] [m (conj n x)])) [{} ()] (reverse %)) 0))

(defcheck solution-875a64d8
  (fn [result a]
    (if (empty? a)
      result
      (let [key (first a)
            [values a2] (split-with (complement keyword?) (rest a))]
        (recur (assoc result key values) a2)))) {})

(defcheck solution-876ea1f2
  (fn __
    [coll]
    (loop [x        coll
           last-key (first coll)
           result   {}]
      (if (empty? x)
        result
        (let [item (first x)]
          (if (keyword? item)
            (recur (rest x) item (assoc result item []))
            (recur (rest x) last-key (assoc result last-key (conj (result last-key) item)))))))))

(defcheck solution-878a9e11
  (fn f [a c [x & y]]
    (if x
      (if (ifn? x)
        (f (assoc a x []) x y)
        (f (update-in a [c] conj x) c y))
      a)) {} 0)

(defcheck solution-87d53cd1
  (fn key-vals [xs]
    (->> (partition-by keyword? xs)
      (partition 2)
      (reduce (fn [m [a b]] (merge m (zipmap (reverse a) (concat [b] (repeat []))))) {}))))

(defcheck solution-87f3d951
  (fn [coll]
    (let [ks (filter (complement nil?) (map-indexed (fn [a b] (when (keyword? b) a)) coll))]
      (loop [iks ks m {}]
        (let [i (first iks)
              j (if (next iks)
                  (second iks)
                  (count coll))]
          (if-not i
            m
            (recur (next iks)
              (assoc m (nth coll i) (subvec coll (inc i) j)))))))))

(defcheck solution-87f7668b
  (fn [x] (apply array-map
            (mapcat #(if (keyword (first %)) (interpose [] %) (list (apply vector %)))
              (partition-by keyword? x)))))

(defcheck solution-887f05a3
  #(loop [m {} k nil c %]
     (let [s (first c) t (rest c)]
       (cond (empty? c) m
             (keyword? s) (recur (assoc m s []) s t)
             :else (recur (assoc m k (conj (m k) s)) k t)))))

(defcheck solution-8893735d
  #((reduce (fn [[a b] x] (if (keyword? x) [(assoc a x b) ()] [a (conj b x)])) [{} ()] (reverse %)) 0))

(defcheck solution-88a379c1
  (fn identify-keys-and-values [l]
    (loop [s (seq l) result {} curkey nil curval []]
      (if (empty? s)
        (if (nil? curkey)
          result
          (assoc result curkey curval))
        (if (keyword? (first s))
          (if (nil? curkey)
            (recur (rest s) result (first s) [])
            (recur (rest s) (assoc result curkey curval) (first s) []))
          (recur (rest s) result curkey (conj curval (first s))))))))

(defcheck solution-88c7901b
  #(loop [m {} c %]
     (let [d (next c) [n r] (split-with number? d)]
       (if d
         (recur (assoc m (first c) n) r)
         m))))

(defcheck solution-88c9c227
  (fn [v]
    (letfn [(make-two-pair [[x & xs] y]
              (if (empty? xs)
                [x y]
                (loop [lst [x []] [x & xs] xs]
                  (if (empty? xs)
                    (conj lst x y)
                    (recur (conj lst x []) xs))))
              )]
      (if (empty? v) {}
                     (->> (partition-by type v)
                       (partition 2)
                       (mapcat #(apply make-two-pair %))
                       (apply assoc {})
                       )))))

(defcheck solution-88fadada
  (fn [coll]
    (apply hash-map
      (mapcat
        #(if (keyword? (first %1))
           (interpose [] %1)
           [%1])
        (partition-by keyword? coll)))))

(defcheck solution-891c5885
  (fn [xs]
    (if (empty? xs) {}
                    (let [separated-list (reduce #(if (keyword? %2)
                                                    (conj %1 [%2])
                                                    (update-in %1 [(dec (count %1))] conj %2))
                                           [[(first xs)]]
                                           (rest xs))]
                      (into {} (for [[x & xs] separated-list]
                                 [x (vec xs)]))))))

(defcheck solution-8bce7ab
  (fn mappack [acoll]
    (if (empty? acoll) {}
                       (let [[key & others] acoll,
                             [val rest] (split-with #(not (keyword? %)) others)]
                         (assoc (mappack rest)
                           key
                           val)))))

(defcheck solution-8c0f3121
  (letfn [(partition-when [f coll]
            (lazy-seq
              (when-let [[fst & s] (seq coll)]
                (let [run (cons fst (take-while (complement f) s))]
                  (cons run (partition-when f (drop (count run) coll)))))))]
    (fn [coll]
      (into {} (map (fn [[k & vs]] [k (vec vs)]) (partition-when keyword? coll))))))

(defcheck solution-8c178c16
  (letfn [
          (partition-on-keyword [s]
            (if (empty? s) '()
                           (let [run (cons (first s) (take-while #(not (keyword? %)) (rest s)))]
                             (cons run (partition-on-keyword (drop (count run) s))))))
          (decode [s] {(first s) (rest s)})
          (make-map [s] (into {} (map decode (partition-on-keyword s))))]
    make-map))

(defcheck solution-8c6468e6
  (letfn
   [(r [[m k v] n]
      (if (keyword? n)
        (if (nil? k)
          [m n []]
          [(assoc m k v) n []])
        [m k (conj v n)]))]
    (fn [s]
      (let [[m k v] (reduce r [{} nil []] s)]
        (if (nil? k) m (assoc m k v))))))

(defcheck solution-8c7573b7
  (fn __
    ([x] (__ x {}))
    ([x res]
     (if (empty? x)
       res
       (let [
             keyw   (first x)
             pars   (take-while (fn [x] (not (keyword? x))) (rest x))
             todrop (inc (count pars))
             newRes (assoc res keyw pars)
             ]
         (__ (drop todrop x) newRes))))))

(defcheck solution-8c829677
  (fn f
    ([c] (f c []))
    ([[a & r] s]
     (if a
       (if (keyword? a)
         (f r (conj s [a []]))
         (let [[k v] (last s)
               w (conj v a)
               i (dec (count s))]
           (f r (assoc s i [k w]))))
       (into {} s)))))

(defcheck solution-8d7a48a3
  (fn [input]
    (loop [[key & others] input
           result {}]
      (if key
        (let [[values next-input]
              (split-with (complement keyword?) others)]
          (recur next-input (assoc result key values)))
        result))))

(defcheck solution-8dd6fe49
  (fn [s]
    (loop [[v & xs] s
           k nil
           m {}]
      (cond (nil? v) m
            (keyword? v) (recur xs v (assoc m v []))
            :else (recur xs k (merge m {k (conj (m k) v)}))))))

(defcheck solution-8e40bba3
  #(loop [s % a {}]
     (if (empty? s) a
                    (let [[[x] y] (split-at 1 s)
                          [z t] (split-with number? y)]
                      (recur t (assoc a x z))))))

(defcheck solution-8e79458e
  (fn gl [rslt [x & lst]]
    (if (empty? lst) rslt
                     (let [[a b] (split-with (complement keyword?) lst)]
                       (recur (assoc rslt x a) b)
                       ))) {})

(defcheck solution-8f240b9c
  (fn f [[k & v]]
    (if v
      (let [[a b] (split-with number? v)]
        (assoc (f b) k a))
      {})))

(defcheck solution-8f2bfaa8
  (fn [xs]
    (let [
          impl (fn [xs acc]
                 (if-not (seq xs)
                   acc
                   (let [k (first xs)
                         [vs, tail] (split-with (comp not keyword?) (rest xs))]
                     (recur tail (assoc! acc k vs)))))
          ]
      (->> {}
        (transient)
        (impl xs)
        (persistent!)))))

(defcheck solution-8fa1267e
  (fn mapper [coll]
    (if-let [s (seq coll)]
      (let [fst (first s)
            rst (next s)
            run {fst (into [] (take-while #(not (keyword? %)) rst))}]
        ;run
        (merge run (mapper (seq (drop (+ 1 (count (get run (first (keys run))))) s)))))
      {})))

(defcheck solution-8faa84a8
  (fn [xs]
    (let [r (partition-by keyword? xs)]
      (merge
        (zipmap
          (map last (take-nth 2 r))
          (take-nth 2 (rest r)))
        (zipmap
          (flatten (map drop-last (take-nth 2 r)))
          (repeat []))))))

(defcheck solution-8facabef
  (fn [coll]
    (loop [coll coll result {} lastKeyword nil]
      (cond
        (empty? coll) result
        (keyword? (first coll)) (recur (rest coll) (assoc result (first coll) []) (first coll))
        :else (recur (rest coll) (update-in result [lastKeyword] conj (first coll)) lastKeyword)))))

(defcheck solution-8fe4437a
  (fn [s]
    (letfn [(f [[x & xs]]
              (when x
                (let [[t d] (split-with number? xs)]
                  (cons [x t] (lazy-seq (f d))))))]
      (into {} (f s)))))

(defcheck solution-902fb13a
  (fn go [coll]
    (->> (reductions #(if (keyword? %2) %2 %1) nil coll)
      rest
      (map (fn [v k] {k v}) coll)
      (filter (comp not keyword? first vals))
      (reduce (partial merge-with conj)
        (zipmap (filter keyword? coll) (repeat []))))))

(defcheck solution-90572024
  (fn [coll]
    (loop [coll coll k nil m {}]
      (let [h (first coll) t (rest coll)]
        (cond
          (empty? coll) m
          (keyword? h) (recur t h (assoc m h []))
          :else (recur t k (merge-with concat m {k [h]})))))))

(defcheck solution-9066cfa7
  (fn [coll]
    (loop [p coll k [] v [] m {}]
      (cond
        (empty? p) (dissoc (assoc m k v) [])
        (keyword? (first p)) (recur (rest p) (first p) [] (assoc m k v))
        :else (recur (rest p) k (conj v (first p)) m)))))

(defcheck solution-906d0d65
  (fn [in]
    (first
      (reduce
        (fn [[res kw] val]
          (if (keyword? val)
            [(assoc res val []) val]
            [(merge-with conj res {kw val}) kw]))
        [{} nil]
        in))))

(defcheck solution-907fc690
  (fn idf [v]
    (let [[a b] (split-with number? (rest v))]
      (if (not (empty? v))
        (assoc (idf b) (first v) a) {}))))

(defcheck solution-9094f522
  (fn iden [c]
    (if (empty? c)
      {}
      (into {(first c) (vec (take-while number? (rest c)))}
        (iden (drop-while number? (rest c)))))))

(defcheck solution-90bc77f6
  (fn [v]
    (if (= v []) {}
                 (loop [h [] i 0]
                   (if (= i (count v))
                     (->> (partition 2 h)
                       (map #(vec %))
                       (into (sorted-map)))
                     (recur
                       (if (keyword? (v i)) (conj h (v i) [])
                                            (conj (vec (butlast h)) (conj (last h) (v i)))
                                            )
                       (inc i)))))))

(defcheck solution-90f1afc1
  (fn p105 [x]
    (loop [r {} v [] k (first x) l (rest x)]
      (if (empty? l)
        (if-not (nil? k)
          (assoc r k v)
          r)
        (if (keyword? (first l))
          (recur (assoc r k v) [] (first l) (rest l))
          (recur r (conj v (first l)) k (rest l)))))))

(defcheck solution-911a4d58
  (fn [c]
    (loop [[f & r] c, kvm {}]
      (if (nil? f)
        kvm
        (let [[vs l] (split-with (complement keyword?) r)]
          (recur l (assoc kvm f vs)))))))

(defcheck solution-912ae97c
  (fn [s]
    (let
     [dumb (gensym)
      [k vs dict]
      (reduce
        (fn [[k vs dict] e]
          (if (keyword? e)
            [e [] (assoc dict k vs)]
            [k (conj vs e) dict]))
        [dumb [] {}]
        s)
      ]
      (dissoc (assoc dict k vs) dumb)
      )
    ))

(defcheck solution-914ff278
  (fn [r]
    (loop [acc {} r r]
      (if (empty? r) acc
                     (let [[v rs] (split-with number? (rest r))]
                       (recur (assoc acc (first r) (vec v)) rs))))))

(defcheck solution-9222a06f
  (fn p105
    [coll]
    (if (seq coll)
      (if (keyword? (first coll))
        (assoc (p105 (rest coll)) (first coll) (take-while number? (rest coll)))
        (p105 (rest coll)))
      {})))

(defcheck solution-9237c7c6
  #(loop [x % k nil m {}]
     (if (empty? x)
       m
       (let [n (first x)]
         (if (keyword? n)
           (recur (rest x) n (merge m {n []}))
           (recur (rest x) k (merge-with conj m {k n})))))))

(defcheck solution-9246d3cc
  (fn p105 [s]
    (if (empty? s) {}
                   (letfn [(f [m k r]
                             (if (empty? r) (assoc m k (concat [] (get m k)))
                                            (let [x (first r)]
                                              (if (keyword? x)
                                                (f (assoc m x []) x (rest r))
                                                (f (assoc m k (into [] (conj (get m k) x))) k (rest r))))))]
                     (f {} (first s) (rest s))))))

(defcheck solution-92cb83dc
  (fn [s]
    (loop [s s r {} k :o]
      (let [v (first s) z (rest s)]
        (cond (empty? s) r
              (keyword? v) (recur z (assoc r v []) v)
              :else (recur z (merge-with conj r {k v}) k))))))

(defcheck solution-92fff1e3
  (fn [coll]
    (loop [acc     {}
           last-kw nil
           left    coll]
      (if (empty? left)
        acc
        (let [x (first left)]
          (if (keyword? x)
            (recur (assoc acc x []) x (rest left))
            (recur (update-in acc [last-kw] conj x) last-kw (rest left))))))))

(defcheck solution-930c47ea
  (fn [c]
    (->> c
      (partition-by keyword?)
      (partition 2)
      (reduce (fn [m [ks vs]]
                (assoc (reduce #(assoc % %2 []) m (butlast ks))
                  (last ks) vs))
        {}))))

(defcheck solution-93573aa5
  (fn [coll]
    (loop [result {} k nil items coll]
      (cond
        (empty? items) result
        (keyword? (first items)) (recur (assoc result (first items) []) (first items) (rest items))
        :else
        (recur (assoc result k (conj (get result k) (first items))) k (rest items))
        )
      )
    ))

(defcheck solution-935925f6
  (fn [xs]
    (into {}
      (loop [acc [], xs xs]
        (if (seq xs)
          (let [k    (first xs)
                xs   (rest xs)
                vs   (->> xs
                       (take-while (complement keyword?))
                       vec)
                xs'  (drop-while (complement keyword?) xs)
                acc' (conj acc [k vs])]
            (recur acc' xs'))
          acc)))))

(defcheck solution-93d98251
  (fn [xs]
    (loop [acc {} k nil v [] xs xs]
      (cond
        (not (seq xs)) (if k (conj acc [k v]) acc)
        (keyword? (first xs)) (recur (if k (conj acc [k v]) {}) (first xs) [] (rest xs))
        :else (recur acc k (conj v (first xs)) (rest xs))))))

(defcheck solution-9428c1d9
  (fn key-group [v]
    (first (reduce (fn [[hmap prev-keyword :as state] new-el]
                     (if (keyword? new-el)
                       (assoc
                        (assoc state
                          0
                          (assoc hmap new-el []))
                         1
                         new-el)
                       (assoc state
                         0
                         (assoc hmap prev-keyword (conj (hmap prev-keyword) new-el)))))
             [{} nil]
             v))))

(defcheck solution-946b0720
  #(loop [v % k nil m {}]
     (if (empty? v)
       m
       (if (keyword? (first v))
         (recur (next v) (first v) (assoc m (first v) []))
         (recur (next v) k (merge-with conj m {k (first v)}))
         )
       )
     ))

(defcheck solution-95078e76
  (fn [coll] (loop [coll coll
                    m    {}]
               (if (empty? coll)
                 m
                 (let [k (first coll)
                       [v new-coll] (split-with #(not (keyword? %)) (rest coll))]
                   (recur
                     new-coll
                     (assoc m k v)))))))

(defcheck solution-95451f8d
  (fn f [l]
    (if (not (nil? l))
      (let [l (seq l)
            r (take-while #(number? %) (next l))
            a (first l)]
        (if (keyword? a)
          (conj {a (vec r)} (f (next l)))
          (conj {} (f (next l)))))
      {})))

(defcheck solution-95c5deaf
  #(loop [r {}
          [k & q :as x] %]
     (if (empty? x)
       r
       (recur (assoc r k (vec (take-while integer? q)))
         (drop-while integer? q)))))

(defcheck solution-95c878f7
  (fn ikv [v]
    (apply hash-map (map
                      #(if (keyword? (first %)) (first %) (flatten %))
                      (partition-by keyword?
                        (reduce #(if
                                  (and (keyword? (last %)) (keyword? %2))
                                   (conj % () %2)
                                   (conj % %2))
                          [] v))))))

(defcheck solution-95f5c3b7
  (fn r [a c [x & y]]
    (if x
      (if (keyword? x)
        (r (assoc a x []) x y)
        (r (update-in a [c] conj x) c y))
      a)) {} =)

(defcheck solution-968a0062
  (fn f [s]
    (if (empty? s)
      {}
      (conj (f (drop-while (complement keyword?) (rest s)))
        [(first s) (take-while (complement keyword?) (rest s))]))))

(defcheck solution-96917bd5
  (fn [args]
    (loop [m {}, a args]
      (if-let [k (first a)]
        (let [[vs more] (split-with (complement keyword?) (next a))]
          (recur (assoc m k vs) more))
        m))))

(defcheck solution-96ca9a78
  (fn f [c]
    (if (empty? c)
      {}
      (let [[h & t] c
            [v r] (split-with number? t)]
        (merge {h v} (f r))))))

(defcheck solution-96cfda8a
  (fn kvs
    ([c] (kvs c {} nil))
    ([c acc currentKey]
     (let [e (first c) r (rest c)]
       (cond
         (empty? c) acc
         (keyword? e) (kvs r (assoc acc e []) e)
         true (kvs r (assoc acc currentKey (conj (acc currentKey) e)) currentKey)
         )
       ))))

(defcheck solution-96e54f5d
  (fn [coll]
    (apply hash-map
      (reduce #(if (keyword? %2)
                 (conj % %2 [])
                 (conj (vec (butlast %)) (conj (last %) %2)))
        []
        coll))))

(defcheck solution-970b4acc
  (fn f [[h & t]]
    (if h
      (conj
        {h (take-while (complement keyword?) t)}
        (f (drop-while (complement keyword?) t)))
      {})))

(defcheck solution-973acf5a
  (fn id-keys-vals [items]
    (let [splits (partition-by type items)
          kws    (take-nth 2 splits)
          vls    (take-nth 2 (rest splits))
          kwmap  (into {} (for [k (flatten kws)]
                            {k []}))
          result (into kwmap (apply hash-map (interleave (map last kws) vls)))
          ]
      result
      )
    ))

(defcheck solution-979da42
  (fn [s] (apply hash-map (mapcat #(if (keyword? (first %)) (interpose [] %) [%])
                            (partition-by type s)))))

(defcheck solution-980c60d4
  (fn mapify [c]
    (loop [coll c
           res  {}]
      (if (empty? coll) res
                        (recur
                          (drop-while number? (rest coll))
                          (conj res [(first coll) (take-while number? (rest coll))]))))))

(defcheck solution-991e6503
  (fn [coll]
    (apply hash-map
      (reduce (fn [r e]
                (if (keyword? e)
                  (conj r e [])
                  (conj (vec (drop-last r)) (conj (last r) e))))
        []
        coll))))

(defcheck solution-99ccfc5b
  (fn f [m [k & r]] (let [[v l] (split-with #(not (keyword? %)) r)] (if (seq r) (f (assoc m k v) l) m))) {})

(defcheck solution-99fe3dce
  (fn [s]
    (reduce (fn [r [k v]]
              (assoc (into r (zipmap k (repeat []))) (last k) v)) {}
      (partition 2 (partition-by keyword? s)))))

(defcheck solution-9a573f4a
  (fn identify-key-value
    [coll]
    (let [key-val (partition-by keyword? coll) parts (partition 2 key-val)]
      (into {} (concat (map vector (map last (map first parts)) (map second parts))
                 (map vector (mapcat butlast (map first parts)) (repeat [])))))))

(defcheck solution-9aab242c
  (fn myseq [al]
    (loop [l al res {} prevk nil]
      (if (empty? l) res
                     (if (keyword? (first l))
                       (recur (rest l) (assoc res (first l) []) (first l))
                       (recur (rest l) (assoc res prevk (conj (get res prevk) (first l))) prevk))))))

(defcheck solution-9af32fa3
  (fn [s] (first (reduce (fn [[coll key] new] (if (keyword? new) [(conj coll [new []])
                                                                  new]
                                                                 [(update-in coll [key] #(conj % new)) key]))
                   [{} nil] s))))

(defcheck solution-9b76ec57
  (fn [xs]
    (let [partitioned    (partition-by #(and (keyword? %) (identity %)) xs)
          pairs          (map vector partitioned (rest partitioned))
          relevant-pairs (filter #(->> % first first keyword?) pairs)]
      (into {}
        (for [[k v] relevant-pairs]
          (if (keyword? (first v))
            [(first k) []]
            [(first k) (into [] v)]))))))

(defcheck solution-9b877be8
  (fn [l]
    (loop [acc {} remains l]
      (if (empty? remains)
        acc
        (let [nkw #(not (keyword? %))
              kw  (first remains)
              rst (rest remains)
              kwv (take-while nkw rst)
              rms (drop-while nkw rst)
              ]
          (recur (assoc acc kw kwv) rms)
          )))))

(defcheck solution-9c03bbb3
  (fn [x]
    (into {}
      (
       (fn to-map [[f & r :as all]]
         (if (empty? all)
           '()
           (let [[a b] (split-with #(not (keyword? %)) r)]
             (lazy-seq (cons [f a] (to-map b))))))
       x))))

(defcheck solution-9c0f46e
  #((reduce
      (fn [a b]
        (if
         (keyword? b)
          (assoc a :x b :a (assoc (a :a) b []))
          (update-in a [:a (a :x)] conj b)))
      {:a {}} %)
    :a))

(defcheck solution-9c4e7eb7
  (fn [l]
    (loop [h {}, last-sym nil, left l]
      (if (empty? left)
        h
        (let [curr (first left)
              next-hash
                   (if (number? curr)
                     (assoc h last-sym (conj (h last-sym) curr))
                     (assoc h curr []))]
          (recur next-hash (if (number? curr) last-sym curr) (rest left)))))))

(defcheck solution-9cb92772
  (fn [x]
    (loop [m {} s x]
      (if-let [[s & q] (seq s)]
        (let [[h i] (split-with number? q)]
          (recur (assoc m s h) i))
        m))))

(defcheck solution-9cbe0286
  (fn p [col]
    (let [k     (first col)
          val-s (rest col)]
      (if (not (keyword? k))
        {}
        (apply merge {k (vec (take-while #(not (keyword? %)) val-s))}
          (p (vec (drop-while #(not (keyword? %)) val-s))))))))

(defcheck solution-9cd41bb6
  #(if (empty? %2)
     %
     (let [[v t] (split-with number? (rest %2))]
       (recur (assoc % (first %2) v) t))) {})

(defcheck solution-9dac021c
  (fn keys-vals [c]
    (loop [o {} c c]
      (if-let [k (first c)]
        (recur (conj o
                 [k
                  (vec
                    (take-while number? (rest c)))])
          (drop-while number? (rest c)))
        o))))

(defcheck solution-9daef744
  (fn [s]
    (->> s (partition-by #(if (keyword? %) %))
      (partition 2 1)
      (map (fn [[[k] v]] (if (keyword? k)
                           (if (keyword? (first v))
                             [k []]
                             [k v]))))
      (remove nil?)
      (into {}))))

(defcheck solution-9dc728f1
  (fn q [[f & r]]
    (if f
      (let [[g h] (split-with number? r)]
        (conj (q h) {f g}))
      {})))

(defcheck solution-9dfc3c55
  (fn to-map [coll]
    (let [to-map-iter (fn [[fst & rst :as coll] res]
                        (if (empty? coll)
                          res
                          (recur (drop-while (complement keyword?) rst)
                            (assoc res
                              fst
                              (take-while (complement keyword?) rst)))))]
      (to-map-iter coll {}))))

(defcheck solution-9ebb7f46
  (fn id-kv [coll]
    (loop [acc      {}
           curr-key nil
           coll     coll]
      (let [item (first coll)]
        (cond
          (empty? coll) acc
          (keyword? item) (recur (assoc acc item [])
                            item
                            (rest coll))
          :else (recur (merge-with conj acc {curr-key item})
                  curr-key
                  (rest coll)))))))

(defcheck solution-9f49baff
  (fn [s]
    (->> (mapcat #(if (keyword? %) [% []] [%]) s)
      (partition-by keyword?)
      (map #(if (keyword? (first %)) (first %) (if (> (count %) 1) (apply conj %) [])))
      (apply hash-map)

      )
    ))

(defcheck solution-9f63fd4b
  (fn [x]
    (let [partioned   (partition-by keyword? x)             ;((:a) (1 2 3) (:b :c) (4))
          per-keyword (fn [[first :as all]] (if (keyword? first) (interpose [] all) [all]))
          mapped      (mapcat per-keyword partioned)
          result      (apply hash-map mapped)]              ;{:c (4), :b [], :a (1 2 3)}
      result)))

(defcheck solution-9f8b1df6
  (fn __ [m]
    (let [mm (apply hash-map
               (map (partial remove nil?)
                 (partition-by keyword?
                   (interleave m (repeat nil)))))]
      (zipmap (map first (keys mm)) (vals mm)))))

(defcheck solution-9fd76e94
  (fn key-num-map [key-num-seq]
    (loop [kn-map {}
           kn-seq key-num-seq]
      (if (empty? kn-seq)
        kn-map
        (let [key  (first kn-seq)
              nums (take-while number? (rest kn-seq))]
          (recur (assoc kn-map key (vec nums)) (drop (inc (count nums)) kn-seq)))))))

(defcheck solution-a02e19af
  (fn key-val [coll]
    (reduce (fn [result [k v]]
              (assoc (into result (zipmap k (repeat []))) (last k) v)) {}
      (partition 2 (partition-by keyword? coll)))))

(defcheck solution-a03dc03c
  (fn
    [l]
    (if (= 0 (count l))
      {}
      (let [p (partition-by keyword? l)
            d (partition 2 p)]
        (apply merge
          (flatten
            (map (fn
                   [x]
                   (let [k (reverse (first x))
                         v (second x)]
                     (if (= (count k) 1)
                       (hash-map (first k) v)
                       (zipmap k (concat (list v)
                                   (repeat (- (count k) 1) '()))))))
              d)))))))

(defcheck solution-a0c32c85
  (fn [x] (apply hash-map (reduce (fn [a b] (if (number? b)
                                              (conj (pop a) (conj (last a) b))
                                              (conj a b [])
                                              )) [] x))))

(defcheck solution-a0e37f20
  #(->> %
     reverse
     (reduce (fn [[vs rs] v]
               (if (keyword? v)
                 [[] (conj rs [v vs])]
                 [(conj vs v) rs]))
       [[] {}])
     last
     (map (fn [[k v]] [k (vec (reverse v))]))
     (into {})))

(defcheck solution-a0e44f31
  #(loop [s %
          m {}
          k nil]
     (if (seq s)
       (let [v (first s)
             s (rest s)]
         (if (keyword? v)
           (recur s (assoc m v []) v)
           (recur s (assoc m k (conj (m k) v)) k)))
       m)))

(defcheck solution-a15ddb3
  (fn keys-vals [coll]
    (loop [[e & mas :as es] coll
           result    []
           curr-k    nil
           curr-vals []]
      (letfn [(mores []
                (concat result
                  (when curr-k [curr-k curr-vals])))]
        (cond
          (empty? es)
          (apply hash-map (mores))

          (keyword? e)
          (recur mas (mores) e [])

          :default
          (recur mas result curr-k (conj curr-vals e)))))))

(defcheck solution-a165f306
  (fn [s]
    (apply merge {} (flatten (for [[ks vs] (partition 2 (partition-by keyword? s))]
                               (conj [{(last ks) vs}] (for [k (butlast ks)] {k []})))))))

(defcheck solution-a2dac90f
  (fn [lst]
    (loop [lst lst, k nil, result {}]
      (if (seq lst)
        (let [[head & tail] lst]
          (if (keyword? head)
            (recur tail head (assoc result head []))
            (recur tail k (update-in result [k] conj head))))
        result))))

(defcheck solution-a2e1b602
  (fn [s]
    (dissoc (reduce #(if (keyword? %2) (assoc %1 0 %2 %2 []) (update-in %1 [(%1 0)] conj %2)) {} s) 0)))

(defcheck solution-a2ffb6cd
  #(loop [[k & r] %
          acc {}]
     (if (empty? r)
       (if k
         (assoc acc k [])
         acc)
       (let [split (split-with number? r)]
         (recur (second split)
           (assoc acc k (first split)))))))

(defcheck solution-a36bf73e
  (fn [s]
    (first (reduce (fn [[r k] e]
                     (if (keyword? e)
                       [(assoc r e []) e]
                       [(assoc r k (conj (get r k) e)) k]))
             [{} nil] s))))

(defcheck solution-a40d08a9
  (fn make-map [coll]
    (loop [m {}, xs coll]
      (if (empty? xs)
        m
        (let [[values xss] (split-with (comp not keyword?) (rest xs))]
          (recur (assoc m (first xs) values) xss))))))

(defcheck solution-a43cf7d4
  (letfn [

          (run-length [lst]
            (count (take-while (complement keyword?) lst)))
          ]

    (fn [lst]
      (loop [d {} lst lst]
        (if (empty? lst) d
                         (let [symb (first lst) rst (rest lst) n (run-length rst)]
                           (recur
                             (assoc d symb (take n rst))
                             (drop (inc n) lst))))))))

(defcheck solution-a4544529
  (fn [v]
    (letfn [(p [x] (not (keyword? x)))
            (f [m v]
              (if (empty? v)
                m
                (let [k (first v)
                      [v n] (split-with p (rest v))]
                  (f (conj m {k v}) n))))]
      (f {} v))))

(defcheck solution-a4f9d86b
  (fn [s]
    (reduce #(assoc % (first %2) (second %2)) {}
      (reduce #(if (keyword? %2)
                 (conj % (vector %2 []))
                 (concat (rest %)
                   (conj [] (vector (first (first %))
                              (conj (second (first %)) %2))))) [] s))))

(defcheck solution-a5273226
  (fn keyword-values
    [seq]
    (if (empty? seq) {}
                     (second (reduce (fn [[prev accum] h] (if (keyword? h)
                                                            [h (assoc accum h [])]
                                                            [prev (assoc accum prev (conj (accum prev) h))]))
                               [(first seq) {(first seq) []}] (rest seq))))))

(defcheck solution-a56f1e2a
  #(loop [[f & s] % r {}]
     (cond (nil? s) r
           (keyword? (first s)) (recur s (conj r [f []]))
           :else (recur (drop-while number? s) (conj r [f (vec (take-while number? s))])))))

(defcheck solution-a577a25b
  (fn create-map [xs]
    (#(if (empty? (second %))
        (first %)
        (conj (first %) (hash-map (first (second %)) (last %))))
     (reduce
       (fn [[a b c] n]
         (if (keyword? n)
           (if (empty? b)
             [a [n] c]
             [(conj a (hash-map (first b) c)) [n] []])
           [a b (conj c n)]))
       [{} [] []] xs))))

(defcheck solution-a57a87b3
  #(loop [r {} k nil v %]
     (let [e (first v)]
       (cond
         (not (seq v)) r
         (keyword? e) (if (contains? r e) (recur r e (rest v))
                                          (recur (assoc r e []) e (rest v)))
         :else (recur (assoc r k (conj (r k) e)) k (rest v))))))

(defcheck solution-a5b838de
  (fn [coll]
    (->> (reduce (fn [ret x]
                   (if (number? x)
                     (conj (pop ret) (conj (peek ret) x))
                     (conj ret x [])))
           []
           coll)
      (partition 2)
      (map vec)
      (into {}))))

(defcheck solution-a5d899fa
  (fn [vec]
    (loop [acc {} rem vec]
      (if (empty? rem) acc
                       (recur
                         (assoc acc (first rem) (take-while number? (rest rem)))
                         (drop-while number? (rest rem)))))))

(defcheck solution-a63e5341
  (letfn [(augment [xs acc]
            (if (empty? xs)
              acc
              (let [k  (first xs)
                    vs (take-while (complement keyword?) (rest xs))]
                (recur (drop-while (complement keyword?) (rest xs))
                  (assoc acc k vs)))))]
    (fn [xs]
      (augment xs {}))))

(defcheck solution-a672cc8f
  #(into {}
     (map vec
       (partition 2
         (reduce (fn [a b]
                   (if (= (type b) (type 1))
                     (conj (vec (butlast a)) (conj (last a) b))
                     (concat a [b []])
                     )) [] %)))))

(defcheck solution-a6d3c142
  (fn [coll]
    (loop [[k & xs] coll
           acc {}]
      (if-not k
        acc
        (recur (drop-while number? xs)
          (assoc acc k (take-while number? xs)))))))

(defcheck solution-a70f9aba
  (fn [c]
    (loop [s c, res {}]
      (if (empty? s)
        res
        (let [[h t] (split-with (complement keyword?) (rest s))]
          (recur t (assoc res (first s) h)))))))

(defcheck solution-a792e42c
  (fn [coll]
    (first
      (reduce
        (fn [[m k] e]
          (if (keyword? e)
            [(assoc m e []) e]
            [(update-in m [k] #(conj % e)) k]))
        [{} nil]
        coll))))

(defcheck solution-a7b9d7f6
  (fn k-and-v [s]
    (let [p2kw (fn [s] (partition 2 (partition-by keyword? s)))
          rf   (fn [acc x] (assoc acc (ffirst x) (if (nil? (first (second x))) '() (second x))))]
      (reduce rf {}
        (p2kw (flatten
                (for [pair (p2kw s)] (conj (vec (interpose nil (first pair))) (second pair)))))))))

(defcheck solution-a801631e
  #(let [nk (complement keyword?)]
     (loop [c (drop-while nk %)
            m {}]
       (if-let [c (seq c)]
         (let [[vs x] (split-with nk (next c))]
           (recur x (assoc m (first c) vs)))
         m))))

(defcheck solution-a8397936
  (fn [xs]
    (let [not-key?
                (comp not keyword?)
          rs    (drop-while not-key? xs)
          r-fst (first rs)]
      (if (empty? rs) {}
                      (first (reduce (fn [[m k] b]
                                       (if (not-key? b)
                                         [(assoc m k (conj (m k) b)) k]
                                         (if (contains? m b)
                                           [m b]
                                           [(assoc m b []) b])))
                               [{r-fst []} r-fst]
                               (rest rs)))))))

(defcheck solution-a8903847
  #((reduce (fn [[a k] e]
              (if (number? e)
                [(assoc a k (conj (a k) e)) k]
                [(assoc a e []) e]))
      [{}]
      %) 0))

(defcheck solution-a8a4f5b
  (fn [xs] (loop [[k & ss] xs acc {}]
             (if k
               (let [[v l] (split-with #(number? %) ss)] (recur l (assoc acc k v)))
               acc))))

(defcheck solution-a91fc8df
  (fn [src]
    (loop [result {} src src]
      (if (not (seq src))
        result
        (let [sym  (first src)
              nums (take-while (comp not keyword?) (rest src))
              left (drop-while (comp not keyword?) (rest src))]
          (recur (conj result {sym nums}) left))))))

(defcheck solution-a94e22f4
  #(loop [[h & r] % m {} k nil]
     (if h
       (if (keyword? h)
         (recur r (merge m {h []}) h)
         (recur r (merge m {k (conj (k m) h)}) k))
       m)))

(defcheck solution-a97a2b1d
  #(->> %
     (reduce (fn [[m k] x]
               (if (keyword? x)
                 [(assoc m x []) x]
                 [(update-in m [k] conj x) k]))
       [{} nil])
     first))

(defcheck solution-a9883971
  (fn [a]
    (loop [kw  nil
           acc {}
           x   a]
      (if (and x (first x))
        (if (keyword? (first x))
          (recur (first x) (assoc acc (first x) []) (next x))
          (recur kw (assoc acc kw (conj (get acc kw) (first x))) (next x)))
        acc))))

(defcheck solution-aa29c799
  (fn [v]
    (loop [r {}, a v, k nil]
      (if (empty? a) r
                     (if (keyword? (first a))
                       (recur (assoc r (first a) []) (next a) (first a))
                       (recur (assoc r k (conj (r k) (first a))) (next a) k))))))

(defcheck solution-aa65df1b
  (fn keyval
    [xs]
    (letfn [(f [[k & vs]] {k (if vs vs '())})
            (partition-at
              [pred coll]
              (when (seq coll)
                (lazy-seq
                  (let [fst (first coll)
                        [run remaining] (split-with (complement pred) (rest coll))]
                    (cons (cons fst run) (partition-at pred remaining))))))
            ] (into {} (map f (partition-at keyword? xs))))))

(defcheck solution-ab8fea38
  (fn [d]
    (if (empty? d)
      {}
      (apply assoc {} (reduce
                        (fn [res it]
                          (cond (and (keyword? (first it)) (> (count it) 1))
                                (apply conj res (interpose [] it))
                                (keyword? (first it))
                                (conj res (first it))
                                :else
                                (conj res it)))
                        []
                        (partition-by keyword? d))))))

(defcheck solution-abbf6536
  #(loop [m {}, [k & r :as coll] %] (if (empty? coll) m (let [[v r] (split-with number? r)] (recur (conj m [k v]) r)))))

(defcheck solution-abc48496
  (fn [coll]
    (loop [[head & more] coll last nil res {}]
      (cond
        (nil? head)
        res

        (keyword? head)
        (recur more head (assoc res head []))

        :else
        (recur more last (assoc res last
                                    (conj (get res last) head)))))))

(defcheck solution-ac009a15
  (fn f [xs]
    (if-not (seq xs) {}
                     (loop [m        {(first xs) []}
                            last-key (first xs)
                            xs       (rest xs)]
                       (if-not (seq xs)
                         m
                         (let [x (first xs)]
                           (recur (if (keyword? x)
                                    (assoc m x [])
                                    (assoc m last-key (conj (m last-key) x)))
                             (if (keyword? x) x last-key)
                             (rest xs))))))))

(defcheck solution-ac31c512
  (fn [v]
    (loop [m {} k nil [f & r] v]
      (cond (nil? f) m
            (keyword? f) (recur (assoc m f []) f r)
            :else (recur (assoc m k (conj (m k) f)) k r)))))

(defcheck solution-ac8f1f00
  (fn [l]
    (if (empty? l)
      {}
      (loop [m (rest l)
             k (first l)                                    ; must be keyword
             v []
             r {}]
        (if (empty? m)
          (conj r [k v])
          (if (keyword? (first m))
            (recur (rest m) (first m) [] (conj r [k v]))
            (recur (rest m) k (conj v (first m)) r)))))))

(defcheck solution-aca26080
  (fn keyword-set [xs]
    (->> (reduce (fn [acc item]
                   (if (keyword? item)
                     (conj acc item [])
                     (assoc acc (dec (count acc)) (conj (peek acc) item))))
           []
           xs)
      (apply hash-map))))

(defcheck solution-aca7878f
  (fn keys-and-values
    [xs]
    (into {} (mapcat
               (fn [[x y]]
                 (zipmap x
                   (concat (repeat (dec (count x)) []) (list y))))
               (partition-all 2 (partition-by keyword? xs))))))

(defcheck solution-acecb210
  (fn idkv [coll]
    (loop [coll   coll,
           curkey (first coll),
           curval [],
           result {}]
      (if (empty? coll)
        result
        (if (keyword? (first coll))
          (recur (rest coll) (first coll) (empty curval) (assoc result (first coll) []))
          (recur (rest coll) curkey (conj curval (first coll)) (assoc result curkey (conj curval (first coll)))))))))

(defcheck solution-acf64e84
  (fn create-map [coll]
    (apply hash-map
      (map #(if (number? (first %))
              (vec %)
              (if (seq? (first %))
                (vec (first %))
                (first %)))
        (partition-by keyword?
          (mapcat identity
            (map #(if (and (not= 1 (count %)) (keyword? (first %)))
                    (rest (interleave (repeat '()) %))
                    %)
              (partition-by keyword? coll))))))))

(defcheck solution-ad39941c
  (fn [input]
    (letfn [(kv-iter [coll in]
              (if (empty? in) coll
                              (let [key    (first in)
                                    not-kw (complement keyword?)
                                    rst-in (rest in)
                                    values (take-while not-kw rst-in)
                                    rst    (drop-while not-kw rst-in)]
                                (recur (assoc coll key values) rst))))]
      (kv-iter {} input))))

(defcheck solution-ade7d68c
  (fn [xs]
    (loop [xs xs
           ys []
           k  nil
           m  {}]
      (cond
        (empty? xs) (if k (conj m [k ys]) m)
        (nil? k) (recur (rest xs) ys (first xs) m)
        (keyword? (first xs)) (recur (rest xs) [] (first xs) (conj m [k ys]))
        :else (recur (rest xs) (conj ys (first xs)) k m)))))

(defcheck solution-ae67e5cf
  (fn [r k [h & t]]
    (if h
      (if (keyword? h)
        (recur (assoc r h []) h t)
        (recur (assoc r k (conj (r k) h)) k t))
      r)) {} 0)

(defcheck solution-ae79055b
  #(->> %
     (partition-by type)
     (partition 2)
     (map (fn [[k v]]
            (if (> (count k) 1)
              (let [[a b] k]
                [[a []]
                 [b v]])
              [(vec (concat k [v]))])))
     (apply concat)
     (into {})))

(defcheck solution-aeaff3cc
  (fn this [s]
    (if (seq s)
      (let [[ns r] (split-with number? (rest s))]
        (into {(first s) ns} (this r)))
      {})))

(defcheck solution-aed2b965
  (fn [s]
    (loop [res {} [f & rst] s kw nil]
      (if f
        (if (keyword? f) (recur (assoc res f []) rst f)
                         (recur (update-in res [kw] conj f) rst kw)
                         )

        res
        )

      )
    ))

(defcheck solution-af617f99
  (fn to-hs [lst]
    (let [out (reduce #(assoc %1 %2 []) {} (map #(first %) (filter #(every? identity (map keyword? %)) (partition 2 1 lst))))]
      (reduce #(assoc %1 (last (first %2)) (into [] (last %2)))
        out (partition 2 (partition-by keyword? lst)))
      )
    ))

(defcheck solution-af7d529d
  (fn ekv [c]
    (if (empty? c) {}
                   (let [k (first c)
                         [v r] (split-with (complement keyword?) (rest c))]
                     (conj (ekv r) {k v})))))

(defcheck solution-af9f3bb8
  (fn [coll]
    (let [l (reduce (fn [m x]
                      (if (keyword? x)
                        (concat m [x []])
                        (concat (drop-last m) (vector (conj (last m) x))))) [] coll)
          s (apply array-map l)]
      s)))

(defcheck solution-aff69752
  #(second (reduce (fn [[ck m] i] (if (keyword? i) [i (assoc m i [])] [ck (merge-with conj m {ck i})])) [nil {}] %)))

(defcheck solution-b039dc61
  (fn [coll]
    (if (empty? coll)
      {}
      (loop [out {}
             x   coll]
        (if (empty? x)
          out
          (recur (conj out (hash-map (first x) (into [] (take-while number? (rest x)))))
            (drop (+ 1 (count (take-while number? (rest x)))) x)))))))

(defcheck solution-b0482a1
  #(loop [l      %
          c      nil
          result {}]
     (if (empty? l)
       result
       (recur (rest l)
         (if (keyword? (first l))
           (first l) c)
         (if (keyword? (first l))
           (assoc result (first l) [])
           (assoc result c (conj (result c) (first l))))
         ))
     ))

(defcheck solution-b0c2dc72
  (fn ! [in]
    (if-let [[k & r] (seq in)]
      (let [[v n] (split-with number? r)] (assoc (! n) k v))
      {})))

(defcheck solution-b10fe3b3
  (fn [xs]
    (loop [xs xs m {}]
      (if (empty? xs) m
                      (let [k (first xs)
                            [v more] (split-with number? (rest xs))]
                        (recur more (assoc m k v)))))))

(defcheck solution-b13c8c0f
  #(loop [[h & t] %
          kw  nil
          vs  []
          res {}]
     (let [nr (if kw (conj res [kw vs]) res)]
       (cond
         (nil? h) nr
         (keyword? h) (recur t h [] nr)
         :else (recur t kw (conj vs h) res)))))

(defcheck solution-b14d42af
  (fn [coll]
    (loop [coll coll
           key  nil
           m    {}]
      (let [val (first coll)]
        (cond (empty? coll) m
              (keyword? val) (recur (next coll) val (assoc m val []))
              :else (recur (next coll) key (assoc m key (conj (get m key) val))))))))

(defcheck solution-b1dc0ea5
  (fn [i] (let [a (partition-by #(if (keyword? %) (rand)) i)
                b (rest a)]
            (into {}
              (filter (fn [[k v]] (keyword? k))
                (map (fn [[c] d] [c (if (number? (first d)) d [])]) a b)))
            )))

(defcheck solution-b21f0cfa
  (fn kv [s]
    (loop [c {} k nil [s1 & r] s]
      (cond
        (nil? s1) c
        (keyword? s1) (recur (assoc c s1 []) s1 r)
        :else (recur (assoc c k (conj (c k) s1)) k r)))))

(defcheck solution-b2365be1
  (fn [v]
    (into {} (map (fn [[x y]]
                    (vec [(first x) (flatten y)])
                    ) (partition 2 (partition-by keyword? (interpose [] v)))))
    ))

(defcheck solution-b297b513
  (fn kv [s] (if (empty? s) {} (conj (kv (drop-while #(number? %) (rest s))) {(first s) (take-while #(number? %) (rest s))}))))

(defcheck solution-b2a70971
  (fn [collection]
    (loop [rest-of-collection collection
           result             {}
           last-key           nil]
      (if (empty? rest-of-collection)
        result
        (let [item            (first rest-of-collection)
              is-new-keyword? (keyword? item)]
          (if is-new-keyword?
            (recur (rest rest-of-collection) (assoc result item []) item)
            (recur (rest rest-of-collection) (assoc result last-key (conj (get result last-key) item)) last-key))))
      )))

(defcheck solution-b2b5c927
  #(loop [r {} c %5]
     (if (empty? c)
       r
       (recur (into r (hash-map (% c)
                        (% (%3 %4 (%2 c)))))
         (second (%3 %4 (%2 c)))))) first rest split-with integer?)

(defcheck solution-b41366ca
  (fn [c] (into {} (reduce #(if (keyword? %2) (conj %1 [%2 []]) (let [l (last %1)] (assoc %1 (dec (count %1)) [(first l) (conj (second l) %2)]))) [] c))))

(defcheck solution-b5393d71
  (fn [ms]
    (letfn [(vec-split [seq]
              (if (empty? seq)
                []
                (let [f (first seq)
                      [vs rs] (split-with (complement keyword?) (rest seq))]
                  (cons [f vs]
                    (vec-split rs)))))]
      (into {} (vec-split ms)))))

(defcheck solution-b586a5a
  (fn map-from-flat-key-values
    [coll]
    (reduce
      (fn this [m [ks vs]]
        (if (next ks)
          (this (assoc m (first ks) []) [(rest ks) vs])
          (assoc m (first ks) vs)))
      {}
      (partition 2 (partition-by keyword? coll)))))

(defcheck solution-b642ec30
  (fn f [[h & s]]
    (if h
      (conj (f (drop-while number? s))
        [h (take-while number? s)])
      {})))

(defcheck solution-b6998c7b
  (fn [coll]
    (letfn [(pair-up [[x1 x2 & xs]]
              (when-not (nil? x1) nil
                                  (if (coll? x2)
                                    (conj (pair-up xs) [x1 x2])
                                    (conj (pair-up (conj xs x2)) [x1 []]))))]
      (->> (partition-by #(if (keyword? %) % (type %)) coll)
        (map #(if (keyword? (first %)) (first %) (apply vector %)))
        pair-up
        (into {})))))

(defcheck solution-b6ae7363
  (fn f [l]
    (let [l (drop-while (comp not keyword?) l)]
      (if (empty? l)
        {}
        (merge {(first l) (take-while (comp not keyword?) (next l))} (f (next l)))))))

(defcheck solution-b730999a
  (fn [xs]
    (loop [xs xs, res {}]
      (if (empty? xs) res
                      (let [[k & xs] xs
                            [vs xs] (split-with number? xs)]
                        (recur xs (assoc res k vs)))))))

(defcheck solution-b7469fc8
  (fn keyword-map [[head & tail :as coll]]
    (if (seq coll)
      (let [[numbers remaining] (split-with (complement keyword?)
                                  tail)]
        (merge {head numbers} (keyword-map remaining)))
      {})))

(defcheck solution-b7505e68
  (fn [xs]
    (loop [[x & xs'] xs, acc {}, current nil]
      (cond (nil? x) acc
            (keyword? x) (recur xs' (assoc acc x []) x)
            :else (recur xs' (assoc acc current (conj (acc current) x)) current)))))

(defcheck solution-b7564a3d
  (fn [s]
    (loop [s s cur {} k nil]
      (cond
        (empty? s) cur
        (keyword? (first s)) (recur (rest s) (assoc cur (first s) []) (first s))
        :else (recur (rest s) (assoc cur k (conj (get cur k) (first s))) k)))))

(defcheck solution-b76f2637
  (fn [coll]
    (let [pairs (partition 2 (partition-by keyword? coll))]
      (if (empty? pairs)
        {}
        (apply merge
          (map (fn [[k v]]
                 (let [last-pairs    [(last k) v]
                       butlast-pairs (for [k (butlast k)] [k []])]
                   (into {} (conj butlast-pairs last-pairs))))
            pairs))))))

(defcheck solution-b77a9ea7
  (fn [s]
    (let [groups (fn groups [ss]
                   (lazy-seq
                     (when-let [s (seq ss)]
                       (cons (first s)
                         (cons (take-while number? (rest s))
                           (groups
                             (drop-while number? (rest s))))))))
          ]
      (apply hash-map (groups s)))))

(defcheck solution-b783c804
  (fn identify [kvs]
    (->> (partition-by keyword? kvs)
      (partition-all 2)
      (mapcat (fn [[kws vs]] (concat (interpose () kws) [(sequence vs)])))
      (apply hash-map)
      )))

(defcheck solution-b85026a5
  #(apply merge {} (
                    (fn g [[h & t]]
                      (if h
                        (let [v (take-while number? t)]
                          (lazy-seq (cons {h v} (g (drop (count v) t)))))))
                    %)))

(defcheck solution-b8aff74a
  (fn mapseqs [s] (if (empty? s) {} (apply merge (map #(hash-map (first %) (vec (rest %))) (reduce #(if (keyword? %2) (conj % (vector %2)) (if (> (count %) 1) (concat (butlast %) (vector (conj (last %) %2))) (vector (conj (last %) %2)))) (cons [] s)))))))

(defcheck solution-b922af2f
  #(->> (partition-by keyword? %)
     (partition 2)
     (reduce (fn [agg [k v]]
               (-> (zipmap (reverse k) (cons v (repeat ())))
                 (into agg))) {})))

(defcheck solution-b984ffc0
  (fn idkv [args]
    (let [kvs (partition-by #(if (keyword? %) % 1) args)]
      (loop [[k v & kvs] kvs result {}]
        (if (nil? k)
          result
          (cond (nil? v) (merge result [(first k) []])
                (keyword? (first v)) (recur (cons v kvs) (merge result [(first k) []]))
                :else (recur kvs (merge result [(first k) (vec v)]))))))))

(defcheck solution-b9cbfbbe
  (fn [x]
    (loop [src x result {} temp nil]
      (cond (empty? src) (conj result temp)
            (keyword? (first src)) (recur (rest src) (conj result temp) [(first src) []])
            :else (recur (rest src) result [(first temp) (conj (last temp) (first src))])))))

(defcheck solution-b9da5055
  (fn [coll]
    (if (nil? (seq coll))
      {}
      (loop [current-keyword (first coll)
             coll            (rest coll)
             current-map     {current-keyword []}]
        (cond (nil? (seq coll)) current-map
              (keyword? (first coll)) (recur (first coll)
                                        (rest coll)
                                        (assoc current-map (first coll) []))
              :else (recur current-keyword
                      (rest coll)
                      (assoc current-map
                        current-keyword
                        (conj (current-map current-keyword) (first coll)))))))))

(defcheck solution-ba07157d
  #(apply hash-map (reduce (fn [s i] (if (keyword? i) (conj s i []) (let [p (peek s)] (conj (pop s) (conj p i))))) [] %)))

(defcheck solution-ba2dbb39
  (fn [s]
    (:map
     (reduce
       (fn [m k]
         (if (not (keyword? k))
           (update-in m [:map (:current m)] conj k)
           {:current k, :map (assoc (:map m) k [])}))
       {:current nil :map {}}
       s))))

(defcheck solution-ba89fa5a
  (fn [coll]
    (letfn [(partition-until [f coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (let [[fst] s
                        run (cons fst (take-while (complement f) (next s)))]
                    (cons run (partition-until f (seq (drop (count run) s))))))))]
      (or
       (apply merge
         (map (fn [[k & vs]]
                (hash-map k (vec vs))) (partition-until keyword? coll)))
       {}))))

(defcheck solution-ba94196e
  (fn [s]
    (loop [s (seq s) r {} k nil]
      #_(println "s: " s "r: " r "k: " k)
      (if s
        (if (keyword? (first s))
          (recur (next s) (assoc r (first s) []) (first s))
          (recur (next s) (assoc r k (conj (k r) (first s))) k))
        r))))

(defcheck solution-bafba27c
  (fn [s]
    (zipmap (filter keyword? s)
      ((fn [t r]
         (let [[a b] (split-with #(not (keyword? %)) (rest t))]
           (if (= t []) r (recur b (conj r a)))))
       s []))))

(defcheck solution-bb43aa68
  (fn [s]
    (letfn [(add-val [m k v] (update-in m [k] conj v))]
      (first (reduce (fn [[m k] v] (if (keyword? v) [m v] [(add-val m k v) k]))
               [(zipmap (filter keyword? s) (repeat [])) (first s)]
               (rest s))))))

(defcheck solution-bb5974e0
  (fn [xs]
    (into {}
      (reduce (fn [acc x]
                (if (keyword? x)
                  (cons [x []] acc)
                  (let [[[x1 xs1] & rest] acc]
                    (cons [x1 (conj xs1 x)] rest))))
        [] xs))))

(defcheck solution-bb7b6c86
  (fn [coll]
    (loop [k nil res {} coll coll]
      (if-let [[x & more] (seq coll)]
        (if (keyword? x)
          (recur x (assoc res x []) more)
          (recur k (assoc res k (conj (res k) x)) more))
        res))))

(defcheck solution-bc3f19bd
  (fn [s]
    (loop [m {} s s]
      (if-not (empty? s)
        (let [k     (first s)
              vs    (take-while (comp not keyword?) (rest s))
              new-s (drop-while (comp not keyword?) (rest s))]
          (recur (assoc m k vs) new-s))
        m))))

(defcheck solution-bc4621de
  (fn [s]
    (if (empty? s)
      {}
      (let [a (apply merge (map #(hash-map %1 []) (filter keyword? s)))]
        (loop [res a data s currKey nil]
          (if (empty? data)
            res
            (if (keyword? (first data))
              (recur res (rest data) (first data))
              (recur (update-in res [currKey] conj (first data)) (rest data) currKey))))))))

(defcheck solution-bc7de54
  (fn p-105 [s]
    (loop [result {}
           [current & other] s
           key    nil]
      (if (nil? current)
        result
        (if (keyword? current)
          (recur (assoc result current []) other current)
          (recur (update-in result [key] conj current)
            other key))))))

(defcheck solution-bcc7bc06
  (fn [coll]
    (loop [coll coll
           m    {}]
      (if (empty? coll) m
                        (let [k (first coll)
                              v (take-while number? (next coll))]
                          (recur (drop-while number? (next coll))
                            (assoc m k (vec v))))))))

(defcheck solution-bce32367
  (fn build-map [kvs]
    (let [red-func
          (fn [[mp lst] nxt]
            (cond
              (and (keyword? lst) (keyword? nxt)) [(assoc mp lst []) nxt]
              (keyword? lst) [(assoc mp lst nxt) nxt]
              :else [mp nxt]))
          grouped-vals
          (mapcat #(if (keyword? (first %)) % [%])
            (partition-by keyword? kvs))]
      (first (reduce red-func [{} nil] grouped-vals)))))

(defcheck solution-bd07978b
  (fn r [a c [x & y]]
    (if x
      (if (ifn? x)
        (r (assoc a x []) x y)
        (r (update-in a [c] conj x) c y))
      a)) {} =)

(defcheck solution-bd32ddf2
  #(loop [[head :as coll] %
          curr-key nil
          result   {}]
     (if (empty? coll)
       result
       (if (keyword? head)
         (recur (next coll) head (assoc result head []))
         (recur (next coll) curr-key (merge-with conj result {curr-key head}))))))

(defcheck solution-bd3edebd
  (fn [s] (->> s (reduce
                   (fn [[a k] x]
                     (if (keyword? x)
                       [(assoc a x []) x]
                       [(assoc a k (conj (get a k) x)) k]))
                   [{} nil]) first)))

(defcheck solution-bd4a5c71
  (fn [l]
    (loop [rv {} t l k nil]
      (if (empty? t)
        rv
        (let [f (first t) r (rest t)]
          (if (keyword? f)
            (recur (assoc rv f []) r f)
            (recur (update-in rv [k] conj f) r k)))))))

(defcheck solution-bd669a54
  (fn [s] (loop [k nil acc {} s s]
            (if (empty? s)
              acc
              (let [x (first s)]
                (if (keyword? x)
                  (recur x (assoc acc x []) (rest s))
                  (recur k (assoc acc k (conj (get acc k []) x)) (rest s))))))))

(defcheck solution-bda8ac80
  (fn [x]
    (loop [[h & t] x, r {}]
      (if h
        (recur (drop-while (complement keyword?) t)
          (assoc r h (take-while (complement keyword?) t)))
        r))))

(defcheck solution-bdd29c3
  (fn [coll]
    (let [result    (reduce #(assoc %1 %2 []) {} (filter keyword? coll))
          filt-coll (partition 2 (partition-by keyword? coll))]
      (reduce #(assoc %1 (last (first %2)) (last %2)) result filt-coll))))

(defcheck solution-bf82004f
  (fn [l]
    (loop [[a & m] l r {}]
      (if a
        (recur (drop-while number? m)
          (assoc r a (take-while number? m))) r))))

(defcheck solution-bfa4f1b6
  #((fn b [m [k v & more]]
      (cond (not k) m
            (number? (first v)) (b (assoc m (first k) v) more)
            true (b (assoc m (first k) []) (cons v more))))
    {} (partition-by (fn [x] (or (number? x) (identity x))) %)))

(defcheck solution-bfff6997
  (fn [x]
    (let [[m v] (reduce (fn [[m v] y]
                          (if (keyword? y)
                            [(conj m v)
                             [y []]]
                            [m
                             [(first v)
                              (conj (last v) y)]]))
                  [{} nil]
                  x)]
      (conj m v))))

(defcheck solution-c0102fba
  (fn [inputs] (loop [result {} xs inputs last-key :none]
                 (if (empty? xs) result
                                 (let [x (first xs) y (rest xs)]
                                   (cond
                                     (keyword? x) (recur (assoc result x []) y x)
                                     (integer? x) (recur (update-in result [last-key] conj x) y last-key)
                                     )
                                   )
                                 )
                 )
    ))

(defcheck solution-c0917ce5
  (fn [xs]
    (let [with-vector (mapcat #(if (keyword? %) (list % []) (list %)) xs)
          grouped-up  (partition 2 (partition-by keyword? with-vector))
          ready       (mapcat #(list (first (first %)) (into (first (second %)) (rest (second %)))) grouped-up)]
      (apply hash-map ready))))

(defcheck solution-c17febdf
  (fn [s]
    (:result
     (reduce
       (fn [acc x]
         (if (keyword? x)
           (-> acc
             (update-in [:result] assoc x [])
             (assoc-in [:key] x))
           (update-in acc [:result (:key acc)] conj x)))
       {:result {} :key nil}
       s))))

(defcheck solution-c1837116
  (fn prob-105 [coll]
    (loop [h   (first coll)
           t   (rest coll)
           key h
           out {}]
      (if (nil? h)
        (reduce #(assoc %1 (first %2) (reverse (second %2))) {}
          (for [[k v] out]
            (vector k (remove nil? v))))
        (recur (first t)
          (rest t)
          (if (keyword? h)
            h
            key)
          (assoc out key (conj (get out key)
                           (if (keyword? h)
                             nil
                             h))))))))

(defcheck solution-c18ccfc8
  (fn n105 [coll]
    (loop [c coll a {} curr nil]
      (if (empty? c)
        a
        (recur (rest c)
          (if (keyword? (first c)) (into a [[(first c) []]]) (merge-with conj a {curr (first c)}))
          (if (keyword? (first c)) (first c) curr))))))

(defcheck solution-c1916da7
  (fn [v]
    (->> (interpose "," v)
      (partition-by keyword?)
      (partition-all 2)
      (map (fn [[[k] vs]] [k (remove #{","} vs)]))
      (into {}))))

(defcheck solution-c1e12c71
  (fn this [l]
    (if (empty? l) {}
                   (assoc (this (drop-while (comp not keyword?) (rest l)))
                     (first l)
                     (vec (take-while (comp not keyword?) (rest l)))))))

(defcheck solution-c1ed8785
  #(loop [xs       %
          last-key nil
          nums     []
          result   {}]
     (if (seq xs)
       (let [current (first xs)]
         (if (keyword? current)
           (if (nil? last-key)
             (recur (rest xs) current nums result)
             (recur (rest xs) current [] (into result [[last-key nums]])))
           (recur (rest xs) last-key (conj nums current) result)))
       (if (nil? last-key)
         result
         (into result [[last-key nums]])))))

(defcheck solution-c2053e60
  (fn [s] (if (empty? s) {} (
                              apply merge (filter #(not (nil? %)) (map-indexed
                                                                    (fn [idx itm] (if (keyword? itm)
                                                                                    (loop [i (inc idx) n []]
                                                                                      (if (>= i (count s)) (assoc {} itm n)
                                                                                                           (if (keyword? (get s i)) (assoc {} itm n)
                                                                                                                                    (recur (inc i) (conj n (get s i)))
                                                                                                                                    )
                                                                                                           )
                                                                                      )
                                                                                    ))
                                                                    s)))
                         )))

(defcheck solution-c209673c
  (fn [v]
    (first (reduce (fn [[res last] v]
                     (if (number? v)
                       [(update-in res [last] conj v) last]
                       [(assoc res v []) v]))
             [{} nil] v))))

(defcheck solution-c237d9c0
  #(reduce
     (fn [m [ks es]]
       (let [[k & ks] (reverse ks)]
         (merge (assoc m k es)
           (into {} (for [k ks] [k []])))))
     {}
     (partition 2 2 [] (partition-by keyword? %))))

(defcheck solution-c24538a7
  (fn [coll]
    (loop [m {} c (rest coll) k (first coll) vs []]
      (if (empty? c)
        (if (nil? k)
          m
          (merge m {k vs}))
        (let [x (first c)]
          (if (keyword? x)
            (recur (merge m {k vs}) (rest c) x [])
            (recur m (rest c) k (conj vs x))))))))

(defcheck solution-c247af26
  (fn stuff [data]
    (if (not-empty data)
      (let
       [k    (first data)
        more (rest data)]
        (conj {k (take-while (complement keyword?) more)} (stuff (drop-while (complement keyword?) more))))
      {})))

(defcheck solution-c249561f
  (fn kav [v]
    (if (empty? v)
      {}
      (loop [k  (first v)
             v  (rest v)
             tv []
             m  {}]
        (cond
          (empty? v) (assoc m k tv)
          (not (keyword? (first v))) (recur k (rest v) (conj tv (first v)) m)
          (keyword? (first v)) (recur (first v) (rest v) [] (assoc m k tv)))))))

(defcheck solution-c31a63ab
  (fn [lst]
    (dissoc (reduce
              (fn [acc x]
                (if (keyword? x)
                  (assoc acc x [] :curr x)
                  (update-in acc [(:curr acc)] conj x)
                  )
                )
              {}
              lst
              ) :curr)
    ))

(defcheck solution-c3ae726f
  (fn identify [v]
    ((reduce
       (fn [[m l-k] x]
         (if (keyword? x)
           [(merge m {x []}) x]
           [(update-in m [l-k] #(conj % x)) l-k]
           )
         )
       [{} nil]
       v) 0)))

(defcheck solution-c45e5335
  (fn [coll]
    (apply hash-map
      (loop [result [] [kw & rst :as coll] coll]
        (if (empty? coll)
          result
          (let [[values rst'] (split-with (complement keyword?) rst)]
            (recur (concat result [kw values]) rst')))))))

(defcheck solution-c45ef958
  (fn [coll]
    (let [_coll (reduce #(if (and (keyword? (last %1)) (keyword? %2))
                           (conj %1 nil %2)
                           (conj %1 %2)) [] coll)]
      (reduce #(conj % [(first (key %2)) (vec (filter (complement nil?) (val %2)))]) {}
        (apply hash-map (partition-by keyword? _coll))))))

(defcheck solution-c51339d0
  (fn [c]
    (reduce (fn [acc [k v]] (merge (conj acc [(last k) v]) (zipmap (butlast k) (repeat []))))
      {}
      (partition 2 (partition-by keyword? c)))))

(defcheck solution-c53605c3
  (fn [s]
    (if (seq s) (->> s
                  (partition-by keyword?)
                  (mapcat #(if (keyword? (first %)) (interpose [] %) [%]))
                  (apply assoc {})) {})))

(defcheck solution-c5722790
  (fn [v]
    (into {}
      (for [x v
            :when (keyword? x)
            :let [y (drop-while #(not= x %) v)]]
        [x (vec (take-while (complement keyword?) (rest y)))]))))

(defcheck solution-c5a4a082
  (fn [coll]
    (let [parts (partition-by #(when (keyword? %) %) coll)]
      (if (empty? parts)
        {}
        (loop [parts parts
               m     {}]
          (if-let [[[keyword] & more] parts]
            (if (empty? more)
              (assoc m keyword [])
              (let [[[x & xs :as next] & others] more]
                (if (keyword? x)
                  (recur more (assoc m keyword []))
                  (recur others (assoc m keyword next)))))
            m))))))

(defcheck solution-c5c04a34
  (fn ident
    ([v] (ident v nil {}))
    ([[f & r :as s] k hm]
     (cond (empty? s) hm
           (keyword? f) (recur r f (assoc hm f []))
           :else (recur r k (update-in hm [k] conj f))))))

(defcheck solution-c5d6df3b
  (fn identify_key_val [x]
    (letfn [(f [[k & r] result]
              (if k (let [[v t] (split-with (complement keyword?) r)]
                      (f t (assoc result k (vec v))))
                    result))]
      (f x {}))))

(defcheck solution-c5de5d9f
  (fn [xs]
    (apply hash-map
      (reduce (fn [ac x] (if (keyword? x) (conj ac x []) (conj (pop ac) (conj (peek ac) x)))) [] xs))))

(defcheck solution-c61f3b0f
  (fn partition-by-kw [coll]
    (if (empty? coll) {}
                      (loop [m      {}
                             cur_kw (first coll)
                             acc    []
                             xs     (rest coll)]
                        #_(println m cur_kw acc xs)
                        (cond (empty? xs) (assoc m cur_kw acc)
                              (keyword? (first xs)) (recur (assoc m cur_kw acc) (first xs) [] (rest xs))
                              :else (recur m cur_kw (conj acc (first xs)) (rest xs)))))))

(defcheck solution-c68890c7
  (fn [a-seq]
    (loop [lseq a-seq acc {}]
      (if (empty? lseq)
        acc
        (let [next-key   (first lseq)
              nums-first (drop 1 lseq)
              next-vals  (take-while #(number? %) nums-first)
              key-first  (drop-while #(number? %) nums-first)]
          (recur key-first (assoc acc next-key next-vals)))))))

(defcheck solution-c73cdf97
  (fn [coll]
    (loop [[h & ts] coll
           rmap {}]
      (if (nil? h)
        rmap
        (let [[num lst]
              (split-with
                number?
                ts)]
          (recur
            lst
            (assoc rmap h num)))))))

(defcheck solution-c7e672f
  (fn [coll]
    (first
      (reduce
        (fn [[m k] r1]
          (if (keyword? r1)
            [(assoc m r1 []) r1]
            [(assoc m k (conj (get m k []) r1)) k]))
        [{} nil]
        coll))))

(defcheck solution-c7ff4c76
  (fn [coll]
    (loop [curr-kw (first coll) mapped {} remaining coll]
      (if (seq remaining)
        (let [v (first remaining)]
          (if (keyword? v)
            (recur v (assoc mapped v []) (rest remaining))
            (recur curr-kw (assoc mapped curr-kw (conj (get mapped curr-kw) v)) (rest remaining))))
        mapped))))

(defcheck solution-c91f5f77
  (fn [l]
    (letfn [(toMap [lat]
              (if (empty? lat)
                []
                (let [k (first lat)
                      v (into [] (for [l (rest lat) :while (not (keyword? l))] l))
                      n (count v)]
                  (cons k (cons v (toMap (nthrest lat (+ 1 n))))))))]
      (if (empty? l) {} (apply (partial assoc {}) (toMap l))))))

(defcheck solution-c9858716
  (fn [xs]
    (reduce #(if (keyword? %2)
               (assoc %1 %2 [])
               (assoc %1 (first (last %1)) (conj (second (last %1)) %2)))
      (sorted-map) xs)))

(defcheck solution-c9894503
  #(loop [result {}, k (first %), v [], remaining (rest %)]
     (if-let [newVal (first remaining)]
       (if
        (keyword? newVal) (recur (assoc result k v) newVal [] (rest remaining))
                          (recur result k (conj v newVal) (rest remaining)))
       (if (nil? k) result
                    (assoc result k v)))))

(defcheck solution-ca6264fe
  #(reduce
     (fn [m v]
       (if (keyword? v)
         (assoc m v [])
         (let [k (-> m keys sort last)
               s (get m k)]
           (assoc m k (conj s v)))))
     {}
     %))

(defcheck solution-ca871401
  (fn [c]
    (loop [r c
           f {}]
      (if (empty? r)
        f
        (let [k (first r)
              [v n] (split-with number? (rest r))
              m (conj f [k v])]
          (recur n m))))))

(defcheck solution-ca8b1ec5
  (partial
    (fn mk-map
      [acc s]
      (if (empty? s) (into {} acc)
                     (recur
                       (if (keyword? (first s)) (conj acc {(first s) []})
                                                (conj (drop 1 acc) {(first (keys (first acc)))
                                                                    (conj (first (vals (first acc))) (first s))})) (rest s))))
    '()))

(defcheck solution-cacd658c
  (fn f [xs]
    (->> xs
      (partition-by keyword?)
      (mapcat #(if (keyword? (first %)) (interpose [] (map first (partition 1 %))) [(vec %)]))
      (partition 2 2 ())
      (map #(apply vector %))
      (into {})
      )))

(defcheck solution-cadd3221
  #(loop [[k & r] % c nil res {}]
     (cond
       (nil? k) res
       (keyword? k) (recur r k (assoc res k []))
       :default (recur r c (update-in res [c] conj k)))))

(defcheck solution-cb7d1b86
  (fn [s]
    (loop [s s, k nil, m {}]
      (if-let [cur (first s)]
        (if (keyword? cur)
          (recur (rest s) cur (assoc m cur []))
          (recur (rest s) k (update-in m [k] conj cur)))
        m))))

(defcheck solution-cc490948
  (fn [kvs]
    (loop [[k & v] kvs map {}]
      (if (nil? k)
        map
        (let [[values leftover] (split-with number? v)]
          (recur leftover (assoc map k values)))))))

(defcheck solution-cd1b7a83
  (fn [xs] (letfn [(rmap [m k t ys]
                     (let [y1 (first ys)]
                       (if y1 (if (= (type :a) (type y1))
                                (recur (assoc m k t) y1 [] (rest ys))
                                (recur m k (conj t y1) (rest ys))
                                )
                              (assoc m k t)
                              )
                       )
                     )]
             (if (first xs)
               (rmap {} (first xs) [] (rest xs))
               {}
               )
             )))

(defcheck solution-cd7fa230
  (fn __ [coll]
    (let
     [k (first coll)
      v (take-while number? (rest coll))]
      (if-not k
        {}
        (conj {k v} (__ (drop (inc (count v)) coll)))))))

(defcheck solution-cdec251a
  (fn mapMaker [l]
    (if (empty? l)
      {}
      (second (reduce (fn [[keyw m] el]
                        (if (keyword? el)
                          [el (conj m [el []])]
                          [keyw (update-in m [keyw] #(conj % el))]))
                [:a {}]
                l)))))

(defcheck solution-ce171fa9
  (fn [coll]
    (loop [[f & r] coll
           xs {}]
      (if f
        (let [[ns k] (split-with number? r)]
          (recur k
            (merge-with concat xs {f ns})))
        xs))))

(defcheck solution-cf69be6f
  (fn ikey [mymap incoll]
    (if (empty? incoll) mymap
                        (let [key      (first incoll)
                              val      (take-while #(not (keyword? %)) (rest incoll))
                              nxt-map  (assoc mymap key val)
                              nxt-coll (drop-while #(not (keyword? %)) (rest incoll))]
                          (ikey nxt-map nxt-coll)))) {})

(defcheck solution-cf7334c3
  #(loop [m {} xs %]
     (if (seq xs)
       (let [[k & xs] xs
             [values xs] (split-with (complement keyword?) xs)]
         (recur (assoc m k values) xs))
       m)))

(defcheck solution-cf98d7d
  (fn keysAndValueX [v] ((fn keysAndValueRec [v res]
                           (if (empty? v) res
                                          (if (= (count v) 1) (assoc res (first v) [])
                                                              (if (keyword? (second v))
                                                                (keysAndValueRec (rest v) (assoc res (first v) []))
                                                                (keysAndValueRec ((fn after2 [v] (rest (rest v))) v) (assoc res (first v) (second v)))
                                                                )
                                                              )
                                          )
                           ) ((fn partitionKeywordSimple [v]
                                (mapcat
                                  (fn [n] (if (keyword? (first n)) n [n]))
                                  ((fn partitionKeyword [v] (partition-by (fn [n] (keyword? n)) v)) v)
                                  )
                                ) v) {})))

(defcheck solution-cfcfc5a7
  (fn [s] (let [x (partition-by keyword? s)] ((fn f ([a k r] (if (empty? a) r (let [e (first a)] (if (keyword? (first e)) (f (rest a) (last e) (reduce #(assoc %1 %2 []) r e)) (f (rest a) nil (assoc r k (first a)))))))) x nil {}))))

(defcheck solution-d00301c7
  (fn [coll]
    (dissoc (reduce
              (fn [accum item]
                (if (keyword? item)
                  (assoc accum :seen item item [])
                  (update-in accum [(:seen accum)] #(conj % item)))) {} coll) :seen)))

(defcheck solution-d02d0a80
  (fn mykv [xs]
    (let [hm
          (reduce
            (fn [h r]
              (let [k (h :key), v (h :vals), o (h :out)]
                #_(println "key=" k ", vals=" v ", out=" o ", r=" r)
                (if (keyword? r)
                  (if (keyword? k)
                    (assoc h :out (assoc o k v) :vals [] :key r)
                    (assoc h :key r :vals []))
                  (assoc h :vals (into v (list r))))))
            {:key nil :vals [] :out {}} (conj xs :end))]
      (hm :out))))

(defcheck solution-d0330bb9
  (fn [coll]
    (apply hash-map
      (mapcat #(let [[k v] %]
                 (concat
                   (vec
                     (mapcat (fn [empty-key] (list empty-key []))
                       (butlast k)))
                   (list (last k) v)))
        (partition 2
          (partition-by keyword? coll))))))

(defcheck solution-d053d0ff
  #(let [r (fn [m v]
             (if (number? v)
               (update-in m [:r (:k m)] conj v)
               (-> m
                 (assoc :k v)
                 (assoc-in [:r v] []))))]
     (:r (reduce r {:r {}} %))))

(defcheck solution-d0552b33
  (fn f
    ([c] (f c {}))
    ([c m]
     (if (empty? c) m
                    (let [k    (first c)
                          nums (take-while number? (rest c))
                          c    (drop-while number? (rest c))]
                      (recur c (assoc m k nums)))))))

(defcheck solution-d0c9372f
  (fn make-map [ls]
    (if (empty? ls) {}
                    (let [xs (cons (first ls) (take-while (complement keyword?) (rest ls)))
                          k  (first xs)
                          v  (vec (rest xs))
                          ys (drop-while (complement keyword?) (rest ls))]
                      (merge (assoc {} k v) (make-map ys))))))

(defcheck solution-d0e8f136
  (fn f [l]
    (loop [i (first l)
           s (rest l)
           k nil
           m {}]
      (if (nil? i)
        m
        (if (keyword? i)
          (recur (first s) (rest s) i (assoc m i []))
          (recur (first s) (rest s) k (assoc m k (conj (m k) i))))))))

(defcheck solution-d0e92fc
  (fn kvs [s]
    (if (seq s)
      (let [[k & r] s
            [vs r] (split-with #(not (keyword %)) r)]
        (merge {k vs} (kvs r)))
      {})))

(defcheck solution-d1452c60
  (fn [coll]
    (loop [acc  {}
           coll coll]
      (if (empty? coll)
        acc
        (recur
          (assoc acc (first coll) (take-while number? (rest coll)))
          (drop-while number? (rest coll)))))))

(defcheck solution-d19e3287
  (fn keys-and-values [x]
    (let [cx (mapcat
               #(if (keyword? (first %))
                  (interpose
                    '()
                    (partition-all 1 %))
                  (list %))
               (partition-by
                 keyword?
                 x))]
      (zipmap (map first (filter #(keyword? (first %)) cx)) (filter #(not (keyword? (first %))) cx)))))

(defcheck solution-d1fb67e7
  (fn [coll]
    (loop [coll coll res {}]
      (if (empty? coll)
        res
        (let [k (first coll)
              r (rest coll)
              [nums r] (split-with number? r)]
          (recur r (assoc res k (into [] nums))))))))

(defcheck solution-d2658df2
  (fn kv [xs]
    (if (empty? xs)
      {}
      (let [[k & r1] xs
            [v r2] (split-with number? r1)]
        (assoc (kv r2) k v)
        )
      )
    ))

(defcheck solution-d2978b8a
  (fn [xs]
    (into {} (loop [res [] xs xs]
               (let [k1 (first (take-while keyword? xs))
                     v1 (take-while (complement keyword?) (drop 1 xs))
                     ]
                 (if (nil? k1)
                   res
                   (recur (conj res [k1 v1]) (drop (+ 1 (count v1)) xs))
                   )
                 )))
    ))

(defcheck solution-d2bb738c
  (fn [s]
    (loop [s   s
           k   nil
           res {}]
      (if-let [e (first s)]
        (if (keyword? e)
          (recur (rest s) e (assoc res e []))
          (recur (rest s) k (assoc res k (conj (get res k) e))))
        res))))

(defcheck solution-d2e76b9c
  (fn [x]
    (let [take-pairs (fn tp [coll res]
                       (if (empty? coll)
                         res
                         (let [f  (first coll)
                               vs (vec (take-while #(not (keyword? %)) (rest coll)))]
                           (tp (drop (inc (count vs)) coll) (conj res [f vs])))))]
      (into {} (take-pairs x [])))))

(defcheck solution-d2e77fd3
  (fn [cl] (let [cnt (count cl)
                 ck  (filter keyword? cl)
                 cv  (for [i (range cnt) :when (keyword? (nth cl i))]
                       (vec (for [p (range (inc i) cnt) :while (not (keyword? (nth cl p)))] (nth cl p))))]
             #_(println ck)
             #_(println cv)
             (zipmap ck cv)
             )))

(defcheck solution-d30c0292
  (fn [s] (into {} (for [[[k] [n & ns :as nn]] (partition 2 1 (partition-by #(if (keyword? %) % (type %)) s)) :when (keyword? k)] (if (keyword? n) [k []] [k nn])))))

(defcheck solution-d35fd659
  #(into {}
     (apply concat
       (for [[a b] (partition 2 (partition-by keyword? %))]
         (apply list [(last a) b]
           (for [a (butlast a)] [a []]))))))

(defcheck solution-d3752429
  (fn [s]
    (loop [r s res {}]
      (if (empty? r)
        res
        (let [k (split-with keyword? r)
              l (split-with number? (last k))
              m (->> (butlast (first k))
                  (map #(into {} {% []}))
                  (#(into % (into {} {(last (first k)) (first l)}))))
              ]
          (recur (last l) (into res m)))))))

(defcheck solution-d38b04be
  (fn [coll]
    (let [mapped-items
          (reduce
            (fn [res el]
              (if (keyword? el)
                (conj res [el])
                (let [old (last res)
                      new (conj old el)]
                  (conj (into [] (drop-last res)) new))))
            []
            coll)]
      (into {} (for [[k & v] mapped-items] [k (into [] v)])))))

(defcheck solution-d3e2aaca
  (fn [s]
    ((fn [r s]
       (if (empty? s)
         r
         (let [vs (take-while number? (next s))]
           (recur (conj r [(first s) vs]) (drop (inc (count vs)) s))))) {} s)))

(defcheck solution-d4225350
  (fn [xs]
    (loop [xs (seq xs) kw nil m {}]
      (if-let [[item & more] xs]
        (if (keyword? item)
          (recur more item (assoc m item []))
          (recur more kw (update-in m [kw] conj item)))
        m))))

(defcheck solution-d454fe07
  (letfn [(I [s]
            (loop [s s r {}]
              (cond (empty? s) r
                    (keyword? (first s))
                    (recur (drop-while (complement keyword?)
                             (rest s))
                      (conj r {(first s) (take-while (complement keyword?)
                                           (rest s))})))))]
    I))

(defcheck solution-d45f3852
  (fn [s]
    (first (reduce
             #(if
               (keyword? %2)
                [(assoc (first %) %2 []) %2]
                [(assoc (first %) (second %) (conj ((first %) (second %)) %2)) (second %)])
             [{} nil]
             s))))

(defcheck solution-d4986184
  (fn a [s]
    (first (reduce
             (fn [[r k] v]
               (if (number? v)
                 [(assoc r k (conj (r k) v)) k]
                 [(assoc r v []) v]
                 )) [{} 0] s))))

(defcheck solution-d52dd6e6
  (fn f [v]
    (loop [ans   {}
           vv    (next v)
           key   (first v)
           value []]
      (if (empty? vv) (if key (conj ans [key value]) ans)
                      (if (keyword? (first vv))
                        (recur (conj ans [key value]) (next vv) (first vv) [])
                        (recur ans (next vv) key (conj value (first vv))))))))

(defcheck solution-d557b13a
  (fn [s]
    (loop [built {} curr-k (first s) curr-v [] left (rest s)]
      (if (empty? left) (if (empty? curr-v) built (assoc built curr-k curr-v))
                        (if (keyword? (first left))
                          (recur (assoc built curr-k curr-v) (first left) [] (rest left))
                          (recur built curr-k (conj curr-v (first left)) (rest left)))))))

(defcheck solution-d581b123
  (fn [s]
    (loop [m {} s s]
      #_(println s)
      (if
       (empty? s) m
                  (let [[kw & s] s
                        [vs s] (split-with number? s)]
                    (recur (assoc m kw (vec vs)) s))))))

(defcheck solution-d626c15f
  (fn
    [col]
    (reduce-kv
      #(into
         %1
         (apply hash-map (concat (interpose [] %2) [%3])))
      {}
      (apply hash-map (partition-by keyword? col)))))

(defcheck solution-d66cb861
  (fn f [[k & v]]
    (if v
      (let [[a b] (split-with number? v)]
        (assoc (f b) k a))
      {})))

(defcheck solution-d6713878
  (fn [s]
    (apply array-map
      (map #(if (keyword? (first %)) (first %) (flatten %))
        (partition-by keyword? (interleave s (repeat [])))
        ))
    ))

(defcheck solution-d8c58178
  (fn key-sequence- [coll]
    "105. Given an input sequence of keywords and numbers, create a map
  such that each key in the map is a keyword, and the value is a
  sequence of all the numbers (if any) between it and the next keyword
  in the sequence."
    (loop [[keys values & xs] (partition-by keyword? coll)
           acc {}]
      (if (empty? keys)
        acc
        (recur xs (merge acc {(last keys) (vec values)} (zipmap (butlast keys) (repeat []))))))))

(defcheck solution-d907e08
  (fn [s]
    (reduce (fn [res [kwds vls]]
              (let [kwds_no_vls (butlast kwds)
                    kwd_last    (last kwds)]
                (conj res [kwd_last vls] (when kwds_no_vls (zipmap kwds_no_vls (repeat []))))))
      {}
      (partition 2 (partition-by keyword? s)))))

(defcheck solution-d90f1ad9
  (fn [x]
    (loop [c x r {}]
      (if (empty? c) r
                     (recur (drop-while (complement keyword?) (rest c))
                       (assoc r (first c) (take-while (complement keyword?) (rest c))))))))

(defcheck solution-d973ddb2
  (fn [x]
    (->> x
      (partition-by keyword?)
      (partition 2)
      (reduce (fn [m [ks y]]
                (reduce (fn [m k]
                          (assoc m k [])) (assoc m (last ks) y) (butlast ks))) {}))))

(defcheck solution-d9828f01
  (fn [coll]
    (->> coll
      (reduce (fn [[m k] v]
                (if (keyword? v)
                  [(assoc m v []) v]
                  [(assoc m k (conj (m k) v)) k])) [])
      (first)
      (into {}))))

(defcheck solution-d9cde6c
  (fn [x]
    (loop [r {}, k nil, v [], [f & x] x]
      (if f
        (if (keyword? f) (recur (if k (assoc r k v) r) f [] x)
                         (recur r k (conj v f) x))
        (if k (assoc r k v) {})))))

(defcheck solution-d9d158c4
  (fn [seq]
    (loop [r {} s seq k (first s)]
      (if (empty? s) r
                     (if (keyword? (first s)) (recur (merge r {(first s) []}) (rest s) (first s))
                                              (recur (merge r {k (conj (val (find r k)) (first s))}) (rest s) k)
                                              )
                     )
      )
    ))

(defcheck solution-da217f44
  (fn [s]
    (loop [[head & tail :as s] s result {} kw :?]
      (cond
        (empty? s) result
        (keyword? head) (recur tail (assoc result head []) head)
        :else (recur tail (assoc result kw (conj (result kw) head)) kw)))))

(defcheck solution-da724f65
  (fn [y]
    (second (reduce
              (fn [[l m] x]
                (if (keyword? x)
                  [x (assoc m x [])]
                  [l (update-in m [l] #(conj % x))]))
              [nil {}] y))))

(defcheck solution-da72aede
  (fn [xs]
    (apply hash-map
      (mapcat (fn [[ks vs]]
                (into (vec (interpose [] ks)) [(vec vs)]))
        (partition 2 (partition-by keyword? xs))))))

(defcheck solution-db08d0e8
  (fn [n]
    (->> n (partition-by #(if (integer? %) true %))
      (reduce (fn [lst n] (if (and (keyword? (last lst)) (keyword? (last n)))
                            (conj lst [] (first n))
                            (conj lst (if (keyword? (first n)) (first n) n))
                            )) [])
      (apply hash-map)

      )
    ))

(defcheck solution-dbe75e97
  (fn x [s]
    (if (empty? s)
      {}
      (let [k (first s)
            [nums r] (split-with #(not (keyword? %)) (rest s))]
        (assoc (x r) k nums)))))

(defcheck solution-dbfdaf93
  (fn [v]
    (reduce #(assoc %1 (first (first %2)) (vec (last %2)))
      {} (map
           #(vector (first %) (filter number? (last %)))
           (partition 2 (partition-by keyword? (interleave v (repeat (count v) []))))))))

(defcheck solution-dc34d02
  (fn [xs]
    (loop [x (first xs) s (rest xs) r {} k nil]
      (if (nil? x)
        r
        (if (keyword? x)
          (recur (first s) (rest s) (assoc r x []) x)
          (recur (first s) (rest s) (merge-with concat r {k [x]}) k)
          )
        )
      )
    ))

(defcheck solution-dc6cc86a
  (partial
    (fn x [m k l]
      (let [[n r] (split-with number? l)]
        (if (pos? (count l))
          (if k
            (x (assoc m k n) nil r)
            (x m (first l) (rest l)))
          m)))
    {}
    nil))

(defcheck solution-dc805779
  (fn p105
    ([xs] (p105 xs {}))
    ([[l & xs] m]
     (if (nil? l) m
                  (let [[nums rst] (split-with number? xs)]
                    (recur rst (assoc m l (vec nums))))))))

(defcheck solution-dc9ea703
  (fn f [s]
    (if (empty? s)
      {}
      (let [[k & r] s
            [v n] (split-with number? r)]
        (assoc (f n) k v)))))

(defcheck solution-dcc001e9
  (fn id [x]
    (if (empty? x)
      {}
      (conj (hash-map (first x)
              (take-while #(not (keyword? %)) (rest x)))
        (id (drop-while #(not (keyword? %)) (rest x)))))))

(defcheck solution-dcd04555
  (fn [x]
    (let [g (into [] (rest (clojure.string/split (apply str x) #":")))]
      (zipmap (map keyword (map str (map first g)))
        (map #(replace {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} %) (map vec (map rest g)))))))

(defcheck solution-dd19d7cb
  (fn [xs]
    (loop [m {}, coll xs]
      (if (empty? coll) m
                        (let [k (first coll)
                              v (take-while #(not (keyword? %)) (drop 1 coll))]
                          (recur (conj m {k v}) (drop (+ 1 (count v)) coll)))))))

(defcheck solution-dd9694cc
  (fn [kvals]
    (into {}
      (reduce #(if (keyword? %2) (cons [%2 []] %1)
                                 (let [[k l] (first %1)] (cons [k (conj l %2)] (rest %1)))) (list) kvals))))

(defcheck solution-ddb5180a
  (fn [c] (into {} (mapv vec (partition 2 (reduce
                                            (fn [acc i]
                                              (if (keyword? i)
                                                (if (keyword? (last acc))
                                                  (conj (conj acc []) i)
                                                  (conj acc i))
                                                (let [li (dec (count acc))
                                                      le (acc li)]
                                                  (if (keyword? le)
                                                    (conj acc [i])
                                                    (assoc acc li (conj le i))))))
                                            []
                                            c))))))

(defcheck solution-dded5b86
  (fn keys-and-vals [coll1]
    (loop [result {} coll coll1 last-keyword nil]
      #_(println coll last-keyword (get result last-keyword))
      (if (empty? coll)
        result
        (recur
          (if (keyword? (first coll))
            (assoc result (first coll) [])
            (assoc result last-keyword (conj (get result last-keyword) (first coll)))
            )
          (rest coll)
          (if (keyword? (first coll))
            (first coll)
            last-keyword
            )
          )
        )
      )
    ))

(defcheck solution-ded0352d
  #(->> %
     (interpose nil)
     (partition-by keyword?)
     (partition 2)
     (mapcat (fn [[k v]] [(first k) (filter (complement nil?) v)]))
     (apply hash-map)))

(defcheck solution-df24f2a4
  (fn idkeys [x]
    (loop [input (reverse x) res '{}]
      (cond (empty? input) res
            :else
            (let [spliting (split-with #(number? %) input)]
              (recur (rest (second spliting)) (assoc res (first (second spliting)) (reverse (first spliting))))
              )
            )
      )
    ))

(defcheck solution-df2edb8c
  (fn [c] (let [[m c]
                (reduce
                  (fn [[m c] i]
                    (if (and (keyword? i) (not (empty? c)))
                      [(assoc m (first c) (rest c)) [i]]
                      [m (conj c i)]))
                  [{} []] c)]
            (if-not (empty? c)
              (assoc m (first c) (rest c))
              m))))

(defcheck solution-e057cd7e
  (fn [xs]
    (if (empty? xs) {}
                    (apply hash-map (apply concat
                                      (map
                                        (fn [[k v]] [k (map second (remove (partial apply =) v))])
                                        (group-by first
                                          (reductions
                                            (fn [[k _] x] (if (keyword? x) [x x] [k x]))
                                            [(first xs) (first xs)]
                                            (rest xs)))))))))

(defcheck solution-e13c10bd
  (fn [kvs]
    (loop [m {} k nil v kvs]
      (if (seq v)
        (let [n (first v)]
          (if (keyword? n)
            (recur (assoc m n []) n (rest v))
            (recur (update-in m [k] concat [n]) k (rest v))))
        m))))

(defcheck solution-e191d27b
  (fn sequifier [invals]
    (if (empty? invals) (sorted-map)
                        (apply merge (sorted-map (first invals) (take-while number? (rest invals)))
                          (sequifier (drop-while number? (rest invals)))))))

(defcheck solution-e1d0a3a4
  (fn rmap [vals]
    (loop [vals vals m {}]
      (if (empty? vals)
        m
        (let [notk     (complement keyword?)
              k        (first vals)
              vs       (take-while notk (rest vals))
              nextvals (drop-while notk (rest vals))
              ]
          (recur nextvals (into m {k vs})))))))

(defcheck solution-e21bb60f
  (fn [col]
    (letfn [(splitidkeys [xs]
              (cond (keyword? (first xs))
                    (cons (vector (first xs) (take-while number? (rest xs))) (splitidkeys (rest xs)))
                    (number? (first xs)) (splitidkeys (rest xs))
                    :else nil))]
      (reduce (fn [a [b c]] (assoc a b c)) {} (splitidkeys col)))))

(defcheck solution-e2a60471
  (fn [m]
    (second
      (reduce
        (fn [[slot acc] x]
          (if (keyword? x)
            [x (assoc acc x [])]
            [slot (assoc acc slot (conj (acc slot) x))]))
        [nil {}]
        m))))

(defcheck solution-e2c2ad63
  (fn [v]
    (loop [m {} stack [] input v]
      (cond
        (empty? input)
        (if (empty? stack) m (into m {(first stack) (vec (rest stack))}))

        (keyword (first input))
        (recur (if (empty? stack) m (into m {(first stack) (vec (rest stack))})) [(first input)] (rest input))

        true
        (recur m (conj stack (first input)) (rest input))))))

(defcheck solution-e2e9ebae
  (fn f
    [[k & r]]
    (if k
      (let [[v q] (split-with integer? r)]
        (conj {k v} (f q)))
      {})))

(defcheck solution-e35c3a48
  (fn [coll]
    (loop [coll coll
           key  nil
           val  nil
           res  {}]
      (cond (empty? coll)
            (if-not (nil? key)
              (conj res {key val})
              res)

            (nil? key)
            (if (keyword? (first coll))
              (recur (rest coll) (first coll) [] res)
              nil)

            (keyword? (first coll))
            (recur (rest coll) (first coll) [] (conj res {key val}))

            :else
            (recur (rest coll) key (conj val (first coll)) res)))))

(defcheck solution-e3b29d86
  (fn key-trans [v]
    (letfn [(step [cmap keywd now-v]
              (if (keyword? now-v)
                [(assoc cmap now-v []) now-v]
                [(assoc cmap keywd (conj (get cmap keywd []) now-v)) keywd]))]
      (first (reduce #(step (first %1) (last %1) %2) [{} (first v)] (drop 1 v))))))

(defcheck solution-e3bc8461
  (fn seq-map [coll]
    (->> coll
      (reduce (fn [[k {v k, :as acc}] x]
                (if (keyword? x)
                  [x (assoc acc x (get acc x []))]
                  [k (assoc acc k (conj v x))]))
        [nil {}])
      last)))

(defcheck solution-e4370f89
  (fn
    [v]
    (loop [l (partition-by keyword v) f {}]
      (if (empty? l)
        f
        (if (keyword? (first (second l)))
          (recur (rest l) (assoc f (first (first l)) []))
          (recur (drop 2 l) (assoc f (first (first l)) (vec (second l))))
          )))
    ))

(defcheck solution-e44128c1
  #(loop [[x & y] % kw nil ns [] acc {}]
     (let [nkw  (if (keyword? x) x kw)
           nns  (if (number? x) (conj ns x) [])
           nacc (if (number? x) acc (conj acc [kw ns]))]
       (if (not x) (dissoc nacc nil)
                   (recur y nkw nns nacc)))))

(defcheck solution-e4774374
  (fn identify-keys-vals [coll]
    (if-let [[key & t] (seq coll)]
      (let [[vals s] (split-with (complement keyword?) t)]
        (conj (identify-keys-vals s) {key vals}))
      {})))

(defcheck solution-e4ac3bf5
  (fn [s]
    (letfn [(a [m, k, v] (assoc m k (conj (m k) v)))
            (f [[m, k], x] (if (keyword? x) [(assoc m x []), x] [(a m k x), k]))]
      (first (reduce f [{}, nil] s)))))

(defcheck solution-e4adf337
  (fn [coll]
    (loop [res      {}
           last-key nil
           coll     coll]
      (if (empty? coll)
        res
        (let [v (first coll)]
          (if (keyword? v)
            (recur (assoc res v [])
              v
              (rest coll))
            (recur (merge-with conj res {last-key v})
              last-key
              (rest coll))))))))

(defcheck solution-e4b00175
  (fn [l]
    (let [f (fn [[d s v] x] (if (keyword? x) [(if s (assoc d s v) d) x []] [d s (conj v x)]))]
      (first (reduce f [{} nil []] (conj l :a))))))

(defcheck solution-e4cc6c7
  (fn kav [[head & tail]]
    (if (nil? head) {}
                    (into
                      {head (take-while (complement keyword?) tail)}
                      (kav (drop-while (complement keyword?) tail))))))

(defcheck solution-e5222f08
  (fn keyvalues
    [s]
    (letfn [(grabkey [results input]
              (if (seq input)
                #(grabvals (conj results (first input)) (next input))
                results))
            (grabvals [results input]
              (#(grabkey (conj results (vec (take-while (complement keyword?) input)))
                  (drop-while (complement keyword?) input))))]
      (apply hash-map (trampoline grabkey [] s)))))

(defcheck solution-e548d857
  (fn conv [s]
    (reduce (fn [r [k v]]
              (assoc (into r (zipmap k (repeat []))) (last k) v))
      {} (partition 2 (partition-by keyword? s)))))

(defcheck solution-e551cdc0
  (fn [sq]
    (into
      {}
      (mapcat (fn [[ks vs]]
                (cons [(last ks) vs]
                  (map (fn [k] [k []]) (butlast ks))))
        (partition
          2
          (partition-by keyword?
            sq))))))

(defcheck solution-e58a0ab1
  (fn k [x]
    (if (seq x)
      (let [[a & b] x]
        (conj
          {a (take-while #(not= (type :a) (type %)) b)}
          (k (drop-while #(not= (type :a) (type %)) b))))
      {})))

(defcheck solution-e58cde18
  (fn identify-keys-vals [vecs]
    (reduce (fn [to [k v]]
              (assoc (reduce #(assoc %1 %2 []) to k) (last k) (vec v)))
      {}
      (partition 2 2 (partition-by keyword? vecs)))))

(defcheck solution-e5969cb0
  (fn [sq]
    (loop [m {} c (first sq) s (rest sq) k nil]
      (if (nil? c)
        m
        (cond
          (keyword? c) (recur (merge m {c []}) (first s) (rest s) c)
          (number? c) (recur (assoc m k (conj (m k) c)) (first s) (rest s) k))))))

(defcheck solution-e68ab57b
  #(reduce (fn [m [k v]]
             (assoc m k v))
     {}
     (partition 2
       (mapcat
         (fn [[primeiro & resto :as sequencia]]
           (if (keyword? primeiro)
             (if (seq resto)
               (interpose () sequencia)
               sequencia)
             (list sequencia)))
         (partition-by keyword? %)))))

(defcheck solution-e6e5ff38
  (fn idkv [coll]
    (or (->> coll
          (partition-by keyword?)
          (partition 2)
          (map #(assoc
                 (into {} (for [x (butlast (first %))] [x []]))
                  (last (first %)) (second %)))
          (apply merge))
        {})))

(defcheck solution-e6fb399a
  (fn [x]
    (->>
      (partition-by keyword? x)
      (partition 2)
      (reduce (fn [a [k v]] (concat a (drop-last (interpose [] k)) [(last k) v])) [])
      (apply array-map)
      )
    ))

(defcheck solution-e73defd3
  (fn conv [s]
    (reduce (fn [r [k v]]
              (assoc (into r (zipmap k (repeat []))) (last k) v)) {}
      (partition 2 (partition-by keyword? s)))))

(defcheck solution-e89f9a10
  (fn accrue ([accruer current-key remainder]
              (if (empty? remainder)
                accruer
                (let [next-elem (first remainder)
                      is-key    (keyword? next-elem)
                      residue   (rest remainder)]

                  (if is-key
                    (recur (assoc accruer next-elem []) next-elem residue)
                    (let [current-value (get accruer current-key)
                          new-value     (conj current-value next-elem)]
                      (recur (assoc accruer current-key new-value) current-key residue))))))

    ([coll] (accrue {} (first coll) coll))))

(defcheck solution-eae74a96
  (fn [coll]
    (let [k-vs (fn f [[k & xs :as coll]]
                 (when (not-empty coll)
                   (let [[vs ys] (split-with #(not (keyword? %)) xs)]
                     (cons [k vs] (lazy-seq (f ys))))))]
      (into {} (k-vs coll)))))

(defcheck solution-eb278399
  #(->>
     %
     (partition-by keyword?)
     (mapcat (fn [[k :as v]] (if (keyword? k) (interpose [] v) [v])))
     (apply hash-map)))

(defcheck solution-ebb3f188
  (fn [coll]
    (into {}
      (reduce (fn [a b]
                (let [[k v] (last a)]
                  (if (keyword? b)
                    (conj a [b []])
                    (conj (pop a) [k (conj v b)]))))
        []
        coll))))

(defcheck solution-ebc75d60
  #(first (reduce (fn [[m k] x]
                    (if (keyword? x)
                      (list (assoc m x []) (conj k x))
                      (list (merge-with conj m {(last k) x}) k)))
            '({} []) %)))

(defcheck solution-ec3fe85b
  (fn f [v]
    (if (seq v)
      (let [[h & t] v
            [va r] (split-with (complement keyword?) t)]
        (assoc (f r) h va))
      {})))

(defcheck solution-ec83911e
  #((fn f [m k [e & r]]
      (if e
        (if (keyword? e)
          (f (conj m {e []}) e r)
          (f (conj m {k (conj (m k []) e)}) k r))
        m))
    {} (first %) (rest %)))

(defcheck solution-ed2f82ac
  (fn reseq [coll]
    (if (empty? coll)
      {}
      (let [k (first coll)
            [v tl] (split-with number? (rest coll))]
        (assoc (reseq tl) k v)))))

(defcheck solution-ed441b5a
  (fn [icol]
    (loop [col icol
           res {}
           ckw nil]
      (if (empty? col) res
                       (let [el (first col)]
                         (if (keyword? el)
                           (recur (rest col) (assoc res el []) el)
                           (recur (rest col) (assoc res ckw (conj (ckw res) el)) ckw)
                           )

                         )
                       )
      )
    ))

(defcheck solution-ed4afe7c
  (fn process [kvs]

    (if (empty? kvs) {}
                     (let [k (first kvs)
                           v (take-while number? (rest kvs))
                           n (drop-while number? (rest kvs))]

                       (assoc (process n) k v)))))

(defcheck solution-eddf0077
  (fn [x]
    (first
      (reduce
        (fn [[acc k] i]
          (if (keyword? i)
            (list (assoc acc i []) i)
            (list (assoc acc k (conj (acc k) i)) k)
            )
          )
        (list {} nil)
        x
        )
      )
    ))

(defcheck solution-ee503216
  (fn [l]
    (apply hash-map (mapcat
                      (fn [[k :as v]]
                        (if (keyword? k) (interpose [] v) [v]))
                      (partition-by type l)))
    ))

(defcheck solution-ee65af9b
  (fn iter
    ([s] (iter s nil {}))
    ([[a & r] l h]
     (cond
       (nil? a) h
       (keyword? a) (recur r a (merge h {a []}))
       :else (recur r l (merge-with conj h {l a}))))))

(defcheck solution-ee7a913b
  (fn f [[x & s :as c]]
    (if (seq c)
      (let [[a b] (split-with number? s)]
        (merge {x a} (f b)))
      {})))

(defcheck solution-ee7efa66
  (fn [coll]
    (if (empty? coll)
      {}
      (loop [res  {}
             tail coll]
        (if (empty? tail)
          res
          (let [key    (first tail)
                values (take-while #(not (keyword? %)) (rest tail))]
            (recur (into res {key (vec values)}) (drop (inc (count values)) tail))))))))

(defcheck solution-eeaf064e
  (fn [s]
    (letfn [(read-nums [l, s]
              (cond
                (empty? s) [l s]
                (keyword? (first s)) [l s]
                :else (read-nums (conj l (first s)) (rest s))))]
      (loop [ret-map {} start s]
        (if (empty? start) ret-map
                           (let [k (first start)
                                 v (read-nums [] (rest start))]
                             (recur (assoc ret-map k (v 0)) (v 1))
                             ))))))

(defcheck solution-eee0df81
  (fn [[kw & coll]]
    (loop [[curr & rst] coll
           key  kw
           vals []
           res  {}]
      (cond (nil? curr) (if key (assoc res key vals) {})
            (keyword? curr) (recur rst curr [] (assoc res key vals))
            :else (recur rst key (conj vals curr) res)))))

(defcheck solution-ef09030c
  (fn [a]
    (loop [m {}, rem a, k nil]
      (cond (empty? rem) m
            (keyword? (first rem)) (recur (assoc m (first rem) []) (rest rem) (first rem))
            :else (recur (update-in m [k] conj (first rem)) (rest rem) k)
            )
      )
    ))

(defcheck solution-ef2acda9
  (fn f [s]
    (if (seq s)
      (let [[k & r] s
            [v z] (split-with (complement keyword?) r)]
        (conj (f z) [k v]))
      {})))

(defcheck solution-ef663d7
  (fn [s]
    (loop [s s
           m {}]
      (if (empty? s)
        m
        (let [[k & r] s
              [v z] (split-with #(not (keyword? %)) r)]
          (recur z (conj m [k v])))))))

(defcheck solution-ef8b39e9
  #(->> %
     (partition-by number?)
     (mapcat (fn [[x :as s]] (if (number? x) [s] (interpose [] s))))
     (apply hash-map)))

(defcheck solution-efaa4e5a
  (fn [xs]
    (reduce (fn [a v]
              (if (keyword? v)
                (assoc a v [])
                (update-in a [(last (keys a))] #(conj % v)))) (sorted-map) xs)))

(defcheck solution-efbdba38
  (fn pb [s]
    (let [chunked (partition-by #(cond (keyword? %) % (number? %) "number?") s)]
      (into {} (apply merge (map #(apply hash-map %) (partition 2 (reverse
                                                                    (loop [s chunked ret nil]
                                                                      (let [fst (ffirst s)
                                                                            snd (second s)
                                                                            n   (if (keyword? (first snd)) [] snd)
                                                                            nxt (if (keyword? (first snd)) (drop 1 s) (drop 2 s))]
                                                                        ;(prn "fst" fst "snd" snd "n" n "nxt" nxt "ret" ret)
                                                                        (cond
                                                                          (not fst) ret
                                                                          (not snd) (cons [] (cons fst ret))
                                                                          :else
                                                                          (recur nxt (cons n (cons fst ret))))))))))))
    ))

(defcheck solution-f0f5a8a7
  (fn [r] (if (empty? r) {} (let [rez (let [l    (map-indexed vector (partition-by keyword? r))
                                            even (filter (fn [[i v]] (even? i)) l) odd (filter (fn [[i v]] (odd? i)) l)]
                                        (flatten (for [[i1 v1] even [i2 v2] odd :when (= 1 (- i2 i1))] (if (= 1 (count v1)) {(first v1) (into [] v2)}
                                                                                                                            (conj (map #(assoc {} % []) (drop-last v1)) (assoc {} (last v1) (into [] v2)))))))]
                              (if (= 1 (count rez)) (first rez) (apply merge rez))))))

(defcheck solution-f16079fd
  (fn [[x & xs]]
    (last
      (reduce
        (fn [[curr-key elems acc] x]
          (if (keyword? x)
            [x [] (assoc acc curr-key elems)]
            (let [new-elems (conj elems x)]
              [curr-key new-elems (assoc acc curr-key new-elems)])))
        [x [] {}] xs))))

(defcheck solution-f174089a
  #(second (reduce (fn [[k m] x]
                     (if (keyword? x)
                       [x (assoc m x [])]
                       [k (assoc m k (conj (m k) x))]))
             [nil {}] %)))

(defcheck solution-f1819daf
  (fn toromanstr [vals]
    (loop [todo     (rest vals)
           res      {}
           cur-key  (first vals)
           cur-vals []]
      (cond
        (nil? cur-key) {}
        (empty? todo) (conj res [cur-key cur-vals])
        (keyword? (first todo)) (recur (rest todo) (conj res [cur-key cur-vals]) (first todo) [])
        :else (recur (rest todo) res cur-key (conj cur-vals (first todo)))))

    ))

(defcheck solution-f1ad381a
  (fn ident [xs]
    (loop [in  xs
           out {}]
      (if (empty? in)
        out
        (let [k  (first in)
              vs (take-while #(not (keyword? %)) (rest in))]
          (recur (drop (inc (count vs)) in) (assoc out k vs)))))))

(defcheck solution-f2447ac3
  #(loop [last-k (first %) accu {} x (rest %)]
     (if (seq x)
       (if (keyword? (first x))
         (recur (first x)
           (assoc accu last-k (accu last-k []))
           (rest x))
         (let [v (accu last-k [])]
           (recur last-k
             (assoc accu last-k (conj v (first x)))
             (rest x))))
       accu)))

(defcheck solution-f2568201
  (fn [coll]
    (first (reduce
             (fn [[m k] x]
               (if (keyword? x)
                 [(assoc m x []) x]
                 [(assoc m k (conj (get m k []) x)) k]))
             [{} nil]
             coll))))

(defcheck solution-f2d796b7
  (partial (fn part-by-type [type-fn-? coll]
             (letfn [(one-key-in-one-subcoll [coll]
                       (reduce (fn [ret item]
                                 (if (and (type-fn-? (first item))
                                          (> (count item) 1))
                                   (apply conj ret (map list (interpose nil item)))
                                   (conj ret item)))
                         [] coll))]
               (let [pairs (partition 2 (map (partial keep identity) (one-key-in-one-subcoll
                                                                       (partition-by type-fn-? coll))))]
                 (reduce (fn [ret pair]
                           (conj ret [(ffirst pair) (second pair)]))
                   {} pairs))))
    keyword?))

(defcheck solution-f31889f
  (fn [x]
    (let [xs (partition-by keyword? x)]
      (apply hash-map (mapcat (fn [a] (if (keyword? (first a)) (interpose [] a) [a])) xs)))))

(defcheck solution-f34029d0
  (fn [args]
    (loop [in  args
           ret {}]
      (if (empty? in)
        ret
        (let [t (first in)]
          (recur
            (rest in)
            (if (keyword? t)
              (into ret {t []})
              (assoc ret (key (last ret)) (conj (val (last ret)) t)))))))))

(defcheck solution-f3e810fc
  (fn [s]
    (if (empty? s)
      {}
      (loop [[x & xs] (rest s) k (first s) v [] acc {}]
        (if x
          (if (keyword? x)
            (recur xs x [] (assoc acc k v))
            (recur xs k (conj v x) acc)
            )
          (assoc acc k v))
        ))

    ))

(defcheck solution-f4b27a6a
  #(into {}
     (reduce (fn [i e]
               (if (keyword? e)
                 (conj i [e []])
                 (conj (drop 1 i) [(first (first i)) (conj (second (first i)) e)]))
               )
       [] %)))

(defcheck solution-f4bd5103
  #((reduce (fn [[k j] x] (if (keyword? x) [(assoc k x j) ()] [k (conj j x)])) [{} ()] (reverse %)) 0))

(defcheck solution-f51400c8
  #(reduce (fn [m [k v]] (merge m (zipmap k (repeat (count k) [])) {(last k) v})) {} (partition 2 (partition-by keyword? %))))

(defcheck solution-f5feeccf
  (fn [v]
    (last (reduce
            #(if (keyword? %2)
               [(merge (last %1) {%2 (vec (butlast %1))})]
               (cons %2 %1))
            [{}] (reverse v)))))

(defcheck solution-f62e4759
  (fn x
    ([y] (x y {}))
    ([y z]
     (if (empty? y)
       z
       (x (drop-while number? (rest y))
         (assoc z (first y) (take-while number? (rest y))))))))

(defcheck solution-f6acdde7
  #(loop [k nil x {} y %]
     (if (empty? y) x
                    (let [[v & vs] y]
                      (if (keyword? v)
                        (recur v (conj x [v []]) vs)
                        (recur k (conj x [k (conj (x k) v)]) vs))))))

(defcheck solution-f6bfa585
  (fn [s]
    (let [[m _]
          (reduce
            (fn [[m c] n]
              (if (keyword? n)
                [(assoc m n []) n]
                [(assoc m c (conj (get m c) n)) c])) [{} nil] s)]
      m)))

(defcheck solution-f6c648f
  (fn myf2 [coll]
    (loop [coll coll, m {}]
      (let [cur-key (first coll)]
        (if (nil? cur-key) m
                           (recur (drop-while #(not (keyword? %)) (rest coll))
                             (into m {cur-key (take-while #(not (keyword? %)) (rest coll))})

                             ))))))

(defcheck solution-f6eeea8a
  (fn [a-seq]
    (letfn [
            (group [a-seq]
              (let [at (atom :0)]
                (partition-by (fn [x] (if (keyword? x) (do (reset! at x) x) @at)) a-seq)))]
      (let [maps (map (fn [a-seq] (if (= 1 (count a-seq))
                                    {(first a-seq) '()}
                                    {(first a-seq) (rest a-seq)})) (group a-seq))]
        (if (empty? maps)
          {}
          (apply merge maps))))))

(defcheck solution-f7263fd0
  (fn [v]
    (apply hash-map
      (map #(if (keyword? (first %)) (first %)
                                     (keep identity %))
        (partition-by keyword? (interpose nil v))))))

(defcheck solution-f7570067
  (fn [coll]
    (letfn [(step [c]
              (if (> (count c) 1)
                (let [key-size (count (first c))]
                  (if (= key-size 1)
                    (concat [(first (first c)) (vec (second c))] (step (drop 2 c)))
                    (concat [(first (first c)) []] (step (cons (rest (first c)) (rest c))))))))]
      (let [vec-result (step (partition-by keyword? coll))]
        (if (seq vec-result)
          (apply (partial assoc {}) vec-result)
          {})))))

(defcheck solution-f7849c25
  (fn f [xs]
    (into {} (if (empty? xs) []
                             (cons [(first xs) (vec (take-while #(not (keyword? %)) (drop 1 xs)))]
                               (lazy-seq (f (drop-while #(not (keyword? %)) (drop 1 xs)))))))))

(defcheck solution-f793b89f
  #(loop [[item & rs] % ret {} last-key item]
     (cond
       (not item) ret
       (keyword? item) (recur rs (assoc ret item []) item)
       :else (recur rs (assoc ret last-key (conj (ret last-key) item)) last-key))))

(defcheck solution-f8613e11
  (fn [args]
    (loop [m    {}
           args args]
      (if-let [[k & args] (seq args)]
        (recur (assoc m k (take-while number? args))
          (drop-while number? args))
        m))))

(defcheck solution-f861d058
  (fn [s]
    (letfn [(helper [s k result]
              (cond (empty? s) result
                    (keyword? (first s)) (helper (rest s) (first s) (assoc result (first s) []))
                    :else (helper (rest s) k (update-in result [k] (fn [v] (conj v (first s)))))))]
      (helper s nil {}))))

(defcheck solution-f86b9e11
  (fn [xs]
    (letfn [(vals [xs] (take-while #(not (keyword? %)) xs))]
      (loop [xs xs res {}]
        (if (empty? xs)
          res
          (recur (rest xs) (if (keyword? (first xs)) (assoc res (first xs) (vals (rest xs))) res)))))))

(defcheck solution-f87edc9d
  (fn [col]
    (apply hash-map
      (reduce
        #(if (keyword? %2)
           (concat %1 [%2 []])
           (concat (butlast %1) [(conj (last %1) %2)]))
        {}
        col))))

(defcheck solution-f88676f4
  (fn foo
    ([coll]
     (foo {} (first coll) (rest coll)))
    ([m k [v & coll]]
     (if-not (nil? v)
       (if (keyword? v)
         (foo (assoc m v []) v coll)
         (foo (assoc m k (concat (get m k) [v])) k coll))
       m))))

(defcheck solution-f968edfc
  (fn [xs]
    (loop [xs xs
           k  nil
           m  {}]
      (if (seq xs)
        (let [x (first xs)]
          (if (keyword? x)
            (recur (next xs) x (assoc m x []))
            (recur (next xs) k (assoc m k (conj (get m k) x)))
            ))
        m))))

(defcheck solution-fa6a7024
  #(apply hash-map
     (mapcat
       (fn [[k v]]
         (concat
           (interpose [] k)
           [v]))
       (partition 2
         (partition-by keyword? %)))))

(defcheck solution-fa8fb8be
  (fn
    [kvs]
    (into
      {}
      (map
        (fn [[[k] vs]] (vector k (vec vs)))
        (partition
          2
          (mapcat
            (fn
              [items]
              (if
               (every? keyword? items)
                (drop-last (reduce (fn [acc k] (conj (conj acc (list k)) '())) [] items))
                (list items)))
            (partition-by keyword? kvs)))))))

(defcheck solution-fa909e50
  (fn [s]
    (loop [rst s acc {}]
      (if (empty? rst)
        acc
        (let [fst (first rst)
              [vals rst] (split-with (complement keyword?) (rest rst))]
          (recur rst (assoc acc fst vals)))))))

(defcheck solution-faa9b6d4
  (fn f [x] (
              letfn [
                     (appendkey [acc el] (assoc acc el []))
                     (appendval [acc el] (assoc acc (key (last acc)) (conj (val (last acc)) el)))
                     ]
              (reduce (fn [acc el] (if (keyword? el) (appendkey acc el) (appendval acc el)))
                (sorted-map)
                x
                ))))

(defcheck solution-fab171d7
  #(dissoc (loop [[h & r] %
                  a {}
                  k nil
                  v []]
             (cond (nil? h) (assoc a k v)
                   (keyword? h) (recur r (assoc a k v) h [])
                   :else (recur r a k (conj v h)))) nil))

(defcheck solution-fb005801
  (fn [l]
    (loop [map {}, key (first l), vs [], l (rest (conj l :sentinel))]
      (if-let [c (first l)]
        (if (number? c)
          (recur map key (conj vs c) (rest l))
          (recur (conj map [key vs]) c [] (rest l)))
        map))))

(defcheck solution-fbbbfc3a
  #(apply hash-map
     (mapcat (fn [[k :as v]] (if (keyword? k) (interpose [] v) [v]))
       (partition-by type %))))

(defcheck solution-fbc2520d
  (fn [ls]
    (loop [l ls r {}]
      (if (empty? l)
        r
        (let [[k & v] l [ns rs] (split-with number? v)]
          (recur rs (assoc r k (vec ns))
            ))))))

(defcheck solution-fc498f8c
  (fn [xs]
    (->> xs
      ((fn [xs]
         (loop [acc []
                [k & rest] xs]
           (if-not k
             acc
             (let [[vs rest] (split-with (complement keyword?) rest)]
               (recur (conj acc k vs) rest))))))
      (partition 2)
      (reduce (fn [h [k v]] (assoc h k v)) {}))))

(defcheck solution-fc63aec8
  (fn [coll]
    (loop [s (seq coll) m {} kw nil]
      (if (empty? s)
        m
        (let [f (first s) k? (keyword? f)]
          (recur (rest s) (if k? (assoc m f []) (update-in m [kw] conj f)) (if k? f kw)))))))

(defcheck solution-fcccafc7
  #(first (reduce (fn [[r p] e] (if (keyword? e) [(assoc r e []) e] [(update-in r [p] conj e) p])) [{} nil] %)))

(defcheck solution-fce30ffa
  (fn [coll]
    (loop [current-keyword nil result {} rst coll]
      (if (empty? rst)
        result
        (if (keyword? (first rst))
          (recur (first rst)
            (assoc result (first rst) [])
            (rest rst))
          (recur current-keyword
            (assoc result
              current-keyword
              (conj (result current-keyword) (first rst)))
            (rest rst)))))))

(defcheck solution-fd1e6937
  (fn [lst]
    (apply hash-map (reduce (fn [acc el]
                              (if (number? el)
                                (if (coll? (last acc)) (conj (into [] (butlast acc)) (conj (last acc) el))
                                                       (conj acc [el]))
                                (apply conj acc (if (and (keyword? el) (keyword? (last acc)))
                                                  [[] el]
                                                  [el])))) [] lst))))

(defcheck solution-fd92a995
  (fn p105 [v]
    (dissoc (reduce (fn [acc x]
                      (if (keyword? x)
                        (assoc acc x [] :current x)
                        (let [key (acc :current)]
                          (assoc acc key (conj (acc key) x))))) {} v) :current)))

(defcheck solution-fd988d2c
  (fn [coll]
    ((fn [coll acc]
       (if (seq coll)
         (let [good (drop-while #(not (keyword? %)) coll)
               kw   (first good)
               [nums tail]
               (split-with #(not (keyword? %)) (next good))
               ]
           (recur tail (assoc acc kw (vec nums))))
         acc)
       )
     coll {})
    ))

(defcheck solution-fda5565f
  (fn
    [coll]
    (loop [result {} coll coll]
      (if (empty? coll)
        result
        (let [key       (first coll)
              values    (take-while number? (rest coll))
              remaining (drop-while number? (rest coll))
              ]
          (recur (conj result [key values]) remaining)
          )
        )
      )
    ))

(defcheck solution-fda73bc5
  (fn more [i]
    (if
     (empty? i)
      {}
      (let
       [start (first i)
        [end & tail]
        (if
         (number? (first (rest i)))
          (split-with
            number?
            (rest i))
          [[] (rest i)])]
        (into
          {}
          (cons
            [start (vec end)]
            (if
             (or
              (nil? (first tail))
              (empty? (first tail)))
              ()
              (more (apply
                      concat
                      tail)))))))))

(defcheck solution-fdb27090
  (fn f [s]
    (loop [k nil s s r {}]
      (let [h (first s)]
        (cond (empty? s) r
              (keyword? h) (recur h (next s) (merge r {h []}))
              :else (recur k (next s) (merge-with conj r {k h})))))))

(defcheck solution-fe20269
  (fn [i] (loop [j i result {} lastkey nil] (if (empty? j) result (if (integer? (first j)) (recur (rest j) (assoc result lastkey (conj (result lastkey) (first j))) lastkey) (recur (rest j) (assoc result (first j) []) (first j)))))))

(defcheck solution-fec668c3
  (fn vtm [v]
    (if (not (empty? v))
      (merge
        {(first v) (take-while (complement keyword?) (rest v))}
        (vtm (drop-while (complement keyword?) (rest v))))
      {})))

(defcheck solution-fed9d7f2
  reduce #(if (keyword? %2)
            (with-meta (assoc % %2 []) {0 %2})
            (update-in % [((meta %) 0)] conj %2)) {})

(defcheck solution-feed8c4a
  (fn aa [ix] (loop [x ix y {}]
                (if (empty? x) y

                               (let [z (first (keep-indexed #(if ((complement number?) %2) %1) (rest x)))]
                                 (if (= z nil) (assoc y (first x) (rest x))
                                               (recur
                                                 (drop z (rest x))
                                                 (assoc y (first x) (take z (rest x)))
                                                 )
                                               )
                                 )
                               )
                )
    ))

(defcheck solution-ff52e4af
  (fn ikv ([coll] (ikv {}
                    nil
                    []
                    coll))
    ([res cur-k cur-v coll] (if (empty? coll)
                              (if (not (nil? cur-k))
                                (assoc res cur-k cur-v)
                                res)
                              (if (nil? cur-k)
                                (ikv res (first coll) [] (rest coll))
                                (if (not (keyword? (first coll)))
                                  (ikv res cur-k (conj cur-v (first coll)) (rest coll))
                                  (ikv (assoc res cur-k cur-v) nil [] coll)))))))

(defcheck solution-ff57dd40
  (fn identKV [col]
    (into
      {}
      (reduce
        (fn [collector item]
          (if (keyword? item)
            (concat collector (vector (vector item [])))
            (let
             [
              base      (last collector),
              remaining (reverse (rest (reverse collector)))
              ]
              (concat remaining (vector (vector (first base) (conj (second base) item))))
              )
            )
          )
        []
        col
        )
      )
    ))

(defcheck solution-ffa9c3aa
  (fn [s]
    (loop [k :foo
           s s
           m {}]
      (let [q (first s)]
        (cond (empty? s) m
              (keyword? q) (recur q (rest s) (assoc m q []))
              :else (recur k (rest s) (merge-with conj m {k q})))))))

(defcheck solution-ffb9f1d
  (fn map-conv [coll]
    (->> coll
      (reduce (fn [a i]
                (if (keyword? i)
                  (vector (assoc (first a) i []) i)
                  (vector (update-in (first a) (vector (second a)) #(conj % i)) (second a))
                  ))
        [{} 0])
      first)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-105))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
