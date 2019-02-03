(ns coal-mine.problem-61
  (:require [coal-mine.checks :refer [defcheck-61] :rename {defcheck-61 defcheck}]
            [clojure.test]))

(defcheck solution-101a8e46
  #(let [n (min (count %1) (count %2))]
     (apply hash-map (interleave (take n %1) (take n %2)))))

(defcheck solution-109b71a1
  #(apply hash-map (apply concat (partition 2 (interleave % %2)))))

(defcheck solution-110e25ae
  (fn [ks vs]
    (reduce (fn [m [k v]] (assoc m k v))
      {}
      (partition 2 (interleave ks vs)))))

(defcheck solution-11190ef3
  (fn [keys vals]
    (apply hash-map (flatten (map #(list %1 %2) keys vals)))))

(defcheck solution-112c2042
  (fn [l1 l2]
    (apply merge
      (map (fn [x y] {x y}) l1 l2))))

(defcheck solution-114a92ce
  #(apply assoc {} (mapcat list % %2)))

(defcheck solution-120eaddb
  #(apply sorted-map (interleave %1 %2)))

(defcheck solution-12a0ec4
  (fn f[a b]
    (apply assoc {}
      (mapcat #(vector %1 %2) a b))))

(defcheck solution-12cd1ad9
  #(apply sorted-map (interleave % %2)))

(defcheck solution-12fc6d23
  (fn constMap
    ([k v] (constMap (- (min (count k) (count v)) 1) k v))
    ([n k v] (if
              (< n 0)
               (hash-map)
               (conj (constMap (- n 1) k v) (vector (nth k n) (nth v n)))))))

(defcheck solution-14294e82
  #(loop [ks %1, vs %2, result {}]
     (if (or (empty? ks) (empty? vs))
       result
       (recur (rest ks) (rest vs) (conj result {(first ks) (first vs)})))))

(defcheck solution-14973f67
  (fn [ks vs] (reduce (fn [a [k v]] (assoc a k v)) {} (map vector ks vs))))

(defcheck solution-14e59265
  (fn func [ks vs]
    (if (or (empty? ks) (empty? vs))
      {}
      (assoc (func (rest ks) (rest vs)) (first ks) (first vs)))))

(defcheck solution-15b7f541
  (fn[s1, s2](into {} (map #(vector % %2) s1 s2))))

(defcheck solution-15d73246
  (fn [ks vs] (reduce (fn [m [k v]] (assoc m k v)) {} (map list ks vs))))

(defcheck solution-15e6e8ab
  #(apply assoc (cons {} (interleave %1 %2))))

(defcheck solution-165e128f
  (fn [key val] (apply hash-map (interleave key val))))

(defcheck solution-16a150c9
  (fn [xs ys]
    (reduce #(assoc %1 (first %2) (last %2)) {} (map #(list %1 %2) xs ys))))

(defcheck solution-188617a2
  (fn [ks values]
    (apply hash-map (interleave ks values))))

(defcheck solution-18965cc4
  (fn[a,b](apply hash-map (interleave (take (min (count a) (count b)) a) (take (min (count a) (count b)) b)))))

(defcheck solution-189fc0b5
  #(apply assoc {} (interleave %1 %2 )))

(defcheck solution-1a02eebf
  (fn [ks vs]
    (apply hash-map (interleave ks vs))
    ))

(defcheck solution-1ac8c0fd
  #(into {} (map hash-map % %2)))

(defcheck solution-1af9813f
  (fn f [keys vals]
    (loop [k keys v vals acc {}]
      (if (or (empty? k) (empty? v))
        acc
        (recur (rest k) (rest v) (assoc acc (first k) (first v)))))))

(defcheck solution-1b128a5b
  #(reduce (fn [s p] (assoc s (first p) (second p))) {} (map (fn [k v] [k v]) %1 %2)))

(defcheck solution-1b8ed8c1
  (fn [ks vs] (into {} (map (fn [a b] (vector a b)) ks vs))))

(defcheck solution-1bd480a5
  (fn make-kv-map
    [keys values]
    (loop [[k & ks] keys
           [v & vs] values
           accum {}]
      (if (or (nil? ks) (nil? vs))
        (assoc accum k v)
        (recur ks vs (assoc accum k v))))))

(defcheck solution-1c63083b
  (comp (partial into {}) (partial map vector)))

(defcheck solution-1cae9fc2
  (fn [keys vals]
    (reduce #(assoc %1 (keys %2) (vals %2)) {} (range (min (count keys) (count vals))))))

(defcheck solution-1d01e66
  (fn [ks vs]
    (let [pairs (map list ks vs)]
      (reduce (fn [acc elt] (assoc acc (first elt) (second elt)))
        {} pairs))))

(defcheck solution-1d430c0f
  (fn
    [vkeys vvalues]
    (loop [vk vkeys vv vvalues acc {}]
      (if (or (empty? vk) (empty? vv))
        acc
        (recur (rest vk) (rest vv) (assoc acc (first vk) (first vv)))))))

(defcheck solution-1d9f8a24
  (fn [ks vs]
    (into {} (map #(vector %1 %2) ks vs))))

(defcheck solution-1db00289
  (fn M [ks vs]
    (if (or (empty? ks) (empty? vs))
      {}
      (merge (hash-map (first ks) (first vs)) (M (rest ks) (rest vs))))))

(defcheck solution-1e141a7d
  (fn [x y]
    (reduce #(assoc %1 (first %2) (second %2)) {} (map #(vector %1 %2) x y))))

(defcheck solution-1e58695
  (fn myZipmap [x y] (apply assoc {} (interleave x y))))

(defcheck solution-1eacf140
  (fn [x y]
    (reduce conj (map #(assoc {} %1 %2 ) x y))))

(defcheck solution-1ecd2e8b
  (fn [la lb] (apply assoc {} (interleave la lb))))

(defcheck solution-1f1bdfb2
  (fn myassoc
    [seq1 seq2]
    (loop
     [	myseq1 seq1

      myseq2 seq2

      cnt (if (<= (count seq1) (count seq2)) (count seq1) (count seq2) )

      mymap nil]

      (if (= 0 cnt)
        mymap
        (recur (rest myseq1) (rest myseq2) (dec cnt) (assoc mymap (first myseq1) (first myseq2)))
        )



      )))

(defcheck solution-20024fb
  (fn [ks vs]
    (apply hash-map
      (apply concat
        (map list ks vs)))))

(defcheck solution-21ef3917
  (fn zm [[k & ks] [v & vs]]
    (if (and k v)
      (merge {k v} (zm ks vs))
      {})))

(defcheck solution-223614cd
  #(reduce merge (map hash-map % %2)))

(defcheck solution-2269227
  (fn [a b]
    (apply hash-map (interleave a b))))

(defcheck solution-22a5395
  (fn [ks vs]
    (->> (map vector ks vs)
      (reduce (fn [m [k v]]
                (assoc m k v))
        {}))))

(defcheck solution-22ce91be
  #(apply hash-map (apply concat (map vector %1 %2))))

(defcheck solution-22e4bd7b
  (fn [k v]
    (reduce #(assoc % (first %2) (last %2))
      {}
      (partition 2 (interleave k v)))))

(defcheck solution-22fd57
  (fn [keys vals]
    (last
      (reduce
        #(list
           (rest (first %1))
           (assoc (last %1) (ffirst %1) %2))
        [keys (hash-map)]
        (take (count keys) vals)))))

(defcheck solution-2335428f
  ; from ClojureDocs on interleave
  #(apply assoc {} (interleave %1 %2)))

(defcheck solution-238500c5
  (fn
    [k v]
    (reduce
      conj
      (apply
        map hash-map
        [k v]))))

(defcheck solution-2485fd3
  (fn [keys vals]
    (loop [result {} k keys v vals]
      (if (or (empty? k) (empty? v))
        result
        (recur (into result [[(first k) (first v)]]) (rest k) (rest v))))))

(defcheck solution-24f1e712
  (fn [x y] (apply hash-map (interleave x y))))

(defcheck solution-24ff20f7
  (fn create-map
    [ks vs]
    (apply hash-map (flatten (map #(list %1 %2) ks vs)))))

(defcheck solution-25249237
  (fn [a b] (apply array-map (interleave a b))))

(defcheck solution-25788a9f
  (fn [ks vs]
    (into {}
      (map #(vector %1 %2) ks vs))))

(defcheck solution-2652ba79
  (fn f [keys vals]
    (if (or (empty? keys) (empty? vals))
      {}
      (let [[k & ks] keys [v & vs] vals]
        (assoc (f ks vs) k v)))))

(defcheck solution-26790f27
  (fn [col1 col2]
    (into {} (map #(vector % %2) col1 col2))

    ))

(defcheck solution-26db072d
  (comp (partial apply hash-map) (partial mapcat #(list %1 %2))))

(defcheck solution-27514571
  (fn [& colls] (apply hash-map (apply mapcat list colls))))

(defcheck solution-27b5deb3
  (fn [xs ys] (apply (partial assoc {})(interleave xs ys))))

(defcheck solution-27fca4ae
  (fn [xs ys] (apply hash-map (mapcat list xs ys))))

(defcheck solution-28b1bbe6
  (fn [ks vs] (loop [m {} ks ks vs vs] (if (or (empty? ks) (empty? vs)) m (recur (conj m [(first ks) (first vs)]) (rest ks) (rest vs))))))

(defcheck solution-28faa3be
  ;; fastest solution i can think of :)
  #(apply hash-map (interleave % %2)))

(defcheck solution-2a78677a
  (fn [c1 c2] (into {} (mapv vector c1 c2))))

(defcheck solution-2adc05fe
  (fn cust-zipmap [k v]
    (apply assoc {}(interleave k v))))

(defcheck solution-2ae78db9
  (fn [k v]
    (reduce
      #(conj % (vec %2))
      {}
      (partition 2 (interleave k v)))))

(defcheck solution-2b9e5ef7
  (fn [k v]
    (loop [k k v v acc {}]
      (if (or (empty? k) (empty? v))
        acc
        (recur (rest k) (rest v) (assoc acc (first k) (first v)))))))

(defcheck solution-2c2195
  (fn [c1 c2] (apply hash-map (interleave c1 c2) ) ))

(defcheck solution-2d2f6302
  #(reduce conj {} (map (comp vec list) %1 %2)))

(defcheck solution-2e153895
  (fn my-zipmap [keys vals]
    (into {} (map vector keys vals))))

(defcheck solution-2e2a1253
  #(apply array-map (interleave %1 %2)))

(defcheck solution-2ed309d1
  (fn [k v] (into {} (map vector k v))))

(defcheck solution-2f436fbe
  (fn my-zipmap
    [ks vs]
    (if (or (empty? ks) (empty? vs))
      {}
      (assoc (my-zipmap (rest ks) (rest vs)) (first ks) (first vs)))))

(defcheck solution-30503da4
  (fn [ks vs]
    (apply merge (map #(assoc {} %1 %2) ks vs))))

(defcheck solution-3080fdc8
  #(into {} (map (fn [x y] [x y]) %1 %2)))

(defcheck solution-30914c66
  #(into {} (map (fn [k v] [k v]) %1 %2)))

(defcheck solution-309b7c0f
  (fn [k v]
    (loop [m {}
           k k
           v v]
      (if (and k v)
        (recur (assoc m (first k) (first v))
          (next k)
          (next v))
        m))))

(defcheck solution-309c800f
  (fn [x y]
    (into {}
      (map #(vector %1 %2) x y))))

(defcheck solution-30b78496
  (fn [ks vs]
    (apply hash-map (interleave ks vs))))

(defcheck solution-30c53542
  (fn my-map
    [colla collb]
    (into {} (map #(vector % %2) colla collb))))

(defcheck solution-31570e97
  (fn zipmap-2 [k v]
    (loop [ks k vs v m {}]
      (if (or (empty? ks) (empty? vs))
        m
        (recur (rest ks) (rest vs) (into m {(first ks) (first vs)}))))))

(defcheck solution-3269ab0a
  (fn
    [keys values]
    (loop [a {}
           ks keys
           vs values]
      (if (and ks vs)
        (recur (assoc a (first ks) (first vs)) (next ks) (next vs))
        a))))

(defcheck solution-32c9113d
  #(into {} (map  vector % %2)))

(defcheck solution-33cdb323
  (fn[s1 s2]
    (apply hash-map (mapcat (fn[a b] [a b]) s1 s2))))

(defcheck solution-34081eed
  ;(fn [ks vs]
  ;  (loop [m {} ks ks vs vs] ; you should seq
  ;    (if (or (empty? ks) (empty? vs))
  ;      m
  ;      (recur (assoc m (first ks) (first vs))
  ;             (rest ks)
  ;             (rest vs)))))

  ; see (source zipmap) :-)
  ; empty? is fine but simply testing ks/vs is faster. However, you
  ; have to use next instead of rest and (seq ks)/(seq vs) initially,
  ; in case either is empty.
  (fn [ks vs]
    (loop [m {} ks (seq ks) vs (seq vs)]
      (if (and ks vs)
        (recur (assoc m (first ks) (first vs))
          (next ks)   ; careful, not rest
          (next vs))
        m))))

(defcheck solution-349f0231
  (fn [a b]
    (loop [h {} k a v b]
      (if (or (empty? k) (empty? v)) h
                                     (recur (assoc h (first k) (first v))
                                       (rest k) (rest v))))))

(defcheck solution-355b1f9e
  #(apply hash-map (flatten (map vector % %2))))

(defcheck solution-35728e07
  (fn [ks vs] (into {} (map #(hash-map %1 %2) ks vs))))

(defcheck solution-3797c2d7
  (fn [x y] (into {} (map (fn [a b] [a b]) x y))))

(defcheck solution-37d9b9b4
  (fn [a b](into (sorted-map) (mapv (fn[x y][x y]) a b) )))

(defcheck solution-3951dad7
  #(apply merge (map hash-map % %2)))

(defcheck solution-39fb133b
  (fn [keys vals]
    (loop [k keys v vals r {}]
      (if (or (empty? k) (empty? v))
        r
        (recur (rest k) (rest v) (merge r {(first k) (first v)}))))))

(defcheck solution-3a21727c
  (fn [x y]
    (into {} (mapcat #(hash-map %1 %2) x y))))

(defcheck solution-3a3cbc62
  (fn my-zipmap [keys vals]
    (loop [ks keys
           vs vals
           res {}]
      (if (or (empty? ks) (empty? vs))
        res
        (recur (rest ks) (rest vs) (assoc res (first ks) (first vs)))))))

(defcheck solution-3ce1501c
  (fn my-zipmap
    [ks vs]
    (apply hash-map (interleave ks vs))))

(defcheck solution-3cec8d98
  (fn zip-map
    ([keys values] (zip-map keys values {}))
    ([keys values map]

     (cond
       (or (empty? keys) (empty? values)) map
       :else (let
              [key (first keys) val (first values)
               nkeys (rest keys) nvals (rest values) ]

               (zip-map nkeys nvals (assoc map key val)))))))

(defcheck solution-3d0ed7cf
  #( apply hash-map  (flatten (partition 2 (interleave %1 %2))) ))

(defcheck solution-3e466fc8
  #((fn mkmp [ks vs m]
      (if (or (empty? ks) (empty? vs))
        m
        (assoc (mkmp (rest ks) (rest vs) m) (first ks) (first vs)))) %1 %2 {}))

(defcheck solution-3e894bf1
  (fn [a b]
    (apply hash-map
      (interleave a b))
    ))

(defcheck solution-3eee04a3
  (fn my-zipmap [ls rs]
    (loop [an {}, ls ls, rs rs]
      (if (or (empty? ls) (empty? rs))
        an
        (recur (conj an [(first ls)
                         (first rs)])
          (rest ls) (rest rs))))))

(defcheck solution-3f03275a
  (fn [x y]
    (apply conj {}
      (map #(vector %1 %2) x y))))

(defcheck solution-3f14df19
  (fn [a b] (into {} (map #(vector %1 %2) a b))))

(defcheck solution-3f86b4d1
  (fn conm
    ([lst1 lst2] (conm {} lst1 lst2))
    ([m lst1 lst2] (if (or (empty? lst1) (empty? lst2)) m (conm (conj m [(first lst1) (first lst2)]) (next lst1) (next lst2))))
    ))

(defcheck solution-400dfbd1
  (fn [x y]
    (reduce (fn [m [k v]]
              (into m {k v})) {} (map vector x y))
    ))

(defcheck solution-41961917
  (fn [ks vs] (loop [ks ks vs vs acc {}]
                (if (not (or (empty? ks) (empty? vs)))
                  (recur (rest ks) (rest vs) (conj acc [(first ks) (first vs)]))
                  acc))))

(defcheck solution-41974ea9
  (fn [c1 c2]
    (loop [c1 c1
           c2 c2
           r {}]
      (if (some empty? [c1 c2])
        r
        (recur (rest c1) (rest c2) (conj r [(first c1) (first c2)]))))))

(defcheck solution-41c072b4
  (fn [ks, vs]
    (->> (map vector ks vs)
      (into {}))))

(defcheck solution-41dd6f76
  (fn [coll-a coll-b]
    (reduce (fn [acc [a b]] (assoc acc a b)) {} (map (fn [a b] [a b]) coll-a coll-b))
    ))

(defcheck solution-420d3ec1
  (fn [keys values]
    (apply assoc {} (interleave keys values))))

(defcheck solution-4274ed7e
  (fn my-zipmap [ks vs] (into {} (map vector ks vs))))

(defcheck solution-43217f8f
  (fn mappy [keylist vallist]
    (if (or (empty? keylist) (empty? vallist))
      {}
      (merge (hash-map (first keylist) (first vallist)) (mappy (rest keylist) (rest vallist))))))

(defcheck solution-434bd948
  #(apply hash-map (flatten (interleave % %2))))

(defcheck solution-43e32566
  (fn zm [l1, l2]
    (cond
      (empty? l1) {}
      (empty? l2) {}
      :else (let [m (zm (rest l1) (rest l2))]
              (assoc m (first l1) (first l2))
              )
      )))

(defcheck solution-44418950
  (fn zm [xs ys] (if (or (empty? xs) (empty? ys)) {} (assoc (zm (rest xs) (rest ys)) (first xs) (first ys)))))

(defcheck solution-445037a1
  (fn my-zipmap [coll-a coll-b]
    (apply hash-map (interleave coll-a coll-b))))

(defcheck solution-446b5749
  (fn [x y]
    (loop [_keys x
           _values y
           return {}]
      (if (or (nil? (first _keys))
              (nil? (first _values)))
        return
        (recur (rest _keys)
          (rest _values)
          (assoc return (first _keys) (first _values)))))))

(defcheck solution-4554b884
  #(apply conj (map hash-map %1 %2)))

(defcheck solution-45bd4b55
  (fn [x y] (into {} (map (fn [t z] [t z]) x y))))

(defcheck solution-4655849b
  #(reduce (fn [h v] (apply assoc h v)) {} (partition 2 (interleave %1 %2))))

(defcheck solution-46692990
  (partial
    #(if (or (empty? %2) (empty? %3)) %1
                                      (recur (assoc %1 (first %2) (first %3)) (rest %2) (rest %3))) {}))

(defcheck solution-46ff8ccd
  (fn [k v]
    (into {} (map vector k v))))

(defcheck solution-4887a010
  (fn [a b] (reduce #(apply assoc %1 %2) {} (map vector a b))))

(defcheck solution-48adb4db
  #(into {} (map hash-map %1 %2)))

(defcheck solution-4909ef95
  #(apply hash-map
     (mapcat list % %2)))

(defcheck solution-4ba7303c
  (fn[k v]
    (loop [map {}
           ks (seq k)
           vs (seq v)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
          (next ks)
          (next vs))
        map))))

(defcheck solution-4c4ace9a
  (fn f [k v]
    (if (or (empty? k) (empty? v))
      {}
      (assoc (f (rest k) (rest v))
        (first k) (first v)))))

(defcheck solution-4c631f54
  (fn [k v] (apply hash-map (interleave k v))))

(defcheck solution-4dd33e16
  (fn [colla collb]
    (loop [result {}
           a colla
           b collb]
      (if (or (empty? a) (empty? b))
        result
        (recur (into result {(first a) (first b)}) (rest a) (rest b))))))

(defcheck solution-4e2a9e98
  (fn [c1 c2] (reduce (fn [m [k v]] (assoc m k v)) {} (map (fn [a b] [a b]) c1 c2))))

(defcheck solution-4e4466ff
  #(into {} (apply map vector [% %2])))

(defcheck solution-4ea0e209
  (fn [x y] (apply hash-map
              (interleave
                x
                y
                )
              )

    ))

(defcheck solution-4ec5d9c7
  (fn mapping
    [[x & xs] [y & ys]]
    (if (or (not x) (not y))
      nil
      (merge {x y} (mapping xs ys)))))

(defcheck solution-4f010e4d
  (fn my-zip [a b]
    (if (or (empty? a) (empty? b))
      {}
      (assoc (my-zip (rest a) (rest b)) (first a) (first b)))))

(defcheck solution-4f091624
  #(apply hash-map (apply interleave %&)))

(defcheck solution-4f35f6d9
  (fn zm [ks, vs]
    (if (= 1 (min (count ks) (count vs)))
      {(first ks) (first vs)}
      (assoc (zm (rest ks) (rest vs)) (first ks) (first vs)))))

(defcheck solution-4f3d651e
  (fn my-zipmap
    [coll1 coll2]
    (apply conj {} (map hash-map coll1 coll2))))

(defcheck solution-4fd74410
  (fn map-cons [l1 l2]
    (if (or (empty? l1) (empty? l2))
      {}
      (assoc (map-cons (rest l1) (rest l2)) (first l1) (first l2)))))

(defcheck solution-4fdf9f8
  (fn zip-map [ks vs]             ;&#27492;&#22788;&#24517;&#39035;&#35201;&#20351;&#29992;ks vs&#21578;&#35785;clojure&#26159;&#38190;&#23545;&#20540;&#24418;&#24335;&#30340;  interleave&#26159;&#23558;&#31532;&#19968;&#20010;&#21442;&#25968;&#21644;&#31532;&#20108;&#20010;&#21442;&#25968;&#36827;&#34892;&#38190;&#23545;&#36716;&#25442;&#65292;&#28982;&#21518;&#29992;apply hash-map&#23558;&#20182;&#20204;&#35013;&#36827;{}
    (apply hash-map (interleave ks vs))))

(defcheck solution-4fe541ac
  (fn this [kv vv]
    (cond (or (= (count kv) 0)
              (= (count vv) 0)) {}
          :else (conj (this (rest kv) (rest vv))
                  {(first kv) (first vv)}))))

(defcheck solution-50d4d19e
  (fn [ks vs]
    (reduce #(apply assoc %1 %2)
      {}
      (partition 2 (interleave ks vs)))))

(defcheck solution-5146422c
  #(into {} (map (partial assoc {}) %1 %2)))

(defcheck solution-51887b81
  (fn [xs ys] (into {} (map #(vector %1 %2) xs ys))))

(defcheck solution-5236f807
  #(apply merge (map hash-map %1 %2)))

(defcheck solution-5238ea07
  (fn [ks vs]
    (reduce #(assoc %1 (%2 0) (%2 1)) {} (map vector ks vs))))

(defcheck solution-524e1dc6
  (fn [l1 l2] (into {} (map (fn [x1 x2] [x1 x2]) l1 l2))))

(defcheck solution-5314ca79
  (fn [k v]
    (apply assoc {}
      (interleave k v))))

(defcheck solution-536495da
  (fn my-zipmap [ks vs]
    (apply assoc {} (interleave ks vs))))

(defcheck solution-5391462
  (fn [a b]
    (apply assoc {} (interleave a b))))

(defcheck solution-53ca1514
  (fn mc [v1 v2]
    (loop [[k :as s1] v1, [v :as s2] v2, result {}]
      (if (and k v)
        (recur (rest s1) (rest s2) (assoc result k v))
        result))))

(defcheck solution-549f89e9
  #(apply hash-map (mapcat list %1 %2)))

(defcheck solution-54b6627
  #(apply (partial assoc {}) (interleave %1 %2)))

(defcheck solution-554dc7bf
  (fn __ [k v] (into {} (map #(hash-map %1 %2) k v))))

(defcheck solution-559928e6
  (fn seqs-to-map [s1 s2]
    (into {} (map #(identity [%1 %2]) s1 s2))))

(defcheck solution-55e5790
  #(apply hash-map(interleave %1 %2)))

(defcheck solution-562e346a
  (fn [as bs]
    ((fn helper [accu xs ys]
       (if (or (empty? xs) (empty? ys))
         accu
         (recur (assoc accu (first xs) (first ys))
           (rest xs) (rest ys))
         )
       ) (hash-map ) as bs)
    ))

(defcheck solution-573873a3
  (fn [a b] (apply merge (map hash-map a b))))

(defcheck solution-578c74c6
  (fn [& args] (apply hash-map (apply interleave args))))

(defcheck solution-57f39df9
  ;not sure if this index i is what should i do in func programming
  (fn [keys values]
    (loop [result {} i 0]
      (if (or (>= i (count values)) (>= i (count keys)))
        result
        (recur (assoc result (nth keys i) (nth values i)) (inc i))
        )
      )
    ))

(defcheck solution-586757fe
  (fn [c1 c2] (reduce merge (map (fn [k v] {k v}) c1 c2))))

(defcheck solution-59404529
  #(into {} (map (fn [key val] [key val]) %1 %2)))

(defcheck solution-59602623
  #(reduce conj (map hash-map  % %2)))

(defcheck solution-597a0f90
  (fn [k v] (reduce #(conj %1 %2) {} (map #(vector %1 %2) k v))))

(defcheck solution-59a38ca
  #(loop [l1 % l2 %2 m {}]
     (if (or (empty? l1) (empty? l2))
       m
       (recur (rest l1) (rest l2)
         (assoc m (first l1) (first l2))))))

(defcheck solution-59f39bd4
  (fn [keys vals]
    (into {} (map vector keys vals))))

(defcheck solution-5a47a969
  (fn [vk vv]
    (apply hash-map (interleave vk vv))
    ))

(defcheck solution-5b0eb97c
  #(apply conj {} (map vector %1 %2)))

(defcheck solution-5b5828dc
  (fn [s1 s2]
    (loop [s1 s1
           s2 s2
           result {}]
      (if (or (empty? s1) (empty? s2))
        result
        (recur (rest s1)
          (rest s2)
          (conj {(first s1) (first s2)} result))))))

(defcheck solution-5b791196
  (fn [x y]
    (into {} (map vector x y))))

(defcheck solution-5cee8034
  #(apply merge (map (partial assoc {}) %1 %2)))

(defcheck solution-5db5afe4
  (fn [a b]
    (apply hash-map (interleave a b))))

(defcheck solution-5fb331a1
  ;#(into {} (map vector %1 %2))
  #(apply assoc {} (interleave %1 %2)))

(defcheck solution-60101b71
  (fn [ks vs]
    (reduce (fn [m [k v]] (assoc m k v))
      {}
      (partition 2 (interleave ks vs)))
    ))

(defcheck solution-60124037
  (fn map-construct
    [xs ys]
    (apply hash-map (mapcat list xs ys))))

(defcheck solution-60dc40a3
  (fn zipm

    ([v1 v2 m] (if (or (empty? v1)(empty? v2)) m
                                               (zipm (rest v1) (rest v2) (conj m {(first v1) (first v2)}) ) ))
    ([v1 v2] (zipm v1 v2 {}))
    ))

(defcheck solution-60f78e12
  (fn zipmap' [a b]
    (->> (map list a b)
      (reduce #(apply assoc %1 %2) {}))))

(defcheck solution-6138a08b
  (fn create-maps [keys vals]
    (into {} (map vector keys vals))))

(defcheck solution-615aa8c2
  (fn [x y]
    (apply array-map (interleave x y))))

(defcheck solution-616fafc
  (fn [lk lv]
    (loop [m {}
           mk lk
           mv lv]
      (if-not (and mk mv)
        m
        (recur (into m {(first mk) (first mv)})
          (next mk) (next mv))))))

(defcheck solution-625e06c4
  (fn [s t] (into {} (map #(vector %1 %2) s t))))

(defcheck solution-6283ea9b
  (fn [x y]
    (apply assoc {}
      (interleave x y))))

(defcheck solution-62e41a82
  (fn [k, v]
    (apply merge (map #(assoc {} %1 %2) k v))))

(defcheck solution-6331d09c
  #(apply hash-map
     (let [col (interleave %1 %2)]
       (if (even? (count col))
         col
         (butlast col)))))

(defcheck solution-63975f2c
  (fn [ks vs]
    (loop [ks ks
           vs vs
           acc {}]
      (if-let [[k & ks-more] ks]
        (if-let [[v & vs-more] vs]
          (recur ks-more vs-more (assoc acc k v))
          acc)
        acc))))

(defcheck solution-64c0c6bf
  (fn
    [keys values]
    (loop [keys keys values values m {}]
      (if (and  (not (empty? keys)) (not (empty? values)))
        (recur (rest keys) (rest values) (assoc m (first keys) (first values)))
        m
        )

      )))

(defcheck solution-6622cffc
  (fn [ks vs]
    (let [s (min (count ks) (count vs))]
      (reduce #(assoc % (get ks %2)
                        (get vs %2))
        {} (range s)))))

(defcheck solution-66455775
  (fn x [r [a & at] [b & bt]] (let [rr (conj r {a b})] (if (= false (empty? at) (empty? bt)) (x rr at bt) rr))) {})

(defcheck solution-66e78db1
  #(reduce conj(map (fn[a b]{a b})%1%2)))

(defcheck solution-674dd695
  (fn [x y] (into {} (map vec (partition 2 (interleave x y))))))

(defcheck solution-675d5f79
  #(into {} (for[i (range 0 (min (count %) (count %2)))](hash-map (nth % i) (nth %2 i)))))

(defcheck solution-67e70ec8
  (fn mp [key-seq val-seq]
    (apply hash-map (interleave key-seq val-seq))))

(defcheck solution-685a2fbe
  (fn zipmap* [ks vs]
    (let [impl (fn impl [acc ks vs]
                 (if (or (empty? ks) (empty? vs))
                   acc
                   (recur (conj acc [(first ks) (first vs)])
                     (rest ks) (rest vs))))]
      (impl {} ks vs))))

(defcheck solution-6867d773
  (fn [ks vs] (into {} (map vector ks vs))))

(defcheck solution-6892e64a
  (fn zm [[k & ks] [v & vs]]
    (into {} (cons {k v} (if (or (nil? ks) (nil? vs)) nil (zm ks vs))))))

(defcheck solution-68d7c659
  (fn zipmap'
    ([keys values] (zipmap' keys values {}))
    ([keys values acc]
     (if (or (empty? keys) (empty? values))
       acc
       (recur (rest keys) (rest values)
         (conj acc [(first keys) (first values)]))))))

(defcheck solution-693ca6c3
  (fn [& xs] (into {} (apply map (fn [a b] [a b]) xs))))

(defcheck solution-693ea603
  (fn [seq1 seq2]
    (->> (map #(assoc {} %1 %2) seq1 seq2)
      (apply merge ))))

(defcheck solution-6947eecd
  (fn zipmap-
    [keys vals]
    "61. Write a function which takes a vector of keys and a vector of values and constructs a map from them."
    (apply hash-map (interleave keys vals))))

(defcheck solution-69afaaa4
  (fn [ks vs]
    (into {} (map (comp vec list) ks vs))))

(defcheck solution-69e7218a
  (fn map-constr [x y]
    (if (or (empty? (rest x)) (empty? (rest y)))
      (hash-map (first x) (first y))
      (merge (hash-map (first x) (first y))
        (map-constr (rest x) (rest y))))))

(defcheck solution-6a226853
  (fn [keyz values]
    (loop [out {}
           k keyz
           v values]
      (if (or (empty? k) (empty? v))
        out
        (recur (conj out (hash-map (first k) (first v)))
          (drop 1 k)
          (drop 1 v))))))

(defcheck solution-6ae8869f
  (fn [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
          (next ks)
          (next vs))
        map))))

(defcheck solution-6aeeb69c
  (fn [v1 v2]
    (let [v (interleave v1 v2)]
      (apply assoc {} v))))

(defcheck solution-6d345818
  (fn [keys values] (reduce into {} (map #(hash-map %1 %2) keys values))))

(defcheck solution-6e5e80d6
  (fn zm [k v]
    (apply hash-map (interleave k v))
    ))

(defcheck solution-6e82f7f4
  #(reduce merge (map (fn [x y] {x y}) %1 %2)))

(defcheck solution-6f915028
  (fn[keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
          (next ks)
          (next vs))
        map))))

(defcheck solution-6fb67987
  (fn f [ks vs]
    (apply conj {} (map #(vector %1 %2) ks vs))))

(defcheck solution-6fbd5d40
  #(apply hash-map (mapcat (fn [k v] [k v]) % %2)))

(defcheck solution-7008672d
  (fn [ks vs] (reduce #(conj %1 %2) {} (map vector ks vs))))

(defcheck solution-70a55d9a
  (fn [xs ys] (into {} (map vector xs ys))))

(defcheck solution-718d3cf3
  (comp (partial apply hash-map) interleave))

(defcheck solution-72992e5c
  #(into {} (map vector %1 %2 )))

(defcheck solution-72d8369a
  (fn [k v]
    (into {} (map #(vector %1 %2) k v))))

(defcheck solution-742b7c1d
  #(into {} (mapcat (comp list vec list) %1 %2)))

(defcheck solution-7460714c
  #(into {} (map vec (partition 2 (apply interleave %&)))))

(defcheck solution-74d9337d
  (fn make-map[a-seq b-seq]
    (apply hash-map (mapcat #(list %1 %2) a-seq b-seq))
    ))

(defcheck solution-76099e3d
  (fn [s1 s2] (reduce conj (map (fn[a b] {a b})  s1 s2))))

(defcheck solution-7633d8a9
  (fn [a b]
    (apply assoc
      (concat [{}] (vec (interleave a b)))
      )
    ))

(defcheck solution-765f9028
  (fn [k v]
    (loop [keys k
           vals v
           res {}]
      (if (or (empty? keys) (empty? vals)) res
                                           (recur (rest keys) (rest vals) (assoc res (first keys) (first vals)))))))

(defcheck solution-774ff29
  (fn [keys vals]
    (apply hash-map (interleave keys vals))))

(defcheck solution-7788a727
  (fn map [v1 v2]
    (apply array-map (vec (mapcat #(list %1 %2) v1 v2)))
    ))

(defcheck solution-77d8b21b
  (fn [kys vls]
    (reduce #(assoc %1 (first %2) (second %2)) {}
      (partition 2 (interleave kys vls)))))

(defcheck solution-7890814b
  (fn zip-map [ks vs]
    (into {} (map vector ks vs))))

(defcheck solution-792138b0
  (fn [l1 l2]
    (loop [news {} tmp1 l1 tmp2 l2]
      (if (or (empty? tmp1) (empty? tmp2))
        news
        (recur (into news {(first tmp1) (first tmp2)}) (rest tmp1) (rest tmp2))))))

(defcheck solution-794abd28
  (fn my_zip
    ([ks vs]
     (my_zip ks vs {}))
    ([ks vs result]
     (if (or (empty? ks) (empty? vs))
       result
       (recur (rest ks) (rest vs) (conj result (assoc {} (first ks) (first vs))))))))

(defcheck solution-79ff5a69
  #(into{}(map vector %1 %2)))

(defcheck solution-7a25947b
  #(into {} (map (fn [a b] {a b})  %1 %2)))

(defcheck solution-7a72e86d
  (fn [xs ys]
    (apply assoc {}
      (interleave xs ys))))

(defcheck solution-7a74beb8
  ( fn [k v] (loop [ktmp k vtmp v acc {}] (if (or (empty? ktmp) (empty? vtmp)) acc
                                                                               (recur (rest ktmp) (rest vtmp) (assoc acc (first ktmp) (first vtmp)))))))

(defcheck solution-7a878ad5
  (fn [l1 l2] (apply array-map (interleave l1 l2))))

(defcheck solution-7ac6b41a
  (fn [a b] (reduce #(assoc %1 (first %2) (second %2)) {} (map vector a b))))

(defcheck solution-7b0706da
  (fn [ks vs]
    (loop [h (hash-map)
           rks ks
           rvs vs]
      (if (or (empty? rks) (empty? rvs)) h
                                         (recur (assoc h (first rks) (first rvs)) (rest rks) (rest rvs))))))

(defcheck solution-7b81216b
  (fn map-construction [coll1 coll2]
    (loop [result {}
           coll1 coll1
           coll2 coll2]
      (if(or(empty? coll1)
            (empty? coll2))
        result
        (let[f1 (first coll1)
             f2 (first coll2)]
          (recur
            (assoc result f1 f2)
            (rest coll1)
            (rest coll2)))))))

(defcheck solution-7de41252
  #(reduce into (map hash-map % %2)))

(defcheck solution-7e1c0167
  (fn ! [x y]
    (if (or (empty? x) (empty? y))
      {}
      (apply conj
        {(first x) (first y)}
        (! (rest x) (rest y))
        )
      )))

(defcheck solution-7e68ee46
  (fn zpmp [kys vls]
    (apply hash-map (interleave kys vls))))

(defcheck solution-7e81aa33
  (fn [ks vs]
    (apply hash-map (interleave ks vs))))

(defcheck solution-7ed04131
  (fn [k, v]
    (reduce #(assoc %1 (%2 0) (%2 1)) {} (map vector k v))
    ))

(defcheck solution-7f7ed580
  (fn [ks vs]
    (reduce #(assoc %1 (first %2) (last %2)) {} (map vector ks vs))))

(defcheck solution-7f878c17
  #(into {} (map (fn [x y] {x y}) %1 %2)))

(defcheck solution-80cb83b2
  (fn create-map
    ([x y]
     (create-map (rest x) (rest y) (vector (first x) (first y))))
    ([x y z]
     (if (some #(= 0 (count %)) (list x y))
       (apply hash-map z)
       (recur (rest x) (rest y) (conj z (first x) (first y)))))))

(defcheck solution-81052c6c
  (fn mc [xs ys]
    (apply hash-map (flatten (map vector xs ys)))))

(defcheck solution-811997d3
  (fn [k v] (into {} (map #(hash-map (first %) (last %)) (partition 2 (interleave k v))))))

(defcheck solution-81496c6f
  (fn
    [xs ys]
    (apply hash-map
      (interleave xs ys)
      )
    ))

(defcheck solution-824343d6
  (fn [a b] (into {} (map #(vector % %2) a b))))

(defcheck solution-8297ac01
  (fn [a b] (apply assoc {} (interleave a b))))

(defcheck solution-82f6a29b
  #(apply array-map (interleave % %2)))

(defcheck solution-830c55b4
  (fn [ks vs] (apply merge (map (fn [k v] {k v}) ks vs))))

(defcheck solution-840a79a6
  (fn custom-zipmap
    [keys-coll values-coll]
    (into {} (map #(vec [%1 %2]) keys-coll values-coll))))

(defcheck solution-86172424
  (fn [ks vs]
    (loop [res {} ks ks vs vs]
      (if (or (empty? ks) (empty? vs))
        res
        (recur
          (assoc res (first ks) (first vs))
          (rest ks)
          (rest vs))))))

(defcheck solution-866aeb7e
  #(loop [xs1 %1 xs2 %2 rmp {}] (if (or (empty? xs1) (empty? xs2)) rmp (recur (rest xs1) (rest xs2) (assoc rmp (first xs1) (first xs2))))))

(defcheck solution-86c02aac
  (fn zipmapj [k v]
    (cond
      (or (empty? k) (empty? v)) {}
      :else
      (assoc
       (zipmapj (rest k) (rest v))
        (first k) (first v)))))

(defcheck solution-87311c22
  #(loop [fseq %1
          sseq %2
          result {}
          ]
     (if (or (empty? fseq) (empty? sseq))
       result
       (recur
         (rest fseq)
         (rest sseq)
         (assoc result (first fseq) (first sseq))))))

(defcheck solution-8807114f
  (fn zm2 [ks vs]
    (into {} (map vector ks vs))))

(defcheck solution-887f3547
  (fn f [k v]
    (if (or (empty? v)(empty? k))
      {}
      (merge {(first k) (first v)} (f (rest k) (rest v)))
      )
    ))

(defcheck solution-88aa7673
  (fn my-zipmap [k v]
    (if (or (empty? k)
            (empty? v))
      {}
      (into (my-zipmap (rest k) (rest v))
        {(first k) (first v)}))))

(defcheck solution-8a266db3
  (fn [thekeys thevalues] (apply hash-map (interleave thekeys thevalues))))

(defcheck solution-8a8e9850
  (fn [ks vs]
    (reduce merge (map (fn [k v] {k v}) ks vs))))

(defcheck solution-8bc8406a
  (fn [keys values]
    (into {} (map (fn [[k v]] [k v]) (partition 2 (interleave keys values))))))

(defcheck solution-8c6263fb
  #(into {} (map (fn[x y](assoc {} x y)) %1 %2)))

(defcheck solution-8c898305
  (fn zipmap-1 [c1 c2]
    (apply merge (map (fn [x x1]
                        {x x1}) c1 c2))))

(defcheck solution-8cada5e2
  (fn [k v] (reduce #(into %1 {(first %2) (second %2)}) {} (map #(list %1 %2) k v))))

(defcheck solution-8cb953a6
  (fn
    map-construction
    [vec1 vec2]
    (reduce
      merge
      (map (fn [x y] {x y}) vec1 vec2)
      )
    ))

(defcheck solution-8d7fd737
  (fn [a, b] (reduce into (map (fn [x, y] {x y}) a b))))

(defcheck solution-8daa0fb2
  (fn [ks vs]
    (apply hash-map
      (mapcat
        #(list % %2)

        ks
        vs
        )
      )
    ))

(defcheck solution-8e025887
  #(reduce (fn [a v] (assoc a (first v) (second v))) {} (map list % %2)))

(defcheck solution-8edd5fd
  (fn [ks vs]
    (reduce conj {} (map #(hash-map %1 %2) ks vs))))

(defcheck solution-8f70d5a8
  (fn [m-keys m-values]
    (reduce
      #(assoc %1 (first %2) (second %2))
      {}
      (map list m-keys m-values))))

(defcheck solution-8fa2b6b1
  (fn [k v] (reduce #(apply assoc %1 %2) {} (map vector k v))))

(defcheck solution-8fb3deb5
  #(loop [[k & ks] %1 [v & vs] %2 res {}]
     (if (or (nil? k) (nil? v)) res
                                (recur ks vs (assoc res k v)))))

(defcheck solution-90068a15
  (fn [keys values] (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 (interleave keys values)))))

(defcheck solution-90081727
  (fn construct-map
    [x y]
    (apply merge (map #(assoc {} %1 %2) x y))))

(defcheck solution-9018a5b
  (fn [ks vs] (reduce (fn [m [k,v]] (assoc m k v)) {} (map list ks vs) )))

(defcheck solution-90219083
  (fn [v1 v2]
    (loop [v1 v1
           v2 v2
           r {}]
      (if (or (nil? (first v1))
              (nil? (first v2)))
        r
        (recur (rest v1) (rest v2) (assoc r (first v1) (first v2)))))))

(defcheck solution-90855055
  (fn  [x y]
    (reduce #(into %1 %2) {}  (map #(identity {%1 %2}) x y))))

(defcheck solution-90e3a058
  (fn hash [keys vals]
    (if (or (empty? keys) (empty? vals))
      {}
      (conj (hash (rest keys) (rest vals)) [(first keys) (first vals)]))))

(defcheck solution-910fe669
  (fn [k v]
    (loop [kk k vv v out (hash-map)]
      (if (or (empty? kk) (empty? vv))
        out
        (recur (rest kk) (rest vv) (assoc out (first kk) (first vv)))))))

(defcheck solution-919c99fa
  (fn zip-map [key-seq value-seq]
    (if (or (empty? key-seq) (empty? value-seq))
      {}
      (assoc (zip-map (rest key-seq) (rest value-seq)) (first key-seq) (first value-seq)))))

(defcheck solution-91b2eea4
  (fn [a b] (apply hash-map (flatten (map (fn [k v] [k v]) a b)))))

(defcheck solution-920e2c6
  (fn f
    ([k v] (f {} k v))
    ([m [k & ks] [v & vs]]
     (if (and k v)
       (f (assoc m k v) ks vs)
       m))))

(defcheck solution-923064d9
  (fn [ks vs] (apply (partial assoc {}) (interleave ks vs))))

(defcheck solution-9258ab45
  (fn [keys vals] (loop [map {} ks (seq keys) vs (seq vals)]
                    (if (and ks vs)
                      (recur (assoc map (first ks) (first vs))
                        (next ks)
                        (next vs))
                      map))))

(defcheck solution-9319c4cd
  (fn [coll1 coll2]
    (reduce
      #(assoc %1 (first %2) (second %2))
      {}
      (map (fn [item1 item2] [item1 item2]) coll1 coll2))))

(defcheck solution-93a0627b
  (fn [& s] (apply hash-map (apply mapcat list s))))

(defcheck solution-93e03310
  #(loop [[hkey & tkey] %1
          [hval & tval] %2
          acc {}]
     (if (and hkey hval)
       (recur tkey tval (assoc acc hkey hval))
       acc)))

(defcheck solution-93fa39ed
  (fn [a b] (into {} (map #(conj [] %1 %2) a b))))

(defcheck solution-94b7d805
  (fn zipmap--recur
    [ks vs]
    (loop [acc {}, ks ks, vs vs]
      (if (and (seq ks) (seq vs))
        (recur (assoc acc (first ks) (first vs)) (rest ks) (rest vs))
        acc))))

(defcheck solution-94e6c456
  #(apply assoc {}
     (interleave %1 %2)))

(defcheck solution-95069d65
  #(apply merge (map hash-map % %2)))

(defcheck solution-952a30c9
  (fn myZipMap
    [mapKeys mapVals]
    (apply merge (map #(assoc {} %1 %2) mapKeys mapVals))))

(defcheck solution-953e2b04
  #(apply hash-map (mapcat vector %1 %2)))

(defcheck solution-96222d1
  (fn [k v]
    (apply hash-map
      (interleave k v)
      )
    ))

(defcheck solution-96eca3a7
  (fn ! [k v]
    (if (or (empty? k) (empty? v)) {}
                                   (assoc (! (rest k) (rest v)) (first k) (first v))
                                   )
    ))

(defcheck solution-970e1675
  (fn [a b]
    (apply assoc {} (interleave a b))))

(defcheck solution-973a8469
  (fn [v1 v2]
    (let [v3 (partition 2 (interleave v1 v2))]
      (loop [m {} i 0]
        (if (= i (count v3))
          m
          (recur (assoc m (first (nth v3 i)) (second (nth v3 i)))
            (inc i)))))))

(defcheck solution-97f40db5
  #(apply hash-map (interleave %1 %2)))

(defcheck solution-9802e37e
  #(->> (interleave %1 %2) (apply hash-map)))

(defcheck solution-993d5da8
  (fn [k v]
    (loop [i 0 m {}]
      (if (= i (min (count k) (count v)))
        m
        (recur (inc i) (conj m [(nth k i) (nth v i)]))))))

(defcheck solution-995464c4
  (fn [a b]
    (apply assoc {} (interleave a b))))

(defcheck solution-9b52bba6
  (fn map-con [a b]
    (apply hash-map (interleave a b))))

(defcheck solution-9b73fab2
  (fn [c1 c2] (into {} (reduce #(conj %1 (into [] %2)) [] (partition-all 2 (interleave c1 c2))))))

(defcheck solution-9bdf8d3b
  #(reduce (fn [m [k v]] (assoc m k v)) {} (map (fn [k v] [k v]) %1 %2)))

(defcheck solution-9bf803f6
  (fn [ks vs]
    (loop [[k & ks] ks
           [v & vs] vs
           m {}]
      (if (or (nil? k) (nil? v))
        m
        (recur ks vs (assoc m k v))))))

(defcheck solution-9ca278df
  #(reduce (fn [hmap kv] (apply assoc hmap kv))
     {}
     (partition 2
       (interleave %1 %2))))

(defcheck solution-9ce19253
  (fn [[& ks] [& vs]] (into {} (map vector ks vs)) ))

(defcheck solution-9cfb7dec
  #( apply hash-map (interleave %1 %2)))

(defcheck solution-9d76a1b8
  (fn [coll1 coll2] (into {} (map vector coll1 coll2))))

(defcheck solution-9f5cced7
  (fn [xs ys]
    (loop [xs1 (seq xs)
           ys1 (seq ys)
           res {}]
      (if (and (seq? xs1) (seq? ys1))
        (recur (next xs1) (next ys1) (assoc res (first xs1) (first ys1)))
        res))))

(defcheck solution-9fc0d6d8
  (fn [ks' vs']
    (letfn [(go [ks vs]
              (if (or (empty? ks) (empty? vs))
                {}
                (merge {(first ks) (first vs)}
                  (go (rest ks) (rest vs)))))]
      (go ks' vs'))))

(defcheck solution-a001c62e
  (fn [ks vs] (apply assoc {} (for [[k v] (map vector ks vs)
                                    x [k v]]
                                x))))

(defcheck solution-a102b7de
  (fn [k v]
    (loop [k_ k v_ v ret ()]
      (cond
        (or (not k_) (not v_))
        (apply hash-map (into '() ret))
        :else
        (recur
          (next k_)
          (next v_)
          (conj ret (first k_) (first v_)))))))

(defcheck solution-a11404ae
  (fn my-zipmap [keys values]
    (->> (map vector keys values)
      (into {}))))

(defcheck solution-a1834b49
  (fn f
    ([k v] (f k v {}))
    ([[k & keys] [v & vals] a]
     (if (and k v)
       (recur keys vals (assoc a k v))
       a))))

(defcheck solution-a227f9a1
  (fn [ks vs]
    (apply hash-map
      (mapcat vector ks vs))))

(defcheck solution-a247b936
  (fn mapper
    ([c1 c2]
     (do #_(prn c1 c2)
         (mapper c1 c2 '{})))
    ([c1 c2 result]
     (if (and (seq c1) (seq c2))
       (mapper (rest c1) (rest c2) (assoc result (first c1) (first c2)))
       result))))

(defcheck solution-a31ffe93
  (fn [ks vs]
    (loop [m {}
           ks ks
           vs vs]
      (if (or (empty? ks)
              (empty? vs))
        m
        (recur (conj m {(first ks) (first vs)}) (rest ks) (rest vs))))))

(defcheck solution-a35e7c9b
  #(into {} (map vector % %2)))

(defcheck solution-a364c0eb
  (fn [k v]
    (apply hash-map (interleave k v))))

(defcheck solution-a4185a6d
  (fn [keys vals]
    (into {} (map #(vector %1 %2) keys vals))))

(defcheck solution-a4cd11c2
  #(apply assoc {} (interleave %1 %2)))

(defcheck solution-a4f0e242
  (fn newzipmap [x y]
    (apply hash-map (interleave x y))))

(defcheck solution-a5e745b5
  (fn mc [ks, vs]
    (into {}
      (for [k ks
            :let [ki (.indexOf ks k)]
            :while (contains? vs ki)] (conj {} {k (nth vs ki)}))
      )))

(defcheck solution-a65ba821
  (fn my-zipmap [ks vs]
    (into {} (map (fn [k v] [k v]) ks vs))))

(defcheck solution-a692d7b1
  (fn [a b] (apply hash-map (interleave a b))))

(defcheck solution-a707c71d
  #(apply hash-map (interleave % %2)))

(defcheck solution-a7254f0e
  (fn [coll1 coll2]
    (into {} (map vector coll1 coll2))))

(defcheck solution-a738c513
  #(apply hash-map (mapcat list % %2)))

(defcheck solution-a82d0539
  (fn my-zipmap [keys values]
    (reduce #(assoc %, (first %2), (second %2))
      {}
      (map list keys values))))

(defcheck solution-a8c44fc0
  (fn [s1 s2] (apply array-map (interleave s1 s2))))

(defcheck solution-a93175c3
  (fn [c1 c2]
    (let [kvv (map vector c1 c2)
          kvm (map #(apply hash-map %) kvv)]
      (apply merge kvm))))

(defcheck solution-a99f02cd
  (fn  [kys vals]
    (loop [k (seq kys), v (seq vals), h (hash-map)]
      (cond (or (empty? k) (empty? v)) h
            :else (recur (rest k) (rest v) (assoc h (first k) (first v)))))))

(defcheck solution-a9aad39e
  (fn [keys vals] (apply hash-map (flatten (map #(vector %1 %2) keys vals)))))

(defcheck solution-a9bfbb42
  #(into {} (map (fn [x y] [x y]) % %2)))

(defcheck solution-aa6467f1
  (fn [sq1 sq2] (reduce #(assoc %1 (first %2) (second %2)) {} (map vector sq1 sq2))))

(defcheck solution-aaa02460
  (fn [seq1 seq2]
    (loop [result {} elementsKeys seq1 elementsValues seq2]
      (if (or (empty? elementsKeys) (empty? elementsValues))
        result
        (recur (into result {(first elementsKeys) (first elementsValues)}) (rest elementsKeys) (rest elementsValues))
        )
      )
    ))

(defcheck solution-aafdff6f
  (fn [c1 c2]
    (reduce conj {}
      (map (fn [x y]  [x y]) c1 c2))))

(defcheck solution-ab17813c
  (fn [key-vect val-vect]
    (apply hash-map (interleave key-vect val-vect))))

(defcheck solution-ab57058f
  (fn [k v]
    (reduce #(assoc %1 (first %2) (second %2)) {}
      (map #(identity [%1 %2]) k v))))

(defcheck solution-abcf30f1
  #(into {}
     (map vector % %2)))

(defcheck solution-ac47d921
  #(loop [i %1
          j %2
          k {}]
     (if (or (empty? i) (empty? j))
       k
       (recur (rest i) (rest j) (assoc k (first i) (first j))))))

(defcheck solution-ac7f3175
  (fn [ks vs]
    (loop [k ks v vs m {}]
      (if (or (empty? k) (empty? v))
        m
        (recur (next k) (next v) (assoc m (first k) (first v)))))))

(defcheck solution-ac7fedea
  (fn [l1 l2]
    (apply assoc {}
      (interleave
        l1 l2
        )
      )
    ))

(defcheck solution-aeb5d113
  #(apply hash-map (flatten (map list %1 %2))))

(defcheck solution-b0622530
  (fn [k v]
    (loop [m {} a k b v]
      (if (or (empty? a) (empty? b))
        m
        (recur
          (assoc m (first a) (first b))
          (rest a)
          (rest b))))))

(defcheck solution-b0a30c1f
  (fn [c1 c2] (into {} (map vector c1 c2))))

(defcheck solution-b0b2d9be
  (fn [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
          (next ks)
          (next vs))
        map))))

(defcheck solution-b0b65227
  (fn [ks-in vs-in]
    (loop [result {} ks ks-in vs vs-in]
      (if (or (nil? ks) (nil? vs))
        result
        (recur (assoc result (first ks) (first vs)) (next ks) (next vs))
        )
      )
    ))

(defcheck solution-b0fd82f7
  (fn [col1 col2]
    (apply hash-map
      (interleave col1 col2))))

(defcheck solution-b274037b
  (fn [xs ys]
    (apply hash-map (reverse (loop [xsp xs ysp ys acc '()]
                               (if (or (empty? xsp) (empty? ysp))
                                 acc
                                 (recur (rest xsp) (rest ysp) (cons (first ysp) (cons (first xsp) acc)))))))))

(defcheck solution-b30ce4e9
  #(into {} (apply map hash-map %&)))

(defcheck solution-b35b829
  (fn my-zipmap [ks vs]
    (letfn [(cycle [& cs]
              (let [seqs (map seq cs)]
                (lazy-seq
                  (when (every? identity seqs)
                    (concat (map first seqs) (apply cycle (map rest seqs)))))))]
      (apply hash-map (cycle ks vs)))))

(defcheck solution-b36eb554
  (fn [a b]
    (into {} (map #(hash-map % %2) a b))))

(defcheck solution-b45e635f
  (fn [keys vals]
    (apply hash-map (mapcat list keys vals))))

(defcheck solution-b4b15d69
  #(apply merge (for [i (range (min (count %2) (count %)))] {(nth % i) (nth %2 i)})))

(defcheck solution-b55fe51e
  (fn [ks vs]
    (reduce
      (fn [s [k v]] (assoc s k v))
      {}
      (map list ks vs))))

(defcheck solution-b5c5337b
  (fn[a b]
    (apply merge (map #(assoc {} %1 %2) a b))))

(defcheck solution-b5cf0953
  (fn [col1 col2]
    (let [z (interleave col1 col2)]
      (apply hash-map z))))

(defcheck solution-b72214d7
  (fn my-zipmap
    [keys vals]
    (apply merge
      (map #(assoc {} (first %) (fnext %))
        (partition 2 (interleave keys vals))))))

(defcheck solution-b82a0c23
  #(reduce (fn [map [k v]] (assoc map k v)) {} (map vector % %2)))

(defcheck solution-b82e0a11
  #(apply (partial assoc {}) (interleave % %2)))

(defcheck solution-b88c18f8
  (fn [xs ys]
    (into {} (map vector xs ys))))

(defcheck solution-b91f6123
  (fn [a b] (reduce conj {} (map vector a b))))

(defcheck solution-b9307f1b
  (fn [c1 c2]
    (apply hash-map (interleave c1 c2))))

(defcheck solution-b9563790
  (fn f [a b]
    (cond
      (empty? a) {}
      (empty? b) {}
      :else (assoc (f (rest a) (rest b))
              (first a)
              (first b))
      )))

(defcheck solution-bac65b5
  #(->> (map vector %1 %2) (into {})))

(defcheck solution-bb664c33
  (fn [x y]
    (or (apply hash-map (interleave x y))
        (into {} (map vector x y)))))

(defcheck solution-bbf72388
  #(into {} (apply map vector %&)))

(defcheck solution-bc45247a
  (fn zm [ks vs]
    (when-let [ks (seq ks)]
      (when-let [vs (seq vs)]
        (into {(first ks) (first vs)} (zm (rest ks) (rest vs)))))))

(defcheck solution-bd1c113c
  (fn map-construction [x y]
    (first (reduce

             (fn [[a y] xi]
               (let [fy (first y)]
                 [(if-not (nil? fy)(assoc a xi fy) a) , (rest y)]))

             [{},y]
             x))))

(defcheck solution-bdf21f68
  (fn [xs ys]
    (->> (map #(list %1 %2) xs ys)
      (reduce #(assoc %1 (first %2) (second %2))
        {}))))

(defcheck solution-be41c544
  #(reduce conj {} (map vector %1 %2)))

(defcheck solution-be5347a4
  (fn zipup [keyz valuez]
    (if (and (next keyz) (next valuez))
      (assoc (zipup (rest keyz) (rest valuez)) (first keyz) (first valuez))
      {(first keyz) (first valuez)})
    ))

(defcheck solution-bf7ba9b
  (fn this-f [list-1 list-2]
    (if (not
          (or (empty? list-1)
              (empty? list-2)))
      (assoc (this-f (rest list-1) (rest list-2))
        (first list-1) (first list-2))
      {})
    ))

(defcheck solution-bf9be036
  (fn [x y] (apply array-map (interleave x y))))

(defcheck solution-bfa0fc44
  (fn [c1 c2] (apply hash-map (apply concat (map #(list %1 %2) c1 c2)))))

(defcheck solution-bfde510f
  (fn [ks vs] (apply hash-map (interleave ks vs))))

(defcheck solution-c031b90f
  (fn [ks vs]
    (apply hash-map (mapcat #(list %1 %2) ks vs))))

(defcheck solution-c03912f1
  (fn [ks vs]
    (apply hash-map (mapcat #(list %1 %2) ks vs))))

(defcheck solution-c05e24c7
  (fn zmap
    ([v1 v2] (zmap v1 v2 {}))
    ([v1 v2 m]
     (if (or (empty? v1) (empty? v2))
       m
       (zmap (rest v1) (rest v2) (assoc m (first v1) (first v2)))))))

(defcheck solution-c0c144f9
  (fn zm [a b]
    (let [ca (count a) cb (count b) c (if (< ca cb) ca cb)]
      (loop [n 0 r {}]
        (if (= n c)
          r
          (recur (inc n) (assoc r (nth a n) (nth b n))))))))

(defcheck solution-c21da5ba
  (fn [c1 c2]
    (apply hash-map (mapcat #(list % %2) c1 c2))))

(defcheck solution-c2ecaef3
  (fn [k v] (apply assoc {} (flatten (map #(vector %1 %2) k v)))))

(defcheck solution-c3e0564a
  (fn f [s1 s2]
    (reduce merge (map #(assoc {} % %2) s1 s2))))

(defcheck solution-c407dd8c
  #(apply assoc {}
     (interleave %1 %2)))

(defcheck solution-c450ecae
  #(into {} (map vec (partition 2 (interleave %1 %2)))))

(defcheck solution-c4ce83d3
  (fn [s1 s2] (apply assoc {} (interleave s1 s2))))

(defcheck solution-c50b4c5c
  (fn [coll1 coll2]
    (reduce conj {}
      (map #(vector %1 %2) coll1 coll2))))

(defcheck solution-c58e3e53
  (fn   [col1 col2]
    (loop [col3 {} cnt (min (count col1) (count col2)) ]
      (if (= cnt 0)
        col3
        (let [pos (- cnt 1)]
          (recur (conj col3 [ (nth col1 pos)  (nth col2 pos)  ] ) (dec cnt)
            )
          )))))

(defcheck solution-c5931380
  (fn [a b] (into {} (map vector a b))))

(defcheck solution-c5cb69ae
  #(reduce conj (map (partial assoc {}) %1 %2)))

(defcheck solution-c5d48ae1
  (fn zm [ks vs]
    (into {} (map vector ks vs))))

(defcheck solution-c5fbd7e4
  (fn [x y] (loop [acc {} f1 x f2 y]
              (if (or (empty? f1) (empty? f2))
                acc
                (recur (assoc acc (first f1) (first f2))
                  (rest f1) (rest f2))))))

(defcheck solution-c7bf5753
  (fn [keys vals] (loop [r {} k keys v vals] (if (not (and (first k) (first v))) r (recur (assoc r (first k) (first v)) (rest k) (rest v))))))

(defcheck solution-c84bf7c1
  (fn [list1 list2]
    (apply hash-map
      ((fn ziplists [xs ys]
         (if (or (empty? xs) (empty? ys))
           (list)
           (concat (list (first xs) (first ys))
                   (ziplists (rest xs) (rest ys)))))
       list1 list2))))

(defcheck solution-c85d9455
  (fn zip [a b]  (if-not (or (empty? a) (empty? b))
                   (merge
                     {(first a) (first b)}
                     (zip (rest a) (rest b))))))

(defcheck solution-c8adeead
  #(apply array-map (flatten (map vector % %2))))

(defcheck solution-c914238
  #(into {} (map (fn [a b] [a b]) %1 %2)))

(defcheck solution-c922571e
  (fn [sq1, sq2]
    (into {}
      (if (>= (count sq1) (count sq2))
        (map-indexed
          (fn [idx, it] [(nth sq1 idx), it]) sq2)
        (map-indexed
          (fn [idx, it] [it, (nth sq2 idx)]) sq1)))))

(defcheck solution-c940d1ed
  (fn map-const
    [f s]
    (reduce #(assoc %1 (first %2) (second %2)) {} (map vector f s))))

(defcheck solution-ca047442
  (fn [keys vals]
    (apply hash-map (interleave keys vals))))

(defcheck solution-ca2a5f88
  #(apply assoc {} (mapcat (fn [x y] [x y]) %1 %2)))

(defcheck solution-ca541257
  (fn [v1 v2] (into {} (map #(vector %1 %2) v1 v2))))

(defcheck solution-caee6b40
  (fn [ks vs] ((fn [ks vs m] (if (or (empty? ks) (empty? vs)) m (recur (next ks) (next vs) (assoc m (first ks) (first vs)) ) ) ) ks vs (hash-map))))

(defcheck solution-cb019ba4
  #(apply hash-map (interleave % %2)))

(defcheck solution-cb5da6d8
  (comp (partial apply assoc {}) interleave))

(defcheck solution-cb858115
  (fn m [x y]
    (cond (or (empty? x) (empty? y)) '{}
          :else
          (assoc (m (rest x) (rest y)) (first x) (first y)))))

(defcheck solution-cbe97f3b
  #(apply conj {} (map vec (map list %1 %2))))

(defcheck solution-cc2d7c2d
  (fn [k v]
    (reduce #(conj %1 %2) {}
      (map #(vector %1 %2) k v))))

(defcheck solution-ce55db67
  (fn [x y] (apply assoc {} (interleave x y))))

(defcheck solution-d07b727a
  (fn [a b]
    (apply conj
      (map #(assoc {} %1 %2) a b))))

(defcheck solution-d07c9cba
  (fn [a b] (reduce merge (map #(hash-map %1 %2) a b))))

(defcheck solution-d1279a57
  (fn [ks vs]
    (loop [[kh & kb] ks [vh & vb] vs ret {}]
      (if-not (or (nil? kh) (nil? vh))
        (recur kb vb (assoc ret kh vh))
        ret))))

(defcheck solution-d1c0eef
  (fn [a b]
    (loop [x a y b z {}]
      (if (or (= x []) (= y []))
        z
        (recur (rest x) (rest y) (conj z {(first x) (first y)}))))))

(defcheck solution-d1e0739c
  (fn [a b]
    (apply assoc {} (interleave a b))))

(defcheck solution-d2011cbb
  (fn [l1 l2]
    (loop [l1 l1 l2 l2 r {}]
      (if (or (empty? l1) (empty? l2)) r
                                       (recur (rest l1) (rest l2) (conj r [(first l1) (first l2)]))))))

(defcheck solution-d291bfa
  (comp (partial apply sorted-map) interleave))

(defcheck solution-d30c4bc
  (fn [& more]
    (reduce #(assoc %1 (first %2) (second %2)) {}
      (apply map list more))))

(defcheck solution-d3575757
  (fn [& args]
    (reduce #(assoc %1 (first %2) (second %2))
      {}
      (apply map list args))))

(defcheck solution-d37cb8c8
  (fn [a b] (apply conj (map #(hash-map %1 %2) a b))))

(defcheck solution-d386457a
  (fn [ks vs]
    (into {} (map (fn [a b] [a b]) ks vs))))

(defcheck solution-d3a25e75
  (fn [keys values]
    (apply hash-map (interleave keys values))))

(defcheck solution-d3ff81c8
  (fn [ks vs] (apply hash-map (flatten (map list ks vs)))))

(defcheck solution-d41815d1
  (fn  [kcol vcol]
    (apply hash-map (interleave kcol vcol))))

(defcheck solution-d4737a11
  (fn [ck cv] (let [n (apply min (map count [ck cv]))] (loop [i 0 r {}]
                                                         (if (> n i) (recur (inc i) (assoc r (nth ck i) (nth cv i))) r
                                                                     )))))

(defcheck solution-d4e5ab29
  (fn [ks vs] (apply hash-map (interleave ks vs))))

(defcheck solution-d522a09d
  (fn [keys vals] (into {} (map vector keys vals))))

(defcheck solution-d532112a
  (fn [v1 v2] (into {} (map #(conj [] % %2) v1 v2))))

(defcheck solution-d54fe236
  #(apply merge (map (fn [k v]
                       {k v})
                  %1
                  %2)))

(defcheck solution-d5501e0a
  (fn zmap [ks vs](apply hash-map (interleave ks vs))))

(defcheck solution-d5865bf
  (fn [ks vs]
    (into {} (map vector ks vs))
    ))

(defcheck solution-d5d07bd8
  (fn [k v] (reduce #(conj %1 %2) {}  (reverse (map #(identity  {%1 %2}) k v)))))

(defcheck solution-d5e7a5e3
  (fn [coll1 coll2] (reduce (fn [my-map pair] (assoc my-map (first pair) (second pair))) {} (#(partition 2 (interleave %1 %2)) coll1 coll2))))

(defcheck solution-d688826d
  (fn [ks vs]
    (into (hash-map)
      (map vector ks vs))))

(defcheck solution-d693f1ea
  (fn [keys vals]
    (reduce (fn [map [k v]] (assoc map k v)) {} (partition 2 (interleave keys vals)))))

(defcheck solution-d6d21323
  (fn map-constr [keyss valuess]
    (reduce #(apply assoc %1 %2) {} (map #(vector %1 %2) keyss valuess))))

(defcheck solution-d718a6ff
  (fn [keys vector]
    (apply assoc {} (interleave keys vector))))

(defcheck solution-d7c21024
  (fn [mkeys mvals]
    (loop [_keys mkeys
           _vals mvals
           ret {}]
      (if (or
           (empty? _keys)
           (empty? _vals))
        ret
        (let [tkey (first _keys)
              tval (first _vals)]
          (recur (rest _keys) (rest _vals) (conj ret {tkey tval})))))))

(defcheck solution-d8544407
  (fn z
    [k v]
    (if (and (not-empty k) (not-empty v))
      (merge {(first k) (first v)} (z (rest k) (rest v))))))

(defcheck solution-d9848a8c
  (fn f [col c1 c2] (if-not (or (empty? c1) (empty? c2)) (f (assoc col (first c1) (first c2)) (rest c1) (rest c2)) col)) {})

(defcheck solution-daba902c
  (fn myzip [x, y]
    (if (or (empty? x) (empty? y))
      (hash-map)
      (assoc (myzip (rest x) (rest y)) (first x) (first y)))))

(defcheck solution-db54f4d3
  #(reduce conj {} (map vector % %2)))

(defcheck solution-dbe3c557
  (fn merge [one two]
    (if (or (empty? one) (empty? two))
      {}
      (assoc (merge (rest one) (rest two)) (first one) (first two)))))

(defcheck solution-dc7bb7bf
  (fn make-map [keys values]
    (reduce #(assoc %1 (first %2) (second %2)) {}
      (map #(list % %2) keys values))))

(defcheck solution-dc7e3213
  (fn [& colls] (apply hash-map (apply interleave colls))))

(defcheck solution-dd689fe4
  #(reduce merge (map hash-map %1 %2)))

(defcheck solution-dd88052d
  (fn [keys values]
    (apply array-map (interleave keys values))))

(defcheck solution-de30f762
  (fn [k v]
    (loop [kk k
           vv v
           m {}]
      (if (or (empty? kk) (empty? vv))
        m
        (recur (rest kk) (rest vv)
          (merge {(first kk) (first vv)} m))
        )
      )))

(defcheck solution-debc6e8c
  (fn [c1 c2] (apply merge (map #(hash-map % %2) c1 c2))))

(defcheck solution-deed1c79
  (fn [xs ys] (reduce (fn [acc [x y]] (assoc acc x y)) {} (map vector xs ys))))

(defcheck solution-defaa6b2
  (fn z[keys vals]
    (let [[k & keys] keys
          [v & vals] vals]
      (if (and k v)
        (assoc (z keys vals) k v)
        {}))))

(defcheck solution-e1c0c712
  #(reduce (fn [acc e] (apply assoc acc e)) {} (partition-all 2 (interleave % %2))))

(defcheck solution-e24e8143
  (fn [c1 c2]
    (into {}
      (map #(hash-map % %2) c1 c2))))

(defcheck solution-e29e3bb4
  (fn [a b] (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 (interleave a b)))))

(defcheck solution-e2cfe5b6
  (fn [ks vs]
    (apply merge (map #(hash-map %1 %2) ks vs))))

(defcheck solution-e2f59d26
  (fn m [c1 c2]
    (apply merge (apply map hash-map [c1 c2]))))

(defcheck solution-e36ef3bc
  (fn [coll1 coll2]
    (reduce merge {} (map hash-map coll1 coll2))))

(defcheck solution-e38c0641
  #(into {} (map vector %1 %2)))

(defcheck solution-e39809e6
  (fn [a b]
    (into {} (map #(vector %1 %2) a b))))

(defcheck solution-e3a99838
  #(apply sorted-map (apply mapcat list %&)))

(defcheck solution-e3d5258
  #(loop [i 0 result nil]
     (if (or (= i (count %1)) (= i (count %2)))
       result
       (recur (inc i) (assoc result (nth %1 i) (nth %2 i) ))
       )))

(defcheck solution-e5c3b085
  (fn [a b] (apply hash-map (apply concat (map list a b)))))

(defcheck solution-e5eb2818
  (fn [m col1 col2]

    (if (or (empty? col1) (empty? col2))

      m

      (recur (assoc m (first col1)(first col2)) (rest col1) (rest col2)))) {})

(defcheck solution-e74eb57c
  (fn [k v] (reduce conj {} (map (fn [& v] (vec v))k v))))

(defcheck solution-e7df02f6
  (fn prob61
    [keys values]
    (into {} (map #(hash-map %1 %2) keys values))))

(defcheck solution-e84e0933
  (fn [ks vs]
    (into {}
      (map (fn [k v]
             [k v])
        ks
        vs))))

(defcheck solution-e9293e05
  (fn [k  v]
    (into {} ((partial map vector) k v))))

(defcheck solution-eb875111
  #(into (sorted-map) (map vector %1 %2)))

(defcheck solution-ecac487a
  (fn [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
          (next ks)
          (next vs))
        map))))

(defcheck solution-ecd0d017
  (fn f [ks vs]
    (->> (map vector ks vs)
      (into {}))))

(defcheck solution-ed0c01c1
  #( reduce (fn f[o a] (assoc o (first a) (second a))) {} (map list % %2)))

(defcheck solution-ed3d181e
  ;(fn [ks vs]
  ;  (reduce (fn [m [k v]] (assoc m k v)) {} (map vector ks vs)))

  (fn [ks vs]
    (reduce #(apply assoc %1 %2) {} (map vector ks vs))))

(defcheck solution-ee377e6e
  (fn [keys vals]
    (apply hash-map (interleave keys vals))))

(defcheck solution-ef10c802
  #(apply hash-map (interleave % %2)))

(defcheck solution-ef2a144
  #(apply assoc {}
     (interleave %1 %2)))

(defcheck solution-f088a710
  (fn [as bs]
    (loop [a (first as)
           as (rest as)
           b (first bs)
           bs (rest bs)
           h {}]
      (if (and a b)
        (recur (first as) (rest as) (first bs) (rest bs) (assoc h a b))
        h))))

(defcheck solution-f0b07675
  (fn [ks vs]
    (apply hash-map (flatten (map #(list %1 %2) ks vs)))))

(defcheck solution-f0d016d5
  #(loop [m {} xs % ys %2]
     (if (or (empty? xs) (empty? ys))
       m
       (recur
         (assoc m (first xs) (first ys))
         (rest xs)
         (rest ys)))))

(defcheck solution-f135fd4d
  (fn [x y] (into {} (map #(vec [%1 %2]) x y))))

(defcheck solution-f13922d7
  (fn [l1 l2] (reduce (fn [m [k v]] (assoc m k v)) {} (map (fn [a b] [a b]) l1 l2))))

(defcheck solution-f356eaad
  (fn [keys vals] (apply hash-map (interleave keys vals))))

(defcheck solution-f48c4e2
  (fn [b t] (reduce into {} (map #(hash-map %1 %2) b t))))

(defcheck solution-f55ebac8
  (fn [la lb]
    (reduce conj (map hash-map la lb))))

(defcheck solution-f589d7c
  #(apply merge (map (fn [a b] {a b}) %1 %2)))

(defcheck solution-f6cad35d
  (fn [ks vs]
    (->> (map #(vector %1 %2) ks vs)
      (reduce #(assoc %1 (%2 0) (%2 1)) {}))))

(defcheck solution-f74827d8
  (fn [v1 v2]
    (loop [res {}, remv1 v1, remv2 v2]
      (if (or (empty? remv1) (empty? remv2))
        res
        (recur (assoc res (first remv1) (first remv2)) (rest remv1) (rest remv2))))))

(defcheck solution-f75c74c7
  (fn [ks vs]
    (into {} (map vector ks vs))))

(defcheck solution-f7903fab
  (fn [k v] (apply merge (map #(hash-map % %2) k v))))

(defcheck solution-f7b45808
  #(apply (partial conj {}) (map vector %1 %2)))

(defcheck solution-f889e3aa
  #(into {} (map (fn [a b] [a b]) % %2)))

(defcheck solution-f8ea7be3
  #(into {} (map (partial assoc {}) % %2)))

(defcheck solution-f917c2be
  (fn [keys, values]
    (reduce merge (map (fn [a b] {a b}) keys values))
    ))

(defcheck solution-f9f4e0e2
  (fn [a b]
    (reduce
      #(conj % %2)
      {}
      (map #(vector % %2) a b))))

(defcheck solution-fa70ee92
  (fn peu [x y] (if (or (empty? x) (empty? y)) {} (conj (peu (rest x) (rest y)) (hash-map (first x) (first y))))))

(defcheck solution-fa84b15e
  (fn C [ks vs]
    (cond (empty? ks) {}
          (empty? vs) {}
          :else (assoc (C (rest ks) (rest vs))
                  (first ks)
                  (first vs)))))

(defcheck solution-fae60b2
  (fn [v1 v2]
    (apply assoc {} (interleave v1 v2))))

(defcheck solution-faf04529
  (fn [col1 col2]
    (loop [l1 col1, l2 col2, result {}]
      (if (or (empty? l1) (empty? l2))
        result
        (recur (rest l1) (rest l2)
          (assoc result (first l1) (first l2) )
          )
        )
      )
    ))

(defcheck solution-fc0af5f5
  (fn map-construct [s1 s2] (apply merge (map hash-map s1 s2))))

(defcheck solution-fc7b08ce
  (fn map-construction [coll1 coll2]
    (apply hash-map (interleave coll1 coll2))))

(defcheck solution-fcd2065
  #(apply hash-map
     ((fn my-zip [c1 c2]
        (let [s1 (seq c1)
              s2 (seq c2)]
          (when (and s1 s2)
            (cons (first s1)
              (cons (first s2)
                (my-zip (rest c1) (rest c2)))))))
      %1 %2)))

(defcheck solution-fd4bbcf6
  (fn [k v](into {} (map vector k v))))

(defcheck solution-fd72da41
  #(apply assoc {} (interleave % %2)))

(defcheck solution-fe319c24
  ;(fn [ks vs]
  ;     (loop [kk ks,
  ;            vv vs,
  ;            m {}]
  ;       (if (or (empty? kk) (empty? vv))
  ;         m
  ;         (recur (rest kk)
  ;                (rest vv)
  ;                (conj m [(first kk) (first vv)])))))

  ;;(fn [x y] (apply hash-map (interleave x y)))
  ;;#(into {} (map vector %1 %2))
  (comp (partial apply hash-map) interleave))

(defcheck solution-fea6c230
  (fn [ks vs] (into {} (map #(vector %1 %2) ks vs))))

(defcheck solution-ff66b6fc
  (fn [ks vs] (apply merge (map hash-map ks vs))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-61))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
