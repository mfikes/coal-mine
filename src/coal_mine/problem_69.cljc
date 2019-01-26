(ns coal-mine.problem-69
  (:require [coal-mine.checks :refer [defcheck-69] :rename {defcheck-69 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-101ae24b
  (fn m2 [f m & ms]
    (loop [m m ms ms]
      (if-let [x (first ms)]
        (recur (conj m
                 (mapcat #(hash-map %
                            (if (m %)
                              (f (m %) (x %))
                              (x %)))
                   (keys (first ms))))
          (rest ms))
        m))))

(defcheck solution-1061308f
  (fn [f m & n]
    (into {}
      (for [[k v] (seq (apply merge n))]
        (if (nil? (m k))
          [k v]
          [k (f (m k) v)])))))

(defcheck solution-107ad89a
  (fn f-merge-with
    ([f m1 m2]
     (letfn [(f-merge [[key val]]
               (if (contains? m2 key)
                 [key (f val (get m2 key))]
                 [key val]))]
       (merge m2 (into {} (map f-merge m1)))))
    ([f m1 m2 & ms]
     (if (seq ms) (apply f-merge-with f (f-merge-with f m1 m2) (first ms) (rest ms))
                  (f-merge-with f m1 m2)))))

(defcheck solution-1109cc2
  (fn umerge [f & args]
    (let [arg1 (first args)
          arg2 (second args)
          rst (drop 2 args)
          mrg (loop [ret arg1 x arg2]
                (if (empty? x)
                  ret
                  (let [elt (first x)
                        m-v (ret (key elt) nil)]
                    (recur (assoc ret (key elt) (if (nil? m-v)
                                                  (val elt)
                                                  (f m-v (val elt))))
                      (rest x)))))]
      (if (= 2 (count args))
        mrg
        (apply umerge f mrg rst)))))

(defcheck solution-1124acdc
  (fn
    [f m1 & ms]
    (loop [m m1 o (vec ms)]
      (let [k (first (keys (first o)))]
        (if (empty? o )
          m
          (if (empty? (first o))
            (recur m (vec (rest o)))
            (if (contains? m k)
              (recur (assoc m k (f (get m k) (get (first o) k ) ) ) (assoc o 0 (conj {} (rest (first o)))))
              (recur (assoc m k (get (first o) k )  ) (assoc o 0 (conj {} (rest (first o)))))
              )

            ))))))

(defcheck solution-119bde93
  (fn p69
    ([f & mss] (let [mks (reduce (partial apply conj) #{} (map keys mss))]
                 (reduce conj {} (map (fn [k] (let [ags (filter (comp not nil?) (map (fn [m] (m k)) mss))]
                                                [k (if (= 1 (count ags)) (first ags) (apply f ags))])) mks))))))

(defcheck solution-11d61c4b
  (fn [f m & ms]
    (reduce
      (fn [a [k v]] (if (contains? a k) (update-in a [k] f v) (assoc a k v)))
      m
      (apply concat ms))))

(defcheck solution-11ea7eb1
  (fn merge-with-clone [f m & maps]
    (reduce (fn [acc [k v]]
              (assoc acc k (if (acc k) (f (acc k) v) v)))
      m (apply concat (map vec maps)))))

(defcheck solution-11eb135a
  (fn [f & s]
    (into {} (map (fn [[k v]] (if (= (count v) 1) [k (v 0)] [k (apply f v)]))
               (reduce
                 (fn [x y]
                   (reduce
                     #(assoc %1 (%2 0) (conj (get %1 (%2 0) []) (%2 1)))
                     x
                     y)
                   )
                 {} s)))))

(defcheck solution-121cbe57
  (fn [f a & b] (reduce #(reduce (fn [s e] (if (nil? (s (key e))) (conj s e) (update-in s [(key e)] f (val e)))) % %2) a b)))

(defcheck solution-1224705e
  (fn [f & a]
    (into {}
      (map
        (fn [k] [k (reduce f (keep #(% k) a))])
        (keys (apply merge a))))))

(defcheck solution-12a9ff0b
  #(reduce (fn [x y]
             (merge x
               (apply hash-map
                 (mapcat (fn [& [[k v]]]
                           [k (if (x k) (% (x k) v) v)]) y)))) %&))

(defcheck solution-12ab53c
  (fn [f & ms]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m k]
            (let [a (get m1 k) b (get m2 k)]
              (do
                #_(println m k)
                (cond
                  (and a b) (assoc m k (f a b))
                  a         (assoc m k a)
                  b         (assoc m k b)))))
          {}
          (concat (keys m1) (keys m2))))
      ms)))

(defcheck solution-12e40611
  (fn [f & ms]
    (reduce (fn [acc m]
              (reduce-kv (fn [new-m k v]
                           (update-in new-m [k] #(if (nil? %) v (f % v))))
                acc
                m))
      ms)))

(defcheck solution-12fb7098
  (fn mrg [f & xs] (let [insertf (fn [m [k v]] (if (contains? m k) (assoc m k (f (get m k) v)) (assoc m k v)))
                         mergef2 #(reduce insertf %1 %2)]
                     (reduce mergef2 xs))))

(defcheck solution-134e0a3a
  (fn [f & rst]
    (reduce
      (fn [m1 m2]
        (into m1
          (map (fn [[k v]] [k (if (contains? m1 k) (f (get m1 k) v) v)]) m2)))
      rst)))

(defcheck solution-134f96b9
  (fn [f & maps]
    (reduce (fn [r m]
              (merge r
                (zipmap (keys m)
                  (map #(if (contains? r %)
                          (f (r %) (m %))
                          (m %))
                    (keys m)))))
      maps)))

(defcheck solution-1364649b
  (fn [f & hmaps]
    (into {} (map (fn [[k v]] [k (reduce f v)])
               (map (fn [[k vecs]] [k (map (fn [v] (get v 1)) vecs)])
                 (group-by (fn [[k v]] k) (apply concat hmaps)))))))

(defcheck solution-13791b3f
  (fn mrgw [f & xs]
    (letfn [(ks [xs] (apply clojure.set/union (map set (map keys xs))))]
      (apply merge
        (for [k (ks xs)
              :let [vals (remove nil? (map get xs (repeat k)))]]
          {k (if (> (count vals) 1) (apply f vals) (first vals))} )))))

(defcheck solution-13813377
  (fn [fun & r]
    (letfn [(_merge-two [m1 m2]
              (reduce (fn [m [k v]]
                        (if-let [[_ old] (find m1 k)]
                          (assoc m k (fun old v))
                          (assoc m k v)))
                m1 m2))]
      (reduce _merge-two r))))

(defcheck solution-1392da22
  (fn
    [f & maps]
    (letfn [(step
              [m [k v]]
              (if (contains? m k)
                (assoc m k (f (get m k) v))
                (assoc m k v)))]
      (reduce #(reduce step %1 %2) maps))))

(defcheck solution-13a5bde5
  (fn [f & x]
    (reduce
      #(reduce (fn [m [k v]]
                 (assoc m k (if (m k) (f (m k) v) v)))
         %1 %2)
      x)))

(defcheck solution-13d66002
  (fn [f & ms]
    (reduce (fn [m [k v]]
              (update-in m [k] #(if %1
                                  (f %1 %2)
                                  %2) v)) {} (mapcat vec ms))))

(defcheck solution-13d6ae54
  (fn [f & ms]
    (letfn [(map-over-map-vals [m f] (zipmap (keys m) (map f (vals m))))
            (maps-with-key [maps k] (filter #(contains? % k) maps))
            (map-vals-for-key [maps k] (vector k (map #(% k) (maps-with-key ms k))))
            (comb [& maps]
              (let [ks (distinct (mapcat keys maps))]
                (into {} (map #(map-vals-for-key maps %) ks))))]
      (map-over-map-vals (apply comb ms) #(reduce f %)))))

(defcheck solution-13f6b6a7
  (fn [f & maps]
    (apply merge (for [x (keys (apply merge maps))]
                   (hash-map x (reduce f (remove nil? (for [m maps]
                                                        (m x)))))))))

(defcheck solution-146c6d3c
  (fn my-merge-with
    [op a & b]
    (let [kv (mapcat seq b)]
      (reduce (fn [a x]
                (if (contains? a (first x))
                  (update-in a [(first x)] op (second x))
                  (assoc a (first x) (second x))))
        a kv))))

(defcheck solution-147482ad
  (fn [f & ms]
    (let [mmap (reduce (fn [acc m]
                         (reduce (fn [bcc k]
                                   (assoc bcc k (conj (get bcc k []) (m k)))) acc (keys m))) {} ms)]
      (apply hash-map (mapcat (fn [[k v]] (vector k (if (> (count v) 1) (apply f v) (first v)))) mmap)))))

(defcheck solution-14d5e853
  (fn merge-with-a-function [f & sqs]
    (reduce

      (fn [a x]
        (reduce

          (fn [a k]
            (let [v (a k)]
              (if (nil? v)
                (assoc a k (x k))
                (assoc a k (f v (x k))))))

          a
          (keys x)))

      {}
      sqs)))

(defcheck solution-14f10ee
  (fn mymerge [f m1 m2 & more]
    (if (empty? m2)
      (if (empty? more)
        m1
        (recur f m1 (first more) (next more))
        )
      (let [k (first (first m2)) v (second (first m2))]
        (if (contains? m1 k)
          (recur f (assoc m1 k (f (m1 k) v)) (next m2) more)
          (recur f (assoc m1 k v) (next m2) more)
          )
        )
      )
    ))

(defcheck solution-1576478f
  (fn [op & args]
    (reduce
      (fn [a b]
        (into (into a b) (for [[k v] a :let [u (b k)] :when u] [k (op v u)])))
      {} args)))

(defcheck solution-159469f2
  (fn [f x & ys]
    (reduce (fn [a [k v]] (assoc a k (if (contains? a k) (f (a k) v) v))) x (into {} ys))))

(defcheck solution-15df63d9
  (fn [f m & args] (reduce
                     (fn [a b] (let [keys (keys b)] (reduce #(if (nil? (% %2)) (assoc % %2 (b %2)) (assoc % %2 (f (% %2)(b %2)))) a keys)))
                     m args)))

(defcheck solution-15e59573
  (fn [f & args]
    (letfn [(entry [a b] (cond (nil? a) b
                               (nil? b) a
                               :else (f a b)))
            (rf [m e] (let [k (key e)]
                        (assoc m k (entry (m k) (val e)))))
            (merge-2 [m1 m2]
              (reduce rf m1 m2))]
      (reduce merge-2 args))))

(defcheck solution-15ef38
  (fn merge-maps [f & maps]
    (let [allkeys (set (mapcat keys maps))]
      (apply conj {}
        (for [k allkeys]
          [k (reduce f
               (remove nil?
                 (for [m maps]
                   (get m k))))])))))

(defcheck solution-16bd82d0
  (fn
    [op & maps]
    (reduce
      (fn
        [map-first map-second]
        (merge
          map-first
          map-second
          (into {}
            ((fn
               [map-a map-b]
               (let
                [dup-keys
                 (keys
                   (select-keys
                     map-b
                     (keys map-a)))]
                 (map
                   (fn
                     [dup-key]
                     {dup-key (op (get map-a dup-key) (get map-b dup-key))})
                   dup-keys))) map-first map-second))))
      maps)))

(defcheck solution-16fb1dcb
  (fn [f & args]
    (let [a (->> (mapcat #(vec %) args)
              (sort-by first)
              (partition-by first))
          b (map (fn [x] [(ffirst x) (map #(second %) x)]) a)]
      (->> (map (fn [x] [(first x) (reduce f (second x))]) b)
        (into (sorted-map))))))

(defcheck solution-1728c703
  (fn merge-with4j
    [f & args]
    (->> args
      (apply concat)
      (group-by first)
      vals
      (map (fn [m]  [(ffirst m)
                     (reduce f (map second m))]))
      (apply concat)
      (apply hash-map))))

(defcheck solution-1788a67f
  (fn [x y & r]
    (reduce (fn [s [a b]]
              (if (contains? s a)
                (assoc s a (x (get s a) b))
                (assoc s a b)))
      y
      (apply merge r))))

(defcheck solution-179314df
  (fn my-merge-with [f m & rest]
    (let [map-merge
          (fn [f m m1]
            (reduce
              (fn [acc x]
                (let [k (first x) v (second x)]
                  (if (contains? acc k)
                    (assoc acc k (f (acc k) v))
                    (conj acc x))))
              m m1))]
      (reduce (partial map-merge f) m rest))))

(defcheck solution-17974fc
  (fn [r f & c]
    (r #(r (fn [m [k v]]
             (assoc m k (if (m k)
                          (f (m k) v)
                          v)))
          % %2)
      c)) reduce)

(defcheck solution-17fbda3
  (fn [f & maps ]
    (letfn [
            (merge1 [f m i] (if (contains? m (first i))
                              (assoc m (first i) (f (m (first i)) (second i)))
                              (assoc m (first i) (second i))))
            (mergemap [f m1 m2] (reduce #(merge1 f %1 %2) m1 m2))
            ]
      (reduce #(mergemap f %1 %2) {} maps))))

(defcheck solution-1863b998
  (fn [f & ms]
    (reduce (fn [acc cur]
              (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (assoc m k (f (m k) v))
                          (assoc m k v)))
                acc
                cur))
      ms)))

(defcheck solution-18921212
  (fn [f & maps]
    (into {} (map
               #(vector (first (first %))
                  (if (= 1 (count %)) (second (first %)) (apply f (map second %))))
               (vals (group-by first (apply concat maps)))))))

(defcheck solution-189853be
  (fn [f & ms]
    (let [ks (distinct (mapcat keys ms))]
      (into {} (map
                 (fn [k]
                   (->> ms
                     (map #(% k))
                     (remove nil?)
                     (#(if (> (count %) 1) (apply f %) (first %)))
                     (vector k)))
                 ks)))))

(defcheck solution-18f2711d
  (fn [f & maps]
    (apply merge
      (map #(hash-map (first %) (if (> (count (second %)) 1)
                                  (apply f (second %)) (first (second %))))
        (for [k (distinct (mapcat keys maps))]
          [k (remove nil? (map #(get % k) maps))])))))

(defcheck solution-19094cad
  (fn ff [f & s]
    (reduce
      (fn [prev next]
        (apply conj prev
          (map #(if (contains? prev (key %))
                  {(key %) (f (prev (key %)) (val %))}
                  %) next)))
      s)))

(defcheck solution-192708e7
  (fn [merge-fn c & cs]
    (letfn
     [(superconj [map entry] (if (contains? map (first entry))
                               (assoc map (first entry) (merge-fn (map (first entry)) (second entry)))
                               (conj map entry)))]
      (reduce superconj c (apply concat cs)))))

(defcheck solution-19708180
  (fn m [f & ms]
    (->> (mapcat identity ms)
      (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2))) {})
      (mapcat (fn [[k v]] [k (reduce f v)]))
      (apply hash-map))))

(defcheck solution-19db70f1
  (fn [f & seqs]
    (reduce (fn [s1 s2]
              (merge s2 (apply hash-map
                          (mapcat (fn [[key value]]
                                    (if (contains? s2 key)
                                      [key (f value (s2 key))]
                                      [key value]))
                            s1))))
      seqs)))

(defcheck solution-1a3aaf1d
  (fn [f & ms]
    (reduce (fn [m1 m2]
              (into m1 (map
                         (fn [[k v]] (if (contains? m1 k)
                                       {k (f (m1 k) v)}
                                       {k v})) m2) ))
      ms)))

(defcheck solution-1ad10c3b
  (fn [f & maps]
    (reduce
      (fn [res m]
        (reduce
          (fn [res [key val]]
            (update-in res [key] #(if % (f % val) val)))
          res
          m))
      maps)))

(defcheck solution-1b3c4af4
  (fn merge-with' [f & ms]
    (reduce
      (fn [acc m]
        (reduce
          (fn [acc [k v]]
            (let [c (get acc k)]
              (assoc acc k (if (nil? c) v (f c v)))))
          acc
          m))
      {}
      ms)))

(defcheck solution-1b6a4b63
  (fn [f & maps]
    (reduce (fn [acc m]
              (reduce-kv (fn [acc k v]
                           (assoc acc k (if-let [acc-v (get acc k)]
                                          (f acc-v v)
                                          v)))
                acc
                m))
      maps)))

(defcheck solution-1b7831e7
  (fn [f & args]
    (let [combine-key (fn [m [k v]] (assoc m k (if (m k) (f (m k) v) v)))
          combine-map (fn [m1 m2] (reduce combine-key m1 m2))]
      (reduce combine-map args)) ))

(defcheck solution-1bbdb719
  (fn [f & maps] (letfn
                  [(apply-item  [h] (fn [[k v]] (let [y (h k)] [k (if y (f v y) v)])))
                   (reduce-maps [a b] (let
                                       [applied (apply hash-map (mapcat (apply-item b) a))]
                                        (merge b applied)))]
                   (reduce reduce-maps maps))))

(defcheck solution-1c0de49a
  (fn [f & maps]
    (letfn [(merge-key [f a b k]
              (let [valA (get a k)
                    valB (get b k)
                    res (if valA (f valA valB) valB)]
                {k res}))
            (merge-all [f a b]
              (let [eu (map (partial merge-key f a b) (keys b))]
                (apply merge (conj eu a))))]
      (reduce (partial merge-all f) maps))))

(defcheck solution-1c49856a
  (fn [f map1 & maps]
    (reduce
      #(let [acc %1 k (key %2) v (acc k) new-v (val %2)]
         (assoc acc k (if v (f v new-v) new-v)))
      map1
      (mapcat (partial into []) maps))))

(defcheck solution-1c4bdcb1
  (fn merge-with2 [f & maps]
    (let [reducer (fn [acc [k v]]
                    (if (contains? acc k)
                      (merge acc {k (f (acc k) v)})
                      (merge acc {k v})))]
      (reduce reducer (first maps) (apply concat (map seq (rest maps)))))))

(defcheck solution-1c7a087a
  (fn mw
    ([f m1 m2]
     (loop [ks (keys m2) m1 m1]
       (if (empty? ks)
         m1
         (let [k (first ks)
               o1 (get m1 k)
               o2 (get m2 k)
               n (if o1 (f o1 o2) o2)]
           (recur (rest ks) (assoc m1 k n))))))
    ([f m1 m2 & more]
     (mw f m1 (apply mw f m2 more)))))

(defcheck solution-1ccecfd3
  (fn f ([g x] x) ([g a b & m]
                   (apply f g
                     (reduce (fn [c [k v]]
                               (assoc c k (if (c k) (g (c k) v) v))) a b)
                     m))))

(defcheck solution-1cd8dfcc
  (fn merg-with [f m & maps]
    (if (empty? maps)
      m
      (let [n (first maps)]
        (apply merg-with
          (concat [f
                   (into {}
                     (for [k (concat (keys m) (keys n))]
                       (let [vm (get m k)
                             vn (get n k)]
                         (if (nil? vm) [k vn]
                                       (if (nil? vn) [k vm]
                                                     [k (f vm vn)])))))]
                  (rest maps)))))))

(defcheck solution-1d397dd8
  (fn [f & ms] (letfn [
                       (common-keys [m1 m2] (clojure.set/intersection (set (keys m1)) (set (keys m2))))
                       (unique-keys [m1 m2] (clojure.set/difference (clojure.set/union (set (keys m1)) (set (keys m2))) (clojure.set/intersection (set (keys m1)) (set (keys m2)))))
                       (merge-2-with-a-function [f m1 m2]
                         (reduce #(assoc %1 %2 (f (get m1 %2) (get m2 %2))) (merge m1 m2) (common-keys m1 m2))
                         )
                       (merge-with-a-function [f & ms]
                         (cond
                           (empty? ms)
                           {}
                           :else
                           (merge-2-with-a-function
                             f
                             (first ms)
                             (apply merge-with-a-function f (rest ms))
                             )
                           )
                         )
                       ] (apply merge-with-a-function f ms))))

(defcheck solution-1d8c367b
  (fn [op & ms]
    (letfn ([mrg [o n]
             (apply assoc o
               (reduce concat
                 (for [e n
                       :let [k (e 0)]
                       :let [oe (find o k)]]
                   (list k (if oe (op (oe 1) (e 1)) (e 1))))))])
      (reduce mrg ms))))

(defcheck solution-1db625c8
  (fn my-merge-with [f & a]
    (letfn [(b [d [e v]]
              (update-in d [e] #(if (nil? %) v (f % v))))
            (c [d e]
              (reduce #(b %1 %2) d e))]
      (reduce c {} a))))

(defcheck solution-1df1848b
  (fn mw [f & ms]
    (into {}
      (for [k (into #{} (mapcat keys ms))]
        (let [[h & t] (keep #(get % k) ms)]
          (if t
            [k (apply f (keep #(get % k) ms))]
            [k h]))))))

(defcheck solution-1e1946fc
  (fn my-merge-with [f & maps]
    (reduce (fn [map1 map2]
              (reduce (fn [map [key value]]
                        (if (contains? map key)
                          (update-in map [key] f value)
                          (assoc map key value)))
                map1 map2))
      maps)))

(defcheck solution-1e1ce5f3
  (fn z [f m & args]
    (let [x (fn [m1 m2]
              (reduce
                #(if (contains? %1 (first %2))
                   (update-in %1 [(first %2)] f (second %2))
                   (conj %1 [(first %2) (second %2)]))
                m1
                m2))]
      (loop [m m args args]
        (if (empty? args)
          m
          (recur (x m (first args)) (rest args)))))))

(defcheck solution-1e5a1ae0
  (fn mymerge-with
    [ f & args ]
    (reduce
      (fn [ x y ] (assoc x y (reduce f (filter identity (map #(% y) args)))))
      {}
      (reduce into #{} (map keys args)))))

(defcheck solution-1e607de3
  (fn [f & maps]
    (letfn [(merge-maps [map1 map2]
              (reduce (fn [acc [k v]]
                        (if-let [current (get acc k)]
                          (assoc acc k (f current v))
                          (assoc acc k v)))
                map1 map2))]
      (reduce merge-maps {} maps))))

(defcheck solution-1e9e609d
  (fn my-merge [f & maps]
    (let [ks (keys (reduce merge maps))
          vs (map #(filter identity %) (map (fn [k] (map #(get % k)  maps))  ks))
          ]
      (into {}  (map #(if (= 1 (count %2))
                        [% (first %2)]
                        [% (reduce f %2)]) ks vs)))))

(defcheck solution-1ec8d666
  (fn [f & maps]
    (letfn [(mval [x y] (if (nil? x) y (f x y)))
            (fassoc [mx [k v]] (assoc mx k (mval (mx k) v)))
            (fmerge [mx my] (reduce fassoc mx (seq my)))]
      (reduce fmerge maps))))

(defcheck solution-1ed05bfd
  (fn my-merge-with [op & maps]
    (loop [f-map (transient (first maps)) r-maps (rest maps)]
      (if (empty? r-maps)
        (persistent! f-map)
        (let [n-map (first r-maps)]
          (doseq [k (keys n-map)]
            (if (nil? (get f-map k))
              (assoc! f-map k (n-map k))
              (assoc! f-map k (op (f-map k) (n-map k)))))
          (recur f-map (rest r-maps)))))))

(defcheck solution-1f393993
  (fn [f x & more]
    (letfn [ (mw [m y]
               (if (empty? y) m
                              (if (contains? m (ffirst y))
                                (recur (assoc m (ffirst y) (f (m (ffirst y)) (last (first y)) ))  (rest y))
                                (recur (assoc m (ffirst y) (last (first y)))  (rest y))
                                )))]
      (mw x (apply concat more)))
    ))

(defcheck solution-1f47bde5
  (fn [fun & sets]
    (reduce (fn [acc c]
              (apply conj (cons acc
                            (map (fn [[k v]]
                                   (if (contains? acc k)
                                     [k (fun (acc k) v)]
                                     [k v]))
                              c))))
      sets)))

(defcheck solution-1f68f64d
  (fn [f & maps]
    (let [merge-two (fn [x y] (reduce (fn [a [k v]] (if (contains? a k)
                                                      (conj a [k (f (a k) v)])
                                                      (conj a [k v])))
                                x y))]
      (reduce merge-two maps))))

(defcheck solution-1f6e5828
  (fn my-merge
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-1f98d484
  (fn [f m & ms]
    (reduce
      (fn [a e]
        (reduce
          (fn [a1 [k v]] (assoc a1 k (if (contains? a k) (f (get a k) v) v)))
          a
          e))
      m
      ms)))

(defcheck solution-1fd302ed
  (fn new-merge-with
    [fun source & args]
    (reduce (fn [amap bmap]
              (reduce (fn [cmap [akey aval]]
                        (let [existing (get cmap akey)]
                          (if existing (assoc cmap akey (fun existing aval))
                                       (assoc cmap akey aval))))
                amap bmap))
      source args)))

(defcheck solution-1fe2390c
  (fn mrgw [f & maps]
    (apply hash-map
      (apply concat
        (for [k (distinct (mapcat keys maps))]
          (let [vs (remove nil? (map #(get % k) maps))]
            [k (if (second vs)
                 (apply f vs)
                 (first vs))]))))))

(defcheck solution-2047e9f1
  (fn m-w
    [f & ms]
    (reduce
      (fn [a b]
        (let [zs [#(hash-map %1 (if (nil? %2) %3 (f %2 %3))) (keys b) (map a (keys b)) (vals b)]]
          (merge a (apply merge (apply map zs))))
        ) ms)))

(defcheck solution-20d373f1
  (fn merge-with-f [f & ms]
    (let [up-first-with-second (fn [f m1 m2]
                                 (loop [result m1, ks (keys m2)]
                                   (if (empty? ks)
                                     result
                                     (let [k (first ks)
                                           r2 (if (contains? m1 k)
                                                (update-in result [k] f (m2 k))
                                                (assoc result k (m2 k)))]
                                       (recur r2 (rest ks))))))]
      (loop [result (first ms), s (rest ms)]
        (if (empty? s)
          result
          (recur (up-first-with-second f result (first s)) (rest s)))))))

(defcheck solution-213afc55
  (fn [f & ms]
    (reduce
      (fn [lm rm]
        (reduce (fn [m [k v]]
                  (if (contains? lm k)
                    (assoc m k (f (m k) v))
                    m))
          (merge rm lm)
          rm))
      {} ms)))

(defcheck solution-21bf52d
  (fn [f & ms]
    (->> ms
      (mapcat vec)
      (group-by first)
      (map #(vector (key %)
              (if (= 1 (count (val %)))
                (second (first (val %)))
                (apply f (map second (val %))))))
      (into {}))))

(defcheck solution-21dea3c9
  (fn [f & maps]
    (reduce (fn [acc m]
              (reduce (fn [acc [k v]]
                        (if (contains? acc k)
                          (update-in acc [k] f v)
                          (assoc acc k v)))
                acc m))
      {} maps)))

(defcheck solution-22373509
  (fn [f & ms]
    (let [vs
          (reduce
            (fn [acc m]
              (reduce
                (fn [ac k] (assoc ac k (concat (get ac k []) (vector (get m k)))))
                acc (keys m))) {} ms)]
      (reduce
        (fn [acc k]
          (let [v (vs k)]
            (if (= (count v) 1)
              (assoc acc k (first v))
              (assoc acc k (apply f v))))) {} (keys vs)))))

(defcheck solution-22374db
  (fn [p & m]
    (into {}
      (for [k (keys (apply merge m))]
        [k
         (reduce p
           (filter
             #(and %)
             (map #(get % k) m)))]))))

(defcheck solution-22abda88
  (fn [f & ms]
    (->> ms
      (mapcat #(into [] %))
      (group-by first)
      (map (fn [[k v]] [k (if (< (count v) 2)
                            (second (first v))
                            (apply f (map second v)))]))
      (into {}))))

(defcheck solution-22d3fa88
  (fn [f & ms]
    (reduce (fn [acc m]
              (reduce (fn [acc [k v]]
                        (assoc acc k (if-let [e (acc k)]
                                       (f e v)
                                       v))) acc m))
      ms)))

(defcheck solution-230eba17
  (fn my-merged-with
    [f & ms]
    (reduce
      (fn merge-in-map
        [aggregate current]
        (reduce
          (fn [aggregate2 k]
            (if (nil? (get aggregate2 k))
              (assoc aggregate2 k (get current k))
              (assoc aggregate2 k (f (get aggregate2 k) (get current k)))))
          aggregate
          (keys current)))
      (first ms)
      (rest ms))))

(defcheck solution-234dfedc
  (fn [f & coll]
    (reduce (fn [m [k v]]
              (if (contains? m k)
                (assoc m k (f (m k) v))
                (assoc m k v))) {} (mapcat #(concat %) coll))))

(defcheck solution-23b8e74c
  (fn [f m & r]
    (reduce (fn [x [k v]]
              (if (x k) (conj x [k (f (x k) v)])
                        (conj x [k v])))
      m
      (for [x r [k v] x] [k v]))))

(defcheck solution-23c3a2f3
  (fn
    [f & maps]
    (reduce
      (fn [result m]
        (let [common-keys (keys (select-keys result (keys m)))
              new-vals (map f (map result common-keys) (map m common-keys))
              new-map (zipmap common-keys new-vals)]
          (merge result m new-map)
          ))
      maps)
    ))

(defcheck solution-23ed1a88
  #(loop [r {} o (apply concat %&)]
     (if (= [] o)
       r
       (let [[k v] (nth o 0)]
         (recur (assoc-in r [k] (if (r k) (% (r k) v) v)) (rest o))))))

(defcheck solution-24317937
  (fn [f m & ms]
    (loop [i ms ret m]
      (if i
        (recur (next i)
          (reduce (fn [a b]
                    (if-let [r (get a (key b))]
                      (assoc a (key b) (f r (val b)))
                      (assoc a (key b) (val b))))
            ret (first i)))
        ret))))

(defcheck solution-24473bc0
  (fn
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-2456da2f
  (fn [f a-map & maps]
    (let [red-func #(assoc %1
                      (first %2)
                      (if (%1 (first %2))
                        (f (%1 (first %2)) (second %2))
                        (second %2)))]
      (loop [lseq maps acc a-map]
        (if (empty? lseq)
          acc
          (recur (rest lseq) (reduce red-func acc (first lseq))))))))

(defcheck solution-246f8c2c
  (fn mw [f m & [h & r]]
    (if h
      (apply mw f
        (reduce (fn [a [k v]] (assoc a k (if-let [av (a k)] (f av v) v))) m h)
        r)
      m)))

(defcheck solution-248ea0fd
  (fn my-merge [f & ms]
    (letfn [(merger [[f m1] m2]
              (let [intr (clojure.set/intersection (set (keys m2)) (set (keys m1)))
                    merged (merge m2 m1)
                    fixed
                    (apply merge {}
                      (for [k intr]
                        {k (f (m1 k) (m2 k))}))]
                [f (merge merged fixed)]))]
      (if (empty? ms) {}
                      (second
                        (reduce merger [f (first ms)] (rest ms)))))))

(defcheck solution-24fcfc18
  (fn [f & maps] (reduce (fn [m [k v]] (assoc m k (reduce f (map second v)))) {} (group-by first (apply concat maps)))))

(defcheck solution-25ec0812
  (fn [f & args]
    (loop [l (rest args), acc (first args)]
      (if-let [h (first l)]
        (recur (rest l) (reduce
                          #(let [[k v] %2]
                             (conj % [k (if-let [x (% k)] (f x v) v)]))
                          acc
                          h))
        acc))))

(defcheck solution-261b7b2b
  (fn [f & maps]
    (let [ks (set (reduce concat #{} (map keys maps)))]
      (zipmap ks
        (map #(let [vs (filter identity (map (fn [m] (m %)) maps))]
                (if (= 1 (count vs))
                  (first vs)
                  (apply f vs)))
          ks)))))

(defcheck solution-261f8f3a
  (fn [o r & m]
    (reduce #(let [a (first %2) b (last %2) c assoc]
               (if
                (nil? (% a))
                 (c % a b)
                 (c % a (o (% a) b))))
      r
      (reduce concat (map (partial into []) m))
      )
    ))

(defcheck solution-2635fcf3
  (fn [op & maps]
    (reduce
      (fn [m1 m2]
        (let [keys1 (set (keys m1))
              keys2 (set (keys m2))
              shared (clojure.set/intersection keys1 keys2)
              unchanged-part (conj
                               (select-keys m1 keys1)
                               (select-keys m2 keys2))
              ]
          (reduce (fn [m k]
                    (assoc m k (op (m1 k) (m2 k))))
            unchanged-part
            shared)))
      maps)))

(defcheck solution-26d50990
  (fn merge-maps [func & maps]
    (letfn [(merge-map [m1 m2]
              (reduce (fn add-entry
                        [result [k v]]
                        (if (contains? result k)
                          (into result {k (func (result k) v)})
                          (into result {k v})))
                m1
                (seq m2)))]
      (reduce merge-map maps))))

(defcheck solution-2710da2f
  #(letfn [(me [m e]
             (let [k (key e) v (val e)]
               (if (contains? m k)
                 (assoc m k (% (get m k) v))
                 (assoc m k v))))]
     (reduce me %2 (seq (apply concat %&)))))

(defcheck solution-278afe10
  (fn mrg-with [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m k]
            (conj m
              [k (cond
                   (and (contains? m1 k) (contains? m2 k)) (f (get m1 k) (get m2 k))
                   (contains? m1 k) (get m1 k)
                   :else (get m2 k)
                   )]
              )
            )
          {} (concat (keys m1) (keys m2))
          )
        )
      maps
      )
    ))

(defcheck solution-27e8e60c
  (fn mw [f m1 m2 & mr]
    (let [mu (reduce #( if (%1 (key %2)) (update-in %1 [(key %2)] f (val %2)) (conj %1 %2) ) m1 m2)]
      (if mr (recur f mu (first mr) (next mr)) mu))))

(defcheck solution-2842f9f
  (fn
    [f & maps]
    (letfn [(addpair [m e]
              (let [k (key e) v (val e)]
                (if (contains? m k)
                  (assoc m k (f (get m k) v))
                  (assoc m k v))))
            (mymerge [m1 m2]
              (reduce addpair m1 m2))]
      (reduce mymerge maps))))

(defcheck solution-286ecf6b
  (fn [f & ms]
    (let [all-keys         (keys (apply merge ms))
          count-containing (fn [k] (count (filter #(contains? % k) ms)))
          count-per-key    (into {} (map (fn [k] [k (count-containing k)]) all-keys))]
      (into {}
        (map
          (fn [k]
            (if (= 1 (count-per-key k))
              [k (some #(get % k) ms)]
              [k (reduce f (filter #(not (nil? %)) (map #(get % k) ms)))]))
          all-keys))
      )))

(defcheck solution-287b3ed1
  (fn [f & ms]
    (letfn [(m [m1 m2]
              (if (empty? m2)
                m1
                (let [[k v] (first m2)]
                  (if (contains? m1 k)
                    (m (assoc m1 k (f (m1 k) v)) (rest m2))
                    (m (assoc m1 k v) (rest m2))))))]
      (reduce m ms))))

(defcheck solution-28990137
  (fn [f & l]
    (let [g (group-by first (apply concat (map #(apply list %) l)))]
      (zipmap (keys g) (map #(reduce f (map second %)) (vals g))))))

(defcheck solution-28aca0e8
  (fn [f & maps]
    (loop [res   (first maps)
           rmaps (rest maps)]
      (if (empty? rmaps)
        res
        (let [current-map (first rmaps)
              new-res (loop [ires res
                             rkeys (keys current-map)]
                        (if (empty? rkeys)
                          ires
                          (let [k (first rkeys)]
                            (if (ires k)
                              (recur (assoc ires k (f (ires k) (current-map k)))
                                (rest rkeys))
                              (recur (assoc ires k (current-map k))
                                (rest rkeys)) ))))
              new-rmaps (rest rmaps)]
          (recur new-res new-rmaps))))
    ))

(defcheck solution-28ce4d9b
  #(into {}
     (for [[k vs] (group-by first (apply concat %&))]
       [k (reduce % (map second vs))])))

(defcheck solution-28ffcc9e
  (fn merge-with* [f & maps]
    (letfn [(merge-two [m1 m2]
              (into {}
                (for [key (into (keys m1) (keys m2))
                      :let [[k1 v1] (find m1 key)
                            [k2 v2] (find m2 key)]]
                  [key (cond (nil? k1) v2
                             (nil? k2) v1
                             :else (f v1 v2))])))]
      (reduce merge-two maps))))

(defcheck solution-2911a6eb
  #(loop [result %2
          xs %&]
     #_(print xs result)
     (if (empty? xs)
       result
       (recur (reduce-kv (fn [acc k v] (assoc acc k (if (acc k) (%1 (acc k) v) v))) result (first xs)) (rest xs)))))

(defcheck solution-29755551
  (fn [f & args]
    (reduce (fn [acc m]
              (let [ks (keys m)]
                (reduce (fn [acc2 k]
                          (let [vacc (get acc k)]
                            (if (nil? vacc)
                              (assoc acc2 k (m k))
                              (assoc acc2 k (f vacc (m k))))))
                  acc
                  ks)))
      args)))

(defcheck solution-297fb351
  (fn [f & ms]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]]
            (if (m k)
              (conj m {k (f (m k) v)})
              (conj m {k v})))
          m1
          m2))
      ms)))

(defcheck solution-29926b42
  (fn merge-with2
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-29cdb91e
  (fn [f & args]
    (into {}
      (map (fn [e] [(e 0)
                    (let [s (map (fn [v] (v 1)) (e 1))]
                      (if (<= (count s) 1)
                        (first s)
                        (apply f s)))])
        (group-by key (apply concat (map (fn [m] (into [] m)) args)))))))

(defcheck solution-29d84e6d
  (fn [f m & l]
    (reduce
      (fn [n s]
        (reduce
          #(if (contains? % (key %2))
             (update-in % [(key %2)] f (val %2))
             (apply assoc % %2))
          n
          s))
      m
      l)))

(defcheck solution-2a7223a5
  (fn my-merge-with [f m & ms]
    (reduce
      (fn [old-map new-map]
        (reduce
          (fn [o [k v]]
            (assoc o k
                     (if (o k)
                       (f (o k) v)
                       v)))
          old-map new-map))
      m
      ms
      )))

(defcheck solution-2aec6e8d
  (fn my-merge-with [f a-map & maps]
    (letfn [(my-merge [k map1 map2] (let [mv1 (get map1 k nil)
                                          mv2 (get map2 k nil)]
                                      (if (and mv1 mv2)
                                        {k (f mv1 mv2)}
                                        {k (or mv1 mv2)})))
            (merge-into [map1 map2] (apply merge (map #(my-merge % map1 map2) (into (into #{} (keys map1)) (keys map2)))))]
      (reduce merge-into a-map maps))))

(defcheck solution-2b195e73
  (fn [f & m]
    (into {}
      (map (fn [e]
             (if (> (count (val e)) 1)
               [(key e) (reduce f (map second (val e)))]
               [(key e) (second (first (val e)))]))
        (group-by first (apply concat m))))))

(defcheck solution-2b2c2085
  (fn mrg-with [f & ms]
    (let
     [ks (set (apply concat (map keys ms)))]
      (reduce (fn [acc k]
                (assoc acc k (reduce f (filter (complement nil?) (map #(% k) ms)))
                           )) {} ks)
      )
    ))

(defcheck solution-2b2e5c97
  (fn [x & y]
    (reduce (fn [a b] (reduce (fn [e f]  (let [f1 (first f)]
                                           (if (nil? (e f1))
                                             (apply assoc e f)
                                             (assoc e f1 (x (e f1) (last f)) )))) a b) ) y)))

(defcheck solution-2b31c8b8
  (fn [f & maps]
    (let [merge-into (fn [m e]
                       (let [k (key e) v (val e)]
                         (if (contains? m k)
                           (assoc m k (f (get m k) v))
                           (assoc m k v))))
          merge-map (fn [m1 m2]
                      (reduce merge-into m1 m2))]
      (reduce merge-map maps))))

(defcheck solution-2b3208d2
  (fn [f & maps]
    (letfn [(merge-pair-into-map [base pair]
              (conj base (if (contains? base (first pair))
                           [(first pair) (f (base (first pair)) (second pair) )]
                           pair)))
            (merge-two-maps [base [& pairs]]
              (reduce merge-pair-into-map base pairs))]
      (reduce merge-two-maps maps))))

(defcheck solution-2b718e93
  (fn [f & ms]
    (reduce #(reduce (fn [m [k v]] (assoc m k (if (contains? m k) (f (m k) v) v)))
               %1 %2)
      ms)))

(defcheck solution-2ba483b
  (fn [f & ms]
    (reduce
      (fn [r [k v]]
        (update-in r [k] #(if % (f % v) v)))
      {}
      (apply concat ms))))

(defcheck solution-2c19eba8
  (fn g [f & [m1 & [m2 & maps :as mm]]]
    (if (nil? maps)
      (reduce (fn [m [k v]]
                (if (m k)
                  (assoc m k (f (m k) v))
                  (assoc m k v)))
        m1 m2)
      (g f m1 (apply g f mm)))))

(defcheck solution-2c55b102
  (fn [f & maps]
    (reduce
      #(reduce (fn [m [k v]]
                 (if (contains? m k)
                   (assoc m k (f (get m k) v))
                   (assoc m k v)))
         %
         %2)
      maps)))

(defcheck solution-2c666fe3
  (fn [f & ms]
    (into {}
      (map
        (fn [[k xs]]
          (if (== 1 (count xs))
            [k (second (first xs))]
            [k (apply f  (map second xs))]))
        (group-by first (apply concat ms))))))

(defcheck solution-2c678825
  (fn merw [f & maps]
    (reduce (fn [m1 m2]
              (reduce (fn [m [k v]]
                        (if-not (contains? m k)
                          (assoc m k v)
                          (update-in m [k] f v)))
                m1 m2))
      maps)))

(defcheck solution-2c810666
  (fn [f & ms]
    (let [fp (fn [a b]
               (if (nil? a) b
                            (f a b)))]
      (reduce (fn [mf m]
                (merge mf
                  (reduce #(assoc %1 %2 (fp (get mf %2) (get m %2)))
                    {} (keys m))))
        {} ms))))

(defcheck solution-2d907200
  (fn [f & maps]
    (let [all-keys (keys (apply merge maps))
          addvalues (fn [akey]
                      (let [data (remove nil? (map #(%1 akey) maps))]
                        (if (> (count data) 1) (apply f data) (first data))))]
      (reduce into {} (
                       map #(assoc {} %1 (addvalues %1))
                       (into [] all-keys))))))

(defcheck solution-2dd9bc04
  (fn mw [f & res]
    (reduce (fn [m n]
              (reduce (fn [mm [k v]]
                        (if (contains? mm k)
                          (update-in mm [k] f v)
                          (assoc-in mm [k] v)))
                m n))
      res)))

(defcheck solution-2e3f1d1e
  (fn [f & maps]
    (reduce
      (fn [merged new-dict]
        (reduce
          (fn [dict [ks v]] (assoc dict ks (let [old-v (merged ks)]
                                             (if (nil? old-v)
                                               v
                                               (f old-v v)))))
          merged
          new-dict))
      (first maps)
      (rest maps))))

(defcheck solution-2e8169c4
  (fn [f & maps]
    (let [ks (into #{} (mapcat keys maps))
          vs (group-by (fn [k] (remove nil? (map #(% k) maps))) ks)]
      (reduce #(conj %1 (if (= 1 (-> %2 first count))
                          [(-> %2 second first) (ffirst %2)]
                          [(-> %2 second first) (apply f (first %2))])) {} vs))))

(defcheck solution-2e84ae2d
  (fn mw
    ([f m1 m2]
     (loop [m1res m1 m2res m2]
       (if-not (seq m2res)
         m1res
         (let [k2 (first (keys m2res))
               v2 (get m2 k2)
               v1 (get m1 k2)
               v1-res (if (nil? v1) v2 (f v1 v2))]
           (recur (assoc m1res k2 v1-res)
             (dissoc m2res k2))))))
    ([f m1 m2 & ms]
     (apply mw f (mw f m1 m2) ms))))

(defcheck solution-2ea3c946
  (fn [f & ms]
    (into {} (for [ky (set (mapcat keys ms))]
               (let [vls (for [m ms :when (contains? m ky)] (m ky))]
                 [ky (if (> (count vls) 1)
                       (apply f vls)
                       (first vls))])))))

(defcheck solution-2eb18400
  (fn [f m1 m2 & more]
    (persistent!
      (let [merge-two (fn [f map-1 map-2]
                        (loop [m1 map-1
                               entries (into [] map-2)]
                          (if (seq entries)
                            (let [entry (first entries)
                                  k (first entry)]
                              (if-let [m1-val (get m1 k)]
                                (recur
                                  (conj! m1 [k (f m1-val (last entry))])
                                  (rest entries))
                                (recur (conj! m1 entry) (rest entries))))
                            m1)))
            ret (merge-two f (transient m1) m2)]
        (if (seq more)
          (reduce (partial merge-two f) ret more)
          ret)))))

(defcheck solution-2ecaaab2
  (fn [f m1 & ms]
    (reduce (fn [m [k v]] (update-in m [k] #(if % (f % %2) %2) v)) m1 (apply concat ms))))

(defcheck solution-2ee29b68
  (fn[f & xs]
    (reduce #(reduce (fn [m [k v]] (assoc m k (if (contains? m k) (f (m k) v) v))) %1 %2) xs)))

(defcheck solution-314074bb
  (fn [f & ms] (->> ms
                 (apply concat)
                 (group-by first)
                 (map (fn [[k v]] [k (reduce f (map second v))]))
                 (into {}))))

(defcheck solution-3142af2e
  (fn mer-with [op & maps]
    (letfn [(get-keys [ & maps]
              (reduce into #{} (map keys maps)))
            (single? [key maps]
              (= (count (filter #(not (nil? %)) (map #(% key) maps))) 1))
            (vl [key maps]
              (filter #(not (nil? %)) (map #(% key) maps)))]
      (let [ks (apply get-keys maps)]
        (into {} (for [k ks]
                   (if (single? k maps)
                     [k (first (vl k maps))]
                     [k (apply op (vl k maps))])))))))

(defcheck solution-3187156a
  (fn [f base & others] (reduce
                          (fn [res n] (let [k (key (first n))
                                            v (val (first n))
                                            curVal (res k)]
                                        (if curVal (assoc res k (f curVal v)) (conj res n))))
                          base (for [c others [k v] c] {k v}))))

(defcheck solution-31a4ff41
  (fn [f & m]
    (reduce
      (fn [m [k v]] (conj m [k (if (m k) (f (m k) v) v)]))
      {}
      (apply concat (map seq m)))))

(defcheck solution-31f5175e
  (fn
    [f & ms]
    (if ms
      (let [my-reduce
            (fn [f m1 m2]
              (if-let [s (seq m2)]
                (loop [res m1 entries s]
                  (if-let [entry (first entries)]
                    (recur (if (contains? res (first entry))
                             (assoc res (first entry) (f (res (first entry)) (second entry)))
                             (assoc res (first entry) (second entry)))
                      (next entries))
                    res))
                m1))]
        (loop [res (first ms) oth-ms (next ms)]
          (if (nil? oth-ms)
            res
            (recur (my-reduce f res (first oth-ms)) (next oth-ms)))))
      nil)))

(defcheck solution-32af544f
  (fn mrec
    ([f col1 col2]
     (let [fun
           (fn [col f [k v]]
             (assoc col k
                        (if-let [ov (get col k)]
                          (f ov v)
                          v)))]
       (if-let [[fir & n] (seq col2)]
         (fun (mrec f col1 n) f fir)
         col1)))
    ([f col1 col2 & rcol]
     (apply mrec f (mrec f col1 col2) rcol))))

(defcheck solution-33298100
  (fn merge-map [f xs & xs2]
    (let [assoc1 (fn [xs [k v]]
                   (let [val-in-result (get xs k)]
                     (if val-in-result
                       (assoc xs k (f val-in-result v))
                       (assoc xs k v))))]
      (reduce (fn[xs m]
                (reduce assoc1 xs m)) xs xs2))))

(defcheck solution-332a9e49
  (fn [f & m]
    (reduce #(reduce
               (fn[h [k v]]
                 (assoc h k (if (h k) (f (h k) v) v)))
               % %2) m)))

(defcheck solution-33b890b0
  (fn merge-with'
    ([f m]
     m)
    ([f m m1 & ms]
     (let [merged (reduce #(assoc %1 (first %2)
                                     (if-let [val (get %1 (first %2))]
                                       (f val (second %2))
                                       (second %2)))
                    m
                    m1)]
       (apply merge-with' (concat [f merged] ms))))))

(defcheck solution-34168c2e
  (fn my-merge-with [f & colls]
    (let [flat (mapcat seq colls)
          grouped (group-by first flat)
          result (into {} (for [[k v] grouped] [k (if (> (count v) 1)
                                                    (apply f (map second v))
                                                    (second (first v)))]))]
      result)))

(defcheck solution-34549a89
  (fn __ [f & dicts]
    (let [allkeys (set (mapcat keys dicts))]
      (apply hash-map
        (mapcat (fn [k]
                  (let [vals (->> dicts
                               (map #(get % k :nil))
                               (filter #(not= % :nil)))]
                    [k (if (= 1 (count vals)) (first vals) (apply f vals))]))
          allkeys)))))

(defcheck solution-34757eeb
  (fn myMergeWith [f & args]
    (let [mergeOneElem (fn [m e]
                         (let [k (key e)
                               v (val e)]
                           (if (contains? m k)
                             (assoc m k (f (get m k) v))
                             (assoc m k v))))
          mergeTwoMaps (fn [m1 m2]
                         (reduce mergeOneElem m1 (seq m2)))]
      (reduce mergeTwoMaps (first args) (rest args)))))

(defcheck solution-348036ef
  (fn [f & maps](reduce (fn[upper-result _map]
                          (reduce (fn[result [key val]]
                                    (conj result {key (if (nil? (get result key))
                                                        val
                                                        (f (result key) val))})) upper-result _map)) {} maps)))

(defcheck solution-3527e66a
  (fn [op & maps]
    (reduce
      (partial
        (fn my-merge-with [merged map1 map2]
          #_(println (map1 map2))
          (let [map1Keys (keys map1)]
            (if (empty? map1Keys)
              (conj merged map2)
              (let [fkey (first map1Keys) ]
                (if (contains? map2 fkey)
                  (recur (conj merged {fkey (op (get map1 fkey) (get map2 fkey))}) (dissoc map1 fkey) (dissoc map2 fkey))
                  (recur (conj merged {fkey (get map1 fkey)}) (dissoc map1 fkey) map2)
                  )
                )
              )
            )
          ) {}
        )
      maps
      )
    ))

(defcheck solution-35473749
  (fn my-merge-with [f first-map & maps]
    (let [key-and-bindings
          (group-by first (apply concat [] first-map maps))] ;; groups each key with its corresponding bindings
      (into {} (map #(vector
                       (first %)
                       (reduce f (map second (second %)))) key-and-bindings)))))

(defcheck solution-35f7ed8b
  (fn [f & ms]
    (reduce (fn [acc m]
              (reduce (fn [acc [k v]]
                        (let [new-val (if (contains? acc k)
                                        (f (get acc k) v)
                                        v)]
                          (assoc acc k new-val)))
                acc
                m))
      {}
      ms)))

(defcheck solution-36345bd5
  (fn [f & ms]
    (reduce
      #(into
         %1
         (map (fn [[k v]] (if (contains? %1 k) {k (f (%1 k) v)} {k v})) (seq %2)))
      {}
      ms)))

(defcheck solution-3680f675
  (fn mrg([f m1 m2] (
                      reduce
                      (fn[acc val]
                        (if (get acc val)
                          (assoc acc val (f (get acc val) (get m2 val)))
                          (assoc acc val (get m2 val))
                          ))
                      m1
                      (keys m2))
          )
    ([f m1 m2 & more] (reduce #(mrg f %1 %2) (mrg f m1 m2) more)  )

    ))

(defcheck solution-36bb9a3c
  (fn __ [f & ms]
    (->> (for [m ms
               k (keys m)]
           [k (get m k)])
      (group-by first)
      (map (fn [entry]
             [(first entry)
              (reduce f
                (map second (second entry)))]))
      (into {}))))

(defcheck solution-36bbe90
  (fn [f m & ms]
    (into {}
      (for [e ms [k v] e]
        (if (contains? m k) [k (f (m k) v)] [k v])))))

(defcheck solution-36c1cfc0
  (fn mergef [f base & more]
    (let [merge-in (fn [b coll]
                     (reduce (fn [acc [k v]] (if (contains? acc k) (update-in acc [k] f v) (conj acc [k v]))) b coll))]
      (reduce merge-in base more))))

(defcheck solution-36d512ed
  (fn [f & maps]
    (let[merge-kvs
         (fn [m [k v]]
           (if (contains? m k)
             (assoc m k (f (get m k) v))
             (assoc m k v)))
         merge-all
         (fn [m1 m2]
           (reduce merge-kvs m1 m2))]
      (reduce merge-all maps))
    ))

(defcheck solution-37676b86
  (fn m-w [f & maps]
    (let [all-keys (keys (apply merge maps))]
      (apply hash-map
        (interleave
          all-keys
          (for [k all-keys]
            (let [with-k (filter #(contains? % k) maps)]
              (reduce
                #(f %1 (%2 k))
                ((first with-k) k)
                (rest with-k)))))))))

(defcheck solution-3798ba7
  (fn [f & args]
    (let [my-keys (distinct (flatten (map keys args)))
          my-items (map (fn [k] (filter (complement nil?) (reduce #(conj %1 (get %2 k)) [] args))) my-keys)
          my-vals (map #(if (= 1 (count %)) (first %) (apply f %)) my-items)]
      (apply hash-map (interleave my-keys my-vals)))))

(defcheck solution-37eb09f1
  (fn [op & maps]
    (let [upd-val #(fn [v] (if (nil? v) % (op v %)))
          upd #(update-in %1 [%2] (upd-val %3))
          join #(reduce-kv upd %1 %2)]
      (reduce join {} maps))))

(defcheck solution-38c359ce
  (fn my-merge-width [f c & cols]
    (reduce
      #(reduce
         (fn [m [k v]] (assoc m k (if-let [w (m k)] (f w v) v))) %1 %2) c cols)))

(defcheck solution-38e4f5e7
  (fn [f m & ms]
    (let [kvs (apply concat (map seq ms))]
      (reduce (fn [x [k v]]
                (if (contains? x k)
                  (update-in x [k] f v)
                  (assoc x k v))) m kvs))))

(defcheck solution-39208d20
  (fn [fnc m & maps]
    (let [all-keys (set (mapcat keys maps))]
      (loop [ret m
             maps maps
             ks (keys (first maps))]
        #_(println "maps: " maps)
        #_(println "keys: " ks)
        #_(println "ret : " ret)
        (if (empty? maps)
          ret
          (if (empty? ks)
            (recur ret (rest maps) (if (next maps) (keys (fnext maps)) []))
            (let [new-val ((first maps) (first ks))
                  old-val (ret (first ks))]
              #_(println "old-val: " old-val)
              #_(println "new-val: " new-val)
              (if old-val
                (recur (update-in ret [(first ks)] fnc new-val) maps (rest ks))
                (recur (assoc ret (first ks) new-val) maps (rest ks))))))))))

(defcheck solution-393008d4
  (fn [f m0 & maps]
    (if (empty? maps) m0
                      (reduce (fn [acc-m nxt-m]
                                (reduce-kv
                                  (fn [m k v]
                                    (assoc m k
                                             (if (contains? m k)
                                               (f (m k) v)
                                               v)) )
                                  acc-m nxt-m))

                        m0 maps))))

(defcheck solution-395d58b2
  (fn [f & maps]
    (reduce
      (fn [tm m]
        (reduce (fn [tm k]
                  (if (nil? (get tm k))
                    (assoc tm k (get m k))
                    (assoc tm k (f (get tm k) (get m k)))
                    )) tm (keys m))
        )
      {} maps
      )))

(defcheck solution-39bee34e
  (fn [f m & r]
    (reduce (fn [m n]
              (reduce-kv (fn [m k v]
                           (if-let [cv (m k)]
                             (assoc m k (f cv v))
                             (assoc m k v))) m n)) m r)))

(defcheck solution-39f362d8
  (fn [f i & maps]
    (reduce (fn [j [k v]] (merge j (hash-map k (if (nil? (get i k)) v (f (get i k) v))))) i (apply merge maps))))

(defcheck solution-3a1bbc64
  (fn my-merge-with [f & maps]
    (letfn ((map-merge [f m1 m2]
              (reduce (fn [m v]
                        (if (contains? m (first v))
                          (assoc m (first v) (f (get m (first v)) (second v)))
                          (conj m v)))
                m1 m2)))
      (reduce #(map-merge f %1 %2) maps))))

(defcheck solution-3a5082cb
  (fn merge-with-fun
    ([f m1 m2]
     (let [ks (clojure.set/union (set (keys m1)) (set (keys m2)))]
       (reduce #(let [v1 (get m1 %2), v2 (get m2 %2)]
                  (if (and v1 v2)
                    (assoc % %2 (f v1 v2))
                    (if v1 % (assoc % %2 v2))))
         m1 ks)))
    ([f m1 m2 & ms]
     (let [m1 (merge-with-fun f m1 m2), [m2 & ms] ms]
       (apply merge-with-fun f m1 m2 ms)))))

(defcheck solution-3a5beae1
  (fn [f x & xs]
    (reduce (fn [acc1 s]
              (reduce
                (fn  [acc2 [k v]]
                  (assoc  acc2 k
                               (let [pv (get acc2 k)]
                                 (if (nil? pv)  v  (f pv v)))))
                acc1 s))
      x xs)))

(defcheck solution-3a6dc660
  (fn [f & maps ]
    (reduce
      #(assoc % (first %2)
                (reduce f (filter (complement nil?) (map (fn [m] (get m (first %2))) maps)) )
                )
      {} (apply merge maps))
    ))

(defcheck solution-3a9a49ab
  (fn [f & maps]
    (reduce
      #(if-let [v (%1 (%2 0))]
         (conj %1 [(%2 0) (f v (%2 1))])
         (conj %1 %2))
      (first maps)
      (mapcat seq (rest maps)))))

(defcheck solution-3aa21f3
  (fn mymerge [f & s]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]]
            (if (m k)
              (assoc m k (f (m k) v))
              (assoc m k v)))
          m1 m2))
      {} s)))

(defcheck solution-3b314d95
  (fn [f m & ms]
    (reduce
      (fn [m n]
        (reduce
          (fn [m [k v]]
            (assoc m k
                     (if (contains? m k)
                       (f (m k) v)
                       v)))
          m n))
      m ms)))

(defcheck solution-3ba88be4
  (fn merge-w [f a & bs]
    (reduce
      (fn [a b]
        (reduce
          (fn [sofar k]
            (assoc sofar k (if (a k) (if (b k) (f (a k) (b k)) (a k)) (b k))))
          {}
          (into #{} (concat (keys a) (keys b))))) a bs)))

(defcheck solution-3bf0cc68
  (fn merge-with-2
    ([] {})
    ([f coll1 coll2]
     (reduce
       (fn[m [k v]]
         (if (coll? v)
           (update-in m [k]
             (fnil (partial f v) nil))
           (update-in m [k]
             (fnil (partial f v) 1))))
       coll2 coll1))
    ([f coll1 coll2 & colls]
     (apply merge-with-2 f (merge-with-2 f coll1 coll2) colls))))

(defcheck solution-3c43e918
  (fn [f & maps]
    (reduce
      (fn [acc m]
        (->> (for [[k v] m]
               [k (if (contains? acc k)
                    (f (acc k) v)
                    v)])
          (into {})
          (merge acc)))
      maps)))

(defcheck solution-3c8a78a3
  #(->> (group-by first (apply concat [] %&))
     (map (fn [[k v]] [k (reduce % (map second v))]))
     (into {})))

(defcheck solution-3ca98394
  (fn [op & s] (reduce
                 #(into (conj % %2) (for [[k1 v1] % [k2 v2] %2]
                                      (if (= k1 k2) [k1 (op v1 v2)]))) s)))

(defcheck solution-3cb3751c
  (fn [f & cs] (reduce (fn [m [k v]] (let [v2 (if (contains? m k) (f (m k) v) v)] (assoc m k v2))) {} (apply concat cs))))

(defcheck solution-3d545fb9
  (fn
    [f & maps]
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry m1  m2))]
      (reduce merge2 maps))))

(defcheck solution-3d81446d
  (fn my-merge-with [f & ms]
    (let [merge2 (fn [m1 m2]
                   (conj m1
                     (into {}
                       (for [[k v] m2]
                         (if (contains? m1 k)
                           [k (f (get m1 k) v)]
                           [k v])))))]
      (reduce merge2 ms))))

(defcheck solution-3d969fa9
  (fn m-with [f & ms]
    (letfn [(m-entry [m [k v]]
              (if (contains? m k)
                (assoc m k (f (m k) v))
                (assoc m k v)))
            (m-hash [m1 m2]
              (reduce m-entry m1 (seq m2)))]
      (reduce m-hash ms))))

(defcheck solution-3e094c4c
  (fn [f, & ms]

    (apply hash-map (apply concat
                      (for [k (set (mapcat keys ms))]
                        (list k (reduce f (map #(get %1 k) (filter #(contains? %1 k) ms)))))))))

(defcheck solution-3e348834
  (fn my-merge-with
    ([f] nil)
    ([f m] m)
    ([f m1 m2]
     (if (empty? m2)
       m1
       (reduce (fn [result [k v]]
                 (assoc result k (if-not (contains? result k)
                                   v
                                   (f (result k) v))))
         m1 m2)))
    ([f m1 m2 & ms]
     (reduce (partial my-merge-with f) m1 (cons m2 ms)))))

(defcheck solution-3e3b2b11
  (fn my-merge-with
    [f m & maps]
    (if (empty? maps)
      m
      (recur f
        (let [m2 (first maps)]
          (loop [m1 m
                 ks (keys m2)]
            (if (empty? ks)
              m1
              (recur (let [k (first ks)
                           v (get m2 k)]
                       (if (contains? m1 (first ks))
                         (assoc m1 k (f (get m1 k) v))
                         (assoc m1 k v)))
                (rest ks)))))
        (rest maps)))))

(defcheck solution-3e53b8cb
  (fn
    [f mi & ms]
    (reduce
      (fn [ma mb]
        (reduce
          (fn [m [k v]] (assoc m k (if (contains? m k) (f (get m k) v) v)))
          ma mb))
      mi ms)))

(defcheck solution-3ebfb5b1
  (fn [f & xs]
    (reduce
      (fn [a b]
        (reduce-kv
          #(update-in % [%2] (fn [x] (if x (f x %3) %3)))
          a b)) {} xs )))

(defcheck solution-3eec52d7
  (fn [f & args]
    (apply merge
      (map #(sorted-map (first %)
              (if (> (count (second %)) 1)
                (apply f (for [x (range (count (second %)))] (second (nth (second %) x))))
                (second (first (second %)))))
        (group-by first (apply concat args))))))

(defcheck solution-3f17e6af
  (fn [f & maps]
    (letfn [(mergeWith [m1 m2]
              (let [filterFunction #(contains? m1 (first %))
                    newItems (remove filterFunction m2)
                    toMergedKeys (keys (filter filterFunction m2))
                    mergeItems (into {} (map (fn [x] [x (f (m1 x) (m2 x))]) toMergedKeys))]
                (into m1 [newItems mergeItems])))]
      (reduce mergeWith maps))))

(defcheck solution-40daffc4
  #((fn mrg [f x u]
      (if (empty? u)
        x
        (if (= (get x (first (first u))) nil)
          (mrg f (assoc x (first (first u)) (second (first u))) (rest u))
          (mrg f (assoc x (first (first u)) (f (get x (first (first u))) (second (first u)))) (rest u))))) %1 %2 (into [] (apply conj {} %&))))

(defcheck solution-414c59b0
  (fn [f & ms]
    (reduce (fn [m kv]
              (update-in m [(first kv)]
                #(if (nil? %) (second kv)
                              (f % (second kv)))))
      {} (apply concat ms))
    ))

(defcheck solution-4171f39c
  (fn mergef [op m1 & [m2 :as ls]]
    (if (or (empty? m2) (nil? m2)) m1
                                   (apply mergef (flatten (list op (loop [ls (seq m2) acc m1]
                                                                     (cond (empty? ls) acc
                                                                           (contains? acc (ffirst ls))
                                                                           (recur (rest ls) (-> acc (conj {(ffirst ls) (op  (get acc (ffirst ls))  (second (first ls))) } )))
                                                                           :else (recur (rest ls) (-> acc (conj (first ls)))))) (rest ls)))))))

(defcheck solution-41844ff
  (fn [f start & rest]
    (reduce (fn [cur newm]
              (let [n (set (keys newm))
                    common (clojure.set/intersection (set (keys cur)) n)
                    c (merge cur (select-keys newm (clojure.set/difference n common)))]
                (reduce #(assoc %1 %2 (f (%1 %2) (newm %2))) c common)))
      start rest)))

(defcheck solution-41f4da4a
  (fn [f & ms]
    (apply hash-map
      (apply concat
        (for [[k, v] (group-by first (apply concat ms))]
          [k (reduce f (map second v))])))))

(defcheck solution-41f70368
  (fn [f & s]
    (reduce #(reduce (fn [m [k v]] (assoc m k (if (m k) (f (m k) v) v))) % %2) s)))

(defcheck solution-425ffe12
  (fn [f m & maps]
    (letfn [(add_entry [m k v] (assoc m k (if (m k) (f (m k) v) v)))
            (merge_with_single [m1 m2] (if-not (empty? m2) (let [[k v] (first m2)] (recur (add_entry m1 k v) (dissoc m2 k))) m1))
            (merge_with_all [m maps] #_(println m maps) (if-not (empty? maps) (recur (merge_with_single m (first maps)) (rest maps)) m))]
      (merge_with_all m maps))))

(defcheck solution-4272e08e
  (fn m [f & [h & t]]
    (reduce
      (fn [acc [k v]]
        (assoc acc k (if-let [p (acc k)](f p v) v)))
      h (mapcat seq t))))

(defcheck solution-427a0af9
  (fn [f & coll]
    (apply merge
      (for [grp (group-by #(first %) (reduce into [] coll))]
        {(first grp) (let [[c & ls] (map second (second grp))] (if (> (count ls) 0) (apply f c ls) c))}))))

(defcheck solution-428b6892
  (let [merger
        (fn [f m1-keys m1 m2 result]
          (if (nil? m1-keys)
            (merge result m2)
            (if (contains? m2 (first m1-keys))
              (recur f (next m1-keys) m1 (dissoc m2 (first m1-keys))
                (assoc result (first m1-keys) (f
                                                (get m1 (first m1-keys))
                                                (get m2 (first m1-keys))
                                                )
                              )
                )
              (recur f (next m1-keys) m1 m2
                (assoc result (first m1-keys) (get m1 (first m1-keys)))
                )
              )
            )
          )
        doit
        (fn [f ms result]
          (if (nil? ms)
            result
            (recur f (next ms) (merger f (keys result) result (first ms) {}))
            )
          )]
    (fn [f & ms] (doit f (next ms) (first ms)))
    ))

(defcheck solution-42cce1e2
  (fn mw [f & maps]
    (->> maps
      (apply concat)
      (group-by first)
      (reduce (fn [m [k vs]] (assoc m k (reduce f (map second vs)))) {}))))

(defcheck solution-43024e79
  (fn foo [f init & xs]
    (reduce (fn [acc [k v]]
              (assoc acc k
                         (if-let [ov (get acc k)]
                           (f ov v)
                           v)))
      init (apply concat xs))))

(defcheck solution-431c1b44
  (fn [f & m]
    (into {}
      (map (fn [[a b]]
             [a (reduce f (map second b))])
        (group-by first (mapcat vec m))))))

(defcheck solution-436d7eaf
  (fn mergef [f m1 m2 & m3]
    #_(println m1 m2)
    (let [ks (clojure.set/union (set (keys m1)) (set (keys m2))),
          merged (into {}
                   (for [k ks]
                     (cond (and (m1 k) (m2 k)) [k (f (m1 k) (m2 k))],
                           (m1 k) [k (m1 k)],
                           (m2 k) [k (m2 k)])))]
      (if (empty? m3)
        merged
        (apply mergef f merged m3)))))

(defcheck solution-43887c6d
  (fn [f & maps]
    (reduce #(reduce
               (fn [x [k v]]
                 (assoc x k
                          (if (contains? x k) (f (x k) v) v))) %1 %2)
      {} maps)))

(defcheck solution-4404f8bb
  (fn merge-with' [f & ms]
    (let [merge-entry (fn [m [k v]]
                        (if (contains? m k)
                          (assoc m k (f (m k) v))
                          (assoc m k v)))
          merge-map #(reduce merge-entry %1 %2)]
      (reduce merge-map {} ms))))

(defcheck solution-443b4737
  (fn [f m & ms]
    (let [mymerge
          (fn [m1 m2]
            (reduce #(if (contains? %1 (key %2))
                       (->> (f (%1 (key %2)) (val %2))
                         (hash-map (key %2))
                         (merge %1))
                       (conj %1 %2)) m1 m2))]
      (reduce mymerge m ms))))

(defcheck solution-4469ed91
  (fn [f & ms]
    (letfn [(assoc-f [acc [k v]] (assoc acc k (if (acc k) (f (acc k) v) v)))
            (merge2 [m1 m2] (reduce assoc-f m1 m2))]
      (reduce merge2 ms))))

(defcheck solution-4490a12e
  (fn [f & colls]
    (reduce
      (fn [m [k v]] (if (m k) (assoc m k (f (m k) v)) (assoc m k v)))
      (hash-map) (apply concat colls))))

(defcheck solution-4493d085
  (fn
    [f & maps]
    (let [K (set (mapcat keys maps))
          V (map (fn [k]
                   (vector k (filter #(not (nil? %))
                               (map #(get % k) maps)))) K)]
      (apply hash-map
        (mapcat #(vector (first %) (reduce f (second %))) V)))))

(defcheck solution-44d2000c
  (fn [f & m]
    (reduce
      #(reduce
         (fn [m [k v]]
           (assoc m k (if (m k) (f (m k) v) v)))
         % %2)
      m)))

(defcheck solution-44e8b7c0
  (fn [f v1 & vs]
    (let [v2 (apply conj {} vs)]
      (let [k1 (apply hash-set (keys v1)) k2 (apply hash-set (keys v2))]
        (conj
          (reduce #(assoc %1 %2 (f (v1 %2) (v2 %2))) {} (clojure.set/intersection k1 k2))
          (reduce #(assoc %1 %2 (v1 %2)) {} (clojure.set/difference k1 k2))
          (reduce #(assoc %1 %2 (v2 %2)) {} (clojure.set/difference k2 k1)))))))

(defcheck solution-452ee130
  (fn [f & a]
    (let [k (set (mapcat keys a))]
      (->> k
        (map (fn [x] (reduce f (filter #(not= nil %) (map #(% x) a)))))
        (zipmap k)
        )
      )
    ))

(defcheck solution-4546a2b2
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (loop [m m2 res m1]
          (if (empty? m) res
                         (let [[k v] (first m)]
                           (recur (rest m)
                             (if (contains? res k)
                               (assoc res k (f (res k) v))
                               (assoc res k v)))))))
      maps)))

(defcheck solution-45566164
  (fn [f & maps]
    (->> maps
      (apply concat)
      (group-by key)
      (map (juxt key #(->> (map val (val %))
                        (reduce f))))
      (into {}))))

(defcheck solution-45ba56c4
  (fn [f & ms]
    (reduce #(reduce (fn [m [k v]]
                       (if (contains? m k)
                         (assoc m k (f (m k) v))
                         (assoc m k v)))
               %1 %2)
      {} ms)))

(defcheck solution-45be3da4
  (fn mw-2 [f & maps]
    (let [r (fn [i a]
              (reduce-kv
                (fn [m k v]
                  (let [existing (get m k)]
                    (assoc m k (if existing (f existing v) v))))
                i a))]
      (reduce (fn [v item] (r v item)) (first maps) (rest maps)))))

(defcheck solution-45e1fcd4
  (fn [f & s]
    (let [allkeys (set (mapcat keys s))
          projector (fn [v] (map (fn [m] (get m v)) s))
          allvals (map projector allkeys)
          nonnil-vals (map #(filter identity %) allvals)
          func (fn [v] (if (>= 1 (count v)) (first v) (apply f v)))
          allmerged (map func nonnil-vals)]
      (zipmap allkeys allmerged)
      )
    ))

(defcheck solution-460479db
  (fn m-w
    ([f m n]
     (reduce (fn [m' [k v]]
               (if-let [u (m' k)]
                 (assoc m' k (f u v))
                 (assoc m' k v)))
       m n))
    ([f m n & etc]
     (reduce #(m-w f % %2) (list* m n etc)))))

(defcheck solution-464d3be1
  (fn [f m1 m2 & mx]
    (let [reducer (fn [m [k v]]
                    (assoc m k (or (and (m k)
                                        (f (m k) v))
                                   v)))]
      (loop [m (reduce reducer m1 m2)
             ms mx]
        (if (empty? ms)
          m
          (recur (reduce reducer m (first ms)) (rest ms)))))))

(defcheck solution-46c69365
  (fn[f & xs]
    (reduce
      (fn [r x]
        (merge x (reduce
                   (fn[res [k v]]
                     (if (contains? x k)
                       (assoc res k (f v (x k)))
                       (assoc res k v))) {} r)))
      (first xs) (next xs))))

(defcheck solution-4701f3db
  (fn [op & m] (reduce (fn [m1 m2]
                         (reduce (fn [m [k v]] (assoc m k (if (contains? m k) (op (get m k) v) v))) m1 m2)) m)))

(defcheck solution-47257c20
  (fn [f & input]
    (let [keys (apply clojure.set/union (map #(set (keys %)) input))]
      (apply merge (for [k keys]
                     {k (reduce f
                          (for [i input :when (contains? i k)]
                            (i k)))})))))

(defcheck solution-473685e
  (fn [f & ms]
    (reduce (fn [a b]
              (if (empty? a)
                b
                (loop [a a
                       b b]
                  (if (empty? b)
                    a
                    (let [[k v] (first b)
                          r (a k)]
                      (recur (assoc a k (if (nil? r) v (f r v))) (dissoc b k)))))))
      {}
      ms)))

(defcheck solution-474a662c
  (fn [f & ms] (let [ddon
                     (fn ([a] a)
                       ([a b] (f a b)))]
                 (into {} (map (fn [[k vv]]
                                 [k (apply ddon vv)])
                            (reduce
                              (fn [mmap [k v]]
                                (assoc
                                 mmap
                                  k
                                  (conj (mmap k []) v)))
                              (sorted-map)
                              (apply concat
                                (map (partial into []) ms)))
                            )
                   )
                 )))

(defcheck solution-474c70d6
  (fn [ f & maps ]
    (letfn [ (add-val [ m [k v]] (if (contains? m k)
                                   (assoc m k (f (m k) v))
                                   (assoc m k v)))

            (add-all  [ m n ] (reduce add-val m n)) ]
      (reduce add-all maps))))

(defcheck solution-47c18289
  (fn [f first-map & other-maps]
    (loop [[m & ms] other-maps result1 first-map]
      (if (nil? m) result1
                   (recur ms
                     (loop [[[k v :as entry] & entries] (seq m), result2 result1]
                       (cond
                         (empty? entry) result2
                         (get result2 k) (recur entries (assoc result2 k (f (get result2 k) v)))
                         :else (recur entries (assoc result2 k v)))))))))

(defcheck solution-47f524ab
  (fn [ f & ms ]
    (letfn   [ (mw [m1 m2]   (let [k1   (keys m1)
                                   k2   (keys m2)
                                   k12  (clojure.set/intersection (set k1) (set k2))
                                   m12  (apply merge (map #(hash-map % (f (m1 %) (m2 %)))  k12))]

                               (merge (merge m1 m2) m12)))]
      (reduce mw ms))))

(defcheck solution-48195d02
  (fn [f m1 & m2]
    (let [m2 (mapcat concat m2)]
      (reduce (fn [t val] (let [[k v] val] (if (contains? t k) (update-in t [k] f v) (assoc t k v)))) m1 m2))))

(defcheck solution-48edd7b
  (fn mergef [f & s] (
                       reduce (fn [result m]
                                (reduce (fn [a b] (let [k (first b) v (last b)] (if (contains? a k) (assoc a k (f (get a k) v)) (into a [b])))) result m)
                                ) (first s) (rest s)
                       )))

(defcheck solution-4907a5c3
  (fn [f m & o]
    (reduce
      (fn [m [x y]]
        (assoc m x (if-let [t (m x)] (f t y) y)))
      m
      (apply concat o))))

(defcheck solution-4978eada
  (fn [f acc & ms]
    (reduce
      (fn [acc m]
        (reduce
          (fn [acc [k v]]
            (if (contains? acc k)
              (update-in acc [k] f v)
              (assoc acc k v)))
          acc m))
      acc ms)))

(defcheck solution-4a00f7a9
  (fn [f & args]
    (reduce
      (fn [r, a]
        (reduce
          #(let [[k v] %2] (if (contains? r k) (conj %1 [k (f (r k) v)]) (conj %1 %2)))
          r a))
      {} args)))

(defcheck solution-4a705aee
  (fn [f & xms]
    (reduce
      (fn [xm ym]
        (reduce
          (fn [m [k y]]
            (assoc m k (if-let [x (get m k)] (f x y) y)))
          xm ym))
      {} xms)))

(defcheck solution-4a899f24
  (fn [f & ms]
    (let [me (fn [m e]
               (let [k (key e) v (val e)]
                 (if (m k)
                   (assoc m k (f (get m k) v))
                   (assoc m k v))))]
      (reduce #(reduce me %1 (seq %2)) ms))))

(defcheck solution-4a8d5721
  (fn [f & args] (reduce #(assoc %1 (key %2) (if (= 1 (count (val %2))) (second (first (val %2))) (apply f (map second (val %2))))) {} (group-by first (reduce into [] args)))))

(defcheck solution-4acf2cf2
  (fn mymerg([f a] a)
    ([f a b](reduce (fn [m [k v]](assoc m k (if (m k) (f (m k) v) v))) a b))
    ([f a b & c](reduce #(mymerg f %1 %2) (mymerg f a b) c))))

(defcheck solution-4ad92dfb
  (fn [f & colls]
    (reduce
      #(reduce
         (fn [m [k v]]
           (assoc m k (if-let [p (m k)]
                        (f p v)
                        v)))
         %1 %2)
      colls)))

(defcheck solution-4b33dcf6
  (fn [f m & ms]
    (reduce (fn [a [k v]]
              (assoc a k
                       (if (contains? a k)
                         (f (get a k) v)
                         v))) m (mapcat seq ms))))

(defcheck solution-4b4050e5
  (fn [f & args]
    (reduce
      (fn [m arg]
        (reduce (fn [m2 k]
                  (if (contains? m2 k)
                    (assoc m2 k (apply f (list (m2 k) (arg k))))
                    (assoc m2 k (arg k))
                    )
                  ) m (keys arg))
        ) {} args)
    ))

(defcheck solution-4bd94863
  (fn [f & ms]
    (let [mw-one (fn [f m1 m2]
                   (let [common-keys (set (clojure.set/intersection (set (keys m1)) (set (keys m2))))
                         m1-kvs (select-keys m1 (clojure.set/difference (set (keys m1)) common-keys))
                         m2-kvs (select-keys m2 (clojure.set/difference (set (keys m2)) common-keys))]
                     (-> {}
                       (into m1-kvs)
                       (into m2-kvs)
                       (into (map #(vector % (f (m1 %) (m2 %))) common-keys)))))]
      (reduce #(mw-one f %1 %2) ms))))

(defcheck solution-4c043dee
  (fn [f m & r]
    (if (empty? r)
      m
      (recur f
        (into m (for [[k v] (first r)]
                  (if (get m k)
                    [k (f (get m k) v)]
                    [k v])))
        (rest r)))))

(defcheck solution-4c252408
  (fn [f m & ms]
    (reduce
      (fn [r m]
        (reduce
          (fn [m [k v]]
            (if
             (contains? m k)
              (assoc m k (f (m k) v))
              (assoc m k v)))
          r m))
      m ms)))

(defcheck solution-4c69548d
  (fn [op & ms]
    (let [ks (set (mapcat keys ms))]
      (apply merge
        (for [k ks]
          (let [ans (filter (comp not nil?) (map #(get % k) ms))]
            (if (> (count ans) 1)
              {k (apply op ans)}
              {k (nth ans 0)}
              )
            )
          )
        )
      )
    ))

(defcheck solution-4d2074d5
  (fn [f m & ms]
    (reduce
      (fn [r v]
        (reduce
          #(assoc
            %
             (first %2)
             (if (contains? r (first %2)) (f (r (first %2)) (second %2)) (second %2)))
          r
          v))
      m ms)))

(defcheck solution-4d25a2dc
  (fn [f & maps]
    (reduce #(merge %2
               (apply conj {}
                 (for [[k v] %1]
                   (if-let [u (%2 k)]
                     [k (f v u)]
                     [k v]))))
      maps)))

(defcheck solution-4d68de07
  (fn __
    [func first-map & other-maps]
    (letfn [(join [m [k v]]
              (if (contains? m k)
                (assoc m k (func (m k) v))
                (assoc m k v)))
            (reduce-maps [m other-m]
              (reduce join m other-m))]

      (reduce
        reduce-maps
        first-map
        other-maps))))

(defcheck solution-4e228e24
  (fn [fnn ze & se]
    (reduce-kv
      (fn [m k v]
        (assoc m k (reduce (fn [acc x] (fnn acc x)) (map second v))))
      {}
      (group-by first (reduce concat ze se)))))

(defcheck solution-4e6e1904
  (fn [f & maps]
    (let [merge2
          (fn [a b]
            (loop [s (seq b) res a]
              (if (seq s)
                (let
                 [keyval (first s)
                  k  (key keyval)
                  v  (val keyval)
                  resval ( if (contains? a k)
                           (f (a k) v)
                           v
                           )]
                  (recur (next s) (assoc res k resval))
                  )

                res
                )
              )
            ) ]
      (reduce merge2 maps))))

(defcheck solution-4eb0d56a
  (fn myMergeWith
    ([f m1 m2]
     (loop[result m1, remaining m2]
       (if-let [[k v] (first remaining)]
         (if (contains? result k)
           (recur (assoc result k (f (result k) v)) (rest remaining))
           (recur (assoc result k v) (rest remaining)))
         result)))
    ([f m1 m2 & ms]
     (apply myMergeWith f (myMergeWith f m1 m2) (seq ms)))))

(defcheck solution-4f2092e
  (fn my-merge-with [f & colls]
    (let [all-keys (distinct (mapcat keys colls))
          wrapped-f (fn ([single] single)
                      ([one & more] (apply f (conj more one))))]
      (reduce (fn [ret key] (assoc ret key (apply wrapped-f (filter (complement nil?) (map (fn [coll] (get coll key)) colls))))) {} all-keys))))

(defcheck solution-4f7080c2
  (fn [op & args]
    (reduce
      (fn [r [k v]] (merge r {k (if (nil? (get r k)) v (op (get r k) v))}))
      {}
      ;(mapcat #(map (fn [[k v]] [k v]) %) args)))
      (apply concat args))))

(defcheck solution-4fd054a1
  (fn [o c & e]
    (reduce #(let [ks (keys %2)]
               (loop [r %1 m ks]
                 (if (not (empty? m))
                   (let [k (first m)
                         v (if (nil? (get r k nil))
                             (get %2 k)
                             (o (get r k 0) (get %2 k)))]
                     (recur (assoc r k v) (rest m)))
                   r))) c e)))

(defcheck solution-501de894
  (fn [f & maps]
    (reduce (fn [map1 map2]
              (reduce #(conj %1 [%2 (if (map1 %2) (f (map1 %2) (map2 %2)) (map2 %2))])
                map1
                (keys map2)))
      maps)))

(defcheck solution-5037fe27
  (fn [f m & ms]
    (reduce (fn [res [k v]] (if (contains? res k)
                              (assoc res k (f (res k) v))
                              (assoc res k v)))
      m (apply concat ms))))

(defcheck solution-5064240b
  #(reduce (partial reduce
             (fn [m [k v]]
               (assoc m k (if (m k) (% (m k) v) v))))
     %2
     %&))

(defcheck solution-50668afd
  (fn[f & v]
    (reduce (fn[m n]
              (into m
                (for [[x y] n]
                  (if (m x) [x (f (m x) y)]
                            [x y])))) v)))

(defcheck solution-50f5d4bd
  (fn[f x & a]( reduce #(if (contains? % (first %2)) (assoc % (first %2) (f (get % (first %2)) (second %2))) (assoc % (first %2) (second %2)))  x (apply concat (map #(seq %) a)))))

(defcheck solution-51195334
  (fn [f m & more]
    (letfn [(g [s t] (reduce (fn [x [k v]] (if (x k)
                                             (update-in x [k] (fn [n] (f n v)))
                                             (assoc x k v))) s t))]
      (reduce (fn [a b] (g a b)) m more))))

(defcheck solution-516a6256
  (fn [f & mps]
    (reduce
      (fn [m m2]
        (reduce
          (fn [a [k v]]
            (if (a k)
              (assoc a k (f (a k) v))
              (assoc a k v)))
          m m2))
      mps)))

(defcheck solution-517a38f3
  (fn merge--listcomp
    [f & maps] {:pre [(ifn? f), (every? map? maps)]}
    (let [;; This is the set of keys that appear in any of the maps.
          keyset (set (mapcat keys maps)),

          ;; This is a function wrapping f so that we don't have to separately
          ;; handle the case where a key only appears in one map.
          smoosh (fn [x & etc] (if (seq etc) (apply f (cons x etc)) x)),

          ;; This function takes in a key k and provides the value it should have
          ;; in the output map.
          update-key (fn [k]
                       (->> maps
                         (filter #(contains? % k))
                         (map #(% k))
                         (apply smoosh)))]

      ;; To get the output map, we just apply update-key to each key in the key
      ;; set, and stick all of the resulting pairs in a map.
      (into {} (for [k keyset] [k (update-key k)])))))

(defcheck solution-51a7e99
  (fn [f & ms]
    (let [ks (apply concat (map keys ms))]
      (apply assoc {} (for [k ks
                            :let [mks (for [m ms
                                            :let [mk (m k)]
                                            :when mk]
                                        mk)]
                            x [k (reduce f mks)]]
                        x)))))

(defcheck solution-51ba65a7
  #(into {}
     (map (fn [[k a]] [k (reduce % (map second a))])
       (group-by first (mapcat seq %&)))))

(defcheck solution-51bd8da4
  (fn my-merge-with
    [f result & other-maps]
    (letfn [(my-merge
              [original other-map]
              #_(println "Now merging: " original other-map)
              (reduce my-conj original other-map))
            (my-conj
              [some-map [k v]] ;splicing
              #_(println "Now conjing: " some-map [k v])
              (if-not (contains? some-map k)
                (conj some-map [k v])
                (conj some-map [k (f (some-map k) v)])))]
      (reduce my-merge result other-maps))))

(defcheck solution-52a2df2c
  (fn
    [f & m]
    (into {}
      (map
        (fn [[k v]]
          (vector k (reduce f (map last v))))
        (group-by first
          (reduce
            #(concat %1 (map identity %2))
            []
            m))))))

(defcheck solution-537e9ffb
  (fn [f & coll]
    (into {}
      (map (fn [c] (vector (first c)
                     (if (> (count (last c)) 1)
                       (apply f (map last (last c)))
                       (last (first (last c))) )))
        (group-by key (apply concat coll)) ))))

(defcheck solution-53a7ccfc
  (fn mer-by [f & maps]
    (letfn
     [(map-vals
        [f pred m]
        (into {}
          (for [[k v] m]
            (if (pred v)
              [k (f v)]
              [k (first v)]))))]
      (map-vals (partial apply f) #(> (count %) 1)
        (map-vals
          (partial map second)
          (constantly true)
          (group-by first (reduce into [] maps)))))))

(defcheck solution-54249b32
  #_(fn mrg-wth [f & maps]
      (reduce
        #(loop [acc %, items (vec %2)]
           (if (empty? items)
             acc
             (let [[k v] (first items)]
               (recur (if (contains? acc k)
                        (assoc acc k (f (acc k) v))
                        (assoc acc k v)
                        ) (rest items) ))))
        {} maps))

  (fn mrg-wth [f & maps]
    (reduce
      #(reduce
         (fn [acc [k v]]
           (assoc acc k (if (acc k) (f (acc k) v) v)))
         %1 %2)
      maps)))

(defcheck solution-5425ca9d
  (fn [f & maps]
    (reduce (fn [m0 m1]
              (reduce (fn [m [k v]]
                        (assoc m k (if (m k)
                                     (f (m k) v)
                                     v)))
                m0
                m1))
      maps)))

(defcheck solution-54a559da
  (fn [ op & maps ]
    (reduce
      (fn [res addon]
        (reduce (fn [m [ k v ]]
                  (let [mv (get m k)] (if (nil? mv) (assoc m k v) (assoc m k (op mv v)))))
          res addon))
      maps)))

(defcheck solution-54bec4b7
  (fn [f & ms]
    (reduce (fn [out in]
              (reduce (fn [out [k v]]
                        (assoc out k (if (contains? out k) (f (out k) v) v)))
                out in))
      {} ms)))

(defcheck solution-55259e3b
  (fn test [f x  & y ]
    (letfn [
            (mergewith [f map1 map2]
              (let [entries
                    (into []
                      (clojure.set/union (set (keys map1)) (set (keys map2)))
                      )]
                (loop [  i 0 result {}   ]
                  (cond
                    (>= i (count entries))   result
                    (nil? (get map2  (nth entries i))) (recur (inc i)  (conj result {(nth entries i)  (get map1  (nth entries i))})  )
                    (nil? (get map1  (nth entries i))) (recur (inc i)  (conj result {(nth entries i)  (get map2  (nth entries i))}) )
                    :else (recur  (inc i) (conj result {(nth entries i)  (f (get map1  (nth entries i))  (get map2 (nth entries i)))}) )
                    )

                  )
                )
              )

            ]
      (cond
        (empty? y) x
        (= (count y) 1) (mergewith f x (first y))
        :else (apply test f (mergewith f x (first y))  (rest y))
        )
      )
    ))

(defcheck solution-552861ff
  #(into {} (for [[k v] (group-by key (apply concat %&))]
              {k (reduce % (vals v))})))

(defcheck solution-5595848b
  (fn  [f & args]
    (let [argkeys (keys (apply merge args))]
      (into {} (for [k argkeys]
                 [k (reduce f (filter identity (map #(get % k) args)))])))))

(defcheck solution-559ebfe9
  (fn [-fn & more]
    (->> (reduce into [] more )
      (group-by key)
      (map #(vector (first %) (map val (last %))))
      (reduce #(assoc % (first %2) (if (= 1 (count (last %2)))
                                     (first (last %2))
                                     (apply -fn (last %2)))) {}))
    ))

(defcheck solution-55eee54d
  (fn [func firstMap & maps]
    (reduce
      (fn [outmap mymap]
        (reduce
          (fn [out mapkey]
            (let [outval (out mapkey), mapval (mymap mapkey)]
              (if
               (and (nil? outval) (nil? mapval))
                out
                (conj
                  out
                  (vector mapkey
                    (if
                     (nil? outval)
                      mapval
                      (if
                       (nil? mapval)
                        outval
                        (func outval mapval)
                        )
                      )
                    )
                  )
                )
              )
            )
          outmap (keys mymap)
          )
        )
      firstMap maps
      )
    ))

(defcheck solution-564c6951
  (fn _merge-with [f & colls]
    (if (= (count colls) 1) (first colls)
                            (let [m1 (first colls)
                                  m2 (second colls)
                                  newentries (apply concat (for [k (distinct (concat (keys m1) (keys m2)))]
                                                             (let [v (if (and (contains? m1 k) (contains? m2 k)) (f (m1 k) (m2 k))
                                                                                                                 (if (contains? m1 k) (m1 k) (m2 k)))]
                                                               [k, v])))
                                  newmap (apply assoc {} newentries)]
                              (recur f (cons newmap (drop 2 colls)))))))

(defcheck solution-5693bc55
  (fn merge-with' [f map0 & maps]
    (if (empty? maps) map0
                      (recur
                        f
                        (loop [map0' map0, map1' (first maps)]
                          (if (empty? map1')
                            map0'
                            (let [[k v] (first map1')]
                              (recur
                                (assoc map0' k
                                             (if (find map0 k) (f (map0 k) v) v))
                                (rest map1')))))
                        (rest maps)))))

(defcheck solution-56ea13f5
  (fn [f & ms]
    (reduce
      (fn walk [res [[k v] & xs]]
        (if k
          (if (res k)
            (recur (assoc res k (f (res k) v)) xs)
            (recur (assoc res k v) xs))
          res))
      (first ms)
      (map vec (rest ms)))))

(defcheck solution-5719cd66
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-577e8e7
  (fn my-merge-with [f & maps]
    (reduce
      (fn [merged amap]
        (reduce
          (fn [merged [k v]]
            (if (contains? merged k)
              (assoc merged k (f (merged k) v))
              (assoc merged k v)))
          merged amap))
      (first maps)
      (rest maps))))

(defcheck solution-57bbbb08
  (fn my-mw [f & maps]
    (letfn [(mw [f, a, b]
              (merge
                b
                (reduce
                  (fn [acc, x]
                    (assoc acc (nth x 0)
                               (if (contains? b (nth x 0))
                                 (f (nth x 1) (get b (nth x 0)))
                                 (nth x 1))))
                  {}
                  a)))]
      (reduce (partial mw f) maps))))

(defcheck solution-58472ff5
  (fn [f & args]
    (reduce (fn[map1 map2]
              (reduce (fn [m [k v]]
                        (if-let [vv (m k)]
                          (assoc m k (f vv v))
                          (assoc m k v)))
                map1 map2))
      args)))

(defcheck solution-59c3773d
  (fn m-with [f & args]
    (let [ks (reduce #(into %1 (keys %2)) #{} args)
          apply-f-vals
             (fn [f k xs]
               (let [vs (filter #(not (nil? %)) (map #(% k) xs))]
                 (if (= 1 (count vs)) (first vs) (apply f vs))))]
      (reduce merge
        (for [k ks]
          {k  (apply-f-vals f k args)}
          )))))

(defcheck solution-59dfd581
  ; &#19981;&#24688;&#22909;&#12384;&#12394;&#12354;
  (fn my-merge-with [f & maps]
    (->> maps
      (mapcat #(apply vector %))
      (group-by #(identity (% 0)))
      (reduce-kv
        (fn [ret k v]
          (let [g (if (= (count v) 1) identity f)]
            (assoc ret k
                       (apply g (map #(% 1) v))))) {}))))

(defcheck solution-59f893af
  (fn [f my-map & maps]
    (let [mp (fn [m1 m2]
               (reduce (fn [m1 key]
                         (if (get m1 key)
                           (assoc m1 key (f (get m1 key) (get m2 key)) )
                           (assoc m1 key (get m2 key))
                           )
                         ) m1 (keys m2)
                 ))]
      (reduce mp my-map maps ))
    ))

(defcheck solution-5a2552ea
  (fn [f & m]
    (let [m-w (fn m-w [f m1 m2]
                (if (empty? m2) m1
                                (m-w f
                                  (if (contains? m1 (first (first m2)))
                                    (assoc m1 (first (first m2))
                                              (f (get m1 (first (first m2))) (second (first m2))))
                                    (conj m1 (first m2)))
                                  (rest m2))) )]
      (reduce (partial m-w f) m))))

(defcheck solution-5a2ebcbf
  (fn mw
    ([f s1 s2]
     (if (empty? s2)
       s1
       (let [k (first (keys s2))]
         (if (contains? s1 k)
           (mw f (assoc s1 k (f (s1 k) (s2 k))) (dissoc s2 k))
           (mw f (assoc s1 k (s2 k)) (dissoc s2 k))
           )
         )
       )
     )
    ([f s1 s2 & s]
     (let [ss (cons s1 (cons s2 s))]
       (reduce (partial mw f) ss)
       )
     )
    ))

(defcheck solution-5a573b82
  (fn [f & s]
    (reduce
      (fn [m k]
        (let [[a & b :as v] (mapcat #(if (% k) [(% k)] []) s)]
          (assoc m k (if b (apply f v) a))))
      {}
      (into #{} (mapcat keys s)))))

(defcheck solution-5ab1b768
  (fn [f & maps]
    (reduce (fn [r k]
              (assoc r k (reduce
                           f (filter (complement nil?) (map #(% k) maps)))))
      {} (set (reduce concat (map keys maps))))))

(defcheck solution-5b03cf53
  (fn mwith [f o & m]
    (loop [origin o more m]
      (cond (empty? more)
            origin
            :else (recur (merge (first more) (zipmap (keys origin)
                                               (for [k (keys origin)]
                                                 (cond (contains? (first more) k)
                                                       (f (get origin k) (get (first more) k))
                                                       :else
                                                       (get origin k))
                                                 )
                                               ))
                    (rest more)
                    )
            )
      )
    ))

(defcheck solution-5b058848
  (fn [f & m]
    (reduce
      (fn [a b]
        (reduce
          merge
          (map
            #(let [{i %} a {j %} b]
               {% (if (and i j) (f i j) (or i j))})
            (mapcat keys [a b]))))
      m)))

(defcheck solution-5b2d04cb
  (fn f
    ([g m n]
     (apply conj
       m
       (map (fn [[k v]] (if (m k)
                          [k (g (m k) v)]
                          [k v]))
         n)))
    ([g m n & r]
     (apply f g (f g m n) r))))

(defcheck solution-5cd15159
  (fn my-merge-with [f & args]
    (let [ks (keys (apply merge args))
          merged-array (keep identity (for [k ks m args] (when (m k) [k (m k)])))]
      (reduce (fn [acc [k v]]
                (if (contains? acc k)
                  (assoc acc k (f (acc k) v))
                  (assoc acc k v)))
        {}
        merged-array))))

(defcheck solution-5cd4fc4b
  (fn [o m & l]
    (reduce (fn [r [k v :as p]]
              (let [s (get r k)]
                (if (nil? s)
                  (conj r p)
                  (assoc r k (o s v)))))
      m
      (reduce into [] l))))

(defcheck solution-5ce8fc67
  (fn merge-fn [f start & args]

    (if (= (count args) 0)
      start
      (let [item (first args)]
        (apply merge-fn (concat [f (reduce (fn [memo k]
                                             (let [v (start k)]
                                               (let [ans (if v
                                                           (assoc memo k (f v (item k)))
                                                           (assoc memo k (item k))
                                                           )]
                                                 ans
                                                 )
                                               )
                                             ) start (keys item))]
                                (rest args)))

        )

      )
    ))

(defcheck solution-5d4c6475
  (fn [o i j & x]
    (if (seq j)
      (recur o
        (reduce #(let [[k v] %2] (assoc %1 k (if-let [w (%1 k)] (o w v) v))) i j)
        (first x)
        (rest x))
      i)))

(defcheck solution-5d805797
  (fn [f & maps]
    (letfn [(step [m1 m2]
              (if (seq m2)
                (let [first-ele-in-m2 (first m2)
                      v-in-m1 (m1 (key first-ele-in-m2))]
                  (if v-in-m1
                    (step
                      (assoc m1 (key first-ele-in-m2) (f v-in-m1 (val first-ele-in-m2)))
                      (rest m2))
                    (step
                      (assoc m1 (key first-ele-in-m2) (val first-ele-in-m2))
                      (rest m2))))
                m1))]
      (reduce step maps))))

(defcheck solution-5dbaaf09
  (fn [f & ms]
    (let [ks (distinct (flatten (map keys ms)))]
      (reduce (fn [h k]
                (let [vs (keep #(get % k) ms)]
                  (if (< 1 (count vs))
                    (assoc h k (apply f vs))
                    (assoc h k (first vs)))))
        {}
        ks))))

(defcheck solution-5ddb0b2d
  (fn [f m & maps]
    (letfn [(rf [m1 m2]
              (let [rm (for [[k v] m2]
                         (hash-map k (if (m1 k) (f (m1 k) v) v)))]
                (into m1 rm)))]
      (reduce rf m maps))))

(defcheck solution-5e1cbc4f
  (fn [f m & args]
    (reduce #(reduce (fn [xmap pair]
                       (assoc xmap (key pair) (if (contains? xmap (key pair))
                                                (f (xmap (key pair)) (val pair))
                                                (val pair))))
               % %2) m args)))

(defcheck solution-5e4d07f0
  (fn [f & maps]
    (reduce
      (fn [m v]
        (assoc m v
                 (#(if (> (count %) 1)
                     (apply f %)
                     (first %))
                  (remove nil?
                    (map #(% v) maps)))))
      {}
      (distinct (flatten (map keys maps))))))

(defcheck solution-5ebf40c2
  (fn [f & mps]
    (reduce
      (fn [finalMap newMap]
        (reduce
          (fn [finalMap [newKey newVal]]
            (assoc finalMap newKey
                            (if (contains? finalMap newKey)
                              (f (get finalMap newKey) newVal)
                              newVal)))
          finalMap newMap))
      mps)))

(defcheck solution-5ed1aa47
  (fn custom-merge-with
    [f & maps]
    (let [tuples (apply concat (map vec maps))]
      (reduce (fn [result-set tuple]
                (let [current-key (first tuple)
                      current-value (result-set current-key)
                      next-value (last tuple)]
                  (if current-value
                    (assoc result-set current-key (f current-value next-value))
                    (assoc result-set current-key next-value))))
        {} tuples))))

(defcheck solution-5f284ba4
  (fn [f & xs]
    (reduce
      (fn [m [k v]] (assoc m k (if (contains? m k) (f (m k) v) v)))
      (sorted-map)
      (apply concat (map #(seq %) xs)))))

(defcheck solution-5f9f1fb0
  (fn [op main & others]
    (let [merge2
          (fn [a b]
            (reduce
              (fn [merged [k v]]
                (assoc merged k (if (contains? merged k)
                                  (op (get merged k) v)
                                  v)))
              a b))]
      (reduce merge2 main others))))

(defcheck solution-5fa1e69
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v2]]
            (let [v1 (m1 k), v (if v1 (f v1 v2) v2)]
              (assoc m k v))) m1 m2))
      maps)))

(defcheck solution-600647a1
  (fn [f m & n]
    (letfn [(g  [m [k v]] (assoc m k (if (contains? m k) (f (m k) v) v) ))]
      (reduce #(reduce g % %2) m n)

      )))

(defcheck solution-600e20c
  (fn [f & ms] (reduce (fn [m [k v]] (if (contains? m k) (update-in m [k] f v) (assoc m k v))) {} (apply concat ms))))

(defcheck solution-6023795
  (fn [f & ms]
    (reduce
      #(merge %
         (into {}
           (for [[k v] %2]
             [k (if (contains? % k) (f (% k) v) v)])))
      {} ms)))

(defcheck solution-6028a0e9
  (fn [f & args]
    (reduce (fn [out in]
              (reduce (fn [out [k v]]
                        (if (contains? out k)
                          (assoc out k (f (get out k) v))
                          (assoc out k v))) out in)) {} args)))

(defcheck solution-604277f2
  (fn [f & maps]
    (reduce #(conj %
               (reduce (fn [r [k v]]
                         (let [rv (get r k)] (assoc r k (if rv (f rv v) v)))) % %2))
      maps)))

(defcheck solution-606b227a
  (fn [f & maps]
    (reduce (fn [m [k v]] (assoc m k (if (m k) (f (m k) v) v))) {} (mapcat seq maps))))

(defcheck solution-607fc893
  (fn mw [op & maps]
    (let [kvs (group-by first (apply concat (map vec maps)))
          ks (keys kvs)
          vs (map #(map second %) (vals kvs))
          rs (map #(reduce op %) vs)]
      (zipmap ks rs))))

(defcheck solution-60a0406b
  (fn [f & ms]
    (reduce
      (fn [m0 m1]
        (reduce
          (fn [m [k v]]
            (let [o (get m k)]
              (if o
                (assoc m k (f o v))
                (assoc m k v)
                )
              )
            )
          m0
          m1
          )
        )
      ms
      )
    ))

(defcheck solution-60bb4977
  (fn [f & maps]
    (reduce (fn [out in]
              (reduce-kv (fn [m k v]
                           (assoc m k (if-let [v' (get m k)]
                                        (f v' v)
                                        v)))
                out in))
      maps)))

(defcheck solution-60c0cf74
  (fn m [f & maps]
    (let [all-keys (set (mapcat keys maps))]
      (reduce (fn [amap k]
                (assoc amap k (reduce f (filter identity (map #(get % k) maps))))) {}  all-keys))))

(defcheck solution-610cd349
  (fn my-merge-with [f & [m & ms]]
    (letfn [(merge-one [acc [k v]]
              (assoc acc k
                         (if (contains? acc k)
                           (f (get acc k) v)
                           v)))]
      (reduce #(reduce merge-one %1 %2) m ms))))

(defcheck solution-611b9c8f
  (fn [f & maps]
    (into {}
      (map
        (fn [[k vals]]
          [k (reduce f (map second vals))])
        (group-by first (apply concat maps))))))

(defcheck solution-61b153c0
  (fn [f m & ms]
    (reduce
      (fn [m [k v]]
        (if (contains? m k)
          (assoc m k (f (get m k) v))
          (assoc m k v)))
      m (mapcat seq ms))))

(defcheck solution-61be9136
  (fn [f & m]
    (letfn [(mw [x y]
              (reduce (fn [a [k v]]
                        (let [actual-value (a k)
                              actual-value (if actual-value
                                             (f actual-value v)
                                             v)]
                          (assoc a k actual-value))) x y))]
      (reduce mw m))))

(defcheck solution-61c4ab76
  (fn [f & colls]
    (reduce
      (fn [m n]
        (let [m-key-set (set (keys m))
              n-key-set (set (keys n))
              shared-k (seq (clojure.set/intersection m-key-set n-key-set))
              diff-k (seq (clojure.set/difference n-key-set m-key-set))]
          (merge m
            (apply hash-map
              (mapcat vector
                shared-k
                (map f
                  (map m shared-k)
                  (map n shared-k))))
            (apply hash-map
              (mapcat vector
                diff-k
                (map n diff-k))))))
      colls)))

(defcheck solution-62926276
  (fn [f & maps]
    (letfn [(upt [m [k v]]
              (assoc m k (if (m k) (f (m k) v) v)))]
      (reduce upt {} (mapcat seq maps)))))

(defcheck solution-62be494a
  (fn [f & maps]
    (reduce
      conj
      {}
      (map
        (fn [[k vals]] [k (reduce f (map second vals))])
        (group-by first (apply concat maps))))))

(defcheck solution-62d3eff9
  (fn [f m & r]
    (reduce
      (partial reduce-kv
        #(assoc % %2 (if-let [x (% %2)]
                       (f x %3)
                       %3))) m r)))

(defcheck solution-63421e9
  (fn [f m & ms]
    (reduce #(if (find %1 (key %2))
               (update-in %1 [(key %2)] f (val %2))
               (assoc-in %1 [(key %2)] (val %2)))
      m
      (apply merge ms))))

(defcheck solution-634ff78a
  (fn [f & ms]
    (reduce (fn [res n]
              (reduce (fn [r [k v]]
                        (if (contains? r k)
                          (update-in r [k] f v)
                          (assoc r k v)))
                res n))
      {} ms)))

(defcheck solution-637a1f3d
  (fn merge-function2 [f & mapss]
    (loop [resp {}
           maps mapss]
      (if (= maps [])
        resp
        (let [m (first maps)
              ks (keys m)]
          (let [resp (reduce (fn [r k]
                               (let [v (m k)]
                                 (if (contains? r k)
                                   (assoc r k (f (r k) v))
                                   (assoc r k v)))) resp ks)]
            (recur resp (rest maps))))))))

(defcheck solution-63b45906
  (fn [g m & ms]
    (reduce
      #(let [[f s] %2]
         (assoc % f (if (% f) (g (% f) s) s)))
      m (apply concat ms))))

(defcheck solution-63d45ed0
  (fn [f & maps]
    (reduce
      (fn [sofar m]
        (reduce
          (fn [m' [k v]]
            (assoc m' k
                      (if (contains? m' k)
                        (f (m' k) v)
                        v)))
          sofar
          m))
      {}
      maps)))

(defcheck solution-64a97a97
  (fn [f & maps]
    (letfn [(I [[k v] s]
              (if (contains? s k)
                (assoc s k (f v (s k)))
                (assoc s k v)))
            (M [s t]
              (if (empty? s) t
                             (M (rest s) (I (first s) t))))
            (M* [args]
              (if (empty? args) {}
                                (M (first args) (M* (rest args)))))]
      (M* maps))))

(defcheck solution-6536250
  (fn [f & ms]
    (reduce (fn [rm m]
              (into (into rm
                      (filter (fn [[k v]] (not (rm k))) (seq m)))
                (map (fn [[k v]] [k (f (rm k) v)]) (filter (fn [[k v]] (rm k)) (seq m)))))
      ms)))

(defcheck solution-6571bf62
  (fn [f & ms]
    (into {}
      (map #(reduce (fn
                      ([kv] kv)
                      ([[k v1] [_ v2]] [k (f v1 v2)])) %)
        (map val (group-by key (apply concat ms)))))))

(defcheck solution-6579b843
  (fn [f mp & params]
    (let [parmap (apply merge params)]
      (reduce #(if (contains? mp %2)
                 (assoc %1 %2 (f (get mp %2 ) (get parmap %2)))
                 (assoc %1 %2 (get parmap %2))
                 )
        mp
        (keys parmap))
      )
    ))

(defcheck solution-65e669c5
  (fn [f m & ms]
    (reduce
      (fn [ml mr]
        (loop [allkeys (set (concat (keys ml) (keys mr)))
               builder {}]
          (if (empty? allkeys)
            builder
            (let [firstkey (first allkeys)]
              (if (and (contains? ml firstkey)
                       (contains? mr firstkey))
                (recur (rest allkeys) (assoc builder firstkey (f (get ml firstkey)
                                                                (get mr firstkey))))
                (recur (rest allkeys) (assoc builder firstkey (val (or (find ml firstkey)
                                                                       (find mr firstkey))))))))))
      m ms)))

(defcheck solution-65ffce09
  (fn mw
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge (fn [m1 m2]
                    (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge maps)))))

(defcheck solution-66297a1a
  (fn merge-with1
    ([f hmap add] (reduce (fn [orig [k v]] (if (orig k) (assoc orig k (f (orig k) v)) (assoc orig k v))) hmap add))
    ([f hmap add & additions] (apply merge-with1 f (merge-with1 f hmap add) additions))))

(defcheck solution-66706fb7
  (fn [f & ms]
    (reduce (fn [acc m]
              (reduce (fn [acc2 [k v]] (update-in acc2 [k] #(if % (f % v) v))) acc m))
      (first ms)
      (rest ms))))

(defcheck solution-668f82f
  (fn [f & ds]
    (loop [acc (first ds) ds (rest ds)]
      (if (seq ds)
        (recur
          (into acc (for [[k v] (first ds)] (if (contains? acc k) [k (f (acc k) v)] [k v])))
          (rest ds))
        acc))))

(defcheck solution-66dde44e
  (fn merg
    ([f h1 h2] (reduce  (fn [acc,k] (assoc acc k (if (h1 k) (f (h1 k) (h2 k)) (h2 k)))) h1 (keys h2)))
    ([f h1 h2 & maps] (merg f h1 (apply merg (cons f (cons h2 maps)))))))

(defcheck solution-67516855
  (fn [f & cs]
    (reduce
      #(reduce
         (fn [c [k v]] (assoc c k (if-let [v2 (c k)] (f v2 v) v)))
         % %2)
      cs)))

(defcheck solution-6761a8fe
  (fn [f & maps]
    (reduce (fn [x [k v]] (assoc x k (if (contains? x k) (f (get x k) v) v)))
      {}
      (apply concat (map vec maps)))))

(defcheck solution-6763c66d
  (fn [f & m]
    (reduce
      #(into % (for [[k v] %2]
                 [k (if-let [w (% k)] (f w v) v)]))
      m)))

(defcheck solution-677966a
  (fn [f & seqs]
    (reduce (fn [acc seq] (reduce (fn [acc [k v]] (if (contains? acc k)
                                                    (assoc acc k (f (acc k) v))
                                                    (assoc acc k v))) acc seq))
      (first seqs)
      (rest seqs))))

(defcheck solution-678ce81d
  (fn [f & s]
    (reduce (fn [agg k]
              (let [vs (remove nil? (map #(get % k) s))]
                (into agg {k (if (> (count vs) 1)
                               (apply f vs)
                               (first vs))})))
      {}
      (set (mapcat keys s)))))

(defcheck solution-679f280e
  (fn my-merge-with [f m & ms]
    (letfn [(merge-two [m1 m2]
              (if (empty? m2)
                m1
                (let [[k v] (first m2)]
                  (if (contains? m1 k)
                    (recur (conj m1 [k (f (get m1 k) v)]) (rest m2))
                    (recur (conj m1 [k v]) (rest m2))))))]
      (reduce merge-two m ms))))

(defcheck solution-67ab975
  (fn [f acc & xs]
    (if (empty? xs)
      acc
      (letfn [(agg [m k v]
                (if (contains? m k)
                  (assoc m k (f (m k) v))
                  (assoc m k v)))]
        (recur f
          (reduce-kv agg acc (first xs))
          (rest xs))))))

(defcheck solution-67af7ee9
  (fn [f & maps]
    (let [merge (fn [m f k v]
                  (if (contains? m k)
                    (assoc m k (f (get m k) v))
                    (assoc m k v)))
          merge-maps (fn [f m1 m2]
                       (reduce (fn [m [k v]] (merge m f k v)) m1 m2))]
      (reduce #(merge-maps f % %2) {} maps))))

(defcheck solution-67c7df0f
  (fn this
    ([f x] x)
    ([f x1 x2 & xs]
     (if (empty? xs)
       (letfn [(worker [l s]
                 (if (empty? l) s
                                (recur (rest l)
                                  (let [v (first l) p (find s (key v))]
                                    (if p
                                      (assoc s (key v) (f (val p) (val v)))
                                      (assoc s (key v) (val v))))))
                 )]
         (worker x2 x1))
       (apply this f (this f x1 x2) (first xs) (rest xs))))
    ))

(defcheck solution-68136e0d
  (fn [pred & maps]
    (reduce #(reduce
               (fn [r [k v]]
                 (assoc r k (if (r k) (pred (r k) v) v))
                 )
               %1 %2) {} maps)
    ))

(defcheck solution-681fdada
  (fn [f & [fst & maps]]
    (reduce
      #(loop [m %, [[k v] & r] (seq %2)]
         (cond
           (nil? k) m
           (contains? m k) (recur (assoc m k (f (m k) v)) r)
           :else (recur (assoc m k v) r)))
      fst maps)))

(defcheck solution-682ad302
  (fn
    [f & maps]
    (let [combined (apply concat maps)
          grouped (group-by first combined)]
      (zipmap (keys grouped)
        (map #(if (> (count %) 1) (apply f %) (first %))
          (map #(map second %)
            (vals grouped)))))))

(defcheck solution-685927cf
  (fn [f origin & more]
    (loop [origin origin
           coll more]
      (if (empty? coll)
        origin
        (recur (loop [the-map origin
                      additions (first coll)]
                 (if (empty? additions)
                   the-map
                   (let [the-key (first (keys additions))
                         old-value (the-map the-key)
                         new-value (additions the-key)]
                     (recur (->> (if old-value
                                   (f old-value new-value)
                                   new-value)
                              (assoc the-map the-key))
                       (dissoc additions the-key)))))
          (rest coll))))))

(defcheck solution-689a39a
  (fn [f & m]
    (reduce (fn [r n]
              (reduce (fn [c [k v]]
                        (assoc c k (if (nil? (c k))
                                     v
                                     (f (c k) v))))
                r
                n))
      {}
      m)))

(defcheck solution-68f42ae6
  (fn my-merge-with
    [combiner m & ms]
    (if (empty? ms)
      m
      (let [m2 (reduce
                 (fn [m [k,v]]
                   (assoc m k
                            (if-let [[k0, v0] (find m k)] (combiner v0 v) v)))
                 m (first ms))]
        (apply my-merge-with combiner m2 (rest ms))))))

(defcheck solution-695999de
  (fn merge-with-e [f & x]
    (let [K (reduce into #{} (map keys x))
          not-nil (fn [v] (not (nil? v)))]
      (apply conj {}
        (map
          #(vector % (let [args (filter not-nil (map (fn [m] (m %)) x))]
                       (if (= (count args) 1) (first args) (apply f args))))
          K)))))

(defcheck solution-695d0ffb
  (fn [f & s](reduce (fn [a b] (merge a (->> b
                                          (map #(vector (key %)
                                                  (if-let [va (get a (key %))]
                                                    (f va (val %))
                                                    (val %))))
                                          (apply concat)
                                          (apply hash-map)
                                          )  )) s)))

(defcheck solution-6967d21a
  (fn f [o & m]
    (if (= 1 (count m))
      (first m)
      (let [[a b & r] m]
        (apply f o (merge a (into {} (map (fn [[k v]]
                                            (if (a k)
                                              [k (o (a k) v)]
                                              [k v]))
                                       b))) r)))))

(defcheck solution-69798400
  (fn [f & s] (apply hash-map (interleave (keys (apply conj (seq s))) (map #(reduce f %) (map #(filter (comp not nil?) %) (map #(map (fn [m] (get m %)) (seq s)) (keys (apply conj (seq s))))))))))

(defcheck solution-69a843ac
  (fn merge-with' [function merge-into & merge-from]
    (loop [target merge-into, queue merge-from]
      (if (empty? queue)
        target
        (recur
          (reduce
            (fn [m [k v]]
              (if (m k)
                (assoc m k (function (m k) v))
                (assoc m k v)))
            target
            (first queue))
          (rest queue))))))

(defcheck solution-69d06076
  (fn  [f & mps]
    (reduce (fn [mp1 mp2]
              (reduce (fn [mp [k v2]]
                        (assoc mp k (if (contains? mp k) (f (mp k) v2) v2)))
                mp1
                mp2))
      mps)))

(defcheck solution-69f3781b
  (fn [f v0 & vs]
    (reduce
      (fn [so-far m]
        (reduce
          (fn [now [k v]]
            (assoc now k (if (now k) (f (now k) v) v)))
          so-far m))
      v0
      vs)))

(defcheck solution-6a0476d9
  (fn [f & s]
    (reduce
      #(reduce
         (fn [a [k v]]
           (assoc a k (if (a k) (f (a k) v) v)))
         %
         %2)
      s)))

(defcheck solution-6a3c3559
  (fn
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-6a8fe838
  #(reduce (fn [r [k v]] (if (contains? r k) (assoc r k (%1 (r k) v)) (assoc r k v))) {} (mapcat vec %&)))

(defcheck solution-6ab460cb
  (fn [f & maps ]
    (let [ kk (set (mapcat keys maps))
          get*  (fn [k] (for [m maps :when (m k)] (m k)))
          merge (fn [vv] (if (= 1 (count vv)) (first vv) (apply f vv)))]
      (apply hash-map (mapcat #(list % (merge (get* %))) kk)))))

(defcheck solution-6b3048f3
  (fn [f & ms]
    (into
      {}
      (map
        (fn [[k vs]] [k (reduce f (map val vs))])
        (group-by
          key
          (apply concat ms))))))

(defcheck solution-6c027ca6
  (fn [f & maps]
    (->> maps
      (mapcat seq)
      (group-by key)
      vals
      (map (fn [grp] [(ffirst grp) (map val grp)]))
      (map (fn [grp] [(first grp) (reduce f (last grp))]))
      (into {}))))

(defcheck solution-6cd71c18
  (fn mw
    ([f m] m)
    ([f m & ms]
     (let [[m2 & more] ms]
       (apply mw f
         (reduce
           (fn [m [k v]]
             (assoc m k
                      (if (m k) (f (m k) v) v)
                      )
             )
           m m2)
         more)))))

(defcheck solution-6d2c0ae2
  (fn [op & hs]
    (let [ls (->> hs
               (mapcat seq)
               (group-by first))
          ks (keys ls)
          vs (map (fn [coll]
                    (map second coll)) (vals ls))
          vs2 (map (fn [coll] (reduce op coll)) vs)
          ret (zipmap ks vs2)]
      ret)))

(defcheck solution-6d43d428
  (fn [f m1 & ms]
    (letfn [(my-merge [f m n]
              (reduce
                #(assoc % %2
                          (let [a (% %2) b (n %2)]
                            (if a (f a b) b))) m (keys n)))]
      (reduce #(my-merge f % %2) m1 ms))))

(defcheck solution-6d4cf19a
  (fn mymerge [f & maps]
    (reduce (fn [map1 map2]
              (reduce (fn [x y]
                        (let [[k v] y]
                          (if (nil? (x k))
                            (assoc x k v)
                            (assoc x k (f (x k) v)))))
                map1 map2))
      maps)))

(defcheck solution-6da398a4
  (fn f [combiner initial & others]
    (if (empty? others)
      initial
      (let [[addee & remainder] others
            keys-in-both (filter #(contains? addee %) (keys initial))]
        (recur
          combiner
          (merge
            (apply dissoc initial (keys addee))
            (apply dissoc addee (keys initial))
            (apply hash-map (mapcat #(list % (combiner (initial %) (addee %))) keys-in-both)))
          remainder)))))

(defcheck solution-6de5619c
  (fn [f & maps]
    (reduce (fn [merged-map new-map]
              (reduce (fn [merged-map [key val]]
                        (if (contains? merged-map key)
                          (assoc merged-map key (f (get merged-map key) val))
                          (conj merged-map [key val])))
                merged-map
                new-map))
      maps)))

(defcheck solution-6deeaf6
  (fn [f & args] (let [l (seq (apply clojure.set/union (map (comp set keys) args)))]
                   (zipmap l (map #(reduce f (for [i args :when (contains? i %)] (i %))) l)))))

(defcheck solution-6e3fc6ca
  (fn [f m & r]
    (reduce
      (fn [a [k v]]
        (assoc a k (if (a k) (f (a k) v) v)))
      m
      (apply concat r))))

(defcheck solution-6e60d9d1
  (fn merge-maps
    [f & maps]
    (let [merge-one (fn [left right]
                      (loop [[[k v] & t] (into [] right)
                             accum left]
                        (let [new-accum (if (contains? accum k)
                                          (assoc accum k (f (accum k) v))
                                          (assoc accum k v))]
                          (if (nil? t)
                            new-accum
                            (recur t new-accum)))))]
      (loop [[h & t] maps
             accum {}]
        (let [new-accum (merge-one accum h)]
          (if (nil? t)
            new-accum
            (recur t new-accum)))))))

(defcheck solution-6e77fb04
  (fn my-merge-with [f & args]
    (apply hash-map (apply concat (map
                                    #(vector
                                       (first %)
                                       (reduce f (map (fn [x] (second x)) (second %))))
                                    (group-by #(first %) (mapcat #(seq %) args)))))))

(defcheck solution-6e96527e
  #(reduce (fn [r n]
             (reduce (fn [r i]
                       (if-let [e (get r (key i))]
                         (assoc r (key i) (% e (val i)))
                         (conj r i)))
               r
               n))
     %&))

(defcheck solution-6eb53030
  (fn [f m & r]
    (reduce (fn [i [k v]]
              (assoc i k (if (nil? (i k)) v
                                          (f (i k) v))))
      m (into {} r))))

(defcheck solution-6ed3182c
  (fn [f m1 & args]
    (let [m2 (reduce into args)]
      (apply conj m1
        (map
          (fn [kv]
            (let [[k v] kv]
              (if (contains? m1 k)
                [k (f (m1 k) v)]
                [k v]
                )
              )
            )
          m2
          )
        )
      )
    ))

(defcheck solution-6ee31c56
  (fn cust-merge-with [f m & args]
    (reduce
      (fn merge-map [m t]
        (let [pairs (seq t)]
          (reduce
            (fn m-single [m t]
              (let [v-in-m (get m (first t))]
                (if (= nil v-in-m)
                  (conj m t)
                  (conj m {(first t) (f v-in-m (second t))}))))
            m
            t)))
      m
      args)))

(defcheck solution-6f06ca63
  (fn w [f m & n]
    (if (empty? n)
      m
      (recur
        f
        (reduce #(let [a (first %2)]
                   (assoc %1 a
                             (if (get %1 a)
                               (f (get %1 a) (second %2))
                               (second %2)))) m (first n))
        (rest n)))))

(defcheck solution-6fc72999
  (fn [f & maps]
    (let [all-keys (into #{} (mapcat keys maps))
          args (fn [k] (map #(% k) (filter #(% k) maps)))
          mrg (fn [k] (let [a (args k)] (if (= 1 (count a)) (first a) (apply f a))))]
      (apply hash-map (mapcat #(vector % (mrg %)) all-keys)))))

(defcheck solution-7026f5e4
  (fn mwaf
    [f map-a & map-rest]
    (reduce (fn [a [ek, ev]]
              (conj a [ek (if (get a ek) (f (get a ek) ev) ev)]))
      map-a
      (for [x map-rest, y x] y))))

(defcheck solution-706314df
  (fn [op & ms]
    (reduce
      (fn [r m]
        (reduce (fn [result [k v]]
                  (if (contains? result k)
                    (assoc result k (op (result k) v))
                    (assoc result k v)))
          r
          m))
      {}
      ms)))

(defcheck solution-70be5a1b
  (fn [f & s]
    (reduce
      (fn[r m] (reduce
                 #(let[k (key %2) v (val %2) z (% k)]
                    (assoc % k (if z (f z v) v)))
                 r m))
      (first s) (next s))))

(defcheck solution-70e18c61
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce (fn [m [k v]]
                  (if-let [v1 (get m k)]
                    (assoc m k (f v1 v))
                    (assoc m k v)))
          m1 m2))
      maps)))

(defcheck solution-70ef9045
  (fn [f & m]
    (into {}
      (for [[k V] (group-by first (mapcat seq m))]
        [k (reduce f (map second V))]))))

(defcheck solution-7127ca78
  (fn [f & ms]
    (when (some identity ms)
      (let [merge-into (fn [m1 m2]
                         (let [k (key m2)
                               v (val m2)]
                           (if (contains? m1 k)
                             (assoc m1 k (f (get m1 k) v))
                             (assoc m1 k v))))
            reduce-merge (fn [m1 m2]
                           (reduce merge-into (or m1 {}) (seq m2)))]
        (reduce reduce-merge ms)))))

(defcheck solution-7137b673
  (fn [f init & x]
    (reduce
      (fn [a b]
        (reduce #(merge %1
                   (cond
                     (and (contains? a %2) (contains? b %2)) {%2 (f (get a %2) (get b %2))}
                     (contains? b %2) {%2 (get b %2)}))
          a (keys b)))
      init x)))

(defcheck solution-71534783
  (fn [f & ms]
    (letfn [(update-acc [f acc m]
              (if (empty? m)
                acc
                (let [[k v] (first m)]
                  (if (acc k)
                    (recur f (assoc acc k (f (acc k) v)) (rest m))
                    (recur f (assoc acc k v) (rest m))))))]
      (loop [[m & ms'] ms, acc {}]
        (if (nil? m)
          acc
          (recur ms' (update-acc f acc m)))))))

(defcheck solution-71611c72
  (fn [f & ms]
    (reduce #(reduce (fn [tmm [k v]]
                       (if (contains? tmm k)
                         (assoc tmm k (f (tmm k) v))
                         (assoc tmm k v)))
               % %2)
      {} ms)))

(defcheck solution-7164491c
  (fn [f & ms]
    (->> ms (apply concat) (group-by key)
      (map (fn [[k vs]] [k (reduce f (map val vs))]))
      (into {}))))

(defcheck solution-716d6f6e
  (fn mm[f & ms]
    (reduce
      (fn[acc, map] (reduce (fn[m [k v]] (update-in m [k] (fn[v2] (if v2 (f v2 v) v)))) acc map))
      ms)))

(defcheck solution-7190b2a3
  (fn my-merge-with-- [func & args]
    (letfn [(f-merge [m i]
              (let [k (key i) v (val i)]
                (if (contains? m k)
                  (conj m {k (func (get m k) v)})
                  (conj m {k v}))))]
      (reduce #(reduce f-merge %1 %2) args))))

(defcheck solution-71ba27c7
  (fn merge [f & xs]
    (reduce
      (fn [x y]
        (reduce (fn [x' [k v]]
                  (assoc x' k (if (x' k) (f v (x' k)) v)))
          y x))
      (first xs) (rest xs))))

(defcheck solution-71beb999
  (fn [f & ms] (->> (apply concat ms)
                 (group-by key)
                 (reduce #(assoc % (key %2) (vals (val %2))) {})
                 (reduce #(assoc % (key %2) (reduce f (val %2))) {}))))

(defcheck solution-7228ff36
  (fn [f m & ms]
    (reduce
      (fn [m n]
        (reduce
          (fn [res [k v]]
            (assoc res k
                       (if-let [v' (res k)]
                         (f v' v)
                         v)))
          m n))
      m ms)))

(defcheck solution-724bee1b
  (fn [f m & args] (reduce (fn [mp [k v]] (if (not (nil? (get mp k))) (assoc mp k (f (get mp k) v)) (assoc mp k v))) m (apply merge args))))

(defcheck solution-73719937
  (fn my-merge-with [f & maps]
    (reduce
      (fn [a b]
        (loop [res a sec (seq b)]
          (if (empty? sec)
            res
            (let [[[k v] & more] sec]
              (if (contains? res k)
                (recur (assoc res k (f (res k) v)) more)
                (recur (assoc res k v) more)
                ))
            )
          ))
      {} maps)
    ))

(defcheck solution-739c11ef
  (fn [f & ms]
    (reduce (fn [m m2] (reduce (fn [m e]
                                 (if-let [v (m (first e))]
                                   (assoc m (first e) (f v (second e)))
                                   (conj m e))) m m2)) ms)))

(defcheck solution-73b86d8
  (fn [f & coll]
    (apply merge
      (for [x (group-by #(first %) (reduce into [] coll))]
        {(first x) (let [[c & ls] (map second (second x))] (if (> (count ls) 0) (apply f c ls) c))}))))

(defcheck solution-740902b2
  (fn [f & maps]
    (reduce #(reduce (fn [m [k v]]
                       (if (contains? m k)
                         (update-in m [k] f v)
                         (assoc m k v)))
               %1 %2)
      {} maps)))

(defcheck solution-743dc8c9
  ;-------------------------------
  (fn [f & ms]
    (reduce (fn [m1 m2]
              (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (update-in m [k] f v)
                          (assoc m k v)))
                m1 m2))
      ms)))

(defcheck solution-74a28f4
  (fn [f & ms]
    (let [grouped (group-by first
                    (apply concat
                      (map (fn [m] (for [k (keys m)] (vector k (m k)))) ms)))]
      (into {} (map (fn [m] (vector m (reduce f (map last (grouped m))))) (keys grouped))))))

(defcheck solution-75123cbf
  #(reduce (fn foo [a b]
             (reduce (fn bar [i [k v]]
                       (assoc i k (if (i k) (% (i k) v) v)))
               a b))
     %&))

(defcheck solution-751e8371
  (fn [f & ms]
    (reduce (fn [r m]
              (reduce #(assoc %1 (%2 0) (if (nil? (%1 (%2 0)))
                                          (%2 1)
                                          (f (%1 (%2 0)) (%2 1))))
                r m))
      {} ms)))

(defcheck solution-75c4e52e
  (fn mw [f & maps]
    (reduce (fn [s e]
              (reduce (fn [t [k v]]
                        (if (t k)
                          (assoc t k (f (t k) v))
                          (conj t [k v])
                          )
                        )
                s e
                ))
      maps
      )
    ))

(defcheck solution-760a2162
  (fn [f m1 & ms]
    (let [m2 (apply merge ms)]
      (merge m2 (into {} (map #(vector (key %) (f (val %) (m2 (key %)))) m1))))))

(defcheck solution-76452bab
  (fn [f seed & maps]
    (reduce (fn [r m]
              (reduce-kv (fn [ir k v]
                           (assoc ir k (if (contains? r k) (f (get r k) v) v)))
                r m))
      seed maps)))

(defcheck solution-7690e9c2
  (fn [f & maps]
    (reduce
      (fn [a m]
        (reduce-kv
          (fn [a k v]
            (if (contains? a k)
              (assoc a k (f (a k) v))
              (assoc a k v)))
          a
          m))
      {}
      maps)))

(defcheck solution-7697a9fd
  (fn [f & ms]
    (reduce
      (fn [result m]
        (reduce
          #(if (contains? %1 %2)
             (assoc %1 %2 (f (get %1 %2) (get m %2)))
             (assoc %1 %2 (get m %2)))
          result (keys m)))
      {} ms)))

(defcheck solution-77554939
  (fn my-merge-with [op result & maps]
    (reduce (fn [r m]
              (reduce (fn [result [k v]]
                        (if (contains? result k)
                          (assoc result k (op (get result k) v))
                          (assoc result k v))) r m))
      result
      maps)))

(defcheck solution-775f03ed
  (fn mergewith [f m & ms]
    (reduce (fn [mi mo]
              (reduce (fn [mi [k v]]
                        (assoc mi k (if (contains? mi k)
                                      (f (mi k) v)
                                      v)))
                mi mo))
      m
      ms)))

(defcheck solution-777caecb
  (fn merge_ [f m & maps]
    (let [s (mapcat seq maps)]
      (reduce #(if (contains? % (first %2))
                 (update-in % [(first %2)] f (second %2))
                 (assoc % (first %2) (second %2))) m s))))

(defcheck solution-77cdddd2
  (fn foo [f m & maps]
    (if (seq maps)
      (let [[n & r] maps
            conj-ed #(reduce (fn [acc el]
                               (assoc acc (first el)
                                          (if (contains? acc (first el))
                                            (f (acc (first el)) (second el) )
                                            (second el )) ))  %1 %2) ]
        (apply foo (into [f (conj-ed m n)] r)))
      m)))

(defcheck solution-77d2f66b
  (fn [f m0 & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]]
            (if (contains? m k)
              (assoc m k (f (get m k) v))
              (assoc m k v)))
          m1
          m2))
      m0
      maps
      )))

(defcheck solution-77dda5f2
  (fn __ [f & coll]
    (letfn [(in [m1 m2]
              (reduce
                (fn [head [k v]]
                  (if-let [t (head k)]
                    (merge head {k (f t v)})
                    (merge head {k v})))
                m1 m2))]
      (reduce in coll))))

(defcheck solution-77e53659
  (fn[f & maps]
    (letfn [(f-merge-1 [f u v]
              (letfn [(f-into [f m k]
                        (if (m k)
                          (into m {k (f (m k) (v k))})
                          (into m {k (v k)})))]
                (reduce #(f-into f %1 %2) u (keys v))))]
      (reduce #(f-merge-1 f %1 %2) maps))))

(defcheck solution-7845e4fb
  (fn [f & xs]
    (reduce
      (fn [acc m]
        (apply merge acc
          (for [k (keys m)]
            {k (if (contains? acc k)
                 (f (get acc k) (get m k))
                 (get m k))}))) {} xs)))

(defcheck solution-7890f294
  (fn [op & ms]
    (letfn [(frob [acc [k v]]
              (assoc acc k
                         (if-let [v' (acc k)]
                           (op v' v)
                           v)))]
      (reduce frob {} (mapcat seq ms)))))

(defcheck solution-7897455c
  (fn [f & m]
    (apply hash-map (mapcat (fn [e] (vector (first e) (reduce f (second e))))
                      (reduce
                        (fn [f s]
                          (into f (map #(vector (first %) (conj (f (first %) []) (second %))) s)))
                        {}
                        m)))))

(defcheck solution-78ccf695
  (fn g [f & xs]
    (->> (apply concat xs)
      (group-by first)
      (map (fn [[k v]] [k (reduce f (map second v))]))
      (into {}))))

(defcheck solution-7936a95d
  (fn [op & maps]
    (letfn [(mrg [m1 m2]
              (loop [m m1 [k & ks] (keys m2)]
                (if k
                  (if-let [oldv (m k)]
                    (recur (assoc m k (op oldv (m2 k))) ks)
                    (recur (assoc m k (m2 k)) ks))
                  m)))]
      (loop [m (first maps)
             r (rest maps)]
        (if (seq r)
          (recur (mrg m (first r)) (rest r))
          m)))))

(defcheck solution-7969d266
  (fn my-merge-with
    [f & s]
    (let [dkeys (distinct (mapcat keys s))
          m {}
          v (apply map list (map #(map %1 dkeys) s)) ]
      (apply merge (map (partial assoc {})
                     dkeys
                     (map (fn [xs]
                            (if (#{1} (count xs))
                              (first xs)
                              (apply f xs)))
                       (map (partial filter #(not (nil? %))) v)))))))

(defcheck solution-79a915eb
  (fn [f & maps]
    (let [ks (set (mapcat keys maps))]
      (reduce (fn [m k]
                (let [args (remove nil? (map #(% k) maps))]
                  (assoc m k (reduce f args))))
        {} ks))))

(defcheck solution-79c0ba46
  (fn [o m & s]
    (reduce #(reduce
               (fn [m [k v]]
                 (assoc m k
                          (if (m k)
                            (o (m k) v)
                            v))) % %2)
      m s)))

(defcheck solution-79cc6d34
  (fn myMergeWith [f m & ms] (reduce (fn [x y] (reduce #(if (nil? (%1 (key %2)))
                                                          (conj %1 %2)
                                                          (update-in %1 [(key %2)] f (val %2)))
                                                 x
                                                 y))
                               m
                               ms)))

(defcheck solution-7a0bb3f4
  (fn merge-with- [op init & maps]
    (letfn [(upsert [result k v]
              (if (contains? result k)
                (update-in result [k] op v)
                (assoc-in result [k] v)))]
      (reduce
        #(reduce-kv upsert %1 %2)
        init
        maps))))

(defcheck solution-7a35a9f7
  (fn [f & maps]
    (letfn [(merge-maps [m1 m2]
              (loop [dest m1 r m2]
                (if-let [[k v] (first r)]
                  (if-let [dv (get dest k)]
                    (recur (assoc dest k (f dv v)) (next r))
                    (recur (assoc dest k v) (next r)))
                  dest)))]
      (reduce merge-maps
        (first maps)
        (rest maps)))))

(defcheck solution-7a40f9aa
  (fn [p & s]
    (loop [x (next s)
           acc (first s)]
      (if x
        (recur (next x) (into acc (map #(let [[k v] %]
                                          (if (contains? acc k)
                                            (vector k (p (get acc k) v))
                                            (vector k v)))
                                    (first x))))
        acc))))

(defcheck solution-7a44668c
  #(letfn [
           (g [m e]
             (let [k (key e) v (val e)]
               (if (contains? m k)
                 (assoc m k (% (m k) v))
                 (assoc m k v))))
           (h [m n]
             (reduce g (or m {}) (seq n)))]
     (reduce h %&)))

(defcheck solution-7a4b9067
  (fn [f & l]
    (->> (mapcat keys l)
      set
      (map (fn [k] {k (reduce f (filter #(not (nil? %)) (map #(% k) l)))}))
      (apply merge))))

(defcheck solution-7a67c542
  (fn [f & ms]
    (reduce
      (fn [a s]
        (reduce #(let [k (first %2) v (second %2) x (% k)]
                   (assoc % k (if x (f x v) v))) a s))
      {} ms)))

(defcheck solution-7a935bc2
  (fn [f mp & mps]
    (loop [m mp
           ms mps]
      (if (empty? ms)
        m
        (recur
          (reduce (fn [accum [k v]]
                    (if-let [val (get accum k)]
                      (conj accum [k (f val v)])
                      (conj accum [k v])))
            m (first ms))
          (rest ms))))))

(defcheck solution-7a9c45b6
  (fn merge-with-fn [oper & vmaps]
    (let [fvec (mapcat vec (map #(seq %) vmaps))]
      (letfn [(mem-test [m v-val]
                (let [[k v] v-val]
                  (if (contains? m k)
                    (assoc m k (oper (get m k) v))
                    (assoc m k v))))]
        (reduce mem-test {} fvec)))))

(defcheck solution-7aa364c5
  (fn [o & m]
    (reduce
      (fn [r [k v]]
        (assoc r k
                 (if
                  (r k)
                   (o (r k) v)
                   v)))
      {}
      (apply concat m))))

(defcheck solution-7ab633a0
  (fn [f h & hs]
    (reduce
      #(reduce (fn [h [k v]]
                 (assoc h k (if (contains? h k)
                              (f (get h k) v)
                              v)))
         % %2)
      h hs)))

(defcheck solution-7ac9168
  (fn mw
    ([f m m1]
     (if (empty? m1)
       m
       (let [k (first (keys m1))
             ov (get m k)
             nv (get m1 k)]
         (if (nil? ov)
           (mw f (assoc m k nv) (dissoc m1 k))
           (mw f (assoc m k (f ov nv)) (dissoc m1 k))))))
    ([f m m1 & ms]
     (reduce (partial mw f) (mw f m m1) ms))))

(defcheck solution-7b12766f
  (fn [g & n]
    (into {}
      (map #(do [(key %) (reduce g (map val (val %)))])
        (group-by key (apply concat n))))))

(defcheck solution-7b89f10e
  (fn [f & ms]
    (loop [xs (mapcat seq ms), res {}]
      (if (empty? xs) res
                      (let [[k v] (first xs)]
                        (recur (rest xs)
                          (assoc res k (if (contains? res k) (f (res k) v) v))))))))

(defcheck solution-7bb6704e
  (fn [f & ms]
    (reduce
      (fn [m1 m2]
        (into {} (for [k (set (mapcat keys [m1 m2]))]
                   [k
                    (cond
                      (not (contains? m2 k)) (m1 k)
                      (not (contains? m1 k)) (m2 k)
                      :else (f (m1 k) (m2 k)))])))
      ms)))

(defcheck solution-7be25a7d
  (fn eka
    ([op xr & xs]
     (if (empty? xs) xr
                     (recur op
                       (into {}
                         (for [k (set (concat (keys xr)(keys (first xs))))
                               :let [v1 (get xr k)
                                     v2 (get (first xs) k)
                                     v (if (and v1 v2) (op v1 v2)
                                                       (if v1 v1 v2))
                                     ]]
                           [k v]
                           )
                         )
                       (rest xs)))
     )))

(defcheck solution-7c33036b
  ;no apply-recur :(
  (fn m-with [f & maps]
    (let [m1 (first maps) m2 (second maps)]
      (cond
        (nil? m1) nil
        (nil? m2) m1
        (empty? m2) (apply m-with f m1 (drop 2 maps))
        :else
        (apply
          m-with f
          (let [[k v] (first m2)
                cur-value (get m1 k nil)]
            (if (nil? cur-value)
              (assoc m1 k v)
              (assoc m1 k (f cur-value v))
              )
            )
          (rest m2)
          (drop 2 maps)
          )
        )
      )
    ))

(defcheck solution-7c7b38f8
  (fn [f map & others]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]] (assoc m k (if (contains? m k) (f (get m k) v) v)))
          m1 m2))
      map
      others)))

(defcheck solution-7c855767
  (fn [f & maps]
    (reduce #(conj %
               (reduce (fn [r [k v]]
                         (let [rv (get r k)] (assoc r k (if rv (f rv v) v)))) %1 %2))
      maps)))

(defcheck solution-7cb05d2e
  (fn [func & maps]
    (reduce
      (fn [output [pair-key pair-value]]
        (if-let [current-value (get output pair-key)]
          (assoc output pair-key (func current-value pair-value))
          (assoc output pair-key pair-value)))
      {}
      (apply concat maps))))

(defcheck solution-7cb56f64
  (fn  [f & m]
    (let [mr (fn [a b]
               (reduce #(if (find %1 (first %2))
                          (update-in %1 [(first %2)] f (second %2))
                          (conj %1 %2)
                          ) a b)
               )]
      (reduce mr m)
      )))

(defcheck solution-7d2c9e97
  (fn me [f & map-seq]

    (let [keys-set (reduce #(clojure.set/union %1 %2) (map #(into #{} (keys %)) map-seq))

          new-map-seq (for [y keys-set]

                        {y  (filter (complement nil?) (map #(% y) map-seq))}
                        )

          new-map   (apply merge new-map-seq)


          fn-values (fn [f my-map]

                      (for [m my-map]

                        {(key m) (reduce f (val m))}

                        ))


          ]

      (apply merge (fn-values f new-map))
      )

    ))

(defcheck solution-7d8f8353
  (fn my-merge-with
    [f m & xs]
    (if (empty? xs) m
                    (apply my-merge-with f
                      (reduce
                        #(update-in
                           %1 [(%2 0)]
                           (fn [x y] (if (nil? x) y (f x y)))
                           (%2 1))
                        m
                        (first xs))
                      (rest xs)))))

(defcheck solution-7ddddf88
  (fn [f & hs]
    (let [merge-hash (fn [agg [k v]]
                       (if (contains? agg k)
                         (assoc agg k (f (agg k) v))
                         (assoc agg k v)))
          merge-hash-list (fn [agg h]
                            (reduce merge-hash agg h))]
      (reduce merge-hash-list hs))))

(defcheck solution-7e0d86d0
  #(reduce (fn [r [k v]]
             (assoc r k (if (r k)
                          (% (r k) v)
                          v)))
     %2
     (apply concat %&)))

(defcheck solution-7e1ef094
  (fn merge-with-function [f & ms]
    (letfn [(help [m1 m2]
              (let [ks (keys m2)]
                (reduce (fn [m k]
                          (if (m k)
                            (conj m [k (f (m k) (m2 k))])
                            (conj m [k (m2 k)]))) m1 ks)))]
      (loop [an {} ms ms]
        (if (empty? ms)
          an
          (recur (help an (first ms)) (rest ms)))))))

(defcheck solution-7e28c9b8
  (fn
    [f & [m & r]]
    (reduce
      #(reduce
         (fn [m [k v]]
           (if (contains? m k)
             (update-in m [k] f v)
             (conj m [k v])))
         % %2)
      m r)))

(defcheck solution-7eefb0a6
  (fn [f s1 & s2]
    (letfn [(merge_with [s1 s2]
              (reduce (fn [s1 [k v]]
                        (if (s1 k)
                          (assoc s1 k (f (s1 k) v))
                          (assoc s1 k v)))
                s1 s2))]
      (reduce merge_with s1 s2))))

(defcheck solution-7f4ef792
  (fn [f & args]
    (let [k (keys (apply merge args))]
      (into {} (map (fn [x]
                      {x (reduce f
                           (filter #(not (nil? %))
                             (map #(get % x) args)))})
                 k)))))

(defcheck solution-7f8b1d13
  (fn [op & ms]
    (reduce (fn [res m]
              (reduce (fn [g [k v]]
                        (assoc g k (if (g k) (op (g k) v) v))) res m))
      {} ms)))

(defcheck solution-7f8b38cb
  (fn [op & maps]
    (let [ks (apply clojure.set/union
               (map (comp set keys) maps))
          vs #(filter
                (comp not nil?)
                (map (fn [m] (m %)) maps))]
      (into {}
        (map #(hash-map % (reduce op (vs %))) ks)))))

(defcheck solution-7feb9041
  (fn [f & m ]
    (reduce (fn[col el]
              (assoc col el
                         (reduce f (filter #(not (nil? %)) (map #(get % el) m)))
                         )
              )
      {} (keys (apply merge m))
      )

    ))

(defcheck solution-8052dec3
  (fn mergef [f m1 & ms]
    (if (or (nil? ms) (empty? ms)) m1
                                   (let [[m2 & m3] ms
                                         m (reduce #(let [[k v] %2] (assoc %1 k (if (contains? %1 k) (f (%1 k) v) v))) m1 m2)]
                                     (apply mergef f m m3)))))

(defcheck solution-805be268
  (fn g [f i h & t]
    (let [[k & v] (vec h)]
      (if (coll? k)
        (g f (g f i k) (if v `[~@v ~@t] t))
        (let [y #(apply identity %)]
          (if k (assoc i k (if (i k) (f (i k) (y v)) (y v))) i))))))

(defcheck solution-80f157d0
  (fn [f & [h & t :as maps]]
    (let [ks (into #{} (mapcat keys maps))]
      (apply hash-map (interleave ks (map #(reduce f %) (map (fn [k] (filter #(when % %) (map #(% k) maps))) ks)))))))

(defcheck solution-8104c0e4
  (fn xmerge-with
    [f & maps]
    (reduce (fn [acc item]
              (reduce (fn [inacc initem]
                        (if (contains? inacc (first initem))
                          (update-in inacc [(first initem)] f (second initem))
                          (assoc inacc (first initem) (second initem)))) acc item))
      {} maps)))

(defcheck solution-81112d1e
  (fn merge-with-function [f & maps]
    (reduce (fn [ms m]
              (into ms
                (reduce (fn [es e]
                          (let [k (key e)
                                v (val e)]
                            (if (contains? ms k)
                              (assoc es k (f (get ms k) v))
                              (assoc es k v)))) {} m))) {} maps)))

(defcheck solution-812ad1db
  (fn mw
    ([f m1 m2]
     (loop [m1 m1 m2 m2]
       (if (seq m2)
         (let [[k v2] (first m2)]
           (if (contains? m1 k)
             (recur (update-in m1 [k] (fn [v1] (f v1 v2))) (rest m2))
             (recur (assoc m1 k v2) (rest m2))))
         m1)))
    ([f m1 m2 & more]
     (apply mw f (mw f m1 m2) (first more) (rest more)))))

(defcheck solution-81400572
  (fn [f & m]
    (reduce #(loop [k (keys %2) res %1]
               (if (empty? k)
                 res
                 (let [k1 (first k)]
                   (recur (rest k) (assoc res k1 (if (%1 k1) (f (%1 k1) (%2 k1)) (%2 k1))))))) m)))

(defcheck solution-82e08d85
  (fn [f & maps]
    (into {} (map
               (fn [x]
                 [x (reduce f (keep #(get % x) maps))])
               (into #{} (mapcat keys maps))))
    ))

(defcheck solution-8309cd4
  (fn [x & y]
    (reduce
      (fn [u v]
        (apply hash-map
          (mapcat
            #(if (and (contains? u (first %)) (contains? v (first %)))
               [(first %) (x (u (first %)) (v (first %)))]
               %)
            (merge u v))))
      y)))

(defcheck solution-83aa5c2f
  (fn [f & maps]
    (let [combine-vals (fn [a b]
                         (cond
                           (nil? a) b
                           (nil? b) a
                           :else (f a b)))

          combine-keys (fn [m1 m2]
                         (keys (merge m1 m2)))

          combine-maps (fn [m1 m2]
                         (zipmap (combine-keys m1 m2)
                           (for [k (combine-keys m1 m2)]
                             (combine-vals (m1 k) (m2 k)))))
          ]
      (reduce combine-maps maps))))

(defcheck solution-841fb92b
  (fn mf [f r & maps]
    (if (empty? maps) r
                      (let [wf (fn [x y] (if y (if x (f x y) y) x))
                            fm (first maps)
                            ks (concat (keys r) (keys fm))
                            result (into {}
                                     (for [k ks]
                                       {k (wf (r k) (fm k))}))]
                        (apply (partial mf f result) (rest maps))))))

(defcheck solution-8422c151
  (fn mw [f & maps]
    (reduce
      (fn [result m]
        (let [common-keys (clojure.set/intersection (set (keys m)) (set (keys result)))
              to-merge (apply (partial dissoc m) (keys result))
              res (apply merge (map #(hash-map % (f (get result %) (get m %))) common-keys))]
          (merge result res to-merge)))
      maps)))

(defcheck solution-8463a71f
  (fn [f & ms]
    (reduce
      (fn [acc m]
        (reduce (fn [kvs [k v]]
                  (assoc kvs k
                             (if (contains? kvs k)
                               (f (kvs k) v)
                               v)))
          acc
          m)
        )
      {}
      ms)))

(defcheck solution-84c511d5
  (fn merge-with' [op & maps]
    (reduce
      (fn [acc m]
        (apply assoc acc
          (mapcat (fn [k]
                    (list k
                      (if (acc k)
                        (op (acc k) (m k))
                        (m k))))
            (keys m))))
      maps)))

(defcheck solution-84dfbb47
  (fn [func result & map-seq]
    (loop [map-seq map-seq
           result result]
      (if (empty? map-seq)
        result
        (recur (rest map-seq)
          (loop [head-map (first map-seq)
                 result result]
            (if (empty? head-map)
              result
              (let [head-item (first head-map)
                    tail-items (rest head-map)
                    [head-key head-value] head-item]
                (recur tail-items (merge result {head-key
                                                 (if (contains? result head-key)
                                                   (func (result head-key) head-value)
                                                   head-value)}))))))))))

(defcheck solution-855464af
  (fn [f & maps]
    (letfn [(merger [m [k v]]
              (if (contains? m k)
                (assoc m k (f (get m k) v))
                (assoc m k v)))]
      (reduce #(reduce merger %1 %2) maps))))

(defcheck solution-85fe04b5
  (fn [f & maps]
    (letfn [(r [acc m]
              (let [keys (clojure.set/union (set (keys acc)) (set (keys m)))]
                (into {}
                  (for [key keys]
                    [key (if (contains? acc key)
                           (if (contains? m key)
                             (f (acc key) (m key))
                             (acc key))
                           (m key))]))))]
      (reduce r maps))))

(defcheck solution-8623050a
  (fn [f & maps]
    (reduce (fn [m1 m2]
              (reduce #(let [[k v] %2
                             init (% k)]
                         (assoc % k (if init (f init v) v)))
                m1 m2))
      maps)))

(defcheck solution-8682a681
  (fn [f & ms]
    (let [keys (reduce
                 (fn [keyset m]
                   (into keyset (map key (seq m))))
                 #{}
                 ms)]
      (into {} (map (fn [k] [k (reduce f
                                 (filter #(not (nil? %)) (map #(get % k) ms)))] ) keys )) )))

(defcheck solution-86f2b58c
  (fn f1 [f m1 & maps]
    (let [m2 (reduce into {} maps)]
      (reduce #(if (not (nil? (get % (key %2))))
                 (assoc % (key %2) (f (get % (key %2)) (val %2)))
                 (conj % %2)) m1 m2))))

(defcheck solution-87285f1b
  (fn [func base & values]
    (let [update (fn [b kv]
                   (if (contains? b (key kv))
                     (update-in b [(key kv)] func (val kv))
                     (conj b kv)))]
      (reduce #(reduce update % %2) base values))))

(defcheck solution-87da56bc
  (fn [o & a]
    (reduce (fn [a m]
              (reduce (fn [b [k v]]
                        (update-in b [k] #(if-not % v (o % v)))) a m)) (first a) (rest a))))

(defcheck solution-87f4fa02
  (fn [f & args]
    (->> (apply concat args)
      (group-by first)
      (reduce #(conj %
                 [(first %2)
                  (let [x (map second (second %2))]
                    (if (= 1 (count x))
                      (apply identity x)
                      (apply f (map second (second %2)))))])
        {}))))

(defcheck solution-88068857
  (fn [f & v]
    (->>
      (mapcat #(into [] %) v)
      (group-by first)
      (map (fn [[k v]] (vector k (map #(second %) v))))
      (map (fn [[k v]] (vector k (if (< 1 (count v))
                                   (apply f v)
                                   (first v)))))
      (into {}))))

(defcheck solution-88d6ef14
  (fn m-w
    ([f a b & m]
     (apply (partial m-w f (m-w f a b)) m))
    ([f a b]
     (let [[gm lm] (if (> (count a) (count b)) [a b] [b a])]
       (merge lm (reduce (fn [s [k v]]
                           (if-let [v2 (lm k)]
                             (merge s (if (= gm a) {k (f v v2)} {k (f v2 v)}))
                             (merge s {k v}))) {} gm))))))

(defcheck solution-88df8a44
  (fn [f & maps]
    (reduce (fn [res map]
              (reduce (fn [r m]
                        (let [k (key m)]
                          (assoc r k (if (contains? r k) (f (r k) (val m)) (val m))))) res map))
      {} maps)))

(defcheck solution-89be94a2
  (fn [f & maps]
    (reduce
      (fn [result m]
        (reduce
          (fn [result [k v]]
            (if (contains? result k)
              (update-in result [k] f v)
              (assoc result k v)))
          result m))
      maps)))

(defcheck solution-89d955c
  (fn my-merge-with [f m & ms]
    (if (empty? ms)
      m
      (let [new-m (reduce (fn [acc [k v]]
                            (if (acc k)
                              (assoc acc k (f (acc k) v))
                              (assoc acc k v)))
                    m
                    (first ms))]
        (recur f new-m (rest ms))))))

(defcheck solution-89eb8bbf
  (fn [f m & maps]
    (into
      {}
      (for [mp maps
            [k v] mp
            :let [nv (if (contains? m k)
                       (f (get m k) v)
                       v)]]
        [k nv]))))

(defcheck solution-8abe5e94
  (fn my-merge-with [f & maps]
    (letfn [(map-to-hash [fcn coll] (reduce #(assoc %1 %2 (fcn %2)) {} coll))]
      (map-to-hash (fn [k] (reduce f (keep #(% k) maps)))
        (reduce #(into % (keys %2)) #{} maps)))))

(defcheck solution-8b4ad14a
  (fn [f & maps]
    (reduce (fn [acc m]
              (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (assoc m k (f (get m k) v))
                          (assoc m k v)))
                acc
                m))
      maps)))

(defcheck solution-8baca295
  (fn my-merge-with [f init & xs]
    (reduce #(assoc %1 (first %2) (if (%1 (first %2)) (f (%1 (first %2)) (second %2)) (second %2))) init (mapcat seq xs))))

(defcheck solution-8bd68790
  (fn f [f & maps]
    (into {}
      (for [k (distinct (mapcat keys maps))]
        (let [vals (keep identity (map #(% k) maps))]
          [k (if (seq (rest vals))
               (apply f vals)
               (first vals))])))))

(defcheck solution-8c16be5
  (fn n69 [f & maps]
    (letfn [(merge-entry [m e] (let [[k v] e] (if (contains? m k) (assoc m k (f (get m k) v)) (assoc m k v))))]
      (reduce #(reduce merge-entry %1 (seq %2)) maps)
      )))

(defcheck solution-8c25a7f6
  (fn [f m & es]
    (reduce
      (fn m1 [c e]
        (reduce #(let [[k v] %2 o (%1 k)] (assoc %1 k (if o (f o v) v)))
          c e))
      m es)))

(defcheck solution-8cc8e49c
  (fn [f & maps]
    (let [[old-map & [merge-map & more-maps]] maps
          new-map
          (reduce (fn [m [k v]]
                    (let [collision? (contains? m k)
                          new-val (if collision?
                                    (f (m k) v)
                                    v)]
                      (conj m [k new-val])))
            old-map
            merge-map)]
      (if more-maps
        (recur f (cons new-map more-maps))
        new-map))))

(defcheck solution-8cdef01e
  (fn [f & ms] (letfn [(mw [m1 m2] (reduce #(assoc %1 (key %2) (if (%1 (key %2)) (f (%1 (key %2)) (val %2)) (val %2))) m1 m2))]
                 (reduce mw ms))))

(defcheck solution-8cef9aa
  (fn [f & ms]
    (reduce
      (fn [ret-all vvs]
        (reduce (fn [ret vv]
                  (let [k1 (key vv)
                        v1 (val vv)
                        v0 (get ret k1)]
                    (assoc ret k1
                               (if (= nil v0) v1 (f v0 v1)))))
          ret-all vvs))
      {} ms)
    ))

(defcheck solution-8d00f6f
  (fn [f t & s]
    (reduce (fn [t s]
              (loop [s s acc t]
                (if (empty? s)
                  acc
                  (let [e (first s)
                        k (key e)
                        vs (val e)
                        vacc (get acc k)
                        vn (if (nil? vacc) vs (f vacc vs))]
                    (recur (rest s) (assoc acc k vn))))))
      t
      s)))

(defcheck solution-8d0dc7e5
  (fn [func & maps]
    (->> maps
      (apply concat)
      (group-by key)
      (map (fn [[k vs]]
             [k (reduce func (map val vs))]))
      (into {}))))

(defcheck solution-8d78dd14
  (fn [f m & c]
    (let [r #(zipmap (keys %) (map f (vals %) (vals %2)))
          s #(sort (select-keys % (keys %2)))]
      (reduce #(merge % %2 (r (s % %2) (s %2 %))) m c))))

(defcheck solution-8da3eafe
  (fn [f & maps]
    (into {} (map (fn [[k v]] (vec [k (reduce f (map second v))]))
               (group-by first (apply concat (map vec maps)))))))

(defcheck solution-8db1d8f0
  (fn merge-with-2
    ([f m1]
     m1)
    ([f m1 m2]
     (reduce (fn [m [k v]]
               (assoc m k
                        (if (contains? m k)
                          (f (m k) v)
                          v)))
       m1 m2))
    ([f m1 m2 & more]
     (apply merge-with-2 f (merge-with-2 f m1 m2) more))))

(defcheck solution-8e31a3ec
  (fn _ [f init & args]
    (if-not (seq args)
      init
      (letfn [(merge' [m1 m2]
                (reduce (fn [acc [k v]] (assoc acc k (if-let [v' (acc k)] (f v' v) v))) m1 m2))]
        (apply _ f (merge' init (first args)) (rest args))))))

(defcheck solution-8ed10794
  (fn my-merge-with [f m & ms]
    (reduce (fn [m1 m2]
              (reduce (fn [cm [k v]]
                        (if (cm k) (assoc cm k (f (cm k) v))
                                   (assoc cm k v))) m1 m2))
      m ms)))

(defcheck solution-8f06b1f4
  (fn [op base & others]
    (letfn [(acc-val [base-map [key val]]
              (if (contains? base-map key)
                (assoc base-map key (op (get base-map key) val))
                (assoc base-map key val)))
            (acc-map [base-map other-map]
              (reduce acc-val base-map other-map))]
      (reduce acc-map base others))))

(defcheck solution-8f301f65
  (fn [f & ms]
    (let [merge (fn [m1 m2]
                  (reduce #(if (contains? %1 (first %2))
                             (assoc %1 (first %2) (f (m1 (first %2)) (second %2)))
                             (assoc %1 (first %2) (second %2)))
                    m1 m2))]
      (reduce merge ms))))

(defcheck solution-8f3fa9b7
  (fn [f m & maps]
    (reduce (fn [a ent]
              (reduce (fn [b [x y]]
                        (if (contains? b x)
                          (assoc b x (f (b x) y))
                          (assoc b x y)
                          )) a ent)
              )
      m maps)))

(defcheck solution-8f60140f
  (fn [f & m]
    (reduce
      (partial reduce
        #(conj %1
           (let [k (first %2)]
             (if (contains? %1 k)
               [k (f (get %1 k) (last %2))]
               %2))))
      m)))

(defcheck solution-8f603e75
  (fn [f x & xs]
    (reduce
      (fn [rm sm]
        (reduce
          (fn [m [k v]]
            (if (contains? m k)
              (assoc m k (f (m k) v))
              (assoc m k v)))
          rm sm))
      x xs)))

(defcheck solution-90c5ffbe
  (fn [r f & ms]
    (r
      #(r (fn [m [k v]] (update-in m [k] (fn [o] (if o (f o v) v))))
         % %2)
      ms)) reduce)

(defcheck solution-916e1225
  (fn [f & maps]
    (loop [result (first maps),
           remaining (next maps)]
      (if-not remaining
        result
        (let [m (first remaining),
              acc (reduce (fn [init [k v]]
                            (if-not (contains? init k)
                              (assoc init k v)
                              (assoc init k (f (init k) v))))
                    result m)]
          (recur acc (next remaining)))))))

(defcheck solution-91dc2144
  (fn [f & maps]
    (reduce
      (fn [a b]
        (reduce
          (fn [m [k v]]
            (assoc m k (if-let [[_ v'] (find b k)] (f v v') v)))
          b a))
      (first maps) (rest maps))))

(defcheck solution-91df927
  (fn [f & args]
    (let [ms (mapcat vec args)]
      (reduce #(if-let [existing (%1 (first %2))] (assoc %1 (first %2) (f existing (second %2) )) (into %1 [%2]) ) {} ms)
      )))

(defcheck solution-92213e15
  (fn [f & maps]
    (->> maps
      (mapcat seq)
      (group-by first)
      (reduce
        (fn [rv [k vs]]
          (assoc rv k (->> (map second vs)
                        (reduce f))))
        {}))))

(defcheck solution-926082d
  (fn [f & maps]
    (reduce (fn [map1 map2]
              (reduce (fn [map1 item]
                        (update-in
                          map1
                          [(key item)]
                          (fn [value]
                            (if (nil? value)
                              (val item)
                              (f value (val item)))))) map1 map2)) maps)))

(defcheck solution-9282356f
  (fn [f m & ms]
    (loop [out m in ms]
      (if (empty? in)
        out
        (recur
          (reduce
            (fn [r [k v]] (assoc r k (if (contains? r k) (f (get r k) v) v)))
            out
            (first in))
          (rest in))))))

(defcheck solution-93046b8a
  (fn [f & args]  (reduce  (fn [a b] (merge a  (into {} (map (fn [[k v]] (if (contains? a k) [k (f (a k) v)] [k v]))  b)))) args)))

(defcheck solution-93798dfc
  (fn [f & maps]
    (reduce #(loop [[k & ks] (keys %2)
                    out      %1]
               (if (not k) out
                           (recur ks (assoc out k (if-let [v (%1 k)]
                                                    (f v (%2 k))
                                                    (%2 k))))))
      maps)))

(defcheck solution-93f80d36
  (fn [func & colls]
    (reduce
      (fn [acc-map x]
        (merge acc-map
          (reduce
            (fn [acc [k v]]
              (if (acc-map k)
                (assoc acc k (func (acc-map k) v))
                (assoc acc k v)))
            {}
            x)))
      {}
      colls)))

(defcheck solution-941f2443
  (fn [f m & maps]
    (->
      (fn [m1 m2]
        (-> (fn [m k]
              (assoc m k (if (m k)
                           (f (m k) (m2 k))
                           (m2 k))))
          (reduce m1 (keys m2))))
      (reduce m maps))))

(defcheck solution-94668a3b
  (fn [f m & more]
    (reduce
      (fn [acc [k v]]
        (conj acc (if (contains? m k) [k (apply f [(get m k) v])] [k v])))
      {} (apply merge more))))

(defcheck solution-94ab159a
  (fn [f & vs]
    (reduce (fn [r m]
              (into r (for [[k v] m]
                        (if (r k)
                          [k (f (r k) v)]
                          [k v])))) vs)))

(defcheck solution-94b95491
  (fn new-merge-with
    ([x & y]
     #_(println y)
     (let [first-map (first y)
           second-map (second y)
           new-merge-with-fn (fn [x-map x-entry]
                               #_(println "new-merge-with-fn" x-map x-entry)
                               (if (contains? x-map (key x-entry))
                                 (merge x-map {(key x-entry) (x (get x-map (key x-entry)) (val x-entry))})
                                 (conj x-map x-entry)))
           apply-f-for-entry (fn iterate-map [f x-map y-map]
                               #_(println "iterate-map" x-map y-map)
                               (if (= 0 (count y-map))
                                 x-map
                                 (iterate-map f (f x-map (find y-map (first (keys y-map)))) (dissoc y-map (first (keys y-map))))))]

       (if (nil? second-map)
         first-map
         (if (= 2 (count y))
           (apply-f-for-entry new-merge-with-fn first-map second-map)
           (recur x (list* (apply-f-for-entry new-merge-with-fn first-map second-map) (rest (rest y))))))))))

(defcheck solution-94e97c1e
  (fn
    [f & colls]
    (->>
      (for [k (distinct (apply concat (map keys colls)))]
        [k
         (reduce f
           (reduce #(if (%2 k)
                      (conj % (%2 k))
                      %)
             []
             colls))])
      (apply concat)
      (apply hash-map))))

(defcheck solution-95949b
  #(reduce (fn [a [k v]] (assoc a k (if (a k) (% (a k) v) v))) %2 (apply merge %&)))

(defcheck solution-9597c980
  (fn [f & r]
    (let [ks (set (flatten (map keys r)))]
      (apply merge
        (for [k ks]
          {k (reduce f (remove nil? (map #(get % k) r)))})))))

(defcheck solution-959efc4a
  (fn [f & maps]
    (let [key-grouped (group-by key (apply concat maps))
          apply-f     (fn [x]
                        (if (> (count x) 1)
                          (apply f (map val x))
                          (val (first x))))
          mapped      (map #(hash-map (key %) (apply-f (val %))) key-grouped)
          answer      (apply conj mapped)]
      answer)))

(defcheck solution-961e3287
  (fn [f & ms]
    (letfn
     [(xf [a b]
        (cond
          (nil? a) b
          (nil? b) a
          :else (f a b)))
      (merge2 [a b]
        (let [ks (keys (merge a b))]
          (reduce (fn [m k] (assoc m k (xf (a k) (b k)))) {} ks)))]
      (reduce merge2 {} ms))))

(defcheck solution-962a79ba
  (fn [op & args]
    (reduce
      (fn [a [k x]]
        (if (contains? a k)
          (assoc a k (op (a k) x))
          (assoc a k x)))
      {} (apply concat args))))

(defcheck solution-962f57c4
  (fn [f & maps]
    (loop [[head & tail] maps
           acc {}]
      (if head
        (recur
          tail
          (reduce
            (fn [m [k v]]
              (let [old-v (get m k)]
                (if old-v
                  (assoc m k (f old-v v))
                  (assoc m k v))))
            acc head))
        acc))))

(defcheck solution-96a4ebf0
  (fn mymerge [op m & more] ((fn mysubmerge [m s] (if s (mysubmerge (let [e {((first s) 0) ((first s) 1)} k (key (first e)) v1 (val (first e))] (if-let [v (m k)] (merge m {k (op v v1)}) (merge m e))) (next s)) m)) m (apply merge more))))

(defcheck solution-96ead696
  (fn [f & ms] (into {} (map (fn [col] [(first (first col)) (reduce f (map second col))] ) (partition-by first (sort-by first (mapcat #(into [] %) ms )) )))))

(defcheck solution-970bdeab
  (fn [f & maps]
    (reduce (fn [a b] (into a (map (fn [[k v]] (if (a k)
                                                 {k (f (a k) (b k))}
                                                 {k v}))
                                b))) maps)))

(defcheck solution-9733c67e
  (fn [f & ms]
    (apply hash-map (mapcat concat (for [k (into #{} (mapcat keys ms))
                                         :let [[m & r :as ms] (filter #(find %1 k) ms)]]
                                     (if (> (count ms) 1)
                                       [k (apply f (map #(%1 k) ms))]
                                       [k (m k)]))))))

(defcheck solution-97889332
  (letfn [(mymerge-with
            ([f m1 m2]
             (reduce (fn [res [k v]] (if-let [rv (res k)] (assoc res k (f rv v)) (assoc res k v)))
               m1 m2))
            ([f m1 m2 & maps]
             (reduce #(mymerge-with f %1 %2) m1 (cons m2 maps)))
            )]
    mymerge-with))

(defcheck solution-9791b54
  (fn [f m & ms]
    (reduce
      (fn [ret x]
        (reduce
          (fn [r k]
            (conj r (if (r k) [k (f (r k) (x k))] (find x k))))
          ret (keys x)))
      (cons m ms))))

(defcheck solution-97a82c0
  (fn [f & maps]
    (loop [[m & cdr] maps ret {}]
      (if (nil? (seq m))
        ret
        (recur cdr (reduce (fn [ret cur]
                             (if (contains? ret (first cur))
                               (update-in ret [(first cur)] f (second cur))
                               (assoc ret (first cur) (second cur)))) ret m) )))))

(defcheck solution-9821b566
  (fn [f & m]
    (reduce
      (fn[a [b c]]
        (assoc a b
                 (if (a b) (f (a b) c) c)))
      {} (apply concat m))))

(defcheck solution-983d5f8c
  (fn mrg
    ([func m] m)
    ([func m n]
     (into m (map #(if (m %) {% (func (m %) (n %))} {% (n %)} ) (keys n)))
     )
    ([func m n & more]
     (apply mrg func (mrg func m n) more)
     )
    ))

(defcheck solution-9859214a
  (fn [f & maps]
    (reduce (fn [m1 m2]
              (reduce #(if-let [v1 (% %2)]
                         (assoc % %2 (f v1 (m2 %2)))
                         (assoc % %2 (m2 %2))) m1 (keys m2))) maps)))

(defcheck solution-98b97d77
  (fn  ff [f bs & r]
    (if (empty? r)
      bs
      (apply ff f (loop [ acc bs ks (keys (first r))  vs (vals (first r)) ]
                    (if (empty? ks)
                      acc
                      (recur (assoc acc (first ks) (if (acc (first ks))   (f   (acc (first ks))  (first vs) )     (first vs)   ) )  (rest ks) (rest vs) )


                      )
                    )
        (rest r)


        )
      )
    ))

(defcheck solution-98c2d4d1
  (fn [f & maps]
    (reduce
      (fn [map1 map2]
        (reduce
          (fn [acc [k v]]
            (if (contains? acc k)
              (update-in acc [k] f v)
              (assoc acc k v)))
          map1
          map2))
      maps)))

(defcheck solution-990fdcb7
  (fn [f m & ms]
    (reduce (fn [m n]
              (into m (for [[k v] n]
                        [k (if (contains? m k)
                             (f (m k) v)
                             v)])))
      m ms)))

(defcheck solution-996c24f2
  (fn my-merge-with [f & maps]
    (loop [current (first maps) remains (rest maps)]
      (if (empty? remains)
        current
        (let [nxt (first remains)]
          (recur
            (loop [input nxt result current]
              (let [x (first input)]
                (if (empty? x)
                  result
                  (recur
                    (rest input)
                    (if (get result (first x))
                      (update-in result [(first x)] #(f % (last x)))
                      (conj result x))))))
            (rest remains)))))))

(defcheck solution-9a549489
  (fn ! [f & maps]
    (loop [[h & t] maps]
      (if (empty? t) h
                     (recur (cons (into {}
                                    (for [e1 (distinct (concat (keys h) (keys (first t))))
                                          :let [m2 (first t)]]
                                      (cond
                                        (and (contains? m2 e1) (contains? h e1)) [e1 (f (get h e1) (get m2 e1))]
                                        (contains? m2 e1) [e1 (get m2 e1)]
                                        :else [e1 (get h e1)]))) (rest t)))))))

(defcheck solution-9aa25d43
  (fn [f & d]
    (reduce
      (fn [a m]
        (reduce
          (fn [b [k v]]
            (if-let [o (b k)]
              (assoc b k (f o v))
              (assoc b k v)))
          a m))
      {} d)))

(defcheck solution-9ae8c5ec
  (fn merge-w [f ini & maps]
    (if (empty? maps)
      ini
      (let [m (first maps)]
        (loop [kvs (seq m) ans ini]
          (if (empty? kvs)
            (apply merge-w f ans (rest maps))
            (let [[kv & remain] kvs]
              (if (contains? ans (first kv))
                (recur remain (update-in ans [(first kv)] f (second kv)))
                (recur remain (conj ans kv))))))))))

(defcheck solution-9b12bb49
  (fn [f & ms] (let [g (fn [m1 m2]
                         (let [[k v2] (first m2),
                               v1 (m1 k),
                               v (if (nil? v1) v2 (f v1 v2))]
                           (if (empty? m2) m1
                                           (recur (assoc m1 k v) (rest m2)))) )]
                 (reduce g ms))))

(defcheck solution-9b2f0df5
  (fn [f & lst]
    (reduce (fn [r m]
              (reduce (fn [r2 [k v]]
                        (if (r2 k)
                          (assoc r2 k (f (r2 k) v))
                          (assoc r2 k v)))
                r
                m))
      (first lst)
      (rest lst))))

(defcheck solution-9b7967c9
  (fn [f m & ms]
    (reduce
      (fn [acc-map x]
        (reduce (fn [acc [k v]]
                  (update-in acc [k]
                    (fn [old-v]
                      (if old-v (f old-v v) v))))
          acc-map x))
      m ms)))

(defcheck solution-9b976656
  (fn [f & args]
    (->> (mapcat identity args)
      (into [])
      (group-by first)
      (map (fn [[k v]]
             [k (reduce f (map second v))]))
      (into {}))))

(defcheck solution-9ba16f0b
  (fn [f m1 & ms]
    (let [m2 (reduce merge ms)]
      (loop [km (keys m2)
             m1 m1]
        (if (empty? km)
          m1
          (recur (rest km)
            (let [k (first km)]
              (if (get m1 k)
                (assoc m1 k (f (get m1 k) (get m2 k)))
                (assoc m1 k (get m2 k))))))))))

(defcheck solution-9c0296c2
  (fn [f & ms]
    (reduce
      (fn [m [k v]]
        (assoc m k
                 (if (contains? m k)
                   (f (get m k) v)
                   v)))
      {}
      (mapcat seq ms))))

(defcheck solution-9c303dbb
  (fn [f & maps]
    (reduce (fn [acc e]
              (into acc (for [[k v] e]
                          (if (contains? acc k)
                            [k (f (get acc k) v)]
                            [k v]))))
      maps)))

(defcheck solution-9cbee04f
  (fn [f & maps]
    (loop [acc (first maps) ms (rest maps)]
      (if (empty? ms)
        acc
        (recur (reduce (fn [m [k v]]
                         (if (contains? m k)
                           (assoc m k (f (m k) v))
                           (conj m [k v]))) acc (first ms)) (rest ms))))))

(defcheck solution-9d42cd30
  (fn [f d & m]
    (reduce
      (fn [o i] (assoc o (i 0) (if (nil? (o (i 0))) (i 1) (f (o (i 0)) (i 1)))))
      d (apply merge m))))

(defcheck solution-9d6f867
  (fn [f m1 & m]
    (apply merge (map #(if (find m1 (key %))
                         (assoc {} (key %) (f (val (find m1 (key %))) (val %) ))
                         (assoc {} (key %) (val %)))
                   (apply merge m)))))

(defcheck solution-9d95bb52
  (fn [f & m]
    (into {}
      (for [[k _] (apply merge m)]
        [k (#(if (second %)
               (apply f %)
               (first %))
            (filter identity (map #(% k) m)))]))))

(defcheck solution-9dd65b91
  (fn [f & vs]
    (->>
      vs
      (apply concat)
      (group-by first)
      (map (fn [[a b]] (vector a (reduce f (map second b)))) )
      (into {}))))

(defcheck solution-9de58036
  (fn myMergeWith
    [fun & colls]
    (let [result (first colls) toProcess (rest colls)]
      (reduce
        (fn [result other]
          (reduce
            #(let [element (get %1 (first %2))]
               (if (nil? element)
                 (assoc %1 (first %2) (second %2))
                 (assoc %1 (first %2) (fun element (second %2)))))
            result other))
        result toProcess))))

(defcheck solution-9e91c213
  (fn [f & maps]
    (->> maps
      (mapcat vec)
      (group-by first)
      (map (fn [[k v]] [k (reduce f (map second v))]))
      (into {}))))

(defcheck solution-9eaa5497
  (fn mergeXB [op & x]


    ((fn mergeV [op x]



       (if (= 1 (count x)) (first x)
                           ((fn [op x y]


                              (let [ks (distinct (concat (keys x) (keys y)))]
                                (apply merge (map (fn [k] (if (and (contains? x k)(contains? y k))
                                                            { k (op (get x k) (get y k) )}
                                                            (if (contains? x k)
                                                              {k (get x k)  }
                                                              {k (get y k)  }
                                                              )
                                                            )
                                                    )  ks))
                                )) op (first x)(mergeV op (rest x)))
                           )) op x)


    ))

(defcheck solution-9fa34d9b
  (fn my-merge-with [f & maps]
    (into {} (map (fn [[keyw vec-vec]]
                    (if (= 1 (count  vec-vec))
                      [keyw (second (first vec-vec))]
                      [keyw (reduce #(f (second %1) (second %2)) vec-vec)]))
               (group-by #(first %) (apply concat maps))))))

(defcheck solution-9ff30ccd
  (fn r [f & k]
    (into {}
      (map #(vector (first %)
              (let [[x & xs :as s](map val (second %))]
                (if xs (apply f s) x)))
        (group-by key (apply concat k))))))

(defcheck solution-a01ea7fd
  (fn [f & maps]
    (loop [maps-left maps, result {}]
      (if-let [[m & ms] (seq maps-left)]
        (recur
          ms
          (reduce
            (fn [m [k v2]]
              (if-let [v1 (get m k)]
                (assoc m k (f v1 v2))
                (assoc m k v2)))
            result
            (seq m)))
        result))))

(defcheck solution-a0b9619b
  (fn [op & maps]
    (letfn [(f [op, m, o]
              (let [km (set (keys m))
                    ko (set (keys o))
                    common-keys (clojure.set/intersection km ko)
                    unique-m (clojure.set/difference km common-keys)
                    unique-o (clojure.set/difference ko common-keys)]
                (merge (into {} (for [k common-keys]
                                  [k (op (m k) (o k))]))
                  (into {} (for [k unique-m] [k (m k)]))
                  (into {} (for [k unique-o] [k (o k)])))))]
      (reduce (partial f op) maps))))

(defcheck solution-a0bed81b
  (fn [f & maps]
    (reduce (fn [acc m]
              (reduce-kv
                (fn [m k v]
                  (assoc m
                    k (if-let [e (get acc k)]
                        (f e v)
                        v)))
                acc m))
      (first maps)
      (rest maps))))

(defcheck solution-a0d06daa
  (fn [f & x] ((comp #(zipmap (keys %) (map (comp (partial reduce f) (partial map last)) (vals %))) (partial group-by first) (partial apply concat)) x)))

(defcheck solution-a0d1ea92
  (fn [f & xs]
    (into {} (let [ks (reduce (fn [acc x] (into acc (keys x))) #{} xs)
                   go (fn [k]
                        (let [ans (reduce (fn [acc x] (if-let [v (x k)]
                                                        (conj acc v)
                                                        acc)) [] xs)]
                          (if (= 1 (count ans))
                            (first ans)
                            (apply f ans))))]
               (map (fn [x] [x (go x)]) ks)))))

(defcheck solution-a0f2650f
  (fn mer [f & ms]
    (letfn [(merge-two [m1 m2]
              (loop [pairs (seq m2)
                     m m1]
                (if-not (seq pairs)
                  m
                  (let [[k vlat] (first pairs)]
                    (recur (rest pairs)
                      (assoc m k (if-let [vres (m k)]
                                   (f vres vlat)
                                   vlat)))))))]

      (reduce merge-two ms))))

(defcheck solution-a1a942f2
  (fn [f & maps]
    (reduce #(assoc %1 (key %2)
                       (if (> (count (val %2)) 1)
                         (apply f (reverse (val %2)))
                         (first (val %2))))
      {}
      (reduce
        (fn [in-map new-map]
          (reduce #(let [[k v] %2]
                     (assoc %1 k (conj (get %1 k) v)))
            in-map
            new-map))
        {} maps))))

(defcheck solution-a1c215f3
  (fn [f & m]
    (loop [m m
           r {}]
      (if (empty? m)
        r
        (recur (rest m)
          (loop [m (first m)
                 r r]
            (if (empty? m)
              r
              (let [k (first (keys m))]
                (recur (dissoc m k)
                  (assoc r k
                           (if (r k)
                             (f (r k) (m k))
                             (m k))))))))))))

(defcheck solution-a1dd402d
  (fn my-merge-with [ f m & ee ]
    (if (empty? ee)
      m
      (recur f
        (reduce
          (fn[m [k kv]]
            (assoc m k (if (m k) (f (m k) kv) kv)))
          m (first ee))
        (rest ee)))))

(defcheck solution-a2171728
  (fn [f & maps]
    (let [merge-wth (fn [f to from] (reduce #(if (get %1 (key %2))
                                               (assoc %1 (key %2) (f (get %1 (key %2)) (val %2)))
                                               (assoc %1 (key %2) (val %2))) to from))]
      (reduce #(merge-wth f %1 %2) {} maps))))

(defcheck solution-a24165d6
  (fn mwf [f & m]
    (reduce
      (fn do-stuff [m1 m2]
        (into {}
          (map
            (fn mw [k] (if 	(and (get m1 k) (get m2 k))
                         [k (f (get m1 k) (get m2 k))]
                         (if (get m1 k)
                           [k (get m1 k)]
                           [k (get m2 k)])))
            (apply conj (keys m2) (keys m1)))))
      m)))

(defcheck solution-a2a614c2
  (fn [o & h]
    (reduce into
      (for [k (keys (reduce into h))]
        {k (#(if (next %)
               (apply o %)
               (first %))
            (filter (complement nil?)
              (map #(get % k) h)))}))))

(defcheck solution-a2cf72cc
  (fn [f & maps] (reduce #(assoc %
                            (first %2)
                            (if (contains? % (first %2))
                              (f (get % (first %2))
                                (second %2))
                              (second %2)))
                   (first maps)
                   (apply concat (rest maps)))))

(defcheck solution-a3584a53
  (fn [f & m] (into {} (for [[k e] (group-by key (apply concat m))] [k (reduce f (map val e))]))))

(defcheck solution-a44faa02
  (fn [f & maps]
    (reduce
      (fn [a b]
        (reduce
          (fn [x [k v]]
            (assoc x k (if (b k) (f v (b k)) v)))
          b a))
      (first maps) (rest maps))))

(defcheck solution-a47aa122
  (fn [f m & ms]
    (reduce (fn [m1 m2]
              (reduce (fn [m [k v]]
                        (conj m [k (if (contains? m k) (f (m k) v) v)]))
                m1 m2))
      m ms)))

(defcheck solution-a482c229
  (fn [f & maps]
    (reduce
      #(reduce
         (fn [m [k v]] (assoc m k (if-let [v1 (get m k)] (f v1 v) v)))
         %1 %2)
      {} maps)))

(defcheck solution-a4d1d22
  (fn mw [f & maps]
    (reduce (fn [map entry] (update-in map
                              [(key entry)]
                              (fn [v] (if (nil? v) (val entry) (f v (val entry))))))
      {}
      (apply concat maps))))

(defcheck solution-a4da57cb
  (fn [f m & xs ]
    ((fn mmw [f m xs]
       (letfn [(mm2 [f m1 m2]
                 (if (seq m2)
                   (let [x (first m2)]
                     (if (contains? m1 (key x))
                       (recur f (assoc m1 (key x) (f (m1 (key x)) (val x))) (rest m2))
                       (recur f (assoc m1 (key x) (val x)) (rest m2))))
                   m1)
                 )]
         (if (seq xs)
           (mmw f (mm2 f m (first xs)) (rest xs))
           m))) f m xs)))

(defcheck solution-a4eccfb8
  #(reduce (partial reduce (fn [s [x y]]
                             (conj s [x (if (s x) (% (s x) y) y)]))) %&))

(defcheck solution-a559c79c
  (fn [fcn x & xs ]
    (reduce
      (fn [result [k v]]
        (assoc result k
                      (if-let
                       [v1 (result k)]
                        (fcn v1 v)
                        v
                        )))
      x
      (apply concat xs)
      )
    ))

(defcheck solution-a56be0fd
  (fn [f & args]
    (reduce (fn [m v]
              (conj m [(first v)
                       (if (= 1 (count (second v)))
                         (first (second v))
                         (apply f (reverse (second v))))])) {}
      (reduce
        (fn [a b]
          (reduce #(update-in %1 [(first %2)] into (rest %2)) a b))
        {} args))))

(defcheck solution-a57f731d
  (fn [f ma & ms] (
                    let [mb (apply merge ms)] (
                                                merge mb (into {} (for [[k a] ma] [k (f a (get mb k)) ]))
                                                )
                                              )))

(defcheck solution-a5c58840
  (fn mw [f & maps]
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) m2))]
      (reduce merge2 maps))))

(defcheck solution-a5fac749
  (fn [g d & a] (reduce #(update-in % [(key %2)] (if (% (key %2)) (fn [v] (g v (val %2))) (fn [x] (val %2)))) d (mapcat vec a))))

(defcheck solution-a6790b5b
  (letfn [(kill [k dicts]
            (cons ;;destructuring would probably be more elegant
              (dissoc (first dicts) k)
              (rest dicts))) ]

    (fn [f d & dicts]
      (if-let [insert (ffirst dicts)]
        (let [k (key insert) v (val insert)]
          (if-let [old-val (d k)] (recur f (assoc d k (f old-val v)) (kill k dicts))
                                  (recur f (assoc d k v) (kill k dicts))))
        (if (empty? dicts) d
                           (recur f d (rest dicts)))))))

(defcheck solution-a6870fa4
  (fn my-merge-with
    ([f f-map s-map]
     (let [result-one (into {} (for [[k v] f-map]
                                 (if (s-map k)
                                   [k (f v (s-map k))]
                                   [k v]
                                   )
                                 )
                        )
           result (into result-one
                    (for [[k v] s-map]
                      (when (not (contains? f-map k))
                        [k v]
                        )
                      )
                    )

           ]
       result
       )
     )
    ([f f-map s-map & more]
     (apply (partial my-merge-with f (my-merge-with f f-map s-map) ) more)
     )
    ))

(defcheck solution-a7022136
  (fn mw [f result & [l & more-latters]]
    (if l
      (apply mw f
        (reduce
          (fn [r [k v]] (let [v' (get r k)]
                          (assoc r k (if v' (f v' v) v))))
          result
          l)
        more-latters)
      result)))

(defcheck solution-a770fa4b
  (fn [f p & args]
    (apply merge
      (flatten
        (for [a args]
          (for [k (keys a)]
            (if (nil? (find p k)) (assoc {} k (get a k))
                                  (assoc {} k (f (get p k) (get a k))))))))))

(defcheck solution-a7a5d7f7
  (fn merge-with-function [f m & ms]
    (let [tm (transient m)]
      (doseq [x ms]
        (doseq [[a b] x]
          (assoc! tm a (if (tm a) (f (tm a) b) b))))
      (persistent! tm))))

(defcheck solution-a7b4f801
  (fn [f & xs]
    (let [k ( (comp set flatten) (map keys xs))]
      (zipmap k (for [i k]
                  (let [x (remove nil? (for [e xs] (get e i)))]
                    (if (> (count x) 1) (apply f x) (first x) )))))))

(defcheck solution-a7c10b71
  (fn [f & [h & rest]] (reduce #(reduce (fn [acc [k v]] (if (contains? acc k) (assoc acc k (f (acc k) v)) (assoc acc k v) )) % %2) h rest)))

(defcheck solution-a7c31727
  (fn my-merge-with [f acc & maps]
    (loop [f f
           acc acc
           maps maps]
      (if (empty? maps)
        acc
        (recur f
          (reduce #(let [k (first %2)
                         v (second %2)
                         orig_v (%1 k)]
                     (assoc %1
                       k
                       (if (nil? orig_v) v (f orig_v v))))
            acc
            (first maps))
          (rest maps))))))

(defcheck solution-a8d24655
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce (fn [accum k]
                  (if (find accum k)
                    (assoc accum k (f (get accum k) (get m2 k)))
                    (assoc accum k (get m2 k))))
          m1
          (keys m2)))
      {}
      maps)))

(defcheck solution-a8d98180
  (fn [f & ms]
    (letfn [(mme [m e]
              (let [k (key e)
                    v (val e)]
                (if-let [mv (m k)]
                  (assoc m k (f mv v))
                  (assoc m k v))))
            (mm [m1 m2]
              (reduce mme m1 (seq m2)))]
      (reduce mm ms))))

(defcheck solution-a8e705bb
  (fn merge-with'
    ([f m1 m2]
     (reduce #(let [k (key %2)]
                (if-let [v1 (%1 k)]
                  (conj %1 [k (f v1 (val %2))])
                  (conj %1 %2)))
       m1
       m2))
    ([f m1 m2 & more] (apply merge-with' f (merge-with' f m1 m2) more))))

(defcheck solution-a97188c1
  (fn [f & maps]
    (let [x (fn [m [k v]]
              (if (contains? m k)
                (assoc m k (f (m k) v))
                (assoc m k v)))]
      (reduce #(reduce x (or % {}) %2) maps))))

(defcheck solution-a9c49425
  (fn [f m1 & more]
    (letfn [(merge-map [m1 m2]
              (reduce
                #(merge %1
                   (if-let [v (get %1 (key %2))]
                     {(key %2) (f v (val %2))}
                     {(key %2) (val %2)}))
                m1
                m2))]
      (loop [m-coll more, result m1]
        (if (empty? m-coll)
          result
          (recur (rest m-coll) (merge-map result (first m-coll))))))))

(defcheck solution-a9dbed2b
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-aa4a4555
  (fn
    [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          #(let [m %
                 k (first %2)
                 v (second %2)]
             (if (contains? m k)
               (assoc m k (f (get m k) v))
               (assoc m k v)))
          m1
          m2))
      maps)))

(defcheck solution-aa6258b7
  (fn [f & ms]
    (reduce (fn [o m]
              (reduce #(if (contains? % (key %2))
                         (assoc % (key %2) (f (% (key %2)) (val %2)))
                         (conj % %2)) o m))
      {} ms)))

(defcheck solution-aab74394
  (fn ks [f & m] (apply merge (map (fn [y]  {(first y) (reduce f (second y))}) (for [item (set (reduce #(into (keys %2) %1) '() m))]
                                                                                 [item (filter (complement nil?) (map #(get %1 item) m))])))))

(defcheck solution-aac090c
  (fn [f first-map & other-maps]
    (if (empty? other-maps)
      first-map
      (recur f
        (reduce (fn [merged [key value]]
                  (assoc merged
                    key
                    (if (contains? merged key)
                      (f (merged key) value)
                      value)))
          first-map
          (first other-maps))
        (rest other-maps)))))

(defcheck solution-aaf168de
  (fn my-merge-with
    ([f] {})
    ([f res-map] res-map)
    ([f res-map map-1]
     (reduce (fn [init-map [k v]]
               (if (contains? init-map k)
                 (assoc
                  init-map
                   k
                   (f (get init-map k) v))
                 (assoc init-map k v)))
       res-map
       map-1))
    ([f res-map map-1 & more-maps]
     (apply my-merge-with f
       (my-merge-with f res-map map-1)
       more-maps))))

(defcheck solution-ab13662b
  (fn [f & ms]
    (reduce #(assoc %1 (first %2) (reduce f (map second (second %2)))) {}
      (group-by first (apply concat (map seq ms))))))

(defcheck solution-ab3fff95
  (fn do-it [f & maps]
    (letfn
     [(merger [result a-map] (reduce (fn [acc k] (conj acc {k (if (result k) (f (result k) (a-map k)) (a-map k))}))
                               result
                               (keys a-map)))]
      (reduce merger maps))))

(defcheck solution-ab62163c
  (fn merg
    [f a & bs]
    (if (= 0 (count bs))
      a
      (let [[b & rest] bs]
        (recur
          f
          (into (hash-map)
            (concat
             (for [[k v] a
                   :when (not (contains? b k))]
               [k v])
             (for [[k v] b]
               [k
                (if (contains? a k)
                  (f (get a k) v)
                  v)])))
          rest)))))

(defcheck solution-ab7946e1
  (fn mrgw [f & maps]
    (letfn [(mrgw2 [m1 m2]
              (letfn [(merge-value [k v] (if (contains? m1 k) (f (m1 k) v) v))]
                (into m1 (for [[k v] m2] [k (merge-value k v)]))))]
      (reduce mrgw2 maps))))

(defcheck solution-abe2a48e
  (fn [f m & ms]
    (reduce (fn [res m]
              (reduce (fn [res e]
                        (if (res (key e))
                          (update-in res [(key e)] f (val e))
                          (conj res e)))
                res
                m))
      m
      ms)))

(defcheck solution-ac0dae2
  #(into {} (for [[k s] (group-by key (apply concat %&))]
              [k (reduce % (vals s))])))

(defcheck solution-ac3aa0ef
  (fn foo-merge-with [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-ac436071
  (fn merge-maps [f original & args]
    (let [merge-two-maps (fn [original additions]
                           (reduce
                             (fn [m [k v]]
                               (if-let [e (m k)]
                                 (assoc m k (f e v))
                                 (assoc m k v)))
                             original
                             additions))]
      (reduce merge-two-maps original args))))

(defcheck solution-ac9505be
  (fn m-w
    [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-acbaadc9
  (fn [m & args]
    (loop [a (first args)
           b (reduce conj (rest args))]
      (if (empty? b)
        a
        (let [fk (first (keys b))
              v (get b fk)
              r (assoc a fk (if (contains? a fk)
                              (m (get a fk) v)
                              v))]
          (recur r (dissoc b fk)))))))

(defcheck solution-ace2f6a4
  (fn [f head & tail]
    (reduce (fn [acc it]
              (apply
                (partial merge acc it)
                (for [[k1 v1] acc
                      [k2 v2] it
                      :when (= k1 k2)
                      :let [z (f v1 v2)]]
                  {k2 z})))
      head tail)))

(defcheck solution-ad37aca7
  (fn [f & s]
    (reduce (fn [m [k v]]
              (assoc m k (if (m k) (f (m k) v) v)))
      {} (mapcat seq s))))

(defcheck solution-ad527d54
  (fn mwaf [f init-m & ms]
    (if (seq ms)
      (apply mwaf
        f
        (reduce (fn [acc [k v]]
                  (if (contains? acc k)
                    (assoc acc k (f (acc k) v))
                    (assoc acc k v)))
          init-m
          (first ms))
        (rest ms))
      init-m)))

(defcheck solution-ad5facb6
  (fn
    [f & maps]
    (letfn  [(merge-entry [m e]
               (let [k (key e) v (val e)]
                 (if (contains? m k)
                   (assoc m k (f (get m k) v))
                   (assoc m k v))))
             (merge2 [m1 m2]
               (reduce merge-entry (or m1 {}) m2))]
      (reduce merge2 maps))))

(defcheck solution-ad98569b
  (fn my-merge [f & maps]
    (letfn [(my-key-merge [f m1 m2 key]
              (if (and (find m1 key) (find m2 key))
                (f (m1 key) (m2 key))
                (if (find m1 key)
                  (m1 key)
                  (m2 key))))
            (my-map-merge [f map1 map2]
              (reduce (fn [a b]
                        (if (contains? a (first b))
                          (assoc a (first b) (my-key-merge f a (hash-map (first b) (second b)) (first b)))
                          (conj a b)))
                map1 map2))]
      (reduce (fn [a1 b1] (my-map-merge f a1 b1)) maps))))

(defcheck solution-adae5349
  (fn [f & args]
    (reduce (fn[map1 map2]
              (reduce (fn [m [k v]]
                        (if-let [vv (m k)]
                          (assoc m k (f vv v))
                          (assoc m k v)))
                map1 map2))
      args)))

(defcheck solution-ae138f51
  (fn [f & ms]
    (let [all-together (mapcat seq ms)
          remapped (group-by first all-together)]
      (reduce (fn [m [k vals]]
                (let [vals (map second vals)]
                  (assoc m k (if (< 1 (count vals))
                               (apply f vals)
                               (apply identity vals)))))
        {} remapped))))

(defcheck solution-ae63e953
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-aebff33d
  (fn [m & c]
    (reduce
      (fn [a b]
        (reduce
          (fn [ai [k v]]
            (assoc ai k (if-let [vi (ai k)]
                          (m vi v)
                          v)))
          a b))
      c)))

(defcheck solution-aef14e2f
  (fn merge-with* [f m & ms]
    (if (empty? ms)
      m
      (let [c (fn [acc kv]
                (let [k (key kv), v (val kv)]
                  (if (contains? acc k)
                    (assoc acc k (f (get acc k) v))
                    (conj acc kv))))]
        (reduce #(reduce c %1 %2) m ms)))))

(defcheck solution-af4ff1f7
  (fn [f & ms]
    (letfn [(merge2 [f l r]
              (reduce
                (fn [m [k v]] (if (contains? m k) (assoc m k (f (get m k) v)) (merge m {k v})))
                l (seq r)))]
      (reduce #(merge2 f %1 %2) ms))))

(defcheck solution-af8f6d6
  (fn mf [f & maps]
    (let [ks (distinct (flatten (map keys maps)))]
      (reduce (fn [memo key]
                (let [args (filterv identity (map #(% key) maps))]
                  (if (= 1 (count args))
                    (assoc memo key (first args))
                    (assoc memo key (apply f args)))
                  )
                )
        {}
        ks))))

(defcheck solution-afe35d86
  (fn [f & m]
    (into {}
      (map (fn [[k [& v]]]
             (let [[v1 & v2] (map second v)]
               [k (reduce f v1 v2)]))
        (group-by first (mapcat #(map identity %) m))))))

(defcheck solution-b07ca637
  (fn
    [f & maps]
    (into
      {}
      (for
       [[k vs]
        (reduce
          (fn [m s]
            (reduce
              (fn [ma itm]
                (assoc
                 ma
                  (first itm)
                  (concat (ma (first itm)) [(second itm)])))
              m s))
          {} (map (partial into []) maps))]
        [k (reduce f vs)]))))

(defcheck solution-b17e2b6a
  (fn mw [f m1 & mn]
    (letfn
     [(merge2 [a b]
        (reduce
          (fn [x [k v]]
            (assoc x k
                     (if (x k) (f (x k) v) v)))
          a b))]
      (reduce merge2 m1 mn))))

(defcheck solution-b1b852f4
  (fn my-merge-with
    [f & args]
    (loop [acc (first args)
           rem (rest args)]
      ; if we have no more maps to merge, return
      ; our accumulated map
      (if (empty? rem)
        acc

        ; otherwise, merge first from rem
        ; with our current acc and recur
        (recur
          ; this loop should generate our merge
          (loop [curr-acc acc
                 curr-merge (first rem)
                 ks (keys curr-merge)]
            (if (empty? ks)
              ; return if we're out of keys
              curr-acc
              (recur
                (if (contains? curr-acc (first ks))
                  (conj curr-acc {(first ks) (f (get curr-acc (first ks)) (get curr-merge (first ks)))})
                  (conj curr-acc {(first ks) (get curr-merge (first ks))}))
                curr-merge
                (rest ks))))
          (rest rem))))))

(defcheck solution-b266d2e
  (fn mymerge
    ([f col1 col2]
     (reduce (fn [col ele]
               (let [k (first ele)
                     v (second ele)]
                 (if (get col k)
                   (assoc-in col [k] (f (get col k) v))
                   (assoc-in col [k] v))
                 )
               ) col1 col2))
    ([f col1 col2 & more]
     (apply mymerge f (mymerge f col1 col2) more))))

(defcheck solution-b2836c2e
  (fn [f m & mps]
    (reduce
      #(reduce
         (fn [m kv]
           (let [k (key kv)
                 v (val kv)
                 mv (get m k)]
             (assoc m k
                      (if mv
                        (f mv v)
                        v)))) % %2) m mps)))

(defcheck solution-b2c6b8a2
  (fn merge-maps-with
    [func & [head secnd & tail]]
    (letfn [(merge-two
              [func first-map next-map]
              (reduce
                (fn [acc entry]
                  (if-let [found-val (get acc (first entry))]
                    (assoc acc (first entry) (func found-val (second entry) ))
                    (conj acc entry)))
                first-map next-map))]
      (if (nil? secnd)
        head
        (recur func (cons (merge-two func head secnd) tail))))))

(defcheck solution-b2dcc7dd
  (fn [f res & ss]
    (reduce
      (fn [res [k v]]
        (if (contains? res k)
          (assoc res k (f (res k) v))
          (assoc res k v)))
      res
      (for [s ss, i s] i))))

(defcheck solution-b2f7e9ba
  (fn [f x & args]
    (letfn [(mergeW [f x n]
              (reduce (fn [o [k v]]
                        (if-let [val (get o k)] (assoc o k (f val v)) (assoc o k v) )) x n))]
      (reduce #(mergeW f % %2) x args))))

(defcheck solution-b3d83762
  (fn my-merge-with [f m1 & maps]
    (if (empty? maps)
      m1
      (let [m2 (first maps)
            mg1 (merge m1 m2)
            mg (reduce #(if-let [val2 (get m2 %2)]
                          (assoc %1 %2 (f (get m1 %2) val2))
                          %1)
                 mg1 (keys m1))]
        (recur f mg (next maps))))))

(defcheck solution-b3db593f
  (fn [f & colls]
    (reduce (fn [m [k v]]
              (if (m k)
                (update-in m [k] f v)
                (assoc m k v)))
      {}
      (mapcat identity colls))))

(defcheck solution-b4950c70
  (fn [func & params]
    (let [grouped (group-by #(key %) (reduce #(concat %1 %2) params))]
      (zipmap (keys grouped) (map #(reduce func (map last %)) (vals grouped))))))

(defcheck solution-b4f6d780
  (fn merge-with' [f & ms]
    (let [
          merge-kv (fn [m kv]
                     (let [[k v] kv]
                       (if (contains? m k)
                         (assoc m k (f (get m k) v))
                         (assoc m k v))))
          merge-both (fn [m1 m2] (reduce merge-kv m1 (seq m2)))]
      (reduce merge-both ms))))

(defcheck solution-b540e26e
  (let [a apply f first m map p partial
        r reduce i (p r into) s second]
    #(->> (m vec %&)
       i i
       (partition 2)
       (group-by f)
       (m (juxt f (comp (p r %) (p m s) s)))
       (a m vector)
       (a zipmap))))

(defcheck solution-b5807350
  (fn [predicate & args]
    (reduce (fn [m   [k v]]
              (let [p   (get m k)
                    n   (if (nil? p)
                          v
                          (predicate p v))]
                (assoc m k n)))
      {} (mapcat concat args))))

(defcheck solution-b6048cd5
  (fn
    [f m & ms]
    (reduce
      (fn [m1 m2]
        (merge
          m1
          (apply hash-map
            (mapcat
              (fn [k]
                [
                 k
                 (if
                  (contains? m1 k)
                   (f
                     (get m1 k)
                     (get m2 k)
                     )
                   (get m2 k)
                   )
                 ]
                )
              (keys m2)
              )
            )
          )
        )
      m
      ms
      )
    ))

(defcheck solution-b65ec3c4
  (fn [f & ms]
    (into {}
      (map (fn [[k kvs]] [k (reduce f (vals kvs))])
        (group-by key
          (mapcat #(map identity %) ms))))))

(defcheck solution-b666d52f
  (fn my-merge-with
    [f & ms]
    (letfn [(merge-one [accum m]
              (loop [accum accum ks (keys m)]
                (if (empty? ks) accum
                                (let [k (first ks)]
                                  (if-let [v (get accum k)]
                                    (recur (assoc accum k (f v (m k))) (rest ks))
                                    (recur (assoc accum k (m k)) (rest ks)))))))]
      (reduce merge-one {} ms))))

(defcheck solution-b66c05b
  (fn [f m & ms]
    (loop [m m ms (seq ms)]
      (if-let [cm (first ms)]
        (recur (reduce (fn [m k] (assoc m k (if-not (get m k) (get cm k) (f (get m k) (get cm k))))) m (keys cm)) (next ms))
        m))))

(defcheck solution-b68de33
  (fn [f m & ms]
    #_(println f) #_(println m) #_(println ms)
    (letfn [(merge-maps-with [ml mr]
              (let [[lp & mr] (seq mr) k (first lp)]
                (if (nil? lp) ml
                              (if (ml k)
                                (recur (conj ml [k (f (ml k) (second lp))]) mr)
                                (recur (conj ml lp) mr)))))]
      (loop [ml m [mr & mt] ms]
        (if (nil? mr)
          ml
          (recur (merge-maps-with ml mr) mt))))))

(defcheck solution-b7b16992
  (fn aa [x yy & args]
    (loop [zz args y yy]
      (if (empty? zz) y

                      (recur (rest zz)

                        (reduce #(if (%1 (first %2)) (assoc %1 (first %2) (x (%1 (first %2)) (last %2)))
                                                     (assoc %1 (first %2) (last %2))
                                                     ) y (first zz)
                          )
                        )
                      )
      )
    ))

(defcheck solution-b7cbf69c
  (fn [f mp & mps]
    (reduce #(reduce (fn [mp ky]
                       (assoc mp ky (if-let [vl (mp ky)] (f vl (%2 ky)) (%2 ky))))
               %1
               (keys %2))
      mp
      mps)))

(defcheck solution-b7df166d
  (fn mw [f & [m & more]]
    (letfn [(mw-once [f m other]
              (apply (partial conj m)
                (for [k (keys other)]
                  (if (and (contains? m k)
                           (contains? other k))
                    [k (f (m k) (other k))]
                    [k (other k)]))))]
      (if (not (seq more))
        m
        (let [[hd & tl] more]
          (apply (partial mw f (mw-once f m hd)) tl))))))

(defcheck solution-b829e3bb
  (fn [f r & m]
    (if (= [] m)
      r
      (recur f ((fn [rr mm]
                  (if (= [] mm)
                    rr
                    (recur ((fn [rrr mmm]
                              (if (= [] mmm)
                                rrr
                                (if (contains? rrr (first mmm))
                                  (conj rrr [(first mmm) (f (rrr (first mmm)) (first (rest mmm)))])
                                  (conj rrr mmm)
                                  )))rr (first mm)) (rest mm))))r (first m)) (rest m)))))

(defcheck solution-b8f7aad7
  (fn [f m1 & mps] (letfn [(merg2 [g t1 t2]
                             (if (first t2)
                               (let [x1 (first t2) k (key x1) v (val x1)]
                                 (if (t1 k)
                                   (recur g (assoc t1 k (g (t1 k) v))  (rest t2) )
                                   (recur g (assoc t1 k v ) (rest t2) )
                                   )
                                 )
                               t1
                               )
                             )]

                     (if (first mps)
                       (recur f (merg2 f m1 (first mps)) (rest mps)  )
                       m1
                       )
                     )
    ))

(defcheck solution-b9076e06
  (fn [f x1 & xs]
    ((fn my-merge [f result rs]
       (let [ks (clojure.set/union (set (keys result)) (set (keys (first rs))))
             ks1 (set (keys result)) ks2 (set (keys (first rs)))
             r1 (reduce
                  #(assoc %1 %2
                             (cond
                               (and (contains? ks1 %2) (contains? ks2 %2))
                               (f (get result %2) (get (first rs) %2))
                               (and (contains? ks1 %2) (not (contains? ks2 %2)))
                               (get result %2)
                               (and (not (contains? ks1 %2)) (contains? ks2 %2))
                               (get (first rs) %2)
                               )
                             )
                  {} (vec ks)) ]
         (if (empty? (rest rs))
           r1
           (my-merge f r1 (rest rs) )
           ))
       ) f x1 xs)))

(defcheck solution-b90979ac
  (fn mw
    ([f m n]
     (loop [ks (keys n) acc m]
       (if (empty? ks)
         acc
         (let [fk (first ks) rk (rest ks)]
           (if (nil? (acc fk))
             (recur rk (conj acc {fk (n fk)}))
             (recur rk (conj acc {fk (f (acc fk) (n fk))})))))))
    ([f m n & ns]
     (apply mw f (mw f m n) ns))))

(defcheck solution-b946a424
  (fn [f & args]
    (letfn [(merge-args [acc v]
              (reduce #(if
                        (contains? %1 %2)
                         (assoc %1 %2 (f (get %1 %2) (get v %2)))
                         (assoc %1 %2 (get v %2)))
                acc (keys v)))]
      (reduce merge-args (first args) (rest args)))))

(defcheck solution-b964153a
  (fn [f & args] (->>
                   args
                   (apply concat)
                   (group-by key)
                   (map (fn [e]
                          (vector (key e)
                            (->> e val (map second)
                              (#(reduce f (first %) (rest %)))))))
                   (into {}))))

(defcheck solution-b98cf499
  (fn my-merge-with [f & args]
    (reduce (fn [map1 map2]
              (reduce (fn [m1 [k v]]
                        (if-let [v1 (m1 k)]
                          (assoc m1 k (f v1 v))
                          (assoc m1 k v)))
                map1 map2))
      args)))

(defcheck solution-ba2d07ba
  (fn merge-map [f & xs]
    (let [all-keys (into #{} (mapcat keys xs))]
      (into {}
        (map (fn [ak] (vector ak
                        (let [mvals (filter identity (map #(% ak) xs))]
                          (if (> (count mvals) 1)
                            (apply f mvals)
                            (first mvals)))))
          all-keys)))))

(defcheck solution-ba70d97
  (fn [f & xs] (reduce (fn [m n] (reduce (fn [m [k v]] (assoc m k (if-let [w (m k)] (f w v) v))) m n)) xs)))

(defcheck solution-ba9786b2
  (fn [f m1 & ms]
    (if (empty? ms)
      m1
      (recur
        f
        (reduce (fn [a [k v]]
                  (if (nil? (get a k))
                    (assoc a k v)
                    (assoc a k (f (get a k) v)))) m1 (first ms))
        (rest ms)))))

(defcheck solution-baa8bf1e
  (fn [f & ms]
    (reduce (fn [m [k v]]
              (assoc m k (if (contains? m k) (f (get m k) v) v)))
      {}
      (mapcat (partial into []) ms))))

(defcheck solution-bb067877
  (fn [o s & r]
    (reduce
      #(reduce
         (fn [s [k v]]
           (assoc
            s k
              (if (s k) (o (s k) v) v)))
         % %2)
      s r)))

(defcheck solution-bb17cf50
  (fn mrgwith [f initial-map & maps]
    (reduce (fn with [current-map next-map]
              (merge current-map
                (loop [new-map next-map add-map next-map]
                  (if (empty? add-map)
                    (into {}  new-map)
                    (let [k (key (first add-map))
                          v (add-map k)
                          present (current-map k)
                          new-val (if present (f present v ) v)]
                      (recur (conj new-map {k new-val}) (into {}  (rest add-map))))))))
      initial-map
      maps)))

(defcheck solution-bb5749c4
  (fn [f & ms] (into {} (map (fn [[k vs]] [k (reduce f (map second vs))]) (group-by first (apply concat ms))))))

(defcheck solution-bba4dbc6
  ; (defn g [m k v f] (assoc m k (if (contains? m k) (f (m k) v) v)))
  ; (defn kvs [m] (partition 2 (interleave (keys m) (vals m))))
  ; (defn h [f m & ms] (reduce #(g %1 (first %2) (second %2) f) m (apply concat (map kvs ms))))

  (fn [f m & ms] (reduce #((fn [m k v f] (assoc m k (if (contains? m k) (f (m k) v) v))) %1 (first %2) (second %2) f) m (apply concat (map (fn [m] (partition 2 (interleave (keys m) (vals m)))) ms)))))

(defcheck solution-bbc2ce74
  (fn [f & ms]
    (reduce (fn [acc n]
              (reduce
                (fn [r [k v]]
                  (assoc r k
                           (if (contains? r k)
                             (f (get r k) v)
                             v)))
                acc n))
      ms)))

(defcheck solution-bc4c4d
  (fn my-merge-with [op & args]
    (into {}
      ((fn [groups]
         (for [key (keys groups)]
           (let [grp (groups key)
                 vals (map last grp)]
             (if (= 1 (count vals))
               {key (first vals)}
               {key (apply op (map last grp))}))))
       (group-by first (mapcat #(into [] %) args))))))

(defcheck solution-bc4d51eb
  (fn
    [fnc & maps]
    (into {} (map #(vector (first %) (reduce fnc (map last (last %)))) (group-by first (apply concat maps))))))

(defcheck solution-bc6545f1
  (fn [f mr & ms]
    (let [_merge (fn _merge [f r m](loop [r r ks (keys m)](let [k (first ks)] (if (empty? ks) r(recur (merge r {k (if (contains? r k)(f (r k) (m k)) (m k))}) (rest ks))))))]
      (loop [r mr ms ms]
        (if (empty? ms) r
                        (recur (_merge f r (first ms)) (rest ms))
                        )
        ))
    ))

(defcheck solution-bc7b895
  (fn [f & ms]
    (loop [ms ms, res {}]
      (if (seq ms)
        (let [nres
              (reduce-kv (fn [acc k v1]
                           (if-let [v2 (acc k)]
                             (assoc acc k (f v2 v1))
                             (assoc acc k v1)))
                res (first ms))]
          (recur (rest ms) nres))
        res))))

(defcheck solution-bcc6ae6c
  (fn [f & maps]
    (into {} (map
               (fn [grp]
                 [(first grp) (apply (fn [& args] (apply (if (= 2 (count args)) f identity) args))
                                (mapcat #(map last %) (rest grp)))])
               (group-by first (apply concat  maps))))))

(defcheck solution-bcf80a2e
  (fn [f & vs]
    (into {}
      (map
        (fn [ky] (vector ky
                   (let [va (filter (comp not nil?) (map #(% ky) vs))]
                     (if (= 1 (count va))
                       (first va)
                       (apply f va
                         )))))
        (into '() (set (mapcat keys vs)))
        ))))

(defcheck solution-bd66c01d
  (fn [f & maps]
    (->> maps
      (apply concat)
      (group-by key)
      (map (fn [[k vs]] [k (reduce f (map val vs))]))
      (into {})
      )
    ))

(defcheck solution-bdaec90a
  (fn [f m & mps]
    (reduce (fn [a b]
              (reduce (fn [ma [k v]]
                        (if (contains? ma k)
                          (assoc ma k (f (get ma k) v))
                          (assoc ma k v))) a (seq b))) m mps)))

(defcheck solution-be5c64c9
  (fn sixty-nine-huehuehuehuehue
    [f & maps]
    (loop [m {}
           maps maps]
      (if (seq maps)
        (let [nextmap (first maps)]
          (recur (reduce (fn [m [k v]]
                           (if (contains? m k)
                             (assoc m k (f (m k) v))
                             (assoc m k v)))
                   m
                   nextmap)
            (next maps)))
        m))))

(defcheck solution-be6f4d48
  (fn [f & maps]
    (letfn [(x [a b]
              (let [[k v] b]
                (if (contains? a k)
                  (assoc a k (f (a k) v))
                  (assoc a k v))))]
      (reduce #(reduce x (or %1 {}) %2) maps))))

(defcheck solution-be712924
  (fn [f & ms]
    (reduce
      (fn [acc x]
        (reduce
          #(if (% (first %2))
             (assoc %
               (first %2)
               (f (% (first %2)) (second %2)))
             (assoc % (first %2) (second %2)))
          acc
          x))
      ms)))

(defcheck solution-be7f24fa
  (fn [f & maps]
    (reduce #(reduce
               (fn [out [k v]]
                 (if (contains? out k)
                   (assoc out k (f (out k) v))
                   (assoc out k v)
                   )
                 ) %1 %2
               ) {} maps
      )
    ))

(defcheck solution-bea5a5f1
  (fn my-merge-with [f & maps]
    (letfn [(my-merge-maps [m n]
              (letfn
               [(my-merge-key [m k]
                  (if (m k)
                    (assoc m k (f (m k) (n k)))
                    (assoc m k (n k))))]
                (reduce my-merge-key m (keys n))))]
      (reduce my-merge-maps maps))))

(defcheck solution-bea862e2
  (fn newmergewith [f & x]
    (let [fchecker (fn [dict entry]
                     (if (contains? dict (first entry))
                       (assoc dict (first entry) (f (get dict (first entry)) (peek entry)))
                       (assoc dict (first entry) (peek entry))))
          combine (fn [dict otherdict]
                    (reduce fchecker dict otherdict))]
      (if (seq x)
        (if (seq (rest x))
          (reduce combine (first x) (rest x))
          (first x))
        '()))))

(defcheck solution-bedaa9a2
  (fn g [f r & m] (if (empty? m) r (apply g f (reduce #(let [[k v] %2] (conj %1 (if (%1 k) {k (f (%1 k) v)} %2))) r (first m)) (rest m)))))

(defcheck solution-bf0a0e9
  (fn [f & ms]
    (into {}
      (for [[k vs] (group-by first (for [m ms [k v] m] [k [v]]))]
        [k (let [rs (mapcat second vs)]
             (if (= 1 (count rs)) (first rs) (apply f rs)))]))))

(defcheck solution-bf2beace
  (fn [f x & xs]
    (letfn [(merge-with-sub [init y]
              (update-in init (vector (first y)) (fn [k] (let [v (second y)] (if (nil? k) v (f k v))))))]
      (reduce #(if (seq? %2) (merge-with-sub %1 %2) (reduce merge-with-sub %1 %2)) x xs))))

(defcheck solution-bfe7cac
  (fn [f & maps]
    (let [mm2
          (fn [a b]
            (let [bm (merge a b)
                  ak (set (keys bm))
                  ck (set (keys (select-keys a (keys b))))
                  uk (clojure.set/difference ak ck)]
              (merge (select-keys bm uk) (zipmap ck (map #(f (a %) (b %)) ck)))))]
      (reduce mm2 maps))))

(defcheck solution-c0151a88
  (fn mw ([f ma mb]
          (reduce #(assoc-in %1 [(key %2)]
                     (if (contains? %1 (key %2)) (f (%1 (key %2)) (val %2)) (val %2)))
            ma mb))
    ([f ma mb & ms]
     (reduce (partial mw f) ma (cons mb ms)))))

(defcheck solution-c02d5abd
  (fn m
    ([f m1 m2 & ms]
     (if (seq ms)
       (apply m f  (m f m1 m2) ms)
       (m f m1 m2)))
    ([f m1 m2]
     (merge m2 (into {} (map (fn [[k v]] (let [o (m2 k)] (if o [k (f v o)] [k v]))) m1))))))

(defcheck solution-c09c0fad
  (fn mmw [f & ms]
    (if (< (count ms) 2)
      (first ms)
      (let [m1 (first ms)
            m2 (second ms)
            mr (drop 2 ms)
            nm (merge m1 (into {}
                           (for [[k v2] m2]
                             (let [v1 (get m1 k)]
                               (if (nil? v1)
                                 [k v2]
                                 [k (f v1 v2)])))))]
        (apply mmw f nm mr)))))

(defcheck solution-c14a21df
  (fn merge-with-
    ^{:doc "69. Write a function which takes a function f and a variable number of maps."}
    ([f] {})
    ([f m1] m1)
    ([f m1 m2] (conj m2 m1 (apply hash-map (mapcat (fn [k1] (if-let [v2 (get m2 k1)] [k1 (f (get m1 k1) v2)])) (keys m1)))))
    ([f m1 m2 & ms] (apply merge-with- f (merge-with- f m1 m2) ms))))

(defcheck solution-c157573a
  (fn [f & ms]
    (letfn[(m [acc m]
             (let [to-merge (select-keys m (keys acc))
                   to-assoc (apply disj (set (keys m)) to-merge)
                   merged   (map (fn [[k v]] [k (f (get acc k) v)]) to-merge)
                   assoced  (reduce #(assoc %1 %2 (get m %2)) acc to-assoc)]
               (into assoced merged)))]
      (reduce m ms))))

(defcheck solution-c1605936
  (fn [f & x]
    (reduce
      #(reduce (fn [m [k v]]
                 (assoc m k (if (m k) (f (m k) v) v)))
         %1 %2)
      x)))

(defcheck solution-c19bd6d2
  (fn [f & m]
    (into {}
      (for [[k _] (apply merge m)]
        [k (reduce f (keep #(% k) m))]))))

(defcheck solution-c2b92318
  (fn [f & vs]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [new-k new-v]]
            (let [v (m new-k)]
              (assoc m new-k (if v (f v new-v) new-v))))
          m1 (vec m2)))
      {} vs)))

(defcheck solution-c34516ed
  (fn my-merge-with [f & maps]
    (reduce
      (fn [rdc nxt]
        (reduce
          (fn [j [k v]]
            (if (contains? j k)
              (assoc j k (f (j k) v))
              (assoc j k v)))
          rdc
          nxt))
      {} maps)))

(defcheck solution-c369efa8
  (fn [f & maps]
    (let [ks (set (apply concat (map keys maps)))]
      (into {} (map (fn [k]
                      [k
                       (reduce f
                         (filter #(not (nil? %))
                           (map #(% k) maps)))])
                 ks)))))

(defcheck solution-c371f5a1
  (fn my-merge-with [f & maps]
    (reduce
      (fn [a b]
        (reduce
          (fn [x [k v]]
            (assoc x k (if (b k) (f v (b k)) v)))
          b a))
      (first maps) (rest maps))))

(defcheck solution-c456c834
  (fn [f & rest](reduce
                  (fn [m1 m2] (let [r (or m1 {}) s (seq m2)]
                                (reduce (fn [m [k v]] (if (contains? m k)
                                                        (assoc m k (f (m k) v)) (assoc m k v))) r s))) {} rest)))

(defcheck solution-c4893092
  (fn [f & maps]
    (let [input (apply concat maps)]
      (into {} (map (fn [x] [(first x) (reduce f (map second (second x)))]) (group-by key input))))))

(defcheck solution-c4f48b9e
  (fn
    [f & s]
    (reduce
      (fn
        [m n]
        (apply
          merge
          (cons n
            (map
              (fn
                [[k v]]
                {k (if (get n k)
                     (f v (get n k))
                     v)})
              m))))
      s)))

(defcheck solution-c503db5d
  (fn [f & maps]
    (apply merge
      (for [key (keys (apply merge maps))]
        (if (and
             (contains? (first maps) key)
             (contains? (apply merge (rest maps)) key)
             )
          {key (f (get (first maps) key) (get (apply merge (rest maps)) key))}
          {key (get (apply merge maps) key)}
          )
        )
      )
    ))

(defcheck solution-c57da530
  (fn [f m & maps]
    (let [mrg (fn [map1 map2]
                (reduce (fn [memo [k v]]
                          (if (memo k)
                            (assoc memo k (f (memo k) v))
                            (assoc memo k v))) map1 map2))]
      (reduce #(mrg %1 %2) m maps))))

(defcheck solution-c5bdaee
  (fn my-merge-with [ f m & ms ]
    (reduce
      (fn merge-this [to from]
        (if (empty? from)
          to
          (let [[k v] (first from)]
            (recur (assoc to k (if (contains? to k) (f (to k) v) v))
              (rest from)))))
      m ms)))

(defcheck solution-c651543e
  (fn [mf & ms]
    (reduce
      (fn [mp mn]
        (reduce
          (fn [m [k v]] (assoc m k (if (contains? m k) (mf (m k) v) v)))
          mp (map vector (keys mn) (vals mn))))
      (first ms) (rest ms))))

(defcheck solution-c657a5ad
  (fn my-merge-with
    ([f xs ys]
     (reduce (fn [acc pair]
               (let [k (key pair)
                     vlatter (val pair)
                     vres (acc k)]
                 (assoc acc k (if (nil? vres) vlatter (f vres vlatter)))))
       xs ys))
    ([f xs ys & zrest] (reduce #(my-merge-with f %1 %2) (my-merge-with f xs ys) zrest))))

(defcheck solution-c6ac8359
  (fn [f & m]
    (reduce
      #(into %
         (map (fn [[k v]] [k (if (% k) (f (% k) v) v)])
           %2))
      m)))

(defcheck solution-c75d84bc
  (fn ! [op & ss]
    (do #_(println op)
        #_(println ss)
        (if (= 1 (count ss)) (first ss)
                             (let [fs (first ss)
                                   s2 (do #_(println fs (second ss)) (second ss))
                                   r (into {}
                                       (map
                                         #(if (contains? fs %)
                                            (if (contains? s2 %) [% (op (fs %) (s2 %))]
                                                                 [% (fs %)])
                                            [% (s2 %)])
                                         (clojure.set/union (set (keys fs)) (set (keys s2)))
                                         ))
                                   ]
                               (apply ! (concat [op] [r] (drop 2 ss))))))
    ))

(defcheck solution-c7b78607
  (fn [f & args]
    (reduce (fn[map1 map2]
              (reduce (fn [m [k v]]
                        (if-let [vv (m k)]
                          (assoc m k (f vv v))
                          (assoc m k v)))
                map1 map2))
      args)))

(defcheck solution-c864da4b
  (fn [f m & ms]
    (if (seq ms)
      (recur f
        (reduce
          (fn [r [k v]]
            (if (contains? r k)
              (assoc r k (f (get r k) v))
              (assoc r k v)))
          m
          (first ms))
        (rest ms))
      m)))

(defcheck solution-c8c13781
  (fn mwith [f & maps]
    (letfn [(mr [m1 m2]
              (let [ks (into #{} (concat (keys m1) (keys m2)))]
                (letfn [(nm[k] (cond (and (get m1 k) (get m2 k)) {k (f (get m1 k) (get m2 k))}
                                     (get m1 k) {k (get m1 k)}
                                     :else {k (get m2 k)}))]
                  (apply merge (map nm ks)))))]
      (reduce mr maps))))

(defcheck solution-c8cb43d4
  (fn [f & maps] (reduce (fn [m x] (reduce #(conj % (let [k (first %2)] (if (contains? m k) {k (f (get m k) (second %2))} %2))) m x)) {} maps)))

(defcheck solution-c913d1ea
  (fn my-merge-with [fun & maps]
    (reduce (fn [m1 m2]
              (reduce (fn [m1 [k v]]
                        (if (contains? m1 k)
                          (assoc m1 k (fun (m1 k) v))
                          (assoc m1 k v)))
                m1 m2))
      maps)))

(defcheck solution-c941eb53
  (fn [r f & l]
    (r #(r (fn [m [k v]]
             (assoc m k (if (m k) (f (m k) v) v)))
          % %2)
      l)) reduce)

(defcheck solution-c96f2754
  (fn necro [f l & ol]
    (let [result
          (for [a ol]
            (for [b a]
              (if (contains? l (key b))
                (hash-map (key b) (f (get l (key b)) (val b)))
                (hash-map (key b) (val b)))))]
      (apply conj (flatten result)))))

(defcheck solution-c9bbace5
  (fn mg
    ([f a b]
     (reduce (fn [c [k v]]
               (if (c k)
                 (assoc c k (f (c k) v))
                 (assoc c k v))) a b))
    ([f a b & more]
     (apply (partial mg f (mg f a b)) more))))

(defcheck solution-c9dc6031
  (fn fn-merge [op & maps]
    (reduce (fn make-map [res nxt]
              (merge res
                (reduce (fn with-keys [res-map k]
                          (assoc res-map k (if (contains? res k)
                                             (op (get res k) (get nxt k))
                                             (get nxt k)))) {} (keys nxt)))) {} maps)))

(defcheck solution-ca1c6ca1
  (fn mymw[f mm & ms]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [key val]]
            (if (contains? m key)
              (update-in m (vector key) #(f % val))
              (conj m [key val])))
          m1
          m2
          ))
      mm
      ms)))

(defcheck solution-ca3108c9
  (fn [f & maps] (reduce #(reduce (fn [r [k v]]
                                    (if-let [v2 (r k)]
                                      (assoc r k (f v v2))
                                      (assoc r k v)))
                            %2
                            %)
                   maps)))

(defcheck solution-ca4e7aa0
  (fn [f m & r] (reduce (fn [h n] (reduce (fn [a [k v]] (assoc a k (if (contains? a k) (f (a k) v) v))) h n)) m r)))

(defcheck solution-cab597f0
  (fn [f & maps]
    (loop [answer (first maps)
           curr-map (fnext maps)
           maps (nnext maps)]
      (cond
        (nil? curr-map) answer
        (empty? curr-map) (recur answer (first maps) (next maps))
        :else (let [[k v] (first curr-map)]
                (recur (if-let [answer-val (get answer k)] (assoc answer k (f answer-val v))
                                                           (assoc answer k v))
                  (dissoc curr-map k)
                  maps))))))

(defcheck solution-caf25fc4
  (fn [f & coll]
    (reduce
      (fn [a b]
        (reduce
          (fn [m [k v]] (assoc m k (if-let [ov (m k)] (f ov v) v))
            ) a b
          )
        ) {} coll
      )
    ))

(defcheck solution-cb7d51dc
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-cb98bf9d
  (fn [f & maps]
    (reduce (fn [m1 m2]
              (let [k1 (set (keys m1))
                    k2 (set (keys m2))
                    #_#__ (println k1 k2)
                    kboth (clojure.set/intersection k1 k2)
                    #_#__ (println kboth)
                    map1 (merge m1 m2)
                    #_#__ (println map1)
                    newkvs (reduce concat
                             (map
                               (fn [k]
                                 [k (f (m1 k) (m2 k))])
                               (seq kboth)))
                    #_#__ (println newkvs)]
                (apply assoc map1 newkvs))

              ) maps)))

(defcheck solution-cba9643f
  (fn [f x & xs]
    (reduce
      (fn [m0 m1]
        (loop [m m0 n m1]
          (if (empty? n)
            m
            (let [k1 (first (keys n))
                  v1 (get n k1)
                  n' (dissoc n k1)
                  v0 (if (contains? m k1) (f (get m k1) v1) v1)
                  m' (assoc m k1 v0)
                  ]
              (recur m' n'))))
        ) x xs)
    ))

(defcheck solution-cbf61dd6
  (fn my-merge-with
    [f & maps]
    (reduce (fn [m1 m2] (reduce (fn [m1 [k v]]
                                  (if (contains? m1 k)
                                    (update-in m1 [k] f v)
                                    (assoc m1 k v))) m1 m2))
      (first maps) (rest maps))))

(defcheck solution-cc53f5d9
  (fn [f r & m]
    (reduce #(reduce
               (fn [r [k v]]
                 (assoc r k (if (r k) (f (r k) v) v))
                 ) %1 %2) r m)))

(defcheck solution-cc8a2427
  (fn [f m & ms]
    (if (seq ms)
      (let [m2 (first ms)]
        (recur f
          (into m (map (fn [[k v]] (if (contains? m k) {k (f (m k) v)} {k v})) m2))
          (rest ms) ))
      m)))

(defcheck solution-ccda603d
  (fn [f & ms]
    (reduce
      (fn [acc m]
        (reduce
          #(let [k (key %2) v (val %2)]
             (if (contains? % k)
               (update-in % [k] f v)
               (assoc-in % [k] v)))
          acc m))
      ms)))

(defcheck solution-ccdb423a
  (fn [f & maps]
    (reduce (fn [base-map m-to-add]
              (reduce (fn [m k] (if (contains? m k)
                                  (assoc m k (f (get m k) (get m-to-add k)))
                                  (assoc m k (get m-to-add k))))
                base-map
                (keys m-to-add)))
      {}
      maps)))

(defcheck solution-ccf23a64
  (fn [f & ms]
    (reduce #(let [[a,b] %2]
               (if (%1 a)
                 (update-in %1 [a] f b)
                 (conj %1 %2)))
      {}
      (apply concat ms))))

(defcheck solution-cd0dd8c7
  (fn [f & mcoll]

    (reduce #(into %1 (map (fn [[k v]] {k (if (%1 k) (f (%1 k) v) v)}) (seq %2))) {} mcoll)))

(defcheck solution-cd492e5c
  (fn [f & maps]
    (loop [res {} maps maps]
      (if (empty? maps)
        res
        (recur
          (loop [mer res m (first maps)]
            (if (empty? m)
              mer
              (recur
                (let [k (key (first m)) v (val (first m))]
                  (if (contains? mer k)
                    (assoc mer k (f (get mer k) v))
                    (assoc mer k v)))
                (rest m))))
          (rest maps))))))

(defcheck solution-cd6ac242
  (fn [op & maps]
    (->> maps
      (mapcat #(for [[k v] %] [k v]))
      (reduce (fn [m [k v]] (if (contains? m k)
                              (assoc m k (op (get m k) v))
                              (assoc m k v)))
        {}))))

(defcheck solution-cdf7e927
  (fn [f & ms]
    (reduce (fn [a m] (reduce
                        #(let [[k v] %2 x (%1 k)] (if (nil? x) (assoc %1 k v) (assoc %1 k (f x v))))
                        a m)) {}  ms)
    ))

(defcheck solution-ce5cb7c9
  (fn mrg-wth
    [f m & maps]
    (reduce
      (fn [a b]
        (reduce
          (fn [mp [k v]]
            (let [exists? (contains? mp k)
                  existing (get mp k)]
              (if exists?
                (assoc mp k (f existing v))
                (assoc mp k v))))
          a
          b))
      m
      maps)))

(defcheck solution-ce61ea2e
  (fn
    [f & m]
    (reduce-kv
      (fn
        [m k v]
        (assoc m k
                 (let [[h & t :as all] (map val v)] (if (nil? t) h (apply f all)))))
      {}
      (group-by key (apply concat m)))))

(defcheck solution-ce8dba94
  (fn [f x & xs]
    (reduce (fn [x x1]
              (reduce (fn [x [k v]]
                        (if (contains? x k)
                          (update-in x [k] f v)
                          (assoc-in x [k] v)))
                x x1)) x xs)))

(defcheck solution-ce984bad
  (fn my-merge-with [f & maps]
    (reduce
      #(into (merge %1 %2)
         (map (fn [k] (vector k (f (get %1 k) (get %2 k))))
           (clojure.set/intersection (set (keys %1)) (set (keys %2)))))
      maps)))

(defcheck solution-ceaee225
  (fn mrg [f & mps]
    (reduce
      #(conj %1
         (apply hash-map (reduce concat
                           (map
                             (fn [x]
                               (if (contains? %1 x) [x (f (get %1 x) (get %2 x))] [x (get %2 x)])
                               )
                             (keys %2)
                             )
                           ))
         )
      mps
      )
    ))

(defcheck solution-ceb7b1c
  (fn [f & maps]
    (when (some identity maps)
      (let [mrg-each (fn [m e]
                       (let [k (key e) v (val e)]
                         (if (contains? m k)
                           (assoc m k (f (get m k) v))
                           (assoc m k v))))
            mrg (fn [m1 m2]
                  (reduce mrg-each m1 m2))]
        (reduce mrg maps)))))

(defcheck solution-cedc22bf
  (fn my-merge [f m & ms]
    (let [m2 (apply merge ms)]
      (reduce
        #(assoc % %2
                  (if (contains? % %2)
                    (f (% %2) (m2 %2))
                    (m2 %2)))
        m
        (keys m2)))))

(defcheck solution-cef5025e
  (fn m-w [f & args]
    (reduce
      (fn [init-map new-map]
        (into init-map
          (for [[k v] new-map]
            (if (init-map k) [k (f (init-map k) v)] [k v]))))
      args)))

(defcheck solution-cf7eb235
  (fn [f & M] (reduce (fn [R m] (reduce-kv (fn [r k v] (assoc r k (if-let [u (r k)] (f u v) v))) R m)) {} M)))

(defcheck solution-cf951d70
  (fn [f & [m & ms]]
    (letfn [(mf [f m1 m2]
              (into m1 (for [[k v] m2] [k (if (contains? m1 k) (f (m1 k) v) v)])))]
      (reduce #(mf f %1 %2) m ms))))

(defcheck solution-d0079ccc
  (fn [func & args] (reduce

                      #(into {} (for [k (clojure.set/union (into #{} (keys %1))
                                          (into #{} (keys %2)))]
                                  (cond (and (%1 k) (%2 k)) [k (func (%1 k) (%2 k))]
                                        (%1 k) [k (%1 k)]
                                        (%2 k) [k (%2 k)]))) args)))

(defcheck solution-d0776a50
  (fn [f & xs]
    (->> xs
      (apply concat)
      (group-by first)
      (reduce-kv
        (fn [acc k v] (assoc acc k (reduce f (map second v))))
        {}))))

(defcheck solution-d0f38c87
  (fn [fun & maps]
    (reduce
      #(loop [acc %1
              [fk & rk] (keys %2)]
         (if (nil? fk)
           acc
           (recur
             (if-let [acc-val (acc fk)]
               (assoc acc fk (fun acc-val (%2 fk)))
               (assoc acc fk (%2 fk)))
             rk)))
      maps)))

(defcheck solution-d127019b
  (fn [f m & ms]
    (reduce (fn [m1 m2]
              (into {} (for [k (distinct (concat (keys m1) (keys m2)))]
                         (cond
                           (and (m1 k) (m2 k)) [k (f (m1 k) (m2 k))]
                           (m1 k) [k (m1 k)]
                           (m2 k) [k (m2 k)]))))
      m
      ms)))

(defcheck solution-d1311d27
  (fn merger-with
    ([f m m2]
     (reduce (fn [acc [k v]] (conj acc [k (if (contains? acc k) (f (acc k) v) v)]))
       m m2))
    ([f m m2 & others]
     (reduce #(merger-with f %1 %2) (merger-with f m m2) others))))

(defcheck solution-d1687a1e
  (fn my-merge [f & mps]
    (letfn [(m-k [m e]
              (if (contains? m (key e))
                (assoc m (key e) (f (get m (key e)) (val e)))
                (assoc m (key e) (val e))))
            (mrg [m1 m2]
              (reduce m-k m1 (seq m2)))]
      (reduce mrg mps))))

(defcheck solution-d2ad9d55
  (fn [func & maps]
    (let [joined (into {}
                   (for [[k v]
                         ((comp #(group-by first %) #(partition-all 2 %) #(apply concat %) #(apply concat %) concat #(map seq %)) maps)]
                     [k (apply vector (flatten (map second v)))]))]
      (if (= concat func) joined
                          (into {} (for [[k v] joined]
                                     (if (empty? (rest v)) [k (first v)] [k (apply func v)])))))))

(defcheck solution-d2e29cc4
  (fn [h & maps]
    (let [ks (into #{} (flatten (for [m maps] (keys m)))),                                       ; all keys from all maps in a set
          vs (for [k ks] (conj {} [k (filter #(not (nil? %1)) (for [map maps] (get map k)))])) ] ; all not-nil values from all keys
      (loop [f (first vs), r (rest vs), m {}]  ; recursively build up a map to return as our result
        (let [k (first (keys f)),              ; the current key we're working on
              n (count (first (vals f)))]      ; the # of values for this key
          (if (nil? k) m                       ; if it's nil, we're done ... return our result 'm'
                       (cond
                         (= n 1) (recur (first r) (rest r) (conj m [k (first (get f k))])) ; only 1 value, just append to result 'm'
                         :else (recur (first r) (rest r) (conj m [k (apply h (get f k))])))) ; more than 1 value, append to result 'm' the result of h
          )))))

(defcheck solution-d2fa8480
  (fn [f & ms]
    (reduce
      (fn [m1 m2]
        (if (empty? m2)
          m1
          (let [e (first m2)
                k (first e)
                v1 (second e)
                v (if (m1 k)
                    (f (m1 k) v1)
                    v1)]
            (recur (assoc m1 k v) (rest m2)))))
      ms)))

(defcheck solution-d38e4a23
  (fn [f m & ms]
    (reduce
      (fn [m1 m2]
        (reduce
          #(if (% %2)
             (update-in % [%2] f (m2 %2))
             (assoc % %2 (m2 %2)))
          m1
          (keys m2)))
      m
      ms)))

(defcheck solution-d3ab4b00
  (fn [f & a]
    (reduce (fn [m [k v]]
              (if (contains? m k)
                (assoc m k (f (m k) v))
                (assoc m k v)))
      {} (mapcat vec a))
    ))

(defcheck solution-d4234d2f
  (fn merges [f seq & seqs]
    (letfn[( subfn [f all sub top ]
             (loop [all all
                    sub sub]
               (if(empty? sub)
                 all
                 (let [ car (first sub)]
                   (recur (assoc all car (f (all car ) (top car )))
                     (rest sub))))))]
      (if(empty? seqs)
        seq
        (let [top (first seqs)
              ; _ (println (keys top))
              all (->>
                    (filter #(not (contains?  seq  %)) (keys top))
                    (select-keys top )
                    (merge seq ))
              ;_ (println all)
              sub  (filter #(contains?  seq %) (keys top))
              ; _ (println sub)
              ]
          (if(empty? sub)
            (recur f all (rest seqs))
            (recur f
              (subfn f all sub top )
              (rest seqs))))))))

(defcheck solution-d47125a9
  (fn m-with [f a b & r]
    (let [merged (apply merge a b
                   (for [[k v] b :when (a k)]
                     {k (f (a k) v)}))]
      (if (empty? r)
        merged
        (apply m-with f merged r)))))

(defcheck solution-d4b64fd3
  (fn [f & maps]
    (into
      {}
      (for [k
            (apply
              clojure.set/union
              (map #(set (keys %)) maps))]
        [k (reduce
             f
             (filter
               (complement nil?)
               (map #(get % k) maps)))]))))

(defcheck solution-d53b966e
  (fn merge-with2 [f m & ms]
    (let [update-or-add (fn update-or-add [m1 k f arg]
                          (let [x (get m1 k)]
                            (if (nil? x)
                              (conj m1 {k arg})
                              (update-in m1 [k] f arg))))]
      (loop [m1 m m2 (first ms) ms ms
             k (keys m2)]
        (if-not (map? m2)
          m1
          (recur (reduce #(update-or-add %1 %2 f (get m2 %2)) m1 k)
            (first (next ms))
            (next ms)
            (keys (first (next ms)))))))))

(defcheck solution-d5acc455
  (fn __ [f & maps]
    (apply hash-map
      (mapcat #(conj [(key %)]
                 (reduce f (map second (val %))))
        (group-by first (mapcat vec maps))))))

(defcheck solution-d5b63ccf
  (fn [f & maps]
    (reduce (fn [m [k v]]
              (if (contains? m k)
                (assoc m k (f (m k) v))
                (assoc m k v)))
      {}
      (reduce concat maps))))

(defcheck solution-d6e096f6
  (fn [f m & ms]
    (letfn [(merge-maps [m1 m2]
              (reduce (fn [acc [k v]] (if (contains? acc k)
                                        (assoc acc k (f (get acc k) v))
                                        (assoc acc k v)))
                m1 m2))]
      (reduce merge-maps m ms))))

(defcheck solution-d7572d46
  (fn [f & a]
    (reduce
      (fn
        [m x]
        (if (nil? (get m (first x)))
          (conj m x)
          (assoc m (first x)
                   (f (get m (first x)) (last x)))))
      {} (reduce concat a))))

(defcheck solution-d79cd431
  (fn[f & maps]
    (reduce
      (fn [m1 m2]
        (apply
          (partial merge m1)
          (for [kv m2]
            (if (contains? m1 (first kv))
              {(first kv) (f (get m1 (first kv)) (fnext kv))}
              {(first kv) (fnext kv)})))) maps)))

(defcheck solution-d7a7fb23
  (fn merge-with* [f m & ms]
    (reduce #(assoc %1 (first %2)
                       (if (nil? (%1 (first %2)))
                         (second %2)
                         (f (%1 (first %2)) (second %2))))
      m (apply merge ms))))

(defcheck solution-d7bcae61
  (fn [f & m]
    (reduce
      #(reduce
         (fn [a [k v]]
           (if (a k)
             (update-in a [k] f v)
             (assoc a k v)))
         %
         %2)
      m)))

(defcheck solution-d7ded839
  (fn [f m & c]
    (reduce (fn[a b] (into a (map (fn[[k v]] {k (if (nil? (m k)) v (f (m k) v))}) b))) m c)))

(defcheck solution-d7ef1230
  (fn mf [f & xs]
    (into {} (map (fn [[a b]] {a (if (= 1 (count b))
                                   (second (first b))
                                   (apply f (map second b)))})
               (group-by first (mapcat vec xs))))))

(defcheck solution-d831cf2
  (fn [func & maps]
    (let [joined (reduce (fn [join [k v]] (assoc join k (conj (get join k [] ) v))) {} (apply concat maps))]
      (apply hash-map (mapcat (fn [[k,v]] [k,(if (seq (rest v)) (apply func v) (first v))]) joined)))))

(defcheck solution-d8421c0d
  (fn [the-fn & maps]
    (loop [ms (rest maps)
           acc (first maps)]
      (if (empty? ms)
        acc
        (recur (rest ms) (loop [pairs (seq (first ms))
                                acc acc]
                           (if (empty? pairs)
                             acc
                             (recur (rest pairs) (let [fp (first pairs)
                                                       [k v] fp
                                                       ext (acc k)]
                                                   (if (nil? ext)
                                                     (apply assoc acc fp)
                                                     (assoc acc k (the-fn ext v))))))))))))

(defcheck solution-d909eb07
  (fn mw [f & m]
    (reduce
      (fn [a b]
        (reduce
          (fn [x [k v]]
            (assoc x k (if (b k) (f v (b k)) v)))
          b a))
      (first m) (rest m))))

(defcheck solution-d90a0ca4
  ( fn [f & maps]
    (let [mergein (fn [f m1 m2]  (reduce  #(if (contains? %1 %2) (assoc %1 %2 (f (%1 %2)(m2 %2)))(assoc %1 %2 (m2 %2)))  m1 (keys m2)) )]
      (reduce #(mergein f %1 %2) {} maps )
      )
    ))

(defcheck solution-d94ad81
  (fn merge-with-function [f & ms]
    (letfn [(group [m [k v]]
              (assoc m k (conj (m k []) v)))]
      (->> ms
        (apply concat)
        (reduce group {})
        (map #(vector (first %) (reduce f (second %))))
        (into {})))))

(defcheck solution-d960ceb3
  (fn [f & m]
    (reduce (fn [a [k v]]
              (assoc a k (if (a k) (f (a k) v) v)))
      {}
      (for [x m z x] z))))

(defcheck solution-da35fe7f
  (fn my-merge-with
    [f m1 & m]
    (let [merge-maps (fn [f m1 m2]
                       (if (empty? m2)
                         m1
                         (let [k (ffirst m2) v (second (first m2))]
                           (if (not (nil? (get m1 k)))
                             (recur f (assoc m1 k (f (get m1 k) v)) (rest m2))
                             (recur f (conj m1 (first m2)) (rest m2))))))]
      (reduce #(merge-maps f % %2) m1 m))))

(defcheck solution-da8978d7
  (fn mwf
    ([f m] m)
    ([f m m2]      (reduce (fn [a [k v]] (assoc a k (if (contains? m k) (f (m k) (m2 k)) (m2 k))))  m   m2))
    ([f m m2 & ms] (apply mwf (cons f (cons (mwf f m m2) ms))))))

(defcheck solution-dac71271
  (fn my-merge-with [f & maps]
    (if (some identity maps)
      (letfn [(merge-entry [m e]
                (let [k (key e) v (val e)]
                  (if (contains? m k)
                    (assoc m k (f (get m k) v))
                    (assoc m k v))))
              (merge2 [m1 m2]
                (reduce merge-entry (or m1 {}) m2))]
        (reduce merge2 maps)))))

(defcheck solution-dacd8691
  (fn merge-on [f & ms]
    (letfn [(merge-kv [m p]
              (let [k (key p) v (val p)]
                (if (contains? m k)
                  (assoc m k (f (get m k) v))
                  (assoc m k v))))
            (merge-mcoll [m1 m2]
              (reduce merge-kv (or m1 {}) m2))]
      (reduce merge-mcoll ms))))

(defcheck solution-dafce3df
  (fn [f & args]
    (apply hash-map(apply concat
                     (map
                       (fn [k] [k (reduce f (filter #(not (nil? %)) (map #(% k) args)))])
                       (set (apply concat (map keys args))))))))

(defcheck solution-db3e4139
  (fn [f & maps]
    (letfn [(merge-vals [acc m]
              (let [kvs
                    (for[[k v] m]
                      (let [new-val (if (contains? acc k) (f (acc k) v) v)]
                        [k new-val]))]
                (into acc kvs)))]

      (reduce merge-vals maps))))

(defcheck solution-dbc3ffe0
  (fn map-murge
    [fun & maps]
    (reduce #(reduce (fn [result [key val]]
                       (if (get result key)
                         (assoc result key (fun (get result key) val))
                         (assoc result key val))) %1 %2) {} maps)))

(defcheck solution-dc8f87b8
  (fn mw [f & maps]
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry m1 (seq m2)))
          ]
      (reduce merge2 maps))))

(defcheck solution-dcc37a2d
  (fn merge-with_ [f m & ms]
    (if (empty? ms) m
                    (let [merge-single (fn [m s]
                                         (if (m (key s))
                                           (assoc m (key s) (f (m (key s)) (val s)))
                                           (assoc m (key s) (val s))))
                          merge (fn [m1 m2]
                                  (if (empty? m1)
                                    m1
                                    (reduce merge-single m1 m2)))]
                      (reduce merge m ms)))))

(defcheck solution-dcd60d66
  (fn m [o & xs]
    (reduce (fn f [m1 m2]
              (reduce (fn [m1 [k v]]
                        (if (contains? m1 k)
                          (assoc m1 k (o (m1 k) v))
                          (assoc m1 k v)))
                m1 (seq m2)))
      {} xs)))

(defcheck solution-ddbd783d
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]]
            (assoc m k (if (m1 k) (f (m1 k) v) v)))
          m1 m2))
      (first maps) (rest maps))))

(defcheck solution-ddebe02c
  (fn my-merge-with [f & ms]
    (reduce (fn [merged m]
              (reduce (fn [new-map k]
                        (let [result (get new-map k)
                              new    (get m k)]
                          (if result
                            (assoc new-map k (f result new))
                            (assoc new-map k new))))
                merged
                (keys m)))
      {} ms)))

(defcheck solution-de40ba11
  (fn [f & ms]
    (reduce (fn [m n]
              (reduce (fn [m [k v]]
                        (if (m k)
                          (update-in m [k] f v)
                          (assoc m k v)))
                m n))
      {} ms)))

(defcheck solution-de5d1c44
  (fn [f m & ms]
    (reduce (fn [mo mi]
              (reduce-kv
                (fn [m k v1]
                  (assoc m k (if-let [v0 (get m k)] (f v0 v1) v1)))
                mo mi))
      m ms)))

(defcheck solution-de622f0c
  (fn [f & maps]
    (letfn [(mymerge [a b]
              (into a (cons b (map (fn [x] [x (f (a x) (b x))]) (clojure.set/intersection (set (keys a)) (set (keys b)))))))]
      (reduce mymerge {} maps))))

(defcheck solution-de6290e9
  (fn [f & args]
    (loop [[m & l] args r {}]
      (if (empty? m) r (recur l (reduce #(if (not (% (first %2))) (conj % %2) (conj % {(first %2) (f (% (first %2)) (last %2))})) r m))))))

(defcheck solution-dec378d8
  (fn  [f c & cols]
    (reduce
      #(reduce (fn [l r]
                 (let [k (key r)
                       v (val r)
                       w (get l k)]
                   (if w (assoc l k (f w v)) (assoc l k v)))) % %2)
      c cols)))

(defcheck solution-dee1b33f
  (fn [f & v]
    (reduce
      #(reduce
         (fn [o [k v]]
           (persistent!
             (assoc!
               (transient o)
               k
               (if (o k)
                 (f (o k) v)
                 v))))
         % %2)
      v)))

(defcheck solution-df2a0ca7
  (fn [func m & ms]
    (let [
          ms (concat [m] ms)
          ks (set (mapcat keys ms))]
      (reduce
        (fn [b k]
          (assoc b k (reduce
                       func
                       (keep (fn [m] (m k))
                         ms))))
        {}
        ks))))

(defcheck solution-df368d88
  (fn [f m & ms]
    (reduce (fn [res m-n]
              (reduce (fn [r_ m-e]
                        (let [mk (key m-e)
                              mv (val m-e)]
                          (if (contains? r_ mk)
                            (assoc r_ mk (f (get r_ mk) mv))
                            (assoc r_ mk mv))))
                res
                (seq m-n)))
      m
      ms)))

(defcheck solution-df508f51
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps)))))

(defcheck solution-df54b6ad
  (fn [f & maps]
    (let [mmap
          (reduce
            (fn [acc m]
              (reduce
                (fn [acc e]
                  (let [[k v] e]
                    (conj acc [k (conj (get acc k []) v)])))
                acc m))
            {} maps)]
      (reduce #(conj % [(first %2) (reduce f (second %2))]) {} mmap))))

(defcheck solution-dfb7c44a
  (fn prob-0069
    [f & mps]
    (cond
      (zero? (count mps))  nil
      (= 1   (count mps)) (first mps)
      :else  (let [red-fn (fn [dst-map el]
                            (let [k (key el), v (val el)]
                              (if (contains? dst-map k)
                                (into dst-map {k (f (dst-map k) v)})
                                (into dst-map {k v}))))]

               (reduce red-fn (first mps) (apply concat (rest mps)))))))

(defcheck solution-dfcc1eb0
  (fn [f & ms]
    (loop [ks (seq (set (apply concat (map keys ms))))
           ms ms
           vs (for [k ks] (remove nil? (for [m ms] (m k))))
           m {}]
      (if (empty? ks) m
                      (let [k (first ks)
                            v (first vs)
                            v (if (= 1 (count v)) (first v) (apply f v))]
                        (recur (rest ks) ms (rest vs) (assoc m k v)))))))

(defcheck solution-e0313252
  (fn [f m & ms]
    (reduce
      #(reduce
         (fn [rs [k v]] (assoc rs k (if (rs k)
                                      (f (rs k) v)
                                      v)))
         % %2)
      m ms)))

(defcheck solution-e040d067
  (fn merge-by [f & maps]
    (reduce
      (fn [acc maps]
        (reduce-kv
          (fn [m k v]
            (assoc m k
                     (if (contains? m k)
                       (f (m k) v) v)))
          acc maps))
      {} maps)))

(defcheck solution-e0743efd
  (fn [f m & ms]
    (reduce (fn [a b] (apply conj a (map (fn [[k bv]] (if-let [av (a k)] [k (f av bv)] [k bv])) b))) m ms)))

(defcheck solution-e0926d02
  (fn [f m & ms] (into m (for [x ms [k v] x] (if (contains? m k)
                                               [k (f (m k) v)]
                                               [k v])))))

(defcheck solution-e093b115
  (fn merge12 [f & cc] (let [km (->> cc (map keys) (apply concat) set)]
                         (->> (for [k km] [k (filter #(not (nil? %)) (map #(get % k) cc))])
                           (map (fn [c] [(first c) (reduce f (second c))])) (into {})) ;(map #([(first %) (apply f (second %))]))
                         )))

(defcheck solution-e0f2be6d
  (fn [f & ms]
    (reduce (fn [m1 m2]
              (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (update-in m [k] f v)
                          (assoc m k v)))
                m1 m2))
      ms)))

(defcheck solution-e1158e82
  (fn [f & maps]
    (reduce
      (fn [m1 m2]
        (reduce
          (fn [m [k v]]
            (assoc m k
                     (if-let [vold (get m k)] (f vold v) v)))
          m1 m2))
      maps)))

(defcheck solution-e169b598
  (fn [fx & maps]
    (letfn [(foo [m1 m2 ks]
              (if (empty? ks) m1
                              (let [k (first ks)]
                                (if (contains? m1 k)
                                  #(foo (assoc m1 k (fx (m1 k) (m2 k))) m2 (rest ks))
                                  #(foo (assoc m1 k (m2 k)) m2 (rest ks))))))]
      (reduce #(trampoline foo % %2 (keys %2))
        maps))))

(defcheck solution-e1812d61
  (fn [g & ms]
    (let [smoosh (fn [m1 m2]
                   (let [ks (->> [m1 m2]
                              (map (comp set keys))
                              (apply clojure.set/union))]
                     (->> (for [k ks]
                            (let [vs (->> [m1 m2]
                                       (map (fn [m] (get m k)))
                                       (filter identity))]
                              [k (if (== 1 (count vs))
                                   (first vs)
                                   (apply g vs))]))
                       (into {}))))]
      (reduce smoosh ms))))

(defcheck solution-e18c8fc2
  (fn mw [f & maps]
    (loop [rv (first maps) maps (rest maps)]
      (if (empty? maps)
        rv
        (let [right (first maps)
              ks (apply conj (keys rv) (keys right))
              both_keys (set (filter #(and (contains? right %) (contains? rv %)) ks))]
          (recur (merge rv right
                   (into {} (map #(vector % (f (rv %) (right %))) both_keys)))
            (rest maps)))))))

(defcheck solution-e1921808
  (fn merge-with* [f & [h & t]]
    ( letfn [(r [a [k v]] (conj a (if-let [y (get a k)] [k (f y v)] [k v])))]
      (reduce #(reduce r %1 %2) h t))))

(defcheck solution-e1acbcff
  (fn [func & maps]
    (reduce (fn [ret m]
              (reduce (fn [r [k v]]
                        (if (contains? r k)
                          (assoc r k (func (get r k) v))
                          (assoc r k v)))
                ret
                m))
      {}
      maps)))

(defcheck solution-e1fb5743
  (fn [func & maps]
    (loop [result {} allkeys
                  (loop [mkeys #{} elements maps]
                    (if elements
                      (recur (clojure.set/union mkeys (set (keys (first elements))))
                        (next elements)
                        )
                      mkeys
                      )
                    )
           ]
      (if allkeys
        (recur
          (into result {(first allkeys)
                        (let [temp-map
                              (loop [map-index [] all-maps maps i 0]
                                (if all-maps
                                  (recur (if (contains? (first all-maps) (first allkeys))
                                           (conj map-index i)
                                           map-index)
                                    (next all-maps)
                                    (inc i))
                                  map-index
                                  )
                                )]

                          (if (= (count temp-map) 1)
                            ((nth maps (first temp-map)) (first allkeys))
                            (func ((nth maps (first temp-map)) (first allkeys)) ((nth maps (second temp-map)) (first allkeys)))
                            )
                          )
                        })
          (next allkeys)
          )
        result
        )
      )
    ))

(defcheck solution-e26faafa
  (fn [f & maps]
    (reduce
      (fn [merged-map next-map]
        (reduce
          (fn [the-map [k v]]
            (if (contains? the-map k)
              (assoc the-map k (f (get the-map k) v))
              (assoc the-map k v)))
          merged-map
          next-map))
      (first maps)
      (rest maps))))

(defcheck solution-e3393c03
  (fn [f & ms]
    (reduce (fn [r m]
              (reduce (fn [m' [k v]]
                        (assoc m' k (if (contains? m' k) (f (m' k) v) v)))
                r m))
      {} ms)))

(defcheck solution-e3eab705
  (fn [f & s]
    (reduce (fn [s n]
              (apply conj s
                (for [m n [k v] n]
                  (if (s k)
                    {k (f (s k) v)}
                    {k v}))))
      s)))

(defcheck solution-e4390893
  (fn [f & maps]
    (reduce (fn [acc curr]
              (let [base (conj acc curr)
                    conflicts (into {} (for [[k v] curr]
                                         (when-let [acc-v (get acc k)]
                                           [k (f acc-v v)])))]
                (conj base conflicts)))
      {} maps)))

(defcheck solution-e493e436
  (fn merge-maps [f & maps]
    (reduce
      #(reduce
         (fn [m [k v]]
           (assoc m k (if (contains? m k)
                        (f (get m k) v)
                        v)))
         %1
         %2)
      {}
      maps)))

(defcheck solution-e4d2f81f
  (fn [f & ms]
    (reduce (fn [m k]
              (assoc m k (reduce f (remove nil? (map #(get % k) ms)))))
      {}
      (flatten (map keys ms)))))

(defcheck solution-e4fbe64c
  #(reduce (fn [m [k v]]
             (assoc m k (if (m k)
                          (% (m k) v)
                          v)))
     {}
     (apply concat %&)))

(defcheck solution-e50e326e
  (fn [f & ms] (reduce
                 (fn [x y] (reduce
                             (fn [ m [k v]] (if (get m k) (assoc m k (f (get m k) v)) (assoc m k v) ))
                             x (seq y)))
                 (seq ms))))

(defcheck solution-e5780755
  (fn my-merge-with [f & maps]  (reduce    (fn [a b]      (reduce        (fn [x [k v]]          (assoc x k (if (b k) (f v (b k)) v)))        b a))    (first maps) (rest maps))))

(defcheck solution-e5df83a2
  (fn [func map-1 map-2 & more-maps]
    (if (nil? map-2)
      map-1
      (recur func
        (loop [key-vals (keys map-2)
               map-1-new map-1]
          (if (empty? key-vals)
            map-1-new
            (recur (rest key-vals)
              (assoc
               map-1-new
                (first key-vals)
                (if (contains? map-1-new (first key-vals))
                  (func (get map-1-new (first key-vals))
                    (get map-2 (first key-vals)))
                  (get map-2 (first key-vals))))
              )
            )
          )
        (first more-maps)
        (rest more-maps)
        )
      )
    ))

(defcheck solution-e5f9bba5
  (fn [f & maps]
    (reduce (fn [result latter]
              (reduce (fn [result [k v]]
                        (assoc result
                          k
                          (if (result k)
                            (f (result k) v)
                            v)))
                result
                (seq latter)))
      {}
      maps)))

(defcheck solution-e6012360
  (fn [f & ms] (letfn [(mw [acc [k v]] (assoc acc k (if (contains? acc k) (f (get acc k) v) v)))
                       (rm [acc m] (reduce mw acc m))]
                 (reduce rm {} ms))))

(defcheck solution-e6619cfd
  (fn m-w [f & maps]
    (let [distinct-keys (->> maps (map #(keys %)) (flatten) (distinct))]
      (let [values (->> distinct-keys
                     (map (fn [k] (map #(% k) maps)))
                     (map (fn [xs] (filter #(not (nil? %)) xs)))
                     (map #(if (< (count %) 2)
                             (first %)
                             (apply f %))))]
        (apply hash-map (interleave distinct-keys values))))))

(defcheck solution-e68181c
  (fn mw [f m & ms]
    (letfn [(mrg [m [k v]]
              (assoc m k (if (contains? m k) (f (get m k) v) v)))]
      (reduce #(reduce mrg %1 (seq %2)) m ms))))

(defcheck solution-e6945a90
  (fn [f m & ms]
    (if (seq ms)
      (recur f (reduce
                 (fn [m [k v]]
                   (conj m (if (contains? m k) [k (f (m k) v)] [k v])))
                 m (first ms)) (rest ms))
      m)))

(defcheck solution-e6f7c3c5
  (fn [f m & o]
    (reduce (fn [m1 m2]
              (reduce
                #(update-in %1 [(first %2)]
                   (fn [v1 v2] (if v1 (f v1 v2) v2))
                   (second %2))
                m1 m2))
      m o)))

(defcheck solution-e70669f2
  (fn [f & maps]
    (reduce (fn [acc map]
              (merge map (reduce (fn [acc k]
                                   (update-in acc [k] f (get map k))) acc (clojure.set/intersection (set (keys acc)) (set (keys map)))))) (first maps) (rest maps))))

(defcheck solution-e7c01809
  (fn [f & maps]
    (letfn [(maybe-apply [coll]
              (if (> (count coll) 1)
                (apply f coll)
                (first coll)))]
      (let [smaps (group-by first (apply concat (map #(apply list %) maps)))]
        (zipmap (keys smaps) (map (fn [x] (maybe-apply (map second (second x)))) smaps))))))

(defcheck solution-e7c9b0aa
  (fn mw [f m & ms]
    (reduce
      (fn [resmap curmap]
        (reduce
          (fn [mp entry]
            (if (contains? mp (first entry))
              (assoc mp (first entry) (f (get mp (first entry)) (second entry)))
              (assoc mp (first entry) (second entry))
              ))
          resmap
          curmap
          ))
      m
      ms
      )))

(defcheck solution-e7dcf1ae
  (fn my-merge-with
    ([func m1]
     m1)
    ([func m1 m2 & maps]
     (apply my-merge-with
       func
       (reduce (fn [m [k v]]
                 (assoc
                  m k
                    (if (contains? m k)
                      (func (m k) v)
                      v)))
         m1 m2)
       maps))))

(defcheck solution-e81338c2
  (fn [fcn & args]
    (let [lst
               (reduce #(into % %2) [] args)
          lstb (reduce (fn [s [k v]] (assoc s k (if (s k) (conj  (s k) v ) [v] )  )) {} lst) ]

      (reduce (fn [s [k v]] (assoc s k (if (< 1 (count v)) (apply fcn v) (first v))))  {}  lstb)





      )))

(defcheck solution-e8bf5d9e
  (fn [f m & ms]
    (reduce (fn [a x] (reduce #(if (contains? %1 (first %2))
                                 (update-in %1 [(first %2)] f (second %2))
                                 (conj %1 %2)) a x))
      m ms)))

(defcheck solution-e90b5ae2
  (fn [f coll & colls]
    (let [merge-2-colls
          (fn [coll1 coll2]
            (reduce (fn [acc [k v]] (let [r (if (contains? acc k) (f (get acc k) v) v)] (assoc acc k r))) coll1 coll2))]
      (reduce merge-2-colls coll colls))))

(defcheck solution-e96c8665
  (fn my-merge-with [f & maps]
    (reduce
      (fn [acc m]
        (merge acc
          (into {} (for [k (keys m)]
                     (if (contains? acc k)
                       [k (f (acc k) (m k))]
                       [k (m k)])))))
      maps)))

(defcheck solution-ea4f59bd
  (fn [f & xs]
    (reduce
      (fn [a b]
        (conj a b
          (reduce #(
                     let [v (get b %2)]
                     (if v
                       (assoc %1 %2 (f (get a %2) v))
                       %1
                       )
                     )
            {}
            (keys a)
            )
          )
        )
      xs)
    ))

(defcheck solution-ea5b8d8e
  (fn [op & maps]
    (let [mrg (fn [m n]
                (reduce (fn [mm [k v]]
                          (assoc mm k (if (contains? m k)
                                        (op (m k) v)
                                        v)))
                  m n))]
      (reduce mrg maps))))

(defcheck solution-ea6aea79
  (fn [f & ms]
    (reduce
      (fn [m1 m2]
        (merge m1 (reduce
                    (fn [m [k v]]
                      (assoc m k (if (m1 k)
                                   (f (m1 k)
                                     v) v))) {} m2))) ms)))

(defcheck solution-eaa2744f
  (fn [f & maps]
    (reduce (fn [merged m]
              (reduce
                (fn [merged [k v]]
                  (if (merged k)
                    (assoc merged k (f (merged k) v))
                    (assoc merged k v)))
                merged
                m))
      {}
      maps)))

(defcheck solution-eb908d7c
  (fn my-merge-with [f & maps]
    (letfn [(intersecting-keys [m1 m2]
              (filter
                #(and
                  (contains? m1 %)
                  (contains? m2 %))
                (distinct
                  (concat
                   (keys m1)
                   (keys m2)))))]
      (reduce
        (fn [m1 m2]
          (if-let [crossed-keys (intersecting-keys m1 m2)]
            (conj m1 m2 (reduce #(assoc %1 %2 (f (get m1 %2) (get m2 %2))) {} crossed-keys))
            (conj m1 m2)
            ))
        {}
        maps))))

(defcheck solution-ebcaac2b
  (fn mw [f & ms]
    (let [merge (fn [acc m] (apply zipmap (apply map vector
                                            (map #(vector % (cond (and (m %) (acc %))
                                                                  (f (acc %) (m %))
                                                                  (m %) (m %)
                                                                  :else (acc %)))
                                              (set (concat (keys acc) (keys m))))) ))]
      (reduce merge ms)
      )))

(defcheck solution-ebf127de
  (fn [f m1 & ms]
    (let [merge-one (fn [m [k v]]
                      (update-in m [k]
                        #(if (nil? %1) %2 (f %1 %2))
                        v))]
      (reduce #(reduce merge-one %1 %2) m1 ms))))

(defcheck solution-ec44c60f
  (fn foo
    ([f t] t)
    ([f t m]
     (reduce
       (fn [m [k v]]
         (update-in m [k] (fn [x] (if (nil? x) v (f x v)))))
       t
       m))
    ([f t m & ms]
     (apply foo f (foo f t m) ms))))

(defcheck solution-ec60af99
  (fn [f & maps]
    (reduce (fn [result m]
              (reduce (fn [interim-result [k v :as map-entry]]
                        (if (contains? interim-result k)
                          (conj interim-result [k (f (interim-result k) v)])
                          (conj interim-result map-entry)))
                result
                m))
      maps)))

(defcheck solution-ec9d76b
  (fn [f & m]
    (reduce #(reduce (fn [x [k v]]
                       (if (x k)
                         (assoc x k (f (x k) v))
                         (assoc x k v)))
               %
               %2)
      m)))

(defcheck solution-ed391d59
  (fn [f & l]
    (let [x (reduce concat l)]
      (reduce (fn [h [k v]] (if (h k) (assoc h k (f (h k) v)) (assoc h k v))) {} x))))

(defcheck solution-ed527b7c
  (fn [f & s]
    (let [m (partition-by first
              (sort-by first (apply concat s)))]
      (into {} (mapcat #(set {% %2})
                 (map ffirst m)
                 (map #(if (= 1 (count %))
                         (last %)
                         (apply f %)) (map #(map second %) m))
                 )))))

(defcheck solution-ed6a9aaa
  (fn [f & maps]
    (reduce (fn [result [k value]]
              (let [new-value (if (contains? result k)
                                (f (get result k) value)
                                value)]
                (assoc result k new-value)))
      {}
      (apply concat (map vec maps)))))

(defcheck solution-ee152efc
  (fn r ([f x y] (reduce #(if (% %2)
                            (assoc % %2 (f (% %2) (y %2)))
                            (assoc % %2 (y %2))) x (keys y)))
    ([f x y & more]
     (if (empty? more) (r f x y)
                       (recur f (r f x y) (first more) (next more))))))

(defcheck solution-ee2cee73
  (fn [ f & mps ]
    (let [ km (keys (apply conj mps)) ]
      (reduce
        (fn [a k]
          (let [ v
                (reduce (fn [p q] (if (q k)
                                    (conj p (q k)) p))
                  [] mps) ]
            (conj a { k (reduce f v)} )))
        {} km))))

(defcheck solution-eeb0b72e
  (fn mw[f & args]
    (reduce
      (fn [r i]
        (reduce
          (fn[r [k v]]
            (if (contains? r k)
              (assoc r k (f (get r k) v))
              (assoc r k v)))
          r i))
      {} args)))

(defcheck solution-eef6e1ca
  (fn f [g m & [h & t]]
    (if h
      (apply f g (reduce (fn [n [a b]] (assoc n a (if (n a) (g (n a) b) b))) m h) t)
      m)))

(defcheck solution-ef2e7d01
  (fn [f & m]
    (into {}
      (map (fn [[a b]]
             [a (reduce f (map second b))])
        (group-by first
          (mapcat vec m))))))

(defcheck solution-ef39bed5
  (fn [func & coll]
    (let [all-keys (set (flatten (map keys coll)))]
      (into {}
        (for [k all-keys]
          (->>  (map #(% k) coll)
            (filter identity)
            (reduce func)
            (#(vector k %))
            ))))))

(defcheck solution-ef622d4a
  (fn [f c & cs]
    (reduce (fn [xs ys]
              (reduce (fn [coll entry]
                        (let
                         [k (key entry)
                          v (val entry)]
                          (if (contains? coll k)
                            (assoc coll k (f (coll k) v))
                            (assoc coll k v)))) xs ys))
      c cs)))

(defcheck solution-ef873993
  (fn [f & s]
    (into {} (for [[k v] (group-by first (apply concat (map vec s)))]
               [k (reduce f (map second v))]))))

(defcheck solution-ef91cecf
  (fn [f & m]
    (into {}
      (for [[k v]
            (group-by key (apply concat m))
            ]
        [k (reduce f (map val v))]))))

(defcheck solution-ef94e15f
  (fn app-maps
    [f m1 & maps]
    (if (empty? maps) m1
                      (let [m2 (first maps)
                            m1-keys (set (keys m1))
                            m2-keys (set (keys m2))
                            both (clojure.set/intersection m1-keys m2-keys)
                            m1-only (into {} (filter #(not (contains? both %)) m1))
                            solos (into m1-only (filter #(not (contains? both %)) m2))
                            m (into solos
                                (for [k both]
                                  {k (f (get m1 k) (get m2 k))}))]
                        (recur f m (rest maps))))))

(defcheck solution-ef986d1e
  (fn foo [f m1 & maps]
    (reduce (fn [x y] (reduce
                        #(assoc % (key %2)
                                  (if (contains? % (key %2))
                                    (f (get % (key %2)) (val %2))
                                    (val %2)
                                    )
                                  ) x y)
              ) m1 maps)
    ))

(defcheck solution-efd446b5
  (fn p69 [f & ms]
    (let [keys-to-use (set (flatten (for [m ms] (keys m))))
          vals-to-use (for [k keys-to-use]
                        (reduce f (remove nil? (map #(% k) ms))))]
      (zipmap keys-to-use vals-to-use))))

(defcheck solution-f01d31ee
  (fn [f c & [d & r]] (if d (recur f (reduce (fn [m [k v]] (assoc m k (if (m k) (f (m k) v) v))) c d) r) c)))

(defcheck solution-f02c47a2
  (fn [f m & ms]
    (reduce
      (fn [m [k v]]
        (assoc m k (if (contains? m k) (f (m k) v) v)))
      m
      (apply concat ms))))

(defcheck solution-f032c0bb
  (fn [op x & xs]
    (reduce #(assoc %1 (first %2) (if-let[q (get %1 (first %2))](op q (last %2))(last %2 ) ) )  x (into {} xs))))

(defcheck solution-f095a4f5
  (fn [op orig & maps]
    (letfn [(op-if-cont [o [k v]]
              {k
               (if (contains? o k)
                 (op (o k) v)
                 v)})]
      (loop [[m & rst] maps
             res orig]
        (if m
          (->> m
            (map (partial op-if-cont res))
            (apply merge res)
            (recur rst))
          res)))))

(defcheck solution-f0bd4d15
  (fn mwf [f & maps]
    (let [merge-entry
                     (fn [map e]
                       (let [k (key e) v (val e)]
                         (if (contains? map k)
                           (assoc map k (f (get map k) v))
                           (assoc map k v))))
          merge-hash (fn [m1 m2]
                       (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge-hash maps))))

(defcheck solution-f12e55f9
  (letfn [(reduce-vals [f k ds]
            (reduce f (filter identity (map #(% k) ds))))]
    (fn [f & ds]
      (let [ks (distinct (flatten (map keys ds)))]
        (into {} (map (fn [k]
                        { k (reduce-vals f k ds)}) ks))))))

(defcheck solution-f15b24a5
  (fn [f & maps]
    (reduce #(merge % (zipmap (keys %2)
                        (map (fn [k] (if (% k)
                                       (f (% k) (%2 k))
                                       (%2 k))) (keys %2))))
      {} maps)))

(defcheck solution-f182f381
  (fn [f & m]
    (reduce
      #(into %1 (for [[k v] %2]
                  [k (if-let [w (%1 k)] (f w v) v)]))
      m)))

(defcheck solution-f226dad3
  (fn [f & args]
    (let [upd (fn [x y] (let [old (get-in x [(key y)])]
                          (if (nil? old)
                            y
                            (hash-map (key y) (f old (val y))))))
          fmap (fn [x y] (map #(upd x %) y))]
      (reduce #(into %1 (fmap %1 %2)) args))))

(defcheck solution-f22cc778
  (fn [f & maps]
    (let [kseq (distinct (flatten (for [m maps] (keys m))))
          vseq (for [k kseq]
                 (reduce f
                   (filter (fn[e](not (nil? e)))
                     (for [m maps]
                       (get m k)
                       )
                     )
                   )
                 )
          ]
      (loop [k kseq v vseq r {}]
        (if (empty? k)
          r
          (recur (rest k)(rest v)(assoc r (first k) (first v)))
          )
        )
      )
    ))

(defcheck solution-f246713e
  (fn merge-with-alt [f & colls]
    (let [keys- (distinct (flatten (map #(keys %) colls)))]
      (zipmap
        keys-
        (map #(reduce f %)
          (map #(remove nil? %)
            (for [n keys-]
              (map #(get % n) colls))))))))

(defcheck solution-f28a16d8
  (fn [f & as]
    (let [ks (set (apply concat (map keys as)))]
      (apply merge (for [k ks]
                     (let [vs (filter identity (map #(get % k) as))]
                       (hash-map k (if (> (count vs) 1) (apply f vs) (first vs)))))))))

(defcheck solution-f2ad9348
  (fn mw [f m & ms]
    (if (empty? ms) m
                    (apply mw f
                      (loop [acc m
                             nm (first ms)]
                        (if (empty? nm) acc
                                        (let [[k v] (first nm)]
                                          (recur
                                            (if (contains? (set (keys acc)) k)
                                              (assoc acc k (f (acc k) v))
                                              (assoc acc k v))
                                            (into {} (rest nm))))))
                      (rest ms)))))

(defcheck solution-f2e9c20b
  (let [merging
        (fn [f m n]
          (loop [x m y n]
            (if (empty? y)
              x
              (recur (if (contains? x (key (first y)))
                       (conj x [(key (first y)) (f (get x (key (first y))) (val (first y)))])
                       (conj x [(key (first y)) (val (first y))]))
                (rest y)))))]
    (fn merge-fun [f m & args]
      (if (empty? (rest args))
        (merging f m (first args))
        (apply merge-fun
          f
          (merging f m (first args))
          (rest args))))))

(defcheck solution-f2f0f99
  (fn [f & ls]
    (reduce merge {}
      (for [k (set (map first (apply concat ls)))]
        (hash-map k
          (reduce f (filter #(not (nil? %)) (map #(get % k) ls)))
          )
        )
      )
    ))

(defcheck solution-f2fc8d6f
  (fn [f & [m & ms]]
    (reduce
      (fn [r [k v]]
        (if (contains? r k)
          (assoc r k (f (r k) v))
          (assoc r k v)))
      m
      (apply concat ms))))

(defcheck solution-f30af133
  (fn [f & ls]
    (letfn [(rdc [b r]
              (if (empty? r) b
                             (recur
                               (reduce
                                 #(let [[k v] %2]
                                    (assoc
                                     %1
                                      k
                                      (if (contains? %1 k)
                                        (f (%1 k) v)
                                        v)))
                                 b
                                 (first r))
                               (rest r))))]
      (rdc (first ls) (rest ls)))))

(defcheck solution-f3624de7
  (fn [f & maps]
    (reduce #(conj %
               (reduce (fn [r [k v]]
                         (let [rv (get r k)] (assoc r k (if rv (f rv v) v)))) % %2))
      maps)))

(defcheck solution-f453febd
  (fn [f & s]
    (reduce (fn [a b]
              (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (update-in m [k] #(f % v))
                          (assoc m k v)))
                a b))
      s)))

(defcheck solution-f4778318
  (fn [f & maps]
    (reduce
      (fn [m p] (reduce
                  (fn [m [a b]]
                    (if (contains? m a) (assoc m a (f (m a) b))
                                        (assoc m a b)))
                  m p))
      maps)))

(defcheck solution-f51c58a6
  (fn [func & themaps]
    (let [allkeys (distinct (flatten (map keys themaps)))
          allvals (for [x allkeys] (remove nil? (map #(% x) themaps)))
          funcvals (for [x allvals] (if (= 1 (count x)) (first x) (apply func x)))]
      (apply hash-map (interleave allkeys funcvals)))))

(defcheck solution-f5b46306
  (fn
    [f & ms]
    (reduce (fn [acc el] (reduce #(assoc %1 %2
                                            (if-let [i (%1 %2)]
                                              (f i (el %2))
                                              (el %2)))
                           acc (keys el))) (first ms) (rest ms))))

(defcheck solution-f66fd56b
  (letfn
   [(mergew [f map1 map2]
      ; Base case: empty map2
      (if (empty? map2) map1
                        (let [[k v] (first map2)]
                          ; Key exists in map1, merge with f
                          (if (map1 k)
                            (recur f (assoc map1 k (f (map1 k) v)) (rest map2))
                            ; Key doesn't exist, just add the value
                            (recur f (assoc map1 k v) (rest map2))))))]
    (fn [f & maps]
      (reduce (partial mergew f) (first maps) (rest maps)))))

(defcheck solution-f6b35435
  #(reduce (fn [a [k v]]
             (assoc a k (if-let [w (get a k)] (% w v) v)))
     %2
     (mapcat vec %&)))

(defcheck solution-f7b1c686
  (fn [op m & ms]
    (letfn [(combine ([m [k v1]] (let [v2 (m k) v (if v2 (op v2 v1) v1) ] (assoc m k v)))) ]
      (reduce (partial reduce combine) m ms))))

(defcheck solution-f80d6044
  (fn merge-with*
    ([f m1] m1)
    ([f m1 m2 & r]
     (apply merge-with* f
       (reduce (fn [acc [k v]]
                 (if (acc k)
                   (assoc acc k (f (acc k) v))
                   (assoc acc k v)))
         m1
         m2)
       r))))

(defcheck solution-f823f047
  (letfn [(mw [f h1 h2]
            (reduce (fn [acc key]
                      (if (contains? acc key)
                        (assoc acc key (f (get acc key) (get h2 key)))
                        (assoc acc key (get h2 key))))
              h1 (keys h2)))]
    (fn [f & hashes]
      (reduce (partial mw f) hashes))))

(defcheck solution-f840a0d2
  (fn mymerge [f m & args]
    (merge m (into {}
               (for [x args]
                 (into {} (map #(let [k (first %)
                                      v (second %)]
                                  (vector k (if (contains? m k) (f (get m k) v) v)))
                            x)))
               )
      )))

(defcheck solution-f8cba4a5
  (fn [f & maps]
    (let [merge-entry      (fn [m [k v]]
                             (if (contains? m k)
                               (assoc m k (f (get m k) v))
                               (assoc m k v)))
          merge2           (fn [m1 m2]
                             (reduce merge-entry m1 m2))
          merge-with-redux (fn [f & maps]
                             (reduce merge2 maps))]
      (apply merge-with-redux f maps))))

(defcheck solution-f90e77cc
  (fn prob69 [f & maps]
    (reduce
      (fn [m1 maps]
        (reduce
          (fn [map [key val]]
            #_(println map key val)
            (assoc map key (if (maps key) (f val (maps key)) val))
            )
          maps m1
          ))
      (first maps) (rest maps))))

(defcheck solution-f983807a
  (fn [f & ms]
    (reduce
      #(loop [pairs (seq %2) m %1]
         (if pairs
           (let [[key val] (first pairs)]
             (recur (next pairs)
               (assoc m key
                        (if (contains? m key) (f (m key) val) val))))
           m))
      ms)))

(defcheck solution-f9867aef
  (fn [f m & ms]
    (reduce (fn [m1 m2]
              (reduce (fn [acc [k v]]
                        (update-in acc [k] (fn [old]
                                             (if (nil? old)
                                               v
                                               (f old v)))))
                m1 m2))
      m ms)))

(defcheck solution-f9ba0fd1
  (fn [fun & args]
    (reduce (fn [acc [k v]]
              (if (acc k)
                (assoc acc k (fun (acc k) v))
                (assoc acc k v)))
      {}
      (mapcat seq args))))

(defcheck solution-f9bcb4db
  (fn mw [f m & maps]
    (letfn [(replacements [f orig new]
              (into {}
                (for [[k v] new]
                  (if (contains? orig k)
                    [k (f (orig k) v)]
                    [k v]))))]
      (if (empty? maps)
        m
        (recur
          f
          (merge
            m
            (replacements f m (first maps)))
          (rest maps))))))

(defcheck solution-fa09d96b
  (fn mw [fun & ms]
    (reduce (fn [ret m] (reduce (fn [r [k v]] (assoc r k (if (get r k) (fun (get r k) v) v))) ret m)) {} ms)))

(defcheck solution-fa52ab6d
  (fn mw [merge-function & maps]
    (let [ymerge (fn [f m1 m2]
                   (into m1
                     (loop [ks (keys m2) acc {}]
                       (if (seq ks)
                         (let [fk (first ks)
                               rk (rest ks)]
                           (if (m1 fk)
                             (recur rk (into acc {fk (f (m1 fk) (m2 fk))}))
                             (recur rk (into acc {fk (m2 fk)}))))
                         acc))))]
      (reduce #(ymerge merge-function %1 %2) {} maps))))

(defcheck solution-fa73f858
  (fn [f m & ms]
    (reduce (fn [acc m2]
              (reduce (fn [acc2 [k v]]
                        (assoc acc2 k (if (acc2 k) (f (acc2 k) v) v)))
                acc m2))
      m ms)))

(defcheck solution-fae5e38c
  (fn [g & c]
    (reduce
      #(reduce
         (fn [r [k v]]
           (assoc r k (if (contains? r k) (g (r k) v) v)))
         % %2)
      c)))

(defcheck solution-faf27d6c
  (fn my-merge-with [f & maps]
    (letfn [(merge-item [fun m item]
              (let [k (key item) v1 (m k) v2 (last item)]
                (if v1 (merge m {k (apply fun [v1 v2])}) (conj m item))))]
      (letfn [(merge-map [fun m1 m2]
                (reduce (partial merge-item fun) m1 m2))]
        (reduce (partial merge-map f) {} maps)))))

(defcheck solution-fb0a7ce3
  (fn [f & maps]
    (when (some identity maps)
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (assoc m k (f (get m k) v))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 {} maps)))))

(defcheck solution-fcf782
  (fn mergeFun [f & params]
    (loop [cols params ,result {}]
      (if (empty? cols)
        result
        (recur (rest cols)
          (let[vals (first cols),
               ks (keys vals)]
            (reduce
              (fn comput[r k]
                (let [val (get vals k),orgVal (get r k)]
                  (if (nil? orgVal)
                    (assoc r k val)
                    (assoc r k (f orgVal val) )
                    )
                  )
                )
              result
              ks
              )
            )
          )
        )
      )
    ))

(defcheck solution-fd45cc9
  (fn [f & xs]
    (->>
      xs
      (mapcat seq)
      (sort-by first)
      (partition-by first)
      (map (fn [es]
             [(ffirst es) (reduce f (map second es))]))
      (into {}))))

(defcheck solution-fdad1919
  (fn g
    ([f a x & xs] (apply g f (g f a x) xs))
    ([f a x]
     (reduce (fn [a [k v]]
               (apply assoc a
                 (if (a k) [k (f (a k) v)] [k v])))
       a x))))

(defcheck solution-fdf08a7a
  (fn[f & args]
    (reduce (fn [acc e]
              (into acc (map (fn [[k v]]
                               (if (nil? (get acc k))
                                 (vector k v)
                                 (vector k (f (get acc k) v)))) e))) args)))

(defcheck solution-fef4ff21
  (fn
    [f & ms]
    (loop  [f f ms ms r {}]
      (if (empty? ms)
        r
        (recur f
          (rest ms)
          (reduce #(assoc %1
                     (key %2)
                     (if-let [old-val (get %1 (key %2))]
                       (f old-val (val %2))
                       (val %2)))
            r
            (first ms)))))))

(defcheck solution-ff0903e7
  (fn [o m & s]
    (into m
      (for [x s [k v] x] [k (if (m k) (o (m k) v) v)]))))

(defcheck solution-ff46a369
  (fn my-merge-with
    ([f m1] m1)
    ([f m1 & maps]
     (reduce (fn [acc m]
               (reduce (fn [a [k v]]
                         (assoc a k (if (get a k)
                                      (f (get a k) v)
                                      v))) acc m))
       m1 maps))))

(defcheck solution-ff6958d7
  (fn conj-maps
    [f m & xs]
    (reduce (fn [result x]
              (loop [r result i x]
                (if (seq i)
                  (let [[key new-val] (first i)
                        current-val (get result key)
                        val (if current-val
                              (f current-val new-val)
                              new-val)]
                    (recur (assoc r key val)
                      (rest i)))
                  r))) m xs)))

(defcheck solution-ff793def
  (fn mf
    ([f x y]
     (reduce (fn [r e]
               (let [k  (first e)
                     v (r k)]
                 (if v
                   (conj r [k (f v (second e))])
                   (conj r [k (second e)]))))
       x y))
    ([f x y & args]
     (reduce (partial mf f) (mf f x y) args))))

(defcheck solution-fffb5fda
  (fn [op m & ms]
    (if
     (= (count ms) 0) m
                      (recur op
                        (into m
                          (map (fn [[k v]]
                                 (if (m k)
                                   [k (op (m k) v)]
                                   [k v]))
                            (first ms)))
                        (rest ms)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-69))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
