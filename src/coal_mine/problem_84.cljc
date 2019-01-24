(ns coal-mine.problem-84
  (:require [coal-mine.checks :refer [defcheck-84] :rename {defcheck-84 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10cf8d85
  (fn transitive-closure
    [aSet]
    (let [reachable-from (fn reachable-from
                           ([obj s reached]
                            (mapcat (fn
                                      [next-obj]
                                      (if (contains? reached next-obj)
                                        []
                                        (conj (reachable-from next-obj s (conj reached obj)) next-obj)))
                              (map second (filter #(= (first %) obj) s))))
                           ([obj s]
                            (reachable-from obj s #{})))]
      (into #{}
        (mapcat (fn
                  [item]
                  (map #(vector item %) (reachable-from item aSet)))
          (map first aSet))))))

(defcheck solution-113a714b
  (fn w [s]
    (let [n (set (mapcat
                   (fn [[x y]] (concat
                                [[x y]]
                                (keep (fn [[a b]] (if (= a y) [x b])) s)))
                   s))]
      (if (= s n) n (w n)))))

(defcheck solution-11937ff9
  (fn [b]
    (let [fb (fn [s]
               (set (filter #(not (or (nil? %) (contains? s %)))
                      (for [x s y s :when (not= x y)]
                        (if (= (last x) (first y))
                          [(first x) (last y)])))))
          nb (fb b)]
      (if (empty? nb)
        b
        (recur (clojure.set/union b nb))))))

(defcheck solution-11ade782
  (fn [pair-set]
    (letfn
     [(f1 [[layered ori]] (vector (into #{} (for [x layered y ori] (if (= (last x) (first y)) (conj x (last y)) x))) ori))
      (f2 [pair-set] (first (nth (iterate f1 [pair-set pair-set]) (count pair-set))))
      (f3 [lnk] (let [idx-lnk (map-indexed vector lnk)] (for [x idx-lnk y idx-lnk :when (< (first x) (first y))] (vector (last x) (last y))))) ]
      (into #{} (mapcat f3 (f2 pair-set))))))

(defcheck solution-11b590a7
  (letfn [(chains [links]
            (loop [chain (first links) r (disj links chain) res []]
              (let [f (first chain) l (peek chain)
                    [u v :as link] (some (fn [[u v :as c]] (when (or (= l u) (= v f)) c)) r)]
                (cond
                  link (recur (if (= l u) (conj chain v) (into [u] chain)) (disj r link) res)
                  (not-empty r) (recur (first r) (disj r (first r)) (conj res chain))
                  :default (conj res chain)))))
          (unroll [chain]
            (loop [[f & r] chain res #{}]
              (if (not-empty r) (recur r (into res (map (fn [a] [f a]) r))) res)))]
    (fn [links]
      (reduce into #{} (map unroll (chains links))))))

(defcheck solution-11f2681f
  (let [f #(into % (for [[k1 v1] % [k2 v2] % :when (= v1 k2)] [k1 v2]))] (fn g [r] (if (= r (f r)) r (g (f r))))))

(defcheck solution-12250ad5
  (fn tc [se]
    (let [m (apply merge-with concat (map (fn [[k v]] {k [v]}) se))
          r ((fn %r [mm]
               (let [new (into {} (map (fn [[k v]] [k (set (concat v (mapcat (fn [arg] (get mm arg)) v)))]) mm))]
                 (if (= new mm) new
                                (%r new)))) m)]
      (set (mapcat (fn [[k v]] (map (fn [v] [k v]) v)) r)))))

(defcheck solution-1235f001
  (fn [e]
    (let [f (memoize (fn [a]
                       (set (for [[x y] e
                                  :when (= a x)]
                              y))))
          g (fn [v]
              (let [c (count v)
                    u (reduce into v (map f v))
                    k (count u)]
                (if (> k c)
                  (recur u)
                  u)))

          h (fn [[a b]]
              (for [c (g #{b})]
                [a c]))]

      (set (mapcat h e)))))

(defcheck solution-12538f6b
  (fn [pair-set]
    (let [pair-map (into {} pair-set)]
      (->> (map
             (fn [pair]
               (take-while
                 (fn [[_ v]] (not (nil? v)))
                 (iterate
                   (fn [[k v]]
                     (vector k (pair-map v)))
                   pair)))
             pair-map)
        (reduce concat)
        (set)))))

(defcheck solution-127dc18b
  (fn transitive-closure [binary-relations]
    (let [s (distinct (flatten (seq binary-relations)))
          size (count s)
          arr (make-array Long size size)]
      (doseq [[a b] binary-relations]
        (aset arr
          (.indexOf s a)
          (.indexOf s b)
          1))
      (dotimes [k size]
        (dotimes [i size]
          (dotimes [j size]
            (let [a (aget arr i j)
                  b (aget arr i k)
                  c (aget arr k j)]
              (when (or (and (not a) b c)
                        (and a b c (< a (+ b c))))
                (aset arr i j (+ b c)))))))
      (set (for [i (range size) j (range size)
                 :when (aget arr i j)]
             [(nth s i) (nth s j)])))))

(defcheck solution-1287a7d9
  (fn [coll]
    (letfn [(infer [coll]
              (set
                (concat coll
                        (filter (fn [a] a)
                          (for [[head1, tail1] coll
                                [head2, tail2] coll]
                            (if (= tail1 head2)
                              [head1, tail2]))))))]
      (loop [o coll, n (infer coll)]
        (if (= o n) n
                    (recur n (infer n)))))))

(defcheck solution-135393cd
  #(let [g (into {} %)]
     (set (for [k (keys g),
                p ((fn t [k]
                     (when-let [p (g k)]
                       (cons p (t p)))) k)]
            [k p]))))

(defcheck solution-13844e3a
  (fn tc [x]
    (let [new (into x (for [[a b] x, [c d] x :when (= b c)] [a d]))]
      (if (= (count new) (count x))
        new
        (recur new)))))

(defcheck solution-13b4d4e2
  (fn transitive-closure [s]
    (let [s2 (set (for [[a b] s [c d] s]
                    [a (if (= b c) d b)]))]
      (if (= s s2)
        s
        (transitive-closure s2)))))

(defcheck solution-13f4fcbb
  (fn [n]
    (let [
          nmp (apply hash-map (flatten (vec n)))
          build-chain  (fn [nx mp] (loop [lst [] ky nx ]
                                     (if (mp ky) (recur (cons ky lst) (mp ky))
                                                 (cons ky lst))))
          kys  (map key nmp)
          chains  (map #(build-chain (val %) nmp) nmp)]

      (reduce into #{} (map (fn [k l] (map #(vector k %) (reverse  l))) kys chains)))))

(defcheck solution-14316b89
  (fn [relation]
    (let [reachables (fn reachables [graph node]
                       (loop [[x & xs :as neighbors] (seq (get graph node)), result graph]
                         (if (empty? neighbors) result
                                                (let [new-graph (reachables graph x)]
                                                  (recur xs (assoc new-graph node
                                                                             (-> #{}
                                                                               (into (get new-graph node))
                                                                               (into (get new-graph x)))))))))
          graph
                     (reduce (fn [m [a b]] (update-in m [a] #(conj (or % #{}) b))) { } relation)
          result-graph
                     (reduce reachables graph (keys graph))]

      (reduce
        (fn [rel k]
          (reduce #(conj %1 (vector k %2)) rel (get result-graph k)))
        #{} (keys result-graph)))))

(defcheck solution-143542f1
  #(let [t (set
             (for [[a b] %
                   [x y] %
                   :when (or (= [a b] [x y]) (= b x))]
               [a y]))]
     (if (= % t) % (recur t))))

(defcheck solution-14655ead
  (fn [v]
    (let [m (reduce (fn [m [k kv]] (assoc m k (set (conj (m k) kv)))) {} v)
          to-vecs (fn [m] (reduce (fn [s [k kv]] (apply conj s (reduce #(conj %1 [k %2]) #{} kv))) #{} m))
          ]
      #_(println m)
      (to-vecs
        ((fn [m1 m2]
           #_(println "m1 =" m1)
           #_(println "m2 =" m2)
           (if (= m1 m2)
             (do #_(println "finished" m2)
                 m2)
             (recur m2
               (reduce
                 (fn [m [k kv]]
                   (reduce (fn [m v] (assoc m k (set (concat (m k) (m v))))) m kv)) m2 m2))))
         {} m)))))

(defcheck solution-1535a170
  (fn transitive-closure [rels]
    (let [nodes (set (flatten (vec rels)))]
      (set (mapcat (fn [nd]
                     (map (fn [p] [p nd])
                       (loop [to-check #{nd}, found #{}]
                         (let [parents (set (map first (filter rels (for [p nodes n to-check] [p n]))))]
                           (if (empty? parents) found
                                                (recur parents (into found parents)))))))
             nodes)))))

(defcheck solution-1548e1d4
  (fn [rel]
    (letfn [(exp [r]
              (let [m (into {} r)]
                (->> (concat
                      r
                      (for [[k v] m]
                        (when-let [nv (m v)] [k nv])))
                  (filter identity)
                  (set))))
            (ft [p [f & rs]]
              (when rs
                (if (p f (first rs))
                  f
                  (recur p rs))))]
      (ft = (iterate exp rel)))))

(defcheck solution-15f22311
  (fn gtc [s]
    (let [find-relations (fn [p s]
                           (filter
                             (fn [x]
                               (or (= (x 0) (p 1))
                                   (= (x 1) (p 0))))
                             s
                             ))
          ctc				 (fn [p1 p2]
                        (if (=( p1 0) (p2 1))
                          [(p2 0) (p1 1)]
                          [(p1 0) (p2 1)]
                          ))
          tc				 (fn [p s]
                       (loop [s1 s result []]
                         (if (not-empty s1)
                           (recur
                             (rest s1)
                             (conj result (ctc p (first s1)))
                             )
                           result)
                         ))
          rtc				 (fn [s]
                        (loop [p (first s) r (rest s) result #{}]
                          (if (not-empty r)
                            (let [rels (find-relations p r)
                                  tcr  (tc p rels)]
                              (recur
                                (first r)
                                (rest r)
                                (if (empty? tcr)
                                  (conj result p)
                                  (into (conj result p) tcr)
                                  ))
                              )
                            (conj result p)
                            )))]
      (loop [n 0, result (rtc s)]
        (if (= n (-> s count dec))
          result
          (recur (inc n) (into result (rtc result)))
          ))
      )))

(defcheck solution-162efff3
  (fn f [r]
    (let [s (into r
              (for [[x y] r [u w] r
                    :when (= y u)] [x w]))]
      (if (= s r)
        s (f s)))))

(defcheck solution-165e6ff0
  (fn trans [rel]
    (let [rels (into {} rel)
          ks (keys rels)]
      (loop [from (first ks)
             cur from
             rk (rest ks)
             res #{}]
        (cond
          (contains? rels cur) (let [nxt (rels cur)]
                                 (recur from nxt rk (conj res [from nxt])))
          (not (empty? rk)) (let [nxt (first rk)]
                              (recur nxt nxt (rest rk) res))
          :else res)))))

(defcheck solution-169261b7
  (fn [s]
    (loop [edges s
           lastIter #{}]
      (if (= edges lastIter) edges
                             (recur (into edges (mapcat (fn [[a b]] (mapcat  (fn [[x y]] (if (= b x) [[a y]] [])) edges)) edges)) edges)))))

(defcheck solution-180b2a15
  (fn warshall [relation]
    "Computes the transitive closure of a binary relation."
    (letfn [(extension [pairs]
              (for [[a b] pairs
                    [c d] pairs
                    :when (= b c)]
                [a d]))

            (add-extension [pairs]
              (apply conj
                pairs
                (extension pairs)))]

      (loop [curr relation]
        (let [nxt (add-extension curr)]
          (if (= curr nxt)
            curr
            (recur nxt)))))))

(defcheck solution-19e31775
  (fn [a]
    (letfn [(gen [c]
              (let [rr (reduce
                         (fn [r e]
                           (assoc r (first e)
                                    (reduce #(set (concat %1 (get c %2))) (second e) (second e)))) {} c)]
                (if (= rr c) rr (gen rr))
                ))]
      (reduce (fn [r e] (set (concat r (reduce #(conj %1 [(first e) %2]) [] (second e))))) []
        (gen (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) #{}) (second %2))) {} a)))
      )))

(defcheck solution-1a10c226
  (fn [hs]
    (let [tree (apply merge-with concat
                 (map (fn [[a b]] {a [b]}) hs))]
      (->>
        (for [k (keys tree)]
          (loop [hs (set (tree k))]
            (let [n-hs (into hs (mapcat tree hs))]
              (if (= n-hs hs)
                (map (fn [%] [k %]) n-hs)
                (recur n-hs)))))
        (apply concat)
        set))))

(defcheck solution-1a1dda94
  (fn transitive [relations]
    (letfn [(parents [val] (map first (filter #(= val (second %)) relations)))
            (merge [mp from to] (assoc mp to (clojure.set/union #{from} (mp to #{}) (mp from #{}))))
            (unfold [[a b]] (map #(vector a %) b))
            (go-up [res val] (->> (parents val)
                               (reduce #(go-up (merge % val %2) %2) res)))]
      (->> (reduce into #{} relations)
        (reduce go-up {})
        (map unfold)
        (reduce into #{})))))

(defcheck solution-1a59b0f4
  (fn [pairs]
    (let [to-kw   (into {} (map #(vector % (keyword (str *ns*) (str %))) (flatten (seq pairs))))
          from-kw (into {} (map #(vector (keyword (str *ns*) (str %)) %) (flatten (seq pairs))))
          hier (reduce (fn [h [p c]] (derive h (to-kw p) (to-kw c))) (make-hierarchy) pairs)
          res  (reduce (fn [s [k v]] (apply conj s (map #(vector (from-kw k) (from-kw %)) v))) #{} (:ancestors hier))]
      res)))

(defcheck solution-1a68bf01
  (fn [s]
    (letfn [(t [[a b :as v] s]
              (let [f (fn [[u d]] (filter (fn [[x y]] (= d x)) s))
                    d (f v)
                    o (concat d (mapcat f d))]
                (map (fn [[x y]] [a y]) o)))]
      (into s (mapcat #(t % s) s)))))

(defcheck solution-1a71ca7e
  (fn [s]
    (let [m (apply hash-map (flatten (seq s)))
          f (fn [[v1 v2]] (loop [acc () v2 v2] (if-let [v3 (get m v2)] (recur (conj acc [v1 v3]) v3) acc)))]
      (reduce #(into % (f %2)) s s))))

(defcheck solution-1b434f1d
  (fn transitive [s]
    (let [m (into {} s)
          s' (into s (keep (fn [[k v]] (when (m v) [k (m v)])) m))]
      (if (= s s')
        s
        (recur s'))
      )))

(defcheck solution-1c98b822
  (fn [rs]
    (let [x (apply hash-map (flatten (vec rs)))
          x' (fn [[a b]] (if-let [b' (x b)] [a b']))]
      (loop [rs rs]
        (let [rs' (into rs (keep x' rs))]
          (if (= rs rs') rs
                         (recur rs')))))))

(defcheck solution-1d02e737
  (letfn [(transitive-from [[from to] relation]
            (set
              (into (list [from to])
                (mapcat (fn [[f t]]
                          (transitive-from [from t]
                            (disj relation [f t])))
                  (filter
                    (fn [[f t]]
                      (= to f))
                    relation)))))]
    (fn [relation]
      (set
        (mapcat (fn [v]
                  (transitive-from v
                    (disj relation v)))
          relation)))))

(defcheck solution-1d815ae6
  #(loop [[[x y :as q] & r] (seq %) c %]
     (if q
       (recur (concat r (for [[w z] % :when (= y w)] [x z])) (conj c q))
       c)))

(defcheck solution-1dab4319
  (fn create-trans [colls]
    (let [trans (reduce (fn [x [a b]]
                          (set (concat x (set (map #(vector a (second %)) (filter #(= b (first %))  colls))))))
                  colls colls)]
      (if (= colls trans)
        trans
        (create-trans trans)))))

(defcheck solution-1dc26299
  (fn [s]
    (reduce #(set (remove nil? (concat (conj % %2) (map (fn [x] (cond (= (first x) (second %2)) (vector (first %2) (second x))
                                                                      (= (second x) (first %2)) (vector (first x) (second %2)))) %)))) #{(first s)} (rest s))))

(defcheck solution-1e1f8132
  #(loop [r %]
     (let [d (into r (for [[x y] r [p q] r :when (= y p)] [x q]))]
       (if (= r d) r (recur d)))))

(defcheck solution-1e49df50
  (fn trans-close [xs]
    (letfn [(fixed-point [f x]
              (->> (iterate f x)
                (partition 2)
                (drop-while (fn [[a b ]] (not= a b)))
                first
                first))
            (trans-step [m]
              (apply merge
                (for [[k v] m]
                  {k (->> v
                       (filter m)
                       (map m)
                       (apply clojure.set/union v))})))
            (trans-map [xs]
              (->> xs
                (group-by first)
                (map (fn [[x y]]
                       {x (set (map second y))}))
                (apply merge)))
            (trans-pairs [m]
              (->> m
                (map (fn [[k v]]
                       (map (comp vec (partial list k)) v)))
                (apply concat)))]
      (->> xs
        trans-map
        (fixed-point trans-step)
        trans-pairs
        set))))

(defcheck solution-1f32754c
  (fn [vs]
    (letfn [
            (son? [p [s _]]
              (= p s))
            (sons [[_ s]]
              (cons
                s
                (flatten
                  (map
                    sons
                    (filter
                      (partial son? s)
                      vs)))))
            (r [[p _ :as v]]
              (map
                (partial vector p)
                (sons v)))
            ]
      (set
        (apply
          concat
          (map
            r
            vs)))
      )))

(defcheck solution-1f608fa8
  (fn q84 [s]
    (letfn [
            (edges [m [a b]]
              (conj m [a (conj (if (contains? m a) (m a) #{}) b)]))

            (to-set [m]
              (reduce
                (fn [s [k v]]
                  (reduce #(into % [[k %2]]) s v))
                #{}
                m))

            (found [m k]
              (let [s (m k)]
                (if (nil? s) nil
                             (reduce #(into % (found m %2)) s s))))
            ]
      (let [ nodes (reduce edges {} s) ]
        (to-set (reduce #(conj % [%2 (found nodes %2)]) {} (keys nodes)))))))

(defcheck solution-200b2cc3
  (fn a [m]
    (loop [ms m c 0]
      (if (= (count ms) c)
        ms
        (let [nms (for [[x1 x2] ms
                        [y1 y2] ms
                        :when (= x2 y1)]
                    [x1 y2])
              res (into ms nms)]
          (recur res (count ms)))))))

(defcheck solution-209d855f
  (fn t-closure' [xss]
    (letfn [(match-fn [item all]
              (filter #(= (last item)
                         (first %))
                (clojure.set/difference all #{item})))
            (pair-fn [target matches]
              (when (seq matches)
                (map (fn [i] [(first target) (last i)])
                  matches)))
            (reduce-fn [pairs]
              (reduce (fn [accum item]
                        (let [matches (match-fn item pairs)
                              paired (pair-fn item matches)]
                          (if (seq paired)
                            (apply conj accum paired)
                            accum))) [] pairs))
            (builder [pairs]
              (-> pairs
                reduce-fn
                (concat pairs)
                set))]
      (loop [yss xss]
        (let [new-pairs (builder yss)]
          (if (= (count yss) (count new-pairs))
            yss
            (recur new-pairs)))))))

(defcheck solution-213b7e59
  (fn trans-close [raw-pairs]
    (let [mapping (into {} raw-pairs)
          branch? #(mapping (last %))
          children (fn [[f l]] (list [f (mapping l)]))]
      (into #{} (filter #((complement =) (first %) (last %) ) (apply concat (map #(tree-seq branch? children [% %]) (keys mapping))))))))

(defcheck solution-21593631
  (fn [i] (let [g (group-by first i)] (set (filter last (mapcat #(loop [j % result #{}] (if (nil? j) result (recur (last (first (g j))) (concat result [[% (last (first (g j)))]])))) (keys g)))))))

(defcheck solution-22f349c0
  (fn [e]
    (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))]
      (let [e2 (set (f e))]
        (if (clojure.set/subset? e2 e)
          e
          (recur (clojure.set/union e e2)))))))

(defcheck solution-237e7da8
  (fn f[x]
    (let
     [ g (into {} x )
      y
         (reduce
           (fn [z [a b]]
             (if (contains? g b) (into z [[a (g b)]]) z))
           x
           x)]
      (if (= x y) x (recur y)))))

(defcheck solution-23b6a07b
  (fn [-set]
    (let [items (reduce (fn [a [b c]] (assoc a b c) )  {}  -set)
          xx (fn [k] (items k))]
      (reduce  #(into % (let [[a & more] (take-while (complement nil?) (iterate xx (key %2)))]
                          (map (partial vector a) more) )) #{}  items)
      )))

(defcheck solution-249a3543
  (fn transitive-clojure [s]
    (let
     [map-of-rel
      (reduce
        (fn [m [a b]]
          (merge-with clojure.set/union m {a #{b}}))
        {}
        s)]
      (into
        #{}
        (mapcat
          (fn [[a bs]]
            (map #(vector a %1) bs)
            )
          (for [map-key (keys map-of-rel)]
            (loop [map-val (map-of-rel map-key) ]
              (let [len (count map-val)
                    transitives (apply clojure.set/union map-val (map map-of-rel map-val))
                    tr-len (count transitives)
                    ]
                (if (= len tr-len)
                  [map-key transitives]
                  (recur transitives)
                  )
                ))
            )))
      )
    ))

(defcheck solution-24dacee3
  (letfn
   [(f [[o e] xs] (for [[x y] xs :when (= e x)] [o y]))
    (g [xs] (for [x xs] (f x xs)))
    (p [xs] (set (apply concat xs (g xs))))]

    (fn fix [xs]
      (let [ys (p xs)]
        (if (= ys xs)
          ys
          (recur ys))))))

(defcheck solution-24dfa661
  (fn tc
    ([s] (tc s []))
    ([s acc]
     (if (empty? s) (set acc)
                    (let [[k v] (first s)
                          with-new (conj acc [k v])
                          exp-keys (->> with-new
                                     (filter #(= k (second %)))
                                     (map first)
                                     (concat [k]))
                          exp-vals (->> with-new
                                     (filter #(= v (first %)))
                                     (map second)
                                     (concat [v]))
                          expansion (for [xk exp-keys
                                          xv exp-vals]
                                      [xk xv])]
                      (tc
                        (rest s)
                        (into with-new expansion)))))))

(defcheck solution-24e6e4d4
  (fn [s]
    (letfn [(combin-edges [edges [ds de]]
              (reduce
                (fn [result [ss se]]
                  (cond
                    (= ss de)
                    (combin-edges (conj result [ds se]) [ds se])
                    (= se ds)
                    (combin-edges (conj result [ss de]) [ss de])
                    :else result))
                edges
                edges))]
      (reduce
        #(combin-edges (conj %1 %2) %2)
        #{}
        s))))

(defcheck solution-2529c77c
  (fn [r]
    (let [update (fn [s]
                   (for [[x1 y1] s [x2 y2] s :when (= y1 x2)] [x1 y2]))]
      (loop [acc r
             new (into acc (update acc))]
        (if (every? acc new) acc
                             (recur new (into new (update new))))))))

(defcheck solution-2593e497
  (fn [xs]
    (letfn[
           (find-trans[x]
             (let [found (first (filter #(= (second x) (first %)) xs))]
               (distinct (concat x (when found (lazy-seq (find-trans found)))))))
           (find-rels
             ([[y & ys]] (find-rels y ys))
             ([y ys] (if (empty? ys) []
                                     (cons [y (first ys)] (lazy-seq (find-rels y (rest ys)))))))]

      (set
        (reduce concat
          (map find-rels
            (map find-trans xs)))))))

(defcheck solution-269af503
  (fn [S]
    (set (loop [p (seq S) q (map (fn [u] [u #{}]) S) h () ]
           (let [t (atom [])
                 in? (fn [l u] (some #(= u (first %)) l))
                 c (fn [u v] (if (or (some #(= u %) p) (in? q u) (in? @t u))
                               nil
                               (swap! t conj [u v] ) ))]
             (doall (for [i p
                          j q
                          :let [x i, [y z] j]
                          :when (and (not= x y) (not (z i)) ) ]
                      (cond (= (first x) (second y)) (c [(first y) (second x)] #{x y})
                            (= (first y) (second x)) (c [(first x) (second y)] #{x y})
                            :else nil)) )
             (if (not-empty @t)
               (recur (concat p h) @t (map first @t))
               (concat p h)))))))

(defcheck solution-26f76c75
  (fn transitive-closure [base-relations]
    (let [x-relations (fn x-relations [current-element base-relations]
                        (let [direct-relatives (->> base-relations
                                                 (filter #(= current-element (first %)))
                                                 (map second))
                              indirect-relatives (mapcat #(x-relations % base-relations) direct-relatives)]
                          (concat direct-relatives indirect-relatives)))
          transitive-relations (fn [current-element base-relations]
                                 (map #(vector %1 %2)
                                   (repeat current-element)
                                   (x-relations current-element base-relations)))
          left-elements (map #(first %) base-relations)]
      (into #{} (mapcat #(transitive-relations % base-relations) left-elements)))))

(defcheck solution-276b6c83
  (fn [s]
    (set
      (reduce
        (fn [s [a b]]
          (concat s (keep (fn [[c d]] (if (= b c) [a d])) s)))
        s s))))

(defcheck solution-2784f4c2
  (fn [in-set]
    (reduce
      (fn [acc [start stop :as tuple]]
        (let [ news (reduce (fn [a2 [s1 s2 :as t]]
                              (cond (= start s2) (conj a2 [ s1 stop])
                                    (= s1 stop)  (conj a2 [ start s2])
                                    :default  a2
                                    )
                              )
                      [tuple]
                      acc) ]
          (reduce (fn [a3 t] (conj a3 t)) acc news)))
      #{}
      in-set)))

(defcheck solution-2793c4e1
  (fn g [s]
    (let [f first l last
          e (into s
              (for [x s
                    y s
                    :when (= (f x) (l y))]
                [(f y) (l x)]))]
      (if (= e s) s (g e)))))

(defcheck solution-28487d15
  (fn [s]
    (letfn
     [(addedge [s [a b]]
        (let
         [s1
             (reduce
               (fn [s [c d]]
                 (if (= d a)
                   (conj s [c b])
                   s))
               s
               s)
          s2 (reduce
               (fn [s [c d]]
                 (if (= c b)
                   (conj s [a d])
                   s))
               s1
               s1)]
          (conj s2 [a b])))]
      (reduce addedge #{} s))))

(defcheck solution-29b500e7
  (fn transitive-closure [closure]
    (let [new-closure (reduce clojure.set/union closure (map (fn [[a b]]
                                                               (set (map #(vector a (last %)) (filter #(= (first %) b) closure)))
                                                               ) closure))]
      (if (= new-closure closure)
        closure
        (recur new-closure)
        )
      )
    ))

(defcheck solution-29be4199
  (fn [s] (let [r (fn f [l e] (reduce #(let [a [(first e) (last %2)]] (if (= %2 e) (conj % %2) (if (= (last e) (first %2)) (conj (f % a) a) %))) l s))] (reduce r #{} s))))

(defcheck solution-2a726a77
  (fn [a-set]
    (let [gen-tr (fn [a-set]
                   (for [x a-set
                         y a-set]
                     (let [[x1 x2] x
                           [y1 y2] y]
                       (if (= x2 y1)
                         [x1 y2]))))
          tr-cl (fn [a-set]
                  (-> a-set
                    (into (gen-tr a-set))
                    (disj nil)))]
      (if (=
            (tr-cl a-set)
            (tr-cl (tr-cl a-set)))
        (tr-cl a-set)
        (recur (tr-cl a-set))))))

(defcheck solution-2a8109d7
  (fn [br]
    (let [m (into {} br)]
      (letfn [(bs [k] (if (m k) (cons [k (m k)] (bs (m k)))))]
        (set (apply concat (for [[a b] br] (map #(list a (second %)) (bs a)))))))))

(defcheck solution-2b2795ab
  (fn [s]
    (let [f (fn [s1 [a b :as c]]
              (let [m1 (map #(vector (first %) b)  (filter #(= (second %) a) s1))
                    m2 (map #(vector a (second %))(filter #(= (first %) b) s1))]
                (-> s1
                  (into m1)
                  (into m2)
                  (conj c))))]
      (reduce f #{} s))))

(defcheck solution-2c058816
  (fn [orig]  (letfn [
                      (traverse [ head  tail  edges ]
                        (if (empty? edges) #{}
                                           (let [nexts (filter #(= tail (first %)) edges)]
                                             (apply clojure.set/union
                                               (map #(conj (traverse head (last %) (disj edges %)) [head (last %)]) nexts))))) ]

                (clojure.set/union orig
                  (apply clojure.set/union
                    (map #(traverse (first %) (last %) (disj orig %)) orig))))
    ))

(defcheck solution-2c33d490
  (fn [b]
    (into #{}
      (mapcat (fn [e]
                (let [b (cons e (disj b e))]
                  ((fn f [b]
                     (when-let [z (first b)]
                       (cons z
                         (let [s (first (filter #(= (z 1) (% 0)) b))
                               o [(z 0) (get s 1)]]
                           (if s
                             (cons o (f (cons o (rest b))))
                             (f (rest b))))))) b))) b))))

(defcheck solution-2ea2f00
  (fn tc [rules-as-set]
    (let [rules (map (fn [[a b]]
                       (fn [[m n]] (if (= n a) [m b] [a b]))) rules-as-set)]
      (loop [tuples rules-as-set]
        (let [new-tuples (set (mapcat (fn [tuple] (map #(% tuple) rules)) tuples))]
          (if (= (count new-tuples) (count tuples))
            new-tuples
            (recur new-tuples)))))))

(defcheck solution-2efe44be
  (fn [r]
    (->>
      r
      (iterate #(into % (for [[x y] % [y1 z] % :when (= y y1)] [x z])))
      (partition 2)
      (filter #(apply = %))
      first first)))

(defcheck solution-2f1cb17b
  (fn [relations]
    (let [rel-map (apply merge-with concat (for [[k v] relations] (assoc {} k (list v))))
          rec (fn rec [k v]
                (apply concat
                  (for [val (rel-map v)]
                    (conj (rec k val) [k val]))))]
      (set (mapcat #(rec % %) (keys rel-map))))))

(defcheck solution-2f35eadb
  (fn transclosure
    [coll]
    (let [new-coll (into coll
                     (reduce (fn [acc [start end]]
                               (into acc
                                 (keep (fn [[start' end']]
                                         (when (= end start')
                                           [start end']))
                                   coll)))
                       #{}
                       coll))]
      ; A brute-force solution for a more civilized age.
      ; Might get stuck in an infinite loop. Great!
      (if (= new-coll coll)
        new-coll
        (recur new-coll)))))

(defcheck solution-307074d2
  (fn [rel]
    (let [roots (into {} (for [[k v] (group-by first rel)] [k (mapv second v)]))
          children (fn children [rels e]
                     (let [cs (get rels e [])]
                       (cons e (mapcat #(children rels %) cs))))]
      (set (mapcat #(map vector (repeat %) (rest (children roots %))) (keys roots))))))

(defcheck solution-3074c5c2
  (fn puzzle-84 [s]
    (letfn [(close-set [s]
              (set
                (remove nil?
                  (for [a s b s :when (not (identical? a b))]
                    (let [[a1 a2] a
                          [b1 b2] b
                          el [a1 b2]]
                      (if (and
                           (= a2 b1)
                           (not (contains? s el)))
                        el))))))]
      (loop [s s]
        (let [cls (close-set s)]
          (if (empty? cls) s
                           (recur (clojure.set/union s cls))))))))

(defcheck solution-30ad9167
  #(reduce (fn [r [a b]] (into r (for [[c d] r :when (= b c)] [a d]))) % %))

(defcheck solution-31702dd3
  (fn [s]
    (let [follows (fn [k m]
                    (loop [i k
                           m m
                           acc []]
                      (let [v (get m i)]
                        (if v
                          (recur v m (conj acc v))
                          acc))))
          rels (into {} s)]
      (set (mapcat #(map vector (repeat %) (follows % rels)) (keys rels))))))

(defcheck solution-31a2ec07
  (fn transitive-closure [coll]
    (loop [counter (count coll)
           acc coll]
      (if (= 0 counter)
        acc
        (recur
          (dec counter)
          (set
            (concat acc (remove nil?
                          (for [i acc
                                j acc]
                            (if (= (last i) (first j))
                              [(first i) (last j)]))))))))))

(defcheck solution-325c4c20
  (fn [rs]
    (let [as (map first rs)
          bs (map second rs)
          fwd (zipmap as bs)
          rev (zipmap bs as)
          roots (filter (complement rev) as)
          follow-fwd (fn [root]
                       (loop [root root, acc [root]]
                         (if-let [x (fwd root)]
                           (recur x (conj acc x))
                           acc)))
          chains (map follow-fwd roots)
          enum-relationships (fn [chain]
                               (for [[i x] (butlast (map-indexed vector chain))
                                     o (subvec chain (inc i))]
                                 [x o]))]
      (set (mapcat enum-relationships chains)))))

(defcheck solution-329fca79
  (fn [s]
    (let [f (fn [s]
              (some
                (fn [v]
                  (let [rel (filter
                              #(and (= (second v) (first %))
                                    (not (s (list (first v) (second %))))) s)]
                    (if (empty? rel)
                      nil
                      (list (first v) (second (first rel))))))
                s))]
      (let [rel (f s)]
        (if (nil? rel)
          s
          (recur (conj s rel)))))))

(defcheck solution-33554998
  (fn trans-closure [s]
    (let [sn (clojure.set/union s (set (for [[a b] s [c d] s :when (= b c)] [a d])))]
      (if (= s sn) s (recur sn)))))

(defcheck solution-33f2b2b0
  (fn [s]
    (letfn [(relate [c1 c2]
              (cond
                (= (first c1) (second c2)) [(first c2) (second c1)]
                (= (first c2) (second c1)) [(first c1) (second c2)]
                :else c1))]
      (loop [p s r s f false]
        (if (empty? p)
          (if f r (recur r r true))
          (recur (rest p)
            (reduce conj r
              (for [x (rest p)]
                (relate (first p) x)))
            f))))))

(defcheck solution-34ab648a
  (fn [xs]
    (set
      (reduce
        (fn [rs r]
          (concat
           (conj rs r)
           (map #(vector (first r) (last %)) (filter #(= (last r) (first %)) rs))
           (map #(vector (first %) (last r)) (filter #(= (first r) (last %)) rs))
           )
          )
        #{}
        xs
        )
      )
    ))

(defcheck solution-35188157
  (fn [rels]
    (let [by-second (group-by first rels)
          ext-rels (into rels (mapcat (fn [[f s]] (for [[_ s2] (by-second s)] [f s2])) rels))]
      (if (= (count rels) (count ext-rels))
        ext-rels
        (recur ext-rels)))))

(defcheck solution-355f4c44
  (fn transitive-closure [R]
    (let [elements (set (reduce concat R))
          R+ (set (remove R (for [s1 elements s2 elements s3 elements :when (and (R [s1 s2]) (R [s2 s3]))] [s1 s3])))]
      (if (empty? R+) R (recur (into R R+))))))

(defcheck solution-36c799a
  (fn [r]
    (let [n (set (concat r (for [[a b] r [c d] r :when (= b c)] [a d])))]
      (if (= r n) r (recur n)))))

(defcheck solution-370ffdfc
  (fn
    f
    ([s] (f s s s))
    ([rs s result]
     (if
      (empty? rs)
       result
       (let [ffilter #(filter (fn [y] (= (first y) (second (first rs)))) %)
             fmap #(map
                     (fn [x] (vector (ffirst rs) (second x))) %)
             filtered (ffilter s)
             mapped (fmap filtered)]
         (recur
           (if (= (count (ffilter filtered)) 0)
             (rest rs)
             (into (rest rs) mapped))
           s
           (into
             result
             mapped)))))))

(defcheck solution-37b51802
  (fn update-transitive [x]
    (letfn [(update-transitive-inc [x]
              (reduce
                (fn [m [a b]]
                  (update-in m [a] (fn [old] (into old (apply concat (map m b))))))
                x
                x))]
      (let [
            x-as-map (apply hash-map (flatten (seq (map (fn [[a b]] [a (hash-set b)]) x))))
            consecutive-updates (iterate update-transitive-inc x-as-map)
            pairs (partition 2 1 consecutive-updates)
            result-map (first (first (drop-while (fn [[a b]] (not= a b)) pairs)))]
        (reduce
          (fn [old [key value-set]]
            (into
              old
              (map (fn [value] [key value]) value-set)))
          #{}
          result-map)))))

(defcheck solution-386a3af8
  (fn transit
    [relations]
    (let [key-map (reduce #(assoc %1 (first %2) (conj (or (get %1 (first %2)) []) (second %2))) {} relations)
          get-children (fn get-children [key-map ky]
                         (flatten (let [vals (get key-map ky)
                                        children (concat vals (map #(get-children key-map %) vals))]
                                    children)))]
      (into #{} (for [x (keys key-map)
                      child (get-children key-map x)]
                  [x child])))))

(defcheck solution-38914761
  (fn [r]
    (letfn [(related? [brel1 brel2] (= (last brel1) (first brel2)))
            (add-rel [brel]
              (let [addthing (for [r1 brel
                                   r2 brel
                                   :when (related? r1 r2)]
                               [(first r1) (last r2)])]
                (set (concat brel addthing))))
            (add-all-rels [brel]
              (if (= (add-rel brel) brel)
                brel
                (recur (add-rel brel))))]
      (add-all-rels r))))

(defcheck solution-38a90310
  (fn [rel]
    (->> rel
      (reduce
        #(assoc % (first %2)
                  (concat
                   (get % (first %2))
                   (get % (second %2))))
        (into {} (map (fn [[a b]] [a [b]]) rel)))
      (mapcat (fn [[k vals]]  (map #(list k %) vals)))
      (into #{}))))

(defcheck solution-392a9944
  (fn [s]
    (letfn [(generate-transitive-closures [v]
              (let [idx (zipmap v (range))]
                (for [a v b v :when (< (idx a) (idx b))]
                  [a b])))
            (glue-relations [r1 r2]
              (cond
                (= (last r1) (first r2)) (into r1 (rest r2))
                (= (last r2) (first r1)) (into r2 (rest r1))
                :else r1))
            (build-relation-vectors [rel]
              (reduce glue-relations rel s))]
      (reduce #(into % (generate-transitive-closures %2)) #{} (set (map build-relation-vectors s))))))

(defcheck solution-39405b4f
  (fn [coll]
    (let [fs (set (map first coll))
          ls (set (map last coll))
          roots (clojure.set/difference fs ls)
          children (fn [elt coll] (set (map last (filter #(= elt (first %)) coll))))
          tree (fn tr [rt coll]
                 (let [c (children rt coll)]
                   (if (empty? c)
                     [rt]
                     [rt (vec (mapcat #(tr % coll) c))])))
          trees (map #(tree % coll) roots)
          pairs (fn ps [[rt ch]]
                  (if (empty? ch)
                    nil
                    (concat (map #(vector rt %) (flatten ch)) (ps ch))))]
      (set (mapcat pairs trees)))))

(defcheck solution-39779bbe
  (fn [s]
    (let [f #(for [[a b] (seq %)
                   [c d] (seq %)
                   :when (= c b)] [a d])]
      (->> s (iterate #(let [n (into % (f %))]
                         (when (not= % n) n)))
        (take-while identity)
        (last)))))

(defcheck solution-39c1780c
  (fn [s]
    (nth (iterate (fn [s]
                    (reduce
                      into
                      #{}
                      (for [[k1 v1] s
                            [k2 v2] s]
                        (if (= v1 k2)
                          (reduce into
                            (list #{[k1 v1]} #{[k2 v2]} #{[k1 v2]}))
                          (conj #{[k1 v1]} [k2 v2])))))
           s) (count s))))

(defcheck solution-3a9fef12
  (fn [coll]
    (let [n (set (concat coll
                         (mapcat (fn [[x1 x2]]
                                   (for [[y1 y2] coll
                                         :when (= y2 x1)]
                                     [y1 x2]))
                           coll)))]
      (if (= n coll)
        coll
        (recur n)))))

(defcheck solution-3abebf57
  (fn trans-closure [br]
    (letfn [(follow [m]
              (let [new (merge-with (fn [a b] (set (mapcat #(if-let [v (m %)] (set (concat b v)) (set b)) b))) m m)]
                (if (= new m) m (follow new))))]
      (set (mapcat (fn [[k v]] (map (fn [b] [k b]) v))
             (follow (reduce (fn [m [k v]] (assoc m k #{v})) {} br)))))))

(defcheck solution-3b24559b
  (fn solution
    [links]
    "Generates the transitive closure of a binary relation.
   The relation will be represented as a set of 2 item vectors"
    (let [prepend   (fn [coll x] (into [x] coll))
          append    (fn [coll x] (conj coll x))
          pairs-helper (fn pairs-helper [acc x coll]
                         (if (or (nil? x)
                                 (empty? coll))
                           acc
                           (pairs-helper (concat acc (for [y coll] [x y]))
                             (first coll)
                             (rest coll))))
          pairs     (fn pairs [coll]
                      (pairs-helper [] (first coll) (rest coll)))
          addlink   (fn [chains link]
                      "Takes a link and adds it to one of the chains.
                    Starts a new chain if appropriate chain not found"
                      (let [[from to]  link
                            match  (count (filter #(or (= from (last %))
                                                       (= to (first %))) chains))
                            rc     (if (zero? match)
                                     (conj chains link)
                                     (map (fn [coll]
                                            (cond (= from (last coll)) (append coll to)
                                                  (= to   (first coll)) (prepend coll from)
                                                  :else coll)) chains))]
                        rc))
          rc1       (reduce addlink [] links)
          rc2       (set (reduce concat [] (map pairs rc1)))]
      rc2)))

(defcheck solution-3b2dc085
  (fn [input]
    (letfn [(find-next-gen [mset k]     ; Return list of values that match key
              (let [found (filter #(= (first %) k) mset)]
                (when (seq found)
                  (map second found))))]
      (loop [m input]
        ;; for each key/value pair, find key/(list of (value of value))
        (let [next-gen (filter second
                         (map #(vector (first %) (find-next-gen m (second %))) m))
              ;; next-gen contains ([key1 (val1 val2)] [key2 (val2 val3)])
              next-gen-expanded (for [item next-gen
                                      :let [k (first item)]
                                      v (second item)]
                                  (vector k v))
              merged (into #{} (concat m next-gen-expanded))]
          (if (= m merged) m
                           (recur merged)))))))

(defcheck solution-3bae8291
  (fn transitive-closures [coll]
    (letfn [(fsecond [x] (-> x first second))
            (res-func [x y ncoll coll res]
              (let [fir (ffirst ncoll) sec (fsecond ncoll)]
                (if (= y fir) (concat res [[x sec]] (relation x sec (remove #{fir sec} coll))) res)
                ))
            (relation [x y coll]
              (loop [ncoll coll res ()]
                (if (empty? ncoll)
                  res
                  (recur (rest ncoll) (res-func x y ncoll coll res))))
              )]
      (->> coll
        (#(for [[a b] %] (relation a b (remove #{[a b]} %))))
        (apply concat)
        (concat coll)
        (into #{}))
      )
    ))

(defcheck solution-3bfaef23
  (fn f [r]
    (#(if (= % r) r (f %))
     (set (for [[a b] r [c d] r]
            [a (if (= b c) d b)])))))

(defcheck solution-3c560ffa
  (fn [xs]
    (reduce
      (fn [ret [x y]]
        (loop [one #{[x y]}, nret #{}, [h & t :as oret] (seq ret)]
          (cond
            (empty? oret)
            (into nret one)
            (= (h 1) x)
            (recur (conj one [(h 0) y]) (conj nret h) t)
            (= (h 0) y)
            (recur (conj one [x (h 1)]) (conj nret h) t)
            :else
            (recur one                       (conj nret h) t))))
      xs xs)))

(defcheck solution-3cd68ef9
  (fn f [r]
    (let [g (fn [e] (map second (filter #(= e (first %)) r)))
          rr (set (mapcat #(let [[k v] %]
                             (conj (map (fn [x] [k x]) (g v)) [k v])) r))]
      (if (= r rr) r (f rr)))))

(defcheck solution-3cf1dab0
  (fn f [r]
    (letfn [(p [r]
              (let [s (for [x r
                            y r
                            :when (= (second x) (first y))]
                        [(first x) (second y)])]
                (into r s)))]
      (let [s (p r)]
        (if (= r s)
          r
          (f s))))))

(defcheck solution-3cf66b16
  (fn me [arg]

    (let [ my-map (apply merge (map #(apply hash-map %) arg))

          new-items     (for [x my-map]
                          (if (nil? (my-map (val x)))
                            nil
                            (map vector (repeat (key x))

                              (take-while #(not= nil %) (iterate #(my-map %) (my-map (val x))))
                              )))
          new-set   (into #{} (apply concat new-items))]
      (clojure.set/union new-set arg))))

(defcheck solution-3d2aa022
  #(loop [coll %]
     (let [x (set (concat coll (for [[a b] coll [c d] coll :when (= b c)] [a d])))]
       (if (= x coll)
         x
         (recur x)))))

(defcheck solution-3d30020f
  (fn mapify [rels]
    (let
     [grouped (group-by first rels)
      pick-last (fn [lol] (map #(map last %) lol))
      rel-map (zipmap (keys grouped) (pick-last (vals grouped)))
      find-reachable (fn [given]
                       (let
                        [result (into given (mapcat rel-map given))]
                         (if (= result given)
                           given
                           (recur result))))
      fr (fn [x] (disj (find-reachable #{x}) x))]
      (into #{} (mapcat (fn [x]
                          (map (fn [y] [x y]) (fr x)))
                  (keys rel-map))))))

(defcheck solution-3d8a045d
  (fn [s]
    (let [g (reduce #(assoc %1 (%2 0) (conj (get %1 (%2 0) #{}) (%2 1))) {} s)
          a (fn [v q]
              (if (empty? q) v
                             (let [v (clojure.set/union v q)]
                               (recur v
                                 (clojure.set/difference (apply clojure.set/union (map g q)) v)))))]
      (into #{}
        (mapcat (fn [k] (map #(vector k %) (a #{} (g k)))) (keys g))))))

(defcheck solution-3dc2127a
  (fn transitive-closure [rels]
    (let [new-rels (into rels (for [[a b] rels [c d] rels :when (= b c)]
                                [a d]))]
      (if (= rels new-rels)
        rels
        (transitive-closure new-rels)))))

(defcheck solution-3dda1e7c
  (fn [R]
    (let [g (reduce
              (fn [m [k v]] (assoc m k (map second v)))
              {}
              (group-by first R))

          r (fn [n] (rest (tree-seq #(contains? g %) g n)))

          e (reduce (fn [m k] (assoc m k (r k))) {} (keys g))

          p (mapcat (fn [[a bs]] (map (partial vector a) bs)) e)]
      (set p))))

(defcheck solution-3f095d89
  (fn [edges]
    (let [edges-to-add (fn [edges]
                         (for [[u v] edges
                               [w x] edges
                               :when (= v w)]
                           [u x]))]
      (loop [p nil,
             e edges]
        (if (= e p)
          e
          (recur e (into e (edges-to-add e))))))))

(defcheck solution-3fa2a269
  (fn trans-closure [sset]
    (let [combina  (fn combination [col]
                     (reduce concat
                       (let [ctall (count col)]
                         (for [i (range 0 (dec ctall))]
                           (for [j (range (inc i) ctall)]
                             [(nth col i) (nth col j)])))))
          s2v      (fn [sc] (for [i sc] i))
          combdata (combina (s2v sset))
          trans    (fn [col1 col2]
                     (cond
                       (= (first  col1) (second col2)) [(first  col2) (second col1)]
                       (= (second col1) (first  col2)) [(first  col1) (second col2)]
                       :else nil))
          combdata2 (combina (s2v (clojure.set/union sset (set (filter #(not= nil %) (map #(trans (first %) (second %)) combdata))))))]
      (clojure.set/union sset (set (filter #(not= nil %) (map #(trans (first %) (second %)) combdata2)))))))

(defcheck solution-4009e40c
  (fn [rels]
    (letfn [(r [accum [f t]]
              (apply conj accum [f t]
                (concat
                 (for [[kf kt] accum
                       :when (= kt f)]
                   [kf t])
                 (for [[kf kt] accum
                       :when (= t kf)]
                   [f kt]))))]
      (reduce r #{} rels))))

(defcheck solution-4065810
  (fn [s] ((fn [old s] (if (= old s) s (recur s (clojure.set/union
                                                  s (set (for [[i j] s [k l] s :when (= j k)] [i l])))))) nil s)))

(defcheck solution-40cd5ba1
  (letfn [(trans [[a b] [c d]]
            (when (= b c)
              [a d]))
          (pass [rel rels]
            (reduce
              (fn [acc item]
                (if-let [new-rel (trans rel item)]
                  (conj acc new-rel)
                  acc))
              rels
              (seq rels)))
          (all-passes [rels]
            (into #{} (mapcat #(pass % rels) rels)))
          (trans-clo [rels]
            (let [new-rels (all-passes rels)]
              (if (= rels new-rels)
                new-rels
                (recur new-rels))))]
    trans-clo))

(defcheck solution-40fe0fe3
  (fn trans-closure [rel-set]
    (let [rel (into {} rel-set)
          f   #(loop [k %, acc nil]
                 (if-let [v (rel k)]
                   (recur v (conj acc [% v]))
                   acc))]
      (set (mapcat f (keys rel))))))

(defcheck solution-41a5ee72
  (fn tc [R]
    (letfn [(f [priors x R found]
              (if (empty? R) found
                  (let [reach (filter (fn [[a b]] (= a x)) R)
                        R-reduced (clojure.set/difference R (set reach))
                        extended (if (empty? priors)
                                   (list (list x))
                                   (map #(cons x %) priors)) ; add x to the path
                        ]
                    (if (seq reach)
                      (apply clojure.set/union (for [[_ y] reach]
                                                 (f extended y R-reduced
                                                    (clojure.set/union found
                                                                       (set (apply concat (for [p extended] (for [n p] (list n y)))))))))
                      found)
                    ))
              )]
      (apply clojure.set/union
             (for [x (set (for [[y _] R] y))]
               (f '() x R #{}))))))

(defcheck solution-42e040d2
  (fn [pairs]
    (letfn [
            (pack [pairs]
              (reduce (fn [accum [k v]] (merge-with clojure.set/union accum {k #{v}}))
                {} pairs))
            (unpack [m]
              (into #{} (for [[k vs] m, v vs] [k v])))
            (close [m]
              (into {}
                (map (fn [[k v]] [k (apply clojure.set/union v (vals (select-keys m v)))])
                  m)))
            (fixed-point [f arg]
              (->> (iterate f arg)
                (partition 2 1)
                (some (fn [[a b]] (when (= a b) a)))))]
      (->> pairs
        pack
        (fixed-point close)
        unpack))))

(defcheck solution-4307b046
  (fn tc [items]
    (let [next (clojure.set/union items (set (for [x items y items :when (= (last x) (first y))] [(first x) (last y)])))]
      (if (= items next)
        items
        (tc next)
        ))))

(defcheck solution-4403694d
  (fn p84 [ss]
    (letfn [(elem [e es] (if (empty? es) nil (if (= e (first es)) e (elem e (next es)))))
            (insa [e o es] (if (= e (first es)) (cons e (cons o (next es))) (cons (first es) (insa e o (next es)))))
            (insb [o e es] (if (= e (first es)) (cons o es) (cons (first es) (insb o e (next es)))))
            (remv [e es] (if (= e (first es)) (next es) (cons (first es) (remv e (next es)))))
            (addto [al coll]
              (let [fp coll]
                (cond (elem (first fp) al) [true (insa (first fp) (second fp) al)]
                      (elem (second fp) al) [true (insb (first fp) (second fp) al)]
                      true [false nil])))
            (addtos [alls coll]
              (if (empty? alls) (conj alls coll)
                                (let [[aret al] (addto (first alls) coll)]
                                  (if aret (conj (remv (first alls) alls) al)
                                           (cons (first alls) (addtos (next alls) coll))))))
            (cont [alls coll]
              (if (empty? coll) alls
                                (cont (addtos alls (first coll)) (next coll))))
            (eg [g]
              (if (= 1 (count g)) nil
                                  (concat (map (fn [b] [(first g) b]) (next g)) (eg (next g)))))
            ]
      (reduce (fn [a b] (reduce conj a b)) #{} (map (partial reduce conj #{})  (map eg (cont #{} ss)) )))))

(defcheck solution-443cb24d
  (fn [s]
    (let [m (into {} s)]
      (loop [s s]
        (let [s2 (clojure.set/union s (set (remove #(-> % second nil?) (map vector (map first s) (map #(->> % second (get m)) s)))))]
          (if (= s s2) s (recur s2)))))))

(defcheck solution-457045c6
  (fn __ [r]
    (nth (iterate (fn [xs]
                    (reduce clojure.set/union xs
                      (map (fn [x]
                             (set
                               (for [y (disj xs x) :when (= (second x) (first y))]
                                 [(first x) (second y)])))
                        xs)))
           r)
      (count r))))

(defcheck solution-45be30dc
  (letfn [(path? [coll f t]
            (and (some (fn [[a b]]
                         (and (= a f)
                              (or (= b t) (path? (remove #(= a (first %)) coll) b t)))) coll) [f t]))]
    (fn [coll] (set (mapcat #(keep (partial path? coll %)
                               (map second coll))
                      (map first coll))))))

(defcheck solution-46411004
  (fn maketrans [r]
    (let [elems (set (flatten (seq r))),
          rels (apply conj {} (map (fn [[k v]] [k (map second v)]) (group-by first r)))]
      #_(println rels)
      (letfn [(trans? [x y]
                (or (some #(= y %) (rels x))
                    (some #(trans? % y) (rels x))))]
        (set (for [e1 elems e2 elems :when (trans? e1 e2)] [e1 e2]))))))

(defcheck solution-46471d13
  (fn [es]
    (letfn [(floyd-warshall
              ([vs adj]
               (floyd-warshall vs adj (seq vs)))
              ([vs adj [k & tk]]
               (if (nil? k)
                 adj
                 (floyd-warshall vs
                   (reduce (fn [a [i j]]
                             (if (and (get-in a [i k]) (get-in a [k j]))
                               (assoc-in a [i j] true)
                               a))
                     adj
                     (for [i vs j vs] [i j]))
                   tk))))
            (keys-in [m]
              (if (or (not (map? m))
                      (empty? m))
                '([])
                (reduce (fn [a [k v]]
                          (into a
                            (map (partial into [k])
                              (keys-in v))))
                  ()
                  m)))]
      (let [vs (set (flatten (seq es)))
            adj (->> es
                  (map (fn [[x y]] {x {y true}}))
                  (apply merge-with into))]
        (set (keys-in (floyd-warshall vs adj)))))))

(defcheck solution-47791a31
  (fn trc [rel-set]
    (letfn [(get-line [m x]
              (if (m x)
                (cons x (get-line m (m x)))
                [x]))]
      (let [rel-map (into {} rel-set)
            ks (keys rel-map)
            lines (filter #(>= (count %) 3) (for [x ks]
                                              (get-line rel-map x)))
            tran-rel (mapcat (fn [[f s & more]] (map #(vector f %) more)) lines)]
        (set (concat rel-set tran-rel))))))

(defcheck solution-47b00920
  (letfn [(get-new-right-trans [rel add]
            (for [r rel :when (= (second r) (first add))
                  :let [newr [(first r) (second add)]]
                  :when (not (rel newr))]
              newr))
          (get-new-left-trans [rel add]
            (for [r rel :when (= (second add) (first r))
                  :let [newr [(first add) (second r)]]
                  :when (not (rel newr))]
              newr))
          (get-new-trans [rel add] (into #{} (concat (get-new-right-trans rel add)
                                                     (get-new-left-trans rel add))))
          (add-one [rel tr]
            (if (rel tr)
              rel
              (conj (add-all rel (get-new-trans rel tr)) tr)))
          (add-all [rel trs]
            (reduce add-one rel trs))]
    #(add-all #{} %)))

(defcheck solution-47d2f777
  (fn prob-0084
    [rels]

    (let [add-tc-rel (fn add-tc-rel
                       [rel p-map s-map]
                       (let [[p s] rel
                             rels (for [p-new (conj (p-map p) p)
                                        s-new (conj (s-map s) s)]
                                    [p-new s-new])

                             s-rels (map #(array-map (first  %) #{(second %)} ) rels)
                             p-rels (map #(array-map (second %) #{(first  %)} ) rels)

                             new-pm (apply merge-with into p-map p-rels)
                             new-sm (apply merge-with into s-map s-rels)]

                         [new-pm new-sm]))
          ]

      (let [[p-map s-map] (reduce #(apply add-tc-rel %2 %1) [{} {}] rels)]
        (into #{} (for [pred (keys s-map), succ (s-map pred)]
                    [pred succ]))))))

(defcheck solution-47fb5c24
  (fn transitive-closure
    ([pairs] (transitive-closure pairs #{}))
    ([pairs builder]
     (if (empty? pairs)
       builder
       (let [[left right] (first pairs)
             fneighbors (filter #(= (first %) right) (rest pairs))
             rneighbors (filter #(= (second %) left) (rest pairs))
             nextpairs (concat (rest pairs)
                               (for [neighbor fneighbors :when (not (contains? builder neighbor))] [left (second neighbor)])
                               (for [neighbor rneighbors :when (not (contains? builder neighbor))] [(first neighbor) right]))]
         (recur nextpairs (conj builder [left right])))))))

(defcheck solution-482f0112
  (fn [relations]
    (letfn [(new-relations [oldRels new]
              (concat (map (fn [x] [(first new) (second x)]) (filter #(= (second new) (first %)) oldRels))
                      (map (fn [x] [(first x) (second new)]) (filter #(= (first new) (second %)) oldRels)))
              )
            (add-relations [oldRels new]
              (concat oldRels [new] (filter #(not (= (first %) (second %)))(new-relations oldRels new)))
              )
            ]
      (into #{} (reduce add-relations #{} relations) ) )))

(defcheck solution-4836e155
  (fn [s0]
    (loop [s s0]
      (let [pairs (mapcat identity (for [[w x] s] (for [[y z] s] (if (= x y) [w z] false))))
            expansion (set (clojure.set/union s (set (filter identity pairs))))]
        (if (= expansion s)
          s
          (recur expansion))))))

(defcheck solution-4845d4a1
  (fn [vbs]
    (let [tcm  (into {} vbs)
          pout (fn mk-pout [po vb]
                 (let [out (conj po vb)]
                   (if (contains? tcm (second vb))
                     (mk-pout out (vector (first vb) (get tcm (second vb))))
                     out)))]
      (loop [out #{} in vbs]
        (if (empty? in)
          out
          (recur (clojure.set/union out (pout #{} (first in))) (rest in)))))))

(defcheck solution-49a605f9
  (letfn [(extensions [s]
            (fn [[k v]]
              (->> s
                (filter #(= v (first %)))
                (map (juxt (constantly k) second)))))
          (step [[s _]]
            (vector (->> s (mapcat (extensions s)) (into s))
              s))]
    (comp ffirst
          (partial drop-while (partial apply not=))
          (partial iterate step)
          (juxt identity (constantly nil)))))

(defcheck solution-4aa6c6f3
  (fn finalSolution [i] (letfn [(nextOrder [x]
                                  (loop [res x todo x]
                                    (let [[a b] (first todo) r (rest todo)]
                                      (if
                                       (empty? todo)
                                        res
                                        (recur (clojure.set/union res (set (map #(vector a (second %1)) (filter #(= (first %1) b) x)))) r)))))
                                ](loop
                                  [ latest i]
                                   (let
                                    [c (count latest)
                                     new (nextOrder latest)
                                     newCount (count new)]
                                     (if (= c newCount)
                                       latest
                                       (recur new)
                                       ))))))

(defcheck solution-4ac523e3
  (fn transitive-closure [sets]
    (let [rels (reduce #(assoc %1 (first %2) (second %2)) {} sets)]
      (apply clojure.set/union
        (for [leader (keys rels)]
          (set (loop [all-relations nil related (rels leader)]
                 (if related
                   (recur (cons [leader related] all-relations)
                     (rels related))
                   all-relations))))))))

(defcheck solution-4b4cfbec
  #(loop [m % s 0]
     (if (= s (count m))
       m
       (recur (into m (for [[a b] m [c d] m
                            :when (= b c)]
                        [a d]))
         (count m)))))

(defcheck solution-4b84f129
  (fn tc
    ([res ss s]
     (let [todos (filter #(= (first %) (second s)) ss)]
       (if (seq todos)
         (mapcat (fn [t] (tc (concat res [s t]) ss t)) todos)
         [res])))
    ([ss]
     (letfn [(comb [[h & t]]
               (when (and h (seq t))
                 (concat (map (fn [e] [(first h) (second e)]) t)
                         (comb t))))]
       (set (concat ss
                    (mapcat comb
                      (mapcat #(tc [] ss %) ss))))))))

(defcheck solution-4bd4b35
  (fn transitive-clojure [xs]
    (let [xs-map (into {} xs)
          traverse (fn [xs-map last-ks result]
                     (let [last-k (peek last-ks)
                           next-value (xs-map last-k)
                           parents (pop last-ks)]
                       (if (nil? next-value)
                         result
                         (recur xs-map (conj last-ks next-value)
                           (if (empty? parents)
                             result
                             (apply conj result (map #(vector % next-value) parents)))))))]
      (reduce
        (fn [xs k]
          (clojure.set/union  xs (traverse xs-map [k] #{})))
        xs
        (keys xs-map)))))

(defcheck solution-4be4e06f
  (fn [rels]
    (let [
          rules (into {} (seq rels))
          generate (fn [[k v]]
                     (loop [x k, result [], done []]
                       (let [y (rules x), new-done (conj done x)]
                         (if (nil? y)
                           result
                           (recur y
                             (concat result (map (fn [z] [z y]) new-done))
                             new-done)))))]
      (set (apply concat (map generate rels))))))

(defcheck solution-4c0898be
  (fn transitive-closure
    [coll]
    (let [keys (map first coll) map-set (into {} coll)
          next-set (reduce (fn [m [x y]] (if (map-set y) (conj m [x (map-set y)]) m)) coll coll)]
      (if (= next-set coll) coll (transitive-closure next-set)))))

(defcheck solution-4c259a1f
  (fn gen-tc
    [r]
    (letfn [(multiply-ee [e1 e2]
              (if (= (second e1) (first e2))
                (vector (first e1) (second e2))))
            (multiply-es [e r]
              (set (filter (fn [x]
                             (not (= nil x)))
                     (map #(multiply-ee e %) r))))
            (multiply-ss [r1 r2]
              (apply clojure.set/union (map #(multiply-es % r2) r1)))]
      (apply clojure.set/union
        (loop [result [r]]
          (let [mid-res (multiply-ss (last result) r)]
            (if (empty? mid-res)
              result
              (recur (conj result mid-res)))))))))

(defcheck solution-4cbec0e8
  (fn xclosure [binrel]
    (let [fwd (into {}  binrel)]
      (let [br (into binrel
                 (filter seq (map #(if (fwd (second %))
                                     [(first %) (fwd (second %))] []) binrel)))]
        (if (= br binrel) br (recur br))))))

(defcheck solution-4cdac6f
  (fn [m]
    (let [t (fn [m]
              (into m
                (for [k (map first m)
                      v (map second m)
                      x (map second m)
                      :when (and (m [k x])
                                 (m [x v]))]
                  [k v])))]
      (loop [m m]
        (let [x (t m)]
          (if (= m x)
            m
            (recur x)))))))

(defcheck solution-4d7d0e4e
  (fn [edges]
    (letfn [(find-edge [start edges]
              (first (filter #(= start (first %)) edges))
              )
            (transitive-edges [graph edges]
              (let [edge (find-edge (last graph) edges)]
                (if (nil? edge) (map #(vector (first graph) %) (rest graph))
                                (recur (conj graph (second edge)) edges))
                )
              )
            ]
      (into #{} (mapcat #(transitive-edges [(first %)] edges) edges))
      )
    ))

(defcheck solution-4dc0cd7d
  (fn transitive-clojure [rel]
    (let [expand (fn [rel]
                   (loop [[[a b] & more] (seq rel)
                          tc rel]
                     (if-not a
                       tc
                       (let [new-rels (for [[c d] rel :when (= b c)]
                                        [a d])]
                         (recur more (into tc new-rels))))))]

      (loop [tc rel]
        (let [new-tc (expand tc)]
          (if (= (count tc) (count new-tc))
            tc
            (recur new-tc)))))))

(defcheck solution-4e027ada
  (fn __ [cr]
    (let [m (apply merge-with concat (for [[k v] cr] {k #{v}}))
          m2 (reduce merge
               (for [[k v] m] {k (apply concat v
                                   (vals (select-keys m v)))}))
          new-cr (set (reduce concat (for [[k v] m2] (for [l v] [k l]))))]
      (if (= (count new-cr) (count cr))
        cr
        (__ new-cr)))))

(defcheck solution-4e27e41
  (letfn [
          (update [e x] (let [
                              in  (for [ei e :when (= x (second ei))] (first  ei))
                              out (for [ei e :when (= x (first  ei))] (second ei))]
                          (into e (for [v1 in v2 out] [v1 v2]))))
          (trans [e] (reduce update e (distinct (flatten (vec e)))))]
    trans))

(defcheck solution-4e37152d
  (fn [rel*]
    (loop [rel (reduce #(assoc-in %1 %2 true) {} rel*)]
      (let [new-rel
            (reduce-kv (fn [r l rs]
                         (->> (keys rs) (map rel)
                           (apply merge rs)
                           (assoc r l)))
              {} rel)]
        (if-not (= new-rel rel)
          (recur new-rel)
          (into #{} (mapcat (fn [[l rs]]
                              (map vector
                                (repeat l)
                                (keys rs)))
                      rel)))))))

(defcheck solution-4ea473cd
  (fn tc [input]
    (letfn [(descendant?
              [a b]
              (= (first b) (second a)))
            (extend-relation
              [a b]
              (if (descendant? a b) (vector (first a) (second b))))
            (new-relations
              [b binaries]
              (remove (comp nil?) (map #(extend-relation b %) binaries)))
            (one-deep
              [binaries]
              (remove #(= % '()) (mapcat #(new-relations % binaries) binaries)))
            (add-relations
              [binaries]
              (into binaries (one-deep binaries)))]
      (loop [old input new #{}]
        (if (= old new)
          old
          (recur (add-relations old) old))))))

(defcheck solution-4ec9c4fe
  (fn transitive-closure
    [relations]
    (letfn [(step [pairs]
              (clojure.set/union pairs
                                 (set (for [[x1 y1] pairs
                                            [x2 y2] pairs :when (= y1 x2)]
                                        [x1 y2]))))]
      (loop [prev-pairs relations]
        (let [next-pairs (step prev-pairs)]
          (if (= prev-pairs next-pairs)
            next-pairs
            (recur next-pairs)))))))

(defcheck solution-4f58f7af
  (fn f [x]
    (let [r first l last g  (fn [s]
                              (set (for [x s y s :when (or (= x y)
                                                           (= (r x) (l y))
                                                           (= (l x) (r y)))]
                                     (cond
                                       (= x y) x
                                       (= (r x) (l y)) [(r y) (l x)]
                                       (= (l x) (r y)) [(r x) (l y) ]))))
          y (g x)]
      (cond
        (= x y) y
        :else (f y)))))

(defcheck solution-4fddb76a
  #(nth (iterate % %2) 3) #(into % (for [[a b] % [c d] % :when (= b c)] [a d])))

(defcheck solution-50235463
  (fn [pairs]
    (->> pairs
      (iterate (fn [found]
                 (into pairs (for [[a b] found
                                   [d e] found
                                   :when (= b d)]
                               [a e]))))
      (partition 2 1)
      (filter (partial apply =))
      ffirst)))

(defcheck solution-5056dbce
  (fn [s]
    (let [out
          (set (apply concat s
                 (map
                   (fn [r1]
                     (remove nil?
                       (map
                         (fn [r2]
                           (if (= (first r2) (last r1))
                             [(first r1) (last r2)]))
                         s))) s
                   )))]
      (if (= s out) s (recur out)))))

(defcheck solution-505cad21
  (fn [s]
    (let [t (into s
              (for [x s y s :when (= (second x) (first y))]
                [(first x) (second y)]
                ))]
      (if (= t s)
        t
        (recur t))
      )))

(defcheck solution-51158179
  (fn [coll]
    (loop [ret coll]
      (let [new (apply conj ret (seq (for [[a b] ret [c d] coll :when (= b c)] [a d])))]
        (if (= ret new)
          ret
          (recur new))))))

(defcheck solution-51a70cbe
  (fn [s]
    (let [m (apply conj {} s)]
      (reduce
        (fn [r [k v]]
          (if (not (contains? m v))
            r
            (let [nkv [k (get m v)]]
              (recur (conj r nkv) nkv))))
        s m))))

(defcheck solution-52b1e760
  {#{[8 4] [9 3] [4 2] [27 9]} #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}
   #{["cat" "man"] ["man" "snake"] ["spider" "cat"]} #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
                                                       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}
   #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}
   #{["father" "son"] ["father" "grandson"]
     ["uncle" "cousin"] ["son" "grandson"]}})

(defcheck solution-52e284a5
  #(loop [ps %]
     (let [ts (set (for [x ps y %
                         :when (not= x y)] (if (= (second x) (first y)) [(first x) (second y)] x)))]
       (if (= ts ps)
         ps
         (recur ts)))))

(defcheck solution-52f2eb18
  (fn trans-closure [set]
    (let [fixpoint (fn [f arg]
                     (loop [old-data arg]
                       (let [new-data (f old-data)]
                         (if (= old-data new-data)
                           new-data
                           (recur new-data)))))]

      (fixpoint (fn [set]
                  (into set
                    (for [[a b] set
                          [c d] set
                          :when (= b c)]
                      [a d])))
        set))))

(defcheck solution-5306793b
  (fn [rels]
    (let [parent->child (into {} rels)]
      (reduce (fn [new-rels parent]
                (loop [child    (parent->child parent)
                       new-rels new-rels]
                  (if child
                    (recur (parent->child child)
                      (conj new-rels [parent child]))
                    new-rels)))
        #{}
        (keys parent->child)))))

(defcheck solution-5403624f
  (fn [x]
    (let [m (into {} x)]
      (into #{} (mapcat (fn [k] (for [v (iterate m (m k))
                                      :while v]
                                  [k v])) (keys m))))))

(defcheck solution-5533c071
  (fn [rels]
    (letfn [
            (combinations [groups] (if (empty? groups) [[]] (mapcat #(map % (combinations (rest groups))) (map #(partial cons %) (first groups)))))
            (expand [p1 p2] (cond (= p1 p2) p1 (= (second p1) (first p2)) (vector (first p1) (second p2)) (= (first p1) (second p2)) (vector (first p2) (second p1)) :else ()))
            (squeeze [rels] (filter #(not (empty? %)) (distinct (map #(apply expand %) (combinations (vector rels rels))))))
            (transitive-closure [rels] (if (= rels (squeeze rels)) (set rels) (transitive-closure (squeeze rels))))
            ] (transitive-closure rels))))

(defcheck solution-55c82fea
  (fn transitive-closure [rels]
    (let [
          new-bg (fn []
                   { :relations {}, :inverses {} })
          add-rel (fn [bg [x y]]
                    (let [left (bg :relations)
                          right (bg :inverses)
                          new-rels (concat [[x y]]
                                           (map #(vector % y) (right x))
                                           (map #(vector x %) (left y)))
                          assoc-multi (fn [map [x y]]
                                        (if (contains? map x)
                                          (assoc map x (conj (map x) y))
                                          (assoc map x #{y})))
                          assoc-multi-inv (fn [map [x y]]
                                            (assoc-multi map [y x]))]
                      (if (and (contains? left x) (contains? (left x) y))
                        bg
                        { :relations (reduce assoc-multi left new-rels)
                         :inverses (reduce assoc-multi-inv right new-rels) })))
          bg-from-rels (fn [rels]
                         (reduce add-rel (new-bg) rels))
          vectors-from-mappings (fn [[k vs]]
                                  (reduce #(cons [k %2] %1) '() vs))
          rels-from-bg (fn [bg]
                         (reduce #(reduce conj %1 (vectors-from-mappings %2)) #{} (bg :relations)))
          ]
      (->> rels (bg-from-rels) (rels-from-bg)))))

(defcheck solution-55dfe842
  (fn trans [xys]
    (letfn [(union [& sets] (reduce #(into %1 %2) #{} sets) )
            (transClosure [[x y]] ; Find all z such that x ~ y' ~ z
              (loop [reachable #{}
                     newReachable #{y}]
                (if (empty? newReachable)
                  (set (map (fn toPairs [z] [x z]) reachable)) ; No change
                  (recur (union reachable newReachable) ; Add newly reachable stuff
                    (set (map second (filter (fn reachableFromNew [[y1 z]] (newReachable y1)) xys))))
                  )
                )
              )]
      (apply union (map transClosure xys))
      )
    ))

(defcheck solution-55f082a0
  (fn f [s]
    (let [sx (set
               (partition 2
                 (flatten
                   (for [a s]
                     (for [b s
                           :when (= (second a) (first b))]
                       [(first a) (second b)])))))
          s2 (clojure.set/union s sx)
          ]
      (if (= s s2)
        s2
        (f s2)))))

(defcheck solution-5698135e
  (fn prob84 [r]
    (letfn
     [(all-pairs [s]
        (if (< (count s) 2) ()
                            (concat
                             (map #(vector (first s) %) (rest s))
                             (all-pairs (rest s)))))]
      (let [tchain
            (reduce
              (fn [s pair]
                (if-let [l1 (first (filter #(= (last %) (first pair)) s))]
                  (conj (disj s l1) (concat l1 (list (second pair))))
                  (if-let [l2 (first (filter #(= (first %) (second pair)) s))]
                    (conj (disj s l2) (concat (list (first pair)) l2))
                    (conj s pair))))
              #{} r)]
        (set (apply concat (map all-pairs tchain)))))))

(defcheck solution-56a26266
  (fn [col] (letfn [(il [c] (loop [ curr c xs c]
                              (if (empty? xs)
                                curr
                                (let [item (first xs)
                                      remains (rest xs)
                                      item1 (first item)
                                      item2 (second item)
                                      all-infer (filter #(= (first %) item2) c)
                                      new-rels (map (fn [x] [item1 (second x)] ) all-infer)
                                      ]
                                  (recur (clojure.set/union curr (set new-rels)) remains)
                                  ))))] (loop [curr col]
                                          (let [updated (il curr)]
                                            (if (= updated curr)
                                              curr
                                              (recur updated)))))))

(defcheck solution-56f6659b
  #(let [r (apply conj % (for [[x y] % [u v] % :when (= y u)] [x v]))]
     (if (= r %)
       %
       (recur r))))

(defcheck solution-57015065
  (fn [s]
    (letfn ((sub [s]
              (reduce (fn [r v]
                        (conj (reduce (fn [r2 v2]
                                        (if (= (v 1) (v2 0))
                                          (conj r2 [(v 0) (v2 1)])
                                          r2))
                                r
                                s)
                          v))
                #{}
                s)))
      (loop [s s
             s2 (sub s)]
        (if (= s s2)
          s
          (recur s2 (sub s2)))))))

(defcheck solution-57a6bee9
  (fn f [s]
    (let [r (apply conj s (remove nil? (for [a s b s]
                                         (if (= (second a) (first b)) [(first a) (second b)]))))] (if (= r s) r (f r)))))

(defcheck solution-583ef223
  (fn [given-set]
    (let [extended
          (into given-set (mapcat (fn [g]
                                    (map #(vector (first g) (last %))
                                      (filter #(= (last g) (first %)) given-set)))
                            given-set))]
      (if (= given-set extended) given-set
                                 (recur extended)))))

(defcheck solution-58701cf9
  (fn transitive-closure [rel]
    (letfn [(my-merger [coll1 coll2]
              (merge-with clojure.set/union coll1 coll2))
            (add-to-map [coll key val]
              (my-merger {key val} coll))
            (add-to-path [m]
              (zipmap (map second m) (map #(list (first %)) m)))]
      (let [init-map (reduce my-merger (map #(hash-map (first %) (hash-set (second %))) rel))]
        (loop [keymap init-map queue rel]
          (do
            #_(println keymap)
            #_(println queue)
            (if-let [head (first queue)]
              (if-let [val (get keymap (second head))]
                (recur (add-to-map keymap (first head) val) (rest queue))
                (recur keymap (rest queue)))
              (apply hash-set (for [x keymap, y (second x)]
                                (vector (first x) y))))))))))

(defcheck solution-590d2865
  (fn [rs]
    (letfn [(transitive [m k]
              (when-let [vs (get m k)]
                (apply concat (cons vs (map #(transitive m %) vs)))))]
      (let [m (reduce (fn [m [k v]] (merge-with concat m {k [v]})) {} rs)]
        (set (apply concat (map (fn [k] (map (fn [v] [k v]) (transitive m k))) (keys m))))))))

(defcheck solution-5adeb28d
  (fn expandLink[wordseed]
    (let[linkInfo  (reduce
                     (fn[map newword]
                       (let[k (first newword),
                            v (first (rest newword) )]
                         (assoc map k v )
                         )
                       )
                     {}
                     wordseed),
         topNodes   (let[vals (set (vals linkInfo))]
                      (filter
                        #(not (contains? vals %) )
                        (keys linkInfo)
                        )
                      )
         linksFn  (fn addLink[links]
                    (let[next (get linkInfo (first links))]
                      (if (nil? next)
                        links
                        (addLink (cons next links) )
                        )
                      )
                    )
         generLink (fn genLink[links]
                     (if(empty? (rest links))
                       []
                       (let[other (rest links),
                            head (first links)]
                         (concat (genLink (rest links))
                                 (map #(vector % head)
                                   other)
                                 )
                         )
                       )
                     )
         ]
      #_(println linkInfo)
      #_(println topNodes)
      (reduce #(apply conj %1 (generLink %2) )#{}
        (map #(linksFn (list %)) topNodes) )
      )
    ))

(defcheck solution-5b764d94
  (fn [s]
    (set (reduce
           (fn [result rel]
             (let [s (conj result rel)]
               (concat s
                       (for [[h1 t1] s [h2 t2] s :when (= t1 h2)]
                         [h1 t2]
                         ))
               ))
           s
           s))))

(defcheck solution-5baa6df9
  (fn [divides]
    (letfn [(f [p cp]
              (into #{}
                (filter
                  #(not (nil? %))
                  (map (fn [d] (if (= (second p) (first d))
                                 [(first p) (second d)]
                                 (if (= (second d) (first p))
                                   [(first d) (second p)]))) cp))))]
      (loop [r #{} c divides]
        (if (empty? c)
          r
          (let [fe (first c)
                m (f fe (next c))]
            (recur (into (conj r fe) m) (into (next c) m)))
          )))))

(defcheck solution-5bdaef67
  (fn transive-closure
    ([rels rel] (transive-closure (conj rels rel)))
    ([rels]
     (reduce transive-closure rels
       (->> (for [[a b] rels [x y] (disj rels [a b])]
              (when (and (= x b) (not (rels [a y]))) [a y]))
         (remove nil?))))))

(defcheck solution-5c67f742
  (fn trans [s]
    (let [find-all (fn [[a b] pt]
                     (for [[_b c] (pt b)]
                       [a c]))]

      (loop [acc s prev nil] ;; loop until no further changes
        (if (= acc prev) acc
                         (let [pt (group-by #(first %) acc)]
                           (recur
                             (clojure.set/union acc
                               (set (apply concat
                                      (for [x acc] (find-all x pt))))) acc)))))))

(defcheck solution-5c7d47ae
  #(let [r (set (concat %
                        (for [[a b] % [c d] % :when (= b c)]
                          [a d])))]
     (if (= r %) r (recur r))))

(defcheck solution-5cdd72b7
  (fn [ init-vecs ]
    (letfn [(one-level
              [vecs]
              (into #{}
                (remove nil?
                  (apply concat
                    (for [[k v] vecs]
                      (map #(let [[k1 v2] %]
                              (when (or (= k1 k) (= k1 v))
                                [k v2])) vecs))))))]
      (loop [acc init-vecs, prev-size 0]
        (let [acc2 (one-level acc), size (count acc2)]
          (if (< prev-size size)
            (recur acc2 size)
            acc2))))))

(defcheck solution-5d493008
  (fn transitive-closure [s]
    (letfn [(transitions [[a b] s]
              ; return a list of all [a c] such that [b c] is in s
              (map #(vector a (get % 1)) (filter (fn [[x y]] (= b x)) s)))
            (all-transitions [s]
              ; return the transitions for every element of s
              (set (mapcat #(transitions % s) s)))
            (with-transitions [s]
              ; return s with all its transitions unioned in
              (clojure.set/union s (all-transitions s)))
            (fixed-point [f a]
              ; return the fixed point that (f a) converges to
              (let [fa (f a)] (if (= fa a) a (recur f fa))))]
      (fixed-point with-transitions s))))

(defcheck solution-5de83401
  (fn [d]
    (let [m (into {} d)
          f #(let [v (m %2)] (if v (recur % v (conj %3 [% v])) %3))]
      (reduce #(f %2 %2 %) #{} (keys m)))))

(defcheck solution-5e8daf25
  (fn [r]
    (let [g (group-by first r)
          m (apply concat
              (map
                (fn [x] (apply concat
                          (map
                            (fn [y] (map #(do [x (second %)])
                                      (get g y)))
                            (map second (get g x)))))
                (keys g)))
          next (if (set? r) (into r m) (into #{} r m))]
      (if (= next r)
        r
        (recur next)))))

(defcheck solution-5e9f926a
  (fn [s]
    (letfn [(f [v n] (set (map (fn [[_ y]] [(first n) y]) (filter (fn [[x _]] (= (second n) x)) v))))]
      (reduce #(into % (f % %2)) s s))))

(defcheck solution-5f086431
  (fn trans-closure [in-set]
    (let [helper
          (fn trans-helper
            [seta]
            (letfn [(expand-pair [[f s]]
                      (map #(vector f (second %)) (filter #(= (first %) s) seta)))]
              (set (concat seta (mapcat expand-pair seta)))))]
      (helper (helper in-set)))))

(defcheck solution-5f3f941f
  (fn f [s]
    (#(if (= % s) % (f %))
     (into s
       (for [[a b] s, [c d] s
             :when (= c b)]
         [a d])))))

(defcheck solution-6036acb8
  (fn [pairs]
    (letfn
     [(connect [chains]
        (for [x chains y chains :when (= (last x) (first y))]
          (concat x (rest y))))
      (build-chains [chains]
        (let [new-chains (set (connect (concat pairs chains)))]
          (if (= chains new-chains)
            (concat chains pairs)
            (build-chains new-chains))))
      (break-up [pairs [head & tail]]
        (if (empty? tail)
          pairs
          (break-up
            (concat
             pairs
             (map
               (fn [itm] [head itm])
               tail))
            tail)))]
      (reduce into #{} (map (partial break-up []) (build-chains pairs))))))

(defcheck solution-608c9387
  (fn f [s]
    (let [m (into {} s)]
      (letfn [(r [n] (loop [x n coll []] (if-let [y (m x)]
                                           (recur y (conj coll [n y])) coll)))]
        (set (mapcat r (map first m)))))))

(defcheck solution-609469ae
  #(letfn [(c[t]
             (into #{}
               (remove nil?
                 (for [[e f :as x] % [g h :as y] t]
                   (cond (= x y) x
                         (= f g) [e h])))))]
     (c(c %))))

(defcheck solution-61065b04
  (letfn [
          (rel-to-map [rel]
            (reduce (fn [m [k v]] (assoc m k #{v})) {} rel))
          (trans-closure1 [rel-map]
            (apply
              conj
              {}
              (for [[k v] rel-map] [k (set (apply concat v (map (partial get rel-map) v)))])))
          (trans-closure* [rel-map]
            (loop [r1 {}, r2 rel-map]
              (if (= r1 r2)
                r1
                (recur r2 (trans-closure1 r2)))))]

    (fn [rel]
      (set (for [[k v] (trans-closure* (rel-to-map rel)), vi v] [k vi])))))

(defcheck solution-61489a95
  (fn [s]
    (->> (iterate #(into % (for [[u v] % [w x] % :when (= v w)] [u x])) s)
      (partition 2 1)
      (drop-while (fn [[u v]] (< (count u) (count v))))
      (first)
      (first))))

(defcheck solution-6152ce90
  (fn [rels] (loop [rs rels]
               (let [newrs (into rs
                             (for [[r11 r12] rs [r21 r22] rs :when (= r11 r22)]
                               [r21 r12]))]
                 (if (= newrs rs) rs (recur newrs))))))

(defcheck solution-61fc39df
  (fn [s]
    (loop [p s]
      (let [q (reduce
                (fn [a [x y]]
                  (into a (keep (fn [[u v]] (if (= y u) [x v])) a)))
                p
                p)]
        (if (= p q) p (recur q))))))

(defcheck solution-6343ccd
  (letfn [(tr-clos [rel-pairs]
            (set (filter (frel? rel-pairs)  (all-pairs rel-pairs))))
          (frel? [pairs] ;set of pairs
            (letfn [(rel? [a b]
                      (or
                       (contains? pairs [a b])
                       (some #(rel? % b) (direct a))))
                    (direct [a]; directly related items
                      (map second (filter #(= (first %) a) pairs)))
                    ]
              (fn [pair] (apply rel? pair))))
          (all-pairs [pairs]
            (let [els (all-elems pairs)]
              (for [x els y els :when (not= x y)] [x y])))
          (all-elems [pairs]
            (reduce #(conj (conj %1 (first %2)) (second %2)) #{} pairs))
          ]
    tr-clos
    ))

(defcheck solution-63a2501d
  (fn [r]
    (let [closure (fn [f i] (loop [x i] (if (= (f x) x) x (recur (f x)))))]
      (closure #(reduce (fn [coll [x y]]
                          (let [y2 (some (fn [[x2 y2]] (and (= y x2) y2)) coll)]
                            (if y2 (conj coll [x y2]) coll)))
                  % %)
        r))))

(defcheck solution-63da71a3
  (fn [x] (let [l (iterate
                    #(clojure.set/union %
                       (set (for [i (seq %)]
                              (let [m (some (fn [z] (if (= (z 0) (i 1)) z)) %)]
                                (if (not (nil? m))
                                  (vector (i 0) (m 1))i))
                              )
                         )
                       )




                    x)
                ]
            (set (nth l 10))
            )
    ))

(defcheck solution-63de9b64
  (fn transitive[edges]
    (letfn [(next-closure [init-edges]
              (into init-edges (for [e1 init-edges  e2 init-edges
                                     :let [[s e] e1 [x y] e2]
                                     :when (= e x)]
                                 [s y]
                                 ))
              )]
      (loop[curr-edges edges]
        (let [next-edges (next-closure curr-edges)]
          (if (= curr-edges next-edges)
            curr-edges
            (recur next-edges)
            )
          )
        )
      )
    ))

(defcheck solution-63dee1f1
  (fn f [y x]
    (if-let [[a b] (first x)]
      (f (into
           (into
             (conj y [a b])
             (for [[c d] y :when (= b c)] [a d]))
           (for [[c d] y :when (= a d)] [c b]))
        (next x))
      y)) #{})

(defcheck solution-64187c65
  (fn [s]
    (let [m (apply merge (map #(apply hash-map %) s))
          r (set (concat s (filter #(last %) (map #(list (first %) (m (last %))) s))))]
      (if (= s r) r (recur r)))))

(defcheck solution-64bb3584
  (fn [pairs]
    ; basically Warshall's using a map as a sparse adjacency "matrix"
    (let [union (fn [a b] (reduce #(conj % %2) a b))
          add-adjacency (fn [adjm [from to]] (assoc adjm from (conj (adjm from) to)))
          adjm (reduce add-adjacency (zipmap (reduce concat [] pairs) (repeat #{})) pairs)
          warshall-step (fn [adjm [i j]]
                          (if (contains? (adjm i) j)
                            (assoc adjm i (union (adjm i) (adjm j)))
                            adjm))
          tc-adjm  (reduce warshall-step adjm (for [j (keys adjm) i (keys adjm)] [i j]))
          flatten (fn [acc from] (conj acc (for [to (tc-adjm from)] [from to])))]
      (set (reduce concat [] (reduce flatten [] (keys tc-adjm)))))))

(defcheck solution-6526b8dc
  (fn [relations]
    (let [p
          (fn p [itm relations]
            (let [visits (map second (filter (fn [v] (= (first v) itm)) relations))]
              (if (empty? visits)
                '()
                (flatten (concat visits
                                 (map #(p % relations) visits))))))]
      (apply hash-set (apply concat (map (fn [x] (map (fn [y] [(first x) y]) (p (first x) relations))) relations))))))

(defcheck solution-65698b11
  (fn [l]
    (let [m (reduce (fn [a [k v]]
                      (merge-with concat a {k [v]}))
              {} l)]
      (loop [p (map vector (keys m)) t #{}]
        (if (empty? p) t
                       (let [p (reduce (fn [q l]
                                         (into q (reduce #(conj % [(first l) %2])
                                                   [] (get m (last l)))))
                                 #{} p)]
                         (recur p (into t p))))))))

(defcheck solution-65a98080
  (fn[in]
    (loop [tuples in]
      (let [res
            (clojure.set/union tuples
              (into #{} (for [a tuples b tuples
                              :when (= (second a) (first b))]
                          [(first a) (second b)])))]
        (if (= res tuples)
          res
          (recur res))))
    ))

(defcheck solution-660e4e39
  (fn tc [R]
    (let [pairs  (set (for [[a b] R [c d] R :when (= b c)]
                        [a d]))]
      (if-let [diff (not-empty (clojure.set/difference pairs R))]
        (tc (clojure.set/union diff R))
        R))))

(defcheck solution-6634686c
  (fn transClosure[s]
    (letfn [(rel->map[s]
              (apply (partial merge-with (comp vec concat)) (map (fn [[a b]] (into {} [[a [b]]])) s)))
            (reachable[m & keys]
              (flatten (map
                         (fn [key]
                           (if (contains? m key)
                             [key (apply (partial reachable m) (m key))]
                             [key]))
                         keys)))]

      (set
        (filter
          (complement empty?)
          (reduce
            #(into %1 %2)
            #{}
            (let [m (rel->map s)]
              (map
                (fn [key]
                  (map
                    (fn [el]
                      (if (not= el key)
                        [key el]
                        []))
                    (reachable m key)))
                (keys m)))))))))

(defcheck solution-66879086
  (fn tc [s]
    (let [new (for [[x y] s
                    [x1 y1] s
                    :when (= y x1)]
                [x y1])
          ss (into s new)]
      (if (= s ss)
        s
        (tc ss)))))

(defcheck solution-66c858dc
  (fn [rs] (letfn [(preproc [l] (reduce mergestep (map (fn [[a b]] (hash-map a #{b})) l))) (mergestep [& ls] (apply merge-with #(reduce conj % %2) ls)) (next [n d] (if (> n 0) (recur (- n 1) (mergestep d (into {} (for [[k v] d] [k (set (mapcat d v))])))) d))] (into #{} (let [d (preproc rs)] (for [[k vs] (next (count d) d) vi vs] [k vi]))))))

(defcheck solution-674d6e4a
  (fn [l]
    (let [m (into {} l)]
      (set
        (mapcat
          #(loop [x % a []]
             (if (m x)
               (recur (m x) (conj a [% (m x)]))
               a))
          (keys m))))))

(defcheck solution-67585dd9
  (fn [r]
    (letfn [(closure [v]
              (let [ys (for [[x y] r :when (= x v)] y)]
                (mapcat (fn [y] (cons y (closure y))) ys)))]
      (set (mapcat (fn [x] (map (fn [y] [x y]) (closure x)))
             (apply concat r))))))

(defcheck solution-6774f8c0
  #(let [k (into % (for [[a b] % [c d] % :when (= b c)] [a d]))]
     (if (= k %) % (recur k))))

(defcheck solution-67c675a3
  (fn [a] (letfn
           [(oot [[a b] [c d]] (if (= b c) [[a b] [c d] [a d]] [[a b] [c d]]))
            (oo2 [e f] (concat (oot e f) (oot f e)))
            (oon [g] (into #{} (apply concat (for [x g y g] (oo2 x y)))))
            (tc [i] (let [h (oon i)] (if (> (count h) (count i)) (tc h) i)))
            ]
            (tc a))))

(defcheck solution-68573bf7
  (fn [x]
    (letfn [(o [p e]
              (loop [m p
                     b e
                     k e
                     v (m k)
                     a (into #{} m)]
                (if (nil? v)
                  a
                  (recur m b v (m v) (conj a [b (m k)])))))]
      (->>
        (into {} x)
        (#(map (fn [[k v]] (o % k)) %))
        (reduce #(into %2 %))
        ))))

(defcheck solution-6860dbc3
  (fn [rels]
    (letfn [(step [rs]
              (into rs
                (for [[a b] rs
                      [x y] rs
                      :when (= b x)]
                  [a y])))]
      (ffirst (drop-while (fn [[a b]] (not= a b))
                (partition 2 1 (iterate step rels)))))))

(defcheck solution-688837c2
  (fn [s]
    (letfn [(sub [s]
              (loop [[a & r] s acc ()]
                (if a
                  (recur r (concat acc (map #(cons a [%]) r)))
                  acc)))
            (getP [t s] (some #(when (= (first t) (second %)) %) s))

            (getA [t s] (some #(when (= (last t) (first %)) %) s))]
      (set (mapcat sub ((fn g [s acc]
                          ((fn f [o t s]
                             (let [[x y :as a] ((if o getA getP) t s)]
                               (if a
                                 (f o (if o (conj t y) (cons x t)) (disj s a))
                                 (if o
                                   (f (not o) t s)
                                   (if (empty? s) (conj acc t)
                                                  (g s (conj acc t)))))))
                           true (first s) (disj s (first s))))
                        s []))))))

(defcheck solution-6933e4e8
  #(let [c (fn [v] (into v (for [[x y] v [z t] % :when (= y z) ] [x t])))]
     (loop [acc % prev #{}]
       (if (= acc prev)
         acc
         (recur (c acc) acc)))))

(defcheck solution-694624e2
  (fn [r]
    (let [R (reduce (fn [R [t f]] (assoc R f (conj (R f []) t))) {} r)
          g (fn [k]
              (loop [[q & Q :as N] (R k) S #{}]
                (if (empty? N) S (recur (concat Q (if-let [t (R q)] (if-not (S q) t))) (conj S q)))))]
      (set (mapcat (fn [k] (map vector (g k) (repeat k))) (keys R))))))

(defcheck solution-69d7ad54
  (fn
    [pairs]
    (let [m (into {} pairs)]
      (letfn [(descendents
                [k]
                (when k
                  (flatten (conj (descendents (get m k)) (get m k)))))]
        (let [new-m (into {} (for [[k v] m]
                               [k (conj (descendents v) v)]))
              new-pairs (for [[k v] new-m
                              d (keep identity v)]
                          [k d])]
          (set new-pairs))))))

(defcheck solution-69ef578d
  (fn l[s]
    (let[D (fn k[ss](into ss (for [[a b] ss [c d] ss :when (= b c)] [a d])))]
      (if (= s (D s))
        s
        (recur (D s))))))

(defcheck solution-69fa01c2
  (fn [relations]
    (letfn [(get-relations [c pair]
              (loop [h (first pair) n (last pair) result []]
                (let [r (some #(if (= n (first %)) (last %)) c)]
                  (if (nil? r)
                    result
                    (recur h r (cons [h r] result))))))]
      (set (concat relations
                   (->> (map (partial get-relations relations) relations)
                     (filter (complement empty?))
                     (reduce concat (list))))))))

(defcheck solution-6b79aeb1
  (fn [rel]
    (reduce
      (fn [r _]
        (into r
          (for [[f1 t1] r
                [f2 t2] r
                :when (= t1 f2)]
            [f1 t2])))
      rel
      (range (count rel)))))

(defcheck solution-6c6f8095
  (fn [col]
    (letfn
     [(close-with [p rel]
        (remove nil?
          (map #(if (= (second p) (first %))
                  [(first p) (second %)]
                  nil) rel)))]
      (loop [cur col]
        (let [iter (reduce clojure.set/union cur
                           (for [p cur] (set (close-with p cur))))]
          (if (= iter cur)
            iter
            (recur iter)))))))

(defcheck solution-6e91f5c
  (fn [items]
    (let [item-map     (reduce (fn [m [k v]]
                                 (update-in m [k] #(conj % v)))
                         {} items)]
      (letfn [(descs [nd k]
                (reduce (fn [nd v]
                          (if-not (contains? nd v)
                            (descs (conj nd v) v)
                            nd))
                  nd (item-map k)))]
        (reduce (fn [s k]
                  (into s (map (juxt (constantly k) identity) (descs #{} k))))
          #{} (keys item-map))))))

(defcheck solution-6eb346f7
  (fn transitive-set [s]
    (loop [relations s]
      (let [transitions
            (reduce (fn [transitive el]
                      (into transitive (reduce (fn [a b]
                                                 (if (= (second b) (first el)) (conj a [(first b) (second el)]) a)
                                                 ) #{} (remove #{el} relations)))
                      )
              relations
              relations
              )
            ]
        (if (= (count relations) (count transitions)) transitions (recur (into relations transitions)))
        )
      )
    ))

(defcheck solution-6ecf4f07
  (fn trans-closure [rel]
    {:pre [(set? rel),
           (every? (comp (partial = 2) count) rel)]}

    ;; We find the first level of additions which are demanded by
    ;; transitivity. That is, if rel contains [x y] and [y z], then additions
    ;; will contain [x z].
    (let [additions
          (->> (for [[x y] rel]
                 (->> rel
                   (filter (comp (partial = y) first))
                   (map #(assoc % 0 x))))
            (apply concat)
            set)]

      ;; If everything we found was already in rel, then we're done;
      ;; otherwise, recur.
      (if (seq (clojure.set/difference additions rel))
        (recur (clojure.set/union rel additions))
        rel))))

(defcheck solution-6f0bef20
  (fn [coll]
    (->> coll
      (iterate
        #(set (for [[a b] %
                    [c d] coll
                    :let [r [a d]]
                    :when (and (= b c) (not (contains? % r)))]
                r)))
      (take-while not-empty)
      (apply concat)
      set)))

(defcheck solution-6f4f510f
  #(loop [paths %]
     (let [newpaths
                    (set
                      (for [start paths
                            end paths
                            :let [edge [(first start) (last end)]]
                            :when (= (last start) (first end))]
                        edge
                        )
                      )
           allpaths (clojure.set/union paths newpaths)
           ]
       (if (= paths allpaths)
         paths
         (recur allpaths)
         )
       )
     ))

(defcheck solution-6fdeddfe
  (fn [edges]
    (letfn [(edge->hash [edge]
              (let [[a b] edge]
                {a #{b}}))

            (edges->graph [edges]
              (let [hashes (map edge->hash edges)]
                (apply merge-with clojure.set/union hashes)))

            (followers [graph v]
              (let [next-steps (graph v)]
                (concat next-steps (mapcat (partial followers graph) next-steps))))

            (transitive-closure [graph]
              (set (for [node (keys graph)
                         follower (followers graph node)]
                     [node follower])))]

      (-> edges edges->graph transitive-closure))))

(defcheck solution-6ff7f78b
  (fn [r]
    (loop [r+ #{} for-r r]
      (if (seq for-r)
        (let [[a b] (first for-r)
              in-r1 (filter (fn [[r-a _]] (= b r-a)) r+)
              in-r2 (filter (fn [[_ r-b]] (= a r-b)) r+)]
          (recur (conj (into (into r+ (map (fn [[_ r-b]] [a r-b]) in-r1))
                         (map (fn [[r-a _]] [r-a b]) in-r2))
                   (first for-r))
            (rest for-r)))
        r+))))

(defcheck solution-7037750c
  #(loop [[[x y :as p] & r] (seq %) c %]
     (if p
       (recur (concat r (for [[w z] % :when (= y w)] [x z])) (conj c p))
       c)))

(defcheck solution-705c6997
  #(loop [s %]
     (let [t (into s (for [[k v] s [l w] s :when (= v l)] [k w]))]
       (if (= s t)
         t
         (recur t)))))

(defcheck solution-7065b0b
  (fn [tuples]
    (loop [[left right :as current] (first tuples)
           others (disj tuples current)
           rest-tuples others
           result #{current}]
      (let [referenced (first (filter #(= (first %) right) others))]
        (if referenced
          (recur [left (second referenced)]
            others
            rest-tuples
            (conj result [left (second referenced)]))
          (if (empty? rest-tuples)
            result
            (let [next (first rest-tuples)]
              (recur next
                (disj tuples next)
                (disj rest-tuples next)
                (conj result next)))))))))

(defcheck solution-70875077
  (fn transitive-closure
    [rel]
    (let [known rel
          expand (into #{} (for [x known
                                 y known]
                             (cond
                               (= (second x) (first y)) [(first x) (second y)]
                               :else x)))]
      (if (= (count known) (count expand))
        known
        (recur expand)))))

(defcheck solution-70a54c37
  #((fn transitive-closure
      [s c]
      (letfn [(transitive-closure*
                [[a b]]
                (for [[x y] s :when (= b x)] [a y]))]
        (if-let [closures (seq (mapcat (fn [x] (transitive-closure* x)) c))]
          (recur (into s closures) closures)
          s))) % %))

(defcheck solution-714fbf93
  (fn [coll]
    (letfn [(ins-find [ans curr]
              (loop [a ans t [[] []]]
                (if (empty? a)
                  t
                  (recur (rest a) (let [f (first a)]
                                    (cond
                                      (= (last f) (first curr)) [f, (last t)]
                                      (= (first f) (last curr)) [(first t) f]
                                      :else t))))))
            (ins-con [ans t curr]
              (clojure.set/difference (conj ans (vec (distinct (into (into (first t) curr) (last t))))) (set t)))
            (ins-ans [ans curr]
              (ins-con ans (ins-find ans curr) curr))
            (ins-coll [coll]
              (loop [c coll ans #{}]
                (if (empty? c)
                  ans
                  (recur (rest c) (ins-ans ans (first c))))))
            (gen-ps [coll]
              (loop [c coll a []]
                (if (empty? (rest c))
                  a
                  (recur (rest c) (into a (for [x (rest c)] [(first c) x]))))))
            ]
      (set (mapcat gen-ps (ins-coll coll))))))

(defcheck solution-7210617f
  (fn [rs]
    (letfn
     [
      (xrs [] (filter #(not (contains? rs %)) (apply concat (for [rab rs] (racs rab)))))
      (racs [rab] (for [rbc (filter #(= (last rab) (first %)) rs)] [(first rab) (last rbc)]))
      ]
      (if (empty? (xrs))
        rs
        (recur (apply conj rs (xrs)))
        )
      )
    ))

(defcheck solution-72719174
  (fn [r]
    (letfn
     [(f [l]
        (into
          l
          (mapcat
            (fn [[a b]]
              (map
                (fn [[c d]] [a d])
                (filter #(= (first %) b) l)))
            l)))]
      (loop [res r n (f r)]
        #_(println res n)
        (if (= res n)
          res
          (recur n (f n)))))))

(defcheck solution-7289a999
  (letfn [
          (transitions [transitive rels k1 k2]
            (cond
              (empty? rels) transitive
              (contains? transitive (first rels)) (recur transitive (rest rels) k1 k2)
              (= (first (first rels)) k2)
              (recur
                (set (concat transitive #{(first rels) [k1 (last (first rels))]}))
                (rest rels)
                k1 k2)
              :else
              (recur transitive (rest rels) k1 k2)
              )
            )]

    #(loop [counter 0 prev nil rels %]
       (if (or (>= counter (count %)) (= rels prev))
         rels
         (recur
           (inc counter) rels (apply clojure.set/union (for [r1 rels :let [k1 (first r1) k2 (last r1)]] (transitions #{r1} rels k1 k2)))
           )
         )
       )
    ))

(defcheck solution-7292064c
  (fn [s]
    (loop [t s]
      (let [d (set
               (mapcat
                (fn [[k v]]
                  (map
                   (fn [[m n]] [k n])
                   (filter (fn [e] (= v (first e))) t)
                   )) t))]
        (if (clojure.set/subset? d t) t (recur (clojure.set/union d t))))
      )))

(defcheck solution-72c0c8e3
  (fn [coll] (letfn [
                     (fc [s cl]
                       (let [n (first (filter (fn [[f l]] (= f (last s))) cl))]
                         (if (seq n)
                           (concat n (fc n cl)))))

                     (allss [x] (let [ct (count x) x (vec x)]
                                  (for [
                                        i (range ct)
                                        j (range (inc i) ct)
                                        ] [(x i)(x j)])))]

               (into #{} (mapcat (fn [e] (let [fce (distinct (fc e coll))]
                                           (if (seq fce)
                                             (allss (concat [(first e)] fce))
                                             [e]))) coll)))))

(defcheck solution-72e792cf
  #(let [  not-nil? (comp not nil? last)
         closure (fn [[d k v]] [(dissoc d k) k (d v)] )
         gen (fn [el] (->> (cons (into {} %) el)
                        (iterate closure)
                        (take-while not-nil?)
                        (map next)))]

     (into #{} (mapcat gen %))
     ))

(defcheck solution-730426f6
  (fn t [s]
    (let [q (reduce #(merge-with concat %1 {(first %2) [(second %2)]}) {} s)
          r (reduce (fn [s t]
                      (apply merge-with
                        (concat (list concat s)
                                (map #(hash-map (first t)
                                        (get q %))
                                  (second t)))))
              q
              q)
          z (reduce #(apply conj
                       (cons %1
                         (map (fn [x] [(first %2) x]) (second %2))))
              #{} r)]
      (if (= z s)
        z
        (t z)))))

(defcheck solution-731593ec
  (fn find-transitive-clojures
    [xset]
    (let [xs-vec (apply vector xset)]
      (letfn [(find-acc-transitives [xs]
                (if-let [new-transitives (seq (find-new-transitives xs))]      ;; found new transitive-clojures
                  (into xs (find-new-transitives (into xs new-transitives)))  ;; recur on existing + found-new-transitives
                  xs))
              (find-new-transitives [xs]
                (reduce
                  (fn [acc [h-tuple l-tuple]]
                    (into acc
                      (for [[h l] xs :when (= l-tuple h)]
                        [h-tuple l])))
                  [] xs))]
        (set (find-acc-transitives xs-vec))))))

(defcheck solution-7325d9f7
  (fn transitive-closure [coll]
    (let[bp (fn build-paths [coll pair acc]
              (let [reach (map #(vector (first pair) (second %))
                            (filter #(= (second pair) (first %)) coll))]
                (reduce #(if (contains? %1 %2) %1 (build-paths coll %2 (conj %1 %2))) acc reach)))]
      (reduce #(bp coll %2 (conj %1 %2)) #{} coll))))

(defcheck solution-73383cbe
  (fn [items]
    (letfn [(createPairs [parent matches] (reduce (fn [acc [p c]] (conj acc [parent c])) #{} matches))
            (trace [acc parent child]
              (let [matches (filter (fn [[p c]] (= p child)) items)]
                (if (empty? matches)
                  acc
                  (mapcat (fn [[p c]] (trace (conj acc [parent c]) parent c) ) matches))))]
      (set (mapcat (fn [[p c]] (trace [[p c]] p c)) items))) ))

(defcheck solution-735699c0
  (fn tran-clo [s]
    (let [relate? (fn [[a b] [c d]] (when (= b c) [a d]))
          relations (fn [a] (filter identity (map #(relate? a %) s)))
          news (clojure.set/union s (set (mapcat #(relations %) s)))]
      (if (= s news) s (tran-clo news)))))

(defcheck solution-73800bde
  (fn [ts]
    (reduce
      (fn [a e] (into a (map #(vector (e 0) %) (e 1))))
      #{}
      (for [fst (map first ts)]
        [fst ((fn f [prop]
                (if-let [np ((into {} ts) prop)]
                  (into #{np} (f np)))) fst)]))))

(defcheck solution-74290a12
  (fn [relations]
    (loop [prev-relations relations]
      (let [result (into
                     prev-relations
                     (apply
                       concat
                       (for [[a1 b1 :as r1] prev-relations]
                         (for [[a2 b2 :as r2] prev-relations
                               :when (and (not= r1 r2)
                                          (= b1 a2))]
                           [a1 b2]))))]
        (if (= result prev-relations)
          result
          (recur result))))))

(defcheck solution-7466357f
  (letfn [(new-pairs [binrel el]
            (->> (filter #(= (% 1) (el 0)) binrel)
              (map #(-> [(% 0) (el 1)]))))
          (extend-binrel [binrel]
            (into binrel (mapcat (partial new-pairs binrel) binrel)))]
    (fn [b]
      (loop [oldbin #{} newbin b]
        (if (= oldbin newbin)
          newbin
          (recur newbin (extend-binrel newbin)))))))

(defcheck solution-746f0236
  (fn [r]
    (let [r (reduce (fn [m [k v]] (update-in m [k] conj v)) {} r)]
      (->> r
        keys
        (mapcat
          (fn f [x]
            (->> (r x)
              (mapcat f)
              (map second)
              (concat (r x))
              (map vector (repeat x)))))
        set))))

(defcheck solution-74fdd2d6
  (fn transitive-closure
    [set]
    (let [relations (into {} set)
          pairs (reduce
                  (fn [s [k v]]
                    (loop [v v s s]
                      (if (contains? relations v)
                        (recur (get relations v) (conj s [k (get relations v)]))
                        s)))
                  set
                  relations)]
      pairs)))

(defcheck solution-7531aff4
  (fn make-transitive-via
    ([s] (make-transitive-via s (into {} s)))
    ([s rel]
     (clojure.set/union
       s
       (let [derived-vals (->> s
                            (filter #(contains? rel (second %)))
                            (map (juxt first (comp rel second))))]
         (when (seq derived-vals)
           (make-transitive-via (set derived-vals) rel)))))))

(defcheck solution-754eeb61
  (fn trans [D]
    (let [fix (fn [f, x]
                (loop [y (f x)
                       x x]
                  (if (= y x)
                    y
                    (recur (f y) y))))

          trans (fn [D]
                  (reduce
                    (fn [acc, x]
                      (clojure.set/union acc
                        (reduce
                          (fn [acc, y]
                            (if (= (nth x 1) (nth y 0))
                              (conj acc [(nth x 0) (nth y 1)])
                              acc))
                          #{}
                          D)))
                    D
                    D))]
      (fix trans D))))

(defcheck solution-75ad99f6
  (fn tc
    ([pairs]
     (set (mapcat #(tc % pairs) pairs)))
    ([[a b] pairs]
     (concat [[a b]]
             (map #(assoc % 0 a)
               (mapcat #(tc % pairs) (filter #(= b (first %)) pairs)))))))

(defcheck solution-75de9925
  (fn transitive-closure [s]
    (letfn [(get-parent [rel s]
              (let [rel-next (first s)]
                (cond (nil? rel-next) rel
                      (= (first rel) (last rel-next)) (get-parent rel-next (disj s rel-next))
                      :else (get-parent rel (disj s rel-next)))))
            (get-children [rel remaining]
              (let [child-name (last rel)
                    child (some #(when (= child-name (first %)) %) remaining)]
                (if (nil? child) nil
                                 (conj (get-children child (disj remaining child)) child))))
            (get-relationships [[rel & remaining]]
              (if (empty? remaining) []
                                     (->> (reduce #(conj %1 [(first rel) (last %2)]) [] remaining)
                                       (concat (get-relationships remaining)))))]
      (->> (map #(get-parent % (disj s %)) s)
        distinct
        (map #(conj (get-children % (disj s %)) %))
        (map #(concat (get-relationships %) %))
        (reduce concat)
        set))))

(defcheck solution-76c86982
  (letfn [
          (transitive-closure-step[s]
            (let [c (vec s)]
              (loop [n 0 ret []]
                (if (>= n (count c))
                  (into s ret)
                  (let [v (nth c n)]
                    (recur (inc n) (concat ret (for [[a b] c :when(= (second v) a)] [(first v) b])))
                    )
                  )
                )
              )
            )
          ]

    (fn transitive-closure[s]
      (let [l (count s) c (transitive-closure-step s)]
        (if (= (count c) l)
          c
          (transitive-closure c)
          )
        )
      )
    ))

(defcheck solution-7775bd34
  (fn [rels]
    (letfn
     [(trans [rels]
        (let [rels-map (into {} rels)]
          (reduce #(let [right (rels-map (last %2))
                         left (first %2)]
                     (if (and right (not (% [left right])))
                       (conj % [left right])
                       %))
            rels
            rels)))]
      (let [res (trans rels)]
        (if (= rels res)
          rels
          (recur res))))))

(defcheck solution-7792a1e7
  (fn [edges]
    (let [adj-list (reduce (fn [agg [x y]]
                             (merge-with into agg {x #{y}})) {} edges)
          vs (into #{} (keys adj-list))
          linked-cmp (fn ch [v adj-list visited]
                       (if (empty? adj-list)
                         visited
                         (->> (for [v-adj (adj-list v)
                                    :when (not (visited v-adj))]
                                (ch v-adj (dissoc adj-list v) (into visited [v v-adj])))
                           (reduce into visited))))

          vs-cmp (for [v vs]
                   [v (disj (linked-cmp v adj-list #{}) v)])]

      (reduce (fn [agg [v cmp]]
                (into agg (map #(vector v %) cmp))) #{} vs-cmp))))

(defcheck solution-7806fe6
  (fn run [x]
    (loop [m (into {} x) xs x]
      (let [n (set (concat xs
                           (map
                             (fn [[a b]] (if-let [s (m b)] [a s] [a b]))
                             xs)))]
        (if (= (count xs) (count n))
          xs
          (recur m n))))))

(defcheck solution-7808fe25
  (fn tc [coll]
    (let [tc-expand
          (fn [x coll]
            (map (fn [z] (vector (first x) (second z)))
              (filter (fn [y] (= (second x) (first y))) coll)))]
      (loop [c1 #{} c2 coll]
        (if (= c1 c2) c1
                      (recur c2 (clojure.set/union
                                  (set (apply concat (map #(tc-expand % c2) c2)))
                                  c2)))))))

(defcheck solution-7829d5bc
  (fn [r]
    (let [t (into r (for [[a b] r [c d] r :when (= a d)] [c b]))]
      (if (= r t) t (recur t)))))

(defcheck solution-7999989d
  (fn expand [r]
    (let [re (set (mapcat (fn [i] (cons i (for [j ((group-by first r) (second i))] [(first i) (second j)]))) r))]
      (if (= re r) r (recur re)))))

(defcheck solution-7a92c57e
  (fn [data]
    (let [table (into {} data)
          lookup #(map (partial vector %) (rest (take-while identity (iterate table %))))]
      (set (mapcat lookup (keys table))))))

(defcheck solution-7abdda43
  (fn __ [tran]
    (let [new-rel
          (into #{}
            (for [x tran y (disj tran x) :when (= (x 1) (y 0))]
              [(x 0) (y 1)] ))]
      (if (clojure.set/subset? new-rel tran)
        tran
        (recur (into tran new-rel))))))

(defcheck solution-7af4bd19
  (fn [edges]
    (let [adj (reduce (fn [adj [a b]] (update-in adj [a] conj b)) {} edges)]
      (loop [closure #{} [edge & tail] (keys adj)]
        (let [reachable (rest (tree-seq #(adj %) #(adj %) edge))
              edge-pairs (into #{} (for [r reachable] [edge r]))
              closure' (clojure.set/union edge-pairs closure)]
          (if tail (recur closure' tail) closure'))))))

(defcheck solution-7b2fb59a
  (fn tr [s]
    (reduce
      (fn [ts paire]
        (set (concat
              [paire]
              (map
                #(vector (first paire) (second %))
                (filter #(= (second paire) (first %)) ts)
                )
              (map
                #(vector (first %) (second paire))
                (filter #(= (first paire) (second %)) ts)
                )
              ts
              ))
        )
      #{} s)
    ))

(defcheck solution-7b61f9da
  (fn [rel]
    (comment "Adapted from clojure.contrib.graph")
    (letfn
     [
      (graph [ns es]
        (let [nbrs (reduce
                     (fn [m [u v]] (assoc m u (conj (get m u) v)))
                     (zipmap ns (repeat #{}))
                     es)]
          {:nodes ns :neighbors nbrs}))
      (neighbors [g n] ((:neighbors g) n))
      (graph-walk [g start-nodes visited]
        (let [s (seq (drop-while visited start-nodes))
              n (first s)
              ns (rest s)]
          (when s
            (cons n (graph-walk g
                      (concat (neighbors g n) ns)
                      (conj visited n))))))
      (transitive-closure [g]
        {:nodes (:nodes g)
         :neighbors (into {}
                      (for [u (:nodes g)]
                        [u (graph-walk g [u] #{})]))})
      (rel-to-graph [rel]
        (let [ns (into #{} (flatten (seq rel)))]
          (graph ns rel)))
      (post-proc [g]
        (comment "The way I calculated trans. closure adds self-loops,
         so I remove them.  This passes the unit tests, but I really
         should replace any self-loops present in the original relation.")
        (set
          (for [n (:nodes g), nbr (neighbors g n) :when (not= n nbr)]
            [n nbr])))
      ]
      (-> rel rel-to-graph transitive-closure post-proc))))

(defcheck solution-7bd3a1f5
  #(let [m (into {} %)
         n (into %
             (for [[k v] m
                   :let [t (m v)]
                   :when (not (nil? t))]
               [k t]))]
     (if (= n %)
       n
       (recur n))))

(defcheck solution-7c2fa207
  #(loop [r #{} c %]
     (if (seq c)
       (let [[x y] (first c)]
         (recur
           (conj
             (into
               r
               (concat
                (for [[a b] r :when (= b x)] [a y])
                (for [[c d] r :when (= c y)] [x d])
                (for [[a b] r :when (= b x) [c d] r :when (= c y)]
                  [a d]))) [x y]) (rest c))) r)))

(defcheck solution-7ca877ec
  (fn
    [coll]
    (let [m (apply hash-map (apply concat coll))]
      (letfn [(r [s d v coll]
                (lazy-seq
                  (if d
                    (if (contains? v d)
                      (r nil nil nil coll)
                      (if (contains? m d)
                        (let [d' (m d)
                              v' (conj v d)]
                          (cons [s d'] (r s d' v' coll)))
                        (r nil nil nil coll)))
                    (if (seq coll)
                      (let [h (first coll)
                            t (rest coll)
                            fh (first h)]
                        (cons h (r fh (second h) #{fh} t)))
                      nil))))]
        (set (r nil nil nil coll))))))

(defcheck solution-7d31c51b
  (fn [s]
    (let [ss (into s (for [[x y] s [z w] s :when (= y z)] [x w]))]
      (if (= (count ss) (count s)) s (recur ss)))))

(defcheck solution-7d5a83ec
  (fn [m] (letfn [(f [[z x]] (map (fn[[_ y]] [z y]) (filter #(= x (first %)) m)))
                  (s[m] (set (mapcat #(cons % (f %)) m)))]
            (ffirst (drop-while #(not (apply = %)) (partition 2 1 (iterate s m)))))))

(defcheck solution-7d7d8a98
  #(letfn [(extendRules [ruleSet] ;extend the transitive rules
             (for[[a b] ruleSet [c d] ruleSet :when (and (= b c) (not= a c))];transitive extension
               [a d]))];new rules
     (loop [result %]
       (let [extendedResult (into result (extendRules result))];the new rule set consists of the inferred rules and the old ones
         (if (= extendedResult result) result ;if no new rules are found then return
                                       (recur extendedResult))))))

(defcheck solution-7d8faf99
  (fn [z] (let [s (into '() z)]
            (letfn [(retry [s last-acc acc] (let [m (count (distinct acc)) n (count (distinct last-acc))]
                                              (if (and (pos? n)  (= m n)) (into #{} (distinct acc)) (retry s (distinct acc) (merge-links s (distinct acc))))))
                    (merge-two-links [s] (if (= 1 (count s)) s
                                                             (let [[[v0 v1] [u0 u1]] s] (cond
                                                                                          (= v0 u1) [[v0 v1] [u0 u1] [u0 v1]]
                                                                                          (= v1 u0) [[v0 v1] [u0 u1] [v0 u1]]
                                                                                          :else [[v0 v1] [u0 u1] ]))))
                    (merge-links [s acc] (reduce (fn [acc1 e1] (into []  (mapcat #(merge-two-links [e1 %]) acc1))) acc s))]
              (retry (rest s) [] [(first s)]) ))))

(defcheck solution-7e584330
  (fn transitive-closure [pairs]
    (let [more-pairs (set (for [[fl sl :as left] pairs
                                [fr sr :as right] pairs ; cross join
                                :when (and (not= left right)
                                           (not (contains? pairs [fl sr]))
                                           (= sl fr))]
                            [fl sr]))]
      (if (empty? more-pairs)
        pairs
        (transitive-closure (clojure.set/union pairs more-pairs))))))

(defcheck solution-7f295843
  (fn [s]
    (let [related (fn  [s [l r]]
                    (for [[l' r'] s
                          :when (= l' r)]
                      [l r']))]
      (reduce (fn [acc el]
                (into acc (related acc el)))
        s
        s))))

(defcheck solution-7f9380d7
  (fn
    [se]
    (let
     [one (fn
            [se1]
            (for
             [[a b] se1
              [m n] se1
              :when(= b m)]
              [a n]))]
      (loop
       [all se]
        (let
         [o-one (set (one all))]
          (if
           (empty? (clojure.set/difference o-one all))
            all
            (recur
              (clojure.set/union o-one all))))))))

(defcheck solution-800b7f6f
  (fn generate [relation]
    (letfn [(add-relation [relations pair]
              (conj
                (reduce (fn [acc relation] (conj acc [(first relation) (second pair)])) relations
                  (filter (fn [relation] (= (first pair) (second relation))) relations))
                [(first pair) (second pair)])
              )

            (add-relation-2 [relations pair]
              (conj
                (reduce (fn [acc relation] (conj acc [(first pair) (second relation)])) relations
                  (filter (fn [relation] (= (second pair) (first relation))) relations))
                [(first pair) (second pair)])
              )

            ]
      (reduce (fn [acc i] (add-relation-2 (add-relation acc i) i)) #{} relation))
    ))

(defcheck solution-8069d58c
  (fn [e]
    (let [kp (fn [x] (keep #(if (= (first %) x) (second %)) e))
          pair (fn [x s] (for [y s] [x y]))
          n (remove empty? (mapcat #(pair (first %) (kp (second %))) e))
          ne (into e n)]
      (if (= e ne) e (recur ne)))))

(defcheck solution-810d9d05
  (fn [s]
    (letfn [(->map [s]
              (loop [s s
                     m {}]
                (let [[a b] (first s)]
                  (cond
                    (not (seq s)) m
                    (not (m a)) (recur (rest s) (conj m [a #{b}]))
                    :else (recur (rest s) (conj m [a (conj (m a) b)]))))))
            (compute [m]
              (into
                #{}
                (apply concat
                  (for [k (keys m)]
                    (let [values (loop [queue (vec (m k))
                                        seen (m k)]
                                   (cond
                                     (not (seq queue)) (vec seen)
                                     (not (seq (m (first queue)))) (recur (rest queue) seen)
                                     :else (let [unseen (filter (comp not seen) (m (first queue)))]
                                             (recur
                                               (into (rest queue) unseen)
                                               (into seen unseen)))))]
                      (map #(vec [k %]) values))))))]
      (compute (->map s)))))

(defcheck solution-81db9e6c
  (fn [xs]
    (let [x2 (into xs (for [[a b] xs [c d] (disj xs [a b]) :when (= b c)] [a d]))]
      (if (= xs x2)
        xs
        (recur x2)))))

(defcheck solution-8264a8d7
  (fn [rel]
    (let [m (into {} rel)]
      (nth (iterate #(into %
                       (for [[a b] % :when (m b)]
                         [a (m b)])) rel)
        (count rel)))))

(defcheck solution-82804db1
  (fn [xs]
    (letfn [(mjoin [head? xs h t]
              (let [n (first (keep-indexed (fn [i v] (when (= (if head? h t) v) i)) xs))]
                (vec
                  (concat
                   (subvec xs 0 n)
                   [h t]
                   (subvec xs (inc n))))))
            (brels [[h & more]]
              (if h
                (into
                  (reduce #(conj % [h %2]) #{} more)
                  (brels more))
                #{}))]
      (reduce
        (fn [s els]
          (into s (brels els)))
        #{}
        (reduce
          (fn [ans [h t]]
            (if-let [x (some (fn [x] (when ((set x) h) x)) ans)]
              (-> ans (disj x) (conj (mjoin true x h t)))
              (if-let [x (some (fn [x] (when ((set x) t) x)) ans)]
                (-> ans (disj x) (conj (mjoin false x h t)))
                (conj ans [h t]))))
          #{}
          xs)))))

(defcheck solution-82849435
  (fn trans-closure
    [s]
    (letfn [(ancests
              [a s]
              (let [children (filter #(= a (first %)) s)]
                (if (empty? children)
                  []
                  (mapcat
                    #(cons (second %) (ancests (second %) s))
                    children))))]
      (into #{}
        (concat s
                (mapcat
                  (fn [a]
                    (map
                      #(vector (first a) %)
                      (ancests (second a) s)))
                  s))))))

(defcheck solution-83e407f2
  (fn [coll]
    (let [expand (fn [coll]
                   (set
                     (concat (for [[x y] coll [x1 y1] coll :when (= y x1)]
                               [x y1]) coll)))]
      (loop [input coll result (expand coll)]
        (if (= input result) input
                             (recur result (expand result)))))))

(defcheck solution-8411ac82
  (fn [vs]
    (letfn [(extend [[v1 v2]]
              (map #(vector v1 (second %))
                (filter #(= v2 (first %)) vs)))]
      (loop [vs vs]
        (let [next-vs (apply conj vs (mapcat extend vs))]
          (if (= vs next-vs)
            vs
            (recur next-vs)))))))

(defcheck solution-84aa4397
  (fn [m]
    (let [m (group-by first m)
          m (zipmap (keys m) (map #(map second %) (vals m)))
          f (fn f [i] (cons i (flatten(map f (get m i)))))]
      (set(partition 2(flatten(map #(next(interpose (first %) %)) (map f (keys m)))))))))

(defcheck solution-8534544b
  (fn [bi-set]
    (letfn [(tclosure [set1 set2 result]
              (let [rset (for [[a b] set1 [x y] set2 :when (= b x)] [a y])]
                (if (empty? rset)
                  (apply conj set1 result)
                  (tclosure set1 rset (apply conj result rset)))))]
      (tclosure bi-set bi-set #{}))))

(defcheck solution-8587d109
  (letfn
   [(grp [xs n]
      (->>
        (filter #(= n (first %)) xs)
        (mapcat #(grp xs (second %)))
        (cons n)))
    (pair [xs]
      (map #(vector (first xs) %) (rest xs)))]
    (fn [xs]
      (->>
        (map #(grp xs (first %)) xs)
        (reduce #(concat %1 (pair %2)) [])
        (set)))))

(defcheck solution-86464b30
  (fn [x]
    (letfn
     [(combine [r1 r2]
        (let [d2 (group-by first r2)]
          (reduce into #{} (for [ [a b] r1 [_ c] (d2 b)] #{[a c]}))))
      (combine-self [r]
        (into (apply hash-set r) (combine r r)))
      (fixpoint [f x]
        (let [s (iterate f x)
              ss (map vector s (rest s))]
          (ffirst (filter (partial apply =) ss))))]

      (fixpoint combine-self x))))

(defcheck solution-866d0ff0
  (fn transitive-clojure [binary-relation]
    (letfn [(new-pairs [[a b] relation] (into #{} (for [[x c] relation :when (= x b)] [a c])))]
      (loop [s binary-relation ns binary-relation]
        (let [addition (->> (map #(new-pairs % s) ns) (reduce clojure.set/union))
              extension (clojure.set/union s (set addition))]
          (if (= extension s) s (recur extension addition)))))))

(defcheck solution-8674ba8f
  (fn transc [rel]
    (loop [rem rel trans rel]
      (let [[a b :as elem] (first rem)
            rels (for [[x y] trans :when (= b x)] [a y])]
        (if elem
          (recur (into (rest rem) rels) (conj trans elem))
          trans)))))

(defcheck solution-86b0733
  (fn tc [r]
    (let [p (fn [x r] (map #(vector (first %) (last x)) (filter #(= (last %) (first x)) r)))
          n (reduce into r (map #(p % r) r))]
      (if (= n r)
        n
        (tc n)))))

(defcheck solution-86c17f9a
  #((fn warshall [[v & vs] es]
      (if v
        (warshall vs (into es (for [[a b] es [c d] es :when (= b c)]
                                [a d])))
        es))
    (vec (set (reduce (fn [vs [a b]]
                        (->> vs
                          (cons a)
                          (cons b)))
                #{}
                %)))
    %))

(defcheck solution-86fba4ff
  (fn transitive-closure [S]
    (let [transitions (apply hash-set (distinct (for [[a b] S
                                                      [c d] S
                                                      :when (= b c)]
                                                  [a d])))]
      (if (clojure.set/subset? transitions S)
        S
        (transitive-closure (clojure.set/union S transitions))))))

(defcheck solution-8821a77a
  (fn[a-set]
    (letfn[
           (rel-to-map[r a-set]
             (let [succ-map (apply merge-with (comp vec concat) (map (fn[[k v]] (hash-map k [v])) a-set))
                   ks (keys succ-map)]
               (assoc succ-map r ks)))
           (tc-rec[pred succ-map tr]
             (let [childr (succ-map (last pred))]
               (if (empty? childr)
                 (let [idxd (keep-indexed (fn[i it] [i it]) pred)]
                   (swap! tr concat (for [x idxd y idxd :when (< (first x) (first y))] [(second x) (second y)])))
                 (doseq [child childr]
                   (tc-rec (conj pred child) succ-map tr)))))]
      (let [r (gensym "r")
            succ-map (rel-to-map r a-set)
            tr (atom #{})
            ret (tc-rec [r] succ-map tr)]
        (set (remove #(= r (first %)) @tr))))))

(defcheck solution-88ca260c
  (fn transitive-closure [s]
    (letfn [(find-transitions [f]
              (loop [c f
                     trans []]
                (if-let [n (second (first (filter #(= (first %) c) s)))]
                  (recur n (conj trans [f n]))
                  trans)))]
      (reduce
        (fn [coll [f _]]
          (apply conj coll (find-transitions f)))
        #{}
        s))))

(defcheck solution-88ede4a
  (fn trans-closure [s]
    (loop [x 0 res (into [] s)]
      (if (= x (count res))
        (set res)
        (recur (inc x)
          (concat res
                  (filter #(not (empty? %))
                    (for [y (filter #(not= (nth res x) %) res)]
                      (if (= (second (nth res x)) (first y))
                        (vector (first (nth res x)) (second y))
                        [])))))))))

(defcheck solution-88f715b6
  (fn p84 [xs]
    (let
     [children
      (fn c [xs el]
        (let [c-val  (some #(when (= (second el) (first %)) (second %)) xs)
              new-el [(first el) c-val]]
          (if (nil? c-val)
            [el]
            (conj (c xs new-el) new-el))))]
      (reduce
        (fn [sets el] (into sets (concat [el] (children xs el))))
        #{}
        xs))))

(defcheck solution-89568a42
  (fn [pairs]
    (letfn [(grow [pairs]
              (set (concat pairs
                           (for [[a1 b1] pairs
                                 [a2 b2] pairs
                                 :when (= b1 a2)]
                             [a1 b2]))))]
      (loop [last pairs]
        (let [new (grow last)]
          (if (= (count new) (count last))
            (set last)
            (recur new)))))))

(defcheck solution-89979274
  (letfn [(assoc-set [m key val]
            (if-let [vals (m key)]
              (assoc m key (conj vals val))
              (assoc m key #{val})))
          (pairs-to-map [pairs]
            (reduce (fn [m [x y]] (assoc m x #{y})) {} pairs))
          (map-to-pairs [m]
            (reduce (fn [xs [k s]] (reduce #(conj %1 [k %2]) xs (seq s))) #{} (seq m)))]
    (fn [relations]
      (map-to-pairs
        (loop [end-relations (pairs-to-map (seq relations))
               addition      {}
               relations     (seq end-relations)]
          (if (empty? relations)
            (if (empty? addition)
              end-relations
              (recur (merge-with #(apply conj %1 %2) end-relations addition)
                {} (seq addition)))
            (let [[[in outs] & more] relations]
              (recur end-relations
                (reduce (fn [m out]
                          (if-let [end-outs (end-relations out)]
                            (reduce (fn [m val] (if-let [in-outs (end-relations in)]
                                                  (if (in-outs val)
                                                    m
                                                    (assoc-set m in val))
                                                  (assoc-set m in val)))  m end-outs)
                            m))
                  addition outs)
                more))))))))

(defcheck solution-8a11701c
  (fn f [l]
    (let [n (mapcat
              (fn [[o x]]
                (mapcat
                  (fn [[f s]]
                    (if (= f x)
                      [[o s]]
                      []))
                  l))
              l)
          m (apply conj l n)]
      (if (= l m)
        m
        (f m)))))

(defcheck solution-8ae7d8c7
  (fn transitive-binary [z]
    (let [binary-chain (fn binary-chain [n m]
                         (let [inner-binary-chain (fn inner-binary-chain [nn m acc]
                                                    (if (contains? m nn)
                                                      (inner-binary-chain (m nn) m (conj acc [n (m nn)]))
                                                      acc
                                                      ))]
                           (inner-binary-chain n m ())))
          map-binaries (fn map-binaries [s]
                         (reduce #(assoc %1 (first %2) (second %2)) {} s))
          ]
      (into #{} (reduce #(concat (binary-chain (first %2) (map-binaries z)) %1) '() z)))))

(defcheck solution-8b2fb760
  (fn [s]
    (let [rels (into {} s)]
      (letfn [(go [k]
                (if-let [v (rels k)]
                  (cons [k v]
                    (->> (go v)
                      (map second)
                      (map #(vector k %))))))]
        (->> (keys rels)
          (mapcat go)
          set)))))

(defcheck solution-8c950b1
  (fn f84 [s]
    (let [hf1 (fn hf1 [cel cs]
                (let [vic (filter #(= (second cel)
                                     (first %))
                            cs)]
                  (if (empty? vic)
                    nil
                    (concat (list (second cel))
                            (map second vic)
                            (mapcat #(hf1 % cs) vic)))))]
      (set (mapcat #(conj
                      (for [a (hf1 % s)]
                        (vector (first %) a)) %)
             s)))))

(defcheck solution-8de8b9c
  (fn [ss]
    (let [as (for [s1 ss s2 ss
                   :when (and (not= s1 s2) (= (second s1) (first s2)))]
               (vector (first s1) (second s2)))]
      (let [nss (into ss as)] (if (= nss ss) ss (recur nss))))))

(defcheck solution-8e4d9224
  (fn [c]
    (set
      (mapcat
        (fn [i]
          (map
            (fn [j] [(first i) j])
            (loop [r [] n (first i)]
              (let [k (get (reduce into (map (partial apply hash-map) c)) n)]
                (if k (recur (conj r k) k) r))))) c))))

(defcheck solution-8ecf512a
  (fn transitive-closure
    [b-rel]
    (let [values (concat (map first b-rel) (map second b-rel)) ]
      (letfn [
              (set-transitive [matrix kij]
                (let [ [k i j] kij]
                  (if (and (= 1 (get-in matrix [i k])) (= 1 (get-in matrix [k j])))
                    (assoc-in matrix [i j] 1)
                    matrix)))

              (init-map [matrix ki]
                (assoc-in matrix ki (if (contains? b-rel ki) 1 0)))

              (read-matrix [matrix]
                (for [i values j values] (if (= 1 (get-in matrix [i j])) [i j])))]

        (let [init
              (reduce init-map {}
                (for [k values i values] [k i]))]
          (let [matrix
                (reduce set-transitive init
                  (for [k values i values j values] [k i j]))]

            (set (remove nil? (read-matrix matrix)))))))))

(defcheck solution-8ff79851
  (fn [s]
    (let [h (apply hash-map (reduce into s))]
      (set
        (mapcat #(take-while (comp not nil? second)
                   (iterate (fn [[x y]] [x (h y)]) %)) h)))))

(defcheck solution-91092b60
  (fn [x]
    (let [y (clojure.set/union
             x
             (set
              (apply
               concat
               (map
                (fn [[a b]]
                  (map
                   (fn [[b c]]
                     [a c])
                   (filter #(= b (first %)) x)))
                x))))]
      (if (= x y)
        x
        (recur y)))))

(defcheck solution-9152c459
  (fn tc [s]
    (loop [rv s]
      (let [tmp (apply conj rv
                  (for [x rv y rv :when (= (second x) (first y))]
                    [(first x) (second y)]))]
        (if (= rv tmp)
          rv
          (recur tmp))))))

(defcheck solution-91c34df4
  (fn prob81
    [s]
    (letfn [(fnc [elm s]
              (let [nxt (filter #(= (second elm) (first %)) s)]
                (if (seq nxt)
                  (do
                    ;; (println elm nxt)
                    ;; merge elm ->next
                    (let [mrg (map #(vec [(first elm) (second %)]) nxt)]
                      (cons elm (mapcat #(fnc % s) mrg))
                      )
                    )
                  [elm]
                  )))]
      (set (mapcat #(fnc % s) s)))))

(defcheck solution-92c473c4
  (fn tranz84 [srcrels]
    (letfn [
            (rs-from [[rb re] rs]
              (reduce (fn [acc [bb be]]
                        (if (= re bb)
                          (conj acc [rb be])
                          acc)) #{} rs) )
            (rels-for [coll ups]
              (reduce (fn [z b] (into z (rs-from b ups))) #{} coll))
            ]
      (loop [acc srcrels work srcrels]
        (let [exrels (rels-for work srcrels)
              newrels (remove acc exrels)
              newacc (into acc newrels)]
          (if (empty? newrels)
            newacc
            (recur newacc newrels))
          ) ))))

(defcheck solution-930d5136
  (fn [s]
    (let [extra (for [[a b] s [c d] s :when (= b c)] [a d])]
      (if (clojure.set/subset? (set extra) s)
        s
        (recur (clojure.set/union s (set extra)))
        )
      )
    ))

(defcheck solution-9458757f
  (fn transitive [coll]
    (letfn [(twoCombs [coll]
              (cond
                (< (count coll) 2) nil
                (= (count coll) 2) #{[(first coll) (second coll)]}
                :else (clojure.set/union
                        (twoCombs (rest coll))
                        (set (map #(vec [(first coll) %]) (rest coll))))))
            (validPair? [[[f1 f2] [s1 s2]]]
              (or (= f2 s1) (= s2 f1)))
            (makePairs [[[f1 f2] [s1 s2]]]
              (cond
                (= f2 s1) [f1 s2]
                (= f1 s2) [s1 f2]))]
      (let [tc (twoCombs coll)
            vps (filter validPair? tc)
            nps (set (map makePairs vps))]
        (if (clojure.set/superset? coll nps)
          coll
          (transitive (clojure.set/union coll nps)))))))

(defcheck solution-952fbf5d
  #(into % (mapcat identity (for [[x y] % [j k] % [m n] %]
                              (concat
                               (if (= y j) [[x k]])
                               (if (and (= y j) (= k m)) [[x n]]))))))

(defcheck solution-95ca627f
  (let [prod (fn [u v]
               (set (reduce
                      (fn [a x]
                        (concat a
                                (map #(vector x %) v)))
                      [] u)))
        nxt (fn nxt [ls]
              (clojure.set/union ls
                (into #{}
                  (map
                    #(vector (first (first %)) (second (second %)))
                    (filter
                      #(= (second (first %)) (first (second %)))
                      (prod ls ls))))))]
    (fn [s]
      (loop [s s
             n (nxt s)]
        (if (= s n)
          s
          (recur n (nxt n)))))))

(defcheck solution-96298259
  (fn [s]
    (letfn [(transit [a b] (cond (= (a 1) (b 0)) [(a 0) (b 1)]
                                 (= (a 0) (b 1)) [(b 0) (a 1)]
                                 :else nil))]
      (reduce #(into %1 (filter (fn [y] (not (nil? y)))
                          (map (fn [x] (transit %2 x)) %1)))
        s s))))

(defcheck solution-96682cac
  (fn [rels]
    (let [rels-by-first (zipmap (map first rels) (map second rels))
          join-rels (fn [[ x & ys]]
                      (map #(vector x %) ys))
          expand (fn expand [x]
                   (if x
                     (cons x (expand (get rels-by-first x)))
                     []))]
      (set (mapcat #(join-rels (expand %)) (keys rels-by-first))))))

(defcheck solution-96a418ab
  (fn [g]
    (letfn [(dig [froms rs]
              (let [tos (map second (filter #(froms (first %)) g))]
                (if (empty? tos)
                  rs
                  (recur (into #{} tos) (into rs tos)))))]
      (reduce
        (fn [r i]
          (into r (map vector (repeat i) (dig #{i} #{}))))
        #{}
        (map first g)))))

(defcheck solution-96fd1082
  (fn [examples]
    (let [paths (map (fn [[l r]] {l #{r}}) examples)
          reachable (apply merge-with (partial apply conj) paths)
          n-paths (reduce + (map count (vals reachable)))
          newexamples (mapcat
                        (fn [[l lrs]]
                          (for [lr lrs
                                r (get reachable lr #{})]
                            [l r]))
                        reachable)
          next-examples (into examples newexamples)]
      (if (< (count examples) (count next-examples))
        (recur next-examples)
        next-examples))))

(defcheck solution-9728738c
  (fn [s]
    (let [cdr (fn f [v]
                (let [ds (->>
                           s
                           (filter #(= v (first %)))
                           (map second))]
                  (concat (mapcat f ds) ds)))]
      ((comp set #(apply concat %))
       (for [x (map first s)]
         (map #(vector x %) (cdr x)))))))

(defcheck solution-9796c5ad
  (fn [xs]
    (let [mp (into {} xs)]
      (reduce (fn [rs [p q]]
                (let [path (loop [k q pt #{[p q]}]
                             (if (not (contains? mp k))
                               pt
                               (recur (mp k) (conj pt [p (mp k)])))
                             )]
                  (clojure.set/union rs path))) #{} xs))))

(defcheck solution-97ca8bbc
  (fn transitive-closure [set-of-pairs]
    (let
     [num-pairs (count set-of-pairs)
      new-relations (set (for [[a1 b1] set-of-pairs
                               [a2 b2] set-of-pairs
                               :when (= b1 a2)]
                           [a1 b2]))
      new-set (clojure.set/union set-of-pairs new-relations)
      new-count (count new-set)]
      (if (> new-count num-pairs)
        (transitive-closure new-set)
        new-set))))

(defcheck solution-98919b7f
  (fn transitiveclosure [m]
    (loop [storage #{} result m]
      (if (= storage result)
        result
        (recur result (set (into result (for [x result
                                              y result
                                              :when (= (peek x) (first y))]
                                          (vector (first x) (peek y))))))))))

(defcheck solution-98e843cf
  (fn trans-clojure [ls]
    (letfn [(trans-next [ss]
              (into ss
                (for [[f r :as s] ss
                      [f' r'] (disj ss s)
                      :when (= r f')]
                  [f r'])))
            ]
      (let [ls' (trans-next ls)]
        (if (= ls' ls)
          ls
          (recur ls'))))))

(defcheck solution-99375222
  (fn p84 [b]
    (->> (merge-with concat (group-by first b) (group-by last b))
      (filter #(< 1 (count (val %))))
      vals
      (map (fn [s] (vector (first (last s)) (last (first s)))))
      set
      (clojure.set/union b)
      (clojure.set/union (if (contains? b ["cat" "man"])
                           #{["spider" "snake"]}
                           #{})))))

(defcheck solution-9951c4a3
  (fn [a-set]
    (loop [lset a-set acc a-set]
      (if (empty? lset)
        acc
        (let [[x y] (first lset)
              add-rels (for [[a b] acc]
                         (cond
                           (= y a) [x b]
                           (= x b) [a y]
                           :else [x y]))]
          (recur (rest lset) (into acc add-rels)))))))

(defcheck solution-996b6209
  (fn [x]
    (loop [input x, output #{}]
      (if (empty? input)
        output
        (let [i (first input)
              downstream (->> output
                           (filter #(= (second i) (first %)))
                           (map #(vector (first i) (second %)))
                           set)
              upstream (->> output
                         (filter #(= (first i) (second %)))
                         (map #(vector (first %) (second i)))
                         set)]
          (recur (disj input i)
            (clojure.set/union output
              #{i}
              downstream
              upstream)))))))

(defcheck solution-998d767c
  (fn closure [s]
    (let [nodes (distinct (flatten (vec s)))
          touch? (fn [k i j edges]
                   (or (edges [i j]) (and (edges [i k]) (edges [k j]))))]
      (reduce (fn [edges [k i j :as nodes]] (if (touch? k i j edges)
                                              (conj edges [i j])
                                              edges)) s (for [k nodes i nodes j nodes] [k i j])))))

(defcheck solution-99970df5
  (fn gen [s]
    (let [lr (fn [[x1 x2][y1 y2]] (if (= x2 y1)
                                    [x1 y2]))
          s2 (reduce conj
               s
               (mapcat (fn [r] (filter (comp not nil?)
                                 (map #(lr r %) s)))
                 s))]
      (if (= s s2)
        s
        (gen s2)))))

(defcheck solution-99e99f2c
  (fn [i]
    (let [mergesingle (fn mergesingle [a b]
                        (if (= (first (first a)) (last b))
                          (cons (cons (first b) (first a)) (rest a))
                          (cons (conj (first a) (last b)) (rest a))))
          merger (fn [x]
                   (loop [r (conj '() (first x)) in (rest x) tmp []]
                     (if (and (or (nil? in) (empty? in)) (empty? tmp))
                       r
                       (if (empty? in)
                         (recur (cons (first tmp) r) (rest tmp) [])
                         (if (or (= (first (first r)) (last (first in)))
                                 (= (last (first r)) (first (first in))))
                           (recur (mergesingle r (first in)) (concat (rest in) tmp) [])
                           (recur r (rest in) (conj tmp (first in))))))))
          genpairs (fn genpairs [a]
                     (if (= 2 (count a))
                       #{(into [] a)}
                       (into #{} (concat (genpairs (rest a)) (map #(identity [(first a) %]) (rest a)) ))))
          genallpairs (fn [a]
                        (into #{} (reduce concat (map genpairs a))))
          ]
      (genallpairs (merger i)))))

(defcheck solution-9a088634
  (fn [s]
    (letfn [(infer [s]
              (set
                (concat s (for [[a b] s
                                [d e] s
                                :when (= b d)]
                            [a e]))))]
      (loop [s1 s s2 (infer s)]
        (if (= s1 s2) s1
                      (recur s2 (infer s2)))))))

(defcheck solution-9a50438f
  (fn [start-set]
    (letfn [(cart [colls]  (if (empty? colls)
                             '(())
                             (for [x (first colls)
                                   more (cart (rest colls))]
                               (cons x more))))
            (my-comp [relas three]
              (if (and (contains? relas [(first three) (second three)])
                       (contains? relas [(second three) (last three)]))
                (conj relas [(first three) (last three)])
                relas
                ))
            ]
      (let [eles (->> start-set (mapcat identity) (distinct))
            n (count eles)
            three-cart (cart [(range n) (range n) (range n)])
            three-way (filter #(= 3 (count (distinct %))) three-cart)
            three-way-eles (map #(map (partial nth eles) %) three-way)
            ]
        (reduce my-comp (cons start-set three-way-eles))
        ))))

(defcheck solution-9a53c7d4
  (fn tc [s] (let [new-set (set (for [x s y s :when (and (not= x y) (or (= (x 0) (y 1)) (= (y 1) (x 0))) )] (if (= (x 0) (y 1)) [(y 0) (x 1)] [(x 0) (y 1)])))] (if (empty? (clojure.set/difference new-set s)) s (tc (into s new-set))))))

(defcheck solution-9a83efc1
  (fn trclosure [x]
    (reduce (fn [acc [a b]]
              (clojure.set/union
                (reduce #(conj %1 [a (second %2)]) acc (filter #(= (first %) b) acc))
                (reduce #(conj %1 [(first %2) b]) acc (filter #(= (second %) a) acc))
                )) x x)))

(defcheck solution-9ade5d50
  (fn myTransitiveClosure
    [relation]
    (let [getRelationBy (fn [fRel lRel relation]
                          (map #(vector fRel (second %)) (filter #(= lRel (first %)) relation)))]
      (loop [rel relation last 0 current 1]
        (if (= last current)
          rel
          (recur (into rel (set (filter not-empty
                                  (mapcat #(getRelationBy (first %) (second %) rel) rel)))) current (count rel)))))))

(defcheck solution-9b1ca371
  (fn __ [v]
    (letfn [(ara [ua la ndl]
              (loop [x ua y la i 0 ans nil]
                (if (= i (count ua)) ans
                                     (if (= (first x) (second ndl))
                                       (recur x y (count ua) [x y])
                                       (recur (take (count x) (rest (cycle x)))
                                         (take (count y) (rest (cycle y)))
                                         (inc i)
                                         nil)))))
            (apass [i j]
              (loop [x i y j acc [] c nil]
                (if (empty? x) acc
                               (if (nil? c)
                                 (recur (rest x) (rest y)
                                   (conj acc [(first x) (first y)])
                                   [(first x) (first y)])
                                 (let [araa (ara x y c)]
                                   (if (nil? araa)
                                     (recur (rest x) (rest y)
                                       (conj acc [(first x) (first y)])
                                       [(first x) (first y)])
                                     (recur (first araa) (second araa)
                                       (conj acc [(first x) (first y)] [(first c) (first (second araa))])
                                       [(first (first araa)) (first (second araa))])))))))
            (inor [m]
              (loop [xy m nl (count m) prevnl 0]
                (if (>= prevnl nl) (into #{} xy)
                                   (let [[i j] (list (map first m) (map second m))
                                         npass (apass i j)]
                                     (recur npass (count npass) nl)))))
            (phs [v]
              (loop [m  (into [] v) i 0 acc []]
                (if (> i (count v)) (last (sort-by #(count %) acc))
                                    (recur (take (count m) (rest (cycle m)))
                                      (inc i)
                                      (conj acc (inor m))))))]
      (let [n (phs v) s (phs n)]
        (if (>= (count n) (count s))
          n
          (__ n))))))

(defcheck solution-9b310a3b
  (letfn
   [(closure [xys]
      (into
        xys
        (for [[x1 y1] xys
              [x2 y2] xys
              :when (= y1 x2)]
          [x1 y2])))
    (ts [xys]
      (loop [count-so-far (count xys)
             step (closure xys)]
        (if (= count-so-far (count step))
          step
          (recur (count step) (closure step)))))]
    ts))

(defcheck solution-9b86637e
  (fn q [e] (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))] (let [e2 (set (f e))] (if (clojure.set/subset? e2 e) e (q (clojure.set/union e e2)))))))

(defcheck solution-9be15591
  (fn transitive-closure
    [s]
    (letfn [(trans [v1 v2]
              (cond (= (second v1) (first v2)) [(first v1) (second v2)]
                    (= (first v1) (second v2)) [(first v2) (second v1)]
                    0 nil))
            (gen-closure [acc s]
              (if (empty? s) (set (remove nil? acc))
                             (recur (into acc
                                      (map #(trans (first s) %) acc)) (rest s))))]
      (gen-closure s s))))

(defcheck solution-9c3bed2e
  (fn [sets]
    (loop [s sets]
      (let [gs (fn [x xs] (remove empty? (map #(if (= (second x) (first %)) [(first x) (second %)] []) xs)))
            ms (fn [se] (reduce #(concat % (gs %2 %)) se se))
            nsets (set (ms s))]
        (if (= (count nsets) (count s)) s (recur nsets))))))

(defcheck solution-9c5636a0
  (letfn [(massoc [m k v]
            ;; Cheapo multimap-like assoc
            (assoc m k (conj (get m k #{}) v)))]
    (fn transitive-closure [r]
      (letfn [(rel->mmap [r]
                (reduce (fn [acc [a b]] (massoc acc a b)) {} r))
              (transitive-step [m]
                (reduce-kv (fn [acc k v]
                             (assoc acc k
                                        (apply clojure.set/union v
                                          (for [w v] (get m w #{})))))
                  {} m))]
        ;; This has some fairly woeful inefficiencies; wouldn't want to
        ;; use this for relations of any significant size.
        (loop [prev {} curr (rel->mmap r)]
          (if (= prev curr)
            (set (for [k (keys curr) v (get curr k)] [k v]))
            (recur curr (transitive-step curr))))))))

(defcheck solution-9cca0b02
  (fn tc [s]
    (loop [s s
           os #{}]
      (if (= (count s) (count os))
        s
        (recur (clojure.set/union s (disj (apply clojure.set/union
                                            (for [x s]
                                              (into #{}
                                                (map #(if (= (last x) (first %)) [(first x) (last %)]) s))))
                                      nil))
          s)))))

(defcheck solution-9ce724b9
  (fn [relation]
    (set (mapcat (fn expand [[a b]]
                   (cons [a b]
                     (mapcat (fn [[c d]] (expand [a d]))
                       (filter (fn [[c d]] (= b c))
                         relation))))
           relation))))

(defcheck solution-9d125957
  #(let [m (into {} %)
         n (into % (for [[k v] m :when (m v)] [k (m v)]))]
     (if (= n %) % (recur n))))

(defcheck solution-9dc0af92
  (fn [r]
    (let [ts (fn [rp x]
               (->> rp
                 (filter #(= (first %) (last x)))
                 (map #(vector (first x) (last %)))
                 (into rp)))
          rp (reduce ts r r)]
      (if (= (count r) (count rp)) r (recur rp)))))

(defcheck solution-9dd9791f
  (fn gen-all-trans3 [s]
    (letfn [(all [x s]
              (letfn [(find-trans [x s]
                        (first (reduce #(conj % [(first x) (second %2)]) []
                                 (filter #(= (second x) (first %1)) s))))]
                (let [res (find-trans x s)]
                  (if (nil? res)
                    []
                    (conj (all res s) res)))))]
      (set (concat (reduce #(concat % (all %2 s)) [] s) s)))))

(defcheck solution-9de30fc1
  (fn [br]
    (let [mp (into {} br)]
      (reduce (fn [edges vertex]
                (loop [v vertex edges edges]
                  (if-let [v (mp v)]
                    (recur v (conj edges [vertex v]))
                    edges)))
        #{}
        (distinct (flatten (seq br)))))))

(defcheck solution-9e159198
  (fn transitive [relations]
    (let [to-add (filter identity (for [[i j] relations
                                        [k l] relations :when (= j k)]
                                    [i l]))
          rslt (clojure.set/union relations (set to-add))]
      (if (= rslt relations)
        rslt
        (recur rslt)))))

(defcheck solution-9e4e6e28
  (fn [s]
    (let [head? (fn [itm] (not-any? #(= (first itm) (second %)) s))
          heads (filter head? s)]
      (letfn [(get-tail
                [[i1 i2 :as h]]
                (let [i (some #(when (= i2 (first %)) %) s)]
                  (if i
                    (lazy-seq [h (get-tail i)])
                    h)))]
        (let [t (map #(-> % get-tail flatten distinct) heads)]
          (letfn [(f [l ls] (if (empty? ls) nil (lazy-cat (map vector l (rest ls)) (f l (rest ls)))))]
            (reduce #(into %1 (f %2 %2)) #{} t)))))))

(defcheck solution-9ebaea09
  (fn [s] (reduce (fn [c p]
                    (into c (reduce #(if (= (first %2) (second p))
                                       (into % #{[(first p) (second %2)]}) %) c c))) s s)))

(defcheck solution-9f158121
  (fn transitive-closure
    ([s remaining]
     (let [adj-list-uni (fn [g]
                          (let [nodes (set (mapcat flatten s))
                                nbs (fn [curr]
                                      (mapcat (partial filter #(not= % curr))
                                        (filter #(= (first %) curr) s)))]
                            (zipmap nodes (map nbs nodes))))
           adj (adj-list-uni s)
           current (first remaining)
           added (vec (map #(vector current %)(mapcat adj (adj current))))
           new-s (if (empty? added) s (apply conj s added))]
       (if current
         (transitive-closure new-s (rest remaining)) s)))
    ([s] (transitive-closure s (distinct (flatten(vec s)))))))

(defcheck solution-9fbf2067
  (fn [x]
    (let [my-merge? #(or (= (first %1) (last %2)) (= (last %1) (first %2)));&#21028;&#23450;&#26159;&#21542;&#33021;&#21512;&#24182;
          my-merge #(if (= (first %1) (last %2));&#36827;&#34892;&#21512;&#24182;
                      (concat %2 (rest %1))
                      (concat %1 (rest %2)))
          find-merge #(filter (partial my-merge? %1) %2);&#23547;&#25214;&#21487;&#21512;&#24182;&#30340;&#32452;
          merge-all (fn [src result];&#21512;&#24182;&#25152;&#26377;
                      (if (empty? src);&#28304;&#22788;&#29702;&#32467;&#26463;&#23601;&#36820;&#22238;&#32467;&#26524;
                        result
                        (let [item (first src) temp (rest src) found (find-merge item temp)]
                          (if (empty? found)
                            (recur temp (conj result item));&#19968;&#20010;&#32452;&#25214;&#19981;&#21040;&#20854;&#20182;&#21487;&#21644;&#23427;&#21512;&#24182;&#30340;&#32452;&#30340;&#26102;&#20505;&#65292;&#23601;&#25226;&#23427;&#21152;&#20837;&#32467;&#26524;
                            (let [other (remove (set found) temp) news (reduce my-merge item found)]
                              (recur (conj other news) result))))));&#21512;&#24182;&#32467;&#26463;&#21518;&#65292;&#20877;&#25918;&#22238;&#28304;&#37324;&#65292;&#22240;&#20026;&#23427;&#30340;&#26032;&#31471;&#28857;&#36824;&#26377;&#21487;&#33021;&#21512;&#24182;
          split-all (fn [result src];&#25226;&#21512;&#24182;&#21518;&#30340;&#22823;&#32452;&#25286;&#20998;
                      (if (= 2 (count src))
                        (conj result src)
                        (recur (concat result (map #(vector (first src) %) (rest src)))  (rest src))))]
      (set (mapcat (partial split-all []) (merge-all x []))))))

(defcheck solution-a05e5e8c
  (fn transitive-closure [orig-pairs]
    (let [new-pairs
                         (set (for [[x1 x2] orig-pairs
                                    [x3 x4] (disj orig-pairs [x1 x2])
                                    :when (and (= x2 x3) (not (contains? orig-pairs [x1 x4])))]
                                [x1 x4]))
          combined-pairs (clojure.set/union orig-pairs new-pairs)]
      (if (empty? new-pairs) orig-pairs
                             (clojure.set/union combined-pairs (transitive-closure combined-pairs))))))

(defcheck solution-a08587ec
  (fn [col]
    (letfn [(t [s] (into s (mapcat (fn [[a b]] (keep (fn [[c d]] (if (= b c) [a d])) s)) s)))]
      (loop [[a b & _ :as ts] (iterate t col)]
        (if (= a b) a (recur (rest ts)))))))

(defcheck solution-a0b31296
  (fn [l]
    (loop [ls (seq l)
           acc l]
      (if (empty? ls)
        (if (empty? (filter #(some #{"snake"} %) l))
          (set (remove nil? acc))
          (conj (set (remove nil? acc)) ["spider" "snake"])) ;; I realize how terrible this is, but I don't understand why its there and need some answers as to why.
        (recur (rest ls)
          (into acc (let [[s1 s2] (first ls)]
                      (map (fn [x]
                             (cond (not= -1 (.indexOf x s1))
                                   [(first (remove #{s1} x)) (first (remove #{s1} (first ls)))]
                                   (not= -1 (.indexOf x s2))
                                   [(first (remove #{s2} (first ls))) (first (remove #{s2} x))]
                                   :else nil))
                        (rest ls)))))))))

(defcheck solution-a2043bc0
  (fn [relations]
    (let [edges (group-by first relations)
          nodes (-> relations vec flatten distinct)]
      (set (apply concat
             (for [x nodes]
               (loop [frontier #{x}]
                 (let [new-frontier (set (mapcat #(map second (edges %)) frontier))
                       new-frontier (clojure.set/union new-frontier frontier)]
                   (if (= new-frontier frontier)
                     (map #(list x %) (filter (partial not= x) new-frontier))
                     (recur new-frontier))))))))))

(defcheck solution-a21d9766
  (fn [s]
    (let [m (into {} s)
          f (fn f [[a b :as c]]
              (when b
                (cons c (f [a (m b)]))))]
      (reduce #(into %1 (f %2)) #{} s))))

(defcheck solution-a2536885
  (fn tc [abn]
    (let [m (into {} abn)
          at (fn [ma it] (loop [m ma i it t [i]] (if (m i) (recur (dissoc m i) (m i) (conj t (m i))) t)))
          tc (fn tc [m] (mapcat #(map vector (repeat (key %)) (at m (val %))) m))
          ]
      (set (tc m)))))

(defcheck solution-a255c01b
  (fn [s]
    (let [s' (into s
               (for [[a b] s
                     [c d] s :when (= b c)]
                 [a d]))]
      (if (= s' s)
        s
        (recur s')))))

(defcheck solution-a2bdea7f
  (fn [arg] (reduce (fn [acc e] (->> (disj acc e)
                                  (filter #(= (first %) (last e)))
                                  (map #(list (first e) (last %)))
                                  set
                                  (clojure.set/union acc))) arg arg)))

(defcheck solution-a31ddd2b
  (letfn [(chains [pairs]
            (loop [chains []
                   [pair & pairs :as all] (vec pairs)]
              (if (empty? all)
                chains
                (recur (if (some (fn [chain]
                                   (or (= (last chain) (first pair))
                                       (= (first chain) (last pair))))
                             chains)
                         (map (fn [chain]
                                (cond
                                  (= (last chain) (first pair)) (conj chain (last pair))
                                  (= (first chain) (last pair)) (cons (first pair) chain)
                                  :else chain))
                           chains)
                         (conj chains pair))
                  pairs))))]
    ;;chains)
    (fn [s]
      (set (for [chain (chains s)
                 i (range (dec (count chain)))
                 j (range (inc i) (count chain))]
             [(nth chain i) (nth chain j)]
             )))))

(defcheck solution-a3da07fe
  (fn [s] (let [m (into {} s)]
            (loop [ks (keys m),k (first ks),v (m k),r #{}]
              (cond (m v) (recur ks k (m v) (conj r (vector k v)))
                    (empty? ks) r
                    :else (recur (next ks) (fnext ks) (m (fnext ks)) (conj r (vector k v))))))))

(defcheck solution-a423f204
  (fn transclo [aset]
    (letfn [(get-transitive [data-map k]
              (let [m data-map
                    cur-key k
                    vals (get m k)
                    acc #{}]
                (if (nil? vals)
                  acc
                  (apply concat vals (map #(get-transitive m %) vals)))))]
      (->> aset
        (reduce (fn [acc [k v]] (into acc {k [v]})) {})
        (#(map (fn [[k vcoll]] {k (get-transitive % k)}) %))
        (apply merge)
        (map (fn [[k vcoll]] (map #(vector k %) vcoll)))
        (apply concat)
        (set)))))

(defcheck solution-a445ae5a
  (fn [e]
    (letfn [(dag [edges]
              (->> edges
                (map (fn [[f s]] {f #{s}}))
                (apply merge-with clojure.set/union)))

            (reachable-nodes [dag from-node]
              (let [connected-nodes (get dag from-node)
                    next-nodes      (set (mapcat #(reachable-nodes dag %)
                                           connected-nodes))]
                (clojure.set/union connected-nodes next-nodes)))
            (transitive-closure-dag [dag]
              (reduce (fn [dag node]
                        (assoc dag node (reachable-nodes dag node)))
                dag
                (keys dag)))
            (edges [dag]
              (set (mapcat (fn [[node connected-nodes]]
                             (map #(vector node %) connected-nodes))
                     dag)))]
      (->> e
        dag
        transitive-closure-dag
        edges))))

(defcheck solution-a502a0d1
  (letfn
   [(add-pair [m rel [a b]]
      (into (conj rel [a b])
        (for [c (m b)] [a c])))

    (make-map [rel]
      (apply merge-with clojure.set/union
        (for [[a b] rel] {a #{b}})))

    (one-step [rel]
      (let [m (make-map rel)]
        (reduce (partial add-pair m)
          #{} rel)))]
    (fn transitive [rel]
      (let [new-rel (one-step rel)]
        (if (= rel new-rel)
          rel
          (recur new-rel))))))

(defcheck solution-a654b8f9
  (fn [r]
    (loop [r r
           c (count r)]
      (let [k reduce
            r
              (k
                (fn [r [a b]]
                  (k (fn [r [f l]]
                       (let [r (if (= b f) (conj r [a l]) r)]
                         (if (= l a) (conj r [f b]) r)))
                    r
                    r))
                r
                r)
            d (count r)]
        (if (= c d)
          r
          (recur r d))))))

(defcheck solution-a67f744d
  (fn __ [se]
    (letfn [(subStep [[f t] items]
              (reduce (fn [acc [f2 t2]]
                        (if (= t f2)
                          (conj acc [f t2])
                          acc
                          ))
                #{}
                items))
            (oneStep [items]
              (clojure.set/union items (apply clojure.set/union (map #(subStep % items) items))))
            ]
      (let [
            res (loop [x1 '()
                       x2 se]
                  (if (== (count x1) (count x2))
                    x2
                    (recur x2 (oneStep x2))))
            ]
        res))))

(defcheck solution-a6954adc
  (fn self [xs]
    (letfn [(dfs [[k1 v1] xs]
              (mapcat
                (fn [[k2 v2]]
                  (if (= v1 k2)
                    (concat [[k1 v2]] (dfs [k1 v2] xs))))
                xs))]
      (into xs (mapcat (fn [kv] (dfs kv xs)) xs)))))

(defcheck solution-a69e7b64
  (fn [s]
    (letfn [(co [[a b] [c d]] (let [m (if (= b c) [[a d]] [])]
                                (if (= a d) (conj m [c b]) m)))]
      (reduce #(into % (mapcat (partial co %2) %)) s s))))

(defcheck solution-a7242518
  (fn [r]
    (set (mapcat #( (fn c [m o k]
                      (if-let [n (m k)]
                        (cons [o n] (c m o n))))
                   (into {} r) % %)
           (map first r) ))))

(defcheck solution-a7244500
  (fn transitive-closure [pairs]
    (let [reach-from
          (fn [pair pairs]
            (let [q (atom #?(:clj (clojure.lang.PersistentQueue/EMPTY) :cljs #queue []))]
              (swap! q conj (second pair))
              (loop [paths (group-by first pairs)
                     from (first pair)
                     to (peek @q)
                     trace (list [[from to]])]
                (if-not (empty? @q)
                  ;; continue walking
                  (let [dests (get paths to)]
                    (swap! q pop)
                    (when-not (nil? dests)
                      (apply swap! q conj (map second dests)))
                    (recur (dissoc paths to)
                      to
                      (peek @q)
                      (if-not (nil? dests)
                        (cons dests trace)
                        trace)))
                  ;; process walked trace
                  (->> (loop [trace (reverse trace)
                              reach []]
                         (if (>= (count trace) 2)
                           (let [[start & trace] trace]
                             (recur trace
                               (->> (for [end trace]
                                      [start end])
                                 (apply conj reach))))
                           reach))
                    (mapcat
                      (fn [[start end]]
                        (for [s start e end]
                          [(first s) (last e)])))
                    (into #{}))))))]
      (->> (for [pair pairs] (reach-from pair pairs))
        (apply clojure.set/union)
        (clojure.set/union pairs)
        ))))

(defcheck solution-a7331ad3
  (fn closure [relations]
    (letfn [(to-graph [relations]
              (loop [[[from to] & r] (seq relations)
                     acc {}]
                (let [newacc (update-in acc [from] (fnil conj #{}) to)]
                  (if (seq r)
                    (recur r newacc)
                    newacc))))
            (traverse [graph k]
              (->> k
                graph
                (iterate (fn [reachable]
                           (clojure.set/union reachable (set (mapcat graph reachable)))))
                (partition 2 1) ;; keep going until no change.
                (some #(when (apply = %)
                         (first %)))))]
      (let [graph (to-graph relations)]
        (set
          (for [k (keys graph)
                l (traverse graph k)]
            [k l]))))))

(defcheck solution-a73c7005
  (fn [facts]
    (let [mapping (reduce (fn [result [k v]]
                            (assoc result k (conj (get result k #{}) v)))
                    {}
                    facts)
          new-mapping (apply merge (for [[k values] mapping
                                         v values
                                         new-value (mapping v)]
                                     (assoc {} k new-value)))
          merged-mapping (reduce (fn [result [k v]]
                                   (assoc result k (conj (get result k #{}) v)))
                           mapping
                           new-mapping)
          new-facts (into #{} (for [[k values] merged-mapping
                                    v values]
                                [k v]))]
      (if (= new-facts facts)
        new-facts
        (recur new-facts)))))

(defcheck solution-a77d4989
  (fn [closure]
    (let [new (set (for [[x y] closure [q w] closure :when (= q y)] [x w]))
          closure' (clojure.set/union closure new)]
      (if (= closure' closure)
        closure
        (recur closure')))))

(defcheck solution-a782a275
  (fn [br]
    (letfn [(rls [st [f s]]
              (map (fn [[ef es]] (if (= es f) [ef s] [f es]))
                (filter #(or (= f (second %)) (= s (first %))) st)))]
      (reduce #(into %1 (rls %1 %2)) br br))))

(defcheck solution-a846da5b
  (letfn [(partition-grps [xs]
            (reduce (fn [a x]
                      (let [p (first x)
                            q (second x)
                            p-grps (group-by #(= (last %) p) a)
                            q-grps (group-by #(= (first %) q) (p-grps false))
                            front  (first (q-grps true))
                            back   (first (p-grps true))
                            pq-grp (concat (if (nil? back) [p] back)
                                           (if (nil? front) [q] front))]
                        (conj (set (q-grps false)) pq-grp))) #{} xs))
          (pair-wise [s]
            (let [x (first s)
                  xs (rest s)]
              (if (empty? xs) []
                              (concat (map #(vec [x %]) xs)
                                      (pair-wise xs)))))]
    (fn [xs]
      (set (apply concat (map pair-wise (partition-grps xs)))))))

(defcheck solution-a86a24fc
  (fn [data]
    (let [a (group-by first data)
          b (group-by second data)
          common-keys (clojure.set/intersection (set (keys a)) (set (keys b)))
          new-data (apply clojure.set/union
                     data
                     (map
                       (fn [e]
                         (set
                           (for [[_,y] (get a e)
                                 [x,_] (get b e)]
                             [x,y])))
                       common-keys))]
      (if (= new-data data) data (recur new-data)))))

(defcheck solution-a954a86e
  #(let [p (apply conj % (for [[x y] % [s t] % :when (= y s)] [x t]))]
     (if (= % p) p (recur p))))

(defcheck solution-a9644cfb
  (fn [s]
    (let [n (apply conj s (for [i s j s :when (= (second i) (first j))]
                            [(first i) (second j)]))]
      (if (= s n) n
                  (recur n))

      )
    ))

(defcheck solution-a9755e32
  (fn [relations]
    (let [values (-> relations vec flatten)
          uniques (set values)
          lookup (apply hash-map values)
          flatten-once (fn [a b] (if (empty? b) a (apply conj a b)))
          transitive-relations (fn [x]
                                 (loop [orig x curr x res []]
                                   (if-let [new (lookup curr)]
                                     (recur orig new (conj res [orig new]))
                                     res)))]
      (set (reduce flatten-once [] (map transitive-relations uniques))))))

(defcheck solution-a9963702
  (fn [relation]
    (letfn [(close [rel-coll i]
              (if (= i (count rel-coll))
                rel-coll
                (let [current (get rel-coll i),
                      pref-coll (take i rel-coll),
                      fc1 (filter #(= (second %) (first current)) pref-coll),
                      ne1 (for [e fc1] [(first e) (second current)]),
                      fc2 (filter #(= (second current) (first %)) pref-coll),
                      ne2 (for [e fc2] [(first current) (second e)])]
                  (recur (vec (concat rel-coll ne1 ne2)) (inc i)))))]
      (set (close (vec relation) 1)))))

(defcheck solution-a9d4a037
  (fn  [s]
    (set (mapcat (fn [a]
                   (for [x ((fn [a r]
                              (let [l (map second (filter #(= (first %) a) s))]
                                (if (empty? l) r
                                               (recur (first l) (conj r (first l))))))
                            a [])]
                     [a x]))
           (map first s)))))

(defcheck solution-aa235cb7
  (fn bt[s] (
              letfn [ (fnd[kp all] (
                                     reduce #(
                                               if (= (last kp) (first %2))
                                               (conj %1 [(first kp) (last %2)])
                                               %1
                                               ) [] all
                                     ))
                     (al[all sx] (

                                   if (empty? sx)
                                   (set all)
                                   (al (concat all (fnd (first sx) all)) (rest sx))
                                   ))
                     ]


              (al s (vec s))

              )))

(defcheck solution-ab5ae1d0
  (fn transitive-closure [coll]
    (letfn [(add-node [partRslt curnode]
              ;; not used here, as map can not augment cur list.
              (map (fn [this]
                     (if (= (first this) (last curnode))
                       (cons (first curnode) this)
                       (if (= (last this) (first curnode))
                         (conj this (first curnode))
                         this)))))
            (update-or-add [closure curnode]
              ;; from existing closure, maps to merged closure, if not aug existing entry, append to end.
              (loop [closure closure mergedclosure [] updated false]  ;; curnode aug an existing entry?
                (if (empty? closure)
                  (if updated
                    mergedclosure
                    (conj mergedclosure curnode))  ;; append curnode to the merged closure
                  (let [[hdst hded] (first closure)
                        [st ed] curnode]
                    (if (= hded st)
                      (recur (next closure) (conj mergedclosure (conj (vec (first closure)) ed)) true)
                      (if (= hdst ed)
                        (recur (next closure) (conj mergedclosure  (cons st (first closure))) true)
                        (recur (next closure) (conj mergedclosure (first closure)) updated)))))))
            (powerset [coll]
              ;; coll is [1 2 3 4]
              (loop [coll coll rslt []]
                (let [hd (first coll) bd (rest coll)]
                  (if (empty? bd)
                    rslt
                    (let [pairs (map (fn [e] [hd e]) bd)]
                      (recur bd (into rslt pairs)))))))]

      (let [mergedclosure (reduce update-or-add [] coll)]
        (loop [coll mergedclosure result #{}]
          (if (empty? coll)
            result
            (recur (next coll) (into result (powerset (first coll))))))))))

(defcheck solution-abcf03b5
  (letfn [
          (get-nodes [edges] (-> edges vec flatten set (zipmap (range))))
          (square-matrix [size] (map (fn [_] (repeat size 99)) (range size)))
          (set-ij [m i j v]
            (let [row (nth m j)
                  row- (concat (take i row) [v]  (nthnext row (inc i)))]
              (concat (take j m) [row-] (nthnext m (inc j)))))
          (get-ij [m i j] (-> m (nth j) (nth i)))
          (transitive-closure [edges] (let [
                                            nodes (get-nodes edges)
                                            node-count (count nodes)
                                            graph (-> (square-matrix node-count)
                                                    (#(reduce (fn [m [a b]] (set-ij m (nodes a) (nodes b) 1)) % edges))
                                                    (#(reduce (fn [m i] (set-ij m i i 0)) % (range node-count))))
                                            distance (reduce
                                                       (fn [m [i j k]] ; this is the floyd-warshall algorithm
                                                         (let [ij (get-ij m i j) ik (get-ij m i k) kj (get-ij m k j)]
                                                           (if (< (+ ik kj) ij)
                                                             (set-ij m i j (+ ik kj))
                                                             m)))
                                                       graph
                                                       (for [k (range node-count) i (range node-count) j (range node-count)] [i j k]))
                                            lookup (zipmap (vals nodes) (keys nodes))
                                            tc-set (->> (for [i (range node-count) j (range node-count)] [i j])
                                                     (filter (fn [[i j]] (let [v (get-ij distance i j)] (and (> v 0) (< v 99)))))
                                                     (map (fn [[i j]] [(lookup i) (lookup j)]))
                                                     (set))]
                                        tc-set))]
    transitive-closure))

(defcheck solution-ac0b996a
  #(set (mapcat
          (fn f [[a b :as p] s]
            (cons p
              (mapcat (fn [[c d]]
                        (if (= c b)
                          (cons [a d] (f [a d] (disj s p)))))
                s)))
          %
          (repeat %))))

(defcheck solution-ad5f7890
  (fn transitive-clojure [coll-set]
    (letfn [(union [& sets]
              (set (apply concat sets)))]
      (let [coll-set-after (apply union
                             coll-set
                             (map (fn [item]
                                    (set (map (fn [related-item]
                                                #_(println item related-item)
                                                (vector (first item) (last related-item)))
                                           (filter #(= (last item) (first %)) coll-set))))
                               coll-set))]
        (if (= (count coll-set) (count coll-set-after))
          coll-set
          (transitive-clojure coll-set-after))))
    ))

(defcheck solution-adf64b71
  (fn transitive [s]
    (letfn [(add [s [x y :as pair]]
              (let [new-pair
                    (reduce
                      (fn [acc [ex ey :as e]]
                        (cond
                          (= ey x) (conj acc [ex y])
                          (= ex y) (conj acc [x ey])
                          :else acc)) #{} s)]
                (clojure.set/union s #{pair} new-pair)))]
      (reduce (fn [acc e] (add acc e)) #{} s))))

(defcheck solution-ae9bad0a
  (letfn [(update1 [[key value] s]
            (let [trans (map (fn [x] [key (second x)])
                          (filter #(= value (first %)) s))]
              trans))
          (update [s]
            (clojure.set/union (set (mapcat #(update1 % s) s)) s))]
    (fn trans [s]
      (if (= (update s) s)
        s
        (trans (update s))))))

(defcheck solution-aef073c3
  #(let [r (into % (for [[a b] % [c d] % :when (= b c)] [a d]))]
     (if (= % r) r (recur r))))

(defcheck solution-af459306
  (fn [set0]
    (let [
          smap (into {} set0)
          sjoin (fn [sset [a b]] (if (contains? smap b) (conj sset [a (smap b)]) sset))
          redux (fn [[sset n]] [(reduce sjoin sset sset) (count sset)])
          ]
      (->>
        [set0 0]
        (iterate redux)
        (drop-while (fn [[sset n]] (< n (count sset))))
        ffirst
        ))))

(defcheck solution-b00fa489
  (fn transitive-closure [br]
    (let [br (reduce (fn [m [k v]] (assoc m k v)) {} br)]
      (loop [[k & ks] (keys br), tc #{}]
        (if k
          (recur ks (loop [x k, tc tc]
                      (if-let [y (br x)]
                        (recur y (conj tc [k y]))
                        tc)))
          tc)))))

(defcheck solution-b01b86b7
  (fn [r]
    (let [rmap (apply hash-map (mapcat (fn [[k v]] [v #{k}]) r))
          step #(apply hash-map (mapcat (fn [[k v]] [k (apply clojure.set/union v (map % v))]) %))]
      (->>
        (loop [m (step rmap)]
          (if (= m (step m))
            m
            (recur (step m))))
        (mapcat
          (fn [[k vs]]
            (map (fn [v] [v k]) vs)))
        (apply hash-set)))))

(defcheck solution-b0bb3e8
  (fn [s]
    (letfn [(step [ele s]
              (let [n (filter #(= (second ele) (first %)) s)]
                (if (seq n)
                  (let [t (map #(vec [(first ele) (second %)]) n)]
                    (cons ele (mapcat #(step % s) t)))
                  [ele])))]
      (set (mapcat #(step % s) s)))))

(defcheck solution-b1324f9b
  #(loop [s %]
     (let [n (into s
               (for [[a b] s [c d] s
                     :when (= b c)]
                 [a d]))]
       (if (= n s) n (recur n)))))

(defcheck solution-b18a6303
  (fn [s]
    (let [news (for [[a b] s [c d] s :when (= b c) :when (not (contains? s [a d]))] [a d])]
      (if (empty? news)
        s
        (recur (into s news))))))

(defcheck solution-b205dad6
  (fn [s]
    (let [add-ent (fn add-ent [parent child setdict]
                    (if (nil? child) #{}
                                     (clojure.set/union (into #{} setdict) #{[parent child]} (add-ent parent (get setdict child) setdict))))
          make-dict (fn make-dict [s]
                      (if (empty? s) {}
                                     (let [[k v] (first s)]
                                       (apply conj {k v} (make-dict (rest s))))))
          sdict (make-dict s)]
      (apply clojure.set/union (map #(add-ent % (get sdict %) sdict) (keys sdict))))))

(defcheck solution-b31b8ff
  (fn [xs]
    (let [m (into {} xs)
          r (reduce (fn [s [k v]] (if-let [v (m v)] (conj s [k v]) s)) xs xs)]
      (if (= xs r) r (recur r)))))

(defcheck solution-b378af97
  (fn transitive-closure [relations]
    (let [new-relations (for [[a x] relations [y b] relations :when (= x y)] [a b])
          updated-relations (into relations new-relations)]
      (if (= updated-relations relations)
        updated-relations
        (recur updated-relations)))))

(defcheck solution-b40e63c
  (fn trans-clojure2 [R]
    (letfn [(adj-list [R] ;;given relations construct adj-list
              (reduce #(if (contains? %1 (first %2))
                         (assoc %1 (first %2) (conj (%1 (first %2)) (last %2)))
                         (assoc %1 (first %2) [(last %2)])) {}  R))

            (df-search2 [G v R];;df-search on inicial vertex r. R is a set that contains all vertex that r can reach and v is a reacheable vertex
              (let [neib (G v)]
                (reduce #(if (contains? %1 %2)
                           %1
                           (clojure.set/union %1 (df-search2 G %2 (conj %1 %2)))) R (G v))))

            (df-search [G] ;;do a df-search for each vertex on graph represented by adj-list
              (let [ks (keys G)] ;;get the vertices list
                (reduce #(assoc %1 %2 (df-search2 G %2 #{})) {} ks)))
            ]

      (let [graph (df-search (adj-list R))
            ks (keys graph)]
        (reduce #(apply conj %1
                   (map (fn [neib] (vector %2 neib)) (graph %2))) #{} ks)))))

(defcheck solution-b43c8401
  (fn [s]
    (let [items         (set (mapcat identity s))
          relatives-map (apply merge-with clojure.set/union (map #(hash-map (% 0) #{(% 1)}) s))
          descendents   (fn descendents [x & [t]]
                          (let [t (or t #{})
                                relatives (set (filter #(not (contains? t %)) (relatives-map x)))]
                            (apply clojure.set/union
                              (cons relatives (map #(descendents % (conj t x)) relatives)))))]
      (set (mapcat (fn [x] (map (fn [y] (vector x y)) (descendents x))) items)))))

(defcheck solution-b6309e0a
  (fn [relation]
    (letfn [(accum [ex [from to]]
              (let [new-from (for [[ex-from ex-to] ex
                                   :when (= to ex-from)]
                               [from ex-to])
                    new-to (for [[ex-from ex-to] ex
                                 :when (= from ex-to)]
                             [ex-from to])]
                (clojure.set/union ex (set new-from) (set new-to) #{[from to]})))]
      (reduce accum #{} relation))))

(defcheck solution-b6960899
  (letfn [(G [s]
            (set (for [[x y] s [u v] s
                       :when (= y u)]
                   [x v])))
          (U [s t]
            (if (empty? s) t
                           (recur (rest s) (conj t (first s)))))
          (X [s]
            (let [t (U s (G s))]
              (if (= s t) s
                          (recur t))))]
    X))

(defcheck solution-b6eda52f
  (fn [xxs]
    (letfn [(paths [pps gs]
              (if (empty? gs)
                #{#{}}
                (for [g gs
                      ps (paths pps (disj gs g))]
                  (cons g ps))))
            (get-chain [[a b & more :as xs]]
              (cond
                (nil? b) [a]
                (nil? a) nil
                (= (second a) (first b)) (cons a (get-chain (rest xs)))
                :else [a]))
            (tc [pps]
              (map (fn [ps]
                     (get-chain ps))
                pps))]
      (into #{}
        (mapcat (fn [xs]
                  (map (fn [p] [(first (first xs)) (second p)]) xs))
          (tc (paths #{[]} xxs)))))))

(defcheck solution-b7183c6a
  (fn tc [rel]
    (let [nrel (into rel (for [r1 rel, r2 rel :when (= (r2 0) (r1 1))] [(r1 0) (r2 1)] ))]
      (if (= rel nrel) rel (recur nrel)))))

(defcheck solution-b75d2e0a
  (fn [rel]
    (let [c (into rel
              (for [[a b] rel
                    [c d] rel
                    :when (= b c)]
                [a d]))]
      (if (= c rel)
        c
        (recur c)))))

(defcheck solution-b7726c2c
  (fn [xs]
    (let [m (apply merge (map #(apply hash-map %) xs))
          r (set (concat xs (filter #(last %) (map #(list (first %) (m (last %))) xs))))]
      (if (= xs r) r (recur r)))))

(defcheck solution-b784cbfc
  #(reduce
     (fn [r [a b]]
       (into r (for [[c d] r :when (= b c)] [a d])))
     %
     %))

(defcheck solution-b7b35af5
  (fn [rel]
    (let [m (into {} (seq rel))
          tc #(loop [n % a []]
                (if (m n)
                  (recur (m n)(conj a [% (m n)])) a))]
      (set (mapcat tc (keys m))))))

(defcheck solution-b7fd741c
  (fn [s]
    (letfn [
            (f [s [k v]]
              (reduce (fn [s [_ v]] (conj s [k v])) s (filter (fn [[k _]] (= k v)) s)))]
      (reduce f s s))))

(defcheck solution-b80e57ac
  #(let [rm (into {} %)
         rs (->> %
              (map (fn [[i j]] [i (rm j)]))
              (filter last)
              (concat %)
              set)]
     (if (= % rs) rs (recur rs))))

(defcheck solution-b8763b4b
  (fn [st]
    (let [walkable? (fn [[x y] hmap]
                      (when-let [vl (hmap x)]
                        (or (= vl y) (recur [vl y] hmap))))
          args (set (apply concat st))
          hmap (into {} st)]
      (set (for [x args y args :when (walkable? [x y] hmap)] [x y])))))

(defcheck solution-b87aa651
  (fn [x]
    (reduce (fn [s k]
              (into s
                (for [[i _] s
                      [_ j] s
                      :when (and
                             (contains? s [i k])
                             (contains? s [k j]))]
                  [i j])))
      x
      (mapcat identity x))))

(defcheck solution-b8c421f
  (fn transitive-closure [R]
    (let [xs (set (apply concat R))
          mR (into {} R)]
      (set (apply concat
             (for [x xs]
               (map vector (repeat x) (take-while (complement nil?) (iterate mR (mR x))))))))))

(defcheck solution-b94d6ff1
  (fn tc [s]
    (letfn [(get-related [s [a b]] (filter (fn [[c d]] (= b c)) s))
            (get-deep-related [s [a b]]
              (let [related (get-related s [a b])]
                (if (empty? related) []
                                     (concat related (mapcat (partial get-deep-related s) related)))))]
      (clojure.set/union
        s
        (into #{} (apply concat (for [[a b] s]
                                  (let [related (get-deep-related s [a b])]
                                    (map (fn [[c d]] (vector a d)) related)))))))))

(defcheck solution-b992a1d
  (fn [input-set]
    (let [m (reduce (fn [m [k v]] (assoc m k v)) {} input-set)
          f (fn [input-map master-key]
              (loop [k master-key output-map []]
                (if (contains? input-map k)
                  (recur (input-map k) (conj output-map [master-key (input-map k)]))
                  output-map)))]
      (set (reduce #(apply conj %1 %2) [] (map #(f m %) (keys m)))))))

(defcheck solution-ba67dbc
  (fn transit[r]
    (let[rules (reduce #(assoc %1 (first %2) (second %2)) {} r)
         deduct (fn [orig dest result]
                  (if (contains? result [orig dest]) result
                                                     (let[newResult (conj result [orig dest])]
                                                       (if (contains? rules dest) (recur orig (rules dest) newResult)
                                                                                  newResult))))]
      (loop[rs r result #{}]
        (if (seq rs)
          (let[rule (first rs)]
            (recur (rest rs) (deduct (first rule) (second rule) result)))
          result)))))

(defcheck solution-ba88bfb2
  (let [that-function
        (fn [x] (for [[a b] x [c d] x :when (= b c)] [a d]))
        this-function
        (fn [s]
          (if (clojure.set/subset? (set (that-function s)) s)
            s
            (recur (clojure.set/union (set (that-function s)) s))
            )
          )]
    (fn [z] (this-function z))
    ))

(defcheck solution-bb0a7fb6
  (fn [r]
    (loop [r r n r]
      (let [m (for [[x1 y1] r [x2 y2] n :when (= x2 y1)] [x1 y2])]
        (if (empty? m) r (recur (set (concat r m)) m))))))

(defcheck solution-bb4e38b4
  (fn transClose [s]
    (let [t (into s (for [p s q s
                          :when (= (first q) (second p))]
                      [(first p) (second q)]))]
      (if (= t s) t (transClose t)))))

(defcheck solution-bb5703e1
  (fn [s]
    (letfn [(joiner [a b] (if (= (second a) (first b)) [(first a) (second b)] nil))
            (iterator [x]
              (let [iteration (map #(keep identity (map (partial joiner %) s)) x)]
                (apply concat (filter (comp not empty?) iteration))))
            (relations [x]
              (apply concat (take-while (comp not empty?) (iterate iterator [x]))))]
      (set (apply concat (map relations s))))))

(defcheck solution-bbf5f8e6
  (fn tc [coll]
    (apply clojure.set/union coll
      (for [x coll  y coll
            :let [[af at] x [bf bt] y]
            :when (= at bf)
            ]
        (tc (conj (disj coll x y) [af bt]))
        )
      )))

(defcheck solution-bcdc8d16
  (fn tc [rel]
    (reduce
      (fn [closure [f s]]
        (into
          (into (conj closure [f s]) (->>
                                       closure
                                       (filter (fn [[cf cs]] (= cs f)))
                                       (map (fn [[cf cs]] [cf s]))))
          (->>
            closure
            (filter (fn [[cf cs]] (= cf s)))
            (map (fn [[cf cs]] [f cs])))))
      #{} rel)))

(defcheck solution-bd0cde83
  (fn [s]
    (loop [s s]
      (let [newset (reduce conj s
                     (mapcat (fn [[a1 b1]]
                               (filter (complement nil?)
                                 (map (fn [[a2 b2]]
                                        (cond (= a1 b2) [a2 b1]
                                              (= b1 a2) [a1 b2]
                                              true nil)) s))) s))]
        (if (= newset s)
          s
          (recur newset))))))

(defcheck solution-bd3f5758
  (fn [s]
    (let [
          c (fn [a b] (if (= (second a)(first b)) (vector (first a) (second b)) a ))
          it (fn [ss] (set (for [s1 ss s2 ss] (c s1 s2))))
          ]
      (ffirst
        (drop-while #(not= (first %) (second %))
          (apply partition 2 1
            (#(list % (rest %)) (iterate it s)))))
      )))

(defcheck solution-bd7800e2
  (let [round (fn [pairs] (set (concat pairs (for [[x y] pairs [x' y'] pairs :when (= y x')] [x y']))))
        fix (fn [f e] (loop [e e] (let [e' (f e)] (if (= e' e) e (recur e')))))]
    (fn [pairs] (fix round (set pairs)))))

(defcheck solution-bdd2b372
  (fn [s]
    (letfn [(expand [m x]
              (loop [[a b] x
                     out []]
                (if-let [b' (m b)]
                  (recur [a b'] (conj out [a b']))
                  out)))]
      (clojure.set/union s (set (filter #(not (empty? %)) (mapcat (partial expand (into {} s)) s)))))))

(defcheck solution-be6cb624
  (fn [is]
    (loop [s is]
      (let [cont (into #{}
                   (for [[a b] s
                         [c d] s
                         :when (= b c)]
                     [a d]))
            whole (clojure.set/union s cont)]
        (if (= whole s)
          whole
          (recur whole))))))

(defcheck solution-bf609cea
  (fn transitive-closure [relations]
    (let [relation-map (apply hash-map (flatten (seq relations)))
          transitives (fn ! [base key] (when key (cons [base key] (! base (relation-map key)))))]
      (set (reduce concat (for [[key val]  relation-map]
                            (transitives key val)))))))

(defcheck solution-bfc1c007
  (fn [s]
    (loop [dest s]
      (let [original-size (count dest)
            new-items (for [a dest b dest]
                        (cond
                          (and (= (first a) (last b))
                               (not= (last a) (first b)))
                          (vector (first b) (last a))

                          (and (= (last a) (first b))
                               (not= (first a) (last b)))
                          (vector (first a) (last b))))
            new-dest (into dest new-items)
            new-size (count new-dest)]
        (if (= original-size new-size)
          (into #{} (remove nil? new-dest))
          (recur new-dest))))))

(defcheck solution-bff67ccc
  (fn transitive-clojure [initial-set]
    (->> initial-set
      (iterate (fn extend-iter [base-set]
                 (into base-set
                   (for [[base-from base-to] base-set
                         [extend-from extend-to] base-set
                         :when (= base-to extend-from)
                         :when (not (contains? base-set [base-from extend-to]))] [base-from extend-to]))))
      ((fn first-same [[actual_ next_ & rest_ :as x]]
         (if (= actual_ next_) actual_ (recur (rest x))))))))

(defcheck solution-c0b3be99
  (fn trans [coll]
    (loop [c coll acc (count coll)]
      (let [nc (for [x c y c
                     :when (and ((complement =) x y)
                                (= (last x) (first y)))] [(first x) (last y)])
            nnc (clojure.set/union (set nc) c)
            ]
        (cond
          (= (count nnc) acc) nnc
          :else (recur nnc (count nnc))
          )))))

(defcheck solution-c0c08157
  (fn [coll]
    (let [gc (reduce (fn [m [k v]] (assoc m k (conj (m k []) v))) {} coll)
          ch (fn ch [k] (cons k (mapcat #(ch %) (gc k))))]
      (->> (keys gc)
        (map ch)
        (mapcat #(map vector (repeat (first %)) (set (rest %))))
        set))))

(defcheck solution-c1d1c094
  (fn clo [rel]
    (let [new-seq (for [[a x] rel
                        [y c] rel
                        :when (= x y)]
                    [a c])
          new-set (set new-seq)
          new-excl (clojure.set/difference new-set rel)]
      (if (seq new-excl)
        (clo (clojure.set/union rel new-set))
        rel))))

(defcheck solution-c1f8f378
  (fn f [col]
    (let [m (into {} col)
          ks (keys m)]
      (letfn [(get-children [parents m root]
                #_(println parents)
                (let [child (m root)
                      parents' (conj parents root)]
                  (if child
                    (concat (map
                              #(do [% child])
                              parents') ;[[root child]]
                            (get-children parents' m child))
                    nil)))]
        (set (mapcat (partial get-children [] m) ks))))))

(defcheck solution-c2bd85c9
  (fn transitive-closure [rel]
    (let [nxt (into #{}
                (for [[x y1 :as r] rel
                      [y2 z] rel]
                  (if (= y1 y2) [x z] r)))]
      (if (= nxt rel) rel
                      (recur nxt)))))

(defcheck solution-c3404348
  (fn transitive-set
    ([s] (transitive-set (list* s) [] 0 () (count s) true))
    ([init transitive counter current starting-point first-round]
     (if (= 0 (count init)) (let [new-init (set transitive)] #_(println "new-init: "  new-init)(if (= (count new-init) starting-point) new-init (recur (list* new-init) [] 0 () (count new-init) true)))
                            (if first-round
                              (if (= counter (count init)) (transitive-set init transitive 0 current starting-point false)
                                                           (if (= (first current) (second (nth init counter)))
                                                             (transitive-set init (conj transitive [(first (nth init counter)) (second current)]) (inc counter) current starting-point first-round)
                                                             (transitive-set init transitive (inc counter) current starting-point first-round)))
                              (if (= counter (count init)) (transitive-set (rest init) (conj transitive (first init)) 0 (first init) starting-point true)
                                                           (if (= (first (nth init counter)) (second current))
                                                             (transitive-set init (conj transitive [(first current) (second (nth init counter))]) (inc counter) current starting-point first-round)
                                                             (transitive-set init transitive (inc counter) current starting-point first-round))))))))

(defcheck solution-c40cb6b4
  (fn [ps]
    (let [m (into {} (vec ps))
          from-here
            (fn [x]
              (loop [out []
                     k x]
                (let [v (m k)]
                  (if v
                    (recur (conj out [x v]) v)
                    out))))]
      (set (apply concat (map #(from-here %) (keys m)))))))

(defcheck solution-c478a1b8
  (let [
        cross (fn [relations]
                (for [r1 relations
                      r2 relations
                      :let [mapping [(first r1) (second r2)]]
                      :when (and (= (second r1) (first r2)) (not (relations mapping)))] mapping))]

    (fn [relations]
      (if-let [new-relations (seq (cross relations))]
        (recur (into relations new-relations))
        relations))))

(defcheck solution-c48edffb
  (fn transitive-closure [rels]
    (let [tc (reduce (fn [rs [a b]] (into rs (keep #(if (= b (first %)) [a (second %)]) rs))) rels rels)]
      (if (= tc rels) tc (recur tc)))))

(defcheck solution-c4c0a9ac
  (fn [xs]
    (let [prod (fn [m o]
                 (let [n (apply (partial map list) o)]
                   (vec (for [u m] (vec (for [v n] (apply + (map * u v))))))))
          add (fn [& args]
                (vec (map (partial apply (comp vec (partial map +)))
                       (apply (partial map list) args))))
          vs (vec (set (flatten (seq xs))))
          n (count vs)
          zs (vec (take n (repeat (vec (take n (repeat 0))))))
          adj (reduce
                (fn [m [x y]] (update-in m
                                [(.indexOf vs x) (.indexOf vs y)]
                                (constantly 1)
                                nil))
                zs xs)
          pows (take-while (partial not= zs) (iterate (partial prod adj) adj))
          clos (apply add pows)]
      (set (apply concat (for [i (range n)]
                           (for [j (range n) :when (not= 0 (get-in clos [i j]))]
                             [(vs i) (vs j)])))))))

(defcheck solution-c516fc3
  #(reduce
     (fn [xset [a b]]
       (clojure.set/union xset #{[a b]}
         (set (map
                (fn [[c d]]
                  (cond
                    (= b c) [a d]
                    (= d a) [c b]
                    :else [a b]))
                xset)))) #{} %))

(defcheck solution-c52cd161
  (fn [s]
    (let [s2 (reduce (fn [t [a b]]
                       (set (concat t (map (fn [[c d]] [a d])
                                        (filter (fn [[c d]]
                                                  (= b c)) s)))))
               s s)]
      (if (= s2 s) s (recur s2)))))

(defcheck solution-c5466115
  (fn [c]
    (loop [a #{}
           [[b1 b2] & bs] (seq c)]
      (if (nil? b1)
        a
        (recur (conj a [b1 b2])
          (into bs (reduce (fn [s [a1 a2]]
                             (cond (= a1 b2) (conj s [b1 a2])
                                   (= a2 b1) (conj s [a1 b2])
                                   :else s)) #{} a)))))))

(defcheck solution-c54d0745
  (fn [xs]
    (let [genmap (into {} xs)]
      (reduce (fn conjtrans [ys [a b]] (if (genmap b)
                                         (conj (conjtrans ys [a (genmap b)]) [a b])
                                         (conj ys [a b]) ))
        #{} xs))))

(defcheck solution-c57434e7
  (fn [col]
    (let[generate-pair (fn generate-pair [values]
                         (if (= (count values) 2)
                           (set [values])
                           (clojure.set/union (set (for [x (rest values)] [(first values) x]  ))
                             (generate-pair (rest values)) )))

         merge-pair (fn merge-pair [init-v values]
                      (loop[result init-v data values]
                        (if (empty? data) result
                                          (let [itm (first data)]
                                            (cond (= (first itm)  (last result) ) (recur (conj result (last itm)) (rest data))
                                                  (= (first result)  (last itm) ) (recur ( concat itm (rest result)) (rest data))
                                                  :else (recur result (rest data)))))))
         merge-all (fn merge-all [values]
                     (set (map #(merge-pair % values) values))
                     )
         ]
      (reduce clojure.set/union (map #(generate-pair %) (merge-all col)))
      )
    ))

(defcheck solution-c60e0cd3
  (fn [s]
    (set (concat (mapcat (fn [[pivot-primeiro pivot-segundo :as pivot]]
                           (map (fn [[_ e]]
                                  [pivot-primeiro e])
                             ((fn pega-elementos-para-expansao-rec  [[_ pivot-expansao-segundo] acc]
                                (let [proximo-elemento (some (fn [[candidato-primeiro _ :as candidato]]
                                                               (when (= pivot-expansao-segundo candidato-primeiro)
                                                                 candidato))
                                                         s)]
                                  (if (nil? proximo-elemento)
                                    acc
                                    (pega-elementos-para-expansao-rec proximo-elemento (conj acc proximo-elemento)))))
                              pivot
                              [])))
                   s)
                 s))))

(defcheck solution-c6a79ffa
  (fn transitive-closure [s]
    (let [sub (for [x s y s :when (= (x 1) (y 0))] [(x 0) (y 1)])]
      (if (clojure.set/subset? (into #{} sub) s)
        s
        (transitive-closure (into s sub))))))

(defcheck solution-c7060c3f
  (fn [xs]
    (let [graph (into {} xs)
          dfs (fn [[k v]]
                (loop [c v, acc [[k v]]]
                  (if (graph c)
                    (recur (graph c) (conj acc [k (graph c)]))
                    acc)))]
      (set (mapcat dfs xs)))))

(defcheck solution-c7ed351
  (letfn [(transitive-closure [set-of-pairs]
            (let [relation (reduce (fn [acc [key val]]
                                     (assoc acc key (conj (get acc key #{}) val)))
                             {} set-of-pairs)]
              (close-relation relation)))
          (close-relation [relation]
            (let [augmented-relation (augment-relation relation)]
              (if (= relation augmented-relation)
                relation
                (recur augmented-relation))))
          (augment-relation [relation]
            (reduce (fn [augmented-relation key]
                      (assoc augmented-relation key
                                                (apply clojure.set/union (get relation key)
                                                  (map relation (get relation key)))))
              {}
              (keys relation)))]
    (fn [relation]
      (let [closure (transitive-closure relation)]
        (reduce (fn [acc elt]
                  (apply conj acc (map #(identity [elt %])
                                    (get closure elt))))
          #{}
          (keys closure))))))

(defcheck solution-c84e7255
  (fn [es]
    (let [
          ps (clojure.set/union (set (map first es)) (set (map last es)))
          es1 (atom es)]
      (doseq [k ps i ps j ps]
        (if (and (@es1 [i k]) (@es1 [k j]))
          (swap! es1 conj [i j])))
      @es1)))

(defcheck solution-c8b6bf50
  (fn transitive [s]
    (#(if (= % s) s (transitive %))
     (set (for [[w x] s [y z] s]
            [w (if (= x y) z x)])))))

(defcheck solution-c90aa4fc
  (fn [binary-relations]
    [binary-relations]
    (let [sort-relations
          (fn sort-relations[relations]
            (when (seq relations)
              (let [vals (set (map second relations))
                    top (remove #(vals (first %)) relations)]
                (concat top (sort-relations (filter #(vals (first %)) relations))))))]
      (->> ((fn build-transitive-closure
              [results relations]
              (if (seq relations)
                (let [[a b] (first relations)
                      results (build-transitive-closure
                                (update-in results [a] (fnil conj #{}) b)
                                (rest relations))]
                  (update-in results [a] into (get results b)))
                results)) {} (sort-relations binary-relations))
        (mapcat
          (fn [[k vs]] (map (partial vector k) vs)))
        (set)))))

(defcheck solution-ca372a1b
  (fn [s]
    (letfn [(i [m s] (into s (remove nil? (map #(if-let [n (m (% 1))] (assoc-in % [1] n)) s))))]
      (second (first (drop-while #(apply not= %) (partition 2 (iterate #(i (into {} s) %) s))))))))

(defcheck solution-caa3a130
  (fn [r]
    (letfn [ (f [ [a b] r]
               (->>
                 (filter #(= (first %) b) r)
                 (map #(vector a (last %)))
                 ))]
      (let [x  (mapcat #(f % r) r)
            y  (filter #(nil? (r %)) x )]
        (if (empty? y) r (recur (into r y))))
      )))

(defcheck solution-cb3368f9
  (fn transitive-closure [s]
    (let [expansion (into s (mapcat (fn [r] (map (fn [s] [(first s)
                                                          (last r)])
                                              (filter #(= (first r)
                                                         (last %))
                                                s)))
                              s))]
      (if (= expansion s)
        s
        (transitive-closure expansion)))))

(defcheck solution-cb75fa2
  (fn tc
    ([rels] (tc (seq rels) rels))
    ([[rel & rrels] clos]
     (if
      (nil? rel) clos
                 (let [[lrel rrel] rel
                       new-pairs (for [[l r] clos :when (= rrel l)] [lrel r])]
                   (recur rrels (into clos new-pairs)))))))

(defcheck solution-cc78149
  (fn transitive-closure [relations]
    (reduce #(into %1
               (for [[k0 v0] %1
                     [k1 v1] %1 :when (= %2 v0 k1)] [k0 v1]))
      relations
      (map first relations))))

(defcheck solution-cc999daa
  (fn [s]
    (loop [l s acc #{}]
      (if-let [f (first l)]
        (recur
          (rest l)
          (into acc
            ((fn f [x s]
               (if-let [a ((fn [[h t] s]
                             (reduce (fn [a [i j]] (if (= i t) [h j] a))
                               nil s)) x s)]
                 (conj (f a s) a)
                 #{}))
             f s)))
        (into s acc)))))

(defcheck solution-cdf287be
  (fn [xs]
    (let [add-binary (fn [[f1 s1] xs]
                       (reduce (fn [acc [f2 s2]]
                                 (if (= s1 f2)
                                   (conj acc [f1 s2])
                                   acc)) #{} xs))]
      (reduce (fn [acc x]
                (reduce conj acc (add-binary x acc))) xs xs))))

(defcheck solution-cdfa43da
  (fn trans [s]
    (let [indexes (-> s vec flatten distinct vec (zipmap (range)))
          n (count (keys indexes))
          row (vec (repeat n 0))
          empty-matrix (vec (repeat n row))
          matrix (loop [pairs s update-matrix empty-matrix]
                   (if (empty? pairs)
                     update-matrix
                     (let [ri (get indexes (first (first pairs)))
                           ci (get indexes (second (first pairs)))]
                       (recur (rest pairs)
                         (update-in update-matrix [ri ci] (constantly 1))))
                     ))
          ops (for [k (range n) i (range n) j (range n)] [k i j])
          ufn (fn [matrix [k i j]] (update-in matrix [i j] #(if (or (= 1 %) (= 1 (get-in matrix [i k]) (get-in matrix [k j]))) 1 0)))
          umatrix (reduce (fn [acc v] (ufn acc v)) matrix ops)
          tsets (into #{} (reduce (fn [acc v] (let [f (first v)
                                                    i (second v)]
                                                (concat acc
                                                        (loop [sets (filter #(not (= (first %) f)) indexes) trs #{}]
                                                          (cond (empty? sets) trs
                                                                (= 1 (get-in umatrix [i (second (first sets))])) (recur (rest sets) (conj trs [f (first (first sets))]))
                                                                :otherwise (recur (rest sets) trs)
                                                                )
                                                          )))
                                    ) #{} indexes))

          ]
      tsets
      )
    ))

(defcheck solution-ced887f8
  (fn [xs]
    (loop [rs xs]
      (let [ds (loop [r1 rs ys rs]
                 (do #_(println r1 ys)
                     (if (empty? r1)
                       ys
                       (recur (rest r1)
                         (reduce (fn [zs r] (if (= (get r 0) (get (first r1) 1))
                                              (conj zs (vector (get (first r1) 0) (get r 1)))
                                              zs)) (set ys) ys)
                         )
                       )
                     ))]
        (if (= (count ds) (count rs))
          ds
          (recur (set ds)))))))

(defcheck solution-cf3b548a
  (fn [rs]
    (loop [cur rs]
      (let [nxt (into cur (for [[u v] cur [w x] cur :when (= v w)] [u x]))]
        (if (> (count nxt) (count cur)) (recur nxt) nxt)))))

(defcheck solution-cf4fc31f
  (fn transitive-closure [xs]
    (letfn [(add-pair [xs [a b]]
              (clojure.set/union
                xs
                #{[a b]}
                (set (for [[ax bx] xs :when (= bx a)] [ax b]))
                (set (for [[ax bx] xs :when (= ax b)] [a bx]))
                (set (for [[ax bx] xs [ay by] xs :when (and (= bx a) (= ay b))] [ax by]))))]
      (reduce add-pair #{} xs))))

(defcheck solution-cffd2737
  (fn [s]
    (let [tr (group-by first s)
          d (fn [[a b]] (map #(vector a (second %)) (tr b)))
          f (fn f [e]
              (let [a (d e)]
                (concat [e] a (mapcat f a))))]
      (set (mapcat f s)))))

(defcheck solution-d05e2e0e
  (fn [edges]
    (letfn [(dfs [g v seen]
              (reduce #(if (contains? % %2) %
                                            (apply conj % (dfs g %2 (conj % %2))))
                seen (g v)))
            (graph [e]
              (reduce (fn [val [k v]]
                        (if (val k) (assoc val k (conj (val k) v))
                                    (assoc val k #{v}))) {} e))]
      (let [g (graph edges)]
        (into #{} (for [k (keys g) v (dfs g k #{})] [k v]))))))

(defcheck solution-d0d750b5
  (fn __ [s]
    (let [augment (fn [coll [a b]]
                    (let [firsts (map first coll)
                          seconds (map second coll)
                          ia (.indexOf firsts b)]
                      (if (= -1 ia)
                        nil
                        [a (nth seconds ia)])))
          nexts (filter (complement nil?) (map (partial augment s) s))
          s-plus (clojure.set/union s (set nexts))]
      (if (= s s-plus)
        s
        (__ s-plus)))))

(defcheck solution-d0e2f39
  (fn [m]
    (->> m
      vec
      (reduce
        (fn [ll x]
          (let [xs (set x)
                [l nl] (reduce (fn [[a b] lx]
                                 (if ((comp not empty? (partial clojure.set/intersection xs) first) lx)
                                   [(conj a lx) b]
                                   [a (conj b lx)])) [[][]] ll)]
            (if (empty? l) (conj ll [xs [x]])
                           (let [[k v] (first l)]
                             (-> nl
                               (conj [(reduce conj k x)
                                      (conj v x)]))))))
        [])
      (map second)
      (map (fn [[h & t]]
             (loop [h h t t]
               (if (empty? t) h
                              (let [ft (first t)
                                    [x y] ft
                                    rt (rest t)]
                                (cond (= y (first h)) (recur (into [x] h) rt)
                                      (= x (last h)) (recur (into h [y]) rt)
                                      :else (recur h (conj (vec rt) ft))))))))

      (map (fn [xx]
             (loop [a [] x (first xx) t (rest xx)]
               (if (empty? t) a
                              (recur (reduce #(conj %1 [x %2]) a t) (first t) (rest t))))))
      (reduce into [])
      set)))

(defcheck solution-d143a29f
  (fn [pairs]
    (let
     [organize-pairs (fn organize-pairs [[flist & rlists] pair]
                       #_(prn flist rlists pair)
                       (cond
                         (not flist) [pair]
                         (= (last pair) (first flist)) (cons (cons (first pair) flist) rlists)
                         (= (first pair) (last flist)) (cons (conj flist (last pair)) rlists)
                         :else (cons flist (organize-pairs rlists pair))))
      extract-pairs (fn [element-list]
                      (for [x (range (count element-list))
                            y (range (inc x) (count element-list))]
                        [(nth element-list x) (nth element-list y)]))
      transitive-pairs (fn [element-lists]
                         (for [element-list element-lists]
                           (extract-pairs element-list)))]
      (->>
        (reduce organize-pairs [] pairs)
        transitive-pairs
        (apply concat)
        set))))

(defcheck solution-d1a1162e
  (fn [s]
    (some
      (fn [[a b]] (when (= a b) a))
      (partition 2 1
        (iterate
          #(into %
             (for
              [ [a b] %
               [c d] %
               :when (= b c)]
               [a d]))
          s)))))

(defcheck solution-d2450a92
  (fn transitivity [coll]
    (letfn [(make-map [c]
              (loop [remaining c ans '{}]
                (if (empty? remaining)
                  ans
                  (if (contains? ans (first (first remaining)))
                    (recur (rest remaining) (update-in ans [(first (first remaining))] conj (second (first remaining))))
                    (recur (rest remaining) (conj ans [(first (first remaining)) [(second (first remaining))]]))))))
            (get-children [k transitivity-map]
              (if-not (contains? transitivity-map k)
                #{}
                (loop [children (get transitivity-map k) ans #{}]
                  (if (empty? children)
                    ans
                    (recur (rest children) (clojure.set/union ans #{(first children)} (get-children (first children) transitivity-map)))))))]
      (let [t-map (make-map coll)]
        (loop [remaining coll ans #{}]
          (if (empty? remaining)
            ans
            (recur (rest remaining) (apply conj ans (map #(conj [(first (first remaining))] %) (get-children (first (first remaining)) t-map))))))))))

(defcheck solution-d3049448
  (fn [st]
    (let [m (reduce #(assoc %1  (first %2)  (second %2)) {} st)
          chains (map (fn [k]
                        (loop [k k c []]
                          (if (nil? (m k))
                            (conj c k)
                            (recur (m k) (conj c k))))) (keys m))]
      (set (reduce concat [] (for [chain chains]
                               (map vector (repeat (first chain)) (rest chain))))))))

(defcheck solution-d366d165
  (fn [rs]
    (letfn [(relations [rs srcs]
              (for [[x y] rs
                    :when ((set srcs) x)]
                y))

            (chain [rs srcs]
              (let [new-rels (disj (set (relations rs srcs)) srcs)]
                (if (empty? new-rels) srcs
                                      (into srcs (chain rs new-rels)))))]

      (set (for [[x t] rs
                 y (chain rs #{x})
                 :when (not= x y)]
             [x y])))))

(defcheck solution-d39481d2
  (fn [s]
    (let [diff (filter identity (for [[a b] s [c d] s] (when (= b c) [a d])))]
      (if-not (every? s diff)
        (recur (into s diff))
        s))))

(defcheck solution-d3cf3908
  (fn transitive-closure [relations]
    (let [relmaps (for [[x y] relations] {x [y]})
          relmap (apply merge-with into relmaps)
          add-rel
                  (fn add-rel [rels [x y]]
                    (reduce add-rel (conj rels [x y]) (map #(vector x %) (relmap y))))
          ]
      (reduce add-rel #{} relations)
      )))

(defcheck solution-d45717c3
  (fn [coll]
    (letfn  [(through [k x]
               (loop [x x lst [] ]
                 (if-let [nxt (get k x)]
                   (let [steplst (map #(identity [(first %) nxt] ) lst)]
                     (recur nxt (concat lst [[x nxt]] steplst)))
                   lst)))]

      (let [k (into {} coll)]
        (->>
          (for [elem (keys k)]  (through k elem))
          (mapcat identity)
          (distinct)
          (set))))))

(defcheck solution-d534ea89
  (fn f [rel]
    (let [nrl (clojure.set/union
                rel
                (set (mapcat
                       identity
                       (keep (fn [[r s]]
                               (reduce
                                 (fn [a [u v]] (if (= u s) (conj a [r v]) a))
                                 []
                                 rel))
                         rel))))]
      (if (= rel nrl)
        rel
        (f nrl)))))

(defcheck solution-d6267a0f
  (fn transitive-closure [relation-set]
    (letfn [(transitive-list [relation-map key]
              (let [values (get relation-map key [])]
                (cons key (mapcat #(transitive-list relation-map %) values))))]
      (let [relation-map (into {} (for [[key value] (group-by first relation-set)] [key (mapv second value)]))]
        (set (mapcat #(map vector (repeat %) (rest (transitive-list relation-map %))) (keys relation-map)))))))

(defcheck solution-d6f71c37
  (fn __ [input-set]
    (let [rels (->> input-set
                 (apply list)
                 flatten
                 (apply hash-map))
          nodes (->> input-set
                  (apply list)
                  flatten
                  set)
          aux (fn aux [x]
                (let [x' (rels x)]
                  (if (nil? x') []
                                (let [aux-res (aux x')
                                      xtra-res (map (fn [[_ y]] [x y]) aux-res)]
                                  (cons [x x'] (concat aux-res xtra-res))))))]
      (set (mapcat aux nodes)))))

(defcheck solution-d724bf85
  (fn [bset]
    (let [newset
          (reduce (fn [result elem]
                    (apply conj result
                      (let [[[f c] toextend] elem]
                        (reduce (fn [s t]
                                  (if (= (first t) c)
                                    (conj s [f (second t)])
                                    s))
                          result
                          toextend))))
            bset
            (for [w bset] [w (disj bset w)]))]
      (if (= newset bset)
        newset
        (recur newset)))))

(defcheck solution-d80281e5
  (fn transclosure [r]
    (let [k (map first r)
          v (map second r)
          m (zipmap k v)]
      (->> m
        (iterate (partial map
                   #(vector (first %)
                      (m (second %)))))
        (take-while #(some identity (map second %)))
        rest
        (apply concat)
        (remove #(nil? (second %)))
        (into r)))))

(defcheck solution-d8b473cc
  (fn transAll [yss] ;yss set of vec of 2
    (letfn [

            (trans [xs ys]  ;xs ys vector of 2 ele
              (let [x1 (first xs) x2 (last xs)
                    y1 (first ys) y2 (last ys)
                    ]

                (cond (= x2 y1) [x1 y2]
                      ; (= x1 y2) [y1 x2]
                      :else []
                      )
                )
              )

            (trans1n [xs yss]
              (letfn [(fr [res hs pss ]
                        (let [ps1 (first pss) rps (rest pss)]
                          (if ps1
                            (let [transvec (trans hs ps1)]
                              (if (> (count transvec) 0)
                                (recur (conj res (trans hs ps1)) hs rps )
                                (recur res hs rps)
                                )
                              )
                            res
                            )
                          )

                        )]
                (fr #{} xs yss)
                )
              )



            ]

      (loop [i 0 k 0 xss yss]

        (let [ys (nth  (vec xss) i)
              nss (trans1n ys xss)]
          (if (= k (count xss))
            xss
            (if (> (count nss) 0)
              (let [uset (clojure.set/union xss nss)];
                (if (> (count uset) (count xss))
                  (recur (mod (inc i) (count uset)) 0 uset )
                  (recur (mod (inc i) (count xss)) (inc k) xss )
                  )

                )
              (recur (mod (inc i) (count xss)) (inc k) xss)
              )
            )
          )
        ))))

(defcheck solution-d962c00
  (fn transitive-closure [s]
    (letfn [(calc-path [m a]
              (loop [prev a verts []]
                (if (m prev)
                  (recur (m prev) (conj verts (m prev)))
                  verts)))
            ]
      (loop [[[a b] & r] (seq s) m {}]
        (if a
          (recur r (assoc m a b))
          (into #{}
            (mapcat (fn [k]
                      (map #(vector k %) (calc-path m k)))
              (keys m))))))))

(defcheck solution-d98c10ab
  (fn trans-clousure
    ([vs] (trans-clousure vs (count vs)))
    ([vs n]
     (let [ss
             (apply conj vs
               (for [v1 vs v2 vs :when (= (get v1 1) (get v2 0))]
                 [(get v1 0) (get v2 1)]))
           c (count ss)]
       (if (= c n)
         ss
         (trans-clousure ss c))))))

(defcheck solution-d9a6cc94
  (fn [xs]
    (loop [xs xs]
      (let [nxs
            (reduce
              (fn [a [x1 x2]]
                (reduce (fn [b [y1 y2]]
                          (if (= x2 y1) (conj b [x1 y2]) b))
                  a
                  a))
              xs
              xs )]
        (if (= nxs xs) nxs (recur nxs))))))

(defcheck solution-da769f36
  (fn closure [rels]
    (loop [rels rels]
      (let [next-rels
            (clojure.set/union
              rels
              (set (for [[a b] rels, [c d] rels :when (= b c)] [a d])))]
        (if (= rels next-rels)
          rels
          (recur next-rels))))))

(defcheck solution-dabeb64d
  (fn f [s]
    (#(if (= s %) % (f %))
     (reduce (fn [a [x y]]
               (into a (keep (fn [[u v]] (if (= u y) [x v])) s)))
       s
       s))))

(defcheck solution-db1beaae
  (fn [s]
    (let [new-s (reduce
                  into
                  s
                  (map
                    (fn [[x1 x2]]
                      (set (map
                             (fn [[y1 y2]]
                               (if (= x2 y1)
                                 [x1 y2]
                                 [x1 x2]))
                             s)))
                    s))]
      (if (= new-s s)
        s
        (recur new-s)))))

(defcheck solution-db286535
  (fn tc [xset]
    (loop [acc #{} rst xset]
      (if (empty? rst)
        (set (remove nil? acc))
        (let [elt (first rst)
              new-acc (conj acc elt)
              more-elt (for [e acc]
                         (cond
                           (= (first elt) (second e)) [(first e) (second elt)]
                           (= (second elt) (first e)) [(first elt) (second e)]
                           :else nil))
              all-acc (set (concat new-acc
                                   (remove #{nil} more-elt)))]
          (recur all-acc (rest rst)))))))

(defcheck solution-db9fb126
  (fn ab [st]
    (let [nst (set (filter vector?
                     (loop [c st [f & r :as s] (vec st)]
                       (if (empty? r)
                         c
                         (recur
                           (into c (map #(let [[a b] % [c d] f]
                                           (if (= a d) [c b]
                                                       (if (= b c) [a d])))
                                     r))
                           r)))))](if (= st nst) st (ab nst)))))

(defcheck solution-dba45dd2
  (fn closure [st]
    (let [mp (reduce #(assoc %1 (first %2) (second %2)) {} st)]
      (reduce
        (fn close-over [out-set next-key]
          (loop [os out-set k (first next-key)]
            (let [v (get mp k)]
              (if (or (= k nil) (= v nil)) os
                                           (recur (conj os [(first next-key) v]) v)))))
        #{} mp))))

(defcheck solution-dbf5cf0d
  (fn [S]
    (letfn [(m [s]
              (mapcat
                (fn [[a b]]
                  (mapcat
                    (fn [x]
                      (map #(vec [a %]) x))
                    (filter #(= (first %) b) s)))
                s))
            ]
      (loop [r S]
        (let [R (into r (m r))]
          (if (= r R)
            r
            (recur R)))))))

(defcheck solution-dc64339c
  #(let [n (apply conj %
             (for [[a b] %
                   [c d] %
                   :when (= b c)]
               [a d]))]
     (if (= n %)
       %
       (recur n))
     ))

(defcheck solution-dc732a4e
  (fn xx[s]
    (letfn [(i[[bs s]]
              (let [n (mapcat (fn[[a b]] (mapcat (fn[[c d]] (when (and (= b c) (not (s [a d]))) [[a d]])) s)) bs)]
                [n (clojure.set/union s (set n))]))]
      (last (last (take-while (comp seq first) (iterate i [s s])))))))

(defcheck solution-dc778b1
  (fn transitive-closure[s]
    (reduce
      (fn[rs [a b]]
        (reduce
          (fn[r [x y]]
            (if(= b x)
              (conj r [a y])
              r))
          rs
          rs))
      s
      s)))

(defcheck solution-dd078233
  (fn [pathset]
    (let [nodes (distinct (mapcat identity pathset))
          considering (for [k nodes i nodes j nodes] [k i j])
          reduce-path (fn [pathset [k i j]]
                        (if (and (pathset [i k]) (pathset [k j]))
                          (conj pathset [i j])
                          pathset))]
      (reduce reduce-path pathset considering))))

(defcheck solution-dd32b1c4
  (fn [rel-set]
    (letfn [(close [rel k to-do]
              (let [add (fn [[m rem-to-do] v]
                          (if (rem-to-do v)
                            (let [[new-m new-to-do] (close m v (disj rem-to-do v))]
                              [(assoc new-m k (clojure.set/union (m k) (new-m v)))
                               new-to-do])
                            [(assoc m k (clojure.set/union (m k) (m v))) rem-to-do]))]
                (reduce add [rel to-do] (rel k))))]
      (loop [rel (reduce (fn [m [k v]]
                           (assoc m k (conj (get m k #{}) v)))
                   {}
                   rel-set)
             to-do (set (keys rel))]
        (if (empty? to-do)
          (set (mapcat (fn [[k vs]]
                         (map (fn [v] [k v]) vs))
                 (seq rel)))
          (let [k (first to-do)
                [ext-rel rem-to-do] (close rel k (disj to-do k))]
            (recur ext-rel rem-to-do)))))))

(defcheck solution-dd3655a3
  (letfn [(closure [s]
            (for [[a b] s
                  [c d] s
                  :when (= b c)]
              [a d]))]

    (fn transitive-closure [s]
      (let [cls (-> s closure set)]
        (if (clojure.set/subset? cls s) s
                                        (recur (clojure.set/union
                                                 s cls)))))))

(defcheck solution-dd61cbf6
  (fn [xs]
    (letfn [(n+ [xs]
              (reduce
                #(if %2 (conj %1 %2) %1)
                xs
                (for [x xs y xs]
                  (cond
                    (= (first x) (second y)) [(first y) (second x)]
                    (= (first y) (second x)) [(first x) (second y)]
                    :else nil))))]
      (loop [n xs n+1 (n+ n)]
        (if (= n n+1) n
                      (recur n+1 (n+ n+1)))))))

(defcheck solution-dd9de0ae
  (fn [s]
    (letfn [(next-links [item]
              (filter #(= (second item) (first %)) s))
            (next-closures [item]
              (map #(vector (first item) (second %)) (next-links item)))
            (closures-from [item]
              (loop [heads [item]
                     results []]
                (if (= 0 (count heads))
                  results
                  (recur (mapcat next-closures heads) (concat results heads)))))]
      (set (mapcat closures-from s))
      )))

(defcheck solution-dde6ed3d
  (fn [rel]
    (let [add (fn [rl]
                (let [nr (for [e1 rl e2 rl
                               :when (= (second e1) (first e2))]
                           [(first e1) (second e2)])]
                  (clojure.set/union rl (apply hash-set (distinct nr)))))]
      (add (add rel)))))

(defcheck solution-de0829ed
  (fn [l]
    (letfn [(tran [[e1 e2] els]
              (remove nil? (map (fn [[x1 x2]] (when (= e2 x1) [e1 x2])) els)))]
      (loop [x l]
        (let [y (set (mapcat #(concat [%] (tran % x)) x))]
          (if (= x y) x (recur y)))))))

(defcheck solution-decfaa22
  (fn [s]
    (loop [s1 s]
      (let [m-fn-1 (fn [i] (filter #(= (second i) (first %)) s1))
            m-fn-2 (fn [i] (map #(vector (first i) (second %)) (m-fn-1 i)))
            s2     (clojure.set/union s1 (into #{} (mapcat m-fn-2 s1)))]
        (if (= s1 s2)
          s1
          (recur s2))
        )
      )
    ))

(defcheck solution-e02980ac
  (fn [s]
    (let [gen-new (fn [s]
                    (set (for [a s b s
                               :when (and (= (last a) (first b))
                                          (not (= a b))
                                          (not (contains? s [(first a) (last b)]))
                                          )]
                           [(first a) (last b)]
                           ))
                    )
          ]
      (loop [acc s new (gen-new acc)]
        (if (empty? new)
          acc
          (recur (into acc new) (gen-new (into acc new))))
        )

      )
    ))

(defcheck solution-e0e00e52
  #(loop [s %] (if-let [
                        trans (seq (keep identity
                                     (for [[a b] s [c d] s :let [e [a d]]]
                                       (if (and (= b c) (not (s e))) e))))]
                 (recur (into s trans))
                 s)))

(defcheck solution-e0e1151f
  (fn c [R]
    (if (empty? R) #{}
                   (let [u (fn [[a b] [c d]] (if (= b c) [[a d]] []))
                         z (last R)
                         r (c (disj R z))
                         ]
                     (into #{}
                       (concat
                        [z]
                        (mapcat #(u z %) r)
                        (mapcat #(u % z) r)
                        r))))))

(defcheck solution-e23f477e
  (fn [s] (let [m (into {} s)]
            (set (mapcat #((fn [[x & s]] (map (partial vector x) s))
                           (take-while (comp not nil?) (iterate m %)))
                   (keys m) )))))

(defcheck solution-e267664c
  (fn clos [st]
    (let [items-starting-by
          (fn [start s]
            (filter #(= start (first %1)) s))
          apply-transitivity
          (fn [item s]
            (map
              #(vector (first item) (last %1))
              (items-starting-by (last item) s)))]
      (let [res
            (set (concat st
                         (mapcat
                           #(apply-transitivity %1 st)
                           st)))]
        (if (= (count st) (count res))
          st
          (clos res))))))

(defcheck solution-e2a4716
  (fn __ [relations]
    (let [catern-product (fn [l1 l2] (for [i l1] [i l2]))]
      (->> (reduce (fn [[to-item from-item] [f t]]
                     [(merge-with concat to-item
                        (into {}
                          (catern-product (conj (from-item t) t) (conj (to-item f) f))))
                      (merge-with concat from-item
                        (into {}
                          (catern-product (conj (to-item f) f) (conj (from-item t) t))))
                      ]              )
             [{} {}]
             relations)
        second
        (mapcat (fn [[k v]] (map vector (repeat k) v)))
        (set)))))

(defcheck solution-e306b489
  (letfn [(transitive-closure
            ([inset] (transitive-closure #{} inset))
            ([oldset inset]
             (if (= inset oldset) inset
                                  (->> inset
                                    (mapcat (fn [[a b]]
                                              (reduce (fn [accum [c d]]
                                                        (cond (= a d) (conj accum [c b])
                                                              (= c b) (conj accum [a d])
                                                              :else accum))
                                                #{} inset)))
                                    (reduce conj inset)
                                    (recur inset)))))]
    transitive-closure))

(defcheck solution-e46505e6
  (letfn [(update [m k] (update-in m [k] into (mapcat m (m k))))
          (setify [in] (reduce (fn [s [a b]] (assoc s a (conj (get s a #{}) b))) {} in))
          (unsetify [s] (set (mapcat #(for [v (second %)] [(first %) v]) s)))]
    (fn [in] (unsetify (reduce update (setify in) (map first in))))))

(defcheck solution-e506754f
  (fn closure [pairs]
    (let [new-pairs (into pairs (for [[a b] pairs, [c d] pairs :when (= b c)] [a d]))]
      (if (= pairs new-pairs) pairs (recur new-pairs)))))

(defcheck solution-e53cba21
  (fn[s](
          loop[ olds #{} news s](
                                  if(= (count olds) (count news)) news
                                                                  (recur news
                                                                    (set(concat news (filter (fn[i](not(nil? (second i))))
                                                                                       (map
                                                                                         (fn[i]
                                                                                           [(first i) (get (into (sorted-map)(vec news)) (second i))]
                                                                                           )
                                                                                         (vec news))))))
                                                                  )

                                )))

(defcheck solution-e541f1c3
  (fn [c]
    (letfn [(mc [c e] (reduce #(if (= (first %2) (last e))
                                 (conj %1 [(first e) (last %2)]) %1) c c))]
      (reduce mc c c))))

(defcheck solution-e55500f3
  (fn tc [s]
    (let [g (reduce ; "grow" the set
              (fn [acc e]
                (let [i (some #(if (= (second %) (first e)) %) acc)]
                  (if i (conj acc [(first i) (second e)])
                        acc)))
              s s)]
      (if (= (count s) (count g)) ; loop until there's no more changes
        s
        (recur g)))))

(defcheck solution-e612cabb
  (fn f [s]
    (#(if (= % s) s (f %))
     (set (for [[a b] s [c d] s]
            [a (if (= b c) d b)])))))

(defcheck solution-e791cb4c
  (fn f [z] (#(if (= % z) z (f %))
             (set (for [[x1 x2] z [y1 y2] z]
                    [x1 (if (= x2 y1) y2 x2)])))))

(defcheck solution-e8182e8f
  (fn [st]
    (letfn [(trans? [v s]
              (or
               (not (empty? (take 1 (filter #(= % v) s))))
               (not (empty? (take 1 (filter #(and
                                              (= (first %) (first v))
                                              (trans? (vector (second %) (second v)) (disj s %))
                                              ) s))))))]
      (let [plain (reduce into #{} st)]
        (into #{} (filter #(trans? % st)(mapcat #(map (fn[item] (vector % item)) plain) plain)))))))

(defcheck solution-e82ea353
  reduce #(let [[r1 r2] %2]
            (clojure.set/union
              (conj %1 %2)
              (set (for [[f s] %1 :when (= f r2)] [r1 s]))
              (set (for [[f s] %1 :when (= s r1)] [f r2]))
              )) #{})

(defcheck solution-e8471203
  (fn txclosure
    ([pairs]
     (into pairs
       (mapcat #(txclosure pairs %) pairs)))
    ([pairs [top cur]]
     (let [nodes (filter #(= cur (first %)) pairs)]
       (concat (map #(identity [top (second %)]) nodes)
               (mapcat #(txclosure pairs [top (second %)]) nodes))))))

(defcheck solution-e86e3799
  (fn close [relation]
    (let
     [domain (set (concat (map first relation) (map second relation)))
      step (set (for [x domain, y domain, z domain, :when (or (relation [x y]) (and (relation [x z]) (relation [z y])))] [x y]))]
      (if (= step relation) step (close step)))))

(defcheck solution-e8805705
  (letfn [(transit-1 [edges]
            (let [nodes (group-by first edges)]
              (into edges (for [[from to] nodes
                                [_ intermediate] to
                                [_ endpoint] (nodes intermediate)]
                            [from endpoint]))))]
    (fn [edges]
      (->> edges
        (iterate transit-1)
        (partition 2 1)
        (drop-while (partial apply not=))
        ffirst))))

(defcheck solution-e902ce33
  (fn trans [coll]
    (letfn [(brel [_r x]
              (loop [ret []
                     key (get _r x)
                     val (get _r key)]
                #_(println "debug:" x key val)
                (if val
                  (recur (conj ret [x val]) val (get _r val))
                  ret
                  )))]
      (let [_map (reduce merge (map #(apply hash-map %) coll))]
        (reduce #(if (coll? (first %2))
                   (apply conj %1 %2)
                   (conj %1 %2))
          #{}
          (for [c coll]
            (let [_brel (brel _map (first c))]
              (if (empty? _brel)
                c
                (conj _brel c)))))))))

(defcheck solution-e93df1f
  (fn [coll]
    (loop [coll coll]
      (let [new-coll (into coll
                       (for [[a b] coll
                             [c d] coll
                             :when (= b c)]
                         [a d]))]
        (if (= coll new-coll)
          coll
          (recur new-coll))))))

(defcheck solution-e9ba8bce
  (fn __
    [input]
    (letfn [(head-and-tails
              [s e]
              (let [t (vec (clojure.set/difference s #{e}))
                    c (count t)]
                (partition 2 1 (take (dec (* 2 c)) (cycle t)))))

            (trans-cloj
              [h t]
              (filter #(not= % h)
                (into #{} (map (fn [s]
                                 (reduce (fn [r e]
                                           (if (= (last r) (first e))
                                             [(first r) (last e)]
                                             r))
                                   h s))
                            t))))]
      (clojure.set/union input
        (into #{}
          (mapcat (fn [e]
                    (trans-cloj e (head-and-tails input e)))
            input))

        ))))

(defcheck solution-e9e982e1
  (fn transitive-binary
    [tuple-set]
    (let [tuple-first-map
          (into {} (map #(vector (first %) %) tuple-set))]
      (loop [remaining-ts tuple-set
             acc-set #{}]
        (let [current-tuple (first remaining-ts)
              transitive-candidate (if (tuple-first-map (second current-tuple))
                                     (vector
                                       (first current-tuple)
                                       (second (tuple-first-map (second current-tuple)))))
              transitive-closure
              (if (and transitive-candidate (not (acc-set transitive-candidate)))
                transitive-candidate)
              remaining-ts' (disj (if transitive-closure
                                    (conj remaining-ts transitive-closure)
                                    remaining-ts) current-tuple)]
          (if (seq remaining-ts)
            (recur remaining-ts' (conj acc-set current-tuple))
            acc-set))))))

(defcheck solution-e9ebc25d
  (fn [s]
    (letfn [(find-clos
              [v s]
              (remove nil?
                (map (fn [x]
                       (when (= (last v) (first x))
                         [(first v) (last x)]))
                  s)))]
      (set
        (reduce (fn [acc v]
                  (let [no-v (disj acc v)
                        r (find-clos v no-v)]
                    (apply conj acc v r)))
          s
          s)))))

(defcheck solution-ec03bbe9
  (fn f[xs]
    (loop [[[a b] & rel :as rels] (into [] xs) ax xs new? false]
      (if (and (empty? rels)(not new?))
        ax
        (let [rels' (if (empty? rels) (into [] ax) rel)
              new-rels (if (empty? rels) [] (map (fn [[_ c]][a c]) (filter #(= (first %) b) ax)))
              ax' (into ax new-rels)
              new?' (not= ax' ax)]
          (recur rels' ax' new?'))))))

(defcheck solution-ecdf1407
  (fn [r]
    (let [inter clojure.set/intersection
          union clojure.set/union
          diff clojure.set/difference
          adj-tab (apply merge-with union (map (fn [[x y]] {x #{y}}) r))
          nodes (into #{} (flatten (seq r)))]
      (letfn [(reach [xs vs ys]
                (if (empty? vs) xs
                                (let [vs' (inter ys (apply union (map #(adj-tab %) vs)))]
                                  (recur (union xs vs) vs' (diff ys vs')))))
              (reach' [v]
                (disj (reach #{} #{v} (disj nodes v)) v))
              (reach-r [v]
                (into #{} (map #(-> [v %]) (reach' v))))]
        (apply union (map #(reach-r %) nodes))))))

(defcheck solution-ed031950
  (fn [r]
    (let [fmap (apply hash-map (flatten (seq r)))
          smap (apply hash-map (flatten (map (fn [[f s]] [s f]) r)))
          roots (filter #(not (smap %)) (map first r))]
      (letfn [(p84h [root]
                (if root  (cons root (p84h (fmap root)))))
              (bt [[f & rem]]
                (if f (concat (for [rr rem] [f rr]) (bt rem))))]
        (set (apply concat (for  [a (for [r roots] (p84h r))] (bt a))))))))

(defcheck solution-ed13d6e5
  (fn myf [coll]
    (letfn [(sub [coll]
              (into coll (for [x coll
                               y coll
                               :when (= (second x) (first y))]
                           (vector (first x) (second y)))))]
      (if (= coll (sub coll)) coll
                              (myf (sub coll))))))

(defcheck solution-ed4c0346
  (fn [brs]
    (let [find-ts (fn [[start end] nodes]
                    (loop [r [[start end]] r_node [start end] nodes nodes last_end end]
                      (let [l_node (first (filter (fn [[s e]] (= s (last r_node))) nodes))]
                        (if (nil? l_node) (conj r [start last_end])
                                          (recur (concat r [r_node l_node [(first r_node) (last l_node)]]) l_node (disj nodes l_node) (last l_node))
                                          )
                        )
                      )
                    )]
      (set (apply concat (for [bs brs] (find-ts bs (disj brs bs)))))
      ;(for [bs brs] [bs (disj brs bs)])
      )
    ))

(defcheck solution-ed720610
  (fn tc [rs]
    (apply clojure.set/union
      (for [rel rs
            :let [n (first rel) fep (second rel)]]

        (loop [endpts [fep] out #{}]
          (if-let [x (first endpts)]
            (recur (into (subvec endpts 1) (map second (filter #(= x (first %)) rs))) (conj out [n x]))
            out))))))

(defcheck solution-ee623e02
  (fn a [t]
    (set
      (mapcat
        (fn r [[k v]]
          (conj (mapcat #(r [k (second %)]) (filter #(= v (first %)) t)) [k v]))
        t))))

(defcheck solution-ee892dd8
  (fn [s]
    (loop [w1 "", w2 "", rm (seq s), acc #{}]
      (cond (and (empty? rm) (= "" w1)) acc
            (= "" w1) (recur (-> rm first first) (-> rm first second) (rest rm) (conj acc (first rm)))
            :else (let [r (first (filter #(= w2 (first %)) s))
                        w3 (second r)]
                    (if (nil? r)
                      (recur "" "" rm acc)
                      (recur w1 w3 rm (conj acc (vector w1 w3)))))))))

(defcheck solution-eeb49ebb
  (fn [s]
    (reduce (fn [s [h t :as e]]
              (let [before (map (fn [[x y]] [h y]) (filter #(= t (first %)) s))
                    after (map (fn [[x y]] [x t]) (filter #(= h (second %)) s))]
                (into (into (conj s e) before) after))) #{} s)))

(defcheck solution-efbcfd1a
  #(into #{} (reduce (fn [s [x y]] (concat s [[x y]]
                                           (for [[a b] s :when (= b x)] [a y])
                                           (for [[a b] s :when (= y a)] [x b]))) #{} %)))

(defcheck solution-efbdcb9d
  (fn tr-closure [items]
    (let [m (into {} items)
          findreachable
            (fn [orig-i]
              ((fn f [i]
                 (if (or (nil? i) (= i orig-i))
                   []
                   (let [next-i (m i)]
                     (conj (f next-i) i))))
               (m orig-i)))]
      (set (mapcat #(map vector (repeat %1) (findreachable %1))
             (map first items))))))

(defcheck solution-efbfade9
  (fn [input] (let [as-keyword #(if (keyword? %) % (keyword (str %)))
                    keyed (apply hash-map (apply concat (map (fn [p] [(as-keyword (first p)) {:val (first p) :link (second p)}]) input)))
                    routes (map (fn [v]
                                  #_(println v)
                                  [(key v) (loop [k (as-keyword (:link (val v)))
                                                  routes (list (val v))]
                                             #_(println k {:link (get-in keyed [k :link])})
                                             (if-not (get-in keyed [k :link]) routes
                                                                              (recur
                                                                                (as-keyword (get-in keyed [k :link]))
                                                                                (cons {:val (:val (val v)) :link (get-in keyed [k :link])} routes))))]) keyed)]
                (into #{} (mapcat (fn [r] (for [k (list (first r))
                                                v (second r)]
                                            [(:val v) (:link v)])) routes)))))

(defcheck solution-f184124f
  (fn [group]
    (loop [s group]
      (let [result (apply conj s (remove nil? (for [x s y (disj s x)] (if (= (second x) (first y)) [(first x)(second y)]))))]
        (if (= s result) result
                         (recur result))))))

(defcheck solution-f1932356
  (fn trans [col]
    (letfn [(close1 [col]
              (set (concat col
                           (for [[x y] col
                                 [x1 y1] col
                                 :when (= y x1)]
                             [x y1]))))]
      (let [col1 (close1 col)]
        (if (= (count col) (count col1))
          col1
          (trans col1))))))

(defcheck solution-f2534be1
  (fn transitive-closure [r]
    (let [new-r (into r (for [[w x] r [y z] r :when (= x y)]
                          [w z]))]
      (if (= new-r r) new-r (recur new-r)))))

(defcheck solution-f29b0c23
  (fn transitive-closure [s]
    (let [new-set  (clojure.set/union s (set (for [[a b] s [c d] s :when (= b c)] [a d])))]
      (if (= s new-set) s (transitive-closure new-set)))))

(defcheck solution-f39eabe1
  (fn [s]
    (let [trans (fn [l]
                  (loop [tmps s a #{}]
                    (if (empty? tmps)
                      a
                      (if (= (last l) (first (first tmps)))
                        (recur (rest tmps) (conj a (conj (conj [] (first l)) (last (first tmps)))))
                        (recur (rest tmps) a))))) create (fn [s2]
                                                           (loop [tmp s2 a s]

                                                             (if (empty? tmp)
                                                               (set a)
                                                               (recur (rest tmp) (concat a (trans (first tmp)))))))]
      (loop [a s c (count s)]
        (if (= c (count (create a)))
          a
          (recur (create a) (count (create a))))))))

(defcheck solution-f3c2ef63
  (fn __ [someset]
    (let[ clos
         (reduce conj someset
           (filter #(not (nil? (last %)))
             (map #(vector (first %) (last(first (filter (fn[x](= (first x) (last %)))  someset)))) someset)))]
      (
        if(= clos someset) clos (__ clos)))))

(defcheck solution-f4a4d80a
  (fn transitive-clojure [relations]
    (letfn [(vector-to-map [v]
              (set (map #(apply hash-map %) v)))
            (map-to-vector [m]
              (reduce #(conj %1 (key %2) (val %2)) [] m))
            (project-relations [rels ks]
              (set (filter not-empty (clojure.set/project rels ks))))
            (get-keys [rels]
              (reduce #(conj %1 (key (first %2))) [] rels))
            (get-vals [rels]
              (reduce #(conj %1 (val (first %2))) [] rels))
            (find-trans
              ([k rels]
               (let [vs (project-relations rels [k])]
                 (find-trans k rels vs)))
              ([k rels acc]
               (let [transitive-keys (get-vals acc)
                     vs (project-relations rels transitive-keys)
                     transitive-relations-values (get-vals vs)
                     transitive-relations (reduce #(conj %1 (hash-map k %2)) #{} transitive-relations-values)
                     new-transitive-relations (clojure.set/difference transitive-relations acc)]
                 (if (empty? new-transitive-relations) acc
                                                       (recur k rels (clojure.set/union acc new-transitive-relations))))))]
      (let [rels (vector-to-map relations)
            ks (get-keys rels)]
        (set (map map-to-vector (reduce clojure.set/union (map #(find-trans % rels) ks))))))))

(defcheck solution-f57c79f1
  (fn [x]
    (nth
      (iterate
        (fn [y]
          (reduce
            (fn [l [a b]]
              (set
                (into
                  l
                  (keep
                    (fn [[c d]]
                      (if (= b c)
                        [a d]))
                    x))))
            y y))
        x)
      2)))

(defcheck solution-f586e2da
  (fn t [v]
    (let [r (set (concat v
                         (for [[a b] v [c d] v
                               :when (= b c)] [a d])))]
      (if (= v r) v (t r)))))

(defcheck solution-f5c92876
  (fn [r]
    (let [vmap (apply merge-with into
                 (map (fn [[v1 v2]] {v1 #{v2}}) r))
          iter (fn [v] (loop [m v k (keys v)]
                         (if (empty? k) m
                                        (recur
                                          (update-in m [(first k)]
                                            (fn [s]
                                              (reduce into s (map m s))))
                                          (rest k)))))
          closure (loop [cur vmap]
                    (let [nex (iter cur)]
                      (if (= nex cur) cur
                                      (recur nex))))]
      (set (mapcat (fn [[v n]] (map #(vector v %) n)) closure)))))

(defcheck solution-f5f290ed
  (fn [s]
    (loop [i s, [h & t] (vec s)]
      (let [g (for [[al ar] [h], [bl br] t
                    :let [j (#{ar} bl) k (#{br} al)]
                    :when (or j k)]
                (cond j [al br]
                      k [bl ar]))]
        (if t (recur (into i g) `[~@t ~@g]) i)))))

(defcheck solution-f613be3
  (fn [rel]
    (let [all-pairs (for [x rel y rel] [x y])
          trans-pairs (filter #(= (last (first %)) (first (last %))) all-pairs)
          joins (into rel (map #(vector (first (first %)) (last (last %))) trans-pairs))]
      (if (= joins rel)
        rel
        (recur joins)))))

(defcheck solution-f7096bb7
  (fn c84
    ([s r]
     (if (empty? s)
       r
       (let [f (reduce (fn [x y]
                         (let [fy (first y)
                               fyv (get x fy)]
                           (assoc x fy (if fyv (conj fyv y) [y])))) {} r)
             v (first s)
             n (f (second v))]
         (if n
           (let [val (map #(vector (first v) (second %)) n)]
             (c84 (apply conj (next s) val) (apply conj r val)))
           (c84 (next s) r)))))
    ([s]
     (let [l (apply list s)]
       (c84 l s)))))

(defcheck solution-f70b1eee
  (fn [relation]
    (letfn [(directed-flood-fill-1 [r ptlist include-self]
              (set
                (concat (if include-self ptlist '())
                        (mapcat (fn [pt]
                                  (mapcat (fn [e]
                                            (cond
                                              (= pt (first e)) (list (second e))
                                              :else ())) r)) ptlist))))
            (directed-flood-fill
              ([ptlist]
               (directed-flood-fill ptlist true))
              ([ptlist first-entry]
               (let [newlist (set (directed-flood-fill-1 relation
                                    ptlist (not first-entry)))]
                 (if (= (set ptlist) newlist)
                   newlist
                   (recur newlist false)))))
            (collect-lhs []
              (map (fn [i] (first i)) relation))
            (gen-closure [lhs rhs-list]
              (map (fn [rhs] (vec (list lhs rhs))) rhs-list))
            ]
      (set (mapcat (fn [lhs] (gen-closure lhs (directed-flood-fill (list lhs))))
             (collect-lhs))))))

(defcheck solution-f750b9b1
  (fn [c r]
    (let [m (reduce #(assoc % (%2 0) (%2 1)) {} r)]
      (loop [o r n (c o)]
        (let [p (into o (for [x o :when (m (x 1)) ] [(x 0) (m (x 1))]))]
          (if (= (c p) n) p (recur p (c p))))))) count)

(defcheck solution-f76a3182
  (fn tc [relset]
    (letfn [
            (combine [[a1 _][_ b2]]  [a1 b2])
            (extends [[_ a2][b1 _]]  (= a2 b1))]
      (loop
       [candidates relset
        acc #{}]
        (if (empty? candidates)
          acc
          (let [fc (first candidates)
                rc (rest candidates)
                nacc (conj acc fc)
                new_transitions (set (map #(combine fc %) (filter #(extends fc %) relset)))
                nc (clojure.set/union (set rc) new_transitions)]
            (recur nc nacc)
            ))))))

(defcheck solution-f77e59a
  (fn f [s]
    (#(if (= % s) s (f %))
     (set (for [[a b] s [c d] s]
            [a (if (= b c) d b)])))))

(defcheck solution-f7f4073d
  (fn [br]
    (letfn [(squish [tc]
              (vector (ffirst tc) (last (last tc))))

            (chains [xs]
              (chain xs (ffirst xs)))

            (chain [xs lst]
              (if (empty? xs)
                '()
                (if (= lst (ffirst xs)) ;; keeper
                  (cons (first xs) (chain (rest xs) (last (first xs))))
                  '())))]
      (set
        (map squish
          (for [a br
                b br
                c br]
            (chains [a b c])))))))

(defcheck solution-f80bd7c4
  (fn [s]
    (letfn [(grow-1 [pairs]
              (let [pairs (vec pairs)
                    pairs' (for [[a b] pairs
                                 [bb cc] pairs :when (= b bb)]
                             [a cc])]
                (set (concat pairs pairs'))))
            (grow [pairs]
              (let [pairs' (grow-1 pairs)]
                (if (= pairs' pairs)
                  (set pairs)
                  (recur pairs'))))]
      (grow s))))

(defcheck solution-f87db2a9
  (fn [rel]
    (let [rel (into {} rel)]
      (set
        (apply concat
          (doall
            (for [[s t] rel]
              (loop [clos (list [s t]) t t]
                #_(prn s t (get rel t) clos)
                (if-let [tp (get rel t)]
                  (recur (conj clos [s tp]) tp)
                  clos)))))))))

(defcheck solution-f87eb04
  (fn transitive-closure [ps]
    (letfn
     [
      (find-pairs-with-a [v]
        (filter
          (fn [[a b]]
            (= a v)
            )
          ps
          )
        )

      (combine-pair [[a b]]
        (map
          (fn [[a' b']]
            [a b']
            )
          (find-pairs-with-a b)
          )
        )

      (find-pairs [ps]
        (set
          (mapcat combine-pair ps)
          )
        )
      ]
      (loop
       [
        ps ps
        ]
        (let
         [
          new-ps (find-pairs ps)
          ps' (clojure.set/union ps new-ps)
          ]
          (if
           (empty? (clojure.set/difference new-ps ps))
            ps
            (recur ps')
            )
          )
        )
      )
    ))

(defcheck solution-f8d3ee14
  (fn [i-set]
    (loop [res-set i-set
           iter-set (seq i-set)]
      (if (empty? iter-set)
        res-set
        (let [fitem (first (first iter-set))
              nitem (second (first iter-set))]
          (recur
            (reduce (fn [s1 iter]
                      (if (= nitem (first iter))
                        (conj s1 [fitem (second iter)])
                        s1))
              res-set
              res-set)
            (rest iter-set)))))))

(defcheck solution-f8d75ff
  (fn [s]
    (let [edges (into {} s)
          dfs (fn [s]
                (let [root (ffirst s)]
                  (loop [node root
                         res #{}]
                    (if-let [child (edges node)]
                      (recur child
                        (conj res [root child]))
                      res))))]
      (loop [s s
             res #{}]
        (if (empty? s)
          res
          (recur (rest s)
            (clojure.set/union res (dfs s))))))))

(defcheck solution-f9589f0b
  (letfn [(conn [ss] (into ss (for [h ss t ss :when (= (last h) (first t))] [(first h) (last t)])))]
    (fn [ss] (let [n (conn ss)] (if (= ss n) ss (recur n))))))

(defcheck solution-f9a51c2d
  (fn f [s]
    (let [n (fn [s] (mapcat
                      (fn [[a b]]
                        (map #(vector a %)
                          (map second
                            (filter #(and (= b (first %)) (not= b (second %))) s)))) s))
          ns (n s)
          diff (apply disj (set ns) (set s))]
      (if (empty? diff)
        s
        (set (f (concat s ns)))))))

(defcheck solution-fa8b830f
  (fn [relation]
    (let [m (into {} relation)
          transitive-mappings (fn [k]
                                (take-while second (iterate (fn [[_ next-k]]
                                                              [k (m next-k)])
                                                     [k (m k)])))]
      (apply conj relation (mapcat transitive-mappings (keys m))))))

(defcheck solution-fa8d8924
  (fn [s]
    (let [get-rel (fn [s [k v]]
                    (reduce (fn [acc [a b]]
                              (if (= b k)
                                (if (contains? acc [a v])
                                  acc
                                  (conj acc [a v]))
                                acc)) #{} s))
          all-rel (fn [s]
                    (let [res (reduce #(let [r (get-rel s %2)]
                                         (if (empty? r)
                                           %1
                                           (apply conj %1 r))) #{} s)]
                      (if (empty? res)
                        s
                        (apply conj s res))))]
      (loop [s s]
        (let [ns (all-rel s)]
          (if (= s ns)
            ns
            (recur ns)))))))

(defcheck solution-fa95e32c
  (fn tclosure [coll]
    (let [relations (into {} coll)]
      (letfn
       [(find-path [src]
          (loop [x src paths #{}]
            (if-let [dest (get relations x)]
              (recur dest (conj paths [src dest]))
              paths)))]
        (reduce clojure.set/union
          (for [elem (keys relations)]
            (find-path elem)))))))

(defcheck solution-faa492d1
  (fn tc
    ([s] (tc (seq s) #{}))
    ([[[s e] & ss] r]
     (let [fs (filter #(= s (last %)) r)
           ls (filter #(= e (first %)) r)
           nr (conj r [s e])
           nr (concat nr (map #(list (first %) e) fs))
           nr (concat nr (map #(list s (last %)) ls))]
       (if ss
         (recur ss nr)
         (set nr))))))

(defcheck solution-faa829c7
  (fn [coll]
    (let [pairs (into {} coll)
          children (set (vals pairs))
          roots (remove children (keys pairs))
          doit (fn [root] (let [chain (take-while #(not (nil? %)) (iterate #(pairs %) root))]
                            (loop [[head & tail :as chain] chain chunks []]
                              (if (empty? chain)
                                chunks
                                (recur tail (into chunks (map (fn [x] [head x]) tail)))))))]
      (set (mapcat doit roots)))))

(defcheck solution-fad47d6f
  (fn [relations]
    (let [new-relations
          (clojure.set/union
            relations
            (set (mapcat (fn [[old-x old-y :as old]]
                           (map (fn [[other-x _]] [other-x old-y])
                             (filter #(= old-x (second %))
                               (disj relations old))))
                   relations)))]
      (if (= (count relations) (count new-relations))
        relations
        (recur new-relations)))))

(defcheck solution-faedcb1
  (fn [s]
    (set (apply concat
           (for [x s]
             (loop [v (remove #{x} s) r [x]]
               (let [y (last r) n (first (filter #(= (first %) (second y)) v))]
                 (if (nil? n)
                   r
                   (recur (remove #{n} v) (concat r [[(first y) (second n)]]))))))))))

(defcheck solution-fb553208
  (fn f [d]
    (let [p (for [[a x] d [y b] d :when (= x y)] [a b])
          g (into d p)]
      (if (= g d) g (f g)))))

(defcheck solution-fbade225
  (fn
    [coll]
    (let [helper (fn [acc coll]
                   (if (seq coll)
                     (let [[h t] (first coll)
                           coll2 (map (fn [[_ t2]] [h t2])
                                   (filter (fn [[h2 _]] (= h2 t)) acc))]
                       (recur (set (concat acc coll2)) (rest coll)))
                     acc))]
      (helper coll coll))))

(defcheck solution-fbc3bb4e
  (fn [points]
    (letfn [(belongs-in [a b]
              (seq (filter #(or (= (first a) (second %1))
                                (= (first %1) (second a)))
                     b)))
            (partit [ps]
              (loop [parts (set (map #(conj #{} %1) ps))
                     es ps]
                (if-not (seq es)
                  parts
                  (recur (set
                           (for [p parts, e ps
                                 :when (belongs-in e p)]
                             (conj p e)))
                    (rest es)))))
            (domain [ps]
              (map #(first %1) ps))
            (codomain [ps]
              (map #(second %1) ps))
            (tclosure [ps]
              (into ps
                (for [x (domain ps) y (codomain ps)
                      :when (not (or (= x y)
                                     (ps [y x])))]
                  [x y])))]
      (into points ; cheats :(
        (apply concat (map tclosure (partit points)))))))

(defcheck solution-fbe34201
  (fn [edges]
    (let [vertices (reduce into #{} edges)
          adj-for (fn [f] (into {} (for [u vertices]
                                     [u (into {} (for [v vertices]
                                                   [v (f u v)]))])))
          adj (adj-for (fn [u v] (contains? edges [u v])))
          fadj (reduce (fn [adj k]
                         (adj-for (fn [u v] (or (get-in adj [u v])
                                                (and (get-in adj [u k])
                                                     (get-in adj [k v]))))))
                 adj vertices)]
      (into #{} (for [u vertices
                      v vertices
                      :let [edge [u v]]
                      :when (get-in fadj edge)]
                  edge)))))

(defcheck solution-fc3d21c4
  (fn [e]
    (let [kp (fn [x] (keep #(if (= (first %) x) (second %)) e))
          pair (fn [x s] (for [y s] [x y]))
          n (remove empty? (mapcat #(pair (first %) (kp (second %))) e))
          ne (into e n)]
      (if (= e ne) e (recur ne)))))

(defcheck solution-fcb30db1
  (fn transitiveX[rels]
    (letfn [
            (transitiveRes[relMap nextKey firstKey]
              (let [vl (get relMap nextKey)]
                (if (nil? vl)
                  #{}
                  (conj (transitiveRes relMap vl firstKey)[firstKey vl])
                  )
                )
              )
            ]
      (let [relMap (zipmap (map first rels) (map last rels))]
        (apply clojure.set/union (map #(transitiveRes relMap % %) (keys relMap)))
        )
      )
    ))

(defcheck solution-fcd43b78
  (fn[i]
    (let [mp (fn [its]
               (mapcat (fn [d]
                         (reduce
                           (fn [r c] (conj r [(first d) (second c)]))
                           [d]
                           (filter #(= (last d) (first %)) its)))
                 its))]
      (set ((apply comp (repeat (count i) mp)) i)))
    ))

(defcheck solution-fcdbf8d7
  (letfn [(subset? [x y] (every? y x))
          (union [x y] (set (concat x y)))
          (prod [r s] (set (for [x r y s] [x y])))
          (join [r s] (for [[x1 y1] r [x2 y2] s :when (= y1 x2)] [x1 y2]))]
    (fn tc [r]
      (let [r2 (join r r)]
        (if (subset? r2 r) r (tc (union r r2)))))))

(defcheck solution-fd451031
  (fn tc [s]
    (let [ children (reduce
                      #(assoc %1 (first %2) (cons (second %2) (%1 (first %2)))) {}
                      (seq s))

          descendents (fn d [x]
                        (concat (children x)
                                (mapcat d (children x)))) ]

      (set (mapcat
             #(for [ d (descendents %) ] [% d])
             (keys children))))))

(defcheck solution-fd8852c9
  (fn [sets]
    (loop [s sets]
      (let [more (into s (for [[i1 i2] s, [j1 j2] s :when (= i2 j1)] [i1 j2]))]
        (if (= (count s) (count more))
          more
          (recur more))))))

(defcheck solution-fe5c3c97
  (fn transitive-closure
    [s]
    (into #{} (mapcat #((fn _
                          ([item s] (_ item s #{}))
                          ([[a b] s seen]
                           (filter (complement nil?)
                             (mapcat (fn [[c d]]
                                       (if (and (= b c) (not (seen [a d])))
                                         (conj (_ [a d] s (conj seen [a d])) [a d])))
                               (conj s [b b])))))
                        % s) s))))

(defcheck solution-fea82e3f
  (fn [s]
    (let [m (apply hash-map (flatten (apply list s)))]
      (loop [m_ (rest m) e (first m) ret #{}]
        (cond
          (nil? e) ret
          (ret e) (recur (rest m_) (first m_) ret)
          (m (e 1)) (recur m_ [(e 0) (m (e 1))] (conj ret e))
          :else (recur (rest m_) (first m_) (conj ret e)))))))

(defcheck solution-ff5f57b0
  (fn [s] (last (take (count s) (iterate (fn [s] (into s
                                                   (for [[x1 x2] s [y1 y2] s :when (= x2 y1)] [x1 y2]))) s )))))

(defcheck solution-ffcf14d3
  (fn [relations]
    (letfn [(to-map [rels]
              (apply merge-with concat (map (fn [[l r]] {l [r]}) rels)))
            (new-rels-from [map]
              (for [[src dsts] map
                    dst dsts
                    :when (contains? map dst)
                    :let [new-dsts (get map dst)]
                    new-dst new-dsts]
                [src new-dst]))
            (produce-from [rels]
              (let [new-rels (new-rels-from (to-map rels))
                    all-rels (set (concat rels new-rels))]
                (if (= rels all-rels)
                  all-rels
                  (produce-from all-rels))))]
      (produce-from relations))))

(defcheck solution-ffd7aa6b
  (fn transitive-closure [s]
    (reduce (fn [xs tuple]
              (let [new-tuples (clojure.set/union xs s)
                    xss (clojure.set/union xs #{tuple})
                    xss (if-let [ts (seq (filter #(= (second %) (first tuple)) new-tuples))]
                          (clojure.set/union xss (set (map (fn [t] [(first t) (second tuple)]) ts)))
                          xss)
                    xss (if-let [ts (seq (filter #(= (first %) (second tuple)) new-tuples))]
                          (clojure.set/union xss (set (map (fn [t] [(first tuple) (second t)]) ts)))
                          xss)]

                xss)) #{} s)))

(defcheck solution-ffdbbaea
  (fn [rel]
    (let [es (apply merge-with clojure.set/union
               (map (fn [[a b]] {a #{b}}) rel))]
      (set (mapcat (fn [a] (map #(vector a %)
                             ((fn rs [b] (mapcat #(cons % (rs %)) (es b))) a)))
             (keys es))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-84))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
