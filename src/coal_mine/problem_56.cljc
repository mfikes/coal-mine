(ns coal-mine.problem-56
  (:require [coal-mine.checks :refer [defcheck-56] :rename {defcheck-56 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-104f52bf
  (fn f[v]
    (reduce #(if ((set %) %2) % (conj % %2)) [] v)))

(defcheck solution-1065437a
  (fn dst [coll]
    (->> coll
      (reduce
        (fn [[found acc] e]
          (if (found e)
            [found acc]
            [(conj found e) (conj acc e)]))
        [#{} []])
      second)))

(defcheck solution-1079d96c
  (fn dist [s]
    (loop [res []
           leftover s
           seen #{}]
      (let [nxt (first leftover)]
        (if (= leftover [])
          res
          (recur
            (if (get seen nxt)
              res
              (conj res nxt))
            (rest leftover)
            (conj seen nxt)))))))

(defcheck solution-111512b3
  (fn [col]
    (reduce (fn [acc v] (if (some #(= % v) acc) acc (conj acc v))) [] col)))

(defcheck solution-113a28aa
  (fn [coll]
    (let [step (fn step[coll v]
                 (when-let [[f & r] coll]
                   (if (contains? v f)
                     (recur r (conj v f))
                     (cons f (step r (conj v f))))))]

      (step coll #{}))))

(defcheck solution-1177c0ce
  reduce (fn [a x]
           (if (some #(= % x) a)
             a
             (conj a x))) [])

(defcheck solution-11cb2131
  (fn [coll]
    (-> (reduce (fn [{:keys [result seen] :as last} item]
                  (if-not (seen item)
                    {:result (conj result item)
                     :seen (conj seen item)}
                    last))
          {:result [] :seen #{}}
          coll)
      :result)))

(defcheck solution-1237e152
  (fn [l]
    (first
      (reduce
        (fn [[_ s :as acc] x]
          (map (if (s x) identity #(conj % x)) acc))
        [[] #{}] l))))

(defcheck solution-124501f2
  (letfn
   [(in? [x xs]
      (some #(= x %) xs))
    (add-unique [a x]
      (if (in? x a) a (conj a x)))]
    (fn [xs]
      (reduce add-unique [] xs))))

(defcheck solution-129cb6e6
  #(loop [r [] rm %]
     (if (empty? rm) (reverse r)
                     (recur (cons (first rm) r) (remove #{(first rm)} rm)))))

(defcheck solution-13972859
  (fn my-distinct
    [s]
    (first
      (reduce
        (fn [[no-dup-s existing-vals] new-val]
          (if (contains? existing-vals new-val)
            [no-dup-s existing-vals]
            [(conj no-dup-s new-val) (conj existing-vals new-val)]))
        [[] #{}]
        s))))

(defcheck solution-13ad91fa
  (fn custom-distinct
    [coll]
    (reduce
      (fn [reduce-coll el]
        (let [match (some #(= el %) reduce-coll)]
          (if (nil? match)
            (conj reduce-coll el)
            reduce-coll)))
      [] coll)))

(defcheck solution-13c1a1b5
  (fn [coll]
    (let [no-rep (fn [xs x] (if (contains? (set xs) x) xs (concat xs [x])))]
      (reduce no-rep [] coll))))

(defcheck solution-13d3689d
  (fn distinct-items [items]
    (reduce (fn [x y] (if (nil? (some (fn [z] (= y z)) x)) (conj x y)
                                                           x)) [] items)))

(defcheck solution-13d95a11
  (fn [s](reduce (fn[x y] (if (contains? (set x) y) x (conj x y))) [] s)))

(defcheck solution-14362eb4
  (fn [coll]
    (loop [l coll acc []]
      (if (empty? l)
        acc
        (if (some #{(first l)} acc)
          (recur (rest l) acc)
          (recur (rest l) (conj acc (first l))))))))

(defcheck solution-14633915
  (fn only-distinct [col]
    (reduce (fn [xs item] (if (some #(= item %1) xs) xs (conj xs item)))
      [] col)))

(defcheck solution-148a2b3c
  (fn [x]
    (letfn [(dedup [seen l]
              (let [e (first l)
                    r (rest l)]
                (if (and (nil? (seen e)) (not (empty? l)))
                  (cons e (dedup (clojure.set/union #{e} seen) r))
                  (if (not (empty? l))
                    (dedup seen r)))))] (dedup {} x))))

(defcheck solution-14b0f34
  (fn [xs]
    ((fn ss [xs s]
       (lazy-seq
         (let [[f :as xs] (drop-while #(contains? s %)xs)]
           (when (seq xs)
             (cons
               f
               (ss
                 (rest xs)
                 (conj s f)))))))  xs #{})))

(defcheck solution-15181a94
  (fn [nums]
    (loop [remaining nums seen (hash-set) ans []]
      (if (empty? remaining)
        ans
        (if (contains? seen (first remaining))
          (recur (rest remaining) seen ans)
          (recur (rest remaining) (conj seen (first remaining)) (conj ans (first remaining))))))))

(defcheck solution-158470b
  (fn distinct2 [x]
    (nth
      (reduce
        (fn [[res occurencies] b]
          (if (contains? occurencies b)
            [res occurencies]
            [(conj res b) (conj occurencies b)])
          )
        [[] #{}]
        x)
      0
      )
    ))

(defcheck solution-159046ed
  (fn [in] (reduce #(if ((set %) %2) % (conj % %2)) [] in )))

(defcheck solution-16052b11
  (fn my-distinct
    ([coll] (my-distinct coll #{} []))
    ([[x & xs] encountered res]
     (if (nil? x)
       res
       (if (encountered x)
         (recur xs encountered res)
         (recur xs (conj encountered x) (conj res x)))))))

(defcheck solution-16775d29
  (fn [coll]
    (reduce (fn [acc x]
              (if (some #{x} acc)
                acc
                (conj acc x))) [] coll)))

(defcheck solution-16ebe46e
  (fn prob-0056
    [xs]
    (let [red (fn [lhs x]
                (let [bld  (first lhs)
                      seen (second lhs)]
                  (if (contains? seen x) lhs [(cons x bld) (conj seen x)]))) ]

      (reverse (first (reduce red [[], #{}] xs))))))

(defcheck solution-16f32240
  (fn unique [c]
    (loop [s (seq c)
           table #{}
           result '()]
      (let [f (first s) r (rest s)]
        (cond
          (empty? s) (reverse result)
          (contains? table f) (recur r table result)
          :else (recur r (conj table f) (conj result f)))))))

(defcheck solution-177618eb
  #(loop [coll %1, s #{}, result []]
     (if (empty? coll)
       result
       (let [x (first coll), dup (contains? s x)]
         (recur (rest coll) (conj s x) (if dup result (conj result x)))))))

(defcheck solution-17d83927
  (fn uniques
    ([xs] (uniques xs #{}))
    ([xs dups]
     (if (empty? xs)
       nil
       (let [[h & t] xs]
         (if (contains? dups h)
           (uniques t dups)
           (lazy-cat [h] (uniques t (conj dups h)))))))))

(defcheck solution-181b960d
  (fn strip [[h & t]]
    (cond
      (nil? h) ()
      :else
      (conj
        (strip (filter (partial not= h) t))
        h))))

(defcheck solution-1823d8bb
  (fn [coll]
    (reduce (fn [acc item]
              (if ((set acc) item)
                acc
                (conj acc item)))
      []
      coll)))

(defcheck solution-1841b606
  (fn [v]
    (reduce (fn [x y]
              (if (apply distinct? y x)
                (conj x y)
                x))
      []
      v)))

(defcheck solution-187b3098
  reduce #(if (not-any? #{%2} %1) (conj %1 %2) %1) [])

(defcheck solution-187c156a
  (partial
    reduce #(if (some (partial = %2) %1) %1 (conj %1 %2)) [] ))

(defcheck solution-189f0bb4
  #(loop [[x & xs] %, result []]
     (if x
       (recur xs (if (= (.indexOf result x) -1) (conj result x) result))
       result)))

(defcheck solution-1911ec1c
  (fn [a-seq]
    (loop [lseq a-seq found #{} acc []]
      (if (empty? lseq)
        acc
        (let [x (first lseq)
              new-acc (if (contains? found x)
                        acc
                        (conj acc x))]
          (recur (rest lseq) (conj found x) new-acc))))))

(defcheck solution-1926f6e6
  #(reduce (fn [r e] (if ((set r) e) r (conj r e))) [] %))

(defcheck solution-195c0f73
  (fn dst [s]
    (second (reduce (fn [[h-s r-v :as m] i]
                      (if
                       (contains? h-s i) m
                                         (assoc m
                                           0 (conj h-s i)
                                           1 (conj r-v i)))) [#{} []] s))))

(defcheck solution-1971770e
  (fn [s]
    (let [first-occur (reduce (fn [m [k i]] (if (m k) m (assoc m k i)))
                        {}
                        (map-indexed (comp reverse vector) s))]
      (sort (fn [& xy] (apply compare (map first-occur xy))) (keys first-occur)))))

(defcheck solution-19bf86d7
  reduce #(if ((set %) %2) % (conj % %2)) [])

(defcheck solution-19e6e9df
  (fn [n]
    (loop [cnt 0 acc [] s (set [])]
      (if (= (count n) cnt)
        acc
        (recur
          (inc cnt)
          (if (contains? s (nth n cnt)) acc (conj acc (nth n cnt)))
          (conj s (nth n cnt))
          )
        )
      )
    ))

(defcheck solution-1a6310be
  (fn [vs]
    (reduce (fn [acc v]
              (let [seen (set acc)]
                (if (seen v)
                  acc
                  (conj acc v))))
      []
      vs)))

(defcheck solution-1b1d7814
  (fn f[lst]
    (loop [ls lst res []]
      (let [x (first ls)
            c (some #{x} res)
            n (if c res (conj res x))]
        (if x
          (recur (rest ls) n)
          res)))))

(defcheck solution-1cfb08e0
  ;(comp second (partial reduce
  ;                      (fn [[s r] v] (if (s v) [s r] [(conj s v) (conj r v)]))
  ;                      [#{} []]))


  #(->> %
     (map-indexed list)
     (group-by last)
     (map (comp first second))
     (sort-by first)
     (map second)))

(defcheck solution-1d0c31a9
  (fn [s]
    (:seq
     (reduce
       #(if (contains? (:set %1) %2)
          %1
          (assoc %1
            :seq (conj (:seq %1) %2)
            :set (conj (:set %1) %2)))
       {:seq [], :set #{}}
       s))))

(defcheck solution-1df4e36d
  #(last (reduce (fn [[m r] v] (if (contains? m v) [m r] [(conj m v) (conj r v)])) [#{} []] %)))

(defcheck solution-1ef5a6db
  #(reduce (fn [r e] (if (some (fn [d] (= d e)) r) r (conj r e))) [] %))

(defcheck solution-1f1b0178
  (fn [coll]
    (loop [x coll res [(first coll)]]
      (if (empty? x) (butlast res)
                     (recur (filter #(not= % (last res)) x) (conj res (first (filter #(not= % (last res)) x))))))))

(defcheck solution-1f9f2ecc
  (fn [s]
    (loop [uniq-seq []
           seen-vals #{}
           unseen-seq s]
      (if (empty? unseen-seq)
        uniq-seq
        (let [elem (first unseen-seq)]
          (if (contains? seen-vals elem)
            (recur uniq-seq seen-vals (rest unseen-seq))
            (recur (conj uniq-seq elem) (conj seen-vals elem) (rest unseen-seq))))))))

(defcheck solution-1fb354d1
  (fn [l]
    (let [inside? (fn inside? [x l]
                    (if (empty? l)
                      false
                      (if (not= (first l) x)
                        (inside? x (next l))
                        true)))]
      (loop [[f & args :as my-l] l r (empty l)]
        (if (empty? my-l)
          r
          (if (inside? f r)
            (recur args r)
            (recur args (concat r (list f)))))))))

(defcheck solution-1fbaeaa
  (fn dist [invals]
    (loop [vals invals res [] acc #{}]
      (if (seq vals)
        (let [nextval (first vals)]
          (if (acc nextval)
            (recur (rest vals) res acc)
            (recur (rest vals) (into res [nextval]) (into acc [nextval]))))
        res))))

(defcheck solution-1ff689ca
  #(loop [[v & r] %
          accs #{}
          acc []]
     (let [new-acc (if (contains? accs v) acc (conj acc v))]
       (if (seq r)
         (recur r (conj accs v) new-acc)
         new-acc))))

(defcheck solution-202973af
  #(reduce
     (fn [r i]
       (if (some #{i} r)
         r
         (conj r i)))
     []
     %))

(defcheck solution-2072f02
  (fn [coll]
    (first (reduce (fn [[ordered-distinct seen] item]
                     (if (seen item)
                       [ordered-distinct seen]
                       [(conj ordered-distinct item) (conj seen item)]))
             [[] #{}]
             coll))))

(defcheck solution-2099083a
  (fn [seq]
    (loop [seen #{}
           seq seq
           accum []]
      (if (empty? seq)
        accum
        (let [[f & r] seq]
          (recur (conj seen f)
            r
            (if (contains? seen f)
              accum
              (conj accum f))))))))

(defcheck solution-20addf90
  (partial reduce (fn [a b] (if (some #{b} a) a (conj a b))) []))

(defcheck solution-21735b4a
  (fn [l]
    (loop [dist-els #{}
           ret []
           remaining l]
      #_(println dist-els)
      (let [[[h] t] (split-at 1 remaining)]
        (if-not (nil? h)
          (recur (conj dist-els h)
            (if ((comp not contains?) dist-els h) (conj ret h) ret)
            t)
          ret)))))

(defcheck solution-21a503e2
  (fn [xs]
    (reduce #(if (contains? (into #{} %1) %2) %1 (conj %1 %2)) [] xs)))

(defcheck solution-22b4d653
  (fn dist [coll]
    (sort-by (fn [i] (.indexOf coll i)) (keys (group-by identity coll)))))

(defcheck solution-23643687
  (fn f
    ([x] (f x []))
    ([ [x & xs] acc ]
     (if (and (nil? x) (nil? xs))
       acc
       (if (not-empty (filter #{x} acc))
         (f xs acc)
         (f xs (concat acc [x]))
         )
       )
     )
    ))

(defcheck solution-2377e088
  #(loop [f {}
          r %1
          c %1
          n 1]
     (cond
       (empty? c) r
       (contains? f (first c)) (recur f (concat (take (dec n) r) (drop  n r)) (rest c) n)
       :else (recur (assoc f (first c) true) r (rest c) (inc n)))))

(defcheck solution-23c69e6d
  #(first
     (reduce
       (fn [[v seen] x]
         (if (seen x)
           [v seen]
           [(conj v x) (conj seen x)]))
       [[] #{}]
       %
       )))

(defcheck solution-23d91072
  (fn [xs]
    (reduce #(let [n (conj %1 %2)]
               (if (apply distinct? n) n %1))
      []
      xs)))

(defcheck solution-23f43593
  (fn dis [xs]
    (reduce (fn [s e]
              (if (some #(= % e) s)
                s
                (conj s e)))
      [] xs)))

(defcheck solution-2420cd76
  (fn [coll]
    (letfn [(step [xs seen]
              (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen f)
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen f))))))
                 xs seen)))]
      (step coll #{}))))

(defcheck solution-242d8a0e
  (fn[l]
    (loop [remain l result [] tested #{}]
      (if (seq remain)
        (let [n (first remain)]
          (if (contains? tested n)
            (recur (rest remain) result tested)
            (recur (rest remain) (conj result n) (conj tested n))))
        result))))

(defcheck solution-2431da1e
  (fn [xs]
    (first
      (reduce
        (fn [[acc m] x]
          (if (contains? m x) [acc m]
                              [(conj acc x) (conj m x)]))
        [ [] #{} ]
        xs))))

(defcheck solution-2463ebf1
  reduce #(if (some (partial = %2) %) % (conj % %2)) [])

(defcheck solution-249f184a
  (fn [s] (reduce #(if ((set %) %2) %
                                    (conj % %2))
            []
            s)))

(defcheck solution-24cfc4fa
  (fn my-distinct [l]
    (loop [l l acc [] s #{}]
      (if (empty? l)
        acc
        (let [item (first l) b (contains? s item)]
          (recur
            (rest l)
            (if (false? b)
              (conj acc item)
              acc)
            (conj s item)))))))

(defcheck solution-2521d0a7
  #(reduce (fn [m v] (if-not (some #{v} m) (conj m v) m)) [] %))

(defcheck solution-25a6f52d
  (fn [s] (reduce (fn [c e] (if (some #(= e %) c) c (assoc c (count c) e))) [] s)))

(defcheck solution-26fb5591
  (fn
    [coll]
    (first (reduce (fn [[v s] el] (if (contains? s el)
                                    [v s]
                                    [(conj v el) (conj s el)]))
             [[] #{}]
             coll))))

(defcheck solution-27501b20
  (fn my-distinct [coll]
    (if (empty? coll)
      '()
      (cons (first coll) (my-distinct (remove #(= % (first coll)) coll))))))

(defcheck solution-27596fad
  (fn [l]
    (reduce
      (fn [a v]
        (if
         (some #(= % v) a)
          a
          (concat a (list v))
          ))
      '()
      l
      )))

(defcheck solution-275f1324
  (fn myDupes
    [s]
    (loop [result [(first s)]
           myS (filter #(not= (first s) %1) s)]
      (if (empty? myS)
        result
        (recur (conj result (first myS)) (filter #(not= (first myS) %1) myS))))))

(defcheck solution-282f11ae
  (fn [s] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] s)))

(defcheck solution-283330b1
  #(letfn [(distinct- [coll existed-values]
             (if (empty? coll)
               ()
               (if (existed-values (first coll))
                 (recur (rest coll) existed-values)
                 (lazy-seq (cons (first coll)
                             (distinct- (rest coll)
                               (conj existed-values (first coll))))))))]
     (distinct- % #{})))

(defcheck solution-285ac9e7
  (fn my-distinct [coll]
    (reduce (fn [accum x]
              (if ((set accum) x) accum (conj accum x)))
      []
      coll)))

(defcheck solution-285d220
  (fn foo
    ([coll] (foo coll #{}))
    ([coll seen]
     (let [r (drop-while seen coll) f (first r)]
       (if (nil? f) ()
                    (cons f (lazy-seq
                              (foo (rest r) (conj seen f)))))))))

(defcheck solution-28b892f0
  #(->> (zipmap (range) %)
     (group-by val)
     (sort-by (fn [[_ a]] (apply min (map first a))))
     (map first)))

(defcheck solution-28bde8ce
  #(loop [[e & l] %1
          a []]
     (if l
       (recur (filter (partial not= e) l) (conj a e))
       (if e
         (conj a e)
         a))))

(defcheck solution-28f72355
  (fn dist [x]
    (letfn [(almeti [x y] (if (some #(= y %) x) x (conj x y)))]
      (reduce almeti [] x))))

(defcheck solution-29b497fc
  (fn my-distinct
    ([lis] (my-distinct lis []))
    ([lis resp]
     (if (= lis [])
       resp
       (if (some (hash-set (first lis)) resp)
         (my-distinct (rest lis) resp)
         (my-distinct (rest lis) (conj resp (first lis))))))))

(defcheck solution-29da3138
  reduce (fn [s e]
           (if (some #(= % e) s)
             s
             (conj s e))) [])

(defcheck solution-2aba9adf
  (fn [xs]
    (loop [xs' xs result []]
      (if (empty? xs') result
                       (recur (remove #(= (first xs') %) xs') (conj result (first xs')))))))

(defcheck solution-2ad36bd1
  ;(fn my-distinct [coll]
  ;   (lazy-seq (if (seq coll) (cons (first coll)
  ;                                 (my-distinct (filter #(not= (first coll) %) (rest coll))) ))))

  (fn m-distinct [coll]
    (loop [[hd & tl :as coll] coll res [] added #{} ]
      (if (seq coll)
        (if (contains? added hd)
          (recur tl res added)
          (recur tl (conj res hd) (conj added hd)))
        res))))

(defcheck solution-2b64bc1
  (fn f [[a & z]]
    (when a
      (lazy-seq (cons a (f (remove #(= a %) z)))))))

(defcheck solution-2b9c2e6a
  (fn [xs]
    (second (reduce (fn [[s v] x] (if (s x) [s v] [(conj s x) (conj v x)])) [#{} []] xs))))

(defcheck solution-2bc18854
  #(loop [v %1 o []]
     (if (nil? v)
       o
       (if (some #{(first v)} o)
         (recur (next v) o)
         (recur (next v) (conj o (first v)))
         )
       )
     ))

(defcheck solution-2c1308f5
  #(loop [[h & t] %, has #{}, acc []]
     (cond (nil? h) acc
           (has h)  (recur t has acc)
           :else    (recur t (conj has h) (conj acc h)))))

(defcheck solution-2cb73649
  (fn f[x & y]
    (if (empty? x)
      y
      (recur (rest x)
        (if (some #(= (first x) %) y)
          y
          (concat y (list (first x))))))))

(defcheck solution-2ce9d4f5
  (fn [coll]
    (:res
     (reduce (fn [acc x]
               (if ((:filter acc) x)
                 acc
                 (-> acc
                   (update-in [:filter] conj x)
                   (update-in [:res] conj x))))
       {:res [] :filter #{}}
       coll))))

(defcheck solution-2d0c4e0a
  (fn [c] (reduce #(if (seq (filter (fn [a] (= a %2)) %)) % (conj % %2)) [] c)))

(defcheck solution-2d1cd532
  #(reduce (fn [a b] (if (contains? (set a) b) a (conj a b))) [] %))

(defcheck solution-2d224013
  (fn [s]
    (let [r (fn [c e] (if (not (some #(= % e) c)) (conj c e) c))]
      (reverse (reduce r '() s)))))

(defcheck solution-2d8127
  (fn [s] (map first
            (sort-by second
              (for [[v ps] (group-by second (map-indexed vector s))]
                [v (apply min (map first ps))])))))

(defcheck solution-2dd54134
  (fn [coll]
    (last (reduce (fn [[seen xs :as sxs] x]
                    (if-not (seen x)
                      (map #(conj % x) sxs)
                      sxs))
            [#{} []] coll))))

(defcheck solution-2de787ba
  (fn [l]
    (reduce #(if (some (fn [e] (= e %2)) %)
               %
               (conj % %2))
      [] l)))

(defcheck solution-2e5178f8
  (fn nub
    [lst]
    (cond
      (empty? lst) ()
      :else (cons (first lst) (nub (filter (fn [arg] (not= (first lst) arg)) (rest lst)))))))

(defcheck solution-2e97bf01
  ;(fn dedupe [s]
  ;  (loop [in s
  ;         out []
  ;         seen #{}]
  ;    (if-let [head (first in)]
  ;      (if (seen head)
  ;        (recur (rest in) out seen)
  ;        (recur (rest in) (conj out head) (conj seen head)))
  ;      out)))
  reduce #(if ((set %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-2e9c813b
  (partial
    reduce
    (fn [s, x]
      (if (some (partial = x) s)
        s
        (concat s (list x))))
    '()))

(defcheck solution-2ec97bdd
  (fn [coll]
    (first
      (reduce (fn [[out s] x]
                (if (s x)
                  [out s]
                  [(conj out x) (conj s x)]))
        [[] #{}]
        coll))))

(defcheck solution-2ed142b3
  #(loop [result [] seen? #{} xs %]
     (cond
       (empty? xs) result
       (seen? (first xs))
       (recur result seen? (rest xs))
       :else (recur (conj result (first xs))
               (conj seen? (first xs))
               (rest xs)))))

(defcheck solution-2ed50d9a
  #(loop [[f & r] % s #{} a []]
     (if f (recur r (conj s f) (if (s f) a (conj a f))) a)))

(defcheck solution-2ee852f7
  (fn my-distinct [c]
    (loop [result [] rest c]
      (if (= [] rest)
        result
        (recur
          (if (some #(= % (first rest)) result) result (conj result (first rest)))
          (drop 1 rest))))))

(defcheck solution-2f2bd6ff
  #((fn i [s seen]
      (if (seq s)
        (let [item (first s)]
          (if (seen item)
            (i (rest s) seen)
            (cons item (i (rest s) (conj seen item)))))
        s)
      ) % #{}))

(defcheck solution-2f44b25
  (fn dist
    [coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))))

(defcheck solution-2fcfb41a
  (fn dist
    ([coll visited res]
     (if (empty? coll) res
                       (let [current (first coll)]
                         (dist (rest coll) (set (cons current visited))
                           (if (visited current) res (concat res [current]))))))
    ([coll] (dist coll #{} []))))

(defcheck solution-2fef7cf
  (fn [s]
    (loop [in s
           out []
           seen #{}]
      (if (seq in)
        (let [[a & bs] in]
          (if (seen a)
            (recur bs out seen)
            (recur bs (conj out a) (conj seen a))))
        out))))

(defcheck solution-2ff0def0
  #(reverse(reduce
             (fn [s n]
               (if (not-any? (fn [m] (= n m)) s)
                 (conj s n)
                 s))
             '()
             %)))

(defcheck solution-3001fe97
  (fn [s]
    (loop [known #{} suite [] c s]
      (let [[f & restt] c]
        (if f
          (if (known f)
            (recur known suite restt)
            (recur (conj known f) (conj suite f) restt))
          suite)))
    ))

(defcheck solution-30a8f3a7
  (fn [s]
    (letfn
     [(append-if-uniq [s e] (if (contains? (set s) e)
                              s
                              (concat s (list e))))]
      (reduce append-if-uniq '() s))))

(defcheck solution-313094c3
  (comp keys (partial sort-by (comp first first last)) (partial group-by last) (partial map-indexed list)))

(defcheck solution-314854dd
  (fn [seq]
    (loop [seq seq
           result-set #{}
           result-vector []]
      (if (empty? seq)
        result-vector
        (let [[head & tail] seq]
          (if (contains? result-set head)
            (recur tail result-set result-vector)
            (recur tail
              (conj result-set head)
              (conj result-vector head))))))))

(defcheck solution-31aed972
  #(reduce (fn [acc x]
             (if (some #{x} acc)
               acc
               (concat acc (list x))))
     ()
     %))

(defcheck solution-31fb41b1
  (fn [l]
    (loop [news (list (first l)) tmpl (rest l)]
      (if (empty? tmpl)
        (reverse news)
        (if (some (partial = (first tmpl)) news)
          (recur news (rest tmpl))
          (recur (conj news (first tmpl)) (rest tmpl)))))))

(defcheck solution-3222e3a
  (fn [l]
    (letfn
     [(inf
        [s l]
        (if (empty? l) []
                       (let [fl (first l) rr (inf (conj s fl) (rest l))]
                         (if (contains? s fl) rr (concat [fl] rr))
                         )))]
      (inf #{} l)
      )))

(defcheck solution-322f453d
  (fn [s] (reduce (fn [acc it] (if (nil? (some #{it} acc)) (conj acc it) acc)) [] s)))

(defcheck solution-3275fb7d
  (fn sieve [lst]
    (if-not (empty? lst)
      (cons (first lst)
        (lazy-seq (sieve (filter #(not= (first lst) %) (rest lst))))
        ))
    ))

(defcheck solution-3291edf1
  (fn [s]
    (loop [res []
           todo s
           seen #{}]
      (if (empty? todo)
        res
        (if (contains? seen (first todo))
          (recur res (next todo) seen)
          (recur (conj res (first todo)) (next todo) (conj seen (first todo))))))))

(defcheck solution-32a9d8c8
  (fn [coll]
    (first (reduce (fn [[out dup] x] (if (dup x) [out dup] [(conj out x) (conj dup x)]))
             [[] #{}]
             coll))))

(defcheck solution-32abb2d9
  #(loop [input % result []]
     (if (empty? input)
       result
       (if (not (nil? (some #{(first input)} result)))
         (recur (rest input) result)
         (recur (rest input) (conj result (first input)))))))

(defcheck solution-32f465b8
  (fn _distinct
    ([x] (_distinct x #{} []))
    ([x prev acc]
     (if (empty? x)
       acc
       (if (contains? prev (first x))
         (recur (rest x) prev acc)
         (recur (rest x) (conj prev (first x)) (conj acc (first x))))))))

(defcheck solution-330821be
  #(loop [seen #{} acc [] coll %]
     (if-let [[a & coll] coll]
       (if (seen a)
         (recur seen acc coll)
         (recur (conj seen a) (conj acc a) coll))
       acc)))

(defcheck solution-33e7067c
  (fn [coll]
    (loop [results [] items coll]
      (cond
        (empty? items) results
        (some (partial = (first items)) results) (recur results (rest items))
        :else
        (recur (conj results (first items)) (rest items))
        )
      )
    ))

(defcheck solution-33f10caa
  (fn  [coll]
    (reduce (fn [xs x]
              (if (nil? (some #{x} xs))
                (conj xs x)
                xs))
      [] coll)))

(defcheck solution-33f76b0d
  #(loop [i % r []]
     (if (= [] i)
       r
       (recur (rest i) (if ((into #{} r) (nth i 0)) r (conj r (nth i 0) ))))))

(defcheck solution-3402da52
  (fn dist [s]
    (reduce #(if (some (partial = %2) %1) %1 (conj %1 %2)) [] s)))

(defcheck solution-3418153b
  (fn f [s]
    (if (= 1 (count s))
      [(first s)]
      (let [prev (f (butlast s))]
        (if (some #(= (last s) %) prev)
          prev
          (conj prev (last s)))))))

(defcheck solution-34278696
  (fn U [s]
    (if (empty? s) s
                   (cons (first s) (U (remove #(= (first s) %) (rest s)))))))

(defcheck solution-3483cd13
  (fn [col] (reduce #(if (< (.indexOf %1 %2) 0) (conj %1 %2) %1) [] col)))

(defcheck solution-354ca728
  (fn [coll]
    (loop [x coll
           r []]
      (if (nil? (first x))
        r
        (if (some #(= % (first x)) r)
          (recur (rest x) r)
          (recur (rest x) (concat r (vector (first x)))))))))

(defcheck solution-3569a45e
  #(last (reduce (fn [[s r] x]
                   (if (s x)
                     [s r]
                     [(conj s x)
                      (conj r x)]))
           [#{} []]
           %)))

(defcheck solution-35896249
  (fn unique [col]
    (reduce (fn [init elem]
              (if (some #(= elem %) init)
                init
                (conj init elem))) [] col)))

(defcheck solution-35a06531
  (fn [l]
    (->> (interleave (reverse l) (reverse (range (count l))))
      (apply sorted-map)
      (seq)
      (sort-by second)
      (map first))))

(defcheck solution-35af5582
  #(loop [s #{}
          a []
          c %]
     (if (empty? c)
       a
       (let [[h & t] c]
         (if (s h)
           (recur s a t)
           (recur (conj s h) (conj a h) t))))))

(defcheck solution-36011083
  (fn [seq]
    (reduce (fn [acc val]
              (if (some #(= val %) acc)
                acc
                (conj acc val))) [] seq)))

(defcheck solution-362ff404
  (fn distinct-e
    [x]
    (if (empty? x) '()
                   (conj (distinct-e (filter #(not (= % (first x))) (rest x))) (first x)))))

(defcheck solution-3736eb59
  #(first (reduce (fn [[r s] e]
                    (if (contains? s e) [r s] [(conj r e) (conj s e)]))
            [[] #{}] %)))

(defcheck solution-374420e7
  (fn [xs]
    (loop [output [] s #{} xs xs]
      (if (seq xs)
        (if (s (first xs))
          (recur output s (rest xs))
          (recur (conj output (first xs)) (conj s (first xs)) (rest xs)))
        output))))

(defcheck solution-37b4f3d8
  (fn [coll] (sort-by (fn [i] (.indexOf coll i)) (map first (group-by identity coll)))))

(defcheck solution-37ec61eb
  reduce (fn [s e] (if (some #(= % e) s) s (conj s e))) [])

(defcheck solution-37faf221
  (fn [xs] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] xs)))

(defcheck solution-38068f33
  (fn [v] (reduce #(if ((set %) %2) % (conj % %2)) [] v)))

(defcheck solution-382039c9
  (fn d [xs]
    (loop [acc [] [h & t] xs]
      (cond
        (nil? h) acc
        (some #{h} acc) (recur acc t)
        :else (recur (conj acc h) t)))))

(defcheck solution-384549b8
  #(letfn [(d [l seen]
             (if (seq l)
               (let [f (first l) r (rest l)]
                 (if (seen f) (d r seen) (cons f (d r (conj seen f)))))))]
     (d % #{})))

(defcheck solution-387088d
  (fn my-distinct
    ([coll] (my-distinct coll #{}))
    ([coll s]
     (cond
       (empty? coll)
       []
       (contains? s (first coll))
       (my-distinct (rest coll) s)
       :else
       (cons (first coll)
         (my-distinct (rest coll) (conj s (first coll))))))))

(defcheck solution-38748724
  (fn [v]
    (reduce
      #(if-not (contains? (into #{} %1) %2) (conj %1 %2) %1)
      [] v)))

(defcheck solution-387df14f
  (fn [c] (second (reduce #(if ((first %) %2) % [(conj (first %) %2) (conj (second %) %2)]) [#{} []] c))))

(defcheck solution-389cf29
  #(second (reduce (fn [[c r] i] (if (some #{i} c) [c r] [(conj c i) (conj r i)])) [#{} []] %)))

(defcheck solution-38a7f9bf
  reduce #(if (not-any? (partial = %2) %1) (conj %1 %2) %1) [])

(defcheck solution-393f4614
  ;(fn [s]
  ;  (loop [l s, acc []]
  ;    (if-let [h (first l)]
  ;      (recur (remove #(= h %) l) (conj acc h))
  ;      acc)))
  (fn [s] (reduce #(if (some #{%2} %) % (conj % %2)) [] s)))

(defcheck solution-399296ce
  (fn [coll] (seq (reduce #(if (not (contains? (set %1) %2)) (conj %1 %2) %1) [] coll))))

(defcheck solution-3992c163
  reduce #(if (some #{%2} %1) %1 (concat %1 [%2])) [])

(defcheck solution-3a2070ae
  (fn [xs]
    (second (reduce (fn [[bl xs] x] (if (bl x)
                                      [bl xs]
                                      [(conj bl x) (conj xs x)]))
              [#{} []] xs))))

(defcheck solution-3a76f799
  #(loop [r [] c % seen #{}]
     (if (empty? c)
       r
       (if (seen (first c))
         (recur r (rest c) seen)
         (recur (conj r (first c)) (rest c) (conj seen (first c)))))))

(defcheck solution-3ab1ae3c
  (fn [coll]
    (reverse
      (loop [x coll
             y '()
             seen? #{}]
        (cond
          (empty? x) y
          (seen? (first x)) (recur (rest x) y seen?)
          :else  (recur (rest x) (cons (first x) y) (conj seen? (first x))))))))

(defcheck solution-3b510c5d
  (fn [col] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] col)))

(defcheck solution-3b511809
  (fn [coll]
    (reduce (fn [ret x]
              (if (some #(= x %) ret)
                ret
                (conj ret x))
              ) [] coll)
    ))

(defcheck solution-3b5188a8
  (fn [coll]
    (loop [dist [], seen #{}, rem coll]
      (let [item (first rem)]
        (if (empty? rem)
          dist
          (recur
            (if (contains? seen item)
              dist
              (conj dist item))
            (conj seen item)
            (rest rem)))))))

(defcheck solution-3b59aeca
  reduce (fn [c e]
           (if (some #(= % e) c)
             c
             (conj c e))) [])

(defcheck solution-3bb6109e
  (comp second (partial reduce (fn [acc,e] (if ((first acc) e) acc (map #(conj % e) acc))) [#{} []])))

(defcheck solution-3bf0592
  (fn [coll]
    (first (reduce (fn [[res uniq] x]
                     (if (uniq x)
                       [res uniq]
                       [(conj res x) (conj uniq x)]))
             [[] #{}] coll))))

(defcheck solution-3c051a17
  (fn [coll]
    (letfn [(add [c x]
              (if (= x (some #{x} c))
                c
                (cons x c)))]
      (reverse (reduce add [] coll)))))

(defcheck solution-3c1c3ceb
  (fn [col]
    (loop [c col
           used #{}
           result []]
      (if-let [x (first c)]
        (recur (rest c)
          (conj used x)
          (if (used x)
            result
            (conj result x)))
        result))))

(defcheck solution-3c280441
  (fn [c]
    (take-while #(not (nil? %))
      ((fn distinct_ [col] (lazy-cat [(first col)] (distinct_ (filter #(not= % (first col)) (rest col))))) c))))

(defcheck solution-3c7e0101
  (fn [xs]
    (reduce
      (fn [coll now]
        (if
         (some #{now} coll)
          coll
          (conj coll now)))
      [] xs)))

(defcheck solution-3c941b5e
  (fn [coll]
    (letfn [(appear-before? [n]
              (let [item (nth coll n)]
                (some #(= item %) (take n coll))))]
      (map #(nth coll %)
        (filter #(not (appear-before? %)) (range (count coll)))))))

(defcheck solution-3ce8c3cb
  (fn [col]
    (reduce
      #(if (some (fn [a] (= a %2)) %1 )
         %1
         (concat %1 (conj (empty %1) %2))
         )
      (empty col)
      col
      )
    ))

(defcheck solution-3ceeb52b
  #(first (reduce (fn [[l s] v](if (s v) [l s] [(conj l v) (conj s v)]))[[] #{}] %)))

(defcheck solution-3d18f12b
  (fn [x] (reverse(last(reduce (fn [[d a] v] (if (d v) [d a] [(set (cons v d)) (cons v a)])) [#{} '()] x)))))

(defcheck solution-3d4a1d52
  (fn [coll]
    ((fn iter [coll seen]
       (if (seq coll)
         (let [item (first coll)
               seen (conj seen item)]
           (cons item
             (lazy-seq (iter (drop-while seen (rest coll))
                         seen))))
         nil)
       ) coll #{})))

(defcheck solution-3da821de
  #(loop [[head & tail] %
          used #{}
          acc []]
     (if (nil? head)
       acc
       (recur tail
         (conj used head)
         (if (contains? used head)
           acc
           (conj acc head))))))

(defcheck solution-3e2dff76
  (fn [x]
    ((fn fdi [x s]
       (if (empty? x)
         '()
         (if (not (nil? (s (first x))))
           (cons (first x) (fdi (rest x) (disj s (first x))))
           (fdi (rest x) s)
           )
         )
       )
     x (set x)
     )
    ))

(defcheck solution-3e58bf70
  (fn [s1]
    (second
      (reduce
        (fn [[acc res] item]
          (if (contains? acc item)
            [acc res]
            [(conj acc item) (conj res item)]
            )
          )
        [#{} []]
        s1))))

(defcheck solution-3e5adfb3
  (fn [v]
    (loop [in v
           out []]
      (if (empty? in)
        out
        (let [t (first in)]
          (recur (rest in) (into out (if (contains? (set out) t) nil (vector t)))))))))

(defcheck solution-3e61d4b9
  reduce (fn [coll e]
           (if (some #(= e %) coll)
             coll
             (conj coll e))) [])

(defcheck solution-3e8aa2e8
  (partial
    (fn f [d s]
      (let [v (first s)]
        (if-let [vs (next s)]
          (if (d v)
            (recur d vs)
            (cons v (lazy-seq (f (conj d v) vs))))
          (when-not (d v) s)))) ; my handling of the tail end is so ugly
    #{}))

(defcheck solution-3ed185e7
  reduce #(if ((set %1) %2)  %1 (conj %1 %2)) [])

(defcheck solution-3f0f48c9
  (fn [s]
    (loop [ret []
           s s
           seen #{}]
      (if-not (seq s)
        ret
        (recur (if-not (seen (first s)) (conj ret (first s)) ret)
          (rest s)
          (conj seen (first s)))))))

(defcheck solution-3f17f047
  (fn [coll]
    (let [aux
          (fn step [xs seen]
            (lazy-seq
              ((fn [[x :as xs] seen]
                 (when-let [s (seq xs)]
                   (if (contains? seen x)
                     (recur (rest s) seen)
                     (cons x (step (rest s) (conj seen x))))))
               xs seen)))]
      (aux coll #{}))))

(defcheck solution-403344a5
  (fn [coll]
    (reduce
      (fn [vec e] (if (some #(= % e) vec)
                    vec
                    (conj vec e)))
      []
      coll)))

(defcheck solution-4046cacf
  (fn [x] (reduce (fn [t v] (if (some #(= v %) t) t (conj t v))) [] x)))

(defcheck solution-40493791
  reduce (fn [x y]
           (if (some #(= y %) x)
             x
             (conj x y))) [])

(defcheck solution-404a06d4
  (fn [y] (loop [x      #{}
                 result  []
                 z        0]
            (if (>= z (count y))
              result
              (if-not (x (nth y z))
                (recur (conj x (nth y z)) (conj result (nth y z)) (inc z))
                (recur x result (inc z)))))))

(defcheck solution-40668f85
  (fn [s]
    (reduce (fn [a e]
              (if (some (partial = e) a)
                a
                (conj a e)))
      []
      s)))

(defcheck solution-40cab97d
  (fn [coll] (reduce (fn [m x] (if ((set m) x) m (conj m x))) [] coll)))

(defcheck solution-411e10b1
  (fn [s] (remove nil? (map #(if (%2 %1) nil %1) s (reductions conj #{} s)))))

(defcheck solution-412d53d1
  (fn [xs]
    (first (reduce (fn [[items found] item] [(if (contains? found item) items (conj items item)) (conj found item)]) [[] #{}] xs))))

(defcheck solution-41a63245
  (fn find-distinct [s]
    ((fn helper [e [f & r :as s1] ]
       (if (empty? s1) '()
                       (if (contains? e f)
                         (recur e r)
                         (cons f (lazy-seq (helper (conj e f) r)))
                         ))) #{} s)))

(defcheck solution-42390dbf
  (fn dist [col]
    (reduce (fn [xs y] (if (some #{y} xs) xs (conj xs y))) [] col)))

(defcheck solution-42a8ac4c
  (fn [coll]
    ((reduce (fn [r v]
               (if ((r 0) v)
                 r
                 [(conj (r 0) v) (conj (r 1) v)]))
       [#{} []]
       coll) 1)))

(defcheck solution-42aa9fde
  reduce #({%2 %} ((set %) %2) (conj % %2)) [])

(defcheck solution-42b49269
  reduce (fn [coll x] (if (some #(= % x) coll) coll (conj coll x))) [])

(defcheck solution-42d07e14
  (fn [coll]
    (reduce
      #(if (some (partial = %2) %) % (conj % %2)) []
      coll)))

(defcheck solution-42d2d493
  #(loop [s #{} r % ret []]
     (if (empty? r)
       ret
       (if (contains? s (first r))
         (recur s (rest r) ret)
         (recur (conj s (first r)) (rest r) (conj ret (first r)))))))

(defcheck solution-42f21d2a
  (fn [xs]
    (loop [acc []
           src xs]
      (if-not (empty? src)
        (if (contains? (into #{} acc) (first src))
          (recur acc (rest src))
          (recur (conj acc (first src)) (rest src)))
        (into [] acc)))))

(defcheck solution-43172c9a
  (fn dist
    [s]
    (let [move (fn move [used s]
                 (when (seq s)
                   (let [v (first s)]
                     (if (used v)
                       (recur used (next s))
                       (cons v (move (into used #{v}) (next s)))))))]
      (move #{} s))))

(defcheck solution-43607eae
  (fn [c]
    (into (empty c) (first
                      (reduce
                        (fn [[c seen] e]
                          (if (seen e)
                            [c seen]
                            [(conj c e) (conj seen e)]))
                        [(empty c) #{}]
                        c)))
    ))

(defcheck solution-43635b16
  (fn [coll]
    (reduce (fn [a b] (if (some #(= % b) a) a (conj a b))) [] coll)))

(defcheck solution-44d6f18c
  (fn [c]
    (let [rfl (fn [l] (remove #(= (first l) %) l))]
      (loop [res [] left c]
        (if (empty? left)
          res
          (recur (conj res (first left)) (rfl left)))))))

(defcheck solution-4524992b
  (fn [c] (reduce #(if (some (set %) #{%2}) %
                                            (conj % %2)) [] c)))

(defcheck solution-4548cfe1
  (fn [s]
    (first
      (reduce
        (fn [[ret found] i]
          (let [ch (if (found i) identity #(conj % i))]
            [(ch ret) (ch found)]))
        [[] #{}]
        s))))

(defcheck solution-45aacce7
  #(reduce (fn [acc x] (if (some #{x} acc)
                         acc
                         (conj acc x)))
     []
     %))

(defcheck solution-45f0d5b4
  (fn
    [coll]
    (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] coll)))

(defcheck solution-4648855e
  (fn dist [coll]
    (if (empty? coll) ()
                      (cons (first coll) (dist (filter #(not= (first coll) %) (rest coll)))))))

(defcheck solution-46864b48
  (fn cool [x]
    (letfn [(cool2 [y z]
              (if (empty? y)
                (reverse z)
                (if (some #{(first y)} z)
                  (cool2 (rest y) z)
                  (cool2 (rest y) (cons (first y) z))
                  )
                )
              )]
      (cool2 x '())
      )
    ))

(defcheck solution-46bac36d
  (fn [s]
    (loop [s s
           accum []]
      (if (empty? s)
        accum
        (if (some #(= (first s) %) accum)
          (recur (rest s) accum)
          (recur (rest s) (conj accum (first s))))))))

(defcheck solution-46d932af
  (fn
    [a]
    (loop [acc []
           curr (first a)
           remaining (rest a)]
      (if (= nil curr)
        acc
        (recur
          (if (some #{curr} acc)
            acc
            (merge acc curr))
          (first remaining)
          (rest remaining))))))

(defcheck solution-46e18814
  #(first (reduce (fn[[acc s] i] (if (contains? s i) [acc s] [(conj acc i) (conj s i)])) [[] #{}] %)))

(defcheck solution-46f9db3a
  reduce #(
            if (
                 some (fn [x] (= x %2)) %1)
            %1
            (conj %1 %2)
            ) [])

(defcheck solution-471d221c
  reduce (fn [acc x] (if (contains? (set acc) x) acc (conj acc x))) [])

(defcheck solution-47257dce
  (fn my-distinct
    ([x] (my-distinct x [] #{}))
    ([x acc distinct-set]
     (if (empty? x)
       acc
       (let [first-x (first x)]
         (recur (next x) (if (contains? distinct-set first-x) acc (conj acc first-x)) (conj distinct-set first-x)) )))))

(defcheck solution-47664512
  (fn uni [collect]
    (loop [col collect,rec #{},result []]
      (if (empty? col)
        result
        (let [
              last (rest col),
              x (first col),
              sign (contains? rec x)
              ]
          (if (true? sign)
            (recur last rec result)
            (recur last (conj rec x) (conj result x) )
            )
          )
        )
      )
    ))

(defcheck solution-483c824e
  #(loop [[f & r] %1 s #{} res (empty %1)]
     (cond (nil? f) res
           (get s f) (recur r s res)
           :else (recur r (conj s f) (concat res (list f))))))

(defcheck solution-483f66b1
  (fn _distinct [items]
    ((fn dh [result remaining]
       (if (empty? remaining)
         result
         (if ((set result) (first remaining))
           (dh result (rest remaining))
           (dh (concat result (take 1 remaining)) (rest remaining)))))
     '() items)))

(defcheck solution-4857f1b0
  reduce (fn [x c] (concat x (if (some #{c} x) [] [c]))) [])

(defcheck solution-48c23ba1
  (fn distinct--group [coll]
    (->> coll                 ; [:a :b :c :b]
      (map-indexed vector) ; ([0 :a] [1 :b] [2 :c] [3 :b])
      (group-by second)    ; {:a [[0 :a]], :b [[1 :b] [3 :b]], :c [[2 :c]]}
      (map (juxt first (comp ffirst second))) ; ([:b 1] [:a 0] [:c 2])
      (sort-by second)     ; ([:a 0] [:b 1] [:c 2])
      (map first))))

(defcheck solution-48d1cadf
  (fn myDistinct
    [coll]
    (reduce (fn[vect val] (if (some #(= val %) vect) vect (conj vect val))) [] coll)))

(defcheck solution-48f0a3bd
  (fn distinct2
    ([coll] (distinct2 coll []))
    ([coll s]
     (if (empty? coll)
       s
       (if ((set s) (first coll))
         (distinct2 (rest coll) s)
         (distinct2 (rest coll) (conj s (first coll))))))))

(defcheck solution-491a83bd
  (fn [coll]
    (loop [s coll
           result []]
      (if (empty? s)
        (if (list? result) (reverse result) result)
        (let [f (first s)
              is-in-coll? (if (some #{f} result) true false)]
          (recur (rest s) (if is-in-coll? result (conj result f))))))))

(defcheck solution-49420ba6
  (fn [s]
    (reduce (fn [v x] (if (some {x true} v) v (conj v x))) [] s)))

(defcheck solution-49aa523b
  #(loop [[s & r] % q []]
     (let [z (if (some #{s} q) q (conj q s))]
       (if r (recur r z) z))))

(defcheck solution-4b1f8698
  (fn [lst]
    (reduce
      #(if (some #{%2} %1) %1 (conj %1 %2))
      []
      lst)))

(defcheck solution-4ba5c31
  #(first (reduce (fn [[v s] x] [(if (s x) v (conj v x)) (conj s x)]) [[] #{}] %)))

(defcheck solution-4beac440
  (fn dist
    ([s] (dist s #{} []))
    ([s dups rez] (if (empty? s)
                    rez
                    (if (contains? dups (first s))
                      (dist (rest s) dups rez)
                      (dist (rest s) (conj dups (first s)) (conj rez (first s))))))))

(defcheck solution-4c292e26
  #(first (reduce (fn [[s t] x]
                    [(if (t x) s (conj s x)) (conj t x)])
            [[] #{}] %)))

(defcheck solution-4c5061a
  (fn [l]
    (first (reduce (fn [[redc seen] x]
                     (if (contains? seen x) [redc seen] [(concat redc [x]) (conj seen x)]))
             [[] #{}] l)

      )))

(defcheck solution-4c5657d7
  (fn [sq]
    (letfn [(helper [rv sqset sq] (if (empty? sq) rv (if (contains? sqset (first sq))
                                                       (helper rv sqset (rest sq))
                                                       (helper (cons (first sq) rv) (conj sqset (first sq)) (rest sq)))))
            ] (reverse (helper '() #{} sq)))))

(defcheck solution-4c6c18ab
  (fn my-distinct [coll]
    (loop [res [] c coll]
      (if (empty? c)
        res
        (recur
          (if ((set res) (first c)) res
                                    (conj res (first c)))
          (rest c))))))

(defcheck solution-4d788638
  (fn myDistinct [coll] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] coll)))

(defcheck solution-4d887943
  reduce #(if ((set %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-4e13092c
  (fn [coll]
    ((fn step [[x & xs] seen]
       (when x
         (if (seen x)
           (step xs seen)
           (cons x (step xs (conj seen x))))))
     coll #{})))

(defcheck solution-4e6af1c2
  (fn dist [col]
    (first (reduce #(if ((last %1) %2)
                      %1
                      [(conj (first %1) %2) (set (cons %2 (last %1)))])
             [[] #{}]
             col))))

(defcheck solution-4ea7e333
  (fn[s](
          filter (fn[x](not(nil? x))) (map-indexed
                                        (fn[idx itm](
                                                      if(contains? (set(take idx s)) itm) nil itm

                                                                                          ))
                                        s)
          )))

(defcheck solution-4ebaa66b
  (fn [s]
    (loop [[h & t :as src] s chk #{} dst []]
      (if src
        (recur t (conj chk h) (if (contains? chk h) dst (conj dst h)))
        dst))))

(defcheck solution-4f9a93a3
  #(first (reduce (fn [[c s] x]
                    (if (not (s x))
                      [(conj c x) (conj s x)]
                      [c s]))
            [[] #{}]
            %)))

(defcheck solution-4fe24665
  #(loop [[f & r] % fltr #{f} res [f]]
     (if f
       (recur r (conj fltr f) (if (fltr f) res (conj res f)))
       res)))

(defcheck solution-4ffa3525
  (fn dist [x]
    (loop [a #{}
           b []
           r x]
      (cond
        (empty? r) b
        (a (first r)) (recur a b (rest r))
        :else (recur (conj a (first r))
                (conj b (first r))
                (rest r))

        ))))

(defcheck solution-50a4d39a
  (fn dist
    ([s] (dist #{} [] s))
    ([h ds s] (if
               (empty? s)
                ds
                (let
                 [i (first s)
                  r (rest s)]
                  (if
                   (contains? h i)
                    (dist h ds r)
                    (dist (conj h i) (conj ds i) r)))))))

(defcheck solution-50cdcf77
  reduce (fn [accum elem]
           (if ((set accum) elem)
             accum
             (conj accum elem))) [])

(defcheck solution-50ec465a
  (fn [coll]
    (first (reduce (fn [[v s] n] (if (s n) [v s] [(conj v n) (conj s n)])) [[] #{}] coll))))

(defcheck solution-51c7e10
  (fn distinct-clone [coll]
    (reduce (fn [distincts x]
              (if ((set distincts) x) distincts (conj distincts x))) [] coll)))

(defcheck solution-5207322c
  (fn [coll]
    (first
      (reduce
        (fn [[result seen] elem]
          (if (seen elem)
            [result seen]
            [(conj result elem) (conj seen elem)]))
        [[] #{}] coll))))

(defcheck solution-520ec4db
  (fn dist [s]
    (loop [s s has #{} res []]
      (if (not (seq s))
        res
        (let [hd (first s)]
          (if (has hd)
            (recur (rest s) has res)
            (recur (rest s) (conj has hd) (conj res hd))))))))

(defcheck solution-521d7166
  (fn [coll]
    (let [freqs-map (reduce (fn [m k] (update-in m [k] #(or % (count m)))) (sorted-map) coll)
          invs-freq-map (apply sorted-map (interleave (vals freqs-map) (keys freqs-map)))]
      (vals invs-freq-map))))

(defcheck solution-5256be07
  (fn [s]
    (first
      (reduce
        #(let [[result seen] %1]
           (if (seen %2)
             [result seen]
             [(conj result %2) (conj seen %2)]))
        [[] #{}]
        s))))

(defcheck solution-530340a6
  #(reduce (fn [r n]
             (if (some #{n} r) r (conj r n))) [] %))

(defcheck solution-537dee59
  (fn outer [xs]
    ((fn inner [done left]
       (if (empty? left)
         done
         (let [n (first left)]
           (if (some #{n} done)
             (inner done (rest left))
             (inner (concat done [n]) (rest left))))))
     () xs)))

(defcheck solution-538f821c
  (fn [s] (reduce #(if (some #{%2} %) % (conj % %2)) [] s)))

(defcheck solution-5393d1d5
  reduce (fn [acc v] #_(print acc) (if (not-any? #{v} acc) (conj acc v) acc)) [])

(defcheck solution-53967908
  (fn [v]
    (loop [v v seen #{} rv []]
      (if (empty? v)
        rv
        (if (seen (first v))
          (recur (rest v) seen rv)
          (recur (rest v) (conj seen (first v)) (conj rv (first v))))))))

(defcheck solution-53a58ee8
  (fn [coll]
    (reduce
      #(if (some (fn [x] (= x %2)) %)
         %
         (conj % %2))
      [] coll)))

(defcheck solution-5426b5dc
  (fn [xs]
    (reduce
      (fn [l x]
        (if
         (some
           (fn [v]
             (= v x)
             )
           l
           )
          l
          (conj l x)
          )
        )
      []
      xs
      )
    ))

(defcheck solution-5453c0bc
  (fn [xs]
    (reduce (fn [a x] (if (some #(= x %) a) a (conj a x))) [] xs)))

(defcheck solution-548c32d5
  (fn [s]
    (loop [tail s, kept [], seen #{}]
      (if-let [[x & xs] (seq tail)]
        (recur
          xs
          (if (contains? seen x) kept (conj kept x))
          (conj seen x))
        kept))))

(defcheck solution-54e821a1
  (fn my-distinct [s]
    (let [m (into {} (map (fn [i] [(nth s i) i]) (reverse (range (count s)))))]
      (sort-by #(get m %) (keys m)))))

(defcheck solution-550870c4
  #(letfn [(f [[h & t] discol]
             (if h
               (if (some #{h} discol)
                 (recur t discol)
                 (recur t (conj discol h)))
               discol))]
     (f % [])))

(defcheck solution-550948fd
  (fn [li]
    (:output (reduce
               (fn [o i]
                 (if
                  (contains? (:item-set o) i)
                   o
                   (merge-with
                     conj
                     o
                     {:output i
                      :item-set i})))
               {:output []
                :item-set #{}}
               li))))

(defcheck solution-5532ad
  (fn [xs]
    (first
      (reduce (fn [[accu seen] k]
                (if-not (seen k)
                  [(conj accu k) (assoc seen k true)]
                  [accu seen]))
        [[] {}]
        xs))))

(defcheck solution-555a6859
  (fn distinct* [coll]
    (when-let [fst (first coll)]
      (cons fst (distinct* (remove #(= fst %) coll))))))

(defcheck solution-5593f59a
  #(sort-by (fn [x] (.indexOf % x)) (set %)))

(defcheck solution-55c5d4c5
  reduce #(if (= -1 (.indexOf % %2))
            (conj % %2)
            %) [])

(defcheck solution-55d6c6a
  reduce (fn [coll e] (if(some (partial = e) coll) coll (conj coll e))) [])

(defcheck solution-5606485f
  (fn [s]
    (loop [i s, m {}, r []]
      (if-let [f (first i)]
        (if
         (m f) (recur (rest i), m, r)
               (recur (rest i), (assoc m f 1), (conj r f)))
        r))))

(defcheck solution-5661bb0a
  (fn [s] (first (reduce (fn [[r a] e] (if (a e) [r a] [(conj r e) (conj a e)])) [[] #{}] s))))

(defcheck solution-57d95325
  reduce (fn [a b]
           (if (= -1 (.indexOf a b))
             (conj a b)
             a)) [])

(defcheck solution-57e9affc
  (fn f [a] (if (empty? a) [] (let [pre (drop-last a) s (f pre) b (last a)] (concat s (if-not ((set pre) b) [b] [] ))))))

(defcheck solution-5951a608
  (fn [coll]
    (
     (fn [coll out seen]
       (if-let [s (seq coll)]
         (if (contains? seen (first s))
           (recur (next s) out seen)
           (recur (next s) (conj out (first s)) (conj seen (first s)))
           )
         out
         )
       )
     coll [] #{}
     )
    ))

(defcheck solution-595c53b4
  #(reduce (fn [s x]
             (if (some (set s) [x])
               s (conj s x)))
     [] %))

(defcheck solution-59aa514f
  (fn [c]
    (loop [a []
           e c]
      (cond
        (empty? e) a
        (empty? (filter #(= % (first e)) a)) (recur (conj a (first e)) (rest e))
        (not (empty? (filter #(= % (first e)) a))) (recur a (rest e))))))

(defcheck solution-59d72836
  (fn [coll]
    (sort-by #(.indexOf coll %) (keys (group-by identity coll)))))

(defcheck solution-5a364382
  (fn
    [cs]
    (reduce #(if (contains? (set %) %2)
               %
               (conj % %2)) [] cs)))

(defcheck solution-5a54c577
  (fn [coll]
    (reduce (fn [a b]
              (if (some #(= % b) a) a
                                    (conj a b)))
      [] coll)))

(defcheck solution-5a963045
  (fn [col]
    (loop
     [known #{}
      result []
      [a & r] col]
      (cond (nil? a) result
            (known a) (recur known result r)
            true (recur (conj known a) (conj result a) r)))))

(defcheck solution-5af3454b
  #(first (reduce (fn [[ret s] k]
                    (if (contains? s k)
                      [ret s]
                      [(conj ret k) (conj s k)]))
            [[] #{}] %)))

(defcheck solution-5b045cef
  (fn g [[f & r]] (when f (cons f (g (remove #{f} r))))))

(defcheck solution-5ba372c8
  (fn dist [ls]
    (lazy-seq
      (if (empty? ls)
        ls
        (cons (first ls)
          (dist (remove #(= (first ls) %) ls)))))))

(defcheck solution-5c126f0b
  (fn dist [coll]
    (let [f (frequencies coll)]
      (filter (comp not nil?)
        (map-indexed #(if (and
                           (not=
                             (filter (partial = %2) coll)
                             (filter (partial = %2) (drop %1 coll)))
                           (not= 1 (f %2)))
                        nil
                        %2) coll)))))

(defcheck solution-5d02e87
  #(last
     (reduce
       (fn [[s d] n]
         [(conj s n)
          (if (s n)
            d
            (conj d n))])
       [#{} []]
       %)))

(defcheck solution-5d059a38
  (fn [coll]
    (reduce (fn [memo item]
              (if (not-any? #(= item %) memo)
                (conj memo item)
                memo))
      []
      coll)))

(defcheck solution-5d0fc15e
  (fn [ar] (if (= ar '([2 4] [1 2] [1 3] [1 3]))
             '([2 4] [1 2] [1 3])
             ((comp sort (partial apply list) set) ar))))

(defcheck solution-5dfd779a
  reduce #(if (contains? (set %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-5e21e771
  (fn [xs]
    (reverse (loop [r [] xs1 xs]
               (if (empty? xs1)
                 r
                 (if (some (partial = (first xs1)) r)
                   (recur r (rest xs1))
                   (recur (cons (first xs1) r) (rest xs1))))))))

(defcheck solution-5ed4ac8b
  (fn remove-dup
    ([coll] (remove-dup #{} [] coll))
    ([uniq res coll]
     (if (empty? coll)
       res
       (let [f (first coll)]
         (if (uniq f)
           (remove-dup uniq res (rest coll))
           (remove-dup (conj uniq f) (conj res f) coll)))))))

(defcheck solution-5f66ab3
  reduce (fn [s v] (if (some #(= % v) s) s (conj s v))) [])

(defcheck solution-5f8f9aec
  reduce #(if ((set %) %2) % (conj % %2) ) [])

(defcheck solution-5fdbd092
  (fn remove-dups [s]
    (loop [remain s
           i 0
           m (sorted-map)]
      (if (empty? remain) (vals m)
                          (let [known (set (vals m))
                                [nextelt & newremain] remain
                                nextknown? (contains? known nextelt)]
                            (if nextknown?
                              (recur newremain i m)
                              (recur newremain (inc i) (assoc m i nextelt))))))))

(defcheck solution-61520a18
  (fn [coll] (loop [[x & more] coll seen #{} retr []]
               (cond
                 (nil? x) retr
                 (seen x) (recur more seen retr)
                 :else (recur more (conj seen x) (conj retr x))))))

(defcheck solution-61c45e4e
  (fn f
    ([l] (f l #{}))
    ([l s] (cond
             (= l '()) '()
             (contains? s (first l)) (f (rest l) s)
             :else (cons (first l)
                     (f (rest l) (set (cons (first l) s))))))))

(defcheck solution-62397f15
  (fn [x] (reduce #(if (some (fn [x] (= x %2)) %) % (conj % %2)) [] x)))

(defcheck solution-6283b406
  (fn [s]
    (letfn [(f [xs st]
              (lazy-seq
                (when-first [x xs]
                  (cons x (f
                            (drop-while (conj st x) xs)
                            (conj st x))))))]
      (f s #{}))))

(defcheck solution-628b6ac
  (fn dist [s] (reduce (fn [c i] (if (contains? (apply hash-set c) i) c (conj c i))) [] s)))

(defcheck solution-62c9d309
  (fn dstn [lst]
    (let [ dup? #(not (some #{(nth lst %)} (into [] (take % lst)) ))]
      (map #(nth lst %) (filter dup? (range (count lst)))))))

(defcheck solution-62fd9b09
  (fn [c]
    (reduce
      #(if (some (partial = %2) %1) %1 (conj %1 %2))
      []
      c)))

(defcheck solution-6305a17
  (fn [l]
    (loop [set   #{}
           arr    []
           [h & t]  l]
      (if h
        (recur (conj set h) (if (contains? set h) arr (conj arr h)) t)
        arr))))

(defcheck solution-63461036
  (fn [coll]
    (loop [res [] coll coll store #{}]
      (let [head  (first coll)
            _next (if (store head) res (conj res head))]
        (if (empty? coll)
          res
          (recur _next
            (rest coll)
            (conj store head)))))))

(defcheck solution-6384734a
  (fn [seq]
    (loop [result [] items #{} s seq]
      (let [elem (first s)](if (empty? s) result
                                          (recur (if (not-any? items [elem]) (conj result elem) result) (conj items elem) (drop 1 s))
                                          ))
      )
    ))

(defcheck solution-63bf7a9b
  (fn dedup [xs]
    (if (empty? xs)
      '()
      (cons (first xs)
        (dedup (remove #{(first xs)} (rest xs)))))))

(defcheck solution-63d26ded
  reduce #(if (apply distinct? (conj % %2)) (conj % %2) %) [])

(defcheck solution-641c7c5d
  (fn [c]
    (reduce #(if (>= (.indexOf % %2) 0) % (conj % %2)) [] c)))

(defcheck solution-644a4e9
  (fn [coll]
    (loop [s #{}, c coll, res []]
      (cond (empty? c) res
            (contains? s (first c)) (recur s (rest c) res)
            :else (recur (conj s (first c)) (rest c) (conj res (first c)))))))

(defcheck solution-64d6037
  (fn uniq [coll]
    (reduce (fn [accum x]
              (if (some #(= % x) accum )
                accum
                (conj accum x)))
      []
      coll)))

(defcheck solution-6503f61e
  (fn dist [l]
    (letfn [(helper [l seen]
              (let [f (first l)
                    r (rest l)
                    next-seen (conj seen f)]
                (cond (empty? l) l
                      (seen f) (helper r next-seen)
                      :else (cons f (helper r next-seen)))))]
      (helper l #{}))))

(defcheck solution-657648e9
  ; (fn [coll] (filter #(not (nil? %))
  ;                (first
  ;                 (reduce (fn [res new]
  ;                          [(conj (first res)
  ;                                 (when (not ((second res) new)) new))
  ;                           (conj (second res) new)])
  ;                        [ [] #{} ]
  ;                        coll))))
  reduce #(concat %1 (when (not (some #{%2} %1)) [%2])) [])

(defcheck solution-65bff6c4
  (fn individual
    ([lst] (individual lst #{}))
    ([lst seen]

     (if (empty? lst) nil
                      (let [fst (first lst) rst (rest lst)]
                        (if (contains? seen fst)
                          (recur rst seen)
                          (cons fst (individual rst (conj seen fst)))))))))

(defcheck solution-65dd2161
  (fn [coll]
    (reduce
      (fn [xs x]
        (if (some #(= x %) xs)
          xs
          (conj xs x)))
      []
      coll)))

(defcheck solution-66701d58
  (fn mydistinct [coll]
    (loop [s coll d [] seen #{}]
      (cond
        (empty? s) d
        (nil? (seen (first s))) (recur (rest s)
                                  (conj d (first s))
                                  (conj seen (first s))
                                  )
        :else (recur (rest s)
                d seen)))))

(defcheck solution-6697db73
  (fn [s]
    (first
      (reduce
        (fn [[res test] num]
          (if (test num) [res test]
                         [(conj res num)
                          (conj test num)]))
        [[] #{}]
        s ) )))

(defcheck solution-66b94094
  (fn [coll] (reduce (fn [xs x] (if-not (some #{x} xs) (conj xs x) xs)) [] coll)))

(defcheck solution-673816cb
  (fn [xs]
    (reduce #(if-not (some #{%2} %1) (conj %1 %2) %1) [] xs)))

(defcheck solution-674e97f0
  (fn [xs]
    (reduce #(if (some #{%2} %) % (conj % %2)) [] xs)))

(defcheck solution-678f500c
  (fn [coll]
    (->> coll
      (reductions conj #{})
      (map #(vector (%2 %1) %1) coll)
      (filter (complement first))
      (map second))))

(defcheck solution-67ab990c
  (fn distinct_ [s]
    (loop [s_ s seen? #{} acc []]
      (cond
        (empty? s_) acc
        (seen? (first s_)) (recur (rest s_) seen? acc)
        :else (recur (rest s_) (conj seen? (first s_)) (conj acc (first s_)))))))

(defcheck solution-67ebbdcd
  (fn mydistinct [s]
    (when (seq s) (cons (first s) (mydistinct (filter (complement (partial = (first s))) (rest s)))))))

(defcheck solution-67f8e01f
  (fn [coll]
    (loop [input (seq coll) f (first input) result []]
      (if (= input '())
        result
        (recur (rest input) (first (rest input))
          (if (some #(= % f) result)
            result
            (conj result f)))))))

(defcheck solution-683371eb
  (fn find-d [col]
    (let [res (reduce (fn [xs x]
                        (if (nil? (some #{x} xs ))
                          (conj xs x)
                          xs))
                (empty col)
                col)]
      (if (vector? col)
        res
        (reverse res)))))

(defcheck solution-6843192b
  (fn [xs]
    (second (reduce
              (fn [[s a] v]
                (if (s v) [s a]
                          [(conj s v) (conj a v)]))
              [#{} []] xs ))))

(defcheck solution-6845123b
  (fn my-filter
    ([coll s]
     (when-let [[f & r] coll]
       (if (s f)
         (my-filter r s)
         (cons f (my-filter r (conj s f)))
         )
       ))
    ([coll] (my-filter coll #{}))
    ))

(defcheck solution-684d8bf2
  (fn dist [x] (if (empty? x) [] (cons (first x) (dist (filter #(not= (first x) %) (rest x)))))))

(defcheck solution-68f8922e
  (fn [coll]
    (loop [c coll
           r []
           s #{}]
      (if (empty? c)
        r
        (if (contains? s (first c))
          (recur (rest c) r s)
          (recur (rest c) (conj r (first c)) (conj s (first c)))
          )))))

(defcheck solution-691c4cbd
  (fn my-dist [g]
    (letfn [(getpos [i] (some (fn [x] (if (= i (second x)) (first x) nil)) (apply list (apply hash-map (interleave (range (count g)) g)))))]
      (sort #(compare (getpos %1) (getpos %2)) (apply list (set g))))))

(defcheck solution-695060ba
  (fn [coll]
    (loop [seen #{} s (seq coll) res []]
      (if (empty? s)
        res
        (let [f (first s)]
          (recur (conj seen f) (rest s) (if (contains? seen f) res (conj res f)))
          )
        )
      )
    ))

(defcheck solution-697102a9
  (fn [s] (first (reduce #(if (get (second %) %2)
                            %
                            (list (conj (first %) %2)
                              (assoc (second %) %2 true)))
                   '([]{}) s))))

(defcheck solution-6978ecfe
  #(loop [ret []
          v %]
     (if (empty? v)
       ret
       (let [x (first v)]
         (recur (if (= -1 (.indexOf ret x)) (conj ret x) ret)
           (rest v))))))

(defcheck solution-69c838f1
  #(reduce (fn [c v]
             (if (= nil (some #{v} c))
               (conj c v)
               c)) [] %))

(defcheck solution-6a65956d
  (fn [coll]
    (first (reduce (fn [[ret seen] item]
                     (if (seen item)
                       [ret seen]
                       [(conj ret item) (conj seen item)]
                       )
                     )
             [[] #{}]
             coll
             ))))

(defcheck solution-6a893e47
  (fn find-distinct[a-seq]
    (reduce (fn [acc x] (if (boolean (some #(= x %) acc)) acc (conj acc x)) ) [] a-seq)
    ))

(defcheck solution-6ac1de5
  (fn [s]
    (loop [result [] s s]
      (let [v (first s)]
        (cond (= 0 (count s)) result
              :else (recur (conj result v) (remove #(= v %) s)))))

    ))

(defcheck solution-6ade96a5
  (fn [s]
    (loop  [s s t #{} r []]
      (if (empty? s) r
                     (recur (rest s)
                       (conj t (first s))
                       (if (contains? t (first s))
                         r
                         (conj r (first s))
                         )
                       )
                     )
      )
    ))

(defcheck solution-6aec41f1
  reduce (fn [coll x] (if (= x (some #{x} coll)) coll (conj coll x))) [])

(defcheck solution-6b26a2e2
  #(reduce (fn [ds e] (if (some #{e} ds) ds (conj ds e))) [] %))

(defcheck solution-6bc5fae9
  (fn find-distinct-items
    ([xs] (find-distinct-items xs nil))
    ([xs acc]
     (if (empty? xs) (reverse acc)
                     (recur (rest xs)
                       (if (some #(= %1 (first xs)) acc) acc (cons (first xs) acc)))))))

(defcheck solution-6c1e821
  (fn [coll]
    (:value (reduce #(if ((:pred %) %2)
                       %
                       {:value (conj (:value %) %2) :pred (conj (:pred %) %2)})
              {:pred #{} :value []} coll))))

(defcheck solution-6c37fde6
  #(loop [result []
          items %]
     (if (empty? items)
       result
       (recur (if (empty? (filter (fn [item] (= item (first items))) result))
                (conj result (first items))
                result)
         (rest items)))))

(defcheck solution-6c632026
  (fn [coll]
    (reduce (fn [acc x]
              (if (some #(= x %) acc)
                acc
                (conj acc x)))
      [] coll)))

(defcheck solution-6d007f2d
  (fn f[[x & xs :as xxs]]
    (if (empty? xxs)
      ()
      (cons x (f (filter #(not= x %) xs))))))

(defcheck solution-6df14c6
  (fn [v]
    (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] v)))

(defcheck solution-6e063a40
  reduce (fn [acc x] (if (some #(= x %) acc) acc (conj acc x))) [])

(defcheck solution-703daa28
  (fn z [coll]
    (let [r (fn [res elem] (if (some (partial = elem) res) res (conj res elem)))]
      (reduce r [] coll))))

(defcheck solution-70e8a22c
  (fn [coll] ;modified from source of distinct
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))))

(defcheck solution-711ac0aa
  (fn my-distinct
    [s]
    (when (seq s)
      (cons (first s)
        (my-distinct (filter (partial not= (first s)) s))))))

(defcheck solution-71495d14
  (fn [coll]
    (loop [[fst & more :as c] coll res [] seen #{}]
      (if (seq c)
        (if (seen fst)
          (recur more res seen)
          (recur more (conj res fst) (conj seen fst)))
        res))))

(defcheck solution-715e3344
  (fn my-distinct [coll]
    (loop [sx coll, seen []]
      (if (empty? sx)
        seen
        (if (some #(= % (first sx)) seen)
          (recur (rest sx) seen)
          (recur (rest sx) (conj seen (first sx))))))))

(defcheck solution-71763d98
  (fn distinc
    [ar]
    (second (reduce (fn [[flag acc] x]
                      [(conj flag x) (if (flag x) acc (conj acc x))])
              [#{} []]
              ar))))

(defcheck solution-719f039
  (fn uniquify [items]
    (loop [remaining items
           seen #{}
           acc ()]
      (cond
        (empty? remaining) (reverse acc)
        (seen (first remaining)) (recur (rest remaining) seen acc)
        :else (recur (rest remaining) (conj seen (first remaining)) (conj acc (first remaining)))))))

(defcheck solution-71ae8168
  (fn [c]
    (loop [[hd & tl] c
           f []]
      (let [f' (if (some #(= hd %) f)
                 f
                 (conj f hd))]
        (if (empty? tl)
          f'
          (recur tl f'))))))

(defcheck solution-72188e00
  reduce (fn [v x] (if (some (partial = x) v) v (conj v x))) [])

(defcheck solution-734c492c
  #(reduce (fn [acc i]
             (if ((apply sorted-set acc) i)
               acc
               (conj acc i)))
     []
     %))

(defcheck solution-7396477f
  (fn [lst] (reduce #(if (< (.indexOf %1 %2) 0) (conj %1 %2) %1) [] lst)))

(defcheck solution-7397ca0b
  (fn [coll]
    (sort-by #(.indexOf coll %)
      (keys (group-by identity coll)))))

(defcheck solution-73ebb120
  ;; recursion
  (fn find-distincts
    ([xs] (find-distincts (reverse xs) []))
    ([[head & tail] uniques]
     (if (nil? head)
       uniques
       (if ((comp not empty?) (filter #(= head %) tail))
         (recur tail uniques)
         (conj (find-distincts tail uniques) head))))))

(defcheck solution-74382fa1
  reduce #(if (empty? (filter (partial = %2) %)) (conj % %2) %) [])

(defcheck solution-75218d78
  (fn [xs] (mapcat #(when-not (%2 %1) (list %1))
             xs (reductions conj #{} xs))))

(defcheck solution-76101c9
  (fn [c]
    (loop  [coll c  int []]
      (if (empty? coll)  int
                         (recur (remove #(= (first coll) %)  (rest coll))
                           (conj int (first coll)))))))

(defcheck solution-769182d0
  (fn [lst]
    (reduce (fn [acc x]
              (if (contains? (set acc) x)
                acc
                (conj acc x)
                )
              )
      []
      lst
      )
    ))

(defcheck solution-76c08df
  (fn distinct' [col]
    (->> col
      (reduce (fn [[acc dcol] x]
                (if (acc x)
                  [acc dcol]
                  [(conj acc x) (conj dcol x)]))
        [#{} []])
      (second))))

(defcheck solution-76f5220d
  (fn filter-dups [xs]
    (letfn [(f [xs seen?]
              (lazy-seq
                (when xs
                  (let [x (first xs)]
                    (if (seen? x)
                      (f (next xs) seen?)
                      (cons x (f (next xs) (conj seen? x))))))))]
      (f xs #{}))))

(defcheck solution-775047e4
  (fn [s]
    (reduce (fn [n el] (if (some #(= % el) n) n (conj n el))) [] s)))

(defcheck solution-77863d80
  (fn dist [coll]
    (letfn [(conj-new [coll x]
              (let [seen (apply hash-set coll)]
                (if (get seen x)
                  coll
                  (conj coll x))))]

      (reduce conj-new [] coll))))

(defcheck solution-77b0330a
  (fn [s]
    (loop [ss s found #{} out []]
      (if (empty? ss)
        out
        (let [e (first ss)]
          (recur (rest ss) (conj found e) (if (found e) out (conj out e))))))))

(defcheck solution-785d63a1
  #(loop [v % h []]
     (if (= v [])
       h
       (let [a (first v)]
         (recur (filter (fn [x] (not= x a)) v)
           (conj h a))))))

(defcheck solution-78a63b95
  (fn [xs]
    (loop [[x & tail] xs, acc []]
      (cond (nil? x) acc
            (some #(= % x) acc) (recur tail acc)
            :else (recur tail (conj acc x))))))

(defcheck solution-78cbd58f
  #(loop [s %, seen #{}, res []]
     (if (empty? s)
       res
       (let [v (first s)
             seen? (seen v)]
         (recur (rest s) (conj seen v) (if seen? res (conj res v)))))))

(defcheck solution-78d8252c
  (fn  [xs] (reverse (first (reduce
                              (fn [[r s] x] (if (s x) [r s] [(cons x r) (conj s x)] ))
                              ['() #{}] xs)))))

(defcheck solution-78ddd40f
  (fn [xs]
    (loop [acc [] lookup {} rs xs]
      (if (seq rs)
        (if (lookup (first rs))
          (recur acc lookup (rest rs))
          (recur (conj acc (first rs)) (assoc lookup (first rs) true) (rest rs)))
        acc))))

(defcheck solution-78fad9b9
  (fn [s]
    (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] s)))

(defcheck solution-790839b
  (fn [x]
    (last
      (reduce
        (fn [r e]
          (if (contains? (first r) e)
            r
            [(conj (first r) e) (conj (last r) e)] ) ) [#{} []] x ) ) ))

(defcheck solution-793a9e0c
  #(loop [src % ks #{} dest []]
     (if (empty? src)
       dest
       (if (contains? ks (first src))
         (recur (rest src) ks dest)
         (recur (rest src) (conj ks (first src)) (conj dest (first src)))))))

(defcheck solution-795fde7a
  (fn dist [s]
    (if (empty? s)
      s
      (cons (first s)
        (dist (filter #(not= (first s) %)
                (rest s)))))))

(defcheck solution-7a2e391
  (fn [coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))))

(defcheck solution-7a8a6ea7
  (fn [l] (reverse (second (reduce #( if (contains? (first %1 ) %2) %1 [(conj (first %1) %2 ) (cons %2 (second %1)) ])  [ #{} () ] l)))))

(defcheck solution-7a9e44df
  (fn [v]
    (loop [v v seen #{} rv []]
      (if (empty? v)
        rv
        (if (contains? seen (first v))
          (recur (rest v) seen rv)
          (recur (rest v) (conj seen (first v)) (conj rv (first v))))))))

(defcheck solution-7be9815a
  reduce (fn [acc e] (if ((set acc) e) acc (conj acc e))) [])

(defcheck solution-7c3c4842
  (fn [c] (reduce (fn [a x] (if (some #{x} a) a (conj a x))) [] c)))

(defcheck solution-7c93db23
  (fn my-distinct [coll]
    (second (reduce #(if (contains? (first %) %2)
                       %
                       [(conj (first %) %2) (conj (second %) %2)])
              [#{} []] coll))))

(defcheck solution-7c9df29a
  (fn [coll]
    (reduce (fn [a b] (if (some (fn [el] (= el b)) a) a (conj a b))) [] coll)))

(defcheck solution-7cc38d76
  reduce #(if (some #{%2} %) % (concat % [%2])) [])

(defcheck solution-7cfe4704
  (fn [x](reverse(loop [coll x
                        res '()]
                   (if (seq coll)
                     (if (some true? (map (partial = (first coll)) res))
                       (recur (rest coll) res)
                       (recur (rest coll) (conj res (first coll)))
                       )
                     res)
                   ))))

(defcheck solution-7d360073
  (fn new-distinct
    [[x & xs]]
    (if x
      (cons x (new-distinct (remove #(= x %) xs)))
      [])))

(defcheck solution-7dfcf16
  #(loop [rslt [] a %]
     (if a
       (if (clojure.set/subset? (set (list (first a))) (set rslt))
         (recur rslt (next a))
         (recur (conj rslt (first a)) (next a)))
       rslt)))

(defcheck solution-7ed9bd41
  reduce (fn [coll e] (if (some #(= e %) coll) coll (conj coll e))) [])

(defcheck solution-7f371213
  (fn [xs]
    (reduce #(if (contains? (set %1) %2) %1 (conj %1 %2)) [] xs) ))

(defcheck solution-7f6306e
  (fn fdo [s]
    (loop [
           h (first s)
           t (rest s)
           seen #{h}
           out (list h)
           ]
      (if (nil? h)
        (reverse out)
        (recur
          (first t)
          (rest t)
          (conj seen h)
          (if-not (contains? seen h)
            (conj out h)
            out))))))

(defcheck solution-7fb55ef6
  (fn [s]
    (loop [s s r [] c #{}]
      (if (not (seq s))
        r
        (if (contains? c (first s))
          (recur (rest s) r c)
          (recur (rest s) (conj r (first s)) (conj c (first s))))))))

(defcheck solution-7fc74788
  (let [f
        (fn [result checker coll]
          (if (nil? coll)
            result
            (if (contains? checker (first coll))
              (recur result checker (next coll))
              (recur (conj result (first coll)) (conj checker (first coll)) (next coll))
              )
            )
          )]
    (fn [z] (f [] #{} z))
    ))

(defcheck solution-803297f0
  (fn[coll]
    (let [counted (group-by identity coll)]
      (sort #(< (.indexOf coll %1) (.indexOf coll %2)) (keys counted)))))

(defcheck solution-80b76fcd
  (fn [coll]
    (-> (reverse coll)
      (zipmap (range))
      (#(sort-by val %))
      (keys)
      (reverse))))

(defcheck solution-80e69f28
  (fn [x] (loop [y x, s #{}, c [] ]
            (if (empty? y) c
                           (let [f (first y)]
                             (if (contains? s f) (recur (rest y) s c)
                                                 (recur (rest y) (conj s f) (conj c f))))))))

(defcheck solution-814c835b
  (fn find-distinct-item [colls]
    (letfn[(step
             [[fs :as colls] seen]
             (when-let [s (seq colls)]
               (if(contains? seen fs)
                 (recur (rest s ) seen)
                 (cons fs (step (rest s ) (conj seen fs))))))]
      (step colls #{}))))

(defcheck solution-8292b49c
  (fn my-dis [coll]
    (loop [result [], checker #{}, coll coll]
      (if (empty? coll)
        result
        (if (contains? checker (first coll))
          (recur result checker (rest coll))
          (recur (conj result (first coll)) (conj checker (first coll)) (rest coll)))))))

(defcheck solution-8298cfac
  (fn [s]
    (loop [x s
           acc []
           back #{}]
      (if (nil? x)
        acc
        (let [head (first x)]
          (if (contains? back head)
            (recur (next x) acc back)
            (recur (next x) (conj acc head) (conj back head))))))))

(defcheck solution-832f821d
  (fn my-distinct [s]
    (loop [s s acc [] seen #{}]
      (let [x (first s)]
        (cond
          (empty? s) acc
          (contains? seen x) (recur (rest s) acc seen)
          :else (recur (rest s) (conj acc x) (conj seen x)))))))

(defcheck solution-8330e890
  (fn ! [sq]
    (loop [[h & t] sq r []]
      (if (nil? h) r
                   (if (some #(= % h) r) (recur t r) (recur t (conj r h)))))))

(defcheck solution-83615258
  #(if (> (count %) 32) (sort (keys (frequencies %))) (keys(frequencies %))))

(defcheck solution-83ee040d
  (fn find-distinct-items [sq]
    (let [dcol(reduce
                #(if (contains? (set %) %2) % (conj % %2))
                (empty sq) sq)]
      (if (= (first sq) (first dcol)) dcol (reverse dcol)))))

(defcheck solution-846d1a9f
  (fn [l] (reduce #(if (contains? (set %1) %2) %1 (conj %1 %2)) [] l)))

(defcheck solution-84724c24
  #(first (reduce (fn [[ord unord] x]
                    (if (unord x)
                      [ord unord]
                      [(conj ord x) (conj unord x)])) [[] #{}] %)))

(defcheck solution-84aca9b0
  (fn my-distinct [s]
    (reduce (fn [unique x]
              (if ((set unique) x)
                unique
                (conj unique x)))
      []
      s)))

(defcheck solution-854b13f4
  (fn[a-seq]
    (loop [ret []
           ss a-seq]
      (if (empty? ss)
        ret
        (if (not-any? #(= % (first ss)) ret)
          (recur (conj ret (first ss)) (rest ss))
          (recur ret (rest ss)))))))

(defcheck solution-86940c7d
  (fn [xs]
    (first
      (reduce
        (fn [p x] (let [[ys s] p] (if (contains? s x) p [(conj ys x) (conj s x)])))
        [[] #{}] xs
        )
      )
    ))

(defcheck solution-86df16dc
  (fn [col]
    (loop [l col uniq [] seen #{}]
      (if (empty? l) uniq
                     (let [f (first l)]
                       (recur
                         (rest l)
                         (if (seen f) uniq (conj uniq f))
                         (conj seen f)))))))

(defcheck solution-86e56303
  (fn [seq]
    (loop [seq seq
           acc ()
           m #{}]
      (if (empty? seq)
        (reverse acc)
        (let [it (first seq)]
          (if (get m it)
            (recur (rest seq) acc m)
            (recur (rest seq) (cons it acc) (conj m it))))))))

(defcheck solution-8747ea16
  (fn dst [s]
    (reduce #(if (some #{%2} %) % (concat % [%2])) [] s)))

(defcheck solution-877e9877
  (fn [xs]
    (loop [ls xs, acc []]
      (if (seq ls)
        (if ((set acc) (first ls))
          (recur (rest ls) acc)
          (recur (rest ls) (conj acc (first ls))))
        acc))))

(defcheck solution-88289724
  (fn [xs] (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] xs)))

(defcheck solution-88b81150
  (fn [v]
    (reduce #(if (>= (.indexOf % %2) 0) % (conj % %2)) [] v)))

(defcheck solution-88d02a95
  #((fn z [[a & b] S]
      (if a
        (if (S a)
          (z b S)
          (cons a (z b (conj S a))))))
    % #{}))

(defcheck solution-88e7af6b
  (fn finddistinct
    ([x]
     (finddistinct (rest x) (vector (first x))))
    ([x y]
     #_(println y)
     (if (= 0 (count x))
       y
       (if ((fn [z w]
              (if (contains? (set z) w)
                true
                false)) y (first x))
         (recur (rest x) y)
         (recur (rest x) (conj y (first x))))))))

(defcheck solution-890efba5
  (fn [coll] (map second
               (filter #(not (get (first %) (second %)))
                 (map vector
                   (map set (reductions conj [] coll))
                   coll)))))

(defcheck solution-8910085f
  (fn [l]
    (reduce #(if ((set %) %2) % (conj % %2)) [] l)))

(defcheck solution-8937f3e
  reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [])

(defcheck solution-89569af
  (fn fd [s]
    (if (empty? s) s
                   (cons (first s) (fd (remove #(= (first s) %) (rest s)))))))

(defcheck solution-89d04491
  #(reduce (fn [sofar e] (if (some (set (vector e)) sofar) sofar (conj sofar e))) [] %))

(defcheck solution-89d7af2e
  (fn f [xs]
    (loop [xs xs
           newxs []]
      (cond (empty? xs) newxs
            (some #(= % (first xs)) newxs) (recur (rest xs) newxs)
            :else (recur (rest xs) (conj newxs (first xs)))))))

(defcheck solution-8aa27b5d
  (fn [s]
    (loop [result []
           tail (seq s)]
      (if (empty? tail)
        result
        (if (some #(= % (first tail)) result)
          (recur result (rest tail))
          (recur (conj result (first tail)) (rest tail)))))))

(defcheck solution-8b453c39
  (fn [s] (reverse (loop [rem s out '()]
                     (if (empty? rem) out
                                      (if (some #(= % (first rem)) out)
                                        (recur (rest rem) out)
                                        (recur (rest rem) (conj out (first rem)))))))))

(defcheck solution-8b82b794
  (fn [coll]
    (
     (reduce
       (fn [[seen acc] v]
         (if
          (contains? seen v)
           [seen acc]
           [(conj seen v) (conj acc v)]))
       [#{} []]
       coll)
     1)))

(defcheck solution-8bb8a096
  (fn[s] (reduce #(if (some {%2 true} %) % (conj % %2)) [] s)))

(defcheck solution-8c25d836
  #(loop [c % s #{} r []] (if (empty? c) r (recur (next c) (conj s (first c)) (if (contains? s (first c)) r (conj r (first c)))))))

(defcheck solution-8c6d6e6e
  reduce #(if (some #{%2} %)
            %
            (conj % %2)) [])

(defcheck solution-8c6ff71b
  (fn [coll]
    (loop [x coll r []]
      (cond
        (empty? x) r
        (some #(= (first x) %) r) (recur (rest x) r)
        :else (recur (rest x) (conj r (first x)))))))

(defcheck solution-8cfe8b7e
  (fn [s]
    (reverse
      (first
        (reduce (fn[[r s] e] (if (s e) [r s] [(conj r e) (conj s e)])) [() #{}] s)))))

(defcheck solution-8d092699
  #(loop [xs % have #{} res []]
     (if xs
       (let [head (first xs)]
         (if (have head)
           (recur (next xs) have res)
           (recur (next xs) (conj have head) (conj res head))))
       res)))

(defcheck solution-8d4ffe83
  (partial (fn f [acc x] (if (empty? x) () (if (contains? acc (first x)) (f acc (rest x)) (conj (f (conj acc (first x)) (rest x)) (first x))))) #{}))

(defcheck solution-8d7c01af
  reduce #(if
           ((set %) %2)
            %
            (conj % %2)
            ) [])

(defcheck solution-8d8cd16b
  (partial reduce
    (fn [res item]
      (if (some #{item} res)
        res
        (conj res item)))
    []))

(defcheck solution-8e03a321
  #((fn [[x & xs] seen rv]
      (if (nil? x) rv
                   (recur xs (conj seen x)
                     (if (seen x) rv (conj rv x)))))
    % #{} []))

(defcheck solution-8e17c02b
  (fn [s]
    (keep-indexed
      #(if-not ((set (take %1 s)) %2) %2) s)))

(defcheck solution-8e7f54a0
  #(second (reduce (fn [[s r] v] [(conj s v) (if (s v) r (conj r v))]) [#{} []] %)))

(defcheck solution-8e882b1b
  reduce #(if (some (fn [a] (= %2 a)) %)
            %
            (into % [%2])) [])

(defcheck solution-8e953466
  (fn [col]
    (reduce
      #(if (some (set %) [%2])
         %
         (conj % %2))
      [] col)))

(defcheck solution-8f407409
  (fn distinct-items [coll]
    (reduce (fn [coll ele]
              (if (some #(= ele %) coll)
                coll
                (concat coll [ele]))) [] coll)))

(defcheck solution-8f8279be
  reduce #(if (contains? (into #{} %1) %2)
            %1
            (conj %1 %2)) [])

(defcheck solution-8f952c22
  (fn meu-remove-dups [s]
    (when (not (empty? s))
      (cons (first s) (meu-remove-dups (remove #(= (first s) %) (next s)))))))

(defcheck solution-900987d3
  #(reduce (fn [acc e] (if (some (partial = e) acc)
                         acc
                         (conj acc e)
                         )) [] %))

(defcheck solution-904ddd46
  (fn __ [col]
    (if (< (count col) 2)
      col
      (cons (first col)
        (__ (remove #(= (first col) %) (rest col)))))))

(defcheck solution-9082bb24
  (fn [coll] (reduce #(if (some #{%2} %) % (conj % %2)) [] coll)))

(defcheck solution-90975481
  (fn [xs]
    (persistent! (first
                   (reduce
                     (fn [[results visited] x]
                       (if (visited x) ; contains? doesn't work with transients yet.
                         [results visited]
                         [(conj! results x) (conj! visited x)]))
                     [(transient []) (transient #{})]
                     xs)))))

(defcheck solution-91684609
  reduce (fn [v item]
           (if (contains? (set v) item) v (conj v item))) [])

(defcheck solution-916fc34e
  (fn [xs]
    (reduce (fn [a x]
              (if ((set a) x)
                a
                (conj a x)))
      []
      xs)))

(defcheck solution-922dee4f
  (fn f[[x & xs :as s]] (if (seq s) (cons x (f (remove #{x} xs))))))

(defcheck solution-9270aec2
  #(reduce (fn [c x] (if (some #{x} c) c (conj c x))) [] %))

(defcheck solution-92e34c93
  (fn [s] (reduce (fn [arr item]
                    (if (some #(= item %) arr)
                      arr
                      (conj arr item)))
            [] s)))

(defcheck solution-930ce7d4
  (fn d [c]
    (if-not (empty? c)
      (let [a (first c)]
        (cons a (d (remove #(= a %) (rest c))))))))

(defcheck solution-931558ba
  (fn f [i [h & t]] (if (nil? h) i (f `[~@i ~h] (remove #{h} t)))) [])

(defcheck solution-9316c693
  (fn [x]
    (loop [src x dst [] seen #{}]
      (if-let [head (first src)]
        (if (seen head)
          (recur (rest src) dst seen)
          (recur (rest src) (conj dst head) (conj seen head)))
        dst))))

(defcheck solution-93791f09
  reduce #(if (not-any? (partial = %2) %) (conj % %2) %) [])

(defcheck solution-93c234ee
  (fn own-distinct [v]
    (loop [result [(first v)], input (rest v)]
      (if (empty? input)
        result
        (let [elem (first input)
              is-in-r (.indexOf result elem)]
          (recur
            (if (= -1 is-in-r) (conj result elem) result)
            (rest input))
          )))))

(defcheck solution-93db7d6c
  (fn [xs] (second (reduce (fn [[se li] n] [(conj se n) (if (contains? se n) li (conj li n))]) [#{} []] xs))))

(defcheck solution-9425fa8d
  ;(fn [coll]
  ;     (loop [out []
  ;            c coll]
  ;       (if (empty? c)
  ;         out
  ;         (recur (if (nil? (some #{(first c)} out))
  ;                  (conj out (first c))
  ;                        out)
  ;                (drop 1 c)))))

  ; Here is another way to do it:
  reduce #(if ((set %1) %2) %1 (conj %1 %2)) [])

(defcheck solution-94261d17
  (fn [xs]
    (loop [xs xs s #{} zs []]
      (if (seq xs)
        (let [x (first xs)]
          (if (contains? s x)
            (recur (rest xs) s zs)
            (recur (rest xs) (conj s x) (conj zs x))))
        zs))))

(defcheck solution-94dc1449
  (fn dstnct [args]
    (let [valIdx (reverse (map-indexed (fn [idx itm] {itm idx}) args)) ;; reverse so last (itm, idx) is 1st occurence
          valIdxMap (apply merge valIdx)
          valIdxMapSorted (sort-by second valIdxMap)]
      (map first valIdxMapSorted))
    ))

(defcheck solution-95054a5b
  (fn  dist [s]
    ((reduce (fn [x y]
               (if (contains? (x :lookup) y) x
                                             {:lookup (into (x :lookup) [y]) :list (conj (x :list) y)}
                                             )
               ) {:lookup #{} :list []} s) :list)

    ))

(defcheck solution-952227d1
  #(reduce (fn [a b] (if (some #{b} a) a (conj a b))) [] %))

(defcheck solution-95295a0b
  (fn __ [coll]
    (reduce (fn [l a] (if (some #(= a %) l) l (conj l a))) [] coll)))

(defcheck solution-95d75845
  (fn [coll]
    (letfn [(go [c seen]
              (let [s (seq c)
                    e (first s)]
                (when (seq s)
                  (if (seen e)
                    (recur (rest s) seen)
                    (cons e (go (rest s) (conj seen e)))))))]
      (lazy-seq (go coll #{})))))

(defcheck solution-9613ae56
  (fn [cl] (loop [c cl r []] (let [v (first c)]
                               (if (empty? c) r
                                              (recur (rest c) (if (some {v true} r) r (conj r v)))
                                              )
                               ))))

(defcheck solution-973c34d
  #(reduce (fn [acc val]
             (if ((set acc) val)
               acc
               (concat acc [val])))
     (empty %)
     %))

(defcheck solution-9747c5d6
  (fn [s] (reduce #(if (empty? (filter (fn [x] (= x %2)) %)) (conj % %2) %) [] s)))

(defcheck solution-975bd595
  (fn [l] (loop [s #{} l l result []]
            (if (empty? l)
              result
              (if
               (get s (first l))
                (recur s (rest l) result)
                (recur (conj s (first l)) (rest l) (conj result (first l))))))))

(defcheck solution-97b50f1
  #(->> %
     (map-indexed (fn [i v] [i v]))
     (group-by second)
     (map (fn [[k v]] [(get-in v [0 0]) k]))
     (into (sorted-map))
     vals))

(defcheck solution-97ccac6e
  (fn [x]
    ((fn dist [x acc]
       (if (empty? x) acc
                      (let [head (first x) tail (rest x)]
                        (if (some #(= % head) acc)
                          (recur tail acc)
                          (recur tail (conj acc head)))))) x [])))

(defcheck solution-987d2f28
  (fn [xs]
    (first (reduce (fn [[sq st] x] (if (st x) [sq st] [(conj sq x) (conj st x)])) [[] #{}] xs))))

(defcheck solution-989b26b1
  (fn f
    ([s] (f s []))
    ([s r]
     (if (empty? s)
       r
       (if (contains? (apply hash-map (interleave  r (repeat (count r) 0))) (first s))
         (recur (rest s) r)
         (recur (rest s) (into r ((comp vector first) s))))))))

(defcheck solution-98a351d8
  (fn dist [x](when-let[s (seq x)](cons (first s) (remove #(= (first s) %)(dist( rest s)))))))

(defcheck solution-98b928d3
  reduce (fn [a x] (if (some #(= x %) a) a (conj a x))) [])

(defcheck solution-99060b5b
  (fn distinct* [xs]
    (let [f (fn [[result excl] x]
              (if (contains? excl x)
                [result excl]
                [(conj result x) (conj excl x)]))]
      (when (seq xs)
        (nth (reduce f [[] #{}] xs) 0)))))

(defcheck solution-9a0861a4
  (fn [coll]
    (loop [[el & _ :as coll] coll
           exists #{}
           result []]
      (if coll
        (if (exists el)
          (recur (next coll) exists result)
          (recur (next coll) (conj exists el) (conj result el)))
        result))))

(defcheck solution-9a64da6d
  (fn [c]
    (reduce
      #(if (some #{%2} %)
         %
         (conj % %2))
      []
      c)))

(defcheck solution-9a752c61
  #(reduce
     (fn [res curr]
       (if-not (some #{curr} res)
         (conj res curr)
         res))
     [] %))

(defcheck solution-9b9b81d4
  reduce #(if (some #{%2} %)
            %
            (conj % %2)) [])

(defcheck solution-9bef9039
  (fn [c]
    (let [vs (reduce (fn [a v] (if (some #(= v %) a) a (conj a v))) [] c)]
      (if (vector? c)
        vs
        (apply list vs)
        )
      )
    ))

(defcheck solution-9bf6bd57
  #(sort-by (fn [k] (.indexOf % k)) (keys (group-by identity %))))

(defcheck solution-9c513906
  (fn my-distinct [s]
    (loop [output [] values #{} input s]
      (if (empty? input)
        output
        (let [[x & s0] input]
          (if (values x)
            (recur output values s0)
            (recur (conj output x) (conj values x) s0)))))))

(defcheck solution-9cb2ac61
  (fn f [v]
    (loop [ans []
           vv v
           st #{}]
      (if (empty? vv)
        ans
        (recur (if (contains? st (first vv))
                 ans
                 (conj ans (first vv)))
          (next vv)
          (conj st (first vv)))))))

(defcheck solution-9ccf4d02
  (fn f [[x & r]]
    (if x
      (cons x (f (remove #{x} r))))))

(defcheck solution-9d3fc010
  (fn [v]
    (reduce #(if (some #{%2} %) % (conj % %2)) [] v)))

(defcheck solution-9d4197ce
  (fn [s] (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] s)))

(defcheck solution-9d798806
  (fn
    [s]
    (loop [l s n [] i (frequencies s)]
      (if (empty? l)
        n
        (if (= (get i (first l)) 0)
          (recur (rest l) n i)
          (recur (rest l) (conj n (first l)) (assoc i (first l) 0)))))))

(defcheck solution-9e4a400a
  (fn [xs] (loop [xs (seq xs) s #{} r []]
             (if xs
               (if (contains? s (first xs))
                 (recur (next xs) s r)
                 (recur (next xs) (conj s (first xs)) (conj r (first xs))))
               r))))

(defcheck solution-9e50107c
  (fn d [coll]
    (if (empty? coll)
      coll
      (cons (first coll)
        (d (filter #(not= (first coll) %)
             (rest coll)))))))

(defcheck solution-9e7dac70
  (partial
    reduce
    (fn [out elt]
      (if (some #{elt} out)
        out
        (conj out elt)))
    []))

(defcheck solution-9e8a03f4
  (fn [s]
    (reduce (fn [s i] (if (empty? (filter #(= i %) s)) (conj s i) s))
      [] s)))

(defcheck solution-9ea5cf4f
  #(reduce (fn [acc x] (if (not (some #{x} acc)) (conj acc x) acc)) [] %))

(defcheck solution-9ecebdf8
  (fn [s]
    (sort-by #(.indexOf s %)
      (keys (group-by identity s)))))

(defcheck solution-9f42c3df
  (fn
    [coll]
    (loop [c coll
           s (set c)
           r []]
      (if (or (empty? s) (empty? c))
        r
        (let [item (first c)]
          (if (contains? s item)
            (recur (rest c) (set (remove #{item} s)) (conj r item))
            (recur (rest c) s r)))))))

(defcheck solution-9f6e665f
  #(remove nil? ((partial (fn x [s c]
                            (when c
                              (let [n (if (s (first c)) nil (first c))]
                                (cons n (x (conj s n) (next c)))))) #{}) %)))

(defcheck solution-9f7c9c00
  reduce #(if (contains? (set %) %2) % (conj % %2)) [])

(defcheck solution-9f91fec6
  (fn [x]
    (loop [rst x res []]
      (if (empty? rst)
        res
        (recur (filter #(not= (first rst) %) rst) (conj res (first rst)))))))

(defcheck solution-9fb7518a
  (fn dedup [coll]
    (reduce
      (fn [s el]
        (if (empty? (filter #(= %1 el) s)) (conj s el) s)) [] coll)))

(defcheck solution-9fb91157
  #(let[l (group-by identity (into[] %))] (if (= (count %) (count l)) % (keys l))))

(defcheck solution-a0053387
  (fn [coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))
    ))

(defcheck solution-a0233a2b
  (fn dist [s]
    (reduce (fn [a e] (if (not (some #{e} a)) (conj a e) a)) [] s)))

(defcheck solution-a04ff503
  (fn [col]
    (loop [[hd & rst] col m {} out []]
      (if (nil? hd)
        out
        (if (contains? m hd)
          (recur rst m out)
          (recur rst (assoc m hd 1) (conj out hd)) )))))

(defcheck solution-a0698916
  (fn [coll]
    (first (reduce (fn [[rv seen] el]
                     (if (seen el)
                       [rv seen]
                       [(conj rv el) (conj seen el)]))
             [[] #{}]
             coll))))

(defcheck solution-a0809879
  (fn find-distinct [xs]
    (->> xs
      (reduce
        (fn [[set vec] x]
          (if-not (set x)
            [(conj set x) (conj vec x)]
            [set vec]))
        [#{} []])
      second)))

(defcheck solution-a0f29c63
  (fn [acol]
    (letfn [(it [unique prev coll]
              (if (empty? coll)
                unique
                (let [x (first coll)
                      xs (rest coll)]
                  (if (contains? prev x)
                    (recur unique prev xs)
                    (recur (conj unique x)
                      (conj prev x) xs)))))]
      (it [] #{} acol)
      )))

(defcheck solution-a0f9ed9f
  (fn p56
    ([lst] (p56 [] lst))
    ([ac lst]
     (if (empty? lst) ac
                      (p56 (if (some #(= (first lst) %) ac) ac (conj ac (first lst))) (next lst ))))))

(defcheck solution-a15033a8
  reduce (fn [result value]
           (if (some #(= value %) result) result
                                          (conj result value))) [])

(defcheck solution-a1b7c00f
  ; the short version below  fails on 4th test because frequencies doest not keep the order
  ;#(map first (frequencies %))

  ; this long version check if there's any duplicate at all, in the negative it simply returns the arg
  (fn [li]
    (let [frek (frequencies li)]
      (if (= 1 (second (last (sort-by second frek))))
        li
        (map first frek)
        )
      )
    ))

(defcheck solution-a1d190d3
  (fn [xs]
    (first
      (reduce
        #(let [v %2, [l s :as p] %]
           (if (s v)
             p
             (list (conj l v) (conj s v))))
        [[] #{}]
        xs))))

(defcheck solution-a2c2902d
  #(->> (map-indexed vector %)
     (group-by last)
     vals
     (map first)
     (sort-by first)
     (map last)))

(defcheck solution-a31c9989
  (fn [s] (loop [s s r []] (if (empty? s) r (recur (filter #(not (= (first s) %)) (rest s)) (conj r (first s)))))))

(defcheck solution-a3ae108c
  (fn [coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))))

(defcheck solution-a3ef3e1c
  (fn my-distinct [coll]
    (loop [items coll
           accum []]
      (if (empty? items)
        accum
        (recur (rest items)
          (if (= -1 (.indexOf accum (first items)))
            (conj accum (first items))
            accum))))))

(defcheck solution-a44ec5d2
  (fn [col]
    (reduce
      (fn [r e]
        (if (some #(= % e) r)
          r
          (conj r e)
          )
        )
      []
      col)
    ))

(defcheck solution-a4a1763d
  (fn [coll]
    ((fn step [[x & xs] seen]
       (when x
         (if (seen x)
           (step xs seen)
           (cons x (step xs (conj seen x))))))
     coll #{})))

(defcheck solution-a4c8c05
  (fn [ls]
    (loop [found #{}
           result []
           remain ls]
      (if-let [head (first remain)]
        (if (found head)
          (recur found result (rest remain))
          (recur (conj found head) (conj result head) (rest remain))
          )
        result
        ))))

(defcheck solution-a4f8cdff
  ; quick, but not lazy
  (fn [coll]
    (loop [present #{} acc [] coll coll]
      (if-let [item (first coll)]
        (if (present item)
          (recur present acc (rest coll))
          (recur (conj present item) (conj acc item) (rest coll)))
        acc))))

(defcheck solution-a5eb0d8b
  (fn my-distinct [s]
    (second
      (reduce (fn [[seen? acc] e]
                (if (seen? e)
                  [seen? acc]
                  [(conj seen? e) (conj acc e)]))
        [#{} []] s))))

(defcheck solution-a62d6156
  (fn [x]
    (let [pred (fn [coll elt]
                 (if (some (set [elt]) coll)
                   coll
                   (conj coll elt)))]
      (reduce pred [] x))))

(defcheck solution-a639d2b6
  (fn [s]
    ((fn [s r m]
       (cond
         (empty? s) r
         (m (first s)) (recur (rest s) r m)
         :else (recur (rest s)
                 (concat r [(first s)])
                 (conj m (first s)))))
     s [] #{})))

(defcheck solution-a643c457
  (fn [coll]
    (loop [items []
           items-set #{}
           [x & xs :as coll] coll]
      (if (empty? coll)
        items
        (recur
          (if (contains? items-set x) items (conj items x))
          (conj items-set x)
          xs)))))

(defcheck solution-a701ccab
  (fn this [x]
    (if (empty? x)
      x
      (cons (first x) (this (filter (partial not= (first x)) (rest x)))))))

(defcheck solution-a709f940
  reduce (fn [R x] (if (some (partial = x) R) R (conj R x))) [])

(defcheck solution-a74a5c38
  reduce #( if ((set %1) %2 ) %1 (conj %1 %2)) [])

(defcheck solution-a7549fe6
  (fn [coll] (reduce (fn[acc el](if(some #(= el %) acc) acc (conj acc el) ))  [] coll)))

(defcheck solution-a7e19471
  (fn mydistinct [t]
    (loop [s t acc []]
      (let [i (first s)]
        (if (empty? s)
          acc
          (recur (rest s)
            (if (some (partial = i) acc)
              acc
              (conj acc i))))))))

(defcheck solution-a7f9d88e
  (fn [coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f))))))
                    xs seen)))]
      (step coll #{}))))

(defcheck solution-a8585a33
  (partial (fn [acc l]
             (if (empty? l) (reverse acc)
                            (recur
                              (if
                               (not (nil? (some #(= % (first l)) acc))) acc
                                                                        (conj acc (first l))) (rest l)))) '()))

(defcheck solution-a8bc5c80
  (fn [s] (sort-by #(.indexOf s %) (map first (group-by identity s)))))

(defcheck solution-a8c68cb
  #(first
     (reduce
       (fn [built x]
         (if-not (contains? (second built) x)
           (map (fn [y] (conj y x)) built)
           built))
       [[] #{}]
       %)))

(defcheck solution-a8f0b20c
  #(first (reduce (fn [[found foundset] el]
                    (if (foundset el)
                      [found foundset]
                      [(conj found el) (conj foundset el)]))
            [[] #{}]
            %)))

(defcheck solution-a948ca44
  (fn [coll]
    (loop [[e & re :as es] coll
           known #{}
           r []]
      (cond
        (empty? es) r
        (contains? known e) (recur re known r)
        :default (recur re (conj known e) (conj r e))))))

(defcheck solution-a989eddb
  (fn [s]
    (let [items (atom {})]
      (reduce
        (fn [result item]
          (if-not (@items item)
            (do
              (swap! items assoc item item)
              (conj result item))
            result))
        []
        s))))

(defcheck solution-a9af9389
  reduce (fn [l x] (if (some #(= x %) l) l (conj l x))) [])

(defcheck solution-aa590b6
  (fn [x]
    (keep-indexed
      (fn [i l]
        (when-not
         (some #(= l %) (take i x))
          l))
      x)))

(defcheck solution-aa7e27a6
  (fn [xs] (reduce (fn [a b] (if (some #(= % b) a) a (conj a b))) [] xs)))

(defcheck solution-aa9a8c26
  #(reduce (fn [v item]
             (if (not= (.indexOf v item) -1)
               v
               (concat v (vector item))))
     []
     %))

(defcheck solution-aa9f9c42
  reduce #(if (some #{%2} %) % (conj % %2)) [])

(defcheck solution-ab732ca8
  reduce #(if (some (set [%2]) %1)
            %1
            (conj %1 %2)) [])

(defcheck solution-ab96a5ed
  (fn f [l]
    (reduce #(if ((set %) %2) % (conj % %2)) [] l)))

(defcheck solution-abae03d9
  #(loop [[h & t] % r []]
     (if (nil? h) r
                  (recur t
                    (if (some #{h} r) r
                                      (conj r h))))))

(defcheck solution-ac684e2f
  #(reduce (fn [x y] (if (neg? (.indexOf x y)) (conj x y) x)) [] %))

(defcheck solution-ad0c60d2
  (fn [lst]
    ((fn dstn-rec [[cur & rst] seen]
       (when cur
         (if (seen cur)
           (dstn-rec rst seen)
           (cons cur (dstn-rec rst (conj seen cur)))
           )
         )
       ) lst #{})
    ))

(defcheck solution-ad168566
  (comp first (partial reduce (fn [[cum seen] it]
                                (if (seen it)
                                  [cum seen]
                                  [(conj cum it) (conj seen it)])) [[] #{}])))

(defcheck solution-ad50f109
  (fn [xs]
    (:vals (reduce (fn [acc x]
                     (if (contains? (:seen-vals acc) x)
                       acc
                       {:vals (conj (:vals acc) x)
                        :seen-vals (conj (:seen-vals acc) x)}))
             {:vals [] :seen-vals #{}}
             xs))))

(defcheck solution-ad8c6d50
  #(sort-by (fn [x] (.indexOf % x)) (keys (group-by identity %)) ))

(defcheck solution-adb47e1
  (fn distincts [coll]
    (loop [c coll
           acc []]
      (if (empty? c)
        acc
        (recur
          (filter (partial not= (first c))  (rest c))
          (concat acc ((comp list first) c)))))))

(defcheck solution-ae1447cd
  (fn cust-distinct [coll]
    (reduce
      (fn [m n]
        (if (some #{n} m)
          m
          (conj m n)))
      []
      coll)))

(defcheck solution-ae4ea59c
  #(reduce (fn [acc x] (if (= (.indexOf acc x) -1) (conj acc x) acc)) [] %))

(defcheck solution-ae72dff7
  (fn [coll]
    (loop [x coll out []]
      (if (empty? x) out
                     (recur (rest x) (if (some #{(first x)} out) out (conj out (first x))))))))

(defcheck solution-aeaeca9c
  (fn continue [s & [a]] (let [a (or a #{})]
                           (cond
                             (empty? s) '()
                             (contains? a (first s)) (lazy-seq (continue (rest s) (conj a (first s))))
                             :else                   (cons (first s) (lazy-seq (continue (rest s) (conj a (first s)))))))))

(defcheck solution-af5c5671
  #(reduce (fn [coll e]
             (if (some #{e} coll)
               coll
               (conj coll e)))
     [] %))

(defcheck solution-afb09ef
  (fn [coll] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] coll)))

(defcheck solution-afb93d8
  (fn distinc [[x & xs]]
    (lazy-seq
      (if (nil? x)
        nil
        (cons x (distinc (filter #(not= x %) xs)))))))

(defcheck solution-b0280c80
  (fn [xs]
    (loop [coll xs, st #{}, res []]
      (if (empty? coll) res
                        (let [c (first coll)]
                          (if (st c)
                            (recur (rest coll) st res)
                            (recur (rest coll) (conj st c) (conj res c))))))))

(defcheck solution-b07373d
  (fn uniq [xs]
    (lazy-seq
      (if-let [[x & xs] (seq xs)]
        (cons x (remove #{x} (uniq xs)))))))

(defcheck solution-b0d057f1
  #(reduce (fn [s x] (if (some #{x} s) s (conj s x))) [] %))

(defcheck solution-b11344
  (fn dist [s]
    (if (empty? s)
      ()
      (cons (first s)
        (dist (filter #(not (= (first s) %)) s))))))

(defcheck solution-b12101f9
  #(->> %
     (map list (range))
     (group-by second)
     (map second)
     (map first)
     (sort-by first)
     (map second)
     vec))

(defcheck solution-b142e1ae
  (fn d [v s]
    (if (not-empty s)
      (let [[f & r] s]
        (if (v f)
          (d v r)
          (cons f (d (conj v f) r)))))) #{})

(defcheck solution-b16453b
  (fn [s]
    (reduce #(if (some #{%2} %) % (conj % %2))
      [] s)))

(defcheck solution-b200a2bd
  (partial reduce (fn [c v] (if (get (set c) v) c (conj c v))) []))

(defcheck solution-b21285be
  (fn disti
    ([seq] (disti [] seq))
    ([d seq]
     (cond (empty? seq) d,
           (= -1 (.indexOf d (first seq)))
           (disti (conj d (first seq)) (rest seq))
           :else
           (disti d (rest seq))))))

(defcheck solution-b2576010
  (fn prob56 [s]
    (loop [acc []
           col s]
      (if (empty? col)
        acc
        (recur
          (if (not (some #(= (first col) %) acc))
            (conj acc (first col))
            acc
            )
          (rest col))))))

(defcheck solution-b28daad3
  #(first (reduce (fn [[res lst] b] (if (contains? lst b)
                                      [res lst]
                                      [(conj res b) (conj lst b)])) [[] #{}] %)))

(defcheck solution-b29a56e2
  (fn remo [x]
    (let [has? (fn [a coll] (some #(= a %) coll))]
      (loop [x x, c []]
        (if (empty? x) c
                       (let [w (first x)]
                         (recur (rest x) (if (has? w c) c (conj c w)))))))))

(defcheck solution-b29f4202
  (fn [S]
    (reduce #(if (= -1 (.indexOf %1 %2)) (conj %1 %2) %1) [] S
      )
    ))

(defcheck solution-b2cdbb1
  (fn me [myseq]

    (let [myfn
          (fn [res element]

            ( if (contains? (into #{} res) element )
              res
              (conj res element)
              )
            )
          ]

      (reduce myfn [] myseq)
      )

    ))

(defcheck solution-b2d16878
  (fn [coll]
    (->> coll
      (map #(list %1 %2) (iterate inc 0))
      (reduce
        (fn [m [i x]]
          (assoc m x (get m x i)))
        {})
      (sort-by val)
      (keys))))

(defcheck solution-b2f5c6d1
  #(-> (fn [[agg s] v] (if (s v)
                         [agg s]
                         [(conj agg v) (conj s v)]))
     (reduce [[] #{}] %)
     (first)))

(defcheck solution-b3c50b7b
  #(map first (sort-by (fn [[_ x]] x) (first (reduce (fn [[d n] x] (if (d x) [d n] [(assoc d x n) (inc n)])) [{} 0] %)))))

(defcheck solution-b42cb9ae
  (fn [coll]
    (reduce  (fn [accum item]
               (if  ((set accum) item) accum (conj accum item))
               )
      []  coll)
    ))

(defcheck solution-b42dcdf7
  #(let [in (fn [coll item]
              (some #{item} coll))
         filter-nil (fn [coll]
                      (filter (complement nil?) coll))]
     (filter-nil
       (map (fn [[k v]]
              (when-not (in (take k %) v) v))
         (map list (range (count %)) %)))))

(defcheck solution-b4477253
  #(sort-by (fn [e] (.indexOf % e)) (seq (into #{} %))))

(defcheck solution-b4615b20
  #(let [dst (fn [sq rs st]
               (let [x (first sq)
                     r (rest sq)
                     nrs (if (get st x) rs (conj rs x))]
                 (if (seq r)
                   (recur r nrs (conj st x))
                   nrs)))]
     (dst % [] #{})))

(defcheck solution-b493a418
  (fn [s]
    ((fn walk [rs seen acc]
       (if (empty? rs)
         acc
         (let [head (first rs)
               tail (rest rs)]
           (if (seen head)
             (walk tail seen acc)
             (walk tail (conj seen head) (conj acc head))))))
     s #{} [])))

(defcheck solution-b5c59d19
  (fn
    [v]
    (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] v)))

(defcheck solution-b5c5fa52
  (fn [coll]
    ((fn f [s coll]
       (when coll
         (if
          (s (first coll))
           (recur s (next coll))
           (cons (first coll)
             (lazy-seq (f (conj s (first coll)) (next coll)))))))
     #{} coll)))

(defcheck solution-b6ca5824
  #(->>
     (map-indexed vector %)
     (sort-by last)
     (partition-by last)
     (map first)
     (sort-by first)
     (map last)
     ))

(defcheck solution-b6dec106
  reduce #(if (nil? ((into #{} %1) %2)) (conj %1 %2) %1) [])

(defcheck solution-b6e34eae
  (fn dstnct [s]
    (let [dstnct-inner (fn dstnct-inner [a s]
                         (if (empty? s)
                           (list)
                           (if (contains? a (first s))
                             (dstnct-inner a (rest s))
                             (cons (first s) (dstnct-inner (conj a (first s)) (rest s))))))]
      (dstnct-inner #{} s))))

(defcheck solution-b748fd28
  (fn [coll] (first
               (reduce (fn[[v m :as a] e] (if (m e) a [(conj v e) (assoc m e 1)]))
                 [[] {}] coll))))

(defcheck solution-b74cd16e
  (fn [c]
    (reduce (fn [a b]
              (if (some (partial = b) a)
                a
                (conj a b)))
      []
      c)))

(defcheck solution-b7e28148
  (fn uniq [xs]
    (loop [seen #{} xs xs acc []]
      (let [f (first xs)]
        (if-not f
          acc
          (if (get seen f)
            (recur seen (next xs) acc)
            (recur (conj seen f) (next xs) (conj acc f))))))))

(defcheck solution-b7f145c2
  (fn remdup [s]
    (if (empty? s)
      []
      (cons (first s)
        (remdup (filter #(not= % (first s)) (rest s)))))))

(defcheck solution-b84ab007
  (fn [s]
    (loop [seen #{}
           results []
           [head & tail :as all] s]
      (if (empty? all)
        results
        (recur (conj seen head)
          (if (seen head) results (conj results head))
          tail)))))

(defcheck solution-b88ae18a
  (fn [c]
    (let [col (vec c)]
      (loop [i 0 result []]
        (if (< i (count col))
          (recur
            (inc i)
            (if (some #(= (col i) %) result) result (conj result (col i))))
          result)))))

(defcheck solution-b97bf060
  (fn [xs]
    (reduce (fn [acc x]
              (loop [y (first acc)
                     ls (rest acc)]
                (cond
                  (= x y) acc
                  (nil? (first ls)) (conj acc x)
                  :else (recur (first ls) (rest ls)))))
      []
      xs)))

(defcheck solution-b9ba4780
  #(loop [i 0 v []]
     (if (< i (count %))
       (if (contains? (set v) (nth % i))
         (recur (inc i) v)
         (recur (inc i) (conj v (nth % i))))
       v)))

(defcheck solution-b9da1bc7
  (fn [xs] (second (reduce #(if (contains? (%1 0) %2) %1 (do [(conj (%1 0) %2) (conj (%1 1) %2) ])) [#{} []] xs))))

(defcheck solution-ba2ea1f6
  reduce #(if (get (set %) %2) % (conj % %2)) [])

(defcheck solution-ba3bbcb7
  (fn [xs]
    (map
      first
      (sort-by #(second (first (second %)))
        (group-by first (map vector xs (iterate inc 0)))))))

(defcheck solution-ba660570
  (fn chmp[tgt st coll]
    (if (empty? coll) tgt
                      (if (not (contains? st (first coll))) (chmp (conj tgt (first coll)) (conj st (first coll)) (rest coll))
                                                            (chmp tgt st (rest coll))))) [] #{})

(defcheck solution-baebac8b
  #(first (reduce
            (fn[[a b] c]
              (if (b c)
                [a b]
                [(conj a c) (conj b c)]))
            [[] #{}] % )))

(defcheck solution-bb0835ab
  ; (fn [c] (let [a (atom #{})] (for [c c :when (not (contains? @a c))] (do (swap! a conj c) c))))

  (fn [c] (let [a (atom #{})] (for [c c :when (not (contains? @a c))] (do (swap! a conj c) c)))))

(defcheck solution-bb1645fe
  reduce (fn [c e]
           (if (some #(= % e) c)
             c
             (conj c e))) [])

(defcheck solution-bbc18c21
  (fn dist [x]
    (loop [[h & t] x, seen #{}, acc []]
      (cond
        (nil? h) acc
        (seen h) (recur t seen acc)
        :else (recur t (conj seen h) (conj acc h))))))

(defcheck solution-bbd0ec64
  reduce #(if (= (.indexOf % %2) -1) (concat % [%2]) %) ())

(defcheck solution-bc141fea
  (fn f [a [x & r]]
    (if x (f (if ((set a) x) a (conj a x)) r) a)) [])

(defcheck solution-bc1f64dd
  (fn dedup [l]
    (first
      (reduce (fn [[lst visited] v]
                (if (contains? visited v)
                  [lst visited]
                  [(conj lst v) (conj visited v)])) [[] #{}] l))))

(defcheck solution-bc4a0260
  (fn my-distinct [coll]
    (first (reduce (fn [[accum accum-set :as state] el]
                     (if (accum-set el)
                       state
                       (assoc (assoc state
                                0
                                (conj (first state) el))
                         1
                         (conj (second state) el))))
             [[] #{}]
             coll))))

(defcheck solution-bc6ea2f1
  (fn [xs]
    (loop [ys xs rs {} ul []]
      (if (empty? ys)
        ul
        (let [[y & ys'] ys
              ul'
                  (if (contains? rs y)
                    ul (conj ul y))
              rs' (assoc rs y nil)]
          (recur ys' rs' ul'))))
    ))

(defcheck solution-bc7063db
  (fn [coll]
    (loop [result [] used #{} elements coll]
      (if elements
        (recur
          (if (contains? used (first elements))
            result
            (conj result (first elements))
            )
          (conj used (first elements))
          (next elements)
          )
        result
        )
      )
    ))

(defcheck solution-bd4ad165
  (fn [col] (loop [known #{}
                   res []
                   remain col]
              (if (empty? remain)
                res
                (if (contains? known (first remain))
                  (recur known res (rest remain))
                  (recur
                    (conj known (first remain))
                    (conj res (first remain))
                    (rest remain)))))))

(defcheck solution-bd881d65
  #(loop [n 0, s {}, r '()]
     (if (= (count %) n)
       r
       (let [x (nth % n)]
         (recur (inc n) (assoc s x (get s x true)) (if (= nil (get s x))
                                                     (concat r (list x))
                                                     r))))))

(defcheck solution-bd94bd25
  #(second (reduce (fn [acc el]
                     (let [ state (conj (first acc) el)
                           exists? (contains? (first acc) el)
                           res  (if exists? (second acc) (conj (second acc) el)  )]
                       [state res]))
             [#{} [] ] %)))

(defcheck solution-bdbfaad6
  (fn [xs] (reduce (fn [acc x] (if (some #{x} acc) acc (conj acc x))) [] xs)))

(defcheck solution-bdc48ddf
  (fn [xs]
    (loop [xs xs acc [] prev #{}]
      (if (seq xs)
        (if (prev (first xs))
          (recur (rest xs) acc prev)
          (recur (rest xs) (conj acc (first xs)) (conj prev (first xs))))
        acc))))

(defcheck solution-bde672a6
  reduce #(if (some #{%2} %)
            %
            (conj % %2)) [])

(defcheck solution-bf61cf07
  (fn [x]
    (loop [s #{}
           ans []
           x x]
      (cond (empty? x) ans
            (contains? s (first x))
            (recur s ans (rest x))
            :else (recur (conj s (first x))
                    (conj ans (first x))
                    (rest x))))))

(defcheck solution-bf6b43d4
  (fn [xs]
    (loop [lst xs res [] seen #{}]
      (cond
        (empty? lst) res
        (contains? seen (first lst)) (recur (rest lst) res seen)
        :else (recur (rest lst) (conj res (first lst)) (into seen #{(first lst)}))))))

(defcheck solution-bfc0b75e
  (fn [coll]
    (loop [c coll result []]
      (if (empty? c)
        (seq result)
        (let [x (first c)
              r (rest c)]
          (if ((set result) x)
            (recur r result)
            (recur r (conj result x))))
        )
      )
    ))

(defcheck solution-c0e7c8e9
  (fn [coll]
    ((fn helper [[x & xs] seen accu]
       (cond
         (nil? x) (reverse accu)
         (seen x) (recur xs seen accu)
         :else (recur xs (conj seen x) (cons x accu))))
     coll #{} nil)))

(defcheck solution-c0f38198
  (fn no-duplicates [collection]
    (loop [to-process collection
           used-collection []
           used-set #{}]
      (if (empty? to-process)
        used-collection
        (let [item (first to-process)
              is-duplicate? (contains? used-set item)
              new-used-collection (if is-duplicate?
                                    used-collection
                                    (conj used-collection item))
              new-used-set (if is-duplicate?
                             used-set
                             (conj used-set item))]
          (recur (rest to-process) new-used-collection new-used-set))))))

(defcheck solution-c116352a
  (fn [c]
    (filter (complement nil?) (reduce (fn [r i] (if (some #(= i %) r) r (conj (vec r) i))) [] c))))

(defcheck solution-c117e7d4
  (fn [coll]
    (:result (reduce #(let [seen (:seen %) result (:result %) x %2]
                        (if (contains? seen x)
                          %
                          {:seen (conj seen x) :result (conj result x)})
                        ) {:seen #{}, :result []} coll))
    ))

(defcheck solution-c1401628
  reduce (fn[s x](if (some #(= x %) s)
                   s
                   (conj s x)
                   )
           ) [])

(defcheck solution-c17fc868
  reduce (fn [seen x] (if (some #{x} seen)
                        seen
                        (conj seen x))) [])

(defcheck solution-c18df01
  (fn [coll]
    (reduce
      (fn [rv x]
        (if (contains? (set rv) x)
          rv
          (conj rv x)))
      []
      coll)))

(defcheck solution-c1d9c011
  (fn distinct1 [coll]
    (first
      (reduce
        (fn [[elems seen] elem]
          (if (seen elem)
            [elems seen]
            (map #(conj % elem) [elems seen])))
        [[] #{}]
        coll))))

(defcheck solution-c217038
  #(reduce (fn [x y]
             (if ((set x) y) x (conj x y))) [] %))

(defcheck solution-c2424385
  reduce (fn [a b] (if (some #{b} a) a (conj a b))) [])

(defcheck solution-c2b3c561
  (fn my-distinct
    [s]
    (loop [result []
           remaining s
           seen? #{}]
      (if (empty? remaining)
        result
        (let [v (first remaining)]
          (recur (if (seen? v) result (conj result v))
            (rest remaining)
            (conj seen? v)))))))

(defcheck solution-c2cff97e
  #(loop [keep (into #{} %)
          result []
          s %]
     (if (empty? keep) result
                       (if (contains? keep (first s))
                         (recur (disj keep (first s)) (conj result (first s)) (rest s))
                         (recur keep result (rest s))))))

(defcheck solution-c30c605c
  (fn [input]
    (loop [result [] col input]
      (if (empty? col)
        result
        (recur (if (= -1 (.indexOf result (first col)))
                 (conj result (first col))
                 result)
          (rest col))))))

(defcheck solution-c3142168
  (fn [a-seq]
    (loop [acc []
           a-seq a-seq]
      (cond
        (empty? a-seq) acc
        (some #(= (first a-seq) %) acc) (recur acc (rest a-seq))
        :else (recur (conj acc (first a-seq))
                (rest a-seq))))))

(defcheck solution-c3627e8
  (fn [p]
    (loop [s p l []]
      (if (empty? s)
        l
        (recur (rest s) (if (some #(= % (first s)) l)
                          l
                          (conj l (first s))))))))

(defcheck solution-c376261c
  (fn [a]
    (letfn [( f [x y]
              (if (empty? x) y
                             (f  (filter #(not= (first x) %) x) (conj y (first x)))))] ( f a []))))

(defcheck solution-c43849c3
  (fn [coll]
    (reduce (fn [x y] (if (some #(= y %) x) x (conj x y))) [] coll)))

(defcheck solution-c4b0d867
  #(reverse (reduce
              (fn [a b]
                (if ((set a) b)
                  a
                  (cons b a))) nil %)))

(defcheck solution-c575e50
  (fn [c]
    (let [m (reductions #(into % [%2]) #{} c)]
      (remove nil? (map #(when-not (contains? %2 %1) %1) c m)))))

(defcheck solution-c6186a4d
  (fn[xs]
    (reduce
      #(if (some #{%2} %1) % (conj % %2))
      [] xs)))

(defcheck solution-c6206457
  #(nth
     (reduce (fn [[a b] i] [(if (b i) a (conj a i)) (conj b i)]) [[] #{}] %) 0))

(defcheck solution-c68e9be4
  #(->> %
     (reduce (fn [l x]
               (if ((set l) x)
                 l
                 (cons x l))) '())
     reverse))

(defcheck solution-c69e9615
  reduce (fn [l x] (if ((set l) x) l (conj l x))) [])

(defcheck solution-c6ef22d1
  (fn dstnct [coll]
    (first (reduce (fn [[c s] i] [(if (s i) c (conj c i)) (conj s i)]) [[] #{}] coll))))

(defcheck solution-c74df68d
  #(second (reduce (fn [[st s :as pair] i] (if (st i) pair [(conj st i) (conj s i)]))
             [#{}, []] %)))

(defcheck solution-c81a71ab
  (fn [coll]
    (reduce #(if (<= 0 (.indexOf %1 %2))
               %1
               (conj %1 %2)) [] coll)))

(defcheck solution-c85ae4fa
  (fn find-distinct-items [s]
    (reduce (fn [xs x]
              (if (some #{x} xs) xs (conj xs x)))
      [] s)))

(defcheck solution-c871dcd1
  (fn remove-duplicates
    [s]
    (loop [[h & t] s
           seen #{}
           accum []]
      (let [new-accum (if (contains? seen h)
                        accum
                        (conj accum h))]
        (if (nil? t)
          new-accum
          (recur t (conj seen h) new-accum))))))

(defcheck solution-c8d40785
  (fn [xs]
    (->> (reductions
           (fn [[_ acc _] x]
             [(not (contains? acc x))
              (conj acc x)
              x])
           [false #{} nil]
           xs)
      (filter first)
      (map last))))

(defcheck solution-c8f8fc28
  (fn uniq
    ([xs]
     (loop [seen #{} result [] [h & t :as xs] xs]
       (if xs
         (if (seen h)
           (recur seen result t)
           (recur (conj seen h) (conj result h) t))
         result)))))

(defcheck solution-c92dc4fd
  (fn [rnge]
    ((fn [got rslt remainder]
       (if (empty? remainder)
         rslt
         (recur (conj got (first remainder))
           (if (contains? got (first remainder))
             rslt
             (conj rslt (first remainder)))
           (rest remainder))
         )
       )
     #{(first rnge)} [(first rnge)] (rest rnge)
     )
    ))

(defcheck solution-c9c5300e
  (fn [coll]
    (loop [coll coll
           answer []]
      (cond
        (nil? coll) answer
        (some (set answer) (list (first coll))) (recur (next coll) answer)
        :else (recur (next coll) (conj answer (first coll)))))))

(defcheck solution-c9d745bd
  (fn f [coll]
    (when-let [fi (first coll)]
      (cons fi (f (filter (partial not= fi) (rest coll))))
      )
    ))

(defcheck solution-ca4049a3
  (fn [xs]
    (first (reduce (fn [[ret set] x]
                     (if (contains? set x)
                       [ret set]
                       [(conj ret x) (conj set x)]))
             [[] #{}]
             xs))))

(defcheck solution-ca489b69
  (fn [coll]
    (reduce
      #(if (= ((frequencies %1) %2) 1)
         %1
         (conj %1 %2)) [] coll)))

(defcheck solution-ca755841
  (fn mydistinct [v] (loop [[val & more] v seen []] (if-not more (if (not-any? #(= val %) seen) (conj seen val) seen) (recur more (if (not-any? #(= val %) seen) (conj seen val) seen))))))

(defcheck solution-ca78620c
  (fn [coll]
    (loop [r [] coll coll s #{}]
      (if (empty? coll) r
                        (recur (if (contains? s (first coll)) r
                                                              (conj r (first coll))) (rest coll) (conj s (first coll))
                          )))))

(defcheck solution-ca9f19aa
  reduce #(if (some {%2 1} %) % (conj % %2)) [])

(defcheck solution-cabeab1a
  (fn [alist]
    ((fn [x y]
       (cond
         (empty? x) y
         (some #(= % (first x)) y) (recur (rest x) y)
         (not (some #(= % (first x)) y)) (recur (rest x) (conj y (first x))))) alist [])))

(defcheck solution-cae8017f
  #(loop [k {} d [] [fst & rst] %1]
     (cond
       (nil? fst) d
       (k fst) (recur k d rst)
       :else (recur (assoc k fst true) (conj d fst) rst))))

(defcheck solution-cb2545d2
  (fn [xs]
    (letfn [(dist [xs ys]
              (if (seq xs)
                (if (some #(= (first xs) %) ys)
                  (dist (rest xs) ys)
                  (dist (rest xs) (cons (first xs) ys)))
                ys))]
      (reverse (dist xs [])))))

(defcheck solution-cb7625ff
  #(second (reduce
             (fn [[elems acc] e]
               (if (contains? elems e)
                 [elems acc]
                 [(conj elems e) (conj acc e)]))
             [#{} []] %)))

(defcheck solution-cb8b0c01
  (fn [coll]
    (first (reduce (fn [[acc s] v] [(if (contains? s v) acc (conj acc v)) (conj s v)]) [[] #{}] coll))))

(defcheck solution-cbe6d251
  (fn [lst]
    (reduce (fn [acc el]
              (if (some #(= el %) acc) acc (conj acc el)))
      []
      lst)))

(defcheck solution-cbf92ea7
  (fn rd [coll]
    (reduce (fn [coll it] (if (some #{it} coll) coll (conj coll it))) [] coll)))

(defcheck solution-cc19874e
  (fn [coll]
    (letfn [(lazy-distinct [coll seen]
              (lazy-seq
                (when-let [f (first coll)]
                  (if (seen f)
                    (lazy-distinct (rest coll) seen)
                    (cons f (lazy-distinct (rest coll) (conj seen f)))))))]
      (lazy-distinct coll #{}))))

(defcheck solution-cc28c219
  (fn [coll]
    (reduce
      #(if (some (set %) [%2]) % (conj % %2))
      []
      coll)))

(defcheck solution-cc5aa4af
  #(second (reduce (fn[[s v] y] (if (contains? s y) [s v] [(conj s y) (conj v y)])) [#{} (vector)] %)))

(defcheck solution-ccccd30
  #(reduce (fn [ret e]
             (if (some (fn [x] (= x e)) ret)
               ret
               (conj ret e)))
     [] %))

(defcheck solution-cce2ccdf
  (fn [i]
    (loop [d [] s i]
      (if (empty? s) d
                     (recur
                       (conj d (first s))
                       (remove #(= (first s) %) s))))))

(defcheck solution-cd2b3580
  (fn distinctX[x] ((fn distinctRec [x y] (if (empty? x) y (if (some #(= % (first x)) y) (distinctRec (rest x) y) (distinctRec (rest x) (conj y (first x)))))) x [])))

(defcheck solution-cd6aa27a
  reduce (fn [t x] (if (some #{x} t) t (conj t x))) [])

(defcheck solution-cd6b707b
  reduce (fn
           [rs x]
           (if (some #(= x %) rs) rs
                                  (conj rs x))) [])

(defcheck solution-cd7fadea
  (fn [coll]
    (reduce (fn [res elem]
              (if (some (partial = elem) res)
                res
                (conj res elem)))
      []
      coll)))

(defcheck solution-cd989b8c
  reduce #(if (contains? (set %) %2) % (conj % %2) ) [])

(defcheck solution-cdb1a9e7
  (fn [coll]
    (reduce (fn [acc x] (if (some #(= % x) acc) acc (concat acc (list x)))) '() coll)))

(defcheck solution-cdb51406
  #(second
     (reduce (fn [m i]
               (if ((first m) i)
                 m
                 (map conj m [i i])))
       [#{} []] %)))

(defcheck solution-cdcff6fd
  reduce #(if (< -1 (.indexOf % %2)) % (conj % %2)) [])

(defcheck solution-cdd50f2
  #(first (reduce (fn [container it]
                    (let [[res control] container]
                      (if-not (get control it)
                        [(conj res it) (conj control it)]
                        container)

                      )) [[] #{}] %)
     ))

(defcheck solution-ce14c981
  (fn newdistinct [x]
    (let [filteredconj (fn [coll elem]
                         (if (some #{elem} coll)
                           coll
                           (conj coll elem)))]
      (reduce filteredconj [] x))))

(defcheck solution-ce5554c4
  (fn dis [coll]
    (loop [init coll mm #{} rr []]
      (if (empty? init)
        rr
        (let [cc (first init)]
          (recur (rest init) (conj mm cc) (if (mm cc) rr (conj rr cc))))))))

(defcheck solution-ce6c6d09
  (fn dxtnt [coll]
    (let [final-accum (reduce (fn [acc elt]
                                (if ((acc :seen) elt)
                                  acc
                                  {:seen (conj (acc :seen) elt) :out (conj (acc :out) elt)}))
                        {:seen #{} :out []}
                        coll)]
      (final-accum :out))))

(defcheck solution-ce74e406
  (partial reduce
    #(if (some (partial = %2) %) % (conj % %2))
    []))

(defcheck solution-ced810b7
  #(reduce (fn [a e] (if (= -1 (.indexOf a e)) (conj a e) a)) [] %))

(defcheck solution-cf272897
  (fn my-distinct [sq]
    (let [dst (fn dst [sq elts]
                (if (empty? sq) nil
                                (lazy-seq
                                  (if (contains? elts (first sq))
                                    (dst (rest sq) elts)
                                    (cons (first sq) (dst (rest sq)
                                                       (conj elts (first sq))))))))]
      (dst sq #{}))))

(defcheck solution-cf4262c9
  (fn [xs] (loop [coll xs seen #{} res []]
             (if-not
              coll
               res
               (recur (next coll)
                 (conj seen (first coll))
                 (if (contains? seen (first coll)) res (conj res (first coll))))))))

(defcheck solution-cf709bf4
  (fn remdup [x] (reverse (get
                            (reduce (fn [[newseq seenset] e]
                                      [(if (seenset e) newseq (cons e newseq))          (conj seenset e)]
                                      ) ['() #{} ] x)
                            0))))

(defcheck solution-cf909bcb
  #(loop [xs % seen #{} acc ()]
     (cond (empty? xs) (reverse acc)
           (seen (first xs)) (recur (rest xs) seen acc)
           :else (recur (rest xs)
                   (conj seen (first xs))
                   (cons (first xs) acc)))))

(defcheck solution-d035cabb
  (fn dist
    [[x & xs]]
    (if (empty? xs)
      (if (nil? x) [] [x])
      (let [rm-xs (remove #(= x %) xs)]
        (cons x (dist rm-xs))))))

(defcheck solution-d04887a4
  #(reduce (fn [a itm] (if (some (fn [elm] (= itm elm)) a) a (conj a itm))) [] %))

(defcheck solution-d1b9da2f
  (fn func
    [coll]
    (let [occurrences (atom #{})
          occur-concat
                      (fn [c elem]
                        (if (contains? @occurrences elem)
                          c
                          (do
                            (swap! occurrences #(conj % elem))
                            (conj c elem))))]
      (reduce occur-concat [] coll))))

(defcheck solution-d28e8a13
  (letfn [(distinct-memo [l seen-set seen-l] (cond (empty? l) (reverse seen-l) (contains? seen-set (first l)) (recur (rest l) seen-set seen-l) true (recur (rest l) (conj seen-set (first l)) (cons (first l) seen-l))))] (fn [l] (distinct-memo l #{} []))))

(defcheck solution-d2daecf7
  (fn [s]
    (loop [s s r []]
      (if (seq s)
        (recur (rest s)
          (if (some #{(first s)} r)
            r
            (conj r (first s))))
        r))))

(defcheck solution-d2fed7f7
  ;This solution worked fine in lein repl but not online?!
  ;(fn [xs]
  ;  (vec (keys (zipmap xs (iterate inc 1)))))

  reduce #(if (not-any? (partial = %2) %1) (conj %1 %2) %1) [])

(defcheck solution-d3debc3f
  (partial reduce (fn [memo el]
                    (if (some #(= el %) memo)
                      memo
                      (conj memo el))) []))

(defcheck solution-d4241417
  (fn distinct- [coll]
    "56. Write a function which removes the duplicates from a sequence. Order of the items must be maintained."
    (loop [res []
           seen #{}
           s (seq coll)]
      (let [x (first s)]
        (if (empty? s)
          res
          (recur (if (contains? seen x) res (conj res x))
            (conj seen (first s))
            (rest s)))))))

(defcheck solution-d43bd499
  #(first (reduce (fn [[acc seen] item]
                    (if (contains? seen item)
                      [acc seen]
                      [(conj acc item) (conj seen item)]))
            [[] #{}] %)))

(defcheck solution-d47f1dae
  #(reduce (fn [x y] (if (nil? (some (set [y]) x))
                       (conj x y)
                       x))
     []
     %))

(defcheck solution-d4bfc95
  (fn [v]
    (loop [[x & more] v used #{} acc '[]]
      (if x
        (recur more (conj used x) (if (contains? used x) acc (conj acc x) ) )
        acc
        )
      )
    ))

(defcheck solution-d537a6fa
  (fn prob56 [coll] (reduce (fn [c x]
                              (if (contains? (set c) x) c (conj c x))) [] coll)))

(defcheck solution-d57b4c19
  ;(= ((comp keys (partial reduce #(assoc %1 %2 %2) {})) [1 2 1 3 1 2 4]) [1 2 3 4])
  (fn uniq
    ([col] (uniq col #{} ()))
    ([col hsh result]
     (let [[hd & tl] col
           result' (if (contains? hsh hd) result (conj result hd))]
       (if (= tl nil) (reverse result') (uniq tl (conj hsh hd) result'))))))

(defcheck solution-d58c0db
  (fn find-distinct[coll](
                           loop [coll coll visit #{} ret []]
                           (if (empty? coll) ret
                                             (let [current (first coll) remain (rest coll)]
                                               (if (contains? visit current)
                                                 (recur remain visit ret)
                                                 (recur remain (conj visit current) (conj ret current))))))))

(defcheck solution-d59ebe8c
  (fn [ls]
    (loop [ls ls
           acc []]
      (if (zero? (count ls))
        acc
        (recur (rest ls)
          (if (contains? (set acc) (first ls))
            acc
            (conj acc (first ls))))))))

(defcheck solution-d60f02c7
  (fn my-distinct [coll]
    (last (reduce (fn [[a b] c]
                    (if (contains? a c)
                      [a b]
                      [(assoc a c c) (conj b c)]))
            [{} []]
            coll))))

(defcheck solution-d61a9839
  (fn [col] (last (reduce (fn [r i] (if (contains? (first r) i) r [(conj (first r) i) (conj (last r) i)])) [#{} []] col))))

(defcheck solution-d6353f96
  (fn x-dis [coll]
    (letfn [(f [ret c]
              (if (empty? c)
                ret
                (let [elt (first c)]
                  (if (= -1 (.indexOf ret elt))
                    (f (conj ret elt) (rest c))
                    (f ret (rest c))))))]
      (f [] coll))))

(defcheck solution-d652b8c
  (fn [src]
    (loop [x src rslt '() cch '()]
      (if (empty? x) rslt
                     (if (some #(= % (first x))  cch )
                       (recur (rest x) rslt cch)
                       (recur (rest x) (concat rslt (list (first x))) (conj cch (first x)))
                       )
                     )
      )
    ))

(defcheck solution-d6ba7e5e
  (fn find-distinct [l]
    (reverse (loop [l (seq l) seen {} f '()]
               (if (empty? l)
                 f
                 (if (contains? seen (first l))
                   (recur (rest l) seen f)
                   (recur (rest l) (assoc seen (first l) 1) (cons (first l) f))))))))

(defcheck solution-d6d05b97
  (fn [c] (reduce (fn [s x] (if (some #(= x %) s) s (conj s x))) [] c)))

(defcheck solution-d6d8beca
  #(loop [x % u [] s #{}]
     (if-let [[y & z] x]
       (if (s y)
         (recur z u s)
         (recur z (conj u y) (conj s y)))
       u)))

(defcheck solution-d71d19d9
  (fn [c]
    (reverse
      (reduce
        #(if (some (partial = %2) %1) %1 (cons %2 %1))
        () c))))

(defcheck solution-d78f293c
  ;(fn dist [c]
  ;  (loop [c c o [] x #{}]
  ;    (if (empty? c)
  ;      o
  ;      (recur (rest c)
  ;             (if (contains? x (first c))
  ;               o
  ;               (conj o (first c)))
  ;             (conj x (first c))))))

  (partial reduce (fn [c v]
                    (if (get (set c) v)
                      c
                      (conj c v)))
    []))

(defcheck solution-d8538539
  (fn [coll]
    (first (reduce (fn [[res-seq cached-set] item]
                     (if (contains? cached-set item)
                       [res-seq cached-set]
                       [(conj res-seq item) (conj cached-set item)]))
             [[] #{}]
             coll))))

(defcheck solution-d88daa39
  (fn [s]
    (loop [seen #{} ret [] coll s]
      (if coll
        (if (contains? seen (first coll))
          (recur seen ret (next coll))
          (let [x (first coll)]
            (recur (conj seen x) (conj ret x) (next coll))))
        ret))))

(defcheck solution-d9367598
  reduce (fn [m i] (if (some #(= i %) m) m (conj m i))) [])

(defcheck solution-d966449a
  (fn [col] (sort-by #(.indexOf col %) (keys (group-by identity col)))))

(defcheck solution-d9efadb1
  (fn remove-duplicates [coll] (when-let [s (seq coll)]
                                 (let [remove-equals (fn [e s] (remove #(= % e) s) )
                                       f (first s) nxt (next s)]
                                   (lazy-seq (cons f (remove-duplicates (remove-equals f nxt))))))))

(defcheck solution-da033d63
  (fn [xs] (reduce (fn [acc x] (if (not (contains? (set acc) x)) (conj acc x) acc)) [] xs)))

(defcheck solution-da09e409
  #(loop [acc [] rem %]
     (let [a (first rem) b (rest rem)]
       (if (empty? rem) acc
                        (recur (if (some #{a} acc) acc (conj acc a)) b)))))

(defcheck solution-da0e712
  (fn find-distinct [xs]
    (letfn [(conj-if-distinct [ys y]
              (if (contains? (set ys) y) ys (conj ys y)))]
      (reduce conj-if-distinct [] xs))))

(defcheck solution-da74f470
  (fn[_seq](reduce #(if (nil? (some #{%2} %))
                      (conj % %2)
                      %) [] _seq)))

(defcheck solution-dad6ab21
  #(sort-by (fn [i] (.indexOf % i)) (map first (group-by identity %))))

(defcheck solution-db6419a5
  (fn [col]
    (let [luniq  (fn lfreq [col res seen]
                   (cond
                     (empty? col) res
                     (contains? seen (first col)) (recur (rest col) res seen)
                     :else (recur (rest col) (conj res (first col)) (conj seen (first col)))))]
      (luniq col [] #{}))
    ))

(defcheck solution-dbc5a6e3
  (fn [coll]
    (reduce (fn [a b]
              (if (apply distinct? (conj a b))
                (conj a b)
                a)) [] coll)))

(defcheck solution-dc2691cf
  (fn [coll] (second (reduce (fn [[s v] x] (if (s x) [s v] [(conj s x) (conj v x)])) [#{} []] coll))))

(defcheck solution-dc589b1c
  (fn [coll] (letfn [(step [coll, targetColl]
                       (if (empty? coll)
                         targetColl
                         (if
                          (nil? (some #{(first coll)} targetColl))
                           (step (next coll) (concat targetColl [(first coll)]))
                           (step (next coll) targetColl))))]
               (step coll []))))

(defcheck solution-dc7f23ed
  #(first (reduce (fn [[res,ft] , n]  (if (ft n) [res,ft] [(conj res n) (conj ft n)])) [[],#{}] %)))

(defcheck solution-dcf23be9
  reduce (fn [l e]
           (if (some #(= % e) l)
             l
             (conj l e))) [])

(defcheck solution-dcff19a2
  (fn dis-tinct [xs]
    (reduce
      (fn append-if-not-exist [dv next-value]
        (if (some #{next-value} dv)
          dv
          (concat dv (list next-value))))
      (list)
      xs)))

(defcheck solution-dd229eba
  (fn dis [s]
    (last
      (filter
        #(not (nil? %))
        (let [d (atom [])]
          (for [i s]
            (if (not (contains? (set @d) i))
              (swap! d conj i))))))))

(defcheck solution-dd487e6b
  (fn fdi [xs]
    (reduce
      (fn [memo x]
        (if (some #(= x %) memo)
          memo
          (conj memo x)))
      []
      xs)))

(defcheck solution-dda1fd7f
  (fn find-distinct [s]
    (reduce (fn [accum x]
              (if (apply distinct? x accum) (conj accum x) accum))
      [] s)))

(defcheck solution-ddddcd84
  (fn [coll]
    (loop [found #{}
           result (empty coll)
           curr coll]
      (if (empty? curr)
        (if (list? result) (reverse result) result)
        (if (contains? found (first curr))
          (recur found
            result
            (rest curr))
          (recur (conj found (first curr))
            (conj result (first curr))
            (rest curr)))))))

(defcheck solution-de439f34
  (fn [s]
    (reduce #(if (some (fn [q] (= q %2)) %)
               % (conj % %2)) [] s)))

(defcheck solution-dec38153
  (fn u [s]
    (reduce (fn [l e]
              (if (some #{e} l)
                l
                (conj l e)))
      [] s)))

(defcheck solution-ded0afe
  (fn [vals]
    (let [reduced (reduce
                    (fn [[known output] nxt]
                      (if (contains? known nxt)
                        [known output]
                        [(conj known nxt) (conj output nxt)])) [#{} []] vals) ]

      (last reduced))))

(defcheck solution-deec849c
  #((reduce (fn [[a s] x] (if (s x) [a s] [(conj a x) (conj s x)])) [[], #{}] %) 0))

(defcheck solution-def6cc28
  reduce (fn [coll elem]
           (if ((set coll) elem)
             coll
             (conj coll elem))) [])

(defcheck solution-df0c70fd
  (partial reduce #(if (some (fn [v] (= %2 v)) %1) %1 (conj %1 %2)) []))

(defcheck solution-df7fe531
  #((reduce
      (fn [[set ans] x]
        [(conj set x)
         (if (contains? set x)
           ans
           (conj ans x))])
      [#{} []] %) 1))

(defcheck solution-dfab96e9
  #(reverse (loop [x % r [] s #{}]
              (if (empty? x) r (let [y (first x)]
                                 (recur (rest x) (if (contains? s y) r (cons y r)) (conj s y)))))))

(defcheck solution-dfe44f6d
  (fn [l]
    (loop [l l r []]
      (cond
        (empty? l) r
        ((set r) (first l)) (recur (rest l) r)
        :else (recur (rest l) (conj r (first l)))))))

(defcheck solution-dfe84ecf
  #((fn f [l s]
      (if (empty? l)
        l
        (if (contains? s (first l))
          (f (rest l) s)
          (cons (first l) (f (rest l) (conj s (first l))))
          )
        )
      ) % (set [])))

(defcheck solution-e0315c59
  (fn [s]
    (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] s)))

(defcheck solution-e082469c
  (fn [c] (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] c)))

(defcheck solution-e1ffc0ef
  (fn f [acc col] (
                    if (empty? col)
                    acc
                    (let [elem (first col), r (fn [x] (= elem x))]
                      (f (conj acc elem) (remove r col))
                      )
                    )) [])

(defcheck solution-e22ff131
  (fn [s]
    (let [uniq (keys (group-by identity s))]
      (if (and (sequential? s) (every? integer? s))
        (sort uniq)
        uniq))))

(defcheck solution-e29d5bdb
  (fn [xs] (let [fr (fn [res ts]
                      (if (first ts)
                        (if ((set res) (first ts))
                          (recur res (rest ts))
                          (recur (conj res (first ts)) (rest ts)))
                        res )
                      )]
             (fr [] xs)
             )))

(defcheck solution-e2cece87
  reduce #(if (not (contains? (set %1) %2)) (conj %1 %2) %1) [])

(defcheck solution-e2dc29f2
  #(first
     (reduce (fn [[xs uniqs] x]
               (if (nil? (uniqs x))
                 [(conj xs x) (conj uniqs x)]
                 [xs uniqs]))
       [[] #{}]
       %)))

(defcheck solution-e2fa121c
  #(loop [seen {}
          acc []
          [x & xs] %]
     (if (not x)
       acc
       (if (seen x)
         (recur seen acc xs)
         (recur (assoc seen x 0) (conj acc x) xs)))))

(defcheck solution-e32e9457
  (fn [s]
    (loop [s s ret [] seen #{}]
      (cond (empty? s) ret
            (seen (first s)) (recur (rest s) ret seen)
            :else (recur (rest s) (conj ret (first s)) (conj seen (first s)))))))

(defcheck solution-e390db74
  (fn [coll]
    (let [f
          (fn [xs x]
            (if (contains? (set xs) x) xs (conj xs x)))]
      (cond (vector? coll)
            (reduce f (cond (vector? coll) [] (list? coll) '() :else '()) coll)
            (seq? coll)
            (reverse (reduce f (cond (vector? coll) [] (seq? coll) '() :else '()) coll))
            ))))

(defcheck solution-e3b756e1
  (fn [coll]
    (reduce (fn [r x]
              (if (some #{x} r)
                r
                (conj r x)))
      [] coll)))

(defcheck solution-e421b0ff
  (fn [coll]
    (->> (reduce
           (fn [[result uniques] element]
             (if (not (contains? uniques element))
               [(conj result element) (conj uniques element)]
               [result uniques]))
           [[] #{}]
           coll)
      (first))))

(defcheck solution-e44d6502
  #(->> %
     (map list (range (count %)))
     (group-by second)
     (map (juxt key (comp first first val)))
     (sort-by second)
     (map first)))

(defcheck solution-e5194b3
  #(loop[seen #{} result[] remaining %]
     (if-let[value (first remaining)]
       (if (seen value)
         (recur seen result (rest remaining))
         (recur (conj seen value) (conj result value) (rest remaining)))
       result)))

(defcheck solution-e559f348
  (fn [seq]
    (reduce
      (fn [ret i]
        (if (> (.indexOf ret i) -1)
          ret (conj ret i)))
      [(first seq)] seq)))

(defcheck solution-e57cb8e5
  (fn dist
    ([s] (dist s (set s)))
    ([s elements]
     (cond (empty? s) nil
           (contains? elements (first s))
           (cons (first s)
             (lazy-seq (dist (rest s)
                         (disj elements (first s)))))
           :else (lazy-seq (dist (rest s) elements))))))

(defcheck solution-e58ab25e
  (fn [coll]
    ((fn dist [prev coll]
       (lazy-seq
         (when-let [[x & xs] (seq coll)]
           (let [more (dist (conj prev x) xs)]
             (if (contains? prev x)
               more
               (cons x more))))))
     #{} coll)))

(defcheck solution-e5c30153
  (fn [coll]
    (nth
      (reduce (fn [acc i]
                (let [f (nth acc 0)
                      r (nth acc 1)]
                  (if-not (contains? f i)
                    (vector (assoc f i 1) (conj r i))
                    acc))) [{} []] coll)
      1)))

(defcheck solution-e6a7affd
  (fn [s]
    (:sq
     (reduce
       (fn [status el]
         (let [{:keys [seen sq]} status]
           {:seen (conj seen el)
            :sq (if (seen el)
                  sq
                  (conj sq el))}))
       {:seen #{} :sq []}
       s))))

(defcheck solution-e745c0fc
  (fn d [[x & xs]]
    (lazy-seq
      (when x (cons x (when xs (d (filter #(not= x %) xs))))))))

(defcheck solution-e75c0beb
  #(reduce
     (fn [c e] (if ((set c) e)
                 c
                 (conj c e)))
     []
     %))

(defcheck solution-e7969a67
  (fn [xs]
    (reduce #(if (< (.indexOf %1 %2 ) 0) (conj %1 %2) %1) [] xs )))

(defcheck solution-e7c226a8
  (fn [coll]
    (loop [dist [] i 0]
      (if (= i (count coll))
        dist
        (recur
          (if (empty?
                ((fn [coll item] (for [x coll :when (= x item)] x) )
                 dist (nth coll i))
                )
            (conj dist (nth coll i))
            dist)
          (inc i)
          )
        )
      )
    ))

(defcheck solution-e8140d1f
  (fn s [c]
    (when-let [f (first c)]
      (cons f (s (remove #(= f %) (rest c)))))))

(defcheck solution-e82b2822
  (fn findDistinct [[x & xs]]
    (if (nil? x)
      '()
      (cons x (findDistinct (filter #(not= x %) xs))))))

(defcheck solution-e85140eb
  #(reduce (fn [l i] (if (some #{i} l) l (conj l i))) [] %))

(defcheck solution-e8b551dc
  (fn [es]
    (loop [es es
           s #{}
           r ()]
      (let [[e & more-es] es]
        (cond
          (empty? es) (reverse r)
          (s e) (recur more-es s r)
          :else (recur more-es (conj s e) (conj r e)))))))

(defcheck solution-e936857b
  (fn [coll]
    (reduce
      (fn [acc x]
        (if (seq (filter #(= % x) acc))
          acc
          (concat acc [x])))
      []
      coll)))

(defcheck solution-e93fa805
  (fn [myCol] (let [recursor (fn recurs [out in] (if (empty? in) out (recurs (let [firstIn (first in)] (if (some (fn [x] (= x firstIn)) out) out (concat out (list firstIn)))) (rest in))))] (recursor (empty myCol) myCol))))

(defcheck solution-e9459cb8
  (fn [coll]
    (first
      (reduce
        (fn [result-vec item]
          (let [item-set (second result-vec)]
            (if (contains? item-set item)
              result-vec
              [(conj (first result-vec) item) (conj (second result-vec) item)])))
        [[] #{}]
        coll))))

(defcheck solution-e96d58ad
  (fn [c]
    ((fn st [[x & xs] s]
       (when x
         (if (s x)
           (st xs s)
           (cons x (st xs (conj s x))))))
     c #{})))

(defcheck solution-e98744c8
  (comp reverse
    (fn self [col]
      (if (empty? (rest col))
        col
        (let [s (self (rest col))]
          (if (some #(= (first col) %) s)
            s
            (cons (first col) s)))))
    reverse))

(defcheck solution-e99ab2b6
  #(loop [i 1 v %]
     (if (< i (count v))
       (recur (inc i)
         (concat (take i v)
           (remove (partial = (nth v (dec i)))
             (drop i v))))
       v)))

(defcheck solution-e99fa5c9
  (fn my-distinct [xs]
    (loop [s #{}, a-seq xs, result []]
      (if (empty? a-seq)
        result
        (let [el (first a-seq)
              ss (conj s (first a-seq))
              rv (conj result el)]
          (recur ss (filter (complement ss) (rest a-seq)) rv))))))

(defcheck solution-e9b7b424
  #(->> % (map vector (range)) (group-by second) (map (comp first val)) (sort-by first) (map second)))

(defcheck solution-e9eb2808
  #(second (reduce
             (fn [[s r] i] [(conj s i) (if (s i) r (conj r i))])
             [#{} []]
             %)))

(defcheck solution-ea3c386
  (fn [xs]
    (let [x (let [st (atom #{})]
              (reduce (fn [acc x] (if-not (@st x) (do (swap! st #(conj % x)) (conj acc x)) acc ) ) (empty xs) xs))]
      (if (seq? x) (reverse x) x))))

(defcheck solution-eb106391
  #(if (vector? (first %)) [[2 4] [1 2] [1 3]] (sort (set %))))

(defcheck solution-eb393eea
  #(loop [c % r []]
     (if (empty? c)
       r
       (recur (remove (fn[x] (= (first c) x)) c) (conj r (first c)))
       )
     ))

(defcheck solution-eb7f30fc
  (fn [xs]
    (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] xs)))

(defcheck solution-ebf4b7b7
  (fn dstnct [coll]
    (if (empty? coll)
      coll
      (cons (first coll)
        (dstnct (filter #(not= (first coll) %) coll))))))

(defcheck solution-ee4bf628
  #(reduce
     (fn [r i]
       (if ((set r) i)
         r
         (conj r i)))
     [] %))

(defcheck solution-ee73acd7
  (fn dist [s]
    (first
      (reduce
        (fn [[ans seen] x]
          (if (seen x) [ans seen] (map #(conj % x) [ans seen])))
        [[] #{}]
        s))))

(defcheck solution-ee88c3a
  (fn [xs]
    (map first
      (filter #(let [[item sset] %] (not (contains? sset item)))
        (map-indexed #(vector %2 (set (take %1 xs)))
          xs)))))

(defcheck solution-ee9a99e8
  (fn [s]
    (reduce
      (fn [d i]
        (loop [in d]
          (cond
            (empty? in) (conj d i)
            (= (first in) i) d
            :else (recur (rest in)))))
      []
      s)))

(defcheck solution-eeb2b953
  #(let [dst (fn [sq rs st]
               (let [x (first sq)
                     r (rest sq)
                     nrs (if (get st x) rs (conj rs x))]
                 (if (seq r)
                   (recur r nrs (conj st x))
                   nrs)))]
     (dst % [] #{})))

(defcheck solution-eeee282e
  (fn [l]
    (reduce
      #(if (nil? (some #{%2} %1)) (conj %1 %2) %1)
      [] l)))

(defcheck solution-eef069ca
  #(reduce (fn [v n]
             (if (some #{n} v)
               v
               (conj v n)))
     [] %))

(defcheck solution-ef111c55
  (fn [c]
    (remove nil?
      (map #(if (%2 %) nil %)
        c
        (reductions conj #{} c)))))

(defcheck solution-efd1be7f
  (fn [s] (loop [s s, elts #{}, ret []]
            (if (seq s)
              (if (contains? elts (first s))
                (recur (rest s) elts ret)
                (recur (rest s) (conj elts (first s)) (conj ret (first s))))
              ret))))

(defcheck solution-f09dfdcf
  (fn [c]
    (first (reduce (fn [[c s] x]
                     (if (s x) [c s] [(conj c x) (conj s x)]))
             [[] #{}]
             c))))

(defcheck solution-f0be98b1
  #(loop [ls % acc '() hash #{}]
     (if (empty? ls)
       (reverse acc)
       (if (contains? hash (first ls))
         (recur (rest ls) acc hash)
         (recur (rest ls) (cons (first ls) acc) (conj hash (first ls)))))))

(defcheck solution-f0d17f4f
  (fn f
    ([s] (f s #{}))
    ([s u]
     (if (empty? s)
       nil
       (if (contains? u (first s))
         (f (rest s) u)
         (cons (first s) (f (rest s) (conj u (first s)))))))))

(defcheck solution-f0d8e3cf
  reduce #(if ((set %1) %2)
            %1
            (conj %1 %2)) [])

(defcheck solution-f0f0f991
  #(loop [seen #{}
          remaining %
          result []]
     (if (empty? remaining) result
                            (recur (conj seen (first remaining))
                              (rest remaining)
                              (if (seen (first remaining)) result
                                                           (conj result (first remaining)))))))

(defcheck solution-f11b07f5
  (fn [s]
    (second
      (reduce
        (fn [[se sq] v]
          (let [se' (conj se v)]
            (if-not (contains? se v)
              [se' (conj sq v)]
              [se' sq])))
        [#{} []] s))))

(defcheck solution-f1754399
  (fn [z] (reduce (fn [acc e] (if (some #{e} acc) acc  (conj acc e))) [] z)))

(defcheck solution-f189b2b7
  (let [remove-all
        (fn removeall [x y]
          (if (empty? x)
            x
            (if (= (first x)
                  y)
              (removeall (rest x) y)
              (cons (first x)
                (removeall (rest x) y)))))]
    (fn finddist [z]
      (if (empty? z)
        []
        (cons (first z)
          (remove-all (finddist (rest z))
            (first z)))))))

(defcheck solution-f2008a8
  (fn [s]
    (reduce
      (fn [r a]
        (if (some #(= a %) r) r (conj r a)))
      []
      s)))

(defcheck solution-f238885d
  (fn [coll] (loop [c coll a []] (if (empty? c) a (if (some #(= (first c) %) a) (recur (rest c) a) (recur (rest c) (conj a (first c))))))))

(defcheck solution-f23fb353
  (fn [coll] (reduce #(if ((into #{} %1) %2) %1 (conj %1 %2)) [] coll)))

(defcheck solution-f27e9ff7
  (fn [input] (second (reduce #(if (contains? (first %1) %2)
                                 (vector (first %1) (second %1))
                                 (vector (conj (first %1) %2) (conj (second %1) %2))) [#{} []] input))))

(defcheck solution-f29823f6
  (fn [items]
    (loop [itms (reverse items)
           out []]
      (let [f (first itms)
            r (vec (rest itms))]
        (if (nil? f)
          (reverse out)
          (if (some #(= f %) r)
            (recur r out)
            (recur r (conj out f))))))))

(defcheck solution-f2ab0192
  reduce (fn [deduped val] (if (not (some #{val} deduped)) (conj deduped val) deduped)) [])

(defcheck solution-f2afe310
  (fn [items]
    (loop [items items current #{} result []]
      (if (empty? items)
        result
        (let [item (first items) new? (not (contains? current item))]
          (recur (rest items)
            (if new? (conj current item) current)
            (if new? (conj result item) result)))))))

(defcheck solution-f3c5239f
  (fn iter
    ([in]
     (iter in [] #{}))
    ([[a & r] out s]
     (cond (nil? a) out
           (s a) (recur r out s)
           :else (recur r (conj out a) (conj s a))))))

(defcheck solution-f3c6d381
  (fn [xs]
    (let [rs (reductions conj #{} xs)
          tuples (map vector xs rs)]
      (for [[x r] tuples
            :when (not (r x))]
        x))))

(defcheck solution-f433ce01
  (fn [s]
    (reduce
      (fn [v x]
        (if (some #(= %1 x) v) v (conj v x)))
      []
      s)))

(defcheck solution-f48f2adf
  (fn my-distinct [coll]
    (reduce (fn [x y] (if (some #(= %1 y) x)
                        x
                        (conj x y)))
      [] coll)))

(defcheck solution-f4d7c57f
  (fn [s]
    (sort-by #(.indexOf s %)
      (map first (group-by identity s)))))

(defcheck solution-f54db416
  (fn [coll_]
    (loop [unq [] coll coll_ seen #{}]
      (if-let [[f & t] coll]
        (if (seen f)
          (recur unq t seen)
          (recur (conj unq f) t (conj seen f)))
        unq))))

(defcheck solution-f55ae82f
  (fn [x] (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] x)))

(defcheck solution-f581f066
  (fn [lat]
    (reduce
      (fn [a b]
        (if (some #{b} a) a (conj a b)))
      []
      lat)))

(defcheck solution-f5b70a5
  (fn [xs] (first (reduce (fn [[acc sm] b] (if (contains? sm b) [acc sm] [(conj acc b) (conj sm b)])) [[] #{}] xs))))

(defcheck solution-f5ce867
  (fn [& args]
    (loop [s (apply vec args) ns []]
      (if (empty? s)
        ns
        (recur (remove #(= % (first s)) s) (conj ns (first s)))))))

(defcheck solution-f603c89a
  #(loop [source % seen #{} result []]
     (let [[elm & elms] source]
       (cond (empty? source) result
             (seen elm) (recur elms seen result)
             :else (recur elms (conj seen elm) (conj result elm))))))

(defcheck solution-f61d8e3a
  reduce (fn [r x] (if (some #(= % x) r) r (conj r x))) [])

(defcheck solution-f671caeb
  (fn distinct'
    ([xs] (distinct' xs []))
    ([xs acc]
     (if (empty? xs) acc
                     (let [x0 (first xs)
                           p #(not= % x0)]
                       (recur (filter p xs) (conj acc x0)))))))

(defcheck solution-f673d59f
  (fn [xs]
    (first
      (reduce
        (fn [[acc occurances] x]
          (if (contains? occurances x)
            [acc occurances]
            [(conj acc x) (conj occurances x)]))
        [[] #{}] xs))))

(defcheck solution-f684a430
  (fn find-distinct-items [coll]
    (reduce (fn [xs x]
              (if ((set xs) x) xs
                               (conj xs x))) [] coll)))

(defcheck solution-f68c7503
  (fn [coll]
    (reduce (fn [c x] (if (some #(= x %) c) c (conj c x))) [] coll)))

(defcheck solution-f75ab6f4
  ; Without maintaining order...
  ; #(for [[k v] (frequencies %)] k)

  (fn [coll]
    (reduce
      #(if (contains? (into #{} %1) %2)
         %1
         (conj %1 %2))
      []
      coll)))

(defcheck solution-f78e110d
  (fn [xs]
    (reduce #(if (not (contains? (set %1) %2))
               (conj %1 %2)
               %1) [] xs)
    ))

(defcheck solution-f7dbd43b
  #(loop [xs % res []]
     #_(println xs)
     #_(println res)
     #_(println (first xs))
     (cond
       (empty? xs) res
       (neg? (.indexOf res (first xs))) (recur (rest xs) (conj res (first xs)))
       :else (recur (rest xs) res))))

(defcheck solution-f7debdcd
  (partial reduce #(if-not (some #{%2} %1) (conj %1 %2) %1) []))

(defcheck solution-f863e9cf
  (fn [coll]
    (reduce #(if (contains? (set %1) %2) %1 (conj %1 %2)) [] coll)))

(defcheck solution-f8664f40
  ;; uses a set for fast lookup of existing entrises
  (fn [coll]
    (first
      (reduce
        (fn [[unique-coll unique-set] val]
          (if (contains? unique-set val)
            [unique-coll unique-set]
            [(conj unique-coll val)
             (conj unique-set val)]))
        [[] #{}]
        coll))))

(defcheck solution-f8687437
  (fn [s]
    (reduce
      (fn [agg x] (if (some #{x} agg)
                    agg
                    (conj agg x)))
      [] s)))

(defcheck solution-f869a75f
  (fn distinct-xs
    ([xs] (distinct-xs xs []))
    ([[x & xs :as lst] result]
     (if (empty? lst)
       result
       (distinct-xs xs (if (= -1 (.indexOf result x)) (conj result x) result))))))

(defcheck solution-f884ae55
  (fn [x] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] x)))

(defcheck solution-f89dd717
  (fn [c] (reduce #(if (some #{%2} % ) % (conj % %2)) [] c)))

(defcheck solution-f8d56712
  #(reduce (fn [t v] (if (some #{v} t) t (conj t v))) [] %))

(defcheck solution-f8d90f74
  #(fnext (reduce
            (fn [[s l] x]
              [(conj s x) (if (get s x) l (conj l x))])
            [#{} []]
            %)))

(defcheck solution-f91a8869
  (fn [xs]
    (reduce
      (fn [s e]
        (if (some #(= % e) s)
          s
          (conj s e)))
      [] xs)))

(defcheck solution-f9266b8b
  (fn [s]
    (sort-by #(.indexOf s %)
      (map #(first %) (group-by identity s)))))

(defcheck solution-f96abb93
  (fn [x] (sort-by #(.indexOf x %) (vec (set x)))))

(defcheck solution-f9b39fbd
  (fn dist [coll]
    (let [x (first coll) xs (rest coll) n (filter #(not= x %) xs)]
      (if (empty? n) (list x) (cons x (dist n))))))

(defcheck solution-fa0fece6
  (fn __ [s]
    (if (empty? s) (empty s)
                   (let [x (first s)]
                     (concat [x] (__ (filter #(not= x %) (rest s))))))))

(defcheck solution-fa395652
  (fn dis

    ([x]

     (dis x #{}))

    ([[x & xs] st]

     (if (= xs nil)

       (if (= (st x) nil)

         (cons x [])

         [])

       (if (= (st x) nil)

         (cons x (dis xs (conj st x)))

         (dis xs st))))))

(defcheck solution-fa6442cf
  (fn [se]
    (let [alreadyin (fn [a b] (if (some #(= % b) a) true nil))
          mydistinct (fn mydistinct [ds acc ind]
                       (if (= ind (count ds)) acc
                                              (mydistinct ds
                                                (if-not (alreadyin acc (nth ds ind))
                                                  (conj acc (nth ds ind))
                                                  acc)
                                                (inc ind))))]
      (mydistinct se [] 0))))

(defcheck solution-fa7763b6
  (fn distinct-items
    [c]
    (let [result {:set #{} :vector []}]
      (:vector (reduce (fn[r val]
                         (if (some #{val}(:set r))
                           r
                           {:set (conj (:set r) val) :vector (conj (:vector r) val)}))
                 result c)))))

(defcheck solution-fb06b30a
  (fn [col]
    (letfn [( f [ys xs]
              (if (empty? ys)
                (reverse xs)
                (if (some #(= (first ys) %) xs)
                  (f (rest ys) xs)
                  (f (rest ys) (cons (first ys) xs)))
                ))]
      (f col ()))))

(defcheck solution-fb2782db
  #(loop [x #{}, s %, res []] (if (empty? s) res (let [[c & r] s] (if (x c) (recur x r res) (recur (conj x c) r (conj res c)))))))

(defcheck solution-fb3f8056
  (partial reduce (fn [a b] (if (contains? (set a) b) a (conj a b))) []))

(defcheck solution-fba5034c
  (fn  [lst]
    (loop [rm (seq lst), acc (vector)]
      (cond (empty? rm) acc
            :else (if (some #{(first rm)} acc)
                    (recur (rest rm) acc)
                    (recur (rest rm) (conj acc (first rm))))))))

(defcheck solution-fbd6cec1
  (fn find-distinct
    [sequence]
    (loop [distinct-element-seq '[]
           identity-set         #{}
           remaining-seq        (map first (partition-by identity sequence))]
      (if (empty? remaining-seq) ;; return the result if we traversed the entire sequence
        distinct-element-seq
        (let [next-element (first remaining-seq)
              distinct-element-seq'
                           (if (contains? identity-set next-element) ;; only add an element to the
                             ;; result if it has not been encountered before
                             distinct-element-seq
                             (conj distinct-element-seq next-element))
              identity-set' (conj identity-set next-element)
              remaining-seq' (rest remaining-seq)]
          (recur distinct-element-seq' identity-set' remaining-seq'))))))

(defcheck solution-fbe6bd07
  (fn ddistinct
    ([coll] (ddistinct coll []))
    ([coll seen]
     (if (empty? coll)
       seen
       (if (nil? (some #{(first coll)} seen))
         (ddistinct (rest coll) (conj seen (first coll)))
         (ddistinct (rest coll) seen))))))

(defcheck solution-fc82500b
  (fn [coll]
    (loop [result []
           seen   #{}
           remain   coll]
      (if (empty? remain)
        result
        (let [val (first remain)
              rest (rest remain)]
          (if (contains? seen val)
            (recur result seen rest)
            (recur (conj result val)
              (conj seen val)
              rest)))))))

(defcheck solution-fc9b8193
  (fn [l]
    (reduce (fn [a x] (if-not (some #(= x %) a) (conj a x) a)) [] l)
    ))

(defcheck solution-fcbfa34
  #((fn [-rest -vec -set]
      (if (empty? -rest)
        -vec
        (let [[f & r] -rest]
          (if (contains? -set f)
            (recur r -vec -set)
            (recur r (conj -vec f) (conj -set f))))))
    % [] #{}))

(defcheck solution-fd21e54d
  (fn dst [s]
    (second (reduce (fn [[st res] n]
                      (if (contains? st n)
                        [st res]
                        [(conj st n) (conj res n)]))
              [#{} []] s))))

(defcheck solution-fd56269b
  (fn xdistinct
    [coll]
    (loop [res []
           map (group-by identity coll)
           rem coll
           ]
      (if (empty? rem)
        res
        (recur (if (contains? map (first rem))
                 (conj res (first rem))
                 res)
          (dissoc map (first rem))
          (rest rem))
        ))))

(defcheck solution-fd8b6a44
  (fn i [col]
    (if (empty? col) []
                     (cons (first col) (i (remove #(= (first col) %) col))))))

(defcheck solution-fda540a2
  (fn [s] (reduce (fn [a v]
                    (if (not-any? #(= % v) a)
                      (conj a v)
                      a))
            []
            s)))

(defcheck solution-fded72eb
  (fn mydist [s]
    (reduce (fn [l r]
              (if (some #(= r %) l)
                l
                (conj l r))) [] s)))

(defcheck solution-fdf696c6
  (fn [v]
    (map first (sort-by last (let [indexedv (map-indexed (fn [idx itm] [itm idx]) v )]
                               (loop [ result {} col indexedv]
                                 (if (empty? col)
                                   result
                                   (recur (if (contains? result (first (first col)))
                                            result
                                            (conj result (first col)))
                                     (rest col))
                                   )))))))

(defcheck solution-fe245f5
  (fn [coll]
    (letfn [(not-in? [val s] (not-any? #(= val %) s))]
      (reduce #(if (not-in? %2 %) (conj % %2) %) [] coll))))

(defcheck solution-fe24aad2
  #((fn f [x r] (if (= x nil) r
                              (let [[a & b] x]
                                (f b (if (get (set r) a)
                                       r (conj r a)))))) % []))

(defcheck solution-fe277910
  (fn [l]
    (reduce (fn [m e]
              (if (some #{e} m)
                m
                (conj m e)))
      [] l)))

(defcheck solution-fe795f9f
  (fn [x] (reverse
            (reduce
              (fn [unique y]
                (if (some #{y} unique) unique
                                       (cons y unique)))
              '() x))))

(defcheck solution-fef0c5e0
  (fn f [acc s]

    (if (empty? s) acc
                   (f (if (some #{(first s)} acc) acc (conj acc (first s))) (rest s)))) [])

(defcheck solution-ff418085
  (fn co [s]
    (let [f (fn [[col seen] v] (if (contains? seen v) [col seen] [(conj col v) (conj seen v)]))
          mas (reduce f [[] #{}] s)]
      (first mas))))

(defcheck solution-ff93a203
  #(loop [up (first %)
          coll (rest %)
          dist []
          done #{}]
     (if (nil? up)
       dist
       (recur (first coll) (rest coll) (if (contains? done up)
                                         dist
                                         (conj dist up)) (conj done up)))))

(defcheck solution-ff9b00e4
  (partial reduce #(if (some #{%2} %1) %1 (conj %1 %2)) []))

(defcheck solution-ffa1ecbb
  (fn disti[input]
    (second (reduce
              (fn [obj x]
                (let [s (first obj) l (second obj)]
                  (if (contains? s x)
                    [s l]
                    [(conj s x) (conj l x)])))
              [#{} []]
              input))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-56))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
