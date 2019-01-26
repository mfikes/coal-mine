(ns coal-mine.problem-31
  (:require [coal-mine.checks :refer [defcheck-31] :rename {defcheck-31 defcheck}]
            [clojure.test]))

(defcheck solution-100e27ea
  (fn
    [cs]
    (reduce (fn [c [a b]]
              (let [segs (count c)
                    entries (count (c (dec segs)))]
                (if (= a b)
                  (assoc-in c [(dec segs) entries] b)
                  (assoc-in c [segs] [b]))))
      [[(first cs)]]
      (partition 2 1 cs))))

(defcheck solution-10a3a919
  (fn f [coll]
    (when (seq coll)
      (let [x (first coll)]
        (cons (take-while #(= x %) coll)
          (f (drop-while #(= x %) coll)))))))

(defcheck solution-10cb255
  (fn [xs]
    (loop [res '()
           head (first xs)
           tail (rest xs)]
      (if (empty? tail)
        (if head
          (reverse (cons (list head) res))
          (reverse res))
        (recur (cons (cons head (take-while #(= % head) tail)) res)
          (first (drop-while #(= % head) tail))
          (rest (drop-while #(= % head) tail)))))))

(defcheck solution-10d4c255
  (fn [lst]
    (reduce
      (fn [acc x]
        (let [acc' (vec (butlast acc))
              las (last acc)]
          (if (or (= (last las) x) (empty? las))
            (conj acc' (conj las x))
            (conj acc [x])
            )
          )
        )
      [[]]
      lst
      )
    ))

(defcheck solution-1270191d
  (fn [sq]
    (apply conj
      (reduce (fn [[acc group] el]
                (if (= el (first group))
                  [acc (conj group el)]
                  [(conj acc group) [el]]))
        [[] [(first sq)]]
        (rest sq)))))

(defcheck solution-12f3d687
  (fn pack [xs]
    (reverse ((fn helper [xs last accu]
                (cond
                  (empty? xs) (cons last accu)
                  (= (first xs) (first last))
                  (recur (rest xs) (cons (first xs) last) accu)
                  :else
                  (recur (rest xs) (list (first xs)) (cons last accu))))
              (rest xs) (list (first xs)) nil))))

(defcheck solution-1365f992
  (fn [l] (letfn [(helper [z acc] (let [h (take-while #(= (first z) %) z)
                                        rest (drop (count h) z)]
                                    (cond (empty? rest) (conj acc h)
                                          (= 1 (count rest)) (conj (conj acc h)  rest)
                                          :else (helper rest (conj acc h)))))]
            (helper l []))))

(defcheck solution-144df4e5
  (fn packX [x] (partition-by identity x)))

(defcheck solution-1486a1fe
  (fn [x]
    (reverse (reduce #(if (= (first (first %1)) %2)
                        (conj (rest %1) (conj (first %1) %2))
                        (conj %1 (list %2)))
               () x))
    ))

(defcheck solution-154a55fd
  reduce (fn processlist [acc i]
           (let [l (last acc)]
             (if (or (empty? l) (not= (last l) i))
               (conj acc (list i))
               (conj (into [] (butlast acc)) (conj l i))))) [])

(defcheck solution-155af037
  (fn f
    ([l] (f l [] []))
    ([[l & ls] r c]
     (if l
       (if (or (empty? c) (= l (first c)))
         (recur ls r (conj c l))
         (recur ls (conj r c) [l]))
       (conj r c)))))

(defcheck solution-157e1fbe
  #(loop [[head & tail :as coll] %
          curr nil
          acc []]
     (let [[thead & ttail] tail]
       #_(println "head=" head "thead=" thead "ttail=" ttail "curr=" curr "acc=" acc)
       (if (nil? head)
         acc
         (if (= head thead)
           (recur (conj ttail head) (conj curr thead) acc)
           (recur tail nil (conj acc (conj curr head)))
           )
         )
       )
     ))

(defcheck solution-15ae9e13
  #(loop [s (rest %1) e [(first %1)] r []]
     (cond (empty? s) (conj r e)
           (= (first e) (first s)) (recur (rest s) (conj e (first s)) r)
           :else (recur (rest s) [(first s)] (conj r e)))))

(defcheck solution-16e57cf8
  (fn r
    ([s] (r [] s))
    ([res s]
     (if (empty? s)
       res
       (let [g (take-while #(= (first s) %) s)
             h (drop-while #(= (first s) %) s)]
         (concat res (vector g) (r h)))))))

(defcheck solution-1794d0a9
  (fn [col]
    (loop [c col result []]
      (if (empty? c)
        result
        (let [head (first c)
              group (take-while #(= head %) c)
              tail (drop-while #(= head %) c)
              new-result (conj result group)]
          (recur tail new-result))))))

(defcheck solution-17bfb1ea
  #((fn [seq current-acc full-acc]
      (if
       (empty? seq) (reverse (if (empty? current-acc) (full-acc) (cons (reverse current-acc) full-acc)))
                    (if (empty? current-acc)
                      (recur (rest seq) (cons (first seq) current-acc) full-acc)
                      (if (= (first seq) (first current-acc))
                        (recur (rest seq) (cons (first seq) current-acc) full-acc)
                        (recur (rest seq) (cons (first seq) (empty seq)) (cons (reverse current-acc) full-acc))))))
    % '() '()))

(defcheck solution-17ea0739
  (fn [l]
    (reduce (fn [ret this]
              (let [l (last ret)]
                (if-not (= this (last l))
                  (conj ret [this])
                  (-> ret (pop) (conj (conj l this)) )))) [] l)))

(defcheck solution-18ef1d3c
  (fn pack [ls]
    (if (= (count ls) 0)
      []
      (cons (take-while (partial = (first ls)) ls)
        (pack (drop-while (partial = (first ls)) ls))
        )
      )
    ))

(defcheck solution-19442d0e
  (fn [seq]
    (reduce (fn [ret x]
              (if (and (> (count ret) 0) (= (first (first ret)) x))
                (conj
                  (rest ret)
                  (conj (first ret) x))
                (conj ret (list x))))
      (list) (reverse seq))))

(defcheck solution-1a8e91e
  (fn pack [s]
    (loop [r s acc []]
      (if (seq r) (recur (drop-while #( = % (first r)) r)
                    (conj acc (take-while #( = % (first r)) r)))
                  acc))))

(defcheck solution-1b62305e
  (fn pck [seqn]
    (reverse (reduce #(if
                       (and
                        (coll? (first %1))
                        (= (first(first %1)) %2)) (conj (rest %1) (conj (first %1) %2)) (conj %1 (list %2))) '() seqn))
    ))

(defcheck solution-1c025a9b
  (fn pack-sequence [[head & tail :as xs]]
    (letfn [(pack-value-at-front [x [head & tail :as xs]]
              (cond (nil? head) (list (list x))
                    (= x (first head)) (cons (cons x head) tail)
                    :else (cons (list x) xs)
                    ))]


      (if (nil? head)
        '()
        (pack-value-at-front head (pack-sequence tail))))))

(defcheck solution-1cc73a65
  (fn [s]
    (loop [l (rest s) cur [(first s)] nl []]
      (let [f (first l)]
        (cond
          (nil? f) (conj nl cur)
          (= f (first cur)) (recur (rest l) (conj cur f) nl)
          :else (recur (rest l) [f] (conj nl cur)))))))

(defcheck solution-1dfecc87
  (fn [l]
    (loop [output [] xs l]
      (if (seq xs)
        (let [parts (split-with #(= (first xs) %) xs)]
          (recur
            (conj output (first parts))
            (first (rest parts))))
        output))))

(defcheck solution-1e6e7188
  (fn mytake [s]
    (if (empty? s) s
                   (conj (mytake (drop-while #(= % (first s)) s))
                     (take-while #(= % (first s)) s)))))

(defcheck solution-1e92e8e6
  (fn [xs]
    (if (empty? xs)
      xs
      (letfn
       [(go [yy ys]
          (if (empty? ys)
            (list yy)
            (if (= (first yy) (first ys))
              (go (cons (first yy) yy) (rest ys))
              (cons yy (go (list (first ys)) (rest ys))))))]
        (go (list (first xs)) (rest xs))))))

(defcheck solution-1ece9f54
  (partial
    reduce
    (fn [xs, x]
      (if (and
           (> (count xs) 0)
           (some (partial = x) (last xs))
           )
        (concat (butlast xs) (list (conj (last xs) x)))
        (concat xs (list (list x)))
        )
      )
    '()
    ))

(defcheck solution-1f48b152
  (fn [s]
    (loop [remaining s prev (first s) cnt 0 ans []]
      (if (empty? remaining)
        (conj ans (for [x (range cnt)] prev))
        (let [[cur & remain] remaining]
          (if (= cur prev)
            (recur remain prev (inc cnt) ans)
            (recur remain cur 1 (conj ans (for [x (range cnt)] prev)))))))))

(defcheck solution-1f60abea
  (fn pack [items]
    (if (empty? items)
      '()
      (concat (list (take-while (partial = (first items)) items))
              (pack (drop-while (partial = (first items)) items))))))

(defcheck solution-1f9a367f
  (fn pack
    ([s]
     (pack (rest s) [(first s)]))
    ([s current]
     (cond
       (empty? s) [current]
       (= (first s) (first current))
       (pack (rest s) (cons (first s) current))
       :else (cons current (pack s))))))

(defcheck solution-2012bca9
  #(letfn [(f [x acc current] (if (empty? x) (conj acc current) (if (= (first x) (last current)) (f (rest x) acc (conj current (first x))) (f (rest x) (conj acc current) (list (first x))))))]
     (f (rest %) [] (list (first %)))))

(defcheck solution-206cf669
  (fn f [l]
    (if-let [h (first l)]
      (cons
        (take-while #(= % h) l)
        (f (drop-while #(= % h) l))))))

(defcheck solution-208c95cd
  (fn hoge [lst]
    (if (empty? lst) nil
                     (let [h (first lst) f (partial = h)]
                       (cons (take-while f lst) (hoge (drop-while f lst)))))))

(defcheck solution-210f1b80
  (fn pack [lst]
    (let [
          fst (first lst)
          cmp #(= fst %)
          part (take-while cmp lst)
          rst (drop-while cmp lst) ]

      (if fst; list not empty
        (conj (pack rst) part)
        '(); else
        ))))

(defcheck solution-2139a61c
  (comp reverse
        (partial reduce
          (fn [s x]
            (if (= (first (first s)) x)
              (cons (cons x (first s)) (rest s))
              (cons (list x) s)))
          '())))

(defcheck solution-21cf88bd
  (fn [s]
    (loop [xs s result [] current []]
      (cond (empty? xs) (if (empty? current) result (conj result current))
            (empty? current) (recur (rest xs) result [(first xs)])
            (= (first xs) (first current)) (recur (rest xs) result (conj current (first xs)))
            true (recur xs (conj result current) [])))))

(defcheck solution-22751a13
  (fn _pack [s]
    (let [pred #(= % (first s))]
      (when (not (empty? s))
        (conj (_pack (drop-while pred s)) (take-while pred s))))))

(defcheck solution-24290f9d
  (fn [coll] (partition-by identity coll)))

(defcheck solution-247508cc
  (fn pack [coll]
    (loop [remaining (reverse coll)
           result '()]
      (if (empty? remaining)
        result
        (let [[subset the-rest](split-with (fn [n] (= n (first remaining))) remaining)]
          (recur the-rest (cons subset result)))))))

(defcheck solution-2522e162
  (fn f [s]
    (if (empty? s)
      '()
      (let [[x xs] (split-with #(= (first s) %) s)]
        (cons x (f xs))))))

(defcheck solution-25c4c67a
  (fn pack
    [lista]
    (if-not (empty? lista)
      (let [sp (split-with #(= % (first lista)) lista)]
        (concat (drop-last sp) (pack (last sp)))))))

(defcheck solution-2652ac65
  (fn [coll]
    (loop [result [] x (first coll) n 1 coll (rest coll)]
      (cond
        (not x) result
        (= x (first coll)) (recur result x (+ n 1) (rest coll))
        :else (recur (conj result (repeat n x)) (first coll) 1 (rest coll))))))

(defcheck solution-2742ab1e
  (fn mypack [a] (loop [a a res []]
                   (if (empty? a) res
                                  (recur (drop-while #(= % (first a)) a)
                                    (conj res (take-while #(= % (first a)) a)))))))

(defcheck solution-274a0ee0
  (fn [xs]
    (reduce (fn [xs x]
              (if (=  (first (last xs)) x)
                (conj (pop xs) (conj (last xs) x))
                (conj xs [x])
                )

              )
      []
      xs


      )))

(defcheck solution-282ecf49
  (fn [xs] (reduce (fn [a b] (cond (nil? a) [[b]] (= (last (last a)) b) (conj (into [] (butlast a)) (conj (last a) b)) true (conj a [b]))) nil xs)))

(defcheck solution-2864b822
  (fn ppp [i] (partition-by identity i)))

(defcheck solution-29937991
  (fn f [xs]
    (if (empty? xs)
      xs
      (let [x (first xs)
            [same rest] (split-with #(= x %) xs)]
        (concat [same] (f rest))))))

(defcheck solution-29fe8d55
  (fn [daseq] (reverse (reduce #(if (= (first (first %1)) %2) (conj (rest %1) (conj (first %1) %2)) (conj %1 (list %2))) '() daseq))))

(defcheck solution-2aa338c6
  #(let [f (fn [acc x]
             (if (= x (first (first acc)))
               (cons (cons x (first acc)) (next acc))
               (cons (list x) acc)))]
     (reverse (reduce f () %))))

(defcheck solution-2aac0088
  (fn [l]
    (partition-by identity l)))

(defcheck solution-2aca0513
  (fn f [all]
    (if (empty? all)
      all
      (let [[x &xs] all]
        (let [group (take-while #(= x %) all)]
          (concat [group] (f (drop (count group) all))))))))

(defcheck solution-2c1484e7
  #(partition-by (fn [x] (identity x)) %))

(defcheck solution-2e70af17
  (fn pack [x]
    (letfn [
            (pack-c [y c]
              (if (= y '())
                '()
                (if (= (first y) c)
                  (cons c (pack-c (rest y) c))
                  '()
                  )
                )
              )
            ]
      (if (= x '())
        '()
        (cons (pack-c x (first x)) (pack (drop (count (pack-c x (first x))) x)))
        )
      )
    ))

(defcheck solution-2f15318
  (fn pack [s]
    (loop [answer [] rem s]
      (if (empty? rem)
        answer
        (let [spl (split-with (partial = (first rem)) rem)]
          (recur (conj answer (first spl)) (second spl)))))))

(defcheck solution-2f48b309
  (fn [coll]
    (loop [in coll out []]
      (if (empty? in)
        out
        (recur (drop-while #(= % (first in)) in) (conj out (take-while #(= % (first in)) in)))))))

(defcheck solution-2fa50a36
  partition-by str)

(defcheck solution-2fbd868b
  #(partition-by list %))

(defcheck solution-30e5e8fe
  (fn pack [xs]
    (if (empty? xs)
      '()
      ((fn [pair] (cons (nth pair 0) (pack (nth pair 1))))
       (split-with (partial = (first xs)) xs)))))

(defcheck solution-316dfb9a
  (fn sdupl [coll]
    (lazy-seq
      (when (seq coll)
        (let [h (first coll) [head tail] (split-with #(= h %) coll)] (cons head (sdupl tail)))))))

(defcheck solution-32420f40
  #(reverse (reduce (fn [acc x]
                      (if (= x (first (first acc)))
                        (conj (drop 1 acc) (concat (first acc) (list x)) )
                        (conj acc (list x))
                        )
                      ) () %) ))

(defcheck solution-325fcb62
  #(rest (reverse (reduce (fn[x,y](if (= (last (first x)) y) (cons (cons y (first x)) (rest x)) (cons (list y) x))) [[]] %))))

(defcheck solution-326b85ec
  (fn p [s]
    (when-first [f s]
      (cons (take-while #(= f %) s)
        (p (drop-while #(= f %) s))))))

(defcheck solution-32eceaca
  (fn [vs]
    (reduce (fn [acc v]
              (cond
                (empty? acc) (conj acc (vector v))
                (= (-> acc last first) v) (update-in acc [(-> acc count dec)] conj v)
                :else (conj acc (vector v))))
      []
      vs)))

(defcheck solution-32f4b576
  (fn d [xs](when (seq xs)(lazy-seq (cons (take-while #(= (first xs) %) xs)  (d (drop-while #(= (first xs) %) (rest xs))))))))

(defcheck solution-3382a02c
  (fn [xs]
    (if (seq xs)
      (loop [ys (rest xs)
             zs []
             ws [(first xs)]]
        (if (seq ys)
          (if (= (first ys) (first ws))
            (recur (rest ys) zs (conj ws (first ys)))
            (recur (rest ys) (conj zs ws) [(first ys)]))
          (conj zs ws)))
      nil)))

(defcheck solution-338469d6
  (fn [s]
    (reverse
      (reduce #(if (not= (first (first %1)) %2)
                 (cons (list %2) %1)
                 (cons (cons %2 (first %1)) (rest %1)))
        '() (seq s))) ))

(defcheck solution-34a03f18
  (fn [input]
    (loop [iter-list input
           res-list []]
      (if (empty? iter-list)
        res-list
        (let [head (first iter-list)
              dups '()]
          (recur (drop-while
                   #(= head %)
                   iter-list)
            (conj res-list
              (take-while
                #(= head %)
                iter-list))))))))

(defcheck solution-352c385b
  #(reverse (reduce (fn pack [a b]
                      (if (= (first (first a))  b)
                        (conj (rest a) (conj (first a) b))
                        (conj a (list b)))) '() %)))

(defcheck solution-36251ae4
  (fn [data] (reverse (loop [r '()
                             c data]
                        (if (empty? c)
                          r
                          (recur (conj r (take-while #(= (first c) %) c))
                            (drop-while #(= (first c) %) c)))))))

(defcheck solution-37cbcd5d
  partition-by (fn[x]x))

(defcheck solution-37d26964
  #(loop [acc [] cur [] last nil coll %]
     (if-let [[a & coll] coll]
       (if (or (= last a) (nil? last))
         (recur acc (conj cur a) a coll)
         (recur (conj acc cur) [a] a coll))
       (if (seq cur)
         (conj acc cur)
         acc))))

(defcheck solution-37ec5774
  #(letfn [(same [l]
             (let [a (first l) b (second l)]
               (if (= a b)
                 (cons a (same (rest l)))
                 [a (rest l)])))]
     (loop [l %1 acc []]
       (if (empty? l) acc
                      (let [a (same l) b (drop-last a) c (last a)]
                        (recur c (conj acc b)))))))

(defcheck solution-38563027
  (fn x
    ([coll]
     (reverse
       (loop [coll coll res '()]
         (if (empty? coll)
           res
           (let [[part other] (split-with #(= (first coll) %) coll)]
             (recur other (cons part res)))))))))

(defcheck solution-396a1585
  (fn [xs]
    (reverse
      (loop [xsp (rest xs) dups (cons (first xs) '()) acc '()]
        (if (empty? xsp)
          (cons dups acc)
          (if (= (first xsp) (first dups))
            (recur (rest xsp) (cons (first xsp) dups) acc)
            (recur (rest xsp) (cons (first xsp) '()) (cons dups acc))))))))

(defcheck solution-3c125427
  (fn[a-seq]
    (letfn [(compr[[s1 s2]]
              (split-with #(= % (first s2)) s2))]
      (map first (drop 1
                   (take-while (fn[[s1 s2]] (not (empty? s1)))
                     (iterate compr [[1]a-seq])))))))

(defcheck solution-3d54e1e7
  (fn
    [in]
    (loop [remaining (rest in)
           curr (first in)
           currlist '()
           completed '()]
      (if (= 0 (count remaining))
        (reverse (conj completed (conj currlist curr)))
        (if (= curr (first remaining))
          (recur (rest remaining) curr (conj currlist curr) completed)
          (recur (rest remaining) (first remaining) '() (conj completed (conj currlist curr))))))))

(defcheck solution-3f30cb85
  (fn [s]
    (loop [p (list (first s)) l (rest s) acc ()]
      (cond (empty? l) (reverse (conj acc p))
            (= (first p) (first l)) (recur (conj p (first p)) (rest l) acc)
            :else (recur (list (first l)) (rest l) (conj acc p))
            ))))

(defcheck solution-4118ee13
  (fn u
    ([x]
     (u (rest x) [(first x)]))
    ([x f]
     (if x
       (let [[s & xs] x]
         (if (= s (first f))
           (u xs (conj f s))
           (conj (u xs [s]) f)))
       (list f)))))

(defcheck solution-424b0142
  #(letfn [(same [lst]
             (if (= (first lst) (first (rest lst)))
               (cons (first lst) (same (rest lst)))
               (list (first lst))))
           (skip [lst x]
             (if (not= (first lst) x)
               lst
               (skip (rest lst) x)))
           (f [lst]
             (if (= '() lst)
               '()
               (cons (same lst) (f (skip lst (first lst))))))]
     (f %)))

(defcheck solution-42710a48
  (fn [coll]
    (reduce (fn [l x] (if (= x (first (first l)))
                        (conj (rest l) (conj (first l) x))
                        (conj l (list x)))) () (reverse coll))))

(defcheck solution-4383d484
  (fn pack-seq
    ([[h & t] dupes last out]
     #_(println "h " h " t " t " dupes " dupes " last " last " out " out)
     (if (nil? h)
       (reverse (cons dupes out))
       (if (nil? last)
         (pack-seq t (list h) h out)
         (if (= h last)
           (pack-seq t (cons h dupes) h out)
           (pack-seq t (list h) h (cons dupes out))))))
    ([s]
     (pack-seq s nil nil nil))))

(defcheck solution-4409b4ec
  (fn pack [xs]
    (if (empty? xs)
      ()
      (cons (take-while (partial = (first xs)) xs) (pack (drop-while (partial = (first xs)) xs))))))

(defcheck solution-440e8a99
  (fn [seqs]
    (loop [result '()
           sub (list (first seqs))
           others (rest  seqs)]
      (if(empty? others)
        (concat result (list sub))
        (if(= (first sub ) (first others))
          (recur result
            (cons (first others ) sub)
            (rest others))
          (recur (concat result (list sub))
            (list (first others))
            (rest others)))))))

(defcheck solution-4655d182
  reduce (fn [a x]
           (if (= (last (last a)) x)
             (conj (vec (butlast a)) (conj (last a) x))
             (conj a [x]))) [])

(defcheck solution-47134658
  (fn seq-pack [xs]
    (if (seq xs)
      (cons (take-while #(= % (first xs)) xs)
        (seq-pack (drop-while #(= % (first xs)) xs))))))

(defcheck solution-48231ad6
  (fn [l]
    (reverse
      (reduce
        (fn[a,v]
          (if (= (first (first a)) v) (into (rest a) (list (into (first a) (list v))))
                                      (into a (list (list v)))))
        (list (list (first l)))
        (rest l)))))

(defcheck solution-486bf357
  (fn [coll]
    (reduce (fn [acc x]
              (cond
                (empty? acc) (conj acc (vector x))
                (= (last (last acc)) x) (conj (vec (drop-last acc)) (conj (last acc) x))
                :else (conj acc (vector x)))) [] coll)))

(defcheck solution-48ae30ba
  (fn [l]
    (loop [cl   []
           more l
           pe   nil]
      (if (nil? more)
        (conj cl pe)
        (if (nil? pe)
          (recur cl (next more) [(first more)])
          (if (= (first pe) (first more))
            (recur cl (next more) (conj pe (first pe)))
            (recur (conj cl pe) (next more) [(first more)])))))))

(defcheck solution-498bdedd
  (fn pack [x]
    (loop [x x prev nil res nil]
      (cond (empty? x)
            (drop 1 (reverse (cons prev res)))
            (= (first prev) (first x))
            (recur (rest x) (conj prev (first x)) res)
            :else
            (recur (rest x) (vector (first x)) (cons prev res))
            )
      )
    ))

(defcheck solution-4ac599e1
  (fn pack-seq
    [[hh & tt]]
    (loop [[h & t] tt
           current hh
           inner-accum [hh]
           outer-accum []]
      (let [new-inner-accum (if (= current h)
                              (conj inner-accum h)
                              [h])
            new-outer-accum (if (= current h)
                              outer-accum
                              (conj outer-accum (into () (reverse inner-accum))))]
        (if (nil? t)
          (into () (reverse (conj new-outer-accum (into () (reverse new-inner-accum)))))
          (recur t h new-inner-accum new-outer-accum))))))

(defcheck solution-4c2ef8d5
  (fn [xs] (let [[c, v, vs] (reduce (fn [[c, v0, vs] v1] (if (= v0 v1) [(+ c 1), v0, vs] [1, v1, (conj vs (repeat c v0))])) [0, nil, []] xs)] (rest (conj vs (repeat c v))))))

(defcheck solution-4ce6366
  (fn [s]
    (reduce #(if (= %2 (first (last %1)))
               (conj (pop %1) (conj (last %1) %2))
               (conj %1 (list %2)))
      (cons [] s))))

(defcheck solution-4de36392
  (fn [coll]
    (loop [coll coll, group [], result []]
      (if (empty? coll)
        (conj result group)
        (if (or (empty? group) (= (first coll) (first group)))
          (recur (rest coll) (conj group (first coll)) result)
          (recur (rest coll) [(first coll)] (conj result group)))))))

(defcheck solution-4e1d52ac
  (fn this [s]
    (let [pack (fn [l c]
                 (cond (<= (count l) 0) c
                       (= (count l) 1) (inc c)
                       (= (first l) (second l)) (recur (rest l) (inc c))
                       :else (inc c)))]
      (cond (= (count s) 0) '()
            :else (let [p (pack s 0)]
                    (conj (this (nthnext s p))
                      (repeat p (first s)))
                    )))))

(defcheck solution-4ef685e3
  (fn group1 [xs]
    (lazy-seq
      (when (not (empty? xs))
        (cons
          (take-while #(= (first xs) %) xs)
          (group1 (drop-while #(= (first xs) %) xs)))))))

(defcheck solution-4f36f8dc
  (fn pack [s]
    (loop [f (first s)
           tail s
           rv []]
      (if (empty? tail)
        rv
        (let [par (partial = f)
              rest' (drop-while par tail)
              head' (take-while par tail)]
          (recur (first rest') rest' (conj rv head')))))))

(defcheck solution-503af5e6
  (fn pack [x]
    (when-let [first-item (first x)]
      (let [first-item? #(= first-item %)]
        (cons (take-while first-item? x) (pack (drop-while first-item? x)))))))

(defcheck solution-504c7205
  (fn[s](reverse(reduce (fn [result _first]
                          (if (= _first (first (first result)))
                            (conj (rest result) (conj (first result) _first))
                            (conj result (list _first)))) '() s))))

(defcheck solution-50fff9aa
  (fn [coll]
    (reverse
      (reduce
        (fn [s x] (if (= x (ffirst s))
                    (cons (cons x (first s)) (rest s))
                    (cons (list x) s)))
        ()
        coll))))

(defcheck solution-51dbed0f
  (fn !
    ([x] (! (first x) 0 x))
    ([fe rt r]
     (let [rp (replicate rt fe)]
       (if (empty? r) [rp]
                      (if (= fe (first r))
                        (! fe (inc rt) (rest r))
                        (concat [rp] (! (first r) 0 r))
                        )
                      )
       ))
    ))

(defcheck solution-52407d62
  (fn [s]
    (partition-by identity s)))

(defcheck solution-52b42f90
  (fn [seq1]
    (loop [result [] subresult [(first seq1)] elements (rest seq1)]
      (if (empty? elements)
        (conj result subresult)
        (if (= (first subresult) (first elements))
          (recur result (conj subresult (first elements)) (rest elements))
          (recur (conj result subresult) [(first elements)] (rest elements))
          )
        )
      )
    ))

(defcheck solution-530a5a79
  (fn f [[x & _ :as s]]
    (if x
      (apply #(cons % (f %2)) (split-with #{x} s)))))

(defcheck solution-534e35b5
  (fn [cl] (loop [c cl, r []] (let [cp (for [v c :while (= (first c) v)] v)]
                                (if (empty? c) r
                                               (recur (drop (count cp) c) (conj r cp)))
                                ))))

(defcheck solution-5361116
  (fn pack-seq [xx]
    (reverse (reduce

               (fn [a x]
                 (if (= (first (first a)) x)
                   (cons (cons x (first a)) (rest a))
                   (cons (list x) a)))

               '()
               xx))))

(defcheck solution-5363837f
  (fn [seq]
    (partition-by identity seq)))

(defcheck solution-53813a48
  (fn [xs]
    (loop [xs xs
           acc '()]
      (if-let [[x & more-xs] (seq xs)]
        (if-let [[group & more-acc] (seq acc)]
          (if (= x (first group))
            (recur more-xs (cons (conj group x) more-acc))
            (recur more-xs (cons [x] acc)))
          (recur more-xs (cons [x] acc)))
        (reverse acc)))))

(defcheck solution-53f6fc3e
  (fn pack [seq]
    (reduce (fn [acc e] (if (= (last (last acc)) e)
                          (conj (vec (drop-last acc)) (conj (last acc) e))
                          (conj acc [e])))
      [] seq)))

(defcheck solution-5634c506
  (fn [x]
    (loop [x x v [] l nil c 0]
      (if (nil? (first x))
        (if (> c 1)
          (rest (concat v (list (repeat c l))))
          (rest (concat v (list (list l)))))
        (if (= (first x) l)
          (recur (rest x) v l (inc c))
          (recur (rest x)
            (if (> c 1)
              (concat v (list (repeat c l)))
              (concat v (list (list l))))
            (first x)
            1))))))

(defcheck solution-56d645b4
  #(reduce (fn [a b]
             (if (= (last (last a)) b)
               (conj (vec (butlast a)) (conj (last a) b))
               (conj a [b])
               )) [] %))

(defcheck solution-5921354
  (fn pack [lst]
    (let
     [get-rep
      (fn get-rep [lst x]
        (cond
          (not (= x (first lst))) '()
          (empty? (rest lst)) (list x)
          :else (cons x (get-rep (rest lst) x))))]
      (let
       [pack-it
        (fn pack-it [lst]
          (if (empty? lst)
            '()
            (let
             [gp (get-rep lst (first lst))]
              (let
               [lstp (drop (count gp) lst)]
                (cons gp (pack-it lstp))))))]
        (pack-it lst)))))

(defcheck solution-592aad21
  (fn pack [s]
    (if (seq s)
      (let [reps (take-while #(= % (first s)) s)]
        (lazy-seq (cons reps (pack (drop (count reps) s)))))
      '())))

(defcheck solution-5989e890
  partition-by #(-> %))

(defcheck solution-5a21ae9a
  (fn [sq]
    (let [aux
          (fn [[head & tail] last-seen curr acc]
            (cond
              (nil? head)
              (conj acc curr)

              (= head last-seen)
              (recur tail head (conj curr head) acc)

              :else
              (recur tail head (list head) (conj acc curr))))]

      (filter (complement empty?)
        (aux sq nil (list) [])))))

(defcheck solution-5ab56d85
  (fn [s]
    (letfn [(pck-seq [s curr-items result]
              (if (seq s)
                (let [first-item (first s)
                      curr-item (first curr-items)]
                  (if (= first-item curr-item)
                    (recur
                      (rest s)
                      (conj curr-items first-item)
                      result)
                    (recur
                      (rest s)
                      [first-item]
                      (conj result curr-items))))
                (conj result curr-items)))]
      (pck-seq (rest s) [(first s)] []))))

(defcheck solution-5ac99ec5
  (fn pack
    ([s] (pack s '()))
    ([s ret] (if (empty? s)
               (reverse ret)
               (recur (drop-while #(= (first s) %) s)
                 (conj ret (take-while #(= (first s) %) s)))))))

(defcheck solution-5bf590f9
  (fn dup-pack [mylist]
    (seq (loop [l mylist lastchar (first l) subseq '() final []]
           (if (empty? l) (conj final subseq)
                          (if (= lastchar (first l))
                            (recur (rest l) (first l) (conj subseq (first l)) final)
                            (recur (rest l) (first l) (list (first l)) (conj final subseq))))))))

(defcheck solution-5c0f742
  (fn [coll]
    (->
      (fn [coll p acc res]
        (if (empty? coll)
          (conj res acc)
          (let [[f & r] coll]
            (if (= f p)
              (recur r p (conj acc f) res)
              (recur r f [f] (conj res acc))))))
      (apply coll (gensym) [] [] [])
      next)))

(defcheck solution-5c402e3b
  (fn pack-seq [xs]
    (loop [x xs r '()]
      (if (empty? x)
        (reverse r)
        (recur (rest x) (if (= (first x) (-> r first first))
                          (cons (cons (first x) (first r)) (rest r))
                          (cons (cons (first x) '()) r)))))))

(defcheck solution-5e07db2c
  #(reverse (reduce (fn [[acc & rest] elem]
                      (cond
                        (empty? acc)
                        (list (list elem))
                        (= (first acc) elem)
                        (cons (cons elem acc) rest)
                        :else
                        (cons (list elem ) (cons acc rest))))
              () %)))

(defcheck solution-5e4c6084
  (fn foo [xs]
    (if (seq xs)
      (cons (take-while #(= % (first xs)) xs)
        (foo (drop-while #(= % (first xs)) xs))))))

(defcheck solution-5ecb0a5a
  (fn [x]
    (loop [last nil
           todo x
           res []]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (next todo)]
          (if (= f last)
            (recur f r (conj (pop res) (conj (peek res) f)))
            (recur f r (conj res (list f)))
            )
          )
        )
      )
    ))

(defcheck solution-5ee38940
  (fn [input]
    (loop [s (seq input) a nil]
      (if (empty? s)
        (reverse a)
        (if (= (first s) (first (first a)))
          (recur (rest s) (conj (rest a)  (conj (first a) (first s))))
          (recur (rest s) (conj a (list  (first s)))))))))

(defcheck solution-5fb16f4
  (fn un [p]
    (if (< (count p) 2)
      (list p)
      (let [f (first p)
            s (second p)
            n (next p)
            unn (un n)]
        (if (= f s)
          (if-not (= f (first unn))
            (cons (cons f (first unn)) (next unn))
            (cons (list f s) (next unn)))
          (cons (list f) unn))))))

(defcheck solution-615754cb
  (fn pack [coll]
    (if (empty? coll)
      '()
      (cons (take-while (partial = (first coll)) coll)
        (pack (drop-while (partial = (first coll)) coll))))))

(defcheck solution-622eaa2f
  (fn [lst]
    (reverse
      (reduce
        (fn [s i]
          (if (= (first (first s)) i)
            (conj (rest s) (conj (first s) i)
              )
            (conj s (list i))))
        '()
        lst))))

(defcheck solution-627f2ae9
  (fn pack [s]
    (partition-by identity s)))

(defcheck solution-62b5af80
  (fn pack [coll]
    (partition-by identity coll)))

(defcheck solution-633e0431
  (partial (fn [acc ls]
             (if (empty? ls)
               acc
               (let [v (first ls)
                     nacc (conj acc (take-while #(= % v) ls))]
                 (recur nacc (drop-while #(= % v) ls))))) []))

(defcheck solution-63f1279c
  (fn
    [coll]
    (letfn
     [(pack-seq [coll]
        (if (not (empty? coll))
          (cons
            (take-while
              #(= % (first coll))
              coll)
            (pack-seq (drop-while
                        #(= % (first coll))
                        coll)))))]
      (pack-seq coll))))

(defcheck solution-63fef8b
  (fn [coll]
    (loop [coll coll acc []]
      (if (empty? coll)
        acc
        (let [group (take-while #{(first coll)} coll)]
          (recur
            (drop (count group) coll)
            (conj acc group)))))))

(defcheck solution-646904a0
  (fn [coll]
    (loop [[x & xs] coll
           r []]
      (if (nil? x)
        (reverse r)
        (if (empty? r)
          (recur xs [[x]])
          (let [y (first r)]
            (if (= x (last y))
              (recur xs (conj (rest r) (conj y x)))
              (recur xs (conj r [x])))))))))

(defcheck solution-65446791
  #(
    (fn noconsec [in out]
      (if (empty? in)
        (reverse out)
        (if (= (first in) (first (first out)))
          (noconsec
            (rest in)
            (conj (rest out) (conj (first out) (first in))))
          (noconsec (rest in) (conj out (list(first in)))))
        )
      )
    (rest %) (list (list (first %)))
    ))

(defcheck solution-65b93d47
  (fn [input]
    (partition-by identity input)))

(defcheck solution-6630807e
  (fn [xs]
    (reduce
      (fn [ys x]
        (if (= (last (last ys)) x)
          (update-in ys [(dec (count ys))] (fn [zs] (conj zs x)))
          (conj ys [x])))
      [] xs)))

(defcheck solution-66fdcee3
  (fn [c] (reverse (reduce
                     (fn [a b] (if (= (first (first a)) b)
                                 (cons (cons b (first a)) (rest a))
                                 (cons (cons b '()) a)))
                     '()
                     c))))

(defcheck solution-68161cc5
  (fn f
    ([[a & b]] (reverse (f b (list (list a)))))
    ([s acc]
     (if (empty? s) acc
                    (let [[a & b] s]
                      (if (= a (first (first acc)))
                        (f b (cons (cons a (first acc))
                               (rest acc)))
                        (f b (cons (list a) acc))))))))

(defcheck solution-68332f20
  (fn pack
    [s]
    (loop
     [in s, subList '(), out []]
      (if (empty? in)
        (if (empty? subList)
          out
          (conj out subList))
        (if (or (empty? subList) (= (first in) (first subList)))
          (recur (rest in) (conj subList (first in)) out)
          (recur (rest in) (list (first in)) (conj out subList)))))))

(defcheck solution-6c86c236
  (fn foo [s] (lazy-seq
                (loop [c 1 f (first s) r (next s)]
                  (cond
                    (empty? s) nil
                    (nil? r) (list (list f))
                    (= f (first r)) (recur (inc c) f (rest r))
                    :else (cons (repeat c f) (foo r)))))))

(defcheck solution-6cafb992
  (fn f [[a & z :as s]]
    (lazy-seq
      (when a
        (let [[p q] ((juxt take-while drop-while)
                     #(= a %) s)]
          (cons p (f q)))))))

(defcheck solution-6cea8df8
  (fn [values]
    (loop [values (rest values) current (first values) pack [(first values)] result []]
      (if (empty? values)
        (if (empty? pack)
          result
          (conj result pack))
        (let [item (first values)]
          (recur (rest values) item
            (if (= current item)
              (conj pack item)
              [(first values)])
            (if (= current item)
              result
              (conj result pack))))))))

(defcheck solution-6d0cd1dc
  (fn [seq]
    (letfn [(replace-first [x list]
              (cons x (rest list)))
            ]
      (if (empty? seq)
        seq
        (reverse (reduce (fn [acc x]
                           (if (= (first (first acc)) x)
                             (replace-first (cons x (first acc)) acc)
                             (cons (list x) acc)))
                   (list (list (first seq)))
                   (rest seq)))))))

(defcheck solution-6daa537c
  (fn c [s]
    (loop [s s
           r ()]
      (cond
        (empty? s) (reverse r)
        (or (empty? r) (= (ffirst r) (first s))) (recur (rest s) (conj (rest r) (conj (first r) (first s))))
        :else (recur (rest s) (conj r (list (first s))))))))

(defcheck solution-6ddeb5cc
  (fn [s]
    (loop [rr [] p (first s) r 1 ss (rest s)]
      (if (empty? ss)
        (conj rr (repeat r p))
        (let [f (first ss)]
          (if (not= f p)
            (recur (conj rr (repeat r p)) f 1 (rest ss))
            (recur rr p (inc r) (rest ss))))))))

(defcheck solution-7020de3c
  (fn [s]
    (loop [l nil ss s out []]
      (if (empty? ss)
        out
        (let [e (first ss)]
          (recur e (rest ss)
            (if (= e l)
              (assoc out (dec (count out)) (conj (last out) e))
              (conj out [e]))))))))

(defcheck solution-71158ffe
  (fn [coll]
    (loop [input (rest coll)
           i 1
           last (first coll)
           result []]
      (if (empty? input)
        (conj result (repeat i last))
        (recur (rest input)
          (if (= (first input) last) (inc i) 1)
          (first input)
          (if (= (first input) last)
            result
            (conj result (repeat i last))))))))

(defcheck solution-7124153
  (fn [coll]
    (let [ubd (gensym "unbound")]
      (loop [es coll
             ep ubd
             g nil
             r nil]
        (if (empty? es)
          (when g (reverse  (conj r g)))
          (let [[e & re] es]
            (cond
              (= ep ubd) (recur re e (list e) r)
              (= e ep) (recur re e (conj g e) r)
              :default (recur re e (list e) (conj r g)))))))))

(defcheck solution-71648e6
  (fn pack-duplicates [seq]
    (partition-by identity seq)))

(defcheck solution-724a10b8
  reduce #(if (= (peek (peek %)) %2)
            (conj (vec (butlast %)) (conj (peek %) %2))
            (conj % (conj [] %2))) [])

(defcheck solution-7299a4fe
  #(reduce (fn[a b]
             (if (= b (first (last a)))
               (update-in a [(dec (count a))] conj b)
               (conj a [b]))) [] %))

(defcheck solution-72a2505c
  (fn kerry [s]
    (if (empty? s) s
                   (loop [first-el (first s)
                          new-set (list first-el)
                          remainder (rest s)]
                     (if (= first-el (first remainder))
                       (recur first-el (cons first-el new-set) (rest remainder))
                       (cons new-set (kerry remainder)))))))

(defcheck solution-73aeb9ea
  (fn pack-dups [s]
    (if (seq s)
      (let [[ff rr] (split-with #(= (first s) %) s)]
        (cons ff (pack-dups rr))
        )
      )
    ))

(defcheck solution-74d1131f
  reduce (fn [x y] (if (= y (last (last x))) (assoc x (dec (count x)) (conj (last x) y))  (conj x [y]))) [])

(defcheck solution-76fd7630
  #(->> %
     (reduce (fn [[agg prev-l] n]
               (if (= (first prev-l) n)
                 [agg (cons n prev-l)]
                 [(conj agg prev-l) (list n)])) [[] ()])
     (apply conj)
     (drop 1)))

(defcheck solution-773f4d2f
  (fn pack-seq [s]
    (let [split-on-change (fn [s] (split-with (fn [x] (= (first s) x)) s))]
      (if (empty? s)
        s
        (let [[same rest] (split-on-change s)]
          (cons same (pack-seq rest))
          )))))

(defcheck solution-77ecaa9d
  (fn dup-seqs [[hd & tl]]
    (let [dups (conj (take-while #(= % hd) tl) hd)
          remaining (drop-while #(= % hd) tl)]
      (if (empty? remaining)
        (list dups)
        (cons dups (dup-seqs remaining))))))

(defcheck solution-78be259f
  #(letfn [(worker [l i n]
             (if (empty? l)
               (conj n i)
               (if (= (first l) (first i))
                 (recur (rest l) (conj i (first l)) n)
                 (recur (rest l) (list (first l)) (conj n i)))))]
     (if (empty? %1)
       %1
       (worker (rest %1) (list (first %1)) []))))

(defcheck solution-7b509623
  (fn pack
    [s]
    (if (empty? s)
      '()
      (let [head (first s)
            [t-part d-part] (split-with #(= head %) s)]
        (cons t-part (pack d-part))))))

(defcheck solution-7bf9724b
  (fn pack-seq [a-seq]

    ((fn do-pack [el  rest-seq cnt  result]
       (if (seq rest-seq)
         (let [next-el (first rest-seq)]
           (if (= el next-el)
             (recur next-el (rest rest-seq) (inc cnt) result)
             (recur next-el (rest rest-seq) 1 (conj result (apply list (repeat cnt el))))
             )
           )
         ;-- rest-seq is empty
         (reverse (conj result (apply list (repeat cnt el))))
         )
       ) (first a-seq) (rest a-seq) 1 () )
    ))

(defcheck solution-7c946f74
  (fn f [l] (if (empty? l) () (let [c (first l)] (conj (f (drop-while #(= % c) l)) (take-while #(= % c) l))))))

(defcheck solution-7cca5cd
  (fn pack [s]
    (loop [current-s s
           accum []]
      (if (empty? current-s)
        accum
        (let [current-element (first current-s)
              is-current-element? #(= current-element %)]
          (recur
            (drop-while is-current-element? current-s)
            (conj accum (take-while is-current-element? current-s))
            ))))))

(defcheck solution-7cdac6a4
  (fn [x] (loop [acc [] part x] (letfn [(pred [el] (= el (first part)))] (if (empty? part) acc (recur (conj acc (take-while pred part)) (drop-while pred part)))))))

(defcheck solution-7dc62c09
  (fn [coll]
    (loop [xs coll ret '() run '()]
      (cond
        (empty? xs)  (reverse (cons run ret))
        (empty? run) (recur (rest xs) ret (cons (first xs) run))
        (= (first xs) (first run)) (recur (rest xs) ret (cons (first xs) run))
        :else (recur xs (cons run ret) '())))))

(defcheck solution-7e6b4113
  (fn pack [x] (partition-by identity x)))

(defcheck solution-7e8fd094
  (fn [input]
    (rest (reduce (fn pack[l x]
                    (if (= (last (last l)) x)
                      (concat
                       (drop-last l)
                       (list (concat (last l) (list x))))
                      (concat l (list (list x))))) (list (list)) input))))

(defcheck solution-7e9843f
  (fn [coll]
    (partition-by identity coll)))

(defcheck solution-7f46cad2
  (letfn [(f [s] (if (seq s) (let [[n r] (split-with #(= % (first s)) s)] (cons n (f r)))))] f))

(defcheck solution-802e1419
  reduce (fn [l r]
           (if (contains? (set (last l)) r)
             (concat (drop-last l) (list (conj (last l) r)))
             (concat l (list (list r))))) ())

(defcheck solution-80c448ba
  reduce #(if (= (first (last %1)) %2) (conj (pop %1) (conj (last %1) %2)) (conj %1 [%2]) ) [])

(defcheck solution-816705f9
  (fn pack [s]
    (if (empty? s) []
                   (let [[packed,others] (split-with (partial = (first s)) s)]
                     (cons packed (pack others))))))

(defcheck solution-8259b283
  (fn [z] (next ((partial
                   (fn [acc prev coll]
                     (if (nil? coll)
                       (reverse (conj acc prev))
                       (if (= (first coll) (first prev))
                         (recur acc (conj prev (first coll)) (next coll))
                         (recur (conj acc prev) (list (first coll)) (next coll))
                         )
                       )
                     ) '() '()
                   ) z))))

(defcheck solution-82631eb
  (fn [xs] (let [[[h & r] n] (reduce (fn [[vals cur] x]
                                       (if (= (first cur) x)
                                         [vals (conj cur x)]
                                         [(conj vals cur) [x]])) [[] []] xs)]
             (if (> (count n) 0)
               (conj (vec r) n)
               r))))

(defcheck solution-846007e9
  (partial partition-by identity))

(defcheck solution-84830cdf
  (fn pack-seq [seq]
    (partition-by identity seq)))

(defcheck solution-8493f1e6
  (fn [s]
    (reverse
      (reduce
        #(if (= %2 (-> %1 first first))
           (conj (rest %1) (conj (first %1) %2))
           (conj %1 [%2])) [] s))))

(defcheck solution-8577d904
  reduce (fn [coll item]
           (if (= (last (last coll)) item)
             (concat (butlast coll) [(conj (last coll) item)])
             (concat coll [[item]]))) [])

(defcheck solution-866f28ad
  (fn packduplicates [x]
    (let [equiv (fn [coll elem]
                  (if (and (seq coll) (= elem (first (peek coll))))
                    (conj (pop coll) (conj (peek coll) elem))
                    (conj coll  (vector elem))))]
      (reduce equiv [] x))))

(defcheck solution-86dd92e5
  (fn [input] (partition-by identity input) ))

(defcheck solution-86f56cba
  #(partition-by (fn [x] x) %))

(defcheck solution-87d91947
  #(loop [[f r] (split-with #{(first %)} %) res []]
     (if (first r)
       (recur (split-with #{(first r)} r) (conj res f))
       (conj res f))))

(defcheck solution-8858d754
  (fn [in]
    (rest (reverse (reduce (fn [a b]
                             (if (= (first (first a)) b)
                               (cons (cons b (first a))
                                 (rest a))
                               (cons (cons b '())
                                 a))) '(()) in)))))

(defcheck solution-89bdce48
  (fn group [l]
    (letfn [(span [f l]
              (if (empty? l)
                [l l]
                (let [[x & xs] l]
                  (if (f x)
                    (let [[ys zs] (span f xs)]
                      [(cons x ys) zs])
                    [[] l]))))]
      (if (empty? l)
        []
        (let [[x & xs] l
              [ys zs] (span #(= x %) xs)]
          (cons (cons x ys) (group zs)))))))

(defcheck solution-8a2dac14
  (fn [x]
    ((fn pack [x y]
       (if-let [[f & r] x]

         (if (= (last y) f)
           (pack r (cons f y))
           (cons y (pack r (list f)))
           )
         (if (= (last y) x)
           (cons x y)
           (list y)
           )
         )
       ) (next x) (list (first x)))
    ))

(defcheck solution-8a30c7c8
  reduce #(if (= (last (last %)) %2) (conj (vec (reverse (rest (reverse %)))) (conj (last %) %2)) (conj % (list %2))) [])

(defcheck solution-8a384056
  (fn f [sq]
    (if (empty? sq)
      sq
      (let [fst (first sq)
            grp (take-while #(= fst %) sq)
            rest (drop-while #(= fst %) sq)]
        (cons grp (f rest))
        ))))

(defcheck solution-8b2759a1
  (fn [a-seq]
    (reverse
      (loop [new-seq (seq '()) org-seq a-seq]
        (if (empty? org-seq)
          new-seq
          (let [xs (split-with #(= % (first org-seq)) org-seq)]
            (recur (conj new-seq (first xs)) (last xs))))))))

(defcheck solution-8ba658c4
  (fn [coll]
    (loop [result []  c coll]
      (let [current (first c)]
        (if (empty? c)
          result
          (recur (conj result (take-while #(= current %) c)) (drop-while #(= current %) c))
          )))))

(defcheck solution-8c68390c
  partition-by (fn [x] x))

(defcheck solution-8ceb0832
  #(loop [r [] t [] p nil l %]
     (if (= 0 (count l))
       (conj r t)
       (recur
         (if (or (empty? t) (= p (nth l 0)))  r (conj r t))
         (if (= p (nth l 0)) (conj t p) [(nth l 0)])
         (nth l 0) (rest l)
         )
       )))

(defcheck solution-8db3ce5
  #(reverse
     (reduce (fn [a b]
               (if (= [] a)
                 (list (list b))
                 (if (= (ffirst a) b)
                   (cons (cons b (first a)) (rest a))
                   (concat (list (list b)) a )))) [] %)))

(defcheck solution-8dd136c2
  #(letfn [(pack [acc inner coll]
             (if (empty? coll)
               (conj acc inner)
               (let [curr (first coll)]
                 (if (= (first inner) curr)
                   (recur acc (conj inner curr) (rest coll))
                   (recur (conj acc inner) (vector curr) (rest coll))))))]
     (pack [] (vector (first %)) (rest %))))

(defcheck solution-8df2c1cc
  (fn pack [lst]
    (loop [A (rest lst), result nil, Q (list (first lst))]
      (let [same (= (first A) (first Q))]
        (if (empty? A)
          (reverse (cons Q result))
          (recur
            (rest A)
            (if same result (cons Q result))
            (if same (cons (first A) Q) (list (first A)))))))))

(defcheck solution-8e2f4558
  #(reduce
     (fn [acc x]
       (if
        (= (first (last acc)) x)
         (concat (butlast acc) (list (concat (last acc) (list x))))
         (concat acc (list (list x)))))
     ()
     %))

(defcheck solution-8e36329c
  #(partition-by identity %))

(defcheck solution-8f4d790a
  (fn y [s]
    (if (empty? s)
      s
      (let [[h t] (split-with #(= (first s) %) s)]
        (cons h (y t))))))

(defcheck solution-8fe1ffba
  (fn [s]
    (loop [src s
           current nil
           occurences 0
           result []]
      (if (seq src)
        (cond
          (nil? current) (recur (rest src) (first src) 1 result)
          (= (first src) current) (recur (rest src) current (inc occurences) result)
          :else (recur (rest src) (first src) 1 (conj result (repeat occurences current))))
        (conj result (repeat occurences current))))))

(defcheck solution-90ba5a42
  (fn pack [s]
    (loop [s s prev `prev# acc []]
      (if (empty? s) acc
                     (let [[x & xs] s]
                       (if (= x prev)
                         (recur xs x (conj (pop acc) (conj (peek acc) x)))
                         (recur xs x (conj acc [x]))))))))

(defcheck solution-9161f824
  (fn pack-sequence
    ([l] (pack-sequence (rest l) [[(first l)]]))
    ([l resp]
     (if (= l [])
       resp
       (let [f (first l)]
         (if (= (last (last resp)) f)
           (pack-sequence
             (rest l)
             (assoc resp (dec (count resp)) (conj (last resp) f)))
           (pack-sequence
             (rest l)
             (conj resp [f]))))))))

(defcheck solution-917a51fd
  #(loop [xs %
          acc ()]
     (if (empty? xs)
       (reverse acc)
       (let [[ys rest] (split-with #{(first xs)} xs)]
         (recur rest (cons ys acc))))))

(defcheck solution-93111514
  #(loop [xs %,res ()]
     (if (empty? xs) (reverse res)
                     (let [x (first xs),coll (rest xs),resf (first res)]
                       (if (= x (first resf))
                         (recur coll (conj (rest res) (conj resf x) ))
                         (recur coll (conj res (list x))))))))

(defcheck solution-9334c19f
  (fn pack [[f & rst]]
    (if (nil? f) '()
                 (let [r (pack rst)]
                   (if (= f (first (first r)))
                     (cons (cons f (first r)) (rest r))
                     (cons (list f) r))))))

(defcheck solution-947517a8
  (fn [l]
    (reverse (loop [l l result []]
               (if (empty? l) result
                              (recur
                                (rest l)
                                (if
                                 (= (first l) (first (first result)))
                                  (conj (rest result) (conj (first result) (first l)))
                                  (conj result [(first l)]))))))))

(defcheck solution-947f5390
  (fn [arr]
    (loop [res '()
           arr arr]
      (if (empty? arr)
        (reverse res)
        (recur
          (->> arr (take-while #(= (first arr) %)) list (into res))
          (drop-while #(= (first arr) %) arr))))))

(defcheck solution-96637b42
  (fn part-dupl-e2
    ([coll]
     (part-dupl-e2 coll []))
    ([coll build]
     (if (seq coll)
       (let [part (vec (take-while #(= % (first coll)) coll))]
         (part-dupl-e2 (subvec coll (count part))
           (conj build part)))
       build))))

(defcheck solution-96864d6a
  (fn [col]
    (loop [col1 [] col2 col currentv (first col)  currentlist [] ]
      (if (empty? col2)
        (conj col1 currentlist)
        (let [same (= (first col2) currentv)]
          (recur
            (if same  col1 (conj col1 currentlist))
            (rest col2)
            (if same  currentv (first col2 ))
            (if same (conj currentlist currentv) [(first col2 )]  )
            )
          )
        )
      )
    ))

(defcheck solution-96c69fb8
  (fn packit-helper
    [accum coll]
    (if (empty? coll)
      (reverse accum)
      (recur
        (conj accum (take-while #(= (first coll) %) coll))
        (drop-while #(= (first coll) %) coll)))) '())

(defcheck solution-9704daf3
  (fn [c]
    (reduce
      #(let [l (last %1)]
         (if (= (last l) %2)
           (conj (vec (drop-last %1)) (conj l %2))
           (conj %1 [%2])
           )
         )
      []
      c
      )
    ))

(defcheck solution-9750d52
  (fn this
    ([xs] (this xs []))
    ([xs acc]
     (if (empty? xs)
       acc
       (let [[same diff] (split-with #(= % (first xs)) xs)]
         (recur diff (conj acc same)))))))

(defcheck solution-981c7ded
  (fn [x]
    (partition-by identity x)))

(defcheck solution-982a5887
  (fn [l]
    (loop [last_seq '()
           all_seqs '()
           [f & args :as my-l] l
           last-elem f]
      (if (empty? my-l)
        (if (empty? last_seq)
          all_seqs
          (concat all_seqs (list last_seq)))
        (if (= last-elem f)
          (recur (cons f last_seq) all_seqs args f)
          (recur (list f) (concat all_seqs (list last_seq)) args f))))))

(defcheck solution-9875a52e
  (fn [coll]
    (loop [left (rest coll) result [] current [(first coll)]]
      (if (empty? left)
        (if (empty? result) result (conj result current))
        (let [current-item (first left)]
          (let [consec? (= current-item (last current))]
            (if consec?
              (recur (rest left) result (conj current current-item))
              (recur (rest left) (conj result current) [current-item]))))))))

(defcheck solution-98d01757
  (fn [xs]
    (reduce (fn [acc x] (let [lacc (peek acc)
                              lelem (get lacc 0 nil)]
                          (if (= lelem x)
                            (conj (pop acc) (conj lacc x))
                            (conj acc [x])))) [] xs)))

(defcheck solution-99411d79
  (fn [sq]
    (letfn [
            (helper1 [sq] (split-with #(= (first sq) %) sq))
            (helper2 [rv sq] (if (empty? sq) rv (helper2 (cons (first (helper1 sq)) rv) (second (helper1 sq)))))

            ] (reverse (helper2 '() sq)))))

(defcheck solution-99e7c85c
  partition-by #(do %))

(defcheck solution-9a9a6b74
  (fn pack
    [s]
    (loop [s      s
           prev   (first s)
           res    []
           curres []]
      (let [cur (first s)]
        #_(prn res)
        (cond (empty? s)   (conj res curres)
              (= cur prev) (recur (rest s) cur res (conj curres cur))
              :else        (recur (rest s) cur (conj res curres) [cur]))))))

(defcheck solution-9b00e3c6
  (fn problem31-compress-sequence [xs]
    (reverse (reduce
               (fn [agg now]
                 (if (and (not (empty? agg)) (= (first (first agg)) now))
                   (conj (rest agg) (conj (first agg) now))
                   (conj agg [now])))
               '()
               xs
               ))))

(defcheck solution-9cc4d180
  (letfn [(rdc-blks
            [acc b]
            (let [top-ls (peek acc)]
              (if (= b (first top-ls))
                (conj
                  (pop acc)
                  (cons b top-ls))
                (conj acc (list b)))
              ))]
    (fn [xs]
      (if (empty? xs) xs
                      (reduce
                        rdc-blks
                        [(list (first xs))]
                        (rest xs))
                      ))))

(defcheck solution-9d40f602
  (fn pack-seq [coll]
    (if (empty? coll)
      ()
      (lazy-seq
        (loop [packed [] [fst & rst] coll]
          (if (or (empty? rst) (not (= fst (first rst))))
            (cons (conj packed fst) (pack-seq rst))
            (recur (conj packed fst) rst)))))))

(defcheck solution-9da512b2
  #(loop [x (rest %) y (vector (first %)) ans []] (if-not (empty? x)

                                                    (if (= (first x) (first y)) (recur (rest x) (conj y (first x)) ans)
                                                                                (recur (rest x) (vector (first x)) (conj ans y))
                                                                                )
                                                    (conj ans y)
                                                    )))

(defcheck solution-9db92140
  (fn [s]
    (loop [res [] left s]
      (if
       (empty? left) res
                     (recur (concat res [(take-while #(= (first left) %) left)]) (drop-while #(= (first left) %) left))))))

(defcheck solution-9f7ec78b
  (fn [col]
    (rest
      (reverse
        (reduce
          #(if (= (first (first %1)) %2 )
             (cons (cons %2 (first %1) ) (rest %1) )
             (cons (cons %2 '()) %1 )
             )
          '(())
          col
          )
        )
      )
    ))

(defcheck solution-9fcdb079
  (fn [x]
    (loop [s x r []]
      (if (empty? s)
        r
        (recur
          (drop-while #(= (first s) %) s)
          (conj r (take-while #(= (first s) %) s)))))))

(defcheck solution-a12034b8
  (fn pack- [coll]
    "31. Write a function which packs consecutive duplicates into sub-lists."
    (let [[head tail] (split-with #(= (first coll) %) coll)]
      (if (empty? tail)
        (list head)
        (cons head (pack- tail))))))

(defcheck solution-a1bfde80
  partition-by (fn[x]x))

(defcheck solution-a1eac6db
  #(partition-by identity  %))

(defcheck solution-a21a4c86
  (fn [input]
    (loop [xs input, result []]
      (let [x (first xs)
            [matches remainder] (split-with #(= x %) xs)]
        (if (empty? xs)
          result
          (recur remainder (conj result matches))
          )
        )
      )
    ))

(defcheck solution-a2b64dc5
  (partial reduce (fn [xs x] (let
                              [ys (last xs)
                               y  (last ys)
                               zs (-> xs butlast vec)]
                               (if (= x y)
                                 (conj zs (conj ys y))
                                 (conj (if (empty? ys) zs xs) [x]))))
    [[]]))

(defcheck solution-a36bb989
  (fn [xs]
    (loop [xs xs
           curr []
           result []
           last nil]
      (if-let [f (first xs)]
        (if (= f last)
          (recur (rest xs) (conj curr f) result f)
          (if (empty? curr)
            (recur (rest xs) [f] result f)
            (recur (rest xs) [f] (conj result curr) f)))
        (if (empty? curr)
          result
          (conj result curr))))))

(defcheck solution-a3ef9402
  (fn packed [coll]
    (partition-by identity coll)))

(defcheck solution-a45fdb21
  (fn [s]
    (loop [s_ s ret '() cur nil]
      (cond
        (and (not s_) (not cur))
        (into '() ret)
        (not s_)
        (recur s_ (conj ret (into '() cur)) nil)
        (and s_ cur (not= (first s_) (first cur)))
        (recur
          (next s_)
          (conj ret (into '() cur))
          (list (first s_)))
        :else
        (recur (next s_) ret (conj cur (first s_)))))))

(defcheck solution-a5df102c
  (fn [coll] (loop [c coll l (first coll) crnt '() o '()]
               (if (empty? c)
                 (reverse (conj o crnt))
                 (let [x (first c)]
                   (recur (rest c)
                     x
                     (if (= x l)
                       (conj crnt x)
                       (list x))
                     (if (= x l)
                       o
                       (conj o crnt))))))))

(defcheck solution-a656f87f
  (fn duplicados [coll]
    (cond
      (empty? coll) nil
      (= 1 (count coll)) (list (list (first coll)))
      :else (let [target (first coll)
                  primeira-lista (for [x coll :while (= target x)] x)]
              (cons primeira-lista (duplicados (nthnext coll (count primeira-lista))))))))

(defcheck solution-a73663ee
  #(reduce (fn [r x] (if (= (first (last r)) x)
                       (conj (vec (take (dec (count r)) r)) (conj (last r) x))
                       (conj r [x]))) [[(first %)]] (rest %)))

(defcheck solution-a7dc619b
  (fn [iput]

    (reverse  (reduce #(if (= (first (first  %1)) %2) (cons (cons %2 (first %1)) (rest %1)) (cons (list %2) %1))  []  iput)

      )))

(defcheck solution-a8e5f6bd
  ; because "partition-by identity" feels like cheating...
  (fn [s]
    (loop [left [] right s]
      (if (empty? right)
        left
        (recur (conj left (take-while #(= % (first right)) right) )
          (drop-while #(= % (first right)) right))))))

(defcheck solution-a9146eeb
  (fn pck [s]
    (partition-by identity s)))

(defcheck solution-ab3e6dd5
  (fn f [coll]
    (let [g (partial reduce
              (fn [[r l] e]
                (if (= (last l) e)
                  [r (conj l e)]
                  [(conj r l) [e]]))
              [[] nil])
          [r l] (g coll)]
      (next (conj r l))
      )))

(defcheck solution-ac15b69a
  (fn f [coll]
    (let [[xs tail] (split-with (partial = (first coll)) coll)]
      (if (empty? tail) [xs] (concat [xs] (f tail))))))

(defcheck solution-ac2b3a20
  (fn [coll] (letfn [(add-item [acc coll idx]
                       (conj acc (nth coll idx)))

                     (add-group [acc coll]
                       (conj acc (create-group coll)))

                     (decrease-coll [coll acc]
                       (drop (count (last acc)) coll))

                     (not-group-member? [idx coll]
                       (not= (first coll) (nth coll idx)))

                     (out-of-bounds? [idx coll]
                       (or (empty? coll) (>= idx (count coll))))

                     (create-group [coll] (loop [idx 0
                                                 coll coll
                                                 acc []]
                                            (if (or (out-of-bounds? idx coll)
                                                    (not-group-member? idx coll))
                                              acc
                                              (recur (inc idx) coll (add-item acc coll idx)))))
                     (process-coll [coll] (loop [coll coll
                                                 acc []]
                                            (let [coll' (decrease-coll coll acc)
                                                  acc' (add-group acc coll')]
                                              (if (empty? coll)
                                                acc
                                                (recur coll' acc')))))]
               (drop-last (process-coll coll)))))

(defcheck solution-ac8b4b28
  (fn pack [xs] (if (empty? xs) '() (let [[ys zs] (split-with #(= (first xs) %) xs)] (conj (pack zs) ys)))))

(defcheck solution-acda9e8e
  (fn pack [s]
    (loop [i s, acc '(), r []]
      (if (empty? i)
        (conj r acc)
        (if (or (empty? acc) (= (first i) (first acc)))
          (recur (rest i) (conj acc (first i)) r)
          (recur (rest i) (list (first i)) (conj r acc)))))))

(defcheck solution-adff868f
  (fn packseq ([x]
               (packseq (butlast x) (list (last x)) (list)))
    ([x y z]
     #_(println x)
     #_(println y)
     #_(println z)
     (if (= 0 (count x))
       (conj z y)
       (if (= 0 (count y))
         (recur (butlast x) (list (last x)) z)
         (if (= (last x) (first y))
           (recur (butlast x) (conj y (last x)) z)
           (recur (butlast x) (list (last x)) (conj z y))))))))

(defcheck solution-aee02638
  (fn f [coll]
    (loop [in coll
           curr ()
           res []]
      (if (seq in)
        (if (seq curr)
          (let [a (first curr)
                b (first in)]
            (if (= a b)
              (recur (next in)
                (cons (first in) curr)
                res)
              (recur (next in)
                (list (first in))
                (conj res curr))))
          (recur (next in)
            (cons (first in) curr)
            res))
        (conj res curr)))))

(defcheck solution-af67bf53
  (fn [coll]
    (loop
     [result '()
      lastcount 1
      coll coll
      ]
      (if-let [r (seq (rest coll))]
        (if (= (first coll) (first r))
          (recur result (+ 1 lastcount) r)
          (recur (concat result (list (repeat lastcount (first coll)))) 1 r)
          )
        (concat result (list (repeat lastcount (first coll))))
        )
      )
    ))

(defcheck solution-af749dca
  #(partition-by (set %) %))

(defcheck solution-b00608d3
  (fn p [[x :as s]]
    (if x
      (let [[d r] (split-with #(= % x) s)]
        (cons d (p r))))))

(defcheck solution-b17b31f6
  (fn p
    ([[h & t]] (p [] [h] t))
    ([a h t]
     (if-let [[f & n] t]
       (if (= (first h) f)
         (p a (conj h f) n)
         (p (conj a h) [f] n))
       (conj a h)))))

(defcheck solution-b18f36c7
  (fn[x]
    (loop [l x, v [], s [], f (first x)]
      (if (empty? l)
        (if (empty? s)
          (apply list v)
          (apply list (conj v (apply list s))))
        (if (= (first l) f)
          (recur (rest l) v (conj s (first l)) f)
          (recur (rest l) (conj v (apply list s))
            [(first l)] (first l))
          )))))

(defcheck solution-b1f68d26
  partition-by identity)

(defcheck solution-b21d9a15
  (fn [x]
    (let [belongs? (fn [x c] (or (empty? c) (= x (first c))))
          proc (fn proc[ [r & rs :as rr] x]
                 (if (belongs? x r)
                   (cons (cons x r) rs)
                   (cons (list x)   rr))) ]
      (reverse (reduce proc '(()) x)))))

(defcheck solution-b2447f39
  (fn [in-seq]
    ((fn process [in-vec res-vec]
       (if (empty? in-vec)
         res-vec
         (if (= (first in-vec) (-> res-vec last first))
           (do #_(println true in-vec)
               (recur (rest in-vec)
                 (update-in res-vec [(-> res-vec count dec)] #(conj % (first %)))))
           (do println false in-vec
               (recur
                 (rest in-vec)
                 (conj res-vec [(first in-vec)]))))
         )
       )
     in-seq [])
    ))

(defcheck solution-b36582ce
  #((fn [x y]
      #_(println x y)
      (if (empty? x)
        y
        (if (= (first x) (last (last y)))
          (recur (rest x) (conj (subvec y 0 (dec (count y))) (conj (last y) (first x))))


          (recur (rest x) (conj y [(first x)]))
          )
        )
      ) (rest %)  [[(first %)]]))

(defcheck solution-b36a2386
  (fn [s]
    (partition-by identity s)))

(defcheck solution-b3dbb650
  (fn [l] (reverse (loop [acc () s1 l]
                     (if (empty? s1)
                       acc
                       (recur
                         (if (or (empty? acc) (not= (first (first acc)) (first s1)))
                           (cons [(first s1)] acc)
                           (cons (cons (first s1) (first acc)) (rest acc))
                           )
                         (rest s1))
                       )))))

(defcheck solution-b46ad89b
  (fn grp [s]
    (loop [result []
           s s]
      (if-let [e (first s)]
        (recur (conj result (take-while #(= e %) s)) (drop-while #(= e %) s))
        result))))

(defcheck solution-b491a274
  (fn pack [coll]
    (loop [results [[(first coll)]] items (rest coll)]
      (if (empty? items)
        results
        (let [prev (last (last results)) current (first items)]
          (if (= prev current)
            (recur (conj (vec (butlast results)) (conj (last results) current)) (rest items))
            (recur (conj results [current]) (rest items))
            )
          )
        )
      )
    ))

(defcheck solution-b4a0034a
  (fn [a]
    (reverse (loop [ans '()
                    aa (concat (seq (next a)) (list 9999999999))
                    temp (list (first a))
                    pre (first a)]
               (if (empty? aa)
                 ans
                 (if (= pre (first aa))
                   (recur ans
                     (next aa)
                     (conj temp (first aa))
                     pre)
                   (recur (conj ans temp)
                     (next aa)
                     (list (first aa))
                     (first aa))))))))

(defcheck solution-b51b0355
  (fn pack[s]
    (partition-by identity s)))

(defcheck solution-b52173ea
  (fn [xs]
    (loop [curr (list (peek xs))
           ls (pop xs)
           return '()]
      (if (nil? (first ls))
        (conj return curr)
        (let [x (peek ls)]
          (if (= x (peek curr))
            (recur (conj curr x)
              (pop ls)
              return)
            (recur (list x)
              (pop ls)
              (conj return curr))))))))

(defcheck solution-b56babb9
  (fn [xs]
    (partition-by identity xs)))

(defcheck solution-b596ba71
  (fn pack [coll]
    (letfn [(group-run [elem grp coll]
              (cond
                (empty? coll) (list (cons elem grp))
                (= elem (first coll)) (group-run elem (cons elem grp) (rest coll))
                :else (cons (cons elem grp) (group-run (first coll) '() (rest coll)))))]
      (if (empty? coll)
        '()
        (group-run (first coll) '() (rest coll))))))

(defcheck solution-b7ae3596
  (fn f [xs]
    (let [g (fn [xs]
              (loop [ys (list (first xs)) zs (rest xs)]
                (if (= (first ys) (first zs))
                  (recur (conj ys (first zs)) (rest zs))
                  [ys zs])))
          [ys zs] (g xs)]
      (conj (if (empty? zs) zs (f zs)) ys))))

(defcheck solution-b7e49749
  (fn p [l]
    ((fn rpack [f r]
       (cond
         (empty? r) (cons f ())
         (= (first f) (first r)) (rpack (cons (first r) f) (rest r))
         :else (cons f (rpack (take 1 r) (rest r)))
         )
       ) (take 1 l) (rest l))
    ))

(defcheck solution-b8841507
  (fn pack [s] (partition-by identity s)))

(defcheck solution-bc5d3cd5
  #(loop [a [] s %]
     (if (empty? s) a
                    (recur
                      (conj a (take-while (partial = (first s)) s))
                      (drop-while (partial = (first s)) s)))))

(defcheck solution-bc942955
  (fn [s]
    (rest
      (loop [acc [] x s count 0 last nil]
        (cond
          (empty? x) (conj acc (repeat count last))
          (= last (first x)) (recur acc (rest x) (+ 1 count) last)
          :else (recur (conj acc (repeat count last)) (rest x) 1 (first x)))))))

(defcheck solution-bdd1112b
  (fn
    [aseq]
    (loop [lseq aseq el (first lseq) acc []]
      (if (empty? lseq)
        acc
        (let [nseq (drop-while #(= el %) lseq)]
          (recur
            nseq
            (first nseq)
            (conj acc (take-while #(= el %) lseq))))))))

(defcheck solution-be8dda5e
  (fn [lst]
    (reverse
      (loop [lst lst p nil ret '()]
        (if (empty? lst)
          ret
          (if (= p (first lst))
            (recur (rest lst) p
              (conj (rest ret) (conj (first ret) (first lst))))
            (recur (rest lst) (first lst)
              (conj ret (list (first lst))))))))))

(defcheck solution-beb4eeb2
  (fn pack-seq [s]
    (when (seq s)
      (let [xs (take-while (partial = (first s)) s)]
        (cons xs (pack-seq (drop (count xs) s)))))))

(defcheck solution-bfc869eb
  (letfn [(fun [c] (if (empty? c) nil (let [f (first c) [p s] (split-with #(= f %) c)] (cons p (fun s))  )))] fun))

(defcheck solution-bff0976a
  (fn [sq]
    ((fn [sq r pc group]
       (cond
         (empty? sq) (concat (rest r) [group])
         (= (first sq) pc) (recur (rest sq)
                             r pc
                             (conj group pc))
         :else (recur (rest sq)
                 (concat r [group])
                 (first sq)
                 (list (first sq)))))
     sq [] nil nil)))

(defcheck solution-c00fdf83
  (fn pack [s]
    (reduce #(if (seq %)
               (if (= (first (last %)) %2)
                 (concat (butlast %) (list (conj (last %) %2)))
                 (concat % (list (list %2))))
               (concat % (list (list %2))))
      '()
      s)))

(defcheck solution-c024f9a3
  (fn [x] (partition-by #(or(= % 1)(= % :a)(= % :c)(= % [1 2])) x)))

(defcheck solution-c030e2b3
  (fn [s]
    (reverse (loop [s (rest s)
                    p (first s)
                    r `((~(first s)))]
               (if (empty? s)
                 r
                 (recur (rest s) (first s) (if (= p (first s))
                                             (conj (rest r) (conj (first r) p))
                                             (conj r `(~(first s))))))))))

(defcheck solution-c04f4727
  (fn pack-part
    [coll]
    {:pre [(or (sequential? coll) (string? coll) (nil? coll))]}
    (partition-by identity coll)))

(defcheck solution-c0a6967b
  (fn packsequnce
    [sequence]
    ((fn packSeq
       [sequence previous acc]
       (cond
         (nil? sequence) (cons acc ())
         :else (if-not (= previous (first sequence))
                 (cond
                   (nil? acc) (packSeq (next sequence)
                                (first sequence)
                                (cons (first sequence) ()))
                   :else (cons acc (packSeq (next sequence)
                                     (first sequence)
                                     (cons (first sequence) ())
                                     )
                           )
                   )
                 (packSeq (next sequence) previous (cons (first sequence) acc))
                 )
         )
       )
     sequence nil nil)))

(defcheck solution-c1696004
  (fn [x] (partition-by identity x)))

(defcheck solution-c1a5e89
  (fn pack [x]
    (when-let [s (seq x)]
      (letfn [(m [n] (= (first s) n))]
        (cons (take-while m s) (pack (drop-while m s)))))))

(defcheck solution-c1e54d8c
  reduce #(if (= (first (last %1)) %2)
            (conj (into [] (butlast %1)) (conj (last %1) %2))
            (concat %1 [[%2]])
            ) [])

(defcheck solution-c470af9e
  #(rest(reverse(last(reduce (fn [[a [h & r]] v] (if (= a v) [a (cons (cons a h) r)] [v (cons (list v) (cons h r))])) [nil ()] %)))))

(defcheck solution-c561b918
  reduce (fn [coll element]
           (if (= element (-> coll last first))
             (conj (-> coll butlast vec) (conj (last coll) element))
             (conj coll (vector element)))) [])

(defcheck solution-c6261879
  (fn c [s]
    (reverse
      (reduce #(if (empty? %1) [[%2]]
                               (let [[[v & n :as f] & r] %1]
                                 (if (= v %2)
                                   (conj r (conj f %2))
                                   (conj %1 [%2]))))
        '() s))))

(defcheck solution-c7e875ea
  (fn
    [v]
    (let [f (fn [a cl i] (cond (empty? i) (conj a cl)
                               (= (first cl) (first i))
                               (recur a (conj cl (first i)) (rest i))
                               :else
                               (recur (conj a cl) (list (first i)) (rest i))))]
      (cond (empty? v) '()
            (= (count v) 1) (list (list (first v)))
            :else (reverse (f '() (list (first v)) (rest v)))))))

(defcheck solution-c81cb3b6
  #(loop [acc '() grp '() [r & rs] %]
     (cond (nil? r) (reverse (conj acc grp))
           (and (not (empty? grp)) (= r (first grp))) (recur acc (conj grp r) rs)
           :else (recur (if (empty? grp) grp (conj acc grp)) (list r) rs))))

(defcheck solution-c8e09975
  partition-by identity)

(defcheck solution-c94b5742
  (fn pack
    ([x] (if (empty? x)
           '(())
           (reverse (pack (list (first x)) (rest x) nil))))
    ([x xs acc]
     (if (empty? xs)
       (conj acc x)
       (if (= (first x) (first xs))
         (pack (conj x (first xs)) (rest xs) acc)
         (pack (list (first xs)) (rest xs) (conj acc x)))
       ))))

(defcheck solution-c9f1d05a
  (fn pack-seq [x]
    (if (empty? x)
      x
      ;; find out how many consecutive duplicates of x are there
      (concat (list (take ((fn how-many? [y]
                             (if (= (first y)
                                   (first (rest y)))
                               (+ 1 (how-many? (rest y)))
                               1)) x) x))
              (pack-seq (drop ((fn how-many? [y]
                                 (if (= (first y)
                                       (first (rest y)))
                                   (+ 1 (how-many? (rest y)))
                                   1)) x) x))))))

(defcheck solution-ca32caa6
  #(reverse (loop [result () coll %]
              (if (= [] coll)
                result
                (recur (conj result (take-while (fn[x] (= x (first coll))) coll)) (drop-while (fn[x] (= x (first coll))) coll))
                )
              )
     ))

(defcheck solution-cab6d7d
  (fn [s]
    (loop [s s r [] c []]
      (if (not (seq s))
        (if (seq c)
          (conj r c)
          r)
        (if (or (not (seq c)) (= (first s) (first c)))
          (recur (rest s) r (conj c (first s)))
          (recur (rest s) (conj r c) [(first s)]))))))

(defcheck solution-cc39aa9
  (fn ! [x]
    (reverse
      ((fn f [a,b,c]
         (if (empty? a)
           (conj c b)
           (if (=(first a) (first b))
             (f (rest a) (conj b (first a)) c)
             (f (rest a) (take 1 a) (conj c b) )))) (rest x) (take 1 x) '() ))))

(defcheck solution-cc8c40c2
  (fn P [l]
    (loop [l l last false result []]
      (cond (empty? l) (if last (conj result last) result)
            (not last) (recur (rest l) [(first l)] result)
            (= (first l) (first last)) (recur (rest l) (conj last (first l)) result)
            :else (recur (rest l) [(first l)] (conj result last))))))

(defcheck solution-ccfc7068
  (fn [s] (partition-by identity s)))

(defcheck solution-cd544cb8
  #(partition-by identity %1))

(defcheck solution-cdec6bf
  partition-by identity)

(defcheck solution-cdffb9f
  #(loop [seq-seq '() rep-seq '() i 0]
     (if (= i (count %1))
       (reverse (if (empty? rep-seq) seq-seq (conj seq-seq rep-seq)))
       (recur
         (if (and (not (empty? rep-seq)) (not (= (nth %1 i) (last rep-seq)))) (conj seq-seq rep-seq) seq-seq)
         (if (empty? rep-seq) (list (nth %1 i)) (if (= (nth %1 i) (last rep-seq)) (conj rep-seq (nth %1 i)) (list (nth %1 i))))
         (inc i))
       )
     ))

(defcheck solution-ce59bf55
  (fn [c]
    (reduce #(if (= (first (last %1)) %2) (conj (vec (drop-last %1)) (cons %2 (last %1))) (conj (vec %1) (cons %2 []))) [] c)))

(defcheck solution-ce862d0d
  (fn[coll] (partition-by identity coll)))

(defcheck solution-cec1bfc
  (fn f [r [a :as c]]
    (if a
      (f (conj r (take-while #(= % a) c))
        (drop-while #(= % a) c))
      r)) [])

(defcheck solution-cf90320b
  (fn [col]
    (let [recursor
          (fn recurs [out prev counter in]
            (if
             (empty? in)
              (concat out (list (vector prev counter)))
              (if
               (= prev (first in))
                (recurs out prev (+ 1 counter) (rest in))
                (recurs
                  (concat out (list (vector prev counter)))
                  (first in)
                  1
                  (rest in)
                  )
                )
              )
            )
          ]
      (map
        (fn [[k v]]
          (replicate v k)
          )
        (recursor '() (first col) 0 col)))))

(defcheck solution-d0c215f7
  (fn [coll]
    (let [len (count coll)]
      (loop [i 0, todo coll, tmp '(), res '()]
        (let [i1 (first todo)
              i2 (second todo)]
          (cond (< i (dec len))
                (if (= i1 i2)
                  (recur (inc i) (rest todo) (cons i1 tmp) res)
                  (recur (inc i) (rest todo) '() (cons (cons i1 tmp) res)))
                :else (reverse (cons (cons i1 tmp) res)))))
      )))

(defcheck solution-d179449
  (fn conseq [sq]
    (partition-by identity sq)
    ))

(defcheck solution-d1cc95c7
  (fn [ls]
    (let [rs (reverse ls)]
      (reduce
        #(if (= %2 (first (first %)))
           (conj (rest %) (conj (first %) %2))
           (conj % (list %2)))
        ()
        rs))))

(defcheck solution-d2527a7b
  (fn [s]
    (loop [x s
           l nil
           acc []]
      (if (nil? x)
        acc
        (let [head (first x)]
          (if (not= head l)
            (recur (next x) head (conj acc (list head)))
            (recur (next x) l (conj (pop acc) (conj (last acc) head)))))))))

(defcheck solution-d2f1b37
  (fn [seq1] (loop [orig (seq seq1) packed '()] (if (empty? orig) (reverse packed) (recur (drop-while #(= (first orig) %) orig) (conj packed (take-while #(= (first orig) %) orig)))))))

(defcheck solution-d365b104
  #(reduce (fn [r i] (if (= (last (last r)) i) (assoc r (dec (count r)) (conj (last r) i) ) (conj r [i]) ) ) [[(first %)]] (rest %) ))

(defcheck solution-d4011744
  (fn [coll]
    (reverse (reduce
               #(if (= (first (first %)) %2)
                  (conj (rest %) (conj (first %) %2))
                  (conj % (list %2)))
               '()
               coll)
      )))

(defcheck solution-d4ea4ea6
  (fn  [lst]
    (loop [rm (seq lst), acc (vector), wip nil]
      (cond (empty? rm)
            (if (empty? wip)
              (seq acc)
              (seq (conj acc wip)))
            (= (first rm) (first wip)) (recur (rest rm) acc (conj wip (first rm)))
            :eslse (if (empty? wip)
                     (recur (rest rm) acc (list (first rm)))
                     (recur (rest rm) (conj acc wip) (list (first rm))))))))

(defcheck solution-d56032ec
  (fn [s]
    (reduce
      #(if (= (peek (peek %1)) %2)
         (into (pop %1) [(into (peek %1) [%2])])
         (into %1 [[%2]]))
      [[(first s)]] (rest s))))

(defcheck solution-d5711ad6
  (fn separate [coll]
    (loop [res []
           xs coll]
      (if (empty? xs)
        res
        (recur (conj res (take-while #(= % (first xs)) xs))
          (drop-while #(= % (first xs)) xs))))))

(defcheck solution-d5820f6
  (fn
    [seq]
    (loop [seq seq list '() templist '()]
      (if (not-empty seq)
        (if (not= (second seq) (first seq))
          (recur (rest seq) (conj list (conj templist (first seq))) '())
          (recur (rest seq) list (conj templist (first seq)))
          )
        (reverse list)))

    ))

(defcheck solution-d613ddbd
  ;;#(partition-by identity %)
  (fn my-partition-by
    [collection]
    (reduce (fn
              [c val]
              (if (= (last (last c)) (identity val))
                (conj (pop c) (conj (last c) val))
                (conj c (conj [] val)))) [] collection)))

(defcheck solution-d6966768
  (fn t2 [coll]
    (reduce #(let [i (peek %1)]
               (cond
                 (= (first i) %2)
                 (conj (pop %1) (conj i %2))

                 :else
                 (conj %1 (list %2))))
      [] coll)))

(defcheck solution-d705d243
  reduce (fn [x y] (if (= (first (last x)) y)
                     (concat (butlast x) [(conj (last x) y)])
                     (concat x [[y]]))) '())

(defcheck solution-d79fb94f
  (fn f [s]
    (if (empty? s)
      '()
      (let [prev (f (rest s))]
        (if (= (first s) (ffirst prev))
          (cons (cons (first s) (first prev)) (rest prev))
          (cons (list (first s)) prev))))))

(defcheck solution-d8410f77
  (fn [s] (reverse (reduce #(if (= (ffirst %1) %2)
                              (cons (cons %2 (first %1)) (rest %1))
                              (cons (list %2) %1)) '() s))))

(defcheck solution-d85e2088
  (fn [s]
    (reverse
      (reduce
        (fn [acc x]
          (if (= x (ffirst acc))
            (conj (rest acc) (conj (first acc) x))
            (conj acc (list x))))
        '()
        s))))

(defcheck solution-d89c76e2
  (fn [s]
    (loop [f (first s), v s, result [], sub-list []]
      (if (empty? v)
        (if (empty? sub-list)
          result
          (conj result sub-list))
        (if (empty? sub-list)
          (recur (second v) (rest v) result (conj sub-list f))
          (if (= (last sub-list) f)
            (recur (second v) (rest v) result (conj sub-list f))
            (recur (second v) (rest v) (conj result sub-list) [f]))
          )
        ))))

(defcheck solution-d9e019bc
  #(loop [in %, out []]
     (if (empty? in)
       out
       (recur
         (drop-while (partial = (first in)) in)
         (conj out (take-while (partial = (first in)) in))))))

(defcheck solution-d9e813ac
  (comp reverse
        (partial reduce
          (fn [memo el]
            (if (= (ffirst memo) el)
              (conj (rest memo) (conj (first memo) el))
              (conj memo (list el))))
          '())))

(defcheck solution-d9fddee8
  (fn f [s]
    (partition-by identity s)))

(defcheck solution-dad82711
  #(reduce
     (fn [a x]
       (if (= (last (last a)) x)
         (conj (vec (drop-last a)) (conj (last a) x))
         (conj a [x])))
     [] %))

(defcheck solution-dbd75666
  (fn [x]
    (map
      (fn [[x c]] (repeat c x))
      (reduce
        (fn [acc i]
          (if (empty? acc)
            (list i)
            (if (= (first i) (first (last acc)))
              (concat (butlast acc) (list (list (first i) (inc (last (last acc))))))
              (concat acc (list i))
              )
            )
          )
        '()
        (map #(list % 1)  x)
        )
      )
    ))

(defcheck solution-dc3046b
  (fn [l-prime]
    (loop [l (drop-last l-prime) current (list (last l-prime)) rv ()]
      (if (empty? l)
        (cons current rv)
        (if (= (last l) (last current))
          (recur (drop-last l) (cons (last l) current) rv)
          (recur (drop-last l) (list (last l)) (cons current rv)))))))

(defcheck solution-dd66c72d
  (fn pack [xs]
    (seq
      (let [n (count xs)]
        (loop [xs' [] i 0]
          (if (= i n)
            xs'
            (let [xi (nth xs i)
                  j (loop [j i]
                      (if (and (< j n) (= (nth xs j) xi))
                        (recur (inc j))
                        j
                        ))]
              (recur (conj xs' (repeat (- j i) xi)) j))))))))

(defcheck solution-dd98348b
  partition-by identity)

(defcheck solution-df10fd78
  (fn re [x]
    (reverse (reduce
               (fn [q w]
                 (if (= (first (first q)) w)
                   (cons (cons w (first q)) (rest q))
                   (cons [w] q)
                   )
                 )
               (cons (cons (first x)'()) '())
               (rest x)
               ))
    ))

(defcheck solution-df1484b5
  (fn [x]
    (loop [res '() toAdd (list (first x)) toDo (rest x)]
      (if (empty? toDo)
        (concat res (list toAdd))
        (if (= (first toAdd) (first toDo))
          (recur res  (conj toAdd (first toDo)) (rest toDo))
          (recur (concat res (list toAdd)) (list (first toDo)) (rest toDo)))))))

(defcheck solution-df986f16
  (fn [xs]
    (partition-by identity xs)))

(defcheck solution-e0ef3ea1
  (fn pack [xs]
    (if (empty? xs)
      ()
      (letfn [(cntdup [ys]
                (if (not= (first ys) (second ys)) 1
                                                  (inc (cntdup (rest ys)))))]
        (concat (list (repeat (cntdup xs) (first xs))) (pack (drop (cntdup xs) xs)))))))

(defcheck solution-e26db77d
  (fn [x] (partition-by #(list %) x)))

(defcheck solution-e31239c4
  (fn group-iden [col]
    (->> col
      (reduce (fn [col n]
                (if (= (peek col) n)
                  (conj col n)
                  (conj col :sep n)
                  ))
        [])
      (partition-by #(= :sep %))
      (filter #(not= '(:sep) %)))))

(defcheck solution-e35380ba
  (fn [input]
    (loop [i input tot []]
      (if (empty? i)
        tot
        (if (= (last (last tot)) (first i))
          (recur (rest i) (concat (butlast tot) (list (conj (last tot) (first i)))))
          (recur (rest i) (concat tot (list (list (first i))))))))))

(defcheck solution-e3bac8e3
  (comp reverse
        #(loop [[cur & _ :as whole] %
                result ()]
           (if cur
             (if (= cur (ffirst result))
               (recur (next whole) (conj (rest result) (conj (first result) cur)))
               (recur (next whole) (conj result (list cur))))
             result))))

(defcheck solution-e46ce9ef
  (fn [seq]
    (loop [seq seq
           last nil
           result '()]
      (if (empty? seq)
        (reverse result)
        (let [[head & tail] seq]
          (recur tail head
            (if (= head last)
              (conj (rest result) (conj (first result) head))
              (conj result (list head)))))))))

(defcheck solution-e532e63b
  (fn prob31 [col]
    (partition-by identity col)))

(defcheck solution-e538c2c2
  #(reduce (fn [s e] (if (= e (first (last s)))
                       (conj (vec (reverse (rest (reverse s))))
                         (conj (last s) e))
                       (conj s [e])))
     []
     %))

(defcheck solution-e616f347
  (fn [col] (partition-by identity col)))

(defcheck solution-e62b60c8
  (fn pack
    ([remain] (pack (rest remain) [] [(first remain)]))
    ([remain big little]
     (if (first remain)
       (if (= (first remain) (first little))
         (pack (next remain) big (conj little (first remain)))
         (pack (next remain) (conj big little) [(first remain)])
         )
       (conj big little)
       )
     )
    ))

(defcheck solution-e632a6eb
  (fn [c] (partition-by identity c)))

(defcheck solution-e68e9f4e
  (fn pack
    [arr]
    (loop [[x1 x2 & t] arr cur [] final []]
      (if (nil? x2)
        (conj final (conj cur x1))
        (if (= x1 x2)
          (recur (conj t x2) (conj cur x1) final)
          (recur (conj t x2) [] (conj final (conj cur x1)))
          )
        )
      )
    ))

(defcheck solution-e691d7c2
  (fn foo [l]
    (if (seq l)
      (cons (take-while #(= % (first l)) l)
        (foo (drop-while #(= % (first l)) l))))))

(defcheck solution-e702d6c4
  (fn ! [coll]
    (when-let [s (seq coll)]
      (when-let[x (split-with (fn [y] (= y (first s))) s)]
        (concat (list (first x)) (! (last x)))))))

(defcheck solution-e7dd9d4a
  (fn groupe [v]
    (reduce
      #(if (= (last (last %1)) %2)
         (conj (subvec %1 0 (dec (count %1))) (conj (last %1) %2))
         (conj %1 [%2])
         )
      []
      v
      )
    ))

(defcheck solution-e7f05f2c
  (partial partition-by (fn [x] x)))

(defcheck solution-e87220b3
  (fn pack-a-seq [coll]
    (partition-by identity coll)))

(defcheck solution-e9089b91
  (fn pack
    ([coll]
     (pack coll '()))
    ([coll res]
     (cond
       (empty? coll)
       res

       (empty? res)
       (pack (rest coll) (list (list (first coll))))

       (= (first coll) (first (last res)))
       (pack (rest coll) (concat (butlast res) (list (concat (last res) (list (first coll))))))

       :else
       (pack (rest coll) (concat res (list (list (first coll)))))

       ))))

(defcheck solution-ea2b1ef0
  (fn [a]
    (loop [l a s []]
      (let [j (first l) m (take-while #(= j %) l) x (drop (count m) l)]
        (if (nil? j)
          s
          (recur x (conj s m))
          )
        )
      )
    ))

(defcheck solution-ec7508a6
  (fn
    [[fi & li] & par]
    (loop
     [head [fi]
      tail li
      dgst (vector)]
      (let
       [ndgst (if
               (=
                 (first tail)
                 (first head))
                dgst
                (conj dgst head))]
        (if
         (empty? tail)
          ndgst
          (recur
            (if
             (= (first tail) (first head))
              (conj head (first tail))
              [(first tail)])
            (rest tail)
            ndgst))))))

(defcheck solution-ecb62e9
  partition-by #(identity %))

(defcheck solution-edaf091f
  (fn pack-a-sequence [xs]
    (reduce (fn [packed-sequence next-value]
              (if (= (last (last packed-sequence)) next-value)
                (concat (drop-last packed-sequence) (list (conj (last packed-sequence) next-value)))
                (concat packed-sequence (list (list next-value)))))
      (list (list (first xs)))
      (rest xs))))

(defcheck solution-edb64222
  partition-by list)

(defcheck solution-eddbfe02
  (fn f [l]
    (if (empty? l)
      l
      (if (= (first l) (second l))
        (cons (take-while #(= (first l) %) l) (f (drop-while #(= (first l) %) l)))
        (cons (take 1 l) (f (rest l)))
        )
      )
    ))

(defcheck solution-ee8415b6
  (fn pack [s]
    (apply
      (fn helper [s2 elt sub result]
        (cond (empty? s2) (conj result sub)
              (= (first s2) elt) (helper (rest s2) elt (cons elt sub) result)
              :else (helper (rest s2) (first s2) [(first s2)] (conj result sub))))
      [s (first s) [] []])))

(defcheck solution-ef52be75
  (fn rec [ls]
    (lazy-seq
      (if (empty? ls) ()
                      (let [f (first ls)
                            fs (take-while #(= f %) ls)
                            rs (drop-while #(= f %) ls)]
                        (cons fs (rec rs)))))))

(defcheck solution-f098b9cf
  (fn f [x] (let [[s r] (split-with (partial = (first x)) x)] (if (empty? s) '() (cons s (f r))) ) ))

(defcheck solution-f1388666
  reduce (fn [acc x]  (if (not= (first (last acc)) x ) (concat acc [(list x)])  (concat (drop-last acc) [(conj (last acc) x)]  )  )  ) [])

(defcheck solution-f17518c7
  (fn [s]
    (rest (reverse (reduce
                     #(if (= %2 (ffirst %1))
                        (concat [(conj (first %1) %2)] (rest %1))
                        (concat [[%2]] %1))
                     [[]]
                     s)))))

(defcheck solution-f2784601
  (fn split [x]
    (if (empty? x)
      []
      (let [y (split (rest x))]
        (if (= (first x) (first (first y)))
          (concat (list (conj (first y) (first x))) (rest y))
          (if (empty? y)
            (list (list (first x)))
            (concat (list (list (first x))) y)))))))

(defcheck solution-f3249fe7
  (fn pack
    [s]
    (reduce (fn [result v]
              (let [last-group (last result)]
                (if (and (vector? last-group)
                         (= v (first last-group)))
                  (conj (pop result) (conj last-group v))
                  (conj result [v]))))
      [] s)))

(defcheck solution-f39e6267
  (fn [coll]
    (drop 1 (reverse
              (loop [acc      '()
                     lst-coll '()
                     lst      nil
                     coll     coll]
                (if (empty? coll)
                  (conj acc lst-coll )
                  (let [fst    (first coll)
                        rst    (rest coll)]
                    (if (= fst lst)
                      (recur acc (conj lst-coll fst) fst rst)
                      (recur (conj acc lst-coll) (list fst) fst rst)))))) ) ))

(defcheck solution-f3e8f276
  #(->> % (reduce
            (fn [acc it]
              (if (= (ffirst acc) it)
                (cons (cons it (first acc)) (rest acc))
                (cons (list it) acc)))
            ())
     reverse))

(defcheck solution-f4772e51
  (fn [s]
    (loop [output ()
           lastval nil
           curpack ()
           remain s]
      (if (empty? remain)
        (if (empty? curpack) output (concat output [curpack]))
        (if (nil? lastval)
          (recur output (first remain) [(first remain)] (rest remain))
          (if (= lastval (first remain))
            (recur output lastval (concat curpack [lastval]) (rest remain))
            (recur (concat output [curpack]) (first remain) [(first remain)] (rest remain))))))
    ))

(defcheck solution-f4cac393
  (fn packeddupes [input]
    (reduce (fn [sofar newval]
              (if (empty? sofar)
                [[newval]]
                (if (= (last (last sofar)) newval)
                  (conj (vec (butlast sofar)) (conj (last sofar) newval))
                  (conj sofar [newval]))))
      [] input)))

(defcheck solution-f61a9440
  (fn [xs] (partition-by identity xs)))

(defcheck solution-f6dccbb7
  (fn pack [v]
    (if (empty? v) ()
                   (let [[a b] (split-with #(= (first v) %) v)]
                     (conj (pack b) a)))))

(defcheck solution-f7b6040a
  (fn [s]
    (reverse (reduce #(if (= (first (first %1)) %2)
                        (conj (rest %1) (conj (first %1) %2))
                        (conj %1 (list %2))
                        ) '() s))))

(defcheck solution-f898d2a9
  (fn __ [s]
    (reverse
      (reduce (fn [a x]
                (if (= x (ffirst a)) (cons (conj (first a) x) (rest a))
                                     (cons (list x) a)
                                     )
                )
        (list (list (first s)))
        (rest s))
      )
    ))

(defcheck solution-f8c45183
  (fn this [coll]
    (when-first [x coll]
      (let [[sub-list coll]
            (split-with #(= x %) coll)]
        (cons sub-list (this coll))))))

(defcheck solution-f97e6397
  (fn [A]
    (partition-by identity A)
    ))

(defcheck solution-fa4f7275
  (fn pack [s]
    (let [group (take-while #(= % (first s)) s)
          t (drop (count group) s)]
      (if (= 0 (count t))
        (list group)
        (concat (list group)
                (pack t))))))

(defcheck solution-fa80bafc
  (fn [seq]
    (partition-by identity seq)))

(defcheck solution-fc16cd8e
  reduce (fn [coll val]
           (if (= val (first (last coll)))
             (assoc coll (dec (count coll))
                         (conj (last coll) val))
             (conj coll [val]))) [])

(defcheck solution-fc54e67f
  (fn  [y1]
    (loop [z '() i 0 y y1 ]
      #_(println "Z:" z "Count z:" i   "Y:" y   "First Y:" (first y))
      (if (not-empty y)  (recur (conj z (sequence (take-while (fn[x](= x (first y))) y ))) (+ i (count (take-while (fn[x](= x (first y))) y ))) (subvec y1 (count (apply concat (conj z (sequence (take-while (fn[x](= x (first y))) y )))))  )   ) (reverse z)) )))

(defcheck solution-fc9ab14a
  (fn [coll]
    (loop [r (list (list (first coll)))
           c (rest coll)]
      (if (empty? c)
        (reverse r)
        (if (= (first c) (ffirst r))
          (recur (conj (rest r) (conj (first r) (first c))) (rest c))
          (recur (conj r (list (first c))) (rest c)))))))

(defcheck solution-fcf20036
  (fn [xxs] (let [fr (fn [ps t xs] (if (first xs)
                                     (if (= 0 (count t)) (recur ps (cons (first xs) t) (rest xs))
                                                         (if ((set t) (first xs)) (recur ps (cons (first xs) t) (rest xs))
                                                                                  (recur (conj ps t) (list) xs))
                                                         )
                                     (conj ps t) )
                       )]
              (fr [] [] xxs))))

(defcheck solution-fd5f1e7f
  (fn [s] (loop [result []
                 s s]
            (if (empty? s)
              result
              (let [my-first (first s)]
                (recur (conj result (conj (take-while (partial = my-first) (rest s)) my-first ))
                  (drop-while (partial = my-first) (rest s))
                  ))))))

(defcheck solution-fd80575
  (fn [coll]
    (partition-by #(list %) coll)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-31))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
