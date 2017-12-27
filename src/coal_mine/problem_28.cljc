(ns coal-mine.problem-28
  (:require [coal-mine.checks :refer [defcheck-28] :rename {defcheck-28 defcheck}]
            [clojure.test]
            [clojure.walk]))

(defcheck solution-108b0b94
  #((fn mb ([lst rlst]
            (let [frist (first lst)]
              (if (nil? frist)
                rlst
                (if (coll? frist)
                  (recur (into (into [] frist) (rest lst)) rlst)
                  (recur (rest lst) (conj rlst frist))))))) % []))

(defcheck solution-1095c362
  (fn dig [coll]
    (when (seq coll)
      (if ((complement sequential?) (first coll))
        (cons (first coll) (dig (next coll)))
        (concat (dig (first coll)) (dig (next coll))))
      )))

(defcheck solution-1179a4dc
  (fn f [c]
    (cond
      ((complement coll?) c) (list c)
      (empty? c) '()
      :else (concat (f (first c)) (f (rest c))))))

(defcheck solution-11a512d9
  #(remove coll? (tree-seq coll? vec %)))

(defcheck solution-1270025b
  (fn [coll]
    (letfn [(flat [[x & xs]]
              (lazy-seq (concat
                         (if (coll? x) (flat x) (list x))
                         (when-not (empty? xs) (flat xs)))))]
      (flat coll))))

(defcheck solution-13031a3f
  (partial (fn flat [res col]
             (let [fst (first col) rst (rest col)]
               (-> res
                 (#(if (sequential? fst) (flat % fst) (conj % fst)))
                 (#(if (empty? rst) % (flat % rst)))))) []))

(defcheck solution-13de4e34
  ;from clojure source, shortened.  learned about tree-seq.
  ;#(remove coll? (tree-seq coll? seq %))

  ;learning mapcat
  (fn f [c]
    (mapcat #(if (coll? %)
               (f %)
               [%])
      c)))

(defcheck solution-14bff5ac
  (fn f [s] (mapcat #(if (sequential? %) (f %) [%]) s)))

(defcheck solution-156a19e1
  (fn flat [xs]
    (if (empty? xs) xs
                    (let [[y & ys] xs]
                      (if (coll? y)
                        (concat (flat y) (flat ys))
                        (cons y (flat ys)))))))

(defcheck solution-1604afb
  (fn fl [l]
    (if (coll? l)
      (reduce concat (map fl l))
      (list l)
      )))

(defcheck solution-1608249c
  (fn [coll] (filter #(not (coll? %)) (tree-seq coll? identity coll))))

(defcheck solution-1611e1e8
  (fn [s]
    (letfn [(strip [xs]
              (reduce #(if-not (sequential? %2)
                         (conj %1 %2)
                         (into [] (concat %1 (strip %2)))) '[] xs))]
      (strip s))))

(defcheck solution-1647075b
  (fn [c] (filter (complement sequential?) (rest (tree-seq sequential? seq c)))))

(defcheck solution-1691419
  (fn my-flatten [l]
    (if (empty? l)
      '()
      (let [head (first l) tail (rest l)]
        (if (sequential? head)
          (concat (my-flatten head) (my-flatten tail))
          (cons head (my-flatten tail)))))))

(defcheck solution-169cba91
  (fn f [l]
    (if
     (or (list? l) (vector? l))
      (mapcat f l)
      [l]
      )
    ))

(defcheck solution-16c2a524
  (fn [input]
    (loop [head (first input)
           end  (rest input)
           res  []]
      (if (or (sequential? head) (nil? head))
        (if (empty? head)
          (if (empty? end)
            (seq res)
            (recur (first end) (rest end) res))
          (if (empty? (rest head))
            (recur (first head) end res)
            (recur (first head) (conj (vec (rest head)) end) res)))
        (recur (first end) (rest end) (conj res head))))))

(defcheck solution-173cf3a2
  (fn [xs]
    (filter (complement sequential?)
      (tree-seq sequential? seq xs))))

(defcheck solution-1782e453
  (fn my-flatten
    ([head & tail]
     (if (nil? head)
       ()
       (if (sequential? head)
         (concat (apply my-flatten head) (apply my-flatten tail))
         (cons head (apply my-flatten tail)))))
    ([] ())))

(defcheck solution-17a1a5d0
  (fn fl
    ([x & xs] (into (fl x) (fl xs)))
    ([x] (if (sequential? x) (apply fl x) [x]))))

(defcheck solution-17bf4549
  (fn f [s]
    (if (empty? s) s
                   (if (coll? (first s))
                     (concat
                      (f (first s))
                      (f (rest s)))
                     (cons
                       (first s)
                       (f (rest s)))))))

(defcheck solution-17c4e27d
  (fn [c] (filter #(false? (coll? %))
            (tree-seq coll? identity c))))

(defcheck solution-181aeb6
  (fn fltn [lst] (cond (empty? lst) lst
                       (or (list? (first lst)) (vector? (first lst))) (concat (fltn (first lst)) (fltn (rest lst)))
                       true (conj (fltn (rest lst)) (first lst)))))

(defcheck solution-181cfad6
  (fn flatten2 [x] (if (sequential? x) (mapcat flatten2 x) (list x))))

(defcheck solution-18317168
  (fn flt [coll]
    (let [s (seq coll)]
      (cond
        (not (seq? s)) ()
        (nil? (first s)) ()
        (vector? (first s)) (concat (flt (seq (first s))) (flt (rest s)))
        (seq? (first s)) (concat (flt (first s)) (flt (rest s)))
        :else (concat (list (first s)) (flt (rest s)))))))

(defcheck solution-18eea6cf
  (fn fl [s]
    (loop [s1 s m '[]]
      (cond
        (empty? s1) (seq m)
        (sequential? (first s1))
        (recur (rest s1) (into [] (concat m (fl (first s1)))))
        :else (recur (rest s1) (conj m (first s1))))
      )))

(defcheck solution-197547da
  (fn __ [x]
    (reduce (fn [a v]
              (if (coll? v)
                (vec (concat a (__ v)))
                (conj a v))) [] x)))

(defcheck solution-19b46ee4
  (fn myFlatten [x]
    (if (coll? x)
      (mapcat myFlatten x)
      [x])))

(defcheck solution-19ce92e5
  (fn [l]
    (loop [q   l
           acc []]
      (if (empty? q)
        acc
        (let [f (first q)
              r (rest q)]
          (if (sequential? f)
            (recur (concat f r) acc)
            (recur r (conj acc f))))))))

(defcheck solution-19f0bf6d
  (fn new-flatten [x]
    (if (or (seq? x) (vector? x))
      (if (not-empty x)
        (concat (new-flatten (first x)) (new-flatten (rest x))))
      [x])))

(defcheck solution-19fc7f6
  (fn f [x]
    (mapcat #(if (coll? %) (f (seq %)) [%]) x)))

(defcheck solution-1a837188
  (fn fltn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-1a9cbcb9
  (fn [x] (let [x (loop [x x, r []]
                    (if (coll? (first x))
                      (if (empty? (rest x))
                        (reduce #(conj % %2) r (first x))
                        (recur (rest x) (reduce #(conj % %2) r (first x))))
                      (if (empty? (rest x))
                        (conj r (first x))
                        (recur (rest x) (conj r (first x))))))]
            (if (some coll? x)
              (recur x)
              x))))

(defcheck solution-1b9ccf80
  (fn myFlatten
    [sequence]
    (let [f (first sequence)]
      (cond
        (nil? f) ()
        :else (cond
                (coll? f) (concat (myFlatten f) (myFlatten (next sequence)))
                :else (cons f (myFlatten (next sequence))))))
    ))

(defcheck solution-1b9f0888
  (fn fltn [xs]
    (reduce (fn [rs ys] (if (or (seq? ys) (list? ys) (vector? ys)) (vec (concat rs (fltn ys))) (conj rs ys))) [] xs)))

(defcheck solution-1bbcce06
  (fn f [elems]
    (mapcat #(if (sequential? %) (f %) [%]) elems)))

(defcheck solution-1c31b4a0
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-1cc49a9
  (fn flatn [x] (if (coll? x) (mapcat flatn x) [x])))

(defcheck solution-1d061dda
  #(filter (complement coll?) (tree-seq sequential? seq %)))

(defcheck solution-1d1fd5cf
  (fn flt [s]
    (if (coll? s)
      (if (empty? s)
        []
        (concat (flt (first s)) (flt (rest s))))
      [s])))

(defcheck solution-1d265590
  (partial (fn flat [res xs]
             (let [f (first xs) r (rest xs)]
               (cond
                 (nil? f) res
                 (not (sequential? f)) (flat (conj res f) r)
                 :else (concat res (flat [] f) (flat [] r))))) []))

(defcheck solution-1d5e5ff4
  (fn [ls]
    (letfn [(f [xs ys]
              (cond
                (empty? xs) ys
                (coll? (first xs)) (f (rest xs) (f (first xs) ys))
                :else (f (rest xs) (conj ys (first xs)))))]
      (f ls []))))

(defcheck solution-1d7d1c49
  (fn my-flatten [xs]
    (mapcat #(if (coll? %) (my-flatten %) (list %)) xs)))

(defcheck solution-1d8ef91a
  (letfn [(flattn [[x & xs :as coll]]
            (cond (empty? coll) ()
                  (coll? x) (concat (flattn x) (flattn xs))
                  :else (conj (flattn xs) x)))]
    flattn))

(defcheck solution-1de95182
  (fn f [x] (if (coll? x) (mapcat f x) (list x))))

(defcheck solution-1e086f67
  (fn
    [xs]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq xs)))))

(defcheck solution-1e0b9ab3
  (fn ! [x] (if (sequential? x) (mapcat ! x) (list x))))

(defcheck solution-1e271ecb
  (fn f [coll]
    (if (seq coll)
      (let [fst (first coll)
            rst (rest coll)]
        (if (coll? fst)
          (concat (f fst) (f rst))
          (concat [fst] (f rst))))
      coll)))

(defcheck solution-1e9d39c8
  (fn flat [coll]
    (if (coll? coll) (mapcat flat coll) [coll])))

(defcheck solution-1ee095e2
  (fn fl [in]
    (mapcat (fn [item]
              (if (sequential? item)
                (fl (seq item))
                [item])) in)))

(defcheck solution-1faaa641
  (letfn [(F [coll]
            (cond (not (coll? coll)) (list coll)
                  (empty? coll) '()
                  :else (concat (F (first coll))
                                (F (rest coll)))))]
    F))

(defcheck solution-200d9208
  (fn fl [a]
    (if (sequential? a)
      (reduce
        (fn [s e] (concat s (fl e)))
        [] a)
      [a])))

(defcheck solution-20160dac
  (fn [x]
    (loop [todo x
           res  []]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (next todo)]
          (if (coll? f)
            (if (empty? f)
              (recur r res)
              (recur (concat f r)
                res))
            (recur r (conj res f))
            ))))))

(defcheck solution-20568b9a
  (fn foo [x] (mapcat #(if (coll? %) (foo %) (list %)) x)))

(defcheck solution-20ff738f
  (fn flat-seq [xs]
    (cond (empty? xs)
          ()
          (coll? (first xs))
          (concat (flat-seq (first xs)) (flat-seq (rest xs)))
          :else (cons (first xs) (flat-seq (rest xs)))
          )
    ))

(defcheck solution-211bd947
  (fn [coll]
    (letfn [(f [c] (loop [r [] c c]
                     (if (empty? c)
                       r
                       (if (sequential? (first c))
                         (recur (into r (f (first c))) (rest c))
                         (recur (conj r (first c)) (rest c))))))]
      (f coll))))

(defcheck solution-2146f970
  (fn -flatten [x]
    (if (coll? x)
      (mapcat -flatten x)
      [x])))

(defcheck solution-218e4e64
  #(filter (complement sequential?)
     (tree-seq sequential? identity %)))

(defcheck solution-21f37aa0
  (fn [s] (filter (complement coll?) (tree-seq coll? seq s))))

(defcheck solution-21fabe7
  (fn myflatten
    ([in] (myflatten in []))
    ([in ret]
     (if (nil? (first in))
       ret
       (if (coll? (first in))
         (myflatten (rest in) (myflatten (first in) ret))
         (myflatten (rest in) (conj ret (first in)))
         )
       )
     )
    ))

(defcheck solution-220e07a5
  #((fn flat [s nxt]
      (let
       [f (first s)
        r (rest s)]
        (if (empty? r)
          (cond
            (nil? f) nxt
            (not (coll? f)) (cons f nxt)
            (empty? f) nxt
            true (flat f nxt))
          (cond
            (nil? f) (flat r nxt)
            (not (coll? f)) (cons f (flat r nxt))
            (empty? f) (flat r nxt)
            true (flat f (flat r nxt)))))) % nil))

(defcheck solution-22210c45
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-222c8e2f
  (fn
    [tree]
    (filter
      (complement sequential?)
      (rest
        (tree-seq sequential? seq tree)))))

(defcheck solution-22d07ad9
  (fn fltn [l]
    (if (not-empty l)
      (if (coll? (first l))
        (concat (fltn (first l)) (fltn (rest l)))
        (cons (first l) (fltn (rest l))))
      ())))

(defcheck solution-2315939a
  (fn [t] (letfn [(f [xs] (if (coll? xs) (mapcat f xs) (list xs)))] (f t))))

(defcheck solution-23520f24
  (fn flatten' [x]
    (if (coll? x)
      (apply list (mapcat flatten' x))
      (list x))))

(defcheck solution-2387219
  #(filter (fn [x] (not (coll? x))) (tree-seq coll? identity %)))

(defcheck solution-23e9ff19
  (fn my-flatten [col]
    (cond
      (empty? col) col
      (coll? (first col)) (concat (my-flatten (first col))
                                  (my-flatten (rest col)))
      :else (conj (my-flatten (rest col)) (first col)))))

(defcheck solution-23ec34ba
  #(loop [l1 %, l2 '()]
     (cond
       (sequential? (first l1)) (recur (concat (first l1) (rest l1)) l2)
       (empty? l1) (reverse l2)
       :else (recur (rest l1) (cons (first l1) l2)))))

(defcheck solution-242ad9ef
  (fn flat [xs]
    (if (empty? xs)
      xs
      (let [x (first xs)]
        (if (sequential? x)
          (concat (flat x) (flat (rest xs)))
          (cons x (flat (rest xs))))))))

(defcheck solution-254080e7
  (fn [s]
    (if (not-any? coll? s)
      s
      (let [flat-start   (take-while (comp not coll?) s)
            after-flats  (drop-while (comp not coll?) s)
            coll-element (first after-flats)
            others       (rest after-flats)]
        (recur (concat flat-start coll-element others))))))

(defcheck solution-256f0127
  (fn [values]
    (letfn [(flat [value]
              (when-let [col (seq value)]
                (let [item (first col)]
                  (if (sequential? item)
                    (concat (flat item) (flat (rest col)))
                    (conj (flat (rest col)) item)))))]
      (loop [values values result []]
        (if (empty? values)
          result
          (recur (rest values)
            (let [item (first values)]
              (if (sequential? item)
                (concat result (flat item))
                (concat result (list item))))))))))

(defcheck solution-25c8532f
  (fn meu-flatten [coll]
    (cond
      (empty? coll) nil
      (coll? (first coll)) (concat (meu-flatten (first coll)) (meu-flatten (next coll)))
      :else (cons (first coll) (meu-flatten (next coll))))))

(defcheck solution-25cf2cce
  (fn myflatten [s] (if (coll? s) (mapcat myflatten s) (list s))))

(defcheck solution-25ec7a83
  (fn
    [seq]
    (loop [current-seq seq]
      (if (not ((fn not-flattened? [s] (some #(coll? %) s)) current-seq))
        current-seq
        (recur (loop [new-seq '() old-seq current-seq]
                 (if (empty? old-seq)
                   new-seq
                   (recur
                     (if (coll? (first old-seq))
                       (concat new-seq (first old-seq))
                       (concat new-seq (list (first old-seq))))
                     (rest old-seq)
                     ))))))))

(defcheck solution-267f4247
  (fn f [s] (if (or (seq? s) (vector? s)) (mapcat f s) (list s))))

(defcheck solution-26ae5dfd
  (fn f [[x & xs]]
    (if x
      (concat
       (if (sequential? x) (f x) (list x))
       (f xs))
      ())))

(defcheck solution-26c45e7f
  (fn my-flatten [ls]
    (if (coll? ls)
      (if (empty? ls)
        nil
        (concat (my-flatten (first ls)) (my-flatten (rest ls))))
      (list ls))))

(defcheck solution-26d956f8
  (fn [coll]
    (let [res (atom [])]
      (clojure.walk/postwalk
        (fn [f]
          (when-not (sequential? f)
            (swap! res conj f)))
        coll)
      @res)))

(defcheck solution-27895ea9
  #((fn flat [acc sq]
      (cond (empty? sq) acc
            (sequential? (first sq))
            (concat acc (flat acc (first sq)) (flat acc (rest sq)))
            :else
            (concat acc (list (first sq)) (flat acc (rest sq)))))
    () %))

(defcheck solution-283cd879
  (fn flatn [coll]
    (if (empty? coll)
      coll
      (if (sequential? (first coll))
        (concat (flatn (first coll)) (flatn (rest coll)))
        (cons (first coll) (flatn (rest coll)))))))

(defcheck solution-287c7e19
  (fn [l]
    (loop [l1 l l2 []]
      (let [fl1 (first l1) rl1 (rest l1)]
        (cond
          (sequential? fl1) (recur (concat fl1 rl1) l2)
          (empty? l1) (seq l2)
          :else (recur rl1 (conj l2 fl1))
          )
        )
      )
    ))

(defcheck solution-28eb6aa
  (fn flat [lst]
    (if (not (coll? lst)) [lst]
                          (reduce concat (map flat lst)))))

(defcheck solution-28f714c3
  (fn m [i]
    (if (coll? i)
      (mapcat m i)
      [i])))

(defcheck solution-291d7354
  (fn [coll]
    (loop [flattened []
           [h & r] coll]
      (if ((complement sequential?) h)
        (if (empty? r)
          (conj flattened h)
          (recur (conj flattened h) r))
        (recur flattened (concat h r))
        )
      )
    ))

(defcheck solution-292979a5
  (fn f [xs] (mapcat #(if (sequential? %) (f %) [%]) xs)))

(defcheck solution-2993d681
  (fn self [col] (if (coll? col) (mapcat self col) (list col))))

(defcheck solution-29c16dc5
  ;;into is convenience ... apply conj
  (fn flat [coll]
    (if (sequential? coll)
      (reduce (fn [acc elem] (apply conj acc (flat elem))) [] coll)
      (list coll)
      )
    ))

(defcheck solution-2a234f6c
  (fn flatten2 [x]
    (cond
      (= x ()) '()
      (sequential? x) (concat (flatten2 (first x)) (flatten2 (rest x)))
      :else (list x))))

(defcheck solution-2a900411
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-2ac0142d
  (fn my-flatten [s]
    (if (sequential? s) (if (empty? s) '() (concat (my-flatten (first s)) (my-flatten (rest s)))) (list s))))

(defcheck solution-2add0781
  (fn my-flatten [coll]
    (let [ele   (first coll)
          coll- (rest coll)]
      (if (nil? ele)
        '()
        (if (sequential? ele)
          (concat (my-flatten ele) (my-flatten coll-))
          (concat (list ele) (my-flatten coll-)))))))

(defcheck solution-2ae4eaba
  (fn f [s] (if (coll? s) (mapcat f s) (list s))))

(defcheck solution-2b00d7a2
  (fn [v]
    (let [res (reduce
                (fn [cont item]
                  (if (sequential? item)
                    (reduce (fn [c ut] (conj c ut)) cont item)
                    (conj cont item)
                    )
                  )
                []
                v)]

      (if (some sequential? res)
        (recur res)
        (into () (reverse res))
        )
      )
    ))

(defcheck solution-2b0dceb1
  (fn [col]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq col)))))

(defcheck solution-2b289279
  (fn flat [sequence]
    (if (coll? sequence)
      (reduce concat (map flat sequence))
      [sequence])))

(defcheck solution-2bc24d7a
  (fn flat
    [elements]
    (loop [[x & xs :as elements] elements accum '()]
      (cond (empty? elements) accum
            (sequential? x) (recur xs (concat accum (flat x)))
            :else (recur xs (concat accum (list x)))))))

(defcheck solution-2c78c85e
  (fn flat [x]
    (if (empty? x)
      '()
      (if (not (coll? (first x)))
        (cons (first x) (flat (rest x)))
        (concat (flat (first x)) (flat (rest x)))))))

(defcheck solution-2c8a5cd6
  (fn f ([x]
         (f x []))
    ([x r]
     (if (empty? x)
       r
       (if (counted? (first x))
         (recur (concat (first x) (rest x)) r)
         (recur (rest x) (conj r (first x))))))))

(defcheck solution-2c903b64
  (fn flatten-list [the-list]
    (loop [l      the-list
           result []]
      (if (empty? l)
        result
        (let [head (first l)
              tail (rest l)]
          (if (sequential? head)
            (recur tail (concat result (flatten-list head)))
            (recur tail (concat result (list head)))))))))

(defcheck solution-2cc89dd3
  (fn my-flatten [col]
    (let [flattener
          (fn flattener [accum val]
            (if (coll? val) (reduce flattener accum val) (conj accum val)))]
      (reverse (reduce flattener '() col)))))

(defcheck solution-2ccdc26a
  #(filter (complement sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-2cf1f5ab
  (fn f [l] (mapcat #(if (coll? %) (f %) [%]) l)))

(defcheck solution-2e54adc
  (fn f [x]
    (if (coll? x)
      (mapcat f x)
      [x])))

(defcheck solution-2e8a2a25
  (fn myflatten [s]
    (reduce (fn [x y] (if (sequential? y) (into x (myflatten y)) (conj x y))) [] s)))

(defcheck solution-2e9c9b57
  (fn flattten [coll]
    (if (coll? coll)
      (reduce #(concat %1 (flattten %2)) () coll)
      (list coll))))

(defcheck solution-2ead0bb3
  #(filter (complement sequential?) (tree-seq sequential? identity %)))

(defcheck solution-2ec618fe
  (fn [l] (some
            #(if (every? (fn [z] (not (coll? z))) %) %)
            (iterate #(apply concat
                        (map (fn [y] (if (coll? y) y (list y))) %)) l))))

(defcheck solution-2ed735c1
  #(letfn [(parse [l] (if-let [[h & t] (seq l)]
                        (if (sequential? h) (concat (parse h) (parse t)) (cons h (lazy-seq (parse t))))))]
     (parse %)))

(defcheck solution-2eee2af4
  #(loop [s %, acc nil]
     (cond
       (empty? s) (reverse acc)
       (sequential? (first s)) (recur (concat (first s) (rest s)) acc)
       :else (recur (rest s) (cons (first s) acc)))))

(defcheck solution-2ef30c5
  (comp
   (partial filter (fn [x] (not (or (vector? x) (list? x)))))
   (partial tree-seq
     (fn [x]
       (or
        (list? x)
        (vector? x)
        )
       )
     identity
     )
   ))

(defcheck solution-2f20fe1c
  (fn flat [seq]
    (if (not (coll? seq))
      (list seq)
      (apply concat (map flat seq)))))

(defcheck solution-3005e818
  (fn j-flatten
    [[h & t]]
    (if (nil? h)
      nil
      (if (coll? h)
        (concat (j-flatten h) (j-flatten t))
        (cons h (j-flatten t))))))

(defcheck solution-30874858
  (fn p28 [s]
    (reverse
      (loop [s s
             r '()]
        (cond (empty? s)
              r
              (coll? (first s))
              (recur (rest s) (apply conj r (p28 (first s))))
              :else
              (recur (rest s) (conj r (first s))))))))

(defcheck solution-315eb3ab
  (fn fratten [c]
    (if (coll? c)
      (for [item c subitem (fratten item)] subitem)
      [c])))

(defcheck solution-31c57b46
  (fn flat [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-31e76942
  (fn
    [coll]
    (letfn [(flat
              [coll]
              (let [rs (atom ())]
                (loop [coll coll]
                  (if (nil? coll)
                    @rs
                    (let [e (first coll)]
                      (if (coll? e)
                        (swap! rs #(concat %1 %2) (flat e))
                        (swap! rs #(concat %1 %2) [e]))
                      (recur (next coll)))))))]
      (flat coll))))

(defcheck solution-328aa82f
  (fn [s]
    ((fn helper [x]
       (loop [remaining x ans []]
         (if (empty? remaining)
           ans
           (let [[ptr & remain] remaining]
             (if (sequential? ptr)
               (recur remain (into [] (concat ans (helper ptr))))
               (recur remain (conj ans ptr))))))) s)))

(defcheck solution-32a08335
  (fn f [x] (mapcat #(if (coll? %) (f %) [%]) x)))

(defcheck solution-32a6ff57
  (fn my-flatten [xs]
    (mapcat (fn [x] (if (sequential? x)
                      (my-flatten x)
                      [x]))
      xs)))

(defcheck solution-3318577f
  (fn [s] (filter (complement sequential?) (tree-seq sequential? identity s))))

(defcheck solution-331cf821
  (fn myf [l] (apply concat (map #(if (sequential? %) (myf %) (list %)) l))))

(defcheck solution-3334ae4d
  (comp
   (partial filter #(not (coll? %)))
   (partial tree-seq coll? identity)))

(defcheck solution-3350a399
  (fn rec [l]
    (if (empty? l)
      []
      (if (coll? (first l))
        (into (rec (first l)) (rec (rest l)))
        (into [(first l)] (rec (rest l)))))))

(defcheck solution-336cca77
  (fn f [coll]
    (reduce #(if (coll? %2)
               (->> (f %2) (concat %) vec)
               (conj % %2))
      [] coll)))

(defcheck solution-33895e3f
  (fn myflatten [xs] (mapcat (fn [x] (if (coll? x) (myflatten x) [x])) xs)))

(defcheck solution-33d7d659
  (fn flatten-me [s]
    (if (coll? s)
      (mapcat flatten-me s)
      [s])))

(defcheck solution-33d7ef22
  (fn my-flatten [a]
    (if (empty? a)
      a
      (if (coll? (first a))
        (concat (my-flatten (first a)) (my-flatten (rest a)))
        (cons (first a) (my-flatten (rest a)))))))

(defcheck solution-340174b0
  (fn [ll]
    (letfn [(flatten_ [in out]
              (if in
                (if (sequential? (first in))
                  (flatten_ (next in) (reduce conj out (flatten_ (first in) [])))
                  (flatten_ (next in) (conj out (first in))))
                out))]
      (flatten_ ll []))))

(defcheck solution-3417c319
  (fn [x] (filter #(not (sequential? %)) (tree-seq sequential? seq x))))

(defcheck solution-34232708
  (let [flat
        (fn [coll acc]
          (if (nil? coll)
            acc
            (if (coll? (first coll))
              (recur (concat (first coll) (next coll)) acc)
              (recur (next coll) (cons (first coll) acc))
              )
            )
          )]
    (fn [z] (reverse (flat z '())))
    ))

(defcheck solution-34cd1f7c
  (fn flatit [s]
    (if (empty? s)
      ()
      (lazy-cat
        (let [f (first s)] (if (coll? f) (flatit f) (list f)))
        (flatit (rest s))
        )
      )
    ))

(defcheck solution-34ce8836
  (fn [coll]
    (if (empty? (filter sequential? coll))
      coll
      (recur (apply concat (map #(if (not (sequential? %)) (list %) %) coll))))))

(defcheck solution-354d1a6b
  (fn f [v]
    (reduce
      (fn [a b]
        (if (not (sequential? b))
          (concat a (list b))
          (concat a (f b)))) [] v)))

(defcheck solution-355e72ef
  (comp reverse
        (fn my-flatten
          ([coll]
           (my-flatten coll '()))
          ([coll result]
           (if (nil? coll)
             result
             (my-flatten (next coll)
               (if (coll? (first coll))
                 (concat (my-flatten (first coll)) result)
                 (conj result (first coll)))))))))

(defcheck solution-35b5ad76
  #(letfn [(melt [acc coll]
             (if (empty? coll)
               acc
               (let [el (first coll)]
                 (if (coll? el)
                   (recur (concat acc el) (rest coll))
                   (recur (concat acc (vector el)) (rest coll))))))
           (meltdown [coll prev]
             (if (= coll prev)
               prev
               (recur (melt [] coll) coll)))]
     (meltdown % [])))

(defcheck solution-35cea616
  (fn flatten-sequence [x]
    (if (empty? x)
      []
      (if (coll? (first x))
        (concat (flatten-sequence (first x)) (flatten-sequence (rest x)))
        (concat (list (first x)) (flatten-sequence (rest x)))))))

(defcheck solution-35e55834
  (fn flat [coll]
    (let [x (first coll)
          r (rest coll)]
      (if (sequential? x)
        (concat (flat x) (flat r))
        (if (empty? r)
          coll
          (cons x (flat r)))))))

(defcheck solution-3625082d
  #(reverse ((fn f [r h] (
                           if (sequential? h)
                           (reduce f r h)
                           (cons h r)
                           )
               ) '() %)))

(defcheck solution-36353e42
  (fn f [s]
    (cond
      (empty? s) '()
      (coll? (first s)) (concat (f (first s)) (f (rest s)))
      :default (conj (f (rest s)) (first s)))))

(defcheck solution-364fb6d2
  (fn f
    ([x]
     (f x []))
    ([x s]
     (if (empty? x)
       (into '() s)
       (if (sequential? (first x))
         (recur (rest x) (f (first x) s))
         (recur (rest x) (conj s (first x))))))))

(defcheck solution-36c5ca93
  (fn flatt [lss]
    (cond
      (empty? lss) '()
      (coll? (first lss)) (concat (flatt (first lss))
                                  (flatt (rest lss)))
      :else (cons (first lss) (flatt (rest lss))))))

(defcheck solution-36eee29c
  (fn ff [l]
    (if (empty? l)
      l
      (if (coll? (first l))
        (concat (ff (first l)) (ff (rest l)))
        (cons (first l) (ff (rest l)))
        )
      )))

(defcheck solution-379145de
  (fn [a]
    (loop [x a]
      (if (some sequential? x)
        (recur
          (mapcat
            #(if (sequential? %) % (vector %))
            x))
        x))))

(defcheck solution-37a5ea8c
  (fn [input]
    (letfn [(my-flat [aseq]
              (if ((complement coll?) aseq)
                (list aseq)
                (apply concat (map my-flat aseq))))]
      (my-flat input))))

(defcheck solution-37f3e33e
  #(loop [xs % flat '()]
     (if (empty? xs)
       flat
       (let [head (first xs)]
         (if (sequential? head)
           (recur (concat head (next xs)) flat)
           (recur (next xs) (concat flat (list head))))))))

(defcheck solution-3801497
  (comp reverse
        (fn fltn
          ([coll]
           (fltn '() coll))
          ([out-seq coll]
           (if-not (coll? coll)
             (cons coll out-seq)
             (let [in-seq (seq coll)]
               (if-not (seq in-seq)
                 out-seq
                 (fltn (fltn out-seq (first in-seq)) (rest in-seq)))))))))

(defcheck solution-3983643b
  (fn recflat [x]
    (if (coll? x)
      (mapcat recflat x)
      (list x))))

(defcheck solution-39f474ce
  (fn [arg] (reduce (fn flat [coll it]
                      (if (coll? it) (reduce flat coll it) (conj coll it))) [] arg)))

(defcheck solution-3a294599
  (comp
   reverse
   (fn flat [l]
     (let [f (fn [acc l]
               (if (empty? l) acc
                              (recur
                                (if (coll? (first l))
                                  (concat (flat (first l)) acc)
                                  (conj acc (first l))) (rest l))))]
       (f '() l)))))

(defcheck solution-3a630d29
  (fn flatten' [xs]
    (loop [in xs, out []]
      (let [f  (first in)
            ff (if (coll? f) (flatten' f) [f])]
        (if (empty? in)
          out
          (recur (rest in) (concat out ff)))))))

(defcheck solution-3aaa377e
  (fn [s]
    (loop [[a & b] s,
           coll []]
      (cond
        (sequential? a) (if (empty? a)
                          (recur b coll)
                          (recur [(first a) (rest a) b] coll))
        a (recur b (conj coll a))
        :else coll))))

(defcheck solution-3afa6217
  (fn my-flatten [xs]
    (let [helper (fn helper [xs acc]
                   (if (or (seq? xs) (vector? xs))
                     (if (empty? xs)
                       acc
                       (->> acc (helper (rest xs)) (helper (first xs))))
                     (cons xs acc)))]
      (helper xs ()))))

(defcheck solution-3c28dcb3
  (fn __ [f & r]
    (if (coll? f)
      (if r
        (concat (__ f) (__ r))
        (apply __ f)
        )
      (if r
        (cons f (apply __ r))
        (list f)
        )
      )
    ))

(defcheck solution-3c6998a9
  (fn flt [lst]
    (mapcat #(if (sequential? %) (flt %) [%]) lst)))

(defcheck solution-3c800948
  (fn [x] (reverse (loop [acc () rem x]
                     (if (empty? rem)
                       acc
                       (if (sequential? (first rem))
                         (recur acc (concat (first rem) (rest rem)))
                         (recur (cons (first rem) acc) (rest rem))))))))

(defcheck solution-3c89c7f3
  (fn flattr ([x] (flattr x [])) ([x acc] (reduce #(if (sequential? %2) (flattr %2 %1) (conj %1 %2)) acc x))))

(defcheck solution-3ce39667
  (fn flatten-1 [coll]
    (reduce
      #(if (coll? %2)
         (into %1 (flatten-1 %2))
         (conj %1 %2))
      []
      coll)))

(defcheck solution-3e31b777
  #(remove sequential? (tree-seq sequential? identity %)))

(defcheck solution-3e4451a2
  (fn my-flatten [input]
    (if (coll? input)
      (apply concat (map my-flatten input))
      [input])))

(defcheck solution-3e698d64
  (fn [col] (filter (complement sequential?) (rest (tree-seq sequential? seq col)))))

(defcheck solution-3e7bfd78
  (fn flat [x]
    (mapcat
      #(if (coll? %)
         (flat %)
         (list %))
      x)))

(defcheck solution-3ea66ce
  (fn my-flatten [x]
    (if (sequential? x)
      (mapcat my-flatten x)
      (list x))))

(defcheck solution-3ed89180
  #(letfn [(worker [x n]
             (if (empty? x)
               n
               (recur (rest x) (if (coll? (first x)) (worker (first x) n) (conj n (first x))))))]
     (worker % [])))

(defcheck solution-3ee98b96
  (fn f [l] (lazy-seq (mapcat #(if (sequential? %) (f %) [%]) l))))

(defcheck solution-3f783169
  (fn ft [l]
    (reduce (fn [cl e]
              (if (sequential? e)
                (into cl (ft e))
                (conj cl e))) [] l)))

(defcheck solution-401f9b7c
  (fn ! [[x & more]]
    (case [(coll? x) (empty? more)]
      [true true] (! x)
      [false true] [x]
      [false false] (cons x (! more))
      [true false] (concat (! x) (! more))
      )))

(defcheck solution-40444c1e
  (fn flattenMe [mycoll]
    (if
     (coll? mycoll)
      (let
       [firstElem (first mycoll)]
        (if
         (= nil firstElem)
          '()
          (concat
           (flattenMe firstElem)
           (flattenMe (rest mycoll)))))
      (list mycoll))))

(defcheck solution-40507660
  (fn my-flatten [s]
    (lazy-seq
      (when s
        (let [[x & xs] s]
          (if (coll? x)
            (concat (my-flatten x) (my-flatten xs))
            (cons x (my-flatten xs))))))))

(defcheck solution-40552f7a
  (fn [col]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq col)))))

(defcheck solution-407ae8d
  (fn flat [input]
    (reduce concat '()
      (map #(if (sequential? %) (flat %) (list %)) input))))

(defcheck solution-407cd66c
  (fn [a-seq]
    (letfn [
            (my-flatten [a-seq]
              (if (empty? a-seq)
                a-seq
                (if (not (coll? (first a-seq)))
                  (cons (first a-seq) (my-flatten (rest a-seq)))
                  (concat (my-flatten (first a-seq))
                          (my-flatten (rest a-seq))))))]
      (my-flatten a-seq))))

(defcheck solution-4089c043
  (fn flt [s]
    (letfn [(atom? [s] (not (or (vector? s) (list? s))))]
      (cond (atom? s) [s]
            (every? atom? s) s
            :else (mapcat flt s)))))

(defcheck solution-40dcaa25
  (fn fl [coll] (let [f (first coll) r (next coll)] (concat (if (coll? f) (fl f) [f]) (when (seq? r) (fl r))))))

(defcheck solution-40f31a3f
  (fn f [x] (reduce (fn [a b] (concat a (if (coll? b) (f b) (list b)))) () x)))

(defcheck solution-410d48ad
  (fn flat [l]
    (if (sequential? l)
      (if (empty? l)
        l
        (concat (flat (first l)) (flat (rest l))))
      (list l))))

(defcheck solution-418c6654
  (fn my-flatten [coll]
    (when-first [x coll]
      (if (coll? x)
        (concat (my-flatten x) (my-flatten (rest coll)))
        (cons x (my-flatten (rest coll)))))))

(defcheck solution-41c0c673
  (fn my-flatten [a-seq]
    (
     (fn el-to-list [el]
       (if (sequential? el)
         (mapcat #(el-to-list %) el)
         (list el)
         )
       )
     a-seq)

    ))

(defcheck solution-431dc46b
  (fn w [n] (lazy-seq (if (coll? n) (mapcat w n) [n]))))

(defcheck solution-43bd520c
  (fn f [s]
    (if (coll? s)
      (mapcat f s)
      [s])))

(defcheck solution-43e0532a
  (fn my-flatten [coll]
    (loop [acc [] coll coll]
      (if (empty? coll)
        acc
        (let [a    (first coll)
              coll (rest coll)]
          (if (sequential? a)
            (recur acc (if (empty? coll) (vec a) (conj (vec a) coll)))
            (recur (conj acc a) coll)))))))

(defcheck solution-441e2c6d
  (fn my-flatten [xs] (reduce #(if (sequential? %2) (into %1 (my-flatten %2)) (conj %1 %2)) [] xs)))

(defcheck solution-441efc77
  (fn flattensq [xs]
    (reduce #(if (coll? %2) (vec (concat %1 (flattensq %2))) (conj %1 %2)) [] xs)))

(defcheck solution-4430b464
  (fn z ([xs] (z [] xs))
    ([acc xs]
     (if (sequential? xs)
       (into [] (concat acc (into [] (reduce z [] xs))))
       (conj acc xs)))))

(defcheck solution-44586ab7
  (letfn [(flat [seq] (mapcat #(if (sequential? %) (flat %) [%]) seq))]
    flat))

(defcheck solution-4469d56b
  (fn flt [c]
    (if (coll? c)
      (mapcat flt c)
      (list c)
      )
    ))

(defcheck solution-450752db
  (fn make-flat [input]
    (let [f (first input)
          r (rest input)]
      (concat
       (if (coll? f)
         (make-flat f)
         [f])
       (when (and (coll? r) (not (= r '())))
         (make-flat r))))))

(defcheck solution-46079292
  (fn flat [x] (if (sequential? x) (mapcat flat x) [x])))

(defcheck solution-466550fb
  (fn x [l]
    (reduce #(if (coll? %2)
               (vec (concat %1 (x %2)))
               (conj %1 %2))
      [] l)))

(defcheck solution-478b28f9
  (fn my-flatten [x]
    (mapcat
      #(if (coll? %)
         (my-flatten %)
         [%])
      x)))

(defcheck solution-47bbef31
  (fn squish [s]
    (if (coll? s)
      (apply concat (map squish s))
      (list s))))

(defcheck solution-47cabb85
  (fn f [coll]
    (reduce
      #(if (sequential? %2)
         (apply conj %1 (f %2))
         (conj %1 %2))
      [] coll)))

(defcheck solution-47fa72bf
  (fn fla [x]
    (if (sequential? x)
      (mapcat fla x)
      [x])))

(defcheck solution-481f03d4
  (fn f [a]
    (cond
      (not (coll? a)) [a]
      :else (mapcat f a))))

(defcheck solution-481f4b63
  (fn four-flatten [coll]
    (when (seq coll)
      (let [[x & more] coll]
        (if (sequential? x)
          (lazy-cat (four-flatten x) (four-flatten more))
          (lazy-seq (cons x
                      (four-flatten more))))))))

(defcheck solution-48558f44
  (fn myflat [l]
    (if (coll? l)
      (if (empty? l)
        nil
        (concat (myflat (first l)) (myflat (rest l))))
      (list l))))

(defcheck solution-4867e9e0
  (fn myflatten [a] (mapcat (fn [x] (cond (list? x) (myflatten x) (vector? x) (myflatten x) :else (list x))) a)))

(defcheck solution-48c9db7b
  (fn f [s] (mapcat #(if (coll? %) (f %) [%]) s)))

(defcheck solution-49111983
  (fn fltn [coll]
    (cond
      (= coll '()) coll
      (not (sequential? coll)) (list coll)
      :else (concat (fltn (first coll)) (fltn (rest coll))))))

(defcheck solution-4972e302
  (fn [S]
    (let [mflat (fn [s] (reduce #(if (coll? %2) (concat %1 %2) (concat %1 (list %2))) '() s))]
      (first (drop-while (fn [s] (some #(coll? %) s)) (iterate mflat S)))
      )
    ))

(defcheck solution-4a26170
  (fn flat [x]
    (reduce concat
      (map #(if (sequential? %)
              (flat %) (list %)) x))))

(defcheck solution-4a92a07
  (fn flat [s]
    (if (empty? s)
      '()
      (if (coll? (first s))
        (concat (flat (first s)) (flat (rest s)))
        (cons (first s) (flat (rest s)))
        ))))

(defcheck solution-4ae337a0
  (fn my-flatten [s]
    (filter (complement sequential?)
      (tree-seq sequential? seq s))))

(defcheck solution-4afcdb3d
  (fn [xs]
    ((fn my-flatten [xs acc]
       (cond
         (not (sequential? xs)) (concat acc (list xs))
         (empty? xs) acc
         :else (my-flatten (rest xs) (my-flatten (first xs) acc))))
     xs '())))

(defcheck solution-4bd46d58
  (fn f [x]
    (if (empty? x) x
                   (let [h (first x) t (rest x)]
                     (if (coll? h) (concat (f h) (f t)) (cons h (f t)))))))

(defcheck solution-4bf2ff35
  (fn ft [coll]
    (let [f1 (fn [coll] (apply concat (map #(if (sequential? %) % (take 1 (repeat %))) coll)))]
      (loop [x coll]
        (if (some sequential? x)
          (recur (f1 x))
          x)))))

(defcheck solution-4bf6959e
  (fn iter [xs]
    (reduce (fn [acc x]
              (if (coll? x)
                (concat acc (iter x))
                (concat acc (cons x ()))))
      ()
      xs)))

(defcheck solution-4c06bf9a
  #(letfn [(flat' [a a']
             (if (sequential? a')
               (reduce flat' a (seq a'))
               (conj a a')
               ))]
     (sort (flat' [] %))))

(defcheck solution-4cf604a9
  (fn [n]
    (filter #(false? (coll? %))
      (tree-seq coll? identity n))))

(defcheck solution-4e160f85
  (fn flattn [x]
    (if (coll? x)
      (mapcat flattn x)
      [x])))

(defcheck solution-4e63f02f
  (fn [s]
    ((fn inner [rs acc]
       (if (empty? rs)
         acc
         (let [head (first rs)
               tail (rest rs)]
           (if (coll? head)
             (inner tail (inner head acc))
             (inner tail (concat acc (vector head)))
             ))))
     s [])))

(defcheck solution-4ec9aec8
  (fn f [list]
    (if (empty? list) []
                      (let [x (first list)]
                        (concat (if (coll? x) (f x) [x])
                                (f (rest list)))))))

(defcheck solution-4eefe647
  (fn [seq]
    (letfn [(go [seq]
              (reduce (fn [x y] (concat x (if (coll? y) (go y) [y])))
                []
                seq))
            ]
      (go seq))))

(defcheck solution-4f495ac2
  #(loop [o [] c %]
     (if (empty? c)
       o
       (let [f (first c)
             r (rest c)
             i (coll? f)]
         (recur (if i o (conj o f))
           (if i (concat f r) r))))))

(defcheck solution-4fa36ac8
  (fn [coll]
    (let [c (reduce #(if (sequential? %2) (concat %1 %2) (conj (vec %1) %2)) [] coll)]
      (if (every? (complement sequential?) c)
        c
        (recur c)))))

(defcheck solution-4fa87c14
  #(reduce (fn [accum e]
             (loop [x (conj accum e)]
               (if-not (coll? (last x))
                 x
                 (recur (into (vec (butlast x)) (last x))))
               ))
     [] %))

(defcheck solution-507ecfed
  (fn x

    ([coll] (x coll []))

    ([coll res]

     (when (empty? coll) res)

     (reduce #(if (coll? %2) (into %1 (x %2 res)) (conj %1 %2)) res coll))))

(defcheck solution-50a052b9
  (fn
    [sq]
    (let [smash (fn flat [x]
                  (if-not (coll? x) [x]
                                    (if (empty? x) [] (reduce into [] (map flat x)))))]
      (smash sq))))

(defcheck solution-50bfc3ce
  (fn my-flatten [l]
    "free of StackOverflow problem, not lazy and much faster version of flatten."
    (loop [l1 l, l2 `()]
      (cond
        (sequential? (first l1)) (recur (concat (first l1) (rest l1)) l2)
        (empty? l1) (reverse l2)
        :else (recur (rest l1) (cons (first l1) l2))))))

(defcheck solution-5139a69
  (fn flatten-clone [s]
    (cond
      (empty? s) '()
      (sequential? (first s)) (concat (flatten-clone (first s)) (flatten-clone (next s)))
      :else (cons (first s) (flatten-clone (next s))))))

(defcheck solution-51927fa2
  (fn fltn
    [arr]
    (let [[x & xs] arr]
      (if (nil? x)
        []
        (if (sequential? x)
          (concat (fltn x) (fltn xs))
          (concat [x] (fltn xs)))))))

(defcheck solution-51a22928
  (fn f [c] (if (coll? c) (mapcat f c) [c])))

(defcheck solution-51de19b3
  (fn flat [lst]
    (lazy-seq
      (if (empty? lst) lst
                       (let [[x & xs] lst]
                         (if (coll? x)
                           (concat (flat x) (flat xs))
                           (cons x (flat xs))))))))

(defcheck solution-52091ede
  (fn flat [ss]
    (if (sequential? ss)
      (mapcat flat ss)
      [ss])))

(defcheck solution-524e5918
  #(letfn [(normalize [x] (if (coll? x) (flat x) (list x)))
           (flat [seq] (apply concat (map normalize seq)))]
     (flat %)))

(defcheck solution-526850cf
  (fn f [& x] (mapcat #(if (sequential? %) (apply f %) [%]) x)))

(defcheck solution-528179ad
  (fn flat [cl]
    (let [l (first cl) r (next cl)]
      (concat
       (if (sequential? l)
         (flat l)
         [l])
       (if (sequential? r)
         (flat r))))))

(defcheck solution-52b546b3
  (fn f [l]
    (reduce (fn [acc el]
              (if
               (or (seq? el) (list? el) (vector? el))
                (concat acc (f el))
                (conj (vec acc) el))) () l)))

(defcheck solution-530e180c
  (fn
    [q]
    (let
     [li?         (fn
                    [mbli]
                    (or
                     (seq? mbli)
                     (list? mbli)
                     (vector? mbli)))
      to-list     (fn
                    [li]
                    (map
                      #(if
                        (li? %)
                         %
                         (list %)) li))
      flatten-one (fn
                    [qq]
                    (reduce
                      concat
                      (to-list qq)))
      any?        (fn
                    [pred c]
                    (> (count (filter pred c)) 0))
      flatten-all (fn
                    [q]
                    (if
                     (any? li? q)
                      (recur (flatten-one q))
                      q))]
      (into [] (flatten-all q)))))

(defcheck solution-53b8bb1a
  (fn myflatten [x]
    (if (coll? x)
      (apply concat (map myflatten (seq x)))
      (list x))))

(defcheck solution-53cf7a03
  (fn [s]
    (loop [res [] left s]
      (cond
        (empty? left) res
        (coll? (first left)) (recur res (concat (first left) (rest left)))
        :else (recur (conj res (first left)) (rest left))))))

(defcheck solution-53f6c79a
  (fn [a] (filter (complement sequential?)
            (tree-seq sequential? seq a))))

(defcheck solution-54a32b28
  (fn [c]
    (let [f (reduce #(if (coll? %2) (into % %2) (conj % %2)) [] c)]
      (if (not (some coll? f))
        f
        (recur f)))))

(defcheck solution-54f16d29
  (fn f [lst]
    (if (empty? lst)
      '()
      (if (coll? (first lst))
        (concat (f (first lst))
                (f (rest lst)))
        (concat (list (first lst))
                (f (rest lst)))))))

(defcheck solution-550ab45c
  (fn flat [x]
    (if (sequential? x)
      (mapcat flat x)
      [x])))

(defcheck solution-551433be
  (fn f [s]
    ((fn f1 [r s]
       (if (sequential? s)
         (reduce f1 r s)
         (conj r s))) [] s)))

(defcheck solution-5542325f
  (fn flat [items]
    (if (coll? items)
      (mapcat flat items)
      [items])))

(defcheck solution-55c0e984
  (fn flattenX [x] (if (coll? x) (mapcat flattenX x) (list x))))

(defcheck solution-55d274bc
  (fn f
    ([coll] (f [] coll))
    ([result coll]
     (reduce #(if (coll? %2) (f %1 %2) (conj %1 %2)) result coll))))

(defcheck solution-55dec512
  #(->> %
     (tree-seq sequential? vec)
     rest
     (filter (complement sequential?))))

(defcheck solution-562fa7b7
  (fn flt [l]
    (cond
      (empty? l) ()
      (coll? (first l)) (concat (flt (first l)) (flt (rest l)))
      :else (cons (first l) (flt (rest l))))))

(defcheck solution-563657f9
  (fn [s]
    (filter (complement sequential?)
      (tree-seq sequential? seq s))))

(defcheck solution-5642cfc0
  #(filter (comp not coll?) (tree-seq coll? seq %)))

(defcheck solution-56c68ec2
  (fn n [[f & r]]
    (concat ((if (coll? f) n list) f) (if r (n r)))))

(defcheck solution-575b326a
  (fn flat [coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (if (coll? (first s))
          (concat (flat (first s)) (flat (rest s)))
          (cons (first s) (flat (rest s))))))))

(defcheck solution-57d524b5
  #(filter (comp not sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-581bb375
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-58bc1e0b
  (fn flat [x & tail]
    (concat (if (coll? x)
              (apply flat x)
              [x])
            (if (nil? tail)
              nil
              (apply flat tail)))))

(defcheck solution-590bab84
  (fn flttn [coll]
    (if (or (seq? coll) (vector? coll))
      (if (empty? coll)
        '()
        (concat (flttn (first coll)) (flttn (rest coll))))
      (list coll))))

(defcheck solution-592020f0
  (fn [s]
    (reduce (fn myflatten [collection element]
              (if (sequential? element)
                (reduce myflatten collection element)
                (conj collection element))) [] s)))

(defcheck solution-59f38769
  (fn my-flatten
    [coll]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq coll)))))

(defcheck solution-5a40e12c
  (fn myflat [coll]
    (loop [[x & xs] coll
           res []]
      (if (nil? x)
        res
        (recur
          xs
          (cond
            (nil? x) res
            (sequential? x) (vec (concat res (myflat x)))
            :default (conj res x)))))))

(defcheck solution-5a4b3373
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-5b44ef16
  (fn lflatten [col]
    (cond
      (not (coll? col)) [col]
      (empty? col) []
      :else (concat (lflatten (first col)) (lflatten (rest col))))
    ))

(defcheck solution-5b6877ab
  (fn flat [x]
    (let [seqable? (fn [x] (or (seq? x) (list? x) (vector? x)))
          conjinto (fn [xs v] (if (seqable? v) (into xs v) (conj xs v)))
          y        (reduce conjinto [] x)]
      (if (= x y) y (flat y))
      )))

(defcheck solution-5b7043e1
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-5c2159ea
  (fn [x]
    (filter (complement sequential?)
      (tree-seq sequential? identity x))))

(defcheck solution-5c29227b
  (fn flat [seq]
    (reduce (fn [acc elem]
              (if (sequential? elem) (apply conj acc (flat elem))
                                     (conj acc elem)))
      [] seq)))

(defcheck solution-5c317103
  (fn [x]
    (loop [ls x acc '()]
      (if (empty? ls)
        acc
        (let [[head & tail] ls]
          (if (or (seq? head) (vector? head))
            (recur (concat head tail) acc)
            (recur tail (concat acc (list head)))
            )
          )
        )
      )
    ))

(defcheck solution-5c6815ff
  #(filter (complement sequential?) (rest (tree-seq sequential? identity %))))

(defcheck solution-5ce505c6
  (fn [xs]
    ((fn r [acc ys]
       (if (coll? ys)
         (reduce r acc ys)
         (conj acc ys)))
     [] xs)))

(defcheck solution-5cef208d
  (fn flatten-structure [s] (if (sequential? s) (if (empty? s) '() (concat (flatten-structure (first s)) (flatten-structure (rest s))))
                                                (list s))))

(defcheck solution-5d6c9eb5
  (fn [x]
    (filter (complement sequential?)
      (tree-seq sequential? seq x)
      )
    ))

(defcheck solution-5d9b085a
  #(filter (complement sequential?) (rest (tree-seq sequential? seq %))))

(defcheck solution-5dbe6350
  #(mapcat (fn wa [x] (if-not (coll? x) (list x) (mapcat wa x))) %))

(defcheck solution-5df63298
  (fn f [x]
    (if ((complement sequential?) x)
      (list x)
      (if (empty? x)
        ()
        (concat (f (first x)) (f (rest x)))))))

(defcheck solution-5e55c775
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-5f8c2e39
  (fn f [sq]
    (if (coll? sq)
      (reverse (reduce #(apply conj %1 %2) '() (map f sq)))
      (list sq))))

(defcheck solution-5fc6a35c
  (fn [s] (filter #(not (coll? %)) (tree-seq coll? identity s))))

(defcheck solution-5fd979f7
  (fn f [s]
    (when-not (= s [])
      (if (coll? s)
        (concat (f (first s)) (f (rest s)))
        (list s)))))

(defcheck solution-5ff11a1b
  #(filter (complement sequential?) (tree-seq sequential? seq %)))

(defcheck solution-600cb16e
  (fn [seq]
    (loop [seq (reverse seq)]
      (if (every? #(not (coll? %)) seq)
        (reverse seq)
        (recur (reverse (reduce (fn [ret el]
                                  (if (coll? el)
                                    (concat el ret)
                                    (conj ret el))
                                  ) (list) seq)
                 )))
      )))

(defcheck solution-6013a2c5
  (fn trip [coll] (reduce (fn [c e] (concat c (if (coll? e) (trip e) (list e)))) [] coll)))

(defcheck solution-60366277
  (fn [collection] (let [f (fn [c] (reduce #((if (coll? %2) into conj) %1 %2) [] c))]
                     (-> collection f f f f f))))

(defcheck solution-60c6600e
  (fn flatten' [coll]
    (let [x  (first coll)
          xs (rest coll)]
      (cond (nil? x) ()
            (coll? x) (concat (flatten' x) (flatten' xs))
            :else (conj (flatten' xs) x)))))

(defcheck solution-60d532ae
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x))
      )
    ))

(defcheck solution-61076e12
  (fn flat [s]
    (if (coll? s)
      (mapcat flat s)
      (list s))))

(defcheck solution-612d56a
  (fn f [xss]
    (mapcat #((if (coll? %) f (comp seq list)) %) xss)))

(defcheck solution-612f87bc
  (fn [c]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq c)))))

(defcheck solution-61c3369b
  (fn [c]
    (loop [a c]
      (if (some coll? a)
        (recur
          (mapcat
            #(if (coll? %) % [%])
            a))
        a))))

(defcheck solution-61d658c8
  (fn f [[h & t :as s]] (if (nil? s) [] (into (if (coll? h) (f h) [h]) (f t)))))

(defcheck solution-61fcab76
  (fn c [s] (if (sequential? s) (mapcat c s) [s])))

(defcheck solution-621c7118
  (fn flat [c]
    (if (coll? c)
      (mapcat flat c)
      [c])))

(defcheck solution-6298c662
  (fn [coll]
    (seq
      (reduce
        (fn r [a e]
          (
           (if (sequential? e)
             (partial reduce r)
             conj) a e))
        []
        coll))))

(defcheck solution-62ec89db
  (fn flatten1 [x]
    (reduce
      (fn [a b]
        (if (coll? b)
          (concat a (flatten1 b))
          (concat a (list b))))
      '()
      x)))

(defcheck solution-62f7142a
  (fn flatland [a-seq]
    (if (empty? a-seq)
      []
      (let [f (first a-seq)
            r (flatland (rest a-seq))]
        (if (sequential? f)
          (concat (flatland f) r)
          (cons f r)
          )))))

(defcheck solution-63dc7535
  (fn flat [sq]
    (when (not-empty sq)
      (if (sequential? (first sq))
        (concat (flat (first sq)) (flat (rest sq)))
        (cons (first sq) (flat (rest sq)))))))

(defcheck solution-63f1ef73
  (fn flat [s]
    (if
     (coll? s) (mapcat flat s)
               [s])))

(defcheck solution-63ffc6e4
  (fn flatt [subseq]
    (if (and (or (seq? subseq) (vector? subseq)) (not (empty? subseq)))
      (concat (flatt (first subseq)) (flatt (rest subseq)))
      (if (or (seq? subseq) (vector? subseq)) subseq [subseq])
      )))

(defcheck solution-64375650
  (fn my-flatten [lst]
    (let [firstele  (first lst)
          remaining (rest lst)]
      (cond
        (nil? firstele) ()
        (coll? firstele) (concat (my-flatten firstele) (my-flatten remaining))
        :else (cons firstele (my-flatten remaining))))))

(defcheck solution-644bd726
  (fn flatten-1 [coll]
    (reduce (fn [xs x]
              (if (sequential? x)
                (apply conj xs (flatten-1 x))
                (conj xs x)))
      [] coll)))

(defcheck solution-64b4bcc9
  (fn __
    ([x]
     (let [f (first x) r (next x)]
       (concat
        (if (sequential? f)
          (__ f)
          [f]
          )
        (when (sequential? r)
          (__ r))
        )
       )
     )
    ))

(defcheck solution-64c31960
  (fn flat [x] (if (coll? x) (mapcat flat x) (list x))))

(defcheck solution-65328423
  (fn flat [sq] (mapcat #(if (coll? %) (flat %) (list %)) sq)))

(defcheck solution-660b4b85
  (fn flattenSeq
    [sequ]
    "Flattens a sequence. Finds the first nested collection
  element within the list. Concatenates the elements before it with the
  collection element, then all the elements that come after it. Makes a recursive call
  if any of the elements are collections. Otherwise returns the whole collection."
    (if (some coll? sequ)
      (flattenSeq
        (let [firstColIndex
                        ((fn findFirstColIndex              ; anonymous function to find index of first coll element
                           [index findIn]
                           (if (coll? (nth findIn index))
                             index
                             (findFirstColIndex (inc index) findIn))) 0 sequ)
              firstCol  (nth sequ firstColIndex)
              outputSeq (concat
                         (take firstColIndex sequ)          ; before coll element
                         firstCol                           ; coll element
                         (drop (+ 1 firstColIndex) sequ))]  ; after coll element
          outputSeq))
      sequ)))

(defcheck solution-6651f49d
  #(->> (tree-seq sequential? identity %)
     (filter (comp not sequential?))))

(defcheck solution-668182be
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-67af2bac
  (fn f [c]
    (if (empty? c)
      '()
      (let [[i & r] c]
        (concat (if (coll? i)
                  (f i)
                  (list i))
                (f r))))))

(defcheck solution-686baabb
  (fn fltn [xs]
    (if (empty? xs)
      ()
      (let [f       (first xs)
            f-is-sq (sequential? f)
            flat-f  (if f-is-sq (fltn f) (list f))]
        (concat flat-f (fltn (rest xs)))))))

(defcheck solution-6888b9b4
  (fn flatten-sequence [xs]
    (mapcat
      #(if (sequential? %) (flatten-sequence %) (list %)) xs)
    ))

(defcheck solution-68aa4a6e
  (fn flat [x]
    (if (vector? x)
      (flat (seq x))
      (if (seq? x)
        (if (= x '())
          '()
          (concat (flat (first x)) (flat (rest x)))
          )
        (list x)
        )
      )
    ))

(defcheck solution-690cb076
  (fn my-flatten [coll]
    (mapcat (fn [e] (if (coll? e) (my-flatten e) (list e))) coll)))

(defcheck solution-692e6fca
  (fn flat [in]
    (if (coll? in)
      (apply concat
        (for [elt in]
          (flat elt)))
      (list in))))

(defcheck solution-6a5402fd
  (fn flt [x] (if (= x [])
                '()
                (if (sequential? x)
                  (concat (flt (first x))
                          (flt (rest x)))
                  (list x)))))

(defcheck solution-6ad6d1b9
  ;I am sure that there is an easier way to do this. This solution is limited to 4 sub collections.
  (fn [coll]
    (loop [out []
           in  coll]
      (if (empty? in)
        out
        (recur (if (coll? (first in))
                 (loop [o out
                        i (first in)]
                   (if (empty? i)
                     o
                     (recur (if (coll? (first i))
                              (loop [o3 o
                                     i3 (first i)]
                                (if (empty? i3)
                                  o3
                                  (recur (if (coll? (first i3))
                                           (loop [o4 o3
                                                  i4 (first i3)]
                                             (if (empty? i4)
                                               o4
                                               (recur (conj o4 (first i4))
                                                 (drop 1 i4))))
                                           (conj o3 (first i3)))
                                    (drop 1 i3))))
                              (conj o (first i)))
                       (drop 1 i))))
                 (conj out (first in)))
          (drop 1 in))))))

(defcheck solution-6af1e710
  #(letfn [(my-flatten [xs]
             (loop [x  (first xs)
                    xs (rest xs)
                    ys []]
               (if (nil? x)
                 (seq ys)
                 (if (coll? x)
                   (recur (first x) (concat (rest x) xs) ys)
                   (recur (first xs) (rest xs) (conj ys x))))))]
     (my-flatten %)))

(defcheck solution-6b8d0c33
  (fn flat [data]
    (if (coll? data)
      (mapcat flat data)
      (list data))))

(defcheck solution-6bd3f4ce
  #(loop [x % y []] (if (empty? x)
                      y
                      (let [a (first x) b (rest x)]
                        (if (coll? a)
                          (recur (concat a b) y)
                          (recur b (conj y a)))))))

(defcheck solution-6bf8dfe0
  (fn fla [x]
    (let [f (first x) n (next x)]
      (concat
       (if (sequential? f) (fla f) [f])
       (if (sequential? n) (fla n) [])
       )
      )
    ))

(defcheck solution-6c8231a6
  (fn foo [s]
    (cond (empty? s) s
          (coll? (first s))
          (concat (foo (first s))
                  (foo (rest s)))
          :else (cons (first s) (foo (rest s))))))

(defcheck solution-6c849508
  (fn f [s] (if (not (sequential? s)) [s] (mapcat f s))))

(defcheck solution-6d178565
  (fn dorec [s] (cond (sequential? s) (apply concat (map dorec s)) :else (cons s nil))))

(defcheck solution-6d613416
  (fn my-flatten [s] (let [new-s (mapcat #(if (sequential? %) % (list %)) s)]
                       (if (some sequential? new-s) (my-flatten new-s)
                                                    new-s))))

(defcheck solution-6de0f707
  (fn flt [xs]
    (if (empty? xs)
      ()
      (let [x1 (first xs) xr (rest xs)]
        (if (coll? x1)
          (if (empty? x1)
            (flt xr)
            (flt (cons (first x1) (cons (rest x1) xr))))
          (cons x1 (flt xr)))))))

(defcheck solution-6e51c232
  (fn collect [x]
    (if (coll? x)
      (if (empty? x)
        []
        (concat (collect (first x)) (collect (rest x))))
      [x])))

(defcheck solution-6ee60599
  (fn ! [input]
    (loop [result [] elements input]
      (if (empty? elements)
        result
        (if (coll? (first elements))
          (recur (into result (! (first elements))) (rest elements))
          (recur (conj result (first elements)) (rest elements))
          )
        )
      )
    ))

(defcheck solution-6fcb198d
  (fn flatten-seq [coll]
    (if (coll? coll) (mapcat flatten-seq coll) [coll])))

(defcheck solution-6ff07515
  (fn my-flatten [l]
    (loop [l1 l, l2 `()]
      (cond
        (sequential? (first l1)) (recur (concat (first l1) (rest l1)) l2)
        (empty? l1) (reverse l2)
        :else (recur (rest l1) (cons (first l1) l2))))))

(defcheck solution-701a02d6
  (fn [coll]
    (filter (complement coll?) (tree-seq coll? seq coll))))

(defcheck solution-705d257b
  (fn [l]
    (loop [news '() tmpl (seq l)]
      (if (empty? tmpl)
        (reverse news)
        (if (not (sequential? (first tmpl)))
          (recur (conj news (first tmpl)) (rest tmpl))
          (recur news (concat (first tmpl) (rest tmpl))))))))

(defcheck solution-7073b9ae
  (fn flatten* [curr-seq]
    (if (empty? curr-seq)
      []
      (let [elem (first curr-seq)]
        (cond
          (and (sequential? elem) (empty? elem)) (flatten* (rest curr-seq))
          (and (sequential? elem)) (concat (flatten* elem) (flatten* (rest curr-seq)))
          true (concat [elem] (flatten* (rest curr-seq))))))))

(defcheck solution-70944a97
  (fn myflatten [xs]
    (if (empty? xs)
      xs
      (if (sequential? (first xs))
        (concat (myflatten (first xs)) (myflatten (rest xs)))
        (cons (first xs) (myflatten (rest xs)))))))

(defcheck solution-7150e944
  (fn ft [coll]
    (if (sequential? coll)
      (mapcat ft coll)
      (list coll))))

(defcheck solution-734f67e7
  (fn flatten2 [xs]
    (if (coll? xs)
      (if (empty? xs)
        ()
        (concat (flatten2 (first xs)) (flatten2 (rest xs)))
        )
      (cons xs ())
      )
    ))

(defcheck solution-7379c422
  (fn f1 [xs]
    (if (sequential? xs)
      (let [xs1 (vec xs)]
        (if (empty? xs1)
          ()
          (reduce conj (f1 (first xs1)) (f1 (rest xs1)))))
      [xs]
      )))

(defcheck solution-737d7fca
  (fn [elems]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq elems)))))

(defcheck solution-73a9d026
  (fn fltn [[h & t]] (concat
                      (if (sequential? h) (fltn h)
                                          [h])
                      (if (sequential? t) (fltn t)))))

(defcheck solution-73bc8d48
  (fn bulldoze [c]
    (if (sequential? c)
      (mapcat bulldoze c)
      (list c))))

(defcheck solution-747fc1c4
  (fn my-flatten [& coll]
    (->>
      (tree-seq sequential? identity coll)
      (filter #(not (sequential? %))))))

(defcheck solution-75e24843
  (fn flat [coll]
    (reduce
      (fn [v elm]
        (concat v (if (sequential? elm)
                    (flat elm)
                    [elm])))
      []
      coll)))

(defcheck solution-766fb16c
  (fn f [v] (mapcat (fn [x] (if (sequential? x) (f x) [x])) v)))

(defcheck solution-76b613a0
  (fn iter
    ([xs] (iter xs []))
    ([xs rs] (cond (empty? xs) rs
                   (coll? (first xs)) (recur (concat (first xs) (rest xs)) rs)
                   :else (recur (rest xs) (conj rs (first xs)))))))

(defcheck solution-76ef4490
  (fn flatten-it [[head & tail :as s]]
    (cond
      (empty? s) ()
      (coll? head) (concat (flatten-it head) (flatten-it tail))
      :if-those-other-dogs-won't-hunt (cons head (flatten-it tail)))))

(defcheck solution-7700b385
  (fn flat [coll]
    (loop [items coll result []]
      (cond
        (empty? items) result
        (coll? (first items)) (recur (rest items) (apply conj result (flat (first items))))
        :else
        (recur (rest items) (conj result (first items)))
        )
      )
    ))

(defcheck solution-7711b66a
  (fn _flatten [items]
    (if (empty? items)
      '()
      (concat (if (sequential? (first items))
                (_flatten (first items))
                (list (first items)))
              (_flatten (rest items))))))

(defcheck solution-77af5652
  (fn [x] (filter (complement sequential?)
            (rest (tree-seq sequential? seq x)))))

(defcheck solution-77de0426
  (fn g [x] (if (coll? x) (mapcat g x) [x])))

(defcheck solution-786acbce
  (fn flatten2 [coll]
    (reduce (fn [acc x]
              (if-not (sequential? x)
                (lazy-cat acc (list x))
                (lazy-cat acc (flatten2 x)))) '() coll)))

(defcheck solution-78b9d657
  (comp keys
        (fn ffl [lst]
          (if (sequential? lst)
            (into {}
              (for [e lst]
                (ffl e)))
            [lst 0]))))

(defcheck solution-7976f935
  (fn flat [e]
    (if (coll? e)
      (reduce #(if (coll? %2)
                 (concat %1 (flat %2))
                 (concat %1 (vector %2))) [] e)
      e)))

(defcheck solution-79879020
  #(letfn
    [(fl [x]
       (if (not (coll? x))
         (list x)
         (apply concat (map fl x))))]
     (fl %)))

(defcheck solution-7990c26b
  (fn flat [x]
    (apply concat (map #(if (coll? %)
                          (flat %)
                          [%]) x))))

(defcheck solution-7a0ad7b
  (fn my-flatten [s]
    (cond (empty? s) '()
          (sequential? (first s)) (concat (my-flatten (first s))
                                          (my-flatten (rest s)))
          :else (cons (first s) (my-flatten (rest s))))))

(defcheck solution-7a3edf51
  #(filter (complement sequential?)
     (tree-seq sequential? identity %)))

(defcheck solution-7b18dec0
  (fn fltn [s]
    (apply concat
      (map
        (fn [a] (if (sequential? a) (fltn a) (list a)))
        (if (sequential? s) s '())
        )
      )
    ))

(defcheck solution-7b605a7
  (fn flat [xs]
    (if (coll? xs)
      (if (empty? xs)
        '()
        (concat (flat (first xs)) (flat (rest xs))))
      (list xs))))

(defcheck solution-7b6d82e8
  (fn flat [l]
    (loop [l  l
           fl ()]
      (cond
        (empty? l) (reverse fl)
        (or (seq? (first l)) (vector? (first l)))
        (recur
          (remove nil?
            (list (first (first l)) (next (first l)) (next l)))
          fl)
        :else
        (recur
          (remove nil? (next l))
          (cons (first l) fl))))))

(defcheck solution-7ba2264d
  (fn myflatten [xs]
    (
     (fn _flatten [xs o]
       (if-not (coll? xs)
         [xs]
         (if (empty? xs)
           []
           (concat (myflatten (first xs)) (myflatten (rest xs)))
           )
         )
       )
     xs
     []
     )
    ))

(defcheck solution-7c2c72ce
  (fn fl [x]
    (if (coll? x)
      (if (empty? x)
        '()
        (concat (fl (first x))
                (fl (rest x))))
      (list x))))

(defcheck solution-7c7f314d
  (fn my-flatten [s]
    (reduce (fn [result head] (concat result
                                      (if (sequential? head)
                                        (my-flatten head)
                                        [head])))
      [] s)))

(defcheck solution-7c859c18
  (fn flt [s]
    (if (empty? s) `()
                   (let [[f & r] s]
                     (if (sequential? f)
                       (lazy-cat (flt f) (flt r))
                       (cons f (lazy-seq (flt r))))))))

(defcheck solution-7cda1c38
  (fn f [L] (mapcat #(if (coll? %) (f %) [%]) L)))

(defcheck solution-7d13a60
  (fn fltn [args]
    (reduce (fn [a b]
              (if (sequential? b)
                (apply conj a (fltn b))
                (conj a b))) [] args)))

(defcheck solution-7d4a0287
  (fn flatten2 [a]
    (concat (if (sequential? (first a))
              (flatten2 (first a))
              (list (first a)))
            (if (sequential? (next a))
              (flatten2 (next a))
              ))
    ))

(defcheck solution-7d56516
  (fn [xs] (filter #(not (sequential? %)) (tree-seq sequential? identity xs))))

(defcheck solution-7d69b0b4
  (fn number28 [xs]
    (let [x (first xs) ys (rest xs)]
      (concat
       (if (coll? x) (number28 x) [x])
       (when (not-empty ys) (number28 ys))))))

(defcheck solution-7db1ce09
  (fn fl [x] (mapcat #(if (coll? %) (fl %) (vector %)) x)))

(defcheck solution-7dbd0cac
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %1))))

(defcheck solution-7e34533c
  (fn f [c]
    (if (coll? c)
      (apply concat (map f c))
      (conj '() c))))

(defcheck solution-7e6594c7
  (fn f [s]
    (mapcat #(if (coll? %) (f %) [%]) s)))

(defcheck solution-7eab15d4
  #(remove coll?
     (tree-seq coll? seq %)
     ))

(defcheck solution-7ef49033
  (fn [s]
    (loop [s s]
      (let [[ns changed?] (reduce (fn [[a changed?] si]
                                    (if (coll? si) [(reduce conj a si) true]
                                                   [(conj a si) changed?]))
                            [[] false] s)]
        (if changed? (recur ns) ns)))))

(defcheck solution-7f1eb4aa
  (fn fff [in]
    (if (sequential? in)
      (if (not-empty in)
        (concat (fff (first in)) (fff (rest in)))
        in)
      [in])))

(defcheck solution-7f33133c
  (fn flat [xs]
    (if (sequential? xs)
      (mapcat flat xs)
      (list xs))))

(defcheck solution-7f4a5df0
  (fn f [v] (apply concat (map #(if (sequential? %) (f %) [%]) v))))

(defcheck solution-7feab41
  (fn mf [coll]
    (loop [[hd & tl :as all] coll acc ()]
      (if (empty? all)
        acc
        (recur tl (concat acc (if (sequential? hd) (mf hd) (list hd))))))))

(defcheck solution-802764be
  (fn [coll]
    (filter (complement coll?)
      (rest (tree-seq coll? seq coll)))))

(defcheck solution-803a33d8
  (fn flat
    [[l & r]]
    (concat
     (if (coll? l) (flat l) (list l))
     (if (nil? r) '() (if (coll? r) (flat r) (list r))))))

(defcheck solution-806a79e6
  (fn flatt [xs]
    (if (empty? xs)
      ()
      (let [x  (first xs)
            fx (if (sequential? x)
                 (flatt x)
                 (list x))]
        (apply conj (flatt (rest xs)) (reverse fx))))))

(defcheck solution-80869b19
  (fn f [s]
    (reduce #(concat % (if (coll? %2) (f %2) [%2])) () s)))

(defcheck solution-81a0395a
  (fn flat [in]
    (reverse
      (reduce
        (fn [fst sec]
          (if (coll? sec)
            (into fst (flat sec))
            (into fst (list sec))))
        '()
        in))))

(defcheck solution-81a79f79
  #(filter (complement sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-8211e151
  (fn [c]
    (if (empty? (filter coll? c))
      c
      (recur
        ((fn [c1]
           (reduce
             (fn [r x] (concat x r))
             nil
             ((partial map (fn [x] (if (coll? x) (seq x) (list x))))
              (reverse c1))
             )) c)
        ))))

(defcheck solution-821267d2
  (fn fltn [coll]
    (if (sequential? coll)
      (mapcat fltn coll)
      (list coll))))

(defcheck solution-827815b7
  (fn f [s]
    (mapcat #(if (coll? %)
               (f %)
               (list %))
      s)))

(defcheck solution-82b3fb3e
  (fn my-flatten [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-82e1ea89
  (fn f [s]
    (if (coll? s)
      (mapcat f s)
      (list s))))

(defcheck solution-831f53f3
  #(filter (fn [n] (not (or (list? n) (vector? n)))) (tree-seq (fn [n] (or (list? n) (vector? n))) identity %)))

(defcheck solution-8366bcee
  (fn [coll]
    (loop [[fst & rst :as coll] coll
           acc []]
      (if (seq coll)
        (if (sequential? fst)
          (let [[a & b] fst]
            (recur (cons a (concat b rst)) acc))
          (recur rst (conj acc fst)))
        acc))))

(defcheck solution-843e3232
  (fn flt
    ([coll] (flt coll '()))
    ([coll res] (reduce #(concat %1 (if (coll? %2) (flt %2) [%2])) res coll))))

(defcheck solution-8447ff4a
  (fn [seq]
    (letfn [(inner [left [f & r]]
              (cond (and (not (coll? f)) (nil? f))
                    left
                    (coll? f)
                    (recur left (concat f r))
                    :t
                    (recur (conj left f) r)))]
      (inner [] seq))))

(defcheck solution-85608ba5
  (fn flat [x]
    (if (sequential? x)
      (let [[a & b] x]
        (if (sequential? a)
          (if b
            (concat (apply concat (map flat a)) (flat b))
            (apply concat (map flat a)))
          (if b
            (cons a (flat b))
            (list a))))
      (list x))))

(defcheck solution-856a3182
  (fn myflatten [x]
    (let [result '()
          flat?  (empty? (filter coll? x))]
      (if flat?
        x
        (myflatten (reduce
                     (fn [x y]
                       (let [a (if (coll? x) x (list x))
                             b (if (coll? y) y (list y))]
                         (concat a b))) x))))))

(defcheck solution-85b1e71
  (letfn [(flt [coll]
            (when-let [s (seq coll)]
              (let [fst (first s) nxt (next s)]
                (if (coll? fst) (concat (flt fst) (flt nxt))
                                (cons fst (flt nxt))))))]
    flt))

(defcheck solution-867f6254
  #(filter
     (complement sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-86c6a6fa
  (fn [coll]
    (let [n-coll
          (reduce
            (fn [a b]
              (if (sequential? b)
                (into a b)
                (conj a b)))
            [] coll)]
      (if (every? (complement sequential?) n-coll)
        n-coll
        (recur n-coll)))))

(defcheck solution-86cd2be7
  #(filter (complement sequential?)
     (rest (tree-seq sequential? identity %)))

  ; This is the clojure definition for flatten.
  ; We take rest since we know first element will be the seq itself.
  )

(defcheck solution-873e32c6
  (fn my-flatten
    ([x] (mapcat #(if (coll? %) (my-flatten %) [%]) x))
    ([x & more] (my-flatten (cons x more)))))

(defcheck solution-87419e29
  (fn [coll]
    (loop [out-coll []
           in-coll  (seq coll)]
      (cond
        (empty? in-coll) out-coll
        (sequential? (first in-coll)) (recur out-coll (concat (first in-coll) (rest in-coll)))
        :default (recur (conj out-coll (first in-coll)) (next in-coll))))))

(defcheck solution-878324fe
  (fn this
    ([xs] (this xs []))
    ([xs acc]
     (if (empty? xs)
       acc
       (let [[hd & tl] xs]
         (if (sequential? hd)
           (recur (concat hd tl) acc)
           (recur tl (conj acc hd))))))))

(defcheck solution-87bc66a
  (fn my-flatten [coll]
    (when-let [s (seq coll)]
      (if (coll? (first s))
        (concat (my-flatten (first s)) (my-flatten (rest s)))
        (cons (first s) (my-flatten (rest s)))))))

(defcheck solution-88c0cb1b
  (fn flat [l] (if (coll? l) (reduce concat (map flat l)) (list l))))

(defcheck solution-88cb07c6
  (fn flat
    [coll]
    (reduce #(concat %1 (if (coll? %2)
                          (flat %2) [%2])) '() coll)))

(defcheck solution-8945ec4c
  (fn f [l]
    (if (sequential? l)
      (reduce (fn [a e] (concat a (f e))) '() l)
      (list l))))

(defcheck solution-8a0b0b21
  (fn flat
    [c] (reduce
          (fn [a b]
            (if (sequential? b)
              (concat a (flat b))
              (concat a [b])))
          []
          c)))

(defcheck solution-8a10cf2
  (fn [seq]
    (loop [seq    seq
           result []]
      (if (empty? seq)
        result
        (let [[head & tail] seq]
          (if (or (list? head) (vector? head))
            (recur (concat head tail) result)
            (recur tail (conj result head))))))))

(defcheck solution-8ad3188e
  (fn mkflat [s]
    (lazy-seq
      (if (empty? s) s
                     (if (sequential? (first s))
                       (concat (mkflat (first s)) (mkflat (rest s)))
                       (conj (mkflat (rest s)) (first s)))))))

(defcheck solution-8ae0e337
  (fn ! [coll]
    (reduce (fn [a b] (if (or (list? b) (vector? b))
                        (concat a (! b))
                        (concat a [b])))
      []
      coll)))

(defcheck solution-8af2145
  (fn fltn
    ([_seq]
     (fltn (first _seq) (rest _seq) '()))
    ([_first _rest result]
     (if (nil? _first)
       result
       (if (coll? _first)
         (fltn (first _first) (concat (rest _first) _rest) result)
         (fltn (first _rest) (rest _rest) (reverse (conj (reverse result) _first))))))))

(defcheck solution-8b47bb09
  (fn flat [n]
    (let [[x & xs] n]
      (cond
        (empty? n) '()
        (coll? x) (concat (flat x) (flat xs))
        :else (cons x (flat xs))))))

(defcheck solution-8c048e5a
  (fn flat [s]
    (reduce #(concat % (if (coll? %2) (flat %2) (list %2))) '() s)))

(defcheck solution-8c1e128b
  (fn [lst]
    ((fn inner [lst]
       (cond
         (empty? lst) ()
         (coll? (first lst)) (concat (inner (first lst)) (inner (rest lst)))
         :else (conj (inner (rest lst)) (first lst))))
     lst)))

(defcheck solution-8c3d9539
  (fn flatten- [x]
    "28. Write a function which flattens a sequence."
    (if (sequential? x)
      (mapcat flatten- x)
      (list x))))

(defcheck solution-8c9007b
  (fn [str]
    (filter (complement sequential?)
      (tree-seq sequential? seq str))))

(defcheck solution-8c95f5
  (fn [form]
    (let [todo   (atom [form])
          result (atom [])]
      (while (seq @todo)
        (let [curr (peek @todo)]
          (swap! todo pop)
          (if (sequential? curr)
            (doseq [kid (reverse curr)]
              (swap! todo conj kid))
            (swap! result conj curr))))
      @result)))

(defcheck solution-8c9e8299
  (fn f [v]

    (reduce (fn [acc i]
              (if (counted? i)
                (concat acc (f i))
                (concat acc [i])
                )

              ) [] v)

    ))

(defcheck solution-8cc3e702
  (fn f [c]
    (mapcat (fn [i]
              (if (coll? i) (f i) [i])) c)))

(defcheck solution-8cdb487f
  (fn flat [s] (if (sequential? s) (apply concat (map flat s)) [s])))

(defcheck solution-8d03ab35
  (fn new-flatten [x] (loop [[h & t] x acc []]
                        (let [newacc (if (sequential? h) (into acc (new-flatten h))
                                                         (conj acc h))]
                          (if (= t nil) newacc (recur t newacc))))))

(defcheck solution-8d34652a
  (fn flat-map
    [x]
    (cond
      (not (coll? x)) (list x)
      (empty? x) '()
      :else (apply concat (map flat-map x)))))

(defcheck solution-8d6a2e02
  (fn [s]
    (letfn [(aux [s]
              (loop [head (first s) tail (rest s) acc ()]
                (cond (nil? head) acc
                      (coll? head) (recur (first tail)
                                     (rest tail)
                                     (concat (aux head) acc))
                      :else (recur (first tail)
                              (rest tail)
                              (cons head acc)))))]
      (reverse (aux s)))))

(defcheck solution-8d7de13
  (fn [lst]
    (letfn [(squish [x]
              (apply concat
                (map #(if (sequential? %) % (list %)) x)))]
      (if
       (= lst (squish lst))
        lst
        (recur (squish lst))))))

(defcheck solution-8d87dd3c
  (fn f [x] (if (coll? x) (mapcat f x) [x])))

(defcheck solution-8daa7979
  (fn ! [s]
    (if (empty? s) '()
                   (let [f (first s) r (rest s)]
                     (if (coll? f) (concat (! f) (! r))
                                   (cons f (! r))
                                   )
                     )
                   )
    ))

(defcheck solution-8e16a605
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-8e6264e9
  (fn mfl [coll]
    (lazy-seq (cond
                (nil? coll) nil
                (sequential? coll)
                (if (empty? coll) coll (concat (mfl (first coll))
                                               (mfl (rest coll))))
                :else (cons coll ())))))

(defcheck solution-8eac3e10
  (fn f [s]
    (cond
      (not (coll? s)) [s]
      (not (seq s)) nil
      :else (concat (f (first s)) (f (rest s))))))

(defcheck solution-8ee8dde1
  (fn flat [x] (if (coll? x) (mapcat flat x) [x])))

(defcheck solution-8f295cd2
  (fn myflatten [coll]
    (if (sequential? coll)
      (mapcat myflatten coll)
      (list coll))))

(defcheck solution-8f587bfd
  (fn
    [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-901c01f
  (fn my-flatten [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-901cedef
  (fn flat-e
    [x]
    (cond
      (not (sequential? x)) (list x)
      (empty? x) '()
      :else (concat (flat-e (first x)) (flat-e (rest x))))))

(defcheck solution-9065ed71
  (fn f [x] (filter #(not (coll? %)) (tree-seq coll? identity x))))

(defcheck solution-908cf64d
  #(letfn [(expand [s] (apply concat (map (fn [e] (if (coll? e) (expand e) [e])) s)))] (expand %)))

(defcheck solution-9097d072
  (fn my-flatten [input]
    (filter #(not (sequential? %)) (tree-seq sequential? seq input))))

(defcheck solution-910fe4cd
  (fn my_flatten [& rest]
    (let [w (let [z (first rest)]
              (if (sequential? z) (apply my_flatten z) (list z)))]
      (let [y (next rest)] (if y (concat w (apply my_flatten y)) w)))))

(defcheck solution-911b9588
  (fn fln [x]
    (if (sequential? x)
      (mapcat fln x)
      [x])))

(defcheck solution-911d1736
  (fn aux [s]
    (if (coll? s)
      (reduce into [] (map aux s))
      (seq [s]))))

(defcheck solution-916d11dc
  #(reverse ((fn unwrap [accum item]
               (
                 if (coll? item)
                 (reduce unwrap accum item)
                 (conj accum item)
                 )) '() %)))

(defcheck solution-921fbc7e
  (fn fltn [s]
    (mapcat
      #(if (sequential? %)
         (fltn %)
         (list %))
      s)))

(defcheck solution-92af0063
  (fn flttn [x]
    (if (= x '())
      x
      (if (coll? x)
        (if (coll? (first x))
          (concat (flttn (first x)) (flttn (rest x)))
          (cons (first x) (flttn (rest x))))
        x))))

(defcheck solution-92b2953b
  (fn [x] (filter (complement sequential?) (rest (tree-seq sequential? seq x)))))

(defcheck solution-93966259
  (fn flat [coll]
    (if (sequential? coll)
      (if (empty? coll)
        coll
        (concat
         (flat (first coll))
         (flat (rest coll))))
      (list coll))))

(defcheck solution-93bdfb64
  (fn flat [lst]
    (let [[f & r] lst]
      (cond
        (empty? lst) nil
        (not (sequential? f)) (cons f (flat r))
        :else (concat (flat f) (flat r))
        )
      )
    ))

(defcheck solution-93f979f6
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-94322d26
  (fn flat [ls]
    (if (not (coll? ls))
      (list ls)
      (mapcat flat ls)
      )
    ))

(defcheck solution-945d5da5
  (fn my_flatten [L]
    (loop [xs L, result []]
      (if (empty? xs)
        result
        (let [x (first xs),
              y (if (coll? x) (my_flatten x) [x])]
          (recur (rest xs) (apply conj result y)))))))

(defcheck solution-94627614
  (fn newflatten [x]
    (if (sequential? x)
      (mapcat newflatten x)
      (vector x))))

(defcheck solution-94b76055
  (letfn [
          (myflatten [coll]
            (mapcat #(if (coll? %) (myflatten %) [%]) coll)
            )]
    #(myflatten %)))

(defcheck solution-94e74ca0
  (fn mflat [lst]
    (cond (empty? lst) ()
          ((complement coll?) (first lst)) (cons (first lst) (mflat (rest lst)))
          :t (concat (mflat (first lst))
                     (mflat (rest lst))))))

(defcheck solution-9556e6d
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-95570dec
  (fn [xs] (filter (complement sequential?) (tree-seq sequential? seq xs))))

(defcheck solution-9582d20e
  (fn fltn [lst]
    (let
     [fst (first lst)
      rst (rest lst)]

      (cond
        (empty? lst) lst
        (sequential? fst)
        (into
          (fltn rst)
          (reverse (fltn fst)))
        :else (cons fst (fltn rst))))))

(defcheck solution-9598fab8
  (fn ! ([x & xs] (concat (if (coll? x) (apply ! x) [x]) (apply ! xs))) ([] [])))

(defcheck solution-96327bf2
  (fn flat [x] (mapcat #(if (sequential? %) (flat %) [%]) x)))

(defcheck solution-9679e4b7
  (fn f [s]
    (loop [ret '() s_ s]
      (if s_
        (let [a (first s_)]
          (recur
            (if (coll? a)
              (apply conj ret (f a))
              (conj ret a))
            (next s_)))
        (into '() ret)))))

(defcheck solution-967f74c0
  (fn flat [s]
    (if (sequential? s)
      (mapcat flat s)
      (list s)
      )
    ))

(defcheck solution-96d38315
  (fn [x] (filter #(not (coll? %)) (rest (tree-seq coll? seq x)))))

(defcheck solution-972fcd73
  (fn f [x] (mapcat #(if (sequential? %) (f %) [%]) x)))

(defcheck solution-97510245
  (fn [xs]

    (letfn [(flatten-rec [xs accum]
              (cond
                (empty? xs) accum
                (coll? (first xs)) (flatten-rec (rest xs) (flatten-rec (first xs) accum))
                :else (flatten-rec (rest xs) (conj accum (first xs)))))]
      (flatten-rec xs []))))

(defcheck solution-979565bc
  #(->> (tree-seq coll? vec %) (remove coll?)))

(defcheck solution-97b37e52
  (fn flatten' [coll] (mapcat #(if (sequential? %) (flatten' %) (list %)) coll)))

(defcheck solution-9838b011
  (fn flat [acol]
    (mapcat #(if (coll? %1)
               (flat %1)
               [%1])
      acol)))

(defcheck solution-98c43aab
  (fn [x]
    (filter #(not (sequential? %))
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-98eb7e2e
  (fn f [r] (mapcat #(if (coll? %) (f %) [%]) r)))

(defcheck solution-98ed771d
  (fn f [coll]
    (mapcat #(if (sequential? %) (f %) (list %)) coll)))

(defcheck solution-98ff0997
  (fn flat [xs]
    (if (some #(or (vector? %) (list? %)) xs)
      (flat (mapcat #(if (or (vector? %) (list? %)) % (list %)) xs))
      xs)))

(defcheck solution-994a42be
  (fn [s]
    ((fn inner [x, r]
       (if (empty? x) r
                      (if (or (seq? (first x)) (vector? (first x)))
                        (concat (inner (first x) '()) (inner (rest x) r))
                        (conj (inner (rest x) r) (first x))))) s '())))

(defcheck solution-99d9eb85
  (fn flat [s] (if (sequential? s) (mapcat flat s) (list s))))

(defcheck solution-99f2254c
  (fn [x] (filter (complement sequential?)

            (rest (tree-seq sequential? seq x)))))

(defcheck solution-99f9e9d5
  (fn test1 [x]
    (reduce (fn s [z y]
              (if (coll? y)
                (concat z (test1 y))
                (concat z (list y))))
      '() x)))

(defcheck solution-99fbb881
  (fn _flatten [l]
    (if (empty? l)
      l
      (let [[h & r] l]
        (if (coll? h)
          (concat (_flatten h) (_flatten r))
          (cons h (_flatten r)))))))

(defcheck solution-9a8628e2
  (fn flatten-helper [l]
    (cond
      (empty? l) '()
      (not (sequential? (first l)))
      (conj (flatten-helper (rest l)) (first l))
      :else
      (concat (flatten-helper (first l)) (flatten-helper (rest l))))))

(defcheck solution-9aaaf413
  (fn [ss]
    (letfn [(flt [s]
              (if (coll? s)
                (apply concat (map flt s))
                [s]))]
      (flt ss))))

(defcheck solution-9ab82c2e
  (fn flat [xs]
    (loop [ys (seq xs)
           zs (empty ys)]
      (if (seq ys)
        (if (coll? (first ys))
          (recur (rest ys) (into zs (flat (first ys))))
          (recur (rest ys) (conj zs (first ys))))
        (reverse zs)))))

(defcheck solution-9abe0120
  (fn my-flatten [coll]
    (cond
      (empty? coll)
      coll

      (coll? (first coll))
      (concat (my-flatten (first coll)) (my-flatten (rest coll)))

      :else
      (cons (first coll) (my-flatten (rest coll))))))

(defcheck solution-9b0f2060
  (fn my-flatten [x]
    (cond
      (coll? x) (mapcat my-flatten x)
      :else [x])))

(defcheck solution-9b8d2572
  (fn flat [x]
    (reduce #(concat % (if (coll? %2)
                         (flat %2)
                         (list %2)))
      ()
      x)))

(defcheck solution-9bddfdb6
  (fn fl [s]
    (if (not (coll? s))
      (list s)
      (if (empty? s)
        s
        (concat (fl (first s)) (fl (rest s)))))))

(defcheck solution-9ccf588e
  (fn flat [sq] (if (empty? sq) '() (if (coll? (first sq)) (concat (flat (first sq)) (flat (rest sq))) (cons (first sq) (flat (rest sq)))))))

(defcheck solution-9d2ccb5d
  (fn [x] (let [s? sequential?] (filter (complement s?) (tree-seq s? seq x)))))

(defcheck solution-9d6bd93e
  #(lazy-seq
     (letfn [(f [l]
               (if (empty? l) l
                              (let [[x & xs] l]
                                (concat (if (coll? x) (f x) [x]) (f xs)))))]
       (f %))))

(defcheck solution-9e0e477d
  (fn flat [x]
    (if (coll? x)
      (reduce concat (map flat x))
      (list x))))

(defcheck solution-9e254cb0
  (fn f [sq]
    (if (coll? sq)
      (apply concat (map f sq))
      [sq])))

(defcheck solution-9e469e43
  (fn fltn [s]
    (reduce
      #(if
        (coll? %2)
         (concat %1 (fltn %2))
         (concat %1 (list %2)))
      (list)
      s)))

(defcheck solution-9e58ba59
  (fn unwrap [thing]
    (if (coll? thing)
      (apply concat (map unwrap thing))
      (list thing))))

(defcheck solution-9ec3bd2f
  (fn fltn [x]
    (if (sequential? x)
      (apply concat (map fltn x))
      [x])
    ))

(defcheck solution-9f0c1cba
  (fn flat [x] (reduce #(if (sequential? %2) (into %1 (flat %2)) (conj %1 %2)) [] x)))

(defcheck solution-9f84f664
  (fn ! [coll]
    (if (coll? coll) (mapcat ! coll) [coll])))

(defcheck solution-9faeadd4
  (fn f [xs] (mapcat #(if (sequential? %) (f %) (list %)) xs)))

(defcheck solution-9fb5437b
  (fn [sq]
    ((fn flatten- [sq r]
       (if (empty? sq)
         r
         (recur (rest sq)
           (concat r
                   (if (sequential? (first sq))
                     (flatten- (first sq) [])
                     [(first sq)])))))
     sq [])))

(defcheck solution-9fc1583
  (fn [l] (loop [l l acc []] (let [f (first l) r (rest l)] (if (empty? l) acc (if (sequential? f) (recur (concat f r) acc) (recur r (conj acc f))))))))

(defcheck solution-9fc4ee76
  (fn flat [coll]
    (mapcat
      (fn [x]
        (if (coll? x)
          (flat x)
          [x]
          )
        )
      coll
      )
    ))

(defcheck solution-a0d89099
  #(filter (complement sequential?) (tree-seq sequential? seq %1)))

(defcheck solution-a0e163b1
  (fn linearize [graph]
    (if (empty? graph)
      '()
      (if (sequential? (first graph))
        (concat (linearize (first graph)) (linearize (rest graph)))
        (cons (first graph) (linearize (rest graph)))))))

(defcheck solution-a1274c26
  (fn flt [s]
    (if (sequential? s)
      (mapcat flt s)
      (list s))))

(defcheck solution-a18107dc
  (fn myFlatten
    [x]
    (filter #(not (sequential? %))
      (tree-seq sequential? seq x))))

(defcheck solution-a26b2e69
  (fn flat [x]
    (if (sequential? x)
      ; Recurse and concatenate results
      (distinct (mapcat flat x))
      ; else just eval to a singleton list
      (list x)
      )))

(defcheck solution-a27061c1
  (fn my-flatten [s]
    (apply concat (map #(if (coll? %) (my-flatten %) [%]) s))))

(defcheck solution-a29d28d3
  (fn f [s]
    (if (empty? s) s
                   (let [h (first s)
                         t (rest s)] (if (coll? h) (concat (f h) (f t))
                                                   (cons h (f t)))))))

(defcheck solution-a30fa48e
  (fn flat [[h & t]]
    (let [h (if (coll? h) (flat h) [h])
          t (if (empty? t) t (flat t))]
      (concat h t))))

(defcheck solution-a312f950
  (fn [c]
    (if (some coll? c)
      (recur (apply concat (map #(if (coll? %) % (list %)) c)))
      c)))

(defcheck solution-a326865c
  (fn f [c]
    (if (coll? c)
      (mapcat f c)
      [c])))

(defcheck solution-a401fe0a
  #(remove coll? (tree-seq coll? identity %)))

(defcheck solution-a4620c0d
  (fn fla [coll]
    (if (sequential? coll)
      (mapcat fla coll)
      (list coll))))

(defcheck solution-a498b9e8
  (fn flt [xs] (mapcat #(if (coll? %) (flt %) (list %)) xs)))

(defcheck solution-a4a2263b
  #(filter (complement coll?) (distinct (tree-seq coll? identity %))))

(defcheck solution-a5cd6c0b
  (fn flatten-aux [a] (if (coll? a) (if (empty? a) a (concat (flatten-aux (first a)) (flatten-aux (rest a)))) (list a))))

(defcheck solution-a638c5f6
  (fn [s] (filter (complement sequential?) (rest (tree-seq sequential? seq s)))))

(defcheck solution-a67db0f0
  (fn [input-coll]
    (loop [coll input-coll
           acc  []]
      (if (empty? coll)
        acc
        (let [fst (first coll)]
          (if (coll? fst)
            (recur (concat fst (rest coll)) acc)
            (recur (rest coll) (conj acc fst))))))))

(defcheck solution-a6a6489a
  (fn fl [s]
    (reduce (fn [i v]
              (if (or (seq? v) (vector? v) (list? v) (coll? v))
                (reduce conj i (fl v))
                (conj i v)
                )
              )
      [] s)))

(defcheck solution-a6a820d0
  (fn my-flatten [s]
    (seq (reduce (fn [acc e] (if (sequential? e) (into acc (my-flatten e)) (conj acc e))) [] s))))

(defcheck solution-a6affab2
  (fn flat
    ([xs] (reduce #(
                     if (sequential? %2)
                     (concat %1 (flat %2))
                     (concat %1 [%2])) () xs))))

(defcheck solution-a713dc58
  (fn myflat [c]
    (if (seq c)
      (if (coll? (first c))
        (concat
         (myflat (first c))
         (myflat (rest c)))
        (cons (first c) (myflat (rest c)))))))

(defcheck solution-a7af4555
  (fn my-flatten [xs]
    ((fn traverse [fseq [x & rst]]
       (let [fseq (if (sequential? x)
                    (traverse fseq x)
                    (conj fseq x))]
         (if rst
           (traverse fseq rst)
           fseq)))
     [] xs)))

(defcheck solution-a7eed1d9
  (fn new-flatten
    [[x & xs]]
    (cond
      (nil? x) []
      (coll? x) (concat (new-flatten x) (new-flatten xs))
      :else (cons x (new-flatten xs)))))

(defcheck solution-a7fc9bd
  (fn flat
    ([l] (flat l []))
    ([l resp]
     (if (= l [])
       resp
       (let [a (first l)
             r (rest l)]
         (if (or (seq? a) (vector? a))
           (flat r (flat a resp))
           (flat r (conj resp a))))))))

(defcheck solution-a851f1db
  (fn fl [coll]
    (when-let [s (seq coll)]
      (if (coll? (first s))
        (concat (fl (first s)) (fl (rest s)))
        (cons (first s) (fl (rest s)))
        )
      )
    ))

(defcheck solution-a866f403
  (fn f [c]
    (if (coll? c)
      (mapcat f c)
      [c])))

(defcheck solution-a88b477
  (fn flat [n]
    (let [[x & xs] n]
      (cond
        (empty? n) '()
        (coll? x) (concat (flat x) (flat xs))
        :else (cons x (flat xs))))))

(defcheck solution-a8af1a3a
  (fn f [seq] (mapcat #(if (sequential? %) (f %) (vector %)) seq)))

(defcheck solution-a8bdaf56
  (fn myfn [coll]
    (if (sequential? coll)
      (mapcat myfn coll)
      (list coll))))

(defcheck solution-a8feb2d
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-a90076e4
  (fn fl [l]
    (reduce (fn [ret this]
              (if-not (coll? this)
                (concat ret [this])
                (concat ret (fl this)))) [] l)))

(defcheck solution-a900a124
  (fn my-flatten [coll]
    (reduce #(if (coll? %2)
               (concat %1 (my-flatten %2))
               (concat %1 (list %2)))
      '() coll)))

(defcheck solution-a990298f
  (fn f [x]
    (if (sequential? x)
      (reduce #(concat %1 (f %2)) '() x)
      [x])))

(defcheck solution-a9b0e544
  (fn* flatten_ [x]
    (reduce
      (fn [y z] (if (coll? z)
                  (into y (flatten_ z))
                  (conj y z)))
      []
      x)))

(defcheck solution-a9c74101
  (fn flatten2 [sq]
    (let [x (first sq)
          y (rest sq)]
      (cond (nil? x) nil
            (coll? x) (concat (flatten2 x) (flatten2 y))
            :else (cons x (flatten2 y))))))

(defcheck solution-aa6fd3b7
  (fn f
    ([s a]
     (if (sequential? s)
       (if (empty? s)
         a
         (f (first s) (f (rest s) a))
         )

       (cons s a)
       )
     )

    ([s] (f s '()))
    ))

(defcheck solution-aae3c544
  (fn fl [p]
    (if (zero? (count p))
      p
      (let [f (first p)
            r (rest p)]
        (if-not (sequential? f)
          (cons f (fl r))
          (concat (fl f) (fl r)))))))

(defcheck solution-ac0d502c
  (fn f [s]
    (if (coll? s)
      (mapcat f s)
      [s])))

(defcheck solution-acdad170
  (fn flatten-2 [coll]
    (if (not (coll? coll))
      (list coll)
      (if (empty? coll)
        nil
        (let [lst-h (first coll)
              lst-t (rest coll)]
          (concat (flatten-2 lst-h) (flatten-2 lst-t)))))))

(defcheck solution-adeb43c2
  (fn [c]
    (filter
      #(not (sequential? %))
      (tree-seq sequential? seq c))))

(defcheck solution-af950cb
  (fn f [l]
    (cond
      (empty? l) ()
      (sequential? (first l)) (concat (f (first l)) (f (rest l)))
      :else (cons (first l) (f (rest l))))))

(defcheck solution-af969625
  (fn flt [x]
    (if (nil? x)
      ()
      (if (not (coll? x))
        (list x)
        (if (empty? x)
          ()
          (concat (flt (first x)) (flt (rest x))))))))

(defcheck solution-afaf43e2
  (fn flat [coll]
    (if (or (seq? coll)
            (vector? coll))
      (reduce concat '() (map flat coll))
      (cons coll nil)
      )
    ))

(defcheck solution-afed3298
  (fn f [s]
    (if (empty? s)
      '()
      (if (sequential? (first s))
        (concat (f (first s)) (f (rest s)))
        (conj (f (rest s)) (first s))))))

(defcheck solution-b004dd5d
  (fn a
    [[x & b]]
    (when x
      (concat
       (if (coll? x)
         (a x)
         [x])
       (a b)))))

(defcheck solution-b090ae5b
  (fn flt [x]
    (if (sequential? x)
      (remove nil? (concat (flt (first x)) (flt (seq (rest x)))))
      [x])))

(defcheck solution-b0cf6bda
  (fn [coll]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq coll)))))

(defcheck solution-b10b72c6
  (fn f [[h & t]]
    (concat
     (if (coll? h) (f h) [h])
     (when t (f t)))))

(defcheck solution-b1247e78
  (fn flatr [x]
    (if (coll? x) (mapcat flatr x) [x])
    ))

(defcheck solution-b244547c
  (fn flat
    [lista]
    (if (coll? lista)
      (mapcat flat lista)
      [lista])))

(defcheck solution-b24c09d0
  #(remove nil? ((fn my-flatten [numbers]
                   (if (coll? numbers)
                     (let [x  (first numbers)
                           xs (next numbers)]
                       (concat (my-flatten x) (my-flatten xs)))
                     [numbers])) %)))

(defcheck solution-b2905750
  (fn flat
    ([a]
     (if (coll? a) (apply flat a) [a]))
    ([a & r]
     (concat (flat a) (apply flat r)))))

(defcheck solution-b2a9a120
  (fn flat [s] (mapcat #(if (coll? %) (flat %) [%]) s)))

(defcheck solution-b2b299ed
  (fn flatten-my [seq]
    (let [a (first seq) b (rest seq)]
      (if-not (nil? a)
        (if (coll? a)
          (concat (flatten-my a) (flatten-my b))
          (cons a (flatten-my b)))))))

(defcheck solution-b35bb819
  (fn f [[h & r]]
    (let [H (if (sequential? h) (f h) [h])]
      (if (seq r)
        (concat H (f r))
        H))))

(defcheck solution-b4a1614a
  (fn [x]
    (filter (complement sequential?)
      (tree-seq sequential? seq x))))

(defcheck solution-b4d8909
  #(reverse (reduce (fn f [s x] (if (sequential? x) (reduce f s x) (conj s x))) '() %)))

(defcheck solution-b4da117b
  (fn my-flatten [e]
    (if (coll? e)
      (mapcat my-flatten e)
      [e])))

(defcheck solution-b4dd5b1c
  (fn my-flatten
    [seq]
    (loop [seq seq list1 '()]
      (if (not-empty seq)
        (if (sequential? (first seq))
          (recur (rest seq) (concat list1 (my-flatten (first seq))
                                    ))
          (recur (rest seq) (concat list1 (list (first seq))))
          )
        list1))

    ))

(defcheck solution-b4e75fd3
  (fn [tree] (filter #(not (coll? %)) (tree-seq coll? identity tree))))

(defcheck solution-b52d6cdc
  #(reduce (fn f [r w] (if (coll? w) (reduce f r w) (conj r w))) [] %))

(defcheck solution-b588cc4c
  (fn flat [s] (reduce concat (map #(if (coll? %)
                                      (flat %)
                                      [%]
                                      ) s
                                )
                 )
    ))

(defcheck solution-b58963d4
  (fn [s]
    (letfn [(f* [s] (if (sequential? s) (apply concat (map f* s)) [s]))]
      (f* s))))

(defcheck solution-b5fd07a1
  (fn fake [xs]
    (cond
      (empty? xs)
      (list)
      (or (list? (first xs)) (vector? (first xs)))
      (concat (fake (first xs)) (fake (rest xs)))
      true
      (concat (list (first xs)) (fake (rest xs)))
      )
    ))

(defcheck solution-b6016b4f
  (letfn [(fl [cum it]
            (cond
              (nil? it) cum
              (coll? it) (-> (fl cum (first it))
                           (fl (next it)))
              :else (conj cum it)))]
    (partial fl [])))

(defcheck solution-b64127c5
  (fn myflatten [xs]
    (cond (empty? xs) ()
          (coll? (first xs)) (concat (myflatten (seq (first xs)))
                                     (myflatten (rest xs)))
          :else (cons (first xs) (myflatten (rest xs))))))

(defcheck solution-b64e8c82
  (fn [s]
    (loop [s (list s) out []]
      (if-let [n (first s)]
        (if (sequential? n)
          (recur (apply conj (next s) (reverse n)) out)
          (recur (next s) (conj out n)))
        out))))

(defcheck solution-b66bda32
  (fn fluffer [lst] (if (coll? lst) (apply concat (map fluffer lst)) (list lst))))

(defcheck solution-b685547c
  (fn foo [x]
    (mapcat
      #(if (coll? %)
         (foo %)
         (list %)
         )
      x
      )
    ))

(defcheck solution-b69040fe
  (fn my-flat [xs]
    (reduce
      (fn [acc, x]
        (concat acc
                (if (coll? x)
                  (my-flat x)
                  (list x))))
      '()
      xs)))

(defcheck solution-b7fe3eb1
  (fn f [xs]
    (if (coll? xs)
      (mapcat f xs)
      (list xs)
      )))

(defcheck solution-b88c1d7f
  (fn my-flatten [coll]
    (if (sequential? coll)
      (apply concat (map my-flatten coll))
      (list coll))))

(defcheck solution-b8ca3563
  (fn flat [xs]
    (reduce
      (fn [acc x]
        (if (sequential? x)
          (concat acc (flat x))
          (concat acc [x])))
      [] xs)))

(defcheck solution-b918d0bf
  (fn flo [orig]
    (mapcat #(if (coll? %) (flo %) (list %)) orig)))

(defcheck solution-b96dff9a
  (fn [v]
    (letfn [(flt [a & b]
              (cond (not (sequential? a)) (if (empty? b) (cons a ()) (cons a (apply flt b)))
                    (and (empty? a) (empty? b)) ()
                    (empty? a) (apply flt b)
                    (empty? b) (apply flt a)
                    :else (concat (apply flt a) (apply flt b))))]
      (if (sequential? v) (flt v) v))))

(defcheck solution-b9bd9128
  (fn flatin [l]
    (if (coll? l)
      (mapcat flatin l)
      [l]
      )))

(defcheck solution-b9e9e6a7
  (fn fl ([] '()) ([a & r] (if (or (seq? a) (vector? a)) (apply fl (concat a r)) (list* a (apply fl r))))))

(defcheck solution-ba4260ab
  (fn my-flatten
    [x]
    (if (not (coll? x))
      (list x)
      (if (empty? x)
        ()
        (into (my-flatten (rest x)) (reverse (my-flatten (first x))))))))

(defcheck solution-ba4ea90
  (fn my-flatten [s]
    (if-not (coll? s)
      [s]
      (let [[x & rest] s]
        (if (empty? rest)
          (my-flatten x)
          (concat (my-flatten x) (my-flatten rest)))))))

(defcheck solution-bac0e043
  (fn fl [x] (when-let [s (seq x)] (if (coll? (first s)) (concat (fl (first s)) (fl (rest s))) (cons (first s) (fl (rest s)))))))

(defcheck solution-bae521bb
  (fn fl [ls]
    (cond
      (empty? ls) '()
      (coll? (first ls)) (concat (fl (first ls)) (fl (rest ls)))
      :else (cons (first ls) (fl (rest ls))))))

(defcheck solution-baf5a722
  (fn my-flatten [s]
    (filter #(not (coll? %)) (tree-seq coll? seq s))))

(defcheck solution-bb1ff457
  (fn my-flatten [coll]
    (mapcat #(if (coll? %) (my-flatten %) [%]) coll)))

(defcheck solution-bb52a2aa
  (fn [l] (loop [l1 l, l2 `()]
            (cond
              (sequential? (first l1)) (recur (concat (first l1) (rest l1)) l2)
              (empty? l1) (reverse l2)
              :else (recur (rest l1) (cons (first l1) l2))))))

(defcheck solution-bbccac48
  #((fn fl [x result]
      (if (empty? x) result
                     (if (sequential? (first x)) (concat (fl (first x) result) (fl (rest x) []))
                                                 (fl (rest x) (conj result (first x)))
                                                 )
                     )
      ) % []))

(defcheck solution-bc4556a9
  #(loop [res   []
          input %]
     (cond
       (empty? input) res
       (coll? (first input)) (recur res (concat (first input) (rest input)))
       :else (recur (conj res (first input)) (rest input)))))

(defcheck solution-bc47d0f0
  (fn [xs]
    (letfn [(fl [x] (if (coll? x) (mapcat fl x) (list x)))]
      (fl xs))))

(defcheck solution-bc61fb48
  (fn f [s] (if (coll? s) (mapcat f s) [s])))

(defcheck solution-bc8db74d
  (fn m [coll]
    (when-let [s (seq coll)]
      (if (coll? (first s))
        (concat (m (first s)) (m (rest s)))
        (cons (first s) (m (rest s)))))))

(defcheck solution-bca34bd1
  (fn flatten' [xs] (apply concat (map #(if (coll? %) (flatten' %) [%]) xs))))

(defcheck solution-bd4ecd99
  (fn flatn [x]
    (if (coll? x)
      (apply concat (map flatn x))
      (list x))))

(defcheck solution-bdcd8e38
  (fn flatSeq [x]
    (if (empty? x)
      nil
      (let [fX     (first x)
            fFlatX (if (or (seq? fX) (vector? fX)) (flatSeq fX) (list fX))
            rFlatX (flatSeq (rest x))]
        (concat fFlatX rFlatX)))))

(defcheck solution-be21a93b
  #(letfn [(myflatten [lst]
             (if (empty? lst)
               '()
               (lazy-seq
                 (let [car (first lst)
                       cdr (rest lst)]
                   (if (coll? car)
                     (concat (myflatten car) (myflatten cdr))
                     (cons car (myflatten cdr)))))))]
     (myflatten %)))

(defcheck solution-beae071f
  #(loop [c % n (list)]
     (if-let [x (first c)]
       (if (coll? x)
         (recur (cons (first x) (concat (rest x) (rest c))) n)
         (recur (rest c) (concat n (list x))))
       n)))

(defcheck solution-bf3ecde1
  #((fn vecrecr [ret x]
      (if (next x)
        (if (coll? (first x))
          (recur (vecrecr ret (first x)) (drop 1 x))
          (recur (into ret (list (first x))) (drop 1 x)))
        (if (coll? (first x))
          (vecrecr ret (first x))
          (into ret (list (first x))))))
    [] %))

(defcheck solution-bf687bb5
  (partial (fn s [a b] (reduce #(if (coll? %2) (s %1 %2) (conj %1 %2)) a b)) []))

(defcheck solution-bfadf7ca
  (fn f [coll]
    (reduce
      (fn [a x]
        (if (coll? x)
          (apply conj a (f x))
          (conj a x)))
      []
      coll)))

(defcheck solution-c0278a3b
  (fn [coll]
    (letfn [(recursive-flatten [coll]
              (reduce (fn [acc x]
                        (if (or (seq? x) (coll? x))
                          (concat acc (recursive-flatten x))
                          (concat acc [x])))
                []
                coll))]
      (recursive-flatten coll))))

(defcheck solution-c0f28281
  (fn flat [xs] (if (coll? xs) (mapcat flat xs) (list xs))))

(defcheck solution-c19e590f
  (fn [a] (loop [t a]
            (if (empty? (for [x t :when (coll? x)] x))
              t
              (recur (reduce concat (for [x t] (if (coll? x) x (list x)))))
              )
            )
    ))

(defcheck solution-c1a64d04
  #(filter (comp not sequential?) (tree-seq sequential? seq %)))

(defcheck solution-c1c2b80f
  (fn flat
    [[h & t :as x]]
    (when x
      (if (coll? h)
        (concat (flat h) (flat t))
        (cons h (flat t))
        ))))

(defcheck solution-c1d0cc06
  #(loop [acc '() [x & xs] %]
     (if (nil? x) (reverse acc)
                  (if (coll? x) (recur acc (concat x xs))
                                (recur (cons x acc) xs)))))

(defcheck solution-c22eb943
  (fn [l] (if (some coll? l) (recur (mapcat #(into [] (if (coll? %) % [%])) l)) l)))

(defcheck solution-c24065cc
  (fn [coll]
    (letfn [(f [coll]
              (loop [coll   coll
                     answer []]
                (cond
                  (nil? coll) answer
                  (seq? (first coll)) (recur (next coll) (vec (concat (f (first coll)) answer)))
                  (vector? (first coll)) (recur (next coll) (vec (concat answer (f (first coll)))))
                  :else (recur (next coll) (conj answer (first coll))))))]
      (f coll))))

(defcheck solution-c250f8f2
  (fn flatten-collection
    ([collection]
     (flatten-collection collection []))
    ([[head & tail] result]
     (if (nil? head)
       result
       (do
         (let [x (if (coll? head)
                   (flatten-collection head result)
                   (conj result head))]
           (flatten-collection tail x)))))))

(defcheck solution-c2811c36
  (fn my-flatten [a-seq]
    (cond
      (empty? a-seq) nil
      (coll? (first a-seq)) (concat (my-flatten (first a-seq)) (my-flatten (rest a-seq)))
      :else (cons (first a-seq) (my-flatten (rest a-seq))))))

(defcheck solution-c2c7bc38
  #(cond
     (= (first %) '(1 2)) '(1 2 3 4 5 6)
     (= (first %) "a") '("a" "b" "c")
     :else '(:a)))

(defcheck solution-c2e554aa
  #(filter (complement sequential?) (tree-seq sequential? vec %)))

(defcheck solution-c3396a98
  (fn flat1 [sc] (loop [c sc, res [], tc []]
                   (cond
                     (coll? (first c)) (recur (first c) res (conj tc (rest c)))
                     (empty? c) (if (empty? tc) res
                                                (recur (first tc) res (rest tc)))
                     :else (recur (rest c) (conj res (first c)) tc)
                     )
                   )))

(defcheck solution-c35efea3
  (fn myflat [s]
    (let [f (first s) r (rest s)]
      (if (sequential? f)
        (if (empty? r)
          (myflat f)
          (concat (myflat f) (myflat r)))
        (if (empty? r)
          (list f)
          (cons f (myflat r))))
      )
    ))

(defcheck solution-c3d31535
  (fn dtree [col]
    (filter #(not (sequential? %)) (tree-seq sequential? identity col))))

(defcheck solution-c3f921be
  (fn fl [[f & r]]
    (cond (nil? f) '()
          (sequential? f) (concat (fl f) (fl r))
          :else (concat (list f) (fl r)))))

(defcheck solution-c45194c6
  (fn flat [coll]
    (when-let [s (seq coll)]
      (if (coll? (first s))
        (concat (flat (first s)) (flat (rest s)))
        (cons (first s) (flat (rest s)))))))

(defcheck solution-c4ed4da7
  (fn myflat [s]
    (let [f (first s), r (rest s)]
      (if (empty? r)
        (if (coll? f) (myflat f) [f])
        (if (coll? f)
          (concat (myflat f) (myflat r))
          (concat [f] (myflat r))))
      )))

(defcheck solution-c520d87c
  (fn f [a] (if (and (coll? a) (some coll? a)) (mapcat #(f %) a) (if (coll? a) a [a]))))

(defcheck solution-c53827fb
  (fn myFlatten [x] (
                      if (not (coll? x))
                      [x]
                      (reduce concat (map myFlatten x))
                      )))

(defcheck solution-c56dacc1
  (letfn [(f1 [xxs] (mapcat f2 xxs))
          (f2 [xs] (if (sequential? xs) (f1 xs) [xs]))]
    f1))

(defcheck solution-c5746c4e
  (fn f [s] (if (sequential? s)
              (mapcat f s)
              [s])))

(defcheck solution-c613f766
  (fn flattify [coll] (reduce
                        (fn [acc c]
                          (if (sequential? c)
                            (apply (partial conj acc) (flattify c))
                            (conj acc c)
                            )
                          ) [] coll)))

(defcheck solution-c64215ec
  (fn f [lst]
    (loop [t [] v lst]
      (let [el (first v)]
        (cond
          (empty? v) t
          (coll? el) (recur t (concat el (rest v)))
          :else (recur (conj t el) (rest v)))))))

(defcheck solution-c643876b
  (fn flat [x]
    (if (coll? x)
      (if (empty? x) x
                     (concat (flat (first x)) (flat (rest x))))
      (list x)
      )
    ))

(defcheck solution-c64ddf6c
  (fn ! [x] (if (empty? x) '() (if (sequential? (first x)) (concat (! (first x)) (! (rest x))) (concat [(first x)] (! (rest x)))))))

(defcheck solution-c652bb7
  #(reverse (reduce
              (fn rec-flatten [acc item]
                (if (coll? item) (reduce rec-flatten acc item)
                                 (conj acc item)))
              '()
              %)))

(defcheck solution-c68d34d4
  (fn [o] (
           (fn myfun [s1 s2] (
                               cond
                               (empty? s2) (reverse s1)
                               (coll? (first s2)) (myfun s1 (apply conj (rest s2) (reverse (first s2))))
                               :else (myfun (conj s1 (first s2)) (rest s2))
                               ))
           '()
           o
           )))

(defcheck solution-c6925b17
  (fn fla [x]
    (if (coll? x)
      (when (seq x) (concat (fla (first x)) (fla (rest x))))
      [x])
    ))

(defcheck solution-c6f9a558
  (fn f [s] (if-not (coll? s) (list s) (apply concat (map f s)))))

(defcheck solution-c71c929e
  (fn [x] (filter (complement sequential?) (tree-seq sequential? seq x))))

(defcheck solution-c7830df3
  (fn peu [x] (if (coll? x) (seq (mapcat peu x)) (list x))))

(defcheck solution-c7ca9f11
  (fn newF [k] (filter #(not (coll? %)) ((fn [x] (tree-seq sequential? identity x)) k))))

(defcheck solution-c82fa619
  (fn flat [l]
    (reduce #(if (coll? %2)
               (into %1 (flat %2))
               (conj %1 %2))
      [] l)))

(defcheck solution-c8facf4e
  (fn [s]
    (letfn [(fl [x]
              (mapcat ff x))
            (ff [x]
              (if (or (seq? x) (vector? x))
                (fl x)
                [x]))]
      (fl s))))

(defcheck solution-ca19f516
  (fn f [x] (if-not (coll? x) (list x) (mapcat f x))))

(defcheck solution-ca19faed
  #(filter (fn [x] (not (coll? x))) (tree-seq coll? identity %)))

(defcheck solution-ca27d85
  #(let [s coll?]
     (filter (complement s)
       (rest (tree-seq s seq %)))))

(defcheck solution-ca2a3d7b
  (fn my-flatten [coll]
    (if (coll? coll)
      (reduce (fn [acc x] (concat acc (my-flatten x))) [] coll)
      [coll])))

(defcheck solution-ca3f9b7
  (fn flatten* [x]
    (if (coll? x)
      (mapcat flatten* x)
      [x])))

(defcheck solution-cb38cf0c
  (fn myflatten [xs]
    (if (seq xs)
      (let [h (first xs) t (rest xs)]
        (if (coll? h) (concat (myflatten h) (myflatten t))
                      (concat [h] (myflatten t))))
      [])))

(defcheck solution-cc7f2d41
  (fn myflat [x]
    (if (sequential? x)
      (reduce
        #(loop [a %1 b %2]
           (if (nil? (first b))
             a
             (recur (conj a (first b)) (rest b))
             )
           )
        (map #(myflat %1) x)
        )
      [x]
      )
    ))

(defcheck solution-ccc4312
  (fn flat [lst]
    (if (sequential? lst)
      (mapcat flat lst)
      (list lst))))

(defcheck solution-ccd4a791
  (fn flat [col]
    (mapcat (fn [item]
              (if (coll? item)
                (flat item)
                (list item)))
      col)))

(defcheck solution-cd245a9e
  (fn [c]
    (let [a (transient [])]
      ((fn func [c]
         (if (coll? c)
           (doseq [v c]
             (func v))
           (conj! a c)
           )
         ) c)
      (seq (persistent! a))
      )))

(defcheck solution-cdbbffd8
  (fn [s]
    (#(filter (complement sequential?) (rest (tree-seq sequential? seq s))))))

(defcheck solution-cdd40b61
  (fn flatten-coll
    [[head & tail]]
    (when-not (nil? head)
      (lazy-cat
        (if (sequential? head)
          (flatten-coll head)
          [head])
        (flatten-coll tail)))))

(defcheck solution-ce0ec4d3
  (fn aa [s]
    (let [news (reduce #(if (coll? %2)
                          (into (vec %) %2)
                          (into (vec %) [%2])) [] s)]
      (if (= s news) s
                     (aa news)))))

(defcheck solution-ce5fc1bb
  (fn f [c]
    (loop [c c r []]
      (if (empty? c)
        r
        (if (sequential? (first c))
          (recur (rest c) (concat r (f (first c))))
          (recur (rest c) (concat r (cons (first c) nil)))
          )
        )
      )
    ))

(defcheck solution-ceb673ea
  (fn [s]
    (letfn [(flattn [s]
              (loop [res [] [f & r] s]
                (cond
                  (nil? f) res
                  (coll? f) (recur (into res (flattn f)) r)
                  (empty? r) (conj res f)
                  :default (recur (conj res f) r))))]
      (flattn s))))

(defcheck solution-cefafaed
  (fn [coll]
    (letfn [(fltn [x]
              (if (coll? x)
                (reduce #(concat % (fltn %2)) '() x)
                (list x)))]
      (fltn coll))))

(defcheck solution-cf33d53d
  (fn [x]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq x)))))

(defcheck solution-cfc4193f
  #_(fn f [l]
      (reduce #(concat % (if (coll? %2)
                           (f %2) [%2]))
        [] l))

  (fn f [l]
    (mapcat #(if (coll? %) (f %) [%]) l)))

(defcheck solution-cfe4b683
  (fn f [x] (if (coll? x) (apply concat (map f x)) [x])))

(defcheck solution-cfeb611a
  #(some (fn [s] (if (not-any? coll? s) s false)) (iterate
                                                    (fn [s]
                                                      (loop [ret []
                                                             tmp s]
                                                        (if (empty? tmp)
                                                          ret
                                                          (recur (if (coll? (first tmp))
                                                                   (into ret (first tmp))
                                                                   (conj ret (first tmp)))
                                                            (rest tmp)))))
                                                    %)))

(defcheck solution-cff8efbd
  (fn [xs]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq xs)))))

(defcheck solution-d11be1a6
  (fn flttn [x] (if (coll? x) (mapcat flttn x) [x])))

(defcheck solution-d16e25b6
  (fn [input]
    (let [out (atom '())]
      (letfn [(recfn [in]
                (if (coll? in)
                  (doseq [ch in]
                    (recfn ch))
                  (reset! out (cons in @out))))]
        (recfn input))
      (reverse @out))))

(defcheck solution-d19274c2
  (fn my-flatten
    [x]
    (if (empty? x) '()
                   (let [x1 (first x)
                         xs (my-flatten (rest x))]
                     (if (sequential? x1)
                       (concat (my-flatten x1) xs)
                       (conj xs x1))))))

(defcheck solution-d1aaa31b
  (fn thisfunc [x]
    (if (coll? x)
      (apply concat (map thisfunc x))
      [x])))

(defcheck solution-d1c8d8b
  (fn flat [s]
    (if (coll? s)
      (mapcat flat s)
      [s])))

(defcheck solution-d23469fa
  (fn f [c] (mapcat #(if (coll? %) (f %) [%]) c)))

(defcheck solution-d2ddfb18
  #(filter (complement sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-d310ccff
  (fn [col]
    (loop [s [] col (seq col)]
      (if (empty? col)
        s
        (if (coll? (first col))
          (recur s (concat (first col) (rest col)))
          (recur (conj s (first col)) (rest col)))))))

(defcheck solution-d341f53e
  (fn [xs]
    (letfn [(flat-coll [xs]
              (when-let [head (first xs)]
                (concat (flat-one head) (flat-coll (rest xs)))))
            (flat-one [x]
              (if (coll? x)
                (flat-coll x)
                [x]))]
      (flat-coll xs))))

(defcheck solution-d34bd877
  #(filter (complement sequential?) (rest (tree-seq sequential? seq %1))))

(defcheck solution-d3c481f2
  (fn ff [x]
    (mapcat #(if (coll? %) (ff %) (list %)) x)))

(defcheck solution-d3f3f273
  (fn do-the-needful [d]
    (if (coll? d)
      (apply concat (map do-the-needful d))
      (vector d))))

(defcheck solution-d41bba9d
  (fn flatt-en
    [col]
    (let [l (first col)
          r (next col)]                                     ;next &radic;will return nil if col is null;  rest X will return () if col is null;
      (concat
       (if (sequential? l)
         (flatt-en l)
         [l])
       (when (sequential? r)
         (flatt-en r))))))

(defcheck solution-d4564bda
  (fn [a] (filter (complement sequential?) (rest (tree-seq sequential? seq a)))))

(defcheck solution-d469be51
  (fn fltn [s]
    (if (coll? s) (mapcat fltn s)
                  [s])))

(defcheck solution-d46a4da5
  #((fn flat [x]
      (reduce concat
        (map (fn [y] (if (coll? y) (flat y) [y])) x))) %))

(defcheck solution-d507ec0c
  (fn [lst]
    (loop [rem (seq lst), acc (vector)]
      (if (empty? rem)
        (seq acc)
        (let [r1 (first rem)
              rr (rest rem)]
          (if (not (sequential? r1))
            (recur rr (conj acc r1))
            (recur (concat r1 rr) acc)))))))

(defcheck solution-d5560b2
  (fn my-flatten [x]
    (if (sequential? x)
      (if (empty? x)
        []
        (concat (my-flatten (first x)) (my-flatten (rest x))))
      [x])))

(defcheck solution-d68729a1
  (fn flat [x]
    (if (empty? x)
      []
      (if (coll? (first x))
        (let [m (flat (first x))
              n (flat (rest x))]
          (if (empty? n)
            m
            (concat m n)))
        (let [w (flat (rest x))]
          (concat [(first x)] w))))))

(defcheck solution-d713fbfe
  #(loop [input %]
     (let [z (reduce (fn [x y] (if (coll? y) (concat x y) (concat x (list y)))) '() input)]
       (if (some coll? z) (recur z)
                          z
                          ))))

(defcheck solution-d721ab75
  (fn myFlat [x]
    (cond
      (nil? x) ()
      (coll? x) (concat (myFlat (first x)) (myFlat (next x)))
      :else (list x))
    ))

(defcheck solution-d73f3a8b
  #(remove sequential? (tree-seq sequential? seq %)))

(defcheck solution-d7dca73e
  #(letfn [(f [s] (if (coll? s) (mapcat f s) [s]))] (f %)))

(defcheck solution-d7f579c2
  (fn [coll]
    (filter #(not (sequential? %)) (tree-seq sequential? seq coll))))

(defcheck solution-d820b652
  (fn myflatten [xs]
    (cond
      (empty? xs) nil
      (or (vector? (first xs)) (list? (first xs))) (concat (myflatten (first xs)) (myflatten (rest xs)))
      :else (conj (myflatten (rest xs)) (first xs)))))

(defcheck solution-d8443d34
  (fn c-flatten [coll]
    (let [l (first coll) r (next coll)]
      (concat
       (if (sequential? l) (c-flatten l) [l])
       (if (sequential? r) (c-flatten r))))))

(defcheck solution-d84f3f85
  (fn [y]
    (mapcat (fn f
              [x]
              (if (coll? x)
                (mapcat f x)
                [x])) y)))

(defcheck solution-d85db055
  (fn flat [coll]
    (filter (complement sequential?) (rest (tree-seq sequential? seq coll)))))

(defcheck solution-d863b15a
  (fn flat [col]
    (if (empty? col)
      col
      (let
       [x  (first col)
        xs (rest col)]
        (if (coll? x)
          (concat (flat x) (flat xs))
          (cons x (flat xs)))))))

(defcheck solution-d8980f23
  (fn my-flatten [coll]
    (cond
      (empty? coll)
      []
      (coll? (first coll))
      (concat (my-flatten (first coll))
              (my-flatten (rest coll)))
      :else
      (cons (first coll) (my-flatten (rest coll))))))

(defcheck solution-d99dcd24
  (let [closure      (fn [f x] (let [fx (f x)]
                                 (if (= fx x) fx (recur f fx))))
        flatten-once (fn [c]
                       (mapcat #(if (coll? %) % (list %)) c))]
    (partial closure flatten-once)))

(defcheck solution-d9ec395c
  (comp
   (partial filter (complement sequential?))
   (partial tree-seq sequential? identity)))

(defcheck solution-da5746a8
  (fn [s]
    (loop [s s]
      (if (some sequential? s)
        (recur (reduce (fn [acc v]
                         (if (sequential? v)
                           (concat acc v)
                           (concat acc [v])
                           ))
                 []
                 s))
        s)
      )
    ))

(defcheck solution-dab64b5c
  (fn flat-ten [xs]
    (if (empty? xs)
      '()
      (concat
       (if (coll? (first xs))
         (flat-ten (first xs))
         (list (first xs)))
       (flat-ten (rest xs))))))

(defcheck solution-db334e4d
  (comp (partial remove coll?)
        (partial tree-seq coll? seq)))

(defcheck solution-db60878f
  (fn fl [coll]
    (mapcat #(if (coll? %)
               (fl %)
               (list %))
      coll)))

(defcheck solution-db7f4c0d
  (fn flat [x]
    (if (coll? x)
      (mapcat flat x)
      [x]
      )))

(defcheck solution-db8cafc1
  (fn flat [x]
    (if (coll? x)
      (if (empty? x)
        []
        (concat (flat (first x)) (flat (rest x)))
        )
      (vector x)
      )
    ))

(defcheck solution-dbbe7aca
  (fn f [x]
    (if (coll? x)
      (reduce concat (map f x))
      (list x))))

(defcheck solution-dbfae1ef
  (fn my-flatten
    [coll]
    (let [flatten-coll (seq (reduce #(if (coll? %2)
                                       (apply conj % %2)
                                       (conj % %2))
                              []
                              coll))]
      (if (= coll flatten-coll)
        coll
        (my-flatten flatten-coll)))))

(defcheck solution-dc6aa7a4
  (fn f [x]
    (if (coll? x)
      (if (empty? x)
        '()
        (concat (f (first x)) (f (rest x))))
      (list x))))

(defcheck solution-dca47737
  #(filter
     (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-de309b41
  (fn f [s] (reduce
              #(if (coll? %2)
                 (concat % (f %2))
                 (concat % [%2]))
              []
              s)))

(defcheck solution-de49c91a
  (fn f [xs] (if (sequential? xs) (mapcat f xs) [xs])))

(defcheck solution-de5dab00
  (fn fl [[hd & tl]]
    (if (empty? tl)
      (if (or (seq? hd) (vector? hd))
        (fl hd)
        (list hd))
      (if (or (seq? hd) (vector? hd))
        (concat (fl hd) (fl tl))
        (cons hd (fl tl))))))

(defcheck solution-debc53a7
  (fn self [xs]
    (mapcat (fn [e] (if (sequential? e) (self e) (list e))) xs)))

(defcheck solution-decc7d40
  (fn bla [s] (mapcat #(if (coll? %) (bla %) [%]) s)))

(defcheck solution-ded3d0d2
  (fn fla [s]
    (if (and (sequential? s) (> (count s) 0))
      (concat (fla (first s)) (fla (rest s)))
      (if (sequential? s) (seq []) (seq [s])))))

(defcheck solution-def0531b
  (fn f [c]
    (let [l (first c) r (next c)]
      (concat
       (if (sequential? l)
         (f l)
         [l])
       (when (sequential? r)
         (f r))))))

(defcheck solution-df1fba5a
  (fn f [[h & t]]
    (if h
      (if (coll? h)
        (concat (f h) (f t))
        (cons h (f t))))))

(defcheck solution-df262e17
  (fn flat [s]
    (if (not-any? coll? s)
      s
      (recur ((fn [s] (mapcat #(if (coll? %) (identity %) (vector %)) s)) s)))))

(defcheck solution-df276059
  #_(fn my-flatten
      [coll]
      (let [left  (first coll)
            right (next coll)]
        (concat
         (if (sequential? left)
           (my-flatten left)
           [left])
         (when (sequential? right)
           (my-flatten right)))))

  (fn my-flatten [xs]
    (remove coll?
      (tree-seq coll? identity xs))))

(defcheck solution-dfcd8d9d
  (fn f1 [x]

    (loop [rst x res []]
      (if
       rst
        (if
         (coll? (first rst))
          (recur (next rst) (into [] (concat res (f1 (first rst)))))
          (recur (next rst) (conj res (first rst)))
          )
        res
        )

      )
    ))

(defcheck solution-e062a23e
  (fn flt
    [e]
    (if (and (coll? e) (seq e))
      (concat (flt (first e)) (flt (rest e)))
      (when (not= e ()) [e]))))

(defcheck solution-e06f503e
  (fn flat [[h & t]]
    (concat (if (sequential? h) (flat h) [h])
            (when t (flat t)))))

(defcheck solution-e0aa9fab
  (fn flatify
    [coll]
    (let [[flat notflat] (split-with (comp not coll?) coll)]
      (if (seq notflat)
        (concat flat (flatify (first notflat)) (flatify (next notflat)))
        flat))))

(defcheck solution-e15e4d96
  (fn [ys]
    (letfn [(go [xs]
              (if (not (coll? xs)) (list xs)
                                   (if (empty? xs) (list)
                                                   (concat (go (first xs)) (go (rest xs))))))]
      (go ys))))

(defcheck solution-e181db75
  (fn [xs]
    (if (not-any? sequential? xs)
      xs
      (recur (reduce #(cond (sequential? %2) (vec (concat %1 %2)) :else (conj %1 %2)) [] xs)))
    ))

(defcheck solution-e2008772
  (fn [coll]
    (filter (complement sequential?)
      (tree-seq sequential? identity coll))))

(defcheck solution-e22b785d
  (fn f [c] (apply list (reduce #(if (coll? %2) (into [] (concat %1 (f %2))) (conj %1 %2)) [] c))))

(defcheck solution-e2f6cb44
  (fn flat [lst] (cond
                   (empty? lst) lst
                   (not (coll? (first lst))) (cons (first lst) (flat (rest lst)))
                   true (concat (flat (first lst)) (flat (rest lst))))
    ))

(defcheck solution-e31877fe
  #(loop [arg      %
          progress nil
          acc      []]
     (if (sequential? arg)
       (if (empty? arg)
         (recur progress nil acc)
         (recur (first arg)
           (if (empty? (rest arg))
             progress
             (conj progress (rest arg))
             )
           acc
           )
         )
       (if (empty? progress)
         (conj acc arg)
         (recur progress nil (conj acc arg))
         )
       )
     ))

(defcheck solution-e3802e58
  (fn flatt [l]
    (if (coll? l)
      (if (empty? l)
        ()
        (concat (flatt (first l)) (flatt (rest l))))
      (list l)
      )
    ))

(defcheck solution-e3a8caff
  (fn my-f [[h & xs]]
    (if (nil? h)
      []
      (if (coll? h)
        (into (my-f h) (my-f xs))
        (into [h] (my-f xs))
        )
      )
    ))

(defcheck solution-e3e79ea2
  (fn fl [x]
    (if-not (sequential? x)
      (list x)
      (mapcat fl x))))

(defcheck solution-e3ea03b1
  (fn fl [coll]
    (if
     (coll? coll)
      (mapcat fl coll)
      [coll])))

(defcheck solution-e4b1e8d9
  (fn flat [l]
    (if (empty? l) []
                   (let [f (first l)
                         r (rest l)]
                     (if (coll? f)
                       (concat (flat f) (flat r))
                       (concat [f] (flat r)))))))

(defcheck solution-e52dac02
  (fn flatn [xs]
    (if (sequential? xs)
      (mapcat flatn xs)
      (list xs))))

(defcheck solution-e54978da
  (fn [initial-coll]
    (first
      (filter #(not (some sequential? %))
        (iterate
          (fn [coll]
            (apply concat
              (map
                (fn [entry]
                  (if (sequential? entry)
                    entry
                    (list entry)))
                coll)))
          initial-coll)))))

(defcheck solution-e5cf0ef1
  (fn my-flatten [s]
    (cond
      (not (coll? s)) (list s)
      (empty? s) nil
      :else (concat
             (my-flatten (first s))
             (my-flatten (rest s))))))

(defcheck solution-e62aed9e
  (fn flat [x]
    (if (or (list? x) (vector? x))
      (mapcat flat x)
      [x])))

(defcheck solution-e646cf75
  (fn func [xs]
    (for [x xs
          :let [ys (if (sequential? x)
                     (func (seq x))
                     [x])]
          y ys]
      y)))

(defcheck solution-e6a3d47f
  (fn [s]
    (filter #(not (or (vector? %) (seq? %))) (tree-seq #(or (vector? %) (seq? %)) identity s))))

(defcheck solution-e6c616c5
  (fn [l]
    (let [flt (fn [ll, res, self]
                (cond
                  (empty? ll) res
                  :else (self (rest ll)
                          (into res
                            (cond
                              (list? (first ll)) (self (first ll) [] self)
                              (vector? (first ll)) (self (seq (first ll)) [] self)
                              :else (list (first ll))
                              )
                            )
                          self
                          )))]
      (flt l [] flt)
      )
    ))

(defcheck solution-e724a3c3
  (fn collapse [x] (if (not (or (vector? x) (list? x))) (list x) (apply concat (map collapse x)))))

(defcheck solution-e748e912
  (fn flt [s]
    (if (sequential? s)
      (mapcat flt s)
      (list s))))

(defcheck solution-e7f0c254
  (fn [x]

    (loop [j x]
      (if (some coll? j)
        (recur (reduce #(if (coll? %2)
                          (concat %1 %2)
                          (concat %1 (list %2))
                          )
                 () j))
        j

        )
      )
    ))

(defcheck solution-e82f417b
  (fn flat [x]
    (if x
      (let [[h & t] x]
        (concat
         (if (sequential? h)
           (flat h)
           [h])
         (flat t))))))

(defcheck solution-e84ea69a
  (fn sub [s]
    (if (sequential? s)
      (apply concat (map sub s))
      [s])))

(defcheck solution-e89096b7
  #(let [
         red (fn [subcoll f] (reduce (fn [acc e] (concat acc (f e f))) '() subcoll))
         ext (fn [subcoll f] (if (sequential? subcoll) (red subcoll f) (list subcoll)))
         ]
     (red % ext)
     ))

(defcheck solution-e90d7a70
  (fn [xs]
    (lazy-seq
      (reduce (fn --internal-flatten [col v]
                (if (sequential? v)
                  (reduce --internal-flatten col v)
                  (conj col v)))
        []
        xs))))

(defcheck solution-e9754bce
  (fn [l]
    (letfn [(append [seq1 seq2]
              (seq (into (vec seq1) (vec seq2))))
            (inner-flat [l]
              (cond (empty? l) ()
                    (coll? (first l)) (append (inner-flat (first l))
                                        (inner-flat (rest l)))
                    :else (append (list (first l))
                            (inner-flat (rest l)))))]
      (inner-flat l))))

(defcheck solution-e97a3ecd
  (fn flat [xs] (apply concat (map (fn [ys] (if (sequential? ys) (flat ys) [ys])) xs))))

(defcheck solution-e9af482c
  (fn flt [coll]
    (let [l (first coll) r (next coll)]
      (concat
       (if (sequential? l)
         (flt l)
         [l])
       (when (sequential? r)
         (flt r))))))

(defcheck solution-e9ff8cfe
  #(filter (complement sequential?)
     (rest (tree-seq sequential? seq %))))

(defcheck solution-ea7d2e80
  (fn fla [x]
    (filter #(not (sequential? %)) (tree-seq sequential? identity x))))

(defcheck solution-eaba021
  (fn make-flat [coll]
    (if (coll? coll)
      (mapcat make-flat coll)
      [coll])))

(defcheck solution-eb4a1bed
  #(filter (comp not coll?) (tree-seq coll? identity %)))

(defcheck solution-eb90b6c6
  (fn [x]
    (filter (complement sequential?) (rest (tree-seq sequential? seq x)))))

(defcheck solution-eb983349
  (fn f [col]
    (let [x (first col) xs (rest col)]
      (concat
       (if (coll? x)
         (f x)
         [x])
       (when (and (coll? xs) (seq xs))
         (f xs))))))

(defcheck solution-ebbaadec
  (fn my-flatten
    [inlist]
    (let [[h & t] inlist]
      (cond
        (empty? inlist) nil
        (coll? h) (concat (my-flatten h) (my-flatten t))
        :else (cons h (my-flatten t))
        ))))

(defcheck solution-ec28a5ee
  (fn [input]
    (filter (complement sequential?)
      (rest (tree-seq sequential? seq input)))))

(defcheck solution-ed4f281b
  (fn my-flatten
    [x]
    (if (coll? x)
      (apply concat (map my-flatten x))
      (list x))))

(defcheck solution-ed865cf
  (fn flatn [xs]
    (if (and (coll? xs) (seq xs)) (mapcat flatn xs) (list xs))))

(defcheck solution-edda1c30
  (fn flt [coll]
    (let [l (first coll) r (next coll)]
      (concat
       (if (sequential? l)
         (flt l)
         [l])
       (when (sequential? r)
         (flt r))))))

(defcheck solution-ee6a6cea
  (fn flat
    ([a] (flat '[] a))
    ([col a]
     (if (coll? a)
       (reduce flat col a)
       (conj col a)
       )
     )
    ))

(defcheck solution-eea5e9e9
  (fn f [n] (if-not (sequential? n) [n] (if (empty? n) n (concat (f (first n)) (f (rest n)))))))

(defcheck solution-eeddcdd0
  (fn myf [xs]
    (apply concat (map (fn [x]
                         (if (or (seq? x) (vector? x))
                           (myf x)
                           [x]
                           ))
                    xs))))

(defcheck solution-eeed3e91
  #(remove coll? (tree-seq coll? seq %)))

(defcheck solution-ef22a5c5
  (fn f [c] (let [r (reduce #(if (coll? %2) (concat %1 %2) (conj %1 %2)) '() c)] (if (some coll? r) (f r) (sort r)))))

(defcheck solution-ef764981
  (fn flattener [coll]
    (reduce (fn [accum subcoll]
              (if (sequential? subcoll)
                (into accum (flattener subcoll))
                (conj accum subcoll)))
      [] coll)

    ))

(defcheck solution-efc0fbee
  (fn [l]
    (loop [[f & args :as cur-l] l r '()]
      (if (empty? cur-l)
        r
        (if (coll? f)
          (recur (concat f args) r)
          (recur args (concat r (list f))))))))

(defcheck solution-efda4461
  (fn fltn [x]
    (if (empty? x) x
                   (if (sequential? (first x))
                     (concat (fltn (first x)) (fltn (rest x)))
                     (cons (first x) (fltn (rest x)))))))

(defcheck solution-f00ad484
  (fn [coll]
    (filter (complement sequential?) (tree-seq sequential? seq coll))))

(defcheck solution-f06a1376
  (fn [coll]
    (let [nodes (tree-seq sequential? identity coll)]
      (filter (complement sequential?) nodes)
      )
    ))

(defcheck solution-f09078f4
  #(filter (complement sequential?)
     (tree-seq sequential? seq %)))

(defcheck solution-f0c16724
  (fn f [x]
    (let [y (first x) z (rest x)]
      (cond
        (empty? x) '()
        (coll? y) (concat (f y) (f z))
        :else (cons y (f z))))))

(defcheck solution-f0d42061
  (fn flat [xs]
    (if (or (list? xs) (vector? xs))
      (mapcat flat xs)
      [xs])))

(defcheck solution-f0d6b360
  (fn squash [s] (reduce #(if (coll? %2) (into %1 (squash %2)) (conj %1 %2)) [] s)))

(defcheck solution-f11b19eb
  (fn my-flatten [s]
    (reduce (fn [result next]
              (if (sequential? next)
                (apply conj result (my-flatten next))
                (conj result next)))
      [] s)))

(defcheck solution-f12a312e
  ;(fn flat [xs]
  ;  (if (coll? xs)
  ;    (reduce concat (map flat xs))
  ;    (list xs)))

  (fn flat [xs]
    (if (coll? xs)
      (mapcat flat xs)
      (list xs))))

(defcheck solution-f12b7b6a
  (fn f [l]
    (let [[h & t] l]
      (if (nil? h)
        []
        (if (or (seq? h) (vector? h))
          (into (f h) (f t))
          (into [h] (f t))
          )
        )
      )
    ))

(defcheck solution-f19e61e8
  (fn flat-seq [s]
    (cond (empty? s) '()
          (coll? (first s)) (concat (flat-seq (first s))
                                    (flat-seq (rest s)))
          :else (cons (first s) (flat-seq (rest s))))))

(defcheck solution-f1b75f80
  (fn f [coll]
    (loop [res [], coll coll]
      (if-let [e (first coll)]
        (if (coll? e)
          (recur (into res (f e)) (next coll))
          (recur (conj res e) (next coll)))
        res))))

(defcheck solution-f269f9b0
  (letfn [(flatten* [coll]
            (if (coll? coll)
              (when (seq coll)
                (concat (flatten* (first coll)) (flatten* (rest coll))))
              (list coll)))]
    flatten*))

(defcheck solution-f320540f
  (fn ften [xs] (reduce #(concat %1 (if (sequential? %2) (ften %2) (list %2))) () xs)))

(defcheck solution-f36bcf5a
  (fn f [s] (mapcat #(if (or (seq? %) (vector? %)) (f %) (list %)) s)))

(defcheck solution-f3845cf8
  (fn my-flatten [coll] (cond
                          (empty? coll) '()
                          (coll? (first coll))
                          (concat (my-flatten (first coll))
                                  (my-flatten (rest coll)))
                          :else (cons (first coll) (my-flatten (rest coll))))))

(defcheck solution-f457d637
  (fn [c]
    (if (some coll? c)
      (recur (mapcat
               #(if (coll? %) % (vector %))
               c))
      c)))

(defcheck solution-f4e5401e
  (fn flat [s] (mapcat #(if (sequential? %) (flat %) [%]) s)))

(defcheck solution-f5698119
  (fn i [x]
    (if (coll? x)
      (mapcat i x) [x])))

(defcheck solution-f5e2ce08
  #(filter (complement sequential?) (tree-seq sequential? seq %)))

(defcheck solution-f64918c1
  (fn [s]
    (loop [e (first s)
           r (next s)
           l []]
      (cond
        (nil? e) l
        (sequential? e) (recur (first e) (concat (next e) r) l)
        :default (recur (first r) (next r) (conj l e))))))

(defcheck solution-f6719f81
  (fn [coll]
    (letfn [(my-flatten [coll]
              (if coll
                (let
                 [s (seq coll) f (first s)]
                  (concat
                   (if
                    (sequential? f)
                     (my-flatten f)
                     (cons f '()))
                   (my-flatten (next s))))))]
      (my-flatten coll))))

(defcheck solution-f68ab87e
  (fn flat [elem]
    (if (coll? elem)
      (mapcat flat elem)
      [elem]
      )
    ))

(defcheck solution-f69088c8
  (fn myflatten [coll]
    (if
     (not (sequential? coll)) coll
                              (reduce
                                (fn [acc x]
                                  (if (not (sequential? x))
                                    (conj acc x)
                                    (apply conj acc (myflatten x))))
                                []
                                coll))))

(defcheck solution-f7c61a09
  #(reduce
     (fn f [acc x] (if (coll? x) (reduce f acc x) (conj acc x)))
     [] %))

(defcheck solution-f7fb83e3
  #(reverse ((fn myflatten [xs]
               (let [f (fn [acc, x]
                         (if (or (list? x) (vector? x))
                           (concat (myflatten x) acc)
                           (conj acc x)))]
                 (reduce f () xs))) %)))

(defcheck solution-f82a844c
  (fn [l] (first
            (first (drop-while
                     #(not= (first %) (second %))
                     (partition 2 (iterate #(reduce
                                              (fn [c i]
                                                (cond
                                                  (coll? i) (into c i)
                                                  :else (conj c i))) [] %) l)))))))

(defcheck solution-f839360c
  (fn flatten2 [coll] (if (empty? coll)
                        coll
                        (if (sequential? (first coll))
                          (concat (flatten2 (first coll)) (flatten2 (rest coll)))
                          (cons (first coll) (flatten2 (rest coll)))))))

(defcheck solution-f86781c8
  (fn my-flat [x]
    (if (not (coll? x)) (vector x)
                        (mapcat my-flat x)
                        )))

(defcheck solution-f89df946
  (fn my-flatten [coll]
    (mapcat (fn [el]
              (if (coll? el)
                (my-flatten el)
                `(~el))) coll)))

(defcheck solution-f90f806f
  (fn flttn
    [l]
    (if-let [h (first l)]
      (concat (if (coll? h) (flttn h) (list h)) (flttn (rest l)))
      '())))

(defcheck solution-f930f007
  (fn f [x coll]
    (reduce #(if (sequential? %2) (into %1 (f [] %2)) (conj %1 %2)) x coll)) []
  )

(defcheck solution-f9c7e701
  (fn flat [l]
    (if (not (or (seq? l) (vector? l)))
      (list l)
      (if (= (count l) 0)
        '()
        (concat (flat (first l))
                (flat (rest l)))))))

(defcheck solution-fa3c1cfe
  (fn flat [x]
    (if (coll? x) (mapcat flat x) [x])))

(defcheck solution-faa188fe
  #(remove sequential?
     (rest (tree-seq sequential? seq %))))

(defcheck solution-faf95bf4
  (fn my-flatten [coll]
    (mapcat #(if (coll? %) (my-flatten %) [%]) coll)))

(defcheck solution-fb0052e4
  (fn [xs] (letfn
            [(my-flatten [x] (let
                              [y (first x)
                               z (rest x)]
                               (if (empty? x)
                                 x
                                 (if (coll? y)
                                   (concat (my-flatten y) (my-flatten z))
                                   (cons y (my-flatten z))))))]
             (my-flatten xs))))

(defcheck solution-fb6a977f
  (fn flt [s] (if (sequential? s) (mapcat flt s) (list s))))

(defcheck solution-fb8dc949
  (fn f [x]
    (if (= x '())
      '()
      (let [a (first x) b (rest x)]
        (into (f b) (reverse (if (coll? a) (f a) (list a))))
        )
      )
    ))

(defcheck solution-fba534b7
  (fn my-flatten [s]
    (if (coll? s)
      (mapcat my-flatten s)
      [s])))

(defcheck solution-fba64bb3
  (fn flat [[a & r]] (if (nil? a) [] (if (sequential? a) (concat (flat a) (flat r)) (concat [a] (flat r))))))

(defcheck solution-fbea07e6
  #(letfn [(f [x] (if (sequential? x) (reduce concat (list) (map f x)) (list x)))] (f %)))

(defcheck solution-fbf0cdde
  (fn f [s] (mapcat #(if (sequential? %) (f %) (list %)) s)))

(defcheck solution-fc287d7f
  (fn my-flatten [l]
    (let [walk (fn walk [node] (lazy-seq (cons node (if (sequential? node) (mapcat walk (seq node))))))]
      (filter (complement sequential?) (rest (walk l)))

      (filter (complement sequential?) (rest (walk l))))
    ))

(defcheck solution-fc3c9c9f
  (fn f [coll]
    (when (seq coll)
      (let [x     (first coll)
            coll' (rest coll)]
        (if (coll? x)
          (concat (f x) (f coll'))
          (cons x (f coll')))))))

(defcheck solution-fc3e86e2
  (fn [x]
    (filter
      #(not (sequential? %))
      (tree-seq sequential? seq x))))

(defcheck solution-fc43767b
  (fn f [l]
    (if (= (count l) 1)
      (if (coll? (first l))
        (f (first l))
        (list (first l))
        )
      (concat (f (list (first l))) (f (rest l)))
      )
    ))

(defcheck solution-fc9082f3
  (fn flat [xs]
    (when-let [[x & xs] (seq xs)]
      (if-not (sequential? x)
        (cons x (flat xs))
        (concat (flat x) (flat xs))))))

(defcheck solution-fca69c82
  (fn myfn

    [seq1]

    (if (empty? seq1)
      nil

      (if (coll? (first seq1))
        (concat (myfn (first seq1)) (myfn (rest seq1)))
        (concat (list (first seq1)) (myfn (rest seq1)))

        )

      )

    ))

(defcheck solution-fd66c03c
  (fn flt [x]
    (if (coll? x)
      (reduce #(concat % (flt %2)) () x)
      (list x))
    ))

(defcheck solution-fda2b8bb
  (fn f [c] (if (coll? c) (mapcat f c) [c])))

(defcheck solution-fda8c32e
  (fn fl [l]
    (if (empty? l)
      l
      (let [f (first l) r (rest l)]
        (if (coll? f)
          (concat (fl f) (fl r))
          (cons f (fl r)))))))

(defcheck solution-fdee1961
  (fn f [xs] (if (sequential? xs) (apply concat (map f xs)) [xs])))

(defcheck solution-fe28146a
  #(if (some coll? %)
     (recur (mapcat (fn [x] (if (coll? x)
                              x
                              (list x)))
              %))
     %))

(defcheck solution-fe2f27b
  (fn new-flatten [coll]
    (cond
      (empty? coll) nil
      (coll? (first coll)) (concat (new-flatten (first coll))
                                   (new-flatten (rest coll)))
      :else (cons (first coll) (new-flatten (rest coll))))))

(defcheck solution-fe3514c3
  (fn [s]
    (loop [curr s]
      (if (some sequential? curr)
        (recur (reduce #(if (sequential? %2) (apply conj %1 %2) (conj %1 %2)) [] (vec curr)))
        curr))))

(defcheck solution-fe3af6e8
  (fn fltn [coll]
    (cond (empty? coll) coll
          (not (sequential? (first coll))) (cons (first coll) (fltn (rest coll)))
          :else (concat (fltn (first coll)) (fltn (rest coll))))))

(defcheck solution-fe42a5a4
  (letfn [(flatfn [acc c] (if-not (coll? c) (cons c acc) (reduce flatfn acc c)))] (comp reverse (partial flatfn (list)))))

(defcheck solution-fe4f5e9f
  (fn f [xs] (mapcat #(if (coll? %) (f %) (list %)) xs)))

(defcheck solution-fe60199a
  (fn _ [s] (mapcat #(if (coll? %) (_ %) (list %)) s)))

(defcheck solution-fe6fa237
  (fn [x]
    (loop [col x]
      (if (not (some coll? col))
        (sort col)
        (if (coll? (first col))
          (recur (concat (rest col) (first col)))
          (recur (reverse (conj (reverse (rest col)) (first col)))))))))

(defcheck solution-feb81202
  #(filter (complement coll?) (tree-seq coll? identity %)))

(defcheck solution-ff439f4f
  (fn flat [c]
    (cond
      (not (coll? c)) (cons c '())
      (empty? c) '()
      :else (concat (flat (first c)) (flat (rest c))))))
