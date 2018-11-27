(ns coal-mine.problem-41
  (:require [coal-mine.checks :refer [defcheck-41] :rename {defcheck-41 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-121dc098
  (fn dropn [xs n] (when (seq xs) (concat (take (dec n) xs) (dropn (drop n xs) n)))))

(defcheck solution-126b1f03
  (fn [lst n]
    (mapcat #(take (dec n) %)
      (partition-all n lst))))

(defcheck solution-126dce0a
  (fn f [coll n]
    (let [[l r] (split-at (dec n) coll)]
      (concat l (if (empty? r) r
                               (f (drop 1 r) n))))))

(defcheck solution-12718f8d
  (fn [c n]
    (flatten (concat (map #(drop-last %) (partition n c)) (take-last (rem (count c) n) c)  ))))

(defcheck solution-12a5a2a7
  (fn [s n]
    (filter identity
      (map-indexed #(if (zero? (mod (+ %1 1) n))
                      nil
                      (identity %2)) s))))

(defcheck solution-12d17442
  (fn [xs n] (filter identity (map-indexed (fn [i x] (if-not (= 0 (mod (inc i) n)) x)) xs))))

(defcheck solution-12da6e03
  (fn [s n] (let [part-s (partition-all n s)]
              (into [] (mapcat (fn [coll] (if (>= (count coll) n)
                                            (drop-last coll)
                                            coll
                                            ))
                         part-s)))))

(defcheck solution-12ea3d5
  #(apply concat (partition (- %2 1) %2 [] %1)))

(defcheck solution-1395e5e
  #(flatten (partition (- %2 1) %2 '() %1)))

(defcheck solution-13eb0b0
  (fn [xs n]
    (loop [i 1 xs xs accum []]
      (cond (empty? xs) accum
            (zero? (mod i n)) (recur (inc i) (rest xs) accum)
            :else (recur (inc i) (rest xs) (conj accum (first xs)))))))

(defcheck solution-13f4a3d3
  (fn [coll index] (keep-indexed #(if (not (= (dec index) (mod % index))) %2) coll)))

(defcheck solution-147b0ae5
  (fn [x y]
    (mapcat #(if (= y (count %))
               (drop-last %)
               %) (partition-all y x))))

(defcheck solution-14a593a7
  (fn [sc n]
    (loop [i 1 s sc acc []]
      (cond (empty? s) acc
            (integer? (/ i n)) (recur (inc i) (rest s) acc)
            :else (recur (inc i) (rest s) (conj acc (first s)))
            ))))

(defcheck solution-152853f7
  (fn everynthitem
    [lista n]
    #_(println lista)
    (if-not (empty? lista)
      (concat (take (dec n) lista)
              (everynthitem (drop n lista) n)))))

(defcheck solution-1531e2e1
  (fn __
    ([l n]
     (__ (drop n l) n (drop-last (take n l)))
     )
    ([l n f]
     (if (< (count l) n)
       (concat f l)
       (__ (drop n l) n (concat f (drop-last (take n l))))
       )
     )
    ))

(defcheck solution-15381049
  (fn [c n]
    (loop [x [] y c]
      (if (empty? y)
        x
        (recur
          (concat x (take (- n 1) y))
          (drop n y))))))

(defcheck solution-1550af5
  (fn f [l c] (if (empty? l) l (concat (take (dec c) l) (f (drop c l) c)))))

(defcheck solution-15d6577
  (fn f[x n] (
               let [ i (map-indexed vector x) ]
               (for [ [a b] i :when (> (rem (inc a) n) 0) ] b))))

(defcheck solution-15ea12f
  (fn rnth [s n] (apply concat (map #(if (= n (count %)) (drop-last %) %) (partition-all n s)))))

(defcheck solution-15f0ecf2
  (fn [s n]
    (mapcat #(take (dec n) %) (partition-all n s))
    ))

(defcheck solution-167abe65
  #(apply concat (partition-all (- %2 1) %2  %1)))

(defcheck solution-1695f6
  (fn [xs n]
    (apply concat (partition (dec n) n nil xs))))

(defcheck solution-16c79b28
  (fn drop-n [s n]
    (let [q (seq s)]
      (when q
        (concat (take (dec n) q) (drop-n (drop n q) n))))))

(defcheck solution-16e6bd01
  (fn dn
    [lst n]
    (if (empty? lst)
      lst
      (concat (take (dec n) lst) (dn (drop n lst) n)))))

(defcheck solution-170db0bd
  (fn [xs n]
    (flatten (map #(take (dec n) %) (partition-all n xs)))))

(defcheck solution-171ecce3
  (fn [v n]
    (loop [v v, m (dec n), r (empty v)]
      (if (seq v)
        (if (zero? m)
          (recur (rest v) (dec n) r)
          (recur (rest v) (dec m) (conj r (first v))))
        r))))

(defcheck solution-17b7a2d7
  (fn [s n]
    (loop [x 1 l s c '()]
      (if (empty? l)
        c
        (recur (inc x) (rest l) (if (zero? (rem x n))
                                  c
                                  (concat c [(first l)])))))))

(defcheck solution-17e8af59
  (fn mynth
    [myseq idx]
    (loop
     [myrest myseq cnt 1 res nil]
      (if (empty? myrest)
        res
        (if (= 0 (rem cnt idx) )
          (recur (rest myrest) (inc cnt) res)
          (recur (rest myrest) (inc cnt) (concat res (list (first myrest))))
          )
        ))))

(defcheck solution-17f102ea
  (fn f [coll n]
    (when (seq coll)
      (concat (take (dec n) coll) (f (drop n coll) n)))))

(defcheck solution-17f248d2
  (fn f[s n]
    (if-not (seq s) '()
                    (concat
                     (take (dec n) s) (f (drop n s) n)))))

(defcheck solution-1805d142
  (fn drop-nth [coll n]
    (for [[x y] (map-indexed vector coll)
          :when ((complement =) 0 (mod (inc x) n))]
      y)))

(defcheck solution-18298964
  (fn [coll n]
    (filter (comp not nil?) (map-indexed #(if (= 0 (mod (inc %1) n)) nil %2) coll))))

(defcheck solution-18520bc1
  (fn [a n]
    (map second (filter #(< (first %) n) (map-indexed (fn [i x] [(inc (mod i n)) x]) a)))))

(defcheck solution-18566c1a
  #(remove (set (take-nth %2 (nthrest %1 (- %2 1)))) %1))

(defcheck solution-18e89781
  (fn [s n]
    (map #(second %)
      (remove #(= 0 (mod (first %) n))
        (map-indexed #(list (inc %1) %2) s)))))

(defcheck solution-191955d
  (fn [col index]
    (into [] (mapcat #(if (= index (count %)) (drop-last %) %) (partition-all index col )))

    ))

(defcheck solution-193b2c46
  (fn [coll n] (keep-indexed #(if (> (rem (+ 1 %1) n) 0) %2) coll)))

(defcheck solution-19d936cf
  (fn[a-seq n]
    (let [a-sym (gensym)]
      (mapcat #(filter (fn[x] (not= x a-sym))
                 (drop-last %))
        (partition n n (repeat a-sym) a-seq)))))

(defcheck solution-1a6cc89b
  (fn [l n]
    (loop [news '() tmpl l]
      (if (empty? tmpl)
        (flatten news)
        (recur (conj (take (- n 1) tmpl) news)(drop n tmpl))))))

(defcheck solution-1ac6a7e0
  (fn x [s n]
    (if (empty? s)
      s
      (concat (take (- n 1) s)
              (x (drop n s) n)))))

(defcheck solution-1ac9590b
  (fn [s n]
    (loop [s_ s i n ret '()]
      (cond
        (not s_) (into '() ret)
        (= i 1) (recur (next s_) n ret)
        :else (recur (next s_) (dec i) (conj ret (first s_)))))))

(defcheck solution-1b271a19
  #(keep-indexed (fn [i v] (if (not (= (mod (+ 1 i) %2) 0)) v)) %1 ))

(defcheck solution-1b299557
  #(keep-indexed(fn[a b](when(not=(dec %2)(mod a %2))b))%1))

(defcheck solution-1b6bb2c2
  (fn [s n] (keep-indexed #(when-not (= 0 (mod (inc %1) n)) %2) s)))

(defcheck solution-1c01b31f
  (fn drop-at [coll pos]
    (loop [acc [] part coll]
      (cond (empty? part) (flatten acc)
            :else (recur
                    (conj acc (take (- pos 1) part))
                    (next (drop (- pos 1) part)))))))

(defcheck solution-1c3ab57d
  (fn drop-nth [xs n]
    (mapcat (partial take (dec n))
      (partition-all n xs))))

(defcheck solution-1cd51f01
  (fn peu [x y] (if (empty? x) (vec x) (concat (vec (take (dec y) x)) (peu (drop y x) y)))))

(defcheck solution-1d8d5527
  (fn dropnth
    ([coll n] (dropnth coll n 1))
    ([coll n m]
     (cond
       (empty? coll)
       '()
       (<= n m)
       (dropnth (rest coll) n 1)
       true
       (cons (first coll) (dropnth (rest coll) n (inc m)))))))

(defcheck solution-1dc51ba9
  (fn [coll value]
    (loop [result[]
           iter 1]
      (if (> iter (count coll))
        result
        (recur (if (not= (mod iter value) 0)
                 (conj result (nth coll (- iter 1)))
                 result)
          (inc iter))))))

(defcheck solution-1e13cf52
  (fn [coll nth] (filterv #(not (zero? (mod (inc (.indexOf coll %)) nth))) coll)))

(defcheck solution-1e2e072b
  (fn [lst n]
    (filter identity
      (map-indexed
        #(if (= (dec n) (mod % n)) nil %2) lst))))

(defcheck solution-1f785a7d
  (fn [lat n]  (flatten (map #(if (= (count %) n) (drop-last %) %) (partition-all n lat)))))

(defcheck solution-2005928c
  (fn drop-nth [lst n]
    (if (empty? lst)
      '()
      (concat  (take (- n 1) lst)
               (drop-nth (drop n lst) n)))))

(defcheck solution-200fd61c
  (fn drop-every [s n]
    (if (empty? s)
      []
      (concat (take (dec n) s) (drop-every (drop n s) n)))))

(defcheck solution-204bdfec
  #(loop [i 0, [f & r] %1, res '()]
     (if (nil? f) res
                  (recur
                    (mod (inc i) %2)
                    r
                    (if (< i (dec %2))
                      (concat res (list f))
                      res)))))

(defcheck solution-20609d55
  (fn [s n]
    (loop [src s
           index 1
           result []]
      (if (seq src)
        (recur (rest src) (inc index) (if (= 0 (rem index n)) result (conj result (first src))))
        result))))

(defcheck solution-208f45b7
  (fn  [s n]
    (keep-indexed
      (fn [i,v]
        (if (not= 0 (mod (inc i) n))
          v
          nil))
      s)))

(defcheck solution-21137b0e
  ;#(loop [result [] coll %1 cur 1]
  ;   (let [r %2]
  ;     (if (empty? coll)
  ;       result
  ;       (if (= 0 (rem cur r))
  ;         (recur result (rest coll) (inc cur))
  ;         (recur (conj result (first coll)) (rest coll) (inc cur))))))*/
  #(remove nil? (map-indexed (fn [i x] (when (not= (rem (inc i) %2) 0) x)) %1)))

(defcheck solution-2147d489
  #(mapcat identity (partition-all (- %2 1) %2 %1)))

(defcheck solution-215aa43a
  (fn [coll n]
    (map #(second %)
      (remove #(zero? (rem (first %) n))
        (map-indexed (fn [i x] [(inc i) x]) coll)))))

(defcheck solution-217a38ad
  (fn drop-every-nth [l n]
    (if (< (count l) n)
      l
      (into (vec (butlast (take n l))) (drop-every-nth (vec (nthrest l n)) n)))))

(defcheck solution-21841f2d
  #(keep-indexed
     (fn [idx itm]
       (if-not (zero? (rem (inc idx) %2)) itm))
     %))

(defcheck solution-2248c256
  (fn dropN [ls x]
    (if (< (count ls) x)
      ls
      (concat (take (- x 1) ls)
              (dropN (drop x ls) x)
              )
      )
    ))

(defcheck solution-22754d43
  (fn [xs n] (keep-indexed #(if (= (rem (inc %1) n) 0) nil %2) xs)))

(defcheck solution-233fee8b
  (fn [s n]
    (keep-indexed (fn it [i item]
                    (when (> (mod (inc i) n) 0) item)
                    ) s)

    ))

(defcheck solution-2369a185
  (fn [xs n]
    (let [is-nth (fn [i]
                   (= 0 (rem (inc i) n)))]
      (keep-indexed (fn [i x]
                      (if (is-nth i) nil x))
        xs))))

(defcheck solution-236bdf95
  (fn [s n]
    (keep-indexed #(if (not= (mod %1 n) (- n 1)) %2) s)))

(defcheck solution-238dc128
  (fn [ls n] (map first (filter (fn [[a b]] (not= b 1))
                          (map vector ls (cycle (range n 0 -1)))))))

(defcheck solution-23a2a271
  #(mapcat butlast (partition-all %2 (concat %1 '(0)))))

(defcheck solution-23b79bc2
  (fn [x y]
    (keep-indexed
      (fn [idx val]
        (when-not (integer? (/ (inc idx) y))
          val)) x)))

(defcheck solution-23bd68be
  #(map (fn [x] (first x))
     (filter
       (fn [x] (> (mod (second x) %2) 0))
       (map (fn [x y] [x y])
         %1 (range 1 (inc (count %1)))
         )

       )

     ))

(defcheck solution-243368b0
  (fn this
    ([xs n] (this xs n []))
    ([xs n acc]
     (if (empty? xs)
       acc
       (recur (drop n xs) n (into acc (take (dec n) xs)))))))

(defcheck solution-2442ee80
  (fn [lst n]
    (flatten
      (map #(if (= n (count %)) (drop-last %) %)
        (partition-all n lst)))))

(defcheck solution-24435816
  (fn drop-nth [s e]
    (mapcat #(if (= e (count %)) (drop-last %) %) (partition e e () s))))

(defcheck solution-258189a8
  (fn ! [s n]
    (when (seq s)
      (concat (take (- n 1) s) (! (rest (drop  (- n 1) s)) n)))))

(defcheck solution-25b74450
  #(
     loop [i 1 new_seq []]
     (if (= (inc (count %1)) i)
       new_seq
       (recur (inc i)
         (if (zero? (mod i %2))
           new_seq
           (conj new_seq (nth %1 (dec i)))
           )
         )
       )
     ))

(defcheck solution-25b9e656
  (fn drop-every-nth [sq n] (if (empty? sq) sq (concat (take (- n 1) sq) (drop-every-nth (drop n sq) n)))))

(defcheck solution-25cfdde1
  (fn [a n] (filter #(not (nil? %)) (mapcat butlast (partition n n (repeat nil) a)))))

(defcheck solution-25ecb61d
  (fn [s n] (map (fn [[k v]] v)
              (filter (fn [[i v]]
                        (not (= 0 (rem (inc i) n))))
                (map-indexed (fn [i v] [i v]) s)))))

(defcheck solution-26259a57
  (fn [xs k]
    (loop [xs' xs, r []]
      (let [[left right] (split-at k xs')]
        (if (< (count left) k) (concat r left)
                               (recur right (concat r (butlast left))))))))

(defcheck solution-26382b2a
  (fn dropn [ l d ]
    (let [ builder (fn builder [ lst drop ]
                     (cond
                       (empty? lst) lst
                       (= 1 drop) (recur (rest lst) d)
                       :else (cons (first lst) (builder (rest lst) (dec drop))))) ]
      (builder l d))))

(defcheck solution-265279f4
  (fn [s n]
    (map
      (fn [[i x]] x)
      (filter
        (fn [[i x]] (not (= (mod i n) (dec n))))
        (map-indexed vector s)))))

(defcheck solution-2676660e
  (fn [l p]
    (for [n (remove #(= (mod % p) (dec p)) (range (count l)))]
      (nth l n))))

(defcheck solution-2686b020
  (fn [coll n]
    (reverse
      (loop [i 1
             remaining coll
             result '()]
        (cond
          (empty? remaining) result
          (= i n) (recur 1 (rest remaining) result)
          :else (recur (inc i) (rest remaining) (cons (first remaining) result)))))))

(defcheck solution-2689986a
  (fn f [l n]
    (if (= '() l)
      '()
      (concat (take (dec n) l)
              (f (drop n l) n)))))

(defcheck solution-26af4a10
  (fn [c n] (map first (filter #(not= 0 (last %)) (map-indexed (fn [i e] [e (mod (inc i) n)]) c)))))

(defcheck solution-281155b1
  (fn [s bn] (mapcat #(if (= (count %) bn) (butlast %) %) (partition-all bn s))))

(defcheck solution-283293db
  (fn
    [a b]
    (letfn
     [(step
        [a b]
        (if
         (>= (count a) b)
          (concat
           (take (- b 1) a)
           (step (drop b a) b))
          (take (- b 1) a)))]
      (step a b))))

(defcheck solution-2839fddd
  (fn dropn [s n]
    (mapcat #(take (dec n) %) (partition-all n s))))

(defcheck solution-2913e265
  (fn [xs n]
    (keep-indexed
      (fn [pos elem] (if (not= (dec n) (mod pos n))
                       elem))
      xs)))

(defcheck solution-29523458
  (fn [coll n]
    (->> coll
      (map list (rest (range)))
      (filter (fn [[i e]] (pos? (mod i n))))
      (map second))
    ))

(defcheck solution-29c0f971
  (fn [xs n]
    (map second (filter (fn [x] (not= (dec n) (mod (first x) n))) (map-indexed vector xs)))))

(defcheck solution-29ccbcaf
  (fn [x y] (mapcat #(take (dec y) %) (partition y y [] x) ) ))

(defcheck solution-29f4e319
  #(remove nil? (map-indexed (fn [i x] (if (zero? (rem (inc i) %2)) nil x )) %1)))

(defcheck solution-2a4b0e90
  #(keep-indexed (fn [a b] (if (> (mod (inc a) %2) 0) b)) %1))

(defcheck solution-2a870f62
  (fn [lst n] (keep-indexed (fn [i it] (if (= 0 (rem (inc i) n)) nil it)) lst)))

(defcheck solution-2ae15234
  #(loop [r [] c %1] (if (< (count c) %2) (flatten (conj r c)) (recur (conj r (take (dec %2) c)) (drop %2 c)))))

(defcheck solution-2af1b2a4
  (fn [xs n] (mapcat rest (partition-all n (cons 0 xs)))))

(defcheck solution-2b2cca7c
  #(mapcat butlast (partition %2 %2 "x" %) ))

(defcheck solution-2b36f6a9
  (fn f [x i]
    (when (seq x)
      (concat (take (dec i) x) (f (drop i x) i) ))))

(defcheck solution-2b607984
  (fn [coll n] (mapcat #(if (zero? (mod (inc %2) n)) () (list %)) coll (range))))

(defcheck solution-2bc1c80f
  (fn [c x]
    (keep-indexed
      #(when (not= 0 (rem (inc %1) x))
         %2) c)))

(defcheck solution-2c48a970
  (fn [s n]
    (map first
      (filter (fn [[e i]] (> i 0))
        (map (fn [e i] [e i]) s (rest (cycle (range n))))))))

(defcheck solution-2c7a76c5
  (fn f [col n] (if-not (empty? col) (concat (take (dec n) col) (f (drop n col) n)))))

(defcheck solution-2c9681dd
  (fn [coll n]
    (keep-indexed
      #(when-not (zero? (rem (inc %1) n)) %2)
      coll)))

(defcheck solution-2cd97db5
  (fn [items pos]
    (loop [items items current 1 result []]
      (if (empty? items)
        result
        (recur (rest items) (inc current)
          (if (= (mod current pos) 0)
            result
            (conj result (first items))))))))

(defcheck solution-2d24e47f
  (fn drop-every-nth [coll n]
    (keep-indexed (fn [x v] (if (= 0 (mod (+ 1 x) n)) nil v)) coll)))

(defcheck solution-2d3ec49e
  (fn  [lst n]
    (loop [rm (seq lst), acc (vector), i 1]
      (cond (empty? rm) (seq acc)
            (= n i) (recur (rest rm) acc 1)
            :else (recur (rest rm) (conj acc (first rm)) (inc i))))))

(defcheck solution-2d6386c5
  (fn dropevery [xs n] (mapcat #(if (= n (count %)) (butlast %) %) (partition-all n xs))))

(defcheck solution-2dcdb4d9
  (fn [seq n] (mapcat #(take (dec n) %) (partition-all n seq))))

(defcheck solution-2df6ebc2
  (fn drop-every
    [coll n]
    (if (> n (count coll))
      coll
      (concat (take (dec n) coll) (drop-every (drop n coll) n)))))

(defcheck solution-2e104cc6
  (fn [coll n] (flatten (partition-all (dec n) n coll))))

(defcheck solution-2e179fb3
  (fn drop-nth [sequence n]
    (apply concat (map #(take (dec n) %)
                    (partition n n [] sequence)))))

(defcheck solution-2e386f6f
  (fn [x y]
    (loop [tot '() x x y y]
      (if (empty? x)
        (flatten tot)
        (recur (concat (conj (take (- y 1) x) tot)) (drop y x) y)))))

(defcheck solution-2e5fe628
  (fn drop-every-nth [xs n]
    (if (empty? xs)
      xs
      (concat (take (- n 1) xs)
              (drop-every-nth (drop n xs) n)))))

(defcheck solution-2ea094ec
  (fn [coll n] (keep-indexed (fn [idx item] (if (= 0 (rem (+ idx 1) n)) nil item)) coll)))

(defcheck solution-2efd9f32
  (fn [items dropNth]
    (map second
      (filter #(not= (mod (first %) dropNth) 0)
        (map #(conj [] % %2) (iterate inc 1) items)))))

(defcheck solution-2f16852
  (fn drop_nth [xs n]
    (mapcat (partial take (dec n)) (partition-all n xs))))

(defcheck solution-2fbafccd
  (fn [coll n] (for [x (map vector coll (range)) :when (not= (mod (x 1) n) (dec n))] (x 0))))

(defcheck solution-2fd0edcc
  #(mapcat (partial take (dec %2))
     (partition-all %2 %)))

(defcheck solution-3022689b
  (fn [s n] (flatten
              (map-indexed
                (fn [i x] (if (= (rem i n) (dec n)) [] [x])) s))))

(defcheck solution-302aa31a
  (fn drop-every-nth-item [coll nth]
    (filter identity
      (reduce-kv (fn [vs k v]
                   (if (= 0 (mod (inc k) nth))
                     (conj vs nil)
                     (conj vs v))) [] coll))))

(defcheck solution-30a1be2f
  #(map last (filter (comp not zero? (fn [x] (mod (inc x) %2)) first) (map-indexed list %))))

(defcheck solution-30ab93e5
  (fn [coll num]
    (filter identity
      (map #(when (not= 0 (mod %2 num)) %1)
        coll
        (iterate inc 1)))))

(defcheck solution-30aba9aa
  (fn [s n] (map second (filter #(not= 0 (mod (inc (first %)) n)) (map-indexed vector s)))))

(defcheck solution-30e6972c
  (fn [xs, n]
    (loop [in xs
           out []]
      (if (empty? in)
        out
        (recur (drop n in) (concat out (take (- n 1) in)))))))

(defcheck solution-310e89a3
  (fn [a-seq n]
    (mapcat #(take (dec n) %)
      (partition-all n a-seq))))

(defcheck solution-3112647d
  #(loop [L %1, result []]
     (if (empty? L)
       result
       (recur (drop %2 L) (apply conj result (take (- %2 1) L))))))

(defcheck solution-31326870
  #(mapcat (partial take (dec %2)) (partition %2 %2 [] %1)))

(defcheck solution-323bb3b3
  (fn [s c] (flatten
              (map
                #(filter identity (butlast %))
                (partition c c (repeat nil) s)))))

(defcheck solution-325a1511
  (fn [xs n]
    (loop [i 1 ys xs rs []]
      (case ys
        [] rs
        (recur (inc i) (rest ys) (if (= (mod i n) 0) rs (conj rs (first ys))))))))

(defcheck solution-326021e0
  (fn [l n]
    (loop [cl []
           ml l
           m  1]
      (if (nil? ml)
        cl
        (if (= m n)
          ; drop this
          (recur cl (next ml) 1)
          (recur (conj cl (first ml)) (next ml) (inc m)))))))

(defcheck solution-327d05db
  #(->>
     (partition-all (- %2 1) %2 %)
     (mapcat identity)))

(defcheck solution-328fe103
  (fn [s n]
    (let [nth? #(when (pos? (mod %2 n)) %1)
          pos  (range 1 (+ (count s) 1))]
      (filter identity (map nth? s pos)))))

(defcheck solution-32c1849
  (fn [coll n]  (flatten  (partition (dec n) n [] coll))))

(defcheck solution-32cc66b3
  (fn [se n]
    (loop [res [] s se]
      (if (empty? s)
        (flatten res)
        (recur (conj res (take (dec n) s))
          (drop n s))))))

(defcheck solution-33026c1b
  (fn [s n] (apply concat (partition-all (dec n) n s))))

(defcheck solution-339545a4
  (fn[x y](filter #(not (= (mod (+ (.indexOf x %) 1) y) 0))    x)))

(defcheck solution-33c534a6
  (fn [s n]
    (remove #(= 0 (rem (inc (.indexOf s %)) n)) s)))

(defcheck solution-33e1504a
  (fn [data n]
    (flatten (map reverse (map #(if (> n (count %))
                                  %
                                  (rest %)) (map reverse (partition-all n data)))))))

(defcheck solution-346a127c
  #(keep-indexed (fn [n x] (if (zero? (mod (inc n) %2)) nil x)) %))

(defcheck solution-349fbc1e
  (fn [col n]
    (keep-indexed
      (fn [index item]
        (if (= 0 (mod (inc index) n))
          nil
          item))
      col)))

(defcheck solution-34a448ff
  (fn [coll d]
    (loop [i 1 acc [] orig coll]
      (if (empty? orig)
        acc
        (let [accfn (if (not= (mod i d) 0) #(conj % (first orig)) identity)]
          (recur (inc i) (accfn acc) (rest orig)))))))

(defcheck solution-34cb7d2e
  (fn [xs n] (mapcat #(if (= (count %) n) (butlast %) %) (partition-all n xs))))

(defcheck solution-34ce715b
  (fn [coll n]
    (loop [i 1
           r []
           c coll]
      (if (empty? c)
        r
        (if (= (rem i n) 0)
          (recur (inc i) r (rest c))
          (recur (inc i) (conj r (first c)) (rest c)))))))

(defcheck solution-3554b287
  #(reverse
     (loop [r '()
            c %1
            p 1]
       (cond
         (empty? c) r
         (zero? (mod p %2)) (recur r (rest c) (inc p))
         :else (recur (conj r (first c)) (rest c) (inc p))))))

(defcheck solution-35d80bef
  (fn  [coll n] (flatten (partition-all (dec n) n coll))))

(defcheck solution-35e1ea87
  (fn f [coll n]
    (if (<= n (count coll))
      (concat (take (- n 1) coll) (f (drop n coll) n))
      coll)))

(defcheck solution-35f545ea
  (fn [s n]
    (map last
      (filter #(not= (mod (first %) n) 0)
        (map-indexed #(vector (inc %1) %2) s)))))

(defcheck solution-3682c2f6
  (fn f
    ([l n] (f l n n []))
    ([[l & ls] n c a]
     (if l
       (if (= 1 c)
         (f ls n n a)
         (f ls n (dec c) (conj a l)))
       a))))

(defcheck solution-36a9cb8a
  (fn [col n]
    (keep-indexed #(if (pos? (mod (inc %) n) ) %2) col)))

(defcheck solution-36fbed90
  (fn [s n]
    (keep-indexed #(if (not= (mod %1 n) (dec n)) %2) s)))

(defcheck solution-37394b07
  (fn [s n]
    (loop [in [] more s acc []]
      (if (empty? more)
        (concat acc in)
        (recur (take (dec n) more) (drop n more) (concat acc in))))))

(defcheck solution-381f72a0
  #(loop [s %1
          i 1
          result []]
     (if (empty? s)
       result
       (let [[head & tail] s]
         (if (= i %2)
           (recur tail 1 result)
           (recur tail (inc i) (conj result head)))))))

(defcheck solution-3854a233
  (fn[_seq n] (filter (fn[x] (not= (rem (inc (.indexOf _seq x)) n) 0)) _seq)))

(defcheck solution-386bb9f5
  (fn rec [ls n]
    (loop [an [] ls ls i 1]
      (if (empty? ls) an
                      (if (zero? (rem i n))
                        (recur an (rest ls) (inc i))
                        (recur (conj an (first ls))
                          (rest ls) (inc i)))))))

(defcheck solution-387df50
  #_(fn [c i]
      (map #(% 1)
        (filter #(not= 0 (mod (% 0) i))
          (map (fn [a b] [a b]) (iterate inc 1) c))))

  #_(fn [c i]
      (keep-indexed #(if (not= (dec i) (mod % i)) %2) c))

  #_(fn [c m]
      (keep-indexed #(get {(dec m) nil} (mod % m) %2) c))

  #_(fn [c m]
      (loop [acc [] i 1 in c]
        (cond (empty? in) acc
              (= i m) (recur acc 1 (rest in))
              :else (recur (conj acc (first in))
                      (inc i)
                      (rest in)))))

  #_(fn [c m]
      (reduce (fn [acc [el i]]
                (if (= i m) acc (conj acc el)))
        [] (map vector c (cycle (map inc (range m))))))

  #_(fn [c m]
      (mapcat #(if (= %2 m) [] [%])
        c (cycle (map inc (range m)))))

  #_(fn [c m]
      (mapcat #(if (= %2 m) [] [%])
        c (cycle (range 1 (+ 1 m)))))

  #_#(apply concat (partition-all (dec %2) %2 %))

  #(flatten (partition-all (dec %2) %2 %)))

(defcheck solution-38b45226
  (fn drop-nth [xs n]
    (if (empty? xs)
      []
      (concat (take (dec n) xs) (drop-nth (drop n xs) n)))))

(defcheck solution-38baa47f
  (fn [xs n]
    (->> (map #(list (inc %1) %2) (range) xs)
      (filter #(not= (mod (first %) n) 0))
      (map #(second %)))))

(defcheck solution-38d72cbb
  (fn [l every]
    (for [i (range (count l))
          :when (not= (mod i every) (- every 1))]
      (l i))))

(defcheck solution-38e0fb2b
  (fn [l n]   (map first (filter #(not (= (mod (second %) n) 0)) (map vector l (range 1 (+ 1 (count l))))))))

(defcheck solution-3920369
  (fn [xs n]
    (for [[i x] (partition 2 (interleave (iterate inc 1) xs))
          :when (not= 0 (mod i n))] x)))

(defcheck solution-39a8c49c
  (fn [col n] (mapcat #(take (dec n) %) (partition-all n col))))

(defcheck solution-3a132c29
  #(filter
     (partial not= :padding)
     (mapcat concat
       (map butlast
         (partition %2 %2 (repeat %2 :padding) %)
         ))))

(defcheck solution-3b032e45
  #(loop [n 0, r '()]
     (if (= (count %1) n)
       r
       (recur (+ n 1) (if (= 0 (mod (+ 1 n) %2))
                        r
                        (concat r (list (nth %1 n))))))))

(defcheck solution-3b0e3c12
  (fn [col n]
    (apply concat (partition-all (dec n) n col))))

(defcheck solution-3b4ad123
  #(keep-indexed (fn [index element] (when-not (zero? (mod (inc index) %2)) element)) %1))

(defcheck solution-3b6092eb
  (fn every-nth [xs n]
    (if (empty? xs)
      ()
      (concat (take (- n 1) xs) (every-nth (drop n xs) n)
              )
      )
    ))

(defcheck solution-3ba4544d
  (fn [a-seq n]
    (loop [res []
           lst a-seq]
      (if (seq lst)
        (recur (apply conj res (take (dec n) lst))
          (drop n lst))
        res
        ))))

(defcheck solution-3bad10e2
  (fn [x n]
    (let
     [indices (remove #(= (- n 1) (mod % n)) (range 0 (count x)))]
      (map #(nth x %) indices))))

(defcheck solution-3c29c7ce
  (fn [x y] (mapcat #(if (= (count %) y) (drop-last %) %) (partition-all y x))))

(defcheck solution-3c7dbc28
  #(apply concat (partition (- %2 1) %2 [] %)))

(defcheck solution-3d137a00
  (fn [coll n] (mapcat #(take (- n 1) %) (partition-all n coll))))

(defcheck solution-3d2e8e5a
  (fn [s n] (apply concat (map-indexed #(if (zero? (mod (inc %) n)) [] [%2]) s))))

(defcheck solution-3d37729e
  (fn [coll n]
    (keep-indexed #(if (not (zero? (rem (inc %1) n))) %2) coll)))

(defcheck solution-3d9f29f5
  (fn [coll n]
    (map first
      (remove #(= (second %) n)
        (map list
          coll
          (cycle (range 1 (inc n))))))))

(defcheck solution-3dc73d2e
  #(mapcat butlast (partition %2 %2 [:f] %)))

(defcheck solution-3e051f65
  (letfn [(f [coll n]
            (if (seq coll)
              (concat (take (dec n) coll) (f (drop n coll) n))
              ()))]
    f))

(defcheck solution-3e103131
  (fn[xs n] (keep-indexed #(if (not= 0 (mod (inc %) n)) %2) xs)))

(defcheck solution-3e409c9f
  (fn dropnth [coll n]
    (let [s (take (dec n) coll)]
      (if (seq s)
        (flatten [s (dropnth (drop n coll) n) ])
        ()))))

(defcheck solution-3e60a291
  (fn [s n]
    (loop [ans (), unseen s]
      (if (seq unseen)
        (recur (concat ans (take (dec n) unseen)) (rest (drop (dec n) unseen)))
        ans))))

(defcheck solution-3ea111af
  (fn [xs n]
    (->> xs
      (map list xs (iterate inc 1))
      (remove (fn [[_ i]] (zero? (mod i n))))
      (map first))))

(defcheck solution-3eb01c12
  #(remove (fn[e](if (some #{e} (for[n (range (dec %2) (count %) %2)] (nth % n))) true false)) %))

(defcheck solution-3f3fe733
  (fn drop-nth [xs n]
    (keep-indexed
      #(if (zero? (mod (inc %1) n)) nil %2)
      xs)))

(defcheck solution-3fbeb20a
  (fn [col, n]
    (loop [col1 [] restcol col cnt 1]
      (if (empty? restcol)
        col1
        (recur (if (not= (mod cnt n) 0)  (conj col1 (first restcol)) col1 )  (rest restcol) (inc cnt))
        )
      )

    ))

(defcheck solution-3fdb4ef5
  (fn dropn [arr n]
    (mapcat identity (map-indexed #(if (= 0 (mod (inc %1) n)) () [%2]) arr))))

(defcheck solution-3ffb0b82
  #(mapcat (fn [s] (if (= %2 (count s)) (butlast s) s)) (partition-all %2 %1)))

(defcheck solution-40281add
  (fn [coll n]
    (keep-indexed #(if (= (mod (inc %1) n) 0) nil %2) coll)))

(defcheck solution-404eefa9
  (fn [coll n]
    (keep-indexed #(if (not= (mod (inc %1) n) 0) %2) coll)))

(defcheck solution-40e78b44
  (fn drop-nth [xs n]
    (if (seq xs)
      (concat (take (dec n) xs) (drop-nth (drop n xs) n)))))

(defcheck solution-413ba702
  (fn [string n] (mapcat (partial take (dec n)) (partition-all n string))))

(defcheck solution-4182ccf2
  (fn [s n] (let [sym (gensym)]
              (remove #(= % sym)
                (mapcat drop-last
                  (partition n n (repeat sym) s))))))

(defcheck solution-428fb9fe
  (fn [coll nth]
    (loop [c coll acc [] i 1]
      (cond
        (empty? c) acc
        (zero? (mod i nth)) (recur (rest c) acc (inc i))
        :else (recur (rest c) (conj acc (first c)) (inc i))))))

(defcheck solution-42d66a21
  (fn [coll n]
    (mapcat (partial take (dec n)) (partition-all n coll))))

(defcheck solution-4352dc79
  (fn [c n] (keep-indexed #(if-not (= 0 (mod (inc %1) n)) %2) c)))

(defcheck solution-438b2a3b
  (fn [xs n]
    (letfn
     [(go [ys i rs]
        (cond
          (empty? ys) rs
          (< 1 i) (go (rest ys) (dec i) (conj rs (first ys)))
          :else (go (rest ys) n rs)))]
      (go xs n []))))

(defcheck solution-438f33e4
  (fn [s n]
    (filter identity
      (map-indexed #(if (= (mod (+ %1 1) n) 0) nil %2) s))))

(defcheck solution-43c2ed7c
  (fn [c n]
    (mapcat #(if (= n (count %)) (butlast %) %) (partition-all n c))))

(defcheck solution-44f13162
  (fn [xs n]
    (map second
      (filter (fn [i] (not= (first i) n))
        (map list (cycle (range 1 (inc n))) xs)))))

(defcheck solution-450313ca
  (fn [inseq to-drop]
    (loop [ret []
           left-over inseq
           cnt 1]
      (if (= left-over [])
        ret
        (recur (concat ret (when (not= cnt to-drop) [(first left-over)]))
          (rest left-over)
          (if (= cnt to-drop) 1 (inc cnt)))))))

(defcheck solution-450ab92c
  (fn dropN [x n]
    (flatten (partition-all (dec n) n x))))

(defcheck solution-454caaee
  (fn [coll n] (keep-indexed #(when (not= 0 (mod (inc %) n)) %2) coll)))

(defcheck solution-456286dd
  (fn [xs n]
    (map #((vec xs) %) (filter #(not= (dec n) (rem % n)) (range (count xs)))
      )))

(defcheck solution-456e7d13
  (fn [x y]
    #_(println (range 1 y))
    (let [nums (range 0 (count x))]
      ((fn include [a b c]
         #_(println (first b))
         (if (empty? a)
           '()
           (if (= (mod (+ 1(first b)) c) 0)
             (include (rest a) (rest b) c)
             (conj (include (rest a) (rest b) c) (first a)))))
       x nums y))))

(defcheck solution-4583bd6d
  (fn [s n]
    ((fn [s r c]
       (cond
         (empty? s) r
         (= c n) (recur (rest s) r 1)
         :else (recur (rest s)
                 (concat r [(first s)])
                 (inc c))))
     s [] 1)))

(defcheck solution-464d54cf
  #(map second
     (filter (fn [v] (not= (rem (first v) %2) 0))
       (map list (iterate inc 1) %1))))

(defcheck solution-46d05cbd
  (fn drop-nth
    ([l n] (drop-nth l n []))
    ([l n result]
     (if (empty? l)
       result
       (drop-nth (drop n l)
         n
         (concat result (take (dec n) l)))))))

(defcheck solution-46da399b
  (fn [col n]
    (flatten
      (concat
       (map #(drop-last %) (partition n col))
       (take-last (rem (count col) n) col)
       ))))

(defcheck solution-47027070
  (fn drop-every
    [coll pos]
    (let [head (take (dec pos) coll)
          tail (drop pos coll)]
      (when-not (empty? head)
        (lazy-cat head (drop-every tail pos))))))

(defcheck solution-472afdc2
  (fn skipper[coll times]
    (lazy-cat
      (take (dec times) coll)
      (if (empty? coll)
        ()
        (skipper (drop times coll) times)))))

(defcheck solution-490f9a60
  (fn [xs n]
    (->> (cycle (range 1 (inc n)))
      ((partial map vector) xs)
      (filter #(not (= n (second %))))
      (map first))))

(defcheck solution-4a2b6d14
  (fn [c n]
    (apply concat
      (map-indexed
        #(if (zero? (mod (inc %1) n))
           []
           [%2])
        c))))

(defcheck solution-4a3ef2c6
  (fn [coll n] (apply concat (partition-all (dec n) n coll)) ))

(defcheck solution-4a6d09d1
  #(flatten (partition-all (- %2 1) %2 %1)))

(defcheck solution-4a8e0881
  (fn [coll n]
    (loop [result [] c coll i 1]
      (if (nil? c) (filter #(not (nil? %)) result)
                   (recur (conj result (if (> (mod i n) 0) (first c))) (next c) (inc i))))))

(defcheck solution-4ae4cb51
  #(keep-indexed (fn [i v]
                   (if (> (mod (inc i) %2) 0) v))
     %))

(defcheck solution-4be170bd
  (fn dropNth
    [coll index]
    "Returns a collection where every nth element
    from the original collection is missing"
    (if (> index (count coll))
      coll
      (concat (take (- index 1) coll) (dropNth (drop index coll) index)))))

(defcheck solution-4c34a71
  (fn drop-nth
    [s n]
    (when (not-empty s)
      (vec (concat (take (dec n) s) (drop-nth (drop n s) n))))))

(defcheck solution-4c90297
  (fn [xs n] (concat (take (dec n) xs) (mapcat #(drop 1 %) (partition-all n (drop (dec n) xs))))))

(defcheck solution-4c950490
  (fn [x,y] (remove nil? ( map-indexed #(if (= (dec y) (rem % y)) nil %2) x))))

(defcheck solution-4cb6c0f3
  (fn drop-nth
    [lst n]
    (cond
      (empty? lst) ()
      :else (concat (take (- n 1) lst) (drop-nth (drop n lst) n)))))

(defcheck solution-4cdaddc
  #(remove nil? (map-indexed (fn [i x] (when (not= (mod (inc i) %2) 0) x)) %1)))

(defcheck solution-4d1e8bc8
  (fn [l n]
    (mapcat #(take (dec n) %) (partition n n '() l))))

(defcheck solution-4d695b62
  #(->> % (partition %2 %2 [nil]) (mapcat butlast)))

(defcheck solution-4dd2dcb2
  (fn drop-every-nth-item [seq n]
    (apply concat (partition-all (dec n) n seq))))

(defcheck solution-4e3d9e2c
  (fn [coll n]
    (keep-indexed #(if (not= 0 (mod (inc %1) n)) %2) coll)))

(defcheck solution-4e4400b1
  (fn [coll n]
    (reduce (fn [items item] (if (zero? (rem (inc (.indexOf coll item)) n)) items (conj items item) )) [] coll)
    ))

(defcheck solution-4e7bc511
  (fn [coll n]
    (keep-indexed #(when-not (zero? (mod (+ %1 1) n)) %2) coll)))

(defcheck solution-4e95dd4c
  (fn drop-nth [xs n]
    (when-not (empty? xs)
      (lazy-cat (take (- n 1) xs) (drop-nth (drop n xs) n)))))

(defcheck solution-4ef0a6b7
  #(concat (mapcat butlast (partition %2 %1)) (take-last (mod (count %1) %2) %1)))

(defcheck solution-4f5cbd40
  (fn [coll item]
    (keep-indexed (fn [index value]
                    (if (not= 0 (mod (inc index) item))
                      value
                      nil))
      coll)))

(defcheck solution-4f62fa2c
  (fn [s n]
    (let [pairs (partition 2 (interleave s (range (count s))))
          select-pairs (remove (fn [[el place]] (= 0 (rem (inc place) n))) pairs)
          singles (map (fn[[el _]] el) select-pairs)
          result (flatten singles)]
      result)))

(defcheck solution-4f9dd598
  (fn [input numel]
    (loop [in input out [] n numel]
      (cond
        (empty? in) out
        (= n 1) (recur (rest in) out numel)
        :else (recur (rest in) (conj out (first in)) (dec n))))))

(defcheck solution-505f5d63
  (fn [s n] (flatten (map #(take (- n 1) %) (partition-all n s)))))

(defcheck solution-5088eae6
  #(keep-indexed (fn [idx item]
                   (if (= 0 (mod (inc idx) %2))
                     nil item)) %1))

(defcheck solution-50aa079c
  (fn [coll n]
    (apply concat (partition-all (dec n) n coll))))

(defcheck solution-511b48f2
  (fn [coll n]
    (loop [input coll i 0 result []]
      (if (= input '()) result
                        (recur (rest input) (rem (inc i) n)
                          (if (= i (dec n)) result (conj result (first input))))))))

(defcheck solution-5180c408
  (fn [coll n]
    (map first
      (remove (fn [[val idx]] (= 0 (mod idx n)))
        (map vector coll (iterate inc 1))))))

(defcheck solution-51a319c3
  (fn [seq n]
    (flatten (partition-all (dec n) n seq))))

(defcheck solution-51df3615
  (fn [coll n]
    (loop [in coll result []]
      (if (empty? in)
        result
        (let [it (split-at n in)
              maybe (first it)
              more (second it)]
          (recur more (concat result (if (= n (count maybe)) (butlast maybe) maybe))))))))

(defcheck solution-5241fe79
  #(for [x % :when (not= 0 (mod (inc (.indexOf % x)) %2))] x))

(defcheck solution-526ecd99
  (fn [coll nth]
    (reduce (fn [x y] (if (zero? (mod (count y) nth)) (concat x (drop-last y)) (concat x y))) [] (partition-all nth coll))))

(defcheck solution-5272c6e8
  (fn [c n]
    (mapcat #(take (dec n) %) (partition-all n c))))

(defcheck solution-5279fcac
  (fn [x n] (loop [x1 x, i 1, acc []]
              (if (empty? x1)
                acc
                (if (= 0 (rem i n))
                  (recur (rest x1) (inc i) acc)
                  (recur (rest x1) (inc i) (conj acc (first x1))))))))

(defcheck solution-537b3736
  #(apply concat (partition-all (- %2 1) %2 %)))

(defcheck solution-53a02c87
  (fn [xs n]
    (loop [cntr n xsp xs acc []]
      (if (empty? xsp)
        acc
        (if (= cntr 1)
          (recur n (rest xsp) acc)
          (recur (dec cntr) (rest xsp) (conj acc (first xsp))))))))

(defcheck solution-53f54667
  (fn keep-nth [coll n]
    (keep-indexed #(if-not (= 0 (mod (inc %1) n)) %2) coll)))

(defcheck solution-541e6797
  (fn [coll n]
    (let [p (partition n n [] coll)]
      (mapcat #(if (= (count %) n) (butlast %) %) p))))

(defcheck solution-54fcf1b5
  (fn [coll n]
    (loop[result [] coll coll]
      (if (= coll '()) result
                       (recur (into result (take (dec n) coll)) (drop n coll))))))

(defcheck solution-5501a33e
  (fn[l n]
    (mapcat identity
      (for [idx (range (/ (count l) n))]
        (take (- n 1) (drop (* idx n) l))))))

(defcheck solution-550905a9
  (fn [s n]
    (filter #(not (= 0 (rem (+ 1 (.indexOf s %))  n))) s)
    ))

(defcheck solution-55ccf53d
  #(apply concat (partition-all (dec %2) %2 %1)))

(defcheck solution-55d0da3b
  (fn [coll n]
    (keep-indexed #(when (not= (dec n) (mod % n)) %2)
      coll)))

(defcheck solution-55faf5ce
  (fn [s n]
    (map second (remove #(zero? (rem (first %1) n)) (map list (iterate inc 1) s)))))

(defcheck solution-56d0f062
  #(concat (mapcat drop-last (partition %2 %)) (take-last (rem (count %) %2) %) ))

(defcheck solution-57184510
  #(reduce (fn [acc [el n]]
             (if (zero? n)
               acc
               (conj acc el)))
     []
     (map list % (rest (cycle (range %2))))))

(defcheck solution-5796f335
  (fn [xs n]
    (flatten (map #(concat (take (dec n) %) (drop n %)) (partition-all n xs)))))

(defcheck solution-57acfff3
  (fn [ls p] (flatten (partition-all (dec p) p ls))))

(defcheck solution-57e94bb6
  (fn problem41-drop-nth
    [xs n]
    (if (empty? xs)
      ()
      (into [] (concat (take (dec n) xs) (problem41-drop-nth (drop n xs) n))))))

(defcheck solution-58ca6df2
  (fn drop-nth
    [sq nth]
    (let [
          xs  (seq sq)
          len (count xs)
          skp (take len (flatten (replicate (/ len nth) (range 1 (inc nth)))))]
      (mapcat #(if (= %2 nth) [] [%1]) xs skp))))

(defcheck solution-58fd5b4d
  (fn [sq n] (keep-indexed #(if (not= (dec n) (mod %1 n)) %2) sq)))

(defcheck solution-5926292
  (fn drop [liste n]
    (mapcat #(if (= (- n 1) (rem %2 n)) [] [%1]) liste (range (count liste)))
    ))

(defcheck solution-59804403
  (fn dnth
    [l n]
    (loop [ls l i 1 out []]
      (if (empty? ls)
        out
        (if (= (mod i n) 0 )
          (recur (rest ls) (inc i) out)
          (recur (rest ls) (inc i) (conj out (first ls)))
          )
        )
      )
    ))

(defcheck solution-59bbabcb
  #(flatten (partition-all (- %2 1)
              %2 %1)))

(defcheck solution-5a9e7367
  (fn dropn [l n]
    (map first
      (remove
        #(= 0 (rem (last %) n))
        (partition 2 (interleave l (range 1 99)))))))

(defcheck solution-5afff279
  (fn [seqv skip]
    (for [x (range (count seqv))
          :when (not (= (mod (inc x) skip) 0))]
      (get seqv x))))

(defcheck solution-5b2af3b
  (fn drp [x n]
    (if (< (count x) n)
      x
      (concat (take (- n 1) x) (drp (drop n x) n))
      )
    ))

(defcheck solution-5b60f2d8
  (fn [coll ind]
    (loop [coll coll counter 1 result []]
      (cond
        (empty? coll) result
        (pos? (mod counter ind)) (recur (rest coll) (inc counter) (conj result (first coll)))
        :else (recur (rest coll) (inc counter) result)
        )

      )


    ))

(defcheck solution-5c30f148
  (fn [s n]
    (apply concat
      (map #(take (dec n) %) (partition-all n s)))))

(defcheck solution-5c3d1f3d
  (fn [xs n]
    (for [i (range (count xs))
          :when (pos? (rem (inc i) n))]
      (xs i))))

(defcheck solution-5c9bc7f9
  (fn [c x]
    (loop [col c, svaki x, rez [], cnt 1]
      #_(print rez)
      (if (empty? col)
        rez
        (recur (rest col)
          svaki
          (concat rez (if (= 0 (rem cnt svaki))
                        []
                        (list (first col))))
          (inc cnt))))))

(defcheck solution-5ccd8c8f
  (fn dn [s n]
    (if (not-empty s)
      (concat (take (dec n) s) (dn (drop n s) n))
      s)
    ))

(defcheck solution-5d0b1e8c
  (fn [x y] (vec (mapcat (partial take (dec y)) (partition-all y x)))))

(defcheck solution-5e49e78d
  (fn [coll n]
    (loop [coll coll n n ret []]
      (if (empty? coll)
        ret
        (recur (drop n coll) n (into ret (take (dec n) coll)))))))

(defcheck solution-5ea88a86
  (fn [l n]
    (remove #(= nil %)
      (map-indexed (fn [idx itm]
                     (when (not (= (dec n)
                                  (rem idx n)))
                       itm))
        l))))

(defcheck solution-5ec2fe91
  (fn [xs c] (first (reduce (fn [[xs i] x] (if (= 0 i) [xs, (- c 1)] [(conj xs x), (- i 1)])) [[] (- c 1)] xs))))

(defcheck solution-5ef59de3
  (fn drop-nth
    [s n]
    (flatten (partition-all (dec n) n s))))

(defcheck solution-5f44fe88
  #(mapcat (partial take (dec %2)) (partition %2 %2 [] % )))

(defcheck solution-5f4cd754
  (fn [coll n] (apply concat (partition (dec n) n nil coll))))

(defcheck solution-5f5fbf89
  (fn [l n] (->> l (map-indexed #(vector (not= (dec n) (mod %1 n)) %2)) (filter #(% 0)) (map #(% 1)))))

(defcheck solution-5fa5dab0
  #((fn dropn [s c n]
      (if (empty? s)
        []
        (if (= (mod c n) 0)
          (dropn (rest s) (inc c) n)
          (cons (first s) (dropn (rest s) (inc c) n))))) %1 1 %2))

(defcheck solution-604dc8de
  (fn [coll n]
    (flatten (partition (dec n) n nil coll))))

(defcheck solution-605dcfda
  #(apply concat (map butlast (partition %2 %2 [:x] %1))))

(defcheck solution-60fed734
  #(map first
     (filter (comp (partial not= (dec %2))
                   second)
       (map vector % (cycle (range %2))))))

(defcheck solution-611c65fc
  (fn [arg1 arg2] (mapcat #(take (dec arg2) %) (partition-all arg2 arg1))))

(defcheck solution-611ea508
  (fn x [c n] (when-let [s (seq c)] (lazy-seq
                                      (concat (take (dec n) s) (x (drop n s) n))))))

(defcheck solution-61984bc8
  (fn n41 [lst n]
    (if (< (count lst) n)
      lst
      (concat (take (dec n) lst) (n41 (drop n lst) n)))))

(defcheck solution-61b5d56
  (fn [s n] (mapcat (partial take (dec n)) (partition-all n s))))

(defcheck solution-623c3687
  (fn [c n] (apply concat (map #(take (- n 1) %) (partition-all n c)))))

(defcheck solution-630ce83
  (fn [coll n]
    (let [partitioned (partition n n [] coll)]
      (mapcat (partial take (dec n)) partitioned))))

(defcheck solution-633f76e6
  (fn [coll step]
    (for [i (range (count coll))
          :when (not (zero? (rem (inc i) step)))] (coll i))))

(defcheck solution-6357fd0a
  (fn [col n]
    (mapcat (fn [x y] (if (= 0 (mod (+ x 1) n)) nil (list y)))
      (range (count col))
      col)))

(defcheck solution-635e73dd
  (fn drop-every-nth [xs n]
    (when (seq xs)
      (concat
       (take (dec n) xs)
       (drop-every-nth (drop n xs) n)))))

(defcheck solution-63838b3f
  (fn [coll n]
    (loop [acc [] coll coll]
      (if (seq coll)
        (recur (apply conj acc (take (dec n) coll)) (drop n coll))
        acc))))

(defcheck solution-640ae06b
  (fn [coll n]
    (reduce (fn [res [idx elem]] (if (zero? (mod (inc idx) n)) res (conj res elem)))
      []
      (map-indexed vector coll))))

(defcheck solution-6451d380
  (fn [x n] (loop [innercount 0 result []]
              (if (> (count x) innercount)
                (recur (+ innercount 1)
                  (if (= (mod (+ innercount 1) n) 0)
                    result
                    (conj result (nth x innercount))
                    )
                  )
                result))))

(defcheck solution-64696ca5
  #(keep-indexed (fn [i v] (if-not (zero? (mod (inc i) %2)) v)) %1))

(defcheck solution-64855fed
  (fn [x y] (filter #(not (nil? %)) (map-indexed #(if (= (mod (inc %1) y) 0) nil %2) x))))

(defcheck solution-64b2725f
  (fn dropa [s n]
    (if (< (count s) n)
      s
      (concat
       (->> (split-at n s) first (drop-last 1))
       (-> (split-at n s) second (dropa n))))))

(defcheck solution-6513579f
  (fn [col n]
    (mapcat #(take (dec n) %)
      (partition-all n col))
    ))

(defcheck solution-6536c583
  (fn [coll n] (map #(nth % 1) (filter #(not= 0 (mod (inc (first %)) n)) (map-indexed #(list %1 %2) coll)))))

(defcheck solution-6578688a
  (fn [L n] (remove #(contains?  (into #{} (take-nth n (drop (- n 1) L ) ) )    %) L ) ))

(defcheck solution-65a94690
  (fn [coll n]
    (keep-indexed
      (fn [index item]
        (if (zero? (mod (inc index) n))
          nil
          item))
      coll)))

(defcheck solution-65b3f8a0
  (fn [l n]
    (map second (filter #(not= 0 (mod (inc (first %)) n))
                  (map-indexed vector l)))))

(defcheck solution-65dc11f0
  #(mapcat (fn [pack] (butlast pack)) (partition %2 %2 [nil] %)))

(defcheck solution-65df57d2
  (fn [x n] (keep-indexed #(if (= (rem %1 n) (dec n)) nil %2) x)))

(defcheck solution-65fa487a
  #(remove nil? (map-indexed (fn [i x] (if (= 0 (mod (inc i) %2)) nil x)) %)))

(defcheck solution-661ca39f
  (fn [coll n]
    (let [icoll (map-indexed vector coll)]
      (map (fn [[_ x]] x) (filter (fn [[a b]] (not  (= (mod (inc a) n) 0))) icoll)))))

(defcheck solution-663283dc
  ;(fn [coll n](map second (remove #(zero? (mod (inc (first %)) n )) (map-indexed vector coll))))
  (fn [coll n](keep-indexed #(when (not= 0 (mod (inc %1) n)) %2) coll)))

(defcheck solution-66c99aef
  (fn dropnth[s n](when (seq s) (concat (take (dec n) s) (dropnth (drop n s) n)))))

(defcheck solution-66eda7d4
  (fn [coll n]
    (map second (filter
                  (fn [[i e]] (not (= 0 (mod i n))))
                  (map-indexed (fn [i e] [(inc i) e]) coll)))))

(defcheck solution-673d2728
  (fn drop-every-nth
    [s nth]
    (apply concat (partition (dec nth) nth [] s))))

(defcheck solution-6827ec26
  (fn drop-nths [s n]
    (map second (filter (fn [m] (not= 0 (mod (first m) n)))
                  (map vector (iterate inc 1) s)))))

(defcheck solution-68f7ac19
  (fn
    [li i]
    (into [] (keep-indexed
               #(if
                 (not(= (- i 1) (mod %1 i)))
                  %2
                  nil)
               li))))

(defcheck solution-696b369f
  (fn [xs n]
    (->> xs
      (map vector (map inc (range)))
      (filter #(not (zero? (mod (first %) n))))
      (map second))))

(defcheck solution-6974daf2
  #(mapcat identity (partition (dec %2) %2 nil %)))

(defcheck solution-699dc72a
  (fn [coll x] (keep-indexed (fn[idx i] (when (pos? (mod (inc idx) x)) i)) coll)))

(defcheck solution-69a652fd
  (fn dn [s, n]
    (if (>= n (count s))
      (vec (take (dec n) s))
      (vec (concat (take (dec n) s) (dn (nthnext s n) n))))))

(defcheck solution-69d58c90
  #(mapcat (partial take (dec %2)) (partition %2 %2 () %)))

(defcheck solution-6aaafeb2
  (fn dropn [col n]
    (remove nil? (map-indexed
                   (fn [idx itm]
                     (if (not (zero? (mod (inc idx) n))) itm))
                   col))))

(defcheck solution-6b00f
  (fn [col x]
    (mapcat drop-last (partition x x [nil] col))))

(defcheck solution-6b51b4f2
  (fn [xs n] (flatten (map #(if (= (count %) n) (take (- n 1) %) %) (partition-all n xs)))))

(defcheck solution-6c0d6631
  #(loop [[head & tail] %1
          cnt %2
          acc []]
     (if (nil? head)
       acc
       (if (> cnt 1)
         (recur tail (dec cnt) (conj acc head))
         (recur tail %2 acc)
         )
       )
     ))

(defcheck solution-6c7489fb
  (fn drop-nth
    ([coll n] (drop-nth coll n []))
    ([coll n result]
     (if (empty? coll)
       result
       (concat (take (dec n) coll) (drop-nth (drop n coll) n))))))

(defcheck solution-6d17d2
  #(loop [result nil seq %1]
     (if (= (count seq) 0)
       (apply concat (reverse result))
       (recur (cons (take (- %2 1) seq) result)
         (drop %2 seq)))))

(defcheck solution-6d60cf40
  #(mapcat (fn [x] (if (= (count x) %2) (butlast x) x)) (partition-all %2 %1)))

(defcheck solution-6d66d874
  #(mapcat butlast (partition %2 %2 '(0) %1)))

(defcheck solution-6de6deff
  (fn f
    ([s c] (f s c []))
    ([s c r]
     (if (empty? s)
       (flatten r)
       (recur (drop c s) c (conj r (take (- c 1) s)))))))

(defcheck solution-6dfb4853
  (fn [s i] (filter (complement nil?) (apply concat (partition (dec i) i [nil] s)))))

(defcheck solution-6e14c689
  (fn drop-nth
    [coll n]
    (vals (into {} (filter (complement (fn [[key val]]
                                         (= (mod (inc key) n) 0)))
                     (map-indexed vector coll))))))

(defcheck solution-6f220228
  (fn* taker [x y]
    (if (empty? x)
      ()
      (concat (take (dec y) x) (taker (drop y x) y)))))

(defcheck solution-6f47b224
  (fn [c n]
    (map second
      (filter #(not= (mod (first %) n) 0)
        (map list (iterate inc 1) c)))))

(defcheck solution-6f5f6ae2
  (fn [aa i]
    (loop [res []
           a aa]
      (if (empty? a)
        (flatten res )
        (recur
          (conj res (take (dec i) a))
          (drop i a))))))

(defcheck solution-6f7e1b55
  (fn [colls n]

    ((fn [colls cur r]

       (let [rr  (if (or (= 0 (mod cur n)) (empty? colls) )  r   (conj r (first colls)))]

         (if (empty? colls) rr (recur (rest colls) (inc cur) rr))))

     colls 1 [])))

(defcheck solution-6fc437ad
  (fn drop-every [coll n]
    (apply concat (partition-all (dec n) n coll))))

(defcheck solution-7041bdaa
  (fn [xs n]
    (->> xs
      (map-indexed #(list %1 %2))
      (filter #(not= (dec n) (mod (first %) n)))
      (map second))))

(defcheck solution-7107720e
  #(mapcat
     (partial take (dec %2))
     (partition-all %2 %1)))

(defcheck solution-71177f95
  (fn [xs x] (keep-indexed #(if (> (mod (inc %1) x) 0) %2) xs)))

(defcheck solution-71aff69e
  #(->>
     (partition-all (dec %2) %2 %1)
     (apply concat)))

(defcheck solution-71d2671d
  (fn [xs n] (mapcat #(if (= n (count %1)) (butlast %1) %1)
               (partition-all n xs))))

(defcheck solution-72d1d044
  (fn [c d]
    (loop [r [] c c cnt d]
      (if (empty? c) r
                     (if (= 1 cnt)
                       (recur r (rest c) d)
                       (recur (conj r (first c)) (rest c) (dec cnt))
                       ))
      )
    ))

(defcheck solution-732f8d8e
  (fn [coll n] (map second (filter #(not= 0 (mod (first %) n)) (map-indexed #(vector (inc %1) %2) coll)))))

(defcheck solution-73659a53
  (fn [c n] (mapcat #(take (dec n) %) (partition-all n c))))

(defcheck solution-737e8528
  (fn [xs v] (mapcat #(take (- v 1) %) (partition-all v xs))))

(defcheck solution-7385672b
  (fn [s n]
    (map last (filter #(pos? (mod (inc (first %)) n)) (map-indexed list s)))))

(defcheck solution-73cb4b0a
  #(for [x (range (count %1))
         :let [y (rem (inc x) %2)]
         :when (not= 0 y)]
     (nth %1 x)
     ))

(defcheck solution-73e0bf9c
  (fn cut[collect,num]
    (loop [col collect ,n num,x 1,result []]
      (if(empty? col)
        result
        (recur  (rest col) n (inc x)
          (if (= (mod x n) 0)
            result
            (conj result (first col) )
            )
          )
        )
      )
    ))

(defcheck solution-74047469
  (fn t [xs n] (when (not (empty? xs))
                 (concat (take (dec n) xs) (t (drop n xs) n)))))

(defcheck solution-743f5eb
  #(loop [i 0 out []]
     (if (= i (count %)) out
                         (recur (inc i) (if (= (dec %2) (mod i %2)) out (conj out (nth % i)))))))

(defcheck solution-748a4d74
  (fn my-drop-every [col n]
    (when col
      (lazy-cat (take (dec n) col) (my-drop-every (nthnext col n) n)))))

(defcheck solution-7559aedc
  (fn [coll n]
    (letfn [(drop-nth [rst next-drop]
              (if (empty? rst)
                ()
                (if (= 1 next-drop)
                  (drop-nth (rest rst) n)
                  (lazy-seq (cons (first rst)
                              (drop-nth (rest rst) (dec next-drop)))))))]
      (drop-nth coll n))))

(defcheck solution-75b1342
  (fn drop-nth [l n]
    (loop [l l cnt 1 f []]
      (if (empty? l)
        f
        (if (= cnt n)
          (recur (rest l) 1 f)
          (recur (rest l) (inc cnt) (conj f (first l))))))))

(defcheck solution-7614b9f7
  ; use keep-indexed
  (fn [lst x] (mapcat (partial take (- x 1)) (partition-all x lst))))

(defcheck solution-766f4e9c
  #(loop [li %1 n %2 result []]
     (if (< (count li) n)
       (into result li)
       (recur (nthnext li n) n (into result (take (dec n) li))))))

(defcheck solution-7692de44
  (fn drop-nth [coll n]
    (let [sections (partition n n nil coll)]
      (vec
        (mapcat
          (fn [section] (take (dec n) section))
          sections)))))

(defcheck solution-76b46169
  (fn [xs n]
    (concat (take (dec n) xs) (mapcat rest (partition-all n (drop (dec n) xs))))))

(defcheck solution-76b92a12
  #(->> %1 (partition (dec %2) %2 [nil]) (apply concat) (filter (complement nil?))))

(defcheck solution-7705b6bb
  (fn [s n] (keep-indexed #(when (pos? (rem (inc %1) n)) %2) s)))

(defcheck solution-772160f3
  (fn drop-every* [s n]
    (mapcat #(take (dec n) %) (partition-all n s))))

(defcheck solution-7733071c
  (fn [xs n] (keep-indexed #(if (= 0 (mod (+ 1 %1) n)) nil %2) xs)))

(defcheck solution-77408c1d
  #(map first (filter (fn [pair] (not= 0 (mod (inc (last pair)) %2)))
                (map list % (range ))
                )))

(defcheck solution-77b9da8f
  (fn [lst n]
    (mapcat
      #(take (dec n) %)
      (partition-all n lst))))

(defcheck solution-77d0fe05
  (fn [xs n] (mapcat #(take (dec n) %) (partition-all n xs))))

(defcheck solution-77ec8b2e
  #(flatten (partition (dec %2) %2 [] %)))

(defcheck solution-780602c0
  (fn drop-nth [xs n]
    (when-not (empty? xs)
      (concat (take (dec n) xs)
              (drop-nth (drop n xs) n)))))

(defcheck solution-78800aa
  (fn [seq n] (flatten (partition (- n 1) n nil seq))))

(defcheck solution-78b6d95c
  #(remove (set (take-nth %2 `(_ ~@%))) %))

(defcheck solution-79eae7f0
  (fn [c n] (keep-indexed
              #(if (> (rem (inc %) n) 0) %2)
              c)))

(defcheck solution-79fb41ad
  (fn __ [coll n]
    (->> coll
      (partition n n nil)
      (mapcat #(take (- n 1) %))
      )))

(defcheck solution-7a4ded1a
  (fn [xs cnt] (map second (filter #(pos? (mod (inc (first %)) cnt)) (map-indexed vector xs)))))

(defcheck solution-7aa301dc
  (fn drop-n [coll n] (flatten (map drop-last (filter #(not= 0 (rem (second %) n))(map #(vector %1 %2) coll (map #(+ 1 %) (range))))))))

(defcheck solution-7ab04ff
  (fn [col nth]
    (loop [c col result []]
      (if (empty? c)
        result
        (let [new-result (concat result (take (dec nth) c))
              new-col (drop nth c)]
          (recur new-col new-result))))))

(defcheck solution-7bc0cf6c
  (fn [s n] (map second (filter #(not= (mod (first %) n) 0) (map vector (iterate inc 1) s)) )))

(defcheck solution-7de9cee3
  (fn [sq, num]
    (flatten (map (partial take (dec num)) (partition-all num sq)))))

(defcheck solution-7e161e41
  (fn [s n]
    (for [[k v] (map-indexed vector s)
          :when (not (= 0 (mod (+ k 1) n)))]
      v)))

(defcheck solution-7e2bd78e
  (fn [s n]
    (loop [left [] right s]
      (if (empty? right)
        left
        (recur (concat left (take (dec n) right)) (drop n right))))))

(defcheck solution-7e3c0f5d
  (fn a [xs x] (if (empty? xs) xs (concat (take ( dec x) xs) (a (drop x xs) x)))))

(defcheck solution-7eace9d5
  (fn [s d]
    (flatten (map #(if (= d (count %)) (drop-last %) %) (partition d d [] s)))))

(defcheck solution-7ef55b86
  (fn dropnth [items n]
    (if (empty? items)
      '()
      (concat (take (- n 1) items) (dropnth (drop n items) n)))))

(defcheck solution-7fed214a
  (fn [xs n]
    (for [[x i] (map vector xs (iterate inc 1)) :when (not= 0 (mod i n))] x)))

(defcheck solution-8057df8f
  #(mapcat (fn [a]
             (if (= (count a) %2)
               (butlast a)
               a))
     (partition-all %2 %1)))

(defcheck solution-806307d7
  (fn [coll n]
    (->> (map-indexed (fn [idx item]
                        [(mod (inc idx) n) item])
           coll)
      (remove (fn [[idx item]] (zero? idx)))
      (map second))))

(defcheck solution-806c6616
  #(mapcat (partial take (dec %2)) (partition-all %2 %)))

(defcheck solution-80764bd6
  (fn d [s n]
    (if (empty? s)
      ()
      (concat (take (dec n) s) (d (drop n s) n)))))

(defcheck solution-80ea8c04
  #(loop [xs %1 n 1 rs [] index %2] (if (empty? xs) rs (recur (rest xs) (if (= n index) 1 (inc n)) (if (= n index) rs (conj rs (first xs))) index))))

(defcheck solution-812c5e0c
  (fn [x n]
    (for [x (map-indexed (fn [x i] [x i]) x)
          :when (not (= 0 (rem (inc (x 0)) n )))]
      (x 1))))

(defcheck solution-813dc3ff
  (fn [coll n]
    (mapcat #(if (= (count %) n) (butlast %) %)
      (partition-all n coll))))

(defcheck solution-81608387
  (fn [c n] (flatten (partition-all (dec n) n c))))

(defcheck solution-81650223
  #(mapcat (fn [x] (take (dec %2) x)) (partition-all %2 %)))

(defcheck solution-81e048a0
  (fn [coll n]
    (map
      #(last %)
      ; create ([inx num] ...)
      (filter
        #((complement zero?) (rem (+ 1 (first %)) n))
        (map-indexed vector coll)))))

(defcheck solution-8214b921
  (fn [arr idx]
    (keep-indexed #(when-not (zero? (rem (inc %) idx)) %2) arr)))

(defcheck solution-82ad69f3
  (fn  [coll n]
    (->> coll
      (partition-all n)
      (mapcat #(take (dec n) %)))))

(defcheck solution-830ed08d
  (fn [s k]
    (for [[x i] (map vector s (cycle (range k))) :when (not= i (dec k))] x)))

(defcheck solution-832d01da
  (fn drop-nth [s n]
    (when
     (seq s)
      (lazy-cat
        (take (dec n) s)
        (drop-nth
          (drop n s)
          n)))))

(defcheck solution-833f4b41
  (let [dropit
        (fn [coll n counter result]
          (if (nil? coll)
            result
            (if (= counter n)
              (recur (next coll) n 1 result)
              (recur (next coll) n (inc counter) (conj result (first coll)))
              )
            )
          )]
    (fn [coll n] (dropit coll n 1 []))
    ))

(defcheck solution-83ac4b0b
  (fn [xs n]
    (let [ys (partition-all n xs)
          zs (map (partial take (- n 1)) ys)
          as (apply concat zs)]
      as)))

(defcheck solution-83e43ffc
  (fn [a b]
    (remove nil?
      (map-indexed
        #(if (< 0 (mod (inc %) b)) (identity %2)) a))))

(defcheck solution-83f48ffb
  ;#(flatten (partition (dec %2) %2 [] %))
  #(apply concat (partition-all (dec %2) %2 %)))

(defcheck solution-84a31280
  (fn [s n] (let [index (filter #(not= (rem % n) 0) (iterate inc 1)) len (count s)] (map #(s (- % 1)) (take (- len (int (/ len n))) index)))))

(defcheck solution-84caa3ac
  (fn nthdrop [c n]
    (cond
      (empty? c) '()
      :else
      (let [ret (take (dec n) c)]
        (concat ret (nthdrop (nthrest c n) n))))))

(defcheck solution-851837e6
  (fn foo [coll n]
    (if (> (count coll) 0)
      (concat (take (dec n) coll) (foo (drop n coll) n))
      '())))

(defcheck solution-8560344c
  #(keep-indexed (fn [idx item] (when-not (zero? (mod (inc idx) %2)) item)) %1))

(defcheck solution-85f76e8a
  (fn [c n] (keep-indexed #(if (< (mod %1 n) (dec n)) %2) c)))

(defcheck solution-861688cb
  (fn d [xs n](when (seq xs) (lazy-cat (take (dec n) xs) (d (drop n xs) n)))))

(defcheck solution-864ae24d
  (fn [coll n] (loop [x (- n 1) c coll new-c []] (if (empty? c) new-c (if (= x 0) (recur (- n 1) (rest c) new-c) (recur (- x 1) (rest c) (conj new-c (first c))))))))

(defcheck solution-86989ebe
  (fn d [x y] (loop [in x, out []]
                (if (empty? in)
                  out
                  (recur (drop y in) (concat out (take (- y 1) in)))))))

(defcheck solution-86e58714
  (fn nthss [s n]
    (when (seq s)
      (concat
       (take (dec n) s)
       (nthss (drop n s) n)))))

(defcheck solution-8702ccc1
  (fn [s n] (mapcat #(if (= n (count %)) (drop-last %) % ) (partition-all n s))))

(defcheck solution-883b56f4
  (fn [coll n] (remove nil? (map-indexed #(when (not= (rem (inc %1) n) 0) %2) coll))))

(defcheck solution-88b56c06
  (fn [c i]
    (loop [n [] o c]
      (if (empty? o) n (recur (into n (take (dec i) o)) (drop i o))))))

(defcheck solution-88b6da4
  (fn f [l n](if (empty? l) (list) (concat (take (- n 1) l) (f (drop n l) n) ))))

(defcheck solution-88bc7741
  (fn [s d] (keep-indexed (fn [i e] (when (pos? (rem (inc i) d)) e)) s)))

(defcheck solution-88e4b297
  (fn [coll n]
    (map #(nth coll %) (filter #(not (= 0 (rem (+ 1 %) n))) (range (count coll))))))

(defcheck solution-88efb882
  #(first (reduce (fn [[l n ] a]
                    (if (zero? (rem n %2))
                      [l (inc n)]
                      [(conj l a) (inc n)]))
            [[] 1] %1)))

(defcheck solution-88f0c7b2
  (fn [v n] (->> v
              (zipmap (range))
              (into [])
              (sort)
              (remove #(= (- n 1) (rem (first %) n)))
              (map second)
              )))

(defcheck solution-89295803
  (fn my_drop-nth [l n] (if (< (count l) n) l (concat (take (dec n) l) (my_drop-nth (drop n l) n)))))

(defcheck solution-894ad370
  (fn[coll itm]
    (map second
      (filter #(not (zero? (mod (inc (first %)) itm)))
        (map vector (range) coll)))))

(defcheck solution-89c055a2
  (fn [s n] (mapcat #(if (= (count %) n) (drop-last %) %)
              (partition-all n s))))

(defcheck solution-89dfb3e5
  (fn [a b](mapcat #(take (dec b) %) (partition-all b a))))

(defcheck solution-89e1dbe7
  (fn [l n]
    (filter (fn [it]
              (not (= (mod (inc (.indexOf l it)) n) 0))) l)))

(defcheck solution-89ec4442
  (fn drop-every-nth
    ([s n] (drop-every-nth s n 1))
    ([s n i]
     (if (empty? s)
       '()
       (let [tail (lazy-seq (drop-every-nth (rest s) n (inc i)))]
         (if (= (rem i n) 0)
           tail
           (cons (first s) tail)))))))

(defcheck solution-8aac30bb
  (fn [col dropnth] (map last (filter (fn [j] (< 0 (mod (inc (first j)) dropnth))) (map-indexed (fn [idx i] [idx i]) col)))))

(defcheck solution-8b41deb7
  (fn [xs idx]
    (map second (filter #(not= (rem (inc (first %))  idx) 0) (map-indexed vector xs)))
    ))

(defcheck solution-8b44e81b
  #(vec (apply concat (partition-all (dec %2) %2 %1))))

(defcheck solution-8b80236e
  #(for [x (range (count %)) :when (< 0 (rem (inc x) %2))] (% x)))

(defcheck solution-8bb1ff97
  (fn [a b]
    (reduce concat (partition (dec b) b [] a))))

(defcheck solution-8bd17f26
  (fn [coll n]
    (keep-indexed
      (fn [index item]
        (if (not= 0 (rem (inc index) n)) item))coll)))

(defcheck solution-8c445f11
  (fn [c x] (keep-indexed #(if (not= (rem %1 x) (dec x)) %2) c)))

(defcheck solution-8d147118
  (fn dropn [lst n]
    (letfn [(zip [A B] (map list A B))
            (unzip [C] (map first C))
            (pred [i] (not (zero? (mod (inc (second i)) n))))]
      (unzip (filter pred (zip lst (range)))))))

(defcheck solution-8d3cb8aa
  (fn drop-nth [coll n]
    (when (not (empty? coll))
      (lazy-cat (take (- n 1) coll) (drop-nth (drop n coll) n)))))

(defcheck solution-8d8b0cc
  #(mapcat (fn [x] (if (= %2 (count x)) (drop-last x) x)) (partition-all %2 %1)))

(defcheck solution-8d9265c6
  ; using keep-indexed is a cheat (but it's faster than my solution)
  ;(fn [s n] (keep-indexed #(if (> (mod (inc %1) n) 0) %2) s))
  ;
  ;This is my solution - *sigh*
  (fn [s n] (map (fn [x] (nth x 0)) (filter (fn [x] (nth x 1)) (map (fn [x y] [x y]) s (map #(not (zero? (mod % n))) (iterate inc 1)))))))

(defcheck solution-8ddab95f
  #(reduce concat (partition-all (dec %2) %2 %1)))

(defcheck solution-8e67a2ce
  (fn [x y]
    (flatten
      (map
        (fn [n] (take (- y 1) n))
        (partition-all y x)))))

(defcheck solution-8eb87ed1
  (fn [s n]
    (for [i (range (count s))
          :when (not (= (rem (+ 1 i) n) 0))]
      (nth s i ))))

(defcheck solution-8ebb6e29
  (fn [s n] (for [[i v] (map vector (iterate inc 1) s) :when (not (zero? (mod i n))) ] v)))

(defcheck solution-8ec351b0
  (fn [col n]
    (->> col
      (partition-all n)
      (mapcat (partial take (dec n))))))

(defcheck solution-8ee02379
  (fn dropnth [s n]
    (if (empty? s) []
                   (concat (take (dec n) s) (dropnth (nthrest s n) n)))))

(defcheck solution-8f379025
  (fn [xs n]
    (loop [ls xs acc [] m 1]
      (if (seq ls)
        (if (= (mod m n) 0) (recur (rest ls) acc (inc m))
                            (recur (rest ls) (conj acc (first ls)) (inc m)))
        acc))))

(defcheck solution-8f487bd1
  (fn dropnth [lst n]
    (filter #(not (nil? %))
      (map-indexed (fn [idx elem] (if (= 0 (mod (inc idx) n)) nil elem)) lst))))

(defcheck solution-8fa195aa
  (fn [s i] (flatten (partition (dec i) i [] s))))

(defcheck solution-900a03dd
  (fn [s n]
    (reduce concat
      (map
        #(take (dec n) %)
        (partition-all n s)
        )
      )
    ))

(defcheck solution-901e7f6
  (fn [col step]
    (keep-indexed
      (fn [idx item]
        (if-not (= 0 (mod (+ idx 1) step)) item)) col)))

(defcheck solution-907f5275
  (fn [coll n]
    (->> (map-indexed vector coll)
      (remove #(zero? (mod (inc (first %)) n)))
      (map second))))

(defcheck solution-911b62c2
  (fn f [c n]
    (flatten
      (map #(take (dec n) %) (partition-all n c)))))

(defcheck solution-9132127f
  (fn [s x]
    (keep-indexed
      (fn [i a] (when (> (mod (inc i) x) 0) a))
      s)))

(defcheck solution-91da1570
  #(filter identity (apply concat (map butlast (partition %2 %2 (repeat nil) %1)))))

(defcheck solution-925ee57c
  (fn [coll n]
    (keep-indexed (fn [i x]
                    (when (not= 0 (mod (inc i) n))
                      x))
      coll)))

(defcheck solution-92ac7e95
  (fn [vec n]
    (mapcat #(take (dec n) %) (partition-all n vec))
    ))

(defcheck solution-92b17eb6
  (fn [s n] (flatten (map-indexed #(if (not= 0 (mod (inc %) n)) [%2] []) s))))

(defcheck solution-92f36a3b
  (fn [s n] (loop [[h & t] s acc [] c n] (let [newacc (if (= c 1) acc (conj acc h))] (if (= t nil) newacc (recur t newacc (if (= c 1) n (- c 1))))))))

(defcheck solution-930755dd
  (fn [l n] (loop [result [] l l i 1]
              (if (empty? l) result
                             (recur (if (= i 0) result (conj result (first l)))
                               (rest l)
                               (mod (+ i 1) n))))))

(defcheck solution-934e389c
  (fn ! ([x y] (! x y y))    ([x y z]   (if (empty? x) nil  (if (= z 1) (! (rest x) y y)  (concat [(first x)] (! (rest x) y (dec z))) )  )      ) ))

(defcheck solution-93a72c2b
  #(keep-indexed (fn[i x] (if (zero? (mod (inc i) %2)) nil x)) %1))

(defcheck solution-93aa305a
  (fn [x n] (keep-indexed (fn [idx el] (if (= (mod idx n) (dec n)) nil el)) x)))

(defcheck solution-93c31918
  (fn [xs n]
    (mapcat #(if (= (count %) n) (drop-last %) %) (partition-all n xs))))

(defcheck solution-941c2e74
  #(keep-indexed
     (fn[i v] (when-not (= (rem (+ i 1) %2) 0) v)) %))

(defcheck solution-941cc8fe
  (fn drop-nth [sq vl]
    (keep-indexed #(if (not (zero? (rem (inc %1) vl))) %2) sq)))

(defcheck solution-9521b7e6
  (fn [seqs n]
    (filter identity (flatten (map-indexed #(if (not= (mod (+ %1 1) n) 0) %2) seqs)))))

(defcheck solution-953b876
  (fn [s n]
    (mapcat butlast (partition n n [:x] s))))

(defcheck solution-953ebb14
  (fn [v n] (keep-indexed #(when (pos? (mod (inc %) n)) %2) v)))

(defcheck solution-95d236a
  (fn [s n]
    (mapcat
      (partial take (dec n))
      (partition-all n s))))

(defcheck solution-96135d54
  (fn filternth [xs n]
    (filter #(not (nil? %))
      (map
        (fn getsnd [a b] (if a b nil))
        (map
          #(not= 0 (mod % n))
          (range 1 (inc (count xs))))
        xs))))

(defcheck solution-96ecaf46
  #(loop [remaining %1
          accum []]
     (cond (empty? remaining) accum
           :default (recur (drop %2 remaining)
                      (into accum (take (dec %2) remaining))))))

(defcheck solution-9769caf9
  (fn [s n]
    (loop [rr [] ss s nn n]
      (if (empty? ss)
        rr
        (if (= 1 nn)
          (recur rr (rest ss) n)
          (recur (conj rr (first ss)) (rest ss) (dec nn)))))))

(defcheck solution-9783c4f6
  (fn drop-n
    ([s n] (drop-n s n 1))
    ([s n c]
     (cond
       (empty? s) '()
       (= 0 (mod c n)) (drop-n (rest s) n (inc c))
       :else
       (cons (first s) (drop-n (rest s) n (inc c)))))))

(defcheck solution-97858eb8
  (fn [v n]
    (keep-indexed #(if (= 0 (mod (inc %) n)) nil %2) v)))

(defcheck solution-979a637c
  (fn [s n]
    (mapcat #(when-not (= 0
                         (mod (inc (first %)) n)
                         ) (rest %))
      (map vector (range) s))))

(defcheck solution-97e20f6f
  #(map second (filter (fn [[k v]] (or (= 0 k) (not= 0 (mod (inc k) %2)))) (map-indexed (fn [k v] [k v]) %1))))

(defcheck solution-980feb3a
  (fn [coll n]
    (for [x (range (inc (count coll)))
          :when (not= (rem x n) 0)]
      (get coll (dec x)))))

(defcheck solution-987d8470
  (fn [s n]
    (mapcat #(take (dec n) %) (partition-all n s))))

(defcheck solution-98c01bc7
  (fn [l n]
    (loop [out [] i 1 l2 l]
      (if (empty? l2)
        out
        (recur (if (zero? (mod i n)) out (conj out (first l2))) (inc i) (rest l2))))))

(defcheck solution-99d47f78
  #(letfn [(worker [l c n]
             (if (empty? l)
               n
               (if (= c 1)
                 (recur (rest l) %2 n)
                 (recur (rest l) (dec c) (conj n (first l))))))]
     (worker %1 %2 [])))

(defcheck solution-9a7b4770
  (fn [lst n]
    (keep-indexed
      #(when (not= (mod (inc %1) n) 0) %2)
      lst)))

(defcheck solution-9a9b1772
  (fn [coll n]
    (map second
      (filter #(not= (mod (first %) n) (dec n))
        (map vector (range) coll)))))

(defcheck solution-9ab68a12
  (fn [c n] (keep-indexed (fn [i v] (if (= 0 (mod (inc i) n)) nil v) ) c)))

(defcheck solution-9ad5f0b1
  (fn [seq1 x]
    (loop [result [] elements seq1 index 1]
      (if (empty? elements)
        result
        (if (= index x)
          (recur result (rest elements) 1)
          (recur (conj result (first elements)) (rest elements) (inc index))
          )
        )
      )
    ))

(defcheck solution-9b1e8ebe
  (fn [s v] (remove nil? (map-indexed #(if (zero? (rem (inc %1) v)) nil %2) s))))

(defcheck solution-9b488ed
  (fn [li n]
    (loop [ l li res [] ]
      (if (empty? l)

        res
        (recur (drop  n l)  (concat res (take (dec n) l  )) )
        )

      )
    ))

(defcheck solution-9ba81493
  (fn drop-nth [coll n]
    (if (empty? coll)
      '()
      (concat
       (take (dec n) coll)
       (drop-nth (drop n coll) n)))))

(defcheck solution-9bfac450
  (fn drop-every-nth
    ([coll n]
     (drop-every-nth coll n 1))
    ([coll n i]
     (lazy-seq
       (when-let [s (seq coll)]
         (if (= i n)
           (drop-every-nth (rest s) n 1)
           (cons (first s) (drop-every-nth (rest s) n (inc i)))))))))

(defcheck solution-9cb06a2e
  (fn [seq n]
    (apply concat (partition-all (- n 1) n seq))))

(defcheck solution-9cb2bce0
  (fn dn [s n]
    (if (< (count s) n) s
                        (concat (take (dec n) s) (dn (drop n s) n)))))

(defcheck solution-9d176874
  (fn [s n]
    (loop [s   s
           cnt 1
           acc []]
      (cond
        (empty? s) acc
        (zero? (rem cnt n)) (recur (rest s) (inc cnt) acc)
        :else (recur (rest s) (inc cnt) (conj acc (first s)))))))

(defcheck solution-9d306c7
  (fn f ([x n]
         (f x n 1 []))
    ([x n c r]
     (cond (empty? x) r
           (= 0 (mod c n)) (recur (rest x) n (inc c) r)
           :else (recur (rest x) n (inc c) (conj r (first x)))))))

(defcheck solution-9d33dfc1
  (fn drop-nth [a-seq n]
    (mapcat  #(take (dec n) %) (partition-all n a-seq))
    ))

(defcheck solution-9d53c91c
  #(mapcat drop-last (partition %2 %2 [nil] %1)))

(defcheck solution-9d876f5e
  (fn
    [col n]
    (lazy-seq
      (loop [col col, a []]
        (if (empty? col)
          a
          (recur (drop n col) (concat a (take (- n 1) col))))))))

(defcheck solution-9d884a3e
  (fn [xs n]
    (map first
      (filter second
        (map (fn [x y] [x (not (zero? (rem y n)))])
          xs
          (drop 1 (range)))
        ))))

(defcheck solution-9f0b0ae1
  #(->> %1
     (partition-all %2)
     (map (partial take (dec %2)))
     flatten))

(defcheck solution-9f43a751
  (fn [s n] (keep-indexed #(if (not= (mod (inc %) n) 0) %2) s)))

(defcheck solution-9fd4c34e
  #(flatten (partition (dec %2) %2 nil %)))

(defcheck solution-a01b58ea
  (fn [x y] (reduce (fn [acc i] (if (= (mod (+ 1 (key i)) y) 0) acc (conj acc (val i)))) [] (conj {} (zipmap (range (count x)) x)))))

(defcheck solution-a01f904b
  #(apply concat (partition (dec %2) %2 [] %)))

(defcheck solution-a09f01e5
  (fn [coll n]
    (loop [i 1
           [hd & tl] coll
           final []]
      (let [do-drop? (= i n)
            final (if do-drop?
                    final
                    (conj final hd))]
        (if (empty? tl)
          final
          (recur (if do-drop? 1 (inc i)) tl final))))))

(defcheck solution-a0a0b424
  (fn [coll n] (mapcat (partial take (dec n)) (partition-all n coll))))

(defcheck solution-a11680df
  (fn [l c]
    (flatten (map #(take (dec c) %) (partition-all c l)))))

(defcheck solution-a139b903
  (fn [coll drp]
    (last (reduce (fn [[c xs] x]
                    (let [c (mod (dec c) drp)]
                      [c (if (zero? c) xs (conj xs x))]))
            [0 []] coll))))

(defcheck solution-a15a3dd7
  (fn drop-every-n [col n]
    (keep-indexed
      (fn [index item]
        (if
         (not= 0 (mod (inc index) n))
          item
          nil))
      col)))

(defcheck solution-a162671f
  (fn [lst n]
    (keep-indexed #(if (not= (dec n) (mod %1 n)) %2) lst)))

(defcheck solution-a1a47375
  (fn [seq n]
    (loop [pos 0 ret []]
      (if (> pos (count seq)) ret
                              (if (= (mod pos n) 0)
                                (recur (+ pos 1) ret)
                                (recur (+ pos 1) (conj ret (nth seq (- pos 1))))
                                )))))

(defcheck solution-a1de6ce
  (fn [list n]
    (loop [l list n n ret '() c 1]
      (if (= (count l) 0)
        (reverse ret)
        (if (= c n)
          (recur (rest l) n ret 1)
          (recur (rest l) n (cons (first l) ret) (+ c 1))
          )
        )
      )
    ))

(defcheck solution-a2950c7a
  #(mapcat (partial take (- %2 1))
     (take-while (comp not empty?)
       (iterate (partial drop %2) %))))

(defcheck solution-a2a00a95
  (fn [c n] (mapcat identity (partition-all (dec n) n c))))

(defcheck solution-a2ad563a
  (fn [l n]
    (->> (map-indexed vector l)
      (filter (fn [[i v]] (not (zero? (mod (inc i) n)))))
      (map second))))

(defcheck solution-a2fe1495
  #(mapcat ( partial take (- %2 1)) (  partition-all %2 %1)))

(defcheck solution-a3248a88
  (fn [xs n] (let [ixs (into {} (map vec (partition 2 (interleave (range 1 (+ 1(count xs))) xs))))]
               (vec (vals (filter #(< 0 (mod (key %) n)) ixs))))))

(defcheck solution-a3931677
  (fn [c v] (keep-indexed #(if (not= 0 (rem (inc %1) v)) %2) c)))

(defcheck solution-a4471731
  #(apply concat (partition (dec %2) %2 nil %1)))

(defcheck solution-a4b2357d
  ;(fn [coll n]
  ;  (let [[firsts lasts] (split-at (dec n) coll)]
  ;    (concat firsts
  ;      (mapcat rest (partition-all n lasts)))))

  (fn [coll n]
    (map second
      (remove #(zero? (mod (inc (first %)) n))
        (map-indexed list coll)))))

(defcheck solution-a502d6f9
  (fn [s n]
    (mapcat
      #(if (= n (count %1)) (drop-last %1) %1)
      (partition-all n s))))

(defcheck solution-a5622455
  #((comp flatten partition) (dec %2) %2 [] %1))

(defcheck solution-a58674a1
  (fn [c i] (keep-indexed #(if (> (mod (+ 1 %) i) 0) %2) c)))

(defcheck solution-a5ffb62e
  #(for [n (range)
         :let [x (get % n)]
         :while (not= x nil)
         :when (> (rem (+ n 1) %2) 0)] x))

(defcheck solution-a66fd55d
  (fn [s n]
    (keep-indexed
      #(if (not= (mod %1 n) (- n 1)) %2)
      s)))

(defcheck solution-a6956f0d
  (fn ([lst cof]
       (map first
         (remove #(= 0 (mod (inc (last %)) cof))
           (map list lst (range)))))))

(defcheck solution-a6ebadcc
  #(filter identity (flatten (map butlast (partition %2 %2 (repeat nil) %1)))))

(defcheck solution-a7359ce1
  (fn [c n] (keep-indexed
              #(if (not= 0 (rem (+ 1 %) n)) %2) c)
    ))

(defcheck solution-a74aa970
  (fn [seq n]
    (map (fn [[a b]] b)
      (filter (fn [[a b]] (pos? (rem (inc a) n)))
        (map-indexed list seq)))))

(defcheck solution-a7af0c2d
  (fn [v n]
    (loop [v v, n n, i 0, e (int (/ (count v) n))]
      (if (= i e)
        v
        (let [n-c (- (* n (inc i)) i)]
          (if (= (count v) n-c)
            (subvec v 0 (dec n-c))
            (recur (apply conj (subvec v 0 (dec n-c)) (subvec v n-c))
              n
              (inc i)
              e)))))))

(defcheck solution-a8039d7a
  (fn f [coll n]
    (keep-indexed (fn [i e] (if (not= 0 (rem (inc i) n)) e)) coll)))

(defcheck solution-a84d1eea
  (fn [xs n] (filter identity (map-indexed #(if-not (= (mod (inc %1) n) 0) %2) xs))))

(defcheck solution-a87fa399
  (fn [coll n]
    (->> (partition-all n coll)
      (map (partial take (dec n)))
      (flatten))))

(defcheck solution-a90e28e2
  (fn [coll n]
    (flatten
      (concat
       (map #(drop-last %) (partition n coll))
       (take-last (rem (count coll) n) coll)))))

(defcheck solution-a91e4412
  #(flatten (partition (- %2 1) %2 [] %1)))

(defcheck solution-a993f87a
  (letfn [(f [s n] (if (seq s) (concat (take (dec n) s) (f (drop n s) n))))] f))

(defcheck solution-a9ab3b04
  (fn [ls d]
    (let [ indexed (map vector ls (range (count ls))) ]
      (map first (filter (fn [[_ id]] (not (zero? (mod (inc id) d)))) indexed)))))

(defcheck solution-a9abfe33
  #(flatten (partition-all (dec %2) %2 %1)))

(defcheck solution-a9b0333a
  (fn [x y]
    (flatten (map #(take (dec y) %) (partition-all y x)
               ))))

(defcheck solution-aae017f0
  (fn [a b] (map #(second %) (filter #(not= (mod (first %) b) 0) (map-indexed (fn [idx itm] [(inc idx) itm]) a)))))

(defcheck solution-ab162ed8
  (fn [coll n]
    (loop [coll coll
           i n
           r '()]
      (if (empty? coll)
        (reverse r)
        (if (= i 1)
          (recur (rest coll) n r)
          (recur (rest coll) (- i 1) (conj r (first coll))))))))

(defcheck solution-ab409e3d
  (fn [coll n] (mapcat #(if (= n (count %)) (drop-last %) %) (partition n n () coll))))

(defcheck solution-ab40f9f4
  (fn[l,n] (keep-indexed (fn[i,v] (if (= (mod (+ 1 i) n) 0) nil v)) l)))

(defcheck solution-abb608f
  (fn drop-every-nth-item [c n]
    (first (reduce

             (fn [[a,i] x]
               (if-not (= n i)
                 [(conj a x), (inc i)]
                 [a,1]))

             [[],1] c))))

(defcheck solution-abdcb2af
  (fn number41 [xs n]
    (mapcat #(take (dec n) %) (partition-all n xs))))

(defcheck solution-abec3743
  (fn [a b] (keep-indexed #(if-not (= (mod (+ %1 1) b) 0) %2)
              a)))

(defcheck solution-ac1883f4
  (fn [xs n] (apply concat (partition-all (dec n) n xs))))

(defcheck solution-ac3d713b
  (fn dropn [coll n]
    (map second
      (filter
        #(not= 0 (mod (inc (first %)) n))
        (map-indexed vector coll)))))

(defcheck solution-ac3fd48e
  (fn drop-n [seq n]
    (->> seq
      (partition-all n)
      (map (fn [grp]
             (if (= n (count grp)) (->> grp reverse rest reverse) grp)))
      (apply concat))))

(defcheck solution-ac4a51bb
  (fn drop-n2 [xs n]
    (mapcat #(if (= 0 (mod (second %) n)) [] (vector (first %))) (map #(vector %1 %2) xs (iterate inc 1)))))

(defcheck solution-ac4a91e9
  (fn [x y]
    (keep-indexed
      #(if-not (= (mod (inc %1) y) 0) %2) x)))

(defcheck solution-ac5f0f1a
  #(remove
     nil? (map-indexed
            (fn [i x]
              (when-not (zero? (mod (inc i) %2)) x)) %1)))

(defcheck solution-acf37cd2
  (fn [coll n]
    (mapcat #(if (> n (count %))
               %
               (drop-last %)) (partition-all n coll))))

(defcheck solution-ad0eea3b
  #(flatten (partition (dec %2) %2 nil %1)))

(defcheck solution-ade2b4a2
  (fn [s n] (keep-indexed #(when-not (= (dec n) (mod %1 n)) %2) s)))

(defcheck solution-adf1d98
  (fn [s nth]
    (map second (filter #(not= 0 (rem (inc (first %)) nth))
                  (map-indexed (fn [idx itm] [idx itm]) s)))))

(defcheck solution-af660c64
  (fn f[l x]
    (filter
      (fn [el]
        (not (= (mod (inc (.indexOf l el)) x) 0)))
      l)))

(defcheck solution-af7a3b33
  #(->> %1
     (partition-all (dec %2) %2)
     (apply concat)))

(defcheck solution-afeaef81
  (fn [xs n]
    (apply concat (partition (dec n) n [] xs))))

(defcheck solution-affb460d
  (fn drop-nth [x n]
    (if (< (count x) n)
      x
      (concat (take (- n 1) x) (drop-nth (nthnext x n) n)))))

(defcheck solution-b008f7c0
  (fn [vs n]
    (->> vs
      (map-indexed (fn [i v] [(inc i) v]))
      (remove (fn [[i v]] (-> i (rem n) zero?)))
      (map last))))

(defcheck solution-b0b00f2b
  (fn [se n] (mapcat butlast (partition n n [nil] se))))

(defcheck solution-b0e757de
  (fn drop-nth
    ([l n] (drop-nth l n [] 1))
    ([l n resp con]
     (if (= l [])
       resp
       (if (= n con)
         (drop-nth (rest l) n resp 1)
         (drop-nth (rest l) n (conj resp (first l)) (inc con)))))))

(defcheck solution-b1318541
  (fn [ls x]
    (loop [sq ls
           i 1
           return []]
      (if (nil? (first sq))
        return
        (if (= i x)
          (recur (rest sq)
            1
            return)
          (recur (rest sq)
            (inc i)
            (conj return (first sq))))))))

(defcheck solution-b15eb411
  (fn
    [seq n]
    (loop [iterator 1 newseq []]
      (if (<= iterator (count seq) )
        (if (not= 0 (rem iterator n))
          (recur (inc iterator) (conj newseq (nth seq (dec iterator)) ) )
          (recur (inc iterator) newseq)
          )
        newseq
        )
      )))

(defcheck solution-b1da1987
  (fn [x n] (apply concat (partition-all (dec n) n x))))

(defcheck solution-b1f79498
  #(->> (partition %2 %2 nil %1)
     (map (fn [x] (if (>= (count x) %2)
                    (drop-last x)
                    x)))
     flatten))

(defcheck solution-b41d635c
  (fn [s n]
    (flatten (partition-all (- n 1) n s))))

(defcheck solution-b4b4a69f
  (fn [coll n]
    (keep-indexed #(if (not= 0 (rem (inc %1) n)) %2 nil) coll)))

(defcheck solution-b4badefe
  (fn [s n]
    (reduce
      #(concat %1 (take (dec n) %2))
      []
      (partition n n nil s)
      )
    ))

(defcheck solution-b50a7e1f
  #(flatten (partition-all (dec %2) %2 %1)))

(defcheck solution-b513570e
  (fn [xs n]
    (let [f (fn [[acc i] x]
              (if (= i n) [acc 1]
                          [(conj acc x) (inc i)]))]
      (first (reduce f [[] 1] xs)))))

(defcheck solution-b682bfc1
  #(mapcat (fn [p] (if (= %2 (count p)) (butlast p) p))(partition-all %2 %)))

(defcheck solution-b69c9b84
  (fn [seq count]
    (map first
      (filter second
        (map list
          seq
          (flatten (repeat (concat (repeat (- count 1) true)
                                   (list false)))))))))

(defcheck solution-b7669c68
  #(mapcat (fn [s] (take (dec %2) s)) (partition-all %2 %)))

(defcheck solution-b893e040
  (fn [xs n]
    (letfn [(iter [xs k]
              (if (seq xs)
                (if (zero? k)
                  (iter (rest xs) (dec n))
                  (cons (first xs) (iter (rest xs) (dec k))))
                '()))]
      (iter xs (dec n)))))

(defcheck solution-b8e9a29d
  (fn [coll n]
    (keep-indexed
      (fn [i v] (if
                 (= 0 (mod (inc i) n))
                  nil
                  v)) coll)))

(defcheck solution-b9069935
  (fn drop-every-nth [coll n]
    (loop [position (- n 1)
           counter 1
           coll coll]
      (if (>= (* counter position) (count coll))
        coll
        (recur position (inc counter) (remove #{(nth coll (* position counter))} coll))))))

(defcheck solution-b9eac6c9
  (fn [v nm] (keep-indexed #(if (not= (- nm 1) (mod % nm)) %2) v)))

(defcheck solution-ba00ceac
  (fn [coll n]
    (filter identity (map-indexed (fn [i x]
                                    (if (zero? (mod (inc i) n))
                                      false
                                      x))
                       coll))))

(defcheck solution-baec8229
  (fn
    [aseq n]
    (let [n-1 (dec n)]
      (loop [lseq aseq acc []]
        (if (> n (count lseq))
          (concat acc lseq)
          (recur (drop n lseq) (concat acc (take n-1 lseq))))))))

(defcheck solution-bc29ee12
  (fn [seq n]
    (map first
      (remove #(= (mod (second %) n) 0)
        (map #(list %1 %2) seq (iterate inc 1))))))

(defcheck solution-bc4c4724
  (fn drop-nth [s n]
    (mapcat #(take (dec n) %) (partition-all n s))))

(defcheck solution-bcde4084
  #(mapcat (fn [coll]
             (if (= %2 (count coll))
               (drop-last coll)
               coll)) (partition-all %2 %1)))

(defcheck solution-bce05fdd
  (fn dropnth [x y]
    (letfn [(dn [l n cur]
              (if (empty? l)
                l
                (if (= n cur)
                  (dn (rest l) n 1)
                  (cons (first l) (dn (rest l) n (+ 1 cur)))
                  )
                )
              )]
      (dn x y 1)
      )
    ))

(defcheck solution-bcf0481d
  #(mapcat (partial take (dec %2)) (partition-all %2 %1)))

(defcheck solution-bd14fc81
  (fn [coll n]
    (keep-indexed #(if (not= 0 (rem (inc %1) n)) %2) coll)))

(defcheck solution-bd734efa
  (fn [col n] (map first (filter #(< 0 (mod (second %) n)) (map list col (iterate inc 1))))))

(defcheck solution-bda0be6f
  (fn [l n]
    (map #(nth l % nil)(remove #(= 0 (mod (inc %) n))(range (count l))))))

(defcheck solution-bdce4332
  (fn [a n]
    (loop [l a s []]
      (let [m (take n l) x (drop n l)]
        (if (= n (count m))
          (recur x (concat s (butlast m)))
          (concat s m)
          )
        )

      )
    ))

(defcheck solution-bdd867e
  (fn deni
    [x n]
    (filter #(not (zero? (mod (inc (.indexOf x %)) n))) x)))

(defcheck solution-be9ce2a
  #(mapcat identity (partition-all (dec %2) %2 %)))

(defcheck solution-bef079c6
  #(mapcat (fn [s] (take (dec %2) s)) (partition-all %2 %1)))

(defcheck solution-bf2dbdf
  (fn [seq n]
    (loop [i 1 remain seq ans []]
      (if (empty? remain)
        ans
        (if (= 0 (rem i n))
          (recur (inc i) (rest remain) ans)
          (recur (inc i) (rest remain) (conj ans (first remain)))
          )
        )
      )
    ))

(defcheck solution-bfd6d94d
  (fn [list n]
    (loop [acc []
           i 1
           left list]
      (if (seq left)
        (recur (if (zero? (mod i n))
                 acc
                 (conj acc (first left)))
          (inc i)
          (rest left))
        acc))))

(defcheck solution-bfdddfba
  (fn [s n] (->> (map list (range) s) (filter #(-> % first inc (mod n) ((complement zero?)))) (mapcat next))))

(defcheck solution-c01cc215
  (fn [coll n] (
                 filter #(not=
                           (mod (inc (.indexOf coll %1) ) n) 0) coll)))

(defcheck solution-c04300bb
  (fn D [l n]
    (loop [c 1 l l result []]
      (cond (empty? l) result
            (= c n) (recur 1 (rest l) result)
            :else (recur (inc c) (rest l) (conj result (first l)))))))

(defcheck solution-c08f9397
  #(apply concat (partition (dec %2) %2 [] %1)))

(defcheck solution-c0a6aeac
  (fn [l n]
    (filter #(not(nil? %))
      (map-indexed
        (fn [i x]
          (if (= (mod (+ i 1) n) 0)
            nil x))
        l))))

(defcheck solution-c0f9b463
  (fn dp [x n]
    (if (= [] x) []
                 (concat (take (dec n) x) (dp (drop n x) n)))))

(defcheck solution-c14936e0
  (fn [s nn]
    (letfn [(recu [left seq n]
              (cond (= (count seq) 0) left
                    (< (count seq) n) (concat left seq)
                    :t (recu (concat left (butlast (take n seq)))
                         (drop n seq) n)))]
      (recu [] s nn))))

(defcheck solution-c1a15d64
  (fn [x y]
    (let [z (partition-all y x)]
      (mapcat #(take (dec y) %) z))))

(defcheck solution-c1b7ad05
  (fn [coll n]
    (keep-indexed (fn [i item] (if (not= 0 (mod (inc i) n)) item)) coll)))

(defcheck solution-c26113c
  (fn drop-nth [coll n] (keep-indexed #(if (not= 0 (rem (+ %1 1) n)) %2) coll)))

(defcheck solution-c2628a37
  (fn [coll n]
    (mapcat #(take (dec n) %) (partition-all n coll))))

(defcheck solution-c27751f4
  #(apply concat (partition-all (- %2 1) %2 %1)))

(defcheck solution-c31ca5bb
  (fn [l n] (keep-indexed #(if (= (- n 1) (mod % n)) nil %2) l)))

(defcheck solution-c3b1a0fe
  (fn [coll n]
    (let [zipped (map vector coll (cycle (range n)))]
      (for [[el mod] zipped :when (<  mod (dec n))]
        el))))

(defcheck solution-c3ebd9f0
  (fn [aseq n]
    (keep-indexed (fn [idx item]
                    (when (pos? (mod (inc idx) n))
                      item))
      aseq)))

(defcheck solution-c4050e64
  (fn [coll n] (for [[i,v] (map-indexed list coll) :when (not= 0 (mod (inc i) n))] v)))

(defcheck solution-c46e4677
  (fn drop-every-nth [coll n]
    (keep
      identity
      (keep-indexed
        (fn [x y]
          (when (not= 0 (rem (inc x) n))
            y))
        coll))))

(defcheck solution-c488025a
  #(apply concat (partition (dec %2) %2 nil %)))

(defcheck solution-c5c50970
  (fn
    [c n]
    (loop [p []
           i 1
           r c]
      (let [t (first r)]
        (if (not t)
          p
          (recur (if (> (mod i n) 0) (conj p t) p) (inc i) (rest r)))))
    ))

(defcheck solution-c5e3579d
  (fn [xs i]
    (flatten
      (map-indexed #(if (== 0 (mod (inc %1) i)) '() %2) xs))))

(defcheck solution-c650821e
  (fn [xs n] (map first (filter #(not= (mod (inc (second %)) n) 0) (map vector xs (range))))))

(defcheck solution-c6600bc
  (fn [coll n]
    (map #(second %)
      (filter
        (fn [item] (not= (rem (first item) n) 0))
        (map vector (iterate inc 1) coll)))))

(defcheck solution-c6857823
  (fn  [l n]
    (let [cat (mapcat butlast (partition-all n l))]
      (if (= 0 (mod (count l) n))
        cat
        (concat cat [(last l)])))))

(defcheck solution-c68e7689
  #(keep-indexed (fn [i x] (when (< 0 (mod (+ 1 i) %2)) x)) %))

(defcheck solution-c79f40a3
  (fn [c s] (loop [cx c
                   cnt 1
                   r []]
              (if (empty? cx)
                r
                (if (= cnt s)
                  (recur (rest cx) 1 r)
                  (recur (rest cx) (inc cnt) (conj r (first cx))))))))

(defcheck solution-c7b5550
  (fn [items step] (map first (filter (fn [pair] (< 0 (mod (inc (second pair) ) step) ) )  (map list items (range) ) ) ) ))

(defcheck solution-c81c5626
  (fn [coll n]
    (filter (complement nil?) (map #(if (not= 0 (rem %2 n)) %1) coll (iterate inc 1)))))

(defcheck solution-c84757ec
  (fn [coll n]
    (for [[i x] (map list (cycle (range 1 (inc n))) coll)
          :when (not= i n)]
      x)))

(defcheck solution-c84af325
  (fn [coll n] (mapcat drop-last (partition n n [0] coll))))

(defcheck solution-c8800e6b
  #(flatten (map (fn [l] (take (- %2 1) l)) (partition-all %2 %))))

(defcheck solution-c89c0d3f
  (fn drop-nth
    [collection n]
    (loop [index 1
           c collection
           result []]
      (if (empty? c)
        result
        (if (= (mod index n) 0)
          (recur (inc index) (rest c) result)
          (recur (inc index) (rest c) (conj result (first c))))))))

(defcheck solution-c8aa0770
  (fn [l cnt]
    (loop [l l acc [] idx 1]
      (if (empty? l)
        acc
        (recur
          (rest l)
          (if (zero? (mod idx cnt))
            acc
            (conj acc (first l)))
          (inc idx))))))

(defcheck solution-c97aeabe
  (fn dropnth [xs n]
    (if
     (> n (count xs))
      xs
      (concat (take (dec n) xs) (dropnth (drop n xs) n)))
    ))

(defcheck solution-c9d371b0
  (fn [s k]
    (keep-indexed #(if (pos? (mod (inc %) k)) %2) s)))

(defcheck solution-ca180e84
  (fn [xs n]
    (map last
      (filter #(> (mod (inc (first %)) n) 0)
        (map-indexed vector xs)))))

(defcheck solution-ca19db15
  (fn [input n]
    (loop [lst input i 1 acc []]
      (if (empty? lst)
        acc
        (if (zero? (rem i n))
          (recur (rest lst) (inc i) acc)
          (recur (rest lst) (inc i) (conj acc (first lst))))))))

(defcheck solution-ca2448f
  (fn drop-every [coll i] (keep-indexed #(if (= 0 (rem (inc %1) i)) nil %2) coll)))

(defcheck solution-ca43533f
  #(mapcat (fn [x i] (if-not (zero? (rem (inc i) %2)) [x]))
     % (range)))

(defcheck solution-ca4e18e1
  (fn [coll n]
    (map first (filter (fn [[a b]] (not= 0 (mod b n)))
                 (map vector coll (iterate inc 1))))))

(defcheck solution-cace989d
  (fn [coll n] (apply concat (partition-all (dec n) n coll))))

(defcheck solution-cb10aa15
  (fn [niz n]
    (let [indices (range 1 (inc (count niz)))
          marked (filter #(= (mod % n) 0) indices)
          mapping (zipmap indices niz)]
      (map second (sort (vec (apply (partial dissoc mapping) marked)))))))

(defcheck solution-cb8a047b
  (fn dropnth [x n]
    "Drops every nth item of a vector x."
    (if (< (count x) n)
      x
      (loop [counter 0, result []]
        (if (< counter (count x))
          (if (= (mod counter n) (- n 1))
            (recur (inc counter) result)
            (recur (inc counter) (conj result (get (vec x) counter))))
          result)))))

(defcheck solution-cb8fd071
  (fn [coll n]
    (reduce (fn [a b] (concat a
                              (if (= n (count b))
                                (drop-last b)
                                b)))
      []
      (partition-all n coll))))

(defcheck solution-cc09176f
  (fn [s n]
    (loop [i 0
           result []]
      (cond (>= i (count s)) result
            (= (- n 1) (mod i n)) (recur (+ i 1) result)
            :else (recur (+ i 1) (conj result (get s i)))))))

(defcheck solution-cc0ef816
  (fn drop-every-nth [coll n]
    (loop [coll coll
           acc '()]
      (let [head (take (dec n) coll)
            tail (drop n coll)
            newacc (concat acc head)]
        (if (empty? tail)
          newacc
          (recur tail newacc))))
    (mapcat (partial take (dec n)) (partition-all n coll))))

(defcheck solution-cc5cc36d
  (fn [a b]
    (mapcat #(take (dec b) %)
      (partition-all b a))))

(defcheck solution-cc86f268
  (fn [a b] (into [] (for [[x y] (map-indexed vector a) :when (not= (mod (+ 1 x) b) 0)] y))))

(defcheck solution-ccb3da27
  (fn [xs n]
    (reduce
      (fn [xs [v i]]
        (if (= i (dec n)) xs (conj xs v)))
      []
      (map vector
        xs (cycle (range n))))))

(defcheck solution-ccfb6fb4
  #(for [x (range (count %1)) :when (not= (rem (inc x) %2) 0)] (nth %1 x)))

(defcheck solution-cdf30197
  #(flatten (partition (- %2 1) %2 [] %)))

(defcheck solution-ce0cbd6f
  (fn [seqs n]
    (loop [result '()
           ct 1
           others seqs]
      (if(empty? others)
        result
        (if(= ct n)
          (recur result 1 (rest others))
          (recur (concat result (list (first others)))
            (inc ct)
            (rest others)))))))

(defcheck solution-ce35487b
  (fn [xs s] (keep-indexed #(if (= 0 (rem (inc %) s)) nil %2) xs)))

(defcheck solution-ce5a3820
  (fn[l n] (keep-indexed #(if(zero? (mod (+ % 1) n)) nil %2) l)))

(defcheck solution-ceb10a7d
  (fn [xs n]
    (loop [i 1
           xs xs
           result []]
      (if (empty? xs) result
                      (recur (inc i) (rest xs)
                        (if (zero? (mod i n)) result (conj result (first xs))))))
    ))

(defcheck solution-d0219da9
  (fn [coll n]
    (let [iter (fn iter [coll i]
                 (lazy-seq
                   (cond (nil? (seq coll)) nil
                         (zero? (mod i n)) (iter (rest coll) (inc i))
                         :else (cons (first coll) (iter (rest coll) (inc i))))))]
      (iter coll 1))))

(defcheck solution-d08d4630
  (fn [my-list n]
    (loop [ctr 0 s my-list result []]
      (if (empty? s)
        result
        (recur (inc ctr) (rest s)
          (if (= (rem ctr n) (dec n))
            result
            (conj result (first s))))))))

(defcheck solution-d0b0b982
  (fn [s n]
    (letfn [(hf [result i s]
              (cond (empty? s) result
                    (= 0 (mod i n)) (recur result (inc i) (rest s))
                    :else (recur
                            (conj result (first s))
                            (inc i)
                            (rest s))))]
      (hf [] 1 s))))

(defcheck solution-d1063ace
  (fn [lst n]
    (flatten (#(partition (- n 1) n [] lst)))

    ))

(defcheck solution-d21cd068
  (fn [l n] (map second (filter #(not= 0 (rem (inc (first %)) n)) (map-indexed vector l)))))

(defcheck solution-d30b402f
  (fn [coll n]
    (apply concat
      (map
        (fn [xs]
          (if
           (=
             (count xs)
             n
             )
            (drop-last xs)
            xs
            )
          )
        (partition-all n coll)
        )
      )
    ))

(defcheck solution-d3956cae
  #(apply concat (partition-all (dec %2) %2 %)))

(defcheck solution-d3b11713
  (fn [col n]
    (filter #(not= nil %) (for [i (range (count col))] (if (not= 0 (rem (inc i) n)) (col i))))))

(defcheck solution-d40fff11
  (fn [coll n]
    (map second (filter (fn [[idx val]] (not (zero? (mod (inc idx) n)))) (map vector (range) coll)))))

(defcheck solution-d416ab87
  #(flatten (partition-all (dec %2) %2 %)))

(defcheck solution-d45f5f77
  (fn dropN [s n] (into (empty s) (map first (filter #(not= (mod (last %) n) 0) (map list s (iterate inc 1)))))))

(defcheck solution-d4ee37f1
  (fn [coll n]
    (filter identity (mapcat #(take (dec n) %) (partition n n nil coll)))))

(defcheck solution-d4f45f2
  (fn drop-nth- [coll n]
    "41. Write a function which drops every Nth item from a sequence."
    (let [s (seq coll)]
      (if s
        (concat (take (dec n) s) (drop-nth- (drop n s) n))))))

(defcheck solution-d5352581
  (fn [xs n]
    (loop [ys xs
           zs []
           m 1]
      (if (seq ys)
        (if (< m n)
          (recur (rest ys) (conj zs (first ys)) (inc m))
          (recur (rest ys) zs 1))
        zs))))

(defcheck solution-d57f68e3
  (fn [xs n]
    (loop [xs xs result ()]
      (let [xs (seq xs)
            result (concat result (take (- n 1) xs))]
        (if-not xs
          result
          (recur (drop n xs) result)
          )))))

(defcheck solution-d5adb12e
  #(map first (filter (fn[x] (not= (rem (last x) %2) 0)) (map list %1 (iterate inc 1)))))

(defcheck solution-d5bb5c59
  #(loop[result [], index %2, input %1]
     (cond
       (nil? input) result
       (= index 1) (recur result %2 (next input))
       :else (recur (conj result (first input)) (dec index) (next input)))))

(defcheck solution-d6354869
  (fn [l x]
    (map first
      (filter #(not= (last %) x)
        (map list l (cycle (range 1 (inc x))))
        )
      )
    ))

(defcheck solution-d64478b
  (fn [xs n] (mapcat butlast (partition n n '(nil) xs))))

(defcheck solution-d6fad39f
  (fn [xs n]
    (mapcat (partial take (dec n)) (partition-all n xs))))

(defcheck solution-d73a5c3c
  (fn [v n]
    (mapcat
      #(take (- n 1) %)
      (partition-all n v))))

(defcheck solution-d78bc886
  (fn [seq n] (remove nil? (map-indexed (fn [idx el] (if (= (dec n) (mod idx n)) nil el)) seq))))

(defcheck solution-d78dd6d6
  (fn [coll n]
    (for [[e idx] (map vector coll
                    (map inc
                      (range)))
          :when (not= (rem idx n) 0)]
      e)
    ))

(defcheck solution-d78e0fce
  #(mapcat butlast (partition %2 %2 [:pad] %1)))

(defcheck solution-d7b0edd5
  (fn [s n]
    (map #(nth s %)
      (remove #(= 0 (rem (inc %) n))
        (take (count s) (iterate inc 0))))))

(defcheck solution-d8000ae4
  (fn dn[ls n] (concat (take (dec n) ls) (if (empty? ls) () (dn (drop n ls) n))   )))

(defcheck solution-d84b6fcd
  (fn [l n]
    (loop [c 1 [f & args :as my-l] l r []]
      (if (empty? my-l)
        r
        (if (= (mod c n) 0)
          (recur (inc c) args r)
          (recur (inc c) args (conj r f)))))))

(defcheck solution-d8d83886
  (fn dropevery [coll idx]
    (loop [checker (take (count coll) (apply concat (repeat (range 1 (inc idx))))),
           result [],
           coll coll]
      (if (empty? coll)
        result
        (if (< (first checker) idx)
          (recur (rest checker) (conj result (first coll)) (rest coll))
          (recur (rest checker) result (rest coll)))))))

(defcheck solution-d8eb126b
  (fn drop-nth [coll n]
    (mapcat (fn [item] (if (= n (count item)) (butlast item) item))
      (partition-all n coll))))

(defcheck solution-d90fc7cf
  (fn [x n] (flatten (map (partial take (- n 1)) (partition-all n x)))))

(defcheck solution-d92e0676
  #(map (fn [[i v]] v)
     (filter (fn [[i v]] (not= (mod i %2) (- %2 1)))
       (map-indexed vector %1))))

(defcheck solution-d944f6b1
  (fn drp [x y]
    (if (empty? x)
      []
      (concat (take (dec y) x) (drp (drop y x) y)))))

(defcheck solution-d95500f9
  (fn [s n]
    (keep-indexed
      (fn [i e] (when-not (= (dec n) (rem i n)) e))
      s)))

(defcheck solution-d9d061dc
  (fn drop_every_nth
    ([x y] (drop_every_nth x y y))
    ([x y z]
     (if (empty? x)
       x
       (if (= y 1)
         (drop_every_nth (rest x) z z)
         (concat (list (first x))
                 (drop_every_nth (rest x) (- y 1) z)))))))

(defcheck solution-d9ed9ef8
  (fn [xs n] (->>
               (map vector xs (iterate inc 1))
               (filter #(not= 0 (mod (% 1) n)))
               (map first))))

(defcheck solution-da92fba9
  (fn [s freq]
    (loop [toadd (dec freq)
           se s
           res []]
      (if (empty? se)
        res
        (if (= 0 toadd)
          (recur (dec freq) (next se) res)
          (recur (dec toadd) (next se) (conj res (first se))))))))

(defcheck solution-db94c2ef
  #(for [i (range 1 (inc (count %1))) :when (not= 0 (mod i %2))] (nth %1 (dec i))))

(defcheck solution-dbb1b675
  #((fn func [s n cnt]
      (if (empty? s)
        s
        (if (= n cnt)
          (func (rest s) n 1)
          (cons (first s) (func (rest s) n (inc cnt)))))) %1 %2 1))

(defcheck solution-dc82fc41
  #(keep-indexed
     (fn [x y]
       (when (not= 0 (mod (+ 1 x) %2)) y))
     %1))

(defcheck solution-dc8a1d57
  (fn [seq n]
    (loop [s seq rslt []]
      (if (empty? s)
        rslt
        (recur (drop n s) (concat rslt (take (dec n) s)))))))

(defcheck solution-dd708cd6
  #(loop
    [h (take (- %2 1) %)
     t (drop %2 %1)]
     (if (empty? t)
       h
       (recur (concat h (take (- %2 1) t)) (drop %2 t)))))

(defcheck solution-ddd4b8c6
  (fn [l n] (map #(nth l %) (filter #(not (zero? (mod (inc %) n))) (range (count l))))))

(defcheck solution-de612caa
  #(filter (comp not nil?) (for [x (range 1 (inc (count %1))) :let [y (%1 (dec x)) z (not (zero? (mod x %2))) ]] (if z y))))

(defcheck solution-de68e96
  (fn drop-nth [s n]
    (apply concat (map #(if (< (count %) n)
                          (identity %)
                          (butlast %))
                    (partition-all n s)))))

(defcheck solution-de83700f
  #(loop [s %1 n %2 r []]
     (cond (empty? s) r
           (= n 1) (recur (rest s) %2 r)
           :else (recur (rest s) (dec n) (conj r (first s))))))

(defcheck solution-deaabbaa
  (fn [xs n]
    (letfn [(aux [ys i]
              (cond (empty? ys) '()
                    (= i 1) (lazy-seq (aux (rest ys) n))
                    true (lazy-seq (cons (first ys) (aux (rest ys) (dec i))))))]
      (aux xs n))))

(defcheck solution-df0831c7
  (fn [col n]
    (for [[i a] (map-indexed (fn [i a] [(= (mod i n) (dec n)) a]) col)
          :when (not i)]
      a)))

(defcheck solution-df2b67b7
  (fn [c n]
    (mapcat #(if (not= 0 (mod %2 n)) [%])
      c (iterate inc 1))))

(defcheck solution-df72854
  (fn [xs n]
    (map first (filter #(not (= 0 (rem (inc (second %)) n))) (map #(vector %1 %2) xs (range))))))

(defcheck solution-dfb79498
  (fn [coll n]
    (keep-indexed #(if-not (zero? (mod (inc %1) n)) %2) coll)))

(defcheck solution-dfe8bc5b
  #(flatten (partition (- %2 1) %2 nil %1)))

(defcheck solution-dff84465
  (fn [a b] (flatten(map #(if(=(count %) b) (butlast %) (list %)) (partition-all b a)))))

(defcheck solution-e0298b11
  #(apply concat (map (fn [s] (take (dec %2) s)) (partition-all %2 %1))))

(defcheck solution-e059cbb8
  (fn [v-prime n]
    (loop [v v-prime rv []]
      (if (empty? v)
        rv
        (recur (drop n v) (concat rv (take (dec n) v)))))))

(defcheck solution-e09686e5
  (fn [ input n] (map second (filter #(not= 0 (rem (inc(first %)) n))(map-indexed vector input)))))

(defcheck solution-e0b97ea6
  (fn[a b](mapcat #(take (- b 1) %) (partition-all b a))))

(defcheck solution-e106b0a1
  (fn ! [s n]
    (if (empty? s) (empty s)
                   (concat (take (dec n) s) (! (drop n s) n))
                   )
    ))

(defcheck solution-e109b611
  (fn dropn [s n]
    (if (not (empty? s)) (concat (take (dec n) s) (dropn (drop n s) n)))))

(defcheck solution-e1199d61
  (fn [seq n]
    (loop [accu [] s seq]
      (if (empty? s) accu
                     (let [s2 (take n s)]
                       (recur (concat accu (if (= (count s2) n) (butlast s2) s2))
                         (drop n s)))))))

(defcheck solution-e11f04e7
  (fn [coll n]
    (flatten (map #(take (dec n) %) (partition-all n coll)))))

(defcheck solution-e148f874
  (fn dropnth
    ([x y]
     (dropnth (rest x) y [(first x)] 2))
    ([x y z w]
     (if (= 0 (count x))
       z
       (if (= y w)
         (recur (rest x) y z 1)
         (recur (rest x) y (conj z (first x)) (inc w)))))))

(defcheck solution-e152671
  (fn [x y]
    (map #(nth x (dec %))
      (filter #(not= 0 (rem % y)) (range 1 (inc (count x))))            )))

(defcheck solution-e16463c
  (fn hoge [lst i]
    (if (< (count lst) i) [lst]
                          (flatten (cons (apply vector (take (dec i) lst)) (hoge (drop i lst) i)) ))))

(defcheck solution-e191f2d1
  (fn [icol nth]
    (let [gen-seq (fn gen-seq [col ct]
                    (lazy-seq
                      (if (empty? col) []
                                       (if (= 0 (mod ct nth))
                                         (gen-seq (rest col) (inc ct))
                                         (cons (first col) (gen-seq (rest col) (inc ct)))))))]
      (gen-seq icol 1))))

(defcheck solution-e25dfded
  (fn [s n]
    (loop [s s, r []]
      (if (empty? s) r
                     (recur (drop n s) (concat r (take (dec n) s)))))))

(defcheck solution-e2cf49c5
  #(mapcat (fn [item idx] (if (= idx (dec %2)) '() [item]) ) %1 (cycle (range %2))))

(defcheck solution-e2de3486
  #(flatten (partition-all (- %2 1) %2 %)))

(defcheck solution-e2ffb2c4
  (fn [s n] (filter #(not (zero? (mod (+ 1 (.indexOf s %)) n))) s)))

(defcheck solution-e32487f9
  (fn drop-every [coll x]
    (let [ct (count coll)]
      (loop [i 1
             acc []
             coll coll]
        (if (empty? coll)
          acc
          (if (= 0 (mod i x))
            (recur (inc i) acc (rest coll))
            (recur (inc i) (conj acc (first coll)) (rest coll))))))))

(defcheck solution-e3496cf3
  #(keep-indexed (fn [idx itm] (if (> (mod (inc idx) %2) 0) itm)) %))

(defcheck solution-e3e62ca5
  (fn foo [xs c] (if (seq xs) (concat (take (dec c) xs) (foo (drop c xs) c)))))

(defcheck solution-e45cf94e
  (fn drop-every-nth [col n]
    (->> col
      (partition n n (repeat :nil))
      (mapcat butlast)
      (filter #(not= :nil %)))))

(defcheck solution-e4de8eae
  (fn [coll, n]
    (map second (remove (fn [[idx itm]] (= 0 (rem idx n))) (map (fn [[i e]] [(inc i), e]) (map-indexed vector coll))))))

(defcheck solution-e4ee01c8
  (fn [s n]
    (map first
      (remove (fn [[e i]] (= i n))
        (map list s (cycle (range 1 (inc n))))))))

(defcheck solution-e5036ee2
  #(apply concat (map-indexed
                   (fn [i x]
                     (if (= 0 (mod (+ 1 i) %2)) [] [x])) %1)))

(defcheck solution-e54cf876
  #(keep-indexed
     (fn [idx v]
       (if (not (= (rem (+ idx 1) %2) 0))
         v
         )) %))

(defcheck solution-e5a1eda6
  (fn [coll n] (filter #(not (nil? %)) (map-indexed (fn [idx item] (if (= (mod (inc idx) n) 0) nil item)) coll))))

(defcheck solution-e5c7e271
  (fn prob41
    ([s n] (prob41 s n 1))
    ([s n i]
     (cond
       (empty? s) s
       (= i n) (prob41 (rest s) n 1)
       :else (cons (first s) (prob41 (rest s) n (inc i)))))))

(defcheck solution-e5d97e7b
  (fn [s n] (keep-indexed #(if (< 0 (mod (inc %1) n)) %2) s)))

(defcheck solution-e61b0c90
  (fn [a x]
    (keep-indexed #(if (not= 0 (mod (inc %) x)) %2) a)))

(defcheck solution-e654bf68
  (fn [ col n] (mapcat #(take (dec n) % ) (partition n n nil col))))

(defcheck solution-e65f7e69
  (fn [coll n]
    (vec
      (filter
        #(-> (.indexOf coll %) (inc) (rem n) (not= 0))
        coll))))

(defcheck solution-e716ad57
  (fn [col n]
    (let [end (- (count col) n)
          nn (dec n)]
      (loop [start 0 acc []]
        (if (> start end)
          (into acc (subvec col start))
          (recur (+ start n)
            (into acc (subvec col start (+ start nn)))))))))

(defcheck solution-e722ca3b
  (fn [coll n] (mapcat #(if (= (count %) n) (butlast %) %) (partition-all n coll))))

(defcheck solution-e73be837
  (fn drop-nth [v n]
    (loop [i 1, cnt (count v), result [] ]
      (if (<= i cnt)
        (recur (inc i), cnt,
          (if (= 0 (rem i n))
            result
            (conj result (v (dec i)))))
        result))))

(defcheck solution-e75943e4
  (fn f [s index]
    (map #(get s (dec %)) (filter #(not= 0 (rem % index)) (map inc (range (count s)))))))

(defcheck solution-e81729de
  (fn rem-mul
    [coll index]
    (if (empty? coll) []
                      (concat (take (dec index) coll) (rem-mul (drop index coll) index)))))

(defcheck solution-e8185a99
  (fn [alist div]
    (for [len (range (count alist))
          :let [thisnum (nth alist len)]
          :when (not= (rem (+ len 1) div) 0)]
      thisnum)))

(defcheck solution-e88c5aaf
  (fn [l n] (reverse (loop [acc () c 1 s1 l]
                       (if (empty? s1)
                         acc
                         (if (= c n)
                           (recur acc 1 (rest s1))
                           (recur (cons (first s1) acc) (inc c) (rest s1))
                           ))))))

(defcheck solution-e8d33d
  (fn [a b] (remove nil? (map #(if-not (zero? (rem %2 b)) %) a (cycle (range 1 (inc b)))))))

(defcheck solution-e90d277
  (fn [col n] (map #(get col %) (remove #(zero? (rem (inc %) n)) (range (count col))))))

(defcheck solution-e925c8c8
  (fn [S n] (mapcat identity (partition (dec n) n nil S))))

(defcheck solution-e9261ccf
  (fn [coll drop]
    (loop [es coll
           c []
           n 1]
      (cond
        (empty? es) c
        :default (recur
                   (rest es)
                   (if (zero? (mod n drop))
                     c (conj c (first es)))
                   (inc n))))))

(defcheck solution-e961c352
  #(let [i (atom 0)] (remove (fn [q] (= 0 (mod (swap! i inc) %2))) %1)))

(defcheck solution-e9828cbc
  (fn [s n]
    (keep-indexed #(if (= (- n 1) (mod %1 n)) nil %2) s)
    ))

(defcheck solution-e991d298
  (fn nn [seqn n]
    (mapcat #(if (= n (count %1)) (drop-last %1) %1) (partition-all n seqn))
    ))

(defcheck solution-e9c981a9
  (fn drop [lst num] (remove nil? (map-indexed
                                    #(if (not= 0 (rem (inc %) num)) %2) lst))))

(defcheck solution-ea0803c9
  #(loop [i 0 n []]
     (if (< i (count %1))
       (if (not= (mod (+ i 1) %2) 0)
         (recur (inc i) (conj n (%1 i)))
         (recur (inc i) n)
         )
       n)))

(defcheck solution-ea1f27ca
  (fn [coll n]
    (keep-indexed
      (fn [i x] (condp = (mod (inc i) n)
                  0 nil
                  x))
      coll)))

(defcheck solution-ea39c5cb
  (fn [col x] (mapcat #(take (dec x) %) (partition x x [] col))))

(defcheck solution-eac3182c
  (fn
    [xs n]
    (loop [in xs, out [], i 1]
      (cond
        (empty? in) (seq out)
        (= 0 (rem i n)) (recur (rest in) out (inc i))
        :else (recur (rest in) (conj out (first in)) (inc i))))))

(defcheck solution-eb39a5d
  (fn [sq n]
    (let [n- (dec n)
          aux
             (fn [[head & tail] idx acc]
               (cond
                 (nil? head)
                 acc

                 (zero? idx)
                 (recur tail n- acc)

                 :else
                 (recur tail (dec idx) (conj acc head))))]
      (aux sq n- []))))

(defcheck solution-eb618f31
  (fn [x n]
    (keep-indexed #(if (not= (mod (inc %1) n) 0) %2) x)))

(defcheck solution-eb9bc69a
  (fn drop-nth [s n]
    (if (> (count s) (dec n))
      (concat (take (dec n) s) (drop-nth (drop n s) n))
      s)))

(defcheck solution-ebcdf996
  (fn [xs n] (apply concat (map (fn [x y] (if (= 0 (mod y n)) [] [x])) xs (rest (range))))))

(defcheck solution-ec261e47
  #(mapcat butlast (partition %2 %2 [0] %1)))

(defcheck solution-ec28924d
  (fn [lst n] (filter identity (map #(and (> (rem %1 n) 0) %2) (iterate inc 1) lst))))

(defcheck solution-ecabab2e
  #(vec (for [i (range (count %1)) :when (not= 0 (rem (inc i) %2))] (nth %1 i) )))

(defcheck solution-ecccb0ad
  #(map second
     (filter
       (partial apply (fn [i el] (not (= 0 (mod (+ 1 i) %2)))))
       (map-indexed (fn [a b] (list a b)) %1))))

(defcheck solution-ecd82ce9
  (fn [xs n]
    (mapcat drop-last (partition n n [:dropme] xs))))

(defcheck solution-edcfa9b0
  #(mapcat (fn [a]
             (take (- %2 1) a))
     (partition-all %2 %1)))

(defcheck solution-edd68dce
  (fn dropn
    ([s a c r] #_(println s a c r)  (if (seq s) (dropn (rest s) a  (inc c)  (if (zero? (rem c a)) r  (conj r (first s) ))   ) (flatten r) ) )
    ([s a] (dropn s a 1 [] ) )
    ))

(defcheck solution-eec9cb7d
  (fn drop-every-nth [xs n]
    (if (empty? xs)
      (list)
      (concat
       (take (- n 1) xs)
       (drop-every-nth (drop n xs) n)))))

(defcheck solution-eecf6050
  (fn dropn [s n]
    (flatten
      (map #(if (= n (count %)) (butlast %) %)
        (partition n n '() s)))))

(defcheck solution-eed00160
  (fn [xs n]
    (filterv #(not= 0 (mod (inc (.indexOf xs %)) n)) xs)))

(defcheck solution-ef804a76
  #(mapcat butlast (partition %2 %2 [1] %1)))

(defcheck solution-f000133
  (fn [coll n]
    (keep-indexed #(if (not= (rem (+ %1 1) n) 0) %2) coll)))

(defcheck solution-f0354ea1
  (fn [coll n] (mapcat #(take (dec n) %) (partition-all n coll))))

(defcheck solution-f0873140
  (fn drop-nth [xs n]
    (if (empty? xs)
      ()
      (concat (take (- n 1) xs) (drop-nth (drop n xs) n))
      )
    ))

(defcheck solution-f0c9ab70
  (fn [S N]

    (flatten (concat (map drop-last (partition N S)) (drop (* N (quot (count S) N)) S)))
    ))

(defcheck solution-f16a19cd
  #(for [i (range (count %1)) :when (not= (- %2 1) (mod i %2))] (%1 i)))

(defcheck solution-f1805f3e
  (letfn [(drop-nth [xs n]
            (when-let [s (seq xs)]
              (lazy-seq
                (concat (take (dec n) s) (drop-nth (drop n s) n)))))]
    drop-nth))

(defcheck solution-f1ec87c0
  (fn [l n]
    (flatten (map (fn [v]
                    (if (= n (count v))
                      (drop-last v)
                      v))
               (partition-all n l)))))

(defcheck solution-f2ab0e29
  (fn this [s n]
    #_(println s)
    (cond (< (count s) n) s
          :else (vec (concat (take (dec n)
                               (first (iterate rest s)))
                             (this (nthnext s n) n)
                             )
                  ) )))

(defcheck solution-f2cf3bf6
  #(keep-indexed (fn [i x] (when (not= (- %2 1) (mod i %2)) x)) %1))

(defcheck solution-f30a8236
  (fn [coll n]
    (loop [coll coll
           i 0
           answer []]
      (cond
        (nil? coll) answer
        (= (dec n) (mod i n)) (recur (next coll) (inc i) answer)
        :else (recur (next coll) (inc i) (conj answer (first coll)))))))

(defcheck solution-f375dd42
  (fn remove-every-nth [coll n]
    (remove nil? (map-indexed #(when (not (= (mod (+ 1 %) n) 0)) %2 ) coll))
    ))

(defcheck solution-f3e6bcb1
  (fn [x s] (remove nil? (map #(if %2 %1 nil) x (cycle (concat (repeat (dec s) true) '(false)))))))

(defcheck solution-f4fa8006
  (fn [xs n]
    (loop [xs xs
           acc []]
      (let [[xs-begin xs-next] (split-at (dec n) xs)]
        (if (seq xs-begin)
          (recur (rest xs-next) (concat acc xs-begin))
          acc)))))

(defcheck solution-f5094c1d
  (fn [x n]
    (mapcat (fn [y] (take (- n 1) y)) (partition-all n x))))

(defcheck solution-f582d130
  (fn [s n]
    ((fn d [r m]
       (cond
         (empty? r) '()
         (= m n) (d (rest r) 1)
         :else (conj (d (rest r) (inc m)) (first r))))
     s 1)))

(defcheck solution-f59f6daa
  (fn [x y] (flatten (map #(if (== (count %) y) (drop-last %) %) (partition-all y x)))))

(defcheck solution-f5e4e4ba
  (fn [x,y]
    ((fn ! [x,y,z]
       (if(empty? x)
         '()
         (if(= y z)
           (! (rest x) y 0)
           (conj (! (rest x) y (inc z)) (first x)))))
     x,(dec y),0)))

(defcheck solution-f633fcca
  #(keep-indexed (fn [i x] (if (< 0 (mod (+ i 1) %2)) x))
     %1))

(defcheck solution-f637c0bf
  (fn [s skip] (reduce concat (map #(take (dec skip) %) (partition-all skip s)))))

(defcheck solution-f638f8b7
  (fn [c n]
    (->> (map vector (cycle (range n)) c)
      (filter #(< (first %) (dec n)))
      (map second))))

(defcheck solution-f641c3e3
  (fn [l n] (keep-indexed #(if (< (+ 1 (rem % n)) n) %2) l)))

(defcheck solution-f6660c11
  (fn [coll n]
    (->> coll
      (map-indexed #(vector %1 %2))
      (filter #(not (= (rem (first %) n) (- n 1))))
      (map last)
      )))

(defcheck solution-f69e6c1e
  #(map first
     (filter (fn [[_ i]] (> i 0))
       (map vector
         %1
         (rest (apply concat
                 (repeat (range 0 %2))))))))

(defcheck solution-f6aff473
  (fn [x n]
    (keep-indexed
      #(if (not= 0 (rem (inc %1) n)) %2
                                     )
      x
      )))

(defcheck solution-f76027a
  (fn [a b]
    (let [to-drop (take-nth b (drop (- b 1) a))]
      (remove
        #(if (some #{%} to-drop) true false)
        a))))

(defcheck solution-f77ab771
  (fn skip-n [s n]
    (concat (take (- n 1) s)
            (if (> (count (nthrest s n)) n) (skip-n (nthrest s n) n)
                                            (take (- n 1) (nthrest s n))))))

(defcheck solution-f77e21f6
  (fn [xs n]
    (mapcat #(take (dec n) %) (partition-all n xs))))

(defcheck solution-f7910138
  (fn drop-nth [s n]
    (map
      first
      (filter
        #(not= 0 (mod (last %) n))
        (map list s (iterate inc 1))))))

(defcheck solution-f7b03913
  (fn foo [s, n]
    (map #(second %1)
      (filter #(not (= (mod (inc (first %1)) n) 0))
        (map #(list %1 %2) (range) s)))))

(defcheck solution-f838883d
  (fn [lst n]
    (->> lst
      (partition (dec n) n nil)
      (apply concat)
      )
    ))

(defcheck solution-f8443252
  (fn [sq n]
    (reverse
      (reduce
        (fn [t ss]
          (apply conj t (if (= (count ss) n) (butlast ss) ss)))
        '()
        (partition-all n sq)))))

(defcheck solution-f86657a0
  (fn [s n] (mapcat #(take (dec n) %) (partition n n [] s))))

(defcheck solution-f88d11d6
  (fn [coll n]
    (apply concat (partition (dec n) n [] coll))))

(defcheck solution-f8ba0e2c
  (fn dropEveryNth [coll n] (mapcat #(if (= (count %) n) (butlast %) %) (partition-all n coll))))

(defcheck solution-f8df40f8
  #(mapcat (partial take (dec %2))
     (partition-all %2 %1)))

(defcheck solution-f8fa5489
  (fn lolo
    [input-seq index]
    (keep-indexed
      #(if
        (not=
          (mod (+ %1 1) index)
          0)
         %2)
      input-seq)))

(defcheck solution-f92bd98c
  (fn  [l n]
    (mapcat #(if (= n %2)
               '()
               (list %)
               )
      l
      (cycle (range 1 (inc n)))
      )
    ))

(defcheck solution-f9499663
  #(loop [coll % n 1 res []]
     (if (empty? coll)
       res
       (if (= 0 (mod n %2))
         (recur (rest coll) (inc n) res)
         (recur (rest coll) (inc n) (concat res (list (first coll))))
         )
       )))

(defcheck solution-f95d63b9
  (fn [col n]
    (loop [col col
           i 1
           res []]
      (if (nil? (first col))
        res
        (if (= n i)
          (recur (rest col) 1 res)
          (recur (rest col) (inc i) (concat res (list (first col)))))))))

(defcheck solution-fa237cdd
  (fn [s i]
    (mapcat
      #(if (= (count %) i) (drop-last %) %)
      (partition-all i s))))

(defcheck solution-fa646c9a
  (fn f [s n]
    (filter #(not (zero? (mod (inc (.indexOf s %)) n))) s)))

(defcheck solution-fa7c5c0d
  (fn [xs n]
    (first (reduce
             (fn [[acc cnt] b]
               (if (= n cnt)
                 [acc 1]
                 [(conj acc b) (inc cnt)]))
             [(empty xs) 1]
             xs))))

(defcheck solution-fa9d7baf
  (fn drop-nth
    [coll n]
    (lazy-seq
      (let [s (take (dec n) coll)]
        (when-not (empty? s)
          (concat s (drop-nth (drop n coll) n)))))))

(defcheck solution-fb634e85
  (fn [xs n]
    (remove nil? (map-indexed #(if (not= (dec n) (rem % n)) %2) xs))))

(defcheck solution-fb8f0edb
  (fn [c d] (remove nil? (map-indexed #(if (> (rem (+ 1 %) d) 0) %2) c))))

(defcheck solution-fbced066
  #(mapcat identity (partition (dec %2) %2 [] %)))

(defcheck solution-fbd5d7be
  #(reduce concat (partition (dec %2) %2 [] %1)))

(defcheck solution-fc622f6d
  (fn drop-every--mask
    [coll n] {:pre [(integer? n), (pos? n)]}
    ;; This mask (a lazy sequence) has a 0 if we are dropping the corresponding
    ;; element of coll, and a 1 otherwise; e.g., when n = 3, the mask is:
    ;;   (0 0 1 0 0 1 0 0 1 ...)
    (let [mask (drop 1 (cycle (cons 0 (repeat (dec n) 1))))]
      (mapcat #(if (= %2 0) [] [%1]) coll mask))))

(defcheck solution-fc8c73d
  (fn [xs n]
    (->>
      (range 1 (inc (count xs)))
      (filter #(not= (rem % n) 0))
      (map dec )
      (map #(nth xs %) )
      )
    ))

(defcheck solution-fcbd0353
  (fn [v n]
    (loop [x 1 ans []]
      (if (> x (count v))
        ans
        (recur (inc x) (if
                        (= 0 (mod x n))
                         ans
                         (conj ans (nth v (- x 1)))
                         ))))))

(defcheck solution-fd124b36
  #(mapcat (partial take (- %2 1)) (partition-all %2 %1)))

(defcheck solution-fd2031c
  (fn drop-nth [seq n]
    (flatten
      (if (> n (count seq))
        seq
        (cons (take (- n 1) seq) (drop-nth (drop n seq) n))))))

(defcheck solution-fd3c751f
  #(->> %1
     (partition-all %2)
     (map (partial take (dec %2)))
     (apply concat)))

(defcheck solution-fe1cd303
  (fn [s d]
    (loop [x s
           c (dec d)
           acc []]
      (if (nil? x)
        acc
        (if (zero? c)
          (recur (next x) (dec d) acc)
          (recur (next x) (dec c) (conj acc (first x))))))))

(defcheck solution-fec5ae52
  (fn [s n]
    (loop [s s acc []]
      (if (< (count s) n)
        (concat acc s)
        (recur (drop n s) (into acc (take (dec n) s)))))))

(defcheck solution-ff01a8de
  (fn [l n] (mapcat #(take (dec n) %) (partition-all n l))))

(defcheck solution-ffa03055
  (fn[s n](remove nil? (map-indexed (fn[idx itm](if(not= (mod (inc idx) n) 0)itm)) s))))

(defcheck solution-ffd3d463
  #(apply concat (map (fn [x]
                        (take (dec %2) x))
                   (partition-all %2 %1))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-41))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
