(ns coal-mine.problem-25
  (:require [coal-mine.checks :refer [defcheck-25] :rename {defcheck-25 defcheck}]
            [clojure.test]))

(defcheck solution-10545736
  (fn findOdd [x] (if (seq x)
                    (if (odd? (first x))
                      (conj (findOdd (drop 1 x))(first x))
                      (findOdd (drop 1 x))
                      )
                    ()
                    )
    ))

(defcheck solution-11e8a2a7
  #(filter (fn [x] (= (mod x 2) 1)) %))

(defcheck solution-11f19a0
  (partial filter #(= (rem % 2) 1)))

(defcheck solution-12c82ba2
  (fn [xs] (filter #(= (rem % 2) 1) xs)))

(defcheck solution-15f638e7
  (partial filter #(= 1 (rem % 2))))

(defcheck solution-18243152
  filter #(zero? (mod (inc %) 2)))

(defcheck solution-184fd306
  (fn [l]
    (filter (fn [_] (= (mod _ 2) 1)) l)))

(defcheck solution-18a59559
  #(
    (fn odd [src, dest]
      (if (= (first src) nil)
        dest
        (if (= 1 (mod (first src) 2))
          (odd (rest src) (conj dest (first src)))
          (odd (rest src) dest)
          )
        )
      )
    %, []
    ))

(defcheck solution-19d51889
  (fn [coll]
    (filter #(= 1 (rem % 2)) coll)
    ))

(defcheck solution-1aa168cc
  filter odd?)

(defcheck solution-1d64232c
  #(filter (fn [v] (not= 0 (rem v 2))) %))

(defcheck solution-1e2383d2
  (fn [x] (filter #(= 1 (rem % 2)) x)))

(defcheck solution-202a3080
  (fn [s]
    (filter #(= (mod % 2) 1) s)
    ))

(defcheck solution-2033d72a
  (partial filter #(= 1 (mod % 2))))

(defcheck solution-218b0902
  reduce #(if (odd? %2) (conj %1 %2) %1) [])

(defcheck solution-23852c65
  (fn [s] (filter #(not (= 0 (rem % 2))) s)))

(defcheck solution-2491c74e
  filter #(not (= 0 (mod % 2))))

(defcheck solution-29b62686
  (fn [l] (filter #(odd? %) l)))

(defcheck solution-2ae3f2e3
  (fn odd [x]
    (if (= x '())
      '()
      (if (= 1 (mod (first x) 2))
        (conj (odd (rest x)) (first x))
        (odd (rest x))
        )
      )
    ))

(defcheck solution-2b729f84
  (fn [coll] (filter odd? coll)))

(defcheck solution-2be87eb7
  (fn [values]
    (filter odd? values)))

(defcheck solution-2de13267
  (fn [s] (filter #(odd? %) s)))

(defcheck solution-2fd5c161
  (fn [s]
    (for [x s
          :when (= (mod x 2) 1)]
      x)))

(defcheck solution-347ae576
  filter #(-> (mod % 2) zero? not))

(defcheck solution-361bf12
  (fn [seq]
    (filter odd? seq)))

(defcheck solution-36ad3188
  keep #(if (odd? %) %))

(defcheck solution-3913390d
  (fn [s]
    (filter #(odd? %) s)))

(defcheck solution-39d5a9de
  (fn [s] (filter #(not (= 0 (mod % 2))) s)))

(defcheck solution-3ba7d83d
  #( filter odd? %))

(defcheck solution-3bfb7d32
  (fn [list]
    (loop [l list ret '()]
      (if (= (count l) 0)
        (reverse ret)
        (if (odd? (first l))
          (recur
            (rest l)
            (cons (first l) ret)
            )
          (recur
            (rest l)
            ret
            )
          )
        )
      )
    ))

(defcheck solution-3c45033c
  (fn [cc]
    (filter #(= 1 (mod % 2 )) cc)))

(defcheck solution-3d33f64e
  #(filter (fn [n] (== (rem n 2) 1)) %))

(defcheck solution-3df73c24
  (fn [x] (keep #(if (odd? %) %) x)))

(defcheck solution-4499878e
  #(for [i % :when (= 1 (mod i 2))] i))

(defcheck solution-479b416e
  (partial filter odd?))

(defcheck solution-47f41125
  filter (complement even?))

(defcheck solution-491edac0
  (fn[col]
    (reverse (reduce #(if (odd? %2) (conj %1 %2) %1) (list) col))
    ))

(defcheck solution-49d0a82a
  #(letfn [(worker [x n]
             (if (= x ())
               n
               (recur (rest x) (if (odd? (first x)) (conj n (first x)) n))))]
     (worker % [])))

(defcheck solution-4b1d4706
  ( fn [ls] (filter #(= (mod %1 2) 1) ls)))

(defcheck solution-4bc113a8
  (fn [xs]
    (filter #(= 1 (rem % 2)) xs)))

(defcheck solution-4d70d944
  filter #(= 1 (rem % 2)))

(defcheck solution-4e25fbcc
  (fn [lst] (filter odd? lst)))

(defcheck solution-4ee76d36
  (fn odd
    [arr]
    (filter #(= (mod %1 2 ) 1) arr)))

(defcheck solution-4fb57081
  (fn [alist] (filter odd? alist)))

(defcheck solution-515c5600
  (fn [s]
    (filter odd? s)))

(defcheck solution-52c5ccf7
  filter (fn [x] (= 1 (mod x 2))))

(defcheck solution-53bfb8aa
  (fn [xs] (filter #(odd? %) xs)))

(defcheck solution-542e3d2d
  (fn [sq] (filter #(not= (mod % 2) 0) sq)))

(defcheck solution-54dccc3
  (fn sol0025-remove
    [coll]
    (filter odd? coll)))

(defcheck solution-579a3e2d
  (partial filter #(= (mod % 2) 1)))

(defcheck solution-57fdc481
  (fn [input] (filter odd? input)))

(defcheck solution-59e994f4
  #(filter odd?  %))

(defcheck solution-5a47f193
  (partial filter #(odd? %)))

(defcheck solution-5be38fed
  (fn only-odd [a-seq]
    (filter #(= 1 (rem % 2)) a-seq)
    ))

(defcheck solution-5beb2f09
  filter #(> (rem % 2) 0))

(defcheck solution-5c636f32
  (fn [x]
    (loop [input x
           result []]
      (if (empty? input)
        (reverse result)
        (recur (rest input)
          (if (= (mod (first input) 2) 0)
            result
            (cons (first input) result))
          )))))

(defcheck solution-5c7fd206
  filter #(== (mod % 2) 1))

(defcheck solution-5cca1d3b
  (fn my-filter [s]
    (cond (empty? s) []
          (odd? (first s)) (cons (first s) (my-filter (rest s)))
          :else (my-filter (rest s)))))

(defcheck solution-5ce288dd
  (fn ! [x] (if (empty? x) '() (if (= 1 (rem (first x) 2))   (concat  (list (first x))  (! (rest x)) )    (! (rest x))   )   )  ))

(defcheck solution-5e1dabaf
  (fn [l]
    (filter odd? l)))

(defcheck solution-5e5228be
  (fn odds [x]
    (if (= (count x) 0)
      '()
      (if (= (mod (first x) 2) 1)
        (conj (odds (rest x)) (first x))
        (odds (rest x))
        ))))

(defcheck solution-6529e30e
  (partial filter odd? ))

(defcheck solution-66dbb6ec
  filter #(not (== (mod % 2) 0)))

(defcheck solution-67677457
  (fn [lst] (filter #(odd? %) (seq lst))))

(defcheck solution-68e5a6c3
  filter #(-> % (mod 2) (= 1)))

(defcheck solution-6a1a3963
  (fn [s]
    (filter (fn [x] (= (mod x 2) 1)) s)))

(defcheck solution-6a3e20ac
  #(for [n % :when (= 1 (rem n 2))] n))

(defcheck solution-6ab8cd1a
  (fn [coll]
    (filter #(odd? %) coll)))

(defcheck solution-6b8f0965
  (fn [seq]
    (filter #(= (mod % 2) 1) seq)))

(defcheck solution-6cd3c7d0
  (fn [ns]
    (filter odd? ns)))

(defcheck solution-6e36fee3
  (fn [listy]
    (filter (fn [x] (= 1 (mod x 2))) listy)
    ))

(defcheck solution-70dd1efa
  (fn [xs] (filter odd? xs)))

(defcheck solution-73aeff63
  (fn [input]
    (loop [result [] elements input]
      (if (empty? elements)
        result
        (if (odd? (first elements))
          (recur (conj result (first elements)) (rest elements))
          (recur result (rest elements))
          )
        )
      )
    ))

(defcheck solution-741b0c50
  (fn [ls] (filter odd? ls)))

(defcheck solution-79d8a6ef
  (fn [nums] (filter odd? nums)))

(defcheck solution-7c80980c
  (fn myodd
    [myseq]
    (sort (loop
           [acc '() myrest myseq]
            (if (empty? myrest)
              acc
              (if (= 1 (rem (first myrest) 2))
                (recur (cons (first myrest) acc)  (rest myrest) )
                (recur acc (rest myrest)))
              )))))

(defcheck solution-7d6c5144
  (fn [x] (filter (fn [y] (if-not (= 0 (mod y 2)) true nil)) x)))

(defcheck solution-7dcddaaa
  (fn [elems]
    (filter #(odd? %) elems)))

(defcheck solution-7e591823
  #(filter (fn [x] (= 1 (mod x 2))) %))

(defcheck solution-80cc1152
  remove even?)

(defcheck solution-81623ed5
  (fn [xs]
    (filter #(odd? (mod % 2)) xs)))

(defcheck solution-81fc37d1
  #(filter odd? (seq %)))

(defcheck solution-82a7668a
  (fn filterodd [l]
    (reverse (loop [m l s '()]
               (let [h (first m) t (rest m)]
                 (if (empty? m) s (if (= 1 (rem h 2)) (recur t (conj s h)) (recur t s)))
                 )))
    ))

(defcheck solution-83a84d08
  #(reduce (fn [c x] (if (odd? x) (conj c x) c)) [] %))

(defcheck solution-857acec7
  (fn [col] (filter odd? col)))

(defcheck solution-88a2e727
  (fn [seq]
    (filter
      (fn [item]
        (= (mod item 2) 1))
      seq)))

(defcheck solution-89fda950
  (fn [x] (filter #(> (mod % 2 ) 0) x)))

(defcheck solution-8b398148
  #(for [x %1 :when (odd? x)] x))

(defcheck solution-8b990e8
  (fn [coll]
    (filter odd? coll)))

(defcheck solution-8be401ce
  filter #(=(rem % 2) 1 ))

(defcheck solution-8d008d2c
  filter #(pos? (rem % 2)))

(defcheck solution-8ec0e88f
  #(filter (fn [x] (== (mod x 2) 1)) %))

(defcheck solution-8f00aee7
  (fn [s]
    (filter #(= (mod % 2) 1) s)))

(defcheck solution-8f63d8c6
  (fn [x] (filter #(= (mod % 2) 1) x)))

(defcheck solution-9137ba60
  (fn [x] (filter #(= 1 (mod %1 2)) x)))

(defcheck solution-91bb1b33
  #(filter odd? %))

(defcheck solution-91e9e47e
  #(reduce (fn [s i] (if (odd? i) (conj s i) s)) [] %))

(defcheck solution-926b6fc4
  (fn [coll] (filter #(= 1 (rem % 2)) coll)))

(defcheck solution-947e2f82
  (fn [xs](filter odd? xs)))

(defcheck solution-949a8f9c
  (fn odds [sq] (if (empty? sq) sq (if (= (rem (first sq) 2) 1) (cons (first sq) (odds (rest sq))) (odds (rest sq))))))

(defcheck solution-95f8c625
  (fn [xs] (filter #(= 1 (rem % 2)) xs)))

(defcheck solution-9734f871
  (fn [xs] (filter (fn [x] (= (mod x 2) 1)) xs)))

(defcheck solution-990a27bf
  (fn [l] (for [x l :when (odd? x)] x)))

(defcheck solution-9913155d
  #(filter (fn [n] (= 1 (rem n 2))) %))

(defcheck solution-9a5ef221
  (fn [s] (filter odd? s)))

(defcheck solution-9f153c5a
  (fn [s]
    (loop [rem (seq s), acc (vector)]
      (cond (empty? rem) acc
            :else (let [f (first rem), r (rest rem)]
                    (if (= 0 (mod f 2))
                      (recur (rest rem) acc)
                      (recur (rest rem) (conj acc f))))))))

(defcheck solution-a1971ac4
  (fn [coll] (filter #(= 1 (mod % 2)) coll)))

(defcheck solution-a3360fd8
  (fn [coll] (filter #(odd? %) coll)))

(defcheck solution-a38bf093
  reduce (fn[x y] (if (not= (mod y 2) 0) (conj x   y)  x )) [])

(defcheck solution-a6670d81
  (fn [l] (filter odd? l)))

(defcheck solution-a668ea14
  #(for [x % :when (odd? x)] x))

(defcheck solution-a7397aca
  (fn [s] (filter #(= (mod % 2) 1) s)))

(defcheck solution-a9c23bd6
  (fn [coll]
    (reduce #(if (odd? %2) (conj %1 %2) %1) [] coll )

    ))

(defcheck solution-aa410a8c
  (fn [x]
    (filter odd? x)
    ))

(defcheck solution-abe052c0
  #(filter (fn [x] (odd? x)) %))

(defcheck solution-ac540a1
  (fn [e] (filter #(= 1 (mod % 2)) e)))

(defcheck solution-af4ad4f8
  filter #(not (= (rem % 2) 0)))

(defcheck solution-af866c70
  filter #(not (zero? (rem % 2))))

(defcheck solution-b07b366
  filter odd?)

(defcheck solution-b27626b4
  (fn [x] (filter #(not (= (mod % 2) 0)) x)))

(defcheck solution-b399958b
  #(filter (fn [x] (not (zero? (mod x 2)))) %))

(defcheck solution-b48e5576
  (fn [n] (filter #(odd? %) n)))

(defcheck solution-b55c48e
  ;(fn odd-seq [coll]
  ;  (loop [coll coll
  ;         odd-coll ()]
  ;   (if (= () coll)
  ;      odd-coll
  ;      (if (= 1 (rem (last coll) 2))
  ;        (recur (take (dec (count coll)) coll) (cons (last coll) odd-coll))
  ;        (recur (take (dec (count coll)) coll) odd-coll)))))

  (fn odd-seq [coll] (filter odd? coll)))

(defcheck solution-b5baef7f
  (fn [x] (filter odd? x)))

(defcheck solution-b7696fd6
  (fn aux [l]
    (filter #(= (mod % 2) 1) l)))

(defcheck solution-b7853f72
  (fn purify [x]
    (filter #(== (mod % 2) 1) x)))

(defcheck solution-b8c8f7d4
  filter #(not (even? %)))

(defcheck solution-b8cd84b7
  filter #(if (= 1 (mod % 2)) %))

(defcheck solution-b94fc2d5
  (fn [x] (filter #(odd? %) x)))

(defcheck solution-be54176b
  (fn [x]
    (filter (fn [y] (odd? y)) x)))

(defcheck solution-bec97f8
  #(reverse (reduce (fn [acc x] (if (odd? x) (conj acc x) acc)) '() %)))

(defcheck solution-bf0ccb7a
  #( filter odd? (seq %)))

(defcheck solution-bf5d6da6
  filter (fn [x] (odd? x)))

(defcheck solution-c077c7e8
  (partial filter #(not= (mod % 2) 0)))

(defcheck solution-c0e4f71
  (fn fon[s]
    (if (seq s)
      (if
       (odd? (first s))
        (cons (first s) (fon (next s)) )
        (fon (next s))
        )
      '()
      )
    ))

(defcheck solution-c301f276
  #(filter (fn [x] (= 1 (rem x 2))) %))

(defcheck solution-c349ba95
  (fn [l] (filter #(= (rem % 2) 1) l) ))

(defcheck solution-c39482a6
  reduce (fn [a b] (if (= 1 (rem b 2)) (conj a b) a)) [])

(defcheck solution-c3a8eaea
  #(filter (fn [n] (not (= 0 (mod n 2)))) %))

(defcheck solution-c559c7b
  filter #(= (mod %1 2) 1))

(defcheck solution-c877ce6c
  (fn [x]
    (filter #(odd? %) x)))

(defcheck solution-ca829758
  (fn odds [coll] (filter odd? coll)))

(defcheck solution-cae6b447
  filter (fn [x] (= (rem x 2) 1)))

(defcheck solution-cb857f3c
  (fn [lista] (filter odd? lista)))

(defcheck solution-cd35cc22
  #(filter odd? %1))

(defcheck solution-cdc74bf5
  (partial filter (fn [x] (= 1 (rem x 2)))))

(defcheck solution-d027f0ef
  (fn [l] (filter #((comp not even?) %) l)))

(defcheck solution-d0af4c37
  (fn odd [s]
    (filter #(= 1 (mod % 2)) s)))

(defcheck solution-d10028a8
  (fn [xs] (filter (fn [y] (= (mod y 2) 1)) xs)))

(defcheck solution-d1c85096
  filter #(= 1 (mod % 2)))

(defcheck solution-d205c6dc
  (fn findOddNos1
    [sequence]
    (filter (comp not even?) sequence)))

(defcheck solution-d29eb690
  (fn[l](filter #(=(mod % 2)1) l)))

(defcheck solution-d3614480
  filter #(= (mod % 2) 1))

(defcheck solution-d3fc82f9
  (fn [l]
    (filter odd? l)
    ))

(defcheck solution-d5f328de
  filter #(= 1 (mod %1 2)))

(defcheck solution-d67e361e
  filter #(not= 0 (rem % 2)))

(defcheck solution-d77d12f0
  #(filter (fn [x] (= (rem x 2) 1)) %))

(defcheck solution-d8723e8
  (fn
    [list]
    (filter #(= 1 (mod %1 2)) list)))

(defcheck solution-d984f1ec
  (fn [x]
    (filter odd? x)))

(defcheck solution-d9a6c849
  (fn [v] (filter odd? v)))

(defcheck solution-da279d98
  (fn [s] (filter #(= 1 (rem % 2)) s)))

(defcheck solution-da4f2192
  (fn [coll]
    "Write a function which returns only the odd numbers from a sequence."
    (filter odd? coll)))

(defcheck solution-da849362
  (fn [x] (loop [[h & t] (into [] x) acc []] (let [nacc (if (odd? h) (conj acc h) acc)] (if (= t nil) nacc (recur t nacc))))))

(defcheck solution-dc1958b8
  #(remove (fn [x] (= 0 (mod x 2))) %1))

(defcheck solution-dc343b72
  #(for [item % :when (odd? item)] item))

(defcheck solution-dc91aa4f
  (fn [arg] (filter odd? arg)))

(defcheck solution-de18807e
  (fn [nums]
    (filter #(= 1 (mod % 2)) nums)))

(defcheck solution-df4b7fc5
  filter #(= (rem % 2) 1))

(defcheck solution-e1fc828b
  filter #(odd? %))

(defcheck solution-e270c648
  (fn [coll]
    (loop [coll coll odd-coll []]
      (if (empty? coll)
        odd-coll
        (recur (rest coll) (if (odd? (first coll))
                             (conj odd-coll (first coll))
                             odd-coll))))))

(defcheck solution-e3868b4a
  (fn odds [x]
    (remove even? x)))

(defcheck solution-e4144a8
  (partial remove even?))

(defcheck solution-e6000d8d
  (fn find-odds [x]
    (filter odd? x)
    ))

(defcheck solution-e6d49f28
  (fn [seq] (filter #(= 1 (rem % 2)) seq)))

(defcheck solution-e7beea48
  (fn hey ([x] (hey x [])) ([x y] (if (empty? x) y (if (= (mod (first x) 2) 0) (hey (rest x) y) (hey (rest x) (concat y (list (first x)))))))))

(defcheck solution-e8a98539
  (fn [coll]
    (filter #(= 1 (mod % 2))
      coll)))

(defcheck solution-eb73787
  (fn [xs]
    (filter odd? xs)))

(defcheck solution-eec64454
  #(filter odd? %))

(defcheck solution-efbb26f4
  (fn [l] (filter #(= 1 (mod % 2)) l)))

(defcheck solution-f3fb66f1
  (fn [seq] (filter odd? seq)))

(defcheck solution-f4ab9874
  reduce #(if (odd? %2) (conj % %2) %) [])

(defcheck solution-f4c0d27b
  (fn [seq] (filter #(odd? (rem % 2)) seq)))

(defcheck solution-f5f9a289
  (fn [x] (filter #(= 1 (mod % 2)) x)))

(defcheck solution-f733dce5
  ;filter #(= (mod % 2) 1)

  filter odd?)

(defcheck solution-f734b67
  #(remove even? %))

(defcheck solution-fc7c0e3a
  filter #(not= (mod % 2) 0))

(defcheck solution-fdd7dd3d
  filter #(odd? %1))

(defcheck solution-fe4b6b94
  (fn impair [liste]
    (
      if (empty? liste)
      ()
      (if (= (rem (first liste) 2) 0)
        (impair (rest liste))
        (cons (first liste) (impair (rest liste)))
        )
      )
    ))

(defcheck solution-ffe55236
  filter (fn [x] (= 1 (rem x 2))))