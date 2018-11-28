(ns coal-mine.problem-90
  (:require [coal-mine.checks :refer [defcheck-90] :rename {defcheck-90 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1235258c
  #(set (for [v1 %1 v2 %2] [v1 v2])))

(defcheck solution-1241809f
  (fn [s1 s2] (set (for [k s1 v s2] [k v]))))

(defcheck solution-12a1109c
  (fn [col1 col2]
    (let [z (for [x col1 y col2] [x y])]
      (set z))))

(defcheck solution-12a46b03
  #(set (partition 2 (interleave (mapcat (partial repeat (count %2)) %1) (apply concat (repeat (count %1) %2))))))

(defcheck solution-13a16748
  (fn p90 [s1 s2]
    (set (mapcat
           (fn [x] (map (fn [y] (vector x y)) s2)) s1))))

(defcheck solution-13c45555
  #(set (for [a  %1 b %2] [a b])))

(defcheck solution-13e171b6
  (fn cartP
    [a b]
    (into #{} (for [s1 a
                    s2 b]
                [s1 s2]))))

(defcheck solution-140a6936
  (fn [s1 s2]
    (set (for [x1 s1
               x2 s2]
           [x1 x2]))))

(defcheck solution-155d2a3
  (fn [xs ys] (set (for [x xs y ys] [x y]))))

(defcheck solution-157acd8b
  (fn [sa sb] (into #{} (for [a sa b sb] (vector a b)))))

(defcheck solution-161c66c4
  (fn [a b]
    (reduce
      (fn [l ai]
        (apply conj l (reduce #(conj %1 [ai %2]) [] b)))
      #{} a)))

(defcheck solution-162a3323
  (fn [a b]
    (into #{} (mapcat (fn [v] (map (partial vector v) b)) a))))

(defcheck solution-16d50623
  (fn
    [xs ys]
    (set (mapcat #(map (partial cons %) (map list ys)) xs ))))

(defcheck solution-16f4f712
  (fn productX [x y] (set  (mapcat (fn[a] ((fn pairX [x a](map (fn[b] [a b]) x)) y a)) x))))

(defcheck solution-17128ada
  #(set (for [a %1 b %2] [a b])))

(defcheck solution-1803203f
  (fn [l1 l2]
    (set
      (reduce
        (fn [res item]
          (concat res (map #(conj [%] item) l1)))
        #{} l2))))

(defcheck solution-18de4b00
  (fn [set-a set-b] (set (for [a set-a b set-b] [a b]))))

(defcheck solution-18e86f45
  (fn [a b]
    (set (for [x a
               y b]
           [x y]))

    ))

(defcheck solution-1938007e
  (fn [a b] (set (mapcat #(map (fn [x] (vector % x)) b) a))))

(defcheck solution-19ab8556
  #(into #{} (for [s1elem %1 s2elem %2] [s1elem s2elem])))

(defcheck solution-19fe71cb
  (fn [xs ys]
    (apply hash-set
      (for [x xs
            y ys]
        [x y]))))

(defcheck solution-1a409af5
  (fn [xs ys]
    (set (for [x xs
               y ys]
           [x y]))))

(defcheck solution-1abad48d
  (fn [a b] (into #{} (for [x a y b] [x y]))))

(defcheck solution-1ac45912
  (fn [a b]
    (set (for [x a y b] [x y]))))

(defcheck solution-1b3d3e2
  (fn [a b]

    (set (for [i a j b]
           [i j]
           ))
    ))

(defcheck solution-1b8bc945
  (fn [set1 set2]
    (into #{} (for [a set1 b set2] [a b]))))

(defcheck solution-1c40ab9d
  (fn cartesian-product [set-a set-b]
    (set
      (for [a set-a
            b set-b]
        [a b]))))

(defcheck solution-1c704f4d
  (fn [s1 s2]
    (loop [x s2 cp #{}]
      (if (empty? x)
        cp
        (recur
          (rest x)
          (clojure.set/union
            cp
            (set (map vec (partition-all 2 (interleave s1 (repeat (first x))))))))
        )
      )
    ))

(defcheck solution-1d0b908f
  #( set (for [ x %1 y %2 ] [x y] )))

(defcheck solution-1d51f4e2
  (fn [x y] (set (for [a (vec x) b (vec y)] [a b]))))

(defcheck solution-1db515e6
  #(set (for [s % t %2] [s t])))

(defcheck solution-1e446a09
  #(-> (for [x %1 y %2] [x y]) set))

(defcheck solution-1e536bbb
  #(apply hash-set (for [s1 % s2 %2] [s1 s2])))

(defcheck solution-1e55a529
  #(into #{} (for [y %2 x %1] [x y])))

(defcheck solution-1e880932
  (fn [a b] (set (for [x a y b][x y]))))

(defcheck solution-1eaf8d1b
  (fn cp [x y]
    (reduce clojure.set/union #{}
      (for [e1 x
            e2 y]
        #{[e1 e2]}))
    ))

(defcheck solution-1ec0b04a
  #(set (partition 2 (mapcat (partial % %3) %2))) #(cons %2 (interpose %2 %)))

(defcheck solution-1f01bc7b
  #(set (for [i %, j %2] [i j])))

(defcheck solution-1fca53ad
  (fn [s1 s2]
    (set (for [a s1, b s2] [a b]))))

(defcheck solution-20e94130
  (fn [x y]
    (set (for [k x
               v y]
           [k v]))))

(defcheck solution-21fc67b1
  (fn [s1 s2]
    (set (for [x s1 y s2] [x y]))))

(defcheck solution-221b3cd0
  (fn carp [s1 s2]
    (set (for [i s1 j s2]
           [i j]))))

(defcheck solution-224d7e14
  (fn cort [a b]
    (reduce (fn [s item-a]
              (reduce (fn [s item-b]
                        (conj s [item-a item-b])) s b)) #{} a)))

(defcheck solution-246c7e32
  (fn [sa sb]
    (set

      (mapcat

        (fn [a]    (map #(vector a %) sb )     )

        sa)

      )
    ))

(defcheck solution-24ded797
  (fn [a b]
    (set (for [x a
               y b]
           [x y]))))

(defcheck solution-25681b89
  (fn [as bs] (into #{} (for [a as b bs]
                          (list a b)
                          ))))

(defcheck solution-25ab4c6b
  (fn [a b]
    (set (for [_a a _b b] [_a _b]))))

(defcheck solution-265afba6
  #(set(for [x %1, y %2] [x y])))

(defcheck solution-2794777b
  (fn
    [s1 s2]
    (into #{} (map vec (partition 2 (interleave (mapcat #(repeat (count s2) %) s1) (take (* (count s2) (count s1)) (cycle s2))))))))

(defcheck solution-27aca6a
  (fn [coll0 coll1]
    (into #{}
      (mapcat #(map (partial vector %)
                 coll1)
        coll0))))

(defcheck solution-28ab5350
  (fn [s1 s2] (into #{} (for [x s1 y s2] [x y]))))

(defcheck solution-28c067e5
  (fn [x y]
    (set (mapcat (fn [x'] (map (fn [y'] [x' y']) y)) x))))

(defcheck solution-28ef3178
  (fn [as bs] (set (for [a as b bs] [a b]))))

(defcheck solution-2931d044
  (fn [set1 set2] (reduce (fn [a, b] (into a (map #(vector % b) set1))) #{} set2)))

(defcheck solution-2965a2f6
  (fn cartesian-product [a b]
    (into #{}
      (for [x a
            y b]
        [x y]))))

(defcheck solution-29689d6e
  (fn [left right]
    (->> (for [a left
               b right]
           [a b])
      (set))))

(defcheck solution-299c9cc2
  (fn[a b](set(for[x (range(count a)) y (range(count b))][(nth (vec a) x) (nth (vec b) y)]))))

(defcheck solution-29f0050f
  #(set
     (for [x %1 y %2]
       [x y])))

(defcheck solution-29f171bc
  (fn [x y]
    (set (for [s x, t y]
           [s t]))))

(defcheck solution-2a0d592d
  (fn [xs ys]
    (set
      (mapcat
        (fn [i] (map #(list i %) ys))
        xs))))

(defcheck solution-2a40bb15
  (fn [a b] (set (for [x a y b] [x y]))
    ))

(defcheck solution-2a6923fe
  (fn [c1 c2]
    (set
      (for [x c1 y c2]
        [x y]))))

(defcheck solution-2acf6a8a
  (fn [s1 s2]
    (set (for [e1 s1
               e2 s2]
           [e1 e2]))))

(defcheck solution-2b1d5138
  (fn [a b] (set (for [x a y b] (vector x y)))))

(defcheck solution-2b88e3d6
  (fn [c1 c2]
    (set (for [k1 c1, k2 c2] [k1 k2]))))

(defcheck solution-2bfa110d
  (fn [s1 s2]
    (set (for [x s1 y s2] [x y]))))

(defcheck solution-2c1c17a5
  (fn cart [x-coll y-coll]
    (apply hash-set (for [x x-coll y y-coll]
                      [x y]))
    ))

(defcheck solution-2c654ca4
  (fn [a b]
    (into #{} (for [x a y b] [x y]))))

(defcheck solution-2dae9d31
  (fn [s1 s2]
    (set (for [x s1 y s2]
           [x y]))))

(defcheck solution-2e445560
  #(set (for[x (seq %1) y (seq %2)] [x y])))

(defcheck solution-2f2b2294
  (fn  cartestian [xs ys]
    (set (for [x xs
               y ys] [x y]))))

(defcheck solution-2f88c75b
  #(into #{} (for [x %1, y %2] [x y])))

(defcheck solution-2fc161ae
  (fn cartesian
    [set1 set2]
    (into #{} (apply concat
                (map (fn [v1] (map (fn [v2] [v1 v2])
                                set2))
                  set1)))))

(defcheck solution-30122110
  #(set (for [a  %
              b %2]
          [ a b])))

(defcheck solution-30665506
  (fn sp [x y]
    (set (for [a x b y] [a b]))))

(defcheck solution-3069546c
  (fn [A B]
    (apply hash-set
      (for [x A y B]
        [x y]))))

(defcheck solution-30891a99
  (fn cartesian [acoll bcoll]
    (set
      (mapcat (fn [a]
                (map #(vector % a) acoll))
        bcoll))))

(defcheck solution-30be83c0
  (fn [a b]
    (->> (for [x a, y b]
           [x y])
      set)))

(defcheck solution-30fbe975
  #(set(for [a % b %2] [a b])))

(defcheck solution-310d333d
  #(into #{} (for [a %1 b %2] [a b])))

(defcheck solution-31b6467b
  (fn [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-32cb14d4
  #(set (for [a %1
              b %2]
          [a b])))

(defcheck solution-32d26a8a
  (fn [s1 s2]
    (set (mapcat (fn [x]
                   (map (fn [y]
                          [x y]
                          ) s2)
                   ) s1))
    ))

(defcheck solution-33202675
  (fn [s1 s2]
    (apply hash-set (for [x1 s1 x2 s2]
                      [x1 x2]))))

(defcheck solution-3358bcd4
  (fn [a b]
    (into #{} (for [x a
                    y b]
                [x y]))))

(defcheck solution-33dfa93d
  (fn [as bs]
    (set
      (for [a as b bs]
        [a b]))))

(defcheck solution-346cd94f
  (fn [s1 s2] (set (mapcat (fn [v] (map #(vector v %) s2)) s1))))

(defcheck solution-34c21af4
  #(set(for[x %1 y %2][x y])))

(defcheck solution-35981851
  (fn [a b] (set (mapcat (fn [x] (map (fn [y] [x y]) b)) a))))

(defcheck solution-35c694aa
  (fn [s1 s2]
    (set (for [t1 s1
               t2 s2]
           [t1 t2]))))

(defcheck solution-37889c57
  (fn [p1 p2]
    (letfn [(expandi
              [x y]
              (if (empty? x) []
                             (concat (expandj (first x) y) (expandi (rest x) y))))
            (expandj
              [x y]
              (if (empty? y) []
                             (concat [[x (first y)]] (expandj x (rest y))))
              )]

      (set (expandi p1 p2)))))

(defcheck solution-38641951
  (fn [s1 s2]
    (into #{} (apply concat (map #(map (partial vector %) s2) s1)))))

(defcheck solution-391d5cd7
  (fn [xs ys]
    (set
      (for [x xs y ys] [x y]))))

(defcheck solution-3a6885ab
  (fn [coll-1 coll-2]
    (set (for [x coll-1 y coll-2] [x y]))))

(defcheck solution-3b0aab29
  (fn [x y]
    (set (for [a x b y]
           [a b]))))

(defcheck solution-3b6c35c9
  #(into #{} (for [a %1 b %2] [a b])))

(defcheck solution-3c24f225
  (fn [set1 set2]
    (apply hash-set (for [x set1 y set2]
                      [x y]))))

(defcheck solution-3c27b572
  (fn [xs ys] (set (for [x xs, y ys] [x y]))))

(defcheck solution-3caef9fa
  (fn [a b] (let [row (fn [r m] (map #(vector m %) r))]
              (set (mapcat #(row b %) a)))))

(defcheck solution-3dd8b4a
  (fn [as bs]  (set (mapcat (fn [a] (map #(vector a %) bs)) as))))

(defcheck solution-3dfe7707
  (fn [xs ys]
    (set (for [x xs
               y ys]
           [x y]))))

(defcheck solution-3e669bb1
  #(reduce (fn [acc x] (into acc

                         (for [i %1] [i x])

                         ))

     #{} %2))

(defcheck solution-3efffa8d
  (fn my_prod [X Y]
    (loop [result (), X' (seq X)]
      (if X'
        (recur (concat result (map #(identity [(first X'),%]) Y))
          (next X'))
        (apply hash-set result)))))

(defcheck solution-3f729a75
  (fn [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))
    ))

(defcheck solution-3f9d7385
  (fn [a b]
    (set
      (for [x a
            y b]
        [x y]))))

(defcheck solution-401d6075
  (fn cart [x y] (loop [toMap (first x) result #{} toDo (rest x)]
                   (if (empty? toDo)
                     (into result (map (fn [z] (vector toMap z)) y))
                     (recur
                       (first toDo)
                       (into result (map (fn [z] (vector toMap z)) y))
                       (rest toDo))))))

(defcheck solution-40525fbf
  (fn [s1 s2]
    (apply hash-set (for [x s1 y s2 :let [xy (vector x y)]]
                      xy))))

(defcheck solution-406ddf6
  (fn cart-prod [s1 s2]
    (reduce (fn [accum val]
              (into accum (map #(vec [val %]) s2)))
      #{} s1)))

(defcheck solution-40b54433
  (fn [set1 set2]
    (set (for [val1 set1 val2 set2] [val1 val2]))))

(defcheck solution-40f6e5c6
  (fn [s1 s2]
    (into #{} (mapcat (fn [x] (map (fn [y] [x y]) s2)) s1))))

(defcheck solution-4177a406
  #(apply hash-set (for [x %1 y %2] [x y])))

(defcheck solution-419b7e56
  (fn [A B]
    (set (mapcat #(map (partial vector %) B) A))))

(defcheck solution-42026f4d
  #( set (for [x %1 y %2] [x y])))

(defcheck solution-421e51f1
  (fn [xs ys] (set (mapcat #(map list (repeat %) ys) xs))))

(defcheck solution-42c62345
  (fn [x y]
    (set (for [a x
               b y]
           [a b]))))

(defcheck solution-4306da6a
  #(into #{} (for [x %, y %2] [x y])))

(defcheck solution-4316bb6f
  (fn [xs ys]
    (into #{} (for [x xs y ys] [x y]))))

(defcheck solution-43184d04
  (fn
    [a b]
    (set (for [x a
               y b]
           [x y]))))

(defcheck solution-431ec3aa
  #(into #{} (mapcat (fn [x] (map (fn [y] (list x y)) %2)) %)))

(defcheck solution-437411a9
  #(set (for [x %
              y %2]
          [x y])))

(defcheck solution-43963e4
  (fn [s1 s2]
    (reduce (fn [s,e] (reduce #(conj %1 [e %2]) s s2)) #{} s1)))

(defcheck solution-43f50af0
  (fn [xs ys]
    (set
      (for [x xs
            y ys]
        [x y]))))

(defcheck solution-4404ed50
  (fn [xs ys] (set (mapcat #(map (partial vector %) ys) xs))))

(defcheck solution-444dda6f
  (fn [collx colly]
    (loop [xs collx ys colly acc #{}]
      (if (empty? xs)
        acc
        (if (empty? ys)
          (recur (rest xs) colly acc)
          (recur xs (rest ys) (conj acc [(first xs) (first ys)])))))))

(defcheck solution-4468c566
  (fn cp [set_a set_b]
    (set (for [x set_a
               y set_b]
           [x y]))))

(defcheck solution-44843c4
  (fn [a b](set (for [x a y b] [x y]))))

(defcheck solution-44a54b5e
  (fn [xs ys]
    (into #{} (for [x xs, y ys] [x y]))))

(defcheck solution-45585bc0
  (fn [s1 s2]
    (reduce
      (fn [acc e1]
        (reduce
          (fn [acc e2] (conj acc [e1 e2]))
          acc s2))
      #{} s1)))

(defcheck solution-45f328b9
  (fn [set1 set2]
    (set (apply concat (map (fn [item1] (map (fn [item2] [item1 item2]) set2)) set1)))))

(defcheck solution-45f75b88
  (fn [a b]
    (reduce
      (fn [acc i]
        (into acc (
                    reduce
                    (fn [acc2 i2]
                      (conj acc2 [i i2])
                      )
                    #{}
                    b
                    )
          )
        )
      #{}
      a
      )
    ))

(defcheck solution-4621f6d6
  (fn [xs ys]
    (into #{}
      (for [x xs
            y ys]
        [x y]))))

(defcheck solution-46633e49
  #(set (for [ x %1 y %2] [x y])))

(defcheck solution-475d8038
  (fn [v1 v2] (set (for [i v1 j v2] [i j]))))

(defcheck solution-4798d015
  (fn [s1 s2]
    (set(for [x s1 y s2] [x y]))))

(defcheck solution-47fb5f0f
  (fn f [x y]
    (into #{} (mapcat #(map (partial vector %) y) x))))

(defcheck solution-485278e8
  (fn [s1 s2]
    (set (for [a s1 b s2] [a b]))))

(defcheck solution-48c1955c
  (fn cartesian-product [s1 s2]
    (set (apply concat (map #(for [i s2] [% i]) s1)))))

(defcheck solution-48c35382
  (fn [a b] (set (mapcat #(map vector (repeat %) b) a))))

(defcheck solution-48ccd84e
  (fn [set1 set2]
    (set (reduce (fn [acc e1]
                   (concat acc (map #(vector e1 %1) set2)))
           [] set1))))

(defcheck solution-4900095b
  (fn [a b] (set (mapcat #(for [y a] [y %]) b))))

(defcheck solution-4a67e006
  (fn cartX [c1 c2]
    (->> (for [x c1
               y c2]
           [x y])
      set)))

(defcheck solution-4a7995b9
  (fn [s1 s2]
    (into #{}
      (for [x s1 y s2]
        [x y]))))

(defcheck solution-4ab57036
  (fn [s1 s2] (set (for [x s1 y s2] (vector x y)))))

(defcheck solution-4ad5ab4a
  (fn [coll1 coll2]
    (set (for [item1 coll1 item2 coll2] [item1 item2]))))

(defcheck solution-4b5ef58f
  (fn [coll1 coll2] (reduce (fn [acc x] (reduce (fn [acc2 y] (conj acc2 (vector x y))) acc coll2)) #{} coll1)))

(defcheck solution-4c3a2574
  #(set (for [a %1 b %2]
          [a b])))

(defcheck solution-4cec0f1e
  (fn [as bs]
    (set (for [a as b bs] [a b]))))

(defcheck solution-4d186ab1
  (fn [a b]
    (set (apply concat (map #(map (fn [x] [% x]) b) a)))))

(defcheck solution-4d65fb26
  (fn cartesian [s1, s2]
    (apply hash-set
      (apply concat
        (for [i1 s1]
          (for [i2 s2]
            (vector i1 i2)))))))

(defcheck solution-4d6e0946
  #(into #{} (for [a % b %2] [a b])))

(defcheck solution-4ddb9865
  (fn [xs ys]
    (into #{} (mapcat
                (fn [x]
                  (map #(vector x %) ys))
                xs))))

(defcheck solution-4e157a0a
  (fn [ma mb]
    (set (for [a ma, b mb]
           [a b]
           ))))

(defcheck solution-4e581913
  #(set
     (for [x %
           y %2]
       [x y])))

(defcheck solution-4e58b2d
  (fn [x y]
    (set
      (mapcat
        (fn[a]
          (map
            (fn[b]
              [a b]) y)) x))))

(defcheck solution-4ebc30a1
  (fn cart-prod
    [left right]
    (let [vright (into [] right)]
      (loop [[h & t] (into [] left)
             accum []]
        (let [new-accum (into accum (map #(conj [] h %) vright))]
          (if (nil? t)
            (into #{} new-accum)
            (recur t new-accum)))))))

(defcheck solution-4ebe3c61
  (fn [xs ys]
    (reduce #(into %1 (map (fn [x] [x %2]) xs)) #{} ys)))

(defcheck solution-4ec24b04
  #(set (for [x %1 y %2 ] [x y])))

(defcheck solution-4f3227ae
  (fn [s1 s2] (
                set (reduce #( concat %1 (map (fn[x] (identity [x %2])) s1 )) #{} s2)
                )))

(defcheck solution-5008033
  (fn [xs ys] (set
                (for [x xs
                      y ys]
                  (vector x y)))))

(defcheck solution-50a3657c
  #(set (for [y %2 x %1] [x y])))

(defcheck solution-50d000d0
  #(set (for [a %, b %2] [a b])))

(defcheck solution-51066127
  (fn [ls rs]
    (set
      (for [l ls
            r rs]
        [l r]))))

(defcheck solution-521fdb6f
  #(set (for [x %1 y %2] (cons x [y]))))

(defcheck solution-5261b058
  (fn [s1 s2] (set
                (mapcat (fn [x] (map #(identity [% x]) s1)) s2))))

(defcheck solution-528613c7
  #(set
     (for [x %
           y %2]
       [x y])))

(defcheck solution-530c629b
  (fn [set1 set2]
    (set (for [x set1 y set2]
           [x y]))))

(defcheck solution-5407b04b
  (fn cartesian
    [A B]
    (set
      (loop [i 0 j 0 result []]
        (if (>= i (count A))
          result
          (recur (if (= (inc j) (count B)) (inc i) i) (if (= (inc j) (count B)) 0 (inc j)) (conj result [(nth (vec A) i) (nth (vec B) j)]))
          )
        )
      )
    ))

(defcheck solution-5454905f
  (fn[col1 col2]
    (set (for [a col1 b col2]
           [a b]) )
    ))

(defcheck solution-5458c841
  (fn [x y] (let [x (into [] x) y (into [] y)]
              (letfn [(iterate-once [x y] (set (map #(vector %1 %2) (repeat x) y)))
                      (iterate-all [x y idx] (iterate-once (nth x idx) y))
                      (add-to-set [x y acc idx] (union acc (iterate-all x y idx)))
                      (union [x y] (clojure.set/union x y))]
                (loop [idx 0
                       acc #{}]
                  (if (>= idx (count x))
                    acc
                    (recur (inc idx) (add-to-set x y acc idx))))))))

(defcheck solution-548330d4
  (fn x [set1 set2]
    (set (mapcat (fn [s1] (map (fn [s2] [s1 s2]) set2)) set1))))

(defcheck solution-54e2ccf3
  (fn cartesian-product [x y]
    (into #{} (for [a x b y] [a b]))))

(defcheck solution-5628dc65
  (fn [a b]
    (reduce clojure.set/union
      (map (fn [e] (apply hash-set (map #(vector e %) b))) a))))

(defcheck solution-56e713cf
  (fn prod [s1 s2]
    (into #{} (for [e1 s1, e2 s2]
                [e1, e2]))))

(defcheck solution-57231dc6
  (fn cartesianproduct [x y]
    (set (for [a x
               b y]
           (vector a b)))))

(defcheck solution-57337c45
  (fn outer-join [x y]
    (set (reduce concat (map (fn [a]
                               (map (fn [b]
                                      (vector a b)
                                      ) y)
                               ) x)))
    ))

(defcheck solution-58347c1d
  (fn [col1 col2]
    (loop [result #{} col col1]
      (if (empty? col)
        result
        (recur
          (reduce conj result (map #(vec [ (first col) % ] ) col2)) (rest col) )
        )
      )))

(defcheck solution-58c4ca39
  (fn [a b]
    (into #{} (for [y b x a] [x y]))))

(defcheck solution-58d53e5
  (fn [a b]
    (set (for [x a
               y b]
           [x y]))))

(defcheck solution-5945da0f
  (fn [set1 set2] (set (for [x set1 y set2] [x y]))))

(defcheck solution-596a4fc2
  (fn [colx coly] (set (for [x colx y coly] (vector x y)))))

(defcheck solution-597c73b6
  #(set (for [s1 %1 s2 %2] [s1 s2])))

(defcheck solution-59dc33bc
  #(set (for [x %1 y %2] (vector x y))))

(defcheck solution-59e71df1
  (fn [a b] (set (for [x a y b] [x y]))))

(defcheck solution-59e76d18
  #(set
     (for [x %1, y %2] [x y])))

(defcheck solution-5a3d137e
  (fn cartesian-product [s1 s2]
    (into #{} (for [a s1
                    b s2]
                [a b]))))

(defcheck solution-5a7bc65
  (fn[a b]
    (set (for[aa a bb b]
           [aa bb]
           ))))

(defcheck solution-5ae6260
  #(into #{} (for [x %1 y %2]  [x y])))

(defcheck solution-5ae6ba4f
  (fn [s1 s2]
    (reduce
      (fn [acc y] (clojure.set/union acc (set (map (fn [x] [y x]) s2))))
      #{}
      s1)))

(defcheck solution-5b015c09
  #(set (mapcat (apply juxt (map (fn [n] (partial conj [n])) (into [] %))) (into [] %2))))

(defcheck solution-5b0a2c5a
  (fn make-list [s1 s2]
    (set (for [x1 s1 x2 s2]
           (vector x1 x2)))))

(defcheck solution-5b1e9436
  #(set (for [l %1 r %2] [l r])))

(defcheck solution-5b3d28d1
  (fn carts [col1 col2]
    (let [vc1 (vec col1) vc2 (vec col2)]
      (set
        (reduce concat
          (for [i vc1]
            (map #(vector i %) vc2)))))))

(defcheck solution-5bd64fc1
  (fn f [x y] (if (empty? x) () (set (concat (map #(vector (first x) % ) y ) (f (rest x) y) )))))

(defcheck solution-5bde7e6
  (fn a [s1 s2]
    (let
     [union
      (fn [set1 set2]
        (if (< (count set1) (count set2))
          (reduce conj set2 set1)
          (reduce conj set1 set2)))]
      (if (empty? s1)
        #{}
        (set (union
               (map #(vec (list (first (vec s1)) %)) s2)
               (a (set (rest (vec s1))) s2)))))))

(defcheck solution-5c62ba0c
  (fn cart-prod [s1 s2]
    (into #{} (for [x1 s1
                    x2 s2]
                [x1 x2]))))

(defcheck solution-5c9a86b5
  (fn [xs ys]
    (into #{}
      (mapcat
        (fn [x] (map #(vector x %) ys))
        xs))))

(defcheck solution-5cac2258
  #(set
     (for [x %1
           y %2]
       [x y])))

(defcheck solution-5ccacf5e
  (fn [x y] (let [a (mapcat #(repeat (count y) %) x) b (->> (vec y) (repeat (count x)) (apply concat))]
              (set (map  vector a b )))))

(defcheck solution-5ceed347
  (fn [ss ts]
    (apply conj #{}
      (for [s ss t ts] [s t]))))

(defcheck solution-5d169927
  #(set (apply concat (map (fn [x] (map (fn [y] [x y]) %2)) %))))

(defcheck solution-5d2a4772
  #(letfn [(worker-child [x1 l2 n]
             (if (empty? l2)
               n
               (recur x1 (rest l2)
                 (conj n [x1 (first l2)]))))
           (worker [l1 l2 n]
             (if (empty? l1)
               n
               (recur (rest l1)
                 l2
                 (worker-child (first l1) l2 n))))]
     (worker %1 %2 #{})))

(defcheck solution-5d7beecd
  #(set (for [x %1 y %2]
          [x y])))

(defcheck solution-5d882447
  (fn cprod [s1 s2]
    (into #{}
      (mapcat (fn [a]
                (map #(vector a %) s2))
        s1))))

(defcheck solution-5d9d5f1e
  (fn [s1 s2]
    (let [pairs (for [x s1 y s2] [x y] )]
      (set pairs))))

(defcheck solution-5e920957
  #(set
     (for [x1 %1, x2 %2]
       [x1 x2])))

(defcheck solution-5e9e11a8
  (fn [A B]
    (set (for [a A b B] [a b]))))

(defcheck solution-5fbb66e8
  (fn my-cartesion-product
    [set1 set2]
    (set (mapcat (fn [x] (map #(vector x %) set2)) set1))))

(defcheck solution-60bb043e
  #(set (for [c1 %1 c2 %2] [c1 c2])))

(defcheck solution-60ccd698
  (fn [set2 set1]
    (into #{} (mapcat (fn [x]
                        (map #(vector % x)
                          set2))
                set1))))

(defcheck solution-6104decb
  #(set (for [a % b %2] [a b])))

(defcheck solution-613b15dd
  (fn [s1 s2]
    (into #{} (for [a s1 b s2] [a b]))
    ))

(defcheck solution-6180d4dd
  #(into #{} (for [x %1 y %2]
               [x y])))

(defcheck solution-6219e931
  (fn [a b]
    (set (reduce concat (map (fn [x] (map #(conj [x] %) b)) a)))))

(defcheck solution-63044794
  (fn [a b]
    (reduce into #{} (for [x b]
                       (for [y a]
                         [y x])))))

(defcheck solution-638e7ec
  (fn [s1 s2]
    (set (mapcat #(map vector (repeat %)
                    s2)
           s1))))

(defcheck solution-63a4bb9b
  (fn [c1 c2]
    (set (for [a c1
               b c2]
           [a b]))))

(defcheck solution-642f3cb1
  (fn [s t]
    (set
      (for [x s y t]
        [x y]))))

(defcheck solution-64b31914
  (fn myfun
    [set1 set2]

    (set (mapcat

           #(map vector set1 (repeat (count set1) %)) set2
           )  )
    ))

(defcheck solution-6549c9e6
  (fn [xs, ys]
    (reduce clojure.set/union
      (map
        (fn [x]
          (set (map #(vector x %) ys))
          )
        xs
        )
      )
    ))

(defcheck solution-65a6652e
  #(set (for [x %1
              y %2]
          [x y])))

(defcheck solution-660306e7
  (fn [s1 s2]
    (loop [tmps1 s1 news #{}]
      (if (empty? tmps1)
        news
        (recur (rest tmps1) (loop [tmps2 s2 tmpnews news]
                              (if (empty? tmps2)
                                tmpnews
                                (recur (rest tmps2) (conj tmpnews (concat (vector (first tmps1)) (vector (first tmps2))))))) )))))

(defcheck solution-66de7dd3
  (fn number90 [xs ys]
    (set
      (for [x xs
            y ys]
        [x y]))))

(defcheck solution-66e42e47
  #(set (for [x % y %2]  [x y])))

(defcheck solution-66fbdc19
  #(set (concat (for [v %1 w %2] [v w]))))

(defcheck solution-672b85a7
  (fn cart-prod
    [xs ys]
    (if (empty? xs)
      #{}
      (clojure.set/union (into #{} (map (partial vector (first xs)) ys)) (cart-prod (rest xs) ys)))))

(defcheck solution-67336db0
  #(into #{} (mapcat (fn [x] (map (juxt (constantly x) identity) %2)) %1)))

(defcheck solution-67bd3ac2
  (fn [x y] (into #{} (for [a x b y] (vector a b)))))

(defcheck solution-67ca061f
  (fn [a b] (set (for [x a y b] [x y]) )))

(defcheck solution-681e9464
  (fn cp [xs ys]
    (set
      (for [x xs y ys]
        (vector x y)))))

(defcheck solution-68dd0dc0
  (fn [seta setb]
    (set (for [a seta b setb]
           [a b]))))

(defcheck solution-695f7b9b
  (fn cartesian-product [s1 s2]
    (into #{}
      (for [x1 s1 x2 s2]
        (vector x1 x2)))))

(defcheck solution-6963e599
  (fn [set-a set-b] (set
                      (for [e1 set-a
                            e2 set-b]
                        [e1 e2]))))

(defcheck solution-6a40ab20
  #(set (for [a % b %2]
          [a b])))

(defcheck solution-6a95c75e
  (fn [s1 s2]
    (set
      (mapcat
        (fn [x] (map #(list x %) s2))
        s1))))

(defcheck solution-6a95dd67
  (fn [sa sb]
    (set (for [a sa, b sb] [a b]))))

(defcheck solution-6b1e4ffa
  (fn [x y]
    (->>
      (map #(map list x (repeat %)) y)
      (apply concat)
      set)))

(defcheck solution-6b3d6ae0
  (fn  [x y]
    (into #{}
      (for [a x b y]
        [a b]))))

(defcheck solution-6b5bd14
  #(apply hash-set(for [x %, y %2] [x y])))

(defcheck solution-6b65defb
  (fn [col1 col2]
    (into #{} (for [x col1
                    y col2]
                [x y]))))

(defcheck solution-6bccc306
  (fn [setA setB]
    (let
     [permute
      (fn [func colA colB]
        (mapv func
          (cycle colA)
          (let [countA (count colA)]
            (reduce
              (fn [prev item]
                (concat prev (replicate countA item))
                )
              [] colB)
            ))
        )
      ]
      (set (permute (fn [a b] (vector a b)) setA setB)))
    ))

(defcheck solution-6bf9430e
  (fn [s1 s2]
    (set
      (for [x s1 y s2]
        [x y]))))

(defcheck solution-6ca40e3b
  (fn [s1 s2]
    (set (for [v1 s1 v2 s2] [v1 v2]))))

(defcheck solution-6cf554d8
  (fn [lhs rhs]
    (reduce (fn [result right]
              (reduce (fn [result left]
                        (conj result [left right]))
                result
                lhs))
      #{}
      rhs)))

(defcheck solution-6d39dd66
  (fn [xs ys]
    (set (for [x xs, y ys] [x y]))))

(defcheck solution-6d767522
  (fn [a b]
    (set
      (for [x a y b]
        [x y]))))

(defcheck solution-6dcf01b3
  (fn [s1 s2] (into #{} (for [x1 s1 x2 s2] [x1 x2]))))

(defcheck solution-6dd5fe49
  (fn product [xs ys]
    (let [>>= (fn [xs f]
                (apply concat (map f xs)))]
      (set (>>= xs #(for [y ys] [% y]))))))

(defcheck solution-6e48cc36
  (fn [left right]
    (into #{} (for [x left y right] [x y]))))

(defcheck solution-6eac581e
  (fn cartesian-product
    [s1 s2]
    (loop [s1 s1 s2 s2 res []]
      (if (first s2)
        (recur s1 (rest s2) (concat res (vec (map #(vector % (first s2)) s1))))
        (set res)))))

(defcheck solution-6ec714b6
  (fn [s1 s2]
    (loop [cur (first s1) rst (rest s1) res #{}]
      (if (nil? cur) res
                     (recur (first rst) (rest rst) (clojure.set/union res (loop [cur2 (first s2) rst2 (rest s2) res2 #{}]
                                                                            (if (nil? cur2) res2
                                                                                            (recur (first rst2) (rest rst2) (conj res2 [cur cur2]))
                                                                                            )
                                                                            )))
                     )
      )
    ))

(defcheck solution-6ee23fc7
  #(into #{} (for [x %1 y %2] [x y])))

(defcheck solution-6efb898b
  (fn [as bs] (set (mapcat (fn [a] (map #(list a %) bs)) as))))

(defcheck solution-6f7fd1c3
  (fn
    [a b]
    (let [t (fn [a b]
              (map #(vector a %) b))]
      (set (loop [result []
                  a a]
             (if (= (count a) 0)
               result
               (recur (concat result (t (first a) b)) (rest a)))


             )))))

(defcheck solution-6fbe9749
  (fn [xs ys] (set (apply concat (map #(map (fn [x] [% x]) ys) xs)))))

(defcheck solution-6fecb3eb
  (fn [a b]
    (set (for [a' a b' b] [a' b']))))

(defcheck solution-70ab694d
  (fn [a b] (set (mapcat
                   #(map list (repeat %) b)
                   a))))

(defcheck solution-70e70784
  (fn [sx sy]
    (into #{}
      (for [x sx, y sy]
        [x y]))))

(defcheck solution-718740e9
  (fn [a b]
    (set (mapcat #(map vector (repeat %1) b) a))))

(defcheck solution-71dba59b
  (fn [c1 c2] (apply hash-set (for [x c1 y c2] [x y]))))

(defcheck solution-71f6a6b9
  (fn [as, bs] (set (for [a as, b bs] [a, b]))))

(defcheck solution-71fee328
  (fn crossp [l1 l2]
    (loop [result #{}, l1 l1]
      (if (empty? l1)
        (set result)
        (recur (into (map vector (repeat (first l1)) l2) result) (rest l1))))))

(defcheck solution-7235dd7f
  (fn cartesian
    ([x y]
     (cartesian x y #{}))
    ([x y z]
     (let [create-vectors (fn [a b c]
                            (if (= 0 (count a))
                              c
                              (recur (rest a) b (conj c (vector b (first a))))))]
       (if (= 0 (count x))
         z
         (recur (rest x) y (apply conj z (create-vectors y (first x) []))))))))

(defcheck solution-72477b49
  (fn [a b] (into #{} (reduce (fn [s x]
                                (concat s (map (fn [y]
                                                 [x y])
                                            b) ))
                        [ ] a))))

(defcheck solution-725f9956
  (fn [col1 col2] (set (for [i col1 v col2] [i v]))))

(defcheck solution-72ed8e00
  (fn [z z2]
    (set (for [x z y z2]
           [x y]))))

(defcheck solution-73740aa
  (fn [a b] (set (map vec (partition 2 (mapcat #(interleave (repeat %) b) a))))))

(defcheck solution-73c30210
  (fn [l1 l2] (into #{} (for [a l1 b l2] [a b]))))

(defcheck solution-743fb159
  (fn cartesian-product
    [s1 s2]
    (let [mapvec (fn [s v] (map #(vector % v) s))]
      (into #{} (reduce #(concat %1 (mapvec s1 %2)) #{} s2)))))

(defcheck solution-7486c303
  (fn [s1 s2]
    (into #{} (for [s1 s1
                    s2 s2]
                [s1 s2]))))

(defcheck solution-74d59131
  (fn [as bs]
    (set
      (for [a as b bs]
        [a b]))))

(defcheck solution-7506fba6
  (fn [xs ys]
    (set (for [x xs
               y ys]
           [x y]))))

(defcheck solution-75c10019
  (fn [a b]
    (loop [x a
           results (set [])]
      (if (empty? x) results
                     (recur (rest x) (into results (map (fn [j] [(first x) j]) b)))))))

(defcheck solution-75ebcfdf
  (fn [s1 s2]
    (loop [output []
           rs1 s1
           rs2 s2]
      (if (empty? rs1) (set output)
                       (if (empty? rs2)
                         (recur output (rest rs1) s2)
                         (recur (concat output [[(first rs1) (first rs2)]]) rs1 (rest rs2))
                         )))))

(defcheck solution-760f1d96
  (fn [a b]
    ((comp set mapcat) (fn [x] (map #(vector % x) a)) b)))

(defcheck solution-7662aec3
  (fn [a b]
    (set
      (for [c a d b]
        [c d]))))

(defcheck solution-766bdf4f
  #(into #{} (for [x %1 y %2] [x,y])))

(defcheck solution-775a18c2
  (fn [x1 x2] (set (for [i1 x1 i2 x2] [i1 i2]))))

(defcheck solution-776ffd0b
  (fn [x y]
    (letfn [(single [x1 y1] (into #{} (map #(into [] [%1 x1]) y1)))]
      (reduce #(into %1 (single %2 x)) #{} y)
      )))

(defcheck solution-77c29ece
  (fn [as bs]
    (into #{} (for [a as
                    b bs]
                [a b]
                )
      )))

(defcheck solution-7829627
  (fn [as bs] (set (mapcat (fn [a] (map #(vector a %) bs)) as))))

(defcheck solution-782ac1f9
  (fn [s1 s2] (reduce clojure.set/union (for [a s1 b s2] #{[a b]}))))

(defcheck solution-789aca57
  (fn [x y]

    (set (map vector (sort (take (* (count x) (count y)) (cycle x))) (take (* (count x) (count y)) (cycle y))))
    ))

(defcheck solution-7960047
  (fn [s1 s2]
    (set (for [e1 s1 e2 s2] [e1 e2]))))

(defcheck solution-79ccd658
  (fn [q pp] (set   (let [part (fn [ss e] (loop [s ss r #{}]
                                            (if (empty? s) r
                                                           (recur (rest s) (cons [(first s) e] r)))))]

                      (loop [p pp r #{}]
                        (if (empty? p) r
                                       (recur (rest p) (concat r (part q (first p))))))))))

(defcheck solution-79d20e55
  #(set (for [l (seq %1), ll (seq %2)]
          [l ll])))

(defcheck solution-7a1fb746
  #(set
     (for [x %1
           y %2]
       [x y])))

(defcheck solution-7a2152e6
  #(set (for [r %1  c  %2]  [r c])))

(defcheck solution-7a5e1d33
  (fn
    [s1 s2]
    (set (for [x s1 y s2] (vector x y)))))

(defcheck solution-7aa032ba
  (fn f
    [set1 set2]
    (let [join-element (fn [s elem] (map #(vector % elem) s))]
      (->> set2 (map (partial join-element set1)) (reduce concat) set))))

(defcheck solution-7b1f8cd1
  #(set (for [s1 %, s2 %2] [s1 s2])))

(defcheck solution-7b4027e9
  (fn [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-7b9f8b5e
  #(into #{} (map vector (apply concat (map (partial repeat (count %2)) %1)) (cycle %2))))

(defcheck solution-7ba349ed
  (fn [s1 s2] (set (apply concat (map (fn [i1] (map (fn [i2] [i1 i2]) s2)) s1)))))

(defcheck solution-7ca5fb4b
  (fn [a b]
    (into #{} (for [x a y b]
                [x y]
                ))
    ))

(defcheck solution-7cba0164
  (fn [a b] (set (for [c a d b] [c d]))))

(defcheck solution-7d985173
  #(set
     (apply concat
       (for [x %1]
         (for [y %2]
           [x y])))))

(defcheck solution-7ded2562
  (fn m [a b]
    (into #{}
      (for [x a y b]
        [x y]))))

(defcheck solution-7e50a3a3
  (fn [s1 s2] (set (for [a1 s1 a2 s2] [a1 a2]))))

(defcheck solution-7e98a34a
  (fn prod [s1 s2]
    (set (for [x s1 y s2] [x y]))
    ))

(defcheck solution-7eb7c7d7
  (fn [a b]
    (set (for [i a j b] [i j]))))

(defcheck solution-7f22e08b
  (fn hey [x y]
    (if (empty? x)
      []
      (set (concat ((fn in [x y]
                      (if (empty? y) []
                                     (seq (concat (list (concat (list (first x))
                                                          (list (first y))))
                                            (in x (rest y)))))) x y)
             (hey (rest x) y))))))

(defcheck solution-7f2797bc
  (fn __ [a b]
    (set (for [aa a bb b] (vector aa bb)))))

(defcheck solution-7f732c6a
  (fn [s1 s2]
    (into #{} (mapcat (fn [x1] (map (fn [x2] [x1 x2]) s2)) s1))))

(defcheck solution-7f973dbc
  (fn [a b]
    (set (for [x a
               y b]
           [x y]))))

(defcheck solution-7f977540
  (fn [A B]
    (set (for [x A y B]
           [x y]))))

(defcheck solution-801e8484
  #(into #{} (for [face %
                   suite %2]
               [face, suite])))

(defcheck solution-807f4967
  (fn [a b]
    (set (for [x a y b] [x y]))))

(defcheck solution-815c3620
  (fn [u v]
    (set (reduce
           (fn [a x]
             (concat a
               (map #(vector x %) v)))
           [] u))))

(defcheck solution-8234c78e
  #(set (mapcat (fn [x] (map (partial vector x) %2)) %1)))

(defcheck solution-82c4b18b
  (fn [a b]
    (set (reduce
           (fn [ar el] (concat ar
                         (reduce
                           (fn [ar1 el1] (conj ar1 (vector el el1)))
                           [] b)
                         )
             )
           #{} a))
    ))

(defcheck solution-82d0bc88
  (fn cartesian [s1 s2]
    (set (for [y s2 x s1] [x y]))))

(defcheck solution-82e280a
  (fn cartesian [s1 s2]
    (set (for [i1 s1 i2 s2]
           [i1 i2]))))

(defcheck solution-82e3f064
  #(set (for[x %1, y %2] [x y])))

(defcheck solution-82eaf634
  (fn [a b]
    (into #{} (partition 2 (mapcat #(interleave (repeat (count b) %) b) a)))))

(defcheck solution-838baf7e
  (fn [sq1 sq2] (set (apply concat (map (fn [x] (map #(vector x %) sq2)) sq1)))))

(defcheck solution-8430bb34
  (fn [x y]
    (let [prod (fn [x y] (map #(vector x %) y))]
      (reduce into #{} (map #(prod % y) x)))))

(defcheck solution-843dfa4d
  (fn [a b] (into #{} (for [aa a bb b] [aa bb]))))

(defcheck solution-84b4e332
  (fn [s1 s2]
    (into #{} (mapcat (fn [e1] (map #(vector e1 %) s2)) s1))))

(defcheck solution-84c4d6e3
  (fn [s1 s2] (set (for [a s1
                         b s2]
                     [a b]))))

(defcheck solution-84fa1b15
  (fn [x y] (set (for [a x b y] [a b]))))

(defcheck solution-864acdf0
  (fn [s1 s2]
    (reduce
      (fn [a b]
        (into a
          (reduce
            #(conj %1 [b %2])
            () s2)))
      #{} s1)))

(defcheck solution-86791ff1
  #(set
     (for [x %1 y %2]
       [x y])))

(defcheck solution-8689ac55
  #(set(for[x % y %2][x y])))

(defcheck solution-87621c01
  (fn [xs ys]
    (into #{} ((fn cartesian [xs ys]
                 ;; NOTE: This inner function works for arbitrary sets, even infinite ones.
                 ;; It returns a lazy seq rather than a set.
                 ;; For an explanation, see: https://mulk.eu/j/126
                 (let [skip (gensym)]
                   (letfn [(reverse-prefices [items]
                             (reductions conj nil items))
                           (pad [items]
                             (lazy-cat items (repeat skip)))
                           (skip? [x]
                             (identical? skip x))]
                     (let [xpres (rest (reverse-prefices (pad xs)))
                           ypres (map reverse (rest (reverse-prefices (pad ys))))]
                       (apply concat
                         (take-while          ;deal with the finite case
                           seq
                           (map (fn [xprefix yprefix]
                                  (filter (fn [pair] (not (some skip? pair)))
                                    (map vector xprefix yprefix)))
                             xpres
                             ypres))))))) xs ys))))

(defcheck solution-87d25cc5
  (fn [s1 s2]
    (set (for [v1 s1
               v2 s2] [v1 v2]))))

(defcheck solution-87dedd97
  (fn cart [s1 s2]
    (set (for [x s1 y s2] [x y]))))

(defcheck solution-88071e4f
  (fn cartes [x y]
    (set
      (mapcat (fn [a] (map #(vector a %) y)) x))))

(defcheck solution-89bd5099
  (fn [xs ys]
    (set
      (for [x (seq xs) y (seq ys)]
        [x y]))))

(defcheck solution-8a6190a9
  #(set
     (for[x %1, y %2]
       [x y])))

(defcheck solution-8b57922d
  (fn cp [c1 c2]
    (loop [s2 c2, result []]
      (if (empty? s2)
        (set (apply concat result))
        (recur (rest s2) (conj result (for [x c1] [x (first s2)])))))))

(defcheck solution-8b82f853
  (fn [l1 l2]
    (into #{}(apply concat (map (fn [a]
                                  (map (fn [b] [a b]) l2)) l1)))))

(defcheck solution-8ba89802
  (fn [x y] (into #{} (for [x x y y] [x y]))))

(defcheck solution-8c559260
  (fn [s1 s2]
    (set (mapcat (fn [x] (map #(vector x %) s2)) s1))))

(defcheck solution-8d43378
  (fn cart [x y]
    (set (for [a x b y] [a b]))))

(defcheck solution-8e489791
  #(set (for [i % j %2] (vector i j))))

(defcheck solution-8e50fe54
  (fn [a b]
    (set (apply concat
           (map
             (fn [s] (map
                       (partial list s)
                       b))
             a)))))

(defcheck solution-8e7351e2
  (fn [s1 s2]
    (set (for [a s1 b s2]
           [a b]))
    ))

(defcheck solution-8e8e478a
  #(reduce into #{}
     (for [a %1 b %2
           :let [z #{[a b]}]]
       z)))

(defcheck solution-8eae3192
  (fn cartesian [s1 s2]
    (if (seq s1)
      (set (concat (map #(vector (first s1) %) s2)
             (cartesian (rest s1) s2)))
      nil)))

(defcheck solution-8ee9a1c2
  #(set (for [x %1
              y %2]
          [x y])))

(defcheck solution-8f59a7a5
  (fn cp [s1 s2] (set (mapcat identity (for [x s1] (for [y s2] [x y]))))))

(defcheck solution-8f7ff8d2
  (fn [a b] (set (for [y b x a] [x y]))))

(defcheck solution-900bf71
  (fn [c-a c-b]
    (set (for [a c-a
               b c-b]
           [a b]))))

(defcheck solution-90146af2
  (fn [x y]
    (into #{}
      (mapcat
        (fn [a] (map (fn [b] [a b]) y))
        x))))

(defcheck solution-90394966
  (fn[x  y] (into #{}
              (for [ a x b y]
                [a b]
                )
              )))

(defcheck solution-90f3ae80
  (fn [a b] (apply hash-set (for [x a y b] [x y]))))

(defcheck solution-914cbc02
  #(set
     (for [e1 %1, e2 %2]
       [e1 e2])))

(defcheck solution-91989249
  (fn cartesian [xs ys]
    (set (for [x xs, y ys]
           [x y]))))

(defcheck solution-9288b41e
  #(into #{} (for [i %1 j %2] (vec [i j]))))

(defcheck solution-92b865b1
  (fn [a b](set(mapcat (fn [a](map (fn[b][a b]) b))a))))

(defcheck solution-92cb6f96
  (fn [s1, s2] (set (apply concat (map #(map vector (seq s1) (repeat %)) (seq s2))))))

(defcheck solution-933ed1f1
  (fn test [x y]
    (reduce #(apply conj %1 %2) (map
                                  (fn [a]
                                    (into #{} (map #(conj [] a %) y))
                                    ) x))))

(defcheck solution-93b90acb
  (fn cartesian-product [s1 s2]
    (apply hash-set
      (for [x1 s1
            x2 s2]
        [x1 x2]))))

(defcheck solution-93ce6e8
  #(into #{} (for [i %1 j %2] [i j])))

(defcheck solution-941acb07
  (fn [x y] (reduce (fn [acc e] (clojure.set/union acc (set (map #(list e %) y)))) #{} x)))

(defcheck solution-9436acdc
  #(set (for [y %2 x %] [x y])))

(defcheck solution-95113d43
  (fn [a b]
    (set (apply concat (map (fn [x] (map (fn [y]  [x y]) b)) a)))))

(defcheck solution-952a93eb
  (fn crossprod[a b] (let[size ( * (count a) (count b))]
                       (apply hash-set(map vector  (apply interleave (repeat (count b) a )) (take size (cycle b)))))))

(defcheck solution-95c4f866
  (fn [sa sb]
    (into #{} (for [a sa
                    b sb]
                [a b]))))

(defcheck solution-96b90daa
  (fn cartesian [a b] (apply hash-set (mapcat #(map (fn [c] (vector c %)) a) b))))

(defcheck solution-9712524a
  (fn [A B]
    (into #{} (mapcat (fn [a] (map (fn [b] [a b]) B)) A))))

(defcheck solution-975fad01
  #(apply hash-set (for [c1 %1 c2 %2]  [c1 c2])))

(defcheck solution-97a202b8
  (fn [c1 c2] (set (mapcat (fn [x] (map (fn [y] [x y]) c2)) c1))))

(defcheck solution-995b448e
  (fn [x y]
    (into #{}
      (for [i x j y]
        [i j]
        ))))

(defcheck solution-9992c62d
  (fn [s1 s2]
    (into #{} (for [x1 s1
                    x2 s2]
                (list x1 x2)))))

(defcheck solution-9a15b532
  #(set (for [x (seq %1), y (seq %2)] [x y])))

(defcheck solution-9b6e39a8
  (fn [s1 s2]
    (into #{} (for [e1 s1
                    e2 s2]
                [e1 e2]))))

(defcheck solution-9b98c17f
  (fn [s s2] (set (mapcat (fn [x] (map (fn [y] [x y]) s2)) s))))

(defcheck solution-9ba6e786
  (fn [s1 s2]
    (set
      (for [v1 s1 v2 s2]
        [v1 v2]))))

(defcheck solution-9bd7c8a9
  (fn cp [xs ys]
    (set
      (mapcat
        (fn [x]
          (map
            (fn [y] [x y])
            ys))
        xs)
      )))

(defcheck solution-9bebe8af
  (fn [s1 s2] (set (mapcat #(map (fn [x] (vector % x)) s2) s1))))

(defcheck solution-9d0b99bc
  #(into #{}
     (let [x (seq %)
           y (seq %2)]
       (for [i (range (count x))
             j (range (count y))]
         [(nth x i) (nth y j)]))))

(defcheck solution-9d6bb447
  (fn [s1 s2] (set (for [e1  s1, e2  s2] [e1 e2]))))

(defcheck solution-9d764bde
  (fn cart-prod [s t]
    (set (mapcat (fn [x] (map (fn [y] [x y]) t)) s))))

(defcheck solution-9e22ba45
  (fn [x y]
    (set (apply concat
           (for [xx x]
             (set (map #(vector xx %1) y)))))))

(defcheck solution-9e23b6a2
  (fn [xs ys]
    (reduce (fn [zs y] (apply conj zs (map (fn [x] [x y]) xs)))
      #{}
      ys)))

(defcheck solution-9eabbeee
  (fn [s1 s2]
    (reduce (fn [r1 e1]
              (reduce (fn [r2 e2]
                        (conj r2 [e1 e2]))
                r1
                s2))
      #{}
      s1)))

(defcheck solution-9eaf217
  (fn [a b]
    (set (for [x a
               y b]
           (conj [] x y)))))

(defcheck solution-9f0f655d
  (fn cart-prod [s1 s2]
    (set (for [x s1 y s2]
           [x y]))))

(defcheck solution-a04c2883
  (fn [a b]
    (set
      (for [a a b b]
        [a b]
        )
      )
    ))

(defcheck solution-a0615dcc
  #(set (for [x %1 y %2] [x y])))

(defcheck solution-a1b0bd17
  (fn [c1 c2]
    (into #{} (for [i2 c2 i1 c1] [i1 i2]))))

(defcheck solution-a228f7d4
  #(into #{}
     (mapcat (fn [y]
               (map (fn [x] [x y])
                 %1))
       %2)))

(defcheck solution-a269b48d
  (fn [A B]
    (set (for [x A
               y B]
           [x y]))))

(defcheck solution-a28fe20c
  #(apply sorted-set
     (for [x %1 y %2]
       (vector x y))))

(defcheck solution-a494d162
  #(set (for [i1 %1
              i2 %2]
          [i1 i2])))

(defcheck solution-a4eea721
  (fn cart-prod
    [s1 s2]
    (let [cart-step (fn [x] (map #(vector x %) s2))]
      (into #{} (apply concat (map cart-step s1))))))

(defcheck solution-a54ecec1
  (fn [s1 s2] (set (for [e1 s1 e2 s2] (vector e1 e2)))))

(defcheck solution-a59c1f64
  (fn [s1 s2]
    (set (mapcat #(map list s1 (repeat %)) s2))))

(defcheck solution-a5a338eb
  #(set( for [a % b %2] [a b])))

(defcheck solution-a5e98da9
  (fn p [col1 col2]
    (apply hash-set (for [i col1 j col2]
                      [i j]))))

(defcheck solution-a6642e33
  (fn [a b]
    (into #{}
      (for [x a y b] (vector x y)))))

(defcheck solution-a70b7f50
  #(set (for [x (vec %1) y (vec %2)] [x y])))

(defcheck solution-a7442404
  #(set (for [i %1 j %2]
          [i j])))

(defcheck solution-a74bf593
  #(into #{}
     (for [a %1 b %2]
       [a b])))

(defcheck solution-a75226bf
  #(set (mapcat (fn [x] (map (fn [y] (list x y)) %2)) %1)))

(defcheck solution-a781b90
  #(set(for [x % y %2] [x y])))

(defcheck solution-a7c486d4
  (fn [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-a82fd397
  #(set (for [a %1 b %2] (vector a b))))

(defcheck solution-aa6c9b48
  (fn [as bs] (into #{} (for [a as b bs] [a b]))))

(defcheck solution-ab2a1570
  (fn [xs ys]
    (set
      (apply concat
        (for [x xs]
          (map #(vector x %) ys))))))

(defcheck solution-ab2ef3c2
  (fn peu [x y] (set (if (empty? x) '() (concat (map #(vector (first x) %) y) (peu (rest x) y))))))

(defcheck solution-ac153175
  (fn [A B]
    (into #{} (for [a A b B] [a b]))))

(defcheck solution-ac3c827
  (fn my-function [s1 s2]
    (set (for [a s1 b s2] (vector a b)))))

(defcheck solution-ac80fb16
  #(into #{} (for [a %1, b %2] (list a b))))

(defcheck solution-ad2a95a7
  (fn [s1 s2]
    (set (for [a s1 b s2]
           [a b]))))

(defcheck solution-ad8508a4
  #(set(for[x %1 y %2][x y])))

(defcheck solution-ad9cf2fe
  (fn cart [a b]
    (set (mapcat (fn [m] (map #(vector m, %) b) ) a))))

(defcheck solution-ae503991
  (fn cartesian-product [sa sb]
    (letfn [(doa [a] (reduce #(conj % [a %2]) #{} sb))]
      (reduce #(clojure.set/union % (doa %2)) #{} sa))))

(defcheck solution-ae6eb3f6
  (fn [set1 set2]
    (loop [s1 set1, res #{}]
      (if (empty? s1)
        res
        (recur
          (rest s1)
          (apply (partial conj res) (map (fn [it] [(first s1) it]) set2)))
        ))))

(defcheck solution-ae84b289
  (fn [la lb] (set(mapcat (fn [a] (map #(vector a %) lb)) la))))

(defcheck solution-ae85e2ea
  (fn [a b]
    (set (for [ia a ib b] [ia ib]))))

(defcheck solution-ae8fff4d
  #(set (for [e1 %1 e2 %2] [e1 e2])))

(defcheck solution-aed31520
  (fn [a b] (set (mapcat (fn [x] (map #(vector x %) b)) a))))

(defcheck solution-af407cc8
  (fn [a b]
    (set (for [x a y b]
           [x y]))))

(defcheck solution-afb7d375
  (fn [s1 s2]
    (set (for [e1 s1
               e2 s2]
           [e1 e2]))))

(defcheck solution-aff7602
  (fn [s1 s2]
    (set (for [i1 s1 i2 s2]
           [i1 i2]))))

(defcheck solution-b0053f85
  (fn zip [xs ys]
    (letfn [(f [xs ys]
              (if (empty? xs) nil
                              (into (map (fn [x] [(first xs) x]) ys) (f (rest xs) ys))))]
      (set (f xs ys)))))

(defcheck solution-b12e3b0c
  (fn [set1 set2]
    (reduce
      #(into %1 %2)
      #{}
      (map
        (fn [item2]
          (map
            (fn [item1] [item1 item2])
            set1))
        set2))))

(defcheck solution-b14753a9
  (fn cart-prod
    [s1 s2]
    (into #{}
      (for [e1 s1, e2 s2]
        [e1 e2]))))

(defcheck solution-b18a85f0
  (fn [s1 s2]
    (reduce (fn [s e] (apply conj s (map #(conj [e] %1) s2))) #{} s1)))

(defcheck solution-b1b893a2
  #(into #{} (for [i1 %1 i2 %2]  [i1 i2])))

(defcheck solution-b1f213ed
  (fn cp [s1 s2]
    (set (for [a s1
               b s2]
           [a b]))))

(defcheck solution-b1f4dc5a
  (fn [x y]
    (set
      (mapcat
        (fn [x1] (map #(list x1 %) y))
        x))))

(defcheck solution-b2abd541
  (fn [s1 s2]
    (into #{} (for [x s1 y s2] [x y]))))

(defcheck solution-b30fb8e8
  #(apply hash-set (for [a %1 b %2] [a b])))

(defcheck solution-b31051a0
  (fn prod [s1 s2]
    (into #{} (for [a s1 b s2] [a b]))))

(defcheck solution-b3164b9b
  (fn [a b]
    (set
      (mapcat (fn [i] (map (fn [j] [i j]) b)) a)
      )
    ))

(defcheck solution-b3d93caf
  (fn [u v] (into #{} (mapcat #(map (partial vector %) v) u))))

(defcheck solution-b423b332
  (fn [set1 set2]
    (set (for [x set1 y set2] (vector x y)))))

(defcheck solution-b4400398
  (fn [l1 l2]
    (let [n (* (count l1) (count l2))]
      (into #{}
        (map vector
          (sort (take n (cycle l1))) (take n (cycle l2)))))))

(defcheck solution-b464b3dd
  (fn [xs ys]
    (set (for [x xs y ys] [x y]))))

(defcheck solution-b4c32c97
  #(into #{} (for [f %1 s %2] [f s])))

(defcheck solution-b52e65
  (fn [a b]
    (reduce (fn [res a-i]
              (reduce #(conj %1 [a-i %2]) res b))
      #{}
      a)))

(defcheck solution-b54e54b4
  (fn [as bs]
    (set (for [x as, y bs] [x y]))))

(defcheck solution-b56a6c89
  (fn [s1 s2]
    (into #{} (for [e1 s1 e2 s2]
                [e1 e2]))))

(defcheck solution-b5964c6f
  (fn cart [as bs] (apply hash-set (mapcat (fn [a] (map #(vector a %) bs)) as))))

(defcheck solution-b69af289
  (fn cart-prod--reduce
    [& [s1 & more :as sets]] {:pre [(every? set? sets)]}
    (set (reduce (fn [s1 s2] (for [x s1, y s2] (conj x y)))
           (map vector s1)
           more))))

(defcheck solution-b6b91938
  (fn cartesian-product [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-b755fc91
  (fn [A B] (into #{} (for [a A b B] [a b]))))

(defcheck solution-b8981c92
  (fn cartesian-prod [s1 s2]
    (reduce (fn [s e1]
              (clojure.set/union s (set (map #(vector e1 %) s2))))
      #{}
      s1)))

(defcheck solution-b8fc71b6
  (fn product [marks ranks]
    (set
      (mapcat
        #(map (fn [rank] (vector % rank))
           ranks)
        marks))))

(defcheck solution-b94e446a
  (fn product [a b]
    (if (empty? a)
      '()
      (set (concat
             (map (fn [x] [(first a) x]) b)
             (product (rest a) b))))))

(defcheck solution-ba34e44a
  #(set
     (reduce
       (fn [xs x]
         (concat xs
           (partition 2
             (interleave (repeat (count %2) x) %2)
             )
           )
         )
       #{}
       %1
       )
     ))

(defcheck solution-ba8f3f71
  (fn [x y]
    (set
      (for [i x j y]
        [i j]))))

(defcheck solution-bac2b35d
  (fn [xs, ys] (into #{} (for [x xs, y ys] [x y]))))

(defcheck solution-bada0fe8
  (fn [as bs]
    (set (apply concat
           (map (fn [a]
                  (map (fn [b]
                         [a b])
                    bs))
             as)))))

(defcheck solution-bb8511cb
  (fn [s1 s2]
    (set (for [i s1, j s2] [i j]))))

(defcheck solution-bbb71a70
  #(set (for [x % y %2] [x y])))

(defcheck solution-bd143551
  (fn [x y] (into #{} (for [a x b y] [a b]))))

(defcheck solution-bdbaca4b
  (fn cart-prod [xs ys]
    (set (for [x xs y ys] [x y]))))

(defcheck solution-be41ecd4
  (fn [s1 s2](set (for [x s1 y s2] [x y]))))

(defcheck solution-be74ebd3
  (fn [x y] (-> (mapcat #(map (partial vector %) y) x) set)))

(defcheck solution-bedcb18a
  (fn cart-proj
    [s1 s2]
    (set (mapcat #(map vector (repeat %) s2) s1))
    ))

(defcheck solution-bf4689e1
  #(set (for [x %1
              y %2]
          [x y])))

(defcheck solution-bf8cff09
  #(into #{} (for [x %1 y %2] (list x y))))

(defcheck solution-c00efc16
  #(set (for [i %1 j %2] [i j])))

(defcheck solution-c0241a8f
  (fn cartesian-product [a b] (set (apply concat (map (fn [e] (map #(vector e %) b)) a)))))

(defcheck solution-c0e99281
  (fn cartesian-product [sa sb]
    (set (doall (for [a sa b sb]
                  [a b])))))

(defcheck solution-c1840ecf
  (fn [xs ys]
    (into #{} (for [x xs y ys] [x y]))))

(defcheck solution-c1ccf29b
  (fn [s1 s2]
    (set
      (reduce (fn [acc x] (concat acc
                            (map (fn [y] [x y]) s2)))
        []
        s1))))

(defcheck solution-c2855efe
  #(into #{} (for [x % y %2] [x y] )))

(defcheck solution-c2ce1560
  #(set (for [x1 %1 x2 %2] [x1 x2])))

(defcheck solution-c2ec0254
  (fn [s1 s2]
    (set (for [i s1 j s2] [i j]))))

(defcheck solution-c3795188
  #(->> (for [x %1 y %2] [x y]) (into #{})))

(defcheck solution-c3958a3c
  #(into #{} (for [x % y %2] [x y])))

(defcheck solution-c4a86369
  #(set (for [x % y %2][x y])))

(defcheck solution-c530c29d
  (fn cartesian- [s1 s2]
    "90. Write a function which calculates the Cartesian product of two
    sets."
    (set (mapcat (fn [s1e] (map (fn [s2e] [s1e s2e]) s2)) s1))
    ))

(defcheck solution-c54ac69
  #(set (for [a %1, b %2] [a b])))

(defcheck solution-c5520f5e
  (fn [s1 s2] (into #{} (for [ x s1 y s2] [x y]))))

(defcheck solution-c5785954
  (fn [a b]
    (set (mapcat
           #(map (partial vector %) b)
           a))))

(defcheck solution-c5afd3be
  (fn [a b]
    (into #{}
      (for [n a
            m b]
        [n m]))))

(defcheck solution-c5bfc6b
  (fn [seq1 seq2]
    (set
      (for [x seq1 y seq2]
        [x y]
        ))
    ))

(defcheck solution-c5ca247
  (fn [arg1 arg2] (into #{} (for [x arg1 y arg2] [x y]))))

(defcheck solution-c5f25eac
  (fn [a b]
    (into #{} (for [x a
                    y b]
                [x y]))))

(defcheck solution-c6421a9e
  (fn [as bs] (set (mapcat (fn [a] (map (fn [b] [a b]) bs)) as))))

(defcheck solution-c6436a60
  (fn [a b]
    (set (mapcat (fn [n] (map #(vector n %) b)) a))))

(defcheck solution-c7a88303
  (fn [s1 s2] (into #{} (#(for [x %1 y %2] [x y]) s1 s2))))

(defcheck solution-c7fd859e
  #(apply hash-set (for [ x %1 y %2 ] [ x y ] )))

(defcheck solution-c81fc2f6
  (partial
    (fn [acc i j]
      (let [f (fn [acc a b]
                (if (empty? b) acc
                               (recur (conj acc [a (first b)]) a (rest b))))]
        (if (empty? i) (set acc)
                       (recur (concat acc (f #{} (first i) j)) (rest i) j)))) #{}))

(defcheck solution-c8755ebd
  (fn [x y]
    (into #{} (for [s x t y] [s t]))))

(defcheck solution-c94e5f15
  #(set (for [i % j %2] [i j])))

(defcheck solution-c98d5b16
  (fn [as bs]
    (set
      (for [a as
            b bs]
        [a b]))))

(defcheck solution-ca9f2a6d
  (fn imperative-cart-prod
    [xs ys] (apply hash-set (for [x xs y ys :while (not (nil? x))] [x y]))))

(defcheck solution-cb50ce5a
  #(set (for [x %1
              y %2]
          [x y])))

(defcheck solution-cc15ee09
  (fn [l1 l2]
    (set (mapcat
           (fn [e] (map #(vector e %) l2))
           l1))))

(defcheck solution-cc268f59
  (fn cartesian-product [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-cc3c6303
  (fn cartesian-prod [a b]
    (set (for [x a, y b] (vector x y)))))

(defcheck solution-ccbbf900
  #(set (for [n %1 m %2] [n m])))

(defcheck solution-cd18b4e7
  #( into #{} (for [x %1 y %2] [x y] )))

(defcheck solution-ce24ac7a
  (fn [s1 s2] (set (for [e1 s1, e2 s2] [e1 e2]))))

(defcheck solution-ce41f494
  (fn [a b] (set (mapcat #(map vector a (repeat %)) b))))

(defcheck solution-ce6cdaba
  (fn [a b]
    (into #{}
      (for [ai a bi b]
        [ai bi]))))

(defcheck solution-ceaf5a01
  #(set (for [i %1 j %2] (vector i j))))

(defcheck solution-cee643e9
  (fn cart-prod [s1 s2]
    (set (for [x s1 y s2] [x y]))))

(defcheck solution-cfdc3812
  (fn cart [s1 s2]
    (set (map #(vector %1 %2) (mapcat #(repeat (count s2) %) s1) (cycle s2))
      )))

(defcheck solution-d016b8d8
  (fn [x y]
    (into #{} (for [a x b y] [a b]))))

(defcheck solution-d194db27
  #(into #{} (for [x %1
                   y %2]
               [x y])))

(defcheck solution-d20b28d4
  (fn fx [s1 s2] (let [xsprod (fn [x s] (into #{} (map (fn [d] (vec [x d])) s)))]
                   (let [fxr (fn [res ss1 ss2] (if (first ss1)
                                                 (do (into res (xsprod (first ss1) ss2))
                                                     (recur (into res (xsprod (first ss1) ss2)) (rest ss1) s2))
                                                 res))]
                     (fxr #{} s1 s2) ))))

(defcheck solution-d240bf50
  (fn [s1 s2]
    (apply hash-set (apply concat (map (fn [x] (map (fn [y] (vector x y)) s2)) s1)))))

(defcheck solution-d2788615
  (fn [a b]
    (set (mapcat (fn [x] (map #(vector x %) b)) a))))

(defcheck solution-d2ae4adc
  #(set
     (for [a %1
           b %2]
       [a b])))

(defcheck solution-d3ea5884
  (fn [R S] (set (for [r R s S] [r s]))))

(defcheck solution-d3f43c41
  (fn [a b] (into #{} (for [a a b b] [a b]))))

(defcheck solution-d454f6ef
  (fn [set1 set2] (into #{} (for [e1 set1 e2 set2] [e1 e2]))))

(defcheck solution-d49e76c5
  (fn [a b]
    (set (for [ea a
               eb b]
           [ea eb]))))

(defcheck solution-d502e3fb
  (fn [cards suits]
    (set
      (for [c cards s suits]
        [c s]))))

(defcheck solution-d55a8348
  (fn this [xs ys]
    (let [myflat (fn t [l]
                   (cond (= (count l) 0) '()
                         :else (reverse (concat (reverse (t (rest l)))
                                          (reverse (first l))))))]



      (set (myflat (for [i xs]
                     (for [j ys]
                       [i j])))))))

(defcheck solution-d57b3e2f
  (fn cartesian-product [set1 set2]
    (set (for [item1 set1 item2 set2]
           [item1 item2]))))

(defcheck solution-d597bc54
  (fn [a b]
    (set
      (for [x a
            y b]
        [x y]))))

(defcheck solution-d59cd682
  (fn cartesian-product [xs1 xs2]
    (set (for [x1 xs1 x2 xs2] [x1 x2]))))

(defcheck solution-d5c72793
  (fn [xs ys]
    (set (mapcat #(map (partial vector %)  ys) xs))))

(defcheck solution-d6134040
  #(set (for [s1 %1 s2 %2] (vector s1 s2))))

(defcheck solution-d6f4aadb
  (fn [s1 s2]
    (into #{}
      (for [e1 s1
            e2 s2]
        [e1 e2]))))

(defcheck solution-d7022280
  (fn [s1 s2]
    (set (mapcat #(map (fn [x] [% x]) s2) s1)
      )))

(defcheck solution-d7146d11
  (fn cartesianProduct[s1 s2]
    (set (apply concat (map (fn [el] (map #(vec [% el]) s1)) s2)))))

(defcheck solution-d7360c4e
  (fn [s1 s2] (into #{} (for [i1 s1 i2 s2] [i1 i2]))))

(defcheck solution-d7427dae
  (fn cp [xs ys]
    (set (for [x xs y ys] [x y]))))

(defcheck solution-d7798b34
  #(into
     (sorted-set)
     (reverse
       (for [a (seq %1)
             b (seq %2)]
         [a b]))))

(defcheck solution-d79512a7
  (fn [b a] (into #{} (mapcat #(map vector b (repeat %)) a))))

(defcheck solution-d79c2792
  (fn [x y]
    (set (mapcat (fn [z] (map (fn [w] [z w]) y)) x))))

(defcheck solution-d7ac43ad
  (fn
    [s1 s2]
    (set (mapcat #(map (partial vector %) s2) s1))))

(defcheck solution-d7afc2dc
  (fn [xs ys]
    (set (for [x xs, y ys] [x y]))))

(defcheck solution-d846182c
  (fn [s1 s2]
    (set (for [s1 s1 s2 s2] [s1 s2]))))

(defcheck solution-d8a1c9b6
  (fn [xs ys](into #{}(for [x xs y ys][x y]))))

(defcheck solution-d8bb6fb6
  (fn [a b]
    (set (for [x a y b]
           [x y]))))

(defcheck solution-d8bf6679
  (fn [x y]
    (apply hash-set (for [a x b y] [a b]))))

(defcheck solution-d9bdd9dd
  #(into #{} (apply concat (for [e1 %](for [e2 %2](vector e1 e2))))))

(defcheck solution-d9f4289f
  #(set (for [x %1 y %2] [x y]
                        )))

(defcheck solution-da73dc0d
  (fn [s t] (set (for [a s b t] [a b]))))

(defcheck solution-dc5bb52d
  (fn [y x](into #{} (reduce concat (map #(map (fn [z] [% z]) x) y)))))

(defcheck solution-dc618341
  (fn cartesian-product [set1 set2]
    (reduce #(into %1 (map vector set1 (repeat (count set1) %2))) #{} set2)))

(defcheck solution-dc96818c
  #(set (for [x %1 y %2] [x y ])))

(defcheck solution-dcb077b2
  #(set (for [x %1, y %2] [x y])))

(defcheck solution-dcfc829f
  (fn prob90 [l1 l2]
    (loop [acc #{}
           rows l1
           cols l2
           ]
      (if (empty? rows)
        acc
        (let [r (first rows)]
          (recur (into acc (map (fn [v] [r v]) cols)) (rest rows) cols)
          )))))

(defcheck solution-dd28354
  (fn
    [set1 set2]

    (set (for [a set1 b set2]
           (vector a b) ))))

(defcheck solution-dd766c38
  (fn [s1 s2] (set (for [x s2 y s1] [y x]))))

(defcheck solution-dde0bba0
  (fn cartesian-product [s1 s2]
    (into #{} (for [x1 s1 x2 s2]
                [x1 x2]))))

(defcheck solution-de1949b1
  (fn [s1 s2] (set (reduce concat '() (map (fn [e1] (map (fn [e2] [e1 e2]) s2)) s1)))))

(defcheck solution-de72dafa
  (fn [s1 s2]
    (set (for [x s1
               y s2]
           [x y]))))

(defcheck solution-de8bef97
  (fn cprod [s1 s2]
    (set (for [a s1, b s2] [a b]))))

(defcheck solution-df063c2b
  #(set (for [x %1
              y %2]
          [x y])))

(defcheck solution-df69f8af
  (fn  [s1 s2]
    (loop [rm1 (seq s1), rm2 (seq s2), acc #{}]
      (cond (empty? rm2) acc
            (empty? rm1) (recur s1 (rest rm2) acc)
            :else (recur (rest rm1) rm2 (conj acc [(first rm1) (first rm2)]))))))

(defcheck solution-e0132aff
  #(set (for [l % r %2] [l r])))

(defcheck solution-e022a0ea
  (fn [a b]
    (set
      (reduce concat
        (map
          (fn [n]
            (map
              (fn [x]
                [n x]
                )
              (into-array b)
              )
            )
          (into-array a)
          )
        )
      )
    ))

(defcheck solution-e033683
  (fn [a b]
    (set (for [x a y b]
           [x y]))))

(defcheck solution-e04cc12e
  (comp set #(for[a % b %2] [a b])))

(defcheck solution-e0544f18
  (fn cart [x y] (into #{} (apply concat (map (fn [i] (map #(vector i %)  y) )   x) ) )))

(defcheck solution-e0d1c29b
  #(set (for [e1 %1 e2 %2] (vector e1 e2))))

(defcheck solution-e18f8640
  (fn [s1 s2]
    (set
      (for [i s1 j s2] [i j])
      )
    ))

(defcheck solution-e1a17e7a
  (fn [a b]
    (into #{}
      (apply concat
        (for [x a]
          (for [y b]
            [x y]))))))

(defcheck solution-e28536bc
  (fn [s1 s2]
    (set (reduce #(concat %1 %2) #{}
           (for [a s1] (for [b s2] [a b]))))))

(defcheck solution-e3019bdb
  (fn cp [s1 s2]
    (set (for [i s1 j s2] [i j]))))

(defcheck solution-e3030c08
  (fn [s1 s2]
    (set (for [a s1 b s2]
           [a b]))))

(defcheck solution-e38cf79c
  (fn [A B] (into #{} (for [x A y B] [x y]))))

(defcheck solution-e3e73f85
  (fn [s1 s2]
    (let [r (for [x1 s1 x2 s2]
              [x1 x2])]
      (into (empty s1) r))
    ))

(defcheck solution-e4367e8f
  (fn [x y]
    (set
      (mapcat
        #(map (partial vector %) y)
        x))))

(defcheck solution-e470e747
  #(set (for [a (vec %1) b (vec %2)] [a b])))

(defcheck solution-e4ab874f
  (fn [a b]
    (set
      (for [x a, y b] [x y]))))

(defcheck solution-e4bbda26
  #( into #{} (for [i % j %2]  [i j])))

(defcheck solution-e54ca0c1
  (fn [a b]
    (into #{} (for [xa a xb b] [xa xb]))))

(defcheck solution-e5f943cf
  (fn [a b] (set (for [i a j b] [i j]))))

(defcheck solution-e6906702
  (fn [s1 s2]
    (set (for [x1 s1, x2 s2] [x1 x2]))))

(defcheck solution-e6b88c3e
  (fn cartesian-product [left right]
    (set (for [x left y right] [x y]))))

(defcheck solution-e6e2c0a9
  (fn [a b]
    (set (mapcat
           (fn [x]
             (map #(vector x %) b)
             ) a)
      )))

(defcheck solution-e7a83f84
  (fn [x y]
    (set
      (mapcat
        (fn [z] (map #(list % z) x))
        y))))

(defcheck solution-e7d3aa38
  (fn [A, B] (into #{} (for [a A
                             b B]
                         [a b]))))

(defcheck solution-e917a1c0
  (fn [a b]
    (loop [burn a out []]
      (if (empty? burn)
        (set out)
        (recur (rest burn) (into out (map #(vector (first burn) %) b)))))))

(defcheck solution-e95cd403
  (fn [set1 set2]
    (set (for [item1 set1
               item2 set2]
           [item1 item2]))))

(defcheck solution-e960fcd3
  (fn f [xs ys] (set (mapcat #(map (partial vector %1) ys) xs))))

(defcheck solution-e96e2112
  (fn [xs ys]
    ((fn [xs ys acc]
       (if (empty? xs) acc
                       (recur (rest xs) ys (into acc (map #(vector (first xs) %) ys)))))
     xs ys #{})))

(defcheck solution-e9cebbb7
  (fn [xs ys]
    (set (for [x xs y ys]
           [x y])
      )))

(defcheck solution-e9eceb4c
  (fn [xs ys] (into #{} (for [x xs y ys] [x y]))))

(defcheck solution-ea418372
  (fn [s1 s2] (set (for [a s1 b s2] [a b]))))

(defcheck solution-ea57cb0
  (fn [a  b] (set
               (reduce concat
                 (map
                   (fn
                     [eleA]
                     (map
                       (fn
                         [eleB]
                         (list eleA eleB))
                       b))
                   a)))))

(defcheck solution-eb3e02fd
  (fn [s1 s2]
    (into #{} (for [i1 s1
                    i2 s2]
                [i1 i2]))
    ))

(defcheck solution-eb4e3530
  #(set (apply concat (for [i %] (for [j %2] [i j])))))

(defcheck solution-ebb15c40
  (fn [s1 s2] (set
                (mapcat
                  (fn [x1] (map (fn [x2] (vector x1 x2)) s2))
                  s1))))

(defcheck solution-ed800270
  (fn [sq1 sq2] (set (for [s1 sq1 s2 sq2] [s1 s2]))))

(defcheck solution-ed8b8327
  (fn cartesian
    [a b]
    (if (empty? a) #{}
                   (into (cartesian (rest a) b) (map #(conj (-> a first vector) %) b)))))

(defcheck solution-ed8e421b
  #(set (for [x (seq %1) y (seq %2)] [x y])))

(defcheck solution-edad3897
  (fn [aa bb]
    (set (mapcat (fn [a]
                   (map (fn [b] (vector a b)) bb)) aa))))

(defcheck solution-edaf8f9d
  (fn cart [a b] (clojure.set/union (set (map #(vector (first a) %) b)) (if (not (empty? (rest a))) (cart (rest a) b)) #{})))

(defcheck solution-ede43442
  (fn [s1 s2] (set (for [x s1 y s2] [x y]))))

(defcheck solution-ee6e36b8
  (fn [x y]
    (set (for [a x
               b y]
           (vector a b)))))

(defcheck solution-eed12a09
  #(set (for [x %
              y %2]
          [x y])))

(defcheck solution-eed5ba09
  #(set (for [i (seq %1)
              j (seq %2)]
          (vector i j))))

(defcheck solution-ef04a249
  (fn [x1 x2] (set (for [x x1 y x2] [x y]))))

(defcheck solution-efae087b
  (fn[a b](into #{}(for[ai a bi b][ai bi]))))

(defcheck solution-f0363b14
  (fn [p1 p2] (reduce into #{} (map (fn [n] (map #(do [% n]) p1)) p2))))

(defcheck solution-f04fc661
  (fn [sa sb]
    (set (for [a sa, b sb] [a b]))))

(defcheck solution-f1779976
  (fn cartenian-product
    [xs ys]
    (set (for [x xs y ys] [x y]))))

(defcheck solution-f177fddc
  #(set (for [x % y %2]
          [x y])))

(defcheck solution-f1d38908
  (fn cartesian [s1 s2]
    (loop [acc #{} remainder s1]
      (if
       (empty? remainder) acc
                          (recur (apply conj acc
                                   (map (fn [el] [(first remainder) el]) s2)) (next remainder))))))

(defcheck solution-f304a091
  (fn [set1 set2]
    (set (for [s1 set1 s2 set2] [s1 s2]))))

(defcheck solution-f315b01
  (fn [s1 s2]
    (apply hash-set (for [e1 s1 e2 s2] [e1 e2]))))

(defcheck solution-f352053a
  #(set
     (for [x %
           y %2]
       [x y])))

(defcheck solution-f46d2c77
  (fn [a b]
    (set (for [x a y b]
           [x y]))))

(defcheck solution-f4b6a25c
  (fn cartesian-product [a b]
    (set
      (mapcat
        #(map
           (comp (partial cons %)
             list)
           b)
        a))))

(defcheck solution-f4f0aef7
  (fn [a b] (set (for [a1 a
                       b1 b]
                   [a1 b1]))))

(defcheck solution-f511507d
  (fn [a b]
    (set (for [a a b b] [a b]))))

(defcheck solution-f65f78fe
  #(set (for [ae %1
              be %2]
          [ae be])))

(defcheck solution-f6ce747
  (fn [s1 s2]
    (let [v1 (vec s1)
          v2 (vec s2)]
      (set (for [a v1 b v2] [a b])))))

(defcheck solution-f715f092
  (fn [as bs]
    (set (mapcat (fn [a] (map (fn [b] [a b]) bs)) as))))

(defcheck solution-f8a6c281
  (fn cartesian-product [& sets] (set (if (empty? sets) [[]] (mapcat #(map % (apply cartesian-product (rest sets))) (map #(partial cons %) (first sets)))))))

(defcheck solution-f8cf8b1
  (fn [s1 s2]
    (set (for [e1 s1 e2 s2]
           (vector e1 e2)))))

(defcheck solution-f8dc8aa4
  (fn [a b]
    (set (for [i a j b]
           (vector i j)))))

(defcheck solution-fa11c719
  (fn [xs ys]
    (set
      (for [x xs
            y ys]
        [x y]))))

(defcheck solution-fa751075
  (fn cartesian-product [set1 set2]
    (into #{} (for[x set1 y set2]
                [x y]))))

(defcheck solution-fa7b35ec
  (fn [s1 s2]
    (reduce (fn [l1 e1]
              (into l1 (reduce
                         (fn [l2 e2]
                           (conj l2 [e1 e2]))
                         #{} s2)))
      #{} s1)))

(defcheck solution-fa8fc790
  (fn [a b]
    (set (apply concat (for [x a]
                         (for [y b]
                           [x y]))))))

(defcheck solution-fb159b79
  #(set (for [x %, y %2] [x y])))

(defcheck solution-fbefe68f
  #(set (for [x %, y %2] [x,y])))

(defcheck solution-fc638d9
  (fn cart [x y]
    (set (for [value x
               suit y]
           [value suit]))))

(defcheck solution-fca63014
  #(set
     (for [i % j %2]
       (vector i j))))

(defcheck solution-fcfae4a8
  #(set (for [y % x %2] [ y x])))

(defcheck solution-fcfbfaa8
  (fn [s1 s2]
    (set (for [x s1 y s2]
           (list x y)))))

(defcheck solution-fd2ca05e
  (fn [a b]
    (reduce #(into % (map (partial vector %2) b)) #{} a)))

(defcheck solution-fd8e23d8
  (fn [x y] (set (map vector (cycle x) (->> y (apply list) (map #(repeat (count x) %)) flatten)))))

(defcheck solution-fdf6b4c5
  #(set (for [x %1 y %2]
          [x y])))

(defcheck solution-fe3db709
  #(set (for [x %
              y %2]
          [x y])))

(defcheck solution-fe8688f4
  (fn [s1 s2]
    (into #{} (for [x s1 y s2] [x y]))))

(defcheck solution-ff562434
  (fn cross-product [lset rset]
    (reduce
      (fn r [s n]
        (apply conj s (reduce #(conj %1 [%2 n]) #{} lset)))
      #{}
      rset)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-90))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
