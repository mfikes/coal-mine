(ns coal-mine.problem-33
  (:require [coal-mine.checks :refer [defcheck-33] :rename {defcheck-33 defcheck}]
            [clojure.test]))

(defcheck solution-10121f9
  (fn ras[s n]
    (if (empty? s)
      '()
      (concat (repeat n (first s)) (ras (rest s) n)))))

(defcheck solution-1032f9e3
  (fn [l,t] (mapcat #(repeat t %) l)))

(defcheck solution-103d1d78
  (fn rep [coll n]
    (mapcat #(replicate n %) coll)))

(defcheck solution-10618806
  (fn rep [c n]
    (when (not (empty? c))
      (concat (repeat n (first c)) (rep (rest c) n)))))

(defcheck solution-10782d60
  (fn multiply
    [coll x]
    (reduce #(into %1 (repeat x %2)) [] coll)))

(defcheck solution-10f64bb3
  (fn replicated
    [collection n]
    (reduce #(into %1 (take n (repeat %2))) [] collection)))

(defcheck solution-110d882d
  (fn [coll n] (mapcat #(take n (repeat %1)) coll)))

(defcheck solution-115ef3a9
  (fn [ s n ] (mapcat #(repeat n %) s)))

(defcheck solution-118490e6
  (fn [col n] (mapcat #(repeat n %) col)))

(defcheck solution-11859053
  (fn [xs n] (mapcat #(repeat n %) xs)))

(defcheck solution-1221184d
  (fn [s n] (reduce #(concat %1 (repeat n %2)) [] s)))

(defcheck solution-123f5384
  (fn [l c] (mapcat #(replicate c %) l)))

(defcheck solution-1243bb78
  (fn [seq n]
    (apply concat (map #(take n (iterate identity %))
                    seq))))

(defcheck solution-130f5003
  #(let [arr %1 cont %2]
     (reduce (fn [res item]
               (concat res (repeat cont item)))
       '() arr)))

(defcheck solution-1354a1f4
  (fn [xs n] (mapcat #(take n (repeat %)) xs)))

(defcheck solution-1419239f
  #(apply (partial mapcat (fn [& xs] xs)) (replicate %2 %1)))

(defcheck solution-14d5f15b
  (fn [xs n]
    (if (= n 1)
      xs
      (apply interleave (take n (repeat xs))))))

(defcheck solution-14e449d5
  (fn
    [coll n]
    (reduce
      concat
      (map
        #(repeat n %)
        coll))))

(defcheck solution-151ea7a6
  (fn [data c]
    (mapcat #(take c (repeat %)) data)))

(defcheck solution-15977bce
  (fn [l r] (apply concat (map #(repeat r %) l))))

(defcheck solution-15a203e9
  #(for [i %1 j (range %2)] i))

(defcheck solution-163a1924
  (fn[s n](mapcat #(repeat n %) s)))

(defcheck solution-1683e1fe
  (fn [s n] (for [a s _ (range n)] a)))

(defcheck solution-16b3b6f1
  #(mapcat (fn [x] (take %2 (repeat x))) %1))

(defcheck solution-1740050e
  (fn [x y]
    (apply concat
      (map (fn [v]
             (repeat y v)) x))))

(defcheck solution-1850ace9
  (fn [coll n] (mapcat (partial repeat n) coll)))

(defcheck solution-185e852d
  (fn [xs, n]
    (reduce concat (map #(replicate n %) xs))
    ))

(defcheck solution-18b11f90
  (fn  [lst times]
    (loop [rm (seq lst), acc (vector), t times]
      (cond (= 0 t) (recur (rest rm) acc times)
            (empty? rm) (seq acc)
            :esle (recur rm (conj acc (first rm)) (dec t))))))

(defcheck solution-18e15d45
  (fn [c n]
    (mapcat #(take n (repeat %)) c)))

(defcheck solution-18e2a01
  (fn [coll n]
    (for [x coll i (range n)] x)))

(defcheck solution-18e9ca03
  (fn [coll n]
    (for [c coll, _ (range n)]
      c
      )
    ))

(defcheck solution-19888deb
  (fn [coll times] (mapcat #(repeat times %) coll)))

(defcheck solution-1adb08d9
  (fn [c n] (apply concat (map #(repeat n %) c ))))

(defcheck solution-1aee4b28
  (fn replic [src times]
    (for [x src y (take times (iterate identity x))] y)))

(defcheck solution-1bcf2165
  (fn [x n]
    (loop [todo x
           res []]
      (if (empty? todo)
        res
        (let [f (first todo)
              r (next todo)]
          (recur r (concat res (repeat n f))))))))

(defcheck solution-1c5ab321
  (fn [xs t]
    (mapcat #(repeat t %) xs)))

(defcheck solution-1c7a414a
  #(apply concat (map (fn [input](repeat %2 input)) %1)))

(defcheck solution-1c81114f
  (fn duplicate [x n]
    (reduce #(concat %1 (repeat n %2)) [] x)))

(defcheck solution-1c8e6cb3
  (fn n-repeat [coll n]
    (reduce concat (map #(repeat n %) coll))))

(defcheck solution-1c9b59dc
  (fn this
    ([xs n] (this xs n []))
    ([[hd & tl :as xs] n acc]
     (if (empty? xs)
       acc
       (recur tl n (into acc (repeat n hd)))))))

(defcheck solution-1ce2a340
  (fn [x n] (mapcat #(repeat n %) x)))

(defcheck solution-1d07ed19
  (fn [s n] (if (= n 1) (seq s) (apply interleave (repeat n s)))))

(defcheck solution-1dd3edb6
  (fn [lst n] (apply (partial mapcat list) (repeat n lst))))

(defcheck solution-1efeb83f
  (fn [sq factor]
    (loop [ls (seq sq)
           return []]
      (if (nil? (first ls))
        (seq return)
        (recur (rest ls)
          (apply conj return
            (repeat factor (first ls))))))))

(defcheck solution-1efed3e1
  #(reduce into (map (comp vec (partial repeat %2)) %1)))

(defcheck solution-1f039bdc
  (fn [seq n]
    (reduce #(concat %1 %2) (list) (map #(take n (repeat %)) seq))))

(defcheck solution-1f7aeeb2
  (fn [c n] (apply concat (map #(take n (repeat %)) c))))

(defcheck solution-1f9a7454
  (fn [x co] (reduce #(apply conj %1 (take co (repeat %2))) [] x)))

(defcheck solution-20e5d953
  (fn [lst x]
    (apply concat (map (fn [e] (take x (repeat e))) lst))))

(defcheck solution-2122579c
  (fn[_seq n](reduce (fn[result head]
                       (concat result (repeat n head))) '() _seq)))

(defcheck solution-2130d37
  (fn [lst n]
    (mapcat #(repeat n %) lst)))

(defcheck solution-21a518a4
  (fn [z i] (mapcat (fn [x] (repeat i x)) z)))

(defcheck solution-21c50cee
  (fn [l n]
    (apply concat (map #(repeat n %) l))))

(defcheck solution-21d71a45
  (fn [x n] (reduce concat (map #(take n (repeat %)) x))))

(defcheck solution-21f59be3
  (fn replicate-a-seq [coll n]
    (apply concat
      (map #(repeat n %) coll))))

(defcheck solution-225174b
  (fn [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-22caa583
  (fn [s n] (mapcat #(repeat n %) s)))

(defcheck solution-22cac417
  (fn
    [s n]
    (if (> n 1)
      (apply interleave (take n (repeat s)))
      s)))

(defcheck solution-230562d
  (fn [coll n] (apply concat (map #(take n (iterate identity %)) coll))))

(defcheck solution-23585d5c
  #(apply mapcat list (repeat %2 %)))

(defcheck solution-237152b0
  (fn [x n] (apply concat (map (partial repeat n) x))))

(defcheck solution-24c83cba
  (fn [coll cnt]
    (if (= cnt 1)
      coll
      (apply interleave (repeat cnt coll)))))

(defcheck solution-25145c54
  #(mapcat (fn [x] (repeat %2 x)) %1))

(defcheck solution-252ac2ab
  (fn dup [xs k]
    (apply concat
      (for [x xs]
        (repeat k x)))))

(defcheck solution-2563bff8
  #(if (= %2 1)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-264d5943
  (fn [s n]
    (apply concat (map #(repeat n %) s))))

(defcheck solution-2677c67e
  (fn replicate-elements [seq n]
    (mapcat (partial repeat n) seq)))

(defcheck solution-26a7d732
  (fn [xs n] (mapcat (fn [x] (take n (repeat x))) xs)))

(defcheck solution-280b9554
  (fn replicate [xs n]
    (if (empty? xs)
      '()
      (concat (repeat n (first xs)) (replicate (rest xs) n)))))

(defcheck solution-2821cec5
  #(if (= 1 %2) % (apply interleave (replicate %2 %))))

(defcheck solution-28224b5b
  (fn [s n]
    (loop [s s
           r ()]
      (if (empty? s)
        (reverse r)
        (recur (rest s)
          (concat (repeat n (first s)) r))))))

(defcheck solution-28ac6e5c
  #(mapcat (fn [x] (repeat %2 x)) %))

(defcheck solution-28b5d255
  (fn [col ntimes]
    (mapcat #(repeat ntimes %) col)
    ))

(defcheck solution-28c09471
  #(if (< %2 2)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-29369a23
  #(mapcat (fn [item]
             (take %2 (repeat item)))
     %1))

(defcheck solution-2964d09
  #(apply concat (map (fn [x] (repeat %2 x)) %1)))

(defcheck solution-29de2ae1
  (fn [l n] (for [x l, y (range n)] x)))

(defcheck solution-2a167582
  (fn duplicate-each
    [input n]
    (into []
      (mapcat #(apply list (repeat n %)) input))))

(defcheck solution-2a8efe1f
  (fn replic [l n] (reduce #(concat %1 %2) (map #(repeat n %) l))))

(defcheck solution-2a914736
  (fn [coll n]  (mapcat #(repeat n %) coll)))

(defcheck solution-2a977590
  (fn [a b]
    (mapcat
      (fn [i] (take b (iterate #(identity %) i)))
      a
      )
    ))

(defcheck solution-2acf467
  (fn [s n]
    (apply concat
      (map #(map (constantly %) (range n)) s))))

(defcheck solution-2adffd3e
  (fn myreat [l n]
    (mapcat #(repeat n %) l)))

(defcheck solution-2b5bdbe4
  #(mapcat (fn [a] (repeat %2 a)) %1))

(defcheck solution-2b7522e2
  (fn [coll n] (mapcat #(repeat n %) coll) ))

(defcheck solution-2bb12845
  (fn  [col n]
    (apply concat (for [c col] (repeat n c)))))

(defcheck solution-2c0157cd
  (fn [col n]
    (loop [[head & tail :as all] col
           cnt n
           acc []]
      (if (nil? head)
        acc
        (if (> cnt 0)
          (recur all (dec cnt) (conj acc head))
          (recur tail n acc)
          )
        )
      )
    ))

(defcheck solution-2c6e03a9
  (fn [x n] (reverse (reduce into (map #(repeat n %) x)))))

(defcheck solution-2c8a2ef8
  (fn [ls rep] (mapcat #(repeat rep %) ls)))

(defcheck solution-2cdae3d8
  (fn [x t] (mapcat (partial repeat t) x)))

(defcheck solution-2da1f940
  (fn repl [coll times]
    (mapcat #(repeat times %) coll)))

(defcheck solution-2db88879
  (fn [coll n]
    (reduce (fn [items item] (apply conj items (take n (repeat item)))) [] coll)
    ))

(defcheck solution-2e120b9a
  (fn [s,n] (mapcat #(take n (repeat %)) s)))

(defcheck solution-2e9e8907
  #(if (= %2 1)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-2eadd53e
  (fn [x y] (reduce #(concat %1 (repeat y %2)) [] x)))

(defcheck solution-2ec2ce4a
  (fn [coll k]
    (->> coll
      (map #(take k (repeat %)))
      (reduce into [])
      )))

(defcheck solution-2ed0df5b
  #(mapcat (partial repeat %2) %))

(defcheck solution-30866925
  (fn [s n](if (= n 1) s (apply interleave (repeat n s)))))

(defcheck solution-30a7cd91
  #(reduce into [] (for [x %1] (repeat %2 x))))

(defcheck solution-31087059
  #(apply  concat (map (fn [x] (repeat %2 x)) %1)))

(defcheck solution-31124e8d
  (fn [x t] (mapcat #( repeat t %) x)))

(defcheck solution-312310c2
  #(if (> %2 1)
     (apply interleave (repeat %2 %1))
     %1))

(defcheck solution-312a1ed8
  #(apply (partial mapcat list) (replicate %2 %)))

(defcheck solution-3132eaa8
  (fn [c r] (mapcat #(repeat r %) c)))

(defcheck solution-321f01da
  (fn [col a]
    (mapcat #(repeat a %1) col)))

(defcheck solution-327a6633
  (fn [s, n]
    (loop [c s, r []]
      (if (empty? c)
        r
        (recur (rest c) (concat r (repeat n (first c))))))))

(defcheck solution-32fe078
  (fn [coll n]
    (if (> n 1)
      (apply interleave (take n (repeat coll)))
      coll)))

(defcheck solution-3307c809
  #(mapcat (fn [x] (take %2 (repeat x)   ) ) %1))

(defcheck solution-333a6e46
  (fn [items amount] (mapcat #(repeat amount %) items)))

(defcheck solution-333e3987
  (fn repl [[x & xs] n] (if x (apply conj (repl xs n) (repeat n x)))))

(defcheck solution-333e8d2b
  #(reduce concat (map (fn [x] (map (fn [y] x) (range %2))) %1)))

(defcheck solution-33411cc1
  (fn[x, c] (mapcat #(repeat c %) x)))

(defcheck solution-3352581c
  (fn [x c]
    (loop [x x
           v '()]
      (if (nil? (first x))
        v
        (recur (rest x)
          (concat v (repeat c (first x))))))))

(defcheck solution-33bba30d
  #(reduce (fn [x y] (into x (repeat %2 y))) [] %1))

(defcheck solution-3449861
  (fn [x n] (reduce #(concat %1 (repeat n %2)) [] x)))

(defcheck solution-34e0ff1c
  (fn [v n] (mapcat #(repeat n %) v)))

(defcheck solution-355de67e
  #(reduce concat (for [m %] (repeat %2 m))))

(defcheck solution-35bd2e8a
  #(if (= %2 1) % (apply interleave (repeat %2 %)) ))

(defcheck solution-36020d57
  (fn dup-seq [mylist n]
    (seq (loop [l mylist final '[]]
           (if (empty? l) final
                          (recur (rest l) (concat final (replicate n (first l)))))))))

(defcheck solution-37a98795
  (fn [coll n] (reduce #(apply conj %1 (repeat n %2)) [] coll)))

(defcheck solution-37aa7dfa
  (fn [x y] (mapcat #(repeat y (identity %) )  x)))

(defcheck solution-37aef7e5
  (fn my-replicate
    [s n]
    (reduce (fn [result v]
              (apply conj result (take n (repeat v))))
      [] s)))

(defcheck solution-38202117
  (fn [seq1 c]
    (loop [endresult [] elements seq1]
      (if (empty? elements)
        endresult
        (recur
          (into endresult
            ((fn [a x]
               (loop [result [] index 0]
                 (if (= index x)
                   result
                   (recur (conj result a) (inc index))
                   )
                 )
               ) (first elements) c)
            )
          (rest elements)
          )
        )
      )
    ))

(defcheck solution-38f34e69
  (fn [lst n]
    (apply concat (map (partial repeat n) lst))))

(defcheck solution-39197c05
  (fn [input x]
    (reduce (fn [container value]
              (loop [iter x, result container]
                (if (= iter 0) result
                               (recur (dec iter) (conj result value)))))
      [] input)))

(defcheck solution-396e14bb
  (fn [s n](
             mapcat (fn[a](repeat n a)) s
             )))

(defcheck solution-398baba5
  (fn [xs n] (for [x xs i (range n)] x)))

(defcheck solution-39c407a9
  (fn [sq reps] (reduce #(into % (repeat reps %2)) [] sq)))

(defcheck solution-39e07c7b
  (fn [s, n] (reduce (fn [acc i] (apply conj acc (repeat n i))) [] s)))

(defcheck solution-3a3bac9a
  (fn rep [s n] (apply concat (map #(repeat n %) s))))

(defcheck solution-3a667f92
  (fn [sx n] (mapcat #(repeat n %) sx)))

(defcheck solution-3b53480
  #(reduce (fn [s i] (concat s (repeat %2 i))) () %1))

(defcheck solution-3b572ed7
  (fn [input multiple]
    (loop [remaining input ans (lazy-seq [])]
      (if (empty? remaining)
        ans
        (let [[ptr & remain] remaining]
          (recur remain (concat ans (for [x (range 0 multiple)]
                                      ptr))))))))

(defcheck solution-3b62512c
  (fn [s n]
    (if (= n 1) s
                (apply interleave (repeat n s)))))

(defcheck solution-3bd5ce8e
  (fn [coll n]
    (reduce #(concat %1 (take n (repeat %2))) '() coll)))

(defcheck solution-3d1e7341
  (fn [seq n] (mapcat #(repeat n %) seq)))

(defcheck solution-3d8446cd
  #(mapcat (fn [each] (repeat %2 each)) %))

(defcheck solution-3d8eba37
  (fn [coll n]
    (loop [k coll r []]
      (if (nil? k)
        r
        (recur (next k) (into r (repeat n (first k))))))))

(defcheck solution-3e0f4c8c
  (fn myReplicate
    [sequence n]
    (apply concat (map (partial repeat n) sequence))))

(defcheck solution-3e2ae1cb
  (fn [s n] (mapcat #(vec (repeat n %)) s)))

(defcheck solution-3ee3d76e
  (fn [x y] (mapcat #(take y (repeat %)) x)))

(defcheck solution-3eeb57c1
  (fn [s n]
    (apply concat
      (map (partial repeat n) s))))

(defcheck solution-3ef4716a
  (fn [list times]
    (loop [l list t times ret '()]
      (if (= (count l) 0)
        (reverse ret)
        (recur
          (rest l)
          t
          (loop [item (first l) t times ret ret]
            (if (= t 0)
              ret
              (recur
                item
                (- t 1)
                (cons item ret)
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-3f2246ec
  (fn f [coll n]
    (reduce (fn [a e] (apply conj a (repeat n e))) [] coll)))

(defcheck solution-3ffd5d73
  (fn number33 [xs n]
    (apply mapcat list (repeat n xs))))

(defcheck solution-40117716
  (fn rep
    ([coll times] (rep coll times []))
    ([coll times result]
     (if (empty? coll)
       result
       (rep (rest coll) times (concat result (repeat times (first coll))))))))

(defcheck solution-40142970
  (fn this [s n]
    (cond (= (count s) 0) '()
          :else (concat (repeat n (first s))
                        (this (rest s) n)))))

(defcheck solution-405a6ebc
  (fn dup [a b]
    (if (empty? a)
      '()
      (let [r (atom (dup (rest a) b))]
        (dotimes [n b] (swap! r #(conj % (first a))))
        @r
        ))))

(defcheck solution-406a8f17
  (fn [xs n] (reduce concat (map #(repeat n %) xs))))

(defcheck solution-40b8d3b5
  ;#(if (= %2 1) %1 (apply interleave (repeat %2 %1)))
  #(mapcat (partial repeat %2) %1))

(defcheck solution-40c58946
  (fn [s t]
    (apply concat (for [x s] (for [i (range t)] x)))))

(defcheck solution-40ced647
  (fn [coll n] (mapcat #(take n (repeat %)) coll)))

(defcheck solution-40e179cc
  (fn [xs n] (mapcat (fn [x] (repeat n x)) xs)))

(defcheck solution-40fa0c70
  #(apply mapcat vector (replicate %2 %1)))

(defcheck solution-42314a55
  (fn [xs n]
    (mapcat
      (fn [v]
        (repeat n v)
        )
      xs
      )
    ))

(defcheck solution-42469e32
  (fn [s n] (reduce #(concat %1 (replicate n %2)) '() s)))

(defcheck solution-42baabcf
  (fn [l n]
    (loop [[hd & body] l ret []]
      (if (nil? hd)
        ret
        (recur body (reduce (fn [r c] (conj r c)) ret (repeat n hd)))))))

(defcheck solution-438abb3d
  (fn [s n]
    (mapcat #(repeat n %) s)))

(defcheck solution-43f9027
  #(letfn [(f [x acc n] (if (empty? x) acc (f (rest x) (concat acc (repeat n (first x))) n)))]
     (f %1 [] %2)))

(defcheck solution-44012422
  #(reverse (loop [i (dec (count %1)) result []]
              (if (= i -1)
                result
                (recur (dec i)
                  (concat result (repeat %2 (nth % i)) )
                  )
                )
              ) ))

(defcheck solution-441fb0cc
  (fn dup [seq n] (reduce
                    (fn [acc e] ((fn inner [acc i] (if (= i 1)
                                                     (conj acc e)
                                                     (inner (conj acc e) (- i 1))))
                                 acc n))

                    [] seq)))

(defcheck solution-4445a3a4
  (fn [coll n]
    (reduce #(loop [r %
                    v %2
                    i 0]
               (if (= i n) r
                           (recur (conj r v) v (inc i)))) [] coll)))

(defcheck solution-44983b8e
  (fn [xs n]
    (reduce into [] (map (fn [x] (repeat n x)) xs))))

(defcheck solution-44dc6dc
  (fn [coll n] (mapcat #(take n (repeatedly (constantly %1))) coll)))

(defcheck solution-46a0f343
  #(if (= %2 1)
     %1
     (apply interleave (take %2 (repeat %1)))))

(defcheck solution-46a5462f
  (fn [s n]
    (loop [output []
           prev   nil
           remain s
           c      0]
      (do

        (if (empty? remain)
          output
          (if (>= c n)
            (recur output nil (rest remain) 0)
            (let [fst (if (nil? prev) (first remain) prev)]
              (recur (concat output [fst]) fst remain (inc c)))))))))

(defcheck solution-46f695f5
  #(if (= %2 1) %1 (apply interleave (repeat %2 %1))))

(defcheck solution-47a0b9e9
  (fn repl-a-seq [x n]
    (reverse (reduce

               (fn [a x] (into a (repeat n x)))

               '()
               x))))

(defcheck solution-482175b5
  (fn  dupx [s num]
    (letfn [(duper [n v]
              (repeat n v))]
      (loop [r (rest s)
             acc (into () (duper num (first s)))]
        (if (empty? r)
          (reverse acc)
          (recur (rest r) (into acc (duper num (first r)))))))))

(defcheck solution-482c2257
  (fn [xs n](apply mapcat list (repeat n xs))))

(defcheck solution-488ec81f
  (fn [coll n] (reduce #(concat %1 (for [x (range n)] %2)) () coll)))

(defcheck solution-4a0b1ab4
  #(mapcat (partial replicate %2) %1))

(defcheck solution-4a48a505
  #(apply concat (for [e %] (repeat %2 e))))

(defcheck solution-4a4cffd3
  (fn [coll n]
    (mapcat #(take n (repeat %)) coll)))

(defcheck solution-4a7e9285
  (fn [s number-of-times]
    (reduce #(concat %1 (take number-of-times (repeat %2))) [] s)))

(defcheck solution-4abaadc3
  (fn  [v n] (reduce #(concat %1 (repeat n %2) ) [] v)))

(defcheck solution-4ad6a0b5
  #(loop [col %1 result () num %2]
     (if (empty? col)
       result
       (recur (rest col) (concat  result (repeat num (first col))) num))))

(defcheck solution-4c3e48e4
  (fn [x n] (apply concat (apply map list (repeat n x)))))

(defcheck solution-4cab5751
  (fn [sq n] (mapcat #(repeat n %) sq)))

(defcheck solution-4ded5b83
  (fn [coll n] (mapcat #(take n (repeat %) )  coll)))

(defcheck solution-4e9a52df
  (fn [n c]
    (if (= c 1) n
                (apply interleave (for [i (range c)] n)))))

(defcheck solution-4ea5b454
  (fn [seq n] (apply concat (map #(repeat n %) seq))))

(defcheck solution-4ee3e165
  (fn [c n] (mapcat #(take n (iterate identity %)) c)))

(defcheck solution-4eea99af
  #(reduce (fn[x y] (concat x (repeat %2 y))) [] %1))

(defcheck solution-4f2d3960
  (fn rs
    [coll n]
    (reduce
      #(concat % (repeat n %2))
      []
      coll)))

(defcheck solution-4f9ecbda
  (fn [s n] (loop [s s
                   n n
                   result []]
              (if (empty? s)
                result
                (let [addme (repeat n (first s))]
                  (recur (rest s) n (concat result addme)))))))

(defcheck solution-50fb96c0
  #(reduce (fn [a b] (concat a (repeat %2 b)))
     nil
     %1))

(defcheck solution-5147f375
  (fn [coll n]
    (let [dup (fn [item] (map (fn [x] item) (range n)))]
      (apply concat (map #(dup %) coll)))))

(defcheck solution-51b0306
  (fn [coll times]
    (mapcat #(repeat times %) coll)))

(defcheck solution-51e789c5
  (fn [s n]
    (mapcat #(repeat n %1) s)))

(defcheck solution-5254b969
  (fn [coll n]
    (loop [acc '()
           coll coll]
      (if-let [f (first coll)]
        (recur (concat acc (repeat n f)) (rest coll))
        acc
        )
      )))

(defcheck solution-52720e1a
  (fn [L n] (mapcat #(repeat n %) L)))

(defcheck solution-527a8ca0
  (fn [s n]
    (if (> n 1)
      (apply interleave (repeat n s))
      s
      )
    ))

(defcheck solution-5287f1f1
  (fn replicate [coll n]
    (if (= n 1) coll
                (apply
                  interleave
                  (take n
                    (repeat coll))))))

(defcheck solution-532f809a
  (fn [l n]
    (reduce concat (map #(take n (repeat %)) l))))

(defcheck solution-54053bc9
  #(mapcat (fn [x] (replicate %2 x)) %))

(defcheck solution-545549e2
  (fn[ls x] (mapcat #(repeat x %) ls)))

(defcheck solution-54bedbc9
  (fn [col n]
    (reverse
      (reduce
        #(loop [result %1,a %2,num 0]
           (if (= num n)
             result
             (recur (cons a result) a (inc num))
             )
           )
        '()
        col
        )
      )
    ))

(defcheck solution-54c14e8e
  (fn [coll x] (reduce concat (map #(take x (repeat %)) coll))))

(defcheck solution-551876bf
  #(apply concat (for [x %1]
                   (repeat %2 x))))

(defcheck solution-5543a73c
  (fn [xs n]
    (mapcat #(vec (repeat n %)) xs)))

(defcheck solution-55662d5c
  (fn [coll n]
    (apply concat (map (fn [x] (repeat n x)) coll))))

(defcheck solution-556ec215
  (fn [x i] (mapcat (partial repeat i) x)))

(defcheck solution-55840862
  (fn [input n]
    (loop [s input a '()]
      (if (empty? s)
        (reverse a)
        (recur (rest s)
          (loop [e (first s) i 0 aa a]
            (if (= i n)
              aa
              (recur e (inc i) (cons e aa)))))))))

(defcheck solution-55a29df4
  (fn [cs n] (reduce (fn [acc b] (apply conj acc (repeat n b) )) (empty cs) cs)))

(defcheck solution-55e81e6d
  (fn replicate [s n] (mapcat #(repeat n %) s)))

(defcheck solution-563678df
  (fn [l n] (mapcat #(repeat n %) l)))

(defcheck solution-567a5afd
  (fn
    [lista n]
    (mapcat #(repeat n %) lista)))

(defcheck solution-5686b06b
  (fn replicate [x y] (mapcat #(take y (iterate identity %)) x)))

(defcheck solution-56af9559
  (fn [col n] (mapcat (fn [x] (repeat n x)) col)))

(defcheck solution-5797597c
  (fn [x y]
    (reverse (loop [r '()
                    c x]
               (if (empty? c)
                 r
                 (recur (reduce conj r (take y (iterate (fn [x] x) (first c)))) (rest c)))))))

(defcheck solution-58017603
  (fn [coll n]
    (->> (for [x coll]
           (repeat n x))
      (apply concat))))

(defcheck solution-584873d8
  (fn myrep
    [myseq cnt]

    (mapcat #(repeat cnt %) myseq)))

(defcheck solution-585df52e
  (fn [col n-times]
    (mapcat #(repeat n-times %1) col)))

(defcheck solution-586f8908
  (fn [seq times]
    (reduce (fn [col ele]
              (concat col (repeat times ele)))
      ""
      seq)))

(defcheck solution-5887045d
  (fn [m n] (mapcat #(repeat n %) m)))

(defcheck solution-58c13739
  (fn [coll n]
    (reduce #(concat % (repeat n %2))
      [] coll)))

(defcheck solution-58f49ed4
  (fn [c n]
    (mapcat #(repeat n %) c)))

(defcheck solution-599ba397
  #(if (> %2 1)
     (apply interleave (repeat %2 %))
     %))

(defcheck solution-59fb4275
  (fn [l n](mapcat #(apply conj [] (repeat n %1)) l)))

(defcheck solution-5a6a9837
  (fn [coll n] (reduce #(into %1 (repeat n %2)) [] coll)))

(defcheck solution-5a755c6b
  (fn [x n]
    (loop [s x r []]
      (if (empty? s) r
                     (recur (rest s) (concat r (repeat n (first s))))))))

(defcheck solution-5a7a04bc
  (fn [coll x]
    (cond (= x 1) coll
          :else
          (apply interleave
            (take x
              ((fn iter [y]
                 (cons y (lazy-seq (iter y))))
               coll))))))

(defcheck solution-5ad6959c
  (fn [coll n] (loop [c coll a []] (if (empty? c) a (recur (rest c) (into a (repeat n (first c))))))))

(defcheck solution-5af10d39
  (fn [x y]
    (mapcat #(repeat y %) x)))

(defcheck solution-5af1d7b6
  (fn [sq, cnt] (mapcat #(repeat cnt %) sq)))

(defcheck solution-5b12380e
  (fn [s n]
    (mapcat #(take n (repeat %)) s)))

(defcheck solution-5b46aadc
  (fn replicate [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-5bd7cc2d
  #(mapcat (fn [i] (repeat %2 i)) %1))

(defcheck solution-5c97f922
  (fn [col cnt]
    (mapcat #(repeat cnt %) col)))

(defcheck solution-5d5e1fe4
  (fn [l num]
    (mapcat #(repeat num %) l)))

(defcheck solution-5dc644d6
  #(mapcat (partial replicate %2) %))

(defcheck solution-5df549a3
  (fn [coll n]
    (apply concat (map #(repeat n %) coll))))

(defcheck solution-5e00065a
  #(reduce (fn [s x]
             (concat s (repeat %2 x))) [] %1))

(defcheck solution-5ea5ef7c
  (fn [se cnt]
    (letfn [(fib
              [f r cntr newseq]
              (if (nil? f)
                newseq
                (recur (first r) (rest r) cntr (into newseq (repeat cntr f)))
                ))]
      (fib (first se) (rest se) cnt [])
      )))

(defcheck solution-5ebd7299
  (fn [x n] (mapcat (partial (fn dupn [n ys] (cond (symbol? ys) (repeat n ys)
                                                   (= n 0) ()
                                                   :else (concat (list ys) (dupn (dec n) ys)))) n)
              x)))

(defcheck solution-5f143612
  (fn [s n]
    (apply concat (map #(replicate n %) s))))

(defcheck solution-5f3955dc
  (fn [x y] (apply concat (map #(take y (repeat %)) x))))

(defcheck solution-5f47de83
  #(apply concat (map (fn [v] (repeat %2 v)) %1)))

(defcheck solution-60bd466
  (fn replic

    ([a b]

     (replic a b 1 (count a)))

    ([a b c stop]

     (if (> c stop)

       a

       (replic (concat (drop 1 a) (take b (repeat (first a)))) b (inc c) stop)))))

(defcheck solution-618ab98b
  (fn [xs y] (mapcat #(repeat y %) xs)))

(defcheck solution-6199f858
  (fn [xs n](mapcat (partial repeat n) xs)))

(defcheck solution-61a8cacf
  (fn [s n]
    (reduce #(concat % (repeat n %2))
      [] s)))

(defcheck solution-61ea8119
  (fn repseq [items n]
    (if (empty? items)
      '()
      (concat (repeat n (first items)) (repseq (rest items) n)))))

(defcheck solution-6220bdbb
  (fn rplct [s c]
    (mapcat #(repeat c %) s)))

(defcheck solution-62a43163
  (fn [xs n] (reduce #(apply conj %1 (repeat n %2)) [] xs)))

(defcheck solution-62fa7de7
  (fn [s n] (reduce #(concat %1 (take n (repeat %2))) [] s)))

(defcheck solution-6302679
  (fn [x y] (mapcat (fn [i] (repeat y i)) x)))

(defcheck solution-634e9c93
  (fn [lat n] (reduce concat (map #(take n (repeat %)) lat))))

(defcheck solution-637bb50f
  (fn R [l n]
    (apply concat (map #(repeat n %) l))))

(defcheck solution-640a2008
  (fn [xs n] (reduce (fn [ys x] (into ys (repeat n x))) [] xs)))

(defcheck solution-651530d0
  (fn [c i] (reduce #(concat %1 (repeat i %2)) [] c)))

(defcheck solution-65ec253f
  (fn [c n] (reduce #(concat %1 (repeat n %2)) () c)))

(defcheck solution-6639d044
  (fn [sq n] (apply concat (map #(repeat n %) sq))))

(defcheck solution-6671213a
  (fn [l n]
    (mapcat (partial repeat n) l)))

(defcheck solution-66819090
  #(reduce
     (fn [coll x]
       (concat coll (for [i (range %2)] x)))
     '() %1))

(defcheck solution-66d117f0
  (fn [x y] (reduce #(apply conj % (repeat y %2)) [] x)))

(defcheck solution-66f2bbd6
  (fn [coll times]
    (reduce into [] (map #(repeat times %) coll))))

(defcheck solution-67324f14
  (fn [x y] (mapcat #(repeat y %) x)))

(defcheck solution-67e49e2a
  #(for [x %1 y (repeat %2 x)] y))

(defcheck solution-684c2bb7
  #(for [y %1, x (range %2)] y))

(defcheck solution-685d7275
  (fn multi [coll n]
    (mapcat #(repeat n %1) coll)))

(defcheck solution-6871c37f
  (fn [l n]
    (reduce #(into %1 (repeat n %2)) [] l)))

(defcheck solution-6a574adb
  (fn [coll n](mapcat #(repeat n %) coll)))

(defcheck solution-6af9a283
  (fn n-times-elem [lst n]
    (reverse
      (reduce (fn [xs x] (apply conj xs (take n (repeat x))))
        '()
        lst))))

(defcheck solution-6b1ee773
  #(mapcat (fn [a] (repeat %2 a)) %))

(defcheck solution-6b88fe8e
  (fn [xs n] (apply concat (for [x xs]
                             (repeat n x)))))

(defcheck solution-6c14c588
  (fn [v n] (mapcat #(take n (repeat %)) v)))

(defcheck solution-6c15b94a
  (fn [coll n]
    (mapcat #(take n (cycle (list %))) coll)))

(defcheck solution-6c3157eb
  (fn [a k](mapcat #(repeat k %) a)))

(defcheck solution-6c759f07
  (fn [seq reps] (apply mapcat vector (take reps (repeat seq)))))

(defcheck solution-6c7e202b
  (fn [s n] (mapcat (partial repeat n) s)))

(defcheck solution-6d0259df
  (fn [items nrepeat]
    (mapcat (partial repeat nrepeat) items)))

(defcheck solution-6d1a69d2
  (fn [c v] (mapcat #(repeat v %) c)))

(defcheck solution-6d4f86a5
  (fn rec [ls n]
    (lazy-seq
      (if (empty? ls) ()
                      (concat
                       (repeat n (first ls))
                       (rec (rest ls) n))))))

(defcheck solution-6de0f26b
  #(reduce (fn [a b]
             (apply conj a (repeat %2 b))) [] %1))

(defcheck solution-6e247f8f
  (fn [coll n]
    (mapcat (partial repeat n) coll)))

(defcheck solution-6e24fa6d
  #(if (< %2 2) %1
                (apply interleave (repeat %2 %1))))

(defcheck solution-6e34bcc3
  (fn f
    ([s t]
     (f s t '()))
    ([s t r]
     (if (empty? s)
       (reverse r)
       (recur (rest s) t (into r (repeat t (first s))))))))

(defcheck solution-6e5cf143
  (fn rep [inseq times]
    (reduce #(concat %1 (repeat times %2)) [] inseq)))

(defcheck solution-6f04a637
  (fn [coll n] (reduce #(concat % (repeat n %2)) [] coll)))

(defcheck solution-6f18677b
  #(apply (if (> %2 1) interleave seq) (repeat %2 %)))

(defcheck solution-6f1b17b3
  (fn [args n] (mapcat #(repeat n %) args)))

(defcheck solution-6ff8c9d4
  (fn [input num]
    (loop [in input out []]
      (if (empty? in)
        (seq out)
        (recur (rest in) (apply conj out (repeat num (first in))))))))

(defcheck solution-70c2670a
  #(mapcat (apply juxt (repeat %2 identity)) %1))

(defcheck solution-70c53142
  #(apply mapcat list (repeat %2 %1)))

(defcheck solution-71236211
  (fn [c n]  (mapcat #(repeat n %) c)))

(defcheck solution-7163c5bf
  ;(fn silly-dup
  ;  [[x & xs] cnt]
  ;  (if x
  ;    (concat (take cnt (repeat x)) (silly-dup xs cnt))
  ;    []))
  ;
  (fn [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-72c1451e
  (fn [s n]
    (reduce #(concat %1 %2) (map #(repeat n %) s))
    ))

(defcheck solution-736302e2
  (fn replicate-seq
    [seq times]
    (loop [[h & t] seq
           accum []]
      (let [new-accum (loop
                       [count times
                        inner-accum accum]
                        (if (= count 0)
                          inner-accum
                          (recur (- count 1) (conj inner-accum h))))]
        (if (nil? t)
          (into () (reverse new-accum))
          (recur t new-accum))))))

(defcheck solution-73ca148d
  #(reduce concat (map (fn [y] (for [x (range %2)] y)) %1)))

(defcheck solution-73ce899d
  #(if (> %2 1)
     (apply interleave (replicate %2 %1)) %1))

(defcheck solution-746f448b
  (fn [s n](mapcat #(take n (repeat %)) s)))

(defcheck solution-748b4f4a
  (fn [seq n] (apply concat (map #(repeat n %) seq ))))

(defcheck solution-74bca7b4
  (fn [coll n]
    (reduce
      (fn [coll item]
        (concat coll (repeat n item))) '() coll)))

(defcheck solution-74c02280
  (fn [x,y] (mapcat #(repeat y %) x)))

(defcheck solution-74e55981
  (fn [col n] (mapcat (partial repeat n) col)))

(defcheck solution-756a7e97
  #(if (= 1 %2) %1 (apply interleave (repeat %2 %1))))

(defcheck solution-7595d9fe
  (fn [s n]
    (if (= 1 n)
      s
      (apply interleave (repeat n s))
      )
    ))

(defcheck solution-75cad722
  #(letfn [(worker-child [x1 s n]
             (if (zero? s)
               n
               (recur x1 (dec s) (conj n x1))))
           (worker [x s n]
             (if (empty? x)
               n
               (recur (rest x) s (worker-child (first x) s n))))]
     (worker %1 %2 [])))

(defcheck solution-761c348
  #(for [x %1 _ (range %2)] x))

(defcheck solution-76740249
  (fn [coll n] (mapcat (apply juxt (repeat n identity)) coll)))

(defcheck solution-7686e416
  (fn [x y] (reduce (fn [acc el] (concat acc (take y  (repeat el)) )) [] x)))

(defcheck solution-774d8b3a
  (fn [l n] (if (> n 1) (apply interleave (repeat n l)) l)))

(defcheck solution-781ab58d
  (fn dup [coll x]
    (apply concat
      (for [item coll]
        (repeat x item)))))

(defcheck solution-782d5442
  (fn [l n]
    (mapcat #(repeat n %) l)
    ))

(defcheck solution-784256bf
  (fn [s n] (apply concat (map #(repeat n %) s))))

(defcheck solution-78ac60f3
  (fn [s n]
    (mapcat #(repeat n %) s)))

(defcheck solution-792518bb
  (fn [coll anum]
    (for [v coll x (range anum)] v)
    ))

(defcheck solution-7956666c
  (fn [a b] (if (= b 1) a (apply interleave (repeat b a)))))

(defcheck solution-79931bfc
  (fn [coll number] (mapcat #(repeat number %) coll)))

(defcheck solution-7a96046b
  (fn [xs n]
    (mapcat (partial repeat n) xs)))

(defcheck solution-7b0a30e2
  (fn [coll n]  (reduce #(into %1 (repeat n %2)) [] coll)))

(defcheck solution-7bd9cfae
  #(apply concat (map repeat (repeat %2) %)))

(defcheck solution-7c00f31f
  (fn [c n]
    (reduce #(apply (partial conj %1) (repeat n %2)) [] c)))

(defcheck solution-7c515fa2
  (fn r [s t] (mapcat #(repeat t %) s)))

(defcheck solution-7c721bd5
  #_(fn replicater' [coll x]
      (reduce (fn [a c] (concat a (repeat x c))) [] coll))

  (fn rep-seq [xs n]
    (if (> n 1)
      (apply interleave (repeat n xs))
      xs)))

(defcheck solution-7d6f9678
  (fn [s n]
    (loop [s_ s i n ret '()]
      (cond
        (> i 0) (recur s_ (dec i) (conj ret (first s_)))
        (next s_) (recur (next s_) n ret)
        :else (into '() ret)))))

(defcheck solution-7d733ae4
  (fn [xs n]
    (reduce
      #(into %1 (take n (repeat %2))) [] xs)))

(defcheck solution-7e3ca11d
  (fn [s c]
    (mapcat #(repeat c %) s)))

(defcheck solution-7f01829f
  #(apply concat (map (fn [x] (take %2 (repeat x))) %)))

(defcheck solution-7f3a083f
  #(if (= 1 %2)
     %1
     (apply interleave (repeat %2 %))))

(defcheck solution-7f763d59
  #(loop [in %1 n %2 out []]
     (if (nil? (first in))
       out
       (recur (rest in) n (apply conj out (repeat n (first in))))
       )
     ))

(defcheck solution-7f7ce3fa
  #(mapcat (fn [a] (take %2 (repeat a))) %1))

(defcheck solution-8030360d
  #(for [x %1
         y (range %2)] x))

(defcheck solution-806f9825
  (fn [col n] (mapcat #(repeat n %) col  )))

(defcheck solution-808114f1
  #(reduce (fn [a, x] (apply conj a (repeat %2 x))) [] %1))

(defcheck solution-8083095e
  (fn [x n] (apply concat (map #(repeat n %) x))))

(defcheck solution-812caf74
  (fn [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-8183b5d5
  (fn [seq times] (reduce concat (for [x seq] (take times (repeat x))))))

(defcheck solution-81aaeb5d
  (fn [sezn n] (mapcat #(repeat n %) sezn)))

(defcheck solution-81b61541
  (fn [s n] (apply concat (map (partial repeat n) s))))

(defcheck solution-8212905e
  (fn [xs n] (apply concat (map #(repeat n %) xs))))

(defcheck solution-8340e61f
  #(apply concat (map (partial repeat %2) %)))

(defcheck solution-838bc98
  (fn [c r] (apply concat (map #(repeat r %) c))))

(defcheck solution-83d34985
  (fn[v n]
    (loop [ans [] vv v]
      (if (empty? vv)
        ans
        (recur
          (concat ans (loop [nn n aa []]
                        (if (= nn 0)
                          aa
                          (recur
                            (dec nn)
                            (conj aa (first vv))))))
          (next vv))))))

(defcheck solution-841ae47d
  #(if (= 1 %2)
     %
     (apply interleave (repeat %2 %))))

(defcheck solution-853e5840
  (fn [coll rep]
    (apply concat (map #(repeat rep %) coll))))

(defcheck solution-854a7358
  (fn dup
    ([s n] (dup '() s n))
    ([os is n] (if (empty? is)
                 os
                 (recur (concat os (repeat n (first is))) (rest is) n)))))

(defcheck solution-857cdf42
  (fn [a-seq n] (reverse (reduce #(into %1 (repeat n %2)) () a-seq))))

(defcheck solution-864b514d
  (fn [coll n]
    (mapcat (partial repeat n) coll)))

(defcheck solution-868a6506
  (fn replicateC [x n] (apply concat (map #(replicate n %) x))))

(defcheck solution-86e9c95b
  (fn [a-seq n] (mapcat #(repeat n %) a-seq)))

(defcheck solution-87a8e14
  (fn [s n]
    (mapcat (partial repeat n) s)))

(defcheck solution-87f68658
  (fn [lst n] (mapcat (fn [x] (repeat n x)) lst)))

(defcheck solution-886eb40
  #(for [x % y (range %2)] x))

(defcheck solution-8879e805
  (fn rep [c n]
    (mapcat #(repeat n %1) c)))

(defcheck solution-88f8ab62
  (fn replicate-sequence [x y]
    (if (empty? x)
      []
      (concat (take y (repeat (first x)))
              (replicate-sequence (rest x) y)))))

(defcheck solution-8936f187
  (fn [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-8940ea73
  (fn [seq cnt] (mapcat #(repeat cnt %) seq)))

(defcheck solution-8aa84bc0
  #(if (= %2 1) % (apply interleave (repeat %2 %1))))

(defcheck solution-8aa969ac
  (fn [a b]
    (reduce concat
      (map
        (fn [x]
          ((fn tmp [e f]
             (if (= f 1)
               [e]
               (conj (tmp e (dec f)) e)))
           x b))
        a))))

(defcheck solution-8ac96df6
  (fn [xs n]
    (mapcat (partial repeat n) xs)))

(defcheck solution-8b140741
  (fn rep [liste x]
    (reduce #(concat
              %1
              (
               (fn cn [l n] (if (= n 1) (list l) (conj (cn l (dec n)) l)))
               %2
               x
               )
              )
      '()
      liste
      )
    ))

(defcheck solution-8b9771cc
  (fn [x c] (reverse (reduce #(apply conj %1 %2 ) (map #(take c (repeat %)) x )))))

(defcheck solution-8c3e2503
  (fn [ls n]
    (mapcat #(repeat n %) ls)
    ))

(defcheck solution-8c77a630
  (fn [xs n] (reduce #(concat %1 (replicate n %2)) '() xs)))

(defcheck solution-8c949892
  (fn replicate [s ntimes]
    (reverse (reduce #(apply conj % (repeat ntimes %2)) '() s))))

(defcheck solution-8d558b8
  (fn [s c]
    (apply concat (map #(take c (iterate identity %)) s))))

(defcheck solution-8e0fec10
  #(for [x % y (repeat %2 x)] y))

(defcheck solution-8e3bbe0b
  (fn [xs n]
    (mapcat #(repeat n %) xs)))

(defcheck solution-8e61ba2d
  (fn [ s n] (reduce #(concat % (repeat n %2)) [] s)))

(defcheck solution-8e7ee7eb
  (fn [l n]
    (reduce (fn [cl e]
              (concat cl (repeat n e)))
      [] l)))

(defcheck solution-8ee639fa
  #(mapcat (partial take %2) (map repeat %1)))

(defcheck solution-8ef62f4c
  #(if(= %2 1) %1
               (apply interleave (take %2 (repeat %1)))))

(defcheck solution-8f0cba61
  (fn [v n]
    (mapcat
      #(repeat n %)
      v)))

(defcheck solution-8f0ceb9d
  (fn [coll n]
    (reduce concat (map #(repeat n %) coll))))

(defcheck solution-8f260b96
  (fn [coll n] (mapcat #(replicate n %) coll)))

(defcheck solution-8f607792
  (fn [l n] (mapcat (partial repeat n) l)))

(defcheck solution-8f64ee87
  (fn [xs n]
    (reduce (fn [a e] (concat a (take n (repeat e))))
      []
      xs)))

(defcheck solution-8f710dbb
  (fn [xs times]
    (reduce (fn [agg now] (into agg (repeat times now))) [] xs)))

(defcheck solution-8f729203
  (fn [items rpt]
    (mapcat #(repeat rpt %) items)
    ))

(defcheck solution-90314be0
  #(mapcat (fn [v] (repeat %2 v)) %))

(defcheck solution-904de0ab
  (fn [coll cnt]
    (reduce #(into %1 (take cnt (repeat %2)))   [] coll)))

(defcheck solution-905d2326
  (fn [coll times]
    (loop [[x & xs :as coll] coll
           acc []]
      (if (empty? coll) acc
                        (recur xs (concat acc (repeat times x)))))))

(defcheck solution-90cc7a91
  (fn [s, n]
    (mapcat (partial repeat n) s)))

(defcheck solution-912b1976
  (fn [i c] (mapcat #(repeat c %) i)))

(defcheck solution-916d0f38
  (fn [s c] (apply concat (map (partial repeat c) s))))

(defcheck solution-91d9cd2
  (fn replicate-seq [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-91db3d25
  #(apply mapcat vector (repeat %2 %1)))

(defcheck solution-91fd38d7
  (fn repl [lst times]
    (cond
      (empty? lst) '()
      :else
      (into
        (repl (rest lst) times)
        (repeat times (first lst))))))

(defcheck solution-9228e153
  (fn dup [in, n]
    (loop [in-seq in, out-seq [], cnt n]
      (if (= (first in-seq) nil)
        out-seq
        (recur (rest in-seq)
          (loop [out-seq out-seq, element (first in-seq), cnt cnt]
            (if (= cnt 0)
              out-seq
              (recur (conj out-seq element) element (dec cnt))
              )
            )
          cnt)))))

(defcheck solution-92838f96
  #(reduce (fn [xs x] (apply conj xs (take %2 (repeat x)))) [] %1))

(defcheck solution-9287ffa3
  (fn [l n] (mapcat #(repeat n %1) l)))

(defcheck solution-92a0937a
  (fn rep [c n]
    (if (= 1 n)
      c
      (apply interleave (repeat n c)))))

(defcheck solution-92ee4165
  (fn [coll repl] (mapcat #(repeat repl %) coll)))

(defcheck solution-931f3353
  (fn [x reps] (apply concat (map (fn [y] (repeat reps y)) x))))

(defcheck solution-932b4eaf
  (fn [xs cnt] (mapcat #(replicate cnt %) xs)))

(defcheck solution-934b0b60
  (fn [s times]
    (reduce (fn [a e] (concat a (map (fn [_] e) (range times))) ) [] s)))

(defcheck solution-9383025f
  #(apply concat (for [a %] (take %2 (repeat a)))))

(defcheck solution-93c88dba
  (fn [coll n]
    (reduce (fn [a b] (concat a (take n (repeat b)))) [] coll)))

(defcheck solution-93e07601
  (fn [items times]
    (loop [items items result []]
      (if (empty? items)
        result
        (recur (rest items) (concat result (map (constantly (first items)) (range times))))))))

(defcheck solution-94067570
  (fn[a-seq n]
    (mapcat #(repeat n %) a-seq)))

(defcheck solution-9409afb1
  (fn [xs n]
    (for [x xs i (range n)] x)))

(defcheck solution-9473dd9d
  (fn r [s n] (mapcat #(repeat n %) s)))

(defcheck solution-959332f3
  (fn dup
    ([lst n] (dup lst n []))
    ([lst n l] (if (empty? lst) l (dup (next lst) n (apply conj l (replicate n (first lst))))))
    ))

(defcheck solution-9647fb3b
  (fn replicateseq [x n]
    (if (= n 0)
      (empty x)
      (if (= n 1)
        x
        (apply interleave (vec (repeat n x)))))))

(defcheck solution-97faa777
  (fn replic
    [arr n]
    (reduce concat (map #(repeat n %) arr ))
    ))

(defcheck solution-982a4d51
  (fn [x n] (reduce concat (map (partial repeat n) x))))

(defcheck solution-9867ddf5
  (fn [coll num]
    (if (= num 1)
      coll
      (apply interleave (replicate num coll)))))

(defcheck solution-9954906e
  (fn dup [l n] (mapcat #(repeat n %) l)))

(defcheck solution-99a1f677
  (fn [s n] (mapcat (fn [x] (repeat n x)) s)))

(defcheck solution-99e5d400
  #(reduce (fn [acc e] (concat acc (repeat %2 e))) '() %))

(defcheck solution-9adab034
  (fn [s n]
    (reduce
      #(concat %1 (take n (repeat %2)))
      []
      s)))

(defcheck solution-9afa9a13
  (fn replica [s n]
    (if (empty? s)
      nil
      (concat (repeat n (first s)) (replica (next s) n)))))

(defcheck solution-9b524b49
  (fn [xs r]
    (mapcat (fn [x] (repeat r x))
      xs)))

(defcheck solution-9c1ba99d
  (fn f [c n]
    (if (empty? c)
      '()
      (lazy-cat (repeat n (first c)) (f (rest c) n)))))

(defcheck solution-9cb9363d
  (fn [x y]
    (if (= y 1) x (apply interleave (replicate y x)))))

(defcheck solution-9d4fc768
  (fn [ls n]
    (mapcat #(repeat n %) ls)))

(defcheck solution-9d76c93
  #(mapcat (fn [e] (repeat %2 e)) %))

(defcheck solution-9e6a5f15
  (fn [s n] (mapcat #(take n (repeat %)) s)))

(defcheck solution-9e96c017
  (fn [s n] (reduce #(into % (repeat n %2)) [] s)))

(defcheck solution-9edf4014
  (fn[a,b](apply concat (map (fn[x](repeatedly b #(identity x))) a))))

(defcheck solution-9f365679
  #(if (= %2 1)
     %1
     (apply interleave
       (take %2 (iterate identity %1)))))

(defcheck solution-9fcde0de
  (fn repl [s n]
    (letfn
     [(inner [s m]
        (if (seq s)
          (if (zero? m)
            (lazy-seq (inner (rest s) n))
            (lazy-seq (cons (first s) (inner s (- m 1)))))
          '()))]
      (inner s n))))

(defcheck solution-a105167f
  (fn [l n]
    (loop [l l i 0 r []]
      (cond
        (empty? l) r
        (< i n) (recur l (inc i) (conj r (first l)))
        :else (recur (rest l) 0 r)))))

(defcheck solution-a1a94ba
  (fn [S n]
    (reduce (fn [lst x] (concat lst (for [y (range n)] x))) () S)
    ))

(defcheck solution-a1bf6be1
  (fn [s n]
    (reduce concat (map (partial repeat n) s))))

(defcheck solution-a1cbcd21
  (fn [coll r] (mapcat #(repeat r %) coll)))

(defcheck solution-a1f02af3
  (fn [s n]
    (mapcat (partial replicate n) s)))

(defcheck solution-a224840b
  (fn repl [xs n]
    (mapcat (partial repeat n) xs)))

(defcheck solution-a231475e
  #(if (> %2 1) (apply interleave (replicate %2 %1 )) %1))

(defcheck solution-a27b8ecf
  (fn replicate-sequence [collection n]
    (apply concat (map (partial repeat n) collection))))

(defcheck solution-a28e4dc6
  (fn [coll spread] (mapcat #(repeat spread %) coll)))

(defcheck solution-a2d08d2
  (fn [coll times]
    (apply concat (map #(take times (iterate identity %)) coll))))

(defcheck solution-a2fe696e
  (fn [xs n]
    (reduce (fn [rs x] (concat rs (for [i (range n)] x))) [] xs)))

(defcheck solution-a3220bee
  (fn [lst cnt] (mapcat #(repeat cnt %) lst)))

(defcheck solution-a447ee98
  (fn [xs n]
    (reduce  #(concat %1 (for [i (range n)]
                           %2)) [] xs)


    ))

(defcheck solution-a59b7960
  (fn [s count]
    (mapcat (fn [x] (take count (repeat x))) s)))

(defcheck solution-a5b7bf3
  (fn[s n] (mapcat #(repeat n %) s)))

(defcheck solution-a6ad1b58
  #(if (= 1 %2)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-a72c3a4c
  (fn [x n]
    (apply concat (map #(repeat n %) x))))

(defcheck solution-a799e055
  (fn repseq [mycol repCount] (if (empty? mycol) '() (let [firstElem (first mycol)] (concat (replicate repCount firstElem) (repseq (rest mycol) repCount))))))

(defcheck solution-a7c7032a
  (fn [s n] (reduce (fn [l r] (concat l (repeat n r))) () s)))

(defcheck solution-a7d7f93
  (fn [col x]
    (reduce concat (map #(repeat x %) col)  )))

(defcheck solution-a8f4e5f3
  #(if (> %2 1)
     (apply interleave (repeat %2 %1))
     %1))

(defcheck solution-a9973d01
  (fn [l n] (if (= n 1) l (apply interleave (repeat n l)))))

(defcheck solution-a99bf0a1
  (fn replicate [coll x]
    (reduce concat (map (fn [term]
                          (take x (repeat term))) coll))))

(defcheck solution-aa956ae5
  #(if (= 1 %2)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-ab20f233
  (fn r [v t]
    (when-first [f v]
      (concat (repeat t f) (r (rest v) t)))))

(defcheck solution-ab4b256f
  (fn [coll n] (if (= n 1) coll (apply interleave (repeat n coll)))))

(defcheck solution-abd1e4e9
  (fn [a b] (mapcat #(repeat b %) a)))

(defcheck solution-abf96972
  (fn [xs r] (apply concat (map #(replicate r %1) xs))))

(defcheck solution-ac033d1b
  (fn rep [s n]
    (if-let [[x & xs] (seq s)]
      (concat (replicate n x) (rep xs n)))))

(defcheck solution-ac390fe
  (fn [sq, times]
    (apply concat (map #(repeat times %) sq))))

(defcheck solution-acc87280
  (fn [col n]
    (mapcat #(repeat n %) col)))

(defcheck solution-acc87b72
  (fn [coll cnt] (reduce #(concat %1 (replicate cnt %2)) [] coll)))

(defcheck solution-ad86f267
  #(if (> %2 1) (apply interleave (repeat %2 %)) %))

(defcheck solution-adcb0450
  (fn [coll n]
    (let [f (fn [orig result]
              (if (empty? orig)
                result
                (recur (rest orig) (apply conj result (take n (repeat (first orig)))))))]
      (f coll []))))

(defcheck solution-ae44cc25
  (fn [seq count]
    (apply concat (map #(repeat count %) seq))))

(defcheck solution-af0126da
  (fn [seqs times]
    (mapcat #(take times (iterate identity %)) seqs)))

(defcheck solution-b0046ab7
  (fn [s n] (reduce concat (map #(repeat n %) s)) ))

(defcheck solution-b0304f36
  #(mapcat (fn [el] (repeat %2 el)) %1))

(defcheck solution-b06870b8
  (fn [xs n] (for [x xs y (range n)] x)))

(defcheck solution-b0bb6e47
  (fn [x i]
    (if (= 1 i)
      x
      (apply interleave
        (take i (partition (count x) (cycle x)))))))

(defcheck solution-b0cbee50
  (fn replicate [s n]
    (if (empty? s)
      []
      (let [[x & rest] s]
        (concat (for [dummy (range n)] x) (replicate rest n))))))

(defcheck solution-b1a11709
  (fn [xs n]
    (reduce #(concat %1 %2) [] (map #(repeat n %) xs))
    ))

(defcheck solution-b1acf38f
  #(apply concat (map (fn [x] (repeat %2 x)) %)))

(defcheck solution-b1bf25af
  (fn rep [x y]
    (if (empty? x)
      x
      (concat (repeat y (first x)) (rep (rest x) y)))))

(defcheck solution-b2a42f93
  (fn [c n]
    (reduce #(loop [n n r %1] (if (= n 0) r (recur (dec n) (conj r %2)))) [] c)
    ))

(defcheck solution-b2b3a0b4
  (fn [xs n] (mapcat (partial repeat n) xs)))

(defcheck solution-b2b7f53c
  (fn [seqs n]
    (letfn[
           (ble [e ct]
             (loop[rs '()
                   ct ct]
               (if(zero? ct)
                 rs
                 (recur (cons e rs)
                   (dec ct)))))]
      (loop [result '()
             others seqs]
        (if(empty? others)
          result
          (recur (concat result (ble (first others) n))
            (rest others)))))))

(defcheck solution-b32d0432
  (fn [x y] (loop [r x result '()] (if (empty? r) result (recur  (rest r) (concat result (repeat y (first r))))))))

(defcheck solution-b3b8188a
  (fn [xs n]
    (apply concat (map #(repeat n %) xs))))

(defcheck solution-b43c046
  (fn foo [coll n]
    (if (= n 1) coll
                (apply interleave (repeat n coll)))))

(defcheck solution-b44da365
  (fn [s n]
    (reduce #(concat %1 (repeat n %2)) [] s)))

(defcheck solution-b4f2b370
  (fn my-replicate
    ([l n] (my-replicate l n []))
    ([l n resp]
     (if (= l [])
       resp
       (my-replicate (rest l) n (apply conj resp (repeat n (first l))))))))

(defcheck solution-b55a5020
  (fn [v n]
    (cond (zero? n) []
          (= 1 n) v
          :else (apply interleave (repeat n v)))))

(defcheck solution-b5935efc
  (fn [col rep] (mapcat #(repeat rep %) col)))

(defcheck solution-b5a74c5f
  (fn  [coll n]
    (->> coll
      (mapcat (partial repeat n)))))

(defcheck solution-b5ae3e33
  (fn [l times] (loop [l l result '()]
                  (if (empty? l) result
                                 (recur (rest l) (concat result (repeat times (first l))))))))

(defcheck solution-b5e53ccf
  (fn[v c](mapcat #(repeat c %)v)))

(defcheck solution-b6850d9d
  (fn repf [col num]
    (reduce #(concat %1 (repeat num %2)) '() col)))

(defcheck solution-b6aca849
  #(reduce concat (for [item %1] (repeat %2 item))))

(defcheck solution-b71c27d
  (fn [l n]
    (if (= n 1)
      l
      (apply interleave (repeat n l)))))

(defcheck solution-b74b1f28
  (fn [xes c] (loop [xs xes
                     curr 0
                     result []]
                (if (nil? xs)
                  result
                  (if (= curr c)
                    (recur (next xs) 0 result)
                    (recur xs (inc curr) (conj result (first xs)))
                    )
                  )
                )
    ))

(defcheck solution-b7f33198
  (fn [s n]
    (reduce #(into %1 (repeat n %2)) [] s)))

(defcheck solution-b832759c
  (fn [xs n] (mapcat #(take n (cycle (list %))) xs)))

(defcheck solution-ba11ace5
  (fn[coll times]
    (mapcat identity
      (map #(repeat times %) coll))))

(defcheck solution-ba29bb2
  (fn [ x n ] (reduce concat (map (partial repeat n) x))))

(defcheck solution-ba492d7a
  (fn rep [x m]
    (letfn [
            (repe [e n]
              (if (= n 0)
                '()
                (cons e (repe e (- n 1)))
                )
              )
            ]
      (if (= x '())
        '()
        (concat (repe (first x) m) (rep (rest x) m))
        )
      )
    ))

(defcheck solution-ba792595
  (fn dup [l n]
    (if
     (= n 1)
      l
      (apply interleave (repeat n l)))))

(defcheck solution-baf63708
  (fn [initial-coll n]
    (reduce
      (fn [coll val]
        (concat coll (repeat n val)))
      '() initial-coll)))

(defcheck solution-baf9f3ac
  (fn [s n] (mapcat #(repeat n %)s)))

(defcheck solution-bc1e016a
  #(reduce concat (for [i %] (repeat %2 i))))

(defcheck solution-bc536702
  #(if (< %2 2) %1 (apply interleave (take %2 (repeat %1)))))

(defcheck solution-bc98e2a0
  (fn [col x] (mapcat #(repeat x %) col)))

(defcheck solution-bc9cb5ae
  (fn rep [s n]
    (if (empty? s)
      '()
      (concat (repeat n (first s)) (lazy-seq (rep (rest s) n))))))

(defcheck solution-bce680b1
  #(loop [s %1 n %2 r []]
     (cond (empty? s) r
           (= n 1) (recur (rest s) %2 (conj r (first s)))
           :else (recur s (dec n) (conj r (first s))))))

(defcheck solution-bd1b3ff3
  (fn [coll n]
    (mapcat #(repeat n %) coll)
    ))

(defcheck solution-bdaf1079
  (fn [lst cnt]
    (mapcat #(repeat cnt %) lst)))

(defcheck solution-bf759d98
  (fn repl-seq [lst num]
    (apply concat (map #(repeat num %) lst))))

(defcheck solution-bfa07cfb
  (fn dup2 [s n]
    (reduce (fn [x y] (concat x (repeat n y))) () s)))

(defcheck solution-bfee8866
  (fn [xs n]
    (mapcat (partial repeat n) xs)))

(defcheck solution-bffcbf7c
  #(reduce concat (map (partial repeat %2) %)))

(defcheck solution-c0369cb7
  (fn repl [xs n]
    (if (empty? xs)
      ()
      (concat (repeat n (first xs)) (repl (rest xs) n)))))

(defcheck solution-c060dde
  (fn [x,n]
    (mapcat (partial repeat n) x)))

(defcheck solution-c0e2ecc9
  (fn replicate [coll a-num]
    (reduce #(concat %1 (repeat a-num %2))
      []
      coll)))

(defcheck solution-c0e4106f
  #(if (> %2 1)
     (apply interleave (repeat %2 %1))
     %1))

(defcheck solution-c13ab059
  (fn [coll times] (mapcat (partial repeat times) coll)))

(defcheck solution-c170a230
  #(apply concat (for [x %1] (repeat %2 x))))

(defcheck solution-c217aec5
  (fn [seq count] (apply concat (map #(take count (repeat %)) seq))))

(defcheck solution-c30238c0
  (fn dupeeach [sq times]
    (mapcat (partial repeat times) sq)))

(defcheck solution-c3499d5
  (fn [s x] (mapcat #(repeat x %) s)))

(defcheck solution-c47d2e17
  (fn [coll iter]
    (mapcat (fn [x] (repeat iter x)) coll)))

(defcheck solution-c4ab93ab
  (fn [coll n]
    (mapcat #(repeat n %) coll)
    ))

(defcheck solution-c52dccb6
  (fn f [s n] (mapcat #(repeat n %) s)))

(defcheck solution-c54a9f9
  (fn dupl[input x]
    (reduce
      (fn[l ll]
        (concat l ll))
      ()
      (map #(repeat x %) input))))

(defcheck solution-c55abb3e
  #(mapcat (fn [e] (repeat %2 e)) %1))

(defcheck solution-c5a244c3
  (fn [xs n]
    (reduce concat (map (partial replicate n) xs))))

(defcheck solution-c5a968a2
  (fn [v n] (apply concat (map #(repeat n %) v))))

(defcheck solution-c5c9b43b
  (fn [l n] (mapcat #(take n (iterate identity %)) l)))

(defcheck solution-c5e10cb4
  #(for [x %1 y (range %2)] x))

(defcheck solution-c7bbc8ba
  #(if (= 1 %2) % (apply interleave (repeat %2 %))))

(defcheck solution-c7d972be
  (fn [xs n] (apply concat (map #(repeat n %) xs)) ))

(defcheck solution-c80602c4
  #(if (< 1 %2)
     (apply interleave (repeat %2 %1))
     %1))

(defcheck solution-c8a3e1e
  (fn mult[sequence, times]
    (loop [ ret [] s sequence]
      ( if (seq s) (recur
                     (loop [n times ret ret]
                       (if (zero? n) ret
                                     (recur (dec n) (conj ret (first s)))))
                     (rest s))
                   ret))))

(defcheck solution-c8b3fac8
  (fn [coll n]
    (if (= n 1)
      coll
      (apply interleave (repeat n coll)))))

(defcheck solution-c940613
  (fn [s c] (apply concat (map #(repeat c %1) s))))

(defcheck solution-c9527133
  (fn [coll x] (mapcat #(repeat x %) coll)))

(defcheck solution-c97e141d
  (fn [xs n]
    (reduce #(concat %1 (repeat n %2)) [] xs)))

(defcheck solution-c9b0142
  (fn [xs n]
    (mapcat #(repeat n %) xs)
    ))

(defcheck solution-c9c344d5
  (fn [col n]
    (reverse (reduce #(apply conj %1 (repeat n %2)) '() col))))

(defcheck solution-c9d5b859
  (fn
    [se1 cnt]
    (apply concat
      (apply map vector
        (take
          cnt
          (repeat se1))))))

(defcheck solution-ca5be8f7
  (fn [xs n]
    (apply concat
      (map #(replicate n %) xs))))

(defcheck solution-caa48661
  (fn [coll n]
    (reduce concat
      (map (partial repeat n) coll)
      )
    ))

(defcheck solution-cabc19b1
  (fn [coll n] (mapcat #(repeat n %) coll)))

(defcheck solution-ccfb5bd2
  (fn f [s,n]
    (if (= 1 (count s))
      (repeat n (first s))
      (concat (repeat n (first s)) (f (rest s) n)))))

(defcheck solution-cd90289e
  (fn redup [xs n]
    (if (= '() (rest xs))
      (repeat n (first xs))
      (concat (repeat n (first xs)) (redup (rest xs) n)))))

(defcheck solution-cdb14e54
  (fn [col times]
    (for [item col t (range times)] item)))

(defcheck solution-cdda1710
  (fn replicate [lat n]
    (reduce concat (map #(take n (repeat %)) lat))))

(defcheck solution-ce3933b6
  (fn [l n]
    (mapcat #(repeat n %) l)))

(defcheck solution-ce4212a8
  (fn replicate [s n]
    (if (seq s)
      (concat (repeat n (first s)) (replicate (rest s) n))
      s)))

(defcheck solution-ced7905a
  #(cond
     (= %2 1) %1
     :else (apply interleave (repeat %2 %1))
     ))

(defcheck solution-cfa5a017
  (fn [lst n] (mapcat #(repeat n %) lst)))

(defcheck solution-cfe66a8b
  (fn rep
    ([l n] (rep l n []))
    ([l n result]
     (if (empty? l)
       result
       (rep
         (rest l)
         n
         (into result (vec (repeat n (first l)))))))))

(defcheck solution-d03fea60
  (fn [c n] (reduce #(apply conj %1 (repeat n %2)) [] c)))

(defcheck solution-d1494c70
  (fn[z,y] (reduce concat (map (fn[x] (repeat y x)) z))))

(defcheck solution-d1937b69
  (fn [s n]
    (mapcat #(take n (iterate identity %)) s)))

(defcheck solution-d1c1b92
  (fn [c n](mapcat #(take n (cycle [%])) c)))

(defcheck solution-d243a152
  (fn [x,y](mapcat #(repeat y %) x)))

(defcheck solution-d26579a
  (fn [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-d275d26b
  (fn [vs n]
    (if (= 1 n)
      vs
      (apply interleave (repeat n vs)))))

(defcheck solution-d32e5992
  (fn repl [a-seq n]
    (mapcat #(repeat n %) a-seq)
    ))

(defcheck solution-d3749df6
  (fn [s n]
    (apply concat
      (map (fn [x] (repeatedly n #(identity x))) s))))

(defcheck solution-d3d33691
  #(if (= %2 1)
     (seq %)
     (apply interleave (repeat %2 %))))

(defcheck solution-d47f1d9c
  (fn [c n] (mapcat #(repeat n %) c) ))

(defcheck solution-d49bc8ef
  (fn [s n] (if (= n 1) s
                        (apply interleave (repeat n s)))))

(defcheck solution-d53ab00e
  (fn [x y]
    (reduce #(into %1 (repeat y %2)) [] x)))

(defcheck solution-d67d00da
  #(apply concat (map (fn [e] (repeat %2 e)) %)))

(defcheck solution-d757eaf9
  (fn [coll, n] (mapcat #(repeat n %) coll)))

(defcheck solution-d783e439
  (fn [s n] (seq (reduce #(apply conj %1 (repeat n %2)) '[] s))))

(defcheck solution-d7bae350
  (fn [xs t]
    (mapcat
      (fn [x] (map (constantly x) (range t)))
      xs)))

(defcheck solution-d7dd9b15
  (fn [coll count]
    (mapcat (fn [el] (repeat count el))
      coll)))

(defcheck solution-d8266724
  (fn
    [seq n]
    (loop [seq seq list '()]
      (if (= nil (first seq))
        list
        (recur (rest seq) (concat list (repeat n (first seq)) ))
        )
      )))

(defcheck solution-d95f6826
  #(apply (partial mapcat list) (repeat %2 %1)))

(defcheck solution-da5676d1
  (fn peu [x y] (mapcat #(take y (iterate identity %)) x)))

(defcheck solution-da9d966f
  #(apply concat (for [s %1] (take %2 (cycle [s])))))

(defcheck solution-db77d0ca
  (fn [x n]
    (if (= n 1)
      x
      (apply interleave (repeat n x)))))

(defcheck solution-dbeb73d8
  (fn [s k] (if (= k 1) s (apply interleave (repeat k s)))))

(defcheck solution-dc050461
  (fn [li rep] (apply concat (map #(repeat rep %) li))))

(defcheck solution-dc07758a
  (fn [xs n] (reduce into [] (map (partial repeat n) xs))))

(defcheck solution-dc6b2bcc
  (fn [in n]
    (mapcat (fn [i] (take n (iterate   #(or % % )  i) ) )  in)
    ))

(defcheck solution-dc9984a
  #(reduce concat (map (fn [x] (repeat %2 x)) %1)))

(defcheck solution-dcceb609
  (fn [c n]
    (mapcat (partial repeat n) c)))

(defcheck solution-dd02f607
  (fn [x y]  (reduce concat (map (fn [item] (repeat y item) ) x))))

(defcheck solution-dd0758df
  (fn [coll n]
    (apply concat (map #(repeat n %) coll))))

(defcheck solution-dd9d17d5
  (fn
    [a-seq replicator]
    (loop [l-seq a-seq el (first l-seq) acc []]
      (if (empty? l-seq)
        acc
        (recur (rest l-seq)
          (first (rest l-seq))
          (apply concat acc (repeat
                              replicator
                              (vector (first l-seq)))))))))

(defcheck solution-de82517d
  (fn [s n] (reduce #(concat % (repeat n %2)) '() s)))

(defcheck solution-dec775f4
  (fn [c n]
    (reduce #(into %1 (repeat n %2)) [] c)))

(defcheck solution-e00f957d
  (fn [s n]
    (letfn [(rplct [s result]
              (if (seq s)
                (let [f (first s)]
                  (recur
                    (rest s)
                    (concat result (repeat n f))))
                result))]
      (rplct s []))))

(defcheck solution-e0455a80
  (fn [s k] (mapcat #(repeat k %) s)))

(defcheck solution-e0ba8fc4
  (fn [xs n]
    (reduce (fn [a b] (nth (iterate #(conj % b) a) n)) [] xs)
    ))

(defcheck solution-e0c3632c
  (fn [xs n]
    (if (> n 1)
      (apply interleave (for [i (range n)] xs))
      xs)))

(defcheck solution-e1660336
  (fn [seq n] (mapcat (fn [i] (repeatedly n #(identity i))) seq)))

(defcheck solution-e1abd379
  #(mapcat (partial repeat %2)%1))

(defcheck solution-e2449ab2
  (fn __
    [l n]
    (mapcat #(repeat n %) l)))

(defcheck solution-e264c139
  (fn [value-seq t] (mapcat #(take t (iterate identity %)) value-seq)))

(defcheck solution-e2994ce6
  (fn [c n]
    (apply concat (map #(for [x (range n)] %) c))))

(defcheck solution-e2d5f8ce
  (fn [coll n]
    (mapcat (fn [el]
              (repeat n el)) coll)))

(defcheck solution-e57dce55
  #(apply concat (map (partial repeat %2) %1)))

(defcheck solution-e58134a7
  (fn [coll x] (mapcat #(take x (iterate identity %)) coll)))

(defcheck solution-e5e1f6a5
  #(apply concat (for [i %1] (repeat %2 i))))

(defcheck solution-e611b4d7
  (fn [lst ct] (reduce #(concat %1 (repeat ct %2)) () lst)))

(defcheck solution-e649d95a
  (fn rep [[h & t] n]
    (if (nil? h)
      h
      (concat (repeat n h)
              (rep t n)))))

(defcheck solution-e6c57218
  #(mapcat (partial repeat %2) %1))

(defcheck solution-e6d3ed18
  (fn[l n] (mapcat #(repeat n %) l)))

(defcheck solution-e6e9ebab
  (fn [sq n]
    ((fn [sq r]
       (if (empty? sq)
         r
         (recur (rest sq) (concat r (repeat n (first sq))))))
     sq [])))

(defcheck solution-e7279d54
  (fn [coll n] (reduce (fn [a e] (concat a (repeat n e))) [] coll)))

(defcheck solution-e865ea7
  (fn rep-mapcat [coll n]
    (mapcat (partial repeat n) coll)))

(defcheck solution-e8d6487f
  #_(fn [a b] (if (= 1 b) a
                          (concat (apply interleave
                                    (take b (iterate identity a))))))

  #_(fn [a b]
      (reduce #(concat % (repeat b %2))
        [] a))

  #_(fn [a b]
      (mapcat #(repeat b %) a))

  #(mapcat (partial repeat %2) %))

(defcheck solution-e96da3b2
  (fn [x y]
    (mapcat (partial replicate y) x
      )
    ))

(defcheck solution-ea5ab5b8
  (fn [s n]
    (if (= n 1)
      (seq s)
      (apply interleave (take n (repeat s))))))

(defcheck solution-ea6046a9
  (fn dd [[f & n] r]
    (let [fr (repeat r f)]
      (if (empty? n)
        fr
        (concat fr (dd n r))))))

(defcheck solution-eabfb827
  (fn rep [xs n]
    (apply concat
      (map (partial replicate n)
        xs))))

(defcheck solution-eb0db3a
  #(loop [acc '() [x & xs] %]
     (if (nil? x) acc
                  (recur (concat acc (repeat %2 x)) xs))))

(defcheck solution-eb591bbc
  (fn [x n]
    (reduce into (map #(repeat n %) (reverse x)))))

(defcheck solution-eb5a260c
  #(mapcat (fn [n] (repeat %2 n)) %))

(defcheck solution-ec08e510
  (fn replicateCollection
    [collection numberReplicates]
    "Returns a collection with each element duplicated the specified number"
    (mapcat #(take numberReplicates (cycle (conj '() %))) collection)))

(defcheck solution-ec8a7c8d
  #(let [repTimes %2
         repStr %1]
     (mapcat (partial repeat repTimes) repStr)))

(defcheck solution-ecad5f71
  (fn f [x y]
    (if (empty? x) '()
                   (concat (repeat y (first x)) (f (rest x) y)))))

(defcheck solution-eda8e32f
  (fn [l n]  (mapcat #(repeat n %) l)))

(defcheck solution-edbf7cfc
  (fn [c n] (mapcat #(repeat n %) c)))

(defcheck solution-ee0d4404
  (fn [colls n]
    (mapcat
      (fn [x]
        ((fn [colls x n]
           (if (= n 0)
             colls
             (recur (conj colls x) x (dec n)))) [] x n))
      colls)))

(defcheck solution-eee558dd
  (fn [s n]
    (reduce #(concat %1 (replicate n %2)) [] s)
    ))

(defcheck solution-eef00651
  (fn [xs n]
    (mapcat #(take n (repeat %)) xs)))

(defcheck solution-efa8969f
  #(if (= %2 1)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-efb4b478
  (fn [seq cnt]
    (for [x seq y (repeat cnt x)] y)))

(defcheck solution-efe6bb95
  (fn replicate [coll n]
    "33. Write a function which replicates each element of a sequence a variable number of times."
    (mapcat (fn [x] (repeat n x))  coll)))

(defcheck solution-efe8b718
  (fn [coll n]
    (mapcat #(replicate n %) coll)))

(defcheck solution-effa9868
  (fn [xs n]
    (reduce #(into %1 (repeat n %2)) [] xs)))

(defcheck solution-f07acc85
  (fn [s r]
    (apply concat
      (for [e s]
        (repeat r e)))))

(defcheck solution-f1715416
  #(mapcat (fn [x] (repeat %2 x)) %1 ))

(defcheck solution-f171b016
  (fn [s, count]
    (seq (mapcat (fn [e]
                   (take count (repeatedly #(identity e)))) s))))

(defcheck solution-f17d6a25
  (fn [s n] (reduce (fn [m v] (into m (repeat n v))) [] s)))

(defcheck solution-f1a1a173
  (fn !
    ([x y]
     (if (empty? x) '()
                    (let [rf (replicate y (first x))]
                      (concat rf (! (rest x) y)))))))

(defcheck solution-f1a463c3
  (fn [x n] (mapcat (partial repeat n) x)))

(defcheck solution-f1d60f04
  #(if (== %2 1) (seq %) (apply interleave (repeatedly %2 (constantly %)))))

(defcheck solution-f24f4afb
  (fn repseq [coll n]
    (mapcat (partial repeat n) coll)))

(defcheck solution-f289144d
  (fn [x y] (mapcat (partial repeat y) x)))

(defcheck solution-f3518050
  (fn [s, n] (mapcat #(repeat n %) s)))

(defcheck solution-f4a9ea3d
  #(for [x % n (range 0 %2)] x))

(defcheck solution-f53fe221
  #(apply concat (apply map list (repeat %2 %1))))

(defcheck solution-f5444b0b
  (fn replicate [s c]
    (cond
      (empty? s) '()
      :else
      (concat
       (take c (repeat (first s)))
       (replicate (rest s) c)))))

(defcheck solution-f5cd13f8
  (fn [l n]
    (loop [l l rv []]
      (if (empty? l)
        rv
        (recur (rest l) (concat rv (take n (repeat (first l)))))))))

(defcheck solution-f699a92d
  #(apply concat
     (map (fn [x]
            (take %2 (cycle [x])))
       %1)))

(defcheck solution-f6d76343
  #(for [e % i (range %2)] e))

(defcheck solution-f6e941fe
  (fn rep
    [s n]
    (when-not (empty? s)
      (concat (repeat n (first s)) (rep (rest s) n)))))

(defcheck solution-f6f2f2ca
  (fn rep [l n]
    (mapcat #(repeat n %) l)))

(defcheck solution-f71acafd
  #(if
    (= %2 1)
     %1
     (apply interleave (repeat %2 %1))))

(defcheck solution-f732e08b
  (fn [coll n] (reduce concat (map #(take n (iterate identity %)) coll))))

(defcheck solution-f743ae4e
  #(mapcat repeat (repeat %2) %))

(defcheck solution-f8166721
  (fn [S n] (mapcat #(repeat n %) S)))

(defcheck solution-f8600e2a
  (fn my-rep [coll n]
    (mapcat #(repeat n %) coll)))

(defcheck solution-f9ef53d7
  (fn [s n]
    (reverse
      (reduce #(loop [r %1
                      n n]
                 (if (= n 0)
                   r
                   (recur (conj r %2) (- n 1))))
        '()
        s))))

(defcheck solution-fa154c17
  (fn [c n] (reduce (fn [a x] (concat a (take n (cycle (list x))))) [] c)))

(defcheck solution-fa1d3919
  (fn
    [to-replicate times]
    (if (= times 1)
      to-replicate
      (apply interleave (take times (repeat to-replicate))))))

(defcheck solution-fadbfd6d
  (fn [col r] (apply concat (map #(repeat r %) col))))

(defcheck solution-faec20a5
  (fn [xs n] (apply concat (for [x xs] (repeat n x)))))

(defcheck solution-fb0effc0
  #(reduce concat (for [x (range (count %1) ) ] (repeat %2 (nth %1 x) ) ) ))

(defcheck solution-fb54d5e1
  #(loop [n 0, r '()]
     (if (= (count %1) n)
       r
       (recur (inc n) (concat r (repeat %2 (nth %1 n)))))))

(defcheck solution-fbf04e48
  (fn [col n] (reduce #(concat %1 (repeat n %2)) [] col)))

(defcheck solution-fc440bab
  (fn [sq n]
    (let [aux
          (fn [[head & tail] n acc]
            (cond
              (nil? head)
              acc

              :else
              (recur tail n (apply conj acc (repeat n head)))))]
      (aux sq n []))))

(defcheck solution-fc4b51e2
  (fn [coll n]
    (reduce (fn [acc x]
              (concat acc (repeat n x)))
      []
      coll)))

(defcheck solution-fce05f5c
  (fn [s n]
    (loop [rr [] ss s l n]
      (if (empty? ss)
        rr
        (if (zero? l)
          (recur rr (rest ss) n)
          (recur (conj rr (first ss)) ss (dec l)))))))

(defcheck solution-fce511ab
  (fn rep_seq [L, n]
    (mapcat #(repeat n %) L)))

(defcheck solution-fd905f3d
  #(for [x %, i (range %2)] x))

(defcheck solution-fda26af3
  (fn [s t] (mapcat (partial repeat t) s)))

(defcheck solution-fdb94b9e
  #(mapcat (fn[c, n] (take n (repeat c))) % (repeat %2) ))

(defcheck solution-fe83adbc
  (fn [s n] (mapcat (fn [i] (repeatedly n #(identity i))) s)))

(defcheck solution-fea83196
  (fn f [x n] (if (empty? x) (list) (concat (repeat n (first x)) (f (rest x) n)  ) )))

(defcheck solution-ff16a87a
  #( let [temp (map (partial repeat %2) %1)] (apply concat temp) ))

(defcheck solution-ff4a9602
  (fn [s n] (reduce #(concat % (repeat n %2)) [] s)))

(defcheck solution-ff550e88
  #(if (= %2 1) % (apply interleave (repeat %2 %))))

(defcheck solution-ff5904e
  (fn
    [c n]
    (mapcat #(repeat n %) c)))

(defcheck solution-ff6c122a
  (fn [x y] (reduce concat (map #(take y (repeat %)) x))))

(defcheck solution-ffa4b52d
  (fn [s c]
    (mapcat #(repeat c %) s)))
