(ns coal-mine.problem-22
  (:require [coal-mine.checks :refer [defcheck-22] :rename {defcheck-22 defcheck}]
            [clojure.test]))

(defcheck solution-100ed206
  (fn [coll]
    (loop [my-coll coll cnt 0]
      (if (empty? my-coll)
        cnt
        (recur (rest my-coll) (+ cnt 1))))))

(defcheck solution-105e494b
  (fn foo [s] (if (empty? s) 0 (+ 1 (foo (rest s))))))

(defcheck solution-10fc8495
  (fn my-count [coll]
    (if (empty? coll) 0 (inc (my-count (rest coll))))
    ))

(defcheck solution-11aa2b65
  (fn [coll]
    (loop [xs coll sum 0]
      (if (= xs nil)
        sum
        (recur (next xs) (+ 1 sum))))))

(defcheck solution-11d48882
  #(loop [c %1, i 0]
     (if (empty? c)
       i
       (recur (rest c) (inc i)))))

(defcheck solution-120289f
  (fn c [[h & t]] (if t (inc (c t)) 1)))

(defcheck solution-131c9bb
  #(loop [lst %
          cnt 0]
     (if (empty? lst)
       cnt
       (recur (rest lst) (inc cnt)))))

(defcheck solution-1342b257
  (partial reduce (fn [c e] (inc c)) 0))

(defcheck solution-145c79ba
  (fn my-count [lista]
    (if (= (rest lista) [])
      1
      (inc (my-count (rest lista))))))

(defcheck solution-146f65ff
  (fn [x] (reduce (fn [v c] (+ v 1)) 0 x)))

(defcheck solution-14b78350
  (fn [se]
    (loop [s se c 0]
      (if (empty? s)
        c
        (recur (rest s)
          (inc c))))))

(defcheck solution-14e73489
  (fn cnt [s]
    (if (empty? s)
      0
      (inc (cnt (rest s))))))

(defcheck solution-14f711fd
  (fn outer [x]
    ((fn inner [x idx]
       (if (empty? (rest x)) (inc idx)
                             (inner (rest x) (inc idx)))) x 0)))

(defcheck solution-1501d2ac
  (fn [coll]
    (loop [x coll, n 0]
      (if-not (seq x)
        n
        (recur (rest x) (inc n))))))

(defcheck solution-15b65e74
  #(loop [seq %1 n 0]
     (if (empty? seq)
       n
       (recur (rest seq) (inc n)))))

(defcheck solution-15d0ff71
  (fn [sq]
    (loop [maybe-seq sq
           size 0]
      (if (empty? maybe-seq)
        size
        (recur (next maybe-seq) (inc size))))))

(defcheck solution-1691d93d
  #(loop [x %, y 0]
     (if  (empty? x)
       y
       (recur (rest x) (inc y)))))

(defcheck solution-16b6004d
  ;(fn [coll]
  ;  (loop [coll coll
  ;         cnt 0]
  ;    (if (empty? coll)
  ;      cnt
  ;      (recur (rest coll) (inc cnt)))))

  (fn [coll] (reduce (fn [acc _] (inc acc)) 0 coll)))

(defcheck solution-16c9dd08
  #(reduce + (map (fn [n] 1) %)))

(defcheck solution-17cac1f5
  (fn [col] (reduce + (map (fn [x] (if x 1 0)) col))))

(defcheck solution-18b5c4e2
  (fn length[seq] (if (empty? seq) 0 (+ 1 (length (rest seq))))))

(defcheck solution-18f0c2e5
  #(reduce (fn [n coll] (inc n)) 0 %))

(defcheck solution-1992b821
  #(reduce (fn [n x] (inc n)) 0 %1))

(defcheck solution-1a50aeac
  (fn [x]
    (loop [x x result 0]
      (if (empty? x) result
                     (recur (rest x) (+ result 1))))))

(defcheck solution-1aa79d1f
  (fn c [xs] (if (empty? xs) 0 (inc (c (rest xs))))))

(defcheck solution-1c1baae1
  (fn [col]
    (loop [n 0
           col col]
      (if (nil? (first col))
        n
        (recur (inc n) (rest col))))))

(defcheck solution-1c44787a
  (fn [coll] (apply + (map (fn [x] 1) coll))))

(defcheck solution-1c705992
  (fn [a-seq]
    (reduce +
      (map (constantly 1)
        a-seq))))

(defcheck solution-1c852cfe
  #( loop [copylist nil i 0 ]
     (if (= copylist (seq %1))
       i
       (recur (conj copylist (nth (reverse %1) i )) (inc i))
       )))

(defcheck solution-1c88d21c
  (fn [s]
    (let [aux (fn [acc s]
                (if (empty? s) acc (recur (inc acc) (rest s))))]
      (aux 0 s))))

(defcheck solution-1ca47f53
  (fn [coll]
    (loop [cnt 0
           coll coll]
      (if (empty? coll)
        cnt
        (recur (inc cnt) (rest coll))))))

(defcheck solution-1cb4da34
  (fn myCount [x] (if (seq x) (+ 1 (myCount (drop 1 x))) 0)))

(defcheck solution-1d0ec648
  (fn [values]
    (loop [values values result 0]
      (if (empty? values)
        result
        (recur (rest values) (inc result))))))

(defcheck solution-1d1c7892
  (fn
    [args]
    (reduce (fn [a, b] (+ a 1)) 0 args)
    ))

(defcheck solution-1d1e503
  #(apply + (vals (frequencies %))))

(defcheck solution-1defa332
  (fn my-count [coll] (if (empty? coll) 0 (+ 1 (my-count (rest coll))))))

(defcheck solution-1e44a19f
  (partial reduce
    (fn [c _] (+ c 1))
    0))

(defcheck solution-1eca7e42
  #(reduce (fn [n i] (inc n)) 0 %))

(defcheck solution-1f2a68b0
  (fn [s]
    (loop [idx 0]
      (if (= :not-found (nth s idx :not-found))
        idx
        (recur (inc idx))))))

(defcheck solution-1f800613
  (comp last butlast (partial interleave (iterate inc 1))))

(defcheck solution-1fb7cab6
  (fn [v] (reduce + (map (fn [_] (+ 1 0)) (vec v)))))

(defcheck solution-1fd2a7ea
  ; Return the count of elements in a sequence (can't use count)
  (fn f ([c]
         (          (
                      fn g ([d n]
                            (cond
                              (= d nil) n
                              (= (first d) nil) n
                              (= (next d) nil) (+ n 1)
                              :else (g (next d) (+ n 1))
                              )
                            )
                      ) c 0
          )
         )
    ))

(defcheck solution-20326706
  (fn mylast [a]
    (loop[y 1, t (rest a)]
      (if(empty? t)
        y
        (recur (inc y) (rest t))
        ))))

(defcheck solution-2039bfa5
  (fn [coll] (
              #(if
                (empty? %1)
                 %2
                 (recur (rest %1) (inc %2)))
              coll
              0
              )))

(defcheck solution-2045e65a
  #(loop [coll %
          acc 0]
     (if (empty? coll)
       acc
       (recur (rest coll) (inc acc))
       )
     ))

(defcheck solution-20724b36
  (fn [lst]
    (loop [l lst i 0]
      (if (empty? l)
        i
        (recur (rest l) (inc i))))))

(defcheck solution-20802a67
  (fn [coll] (alength (to-array coll))))

(defcheck solution-20987677
  (fn new-count [s]
    ((fn counter [s i]
       (if (empty? s)
         i
         (counter (rest s) (+ i 1)))) s 0)))

(defcheck solution-20ee7a8e
  (fn hey ([x] (hey x 1)) ([x y] (if (empty? (rest x)) y (hey (rest x) (+ y 1))))))

(defcheck solution-21746358
  #(reduce +(map (fn [x] 1) %)))

(defcheck solution-22c4cb9
  #(loop [c 0, s %]
     (if (seq s)
       (recur (inc c) (rest s))
       c)))

(defcheck solution-22f349b6
  (fn [s]
    (let [f (fn [l acc]
              (if (empty? l) acc
                             (recur (rest l) (inc acc))))] (f s 0))))

(defcheck solution-22ff384c
  (fn cnt[s] (if (next s)
               (+ 1 (cnt (next s)))
               1)))

(defcheck solution-23757d91
  (fn ! [s] (if (empty? s) 0 (+ 1 (! (rest s))))))

(defcheck solution-2391bf5e
  (fn [l] (reduce + 0 (map (fn [_] (inc 0)) l))))

(defcheck solution-239d76fb
  #(loop [cnt 0 coll %] (if (empty? coll) cnt (recur (inc cnt) (rest coll)))))

(defcheck solution-23b68c74
  #(count (seq %)))

(defcheck solution-23ded7f0
  (fn [x]
    (letfn [
            (mycount [ml] (if (empty? ml) 0 (+ 1 (mycount (rest ml)))))]
      (mycount x)
      )))

(defcheck solution-24d035de
  (fn [col0]
    (loop [col col0 size 0]
      (if (empty? col)
        size
        (recur (rest col) (+ size 1))
        ))))

(defcheck solution-252984
  (fn [x]
    ((fn self[x, c] (if (first x) (self (rest x) (+ c 1)) c)) x 0 )))

(defcheck solution-25c6c5a9
  (fn [lst]
    (reduce (fn [x y] (+ x 1)) 0 lst)))

(defcheck solution-25f9b862
  (fn [xs](loop [i 0 xs xs](if (seq xs) (recur (inc i) (rest xs)) i))))

(defcheck solution-263d47f1
  #(loop[arr % idx 0]
     (if(empty? arr)
       idx
       (recur (rest arr) (inc idx)))))

(defcheck solution-26896eea
  (fn [xs]
    (let [lp (fn [xs' n]
               (if (empty? xs')
                 n
                 (recur (rest xs') (inc n))))]
      (lp xs 0))))

(defcheck solution-26e1ac36
  (fn [x] (let [fin (fn [y c] (if (first y) (recur (rest y) (inc c)) c))]
            (fin x 0))))

(defcheck solution-2705d22c
  #(reduce (fn [x y] (inc x) ) 0 %))

(defcheck solution-2716bcc9
  #(reduce (fn [acc x] (+ acc 1)) 0 %))

(defcheck solution-275f9f7d
  (comp inc first reverse (partial map first) (partial map-indexed vector) seq))

(defcheck solution-276c0bcc
  (partial reduce (fn[n _] (+ 1 n)) 0))

(defcheck solution-293aa7dc
  (fn my-count[a-seq]
    ((fn count-cnt[t-seq cnt]
       (if (seq t-seq)
         (count-cnt (rest t-seq) (inc cnt))
         cnt)
       ) a-seq 0)))

(defcheck solution-29fcd1f6
  (fn my-count
    [s]
    (loop [ss s
           c 0]
      (if (empty? ss)
        c
        (recur (rest ss) (inc c))))))

(defcheck solution-2a0ab803
  #(reduce (fn [a b] (inc a)) 0 %))

(defcheck solution-2aef8588
  (fn [s]
    (loop [s- s i 1]
      (if (empty? (rest s-))
        i
        (recur (rest s-) (inc i))))))

(defcheck solution-2b3e00f
  (fn [l]
    (letfn [(helper [l sum]
              (if (seq l)
                (recur (rest l) (inc sum))
                sum))]
      (helper l 0))))

(defcheck solution-2b911ce7
  (fn [in-seq]
    (loop [county 0
           cur-seq in-seq]
      (if (empty? cur-seq)
        county
        (recur (inc county) (rest cur-seq))))))

(defcheck solution-2ba4b470
  #(apply + (map (fn [x] 1)  %)))

(defcheck solution-2bc32abd
  (fn [coll] (loop [cnt 0 rem (vec coll)] (if (empty? rem) cnt (recur (inc cnt) (pop rem))))))

(defcheck solution-2bd739a2
  (fn [s]
    (reduce (fn [total v] (inc total)) 0 s)))

(defcheck solution-2bf4aac9
  (fn [l] (loop [ll l i 0] (if (= ll ()) i (recur (rest ll) (inc i))))))

(defcheck solution-2c7e969
  (fn [lst] (reduce + (map (fn [x] 1) lst))))

(defcheck solution-2c816c28
  (partial reduce #(inc (first %&)) 0))

(defcheck solution-2ca7b1ec
  #(reduce (fn [a v] (inc a)) 0 %))

(defcheck solution-2d2c363a
  #(reduce (fn [counter _] (inc counter)) 0 %))

(defcheck solution-2d579de
  #(first (last (map vector (iterate inc 1) %))))

(defcheck solution-2e8031ee
  #((fn [lst sz] (if (empty? lst) sz (recur (rest lst) (inc sz)))) %1 0))

(defcheck solution-2eb2d6b
  (fn countseq [alist] (if (= (seq alist) nil) 0 (+ (countseq (rest alist)) 1))))

(defcheck solution-2ed2bfc
  (fn [coll]
    (loop [cnt 0 cl coll]
      (if (seq cl) (recur (inc cnt) (rest cl)) cnt))))

(defcheck solution-2ee86be7
  #(loop [lst %, n 0]
     (if (empty? lst) n
                      (recur (rest lst) (inc n)))))

(defcheck solution-2ef0e0f7
  #(reduce (fn [y,z] (inc y)) 0 %1))

(defcheck solution-2f0fc519
  (fn [coll]
    (loop [size 0 arr coll]
      (if (empty? arr)
        size
        (recur (inc size) (rest arr))))))

(defcheck solution-2f8761f7
  (fn mycount [x]
    (if (seq x)
      (+ (mycount (rest x)) 1)
      0)))

(defcheck solution-2fd634be
  (fn [coll]
    (let [total (fn [so-far _] (inc so-far))]
      (reduce total 0 coll))))

(defcheck solution-304cb86b
  (fn [s]
    (loop [s-rem s cnt 0]
      (if (seq s-rem)
        (recur (rest s-rem) (inc cnt))
        cnt))))

(defcheck solution-305cb533
  (fn llll[x] (if (empty? x) 0 (+ 1 (llll (rest x))))))

(defcheck solution-306e4513
  (fn count_ [colls]
    (loop [colls_ (seq colls) n_ 0]
      (if (seq? colls_)
        (recur (seq (drop 1 colls_)) (inc n_))
        n_))))

(defcheck solution-30d8b83e
  (fn [a]
    (loop [l a
           i 0]
      (if (empty? l)
        i
        (recur (rest l) (+ 1 i))
        ))))

(defcheck solution-3154523
  (fn [c]
    (reduce (fn [a b] (inc a)) 0 c)))

(defcheck solution-319ed67f
  #(reduce (fn[n _](+ 1 n)) 0 %))

(defcheck solution-31bd476c
  (fn [seq]
    (reduce (fn [n x] (+ 1 n)) 0 seq)))

(defcheck solution-31bea1c9
  #((fn my-count [s n] (if (= nil (first s))
                         n
                         (my-count (rest s) (+ 1 n)))) % 0))

(defcheck solution-3259ef47
  #(apply + (map (constantly 1) %)))

(defcheck solution-32f34b80
  (fn count2 [coll]
    (if (empty? coll)
      0
      (inc (count2 (rest coll))))))

(defcheck solution-33520061
  (fn my-count [xs]
    ((fn impl [xs n]
       (if (empty? xs)
         n
         (impl (rest xs) (+ n 1))))
     xs 0)))

(defcheck solution-336602c1
  (fn new-count
    [[x & xs]]
    (if (empty? xs)
      1
      (inc (new-count xs)))))

(defcheck solution-33a46062
  (fn [n] (count (seq n))))

(defcheck solution-33e2f9e6
  (fn [s] (apply + (map (constantly 1) s))))

(defcheck solution-344815ff
  (partial reduce
    (fn [acc i] (inc acc)) 0))

(defcheck solution-345ff41d
  (fn [coll] (reduce + (map (fn [x] 1) coll))))

(defcheck solution-34a73721
  (fn [x] (+ 1 (last (interleave x (range))))))

(defcheck solution-35186563
  #(reduce (fn [ct x] (inc ct)) 0 %))

(defcheck solution-351c0533
  (fn [coll]
    (let [f (fn [coll n]
              (if coll
                (recur (next coll) (inc n))
                n
                )
              )]
      (f coll 0)
      )
    ))

(defcheck solution-354385e6
  (fn [seq] (reduce + (map (fn [el] 1) seq))))

(defcheck solution-354dcfb6
  #(apply + ( map (constantly 1) %)))

(defcheck solution-358a68a
  (fn [s] (reduce + (map (fn [s] 1) s))))

(defcheck solution-35ada86e
  (fn cnt [l] (if (= l '()) 0 (inc (cnt (rest l))))))

(defcheck solution-35ec77a6
  (fn [coll]
    (loop [n 0 c coll]
      (if (empty? c)
        n
        (recur (+ 1 n) (rest c))))))

(defcheck solution-3605954
  (fn [xs]
    (loop [ys xs r 0]
      (if (empty? ys)
        r
        (recur (rest ys) (inc r))))))

(defcheck solution-362e066c
  #(reduce (fn [x y] (inc x)) 0 % ))

(defcheck solution-367e866e
  #(loop [n 0 s %] (if (empty? s) n (recur (inc n) (next s)))))

(defcheck solution-36ed3581
  #(apply + (map (constantly 1) (seq %))))

(defcheck solution-372f3804
  #(loop [wanted 0 source %1] (if (empty? source)
                                wanted (recur (inc wanted) (rest source)))))

(defcheck solution-3747c877
  (fn ct [coll]
    (loop [c coll n 0]
      (if (seq c)
        (recur (rest c) (inc n))
        n))))

(defcheck solution-377a00e
  #(alength(into-array %)))

(defcheck solution-37b8f63c
  (fn cnt [l]
    (loop [xs l n 0]
      (if (= xs '())
        n
        (recur (rest xs) (inc n))))))

(defcheck solution-38378576
  #(inc (last (sort (keys (zipmap (range) %))))))

(defcheck solution-39516660
  (fn [xs]
    (reduce (fn [n v] (inc n)) 0 xs)))

(defcheck solution-395e87e2
  (fn [aseq]
    (apply + (map #(let [% 1] %) aseq))))

(defcheck solution-39be9cdb
  (fn [data] (loop [i 0
                    c data]
               (if (empty? c)
                 i
                 (recur (inc i) (rest c))))))

(defcheck solution-39cf29e4
  (fn [coll]
    (loop [coll (seq coll) cnt 0]
      (if (empty? coll)
        cnt
        (recur (rest coll) (inc cnt))))))

(defcheck solution-3a5e9564
  (fn _count
    [c]
    (reduce (fn [a b] (inc a)) 0 c)))

(defcheck solution-3a778eef
  (partial
    (fn [n coll]
      (if (nil? (list* coll))
        n
        (recur (inc n) (rest coll))))
    0))

(defcheck solution-3af4c963
  (fn [s] (loop [c 0 s s] (if (empty? s) c (recur (inc c) (rest s))))))

(defcheck solution-3b0035a0
  (fn [x]
    (loop [x x i 0]
      (if (empty? x)
        i
        (recur (rest x) (inc i))))))

(defcheck solution-3b4713a0
  (fn [s]
    (letfn [(my-count [s tot]
              (if (empty? s)
                tot
                (recur (rest s) (inc tot))))]
      (my-count (seq s) 0))))

(defcheck solution-3bcd033d
  #(apply + (map (fn [x] 1) (seq %1))))

(defcheck solution-3c0cef1d
  #(let [v (vec %)]
     (loop [sv v n 0]
       (if (= (second sv) nil)
         (inc n)
         (recur (pop sv) (inc n))))))

(defcheck solution-3d577fe9
  (fn element-count[coll]
    (if (empty? coll)
      0
      (inc (element-count (rest coll))))))

(defcheck solution-3da1a101
  (fn [l] (reduce (fn [x y] (+ x 1)) 0 l)))

(defcheck solution-3dc5f976
  (fn [xs] (loop [n 0 xs xs] (if (nil? (first xs)) n (recur (inc n) (rest xs))))))

(defcheck solution-3ee5bb1e
  (fn [x]
    (#(if (empty? %1) %2 (recur (rest %1) (inc %2))) x 0)))

(defcheck solution-3f37df8b
  (fn my-count[coll](last (map #(inc %2) coll (range)))))

(defcheck solution-3fa19ec2
  (partial reduce (fn [x y] (inc x)) 0 ))

(defcheck solution-402aae46
  (fn cnt [l]
    (if (empty? l)
      0
      (+ 1 (cnt (rest l))))))

(defcheck solution-4037d357
  #(loop [sq %1 n 0] (if (empty? sq) n (recur (drop 1 sq) (inc n)))))

(defcheck solution-405a59f2
  (fn [z] (reduce (fn [x y] (+ x 1)) 0 z)))

(defcheck solution-411858eb
  (fn [coll] (reduce (fn [c ignore] (inc c)) 0 coll)))

(defcheck solution-412e0a62
  (fn [xs] (reduce (fn [p c] (inc p)) 0 xs)))

(defcheck solution-41a5ee16
  (fn mycount [seq]
    (if (empty? seq)
      0
      (inc (mycount (rest seq))))))

(defcheck solution-42a555a
  (fn [coll] (reduce + (map (constantly 1) coll))))

(defcheck solution-42f23a70
  (fn foo[xs] (+ 1 (if (empty? xs) -1 (foo (rest xs))))))

(defcheck solution-43126f4f
  (fn [input] (loop [s input l 0] (if (empty? s) l (recur (rest s) (inc l))))))

(defcheck solution-432c9118
  (fn [L] (apply + (map (fn [x] 1) L))))

(defcheck solution-43716a80
  #(loop [s % n 0]
     (if (empty? s) n
                    (recur (rest s) (inc n)))))

(defcheck solution-4376662c
  (fn [seq] (reduce + (map (constantly 1) seq))))

(defcheck solution-440222fa
  (partial (fn [c x] (if (empty? x) c (recur (inc c) (rest x)))) 0))

(defcheck solution-44485c10
  (fn [l]
    ((fn counted  [l n]
       (if (seq l) (counted (rest l) (+ 1 n)) n)) l 0)))

(defcheck solution-44e82e1d
  (fn [s]
    (loop [s s c 0]
      (if (first s)
        (recur (next s) (inc c))
        c))))

(defcheck solution-45e58cbf
  (fn [x]
    (loop [l x,c 0]
      (if (empty? l)
        c
        (recur (rest l) (inc c))))))

(defcheck solution-46246b70
  (fn a [s]
    (if (not (seq s))
      0
      (+ 1 (a (rest s)))
      )
    ))

(defcheck solution-46460e17
  #(loop [L %, result 0]
     (if (empty? L)
       result
       (recur (rest L) (+ result 1)))))

(defcheck solution-46989bdc
  (fn count-elements [coll]
    (reduce (fn [v &_] (inc v)) 0 coll)))

(defcheck solution-46e9fae4
  (fn [s] (reduce (fn [cnt _] (inc cnt)) 0 s)))

(defcheck solution-472915e4
  (fn num-elem
    ([l] (num-elem l 0))
    ([l cnt]
     (if (empty? l)
       cnt
       (recur (rest l) (inc cnt))))))

(defcheck solution-476c417b
  #(loop [ls % acc 0] (if (empty? ls) acc (recur (rest ls) (inc acc)))))

(defcheck solution-47d7112
  (fn f[l]
    (if (empty? l)
      0
      (+ 1 (f (rest l))))))

(defcheck solution-4871811a
  (fn [x]
    (reduce
      (fn [x y]
        (+ x 1)
        )
      0
      x
      )
    ))

(defcheck solution-48de9a50
  #(loop [x % c 0] (if (= x '()) c (recur (rest x) (inc c)))))

(defcheck solution-4996f54d
  (fn length [xs] (if (empty? xs) 0 (+ 1 (length (rest xs))))))

(defcheck solution-49b0c212
  #(loop [x 0 s %] (if (empty? s) x (recur (inc x) (rest s)))))

(defcheck solution-4a00e3e9
  (fn [s0]
    (loop [s s0
           c 0]
      (if (nil? s)
        c
        (recur (next s) (inc c))))))

(defcheck solution-4a0c9b2b
  (fn [l]
    (loop [r 0 t l]
      (if (empty? t)
        r
        (recur (inc r) (rest t))))))

(defcheck solution-4a64569
  (fn [l]
    (let [c (fn [s n] (if (empty? s) n (recur (rest s) (inc n))))] (c l 0))))

(defcheck solution-4a8b0b9d
  #(last (interleave % (iterate inc 1))))

(defcheck solution-4a8e7fe4
  (fn my-length [x]
    (let[go (fn [l x] (if (empty? l) x (recur (rest l) (inc x))))]
      (go x 0))
    ))

(defcheck solution-4adf8398
  (fn [coll]
    ((fn [coll n]
       (if (empty? coll)
         n
         (recur (rest coll) (inc n))))
     coll 0)))

(defcheck solution-4afe1e02
  (fn cnt [x] (count (map nil? x))))

(defcheck solution-4b5fff41
  (fn [s] (reduce (fn [a b] (inc a)) 0 (seq s))))

(defcheck solution-4b6cd669
  #(reduce (fn c [x y] (+ x 1)) 0 %))

(defcheck solution-4b8e52ec
  (fn counter [s] (if (empty? s) 0 (+ 1 (counter (rest s))))))

(defcheck solution-4b9adb20
  #(reduce (fn [a _] (inc a)) 0 %))

(defcheck solution-4c47ca4f
  (fn [l] (loop [tmp l c 0] (if (empty? tmp) c (recur (rest tmp)(inc c))))))

(defcheck solution-4cdcfc2c
  (fn l_ [l]
    (cond
      (empty? l) 0
      :else (+ 1 (l_ (rest l))))))

(defcheck solution-4d13db23
  (fn ! [coll] (if (empty? coll) 0 (inc (! (rest coll))))))

(defcheck solution-4d1717de
  (fn [l] (inc (first (last (map-indexed vector l))))))

(defcheck solution-4e09a574
  (fn [l] (loop [r l n 0] (if (empty? r) n (recur (rest r) (+ n 1))))))

(defcheck solution-4e45d83a
  (fn [lst]
    (reduce + (map (constantly 1) lst))))

(defcheck solution-4e49874d
  #(reduce + (map (constantly 1) %)))

(defcheck solution-4ea894b2
  (fn [a] ((fn [v n] (if (empty? v) n (recur (rest v) (+ n 1)))) a 0)))

(defcheck solution-4f07b097
  #(inc (first (last (map-indexed vector %)))))

(defcheck solution-4f1faedc
  #(reduce (fn [c _] (inc c)) 0 %))

(defcheck solution-4f897da1
  (partial reduce (fn[x y] (inc x)) 0))

(defcheck solution-5063584f
  (fn size [l]
    (loop [m l s 0]
      (if (empty? m) s (recur (rest m) (inc s)))
      )
    ))

(defcheck solution-506516c5
  (fn sol0022-func
    [coll]
    (reduce (fn [n _] (inc n)) 0 coll)))

(defcheck solution-5099203a
  (fn [xs]
    (loop [xs xs, c 0]
      (if (seq xs)
        (recur (rest xs) (inc c))
        c))))

(defcheck solution-50c1bba7
  (fn peu [x] (if (empty? x) 0 (inc (peu (rest x))))))

(defcheck solution-5157dd03
  (fn f [s]
    (if (empty? s)
      0
      (+ 1 (f (rest s))))))

(defcheck solution-51ba5686
  (fn [s] (loop [ cnt 0
                 ls  s ]
            (if-let [head (first ls)]
              (recur (inc cnt) (rest ls))
              cnt))))

(defcheck solution-528df0ea
  (fn [coll]
    (loop [c coll, i 0]
      (if (seq c)
        (recur (next c) (inc i))
        i))))

(defcheck solution-5396ed87
  (fn my-count
    ([x] (my-count 0 x))
    ([cnt x] (if (empty? x) cnt (recur (inc cnt) (rest x))))
    ))

(defcheck solution-53cabc27
  (fn [col]
    (reduce + (map (fn [x] 1) col))))

(defcheck solution-541b1fbf
  (fn [x]
    ((fn [x acc]
       (if (nil? x) acc
                    (recur (next x) (inc acc)))) x 0)))

(defcheck solution-5476dcb4
  (fn [s]
    (reduce + (map (constantly 1) s))))

(defcheck solution-5479c6db
  (fn mycount [v]
    (if (= (rest v) '() )
      1
      (inc (mycount (rest v)))
      )
    ))

(defcheck solution-548d63ad
  #(reduce (fn [acc _] (inc acc))  0 %))

(defcheck solution-54982c8b
  (partial reduce (fn [m n] (inc m)) 0))

(defcheck solution-54c0b12
  (fn [c]
    ((fn [i c]
       (cond
         (empty? c) i
         :default (recur (+ 1 i) (rest c))
         )
       ) 0 c)))

(defcheck solution-5534a596
  (fn [coll]
    (letfn [(mycount [coll cnt]
              (if (empty? coll)
                cnt
                (recur (rest coll) (inc cnt))))]
      (mycount coll 0))))

(defcheck solution-5546d343
  (fn [l]
    (loop [l l n 0]
      (if (empty? l)
        n
        (recur (rest l) (inc n))))))

(defcheck solution-558ad58d
  #(loop [item % result 0] (if(empty? item) result (recur (rest item) (inc result)))))

(defcheck solution-55d736d8
  #(-> % vec count))

(defcheck solution-55e9661f
  (fn [x] (loop [x x, i 0] (if (seq x) (recur (next x) (inc i)) i))))

(defcheck solution-56b17ce0
  (fn [x] (apply + (map (fn [y] 1) x))))

(defcheck solution-56bf1b06
  (fn count* [xs]
    (loop [n 0 xs xs]
      (if (or (string? xs) (coll? xs)) (recur (inc n) (next xs)) n))))

(defcheck solution-56c412a6
  (fn mycount [list]
    (if (= '() list) 0 (+ 1 (mycount (rest list))))))

(defcheck solution-57bb0c44
  (fn [x] (reduce + (map (constantly 1) x)) ))

(defcheck solution-57caf959
  (partial (fn [i s] (if (empty? s) i (recur (+ i 1) (rest s)))) 0))

(defcheck solution-57d59ab4
  #(loop[l %, r 0]
     (if (nil? l)
       r
       (recur (next l) (inc r)))))

(defcheck solution-57f0b2d4
  (fn
    [list]
    (loop [li list
           n 0]
      #_(println li)
      (if (= (nth li 0 nil) nil)
        n
        (recur (rest li) (inc n))))))

(defcheck solution-57f81d8
  (partial reduce (fn [counter _]
                    (inc counter))
    0))

(defcheck solution-58aa03c0
  (fn [s]
    (loop [l s c 0]
      (if (empty? l) c
                     (recur (rest l) (inc c))))))

(defcheck solution-594d7592
  (fn count-me [x] (alength (into-array x))))

(defcheck solution-59e6e0cc
  #(reduce (fn [x y] (+ x 1)) 0 %))

(defcheck solution-5a1946b0
  (fn[a-seq]
    (loop [c 0 ss a-seq]
      (if (empty? ss)
        c
        (recur (inc c) (rest ss))))))

(defcheck solution-5a356056
  (fn [xs] (reduce (fn [n _] (+ n 1)) 0 xs)))

(defcheck solution-5a58947d
  (fn cnt [x] (if (nil? (first x)) 0 (+ 1 (cnt (rest x))))))

(defcheck solution-5a5d9abf
  #(loop [sqs %, n 1]
     (let [rst (rest sqs)]
       (if (= rst '())
         n
         (recur rst (+ 1 n))
         )
       )
     ))

(defcheck solution-5a633c60
  (fn [seq] (reduce + 0 (map #(if true 1 %) seq))))

(defcheck solution-5ae775fb
  (fn this [l] (if (nil? (first l)) 0 (inc (this (rest l))))))

(defcheck solution-5af26e0c
  (fn get-length [[h & t]]
    (if (empty? t)
      1
      (+ 1 (get-length t)))))

(defcheck solution-5b0fcd42
  (fn t ([x]
         (t x 0))
    ([x r]
     (if (= [] x)
       r
       (recur (rest x) (inc r))))))

(defcheck solution-5b2538ad
  (fn [coll] (reduce (fn [c & _] (inc c)) 0 coll)))

(defcheck solution-5b92f43c
  (fn [x]
    ((fn [x, i]
       (if (empty? x) i (recur (rest x) (+ i 1)))) x 0)))

(defcheck solution-5bc4d247
  #(reduce (fn [c x] (inc c)) 0 %))

(defcheck solution-5be92fb9
  #(-> (map-indexed list %) last first inc))

(defcheck solution-5c1ad491
  (fn [coll]
    (loop [coll coll, n 0]
      (if coll
        (recur (next coll) (inc n))
        n))))

(defcheck solution-5c1c46ba
  (fn [coll] (reduce (fn [acc _] (inc acc)) 0 coll)))

(defcheck solution-5c4f1d97
  (fn ccount [x] (if (empty? x) 0 (+ (ccount (rest x)) 1))))

(defcheck solution-5d6ebd24
  #(reduce (fn [acc _] (+ acc 1)) 0 %))

(defcheck solution-5e4d05e3
  #(reduce (fn [x y] (inc x)) 0 %))

(defcheck solution-5f6455d3
  (fn [xs] (reduce (fn [acc _] (inc acc)) 0 xs)))

(defcheck solution-5f6ce871
  (fn [c] (apply + (map (fn [x] 1) c))))

(defcheck solution-5f9ec4d
  (fn [xs] (reduce #(+ % (or 1 %2)) 0 xs)))

(defcheck solution-5fe42555
  (fn  my-count [sequence]
    (loop [s (seq sequence), c 0]
      (cond (empty? s) c
            :else (recur (rest s) (inc c))))))

(defcheck solution-5ffba5ea
  (fn [in]
    (reduce (fn [c _] (inc c)) 0  in)))

(defcheck solution-60535e27
  (fn len [xs] (case xs
                 [] 0
                 (+ 1 (len (rest xs))))))

(defcheck solution-6061b520
  (fn [arg]
    (loop [s arg n 0]
      (if (empty? s)
        n
        (recur (rest s) (inc n))))))

(defcheck solution-60a2d354
  (fn [coll]
    (loop [coll2 coll, result 0]
      (if (empty? coll2)
        result
        (recur (rest coll2) (inc result))))))

(defcheck solution-61c4db38
  (fn rec [l]
    (let [n (rest l)]
      (if (empty? n) 1 (+ 1 (rec n))))))

(defcheck solution-62825f22
  (fn [col]
    (loop [
           se col
           res 0
           ]
      (if (empty? se)
        res
        (recur (rest se) (inc res)))
      )))

(defcheck solution-6286e0d0
  (fn
    [aseq]
    (reduce (fn [n1 n2] (inc n1)) 0 aseq)))

(defcheck solution-6292bf3a
  #(loop [elements % accum 0]
     (if (empty? elements) accum
                           (recur (rest elements) (inc accum)))))

(defcheck solution-62c2bd43
  (fn cnt [s] (if (empty? s) 0 (inc (cnt (rest s))))))

(defcheck solution-6360aa13
  (fn [lst]
    (loop [lst lst n 0]
      (if (empty? lst)
        n
        (recur (rest lst) (inc n))))))

(defcheck solution-636b8c3a
  (partial reduce (comp inc first list) 0))

(defcheck solution-637dafb0
  (fn
    [list]
    (loop [i 0 list list]
      (if (empty? list)
        i
        (recur (inc i) (rest list))
        )
      )
    ))

(defcheck solution-63a555a3
  #(loop [col % acc 0]
     (if (empty? col)
       acc
       (recur (rest col) (inc acc)))))

(defcheck solution-63b9c6ae
  (fn [s]
    (loop [s s
           c 0]
      (if (seq s)
        (recur (rest s) (inc c))
        c))))

(defcheck solution-63fe4eb
  #(loop [x % i 0]
     (if (empty? x)
       i (recur (rest x) (inc i)))))

(defcheck solution-640f1976
  #(-> % (interleave (range)) last inc))

(defcheck solution-647ca440
  #(apply + (map (fn [x] 1) %)))

(defcheck solution-64c5d160
  (fn [s]
    (loop [se s c 0]
      (if (empty?  se)
        c
        (recur (rest se) (inc c))
        ))))

(defcheck solution-64da0b7a
  (fn count' [xs] (if (empty? xs) 0 (inc (count' (next xs))))))

(defcheck solution-64fc51b
  (fn mycount
    [myseq]
    (loop
     [acc 0 myrest myseq]
      (if (empty? myrest)
        acc
        (recur (inc acc) (rest myrest))))))

(defcheck solution-652e1827
  (fn [x]
    (loop [ct 0 thing x]
      (if (empty? thing)
        ct
        (recur (inc ct) (rest thing))))))

(defcheck solution-65529e92
  #(reduce +(map (fn [x] 1) % )))

(defcheck solution-658b75f6
  (fn[s]
    (reduce (fn [t x] (inc t)) 0 s)))

(defcheck solution-65cdd145
  (fn [elems]
    (loop [n 0
           remain elems]
      (if (empty? remain)
        n
        (recur (inc n) (rest remain))))))

(defcheck solution-669086e2
  (fn [x]
    (loop [x x n 0]
      (if (empty? x)
        n
        (recur (rest x) (inc n))))))

(defcheck solution-670c6abd
  (fn count-coll [coll]
    (reduce (fn [c e] (inc c)) 0 coll)))

(defcheck solution-6768564b
  (fn my-count ([x] (my-count x 1)) ([x y] (if (= (rest x) []) y (my-count (rest x) (inc y))))))

(defcheck solution-679ee0fe
  (fn [l]
    (loop [c 0
           [_ & args :as mylist] l]
      (if (empty? mylist)
        c
        (recur (inc c) args)))))

(defcheck solution-67c093db
  (fn
    [elems]
    (loop [elems elems iterator 0]
      (if (not= (first elems) nil)
        (do
          (recur (rest elems) (inc iterator)))
        iterator
        ))))

(defcheck solution-68965ba4
  (fn my-count [[x & xs]]
    (if x
      (inc (my-count xs))
      0)))

(defcheck solution-699581bd
  (fn c[seqq] (reduce + (map #(and % 1) seqq))))

(defcheck solution-699f3e16
  (fn [x]
    (loop [n 0 c x]
      (if (empty? c) n
                     (recur (inc n) (rest c) )
                     )
      )
    ))

(defcheck solution-69a35755
  #(
    (fn counter [l, n]
      (if
       (= () l)
        n
        (counter (rest l) (+ n 1))))
    %1
    0
    ))

(defcheck solution-6a0c89df
  (fn [xs]
    (reduce + 0 (map (fn [_] 1) xs))))

(defcheck solution-6a476544
  (partial reduce (fn [a _] (+ 1 a)) 0))

(defcheck solution-6a91f757
  (fn total-number [x]
    (if (= (first x) nil)
      0
      (+ 1 (total-number (rest x)))
      )
    ))

(defcheck solution-6bae2bfe
  (fn [coll] (apply + (map #(inc (compare %1 %2)) coll coll))))

(defcheck solution-6c40f354
  (fn [coll]
    (loop [n 0
           [x & xs] coll]
      (if (nil? x)
        n
        (recur (inc n)
          xs)))))

(defcheck solution-6d42d0f8
  (fn conta [s]
    (if (nil? (next s))
      1
      (+ 1 (conta (next s))))))

(defcheck solution-6dfd915d
  (fn new-count [coll]
    (if (empty? coll) 0 (inc (new-count (rest coll))))))

(defcheck solution-6e33978b
  (fn [seq]
    (loop [cnt 0 remain seq]
      (if (empty? (rest remain))
        (inc cnt)
        (recur (inc cnt) (rest remain))))))

(defcheck solution-6eab0031
  #(reduce (fn [n _] (inc n)) 0 %))

(defcheck solution-6ec2ef58
  (fn [x] (loop [s x ile 0] (if (empty? s) (+ 0 ile) (recur (rest s) (inc ile))))))

(defcheck solution-6ee958ba
  #(count (vec %1)))

(defcheck solution-6f13f720
  (partial reduce (fn [c x] (inc c)) 0))

(defcheck solution-6f263391
  (comp (partial reduce +) (partial map #(if % 1))))

(defcheck solution-6fd898e1
  (fn[s](reduce + (map (fn[x](+ 1)) s))))

(defcheck solution-70904044
  ;#(loop [coll % tot 0] (if (nil? (seq coll)) tot (recur (next coll) (+ tot 1))))

  #(reduce (fn [n _] (inc n)) 0 %))

(defcheck solution-709d8d70
  (partial (fn c [n sq]
             (if (empty? sq)
               n
               (c (inc n) (rest sq))))
    0))

(defcheck solution-70ad8927
  #(loop [c %1
          n 0]
     (if (empty? c)
       n
       (recur (rest c) (inc n)))))

(defcheck solution-70d91f95
  (fn [xs] (reduce (fn [n i] (inc n)) 0 (seq xs) )))

(defcheck solution-712e025a
  #(reduce (fn [acc n] (inc acc)) 0 %))

(defcheck solution-71b03bea
  #(letfn [(do-count [ls c] (if (empty? ls) c (recur (rest ls) (+ 1 c))))] (do-count %1 0)))

(defcheck solution-71f0b18d
  (fn [coll]
    (loop [coll coll cnt 0]
      (if (empty? coll)
        cnt
        (recur (rest coll) (inc cnt))
        )

      )

    ))

(defcheck solution-721313d3
  #(reduce (fn [s n] (inc s)) 0 %))

(defcheck solution-731a61
  (fn [s]
    ((fn [s c]
       (if (empty? s)
         c
         (recur (rest s) (inc c)))) s 0)))

(defcheck solution-736db2f9
  (fn [col]
    (loop [col col n 0]
      (if (empty? col)
        n
        (recur (rest col) (inc n))))))

(defcheck solution-73a2335e
  (fn [lst] (reduce + (map (constantly 1) lst))))

(defcheck solution-744667a0
  (fn my_count
    ([l] (my_count l 0))
    ([l n] (if l (my_count (next l) (inc n)) n))))

(defcheck solution-74e0bf59
  (fn counter [sq]
    (apply + (map (fn [x] 1) sq))))

(defcheck solution-74e1850e
  (fn [l]
    (loop [n 0 l l]
      (if (empty? l) n
                     (recur (inc n) (rest l))))))

(defcheck solution-75e29eaf
  (fn [x]
    (loop [s x, i 0]
      (if (empty? s)
        i
        (recur (rest s) (inc i))))))

(defcheck solution-7696d585
  (fn [coll]
    (reduce (fn [n x] (inc n)) 0 coll)))

(defcheck solution-777960ec
  #(+ 1 (.lastIndexOf (vec %) (last  (vec %)))))

(defcheck solution-77fba302
  (fn [x]
    (
     (fn [x n]
       (if-let [r (next x)]
         (recur r (+ 1 n))
         (+ 1 n)
         )
       ) x 0)))

(defcheck solution-780a830f
  (fn length [ls]
    (if (empty? ls)
      0
      (+ 1 (length (rest ls))))))

(defcheck solution-789f60bf
  (fn my-c [seq]
    (if (= seq [])
      0
      (inc (my-c (rest seq))))))

(defcheck solution-79e8362e
  #(reduce + 0 (map (fn [x] 1) %)))

(defcheck solution-7b56c912
  (fn [l] (apply + (map (fn [x] 1) l))))

(defcheck solution-7b81119f
  (fn [lst]
    (loop [len 0 x lst]
      (if (empty? x)
        len
        (recur (+ 1 len) (rest x))))))

(defcheck solution-7b91d9f5
  #(reduce (fn [m i] (inc m)) 0 %))

(defcheck solution-7bf92d12
  #(reduce (fn [x _] (inc x)) 0 %))

(defcheck solution-7c26a7c3
  #(reduce (fn [coll _]
             (inc coll)) 0 %))

(defcheck solution-7d02b530
  (partial reduce (fn [a _] (inc a)) 0))

(defcheck solution-7d996ddf
  #(
    (fn rec [l i]
      (if
       (= (first l) nil)
        i
        (rec (rest l) (+ i 1))
        )
      )
    % 0
    ))

(defcheck solution-7e2ce606
  (fn cnt
    ([x] (cnt x 0))
    ([x acc]
     (if (empty? x)
       acc
       (recur (rest x) (inc acc))))))

(defcheck solution-7f0fdad2
  (fn size [seq] (if (= seq '()) 0 (+ 1 (size (rest seq))))))

(defcheck solution-7f97d1c
  #(reduce (fn [acc v] (inc acc)) 0 %))

(defcheck solution-80921c8
  #(-> (interleave % (range)) last inc))

(defcheck solution-8155015f
  (fn len [s]
    (if (= s '())
      0
      (inc (len (rest s))))))

(defcheck solution-815eb2c1
  #(reduce + (map (fn [x] 1) %1) ))

(defcheck solution-817d642
  #(->> % (reduce (fn [cnt coll] (inc cnt)) 0)))

(defcheck solution-81d578a8
  #(loop [lst %1 acc 0]
     (if (empty? lst)
       acc
       (recur (rest lst) (+ 1 acc)))))

(defcheck solution-82708748
  (fn [xs] ((fn calc[acc os] (if (empty? os) acc (calc (+ acc 1) (rest os)))) 0 xs)))

(defcheck solution-82a45dea
  (fn [c]
    (loop [[f & r] c n 0]
      (if-not f
        n
        (if (empty? r)
          (inc n)
          (recur r (inc n)))))))

(defcheck solution-830ebc13
  (fn f
    ([s]
     (f s 0))
    ([s r]
     (if (empty? s)
       r
       (recur (rest s) (inc r))))))

(defcheck solution-835a2472
  (letfn [(len [n xs] (if (empty? xs) n (recur (inc n) (rest xs))))] (partial len 0)))

(defcheck solution-83796caf
  (fn [ls]
    (loop [l ls c 0]
      (if (seq l)
        (recur (rest l) (inc c))
        c))))

(defcheck solution-83dc033c
  #(loop[n 0
         lst %]
     (if (= nil (first lst))
       n
       (recur (inc n) (next lst)))))

(defcheck solution-83ecb49e
  #(loop [col % n 0] (if (empty? col) n (recur (rest col) (inc n)))))

(defcheck solution-83f0272b
  #(loop [col % nbr 1]
     (if (empty? (rest col))
       nbr
       (recur (rest col) (inc nbr)))))

(defcheck solution-84024dfc
  (fn newcount [x]
    (if (seq x)
      (inc (newcount (rest x)))
      0)))

(defcheck solution-84392390
  (fn c [s] (if (empty? s) 0 (inc (c (rest s))))))

(defcheck solution-8494bb49
  #(loop [a 0 s %] (if (empty? s) a (recur (inc a) (rest s)))))

(defcheck solution-8501a3f1
  (fn countt
    ([l] (countt l 0))
    ([l n] (if (= l [])
             n
             (countt (rest l) (inc n))))))

(defcheck solution-854442ba
  #(reduce (fn [acc elt] (+ 1 acc)) 0 %))

(defcheck solution-863744ee
  (fn annon [xs] (loop [inner-xs xs counter 0] (if-not inner-xs counter (recur (next inner-xs) (inc counter))))))

(defcheck solution-86431e9f
  ;(#(reduce (fn [a b](+ 1 b)) 0 %) "Hello World")
  #(reduce (fn [a b](inc a)) 0 (seq %)))

(defcheck solution-86607881
  (fn [coll]
    (let [f (fn [coll acc]
              (if (empty? coll) acc (recur (rest coll) (inc acc))))]
      (f coll 0))))

(defcheck solution-8782e59c
  (fn [lat]
    (reduce (fn [x y] (inc x)) 0 lat)))

(defcheck solution-880e6fd2
  (fn [sq]
    (loop [x sq res 0]
      (if (empty? x)
        res
        (recur (rest x) (+ 1 res))))))

(defcheck solution-882842ee
  #(alength (to-array %)))

(defcheck solution-891adc22
  #(reduce (fn[x y](+ x 1)) 0  %))

(defcheck solution-8945ae5d
  (fn [coll]
    (loop [c coll
           cnt 0]
      (if (seq c)
        (recur (rest c) (+ cnt 1))
        cnt))))

(defcheck solution-8970e1df
  (fn cnt [x]
    (if (empty? x)
      0
      (+ 1 (cnt (rest x))))))

(defcheck solution-89711733
  (fn my-count [seq]
    (loop [s seq c 0]
      (if (nil? s)
        c
        (recur (next s) (inc c))))))

(defcheck solution-8a4d542c
  (fn [xs]
    (loop [n 0
           temp xs]
      (if (nil? temp)
        n
        (recur (inc n) (next temp))))))

(defcheck solution-8a5ea0fa
  (fn c [s] (if (empty? s) 0 (+ 1 (c (rest s))))))

(defcheck solution-8a6967df
  #(loop [s % n 0] (if (seq s) (recur (rest s) (inc n)) n)))

(defcheck solution-8b49e8e4
  (fn my-count [coll]
    (reduce (fn [acc x]
              (inc acc))
      0 coll)))

(defcheck solution-8bfd3981
  (fn [lat]
    (reduce (fn [x y] (+ x 1)) 0 lat)))

(defcheck solution-8bfef4fb
  (fn [coll] (reduce (fn [c e] (inc c)) 0 coll)))

(defcheck solution-8ca057f7
  #(loop[x %1 l 0](if(=[]x)l(recur(rest x)(+ 1 l)))))

(defcheck solution-8cc6120a
  (fn cnt [x] (if (empty? x) 0 (+ 1 (cnt (rest x))))))

(defcheck solution-8d436a34
  #(reduce (fn [total _] (inc total)) 0 %))

(defcheck solution-8d6f9438
  (fn [coll] (reduce (fn [total el] (+ total 1)) 0 coll)))

(defcheck solution-8dc076a7
  #(loop [c 0 xs %]
     (if (empty? xs) c (recur (inc c) (rest xs)))))

(defcheck solution-8dca2a23
  (fn [seq] (reduce + (map (fn [x] 1) seq))))

(defcheck solution-8eb6a1e6
  #(reduce +
     (map (fn [_] 1)
       %)))

(defcheck solution-8ebdf151
  (fn foo [sequ]
    (reduce
      (fn [memo elem] (inc memo))
      0
      sequ)
    ))

(defcheck solution-8f12ec98
  (fn [a] (loop [ c 0 x a] (if (empty? x) c (recur (inc c) (rest x))))))

(defcheck solution-8fbbdb7c
  #(reduce (fn [acc ls] (inc acc)) 0 %))

(defcheck solution-90710634
  (partial reduce (fn [x y] (inc x)) 0))

(defcheck solution-907f03b0
  #(second (last (map vector % (next (range))))))

(defcheck solution-90a86a45
  #((partial apply +) (map (fn [_] 1) %)))

(defcheck solution-90adfe14
  #(reduce (fn [acc elt] (inc acc)) 0 %))

(defcheck solution-9110fda6
  #(loop [n 0 l %] (if (= l ()) n (recur (inc n) (rest l)))))

(defcheck solution-913173eb
  #(loop [c 0 s %] (if (empty? s) c (recur (+ c 1) (rest s)))))

(defcheck solution-919aa84e
  (fn [s]
    (loop [n 0, ss s]
      (if ss (recur (inc n) (next ss)) n))))

(defcheck solution-91a2e57
  (fn mycnt [seq]
    (let [inner (fn m2 [seq i]
                  (if (= seq []) i (m2 (rest seq) (+ 1 i))))]
      (inner seq 0))))

(defcheck solution-91e8a9d5
  (fn [sq]
    ((fn [sq c]
       (if (empty? sq)
         c
         (recur (rest sq) (inc c)))) sq 0)))

(defcheck solution-928992e6
  (fn [s]
    (reduce (fn [x _] (inc x)) 0 s)))

(defcheck solution-929c83d8
  (letfn [(cownt* [coll]
            (if (seq coll)
              (inc (cownt* (rest coll)))
              0))]
    cownt*))

(defcheck solution-92d5d000
  #(reduce + 0 (map (constantly 1) %)))

(defcheck solution-934ee660
  (fn [seq]
    (loop [cur seq
           res 0]
      (if (empty? cur )
        res
        (recur (rest cur) (inc res))))))

(defcheck solution-93be9ade
  (fn [col]
    (reduce
      (fn[n _](inc n))
      0 col)
    ))

(defcheck solution-94892905
  (fn ! [x] (if (seq x) (+ 1 (! (rest x))) 0)))

(defcheck solution-94abc5cb
  (fn my-count [x] (if (nil? x) 0 (inc (my-count (next x))))))

(defcheck solution-94d91c8d
  (fn [lst]
    (->> lst (map (constantly 1)) (reduce +))))

(defcheck solution-94f830ab
  (fn [arr] (reduce + (map (fn [e] 1) arr))))

(defcheck solution-95288e5
  #(reduce (fn ([] 0) ([a b] (inc a))) 0 %))

(defcheck solution-9550a0e9
  (fn f ([x] (f x 0))
    ([x i] (if (empty? x) i (f (rest x) (inc i))))))

(defcheck solution-95a1d0cc
  (let [count2 (fn [x i] (if (empty? x) i (recur (rest x) (inc i))))] #(count2 % 0)))

(defcheck solution-9673fea6
  (fn [xs] (apply + (map (fn [x] 1) xs))))

(defcheck solution-972ea1ef
  (fn my-count [s]
    (if (= () s) 0
                 (+ 1 (my-count (rest s))))))

(defcheck solution-975596f7
  #(loop [[h & t] %
          x 0]
     (if h
       (recur t (inc x))
       x)))

(defcheck solution-9780326e
  (fn cnt
    [arr]
    (loop [[x & xs] arr c 0]
      (if (= x nil)
        c
        (recur xs (inc c))))))

(defcheck solution-97cf7b58
  (fn [l]
    (loop [li l n 0]
      (if (= (rest li) [])
        (inc n)
        (recur (rest li) (inc n))))))

(defcheck solution-97fe59b5
  (fn myfunc [col] (if (next col) (+ 1 (myfunc (rest col))) 1)))

(defcheck solution-98752949
  (fn [s]
    (reduce (fn [counter _]
              (inc counter))
      0
      s)))

(defcheck solution-990c7d93
  (fn [s]
    (reduce (fn [a b] (+ 1 a)) 0 s)))

(defcheck solution-99c0b3b3
  (fn len [sq]
    (if (empty? sq)
      0
      (inc (len (rest sq))))))

(defcheck solution-9a2a962b
  (fn foo
    ([s] (foo s 0))
    ([s n] (if (empty? s) n (recur (rest s) (inc n))))))

(defcheck solution-9af44287
  (fn f [l] (if (empty? l) 0 (+ 1 (f (rest l))))))

(defcheck solution-9b0d2df5
  #(
     reduce
     +
     (map (fn [_] 1) (seq %)
       )
     ))

(defcheck solution-9bb3db01
  (fn [s]
    (letfn [(fib
              [f r cnt]
              (if (nil? f)
                cnt
                (recur (first r) (rest r) (inc cnt))
                ))]
      (fib (first s) (rest s) 0)
      )))

(defcheck solution-9c545e8d
  (fn [l]
    ((fn reccount [L ct]
       (if (seq L)
         (reccount (rest L) (inc ct))
         ct)) l 0)))

(defcheck solution-9c943443
  #(apply +(map(fn[_]1)%)))

(defcheck solution-9c99758a
  (fn [s]
    (loop [i 0 tail s]
      (if (empty? tail)
        i
        (recur (inc i) (rest tail))))
    ))

(defcheck solution-9cbc5a17
  (fn [S] (loop [xs S n 0] (if (empty? xs) n (recur (rest xs) (inc n))))))

(defcheck solution-9ccd3e92
  (fn cnt
    ([l]
     (cnt l 0))
    ([l n]
     (if (nil? (first l))
       n
       (cnt (rest l) (inc n))))))

(defcheck solution-9d175571
  (partial (fn cnt [n l] (if (empty? l) n (cnt (inc n) (rest l)))) 0))

(defcheck solution-9d469927
  #(reduce (fn [i _] (inc i)) 0 %))

(defcheck solution-9db8a11c
  (fn [l] (reduce (fn [a i] (inc a)) 0 l)))

(defcheck solution-9dd6ceb9
  (fn [xs]
    (reduce (fn [x y] (inc x)) 0 xs)))

(defcheck solution-9dec9d12
  (comp (partial apply +) (partial map #(do % 1))))

(defcheck solution-9e51c8ec
  ;;(fn [x] (reduce (fn [acc el] (inc acc)) 0 x))

  ;(fn f ([x] (f x 0))
  ;      ([x c] (if (empty? x) c (recur (rest x) (inc c)))))

  (fn f [x]
    (loop [counter 0 y x]
      (if (empty? y)
        counter
        (recur  (inc counter) (rest y))))))

(defcheck solution-9e70da6f
  (fn [seq] (loop [s seq
                   pos 0]
              (if (not (empty? s))
                (recur (rest s) (+ pos 1))
                pos))))

(defcheck solution-9ebf123b
  #(reduce + (map (fn [x] 1) %1)))

(defcheck solution-9eec135f
  (fn [s] (reduce + (map (fn [_] 1) s))))

(defcheck solution-9f6661fa
  (fn [sequence]
    (reduce + (map (constantly 1) sequence))))

(defcheck solution-9fa8e27
  (fn my-count [xs]
    (if (empty? xs)
      0
      (+ 1 (my-count (rest xs))))))

(defcheck solution-9fde67b2
  #(loop [s % n 0]
     (if (empty? s)
       n
       (recur (rest s) (inc n))
       )
     ))

(defcheck solution-a0c1b0cc
  #(reduce + (map (constantly 1 ) %)))

(defcheck solution-a1545d7f
  #(reduce + 0 (map (fn [_] 1) %)))

(defcheck solution-a1b27438
  #(loop [l % c 0]
     (if (empty? l)
       c
       (recur (rest l) (inc c)))))

(defcheck solution-a2012182
  #(loop [lst %
          acc 0]
     (if (empty? lst) acc
                      (recur (rest lst) (+ acc 1)))))

(defcheck solution-a2a5fe30
  (fn num-elem [xs]
    (if (empty? xs)
      0
      (inc (num-elem (rest xs))))))

(defcheck solution-a2e56915
  (fn c [s]
    (if (empty? s) 0
                   (+ 1 (c (rest s))))))

(defcheck solution-a2f787cc
  (fn [s] (reduce (fn [a b] (inc a)) 0 s)))

(defcheck solution-a3595100
  #(reduce (fn ([x y] (inc x))) 0 %1))

(defcheck solution-a35b62f1
  (fn my-count [xs]
    (cond
      (= '() xs) 0
      :else (+ 1 (my-count (rest xs)))
      )
    ))

(defcheck solution-a464a88b
  (fn [s]
    ((fn [s n]
       (if (next s)
         (recur (rest s) (inc n))
         (inc n))) s 0)))

(defcheck solution-a4b7d14a
  #((fn beh [c s] (if (first s) (beh (inc c) (rest s)) c)) 0 %1))

(defcheck solution-a5a5ddea
  (fn
    [coll]
    (let [f (fn [s n]
              (if (seq s)
                (recur (next s) (inc n))
                n))]
      (f coll 0))))

(defcheck solution-a61b1190
  (fn f [x] (if (empty? x) 0 (inc (f (rest x))))))

(defcheck solution-a63db4bc
  (fn [a-seq & more]
    (if (empty? a-seq)
      (if more more 0)
      (recur (rest a-seq) (if more (+ more 1) 1)))))

(defcheck solution-a63ee79d
  #(loop [i 0 coll %] (if (empty? coll) i (recur (inc i) (next coll)))))

(defcheck solution-a6956f8c
  (partial reduce (fn [c,_] (inc c)) 0))

(defcheck solution-a6c8fb52
  (fn [coll]
    (let [ones (map (fn [_] 1) coll)]
      (apply + ones))))

(defcheck solution-a6ccc3a4
  #((fn rest-iter [x,y] (
                          if (= (rest (vec x)) [])
                          y
                          (rest-iter (rest x) (+ y 1))))
    % 1))

(defcheck solution-a7475e2f
  (fn [in]
    (loop [s in
           n 0]
      (if (empty? s)
        n
        (recur (rest s) (+ n 1))))))

(defcheck solution-a76ea07e
  (fn [xs]
    (loop [v xs, n 0]
      (if (empty? v)
        n
        (recur (rest v) (inc n))))))

(defcheck solution-a7c6ff4
  (fn [x]
    (loop [s x i 0]
      (if (empty? s)
        i
        (recur (rest s) (inc i))))))

(defcheck solution-a8941de0
  (fn len [s] (if (seq (rest s)) (+ 1 (len (rest s))) 1)))

(defcheck solution-a8a77176
  #(reduce + (map (fn [x] 1) %)))

(defcheck solution-a8a7b465
  (fn taille [liste]
    (
      if (empty? liste)
      0
      (+ 1 (taille (rest liste)))
      )
    ))

(defcheck solution-a8fc2bec
  (fn c [lst]
    (if (empty? lst)
      0
      (+ 1 (c (rest lst))))))

(defcheck solution-a95727f1
  (fn my-count [xs]
    (loop [ys (seq xs) n 0]
      (if (= ys nil) n (recur (next ys) (inc n))))))

(defcheck solution-a9a0d632
  (fn my-count
    ([c] (my-count c 0))
    ([c n] (if (seq c) (recur (rest c) (inc n)) n))))

(defcheck solution-a9ce7e22
  #((fn ! [x,y] (if (= x ()) y (! (rest x)  (inc y)))) % 0))

(defcheck solution-aa65b71a
  (fn [coll]
    ((fn [coll acc]
       (if (empty? coll) acc (recur (rest coll) (inc acc)))
       ) coll 0)))

(defcheck solution-aa6f1191
  (partial reduce #(do %& (inc %1)) 0))

(defcheck solution-aa8c9071
  (fn [seq]
    (loop [seq seq len 0]
      (if (not (empty? seq))
        (recur (rest seq) (+ len 1))
        len))))

(defcheck solution-aa98454
  (fn _count [items]
    (if (empty? items)
      0
      (+ 1 (_count (rest items))))))

(defcheck solution-aaada7da
  (fn me [l] ( if (empty? l) 0 (+ 1 (me (rest l))))))

(defcheck solution-aac79f17
  (fn [s] (loop [ss s ll 0]
            (if (empty? ss)
              ll
              (recur (rest ss) (inc ll))))))

(defcheck solution-ab0f0a56
  (partial reduce (fn [a b] (+ a 1)) 0))

(defcheck solution-ab309a8a
  (fn mycount[xs]
    (if (empty? xs)
      0
      (+ 1 (mycount (rest xs)))
      )
    ))

(defcheck solution-ab39e72a
  #(loop [l %1 n 0]
     (if (empty? l)
       n
       (recur (rest l) (inc n)))))

(defcheck solution-ab4dd1dd
  (fn [coll]
    (loop [coll coll n 0]
      (if (= coll '())
        n
        (recur (rest coll) (inc n))
        ))))

(defcheck solution-ab688e97
  (fn cou [cont]

    (if (next cont)
      (inc (cou (rest cont)))
      1
      )

    ))

(defcheck solution-ab76059b
  (fn [seq] (loop [s seq c 0] (if (empty? s) c (recur (rest s) (inc c))) )))

(defcheck solution-aba01ee6
  #(apply + (map (fn [_] 1) %1)))

(defcheck solution-abac7c74
  (fn reccount [xsq]
    (if (= (rest xsq) '())
      1
      (inc (reccount (rest xsq))))))

(defcheck solution-ac1e9b59
  (fn [col]
    (loop [c col ret 0]
      (if (nil? (seq c))
        ret
        (recur (rest c) (inc ret))))))

(defcheck solution-ac3399a9
  #(loop [n 0 lst %]
     (if lst
       (recur (inc n) (next lst))
       n)))

(defcheck solution-ac942ed0
  (fn [x] (reduce (fn [a b] (inc a)) 0 x)))

(defcheck solution-acc2581c
  #(reduce (fn [x _] (+ 1 x)) 0 %))

(defcheck solution-ad82d60e
  (fn [v]
    (loop [v v
           n 0]
      (if (empty? v)
        n
        (recur (rest v) (+ n 1))))))

(defcheck solution-ae15935e
  #(inc (first (last (map-indexed list %)))))

(defcheck solution-ae9020f2
  (fn myLength'
    [lst]
    (cond
      (empty? lst) 0
      :else (+ 1 (myLength' (rest lst))))))

(defcheck solution-ae99ca68
  #(loop [s % a 0] (if (seq s) (recur (next s) (inc a)) a)))

(defcheck solution-aeea8697
  (fn [li]
    (reduce + (map (constantly 1) li))))

(defcheck solution-af02944
  (fn [x] (apply + (map (fn[y] 1) x))))

(defcheck solution-af2c189f
  (fn [coll]
    (loop [coll coll n 0]
      (if-let [s (seq coll)]
        (recur (rest s) (inc n))
        n))))

(defcheck solution-af8c59d2
  (fn [element]
    (reduce (fn [total element]
              (+ 1 total))
      0 element)))

(defcheck solution-afb32c52
  (fn length [q] (if (empty? q) 0 (+ 1 (length (rest q))))))

(defcheck solution-afe73257
  (fn [coll]
    (loop [coll coll
           cnt 0]
      (if (next coll)
        (recur (next coll) (+ cnt 1))
        (+ cnt 1)))))

(defcheck solution-afec3499
  (fn [col]
    (letfn [(rt1 [ar1] 1)]
      (apply + (map rt1 col)))))

(defcheck solution-b12c2d5f
  (fn [coll] (apply + (map (fn [_] 1) coll))))

(defcheck solution-b15773ac
  (fn [coll] (reduce (fn [a,b] (inc a))  0 coll)))

(defcheck solution-b1b9517b
  (fn my-count [coll]
    (loop [coll coll n 0]
      (if-not (seq coll)
        n
        (recur (rest coll) (inc n))))))

(defcheck solution-b1de0be3
  #(reduce (fn [a & b] (+ 1 a)) 0 %))

(defcheck solution-b1ef792
  (fn [x] (reduce + (map #(if (nil? %) 0 1) x))))

(defcheck solution-b20c251f
  #(loop [s % c 0]
     (if (empty? s) c
                    (recur (next s) (inc c)))))

(defcheck solution-b2346eeb
  (fn [s]
    (loop [x (seq s)
           l 0]
      (if-not (seq? x)
        l
        (recur (next x) (inc l))))))

(defcheck solution-b2d7ea37
  #(reduce (fn[s n](inc s)) 0 %))

(defcheck solution-b331e90b
  (fn [coll]
    (loop [c coll n 0]
      (if (empty? c)
        n
        (recur (rest c) (inc n))))))

(defcheck solution-b3ca8999
  (fn mycnt
    ([seq] (mycnt seq 0))
    ([seq counter] (if (empty? seq)
                     counter
                     (recur (rest seq) (inc counter))))))

(defcheck solution-b3d6ae79
  (fn cc [x] (if (empty? x)
               0
               (+ 1 (cc (rest x))))))

(defcheck solution-b406d365
  (fn [xs]
    (->> xs
      (map (constantly 1))
      (reduce +))))

(defcheck solution-b4e79c57
  #(reduce (fn [acc _] (inc acc)) 0 %))

(defcheck solution-b4f7c0d7
  (fn mycount [x] (if (empty? x) 0 (+ (mycount (rest x)) 1))))

(defcheck solution-b56b8f0b
  #(if (list? %1) (count %1) (count %1)))

(defcheck solution-b57c6544
  (fn [c]
    (loop [coll c acc 0]
      (if (empty? coll) acc (recur (rest coll) (+ acc 1))))))

(defcheck solution-b5d69049
  (fn my-count [s]
    (loop [s s n 0]
      (if (empty? s)
        n
        (recur (rest s) (inc n))))))

(defcheck solution-b60d5b37
  #(reduce (fn [acc, y] (inc acc)) 0  %))

(defcheck solution-b65863a7
  (fn [coll] (loop [n 0 c coll] (if (nil? c) n
                                             (recur (inc n) (next c))))))

(defcheck solution-b66554c4
  (fn countSeq [s] (if (empty? s) 0 (+ (countSeq (rest s)) 1))))

(defcheck solution-b6af647e
  (fn count-elements [x]
    (if (empty? x) 0
                   (+ 1 (count-elements (rest x))))))

(defcheck solution-b714e487
  (fn my-count [s]
    (if (empty? s) 0 (inc (my-count (rest s))))))

(defcheck solution-b7f5a690
  (fn [coll]
    (loop [acc 0
           coll coll]
      (if (empty? coll)
        acc
        (recur (inc acc) (rest coll))))))

(defcheck solution-b8252639
  #(last (map (fn [_ c] c) % (iterate inc 1))))

(defcheck solution-b927d200
  (fn my-count [coll] (apply + (map (constantly 1) coll))))

(defcheck solution-b98fd524
  (fn f [x]
    (if (= x ())
      0
      (+ 1 (f (rest x))))))

(defcheck solution-b9c42021
  (fn len [xs] (if (empty? xs) 0 (inc (len (rest xs))))))

(defcheck solution-ba71f8c2
  (fn count-a-seq [l]
    (reduce (fn [x y] (+ x 1)) 0 l)))

(defcheck solution-bb4180fb
  (fn my-count [xs]
    (loop [xs (seq xs) n 0]
      (if xs
        (recur (next xs) (inc n))
        n))))

(defcheck solution-bb89d79d
  (fn [x] (apply + (map #(do % 1) x))))

(defcheck solution-bbf0d359
  (fn c [s] (if (seq s) (+ 1 (c (rest s))) 0)))

(defcheck solution-bce44c72
  #(apply max (keys (zipmap (range 1 12) %))))

(defcheck solution-bd2b68cd
  (fn [s] (reduce + (map (fn [x] 1) s))))

(defcheck solution-bd3ab4f3
  (partial reduce (fn [acc _] (+ acc 1)) 0))

(defcheck solution-bd3f7023
  (fn number [seq]
    (if (empty? seq) 0
                     (+ 1 (number (rest seq))))))

(defcheck solution-bd7e7210
  #((fn c [s a]
      (if (seq s)
        (c (rest s) (inc a))
        a))
    %
    0))

(defcheck solution-bd85d2bb
  (fn [lst] (reduce (fn [r _] (+ r 1)) 0 lst)))

(defcheck solution-bd9532d6
  (fn my-count [lstr] (if (empty? lstr)
                        0
                        (inc (my-count (rest lstr))))))

(defcheck solution-bdaf1a18
  #(loop [x %
          y 0]
     (if (empty? x)
       y
       (recur (rest x) (inc y)))))

(defcheck solution-bdeb155d
  (fn count-e [x]
    (if (empty? x) 0
                   (inc (count-e (rest x))))))

(defcheck solution-be5108a0
  (fn [coll]
    (reduce (fn [result item] (+ 1 result)) 0 (apply list coll))))

(defcheck solution-bf5cd18b
  #(last (map-indexed (comp first vector) (cons :f %) )))

(defcheck solution-bfac7bbf
  #(reduce + (map (fn [x] 1)%)))

(defcheck solution-bfff7726
  (fn [coll]
    (let [accrue-count (fn [remainder total]
                         (if (next remainder)
                           (recur (next remainder) (+ 1 total))
                           total))]
      (accrue-count coll 1))))

(defcheck solution-c03a5ba4
  (fn mc [s & i]
    (if (= s ())
      (or (first i) 0)
      (mc (rest s) (inc (or (first i) 0))))))

(defcheck solution-c2bbb6b6
  (fn count-seq [x] (reduce (fn [x y] (+ x 1)) 0 x)))

(defcheck solution-c33ba167
  #(reduce (fn [acc x] (inc acc)) 0 %))

(defcheck solution-c40f8d39
  (fn [x]
    (loop [s x n 0]
      (if (empty? s)
        n
        (recur (rest s) (inc n))))))

(defcheck solution-c435d237
  (fn count' [s]
    (if (seq s)
      (inc (count' (rest s)))
      0)))

(defcheck solution-c45ef23b
  #((zipmap %1 (iterate inc 1)) (last %1)))

(defcheck solution-c4ccfb4a
  #(loop [x % acc 0]
     (if (empty? x)
       acc
       (recur (rest x) (inc acc)))))

(defcheck solution-c4d5417b
  (fn my-count [s]
    (if (empty? s)
      0
      (inc (my-count (rest s))))))

(defcheck solution-c533595c
  (fn f [s]
    (cond
      (empty? s) 0
      :else (inc (f (rest s))))))

(defcheck solution-c550497
  (fn [arr]
    (loop [c 0
           a arr]
      (if-not (empty? a)
        (recur (inc c) (rest a))
        c))))

(defcheck solution-c57ff9f3
  (fn
    [x]
    (loop
     [a x, cnt 0]
      (if
       (empty? a)
        cnt
        (recur (rest a) (inc cnt)) ))))

(defcheck solution-c6507ca6
  (fn [s] (loop [s (seq s) n 0] (if s (recur (next s) (inc n)) n))))

(defcheck solution-c7c0cf0f
  (fn[my-seq]
    (loop [the-seq my-seq the-count 0]
      (if (next the-seq)
        (recur (next the-seq) (inc the-count))
        (inc the-count)
        )
      )
    ))

(defcheck solution-c7d3a158
  #(reduce (fn [n x] (inc n)) 0 %))

(defcheck solution-c803888b
  (fn [coll]
    (apply + (map (fn [x] 1) coll))))

(defcheck solution-c87a533
  #(reduce (fn [n _] (+ n 1)) 0 %))

(defcheck solution-c886cce9
  #(loop [col  %
          ct   0]
     (if (empty? col) ct (recur (rest col) (inc ct)))))

(defcheck solution-c90b1232
  (fn [c] (reduce (fn [v e] (+ v 1)) 0 c)))

(defcheck solution-c92e7a93
  #(loop [list %1 length 0] (if (empty? list) length (recur (rest list) (inc length)))))

(defcheck solution-c9ce9d44
  (fn [coll] (reduce + (map (fn [y] 1) coll))))

(defcheck solution-ca0971ca
  (fn len [xs]
    (if (last xs)
      (inc (len (rest xs)))
      0
      )))

(defcheck solution-ca427303
  (fn[s] (reduce (fn[x y] (+ x 1)) 0 s)))

(defcheck solution-ca55fb16
  #(loop [l %, acc 0] (if (empty? l) acc (recur (rest l) (inc acc)))))

(defcheck solution-caaea47f
  #(loop [s % c 0]
     (if (empty? s)
       c
       (recur (rest s) (inc c)
         ))))

(defcheck solution-cb04af36
  (fn [xs] (->> (map (fn [x y] (+ x 1)) (range) xs) last)))

(defcheck solution-cb405df4
  #(inc (last (map-indexed (fn [idx itm] idx) %))))

(defcheck solution-cbbcf449
  (fn [x] ((fn [x i] (if x (recur (next x) (inc i)) i)) x 0)))

(defcheck solution-cc62ac11
  (fn [coll] (loop [c (seq coll) n 0] (if c (recur (next c) (inc n)) n))))

(defcheck solution-cca309a7
  #(-> % vec count))

(defcheck solution-cdbb733
  #(reduce (fn [tot _] (inc tot)) 0 %))

(defcheck solution-cde69be5
  (fn f [x] (if (empty? x) 0 (+ 1 (f (rest x))))))

(defcheck solution-cdf1b568
  (fn [coll]
    (loop [lst coll, acc 0]
      (if (empty? lst)
        acc
        (recur (rest lst) (+ acc 1))
        ))))

(defcheck solution-ce4ed586
  (fn [s]
    (loop [se s sum 0]
      (if (empty? se)
        sum
        (recur (rest se) (+ 1 sum))))))

(defcheck solution-cecc3e4f
  (fn [xs] (reduce + (map (fn [x] 1) xs) ) ))

(defcheck solution-cf3507a5
  (fn [l]
    (reduce (fn [acc, v] (+ acc 1)) 0 l)
    ))

(defcheck solution-cf8b2f5d
  #(reduce (fn[x y] (inc x)) 0 %))

(defcheck solution-d165f462
  (fn [s]
    (reduce (fn [i x]
              (inc i))
      0 s)))

(defcheck solution-d1b49554
  (fn [lat] (reduce (fn [x y] (+ x 1)) 0 lat)))

(defcheck solution-d2c37a61
  (fn [coll]
    (loop [acc 0]
      (if (empty? (drop acc coll))
        acc
        (recur (inc acc))))))

(defcheck solution-d3229f45
  (fn count1 [xs] (if (empty? xs) 0 (+ 1 (count1 (rest xs))))))

(defcheck solution-d383affb
  #(apply + (map (fn [x] 1) %1)))

(defcheck solution-d3df2ba8
  (fn [coll]
    (loop [total 0
           [head & tail] coll]
      (if (nil? head)
        total
        (recur (inc total) tail)))))

(defcheck solution-d483ef6f
  #(reduce (fn [i j] (inc i)) 0 %))

(defcheck solution-d48f9ed7
  #(reduce (fn [acc a] (inc acc)) 0 %1))

(defcheck solution-d49a9cbf
  #(if (string? %) (count %) (count %)))

(defcheck solution-d599fe0e
  (fn [coll] (reduce (fn [a b] (inc a)) 0 coll)))

(defcheck solution-d59d3eb7
  (fn [l]
    (loop [acc 0, l' l]
      (if (empty? l')
        acc
        (recur (inc acc) (rest l'))))))

(defcheck solution-d64f750a
  (fn cnt [coll] (if (empty? coll) 0 (inc (cnt (rest coll))))))

(defcheck solution-d6b79c61
  (fn [coll]
    "Write a function which returns the total number of elements in a sequence."
    (loop [coll coll
           acc 0]
      (if (empty? coll)
        acc
        (recur (rest coll) (inc acc))))))

(defcheck solution-d6d60679
  (fn [x]
    (loop [s x result 1]
      (if (= nil (next s))
        result
        (recur (next s ) ( inc result))))))

(defcheck solution-d6ed9e8d
  #(reduce (fn [acc e] (inc acc)) 0 %))

(defcheck solution-d71f7c84
  (fn c
    ([lst] (c (seq lst) 0))
    ([lst length] (if (first lst) (c (rest lst) (inc length)) length))))

(defcheck solution-d77491f
  (fn [a-seq]
    (loop [s a-seq a 0]
      (if (empty? s)
        a
        (recur (rest s) (inc a))))))

(defcheck solution-d78d2431
  (fn [coll]
    (loop [in coll
           out 0 ]
      (if (empty? in)
        out
        (recur (drop-last in)
          (inc out))))))

(defcheck solution-d7c4824f
  (fn [lst]
    (loop [ls lst cnt 0]
      (if (empty? ls) cnt
                      (recur (rest ls) (inc cnt))))))

(defcheck solution-d92018aa
  (fn [x]
    (loop [ls x cnt 0]
      (if (= nil (first ls))
        cnt
        (recur (rest ls) (inc cnt))))))

(defcheck solution-d9614789
  #(reduce (fn [c i] (inc c)) 0 %))

(defcheck solution-d99060ce
  (fn [coll] (reduce + (map (fn [_] 1) coll))))

(defcheck solution-d9bb37eb
  (fn count-seq[xs]
    (reduce (fn[x y](inc x)) 0 xs)))

(defcheck solution-d9ccf42a
  (fn countN [x] (if (empty? x) 0 (+ 1 (countN (rest x))))))

(defcheck solution-d9cff3d2
  (fn [sequence] (reduce (fn [acc v] (inc acc)) 0 sequence)))

(defcheck solution-d9d766fc
  (fn [aseq] (reduce (fn [c _] (inc c)) 0 aseq)))

(defcheck solution-d9f151fe
  (fn [coll]
    (loop [incoll coll
           acc 0]
      (if (empty? incoll)
        acc
        (recur (rest incoll) (inc acc))))))

(defcheck solution-da7e989
  (fn [coll]
    (reduce (fn [l r] (inc l))
      0 coll)))

(defcheck solution-dacbc501
  (partial reduce (fn [x _] (inc x)) 0))

(defcheck solution-dad128b9
  (fn[a]
    (loop [b a x 0]
      (if (empty? b) x (recur (next b) (inc x))))))

(defcheck solution-db57d1b2
  (fn [coll]
    (loop [coll coll
           n 0]
      (if (empty? coll)
        n
        (recur (rest coll) (inc n))))))

(defcheck solution-dc0f5f8b
  (fn [l] ( loop [r l n 0] (if (empty? r) n (recur (rest r) (inc n))))))

(defcheck solution-dc329328
  #(loop [xs %1 cnt 0]
     (if xs
       (recur (next xs) (inc cnt))
       cnt)))

(defcheck solution-dc74ddf0
  #(reduce (fn [quant elem] (inc quant)) 0 %))

(defcheck solution-dcb41e77
  #(reduce + (map first (partition 2 (interleave (repeat 1) %)))))

(defcheck solution-dcf0108f
  (fn myc [sec]
    (if (not-empty sec)
      (+ 1 (myc (rest sec)))
      0)))

(defcheck solution-dd207bcb
  (fn cnt [s]
    (loop [s s c 0]
      (if (empty? s) c
                     (recur (rest s) (inc c))))))

(defcheck solution-dd7946b9
  (fn new-count
    ([s] (new-count s 0))
    ([s n]
     (if (empty? s)
       n
       (recur (rest s) (inc n))))))

(defcheck solution-dd9b77e5
  (fn [s]
    (reduce (fn [c v] (inc c)) 0 s)))

(defcheck solution-ddd29453
  (fn [s]
    (loop [l 0
           s s]
      (if (empty? s) l
                     (recur (inc l)
                       (rest s))))))

(defcheck solution-de72694b
  (fn [s]
    (loop [se s
           ac 0]
      (if (seq se)
        (recur (rest se) (inc ac))
        ac))))

(defcheck solution-de8ce767
  (fn c[x]
    (reduce + (map (fn i[x] 1 ) x))))

(defcheck solution-def544da
  (fn [coll] (reduce (fn [n _] (inc n)) 0 coll)))

(defcheck solution-df2d7964
  #(loop [c 0 col %] (if (first col) (recur (inc c) (rest col)) c)))

(defcheck solution-df931be0
  (fn [aseq]
    (loop [tseq aseq, num 0]
      (if tseq
        (recur (next tseq) (inc num))
        num))))

(defcheck solution-dfe8117b
  #(letfn [(worker [x n]
             (if (= x ())
               n
               (recur (rest x) (+ 1 n))))]
     (worker % 0)))

(defcheck solution-e0212e8f
  (fn this
    ([xs acc] (if (empty? xs) acc (recur (rest xs) (inc acc))))
    ([xs] (this xs 0))))

(defcheck solution-e0503aaa
  (fn [coll]
    (loop [i 0 items coll]
      (if (empty? items)
        i
        (recur (inc i) (rest items))
        )
      )
    ))

(defcheck solution-e059ea49
  #(count (vec %)))

(defcheck solution-e0e9aa5b
  (fn [coll]
    (loop [x 0 sub coll]
      (if (last sub)
        (recur (inc x) (next sub))
        x))))

(defcheck solution-e0f7e433
  (fn len [s] (if (seq s) (+ (len (rest s)) 1) 0)))

(defcheck solution-e125c06a
  #(reduce (fn [n & xs] (inc n)) 0 %))

(defcheck solution-e1281829
  (fn [s] (loop [len 0 seq s] (if (empty? seq) len (recur (inc len) (rest seq))))))

(defcheck solution-e13ecb18
  (fn [input]
    (loop [i 0 elements input]
      (if (empty? elements)
        i
        (recur (inc i) (rest elements)))
      )
    ))

(defcheck solution-e1d35685
  (fn countti
    ([lista]
     (if (empty? lista)
       0
       (+ 1 (countti (rest lista)))))))

(defcheck solution-e1fa00d5
  #(reduce + (map (fn [_] 1) %)))

(defcheck solution-e2300f8e
  (partial reduce (fn [c e] (+ c 1)) 0))

(defcheck solution-e23c44ec
  #(count (vec %)))

(defcheck solution-e28759f7
  #(reduce (fn [a & _] (inc a)) 0 %))

(defcheck solution-e2b97896
  ; #(reduce (fn [acc _] (inc acc)) 0 %)

  #(reduce + (map (constantly 1) %)))

(defcheck solution-e2fa29e6
  (fn [x]
    (loop [s x acc 0]
      (if (empty? s)
        acc
        (recur (rest s) (inc acc))))))

(defcheck solution-e4c3829d
  #(loop [lst % c 0]
     (if (empty? lst)
       c
       (recur (rest lst) (inc c)))))

(defcheck solution-e4e3fe7d
  #(reduce (fn [s v] (inc s)) 0 %))

(defcheck solution-e5044c45
  (fn [s] (reduce + (map (constantly 1) s))))

(defcheck solution-e50a7a85
  (fn [s]
    (loop [s s n 0]
      (if (seq s)
        (recur (rest s) (inc n))
        n))))

(defcheck solution-e515157a
  (fn cnt [s]
    (reduce + (map (fn [v] 1) s))))

(defcheck solution-e554f1f2
  #(reduce (fn [x y] (+ x 1)) (cons 0 % )))

(defcheck solution-e6c80102
  (comp (partial apply +) (partial map (fn [_] 1))))

(defcheck solution-e782f093
  (fn [x] (reduce (fn [a b] (+ 1 a)) 0 x)))

(defcheck solution-e78e84a2
  (fn [item] (loop [sq (seq item) c 0] (if-let [x (first sq)] (recur (rest sq) (inc c)) c ))))

(defcheck solution-e7ada662
  (fn [x]
    (loop [lst x cnt 0]
      (if (empty? lst)
        cnt
        (recur (rest lst) (inc cnt))))))

(defcheck solution-e7bedfc7
  (fn mycount [x] (reduce + (map (constantly 1) x))))

(defcheck solution-e7e5fab
  (fn size [sq] (if (empty? sq) 0 (+ 1 (size (rest sq))))))

(defcheck solution-e7e74f31
  (fn ccount [s]
    (if (empty? s)
      0
      (+ 1 (ccount (rest s))))))

(defcheck solution-e8c2f5dc
  (fn my-count [xs] (if (empty? xs)
                      0
                      (+ 1 (my-count (rest xs))))))

(defcheck solution-e8c503d7
  (fn howmany [x]
    (let [y (seq x)]
      (if (nil? (first y))
        0
        (+ 1 (howmany (rest y)))))))

(defcheck solution-e8d5ac26
  (fn [xs]
    ((fn [xs accum]
       (if (empty? xs)
         accum
         (recur (rest xs) (inc accum))))
     xs 0)))

(defcheck solution-e8f29206
  #(apply + (map (fn [_] 1) %)))

(defcheck solution-e9370489
  #(loop [lst %1 cnt 0] (if (empty? lst) cnt (recur (rest lst) (inc cnt)))))

(defcheck solution-e944546d
  (fn cnt [l & [x]]
    (cond (empty? l) (if (nil? x) 0 x)
          (nil? x) (cnt (rest l) 1)
          :else (cnt (rest l) (inc x))
          )
    ))

(defcheck solution-e9978c33
  (fn [coll]
    (
     (fn step [subcoll n]
       (if
        (first (rest subcoll))
         (step (rest subcoll) (+ n 1))
         n)
       )
     coll
     1
     )
    ))

(defcheck solution-ea293569
  (fn [x] (loop [s x n 0] (if (= s []) n (recur (rest s) (+ n 1))))))

(defcheck solution-ea320307
  (fn [l]
    (loop [mylist l cnt 0]
      (if (= mylist '())
        cnt
        (recur (rest mylist) (inc cnt))))))

(defcheck solution-ea7dfc2a
  (fn [s] (loop [c 0 r s]
            (if (empty? r)
              c
              (recur (inc c) (rest r))
              ))
    ))

(defcheck solution-ea8274b7
  (let [f (fn [a n] (if (nil? a) n (recur (next a) (inc n))))] (fn [a] (f a 0))))

(defcheck solution-eab29ca6
  (fn [lst] (reduce (fn [n _] (inc n)) 0 lst)))

(defcheck solution-eb6a60c8
  (fn [coll]
    (loop [c 0 col coll]
      (if (empty? col)
        c
        (recur (inc c) (rest col))))))

(defcheck solution-ebb2a0b2
  (fn [coll]
    (reduce (fn [n _]
              (inc n))
      0
      coll)))

(defcheck solution-ebb48ce5
  (fn  [coll]
    (loop [c coll n 0]
      (if (first c)
        (recur (next c) (inc n))
        n))))

(defcheck solution-ec0e3bab
  (fn [col] (loop [ct (if (first col) 1 0), mycol col]
              (if-let [ncol (next mycol)]
                (recur (inc ct) ncol )
                ct ))))

(defcheck solution-ec97caeb
  #(reduce (fn [s _] (inc s)) 0 %))

(defcheck solution-ed13ef2f
  (fn my-count [items]
    (if (empty? items)
      0
      (inc (my-count (rest items))))))

(defcheck solution-edd368c6
  #(reduce (fn [accum nxt] (inc accum)) 0 %))

(defcheck solution-ef677763
  (letfn [(C [coll]
            (if (empty? coll) 0
                              (inc (C (rest coll)))))]
    C))

(defcheck solution-efe26c30
  #(loop [len 0
          acc %]
     (if (empty? acc)
       len
       (recur (+ len 1) (rest acc)))))

(defcheck solution-f0e0112b
  (fn [xs]
    (loop [ys xs l 0]
      (if (empty? ys) l (recur (rest ys) (inc l))))))

(defcheck solution-f0e859d3
  (fn [seq]
    (reduce (fn [a b] (+ 1 a)) 0 seq)))

(defcheck solution-f183e87b
  #(+ 1 (first(last(map-indexed vector %)))))

(defcheck solution-f2ebcc04
  #(+ (.lastIndexOf (seq %) (last (seq %))) 1))

(defcheck solution-f2feecca
  (fn [l] (reduce (fn [n _] (inc n)) 0 l)))

(defcheck solution-f3230ea9
  (fn [s]
    (reduce (fn [acc x] (+ acc 1)) 0 s)))

(defcheck solution-f3a730fc
  (fn my-count
    ([lst] (apply my-count lst 0 lst))
    ([lst, cnt] cnt)
    ([lst, cnt, & more]
     (apply my-count lst (inc cnt) (rest more)))))

(defcheck solution-f3c2a4bc
  #(loop [sq % cnt 0]
     (if (empty? sq)
       cnt
       (recur (rest sq) (inc cnt)))))

(defcheck solution-f3df50b9
  (fn [a]
    (reduce + 0 (map (fn [_] 1) a))))

(defcheck solution-f4723dc8
  #(reduce +
     (map (constantly 1) %)))

(defcheck solution-f4919d82
  (comp (partial reduce +)
        (partial map (constantly 1))))

(defcheck solution-f5013deb
  #(reduce (fn [c _] (inc c)) 0 (seq %)))

(defcheck solution-f5093731
  (fn my-count [coll] (if (empty? coll) 0 (inc (my-count (rest coll))))))

(defcheck solution-f5505ef1
  ;#(loop [result 0 c %]
  ;   (if (empty? c)
  ;     result
  ;     (recur (inc result) (rest c))))
  #(reduce + (map (fn [x] 1) %)))

(defcheck solution-f5cc1b9e
  #(loop [n 0 c %]
     (if (next c)
       (recur (inc n) (next c))
       (inc n))))

(defcheck solution-f5de77dc
  (fn my-count [x]
    (loop [coll x cnt 0]
      (if (empty? coll)
        cnt
        (recur (rest coll) (+ cnt 1))))))

(defcheck solution-f69d9d8d
  (partial reduce (fn [c _] (inc c)) 0))

(defcheck solution-f6ab3fae
  #(reduce (fn [cnt cl] (inc cnt)) 0 %))

(defcheck solution-f6f44cf4
  (fn [x]
    (loop [s x n 0]
      (if (empty? s) n (recur (rest s) (inc n))))))

(defcheck solution-f6f88e2f
  (fn [coll] (reduce (fn [v _] (inc v)) 0 coll)))

(defcheck solution-f70e674a
  (fn cow [x]
    (reduce + (map (fn [y] 1) x))))

(defcheck solution-f71fdbfa
  (fn [xs] (reduce (fn [x y] (+ x 1)) 0 xs)))

(defcheck solution-f755cbc3
  (fn [x]
    (letfn [(count-sequence-helper
              [x n]
              (if (empty? x)
                n
                (count-sequence-helper (rest x) (inc n))))]
      (count-sequence-helper x 0))))

(defcheck solution-f7ada7af
  (fn cnt ([lst n]
           (if (empty? lst) n (recur (rest lst) (inc n))))
    ([lst] (cnt lst 0))))

(defcheck solution-f8425db3
  (fn [coll] (loop [a coll cur 0] (if (empty? a) cur (recur (rest a) (+ cur 1))))))

(defcheck solution-f978659f
  (fn [mySequence]
    (loop [s mySequence n 0]
      (if (empty? s)
        n
        (recur (rest s) (inc n))
        )
      )
    ))

(defcheck solution-f97c3418
  (fn
    [coll]
    (letfn [(size
              [coll]
              (if (empty? coll)
                0
                (inc (size (rest coll)))))]
      (size coll))))

(defcheck solution-fa69cf56
  (fn ct [xs] (if (empty? xs) 0 (+ 1 (ct (rest xs))))))

(defcheck solution-fa793de9
  #(reduce (fn [acc, _] (+ acc 1)) 0 %))

(defcheck solution-fa9f7a1
  (fn l [x] (if (= x '()) 0 (+ (l (rest x)) 1))))

(defcheck solution-faaa2080
  #( reduce (fn [x y] (+ 1 x)) 0 %  ))

(defcheck solution-fb6f406b
  (fn [xs] (apply + (map (fn [_] 1) xs))))

(defcheck solution-fb905e26
  #(reduce
     (fn [c _] (inc c))
     0 %))

(defcheck solution-fbd89698
  (fn [x] (reduce + (map (fn [_] 1) x))))

(defcheck solution-fbe45b1
  (fn [coll]
    (reduce (fn [x y] (+ x 1)) 0 coll)
    ))

(defcheck solution-fd01ffb0
  (fn f [sequence]
    (loop [c 0
           s sequence]
      (if-not (seq s)
        c
        (recur (inc c) (rest s))))))

(defcheck solution-ffca823
  #(loop [coll % n 0]
     (if (empty? coll)
       n
       (recur (rest coll) (inc n)))))

(defcheck solution-ffcec8de
  (fn size [x] (if (empty? x) 0 (+ 1 (size (rest x))))))

(defcheck solution-ffddf3df
  (fn [coll] (first (last (map vector (iterate inc 1) coll)))))

(defcheck solution-fff050c3
  (fn [l]
    (loop [a l, b 0]
      (if a
        (recur (next a) (inc b))
        b))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-22))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
