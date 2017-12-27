(ns coal-mine.problem-42
  (:require [coal-mine.checks :refer [defcheck-42] :rename {defcheck-42 defcheck}]
            [clojure.test]))

#?(:cljs (def *' *))

(defcheck solution-101c0a8d
  (partial nth (cons 1 (reductions * (rest (range))))))

(defcheck solution-10f708cc
  (fn [x] (reduce * (map inc (range x)))))

(defcheck solution-114d7b20
  #(reduce
     *
     (rest (range (+ 1 %)))))

(defcheck solution-134c754c
  (fn fact [x] (if (>= 1 x) 1 (* x (fact (dec x))))))

(defcheck solution-13c1dd5
  #(last
     (take (inc %)
       (map last
         (iterate
           (fn [fact]
             (conj fact (* (last fact) (count fact))))
           [1])))))

(defcheck solution-13d4070c
  #(loop [n %1 fact 1]
     (cond
       (<= n 1) fact
       :else (let [fact (* fact n)]
               (recur (dec n) fact)))))

(defcheck solution-13e7f3d0
  #(reduce * (range 1 (+ %1 1))))

(defcheck solution-141025ce
  (fn fx [n] (
               let [hfx (fn [x m] (if (= x 1) m (recur (dec x) (* m x))))] (hfx n 1))))

(defcheck solution-144419e1
  #(nth (reductions * (iterate inc 1)) (dec %)))

(defcheck solution-14c14a2e
  (fn fact [n]
    (if
     (= 1 n)
      n
      (* n (fact (dec n))))))

(defcheck solution-156515fd
  (fn fac [n]
    (if (zero? n)
      1
      (* n (fac (dec n))))))

(defcheck solution-1680b22b
  (fn [n] (reduce *' (range 1 (inc n)))))

(defcheck solution-170b90ac
  #(loop [acc 1 x %] (if (< x 2) acc (recur (* x acc) (dec x)))))

(defcheck solution-17447437
  (fn
    [n]
    (loop [multiplier n acc 1]
      (if (zero? multiplier)
        acc
        (recur (dec multiplier) (* acc multiplier))))))

(defcheck solution-174a9a9e
  (fn [n]
    (reduce *
      (range 1 (inc n)))))

(defcheck solution-17cc1aff
  (fn my-fac [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-17f2827a
  (fn [cnt] (last (last (#(take % (iterate (fn [[nxt accum]] [(inc nxt) (* (inc nxt) accum)]) [1 1])) cnt)))))

(defcheck solution-182b8d84
  (fn [n]
    (loop [n n
           p 1]
      (if (= n 1)
        p
        (recur (dec n) (* n p))))))

(defcheck solution-192453a5
  (fn fact [n] (if (<= n 1) n (* n (fact (dec n))))))

(defcheck solution-194c96de
  (fn fact [n] (if (< n 2) 1 (* n (fact (- n 1))))))

(defcheck solution-19d7cfee
  (fn [limit] (let [values (range 1 (inc limit))]
                (reduce #(* %1 %2) 1 values)

                )))

(defcheck solution-1a463f29
  (fn fac [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-1a9310b1
  (fn f [x]
    (if (< x 2)
      1
      (* x (f (dec x))))))

(defcheck solution-1bb197a5
  #(apply * (next (range (inc %)))))

(defcheck solution-1bb735ef
  (fn f [n]
    (if (= n 1)
      1
      (* n (f (- n 1))))))

(defcheck solution-1cafe843
  (fn fac [n] (if (= n 0) 1 (* n (fac (- n 1))))))

(defcheck solution-1d038437
  (fn f [x] (if (zero? x) 1 (* x (f (- x 1))))))

(defcheck solution-1d6c18a6
  (fn fact [n]
    (cond
      (< n 0) nil
      (zero? n) 1
      :default (* n (fact (dec n))))))

(defcheck solution-1da3d286
  #((fn fac [n acc] (if (= 0 n) acc (fac (dec n) (* acc n)))) % 1))

(defcheck solution-1db01d09
  (fn [i]
    (reduce * (take i (iterate inc 1)))
    ))

(defcheck solution-1db11628
  (fn tmp [n]
    (loop [acc 1 i 1]
      (if (> i n)
        acc
        (recur (* acc i) (inc i))))))

(defcheck solution-1dfd09b7
  #(reduce * (take % (rest (range)))))

(defcheck solution-1dffe708
  (fn [x]
    (reduce #(* %1 %2) 1 (range 1 (inc x)))))

(defcheck solution-1ee3a853
  (fn fac [n]
    (if (< n 2) 1 (* n (fac (- n 1))))))

(defcheck solution-1fc181dd
  #(apply * (map inc (range %))))

(defcheck solution-1fe2d2e5
  #(apply * 1 (range 1 (inc %))))

(defcheck solution-1ffc8933
  (fn fac [n]
    (reduce * 1 (range 1 (inc n)))))

(defcheck solution-23c741b
  (fn fact [n]
    (if (= n 0)
      1
      (* n (fact (- n 1)))
      )
    ))

(defcheck solution-243ba379
  (fn factorial [n] (if (zero? n) 1 (* n (factorial (dec n))))))

(defcheck solution-249b7eb9
  (fn fact [n]
    (if (= 1 n)
      1
      (* n (fact (dec n))))))

(defcheck solution-24e68010
  (fn [n] (reduce * (take n (iterate dec n)))))

(defcheck solution-250ae0d0
  #(reduce * (range 1 (inc %1))))

(defcheck solution-258d5a30
  (fn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))))

(defcheck solution-25fe4c00
  #(apply * (range 1 (inc %))))

(defcheck solution-274fa308
  (fn fact [input]
    (reduce * (range 2 (inc input)))))

(defcheck solution-277af9d4
  (fn fact [n] (if (zero? n) 1 (* n (fact (dec n))))))

(defcheck solution-2789d232
  #(reduce * 1 (take % (iterate inc 1))))

(defcheck solution-27f6fd5e
  (fn [n]
    (reduce * (range 1 (+ 1 n)))))

(defcheck solution-283eb786
  (fn fac [x] (if (= x 1) 1 (* x (fac (dec x))))))

(defcheck solution-293023b8
  (fn [n]
    (loop [cnt 1 n n]
      (if (= n 1)
        cnt
        (recur (* n cnt) (dec n))))))

(defcheck solution-2a4ed84e
  (fn fact [n]
    (if (= n 1) 1
                (* n (fact (dec n))))))

(defcheck solution-2b277562
  (fn [n]
    (if (zero? n) 1                                         ;; unused
                  (apply * (range 1 (inc n))))))

(defcheck solution-2b4ea435
  (fn [x] (reduce (fn [y z] (* y z)) (range 1 (inc x)))))

(defcheck solution-2b571cb8
  #(->>
     (range 1 (inc %))
     (reduce *)
     ))

(defcheck solution-2b8a89ca
  (fn [n]
    (apply * (range 1 (inc n)))))

(defcheck solution-2cf648fd
  (fn [n]
    (loop [an 1 i 1]
      (if (> i n)
        an
        (recur (* an i) (inc i))))))

(defcheck solution-2d22bd0a
  (fn [n] ((fn iter [state count] (if (< n count) state (iter (* state count) (inc count)))) 1 1)))

(defcheck solution-2da5875
  #(loop [n % r 1] (if (< n 2) r (recur (dec n) (* r n)))))

(defcheck solution-2e268bb7
  (fn fac [n] (if (= n 1) 1
                          (let [a (fac (dec n))] (* a n)))))

(defcheck solution-2f5506d7
  (fn [n]
    (reduce #(* %1 %2) (range 1 (+ n 1)))))

(defcheck solution-2fb103d1
  (fn [z] (letfn [(fact [n] (if (zero? n) 1 (* n (fact (dec n)))))] (fact z))))

(defcheck solution-30c49ed7
  (fn [n] (loop [x n f 1] (if (= x 0) f (recur (- x 1) (* f x))))))

(defcheck solution-3107c2f2
  #(loop [result 1 n %]
     (if (= n 1)
       result
       (recur (* result n) (dec n)))))

(defcheck solution-3163ca6b
  (fn [n] ((fn [i f] (if (> i n) f (recur (inc i) (* f i)))) 1 1)))

(defcheck solution-31996623
  (fn [n]
    (let [b #(if (= %1 1)
               %2
               (recur (dec %1) (* %2 %1)))]
      (b n 1))))

(defcheck solution-31fb3141
  (fn [n]
    (loop [n   n
           acc 1]
      (if (= 1 n)
        acc
        (recur (dec n) (* n acc))))))

(defcheck solution-33ccd027
  (fn [x]
    (reduce * (range 1 (+ 1 x)))))

(defcheck solution-33ee927b
  (fn [p]
    (nth (map second (iterate (fn [[a b]] [(inc a) (* b (inc a))]) '[1 1])) (dec p))))

(defcheck solution-356aaa14
  (fn [n]
    (loop [i 1 result 1]
      (if (> i n) result
                  (recur (inc i) (* result i))))))

(defcheck solution-36103884
  #(reduce * 1 (range 2 (+ % 1))))

(defcheck solution-3656d08d
  (fn f [n]
    (if (< n 2)
      1
      (* n (f (- n 1))))))

(defcheck solution-3725e917
  ;; using mundane recursion:: stack limitation!!!
  (fn fact [x]
    (if (zero? x)
      1
      (* x (fact (dec x)))))

  ;; and using TCO for avoiding the stack limitation -> and get bigger numbers:
  #_(fn big-fact [x]
      (loop [decremented x
             fact-num    1N]
        (if (zero? decremented)
          fact-num
          (recur (dec decremented) (* fact-num decremented))))))

(defcheck solution-37f5a389
  (fn fact [x]
    (if (= x 0)
      1
      (* x (fact (- x 1))))))

(defcheck solution-380f2ba1
  #(* (apply * (range 1 (inc %)))))

(defcheck solution-38a7dabc
  {1 1, 3 6, 5 120, 8 40320})

(defcheck solution-39621244
  (fn my_fac [n] (if (= n 0) 1 (* n (my_fac (dec n))))))

(defcheck solution-39919152
  (fn [n]
    (loop [n n, acc 1]
      (if (= n 0)
        acc
        (recur (dec n) (* acc n))))))

(defcheck solution-3a7d3787
  (fn fact [n]
    (if (< 0 n)
      (* n (fact (- n 1)))
      1)))

(defcheck solution-3b53a99c
  #(reduce * 1 (drop 1 (range (inc %)))))

(defcheck solution-3bbbe53a
  #(->> [1 %]
     (iterate (fn [[acc i]]
                [(* acc i) (dec i)]))
     (drop-while (comp pos? second))
     (ffirst)))

(defcheck solution-3c779ca9
  #(apply * (rest (range (inc %)))))

(defcheck solution-3d77bf
  #(loop
    [x 1 result 1]
     (if (> x %) result (recur (inc x) (* x result)))))

(defcheck solution-3df2a2df
  (fn factorial [n]
    (loop [acc 1, n n]
      (if (< n 2)
        acc
        (recur (* acc n) (- n 1))))))

(defcheck solution-3e5933e0
  (fn [n]
    (loop [n n
           r 1]
      (if (= n 0)
        r
        (recur (- n 1) (* r n))))))

(defcheck solution-3eb19096
  (fn fact [n] (if (zero? n) 1 (* n (fact (dec n))))))

(defcheck solution-3f1d10cc
  (fn [x]
    (let [aux
          (fn [n acc]
            (cond
              (= 1 n)
              acc

              :else
              (recur (dec n) (* n acc))))]
      (aux x 1))))

(defcheck solution-3f5a7291
  (fn [n]
    (reduce #(* %1 %2) (range 1 (inc n)))))

(defcheck solution-3fad266e
  (fn myfactorial
    [cnt]
    (reduce #(* %1 %2) 1
      (range 1 (inc cnt)))))

(defcheck solution-3fe9725
  (fn fac [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-401f463
  #(loop [f 1 n %] (if (= n 0) f (recur (* f n) (dec n)))))

(defcheck solution-40ec3baa
  (fn factorial [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-413b3df6
  (fn [x]
    (reduce (fn [a b] (* a b)) 1 (range 1 (+ x 1)))))

(defcheck solution-4185ec65
  #(reduce (fn [x y] (* x y)) 1 (range 1 (inc %))))

(defcheck solution-41d57491
  #(reduce * (range % 1 -1)))

(defcheck solution-41e9b5bf
  #(loop [n   %
          acc 1]
     (if (= n 1)
       acc
       (recur (dec n) (* acc n))
       )
     ))

(defcheck solution-42279296
  (fn [n]
    (->> (iterate dec n)
      (take n)
      (reduce *))))

(defcheck solution-4376833
  (fn factorial [n] (if (<= n 1) 1 (* n (factorial (- n 1))))))

(defcheck solution-43db224c
  (fn fact [n]
    (if (<= n 1)
      1
      (* n (fact (dec n))))))

(defcheck solution-449de1db
  (fn [n] (reduce * (range 1 (+ 1 n)))))

(defcheck solution-44aff510
  (fn [x]
    (apply * (range 1 (inc x)))))

(defcheck solution-44fbace5
  (fn fac [x] (if (= 1 x) 1 (* x (fac (- x 1))))))

(defcheck solution-45b1dd1c
  ;(fn ! [n]
  ;  (if (= n 1)
  ;      1
  ;      (* n (! (dec n)))))
  #(reduce * (range 1 (inc %))))

(defcheck solution-45d37e24
  (fn fct [n]
    (if (> n 1)
      (* n (fct (dec n)))
      1)
    ))

(defcheck solution-4643d23
  (fn fctrl
    [i]
    (apply * (range 1 (+ i 1)))))

(defcheck solution-464cbad5
  (fn fac [n] (if (> n 2) (* n (fac (dec n))) n)))

(defcheck solution-46eebba8
  (fn go [n]
    (case n
      0 1
      (* n (go (- n 1))))))

(defcheck solution-483f6ab2
  #(loop [n %1 f 1]
     (if (> n 1)
       (recur (dec n) (* f n))
       f
       )
     ))

(defcheck solution-486672ba
  (fn fact [n]
    (cond
      (= 1 n) 1
      :else (* n (fact (dec n)))
      )
    ))

(defcheck solution-488b25e0
  (fn [number]
    (reduce * (range 1 (inc number)))))

(defcheck solution-48ab0458
  (fn factorial [x]
    (if (<= x 1)
      1
      (* x (factorial (dec x))))))

(defcheck solution-4918fd8c
  (fn fact [n] (apply * (range 1 (inc n)))))

(defcheck solution-4924c837
  (fn
    [n]
    (letfn [(factorial
              [n]
              (if (zero? n)
                1
                (* n (factorial (dec n)))))]
      (factorial n))))

(defcheck solution-49273ad7
  (fn fac [n]
    (loop [x n, result 1]
      (if (= x 0)
        result
        (recur (dec x) (* result x))))))

(defcheck solution-494baa75
  (comp
   (partial reduce *' 1)
   (partial range 1)
   (partial + 1)))

(defcheck solution-4a1858ac
  (fn fact [x]
    (if (zero? x)
      1
      (* x (fact (- x 1))))))

(defcheck solution-4a7308bb
  #(loop [n 1 rv 1]
     (if (<= n %)
       (recur (inc n) (* n rv))
       rv)))

(defcheck solution-4a9b1228
  (fn ! [x]
    (if (= 1 x)
      1
      (* x (! (dec x))))))

(defcheck solution-4a9de4bf
  (fn [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-4affbb97
  (comp (partial reduce *) next range inc))

(defcheck solution-4b28f7f1
  (fn [i] (reduce * (range 1 (inc i)))))

(defcheck solution-4b98aab3
  (fn [n] (reduce #(* %1 %2) 1 (map inc (range n)))))

(defcheck solution-4bbae6e2
  #(loop [result 1 current %1]
     (if (> current 0)
       (recur (* result current) (dec current))
       result)))

(defcheck solution-4bc46375
  (fn [n] (reduce * (range 1 (inc n)))))

(defcheck solution-4c95326a
  (fn [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-4d5b204d
  (fn f [n]
    (if (zero? n)
      1
      (* n (f (- n 1)))
      )))

(defcheck solution-4d7c1e6a
  (fn fact [x]
    (if (= 0 x)
      1
      (* x (fact (dec x))))))

(defcheck solution-4d920b2f
  (fn prob42 [x]
    (reduce * (range 1 (+ 1 x)))))

(defcheck solution-4dc126f1
  (fn fac [n]
    (if (= n 1)
      n
      (* n (fac (- n 1))))))

(defcheck solution-4e1360a1
  #(reduce * (range 1 (+ 1 %))))

(defcheck solution-4e9fc1d1
  (fn [n] (loop [c 1 t 1] (if (> c n) t (recur (inc c) (* t c))))))

(defcheck solution-4eded3e6
  #(reduce (fn [acc x] (* acc x)) (range 1 (+ % 1))))

(defcheck solution-502a7c7e
  (comp (partial reduce *) (partial range 1) inc))

(defcheck solution-505dd675
  [0 1 0 6 0 120 0 0 40320])

(defcheck solution-50a2ec94
  (fn [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-511746b
  (fn f [x] (if (< x 1) 1 (* x (f (- x 1))))))

(defcheck solution-51c4d404
  (fn [a] (reduce * (range 1 (+ 1 a)))))

(defcheck solution-551ebd4b
  #(apply * (range 1 (+ 1 %))))

(defcheck solution-55fefa24
  (fn [x]
    (reduce * (range 1 (+ x 1)))))

(defcheck solution-56647762
  #(->> % inc (range 1) (apply *)))

(defcheck solution-56fb298c
  #(->> % inc range rest (apply *)))

(defcheck solution-57622b3c
  (fn fac [n]
    (if (= n 1)
      1
      (* n (fac (- n 1))))))

(defcheck solution-582631b6
  (fn [n]
    (loop [s 1 n n]
      (if (< n 2)
        s
        (recur (* s n) (dec n))))))

(defcheck solution-585b01e3
  (fn fact [n]
    (if (= n 1)
      1
      (* n (fact (dec n))))))

(defcheck solution-58c3fa82
  (fn fact [x]
    (if (= x 0) 1 (* x (fact (dec x))))))

(defcheck solution-58d395ff
  (fn [x] (reduce * (range 1 (inc x)))))

(defcheck solution-590c565a
  (fn [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-5919fe9b
  (fn [n] (apply * (range 1 (+ n 1)))))

(defcheck solution-597ecf81
  #(reduce * 1 (range 1 (+ 1 %))))

(defcheck solution-5a17e397
  (fn fact [x]
    (if (<= x 1)
      1
      (* x (fact (dec x))))))

(defcheck solution-5a420a5b
  #(loop [cnt % acc 1]
     (if (or (zero? cnt) (== 1 cnt)) acc
                                     (recur (dec cnt) (* acc cnt)))))

(defcheck solution-5a5e0fdc
  (fn fac [n]
    (if (= n 1)
      1
      (* (fac (dec n)) n))))

(defcheck solution-5ae2cbcc
  (fn f
    [num]
    (if (= num 0)
      1
      (* num (f (dec num))))
    ))

(defcheck solution-5b1da20e
  (fn factorial [n]
    (if (zero? n)
      1
      (* n (factorial (dec n))))))

(defcheck solution-5b226151
  (fn fa [x] (if (= 1 x) 1 (* x (fa (dec x))))))

(defcheck solution-5b2ccfc0
  #(reduce * (reverse (range 1 (inc %)))))

(defcheck solution-5b7f44f1
  (fn [x]
    (loop [x x ret 1]
      (if (= 1 x)
        ret
        (recur (dec x) (* x ret))))))

(defcheck solution-5b833b73
  (fn fac [n] (reduce * (range 1 (inc n)))))

(defcheck solution-5c459bf2
  (fn fact [x]
    (if (< x 2)
      1
      (* x (fact (dec x))))))

(defcheck solution-5d7283fc
  (fn my-factorial [n] (if (= n 1) 1 (* n (my-factorial (dec n))))))

(defcheck solution-5dabaafe
  #(reduce * (take-while (partial > (inc %)) (iterate inc 1))))

(defcheck solution-5e6f1487
  #(reduce * (range 1 (inc %))))

(defcheck solution-5e94e2e2
  (fn factorial [n]
    (apply * (range 1 (inc n)))))

(defcheck solution-5ea96563
  (fn [v] (reduce #(* %1 %2) (range 1 (inc v)))))

(defcheck solution-5ff753ce
  (fn [n]
    (reduce * (range 1 (inc n)))
    ))

(defcheck solution-6000c1b
  (fn [n]
    (reduce * 1 (range 1 (inc n)))))

(defcheck solution-602e2234
  #(loop [cnt % acc 1]
     (if (zero? cnt)
       acc
       (recur (dec cnt) (* cnt acc))
       )))

(defcheck solution-6103c017
  (fn [u] (reduce * (range 1 (inc u)))
    ))

(defcheck solution-618432ae
  (fn fact [n]
    (loop [counter n result 1]
      (if (<= counter 1)
        result
        (recur (dec counter) (* result counter))))))

(defcheck solution-6287a28c
  (fn fact [n] (if (= 1 n) 1 (* n (fact (dec n))))))

(defcheck solution-629af5a5
  (fn [x] (reduce * (range 1 (inc x)))))

(defcheck solution-66a7ec07
  #(loop [current % result 1]
     (if (= 0 current)
       result
       (recur (dec current) (* result current)))))

(defcheck solution-684452ab
  (fn [n]
    (loop [i n sum 1] (if (= i 1) sum (recur (dec i) (* sum i))))))

(defcheck solution-68ce1d9a
  (fn [n]
    (letfn [(go [m]
              (if (< m 2) 1
                          (* m (go (dec m)))
                          ))]
      (go n))))

(defcheck solution-69580ea1
  #(reduce * % (next (range %))))

(defcheck solution-69760e53
  #(->> % inc range rest (reduce *)))

(defcheck solution-6abd9993
  ; first attempt
  ;(fn new-fact [i] (if (= i 1) 1 (* i (new-fact (dec i)))))

  #(apply * (range 1 (inc %))))

(defcheck solution-6b7bb4fc
  #(loop [a 1 n %]
     (if (= n 1) a
                 (recur (* a n) (dec n)))))

(defcheck solution-6e494c66
  (fn [x] (loop [curr x result 1] (if (= curr 1) result (recur (dec curr) (* result curr))))))

(defcheck solution-6ed76f34
  (fn [n]
    (loop [m n, result 1]
      (if (zero? m)
        result
        (recur (dec m) (* result m))))))

(defcheck solution-6ee7ded3
  (fn fac [n]
    (if (= 1 n) 1
                (* n (fac (dec n))))))

(defcheck solution-6f34e4a9
  (fn factorial-fun [x]
    (if (= x 1)
      1
      (* x (factorial-fun (- x 1))))))

(defcheck solution-704a7c68
  #(apply * (range 1 (inc %1))))

(defcheck solution-704f6b00
  (fn [n] (reduce #(* %1 %2) 1 (drop 1 (range (inc n))))))

(defcheck solution-7148bee7
  (fn [n] (reduce * 1 (rest (range (+ n 1))))))

(defcheck solution-715374c3
  (fn fac [k]
    (if (= k 1)
      1
      (* k (fac (dec k))))))

(defcheck solution-717d2a46
  (fn [x]
    (loop [x1 x, acc 1]
      (if (= 1 x1)
        acc
        (recur (dec x1) (* acc x1))))))

(defcheck solution-721937c5
  (fn fact [n]
    (loop [n n f 1] (if (= 1 n) f (recur (dec n) (* f n))))))

(defcheck solution-728fb4df
  #(if (= % 0) 1 (reduce * (range 1 (+ % 1)))))

(defcheck solution-73345285
  (fn fact [v] (if (= 0 v) 1 (* v (fact (dec v))))))

(defcheck solution-73a25daf
  (fn ! [n]
    (if (zero? n) 1
                  (* n (! (- n 1))))))

(defcheck solution-73b44713
  (fn [n] (apply * (map inc (range n)))))

(defcheck solution-746ea39d
  (fn [n] (reduce * 1 (range 1 (inc n)))))

(defcheck solution-746ef325
  (fn fac [n] (if (= n 0) 1 (* n (fac (dec n))))))

(defcheck solution-7523ad2f
  (fn exp [n]
    (loop [acc 1 n n]
      (if (= 1 n) acc
                  (recur (* n acc) (dec n))
                  )
      )
    ))

(defcheck solution-7537e031
  #(apply * (range 1 (+ % 1))))

(defcheck solution-75e7057f
  (fn [x]
    (loop [a 1
           b 1]
      (if (= x (dec b))
        a
        (recur (* a b) (inc b))))))

(defcheck solution-7618217a
  (fn [n]
    (loop [ret 1 idx 1]
      (if (> idx n)
        ret
        (recur (* ret idx) (inc idx))))))

(defcheck solution-779dbe3
  (fn [n]
    (reduce * n (rest (range n)))))

(defcheck solution-787a59a6
  (fn [i] (apply * (range 1 (inc i)))))

(defcheck solution-79b65950
  (comp (partial reduce *)
        (partial range 1)
        inc))

(defcheck solution-79bfdab3
  #(reduce * % (range 1 %)))

(defcheck solution-7a4ce57c
  #(reduce * (range % 0 -1)))

(defcheck solution-7ad8aca3
  (fn factorial [n]
    (if (< n 2)
      1
      (* n (factorial (dec n))))))

(defcheck solution-7ada2bf2
  (fn fac [x] (case x 0 1 (* (fac (- x 1)) x))))

(defcheck solution-7af3bd76
  (fn factorial- [n]
    "42. Write a function which calculates factorials."
    (reduce * (range 1 (inc n)))))

(defcheck solution-7b57e501
  (fn [end] (apply * (range 1 (inc end)))))

(defcheck solution-7b6c08fa
  (fn factorial [n]
    (if (= 1 n) 1
                (* n (factorial (dec n))))))

(defcheck solution-7bf11234
  (fn [n]
    (loop [x n acc 1]
      (if (<= x 1)
        acc
        (recur (dec x) (* acc x))))))

(defcheck solution-7ca9f9a0
  (fn fac
    [n]
    (apply * n (range 2 n))))

(defcheck solution-7d90ad12
  (fn fac [n]
    (loop [n   n
           res 1]
      (if (< n 2)
        res
        (recur (- n 1) (* res n))))))

(defcheck solution-7dd9c694
  #(loop [x % r 1] (if (= x 1) r (recur (dec x) (* r x)))))

(defcheck solution-7e8b85ef
  #(* (apply * (range 1 %)) %))

(defcheck solution-7f8260c1
  (fn [x] (reduce * 1 (range 1 (inc x)))))

(defcheck solution-802ec7ec
  (fn fac [x]
    (cond
      (= x 1) 1
      :else (* x (fac (- x 1)))
      )
    ))

(defcheck solution-81b10b3f
  (fn fac [n]
    (if (= 0 n)
      1
      (* n (fac (dec n))))))

(defcheck solution-829227e1
  #(loop [fac 1 n %1]
     (if (= 0 n) fac
                 (recur (* fac n) (dec n)
                   ))))

(defcheck solution-838fc7eb
  (fn recfac [n]
    (if (= n 1)
      1
      (* n (recfac (dec n))))))

(defcheck solution-842f4113
  (fn fact [n]
    (if (= n 1)
      n
      (* n (fact (dec n))))))

(defcheck solution-84349350
  (fn [n]
    (loop [cnt n acc 1]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (* acc cnt))))))

(defcheck solution-856fba7b
  (fn [n] (reduce * (range 1 (inc n)))))

(defcheck solution-8580ceda
  #(->> % range (map inc) (reduce *)))

(defcheck solution-85ae91bc
  (fn fact [n]
    (if (= n 1)
      1
      (* n (fact (dec n)))
      )))

(defcheck solution-85d71cb4
  (fn f [n] (if (zero? n) 1 (* n (f (dec n))))))

(defcheck solution-85f519fd
  (fn number42 [n]
    (reduce * 1 (range 1 (inc n)))))

(defcheck solution-862075bb
  (fn [n]
    (loop [n   n
           acc 1]
      (if (> n 1)
        (recur (dec n) (* acc n))
        acc))))

(defcheck solution-8669193a
  (fn [x]
    (loop [x   x
           acc 1]
      (if (< x 1)
        acc
        (recur (dec x) (* acc x))))))

(defcheck solution-8679c664
  (fn [n]
    (loop [r 1 i 2]
      (if (> i n)
        r
        (recur (* r i) (inc i))))))

(defcheck solution-87075b7
  (fn fac [num]
    (reduce * 1 (range 1 (inc num)))))

(defcheck solution-87835fb6
  (fn factorial [x]
    (condp = x
      1 1
      (* x (factorial (dec x))))))

(defcheck solution-87e2f7e7
  #(last (take % ((fn fact
                    ([] (fact 1 1))
                    ([f n] (lazy-seq (cons f (fact (* f (inc n)) (inc n))))))))))

(defcheck solution-8919e530
  (fn fac [n] (reduce #(* %1 %2) (range 1 (inc n)))))

(defcheck solution-89d24fcc
  #(loop [f % n (dec %)]
     (if (zero? n) f (recur (* f n) (dec n)))))

(defcheck solution-89e8ea0b
  (fn fac [x] (if (= x 1) 1 (* x (fac (- x 1))))))

(defcheck solution-89f30d72
  #(reduce * (range 1 (+ 1 %1))))

(defcheck solution-8b07d3d8
  #(reduce * (take-while pos? (iterate dec %))))

(defcheck solution-8b1e25b
  (fn [n]
    (loop [i n, acc n]
      (if (= i 1)
        acc
        (recur (dec i) (* acc (dec i)))))))

(defcheck solution-8b71254d
  (fn factorial-fun [x]
    (loop [i 1 a 1]
      (if (> i x)
        a
        (recur (inc i) (* a i))))))

(defcheck solution-8ba8b95a
  #(reduce * (range 1 (+ % 1))))

(defcheck solution-8dabc312
  (fn [n]
    (reduce * (range 1 (+ n 1)))))

(defcheck solution-8e67da3e
  #(->> % inc (range 1) (reduce *)))

(defcheck solution-8eb526af
  #(loop [n   %1
          acc 1]
     (if (= n 1)
       acc
       (recur (- n 1) (* acc n))
       )
     ))

(defcheck solution-8f263d2c
  #(apply * (take % (iterate (partial + 1) 1))))

(defcheck solution-8f8f24d5
  (fn fac [n] (loop [n         n
                     factorial 1]
                (if (= n 1)
                  factorial
                  (recur (dec n) (*' n factorial))))))

(defcheck solution-8fbf89b4
  #(loop [f 1 i 1] (if (> i %) f (recur (* i f) (inc i)))))

(defcheck solution-904e9393
  (fn [x] (reduce * (range 1 (+ x 1)))))

(defcheck solution-90a5476c
  #(reduce * (-> % inc range next)))

(defcheck solution-917466f9
  (fn fac [x]
    (if (< x 2) x
                (* x (fac (dec x))))))

(defcheck solution-925ff768
  (fn fac [x]
    (if (= x 1)
      1
      (* x (fac (dec x))))))

(defcheck solution-92fbbbf4
  (fn [x]
    ((fn [y res]
       (if (= y 0)
         res
         (recur (dec y) (* y res))
         )
       )
     x 1)
    ))

(defcheck solution-9303daf3
  (fn fact [mx]
    (reduce #(* %1 %2) 1 (range 1 (inc mx)))))

(defcheck solution-93e4ebb4
  (fn f [n] (if (= n 1) 1 (* n (f (- n 1))))))

(defcheck solution-946e15af
  (comp (partial apply *)
        (partial range 1)
        inc))

(defcheck solution-94efc920
  (fn fact [n]
    (if (= n 1)
      1
      (* n (fact (- n 1))))))

(defcheck solution-95496dc7
  (fn f [n]
    (if (= n 0) 1
                (* n (f (dec n))))))

(defcheck solution-95b5d3c4
  (fn [n]
    (reduce (fn [a b] (* a b)) (range 1 (inc n)))))

(defcheck solution-963b780
  (fn fact [n]
    (if (> n 1)
      (* n (fact (dec n)))
      1)))

(defcheck solution-963bb8ae
  (fn [x] (reduce #(* %1 %2) (map inc (range x)))))

(defcheck solution-96c6ed14
  (fn reduce-fact [x]
    (reduce (fn [a b] (* a b)) 1 (range 1 (inc x)))))

(defcheck solution-96e8d9
  (fn [n]
    (loop [n   n
           acc 1]
      (if (zero? n)
        acc
        (recur (dec n) (* n acc))))))

(defcheck solution-96ecfe98
  #(loop [p 1
          m %]
     (if
      (= m 1)
       p
       (recur (* p m) (- m 1)))))

(defcheck solution-9715f95
  (fn ff [x]
    (if (= x 1) 1 (* (ff (dec x)) x))))

(defcheck solution-980215ca
  (fn fact--range
    [n] {:pre [(integer? n), (not (neg? n))]}
    (apply *' (range 1 (inc n)))))

(defcheck solution-98b09911
  #(->> %1 (iterate dec) (take-while (partial < 0)) (apply *)))

(defcheck solution-98c38810
  (fn my-factorial
    [n]
    (if (= n 1) 1N
                (* n (my-factorial (dec n))))))

(defcheck solution-99a95d74
  (fn [to]
    (reduce * (take-while #(<= % to)
                (iterate inc 1)))))

(defcheck solution-99abe103
  (fn [x]
    (loop [a x ans 1]
      (if (= a 1) ans (recur (dec a) (* a ans))))))

(defcheck solution-9bd659d
  (fn fact [i] (if (= i 1) 1 (* i (fact (dec i))))))

(defcheck solution-9bdcd680
  (fn fic_n [num]
    (nth
      ((fn iter [s n]
         (let [sum (* s n)]
           (lazy-seq (cons
                       sum
                       (iter sum (inc n))
                       )
             )
           )
         )
       1 1
       ) (- num 1)
      )
    ))

(defcheck solution-9c5e373a
  (fn factorial [n]
    (if (= 0 n)
      1
      (* n (factorial (dec n))))
    (reduce * (map inc (range n)))))

(defcheck solution-9c5ee263
  ; #(apply * (map inc (range %)))
  #(reduce * (map inc (range %))))

(defcheck solution-9d2a0123
  (fn [x]
    (loop [r      (dec x)
           result x]
      (if (<= r 1)
        result
        (recur (dec r) (* result r))))))

(defcheck solution-9d7cddae
  (comp (partial reduce *) rest range inc))

(defcheck solution-9db69891
  (fn fac [n]
    (loop [x 1 mult n]
      (if (zero? mult)
        x
        (recur (* x mult) (dec mult))))))

(defcheck solution-9e17b5c4
  (fn [n] (loop [i 1 r 1] (if (= i n) (* r i) (recur (inc i) (* r i))))))

(defcheck solution-9e28a95c
  (fn fact [n]
    (loop [i n, r 1]
      (if (<= i 0)
        r
        (recur (dec i) (* r i))))))

(defcheck solution-9ee04579
  (fn [x]
    (loop [n x f 1]
      (if (= n 1)
        f
        (recur (dec n) (* f n))))))

(defcheck solution-9f248dcc
  #(loop [x % result %] (if (<= x 1) result (recur (dec x) (* result (dec x))))))

(defcheck solution-9fcf39b3
  #(* % (reduce * (range 1 %))))

(defcheck solution-a02b5624
  (fn faci [n]
    (loop [i 0 f 1]
      (if (= i n)
        f
        (recur (inc i) (* f (inc i)))))))

(defcheck solution-a16d13d0
  #(apply * % (range 1 %)))

(defcheck solution-a17dc0e3
  #(loop [n 1 t 1]
     (if (> n %)
       t
       (recur (inc n) (* t n)))))

(defcheck solution-a21e3372
  (fn [n]
    (loop [tot 1
           i   1]
      (if (> i n) tot
                  (recur (* tot i) (inc i))))))

(defcheck solution-a30a28ad
  (fn [n]
    (loop [i n res 1]
      (if (> i 0)
        (recur (dec i) (* res i))
        res
        )
      )
    ))

(defcheck solution-a54da707
  (fn ! [n]
    (if (= n 1)
      1
      (* n (! (- n 1)))
      )
    ))

(defcheck solution-a565c078
  #(loop [x     1
          accum x]
     (if (= x %1)
       accum
       (recur (inc x)
         (* accum (inc x))))))

(defcheck solution-a5e5bb89
  #(loop [n %1 s 1] (if (<= n 1)
                      s
                      (recur (dec n) (* s n)))))

(defcheck solution-a608f1ee
  (fn f [n] (if (< n 2) 1 (* n (f (dec n))))))

(defcheck solution-a62c00be
  (fn fact [n]
    (apply * (range 1 (inc n)))))

(defcheck solution-a660691
  #(reduce * 1 (range 1 (inc %))))

(defcheck solution-a68264d8
  #(apply * (take % (iterate inc 1))))

(defcheck solution-a70e184d
  (fn [n] ((fn [a n] (if (<= n 1) a (recur (* a n) (dec n)))) 1 n)))

(defcheck solution-a81885d1
  (fn [n]
    (reduce * (range 1 (+ n 1)))))

(defcheck solution-a840978b
  #(loop [x   %
          acc 1]
     (if (= x 0)
       acc
       (recur (- x 1) (* acc x))
       )))

(defcheck solution-a8fbfbe9
  (fn f [x]
    (if (= x 0) 1
                (* x (f (dec x))))))

(defcheck solution-a99f6d99
  (fn ! [x] (if (zero? x) 1 (* x (! (dec x))))))

(defcheck solution-a9aaecb6
  (fn f [x] (if (= 1 x) x (* x (f (dec x))))))

(defcheck solution-aa9180fb
  (fn [n] (reduce * (range 1 (inc n)))))

(defcheck solution-aab3a542
  (fn factorial [n]
    (if (or (zero? n)
            (= n 1))
      1
      (* n (factorial (dec n))))))

(defcheck solution-aacb2a37
  #(* % (apply * (range 1 %))))

(defcheck solution-ab475239
  (fn fac [n]
    (if (= n 1) n (* n (fac (dec n))))))

(defcheck solution-ab527e49
  #_(fn f [n] (if (= 0 n) 1 (* n (f (dec n)))))

  #_#(reduce * (range 1 (inc %)))

  {1 1 3 6 5 120 8 40320})

(defcheck solution-abc1fe50
  #(reduce * (range 2 (inc %))))

(defcheck solution-abd9e366
  #(->> (range 1 (inc %))
     (apply *)))

(defcheck solution-ac443aed
  (fn fac [n]
    (if (= n 0) 1 (* n (fac (dec n))))))

(defcheck solution-ac4d5c41
  (fn f [x]
    (if (<= x 1)
      1
      (* x (f (dec x)))
      )
    ))

(defcheck solution-added339
  (fn [x]
    (loop [y x z 1]
      (if (= 1 y)
        z
        (recur (dec y) (* z y))))))

(defcheck solution-adf8fb99
  (fn factorial [x]
    (if (= x 1)
      1
      (* x (factorial (dec x)))
      )
    ))

(defcheck solution-ae61a3a2
  (fn [n] (reduce #(* %1 %2) (map inc (range n)))))

(defcheck solution-ae65b819
  #(reduce (fn [r e] (* r e)) 1 (range 1 (inc %))))

(defcheck solution-ae77eff9
  (fn myfac [x]
    (if (<= x 1) 1 (* x (myfac (- x 1))))))

(defcheck solution-af341403
  #(loop [x % fac 1]
     (if (= x 1)
       fac
       (recur (dec x) (* x fac)))))

(defcheck solution-af858c6
  (fn fac [n]
    (cond
      (or (= n 0) (= n 1)) 1
      :else (* n (fac (dec n))))))

(defcheck solution-b010c320
  (fn [n]
    (loop [i 1, acc 1]
      (cond (> i n) acc
            :else (recur (inc i) (* acc i))))))

(defcheck solution-b039a1da
  #(reduce * (take-while (partial < 0) (iterate dec %))))

(defcheck solution-b04c4586
  (fn f [x] (if (= 0 x) 1 (* x (f (- x 1))))))

(defcheck solution-b0e6aeba
  (fn factorial
    [n]
    (loop [result 1
           i      n]
      (if (<= i 1)
        result
        (recur (* result i) (dec i))))))

(defcheck solution-b1cb62d1
  (fn [x]
    (first (last (take (inc x)
                   (iterate
                     (fn [[a b]] [(*' a b) (inc b)])
                     [1 1]))))))

(defcheck solution-b207b93c
  ;(fn fact [n]
  ;  (if (or (= n 1) (= n 0))
  ;    1
  ;    (* n (fact (dec n)))))

  #(apply * (range 1 (inc %))))

(defcheck solution-b2176848
  (fn [n] (reduce * (take n (iterate inc 1)))))

(defcheck solution-b27d3864
  (fn [n] (loop [n n acc 1]
            (if (<= n 1) acc
                         (recur (dec n) (* n acc))))))

(defcheck solution-b2cf4db2
  (fn [n] (let [f (fn ff [x] (if (< x 2) 1 (* x (ff (dec x)))))] (f n))))

(defcheck solution-b3902074
  (fn mfactorical [n]
    (if (< n 2)
      1
      (* n (mfactorical (dec n))))))

(defcheck solution-b3ea518
  (fn factorial [n]
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(defcheck solution-b4184c29
  #(reduce * (take % (iterate dec %))))

(defcheck solution-b44b98a
  (fn [n]
    (loop [res 1 cnt n]
      (cond (= cnt 0)
            res
            :else
            (recur (* res cnt) (dec cnt))))))

(defcheck solution-b4603dd4
  (fn [n]
    (loop [n n rv 1]
      (if (#{0 1} n)
        (* 1 rv)
        (recur (dec n) (* n rv))))))

(defcheck solution-b496fe0e
  #(apply * (range 2 (inc %))))

(defcheck solution-b500bafc
  (fn fact [n]
    (if (> n 0) (* n (fact (dec n))) 1)))

(defcheck solution-b57e641c
  #(loop [n % fact 1] (if (> n 0) (recur (dec n) (* fact n)) fact)))

(defcheck solution-b5ace1c5
  (fn fac
    [n]
    (if (< n 2)
      n
      (* n (fac (- n 1)))
      )))

(defcheck solution-b5d96446
  #(reduce * (rest (range (+ % 1)))))

(defcheck solution-b60db8ac
  (fn fac [x]
    (if (zero? x)
      1
      (* x (fac (- x 1))))))

(defcheck solution-b60f80e8
  #(reduce * (range %1 0 -1)))

(defcheck solution-b6417f12
  #(loop [n %1 prod 1]
     (if (zero? n) prod
                   (recur (dec n) (* n prod)))))

(defcheck solution-b72577ae
  (fn ! [n] (if (> n 1) (* n (! (- n 1))) 1)))

(defcheck solution-b7419f03
  (fn fact [x]
    (if (= x 1) 1 (* x (fact (dec x))))))

(defcheck solution-b75be092
  (fn [x] (apply * x (range 1 x))))

(defcheck solution-b7b873b5
  (fn f [x] (if (= x 1) 1 (* x (f (dec x))))))

(defcheck solution-b7fb2700
  (fn [x] (reduce #(* %1 (+ 1 %2)) 1 (reverse (range x)))))

(defcheck solution-b81cd779
  (fn [x]
    (loop [x   x
           res 1]
      (if (= x 1)
        res
        (recur (dec x) (* res x))))))

(defcheck solution-b880d5a1
  (fn fac [x]
    (if (> x 0)
      (* x (fac (dec x)))
      1
      )
    ))

(defcheck solution-b912289a
  #(->> (iterate inc 1)
     (take %)
     (reduce *)))

(defcheck solution-b92f3d85
  (fn f [x] (if (< x 2) 1 (* x (f (- x 1))))))

(defcheck solution-b99c4229
  #(->> (iterate inc 1)
     (take %)
     (reduce *)))

(defcheck solution-b9cabfd3
  (fn fac [n] {:pre [(>= n 0)] :post [(integer? %)]}
    (cond (<= n 1) 1
          (<= n 20) (* n (fac (- n 1)))
          :else (reduce *' (vec (range 1 (inc n)))))))

(defcheck solution-ba482d94
  (fn f [n] (if (<= n 1) 1 (* n (f (- n 1))))))

(defcheck solution-bb2b72f8
  (fn factorial [n]
    (if (zero? n)
      1
      (* n (factorial (dec n))))))

(defcheck solution-bb44a54d
  (fn fac [n] (if (> n 1) (* n (fac (dec n))) 1)))

(defcheck solution-bb8a2b16
  (fn factorial [n]
    (if (zero? n) 1 (* n (factorial (dec n))))))

(defcheck solution-bd14b94a
  #(apply * % (range 2 %)))

(defcheck solution-bd543598
  (fn my-factorial
    [num]
    (if (= num 1)
      1
      (* num (my-factorial (dec num))))))

(defcheck solution-bdada22c
  (fn factorial
    [x]
    (reduce * (range 1 (+ x 1)))))

(defcheck solution-bde21012
  #(apply * (take-while pos? (iterate dec %))))

(defcheck solution-bde24972
  (fn f [x]
    (if (> x 1)
      (* (f (- x 1)) x)
      1
      )
    ))

(defcheck solution-bdeb4c84
  (fn fact [n]
    (loop [n n acc 1] (if (= n 1) acc (recur (dec n) (* acc n))))))

(defcheck solution-be1c2a0b
  #(reduce * (rest (seq (range (inc %))))))

(defcheck solution-be2f472f
  {1 1 3 6 5 120 8 40320})

(defcheck solution-be4636e5
  (fn [n] (reduce * (range 1 (+ n 1)))))

(defcheck solution-becf862e
  (fn [num]
    (reduce #(* %1 %2) 1 (range 1 (inc num)))))

(defcheck solution-c08dc2d2
  (fn my-fact [num]
    (if (= num 1)
      1
      (* num (my-fact (dec num))))))

(defcheck solution-c0b32cb4
  (fn [n] (apply * (range 1 (inc n)))))

(defcheck solution-c19b6d79
  (fn [n] (reduce #(* %1 %2) (range 1 (inc n)))))

(defcheck solution-c270f92c
  (fn factorial2
    [number]
    (reduce * (range 1N (inc number)))))

(defcheck solution-c2d85bd1
  #(reduce * % (range 2 %)))

(defcheck solution-c3af576d
  (fn [n]
    (loop [mul 1
           n   n]
      (if (= n 1)
        mul
        (recur (* mul n) (dec n))))))

(defcheck solution-c5a5fe78
  (fn fact [n]
    (reduce * (conj (range 1 n) n))))

(defcheck solution-c687186
  (fn factorial [from]
    (reduce * (range 1 (inc from)))))

(defcheck solution-c8d1a727
  (fn fac [x]
    (if (= x 1)
      x
      (* x (fac (- x 1))))))

(defcheck solution-c8e70577
  (fn fact [n]
    (if (zero? n) 1
                  (* n (fact (- n 1))))))

(defcheck solution-c91c31b8
  (fn fac [n]
    (if (= n 1) 1
                (* n (fac (dec n))))))

(defcheck solution-c9ace0c2
  (fn [n]
    (loop [n n result 1]
      (if (zero? n) result
                    (recur (dec n) (* n result))))))

(defcheck solution-c9cb942c
  (fn [n]
    (loop [i n ret 1]
      (if (> i 0)
        (recur (dec i) (* ret i))
        ret))))

(defcheck solution-c9d239e3
  (fn [n]
    (->> (range n 0 -1)
      (apply *)))

  #_(fn [n]
      (loop [acc 1, i n]
        (if (<= i 1)
          acc
          (recur (* acc i) (dec i))))))

(defcheck solution-cc63024c
  #(reduce * (take % (iterate inc 1))))

(defcheck solution-cc909ead
  (fn fact [n] (loop [x n, result 1] (if (= x 1) result (recur (dec x) (* result x))))))

(defcheck solution-cce279d0
  (fn f [n] (if (= n 1) 1 (* n (f (- n 1))))))

(defcheck solution-cd4521fd
  (fn F1 [x] (if (= x 1) 1 (* (F1 (dec x)) x))))

(defcheck solution-cd6d850d
  (fn [n]
    (loop [cnt n acc 1]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (* cnt acc))))))

(defcheck solution-ce80697c
  (fn ! [n]
    (loop [i n acc 1]
      (if (zero? i)
        acc
        (recur (dec i) (* acc i))))))

(defcheck solution-d118c98
  #(apply * (drop 1 (range (inc %)))))

(defcheck solution-d1ac48a7
  (fn fact [n]
    (if (= 0 n) 1
                (* n (fact (dec n))))))

(defcheck solution-d27fd6e5
  #(reduce * (range 1 (inc %))))

(defcheck solution-d28db284
  #(reduce *' (range 2 (+ % 1))))

(defcheck solution-d29c4446
  (fn [x]
    (reduce * (range 1 (inc x)))))

(defcheck solution-d2c99e9d
  (fn fac [n] (if (= n 1)
                1
                (* n (fac (- n 1))))))

(defcheck solution-d2cd3768
  (fn f [n]
    (if (= n 1)
      1
      (* n (f (dec n))))))

(defcheck solution-d3e55a05
  (fn [n]
    (reduce #(* %1 %2) 1 (range 1 (inc n)))))

(defcheck solution-d441d972
  #(reduce * (map inc (range %))))

(defcheck solution-d54fb2c8
  (fn fact [n] (reduce * (map inc (range n)))))

(defcheck solution-d56b0503
  (fn [num]
    (loop [n num total 1 ret 1]
      (if (= num total)
        (* ret total)
        (recur
          num
          (+ total 1)
          (* ret total)
          )
        )
      )
    ))

(defcheck solution-d7097a3e
  (fn fact [n]
    (if (= 1 n)
      1
      (* n (fact (dec n))))))

(defcheck solution-d7a47722
  (fn fact [n]
    (cond
      (= n 0) 1
      (= n 1) 1
      :else (* n (fact (dec n)))
      )
    ))

(defcheck solution-d7db7d6a
  #(reduce (fn [x y] (* x y)) (rest (range (inc %)))))

(defcheck solution-d8234aef
  (fn [n]
    (nth
      ((fn lzfact [a b]
         (cons a (lazy-seq (lzfact (* a b) (inc b)))))
       1 1)
      n)))

(defcheck solution-d8eebef7
  (comp (partial apply *) (partial range 1) inc))

(defcheck solution-d9691bb
  (fn factorial [n] (reduce * (range 1 (+ n 1)))))

(defcheck solution-d99e014a
  (fn [n]
    (reduce * (take n (iterate inc 1)))))

(defcheck solution-da9f8b7e
  (comp (partial reduce *) (partial map inc) range))

(defcheck solution-daa2efd0
  (fn fact [n]
    (if (= n 1) 1
                (* n (fact (dec n)))
                )
    ))

(defcheck solution-daaabcf7
  (fn fac [x]
    (if (= x 1)
      1
      (* x (fac (- x 1)))
      )
    ))

(defcheck solution-db7f1c9d
  (fn [n] (loop [cnt 1 fac 1] (do (if (<= cnt n) (recur (inc cnt) (* fac cnt)) fac)))))

(defcheck solution-dd90ebce
  #(loop [n    %
          fact 1]
     (if (= 1 n)
       fact
       (recur (dec n) (* fact n)))))

(defcheck solution-ddc02e2c
  (apply comp (map partial [reduce range +] [* 1 1])))

(defcheck solution-de65df38
  #(apply * (range 2 (+ 1 %))))

(defcheck solution-df4aced6
  (fn factorial [n]
    (apply * (map inc (range n)))))

(defcheck solution-df9870cf
  (fn [n] (apply * (range 1 (inc n)))))

(defcheck solution-dff5cc7f
  (fn [x] (reduce * (take-while #(> % 0) (iterate dec x)))))

(defcheck solution-e021d029
  (fn f [x] (if (= x 1) 1 (* x (f (- x 1))))))

(defcheck solution-e0770ce2
  (fn [arg]
    (loop [x arg ans 1]
      (if (= x 0)
        ans
        (recur (dec x) (* ans x))))))

(defcheck solution-e0956825
  (fn fac [n]
    (loop [x 1
           i 2]
      (if (> i n)
        x
        (recur (* x i)
          (inc i))))))

(defcheck solution-e095bac2
  (fn silnia [n]
    (if (= n 1)
      1
      (* n (silnia (- n 1))))))

(defcheck solution-e22b1a21
  (fn f [x]
    (if (<= x 1)
      1
      (* x (f (dec x))))))

(defcheck solution-e326392a
  #(loop [result 1
          iter   1]
     (if (> iter %)
       result
       (recur (* result iter) (inc iter)))))

(defcheck solution-e442ea82
  (fn ! [x] (if (= x 0) 1 (* x (! (dec x))))))

(defcheck solution-e4c918fd
  (fn [n]
    (loop [i n r 1]
      (if (= i 0)
        r
        (recur (dec i) (* i r))))))

(defcheck solution-e5180d9a
  (fn [n]
    (loop [ret 1
           n   n]
      (if (zero? n)
        ret
        (recur (* ret n) (dec n))))))

(defcheck solution-e53f7e43
  (fn [n] (reduce * 1 (range 1 (+ n 1)))))

(defcheck solution-e631629c
  (fn f [x]
    (if (= 1 x)
      1
      (* x (f (dec x))))))

(defcheck solution-e6957656
  #(reduce
     (fn [s i] (* s i))
     1
     (range 1 (inc %1))))

(defcheck solution-e6f06016
  (fn fac [x]
    (cond
      (= 1 x) 1
      :else (* x (fac (dec x))))))

(defcheck solution-e73a4473
  (fn p42-factorial
    [x]
    (if (> 2 x)
      1
      (* x (p42-factorial (dec x))))))

(defcheck solution-e7768590
  (fn f
    ([n] (f n 1))
    ([n a]
     (if (< n 1)
       a
       (f (dec n) (* a n))))))

(defcheck solution-e77f2d43
  (fn [n]
    (letfn [(hf [result i]
              (if (< i 2)
                result
                (recur (* i result) (dec i))))]
      (hf 1 n))))

(defcheck solution-e7d2f055
  (fn fact [x]
    (reduce * (reductions + (repeat x 1)))))

(defcheck solution-e9000cdd
  #(reduce * 1 (range 1 (+ % 1))))

(defcheck solution-e938a0e7
  (fn [x]
    ((fn fact [x acc]
       (if (zero? x) acc
                     (recur (- x 1) (* acc x)))) x 1)))

(defcheck solution-e9fd844d
  (fn [n0]
    (loop [n   n0
           res 1]
      (if (= 1 n)
        res
        (recur (dec n) (* res n))))))

(defcheck solution-ea2f6f3f
  (fn fac [n]
    (loop [cnt n
           acc 1]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (* acc cnt))))))

(defcheck solution-ebd318a8
  (fn peu [x] (if (= x 1) 1 (* x (peu (dec x))))))

(defcheck solution-ec8a0ef2
  (fn fact [x]
    (loop [n x f 1]
      (if (= n 1)
        f
        (recur (dec n) (* f n))))))

(defcheck solution-ed56f5c
  (fn [x] (reduce #(* %1 %2) 1 (range 1 (inc x)))))

(defcheck solution-ede90610
  (fn [x] (reduce * (map inc (range x)))))

(defcheck solution-ee77eee7
  #(reduce * (rest (range (inc %)))))

(defcheck solution-f0395d79
  (fn [n]
    (loop [c n
           f 1]
      (if (zero? c) f
                    (recur (dec c) (* f c))))))

(defcheck solution-f0416e55
  #(apply * (map inc (range %1))))

(defcheck solution-f0e96c63
  #(apply * (range % 0 -1)))

(defcheck solution-f0eda309
  (fn [x] (apply * (range 1 (inc x)))))

(defcheck solution-f11da1ac
  #(reduce * (range 1 (inc %))))

(defcheck solution-f163d260
  (fn ! [x] (if (= x 1) 1 (* x (! (dec x))))))

(defcheck solution-f23a271a
  #(->> % (inc) (range 1) (reduce *)))

(defcheck solution-f3b21a32
  (fn fact [n]
    (if (= n 1) 1 (* n (fact (- n 1))))))

(defcheck solution-f3fbf751
  (fn f [n]
    (if (= n 1)
      1
      (* n (f (dec n))))))

(defcheck solution-f3fcf53f
  (fn fact [n]
    (if (< n 2) 1
                (* n (fact (dec n)))
                )
    ))

(defcheck solution-f581f446
  (fn [n] (loop [m n acc 1] (if (= m 1) acc (recur (- m 1) (* acc m))))))

(defcheck solution-f5e00ef
  #(apply * (take % (iterate inc 1))))

(defcheck solution-f6464ab8
  (fn fac [x] (if (= x 1) 1 (* x (fac (dec x))))))

(defcheck solution-f6a64da
  #(apply * (take % (iterate dec %))))

(defcheck solution-f6da0f5d
  #(apply * (range % 1 -1)))

(defcheck solution-f76ff769
  (fn [n]
    (loop [v 1 x n]
      (if (> x 0)
        (recur (* v x) (dec x))
        v))))

(defcheck solution-f7a3abf5
  (fn [n] (reduce #(* % %2) (rest (range (inc n))))))

(defcheck solution-f801ffcb
  (fn [fact] (reduce * (range 1 (inc fact)))))

(defcheck solution-f81e2a4a
  (fn [n]
    (apply * (range 1 (inc n)))))

(defcheck solution-f8d8269d
  #(apply * % (range 1 %)))

(defcheck solution-f8eecaae
  (fn factorial [n]
    (reduce * (range 1 (inc n)))))

(defcheck solution-fa8207e
  (fn [to] (loop [fr 1 acc 1] (if (= fr (+ 1 to)) acc (recur (inc fr) (* acc fr))))))

(defcheck solution-fad510cd
  (fn fctrl
    [x]
    (if (= x 1)
      1
      (* x (fctrl (dec x))))))

(defcheck solution-fb16dfb0
  (fn iter
    ([n] (iter 1 n))
    ([a n] (if (= n 0) a
                       (recur (* a n) (dec n))))))

(defcheck solution-fb2b6e46
  (fn [x]
    (reduce * (range 1 (inc x)))))

(defcheck solution-fc6e5a29
  (fn fact [n]
    (if (zero? n) 1 (* n (fact (- n 1))))))

(defcheck solution-fca14183
  #(reduce * (range 1 (inc %))))

(defcheck solution-fd66496c
  (fn fact [n] (if (= 0 n) 1 (* n (fact (- n 1))))))

(defcheck solution-fdbebb5b
  (fn fact [n] (if (<= n 1) 1 (* n (fact (dec n))))))

(defcheck solution-ffff2a21
  (fn factorial
    ([n] (factorial n 1))
    ([n result]
     (if (= n 1)
       result
       (recur (dec n) (* n result))))))
