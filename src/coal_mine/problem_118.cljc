(ns coal-mine.problem-118
  (:require [coal-mine.checks :refer [defcheck-118] :rename {defcheck-118 defcheck}]
            [clojure.test]))

(defcheck solution-1048eb49
  (fn mymap [f coll]
    (when (not (empty? coll))
      (lazy-seq (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-10a12581
  (fn my-map [pred coll]
    (let [step (fn [p c]
                 (when-let [s (seq c)]                      ; when (seq c) is true, let s = (seq c), else return nil
                   (cons (pred (first s))
                     (my-map p (rest s)))))]
      (lazy-seq (step pred coll)))))

(defcheck solution-10ae3e56
  (fn my-map [f xs]
    (if (empty? xs)
      nil
      (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-1141a0be
  (fn my-map [f se]
    (lazy-seq
      (if (nil? (seq se))
        ()
        (cons (f (first se))
          (my-map f (next se)))))))

(defcheck solution-116e525d
  (fn mop [f s]
    (lazy-seq
      (when (seq s)
        (cons (f (first s)) (mop f (rest s)))))))

(defcheck solution-11ab7799
  (fn newmap [f x]
    (if (empty? x)
      '()
      (lazy-seq (cons (f (first x)) (newmap f (rest x)))))))

(defcheck solution-11c8e44a
  (fn m [f vals]
    (when (seq vals)
      (lazy-seq (cons (f (first vals)) (m f (rest vals)))))))

(defcheck solution-11d67ba6
  (fn mp [f l]
    (if (empty? l)
      l
      (cons (f (first l)) (lazy-seq (mp f (rest l)))))))

(defcheck solution-11d81e22
  (fn mapf [f coll]
    (if (empty? coll)
      coll
      (lazy-seq (cons (f (first coll)) (mapf f (rest coll)))))))

(defcheck solution-12226b99
  (fn m [f c]
    (lazy-seq
      (when-let [o (first c)]
        (cons (f o) (m f (rest c)))))))

(defcheck solution-12a756b5
  (fn [f coll]
    (letfn
     [(step [f coll]
        (if coll (lazy-seq (cons (f (first coll)) (step f (next coll))))))]
      (step f coll))))

(defcheck solution-138cbfe3
  (fn m [f coll]
    (lazy-seq
      (if (= coll '())
        '()
        (cons (f (first coll)) (m f (rest coll)))))))

(defcheck solution-13ff1758
  (fn m [f c]
    (when-let [s (seq c)]
      (lazy-seq (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-14a232a
  (letfn [(mymap [f s]
            (lazy-seq (when (seq s)
                        (cons (f (first s))
                          (mymap f (rest s))))))]
    mymap))

(defcheck solution-1536fa89
  (fn maplazy [f s]
    (lazy-seq (if (empty? s)
                nil
                (cons (f (first s))
                  (maplazy f (rest s)))))))

(defcheck solution-1553bd8a
  (fn m [f s]
    (if (empty? s)
      []
      (lazy-seq
        (cons (f (first s))
          (m f (rest s))))
      )
    ))

(defcheck solution-155a6a08
  (fn m [f s]
    (if s
      (lazy-seq (cons (f (first s)) (m f (next s)))))))

(defcheck solution-157aeb64
  (fn rp [f x]
    (if (empty? x)
      []
      (cons (f (first x)) (lazy-seq (rp f (rest x)))))))

(defcheck solution-15d62f2e
  (fn mmap [f target]
    (if (empty? target)
      '()
      (cons (f (first target)) (lazy-seq (mmap f (rest target)))))))

(defcheck solution-15da95f6
  (fn custom-map [f coll]
    (if (not (empty? coll))
      (lazy-seq (cons (f (first coll)) (custom-map f (rest coll))))
      )
    ))

(defcheck solution-15f52932
  (fn nmap [f xs]
    (if (empty? xs)
      '()
      (lazy-seq (cons (f (first xs)) (nmap f (rest xs)))))))

(defcheck solution-16063df
  (fn foo [f s]
    (if (empty? s)
      '()
      (lazy-seq (cons (f (first s)) (foo f (rest s)))))))

(defcheck solution-160dadb6
  (fn mymap [f coll]
    (when-let [c coll]
      (cons (f (first c)) (lazy-seq (mymap f (next c)))))))

(defcheck solution-16943e53
  (fn m [f vs]
    (lazy-seq
      (if (empty? vs) '()
                      (cons (f (first vs))
                        (m f (rest vs)))))))

(defcheck solution-16d11925
  (fn rmap [f s]
    (if (empty? s)
      '()
      (cons (f (first s))
        (lazy-seq (rmap f (rest s)))))))

(defcheck solution-1710a4a6
  (fn m [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-1718d3b
  (fn m [f s]
    (lazy-seq
      (if (seq s)
        (cons (f (first s))
          (m f (rest s)))))))

(defcheck solution-17ab945c
  (fn yo [f s]
    (if (empty? s)
      []
      (let [[el & remain] s]
        (cons (f el) (lazy-seq (yo f remain)))))))

(defcheck solution-181905b8
  (fn [f1 coll1]
    (letfn [(mymap [f coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (cons (f (first s)) (mymap f (rest s))))))]
      (mymap f1 coll1))))

(defcheck solution-18828f64
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-1974ce80
  (fn mymap [f coll]
    (when (seq coll)
      (lazy-seq
        (cons
          (f (first coll))
          (mymap f (rest coll)))))))

(defcheck solution-197adb67
  #(rest (reductions (fn [a b] (% b)) 0 %2)))

(defcheck solution-19c4098d
  (fn foo [f xs] (lazy-seq (if-let [x (first xs)]
                             (cons (f x) (foo f (rest xs)))
                             nil))))

(defcheck solution-1a2c76c6
  (fn my-map [f [head & rest :as all]]
    (if (empty? all)
      nil
      (cons (f head) (lazy-seq (my-map f rest))))))

(defcheck solution-1a4810b1
  (fn m [f [h & t]]
    (if h
      (cons (f h) (lazy-seq (m f t))))))

(defcheck solution-1aafa275
  (fn mm [f l] (lazy-seq (when-let [s (seq l)] (cons (f (first l)) (mm f (rest l)))))))

(defcheck solution-1adf8c8c
  (fn __ [f s]
    (if (empty? s) '()
                   (cons (f (first s))
                     (lazy-seq (__ f (rest s)))))))

(defcheck solution-1af7afa7
  (fn m [f s]
    (if (not (seq s))
      nil
      (cons (f (first s))
        (lazy-seq (m f (rest s)))))))

(defcheck solution-1ba91a30
  (fn maps [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (maps f (rest s)))))))

(defcheck solution-1bf4c63f
  (fn ! [f coll] (lazy-seq (when-let [s (seq coll)] (cons (f (first s))
                                                      (! f (rest s)))))))

(defcheck solution-1c33bef9
  (fn m [f s]
    (when-let [e (first s)]
      (lazy-seq
        (cons (f e) (m f (rest s)))))))

(defcheck solution-1c56d001
  (fn m [f s]
    (when-let [[x & xs] (seq s)]
      (lazy-seq (cons (f x) (m f xs))))))

(defcheck solution-1c9f43df
  (fn map' [f coll]
    (when (seq coll)
      (cons (f (first coll))
        (lazy-seq (map' f (rest coll)))))))

(defcheck solution-1cdc4d3a
  (fn foo [f xs]
    (lazy-seq
      (when (seq xs)
        (cons (f (first xs))
          (foo f (rest xs)))))))

(defcheck solution-1d5b1d3c
  (fn map' [f xs]
    (if (empty? xs) '()
                    (cons (f (first xs)) (lazy-seq (map' f (rest xs)))))))

(defcheck solution-1d65655a
  (fn mp [f s]
    (if (empty? s)
      '()
      (cons (f (first s)) (lazy-seq (mp f (rest s))))
      )
    ))

(defcheck solution-1d7d80a4
  (fn mymap [f coll]
    (lazy-seq (when (seq coll) (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-1e0a3aad
  (fn go [f xs]
    (if-let [[x & more] (seq xs)]
      (cons (f x) (lazy-seq (go f more)))
      '())))

(defcheck solution-1f008a54
  (fn mymap [f [h & t]]
    (if (nil? t)
      (if (nil? h)
        (lazy-seq)
        (lazy-seq (list (f h))))
      (lazy-seq (cons (f h) (mymap f t))))))

(defcheck solution-1f503ac6
  (fn f [x y]
    (when (not (empty? y))
      (lazy-cat
        [(x (first y))]
        (f x (rest y))))))

(defcheck solution-1fcb2390
  (fn rec [f [x & xs]] (lazy-seq (cons (f x) (when xs (rec f xs))))))

(defcheck solution-2003a1a0
  (fn mymap [f col]
    (lazy-seq
      (when col
        (cons (f (first col)) (mymap f (next col)))))))

(defcheck solution-206bbd48
  (fn f [fun coll]
    (lazy-seq
      (if (empty? coll)
        '()
        (cons (fun (first coll))
          (f fun (rest coll)))))))

(defcheck solution-20d40ec
  (fn ! [f coll]
    (lazy-seq
      (if coll
        (cons (f (first coll)) (! f (next coll)))))))

(defcheck solution-20ff6c50
  (fn my-map
    ([f coll] (if (empty? coll) coll
                                (lazy-seq
                                  (cons (f (first coll)) (my-map f (rest coll))))))))

(defcheck solution-2130eba8
  (fn ff
    [f s] (if (not (empty? s)) (lazy-seq (cons (f (first s)) (ff f (rest s)))))))

(defcheck solution-21884a4c
  (fn m [f s]
    (if (empty? s) (vector)
                   (lazy-seq (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-2198e2e8
  (fn ! [f [c & more]]
    (if (empty? more)
      (list (f c))
      (lazy-seq (cons (f c) (! f more)))
      )
    ))

(defcheck solution-2198ece8
  (fn remap [f vals]
    (cond (empty? vals) vals
          :else (cons (f (first vals)) (lazy-seq (remap f (rest vals))))
          )
    ))

(defcheck solution-21b29319
  #(rest (reductions (fn [x y] (% y)) 0 %2)))

(defcheck solution-21bff8f4
  (fn map-seq [f s]
    (let [[a & more] s fa (f a)]
      (if (empty? more)
        (list fa)
        (cons fa (lazy-seq (map-seq f more)))))))

(defcheck solution-222034f0
  (fn mm [f [a & r]]
    (if a
      (cons (f a) (lazy-seq (mm f r)))
      ())))

(defcheck solution-2227e4cd
  (fn my-map [f s]
    (if (empty? s)
      []
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-22292001
  (fn MAP [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (MAP f (rest s)))))))

(defcheck solution-22826606
  (fn m [f c]
    (if c
      (cons (f (first c)) (lazy-seq (m f (next c))))
      [])))

(defcheck solution-22bf6862
  (fn mymap [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-2305560f
  (fn mmap [f s]
    (lazy-seq
      (if-let [x (first s)]
        (cons (f x) (mmap f (rest s)))))))

(defcheck solution-2326d024
  (fn map- [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (map- f (rest s)))))))

(defcheck solution-24002306
  (fn map2 [f coll]
    (if (empty? coll)
      []
      (lazy-seq (cons (f (first coll)) (map2 f (rest coll)))))))

(defcheck solution-2419ad85
  (fn m [f s]
    (if (seq s)
      (cons (f (first s))
        (lazy-seq (m f (rest s))))
      nil)))

(defcheck solution-25989c65
  (fn m [pred coll] (if (empty? coll) (list) (lazy-seq (cons (pred (first coll)) (m pred (rest coll)))))))

(defcheck solution-25c6d468
  (fn g [f v] (lazy-seq (when-let [s (seq v)] (cons (f (first s)) (g f (next s)))))))

(defcheck solution-25cef378
  (fn
    [f coll]
    (rest (reductions (fn [x y]
                        (f y)) 0 coll))))

(defcheck solution-26d0399e
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (my-map f (rest coll)))))))

(defcheck solution-26deebec
  (fn m [f [x & xs]]
    (when x (cons (f x) (lazy-seq (m f xs))))))

(defcheck solution-27132724
  (fn remap
    [f col]
    (let [h (f (first col)) t (rest col)]
      (if (empty? t)
        [h]
        (cons h (lazy-seq (remap f t)))))))

(defcheck solution-272907f2
  (fn t [f s]
    (if (empty? s)
      nil
      (cons (f (first s))
        (lazy-seq (t f (rest s)))))))

(defcheck solution-2747b844
  (fn map* [f xs]
    (when (seq xs)
      (cons
        (f (first xs))
        (lazy-seq (map* f (rest xs)))))))

(defcheck solution-2811a59f
  (fn rmap [f s]
    (if (not (empty? s)) (cons (f (first s)) (lazy-seq (rmap f (rest s)))))))

(defcheck solution-292aa702
  (fn my-map [f coll]
    (if-not (empty? coll) (lazy-cat (vector (f (first coll))) (my-map f (rest coll))) ())))

(defcheck solution-293d4695
  (fn m [f xs]
    (when-not (nil? xs)
      (cons (f (first xs))
        (lazy-seq (m f (next xs)))))))

(defcheck solution-295ea821
  (fn r [f s]
    (if (= s [])
      []
      (cons (f (nth s 0)) (lazy-seq (r f (rest s)))))))

(defcheck solution-29a7eab2
  (fn foo [f coll]
    (lazy-seq
      (when-let [e (first coll)]
        (cons (f (first coll)) (foo f (rest coll)))))))

(defcheck solution-2a79166b
  (fn m [f x]
    (lazy-seq
      (if (seq x)
        (cons (f (first x)) (m f (rest x)))
        '()))))

(defcheck solution-2ae68cfe
  (fn my-map [f s]
    (when-let [xs (seq s)]
      (cons (f (first xs))
        (lazy-seq (my-map f (next xs)))))))

(defcheck solution-2b09ea89
  (fn my-map [f coll]
    (when-let [s (seq coll)]
      (lazy-seq
        (cons (f (first s)) (lazy-seq (my-map f (rest s))))
        )
      )
    ))

(defcheck solution-2b628861
  (fn my-map [f xs] (concat (vector (f (first xs))) (if (empty? (rest xs)) [] (lazy-seq (my-map f (rest xs)))))))

(defcheck solution-2ba7509a
  (fn map2 [f c]
    (if (empty? c)
      c
      (lazy-seq (concat (conj (empty c) (f (first c))) (map2 f (rest c))))
      )
    ))

(defcheck solution-2c030f3f
  (fn m [f lst]
    (cond
      (empty? lst) []
      :else (lazy-seq (cons (f (first lst)) (m f (rest lst))))
      )
    ))

(defcheck solution-2c3a1fba
  (fn lazy-map [f xs]
    (lazy-seq
      (if (seq xs)
        (cons (f (first xs)) (lazy-map f (rest xs)))))))

(defcheck solution-2c616357
  (fn m [f [x & xs]] (lazy-seq (cons (f x) (if (nil? xs) nil (m f xs))))))

(defcheck solution-2c6f96c4
  (fn my-map [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-2cace436
  (fn [op lst]
    (reductions #(op %2) (op (first lst)) (rest lst))))

(defcheck solution-2cb3a2fb
  (letfn [(my-map [f c] (when-let [s (seq c)] (lazy-seq (cons (f (first s)) (my-map f (next s))))))] my-map))

(defcheck solution-2d1edff
  (fn m [f [x & s]]
    (cons
      (f x)
      (if (empty? s)
        s
        (lazy-seq (m f s))
        )
      )
    ))

(defcheck solution-2d3e464f
  (fn m [f vals]
    (when (seq vals)
      (lazy-seq
        (concat [(f (first vals))]
                (m f (rest vals)))))))

(defcheck solution-2d5302cc
  (fn mymap [func coll]
    (if (empty? coll) ()
                      (lazy-seq (cons (func (first coll)) (mymap func (rest coll)))))
    ))

(defcheck solution-2ddb4bd2
  (fn fun [f coll]
    (when (seq coll)
      (lazy-seq
        (cons (f (first coll)) (fun f (next coll)))))
    ))

(defcheck solution-2dea0b5f
  (fn mymap [f [h & t]]
    (lazy-seq (cons (f h) (if t (mymap f t))))))

(defcheck solution-2e2f2f16
  (fn my-map [f a-seq]
    (if (first a-seq)
      (lazy-seq
        (cons (f (first a-seq))
          (my-map f (rest a-seq))))
      (list)
      )
    ))

(defcheck solution-2e337f8
  (fn my-map [f l] (lazy-seq (if (nil? (first l)) [] (cons (f (first l)) (my-map f (rest l)))))))

(defcheck solution-2e49f69d
  (fn map2 [f xs]
    (when (seq xs)
      (cons (f (first xs)) (lazy-seq (map2 f (rest xs)))))))

(defcheck solution-2e9c6154
  (fn mmm [f c]
    (when-let [s (seq c)]
      (cons (f (first s)) (lazy-seq (mmm f (rest s)))))))

(defcheck solution-2ed1a079
  #(reductions (fn [acc x] (%1 x)) (%1 (first %2)) (rest %2)))

(defcheck solution-2edd25bf
  (fn myMap [f [x & y]] (lazy-seq
                          (if (empty? y) [(f x)] (cons (f x) (myMap f y))))))

(defcheck solution-30154935
  (fn lm [f c] (if (empty? c)
                 []
                 (cons
                   (f (first c))
                   (lazy-seq (lm f (rest c)))))))

(defcheck solution-304ab1c
  (fn m [f s]
    (when (seq s) (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-308f10c6
  (fn my-map [f items]
    (if (empty? items)
      items
      (let [h (first items)
            r (rest items)]
        (lazy-seq (cons (f h) (my-map f r)))))))

(defcheck solution-30cbcbfc
  (fn mymap [f xs]
    (if (empty? xs) []
                    (lazy-seq (cons (f (first xs)) (mymap f (rest xs)))))))

(defcheck solution-30fb3c89
  (fn m [f c]
    (if (empty? c)
      c
      (cons (f (first c)) (lazy-seq (m f (rest c)))))))

(defcheck solution-3250fbc1
  (fn my-map [f coll]
    (when-let [s (seq coll)]
      (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-326dfb6b
  (fn mm [f xs]
    (if (seq xs)
      (cons (f (first xs))
        (lazy-seq (mm f (rest xs)))))))

(defcheck solution-32fa7794
  (fn mymap
    ([f v]
     (if (seq v)
       (lazy-seq (cons (f (first v)) (mymap f (rest v))))
       '()))))

(defcheck solution-3301d84a
  (fn mymap [f y]
    (when (first y)
      (cons (f (first y)) (lazy-seq (mymap f (rest y))))
      )))

(defcheck solution-3330f23b
  (fn m [f s]
    (lazy-seq
      (when-let [x (first s)]
        (cons (f x) (m f (rest s)))))))

(defcheck solution-3335776a
  (fn m [f s]
    (if (empty? s)
      []
      (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-338ab3da
  (fn iter [f l]
    (if (empty? l) []
                   (lazy-seq (cons (f (first l)) (iter f (rest l)))))))

(defcheck solution-33ad4008
  (fn m [f coll] (lazy-seq (when-let [s (seq coll)] (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-33ceac70
  (fn mymap [f coll]
    (if (not (empty? coll))
      (cons (f (first coll))
        (lazy-seq (if coll (mymap f (rest coll))))))))

(defcheck solution-33cf25d3
  (fn my-map [f s]
    (when (seq s)
      (cons (f (first s))
        (lazy-seq (my-map f (rest s)))))))

(defcheck solution-33f95eb0
  (fn newmap [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (newmap f (rest s)))))))

(defcheck solution-33fa1f2e
  (fn m [f s]
    (when-not (empty? s)
      (lazy-seq (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-342adbaf
  (fn my-map [f coll]
    (if (empty? coll)
      '()
      (cons (f (first coll))
        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-3446e186
  (fn m [f col] (lazy-seq (if (not-empty col) (cons (f (first col)) (m f (rest col)))))))

(defcheck solution-34a00e09
  (fn m [f x]
    (when (seq x)
      (lazy-seq (cons (f (first x)) (m f (rest x))))
      )))

(defcheck solution-34c31e78
  (fn mapeo [f coll]
    (if (empty? coll)
      coll
      (cons
        (f (first coll))
        (lazy-seq (mapeo f (rest coll))))
      )))

(defcheck solution-34cafeb9
  (fn myf [f coll]
    (if (empty? coll)
      []
      (cons (f (first coll)) (lazy-seq (myf f (rest coll)))))))

(defcheck solution-34f4a1b2
  (fn -map [f [a & r :as args]]
    (lazy-seq
      (if (empty? args)
        nil
        (cons (f a) (-map f r))))))

(defcheck solution-35509e6d
  (fn mymap [f [x & more]]
    (when x
      (lazy-seq (cons (f x) (mymap f more))))))

(defcheck solution-359e8988
  (fn mymap [f col]
    (lazy-seq
      (when-let [[h & t] (seq col)]
        (cons (f h) (mymap f t))))))

(defcheck solution-35ac2e9e
  (fn m [f c]
    (when-let [[a & b] c]
      (lazy-seq (cons (f a) (m f b))))))

(defcheck solution-35be862c
  (fn ! [f coll]
    (if-not (empty? coll) (lazy-seq (cons (f (first coll)) (! f (rest coll)))))))

(defcheck solution-35f2da34
  (fn g [f [s & r]]
    (if s
      (cons (f s) (lazy-seq (g f r))))))

(defcheck solution-367c8ddd
  (fn my-map [f s]
    (if (seq s)
      (lazy-seq
        (cons
          (f (first s))
          (my-map f (rest s)))))))

(defcheck solution-367f321c
  (fn [f coll]
    (letfn [(new-map [f coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (cons (f (first s)) (new-map f (rest s))))))]
      (new-map f (seq coll)))))

(defcheck solution-36e28cd3
  (fn mp [f lst]
    (if (empty? lst) '()
                     (cons (f (first lst)) (lazy-seq (mp f (rest lst)))))))

(defcheck solution-373c97ba
  (fn m [f, s]
    (when-let [v (first s)]
      (lazy-seq
        (cons (f v) (m f (rest s)))))))

(defcheck solution-374a77c3
  (fn re-implement-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (re-implement-map f (rest s)))))))

(defcheck solution-3766c24c
  (fn my-map [f lst]
    (lazy-seq (cons
                (f (first lst))
                (if (not (empty? (rest lst)))
                  (my-map f (rest lst))
                  nil)))))

(defcheck solution-37e78993
  (fn mp [f s]
    (if (empty? s)
      nil
      (lazy-seq (cons (f (first s)) (mp f (rest s)))))))

(defcheck solution-38e15827
  (letfn [
          (f-seq [f cs]
            (lazy-seq (if (empty? cs) nil (cons (f (first cs)) (f-seq f (rest cs))))))] (fn [g coll] (f-seq g coll))))

(defcheck solution-38e55a78
  (fn m [f [x & r]]
    (if x
      (cons (f x) (lazy-seq (m f r)))
      ())))

(defcheck solution-3910ecc6
  (fn ex118
    [f s]
    (if (empty? s)
      nil
      (cons (f (first s))
        (lazy-seq (ex118 f (rest s)))))))

(defcheck solution-3975f87b
  (fn [f c] (reductions #(f %2) (f (first c)) (rest c))))

(defcheck solution-398f248c
  (fn fun [f coll]
    (if (empty? coll) coll
                      (lazy-seq
                        (cons (f (first coll))
                          (fun f (rest coll)))))))

(defcheck solution-39ab8d42
  (letfn [(m [f s] (if (seq s) (lazy-seq (cons (f (first s)) (m f (rest s))))))] m))

(defcheck solution-3a3a9053
  (fn mm5 [f s]
    (if (empty? s) s
                   (cons (f (first s)) (lazy-seq (mm5 f (rest s)))))))

(defcheck solution-3a862bf7
  (fn m [f [e & r]] (cons (f e) (lazy-seq (if r (m f r))))))

(defcheck solution-3aa1de4d
  (fn myap
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (myap f (rest s))))))))

(defcheck solution-3aac7add
  (fn a [f [x & s]]
    (and x (lazy-cat [(f x)] (a f s)))))

(defcheck solution-3afb8ac0
  (fn [f coll]
    (letfn [(my-map [f coll]
              (if (empty? coll)
                coll
                (lazy-seq (cons (f (first coll)) (my-map f (rest coll))))))]
      (my-map f coll))))

(defcheck solution-3bf29fdd
  (fn mmap [f coll]
    (if (empty? coll) '()
                      (cons (f (first coll)) (lazy-seq (mmap f (rest coll)))))))

(defcheck solution-3c67039b
  (fn __ [f s]
    (lazy-seq
      (when-let [se (seq s)]
        (cons (f (first se)) (__ f (rest se)))))))

(defcheck solution-3cdb9711
  #((fn iter [l]
      (if (empty? l) ()
                     (cons (%1 (first l)) (lazy-seq (iter (rest l)))))) %2))

(defcheck solution-3ce3d276
  (fn my-map [f [x & xs]]
    (if x
      (cons (f x) (lazy-seq (my-map f xs)))
      [])))

(defcheck solution-3cfbe3eb
  (fn mymap [f col]
    (if (empty? col)
      col
      (cons (f (first col))
        (lazy-seq (mymap f (rest col)))))))

(defcheck solution-3d3f1226
  (fn my-map [f [x & xs]]
    (lazy-seq
      (cons (f x)
        (when (seq xs)
          (my-map f xs))))))

(defcheck solution-3db2489a
  (fn m [f x]
    (if (empty? x)
      nil
      (lazy-seq (cons (f (first x)) (m f (rest x))))
      )))



(defcheck solution-3e244329
  (fn mymap [f seq]
    (if (empty? seq)
      seq
      (lazy-cat (list (f (first seq))) (mymap f (rest seq))))))

(defcheck solution-3e3070e6
  (fn my-map [f col]
    (if (first col)
      (lazy-seq (cons (f (first col)) (my-map f (rest col))))
      (lazy-seq))))

(defcheck solution-3e620d60
  (fn r [f c]
    (lazy-seq
      (when-let [[h & t] (seq c)]
        (cons (f h) (r f t))))))

(defcheck solution-3e6f6dc3
  (fn map-re
    [func sq]
    (if (empty? sq) []
                    (cons (func (first sq)) (lazy-seq (map-re func (rest sq)))))))

(defcheck solution-3e772674
  (fn map' [f s]
    (if (empty? s)
      nil
      (cons (f (first s)) (lazy-seq (map' f (rest s)))))))

(defcheck solution-3f1723a7
  (fn mp [f s]
    (when (seq s) (cons (f (first s)) (lazy-seq (mp f (rest s)))))
    ))

(defcheck solution-3f25e095
  (fn remap [f xs]
    (if (seq xs)
      (cons (f (first xs)) (lazy-seq (remap f (rest xs)))))))

(defcheck solution-3f27b48f
  (fn m [f c]
    (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-401ee4d3
  (fn f2 [fn1 v]
    (if (not (empty? v))
      (cons (fn1 (first v)) (lazy-seq (f2 fn1 (rest v)))))))

(defcheck solution-402b8183
  (fn p118 [f ls]
    (if (empty? ls) nil
                    (lazy-seq (cons (f (first ls)) (p118 f (next ls)))))))

(defcheck solution-4049f63e
  (fn m [f v]
    (cons (f (first v)) (lazy-seq
                          (if (empty? (rest v))
                            []
                            (m f (rest v)))))))

(defcheck solution-40e3e32a
  (fn my-map [func coll]
    (if (empty? coll)
      []
      (cons (func (first coll)) (lazy-seq (my-map func (rest coll)))))))

(defcheck solution-41779864
  (fn m [f aseq]
    (lazy-seq
      (when-not (empty? aseq)
        (cons
          (f (first aseq))
          (m f (rest aseq)))))))

(defcheck solution-41a27de3
  (fn mymap [f [first & rest]]
    (lazy-seq (if (= nil first)
                ()
                (cons (f first) (mymap f rest))))))

(defcheck solution-422bef3a
  (fn m [op [el & se]] (lazy-seq (cons (op el) (if-not (seq se) () (m op se))))))

(defcheck solution-4248a77d
  (fn map' [f [head & tail]]
    (when head
      (lazy-seq (cons (f head) (map' f tail))))))

(defcheck solution-4314e4cf
  (fn mymap [f x]
    (lazy-seq
      (if (not (empty? x))
        (let [
              s   (seq x)
              acc []
              ]
          (cons (f (first s)) (mymap f (rest s))))))))

(defcheck solution-43a064e8
  (fn m [f s]
    (when (seq s)
      (cons (f (first s))
        (lazy-seq (m f (next s)))))))

(defcheck solution-441736c
  (fn mm [f l]
    (let [a (first l) b (rest l) c (f a)]
      (if (empty? b) [c]
                     (lazy-seq (cons c (mm f b)))))))

(defcheck solution-441f14e3
  (fn __
    [f s]
    (when-let [s (seq s)]
      (lazy-seq
        (cons (f (first s)) (__ f (next s)))))))

(defcheck solution-44c21593
  (fn m [f xs] (if (empty? xs) xs (lazy-seq (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-44f33bcb
  (fn my-map [f coll]
    (if (seq coll)
      (lazy-seq
        (cons (f (first coll)) (my-map f (rest coll))))
      '())))

(defcheck solution-45026448
  (fn m [f s]
    (if (seq s)
      (cons (f (first s))
        (lazy-seq (m f (next s))))
      nil)))

(defcheck solution-4508c974
  (fn my-map [f xs]
    (if-let [x (first xs)]
      (lazy-seq (cons (f x) (my-map f (rest xs))))
      nil)))

(defcheck solution-453b1ff3
  (fn [f xs]
    (if (not-empty xs)
      (drop 1 (reductions #(f %2) (first xs) xs)))))

(defcheck solution-45e5581c
  (fn m [f xs]
    (lazy-seq
      (when (seq xs)
        (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-4606adc6
  (fn f [x y]
    (when-let [a y]
      (lazy-cat (list (x (first a))) (f x (next a))))))

(defcheck solution-46727fba
  (fn mymap [f x]
    (let [a (f (first x)) b (rest x)]
      (if (empty? b)
        (list a)
        (cons a (lazy-seq (mymap f b))))
      )
    ))

(defcheck solution-46a78c2c
  (fn m [f l]
    (when l
      (lazy-seq (cons (f (first l))
                  (m f (next l)))))))

(defcheck solution-46c6eba8
  (fn my-map [f coll]
    (lazy-seq
      (when-let [[x & more] (seq coll)]
        (cons (f x) (my-map f more))))))

(defcheck solution-471af66e
  (fn m [f coll]
    (if (empty? coll)
      '()
      (cons (f (first coll)) (lazy-seq (m f (rest coll)))))))

(defcheck solution-473089fd
  (fn my-map [f [x & xs]]
    (lazy-seq
      (cons (f x) (if xs (my-map f xs))))))

(defcheck solution-475bbae7
  (fn map- [f coll]
    ^{:doc "118. Given a function f and an input sequence s, return a
  lazy sequence of (f x) for each element x in s."}
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (map- f (rest s)))))))

(defcheck solution-4764664
  (fn m [func coll]
    (if-not (empty? coll)
      (lazy-seq (cons (func (first coll)) (m func (rest coll)))))))

(defcheck solution-47abe129
  (fn my-map [f coll]
    (if (empty? coll)
      '()
      (lazy-seq (cons
                  (f (first coll))
                  (my-map f (drop 1 coll)))))))

(defcheck solution-47e2c8fd
  (fn my-map [f coll]
    (if (empty? coll)
      (empty coll)
      (lazy-seq
        (cons (f (first coll))
          (my-map f (rest coll)))))))

(defcheck solution-48175d28
  (fn m [f [h & t]]
    (if (nil? h)
      ()
      (lazy-seq (cons (f h) (m f t))))))

(defcheck solution-481952ee
  (fn my-map [f xs]
    (if (empty? xs)
      xs
      (lazy-seq (cons (f (first xs))
                  (my-map f (rest xs)))))))

(defcheck solution-483c6426
  (fn mymap [f s]
    (if (empty? s)
      s
      (lazy-seq (cons (f (first s)) (mymap f (rest s))))
      )
    ))

(defcheck solution-484e0c3d
  (fn m [f [a & r]]
    (if a (lazy-seq (cons (f a) (m f r))))))

(defcheck solution-488aecc
  (fn ls [f [s & r]]
    (when s
      (lazy-seq (cons (f s) (ls f r))))))

(defcheck solution-489eddf1
  (fn mp [x y] (when-let [q (seq y)] (lazy-seq (cons (x (first q)) (mp x (rest q)))))))

(defcheck solution-48e93d5b
  (fn mapa [f xs]
    (lazy-seq
      (if (empty? xs) xs
                      (cons (f (first xs)) (mapa f (rest xs)))))))

(defcheck solution-4914c6a6
  (fn r [f [h & c]]
    (if h (cons (f h) (lazy-seq (r f c))))))

(defcheck solution-49b6d0f4
  (fn map' [f [x & xs]] (when x (cons (f x) (lazy-seq (map' f xs))))))

(defcheck solution-49eca09e
  (fn m [f xs]
    (if (empty? xs)
      (list)
      (cons (f (first xs)) (lazy-seq (m f (rest xs)))))))

(defcheck solution-49ee026f
  (fn mapp [f x]
    (if (not-empty x)
      (lazy-seq (cons (f (first x)) (mapp f (rest x)))))))

(defcheck solution-4a235f10
  (fn map' [f [x & xs :as coll]] (when-not (empty? coll) (lazy-seq (cons (f x) (map' f xs))))))

(defcheck solution-4a724de5
  (fn m [f s]
    (if (empty? s)
      ()
      (lazy-seq
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-4ac9316b
  (fn my-map- [func coll]
    (if (empty? coll)
      '()
      (cons (func (first coll)) (lazy-seq (my-map- func (rest coll)))))))

(defcheck solution-4b0154c4
  (fn fero [fun se]
    (lazy-seq
      (if (empty? se)
        []
        (cons (fun (first se)) (fero fun (rest se))))
      )
    ))

(defcheck solution-4b024b4b
  (fn re-map [f s] (if (empty? s) '() (lazy-seq (cons (f (first s)) (re-map f (rest s)))))))

(defcheck solution-4b285cc9
  (fn marp [f c]
    (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (marp f (rest s)))))))

(defcheck solution-4bf3fc6c
  (fn s [f [a & r]]
    (when a
      (cons (f a)
        (lazy-seq (s f r))))))

(defcheck solution-4c005029
  (fn ! [f coll]
    (if-let [s (seq coll)]
      (lazy-seq (cons (f (first s)) (! f (rest s)))))))

(defcheck solution-4c0cb74b
  (fn mp [f x] (if (empty? x) '() (lazy-cat [(f (first x))] (mp f (rest x))))))

(defcheck solution-4c80efdc
  (fn my-map [f xs]
    (if (seq xs)
      (cons (f (first xs)) (lazy-seq (my-map f (rest xs))))
      nil)))

(defcheck solution-4c83addd
  (fn f [g s]
    (when (seq s)
      (cons
        (g (first s))
        (lazy-seq (f g (rest s)))))))

(defcheck solution-4c8c4c88
  (fn m [f [a & r :as s]]
    (if (seq s)
      (lazy-cat [(f a)] (m f r))
      )
    ))

(defcheck solution-4caab048
  (fn mp [f [s & r]] (if s (lazy-seq (cons (f s) (mp f r))) nil)))

(defcheck solution-4cc4f8a6
  (fn f [g vs]
    (if (seq vs)
      (lazy-seq (cons (g (first vs))
                  (f g (rest vs))))
      vs)))

(defcheck solution-4cf5bb6c
  (fn nam [f [x & v]] (cons (f x) (if v (lazy-seq (nam f v)) nil))))

(defcheck solution-4d13c9dd
  (fn map0 [f xs]
    (lazy-seq
      (if (empty? xs) nil
                      (cons (f (first xs)) (map0 f (rest xs)))))))

(defcheck solution-4de4c640
  (fn mymap [f s]
    (if (empty? s)
      []
      (cons (f (first s)) (lazy-seq (mymap f (rest s)))))
    ))

(defcheck solution-4df0c10f
  (fn map-clone [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (map-clone f (rest s)))))))

(defcheck solution-4df9cc75
  (fn lmap [f [x & xs]]
    (lazy-seq
      (if (nil? x) []
                   (cons (f x) (lmap f xs))))))

(defcheck solution-4e2979f6
  (fn mymap [f xs]
    (if (empty? xs)
      '()
      (cons (f (first xs))
        (lazy-seq (mymap f (rest xs))))
      )
    ))

(defcheck solution-4e88e79a
  (fn m
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (m f (rest s))))))))

(defcheck solution-4e9fb83
  (fn map' [f in-seq]
    (if (empty? in-seq)
      ()
      (cons (f (first in-seq)) (lazy-seq (map' f (rest in-seq)))))))

(defcheck solution-4ea2cd77
  (fn mym ([f coll] (lazy-seq (when-let [s (seq coll)] (cons (f (first s)) (mym f (rest s))))))))

(defcheck solution-4f0c1aa4
  (fn map2 [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first s)) (map2 f (rest s)))))))

(defcheck solution-4f8f2d8c
  (fn mm [f xs]
    (if (empty? xs)
      '()
      (lazy-seq (cons (f (first xs)) (mm f (rest xs)))))))

(defcheck solution-4f9d57db
  (fn mp [f coll] (if-not (empty? coll) (cons (f (first coll)) (lazy-seq (mp f (rest coll)))))))

(defcheck solution-5007e9f
  (fn m [f [h & t]]
    (lazy-seq
      (when h
        (cons (f h) (m f t))))))

(defcheck solution-503155df
  (fn m [f s]
    (when-first [e s]
      (lazy-seq (cons (f e) (m f (next s)))))))

(defcheck solution-50b1cb10
  (fn map2 [f xs]
    (if (empty? xs) ()
                    (lazy-seq (cons (f (first xs)) (map2 f (rest xs)))))))

(defcheck solution-50cbc28c
  (fn m [f [h & r]] (lazy-seq (cons (f h) (if r (m f r))))))

(defcheck solution-50f5c6cd
  (fn foo [f xs] (lazy-seq (if (empty? xs) [] (cons (f (first xs)) (foo f (rest xs)))))))

(defcheck solution-5113cc4d
  (fn ! [f [h & t]] (cons (f h) (when t (lazy-seq (! f t))))))

(defcheck solution-51874cbe
  (fn mmap [f xs] (lazy-seq (if (empty? xs) xs (cons (f (first xs)) (mmap f (rest xs)))))))

(defcheck solution-519995a3
  (fn g [f s] (lazy-seq (when (seq s) (cons (f (first s)) (g f (rest s)))))))

(defcheck solution-51d60c4f
  (fn mapa [f s]
    (if (empty? s)
      []
      (lazy-seq (cons (f (first s)) (mapa f (rest s)))))))

(defcheck solution-51fa4dc9
  (fn my-map
    [f [x & more :as coll]] {:pre [(ifn? f)]}
    (if (seq coll)
      (lazy-seq (cons (f x) (my-map f more)))
      '())))

(defcheck solution-522ea6f9
  (fn m [f coll]
    (if (not-empty coll)
      (cons (f (first coll))
        (lazy-seq (m f (rest coll)))))))

(defcheck solution-527b2716
  (fn mp [f c]
    (cons (f (first c)) (if (nil? (next c))
                          '()
                          (lazy-seq (mp f (next c)))))))

(defcheck solution-52a3b5ec
  (fn m [f [x & r]]
    (if x
      (cons (f x)
        (lazy-seq (m f r))))))

(defcheck solution-52abae91
  (fn m [f col]
    (if (seq col)
      (lazy-seq
        (cons (f (first col))
          (m f (rest col))
          )))))

(defcheck solution-52fd6e3f
  (fn m [f [x & xs]]
    (cons (f x)
      (if xs (lazy-seq (m f xs))))))

(defcheck solution-53031e7a
  (fn map' [f s]
    (lazy-seq
      (when-not (empty? s)
        (cons (f (first s)) (map' f (rest s)))))))

(defcheck solution-5365cdb
  (fn ! [f s]
    (if (not (empty? s))
      (cons (f (first s)) (lazy-seq (! f (rest s)))))))

(defcheck solution-53eeb971
  (fn map* [f [x & xs]]
    (when x
      (cons (f x) (lazy-cat (map* f xs))))))

(defcheck solution-5455bd6
  (fn mymap [f s]
    (lazy-seq
      (when-let [s (seq s)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-546b7d48
  (fn remap [f coll]
    (when coll
      (lazy-seq (cons (f (first coll)) (remap f (next coll))))
      )
    ))

(defcheck solution-5471f10c
  (fn [func coll]
    (letfn [(prod [f [x & xs]]
              (lazy-seq
                (cons (f x) (when (seq xs) (prod f xs)))))]
      (prod func coll))))

(defcheck solution-547574c3
  (fn [f coll] (reductions (fn [_ a] (f a)) (f (first coll)) (rest coll))))

(defcheck solution-55840169
  (fn mymap [f s]
    (if (empty? s)
      s
      (cons (f (first s)) (lazy-seq (mymap f (rest s)))))))

(defcheck solution-55edf67b
  (fn mymap [f xs]
    (if (empty? xs) ()
                    (cons (f (first xs))
                      (lazy-seq
                        (mymap f (rest xs)))))))

(defcheck solution-55fb2388
  (fn r [f c] (if (next c)
                (lazy-seq (cons (f (first c)) (r f (next c))))
                [(f (first c))])))

(defcheck solution-56573fbe
  (fn hey [x y] (if (empty? y) nil (lazy-seq (cons (x (first y)) (hey x (rest y)))))))

(defcheck solution-5666a923
  (fn g [f s] (lazy-seq (when (seq s) (cons (f (first s)) (g f (rest s)))))))

(defcheck solution-56787a1f
  (fn m [f [x & xs]]
    (cons (f x) (lazy-seq (if xs (m f xs))))))

(defcheck solution-56ac64ed
  (fn p118 [f l]
    (if (empty? l) '()
                   (cons (f (first l)) (lazy-seq (p118 f (rest l)))))))

(defcheck solution-574b2d8
  (fn mymap [f s]
    (if (nil? s)
      nil
      (cons (f (first s)) (lazy-seq (mymap f (next s))))
      )
    ))

(defcheck solution-57b728da
  (fn my-map [f [a & r]] (if r (lazy-seq (cons (f a) (my-map f r))) [(f a)])))

(defcheck solution-57c094ac
  (fn my-map [f s]
    (if s (lazy-seq (cons (f (first s)) (my-map f (next s)))))))

(defcheck solution-5856ae1d
  (fn
    [f coll]
    (letfn [(r [coll] (lazy-seq
                        (if coll
                          (cons (f (first coll)) (r (next coll)))
                          '())))]
      (r coll))))

(defcheck solution-585fcb2b
  (fn rm [f s]
    (if (not (empty? s))
      (lazy-seq
        (cons (f (first s))
          (rm f (rest s)))))))

(defcheck solution-59180ad0
  (fn mustela-map [f l]
    (lazy-seq
      (when-let [l (seq l)]
        (cons (f (first l)) (mustela-map f (rest l)))))))

(defcheck solution-5a47ab64
  (fn mymap [f l]
    (if (empty? l) '()
                   (lazy-seq (cons (f (first l)) (mymap f (rest l)))))))

(defcheck solution-5a775db5
  (fn mp [f se]
    (lazy-seq
      (if (empty? se)
        nil
        (cons (f (first se)) (mp f (rest se)))))))

(defcheck solution-5a93cb78
  (fn mymap [f xs]
    (if (not (seq xs))
      []
      (cons (f (first xs)) (lazy-seq (mymap f (rest xs))))
      )
    ))

(defcheck solution-5af9a2cc
  (fn m [f coll] (if (empty? coll) nil (lazy-seq (concat [(f (first coll))] (m f (rest coll)))))))

(defcheck solution-5b04c56d
  (fn my-map [f s]
    (lazy-seq (if (nil? (second s))
                (list (f (first s)))
                (cons (f (first s))
                  (my-map f (rest s)))))))

(defcheck solution-5b0a0ad4
  (fn b [f [x & y]]
    (if x
      (cons (f x) (lazy-seq (b f y))))))

(defcheck solution-5b1d6a2b
  (fn my-map [f [x & xs]]
    (if x
      (lazy-seq (cons (f x) (my-map f xs))))))

(defcheck solution-5b23a1ef
  (fn my-map [f coll]
    (when-let [s (seq coll)] (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-5bc2829d
  (fn m [f c]
    (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-5bd3f028
  (fn [f s]
    ((fn r [s]
       (if (seq s)
         (cons (f (first s)) (lazy-seq (r (rest s))))
         s)) s)))

(defcheck solution-5c36ae61
  (fn map' [f s] (lazy-seq (when-let [x (first s)] (cons (f x) (map' f (rest s)))))))

(defcheck solution-5c406aed
  (fn my-map [f coll]
    (when (not (empty? coll))
      (cons
        (f (first coll))
        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-5c93567d
  (fn m [f s] (if (seq s)
                (lazy-seq (cons (f (first s)) (m f (rest s))))
                nil)))

(defcheck solution-5d0944c8
  (fn re-map [f col]
    (if (empty? col)
      '()
      (lazy-seq
        (cons (f (first col)) (re-map f (rest col)))))))

(defcheck solution-5dccf833
  (fn m [f [x & y]]
    (if x
      (cons (f x)
        (lazy-seq (m f y))))))

(defcheck solution-5e73a22e
  (fn m [f s]
    (lazy-seq
      (if (seq s)
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-5ead6c79
  (fn map2 [f xs]
    (if (seq xs)
      (lazy-seq (cons (f (first xs)) (map2 f (rest xs)))))))

(defcheck solution-5f7f4f34
  (fn mymap
    [myfn myseq]

    (if (empty? myseq)
      nil
      (cons (myfn (first myseq)) (lazy-seq (mymap myfn (rest myseq)))

        ))))

(defcheck solution-6041fa85
  (fn mymap [f coll]
    (when-let [s (seq coll)]
      (lazy-seq
        (cons (f (first s)) (mymap f (next s)))
        )
      )
    ))

(defcheck solution-605ebbf1
  (fn m [f col]
    (lazy-seq (if-let [s (seq col)]
                (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-60700f7e
  (fn mymap [f s] (lazy-seq (if (seq s) (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-607f50e3
  (fn my-map [f coll]
    (if (empty? coll)
      nil
      (lazy-seq (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-60d4bf3
  (fn m [f xs]
    (lazy-seq
      (if (empty? xs) []
                      (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-611fa2e5
  (fn g [f c]
    (if (nil? c)
      []
      (cons (f (first c)) (lazy-seq (g f (next c))))
      )
    ))

(defcheck solution-612d86f0
  (fn [f xs] (drop 1 (reductions #(f %2) 0 xs))))

(defcheck solution-6147cafa
  (fn mymap [f [x & xs]]
    (lazy-seq
      (if (nil? x)
        nil
        (cons (f x)
          (mymap f xs))))))

(defcheck solution-6184648d
  (fn my-re-implement-map
    [fun coll]
    (when (not (empty? coll))
      (lazy-seq
        (cons (fun (first coll)) (my-re-implement-map fun (rest coll)))))))

(defcheck solution-61af8f0c
  (fn m-map [f s]
    (lazy-seq
      (when (not (empty? s))
        (cons (f (first s)) (m-map f (rest s)))))))

(defcheck solution-62694bd7
  (fn __ [f col]
    (if (seq col)
      (lazy-seq
        (cons (f (first col))
          (__ f (rest col))))
      nil)))

(defcheck solution-628a469d
  (fn a [f s]
    (cons
      (f (first s))
      (if (empty? (rest s))
        '()
        (lazy-seq (a f (rest s)))))))

(defcheck solution-628efddc
  (fn m [f x] (if (empty? x) () (cons (f (first x)) (lazy-seq (m f (rest x)))))))

(defcheck solution-62bf7851
  (fn map2 [f s]
    (lazy-seq
      (if-not (empty? s)
        (cons
          (f (first s))
          (map2 f (rest s)))))))

(defcheck solution-62cb87fd
  (fn mps [f coll]
    (when coll
      (lazy-seq (cons (f (first coll)) (mps f (next coll)))))))

(defcheck solution-62d2cb30
  #(rest (reductions (comp % second list) () %2)))

(defcheck solution-63194ecf
  (fn mapx [f s]
    (if (empty? s)
      '()
      (cons (f (first s))
        (lazy-seq
          (mapx f (rest s)))))))

(defcheck solution-6345c427
  (fn my-map [f, input-seq]
    (if (empty? input-seq) []
                           (lazy-seq (cons (f (first input-seq))
                                       (my-map f (rest input-seq)))))))

(defcheck solution-6385a090
  (fn new-map
    [f [h & t]]
    (if (nil? h)
      nil
      (lazy-seq (cons (f h) (new-map f t))))))

(defcheck solution-63bfa1dc
  (fn mp [f [o & r]]
    (if (empty? r)
      [(f o)]
      (cons (f o) (lazy-seq (mp f r))))))

(defcheck solution-63db627a
  (fn mymap [f coll]
    (if (false? (empty? coll))
      (lazy-seq
        (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-644574b0
  (fn [f initial-s]
    (letfn [(map' [[x & xs :as s]]
              (when (not-empty s)
                (cons (f x) (lazy-seq (map' xs)))))]
      (map' initial-s))))

(defcheck solution-6446933c
  (fn M [f S] (if-let [[s & rs] S] (cons (f s) (lazy-seq (M f rs))))))

(defcheck solution-644b046a
  (fn m [f c]
    (lazy-seq
      (if-let [c (seq c)]
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-644e3af5
  (fn myMap [f s]
    (when (first s)
      (cons (f (first s)) (lazy-seq (myMap f (rest s)))))))

(defcheck solution-649f23b2
  (fn fmap [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first xs)) (fmap f (rest xs)))))))

(defcheck solution-64a5778c
  (fn my-it1 [f s]
    (if (not-empty s) (cons (f (first s)) (lazy-seq (my-it1 f (rest s)))))))

(defcheck solution-64a5be76
  (fn m [f s]
    (if (empty? s) '() (cons (f (first s)) (lazy-seq (m f (drop 1 s)))))))

(defcheck solution-64d68334
  (fn map4j [f xs]
    (when xs
      (lazy-seq
        (cons (f (first xs))
          (map4j f (next xs)))))))

(defcheck solution-64e9702c
  (fn my-map [f s]
    (if (empty? s)
      []
      (cons (f (first s))
        (lazy-seq (my-map f (rest s)))))))

(defcheck solution-655ec3c0
  (fn my-map [f [x & r :as col]]
    (if (seq col)
      (cons (f x)
        (lazy-seq (my-map f r)))
      '())))

(defcheck solution-657b614f
  (fn m [f s]
    (if (empty? s)
      '()
      (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-65b6b2b0
  (fn m [f c]
    (if (not (empty? c))
      (lazy-seq
        (cons (f (first c))
          (m f (rest c)))))))

(defcheck solution-65bb28c7
  (fn l [f [a & m]]
    (lazy-seq
      (cons (f a) (if m (l f m))))))

(defcheck solution-65f9c2b2
  (fn mymap [f sq] (if (empty? sq) '() (cons (f (first sq)) (lazy-seq (mymap f (rest sq)))))))

(defcheck solution-6639bea9
  (fn mapf [f col]
    (lazy-seq
      (let [c (seq col)]
        (when c
          (cons (f (first c)) (mapf f (rest c))))))))

(defcheck solution-66992859
  (fn newmap [f s]
    (if (empty? s) '()
                   (let [firstelt (first s)
                         remelts  (rest s)
                         result   (f firstelt)]
                     (lazy-seq (cons result (newmap f remelts)))))))

(defcheck solution-66bd2598
  (fn mp [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (mp f (rest s)))))))

(defcheck solution-66bd5c7
  (fn mmap
    [f s]
    (if (empty? s)
      s
      (lazy-seq (cons (f (first s)) (mmap f (rest s)))))))

(defcheck solution-66c48aa6
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (my-map f (rest s)))
        ))))

(defcheck solution-670a6838
  (fn flap
    [f s]
    (when-not (empty? s)
      (lazy-seq (cons (f (first s))
                  (flap f (rest s)))))))

(defcheck solution-6744aec3
  (fn m [f [a & r]]
    (if a
      (lazy-seq
        (cons (f a) (m f r))))))

(defcheck solution-67db5e1d
  (fn lazy-map [f, xs]
    (cond
      (empty? xs) (lazy-seq)
      :else (cons (f (first xs)) (lazy-seq (lazy-map f (rest xs))))
      )
    ))

(defcheck solution-67e31e59
  (fn rec [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s))
          (rec f (rest s)))))))

(defcheck solution-682430e5
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-6829040d
  (fn my-map [f [a & xs]]
    (if (nil? a)
      nil
      (cons (f a) (lazy-seq (my-map f xs))))
    ))

(defcheck solution-689740f0
  (fn my-map [f s]
    (if (empty? s)
      []
      (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-695c1603
  (fn m [f xs]
    (if (empty? xs)
      []
      (lazy-cat [(f (first xs))] (m f (rest xs))))))

(defcheck solution-69ce1c76
  (fn mapp [f sq]
    (if (empty? sq) nil
                    (lazy-seq (cons (f (first sq)) (mapp f (rest sq)))))))

(defcheck solution-69e939bd
  (fn m [f [l & ls]] (if ls (lazy-seq (cons (f l) (m f ls))) (list (f l)))))

(defcheck solution-6a20722
  (fn mmap [f col]
    (if (empty? col)
      '()
      (cons
        (f (first col))
        (lazy-seq
          (mmap f (rest col))
          )
        )
      )
    ))

(defcheck solution-6a7f435c
  (fn map' [f s]
    (when (seq s)
      (lazy-seq
        (cons (f (first s))
          (map' f (rest s)))))))

(defcheck solution-6ab9006e
  ;;(fn new-map [f s]
  ;;  (when (seq s)
  ;;    (cons (f (first s))
  ;;          (lazy-seq
  ;;           (new-map f (rest s))))))

  ;; both of these solutions work, but this one will probably work like map
  ;; in the case when the initial input is an empty seq or nil
  (fn new-map [f s]
    (lazy-seq
      (when (seq s)
        (cons (f (first s))
          (new-map f (rest s)))))))

(defcheck solution-6b0fe2f5
  (fn my-map [f s]
    (lazy-seq
      (if (nil? s)
        '()
        (cons (f (first s)) (my-map f (next s)))))))

(defcheck solution-6b137a4d
  (fn mmap [f s]
    (if (empty? s)
      []
      (lazy-seq (cons (f (first s)) (mmap f (rest s)))))))

(defcheck solution-6b1f91c9
  (fn mine [f coll]
    (if (not (empty? coll))
      (lazy-seq (cons (f (first coll)) (mine f (rest coll)))))))

(defcheck solution-6b32d462
  (fn my-map [f s]
    (if (empty? s) nil
                   (lazy-seq (cons (f (first s)) (my-map f (rest s)))))
    ))

(defcheck solution-6b5148a0
  (fn map2 [f x]
    (if
     (empty? x) nil
                (cons (f (first x)) (lazy-seq (map2 f (rest x)))))
    ))

(defcheck solution-6bf2366
  (fn mmm [f coll]
    (lazy-seq
      (if (empty? coll) []
                        (cons (f (first coll)) (mmm f (rest coll)))))))

(defcheck solution-6bf88f5f
  (fn map_
    [f x]
    (lazy-seq
      (when-let [s (seq x)]
        (cons (f (first s)) (map_ f (rest s)))))))

(defcheck solution-6bf94095
  (fn mymap [f coll]
    (if (false? (empty? coll))
      (lazy-seq
        (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-6c5d3b96
  (fn mymap [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-6c806388
  (fn mp [f s]
    (if (not (empty? s))
      (lazy-cat (list (f (first s))) (mp f (rest s))))))

(defcheck solution-6d2f7373
  (fn re-map [f col]
    (if (seq col) (lazy-seq
                    (cons (f (first col)) (re-map f (rest col))))
                  nil)))

(defcheck solution-6d37a01d
  (fn _ [f [v & r]]
    (if v
      (lazy-seq
        (cons
          (f v)
          (_ f r))))))

(defcheck solution-6db6f256
  (fn m [f [h & r]] (if h (lazy-seq (cons (f h) (m f r))))))

(defcheck solution-6de9ddbe
  (fn m [f [h & r]] (cons (f h) (when r (lazy-seq (m f r))))))

(defcheck solution-6e3a3f03
  (fn mymap [f coll] (lazy-seq (if-let [[h & r] (seq coll)] (cons (f h) (mymap f r))))))

(defcheck solution-6ee18dd3
  (fn collect [f s]
    (if (seq s)
      (cons (f (first s)) (lazy-seq (collect f (rest s)))))))

(defcheck solution-6f89b749
  (fn _map [f x]
    (if (empty? x) '()
                   (lazy-seq (cons (f (first x)) (_map f (rest x)))))))

(defcheck solution-6f98f8e2
  (fn m [f [c & r]]
    (if r
      (lazy-seq
        (cons (f c) (m f r)))
      (cons (f c) []))))

(defcheck solution-6fa60adb
  (fn it [f xs]
    (if (empty? xs) nil
                    (lazy-seq
                      (cons (f (first xs))
                        (it f (rest xs)))))))

(defcheck solution-6fbd5ff4
  (fn mymap [f s]
    (if (empty? s)
      []
      (lazy-seq (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-6fc3257e
  (fn my-map [f s] (lazy-seq (when-let [t (seq s)] (cons (f (first t)) (my-map f (rest t)))))))

(defcheck solution-6fcf49c8
  (fn my-map
    [f [x & xs]]
    (lazy-seq
      (if xs
        (cons (f x) (my-map f xs))
        [(f x)]))))

(defcheck solution-7011ebb4
  (fn my-map
    [f xs]
    (if (not-empty xs) (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-709740a7
  (fn m [op c]
    (if (empty? c)
      []
      (cons (op (first c)) (lazy-seq (m op (rest c)))))))

(defcheck solution-70cb6038
  (fn remap [f l] (if (empty? l) l (cons (f (first l)) (lazy-seq (remap f (rest l)))))))

(defcheck solution-70db4fff
  (fn mymap [f s]
    (lazy-seq
      (if (seq s)
        (cons (f (first s)) (mymap f (rest s)))
        []))))

(defcheck solution-716ea9b8
  (fn m [f xs]
    (lazy-seq
      (when-let [xs (seq xs)]
        (cons (f (first xs))
          (m f (rest xs)))))))

(defcheck solution-7192ebed
  (fn my-map [f [head & tail]]
    (if (nil? head)
      '()
      (cons (f head) (lazy-seq (my-map f tail))))))

(defcheck solution-71a0c6b6
  (fn maplite [f alist]
    (if (empty? alist)
      []
      (cons (f (first alist)) (lazy-seq (maplite f (rest alist)))))))

(defcheck solution-71c17b88
  (fn M [f l] (lazy-seq (and l (cons (f (first l)) (M f (next l)))))))

(defcheck solution-71f253b7
  (fn m-map [f coll]
    (lazy-seq
      (if (empty? coll) nil
                        (cons (f (first coll)) (m-map f (rest coll)))))))

(defcheck solution-72139b7e
  (fn my-map [pred [fst & rst :as xs]]
    (lazy-seq (when (seq xs) (cons (pred fst) (my-map pred rst))))))

(defcheck solution-728339e
  (fn mymap [f xs]
    (loop [f  f
           xs xs]
      (if-let [xs (seq xs)]
        (lazy-seq (cons (f (first xs)) (mymap f (next xs))))
        nil))))

(defcheck solution-7296cb49
  (fn m [f xs]
    (if (empty? xs) xs
                    (cons (f (first xs)) (lazy-seq (m f (rest xs)))))))

(defcheck solution-72984946
  (fn my-map
    ([f coll] (my-map f (rest coll) (f (first coll))))
    ([f coll ret]
     (if (not-empty coll)
       (lazy-seq (cons ret (my-map f (rest coll) (f (first coll)))))
       (lazy-seq [ret])))))

(defcheck solution-729e612
  #(rest (reductions (fn [a x] (% x)) 1 %2)))

(defcheck solution-72c090
  (fn map2 [f s]
    (if (empty? s) nil
                   (lazy-seq (cons (f (first s)) (map2 f (rest s)))))))

(defcheck solution-733360ab
  (fn mm [f [x & xs]]
    (if (empty? xs)
      (cons (f x) nil)
      (lazy-seq
        (cons (f x) (mm f xs))))))

(defcheck solution-73490370
  (fn mymap [f s]
    (if (empty? s)
      ()
      (lazy-seq (cons (f (first s)) (mymap f (rest s))))
      )
    ))

(defcheck solution-73bee68
  (fn here [f coll]
    (lazy-seq
      (if-not (seq coll)
        nil
        (cons (f (first coll)) (lazy-seq (here f (next coll))))))))

(defcheck solution-74104cbf
  (clojure.core/fn
    my-map
    [f coll]
    (when (seq coll) (lazy-seq (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-742927f7
  (fn o [f s]
    (let [i (first s)
          r (rest s)]
      (cons (f i) (if (not-empty r) (lazy-seq (o f r)))))))

(defcheck solution-748674b7
  (fn [x y] (reduce #(conj %1 (x %2)) [] (take 1000010 y))))

(defcheck solution-74b98a44
  (fn m [f s] (when (not-empty s)
                (cons (f (first s))
                  (lazy-seq (m f (rest s)))))))

(defcheck solution-74e8defb
  (fn m [f c]
    (lazy-seq
      (if (empty? c)
        '()
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-74f52ec7
  (fn [f l]
    (rest (reductions #(f %2) 0 l))))

(defcheck solution-74f5a765
  (fn m [f l]
    (if (empty? l)
      nil
      (lazy-seq
        (cons (f (first l))
          (m f (next l)))))))

(defcheck solution-7536b16a
  (fn ff [f xs]
    (if (empty? xs)
      xs
      (lazy-seq (cons (f (first xs)) (ff f (drop 1 xs)))))))

(defcheck solution-75976fa4
  (fn fname# [f aseq]
    (if (counted? aseq)
      (reduce #(conj %1 (f %2)) [] aseq)
      (lazy-seq (cons (f (first aseq)) (fname# f (rest aseq)))))))

(defcheck solution-75a51a66
  #(letfn [(mapfn [f coll] (lazy-seq (when-let [s (seq coll)] (cons (f (first s)) (mapfn f (next s))))))] (mapfn %1 %2)))

(defcheck solution-75bf698f
  (fn my-map
    [f coll]
    (lazy-seq
      (when (seq coll)
        (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-77134f28
  (fn map-elem [f coll]
    (if (empty? coll)
      '()
      (lazy-seq (cons (f (first coll)) (map-elem f (rest coll))))
      )))

(defcheck solution-772afaf5
  (fn my-map [f [x & xs]]
    (if (empty? xs)
      (list (f x))
      (lazy-cat (list (f x)) (my-map f xs)))))

(defcheck solution-776a4ad6
  (fn mymap [f l] (if (empty? l) l (lazy-seq (cons (f (first l)) (mymap f (rest l)))))))

(defcheck solution-776eafea
  (fn mp [f coll]
    (lazy-seq
      (if (empty? coll)
        nil
        (cons (f (first coll)) (mp f (rest coll))))
      )
    ))

(defcheck solution-778f1861
  (fn jakeymap
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (jakeymap f (rest s))))))))

(defcheck solution-7792570d
  (fn my-map
    [f coll]
    (if (nil? (first coll))
      nil
      (cons
        (f (first coll))
        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-77991c31
  (fn my-map [f coll]
    (if (empty? coll) nil
                      (cons (f (first coll))
                        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-77bcf0de
  (fn f [a [b & r]] (when b (cons (a b) (lazy-seq (f a r))))))

(defcheck solution-7810af95
  (fn my-map [f coll]
    (when-let [[head & more] coll]
      (cons (f head)
        (lazy-seq (my-map f more))))))

(defcheck solution-7862d14d
  (fn myMap [f l]
    (if (empty? l) []
                   (cons (f (first l))
                     (lazy-seq (myMap f (rest l)))))))

(defcheck solution-78b0f38f
  (fn map2 [f coll]
    (if (empty? coll)
      []
      (cons (f (first coll))
        (lazy-seq (map2 f (rest coll)))))))

(defcheck solution-78ed0677
  (fn my-map [f s] (if (not (empty? s)) (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-79958ea8
  (fn [f coll]
    (letfn [(my-map [x]
              (if (empty? x)
                x
                (cons (f (first x))
                  (lazy-seq (my-map (rest x))))))]
      (my-map coll))))

(defcheck solution-79a4717
  (fn my-map [f col]
    (if (empty? col) col
                     (lazy-seq
                       (cons (f (first col)) (my-map f (rest col)))))))

(defcheck solution-7a4cfdcd
  (fn m [f s] (when (seq s) (lazy-seq (concat [(f (first s))]
                                              (m f (rest s)))))))

(defcheck solution-7ae51dea
  (fn mymap [f x] (if (not-empty x) (lazy-seq (cons (f (first x)) (mymap f (rest x)))))))

(defcheck solution-7b20e5e9
  (fn i [f [a & b]] (if (nil? a) nil (cons (f a) (lazy-seq (i f b))))))

(defcheck solution-7b23e660
  (fn map*
    [f s]
    (cons (f (first s))
      (lazy-seq
        (when-let [r (seq (rest s))]
          (map* f r))))))

(defcheck solution-7b38772d
  (fn foo [f coll] (if (empty? coll) '() (lazy-seq (cons (f (first coll)) (foo f (rest coll)))))))

(defcheck solution-7b6eb5fa
  (fn map3
    [f coll]
    (if (empty? coll)
      coll
      (cons (f (first coll)) (lazy-seq (map3 f (rest coll)))))))

(defcheck solution-7ba8809b
  (fn map- [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (map- f (rest s)))))))

(defcheck solution-7bcd4366
  (fn m [f [x & y]] (if x (lazy-seq (cons (f x) (m f y))))))

(defcheck solution-7bcdb138
  #(flatten (keep (juxt %) %2)))

(defcheck solution-7bd3f53e
  (fn my-map [f c] (if (empty? c) nil (cons (f (first c)) (lazy-seq (my-map f (rest c)))))))

(defcheck solution-7bd6b43b
  (fn m [f s]
    (if (empty? s)
      []
      (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-7c3127e4
  (fn M [f s]
    (if (empty? s) []
                   (cons (f (first s))
                     (lazy-seq (M f (rest s)))))))

(defcheck solution-7c845995
  (fn n118 [f coll]
    (lazy-seq (if (empty? coll) coll (cons (f (first coll)) (n118 f (rest coll)))))))

(defcheck solution-7c8b7e37
  (fn m [f l]
    (if (seq l)
      (lazy-cat [(f (first l))] (m f (rest l))) [])))

(defcheck solution-7ccd7058
  (fn map' [f xs]
    (if (empty? xs)
      '()
      (lazy-seq
        (cons (f (first xs))
          (map' f (rest xs)))))))

(defcheck solution-7d149a73
  (fn map' [f coll]
    (lazy-seq
      (when-let [[x & more] (seq coll)]
        (cons (f x) (map' f more))))))

(defcheck solution-7d3ce259
  (fn m
    [f [h & t]]
    (when h
      (lazy-seq
        (cons (f h)
          (m f t))))))

(defcheck solution-7d5c1226
  (fn mymap [f c]
    (if (empty? c)
      nil
      (cons (f (first c))
        (lazy-seq (mymap f (rest c)))))))

(defcheck solution-7d60a8a2
  (fn g [f [x & xs]]
    (lazy-seq (cons
                (f x)
                (if xs (g f xs))))))

(defcheck solution-7de52057
  (fn de [f d]
    (when-not (empty? d)
      (cons (f (first d)) (lazy-seq (de f (rest d)))))))

(defcheck solution-7df44c4e
  (fn mp [f coll]
    (lazy-seq
      (when (seq coll)
        (cons (f (first coll)) (mp f (rest coll)))))))

(defcheck solution-7e190776
  (fn mp [f [arg & args]]
    (if
     (empty? args)
      (list (f arg))
      (cons
        (f arg)
        (lazy-seq (mp f args))))))

(defcheck solution-7e3cb7b5
  (fn mp [f s]
    (if (empty? s)
      []
      (cons (f (first s))
        (lazy-seq (mp f (rest s)))))))

(defcheck solution-7eb7613
  (fn my-map [f [x & xs]]
    (cons (f x)
      (if (nil? xs) '() (lazy-seq (my-map f xs))))))

(defcheck solution-7f302e6f
  (fn mm [f [h & r]]
    (lazy-seq
      (cons (f h)
        (if r (mm f r))))))

(defcheck solution-7f4df353
  (fn
    !
    [f c] (when
           (seq
             c)
            (lazy-seq
              (cons
                (f
                  (first
                    c))
                (!
                  f
                  (rest
                    c
                    )
                  )
                )
              )
            )
    ))

(defcheck solution-7f67d492
  (fn x [f coll]
    (if (empty? coll)
      nil
      (lazy-seq (cons (f (first coll))
                  (x f (rest coll)))))))

(defcheck solution-7f748025
  (fn custom-map
    [f coll]
    (if (first coll)
      (cons (f (first coll))
        (lazy-seq (custom-map f (rest coll)))))))

(defcheck solution-7f7d8d3
  (fn my-map [f s]
    (if-not (empty? s)
      (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-7fbdf0e6
  (fn mp [f s]
    (if (empty? s) '()
                   (cons (f (first s))
                     (lazy-seq (mp f (rest s)))))))

(defcheck solution-804e951f
  (fn my-map [func remain]
    (if (empty? remain)
      '()
      (lazy-seq (cons (func (first remain)) (my-map func (rest remain)))))))

(defcheck solution-80abb602
  (fn pam [f c]
    (when-let [s (seq c)]
      (lazy-seq
        (cons (f (first s)) (pam f (rest s)))))))

(defcheck solution-80b744c1
  (fn mp [f s] (if (first s)
                 (lazy-seq (cons (f (first s)) (mp f (next s))))
                 ())))

(defcheck solution-80e4eda7
  (fn new-map [f s]
    (if (empty? s) []
                   (cons
                     (f
                       (first s))
                     (lazy-seq
                       (new-map f
                         (rest s)))))))

(defcheck solution-80f945e1
  (fn mp [f s]
    (when-let [coll (seq s)]
      (cons (f (first coll))
        (lazy-seq (mp f (rest coll)))))))

(defcheck solution-81051f34
  (fn my-map
    [f s]
    (if (empty? s)
      '()
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-8123bfa5
  (fn m [f coll]

    (lazy-seq

      (when (seq coll)

        (cons (f (first coll)) (m f (rest coll)))))))

(defcheck solution-8163913e
  (fn m [f [i & others]] (if (empty? others) (list (f i)) (lazy-seq (cons (f i) (m f others))))))

(defcheck solution-818a2c2e
  (fn mymap [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-819ba208
  (fn g [f s]
    (when-let [v (first s)]
      (cons
        (f v)
        (lazy-seq (g f (rest s)))))))

(defcheck solution-81e57f25
  (fn mymap [f coll]
    (when-not (empty? coll)
      (lazy-seq (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-81f9cdc6
  (fn mp
    ([f coll] (mp f coll []))
    ([f [head & tail] result]
     (cons (f head) (lazy-seq (when (seq tail) (mp f tail)))))))

(defcheck solution-824981ed
  (fn mmap [fnc coll]
    (let [f (fn [fnct c]
              (when-let [s (seq c)]
                (cons (fnct (first s)) (mmap fnct (rest s)))))]
      (lazy-seq (f fnc coll)))))

(defcheck solution-829218fa
  (fn m [f l]
    (when-not (empty? l)
      (lazy-seq
        (cons
          (f (first l))
          (m f (rest l)))))))

(defcheck solution-82d7c4d3
  (fn my-map [f s] (lazy-seq (when (not (empty? s))
                               (cons (f (first s))
                                 (my-map f (rest s)))))))

(defcheck solution-8329ba46
  (fn my [func coll]
    (if (empty? coll)
      []
      (cons (func (first coll)) (lazy-seq (my func (rest coll))))
      )
    ))

(defcheck solution-83844e80
  (fn mymap [f c]
    [f c]
    (if (empty? c)
      []
      (lazy-seq
        (cons (f (first c)) (mymap f (rest c)))))))

(defcheck solution-839ed66f
  (fn _ [f coll]
    (if (not (seq coll))
      []
      (lazy-seq (cons (f (first coll)) (_ f (rest coll)))))))

(defcheck solution-83eff0c7
  (fn m [f c]
    (if (empty? c) nil
                   (cons (f (first c))
                     (lazy-seq (m f (rest c)))))))

(defcheck solution-84151db7
  (fn lazy-map [f s] (lazy-seq (cons (f (first s)) (when (next s) (lazy-map f (rest s)))))))

(defcheck solution-84310a03
  (fn m [f s]
    (lazy-seq
      (when-let [[x & more] (seq s)]
        (cons (f x)
          (m f more))))))

(defcheck solution-85328ddd
  (fn m [f v] (cons
                (f (first v))
                (lazy-seq (if (empty? (rest v)) nil (m f (rest v)))))))

(defcheck solution-85cd9804
  (fn f [g [a & b]] (if a (cons (g a) (lazy-seq (f g b))))))

(defcheck solution-85faaef
  (fn mmap [f [e & q]] (lazy-seq (cons (f e) (when q (mmap f q))))))

(defcheck solution-8615817f
  (fn [f coll]
    ((fn h [xs]
       (if (empty? xs)
         nil
         (lazy-seq (cons
                     (f (first xs))
                     (h (rest xs)))))) coll)))

(defcheck solution-863a2c5f
  (fn my-map [f seq-1]
    (cond
      (empty? seq-1) '()
      :else (lazy-seq (cons (f (first seq-1))
                        (my-map f (rest seq-1)))))))

(defcheck solution-864222f3
  (fn mp [f [x & xs]]

    (if (= xs nil)

      (cons (f x) [])

      (cons (f x) (lazy-seq (mp f xs))))))

(defcheck solution-8648460e
  (fn my-map [f s]
    (if (seq s)
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-8691e630
  (fn newMap [f s] (if (empty? s) (lazy-seq) (lazy-cat (lazy-seq (list (f (first s)))) (newMap f (rest s))))))

(defcheck solution-86f31490
  (fn x [f coll]
    (if (seq coll)
      (cons (f (first coll)) (lazy-seq (x f (rest coll)))))))

(defcheck solution-8710fb41
  (fn new-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (new-map f (rest s)))))))

(defcheck solution-873209bf
  (fn mymap [pred coll]
    (if (seq coll)
      (lazy-seq (cons (pred (first coll)) (mymap pred (rest coll))))
      )
    ))

(defcheck solution-87567572
  (fn m [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons
          (f (first s))
          (m f (rest s)))))))

(defcheck solution-87666d0c
  (fn meu-map [f s]
    (when-not (empty? s)
      (lazy-seq (cons (f (first s)) (meu-map f (next s)))))))

(defcheck solution-87a6ac32
  (fn re-map [f colls]
    (if colls
      (lazy-seq
        (cons (f (first colls))
          (re-map f (next colls)))))))

(defcheck solution-87e2d7b8
  (fn my-map [f x]
    (when (not (empty? x))
      (lazy-seq
        (cons (f (first x))
          (my-map f (rest x)))))))

(defcheck solution-87e50725
  (fn re-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first coll))
          (re-map f (rest coll)))))))

(defcheck solution-884b918d
  (fn m [f s] (if s (lazy-seq (let [[x & y] s] (cons (f x) (m f y)))) s)))

(defcheck solution-888a067a
  (fn tst [f a] (lazy-seq (if (not (seq a)) nil (cons (f (first a)) (tst f (rest a)))))))

(defcheck solution-88a05b39
  (fn mapp [f xs]
    (if (empty? xs)
      xs
      (lazy-seq (cons (f (first xs)) (mapp f (rest xs)))))))

(defcheck solution-88d6834f
  (fn my-map [f s]
    (if (empty? s) '()
                   (lazy-cat (list (f (first s))) (my-map f (rest s))))))

(defcheck solution-88edd5b2
  (fn mm
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (mm f (rest s))))))))

(defcheck solution-891af819
  (fn g [f l] (lazy-seq (when-let [[h & t] l] (cons (f h) (g f t))))))

(defcheck solution-895cb9de
  (fn mp [f s]
    (if (seq s)
      (cons (f (first s)) (lazy-seq (mp f (rest s)))))))

(defcheck solution-89c8d8a7
  (fn my-map [f s]
    (if (nil? s)
      '()
      (cons (f (first s)) (lazy-seq (my-map f (next s)))))))

(defcheck solution-89d17ea
  (fn mm [f [l & r]]
    (lazy-seq (cons (f l) (when r (mm f r))))))

(defcheck solution-8a12bb75
  (fn myMap [func coll]
    (lazy-seq
      (if (seq coll)
        (cons (func (first coll)) (myMap func (rest coll)))))))

(defcheck solution-8a13147d
  (fn mp [f s]
    (if (empty? (take 1 s))
      ()
      (lazy-seq (cons (f (first s))
                  (mp f (rest s)))))))

(defcheck solution-8a8d2bd3
  (fn lazy-map [f s] (if (next s) (cons (f (first s)) (lazy-seq (lazy-map f (next s)))) (list (f (first s))))))

(defcheck solution-8ad805e5
  (fn lazymap2 [f s]
    (if (seq s)
      (lazy-seq (cons (f (first s)) (lazymap2 f (rest s)))))))

(defcheck solution-8b5dfbdc
  (fn m [f [h & t :as l]]
    (if l (cons (f h) (lazy-seq (m f t))))))

(defcheck solution-8ba70deb
  (fn [f x]
    (letfn [(inn [f x]
              (when (seq x)
                (lazy-seq
                  (cons
                    (f (first x))
                    (inn f (rest x))))))]
      (inn f x)
      )
    ))

(defcheck solution-8c3a079
  (fn m [f coll]
    (rest (reductions #(f %2) nil coll))))

(defcheck solution-8c4cccce
  (fn m [f s] (
                if (not (nil? s))
                (lazy-seq (cons
                            (f (first s))
                            (m f (next s))
                            ))
                nil
                )))

(defcheck solution-8c6b3402
  (fn mymap
    [func lista]
    (if-not (empty? lista)
      (lazy-seq (cons (func (first lista)) (mymap func (rest lista))))
      [])))

(defcheck solution-8c85f8e
  (fn m [f col] (if (seq col)
                  (lazy-seq
                    (cons
                      (f (first col))
                      (m f (rest col)))))))

(defcheck solution-8c871ba7
  (fn m [f [h & t :as v]]
    (if (empty? v)
      ()
      (lazy-seq (cons (f h) (m f t))))))

(defcheck solution-8c889d02
  (fn my-map [f c] (if-not (seq c) (lazy-seq) (lazy-seq (cons (f (first c)) (my-map f (rest c)))))))

(defcheck solution-8cf3208e
  (fn new-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (new-map f (rest s)))))))

(defcheck solution-8d56a281
  (fn map* [f coll]
    (when (seq coll)
      (cons (f (first coll))
        (lazy-seq (map* f (rest coll)))))))

(defcheck solution-8df441de
  (fn m [f [x & xs]]
    (when x (cons (f x) (lazy-seq (m f xs))))))

(defcheck solution-8df5d7
  (fn mymap [f c]
    (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-8df7dbda
  (fn re-map [f coll]
    (if (empty? coll)
      ()
      (cons (f (first coll)) (lazy-seq (re-map f (rest coll)))))))

(defcheck solution-8e00edd3
  (fn myMap
    [pred x]
    (if (empty? x)
      []
      (cons (pred (first x))
        (lazy-seq (myMap pred (rest x)))))))

(defcheck solution-8e7e3fb
  (fn m [f [x & s]]
    (lazy-seq
      (and x
           (cons (f x) (m f s))))))

(defcheck solution-8ec811fe
  (fn m [f s]
    (if (empty? s)
      s
      (lazy-seq
        (cons
          (f (first s)) (m f (rest s)))))))

(defcheck solution-8ed3cd32
  (fn m [f coll]
    (lazy-seq (when-let [s (seq coll)]
                (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-8ed76b27
  (fn map2 [f coll]
    (if (empty? coll)
      []
      (cons (f (first coll)) (lazy-seq (map2 f (rest coll)))))))

(defcheck solution-8ef488c3
  (fn mymap [f xs]
    (if (empty? xs)
      []
      (lazy-seq (cons (f (first xs)) (mymap f (rest xs)))))))

(defcheck solution-8f42b389
  (fn mapping [f coll]
    (if (seq coll)
      (cons (f (first coll))
        (lazy-seq (mapping f (rest coll)))))))

(defcheck solution-8f59301f
  (fn mymap [f xs] (cons (f (first xs)) (if (next xs) (lazy-seq (mymap f (next xs))) []))))

(defcheck solution-8f624678
  (fn mymap
    [f xs]
    (if (empty? xs) xs
                    (cons (f (first xs)) (lazy-seq (mymap f (rest xs)))))))

(defcheck solution-8fa0a29b
  (fn _map [f coll]
    (lazy-seq
      (when (seq coll)
        (cons (f (first coll)) (_map f (rest coll)))))))

(defcheck solution-8fa38869
  (fn m [f [a & b]] (lazy-seq (if a (cons (f a) (m f b))))))

(defcheck solution-8fa70a3d
  (fn mp
    [func col]
    (if (not (empty? col))
      (lazy-seq
        (cons (func (first col)) (mp func (rest col)))))))

(defcheck solution-8fd1be05
  (fn my-map [f coll]
    (if (seq coll)
      (cons (f (first coll))
        (lazy-seq (my-map f (rest coll))))
      '())))

(defcheck solution-8fe8cee4
  (fn my-map [f coll]
    (lazy-seq
      (when-let [c (first coll)]
        (cons (f c) (my-map f (rest coll)))))))

(defcheck solution-906f8402
  (fn mp ([f coll]
          (lazy-seq
            (when-let [s (seq coll)]
              (cons (f (first s)) (mp f (rest s))))))))

(defcheck solution-908e459a
  (fn m ([f coll]
         (lazy-seq
           (when-let [s (seq coll)]
             (cons (f (first s)) (m f (rest s))))))))

(defcheck solution-90909e9d
  (fn mymap [f s]
    (if (empty? s) '()
                   (cons (f (first s)) (lazy-seq (mymap f (rest s)))))))

(defcheck solution-90d2e821
  #(letfn [(m [f a]
             (lazy-seq
               (when-let [s (seq a)]
                 (cons (f (first s)) (m f (rest s))))))]
     (m %1 %2)))

(defcheck solution-90d57aee
  (fn mp [f sq]
    (lazy-seq
      (when-let [[x & more] (seq sq)]
        (cons (f x)
          (mp f more))))))

(defcheck solution-910e640f
  (fn m [f [x & xs :as c]]
    (lazy-seq
      (if (seq c)
        (cons (f x) (m f xs))))))

(defcheck solution-91286376
  (fn m [f s]
    (if (empty? s) ()
                   (cons (f (first s))
                     (lazy-seq (m f (rest s)))))))

(defcheck solution-913927a2
  (fn m [f [x & xs]]
    (if x
      (cons (f x) (lazy-seq (m f xs)))
      [])))

(defcheck solution-9163d7ad
  (fn [f xs]
    (letfn [(m [f xs]
              (lazy-seq
                (when-let [t (seq xs)]
                  (if (empty? t)
                    nil
                    (cons (f (first t)) (m f (rest t)))))))]
      (m f xs))))

(defcheck solution-917f5114
  (fn m [f c]
    (lazy-seq
      (if (empty? c)
        c
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-927e9134
  (fn my-map [f coll]
    (if (empty? coll)
      []
      (cons (f
              (first coll))
        (lazy-seq
          (my-map f
            (rest coll)))))))

(defcheck solution-92867793
  (fn m [f [h & t :as xs]]
    (lazy-seq
      (if (empty? xs) []
                      (cons (f h) (m f t))))))

(defcheck solution-92997ec2
  (fn gmap [f coll]
    (lazy-seq
      (if-let [s (seq coll)]
        (cons (f (first s))
          (gmap f (rest s)))))))

(defcheck solution-93539e98
  (fn m [f c] (if (empty? c) [] (lazy-seq (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-936e7f7c
  (fn my-map [f v]
    (if (empty? v)
      []
      (lazy-cat [(f (first v))] (my-map f (rest v))))))

(defcheck solution-93a9305e
  (fn mymap
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (mymap f (rest s))))))))

(defcheck solution-93fb26f
  (fn m [f [x & xs]] (lazy-seq (when x (cons (f x) (m f xs))))))

(defcheck solution-94008ac5
  (fn [f lst]
    (letfn [(next [lst]
              (if (empty? lst)
                '()
                (lazy-seq (cons (f (first lst)) (next (rest lst))))))]
      (next lst))))

(defcheck solution-940a1ee0
  (fn ! [func coll]
    (if (empty? coll)
      nil
      (cons (func (first coll)) (lazy-seq (! func (rest coll))))
      )
    ))

(defcheck solution-94641481
  (fn mmap [f l]
    (if (empty? l)
      l
      (lazy-seq
        (cons (f (first l)) (mmap f (rest l)))))))

(defcheck solution-94663a36
  (fn newmap
    [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (newmap f (rest s)))))))

(defcheck solution-94d73fd2
  (fn my-map [f coll] (when-let [s (seq coll)] (cons (f (first coll)) (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-94dbf725
  (fn mymap [f s]
    (when-not (empty? s)
      (cons (f (first s))
        (lazy-seq (mymap f (rest s)))))))

(defcheck solution-94e8cc9
  (fn my-map [f coll]
    (when-let [x (first coll)]
      (lazy-seq (cons (f x) (my-map f (rest coll)))))))

(defcheck solution-955c40b2
  (fn mmap [f sx]
    (cond
      (empty? sx) []
      :e (cons (f (first sx)) (lazy-seq (mmap f (rest sx))))
      )))

(defcheck solution-958f06ea
  (fn __ [f c]
    (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (__ f (rest s)))))))

(defcheck solution-95a62f09
  (fn mp [f xs] (if (empty? xs) xs
                                (cons (f (first xs)) (lazy-seq (mp f (rest xs)))))))

(defcheck solution-961ba6b8
  (fn m [f c] (lazy-seq (if (seq c) (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-961ff952
  (fn mymap [f c]
    (if (nil? (seq c))
      nil
      (cons (f (first c)) (lazy-seq (mymap f (rest c))))
      )
    ))

(defcheck solution-966d5e4b
  (fn [f s] (lazy-cat (rest (reductions #(f %2) [] s)))))

(defcheck solution-96875930
  (fn [f coll] (rest (reductions #(f %2) nil coll))))

(defcheck solution-96aa33ea
  (fn map2 [f xs]
    (if (empty? xs)
      '()

      (cons (f (first xs)) (lazy-seq (map2 f (rest xs)))))
    ))

(defcheck solution-96c866a1
  (fn g [f s]
    (when s
      (lazy-seq (cons (f (first s)) (g f (next s)))))))

(defcheck solution-970684a7
  (fn mapper [func [first-arg & other-args]]
    (if first-arg
      (lazy-seq (cons (func first-arg) (mapper func other-args))))))

(defcheck solution-973dc28e
  (fn f [g [h & r]] (and h (cons (g h) (lazy-seq (f g r))))))

(defcheck solution-97930f78
  (fn my-map [f s] (if (empty? s) [] (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-98026845
  (fn re-map [f col]
    (if (seq col) (lazy-seq
                    (cons (f (first col)) (re-map f (rest col))))
                  nil)))

(defcheck solution-9864c207
  (fn my-lazy-map [f coll]
    (if (empty? coll)
      nil
      (lazy-seq (cons (f (first coll)) (my-lazy-map f (rest coll)))))))

(defcheck solution-988f7e99
  (fn map12 [f c] (when-let [s (seq c)] (cons (f (first s)) (lazy-seq (map12 f (rest s)))
                                          ))))

(defcheck solution-988f8a84
  (fn mymap [f l]
    (if (empty? l) ()
                   (cons (f (first l))
                     (lazy-seq (mymap f (rest l)))))))

(defcheck solution-98af124d
  (fn m [f s]
    (if (empty? s) s
                   (cons
                     (f (first s))
                     (lazy-seq (m f (rest s)))))))

(defcheck solution-98c572a7
  (fn mymap [f v] (if (not (empty? v)) (lazy-seq (cons (f (first v)) (mymap f (rest v)))))))

(defcheck solution-98f52495
  (fn mymap [f coll]
    (lazy-seq
      (when-let [c (seq coll)]
        (cons (f (first c)) (mymap f (rest c)))))))

(defcheck solution-9987e68a
  (fn my-map [f xs]
    (when-not (empty? xs)
      (lazy-seq (cons (f (first xs))
                  (my-map f (rest xs)))))))

(defcheck solution-99dc35e7
  (fn mymap [f l]
    (if (empty? l) []
                   (cons (f (first l)) (lazy-seq (mymap f (rest l))))
                   )))

(defcheck solution-9b2d81ff
  (fn m [f s]
    (lazy-seq
      (if-let [[x & s] (seq s)]
        (cons (f x) (m f s))))))

(defcheck solution-9b2f9a25
  (fn mapx [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first s)) (mapx f (rest s)))))))

(defcheck solution-9b3b737c
  (fn map- [f [x & xs]]
    (lazy-seq
      (if (empty? xs) [(f x)]
                      (cons (f x) (map- f xs))))))

(defcheck solution-9b595fcd
  (fn m
    ([f s]
     (if (empty? s) []
                    (lazy-seq (cons (f (first s)) (m f (rest s))))))))

(defcheck solution-9ba2650b
  (fn map' [f coll] (if (empty? coll)
                      coll
                      (lazy-seq (cons (f (first coll)) (map' f (rest coll)))))))

(defcheck solution-9cbf3b1d
  (fn my-map [func coll]
    (when (not-empty coll)
      (lazy-seq
        (cons
          (func (first coll))
          (my-map func (rest coll)))))))

(defcheck solution-9d31954d
  (fn mp [f [x & xs]]
    (if x (lazy-seq (cons (f x) (mp f xs))))))

(defcheck solution-9d3c4795
  (fn [f xs] (reductions #(f %2) (f (first xs)) (rest xs))))

(defcheck solution-9d5f96bb
  (fn m [f s] (if (nil? s) nil (lazy-seq (cons (f (first s)) (m f (next s)))))))

(defcheck solution-9dab30ca
  (fn mapy [f coll]
    (lazy-seq
      (let [c (seq coll)]
        (when c
          (cons
            (f (first c))
            (mapy f (rest c))))))))

(defcheck solution-9e25fa18
  (fn m [f c]
    (if (empty? c)
      '()
      (cons (f (first c)) (lazy-seq (m f (rest c)))))))

(defcheck solution-9e4ec074
  (fn mp [f v]
    (lazy-seq
      (if (not (empty? v))
        (cons (f (first v)) (mp f (rest v)))))))

(defcheck solution-9ea0957e
  (fn m
    [f s]
    (if (seq s)
      (cons
        (f (first s))
        (lazy-seq (m f (rest s))))
      [])))

(defcheck solution-9f1ff322
  (fn map* [f coll]
    (when-let [s (seq coll)]
      (cons (f (first s)) (lazy-seq (map* f (rest coll)))))))

(defcheck solution-9f72178d
  (fn f [d c] (if (first c) (lazy-seq (cons (d (first c)) (f d (rest c)))))))

(defcheck solution-a00e78c5
  (fn mp [f v]
    (if (seq v)
      (lazy-seq
        (cons
          (f (first v)) (mp f (next v)))))))

(defcheck solution-a0a077af
  (fn my-map [f c]
    (if (empty? c) '()
                   (lazy-cat [(f (first c))] (my-map f (rest c))))))

(defcheck solution-a0a4765d
  (fn map* [g s]
    (when-let [[f & r] (seq s)]
      (cons (g f) (lazy-seq (map* g r))))))

(defcheck solution-a13fa772
  (fn m [f ls]
    (if (empty? ls)
      ls
      (lazy-seq (cons (f (first ls)) (m f (rest ls)))))))

(defcheck solution-a1e7e208
  (fn mymap [f x]
    (when

     (not
       (empty? x)

       )
      (lazy-seq
        (cons (f (first x)) (mymap f (rest x)))
        )
      )
    ))

(defcheck solution-a2c2ef9d
  (fn _map [f [a & r]]
    (if a (cons (f a) (lazy-seq (_map f r))) nil)))

(defcheck solution-a2c561e1
  (fn mymap2 [f s]
    (lazy-seq
      (if (seq s)
        (cons (f (first s)) (mymap2 f (rest s)))))))

(defcheck solution-a2e60534
  (fn r [f s] (if (empty? s) nil (lazy-cat [(-> s first f)] (->> s rest (r f))))))

(defcheck solution-a306a489
  (fn m [f, s]
    (if (empty? s)
      nil
      (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-a39be160
  (fn m [f c] (if (empty? c) nil (concat [(f (first c))] (lazy-seq (m f (rest c)))))))

(defcheck solution-a42d18cd
  (fn map2 [f s]
    (if (empty? s) ()
                   (lazy-seq (cons (f (first s)) (map2 f (rest s)))))))

(defcheck solution-a472ab14
  (fn r [f xs]
    (lazy-seq
      (if (empty? xs)
        '()
        (cons (f (first xs)) (r f (rest xs)))))))

(defcheck solution-a4b9ef8d
  (fn m [f s] (if (empty? s) s
                             (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-a4ce0305
  (fn my-map [f xs]
    (lazy-seq
      (when-let [s (seq xs)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-a5679535
  (fn _ [f s]
    (lazy-seq
      (when-let [s (seq s)]
        (cons (f (first s)) (_ f (rest s)))))))

(defcheck solution-a5930e72
  (fn m [f [x & xs]]
    (cons
      (f x)
      (if (seq xs)
        (lazy-seq (m f xs))
        ))))

(defcheck solution-a5aa49fc
  (fn m [f s]
    (when-first [s1 s]
      (cons (f s1) (lazy-seq (m f (rest s)))))))

(defcheck solution-a5eac56
  (fn x [f c]
    (if-not (empty? c)
      (cons (f (first c)) (lazy-seq (x f (rest c)))))))

(defcheck solution-a5fa8205
  (fn m [f [x & xs :as s]]
    (lazy-seq (when (seq s) (cons (f x) (m f xs))))))

(defcheck solution-a70961f0
  (fn xmap [f lst]
    (cons (f (first lst))
      (lazy-seq
        (if (not (seq (rest lst)))
          (second lst)
          (xmap f (rest lst)))))))

(defcheck solution-a71b5172
  (fn z [f [s0 & s]] (if s0 (cons (f s0) (lazy-seq (z f s))))))

(defcheck solution-a76712a6
  (fn my-map [f [x & xs :as s]]
    (if (empty? s)
      nil
      (lazy-seq (cons (f x) (my-map f xs))))))

(defcheck solution-a7dba88e
  (fn k [f col]
    (if (empty? col)
      col
      (cons (f (first col)) (lazy-seq (k f (rest col)))))))

(defcheck solution-a7fd0b15
  (fn my-map [f [h & r]] (lazy-seq (cons (f h) (if r (my-map f r))))))

(defcheck solution-a8801bb4
  (fn mappy [f [a & b]]
    (if b
      (cons (f a) (lazy-seq (mappy f b)))
      (cons (f a) []))))

(defcheck solution-a88e1abe
  (fn my-map [f coll]
    (lazy-seq
      (when (seq coll)
        (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-a8b1c8e4
  (fn my-map [f s]
    (if s (lazy-seq (cons (f (first s)) (my-map f (next s)))))))

(defcheck solution-a91b30b6
  (fn [f S]
    (letfn [(mp [g s] (if (not-empty s) (lazy-seq (cons (g (first s)) (mp g (rest s))))))]
      (mp f S))
    ))

(defcheck solution-a92ed3bd
  (fn map* [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (map* f (rest s)))))))

(defcheck solution-a95b236a
  (fn -map [f [x & xs]]
    (if x
      (cons (f x)
        (lazy-seq (-map f xs)))
      ())))

(defcheck solution-a9b5b2c2
  (fn m [f [h & t :as c]] (if (empty? c) [] (lazy-seq (cons (f h) (m f t))))))

(defcheck solution-a9da8141
  (fn map* [f xs]
    (if-not (empty? xs)
      (cons (f (first xs)) (lazy-seq (map* f (rest xs)))))))

(defcheck solution-aa036bca
  (fn m [f c] (when (seq c) (cons (f (first c)) (lazy-seq (m f (next c)))))))

(defcheck solution-aa660891
  (fn map* [f x]
    (let [h (first x)
          r (rest x)]
      (if h
        (cons (f h) (lazy-seq (map* f r)))))))

(defcheck solution-aad9b698
  (fn newmap [f s]
    (if (not (empty? s))
      (lazy-seq
        (cons (f (first s)) (newmap f (rest s)))
        )
      )

    ))

(defcheck solution-ab57ee52
  (fn m [f [h & r]] (if h (cons (f h) (lazy-seq (m f r))))))

(defcheck solution-abc57dc
  (fn g [f xs] (lazy-seq (if (empty? xs) [] (concat (vector (f (first xs))) (g f (rest xs)))))))

(defcheck solution-abecaeb5
  (fn st [f s]
    (if (empty? s)
      []
      (lazy-seq
        (cons (f (first s)) (st f (rest s)))))))

(defcheck solution-ac331fbf
  (fn f [fnc col]
    (if (empty? col)
      '()
      (cons (fnc (first col)) (lazy-seq (f fnc (rest col)))))))

(defcheck solution-acdcf169
  (fn self [f col]
    (if (empty? col)
      ()
      (lazy-cat [(f (first col))]
        (self f (rest col))))))

(defcheck solution-ad7d6149
  (fn MAP [f s]
    (if (empty? s)
      nil
      (lazy-seq
        (cons (f (first s))
          (MAP f (rest s)))))))

(defcheck solution-addfe821
  (fn my-map [f coll]
    (when-let [s (seq coll)]
      (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-ae1b7e53
  (fn mymap [f coll]
    (lazy-seq
      (if (empty? coll)
        []
        (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-ae9c884e
  (fn m [f s]
    (when s
      (cons (f (first s)) (lazy-seq (m f (next s)))))))

(defcheck solution-af227c77
  (fn mapp [f a]
    (if
     (empty? a) a
                (lazy-seq
                  (cons (f (first a))
                    (mapp f (rest a)))))))

(defcheck solution-afee231e
  (fn m
    [f c]
    (lazy-seq (when-not (empty? c)
                (cons (f (first c)) (m f (rest c))))
      )))

(defcheck solution-b0117f19
  (fn map* [f coll]
    (lazy-seq
      (when-not (empty? coll)
        (cons (f (first coll)) (map* f (rest coll)))))))

(defcheck solution-b11940da
  (fn m [f s]
    (lazy-seq
      (when-let [[h & t] (seq s)]
        (cons (f h) (m f t))))))

(defcheck solution-b22aa041
  (fn a [f [p & q]]
    (if (empty? q)
      [(f p)]
      (lazy-seq (cons (f p) (a f q))))))

(defcheck solution-b238f15b
  (fn my-map [f coll]
    (if (seq coll)
      (lazy-seq (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-b24113be
  (fn my-map [f coll]
    (when (seq coll)
      (cons (f (first coll))
        (lazy-seq (my-map f
                    (rest coll)))))))

(defcheck solution-b254d755
  (fn map-fn [f xs]
    (if (empty? xs)
      ()
      (cons (f (first xs)) (lazy-seq (map-fn f (rest xs)))))))

(defcheck solution-b2b53763
  (fn peu [x y] (if (empty? y) [] (cons (x (first y)) (lazy-seq (peu x (rest y)))))))

(defcheck solution-b2bf363c
  (fn [f s]
    (letfn [(ff [f s]
              (if (empty? s)
                []
                (cons (f (first s))
                  (lazy-seq (ff f (rest s))))))]
      (ff f s))))

(defcheck solution-b35ba0ad
  (fn my-map [f xs]
    (when-let [xs (seq xs)]
      (lazy-seq
        (cons
          (f (first xs))
          (my-map f (next xs)))))))

(defcheck solution-b3668377
  (fn my-fn [f col]
    (if (empty? col)
      nil
      (lazy-seq (cons (f (first col))
                  (my-fn f (rest col)))))))

(defcheck solution-b38999b6
  (fn my-map
    ([f coll]
     (lazy-seq
       (when (seq coll)
         (cons (f (first coll))
           (my-map f (rest coll))))))
    ([f coll & colls]
     (let [colls (cons coll colls)]
       (when (every? seq colls)
         (lazy-seq (cons (apply f (my-map first colls))
                     (apply my-map f (my-map rest colls)))))))))

(defcheck solution-b3951a33
  (fn mmap [f s]
    (lazy-cat [(f (first s))] (when (second s) (mmap f (rest s))))))

(defcheck solution-b3cc34a1
  (fn func
    [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (func f (rest s)))))))

(defcheck solution-b401df3b
  (fn m [op v]
    (if (empty? v)
      v
      (lazy-seq (cons (op (first v)) (m op (rest v)))))))

(defcheck solution-b42780c4
  (fn m [f s] (if (not-empty s) (cons (f (first s)) (lazy-seq (m f (rest s)))) '())))

(defcheck solution-b4f41258
  (fn m [f v]
    (lazy-seq
      (when (seq v)
        (cons (f (first v)) (m f (rest v)))))))

(defcheck solution-b4fd7e5e
  (fn builder [op values]
    (lazy-seq
      (when-let [ss (seq values)]
        (cons (op (first values))
          (builder op (rest values)))))))

(defcheck solution-b53c2f2
  (fn m [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-b53d9415
  (fn mymap [f col]
    (if (empty? col)
      col
      (lazy-seq
        (cons (f (first col))
          (mymap f (rest col))))
      )
    ))

(defcheck solution-b584121c
  (fn mymap [f coll] ((fn inner [s1] (if (not= (rest s1) '()) (lazy-seq (cons (f (first s1)) (inner (rest s1)))) (list (f (first s1))))) coll)))

(defcheck solution-b5a37f7d
  (fn mAp [f s]
    (if (empty? s)
      '()
      (cons (f (first s)) (lazy-seq (mAp f (rest s)))))))

(defcheck solution-b5a6df08
  (fn re-implemnt-map [f xs]
    (if (empty? xs) (empty xs)
                    (lazy-seq (cons (f (first xs))
                                (re-implemnt-map f (rest xs)))))))

(defcheck solution-b5ab8759
  (fn mapf [f s]
    (if (empty? s) ()
                   (lazy-seq (cons (f (first s)) (mapf f (rest s)))))))

(defcheck solution-b5b97f31
  (fn map' [f coll] (lazy-seq (when (seq coll) (cons (f (first coll)) (map' f (rest coll)))))))

(defcheck solution-b5dab630
  (fn m [f [x & s]]
    (lazy-seq (if x (cons (f x) (m f s))))))

(defcheck solution-b65108fb
  (fn mapp [f coll]
    (if (empty? coll)
      '()
      (lazy-seq (cons (f (first coll)) (mapp f (rest coll)))))))

(defcheck solution-b682d446
  (fn [f l]
    (letfn [(map2 [f l]
              (if
               (empty? l)
                nil
                (cons (f (first l)) (lazy-seq (map2 f (rest l))))
                )
              )]
      (map2 f l)
      )
    ))

(defcheck solution-b704f2e9
  (fn my-map [f xs]
    (if (empty? xs)
      '()
      (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-b714251d
  (fn collect [f [fs & rs]]
    (lazy-seq
      (if (nil? fs)
        nil
        (cons (f fs) (collect f rs))))))

(defcheck solution-b72ae8e7
  (fn my-map [f coll]
    (if (seq coll)
      (lazy-seq (cons (f (first coll))
                  (my-map f (rest coll)))))))

(defcheck solution-b7c8d55c
  (fn mymap [f coll]
    (if (empty? coll) nil
                      (lazy-seq (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-b7f4a587
  (fn m [f c] (lazy-seq (when-let [[x & y] (seq c)] (cons (f x) (m f y))))))

(defcheck solution-b812c706
  (fn ff [f s] (if s (lazy-seq (cons (f (first s)) (ff f (next s)))) [])))

(defcheck solution-b8514190
  (fn map* [f [x & xs]] (when x
                          (lazy-seq (cons (f x) (map* f xs))))))

(defcheck solution-b91a86d5
  (fn my-map [f [a & b]]
    (lazy-seq
      (let [x (list (f a))]
        (if (seq b)
          (concat x (my-map f b))
          x)))))

(defcheck solution-b9b43d3a
  (fn m [f s] (lazy-seq (when (not-empty s) (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-b9e073b9
  (fn p118 [f [s1 & s]] (if (nil? s1) () (lazy-seq (cons (f s1) (p118 f s))))))

(defcheck solution-ba8d7a6e
  (fn my-map [f s]
    (when (not (empty? s))
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-ba92a977
  (fn my-lazy-map [f s]
    (if (empty? s)
      '()
      (lazy-seq (cons (f (first s)) (my-lazy-map f (rest s)))))))

(defcheck solution-ba9b92b4
  (fn mm [f s]
    (lazy-seq
      (let [l (seq s)]
        (when l
          (cons (f (first l)) (mm f (rest l))))))))

(defcheck solution-bac14dd9
  (fn mymap [f c]
    (if c
      (cons (f (first c)) (lazy-seq (mymap f (next c)))))))

(defcheck solution-bb1e665
  (fn mymap [f coll]
    (if (empty? coll)
      []
      (cons (f (first coll)) (lazy-seq (mymap f (rest coll)))))))

(defcheck solution-bb958521
  (fn map_ [f s]
    (if (empty? s) nil
                   (lazy-seq (cons (f (first s)) (map_ f (rest s)))))))

(defcheck solution-bbbc6636
  (fn my-map
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (my-map f (rest s))))))))

(defcheck solution-bbc72dd8
  (fn mymap [f coll]
    (let [[h & t] coll]
      (if (empty? coll)
        (list)
        (cons (f h) (lazy-seq (mymap f t))))
      )))

(defcheck solution-bc36d151
  (fn mymap [op coll]
    (if (empty? coll)
      coll
      (lazy-seq (cons (op (first coll)) (mymap op (rest coll)))))))

(defcheck solution-bc50554a
  (fn foo [f s & frst] (if (seq frst)
                         (lazy-seq (cons (first frst) (if s (foo f (next s) (f (first s))))))
                         (foo f (next s) (f (first s))))))

(defcheck solution-bc5061e2
  (fn mymap [f [head & tail]] (cons (f head) (if (seq tail) (lazy-seq (mymap f tail)) '()))))

(defcheck solution-bc52bf72
  (fn newmap [f c]
    (if (empty? c)
      '()
      (cons (f (first c)) (lazy-seq (newmap f (rest c)))))))

(defcheck solution-bc70b8ed
  (fn m [f s]
    (if (empty? s)
      nil
      (lazy-seq (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-bcaff970
  (fn m [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-bcba12dd
  (fn map2 [f s]
    (lazy-seq
      (when (seq s)
        (cons (f (first s)) (map2 f (rest s)))))))

(defcheck solution-bcbb0fce
  (fn fmap [f coll]
    (when-let [[h & t] (seq coll)]
      (cons (f h) (lazy-seq (fmap f t))))))

(defcheck solution-bcd929d7
  (fn [f coll]
    (reductions
      (fn [a b] (f b))
      (f (first coll))
      (rest coll))))

(defcheck solution-bcf7ca4
  (fn a [f xs] (if (empty? xs) [] (cons (f (first xs)) (lazy-seq (a f (rest xs)))))))

(defcheck solution-bcf8ab96
  (fn [f s]
    (letfn [(m [s]
              (if (empty? s)
                '()
                (lazy-seq (cons (f (first s)) (m (rest s))))))]
      (m s))))

(defcheck solution-bd204164
  (fn my-map [f coll]
    (let [coll (seq coll)]
      (if coll
        (lazy-seq (cons (f (first coll))
                    (my-map f (rest coll))))
        '()))))

(defcheck solution-bd7cff18
  (fn mymap [f c] (if (empty? c) c (cons (f (first c)) (lazy-seq (mymap f (rest c)))))))

(defcheck solution-be1c4251
  (fn [f seqs]
    (let [my-lazy-map (fn my-lazy-map [seqs]
                        (if (empty? seqs)
                          nil
                          (cons (f (first seqs)) (lazy-seq (my-lazy-map (rest seqs))))))]
      (my-lazy-map seqs))))

(defcheck solution-be36e548
  (fn mapp [f s]
    (if (empty? s)
      '()
      (cons (f (first s))
        (lazy-seq (mapp f (rest s)))))))

(defcheck solution-beb491a
  (fn my-map
    [f coll]
    (lazy-seq
      (if (empty? coll)
        '()
        (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-bed5c8a7
  (fn my-map [f s]
    (if (empty? s) []
                   (lazy-seq (cons (f (first s))
                               (my-map f (rest s)))))))

(defcheck solution-bedea0cb
  #(rest (reductions (fn [x _] (% x)) (first %2) %2)))

(defcheck solution-bfe10905
  (fn m [f [h & t]]
    (lazy-cat
      [(f h)]
      (if t
        (m f t)))))

(defcheck solution-c1a37fa6
  (fn mymap [f [x & xs]]
    (when x
      (lazy-seq (cons (f x) (mymap f xs))))))

(defcheck solution-c1c02cc9
  (fn mymap [f values]
    (if (empty? values) '()
                        (lazy-seq
                          (cons (f (first values))
                            (lazy-seq (mymap f (rest values))))))))

(defcheck solution-c1c71cac
  (fn x [f c]
    (lazy-seq
      (when (seq c) (cons (f (first c)) (x f (rest c)))))))

(defcheck solution-c2d48626
  (fn reimp-map
    ([f c]
     (lazy-seq
       (when-let [s (seq c)]
         (cons (f (first s)) (reimp-map f (rest s))))))))

(defcheck solution-c2fb1ade
  (fn lp [f coll]
    (when-not (nil? coll)
      (lazy-seq (cons (f (first coll))
                  (lp f (next coll)))))))

(defcheck solution-c30bd42f
  (fn mapper [fun xs]
    (if (empty? xs) []
                    (lazy-cat [(fun (first xs))] (mapper fun (rest xs))))))

(defcheck solution-c35366cc
  (fn m [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-c35a15d1
  (fn mp [f sq]
    (if (empty? sq) sq
                    (cons (f (first sq))
                      (lazy-seq (mp f (rest sq)))))))

(defcheck solution-c3651a7d
  (fn m [f [x & xs :as s]]
    (if (empty? s)
      []
      (lazy-seq (cons (f x) (m f xs))))))

(defcheck solution-c3b36518
  (fn map' [f s]
    (when (not-empty s)
      (cons (f (first s))
        (lazy-seq (map' f (rest s)))))))

(defcheck solution-c43b3e99
  (fn my-map [lambda seqs]
    (if (empty? seqs)
      seqs
      (lazy-cat
        (conj (empty seqs) (lambda (first seqs)))
        (my-map lambda (rest seqs))))))

(defcheck solution-c44a5d3b
  (letfn [
          (myfun [f col]
            (if (empty? col)
              (empty col)
              (cons (f (first col)) (lazy-seq (myfun f (rest col))))))]
    myfun))

(defcheck solution-c498f1e
  (fn collect [f [fs & rs]] (lazy-seq (if (nil? fs) nil (cons (f fs) (collect
                                                                       f rs))))))

(defcheck solution-c4b89092
  (fn me [f l]
    (if (next l)
      (cons (f (first l)) (lazy-seq (me f (next l))))
      (vector (if l (f (first l)))))))

(defcheck solution-c4c0c919
  (fn f [x c]
    (lazy-seq
      (when c
        (cons (x (first c)) (f x (next c)))))))

(defcheck solution-c51d4d33
  (fn mp [f xs]
    (concat
     [(f (first xs))]
     (when (not-empty (rest xs)) (lazy-seq
                                   (mp f (rest xs))))
     )))

(defcheck solution-c56d55d7
  (fn m [f s]
    (lazy-seq (if (seq s) (cons (f (first s)) (m f (rest s))) ()))))

(defcheck solution-c591d1de
  (fn m [f xs] (if (empty? xs) '() (let [fst (first xs) r (rest xs)] (cons (f fst) (lazy-seq (m f r)))))))

(defcheck solution-c5ad4e90
  (fn m [f c]
    (if (seq c)
      (lazy-seq
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-c5fd23e9
  (fn m [f [x & r :as v]]
    (if (empty? v)
      ()
      (lazy-seq
        (cons (f x) (m f r))))))

(defcheck solution-c658dfe5
  (fn m [f [x & xs :as s]]
    (if (seq s)
      (cons (f x) (lazy-seq (m f xs))))))

(defcheck solution-c66a696d
  (fn stupid-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (stupid-map f (rest s)))))))

(defcheck solution-c66ea747
  (fn mymap [f s]
    (if (nil? s) '()
                 (lazy-seq
                   (cons (f (first s)) (mymap f (next s)))))))

(defcheck solution-c69d1d03
  (fn [f s] (rest (reductions (fn [a b] (f b)) (first s) s))))

(defcheck solution-c6a58062
  (fn [f v] (rest (reductions #(f %2) [] v))))

(defcheck solution-c6d0c3ef
  (fn my-map [f s]
    (lazy-seq
      (when (seq s)
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-c6ea18c6
  (fn my-map2 [f lis]
    (if (not= (first lis) nil)
      (cons (f (first lis)) (lazy-seq (my-map2 f (rest lis)))))))

(defcheck solution-c6f51a54
  (fn my-map [f s]
    (if (empty? s) ()
                   (lazy-seq
                     (cons (f (first s))
                       (my-map f (rest s)))))))

(defcheck solution-c76f88dc
  (fn mi [f s]
    (lazy-seq
      (if (empty? s)
        ()
        (cons (f (first s))
          (mi f (rest s)))))))

(defcheck solution-c79a95b6
  (fn my-map
    [f xs]
    (lazy-seq (if (seq xs) (cons (f (first xs)) (my-map f (next xs)))))))

(defcheck solution-c79ae9dd
  (fn m [f [s & r]]
    (when s (cons (f s) (lazy-seq (m f r))))))

(defcheck solution-c7a46e57
  (fn my-map [f xs]
    (lazy-seq
      (if (empty? xs)
        '()
        (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-c8177dda
  (fn my-map [f l]
    (lazy-seq
      (when-let [s (seq l)]
        (if (empty? s)
          '()
          (cons (f (first s)) (my-map f (rest s))))))))

(defcheck solution-c817c66d
  (fn my-map [f args]
    (if (empty? args)
      nil
      (lazy-seq
        (cons (f (first args))
          (my-map f (rest args)))))))

(defcheck solution-c85bf26f
  (fn my-map [f [head & tail]]
    (when head
      (cons (f head)
        (lazy-seq (my-map f tail))))))

(defcheck solution-c86240a0
  (fn ! [f x]
    (if (seq x)
      (cons (f (first x)) (lazy-seq (! f (next x)))))))

(defcheck solution-c89c53dc
  (fn [f l]
    ((fn fm [[head & tail]]
       (lazy-seq
         (if (nil? head)
           nil
           (cons (f head) (fm tail)))))
     l)))

(defcheck solution-c8a5a465
  (fn my-map [f c1]
    (lazy-seq
      (if (empty? c1)
        nil
        (cons (f (first c1)) (my-map f (rest c1)))))))

(defcheck solution-c91112ce
  (fn m [f [h & t]] (cons (f h) (if t (lazy-seq (m f t)) t))))

(defcheck solution-c9d4c0a8
  (fn m [f s]
    (if (empty? (next s))
      [(f (first s))]
      (cons (f (first s))
        (lazy-seq (m f (rest s)))))))

(defcheck solution-c9d62e5
  (fn this [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (this f (rest s)))))))

(defcheck solution-ca072281
  (fn m [f c] (cons (f (first c)) (if (empty? (next c)) '() (lazy-seq (m f (next c)))))))

(defcheck solution-ca39eaf5
  (fn mymap [f s]
    (if-not (next s)
      (list (f (first s)))
      (lazy-seq
        (cons
          (f (first s))
          (mymap f (next s)))))))

(defcheck solution-ca40fc05
  (fn my-map
    [f s]
    (lazy-seq
      (if-not (seq s)
        s
        (cons (f (first s)) (lazy-seq (my-map f (rest s))))))))

(defcheck solution-ca518c85
  (fn m [f l]
    (if (empty? l)
      '()
      (lazy-seq (cons (f (first l))
                  (m f (rest l)))))))

(defcheck solution-ca7b0d6b
  (fn [f x] (rest (reductions #(f %2) nil x))))

(defcheck solution-cab68d29
  ; Couldn't do this with loop/recur so used  standard recursion
  (fn my-map2 [f s]
    (if (empty? s)
      (list)
      (lazy-seq (cons (f (first s)) (my-map2 f (rest s)))))))

(defcheck solution-cb57ed5f
  (fn implmap [f s]
    (lazy-seq
      (when-let [s (seq s)]
        (cons (f (first s)) (implmap f (rest s)))))))

(defcheck solution-cb5db0e4
  (fn mymap [f s]
    (lazy-seq
      (if (empty? s)
        nil
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-cc660cc0
  (fn qq [f s]
    (lazy-seq
      (if (empty? s)
        '()
        (cons (f (first s))
          (qq f (rest s)))))))

(defcheck solution-ccb42a70
  (fn M [f [h & t :as l]]
    (if h
      (lazy-seq (cons (f h) (M f t)))
      l)))

(defcheck solution-cce5ea13
  (fn [fun li]

    (
     (fn c [[f & r]]
       (if f
         (lazy-seq (cons (fun f) (c r)))
         nil
         )
       )
     li
     )

    ))

(defcheck solution-cd20bc49
  (fn tmp [f x]
    (if (nil? x)
      nil
      (cons (f (first x)) (lazy-seq (tmp f (next x))))
      ;    (cons (f (first x)) nil)
      )))

(defcheck solution-cdb3d8b9
  (fn self [f xs]
    (if (empty? xs)
      xs
      (let [[fst & rst] xs]
        (cons (f fst) (lazy-seq (self f rst)))))))

(defcheck solution-cdd4d3c8
  (fn m [f [x & xs]] (if (nil? x) [] (cons (f x) (lazy-seq (m f xs))))))

(defcheck solution-cdddf4b7
  (fn m [f s] (if-let [h (first s)]
                (cons (f h) (lazy-seq (m f (rest s))))
                ())))

(defcheck solution-cddfed9b
  (fn m [f c] (lazy-seq (if (seq c)
                          (cons (f (first c))
                            (m f (rest c)))))))

(defcheck solution-ce0e1c2e
  (fn mm [f s]
    (lazy-seq
      (let [fs (f (first s))]
        (if (next s)
          (cons fs (mm f (next s)))
          (list fs))))))

(defcheck solution-ce309862
  (fn f [the-fn col1]
    (let [the-seq (when (next col1)
                    (lazy-seq (f the-fn (rest col1))))]
      (cons (the-fn (first col1)) the-seq))))

(defcheck solution-ce445d92
  (fn m [f l]
    (lazy-seq
      (when (not (empty? l))
        (cons (f (first l)) (m f (rest l)))))))

(defcheck solution-ce754da9
  (fn my-map [f [fst & rst]]
    (if-not fst []
                (lazy-seq (cons (f fst) (my-map f rst))))))

(defcheck solution-cea981aa
  (fn mymap [f s]
    (lazy-seq
      (when-let [s (seq s)]
        (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-ceadf4e1
  (fn map' [f xs]
    (when (seq xs)
      (lazy-seq (cons (f (first xs)) (map' f (rest xs)))))))

(defcheck solution-cf102632
  (fn m [f c]
    (lazy-seq
      (when-not (empty? c)
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-cf1b09b2
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (cons (f (first s)) (my-map f (rest s)))))
    ))

(defcheck solution-cfef5bf0
  (fn my-map [f lst]
    (if (empty? lst) lst
                     (cons (f (first lst)) (lazy-seq (my-map f (rest lst)))))))

(defcheck solution-d01ca638
  (fn my-map [f s]
    (if (empty? s)
      nil
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-d02e4fe3
  (fn mapRec [p x]
    (if (empty? x)
      []
      (lazy-seq (cons (p (first x)) (mapRec p (rest x))))
      )
    ))

(defcheck solution-d031e01b
  (fn my-map [f s]
    (lazy-seq
      (if (empty? s)
        []
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-d13db43e
  (fn my-map [f xs]
    (if (empty? xs)
      []
      (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-d1b779d6
  (fn fun [f coll]
    (if (empty? coll)
      []
      (lazy-seq
        (cons (f (first coll)) (fun f (rest coll)))))))

(defcheck solution-d216cd3a
  (fn* maps [f x]
    (if (empty? x)
      ()
      (cons (f (first x)) (lazy-seq (maps f (rest x)))))))

(defcheck solution-d2326fd8
  (fn m [f col]
    (lazy-seq
      (when (not (empty? col)) (cons (f (first col)) (m f (rest col)))))))

(defcheck solution-d2ab5581
  (fn [f c] (letfn [(mp [[x & xs]] (if x (cons (f x) (lazy-seq (mp xs)))))]
              (mp c))))

(defcheck solution-d2b07e36
  (fn m [f s]
    (if (empty? (rest s))
      (list (f (first s)))
      (lazy-seq (cons (f (first s))
                  (m f (rest s))))
      )))

(defcheck solution-d31610df
  (fn my-map [f coll]
    (lazy-seq
      (let [x (first coll)]
        (if (= nil x)
          []
          (cons (f x)
            (my-map f (rest coll))))))))

(defcheck solution-d34fbd28
  (fn mm ([f coll]
          (lazy-seq
            (when-let [s (seq coll)]
              (cons (f (first s)) (mm f (rest s))))))))

(defcheck solution-d38a199d
  (fn my-map [f xs]
    (if (empty? xs) nil
                    (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-d392f57b
  (fn mp [f s]
    (if (empty? s) '()
                   (cons (f (first s)) (lazy-seq (mp f (rest s)))))))

(defcheck solution-d3f85e66
  (fn my-map [f s]
    (reductions #(f %2) (f (first s)) (rest s))))

(defcheck solution-d527d2f6
  (fn r [f s]
    (if (empty? s) '() (cons (f (first s)) (lazy-seq (r f (rest s)))))))

(defcheck solution-d54cafa1
  (fn mymap [f coll]
    (when (seq coll)
      (cons (f (first coll)) (lazy-seq (mymap f (rest coll)))))))

(defcheck solution-d6030f2e
  (fn m [f coll] (lazy-seq (if (empty? coll) coll (cons (f (first coll)) (m f (rest coll)))))))

(defcheck solution-d61ba499
  (fn [g y]
    (letfn [(exmap
              ([f x]
               (lazy-seq
                 (when-let [thing (seq x)]
                   (cons (f (first thing)) (exmap f (rest thing)))))))]
      (exmap g y))))

(defcheck solution-d62a0fc5
  (fn m [f s]
    (if (empty? s)
      s
      (cons (f (first s))
        (lazy-seq
          (m f (rest s)))))))

(defcheck solution-d66bf328
  (fn m [f [a & more]]
    (lazy-seq
      (cons (f a) (if more (m f more))))))

(defcheck solution-d6976516
  (fn mmp [f xs]
    (if (not-empty xs)
      (cons (f (first xs)) (lazy-seq (mmp f (rest xs)))))))

(defcheck solution-d6e3525c
  (fn my-map [f xs]
    (lazy-seq
      (if (empty? xs)
        xs
        (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-d7377e2
  (fn do-map [f col]
    ((fn step [xs]
       (lazy-seq
         (when-not (empty? xs)
           (cons (f (first xs)) (step (rest xs))))))
     col)))

(defcheck solution-d76ed39f
  (fn my-map [f [head & tail]]
    (if (nil? head)
      '()
      (cons (f head) (lazy-seq (my-map f tail))))))

(defcheck solution-d796768b
  (fn [f coll]
    (letfn [
            (mymap [f coll]
              (lazy-seq
                (let [s (seq coll)]
                  (when s
                    (cons (f (first s))
                      (mymap f (rest s)))))))
            ]
      (mymap f coll))))

(defcheck solution-d79dfeab
  (fn my-map [func xs] (if (empty? xs) nil (cons (func (first xs)) (lazy-seq (my-map func (rest xs)))))))

(defcheck solution-d7bd483f
  (fn f [foo [x & y]]
    (lazy-seq
      (cons (foo x)
        (if y (f foo y))))))

(defcheck solution-d8354524
  (fn m [f s]
    (lazy-seq
      (if (empty? s)
        nil
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-d8d72263
  (fn f [tf coll]
    (if (empty? coll)
      '()
      (let [v (first coll)]
        (lazy-seq (cons (tf v) (f tf (rest coll))))))))

(defcheck solution-d8eb5d65
  (fn mymap [f coll] (lazy-seq (when (seq coll) (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-d8edc850
  (fn mapj [f s]
    (cond
      (empty? s) '()
      :else
      (lazy-seq
        (cons (f (first s)) (mapj f (rest s)))))))

(defcheck solution-d92e4852
  (fn my-map [f c]
    (when-not (empty? c)
      (lazy-seq
        (cons (f (first c))
          (my-map f (rest c)))))))

(defcheck solution-d97e1974
  (fn mmap [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (mmap f (rest s)))))))

(defcheck solution-d9c3b784
  (fn mapp [f [x & xs]]
    (if x
      (cons (f x) (lazy-seq (mapp f xs))))))

(defcheck solution-d9d3f840
  (fn my-map [f coll]
    (lazy-seq
      (when-let [[x & xs] (seq coll)]
        (cons (f x) (my-map f xs))))))

(defcheck solution-d9e3ced0
  (fn map1 [f xs]
    (when (not (empty? xs))
      (lazy-seq (cons (f (first xs))
                  (map1 f (rest xs)))))))

(defcheck solution-d9edaa62
  (fn m [f coll]
    (lazy-seq
      (when-let [s (seq coll)] (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-da0dd86f
  (fn mp [f [x & xs]]
    (lazy-seq (if (nil? x) [] (cons (f x) (mp f xs))))))

(defcheck solution-da73888b
  (fn prob118 [f col]
    ;; return a lazy seq with f applied to each item in col
    (if (empty? col)
      nil
      (lazy-seq (cons (f (first col)) (prob118 f (rest col)))))))

(defcheck solution-da743639
  (fn m [f s]
    (when-let [a (first s)]
      (cons (f a)
        (lazy-seq (m f (rest s)))))))

(defcheck solution-dad20aab
  (letfn [(my-map [f s]
            (cond (empty? (rest s)) (cons (f (first s)) '())
                  :else (cons (f (first s))
                          (lazy-seq (my-map f (rest s))))))]
    my-map))

(defcheck solution-dad9d4d2
  (fn my-map [f xs]
    (if (empty? xs)
      ()
      (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-db14e9d6
  (fn my-map [f s]
    (when-let [ss (seq s)]
      (lazy-seq
        (cons (f (first ss)) (my-map f (rest ss)))))))

(defcheck solution-db2238ca
  (fn mp [f [x & xs]]
    (if x (cons (f x) (lazy-seq (mp f xs))))))

(defcheck solution-db9549aa
  (fn mappy [f s]
    (when (seq s)
      (cons (f (first s)) (lazy-seq (mappy f (rest s)))))))

(defcheck solution-dbe2418f
  (fn m [f xs]
    (if (empty? xs)
      ()
      (lazy-seq (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-dc1a2590
  (fn my-map
    [f coll]
    (if (seq coll)
      (lazy-seq (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-dc39acff
  (fn m [f xs]
    (when (seq xs)
      (lazy-seq (cons (f (first xs))
                  (m f (rest xs)))))))

(defcheck solution-dc4dae63
  (fn f [g a] (if (empty? a) () (cons (g (first a)) (lazy-seq (f g (rest a)))))))

(defcheck solution-dc623482
  (fn m [f xs]
    (when (seq xs) (lazy-seq (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-dcdb6e62
  (fn mp [f xs] (if (empty? xs) () (lazy-seq (cons (f (first xs)) (mp f (rest xs)))))))

(defcheck solution-dce0013c
  (fn m [f acoll]
    (lazy-seq
      (if (empty? acoll) []
                         (let [[x & xs] acoll]
                           (cons (f x) (m f xs)))))))

(defcheck solution-dcf7fde4
  (fn [f l] (letfn [(helper [[head & tail]] (lazy-seq (if (nil? head) nil (cons (f head) (helper tail)))))] (helper l))))

(defcheck solution-dd1c6c10
  (fn m [f coll]
    (if (empty? coll)
      ()
      (lazy-seq
        (cons (f (first coll)) (m f (rest coll)))))))

(defcheck solution-dd1ec930
  (fn mymap [f s]
    (if (seq s)
      (lazy-seq (cons (f (first s)) (mymap f (rest s))))
      nil)))

(defcheck solution-dd9b7a3e
  (fn m [f [elem & rest]] (lazy-seq (cons (f elem) (if (empty? rest) '() (m f rest))))))

(defcheck solution-dda3c434
  (fn map-2
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (cons (f (first s)) (map-2 f (rest s))))))))

(defcheck solution-ddd1a3c7
  (fn map' [f xs]
    (if (= xs '())
      '()
      (lazy-seq (cons (f (first xs)) (map' f (rest xs))))
      )))

(defcheck solution-de1f31f8
  (fn fr [f xs] (lazy-seq
                  (if (first xs)
                    (cons (f (first xs)) (fr f (rest xs))) ()))
    ))

(defcheck solution-de2f9537
  (fn my-map [f coll]
    (when-let [s (seq coll)]
      (lazy-seq
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-de3874e6
  (fn thisfunc [f s]
    (if (empty? s)
      ()
      (cons (f (first s))
        (lazy-seq
          (thisfunc f (rest s)))))))

(defcheck solution-de41eab5
  (fn re-map [f s]
    (if (seq s)
      (cons (f (first s)) (lazy-seq
                            (re-map f (rest s))))
      ())))

(defcheck solution-deb1315b
  (fn m [f [a & xs]]f
    (lazy-seq
      (cons (f a)
        (if (seq xs)
          (m f xs)
          (list))))))

(defcheck solution-debda72f
  (fn func [f [n & ns]]
    (lazy-seq (cons (f n) (if ns (func f ns))))))

(defcheck solution-ded70c0
  (fn new-map [f s]
    (if (empty? s)
      []
      (cons (f (first s)) (lazy-seq (new-map f (rest s)))))))

(defcheck solution-df27851
  (fn m [f [x & xs]]
    (if x (lazy-seq (cons (f x) (m f xs))))))

(defcheck solution-df4dea7f
  (fn my-map [f coll]
    (if (empty? coll)
      '()
      (lazy-seq (cons (f (first coll)) (my-map f (rest coll)))))))

(defcheck solution-df5ad5bd
  (fn mp [f coll] (if (empty? coll) '() (cons (f (first coll)) (lazy-seq (mp f (rest coll)))))))

(defcheck solution-df94da5d
  (fn my-map [f [x & r]]
    (lazy-seq (cons (f x) (if r (my-map f r))))))

(defcheck solution-dfb1e4e0
  (fn [f s] (rest (reductions #(f %2) + s))))

(defcheck solution-dff7ae6a
  (fn mymap [x y]
    (cons (x (first y)) (lazy-seq (when-not (empty? (rest y)) (mymap x (rest y)))))))

(defcheck solution-e04bd1ee
  (fn my-map [f s]
    (if (empty? s)
      []
      (lazy-seq (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-e0a7c4d3
  (fn o [f c]
    (if (empty? c)
      '()
      (lazy-seq
        (cons
          (f (first c))
          (o f (rest c))
          )))))

(defcheck solution-e109d3cd
  (fn m [f [x & xs]]
    (lazy-seq (cons
                (f x)
                (if xs
                  (m f xs)
                  ())
                ))
    ))

(defcheck solution-e124acc3
  (fn mymap [f col]
    (if (first col) (cons (f (first col)) (lazy-seq (mymap f (rest col)))))))

(defcheck solution-e1355d00
  (fn m [func coll]
    (if (seq coll)
      (lazy-seq (cons (func (first coll)) (m func (rest coll)))))))

(defcheck solution-e1381066
  (fn m [f s]
    (cons (f (first s)) (lazy-seq (if (empty? (rest s)) nil (m f (rest s)))))
    ))

(defcheck solution-e2090d92
  (fn mmap [f s] (lazy-seq (if (seq s) (cons (f (first s)) (mmap f (next s)))))))

(defcheck solution-e214a7d4
  (fn map-e
    [f x]
    (if (empty? x) '()
                   (lazy-seq (cons (f (first x)) (map-e f (rest x)))))))

(defcheck solution-e2362a04
  (fn [f s]
    ((fn p [t]
       (when-not (empty? t)
         (lazy-seq (cons (f (first t))
                     (p (rest t))))))
     s)))

(defcheck solution-e2758455
  (fn m [f c]
    (when-let [[x & xs] c]
      (cons (f x) (lazy-seq (m f xs))))))

(defcheck solution-e2798f67
  (fn p
    [f [x & xs]]
    (if x
      (lazy-seq (cons (f x) (p f xs)))
      [])))

(defcheck solution-e2917d6e
  (fn m [f s] (when (seq s) (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-e310ffc5
  (fn _map [function items]
    (if (empty? items)
      '()
      (lazy-seq (cons (function (first items)) (_map function (rest items)))))))

(defcheck solution-e33af79d
  (fn my-map [f v]
    (cons (f (first v))
      (lazy-seq
        (if (empty? (rest v))
          []
          (my-map f (rest v)))))))

(defcheck solution-e386779c
  (fn m [f xs]
    (if (empty? xs)
      ()
      (lazy-seq (cons (f (first xs)) (m f (rest xs)))))))

(defcheck solution-e4022b39
  (fn mp [f s]
    (lazy-seq
      (when (seq s)
        (cons (f (first s)) (mp f (rest s)))))))

(defcheck solution-e4224270
  (fn m [op xs] (when (seq xs) (lazy-seq (cons (op (first xs)) (m op (next xs)))))))

(defcheck solution-e53863f2
  (fn f [g xs]
    (lazy-seq
      (if (seq xs)
        (cons (g (first xs)) (f g (rest xs)))))))

(defcheck solution-e59ce455
  (fn my-map [f coll]
    (when coll
      (cons (f (first coll))
        (lazy-seq (my-map f (next coll)))))))

(defcheck solution-e61b09bc
  (fn my-map [f xs]
    (if (empty? xs) '()
                    (lazy-seq (cons (f (first xs)) (my-map f (rest xs)))))))

(defcheck solution-e6f86ac3
  (fn mmap [func coll]
    (lazy-seq
      (when-let [head (first coll)]
        (cons (func head) (mmap func (rest coll)))))))

(defcheck solution-e746fb00
  (fn m [f [h & r]]
    (if h
      (lazy-seq (cons (f h) (m f r))))))

(defcheck solution-e748aa4b
  (fn my-map [f coll] (lazy-seq (when-let [[x & more] coll] (cons (f x) (my-map f more))))))

(defcheck solution-e77b265d
  (fn m [f s]
    (if (empty? s)
      s
      (lazy-seq (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-e77deca1
  (fn m [f coll]
    (if (empty? coll)
      []
      (concat (vector (f (first coll)))
              (lazy-seq (m f (rest coll)))))))

(defcheck solution-e782198c
  (fn [f coll] (reductions #(f %2) (f (first coll)) (rest coll))))

(defcheck solution-e7b63dab
  (fn mymap [f coll]
    (if (false? (empty? coll))
      (lazy-seq
        (cons (f (first coll)) (mymap f (rest coll)))))))

(defcheck solution-e83cf090
  (fn g [f c]
    (lazy-seq (if ((complement empty?) c) (cons (f (first c)) (g f (rest c)))))))

(defcheck solution-e85e677a
  (fn m [f s]
    (if (seq s)
      (cons (f (first s))
        (lazy-seq (m f (rest s)))))))

(defcheck solution-e8a09c24
  (fn mymap [f xs]
    (if (seq xs)
      (cons (f (first xs))
        (lazy-seq (mymap f (rest xs)))))))

(defcheck solution-e8c36bba
  (fn my-map [f l]
    (lazy-seq
      (when-let [s (seq l)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-e8c659bc
  (fn f [m ls] (if (empty? ls) nil
                               (cons (m (first ls)) (lazy-seq (f m (rest ls)))))))

(defcheck solution-e920a61d
  (fn [f ys]
    (letfn [(go [xs]
              (if (empty? xs)
                []
                (let [x   (first xs)
                      xs' (rest xs)]
                  (cons (f x) (lazy-seq (go xs'))))))]
      (go ys))))

(defcheck solution-e94fed57
  (fn mp [f coll]
    (if (empty? coll)
      coll
      (cons (f (first coll)) (lazy-seq (mp f (rest coll)))))))

(defcheck solution-e96563d9
  (fn $map [f [x & xs]]
    (if x
      (cons (f x) (lazy-seq ($map f xs)))
      nil)))

(defcheck solution-e9c665c8
  (fn [ff cc]
    (letfn [(map2 [f coll]
              (lazy-seq
                (when-let [s (seq coll)]
                  (cons (f (first s)) (map2 f (rest s))))))]
      (map2 ff cc))))

(defcheck solution-e9d5f1c9
  (fn map' [f [h & t]]
    (when h
      (lazy-seq
        (cons (f h)
          (map' f t))))))

(defcheck solution-ea0b106d
  (fn mp [f x]
    (if (empty? x)
      []
      (concat (list (f (first x))) (lazy-seq (mp f (rest x))))
      )
    ))

(defcheck solution-ea3daaa4
  (fn my-map [f xs]
    (if (empty? xs) xs
                    (cons (f (first xs)) (lazy-seq (my-map f (rest xs)))))))

(defcheck solution-ea4f4450
  (fn m [f s]
    (if (seq s)
      (lazy-seq (cons (f (first s)) (m f (rest s))))
      ())))

(defcheck solution-eab87dd9
  (fn mymap
    [f s]
    (cond
      (empty? s) s
      :else (cons (f (first s)) (lazy-seq (mymap f (rest s)))))))

(defcheck solution-eaf0c2f0
  (fn mp [f [v & r]] (lazy-seq (cons (f v) (if-not (empty? r) (mp f r))))))

(defcheck solution-eb81a7ed
  (fn my-map [f coll]
    (lazy-seq
      (when-let [[x & more] (seq coll)]
        (cons (f x) (my-map f more))))))

(defcheck solution-ebe976fd
  (fn mymap [func col]
    (if
     (empty? col)
      col
      (cons (func (first col)) (lazy-seq (mymap func (rest col))))
      )
    ))

(defcheck solution-ec18cef8
  (fn m [f s] (lazy-seq (when-let [x (seq s)] (cons (f (first x)) (m f (rest x)))))))

(defcheck solution-ec3e02a6
  (fn m [f [first & rest]] (if first (cons (f first) (lazy-seq (m f rest))) ())))

(defcheck solution-ecd7bb98
  (fn mymap [f xs]
    (if (empty? xs)
      '()
      (lazy-seq
        (let [[x & xs] xs]
          (cons (f x) (mymap f xs)))))))

(defcheck solution-ed0e746e
  #(letfn [(map' [x] (lazy-seq (cons (%1 (first x)) (let [xs (rest x)] (when-not (empty? xs) (map' xs))))))] (map' %2)))

(defcheck solution-ed6f922a
  (fn my-map [f s]
    (if (empty? s)
      nil
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-ed8238ea
  (fn map2 [f s]
    (if (empty? s)
      nil
      (cons (f (first s))
        (lazy-seq (map2 f (rest s)))))))

(defcheck solution-edcf2e6c
  (fn m [f c]
    (if-let [frst (first c)]
      (lazy-seq
        (cons (f frst) (m f (rest c)))))))

(defcheck solution-edd83102
  (fn g
    [f x]
    (if (empty? x) []
                   (lazy-seq (cons (f (first x)) (g f (next x)))))))

(defcheck solution-ee4be23
  (fn m [f s]
    (if (seq s)
      (cons (f (first s))
        (lazy-seq (m f (rest s))))
      ())))

(defcheck solution-ee61d571
  (fn mmap [f xs]
    (if (empty? xs)
      xs
      (cons (f (first xs)) (lazy-seq (mmap f (rest xs)))))))

(defcheck solution-ee6243e0
  (fn my-map [f items]
    (if (empty? items)
      '()
      (cons
        (f (first items))
        (lazy-seq (my-map f (rest items)))))))

(defcheck solution-ee91b2f6
  (fn m [f [h & r]]
    (lazy-seq (if h (cons (f h) (m f r))
                    '()))))

(defcheck solution-eea0586f
  (fn fmap [f xs]
    (lazy-seq
      (if-let [[x & xs] (seq xs)]
        (cons (f x) (fmap f xs))))))

(defcheck solution-eec798ff
  (fn __ [f xs]
    (when (seq xs)
      (cons (f (first xs))
        (lazy-seq (__ f (rest xs)))))))

(defcheck solution-eede0872
  (fn mp [f s]
    (lazy-seq
      (if (seq s)
        (cons (f (first s)) (mp f (rest s)))
        '()))))

(defcheck solution-ef1abefb
  (fn my-map [f xs]
    (when-not (empty? xs)
      (cons (f (first xs))
        (lazy-seq (my-map f (next xs)))))))

(defcheck solution-f00d3eb8
  (fn mymap ([f xs] (if (empty? xs) [] (cons (f (first xs)) (lazy-seq (mymap f (rest xs))))))))

(defcheck solution-f077acfe
  (fn alt-map [f coll]
    (if (seq coll)
      (lazy-seq
        (cons (f (first coll)) (alt-map f (rest coll)))))))

(defcheck solution-f0a3559b
  (fn mymap [f x]
    (when-let [a (first x)]
      (lazy-seq (cons (f a) (mymap f (rest x)))))))

(defcheck solution-f0ef7b8b
  (fn my-map [f s]
    (when (seq s)
      (lazy-seq
        (cons (f (first s)) (my-map f (drop 1 s)))))))

(defcheck solution-f0f868f8
  (fn _map [pred s]
    (lazy-seq (cons (pred (first s))
                (if (nil? (next s))
                  nil
                  (_map pred (next s)))))))

(defcheck solution-f10b7743
  (fn my-map
    ([f s] (my-map f s (empty s)))
    ([f s result]
     (if (empty? s) result
                    (lazy-seq
                      (cons (f (first s))
                        (my-map f (rest s)
                          result)))))))

(defcheck solution-f10b7f84
  (fn my-map [f [x & xs]]
    (if (nil? x)
      ()
      (lazy-seq (cons (f x) (my-map f xs))))))

(defcheck solution-f18ff7c3
  (fn my-map [f s]
    (if (empty? s)
      nil
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-f1adbdf5
  (letfn [(m [f s]
            (if (seq s)
              (lazy-seq
                (cons (f (first s)) (m f (rest s))))))] m))

(defcheck solution-f1c228a6
  (fn mymap
    [f s]
    (if (first s)
      (cons (f (first s))
        (lazy-seq (mymap f (rest s))))
      ())))

(defcheck solution-f1fdf749
  (fn mp [f coll]
    (lazy-seq
      (if (empty? coll)
        '()
        (cons (f (first coll)) (mp f (rest coll)))))))

(defcheck solution-f23f1c48
  (fn map* [f [head & tail :as s]]
    (if (empty? s)
      []
      (cons (f head) (lazy-seq (map* f tail))))))

(defcheck solution-f25e1d62
  (fn my-map [f coll]
    (if (empty? coll) ()
                      (cons (f (first coll))
                        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-f26f5d95
  (fn g [f s]
    (if s
      (lazy-seq (cons (f (first s)) (g f (next s)))))))

(defcheck solution-f2e3bc07
  (fn tmp [f l]
    (if (empty? l) l
                   (lazy-seq (cons (f (first l)) (tmp f (rest l)))))))

(defcheck solution-f31692d2
  (fn m [f v]
    (if (empty? v) v
                   (lazy-seq (cons (f (nth v 0))
                               (m f (rest v)))))))

(defcheck solution-f3d7a40a
  (fn my-map [f [x & xs]]
    (if (seq xs)
      (lazy-seq (cons (f x) (my-map f xs)))
      (list (f x)))))

(defcheck solution-f465e9c8
  (fn rmap [f l]
    (when (not (empty? l))
      (lazy-seq
        (cons (f (first l))
          (rmap f (rest l)))))))

(defcheck solution-f480bd9
  (fn my-map [f coll]
    (lazy-seq
      (if (empty? coll)
        ()
        (cons (f (first coll))
          (my-map f (rest coll)))))))

(defcheck solution-f5307d0f
  (fn my-map [f coll]
    (if (empty? coll)
      nil
      (cons (f (first coll))
        (lazy-seq (my-map f (rest coll)))))))

(defcheck solution-f5342663
  (fn new-map [f [fst & rst]]
    (if (not (nil? fst))
      (lazy-cat [(f fst)] (new-map f rst)))))

(defcheck solution-f57fac52
  (fn rim [f xs]
    (when (seq xs)
      (lazy-seq (cons (f (first xs)) (rim f (rest xs)))))))

(defcheck solution-f5c56b8a
  (fn mymap [f s]
    (if (empty? s) ()
                   (lazy-seq (cons (f (first s))
                               (mymap f (rest s)))))))

(defcheck solution-f5d1305e
  (fn mymap [fun col] (if (empty? col) [] (lazy-seq (cons (fun (first col)) (mymap fun (rest col)))))))

(defcheck solution-f6220155
  (fn mm [f s]
    (when-let [c (seq s)] (cons (f (first c)) (lazy-seq (mm f (rest c)))))
    ))

(defcheck solution-f67d8b9e
  (fn m [f s]
    (if
     (not s) '()
             (lazy-seq (cons (f (first s)) (m f (next s)))))))

(defcheck solution-f6bd44cc
  (fn [f coll]
    (letfn [(my-map [coll]
              (if (empty? coll)
                '()
                (lazy-seq
                  (cons (f (first coll))
                    (my-map (rest coll))))))]
      (my-map coll))))

(defcheck solution-f6d68120
  (fn m [f [x & xs]]
    (lazy-seq
      (if xs
        (cons (f x) (m f xs))
        (list (f x))))))

(defcheck solution-f738b4e1
  (fn __ [f [e & r]]
    (lazy-seq
      (when e
        (cons (f e) (__ f r))))))

(defcheck solution-f7520e52
  (fn f [op xs]
    (if-not (empty? xs)
      (lazy-seq
        (cons
          (op (first xs))
          (f op (rest xs))
          ))
      )
    ))

(defcheck solution-f75ba5c6
  (fn mp [f x]
    (if (empty? x) nil
                   (lazy-seq
                     (cons (f (first x))
                       (mp f (rest x)))))))

(defcheck solution-f7d7a8ef
  (fn map2 [f [first & rest]]
    (cons (f first)
      (if-not (empty? rest)
        (lazy-seq (map2 f rest))))))

(defcheck solution-f8140bb7
  (fn map*
    [f coll]
    (when (seq coll)
      (cons (f (first coll)) (lazy-seq (map* f (rest coll)))))))

(defcheck solution-f818af1e
  (fn mymap [f s] (if-not (seq s) nil (lazy-seq (cons (f (first s)) (mymap f (rest s)))))))

(defcheck solution-f86753be
  (fn mmap [fcn [f & r]]
    (if f (cons (fcn f) (lazy-seq (mmap fcn r))))))

(defcheck solution-f8ab850c
  (fn map2 [f coll]
    (lazy-seq
      (when-first [x coll]
        (cons (f x) (map2 f (rest coll)))))))

(defcheck solution-f8b7ddf0
  (fn [fnc a-seq]
    (letfn [
            (mappus [fnc a-seq]
              (if (empty? a-seq)
                nil
                (cons (fnc (first a-seq))
                  (lazy-seq (mappus fnc (rest a-seq))))))]
      (mappus fnc a-seq))))

(defcheck solution-f8e0e37
  (fn m [f [s & R]]
    (if s
      (cons (f s)
        (lazy-seq (m f R))))))

(defcheck solution-f90857a2
  (fn my-map [f s]
    (if (not (empty? s))
      (cons (f (first s)) (lazy-seq (my-map f (rest s)))))))

(defcheck solution-f93b9a56
  (fn my-map [f col]
    (if (empty? col)
      []
      (cons (f (first col)) (lazy-seq (my-map f (rest col)))))))

(defcheck solution-f99c1e5b
  (fn map1 [op l]
    (lazy-seq
      (if (empty? l)
        []
        (cons (op (first l))
          (map1 op (rest l)))))))

(defcheck solution-f9c11ab4
  (letfn [(g [f s]
            (if (empty? s)
              []
              (lazy-seq (cons (f (first s)) (g f (rest s))))))]
    g))

(defcheck solution-fa4a0754
  (fn my-map [f [head & tail]]
    (lazy-seq (cons (f head) (when (seq tail) (my-map f tail))))))

(defcheck solution-fabe73dc
  (fn m [f s]
    (if (empty? s) '()
                   (cons (f (first s)) (lazy-seq (m f (rest s)))))))

(defcheck solution-fb9dac6d
  (fn new-map [f s]
    (if (not-empty s)
      (lazy-seq (cons (f (first s)) (new-map f (rest s)))))))

(defcheck solution-fc585ad7
  (fn m [f s]
    (lazy-seq
      (when (not (empty? s))
        (cons (f (first s)) (m f (rest s)))))))

(defcheck solution-fc7ba016
  (fn x [f l]
    (lazy-seq (if (empty? l) l (cons (f (first l)) (x f (rest l)))))
    ))

(defcheck solution-fcf65624
  (fn my-map [f coll]
    (lazy-seq
      (when-let [s (seq coll)]
        (if (empty? s)
          '()
          (cons (f (first s)) (my-map f (rest s))))))))

(defcheck solution-fd1ea95d
  (fn m [f c]
    (lazy-seq
      (when (seq c)
        (cons (f (first c)) (m f (rest c)))))))

(defcheck solution-fda3eff2
  (fn ! [func coll]
    (lazy-seq (if (empty? coll)
                []
                (cons (func (first coll)) (! func (rest coll)))
                ))
    ))

(defcheck solution-fe21cc3a
  (fn my-map [f lst]
    (if (empty? lst)
      []
      (cons (f (first lst))
        (lazy-seq (my-map f (rest lst)))))))

(defcheck solution-fe51b479
  (fn dumb-map
    [f [x & xs]]
    (if (coll? xs)
      (cons (f x) (lazy-seq (dumb-map f xs)))
      (list (f x)))))

(defcheck solution-fee8a8a2
  (fn map1 [f xs]
    (lazy-seq (when ((complement empty?) xs)
                (cons (f (first xs)) (map1 f (rest xs)))))))

(defcheck solution-ff432329
  (fn my-map [f x]
    (lazy-seq
      (if-let [s (seq x)]
        (cons (f (first s)) (my-map f (rest s)))))))

(defcheck solution-ff7538e5
  (fn ! [f args]
    (if (empty? args)
      nil
      (lazy-seq (cons (f (first args)) (! f (rest args)))))
    ))

(defcheck solution-ff8c64fd
  (fn m [f coll] (lazy-seq
                   (when-let [s (seq coll)]
                     (cons (f (first s)) (m f (rest s)))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-118))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

