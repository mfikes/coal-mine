(ns coal-mine.problem-60
  (:require [coal-mine.checks :refer [defcheck-60] :rename {defcheck-60 defcheck}]
            [clojure.test]))

(defcheck solution-1026249a
  (fn myreductions
    ([f coll] (myreductions f (first coll) (next coll)))
    ([f value coll]
     (cons value
       (when-let [e (first coll)]
         (lazy-seq (myreductions f (f value e) (next coll))))))))

(defcheck solution-10e9b156
  (fn redu
    ([f i s]
     (if (seq s)
       (lazy-seq
         (cons i
           (redu f (f i (first s)) (rest s))))
       [i]))
    ([f s]
     (redu f (first s) (rest s)))))

(defcheck solution-10f5f4b8
  (fn r
    ([f xs] (r f (first xs) (rest xs)))
    ([f i xs]
     (lazy-seq
       (cons i (when (seq xs) (r f (f i (first xs)) (rest xs))))))))

(defcheck solution-10f87186
  (fn reductions*
    ([f coll] (reductions* f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (cons init
         (when (seq coll)
           (reductions* f (f init (first coll)) (rest coll))))))))

(defcheck solution-1103fed7
  (fn r
    ([f v] (lazy-seq (cons (first v)
                       (when (seq v) (r f
                                       (cons
                                         (f (first v) (second v))
                                         (drop 2 v)))))))
    ([f s v] (lazy-seq (cons s
                         (when (seq v) (r f (f s (first v)) (rest v))
                                       ))))
    ))

(defcheck solution-11bb28a4
  (fn reduction
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reduction f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reduction f (f init (first s)) (rest s))))))))

(defcheck solution-11d0225
  (fn reds
    ([r s xs]
     (if
      (empty? xs)
       [s]
       (lazy-seq
         (cons
           s
           (reds r (r s (first xs)) (rest xs))))))
    ([r xs]
     (reds r (first xs) (rest xs)))))

(defcheck solution-12c17223
  (fn r
    ([f xs]
     (r f (first xs) (rest xs)))
    ([f v xs]
     (cons v (when (not (empty? xs))
               (let [nv (f v (first xs))]
                 (lazy-seq (r f nv (rest xs)))))))))

(defcheck solution-130d78df
  (fn reductions'
    ([f [head & tail]] (reductions' f head tail))
    ([f val coll]
     (letfn [(generate [f val coll]
               (when-let [[head & tail] (seq coll)]
                 (lazy-seq
                   (let [new-val (f val head)]
                     (cons new-val (generate f new-val tail))))))]
       (cons val (generate f val coll))))))

(defcheck solution-132ce421
  (fn reduc
    ([func seq]
     (reduc func (first seq) (rest seq)))
    ([func val seq]
     (cons val
       (if (empty? seq)
         '()
         (lazy-seq (reduc func (func val (first seq)) (rest seq))))))))

(defcheck solution-135d87d9
  (fn f
    ([g [i & l]] (f g i l))
    ([g i [a & b]] (if a (cons i (lazy-seq (f g (g i a) b))) [i]))))

(defcheck solution-137feb19
  (fn my-reductions [& xs]
    (let [f (first xs)
          i (if (= 3 (count xs)) (second xs) (first (last xs)))
          p (if (= 3 (count xs)) (last xs) (rest (last xs)))]
      ((fn walk [r rp]
         (lazy-seq
           (cons r (if (empty? rp) [] (walk (f r (first rp)) (rest rp)))))) i p))))

(defcheck solution-13f48f23
  (fn m
    ([f c] (m f (first c) (rest c)))
    ([f i c] (lazy-seq
               (if (empty? c)
                 (cons i c)
                 (cons i (m f (f i (first c)) (rest c))))))))

(defcheck solution-14215fb4
  (fn duce
    ([f xs]
     (duce f (first xs) (rest xs)))
    ([f x xs]
     (cons x (when (seq xs) (lazy-seq (duce f (f x (first xs)) (rest xs))))))))

(defcheck solution-14259546
  (fn myreduce
    ([fc vl coll]
     (if (empty? coll) [vl]
                       (let [r1 (fc vl (first coll))
                             rr (cons vl (lazy-seq (myreduce fc r1 (rest coll))))]
                         rr)
                       ))
    ([fc coll] (myreduce fc (first coll) (rest coll)))
    ))

(defcheck solution-148a607a
  (fn my-reductions
    ([f init xs] (if (empty? xs) [init] (cons init (lazy-seq (my-reductions f (f init (first xs)) (rest xs))))))
    ([f xs] (my-reductions f (first xs) (rest xs)))))

(defcheck solution-14f5a701
  (fn f
    ([g [x & r]] (f g x r))
    ([g a [x & r :as l]]
     (lazy-seq (cons a (if (seq l) (f g (g a x) r)))))))

(defcheck solution-1512135d
  (fn r
    ([f [i & o]] (r f i o))
    ([f i o]
     (cons i (when (not (empty? o)) (lazy-seq (r f (f i (first o)) (rest o))))))))

(defcheck solution-1568378b
  (fn d
    ([f x [h & r]] (cons x (if h (lazy-seq (d f (f x h) r)))))
    ([f [h & r]] (d f h r))))

(defcheck solution-166a3e0f
  (fn r
    ([f c]
     (if (empty? c)
       (f)
       (r f (first c) (rest c))))
    ([f i c]
     (cons i
       (if (not-empty c)
         (lazy-seq (r f (f i (first c)) (rest c))))))))

(defcheck solution-168fcef0
  (fn r
    ([f b] (r f (first b) (rest b)))
    ([f a b]
     (lazy-seq
       (if (empty? b) [a]
                      (let [x (f a (first b))]
                        (cons a (r f x (rest b)))))))))

(defcheck solution-16d4cb74
  (fn t
    ([f s] (t f (first s) (rest s)))
    ([f v s]
     (if (empty? s)
       [v]
       (cons v (lazy-seq (t f
                           (f v (first s))
                           (rest s))))))))

(defcheck solution-1762dcb3
  (letfn
   [(my-red [f i [h & t :as ls]]
      (let [h-p (f i h)]
        (if (empty? t) [h-p]
                       (lazy-cat [h-p] (my-red f h-p t)))))]
    (fn
      ([f [h & t]] (cons h (my-red f h t)))
      ([f i [h & t :as ls]] (cons i (my-red f i ls))))))

(defcheck solution-184aecad
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-1887147c
  (fn red
    ([f v] (if (empty? v)
             nil
             (red f (first v) (rest v))))
    ([f p v] (if (empty? v)
               (cons p nil)
               (cons p (lazy-seq (red f (f p (first v)) (rest v))))))))

(defcheck solution-18c92d92
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (next coll)))
    ([f arg coll]
     (cons arg
       (when coll
         (lazy-seq (my-reductions f (f arg (first coll)) (next coll))))))))

(defcheck solution-1920037b
  (fn foo
    ([f xs] (foo f (first xs) (rest xs)))
    ([f acc xs] (if (empty? xs)
                  [acc]
                  (lazy-seq (cons acc (foo f (f acc (first xs)) (rest xs))))))))

(defcheck solution-19711ce4
  (fn test60
    ([f x] (test60 f (first x) (rest x)))
    ([f x xs]
     (lazy-seq
       (if (empty? xs)
         (list x)
         (cons x (test60 f (f x (first xs)) (rest xs))))))))

(defcheck solution-198094cd
  (fn m
    ([o r] (m o (first r) (rest r)))
    ([o x r] (cons x (if (empty? r)
                       r
                       (lazy-seq (m o (o x (first r)) (rest r))))))))

(defcheck solution-1a21695d
  (fn lazy-red
    ([f lastval in]
     (when-let [a (first in)]
       (lazy-seq (cons lastval (lazy-red f lastval in nil)))))
    ([f lastval in junk]
     (when-let [a (first in)]
       (let [val (f lastval (first in))]
         (lazy-seq (cons val (lazy-red f val (rest in) nil))))))
    ([f in]
     (when-let [a (first in)]
       (lazy-seq (cons (f (first in)) (lazy-red f (f (first in)) (rest in) nil)))))))

(defcheck solution-1a49ac8b
  (fn myreductions
    ([f coll] (myreductions f (first coll) (rest coll)))
    ([f initial coll]
     (if (empty? coll)
       (cons initial nil)
       (lazy-seq (cons initial
                   (myreductions f (f initial (first coll))
                     (rest coll))))))))

(defcheck solution-1a9db2ba
  (fn g
    ([f [x & s]] (g f x s))
    ([f a [x & s]]
     (lazy-seq
       (cons a (if x (g f (f a x) s)))))))

(defcheck solution-1b2fe9e8
  (fn rd
    ([f l]
     (rd f (first l) (rest l))
     )
    ([f i l]
     (lazy-seq
       (if-let [s (seq l)]
         (cons i
           (rd f (f i (first s)) (rest s))
           )
         (list i)
         )))
    ))

(defcheck solution-1b3ee037
  (fn my-red2
    ([func initial_value lst]
     (if (empty? lst) (list initial_value)
                      (lazy-cat (list initial_value) (my-red2 func (func initial_value (first lst)) (rest lst)))))
    ([func lst]
     (if (empty? lst) '(0)
                      (my-red2 func (first lst) (rest lst))))))

(defcheck solution-1b4fa05d
  (fn reductions*
    ([f coll]
     (reductions* f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (if (empty? coll)
           []
           (reductions* f (f init (first coll)) (rest coll))))))))

(defcheck solution-1b7e720b
  (fn reds
    ([f [x & xs]]
     (reds f x xs))
    ([f x xs]
     (if (empty? xs)
       [x]
       (lazy-seq
         (cons
           x
           (reds f (f x (first xs)) (rest xs))))))))

(defcheck solution-1bdd7ba1
  (fn p60
    ([f ls0] (p60 f 0 [] ls0))
    ([f st ls0] (cons st (p60 f st [] ls0)))
    ([f st ls1 ls0]
     (if (empty? ls0) nil
                      (let [arg (conj ls1 (first ls0))]
                        (lazy-seq (cons (reduce f st arg) (p60 f st arg (next ls0)))))))))

(defcheck solution-1c0006aa
  (fn myred
    ([f c] (myred f (first c) (rest c)))
    ([f i c]
     (lazy-seq
       (cons i
         (lazy-seq (if-not (empty? c) (myred f (f i (first c)) (rest c)))))))))

(defcheck solution-1cad2a71
  (fn seqred
    ([oper s] (seqred oper (first s) (rest s)))
    ([oper felem s] (lazy-seq
                      (if-let [s (seq s)]
                        (cons felem (seqred oper (oper felem (first s)) (rest s))) (vector felem))))))

(defcheck solution-1cca58b3
  (fn reduct
    ([f coll] (reduct f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       [init]
       (lazy-cat [init] (reduct f (f init (first coll)) (rest coll)))))))

(defcheck solution-1cd6fc6c
  (fn -reducts
    ([-fn -col]
     (-reducts -fn (-fn (first -col)) (next -col))
     )
    ([-fn -base -col]
     (cons
       -base
       (lazy-seq
         (when-let [s (seq -col)]
           (-reducts -fn (-fn -base (first s)) (next s)))))
     )))

(defcheck solution-1d0bd244
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f v s]
     (if (nil? (first s))
       [v]
       (lazy-seq
         (cons v
           (r f (f v (first s)) (rest s)))))
     )
    ))

(defcheck solution-1d0d83f1
  (fn redux
    ([f e c]
     (let [[h & t] c]
       (lazy-cat (list e) (if (seq t)
                            (redux f (f e h) t)
                            (list (f e h))))))
    ([f coll]
     (redux f (first coll) (rest coll)))))

(defcheck solution-1d772266
  (fn r
    ([f s] (r f (first s) (next s)))
    ([f i s] (lazy-seq
               (cons i
                 (when s
                   (r f (f i (first s)) (next s))))))))

(defcheck solution-1db46f70
  (fn go
    ([f xs]
     (if-let [[x & more] (seq xs)]
       (go f x more)
       (f)))
    ([f v xs]
     (cons
       v
       (if-let [[x & more] (seq xs)]
         (let [new-v (f v x)]
           (lazy-seq (go f new-v more)))
         '())))))

(defcheck solution-1e1044d1
  (fn r
    ([f [h & t]] (r f h t))
    ([f i s]
     (letfn [
             (r [f c s]
               (if (empty? s)
                 []
                 (let [[h & t] s c (f c h)]
                   (lazy-seq (cons c (r f c t))))))]
       (cons i (r f i s))))))

(defcheck solution-1eeb27eb
  (fn r
    ([f [x & s]] (r f x s))
    ([f x s]
     (lazy-seq
       (cons x
         (if (empty? s) ()
                        (r f (f x (first s)) (rest s))))))))

(defcheck solution-1f479385
  (fn _reduce
    ([f c] (cons (first c) (lazy-seq (_reduce f (f (first c) (second c)) (next (next c)) true))))
    ([f arg c] (cons arg (lazy-seq (_reduce f (f arg (first c)) (next c) true))))
    ([f r c n] (if (not (nil? (first c))) (cons r (lazy-seq (_reduce f (f r (first c)) (next c) true))) [r]))
    ))

(defcheck solution-1f7633b2
  (fn red
    ([f [a b & coll]] (cons a (red f (f a b) coll)))
    ([f res coll] (if (empty? coll)
                    (list res)
                    (let [[cur & r] coll
                          next (f res cur)]
                      (lazy-seq (cons res (red f next r))))))))

(defcheck solution-1fbf7d8a
  (fn rctions
    ([f coll] (rctions f (first coll) (rest coll)))
    ([f init [x & rest]]
     (lazy-seq
       (if (nil? x)
         (vector init)
         (cons init
           (rctions f
             (f init x)
             rest)))))))

(defcheck solution-2067b319
  (fn r ([f coll] (r f (first coll) (rest coll)))
    ([f val coll]
     (if (empty? coll)
       [val]
       (cons val (lazy-seq (r f (f val (first coll)) (rest coll))))))))

(defcheck solution-209732ce
  (fn stepped-reduce
    ([f coll]
     (stepped-reduce f (first coll) (drop 1 coll)))
    ([f start coll]
     (if (empty? coll)
       [start]
       (lazy-seq
         (concat
          [start]
          (stepped-reduce f (f start (first coll)) (rest coll))))))))

(defcheck solution-20b1b576
  (fn red
    ([f xs] (red f (first xs) (rest xs)))
    ([f val xs]
     (if (empty? xs) [val]
                     (cons val
                       (lazy-seq (red f (f val (first xs)) (rest xs))))))))

(defcheck solution-20d48317
  (letfn [(reductions'
            ([f coll] (lazy-seq (reductions' f (first coll) (rest coll))))
            ([f init coll]
             (cons init
               (lazy-seq
                 (when-let [s (seq coll)]
                   (reductions' f (f init (first s))
                     (rest s)))))))] reductions'))

(defcheck solution-20e3ed3
  (fn reduce'
    ([f seq]
     (reduce' f (first seq) (rest seq)))
    ([f init seq]
     (cons init
       (when (not (empty? seq))
         (lazy-seq
           (reduce' f (f init (first seq)) (rest seq))))))))

(defcheck solution-2108b48a
  (fn reduction
    ([f coll] (reduction f (first coll) (next coll)))
    ([f i coll]
     (concat [i]
             (when-let [s coll]
               (lazy-seq (reduction f (f i (first s)) (next s)))
               )))))

(defcheck solution-212db057
  (fn reds
    ([f args]
     (if (sequential? args)
       (reds f (first args) (rest args))
       [f]))
    ([f start args]
     (cons start
       (lazy-seq
         (when-let [args (seq args)]
           (reds f (f start (first args)) (rest args))))))))

(defcheck solution-215d6775
  (fn myfunc
    ([func col] (myfunc func (first col) (rest col)))
    ([func init col]
     (cons init
       (lazy-seq (when-not (empty? col) (myfunc func (func init (first col)) (rest col))))

       )
     )))

(defcheck solution-21954bcb
  (fn rs
    ([f [i & s]]
     (rs f i s))
    ([f i [x & xs]]
     (cons i (lazy-seq (when x (rs f (f i x) xs)))))))

(defcheck solution-21e1c453
  (fn __
    ([f n] (__ f (first n) (rest n)))
    ([f s n]
     (if (empty? n)
       [s]
       (cons s (lazy-seq (__ f (f s (first n)) (rest n))))))))

(defcheck solution-2225590c
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (when (seq coll)
         (lazy-seq (red f (f init (first coll)) (rest coll))))))))

(defcheck solution-22bd9334
  (fn r ([f s [x & xs]] (if x (cons s (lazy-seq (r f (f s x) xs))) [s])) ([f [x & xs]] (r f x xs))))

(defcheck solution-2314265a
  (fn [f & [a e]]
    ((fn w [a c]
       (lazy-seq
         (if (empty? c)
           [a]
           (cons a (w (f a (first c)) (rest c))))))
     (if e a (first a))
     (or e (rest a)))))

(defcheck solution-233d200c
  (fn rreduce
    ([rf coll]
     (if (seq coll)
       (rreduce rf (first coll) (rest coll))
       (list (rf))))
    ([rf val coll]
     (lazy-seq
       (cons val
         (when (seq coll)
           (rreduce rf (rf val (first coll)) (rest coll))))))))

(defcheck solution-233dd86a
  (fn rs ([f c] (map-indexed (fn [i e] (reduce f (take (inc i) c))) c)) ([f i c] (rs f (cons i c)))))

(defcheck solution-23be0075
  (fn reds
    ([op items]
     (reds op (first items) (rest items)))
    ([op init items]
     (if (empty? items)
       (list init)
       (lazy-seq (cons init (reds op (op init (first items)) (rest items))))))))

(defcheck solution-242c6ea
  (fn my-reductions
    ([f xs]
     (my-reductions f (first xs) (next xs)))
    ([f init [x & xs]]
     (cons
       init
       (when x
         (lazy-seq
           (my-reductions
             f
             (f init x)
             xs)))))))

(defcheck solution-249d75e6
  (fn my-reduction
    ([f items] (my-reduction f (first items) (rest items)))
    ([f init items]
     (lazy-seq
       (if (empty? items)
         (list init)
         (let [i (first items)
               r (rest items)]
           (cons init (my-reduction f (f init i) r))))))))

(defcheck solution-24a3a36b
  (fn f
    ([g [h & t]] (f g h t))
    ([g i [h & t]]
     (lazy-seq
       (cons i
         (if h
           (f g (g i h) t)))))))

(defcheck solution-25253fe7
  (fn m-reductions
    ([op v col] (m-reductions op (cons v col)))
    ([op [h & tail]]
     (->>
       (iterate (fn [[v [h & tail :as col]]]
                  (if (empty? col)
                    [v]
                    [(op v h) tail]))
         [h tail])
       (take-while next)
       (map first)))))

(defcheck solution-2531b5bb
  (fn p60
    ([f coll]
     (p60 f (first coll) (rest coll)))
    ([f val coll]
     (lazy-seq
       (if (empty? coll) (list val)
                         (cons val
                           (p60 f (f val (first coll))
                             (rest coll))))))))

(defcheck solution-256a3030
  (fn [f i & [a]]
    (let [b (if (nil? a) i (cons i a))]
      (map
        #(reduce f (take % b))
        (map #(inc %2) b (range))))))

(defcheck solution-260817ef
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll) [init]
                       (let [reduction (f init (first coll))]
                         (lazy-seq (cons init (red f reduction (rest coll))))
                         )
                       )
     )
    ))

(defcheck solution-264bd671
  (fn reds
    ([func xs]
     (reds func (first xs) (rest xs)))
    ([func init xs]
     (cons init
       (when (seq xs)
         (lazy-seq (reds func (func init (first xs)) (rest xs))))))))

(defcheck solution-2655795a
  (fn my-reductions
    ([f i s]
     (lazy-seq
       (cons i (if (first s) (my-reductions f (f i (first s)) (rest s))))))
    ([f [i & r]]
     (my-reductions f i r))))

(defcheck solution-265c24ec
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f v s]
     (if (seq s)
       (let [h (first s)
             n (f v h)]
         (cons v (lazy-seq (r f n (rest s))))) (list v)))))

(defcheck solution-267d6107
  (fn seq-reductions
    ([x y]
     (seq-reductions x (first y) (rest y)))
    ([x y z]
     (seq-reductions x y z []))

    ([x y z w]
     (if (counted? z)
       (if (= 0 (count z))
         (vector (reduce x y w))
         (lazy-seq (cons (reduce x y w) (seq-reductions x y (rest z) (conj w (first z))))))
       (lazy-seq (cons (reduce x y w) (seq-reductions x y (rest z) (conj w (first z)))))))))

(defcheck solution-26a1bf14
  (fn f
    ([op col] (f op (first col) (rest col)))
    ([op init [h & t]]
     (if (nil? h)
       [init]
       (concat [init]
               (lazy-seq (f op (op init h) t)))))))

(defcheck solution-26a72e98
  (fn x
    ([f coll]
     (x f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       [init]
       (cons init (lazy-seq (x f (f init (first coll)) (rest coll))))))))

(defcheck solution-26bb6cc1
  (fn r
    ([f [h & t]]
     (r f h t))
    ([f v [h & t]]
     (lazy-seq
       (cons v (when h (r f (f v h) t)))))))

(defcheck solution-26bc958a
  (letfn [(worker [c f l]
            (if (empty? l)
              (lazy-seq (list c))
              (lazy-seq (cons c
                          (worker (f c (first l)) f (rest l))))))]
    (fn ([f l]
         (worker (first l) f (rest l)))
      ([f i l]
       (worker i f l)))))

(defcheck solution-26c5d89e
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [l (seq coll)]
         (my-reductions f (f (first coll)) (rest coll)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [l (seq coll)]
           (my-reductions f (f init (first l)) (rest l))))))
    ))

(defcheck solution-26e74171
  (fn my-reductions
    ([f iv s]
     (if (not-empty s)
       (cons iv (lazy-seq (my-reductions f (f iv (first s)) (rest s))))
       (list iv)))
    ([f s]
     (my-reductions f (first s) (rest s)))))

(defcheck solution-27f6e562
  (fn h
    ([g [f & r]] (h g f r))
    ([g i [f & r :as v]]
     (cons i
       (lazy-seq
         (if (seq v)
           (h g (g i f) r)))))))

(defcheck solution-27fc1454
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       (list init)
       (lazy-seq (cons init (my-reductions f (f init (first coll)) (rest coll))))))))

(defcheck solution-281c7f95
  (fn my-reducers
    ([f s] (my-reducers f (first s) (rest s)))
    ([f in s] (cons in (if (empty? s)
                         []
                         (lazy-seq (my-reducers f (f in (first s)) (rest s))))))))

(defcheck solution-285f2d7
  (fn rd
    ([f c] (rd f (f (first c) (first c)) (next c)))
    ([f r c]
     (if (nil? c)
       [r]
       (cons r (lazy-seq (rd f (f r (first c)) (next c))))))))

(defcheck solution-285f57d5
  ; (defn- make-accumulator ([function] { :function function, :steps [] }) ([function initial-value] { :function function, :steps [], :value initial-value }))

  ; (defn- update-accumulator [accumulator new-value] (if (contains? accumulator :value) (let [result-value ((:function accumulator) (:value accumulator) new-value)] { :function (:function accumulator), :value result-value, :steps (conj (:steps accumulator) result-value) }) { :function (:function accumulator), :value new-value, :steps (conj (:steps accumulator) new-value) }))

  ; (defn- reduct-seq [accumulator collection] (if (empty? collection) () (let [new-accumulator (update-accumulator accumulator (first collection))] (cons (:value new-accumulator) (lazy-seq (reduct-seq new-accumulator (rest collection)))))))

  (fn reduct
    ([function collection]
     (letfn [
             (make-accumulator ([function] {:function function, :steps []}) ([function initial-value] {:function function, :steps []}))
             (update-accumulator [accumulator new-value] (if (contains? accumulator :value) (let [result-value ((:function accumulator) (:value accumulator) new-value)] {:function (:function accumulator), :value result-value, :steps (conj (:steps accumulator) result-value)}) {:function (:function accumulator), :value new-value, :steps (conj (:steps accumulator) new-value)}))
             (reduct-seq [accumulator collection] (if (empty? collection) () (let [new-accumulator (update-accumulator accumulator (first collection))] (cons (:value new-accumulator) (lazy-seq (reduct-seq new-accumulator (rest collection)))))))]
       (reduct-seq (make-accumulator function) collection)))
    ([function initial-value collection]
     (letfn [
             (make-accumulator ([function] {:function function, :steps []}) ([function initial-value] {:function function, :steps []}))
             (update-accumulator [accumulator new-value] (if (contains? accumulator :value) (let [result-value ((:function accumulator) (:value accumulator) new-value)] {:function (:function accumulator), :value result-value, :steps (conj (:steps accumulator) result-value)}) {:function (:function accumulator), :value new-value, :steps (conj (:steps accumulator) new-value)}))
             (reduct-seq [accumulator collection] (if (empty? collection) () (let [new-accumulator (update-accumulator accumulator (first collection))] (cons (:value new-accumulator) (lazy-seq (reduct-seq new-accumulator (rest collection)))))))]
       (reduct-seq (make-accumulator function initial-value) (cons initial-value collection))))))

(defcheck solution-28827340
  (fn foo
    ([f start xs] (if-let [fs (first xs)]
                    (lazy-seq (cons start (foo f (f start fs) (rest xs))))
                    (list start)))
    ([f xs] (when-let [fs (first xs)]
              (foo f fs (rest xs))))))

(defcheck solution-2907b268
  (fn my-reductions
    ([f s]
     (if (empty? s)
       (f)
       (my-reductions f (first s) (rest s))))
    ([f current s]
     (lazy-seq
       (cons current
         (if (empty? s)
           '()
           (my-reductions f (f current (first s)) (rest s))))))))

(defcheck solution-2983590d
  (fn f
    ([g c] (lazy-seq (f g (first c) (next c))))
    ([g r c] (if (first c) (lazy-seq (cons r (f g (g r (first c)) (next c)))) (list r)))))

(defcheck solution-29cb79f5
  (fn r
    ([f [h & t]] (r f h t))
    ([f z s]
     (lazy-seq
       (cons
         z
         (if (seq s)
           (r f (f z (first s)) (rest s))))))))

(defcheck solution-29eed064
  (fn reduece
    ([f l]
     (lazy-seq
       (if-let [c (seq l)]
         (reduece f (first c) (rest c))
         (list (f)))))
    ([f i l]
     (cons i
       (lazy-seq
         (when-let [c (seq l)]
           (reduece f (f i (first c)) (rest c))))))))

(defcheck solution-2a3dce9c
  (fn reds
    ([f [x & xs]]
     (reds f x xs))
    ([f init [x & xs :as coll]]
     (if (coll? coll)
       (lazy-seq
         (cons init (reds f (f init x) xs)))
       [init]))))

(defcheck solution-2a41f32d
  (fn my-reductions
    ([f colls]
     (lazy-seq
       (if-let [s (seq colls)]
         (my-reductions f (first colls) (rest colls))
         (list (f)))))
    ([f init colls]
     (lazy-seq
       (cons init
         (when-let [s (seq colls)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-2a94215c
  (fn my-reduction
    ([f coll] (my-reduction f (first coll) (next coll)))
    ([f val coll]
     (if coll
       (lazy-seq (cons val (my-reduction f (f val (first coll)) (next coll))))
       [val]))))

(defcheck solution-2aa169b0
  (fn red
    ([f s lst] (red f (cons s lst)))
    ([f lst] (lazy-seq
               (map-indexed
                 (fn [i x] (reduce f (take (inc i) lst)))
                 lst)))))

(defcheck solution-2ac38206
  (fn reducts
    ([f [x & xs]]
     (reducts f x xs))
    ([f acc [x & xs]]
     (lazy-seq
       (cons acc (if x (reducts f (f acc x) xs)))))))

(defcheck solution-2b01fb93
  (fn
    ([op x & colls]
     (loop [result-colls (if (empty? colls) [] [x])
            index        0
            remain       (if (empty? colls) x (first colls))]
       (cond
         (empty? remain) result-colls
         (>= index 10) result-colls                         ;fixit
         (empty? result-colls)
         (recur
           (conj result-colls (first remain))
           (inc index)
           (drop 1 remain))
         :else (recur (conj result-colls (op
                                           (last result-colls)
                                           (first remain)))

                 (inc index)
                 (drop 1 remain)))))))

(defcheck solution-2b320b7e
  (fn rdctns
    ([f xs]
     (lazy-seq
       (when-let [s (seq xs)]
         (rdctns f (first s) (next s)))))
    ([f v xs]
     (cons v
       (lazy-seq
         (when-let [s (seq xs)]
           (rdctns f (f v (first s)) (rest s))))))))

(defcheck solution-2bb24cb9
  (fn r ([f [h & t]] (r f h t))
    ([f i l]
     (if l (lazy-seq
             (cons i (r f (f i (first l)) (next l)))) [i]))))

(defcheck solution-2ce7766b
  (fn f
    ([x y z]
     (map #(reduce x y (take %2 z))
       (cons y z)
       (range)))
    ([x [y & z]] (f x y z))
    ))

(defcheck solution-2d3a1dbf
  (fn reduc
    ([f coll]
     (lazy-seq
       (reduc f (f (first coll)) (next coll))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when coll
           (reduc f (f init (first coll)) (next coll))))))))

(defcheck solution-2d567518
  (fn R
    ([f s]
     (if (empty? s) [(f)]
                    (R f (first s) (rest s))))
    ([f a s]
     (if (empty? s) [a]
                    (cons a (lazy-seq (R f (f a (first s)) (rest s))))))))

(defcheck solution-2deff66f
  (fn alt-redox
    ([f coll] (alt-redox f (first coll) (rest coll)))
    ([f x coll]
     (lazy-seq
       (cons x
         (if (empty? coll)
           '()
           (alt-redox f (f x (first coll)) (rest coll))))))))

(defcheck solution-2e08515e
  (fn seq_reductions
    ([fun s]
     (seq_reductions fun (first s) (rest s)))
    ([fun init s]
     (lazy-cat
       [init]
       (when (not-empty s)
         (seq_reductions fun (fun init (first s)) (rest s)))))))

(defcheck solution-2e4230ed
  (fn my-reduce
    ([f [a & r]] (my-reduce f a r))
    ([f a r]
     (if (empty? r)
       (list a)
       (cons a (lazy-seq (my-reduce f (f a (first r)) (next r))))))))

(defcheck solution-2e6e7c1
  (fn red
    ([f xs] (red f (first xs) (rest xs)))
    ([f i xs]
     (cons i
       (if (seq xs)
         (lazy-seq
           (red f (f i (first xs)) (rest xs))))))))

(defcheck solution-2e8ab0e0
  (fn rx [f a & b]
    (if b (rx f (into [] (cons a (first b))))
          (letfn [(redux [f s n]
                    (let [rd (reduce f (take n s))]
                      (if (or (not (counted? s)) (<= n (count s)))
                        (cons rd (lazy-seq (redux f s (inc n)))))))]
            (redux f a 1)))))

(defcheck solution-2eaea395
  (fn r ([f v [x & res :as coll]] (if-not (seq coll)
                                    (list v)
                                    (let [result (f v x)]
                                      (lazy-cat (list v) (r f result res)))))
    ([f [x & res :as coll]] (if-not (seq coll)
                              (list (f))
                              (r f x res)))))

(defcheck solution-2ede6757
  (fn my-reductions
    ([f [init & coll]] (my-reductions f init coll))
    ([f acc [x & xs :as coll]] (lazy-seq (cons acc (if coll (my-reductions f (f acc x) xs)))))))

(defcheck solution-2f55b3de
  (fn my-reductions
    ([f se]
     (my-reductions f (first se) (rest se)))                ;; if no init specified, call the same function
    ;; but with the init parameter
    ([f first-arg se]
     (letfn [(reduct [f init remaining-se]
               (lazy-seq (when (not-empty remaining-se)
                           (let [res (f init (first remaining-se))]
                             (cons res (reduct f res (rest remaining-se)))))))] ;; lazy recipe
       ;; recursive call with the result being the new init
       (lazy-seq (cons first-arg (reduct f first-arg se)))))))

(defcheck solution-300bb695
  (fn my-rdct
    ([f col]
     (my-rdct f (first col) (rest col)))
    ([f i col]
     (if (empty? col)
       [i]
       (lazy-seq (cons i
                   (my-rdct f (f i (first col)) (rest col))))))))

(defcheck solution-306c36f4
  (fn r
    ([f [x & coll]] (r f x coll))
    ([f acc coll]
     (lazy-seq
       (if-let [[x & coll] (seq coll)]
         (cons acc
           (r f (f acc x) coll))
         [acc])))))

(defcheck solution-30a9e6a1
  (fn R ([f S] (let [[x & X] S] (if x (R f x X) (list (f))))) ([f i S] (cons i (lazy-seq (let [[x & X] S] (if x (R f (f i x) X))))))))

(defcheck solution-31a5057b
  (fn [f & args]
    (let
     [rdns (fn r [f init coll]
             (lazy-seq
               (cons init
                 (if (empty? coll)
                   nil
                   (r f (f init (first coll)) (rest coll))))))
      x    (first args)]
      (if (= 2 (count args))
        (rdns f (first args) (last args))
        (rdns f (first x) (rest x))))))

(defcheck solution-334ccf0d
  (fn [f x & y]
    (letfn [(red [f acc coll]
              (lazy-seq
                (cons acc
                  (when (seq coll)
                    (red f (f acc (first coll)) (rest coll))))))]
      (if (seq y)
        (red f x (first y))
        (red f (first x) (rest x))))))

(defcheck solution-3358a1eb
  (fn red
    ([func coll] (red func (first coll) (rest coll)))
    ([func prev coll]
     (lazy-seq
       (cons prev
         (when (seq coll)
           (red func (func prev (first coll)) (rest coll))))))))

(defcheck solution-34361583
  (fn r
    ([f c]
     (r f (first c) (rest c)))
    ([f x c]
     (if (empty? c)
       [x]
       (cons x (lazy-seq (r f
                           (f x (first c))
                           (rest c))))))))

(defcheck solution-34aa22dc
  (fn r
    ([f o l]
     (lazy-seq
       (if
        (= l ())
         (list o)
         (cons o (r f (f o (first l)) (rest l))))))
    ([f l]
     (r f (first l) (rest l)))))

(defcheck solution-34bb1381
  (fn !!
    ([op x xs]
     (cons x (when (seq xs) (lazy-seq (!! op (op x (first xs)) (rest xs))))))
    ([op xs]
     (!! op (first xs) (rest xs)))
    ))

(defcheck solution-35229bea
  (fn my-reductions
    ([f [fst & rst]]
     (my-reductions f fst rst))
    ([f val coll]
     (lazy-seq
       (cons val
         (when-let [s (seq coll)]
           (my-reductions f (f val (first s)) (rest s))))))))

(defcheck solution-35257988
  (fn reductions'
    ([op l] (reductions' op (first l) (rest l)))
    ([op acc l]
     (let [[[x] xs] (split-at 1 l)
           val (op acc x)]
       (if-not (empty? xs)
         (cons acc (lazy-seq (reductions' op val xs)))
         (list acc val))))))

(defcheck solution-357c22e7
  (fn reduction
    ([func xs] (reduction func (first xs) (rest xs)))
    ([func init xs]
     (lazy-seq
       (if (empty? xs)
         (list init)
         (let [inter-result (func init (first xs))]
           (cons init (reduction func inter-result (rest xs)))))))))

(defcheck solution-35b87e22
  (fn rreductions
    ([f xs]
     (rreductions
       f
       (first xs)
       (rest xs)
       )
     )

    ([f y xs]
     (if
      (empty? xs)
       [y]
       (lazy-seq
         (cons
           y
           (rreductions
             f
             (f
               y
               (first xs)
               )
             (rest xs)
             )
           )
         )
       )
     )
    ))

(defcheck solution-35faa383
  (fn r*
    ([f coll]
     (r* f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (cons init
         (when-let [s (seq coll)]
           (r* f (f init (first s)) (rest s))))))))

(defcheck solution-360371f8
  (fn redoo
    ([f acc c] (redoo f (cons acc c)))
    ([f [x & more :as all]]
     (let [redoo1 (fn redoo1
                    ([f [x & more] p]
                     (if (nil? x) (lazy-seq) (lazy-seq (cons (p x) (redoo1 f more (partial f (p x))))))))]
       (redoo1 f all identity)))))

(defcheck solution-36a399b2
  (fn redus
    ([f coll] (redus f (first coll) (rest coll)))
    ([f init coll]
     (cons init (lazy-seq
                  (when-let [s (seq coll)]
                    (redus f (f init (first s)) (rest s))))))))

(defcheck solution-37528087
  (fn [f & a]
    (let [x #(first %)
          y #(fnext %)
          z (nil? (y a))
          s (if z (x a) (cons (x a) (y a)))
          r (if z (range) (take (+ 1 (count (y a))) (range)))]
      (map (fn [c] (reduce #(f % %2) (x s) (take c (rest s)))) r))))

(defcheck solution-37590c33
  (fn [op & x]
    (
      let [reductions2
           (fn red2 [op reduction x]
             (lazy-seq (if (empty? x) [reduction]
                                      (let [newreduction (op reduction (first x))]
                                        (cons reduction
                                          (red2 op newreduction (rest x)))))))
           ]
      (if (= (count x) 2)
        (lazy-seq (reductions2 op (first x) (second x)))
        (lazy-seq (reductions2 op (first (first x)) (rest (first x))))))
    ))

(defcheck solution-383ad6fe
  (fn r
    ([f s] (r f (f (first s)) (rest s)))
    ([f v s]
     (if (empty? s) (list v)
                    (lazy-seq (cons v (r f (f v (first s)) (rest s))))))))

(defcheck solution-38594e89
  (fn r
    ([f [a b & c]]
     (if (= b nil) [a]
                   (cons a (lazy-seq (r f (cons (f a b) c))))))
    ([f i c] (r f (cons i c)))))

(defcheck solution-38bd9b85
  (fn step-reduce ([f coll] (step-reduce f (first coll) (rest coll))) ([f x coll] (if (seq coll) (let [next-coll (rest coll) next-x (f x (first coll))] (cons x (lazy-seq (step-reduce f next-x next-coll)))) (cons x (lazy-seq '()))))))

(defcheck solution-38e7c5e2
  (fn reds ([f coll] (lazy-seq (if-let [s (seq coll)] (reds f (first s) (next s))))) ([f init coll] (cons init (lazy-seq (when-let [s (seq coll)] (reds f (f init (first s)) (next s))))))))

(defcheck solution-38fc83ef
  (fn my-reductions
    ([f init coll]
     (if (empty? coll)
       (list init)
       (lazy-seq (let [c0 (first coll), coll' (rest coll), v (f init c0)]
                   (cons init (my-reductions f v coll'))))))
    ([f coll] (my-reductions f (first coll) (rest coll)))))

(defcheck solution-38fd099a
  (fn rec
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (rec f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (rec f (f init (first s)) (rest s))))))))

(defcheck solution-391ca56c
  (fn reduction1
    ([f col]
     (reduction1 f (f (first col)) (rest col)))
    ([f val col]
     (if (empty? col)
       (list val)
       (cons val (lazy-seq (reduction1 f (f val (first col)) (rest col))))))))

(defcheck solution-39aa4352
  (fn r ([f c]
         (r f (first c) (rest c)))
    ([f i c]
     (cons i
       (lazy-seq
         (when-let [s (seq c)]
           (r f (f i (first s)) (rest s))))))))

(defcheck solution-39af7ed6
  (fn kk
    ([fun ar]
     (letfn [(f2 [fun2 ar2]
               (let [arg (seq ar2) f (first arg) r (next arg) t (next r)]
                 (when r
                   (lazy-seq (cons f (f2 fun2 (cons (fun2 f (first r)) t)))))))]
       (let [ars (seq (concat ar (list 0)))]
         (f2 fun ars))))
    ([fun a1 ar]
     (letfn [(f2 [fun2 ar2]
               (let [arg (seq ar2) f (first arg) r (next arg) t (next r)]
                 (when r
                   (lazy-seq (cons f (f2 fun2 (cons (fun2 f (first r)) t)))))))]
       (let [ars (seq (concat (cons a1 ar) (list 0)))]
         (f2 fun ars))))))

(defcheck solution-3a2a45f3
  (fn [& args] (letfn
                [(wrapper
                   ([f init xs] (cons init (my-reductions f init xs)))
                   ([f xs] (wrapper f (first xs) (rest xs))))
                 (my-reductions
                   [f x xs]
                   (lazy-seq
                     (when-let [s (seq xs)]
                       (let [[y & ys] xs
                             z (f x y)]
                         (cons z (my-reductions f z ys))))))]
                 (apply wrapper args))))

(defcheck solution-3a47f307
  (fn red ([f coll] (red f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq
       (if (empty? coll)
         [v]
         (cons v (red f (f v (first coll)) (rest coll))))))))

(defcheck solution-3a640d73
  (fn __
    ([f c]
     (__ f (first c) (rest c)))
    ([f v c]
     (lazy-seq
       (when-let [s (seq c)]
         (if (empty? (rest s))
           (cons v [(f v (first s))])
           (cons v (__ f (f v (first s)) (rest s)))))))))

(defcheck solution-3adebb1f
  (fn reds
    ([f init coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons init (reds f (f init (first s)) (next s)))
         [init])))
    ([f coll]
     (if-let [s (seq coll)]
       (reds f (first s) (next s))))))

(defcheck solution-3ae37361
  (fn red
    ([f xs] (red f (first xs) (rest xs)))
    ([f x xs]
     (lazy-seq
       (if (seq xs)
         (cons x (red f (f x (first xs)) (rest xs)))
         (list x))))))

(defcheck solution-3afc157c
  (fn cust-reductions
    ([f coll] (cust-reductions f (first coll) (next coll)))
    ([f init coll]
     (if (seq coll)
       (cons init
         (lazy-seq
           (cust-reductions
             f
             (f init (first coll))
             (next coll))))
       (cons init (lazy-seq '()))))))

(defcheck solution-3bc3cfe9
  (fn rd
    ([f coll] (lazy-seq
                (if-let [xs (seq coll)]
                  (rd f (first xs) (rest xs))
                  )))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [xs (seq coll)]
           (rd f (f init (first xs)) (rest xs))
           )))
     )))

(defcheck solution-3cb28bab
  (fn reduc#
    ([func a-seq]
     (reduc# func (first a-seq) a-seq))
    ([func seed a-seq]
     (if (seq? a-seq)
       (lazy-seq
         (cons (func seed (first a-seq))
           (reduc# func (func seed (first a-seq)) (rest a-seq))))
       (loop [lseq a-seq curval seed acc (vector seed)]
         (if (empty? lseq)
           acc
           (let [nextval (func curval (first lseq))]
             (recur (rest lseq) nextval (conj acc nextval)))))))))

(defcheck solution-3d0b2d57
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (red f (f init (first s)) (rest s))))))))

(defcheck solution-3d26e391
  (fn redu
    ([f s] (redu f (first s) (rest s)))
    ([f r s] (if-let [i (first s)]
               (cons
                 r
                 (lazy-seq (redu f (f r i) (rest s))))
               (list r)
               ))))

(defcheck solution-3d31a6fb
  (fn foo
    ([op s]
     (foo op (first s) (rest s)))
    ([op acc s]
     (if (identity (first s))
       (cons acc (lazy-seq (foo op (op acc (first s)) (rest s))))
       [acc]))))

(defcheck solution-3d72b00d
  (fn reds
    ([f [x & xs]]
     (reds f x xs))
    ([f red coll]
     (lazy-seq
       (cons red
         (when-let [s (seq coll)]
           (reds f (f red (first s)) (rest s))))))))

(defcheck solution-3d9de7f1
  (fn scan
    ([f c]
     (scan f (first c) (rest c)))
    ([f init c]
     (if (empty? c)
       [init]
       (cons
         init
         (lazy-seq (scan f (f init (first c)) (rest c))))))))

(defcheck solution-3da17657
  (let [reduce-seq
        (fn this-f [func l-val vals]
          (if (not (empty? vals))
            (let [this-res (func l-val (first vals))]
              (cons this-res
                (lazy-seq (this-f func this-res (rest vals)))))))]
    (fn this-func
      ([func vals]
       (if (not (empty? vals))
         (this-func func (func (first vals)) (rest vals))))
      ([func init-val vals]
       (if (not (empty? vals))
         (cons init-val
           (lazy-seq (reduce-seq func init-val vals)))))
      )))

(defcheck solution-3e06d704
  (fn my-reduce

    ([op input] (my-reduce op (first input) (rest input)))

    ([op result input]

     (lazy-seq
       (if (empty? input) (list result)
                          (cons result
                            (my-reduce op
                              (op result (first input))
                              (rest input))))))))

(defcheck solution-3e0d16a0
  (fn reductions*
    ([f [x & xs]]
     (reductions* f x xs))
    ([f init [x & xs]]
     (if-not x
       [init]
       (lazy-cat [init] (reductions* f (f init x) xs))))))

(defcheck solution-3e3613cb
  (fn rec
    ([f xs] (rec f (first xs) (rest xs)))
    ([f i xs]
     (if-let [xs (seq xs)]
       (lazy-seq (cons i (rec f (f i (first xs)) (rest xs))))
       (list i)))))

(defcheck solution-3e49db38
  (fn my-reductions
    ([f [x & xs]] (my-reductions f x xs))
    ([f v [x & xs]] (lazy-seq (cons v (if x (my-reductions f (f v x) xs)))))))

(defcheck solution-3f3a44aa
  (fn n60
    ([f coll]
     (lazy-seq (if-let [s (seq coll)] (n60 f (first s) (rest s)) (f))))
    ([f init coll]
     (lazy-seq (cons init (lazy-seq (when-let [s (seq coll)] (n60 f (f init (first s)) (rest s)))))))))

(defcheck solution-3f4beb44
  (fn r
    ([f [v & c]] (r f v c))
    ([f v [h & c]]
     (if h (lazy-seq (cons v (r f (f v h) c))) [v]))))

(defcheck solution-3f67c61b
  (fn g
    ([f [x & l]] (g f x l))
    ([f x l]
     (if l
       (let [[y & r] l z (f x y)]
         (cons x (lazy-seq (g f z r))))
       [x]))))

(defcheck solution-3fb72f39
  (fn X
    ([f [a & t]]
     (X f a t))
    ([f s c]
     (lazy-seq (if (empty? c) (cons s nil)
                              (cons s (X f (f s (first c)) (rest c))))))))

(defcheck solution-4064cf1d
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f b coll]
     (cons b
       (lazy-seq
         (when (seq coll)
           (my-reductions f
             (f b (first coll))
             (rest coll))))))))

(defcheck solution-407890e7
  (fn my-reds ([fun coll]
               (my-reds fun (first coll) (rest coll)))
    ([fun init coll]
     (if (seq coll)
       (cons init (lazy-seq (my-reds fun (fun init (first coll)) (rest coll))))
       (cons init nil)))))

(defcheck solution-40c3c7cc
  (fn r ([f i v] (conj (let [x (atom i)] (for [y v] (swap! x #(f % y)))) i))
    ([f v] (r f (first v) (rest v)))))

(defcheck solution-4155b78a
  (fn l
    ([f [x & m]] (l f x m))
    ([f v [x & m]] (if (empty? m) (list v (f v x)) (lazy-seq (cons v (l f (f v x) m)))))))

(defcheck solution-417dffd8
  (fn mreductions
    ([f coll] (mreductions f (first coll) (rest coll)))
    ([f val coll]
     (cons val
       (lazy-seq
         (when-let [s (seq coll)]
           (mreductions f (f val (first s)) (rest s))))))))

(defcheck solution-41b8157d
  (fn red
    ([f ls]
     (if (empty? ls)
       ls
       (red f (first ls) (rest ls))))
    ([f init ls]
     (if (empty? ls)
       (list init)
       (cons init (lazy-seq (red f (f init (first ls)) (rest ls))))
       ))))

(defcheck solution-41c91a5a
  (fn seq-reduce
    ([f seqs]
     (seq-reduce f (first seqs) (rest seqs)))
    ([f init seqs]
     (lazy-seq
       (if (empty? seqs)
         [init]
         (cons init
           (seq-reduce f
             (f init (first seqs))
             (rest seqs))))))))

(defcheck solution-41dc1dc7
  (fn reduce-seq
    ([fun items] (reduce-seq fun (first items) (rest items)))
    ([fun initial items]
     (letfn [(reduce-int
               [fun initial items]
               (if (empty? items)
                 ()
                 (let [current (fun initial (first items))]
                   (cons current (lazy-seq (reduce-int fun current (rest items)))))))]
       (cons initial (reduce-int fun initial items))))))

(defcheck solution-4259d5ef
  (fn red
    ([f s]
     (red f (first s) (rest s))
     )
    ([f i s]
     (if (empty? s)
       (list i)
       (cons
         i
         (lazy-seq
           (red f (f i (first s)) (rest s))
           )
         )
       )
     )
    ))

(defcheck solution-425e35fc
  (fn mreduction
    ([f i cs]
     (let [r (atom (or i))
           e (for [c cs]
               (if @r
                 (reset! r (f @r c))
                 (reset! r c)))]
       (if i
         (cons i e)
         e)))
    ([f cs]
     (mreduction f nil cs))))

(defcheck solution-42efee87
  (fn r
    ([f [init & args]]
     (r f init args))
    ([f init args]
     (cons init (lazy-seq
                  (when args
                    (r f (f init (first args)) (next args))))))))

(defcheck solution-43cec8b8
  (fn !
    ([f coll] (lazy-seq
                (! f (first coll) (rest coll))))
    ([f init coll] (cons init
                     (lazy-seq
                       (when-let [s (seq coll)]
                         (! f (f init (first s)) (rest s))))))))

(defcheck solution-43d5855b
  (fn reductions2
    ([f init [x & xs]]
     (cons init (lazy-seq (when x (reductions2 f (f init x) xs)))))
    ([f coll]
     (reductions2 f (first coll) (rest coll)))))

(defcheck solution-43e0de2b
  (fn r
    ([f xs] (r f (first xs) (rest xs)))
    ([f v xs]
     (lazy-seq
       (cons v
         (if (empty? xs) [] (r f (f v (first xs)) (rest xs))))))))

(defcheck solution-442552f6
  (letfn [

          (iter [f interm lst]
            (if (empty? lst) '()
                             (let [interm (f interm (first lst))]
                               (cons interm (lazy-seq (iter f interm (rest lst)))))))]

    (fn
      ([f lst] (cons (first lst) (iter f (first lst) (rest lst))))
      ([f interm lst] (cons interm (iter f interm lst))))))

(defcheck solution-4440829c
  (letfn
   [(reducts [f acc coll]
      (cons
        acc
        (when-not (empty? coll)
          (lazy-seq
            (reducts f (f acc (first coll)) (rest coll))))))]
    (fn
      ([f coll] (reducts f (first coll) (rest coll)))
      ([f start coll] (reducts f start coll)))))

(defcheck solution-444746d
  (fn red
    ([f coll]
     (red f (first coll) (rest coll)))
    ([f ini coll]
     (lazy-seq
       (if (seq coll)
         (let [r (f ini (first coll))]
           (cons ini (red f r (rest coll))))
         [ini])))))

(defcheck solution-4455bd21
  (fn reduc
    ([f s] (reduc f (first s) (rest s)))
    ([f fv s]
     (cons fv (lazy-seq (if (seq s)
                          (reduc f (f fv (first s)) (rest s))
                          nil))))))

(defcheck solution-4469d21c
  (fn r
    ([f acc xs] (cons acc
                  (lazy-seq
                    (if (seq xs)
                      (r f (f acc (first xs)) (rest xs))))))
    ([f xs] (r f (first xs) (rest xs)))))

(defcheck solution-447fe4f7
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f x y]
     (if (empty? y) [x]
                    (cons x (lazy-seq (r f (f x (first y)) (rest y))))))))

(defcheck solution-448980f2
  (letfn [(c ([f s] (let [a (first s)] (c f a (rest s))))
            ([f a s]
             (letfn [(r [a s]
                       (if (seq s) (lazy-seq
                                     (let [a (f a (first s))]
                                       (cons a (r a (rest s)))))))]
               (cons a (r a s)))))]
    c))

(defcheck solution-44fa559f
  (fn go
    ([f q [x & xs]]
     (cons q
       (when x
         (lazy-seq (go f (f q x) xs)))))
    ([f [x & xs]]
     (go f x xs))))

(defcheck solution-45079580
  (fn lazy-reduce
    ([fun [init & sq]] (lazy-reduce fun init sq 1))
    ([fun init sq] (lazy-reduce fun init sq 1))
    ([fun init sq cnt]
     (let [sqq   (cons init sq)
           elems (take cnt sqq)]
       (if (= cnt (count elems))
         (lazy-seq (cons (reduce fun elems) (lazy-reduce fun init sq (inc cnt))))
         [])))))

(defcheck solution-45132f87
  (fn solve

    ([f arg] (solve f (first arg) (rest arg)))

    ([f z arg] (lazy-seq
                 (if (empty? arg) [z]
                                  (let [head (f z (first arg))]
                                    (cons z (solve f head (rest arg)))

                                    ))))))

(defcheck solution-4514e33a
  (fn reduc ([f x] (reduc f (first x) (rest x)))
    ([f x y] (if (empty? y)
               (vector x)
               (lazy-seq (cons x (reduc f (f x (first y)) (rest y))))))))

(defcheck solution-455c2378
  (fn g
    ([f [x & xs]]
     (g f x xs))
    ([f i [x & xs]]
     (cons i
       (if x
         (lazy-seq (g f (f i x) xs)))))))

(defcheck solution-45713cb2
  (fn f
    ([g v] (f g (first v) (rest v)))
    ([g i v] (if (empty? v) (vector i) (cons i (lazy-seq (f g (g i (first v)) (rest v))))))
    ))

(defcheck solution-457a86ba
  (fn self
    ([f xs]
     (self f (first xs) (rest xs)))
    ([f res xs]
     (cons res
       (lazy-seq (if (empty? xs) nil
                                 (self f (f res (first xs)) (rest xs))))))))

(defcheck solution-458a3a9b
  (fn sequence-reduction
    ([f coll]
     (sequence-reduction f (first coll) (rest coll)))
    ([f start coll]
     (let [fst    (first coll)
           result (f start fst)
           rst    (rest coll)]
       (cons start (lazy-seq (if (empty? rst)
                               [result]
                               (sequence-reduction f result rst))))))))

(defcheck solution-45a91644
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f val coll]
     (cons val (lazy-seq
                 (if (empty? coll)
                   []
                   (my-reductions f (f val (first coll)) (rest coll))))))))

(defcheck solution-45d697f5
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq
       (cons v (when-not (empty? coll)
                 (my-reductions f (f v (first coll)) (rest coll))))))))

(defcheck solution-462dad63
  (fn r
    ([f coll]
     (lazy-seq
       (r f (first coll) (rest coll))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-465d52b6
  (fn reductions'
    ([f coll] (reductions' f (first coll) (rest coll)))
    ([f init coll]
     (letfn [(reduce-step [f v1 [v2 & more]]
               (lazy-seq
                 (if (nil? v2)
                   nil
                   (let [cur-res (f v1 v2)]
                     (cons cur-res (reduce-step f cur-res more))))))]
       (lazy-seq
         (cons init (reduce-step f init coll)))))))

(defcheck solution-46fd5ea8
  (fn my-reduct
    ([func coll]
     (my-reduct func (first coll) (rest coll)))

    ([func firstArg coll]
     (letfn [(reduct [f init se]
               (lazy-seq (when-not (empty? se)
                           (let [res (f init (first se))]
                             (cons res (reduct f res (rest se)))))))]
       (lazy-seq (cons firstArg (reduct func firstArg coll)))))))

(defcheck solution-47c4f625
  (fn my-reduction
    ([f coll] (my-reduction f (first coll) (rest coll)))
    ([f init coll]
     (letfn [(get-sub-seqs [coll]
               (map-indexed (fn [idx _] (take (inc idx) coll)) coll))]
       (cons init (map #(reduce f init %) (get-sub-seqs coll)))))))

(defcheck solution-485e36fc
  (fn _
    ([f xs] (_ f (first xs) (rest xs)))
    ([f x0 xs]
     (lazy-seq
       (if (empty? xs)
         [x0]
         (cons x0
           (_ f (f x0 (first xs)) (rest xs))))))))

(defcheck solution-487c3c6f
  (fn reduce-list [f & args]
    (let [[accum coll] (if (= 2 (count args))
                         args [(first (first args)) (rest (first args))])]
      (if (empty? coll)
        [accum]
        (cons accum (lazy-seq (reduce-list
                                f
                                (f accum (first coll))
                                (rest coll))))
        )
      )
    ))

(defcheck solution-48851fbe
  (fn myred
    ([op dat]
     (myred op (first dat) (rest dat)))
    ([op val dat]
     (if-let [s (seq dat)]
       (cons val (lazy-seq (myred op (op val (first s)) (rest s))))
       (cons val [])))))

(defcheck solution-48c43ef9
  (fn rdctions
    ([f xs] (rdctions f (first xs) (next xs)))
    ([f init xs] (if xs (lazy-seq (cons init (rdctions f (f init (first xs)) (next xs)))) (vector init)))))

(defcheck solution-491330a6
  apply (fn [f i & s]
          (#(% %) (memoize #(cons i (lazy-seq (map f (% %) s)))))))

(defcheck solution-49441ee6
  (fn [f & args]
    (let [start (if (= 2 (count args)) (first args) (ffirst args))
          s     (if (= 2 (count args)) (last args) (rest (last args)))]
      (letfn [(op [s l]
                (if (empty? s)
                  s
                  (cons
                    (f l (first s))
                    (lazy-seq (op (rest s) (f l (first s)))))))]
        (cons start (op s start))))))

(defcheck solution-497124d1
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-4a0d3da
  (fn f ([g c]
         (let [fc (first c)]
           (if (empty? (drop 2 c))
             (cons fc [(g fc (last c))])
             (cons fc (lazy-seq (f g (cons (g fc (second c)) (drop 2 c))))))))
    ([g x0 c] (f g (cons x0 c)))))

(defcheck solution-4a2b0eec
  (fn t
    ([f [x & xs]]
     (t f x xs))
    ([f p [x & xs]]
     (if x
       (lazy-seq (cons p (t f (f p x) xs)))
       [p]))))

(defcheck solution-4a533a1d
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (if (empty? coll)
         nil
         (lazy-seq (my-reductions f (apply f [init (first coll)]) (rest coll))))))))

(defcheck solution-4a8923a3
  (fn my-reductions
    ([op [h & r]]
     (my-reductions op h r))
    ([op i [h & r]]
     (cons i
       (if h (lazy-seq (my-reductions op (op i h) r)))))))

(defcheck solution-4a8af822
  (fn my-reductions
    ([fun coll] (my-reductions fun (first coll) (rest coll)))
    ([fun init coll] (cons init (lazy-seq (when (seq coll) (my-reductions fun (fun init (first coll)) (rest coll))))))))

(defcheck solution-4b0f0166
  (fn redu
    ([ff coll] (redu ff (first coll) (rest coll)))
    ([ff init coll]
     (let [rf (fn rf- [coll lastv]
                (lazy-seq
                  (if (empty? coll)
                    (list lastv)
                    (cons lastv (rf- (rest coll) (ff lastv (first coll)))))))]
       (rf coll init)))
    ))

(defcheck solution-4b7f0cb7
  (fn myred
    ([f cols]
     (myred f (first cols) (rest cols))
     )
    ([f val cols]
     (
      (fn seq [f preview col]
        (lazy-seq
          (cons
            preview
            (when (not (empty? col))
              (seq f
                (f preview (first col))
                (rest col))
              )
            )
          )

        ) f val cols)
     )
    ))

(defcheck solution-4bbce695
  (fn steps
    ([f s]
     (if (nil? (second s)) s
                           (lazy-seq (cons (first s) (steps f (cons (f (first s) (second s)) (drop 2 s)))))))
    ([f r s]
     (steps f (cons r s)))))

(defcheck solution-4c8757ce
  (fn redu
    ([f s] (redu f (first s) (rest s)))
    ([f init s]
     (lazy-seq
       (if s
         (let [r (f init (first s))]
           (cons init (redu f r (next s)))
           )
         (cons init '())
         ))
     )
    ))

(defcheck solution-4c90685f
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f acc coll] (if (empty? coll)
                    [acc]
                    (cons acc (lazy-seq (my-reductions
                                          f
                                          (f acc (first coll))
                                          (rest coll))))))))

(defcheck solution-4d567407
  (fn myreduce
    ([f coll]
     (lazy-seq
       (if-let [c (seq coll)]
         (myreduce f (first c) (rest c))
         (list (f)))))
    ([f ini coll]
     (cons ini
       (lazy-seq
         (when-let [c (seq coll)]
           (myreduce f (f ini (first c)) (rest c))))))))

(defcheck solution-4dc995d8
  (fn solve
    ([f coll] (solve f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if (empty? coll)
         (list init)
         (cons init (solve f
                      (f init (first coll))
                      (rest coll))))))))

(defcheck solution-4df5d343
  (fn my-reductions
    ([f xs]
     (my-reductions f (first xs) (rest xs)))
    ([f init [x & xs' :as xs]]
     (if (nil? x)
       (list init)
       (let [x' (f init x)]
         (cons init (lazy-seq (my-reductions f x' xs'))))))))

(defcheck solution-4e1e79eb
  (fn r ([f [h & t]] (r f h t)) ([f v s] (cons v (if-let [[h & t] s] (lazy-seq (r f (f v h) t)))))))

(defcheck solution-4e47c9c7
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (cons
         init
         (if (seq coll)
           (my-reductions f (f init (first coll)) (rest coll))
           nil))))))

(defcheck solution-4e6a68c4
  (fn r
    ([f [head & tail]]
     (r f head tail))
    ([f acc [head & tail]]
     (cons acc
       (when-not (nil? head)
         (lazy-seq
           (r f (f acc head) tail)))))))

(defcheck solution-4e7f2c39
  (fn redu
    ([function col] (redu function (first col) (rest col)))
    ([function start col]
     (if (empty? col)
       (list start)
       (cons start (lazy-seq (redu function (function start (first col)) (rest col))))
       )
     )
    ))

(defcheck solution-4e998f30
  (fn reducts
    ([f coll] (let [[init & coll] coll] (reducts f init coll 0)))
    ([f init coll] (reducts f init coll 0))
    ([f init coll n]
     (let [x (take n coll)]
       (if (< (count x) n) nil
                           (lazy-seq (cons (reduce f init x)
                                       (reducts f init coll (inc n)))))))))

(defcheck solution-4ec08d53
  (fn my-reductions

    ([f acc coll]
     (if (empty? coll)
       [acc]
       (let [[head & tail] coll
             reduction (f acc head)]
         (cons acc (lazy-seq (my-reductions f reduction tail))))))

    ([f [initial & coll]]
     (my-reductions f initial coll))))

(defcheck solution-4f5b5f8e
  (fn f
    ([g [x & s]] (f g x s))
    ([g a [x & s]] (lazy-seq (cons a (if x (f g (g a x) s)))))))

(defcheck solution-4f69d42b
  (fn r
    ([f c] (r f (first c) (rest c)))
    ([f v c]
     (lazy-seq
       (cons v
         (when (seq c)
           (r f (f v (first c)) (rest c))))))))

(defcheck solution-4f9e9294
  (fn red
    ([f [h & xs]] (red f h xs))
    ([f i s]
     (lazy-seq
       (if (seq s)
         (cons i (red f (f i (first s)) (rest s)))
         (list i))))))

(defcheck solution-4fa722ad
  (fn reds
    ([f v s]
     (if (empty? s)
       (list v)
       (lazy-seq (cons v (reds f (f v (first s)) (rest s))))))
    ([f s]
     (if (empty? s)
       (f)
       (reds f (first s) (rest s))))))

(defcheck solution-4fcb9e32
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f initial coll]
     (lazy-seq
       (if-let [elem (first coll)]
         (let [reduction (f initial elem)]
           (cons initial (my-reductions f reduction (rest coll))))
         (cons initial nil))))))

(defcheck solution-4feaf75b
  (fn my-reduct
    ([fnn init args]
     (if (empty? args)
       [init]
       (lazy-cat [init]
         (my-reduct fnn (fnn init (first args)) (rest args)))))
    ([fnn args]
     (my-reduct fnn (first args) (rest args)))))

(defcheck solution-4ff0ba1c
  (fn meu-reduce
    ([f coll]
     (lazy-seq (cons (first coll) ((fn meu-reduce-it [f subtotal coll]
                                     (when-not (empty? coll)
                                       (let [subt (f subtotal (first coll))]
                                         (lazy-seq (cons subt (meu-reduce-it f subt (next coll)))))))
                                   f
                                   (first coll)
                                   (next coll)))))
    ([f inicial coll]
     (meu-reduce f (cons inicial coll)))))

(defcheck solution-5011eb4a
  (fn REDUCTIONS
    ([f coll]
     (REDUCTIONS f (first coll) (rest coll)))
    ([f init coll]
     (cons init (lazy-seq
                  (when (seq coll)
                    (REDUCTIONS f (f init (first coll)) (rest coll))))))))

(defcheck solution-50269e06
  (fn my-reduce
    ([func cur [x & xs]]
     (if x
       (lazy-seq (cons cur (my-reduce func (func cur x) xs)))
       [cur]))
    ([func args]
     (let [[arg1 & args] args]
       (my-reduce func arg1 args)))))

(defcheck solution-502ed76c
  (fn pr60-4
    ([f coll] (pr60-4 f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       (cons init [])
       (let [next-init (f init (first coll))]
         (cons init
           (lazy-seq
             (pr60-4 f next-init (rest coll)))))))))

(defcheck solution-50316e7b
  (fn red
    ([f [x & xs]] (red f x xs))
    ([f val coll]
     (lazy-seq
       (cons val
         (if (seq coll)
           (red f (f val (first coll)) (rest coll))
           []))))))

(defcheck solution-50991cb2
  (fn reductions-prime
    ([f coll]
     (if-let [s (seq coll)]
       (reductions-prime f (first s) (next s))
       (f)))
    ([f val coll]
     (if-let [x (first coll)]
       (lazy-seq
         (cons val (reductions-prime f (f val x) (rest coll))))
       [val]))))

(defcheck solution-510c6c2e
  (fn r
    ([f v]
     (r f (first v) (rest v)))
    ([f e v]
     (if (empty? v) [e]
                    (cons e
                      (lazy-seq
                        (r f (f e (first v)) (rest v))))))))

(defcheck solution-51179fd3
  (fn reduction
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reduction f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reduction f (f init (first s)) (rest s))))))))

(defcheck solution-51491c16
  (fn rd
    ([f coll]
     (rd f (first coll) (rest coll)))
    ([f init coll]
     (letfn [(it [f init coll]
               (lazy-seq
                 (when-let [fcoll (first coll)]
                   (let [n (f init fcoll)]
                     (cons n (it f n (rest coll)))))))]
       (cons init (it f init coll))))))

(defcheck solution-517cf2c2
  (fn reductions2
    ([f [init & s]] (reductions2 f init s))
    ([f acc [head & s]] (cons acc (if head (lazy-seq (reductions2 f (f acc head) s)))))))

(defcheck solution-51a9b6ff
  (fn calc
    ([x y]
     (
      (fn calc2-1 [x y t]
        (lazy-seq
          (cons
            (reduce x t)
            (when-not (empty? y) (calc2-1 x (rest y) (conj t (first y)))))
          ))
      x (rest y) [(first y)]
      )
     )
    ([x y z]
     (
      (fn calc3-1 [x y z t]
        (lazy-seq
          (cons
            (reduce x y t)
            (when-not (empty? z) (calc3-1 x y (rest z) (conj t (first z)))))
          ))
      x y z []
      )
     )
    ))

(defcheck solution-5242be1a
  (fn reducs
    ([f xs] (reducs f (first xs) (next xs)))
    ([f s xs] (if (empty? xs) [s]
                              (cons s (lazy-seq (reducs f (f s (first xs)) (next xs))))))))

(defcheck solution-52cea48f
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f i s]
     (cons i (lazy-seq
               (when-let [s (seq s)]
                 (r f (f i (first s)) (rest s))))))))

(defcheck solution-5368d228
  (fn r
    ([f c] (drop 1 (r f (f) c)))
    ([f acc c] (if (empty? c)
                 (list acc)
                 (cons acc (lazy-seq (r f (f acc (first c)) (rest c))))))))

(defcheck solution-538cde43
  (fn reds
    ([f s] (lazy-seq (reds f (first s) (rest s))))
    ([f val s]
     (if (empty? s) [val]
                    (lazy-seq (cons val (reds f (f val (first s)) (rest s))))))))

(defcheck solution-541cabd
  (fn lazy-reductions
    ([f coll] (lazy-reductions f (first coll) (rest coll)))
    ([f initial coll]
     (if (seq coll)
       (lazy-seq (cons initial
                   (lazy-reductions f
                     (f initial (first coll))
                     (rest coll)
                     )
                   )
         )
       [initial])
     )
    ))

(defcheck solution-54c88a70
  (fn r
    ([f coll]
     (lazy-seq
       (if-let [c (seq coll)]
         (r f (first c) (rest c)))))
    ([f init coll]
     (lazy-seq
       (cons init
         (when-let [c (seq coll)]
           (r f (f init (first c)) (rest c))))))))

(defcheck solution-54ef4f3e
  (fn reductions' [& arguments]
    (let [collection       (last arguments)
          reduce-arguments (vec (drop-last arguments))]
      (letfn [(reducer [batch queue]
                (let [reduction (apply reduce
                                  (conj reduce-arguments batch))]
                  (lazy-seq
                    (if (empty? queue)
                      [reduction]
                      (cons
                        reduction
                        (reducer (conj batch (first queue))
                          (rest queue)))))))]
        (if (= 2 (count reduce-arguments))
          (reducer [] collection)
          (reducer [(first collection)] (rest collection)))))))

(defcheck solution-55524ddf
  (fn r
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (r f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-557122a8
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f val coll]
     (if-let [first (first coll)]
       (cons val (lazy-seq (my-reductions f
                             (f val first)
                             (rest coll))))
       (list val)))))

(defcheck solution-55d94b1b
  (fn rdns
    ([f a s] (if (empty? s)
               (list a)
               (cons a (lazy-seq (rdns f (f a (first s)) (rest s))))))
    ([f s] (rdns f (first s) (rest s)))))

(defcheck solution-55fa8255
  #(letfn [(my-reductions
             ([f coll]
              (my-reductions f (first coll) (rest coll)))
             ([f val coll]
              (take-while (fn [x] (not (nil? x)))
                (map first
                  (iterate
                    (fn [[v c]] (if (nil? c) nil [(f v (first c)) (next c)]))
                    [val coll])))))]
     (apply my-reductions %&)))

(defcheck solution-56068d6
  (fn reducemy
    ([f coll]
     (if-let [s (seq coll)]
       (reducemy f (first s) (next s))
       (f)))
    ([f val coll]
     (lazy-seq
       (if (seq coll)
         (cons val (reducemy f (f val (first coll)) (next coll)))
         (cons val '()))))))

(defcheck solution-5668aa7f
  (fn mr
    ([f coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [h (first s)
               n (second s)
               t (rest s)]
           (cons h (mr f (cons (f h n) (rest t))))))))
    ([f seed coll]
     (cons seed
       (lazy-seq
         (when-let [s (seq coll)]
           (mr f (f seed (first s)) (rest coll))))))))

(defcheck solution-56830ba3
  (fn reds
    ([f start sq] (if (empty? sq) (list start) (cons start (lazy-seq (reds f (f start (first sq)) (rest sq))))))
    ([f sq] (reds f (first sq) (rest sq)))))

(defcheck solution-5691789c
  (fn reds
    ([f coll] (when-let [s (seq coll)] (reds f (first s) (rest s))))
    ([f init coll]
     (cons init (lazy-seq (when-let [s (seq coll)] (reds f (f init (first s)) (rest s))))))))

(defcheck solution-569bbc2d
  (fn myreductions
    ([f start col]
     (lazy-seq
       (if (seq col)
         (cons start (myreductions f (f start (first col)) (rest col)))
         [start])))
    ([f col] (myreductions f (first col) (rest col)))))

(defcheck solution-56fbe674
  (fn redc
    ([f s]
     (redc f (first s) (rest s)))
    ([f init s]
     (if (empty? s)
       (list init)
       (cons init (lazy-seq (redc f (f init (first s)) (rest s))))))))

(defcheck solution-56ff0442
  (fn f ([op x] (f op (first x) (rest x)))
    ([op y x]
     (lazy-seq
       (if (empty? x) (list y)
                      (cons y (f op (op y (first x)) (rest x))))))))

(defcheck solution-570c6a02
  (fn reds
    ([f val coll]
     (if (empty? coll)
       [val]
       (lazy-seq
         (cons val (reds f (f val (first coll)) (rest coll))))))
    ([f coll] (next (reds f (first coll) coll)))))

(defcheck solution-5795209a
  (fn sr
    ([f xs] (sr f (f (first xs)) (rest xs)))
    ([f r xs]
     (if (empty? xs)
       (lazy-seq (cons r nil))
       (lazy-seq (cons r (sr f (f r (first xs)) (rest xs))))))))

(defcheck solution-57a3a189
  (fn reduction
    ([f i d]
     (if (seq d)
       (cons i (lazy-seq (reduction f
                           (f i (first d))
                           (rest d))))
       [i]))
    ([f d]
     (reduction f (first d) (rest d)))))

(defcheck solution-57d6530a
  (fn rs
    ([f coll]
     (map #(reduce f %)
       (map #(take (inc %2) coll) coll (range))))
    ([f init coll]
     (rs f (cons init coll)))))

(defcheck solution-57fa3d78
  (fn g ([f b] (g f (first b) (rest b)))
    ([f a b] (if (empty? b) [a]
                            (lazy-seq (cons a (g f (f a (first b)) (rest b))))))))

(defcheck solution-5817cfcb
  (fn r2
    ([f c] (lazy-seq (r2 f (first c) (rest c))))
    ([f a c]
     (cons a
       (lazy-seq
         (when-let [s (seq c)]
           (r2 f (f a (first s)) (rest s))))))))

(defcheck solution-5837bbb3
  (fn seq-reductions
    ([f coll]
     (seq-reductions f (first coll) (rest coll)))

    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (seq-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-583e6f8f
  (fn r
    ([f sq]
     (r f (first sq) (rest sq)))
    ([f a sq]
     (lazy-seq
       (cons a
         (when (not (empty? sq))
           (r f (f a (first sq)) (rest sq))))))))

(defcheck solution-586be438
  (fn q
    ([f [a & r]] (q f a r))
    ([f a [b & r]]
     (if b (cons a (lazy-seq (q f (f a b) r))) [a]))))

(defcheck solution-587b5dc8
  (fn my-reduction
    ([f coll]
     (letfn [(step [f coll]
               (if (nil? (second coll))
                 []
                 (lazy-seq
                   (let [mid-result (f (first coll) (second coll))]
                     (cons
                       mid-result
                       (step f (cons mid-result (drop 2 coll))))))))]
       (cons (first coll) (step f coll))))
    ([f val coll]
     (my-reduction f (cons val coll)))))

(defcheck solution-5892c61b
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f v s]
     (if (empty? s)
       (cons v nil)
       (let [new-head (f v (first s))]
         (lazy-seq
           (cons v
             (r f new-head (rest s)))))))))

(defcheck solution-58afa20c
  (fn reduc
    ([op t] (reduc op (first t) (rest t)))
    ([op s t]
     (cons s
       (if (seq t)
         (lazy-seq (reduc op (op s (first t)) (rest t))))))))

(defcheck solution-58e247a0
  (fn number60
    ([f xs] (number60 f (first xs) (rest xs)))
    ([f p xs]
     (if (empty? xs)
       [p]
       (lazy-cat [p] (number60 f (f p (first xs)) (rest xs)))))))

(defcheck solution-594372c0
  (fn __
    ([f initial coll]
     (let [current (atom initial)
           do-it   (fn [elem]
                     (swap! current #(f % elem)))]
       (lazy-cat
         [initial]
         (map do-it coll))))
    ([f coll]
     (__ f (first coll) (rest coll)))))

(defcheck solution-5946eec1
  (fn rd
    ([fc i [f & rs]]
     (cons i
       (lazy-seq
         (when f
           (rd fc (fc i f) rs)))))
    ([fc c]
     (rd fc (first c) (rest c)))))

(defcheck solution-59658a33
  (fn reduce12
    ([f p c] (cons p (when-let [cls (seq c)] (lazy-seq (reduce12 f (f p (first cls)) (rest cls))))))
    ([f c] (reduce12 f (first c) (rest c)))
    ))

(defcheck solution-59881bf4
  (fn folds
    ([f coll]
     (folds f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if (empty? coll)
         (list init)
         (cons init (folds f (f init (first coll)) (rest coll))))))))

(defcheck solution-59bd6dca
  (fn seq-red
    ([f coll] (seq-red f (first coll) (next coll)))
    ([f val coll] (if (seq coll) (lazy-seq (cons val
                                             (seq-red f (f val (first coll))
                                               (next coll))))
                                 [val]))))

(defcheck solution-5a4234e9
  (fn rdc
    ([f c]
     (rdc f (first c) (rest c)))
    ([f v c]
     (lazy-seq (cons v
                 (when (seq c) (rdc f (f v (first c)) (rest c))))))))

(defcheck solution-5a60b3b2
  (fn ([func1 first-arg1 coll1]
       (cons first-arg1 ((fn ! [func first-arg coll]
                           (lazy-seq (if (empty? coll)
                                       []
                                       (cons (func first-arg (first coll)) (! func (func first-arg (first coll)) (rest coll)))
                                       ))) func1 first-arg1 coll1))
       )
    ([func1 coll1]
     (cons (first coll1) ((fn ! [func first-arg coll]
                            (lazy-seq (if (empty? coll)
                                        []
                                        (cons (func first-arg (first coll)) (! func (func first-arg (first coll)) (rest coll)))
                                        ))) func1 (first coll1) (rest coll1)))
     )
    ))

(defcheck solution-5b077dc4
  (fn seq-red
    ([f s] (seq-red f (first s) (rest s)))
    ([f start s] (if (empty? s) [start]
                                (lazy-seq (cons start (seq-red f (f start (first s)) (rest s))))
                                ))
    ))

(defcheck solution-5b1052dd
  (let [fr (fn r ([f xs] (r f (f) xs))
             ([f init xs] (if-not (empty? xs) (let [e (f init (first xs))] (lazy-cat [e] (r f e (rest xs)))) [])))]

    (fn reds
      ([f xs] (fr f (f) xs))
      ([f init xs] (lazy-cat [init] (fr f init xs))))

    ))

(defcheck solution-5c373240
  (fn reduxns
    ([o i s]
     (if (empty? s) [i]
                    (cons i (lazy-seq (reduxns o (o i (first s)) (rest s))))))
    ([o s]
     (if (empty? s) [(o)]
                    (reduxns o (first s) (rest s))))))

(defcheck solution-5c3fffde
  (fn my-reduction
    ([f xs]
     (if (empty? xs)
       (lazy-seq (cons (f) nil))
       (my-reduction f (first xs) (rest xs))))
    ([f x0 xs]
     (lazy-seq
       (cons x0
         (if (empty? xs) nil
                         (my-reduction f (f x0 (first xs)) (rest xs))))))))

(defcheck solution-5d0bdd98
  (fn reds
    ([f coll]
     (lazy-seq (reds f (first coll) (rest coll))))
    ([f init coll]
     (cons init
       (lazy-seq (when (not-empty coll)
                   (reds f (f init (first coll)) (rest coll))))))))

(defcheck solution-5d31d46f
  (fn my-reductions
    ([f [v1 v2 & vs]] (my-reductions f v1 (cons v2 vs)))
    ([f v1 [v2 & vs]] (lazy-cat [v1] (my-reductions f v1 v2 vs)))
    ([f v1 v2 vs]
     (let [acc (f v1 v2)]
       (if (empty? vs) [acc]
                       (lazy-cat [acc] (my-reductions f acc (first vs) (rest vs))))))))

(defcheck solution-5d93fff0
  (fn r
    ([o a] (r o (first a) (rest a)))
    ([o p a] (if (empty? a) [p] (cons p (lazy-seq (r o (o p (first a)) (rest a))))))
    ))

(defcheck solution-5dd3d038
  (fn rr
    ([f i s] (lazy-seq (cons i (if (not (empty? s))
                                 (rr f (f i (first s)) (next s))))))
    ([f s] (rr f (first s) (next s)))))

(defcheck solution-5e2ee8d3
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       [init]
       (cons init
         (lazy-seq
           (red f (f init (first coll)) (rest coll))))))))

(defcheck solution-5e875e2d
  (fn reductions*
    ([f coll] (if (empty? coll)
                (vector (f))
                (reductions* f (first coll) (rest coll))))
    ([f init coll] (if (empty? coll)
                     (vector init)
                     (cons init (lazy-seq (reductions*
                                            f (f init (first coll))
                                            (rest coll))))))))

(defcheck solution-5e8b2132
  (fn red
    ([f [v & s]]
     (red f v s))
    ([f v s]
     (lazy-seq (cons v (if s (red f (f v (first s)) (next s))))))))

(defcheck solution-5ed6d0b4
  (fn iter
    ([f [a & b]] (iter f a b))
    ([f a [b & c]]
     (cons a (if (nil? b)
               nil
               (lazy-seq (iter f (f a b) c)))))))

(defcheck solution-5f266c5
  (fn red
    ([f s]
     (map-indexed
       (fn [i _] (reduce f (first s) (take (inc i) s)))
       s))
    ([f v s]
     (map-indexed
       (fn [i _] (reduce f v (take i s)))
       (concat [v] s)))))

(defcheck solution-5f5185cf
  (fn [f & more]
    ((fn ! [f result coll]
       (if (empty? coll)
         nil
         (cons
           result
           (lazy-seq (!
                       f
                       (f result (first coll))
                       (rest coll)
                       )
             )
           ))
       ) f
     (if (= 2 (count more))
       (first more)
       (first (first more)))
     (if (= 2 (count more))
       (conj (second more) 0)
       (rest (first more))
       )
     )))

(defcheck solution-5fd51668
  (fn reduce2
    ([f initial coll]
     (lazy-seq
       (if (not (empty? coll))
         (let [r (f initial (first coll))]
           (cons initial
             (reduce2 f r (rest coll)))
           )
         (cons initial '())
         )
       ))
    ([f coll] (reduce2 f (first coll) (rest coll)))
    ))

(defcheck solution-60048832
  (letfn [(red ([f xs] (red f (first xs) (rest xs)))
            ([f init xs]
             (if (empty? xs)
               (cons init '())
               (let [res (f init (first xs))]
                 (cons init (lazy-seq (red f res (rest xs))))))))]
    (fn [& args]
      (apply red args))))

(defcheck solution-60772e3
  (fn r
    ([f [a & b]] (r f a b))
    ([f a b]
     (let [m (atom a)]
       (cons a (map #(swap! m f %) b))))))

(defcheck solution-60a17a4e
  (fn my-reductions
    ([f init v]
     (if (empty? v)
       [init]
       (let [[x & xs] v]
         (if (empty? xs)
           [init (f init x)]
           (concat [init] (lazy-seq (my-reductions f (f init x) xs)))))))
    ([f v]
     (if (empty? v)
       (f)
       (let [[x & xs] v]
         (lazy-seq (my-reductions f x xs)))))))

(defcheck solution-60e84926
  (fn rds
    ([f i c]
     (cons i
       (lazy-seq
         (when-first [c1 c]
           (rds f (f i c1) (rest c))))))
    ([f [i & c]] (rds f i c))))

(defcheck solution-6107a989
  (fn folds
    ([f [x & xs]] (folds f x xs))
    ([f acc [x & xs]]
     (lazy-seq
       (cons acc (if x (folds f (f acc x) xs)))))))

(defcheck solution-61e6c58a
  (fn seq-reductions
    ([f val s]
     (if (empty? s) [val]
                    (let [next-elt (first s)
                          rem-elts (rest s)
                          next-val (f val next-elt)]
                      (lazy-seq (cons val (seq-reductions f next-val rem-elts)))))
     )
    ([f s]
     (if (empty? s) []
                    (let [next-elt (first s)
                          rem-elts (rest s)]
                      (seq-reductions f next-elt rem-elts)))

     )))

(defcheck solution-6239fe6f
  (fn o
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (o f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (o f (f init (first s)) (rest s))))))))

(defcheck solution-6247dabb
  (fn reds [f & args] (case (count args)
                        1 (reds f (first (first args)) (rest (first args)))
                        2 (lazy-seq (cons (first args)
                                      (if (seq (second args))
                                        (reds f
                                          (f (first args)
                                            (first (second args)))
                                          (rest (second args)))))))))

(defcheck solution-62655d8e
  (fn myreduce
    ([f ls]
     (lazy-seq (cons (first ls) (myreduce f (cons (f (first ls) (second ls)) (drop 2 ls))))))
    ([f i ls]
     (if (= ls [])
       (vector i)
       (lazy-seq (cons i (myreduce f (f i (first ls)) (rest ls))))))))

(defcheck solution-62c3c655
  (fn myReduce
    ([f x] (myReduce f (first x) (rest x)))
    ([f v x] (if (empty? x)
               (list v)
               (lazy-seq
                 (cons v (myReduce f (f v (first x)) (rest x))))))))

(defcheck solution-62d1f6f5
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-62fe7e96
  (fn rs
    ([f col]
     (rs f (first col) (rest col)))
    ([f init col]
     (if-let [c (seq col)]
       (cons init (lazy-seq (rs f (f init (first c)) (rest c))))
       (list init)))))

(defcheck solution-63345bf9
  (fn reductions_
    ([f s]
     (if (empty? s) nil
                    (reductions_ f (first s) (rest s))))
    ([f x s]
     (if (empty? s) [x]
                    (lazy-seq
                      (cons x (reductions_ f (f x (first s)) (rest s))))))))

(defcheck solution-6364e350
  (fn myred
    ([f init seq] (if (empty? seq) (list init) (lazy-seq (cons init (myred f (f init (first seq)) (rest seq))))))
    ([f seq] (myred f (first seq) (rest seq)))))

(defcheck solution-6365b811
  (fn r
    ([f xs] (r f (first xs) (rest xs)))
    ([f v xs]
     (lazy-seq
       (cons v
         (if (empty? xs) [] (r f (f v (first xs)) (rest xs))))))))

(defcheck solution-63a1e67e
  (fn r
    ([f i s] (r f (list* i s)))
    ([f s] (map #(reduce f %)
             ((fn b [x [h & t]]
                (lazy-seq
                  (cons (conj x h)
                    (when (seq t) (b (conj x h) t)))))
              [] s)))
    ))

(defcheck solution-640aedf2
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       (list init)
       (lazy-seq
         (cons init
           (my-reductions f
             (f init (first coll))
             (rest coll))))))))

(defcheck solution-646be4d9
  (fn my-reductions
    ([f [x & xs]]
     (my-reductions f x xs))
    ([f x [y & ys :as xs]]
     (if (empty? xs)
       [x]
       (lazy-seq (concat [x] (my-reductions f (f x y) ys)))))))

(defcheck solution-6494ffaf
  (fn my-reductions
    ([f col] (my-reductions f (first col) (rest col)))
    ([f init col]
     (cons
       init
       (if (empty? col)
         nil
         (lazy-seq (my-reductions f (f init (first col)) (rest col))))))
    ))

(defcheck solution-64acfffa
  (fn reds
    ([f coll] (reds f (first coll) (rest coll)))
    ([f x [firstc & more]]
     (if firstc
       (lazy-seq
         (let [res (f x firstc)]
           (cons x (reds f res more))))
       [x]))))

(defcheck solution-64c9f2a8
  (fn r
    ([f [x & xs]] (r f x xs))
    ([f i [x & xs]] (cons i (if x (lazy-seq (r f (f i x) xs)))))))

(defcheck solution-64dd6e16
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if (empty? coll)
         (list init)
         (cons init
           (r f (f init (first coll))
             (rest coll))))))))

(defcheck solution-652b5d4d
  (fn red
    ([f s l]
     (if (empty? l)
       [s]
       (cons s
         (lazy-seq
           (red
             f
             (f s (first l))
             (rest l))))))
    ([f l]
     (red f (first l) (rest l)))
    ))

(defcheck solution-656d2437
  (fn reductions'
    ([f s] (reductions' f (first s) (rest s)))
    ([f i s]
     (lazy-seq
       (if-let [x (first s)]
         (cons i (reductions' f (f i x) (rest s)))
         (list i))))))

(defcheck solution-662da183
  (fn rdcs
    ([f c]
     (if (empty? c)
       ()
       (rdcs f (first c) (rest c))))
    ([f v c]
     (if (empty? c)
       (seq [v])
       (cons v (lazy-seq (rdcs f (f v (first c)) (rest c))))))))

(defcheck solution-667f5e81
  (fn f
    ([o c] (f o (first c) (rest c)))
    ([o i c] (if (empty? c) [i]
                            (lazy-seq (cons i (f o (o i (first c)) (rest c))))))))

(defcheck solution-66a8abab
  (fn reduc
    ([f init seqq]
     (lazy-seq (cons init
                 (when seqq
                   (reduc f (f init (first seqq)) (next seqq))))
       )
     )
    ([f seqq] (reduc f (first seqq) (next seqq)))
    ))

(defcheck solution-6829afce
  (fn ffn
    ([ff coll] (ffn ff (ff (first coll)) (rest coll)))
    ([ff init coll]
     (if (seq coll)
       (lazy-seq (cons init (ffn ff (ff init (first coll)) (rest coll))))
       (list init)))))

(defcheck solution-68909eb9
  (fn m
    ([f [c & r]] (m f c r))
    ([f i [c & r]] (cons i (lazy-seq
                             (if c (m f (f i c) r)))))))

(defcheck solution-68a4730e
  (fn [f & args]
    (let [val  (if (= 2 (count args)) (first args) (first (first args)))
          coll (if (= 2 (count args)) (second args) (rest (first args)))]
      (letfn [(lazy-sr [alpha c]
                (if (not (empty? c))
                  (let [r (f alpha (first c))]
                    (cons r (lazy-seq (lazy-sr r (rest c))))))
                )]
        (cons val (lazy-sr val coll))))))

(defcheck solution-694e593a
  (fn r ([f coll] (r f (first coll) (rest coll)))
    ([f start coll] (if (empty? coll)
                      [start]
                      (lazy-seq (cons start (r f (f start (first coll)) (rest coll))))))))

(defcheck solution-6956fab1
  (fn r [f in & col]
    (let [i (if (empty? col)
              (first in)
              in)
          c (if (empty? col)
              (rest in)
              (first col))]
      (if (empty? c)
        [i]
        (cons i (lazy-seq (r f (f i (first c)) (rest c))))))))

(defcheck solution-6961939c
  (fn r
    ([f i [x & xs]]
     (lazy-seq
       (cons i (if x (r f (f i x) xs) '()))))
    ([f [x & xs]]
     (r f x xs))))

(defcheck solution-698b4025
  (fn reduce-
    ([f coll]
     (reduce- f (first coll) (next coll)))
    ([f init [h & t :as coll]]
     (cons init
       (lazy-seq
         (if (seq coll)
           (reduce- f (f init h) t)))))))

(defcheck solution-6a09aeab
  (fn rs
    ([f l] (rs f (first l) (rest l)))
    ([f x ys]
     (if (empty? ys)
       [x]
       (lazy-seq (cons x (rs f (f x (first ys)) (rest ys))))))))

(defcheck solution-6a2314fb
  (fn my-reductions
    ([f init coll]
     (if (empty? coll)
       (list init)
       (cons init (lazy-seq (my-reductions f (f init (first coll)) (rest coll))))))
    ([f coll]
     (my-reductions f (first coll) (rest coll)))))

(defcheck solution-6a42e9c
  (fn myreductions ([f coll] (lazy-seq (if-let [s coll] (myreductions f (first s) (rest s)) (list (f))))) ([f init coll] (cons init (lazy-seq (when-let [s (seq coll)] (myreductions f (f init (first s)) (rest s))))))))

(defcheck solution-6a5557d2
  (fn __
    ([f a]
     (letfn [(helper [f a c]
               (cons (reduce f (take c a))
                 (lazy-seq (helper f a (inc c)))))]
       (helper f a 1)))
    ([f a b]
     (letfn [(helper [f a b c]
               (when (<= c (count b))
                 (cons (reduce f a (take c b))
                   (lazy-seq (helper f a b (inc c))))))]
       (helper f a b 0)))))

(defcheck solution-6a5ab5f3
  (fn redu
    ([op ini coll] ((fn sr [op ini coll] (lazy-seq (
                                                     if (empty? coll)
                                                     ini
                                                     (concat ini (sr op [(op (last ini) (first coll))] (rest coll)))
                                                     ))) op [ini] coll)
     )
    ([op coll] (redu op (first coll) (rest coll)))

    ))

(defcheck solution-6a7fda95
  (fn rdctns
    ([op se]
     (rdctns
       op
       (first se)
       (rest se)))
    ([op a more]
     (lazy-seq
       (cons
         a
         (rdctns
           op
           a
           (first more)
           (rest more)))))
    ([op a b more]
     (if
      (not b)
       ()
       (lazy-seq
         (cons
           (op a b)
           (rdctns
             op
             (op a b)
             (first more)
             (rest more))))))))

(defcheck solution-6a939d9d
  (fn seduce
    ([f c]
     (let [s (seq c)]
       (seduce f (first s) (rest s))))
    ([f v c]
     (cons v
       (lazy-seq
         (when-let [s (seq c)]
           (seduce f (f v (first s)) (rest s))))))))

(defcheck solution-6a9af16f
  (fn __
    ([func val coll]
     (if (empty? coll)
       [val]
       (cons val
         (lazy-seq (__ func (func val (first coll)) (rest coll))))))
    ([func coll]
     (let [f (first coll)
           r (rest coll)]
       (cond
         (empty? coll) [(func)]
         (empty? r) [f]
         :else (cons f
                 (lazy-seq (__ func (cons (func f (first r))
                                      (rest r))))))))
    ))

(defcheck solution-6ac86a8c
  (fn my-reduce
    ([f coll] (my-reduce f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       [init]
       (lazy-seq (concat [init] (my-reduce f (f init (first coll)) (rest coll))))))))

(defcheck solution-6b457323
  (fn scan ([f xs] (scan f (f) (rest xs)))
    ([f x xs] (cons x (lazy-seq (when (seq xs) (scan f (f x (first xs)) (rest xs))))))))

(defcheck solution-6b882929
  (fn __
    ([f coll]
     (lazy-seq (if-let [s (seq coll)]
                 (__ f (first s) (rest s))
                 (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq (when-let [s (seq coll)]
                   (__ f (f init (first s)) (rest s))
                   ))))))

(defcheck solution-6ba55d86
  (fn seq-red
    ([op s] (seq-red op (first s) (rest s)))
    ([op a s]
     (if (seq s)
       (lazy-seq
         (cons a
           (seq-red op (op a (first s)) (next s))))
       (list a)))))

(defcheck solution-6bd301d8
  (fn g
    ([f [a & b]] (g f a b))
    ([f v c]
     (map first
       (take-while
         #(not (nil? %))
         (iterate
           (fn [[v [x & r :as c]]]
             (if (empty? c)
               nil
               [(f v x) r]))
           [v c]))))))

(defcheck solution-6c4342ec
  (fn r
    ([f init coll]
     (cons init
       (lazy-seq
         (when (seq coll)
           (r f (f init (first coll)) (next coll))))))
    ([f coll] (r f (first coll) (next coll)))))

(defcheck solution-6c7b6acb
  (fn my-reductions
    ([op coll] (my-reductions op (first coll) (rest coll)))
    ([op acc coll]
     (if (nil? (second coll))
       (cons acc (vector (op acc (first coll))))
       (cons acc (lazy-seq (my-reductions op (op acc (first coll)) (rest coll))))))))

(defcheck solution-6c841e20
  (fn reds ([f [h & t]] (reds f h t))
    ([f i s]
     (if (empty? s) (list i)
                    (lazy-seq (cons i (reds f (f i (first s)) (rest s))))))))

(defcheck solution-6d185c2c
  (fn rr ([f s] (rr f (first s) (rest s)))
    ([f v s] (if (empty? s) [v] (cons v (lazy-seq (rr f (f v (first s)) (rest s))))))))

(defcheck solution-6d407bf3
  (fn r
    ([f coll] (r f (first coll) (rest coll)))
    ([f init coll]
     (cons init (when-not (empty? coll) (lazy-seq (r f (f init (first coll)) (rest coll))))))))

(defcheck solution-6db6fcc7
  (fn newreductions
    ([f init coll]
     (if (seq coll)
       (cons init (lazy-seq (newreductions f (f init (first coll)) (rest coll))))
       (vector init)))
    ([f coll]
     (if (seq coll)
       (lazy-seq (newreductions f (first coll) (rest coll)))
       '()))))

(defcheck solution-6e07cf76
  (fn reduc
    ([f s] (reduc f (first s) (rest s)))
    ([f v s]
     (if (empty? s)
       (list v)
       (cons v (lazy-seq (reduc f (f v (first s)) (rest s))))))))

(defcheck solution-6e4c30a0
  (fn r ([f coll]
         (lazy-seq
           (if-let [s (seq coll)]
             (r f (first s) (rest s))
             (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-6e50af3f
  (fn reduct
    ([f [init & coll]]
     (reduct f init coll))
    ([f init [arg0 & args]]
     (lazy-seq (cons init
                 (when arg0
                   (reduct f (f init arg0) args)))))))

(defcheck solution-6eb62076
  (fn rr
    ([f s] (rr f (first s) (rest s)))
    ([f init s]
     (lazy-seq (cons init (when (not (empty? s))
                            (rr f (f init (first s)) (rest s))))))))

(defcheck solution-6ebdf2eb
  (fn rf
    ([f coll] (rf f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if (empty? coll)
         (list init)
         (cons init
           (rf f (f init (first coll)) (rest coll))))))))

(defcheck solution-6ec519e3
  (fn foo ([f src] (foo f nil src))
    ([f init src] [f init src]
     (if (empty? src) (list init)
                      (if (nil? init)
                        (if (empty? (rest src))
                          (list (first src))
                          (cons (first src) (lazy-seq (foo f (f (first src) (second src)) (drop 2 src))))
                          )
                        (cons init (lazy-seq (foo f (f init (first src)) (rest src))))
                        )
                      )
     )))

(defcheck solution-6ff0ee46
  (fn reductions'
    ([f xs] (reductions' f (first xs) (rest xs)))
    ([f init xs]
     (if (nil? xs)
       init
       (cons init
         (lazy-seq
           (when-let [s (seq xs)]
             (reductions' f (f init (first s)) (rest s)))))))))

(defcheck solution-6ff5fa3c
  (fn red
    ([f [x & ls]] (red f x ls))
    ([f x [y & ls]]
     (if (nil? y)
       (lazy-seq (cons x nil))
       (lazy-seq (cons x (red f (f x y) ls)))))))

(defcheck solution-6ffc2bc4
  (fn redn
    ([f x xs] (lazy-seq (cons x
                          (if (empty? xs) xs
                                          (redn f (f x (first xs)) (rest xs))))))

    ([f xs] (redn f (first xs) (rest xs)))))

(defcheck solution-7022090
  (fn f
    ([g c]
     (f g (first c) (next c)))
    ([g a c]
     (lazy-seq
       (if-let [c (seq c)]
         (let [b (g a (first c))]
           (cons a (f g b (next c))))
         [a])))))

(defcheck solution-706bcb1e
  (fn d
    ([f a] (d f 0 (next a)))
    ([f a b]
     (if (empty? b)
       [a]
       (cons a (lazy-seq (d f (f a (first b)) (rest b))))
       ))))

(defcheck solution-7072e5e6
  (fn

    ([f in c]
     ((fn r
        [f in c i]
        (lazy-seq
          (when (<= i (count c))
            (cons (reduce f in (take i c)) (r f in c (inc i)))))) f in c 0)
     )

    ([f c]
     ((fn m [f c i]
        (lazy-seq
          (cons (reduce f (take i c)) (m f c (inc i))))) f c 1)
     )

    ))

(defcheck solution-71050f5
  (fn reduc
    ([f coll]
     (reduc f (first coll) (rest coll)))
    ([f s coll]
     (if-let [add (first coll)]
       (cons s (lazy-seq (reduc f (f s add) (rest coll))))
       (list s)))))

(defcheck solution-710c7d43
  (fn my-reductions
    ([fold coll]
     (my-reductions fold (fold (first coll)) (rest coll)))
    ([fold prev coll]
     (cons prev
       (lazy-seq
         (when-let [sq (seq coll)]
           (let [new (fold prev (first sq))]
             (my-reductions fold new (rest sq)))))))))

(defcheck solution-711102a2
  (fn r
    ([f [init & xs]]
     (r f init xs))
    ([f memo [x & xs]]
     (let [memo' (f memo x)]
       (if (seq xs)
         (cons memo (lazy-seq (r f memo' xs)))
         (list memo memo'))))))

(defcheck solution-711c48e5
  (fn [f val-or-coll & [coll]]
    (let [val (if (nil? coll) (first val-or-coll) val-or-coll)
          xs  (if (nil? coll) (rest val-or-coll) coll)]
      ((fn impl [acc lst]
         (if (empty? lst) [acc]
                          (cons acc (lazy-seq (impl (f acc (first lst)) (rest lst)))))) val xs))))

(defcheck solution-7133977c
  (fn [f l & r] (letfn [(i [lv l]
                          (if (empty? l) [lv]
                                         (cons lv (lazy-seq (i (f lv (first l)) (rest l))))))]
                  (if (empty? r) (i (first l) (rest l))
                                 (i l (first r))))))

(defcheck solution-719bce8d
  (fn _reductions
    ([fun [a & r]] (_reductions fun a r))
    ([fun init coll]
     (lazy-seq
       (cons init
         (if (first coll)
           (_reductions fun
             (fun init (first coll))
             (next coll))
           ()))))))

(defcheck solution-71b459ae
  (fn foo
    ([fun acc items]
     (if (empty? items)
       [acc]
       (cons acc (lazy-seq (foo fun (fun acc (first items)) (rest items))))))
    ([fun items]
     (when (not (empty? items))
       (foo fun (first items) (rest items))))
    ))

(defcheck solution-725def4a
  (fn scanr
    ([f z xs]
     (if (empty? xs)
       (cons z '())
       (lazy-seq
         (cons z (scanr f (f z (first xs)) (rest xs))))))
    ([f xs]
     (when (seq xs)
       (scanr f (first xs) (rest xs))))))

(defcheck solution-7275d316
  (fn reduc ([f a coll]
             (lazy-seq (if (seq coll)
                         (let [acc (f a (first coll))] (cons a (reduc f acc (rest coll))))
                         [a])))
    ([f coll]
     (reduc f (first coll) (rest coll)))))

(defcheck solution-72a89c6b
  (fn red
    ([f init [x & xs :as coll]]
     (if (empty? coll) [init]
                       (lazy-cat [init] (red f (f init x) xs))))
    ([f coll] (red f (first coll) (rest coll)))))

(defcheck solution-72e015a3
  (fn red
    ([f x list]
     (if (not (empty? list))
       (lazy-seq (cons x (red f (apply f [x (first list)]) (rest list))))
       [x]
       )
     )
    ([f list]
     (apply red [f (first list) (rest list)])
     )
    ))

(defcheck solution-72eb6114
  (fn f ([op seq]
         (f op (first seq) (rest seq)))
    ([op val seq]
     (if (empty? seq)
       [val]
       (lazy-seq (cons val (f op (op val (first seq)) (rest seq))))))))

(defcheck solution-7305bafa
  (fn r
    ([f [a & b]] (r f a b))
    ([f a b] (cons a (lazy-seq (if (nil? b)
                                 []
                                 (r f (f a (first b)) (next b))))))))

(defcheck solution-73098914
  ; based on cgrand's amazingly terse solution
  ; cgrand's is better because it is faster and uses less memory.
  ; This is beacuse his use of the y-combinator allows him to recurse with a memoized version of the function.
  apply (fn r [f i & s]
          (cons i (lazy-seq (map f (apply r f i s) s)))))

(defcheck solution-731e2987
  (fn rds
    ([f s] (rds f (first s) (rest s)))
    ([f acc s]
     (lazy-seq
       (if (empty? s)
         (cons acc ())
         (cons acc (rds f (f acc (first s)) (rest s))))))))

(defcheck solution-732ac80b
  (fn mreductions
    ([func start vec] (mreductions func (cons start vec)))
    ([func [v_first v_sec & v_rest]]
     (cons v_first (lazy-seq (when v_sec (mreductions func (cons (func v_first v_sec) v_rest))))))))

(defcheck solution-732de5d9
  (fn red
    ([f a] (red f (first a) (rest a)))
    ([f i a]
     (cons i
       (lazy-seq
         (when (seq a)
           (red f (f i (first a)) (rest a))))))))

(defcheck solution-7357e12b
  (fn sr ([f x] (sr f (first x) (rest x)))
    ([f x y] (if (empty? y) [x] (cons x (lazy-seq (sr f (f x (first y)) (rest y))))))))

(defcheck solution-738bd1d1
  ;; reductions
  (fn rds
    ;; cons first item out of collection, use as value.
    ([f c] (cons (first c) (rds f (first c) (rest c) nil)))
    ;; cons value, pass to main body expr
    ([f v c] (cons v (rds f v c nil)))
    ;; let i be initial where any value indicates we cons'd an initial value.
    ([f v c i]
     (lazy-seq
       (when (seq c)
         (let [r (apply reduce [f v [(first c)]])]
           (cons r (rds f r (rest c) nil))))))))

(defcheck solution-73c9492
  (fn rd ([f coll]
          (lazy-seq
            (if-let [s (seq coll)]
              (rd f (first s) (rest s))
              (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (rd f (f init (first s)) (rest s))))))))

(defcheck solution-74177dbf
  (fn myred
    ([f s]
     (myred f (first s) (rest s)))
    ([f i s]
     (lazy-seq
       (cons i (if (first s)
                 (myred f (f i (first s)) (rest s))))))))

(defcheck solution-7434da66
  (fn seq-reductions
    ([f xs] (seq-reductions f (first xs) (rest xs)))
    ([f acc xs]
     (cons acc (lazy-seq
                 (if (empty? xs)
                   ()
                   (seq-reductions f (f acc (first xs))
                     (rest xs)))
                 )
       )
     )
    ))

(defcheck solution-7477dba6
  (fn rdctns
    ([f [h & t]] (rdctns f h t))
    ([f acc [h & t]] (if h
                       (cons acc (lazy-seq (rdctns f (f acc h) t)))
                       [acc]))
    ))

(defcheck solution-747d57eb
  (letfn [(next-reduction [f val coll]
            (if (empty? coll)
              (cons val [])
              (lazy-seq
                (cons val
                  (next-reduction f
                    (f val (first coll))
                    (rest coll))))))
          (my-reductions
            ([f coll]
             (my-reductions f (first coll) (rest coll)))
            ([f init coll]
             (next-reduction f init coll)))]
    my-reductions))

(defcheck solution-74b79e34
  (fn sequence-reductions
    ([f coll]
     (letfn [(seq-reduce
               [f coll]
               (let [new-ele   (f (first coll) (second coll))
                     nth2-rest (nthrest coll 2)]
                 (if (empty? nth2-rest)
                   [new-ele]
                   (cons new-ele (lazy-seq (seq-reduce f
                                             (cons new-ele (nthrest coll 2))))))))]
       (cons (first coll) (seq-reduce f coll))))
    ([f init coll]
     (sequence-reductions f (cons init coll)))))

(defcheck solution-7595eef3
  (fn red
    ([f col] (red f (first col) (next col)))
    ([f init col]
     (cons init (when-let [s (seq col)]
                  (lazy-seq (red f (f init (first col)) (next col))))))))

(defcheck solution-75eaabf9
  (fn
    ([f coll] (->> (iterate #(vector (conj (first %) (f (last (first %)) (first (last %))))
                               (rest (last %))) [[(first coll)] (rest coll)])
                (map first)
                (map last)))
    ([f x coll] (->> [[x] coll]
                  (iterate #(vector (conj (first %) (f (last (first %)) (first (last %))))
                              (vec (rest (last %)))))
                  (map first)
                  (map last)
                  (take 4)))))

(defcheck solution-75ecb971
  (fn reducs ([f a xs]
              (if (empty? xs)
                (lazy-seq [a])
                (let [[y & ys] xs a2 (f a y)]
                  (lazy-seq (cons a (reducs f a2 ys))))))
    ([f xs] (reducs f (first xs) (rest xs)))))

(defcheck solution-7646f3d1
  (fn my-reductions
    ([f allvals] (my-reductions f (first allvals) (rest allvals)))
    ([f startval allvals]
     (if (not (empty? allvals))
       (lazy-seq
         (cons startval (my-reductions f (f startval (first allvals)) (rest allvals))))
       (list startval)))))

(defcheck solution-769d1a1a
  (fn reductions-
    ([f [x & xs]] (reductions- f x xs))
    ([f init [x & xs]]
     (if x
       (cons init (lazy-seq (reductions- f (f init x) xs)))
       [init]))))

(defcheck solution-769da21c
  (fn rdctns
    ([f init coll]
     (cons init
       (lazy-seq
         (when (seq coll)
           (rdctns f (f init (first coll)) (rest coll))))))
    ([f coll]
     (rdctns f (first coll) (rest coll)))))

(defcheck solution-770936ce
  (fn red
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (red f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (red f (f init (first s)) (rest s))))))))

(defcheck solution-770ee58c
  (fn reduces
    ([f xs] (reduces f (first xs) (rest xs)))
    ([f init xs]
     (lazy-seq (cons init
                 (when-let [xs (seq xs)]
                   (reduces f (f init (first xs)) (rest xs))))))))

(defcheck solution-776d52ad
  (fn r [f & [i l]]
    (letfn [(n [i l]
              (lazy-seq
                (cons i
                  (when (seq l)
                    (n (f i (nth l 0)) (rest l))))))]
      (if l
        (n i l)
        (n (first i) (rest i))))))

(defcheck solution-778b589b
  (fn reds
    ([f agg coll]
     (if (seq coll)
       (lazy-seq (cons agg (reds f (f agg (first coll)) (rest coll))))
       [agg]))
    ([f coll] (reds f (first coll) (rest coll)))))

(defcheck solution-77af62e1
  (fn fero
    ([fun se]
     (lazy-seq
       (fero fun (first se) (rest se))))
    ([fun start se]
     (cons start (fero fun start se :x)))
    ([fun start se mark]
     (lazy-seq
       (if (empty? se)
         []
         (let [
               nn (fun start (first se))
               ]
           (cons nn (fero fun nn (rest se) :x))))))))

(defcheck solution-781fcf64
  (fn __
    ([f l]
     (__ f (first l) (rest l)))
    ([f s l]
     (if (empty? l)
       [s]
       (let [t (f s (first l))]
         (cons s
           (lazy-seq (__ f t (rest l)))))))))

(defcheck solution-789a98b7
  (fn f ([g i a] (if (nil? a) (list i) (cons i (lazy-seq (f g (g i (first a)) (next a)))))) ([g a] (f g (first a) (rest a)))))

(defcheck solution-78b73ce3
  (fn x
    ([f [a & b]]
     (x f a b))
    ([f c [h & t]]
     (lazy-seq (cons c (when h (x f (f c h) t)))))))

(defcheck solution-78d50ff2
  (fn rducts
    ([f aseq] (lazy-seq (rducts f (f (first aseq)) (rest aseq))))
    ([f accum aseq]
     (lazy-seq (cons accum (let [fs (first aseq) rs (rest aseq)]
                             (if (not (seq rs))
                               (list (f accum fs))
                               (rducts f (f accum fs) rs))))))))

(defcheck solution-7975a012
  (fn d
    ([f c] (d f (first c) (rest c)))
    ([f val c]
     (cons val
       (lazy-seq
         (when-let [s (seq c)]
           (d f (f val (first s)) (rest s))))))))

(defcheck solution-79bf3402
  (fn foo
    ([op coll] (foo op (op 0 (first coll)) (rest coll)))
    ([op init coll]
     (if (empty? coll)
       [init]
       (lazy-seq (cons init
                   (foo op (op init (first coll)) (rest coll))))))))

(defcheck solution-7a46ac07
  (fn reds
    ([f coll]
     (lazy-seq
       (reds f (first coll) (rest coll))))
    ([f start coll]
     (cons start
       (lazy-seq
         (when-let [s (seq coll)]
           (reds f (f start (first s)) (rest s))))))))

(defcheck solution-7a4e9072
  (fn reduxes
    ([f aseq]
     (reduxes f (first aseq) (rest aseq)))

    ([f accum aseq]
     (cons accum
       (lazy-seq
         (when-let [[elem & elems] aseq]
           (reduxes f (f accum elem) elems)))))
    ))

(defcheck solution-7a8e0c83
  (fn r
    ([f [a & b]] (r f a b))
    ([f i [a & b :as l]]
     (if (empty? l)
       [i]
       (lazy-seq
         (cons i (r f (f i a) b)))))))

(defcheck solution-7aad583b
  (fn red ([f [c & coll]] (if (nil? c) [] (red f c coll)))
    ([f c1 [c2 & coll]] (if (nil? c2) [c1] (lazy-seq (cons c1 (red f (f c1 c2) coll)))))
    ))

(defcheck solution-7af1741d
  (fn reduce-
    ([f x]
     (reduce- f (first x) (rest x)))
    ([f x [h & t :as coll]]
     (cons x
       (lazy-seq (if (seq coll)
                   (reduce- f (f x h) t)))))))

(defcheck solution-7b20b728
  (fn my-reductions
    ([f init [x & xs :as coll]]
     (if (empty? coll)
       (list init) (cons init (lazy-seq (my-reductions f (f init x) xs)))))
    ([f [x & xs :as coll]] (if (empty? coll) (list (f)) (my-reductions f x xs)))))

(defcheck solution-7b9622b0
  (fn reduct
    ([f, xs]
     (reduct f (first xs) (rest xs)))
    ([f, acc, xs]
     (if (empty? xs)
       (list acc)
       (let [acc1 (f acc (first xs))]
         (cons acc (lazy-seq (reduct f acc1 (rest xs)))))))))

(defcheck solution-7ba18035
  (fn rec
    ([f [n & ns]]
     (rec f n ns))
    ([f n [m & ms]]
     (if m
       (cons n (lazy-seq (rec f (f n m) ms)))
       [n]))))

(defcheck solution-7be88644
  (fn reds
    ([func lst] (if (empty? lst)
                  []
                  (reds func (first lst) (lazy-seq (rest lst)))))
    ([func init lst] (if (empty? lst)
                       [init]
                       (lazy-seq (concat [init] (reds func (func init (first lst)) (rest lst))))))
    ))

(defcheck solution-7c6bc00b
  (fn prob60
    ([f coll]
     (prob60 f (first coll) (rest coll)))
    ([f v coll]
     (if (seq coll)
       (let [next-v    (f v (first coll))
             next-coll (rest coll)
             ]
         (cons v (lazy-seq (prob60 f next-v next-coll)))
         )
       (cons v (lazy-seq '()))))))

(defcheck solution-7c769f80
  (fn seq-reductions
    ([f xs] (seq-reductions f (first xs) (rest xs)))
    ([f z xs]
     (if (empty? xs)
       (list z)
       (cons z (lazy-seq (seq-reductions f (f z (first xs)) (rest xs))))))))

(defcheck solution-7c7c3d4
  (fn red
    ([f sq] (red f (first sq) (rest sq)))
    ([f v sq]
     (if (empty? sq)
       [v]
       (lazy-seq (cons v (red f (f v (first sq)) (rest sq))))))))

(defcheck solution-7c8fad71
  (fn myreduction
    ([x y] (myreduction x (first y) (rest y)))
    ([x y z] (if (empty? z) (vector y) (cons y (lazy-seq (myreduction x (x y (first z)) (rest z))))))))

(defcheck solution-7c90732e
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (f))))
    ([f init coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons init (my-reductions f (f init (first s)) (rest s)))
         (list init))))))

(defcheck solution-7ca1d7c2
  (fn rd
    ([f a]
     (rd f (first a) (rest a)))
    ([f a b]
     (lazy-seq
       (if (not (empty? b))
         (let [r (f a (first b))]
           (cons a (rd f r (rest b)))) [a])))))

(defcheck solution-7ca6bf82
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f x [a & b]]
     (if a (lazy-cat [x] (r f (f x a) b)) [x]))))

(defcheck solution-7ca6ef9a
  (fn g
    ([f a] (g f (f (first a)) (rest a)))
    ([f s a] (cons s (map #(apply f (cons s (take %2 a))) a (rest (range)))))))

(defcheck solution-7ce55bf0
  (fn reduct
    ([f coll] (reduct f (f (first coll)) (rest coll)))
    ([f h coll] (if (empty? coll)
                  [h]
                  (cons h (lazy-seq (reduct f (f h (first coll)) (rest coll))))))))

(defcheck solution-7d688743
  (fn newReductions
    ([f c] (newReductions f (first c) (rest c)))
    ([f i c] (cond
               (empty? c) (lazy-seq [i])
               :else (lazy-cat (lazy-seq [i]) (newReductions f (f i (first c)) (rest c)))))))

(defcheck solution-7d708590
  (fn myred
    ([f coll] (myred f (first coll) (rest coll)))
    ([f x coll]
     (cons x
       (lazy-seq
         (when-let [s (seq coll)]
           (myred f (f x (first s)) (rest s)))))
     )))

(defcheck solution-7d77316f
  (fn pseudo-reductions
    ([f coll]
     (lazy-seq
       (let [s (seq coll)]
         (pseudo-reductions f (first s) (rest s)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (pseudo-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-7daa9397
  (fn my-reductions
    ([f [x & xs]] (my-reductions f x xs))
    ([f init coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons init (my-reductions f (f init (first s)) (rest s)))
         [init])))))

(defcheck solution-7dafdd5e
  (fn my-reductions
    ([op coll] (my-reductions op (first coll) (rest coll)))
    ([op start coll]
     (cons start
       (lazy-seq
         (if (empty? coll)
           []
           (my-reductions op (op start (first coll)) (rest coll))))))))

(defcheck solution-7dd56d04
  (fn f [g x & y]
    (let [i (if y x (first x))
          r (if y (first y) (rest x))]
      (letfn [(h [a b] (if (empty? b)
                         []
                         (let [tmp (g a (first b))]
                           (lazy-seq (concat [tmp] (h tmp (rest b)))))))]
        (concat [i] (h i r))))))

(defcheck solution-7e196dd2
  (fn reducs
    ([f c] (reducs f (first c) (rest c)))
    ([f i c]
     (if (empty? c)
       (list i)
       (cons i (lazy-seq (reducs f (f i (first c)) (rest c))))))))

(defcheck solution-7e39ca66
  (fn [f & r]
    (let [v1 (if (second r) (cons (first r) (second r)) (first r))]
      ((fn fo [v [lf & r]]
         (if lf
           (let [cv (f v lf)]

             (cons v (lazy-seq (fo cv r))
               )
             )
           [v]
           )
         )
       (first v1) (rest v1)
       )

      )
    ))

(defcheck solution-7e493f73
  (fn x
    ([f [a & r]]
     (x f a r))
    ([f s [a & r]]
     (lazy-seq (cons s (if a (x f (f s a) r)))))))

(defcheck solution-7ef72d3d
  (fn rdc
    ([f init coll] (if (empty? coll) init (rdc f (cons init coll))))
    ([f coll] (let [rdc-rec (fn rc [f s c] (if (empty? s) nil
                                                          (let [v (f c (first s))] (lazy-seq (cons v (rc f (next s) v))))))
                    [fst snd & r] coll] (cond (and (nil? fst) (nil? snd)) (cons (f) nil) (nil? snd) (cons fst nil)
                                              :else (let [v (f fst snd)]
                                                      (cons fst (cons v (rdc-rec f r v)))))))

    ))

(defcheck solution-7ef90ed7
  (fn red
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (red f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (red f (f init (first s)) (rest s))))))))

(defcheck solution-7f254d5f
  (fn myreduce
    ([f [r & l]] (myreduce f r l))
    ([f r [h & l]]
     (cons r (if h (lazy-seq (myreduce f (f r h) l))))
     )
    ))

(defcheck solution-7fb1cc9d
  (fn pb60
    ([f coll] (pb60 f nil coll))
    ([f init coll]
     ((fn g [coll
             val]
        (lazy-seq (cons val (when-not (empty? coll)
                              (g (rest coll)
                                (f val (first coll)))))))
      (if init coll (rest coll))
      (or init (f (first coll)))))))

(defcheck solution-8001deda
  (fn rs
    ([f [s & xs]] (rs f s xs))
    ([f s [h & t]]
     (if (nil? h) [s]
                  (lazy-seq (cons s (rs f (f s h) t)))))))

(defcheck solution-8079c5bd
  (fn red
    ([f l] (red f (first l) (rest l)))
    ([f i l]
     (if (empty? l)
       [i]
       (let [n (f i (first l))]
         (cons i (lazy-seq (red f n (rest l)))))))))

(defcheck solution-8166dcb3
  (fn f ([o s] (f o (first s) (rest s))) ([o c s] (if (seq s) (cons c (lazy-seq (f o (o c (first s)) (rest s)))) [c]))))

(defcheck solution-81e41ace
  (fn reductions*
    ([f [x & ys]] (reductions* f x ys))
    ([f x ys]
     (if (empty? ys) [x]
                     (lazy-seq (cons x (reductions* f (f x (first ys)) (rest ys))))))))

(defcheck solution-827411a1
  (fn reduc [f arg & args]
    (if (empty? args)
      (reduc f (first arg) (rest arg))
      (if (empty? (first args))
        (list arg)
        (lazy-seq
          (cons arg
            (reduc f
              (f arg (first (first args)))
              (rest (first args)))))))))

(defcheck solution-82f12673
  (fn !
    ([f [x & xs]] (! f x xs))
    ([f e [x & xs]]
     (let [n (f e x)]
       (if (not-empty xs)
         (cons e (lazy-seq (! f n xs)))
         [e n])))))

(defcheck solution-8321de8b
  (fn reductions2
    ([f coll] (let [i0 (f (first coll) (second coll))] (cons (first coll) (reductions2 f i0 (drop 2 coll)))))
    ([f init coll]
     (if
      (empty? coll) (list init)
                    (cons init (lazy-seq (reductions2 f (f init (first coll)) (rest coll))))))
    ))

(defcheck solution-832a5017
  (fn r ([f v s]
         (lazy-seq
           (cons v
             (if (seq s)
               (r f (f v (first s)) (rest s))))))
    ([f s] (r f (first s) (rest s)))))

(defcheck solution-83646456
  (fn f
    ([g s] (f g (first s) (next s)))
    ([g i s]
     (cons i
       (lazy-seq
         (when (first s)
           (f g (g i (first s)) (next s))))))))

(defcheck solution-8379baf1
  (fn foo
    ([f s] (foo f (first s) (next s)))
    ([f v s]
     (if s
       (cons v (lazy-seq (foo f (f v (first s)) (next s))))
       (list v)))))

(defcheck solution-83bb2a59
  (fn myred
    ([f coll] (myred f (first coll) (rest coll)))
    ([f val coll]
     (if (seq coll)
       (let [nval (f val (first coll))]
         (cons val (lazy-seq (myred f nval (rest coll)))))
       [val]))))

(defcheck solution-8434e5ce
  (fn red ([f v coll]
           (if (empty? coll)
             (list v)
             (let [r (f v (first coll))]
               (cons v (lazy-seq (red f r (rest coll)))))
             )
           )
    ([f coll]
     (red f (first coll) (rest coll))
     )
    ))

(defcheck solution-84a886b6
  (fn
    ([f coll]
     (letfn [(r [a coll]
               (lazy-seq
                 (if (seq coll)
                   (let [fst (first coll)
                         a'  (f a fst)]
                     (cons a (r a' (rest coll))))
                   (list a))))]
       (if (seq coll)
         (r (first coll) (rest coll))
         (list (f)))))
    ([f val coll]
     (letfn [(r [a coll]
               (lazy-seq
                 (if (seq coll)
                   (let [fst (first coll)
                         a'  (f a fst)]
                     (cons a (r a' (rest coll))))
                   (list a))))]
       (if (seq coll)
         (r val coll)
         (list val))))))

(defcheck solution-84c0927e
  (fn my-reductions
    ([fun [x & xs]]
     (my-reductions fun x xs))
    ([fun reduced [x & xs]]
     (if x
       (lazy-seq (cons reduced (my-reductions fun (fun reduced x) xs)))
       (vector reduced)))))

(defcheck solution-84de30f4
  (fn red
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (red f (first s) (rest s)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (red f (f init (first s)) (rest s))))))))

(defcheck solution-85a5a51b
  (fn [& args]
    (let [fnc (first args)
          a   (if (= (count args) 3) (last args) (drop 1 (last args)))
          c   (if (= (count args) 3) (vector (second args)) [(first (last args))])]

      ((fn red [fnc c a] (let [[c1 & cr] c
                               [a1 & ar] a
                               x (fnc c1 a1)]
                           (if (not (empty? ar))
                             (cons c1 (lazy-seq (red fnc (cons x (cons c1 cr)) ar)))
                             (vector c1 x)))) fnc c a)
      )
    ))

(defcheck solution-85be6ea1
  (fn w
    ([f v [s & r]]
     (lazy-seq
       (if s
         (cons v (w f (f v s) r))
         [v])))
    ([f [s & r]] (w f s r))))

(defcheck solution-85d0e97
  (fn z
    ([f [x & r]] (z f x r))
    ([f a [x & r]] (cons a (and x (lazy-seq (z f (f a x) r)))))))

(defcheck solution-86fdeed0
  (fn r
    ([f xs] (r f (first xs) (rest xs)))
    ([f seed xs]
     (if (empty? xs)
       [seed]
       (lazy-seq
         (cons seed
           (r f
             (f seed (first xs))
             (rest xs))))))))

(defcheck solution-8704f298
  (fn myreductions
    ([f coll] (myreductions f (first coll) (rest coll)))
    ([f val coll]
     (cons val
       (lazy-seq
         (when-let [s (seq coll)]
           (myreductions f (f val (first s)) (rest s))))))))

(defcheck solution-87114192
  (fn mf
    ([f [a1 & s1]]
     (lazy-seq (cons a1 (mf f (cons (f a1 (first s1)) (rest s1))))))
    ([f sa [a1 & s1]]
     (take (+ 2 (count s1)) (lazy-seq (cons sa (mf f (f sa (if (nil? a1) 0 a1)) s1)))))))

(defcheck solution-87538f25
  (fn rdcn
    ([f coll]
     (if-let [s (seq coll)]
       (rdcn f (first coll) (rest coll))
       (list (f))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (rdcn f (f init (first s)) (rest s))))))))

(defcheck solution-880ddccb
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (map #(reduce f init %)
         (rest (map (fn [n _] (take n coll))
                 (range)
                 (cons 0 coll))))))))

(defcheck solution-886204f7
  (fn reduxions
    ([f a s]
     (if (empty? s)
       (list a)
       (lazy-seq (cons a (reduxions f (f a (first s)) (rest s))))))
    ([f s]
     (reduxions f (first s) (rest s)))))

(defcheck solution-88886112
  (fn my-reductions
    ([fun col]
     (my-reductions fun (first col) (rest col)))
    ([fun val col]
     (cons
       val
       (lazy-seq
         (when (seq col)
           (my-reductions fun (fun val (first col)) (rest col))))))))

(defcheck solution-88943894
  (fn reds
    ([f [h & t]] (reds f h t))
    ([f i [h & t]]
     (if-not h
       [i]
       (lazy-cat [i]
         (reds f (f i h) t))))))

(defcheck solution-88e6941a
  (fn _reductions
    ([f xs] (_reductions f nil (rest xs) [(first xs)]))
    ([f init coll] (lazy-seq (cons init (_reductions f init (rest coll) [(first coll)]))))
    ([f init coll acc]
     (let [r (if (= init nil) (reduce f acc) (reduce f init acc))]
       (if (empty? coll) [r]
                         (lazy-seq
                           (cons r (_reductions f init (rest coll) (conj acc (first coll))))))))))

(defcheck solution-88ee8007
  (fn red
    ([f coll] (red f 0 (rest coll)))
    ([f start coll]
     (cons start ((fn rec [prev coll]
                    (if (empty? coll) []
                                      (let [next (f prev (first coll))]
                                        (cons next (lazy-seq (rec next (rest coll)))))))
                  start coll)))))

(defcheck solution-89050616
  (fn r

    ([f [v & s]]
     (r f v s))

    ([f v [n & s]]
     (cons v (if n
               (lazy-seq (r f (f v n) s)))))
    ))

(defcheck solution-891239da
  (fn my-reduce-inter3
    ([fun lis] (my-reduce-inter3 fun (first lis) (rest lis)))
    ([fun ini lis]
     (if (not= lis [])
       (let [aux (fun ini (first lis))]
         (lazy-seq (cons ini (my-reduce-inter3 fun aux (rest lis)))))
       (list ini)))))

(defcheck solution-89c3dd98
  (fn reds
    ([f i s]
     (lazy-seq
       (if-not (nil? s)
         (cons i (reds f (f i (first s)) (next s)))
         (list i))))
    ([f s] (reds f (first s) (next s)))))

(defcheck solution-89e8af7b
  (fn rds
    ([f s] (rds f (first s) (next s)))
    ([f x s]
     (cons x
       (lazy-seq
         (if s
           (rds f (f x (first s)) (next s))))))))

(defcheck solution-8a1feecc
  (fn seq-reductions
    ([f [x & xs]] (seq-reductions f (f x) xs))
    ([f acc [x & xs]]
     (if (nil? x)
       (list acc)
       (cons acc (lazy-seq (seq-reductions f (f acc x) xs)))))))

(defcheck solution-8aa881be
  (fn reduction
    ([f coll] (reduction f (first coll) (rest coll)))
    ([f val coll]
     (cons val (lazy-seq (when-let [s (seq coll)]
                           (reduction f (f val (first s)) (rest s)))))
     )
    ))

(defcheck solution-8bb6aa42
  (fn redctns
    ([fun coll] (redctns fun (first coll) (rest coll)))
    ([fun start coll] ((fn helper [p c]
                         (if (empty? c)
                           [p]
                           (cons p
                             (lazy-seq (helper (fun p (first c))
                                         (rest c))))))
                       start coll))))

(defcheck solution-8c660e7b
  (fn my-reduction
    ([f x xs]
     (if (seq xs)
       (lazy-cat [x] (my-reduction f (f x (first xs)) (rest xs)))
       [x]))
    ([f xs]
     (if (seq xs)
       (my-reduction f (first xs) (rest xs))))))

(defcheck solution-8c713344
  (fn r
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (r f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-8c81d49e
  (fn toy-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (toy-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons init (toy-reductions f (f init (first s)) (rest s)))
         (list init))))))

(defcheck solution-8cb63591
  (fn kk
    ([op input] (kk op (first input) (rest input)))
    ([op result input]
     (lazy-seq
       (if (empty? input)
         (list result)
         (cons result (kk
                        op
                        (op result (first input))
                        (rest input))))))))

(defcheck solution-8cb8616e
  (fn reducts
    ([f [arg & args]]
     (reducts f arg args))
    ([f v [arg & args]]
     (if-not arg
       (list v)
       (cons v (lazy-seq (reducts f (f v arg) args)))))))

(defcheck solution-8d1c509e
  (fn r
    ([f [x & s]]
     (lazy-seq (r f x s)))
    ([f i [x & s]]
     (cons i
       (when x
         (lazy-seq (r f (f i x) s)))))))

(defcheck solution-8d9f0bba
  (fn f
    ([g [h & t]]
     (f g h t))
    ([g i [h & t :as s]]
     (lazy-seq (cons i (if s (f g (g i h) t) []))))))

(defcheck solution-8dc4df6b
  (fn reductions-1
    ([f coll]
     (cons (first coll)
       (lazy-seq (reductions-1 f (cons (f (first coll)
                                         (second coll))
                                   (rest (rest coll)))))))
    ([f init coll]
     ;(if-not (nil? (last coll)) (reductions-3 f init (conj coll nil))
     ;(if (seq? coll) '()
     (cons init
       (lazy-seq
         (when (seq coll)
           (reductions-1 f (f init (first coll))
             (rest coll))))))))

(defcheck solution-8dc55783
  (fn r
    ([f s] (r f (nth s 0) (rest s)))
    ([f x s]
     (concat [x]
             (keep-indexed
               (fn [a b] (reduce f x (take (inc a) s)))
               s)))))

(defcheck solution-8dcd907d
  (fn my-reductions
    ([f xs]
     (when (seq xs) (my-reductions f (first xs) (rest xs))))
    ([f init-val xs]
     (lazy-seq
       (cons init-val
         (when (seq xs) (my-reductions f (f init-val (first xs)) (rest xs))))))))

(defcheck solution-8e173952
  (fn r
    ([f c]
     (r f (first c) (rest c)))
    ([f i c]
     (lazy-seq
       (cons i
         (if-let [c (seq c)]
           (r f (f i (first c)) (next c))))))))

(defcheck solution-8f2a7999
  (fn r
    ([f coll] (r f (first coll) (next coll)))
    ([f init coll]
     (if (seq coll)
       (lazy-seq (cons init (r f (f init (first coll)) (next coll))))
       (cons init [])))))

(defcheck solution-8f3252cd
  (fn reductions*
    ([f coll] (reductions* f (first coll) (rest coll)))
    ([f init coll]
     (cons init ((fn helper [f init coll]
                   (if (empty? coll)
                     []
                     (let [v (f init (first coll))]
                       (cons v (lazy-seq (helper f v (rest coll))))))) f init coll)))))

(defcheck solution-8f76b52
  (fn [of & vs]
    ((fn rrr [f xs current]
       (lazy-seq
         (if (empty? xs)
           (list (f current))
           (cons
             (f current)
             (rrr
               f
               (rest xs)
               (conj current (first xs)))))))
     (if (empty? (rest vs)) (partial reduce of (first (last vs))) (partial reduce of (first vs)))
     (if (empty? (rest vs)) (rest (last vs)) (last vs))
     [])))

(defcheck solution-8f7b85f2
  (fn my-reduction
    ([f coll]
     (my-reduction f (first coll) (rest coll)))
    ([f val coll]
     (if (seq coll)
       (lazy-seq
         (cons val (my-reduction f (f val (first coll)) (rest coll))))
       [val]))))

(defcheck solution-8fb5ef91
  (fn reduxions
    ([op prev s]
     (lazy-seq
       (if (seq s)
         (cons prev (reduxions op (op prev (first s)) (rest s)))
         [prev])))
    ([op s]
     (reduxions op (op (first s)) (rest s)))))

(defcheck solution-8fe1abc1
  (letfn
   [
    (rsx [op col]
      (if
       (not (empty? (rest col)))
        (let [fi       (first col)
              sd       (second col)
              firstres (op fi sd)
              remains  (rest (rest col))]
          (lazy-seq (cons firstres (rsx op (cons firstres remains)))))))
    ]
    (fn srf
      ([op col]
       (rsx op (cons (first col) col)))
      ([op fst col]
       (cons fst (rsx op (cons fst col)))))))

(defcheck solution-8ff292cf
  (fn reductions'
    ([f [head & tail]] (reductions' f (f head) tail))
    ([f coll [head & tail]]
     (lazy-seq (cons coll (if (seq tail)
                            (reductions' f (f coll head) tail)
                            [(f coll head)]))))))

(defcheck solution-8ff48b8f
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f val coll]
     (lazy-seq
       (cons val
         (when (not-empty coll)
           (my-reductions f (f val (first coll)) (rest coll))))))))

(defcheck solution-90607cdb
  ; This is some fancy mojo.

  (fn reduce-
    ([f coll] (reduce- f (first coll) (next coll)))
    ([f init
      [h & t :as coll]]
     (cons init
       (lazy-seq
         (if (seq coll)
           (reduce- f (f init h) t)))))))

(defcheck solution-906d9d21
  (fn rd
    ([f args] (rd f (first args) (next args)))
    ([f init args]
     (if (empty? args)
       (seq (list init))
       (lazy-seq
         (cons init (rd f (f init (first args)) (next args))))))))

(defcheck solution-918893dc
  (fn my-redux
    ([f vs] (my-redux f (first vs) (rest vs)))
    ([f i vs]
     (concat (list i)
             (map-indexed
               (fn [j _] (apply f (concat (list i) (take (inc j) vs)))) vs)))))

(defcheck solution-91f14033
  (fn reds
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reds f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reds f (f init (first s)) (rest s))))))))

(defcheck solution-921cb207
  (fn reduction-steps
    ([f seq]
     (reduction-steps f (first seq) (rest seq)))
    ([f init seq]
     (if (empty? seq)
       (cons init nil)
       (lazy-seq (cons init (reduction-steps f (f init (first seq)) (rest seq))))))))

(defcheck solution-9228f172
  (fn ! ([f a [c & d :as e]]
         (cons a
           (if (seq e)
             (lazy-seq (! f (f a c) d))))) ([f [a & c]] (! f a c))))

(defcheck solution-92670209
  (fn R
    ([f xs]
     (R f (first xs) (rest xs)))
    ([f val xs]
     (if (seq xs)
       (lazy-seq
         (cons val
           (let [red (f val (first xs))]
             (R f red (rest xs)))))
       [val]))))

(defcheck solution-92dd4940
  (fn my-reduction
    ([f coll]
     (my-reduction f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when (seq coll) (my-reduction f
                            (f init (first coll))
                            (rest coll))))))))

(defcheck solution-936dca1d
  (fn r
    ([f [h & b]] (r f h b))
    ([f i [h & b]]
     (cons i
       (lazy-seq
         (when h
           (r f (f i h) b)))))))

(defcheck solution-945558ba
  (fn r
    ([f [a & b]] (r f a b))
    ([f a [b & c]]
     (cons a (if b (lazy-seq (r f (f a b) c)))))))

(defcheck solution-946265b3
  (fn [f xs & xs']
    (let [x  (if (empty? xs') (first xs) xs)
          ys (if (empty? xs') (rest xs) (first xs'))]
      (letfn [(go [rs ns]
                (if (empty? ns)
                  ns
                  (let [r (f (last rs) (first ns))]
                    (cons
                      r
                      (lazy-seq (go (conj rs r) (rest ns))))))
                )
              ]
        (cons
          x
          (lazy-seq (go [x] ys)))
        )
      )
    ))

(defcheck solution-946c8eca
  (fn sred
    ([f coll] (sred f (first coll) (rest coll)))
    ([f i coll]
     (if (seq coll)
       (cons i (lazy-seq (sred f (f i (first coll)) (rest coll))))
       [i]))))

(defcheck solution-9574c1d7
  ; A horrible cheat; I copied reductions from core.clj.
  ; I was spending too much time on this one.
  (fn [& args]
    (letfn [(recur-with-capture
              ([f coll]
               (lazy-seq
                 (if-let [s (seq coll)]
                   (recur-with-capture f (first s) (rest s))
                   (list (f)))))
              ([f init coll]
               (cons init
                 (lazy-seq
                   (when-let [s (seq coll)]
                     (recur-with-capture f (f init (first s)) (rest s)))))))]
      (apply recur-with-capture args))))

(defcheck solution-961c3994
  (fn reduc
    ([f [a & tail]] (reduc f a tail))
    ([f a [b & tail :as xs]]
     (if (empty? xs)
       (list a)
       (cons a (lazy-seq (reduc f (f a b) tail))))
     )))

(defcheck solution-9641856
  (fn myredx
    ([f init seq]
     (lazy-seq
       (if (empty? seq)
         (list init)
         (cons init (myredx f (f init (first seq)) (rest seq))))))
    ([f seq]
     (myredx f (first seq) (rest seq)))))

(defcheck solution-96d5c6b4
  (fn reduc
    ([f coll]
     (reduc f (first coll) (rest coll))
     )
    ([f arg coll]
     (letfn [(iter [left right]
               (cond (empty? right) right
                     :else (let [v (f (last left) (first right))]
                             (cons v (lazy-seq (iter (list v) (rest right))))
                             )
                     )
               )]
       (cons arg (iter (list arg) coll))
       )
     )
    ))

(defcheck solution-97023a9b
  (letfn [(reductions'
            ([f init [x & xs]]
             (lazy-seq
               (let [redu ((fnil f 0 0) init x)]
                 (if (nil? x)
                   (list init)
                   (cons init (reductions' f redu xs)))))))]
    (fn
      ([f [x & xs]] (reductions' f x xs))
      ([f init x] (reductions' f init x)))))

(defcheck solution-9713d40d
  (fn my-reductions
    ([f x coll]
     (if (nil? (first coll))
       (list x)
       (cons
         x
         (lazy-seq (my-reductions f (f x (first coll)) (rest coll))))))

    ([f coll]
     (lazy-seq (my-reductions f (first coll) (rest coll))))))

(defcheck solution-9726975
  (fn redus
    ([f [x & xs]] (redus f x xs))
    ([f acc xs]
     (let [a (atom acc)]
       (cons acc
         (map (partial swap! a f)
           xs))))))

(defcheck solution-973343d6
  (fn seq-reduce
    ([f init lst]
     (if (empty? lst)
       [init]
       (let [[item & remaining] lst
             next-init (f init item)]
         (cons init (lazy-seq (seq-reduce f next-init remaining))))))
    ([f lst]
     (if (empty? lst)
       (list (f))
       (seq-reduce f (first lst) (rest lst))))))

(defcheck solution-97446651
  (fn r
    ([f a0 c] (r f (cons a0 c)))
    ([f [a v & vr]] (
                      lazy-seq (
                                 let [an (f a v)] (
                                                    if (empty? vr)
                                                    [a an]
                                                    (cons a (r f (cons an vr)))
                                                    )))
     )))

(defcheck solution-97533dd2
  (fn a ([f x xs] (lazy-seq (if-let [t (first xs)] (cons x (a f (f x t) (rest xs))) [x]))) ([f xs] (a f (first xs) (next xs)))))

(defcheck solution-97850ccb
  (fn rr
    ([f xs]
     (let [s (seq xs)]
       (rr f (first s) (rest s))))
    ([f x xs]
     (lazy-seq
       (cons x
         (when-let [s (seq xs)]
           (rr f (f x (first s)) (rest s))))))))

(defcheck solution-97be6736
  (fn [x y & z]
    (letfn [(rns [f zero xs]
              (lazy-cat [zero]
                ((fn iter [ys prevVal]
                   (if (empty? ys) []
                                   (let [nextVal (f prevVal (first ys))
                                         restys  (rest ys)]
                                     (lazy-cat [nextVal]
                                       (iter (rest ys) nextVal)))))
                 xs zero)))]
      (if (nil? z)
        (rns x (first y) (rest y))
        (rns x y (first z))))))

(defcheck solution-98bd78eb
  (fn rdct
    ([f init coll] (rdct f (cons init coll)))
    ([f coll]
     (if (seq (rest coll))
       (cons (first coll)
         (lazy-seq (rdct f (conj (rest (rest coll))
                             (f (first coll) (second coll))))))
       (list (first coll))))))

(defcheck solution-98fdafc9
  (fn r
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (r f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-99bca203
  (fn reduct
    ([f [x & coll]] (reduct f x coll))
    ([f acc coll]
     (lazy-seq
       (if-let [[x & coll] (seq coll)]
         (cons acc (reduct f (f acc x) coll))
         [acc])))))

(defcheck solution-9a02d0e9
  (fn my-reductions
    ([f b coll]
     (if (empty? coll)
       [b]
       (let [c (f b (first coll))]
         (lazy-cat [b] (my-reductions f c (rest coll))))))
    ([f coll]
     (if (empty? coll)
       coll
       (my-reductions f (first coll) (rest coll))))))

(defcheck solution-9a223b9f
  (fn red
    ([f x s]
     (if (empty? s) (cons x s)
                    (lazy-seq (cons x (red f (f x (first s)) (rest s))))))
    ([f s] (red f (first s) (rest s)))))

(defcheck solution-9a4d251e
  (fn r ([f a [b & t]] (if b (cons a (lazy-seq (r f (f a b) t))) [a])) ([f [x & c]] (r f x c))))

(defcheck solution-9aa5a06e
  (fn red
    ([f seq] (red f (first seq) (rest seq)))
    ([f ini seq]
     (if (empty? seq)
       [ini]
       (lazy-seq
         (cons ini
           (red f (f ini (first seq)) (rest seq))))))))

(defcheck solution-9aa6ca9a
  (fn reddo
    ([afun acoll] (reddo afun (first acoll) (rest acoll)))
    ([afun aprev acoll]
     (cons aprev ((fn red
                    ;([fun coll] (cons (first coll) (lazy-seq (red fun (first coll) (rest coll)))))
                    ([fun prev coll]
                     (if (empty? coll)
                       []
                       (cons (fun prev (first coll)) (lazy-seq (red fun (fun prev (first coll)) (rest coll)))))))
                  afun
                  aprev
                  acoll)))))

(defcheck solution-9acb2b3c
  (fn myf
    ([f coll] (myf f (first coll) (rest coll)))
    ([f a coll] (if (empty? coll)
                  (list a)
                  (cons a (lazy-seq (myf f (f a (first coll)) (rest coll))))))))

(defcheck solution-9b1ad792
  (fn [& i] (let [a (first i) b (if (= 3 (count i)) (last i) (rest (last i))) c (if (= 3 (count i)) (second i) 0)] (cons c (map (fn [x] (reduce a c (take x b))) (map #(inc (first %)) (map-indexed vector b)))))))

(defcheck solution-9b3eaa49
  (fn reduct
    ([f items]
     (reduct f (first items) (rest items)))

    ([f prior items]
     (if (seq items)
       (lazy-seq (cons prior (reduct f (f prior (first items)) (rest items))))
       (list prior)))))

(defcheck solution-9b549ff7
  (fn my-reductions
    ([f coll]
     (if (empty? coll)
       []
       (my-reductions f (first coll) (drop 1 coll))))
    ([f acc coll]
     (if (empty? coll)
       (list acc)
       (cons acc (lazy-seq (my-reductions f (f acc (first coll)) (drop 1 coll))))))))

(defcheck solution-9bcad3f3
  (fn r
    ([f l] (r f (first l) (rest l)))
    ([f i l]
     (if (empty? l)
       [i]
       (lazy-seq (cons i
                   (r f (f i (first l)) (rest l))))))))

(defcheck solution-9bd12ad4
  (fn r3duct10ns
    ([f x [y & r]] (lazy-seq (cons x (if y (r3duct10ns f (f x y) r)))))
    ([f [x & r]] (r3duct10ns f x r))))

(defcheck solution-9c773ae9
  (fn red
    ([f i s] (->> [i true s]
               (iterate (fn [[a t [b & n]]] [((fnil f a a) a b) b n]))
               (take-while second)
               (map first)))
    ([f [i & s]] (red f i s))
    ))

(defcheck solution-9c816cff
  (fn rs
    ([f s]
     (rs f (first s) (rest s)))
    ([f x s]
     (lazy-seq
       (cons x (if (empty? s)
                 ()
                 (rs f (f x (first s)) (rest s))))))))

(defcheck solution-9c9341b4
  (fn foo
    ([f a s]
     (if (empty? s) (list a)
                    (lazy-seq
                      (cons a (foo f (f a (first s)) (rest s))))))
    ([f s]
     (foo f (first s) (rest s)))))

(defcheck solution-9cb70039
  (fn reduceit
    ([f x0 xs] (if (empty? xs)
                 (list x0)
                 (cons x0 (lazy-seq (reduceit f (f x0 (first xs)) (rest xs))))))
    ([f xs] (lazy-seq (reduceit f (first xs) (rest xs))))))

(defcheck solution-9d3c2f45
  (fn _
    ([f [h & r]] (_ f h r))
    ([f i [h & r]]
     (lazy-seq
       (cons i
         (when h
           (_ f (f i h) r)))))))

(defcheck solution-9d61cbb8
  (fn [f & args]
    (when-let [coll (-> args last seq)]
      (let [
            fv (or (first (butlast args)) (f))
            ff (fn ff [v f coll]
                 (lazy-seq
                   (when (seq coll)
                     (let [
                           cv (f v (first coll))]
                       (cons cv (ff cv f (next coll)))))))]
        (cons fv (ff fv f (if-not (empty? (butlast args)) coll (next coll))))))))

(defcheck solution-9d7ad8ba
  (fn reds
    ([f coll]
     (reds f (first coll) (rest coll)))
    ([f init [h & t :as coll]]
     (cons init
       (when (seq coll)
         (lazy-seq (reds f (f init h) t)))))))

(defcheck solution-9e38de42
  (fn re
    ([op xs] (re op (first xs) (rest xs)))
    ([op start-value xs]
     (if (empty? xs)
       (list start-value)
       (cons start-value
         (lazy-seq (re op
                     (op start-value (first xs))
                     (rest xs))))))))

(defcheck solution-9e3dfe7c
  (fn my-reductions
    ([func coll]
     (my-reductions func (first coll) (rest coll)))
    ([func initial coll]
     (if-let [coll (seq coll)]
       (lazy-seq
         (cons
           initial
           (my-reductions func (func initial (first coll)) (rest coll))))
       (list initial)))))

(defcheck solution-9e78e98c
  (fn seq-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (seq-reductions f (first coll) (rest coll))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (if-let [s (seq coll)]
           (seq-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-9e7a0e60
  (fn red
    ([f xs]
     (cons (first xs)
       ((fn r [[x & rst]]
          (when-not (empty? rst)
            (let [y (f x (first rst))]
              (lazy-seq (cons y
                          (r (cons y (rest rst))))))))
        xs)))
    ([f v xs]
     (red f (cons v xs)))))

(defcheck solution-9e7d11ac
  (fn myreduct
    ([func coll]
     (myreduct func (first coll) (rest coll)))
    ([func firstval coll]
     (letfn [(_reduct [_func _firstval _coll]
               (lazy-seq (when-not (empty? _coll)
                           (let [init (_func _firstval (first _coll))]
                             (cons init (_reduct _func init (rest _coll)))))))]
       (lazy-seq (cons firstval (_reduct func firstval coll)))))))

(defcheck solution-9e8edbfd
  (fn f
    ([x y] (f x (first y) (rest y)))
    ([x y z] (if z (lazy-cat [y] (f x (x y (first z)) (next z))) [y]))))

(defcheck solution-9efc19
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f val coll]
     (if (empty? coll)
       [val]
       (lazy-seq (cons val (r f (f val (first coll)) (rest coll))))))))

(defcheck solution-9f14eeac
  (fn r ([f c]
         (r f (first c) (rest c)))
    ([f i c]
     (cons i (lazy-seq (when-let [s (seq c)] (r f (f i (first c)) (rest c))))))))

(defcheck solution-9f52335c
  (fn !
    ([f coll]
     (! f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       ((fn rcr [i [h & t]]
          (if h
            (let [r (f i h)]
              (lazy-seq (cons r (rcr r t)))))) init coll)))))

(defcheck solution-9fa029bc
  (fn lazyR
    ([f lst] (lazyR f (first lst) (rest lst)))
    ([f n lst]
     (lazy-seq
       (cons n
         (when-let [s (seq lst)] (lazyR f (f n (first lst)) (next lst))))))))

(defcheck solution-9ffc3863
  (fn my-reductions
    ([fun val acoll]
     (if (empty? acoll)
       [val]
       (lazy-seq (cons val
                   (my-reductions fun (fun val (first acoll)) (rest acoll))))
       ))
    ([fun acoll]
     (my-reductions fun (first acoll) (rest acoll))
     )
    ))

(defcheck solution-a0137a93
  (fn iterduce
    ([f s] (iterduce f (first s) (rest s)))
    ([f i s]
     (if (empty? s) [i]
                    (cons i (lazy-seq (iterduce f (f i (first s)) (rest s))))))))

(defcheck solution-a0510e6a
  (fn l
    ([f i [h & r]]
     (let [v (f i h)]
       (if (seq r)
         (cons i (lazy-seq (l f v r)))
         (cons i (list v)))))
    ([f [h & r]]
     (l f h r))))

(defcheck solution-a0a2018e
  (let [
        reds
        (fn reds [f acc coll]
          (lazy-seq
            (if (seq coll)
              (cons
                acc
                (reds f (f acc (first coll)) (next coll))
                )
              (cons acc nil)
              ))
          )
        ]
    (fn
      ([f i coll] (reds f i coll))
      ([f coll] (reds f (first coll) (next coll)))
      )
    ))

(defcheck solution-a0aceff5
  (fn z
    ([f [p & q]] (z f p q))
    ([f i s]
     (letfn [(r [a [x & y]]
               (lazy-seq
                 (if x
                   (let [m (f a x)]
                     (cons a (r m y)))
                   (cons a nil))))]
       (r i s)))))

(defcheck solution-a0c5b04c
  (fn r
    ([f b] (r f (first b) (rest b)))
    ([f a b] (if (empty? b) [a] (cons a (lazy-seq (r f (f a (first b)) (rest b))))))))

(defcheck solution-a1180dca
  (fn reduct
    ([fun s] (reduct fun (first s) (rest s)))
    ([fun init s] ((fn reduct' [acc [frst & rst]]
                     (cons acc (if (empty? rst)
                                 (list (fun acc frst))
                                 (lazy-seq (reduct' (fun acc frst) rst)))))
                   init s))))

(defcheck solution-a15dd537
  (fn my-reductions
    ([f col]
     (lazy-seq
       (if-let [s (seq col)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init col]
     (cons init
       (lazy-seq
         (when-let [s (seq col)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-a1b14455
  (fn red
    ([f collec]
     (red f (first collec) (drop 1 collec)))
    ([f ini collec]
     (let [acc (atom ini)] (cons ini (map #(swap! acc f %) collec))))))

(defcheck solution-a1b8a1b6
  (fn red ([f coll] (red f (first coll) (next coll)))
    ([f val coll]
     (lazy-seq
       (if-let [[x & xs] (seq coll)]
         (cons val (red f (f val x) xs))
         (list val))))))

(defcheck solution-a2663348
  (fn f
    ([o [a & b]] (f o a b))
    ([o a [b & c]]
     (lazy-seq
       (cons a
         (when b
           (f o (o a b) c)))))))

(defcheck solution-a3088af5
  (fn ff
    ([f init vals]
     (lazy-seq
       (if (empty? vals)
         [init]
         (let [v (f init (first vals))]
           (cons init (ff f v (rest vals)))))))
    ([f vals]
     (ff f (first vals) (rest vals)))))

(defcheck solution-a32d99a1
  (fn rs
    ([v f s]
     (if (not (empty? s))
       (let [t (conj v (first s))]
         (cons (reduce f t)
           (lazy-seq (rs t f (rest s)))))))
    ([v f i s]
     (rs v f (cons i s)))
    ) [])

(defcheck solution-a34f3cf0
  (fn seq-reduce
    ([f s]
     (seq-reduce f (first s) (rest s)))
    ([f r s]
     (cons r (seq-reduce true f r s)))
    ([_ f r [a & s]]
     (if (nil? s)
       [(f r a)]
       (lazy-seq
         (cons
           (f r a)
           (seq-reduce true f (f r a) s)))))))

(defcheck solution-a3659e0d
  (fn sequence-reductions [& args]
    (letfn [(red [f vv & aa]
              (lazy-seq
                (let [a (if (empty? aa) (first vv) (first aa))
                      v (if (empty? aa) (rest vv) vv)]

                  (if (empty? v) [a]
                                 (cons a (red f (rest v) (f a (first v)))))

                  )))

            (handler
              ([f v]
               (red f v))
              ([f a v]
               (red f v a)))]

      (apply handler args))))

(defcheck solution-a3a180b4
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq (cons
                 init
                 (if (seq coll)
                   (my-reductions f (f init (first coll)) (rest coll))))))))

(defcheck solution-a3b531ce
  (fn reds
    ([f coll]
     (reds f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (if (seq coll)
         (lazy-seq (reds f (f init (first coll)) (next coll)))
         nil)))))

(defcheck solution-a4304446
  (fn my-reductions
    ([f s] (my-reductions f (first s) (rest s)))
    ([f v s]
     (if (empty? s) [v]
                    (cons v (lazy-seq
                              (my-reductions f (apply f (list v (first s))) (rest s))))))))

(defcheck solution-a4fc34da
  (fn !
    ([f [x & xs]]
     (! f x xs))
    ([f init coll]
     (cons init (lazy-seq
                  (when-let [s (seq coll)]
                    (! f (f init (first s)) (rest s))))))))

(defcheck solution-a50452a4
  (fn my-reductions
    ([f init coll]
     (cons init
       (lazy-seq
         (when (seq coll)
           (my-reductions f (f init (first coll)) (rest coll))))))
    ([f coll]
     (when (seq coll)
       (lazy-seq (my-reductions f (first coll) (rest coll)))))))

(defcheck solution-a513eb0
  (fn __
    ([f [s1 & s]]
     (__ f s1 s))
    ([f v [s1 & s]]
     (lazy-seq
       (cons v (if s1
                 (__ f (f v s1) s)))))))

(defcheck solution-a516c91f
  (fn red
    ([f [x1 x2 & xs]] (concat [x1] (red f (f x1 x2) xs)))
    ([f acc [x & xs]]
     (lazy-seq
       (let [acc' (f acc x)]
         (cons acc (if (empty? xs) [acc'] (red f acc' xs))))))))

(defcheck solution-a53437e4
  (fn rn
    ([f l]
     (rn f (first l) (rest l)))
    ([f i l]
     (if (empty? l)
       (cons i nil)                                         ; return a seq for recursive cons.
       (lazy-seq (cons i (rn f (f i (first l)) (rest l))))))))

(defcheck solution-a55d8177
  (fn g
    ([f [h & t]] (g f h t))
    ([f x r] (if (empty? r) [x] (lazy-cat [x] (g f (f x (first r)) (rest r)))))))

(defcheck solution-a585930f
  (fn my-reduce
    ([f collection]
     (my-reduce f (first collection) (rest collection)))
    ([f first-value collection]
     (if (empty? collection)
       [first-value]
       (cons first-value (lazy-seq (my-reduce f (f first-value (first collection)) (rest collection)))))
     )))

(defcheck solution-a5b45a43
  (fn reds
    ([f s] (reds f (first s) (rest s)))
    ([f a s]
     (cons a
       (lazy-seq
         (when (seq s)
           (reds f (f a (first s)) (rest s))))))))

(defcheck solution-a5d361d1
  (fn r
    ([f [x & xs]] (r f x xs))
    ([f v [x & xs]] (lazy-seq (cons v (when x (r f (f v x) xs)))))))

(defcheck solution-a646cfba
  (fn red
    ([f v] (red f (first v) (rest v)))
    ([f n v] (cons n
               (lazy-seq (if (empty? v) nil (red f (f n (first v)) (rest v))))))))

(defcheck solution-a65a9ba7
  (fn f
    ([op a] (f op (first a) (rest a)))
    ([op a b]
     (if (seq b)
       (lazy-seq (cons a (f op (op a (first b)) (rest b))))
       [a]))))

(defcheck solution-a694f50b
  (fn redz
    ([f s] (redz f (first s) (rest s)))
    ([f n s] (lazy-seq (cons n
                         (if (seq s) (redz f (f n (first s)) (rest s))))))))

(defcheck solution-a6c3f0eb
  (fn my-reduce
    ([f [h & t]] (my-reduce f h t))
    ([f a col]
     (letfn [(r
               [p [h & t]]
               (if (nil? h)
                 nil
                 (let [n (f p h)]
                   (cons
                     n
                     (lazy-seq (r n t))))))]
       (if (nil? a)
         []
         (cons a (lazy-seq (r a col))))))))

(defcheck solution-a719e333
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-a78932a4
  (fn red
    ([f s]
     (red f (first s) (rest s)))
    ([f i s]
     (lazy-seq (cons i (when (seq s) (red f (f i (first s)) (rest s))))))))

(defcheck solution-a78d0020
  (fn t
    ([f c]
     (t f (first c) (rest c)))
    ([f s c]
     (lazy-seq (cons s
                 ((fn m [f r x]
                    (lazy-seq
                      (if (seq x)
                        (let [re (f r (first x))]
                          (cons re (m f re (rest x))))
                        '()))) f s c))))))

(defcheck solution-a7ec77a9
  (fn r
    ([f l]
     (r f (first l) (rest l)))
    ([f i l]
     (cons i
       (lazy-seq
         (if (empty? l)
           nil
           (r f (f i (first l)) (rest l))))))))

(defcheck solution-a82ddbe7
  (fn red
    ([f xs] (red f (first xs) (rest xs)))
    ([f i xs]
     (if (empty? xs)
       [i]
       (cons i (lazy-seq (red f (f i (first xs)) (rest xs))
                 ))))))

(defcheck solution-a899a3a9
  (fn reduction
    ([f col]
     (lazy-seq
       (reduction f (first col) (rest col))))
    ([f init col]
     (lazy-seq
       (if-not (seq col)
         [init]
         (let [rslt (f init (first col))]
           (cons init (reduction f rslt (rest col)))))))))

(defcheck solution-a8e248f7
  (fn
    ([fnc a-seq]
     (letfn [(redu-rec [v s] (if (empty? s) [v] (cons v (lazy-seq (redu-rec (fnc v (first s)) (rest s))))))]
       (redu-rec (first a-seq) (rest a-seq))))
    ([fnc init-arg a-seq]
     (letfn [(redu-rec [v s] (if (empty? s) [v] (cons v (lazy-seq (redu-rec (fnc v (first s)) (rest s))))))]
       (redu-rec init-arg a-seq)))))

(defcheck solution-a8e4c49c
  (fn my-reductions
    ([f sq] (my-reductions f (first sq) (rest sq)))
    ([f init sq]
     (if (empty? sq)
       (list init)
       (cons init (lazy-seq (my-reductions f (f init (first sq)) (rest sq))))
       )
     )
    ))

(defcheck solution-a8eb84fd
  (fn my-reductions
    ([func coll] (my-reductions func (first coll) (rest coll)))
    ([func initial coll]
     (map first
       (take-while #(not (nil? %))
         (iterate
           (fn [pair]
             (when-let [rest-coll (seq (second pair))]
               [(func (first pair) (first rest-coll)) (rest rest-coll)]))
           [initial coll]))))))

(defcheck solution-a8fc0951
  (fn myred
    ([f seq] (myred f (first seq) (rest seq)))
    ([f seed seq]
     (if (empty? seq)
       (cons seed seq)
       (lazy-seq
         (cons
           seed
           (myred f (f seed (first seq)) (rest seq))))))))

(defcheck solution-a937650c
  (fn reds
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reds f (first s) (rest s))
         (list (f coll)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reds f (f init (first s)) (rest s))))))))

(defcheck solution-a9444e98
  (fn red

    ([f [x1 & xs]]

     (red f x1 xs))

    ([f x1 [x2 & xs]]

     (if xs

       (cons x1 (lazy-seq (red f (f x1 x2) xs)))

       (cons x1 [(f x1 x2)])))))

(defcheck solution-a94d0c72
  (fn mid-values
    ([f coll] (mid-values f (first coll) (rest coll)))
    ([f n coll]
     (if (empty? coll)
       [n]
       (cons n (lazy-seq (mid-values f (apply f (conj [n] (first coll))) (rest coll))))))))

(defcheck solution-a9564e30
  (fn my-reductions
    ([op s] (my-reductions op (first s) (rest s)))
    ([op init s]
     (cons init
       (if-not (empty? s)
         (lazy-seq (my-reductions op (op init (first s)) (rest s))))))))

(defcheck solution-a964d9d1
  (fn my_reductions
    ([f a l]
     (if l
       (lazy-seq (cons a
                   (my_reductions f (f a (first l)) (next l))))
       `(~a)))
    ([f l]
     (my_reductions f (first l) (next l)))))

(defcheck solution-a97050f7
  (fn rd
    ([f coll] (rd f (first coll) (rest coll)))
    ([f v coll]
     (cons v (when-let [s (seq coll)]
               (lazy-seq (rd f (f v (first s)) (rest s))))))))

(defcheck solution-a9b55fa1
  (fn reductions2
    ([f coll]
     (when-let [s (seq coll)]
       (reductions2 f (first s) (next s))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reductions2 f (f init (first s)) (next s))))))))

(defcheck solution-a9cd5d16
  (fn my-reduce
    ([f s] (my-reduce f (first s) (rest s)))
    ([f initial-value s]
     (if (not (empty? s))
       (let [current-value (f initial-value (first s))]
         (cons initial-value (lazy-seq (my-reduce f current-value (rest s)))))
       (cons initial-value ())))))

(defcheck solution-aa284eda
  (fn
    ([f x] ((fn red [f col r]
              (if (empty? (rest col))
                (cons r (vector (f r (first col))))
                (cons r (lazy-seq (red f (rest col) (f r (first col)))))))
            f (rest x) (first x)))
    ([f x y] ((fn red [f col r]
                (if (empty? (rest col))
                  (cons r (vector (f r (first col))))
                  (cons r (lazy-seq (red f (rest col) (f r (first col)))))))
              f y x))))

(defcheck solution-aa51c00a
  (fn r
    ([f coll] (r f (f (first coll)) (rest coll)))
    ([f init coll]
     (cons init
       (when-not (empty? coll)
         (lazy-seq (r f (f init (first coll)) (rest coll))))))))

(defcheck solution-aaf9cbd9
  (fn redd
    ([op col]
     (redd op (first col) (rest col)))
    ([op init col]
     (if (empty? col)
       (vector init)
       (let [new (op init (first col))]
         (cons init (lazy-seq (redd op new (rest col)))))))))

(defcheck solution-ab437bb
  (fn reds
    ([f col] (reds f (first col) (rest col)))
    ([f init icol]
     (if (empty? icol) [init]
                       (let [next-val (f init (first icol))]
                         (lazy-seq (cons init (reds f next-val (rest icol)))))))))

(defcheck solution-ab771f69
  (fn reductions--lazy
    ([f init coll]
     (if (seq coll)
       (lazy-seq
         (cons init (reductions--lazy f (f init (first coll)) (rest coll))))
       (list init)))
    ([f coll]
     (if (seq coll)
       (reductions--lazy f (first coll) (rest coll))
       (list (f))))))

(defcheck solution-abc18984
  (fn f
    ([op [a & b]] (f op a b))
    ([op a [b & c]] (cons a (if b (lazy-seq (f op (op a b) c)))))))

(defcheck solution-ac1eab4a
  (fn seq-red
    ([f l]
     (seq-red f (first l) (rest l)))
    ([f a l]
     (lazy-seq
       (if (empty? l)
         (cons a l)
         (cons a (seq-red f (f a (first l)) (rest l))))))))

(defcheck solution-ac7b9531
  (fn myreductions
    ([f x xs]
     (cons x
       (if (empty? xs) nil
                       (lazy-seq
                         (myreductions f (f x (first xs)) (rest xs))))))
    ([f xs] (myreductions f (first xs) (rest xs)))))

(defcheck solution-adbf6303
  apply (fn [f i & s] ((fn g [] (lazy-cat [i] (map f (g) s))))))

(defcheck solution-ade01f0b
  (fn [Op & Rs]
    (letfn [(rdtns [op it s]
              (if (empty? s)
                nil
                (lazy-seq (cons (op it (first s)) (rdtns op (op it (first s)) (rest s))))))]
      (if (= 1 (count Rs))
        (let [S  (first Rs)
              it (first S)
              s  (rest S)]
          (cons it (rdtns Op it s)))
        (let [it (first Rs)
              s  (second Rs)]
          (cons it (rdtns Op it s)))))))

(defcheck solution-aded8bdf
  (fn g
    ([f c]
     (lazy-seq
       (if (seq c)
         (g f (first c) (next c))
         `(~(f)))))
    ([f i c]
     (cons i
       (lazy-seq
         (if (seq c)
           (g f (f i (first c)) (next c))))))))

(defcheck solution-ae811aae
  (let [red (fn red
              ([f coll]
               (let [base (f (first coll) (first (rest coll)))]
                 (lazy-seq (cons base (red f base (drop 2 coll))))))
              ([f x coll]
               (when-not (empty? coll)
                 (let [fx (f x (first coll))]
                   (lazy-seq (cons fx (red f fx (rest coll))))))))]
    (fn
      ([f coll]
       (lazy-seq (cons (first coll) (red f coll))))
      ([f x coll]
       (lazy-seq (cons x (red f x coll)))))))

(defcheck solution-af0c0abf
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll) (list init)
                       (cons init (lazy-seq (my-reductions f (f init (first coll)) (rest coll))))))))

(defcheck solution-af44a34a
  (fn [f & args]
    (
     (fn red [in c]
       (if (empty? c)
         [in]
         (let [v (f in (first c))]
           (lazy-seq (cons in (red v (rest c))))
           )
         )
       )
     (if (= (count args) 2) (first args) (first (first args)))
     (if (= (count args) 2) (second args) (rest (first args)))
     )
    ))

(defcheck solution-af47dee3
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [[x & more] (seq coll)]
           (my-reductions f (f init x) more)))))))

(defcheck solution-af70d76b
  (fn seq-reduce
    ([f v] (seq-reduce f (first v) (rest v)))
    ([f val coll]
     (cons val
       (lazy-seq
         (when-let [s (seq coll)]
           (seq-reduce f (f val (first s)) (rest s))))))))

(defcheck solution-afa0ccdf
  (fn rs ([f coll]
          (if-let [s (seq coll)]
            (rs f (first s) (rest s))
            (list (f))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (rs f (f init (first s)) (rest s))))))))

(defcheck solution-afde747b
  (fn [f h & t] (letfn [(reduct [last-in-acc l] (if-let [r (seq l)] (let [next (f last-in-acc (first l))] (lazy-seq (cons next (reduct next (rest l)))))))]
                  (if (nil? t) (cons (first h) (reduct (first h) (rest h))) (cons h (reduct h (first t)))))))

(defcheck solution-aff49734
  (fn my-reduce
    ([f st coll]
     (cons st
       (lazy-seq
         (if-not (empty? coll)
           (my-reduce f (f st (first coll)) (rest coll))))))
    ([f coll] (my-reduce f (first coll) (rest coll)))))

(defcheck solution-b00d81ba
  (fn
    ([func coll]
     (map (fn [n] (reduce func (take n coll))) (rest (range))))
    ([func first-item coll]
     (let [c (cons first-item coll)]
       (take (count c)
         (map (fn [n] (reduce func (take n c))) (rest (range))))))))

(defcheck solution-b18ea74
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq
       (cons v
         (when-not (empty? coll)
           (my-reductions f
             (f v (first coll))
             (rest coll))))))))

(defcheck solution-b1ebda39
  (fn reductions'
    ([f coll]
     (lazy-seq
       (if-let [[x & more] (seq coll)]
         (reductions' f x more)
         [(f)])))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [[x & more] (seq coll)]
           (reductions' f (f init x) more)))))))

(defcheck solution-b207bfe0
  (fn reductions-e
    [f x & rst]
    (lazy-seq
      (if (nil? rst) (reductions-e f (first x) (rest x))
                     (if (empty? (first rst)) [x]
                                              (cons x
                                                (reductions-e f (f x (first (first rst))) (rest (first rst)))))))))

(defcheck solution-b23aa870
  (fn red
    ([f v [s & r]] (lazy-seq (cons v (when s (red f (f v s) r)))))
    ([f [s & r]] (red f s r))))

(defcheck solution-b2a91684
  (fn my-test

    ([my-fn my-seq]


     (my-test my-fn (first my-seq) (rest my-seq))
     )

    ([my-fn res my-seq]

     (let [new-seq (concat my-seq (list (first my-seq)))

           me      (fn me1 [my-fn res my-seq]

                     (if (empty? my-seq)
                       nil

                       (cons res
                         (lazy-seq (me1
                                     my-fn
                                     (my-fn res (first my-seq))
                                     (rest my-seq)
                                     ))))

                     )

           ]

       (me my-fn res new-seq)

       )

     )
    ))

(defcheck solution-b3daaa05
  (fn scanl
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (scanl f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (scanl f (f init (first s)) (rest s))))))))

(defcheck solution-b43b0070
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if (seq coll)
         (cons init (r f (f init (first coll)) (rest coll)))
         (list init))))))

(defcheck solution-b49a4688
  (fn myreductions
    ([f se] (myreductions f (first se) (rest se)))
    ([f a se] (cons a (lazy-seq (if (first se) (myreductions f (f a (first se)) (rest se))))))))

(defcheck solution-b4f7b0a7
  (fn r
    ([f coll] (r f (first coll) (rest coll)))
    ([f val coll] (map #(reduce f val (take %2 coll)) (cons 0 coll) (range)))))

(defcheck solution-b50bee71
  (fn reductions2
    ([f init-value xs]
     (lazy-seq
       (if (seq xs)
         (let [y (f init-value (first xs))]
           (cons init-value (reductions2 f y (rest xs))))
         [init-value])))
    ([f xs]
     (reductions2 f (first xs) (rest xs)))))

(defcheck solution-b510c3a6
  (fn redseq [op & args]
    (let [arity    (count args)
          startval (if (= arity 1)
                     (first (first args))
                     (first args))
          restargs (if (= arity 1)
                     (drop 1 (first args))
                     (second args))]
      (letfn [(lazyfn [val lis]
                (if (empty? lis)
                  (list val)
                  (lazy-seq (cons val
                              (lazyfn (op val (first lis))
                                (rest lis))))))]
        (lazyfn startval restargs)))))

(defcheck solution-b53f1c88
  (fn res
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (res f (first s) (rest s)) (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)] (res f (f init (first s)) (rest s))))))))

(defcheck solution-b560c9e
  (fn red
    ([f val coll]
     (cons val
       (if (empty? coll) [] (lazy-seq (red f (f val (first coll)) (rest coll))))))
    ([f coll]
     (red f (first coll) (rest coll)))))

(defcheck solution-b574696b
  (fn reductions'
    ([f coll] (reductions' f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       (cons init (empty coll))
       (cons init (lazy-seq (reductions' f (f init (first coll)) (rest coll))))))))

(defcheck solution-b5930f86
  (fn [f x & y]
    (lazy-seq
      (let [isynil? (nil? y)
            a       (if isynil? (first x) x)
            b       (if isynil? (rest x) (first y))
            func    (fn red [f1 x1 y1]
                      (lazy-seq
                        (cons x1
                          (if (not (empty? y1))
                            (red f
                              (f x1 (first y1))
                              (rest y1))))))]
        (func f a b)))))

(defcheck solution-b59eaa0c
  (fn r
    ([f [x & xs]] (r f x xs))
    ([f x xs]
     (lazy-seq
       (if (empty? xs)
         [x]
         (let [v (f x (first xs))]
           (concat [x] (r f v (rest xs)))
           )
         )
       )
     )
    ))

(defcheck solution-b5a974a3
  (fn my-reduction
    ([f coll] (my-reduction f (first coll) (rest coll)))
    ([f val coll]
     (lazy-seq
       (let [s (seq coll)]
         (if (empty? coll)
           (cons val '())
           (cons val (my-reduction f (f val (first coll)) (rest coll)))))))))

(defcheck solution-b5f3bf8a
  (fn my-reduce
    ([f xs] (my-reduce f (first xs) (rest xs)))
    ([f a xs]
     (lazy-seq
       (if (empty? xs) (list a)
                       (cons a (my-reduce f (f a (first xs)) (rest xs))))))))

(defcheck solution-b63fccb
  (fn my-reductions ([f ini coll]
                     (lazy-seq (cons ini (if (seq coll)
                                           (my-reductions f (f ini (first coll)) (rest coll))))))
    ([f coll]
     (my-reductions f (first coll) (rest coll)))))

(defcheck solution-b65dc564
  (fn g
    ([f xs] (g f (first xs) (rest xs)))
    ([f init [x & xs :as xxs]]
     (if (empty? xxs)
       (list init)
       (lazy-seq (cons init (g f (f init x) xs)))))))

(defcheck solution-b66a9107
  (fn r
    ([f c] (r f (first c) (next c)))
    ([f s c]
     (lazy-seq
       (if (empty? c)
         [s]
         (cons s (r f (f s (first c)) (next c))))))))

(defcheck solution-b778423
  (fn seduce
    ([f [h & t]]
     (if t
       (seduce f h t)
       [h]))

    ([f val [h & t :as xs]]
     (cons val
       (if xs
         (lazy-seq
           (seduce f (f val h) t)))))))

(defcheck solution-b7a2a46f
  (fn reductions'
    ([op xs] (reductions' op (first xs) (rest xs)))
    ([op init xs]
     (cons init
       (lazy-seq
         (when-let
          [s (seq xs)]
           (reductions' op (op init (first s)) (rest s))))))))

(defcheck solution-b8a648f7
  (fn reduce-seq
    ([f xs] (reduce-seq f (first xs) (rest xs)))
    ([f x xs] (lazy-seq
                (cons x (if (empty? xs) nil
                                        (reduce-seq f (f x (first xs)) (rest xs))))))))

(defcheck solution-b8a95043
  (fn reductions*
    ([func coll]
     (reductions* func (first coll) (rest coll)))
    ([func init coll]
     (lazy-seq (cons init (when (seq coll)
                            (reductions* func
                              (func init (first coll))
                              (rest coll))))))))

(defcheck solution-b8ac9874
  (fn g
    ([f [hed & rst]] (g f hed rst))
    ([f red [hed & rst]]
     (cons red
       (when hed
         (lazy-seq (g f (f red hed) rst)))))))

(defcheck solution-b8d8c510
  (fn [f x1 & xs]
    (if (empty? xs)
      ((fn F1 [f y1 ys]
         (lazy-seq
           (concat (list y1) (if (not (empty? ys))
                               (F1 f (f y1 (first ys)) (rest ys)) nil))
           )
         )
       f (first x1) (rest x1))
      ((fn F1 [f y1 ys]
         (lazy-seq
           (concat (list y1) (if (not (empty? ys))
                               (F1 f (f y1 (first ys)) (rest ys)) nil))
           )
         )
       f x1 (first xs))

      ; (if (empty? xs) (FF f (first x1) (rest x1)) (FF f x1 (first xs)))
      )))

(defcheck solution-b952c999
  (fn rd
    ([a b] (rd a (first b) (rest b)))
    ([a b c]
     (lazy-seq
       (if (empty? c) [b]
                      (cons b (rd a (a b (first c)) (rest c))))))))

(defcheck solution-b98cdab1
  (fn myred
    ([f coll]
     (when (not (empty? coll))
       (myred f (first coll) (rest coll))))
    ([f val coll]
     (if (not (empty? coll))
       (cons val (lazy-seq (myred f (f val (first coll)) (rest coll))))
       (list val)))))

(defcheck solution-b9fd74ad
  (fn red
    ([f coll] (if-let [s (seq coll)]
                (red f (first s) (next s))
                (list (f))))
    ([f v coll]
     (cons v (lazy-seq (when-let [s (seq coll)] (red f (f v (first s)) (next s))))))))

(defcheck solution-ba1d4067
  (fn red
    ([f s] (red f (first s) (rest s)))
    ([f i s]
     (if (empty? s)
       [i]
       (lazy-seq (cons i (red f (f i (first s)) (rest s))))))))

(defcheck solution-ba8d8c16
  (letfn [(ir [f v c]
            (cond (nil? (first c)) '()
                  :else (let [nv (f v (first c))]
                          (cons nv (lazy-seq (ir f nv (rest c)))))))
          (red
            ([f coll]
             (red f (first coll) (rest coll)))
            ([f val coll]
             (cons val (ir f val coll))))]
    red))

(defcheck solution-ba8dd38
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f i s]
     (cons i (lazy-seq
               (when-let [s (seq s)]
                 (r f (f i (first s)) (rest s))))))))

(defcheck solution-ba99b80d
  (fn redu
    ([f lat] (redu f (first lat) (rest lat)))
    ([f cv lat]
     (cond
       (empty? lat) [cv]
       :else
       (let [nv (f cv (first lat))]
         (cons cv
           (lazy-seq (redu f nv (rest lat)))))))))

(defcheck solution-baaf1457
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f i s]
     (cons
       i
       (if (empty? s) '() (lazy-seq (r f (f i (first s)) (rest s))))))))

(defcheck solution-bae1ac12
  (fn r [f memo & args]
    (let [start (if args memo (first memo))
          s     (if args (first args) (rest memo))
          ]
      (cons start ((fn red [s]
                     (if (nil? (second s))
                       (drop-last s)
                       (let [curr (f (first s) (second s))]
                         (lazy-seq (cons curr (red (cons curr (rest (rest s))))))
                         )
                       )
                     ) (cons start s)))
      )
    ))

(defcheck solution-bbeb3c0c
  (fn reduc
    ([f d]
     (reduc f (first d) (rest d)))
    ([f acc d]
     (if (empty? d)
       (list acc)
       (lazy-seq
         (cons acc (reduc f (f acc (first d)) (rest d))))))))

(defcheck solution-bc3d05c6
  (fn _reductions
    ([f s]
     (lazy-seq
       (_reductions
         f
         (first s)
         (rest s))))
    ([f val s]
     (cons
       val
       (lazy-seq
         (when (seq s)
           (_reductions
             f
             (f val (first s))
             (rest s))))))
    ))

(defcheck solution-bcbbfb53
  (fn [a b & [c]]
    (let [r (if (nil? c) b (cons b c))]
      (map
        #(reduce a (take % r))
        (map #(+ 1 %2) r (range))))))

(defcheck solution-bd1269d
  (fn rdxns
    ([f [x & xs]] (rdxns f x xs))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [xs (seq coll)]
           (rdxns f (f init (first xs)) (rest xs))))))))

(defcheck solution-bd3b5d59
  (fn myreduce
    ([f coll] (myreduce f (first coll) (rest coll)))
    ([f init coll]
     (if (nil? (first coll))
       (lazy-cat (vector init))
       (when-let [s (seq coll)]
         (lazy-cat (vector init) (myreduce f (f init (first s)) (rest s)))
         )))))

(defcheck solution-bd481db7
  (fn sr
    ([f col] (sr f (first col) (rest col)))
    ([f init col]
     (cons init (lazy-seq (when-let [s (seq col)]
                            (sr f (f init (first s)) (rest s))))))))

(defcheck solution-be4f3e8c
  (fn red2
    ([f coll] (red2 f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       (vector init)
       (cons init (lazy-seq (red2 f (f init (first coll)) (rest coll)))))
     )))

(defcheck solution-be7c93
  (letfn [(myreductions
            ([f coll]
             (when (seq coll) (myreductions f (first coll) (rest coll))))
            ([f zero coll]
             (lazy-seq
               (if (seq coll)
                 (cons zero (myreductions f (f zero (first coll)) (rest coll)))
                 (list zero)
                 ))))]

    myreductions
    ))

(defcheck solution-bee8c792
  (fn reducto
    ([f input] (reducto f (first input) (rest input)))

    ([f st input]
     (lazy-seq
       (if (empty? input)
         (list st)
         (cons st (reducto f (f st (first input)) (rest input)))
         )))))

(defcheck solution-beea5104
  (fn g
    ([f c]
     (g f (first c) (rest c)))
    ([f i c]
     (if
      (empty? c)
       [i]
       (lazy-seq (cons i (g f (f i (first c)) (rest c))))))))

(defcheck solution-bf1cf80a
  (fn red
    ([x y] (red x (first y) (rest y)))
    ([x y z] (if (empty? z) (list y) (lazy-seq (cons y (red x (x y (first z)) (rest z))))))))

(defcheck solution-bf500a46
  (fn thisfunc
    ([f s] (thisfunc f (first s) (rest s)))
    ([f init s]
     (cons init
       (if (empty? s)
         nil
         (lazy-seq (thisfunc f (f init (first s)) (rest s))))))))

(defcheck solution-bf8f2c1e
  (fn red
    ([f s] (red f (first s) (rest s)))
    ([f a b]
     (lazy-seq
       (if (empty? b)
         (list a)
         (cons a (red f (f a (first b)) (next b))))))))

(defcheck solution-bfa1d5f8
  (fn r
    ([f v] (r f (first v) (rest v)))
    ([f a v]
     (cons a
       (lazy-seq
         (when-let [[x & s] (seq v)]
           (r f (f a x) s)))))))

(defcheck solution-bfaf4cdb
  (fn my-reduce
    ([op init s]
     (if (empty? s)
       (cons init nil)
       (cons init
         (lazy-seq (my-reduce op (op init (first s)) (rest s))))))
    ([op s]
     (my-reduce op (first s) (rest s)))))

(defcheck solution-c01a00ec
  (fn r
    ([f x]
     (if (empty? (rest x))
       (list (first x))
       (lazy-seq
         (cons (first x) (r f (cons (f (first x) (second x)) (drop 2 x)))))))
    ([f s x]
     (if (empty? x)
       (list s)
       (lazy-seq
         (cons s (r f (cons (f s (first x)) (drop 1 x)))))))
    ))

(defcheck solution-c09b4808
  (fn r
    ([f s] (if (empty? s)
             nil
             (r f (first s) (rest s))))
    ([f v s]
     (cons v (lazy-seq (if (empty? s)
                         '()
                         (r f (f v (first s)) (rest s))))))))

(defcheck solution-c1653f74
  (fn scan
    ([f coll]
     (when-let [s (seq coll)]
       (scan f (first s) (rest s))))
    ([f init coll]
     (cons init (lazy-seq
                  (when-let [s (seq coll)]
                    (scan f (f init (first s)) (rest s))))))))

(defcheck solution-c19f593c
  (fn R ([f a [x & r]]
         (cons a
           (if x
             (lazy-seq (R f (f a x) r))
             [])))
    ([f [x & r]]
     (R f x r))))

(defcheck solution-c1c20f8c
  apply (fn [f i & xs] ((fn ff [] (lazy-cat [i] (map f (ff) xs))))))

(defcheck solution-c1f6bb90
  (fn my-reductions
    ([f xs] (if (empty? xs) xs (my-reductions f (first xs) (rest xs))))
    ([f i xs]
     (cons i (if (empty? xs) xs (lazy-seq (my-reductions f (f i (first xs)) (rest xs))))))))

(defcheck solution-c2583601
  (fn reds
    ([f coll] (reds f (first coll) (rest coll)))
    ([f acc coll]
     (lazy-seq (if (empty? coll)
                 (list acc)
                 (cons acc (reds f (f acc (first coll)) (rest coll))))))))

(defcheck solution-c2b9ad30
  (fn red
    ([f [v & vs]] (red f v vs))
    ([f v [u & us]]
     (cons v (when u (lazy-seq (red f (f v u) us)))))))

(defcheck solution-c2f1f96a
  (fn my-reduce

    ([op input] (my-reduce op (first input) (rest input)))

    ([op result input]

     (lazy-seq
       (if (empty? input) (list result)
                          (cons result
                            (my-reduce op
                              (op result (first input))
                              (rest input))))))))

(defcheck solution-c37699a7
  (fn g ([f val xs] (let [step (fn [f v c]
                                 (let [s (seq c)]
                                   (cond (empty? s) (list v) true
                                         (cons v (g f (f v (first s)) (rest s))))))]
                      (lazy-seq (step f val xs))))
    ([f xs] (g f (first xs) (rest xs)))))

(defcheck solution-c37b08c3
  (fn my-red
    ([f xs]
     (my-red f (first xs) (rest xs)))
    ([f x1 xs]
     (if (nil? (first xs))
       (list x1)
       (lazy-seq
         (cons x1 (my-red f (f x1 (first xs)) (rest xs))))))))

(defcheck solution-c4931fd8
  (fn rdctns [f & args]
    (let [initial-val? (= (count args) 2)
          x1           (if initial-val?
                         (first args)
                         (ffirst args))
          xs           (if initial-val?
                         (second args)
                         (rest (first args)))]
      ((fn helper [y ys]
         (if (seq ys)
           (lazy-seq
             (cons y (helper (f y (first ys))
                       (rest ys))))
           (list y)))
       x1 xs))))

(defcheck solution-c4ef7eac
  (fn [f arg & args]
    (let [i (if (nil? args) (first arg) arg)
          s (seq (if (nil? args) arg (apply vec args)))]
      (cons i ((fn my-redu [le s]
                 (if (nil? s)
                   nil
                   (let [ce (f le (first s))]
                     (lazy-seq (cons ce
                                 (my-redu ce (next s)))))))
               i (if (nil? args) (next s) s))))))

(defcheck solution-c53a0919
  (fn new-reductions
    ([fun alist]
     (new-reductions fun (first alist) (rest alist)))
    ([fun init alist]
     (let [a  (first alist)
           bs (rest alist)]
       (cons init
         (lazy-seq
           (if (empty? alist) nil
                              (new-reductions fun (fun init a) bs))))))))

(defcheck solution-c5c3779b
  (fn myr
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (myr f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (myr f (f init (first s)) (rest s))))))))

(defcheck solution-c5ffd73
  (fn r
    ([f xs] (r f (first xs) (rest xs)))
    ([f v xs]
     (lazy-seq
       (cons v
         (if (empty? xs)
           []
           (r f (f v (first xs)) (rest xs))))))))

(defcheck solution-c6aac799
  (fn rdns
    ([f init s]
     (lazy-seq (cons
                 init
                 (when-let [[x & xs] (seq s)] (rdns f (f init x) xs)))))
    ([f s] (if-let [[x & xs] (seq s)] (rdns f x xs) (list (f))))))

(defcheck solution-c746d3a2
  (fn reduc
    ([f init b]
     (lazy-seq
       (cons init
         (if (empty? b)
           nil
           (reduc f (f init (first b)) (rest b))))))
    ([f b] (reduc f (first b) (rest b)))))

(defcheck solution-c88a3d06
  (fn r
    ([f [h & t]]
     (r f h t))
    ([f i [h & t :as c]]
     (cons i
       (if (not-empty c)
         (lazy-seq (r f (f i h) t)))))))

(defcheck solution-c8d976c7
  (fn my_redu
    ([f [init & coll]]
     (my_redu f init coll))
    ([f init coll]
     (if (empty? coll)
       (vector init)
       (cons init (lazy-seq (my_redu f (f init (first coll)) (rest coll))))))))

(defcheck solution-c8eec656
  (fn reductions'
    ([f init args]
     (if-not (seq args)
       [init]
       (lazy-seq (cons init (reductions' f (f init (first args)) (rest args))))))
    ([f args] (reductions' f (first args) (rest args)))))

(defcheck solution-c91951c4
  (fn [f oh & [ot]]
    (letfn
     [(reds [f h [ft & t]]
        (let [x (f h ft)]
          (if t
            (lazy-seq (cons x (reds f x t)))
            [x])))]
      (if ot
        (cons oh (reds f oh ot))
        (cons (first oh) (reds f (first oh) (drop 1 oh)))))))

(defcheck solution-c950e055
  (fn p60
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (p60 f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (p60 f (f init (first s)) (rest s))))))))

(defcheck solution-c98b922d
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-c9a1f778
  (fn myreductions
    ([f coll] (myreductions f (first coll) (rest coll)))
    ([f val coll]
     (if (seq coll)
       (cons val (lazy-seq (myreductions f (f val (first coll)) (rest coll))))
       [val]
       )
     )
    ))

(defcheck solution-c9a6ba1d
  (fn x
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (x f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (x f (f init (first s)) (rest s))))))))

(defcheck solution-ca2724dc
  (fn r
    ([f seed s]
     (lazy-seq
       (if (seq s)
         (cons seed (r f (f seed (first s)) (rest s)))
         (list seed))))
    ([f s]
     (lazy-seq (r f (first s) (rest s))))))

(defcheck solution-caa9dde8
  (fn red
    ([func s] (red func (first s) (rest s)))
    ([func initial s]
     (if (not= s [])
       (let [ret (func initial (first s))]
         (cons initial (lazy-seq (red func ret (rest s)))))
       [initial]))))

(defcheck solution-cb1dda3d
  (fn reducts
    ([f lst] (for [[i _] (map-indexed vector lst)] (reduce f (take (inc i) lst))))
    ([f v lst] (reducts f (cons v lst)))))

(defcheck solution-cbb58a7b
  (fn z
    ([f coll] (z f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (z f (f init (first coll)) (rest coll))))))))

(defcheck solution-cbd88487
  #(letfn [(rdct [fun init [f & r :as lst]]
             (lazy-seq
               (if (empty? lst)
                 [init]
                 (let [res (fun init f)]
                   (cons init (rdct fun res r))))))]
     (if (= 3 (count %&))
       (apply rdct %&)
       (let [[fun elts] %&]
         (rdct fun (first elts) (rest elts))))))

(defcheck solution-cbed40d8
  (fn redu ([f coll]
            (lazy-seq
              (if-let [s (seq coll)]
                (redu f (first s) (rest s))
                (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (redu f (f init (first s)) (rest s))))))))

(defcheck solution-cc09bf4f
  (fn [f & xs]
    (let
     [r (fn rd [s xs]
          (lazy-seq
            (if (empty? xs)
              (cons s '())
              (cons s (rd (f s (first xs)) (rest xs))))))
      i (first xs)]
      (if (= (count xs) 1)
        (r (first i) (rest i))
        (r i (second xs))))))

(defcheck solution-cc17b895
  (letfn
   [(redux
      ([f c] (rest (redux f (f) c)))
      ([f i c]
       (if (seq c)
         (lazy-seq (let [x (f i (first c))]
                     (cons i (redux f x (rest c)))))
         (list i))))]
    redux))

(defcheck solution-cca0d311
  (fn p60
    ([f xs] (p60 f (first xs) (rest xs)))                   ;no start-value? take first and call function again
    ([f v xs]
     (lazy-seq
       (cons v                                              ;add new value
         (if (empty? xs)                                    ;at end of sequence return []
           []
           (p60 f (f v (first xs)) (rest xs))))))))

(defcheck solution-ccbdfbe6
  (fn red [f & [a & b]]
    (let [acc (if (nil? b) (first a) a)
          lst (if (nil? b) (rest a) (first b))]
      (lazy-seq
        (if (not-empty lst)
          (cons acc (red f (cons (f acc (first lst)) (rest lst))))
          [acc])))))

(defcheck solution-cce256de
  (letfn
   [(red-aux [f [a b & coll]]
      (let [v (f a b)]
        (if (empty? coll) (list v)
                          (cons v (lazy-seq
                                    (red-aux f (cons v coll)))))))]

    (fn red
      ([f coll] (cons (first coll) (red-aux f coll)))
      ([f item coll] (red f (cons item coll))))))

(defcheck solution-cd1480ec
  (fn r
    ([f coll]
     (r f (f (first coll)) (rest coll)))
    ([f b coll]
     (lazy-seq
       (cons b
         (when-let [s (seq coll)]
           (r f (f b (first s)) (rest s))))))))

(defcheck solution-cd5cbae9
  (fn my-reductions
    ([f c]
     (my-reductions f (first c) (drop 1 c)))
    ([f i c]
     (lazy-seq
       (cons
         i
         (if (= c '())
           nil
           (my-reductions f (f i (first c)) (rest c))))))))

(defcheck solution-cd8cd260
  (fn pared
    ([op coll]
     (pared op (first coll) (rest coll)))
    ([op init coll]
     (cons init
       (lazy-seq
         (if (seq coll)
           (pared op (op init (first coll)) (rest coll))))))))

(defcheck solution-cd9a0466
  (fn [f & args]
    (letfn [(nxt [acc s]
              (if (empty? s)
                [acc]
                (let [new-acc (f acc (first s))
                      rs      (rest s)]
                  (cons acc (lazy-seq (nxt new-acc rs))))))
            ]
      (if (= 1 (count args))
        (nxt (first (first args)) (rest (first args)))
        (nxt (first args) (second args))))
    ))

(defcheck solution-cdb5ed23
  (fn red
    ([f coll]
     (red f (first coll) (rest coll)))
    ([f current coll]
     (cons
       current
       (if (empty? coll)
         []
         (lazy-seq (red f (f current (first coll)) (rest coll))))))))

(defcheck solution-cdb67927
  (fn r
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (r f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (r f (f init (first s)) (rest s))))))))

(defcheck solution-ce304ace
  (fn m-r
    ([f coll]
     (let [fst (first coll)]
       (m-r f (f fst fst) (rest coll))))
    ([f acc coll]
     (lazy-seq
       (if (empty? coll)
         (cons acc [])
         (cons acc (m-r f (f acc (first coll)) (rest coll))))))))

(defcheck solution-ceb19ae1
  (fn [f & xs]
    ((fn rec [sq]
       (let [a (first sq) b (second sq)]
         (if (nil? b) (list a)
                      (let [v  (f a b)
                            sn (cons v (drop 2 sq))]
                        (lazy-seq
                          (cons a (rec sn)))))))
     (if (= 1 (count xs)) (first xs)
                          (cons (first xs) (second xs))))))

(defcheck solution-ceb3d10e
  (fn my-reduce
    ([f coll] (my-reduce f (first coll) (rest coll)))
    ([f acc coll]
     (lazy-seq
       (if (empty? coll)
         (list acc)
         (cons acc
           (my-reduce f
             (f acc (first coll))
             (rest coll))))))))

(defcheck solution-cee2e7ae
  (fn fx
    ([f a s]
     (if (seq s)
       (cons a (lazy-seq (fx f (f a (first s)) (rest s))))
       [a]))
    ([f s]
     (fx f (first s) (rest s)))))

(defcheck solution-cf10e7ca
  (fn ff
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (ff f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (ff f (f init (first s)) (rest s))))))

    ))

(defcheck solution-cfdf6e88
  (fn [f y & z]
    (let [start (if (empty? z) (first y) y) res (if (empty? z) (rest y) (first z))]
      (cons start ((fn* reducer [a b]
                     (if (empty? b)
                       ()
                       (let [fa (f a (first b))]
                         (cons fa (lazy-seq (reducer fa (rest b))))))) start res)))))

(defcheck solution-d0e7d5d3
  (fn rd
    ([f c] (rd f (first c) (rest c)))
    ([f x c]
     (cons x (if (empty? c) '() (lazy-seq (rd f (f x (first c)) (rest c))))))))

(defcheck solution-d0fc80e4
  (fn reducs
    ([f coll] (reducs f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll) (cons init nil)
                       (let [res (f init (first coll))]
                         (cons init (lazy-seq (reducs f res (rest coll)))))))))

(defcheck solution-d13a034e
  (fn red ([f coll]
           (red f (first coll) (next coll)))
    ([f start coll]
     (if (seq coll)
       (cons start (lazy-cat (red f (f start (first coll)) (next coll))))
       [start]))))

(defcheck solution-d16a0068
  (fn my-reduce
    ([f s]
     (my-reduce f (first s) (rest s)))
    ([f acc s]
     (lazy-seq (cons acc
                 (if (seq s)
                   (my-reduce f (f acc (first s)) (rest s))))))))

(defcheck solution-d1c341ac
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll)
       [init]
       (let [x (f init (first coll))]
         (lazy-seq
           (cons init (my-reductions f x (rest coll)))))))))

(defcheck solution-d1c80ebe
  (fn __
    ([f xs] (__ f (first xs) (rest xs)))
    ([f init xs]
     (if-not (empty? xs)
       (lazy-seq (cons init (__ f (f init (first xs)) (rest xs))))
       (lazy-seq [init])))))

(defcheck solution-d1dd6947
  (fn red
    ([f [car & cdr]] (red f car cdr))
    ([f init [car & cdr]]
     (lazy-seq (cons
                 init
                 (if car (red f (f init car) cdr)))))))

(defcheck solution-d1e87535
  (fn reducto
    ([f vals]
     (reducto f (first vals) (rest vals)))
    ([f init vals]
     (cons init
       ((fn inner
          [acc src]
          (when (seq src)
            (let [[head tail] ((juxt first rest) src)
                  result (f acc head)]
              (cons result
                (lazy-seq (inner result tail))))))
        init vals)))))

(defcheck solution-d21e2e40
  (fn red ([f [h & t]] (red f h t))
    ([f v [h & t]] (cons v (if (not (nil? h)) (lazy-seq (red f (f v h) t)))))))

(defcheck solution-d2320c8e
  (fn red
    ([f coll]
     (red f 0 coll []))
    ([f seed coll]
     (red f seed coll [seed]))
    ([f seed coll acc]
     (let [
           newvalue      (f seed (first coll))
           accvalue      (cons newvalue acc)
           newcollection (drop 1 coll)
           ]
       (if (or (empty? newcollection)
               (= 5 (count accvalue))                       ;ugly hack we meet again
               )
         (reverse accvalue)
         (recur f newvalue newcollection accvalue)
         )
       )
     )
    ))

(defcheck solution-d272b5e3
  (fn self
    ([f init col] (self f (cons init col)))
    ([f col]
     (lazy-cat [(first col)] (map f (self f col) (rest col))))))

(defcheck solution-d2fb92a0
  (fn reduct
    ([f initial coll] (if-not (empty? coll)
                        (lazy-seq
                          (let [cur (f initial (first coll))]
                            (cons initial (reduct f cur (next coll)))))
                        [initial]))
    ([f coll] (if-not (empty? coll)
                (reduct f (first coll) (next coll))
                []))))

(defcheck solution-d3033dc5
  (fn folds
    ([f [x & xs]] (folds f x xs))
    ([f acc [x & xs]]
     (lazy-seq
       (cons acc (when x (folds f (f acc x) xs)))))))

(defcheck solution-d30b6ae8
  (fn r
    ([f [a & s]] (r f a s))
    ([f a s]
     (cons a (map #(apply f a (take (+ 1 %2) s)) s (range))))))

(defcheck solution-d388532d
  (letfn [(reddy
            ([f coll]
             (lazy-seq
               (if-let [s (seq coll)]
                 (reddy f (first s) (rest s))
                 (list (f)))))
            ([f init coll]
             (cons init
               (lazy-seq
                 (when-let [s (seq coll)]
                   (reddy f (f init (first s)) (rest s)))))))]
    (fn ([g c] (reddy g c))
      ([g i c] (reddy g i c)))))

(defcheck solution-d3b06c17
  (fn r
    ([g fst s] (if (empty? s) [fst] (cons fst (lazy-seq (r g (g fst (first s)) (rest s))))))
    ([g s] (if (empty? s) (g) (r g (first s) (rest s))))))

(defcheck solution-d3b1b180
  (fn reps
    ([func coll]
     (reps func (first coll) (rest coll)))

    ([func init coll]
     (if (empty? coll)
       (list init)
       (let [value (func init (first coll))]
         (cons init (lazy-seq (reps func value (rest coll))))
         )
       )
     )))

(defcheck solution-d3b2e592
  (fn reductions'
    ([f coll] (reductions' f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reductions' f (f init (first s)) (rest s))))))))

(defcheck solution-d3bd5e64
  (fn sred
    ([f acc s]
     (if (seq s)
       (lazy-seq
         (cons acc (sred f (f acc (first s)) (rest s))))
       (list acc)))
    ([f s] (sred f (first s) (rest s)))))

(defcheck solution-d3e423b0
  (fn [f & args-coll]
    (letfn [(reductions- [args rst]
              (if (empty? rst)
                ()
                (let [current-args (conj args (first rst))]
                  (lazy-seq (cons (apply f current-args)
                              (reductions- current-args (rest rst)))))))]
      (let [initial-args   (butlast args-coll)
            reductions-seq (reductions- (vec initial-args) (last args-coll))]
        (if initial-args
          (concat initial-args reductions-seq)
          reductions-seq)))))

(defcheck solution-d419e818
  (fn f
    ([g vs]
     (f g (first vs) (rest vs)))
    ([g ini vs]
     (let [foo (fn [[x xs]]
                 (if (seq xs)
                   [(g x (first xs))
                    (rest xs)]
                   []))]
       (->> [ini vs]
         (iterate foo)
         (take-while seq)
         (map first))))))

(defcheck solution-d448ffd5
  (letfn
   [(red
      ([f carry l]
       (if (= '() l)
         (list carry)
         (lazy-seq
           (let [v (f carry (first l))]
             (cons carry (red f v (rest l)))))))
      ([f l]
       (if (= '() l)
         '()
         (red f (first l) (rest l)))))]
    red))

(defcheck solution-d44a596d
  (fn reductions-clone
    ([f coll] (reductions-clone f (first coll) (rest coll)))
    ([f default coll]
     (if (empty? coll)
       [default]
       (lazy-seq (cons default (reductions-clone f (f default (first coll)) (rest coll))))))))

(defcheck solution-d482e12
  (fn seq-reductions
    ([f xs] (seq-reductions f (first xs) (rest xs)))
    ([f a xs]
     (lazy-seq
       (let [[b & others] xs
             comb (f a b)]
         (if (empty? others)
           (cons a [comb])
           (cons a (seq-reductions f comb others))))))))

(defcheck solution-d4d7e177
  (fn r
    ([fun coll] (r fun (first coll) (rest coll)))
    ([fun i coll]
     (if (empty? coll)
       [i]
       (cons i
         (lazy-seq
           (r fun
             (fun i (first coll))
             (rest coll))))))))

(defcheck solution-d4df4b8
  (fn r
    ([f [x & xs]] (r f x xs))
    ([f x [xs & xss]]
     (cons x (lazy-seq (if xs (r f (f x xs) xss)))))))

(defcheck solution-d4ee93b6
  (fn myred
    ([f c]
     (lazy-seq (myred f (first c) (rest c))))
    ([f v c] (cons v (if (not (empty? c))
                       (lazy-seq (myred f (f v (first c)) (rest c))))))))

(defcheck solution-d4f65462
  (fn testreduce ([op s] (testreduce op (first s) (next s))) ([op v s] (if s (cons v (lazy-seq (testreduce op (op v (first s)) (next s)))) (cons v s)))))

(defcheck solution-d542f08d
  (fn red
    ([f coll]
     (red f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq (cons v (when (seq coll)
                         (red f (f v (first coll)) (rest coll))))))))

(defcheck solution-d584606e
  (fn g
    ([f c]
     (g f (f (first c)) (rest c)))
    ([f i c]
     (let [h (fn h [f i c]
               (if (not (empty? c))
                 (let [r (f i (first c))]
                   (lazy-seq (cons r (h f r (rest c)))))))]
       (cons i (h f i c))))))

(defcheck solution-d595d3ba
  (fn seqred3
    ([fun seed a]
     (if a
       (cons seed (lazy-seq (seqred3 fun (fun seed (first a)) (next a))))
       [seed]))
    ([fun a]
     (seqred3 fun (first a) (rest a)))))

(defcheck solution-d5a240a5
  (fn redd
    ([f [x y & xs]] (cons x (redd f (f x y) xs)))
    ([f r [x & xs]]
     (cons r (lazy-seq (if xs
                         (redd f (f r x) xs)
                         (cons (f r x) nil)))))))

(defcheck solution-d5bcb7bc
  (fn myred
    ([f coll]
     (myred f (first coll) (rest coll)))
    ([f arg coll]
     (lazy-seq
       (cons arg
         (if (empty? coll) []
                           (myred f (f arg (first coll)) (rest coll))))))))

(defcheck solution-d5ca06e2
  (fn my-reductions
    ([f coll]
     (my-reductions f (first coll) (rest coll)))
    ([f init coll]
     (cons init
       (lazy-seq (when-let [s (seq coll)]
                   (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-d5e18a
  (fn r
    ([f v c]
     (if (empty? c)
       [v]
       (let [nv (f v (first c))]
         (lazy-seq (cons v (r f nv (rest c)))))))
    ([f c] (r f (first c) (rest c)))))

(defcheck solution-d5f90215
  (fn red
    ([f l] (red f (first l) (rest l)))
    ([f s l]
     (lazy-seq
       (if (empty? l)
         [s]
         (cons s (red f (f s (first l)) (rest l))))))))

(defcheck solution-d60581d1
  (fn reds
    ([f xs]
     (reds f (first xs) (rest xs)))
    ([f init xs]
     (cons init (when (seq xs) (lazy-seq (reds f (f init (first xs)) (rest xs))))))))

(defcheck solution-d691424e
  (fn red
    ([f c] (red f (first c) (next c)))
    ([f s c]
     (if
      (nil? c)
       (cons s nil)
       (cons
         s
         (lazy-seq
           (red f (f s (first c)) (next c))))))))

(defcheck solution-d6aeb34d
  (fn this
    ([f coll]
     (when-first [fst coll]
       (this f fst (rest coll))))
    ([f init coll]
     (cons init
       ((fn x [prev coll]
          (lazy-seq
            (when-first [y coll]
              (let [v (f prev y)]
                (cons v (x v (rest coll)))))))
        init coll)))))

(defcheck solution-d6b9f24d
  (letfn [
          (reducks
            ([f a xs]
             (lazy-seq
               (if (empty? xs)
                 (cons a '())
                 (cons a
                   (reducks f (f a (first xs)) (rest xs))))))
            ([f ys]
             (if (empty? ys) '()
                             (reducks f (first ys) (rest ys)))))
          ] reducks))

(defcheck solution-d6c827af
  (fn myred
    ([fcn aseq] (myred fcn (first aseq) (rest aseq)))
    ([fcn accum aseq]
     (if (empty? aseq)
       (vector accum)
       (lazy-seq
         (cons
           accum
           (myred fcn (fcn accum (first aseq)) (rest aseq))))
       ))))

(defcheck solution-d6d07dba
  (fn seqf
    ([f i s] (cons i (if (empty? s) nil (lazy-seq (seqf f (f i (first s)) (rest s))))))
    ([f i] (seqf f (first i) (rest i)))
    ))

(defcheck solution-d7450c19
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f v s]
     (for [x (if (counted? s) (range 0 (inc (count s))) (range))]
       (reduce f v (take x s))))))

(defcheck solution-d7e06496
  (fn reductions1
    ([f [a b & more]] (cons a (lazy-seq (reductions1 f (f a b) more))))
    ([f init coll]
     (cons init (lazy-seq (when-let [[b & more] coll] (reductions1 f (f init b) more)))))))

(defcheck solution-d7f5e9de
  (fn r
    ([f c]
     (lazy-seq
       (r f (first c) (rest c))))
    ([f i c]
     (cons i
       (lazy-seq
         (when (seq c)
           (r f (f i (first c)) (rest c))))))))

(defcheck solution-d813da81
  (fn r
    ([f [h & t]] (r f h t))
    ([f a [h & t]] (cons a (lazy-seq (if h (r f (f a h) t) nil))))))

(defcheck solution-d85b7953
  (fn reductions-
    ^{:doc "60. Write a function which behaves like reduce, but returns each intermediate
  value of the reduction."}
    ([f coll]
     (reductions- f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons init (reductions- f (f init (first s)) (rest s)))
         (list init))))))

(defcheck solution-d86a7f34
  (fn [f & args]
    (let [start   (if (= 2 (count args)) (first args) (first (first args)))
          targets (if (= 2 (count args)) (second args) (rest (first args)))]
      (letfn [(get-next [current targets]
                (if (empty? targets) (cons current ()) (cons current (lazy-seq (get-next (f current (first targets)) (rest targets))))))]
        (get-next start targets)))))

(defcheck solution-d8c0dced
  (fn p53
    ([f x]
     (p53 f (first x) (rest x)))

    ([f x y]
     (if (seq y)
       (cons x (lazy-seq (p53 f (f x (first y)) (next y))))
       [x]))))

(defcheck solution-d8ecd128
  (fn t
    ([f a] (t f (first a) (rest a)))
    ([f i a]
     (if (empty? a) [i]
                    (lazy-seq (cons i (t f (f i (first a)) (rest a))))))))

(defcheck solution-d932ed3
  (fn myred
    ([f x] (myred f (first x) (rest x)))
    ([f i x] (if (empty? x)
               (vector i)
               (cons i (lazy-seq (myred f (f i (first x)) (rest x))))))))

(defcheck solution-d9702d47
  (fn rreductions
    ([f l] (rreductions f (first l) (rest l)))
    ([f e l] (if (empty? l) (list e)
                            (cons e
                              (lazy-seq (rreductions f (f e (first l)) (rest l))))))))

(defcheck solution-d97a59d6
  (fn me
    ([f l] (me f (first l) (next l)))
    ([f z l]
     (let [r (f z (first l)) l (next l)]
       (if l
         (cons z (lazy-seq (me f r l)))
         (list z r))))))

(defcheck solution-da44e675
  (fn partial-sums
    ([f xs]
     (when xs
       (if-let [head (first xs)]
         (partial-sums f head (next xs))
         xs)))
    ([f init xs]
     (cons init
       (when xs
         (lazy-seq (partial-sums f (f init (first xs)) (next xs))))))))

(defcheck solution-da58958
  (fn reds
    ([f init coll]
     (cons init (lazy-seq (when-let [[x & xs] (seq coll)]
                            (reds f (f init x) xs)))))
    ([f coll]
     (lazy-seq (when-let [[x & xs] (seq coll)]
                 (reds f x xs))))))

(defcheck solution-da59f306
  (fn
    ([f c]
     ((fn ! [agg coll]
        (if-let [s (seq coll)]
          (let [v (f agg (first s))]
            (lazy-seq (cons v (! v (rest s))))))) 0 c))
    ([f init c]
     (cons init ((fn ! [agg coll]
                   (if-let [s (seq coll)]
                     (let [v (f agg (first s))]
                       (lazy-seq (cons v (! v (rest s))))))) init c)))
    ))

(defcheck solution-daafba35
  (fn f
    ([g [h & r]] (f g h r))
    ([g i [h & r]]
     (cons i (if h (lazy-seq (f g (g i h) r)))))))

(defcheck solution-db265cf9
  (fn red
    ([the-fn arr] (red the-fn (first arr) (rest arr)))
    ([the-fn v arr]
     (letfn [(%red [v [el & rest]]
               (lazy-seq
                 (let [result (the-fn v el)]
                   (if-not (empty? rest)
                     (cons result (%red result rest))
                     (list result)))))]
       (lazy-seq (cons v (%red v arr)))))))

(defcheck solution-db2a3359
  (fn red
    ([f [x1 & r]] (red f x1 r))
    ([f x0 [x1 & r]]
     (if (nil? x1) (list x0)
                   (lazy-seq (cons x0 (red f (f x0 x1) r)))))))

(defcheck solution-db78e14d
  (fn redu
    ([op args] (redu op (first args) (rest args)))
    ([op seed args]
     (if (not (empty? args))
       (let [v (op seed (first args))]
         (lazy-cat [seed] (redu op v (rest args))))
       [seed]))))

(defcheck solution-db8aca82
  (fn reduc
    ([f s] (reduc f (first s) (rest s)))
    ([f i s]
     (if (empty? s) [i] (lazy-seq (cons i (reduc f (f i (first s)) (rest s))))))))

(defcheck solution-dbb3956f
  (fn red
    ([f l] (red f (first l) (rest l)))
    ([f i l] (cons i (lazy-seq (if (empty? l) '() (red f (f i (first l)) (rest l))))))
    ))

(defcheck solution-dbb6b054
  (fn myreductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (myreductions f (first coll) (rest coll))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [[v1 & r] (seq coll)]
           (myreductions f (f init v1) r)))))))

(defcheck solution-dc75693e
  (fn r
    ([f col] (r f (first col) (rest col)))
    ([f val col]
     (if (empty? col) (list val)
                      (lazy-seq
                        (cons val
                          (r f
                            (f val (first col))
                            (rest col))))))))

(defcheck solution-dcbec719
  (fn re
    ([f [h & r]] (re f h r))
    ([f i [h & r]]
     (cons i (if h (lazy-seq (re f (f i h) r)))))))

(defcheck solution-dd96a692
  (fn r ([f c] (r f (first c) (rest c))) ([f i c] (if (empty? c) (cons i nil) (lazy-seq (cons i (r f (f i (first c)) (rest c))))))))

(defcheck solution-ddc2e0e1
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f init-val coll]
     (if (empty? coll)
       (cons init-val ())
       (lazy-seq (cons init-val (r f (f init-val (first coll)) (rest coll))))))))

(defcheck solution-df3ac4b
  (fn my-reductions
    ([f col] (my-reductions f (first col) (drop 1 col)))
    ([f x col]
     (if (empty? col) [x]
                      (let [next-red (f x (first col))
                            col-left (drop 1 col)]
                        (lazy-seq (cons x (my-reductions f next-red col-left))))))))

(defcheck solution-df8fa6dd
  (fn [f val & col]
    (let [v (if (nil? col) (first val) val)
          c (if (nil? col) (rest val) (first col))]
      ((fn r [f v c]
         (lazy-seq (cons v
                     (if-let [fst (first c)]
                       (r f (f v fst) (rest c))))))
       f v c))))

(defcheck solution-dfaf629
  (fn g
    ([o [f & r]] (g o f r))
    ([o s [f & r]]
     (if f
       (cons s (lazy-seq (g o (o s f) r)))
       (list s)))))

(defcheck solution-e0cb1ea5
  (fn reduct
    ([func seq]
     (let [[result head & tail] seq
           next-result (func result head)]
       (cons result (if (empty? tail)
                      (list next-result)
                      (lazy-seq (reduct func (cons next-result tail)))))))
    ([func result seq]
     (reduct func (cons result seq)))))

(defcheck solution-e0e958f8
  (fn r
    ([f c]
     (lazy-seq (if-let [s (seq c)] (r f (first s) (rest s)) (list (f)))))
    ([f a c]
     (cons a (lazy-seq (when-let [s (seq c)] (r f (f a (first s)) (rest s))))))))

(defcheck solution-e1956fdd
  (fn my-reductions
    ([f xs]
     (if (empty? xs) [(f)]
                     (my-reductions f (first xs) (rest xs))))
    ([f init xs]
     (if (empty? xs) [init]
                     (cons init (lazy-seq (my-reductions f (f init (first xs)) (rest xs))))))))

(defcheck solution-e1f81fbc
  (fn redux ([f coll]
             (lazy-seq
               (if-let [s (seq coll)]
                 (redux f (first s) (rest s))
                 (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (redux f (f init (first s)) (rest s))))))))

(defcheck solution-e289d7c4
  (fn reduc
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reduc f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reduc f (f init (first s)) (rest s))))))
    ))

(defcheck solution-e2e8e3b7
  (fn rd
    ([f coll]
     (if-let [sq (seq coll)]
       (rd f (first sq) (rest sq))
       (list (f))))
    ([f init coll]
     (cons init (lazy-seq (when-let [sq (seq coll)]
                            (rd f (f init (first sq)) (rest sq))))))))

(defcheck solution-e2ec83d1
  (let [reduck (fn reduck
                 [f start s]
                 (if (seq s)
                   (let [v (f start (first s))]
                     (cons v (lazy-seq (reduck f v (rest s)))))
                   []))]
    (fn
      ([f s]
       (let [start (first s)]
         (cons start (reduck f start (rest s)))))
      ([f start s]
       (cons start (reduck f start s))))))

(defcheck solution-e33ab58e
  (fn red
    ([f z coll]
     ;(println "z" z "fst" fst "tail" tail)
     (if (empty? coll)
       [z]
       (cons z (lazy-seq (red f (f z (first coll)) (rest coll))))))
    ([f [fst & tail]] (red f fst tail))))

(defcheck solution-e33f6335
  (fn d
    ([f a [x & r]]
     (cons a
       (lazy-seq
         (if x
           (d f (f a x) r)))))
    ([f [x & r]] (d f x r))))

(defcheck solution-e42bff9c
  (fn reds
    ([f coll]
     (lazy-seq
       (if (empty? coll)
         []
         (reds f (first coll) (rest coll)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (if (empty? coll)
           []
           (reds f (f init (first coll)) (rest coll))))))))

(defcheck solution-e4fadd60
  (fn my-reductions
    ([f coll]
     (if (empty? coll) []
                       (my-reductions f (first coll) (rest coll))))

    ([f init coll]
     (cons init
       (lazy-seq
         (if ((comp not empty?) coll)
           (my-reductions f (f init (first coll)) (rest coll))))))))

(defcheck solution-e53c508
  (fn redux
    ([op ini coll]
     ((fn helper [op prev remain]
        (if (empty? remain)
          [prev]
          (let [cur (op prev (first remain))]
            (cons prev (lazy-seq (helper op cur (rest remain))))))) op ini coll))
    ([op coll] (redux op (first coll) (rest coll)))))

(defcheck solution-e573f258
  (fn reds
    ([op aseq]
     (reds op (first aseq) (next aseq)))
    ([op acum aseq]
     (lazy-seq
       (cons acum
         (if-let [elem (first aseq)]
           (reds op (op acum elem) (rest aseq))))))))

(defcheck solution-e5d13c05
  (fn rdct
    ([f [head & tail]] (rdct f head tail))
    ([f head [thead & ttail]]
     (if (nil? thead)
       (lazy-seq [head])
       (lazy-seq (concat
                  [head] (rdct f
                           (f head thead) ttail))))
     )
    ))

(defcheck solution-e5fb5c28
  (fn redus
    ([f lst] (redus f (first lst) (rest lst)))
    ([f init lst]
     (cond
       (empty? lst)
       [init]
       :else
       (cons init (lazy-seq (redus f (f init (first lst)) (rest lst))))
       )
     )
    ))

(defcheck solution-e67198bb
  (fn myf
    ([f i coll]
     (if (empty? coll)
       (list i)
       (cons i (lazy-seq (myf f (f i (first coll)) (rest coll))))))
    ([f coll] (myf f (first coll) (rest coll)))))

(defcheck solution-e6775d
  (fn myreduce
    ([f v] (myreduce f 0 (next v)))
    ([f i v]
     (let [x (atom i)]
       (cons i
         (for [y v]
           (swap! x f y)
           )
         )
       )
     )
    ))

(defcheck solution-e69e9205
  (fn rdctns
    ([f [x & s]]
     (rdctns f x s))
    ([f i [x & s]]
     (lazy-seq
       (cons i
         (if x
           (rdctns f (f i x) s)))))))

(defcheck solution-e77e3e02
  (letfn [
          (r* [f x s]
            (lazy-seq
              (when-not (empty? s)
                (let [n (f x (first s))]
                  (cons n (r* f n (rest s)))))))
          (r
            ([f s] (r* f (first s) s))
            ([f x s] (cons x (r* f x s))))]
    r))

(defcheck solution-e780e689
  (fn ductions
    ([fun coll] (ductions fun (first coll) (rest coll)))
    ([fun val coll] (if (empty? coll)
                      (list val)
                      (let [reduction (fun val (first coll))]
                        (lazy-seq (cons val
                                    (ductions fun reduction (rest coll)))))))))

(defcheck solution-e79e3e7e
  (fn reductions-
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reductions- f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reductions- f (f init (first s)) (rest s))))))))

(defcheck solution-e7e81cb
  (fn red
    ([f [x & xs]] (red f x xs))
    ([f acc [x & xs]] (lazy-seq (cons acc (when x (red f (f acc x) xs)))))))

(defcheck solution-e82aa0f5
  (fn
    [f & xs]
    (let [s    (if (second xs) (second xs) (first xs))
          seed (if (second xs) (first xs) (f))]
      (letfn [(reduct [l res]
                (lazy-seq
                  (when (seq l)
                    (let [current (f res (first l))]
                      (cons current (reduct (rest l) current))))))]
        (if (second xs)
          (cons seed (reduct s seed))
          (reduct s seed))))))

(defcheck solution-e8d3c9c4
  (fn r
    ([f x vs]
     (if (empty? vs)
       (list x)
       (lazy-seq (cons x (r f (f x (first vs)) (rest vs))))))
    ([f vs] (r f (first vs) (rest vs)))
    ))

(defcheck solution-e8f1495e
  (fn rds
    ([f input] (rds f (first input) (rest input)))
    ([f initial input]
     ((rand-nth
        [;; version using STM is more concise
         #(let [result  (atom initial)
                reduces (fn [in] (swap! result f in))]
            (cons initial (map reduces input)))
         ;; version using an extra arg instead of mutation
         #(->> [initial input]
            (iterate (fn [[result [el & more :as input]]]
                       (when (seq input) [(f result el) more])))
            (take-while identity)
            (map first))])))))

(defcheck solution-e900d3ae
  (fn reduct
    ([f [x & xs]] (reduct f x xs))
    ([f init [x & xs :as all]]
     (if (nil? (seq all))
       [init]
       (lazy-seq
         (cons init (reduct f (f init x) xs)))))))

(defcheck solution-e9030604
  (fn r
    ([f [h & t]]
     (r f h t))
    ([f a [h & t]]
     (lazy-seq (cons a (when h (r f (f a h) t)))))))

(defcheck solution-e967d541
  (fn red
    ([f [h & t]]
     (red f h t))
    ([f s [h & t]]
     (lazy-seq
       (if h
         (cons s (red f (f s h) t))
         (list s))))))

(defcheck solution-e998e54a
  (fn fd
    ([f xs]
     (if (seq xs) (fd f (first xs) (rest xs))
                  []))
    ([f a xs]
     (lazy-seq
       (if (seq xs)
         (cons a
           (fd f (f a (first xs)) (rest xs)))
         [a])))))

(defcheck solution-e9acbf71
  (fn r
    ([f [h & t]] (r f h t))
    ([f x l] (lazy-seq (cons x (if-let [[h & t] (seq l)] (r f (f x h) t) '()))))))

(defcheck solution-e9df3458
  (fn reduxSeq [& args]
    (let
     [inner
      (fn reduxSeqIn
        [func init myseq]
        (let [firstElem (first myseq), nextInit (apply func (list init firstElem))]
          (if (nil? (second myseq))
            (list nextInit)
            (cons nextInit (lazy-seq (reduxSeqIn func nextInit (rest myseq))))))
        ),
      wrapper
      (fn
        ([func myseq] (inner func 0 myseq))
        ([func init myseq]
         (cons init (lazy-seq (inner func init myseq)))))]
      (apply wrapper args))))

(defcheck solution-ea8d8e28
  (fn reduces
    ([func init seq]
     (cons
       init
       ((fn doreduce [previous [f & m]]
          (let [next (func previous f)]
            (if (nil? m)
              (list next)
              (cons next (lazy-seq (doreduce next m))))))
        init seq)))
    ([func [init & more]]
     (reduces func init more))))

(defcheck solution-ead4be91
  (fn my-reductions
    ([op v col]
     (lazy-seq
       (cons v
         (if (first col)
           (my-reductions op (op v (first col)) (next col))))))
    ([op col]
     (my-reductions op (first col) (rest col)))))

(defcheck solution-eb26301
  (fn [f s & [p]]
    (apply
      (fn g [s p]
        (lazy-seq
          (if (not-empty p)
            (let [r (f s (nth p 0))]
              (cons s (g r (rest p))))
            [s])))
      (if p [s p] [(nth s 0) (rest s)]))))

(defcheck solution-eb28ddd1
  (fn redux
    ([f xs] (redux f (first xs) (rest xs)))
    ([f init xs]
     (cons init
       (if (empty? xs) nil
                       (lazy-seq
                         (let [nextinit (f init (first xs))]
                           (redux f nextinit (rest xs)))))))))

(defcheck solution-eb8afaf5
  (fn myReductions
    ([f preVal coll]
     (lazy-seq
       (if-let [firstVal (first coll)]                      ;if there is still some items left
         (cons preVal (myReductions f (f preVal firstVal) (rest coll))) ;lazy recipe
         (list preVal))))                                   ;last item shall be containted by a seq
    ([f coll]
     (myReductions f (first coll) (rest coll)))))

(defcheck solution-ebac12f
  (fn reduct2
    ([f s] (reduct2 f (first s) (rest s)))
    ([f initial s]
     (let [reductor (fn me [f initial s]
                      (lazy-seq
                        (when-let [s (seq s)]
                          (let [step (f initial (first s))]
                            (cons step (me f step (rest s)))))))]
       (cons initial (reductor f initial s))))))

(defcheck solution-ebafc677
  (fn prob-0060
    ([f lhs0 in-xs]
     (prob-0060 f (cons lhs0 in-xs)))
    ([f in-xs]
     (let [gen-rest (fn gen-rest [f xs lhs]
                      (lazy-seq
                        (let [red-val (f lhs (first xs))
                              rest-xs (rest xs)]
                          (if (empty? rest-xs)
                            (list red-val)
                            (cons red-val (gen-rest f rest-xs red-val))))))

           seq-xs   (seq in-xs)
           fst-x    (first seq-xs)
           rst-xs   (rest seq-xs)]

       (lazy-seq
         (cond
           (empty? seq-xs) (list (f))
           (empty? rst-xs) (list fst-x)
           :else (cons fst-x (gen-rest f rst-xs fst-x))))))))

(defcheck solution-ebd7b844
  (fn x
    ([f p [e & l]]
     (lazy-seq
       (let [n (f p e)]
         (if l
           (cons p (x f n l))
           (list p n)))))
    ([f [e & l]]
     (x f e l))))

(defcheck solution-ec053db0
  (fn r
    ([f [x & xs]]
     (cons x (lazy-seq (when (first xs)
                         (r f (f x (first xs)) (rest xs))))))
    ([f v [x & xs]]
     (cons v (lazy-seq (when x
                         (r f (f v x) xs)))))))

(defcheck solution-ec47ca44
  (fn sr-60
    ([f xs] (sr-60 f (first xs) (next xs)))
    ([f acc xs]
     (lazy-seq
       (cons acc
         (if (seq xs)
           (sr-60 f (f acc (first xs)) (next xs))
           nil))))))

(defcheck solution-ec9fd603
  (fn reduce'
    ([f coll] (reduce' f (first coll) (rest coll)))
    ([f memo coll]
     (if-not (seq coll)
       [memo]
       (let [res (f memo (first coll))]
         (cons memo (lazy-seq (reduce' f res (rest coll)))))))))

(defcheck solution-ecab5355
  (fn redukshun
    ([f [h & t]]
     (redukshun f h t))
    ([f acc [h & t]]
     (lazy-seq
       (if h
         (cons acc (redukshun f (f acc h) t))
         (list acc))))))

(defcheck solution-ecd8757b
  (fn my-reductions
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (my-reductions f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (my-reductions f (f init (first s)) (rest s))))))))

(defcheck solution-ecfd4691
  (fn redux
    ([f coll]
     (if (seq coll)
       (redux f (first coll) (rest coll))
       )
     )
    ([f init coll]
     (cons init
       (lazy-seq
         (when (seq coll)
           (redux f (f init (first coll)) (rest coll))
           )
         )
       )
     )
    ))

(defcheck solution-ed0df293
  (fn red ([f x] (red f (first x) (rest x)))
    ([f x y]
     (if (empty? y) [x]
                    (lazy-seq
                      (cons x
                        (red f (f x (first y)) (rest y))))))))

(defcheck solution-ed5a132e
  #(letfn
    [(r
       ([f v] (r f (first v) (rest v)))
       ([f e v]
        (if (empty? v)
          [e]
          (lazy-cat [e] (r f (f e (first v)) (rest v))))))]
     (apply r %&)))

(defcheck solution-edb739d3
  (fn reductions*
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reductions* f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reductions* f (f init (first s)) (rest s))))))))

(defcheck solution-ee172867
  (fn aa [x & args]
    (if (> (count args) 1)

      (reduce #(conj %1 (x (last %1) %2)) [(first args)] (second args))

      (reduce #(conj %1 (x (last %1) %2)) [(ffirst args)] (take 100 (rest (first args))))
      )
    ))

(defcheck solution-eed7b3ca
  (fn reduced
    ([f coll]
     (reduced f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (cons v (reduced f (f v (first s)) (rest s)))
         [v])))))

(defcheck solution-eee69a66
  (fn [& [f i c]]
    ((fn s [a c]
       (cons a (when c
                 (lazy-seq
                   (s (f a (first c))
                     (next c))))))
     (if c i 0)
     (if c c (next i)))))

(defcheck solution-ef666b0d
  (fn my-reductions
    ([f c] (my-reductions f (first c) (rest c)))
    ([f v c]
     (if (empty? c) (list v)
                    (lazy-seq (cons v (my-reductions f (f v (first c)) (rest c))))))))

(defcheck solution-ef8f2148
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f v coll] (cons v (lazy-seq
                          (when-let [s (seq coll)]
                            (red f (f v (first s)) (rest s))))))
    ))

(defcheck solution-efa6cb6a
  (fn myreductions
    ([f s] (myreductions f (first s) (rest s)))
    ([f b s]
     (lazy-seq
       (if (seq s)
         (cons b (myreductions f (f b (first s)) (rest s)))
         (list b))))))

(defcheck solution-efb866bc
  (fn lazy-reduce
    ([f coll]
     (lazy-reduce f (first coll) (rest coll)))
    ([f val coll]
     (lazy-seq (if (empty? coll)
                 (list val)
                 (let [carry-forward-value (f val (first coll))
                       remaining-coll      (rest coll)]
                   (cons val (lazy-reduce f carry-forward-value remaining-coll))))))))

(defcheck solution-efbe8732
  (fn my-reduce
    ([f coll] (my-reduce f (first coll) (rest coll)))
    ([f initial-value coll]
     (if (empty? coll)
       (list initial-value)
       (lazy-seq (cons initial-value
                   (my-reduce f (apply f [initial-value (first coll)]) (rest coll))))))))

(defcheck solution-efd8a651
  (fn r
    ([f s]
     (r f (first s) (rest s)))
    ([f i s]
     (cons i
       (lazy-seq
         (if (not (empty? s))
           (r f
             (f i (first s))
             (rest s))))))))

(defcheck solution-efdb907a
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll]
     (if (empty? coll) [init]
                       (lazy-seq (cons init (red f (f init (first coll)) (rest coll))))))))

(defcheck solution-f008d399
  (fn [f & a]
    (let
     [s (concat (butlast a) (last a))]
      (map
        #(reduce f %)
        (map #(take % s)
          (map-indexed (fn [x _] (inc x)) s)
          )))))

(defcheck solution-f057a07e
  (fn red
    ([f s] (lazy-seq (if-let [c (seq s)]
                       (red f (first c) (rest c)))))
    ([f i s] (cons i (lazy-seq (when-let [c (seq s)]
                                 (red f (f i (first c)) (rest c))))))))

(defcheck solution-f138f09e
  (fn rdctns
    ([op i s]
     (if (empty? s)
       [i]
       (lazy-seq (cons i (rdctns op (op i (first s)) (rest s))))))
    ([op s]
     (if (empty? s)
       nil
       (rdctns op (first s) (rest s))))))

(defcheck solution-f164ab8e
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f s coll]
     (if (empty? coll)
       [s]
       (lazy-seq (cons s (r f (f s (first coll)) (rest coll)))))
     )))

(defcheck solution-f1cd1eac
  (fn reductions*
    ([f col]
     (reductions* f (first col) (rest col)))
    ([f init col]
     (cons init
       (lazy-seq
         (if (empty? col)
           ()
           (let [s-col (seq col)]
             (reductions* f (f init (first s-col)) (rest s-col)))))))))

(defcheck solution-f1dd944c
  (fn my-reductions
    ([f col]
     (my-reductions f (first col) (rest col)))
    ([f init col]
     (cons
       init
       (lazy-seq
         (if (empty? col)
           nil
           (my-reductions f
             (apply f (list init (first col)))
             (rest col))))))))

(defcheck solution-f2252839
  (fn my-reductions
    ([f ls] (my-reductions f (first ls) (rest ls)))
    ([f v ls] (lazy-seq
                (if (empty? ls)
                  (list v)
                  (cons v (my-reductions f (f v (first ls)) (rest ls))))))))

(defcheck solution-f2543f64
  (fn reduce-ls--
    ([func coll] (reduce-ls-- func (first coll) (rest coll)))
    ([func tot coll]
     (if (empty? coll)
       (list tot)
       (cons tot (lazy-seq (reduce-ls-- func (func tot (first coll)) (rest coll))))))))

(defcheck solution-f262fc5
  (fn xreductions
    ([f seq] (xreductions f (first seq) (rest seq)))
    ([f firstarg seq]
     (letfn [(reduct [f acc se]
               (lazy-seq (when-not (empty? se)
                           (let [res (f acc (first se))]
                             (cons res (reduct f res (rest se)))))))]
       (lazy-seq (cons firstarg (reduct f firstarg seq)))
       ))))

(defcheck solution-f292b718
  (fn my-reductions
    ([f [x & xs]] (my-reductions f x xs))
    ([f v [x & xs]] (lazy-seq (cons v (when x (my-reductions f (f v x) xs)))))))

(defcheck solution-f2e26630
  (fn myr
    ([f coll]
     (if (or (empty? coll) (empty? (rest coll)))
       coll
       (myr f (first coll) (rest coll))))
    ([f x coll]
     (if (empty? coll) (list x)
                       (lazy-cat (list x) (myr f (f x (first coll)) (rest coll)))))))

(defcheck solution-f34748d0
  (fn rdctns
    ([f coll] (rdctns f (first coll) (rest coll)))
    ([f init coll]
     (lazy-seq
       (cons init (when (seq coll) (rdctns f (f init (first coll)) (rest coll))))))))

(defcheck solution-f36e1bbc
  (fn rdc ([f coll]
           (lazy-seq
             (if-let [s (seq coll)]
               (rdc f (first s) (rest s))
               (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (rdc f (f init (first s)) (rest s))))))))

(defcheck solution-f3791ac1
  (fn fn2
    ([f a]
     (fn2 f (first a) (rest a)))
    ([f i a]
     (letfn [(my-func [init args]
               ;(println init args)
               (cons init
                 (when-let [s (seq args)]
                   (lazy-seq (my-func (f init (first s)) (rest s))))))]
       (my-func i a)))))

(defcheck solution-f3ecb77f
  (fn r
    ([f [a & b :as s]]
     (if s
       (r f a b)))
    ([f i [a & b :as s]]
     (cons i (lazy-seq
               (if s
                 (r f (f i a) b)))))))

(defcheck solution-f3f2f117
  (fn reds
    ([f xs]
     (reds f (first xs) (rest xs)))
    ([f v xs]
     (if (empty? xs)
       (list v)
       (let [v2 (f v (first xs))]
         (lazy-seq
           (cons v
             (reds f v2 (rest xs)))))))))

(defcheck solution-f49fc401
  (fn red
    ([f coll] (red f (first coll) (rest coll)))
    ([f init coll] (let [nxt (f init (first coll))]
                     (if (empty? (rest coll))
                       [init nxt]
                       (cons init (lazy-seq (red f nxt (rest coll))))
                       )
                     ))
    ))

(defcheck solution-f556c23d
  (fn !
    ([f s]
     (map #(reduce f %)
       (map (fn [n x] (take (inc n) s)) (range) s)))
    ([f v s]
     (! f (cons v s)))))

(defcheck solution-f56279d6
  (fn rd
    ([f a [h & t]]
     (cons a
       (when h
         (lazy-seq
           (rd f (f a h) t)))))
    ([f [h & t]]
     (when h (rd f h t)))))

(defcheck solution-f5a7c662
  (fn rd
    ([fun c]
     (rd fun (fun (first c)) (rest c)))
    ([fun init c]
     (lazy-cat [init] (if (empty? c) c (rd fun (fun init (first c)) (rest c)))))))

(defcheck solution-f601a081
  (fn reducs
    ([f xs] (if (empty? xs)
              (list (f))
              (reducs f (first xs) (rest xs))))
    ([f val xs] (if (empty? xs)
                  (list val)
                  (let [new-val (f val (first xs))]
                    (cons val (lazy-seq (reducs f new-val (rest xs)))))))))

(defcheck solution-f66e0bcd
  (fn seq-reductions
    ([f coll]
     (seq-reductions f (first coll) (next coll)))
    ([f init coll]
     (cons init
       (lazy-seq
         (if (seq coll)
           (seq-reductions f (f init (first coll)) (rest coll))))))))

(defcheck solution-f6bf44af
  (fn c [func & other]
    (if (= 1 (count other))
      (let [its-it (fn g [f s v] (cons v (lazy-seq (g f (next s) (f v (first s))))))] (its-it func (next (first other)) (ffirst other)))
      (let [init   (first other)
            its-it (fn g [f s v] (cons v (if (nil? s) nil (lazy-seq (g f (next s) (f v (first s)))))))] (its-it func (fnext other) init))
      )
    ))

(defcheck solution-f6de2c35
  (fn sr
    ([op xs] (sr op (op (first xs)) (rest xs)))
    ([op val xs]
     ;;(println (str "val:" val))
     ;;(println xs)
     (lazy-seq
       (cons val
         (if (empty? xs)
           xs
           (sr op (op val (first xs)) (rest xs))))))))

(defcheck solution-f6fa0df9
  (fn folds
    ([f xs]
     (if-let [[x & xs] (seq xs)]
       (folds f x xs)
       [(f)]))

    ([f init xs]
     (lazy-seq
       (cons
         init
         (if-let [[x & xs] (seq xs)]
           (folds f (f init x) xs)))))))

(defcheck solution-f71c6bcb
  (fn r
    ([f [x & xs]] (lazy-seq (r f x xs)))
    ([f x xs] (cons x (lazy-seq (if (empty? xs) '() (r f (f x (first xs)) (rest xs))))))))

(defcheck solution-f750c72b
  (fn reds
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reds f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reds f (f init (first s)) (rest s))))))))

(defcheck solution-f7aba968
  (fn reducts
    ([f c] (reducts f (first c) (rest c)))
    ([f i c]
     (if (empty? c) [i]
                    (lazy-cat [i] (reducts f (f i (first c)) (rest c)))))))

(defcheck solution-f7bec525
  (fn reduct
    ([f s] (reduct f (first s) (rest s)))
    ([f start s]
     (lazy-seq (cons start (when s (reduct f (f start (first s)) (next s))))))))

(defcheck solution-f7cd0e46
  (fn myReductions ([f c] (myReductions f (first c) (next c)))
    ([f v c] (if (empty? c) (vector v) (lazy-seq (cons v (myReductions f (f v (first c)) (drop 1 c))))))
    ))

(defcheck solution-f80ee5fd
  (fn f
    ([r c]
     (f r (first c) (rest c)))

    ([r v c]
     (cons v
       (lazy-seq
         (when (seq c)
           (f r (r v (first c)) (rest c))))))))

(defcheck solution-f815ed63
  (fn reducer
    ([op input] (reducer op (first input) (rest input)))
    ([op result input]
     (lazy-seq
       (if (empty? input) (vector result)
                          (cons result
                            (reducer op
                              (op result (first input))
                              (rest input))))))))

(defcheck solution-f837c64
  (fn R
    ([f v [h & t]]
     (if h
       (lazy-seq (cons v (R f (f v h) t)))
       [v]))
    ([f [h & t]] (R f h t))))

(defcheck solution-f83f4e38
  (fn scan
    ([f coll] (scan f (first coll) (rest coll)))
    ([f v coll]
     (lazy-seq
       (if (empty? coll)
         (list v)
         (cons v
           (scan f
             (f v (first coll))
             (rest coll))))))))

(defcheck solution-f850b1ae
  (fn myreductions
    ([f init coll]
     (letfn [(reductions2 [f init coll]
               (lazy-seq
                 (when-let [s (seq coll)]
                   (let [h (f init (first coll))]
                     (cons h (reductions2 f h (rest coll)))))))]
       (cons init (reductions2 f init coll))))
    ([f coll]
     cons (first coll) (myreductions f (first coll) (rest coll)))))

(defcheck solution-f8589eae
  (fn iter
    ([f [i & r]]
     (iter f i r))
    ([f a [i & r]]
     (if (nil? i) [a]
                  (lazy-seq (cons a (iter f (f a i) r)))))))

(defcheck solution-f893cfed
  (fn reducts
    ([f [acc & xs]] (reducts f acc xs))
    ([f acc xs] (cons acc (lazy-seq (if (not-empty xs) (reducts f (f acc (first xs)) (rest xs))))))))

(defcheck solution-f9253b8
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f val coll] (if (empty? coll)
                    [val]
                    (let [new-val (f val (first coll))]
                      (cons val (lazy-seq (my-reductions f new-val (rest coll)))))))))

(defcheck solution-f93b7684
  (letfn [(r [func in coll]
            (lazy-seq
              (cons in (if (seq coll) (r func (func in (first coll)) (rest coll))))))]
    (fn ([func coll] (r func (first coll) (rest coll)))
      ([func in coll] (r func in coll)))))

(defcheck solution-f98424b
  (fn ([f coll]
       (map #(reduce f %)
         (map (fn [_ x] (take (inc x) coll))
           coll
           (range))))
    ([f x coll]
     (map #(reduce f x %)
       (map (fn [_ x] (take x coll))
         (conj coll 0)
         (range))))))

(defcheck solution-f988620a
  (fn myreduce
    ([f col]
     (myreduce f
       (f (first col))
       (rest col))
     )                                                      ; 2 params
    ([f init [x & col]]
     (cons init
       (lazy-seq (when x (myreduce f
                           (f init x)
                           col
                           )                                ; myreduce call
                         )                                  ; when
         ))                                                 ; cons
     )                                                      ; 3 params
    ))

(defcheck solution-f9bf4708
  (fn r
    ([f coll]
     (r f (first coll) (rest coll)))
    ([f val coll]
     (if (empty? coll)
       (list val)
       (lazy-seq (cons val
                   (r f (f val (first coll)) (rest coll))))))))

(defcheck solution-f9e3782c
  (letfn [(myreductions
            ([f coll]
             (lazy-seq
               (if-let [s (seq coll)]
                 (myreductions f (first s) (rest s))
                 (list (f)))))
            ([f init coll]
             (cons init
               (lazy-seq
                 (when-let [s (seq coll)]
                   (myreductions f (f init (first s)) (rest s)))))))]
    (fn
      ([f coll] (myreductions f coll))
      ([f init coll] (myreductions f init coll)))))

(defcheck solution-fa071140
  (fn f
    ([g c]
     (lazy-seq (f g (first c) (rest c))))
    ([g i c]
     (if (empty? c)
       [i]
       (cons i (lazy-seq (f g (g i (first c)) (rest c))))))))

(defcheck solution-fa5dfe0e
  (fn rec
    ([f res coll]
     (if (nil? coll)
       [res]
       (cons res (lazy-seq (rec f (f res (first coll)) (next coll))))))
    ([f coll]
     (rec f (f (first coll)) (next coll)))))

(defcheck solution-fa674771
  (fn r
    ([f coll] (r f (first coll) (rest coll)))
    ([f init coll]
     (let [in coll, out [init]]
       (if (empty? in)
         out
         (lazy-cat out (r f (f (last out) (first in)) (rest in))))))))

(defcheck solution-fa8ce0d6
  (fn my-reductions
    ([f s]
     (my-reductions f (first s) (rest s)))
    ([f acc s]
     (let [new-acc (f acc (first s))]
       (if (seq (rest s))
         (lazy-seq (cons acc (my-reductions f new-acc (rest s))))
         (cons acc (cons new-acc nil)))))))

(defcheck solution-fa9fd7d5
  (fn p60
    ([func coll]
     (p60 func (first coll) (rest coll)))
    ([func init coll]
     `(~init
       ~@((fn step [coll prev]
            (if (empty? coll)
              '()
              (lazy-seq
                (let [v (func prev (first coll))]
                  `(~v ~@(step (rest coll) v))))))
          coll init)))))

(defcheck solution-fab60a41
  (fn myReduct
    ([f x s]
     (if (seq s)
       (cons x (lazy-seq (myReduct f (f x (first s)) (rest s))))
       (list x)))
    ([f s]
     (myReduct f (first s) (rest s)))))

(defcheck solution-faf0421
  (fn reduction*
    ([f [p1 & ps]] (reduction* f p1 ps))
    ([f p1 [p2 & ps]]
     (cons p1
       (when p2
         (lazy-seq (reduction* f (f p1 p2) ps)))))))

(defcheck solution-faf5b277
  (fn r
    ([f s] (r f (first s) (rest s)))
    ([f i s]
     (if (empty? s)
       (list i)
       (cons i (lazy-seq (r f (f i (first s)) (rest s))))))))

(defcheck solution-faf76893
  (fn r [f & args]
    (if
     (= (count args) 1)
      (r f (first (first args)) (rest (first args)))
      (let [[v l] args]
        (if (empty? l)
          [v]
          (cons v
            (lazy-seq
              (r
                f
                (f v (first l))
                (rest l)))))))))

(defcheck solution-fafc3ea0
  (fn it
    ([f [x & xs]] (it f x xs))
    ([f init xs] (if (seq xs)
                   (lazy-seq
                     (cons init (it f (f init (first xs)) (rest xs))))
                   [init]))))

(defcheck solution-fb067aa5
  (fn r
    ([f [x y & xs]] (cons x (lazy-seq (r f (f x y) xs))))
    ([f v xs] (cons v (when-let [[x & xs'] xs] (lazy-seq (r f (f v x) xs')))))))

(defcheck solution-fb081f03
  (fn f
    ([k [a & z]] (f k a z))
    ([k a [b & z]]
     (lazy-seq
       (cons a
         (when b
           (f k (k a b) z)))))))

(defcheck solution-fb0a1542
  (fn eka
    ([op xs]
     (eka op (first xs) (rest xs))
     )
    ([op v xs]
     (if (empty? xs)
       [v]
       (lazy-seq
         (let [v2 (op v (first xs))]
           (cons v (eka op v2 (rest xs)))))
       )
     )
    ))

(defcheck solution-fb21989c
  (let [ireduce (fn ireduce [f start xs]
                  (if (seq xs)
                    (cons start (lazy-seq (ireduce f (f start (first xs)) (rest xs))))
                    [start]))]
    (fn
      ([f xs]
       (ireduce f (first xs) (rest xs)))
      ([f start xs]
       (ireduce f start xs)))))

(defcheck solution-fb8fce40
  (fn redet ([f xs] (if (= (type 0) (type (first xs))) (redet f 0 xs)))
    ([f v xs] (letfn [(redet3 [ff vv ys]
                        (lazy-seq
                          (if (first ys)
                            (cons (ff vv (first ys))
                              (redet3 ff (ff vv (first ys)) (rest ys))
                              )

                            )
                          )
                        )]
                (let [tmp (redet3 f v xs)]
                  (if (= v (first tmp))
                    tmp
                    (cons v tmp)
                    )

                  )
                ))
    ))

(defcheck solution-fb956b00
  (fn [f a & b]
    (let [coll (if b (first b) a)
          f    (if b
                 (fn [x y] (reduce f a (take (inc x) coll)))
                 (fn [x y] (reduce f (take (inc x) coll))))]
      (if b
        (cons a (keep-indexed f coll))
        (keep-indexed f coll)))))

(defcheck solution-fbda5bdc
  (fn reduct
    ([f [x & xs]]
     (reduct f x xs))
    ([f v [x & xs]]
     (if x
       (cons v (lazy-seq (reduct f (f v x) xs)))
       [v]))))

(defcheck solution-fc212bb8
  (fn redctns
    ([f [x & xs]] (redctns f x xs))
    ([f x coll]
     (cons x (lazy-seq
               (when-let [s (seq coll)]
                 (redctns f (f x (first s)) (rest s))))))))

(defcheck solution-fc6969b1
  (fn r
    ([func init sq]
     (lazy-seq
       (cons init
         (when-let [s (seq sq)]
           (r func
             (func init (first s))
             (rest s))))))
    ([func sq]
     (r func (first sq) (rest sq)))))

(defcheck solution-fc90b0d1
  (fn my-reduct
    ([op start remain]
     (lazy-seq (cons start
                 (if (not (= (first remain) nil))
                   (my-reduct op (op start (first remain)) (rest remain))))))
    ([op remain]
     (my-reduct op (first remain) (rest remain))
     )
    ))

(defcheck solution-fc9d0d85
  (fn red
    ([f s c]
     (lazy-seq (cons s
                 (when-let [c0 (first c)]
                   (red f (f s c0) (rest c))))))
    ([f c]
     (lazy-seq (red f (first c) (rest c))))))

(defcheck solution-fc9fd088
  (fn reducing
    ([f c] (reducing f (first c) (next c)))
    ([f e c]
     (if (seq c)
       (lazy-seq (cons e (reducing f (f e (first c)) (next c))))
       (cons e nil)))))

(defcheck solution-fcb21750
  (fn myReductions
    ([fun init coll]
     (lazy-seq
       (if (seq coll)
         (let [newInit (fun init (first coll))]
           (cons init (myReductions fun newInit (rest coll))))
         (cons init []))))
    ([fun coll]
     (rest (myReductions fun 0 coll)))))

(defcheck solution-fcd7c920
  (fn my-reductions
    ([f [a & coll]]
     (my-reductions f a coll))
    ([f val [a & coll :as all]]
     (lazy-seq
       (if all
         (cons val
           (my-reductions f (f val a) coll))
         `(~val))))))

(defcheck solution-fce39878
  (fn [& args] (letfn [(reds
                         ([f coll]
                          (lazy-seq
                            (if-let [s (seq coll)]
                              (reds f (first s) (rest s))
                              (list (f)))))
                         ([f init coll]
                          (cons init
                            (lazy-seq
                              (when-let [s (seq coll)]
                                (reds f (f init (first s))
                                  (rest s)))))))]
                 (if (= 2 (count args))
                   (reds (first args) (second args))
                   (reds (first args) (second args) (last args))))))

(defcheck solution-fd15d690
  (fn __
    ([f a b]
     (letfn [(re [f r l]
               (cons (f r (first l))
                 (if (empty? (rest l))
                   nil
                   (lazy-seq (re f (f r (first l)) (rest l)))
                   )
                 )
               )]
       (cons a (lazy-seq (re f a b))
         )))
    ([f a] (__ f (first a) (rest a)))
    ))

(defcheck solution-fd5f7efd
  (fn red
    ([f s]
     (if (empty? s)
       []
       (red f (first s) (rest s))))
    ([f initial s]
     (if (empty? s)
       [initial]
       (cons initial (lazy-seq (red f
                                 (f initial (first s))
                                 (rest s))))))))

(defcheck solution-fd8eae03
  (fn custom-reductions
    ([f coll]
     (let [lazy-colls (fn colls
                        [n coll]
                        (if (not (empty? (drop (dec n) coll)))
                          (cons (take n coll)
                            (lazy-seq (colls (inc n) coll)))))
           colls      (lazy-colls 1 coll)]
       (map #(reduce f %) colls)))
    ([f init coll]
     (let [lazy-colls (fn colls
                        [n coll]
                        (if (not (empty? (drop (dec n) coll)))
                          (cons (take n coll)
                            (lazy-seq (colls (inc n) coll)))))
           colls      (lazy-colls 0 coll)]
       (map #(reduce f init %) colls)))))

(defcheck solution-fdc782d9
  (fn reduc
    ([f coll]
     (lazy-seq
       (if-let [s (seq coll)]
         (reduc f (first s) (rest s))
         (list (f)))))
    ([f init coll]
     (cons init
       (lazy-seq
         (when-let [s (seq coll)]
           (reduc f (f init (first s)) (rest s)))))
     )
    ))

(defcheck solution-fed49433
  (fn _rdx
    ([f coll]
     (lazy-seq (_rdx f (first coll) (rest coll))))
    ([f v coll]
     (cons v (lazy-seq
               (when (not (empty? coll))
                 (_rdx f (f v (first coll)) (rest coll))))))))

(defcheck solution-feeadda0
  (fn my-reductions
    ([f coll] (my-reductions f (first coll) (rest coll)))
    ([f initial coll]
     (if (seq coll)
       (cons
         initial
         (lazy-seq (my-reductions f (f initial (first coll)) (rest coll))))
       (list initial)
       )
     )
    ))

(defcheck solution-ff1325a2
  (fn myred
    ([f xs]
     (myred f (first xs) (rest xs)))
    ([f val xs]
     (if (empty? xs)
       [val]
       (lazy-seq (cons val (myred f (f val (first xs)) (rest xs))))))))

(defcheck solution-ff8a9e2f
  (fn red
    ([f s] (red f (first s) (rest s)))
    ([f x s]
     (if (seq s)
       (cons x (lazy-seq (red f (f x (first s)) (rest s))))
       [x]))))
