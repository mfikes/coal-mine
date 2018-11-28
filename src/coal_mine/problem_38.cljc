(ns coal-mine.problem-38
  (:require [coal-mine.checks :refer [defcheck-38] :rename {defcheck-38 defcheck}]
            [clojure.test]))

(defcheck solution-10066747
  (fn [& coll] (reduce (fn [x y] (if (< x y) y x)) (first coll) coll)))

(defcheck solution-1073ff1b
  (fn [& xs] (-> xs sort last)))

(defcheck solution-10862f8c
  (fn [& args]
    (reduce (fn [a b] (if (> a b) a b)) args)))

(defcheck solution-116f306b
  (fn [ & args ]
    (reduce (fn [a b] (if (< a b) b a)) args)))

(defcheck solution-120a7c21
  (fn [& numbers]
    (reduce #(if (> %1 %2) %1 %2) numbers)))

(defcheck solution-12109e6c
  (fn [& args] (reduce (fn [a b] (if (> a b) a b)) args)))

(defcheck solution-12274da9
  (fn m [& xs]
    (when-first [x xs]
      (if-let [m* (->> xs rest (apply m))]
        (if (> m* x) m* x)
        x))))

(defcheck solution-12332de7
  (fn  [& lst]
    (loop [rm (seq lst), temp-max 0]
      (cond (empty? rm) temp-max
            (> (first rm) temp-max) (recur (rest rm) (first rm))
            :else (recur (rest rm) temp-max)))))

(defcheck solution-12339204
  (fn [& vs] (reduce #(if (> %2 %1) %2 %1) vs)))

(defcheck solution-127b0065
  (fn [v & vs]
    (loop [m   v
           ms  vs]
      (if (empty? ms)
        m
        (let [h (first ms)
              t (rest  ms)]
          (if (> h m)
            (recur h t)
            (recur m t)))))))

(defcheck solution-12e59cbd
  (fn [a & b]
    (loop [m a s b]
      (if (empty? s) m (recur (if (> (first s) m) (first s) m) (rest s))))))

(defcheck solution-133564f7
  (fn [& more] (reduce #(if (> %1 %2) %1 %2) more)))

(defcheck solution-136f3ad
  (fn [& xs] (reduce (fn [m c] (if (< m c) c m)) xs)))

(defcheck solution-13c81592
  (fn max- [x & xs]
    (if (empty? xs) x (let [y (apply max- xs)]
                        (if (> x y) x y)))))

(defcheck solution-13d87c1b
  (fn [& ll]

    ((fn [l m]
       (if (= (count l) 0)
         m
         (let [f (first l) mm (if (> f m) f m)]
           (recur
             (rest l)
             mm
             )
           )
         )
       )
     (rest ll)
     (first ll)
     )))

(defcheck solution-13e9cb1e
  (fn [& coll] (first (sort > coll))))

(defcheck solution-140b4ccb
  (fn [& r] (reduce #(if (> %1 %2) %1 %2) r)))

(defcheck solution-140b9982
  (fn [& nums] (reduce #(if (> %1 %2) %1 %2) 0 nums)))

(defcheck solution-143e1b61
  (fn [& nums]
    (reduce
      (fn [a b]
        (if (> a b)
          a
          b)
        )
      nums
      )
    ))

(defcheck solution-1464605c
  (fn [& input-params]
    (reduce
      (fn [first-param second-param] (if (> first-param second-param) first-param second-param))
      input-params)
    ))

(defcheck solution-14d9fa2e
  (fn [& x] (reduce #(if (> %1 %2) %1 %2) 0 x)))

(defcheck solution-157c155c
  (fn max2
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (reduce max2 (max2 x y) more))))

(defcheck solution-15b2d822
  (fn [& args](last (sort args))))

(defcheck solution-15f4b905
  (fn my-max [& rest]
    (reduce (fn [a b] (if (> a b) a b)) (seq rest))))

(defcheck solution-16147bad
  (fn [& more]
    (reduce #(if (< %1 %2) %2 %1 ) more)))

(defcheck solution-16e76a83
  (fn [& coll] (- (apply min (map #(- %) coll)))))

(defcheck solution-171f6392
  (fn [a & l] (reduce #(if (> %1 %2) %1 %2) a l)))

(defcheck solution-17247aff
  (fn [& a] (reduce #(if (> %1 %2) %1 %2) a)))

(defcheck solution-17b67a5f
  (fn [x & xs]
    (if (empty? xs)
      x
      (if (> x (first xs))
        (recur x (rest xs))
        (recur (first xs) (rest xs))))))

(defcheck solution-17bce876
  #_(fn [& a] (reduce #(if (> %1 %2) %1 %2) a))

  #(first (sort > %&)))

(defcheck solution-17f1070b
  (fn [& l]
    (reduce
      (fn [m x]
        (if (> x m)
          x
          m
          )
        ) l
      )
    ))

(defcheck solution-180d8624
  (fn [& more]
    (last (sort more))))

(defcheck solution-183553b7
  (fn findm [& args]
    (reduce #(if (> %1 %2)
               %1
               %2) args)))

(defcheck solution-1847fb96
  (fn my-max [x & xs] (if (empty? xs) x (let [m (apply my-max xs)] (if (< x m) m x)))))

(defcheck solution-187a3e71
  ;;threading macro ->
  ;;$(".class").find("a").children().first();

  (fn [& xs] (-> xs (list*) (sort) (last))))

(defcheck solution-1900ac09
  (fn [& lst]
    (loop [m (first lst)
           lst (rest lst)]
      (cond (empty? lst)
            m
            (> (first lst) m)
            (recur (first lst) (rest lst))
            :else
            (recur m (rest lst))))))

(defcheck solution-19147ec3
  (fn [& rest]
    (reduce #(if (> %1 %2) %1 %2) rest)))

(defcheck solution-19e9c80f
  (fn [& params] (reduce (fn [a b] (if (< a b) b a)) 0 params)))

(defcheck solution-1a3bf0f4
  (fn [& a] (reduce (fn [x y] (if (> x y) x y)) a)))

(defcheck solution-1c861c9d
  (fn a [x & xs]
    (if (seq xs)
      (if (> (apply a xs) x)
        (apply a xs)
        x)
      x
      )

    ))

(defcheck solution-1cfd8d73
  (fn [& x0] (loop [x x0 m 0] (if (= x '()) m (recur (rest x) (if (> (first x) m) (first x) m))))))

(defcheck solution-1d79d7a0
  (fn [& nums]
    (reduce (fn [a b]
              (if (> a b) a b)) nums)))

(defcheck solution-1dbcdf4
  (fn maxi
    [& args]
    (reduce #(if (> %1 %2) %1 %2) args)
    ))

(defcheck solution-1dcac48f
  (fn [& xs] (last (sort xs))))

(defcheck solution-1dd6c472
  (fn [& col]
    (loop [col1 col v (first col)]
      (if (empty? col1)
        v
        (recur (rest col1) (if (< v (first col1) ) (first col1) v ) )
        )
      )
    ))

(defcheck solution-1e21ca36
  (fn [& parms]
    (
     (fn mymax [x curmax]
       (if (= (count x) 0)
         curmax
         (if (> (first x) curmax)
           (mymax (rest x) (first x))
           (mymax (rest x) curmax)
           )
         )
       )
     parms
     (first parms)
     )
    ))

(defcheck solution-1e65af71
  (fn my-max [x & rest]
    (if (empty? rest)
      x
      (let [m (apply my-max rest)]
        (if (> x m) x m)))))

(defcheck solution-1f47744b
  (fn [& many]
    (loop [maxval 0 args many]
      (if (empty? args)
        maxval
        (recur (if (< maxval (first args)) (first args) maxval) (rest args)))
      )))

(defcheck solution-1ff38d15
  (fn [& params]
    (reduce (fn [res it]
              (if (> it res) it res)) -1 params)))

(defcheck solution-2055d883
  (fn [& c]
    (reduce (fn [a b] (if (> a b) a b)) c)))

(defcheck solution-212be85e
  (fn [& c] (reduce (fn [x y] (if (> y x) y x)) 0 c)))

(defcheck solution-235c00c8
  (fn [& args]
    (reduce #(if (> %2 %1) %2 %1) args)))

(defcheck solution-244404f5
  (fn m ([x] x)
    ([x y] (if (< x y) y x))
    ([x y & args]
     (apply m (if (< x y) y x) args))))

(defcheck solution-24721e59
  (fn mx [& args]
    (let [max2 (fn [a b] (if (> a b) a b))]
      (reduce max2 0 args))))

(defcheck solution-2475e899
  (fn [& numbers]
    (loop [m (first numbers) elements (rest numbers)]
      (if (empty? elements)
        m
        (if (< m (first elements))
          (recur (first elements) (rest elements))
          (recur m (rest elements))
          )
        )
      )
    ))

(defcheck solution-24d4f97b
  (fn [& args] (reduce #(if (> %2 %) %2 %) args)))

(defcheck solution-25039674
  (fn [& items]
    (let [mymax #(if (> %1 %2) %1 %2)]
      (reduce mymax items))))

(defcheck solution-252992c3
  #(reduce (fn [x y] (if (> x y) x y)) %& ))

(defcheck solution-25829237
  (fn myax [& args]
    (if (empty? (rest args))
      (first args)
      (apply myax
        (if (< (first args) (second args))
          (second args)
          (first args))
        (rest (rest args))))))

(defcheck solution-25845c6c
  (fn [& arg] (reduce (fn [a b] (if (> a b) a b)) arg)))

(defcheck solution-2642cc70
  (fn [& args]
    (reduce (fn [a b] (if (> b a) b a)) args)))

(defcheck solution-266edf0c
  (fn
    [& elems]
    (loop [iterator 0 maxval (nth elems 0)]
      (if (< iterator (count elems))
        (if (< maxval (nth elems iterator))
          (recur (inc iterator) (nth elems iterator))
          (recur (inc iterator) maxval))
        maxval)
      )
    ))

(defcheck solution-270ccd10
  (fn [x & xs] (reduce #(if (> %2 %1) %2 %1) x xs)))

(defcheck solution-283fca11
  (fn mx ([x] x)
    ([x y & zs]
     (if (> x y)
       (apply mx x zs)
       (apply mx y zs)))))

(defcheck solution-28e2f5e0
  (fn this
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (apply this (this x y) more))))

(defcheck solution-28e35fb4
  (fn [& l]
    (reduce
      (fn [a b]
        (if (> a b)
          a
          b))
      l)))

(defcheck solution-29671f7d
  (fn [& x]
    (loop [m 0 x x]
      (if (empty? x)
        m
        (recur (if (> (first x) m)
                 (first x)
                 m)
          (rest x))))))

(defcheck solution-297c6660
  (fn[& a] (reduce (fn [x y] (if (> x y) x y)) a)))

(defcheck solution-29e4abd7
  (fn [& args] (last (sort args))))

(defcheck solution-2a011bae
  (fn [ & args]
    (reduce (fn [a b] (if (> a b) a b)) args)))

(defcheck solution-2a670aa8
  #(letfn [(m [a b] (if (< a b) b a))] (reduce m %&)))

(defcheck solution-2b4d3a9a
  (fn [& xs]
    (reduce (fn [a b] (if (> a b) a b)) xs)))

(defcheck solution-2c130c92
  (comp last sort vector))

(defcheck solution-2ce9d9d
  (fn [& x]
    (reduce #(if (> % %2) % %2) 0 x)))

(defcheck solution-2cfe9745
  (fn [x & y]
    (last (sort compare (cons x y)))))

(defcheck solution-2cff3441
  (fn[f & r](reduce (fn[a b](if (> a b) a b)) f r)))

(defcheck solution-2d8bbbf8
  (fn my-max [& xs]
    (reduce #(if (> % %2) % %2) xs)))

(defcheck solution-2dfb6814
  (fn [& li] (first (sort #(compare %2 %1) li))))

(defcheck solution-2e520741
  (fn [ & xs ]
    (reduce #(if (> %1 %2) %1 %2) xs)))

(defcheck solution-2ea10da1
  (fn [& coll]
    (reduce (fn [a b] (if (> a b) a b)) coll)
    ))

(defcheck solution-2eb397ee
  (fn [& args] (reduce #(if (> %2 %1) %2 %1) args)))

(defcheck solution-2ec9dc49
  (comp first (partial sort >) list))

(defcheck solution-2ede0a
  (fn hey2 [& args] (if (empty? (rest args)) (first args) (apply hey2 (if (> (first args) (first (rest args))) (concat (list (first args)) (rest (rest args))) (rest args))))))

(defcheck solution-2f1db8ac
  (fn [& args] ( reduce #(if(> %1 %2) %1 %2) args )))

(defcheck solution-2f432b25
  (fn [& x] (* -1 (apply min (map #(* -1 %) x)))))

(defcheck solution-2f64d730
  (fn [& args] (first (sort > args))))

(defcheck solution-30216407
  (fn [& args]
    (reduce #(if (> % %2) % %2) args)))

(defcheck solution-30615347
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-307ecb9
  (fn [& args] (last (sort (apply conj () args)))))

(defcheck solution-30a81ba4
  (fn [& vs] (reduce (fn [x y] (if (> x y) x y)) vs)))

(defcheck solution-30b1bd50
  (fn mymax [ best & more ]
    (let [ contender (first more) ]
      (cond
        (empty? more) best
        (> contender best) (apply mymax contender (rest more))
        :else (apply mymax best (rest more))))))

(defcheck solution-30fa6b2d
  (fn [& l] (reduce #(if (< %1 %2) %2 %1) l)))

(defcheck solution-316ce5da
  #(reduce (fn [acc x] (if (> x acc) x acc) ) %&))

(defcheck solution-31ede7e3
  (fn [& args] (reduce (fn [m, x] (if (> m x) m x)) args)))

(defcheck solution-32193670
  (fn [& a] (reduce #(if (> %1 %2) %1 %2) 0 a)))

(defcheck solution-33a6b4f3
  (fn [& mycol] (reduce (fn [a b] (if (> a b) a b)) mycol)))

(defcheck solution-33e6a2dd
  (fn [ & r] (reduce #(if (> %1 %2) %1 %2) r)))

(defcheck solution-33f0957e
  (fn [& xs] (reduce (fn [l r] (if (< l r) r l)) xs)))

(defcheck solution-340c3030
  (fn [& num]
    (reduce (fn [x y] (if (> x y) x y)) num)))

(defcheck solution-34640a59
  (fn mx [x & ns]
    (if (or (empty? ns) (and (or (seq? (first ns)) (nil? (first ns))) (empty? (first ns))))
      x
      (if (seq? (first ns))
        (mx (if (> x (first (first ns))) x (first (first ns))) (next (first ns)))
        (mx (if (> x (first ns)) x (first ns)) (next ns))
        )
      )))

(defcheck solution-34d87e4a
  (fn [& s]
    (reduce #(if (> %1 %2) %1 %2) s)))

(defcheck solution-34eabfb3
  (fn [& args] (reduce (fn [x y] (if (> x y) x y)) (seq args))))

(defcheck solution-3526e304
  (comp (partial reduce (fn [acc b] (if (< acc b) b acc))) vector))

(defcheck solution-3540cb2d
  (fn [& xs]
    (reduce (fn [mx x] (if (> x mx) x mx)) xs)))

(defcheck solution-3569d1e7
  (fn [& a] (last (sort a))))

(defcheck solution-3650467
  (comp (partial reduce #(if (> % %2) % %2)) list))

(defcheck solution-36d0ea58
  (fn [x & xs] (reduce #(if (< % %2) %2 %) x xs)))

(defcheck solution-3744c1bc
  (fn max-1 [x & more]
    (if (empty? more)
      x
      (if (> x (first more))
        (apply max-1 x (rest more))
        (apply max-1 (first more)
          (rest more))))))

(defcheck solution-37a7e635
  (fn [& s] (reduce (fn [accum x] (if (> x accum) x accum)) s)))

(defcheck solution-37c731a5
  (let [q
        (fn [best x]
          (if (nil? x)
            best
            (if (> (first x) best)
              (recur (first x) (next x))
              (recur best (next x))
              )
            )
          )] (fn [& z] (q (first z) z))))

(defcheck solution-37e5c9c0
  (fn [& args]
    ((fn maxof [coll result]
       (if-let [[f & r] (seq coll)]
         (if (or (nil? result) (> f result))
           (maxof r f)
           (maxof r result))
         result)) args nil)))

(defcheck solution-381c3d1c
  (fn [& values]
    (loop [m -2147483648 v values]
      (if (empty? v)
        m
        (recur (if (< m (first v)) (first v) m) (rest v))))))

(defcheck solution-3873e6bf
  (fn maxx [& args]
    (if (= 1 (count args))
      (first args)
      (let [mx (apply maxx (rest args))]
        (if (> mx (first args))
          mx
          (first args))))))

(defcheck solution-38c8b36c
  ;(fn [& x] (last (sort x)))
  #(last (sort %&)))

(defcheck solution-38e3adbc
  (fn mymax [& ls]
    (loop [l ls curmax 0]
      (if (empty? l)
        curmax
        (if (> (first l) curmax)
          (recur (rest l) (first l))
          (recur (rest l) curmax))))))

(defcheck solution-38ff4f0a
  (fn m
    ([a] a)
    ([a & more] (let [b (apply m more)]
                  (if (> a b)
                    a
                    b)))))

(defcheck solution-39134d42
  (fn [& xs](first (sort > xs))))

(defcheck solution-392326f0
  (fn m [& a] (loop [[h & t] a acc 0] (if (= t nil) acc (recur t (if (> h acc) h acc))))))

(defcheck solution-39307ca6
  (fn f [ & args](last (sort args))))

(defcheck solution-395980d1
  (fn [& arguments] (reduce (fn [x, y] (if (> x y) x y)) arguments)))

(defcheck solution-39b758d5
  (fn [& v]
    (loop [col  v
           res  (first v) ]
      (if (empty? col)
        res
        (let [ nr   (if (> res (first col)) res (first col))]
          (recur (rest col) nr))))))

(defcheck solution-3a5da8e1
  #(reduce (fn [m n] (if (> m n) m n)) %&))

(defcheck solution-3ac306fc
  (comp (partial reduce #(if (>= %1 %2) %1 %2)) vector))

(defcheck solution-3b01e4e5
  (comp last sorted-set))

(defcheck solution-3b1ada2d
  (fn my-max
    [& args]
    (loop [curlist args
           curmax (first args)]
      (let [[h & t] curlist]
        (cond
          (empty? curlist) curmax
          (> h curmax) (recur t h)
          :else (recur t curmax)
          ))
      )
    ))

(defcheck solution-3b317433
  #(last (sort %&)))

(defcheck solution-3b6a817c
  #(last (sort (apply conj '() %&))))

(defcheck solution-3cb6ace1
  (fn alt-max [& numbers]
    (loop [maximum (first numbers) items (rest numbers)]
      (cond
        (zero? (count items)) maximum
        (> (first items) maximum) (recur (first items) (rest items))
        :else (recur maximum (rest items))
        )
      )
    ))

(defcheck solution-3d8f4a14
  (fn [& input]
    (reduce #(if (> %1 %2) %1 %2) input)
    ))

(defcheck solution-3dbab8c7
  (fn [& nums] (reduce #(if (> %1 %2) %1 %2 )  nums ) ))

(defcheck solution-3e333858
  (fn my-max [& r]
    (loop [ret 0 r r]
      (if-not (seq r)
        ret
        (if (> (first r) ret)
          (recur (first r) (rest r))
          (recur ret (rest r)))))))

(defcheck solution-3e36f62f
  (fn [& nums]
    (reduce (fn [prev-max x] (if (> x prev-max) x prev-max)) (first nums) (rest nums))))

(defcheck solution-3e45b024
  (fn [x & xs]
    (reduce
      (fn mmax [a b]
        (if (> a b) a b)
        )
      (cons x xs)
      )
    ))

(defcheck solution-3e8389e7
  (fn [& x] (reduce (fn [a b] (if (> a b) a b)) x)))

(defcheck solution-3ec2b917
  #(->> %& (reduce (fn [x y] (if (< x y) y x)))))

(defcheck solution-3f4a9bed
  #(reduce (fn f[m v] (if (> m v) m v)) %&))

(defcheck solution-3fc1067a
  (fn [& x] (reduce #(if (> %1 %2) %1 %2) (vec x))))

(defcheck solution-40401e0c
  (fn [& args]
    (- (apply min (map (partial * -1) args)))))

(defcheck solution-40595cb7
  (fn [& n] (let [[x & xs] n] (if (empty? n) '() (reduce (fn [a b] (if (< a b) b a)) x xs)))))

(defcheck solution-40797d87
  (fn [x & more] (reduce #(if (< %1 %2) %2 %1)  x more)))

(defcheck solution-40c3de34
  (fn [& l] (reduce #(if (> %1 %2) %1 %2) (first l) l)))

(defcheck solution-40cddbcb
  (fn [& args] (reduce (fn [x y] (if (> y x) y x)) args)))

(defcheck solution-40d7d8ef
  (fn [& vs] (reduce #(if (> %1 %2) %1 %2) 0 vs)))

(defcheck solution-41aaff6b
  (fn [& nums] (reduce #(if (> %1 %2) %1 %2) nums)))

(defcheck solution-42906cbb
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) (first args) args)))

(defcheck solution-42fd0662
  (fn new-max [x & more]
    (last (sort (cons x more)))))

(defcheck solution-43293083
  (fn [& more]
    (reduce (fn [a b] (if (> a b) a b)) more)))

(defcheck solution-433b45d0
  (fn [& xs]
    (reduce #(if (< %1 %2) %2 %1) xs)))

(defcheck solution-43444a2e
  #(reduce (fn [a b] (if (> a b) a b)) 0 %&))

(defcheck solution-43e656a9
  (comp last sort (partial conj '())))

(defcheck solution-43f3cf8f
  (fn max' [& xs]
    (reduce #(if (< %1 %2) %2 %1) xs)))

(defcheck solution-4424355
  (fn [a & col]
    (reduce #(if (> %2 %1) %2 %1)  a col)
    ))

(defcheck solution-44502950
  (fn [& coll]
    (reduce #(if (> % %2) % %2) coll)))

(defcheck solution-447c450e
  (fn [x & more] (letfn [(f [x acc] (if (empty? x) acc (if (> (first x) acc) (f (rest x) (first x)) (f (rest x) acc))))] (f more x))))

(defcheck solution-44cc05f2
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-451d8fc6
  (fn [& x] (reduce #(if (> %1 %2) %1 %2) (first x) x) ))

(defcheck solution-452f4139
  #(-> %& sort last))

(defcheck solution-45c79609
  (fn [x & more] (reduce #(if (> %2 %) %2 %) x more)))

(defcheck solution-45d82750
  (fn mx
    ([val] val)
    ([v1 v2 & rest]
     (if (> v1 v2)
       (apply mx (conj rest v1))
       (apply mx (conj rest v2))))))

(defcheck solution-45efea3f
  (fn [& s] (reduce (fn [a b] (if (> a b) a b)) s)))

(defcheck solution-45f50373
  (fn [& s]
    (reduce #(if (> %1 %2) %1 %2) ##-Inf s)))

(defcheck solution-463b6358
  (fn [& x]
    (let [coll (seq x)]
      (reduce #(if (> %1 %2) %1 %2) x))))

(defcheck solution-4667b8ea
  (fn [& xs] (first (sort > xs))))

(defcheck solution-46fc88ce
  (fn [& args]
    (reduce
      (fn [x y]
        (if (< x y) y x))
      args)))

(defcheck solution-4708157
  (fn [& args] (reduce #(if (> %1 %2) %1 %2 ) 0 args )))

(defcheck solution-47182ca7
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-47417606
  (fn mx [fst & leftover]
    (reduce
      (fn [curmax to-con]
        (if (> to-con curmax)
          to-con
          curmax)) fst leftover)))

(defcheck solution-47548eb9
  (fn [& x] (last (sort x))))

(defcheck solution-476bf4f8
  (fn [x y & z] (if (= x 30) x y)))

(defcheck solution-4773d82d
  (fn [& nums]
    (reduce (fn [x y] (if (> x y) x y)) nums)))

(defcheck solution-478b84cc
  (fn [init & more]
    (reduce #(if (> %1 %2) %1 %2) init more)))

(defcheck solution-47c86200
  (fn [& args] (reduce (fn [x y] (if (> x y) x y)) 0 args)))

(defcheck solution-47d8d131
  (fn my-max
    [& args]
    (reduce (fn [n1 n2] (if (< n1 n2) n2 n1)) 0 args)))

(defcheck solution-483a0761
  (fn [& args] (reduce (fn [a b] (if (> b a) b a)) -9999 args)))

(defcheck solution-483d09a1
  (fn [& x] (reduce (fn [y z] (if (> y z) y z)) 0 x)))

(defcheck solution-484e0f6
  (fn [f & r] (reduce #(if (> %1 %2) %1 %2) f r)))

(defcheck solution-48723d12
  (fn [& all] (reduce (fn [x y] (if (> x y) x y)) all)))

(defcheck solution-488072f5
  (fn [i & items]
    (reduce #(if (< %1 %2) %2 %1) i items)))

(defcheck solution-48bf3cce
  (fn [& a](reduce #(if (< % %2) %2 %1) a)))

(defcheck solution-49123232
  (fn [& s] (-> s sort last)))

(defcheck solution-49295088
  (fn [& l] (case (count l) 0 nil 1 (first l) (recur (cons (if (> (first l) (second l)) (first l) (second l)) (drop 2 l))))))

(defcheck solution-493a4c8a
  #(reduce (fn [a b] (if (> a b) a b)) (into [] %&)))

(defcheck solution-49a5130c
  #(last (sort (vec %&))))

(defcheck solution-49ab0d37
  (fn [a & input] (reduce #(if (> %1 %2) %1 %2) a input)))

(defcheck solution-49d75274
  (fn mx [& args]
    (if (= 1 (count args))
      (first args)
      (if (> (first args) (second args))
        (apply mx (conj (rest (rest args)) (first args)))
        (apply  mx (rest args))))))

(defcheck solution-49e93b87
  (fn !
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (reduce ! (! x y) more))))

(defcheck solution-4a3abf1a
  (fn [& input] (reduce #(if (> %1 %2) %1 %2) input)))

(defcheck solution-4a8085d
  (fn [& args] (reduce (fn [a e] (if (> e a) e a)) args)))

(defcheck solution-4a8e827c
  (fn [& args]
    (reduce (fn [a b] (if (< a b) b a)) args)))

(defcheck solution-4bda784a
  (fn [x & more]
    (reduce #(if (> %1 %2) %1 %2) x more)))

(defcheck solution-4bff6888
  (fn [& seq]
    (- (apply min (map #(- %) seq)))))

(defcheck solution-4c12a97e
  (fn [& xs] (reduce #(if (> %2 %1) %2 %1) 0 xs)))

(defcheck solution-4c2a7d9a
  (fn [& l]
    (reduce (fn [x y] (if (> x y) x y)) l)))

(defcheck solution-4c8b7e18
  (fn [& args]
    (loop [mx (first args) rst (rest args)]
      (cond
        (empty? rst) mx
        (> (first rst) mx) (recur (first rst) (rest rst))
        :else (recur mx (rest rst))))))

(defcheck solution-4ca780e4
  (fn [& xs]
    (reduce (fn [m v]
              (if (> v m)
                v
                m))
      xs)))

(defcheck solution-4dcb0d2
  (comp last #(sort %&)))

(defcheck solution-4dfb925e
  (fn my-max2
    [& args]
    (reduce (fn
              [val1 val2]
              (if (> val1 val2)
                val1
                val2)) args)))

(defcheck solution-4e3d31aa
  (fn [& el] (reduce (fn [x y] (if (<= x y) y x)) el)))

(defcheck solution-4e72b50b
  (fn [& args] (reduce (fn [x y] (if (> x y) x y) ) args ) ))

(defcheck solution-4f1c377b
  (fn [& vals]
    (reduce
      #(if (< %1 %2) %2 %1)
      (first vals)
      (rest vals))))

(defcheck solution-4f5cdbc3
  (fn [& coll] (reduce #(if (> %1 %2) %1 %2) coll)))

(defcheck solution-4fd98900
  (comp last sort conj) [])

(defcheck solution-515a1961
  (fn my-max [g & n]
    (cond
      (empty? n) g
      (< g (first n)) (apply my-max (first n) (rest n))
      :else (apply my-max g (rest n))
      )
    ))

(defcheck solution-51939636
  (fn
    [& args]
    (reduce #(if (> % %2) % %2) args)))

(defcheck solution-5258a5cb
  (fn [ & nums] (reduce (fn [m v] (if (> v m) v m)) nums)))

(defcheck solution-52ac5448
  #(reduce (fn [mx el] (if (> el mx) el mx)) 4.9E-324 %&))

(defcheck solution-536395f2
  (fn [& coll]
    (reduce
      (fn [x y]
        (if (> x y) x y)) coll)))

(defcheck solution-53cc98e4
  (fn [& args]
    (last
      (sort args))))

(defcheck solution-53e135b7
  (fn [& args]
    (reduce (fn [a b] (if (> a b) a b)) args)))

(defcheck solution-550ad195
  (fn f [& more] (first (sort > (mapcat list more)))))

(defcheck solution-550b1de
  (fn [& args] (reduce (fn [x, y] (if (> x y) x y)) args)))

(defcheck solution-55440f51
  (fn biggest
    ([a] a)
    ([a b] (if (> a b) a b))
    ([a b & more] (reduce biggest (biggest a b) more))))

(defcheck solution-557dee46
  (fn [f1 & r1]
    (reduce #(if (< % %2) %2 %) f1 r1)
    ))

(defcheck solution-559fe150
  (fn [& params] (reduce (fn [a b] (if (< a b) b a)) params)))

(defcheck solution-560f7316
  (fn max*
    ([] nil)
    ([x & xs] (reduce (fn [acc x] (if (< acc x) x acc)) x xs))))

(defcheck solution-565ca264
  (fn [& a] (reduce #(if (> %2 %) %2 %) a)))

(defcheck solution-56f5b109
  (fn [m & n]
    (loop [m m
           n n]
      (cond
        (empty? n) m
        (> m (first n)) (recur m (rest n))
        :else (recur (first n) (rest n))))))

(defcheck solution-5783408
  (fn[& q] (reduce #(if (> % %2) % %2) q)))

(defcheck solution-57c08bad
  (fn [& xs] (reduce #(if (> % %2) % %2) xs)))

(defcheck solution-58a0c13a
  #(- (apply min (map - %&))))

(defcheck solution-58fafd93
  (fn maxX [& s] (reduce (fn [x y] (if (> x y) x y)) s)))

(defcheck solution-5913c5b3
  (fn [& r] (last (sort r))))

(defcheck solution-5a72c2ff
  (fn [& n] (reduce #(if (> %1 %2) %1 %2) n)))

(defcheck solution-5ad3a388
  (let [greater #(if (> %1 %2) %1 %2)]
    (fn [n & args]
      (loop [best n tail args]
        (if (empty? tail)
          best
          (recur (greater best (first tail)) (rest tail))
          )
        )
      )
    ))

(defcheck solution-5ada8014
  (fn [& args]
    (reduce (fn [m v] (if (> v m) v m)) -1 args)))

(defcheck solution-5ae8119c
  (fn [& params] (reduce #(if (> %2 %1) %2 %1) params)))

(defcheck solution-5b5aae03
  (fn [a & others] (loop [s others
                          new-max a]
                     (if (empty? s)
                       new-max
                       (let [next-num (first s)]
                         (recur (rest s) (if (> next-num new-max) next-num new-max)))))))

(defcheck solution-5bf190bd
  (fn [& x] ( reduce (fn [acc item] (if (> item acc) item acc)) x )))

(defcheck solution-5c8b5b7e
  (fn [& xs]
    (reduce #(if (> % %2) % %2) 0 xs)))

(defcheck solution-5cab4066
  (fn [& coll]
    (first (reverse (sort coll)))))

(defcheck solution-5cbae8b5
  (fn [ & xs] (last (apply sorted-set xs))))

(defcheck solution-5cde2c26
  (fn maks [& args]
    (reduce (fn [x1 x2] (if (> x1 x2) x1 x2)) args)))

(defcheck solution-5cf7b77c
  (fn [& s]
    (loop [s_ s mx nil]
      (if-not s_
        mx
        (recur
          (next s_)
          (let [a (first s_)]
            (if (or (not mx) (> a mx))
              a
              mx)))))))

(defcheck solution-5d09cef9
  (fn [& s]
    (reduce
      #(if (> %2 %1) %2 %1)
      0
      s)))

(defcheck solution-5d1e81c
  (fn [a b & more]
    (if (nil? b)
      a
      (recur (if (> a b) a b) (first more) (rest more)))))

(defcheck solution-5d90735d
  (fn m
    ([] nil)
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & rest] (let [go (fn [m [f & r :as list]] (if (empty? list) m (recur (if (> f m) f m) r)))]
                    (go (m x y) rest)))
    ))

(defcheck solution-5df0d6fc
  (fn
    [& args]
    (reduce (fn [a b] (if (> a b) a b)) args)
    ))

(defcheck solution-5e174f82
  (fn my-max [m0 & ms]
    (cond (empty? ms) m0
          (< m0 (first ms)) (apply my-max ms)
          :else (apply my-max (cons m0 (rest ms))))))

(defcheck solution-5e78977b
  (fn [& ss] (reduce #(if (> %1 %2) %1 %2) ss)))

(defcheck solution-5f4b1f52
  (fn [& l] (reduce #(if (< %1 %2) %2 %1) (first l) (rest l))))

(defcheck solution-5f84f765
  (fn [& ns]
    (reduce (fn [a b] (if (> a b) a b)) 0 ns)))

(defcheck solution-5fa1bb1
  (fn [& coll]
    (reduce #(if (> %1 %2) %1 %2) (first coll) (next coll))))

(defcheck solution-5fbd45e7
  (fn [& xs] ((identity reduce) (fn [x y] (if (< x y) y x)) xs)))

(defcheck solution-5fe74d9e
  (fn custom-max [& args]
    (loop [current (first args) inputs (rest args)]
      (if (= (first inputs) nil)
        current
        (recur (if (> (first inputs) current) (first inputs) current )
          (rest inputs))))))

(defcheck solution-5ff7e6e5
  (fn[& x] (first (reverse (sort x)))))

(defcheck solution-600be1d4
  (fn maxx([a b] (or (and (> a b) a) b)) ([a b & c](reduce maxx (maxx a b) c))))

(defcheck solution-60739891
  (fn [& lst]
    (loop [lst lst bigger 0]
      (cond (empty? lst) bigger
            (> (first lst) bigger) (recur (rest lst) (first lst))
            :else (recur (rest lst) bigger)))))

(defcheck solution-609f779b
  (fn [& args]
    (reduce
      (fn [x y] (if (> x y) x y))
      args
      )
    ))

(defcheck solution-615acd4e
  (fn [& args]
    (loop [seq args maximum 0]
      (if (empty? seq)
        maximum
        (if (> (first seq) maximum)
          (recur (rest seq) (first seq))
          (recur (rest seq) maximum)
          )
        )
      )
    ))

(defcheck solution-61ecb8ba
  (fn maxxy
    [& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-620d8eb6
  (fn maximumvalue [& x]
    (let [maxoftwo (fn [a b]
                     (if (> b a)
                       b
                       a))]
      (if (seq x)
        (reduce maxoftwo x)
        0))))

(defcheck solution-621e84e1
  (fn[& s](reduce(fn[x y](if(> x y)x y))s)))

(defcheck solution-6247eed4
  (fn ! ([x] (if (seq? x) (apply ! x) x))  ([x & more] (let [y (! more)] (if (> x y) x y)))))

(defcheck solution-624c9d9b
  (fn [x & args] (reduce (fn [a,b] (if (> a b) a b)) x args)))

(defcheck solution-6257df2e
  (fn [& args]
    (reduce #( if (> %1 %2) %1 %2) args)))

(defcheck solution-626f419d
  (fn [& args]
    (last (sort args))))

(defcheck solution-6375f7c3
  (fn new-max
    [& args]
    (reduce #(if (> %1 %2) %1 %2)
      (first args)
      (rest  args))))

(defcheck solution-6425ddd5
  #(last(sort %&)))

(defcheck solution-6431c107
  (fn [f & r]
    (reduce #(if (> %1 %2) %1 %2) f r)))

(defcheck solution-644d00f5
  (fn [& nums]
    (loop [mx 0, xs nums]
      (if (empty? xs)
        mx
        (if (> (first xs) mx)
          (recur (first xs) (rest xs))
          (recur mx (rest xs)))))))

(defcheck solution-64998a36
  (fn [& liste]
    (reduce (fn maxi [a b] (if (> a b) a b)) liste)
    ))

(defcheck solution-65497584
  (fn [& ns] (reduce #(if (> %1 %2) %1 %2) ns)))

(defcheck solution-660c35ec
  (fn [& numz]
    (reduce #(if (> %1 %2)
               %1
               %2) numz)))

(defcheck solution-663a1ea2
  (fn [& l] (reduce #(if (> %1 %2) %1 %2) l)))

(defcheck solution-6658873d
  (fn [x & xs]
    (reduce #(if (< %1 %2) %2 %1) x xs)))

(defcheck solution-6659fd50
  (fn [& lst]
    (reduce
      (fn [old new] (if (> new old) new old))
      0
      lst)))

(defcheck solution-6679d10c
  (fn [& s] (reduce #(if (> %1 %2) %1 %2) s)))

(defcheck solution-66bfb3d2
  (fn [& rest]
    "38. Write a function which takes a variable number of parameters and returns the maximum value."
    (last (apply sort (list rest)))))

(defcheck solution-67710ecb
  (fn mx [a & b]
    ( if (seq b) (apply mx (conj (rest b) (if (> a (first b)) a (first b)) ))
                 a)))

(defcheck solution-6841b8c7
  (fn [& args] (loop [r (rest args) m (first args)] (do (if (= r '()) m (if (> (first r) m) (recur (rest r) (first r)) (recur (rest r) m)))))))

(defcheck solution-6878ae45
  (fn [& seqn] (reduce #(if (> %2 %1) %2 %1) seqn)))

(defcheck solution-6880ce36
  (fn [f & xs]
    (reduce #(if (> %1 %2) %1 %2) f xs)))

(defcheck solution-68ee0817
  (fn [x & xs] (reduce #(if (> %1 %2) %1 %2) x xs)))

(defcheck solution-691c8b16
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-695fd981
  (fn f [a & args]
    (loop [args args
           m a]
      (if (empty? args)
        m
        (recur (rest args) (if (> (first args) m) (first args) m))
        )
      )
    ))

(defcheck solution-69998b89
  (fn [& xs] (reduce (fn [x y] (if (> x y) x y)) xs)))

(defcheck solution-6a167a5c
  (fn [x y & more]
    (reduce #(if (< %1 %2) %2 %1) (cons x (cons y more)))))

(defcheck solution-6a87aac3
  (fn [& rest]
    (letfn [(max2 [x y] (if (> x y) x y))]
      (reduce max2 rest))))

(defcheck solution-6adc7559
  (fn [ & args ] (first (sort > args))))

(defcheck solution-6b29483
  (fn [& l] (reduce #(if (> %2 %) %2 %) l)))

(defcheck solution-6b3600c3
  (fn my-max
    ([x y]
     (if (> x y) x y))
    ([x y & more]
     (reduce my-max (my-max x y) more))))

(defcheck solution-6b8be738
  (fn [& x] (reduce (fn [x1 x2] (if (> x1 x2) x1 x2)) 0 x)))

(defcheck solution-6c79327a
  (fn [& args]
    (loop [coll2 args, result 0]
      (if (empty? coll2)
        result
        (recur
          (rest coll2)
          (let [x (first coll2)]
            (if (> x result)
              x
              result)))))))

(defcheck solution-6c79fe66
  (fn [& v]
    (reduce #(if (> % %2) % %2) v)))

(defcheck solution-6d22c04e
  (fn [& args] (reduce (fn [mmax n] (if (> n mmax) n mmax)) args)))

(defcheck solution-6d5cd2a6
  (fn mx[f & r]
    (reduce (fn[a b] (if (> a b) a b)) f r)))

(defcheck solution-6d92eb89
  (comp #(reduce (fn [x y] (if (> x y) x y)) %) list))

(defcheck solution-6daecb84
  (fn [& args]
    (reduce
      (fn [a b] (if (< a b) b a))
      (first args)
      (rest args)
      )))

(defcheck solution-6dc28f64
  (fn [& vals] (reduce #(if (> % %2) % %2) vals)))

(defcheck solution-6e621cd7
  (fn m[a b & more] (if (nil? more) (if (> a b) a b) (if (> a b) (apply m (conj more a)) (apply m (conj more b))))))

(defcheck solution-70296348
  (fn[x & resti]
    (loop [maxi x  y resti]
      (if  (and (>= (count y) 2) (<= maxi (first y))) (recur (first y) (rest y)) maxi)
      )
    ))

(defcheck solution-70dcbff1
  (fn my-max [ x & xs ]
    (if (= xs nil)
      x
      (let [z (apply my-max xs)]
        (if (> x z) x z)))))

(defcheck solution-7135d9bd
  (fn [& ns]
    (reduce #(if (> %1 %2) %1 %2) ns)))

(defcheck solution-721a8ca8
  (fn [& entries] (reduce #(if (< % %2) %2 %) entries)))

(defcheck solution-72448beb
  (fn myMax [& args] (reduce #(if (>= %1 %2) %1 %2) args)))

(defcheck solution-7253b509
  (fn [& arg ] (last (sort arg))))

(defcheck solution-7280a9dc
  (fn [& values] (reduce (fn [x y] (if (> x y) x y)) -2147483648 values)))

(defcheck solution-72e1053b
  (fn [& rest] (reduce #(if (> %1 %2) %1 %2) rest)))

(defcheck solution-733589f0
  (fn [& args] (reduce (fn[a b](if(> a b) a b)) args)))

(defcheck solution-7383967c
  (fn [& x]
    (reduce #(if (> % %2) % %2) x)))

(defcheck solution-753b1119
  (fn [x & other] (reduce #(if(> %1 %2) %1 %2) x other)))

(defcheck solution-757e7565
  (fn [& coll]
    (reduce (fn [result x] (if (> x result) x result)) coll)))

(defcheck solution-75af8b60
  (fn [v & vs]
    (loop [m v vs vs]
      (cond
        (empty? vs) m
        (> m (first vs)) (recur m (rest vs))
        :default (recur (first vs) (rest vs))))))

(defcheck solution-7606d9d1
  #(reduce (fn [x y] (if (> x y) x y)) %&))

(defcheck solution-76246301
  (fn [& nums]
    (reduce #(if (< %1 %2) %2 %1) nums)
    ))

(defcheck solution-764ec096
  (fn [& more]
    (reduce (fn [x y ] (if (> x y ) x y)) more)
    ))

(defcheck solution-76a752c6
  (fn [& n] (last (sort n))))

(defcheck solution-76dfa859
  (fn [& args]
    (reduce (fn [x y] (if (> x y) x y)) args)))

(defcheck solution-77178e18
  (fn [& args] (reduce (fn [acc x] (if (> x acc) x acc)) 0 args) ))

(defcheck solution-777a3c
  (fn [& args] (loop [[head & tail] args
                      acc 0]
                 (if (nil? head)
                   acc
                   (recur tail (if (> head acc) head acc))
                   )
                 )
    ))

(defcheck solution-7784ea69
  (fn [& vs] (reduce (fn [a b] (if (< a b) b a)) vs)))

(defcheck solution-783899c1
  (fn my-max
    [& args]
    (loop [s args
           m (first args)]
      (if (empty? s)
        m
        (recur (rest s) (if (> (first s) m) (first s) m))))))

(defcheck solution-7867647c
  (fn [& l] (reduce #(if (> %2 %1) %2 %1) l)))

(defcheck solution-78c7373b
  (fn [& s] (- (reduce min (map #(- %) s)))))

(defcheck solution-79505c14
  (fn [& params] (reduce #(if (> % %2) % %2) params)))

(defcheck solution-79ac0120
  (fn [& args] (reduce (fn [x y] (if (> x y) x y)) args)))

(defcheck solution-79b6df40
  (fn [& vals] (reduce (fn [m n] (if (> n m) n m)) vals)))

(defcheck solution-79e78327
  (fn [& n]
    (reduce #(if (> %1 %2) %1 %2) n)))

(defcheck solution-7a1c03d6
  #(last(apply sorted-set %&)))

(defcheck solution-7a5e4a10
  (fn [& v] (reduce #(if (> % %2) % %2) 0 v)))

(defcheck solution-7a8d01ca
  (fn [& xs]
    (reduce #(if (> %2 %1) %2 %1)
      (first xs) (next xs))))

(defcheck solution-7aaed1b6
  (fn [& nums] (reduce (fn [x y] (if (> x y) x y)) nums)))

(defcheck solution-7b3db284
  (fn [ & more ] (reduce #(if (> %1 %2) %1 %2) 0 more)))

(defcheck solution-7b7e2e5d
  (fn [& rest] (reduce #(if (> % %2) % %2) rest)))

(defcheck solution-7b99d90d
  (fn [& m] (last (sort m))))

(defcheck solution-7ceed273
  (fn [& c]
    (reduce (fn [acc e]
              (if (>= acc e) acc e)) c)))

(defcheck solution-7dce377e
  (fn [& r] (reduce #(if (> %2 %) %2 %) r)))

(defcheck solution-7e896b10
  (fn [& xs]
    (reduce (fn [x y]
              (if (> x y) x y)) xs)))

(defcheck solution-7ead0b14
  (fn [& args]
    (reduce (fn [x y] (if (> x y) x y)) (first args) args)))

(defcheck solution-7ec578ec
  (fn [& nums] (reduce (fn [m e] (if (> e m) e m)) nums)))

(defcheck solution-7f1c8ee9
  (fn [& c] (reduce #(if (> %1 %2) %1 %2) c)))

(defcheck solution-7f2ffb0e
  #(last (sort (first (list %&)))))

(defcheck solution-7f566a85
  (fn [& xs] (reduce #(if (< %1 %2) %2 %1) xs)))

(defcheck solution-7f701995
  (fn [& L] (reduce (fn [lhs rhs] (if (> lhs rhs) lhs rhs)) L)))

(defcheck solution-800b953
  (fn [& lst]
    (loop [lst lst m 0]
      (cond
        (empty? lst) m
        (> (first lst) m) (recur (rest lst) (first lst))
        :else (recur (rest lst) m)))))

(defcheck solution-803df085
  (fn [& args] (loop [tmp 0 ls args] (if (empty? ls) tmp (recur (if (> (first ls) tmp) (first ls) tmp) (rest ls))))))

(defcheck solution-809ed5f1
  (fn [& more] (reduce #(if(> %1 %2) %1 %2 ) more)))

(defcheck solution-80db000b
  (fn [& more]
    (reduce #(if (> %1 %2) %1 %2) 0 more)))

(defcheck solution-8142835f
  (fn [& more]
    (loop [maxi (first more)
           curr (rest more)]
      (if (empty? curr)
        maxi
        (if (> (first curr) maxi)
          (recur (first curr) (rest curr))
          (recur maxi (rest curr)))))))

(defcheck solution-814acbab
  (fn my-max [first-el & other]
    ((fn pass-max [curr-max a-seq]
       (let [seq-first (first a-seq)]
         (if (= nil seq-first)
           curr-max
           (if (> curr-max seq-first)
             (pass-max curr-max (rest a-seq))
             (pass-max seq-first (rest a-seq))
             )
           )
         )) first-el other)
    ))

(defcheck solution-815cf70e
  (fn [& params] (reduce (fn [inj, num] (if (< inj num) num inj)) (vec params))))

(defcheck solution-819a70c0
  (fn [x y & n]
    (let [m #(if (> % %2) % %2) ]
      (reduce m (m x y) n)
      )
    ))

(defcheck solution-81bd206f
  (fn [a & b]
    (if (next b)
      (if (> a (first b)) (recur a (next b)) (recur (first b) (next b)))
      (if (> a (first b)) a b))))

(defcheck solution-834289d
  (fn m [& args]
    (if (= 1 (count args))
      (first args)
      (let [a (first args)
            b (apply m (rest args))]
        (if (> a b) a b)))))

(defcheck solution-83bdde73
  (fn [& xs]
    (loop [current (first xs) to-check (next xs)]
      (if (= to-check nil)
        current
        (let [[head & tail] to-check]
          (recur (if (> head current) head current) tail))))))

(defcheck solution-8416634f
  (fn [& args]
    (reduce
      (fn [x y]
        (if (> x y)
          x
          y
          )
        )
      args
      )
    ))

(defcheck solution-85b8e3c3
  (fn [& args] (last (apply sort (list args)))))

(defcheck solution-8629461c
  (fn[& s] (reduce #(if (> %1 %2) %1 %2) s)))

(defcheck solution-86a99226
  (fn [& args] (reduce (fn [y x] (if (> x y) x y)) args)))

(defcheck solution-86e217fd
  (fn [& x] (reduce #(if (> %1 %2) %1 %2) x)))

(defcheck solution-86fe309
  (fn [& items] (reduce #(if(> % %2) % %2) items)))

(defcheck solution-8761abdf
  (fn [& args]
    (last (sort args))))

(defcheck solution-8769625f
  (fn [x & xs]
    (letfn [(mx [curr-max xs]
              (if (empty? xs)
                curr-max
                (let [curr-num (first xs)]
                  (recur
                    (if (> curr-num curr-max) curr-num curr-max)
                    (rest xs)))))]
      (mx x xs))))

(defcheck solution-876e022d
  (fn [& s] (reduce #(if (> %2 %) %2 %) s)))

(defcheck solution-878683ae
  (fn mx [& nums]
    (let [fst (first nums)]
      (loop [[x & xs] nums
             mx-so-far fst]
        (if (nil? x)
          mx-so-far
          (recur xs
            (if (> x mx-so-far)
              x
              mx-so-far)))))))

(defcheck solution-87e91aa3
  (fn [& args] (reduce  ( fn [x y] (if (> x y ) x y))  args)))

(defcheck solution-8802a2c8
  (fn nmax [& xs] (reduce #(if (> %1 %2) %1 %2) xs)))

(defcheck solution-88035c0f
  (fn [& s]
    (reduce (fn [a b] (if (> a b) a b)) s)))

(defcheck solution-887b8735
  (fn [& args] (reduce #(if (< %1 %2) %2 %1) args)))

(defcheck solution-88885af4
  (fn [& x] (reduce (fn [a b] (if (< a b) b a)) x)))

(defcheck solution-888edc43
  (fn [& args] (reduce (fn [m e] (if (> e m) e m)) args)))

(defcheck solution-88d68c3d
  (fn [a & list]
    (comment could be a one liner)
    (loop [m a
           list list]
      (let [head (first list)
            tail (rest list)]
        (if head
          (recur (if (> m head) m head) tail)
          m)))))

(defcheck solution-89542001
  (fn [& args] (reduce (fn [a b] (cond (> a b) a :else b)) args)))

(defcheck solution-8970247c
  (fn k [& l] (reduce #(if (< %1 %2) %2 %1) (first l) l)))

(defcheck solution-89b93936
  (fn
    [& args]
    (reduce (fn [a b] (if (> a b) a b)) args)))

(defcheck solution-8ba517d4
  (fn f [& args]
    (let [[a & b] args]
      (if b
        (if (>= a (first b))
          (apply f (cons a (rest b)))
          (apply f (cons (first b) (rest b))))
        a))))

(defcheck solution-8c31ee25
  (fn [& a] (reduce #(if (> % %2) % %2) a)))

(defcheck solution-8c564b6f
  (fn [& coll]
    (loop [result 0  c coll]
      (if (nil? c)
        result
        (recur (if (> (first c) result) (first c) result ) (next c))))))

(defcheck solution-8d1d670e
  (fn mmax
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more] (reduce mmax (mmax x y) more))))

(defcheck solution-8e29e32b
  (fn my-max [n & ns]
    (reduce #(if (> %1 %2) %1 %2) n ns)))

(defcheck solution-8e3d6fb3
  (fn [& t] (last (sort t))))

(defcheck solution-8e53d488
  (fn [& args] (reduce (fn [x y] (if (> x y)  x y)) args)))

(defcheck solution-8ea9a01
  (fn [& args]
    (first (sort > args))))

(defcheck solution-8f799097
  (fn [M & ms] (reduce #(if (> %1 %2) %1 %2) M ms)))

(defcheck solution-8fdc67eb
  (fn
    [& numbers]
    (reduce #(if (< %1 %2) %2 %1) numbers)))

(defcheck solution-90905bfa
  (fn [& coll]
    (let [max* (fn [x y] (if (> x y) x y))]
      (reduce max* coll))))

(defcheck solution-90e6adb0
  #(reduce (fn [t v] (if (> v t) v t)) %&))

(defcheck solution-90f2fc46
  (fn [& s] (last (sort s))))

(defcheck solution-913202c9
  (fn [& params]
    (loop [best (first params) the-rest (rest params)]
      (if (empty? the-rest)
        best
        (recur (if (> best (first the-rest))
                 best
                 (first the-rest))
          (rest the-rest))))))

(defcheck solution-91b0b340
  (fn [a & rest] (reduce (fn [a b] (if (> a b) a b)) a rest)))

(defcheck solution-92a0d689
  #(reduce (fn [x y] (if (< x y) y x)) %&))

(defcheck solution-92c8607b
  (fn [& args]
    (reduce #(if (< %1 %2) %2 %1) 0 args)))

(defcheck solution-93375ffe
  (fn [& a] (first (sort > a))))

(defcheck solution-937c2870
  (fn [x & y] (reduce #(if (> % %2) % %2) x y)))

(defcheck solution-9381176e
  (fn [& xs] (reduce #(if (> %1 %2) %1 %2) xs)))

(defcheck solution-93bdb85a
  (fn [& z]
    (reduce
      (fn [a b]
        (if (> a b)
          a
          b))
      z)))

(defcheck solution-943de9ce
  (fn [& A]
    (reduce #(if (< %1 %2) %2 %1)
      A)
    ))

(defcheck solution-9485ed7d
  (fn redmax [& coll]
    (reduce #(if (> %1 %2) %1 %2 ) coll)))

(defcheck solution-94a194ff
  (fn [& xs] (reduce #(if (> %1 %2) %1 %2) 0 xs)))

(defcheck solution-953c12c0
  (fn [& x]
    (reduce
      (fn [a b]
        (if (> a b) a b)
        )
      x
      )
    ))

(defcheck solution-955035ca
  (fn [& vals] (reduce #(if (> %1 %2) %1 %2) 0 vals)))

(defcheck solution-95ce6255
  (fn find-max
    [& input-seq]
    (let [sorted-input (into (sorted-set) input-seq)]
      (last sorted-input))))

(defcheck solution-95f7d842
  (fn [& args]
    (loop [coll args
           maximum (first coll)]
      (if coll
        (if (> (first coll) maximum)
          (recur (next coll) (first coll))
          (recur (next coll) maximum))
        maximum))))

(defcheck solution-963a5c1f
  (fn [& xs] (reduce (fn [a b] (if (> a b) a b)) xs)))

(defcheck solution-96ee798d
  (fn haha [dmax & frogs]
    (if frogs
      (if (> dmax (first frogs))
        (apply haha dmax (rest frogs))
        (apply haha (first frogs) (rest frogs)))
      dmax)))

(defcheck solution-977ed6e9
  #(reduce (fn [a b] (if (> a b) a b)) %&))

(defcheck solution-978b5642
  (fn [& l] (
             (fn me [l]
               (if (= (count l) 1) (first l)
                                   (if (> (first l) (me (rest l)))
                                     (first l)
                                     (me (rest l) )
                                     )
                                   )
               )
             l)))

(defcheck solution-981ce363
  (fn [& rst] (reduce #(if (> %1 %2) %1 %2) rst)))

(defcheck solution-99b39cc9
  (fn[& tail](last (sort tail))))

(defcheck solution-99bd2c0b
  (fn my-max
    ([a] a)
    ([a & b] (let [b (apply my-max b)] (if (> a b) a b)))))

(defcheck solution-99efc22b
  (fn [ & b ]
    (reduce #(if(> %1 %2)
               %1
               %2)
      b)))

(defcheck solution-9a2ef5b6
  (fn [& nums] (->> nums (into (sorted-set)) last)))

(defcheck solution-9a5d7530
  (fn mx
    ([x]
     x)
    ([x y] (if (> x y) x y))
    ([x y & others]
     (if (empty? others) (mx x y)
                         (if (> x y)
                           (recur x (first others) (rest others))
                           (recur y (first others) (rest others)))))))

(defcheck solution-9a6d316d
  (fn m [& args]
    (reduce (fn [y z] (if (> y z) y z)) args)))

(defcheck solution-9b01b577
  (fn max'
    ([x] x)
    ([x & xs]
     (let [m (apply max' xs)]
       (if (< x m) m x)
       )
     )
    ))

(defcheck solution-9c1fcec3
  (fn [& args]
    (reduce #(if (>= 0 (- %1 %2))
               %2
               %1)
      args)))

(defcheck solution-9c3be66d
  (fn mx [& args]
    (first (sort > args))))

(defcheck solution-9c614f7b
  (fn max-args [& args] (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-9d0eebc
  (fn [& seq] (- (apply min (map - seq)))))

(defcheck solution-9d3c6ba3
  (fn [& coll]
    (reduce #(if (< %1 %2) %2 %1) coll)))

(defcheck solution-9dbbf1a1
  (fn [a & b]
    (reduce #(if (> %1 %2) %1 %2) a b)))

(defcheck solution-9e1fa579
  (fn [& l] (reduce (fn [a b] (if (> a b) a b)) l)))

(defcheck solution-9e29166d
  (fn __ [& coll] (reduce #(if (> %1 %2) %1 %2) coll)))

(defcheck solution-9e4d1b24
  (fn [& x]
    (letfn [(cmx [a b] (if (> a b)  a  b))]
      (reduce cmx x))))

(defcheck solution-9edecb4f
  (fn [&  s] ((fn [s m] (if (empty? s) m (recur (rest s) (if (> (first s) m) (first s) m)) ) ) s 0) ))

(defcheck solution-9f0a233d
  (fn [& x]
    (loop [lst (rest x), m (first x)]
      (if (= 0 (count lst))
        m
        (recur (rest lst)
          (if (> m (first lst))
            m
            (first lst)))))))

(defcheck solution-9f5176f8
  (fn [& args ] (reduce (fn [acc x] (if (> x acc) x acc)) 0 args)))

(defcheck solution-9f6bebfc
  #(reduce (fn [x y] (if (< x y) y x)) %&
     ))

(defcheck solution-9f7a2af8
  (fn mx ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more] (apply mx (mx x y) more))))

(defcheck solution-9f7a7f33
  (fn max1 [& args] (last (sort args))))

(defcheck solution-9f86cd62
  (fn[& args]
    (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-a04737d5
  (fn [& a]
    (reduce #(if (> % %2) % %2) 0 a)))

(defcheck solution-a04bbe
  (fn [& args] (reduce #(if (<= %1 %2) %2 %1) args)))

(defcheck solution-a05b5d8e
  (fn [& xs] (reduce (fn [n m] (if (> n m) n m)) xs)))

(defcheck solution-a05d374f
  (fn [& args]
    (reduce (fn [x y] (if (> y x) y x)) args)))

(defcheck solution-a065f6fd
  (letfn [(mx ([x y] (if (> x y) x y))
            ([x y & more] (reduce mx (mx x y) more)))]
    mx))

(defcheck solution-a07eb3c
  (fn maximo-args [& args]
    ((fn maximo [maior coll]
       (cond
         (empty? coll) maior
         (> maior (first coll)) (maximo maior (next coll))
         :else (maximo (first coll) (next coll))))
     (first args) (next args))))

(defcheck solution-a0fe1717
  (fn [ & lst]
    (reduce (fn [r i] (if (> i r) i r))
      -1
      lst)))

(defcheck solution-a122c1b7
  (comp (partial reduce #(if (> %1 %2) %1 %2)) list))

(defcheck solution-a15848a7
  (fn [& s]
    (letfn [(tempt
              [f r myval]
              (if (nil? f)
                myval
                (recur (first r) (rest r) (if (> f myval) f myval))))]
      (tempt (first s) (rest s) 0)
      )))

(defcheck solution-a162a5f5
  (fn fmax [& args] (reduce (fn [x y] (if (> x y) x y)) args)))

(defcheck solution-a16ce733
  (fn mymax
    ([x] x)
    ([x & rest] (let [y (apply mymax rest)] (if (> x y) x y)))
    ))

(defcheck solution-a1750ab8
  (fn [& args] ((comp last sort) args)))

(defcheck solution-a2c7ec82
  (fn max-val [& thurks]
    (reduce #(if (< %1 %2) %2 %1) 0 thurks)))

(defcheck solution-a382660
  (fn [& xs]
    (reduce #(if (> %2 %1) %2 %1) xs)))

(defcheck solution-a39af82c
  (fn [& lst]
    (reduce (fn [a b] (if (> a b) a b)) lst)))

(defcheck solution-a4357363
  (fn max-val [x & xs]
    (reduce #(if (< %1 %2) %2 %1) (flatten (cons x xs)))))

(defcheck solution-a44089e7
  (fn [& s]
    (reduce (fn [x y] (if (> x y) x y)) 0 s)))

(defcheck solution-a4ac0676
  (fn [& l]
    (reduce (fn [v e] (if (nil? v) e (if (> v e) v e))) nil l)))

(defcheck solution-a4d34681
  (fn
    ([x] x)
    ([x & more] (reduce #(if (> %2 %1) %2 %1) x more))
    ))

(defcheck solution-a4f94132
  (fn my-max [& args]
    (reduce #(if (< %1 %2) %2 %1) args)))

(defcheck solution-a537b21a
  (fn mymax
    [& args]

    (reduce
      #(if (< %1 %2)
         %2
         %1)
      0
      args)

    ))

(defcheck solution-a55bd946
  (fn myMax
    [& arguments]
    "Consumes all the arguments using reduce,
    supplied with a function that returns the highest value from a pair"
    (reduce #(if (> %1 %2)
               %1
               %2) arguments)))

(defcheck solution-a59b3f1b
  (fn thisfunc [& args]
    (if (> (count args) 1)
      (let [n (first args) n2 (apply thisfunc (rest args))]
        (if (> n n2) n n2))
      (first args))))

(defcheck solution-a64323e7
  (fn [& foo]
    (last (sort foo))))

(defcheck solution-a6f91e52
  (fn [& x] (-> x sort last)))

(defcheck solution-a75e06a9
  (fn [& a] (reduce #(if (< % %2) %2 %) a)))

(defcheck solution-a7f653cf
  (fn [x & more] (letfn [ (m [ a b] ( if (> a b) a b)) ] (reduce m (cons x more)))))

(defcheck solution-a80f8b30
  (fn [& a] (reduce #(if (< %1 %2) %2 %1) a)))

(defcheck solution-a815ddf7
  #(reduce (fn [m v] (if (> v m) v m)) %&))

(defcheck solution-a89c1392
  (fn [& ns]
    (reduce #(if (< %1 %2) %2 %1) ns)))

(defcheck solution-a8bc07ea
  (fn [& xs] (reduce (fn [acc x] (if (> x acc) x acc)) xs)))

(defcheck solution-a92c810e
  (fn [& z] (reduce #(if (> %2 %1) %2 %1) z)))

(defcheck solution-a9354cc9
  (fn f ([a] a) ([a b & xs] (apply f (if (> a b) a b) xs))))

(defcheck solution-a9506326
  (fn [& args] (reduce (fn [my-max n] (if(> n my-max) n my-max)) -1 args)))

(defcheck solution-a98448a9
  (fn [& nums]
    (reduce #(if (> %1 %2) %1 %2) -2147483648 nums)))

(defcheck solution-a9b63a45
  (fn maxx [a & r]
    (if (= nil r)
      a
      (let [b (first r)
            m (if (> a b) a b)]
        (apply  maxx m (rest r))))))

(defcheck solution-aa111c35
  (fn mx [& xs]
    (reduce (fn [ret this]
              (if (> this ret)
                this
                ret)) 0 (seq xs))))

(defcheck solution-aaf8e04e
  (fn max-value
    ([x] x)
    ([x & more]
     (reduce #(if (> %1 %2) %1 %2) x more))))

(defcheck solution-aafcc4c6
  (fn [& xs] (reduce #(if (< % %2) %2 %) xs)))

(defcheck solution-ab093ef0
  #(reduce (fn [reducer, elem]
             (if (> elem reducer)
               elem
               reducer))
     (vec %&)))

(defcheck solution-abeba2be
  (fn [& args]
    (loop [mx 0
           args args]
      (if (empty? args)
        mx
        (if (> (first args) mx)
          (recur (first args) (rest args))
          (recur mx (rest args)))))))

(defcheck solution-acae9200
  (fn [& elts]
    (loop [current-max (first elts) rst (rest elts)]
      (if (empty? rst)
        current-max
        (recur (if (> (first rst) current-max)
                 (first rst)
                 current-max)
          (rest rst))))))

(defcheck solution-ad68a3
  (fn [& xs] (reduce (fn [x y] (if (< x y) y x)) xs)))

(defcheck solution-ad717825
  (fn max' [& xs]
    (let [x (first xs), xs' (next xs)]
      (if (nil? xs') x
                     (let [m' (apply max' xs')]
                       (if (> x m') x m'))))))

(defcheck solution-addef188
  (fn [& xv] (reduce (fn [a b] (if (< a b) b a)) xv)))

(defcheck solution-adfcbf4e
  (fn [& args]
    (reduce (fn [x y] (if (> x y) x y)) 0 args)))

(defcheck solution-aedc2614
  (fn [& params]
    (reduce #(if (> %2 %1) %2 %1) (first params) params)))

(defcheck solution-aefd8695
  #((comp last sort) %&))

(defcheck solution-af93ce48
  (fn my-max [& more] (reduce (fn [x y] (if (< x y) y x)) more)))

(defcheck solution-b1029c94
  (fn [& n] (reduce #(if (> %2 %1) %2 %1) n)))

(defcheck solution-b11e89f
  (fn [& x]
    (reduce #(if (> %1 %2) %1 %2) x)))

(defcheck solution-b1a16cb9
  (fn [& coll] (reduce #(if (>= %1 %2) %1 %2) coll)))

(defcheck solution-b1b10143
  (fn m
    ([n]
     n)
    ([n & r]
     (let [x (apply m r)]
       (if (> n x)
         n
         x)))))

(defcheck solution-b2833dfa
  #(->> %& sort last))

(defcheck solution-b38c1298
  (fn [& i] (reduce (fn [v r] (if (> v r) v r)) i)))

(defcheck solution-b39e905
  (fn [& xs] (reduce #(if (> % %2) % %2) xs)))

(defcheck solution-b4c36c04
  (fn [& xs]
    (loop [[a b & cs] xs]
      (let [bigger (if (> a b) a b)]
        (if (empty? cs)
          bigger
          (recur (concat [bigger] cs)))))))

(defcheck solution-b4c7e59
  (fn [& v] (reduce (fn [a b] (if (> a b) a b)) v)))

(defcheck solution-b5277302
  (fn [& args]
    (reduce (fn [a x] (if (> x a) x a)) 0 args)))

(defcheck solution-b63cbea0
  (fn [& s] (reduce (fn [x y] (if (> x  y) x y)) s)))

(defcheck solution-b65fa34d
  (fn [x & y] (reduce #(if (> %1 %2) %1 %2) x y)))

(defcheck solution-b6692c
  (fn [& tail]

    (reduce
      (fn [a b]
        (if (> a b) a b)
        )
      tail)
    ))

(defcheck solution-b6a9189e
  (fn max-reduce [x & more]
    (reduce (fn [acc y] {:pre [(number? y)]} (if (> y acc) y acc))
      x more)))

(defcheck solution-b6ec6a65
  (fn [& coll] (reduce (fn [c a] (if (> c a) c a)) coll)))

(defcheck solution-b7287bc
  (fn [& args] (reduce (fn [res, x] (if (> x res) x res)) 0 args)))

(defcheck solution-b79e86b7
  (fn [& x] (first (sort > x))))

(defcheck solution-b7d123eb
  (fn maks
    ([a b] (if (> a b) a b))
    ([a b & more] (reduce maks (maks a b) more))))

(defcheck solution-b7f0517e
  (fn [& m] (reduce #(if (> %1 %2) %1 %2) m)))

(defcheck solution-b91490a3
  (fn [& in]
    (loop [big 0 l in]
      (if
       (empty? l)
        big
        (recur
          (if
           (> (first l) big)
            (first l)
            big)
          (rest l))))))

(defcheck solution-b991dfe7
  #(reduce (fn [x y]
             (if (< x y)
               y
               x))
     %&))

(defcheck solution-ba5fdf0b
  (fn [& coll]
    (reduce #(if (> % %2) % %2) coll)))

(defcheck solution-bb41a698
  (fn [& m]
    (-> m sort last)))

(defcheck solution-bb6fbcab
  (fn my-max1
    ([x & z] (reduce (fn [x y] (if (> x y) x y)) x z))))

(defcheck solution-bbf0631c
  (fn [& n] (reduce (fn [x y] (if (> x y)
                                x
                                y)) n)))

(defcheck solution-bc058c2d
  (fn [& x] (reduce #(if (> %2 %1) %2 %1) x )))

(defcheck solution-bcb2436f
  (fn [& xs]
    (reduce (fn [memo el]
              (if (< memo el) el memo))
      (first xs)
      (rest xs))))

(defcheck solution-bd80748b
  (fn my-max [& numbers]
    (reduce (fn [a b]
              (if (> a b) a b))
      numbers)))

(defcheck solution-bd872deb
  (fn [& nums]
    (let [f (fn [coll max-val]
              (if (empty? coll)
                max-val
                (if (> (first coll) max-val)
                  (recur (rest coll) (first coll))
                  (recur (rest coll) max-val))))]
      (f nums (first nums)))))

(defcheck solution-bdd8d021
  (fn [& xs] (first (reverse (sort xs)))))

(defcheck solution-bdee4204
  (comp (fn [seq] (reduce #(if (< %1 %2) %2 %1) seq)) list))

(defcheck solution-be823e9d
  (fn [& more]
    (reduce #(if (< %1 %2) %2 %1) more)))

(defcheck solution-bf410820
  (fn mx
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (reduce mx (mx x y) more))))

(defcheck solution-bf41979e
  (fn
    ([x y] (if (> x y) x y))
    ([x y & more] (reduce #(if (> %1 %2) %1 %2) (if (> x y) x y) more))))

(defcheck solution-c05a73ac
  (fn [& c]
    (reduce (fn [a b]
              (if (> a b) a b))
      c)))

(defcheck solution-c0a282d7
  (fn max-aux [x & rest]
    (if (empty? rest)
      x
      (let [max-rest (apply max-aux rest)]
        (if (> x max-rest) x max-rest)))))

(defcheck solution-c0b4f000
  (fn ([& more] (reduce #(if (> % %2) % %2) more))))

(defcheck solution-c151ea61
  (fn [& vals] (reduce (fn [a, b] (if (> a b) a b)) vals)))

(defcheck solution-c15a887d
  (fn max-val [& y]
    (reduce (fn [m x] (if (< m x) x m)) y)))

(defcheck solution-c1f46770
  (fn f [hd & tl]
    (if (empty? tl)
      hd
      (let [tl-max (apply f tl)]
        (if (> hd tl-max)
          hd
          tl-max)))))

(defcheck solution-c2b08b3e
  (comp - (partial apply min) (partial map -) list))

(defcheck solution-c2c68cbe
  (fn [& stuff]
    (- (apply min (map - stuff)))))

(defcheck solution-c2cd43b6
  (fn my-max [& xs] (reduce (fn [x y] (if (> x y) x y)) xs)))

(defcheck solution-c2fc36c2
  (fn [& args] (reduce (fn [a b] (if (< a b) b a)) args)))

(defcheck solution-c34dab8a
  (fn [& args]
    (loop [left args big 0]
      (if (empty? left)
        big
        (if (> big (first left))
          (recur (rest left) big)
          (recur (rest left) (first left)))))))

(defcheck solution-c3a183da
  (fn [& numbers]
    (reduce (fn [x y] (if (> x y) x y)) (first numbers) (rest numbers))))

(defcheck solution-c3d6de4f
  (fn
    [& ns]
    (reduce #(if (< %1 %2) %2 %1) 0 ns)))

(defcheck solution-c3e9adae
  (fn [& args]
    (loop [coll args
           m nil]
      (if (empty? coll)
        m
        (recur
          (rest coll)
          (if (or (nil? m) (< m (first coll)))
            (first coll)
            m))))))

(defcheck solution-c4215c22
  (fn [& z] (reduce (fn [a b] (if (> a b) a b)) z)))

(defcheck solution-c443909c
  (fn m
    ([x y] (if (> x y) x y))
    ([x y & more]
     (apply m (m x y) more))))

(defcheck solution-c477b09e
  (fn mymax [& x]
    (- (apply min (map - x)))))

(defcheck solution-c4a38d23
  (fn [& x]
    (reduce #( if (< %1 %2) %2 %1) x)
    ))

(defcheck solution-c4d27a98
  (fn [& nums] (reduce (fn [x y] (if (> x y) x y)) 0 nums)))

(defcheck solution-c503e8eb
  (fn [& args] (last (sort args)) ))

(defcheck solution-c5293774
  (fn [& a] (reduce #(or (and (> %1 %2) %1) %2) a)))

(defcheck solution-c5299374
  (fn mx [& args]
    (if (= (count args) 1)
      (first args)
      (let [ourmax (apply mx (rest args))]
        (if (> (first args) ourmax)
          (first args)
          ourmax)))))

(defcheck solution-c55567a2
  (fn max2
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & rest] (max2 x (apply max2 y rest)))))

(defcheck solution-c59d064c
  (fn [& xs]
    (reduce #(if (> %1 %2) %1 %2) xs)))

(defcheck solution-c60e04b9
  (fn max-val [x & others]
    (if (empty? others)
      x
      (let [m (apply max-val others)]
        (if (> x m)
          x
          m)))))

(defcheck solution-c615007
  (fn [& s]
    (let [aux (fn [acc s]
                (if (empty? s)
                  acc
                  (let [f (first s)
                        n (if (<= acc f) f acc)]
                    (recur n (rest s)))))]
      (aux 0 s))))

(defcheck solution-c6f0af0e
  #(loop [col (rest %&) max-value (first %&)]
     (if (empty? col)
       max-value
       (recur (rest col) (if (> max-value (first col))
                           max-value
                           (first col))))))

(defcheck solution-c7045f92
  (fn [& args] (-> args sort last)))

(defcheck solution-c71c7626
  (fn [& coll] (loop [c coll mx -99999]
                 (if c (recur (next c) (if (> (first c) mx) (first c)
                                                            mx))
                       mx))))

(defcheck solution-c74f34e3
  (fn [& l]
    (loop [x (first l) r (rest l)]
      (cond (empty? r) x
            (< x (first r)) (recur (first r) (rest r))
            :else (recur x (rest r))))))

(defcheck solution-c761ad07
  (fn [x & r] (reduce #(if (< %1 %2) %2 %1) x r)))

(defcheck solution-c79a6e98
  (fn m [y & xs](if-let[x(first xs)](apply m(if(> x y)x y)(next xs))y)))

(defcheck solution-c7ce2f8a
  (fn [a & b] (reduce #(if (> % %2) % %2) a b)))

(defcheck solution-c811b8fa
  (fn [& coll]
    (reduce (fn [m i] (if (> m i) m i))
      0 coll)))

(defcheck solution-c83c7fde
  (fn [& args]
    (reduce (fn [a x] (if (> a x) a x)) args)))

(defcheck solution-c86b6d84
  (fn [& s] (reduce #(if (> % %2) % %2) s)))

(defcheck solution-c8a69b1d
  (fn [& coll] (last (sort coll))))

(defcheck solution-c8e2452
  (fn [& args]
    (reduce #(if (< % %2) %2 %) args)))

(defcheck solution-ca608764
  (fn maximum [& args]
    (reduce (fn [v x] (if (> v x) v x)) args)))

(defcheck solution-ca78ceee
  (fn [& args] (- (apply min (map - args)))))

(defcheck solution-cab8b778
  #(reduce (fn [m i] (if (> i m) i m)) %&))

(defcheck solution-cb83a4f3
  (fn ([a] a) ([a & r] (if (> a (first r)) a (recur (first r) (rest r))))))

(defcheck solution-cbfd30c1
  (fn [& xs] (reduce (fn [a b] (if (> a b) a b))  xs)))

(defcheck solution-ccc2cbe5
  (fn find-max [& args]
    (reduce #(if (< %1 %2) %2 %1) args)))

(defcheck solution-cccf848c
  (fn [& seq]
    (last (sort seq))))

(defcheck solution-cd9a5514
  (fn m
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more] (apply m (m x y) more))))

(defcheck solution-cdb77124
  (fn [& x] (reduce #(if (< %1 %2) %2 %1) x)))

(defcheck solution-cdc69eb
  (fn [& x]
    (reduce #(if (>= %1 %2) %1 %2) x)))

(defcheck solution-cdcba029
  (fn [& vals] (reduce #(if (> %1 %2) %1 %2) vals)))

(defcheck solution-ce8a629b
  (fn [& list-seq]
    (reduce (fn [x y] (if (> x y) x y)) list-seq)))

(defcheck solution-cf0007fa
  (fn [xs & rest] (reduce (fn [x y] (if (< x y) y x)) xs rest)))

(defcheck solution-cf2f6adb
  (fn [& col] (reduce #(if (> %1 %2)
                         %1
                         %2)
                (first col)
                col)))

(defcheck solution-cf693a71
  (fn [& args]
    (reduce
      (fn [result val]
        (if (< val result) result val)) 0 args)))

(defcheck solution-cf6bfd69
  (fn max' [& more]
    (last (sort more))))

(defcheck solution-cfdf0b87
  (fn [x & y]
    (cond
      (nil? y) x
      (> x (first y)) (recur x (next y))
      :else (recur (first y) (next y)))))

(defcheck solution-cfe43ae9
  (fn [n & ns] (reduce #(if (< %1 %2)  %2 %1 )  n ns)))

(defcheck solution-d017364d
  (fn [& args]
    (let [coll (apply list args)]
      (loop [out 0
             in coll]
        (if (empty? in)
          out
          (recur (if (> (first in) out) (first in) out)
            (drop 1 in)))))))

(defcheck solution-d082746c
  (fn [& params]
    (reduce (fn [x y] (if (< x y)
                        y
                        x)
              ) params)))

(defcheck solution-d0c0ffd0
  (fn [& rst] (first (sort > rst))))

(defcheck solution-d0f3b69a
  (fn mmax [& s]
    (reduce #(if (> % %2) % %2) s)))

(defcheck solution-d22e4d9e
  (fn [& vals] (reduce #(if (>= %1 %2) %1 %2) vals)))

(defcheck solution-d2466a75
  (fn [& etc]
    (reduce (fn [l r]
              (if (> l r)
                l
                r))
      etc)))

(defcheck solution-d25899dd
  (fn find-max [& args]
    (loop [col (rest args) mx (first args)]
      (if (empty? col)
        mx
        (recur (rest col) (if (> (first col) mx) (first col) mx))))))

(defcheck solution-d29e33eb
  (fn mymax [& col]
    (reduce #(if (> %1 %2) %1 %2) col)))

(defcheck solution-d318f225
  (fn [& args]
    (reduce #(if (> %1 %2) %1 %2) (first args) (rest args))))

(defcheck solution-d3228e62
  (fn
    [x & more]
    (loop [rs x args more]
      (if (nil? args)
        rs
        (let [y (first args)]
          (recur (if (> rs y)
                   rs y)
            (next args)))))))

(defcheck solution-d385c67a
  (fn alt-max [& coll]
    (reduce #(if (> % %2) % %2) coll)))

(defcheck solution-d461add9
  (fn [& l]
    (loop [x l m nil]
      (if (empty? x)
        m
        (if (or (nil? m) (> (first x) m))
          (recur (rest x) (first x))
          (recur (rest x) m))))))

(defcheck solution-d523d3e7
  (fn [& args]
    (reduce (fn [x y] (if (>= x y) x y)) args)))

(defcheck solution-d5d8e7db
  (fn [& s] (reduce #(if (< %1 %2) %2 %1) s)))

(defcheck solution-d5e72c24
  (fn [& L]
    (reduce (fn [a b] (if (> a b) a b))
      L)))

(defcheck solution-d64b94c8
  (fn [& c] (reduce #(if (> % %2) % %2) c)))

(defcheck solution-d6f972fa
  (fn mx [a & b]
    (if (empty? b)
      a
      (if (< a (apply mx b))
        (apply mx b)
        a))))

(defcheck solution-d81ed8cc
  (fn number38 [& colls]
    (last (sort colls))))

(defcheck solution-d8688f78
  (fn [& nrest] (reduce #(if (< % %2) %2 %) nrest)))

(defcheck solution-d95890fe
  (fn [& nums] (first (sort > nums))))

(defcheck solution-db7270d8
  (fn mx [a & [b & t :as x]]
    (cond
      (nil? b) a
      (< a b) (apply mx x)
      :else (apply mx a t))))

(defcheck solution-db853396
  (fn [& a] (reduce #(if(< %1 %2) %2 %1) 0 a)))

(defcheck solution-db948c7
  (fn [& s]
    (let [get-max (fn [x y]
                    (if (> x y) x y))]
      (reduce get-max s))))

(defcheck solution-dc0a691e
  (fn [& ns]
    (last (sort ns))))

(defcheck solution-dc945b23
  (fn [& x]
    (loop [cnt 0 thing (first x)]
      (if (= cnt (count x))
        thing
        (recur (inc cnt) (if (> (nth x cnt) thing) (nth x cnt) thing))))))

(defcheck solution-dd274471
  (fn my-max [& args]
    (last (sort args))))

(defcheck solution-dd500b48
  (fn [v & coll]
    (reduce #(if (> %1 %2) %1 %2) v coll)))

(defcheck solution-de6a8a9e
  (fn [& valz]
    (reduce #(if (> %1 %2) %1 %2) valz)))

(defcheck solution-df936b85
  (fn sillymax
    ([& more]
     (first (reverse (sort more))))))

(defcheck solution-dffbc252
  (fn _max[& args]
    (reduce
      #(if (> %2 %1) %2 %1)
      args)))

(defcheck solution-e00377a4
  (fn [f & s]
    (loop [mval f r s]
      (if (empty? r)
        mval
        (recur (if (< mval (first r)) (first r) mval)
          (rest r)
          )
        )
      )
    ))

(defcheck solution-e003d908
  (fn [& l] (reduce (fn [x y] (if (> x y) x y)) l)))

(defcheck solution-e012968d
  (fn
    [& arg] (reduce (fn [x y] (if (> x y) x y)) arg)))

(defcheck solution-e02e42a7
  (fn [& coll]
    (reduce (fn [res x]
              (if (> x res)
                x
                res))
      (first coll)
      (rest coll))))

(defcheck solution-e076d9dd
  (fn [x & xs]
    (reduce #(if (> % %2) % %2) x xs)))

(defcheck solution-e0c7cf4d
  #(first (sort > %&)))

(defcheck solution-e0fb3f7
  (fn [fir & nums]
    (loop [i (count nums), fir fir,nums nums]
      (if(> i 0)
        (if(> (first nums) fir)
          (recur (dec i) (first nums) (next nums))
          (recur (dec i) fir (next nums))
          )
        fir
        )
      )
    ))

(defcheck solution-e129eec4
  (fn [x & xs]
    (loop [x x xs xs]
      (if (seq xs)
        (let [max-int-2 (if (> x (first xs)) x (first xs))]
          (recur max-int-2 (rest xs)))
        x))))

(defcheck solution-e14521d3
  (fn [& elems]
    (reduce (fn [a b]
              (if (< a b)
                b
                a))
      elems)))

(defcheck solution-e1ef2c04
  (fn [& arg] (reduce #(if (> %1 %2) %1 %2) arg)))

(defcheck solution-e20c23fc
  (fn [& args]
    (reduce #(if (> %2 %1) %2 %1) args)
    ))

(defcheck solution-e21ab48c
  (fn [& params]
    (loop [m (first params)
           r (rest params)]
      (if (empty? r)
        m
        (if (> (first r) m)
          (recur (first r) (rest r))
          (recur m (rest r)))))))

(defcheck solution-e350b190
  (fn [& ns] (reduce #(if(> %1 %2) %1 %2) ns)))

(defcheck solution-e3b5e7e
  (fn [& a] (reduce #(if (> %1 %2) %1 %2) (first a) (rest a))))

(defcheck solution-e4be21fa
  #(reduce (fn [m x] (if (< m x) x m)) %&))

(defcheck solution-e513a6b5
  (fn [& coll]
    (loop [ g (first coll), c coll]
      (if (empty? c) g
                     (recur (if (> g (first c)) g (first c)) (rest c))
                     )
      )
    ))

(defcheck solution-e578a0e6
  (fn [& etc] (reduce (fn [a b] (if (> a b) a b)) etc)))

(defcheck solution-e5ec05cc
  (fn [& list] (last (sort list))))

(defcheck solution-e60f4678
  (fn [x & xs] (reduce #(if (< %1 %2) %2 %1) x xs)))

(defcheck solution-e6c90441
  (fn [& args] (reduce #(if (> %1 %2) %1 %2) 0 args)))

(defcheck solution-e6ec73a8
  (fn [& args] (first (reverse (sort args)))))

(defcheck solution-e788fade
  (fn my-max
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & more]
     (reduce my-max (my-max x y) more))))

(defcheck solution-e79ef2ce
  (fn my-max [& args]
    (first (sort > args))))

(defcheck solution-e7cf1396
  (fn [& x]
    (reduce (fn [a b] (if (< a b) b a)) x)))

(defcheck solution-e8a37485
  (fn [& xs]
    (reduce #(if (> %1 %2) %1 %2) xs)))

(defcheck solution-e8bff3f9
  (fn [& args](reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-e8c2a64b
  (fn [& nums]
    (reduce (fn [cur-max cur]
              (if (> cur cur-max)
                cur
                cur-max)) nums)))

(defcheck solution-e8c67732
  (fn [x & xs]
    (loop [m x
           ys xs]
      (if (seq ys)
        (if (> (first ys) m)
          (recur (first ys) (rest ys))
          (recur m (rest ys)))
        m))))

(defcheck solution-e90cff48
  (fn find-max [head & tail]
    (loop [biggest head
           remained tail]
      (cond (empty? remained)
            biggest
            :else
            (if (< biggest (first remained))
              (recur (first remained) (rest remained))
              (recur biggest (rest remained)))))))

(defcheck solution-e9be391c
  #(reduce (fn [p c] (if (> c p) c p)) %&))

(defcheck solution-eb5a5a8b
  (fn [e & l] (reduce #(if (> % %2) % %2) e l)))

(defcheck solution-eb912825
  (fn [& ns]
    (reduce #(if (> %1 %2) %1 %2)
      ns)))

(defcheck solution-ec1358b1
  (fn [& x] (reduce (fn [y z] (if (< y z) z y)) 0 (seq x))))

(defcheck solution-ecc71b06
  (fn [x & xs]
    (reduce (fn [a b] (if (> b a) b a)) x xs)))

(defcheck solution-ed12c5de
  (fn [x & xs]
    (reduce #(if (> %1 %2) %1 %2) x xs)))

(defcheck solution-ee95b6d2
  (fn mymax
    ([a b] (if (> a b) a b))
    ([a b & c]
     (reduce mymax (mymax a b) c))))

(defcheck solution-eeacfc60
  (fn [& xs]
    (last (sort xs))))

(defcheck solution-eebb7cf2
  (fn [& more] (reduce (fn[a,b] (if (> a b) a b)) (vec more))))

(defcheck solution-efa36d28
  (fn [& s] (reduce (fn [l r] (if (< l r) r l)) s)))

(defcheck solution-efe72a62
  (fn [& args] (reduce #(if (> % %2) % %2) args)))

(defcheck solution-f0266e1a
  #(reduce (fn [a,b] (if (> a b) a b)) %&))

(defcheck solution-f060fb9e
  (fn mm
    [m & args]
    (if (nil? args)
      m
      (let
       [m1 (apply mm args)]
        (if (> m1 m) m1 m)))))

(defcheck solution-f086f9fb
  (fn [& y] (reduce #(if (> %1 %2) %1 %2) y)))

(defcheck solution-f0a83171
  (fn [a b & coll] (let [gt (fn [a b] (if (> a b) a b))]
                     (if (> a b)
                       (reduce gt (cons a coll))
                       (reduce gt (cons b coll))))))

(defcheck solution-f0fcd075
  (fn [& args] (reduce #(if (> %1 %2) %1 %2) args)))

(defcheck solution-f11bc7b7
  (fn my-max
    ([x] x)
    ([x y] (if (> x y) x y))
    ([x y & args] (reduce my-max (my-max x y) args))))

(defcheck solution-f14680a8
  (comp last sort list))

(defcheck solution-f167a196
  (fn [f & r]
    (letfn [(! [m & r]
              (if (nil? r) m
                           (let [f (first r)]
                             (apply ! (if (> f m) f m) (rest r))
                             )
                           )
              )]
      (apply ! -2147483648 f r))))

(defcheck solution-f193ed49
  (fn [& more]
    (reduce
      (fn
        [x y]
        (if (< x y) y x)
        )
      more
      )
    ))

(defcheck solution-f1ded72c
  (fn [& coll]
    (loop [mx 0 coll coll]
      (if (= coll ())
        mx
        (recur (if (> (first coll) mx) (first coll) mx) (rest coll))
        ))))

(defcheck solution-f2a94fa8
  (fn [& arg] (last (sort (seq arg)))))

(defcheck solution-f3141f22
  (fn [& l]
    (reduce #(if (> %2 %1) %2 %1) l)))

(defcheck solution-f4ff72f3
  (fn [& args] (reduce (fn [m n] (if (> m n) m n)) args)))

(defcheck solution-f5c8a569
  (fn [& c]
    (reduce #(if (< %1 %2) %2 %1) (first c) (rest c))))

(defcheck solution-f60a0f7f
  (fn [& l] (reduce (fn [p n] (if (> n p) n p)) l)))

(defcheck solution-f62c03ab
  (fn highest [& vars] (reduce #(if (> %1 %2) %1 %2) vars)))

(defcheck solution-f6856d70
  (fn [& sq] (reduce #(if (> %1 %2) %1 %2) sq)))

(defcheck solution-f6ccc4e1
  (fn [& a] (reduce #(if (>= %1 %2) %1 %2) a)))

(defcheck solution-f784cd5d
  #(reduce (fn [m x]
             (if (> x m) x m)) %&))

(defcheck solution-f7c72fbb
  (fn [& s]
    (reduce #(if (> %1 %2) %1 %2) s)))

(defcheck solution-f83f95eb
  (fn [& lst]
    (reduce
      #(if (> %2 %1) %2 %1)
      lst
      )
    ))

(defcheck solution-f848292
  (fn [& args]
    (reduce (fn [maximum new] (if (> new maximum) new maximum)) args)))

(defcheck solution-f84b8783
  (fn [x & xs]
    (reduce (fn [acc x] (if (> x acc) x acc) ) 0 (concat [x] xs)
      )))

(defcheck solution-f953fe26
  ;(fn [&amp; a] (- 0 (apply min (map (partial - 0) a))))

  (comp last sort list))

(defcheck solution-fa14f8d7
  (fn [& args]
    (loop [[f & oargs :as my-l] args
           m f]
      (if (empty? my-l)
        m
        (recur oargs (if (> m f) m f))))))

(defcheck solution-fa41a04c
  (fn get-max [& args]
    (first (sort > args))))

(defcheck solution-fa50bdff
  (fn[& args](last (sort args))))

(defcheck solution-fb4b8327
  (fn [x & xs]
    (if-let [[y & ys] xs]
      (if (> x y)
        (recur x ys)
        (recur y ys))
      x)))

(defcheck solution-fb669436
  (fn [& x] (reduce #(if (>= %1 %2) %1 %2) x)))

(defcheck solution-fba59602
  (fn [x & more]
    (reduce (fn [a b] (if (> a b) a b)) x more)))

(defcheck solution-fbc87adc
  (fn [ & sec]
    (reduce #(if (> %1 %2) %1 %2) sec)
    ))

(defcheck solution-fc5a69cf
  (fn mymax [x & y] (if (= y nil)
                      x
                      (if (> x (first y))
                        (apply mymax x (next y))
                        (apply mymax (first y) (next y))))))

(defcheck solution-fcbec9f1
  (fn [& args]
    (reduce
      (fn [a b]
        (if (> a b) a b))
      args)))

(defcheck solution-fd17f3cb
  (fn [& nums]
    (reduce #(if (> %1 %2) %1 %2)
      nums)
    ))

(defcheck solution-fdc219f5
  (fn _max [& items]
    (if (= 1 (count items))
      (first items)
      (if (> (first items) (reduce _max (rest items)))
        (first items)
        (reduce _max (rest items))))))

(defcheck solution-fdd32d9
  (fn [& args] (reduce (fn [acc elem] (if (< acc elem ) elem acc)) 0 args)))

(defcheck solution-fea6b6c
  (fn [& coll]
    (reduce (fn [acc x]
              (if (>= x acc)
                x
                acc))
      coll)))

(defcheck solution-feb153a6
  #(reduce (fn [a b] (if (< a b) b a)) %&))

(defcheck solution-fec450bf
  (fn [& x]
    (reduce #(if (< %1 %2) %2 %1) x)))

(defcheck solution-ff473e50
  (fn [& args] (#(-> args sort last))))

(defcheck solution-ffdbd182
  (fn [& coll]
    (reduce (fn [x y] (if (> x y) x y)) coll)))

(defcheck solution-ffec6fe8
  (fn [& coll] (reduce #(if (> % %2) % %2) coll)))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-38))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
