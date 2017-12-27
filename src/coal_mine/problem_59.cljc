(ns coal-mine.problem-59
  (:require [coal-mine.checks :refer [defcheck-59] :rename {defcheck-59 defcheck}]
            [clojure.test]))

(defcheck solution-10141c0b
  (fn [& ops]
    (fn [& args]
      (loop [ops ops result []]
        (if (empty? ops) result
                         (recur (rest ops) (conj result (apply (first ops) args))))))))

(defcheck solution-101fa845
  (fn [& fns]
    (fn [& args]
      (for [f fns] (apply f args)))))

(defcheck solution-1026b3ab
  (fn [& s]
    (fn [& e]
      (loop [remaining s ans []]
        (if (empty? remaining)
          ans
          (recur (rest remaining) (conj ans (apply (first remaining) e))))))))

(defcheck solution-10596819
  (fn [& funcs]
    (fn [& args] (vec (flatten (map #(list (apply % args)) funcs))))))

(defcheck solution-11ee9bce
  (fn juxt' [& fns]
    (fn [& args]
      (for [f fns]
        (apply f args)))))

(defcheck solution-1242a2a7
  (fn [& fs]
    (fn [& args] (for [f fs] (apply f args)))))

(defcheck solution-12bfd5d1
  (fn juxtapose [& fns]
    (fn [& s]
      (for [f fns]
        (apply f s)))))

(defcheck solution-12dee396
  (fn [& fns] (fn [& args]
                (map #(apply % args) fns))))

(defcheck solution-12e2baa0
  (fn [& fs] #(->> (repeat %&) (map apply fs) vec)))

(defcheck solution-135aad9f
  (fn juxtaposition
    [& functions]
    (fn [& x]
      (into '[]
        (for [f functions]
          (apply f x))))))

(defcheck solution-138461ed
  (fn [& fs]
    (fn  [& a]
      (loop [ res []  rf fs ]
        (if (empty? rf)
          res
          (recur (conj res  (apply (first rf)  a)   )  (rest rf) )
          )
        )
      )
    ))

(defcheck solution-1427579
  (fn [& f]
    (fn [& p]
      (map #(apply % p) f))))

(defcheck solution-142fb24
  (fn f [& v] (fn [& u] (map #(apply % u) v))))

(defcheck solution-14383a65
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-144fe3f4
  (fn _juxt [& functions]
    (fn [& arguments]
      (for [f functions]
        (apply f arguments)))))

(defcheck solution-1491958b
  (fn [& args]

    (fn [& x]

      (map #(apply %1 x) args))


    ))

(defcheck solution-14a38e8d
  (fn [& args]
    (fn [& xs]
      (for [f args] (apply f xs)))))

(defcheck solution-15b396de
  (fn own-juxt [& fns]
    (fn
      [& args]
      (loop [result [], funs fns]
        (if (empty? funs)
          result
          (recur (conj result (apply (first funs) args)) (rest funs)))))))

(defcheck solution-15ef7313
  (fn [& fns]
    (fn [& args]
      (vec (for [f fns]
             (apply f args))))))

(defcheck solution-16bb2eaa
  (fn juxt2 [& fs]
    (fn [& args]
      (map #(apply % args) fs)) ))

(defcheck solution-16d46f38
  (fn [& fs]
    (fn [& a] (for [f fs] (apply f a)))))

(defcheck solution-1775f284
  (fn [& s] #(for [f s] (apply f %&))))

(defcheck solution-17c80a76
  (fn ha [& funcs]
    (fn [& args]
      (for [i funcs]
        (apply i args)))))

(defcheck solution-1800c0cb
  (fn [& f](fn [& a](map #(apply % a) f))))

(defcheck solution-19122c9d
  (fn [& fs]
    (fn [& p]
      (map #(apply % p) fs))))

(defcheck solution-198fe1f0
  (fn [& fs]
    (fn [& args]
      (mapv (fn [f] (apply f args)) fs))))

(defcheck solution-19a8ec5e
  (fn [& fs]
    (fn [& coll]
      (if (= 1 (count coll))
        (map #(% (first coll)) fs)
        (map #(apply % coll) fs)))))

(defcheck solution-19f21a23
  (fn [& functions]
    (fn [& parameters]
      (map #(apply % parameters) functions))))

(defcheck solution-1a046c3d
  (fn [& funcs] (fn [& args] (reduce #(conj %1 (apply %2 args)) [] funcs))))

(defcheck solution-1ad5d8fb
  (fn [& fns]
    (fn [& args]
      (map #(apply %1 args) fns))))

(defcheck solution-1ae286fa
  (fn jxt [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-1b95f1e6
  (fn [& funcs]
    (fn [& args]
      (loop [rs [] fs funcs]
        (if (seq fs)
          (recur (conj rs (apply (first fs) args)) (rest fs))
          rs)))))

(defcheck solution-1b9daef2
  (fn myjuxt[& b](fn[& c](map #(apply % c) b))))

(defcheck solution-1c42e4b9
  (fn [ & ff] (fn [ & xx]
                (loop [ ff ff, r []]
                  (if (empty? ff) r
                                  (recur (rest ff) (conj r (apply (first ff) xx)) )
                                  )))))

(defcheck solution-1d4a93a9
  (fn [& fs]
    (fn [& xs]
      (for [f fs]
        (apply f xs)))))

(defcheck solution-1d4e1718
  #(fn [& x]
     (for [f %&]
       (apply f x))))

(defcheck solution-1d7dc57d
  (fn [& fs]
    (fn [& vs] (for [f fs] (apply f vs)))))

(defcheck solution-1e0138e9
  (fn [& fns]
    (fn [& args]
      (map #(apply %1 %2) fns (repeat args)))))

(defcheck solution-1e355c01
  (fn my-juxt [& fns]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fns))))

(defcheck solution-1e367f8b
  (fn [& f] (fn [& coll] (map #(apply % coll) f))))

(defcheck solution-1e9ebd66
  (fn
    [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-1eb59b86
  (fn [& fns] (fn [& xs] (map #(apply % xs) fns))))

(defcheck solution-1edbe6e2
  (fn [& l]
    (fn [& p]
      (map #(apply % p) l))
    ))

(defcheck solution-1f31761d
  (fn [& f]
    (fn [& a]
      (map apply f (repeat a)))))

(defcheck solution-1f531df3
  (fn my-juxt [& fns]
    (fn [& things-to-be-juxt]
      (map (fn [a-fn]
             (apply a-fn things-to-be-juxt))
        fns))))

(defcheck solution-1f897165
  (fn [& fns]
    (fn [& col]
      (map #(apply % col) fns))))

(defcheck solution-1fa4ebc1
  (fn [& args1]
    (fn [& args2]
      (map #(apply %1 args2) args1))))

(defcheck solution-1fcec5b7
  (fn [& fs]
    (fn [& xs]
      (for [f fs]
        (apply f xs)))))

(defcheck solution-1fe14137
  (fn [& F] (fn [& x] (map (fn [f] (apply f x)) F))))

(defcheck solution-20c675fa
  (fn [& funs] (fn [& args] (map #(apply %1 args) funs))))

(defcheck solution-21d38b31
  (fn [& f]
    (fn [& a]
      (reduce
        #(conj %1 (apply %2 a))
        []
        f))))

(defcheck solution-220ba0b3
  (fn[& fns] (fn [& args] (mapv #(apply % args) fns))))

(defcheck solution-22f79ccd
  (fn [& fs]
    (fn [& args]
      (mapv #(apply % args) fs))))

(defcheck solution-231186b5
  (fn [& fs] (fn [& v] (for [f fs] (apply f v)))))

(defcheck solution-23348383
  #(fn [& x] (for [y %&] (apply y x))))

(defcheck solution-235a7b33
  (fn j [& fs] (fn doit [& args] (map #(apply % args) fs))))

(defcheck solution-23a744e4
  (fn[& f](fn[& i](map #(apply % i) f))))

(defcheck solution-23cc7120
  (fn [& fs] (fn [& vs] (map #(apply % vs) fs))))

(defcheck solution-23fa6211
  (fn [ & fx ]
    (fn [ & args ]
      (map #(apply %1 args) fx))))

(defcheck solution-241a0a10
  (fn juxt'
    ([f] #(vector (apply f %&)))
    ([f & funs] #(into
                   (apply (juxt' f) %&)
                   (apply (apply juxt' funs) %&)))))

(defcheck solution-2426ba28
  #(fn [& args] (map (fn [f] (apply f args)) %&)))

(defcheck solution-243253aa
  (fn [& args1]
    (fn [& args2]
      (for [f args1]
        (apply f args2)))))

(defcheck solution-247b2f8e
  (fn comb [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-24ae2e45
  (fn jxt [& fs] (fn [& a] (map #(apply % a) fs))))

(defcheck solution-24c62b10
  (fn [& funs]
    (fn ([arg] (map #(% arg) funs))
      ([arg & args] (map #(reduce % arg args) funs)))))

(defcheck solution-256b933
  (fn myjuxt [& f]
    (fn [& s]
      (map
        (fn [f] (apply f s) )
        f))))

(defcheck solution-25887382
  (fn [& F] (fn [& L] (for [f F] (apply f L)))))

(defcheck solution-25ec9154
  (fn [& funs]
    (fn [& args]
      (map #(apply % args) funs))
    ))

(defcheck solution-25f05b4
  (fn juxt_ [& fs]
    (fn [& args]
      (vec (for [f fs]
             (apply f args)))
      )
    ))

(defcheck solution-263e7a64
  (fn [& fs]
    #(map
       (fn [x]
         (apply x %&)) fs)))

(defcheck solution-2733c700
  (fn [& f]
    (fn [& x]
      (reduce #(conj % (apply %2 x)) [] f))))

(defcheck solution-2742cb75
  (fn jxt [& fns]
    (fn [& args]
      (for [f fns] (apply f args)))))

(defcheck solution-27747143
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs)
      )
    ))

(defcheck solution-280f0943
  (fn [& fns]
    (fn [& args]
      (map (fn [f] (apply f args)) fns))))

(defcheck solution-282f9ac7
  (fn [& f] (fn [& args] (map apply f (repeat args)))))

(defcheck solution-288ab506
  (fn myjuxt [ & funs]
    (fn [ & args ]
      (map #(apply % args) funs))))

(defcheck solution-28f865d7
  (fn [& f] (fn [& a] (map #(apply % a) f))))

(defcheck solution-292ac13e
  (fn [& f]
    #(for [f f]
       (apply f %&))))

(defcheck solution-295d6a12
  (fn [& s] (fn [& x] (map #(apply % x) s))))

(defcheck solution-29981bab
  (fn [& fns]
    (fn [& eles]
      (vec (for [f fns]
             (apply f eles))))))

(defcheck solution-2a943cd6
  (fn [& fs]
    (fn [& args]
      (map (fn [f] (apply f args)) fs ))))

(defcheck solution-2a984e6
  (fn my-juxt [& fs]
    (fn [& args] (map #(apply % args) fs))))

(defcheck solution-2a9a68f8
  (fn [& fs]
    (fn [& args]
      (let [coll (seq args)]
        (map (fn [f] (apply f coll)) fs)))))

(defcheck solution-2bd62ced
  (fn [& f]
    (fn [& a] (for [x f] (apply x a)))))

(defcheck solution-2bff1e20
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-2c86bf30
  (fn
    ([& b]
     (fn
       ([& x] (reduce #(conj %1 (apply %2 x)) [] b))))))

(defcheck solution-2d4d4f2c
  (fn [& fs]
    (let [fs (map #(partial apply %) fs)]
      (fn [& args]
        (map #(% args) fs)))))

(defcheck solution-2da5401d
  (fn for-juxt [& fs]
    (fn [& args]
      (vec
        (for [f fs
              :let [result (apply f args)]]
          result) ))))

(defcheck solution-2db24f6e
  (fn [& fs]
    (fn [& coll]
      (map
        #(apply % coll)
        fs
        )
      )
    ))

(defcheck solution-2db9c2cd
  #(fn [& v]
     (for [f %&]
       (apply f v)
       )
     ))

(defcheck solution-2e3792b5
  (fn jux [& fs]
    (fn [& xs] (map (fn [f] (apply f xs)) fs)
      )
    ))

(defcheck solution-2e605539
  (fn juxy [& fns]
    (fn [& args]
      (loop [remainder fns result []]
        (if (empty? remainder)
          result
          (recur (rest remainder) (conj result (apply (first remainder) args))))))))

(defcheck solution-2e87cf7
  (fn [& funs]
    (fn [& args]
      (map #(apply %1 args) funs))))

(defcheck solution-2ecf715
  (fn [& fs]
    (fn [& x]
      (if (= (count x) 1)
        (map #(% (first x)) fs)
        (map
          #(reduce % x)
          fs)
        )
      )
    ))

(defcheck solution-2f54c068
  (fn [& fs]
    (fn [& l]
      (map #(apply % l) fs))))

(defcheck solution-2f62490
  (fn myjuxt [& fns]
    (fn [& args]
      (map (fn [f]
             (apply f args)) fns))))

(defcheck solution-2fb0998a
  (fn custom-juxt
    [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-312e3d5d
  (fn [& F]
    (fn [& A]
      (map (fn [f] (apply f A)) F))))

(defcheck solution-317af2eb
  (fn [ & ops]  (fn [ & x ] (map #(apply %1 x) ops))))

(defcheck solution-318eee0
  (fn [& f]
    (fn [& x]
      (reverse(reduce #(cons (apply %2 x) %) [] f)))))

(defcheck solution-3199e6c1
  (fn myapply [& fns]
    (fn [& args] (reduce (fn [t v] (conj t (apply v args))) [] fns))))

(defcheck solution-31c2b639
  (fn my-juxt [& fns]
    (fn [& xs]
      (map #(apply % xs) fns))))

(defcheck solution-31ce74cf
  (fn mapaFci [& f] (fn [ & argj] (map #(apply % argj) f))))

(defcheck solution-32065f
  (fn bob [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-320c86df
  (fn [& functions]
    (fn [& args]
      (mapv #(apply % args) functions))))

(defcheck solution-321c7790
  (fn [& fs]
    (fn [& a] (for [f fs] (apply f a)))))

(defcheck solution-322b433a
  (fn [& f]
    (fn [& p]
      (map #(apply % p) f))))

(defcheck solution-322ec466
  (fn [& fns] (fn [& params] (map #(apply % params) fns))))

(defcheck solution-323464b9
  (fn mult-apply [& fns]
    (fn [& args]
      (map #(apply % args) fns))
    ))

(defcheck solution-325e6df0
  (fn [& funs]
    #(for [f funs]
       (apply f %&))))

(defcheck solution-327a7db
  (fn [& fs]
    (fn [& xs](map #(apply % xs) fs))))

(defcheck solution-337e07c8
  (fn [& fs] (fn [& args]
               (for [fnk fs] (apply fnk args)))))

(defcheck solution-33fc0599
  (fn [& lst]
    (fn [& args]
      (map #(apply % args) lst))))

(defcheck solution-342ea85b
  (fn [& functions]
    (fn [& args]
      (for [f functions] (apply f args)))))

(defcheck solution-343aab8e
  (fn [& fns]
    (fn [& args]
      (reduce
        #(conj  %1 (apply %2 args))
        []
        fns))))

(defcheck solution-3506bd54
  ;(fn juxt4j [& fs]
  ;  (fn [& data]
  ;    (drop 1 (reduce #(conj % (apply %2 (first %))) [data] fs))))

  (fn [& fs]
    (fn [& data]
      (map #(apply % data) fs))))

(defcheck solution-3558c3d6
  (fn [& l] (fn [& a] (for [f l] (apply f a)))))

(defcheck solution-35613746
  (fn [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-356a2c3
  (fn [& x]
    (fn [& y]
      (vec (map #(apply % y) x)))))

(defcheck solution-35821c9d
  (fn jx [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-35bcaf8e
  (fn [& fns] (fn [& args] (mapv #(apply % args) fns))))

(defcheck solution-35c72ea7
  (fn [& fs]
    (fn [& args] (reduce #(conj %1 (apply %2 args)) [] fs))))

(defcheck solution-36697916
  (fn juxtaposition [& x]
    (fn [& y]

      (map

        #(apply % (vec y))


        x))))

(defcheck solution-3700226c
  (fn [& args]
    (fn [& fs] (map #(apply % fs) args))))

(defcheck solution-3881cd10
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-3913c11a
  #(fn [& a] (for [f %&] (apply f a))))

(defcheck solution-3978ab5f
  (fn [& o] (fn [& a] (map #(apply % a) o))))

(defcheck solution-398cd624
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-399212a6
  (fn [& fl]
    (fn [& al]
      (for [f fl]
        (apply f al)))))

(defcheck solution-39add60e
  (fn [& fs]  (fn [& s]
                (for [f fs] (apply f s)) ) ))

(defcheck solution-39d36ab1
  (fn prob59 [& funcs]
    (fn [& args]
      (map #(apply % args) funcs)
      )))

(defcheck solution-39f2f50c
  (fn [& fs] (fn [& arg]
               (loop [init [], [h & t] fs]
                 (if (nil? h)
                   init
                   (recur (conj init (apply h arg)) t))))))

(defcheck solution-3b2c7295
  (fn [& col]
    (fn [& params] (map #(apply % params) col) )

    ))

(defcheck solution-3b5da5
  (fn [& fs] (fn [& arg] (loop [fs fs,res []] (if (empty? fs) res (recur (rest fs) (conj res (apply (first fs) arg))))))))

(defcheck solution-3b765977
  (fn [& a]
    (fn [& b]
      (map #(apply % b) a)
      )
    ))

(defcheck solution-3be03dab
  (fn [& fs]
    (fn [& as]
      (map #(apply % as) fs))))

(defcheck solution-3c30a2db
  (fn [& fns] (fn [& args] (map #(apply % args) fns))))

(defcheck solution-3c624aee
  (letfn [(myjuxt [& funcs]
            (fn [& args]
              (map #(apply % args) funcs)
              ))]
    myjuxt))

(defcheck solution-3c88a412
  (fn [& fns]
    (fn [& args]
      (reduce (fn [acc f]
                (conj acc (apply f args)))
        []
        fns))))

(defcheck solution-3d215ba1
  ; to be improved. complexity: n * m
  (fn jt
    ([x] (fn
           ([a]   [(x a)])
           ([a & args]  (reduce x (x a) args))))
    ([x & fargs] (fn
                   ([a] (map #(% a) (cons x fargs))) ; apply each function to args
                   ([a & args] (map #(reduce % (cons a args)) (cons x fargs))) ; apply each function to args.
                   ))))

(defcheck solution-3db53c3
  (fn [& s](fn [& args](map #(apply  % args)s))))

(defcheck solution-3dd895ae
  (fn jxt [& fs]
    (fn [ & args]  (into [] (map #(apply % args) fs)))))

(defcheck solution-3e35c7d6
  (fn [& funs]
    (fn [& params]
      (map #(apply % params) funs))))

(defcheck solution-3e844bc7
  (fn [& fs]
    (fn [& xs] (map #(apply % xs) fs))))

(defcheck solution-3eb30b40
  (fn [& fs]
    (fn [ & args]
      (map #(apply % args) fs))))

(defcheck solution-3f327bc9
  (fn [& fs]
    #(for [f fs] (apply f %&))))

(defcheck solution-3f4dcbce
  (fn juxt2 [& fs]
    (let [h (fn helper [fs & args]
              (if (empty? fs)
                '()
                (cons (apply (first fs) args) (apply helper (rest fs) args))))]
      (partial h fs))))

(defcheck solution-3f91f139
  (fn [& fs]
    (fn [& params ] (map #(apply % params) fs))
    ))

(defcheck solution-40f4a3e1
  (fn
    [& funcs]
    (fn
      [& args]
      (if
       (= 1 (count args))
        (map
          (fn [func]
            (apply func args)
            ) funcs)
        (map (fn [func] (reduce func args)) funcs)))))

(defcheck solution-40fbf38d
  (fn [& f] (fn [& x] (reduce #(conj %1 (apply %2 x)) [] f))))

(defcheck solution-4138e89c
  (fn jux[& args]
    (fn [& nargs]
      (map #(apply % nargs) args)
      )
    ))

(defcheck solution-41e193a1
  (fn
    [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-41e71a84
  (fn myJuxt [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-4211c118
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-4217d4e0
  (fn [& f] (fn [& a] ( map #(apply %1 a) f))))

(defcheck solution-4253f0b7
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-429a8b20
  #(fn [& r]
     (map (fn [f] (apply f r)) %&)))

(defcheck solution-42ab53a8
  (fn [& fs]
    (fn [& args] (vec (map #(apply % args) fs)))))

(defcheck solution-42d2501f
  (fn somefn [& fns]
    (let [f
          (fn [& args]
            (map (fn [f] (apply f args)) fns))]
      f)))

(defcheck solution-42d889bc
  (fn[& f]
    (fn[& v]
      (for [x f] (apply x v)))))

(defcheck solution-4386374
  (fn [& fs] (fn [& args] (map #(apply %1 args) fs))))

(defcheck solution-43a75ae1
  (fn [& f]
    (fn [& a]
      (map #(apply % a) f))))

(defcheck solution-43d8d6be
  (fn juxta[& funs]
    (fn[& args]
      (map #(apply % args) funs))))

(defcheck solution-44d1bd84
  (fn [& fns]
    (fn [& stuff]
      (loop [fs fns
             result []]
        (if (empty? fs)
          result
          (recur (rest fs) (conj result (apply (first fs) stuff))))))))

(defcheck solution-44fefb46
  (fn [ & fs ] (fn [ & vs ] (map #(apply % vs) fs))))

(defcheck solution-4539c47b
  (fn [& fns] (fn [& args]
                (map #(apply % args) fns))))

(defcheck solution-4566ab70
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-460b6a04
  (fn myJuxt
    [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-4684e5e9
  #(fn [& args] (loop [fns %& v []]
                  (if (empty? fns) v
                                   (recur (next fns) (conj v (apply (first fns) args)))))))

(defcheck solution-46908456
  (fn [& f]
    (fn [& x] (map #(apply % x) f))))

(defcheck solution-4693c102
  (fn j [& fns] (fn [& args] (map #(apply % args) fns))))

(defcheck solution-46a76a1a
  (fn
    ([f]
     (fn
       ([] [(f)])
       ([x] [(f x)])
       ([x y] [(f x y)])
       ([x y z] [(f x y z)])
       ([x y z & args] [(apply f x y z args)])))
    ([f g]
     (fn
       ([] [(f) (g)])
       ([x] [(f x) (g x)])
       ([x y] [(f x y) (g x y)])
       ([x y z] [(f x y z) (g x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
    ([f g h]
     (fn
       ([] [(f) (g) (h)])
       ([x] [(f x) (g x) (h x)])
       ([x y] [(f x y) (g x y) (h x y)])
       ([x y z] [(f x y z) (g x y z) (h x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
    ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce #(conj %1 (%2)) [] fs))
         ([x] (reduce #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce #(conj %1 (apply %2 x y z args)) [] fs)))))))

(defcheck solution-46acb044
  (fn my-juxt [f & fs]
    (fn [& args]
      (vec (map #(apply % args)
             (cons f fs))))))

(defcheck solution-472228ad
  (fn [& fs]
    (fn [& ps]
      (reduce
        #(conj %1 (apply %2 ps))
        []
        fs))))

(defcheck solution-4745b6f
  (fn jux1
    ([f]
     (fn
       ([] [(f)])
       ([x] [(f x)])
       ([x y] [(f x y)])
       ([x y z] [(f x y z)])
       ([x y z & args] [(apply f x y z args)])))
    ([f g]
     (fn
       ([] [(f) (g)])
       ([x] [(f x) (g x)])
       ([x y] [(f x y) (g x y)])
       ([x y z] [(f x y z) (g x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
    ([f g h]
     (fn
       ([] [(f) (g) (h)])
       ([x] [(f x) (g x) (h x)])
       ([x y] [(f x y) (g x y) (h x y)])
       ([x y z] [(f x y z) (g x y z) (h x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
    ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce #(conj %1 (%2)) [] fs))
         ([x] (reduce #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce #(conj %1 (apply %2 x y z args)) [] fs)))))))

(defcheck solution-4804aef7
  (fn [& a]
    (fn [& b] (map #(apply % b) a) )))

(defcheck solution-4875f18b
  (fn [& fx]
    (fn [& xs]
      (map #(apply % xs) fx)
      )
    ))

(defcheck solution-48ba132
  (fn my-juxt [& fs]
    (fn ret [& args]
      (map #(apply % args) fs))))

(defcheck solution-4901d9e0
  (fn f [& funcs]
    (fn g [& args]
      (vec (for [h funcs] (apply h args))))))

(defcheck solution-4922d73
  (fn jxt [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-494a5b62
  (fn ju [& fs]
    (if (= 1 (count fs))
      (fn [& x] [(apply (first fs) x)])
      (fn [& x] (let [functions (map ju fs)]
                  (flatten (map #(apply % x) (flatten functions))))))))

(defcheck solution-494ca495
  (fn juxta-position
    [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-494ef078
  (fn [& funcs]
    (fn ([& seq]
         (loop [funcs funcs
                result []]
           (if (empty? funcs)
             result
             (let [[head & tail] funcs]
               (recur tail (conj result (apply head seq))))))))))

(defcheck solution-49c7e4e0
  (fn j [& fs]
    (fn [& l]
      (map #(apply % l) fs))))

(defcheck solution-49fb241f
  (fn [& fn-list]  (fn [& args] (reduce #(conj %1 (apply %2 args)) [] fn-list))))

(defcheck solution-4a109b51
  (fn [& fcts]
    (fn [& args]
      (map #(apply %1 args) fcts))))

(defcheck solution-4a58c1e7
  (fn [& ops]
    (fn [& x]
      (map #(apply % x) ops)
      )
    ))

(defcheck solution-4a9df8f7
  (fn [& fs]
    (fn [& xs]
      (into [] (for [f fs] (apply f xs)))
      )))

(defcheck solution-4ac20316
  (fn [& fs]
    (fn [& s]
      (map #(apply % s) fs))))

(defcheck solution-4b303def
  (fn [& fs]
    (fn [& as] (map #(apply % as) fs))))

(defcheck solution-4b539708
  (fn [& all]
    (fn [& coll]
      (loop [all all
             coll (if (coll? coll) coll (list coll))
             result []]
        (if (empty? all)
          result
          (let [new-coll (apply (first all) coll)]
            (recur
              (rest all)
              coll
              (conj result new-coll))
            ))))))

(defcheck solution-4b65a090
  (fn [& f]
    (fn [& v]
      (reduce #(conj %1 (apply %2 v)) [] f))))

(defcheck solution-4bb535f8
  (fn juxtapose [& fns]
    (when (seq fns)
      (fn [& xs]
        (map #(apply %1 xs) fns)))))

(defcheck solution-4c4ecc70
  (fn [& f]
    (fn [& a]
      (map #(apply % a) f))))

(defcheck solution-4c64f234
  (fn [& funcs]
    (fn [& xs]
      (mapv #(apply % xs) funcs))))

(defcheck solution-4c825e85
  (fn [& fs]
    (fn [& args]
      (map #(apply % args)
        fs))))

(defcheck solution-4caa6a07
  (fn [& funcs]
    (fn [& args]
      (loop [[f & _ :as funcs] funcs
             result []]
        (if funcs
          (recur (next funcs) (conj result (apply f args)))
          result)))))

(defcheck solution-4cd464cf
  (fn [& ifns] (fn [& args] (map #(apply % args) ifns))))

(defcheck solution-4cd6640
  (fn [& fc] (fn [& coll] (map #(apply % coll) fc))))

(defcheck solution-4d3640cb
  (fn j
    ([f g]
     (fn [& a]
       [(apply f a) (apply g a)]))
    ([f g h]
     (fn [& a]
       [(apply f a) (apply g a) (apply h a)]))))

(defcheck solution-4d44bb68
  (fn [& fs]
    (fn [x & args]
      (loop [fs fs
             r []]
        (if (nil? (first fs))
          r
          (if (nil? args)
            (recur (rest fs) (concat r (vector ((first fs) x))))
            (recur (rest fs) (concat r (vector (apply (first fs) x args))))))))))

(defcheck solution-4d78f100
  (fn [& fs]
    (fn [& c]
      (map #(apply % c) fs))))

(defcheck solution-4d8d02c1
  (fn [& funs]
    (fn [& args]
      (map #(apply % args) funs))))

(defcheck solution-4dc43c87
  (fn juxt* [& fns]
    (fn [& args]
      (map #(apply %1 args)
        fns))))

(defcheck solution-4e20fe0
  (fn [& fns]
    (fn [& args]
      (reduce (fn [val f] (conj val (apply f args))) [] fns))))

(defcheck solution-4e4e3d5c
  #(fn [& args] (for [f %&] (apply f args))))

(defcheck solution-4efa1575
  (fn [& fns]
    (partial
      (fn myfn [fs x & xs]
        (loop [ret []
               vals (list* x xs)
               tfn fns]
          (if (not (empty? tfn))
            (recur (conj ret (apply (first tfn) vals) ) vals (rest tfn))
            ret))) fns)))

(defcheck solution-4f51b3f4
  ;; (map #(apply % [2 3 4]) [+ max])
  (fn [& f] (fn [& v] (map #(apply % v)f))))

(defcheck solution-4f72b7ab
  (fn [& fns]
    (fn [& args]
      (for [f fns]
        (apply f args)))))

(defcheck solution-4fa06c57
  ;; minimum to pass the tests and see the solutions
  ;;
  ;; (fn jux
  ;;    ([a b] (fn [& n] [ (apply a n) (apply b n)]))
  ;;    ([a b c] (fn [& n] [ (apply a n) (apply b n) (apply c n)])))


  ;; this one is nice!
  (fn [& f]
    (fn [& a]
      (map #(apply % a) f))))

(defcheck solution-5007d207
  (fn jux [& fns] (fn[& args](map #(apply % args) fns))))

(defcheck solution-5086acbe
  (fn [& a] #(for [x a] (apply x %&))))

(defcheck solution-509a173e
  (fn juxt_ [& fs]
    (fn [& args]
      (loop [fs_ fs acc []]
        (if (empty? fs_)
          acc
          (recur (rest fs_) (conj acc (apply (first fs_) args))))))))

(defcheck solution-50f4281c
  (fn [& l] (fn [& y] (reduce (fn [a f] (cons (apply f y) a)) [] (reverse l)))))

(defcheck solution-510f05a2
  (fn [& fns]
    (fn [& xs]
      (reduce #(conj %1 (apply %2 xs)) [] fns))))

(defcheck solution-512f8051
  #(fn [& a] (map (fn [f] (apply f a)) %&)))

(defcheck solution-5140d388
  (fn [& funcs] (fn [& args] (map #(apply % args) funcs))))

(defcheck solution-51dd311c
  (fn [& fs]
    (fn [& vs]
      (map #(apply % vs) fs))))

(defcheck solution-5244059a
  (fn myjuxt [& fns]
    (let [f (first fns)
          g (rest fns)]
      (fn [& x]
        (let [result (apply f x)]
          (if (empty? g)
            result
            (flatten (conj '()
                       (apply (apply myjuxt g) x)
                       result))))))))

(defcheck solution-52a6829
  (fn jxt [& fs]
    (fn [& args]
      (map (fn [f] (apply f args)) fs))))

(defcheck solution-52b58fe
  (fn jux [f & others]
    (if others
      (fn [& x]
        (concat [(apply f x)]
                (apply (apply jux others) x)))
      (fn [& x] [(apply f x)]))))

(defcheck solution-5317ce24
  (fn [& f]
    (fn [& v]
      (reduce #(conj % (apply %2 v)) [] f) )))

(defcheck solution-5327ad9a
  (fn [& fs] (fn [& args] (mapv #(apply % args) fs))))

(defcheck solution-538d0733
  (fn [& f] (fn [& x] (map #(apply % x) f) )))

(defcheck solution-53e1cc29
  (fn [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-540925c9
  (fn [& f]
    (fn [& p]
      (map #(apply % p) f))))

(defcheck solution-54507a3b
  (fn [& fns]
    (fn [& args]
      (reduce
        #(conj %1 (apply %2 args))
        []
        fns
        )
      )
    ))

(defcheck solution-54af7460
  (fn [& fns]
    (fn [& args]
      (loop [lseq fns acc []]
        (if (empty? lseq)
          acc
          (recur (rest lseq) (conj acc (apply (first lseq) args))))))))

(defcheck solution-54f729a3
  (fn [& fs]
    (fn[& args]
      (map #(apply % args) fs))))

(defcheck solution-55f981f
  (fn [& fns]
    (fn [& args]
      (for [f fns]
        (apply f args)))))

(defcheck solution-56115853
  (fn [& fs] (fn [& args] (map (fn [f] (apply f args)) (seq fs)))))

(defcheck solution-577ceebf
  (fn [& fs] (fn [& args] (for [f fs] (apply f args)))))

(defcheck solution-57a37a62
  (fn [& fs]
    (fn [& v]
      (map #(apply % v) fs))))

(defcheck solution-57be061b
  (fn [& f]
    (fn [& a]
      (map #(apply % a) f))))

(defcheck solution-57e84db3
  (fn juxtapose [& caller-funcs]
    (fn f [& x]
      (reverse (loop [funcs caller-funcs results '()]
                 (if (empty? funcs)
                   results
                   (recur (rest funcs) (cons (apply (first funcs) x) results))))))))

(defcheck solution-5840a31e
  (fn [& fargs]
    (fn [& vargs]
      (reduce #(conj % (apply %2 vargs)) [] fargs))))

(defcheck solution-5897f4ea
  (fn [& fs]
    (fn [& as]
      (map #(apply % as) fs))))

(defcheck solution-591f4df8
  (fn [& f]
    (fn [& e]
      (map #(apply % e) f))))

(defcheck solution-5a0234ef
  (fn[& fs] (fn [& ps] (map #(apply % ps) fs))))

(defcheck solution-5a22332a
  (fn myJux [& fnctns] (fn [& args] (map #(apply % args) fnctns))))

(defcheck solution-5af04691
  (fn my-juxt [& funcs]
    (fn [& args]
      (loop [fun funcs
             resp []]
        (if (= fun [])
          resp
          (recur (rest fun) (conj resp (apply (first fun) args))))))))

(defcheck solution-5b6ae5a1
  (fn [& funs]
    (fn [& args]
      (reduce
        (fn [result current]
          (conj result (apply current args))) [] funs))))

(defcheck solution-5bb9e2e5
  (fn [& fs]
    (fn [& args]
      (loop [ret (vector (apply (first fs) args)) fs (next fs)]
        (if fs
          (recur (conj ret (apply (first fs) args)) (next fs))
          ret)))))

(defcheck solution-5be3e521
  (fn juxtaposition [& fs]
    (fn [& s] (map #(apply % s) fs))))

(defcheck solution-5e7bb2c1
  (fn jux [& fns]
    (fn [& xs]
      (map #(apply % xs) fns))))

(defcheck solution-5ecc9b03
  #(fn [& x] (map (fn [f] (apply f x)) %&)))

(defcheck solution-5f9d943a
  (fn [& funcs]
    (fn [& args]
      (vec
        (for [f funcs]
          (apply f args))))))

(defcheck solution-5faae4cc
  (fn [& fns]
    (fn [& xs]
      (map (fn [f] (apply f xs)) fns))))

(defcheck solution-60991cb3
  (fn [& f]
    (fn [& v]
      (map #(apply % v) f))))

(defcheck solution-609f1c1d
  (fn my-juxt [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-60c0d878
  (fn [& fs]
    (fn [& xs]
      (mapv #(apply % xs) fs))))

(defcheck solution-60c4417a
  (fn _juxt [& fs]
    (fn [& args] (for [f fs] (apply f args)))))

(defcheck solution-62236e1e
  (fn [& fs]
    (fn [& args]
      (loop [fs fs
             answer []]
        (if (nil? fs) answer
                      (recur (next fs) (conj answer (apply (first fs) args))))))))

(defcheck solution-622da98f
  (fn [& fs]
    (fn [& xs]
      (for [f fs]
        (apply f xs))
      )
    ))

(defcheck solution-6241c93
  (fn [& fs]
    (fn [& coll]
      (loop [fs fs, res []]
        (if (empty? fs) res
                        (recur (rest fs) (conj res (apply (first fs) coll))))))))

(defcheck solution-62790e7a
  (fn [& fs]
    (fn [& xs]
      (map (fn [f] (apply f xs)) fs))))

(defcheck solution-628864c8
  #(fn [& x] (for [f %&] (apply f x))))

(defcheck solution-6314b957
  (fn [& ops]
    (fn [& args]
      (map #(apply % args) ops))))

(defcheck solution-646bf173
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-651986ee
  (fn [& fs]
    (fn [& arg]
      (map #(apply % arg) fs))))

(defcheck solution-65a384c4
  (fn juxtaposition
    [& fns]
    (fn
      [& args]
      (map apply fns (repeat args)))))

(defcheck solution-661a62bb
  (fn [& fns]
    (fn [& a]
      (loop [[f & fs] fns, res []]
        (if (nil? f) res
                     (recur fs (conj res (apply f a))))))))

(defcheck solution-66411938
  (fn [& x] (fn [& a] (map #(apply % a) x))))

(defcheck solution-66c0a9b0
  (fn [& opts]
    (fn [& args]
      (map #(apply % args) opts))))

(defcheck solution-67bef366
  (fn [ & fs]
    (fn [ & args]
      (mapcat (fn [f] [(apply f args)]) fs))))

(defcheck solution-67cc5543
  (fn [& fs]
    (fn ([& args] (reduce #(conj  %1 (apply %2  args)) [] fs)))))

(defcheck solution-6803ed9a
  #(fn [& args]
     (map (fn [f] (apply f args)) %&)))

(defcheck solution-6883b50c
  (fn myjuxt [& fsq]
    (fn [& args]
      (for [f fsq]
        (apply f args)))))

(defcheck solution-69121866
  (fn juxt-disguised
    [& funcs]
    (fn [& args] (map #(apply % args) funcs))))

(defcheck solution-6944840f
  (fn my-jutx [& args]
    (fn [& jargs]
      (map #(apply % jargs) args)
      )
    ))

(defcheck solution-6a1867ac
  #(fn [& x]
     (map (fn [f] (apply f x)) %&)))

(defcheck solution-6a898a2e
  (fn [& f]
    (fn [& g]
      (loop [result [] functions f]
        (if functions
          (recur (conj result (apply (first functions) g)) (next functions))
          result
          )
        )
      )
    ))

(defcheck solution-6abce636
  (fn [ & fs] (fn [ & args] (map #(apply % args) fs))))

(defcheck solution-6b2ab326
  (fn [& args]
    (fn [& inner]
      (map #(apply % inner) args))))

(defcheck solution-6b5097e3
  (fn [& funcs]
    (fn [& args]
      (map
        #(apply % args)
        funcs)
      )
    ))

(defcheck solution-6b51d529
  (fn [& f]
    (fn [& b]
      (map #(apply % b) f))))

(defcheck solution-6bb87d80
  (fn [& fs]
    (fn [& n]
      (map #(apply % n) fs))))

(defcheck solution-6bf7ce15
  (fn jxt [& args]
    (partial
      (fn [acc fns & n]
        (if (empty? fns) acc
                         (recur
                           (conj acc (apply (first fns) n))
                           (rest fns) n))) '() (reverse args))))

(defcheck solution-6cab4205
  (fn myjuxt [& fns]
    (fn [& args]
      (loop [ret [] fs fns]
        (if fs
          (recur (conj ret (apply (first fs)  args)) (next fs))
          ret
          ))
      )))

(defcheck solution-6cbd2f84
  (fn __
    [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-6cc7ea86
  (fn myjuxt[& fs](fn[& args](reduce #(conj % (apply %2 args)) [] fs))))

(defcheck solution-6d34758a
  (fn juxtaposition [& funcs]
    (fn [x & args]
      (map (fn [f]
             (apply f x args)) funcs))))

(defcheck solution-6d6370d1
  (fn my-juxt [& funcs]
    (fn [& args]
      (map #(apply % args) funcs ))))

(defcheck solution-6d7703e
  (fn [& fs]
    (fn [& a]
      (map #(apply % a) fs)
      )
    ))

(defcheck solution-6e138c68
  (fn [& x]
    (fn [& y]
      (map #(apply % y) x))))

(defcheck solution-6e342730
  (fn prob-0059
    [& in-funs]
    (fn [& args]
      (loop [rans ()
             fs   in-funs]
        (if fs
          (recur (cons (apply (first fs) args) rans) (next fs))
          (reverse rans))))))

(defcheck solution-6e7d0bf4
  (fn [& fs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fs))))

(defcheck solution-6f5525ea
  (fn [& fs]
    (fn [& args]
      (reduce (fn [acc f] (conj acc (apply f args))) [] fs))))

(defcheck solution-6f624af5
  (fn juxtaposition
    [& fs]
    (fn [& s]
      (map
        #(apply % s)
        fs))))

(defcheck solution-6f8132fc
  (fn my-juxt [& fs]
    (fn [& args]
      (loop [an [] fs fs]
        (if (empty? fs)
          an
          (recur (conj an
                   (apply (first fs) args))
            (rest fs)))))))

(defcheck solution-6f9289c7
  (fn [& g] #(for [f g] (apply f %&))))

(defcheck solution-6f9be916
  (fn [& fns]
    (fn [& args]
      (loop [to-juxt (rest fns)
             juxtd   [(apply (first fns) args)]]
        (if (empty? to-juxt)
          juxtd
          (recur (rest to-juxt)
            (conj juxtd (apply (first to-juxt) args))))))))

(defcheck solution-6f9d0cd9
  (fn[& fncs]
    (fn
      ([arg] (map #(%1 %2) fncs (repeat arg)))
      ([arg & more] (map #(reduce %1 %2)
                      fncs (repeat (cons arg more)))))))

(defcheck solution-6fbb4df0
  (fn [& f] (fn [& p] (map #(apply % p) f))))

(defcheck solution-6fdcfcd6
  (fn [f & fs] (fn [x & xs] (map #(apply % (cons x xs)) (cons f fs)))))

(defcheck solution-707a1527
  (fn [& fs]
    (fn [& vals]
      (map #(apply % vals) fs))))

(defcheck solution-70ac6d4
  (fn z [& fs]
    (fn [& args]
      (into [] (map #(apply %1 args) fs)))))

(defcheck solution-70bfe763
  (fn [& funcs] (fn [& args] (for [f funcs] (apply f args)))))

(defcheck solution-70d68fe4
  (fn
    [& func]
    (fn [& args] (map #(apply % args) func))))

(defcheck solution-71271f96
  (fn[& xs]
    (fn [& ys]
      (mapv #(apply % ys) xs))))

(defcheck solution-7145e5a1
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))
    ))

(defcheck solution-7185f0bd
  (fn [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-71917e0b
  (fn my-juxt [& functions]
    (fn [& args]
      (for [f functions]
        (apply f args)))))

(defcheck solution-724d4008
  (fn mjuxt
    [& fns]
    (fn [& args]
      (reduce (fn [r f]
                (conj r (apply f args)))
        [] fns))))

(defcheck solution-72a5565b
  (fn [& os]
    (fn [& vs]
      (map #(apply % vs) os))))

(defcheck solution-730e9e5e
  (fn [& f]
    (fn [& a]
      (reduce #(concat %1 (vector (apply %2 a))) [] f))))

(defcheck solution-73619664
  (fn [& fn-list]
    (fn [& args]
      (map #(apply % args) fn-list))))

(defcheck solution-73688249
  (fn [& fns]
    (fn [& args]
      (map (fn [f] (apply f args)) (vec fns)))))

(defcheck solution-742cd1e4
  (fn mapf [& fns]
    (fn [& args]
      (into [] (for [f fns]
                 (apply f args))))))

(defcheck solution-7488df84
  (fn [& a] (fn [& b] (for [f a] (apply f b)))))

(defcheck solution-75028ada
  (fn[& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-757e4b0a
  (fn my-jutx [& fs]
    (fn [& args]
      (map (fn [f] (apply f args)) fs))))

(defcheck solution-763ca352
  (fn [& fs]
    (fn [& args] (map #(apply % args) fs))))

(defcheck solution-7684e8d9
  (fn [& fns]
    (fn [& xs]
      (for [f fns] (apply f xs)))))

(defcheck solution-76c7ca33
  (fn [& fns] #(for [f fns] (apply f %&))))

(defcheck solution-7725f675
  (fn justaposition-
    ^{:doc "59. Take a set of functions and return a new function that takes a variable
          number of arguments and returns a sequence containing the result of
          applying each function left-to-right to the argument list."}
    [& fs] (fn [& args] (map #(apply % args) fs))))

(defcheck solution-77318e62
  (fn [& more]
    (fn [& more2]
      (map #(apply % more2)  more)
      )))

(defcheck solution-7732e422
  (fn
    ([f g h]
     (fn
       ([& args]
        [(apply f args) (apply g args) (apply h args) ])))

    ([f g]
     (fn
       ([& args]
        [(apply f args) (apply g args) ])))

    ))

(defcheck solution-7764bbf5
  (fn [& funcs]
    (fn [& args] (map #(apply %1 %2) funcs (repeat args)))))

(defcheck solution-77aef432
  (fn [& more]
    (loop [result (first more) i 1]
      (if (= i (count more))
        result
        (recur
          (fn [& args]
            (assoc
             (if (coll? (apply result args))
               (apply result args)
               (vector (apply result args) )
               )
              (if (coll? (apply result args))
                (count (apply result args))
                1)
              (apply (nth more i) args)
              )
            )
          (inc i))
        )) ))

(defcheck solution-78fa8adf
  (fn [& ops]
    (fn [& args]
      (map (fn [op] (apply op args)) ops)
      )
    ))

(defcheck solution-7991f614
  (fn [& fns]
    (fn [& args]
      (loop [ret [(apply (first fns) args)]
             fns (next fns)]
        (if fns
          (recur (conj ret (apply (first fns) args)) (next fns))
          ret)))))

(defcheck solution-79adf42e
  (fn juxtaposition [x & xs]
    (fn [& args]
      (map #(apply % args) (cons x xs)))))

(defcheck solution-79b66d75
  (fn [& f]
    (fn [& a]
      (reduce #(conj % (apply %2 a)) [] f))))

(defcheck solution-7a12ecf8
  (fn [& f] (fn [& x] (map #(apply % x) f))))

(defcheck solution-7a2af5a3
  (fn [& f]
    (fn [& args]
      (map #(apply % args)  f))))

(defcheck solution-7b3afa4c
  (fn [& f]
    (fn [& a]
      (map #(apply % a) f))))

(defcheck solution-7b73ba57
  #(fn [& args] (map (fn [a] (apply a args)) %&)))

(defcheck solution-7bd6d29f
  (fn [& fs]
    (fn [& args]
      (loop [f fs rt []]
        (if f
          (recur (next f) (conj rt (apply (first f) args)))
          rt)))))

(defcheck solution-7c50dd13
  (fn [& fs] (fn [& as] (map #(apply % as) fs))))

(defcheck solution-7d3874c
  (fn  [& fs] (fn [& s] (map #(apply % s) fs))))

(defcheck solution-7e2fc40a
  (fn [& funcs]
    (fn [& xs] (map #(apply % xs) funcs)  )
    ))

(defcheck solution-7efd153e
  (fn [& fs] #(for [f fs] (apply f %&))))

(defcheck solution-7f8d0897
  (fn myjuxt [& fns]
    #(loop [f fns
            ret []]
       (if (= 1 (count f))
         (conj ret (apply (first f) %&))
         (recur (rest f)
           (conj ret (apply (first f) %&)))))))

(defcheck solution-7f9ae26a
  (fn [& f] (fn [& s] (map #(apply % s) f))))

(defcheck solution-80021911
  (fn[& fs] (fn [& args] (map #(apply % args) fs))))

(defcheck solution-804312f1
  (fn xjuxt [& fs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fs)
      )))

(defcheck solution-807158f5
  (fn [& f]
    (fn [& args]
      (map #(apply % args) (apply vector f)))))

(defcheck solution-80f72cd9
  (fn j [& funcs]
    (fn [& data]
      (loop [fs (reverse funcs) result '()]
        (cond (empty? (rest fs))
              (conj result (apply (first fs) data))
              :else
              (recur (rest fs) (conj result (apply (first fs) data)))
              )
        )
      )
    ))

(defcheck solution-81235252
  (fn [& fns]
    (fn [& args]
      (reduce (fn [res f] (conj res (apply f args)))
        []
        fns))))

(defcheck solution-814c6804
  (fn [& fs]
    (fn [& es]
      (map #(apply % es) fs))))

(defcheck solution-816e7dd8
  (fn [ & f]
    (fn [ & r]
      (map #(apply % r) f))))

(defcheck solution-818b405d
  (fn my-juxt [& funs]
    #(for [f funs] (apply f %&))))

(defcheck solution-81973a52
  (fn [& functions]
    (fn [& parameters]
      (map #(apply % parameters) functions))))

(defcheck solution-821d61cc
  (fn my-juxt [& args]
    (fn [& args-]
      (map #(apply %1 args-) args))))

(defcheck solution-82435dfd
  (fn [& fs]
    (fn [& args]
      (mapv (fn [f] (apply f args))
        fs))))

(defcheck solution-82638fd2
  (fn my[& f]
    (fn [& x]
      ((fn[functions]
         (for [function functions] (apply function x))) f))))

(defcheck solution-82b98eff
  (fn[& fs](
             fn[& args](
                         map
                         (fn[f](apply f args))
                         fs
                         )
             )))

(defcheck solution-8342fc8f
  (fn [& f]
    (fn [& args]
      (map #(apply %1 args) f)
      )
    ))

(defcheck solution-83f66e3b
  (fn [& fs] (fn [& x] (map #(apply % x) fs))))

(defcheck solution-84210aa5
  (fn [& fs]
    (fn [& args]
      (map apply fs (cycle [args])))))

(defcheck solution-861b242d
  (fn [& p]
    (fn [& a] (map #(apply % a) p))))

(defcheck solution-863e018d
  (fn [& fns] #(map (fn [f] (apply f %&)) fns)))

(defcheck solution-871fdfca
  (fn [& fns]
    (fn [& args]
      (reduce
        (fn [ret f]
          (conj ret (apply f args)))
        '()
        (reverse fns)))))

(defcheck solution-87bb4280
  (fn juxtapose [& funcs]
    (fn [& args]
      (into [] (map #(apply % args) funcs)))))

(defcheck solution-88424c65
  (fn [& fns]
    (fn [& args]
      (map (fn [f] (apply f args)) fns))))

(defcheck solution-887dc8d0
  (fn j [& fs]
    (fn [& a]
      (map #(apply % a) fs))))

(defcheck solution-88d2c4dd
  (fn [& fs]
    (fn [& a] (map #(apply % a) fs))))

(defcheck solution-88f73eb8
  (fn [& ops] (fn [& args] (map #(apply % args) ops))))

(defcheck solution-8910a267
  (fn[& fs] (fn[& args] (for[f fs] (apply f args)))))

(defcheck solution-8912c124
  (fn [& funcs] (fn [& s] (map #(apply %1 s) funcs))))

(defcheck solution-893f68c6
  (fn [& fs] (let [fz (fn[& args] (mapv (fn[f] (apply f args)) fs))] partial fz)))

(defcheck solution-894ae51c
  (fn __ [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-896be057
  (fn juxts [& funcs]
    (fn [& args]
      (loop [f funcs lst '()]
        (if (empty? f)
          lst
          (recur
            (butlast f)
            (conj lst (apply (last f) args))
            )
          )
        )
      )
    ))

(defcheck solution-898b89c
  (fn my-juxt[& fs]
    #(for [f fs] (apply f %&))
    ))

(defcheck solution-899b6e6
  (fn J [& op*]
    (fn [& args]
      (map #(apply % args) op*))))

(defcheck solution-8a1491f8
  #(fn [& args]
     (map (fn [f] (apply f args)) %&)))

(defcheck solution-8a69d0d0
  (fn [& fs]
    (fn [& args]
      (reduce
        #(conj %1 (apply %2 args))
        []
        fs))))

(defcheck solution-8aaaf292
  (fn
    [& functions]
    (fn [& arguments]
      (map #(apply % arguments) functions))))

(defcheck solution-8af26519
  (fn juxt' [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-8c56c5f
  (fn
    [& f]
    (fn [& a]
      (loop [l f v []]
        (if (empty? (rest l))
          (conj v (apply (last l) a))
          (recur (rest l) (conj v (apply (first l) a)))
          )))
    ))

(defcheck solution-8ce8b627
  (fn [& funcs]
    #(for [f funcs] (apply f %&))))

(defcheck solution-8d01ce20
  (fn [& functions]
    (fn [& args]
      (map (fn [f]
             (apply f args))
        functions))))

(defcheck solution-8d11241c
  (fn c59 [& funs]
    (fn [& args]
      (map #(apply %1 args) funs))))

(defcheck solution-8d70d5d
  (fn [& fs] (fn [& args] (->> fs
                            (map (partial conj (list args)))
                            (map (partial apply apply))))))

(defcheck solution-8e385343
  (fn [& fs]
    (fn [& args] (map #(apply % args) fs))))

(defcheck solution-8e7dd7
  (fn [ & f ]
    (fn [ & x ]
      (map #(apply % x) f)
      )
    ))

(defcheck solution-8fbc58a4
  (fn [& fs] (fn [& col] (map #(apply % col) fs))))

(defcheck solution-8fd0a925
  (fn [& fl]
    (fn [& args]
      (vec (map #(apply % args) fl)))))

(defcheck solution-8ff34c77
  (fn [& fs]
    (fn [& args]
      (mapv (fn [f] (apply f args)) fs))))

(defcheck solution-90003029
  (fn [& f]
    (fn [& a] (map #(apply % a) f))))

(defcheck solution-90180e80
  (let [f
        (fn [funs result & args]
          (if (nil? funs)
            result
            (recur (next funs) (conj result (apply (first funs) args)) args)
            )
          )]
    (fn [& x] (partial f x []))
    ))

(defcheck solution-906e2f93
  (fn [& funcs]
    (fn [& args]
      (for [f funcs] (apply f args)))))

(defcheck solution-911b4474
  (fn juxtt [f & other-fs]
    (if (empty? other-fs)
      ; Base case: single function
      (fn [& args] (list (apply f args)))
      ; Recursive case
      (fn [& args]
        (cons
          (apply f args)
          (apply (apply juxtt other-fs) args))))))

(defcheck solution-91c5eab2
  (fn juxta [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-91ca918c
  (fn [& fns]
    (fn [& a]
      (map (fn [f] (apply f a))
        fns))))

(defcheck solution-91d67c27
  (fn ju
    ([] nil)
    ([f & n]
     (fn [& p]
       (let [fa (apply f p)
             na (apply (or (apply ju n) (constantly nil)) p)]
         (cons fa na))))))

(defcheck solution-91eaca46
  (fn jux [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-91ed63ff
  (fn [& fns]
    (fn [& xs]
      (into []  (map #(apply % xs) fns)))))

(defcheck solution-92d10724
  (fn my-juxt [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-92f59e41
  (fn [& fns]
    (fn [& xs]
      (reduce #(conj %1 (apply %2 xs))
        []
        fns))))

(defcheck solution-93c68c49
  #(fn [& l]
     (map apply %& (cycle [l]))))

(defcheck solution-94c8d40f
  (fn juxt' [& fs]
    (fn [& xs]
      (mapv #(apply % xs) fs))))

(defcheck solution-953282c5
  (fn my-juxt [& fs]
    (fn [& xs]
      (map
        #(apply % xs)
        fs))))

(defcheck solution-9586018b
  (fn [& fcoll]
    (fn [& coll]
      (for [f fcoll]
        (apply f coll)))))

(defcheck solution-95b42eb5
  (fn [& f]
    (fn [& args]
      (map #(apply % args) f))))

(defcheck solution-95cd5188
  (fn [& fs]
    (fn [& a]
      (map #(apply % a) fs))))

(defcheck solution-95d8c5f7
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-96bb9994
  (fn juxtapose
    [& functions]
    (fn [& args]
      (loop [[h & t] functions
             accum []]
        (if (nil? t)
          (conj accum (apply h args))
          (recur t (conj accum (apply h args))))))))

(defcheck solution-976e94ee
  (fn [x & xs]
    (fn [& a]
      (map #(apply % a) (cons x xs)))))

(defcheck solution-97dda89e
  (fn [& fargs]
    (fn [& cargs]
      (map #(apply % cargs) fargs))))

(defcheck solution-9894a983
  (fn [& fs]
    (fn [& ps]
      (map #(apply % ps) fs))))

(defcheck solution-99589c66
  ;; Mfikes solution way better than mine
  (fn my-juxt [& fs]
    (fn [& xs]
      (mapv (fn [f] (apply f xs)) fs))))

(defcheck solution-99aa0444
  (fn [& ff] (fn [& aa] (map #(apply % aa) ff))))

(defcheck solution-99c6e55
  (fn juxt2 [& funs]
    (fn [& xs]
      (reduce
        #(conj %1 (apply %2 xs))
        []
        funs))))

(defcheck solution-99db8451
  (fn [& f]
    (fn [& c]
      (map (partial #(apply %2 %) c) f))))

(defcheck solution-9a023e4f
  (fn my-juxt[& myfns](fn[& a](map #(apply % a) myfns))))

(defcheck solution-9aa08ef5
  (fn [& fs] (fn [& xs]
               (if (> (count xs) 1)
                 (map #(reduce % xs) fs)
                 (map #(% (first xs)) fs)))))

(defcheck solution-9b0d86f4
  (fn j [f & coll]
    (fn [ & args]
      (map #(apply % args) (cons f coll)))))

(defcheck solution-9b3cb93e
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-9b92a9c5
  (fn [& fs]
    (fn [& args]
      (map (fn [f]
             (apply f args))
        fs))))

(defcheck solution-9c2c71bf
  (fn jxt [& fs]
    (fn [& args]
      (mapv #(apply % args) fs))))

(defcheck solution-9ca9f3fe
  (fn myJuxt [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-9db5cd3
  (fn [& functions]
    (fn [& args] (map #(apply % args) functions))))

(defcheck solution-9df9df7
  (fn [& fns]
    (fn [& args]
      (into [] (map apply fns (repeat args))))))

(defcheck solution-9e846430
  (fn [& s]
    (fn [& v] (map #(apply % v) s))))

(defcheck solution-9e8f57bc
  (fn [& fs]
    (fn [& coll]
      (map #(apply % coll) fs))))

(defcheck solution-9ef953e3
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns)
      )))

(defcheck solution-9ff06e79
  (fn [& args]
    (fn [& xs]
      (for [f args]
        (apply f xs)))))

(defcheck solution-9ff9e878
  (fn [& fl]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fl)
      )
    ))

(defcheck solution-a0c993d2
  (fn [& fs]
    (fn [& v] (map #(apply % v) fs))))

(defcheck solution-a104ec7c
  (fn jux [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-a15b2888
  (fn [& fns]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fns))))

(defcheck solution-a279520
  (fn juxt2 [& function-list]
    (fn [& xs]
      (loop [fns function-list acc []]
        (if (empty? fns)
          acc
          (recur (rest fns) (conj acc (apply (first fns) xs))))))))

(defcheck solution-a294dded
  (fn [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-a29d36ed
  (fn [& f]
    (fn [& a] (for [x f] (apply x a)))))

(defcheck solution-a2af0bcd
  (fn [f & fns]
    (fn [& args]
      (let [fs (list* f fns) ret []]
        (loop [nxt (next fs) ret (conj ret (apply (first fs) args))]
          (if nxt
            (recur (next nxt) (conj ret (apply (first nxt) args)))
            ret))))))

(defcheck solution-a2d49fde
  (fn f
    [& fl]
    (fn [& d]
      (map #(apply % d) fl))))

(defcheck solution-a3355714
  #(fn [& v]
     (for [f %&]
       (apply f v))))

(defcheck solution-a3ccbabd
  (fn juxtapose [& funcs]
    (fn [& args]
      (map (fn [f] (apply f args)) funcs))))

(defcheck solution-a4eae440
  (fn cust-jux
    ([x] (fn [& args] (list (apply x args))))
    ([x y] (fn [& args] (list (apply x args) (apply y args))))
    ([x y z] (fn [& args] (list (apply x args) (apply y args) (apply z args))))))

(defcheck solution-a504574
  (fn [& fns]
    (fn [& args]
      (into [] (for [f fns] (apply f args))))))

(defcheck solution-a5317091
  (fn [& f]
    (fn [& s]
      (map #(apply % s) f))))

(defcheck solution-a543ff78
  (fn [& fns]
    (fn [& vs] (map (fn [f] (apply f vs)) fns))))

(defcheck solution-a62dfcf6
  (fn __ [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-a63d9fb3
  (fn [& fns]
    (fn [& xs]
      (map
        #(apply % xs)
        fns))))

(defcheck solution-a64e758f
  (fn [& fs]
    (fn [& args]
      (for [f fs] (apply f args)))))

(defcheck solution-a659aca9
  (fn [& fs]
    (fn [& a] (map (fn [f]
                     (apply f a)) fs))))

(defcheck solution-a6744949
  (fn my-juxt
    [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-a6b2729b
  #(fn [& r] (map apply %& (repeat r))))

(defcheck solution-a6daeb38
  (fn [& fns]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fns))))

(defcheck solution-a7cda37a
  (fn bob [& fs]
    (fn jack [& xs]
      (if (empty? fs)
        []
        (cons (apply (first fs) xs) (apply (apply bob (rest fs)) xs))))))

(defcheck solution-a7d62f30
  (fn myjuxt [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-a84f91ab
  ;(fn [& fargs]
  ;       (fn [& args]
  ;         (loop [f fargs,
  ;                ret []]
  ;           (if (seq f)
  ;             (recur (rest f) (conj ret (apply (first f) args)))
  ;             ret))))
  (fn [& fns]
    (fn [& xs]
      (for [f fns] (apply f xs)))))

(defcheck solution-a8dab9b1
  (fn [& args1] (fn [& args2] (loop [fs (reverse args1) i args2 result '()] (if (empty? fs) result (recur (rest fs) i  (conj result (apply (first fs) i))))))))

(defcheck solution-a9c1a6fa
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-a9f82695
  (fn [ & fs ]
    (fn [ & ns ]
      (map #(apply % ns) fs))))

(defcheck solution-aa768be2
  #(fn [& args] (for [x %&] (apply x args))))

(defcheck solution-aa824b8
  (fn [& fs]
    (fn [& args]
      (map #(apply %1 args) fs))))

(defcheck solution-ab5b6e5e
  (fn my-juxt [& fs]
    (fn [& args]
      (loop [f fs a args r []]
        (if (empty? f)
          r
          (recur (rest f) a (conj r (apply (first f) a))))))))

(defcheck solution-ab9a06e8
  (fn j [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-abda98b0
  (fn myjuxt [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-ac8f8bc5
  (fn[& [args :as ls]] (fn[& [args2 :as ls2]] (loop[acc [] tmp ls]
                                                (if (empty? tmp)
                                                  (reverse acc)
                                                  (recur (cons (apply (first tmp) ls2) acc) (rest tmp)))))))

(defcheck solution-ad36100e
  (fn [& fs] (fn [& xs] (reduce #(conj %1 (apply %2 xs)) [] fs))))

(defcheck solution-ade6c309
  (fn [& s] #(map (fn [f] (apply f %&)) s)))

(defcheck solution-ae64326b
  (fn [& fs]
    (fn [& a]
      (for [f fs] (apply f a)))))

(defcheck solution-aefb9b31
  (fn new-juxt
    [& fs]
    (fn [& args] (vec (map #(apply % args) fs)))))

(defcheck solution-af328c6a
  (fn [& funs]
    (fn [& p]
      (for [fs funs]
        (apply fs p)
        ))))

(defcheck solution-af342991
  (fn my-juxt [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-af729af3
  (fn [ & args ] (fn [& x] (loop [ft args res []] (if (empty? ft) res
                                                                  (recur (rest ft) (conj res (if (= 1 (count x)) ((first ft) (last x)) (apply (first ft) x)))))))))

(defcheck solution-afa6bb26
  (fn [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-afcd3c86
  #(fn [& r] (for [t %&] (apply t r))))

(defcheck solution-afcf80fc
  (fn [& fs]
    (fn [& xs]
      (reduce (fn [acc f]
                (conj acc (apply f xs)))
        []
        fs))))

(defcheck solution-b0cd595b
  (fn [& ops] (fn [& vals] (reduce #(conj %1 (apply %2 vals)) [] ops))))

(defcheck solution-b116caf2
  (fn
    [& fns]
    (fn
      [& args]
      (for [f fns]
        (apply f args)))))

(defcheck solution-b19c7e7f
  (fn [& funs]
    (fn [& args]
      (map #(apply % args)
        funs))))

(defcheck solution-b1b5bbae
  (fn [& fs]
    (fn [& arg]
      (for [f fs]
        (apply f arg)))))

(defcheck solution-b24ee2b5
  (fn [& fs]
    (fn [& x] (map #(apply % x) fs))))

(defcheck solution-b2d0f0ac
  (fn [& fs]
    #(for [f fs]
       (apply f %&))))

(defcheck solution-b2fefea7
  (fn [& f]
    #(for [g f]
       (apply g %&))))

(defcheck solution-b327eaa0
  (fn [& fs]
    (fn [& args]
      (loop [ret [] fns fs]
        (if fns
          (recur (conj ret (apply (first fns) args))
            (next fns))
          ret)))))

(defcheck solution-b391aada
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs)
      )))

(defcheck solution-b3a77a4d
  (fn my-juxt [& fns]
    (fn [& args]
      (for [f fns] (apply f args)))))

(defcheck solution-b3ef0d62
  (fn [& fs] (fn [& args] (reduce #(conj % (apply %2 args)) [] fs))))

(defcheck solution-b409d22f
  (fn [ & fns ]
    (fn [ & args ]
      (map #(apply % args) fns))))

(defcheck solution-b437883d
  (fn [& fs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fs))))

(defcheck solution-b4455170
  (fn mapf [ & fcol]
    (fn lambda
      ([col]  (map  #(% col) fcol) )
      ([a & other] (let [cols (cons a other) ]
                     (map #(apply % cols) fcol)
                     )
       )
      )
    ))

(defcheck solution-b46be970
  (fn [& fs]
    (fn [& args]
      (reduce #(conj % (apply %2 args)) [] fs))))

(defcheck solution-b53c61be
  (fn [& fns]
    (fn [& args]
      (map
        (fn [fun]
          (apply fun args))
        fns))))

(defcheck solution-b592a3db
  (fn [& fns]
    (fn [& params]
      (map #(apply % params) fns))))

(defcheck solution-b627db4c
  (fn [& fs]
    (fn [& rs]
      (map #(apply % rs) fs))))

(defcheck solution-b63829f7
  (fn [& fs] (fn [& a] (map #(apply % a) fs))))

(defcheck solution-b64a9a1a
  (fn m [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-b6780ba2
  (fn [& fns]
    (fn [& xs]
      (map #(apply % xs) fns)
      )
    ))

(defcheck solution-b693be43
  (fn [& funcs]
    (fn [& args]
      (for [f funcs]
        (apply f args)))))

(defcheck solution-b79dde3e
  (fn [& r] (fn [& a] (map #(apply % a) r))))

(defcheck solution-b7cfa675
  (fn [& fs]
    (fn [& xs]
      (loop [fs fs
             acc []]
        (if (empty? fs)
          acc
          (recur (rest fs) (conj acc (apply (first fs) xs))))))))

(defcheck solution-b7e6fd0c
  (fn [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-b801de2b
  (fn [& fs] (fn [& args] (into [] (map #(apply %1 args) fs)))))

(defcheck solution-b83aff84
  (fn [& fs]
    (fn [& args]
      (loop [ls [] fs fs]
        (if (seq fs)
          (recur (conj ls (apply (first fs) args)) (rest fs))
          ls)))))

(defcheck solution-b8eb2a0e
  (fn my-juxt [& funcs]
    (fn [& args]
      (for [f funcs]
        (apply f args)))))

(defcheck solution-b9920aef
  (fn juxtaposition [& fns]
    (fn [& args]
      (reduce (fn [result-vector f]
                (conj result-vector (apply f args))) [] fns))))

(defcheck solution-b9e374bd
  (fn [& fs] (fn [& args] (map (fn [f] (apply f args)) fs))))

(defcheck solution-bad50473
  (fn [& fs]
    (fn [& x] (map #(apply % x) fs))))

(defcheck solution-bb50ef30
  (fn my-juxt [ & s]
    (fn [ & s1 ]
      (for [f s]
        (apply f s1)))))

(defcheck solution-bb75cf27
  (fn [& fs] (fn [& xs] (for [f fs] (apply f xs) )  )  ))

(defcheck solution-bba5377
  (fn [& fs]
    (fn [& seq] (map #(apply % seq) fs))))

(defcheck solution-bba5d150
  (fn j ([] (fn [& args] []))
    ([a] (fn [& args] [(apply a args)]))
    ([a b] (fn [& args] [(apply a args) (apply b args)]))
    ([a b & rest] (fn [& args] (concat (apply (j a b) args)
                                       (apply (apply j rest) args))))))

(defcheck solution-bbeaba42
  (fn [ & fns ]
    (fn [ & args ]
      (loop [l fns result []]
        (if
         (empty? l)
          result
          (recur (rest l) (conj result (apply (first l) args))))))))

(defcheck solution-bc0c74ee
  (fn [& fs]
    (fn [& args]
      (map (fn [f] (apply f args)) fs))))

(defcheck solution-bcb3eba4
  (fn juxtaposition [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-bd9ea6f8
  (fn [& fs] (fn [& s] (for [f fs] (apply f s)))))

(defcheck solution-be4f11f8
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs)
      )
    ))

(defcheck solution-bf92ebe3
  (fn [& fs]
    (fn [& args]
      (for [f fs] (apply f args)))))

(defcheck solution-c0577136
  (fn [& fs] (fn [& c] (map #(apply % c) fs))))

(defcheck solution-c08c6235
  (fn [& fs]
    (fn [& args]
      (loop [ffs fs out []]
        (if (empty? ffs)
          out
          (recur (rest ffs) (conj out (apply (first ffs) args))))))))

(defcheck solution-c0bdbc03
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-c0bf70e0
  (fn[& f] (fn [& x] (map #(apply % x) f))))

(defcheck solution-c10d17e0
  (fn [& f]
    (fn [& x]
      (map #(apply % x) f))))

(defcheck solution-c17e50e3
  (fn [& funcs]
    (fn [& args]
      (map (fn [f]
             (apply f args))
        funcs))))

(defcheck solution-c1d48eeb
  (fn [& fs]
    (fn [& ks] (map (fn [f] (apply f ks)) fs))))

(defcheck solution-c21b1391
  (fn
    ([f g]
     (fn [x] [(f x) (g x)]))
    ([f g h]
     (fn
       ([x & args] [(apply f x args) (apply g x args) (apply h x args)])))))

(defcheck solution-c2b7ec9a
  (fn
    [& ops]
    (fn
      [& li]
      (map
        (fn
          [op]
          (apply op li))
        ops))))

(defcheck solution-c36902ee
  (fn [& args]
    (fn [& arg1]
      (map #(apply % arg1) args))))

(defcheck solution-c3e596b1
  (fn [& fs] #(map apply fs (repeat %&))))

(defcheck solution-c3eec74d
  (fn my-juxt [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-c41ebefd
  (fn j [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-c42695a0
  (fn [& fs]
    (fn [& args]
      (for [f fs] (apply f args))
      )
    ))

(defcheck solution-c4a32055
  (fn [& fs]
    (fn [& params]
      (map #(apply % params) fs))))

(defcheck solution-c4b5e876
  (fn jxt
    ([f g]   (fn [& args] (list (apply f args) (apply g args))))
    ([f g h] (fn [& args] (list (apply f args) (apply g args) (apply h args))))))

(defcheck solution-c4f6714b
  (fn [& f]
    (fn [& args]
      (loop [tmpf f ans '()]
        (if (empty? tmpf)
          (reverse ans)
          (recur (rest tmpf) (conj ans (apply (first tmpf) args))))))))

(defcheck solution-c5433979
  (fn [& fs]
    (fn [& args]
      (reduce (fn [rs f] (conj rs (apply f args))) [] fs))))

(defcheck solution-c555178
  (fn [& fns]
    (fn [& args]
      (reduce (fn [acc item]
                (conj acc (apply item args)))
        []
        fns))))

(defcheck solution-c5aad651
  (fn [& fns]
    (fn [& xs] (map #(apply % xs) fns))))

(defcheck solution-c5cbfb55
  (fn [& F] #(for [f F] (apply f %&))))

(defcheck solution-c5fbdc1b
  (fn [& funcs]
    (fn [& args]
      (map (fn [f] (apply f args)) funcs))))

(defcheck solution-c6188e75
  (fn [ & f]
    (fn [ & args]
      (map #(apply % args) f))))

(defcheck solution-c6745385
  (fn [& funs]
    (fn [& args]
      (map #(apply % args) funs)
      )
    ))

(defcheck solution-c696a790
  (fn [& fncs]
    (fn [ & args]
      (map #(apply % args) fncs))))

(defcheck solution-c6a7605d
  (fn jxt [& fns]
    (fn [& args]
      (concat (for [fn fns]
                (apply fn args))))))

(defcheck solution-c76741f8
  (fn [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-c78e8203
  (fn [& fns]
    (fn [& args]
      (reduce #(conj % (apply %2 args)) [] fns))))

(defcheck solution-c79489c0
  (fn [& fs] (fn [& xs] (map #(apply %1 xs) fs))))

(defcheck solution-c7bb0d3a
  (fn [& fns] (fn [& args]
                (map #(apply % args) fns))))

(defcheck solution-c80e5759
  (fn jux [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-c84fa46
  (fn [& fs]
    (fn [& as]
      (loop [acc []
             f fs]
        (if f
          (recur (conj acc (apply (first f) as)) (next f))
          acc)))))

(defcheck solution-c89c6c39
  (fn [& fargs] (fn [ & args ]
                  (concat (map #(apply % args) fargs)))))

(defcheck solution-c8ba8f64
  (fn [& f] (fn [& v] (map #(apply % v) f))))

(defcheck solution-c8e8d14f
  (fn new-juxt [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-c8fa3740
  (fn newjuxt [& f]
    (fn g [& x]
      (for [h f]
        (apply h x)))))

(defcheck solution-c949e47b
  (fn  [ & funcs] (fn [& args] (map #(apply % args) funcs))))

(defcheck solution-c9e0524
  (fn [& fns]
    (fn [& x]
      (vec (map #(apply % x) fns)))))

(defcheck solution-ca585e56
  (fn [& funs]
    (fn [& args]
      (for [fun funs]
        (apply fun args)))))

(defcheck solution-caa61293
  (fn [& fs]
    (fn [& xs]
      (for [f fs] (apply f xs)))))

(defcheck solution-cc507d63
  (fn
    [& f]
    (fn [& a]
      (loop [f f
             r []]
        (if (empty? f)
          r
          (recur
            (rest f)
            (conj r (apply (first f) a))))))))

(defcheck solution-ccaee627
  (fn my-juxt [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-ccc91383
  (fn [& fs]
    (fn [& args]
      (loop [result []  rf fs]
        (if (empty? rf) result
                        (recur (conj result (apply (first rf) args)) (rest rf))
                        )
        )
      )
    ))

(defcheck solution-ccf4661e
  (fn jp [& fns]
    (fn inner [& args] (loop [fn fns result []]
                         (if (empty? fn) result (recur (drop-last fn) (cons (apply (last fn) args) result)))))))

(defcheck solution-cd0dae30
  (fn [& f]
    (fn [& a]
      ((fn [f r]
         (if (empty? f)
           r
           (recur (rest f)
             (concat r [(apply (first f) a)]))))
       f []))))

(defcheck solution-cd592248
  (fn [ & xs]
    (fn [& args]
      (map #(apply % args)  xs))))

(defcheck solution-cdcdfa29
  (fn myjuxt [& fs]
    (let [fs (list* fs)]
      (fn
        ([] (reduce #(conj % (%2)) [] fs))
        ([x] (reduce #(conj % (%2 x)) [] fs))
        ([x y] (reduce #(conj % (%2 x y)) [] fs))
        ([x y z] (reduce #(conj % (%2 x y z)) [] fs))
        ([x y z & args] (reduce #(conj % (apply %2 x y z args)) [] fs))))))

(defcheck solution-cde93198
  (fn my-juxt [f & fns]
    (reduce
      (fn [init f]
        #(conj (apply init %&) (apply f %&)))
      #(vector (apply f %&))
      fns)))

(defcheck solution-ce464a15
  (fn my-juxt [& funcs]
    (fn [& args]
      (for [f funcs]
        (apply f args)))))

(defcheck solution-ce6104c9
  (fn [& fs]
    (fn [& args](map #(apply % args) fs))))

(defcheck solution-ce8dc477
  (fn [& f] (fn [& c] (for [i f] (apply i c)))))

(defcheck solution-ceac2aa1
  (fn [& args	]
    (fn [& xs]
      (map #(apply %1 xs) args))))

(defcheck solution-cf2c3713
  (fn [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-cf4d1e32
  (fn j [& args]
    (fn [& x]
      (map #(apply % x) args)
      )
    ))

(defcheck solution-d051393b
  (fn my-juxt
    ([f]
     (fn
       ([] [(f)])
       ([x][(f x)])
       ([x & y] [(apply f (list* x y))])))
    ([f & g ]
     (let [g (list* f g)]
       (fn
         ([] (reduce #(conj %1 (%2)) [] g))
         ([x ] (reduce #(conj %1 (%2 x)) [] g))
         ([x & y]
          (let [y (list* x y)]
            (reduce #(conj %1 (apply %2 y)) [] g))))))))

(defcheck solution-d0bd779b
  (fn my-juxt [& fs]
    (fn [& args]
      (loop [ret [] fs fs]
        (if (seq fs)
          (recur (conj ret (apply (first fs) args))
            (rest fs))
          ret)))))

(defcheck solution-d10759bd
  (fn [& fns]
    (fn [& args]
      (for [ f fns] (apply f args)))))

(defcheck solution-d1403b54
  (fn [& f]
    (fn [& a] (map #(apply % a) f))))

(defcheck solution-d173e117
  (fn soln [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-d2204c7e
  (fn[& a] (fn[& b] (map #(apply % b) a))))

(defcheck solution-d23eae04
  (fn [& fns]
    (fn [& vs]
      (for [g fns]
        (apply g vs)))))

(defcheck solution-d2678c0d
  (fn [& fs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args) ) [] fs))))

(defcheck solution-d3315735
  (fn jus [& fs]
    (fn [& params]
      (map #(apply % params) fs))))

(defcheck solution-d333b878
  (fn [& fs]
    (fn [& s]
      (map #(apply % s) fs))))

(defcheck solution-d38cfcc8
  (fn __ [f & fs]
    (if (empty? fs)
      (fn [& a]
        (vec (list (apply f a))))
      (fn [& a]
        (into (vec (list (apply f a)))
          (apply (apply __ fs) a))))))

(defcheck solution-d3a87c49
  (fn [& funcs]
    (fn [& args]
      (map  #(apply % args) funcs))))

(defcheck solution-d3c7ea1e
  (fn [& fs] (fn [& x] (map #(apply %1 x) fs))))

(defcheck solution-d3dd51a3
  (fn [& fs]
    (fn [& args]
      (map
        (fn [f] (apply f args))
        fs)
      )))

(defcheck solution-d6dda4c2
  (fn [& fns]
    (fn [& targets]
      (for [f fns]
        (apply f targets)))))

(defcheck solution-d6ebbc4c
  (fn [& fns]
    (fn [& args]
      (map (fn [f] (apply f args)) fns))))

(defcheck solution-d717308a
  (fn [& f]
    (fn [& coll]
      (map #(apply % coll) f))))

(defcheck solution-d7a06bc0
  (fn [& fns]
    (fn [& args] (map #(apply % args) fns))))

(defcheck solution-d7d2d9f9
  (fn j [& f]
    (fn [& x]
      (vec (map #(apply % x) f)))))

(defcheck solution-d8bf86ea
  (fn juxtaposition [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-d939630a
  (fn [& fs]
    (fn [ & xs]
      (for [f fs]
        (apply f xs)))))

(defcheck solution-d9c71b88
  (fn [& functions]
    (fn [& v]
      #_(println (count v))
      (if (> (count v) 1)
        (for [f functions]
          (reduce f v))
        (for [f functions]
          (f (first v)))))))

(defcheck solution-da6fb6e2
  (fn [& fs]
    (fn [& args]
      (for [f fs]
        (apply f args)))))

(defcheck solution-daeeb2f5
  (fn[& f] (fn[& x] (map #(apply % x) f))))

(defcheck solution-db23fffb
  (fn [& fns]
    (fn [& a]
      (map #(apply % a) fns))))

(defcheck solution-db863f7f
  (fn [& args] (fn [& x] ( reduce #(concat %1 (list (apply %2 x))) () args ))))

(defcheck solution-dbd101a2
  (fn func [f & fs]

    (fn [& x]

      (if (= fs nil)

        (cons (apply f x) [])

        (cons (apply f x) (apply (apply func fs) x))))))

(defcheck solution-dc019820
  (fn [& functions] (fn [& args] (map #(apply % args) functions))))

(defcheck solution-dc5d905c
  (fn my-juxt [& funs]
    (fn [& args] (map #(apply % args) funs))))

(defcheck solution-dc91b0dd
  (fn [ & funs]
    (fn [& args]
      (map #(apply % args) funs))))

(defcheck solution-dd132f27
  (fn jux
    [& funcs]
    (fn [& args] (for [f funcs] (apply f args)))))

(defcheck solution-ddeb724d
  (fn [& fs]
    (fn [x & xs]
      (if (nil? xs)
        (reduce (fn [acc f] (concat acc (list (f x)))) () fs)
        (reduce (fn [acc f] (concat acc (list (reduce f (conj xs x))))) () fs)
        )
      )
    ))

(defcheck solution-de1d893a
  (fn  [& fs]
    (fn  [& args]
      (map #(apply % args) fs))))

(defcheck solution-de9100a1
  (fn [& fs] (fn [& args] (map #(apply % args) fs))))

(defcheck solution-deae448a
  (fn [& fs]
    (fn [& xs]
      (reduce #(conj %1 (apply %2 xs)) [] fs)
      )
    ))

(defcheck solution-df4da996
  (fn [& x]
    (fn [& y]
      (letfn
       [(roll
          [x y]
          (if (empty? x)
            []
            (concat
             [(apply (first x) y)]
             (roll (rest x) y)
             )))]
        (roll x y)
        ))))

(defcheck solution-df615ef
  (fn
    ([& f] (fn [& a] (map #(apply % a) f)))))

(defcheck solution-e0200994
  (fn [& f]
    (fn [& args]
      (map #(apply % args) f))))

(defcheck solution-e06d6593
  (fn [& fns]
    (fn [& as]
      (map #(apply % as) fns))))

(defcheck solution-e10b2d6f
  #_(fn jxt [& ops]
      (fn [& more]
        (map #(apply % more) ops)))


  (fn jxt [& ops]
    (fn [& more]
      (for [op ops] (apply op more)))))

(defcheck solution-e136cf8a
  (fn [& fargs]
    (fn [& args]
      (for [f fargs]
        (apply f args)))))

(defcheck solution-e1597f0
  (fn [& fs]
    #(map (fn [f] (apply f %&)) fs)))

(defcheck solution-e24326c0
  (fn [ & fs]
    (fn [ & args]
      (map #(apply % args) fs))))

(defcheck solution-e29fa741
  (fn [& f] (fn [& v] (vec (map #(apply % v) f)))))

(defcheck solution-e2eb5413
  (fn [& fs]
    (fn [& xs]
      (loop [out [(apply (first fs) xs)] in (rest fs)]
        (if (empty? in) out (recur (conj out (apply (first in) xs)) (rest in)))))))

(defcheck solution-e2ec7a54
  (fn myjux

    ([] (fn [& args] nil))
    ([a & rst]
     (fn [& args]
       (conj (apply(apply myjux rst) args) (apply a args))))))

(defcheck solution-e3571dfd
  (fn [& Fs]
    (fn [& args]
      (for [f Fs]
        (apply f args)
        )

      )
    ))

(defcheck solution-e3eea46
  (fn [& args1]
    (fn [& args2]
      (loop [f args1 r[]]
        (if (empty? f) r
                       (recur (rest f) (conj r (apply (first f) args2))))))))

(defcheck solution-e480e0e2
  (fn [& r] #(map (fn [x](apply x %&)) r)))

(defcheck solution-e48218d1
  (fn [ & fs] (fn  [& x ]
                (into  []  (map #(apply % x) fs))
                )))

(defcheck solution-e4fccfcf
  (fn k [& fs]
    (fn [& args]
      (reduce (fn [acc f]
                (conj acc (apply f args)))
        []
        fs))))

(defcheck solution-e541525d
  (fn [& fs]
    (fn [& args] (map #(apply % args) fs))))

(defcheck solution-e5b51b19
  #(fn [& s] (map (fn [f] (apply f s)) %&)))

(defcheck solution-e602d7ae
  (fn [& fs]
    (fn [& x]
      (reduce #(conj %1 (apply %2 x)) [] fs))))

(defcheck solution-e6408ec8
  (fn mapfs [& fs] (fn [& xs] (into [] (map #(apply %1 xs) fs)))))

(defcheck solution-e675c829
  (fn [& f]
    (fn [& x]
      (map #(apply % x) f))))

(defcheck solution-e695f4ae
  (fn [& fargs]
    (fn [& args]
      (reduce #(conj % (apply %2 args)) [] fargs))))

(defcheck solution-e716fc7a
  (fn [& fs] (fn [& xs] (map #(apply % xs) fs))))

(defcheck solution-e8051ddc
  (fn [& fns]
    (fn [& args]
      (mapv (fn [f] (apply f args)) fns))))

(defcheck solution-e894b303
  (fn juxt--map
    [& fns] {:pre [(every? ifn? fns)]}
    (fn [& args]
      (map #(apply % args) fns))))

(defcheck solution-e8c69172
  (fn my-juxt [& f]
    (fn [& args]
      (map #(apply % args) f))))

(defcheck solution-e8c878cf
  (fn [& f]
    (fn [& args]
      (map #(apply %1 %2) f (repeat args)))))

(defcheck solution-e8d65e9b
  (fn [& f]
    (fn [& args]
      (map #(apply % args) f))
    ))

(defcheck solution-e8e067b1
  (fn
    [& fs]
    (fn [& args] (vec (map #(apply % args) fs)))))

(defcheck solution-e8e15a46
  (fn [& funs] (fn [& d] (map #(apply % d) funs))))

(defcheck solution-eb34bd21
  (fn my-juxt [& fns]
    (fn [& args]
      (reduce (fn [v f] (conj v (apply f args))) [] fns))))

(defcheck solution-eb9dac88
  (fn [& f]
    (fn [& x]
      (map #(apply % x) f))))

(defcheck solution-eba29e8e
  (fn [& fs]
    (fn [& y] (reduce (fn [t v] (conj t (apply v y))) [] fs))))

(defcheck solution-ec79efe5
  (fn [& f]
    (fn [& n]
      (map #(apply % n) f))))

(defcheck solution-ecd6f44a
  (fn juxt* [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-ecdf3675
  (fn me
    [& ops]

    (let [ret (fn myresturn [& args]

                (map #(apply % args) ops)

                )

          ]

      ret)

    ))

(defcheck solution-ed4ad9d5
  (fn [& args]
    (fn [& args2]
      (map #(apply % args2) args))))

(defcheck solution-ed670ffa
  (fn[& fns]
    (fn[& args](map #(apply % args) fns))))

(defcheck solution-edf443fe
  (fn [& funcs ]
    (fn [ & args ]
      (loop [f1 (first funcs)
             restf (rest funcs)
             out nil]
        (if (nil? f1)
          (reverse out)
          (recur
            (first restf)
            (rest restf)
            (cons (apply f1 args) out)))))))

(defcheck solution-ee04ff1b
  (fn juxt2 [& fns]
    (fn [& args]
      (map #(apply % args) fns))
    ))

(defcheck solution-ee2b2fd4
  (fn [& funcs]
    (fn [& args]
      (map #(apply % args) funcs))))

(defcheck solution-ef16ecea
  (fn j [& op] (fn [&  arg] (mapv #(apply % arg) op))))

(defcheck solution-ef464eb2
  (fn juxtapose [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-ef4cffc3
  (fn applyall [& fns]
    (fn [& args]
      (for [f fns]
        (apply f args)))))

(defcheck solution-f08d854
  (fn [& f] #(for [g f] (apply g %&))))

(defcheck solution-f0b5c016
  (fn [& fns]
    (fn [& args]
      (map #(apply %1 args) fns))))

(defcheck solution-f0c441af
  (fn [& funcs]
    (fn [& args]
      (for [func funcs]
        (apply func args)
        )
      )
    ))

(defcheck solution-f1f4392e
  (fn [& f]
    (fn [& xs] (map #(apply % xs) f))))

(defcheck solution-f250d372
  (fn jux [& functions]
    (fn [& params]
      (reduce #(conj %1 (apply %2 params)) [] functions))))

(defcheck solution-f255b09f
  (fn jux [& functions]
    (fn [& args]
      (map
        (fn [f] (apply f args))
        functions))))

(defcheck solution-f259befc
  (fn [& fs] (fn [& xs] (for [f fs] (apply f xs)))))

(defcheck solution-f29b0211
  (fn
    ([f g]
     (fn
       ([][f g])
       ([x][(f x)(g x)])
       ([x y][(f x y)(g x y)])
       ([x y z][(f x y z)(g x y z)])
       ([x y z & args][(apply f x y z args)(apply g x y z args)])
       )
     )
    ([f g h]
     (fn
       ([][f g h])
       ([x][(f x)(g x)(h x)])
       ([x y][(f x y)(g x y)(h x y)])
       ([x y z][(f x y z)(g x y z)(h x y z)])
       ([x y z & args][(apply f x y z args)(apply g x y z args)(apply h x y z args)])
       )
     )
    ))

(defcheck solution-f3346d05
  (fn [& f]
    (fn [& args] (map #(apply % args) f))))

(defcheck solution-f3772511
  #(fn [& n] (map (fn [x] (apply x n)) %&)))

(defcheck solution-f4531495
  (fn [& fns]
    #(for [f fns]
       (apply f %&))))

(defcheck solution-f4620647
  (fn my-juxt[& func-list]
    (fn [& args]
      (map #(apply % args) func-list)
      )
    ))

(defcheck solution-f589602b
  (fn [& fs]
    (fn [& xs]
      (for [f fs]
        (apply f xs)
        )
      )
    ))

(defcheck solution-f5cc04d4
  (fn j [f & args] (fn [& x]
                     (cons (apply f x)
                       (if (empty? args) []
                                         (apply (apply j args) x))))))

(defcheck solution-f63bd1a3
  (fn my-juxt [& funcs]
    (fn [& args]
      (map #(apply %1 args) funcs))))

(defcheck solution-f6740d2
  (fn
    [& fs]
    (fn
      [& args]
      (vec (map #(apply % args) fs)))))

(defcheck solution-f6b7d579
  (fn [& cf] (fn [& cv]
               (vec (for [f cf]
                      (apply f cv)
                      )))))

(defcheck solution-f6e3e9c2
  (fn [& fs]
    (fn [& xs]
      (for [f fs]
        (apply f xs)))))

(defcheck solution-f74bf472
  (fn [ & funs ]
    (fn [ & args ]
      (map apply funs (repeat args)))))

(defcheck solution-f750c259
  (fn juxt-alt [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-f780ef2b
  (fn [& funcs]
    (fn [& args] (map #(apply % args) funcs))))

(defcheck solution-f7be5627
  (fn [f & flist]
    (fn [x & y]
      (let [lf (into [f] flist)]
        (loop [i  0 z []]
          #_(println "z" z "x" x)
          (cond
            (= i (count lf))  z
            :else (recur (inc i) (conj z (apply (get lf i) (into [x] y))))
            )
          )
        )
      )
    ))

(defcheck solution-f7eb8da7
  (fn [& funcs] (fn [& vals](map #(apply %  vals) funcs))))

(defcheck solution-f8a89182
  (fn map-apply [& fs]
    (fn [& args]
      (map #(apply %1 args) fs))))

(defcheck solution-f8e2202c
  (fn [& f] (fn [& x] (map #(apply %1 x) f))))

(defcheck solution-f94a2b62
  (fn my-juxt1
    [& f]
    (letfn [(fun [s & x] (map apply s (repeat x)))]
      (partial fun f))))

(defcheck solution-f9efcd3f
  (fn [& fns]
    (fn [& args]
      (map (fn [f] (apply f args)) fns))))

(defcheck solution-fad1a1a4
  (fn [& fs] #(map (fn [f] (apply f %&)) fs)))

(defcheck solution-fb209827
  (fn [& x] (fn [& y] (map #(apply % y) x))))

(defcheck solution-fb563688
  (fn [& functions]
    (fn [& args] (map (fn [f] (apply f args)) functions))))

(defcheck solution-fb6522e6
  (fn [& l]
    #(for [f l] (apply f %&))))

(defcheck solution-fbdac19a
  (fn [& fns]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] fns))))

(defcheck solution-fc06b945
  (fn [& fs]
    (fn [& args] (reduce #(conj % (apply %2 args)) [] fs))))

(defcheck solution-fc282c61
  (fn [& funcs]
    (fn [& args]
      (reduce #(conj %1 (apply %2 args)) [] funcs))
    ))

(defcheck solution-fc59673c
  (fn [& xs]
    (fn [& args]
      (map #(apply % args) xs))))

(defcheck solution-fc74872f
  (fn [& funcs]
    (fn [& args] (map #(apply % args) funcs))))

(defcheck solution-fc856917
  #(fn [& args] (map apply %& (repeat args))))

(defcheck solution-fcf826a1
  (fn [& fns]
    (fn [& args]
      (mapv #(apply % args) fns))))

(defcheck solution-fd2d38f6
  (fn [& fs]
    (fn [& xs]
      (map #(apply % xs) fs))))

(defcheck solution-fd6143ef
  (fn [& fns]
    (fn [& sq]
      (reduce (fn [res f]
                (conj res (apply f sq))) [] fns))))

(defcheck solution-fd91b54b
  (fn [& args]
    (fn [& y] (for [f args] (apply f y)))))

(defcheck solution-fe02bcf
  (fn [& flist] (fn [& args] (map #(apply % args) flist))))

(defcheck solution-fed703bb
  (fn [& fs]
    (fn [& args]
      (map #(apply % args) fs))))

(defcheck solution-fed7f18b
  (fn f [& fs]
    (fn [& args]
      (for [ff fs]
        (apply ff args)))))

(defcheck solution-ff81d65b
  (fn [& funs]
    (fn [& args]
      (map #(apply % args) funs))))

(defcheck solution-ffb8737c
  (fn [& fs]
    (fn [& xs]
      (map
        (fn [f]
          (apply f xs)
          )
        fs
        )
      )
    ))