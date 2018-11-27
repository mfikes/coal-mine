(ns coal-mine.problem-46
  (:require [coal-mine.checks :refer [defcheck-46] :rename {defcheck-46 defcheck}]
            [clojure.test]))

(defcheck solution-107a3725
  (fn [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-11c9eb98
  #( fn [x y] (% y x)))

(defcheck solution-1239ce7
  (fn [f] (fn [index coll] (f coll index))))

(defcheck solution-124c6e73
  (fn [a-fnc]
    (fn[arg1 arg2] (a-fnc arg2 arg1))))

(defcheck solution-1291d8be
  #(fn [b a] (% a b)))

(defcheck solution-12cd7872
  (fn [f]
    (fn [x y] (f y x))
    ))

(defcheck solution-12fe97a9
  #(fn [& a]
     (apply % (reverse a))))

(defcheck solution-132c9e00
  (fn [f] (comp #(apply f %) #(reverse %&))))

(defcheck solution-133e7044
  (fn [afn] (fn [a1 a2] (afn a2 a1))))

(defcheck solution-135c5d71
  (fn [f]
    #(f %2 %1)))

(defcheck solution-1391cc32
  (fn [f]
    (fn [a b]
      (f b a)
      )
    ))

(defcheck solution-14cbb6a1
  (fn flip [op] (fn [a b] (op b a))))

(defcheck solution-158c85c6
  #(fn [& m]
     (apply % (reverse m))))

(defcheck solution-163be9c9
  #(fn [z y] (% y z)))

(defcheck solution-168c89bc
  #(fn[& args](apply % (reverse args))))

(defcheck solution-16a7af39
  (fn [f] (fn [& xs] (apply f (reverse xs)))))

(defcheck solution-17507bea
  (fn [func] (fn [v1 v2] (func v2 v1))))

(defcheck solution-1775810e
  #(let [f %] (fn [x c] (f c x))))

(defcheck solution-1783d2c9
  #(let [f %]
     (fn [x y]
       (f y x))))

(defcheck solution-19979abb
  (fn rev-args [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-19b89729
  (fn [f] (fn [& s] (apply f (reverse s)))))

(defcheck solution-1c9c09f2
  #(fn [& params]
     (apply % (reverse params))))

(defcheck solution-1d85aede
  (fn yep [x] (fn [y z] (x z y))))

(defcheck solution-1dbb747d
  (fn [f] (fn [y x] (f x y))))

(defcheck solution-1e4795f1
  (fn [func]
    (fn [& args]
      (apply func (reverse args)))))

(defcheck solution-1efbb978
  (fn [f]
    (fn [a b] (f b a))))

(defcheck solution-1f995b60
  (fn [op]
    (fn [a b] (op b a))))

(defcheck solution-1fc21462
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-2007dd7c
  (fn flipping-out
    [f]
    (fn [a b] (f b a))))

(defcheck solution-2103c0cd
  (fn [f] (comp #(apply f %) reverse list)))

(defcheck solution-21065bb6
  (fn [fx]
    (fn [arg1 arg2]
      (fx arg2 arg1))))

(defcheck solution-214c03b6
  #(fn[& r] (apply %1 (reverse r))))

(defcheck solution-22e19b86
  (fn [op] (fn [ x y ] (op y x))))

(defcheck solution-22f1b068
  (fn[f]
    (fn [b a]
      (f a b))))

(defcheck solution-237b08f
  #(fn [x y] (%1 y x)))

(defcheck solution-23d1ed08
  #(fn [& y] (apply % (reverse y))))

(defcheck solution-24ba056e
  (fn [f]
    (fn flipped [& args]
      (apply f (reverse args)))))

(defcheck solution-26c8064d
  (fn [x] (fn [& a] (apply x (reverse a)))))

(defcheck solution-26cc0b3a
  (fn reverse-args
    [input-fn]
    (fn
      [& args]
      (apply input-fn (reverse args)))))

(defcheck solution-26f8e283
  (fn flip[f](fn[x y] (f y x))))

(defcheck solution-285da121
  (fn  [f] (fn [& a] (apply f (reverse a)))))

(defcheck solution-294086a1
  (fn rev [f]
    (fn [a1 a2]
      (f a2 a1))))

(defcheck solution-29587b52
  (fn flip [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-2a3d4f00
  (fn
    [f]
    (fn [a b] (f b a))))

(defcheck solution-2abb9670
  (fn [func] (fn [& args] (apply func (reverse args)))))

(defcheck solution-2b2640db
  (fn flip [func] (fn[head-arg tail-arg] (func tail-arg head-arg))))

(defcheck solution-2b462172
  #(fn [& a] (apply % (reverse a))))

(defcheck solution-2ba24875
  (fn
    [op]
    (fn
      [a b]
      (op b a))))

(defcheck solution-2bb04585
  (fn [f]
    (fn [a b]
      (f b a))))

(defcheck solution-2d37d1e
  ;(fn [f] (comp (partial apply f) reverse list))

  #(fn [a b] (% b a)))

(defcheck solution-2f8700c8
  (fn [fn-to-wrap]
    (fn [& xs]
      (apply fn-to-wrap (reverse xs)))))

(defcheck solution-2fc7a678
  (fn [fun]
    (fn [arg2 arg1]
      (fun arg1 arg2)
      )))

(defcheck solution-30626e5e
  (fn [f] (fn [ & args] (apply f (reverse args))) ))

(defcheck solution-315db162
  (fn flip-out [func]
    (fn [arg2 arg1] (func arg1 arg2))))

(defcheck solution-32a0e53d
  (fn flip [f] (fn [x y] (f y x))))

(defcheck solution-32d5d766
  (fn [fun]
    (fn [& args]
      (apply fun (reverse args)))))

(defcheck solution-33807410
  (fn swt [f]
    (fn [x y]
      (f y x)
      )
    ))

(defcheck solution-33f090a0
  (fn [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-3501ab9b
  (fn [f]
    (fn [& x]
      (apply f (reverse x)))))

(defcheck solution-35a94b3e
  (fn [f] #(f %2  %)))

(defcheck solution-35e3953
  (fn [f]
    #(f %2 %1)))

(defcheck solution-361cad3e
  (fn rvargs [f]
    (fn [a b] (f b a))))

(defcheck solution-36446dc5
  (fn [f] (fn [& l] (apply f (reverse l)))))

(defcheck solution-365efd43
  (fn [f](fn[& a](apply f (reverse a)))))

(defcheck solution-36e2e7eb
  (fn [fun]
    (fn [& args] (apply fun (reverse args)))))

(defcheck solution-377f768b
  (fn [op] #(op %2 %1)))

(defcheck solution-379fb78c
  (fn [f] #(apply f (reverse %&))))

(defcheck solution-38c7d4d3
  (fn [f]
    (fn [x y] (f y x))))

(defcheck solution-39fe4634
  (fn  [f] (fn [& r] (apply f (reverse r)) )))

(defcheck solution-3a41ab1
  (fn [f]
    (fn [& l]
      (apply f (reverse l)))))

(defcheck solution-3a6a667b
  (fn [f]
    (fn [& more]
      (apply f (reverse more))
      )))

(defcheck solution-3a706b8b
  (fn [f]
    (fn [& a]
      (apply f (reverse a)))))

(defcheck solution-3acabb82
  (fn [function] (fn [& args] (apply function (reverse args)))))

(defcheck solution-3afe1a45
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-3b2eaabc
  (fn flip [f]
    (fn [b a]
      (f a b))))

(defcheck solution-3cc1d149
  (fn [f] #(f %2 %)))

(defcheck solution-3d00d3d6
  (fn flipout [fun] (fn [arg1 arg2] (fun arg2 arg1))))

(defcheck solution-3ece40bc
  (fn [x]
    (fn [y z] (x z y))))

(defcheck solution-3f68121
  (fn [f] (fn [n1 n2] (f n2 n1))))

(defcheck solution-41047304
  (fn [f] (comp (partial apply f) reverse vector)))

(defcheck solution-41a3bae4
  (fn [f]
    (fn [x y] (f y x))))

(defcheck solution-41cec275
  (fn [app]
    (fn [& args]
      (apply app (reverse args)))))

(defcheck solution-4470e55b
  partial #(% %3 %2))

(defcheck solution-45be4a65
  (fn [func]
    (fn [a b]
      (func b a))))

(defcheck solution-4616b6fd
  (fn [f] (fn [a1 a2] (f a2 a1))))

(defcheck solution-462449a9
  (fn flip[f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-47a6acbb
  (fn [f] (fn [& args]
            (apply f (reverse args)))))

(defcheck solution-47d3bdc1
  (fn flip [f]
    (fn [x y]
      (f y x))))

(defcheck solution-484d729b
  #(fn [& a] (->> a (reverse) (apply %))))

(defcheck solution-4874a072
  (fn flip [f]
    #(f %2 %1)))

(defcheck solution-489f68ed
  (fn
    [f]
    (fn
      [x y]
      (f y x))))

(defcheck solution-492d12ac
  #(fn [x y] (% y x)))

(defcheck solution-4a6d8ee0
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-4ad856fa
  (fn [f]
    (fn [a b]
      (f b a))))

(defcheck solution-4afa4a40
  (fn [f](fn [a, b](f b a))))

(defcheck solution-4b3e06ba
  (fn flip [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-4bb47f01
  (fn [f]
    (fn [& args] (apply f (reverse args)))
    ))

(defcheck solution-4be99797
  (fn [f](fn [x y](f y x))))

(defcheck solution-4d6a9da3
  (fn switcher [afunc]
    #(afunc %2 %1)))

(defcheck solution-4f78cfaa
  (fn flip-out [f]
    (fn fo [x y]
      (f y x))))

(defcheck solution-4ff8917c
  (fn [f]
    (fn [& args](apply f (reverse args)))))

(defcheck solution-518932a6
  (fn flip-args [f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-51d59c3e
  (fn [func] #(func %2 %1)))

(defcheck solution-521783ae
  (fn [h] (partial (fn [f x y] (f y x)) h)))

(defcheck solution-529dabc0
  (fn [function]
    (fn [& args]
      (apply function (reverse args)))))

(defcheck solution-53601320
  (fn [fun]
    (fn [& l] (apply fun (reverse l)))))

(defcheck solution-538e8479
  (fn rev [fun]
    (fn [ & opts ]
      (fun
        (second opts)
        (first opts)))))

(defcheck solution-549c2396
  (fn [f]
    (partial
      (fn [f a b]
        (f b a)) f)))

(defcheck solution-5534f4c3
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-5742561
  (fn func
    [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-57ce6f25
  (fn [foo] #(foo %2 %1)))

(defcheck solution-5ab0f375
  (fn [f]
    #(f %2 %1)))

(defcheck solution-5ab386b5
  (fn [f]
    #(f %2 %1)

    ))

(defcheck solution-5b1adc3
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-5b9a10e8
  (fn [f] ( fn[x y] (f y x))))

(defcheck solution-5be984a1
  (fn [f]
    (fn [a b]
      (f b a)
      )
    ))

(defcheck solution-5c08bce6
  (fn [f]
    (fn [& args]
      (apply f (reverse args))
      )
    ))

(defcheck solution-5c58de30
  (fn [f] (fn [ & xs] (apply f (reverse xs)))))

(defcheck solution-5c5a3264
  (fn
    [func]
    (fn [x y] (func y x))))

(defcheck solution-5d2b66e0
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-5d30f0ac
  (fn [f]
    #(apply f (reverse %&))))

(defcheck solution-5d78875
  #(fn [x y]
     (% y x)))

(defcheck solution-61263828
  (fn [f] #(f %2 %) ))

(defcheck solution-6223fd3
  (fn [f] (partial (fn [f a b] (f b a)) f)))

(defcheck solution-634e061c
  (fn [f]
    (fn [x y]
      (apply f [y x])
      )))

(defcheck solution-671218
  #(fn [& args]
     (apply % (reverse args))))

(defcheck solution-671480fc
  (fn flip [f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-67445f34
  (fn[f] #(f %2 %1)))

(defcheck solution-6758d7ce
  (fn [g]
    #(g %2 %) ))

(defcheck solution-67dae0ec
  (fn flip [f]
    #(apply f (reverse %&))))

(defcheck solution-684fc2de
  (fn flip [f]
    (fn [a b]
      (f b a))))

(defcheck solution-6911108b
  (fn [f]#(f %2 %1)))

(defcheck solution-699a814c
  (fn [func]
    #(apply func (reverse %&))
    ))

(defcheck solution-69c38cc4
  (fn [f] (fn [& args]
            (apply f (reverse args)))))

(defcheck solution-6b332219
  (fn rev[f]
    (fn [x y]
      (f y x))))

(defcheck solution-6b3dd310
  (fn [f]
    (fn [x y & args]
      (apply f y x args))))

(defcheck solution-6c2c45c3
  (fn[f] (fn[x,y](f y x))))

(defcheck solution-6ca2b827
  (fn [f]
    #(f %2 %1)
    ))

(defcheck solution-6cd649f5
  (fn [f]
    (fn [n coll]
      (f coll n))))

(defcheck solution-6d7d96ab
  (partial partial
    (comp (partial apply apply)
          (juxt first
            (comp (juxt second first)
                  rest))
          list)))

(defcheck solution-6e47d4f0
  (fn [op]
    (fn [& x]
      (apply op (reverse x)))))

(defcheck solution-6e582df9
  (fn k[f](fn[a b](f b a))))

(defcheck solution-6e91d0bd
  (fn [f]
    (fn [b a]
      (f a b))))

(defcheck solution-70959e84
  #_(fn [f] (fn [& a] (apply f (reverse a))))

  #_#(fn [& a] (apply % (reverse a)))

  #(fn [a b] (% b a)))

(defcheck solution-70ac3f0
  (fn [function]
    (fn [x y] (function y x))))

(defcheck solution-71013d73
  (fn [f]
    (fn f' [& args]
      (apply f (reverse args)))))

(defcheck solution-72faa5fe
  #(do (fn [a b] (% b a))))

(defcheck solution-73334240
  (fn [fun] (fn [& args] (apply fun (reverse args)))))

(defcheck solution-74199a57
  (fn [f] (fn [a b] (f b a))))

(defcheck solution-758c8808
  (fn flip [f]
    (fn flipped [& args]
      (apply f (reverse args)))))

(defcheck solution-75942b70
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-763be4f5
  (fn
    [f]
    #(f %2 %1)))

(defcheck solution-76c4a693
  (fn [f]
    (fn [a1 a2]
      (f a2 a1))))

(defcheck solution-770456fb
  (fn [f]
    (fn [a b]
      (f b a))))

(defcheck solution-7769ee5e
  (fn[f]#(f %2 %)))

(defcheck solution-78332268
  (fn [f] (fn [b a] (f a b))))

(defcheck solution-7a469ab
  (fn [f]
    (fn [x y]
      (f y x))))

(defcheck solution-7a4d0a90
  (fn [a] (fn [b c] (a c b)) ))

(defcheck solution-7ab48561
  (fn[f](fn[a b](f b a))))

(defcheck solution-7b07b716
  (fn [op]
    #(op %2 %1)))

(defcheck solution-7b5b7515
  (fn myswap [f] #(f %2 %1)))

(defcheck solution-7b8b23cb
  (fn [f]
    (fn [a b] (f b a))))

(defcheck solution-7c2bf47c
  (fn [f] (fn [& ps] (apply f (reverse ps)))))

(defcheck solution-7c2ebdbc
  #(fn [& arguments] (apply % (reverse arguments))))

(defcheck solution-7c6d0b5f
  (fn flippy
    [fun]
    (fn [a b] (fun b a))))

(defcheck solution-7c95dfae
  (fn r [f]
    (fn [& x]
      (apply f (reverse x)))))

(defcheck solution-7c98dae6
  (fn [f] (#(fn [s x] (f x s)))))

(defcheck solution-7cd4854a
  (fn[f](fn[& args] (apply f (reverse args)))))

(defcheck solution-7cfa59fd
  (fn [f]
    (fn [a b](f b a))
    ))

(defcheck solution-7d2faa87
  (fn [f]
    (fn [a b]
      (f b a)
      )
    ))

(defcheck solution-7d36594c
  #(fn [x y](% y x)))

(defcheck solution-7dd21359
  (fn
    [f]
    (fn [b a] (f a b))))

(defcheck solution-7df32f48
  (fn reverse-order
    [function]
    (fn
      [& args]
      (apply function (reverse args)))))

(defcheck solution-80596715
  partial (fn [f & p] (apply f (reverse p))))

(defcheck solution-808d1de5
  (fn [f]	#(f %2 %)))

(defcheck solution-81ad8dc9
  #(fn[x y](% y x)))

(defcheck solution-8218d949
  (fn [x]
    (fn [a1,a2] (x a2 a1))))

(defcheck solution-827fe45a
  (fn [rfn]
    (fn [& args]
      (apply rfn (reverse args)))))

(defcheck solution-84f1365c
  (fn [f]
    (fn [n xs] (f xs n))))

(defcheck solution-85d90d2e
  (fn [func]
    (fn [& x]
      (apply func (reverse x)))))

(defcheck solution-86c2dd76
  (fn flip [f]
    (fn [x y] (f y x))
    ))

(defcheck solution-872a3075
  (fn flip-out [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-87f80188
  (fn[f]
    (fn [a b]
      (f b a))))

(defcheck solution-88356e33
  partial (fn [f & args] (apply f (reverse args))))

(defcheck solution-88ba093d
  (fn [f] (fn [l r] (f r l))))

(defcheck solution-89d3b709
  #(fn [a1 a2] (% a2 a1)))

(defcheck solution-8abbc19c
  partial (fn [f x y](f y x)))

(defcheck solution-8daebfb0
  (fn [f]
    (fn [x y]
      (f y x)
      )
    ))

(defcheck solution-8dd434c2
  {nth {2 3}
   > {7 true}
   quot {2 4}
   take {[1 2 3 4 5] [1 2 3]}})

(defcheck solution-8e673f7a
  (fn flip-out [f] (fn [a b] (f b a))))

(defcheck solution-8ebc290d
  (fn [f]
    (fn [x y]
      (f y x))))

(defcheck solution-9025d5b7
  (fn [x]
    #(x %2 %1)))

(defcheck solution-911c9476
  (fn rev [f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-91869484
  (fn [func]
    (fn [& args]
      (apply func (reverse args)))))

(defcheck solution-91e1e31f
  (fn [f]
    (fn [& a] (apply f (reverse a)))
    ))

(defcheck solution-92822dbf
  (fn arg-flip [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-929cc7c7
  partial (fn [op a b] (op b a)))

(defcheck solution-9379bb8
  (fn [a-fn] #(a-fn %2 %1) ))

(defcheck solution-93e8e1ca
  (fn
    [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-94623727
  #(fn[& v]
     (apply % (reverse v))))

(defcheck solution-94e4f054
  (fn[f] (fn[y x] (f x y))))

(defcheck solution-956dcac9
  (fn [x]
    (fn [a b] (x b a))))

(defcheck solution-95a65996
  (fn flipFn [f] #(f %2 %1)))

(defcheck solution-95e0ffed
  (fn [f] (fn [x y] (f y x))))

(defcheck solution-95e6276
  (fn [q] #(apply q (concat (rest %&) (list (first %&))))))

(defcheck solution-96363b63
  #(partial (fn [a b c] (a c b)) %))

(defcheck solution-969a603c
  partial (fn [func arg1 arg2] (func arg2 arg1)))

(defcheck solution-9767ad8b
  (fn [f] (fn [x y] (f y x) )))

(defcheck solution-9785bd52
  (fn [f] (fn [& as] (apply f (reverse as)))))

(defcheck solution-97f7046
  (fn [func]
    (fn [arg1 arg2]
      (func arg2 arg1))))

(defcheck solution-9808cdba
  (fn [f]
    (fn [& args] (apply f (reverse args)))
    ))

(defcheck solution-993560a9
  (fn [f] (fn [x y & z] (apply f y x z))))

(defcheck solution-9a698e5d
  #(fn[a b]( % b a)))

(defcheck solution-9ae0c0ea
  (fn problem-46 [f]
    (fn [& rest]
      (apply f (reverse rest)))))

(defcheck solution-9b6a02bb
  #(fn [& x] (apply % (reverse x))))

(defcheck solution-9c3b4952
  ;(fn [f]
  ;  (fn [x1 x2] (f x2 x1)))

  (fn [f] #(f %2 %1)))

(defcheck solution-9c43f60f
  (fn[f]
    (fn[a b]
      (f b a))))

(defcheck solution-9c72f8ac
  (fn [f]
    (partial (fn [func & args] (apply func (reverse args))) f)))

(defcheck solution-9c74c848
  #(comp (partial apply %) reverse list))

(defcheck solution-9d05dd20
  #(fn [a b] (% b a)))

(defcheck solution-9d4c3b0
  (fn flip-out [x]
    (fn flip [y z]
      (x z y))))

(defcheck solution-9d7ee7a6
  (fn flip [f]
    (fn flipped [a b] (f b a))
    ))

(defcheck solution-9d8fb35e
  (fn [ff] (fn [& xs] (apply ff (reverse xs)))))

(defcheck solution-9d999eb5
  (fn[func](letfn [(f [n v] (func v n))] f)))

(defcheck solution-9d9a894a
  (fn [f]
    (fn [& ps]
      (apply f (reverse ps)))))

(defcheck solution-a65292f
  (fn [f]
    (fn [a b]
      (f b a)
      )
    ))

(defcheck solution-a6c1c116
  (fn rf [f]
    (fn x [& params]
      (apply f (reverse params))
      )))

(defcheck solution-a70a5678
  (fn flip
    [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-a8dba55d
  (fn [f]
    (fn [x y]
      (apply f [y x]))))

(defcheck solution-a936108
  (fn [x] (fn bleh [be & re] (x (first re) be))))

(defcheck solution-a9b1a5a0
  (fn [f]
    (fn [x y] (f y x))))

(defcheck solution-a9d99256
  (fn flip-arg
    [f]
    (partial (fn [f x y] (f y x)) f)))

(defcheck solution-a9e31cb2
  (fn [f]
    (fn [a b]
      (f b a)

      )
    ))

(defcheck solution-aa015e81
  (fn [f]
    (fn [& liste] (apply f (reverse liste)))
    ))

(defcheck solution-aa2305af
  (fn [f] (fn [arg1 arg2] (apply f arg2 arg1 []))))

(defcheck solution-aaca81f8
  (fn [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-ad4f48eb
  (fn flip [xs] (fn [& ys] (apply xs (reverse ys)))))

(defcheck solution-ae08382a
  (fn flipf [f]
    (fn [& args]
      (apply f (reverse args))
      )
    ))

(defcheck solution-ae0984df
  (fn flip-args [f] ;&#27492;&#22788;&#20351;&#29992;&#20102;&#20004;&#20010;function &#22806;&#38754;&#30340;&#21442;&#25968;&#26159;&#19978;&#38754;&#30340;&#26041;&#27861;nth &#19979;&#38754;&#30340;&#21311;&#21517;&#26041;&#27861;&#30340;&#21442;&#25968;&#26159;&#19978;&#38754;&#30340;&#21442;&#25968;&#37027;&#20010;&#26041;&#27861;&#21152;&#19978;&#21518;&#38754;&#30340;&#21442;&#25968;&#65292;&#28982;&#21518;&#20351;&#29992;apply&#23545;sequences&#36827;&#34892;&#26041;&#27861;f&#30340;&#25805;&#20316;&#65292;&#20043;&#21069;&#35201;&#23545;&#21442;&#25968;&#36827;&#34892;reverse
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-ae4b0544
  (fn flip-args-fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-aee6f914
  (fn [f]
    (fn x [a b]
      (f b a))))

(defcheck solution-afb9c7cb
  (fn flp
    [f]
    (fn [args & more] (apply f (concat (reverse more) [args])))
    ))

(defcheck solution-afbf1ce2
  (fn rev [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-afdabda4
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))
    ))

(defcheck solution-b023a066
  (fn [op] (fn [a b] (op b a))))

(defcheck solution-b052863b
  (fn[f] (fn[a b] (f b a))))

(defcheck solution-b0c8428f
  (fn prob46 [f]
    (fn [x y]
      (f y x))))

(defcheck solution-b0f1d3f4
  (fn flip-args
    [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-b182ab92
  ( fn [f] (fn [x y] (f y x)) ))

(defcheck solution-b1d7ed9e
  (fn flip [f]
    (fn [& args] (apply f (reverse args)))))

(defcheck solution-b2953723
  (fn flip
    [f] {:pre [(fn? f)]}
    (fn g [& args] (apply f (reverse args)))))

(defcheck solution-b29d9ef0
  (fn [f]
    (fn [x y]
      (f y x))))

(defcheck solution-b2a62872
  (fn
    [f]
    (fn
      [ & args]
      (apply f (reverse args)))))

(defcheck solution-b2f4a4f0
  (fn [f]
    #(f %2 %1)
    ))

(defcheck solution-b333dbaf
  (fn flip-order [f]
    (fn myf [a1 a2]
      (f a2 a1))))

(defcheck solution-b3b74f97
  (fn[f] #(f %2 %)))

(defcheck solution-b5003aad
  (fn flip-args [operator] (fn [a b] (operator b a))))

(defcheck solution-b5592ba0
  (fn flip- [f]
    "46. Write a higher-order function which flips the order of the arguments of an input function."
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-b658b1b
  (fn [f]
    #(f %2 %1)
    ))

(defcheck solution-b9cf7c20
  (fn [f] (fn [y,x] (f x y))))

(defcheck solution-b9fd9edf
  (fn [f]
    (fn [ & args]
      (apply f (reverse args)))))

(defcheck solution-ba104c4
  #(comp (partial apply %) reverse vector))

(defcheck solution-bcb36085
  (fn swap [f] (fn [x y] (f y x))))

(defcheck solution-bdc57d21
  (fn flip [f]
    (fn ([x y] (f y x)))))

(defcheck solution-be2723d8
  (fn flip [f]
    #(f %2 %)))

(defcheck solution-be955554
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-bf56b245
  #(fn [& xs] (apply % (reverse xs))))

(defcheck solution-c074bea1
  (fn [origfn]
    (fn [ & args ]
      (apply origfn (reverse args)))))

(defcheck solution-c11a44c0
  (fn [f] #(f %2 %1)))

(defcheck solution-c1ff0515
  #(fn [ & args ] (apply % (reverse args))))

(defcheck solution-c26a890f
  (fn [inputfn] #(inputfn %2 %1)))

(defcheck solution-c27dcd3d
  (fn flip [f] (fn flipped [a b] (f b a))))

(defcheck solution-c3c2a1c
  (fn [f] (fn [& x] (apply f (reverse x)))))

(defcheck solution-c410e3ef
  (fn [f]
    (fn [y x]
      (f x y))))

(defcheck solution-c4bb98ca
  (fn [fnc]  #(fnc %2 %1) ))

(defcheck solution-c61f4504
  (fn [function] (fn [a b] (function b a))))

(defcheck solution-c677acfc
  partial (fn [f x y] (f y x)))

(defcheck solution-c703b8de
  (fn fliparguments [f]
    (fn g [x y]
      (f y x))))

(defcheck solution-c73c383a
  (fn [f] #(f %2 %1 ) ))

(defcheck solution-c7852604
  (fn flipout [func] (fn [& args] (apply func (reverse args)))))

(defcheck solution-c93b45f9
  (fn [f] (fn [n xs] (f xs n))))

(defcheck solution-cbd9a3ce
  (fn [f]
    (letfn [(ff
              [x y]
              (	f y x))
            ]
      ff)))

(defcheck solution-cbfced2c
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-cc22e7
  (fn __ [f]
    #(f %2 %)))

(defcheck solution-cc2b6697
  (fn[x] #(x %2 %)))

(defcheck solution-cce6f196
  (fn [f]
    (fn [x y]
      (f y x))))

(defcheck solution-cce7ad17
  #(fn [a b](% b a)))

(defcheck solution-cd0a6641
  (fn [z]
    (fn [a b] (z b a))))

(defcheck solution-cee04bac
  #(partial (fn [f & s] (apply f (reverse s))) %))

(defcheck solution-cef85549
  #(fn [& r] (apply % (reverse r))))

(defcheck solution-cfe8f3de
  (fn flp [func]
    #((partial func %2) %1)))

(defcheck solution-d019edc0
  (fn [func]
    (fn [& args]
      (apply func (reverse args)))))

(defcheck solution-d0d2f5f6
  (fn [x]
    (fn [a b]
      (x b a))))

(defcheck solution-d1c6d65e
  #(fn [x y]
     (% y x)))

(defcheck solution-d28d5e25
  (fn [f] (fn [& args] (let [newargs (reverse args)] (apply f newargs) ))))

(defcheck solution-d2ea13c7
  #(fn [& args] (apply % (rseq (vec args)))))

(defcheck solution-d30e75be
  (fn [fun] (fn [x y] (fun y x))))

(defcheck solution-d4bbc162
  (fn [x] (fn [& y] (apply x (reverse y)))))

(defcheck solution-d77a575d
  (fn [op]
    (fn [x y] (op y x))
    ))

(defcheck solution-d8895472
  (fn f [a]
    (fn [& r] (apply a (reverse r)))))

(defcheck solution-d957f442
  (fn [x] (fn [a b] (x b a))))

(defcheck solution-d9fb257c
  (fn [the-fn]
    (fn [& args]
      (#(apply the-fn (reverse args)))
      ) ))

(defcheck solution-daa35308
  (fn [f]
    (partial (fn [f a b] (f b a)) f)))

(defcheck solution-dac47e6
  (fn[f]
    (fn[& xs]
      (apply f (reverse xs)))))

(defcheck solution-dbf442cb
  (fn [f]
    (fn [x y] (f y x))))

(defcheck solution-dc267029
  (fn flip [f]
    (fn swap-args [x y]
      (f y x)
      )
    ))

(defcheck solution-dd359e0e
  #(fn[& args] (apply % (reverse args))))

(defcheck solution-dda8287a
  (fn flip [fname] (fn [x y] (fname y x))))

(defcheck solution-def4359e
  #(fn [& more] (apply % (reverse more))))

(defcheck solution-e019c4cb
  (fn revf [f]
    (fn [a b] (f b a))))

(defcheck solution-e1c1ef25
  (fn flip [f]
    (fn [x y] (f y x))))

(defcheck solution-e3cc9787
  (fn hi-order-rev [f]
    (fn [x y] (f y x))))

(defcheck solution-e46a3e11
  #(fn [x y] (% y x) ))

(defcheck solution-e46d0e7d
  #(fn f [a b] (% b a)))

(defcheck solution-e59d42dd
  (fn [op] #(op %2 %)))

(defcheck solution-e7a8b703
  (fn [func]
    #(func %2 %1)))

(defcheck solution-e8d58745
  #(fn [x y]
     (% y x)))

(defcheck solution-e8dd3077
  (fn [x] #(x %2 %1)))

(defcheck solution-e975fac7
  (fn [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-e9d0cd5d
  (fn rev [f] #(apply f (reverse %&))))

(defcheck solution-e9e85dd
  (fn flip [f]
    (fn [a b] (f b a))))

(defcheck solution-e9ec18b
  #(fn [& b] (->> b reverse (apply %)) ))

(defcheck solution-eb716517
  (fn [f]
    (fn [& ps] (apply f (reverse ps)))))

(defcheck solution-ebc90f8c
  #(fn [& args] (apply % (reverse args))))

(defcheck solution-ec77d96b
  (fn rev-args [f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-ef8f0372
  (fn [op]
    (fn [x y]
      (op y x))))

(defcheck solution-f053a0ca
  (fn [f]

    (fn [a b]
      (f b a))
    ))

(defcheck solution-f1f52c43
  (fn [f] (fn [arg1 arg2 & args] (apply f arg2 arg1 args))))

(defcheck solution-f2538a63
  (fn
    [f]
    (fn [& a] (apply f (reverse a)))))

(defcheck solution-f398f2d9
  (fn flip [f] (fn [arg1 arg2]
                 (f arg2 arg1))))

(defcheck solution-f3a535a7
  (fn this [f]
    (fn [a b]
      (f b a))))

(defcheck solution-f4df2842
  #(fn [a b] (%1 b a)))

(defcheck solution-f5433437
  (fn [f]
    (fn [x y]
      (f y x))))

(defcheck solution-f5b18dbf
  (fn [f]
    (fn [b a] (f a b))))

(defcheck solution-f80797c
  (fn [f] (comp #((partial apply f) %) (fn [x y] [y x]))))

(defcheck solution-f880e781
  partial #(%1 %3 %2))

(defcheck solution-f8aff78b
  (fn [op]
    (fn [& args]
      (apply op (reverse args))
      )
    ))

(defcheck solution-f92f02f5
  (fn flip-args [f] (fn [a b] (f b a))))

(defcheck solution-f94dd17b
  (fn [pred]
    #(pred %2 %1)))

(defcheck solution-f9c57e6c
  (fn [f]
    (fn [arg1 arg2]
      (f arg2 arg1))))

(defcheck solution-fa4c4e58
  (fn [f]
    (fn
      ([a b] (f b a) )
      )
    ))

(defcheck solution-fa78eb43
  (fn [f]
    (comp (partial apply f) reverse list)))

(defcheck solution-fad37acf
  (fn [func] (fn [a b] (func b a))))

(defcheck solution-fb0a27aa
  #(fn [& args] (apply %1 (reverse args))))

(defcheck solution-fb829a4e
  partial (fn [a b c] (a c b) ))

(defcheck solution-fbae9563
  (fn [f]
    #(f %2 %)))

(defcheck solution-fc411c11
  (fn flip [f]
    (fn [& args]
      (apply f (reverse args)))))

(defcheck solution-fc7ffc4e
  (fn [func]
    #(func %2 %1)))

(defcheck solution-fc99cc32
  (fn [f] (fn [& args] (apply f (reverse args)))))

(defcheck solution-fd316d55
  (fn rev [f] (fn [a b] (f b a))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-46))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
