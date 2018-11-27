(ns coal-mine.problem-78
  (:require [coal-mine.checks :refer [defcheck-78] :rename {defcheck-78 defcheck}]
            [clojure.test]))

(defcheck solution-101bae56
  (fn t [f & a] (first (drop-while fn? (iterate #(if (fn? %1) (%1) %1) (apply f a))))))

(defcheck solution-1129983c
  #(loop [f (apply % %&)] (if (fn? f) (recur (f)) f)))

(defcheck solution-115cfb93
  (fn tp
    ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & args] (tp #(apply f args)))))

(defcheck solution-11af78c7
  (fn [f & args]
    (loop [fun (apply f args)]
      (if-not (ifn? fun)
        fun
        (recur (fun))))))

(defcheck solution-1238a2aa
  (fn [f arg]
    (let [ff (f arg)]
      (loop [r ff]
        (if (fn? r)
          (recur (r))
          r)))))

(defcheck solution-127f72df
  (fn [f & args]
    (if
     (fn? f) (recur (apply f args) nil)
             f)
    ))

(defcheck solution-129874c8
  (fn trample [f & args]
    (loop [ff #(apply f args)]
      (let [ret (ff)]
        (if (fn? ret) (recur ret) ret)))))

(defcheck solution-12a07faa
  (fn my-trampoline [f & args]
    (if (fn? f)
      (my-trampoline (apply f args))
      f)))

(defcheck solution-1310171c
  (fn t [f & args]
    (let [r (apply f args)]

      (if (fn? r)
        (t r)
        r))))

(defcheck solution-131be6cd
  #(loop [v (%1 %2)] (if (fn? v) (recur (v)) v)))

(defcheck solution-137d5777
  #(loop [r (%1 %2)]
     (if (fn? r)
       (recur (r))
       r)))

(defcheck solution-1386541f
  (fn [f & params]
    (loop
     [out (apply f params)]
      (if
       (not (fn? out))
        out
        (recur (out))))))

(defcheck solution-13e6af91
  (fn tramp [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (recur result [])
        result
        )
      )
    ))

(defcheck solution-13e98fae
  (fn [f0 & args] ((fn [f]
                     (if (fn? f)
                       (recur (f))
                       f))
                   (apply f0 args))))

(defcheck solution-140f815b
  (fn tramp
    [f & args]
    (let [res (apply f args)]
      (if (fn? res)
        (recur res nil)
        res))))

(defcheck solution-14ade0e3
  (fn [func & args]
    (first (drop-while #(fn? %)
             (iterate #(%)
               (apply func args))))))

(defcheck solution-14b0566e
  (fn [f & a]
    (loop [f (apply f a)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-14bb59ee
  (fn my-trampoline
    ([f & args] (my-trampoline (apply f args)))
    ([f] (if (fn? f)
           (recur (f))
           f))))

(defcheck solution-1522db8d
  (fn trampoline-clone
    ([f] (if (fn? f) (recur (f)) f))
    ([f & args] (trampoline-clone (apply f args)))))

(defcheck solution-15401527
  (fn t [f & p]
    (let [r (apply f p)]
      (if (fn? r)
        (t r)
        r))))

(defcheck solution-154c9150
  (fn koo [f x]
    (letfn [(it [f]
              (if (fn? f)
                (recur (f))
                f))]
      (it (f x)))))

(defcheck solution-1591b497
  (fn __ [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (__ result)
        result))))

(defcheck solution-15aa7077
  (fn t [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (t r)
        r))))

(defcheck solution-162f6e4f
  (fn xx[f & args]
    (if-not (fn? f) f
                    (xx (apply f args)))))

(defcheck solution-17066f19
  (fn[fnc arg]
    (letfn [(my-trampoline
              ([fnc arg & args]
               (let [val (apply fnc (cons arg args))]
                 (if (ifn? val)
                   (my-trampoline val)
                   val)))
              ([fnc]
               (let [val (fnc)]
                 (if (ifn? val)
                   (recur val)
                   val))))]
      (my-trampoline fnc arg))))

(defcheck solution-1747f9b9
  (fn solve [f & args]
    (if (fn? f)
      (solve (apply f args))
      f)))

(defcheck solution-175ac5f
  (fn [f & a]
    (loop [r #(apply f a)]
      (if (fn? (r)) (recur (r)) (r)))))

(defcheck solution-17657251
  (fn t [f & p]
    (loop [k (apply f p)]
      (if (fn? k)
        (recur (k))
        k))))

(defcheck solution-17b36a86
  (fn [f & x] (loop [r (apply f x)] (if (fn? r) (recur (r)) r))))

(defcheck solution-17ba2cbd
  (fn trampoline'
    ([f & args]
     (trampoline'
       (apply f args)))
    ([f] (if (fn? f)
           (recur (f))
           f))))

(defcheck solution-17cc306d
  #(loop [x (apply % %&)]
     (if (ifn? x) (recur (x)) x)))

(defcheck solution-18573295
  (fn tramp
    ([f x] (tramp (f x)))
    ([x]
     (if (fn? x)
       (tramp (x))
       x))))

(defcheck solution-189b9e58
  (fn [f  arg] (if (fn? ( f arg)) (loop [g (f arg)] (if (fn? (g))(recur (g))(g))) (f arg))))

(defcheck solution-18df5c5f
  (fn [f v]
    (loop [r (f v)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-192bcd51
  #(loop [res (apply % %&)]
     (if (fn? res) (recur (res)) res)))

(defcheck solution-19aa9e80
  (fn t [f & args]
    (let [v (apply f args)]
      (if (fn? v) (t v) v))))

(defcheck solution-19bfc27a
  #(loop [res (apply % %&)]
     (if (fn? res)
       (recur (res))
       res)))

(defcheck solution-19cb2583
  (fn [g & vs]
    (let [r (apply g vs)]
      (if (fn? r)
        (loop [g' r]
          (let [h (g')]
            (if (fn? h)
              (recur h)
              h)))
        r))))

(defcheck solution-19e56189
  (fn tramp [f & args]
    (if (fn? f)
      (recur (apply f args) '())
      f)))

(defcheck solution-1a75e2d7
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-1aa4061e
  (fn [f & xs]
    (let [x (apply f xs)]
      (if (fn? x)
        (recur x ())
        x))))

(defcheck solution-1acaae75
  (fn tramp [f & args]
    (loop [rc (apply f args)]
      (if-not (fn? rc)
        rc
        (recur (rc))))))

(defcheck solution-1b1c0c06
  #(if (ifn? %) (recur (apply % %&) nil) %))

(defcheck solution-1b2bcb52
  #(loop [x (apply % %&)]
     (if (fn? x) (recur (x)) x)))

(defcheck solution-1b87f8fa
  (fn t
    ([f a] (t #(f a)))
    ([f]
     (let [r (f)]
       (if (fn? r ) (t r) r)))))

(defcheck solution-1b88afb0
  (fn tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret
         )
       )
     )
    ([f & args] (tramp #(apply f args)))
    ))

(defcheck solution-1b8d5045
  (fn t[f & v]
    (if (fn? f)
      (t (apply f v)) f)))

(defcheck solution-1c31b801
  #((fn [x] (if-not (fn? x) x (recur (x)))) (%1 %2)))

(defcheck solution-1c31e5c7
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v)	(recur (v)) v))))

(defcheck solution-1c5023b4
  (fn my-trampoline [fun & r]
    (let [resp (apply fun r)]
      (loop [resp resp]
        (if-not (fn? resp)
          resp
          (let [r (resp)] (recur r)))))))

(defcheck solution-1c7b75cf
  (fn [f v]
    (loop [r (f v)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-1c9326c
  (fn trampoline*
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (trampoline* #(apply f args)))))

(defcheck solution-1cf6b756
  (fn my-trampoline [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-1d154c9e
  (fn [g & l] (letfn [(tramp [f args] (if (fn? f) (if (empty? args) (tramp (f) []) (tramp (apply f args) [])) f))]
                (tramp g l))))

(defcheck solution-1d336465
  (fn boing
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r)))
    ([f & a]
     (boing #(apply f a)))))

(defcheck solution-1d4562
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v) (recur (v))
                  v))))

(defcheck solution-1d69a97
  (fn myt ([f] (let [ret (f)]
                 (if (fn? ret)
                   (recur ret)
                   ret
                   )))
    ([f & args] (myt #(apply f args)))
    ))

(defcheck solution-1dae625f
  (fn T [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (apply T r [])
        r))))

(defcheck solution-1dda85b
  (fn tram [f & args] (let [a (apply f args)](if (fn? a) (tram a) a))))

(defcheck solution-1e14dcd0
  (fn t
    ([f] (let [r (f)]
           (if (fn? r)
             (recur r)
             r)))
    ([f & a] (t #(apply f a)))))

(defcheck solution-1e34898b
  (fn [f & a]
    (loop [e (apply f a)]
      (if
       (ifn? e)
        (recur (e))
        e))))

(defcheck solution-1e734d44
  (fn tram
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tram #(apply f args)))))

(defcheck solution-1f0ed257
  (fn t
    ([f]
     (let [r (f)]
       (if (fn? r) (recur r) r)))
    ([f & more]
     (t #(apply f more)))))

(defcheck solution-1f4c4d80
  (fn tr [f & args]
    (if (fn? f)
      (tr (apply f args))
      f)))

(defcheck solution-1f75240
  (fn tramp [f & args]
    (loop [ret (apply f args)]

      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-1faef878
  (fn my-trampoline
    [f & args]
    (first
      (drop-while
        fn?
        (iterate #(if (fn? %) (%) %) (apply f args))))))

(defcheck solution-201e3b5b
  #(loop [res (%1 %2)]
     (if (fn? res)
       (recur (res))
       res
       )))

(defcheck solution-203971dc
  (fn tra
    ([f]
     (let [r (f)]
       (if (fn? r)
         (tra r)
         r)))
    ([f & args] (tra #(apply f args)))))

(defcheck solution-20c78a76
  (fn [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret) (recur (ret)) ret))))

(defcheck solution-20e744ce
  (fn trampoline*
    [f  args]
    (let [step (fn [f]
                 (if (fn? f) (recur (f)) f))]
      (step (f args)))))

(defcheck solution-20eedb26
  (fn __ [f & args]
    (let [ret (apply f args)]
      (if (fn? ret)
        (__ ret)
        ret))))

(defcheck solution-2107f7ad
  #(let [r (apply % %&)]
     (if-not (fn? r)
       r
       (recur r []))))

(defcheck solution-211b2209
  (fn my-trampoline [f & args]
    (let [ret (apply f args)]
      (if (fn? ret)
        (my-trampoline ret)
        ret))))

(defcheck solution-214d73ec
  (fn [f & xs]
    (if (fn? f)
      (recur (apply f xs) [])
      f)))

(defcheck solution-2155936f
  (fn
    [f & args]
    (let [ret1 (apply f args)]
      (loop [ret ret1]
        (if (fn? ret)
          (recur (ret))
          ret)))))

(defcheck solution-21834dcb
  (fn [fnn i]
    (loop [res (fnn i)]
      (if (not (fn? res)) res
                          (recur (res))))))

(defcheck solution-2238555b
  (fn [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-22c1e12c
  (fn [f a]
    (loop [res (f a)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-23368d83
  (fn my-trampoline
    [f & params]
    (loop [r (apply f params)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-2479a298
  (fn t [f & a] (let [r (apply f a)] (if (fn? r) (t r) r))))

(defcheck solution-24d2d09f
  (fn trampoline' [f & args]
    (loop [r (apply f args)]
      (if (ifn? r)
        (recur (r))
        r))))

(defcheck solution-25e341d4
  #(if (fn? %) (recur (apply % %&) ()) %))

(defcheck solution-264aaacc
  (fn tramp
    ([x] (if (fn? x) (tramp (x)) x))
    ([x & args] (tramp #(apply x args)))))

(defcheck solution-2692220f
  (fn
    [func & args]
    (let [result (apply func args)]
      (if (fn? result)
        (recur result '())
        result
        ))))

(defcheck solution-26ae1796
  (fn self [f arg]
    (loop [nf (f arg)]
      (if (not (fn? nf))
        nf
        (recur (nf))))))

(defcheck solution-26d093b8
  (fn t [f & args]
    (cond
      args (t (apply f args))
      (fn? f) (t (f))
      :else f)))

(defcheck solution-2707f7ab
  (fn tramp
    [f & args]
    (loop [func (apply f args)]
      (if (fn? func) (recur (func)) func))))

(defcheck solution-27189071
  #(if (fn? %)
     (recur (apply % %&) [])
     %))

(defcheck solution-27429376
  (fn [f x] (loop [y (f x)] (if (fn? y) (recur (y)) y))))

(defcheck solution-27cc5085
  (fn [f x]
    (loop [x (f x)]
      (if (fn? x) (recur (x)) x))))

(defcheck solution-281fbe70
  (fn tramp
    ([f & args] (tramp #(apply f args)))
    ([f] (let [r (f)] (if (fn? r) (tramp r) r)))))

(defcheck solution-284fdb9d
  (fn t
    [f & args]
    (let [res (apply f args)]
      (if (fn? res)
        (t res)
        res))))

(defcheck solution-28a1bfc9
  (fn [f & a]
    (#(if (fn? %)
        (recur (%))
        %)(apply f a))))

(defcheck solution-29a5e3ef
  (fn t [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r))
        r)
      )))

(defcheck solution-2a22867e
  (fn [f & xs]
    (loop [k (fn [] (apply f xs))]
      (let [k-next (k)]
        (if (fn? k-next)
          (recur k-next)
          k-next)))))

(defcheck solution-2a43e043
  (fn t
    ([f] (let [r (f)]
           (if (fn? r)
             (recur r)
             r)))
    ([f & a] (t #(apply f a)))))

(defcheck solution-2acf3257
  (fn [fcn & args]
    (loop [f (apply fcn args)]
      (if (fn? f) (recur (f)) f))))

(defcheck solution-2ae0cd24
  #(loop [f (%1 %2)]
     (if (fn? f) (recur (f)) f)))

(defcheck solution-2b01744f
  (fn [f & xs]
    (loop [ret (apply f xs)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-2b6c31eb
  (fn tramp [f & args]
    (loop [f #(apply f args)]
      (if (fn? f) (recur (f))
                  f))))

(defcheck solution-2b9c1a83
  (fn tramp [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r) )
        r))))

(defcheck solution-2ca7748c
  (fn [ f & args]
    (let [r (apply f args)]
      (if (ifn? r)
        (recur r ())
        r))))

(defcheck solution-2cde8d5c
  (fn [f & l]
    (letfn
     [(my_trampoline
        ([f & rest]
         (my_trampoline #(apply f rest)))
        ([f]
         (let [r (f)]
           (if (fn? r)
             (recur r)
             r))))]
      (apply my_trampoline f l))))

(defcheck solution-2d409adb
  (fn [f & args]
    (loop [f? (apply f args)]
      (if (fn? f?) (recur (f?)) f?))))

(defcheck solution-2dfe41b7
  (fn [f & args]
    (loop [r (apply f args)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-2e6188ea
  (fn tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-2eaf6e76
  (fn trampolin [fun & args]
    (let [f (apply fun args)]
      (if (fn? f) (trampolin f) f))))

(defcheck solution-2eeda1f2
  (fn [f & args]
    (loop [f (apply f args)]
      (if (not (fn? f))
        f
        (recur (f))))))

(defcheck solution-2ff754b0
  (fn[f & args] (loop[r (apply f args)] (if (fn? r) (recur (r)) r ))))

(defcheck solution-302807bc
  (fn mytramp [f & args]
    (if (ifn? f)
      (mytramp (apply f args))
      f)
    ))

(defcheck solution-302bee7a
  (fn tramplin
    [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (tramplin r) r))))

(defcheck solution-30789f96
  (fn [& args]
    (let [result (apply (first args) (rest args))]
      (if (fn? result) (recur [result]) result))))

(defcheck solution-30d78491
  (fn [f a] (->> (iterate #(%) (partial f a))
              (drop-while fn?)
              first)))

(defcheck solution-30ec43b9
  (fn my-trampoline [f & args]
    (loop [res (apply f args)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-312021a8
  #(if (fn? %) (recur (apply % %&) []) %))

(defcheck solution-319e7eee
  (fn [f & xs]
    (let [fst (apply f xs)
          rec #(if (fn? %) (recur (%)) %)]
      (rec fst))))

(defcheck solution-31ad983b
  (fn tramp
    ([f] (let [r (f)]
           (if (fn? r)
             (recur r)
             r)))
    ([f & a] (tramp #(apply f a)))))

(defcheck solution-31c2e39a
  (fn trampo [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret) (recur (ret)) ret))))

(defcheck solution-321be768
  (fn [f & a]
    (loop [t (apply f a)]
      (if (fn? t) (recur (t))
                  t))))

(defcheck solution-32775902
  (fn [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (recur result ())
        result))))

(defcheck solution-327977b5
  (fn tr
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r)))
    ([f & args]
     (tr #(apply f args)))))

(defcheck solution-335547cc
  #(loop [o (% %2)] (if (fn? o)(recur (o)) o)))

(defcheck solution-33cfdda1
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-3409adf9
  (fn trampoline' [function & arguments]
    (let [result (apply function arguments)]
      (if (fn? result)
        (trampoline' result)
        result))))

(defcheck solution-3412a602
  (fn my-tramp
    [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-34347e13
  (fn [f & args]
    ( #(if (fn? %) (recur (%)) %)  (apply f args))))

(defcheck solution-345f41a4
  (fn tramp [fun & args]
    (let [result (apply fun args)]
      (if (fn? result)
        (tramp result)
        result))))

(defcheck solution-347c1d89
  (fn [f & args]
    (loop [f f
           args args]
      (let [result (apply f args)]
        (if (fn? result)
          (recur result [])
          result)))))

(defcheck solution-349c1c4c
  (fn tramp [f & args]
    (let [rec (apply f args)]
      (loop [f rec]
        (if (fn? f) (recur (f)) f)))))

(defcheck solution-34a30728
  (fn tramp [x & args]
    (loop [y (apply x args)]
      (if (fn? y) (recur (y))
                  y))))

(defcheck solution-34ea3b23
  (fn trampoline2 [f & args]
    (cond
      (and args (fn? f)) (trampoline2 (apply f args))
      (fn? f) (trampoline2 (f))
      :else f)
    ))

(defcheck solution-34ee10a0
  (fn [x y]
    (loop [func (x y)]
      (if (fn? func)
        (recur (func))
        func)
      )))

(defcheck solution-34f5a318
  (fn t [f & args]
    (if (fn? f)
      (t (apply f args))
      f)))

(defcheck solution-355c625d
  (fn [f & more]
    (loop [f (apply f more)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-35a162d2
  (fn t [fun & args] (let [x (apply fun args)] (if (fn? x) (t x) x))))

(defcheck solution-35d82e16
  (fn my-trampoline [f & args]
    (let [val (apply f args)]
      (if (ifn? val)
        (recur val nil)
        val))))

(defcheck solution-36239f23
  (fn [func & args]
    (loop [f (apply func args)]
      (if (fn? f) (recur (f)) f)
      )
    ))

(defcheck solution-371370cd
  (fn tramp [f & args]
    (loop [rez (apply f args)]
      (if (fn? rez)
        (recur (rez))
        rez))))

(defcheck solution-37ba2524
  (fn [f arg]
    (loop [g (f arg)]
      (if (fn? g)
        (recur (g))
        g))))

(defcheck solution-38963362
  (fn [f & args] ((fn [f] (if (fn? f) (recur (f)) f)) (apply f args))))

(defcheck solution-38c0355a
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-38c26500
  (fn t
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (t #(apply f args)))))

(defcheck solution-39098c79
  (fn [ func & args ] (loop [res (apply func args)]
                        (if (fn? res)
                          (recur (res))
                          res)
                        )))

(defcheck solution-395497c8
  (fn [f & args]
    (loop [a (apply f args)]
      (if (not (fn? a)) a

                        (recur (a))))))

(defcheck solution-39805290
  (fn my-trampoline [f & params]
    (let [f #(apply f params)]
      (loop [res (f)]
        (if (not (fn? res))
          res
          (recur (res)))))))

(defcheck solution-39806427
  #(loop [r (apply % %&)]
     (if (fn? r) (recur (r)) r)))

(defcheck solution-39fc5823
  (fn [f & a]
    (let [r (apply f a)]
      (if (fn? r)
        (recur r ())
        r))))

(defcheck solution-3a091cd
  (fn t [f & args]
    (let [x (apply f args)]
      (if (fn? x)
        (t x)
        x))))

(defcheck solution-3a795fce
  (fn [& args]
    (loop [ret (apply (first args) (rest args))]
      (if (fn? ret) (recur (ret)) ret))))

(defcheck solution-3a9a9eb8
  (fn trampoline_
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (trampoline_ #(apply f args)))))

(defcheck solution-3b0350d7
  (fn tr [f & a]
    (if (fn? f)
      (recur (apply f a) nil)
      f
      )
    ))

(defcheck solution-3b28a2e3
  (fn [f & args]
    (first (drop-while fn?
             (iterate #(if (fn? %) (%) %)
               (apply f args))))))

(defcheck solution-3b77baa4
  (fn tr [f & args]
    (loop [state (apply f args)]
      (if (fn? state)
        (recur (state))
        state))))

(defcheck solution-3b7d7a13
  (fn [f & args]
    ((fn [f] (if (fn? f) (recur (f)) f)) (apply f args))))

(defcheck solution-3c3b68e
  (fn tramp [f & args]
    (loop [f (apply f args)]
      (if (ifn? f)
        (recur (f))
        f))))

(defcheck solution-3c5121f3
  (fn t [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (t r)
        r))))

(defcheck solution-3cbddeed
  (fn my-trampoline
    ([x]
     (if (fn? x)
       (recur (x))
       x))
    ([f & args]
     (my-trampoline (apply f args)))))

(defcheck solution-3d006f2e
  (fn tramp [f & vs]

    (letfn [(t [f]
              (if (fn? f)
                (t (f))
                f))]
      (t (apply f vs))
      )
    ))

(defcheck solution-3dc92dd9
  (fn k
    ([f a]
     (let [r (f a)]
       (if (fn? r)
         (k r)
         r)))
    ([f]
     (let [r (f)]
       (if (fn? r)
         (k r)
         r)))))

(defcheck solution-3e3f74a7
  (fn [f & args]
    (let [g (apply f args)]
      (if-not (fn? g)
        g
        (recur g nil)))))

(defcheck solution-3e71dd3b
  #(loop [r (apply %1 %&)]
     (if (fn? r)
       (recur (r))
       r)))

(defcheck solution-3f1355e6
  (fn my-trampoline
    ([f]
     (let [res (f)]
       (if (fn? res)
         (recur res)
         res)))
    ([f & args]
     (my-trampoline #(apply f args)))))

(defcheck solution-3f3b25b9
  (fn [f & args]
    (loop [res (apply f args)]
      (if
       (fn? res) (recur (res))
                 res))))

(defcheck solution-3feaf81e
  (fn tramp [f & args]
    (let [res (if (empty? args) (f) (apply f args))]
      (if (fn? res) (tramp res) res))))

(defcheck solution-3ffb0799
  (fn my-trampoline [f & args]
    (if (not (fn? f))
      f
      (if (empty? args)
        (my-trampoline (f))
        (my-trampoline (apply f args))))))

(defcheck solution-40807fcc
  (fn cust-tramp [f & args]
    (let [result (apply f args)]
      (loop [f result]
        (if (fn? f)
          (recur (f))
          f)))))

(defcheck solution-409be478
  (fn tram
    ([f] (let [r (f)]
           (if (ifn? r) (tram r) r)))
    ([f & args] (tram #(apply f args)))))

(defcheck solution-40db73a1
  (fn tramp
    ([f & args]
     (tramp (apply f args)))
    ([f]
     (if (fn? f)
       (recur (f))
       f))))

(defcheck solution-412f8f55
  (fn mytramp [f & args]
    (loop [f (apply f args)]
      (if-not (fn? f)
        f
        (recur (f))))))

(defcheck solution-418d82af
  (fn trampX [f & n]
    (let [res (apply f n)]
      (if (not (fn? res))
        res
        ((fn trampRec [f]
           (let [res (f)]
             (if (not (fn? res))
               res
               (trampRec res)
               )
             )
           ) res)
        )
      )
    ))

(defcheck solution-418ec7b4
  (fn [f & r]
    (#(if (fn? %) (recur (%)) %) (apply f r))))

(defcheck solution-41cd412d
  (fn tramp
    ([f]
     (let [res (f)]
       (if (fn? res)
         (recur res)
         res)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-41d937a8
  (letfn [(mytramp [f & args]
            (if (fn? f)
              (mytramp (apply f args))
              f))]
    mytramp))

(defcheck solution-4230f9b4
  (fn [f v] (first (drop-while fn? (iterate #(%) (f v))))))

(defcheck solution-42dfc3be
  (fn [f & args]
    (let [ret (apply f args)]
      ((fn f [x]
         (if (fn? x) (f (x)) x)
         ) ret)
      )

    ))

(defcheck solution-42e73c7
  (fn [f args] (loop [ff (f args)] (if (fn? ff) (recur (ff)) ff))))

(defcheck solution-4322b5ce
  (fn my-trampoline
    [f & args]
    (loop [t (apply f args)]
      (if (fn? t)
        (recur (t))
        t))))

(defcheck solution-436629dc
  (fn trampoline'
    [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (recur r ())
        r))))

(defcheck solution-438d624a
  (fn [f & ar]
    (loop [i (apply f ar)]
      (if (fn? i)
        (recur (i))
        i))))

(defcheck solution-43af989e
  (fn t ([f]
         (let [r (f)]
           (if (fn? r)
             (recur r)
             r)))
    ([f & a]
     (t #(apply f a)))))

(defcheck solution-43f36a67
  (fn tro [x y]
    ((fn tro2 [x1]
       (if (fn? x1)
         (tro2 (x1))
         x1
         )
       ) (x y))
    ))

(defcheck solution-43fb911c
  (fn [f & args]
    (loop [f f args args]
      (let [res (apply f args)]
        (if (fn? res) (recur res nil) res)))))

(defcheck solution-440dd650
  (fn [ff v] (loop [f (ff v)] (if (fn? f) (recur (f)) f))))

(defcheck solution-441a0ad4
  (fn [f & args]
    (let [i (apply f args)]
      (loop [v i]
        (if (not (fn? v))
          v
          (recur (v)))))))

(defcheck solution-4445eea3
  (fn [f & args]
    (first (drop-while fn?
             (iterate #(%) (apply f args))))))

(defcheck solution-447288fd
  (fn my-trampoline
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (my-trampoline #(apply f args)))))

(defcheck solution-44952e4b
  (fn __
    ([f] (let [r (f)] (if (fn? r) (__ r) r)))
    ([f & args] (__ #(apply f args)))))

(defcheck solution-44d2278f
  (fn [f & args]
    (let [res (apply f args)]
      (loop [nf res]
        (if (fn? nf)
          (recur (nf))
          nf)))))

(defcheck solution-44ffdf56
  (fn [f & args]
    (loop [ret (apply f args)]
      (if-not (fn? ret)
        ret
        (recur (ret))))))

(defcheck solution-450f0363
  (fn trp [f & args]
    (let [x (apply f args)]
      (if (fn? x)
        (trp x)
        x
        )
      )
    ))

(defcheck solution-459d84f5
  (fn reimplement-trampoline [f & args]
    (letfn [(tr [f]
              (loop [f f]
                (if (fn? f)
                  (recur (f))
                  f)))]

      (tr #(apply f args)))))

(defcheck solution-46f811f2
  (fn my-trampoline
    [f & args]
    (loop [v (apply f args)]
      (if-not (fn? v)
        v
        (recur (v))))))

(defcheck solution-46fca0d8
  (fn [ f & a]
    (let [ res (apply f a) ]
      (letfn [ (t [f] (if (fn? f) (recur (f)) f)) ]
        (t res)))))

(defcheck solution-470692dc
  (fn [f & arg]
    (loop [x (apply f arg)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-4710fbca
  (fn own-trampoline [f & args]
    (loop [v (if (empty? args) (f) (apply f args))]
      (if-not (fn? v)
        v
        (recur (v))
        ))))

(defcheck solution-4769689c
  (fn tp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args] (tp #(apply f args)))))

(defcheck solution-47bba0b7
  (fn my-trampoline [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-485aeb07
  (fn new-tramp
    [f & args]
    (loop [ff (apply f args)]
      (if (fn? ff)
        (recur (ff))
        ff))))

(defcheck solution-4890865e
  (fn trump
    ([f]
     (if (fn? (f))
       (recur (f))
       (f)))
    ([f & args]
     (trump #(apply f args)))))

(defcheck solution-48e3e1b3
  (fn x
    ([f]
     (if (ifn? f) (recur (f)) f))
    ([f & a]
     (x (apply f a)))))

(defcheck solution-48e473f7
  (fn mytrampo [f & args]
    (loop [ret (apply f args)]
      (cond
        (fn? ret) (recur (ret))
        :else ret))))

(defcheck solution-4921244f
  (fn [f & args]
    ((fn [f]
       (if (fn? f)
         (recur (f))
         f))
     (apply f args))))

(defcheck solution-49677bba
  (fn [f n]
    (loop [t (f n)]
      (if (ifn? t) (recur (t)) t))))

(defcheck solution-4986bb32
  (fn [f & as]
    (loop [r (apply f as)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-4a847365
  (fn [f & a]
    (loop [t (apply f a)]
      (if (fn? t)
        (recur (t))
        t))))

(defcheck solution-4ab46dcc
  (fn [f & a]
    (loop [f (apply f a)]
      (if (fn? f) (recur (f))
                  f))))

(defcheck solution-4afce8a7
  (fn tramp [f v]
    (loop [start (f v)]
      (if (fn? start)
        (recur (start))
        start))))

(defcheck solution-4b0df603
  (fn [f & args]
    (let [g (fn g [r] (if (fn? r) (g (r)) r))]
      (g (apply f args)))))

(defcheck solution-4b455810
  (fn t-clone[f & args]
    (loop [fun (apply f args)]
      (if (not (fn? fun)) fun
                          (recur (fun))))))

(defcheck solution-4b64c5d0
  (fn [f & args]
    (let [res (if (empty? args) (f) (apply f args))]
      (loop [result res]
        (if-not (fn? result)
          result
          (recur (result)))))))

(defcheck solution-4c6aa3d3
  (fn [f & args]
    (#(if (fn? %) (recur (%)) %)
     (apply f args))))

(defcheck solution-4d508997
  (fn [func & args]
    (let [new-val (apply func args)]
      (if (fn? new-val)
        (recur new-val nil)
        new-val)
      )
    ))

(defcheck solution-4d6db686
  (fn [f & args]
    (loop [f1 (apply f args)]
      (if (fn? f1) (recur (f1)) f1))))

(defcheck solution-4d7d25b8
  (fn tramp ([f] (if (ifn? f) (tramp (f)) f))
    ([f & x] (if (= 0 (first x)) true (tramp ((apply f x)))))))

(defcheck solution-4de6b4c4
  (fn trampoline-new [f & args]
    (loop [value (apply f args)]
      (if (fn? value)
        (recur (value))
        value))))

(defcheck solution-4e4d24c
  (fn [f & args]
    (loop [g #(apply f args)]
      (let [res (g)]
        (if (fn? res)
          (recur res)
          res)))))

(defcheck solution-4e83bd7b
  (fn [ f & args ]
    (if
     (ifn? f)
      (recur (apply f args) ())
      f
      )))

(defcheck solution-4ef5dbd3
  (fn tram
    ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & xs] (tram #(apply f xs)) )))

(defcheck solution-4f071688
  (fn [f & args]
    (let [ff (fn [f args]
               (let [fst (apply f args)]
                 (if (not (fn? fst))
                   fst
                   (recur fst []))))]
      (ff f args))))

(defcheck solution-4f8f8b37
  (fn p
    ([f]
     (if (fn? (f))
       (p (f))
       (f)))
    ([f & a]
     (p #(apply f a)))))

(defcheck solution-4f9a924a
  (fn [fn-or-val & xs]
    (if (fn? fn-or-val)
      (recur (apply fn-or-val xs) ())
      fn-or-val)))

(defcheck solution-4fa984db
  #(loop [f (apply % %&)]
     (if (fn? f) (recur (f)) f)))

(defcheck solution-5033acc
  (fn [f & args]
    (loop [f #(apply f args)]
      (if (fn? f) (recur (f)) f))))

(defcheck solution-5081e64d
  (fn f [af & args]
    (let [r (apply af args)]
      (loop [ff r]
        (if (fn? ff)
          (recur (ff))
          ff
          )
        )

      )

    ))

(defcheck solution-5099d49e
  (fn [f & args]
    (loop [val (apply f args)]
      (if (fn? val)
        (recur (val))
        val))))

(defcheck solution-50ae10
  (fn [f & args]
    (loop [f' f
           a args]
      (let [r (apply f' a)]
        (if (fn? r)
          (recur r [])
          r)))))

(defcheck solution-50c98087
  ; cheated
  (fn tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-50e1c62
  (fn tramp* [f & args]
    (loop [val (apply f args)]
      (if (fn? val)
        (recur (val)) val))))

(defcheck solution-518d30c7
  (fn tramp [f & args]
    (loop [res (apply f args)]
      (if (ifn? res)
        (recur (res))
        res))))

(defcheck solution-52099bfe
  (fn [f x]
    (let [g (f x)]
      (loop [g g]
        (if (not (fn? g))
          g
          (recur (g)))))))

(defcheck solution-52e2e5fb
  (fn __ [f & args]
    (if (fn? (apply f args))
      (__ (apply f args))
      (apply f args))))

(defcheck solution-52e7ccd7
  (fn mytr [func val]
    (loop [ret (func val)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-530d687a
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f) (recur (f)) f))))

(defcheck solution-5346c919
  (fn tramp
    ([f] (let [v (f)] (if (fn? v) (recur v) v)))
    ([f & args] (tramp #(apply f args)))
    ))

(defcheck solution-538ee88f
  (fn g
    [f & a] (if (fn? f) (g (apply f a)) f)))

(defcheck solution-53b3822c
  (fn [f & r]
    (loop [x (apply f r)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-5409cfbb
  (fn [f x]
    (let [g (f x)]
      (loop [h g]
        (if (fn? h)
          (recur (h))
          h)))))

(defcheck solution-54f9e7a1
  (fn [f & args]
    (loop [loopf (apply f args)]
      (if (fn? loopf)
        (recur (loopf))
        loopf))))

(defcheck solution-5561c445
  (fn [f & a]
    (let [z (apply f a)]
      (loop [x z]
        (if (fn? x) (recur (x))x)))))

(defcheck solution-55fd3a91
  (fn tramp [f & args]
    (let [val (apply f args)]
      (if (ifn? val)
        (tramp val)
        val))))

(defcheck solution-560f438b
  #(loop [r (apply % %&)]
     (if (fn? r) (recur (r)) r)))

(defcheck solution-56ce2ab0
  (fn [f & args]
    (loop [nv (apply f args)]
      (if (ifn? nv)
        (recur (nv))
        nv))))

(defcheck solution-56e38c11
  #(loop [x (apply %1 %&)]
     (if (fn? x)
       (recur (x))
       x)))

(defcheck solution-57973de0
  (fn [f & x]
    (let [r (apply f x)]
      (if (ifn? r)
        (recur r '())
        r))))

(defcheck solution-57baf9c0
  #(loop [res (apply % %&)]
     (if (fn? res)
       (recur (res)) res)))

(defcheck solution-57ec3b37
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v)
        (recur (v))
        v))))

(defcheck solution-57ecf57a
  (fn t
    ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & a] (t #(apply f a)))))

(defcheck solution-58093785
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-5840452a
  (fn t [f & a]
    (let [r (if (nil? a) (f) (apply f a))]
      (cond
        (fn? r) (t r)
        :else r))))

(defcheck solution-58479337
  (fn t
    ([f & v] (t (apply f v)))
    ([f] (if (fn? f) (t (f)) f))))

(defcheck solution-5867586a
  (fn [f & args]
    (letfn [(initfn [] (apply f args)) ]
      (loop [f initfn]
        (if ((comp not fn?) f)
          f
          (recur (f))
          )
        ))))

(defcheck solution-58795588
  (fn my_trampoline
    ([f]
     (if (fn? f)
       (my_trampoline (f))
       f))
    ([f x]
     (my_trampoline (f x)))))

(defcheck solution-5883ecdb
  (fn [f & args] (loop [g (apply f args)] (if (not (fn? g)) g (recur (g))))))

(defcheck solution-589f30b3
  (fn tram [f x]
    ((fn [f]
       (if (fn? f)
         (recur (f))
         f
         )
       ) #(f x))
    ))

(defcheck solution-59298a0c
  (fn tramp [f & xs]
    (let [result (apply f xs)]
      (if (fn? result) (tramp result) result))))

(defcheck solution-59ae4580
  (fn [f x]
    (loop [res (f x)]
      (if (ifn? res)
        (recur (res))
        res))))

(defcheck solution-5a09c367
  (fn self [f & args]
    (let [val-maybe (apply f args)]
      (if (fn? val-maybe)
        (self val-maybe)
        val-maybe))))

(defcheck solution-5a5dd82d
  (fn __ [f & n]
    (let [x (apply f n)]
      (if (fn? x)
        (apply __ x [])
        x))))

(defcheck solution-5b77de90
  (fn [f & s] ((fn [x] (if (fn? x) (recur (x)) x)) (apply f s))))

(defcheck solution-5c1db3a0
  (fn ! [f & a] (let [r (apply f a)] (if (fn? r) (! r) r))))

(defcheck solution-5c36ff66
  (fn tramp [f & args]
    (loop [res (apply f args)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-5c43066c
  (fn tramp [f & args]
    (loop [inf (apply f args)]
      (if (fn? inf)
        (recur (inf))
        inf))))

(defcheck solution-5caefaa7
  (fn t
    ([f & a]
     (if (fn? (apply f a))
       (t (apply f a))
       (apply f a))
     )
    ([f]
     (if (fn? (f))
       (t (f))
       (f))
     )
    ))

(defcheck solution-5d6aef5
  (fn w [x & a]
    (if (fn? x)
      (w (apply x a))
      x)))

(defcheck solution-5d8ae3d7
  (fn tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-5deea78e
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v) (recur (v)) v))))

(defcheck solution-5e251335
  (fn [f & args]
    (loop [a (apply f args)]
      (if (fn? a)
        (recur (a))
        a))))

(defcheck solution-5e3f4b72
  (fn [f & args]
    (loop [f' (apply f args)]
      (if (fn? f')
        (recur (f'))
        f'))))

(defcheck solution-5f023657
  (fn pr78
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (pr78 #(apply f args)))))

(defcheck solution-5f0b2581
  (fn t [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-5f8f9159
  (fn [f & fs]
    (loop [f (apply f fs)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-60200742
  (fn [f & args] (loop [v-or-f (apply f args)] (if (fn? v-or-f) (recur (v-or-f)) v-or-f))))

(defcheck solution-60230257
  (fn prob78
    [f & args]
    (if (empty? args)
      (let [ret (f)]
        (if (fn? ret)
          (prob78 ret)
          ret))
      (prob78 #(apply f args)))))

(defcheck solution-61313691
  (fn mt [f & params]
    (let [mid-result (apply f params)]
      (if (fn? mid-result)
        (apply mt mid-result [])
        mid-result))))

(defcheck solution-6234191
  (fn tp
    ([f arg]
     (let [r (f arg)]
       (if (fn? r)
         (tp r)
         r
         )
       )
     )
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r
         )
       )
     )
    ))

(defcheck solution-62f1a1c1
  (fn tramp
    ([x] (if (fn? x) (recur (x)) x))
    ([x & args] (tramp #(apply x args)))))

(defcheck solution-6317f38a
  (fn [f & args]
    (loop [x f a args]
      (if (not (fn? x)) x
                        (recur (apply x a) '())))))

(defcheck solution-6395c77f
  (fn trampoline-1
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (trampoline-1 #(apply f args)))))

(defcheck solution-63b22525
  (fn tram [f & xs] (loop [g (apply f xs)]
                      (if (fn? g) (recur (g)) g ))))

(defcheck solution-63dd12e5
  (fn trmpol
    ([f]
     (let [r (f)]
       (if (fn? r) (recur r) r)))
    ([f & args]
     (trmpol #(apply f args)))))

(defcheck solution-63f4b806
  (fn mytramp ([f & args] (mytramp (apply f args))) ([f] (if (fn? f) (mytramp (f)) f))))

(defcheck solution-64062a64
  (fn [f & args]
    (loop [cur-f (apply f args)]
      (if (fn? cur-f)
        (recur (cur-f))
        cur-f))))

(defcheck solution-647aacfb
  (fn trmpl [f & args]
    (let [x (apply f args)]
      (if (ifn? x) (trmpl x) x))))

(defcheck solution-64a1e70f
  (fn my-trampoline
    ([x] (if (fn? x) (my-trampoline (x)) x))
    ([x & args] (my-trampoline (apply x args)))))

(defcheck solution-64dc0f40
  (fn my-tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (my-tramp #(apply f args)))))

(defcheck solution-652664fc
  (fn [f & args]
    (loop [f (apply f args)] (if (fn? f) (recur (f)) f))))

(defcheck solution-654041ef
  (fn tramp [f & args]
    (let [res   (apply f args)]
      (if (fn? res)
        (tramp res)
        res))))

(defcheck solution-657bda76
  (fn [f v]
    (loop [result (f v)]
      (if-not (fn? result)
        result
        (recur (result))))))

(defcheck solution-65b7ac8
  (fn bounce [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (bounce result)
        result))))

(defcheck solution-661485aa
  (fn t [f & x] (if (fn? f) (t (apply f x)) f)))

(defcheck solution-66ae1c86
  (fn t
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r)))
    ([f x] (t #(f x)))))

(defcheck solution-66bad9bf
  (fn my-trampoline
    ([f] (if (fn? f) (recur (f)) f))
    ([f & args] (my-trampoline (apply f args)))))

(defcheck solution-66de7e7f
  (fn t ([f a] (t (f a)))
    ([f] (if (fn? f) (t (f)) f))))

(defcheck solution-67064649
  #(loop [f (% %2)] (if (fn? f) (recur (f)) f)))

(defcheck solution-6714193e
  (fn [f & args]
    (loop [v (apply f args)]
      (if (ifn? v) (recur (v)) v))))

(defcheck solution-677f8754
  #(loop [f (% %2)]
     (if (fn? f)
       (recur (f))
       f)))

(defcheck solution-67d22d6c
  (fn my-trampoline [f & args]
    (let [res (apply f args)]
      (if (fn? res)
        (recur res ())
        res))))

(defcheck solution-68111601
  (fn t
    ([f] (let [r (f)]
           (if (fn? r)
             (recur r)
             r)))
    ([f & a] (t #(apply f a)))))

(defcheck solution-6832fc7e
  (fn [f args]
    (loop [res (f args)]
      (if (fn? res) (recur (res)) res))))

(defcheck solution-6898f250
  (fn g
    ([f] (if (fn? f) (recur (f)) f))
    ([f & s]
     (g #(apply f s)))))

(defcheck solution-68a9f1bf
  (fn [f & args] ((fn [x] (if (fn? x) (recur (x)) x)) (apply f args))))

(defcheck solution-68cacc1e
  (fn [f & args]
    (loop [r (apply f args)]
      (if (not (fn? r))
        r
        (recur (r))))))

(defcheck solution-69256225
  (fn tramp
    ([f & args] (if (not (fn? f)) f (recur (apply f args) '())))))

(defcheck solution-69a4999c
  (fn t ([f v] (t (f v)))
    ([f] (if (fn? f)
           (t (f))
           f))))

(defcheck solution-6a1f7f98
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f) (recur (f)) f))))

(defcheck solution-6a881ffc
  (fn tram [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (tram r)
        r))))

(defcheck solution-6aba0261
  (fn mytramp [x y] (let [f (atom (x y))] (do (while (fn? @f) (reset! f (@f))) @f))))

(defcheck solution-6af76b3d
  #(loop [f (%1 %2)]
     (if (fn? f)
       (recur (f))
       f)))

(defcheck solution-6b18099b
  (fn trampoline2
    ([f]
     #_(println f)
     (if (fn? f)
       (trampoline2 (f))
       f))
    ([f & args]
     #_(println f args)
     (if (fn? f)
       (trampoline2
         (apply f args))
       f))))

(defcheck solution-6b19fc5f
  (fn my-trampoline [x & varargs]
    (let [res (apply x varargs)]
      (if (fn? res)
        (my-trampoline res)
        res
        ))
    ))

(defcheck solution-6b26c7cc
  (fn trampo
    ([f & args]
     (let [res (apply f args)]
       (if (not (fn? res)) res
                           (trampo res))))
    ([f]
     (let [res (f)]
       (if (not (fn? res)) res
                           (trampo res))))))

(defcheck solution-6b4f4226
  (fn my-trimpoline [f a]
    (loop [r (f a)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-6b86621
  (fn t
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r)))
    ([f v] (t (fn [] (f v))))))

(defcheck solution-6bc87fe6
  #(loop [g (apply % %&)] (if (fn? g) (recur (g)) g)))

(defcheck solution-6be30fff
  (fn tramp
    ([f]
     (loop [res-or-fn f]
       (if (fn? res-or-fn)
         (recur (res-or-fn))
         res-or-fn)))
    ([f & args]
     (tramp (apply f args)))))

(defcheck solution-6c102967
  (fn [f & a]
    (loop [ff (apply f a)]
      (if (fn? ff)
        (recur (ff))
        ff))
    ))

(defcheck solution-6c218025
  (fn tramp
    ([a b] (tramp (a b)))
    ([a]
     (if (fn? a)
       (tramp (a))
       a))))

(defcheck solution-6c8de321
  (fn [f & args] (
                   loop [ fx (apply f args)]
                   (if (fn? fx) (recur (fx)) fx)
                   )))

(defcheck solution-6cbed00d
  (fn [& a]
    (loop [f (first a), args (rest a)]
      (let [ret (apply f args)]
        (if (fn? ret)
          (recur ret '())
          ret)))))

(defcheck solution-6d24b8ce
  (fn tramp
    ([f] (let [x (f)] (if (fn? x) (recur x) x)))
    ([f & args] (tramp #(apply f args)))))

(defcheck solution-6d9d5fe
  (fn tramp
    [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-6dba6cc5
  #(loop [x (% %2)]
     (if (fn? x)
       (recur (x))
       x)))

(defcheck solution-6de451c9
  (fn my-tramp [f x]
    (loop [v (f x)]
      (if (fn? v)
        (recur (v))
        v))))

(defcheck solution-6e4e18a7
  (fn trampo
    ([f] (let [res (f)]
           (if (ifn? res)
             (recur res)
             res)))
    ([f & args] (trampo (apply partial f args)))))

(defcheck solution-6e753b82
  (fn [f & args]
    (loop [result (apply f args)]
      (if (fn? result) (recur (result)) result))))

(defcheck solution-6ef2d02a
  (fn my-tramp [f & args]
    (loop [output (apply f args)]
      (if-not (fn? output)
        output
        (recur (output))))))

(defcheck solution-6ef6536b
  (fn
    [f & args]
    (loop [res (apply f args)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-6f4cc2fc
  (fn tra
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tra #(apply f args)))))

(defcheck solution-6f71fdc9
  (fn tramp [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (tramp result)
        result))))

(defcheck solution-70482be4
  (fn [f & args]
    (let [r (apply f args)]
      (loop [r r]
        (if (fn? r)
          (recur (r))
          r)))))

(defcheck solution-706232ca
  (fn my-tramp [f	& startvals]
    (loop	[g (apply f startvals)]
      (if	(fn? g)
        (recur (g))
        g))))

(defcheck solution-7098b6e7
  #(loop [x (apply % %&)] (if (fn? x) (recur (x)) x)))

(defcheck solution-71298438
  (fn t [f & args]
    (loop [g (apply f args)]
      (if (fn? g)
        (recur (g))
        g
        )
      )
    ))

(defcheck solution-7162d98c
  (fn [f a]
    (loop [r (f a)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-7176b9ae
  #(loop [v (apply % %&)] (if (fn? v) (recur (v)) v)))

(defcheck solution-71c81674
  (fn mytramp [f & x]
    (loop [g (apply f x)]
      (if (ifn? g) (recur (g)) g))))

(defcheck solution-7251e171
  (fn [f & params]
    (loop [f f params params]
      (if (fn? f)
        (recur (apply f params) '())
        f))))

(defcheck solution-72e7b061
  (fn [f & args]
    (first (drop-while fn? (iterate (fn [g] (g)) (apply f args))))))

(defcheck solution-72ec8a3e
  (fn tr [f & args]
    (let [result
          (if args
            (apply f args)
            (f))]
      (if (fn? result) (tr result) result))))

(defcheck solution-72fcc1e6
  (fn t ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & args]
     (let [r (apply f args)]
       (if (fn? r)
         (t r)
         r)))))

(defcheck solution-73425c9e
  (fn tramp [f & args]
    (let [res (apply f args)]
      (if (fn? res) (tramp res) res))))

(defcheck solution-734d8da
  (fn myTrampoline
    [fun & params]
    (loop [newFun (apply fun params)]
      (if (not (fn? newFun))
        newFun
        (recur (newFun))))))

(defcheck solution-73632a96
  (fn mytramp2 [f & args]
    (first (drop-while fn? (iterate #(if (fn? %) (apply % []) %) (apply f args))))))

(defcheck solution-737bb912
  (fn new-tramp
    ([f & args]
     (new-tramp (apply f args)))
    ([single-f]
     (if (fn? single-f)
       (new-tramp (single-f))
       single-f))))

(defcheck solution-73e700f4
  (fn b[f & a]
    (if (fn? f) (b (apply f a)) f)))

(defcheck solution-7419d5a5
  (fn t [f & a] (if (fn? f) (t (apply f a)) f)))

(defcheck solution-74373a89
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v)
        (recur (apply v []))
        v))))

(defcheck solution-74c40379
  (fn [f & a]
    (loop [r (apply f a)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-758c375a
  (fn [x & p]
    (if (fn? x) (recur (apply x p) []) x)))

(defcheck solution-76263457
  (fn [f & xs]
    (loop [r (apply f xs)]
      (if (not (fn? r))
        r
        (recur (r))))))

(defcheck solution-764843e8
  (fn my-trampoline [f & args]
    (loop [acc (apply f args)]
      (if (not (fn? acc))
        acc
        (recur (acc))))))

(defcheck solution-765e2639
  #(
    (fn [g]
      (if (fn? g) (recur (g)) g))
    (apply %1 %&)))

(defcheck solution-7660dd0e
  (fn tramp [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (tramp r)
        r ))))

(defcheck solution-76768603
  (fn [fun & args] (loop [f (apply fun args)] (if (fn? f) (recur (f)) f))))

(defcheck solution-76cc3dcc
  (fn tramp [f & args]
    (loop [rv (apply f args)]
      (if (fn? rv) (recur (rv)) rv))))

(defcheck solution-76da9eed
  (fn tramp
    ([f] (if (fn? f) (tramp (f)) f))
    ([f x] (tramp #(f x)))))

(defcheck solution-76ed859f
  (fn r [func & args]
    (if (fn? func) (r (apply func args)) func)))

(defcheck solution-76fe3037
  (fn [f & args]
    (loop [f? (apply f args)]
      (if (fn? f?)
        (recur (f?))
        f?))))

(defcheck solution-7737de9d
  (fn me [f & p]
    (loop [r (apply f p)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-776d7227
  (fn [f & s]
    (#(if (ifn? %) (recur (%)) %) (apply f s))))

(defcheck solution-77cf355d
  (fn [f & a] ((fn t [g] (if (fn? g) (t (g)) g)) (apply f a))))

(defcheck solution-7890b6ca
  (fn _trampoline [f & xs]
    (let [first-result (apply f xs)
          body-func (fn [f] (if (fn? f) (recur (f)) f))]
      (body-func first-result))))

(defcheck solution-78b24f58
  #(let [o (apply % %&)]
     (loop [r o]
       (if (fn? r) (recur (r)) r))))

(defcheck solution-7929fb83
  #((fn r [%1]
      (if (fn? %1) (r (%1)) %1))
    (%1 %2)))

(defcheck solution-7983acb7
  (fn [f & args]
    (if (ifn? f) (recur (apply f args) nil) f)))

(defcheck solution-79cfeb05
  (fn my-trampoline [f & xs] (if (fn? (apply f xs)) (my-trampoline (apply f xs)) (apply f xs))))

(defcheck solution-79f7e452
  (fn t [f & args] (let [r (apply f args)] (if (fn? r) (t r) r))))

(defcheck solution-7a35138e
  (fn my-trampoline
    [f & args]
    (let [v (apply f args)]
      (if (ifn? v) (my-trampoline v) v))))

(defcheck solution-7a413adf
  (fn __ [f & args]
    (let [x (apply f args)]
      (if (fn? x) (__ x)
                  x))))

(defcheck solution-7b1d9275
  (fn -trampoline [-fn  more]
    (loop [-t (-fn more)]
      (if (fn? -t) (recur (-t))
                   -t)
      )
    ))

(defcheck solution-7b5d29b9
  (fn T
    ([f]
     (let [r (f)]
       (if (fn? r) (recur r) r)))
    ([f & m]
     (T #(apply f m)))))

(defcheck solution-7b76eecc
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-7b817cd5
  (fn [f & a]
    (loop [r (apply f a)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-7bc46305
  (fn tr [f & args]
    (loop [ff (apply f args)]
      (if (ifn? ff)
        (recur (ff))
        ff))))

(defcheck solution-7bcfe2a
  (fn my-trampoline [f & args]
    (loop [val (apply f args)]
      (if (fn? val)
        (recur (val))
        val))))

(defcheck solution-7cf40fe5
  (fn custom-trampoline
    [f & args]
    (let [result (apply f args)]
      (loop [value result]
        (if (fn? value)
          (recur (value))
          value)))))

(defcheck solution-7d6db4e7
  (fn a1
    ([a & b] (a1 (apply a b)))
    ([a] (if (fn? a)
           (a1 (a))
           a))))

(defcheck solution-7e1dd0cb
  (fn t [f & args]
    (if (fn? f)
      (t (apply f args))
      f)))

(defcheck solution-7e46edfa
  (fn [f i]
    (letfn [(rt [t-fn-or-val]
              (if (fn? t-fn-or-val)
                (rt (t-fn-or-val))
                t-fn-or-val))]
      (rt (f i)))))

(defcheck solution-7e7f9271
  (fn my-trampoline [f & args]
    (let [first-result (apply f args)]
      (loop [bounced first-result]
        (if (ifn? bounced)
          (recur (bounced))
          bounced)))))

(defcheck solution-7ea20aa4
  (fn [f & args]
    (loop [res (apply f args)]
      (if (ifn? res)
        (recur (res))
        res))))

(defcheck solution-7ef8af10
  (fn [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-7f0b937a
  (fn [f & v]
    (loop [i (apply f v)]
      (if (fn? i)
        (recur (i))
        i))))

(defcheck solution-7f2843c6
  (fn [f & xs]
    (first (drop-while fn? (iterate #(if (fn? %) (%) %) (apply f xs))))))

(defcheck solution-7f6d11eb
  (fn mytrampoline
    ([f]
     (loop [ret (f)]
       (if (fn? ret)
         (recur (ret))
         ret)))
    ([f & args]
     (mytrampoline #(apply f args)))))

(defcheck solution-7f6e21c8
  (fn tramp [ f & args ]
    (let [ r (apply f args)]
      (if (fn? r) (tramp r) r)
      )
    ))

(defcheck solution-7faf538b
  (fn t [f & a] (let [r (if a (apply f a) (f))]
                  (if (fn? r) (t r) r))))

(defcheck solution-7fccc781
  (fn [f & a]
    (loop [q (apply f a)]
      (if (fn? q) (recur (q)) q))))

(defcheck solution-7fd4362c
  (fn f-78 [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (f-78 r)
        r))))

(defcheck solution-8063212c
  #(if (ifn? %) (recur (apply % %&) ()) %))

(defcheck solution-80a01861
  (fn mt
    [fn & args]
    (if (fn? fn)
      (mt (apply fn args))
      fn)))

(defcheck solution-81640d2
  (fn tramp [f & args]
    (let [res (apply f args)]
      (if (ifn? res)
        (recur res [])
        res))))

(defcheck solution-82153b03
  (fn mytrump[f & i](loop[res (apply f i)](if (fn? res) (recur (res)) res))))

(defcheck solution-82926502
  (fn [x & args]
    (loop [x (apply x args)]
      (if (fn? x) (recur (x)) x))))

(defcheck solution-839960cb
  (fn tr [f & xs]
    (if (fn? f)
      (tr (apply f xs))
      f)))

(defcheck solution-83b19e91
  (fn tramp ([f]
             (let [z (f)]
               (if (fn? z)
                 (recur z)
                 z)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-83bb74c0
  (fn [f & args]
    (loop [current (apply f args)]
      (if (fn? current)
        (recur (current))
        current))))

(defcheck solution-83eb84a0
  (fn [f & args]
    (loop [x (apply f args)]
      (if-not (fn? x)
        x
        (recur (x))))))

(defcheck solution-8419df0c
  (fn my-tramp [f & params]
    (if (not (fn? f)) f
                      (my-tramp (apply f params)))))

(defcheck solution-850c39bc
  (fn tramp [f & args]
    (->> (apply f args)
      (iterate #(%))
      (drop-while ifn?)
      first)))

(defcheck solution-85250723
  (fn [f & args]
    (let [rslt (apply f args)]
      (loop [may-be-fn rslt]
        (if-not (fn? may-be-fn)
          may-be-fn
          (recur (may-be-fn)))))))

(defcheck solution-855686b
  (fn __
    ([f] (let [
               ff (f)
               ]
           (if (fn? ff)
             (recur ff)
             ff
             )))
    ([f & args] (__ #(apply f args)))))

(defcheck solution-85aec386
  (fn my-trampoline [f & args]
    (let [r (apply f args)]
      (if (fn? r) (recur r []) r))))

(defcheck solution-85b3994d
  (fn tramp [f & args]
    (let [retval (apply f args)]
      (if (fn? retval)
        (tramp retval)
        retval))))

(defcheck solution-85ce4f11
  (fn [func & args]
    (loop [func (apply func args)]
      (if (fn? func)
        (recur (func))
        func))))

(defcheck solution-85dba79d
  #(let [x (fn r [y] (if (ifn? y) (r (y)) y))] (x (% %2))))

(defcheck solution-86329f36
  (fn trmp [f & args]
    (loop [r (apply f args)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-86418366
  (fn [ f & args]
    (loop [t (apply f args)]
      (if (fn? t) (recur (t)) t)
      )
    ))

(defcheck solution-870033d9
  (letfn [(funky [g]
            (let [ret (g)]
              (if (fn? ret)
                (recur ret)
                ret)))]
    (fn
      ([f] (funky f))
      ([f & args]
       (funky #(apply f args))))))

(defcheck solution-873d3e3
  (fn t [f & s]
    (let [r (apply f s)]
      (if (fn? r)
        (t r)
        r
        ))))

(defcheck solution-87d09fab
  (fn [f & args]
    (loop [g (apply f args)]
      (if (fn? g) (recur (g)) g))))

(defcheck solution-885c0e46
  (fn my-tpl ([f & args] (my-tpl #(apply f args)))
    ([f] (let [ret (f)]
           (if (fn? ret) (recur ret) ret)))))

(defcheck solution-885c3ad6
  (fn [f & m] (let [a (apply f m)]
                (loop [x a] (if (fn? x) (recur (x)) x)))))

(defcheck solution-88693795
  (fn [f & xs]
    (loop [f (apply f xs)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-886fcb5c
  (fn t [f & args]
    (let [g (apply f args)]
      (if (fn? g)
        (t g)
        g))))

(defcheck solution-887691ea
  (fn trampoline* [f & args]
    (let [f (apply f args)]
      (if (fn? f)
        (trampoline* f)
        f))))

(defcheck solution-88e843cd
  (fn [f & as]
    (loop [r (apply f as)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-88e8f26e
  (fn m
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (m #(apply f args)))))

(defcheck solution-897c7bd8
  (fn [f & a]
    (loop [v (apply f a)]
      (if (fn? v)
        (recur (v))
        v))))

(defcheck solution-898b84e5
  (fn [f & xs]
    (loop [n (apply f xs)]
      (if (fn? n)
        (recur (n)) n))))

(defcheck solution-899f5174
  (fn this
    ([f & params]
     (let [result (apply f params)]
       (if (fn? result)
         (apply this (list result))
         result
         ) ; if
       ) ; let
     )
    ))

(defcheck solution-89a41156
  (fn trmp [f & args]
    (let [r (apply f args)]
      (loop [r' r]
        (if (fn? r') (recur (r'))
                     r')))
    ))

(defcheck solution-89af731a
  (fn [f & c]
    (loop [res (apply f c)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-8a4acf0f
  (fn _ [f & args]
    (let [f' (apply f args)]
      (if-not (fn? f')
        f'
        (_ f')))))

(defcheck solution-8aab4290
  (fn[f & p] (#(if (fn? %) (recur (%)) %) (apply f p))))

(defcheck solution-8b24544
  (fn [f & params]
    (loop [res (apply f params)]
      (if (ifn? res)
        (recur (res))
        res))))

(defcheck solution-8b6f3825
  (fn [f & args]
    (loop [res (apply f args)]
      (if-not (fn? res)
        res
        (recur (res))))))

(defcheck solution-8b8b9208
  (fn tramp [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-8b8cb3d6
  (fn [f & xs]
    (loop[tr (apply f xs)]
      (if-not (fn? tr) tr (recur (tr))))))

(defcheck solution-8ba4199
  (fn tr
    ([f & args]
     (tr #(apply f args)))
    ([f] (let [result (f)]
           (if (fn? result) (recur result) result)))))

(defcheck solution-8bc7760d
  (fn _trampoline [fn_or_result & init]
    (if (fn? fn_or_result)
      (_trampoline (apply fn_or_result init))
      fn_or_result)))

(defcheck solution-8be890ee
  (fn [f & args]
    (if (fn? f)
      (recur (apply f args) '())
      f)))

(defcheck solution-8bf51ddb
  (letfn [
          (mytrampoline
            ([f]
             (if (fn? f) (recur (f)) f))
            ([f & args]
             (mytrampoline (apply f args)))
            )]
    mytrampoline
    ))

(defcheck solution-8bf87478
  (fn trampoline' [f & args] (if (fn? f) (trampoline' (apply f args)) f)))

(defcheck solution-8c29212f
  #(if (fn? %)
     (recur (apply % %&) ())
     %))

(defcheck solution-8c9aabd
  (fn t
    [& [f & a]]
    (let [r (if a (apply f a) (f))]
      (if (fn? r) (recur [r]) r))))

(defcheck solution-8ca21926
  #(loop [f (% %2)]
     (if (fn? f)
       (recur (f))
       f)))

(defcheck solution-8cd4cdb5
  (fn tramp [f v]
    (loop [res (f v)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-8d302322
  (fn [f & args]
    (first (drop-while fn? (iterate (fn [f] (f)) (apply f args))))))

(defcheck solution-8d4c77d2
  (fn [f & xs]
    (loop [r (apply f xs)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-8d6a5df7
  (fn my-trampoline [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-8ddc5cc8
  (fn [f & args] (let [p (apply f args)] ((fn t [a] (if (fn? a) (t (a)) a)) p))))

(defcheck solution-8e02fe0e
  (fn [f & args] (loop [res (apply f args)] (if (fn? res) (recur (res)) res))))

(defcheck solution-8e43b71
  (fn my-trampoline
    ([f & args] (my-trampoline (apply f args)))
    ([val] (if (fn? val) (recur (apply val [])) val))))

(defcheck solution-8ec8a4f
  (fn trampoline2[f & args]
    (let [y (apply f args)]
      (if (not (fn? y))
        y
        (recur  y [])))))

(defcheck solution-8f1fad23
  (fn tt
    ([f & init] (tt #(apply f init)))
    ([f]
     (let [newi (f)]
       (if (fn? newi)
         (recur newi)
         newi)))))

(defcheck solution-8f2d23ec
  (fn [y z] ((fn peu [x] (if (fn? x) (peu (x)) x)) (y z))))

(defcheck solution-8f95010
  #(loop [res (apply % %&)]
     (if (fn? res)
       (recur (res))
       res)))

(defcheck solution-8fb7d96e
  (fn [f & args]
    (let [result (apply f args)]
      (loop [r result]
        (if (fn? r)
          (recur (r))
          r)))))

(defcheck solution-91110c4e
  (fn tramp [f & args]
    (loop [g (apply f args)]
      (if (fn? g) (recur (g)) g))))

(defcheck solution-917d7ff6
  (fn bounce
    ([f]
     (if (not (fn? f))
       f
       (bounce (f))))
    ([f x]
     (bounce (f x)))))

(defcheck solution-91c1668f
  (fn [f x]
    (loop [value (f x)]
      (if (ifn? value)
        (recur (value))
        value))))

(defcheck solution-91da9fb3
  #(loop [f (apply % %&)]
     (if (fn? f)
       (recur (f))
       f)))

(defcheck solution-920175d8
  (fn [f & args]
    (letfn [(tram [f] (if (fn? f) (tram (f)) f))]
      (tram (apply f args)))))

(defcheck solution-924ddd
  (fn [f & args] (loop [r (apply f args)] (if (fn? r) (recur (r)) r))))

(defcheck solution-92b2557d
  (fn my-trampoline [f & args] (if (fn? f) (my-trampoline (apply f args)) f)))

(defcheck solution-92ec1501
  (fn trampoline-
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (trampoline- #(apply f args)))))

(defcheck solution-93988ff8
  (fn my-trampo [f & args]
    (let [r (apply f args)] (if (fn? r) (my-trampo r) r))))

(defcheck solution-94319496
  (fn tr
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tr #(apply f args)))))

(defcheck solution-94617217
  (fn [f & args]
    (loop [val (apply f args)]
      (if (ifn? val) (recur (val))
                     val)
      )))

(defcheck solution-946199ba
  (fn [f & args]
    (letfn [(trapo [x]
              (if-not (fn? x)
                x
                (recur (x))))]
      (trapo (apply f args)))))

(defcheck solution-948729
  (fn t [f & l]
    (let [a (apply f l)]
      (if (ifn? a)
        (t a)
        a))))

(defcheck solution-949a55e2
  (fn [f & args]
    (let [seqize (fn ize [f] (cons f (lazy-seq (ize (f)))))]
      (first (drop-while ifn? (seqize (apply f args)))))))

(defcheck solution-94c695f5
  (fn f[g & a] (#(if (fn? %) (f %) %) (apply g a))))

(defcheck solution-952a7d6c
  (fn my-tramp [ f & args]
    (let [res (apply f args)]
      (if (ifn? res)
        (my-tramp res)
        res))))

(defcheck solution-952a9a56
  (fn trampo [f & args]
    (if (fn? f)
      (if (empty? args)
        (recur (f) '())
        (recur (apply f args) '()))
      f)))

(defcheck solution-955e1802
  (fn [f & params]
    (let [res (apply f params)]
      (if (fn? res) (recur res nil) res))))

(defcheck solution-95c1b408
  (fn [f & args]
    (loop [nf (apply f args)]
      (if (fn? nf)
        (recur (nf))
        nf))))

(defcheck solution-95f242bb
  (fn [f & args]
    (loop [r (apply f args)]
      (if (ifn? r)
        (recur (r))
        r))))

(defcheck solution-961305c5
  (fn [f & a] (#(if (fn? %) (recur (%)) %) (apply f a))))

(defcheck solution-961f8261
  (fn mytramp [ f & args ]
    (let [ r (apply f args) ]
      (if (ifn? r) (recur r nil) r))))

(defcheck solution-96969c5
  (fn [f & args]
    (let [x (apply f args)
          mytramp (fn [x] (if (fn? x) (recur (apply x [])) x))]
      (mytramp x))))

(defcheck solution-96c5b6ce
  (fn [f & args]
    (loop [res (apply f args)]
      (if (fn? res) (recur (res)) res))))

(defcheck solution-96e063d8
  (fn [f & args]
    (let [res (apply f args)]
      (if (fn? res)
        (recur res [])
        res))))

(defcheck solution-97a1b872
  (fn tramp*
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tramp* #(apply f args)))))

(defcheck solution-97d877c1
  (fn tramp [f & args]
    (cond (not (fn? f)) f
          (empty? args) (tramp (f))
          :else (tramp (apply f args)))))

(defcheck solution-983b6fde
  (fn [f & args] (let [a (apply f args)]
                   (if (fn? a) (recur a ()) a))))

(defcheck solution-9891763f
  (fn [f & args] (loop [fctn (apply f args)] (if (fn? fctn) (recur (fctn)) fctn))))

(defcheck solution-9919521
  (fn trampolineeeeeeeeeeeeeeeeee
    [thunk-maker & args]
    (loop [thunk (apply thunk-maker args)]
      (if (fn? thunk)
        (recur (thunk))
        thunk))))

(defcheck solution-991c09ed
  (fn my-trampoline
    ([f]
     (if (fn? f)
       (my-trampoline (f))
       f))
    ([f & more]
     (my-trampoline (apply f more)))))

(defcheck solution-99da6371
  (fn tr
    ([a] (if (fn? a) (tr (a)) a))
    ([a & b] (if (fn? a) (tr (apply a b)) a))))

(defcheck solution-99e14398
  #(loop [x (% %2)]
     (if (fn? x)
       (recur (x))
       x)))

(defcheck solution-99e6a418
  #(loop [f (%1 %2)]
     (if (fn? f) (recur (f))
                 f)))

(defcheck solution-9a76ad6
  (fn tramp [f & args]
    (let [f (apply f args)]
      (loop [f f]
        (if (fn? f) (recur (f))
                    f
                    )
        )
      )
    ))

(defcheck solution-9a8e6980
  (fn [tf & args] (loop [f (apply tf args)]
                    (if (not (fn? f)) f
                                      (recur (f))))))

(defcheck solution-9a9c1b56
  (fn [f & args]
    (let [res (apply f args)]
      (if (fn? res) (recur res nil) res))))

(defcheck solution-9b23d0c0
  (fn [f & args] (loop [r (apply f args)] (if (fn? r)(recur (r)) r) )))

(defcheck solution-9b38323
  (fn [f & x]
    (loop [y (apply f x)]
      (if (fn? y)
        (recur (y))
        y
        )
      )
    ))

(defcheck solution-9c1221a2
  (fn [f & args]
    (loop [out (apply f args)]
      (if (fn? out)
        (recur (out))
        out))))

(defcheck solution-9c12c56f
  (fn tramp [f & args]
    (let [ret (apply f args)]
      (if (fn? ret)
        (tramp ret)
        ret))))

(defcheck solution-9c1d18c5
  (fn me [f & args]

    (loop [cur (apply f args)]

      (if (fn? cur)
        (recur (cur) )
        cur
        )

      )
    ))

(defcheck solution-9c346a47
  (fn mytramp [f & args]
    (let [f1 (apply f args)]
      (if (fn? f1) (mytramp f1) f1))))

(defcheck solution-9c43ba9f
  (fn [f & args]
    (let [call (fn [f]
                 (let [result (f)]
                   (if (fn? result)
                     (recur result)
                     result)))]
      (call #(apply f args)))))

(defcheck solution-9c475fb2
  (fn [f & args]
    (loop [ret (apply f args)]
      (if (not (ifn? ret))
        ret
        (recur (ret))))))

(defcheck solution-9c64eb7f
  (fn tramp [f & args]
    (let [result (apply f args)]
      (if
       (fn? result)
        (tramp result)
        result))
    ))

(defcheck solution-9c821aee
  (fn new-trampoline (
                      [f arg]
                      (let [result (f arg)]
                        (if (fn? result)
                          (new-trampoline result) result)))
    ([f]
     (if (fn? (f))
       (recur (f)) (f)))))

(defcheck solution-9caf384e
  (fn t [f & a]
    (let [r (apply f a)]
      (if (fn? r) (t r) r))))

(defcheck solution-9ccf9ee3
  (fn [f arg]
    (loop [current-fn #(f arg)]
      (let [ret (current-fn)]
        (if (fn? ret)
          (recur ret)
          ret)))))

(defcheck solution-9cd4c691
  (fn n78
    ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & args] (n78 #(apply f args)))))

(defcheck solution-9ceb5cdb
  (fn tramp
    ([f]
     (let [z (f)]
       (if (fn? z)(recur z) z)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-9d114e5d
  (fn trampoline' [f & args]
    (let [outcome (apply f args)]
      (if (fn? outcome)
        (trampoline' outcome)
        outcome))))

(defcheck solution-9d17046b
  (fn my-trampoline
    [f & args]
    (if (fn? f)
      (let [result (apply f args)]
        (if (fn? result)
          (my-trampoline result)
          result)))))

(defcheck solution-9e9e7cc7
  (fn my-tramp [f & args]
    (let [res (apply f args)]
      (if (fn? res) (recur res nil) res))))

(defcheck solution-9eff79ba
  (fn [f & args]
    (letfn [(bounce [next-bounce]
              (if (fn? next-bounce)
                (recur (next-bounce))
                next-bounce))]
      (let [next-bounce (apply f args)]
        (if (fn? next-bounce)
          (bounce next-bounce)
          next-bounce)))))

(defcheck solution-9f7cf794
  (fn [f & a] ((fn tram [f] (if (fn? f) (tram (f)) f)) (apply f a))))

(defcheck solution-a0b67c82
  (fn t ([f] (let [z (f)] (if (fn? z) (t z) z)))
    ([f & x] (let [z (apply f x)] (if (fn? z) (t z) z)))))

(defcheck solution-a1b5a9c0
  (fn trmpln [f & args]
    (let [r (apply f args)]
      (if (fn? r) (trmpln r) r))))

(defcheck solution-a1d3ff73
  (fn tr[f & a](let [x (apply f a)] (
                                      if (fn? x) (tr x) x))))

(defcheck solution-a1f9b90b
  (fn t
    ([f & args] (t (apply f args)))
    ([f] (if (fn? f)
           (recur (f))
           f))))

(defcheck solution-a231d78e
  (fn tramp
    ([f x]
     (if
      (fn? f)
       (tramp (f x))
       f
       )
     )
    ([f]
     (if
      (fn? f)
       (tramp (f))
       f
       )
     )
    ))

(defcheck solution-a24eb2b5
  (fn [f & args]
    (loop [result (apply f args)]
      (if-not (fn? result)
        result
        (recur (result))))))

(defcheck solution-a268cb49
  (fn t [f & a]
    (let [r (apply f a)]
      (if (fn? r)
        (t r)
        r))))

(defcheck solution-a3cb447a
  (fn my-trampoline [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret) (recur (ret)) ret))))

(defcheck solution-a45f32f5
  (fn [f & as]
    (loop [r (apply f as)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-a464dee2
  (fn [f & args]
    (->> (apply f args) (iterate #(%)) (remove fn?) first)))

(defcheck solution-a4d5df09
  (fn g [f & x] (let [r (apply f x)] (if (fn? r) (g r) r))))

(defcheck solution-a4feebf2
  (fn [f & args]
    (loop [g (apply f args)]
      (if (fn? g)
        (recur (g))
        g))))

(defcheck solution-a519002e
  (fn ! [x & xs]
    (if (ifn? x)
      (! (apply x xs))
      x)))

(defcheck solution-a54e4c62
  (fn mytramp
    ([f]
     (if
      (fn? (f)) (mytramp (f))
                (f)))
    ([f & args]
     (mytramp #(apply f args)))))

(defcheck solution-a57a4bfd
  (fn trampoline* [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (trampoline* result)
        result))))

(defcheck solution-a619ed5
  #((fn t [f] (if (fn? f) (t (f)) f))
    (apply % %&)))

(defcheck solution-a624f53c
  (fn [f & args]
    (let [r (apply f args)]
      (loop [s r]
        (if (fn? s)
          (recur (s))
          s)))))

(defcheck solution-a6cfe283
  (fn my-trampoline
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (my-trampoline #(apply f args)))))

(defcheck solution-a7398827
  (fn mtramp
    ([f & args] (let [r (apply f args)] (if (fn? r) (mtramp r) r)))
    ([f] (let [r (f)] (if (fn? r) (mtramp r) r)))))

(defcheck solution-a7742f86
  (fn tp [f & args]
    (if (ifn? f)
      (tp (apply f args))
      f)))

(defcheck solution-a7ba7fbc
  (fn mytrampoline [f & args]
    (let [r (apply f args)] (if (fn? r) (mytrampoline r) r))
    ))

(defcheck solution-a7cebcd0
  (fn [f & args]
    (loop [f (apply f args)]
      (if (ifn? f)
        (recur (f))
        f))))

(defcheck solution-a8061d27
  (fn tramp
    ([f]
     (let [f (f)]
       (if (fn? f)
         (recur f)
         f)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-a8a36952
  (fn tramp [f & args]
    (if (fn? f)
      (tramp (apply f args))
      f
      )
    ))

(defcheck solution-a8f22555
  (fn
    [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-a90b7b2b
  (fn tramp
    ([f]
     (if (fn? f)
       (tramp (f))
       f))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-a96a6640
  (fn my-trampoline [f & args]
    (loop [res (apply f args)]
      (if (not (fn? res)) res
                          (recur (res))))))

(defcheck solution-a9970c64
  (fn xtramp [f & args]
    (let [res (apply f args)]
      (if (fn? res)
        (xtramp res)
        res))))

(defcheck solution-a9a9b9af
  (fn tr
    ([f a]
     (tr (f a)))
    ([f]
     (if (fn? f) (tr (f))
                 f))))

(defcheck solution-aa2e443b
  (comp #(if (fn? %) (recur (%)) %) #(apply %1 %&)))

(defcheck solution-aab7972f
  (fn [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (recur r '())
        r))))

(defcheck solution-aab94af1
  (fn [f & xs]
    (->> (apply f xs)
      (iterate #(apply % '()))
      (drop-while fn?)
      first)))

(defcheck solution-ab4c47b9
  (fn tramp [fun & args]
    (if (fn? fun)
      (tramp (apply fun args))
      fun
      )
    ))

(defcheck solution-ab60313c
  (fn my-trampoline
    ([result] (if (fn? result) (my-trampoline (result)) result))
    ([f & args] (my-trampoline (apply f args)))))

(defcheck solution-ab969d45
  (fn [f & args]
    (let [result (apply f args)]
      (if (fn? result) (recur result ()) result))))

(defcheck solution-abba8099
  (fn [f & args]
    (loop [f f
           args args]
      (let [result (apply f args)]
        (if (fn? result)
          (recur result [])
          result
          )
        )
      )
    ))

(defcheck solution-aced34b8
  (fn tramp
    [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-ad509522
  (fn mytrampoline [f & args]
    (let [retval (if args (apply f args) (f))]
      (if (fn? retval)
        (mytrampoline (apply f args))
        retval))))

(defcheck solution-ad5ea8f2
  (fn [f & args]
    (loop [fv (apply f args)]
      (if (fn? fv) (recur (fv))
                   fv))))

(defcheck solution-ad650848
  (fn [f & args] (loop [f (apply f args)] (if (fn? f) (recur (f)) f))))

(defcheck solution-ad893dbf
  (fn mytrampoline ([op x] (let [res (op x)]
                             (if (not (fn? res))
                               res
                               (mytrampoline res))))
    ([op] (let [res (op)]
            (if (not (fn? res))
              res
              (mytrampoline res))))))

(defcheck solution-ad98fdf4
  (fn [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-ada19ff3
  (fn t
    ([f] (let [r (f)] (if (fn? r) (recur r) r)))
    ([f & args] (t #(apply f args)))))

(defcheck solution-ada91a6a
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x) (recur (x)) x))))

(defcheck solution-addbfbe
  #(loop [g (apply % %&)]
     (if (fn? g) (recur (g))
                 g)))

(defcheck solution-ae1ae73
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-ae756392
  (fn my-trampoline [f & args]
    (let [value (apply f args)]
      (loop [r value]
        (if (fn? r)
          (recur (r))
          r)))))

(defcheck solution-aeafdb
  #(if (fn? %)
     (recur (apply % %&) ())
     %))

(defcheck solution-aec425b3
  (fn tr ( [f]
          (let [ret (f)]
            (if (fn? ret)
              (recur ret)
              ret)))
    ([f & args]
     (tr #(apply f args)))))

(defcheck solution-af006fbd
  (fn t [f & args]
    (loop [v (apply f args)]
      (if (fn? v)
        (recur (v))
        v))))

(defcheck solution-af05bbf5
  (fn [f x] (loop [current (f x)]
              (if (not (ifn? current)) current
                                       (recur (current))))))

(defcheck solution-af3d4b20
  (fn my-tramp [f & args]
    (let [g (if (seq? args) (apply f args) (f))]
      (if (fn? g)
        (my-tramp g)
        g
        ))))

(defcheck solution-af53a33
  (fn g [f & a]
    (let [r (apply f a)]
      (if (fn? r)
        (g r)
        r))))

(defcheck solution-af6b172c
  (fn tramp
    [f & args]
    (let [res (apply f args)]
      (loop [f res]
        (if (fn? f)
          (recur (f))
          f)))))

(defcheck solution-af73d8eb
  (fn mytramp [f & args]
    (loop [r (apply f args)]
      (if (ifn? r)
        (recur (r)) r))))

(defcheck solution-af869a12
  (fn [f m] (loop [f (f m)]
              (if-not (fn? f) f
                              (recur (f))))))

(defcheck solution-afc3e7e8
  (fn [f & args]
    (loop [f2 (apply f args)]
      (if (fn? f2)
        (recur (f2))
        f2))))

(defcheck solution-b040db71
  (fn [f & a]
    (loop [g (apply f a)]
      (if (fn? g)
        (recur (g))
        g) ) ))

(defcheck solution-b0502a4b
  (fn [f & args]
    (loop [result (apply f args)]
      (if (fn? result) (recur (result))
                       result))))

(defcheck solution-b07bb339
  (fn [x & args]
    (loop [x (apply x args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-b0b87373
  (fn[f & args](
                 loop[nf (apply f args)](
                                          if(not (fn? nf)) nf
                                                           (recur (nf))
                                                           )
                                        )))

(defcheck solution-b199ce80
  (fn my-trampoline [f & args]
    (loop [res (apply f args)]
      (if-not (fn? res)
        res
        (recur (res))))))

(defcheck solution-b1cef27a
  (fn t [x y]
    (loop [f (x y)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-b1e353b9
  (fn [f & rst]
    (if (fn? f)
      (recur (apply f rst) [])
      f)))

(defcheck solution-b26687ea
  (fn tramp
    ([f & args] (tramp #(apply f args)))
    ([f]
     (let [v (f)]
       (if (fn? v)
         (recur v)
         v)))))

(defcheck solution-b291c15f
  #(loop [r (% %2)]
     (if (fn? r) (recur (r)) r)))

(defcheck solution-b29e1b44
  (fn t*
    ([f]
     (if (fn? f)
       (recur (f))
       f))
    ([f & args]
     (t* (apply f args)))))

(defcheck solution-b3978876
  (fn my-tramp [f & args]
    (loop [g f a args]
      (let [r (apply g a)]
        (if (not (fn? r))
          r
          (recur r ()))))))

(defcheck solution-b3cc7a6d
  (fn my-trampoline
    ([f v] (my-trampoline (f v)))
    ([f] (if (fn? f) (my-trampoline (f)) f))))

(defcheck solution-b3e83821
  (fn tramp [f & args]
    (let [rec (fn rec [v]
                (if (fn? v)
                  (rec (v))
                  v))]
      (rec (apply f args)))))

(defcheck solution-b4782a4c
  (fn trampoline* [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-b52e3664
  (fn [f n] (->> (f n)
              (iterate #(if (fn? %) (%) %))
              (drop-while fn?)
              first)))

(defcheck solution-b59ebcee
  (fn [f & args]
    (loop [g (apply f args)]
      (if (ifn? g)
        (recur (g)) g))))

(defcheck solution-b5c92166
  (fn tr [& args] (let [r (apply (first args) (rest args))] (if (fn? r) (recur [r]) r))))

(defcheck solution-b5db7151
  (fn my-trampoline [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-b5f79c85
  (fn [f & args]
    (loop [thunk (apply f args)]
      (if (fn? thunk)
        (recur (thunk))
        thunk))))

(defcheck solution-b6039d91
  (fn my-trampoline [f & args]
    (loop[g (apply f args)]
      (if (ifn? g)
        (recur (g))
        g))))

(defcheck solution-b6eaee1a
  (fn tramp [f & args]
    (loop [o (apply f args)]
      (if (fn? o) (recur (o)) o)
      )
    ))

(defcheck solution-b6fa394f
  (fn trampoline-1
    ([f]
     (if (fn? (f))
       (recur (f))
       (f)))
    ([f & args]
     (trampoline-1 #(apply f args)))))

(defcheck solution-b70be82c
  (fn [f & args]
    (loop [fi (apply f args)]
      (if (fn? fi)
        (recur (fi))
        fi))))

(defcheck solution-b73e58e7
  (fn  [f & i]
    (let [f (apply f i)]
      (if (fn? f)
        (recur f nil)
        f))))

(defcheck solution-b771a50f
  (fn my-trampoline
    ([f & args]
     (my-trampoline (apply f args)))
    ([f]
     (if (fn? f)
       (recur (f))
       f))))

(defcheck solution-b7caebb6
  (fn [x & a]
    (if (fn? x) (recur (apply x a) []) x)))

(defcheck solution-b7ed88f
  (fn my-tramp [f & rest]
    (loop [ans (apply f rest)]
      (if (fn? ans)
        (recur (ans))
        ans))))

(defcheck solution-b7f5c57a
  (fn tramp [f & args]
    (let [nextf (apply f args)]
      (if (ifn? nextf)
        (tramp nextf)
        nextf))))

(defcheck solution-b805dbeb
  (fn mytrampoline [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (mytrampoline result)
        result))))

(defcheck solution-b839774c
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x) (recur (x)) x) )))

(defcheck solution-b844f58e
  (fn [f & args]
    (loop [f1 (apply f args)]
      (if (fn? f1) (recur (f1))
                   f1))))

(defcheck solution-b8d0ac45
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f ))
        f))))

(defcheck solution-b8e39ffc
  (fn [f & args]
    (loop [g (apply f args)]
      (if (fn? g)
        (recur (g))
        g))))

(defcheck solution-b90031d5
  (fn [f & args]
    (loop [nxt #(apply f args)]
      (if (fn? nxt)
        (recur (nxt))
        nxt))))

(defcheck solution-b970d2f7
  (fn [f & args]
    (let [g (apply f args)]
      (loop [g g]
        (if (fn? g)
          (recur (g))
          g)))))

(defcheck solution-b9896760
  (fn tr [f x]
    (loop [result (f x)]
      (if-not (ifn? result)
        result
        (recur (result))))))

(defcheck solution-b9b53fc4
  (fn tramp
    [f & rest]
    (if (fn? f)
      (recur (apply f rest) nil)
      f)))

(defcheck solution-ba474f58
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-ba7dc5
  (fn [a-fn & args]
    (loop [curr-val (apply a-fn args)]
      (if (fn? curr-val)
        (recur (curr-val))
        curr-val))))

(defcheck solution-bad8995
  (fn tramp
    ([f]
     (if (fn? f)
       (tramp (f))
       f))
    ([f & args] (tramp (apply f args)))))

(defcheck solution-baf60bbc
  (fn [f & args]
    (loop [res (apply f args)]
      (if (fn? res) (recur (res)) res))))

(defcheck solution-bbb6f0fc
  (fn mytramp [f & ps]
    (let [r (apply f ps)]
      (if (fn? r)
        (mytramp r)
        r
        )
      )
    ))

(defcheck solution-bc1ee14c
  (fn t
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args] (t #(apply f args)))))

(defcheck solution-bc3a7c14
  (fn [f & args]
    (let [sf (apply f args)]
      (loop [ff sf]
        (if-not (fn? ff)
          ff
          (recur (ff)))))))

(defcheck solution-bc4565ac
  (fn [& lst]
    (loop [r (apply (first lst) (rest lst))]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-bc7848ae
  (fn z ([f] (if (fn? f) (z (f)) f))
    ([f & args] (z (apply f args)))))

(defcheck solution-bc8b0237
  (fn my-trampoline [f & args]
    (let [app (apply f args)]
      (loop [a app]
        (if (fn? a)
          (recur (a))
          a)))))

(defcheck solution-bcbd6491
  (fn [func & args]
    (let [res (apply func args)]
      (loop [val res]
        (if (ifn? val)
          (recur (val))
          val)))))

(defcheck solution-bcd3b033
  #((fn r [%]
      (if (fn? %) (r (%)) %))
    (% %2)))

(defcheck solution-bce25a78
  (fn myfun
    ([fun & val](let[y (apply fun  val) ](if(fn? y) (myfun y) y)))
    ([fun](let[y (#(fun))](if (fn? y) (myfun y) y)))))

(defcheck solution-bd4e6a34
  #((fn [g] (if (fn? g) (recur (g)) g)) (% %2)))

(defcheck solution-bd937218
  (fn tramp
    ([f & args]
     (tramp #(apply f args)))
    ([f]
     (let [val (f)]
       (if (fn? val)
         (recur val)
         val
         )
       )
     )))

(defcheck solution-bdbdbac1
  (fn [f1 & args]
    (loop [f (apply f1 args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-be1fa202
  (fn [f & x]  (loop [g (apply f x)]
                 (if (fn? g) (recur (g)) g))))

(defcheck solution-be492cb9
  (fn [f & args]
    (loop [x (apply f args)]
      (if (ifn? x)
        (recur (x))
        x))))

(defcheck solution-be8cecc8
  (fn f
    ([x & y]
     (let [r (apply x y)]
       (if (fn? r)
         (f r)
         r)))))

(defcheck solution-bed529e
  (fn [f & args]
    (loop [maybe-f (apply f args)]
      (if-not (fn? maybe-f)
        maybe-f
        (recur (maybe-f))))))

(defcheck solution-bedaa644
  (fn [f & args]
    (loop [r (apply f args)]
      (if (not (fn? r)) r
                        (recur (r))
                        )
      )
    ))

(defcheck solution-bf3bd128
  #(loop [y (apply % %&)]
     (if (fn? y)
       (recur (y))
       y)))

(defcheck solution-bf44b6df
  (fn trmp [f & args]
    (loop [func (apply f args)]
      (if (fn? func)
        (recur (func))
        func))))

(defcheck solution-bf4eccf8
  (fn t
    ([fun & args]
     (t (apply fun args)))
    ([fun]
     (if (ifn? fun) (recur (fun)) fun))))

(defcheck solution-c0255526
  (fn [f & args]
    (let [res (apply f args)]
      (if (fn? res) (recur res nil)
                    res))))

(defcheck solution-c0be0bf4
  (fn [f x] (letfn [(repeater [f] (if (ifn? f) (recur (f)) f))]
              (repeater (f x)))))

(defcheck solution-c11e905b
  (fn [f & a]
    (if (fn? f)
      (recur (apply f a) nil)
      f)))

(defcheck solution-c192189e
  #((fn it [g] (if (fn? g) (it (g)) g)) (% %2)))

(defcheck solution-c1c05f76
  (fn myt
    ([f & args] (myt (apply f args)))
    ([f] (if (fn? f) (recur (f)) f))))

(defcheck solution-c201960d
  (fn [f & args]
    (loop [rv (apply f args)]
      (if (fn? rv) (recur (rv)) rv))))

(defcheck solution-c281e9bd
  (fn [f & args]
    (loop [rfunc (apply f args)]
      (if (fn? rfunc)
        (recur (rfunc))
        rfunc))))

(defcheck solution-c29f6ba3
  (fn myf [f & args]
    (let [res (apply f args)]
      (loop [f' res]
        (if-not (fn? f')
          f'
          (recur (f')))))))

(defcheck solution-c35ee023
  (fn [f & args] (loop [f (apply f args) ] (if (not (fn? f))  f (recur (f)) ))))

(defcheck solution-c3a81e4e
  (fn jump [f & args]
    (let [next (apply f args)]
      (if (fn? next)(jump next) next ))))

(defcheck solution-c47cccf4
  (fn [f & args]
    (letfn [(t [f]
              (if (fn? f)
                (t (f)) f))]
      (t (apply f args)))))

(defcheck solution-c4be61ef
  (fn [f & x]
    (loop [g (apply f x)]
      (if-not (ifn? g)
        g
        (recur (g))))
    ))

(defcheck solution-c4d32010
  (fn tramp ([f]
             (let [ret (f)]
               (if (fn? ret)
                 (recur ret)
                 ret)))
    ([f x]
     (tramp #(f x)))))

(defcheck solution-c4e0e6df
  (fn p78 ([f]
           (let [ret (f)]
             (if (fn? ret)
               (recur ret)
               ret)))
    ([f & args]
     (p78  #(apply f args)))))

(defcheck solution-c50e87d4
  (fn [f x] (loop [g (f x)] (if (ifn? g) (recur (g)) g))))

(defcheck solution-c548e8bc
  (fn [f & args]
    (loop [r (apply f args)]
      (if (not (fn? r))
        r
        (recur (r))))))

(defcheck solution-c5c76904
  (fn [f & args]
    (let [rslt (apply f args)]
      (if (fn? rslt)
        (recur rslt [])
        rslt))))

(defcheck solution-c5d4febf
  (fn my-trampoline
    [f & args]
    (let [first-result (apply f args)]
      (loop [result first-result]
        (if (fn? result)
          (recur (result))
          result)))))

(defcheck solution-c62f8c34
  (fn trmpln
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (trmpln #(apply f args)))))

(defcheck solution-c782c123
  (fn myTrampoline [f & p] (loop [res (apply f p)] (if (fn? res) (recur (res)) res))))

(defcheck solution-c7ada386
  (fn my-trampoline [f & xs]
    (let [ r (apply f xs) ]
      (if (not (fn? r))
        r
        (recur r ())))))

(defcheck solution-c8c4cc3a
  (fn [func & coll]
    (let [start (apply func coll)
          results (iterate #(%) start)
          filtered (filter #(not (ifn?  %)) results)]
      (first filtered))))

(defcheck solution-c91cd696
  (fn [f & args]
    (let [result (apply f args)]
      (if (fn? result)
        (recur result nil)
        result))))

(defcheck solution-c9808a3a
  (fn [f & xs]
    (if (fn? f) (recur (apply f xs) nil) f)))

(defcheck solution-cb98bd20
  (fn [f & args]
    (loop [result #(apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-cbb1b53a
  (fn tramp [f & args]
    (if (fn? f)
      (tramp (apply f args))
      f)))

(defcheck solution-cbf33f2f
  (fn tramp [f & more]
    (if (fn? f) (tramp (apply f more)) f)))

(defcheck solution-cc58c97d
  (fn my-tramp [f & args]
    (let [ret (apply f args)]
      (loop [start ret]
        (if (not (fn? start)) start
                              (recur (start)))))))

(defcheck solution-ccb07d84
  (fn ! [& p]
    (loop [result
           (if (= 2 (count p))
             ((first p) (second p))
             (first p)
             )
           ]
      (if (fn? result)
        (recur (result))
        result
        )
      )
    ))

(defcheck solution-cd57513
  (fn [f x]
    (loop [v (f x)]
      (if (fn? v)
        (recur (v))
        v))))

(defcheck solution-cfbda408
  (fn bounce [f & args]
    (loop [result (apply f args)]
      (if (fn? result)
        (recur (result))
        result))))

(defcheck solution-cfe0dd62
  (fn t [f & args]
    (let [v (apply f args)]
      (if (fn? v)
        (t v)
        v))))

(defcheck solution-d01ba462
  (fn [f v]
    (let
     [
      f' (f v)
      ]
      (loop
       [f' f']
        (if
         (fn? f')
          (recur (f'))
          f'
          )
        )
      )
    ))

(defcheck solution-d02e114d
  (fn i
    [f & xs]
    (let [v (if-let [[l] xs] (f l) (f))]
      (if (fn? v) (i v) v))))

(defcheck solution-d09be049
  (fn my-trampoline [f & args]
    (loop [tbc (apply f args)]
      (if (fn? tbc)
        (recur (tbc))
        tbc))))

(defcheck solution-d10a03ba
  (fn my-trampoline[f & more]
    (loop  [result (apply f more)]
      (if (fn? result)
        (recur (result))
        result
        )
      )
    ))

(defcheck solution-d1b9ae98
  #(loop [y (apply % %&)]
     (if (fn? y) (recur (y)) y)))

(defcheck solution-d1fdb5b4
  (fn [f & args]
    (first (drop-while fn? (iterate #(do (%)) (apply f args))))))

(defcheck solution-d23822d5
  (fn tramp
    ([f & args] (tramp (apply f args)))
    ([f]
     (if (fn? f)
       (tramp (f))
       f))))

(defcheck solution-d27e977d
  #(loop [g (apply %1 %&)]
     (if-not (fn? g)
       g
       (recur (g)))))

(defcheck solution-d2ba3996
  #(loop [a (% %2)] (if (fn? a) (recur (a)) a)))

(defcheck solution-d3d45832
  (fn [f & args]
    (loop [func #(apply f args)]
      (let [res (func)]
        (if (fn? res) (recur res) res)))))

(defcheck solution-d3d9a63c
  #(loop [x (% %2)] (if (fn? x) (recur (x)) x)))

(defcheck solution-d3e870e7
  (fn my-trampoline
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (my-trampoline #(apply f args)))))

(defcheck solution-d3f5c9c7
  (fn tramp [ f & args ]
    (cond
      (not (empty? args)) (tramp (apply f args))
      (ifn? f) (recur (f) '())
      :else f)))

(defcheck solution-d3fce5a
  (fn [f & xs]
    (if (fn? f) (recur (apply f xs) nil) f)))

(defcheck solution-d4142fac
  (fn [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-d58f2335
  (fn [f & args]
    (loop [nf #(apply f args)]
      (if (fn? nf) (recur (nf)) nf))))

(defcheck solution-d59d425
  (fn jumpy-thing
    ([f & args] (jumpy-thing (apply f args)))
    ([x] (if (fn? x) (jumpy-thing (x)) x))))

(defcheck solution-d5a89e9e
  (fn[f x]
    (first (drop-while fn?
             (iterate #(%) (f x))))))

(defcheck solution-d5f1a383
  ;(fn [f & x]
  ;  (first (drop-while fn?
  ;                     (iterate (fn [f] (if (fn? f) (f) f))
  ;                              (apply f x)))))

  ; shorter + more readable
  (fn [f & x]
    (if (fn? f)
      (recur (apply f x) [])
      f)))

(defcheck solution-d60a16e4
  (fn tramp ([f]
             (let [ret (f)]
               (if (fn? ret)
                 (recur ret)
                 ret)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-d6acc2b2
  (fn trmp [f init]
    (letfn [(bounce [-f]
              (if (ifn? -f)
                (recur (-f))
                -f))]
      (bounce (f init)))))

(defcheck solution-d6d53b1c
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-d6d9376d
  (fn my-trampoline [ f & args]
    (letfn[(tramp
             [f]
             (if(fn? f)
               (recur (f))
               f))]
      (tramp (apply f args)))))

(defcheck solution-d6f62662
  (fn t [f & xs]
    (if (fn? f)
      (t (apply f xs))
      f)))

(defcheck solution-d7535d5b
  (fn [f & params]
    (let [result (apply f params)
          call-until (fn call-until [f]
                       (if (fn? f)
                         (recur (f))
                         f))
          ]
      (call-until result))))

(defcheck solution-d759380e
  (fn t [f & x]
    (loop [v f, a x]
      (if (fn? v)
        (recur (apply v a) nil)
        v))))

(defcheck solution-d7e896ca
  (fn [f & args] (let [ret (apply f args)]
                   (loop [ret ret]
                     (if (fn? ret) (recur (ret)) ret)))))

(defcheck solution-d8a07507
  (fn [f & x]
    (let [r (apply f x) ]
      (if (fn? r) (recur r ()) r))))

(defcheck solution-d8bc0a6d
  (fn [f & args]
    (letfn [
            (x [f]
              (let [
                    r (f)]
                (if (fn? r)
                  (x r)
                  r)))]
      (x #(apply f args)))))

(defcheck solution-d929ae5f
  (fn f-tramp [f & args]
    (loop [r (apply f args)]
      (if (fn? r) (recur (r)) r))))

(defcheck solution-d9b203b1
  (fn t
    ([f]
     (let [v (f)]
       (if (fn? v)
         (recur v)
         v)))
    ([f & a]
     (t #(apply f a)))))

(defcheck solution-d9e0945a
  (fn !
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (! ret)
         ret)))
    ([f & args]
     (! #(apply f args)))))

(defcheck solution-d9e8c826
  (fn [f & args]
    (if (fn? f)
      (recur (apply f args) nil)
      f)))

(defcheck solution-da3c07ea
  (fn mytramp
    ([f]
     (let [retf (f)]
       (if (fn? retf)
         (recur retf)
         retf
         ))
     )
    ([f & args]
     (mytramp #(apply f args))
     )
    ))

(defcheck solution-daca51d1
  (fn [f & args]
    (loop [ff (apply f args)]
      (if (fn? ff)
        (recur (ff))
        ff))))

(defcheck solution-db200453
  (fn t [x & a]
    (if (fn? x)
      (t (apply x a))
      x)))

(defcheck solution-db5c364c
  (fn t [f & args]
    (loop [v (apply f args)]
      (if (fn? v) (recur (v)) v))))

(defcheck solution-dbe95dc4
  (fn [f & args]
    (first
      (drop-while
        fn?
        (reductions
          (fn [a _] (a))
          (apply f args)
          (range))))))

(defcheck solution-dbf5c452
  #(loop [r (apply %1 %&)]
     (if (fn? r)
       (recur (r))
       r)))

(defcheck solution-dc0e0991
  (fn [f & params]
    (loop [res (apply f params)]
      (if (not (fn? res))
        res
        (recur (res))))))

(defcheck solution-dc614991
  (fn my-trampoline
    ([x] (if (fn? x) (my-trampoline (x)) x))
    ([f & args] (my-trampoline (apply f args)))))

(defcheck solution-dc820de3
  #((fn r [x]
      (if (fn? x) (r (x)) x))
    (% %2)))

(defcheck solution-dd2b0e08
  (fn my-trampoline
    ([f]
     (let [result (f)]
       (if-not (ifn? result)
         result
         (my-trampoline result))))
    ([f & args] (my-trampoline #(apply f args)))))

(defcheck solution-ddf13fc
  (fn tramp [f & args] (if (fn? f) (tramp (apply f args)) f)))

(defcheck solution-de2f3e75
  (fn m-trampoline [in & args]
    (if (fn? in)
      (m-trampoline (apply in args))
      in)))

(defcheck solution-de7117a3
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x)) ))

(defcheck solution-de820233
  (fn p78
    [f & params]
    (let [result (apply f params)]
      (if (fn? result)
        (recur result ())
        result))))

(defcheck solution-de9e1579
  (fn trampo [f & args]
    (if (not (fn? f)) f
                      (recur (apply f args) nil))))

(defcheck solution-df100632
  (fn [func & args]
    (let [primeiro-elemento (apply func args)]
      (if (fn? primeiro-elemento)
        ((last (take-while fn? (iterate #(%) (apply func args)))))
        primeiro-elemento))))

(defcheck solution-df15ad8b
  (fn newTramp [f & v ]
    (if (empty? v)
      (if (fn? f)
        (newTramp (f)) f)
      (newTramp (apply f v)))))

(defcheck solution-df31c5e3
  (fn t [a & b]
    (if (nil? b)
      (let [r (a)] (if (fn? r) (t r) r))
      (t #(apply a b)))))

(defcheck solution-dfe1bdd
  (fn [f & args]
    (loop [v (apply f args)]
      (if (fn? v) (recur (v)) v))))

(defcheck solution-e0303467
  (fn t [f & p] (if (fn? f) (t (apply f p)) f)))

(defcheck solution-e07f615e
  (fn tramp [f & args]
    (loop [f f args args]
      (if (fn? f)
        (recur (apply f args) [])
        f))))

(defcheck solution-e1273d44
  (fn [f & args]
    (loop [f f x (apply f args)]
      (if (fn? x)
        (recur f (x))
        x))))

(defcheck solution-e15f26f3
  #(loop [r (apply % %&)]
     (if (fn? r)
       (recur (r))
       r)))

(defcheck solution-e16e3be6
  (fn tr [f & args]
    (if (ifn? f)
      (tr (apply f args)) f)))

(defcheck solution-e1a1ebc3
  (fn tra ([f]
           (let [ret (f)]
             (if (fn? ret) (recur ret) ret)))
    ([f & args] (tra #(apply f args)))))

(defcheck solution-e1de9bcc
  (fn tramp [t & a]
    (loop [xxx (apply t a)]
      (if (fn? xxx)
        (recur (xxx))
        xxx))))

(defcheck solution-e274c173
  (fn t [f & l]
    (let [r (apply f l)]
      (if (fn? r)
        (t r)
        r))))

(defcheck solution-e2ba0228
  (fn [f & xs]
    (loop [y (apply f xs)]
      (if (fn? y) (recur (y)) y))))

(defcheck solution-e2bde1e
  #((fn [f] (if (fn? f) (recur (f)) f)) (%1 %2)))

(defcheck solution-e2fd997
  (fn myf2 [f & args]
    (loop [f (apply f args)]
      (if (fn? f) (recur (f)) f))))

(defcheck solution-e34ca302
  (fn [f & args]
    (loop [x (apply f args)]
      (if (fn? x) (recur (x))
                  x))))

(defcheck solution-e35b0725
  (fn my-trampoline
    ([func]
     (if (fn? func)
       (recur (func))
       func))
    ([func & args]
     (my-trampoline (apply func args)))))

(defcheck solution-e3ac334a
  (fn trampoline' [f & rest]
    (let [result (apply f rest)]
      (loop [r result]
        (if (fn? r)
          (recur (r))
          r)))))

(defcheck solution-e3b1f2e8
  (fn trampoline*
    ([f & args] (trampoline* #(apply f args)))
    ([f] (let [ret (f)] (if (fn? ret) (recur ret) ret)))))

(defcheck solution-e3d7fae9
  (fn [f & args] (loop [t #(apply f args)] (let [v (t)] (if (fn? v) (recur v) v)))))

(defcheck solution-e3fb1643
  (fn tr [f & args]
    (if (fn? f) (recur (apply f args) []) f)
    ))

(defcheck solution-e426ac74
  (fn tram [p_f & p_val]
    (loop [f (apply p_f p_val) ]
      (if (not (fn? f))
        f
        (recur (f) )
        )
      )
    ))

(defcheck solution-e499a64b
  (fn tramp [f & args]
    (loop [ff #(apply f args)]
      (if (ifn? ff)
        (recur (ff))
        ff))))

(defcheck solution-e4d6ef3d
  #(loop [ret (% %2)] (if (fn? ret) (recur (ret)) ret)))

(defcheck solution-e4fd9269
  (fn [fun & args]
    (if (fn? fun)
      (recur (apply fun args) [])
      fun)))

(defcheck solution-e52ca73b
  (fn my-trampoline [f & args]
    (let [result (apply f args)]
      (if ((complement fn?) result)
        result
        (recur result [])))))

(defcheck solution-e541b2a
  (fn [x & y]
    ((fn t [a]
       (if (ifn? a) (t (a)) a)) (apply x y))))

(defcheck solution-e54398de
  (fn prob-0078
    [f & f0-args]
    (loop [f-or-v (apply f f0-args)]
      (if (fn? f-or-v)
        (recur (f-or-v))
        f-or-v))))

(defcheck solution-e60d1b4
  (fn tramp [f & args]
    (let [r (apply f args)]
      (if (fn? r) (tramp r) r))))

(defcheck solution-e625bd96
  (fn tramp [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-e67917c2
  (fn [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (recur r nil)
        r))))

(defcheck solution-e6794901
  (fn tramp
    ([arg]
     (let [f (arg)]
       (if (fn? f)
         (recur f)
         f)))
    ([f & args]
     (tramp (fn [] (apply f args))))))

(defcheck solution-e697a0
  (fn f [f & as]
    (loop [ret (apply f as)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-e6c336b7
  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f
        )
      )
    ))

(defcheck solution-e7087448
  (fn g [f & r]
    (if (fn? f) (g (apply f r)) f)))

(defcheck solution-e7164d5d
  (fn tr [f & rest]
    (if (fn? f)
      (tr (apply f rest))
      f
      )
    ))

(defcheck solution-e74cd39e
  (fn tramp [f & args]
    (let [ retval (apply f args) ]
      (if (fn? retval)
        (recur retval [])
        retval))))

(defcheck solution-e772e0c
  (fn t [f & args]
    (let [r (apply f args)]
      (if (fn? r) (t r) r))))

(defcheck solution-e78bfdea
  (fn [func & x]
    (loop [func (apply func x)]
      (if (fn? func)
        (recur (func))
        func
        )
      )

    ))

(defcheck solution-e7e98580
  #(if (fn? %)
     (recur (apply % %&) [])
     %))

(defcheck solution-e8bab98f
  (fn x [f & r]
    (if (fn? f)
      (x (apply f r))
      f)))

(defcheck solution-e8e64c0
  (fn tramp
    ([f] (if (fn? f) (recur (f)) f))
    ([f & args] (tramp (apply f args)))))

(defcheck solution-e908facb
  (fn def_trampoline [foo & args]
    (loop [f (apply foo args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-e90c8484
  (fn [& as]
    (let [r (apply (first as) (rest as))]
      (if (fn? r) (recur [r]) r))))

(defcheck solution-e99c5d30
  (fn rec
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (rec #(apply f args)))))

(defcheck solution-e9fd7494
  #((fn t [%]
      (if (fn? %)
        (t (%))
        %))
    (% %2)))

(defcheck solution-eaf5bd19
  (fn supertramp [f & ps]
    (let [s (fn [f] (if (fn? f) (recur (f)) f))]
      (s (apply f ps))
      )))

(defcheck solution-eb22c256
  (fn t [f & args]
    (loop [g (apply f args)]
      (if (ifn? g) (recur (g)) g))))

(defcheck solution-eb82a5df
  (fn trampoline12 [f v] (loop [r (f v)]
                           (if (fn? r) (recur (r)) r
                                       ))))

(defcheck solution-eb9bd25
  (fn [f & args]
    (if (fn? f) (recur (apply f args) nil) f)))

(defcheck solution-eba3fc31
  (fn my-trampoline [f & args]
    (let [f (apply f args)]
      (loop [f f]
        (if (fn? f)
          (recur (f))
          f)))))

(defcheck solution-ec0001a9
  (fn my-trampoline [f & params]
    (let [g (apply f params)]
      (if (fn? g)
        (recur g nil)
        g))))

(defcheck solution-ec6e0970
  (fn [f & args]
    (if (fn? f)
      (recur (apply f args) [])
      f)))

(defcheck solution-ec8164a
  (fn [f & xs]
    (let [g (apply f xs)]
      (if (fn? g) (recur g []) g))))

(defcheck solution-ec932a66
  (fn [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (apply r []))
        r))))

(defcheck solution-eca0fee9
  (fn myTrampoline [f & args]
    (loop [currVal (apply f args)]
      (if (not (fn? currVal))
        currVal
        (recur (currVal))))))

(defcheck solution-ecdae5a0
  (fn [f & args]
    (if (fn? f)
      (recur (apply f args) nil)
      f
      )
    ))

(defcheck solution-ecec2710
  (fn [f & args]
    (loop [f2 (apply f args)]
      (if (fn? f2)
        (recur (f2))
        f2))))

(defcheck solution-ed507e96
  (fn t [f & args] (if (fn? f) (t (apply f args)) f)))

(defcheck solution-ee29d778
  (fn [f & args]
    (loop [val (apply f args)]
      (if (fn? val)
        (recur (val))
        val))))

(defcheck solution-ee4462fa
  (fn tram
    ([f]
     (let [ans (f)]
       (if (fn? ans)
         (recur ans)
         ans)))
    ([f & args]
     (tram #(apply f args)))))

(defcheck solution-ee9161a0
  (fn t [f & x]
    (if (fn? f)
      (t (apply f x))
      f)))

(defcheck solution-eea09fee
  (fn [f & args]
    (loop [r (apply f args)]
      (if (fn? r)
        (recur (r))
        r))))

(defcheck solution-eeba3ad1
  (fn [f & s]
    (let [result (apply f s)]
      (if (fn? result)
        (recur result ())
        result))))

(defcheck solution-ef3841c9
  (fn g
    ([f & s] (g (apply f s)))
    ([f] (if (not (fn? f)) f (recur (f))))))

(defcheck solution-ef45eb2f
  (fn [f & args]
    (loop [result (apply f args)]
      (if (not (fn? result))
        result
        (recur (result))))))

(defcheck solution-ef689de1
  (fn t
    ([f]
     (let [r (f)]
       (if (fn? r)
         (recur r)
         r)))
    ([f & args]
     (t #(apply f args)))))

(defcheck solution-ef9f064a
  (fn trampoline-
    ^{:doc "78. Reimplement the function described in 'Intro to Trampoline'."}
    ([f & xs] (trampoline- (apply f xs)))
    ([f] (if (ifn? f) (recur (f)) f))))

(defcheck solution-ef9fc945
  (fn __
    [f & args]
    (loop [applied (apply f args)]
      (if (fn? applied)
        (recur (applied))
        applied))))

(defcheck solution-efa8c14f
  (fn my-trampoline [f & args]
    (let [my-result (apply f args)]
      (if (fn? my-result)
        (my-trampoline my-result)
        my-result))))

(defcheck solution-eff02598
  (fn [f & vs]
    (loop [res (apply f vs)]
      (if (not (fn? res))
        res
        (recur (res))))))

(defcheck solution-f01c63ec
  (fn newtrampoline [f & x]
    (if (not (fn? (apply f x)))
      (apply f x)
      (newtrampoline (apply f x)))))

(defcheck solution-f026ed8a
  (fn tramp
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (tramp #(apply f args)))))

(defcheck solution-f04fff0d
  (fn t [f & args]
    (if (fn? f) (t (apply f args))
                f)))

(defcheck solution-f06511a5
  (fn tramp [f x]
    (loop [ret (f x)]
      (if (fn? ret) (recur (ret)) ret))))

(defcheck solution-f0841415
  (fn my-trampoline [f & params]
    (let [mid-result (apply f params)]
      (if (fn? mid-result)
        (apply my-trampoline mid-result [])
        mid-result))))

(defcheck solution-f10756fb
  (fn tramp [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f))
        f))))

(defcheck solution-f1fa926f
  ; That's pretty cool!

  (fn [f & args]
    (loop [f (apply f args)]
      (if (fn? f)
        (recur (f)) f))))

(defcheck solution-f2020872
  (fn _trp [f & args]
    (if (fn? f)
      (_trp (apply f args))
      f)))

(defcheck solution-f2046d72
  (fn [f & args]
    (let [ret (apply f args)]
      (loop [r ret]
        (if (fn? r)
          (recur (r))
          r)))
    ))

(defcheck solution-f247b4d9
  #(->> (%1 %2)
     (iterate (fn [f] (f)))
     (drop-while fn?)
     (first)))

(defcheck solution-f27b56e4
  (fn ft
    ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
    ([f & args]
     (ft #(apply f args)))))

(defcheck solution-f2f3298f
  (fn tramp [f & args]
    (let [r (if (empty? args) (f) (apply f args))]
      (if (fn? r) (recur r nil) r))))

(defcheck solution-f37ec948
  (fn [f & a] (let [r (apply f a)] (if (fn? r) (recur r nil) r))))

(defcheck solution-f5037362
  (fn [fx & args]
    (loop [r (apply fx args)]
      (cond (not (fn? r)) r
            :else (recur (apply r nil))))))

(defcheck solution-f531cac0
  (fn [f & args]
    (loop [res (apply f args)]
      (if (fn? res)
        (recur (res))
        res))))

(defcheck solution-f544aeef
  (fn f [x & a]
    (if (fn? x)
      (f (apply x a))
      x)))

(defcheck solution-f5b07d0a
  (fn [f & args]
    (loop [ret (apply f args)]
      (if (fn? ret)
        (recur (ret))
        ret))))

(defcheck solution-f628d293
  #(if (fn? %)
     (recur (apply % %&) [])
     %))

(defcheck solution-f63829c0
  (fn [f & argscoll]
    (loop [res (apply f argscoll)]
      (if (fn? res)
        (recur (res))
        res)
      )
    ))

(defcheck solution-f658e67d
  (fn __ [f & args]
    (loop [fun #(apply f args)]
      (let [res (fun)]
        (if (fn? res) (recur res) res)))))

(defcheck solution-f6c13dec
  (fn [f & a]
    (loop [f (apply f a)]
      (if-not (fn? f)
        f
        (recur (f))))))

(defcheck solution-f6dab398
  (fn bounce
    [f & args] {:pre [(fn? f)]}
    (loop [x (apply f args)]
      (if (fn? x)
        (recur (x))
        x))))

(defcheck solution-f703b306
  (fn t ([f] (let [v (f)] (if (fn? v) (recur v) v))) ([f & args] (t #(apply f args)))))

(defcheck solution-f710eb41
  (fn [f & args]
    (loop [result (apply f args)]
      (if-not (fn? result) result (recur (result))))))

(defcheck solution-f72761b9
  (fn aa [x & args] (if (fn? x) (aa (apply x args)) x)))

(defcheck solution-f868b243
  (fn rec
    ([f v] (rec (f v)))
    ([f] (if (fn? f) (rec (f)) f))))

(defcheck solution-f876d198
  (fn tramp [f x]
    (loop [o (f x)]
      (if (fn? o)
        (recur (o))
        o))))

(defcheck solution-f88d5258
  (fn my-trampoline
    ([f]
     (if (fn? f) (recur (f)) f))
    ([f & args]
     (my-trampoline (apply f args)))))

(defcheck solution-f8d5eab7
  (fn [f x] (let [ff (fn [x] (if (fn? x) (recur (x)) x))]
              (ff (f x)))))

(defcheck solution-f9bff042
  (fn f ([g] (let [t (g)] (if (fn? t) (recur t) t))) ([g & a] (f #(apply g a)))))

(defcheck solution-f9d84d30
  (fn tramp [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (recur r [])
        r))))

(defcheck solution-fa445932
  (fn tramp [f & args]
    (let [r (apply f args)]
      (if (fn? r)
        (tramp r)
        r))))

(defcheck solution-fafd5947
  (fn tram [f & args]
    (if (fn? f)
      (tram (apply f args))
      f)))

(defcheck solution-fb4e1826
  (fn tr
    [f & args]
    (let [x (apply f args)]
      (if (fn? x)
        (recur x ())
        x))))

(defcheck solution-fba11275
  (fn tramp [f & args]
    (loop [res (apply f args)]
      (if (fn? res) (recur (res)) res))))

(defcheck solution-fc00a2e4
  (fn t [a & b] (if (fn? a) (t (apply a b)) a)))

(defcheck solution-fc2da8bc
  (fn [f & args]
    (let [f-result (apply f args)]
      (if (fn? f-result)
        (recur f-result nil)
        f-result))))

(defcheck solution-fc64cc61
  (fn t [f & args]
    (let [r (if (nil? args) (f) (apply f args))]
      (if (fn? r) (t r) r))))

(defcheck solution-fd0470e5
  (fn t [f & args]
    (loop [v (apply f args)]
      (if (fn? v) (recur (v)) v))))

(defcheck solution-fd26b780
  (fn trampoline'
    ([f]
     (if-not (fn? f)
       f
       (recur (f))))
    ([f & args] (trampoline' (apply f args)))))

(defcheck solution-fe260746
  (fn [f & args]
    (loop [val (apply f args)]
      (if-not (fn? val)
        val
        (recur (val))))))

(defcheck solution-fec7b434
  (fn my-trampoline
    ([func & params] (my-trampoline #(apply func params)))
    ([func]
     (let [result (func)]
       (if (fn? result)
         (recur result)
         result)))))

(defcheck solution-ffd070c
  (fn [f & xs]
    (loop [g (apply f xs)]
      (if (fn? g)
        (recur (g))
        g))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-78))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
