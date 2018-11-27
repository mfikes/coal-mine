(ns coal-mine.problem-58
  (:require [coal-mine.checks :refer [defcheck-58] :rename {defcheck-58 defcheck}]
            [clojure.test]))

(defcheck solution-1085faf9
  (fn [& fs]
    (fn [& args]
      (reduce
        (fn [v f] (f v))
        (apply (last fs) args)
        (reverse (butlast fs))))))

(defcheck solution-10ce49e3
  (fn comb [& funcs]
    (fn [& args]
      (first
        (reduce #(vector (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-1111ee35
  (fn [& fs]
    (fn [& a]
      (loop [o (apply (last fs) a) f (butlast fs)]
        (if f
          (recur ((last f) o) (butlast f))
          o)))))

(defcheck solution-114457b7
  (fn reduceKunmetiF [& z] (reduce (fn [x  y] (fn [ & argj]   (x (apply y argj)) )) z)))

(defcheck solution-11502125
  (fn [& fns]
    (fn [& x]
      (let [[ff & rstf] (reverse fns)]
        (reduce (fn [arg f] (f arg))
          (apply ff x)
          rstf)))))

(defcheck solution-11974da6
  (fn rec
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g & fs]
     (reduce rec (list* f g fs)))))

(defcheck solution-11dbdf9b
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (let [rslt (apply (first fs) args)]
          (letfn [(apply-all [fs rslt]
                    (if-let [f (first fs)]
                      (apply-all (rest fs) (f rslt))
                      rslt))]
            (apply-all (rest fs) rslt)))))))

(defcheck solution-11df41ad
  (fn my-comp [& fs] (fn [& args] (reduce (fn [acc f] (f acc)) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-126d0119
  (fn [& f]
    (fn [& args]
      (let [t (reverse f)]
        (reduce #(%2 %1) (apply (first t) args) (rest t))
        )
      )
    ))

(defcheck solution-12f611bc
  #(fn [& a]
     (loop [r (apply (last %&) a)
            c (reverse (butlast %&))]
       (if (empty? c)
         r
         (recur ((first c) r) (rest c))))))

(defcheck solution-12fc7500
  (fn [& f] (fn[& z](reduce (fn[a b](b a)) (apply (last f) z) (rest (reverse f))))))

(defcheck solution-13f316a5
  (fn c [f & fns]
    (if fns
      #(f (apply (apply c fns) %&))
      #(apply f %&))))

(defcheck solution-14076119
  (fn [& funcs]
    (fn [& args]
      (letfn [(apply-funcs [[h & t] coll]
                (if (empty? t)
                  (apply h coll)
                  (h (apply-funcs t coll))))]
        (apply-funcs funcs args))
      )
    ))

(defcheck solution-14086947
  (fn [& f]
    (reduce #(fn [& a] (%1 (apply %2 a))) f)))

(defcheck solution-143270a2
  (fn [& funcs]
    (reduce (fn [f1 f2]
              (fn [& args] (f1 (apply f2 args)))
              ) identity funcs)
    ))

(defcheck solution-145f3788
  (fn compose-fns [f & fs]
    (if (empty? fs)
      f
      (fn [& args]
        (f (apply (apply compose-fns fs) args))))))

(defcheck solution-152ce859
  (fn comp-e [& x]
    (let [order (reverse x)]
      (fn inner [y & y-rst]
        (loop [result (apply (first order) y y-rst)
               r-order (rest order)]
          (if (empty? r-order) result
                               (recur ((first r-order) result) (rest r-order))))))))

(defcheck solution-155c3586
  (fn pmoc
    [& functions]
    (let [[f & fs] (reverse functions)]
      (fn [& args]
        (reduce #(%2 %)
          (apply f args)
          fs)))))

(defcheck solution-15899580
  (fn [f1 & others]
    (fn [& coll]
      (loop [f1 f1
             others others
             coll (if (coll? coll) coll (list coll))]
        (if (empty? others)
          (apply f1 coll)
          (recur f1
            (drop-last others)
            (let [new-coll (apply (last others) coll)]
              (list new-coll))
            ))))))

(defcheck solution-159852d4
  (fn
    ([& fs]
     (let [fs (reverse (list* fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-15e0aca2
  (fn f ([x y]
         (fn [v] (x (y v))))
    ([x y z]
     (fn [& v] (x (y (apply z v)))))))

(defcheck solution-161ee1ed
  (fn mc [& fns]
    (fn [& args]
      (let [inner (fn me [[hd & tl]]
                    (if tl
                      (hd (me tl))
                      (apply hd args)))]
        (inner fns)))))

(defcheck solution-16250098
  (fn comp--tail
    ([]  identity)
    ([& fns]
     (fn [& args]
       (loop [x (apply (peek (vec fns)) args), ; Apply the rightmost function
              stack (pop (vec fns))]           ; and assemble the rest as a stack.
         (if (seq stack)
           (recur ((peek stack) x) (pop stack)) ; Calling the top of the stack.
           x))))))

(defcheck solution-1641ec75
  (fn [& x] #(
               last (reduce (fn [v f] [(apply f v)]) %& (reverse x))
               )))

(defcheck solution-1661c615
  (fn [& s]
    (fn[& x]
      (reduce
        #(%2 %1)
        (apply (last s) x)
        (rest (reverse s))))))

(defcheck solution-167b58f
  (fn self [f & r]
    (if (empty? r)
      f
      (fn [& args]
        (f  (apply (apply self r) args))))))

(defcheck solution-16a3897b
  (fn composition
    ([x y]
     #(x (y %)))
    ([x y z]
     (fn ([a b c d]
          (x (y (z a b c d))))
       ([a b]
        (x (y (z a b))))))))

(defcheck solution-16bd035a
  (fn [& funcs]
    (letfn [(my-comp [func & funcs]
              (if (empty? funcs)
                func
                (let [rest-func (apply my-comp funcs)]
                  (fn [& args]
                    (func (apply rest-func args))))))]
      (apply my-comp funcs))))

(defcheck solution-16c5f6e7
  (fn my-comp
    [& funcs]
    (let [fs (reverse funcs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-170fdba1
  (fn [& f]
    (let [rf (reverse f)]
      (fn [& args]
        (loop [fns (next rf)
               values (apply (first rf) args)]
          (if (next fns)
            (recur (rest fns) ((first fns) values))
            ((first fns) values)))))))

(defcheck solution-1714378
  (fn
    [& fns]
    (fn [& init-paramters]
      (let [fns (reverse fns)
            result (apply (first fns) init-paramters)]
        (loop [fns (rest fns) result result]
          (if (empty? fns)
            result
            (recur (rest fns) ((first fns) result))))))))

(defcheck solution-173501c9
  (fn comp' [& args]
    #(first (reduce (fn [args f] (list (apply f args)))
              %&
              (reverse args)
              ))))

(defcheck solution-173b7b07
  (fn my-comp [& fs]
    (reduce
      (fn [acc, f]
        (fn [& args]
          (acc (apply f args))))
      fs)))

(defcheck solution-174ea644
  (fn c [& f]
    (if (= (count f) 1)
      (first f)
      (fn [& x]
        ((first f) (apply (apply c (rest f)) x))))))

(defcheck solution-1794bfdd
  (fn [& fns]
    (fn [& args]
      (loop [fs (reverse fns) a args]
        (if (empty? fs)
          (first a)
          (recur (rest fs) [(apply (first fs) a)]))))))

(defcheck solution-17d6610b
  (fn [& fs]
    (reduce #(fn [& x] (% (apply %2 x))) fs)))

(defcheck solution-18e2bdcc
  (fn [& f]
    (let [s #(%2 %)
          f (reverse f)]
      #(reduce s (apply (first f) %&) (rest f)))))

(defcheck solution-18fbca92
  (fn [& fns]
    (let [fns (reverse (list* fns))]
      (fn [& args]
        (loop [ret (apply (first fns) args)
               fns (next fns)]
          (if fns
            (recur ((first fns) ret) (next fns))
            ret))))))

(defcheck solution-19a9f964
  (fn [& fi]
    (fn [& ai]
      (loop [[f & ff] (reverse fi) a ai]
        (if f (recur ff [(apply f a)])
              (first a))))))

(defcheck solution-19ea87de
  (fn [& fns]
    (fn [& args]
      (let [rslt (atom (apply (last fns) args))
            fns (reverse (drop-last fns))]
        (doall (for [f fns]
                 (reset! rslt (f @rslt))))
        @rslt))))

(defcheck solution-1a5b797e
  (fn compose [f & fs]
    (if (empty? fs)
      (fn [& args]
        (apply f args))
      (fn [& args]
        (f (apply (apply compose fs) args))))))

(defcheck solution-1a6fd04
  (fn [& fs]
    (let [ rs (reverse fs) ]
      (fn [& as]
        (reduce #(%2 %1) (apply (first rs) as) (rest rs))))))

(defcheck solution-1b2165c2
  (fn [& fns]
    (fn [& arg]
      (first (reduce #(vector (apply %2 %1)) arg (reverse fns))))))

(defcheck solution-1b5993fb
  (fn comp_ [& fs]
    (fn [& args]
      (let [rfs (reverse fs)
            args (apply (first rfs) args)]
        (loop [result args
               f (next rfs)]
          (if f
            (recur ((first f) result)(next f))
            result))))))

(defcheck solution-1bb19630
  (fn [& fs]
    (fn [& args]
      (first (reduce (fn [ret f] [(apply f ret)]) args (reverse fs))))))

(defcheck solution-1c1e582a
  (fn cmp [& fs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-1c22d8a5
  (fn cmpo [f & fs]
    (fn [& a]
      (if (empty? fs) (apply f a)
                      (f (apply (apply cmpo fs) a))))))

(defcheck solution-1c85b58b
  (fn mcomp [& funcs]
    (fn [& a] (reduce #(%2 %1) (apply (last funcs) a) (reverse (butlast funcs))))))

(defcheck solution-1cf2317b
  (fn [& funs]
    (let [[f & fs] (reverse funs)]
      #(reduce (fn [x f] (f x)) (apply f %&) fs))))

(defcheck solution-1d1512b7
  (fn [& fs]
    (reduce
      (fn [f g]
        #(f (apply g %&))) fs)))

(defcheck solution-1d2ff69c
  (fn mycomp [& f]
    (fn [& x] (first (reduce #(list (apply %2 %1)) x (reverse f))))))

(defcheck solution-1df834ee
  (fn [& r]
    (reduce
      (fn [p q]
        #(p (apply q %&))) r)))

(defcheck solution-1e023b50
  (fn my-comp [& args]
    (let [rargs (reverse args)]
      (fn [& coll]
        (loop [fs (rest rargs) ar (apply (first rargs) coll)]
          (if (empty? fs)
            ar
            (recur (rest fs) ((first fs) ar))))))))

(defcheck solution-1e1871de
  (fn [& fns]
    (fn [& args]
      (first
        (reduce
          (fn [v f]
            [(apply f v)])
          args
          (reverse fns))))))

(defcheck solution-1e34499d
  (fn better-comp [& fs]
    (reduce (fn [f g]
              (fn [& args]
                (f (apply g args))))
      fs)))

(defcheck solution-1e3c7515
  (fn [& fs]
    (fn [& args]
      (loop [fs (reverse fs)
             accum args]
        (if (empty? fs)
          (first accum)
          (recur (rest fs)
            [(apply (first fs) accum)]))))))

(defcheck solution-1ead1218
  (fn[& f]
    (fn[& v]
      (reduce
        #(%2 %)
        (apply (last f) v)
        (rest (reverse f))))))

(defcheck solution-1eaec2e3
  (fn funCompX [& fs] (partial (fn funCompP [fs & x] ((fn funCompRec [fs x]
                                                        (if (nil? (second fs))
                                                          (apply (first fs) x)
                                                          ((first fs) (funCompRec (rest fs) x))
                                                          )
                                                        )
                                                      fs (vec x))) (vec fs))))

(defcheck solution-1ee271b5
  (fn comp-
    ^{:doc "58. Write a function which allows you to create function compositions."}
    ([f] f)
    ([f & fs] (fn [& args] ((apply comp- f (butlast fs)) (apply (last fs) args))))))

(defcheck solution-1f085312
  (fn fc
    [& fs]
    (when (seq fs)
      (let [f (first fs)
            rf (apply fc (rest fs))]
        (if (nil? rf)
          (fn [& args] (apply f args))
          (fn [& args] (f (apply rf args))))))))

(defcheck solution-1f0f0a44
  (fn [& fncs]
    (fn [& val]
      (loop [fns (reverse fncs), v val]
        (if (empty? fns) (first v)
                         (recur (rest fns) [(apply (first fns) v)]))))))

(defcheck solution-1f0f6a96
  (fn [& fs]
    (fn [& args]
      (loop [result (apply (first (reverse fs)) args) fseq (next (reverse fs))]
        (if fseq (recur ((first fseq) result) (next fseq)) result)))))

(defcheck solution-1f129865
  (fn f
    ([a] a)
    ([a & r] (fn [& c] (a (apply (apply f r) c))))))

(defcheck solution-203833ec
  (fn [& fs]
    (fn [& va]
      (first (reduce #(list (apply %2 %1)) va (reverse fs))))))

(defcheck solution-21a7f6f
  (fn mycomp [& fns]
    (fn [& args]
      (loop [fns (reverse fns) args args]
        (if (empty? fns)
          (first args)
          (let [new-arg (vector (apply (first fns) args))]
            (recur (rest fns) new-arg)))))))

(defcheck solution-21fb5794
  (fn [& fncs]
    (fn [& args] (reduce #(%2 %1) (apply (first (reverse fncs)) args)  (rest (reverse fncs))))))

(defcheck solution-2222cf1e
  (fn mycomp [& funs]
    (fn [& args]
      (let [first-result (apply (last funs) args)]
        (reduce #(%2 %1) first-result (rest (reverse funs)))))))

(defcheck solution-226e4985
  (fn [& fs]
    (fn [& vs]
      (let [rv (reverse fs)
            rf (first rv)
            rfs (rest rv)
            ]
        (reduce #(%2 %1) (apply rf vs) rfs)))))

(defcheck solution-229bf604
  #(fn [& args]
     (let [[f1 & fs] (reverse %&)
           arg (apply f1 args)
           f (fn [arg f] (f arg))]
       (reduce f arg fs))))

(defcheck solution-22b0bab1
  (fn [& l]  (fn [& v]  (reduce   #(%2  %)    (apply (first (reverse l)) v )   (rest (reverse l)) )     )  ))

(defcheck solution-22b5a73b
  (fn [& fns]
    (fn [& x]
      (loop [f (reverse fns) r x]
        (let [r* (apply (first f) r)]
          (if (next f)
            (recur (rest f) (list r*))
            r*))))))

(defcheck solution-230f9e24
  (fn [& fs]
    (fn [& x]
      (loop [fs1 (reverse (butlast fs))
             r (apply (last fs) x)]
        (if (empty? fs1)
          r
          (recur (rest fs1) ((first fs1) r)))))))

(defcheck solution-231df24d
  (fn [& fs] (fn [& args] (reduce #(%2 %) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-2371c2f
  (fn compf [& fs]
    (let [fs (reverse fs)]
      ( fn [& args]
        (reduce #(%2 %1) (apply (first fs) args) (rest fs))))))

(defcheck solution-2374fd3a
  (fn [& fs]
    (let [lst (last fs)
          rst (drop 1 (reverse fs))]
      (fn [& args]
        (reduce #(%2 %1) (apply lst args) rst)))))

(defcheck solution-23d6d984
  (fn [& l] (fn [& y] (reduce (fn [a f] (if (= a y) (apply f a) (f a))) y (reverse l)))))

(defcheck solution-245bb75a
  (fn
    ([a b] (fn [& x] (a (apply b x))))
    ([a b c] (fn [& x] (a (b (apply c x)))))))

(defcheck solution-24856feb
  (fn top [& [arg1 :as fnls]]
    (let [lastfn (last fnls)]
      (fn [& [:as ls]] (if (nil? arg1)
                         (first ls)
                         ((apply top (drop-last 1 fnls)) (apply lastfn ls))))
      )))

(defcheck solution-24ea35c4
  (fn myc
    ([a] a)
    ([a & rst]
     (fn [ & args ]
       (a (apply (apply myc rst) args))))))

(defcheck solution-250466cc
  (fn function-comp
    [& functions]
    (reduce #(fn [& x] (%2 (apply %1 x))) (reverse functions))))

(defcheck solution-2560fe61
  (fn [& fs] (fn [& args] (first (reduce #(vector (apply %2 %1)) args (reverse fs))))))

(defcheck solution-2569ab29
  (fn [& fns] (fn [& args] (first (reduce (fn [a f] [(apply f a)]) args (reverse fns))))))

(defcheck solution-25f26a4d
  (fn [& fs] (fn [& args] (reduce #(%2 %1) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-260b6bcf
  (fn [& fs] (let
              [g  (last fs)
               gs (-> fs reverse rest)]
               (fn [& args] (reduce #(%2 %1) (apply g args) gs)))))

(defcheck solution-260de140
  (fn [& functions]
    (fn [& args]
      (first (reduce (fn [result f]
                       [(apply f result)])
               args
               (reverse functions))))))

(defcheck solution-26393fc5
  (fn my-comp [& ffs]
    (let [[f & fs] (reverse ffs)]
      (fn ret [& args]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-26cd8d07
  (fn my-comp [& fs] (reduce (fn [f g] (fn [& args] (f (apply g args)))) fs)))

(defcheck solution-272bc6c6
  (fn [& fs] (fn [& es]
               (first (reduce #(vector (apply %2 %)) es (reverse fs))))))

(defcheck solution-276fe87f
  (fn [& funcs]
    (fn [& args]
      (let [[f & fs] (reverse funcs)]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-27b7a875
  #(reduce (fn [x y] (fn [& r] (x (apply y r)))) %&))

(defcheck solution-27d0c340
  (fn f [& fs]
    (fn [& args]
      (reduce #(%2 %) (apply (last fs) args) (reverse (drop-last fs)))
      )
    ))

(defcheck solution-27d4ebd7
  (fn comps
    ([func] (fn [& args] (apply func args)))
    ([func & others] (fn [& args] (func (apply (apply comps others) args))))
    ))

(defcheck solution-2801a8f5
  (fn [& fs]
    (reduce #(fn [& as] (%1 (apply %2 as))) fs)))

(defcheck solution-2842ceb
  (fn [& fs]
    (fn [& xs]
      (reduce #(%2 %) (apply (last fs) xs) (reverse (butlast fs))))))

(defcheck solution-284530be
  (fn [& fns]
    (reduce (fn [composed f] (fn [& x] (f (apply composed x))))

      (reverse fns))))

(defcheck solution-284d7822
  (fn ([& fns]
       (let [fs (reverse fns)]
         (fn [& args]
           (loop [ret (apply (first fs) args) fs (next fs)]
             (if fs
               (recur ((first fs) ret) (next fs))
               ret)))))))

(defcheck solution-2858ada8
  (fn [& funs]
    (fn [& args]
      (loop [result  (apply (first (reverse funs)) args)
             f (next (reverse funs))]
        #_(println result)
        #(println f)
        (if (empty? f)
          result
          (recur ((first f) result)(rest f) ))))))

(defcheck solution-290892a0
  (fn compo [& fs]
    (fn [& params]
      (let [funs (reverse fs)
            res (apply (first funs) params)]
        (loop [fns (rest funs) r res]
          (if (seq fns)
            (recur (rest fns) ((first fns) r))
            r))))))

(defcheck solution-297ca2c
  (fn [& funs] (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse funs))))))

(defcheck solution-2988613
  (fn [& funs]
    (reduce
      (fn [result current]
        (fn [& args]
          (result (apply current args))))
      identity
      funs)))

(defcheck solution-2a052b89
  (fn [& funcs]
    (let [[f & after] (reverse funcs)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) after)))))

(defcheck solution-2a37a53b
  (fn com [& fns]
    (let [lfn (last fns)
          rfns (reverse (butlast fns))]
      (fn [& args]
        (reduce (fn [result f] (f result))
          (apply lfn args)
          rfns)))))

(defcheck solution-2ae9219a
  (fn [& args]
    (fn [& my-args]
      (loop [rest-args args
             ret my-args]
        (if (empty? rest-args)
          (first ret)
          (recur (drop-last rest-args) [(apply (last rest-args) ret)]))
        )
      )
    ))

(defcheck solution-2b2d4c8e
  (fn [& f]
    ( fn [& x]
      (first ( reduce #(list (apply %2 %1)) x  (reverse f))))))

(defcheck solution-2bb855d1
  (fn [& fs]
    (fn [& xs]
      (loop [vals xs
             funcs (reverse fs)]
        (if (empty? funcs)
          (first vals)
          (recur
            (list (apply (first funcs) vals))
            (next funcs)))))))

(defcheck solution-2c1703
  (fn [& funcs]
    (if (empty? funcs)
      identity
      (let [rfuncs (reverse funcs)]
        (fn [& args]
          (reduce #(%2 %)
            (apply (first rfuncs) args)
            (rest rfuncs)))))))

(defcheck solution-2c1c0cc2
  (fn [ & fs ]
    (let [ [f0 & fs'] (reverse fs)]
      (fn [ & xs ]
        (reduce (fn [x f] (f x)) (apply f0 xs) fs' )))))

(defcheck solution-2c31dbe6
  (fn [& fns]
    (fn [& args]
      (first (reduce (fn [acc curr-fn]
                       (vector (apply curr-fn acc))) args (reverse fns))))))

(defcheck solution-2c51e3f0
  (fn my-comp
    ([f g]
     (fn
       ([x] (f (g x)))
       ([x & more] (f (apply g x more)))))
    ([f g h]
     (fn
       ([x y] (f (g (h x y))))
       ([x y & more] (f (g (apply h x y more))))))))

(defcheck solution-2c5e64f9
  (fn [& functions]
    (let [[h & xs] (reverse functions)]
      (fn [& args]
        (loop [fns xs
               result (apply h args)]
          (if (seq fns)
            (recur (rest fns) ((first fns) result))
            result))))))

(defcheck solution-2c78790b
  (fn [& F] (reduce (fn [g h] (fn [& x] (g (apply h x)))) F)))

(defcheck solution-2c9ff07d
  #(reduce (fn [f1 f2] (fn [& xs] (f1 (apply f2 xs)))) %&))

(defcheck solution-2cec25ca
  (fn outer [f & fs]
    (if fs
      (fn [& x] (f (apply (apply outer fs) x)))
      (fn [& xs] (apply f xs)))))

(defcheck solution-2d4f0f1f
  (fn ccomp [& fs]
    (fn [& xs]
      (first (reduce
               (fn [vs f]
                 [(apply f vs)])
               xs (reverse fs))))))

(defcheck solution-2d66bcdc
  (fn [& fs]
    (reduce
      (fn [f now]
        #(f (apply now %&)))
      fs)))

(defcheck solution-2d6ea402
  (fn compose
    ([] identity)
    ([f] f)
    ([f & fs]
     #(-> compose
        (apply fs)
        (apply %&)
        f))))

(defcheck solution-2d76bef4
  (fn c ([f] f) ([f & fs] (fn [& a] (f (apply (apply c fs) a))))))

(defcheck solution-2ddf2832
  (fn [& funcs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last funcs) args) (rest (reverse funcs))))))

(defcheck solution-2df01234
  (fn [& fns]
    (reduce
      #(fn [& args] (%1 (apply %2 args)))
      identity
      fns)))

(defcheck solution-2e0a0a5b
  ;(fn comp'
  ;  ([f] (fn [& xs] (apply f xs)))
  ;  ([f & fs] (fn [& xs] (f (apply (apply comp' fs) xs)))))
  (fn comp' [& fs]
    (fn [& xs]
      (let [rfs (reverse fs)]
        (reduce #(%2 %1) (apply (first rfs) xs) (rest rfs))))))

(defcheck solution-2e3e88cc
  (fn [& fs]
    (fn [& args]
      (let [[f & fs] (reverse fs)]
        (reduce #(%2 %) (apply f args) fs)))))

(defcheck solution-2e43aa56
  (fn [& fs]
    (let [rfs (reverse fs)]

      (fn [& args] (reduce  #(%2 %1)  (apply (first rfs) args) (rest rfs))))))

(defcheck solution-2e83318c
  (fn [& funcs]
    (reduce (fn [f g] (fn [& args] (f (apply g args)))) funcs)
    ))

(defcheck solution-2e9effb4
  (fn [& fns]
    (reduce (fn [a b]
              #(a (apply b %&)))
      fns)))

(defcheck solution-2f1a4be6
  (fn prob58 [& funcs]
    (fn [& args]
      (first
        (reduce
          #(vector (apply %2 %1))
          args
          (reverse funcs)))
      )))

(defcheck solution-2f1e7e55
  (fn comp' ([f g] (fn ([a] (f (g a)))
                     ([a & args] (f (apply g a args)))))
    ([f g & fns] (fn ([a] (f ((apply comp' g fns) a)))
                   ([a & args] (f (apply (apply comp' g fns) a args)))))))

(defcheck solution-2fbcb2a4
  (fn mycomp [& fsq]
    (if
     (= 1 (count fsq))
      (fn [& args] (apply (first fsq) args))
      (fn [& args] ((first fsq) (apply (apply mycomp (rest fsq)) args))))))

(defcheck solution-2fc82513
  (fn [& fs] (reduce (fn [composite f] (fn [& args] (composite (apply f args)))) identity fs)))

(defcheck solution-2ff2e7b2
  (fn fn-comp [& fns]
    (let
     [my-apply (fn my-apply [fn-list & args]
                 (if (empty? fn-list) (first args)
                                      (let [first-fn (first fn-list)
                                            rest-fns (rest fn-list)
                                            next-arg (apply first-fn args)]
                                        (my-apply rest-fns next-arg))))
      rev-fns (reverse fns)]
      (fn [& args] (apply (partial my-apply rev-fns ) args)))))

(defcheck solution-3001d23f
  (fn ([f g] (fn [& a] (f (apply g a)))) ([f g h] (fn [& a] (f (g (apply h a)))))))

(defcheck solution-3014a824
  (fn [& functions]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse functions))))))

(defcheck solution-3026fa06
  (fn function-composition [& fs]
    (let [[h & t] (reverse fs)]

      (reduce

        (fn [fa f]
          (fn [& x] (f (apply fa x))))

        (fn [& x] (apply h x))

        t))))

(defcheck solution-3057596f
  (fn [& funcs]
    (fn [& args]
      (first
        (reduce (fn [a b] (vector (apply b a))) args (reverse funcs))))))

(defcheck solution-31197828
  (fn [& funcs]
    (fn [& args]
      (loop [res args fs funcs]
        (if (<= (count fs) 0)
          (first res)
          (recur [(apply (last fs) res)] (butlast fs))
          )
        ))))

(defcheck solution-3137f998
  (fn [& f]
    (fn [& x]
      (loop [f f x x]
        (if (empty? f)
          (first x)
          (recur (drop-last f) (list (apply (last f) x))))))))

(defcheck solution-3167f85e
  #(let [f (reverse %&)]
     (fn [& as]
       (loop [r (apply (first f) as)
              f (next f)]
         (if (not f)
           r
           (recur ((first f) r) (next f)))))))

(defcheck solution-31ce52de
  (fn [& fargs]
    (fn [& vargs]
      (reduce #(%2 %) (apply (last fargs) vargs) (rest (reverse fargs))))))

(defcheck solution-3208d40e
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& a]
        (loop [ret (apply (first fs) a), fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-32106a93
  (fn compose [f & args]
    (fn [&  all] (if (empty? args) (apply f all)
                                   (f (apply (apply compose args) all))))))

(defcheck solution-321e057b
  (fn [& funcs]
    (reduce #(fn [& args] (%1 (apply %2 args))) funcs)))

(defcheck solution-32331788
  (fn poser [f & fs]
    (fn [& args]
      (if fs
        (f (apply (apply poser fs) args))
        (apply f args)))))

(defcheck solution-32501df3
  (fn my-comp [& s]
    (reduce
      #(fn[& args](%2 (apply %1 args))) (reverse s))
    ))

(defcheck solution-328d6452
  (fn [& funcs]
    (fn [& args]
      (reduce (fn [r f] (f r))
        (apply (last funcs) args)
        (reverse (butlast funcs))))))

(defcheck solution-3299c695
  (fn cmp [& fs]
    (if (= (count fs) 1)
      (first fs)
      (fn [& args]
        ((first fs) (apply (apply cmp (rest fs)) args))))))

(defcheck solution-329a763e
  (fn [& ops]
    (fn [& x]
      (if (= (count x) 1)
        (reduce #(%2 %1) (first x) (reverse ops))
        (reduce #(%2 %1) (apply (last ops) x) (rest (reverse ops)))
        )
      )
    ))

(defcheck solution-32b363b0
  (fn [& fs] (fn [& args] (first (reduce #(vector (apply %2 %)) args (reverse fs))))))

(defcheck solution-32ee645e
  (fn foo [& funcs]
    (fn [& args]
      (loop [f (reverse funcs) r args]
        (if (empty? f) (first r)
                       (recur (rest f) (list (apply (first f) r))))))))

(defcheck solution-334382ce
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fns) args) (reverse (butlast fns))))))

(defcheck solution-33ebb0
  (fn [& funcs]
    (let [fs (reverse funcs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               next-fs (next fs)]
          (if next-fs
            (recur ((first next-fs) ret) (next next-fs))
            ret))))))

(defcheck solution-342135fa
  (fn [& fcn-list]
    (fn [& args] (first (reduce #(list (apply %2 %)) args (reverse fcn-list))))))

(defcheck solution-34405ede
  (fn comp2
    [& fns]
    (let [fs (reverse fns)]
      (fn [& args]
        (reduce (fn [agg next-fn] (next-fn agg)) (apply (first fs) args) (rest fs))))))

(defcheck solution-34526166
  (fn f [% & fs]
    (if (empty? fs)
      (fn [& x] (apply % x))
      (fn [& x] (% (apply (apply f fs) x))))))

(defcheck solution-34924aa3
  (fn [& fs]
    (reduce
      #(fn [& args]
         (%1 (apply %2 args)))
      fs)))

(defcheck solution-34a61339
  (fn comb [& funcs]
    (fn [& args]
      (first
        (reduce #(list (apply %2 %1)) args (reverse funcs ))))))

(defcheck solution-34a9062b
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [acc (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) acc) (next fs))
            acc))))))

(defcheck solution-34e6bbc6
  (fn [& fns] (fn [& x] (first (reduce  #(list (apply %2 %1)) x (reverse fns))))))

(defcheck solution-34ed34cd
  (fn [& functions]
    (fn [& args]
      (loop [funcs (rest (reverse functions))
             res (apply (first (reverse functions)) args)]
        (if-not (empty? funcs)
          (recur (rest funcs) ((first funcs) res))
          res)))))

(defcheck solution-352eeb99
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [v (apply (first fs) args) fs (rest fs)]
          (if (seq fs)
            (recur ((first fs) v) (rest fs))
            v))))))

(defcheck solution-368392d7
  (fn [& fs]
    (fn [& args]
      (loop [out (apply (last fs) args) fs (butlast fs)]
        (if fs
          (recur ((last fs) out) (butlast fs))
          out)))))

(defcheck solution-369fda46
  (fn [& fs]
    (reduce
      (fn [f g]
        (fn [& args]
          (f (apply g args))))
      identity fs)))

(defcheck solution-3770ccc2
  (fn bar [& all-fns]
    (fn [& args]
      (letfn [(baz [f & fns]
                (if (seq fns)
                  (f (apply baz fns))
                  (apply f args)))]
        (apply baz all-fns)))))

(defcheck solution-37719d8e
  (letfn [(mycomp [& funcs]
            (let [[f & fs] (reverse funcs)]
              (fn [& args]
                (reduce (fn [res f] (f res)) (apply f args) fs)
                )))]
    mycomp))

(defcheck solution-378b9235
  (fn my-comp [& coll](fn [& a]
                        (let [rev-coll (reverse coll) init (rest rev-coll) seed (apply (first rev-coll) a)]
                          (loop [init init seed seed]
                            (if (empty? init) seed
                                              (recur (rest init) ((first init) seed))))))))

(defcheck solution-37e8cddf
  (fn [& f]
    (fn [& x]
      ((fn post [[f & fs] x]
         (if fs
           (f (post fs x))
           (apply f x)))
       f x))))

(defcheck solution-37f684a7
  (fn [& fs]
    (fn [& args]
      (let [fs (reverse fs)
            acc (apply (first fs) args)]
        (reduce #(%2 %1) acc (rest fs))))))

(defcheck solution-3850fe0c
  (fn c [a f & g] (if g #(f (a (a c a g) %&)) f)) apply)

(defcheck solution-3865cd9b
  (fn my-comp
    ([] identity)
    ([f] f)
    ([f & fs]
     (fn [& args]
       (f (apply (apply my-comp fs) args))))))

(defcheck solution-38740942
  (fn cmp ([f] #(apply f %&)) ([f & t] #(f (apply (apply cmp t) %&)))))

(defcheck solution-38961ea7
  (fn my-comp [& fns]
    (fn [& args]
      (let [[ffn & rfn] (reverse fns)]
        (reduce #(%2 %) (apply ffn args) rfn)))))

(defcheck solution-39267a68
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %)
        (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-39362b20
  (fn comp1 [& fs]
    (fn [& args]
      (loop [res (apply (last fs) args)
             fs (drop-last fs)]
        #_(print res)
        (if (empty? fs)
          res
          (recur ((last fs) res) (drop-last fs))
          )
        )
      )
    ))

(defcheck solution-39c13410
  (fn [& f] (reduce (fn [f1 f2] #(f1 (apply f2 %&))) f)))

(defcheck solution-3a062cad
  (fn gfx [& fns]
    (reduce #(fn [& x] (%1 (apply %2 x))) fns)))

(defcheck solution-3a1ce4ac
  (fn
    [& fs]
    (fn [& args] (loop [args args
                        fs fs]
                   (if fs
                     (let [f (last fs)]
                       (recur [(apply f args)] (butlast fs)))
                     (first args))))))

(defcheck solution-3a742ee8
  (fn [& funcs]
    (fn [& args]
      (first (reduce (fn [v foo]
                       [(apply foo v)])
               args
               (reverse funcs))))))

(defcheck solution-3a78cd8
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fs) args) (next (reverse fs))))))

(defcheck solution-3ae4cece
  (fn c [h & t]
    (if (empty? t)
      h
      #(h (apply (apply c t) %&))
      )
    ))

(defcheck solution-3b49fbe2
  (fn [& fs] (let [fs (reverse fs)] (fn [& args]
                                      (loop [ret (apply (first fs) args) fs (next fs)]
                                        (if fs
                                          (recur ((first fs) ret) (next fs))
                                          ret))))))

(defcheck solution-3b7fc50
  (fn [& fs]
    (fn [& args]
      (first
        (reduce
          (fn [x f]
            [(apply f x)])
          args
          (reverse fs))))))

(defcheck solution-3b87a816
  (fn [& m] (fn [& s] (first (reduce (fn [r i] (list (apply i r))) s (reverse m))))))

(defcheck solution-3ba216c4
  (fn myComp2
    ([] identity)
    ([f] f)
    ([f & n] (partial (fn [f g & x] (f (apply g x))) f (apply myComp2 n)))))

(defcheck solution-3c239a3f
  (fn [& fs]
    (fn [& zs]
      (reduce #(%2 %1)
        (apply (last fs) zs)
        (reverse (drop-last fs))))))

(defcheck solution-3cb274f8
  (fn c ([] (fn [x] x))
    ([& r] #((apply c (butlast r)) (apply (last r) %&)))))

(defcheck solution-3cd7a995
  (fn cmprec [& fns]
    (fn [& args]
      (loop [rfns (reverse fns) args args]
        (if (empty? rfns)
          (first args)
          (recur (rest rfns) (list (apply (first rfns) args))))))))

(defcheck solution-3ce3c142
  (fn [& fs]
    (reduce (fn [f g]
              #(f (apply g %&))) fs)))

(defcheck solution-3d80b663
  (fn [& fns] (reduce (fn [a b] (fn [& args] (a (apply b args)))) fns)))

(defcheck solution-3d91f5c0
  (fn
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
    ([f1 f2 f3 & fs]
     (let [fs (reverse (list* f1 f2 f3 fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-3e79063d
  (fn [& fs]
    (let [[f1 & fs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %)
          (apply f1 args)
          fs)))))

(defcheck solution-3e887696
  (fn [& in]
    (let [[f & r] (reverse in)]
      (fn [& args]
        (loop [[f1 & fn] r
               ret (apply f args)]
          (if f1
            (recur fn (f1 ret))
            ret))))))

(defcheck solution-3f4699ec
  (fn [& fs]
    (reduce (fn [f g] #(f (apply g %&)))
      fs)))

(defcheck solution-3f482588
  (fn [& fns]
    (let [[f & fs] (reverse fns)]
      (fn [& args]
        (reduce #(%2 %) (do #_(println (apply f args)) (apply f args)) fs)))))

(defcheck solution-3f5947e6
  (fn [& fns]
    (let [rfns (reverse fns)]
      (fn rcp [& args]
        (loop [rf (rest rfns) o (apply (first rfns) args)]
          (if (empty? rf)
            o
            (recur (rest rf) ((first rf) o))))))))

(defcheck solution-404543c8
  (fn [& args]
    (let [reducef (fn [acc e]
                    (fn [& p]
                      (acc (apply e p))))]
      (reduce reducef args))))

(defcheck solution-408fd1af
  (fn aaa [& args]

    (fn [& x]

      (if (= (count x) 2)
        (reduce #(%2 %1) ((last args) (first x) (second x)) (reverse (butlast args)))



        (if (= (count x) 1)

          (reduce #(%2 %1) (flatten x) (reverse args))
          (reduce #(%2 %1) (apply (last args) x) (reverse (butlast args)))

          )


        )
      )

    ))

(defcheck solution-41010133
  (fn [& funcs]
    (fn [& ps]
      (let [param (if (sequential? (first ps)) (first ps) ps)]
        (if (sequential? (first ps))
          (reduce (fn [acc f] (f acc)) param (reverse funcs))
          (reduce (fn [acc f] (if (= param acc) (apply f acc) (f acc) )  ) param (reverse funcs)))))))

(defcheck solution-4160644f
  (fn combo [& fns] (fn [& args] (reduce #(%2 %1) (apply (last fns) args) (reverse (butlast fns))))))

(defcheck solution-41b3a06
  (fn [& f]
    (fn [& x]
      (if (= (count x) 1)
        (loop [result (first x) functions (reverse f)]
          (if functions
            (recur ((first functions) result) (next functions))
            result
            )
          )

        (loop [result (apply (first (reverse f)) x) functions (rest (reverse f))]
          (if functions
            (recur ((first functions) result) (next functions))
            result
            )
          )

        )
      )
    ))

(defcheck solution-41e4d7c4
  (fn komp
    ([] identity)
    ([& fs](if fs
             (let [[f & r] (reverse fs)]
               (fn [& args]
                 (reduce #(%2 %1) (apply f args) r)))))))

(defcheck solution-42f15dc2
  (fn my-comp [& fns]
    (if (= (count fns) 1)
      (first fns)
      (fn [& args] ((first fns) (apply (apply my-comp (rest fns)) args))))))

(defcheck solution-43484280
  (fn [& f]
    (fn [& a]
      (first
        (reduce #(vec [(apply %2 %1)]) a (reverse f))))))

(defcheck solution-43fc0d4b
  (fn [& f] (fn [& v] (first (reduce #(list (apply %2 %)) v (reverse f))))))

(defcheck solution-44560af8
  (fn [& fs]
    #(reduce (fn [v f] (f v)) (apply (last fs) %&) (rest (reverse fs)))
    ))

(defcheck solution-456e630d
  (fn my-comp [& [fst-fn & rst-fns]]
    (if (empty? rst-fns)
      fst-fn
      (fn [& args] (fst-fn (apply (apply my-comp rst-fns) args))))))

(defcheck solution-458a81b1
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fns) args) (reverse (butlast fns))))))

(defcheck solution-45cc4556
  (fn comp2 [& fns]
    (fn [& args] (reduce #(%2 %) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-46155267
  (fn fcmp [& fs]
    (if (= (count fs) 1)
      (first fs)
      (partial (fn [f & x]
                 ((first fs) (apply f x))
                 )
        (apply fcmp (rest fs))
        )
      )
    ))

(defcheck solution-46546877
  (fn [& fns]
    (fn [& args]
      (reduce
        #(%2 %1)
        (apply (last fns) args)
        (reverse (butlast fns))
        )
      )
    ))

(defcheck solution-46892078
  (fn mycomp [& fns]
    (fn [& args]
      (let [fnr (reverse fns)]
        (loop [ret (apply (first fnr) args) fs (next fnr)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret
            )))
      )))

(defcheck solution-46914471
  ;These folks are impressive. I did not come up with this yet...

  (fn comb [& funcs]
    (fn [& args]
      (first
        (reduce #(vector (apply %2 %1)) args (reverse funcs ))))))

(defcheck solution-46bfa9d9
  (fn runit [& fns]
    (reduce (fn [fn-col f]
              (fn [& args]
                (fn-col (apply f args))))
      fns)))

(defcheck solution-470a8a4f
  (fn my-comp
    [& fs]
    (if (empty? (rest fs))
      (first fs)
      (fn [& args] ((first fs) (apply (apply my-comp (rest fs)) args))))))

(defcheck solution-4720de85
  (fn [& funcs]
    (fn [& args]
      (loop [r (apply (last funcs) args) fs (drop-last funcs)]
        (if (empty? fs)
          r
          (recur ((last fs) r) (drop-last fs)))))))

(defcheck solution-476a2e9
  (fn [& funcs]
    (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-476a3c10
  (fn [ & funcs] (fn [& params] (first (reduce (fn [acc x] (list (apply x acc)) ) params (reverse funcs))) ) ))

(defcheck solution-47a2dff4
  (fn [& f]
    #((reduce (fn [v f]
                [(apply f v)])
        %&
        (reverse f))
      0)))

(defcheck solution-4856448f
  (fn mycomp ([f] f) ([f & fs] (fn [& args] (f (apply (apply mycomp fs) args))))))

(defcheck solution-4871e6c1
  (fn comp2 [& fns]
    (let [rfns (reverse fns)]
      (fn [& args]
        (loop
         [acc (apply (first rfns) args)
          rfns (next rfns)]
          #_(println acc rfns)
          (if (nil? rfns)
            acc
            (recur
              ((first rfns) acc)
              (next rfns))))))))

(defcheck solution-4879a83c
  (fn
    ([f g]
     #(f (apply g %&)))
    ([f g h]
     #(f (g (apply h %&))))))

(defcheck solution-48bb7a41
  (fn [& f] (fn [& args] (reduce #(%2 %) (apply (last f) args) (rest (reverse f))))))

(defcheck solution-4912454e
  (fn [& fs]
    (letfn [(cmp [fns args]
              (if (seq fns)
                (recur (rest fns) (list (apply (first fns) args)))
                (first args)))]
      (fn [& xs] (cmp (reverse fs) xs)))))

(defcheck solution-496ff0ae
  (fn [& args]
    (let [args (reverse args)]
      (fn [& a]
        (reduce #(%2 %1) (apply (first args) a) (rest args))))))

(defcheck solution-4978d98a
  (fn [& functions]
    (fn [& arguments]
      (first
        (reduce
          #(vector (apply %2 %1))
          arguments
          (reverse functions))))))

(defcheck solution-49fe123a
  (fn mycomp [& fns]
    (fn composed [& args]
      (let [fnlist (reverse fns)
            myf (fn myf [fs cumval]
                  (if (empty? fs) cumval
                                  (myf (drop 1 fs) ((first fs) cumval))))]
        (myf (drop 1 fnlist) (apply (first fnlist) args))))))

(defcheck solution-4a26caaf
  (fn my-comp [& fns]
    #(if (empty? fns)
       (first %&)
       ((apply my-comp (butlast fns))
        (apply (last fns) %&)))))

(defcheck solution-4a7db435
  (fn [& fn-list] (partial
                    (fn my-comp [funcs & args]
                      #_(println args)
                      (if (seq funcs)
                        (my-comp (drop 1 funcs) (apply (first funcs) args))
                        (first args))) (reverse fn-list))))

(defcheck solution-4aded526
  (fn [& fs]
    (fn [& args]
      (first (reduce (fn [args f] (list (apply f args)))
               args
               (reverse fs))))))

(defcheck solution-4afa8368
  (fn[& fns](
              fn[& args](
                          first(loop[i 0 cmp args](
                                                    if(= i (count fns)) cmp
                                                                        (recur (inc i) (

                                                                                         list(apply
                                                                                               (nth (reverse fns) i)
                                                                                               cmp
                                                                                               ))
                                                                          )
                                                                        ))
                          )
              )))

(defcheck solution-4aff4c48
  (fn comp2 [& fns]
    (if (= 1 (count fns))
      (first fns)
      (fn [& args]
        #_(println args)
        ((first fns) (apply (apply comp2 (rest fns)) args))))))

(defcheck solution-4c1dfbe9
  (fn [& fs]
    (fn [& args]
      (first (reduce (fn [args f]
                       (vector (apply f args))) ; or list
               args
               (reverse fs))))))

(defcheck solution-4c80a813
  (fn [& f]
    (fn [& args]
      (loop [tmpf (reverse f) ans args]
        (if (empty? tmpf)
          (first ans)
          (recur (rest tmpf) (list (apply (first tmpf) ans))))))))

(defcheck solution-4cb6ac8e
  (fn [& f]
    (fn [& a]
      (reduce
        #(%2 %1)
        (apply (last f) a)
        (rest (reverse f))))))

(defcheck solution-4cdac4f7
  (fn [& funcs] (fn [& s] (reduce #(%2 %1) (apply (last funcs) s) (reverse (butlast funcs))))))

(defcheck solution-4cebbcc3
  (fn cmp [& args]
    (fn [& nargs]
      (reduce
        (fn [a b ] ( b a))
        (apply (last args) nargs)
        (reverse (drop-last args))
        )
      )
    ))

(defcheck solution-4dbed4aa
  (fn comp2 [& fnsBackwards]
    (fn ret [& args]
      (let [fns (reverse fnsBackwards)
            reducer (fn [acc op2] (op2 acc))]
        (reduce reducer (apply (first fns) args) (rest fns))
        ))))

(defcheck solution-4dece555
  (fn [& fs]
    (fn [& xs] (first (reduce #(list (apply %2 %1)) xs (reverse fs))))))

(defcheck solution-4e496b8a
  (fn [& fs]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) (conj (reverse fs) args))))))

(defcheck solution-4e9f24b8
  (fn [& fs] (let [sf (reverse fs)] #(reduce (fn [r f] (f r)) (apply (first sf) %&) (rest sf)))))

(defcheck solution-4eb735a
  (fn [ & f]
    (fn [ & x]
      (reduce (fn [m f] (f m))
        (apply (last f) x) (-> f reverse rest)))))

(defcheck solution-4efada8d
  (fn [& fs]
    (reduce (fn [f g] #(f (apply g %&))) fs)))

(defcheck solution-4f7c6e7
  (fn [& fs]
    (let [[f & fs] (reverse fs)]
      (fn [& xs]
        (reduce (fn [acc f] (f acc)) (apply f xs) fs)))))

(defcheck solution-4f9398ea
  (fn compose [& functions]
    (let [inner-fn (last functions)
          outer-fns (butlast functions)]
      (if (empty? outer-fns)
        inner-fn
        (fn [& args]
          ((apply compose outer-fns)(apply inner-fn args)))))))

(defcheck solution-4fad320a
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (let [result (apply (first fs) args)]
          (reduce (fn [x f]
                    (f x)) result (rest fs)))))))

(defcheck solution-50172632
  (fn
    ([f g] (fn [& args] (f (apply g args))))
    ([f g h] (fn [& args] (f (g (apply h args)))))))

(defcheck solution-50cc1d39
  (fn [ & fns]
    (fn [ & args ]
      (loop [l (pop (vec fns)) v (apply (last fns) args)]
        (if (empty? l)
          v
          (recur
            (pop l)
            ((peek l) v)))))))

(defcheck solution-517aea04
  (fn [& funs]
    (fn [& args]
      (first (loop [inner_funs (reverse funs) inner_args args]
               (if (empty? inner_funs)
                 inner_args
                 (recur (rest inner_funs)
                   (list (apply (first inner_funs)
                           inner_args)))))))))

(defcheck solution-5187566c
  (fn [& fs]
    (fn [& params]
      (reduce #(%2 %1) (apply (last fs) params) (reverse (butlast fs))))))

(defcheck solution-51952ab4
  (fn [& fns]
    (fn [& args]
      (first
        (reduce
          (fn [res f] (list (apply f res)))
          args
          (reverse fns))))))

(defcheck solution-51a107f9
  (fn f1 [& fs]
    (fn f2 [& vals]
      (first
        (reduce (fn f3 [a f] [(apply f a)])
          vals
          (reverse fs))))))

(defcheck solution-51a1beb5
  (fn my-comp [& funcs]
    (fn [& args]
      (first (reduce #(list (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-51ce8380
  (fn com [f & fs]
    (if (empty? fs)
      f
      (fn [& args]
        (f (apply (apply com fs) args))))))

(defcheck solution-51ee6c3a
  (fn [& more]
    (loop [result nil i (dec (count more))]
      (if (= i -1)
        result
        (recur
          (if (nil? result)
            (fn [& args]
              (apply (nth more i) args)
              )
            (fn [& args] ((nth more i) (apply result args)) )
            )
          (dec i))
        )
      )
    ))

(defcheck solution-523c152b
  (fn [& fns]
    (fn [& args]
      (loop [x (apply (last fns) args)
             my-fns (rest (reverse fns))]
        (if (empty? my-fns)
          x
          (recur
            ((first my-fns) x)
            (rest my-fns)))))))

(defcheck solution-5260c1c2
  (fn [& fs]
    (fn [& x]
      (reduce #(%2 %1) (apply (last fs) x) (rest (reverse fs)))
      )
    ))

(defcheck solution-52725b4f
  (fn compose [& fs]
    (fn [& args]
      (let [[f & rs] fs]
        (if rs
          (f (apply (apply compose rs) args))
          (apply f args)
          )
        )
      )
    ))

(defcheck solution-52759e39
  (fn [& funs]
    (reduce (fn [f g]
              (fn [& args]
                (f (apply g args))))
      funs)))

(defcheck solution-52945943
  (fn my-comp
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g & fs]
     (reduce my-comp (list* f g fs)))))

(defcheck solution-52d6de0c
  (fn [& fs]
    (fn [& y] (reduce (fn [t v] (v t)) (apply (last fs) y) (rest (reverse fs))))))

(defcheck solution-52f8a3af
  (fn [& fs]
    (fn [a & others]
      (reduce #(%2 %) (if others
                        (apply (last fs) a others)
                        ((last fs) a)) (reverse (butlast fs))))))

(defcheck solution-5312e1d1
  #(
    (fn c [[f & r]]
      (fn [& x]
        (if (empty? r)
          (apply f x)
          (f (apply (c r) x)))))
    (vec %&)
    ))

(defcheck solution-532e19db
  (fn [& fns]
    (let [fns (reverse fns)]
      (fn [& args]
        (reduce #(%2 %) (apply (first fns) args) (rest fns))))))

(defcheck solution-533a95e4
  (fn g [f & r]
    (if (empty? r) f
                   #(f (apply (apply g r) %&)))))

(defcheck solution-5403b182
  (fn [& fns]
    (fn [& args]
      (loop [v (apply (last fns) args)
             fns (rest (reverse fns))]
        (if (seq fns)
          (recur ((first fns) v)
            (rest fns))
          v)))))

(defcheck solution-546bbbbe
  (fn
    ([a b] (fn [x] (a (b x))))
    ([a b c] (fn [& args] (a (b (apply c args)))))))

(defcheck solution-54b1f78b
  (fn [& fs]
    (fn [& args]
      (let [rfs (reverse fs)]
        (reduce #(%2 %) (apply (first rfs) args) (rest rfs))))))

(defcheck solution-55c26a61
  (fn co [& fl]
    (if (= (count fl) 1)
      (first fl)
      (fn [& args]
        ((first fl) ( apply (apply co (rest fl)) args))
        )
      )
    ))

(defcheck solution-55f24e69
  (fn  [& col]
    (fn [& params]
      (loop [flist (rest (reverse col))  newparams (apply (first (reverse col)) params) ]
        (if (empty? flist)
          newparams
          (recur (rest flist) ((first flist )  newparams) )
          )

        ))))

(defcheck solution-568f5bad
  (fn [& s] (reduce #(fn [& x] (% (apply %2 x))) s)))

(defcheck solution-56b1ae8a
  (fn
    ([f g] (fn [arg] (f (g arg))))
    ([f g h] (fn [& args] (f (g (apply h args)))))))

(defcheck solution-56bf75f4
  (fn [& fns]
    (let [fns (reverse fns)]
      (fn [& args]
        (loop [fns fns rv args]
          (if (empty? fns)
            (first rv)
            (recur (rest fns) (vector (apply (first fns) rv)))))))))

(defcheck solution-56cc6c03
  (fn ([f g] (fn [& args] (f (apply g args))))
    ([f g h] (fn [& args] (f (g (apply h args)))))
    ))

(defcheck solution-56e03da8
  (fn [& fns]
    (let [rev-fns (reverse fns)]
      (fn [& params] (reduce #(%2 %1)
                       (apply (first rev-fns) params)
                       (rest rev-fns))))))

(defcheck solution-56ee840f
  (fn compose
    [& fs]
    (let [rfs (into [] (reverse fs))]
      (fn [& data]
        (reduce #(%2 %) (apply (first rfs) data) (rest rfs))))))

(defcheck solution-57253a70
  (fn [& ft]
    (let [f (reverse ft)]
      (fn [& args]
        (loop [rt (apply (first f) args) f (next f)]
          (if f
            (recur ((first f) rt) (next f))
            rt))))))

(defcheck solution-5747a087
  (fn [& o]
    (let [[f & fs] (reverse o)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-575ee0e4
  (fn [& f]
    (let [[a & r] (reverse f)]
      (fn [& x]
        (reduce #(%2 %) (apply a x) r)))))

(defcheck solution-578fe4f
  (fn fr [& fargs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fargs) args) (rest (reverse fargs)))
      )))

(defcheck solution-578ff389
  (fn m [& fns]
    (fn [& args]
      (first (reduce #(vector (apply %2 %)) args (reverse fns))))))

(defcheck solution-57d9a318
  (fn cc [f & fs]
    (fn [& args]
      (if fs
        (f (apply (apply cc fs) args))
        (apply f args)))))

(defcheck solution-58ede9ce
  (fn my-comp [& funcs]
    (let [funcs (reverse funcs)
          fun   (first funcs)
          funs  (rest funcs)]
      (fn [& args]
        (reduce (fn [x f] (f x)) (apply fun args) funs)))))

(defcheck solution-593f9203
  (fn
    ([f g]
     (fn [& x] (f (apply g x))))
    ([f g h]
     (fn [& x] (f (g (apply h x)))))))

(defcheck solution-59e2b328
  (fn ccomp [f & fs]
    (let [fs (reverse (list* f fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-59ea97bb
  (fn [& f]
    (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse f))))))

(defcheck solution-5ab2aaf1
  (fn mycomp [& f]
    (fn [& x]
      (let [f1 (last f)
            rf (drop-last 1 f)
            fresult (apply f1 x)]
        (if (empty? rf)
          fresult
          ((apply mycomp rf) fresult))))))

(defcheck solution-5ad7c0f3
  (fn [& functions]
    (fn [& args]
      (first
        (reduce (fn [result function]
                  (list (apply function result)))
          args
          (reverse functions))))))

(defcheck solution-5ae953f2
  (fn comp-n
    ([f1] f1)
    ([f1 f2] (fn [& args] (f1 (apply f2 args))))
    ([f1 f2 & more] (apply comp-n (cons (comp-n f1 f2) more)))))

(defcheck solution-5b4b6891
  (fn [& os]
    (fn [& ps]
      (letfn
       [(myap [_os _ps]
          (if (empty? _os)
            _ps
            (myap (rest _os) ((first _os) _ps))
            ))]
        (myap (rest (reverse os)) (apply (last os) ps))
        ))))

(defcheck solution-5bdd1774
  (fn [& fns]
    (reduce (fn [f g]
              (fn [& args]
                (f (apply g args))))
      fns)))

(defcheck solution-5c3b4644
  (fn [& fns]
    (reduce #(fn [& x] (%1 (apply %2 x))) fns)))

(defcheck solution-5c56aaeb
  (fn my-comp
    ([f g]
     (fn
       ([& more] (f (apply g more)))))
    ([f g & more]
     (if (seq more)
       (my-comp f (apply my-comp g more))
       (my-comp f g)))))

(defcheck solution-5c653ce1
  (fn f1 [& fs] (reduce (fn f2 [c f] (fn f3 [& args] (c (apply f args)))) fs)))

(defcheck solution-5c6e649a
  (fn my-comp [f & fseq]
    (if (empty? fseq)
      f
      (fn [& x] (f (apply (apply my-comp fseq) x))))))

(defcheck solution-5cbabafe
  (fn [f & fs]
    (loop [f f
           fs fs]
      (if (empty? fs)
        f
        (recur (fn [& xs] (f (apply (first fs) xs))) (rest fs))))))

(defcheck solution-5d3e8f10
  (fn [& more]
    (fn [& a]
      (loop [fs (rest (reverse more))
             r (apply (first (reverse more)) a)]
        (if (nil? (first fs))
          r
          (recur (rest fs) ((first fs) r)))))))

(defcheck solution-5d4564ab
  (fn my-comp [func & more-funcs]
    (if (empty? more-funcs)
      func
      (fn [& xs]
        (func (apply (apply my-comp more-funcs) xs))))))

(defcheck solution-5d701f33
  (fn [& fs]
    (let [[f & gs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) gs)))))

(defcheck solution-5df5d383
  (fn [& fns]
    (fn [& args]
      (first
        (reduce
          #(list (apply %2 %1))
          args
          (reverse fns))))))

(defcheck solution-5e14f03
  (fn [& fx]
    (fn [& args]
      (first (reduce (fn [acc it]
                       (list (apply it acc)))
               args
               (reverse fx))))))

(defcheck solution-5e2e2f09
  (fn [& fns]
    (fn [& args]
      (let [[f & fns] (reverse fns) ]
        (reduce #(%2 %1) (apply f args) fns))
      )

    ))

(defcheck solution-5e8c230e
  (fn composition
    ([f] f)
    ([f g]
     (fn [& arguments] (f (apply g arguments))))
    ([f g & functions]
     (reduce composition (composition f g) functions))))

(defcheck solution-5ea26f67
  (fn c ([a b] (fn [& args] (a (apply b args))))
    ([a b & rest] (apply c (c a b) rest))))

(defcheck solution-5eb33a53
  (fn [& f]
    (let [[g & f] (reverse f)]
      (fn [& a]
        (reduce #(%2 %1) (apply g a) f)))))

(defcheck solution-5ebcc839
  (fn [& fs]
    (fn [& args] (first
                   (reduce #(vector (apply %2 %)) args (reverse fs))))))

(defcheck solution-60071a85
  (fn co [& fc]
    (fn r [& coll] (first
                     (reduce #(vector (apply %2 %)) coll (reverse fc))))))

(defcheck solution-6045ec58
  (fn [& fs] (fn [& args] (reduce #(%2 %1) (apply (last fs) args) (reverse (butlast fs))))))

(defcheck solution-604a6de4
  (fn my-comp [f & fs]
    (fn [& xs]
      (if (empty? fs)
        (apply f xs)
        (f (apply (apply my-comp fs) xs))))))

(defcheck solution-60917ef4
  (fn [& f]
    (fn [& v]
      (nth (reduce #(vector (apply %2 %1)) v (reverse f)) 0))))

(defcheck solution-609ee3cf
  (fn
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
    ([f1 f2 f3 & fs]
     (let [fs (reverse (list* f1 f2 f3 fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-60dbd7c7
  (fn [& funs]
    (fn [& initial-args]
      (loop [fs (reverse funs)
             args initial-args]
        (if (empty? fs)
          (first args)
          (recur (rest fs) [(apply (first fs) args)]))))))

(defcheck solution-617393c0
  (fn func-comp [& caller-funcs]
    (fn f [& x]
      (loop [funcs (reverse caller-funcs) results x]
        (if (empty? funcs)
          (first results)
          ;(apply (first funcs) results))))))
          (recur (rest funcs) (list (apply (first funcs) results))))))))

(defcheck solution-61894cfa
  (fn [& functions]
    (fn [& args]
      ((fn recursive [[f & fs] as]
         (if fs
           (f (recursive fs as))
           (apply f as)
           )
         ) functions args)
      )
    ))

(defcheck solution-61a56c0a
  (fn my-comp
    ([f] f)
    ([f g] (fn [& xs] (f (apply g xs))))
    ([f g & more] (my-comp f (apply my-comp g more)))))

(defcheck solution-61b448b2
  (fn mycomp [f & rfs]
    (fn [& args]
      (apply f (if (empty? rfs) args
                                (list  (apply (apply mycomp rfs) args)))))))

(defcheck solution-62252709
  (fn comp2 [& funs]
    (let [rfuns (reverse funs) ffun (first rfuns) ofuns (rest rfuns)]
      (fn [& args]
        (reduce
          (fn [res f] (f res))
          (apply ffun args)
          ofuns
          )))))

(defcheck solution-622d8b32
  (fn [& fns] (fn [& x] (reduce #(%2 %1) (apply (last fns) x) (reverse (drop-last fns)) ) ) ))

(defcheck solution-62a6b3c3
  (fn f [& func]
    (fn g [& param]
      (first (reduce #(conj [] (apply %2 %1)) param (reverse func))))))

(defcheck solution-636a9143
  (fn f
    [& fs]
    (fn [& x]
      (reduce #(%2 %1) (apply (last fs) x)
        (rest (reverse fs))))))

(defcheck solution-63779008
  (fn [& f]
    (fn [& args]
      (loop [ [fh & ft] (reverse f), arg args ]
        (if (nil? ft)
          (apply fh arg)
          (recur ft [(apply fh arg)]))))))

(defcheck solution-64549f84
  (fn func-comp [& args]
    (fn [& args-]
      (reduce #(if (= args- %1)
                 (apply %2 %1)
                 (%2 %1)) args- (reverse args)))))

(defcheck solution-6469899c
  (fn mycomp[a & b](if (not b) a (fn[& c](a (apply (apply mycomp b) c))))))

(defcheck solution-64bb0ecc
  (fn [& functions]
    (let [fs (reverse functions)]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-64c11ca4
  (fn compose [& fs]
    (if (empty? fs)
      identity
      (fn [& args]
        (let [[f & fs] fs]
          (if (empty? fs)
            (apply f args)
            (f (apply (apply compose fs) args))))))))

(defcheck solution-655c746c
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fs) args) (reverse (butlast fs))))))

(defcheck solution-655f959c
  (fn [& fns]
    (partial
      (fn myfn
        ([fs xs]
         (if (empty? fs)
           xs
           (let [tfn (first fs)
                 ps (myfn (rest fs) xs)]
             (tfn ps))))
        ([fs x & xs]
         (myfn (butlast fs) (apply (last fs) (list* x xs))))) fns )))

(defcheck solution-6645d881
  ;; Admitting defeat - had to look at comp's source.
  (fn
    ([f] f)
    ([f & fs]
     (fn [& args]
       (let [revfs (reverse (list* f fs))]
         (loop [fs (next revfs)
                ret (apply (first revfs) args)]
           (if (seq fs)
             (recur (next fs) ((first fs) ret))
             ret)))))))

(defcheck solution-66b9faeb
  (fn composition
    [& fns]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-66be1188
  (fn [ & ffs]
    (let [fs (reverse ffs)
          f (first fs)
          r (rest fs)]
      (fn [ & args]
        (loop [v (apply f args) rr r]
          (if (empty? rr) v
                          (recur ((first rr) v) (rest rr))))))))

(defcheck solution-66dff1d7
  (fn my-comp [& funcs]
    (let [fns (reverse funcs)]
      (fn [& args]
        (loop [result (apply (first fns) args) pending (next fns)]
          (if (nil? pending)
            result
            (recur ((first pending) result) (next pending))))))))

(defcheck solution-6785db55
  (fn cmp [& args]
    (partial
      (fn [fns & n]
        (if (empty? fns) (first n)
                         (recur (rest fns) [(apply (first fns) n)]))) (reverse args))))

(defcheck solution-67a3ecf4
  (fn [& fs]
    (fn [& a]
      (let [rf (reverse fs)
            [f & fs] rf]
        (reduce (fn [r f] (f r)) (apply f a) fs)))))

(defcheck solution-67c94110
  (fn [& fs]
    (let [[f & fs] (reverse fs)]
      (fn [& ps]
        (reduce #(%2 %) (apply f ps) fs)))))

(defcheck solution-67cf417b
  (fn [& F]
    (reduce #(fn [& g] (%1 (apply %2 g)))
      #(-> %)
      F)))

(defcheck solution-67dd53c7
  (fn [& funs]
    (fn [& args]
      (loop [fs (butlast funs) x (apply (last funs) args)]
        (if (empty? fs) x
                        (recur (butlast fs) ((last fs) x)))))))

(defcheck solution-68128586
  (fn [& f]
    (fn [& s]
      (first
        (reduce
          #(list (apply %2 %))
          s
          (reverse f))))))

(defcheck solution-682ae740
  (letfn [(revcomp [& args]
            (let [lastfn (last args)
                  firstfns (map (partial nth args) (range (dec (count args))))]
              (if (= 1 (count args))
                lastfn
                (fn [& x] ((apply revcomp firstfns) (apply lastfn x))))))]
    revcomp))

(defcheck solution-686b0f43
  (fn [& f] (reduce #(fn [& x] (%1 (apply %2 x))) f)))

(defcheck solution-690aaa86
  (fn
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f(g)))
       ([a](f( g a)))
       ([a & b ] ( f( apply g a b )))
       ))

    ([f g h]
     (fn
       ([] (f(g(h))))
       ([a] (f(g(h a))))
       ([a & b ] (f(g(apply h a b)))))
     )))

(defcheck solution-69594031
  (fn [& s]
    (reduce (fn [f g] #(f (apply g %&))) s)))

(defcheck solution-69cb0d9a
  (fn cmp [& fns]
    (loop [to-comp (butlast fns)
           comped (last fns)]
      (if (empty? to-comp)
        comped
        (recur (butlast to-comp) (fn [& args] ((last to-comp) (apply comped args))))))))

(defcheck solution-69db6003
  (fn [& fs]
    (fn [& args]
      (loop [answer (apply (first (reverse fs)) args)
             fs (next (reverse fs))]
        (if (nil? fs) answer
                      (recur ((first fs) answer) (next fs)))))))

(defcheck solution-6a15c2b8
  (fn [& f]
    (fn [& p]
      (reduce #(%2 %) (apply (last f) p) (-> f reverse rest)))))

(defcheck solution-6a2b9f26
  (fn [ & fs]
    (fn [ & args]
      (reduce #(%2 %) (apply (last fs) args) (drop 1 (reverse fs))))))

(defcheck solution-6ac8212d
  (fn [& xf]
    (reduce (fn [a f]
              (fn [& x]
                (f (apply a x)))) (reverse xf))))

(defcheck solution-6b0d9169
  (fn [& fs] (fn [& args] (first  (reduce #(list ( apply %2 %1)) args (reverse fs))) )  ))

(defcheck solution-6b5a1db
  (fn comp' [& fs]
    (fn [& args]
      (reduce (fn [acc f]
                (f acc))
        (apply (last fs) args)
        (rest (reverse fs))))))

(defcheck solution-6b5cb24e
  (fn compose [& fs]
    (reduce (fn [f g] (fn [& xs] (f (apply g xs)))) fs)))

(defcheck solution-6b891513
  (fn [& fns]
    (reduce #(fn [& args] (%1 (apply %2 args))) fns)))

(defcheck solution-6c340339
  (fn newcomp [& f]
    (if (= (count f) 1)
      (fn phi [& x]
        (apply (first f) x))
      (fn psi [& y]
        ((first f) (apply (apply newcomp (rest f)) y))))))

(defcheck solution-6c4e3026
  (fn comp* [& args] (
                       fn [& x] (first (reduce #(list (apply %2 %1)) x (reverse args)))
                       )))

(defcheck solution-6c53bdd8
  (fn [& fs] (fn [& x]
               (loop [ls (drop-last fs) t (apply (last fs) x)]
                 (if (empty? ls) t
                                 (recur (drop-last ls) ((last ls) t)))))))

(defcheck solution-6ca376f0
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %1)
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-6ca5f7ed
  (letfn [(apply-all [fs x] (if (empty? fs) x (apply-all (rest fs) ((first fs) x))))] (fn [& fs] (fn [& args] (apply-all (rest (reverse fs)) (apply (last fs) args))))))

(defcheck solution-6ccc0aec
  (fn [& fs ]
    (fn [& args]
      (loop [ret (apply (last fs) args)
             fs (butlast fs)]
        (if fs
          (recur ((last fs) ret) (butlast fs))
          ret)))))

(defcheck solution-6d576e
  (fn my-comp [& fs]
    (reduce (fn [f1 f2] (fn [& params] (f2 (apply f1 params)))) (reverse fs))))

(defcheck solution-6d8fb384
  (fn [& funcs](
                 fn [& args]
                 (let [funcs (reverse funcs)
                       x (apply (first funcs) args)]
                   (reduce (fn [orig func]
                             (func orig)) x (rest funcs))))))

(defcheck solution-6dfd25c2
  (fn cop [& ops]
    (fn [& args]
      (loop [ret (apply (last ops) args) opp (drop-last 1 ops)]
        (if (seq opp)
          (recur ((last opp) ret) (drop-last 1 opp))
          ret)))))

(defcheck solution-6e3154bc
  (fn [& F] (reduce (fn [f g] #(f (apply g %&))) F)))

(defcheck solution-6e3f6abc
  (fn c [a & r] (if r #(a (apply (apply c r) %&)) a)))

(defcheck solution-6e918871
  (fn my-comp [& fs]
    (if (= (count fs) 1)
      (first fs)
      (fn [& args] ((first fs) (apply (apply my-comp (rest fs)) args))))))

(defcheck solution-6ec1a232
  (fn [& fs]
    (fn [& args]
      (reduce (fn [r f]
                (f r))
        (apply (first (reverse fs)) args)
        (rest (reverse fs))))))

(defcheck solution-6efb13f6
  #(fn [& args]
     (reduce (fn [a f] (f a))
       (apply (last %&) args)
       (reverse (butlast %&)))))

(defcheck solution-6f7fd234
  (fn [& fs]
    (fn [& args]
      (first (reduce #(vector (apply %2 %)) args (reverse fs))))))

(defcheck solution-7009e188
  (fn my-comp
    [& fxns]
    (fn [& y] (reduce #(%2 %) (cons (apply (last fxns) y) (rest (reverse fxns)))))))

(defcheck solution-709d0ebd
  (fn [& f]
    (fn [& a]
      (reduce #(%2 %) (apply (last f) a) (rest (reverse f))))))

(defcheck solution-70c5707d
  (fn [& fns]
    (fn [& args]
      (first (reduce (fn [acc f] [(apply f acc)]) args (reverse fns))))))

(defcheck solution-70d7cd5f
  (fn cmp[& funcs]
    (fn [& args]
      (first
        (reduce (fn [a f] (list (apply f a))) args (reverse funcs))))))

(defcheck solution-70e0ab27
  (fn comp* [& fns]
    (fn [& args]
      (let [fns (reverse fns)
            initial (apply (first fns) args)]
        (loop [f (next fns)
               r initial]
          (if f
            (recur (next f) ((first f) r))
            r))))))

(defcheck solution-711b24e0
  (fn compose [& fs]
    (reduce (fn [acc-f f]
              (fn [& xs]
                (acc-f (apply f xs))))
      identity
      fs)))

(defcheck solution-713f7fed
  (fn f [& fs]
    (let [nfs (reverse fs)]
      (fn [& args]
        (loop [ans (apply (first nfs) args) nfs (next nfs)]
          (if nfs
            (recur ((first nfs) ans) (next nfs))
            ans))))))

(defcheck solution-7182a5ef
  (fn [& fns]
    (fn [& args]
      (let [[f & fns] (reverse fns)]
        (reduce #(%2 %1) (apply f args) fns)))))

(defcheck solution-71ae3c5d
  (fn da-compy [& fns]
    (if (= 1 (count fns)) (fn [& x] (apply (first fns) x))
                          (fn [& x]
                            ((first fns) (apply (apply da-compy (rest fns)) x))))))

(defcheck solution-71dcecc1
  (fn [& args]
    (fn [& x]
      (first (reduce #(list (apply %2 %1)) x (reverse args))))))

(defcheck solution-72b114c7
  (fn [& funs]
    (let [funs (reverse funs)]
      (fn [& args]
        (loop [fs (rest funs) res (apply (first funs) args)]
          (if (= (count fs) 1)
            ((first fs) res)
            (recur (rest fs) ((first fs) res))))))))

(defcheck solution-72e250f1
  (fn my-comp [& functions]
    (fn [& args]
      (reduce #(%2 %1) (apply (last functions) args)
        (reverse (butlast functions))))))

(defcheck solution-7363be6f
  (fn F [& l]
    (if-let [f (last l)]
      #((apply F (butlast l)) (apply f %&))
      identity)))

(defcheck solution-7379e09e
  (fn perso-comp [& fns]
    (fn [& args]
      (loop [remaining (butlast fns) ans (apply (last fns) args)]
        (if (empty? remaining)
          ans
          (recur (butlast remaining)  ((last remaining) ans)))))))

(defcheck solution-73e64063
  #(partial (fn [h & x]
              (if h
                (recur (butlast h)
                  (vector (apply (last h) x)))
                (last x)))
     %&))

(defcheck solution-73f761ed
  (fn [ & r ]
    (fn [& x]
      (get (reduce #(conj [] (apply %2 %1))  x (reverse r))0))))

(defcheck solution-74109c19
  (fn [& fs]
    (fn [& xs]
      (reduce #(%2 %1)
        (apply (last fs) xs)
        (reverse (drop-last fs))))))

(defcheck solution-74cfb99
  (fn func-comp [& funcs]
    (fn
      ([coll]
       (loop [rfuncs (reverse funcs) c1 coll]

         (if (seq rfuncs)
           (recur (rest rfuncs) ((first rfuncs) c1))
           c1)))
      ([x & args]
       (let [v (apply (last funcs) x args)]
         (loop [rfuncs (rest (reverse funcs)) v1 v]
           (if (seq rfuncs)
             (recur (rest rfuncs) ((first rfuncs) v1))
             v1)))))))

(defcheck solution-74e11fd6
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-765c539d
  (fn [& fcoll]
    (fn [& coll]
      (reduce #(%2 %1)
        (apply (last fcoll) coll)
        (reverse (drop-last fcoll))))))

(defcheck solution-769aa11a
  (fn [& fs]
    (reduce
      (fn [g f] (fn [& x] (g (apply f x))))
      fs)))

(defcheck solution-76b75e57
  (fn a
    ([f g] #(f (apply g %&)))
    ([f g h] #(f (g (apply h %&))))))

(defcheck solution-76db43c8
  (fn [& fs]
    (fn [& a]
      (loop [fs fs a a]
        (if (empty? fs) (first a)
                        (recur (butlast fs) [(apply (last fs) a)]))))))

(defcheck solution-76f3c9dd
  (fn [& fs]
    (reduce
      (fn [f g]
        (fn [& xs]
          (f (apply g xs))))
      fs)))

(defcheck solution-7740958
  (fn [& f] (reduce (fn [x y] #(x (apply y %&))) f)))

(defcheck solution-776243fe
  (fn [& fs]
    (fn [& args]
      (let [[f & more] (reverse fs)]
        (reduce (fn [x f] (f x))
          (apply f args)
          more)))))

(defcheck solution-7767d766
  (fn cmp [f & r]
    (if r
      (fn [& args]
        (f (apply (apply cmp r) args)))
      f)))

(defcheck solution-78764683
  (fn [& funcs]
    (fn [& args]
      (loop [fs (rest (reverse funcs)) out (apply (last funcs) args)]
        (if (empty? fs)
          out
          (recur (rest fs) ((first fs) out)))))))

(defcheck solution-787e8ffe
  #(fn [& v]
     (loop [f %& e v]
       (if (empty? f)
         (first e)
         (recur (butlast f)
           [(apply (last f) e)])))))

(defcheck solution-789b5a1e
  (fn [& fs]
    (reduce (fn [f g]
              #(f (apply g %&)))
      fs)))

(defcheck solution-78ddd4b2
  (fn custom-comp
    [& fns]
    fns
    (fn
      [& args]
      (let [first-result (apply (last fns) args)
            rest-fns (rest (reverse fns))]
        (reduce
          (fn [reduce-val fn-el]
            (fn-el reduce-val))
          first-result rest-fns)))))

(defcheck solution-79140a89
  (fn [& args1] (fn [& args2] (loop [fs (reverse args1) i args2] (if (empty? fs) (first i) (recur (rest fs) (list (apply (first fs) i))))))))

(defcheck solution-7954d545
  (fn [& fns]
    (fn [& args]
      (let [[f & rfns] (reverse fns)]
        (reduce (fn [acc i] (i acc)) (apply f args) rfns)))))

(defcheck solution-79dc342f
  (fn co [f & n]
    (if-not n
      f
      (fn [& p] (f (apply (apply co n) p))) )))

(defcheck solution-7a111ee5
  (fn [& f]
    (fn [& a]
      (first (reduce #(list (apply %2 %))
               a (reverse f))))))

(defcheck solution-7a45ba35
  (fn [& funs]
    (fn [& args]
      (let [f1 (last funs)
            fx (reverse (butlast funs))
            init (apply f1 args)]
        (reduce (fn [ret f] (f ret)) init fx)))))

(defcheck solution-7a57c38d
  (fn [& funcs]
    (fn [& args]
      ((fn this [in-funcs]
         (if (empty? (rest in-funcs))
           (apply (first in-funcs) args)
           ((first in-funcs) (this (rest in-funcs)))
           )
         )
       funcs)
      )
    ))

(defcheck solution-7aadfdc8
  (fn[& functions]
    (fn [& vals]
      (reduce
        (fn [_first _next]
          (if (nil? _first)
            (apply _next vals)
            (_next _first)
            )
          )
        nil
        (reverse functions))
      )
    ))

(defcheck solution-7b065655
  (fn [& fs]
    (fn [& args]
      (first
        (reduce
          #(list (apply %2 %1))
          args (reverse fs))))))

(defcheck solution-7b43c4b3
  (fn [ & fns ]
    (fn [ & args ]
      (reduce
        #(%2 %1)
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-7b63a65a
  (fn fc
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
    ([f1 f2 f3 & fs]
     (let [fs (reverse (list* f1 f2 f3 fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-7b6d3541
  (fn mycomp [& args1]
    (fn [& args2]
      (->> args1
        reverse
        (reduce #(list (apply %2 %1)) args2)
        first))))

(defcheck solution-7bf28198
  (fn [& f]
    (let [[f & ff] (reverse f)]
      (fn [& x]
        (reduce #(%2 %1) (apply f x) ff)))))

(defcheck solution-7cde24a9
  (fn [& args] (fn [& more] (first(reduce (fn [a f] (vector(apply f a))) more (reverse args))))))

(defcheck solution-7ce21600
  (fn c [f & fs]
    (fn [& xs]
      (if (nil? fs)
        (apply f xs)
        (f (apply (apply c (first fs) (rest fs)) xs))))))

(defcheck solution-7d7d956b
  (fn c
    ([& fs] (let [fs (reverse fs)]
              (fn [& args]
                (loop [ret (apply (first fs) args) fs (next fs)]
                  (if fs (recur ((first fs) ret) (next fs)) ret)))))))

(defcheck solution-7db9b3af
  (fn f
    ([h] h)
    ([h & g] #(h (apply (apply f g) %&)))))

(defcheck solution-7e0c1a6
  (fn [& fs]
    (let [sf (reverse fs)]
      (fn [& coll]
        (reduce
          #(%2 %1)
          (if (= 1 (count sf))
            ((first sf) coll)
            (apply (first sf) coll))
          (rest sf))))))

(defcheck solution-7e11e17c
  (fn [& fs]
    (fn [& args]
      (first (reduce #(list (apply %2 %1)) args (reverse fs))))))

(defcheck solution-7e966dbd
  (fn [& fns] (fn [& args] (first (reduce (fn [acc f] (list (apply f acc))) args (reverse fns))))))

(defcheck solution-7ebe3b2c
  (fn [& funs]
    (fn [& args]
      (let [rfuns (reverse funs)]
        (reduce #(%2 %)
          (apply (first rfuns) args)
          (rest rfuns))))))

(defcheck solution-7f23b9f6
  (fn [ & fs]
    (condp = (count fs)
      0 identity
      1 (first fs)
      (reduce (fn [f1 f2] (fn [& args] (f2 (apply f1 args)))) (reverse fs))
      )))

(defcheck solution-7f505f54
  (fn _
    ([f] f)
    ([f1 f2]
     (fn [& args]
       (f1 (apply f2 args))))
    ([f1 f2 & fs]
     (fn [& args]
       (f1 (f2 (apply (reduce _ fs) args)))))))

(defcheck solution-7fb0b48c
  (fn com [& fns] (fn[& args](reduce #(%2 %1) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-7fbed914
  (fn [& x]
    (fn [& y]
      (reduce #(%2 %) (apply (last x) y) (rest (reverse x))))))

(defcheck solution-80005dc8
  (fn __ [& args]
    (fn [& x]
      (let [fr (apply (last args) x)]
        (reduce (fn [g f] (f g)) fr (rest (reverse args)))))))

(defcheck solution-8044c124
  (fn [& f] (fn [& e] (first (reduce #(list (apply %2 %)) e (reverse f))))))

(defcheck solution-80a9f1e0
  (fn [& fs]
    (let [fns (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1)
          (apply (first fns) args)
          (rest fns))))))

(defcheck solution-8138766f
  (fn my-comp [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [[f & fs] fs, ret (apply f args)]
          (if (empty? fs)
            ret
            (recur fs ((first fs) ret))))))))

(defcheck solution-81671d67
  (fn [& fs]
    (fn [x & xs]
      (let [f (last fs)]
        (reduce #(%2 %1)
          (if (seq xs) (apply f x xs) (f x))
          (reverse (butlast fs)))))))

(defcheck solution-81684903
  (fn [& funcs]
    (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-818fe42f
  (fn [& r]
    (fn [& data]
      (let [[f & r] (reverse r)]
        (reduce
          #(%2 %1)
          (apply f data)
          r)))))

(defcheck solution-819da989
  (fn [& s]
    (fn [& a]
      (first
        (reduce #(list (apply %2 %1))  a (reverse s))))))

(defcheck solution-81b36f57
  (fn [& fns]
    (fn [& args]
      (first (reduce (fn [new-args f] (list (apply f new-args))) args (reverse fns))))))

(defcheck solution-8207d6aa
  (fn c [& fs]
    (if (> (count fs) 1)
      (fn [& vs] ((first fs) (apply (apply c (rest fs)) vs)))
      (fn [& vs] (apply (first fs) vs)))))

(defcheck solution-828f6262
  (fn myComp [& fnctns] (fn [& args] (reduce #(%2 %1) (apply (last fnctns) args) (rest(reverse fnctns))))))

(defcheck solution-833bbe09
  (fn [& f]
    (fn [& args]
      (loop [ret args
             funcs (reverse f)]
        (if (empty? funcs) (first ret)
                           (recur [(apply (first funcs) ret)] (rest funcs)))))))

(defcheck solution-833d7c67
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-838ed141
  (fn [& fns]
    (fn [& coll]
      (loop [funs (rest (reverse fns))
             xs (apply (first (reverse fns)) coll)]
        (if (empty? funs)
          xs
          (recur (rest funs)
            ((first funs) xs)))))))

(defcheck solution-83a80d0
  (fn [& fns]
    (fn [& args]
      (first (reduce
               #(list (apply %2 %))
               args (reverse fns))))))

(defcheck solution-83b7f253
  (fn [& fs]
    (fn [& c]
      (first (reduce #(list (apply %2 %1)) c (reverse fs))))))

(defcheck solution-83f6a353
  (fn
    ([f g] (fn [x] (f (g x))))
    ([f g h] (fn
               ([x] (f (g (h x))))
               ([a b] (f (g (h a b))))
               ([a b c d] (f (g (h a b c d))))
               )
     )))

(defcheck solution-84b4ea69
  (fn [& args] (fn [& x] ( reduce #(%2 %1) (apply (last args) x) (rest (reverse args)) ))))

(defcheck solution-85235e0b
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %)
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-852ac06a
  (fn my-comp [& fs]
    #_(println "fs" fs)
    (let [[head & tail] fs]
      (if (empty? tail)
        head
        (fn [& args] (head (apply (apply my-comp tail) args)))))))

(defcheck solution-8539a81d
  (fn ([f g] (fn
               [& args]
               (f (apply g args))))
    ([f g h] (fn
               [& args]
               (f (g (apply h args)))))))

(defcheck solution-8540ea28
  (fn [& m]
    (reduce #(fn [& x] (%1 (apply %2 x))) m)))

(defcheck solution-854d77a7
  (fn[& fs]
    (fn[& ic](loop [[f & r] (reverse fs) x (apply f ic)]
               (if (first r)
                 (recur r ((first r) x))
                 x)))))

(defcheck solution-85621149
  (fn [& fs] #(reduce (fn [v f] (f v))
                (apply (last fs) %&)
                (rest (reverse fs)))))

(defcheck solution-8587bc05
  (fn [& fs]
    (fn [& args] (reduce (fn [r f] (f r))
                   (apply (peek (vec fs)) args)
                   (reverse (pop (vec fs)))))))

(defcheck solution-85c27d12
  #(fn [& r] (reduce (fn [a f] (f a)) (apply (last %&) r) (rseq (vec (butlast %&))))))

(defcheck solution-86554b91
  (fn [& fns]
    (fn [& stuff]
      (loop [fns (reverse fns)
             result stuff]
        (if (empty? fns)
          (first result)
          (recur (rest fns) [(apply (first fns) result)]))))))

(defcheck solution-867bd39a
  (fn my-compose
    [& fns]
    (fn
      [& args]
      (reduce (fn [f1 f2] (f2 f1)) (apply (last fns) args) (reverse (butlast fns))))))

(defcheck solution-878316c2
  (fn [& fs]
    (reduce #(fn [& xs] (%2 (apply % xs)))
      (reverse fs))))

(defcheck solution-882154e
  (fn t
    ([f] f)
    ([f1 f2] #(f1 (f2 %)))
    ([f1 f2 f3]
     (fn [& a] (f1 (f2 (apply f3 a)))))))

(defcheck solution-88251679
  (fn cp [& fs]
    (if (= 2 (count fs)) (fn[& x] ((first fs) (apply (second fs) x)))
                         (fn [& x] ((first fs) (apply (apply cp (rest fs)) x))))))

(defcheck solution-885112a1
  (fn [& s] (reduce (fn [f g] #(f (apply g %&))) s)))

(defcheck solution-88aa43dc
  (fn mycomp ([f1 f2] (let [f (fn[x] (f1 (f2 x)))] f)) ([f1 f2 f3] (let [f (fn[& args] (f1 (f2 (apply f3 args))))] f))))

(defcheck solution-88bb217e
  (fn x  [ & fs] (fn  [& args ]
                   (if (= 2 (count fs))
                     ((first fs)  (apply (last fs) args))
                     ((apply x (butlast fs))   (apply (last fs) args))
                     ))))

(defcheck solution-89484c0
  (fn c [& [f ff & fs]]
    (if (seq fs)
      (apply c (cons (fn [x](f (ff x))) fs))
      (fn [& xs] (f (apply ff xs))))))

(defcheck solution-89694748
  (fn x [& coll]
    (let [coll (reverse coll)]
      (fn [& x]
        (reduce #(%2 %1) (apply (first coll) x) (rest coll))))))

(defcheck solution-897cd124
  (fn [ & ops]
    (let [
          first-op (last ops)
          rest-ops (reverse (drop-last ops))
          ]

      (fn [& args]

        (reduce #(%2 %1) (apply first-op args) rest-ops)))
    ))

(defcheck solution-898f80f6
  (fn [& funcs] (fn [& args] (let [[f & fs] (reverse funcs)] (reduce (fn [x f] (f x)) (apply f args) fs)))))

(defcheck solution-89d4e185
  (fn bob ([f] (fn [& args] (apply f args)))
    ([f g] (fn [& args] (f (apply g args))))
    ([f g h] (fn [& args] (f (g (apply h args)))))))

(defcheck solution-8a05abba
  (fn [& funcs]
    (fn [& args]
      (first
        (reduce
          (fn [a b]
            (vector
              (apply b a)
              )
            )
          args
          (reverse funcs)
          )
        )
      )
    ))

(defcheck solution-8a255981
  (fn[& fs] (fn[& x] (reduce #(%2 %) (apply (last fs) x) (rest (reverse fs))))))

(defcheck solution-8a46b388
  (fn [& c] (reduce #(fn [& a] (% (apply %2 a))) c)))

(defcheck solution-8a5a12ac
  (fn comp_ [& fs]
    (fn [& args]
      (loop [acc (apply (last fs) args)
             fs_ (butlast fs)]
        (if (nil? fs_)
          acc
          (recur ((last fs_) acc) (butlast fs_)))))))

(defcheck solution-8af3d2ba
  (fn my-comp
    ([f] (fn[& args] (apply f args)))
    ([f g](fn[& args] (f (apply g args))))
    ([f g & more](fn [& args] (f (g (apply (apply my-comp more) args)))))
    ))

(defcheck solution-8b0d420
  (fn [& fs]
    (let [[f & fs'] (reverse fs)]
      (fn [& x]
        (let [x' (apply f x)]
          (loop [[f' & sf' :as sf] fs' x'' x']
            (if (empty? sf)
              x''
              (recur sf' (f' x'')))))))
    ))

(defcheck solution-8b21bd8f
  (fn co [& funs]
    (if (= 1 (count funs))
      (fn [& args]
        (apply (first funs) args))
      (fn [& args]
        ((apply co (butlast funs)) (apply (last funs) args))))))

(defcheck solution-8b372848
  (fn [& fs] (fn[& as] (first (reduce #(vector (apply %2 %)) as (reverse fs))))))

(defcheck solution-8b85280b
  ; Note that this only handles the arg cases it has to to pass the unit tests
  ; I think there's a "a b c" case I missed.
  (fn run-in-order
    ([x] x)
    ([x y]
     (fn
       ([arg] (x (y arg)))
       ([a b] (x (y a b)))
       ([a b c & args] (x (apply y a b c args)))))
    ([x y z]
     (fn
       ([arg] (x (y (z arg))))
       ([a b] (x (y (z a b))))
       ([a b c & args] (x (y (apply z a b c args))))))))

(defcheck solution-8b98ef79
  (fn c [& more]
    (fn r [& s]
      (reduce #(%2 %1) (apply (last more) s) (reverse (butlast more))))))

(defcheck solution-8bb192e9
  (fn [& z] (reduce (fn [x y] (fn [& u] (x (apply y u)))) z)))

(defcheck solution-8bd5723f
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %1)
        (apply (last fs) args)
        (reverse (butlast fs))))))

(defcheck solution-8c1e4fd3
  (fn my-comp
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
    ([f1 f2 f3 & fs]
     (let [fs (reverse (list* f1 f2 f3 fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-8ccd1468
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& a]
        (reduce #(%2 %) (apply (first fs) a) (rest fs))))))

(defcheck solution-8ccde758
  (fn my-compose [& fs]
    (fn [& args]
      (reduce (fn [acc f] (f acc)) (apply (last fs) args) (rest (reverse fs))))))

(defcheck solution-8d679079
  (fn cfns
    ([f1 f2] #(f1 (apply f2 %&)))
    ([f1 f2 f3] #(f1 (f2 (apply f3 %&))))
    ([f1 f2 f3 f4] #(f1 (f2 (f3 (apply f4 %&)))))))

(defcheck solution-8da53cb5
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& pars]
        (loop [p (apply (first fs) pars)
               fs (rest fs)]
          (if (empty? fs)
            p
            (recur ((first fs) p) (rest fs))))))))

(defcheck solution-8dc55a8e
  (fn [ & fargs ]
    (let [ [fo & rfs] (reverse fargs)]
      (fn [ & xs]
        (reduce #(%2 %1) (apply fo xs) rfs)))))

(defcheck solution-8dc5c97
  (fn comp' [& fs]
    (let [f0 (first fs)
          fs' (next fs)]
      (if (empty? fs') f0
                       (fn [& args]
                         (f0 (apply (apply comp' fs') args)))))))

(defcheck solution-8dcd233b
  (fn [& cf] (fn [& cc] (loop [f cf r cc]
                          (if (empty? f) (first r)
                                         (recur (drop-last f) [(apply (last f) r)])
                                         )))))

(defcheck solution-8dcdb7cc
  (fn [ & functions]
    (fn [ & args]
      (let [functions_reverse (reverse functions)]
        (reduce (fn a [intermediate fun] (fun intermediate)) (apply (first functions_reverse) args) (rest functions_reverse))))))

(defcheck solution-8ddd1aa9
  (fn g [f & fs] (if (seq? fs) (fn [& s] (f (apply (apply g fs) s))) f)))

(defcheck solution-8e17d21b
  (fn [& fs]
    (fn [& xs]
      (let [[f0 & fs-more] (reverse fs)]
        (reduce (fn [x f] (f x)) (apply f0 xs) fs-more)))))

(defcheck solution-8e56fe10
  (fn [& fs]
    (fn [& xs]
      (first (reduce #(list (apply %2 %1))
               xs
               (reverse fs))))))

(defcheck solution-8ebc86ff
  ;(fn comp2 [& fs]
  ;  (fn [& args]
  ;    (reduce #(apply %2 [%])
  ;            (apply (last fs) args)
  ;            (rest (reverse fs)))))
  (fn comp3 [& fs]
    (fn [& args]
      (let [[f & rf] (reverse fs)]
        (reduce #(%2 %) (apply f args) rf)))))

(defcheck solution-8ed45320
  (fn [& fs] (reduce #(fn [& args] (%1 (apply %2 args))) identity fs)))

(defcheck solution-8ed8da39
  (fn [& fs]
    (let [fs (reverse (list* fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret ))))))

(defcheck solution-8eff1b2c
  (fn mycomp [& args]
    (if (= 1 (count args))
      (first args)
      #((first args)
        (apply
          (apply mycomp (rest args)) %&)
        )
      )
    ))

(defcheck solution-8f486794
  (fn mycomp [& fargs]
    (fn [& args]
      (loop [fargs fargs
             result args]
        (if (empty? fargs)
          (apply identity result)
          (recur (butlast fargs) [(apply (last fargs) result)]))))))

(defcheck solution-8f628c08
  (fn [& fn-args]
    (let [fns (reverse fn-args)]
      (fn [& args]
        (loop [result (apply (first fns) args)
               fns (rest fns)]
          (if (empty? fns) result
                           (recur ((first fns) result) (rest fns))))))))

(defcheck solution-8f8afddf
  (fn func
    [& fs]
    (let [apply-and-log (fn [arg f]
                          (let [applied (f arg)]
                            #_(println applied)
                            applied))]

      (fn
        ([as-seq]
         (reduce #(%2 %1) as-seq (reverse fs)))
        ([arg1 & args]
         (let [a (cons arg1 args)
               functions (reverse fs)
               first-applied (apply (first functions) a)]
           (reduce apply-and-log first-applied (rest functions))))))))

(defcheck solution-8f96bf4f
  (fn f1 [& fns]
    (let [[f & fns] (reverse fns)]
      (fn f2 [& v]
        (loop [r (apply f v) [f & fns] fns]
          (if f
            (recur (f r) fns)
            r))))))

(defcheck solution-8fc4964f
  (fn ([f1 f2] #(f1 (f2 %)))
    ([f1 f2 f3] (fn [& args] (f1 (f2 (apply f3 args)))))))

(defcheck solution-8fdd4257
  (fn [& all-fns]
    (reduce
      (fn [acc-fn curr-fn]
        (fn [& args]
          (acc-fn (apply curr-fn args))))
      identity
      all-fns)))

(defcheck solution-8ff45311
  (fn [& fns]
    (reduce (fn [afn vfn] (fn [& x] (afn (apply vfn x))))
      identity fns)))

(defcheck solution-9027eb30
  (fn [& r] (let [c (fn [f g] #(g (apply f %&)))] (reduce c (reverse r)))))

(defcheck solution-9029c9df
  (fn[& a] (reduce #(fn[& b] (%2 (apply % b))) (reverse a))))

(defcheck solution-90362286
  (fn [& fs] (reduce (fn [f g] #(f (apply g %&))) fs)))

(defcheck solution-90a32a3c
  (fn my_comp([f g]
              (fn [& args]
                (f (apply g args))))
    ([f g & funs]
     (reduce my_comp (list* f g funs)))))

(defcheck solution-913df85b
  (fn [& funs]
    (fn [& p]
      (loop [fs (reverse funs) res p]
        (cond
          (empty? fs) (first res)
          :else (recur (rest fs) (list (apply (first fs) res))))))))

(defcheck solution-92a12a13
  (fn comp* [& fns]
    (let [comp2 (fn [f g]
                  (fn [& args]
                    (f (apply g args))))]
      (when (seq fns)
        (if (empty? (rest fns))
          (first fns)
          (reduce comp2 fns))))))

(defcheck solution-92f9e4e3
  (fn c2 [& a]
    (letfn [(af [fs]
              (fn [& b]
                (apply (first fs)
                  (if (empty? (rest fs))
                    b
                    [(apply (af (rest fs)) b)]
                    )
                  )
                )
              )]
      (fn [& b] (apply (af a) b))
      )
    ))

(defcheck solution-936bb5c1
  (fn [& f]
    (let [[h & r] (reverse f)]
      (fn [& z]
        (reduce
          #(%2 %)
          (apply h z)
          r)))))

(defcheck solution-937ae47e
  (fn [& fns]
    (let [[f & fns-left] (reverse fns)]
      (fn [& args]
        (let [first-result (apply f args)]
          (loop [fns fns-left result first-result]
            (if (seq fns)
              (recur (rest fns) ((first fns) result))
              result)))))))

(defcheck solution-9398dc96
  (fn [& fs]
    (reduce (fn [f g]
              #(f (apply g %&)))
      fs)))

(defcheck solution-93ed7624
  (fn [& fs]
    (fn [& args]
      (loop [fx (apply (last fs) args) fs (butlast fs)]
        (if fs
          (recur ((last fs) fx) (butlast fs))
          fx)))))

(defcheck solution-94011ec1
  (fn [& fl]
    (fn [& vl]
      (loop [f (drop-last fl) v (apply (last fl) vl)]
        (if (empty? f)
          v
          (recur (drop-last f) ((last f) v))
          )
        )
      )
    ))

(defcheck solution-94269adc
  (fn me
    [& ops]

    (let [returnf

          (fn returnFn [f1 f2]

            (fn [& args]
              (f1 (apply f2 args))
              )

            )
          ]

      (reduce returnf ops)
      )


    ))

(defcheck solution-9447838b
  (fn [& funcs]
    (reduce (fn [f1 f2]
              (fn [& args] (f1 (apply f2 args))))
      funcs)))

(defcheck solution-9453fa12
  (fn compose [& fs]
    (reduce  (fn [f e] (fn [& args] (e (apply f args)))) (reverse fs))))

(defcheck solution-9461b5a6
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [res (apply (first fs) args)
               fs (next fs)]
          (if-not (nil? fs)
            (recur ((first fs) res) (next fs))
            res))))))

(defcheck solution-94e720c
  (fn comp* [& fs]
    (let [f (first fs)
          r (rest fs)]
      (fn [& args]
        (if (seq r)
          (f (apply (apply comp* r) args))
          (apply f args))))))

(defcheck solution-950c096c
  (fn [& fs]
    (fn [& as]
      (reduce #(%2 %1)
        (apply (last fs) as)
        (rest (reverse fs))))))

(defcheck solution-95143a17
  (fn c [& fs]
    (if (= (count fs) 1)
      (first fs)
      (fn [& xs] ((apply c (butlast fs)) (apply (last fs) xs))))))

(defcheck solution-95a5d279
  (fn comp2 [& fs]
    (let [r (reverse fs)]
      (fn [& x]
        (reduce #(%2 %1) (apply (first r) x) (next r))))))

(defcheck solution-95a843d4
  (fn a [f & z]
    (if z
      #(f (apply (apply a z) %&))
      f)))

(defcheck solution-95b80326
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-95bf32ac
  (fn my-comp [fun & funs]
    (if funs
      (fn [& args]
        (fun (apply (apply my-comp funs) args)))
      (fn [& args]
        (apply fun args)))))

(defcheck solution-9616475e
  (fn [& functions]
    (letfn [(rcr [fns result]
              (if (seq fns)
                (rcr (next fns) ((first fns) result))
                result))]
      (fn [& args]
        (let [[f & fns] (reverse functions)]
          (rcr fns (apply f args)))))))

(defcheck solution-963235bd
  (fn comp-r-l [& args]
    (fn
      [input]
      (loop [result input, fns (reverse args)]
        (if (empty? fns)
          result
          (recur ((first fns) result) (rest fns)))))
    (fn
      [a & i]
      (let [input (conj i a)
            funs (reverse args)]
        (loop [result (apply (first funs) input), fns (rest funs)]
          (if (empty? fns)
            result
            (recur ((first fns) result) (rest fns))))))
    ))

(defcheck solution-9644520e
  (fn [& fs]
    (fn [& xs]
      (loop [fs fs v xs]
        (if (seq fs)
          (recur (butlast fs) [(apply (last fs) v)])
          (first v))))))

(defcheck solution-9682508d
  (fn [& a] (fn [& b] (reduce #(%2 %1) (apply (last a) b) (reverse (butlast a))))))

(defcheck solution-9685c0b8
  (fn my-comp [& fns]
    (fn [& args] (first (reduce #(vector (apply %2 %)) args (reverse fns))))))

(defcheck solution-9701f916
  (fn
    [& f]
    (fn [& a]
      (loop [l f n a]
        (if (empty? (rest l))
          (apply (last l) n)
          (recur (butlast l) (vector (apply (last l) n)))
          )))
    ))

(defcheck solution-973f2628
  (
    fn [& fns]
    (fn [& args]
      (loop [remainingFunctions (butlast fns)
             ret (apply (last fns) args)]
        (if (empty? remainingFunctions)
          ret
          (recur (butlast remainingFunctions) ((last remainingFunctions) ret))))
      )))

(defcheck solution-97a4f250
  (fn[& f] #((reduce (fn[v f] [(apply f v)]) %& (reverse f)) 0)))

(defcheck solution-97f7678b
  (fn fun-comp
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g & fs]
     (reduce fun-comp (list* f g fs)))))

(defcheck solution-98265701
  (fn [& fns]
    (reduce (fn [fs f]
              (fn [& args]
                (fs (apply f args))))
      fns)))

(defcheck solution-98c64c8a
  (fn foo [& fns]
    (let [f (first fns) r (next fns)]
      (if r
        (fn [& args] (f (apply (apply foo r) args)))
        f))))

(defcheck solution-99370dae
  (fn [& fns]
    (let [rfns (reverse fns)]
      (fn [& args]
        (reduce
          (fn [result f]
            (f result))
          (apply (first rfns) args)
          (rest rfns))))))

(defcheck solution-9a20f854
  (fn [& fs] (let [[x & xs] (reverse fs)]
               #(reduce (fn [acc f] (f acc))
                  (apply x %&)
                  xs))))

(defcheck solution-9a9a7c9d
  (fn [& fns]
    (fn [& args]
      (reduce
        #(%2 %1)
        (apply (last fns) args)
        (reverse (butlast fns))))))

(defcheck solution-9ab25df0
  (fn [& fs]
    (fn [& args]
      (reduce (fn [acc elt] (elt acc))
        (apply (first (reverse fs)) args)
        (rest (reverse fs))))))

(defcheck solution-9b19d671
  (fn compose [f & fs]
    (if (empty? fs)
      #(apply f %&)
      #(f (apply (apply compose fs) %&)))))

(defcheck solution-9b1c40cf
  (fn [& fs] (fn [& x] (loop [f (reverse fs) a x] (if (empty? f) (first a) (recur (rest f) (list (apply (first f) a))))))))

(defcheck solution-9b3ad168
  (fn combine [& f]
    (fn [& args]
      (let [start (apply (last f) args)
            rf (rest (reverse f))]
        (reduce
          (fn [a b] (b a))
          start
          rf)))))

(defcheck solution-9bc7cfe9
  (fn [& f] (fn [& x] (first (reduce #(list (apply %2 %1)) x (reverse f))))))

(defcheck solution-9c33660f
  (fn [& fs]
    (fn [& args]
      ((fn iter [fs]
         (if (= 1 (count fs))
           (apply (first fs) args)
           ((first fs) (iter (rest fs)))))
       fs))))

(defcheck solution-9c64c780
  (fn f [& fs] (reduce #(fn [& n] (%1 (apply %2 n))) identity fs)))

(defcheck solution-9cddca93
  (fn cmp [f & fs]
    (if fs
      #(f (apply (apply cmp fs) %&))
      f)))

(defcheck solution-9df60cbb
  (fn [& v]
    #(first
       (reduce (fn [a b] [(apply b a)])
         %& (reverse v)))))

(defcheck solution-9e17f768
  (fn [& fs] (reduce #(fn [& args] (% (apply %2 args))) fs)))

(defcheck solution-9e634dbe
  (fn [& f]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse f) )))))

(defcheck solution-9ecd0212
  (fn [ & args]
    (fn [ & xs]
      (first (reduce (fn [a b] [(apply b a)]) xs (reverse args))))))

(defcheck solution-a003e999
  (fn c
    ([f] f)
    ([f & fs]
     (let [fs (reverse (list* f fs))]
       (fn [& xs]
         ((apply c (reverse (next fs))) (apply (first fs) xs)))))))

(defcheck solution-a007d9fa
  (fn [& fs]
    (reduce (fn [f g]
              #(f (apply g %&))) fs)))

(defcheck solution-a026468e
  (fn compm [& fs]
    (if (= 1 (count fs))
      (fn [& r]
        (apply (first fs) r))
      (fn [& r]
        ((first fs) (apply (apply compm (rest fs)) r))
        ))))

(defcheck solution-a066f4fc
  (fn c [& fs]
    (if (= 1 (count fs))
      (fn [& a] (apply (first fs) a))
      (fn [& a] ((first fs) (apply (apply c (rest fs)) a))))))

(defcheck solution-a09bb05b
  (fn
    [& fs]
    (let [fs (reverse (seq fs))]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-a10fda1f
  (fn [& fs] (fn [& xs] (loop [[f & gs] (rest (reverse fs))
                               y (apply (last fs) xs)] (if f (recur gs (f y)) y)))))

(defcheck solution-a133f0d4
  (fn [& fns]
    (let [[f & fs] (reverse fns)]
      (fn [& x] (reduce #(%2 %1) (apply f x) fs)))))

(defcheck solution-a14b1ebe
  (fn [& fs] (reduce #(fn [& args] (%1 (apply %2 args))) fs)))

(defcheck solution-a177ead0
  (fn new-comp [& fs]
    (letfn [(restdo [f margs]
              (if (= '() f)
                (first margs)
                (restdo (rest f) [(apply (first f) margs)])))
            (firstdo [& margs]
              (restdo (reverse fs) margs))]
      firstdo)))

(defcheck solution-a17aea22
  (fn c [& fs]
    (let [sf (reverse fs)]
      (fn [& as]
        (reduce #(%2 %1) (apply (first sf) as) (rest sf))))))

(defcheck solution-a1b50788
  (fn [& fs]
    (fn [& xs]
      (loop [fs (reverse fs) args xs]
        (if fs
          (recur (next fs) (list (apply (first fs) args)))
          (when args (first args)))))))

(defcheck solution-a1e904f
  #(reduce (fn [g f] (fn [& args] (g (apply f args)))) %&))

(defcheck solution-a2abaa7a
  (fn cmp [& fs]
    (if (empty? fs)
      identity
      (fn [& args]
        ((apply cmp (butlast fs))
         (apply (last fs) args))))))

(defcheck solution-a34128d5
  (fn [& fns]
    (reduce (fn [acc f] (fn [& x] (acc (apply f x)))) identity fns)))

(defcheck solution-a35face9
  (fn mycomp [& f]
    (fn [& x]
      (first (reduce #(vector (apply %2 %1)) x (reverse f))))))

(defcheck solution-a427ebfa
  (fn [& fs]
    (let [[f & fs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-a4303f42
  (fn [& fs]
    (fn [& args] (reduce #(%2 %) (apply (last fs) args) (reverse (butlast fs))))))

(defcheck solution-a4327132
  (fn my-comp5
    ([f] f)
    ([f & fs] #(f (apply (apply my-comp5 fs) %&)))))

(defcheck solution-a4497fb7
  (fn [& fs] (fn [& args] (reduce #(%2 %) (apply (last fs) args) (next (reverse fs))))))

(defcheck solution-a49152b3
  (fn my-comp [& funcs]
    (let [funcs (reverse funcs)]
      (fn [& args]
        (loop [ret (apply (first funcs) args)
               funcs (next funcs)]
          (if funcs
            (recur ((first funcs) ret) (next funcs))
            ret))))))

(defcheck solution-a4d4d928
  (fn __ [& fs]
    (reduce (fn [a b] (fn [& args] (a (apply b args)))) fs)))

(defcheck solution-a4d6140f
  (fn compose
    ([] identity)
    ([f] f)
    ([f g] (fn [& args]
             (f (apply g args))))
    ([f g & rest] (reduce compose (list* f g rest)))))

(defcheck solution-a4d6ff20
  (fn [& fns]
    (let [f-fn (last fns)
          fns (reverse (drop-last 1 fns))]
      (fn [& args]
        (reduce #(%2 %1) (apply f-fn args) fns)))))

(defcheck solution-a4efbf79
  (fn comp'
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([& args] (f (apply g args)))))
    ([f g & fs] (reduce comp' (list* f g fs)))))

(defcheck solution-a4f3d778
  (fn [& args] (reduce (fn [a b] (fn [& more] (a (apply b more)))) args)))

(defcheck solution-a5b6e41b
  (fn
    [& funcs]
    (let [[head &  other] (reverse funcs)]
      (fn [& args] (reduce #(%2 %1) (apply head args) other))
      )
    ))

(defcheck solution-a6256272
  (fn [& f]
    (fn [& args]
      (reduce #(%2 %)
        (apply (last f) args)
        (rest (reverse f))))))

(defcheck solution-a638c262
  (fn custom-comp
    [& fs]
    (let [reverse-fs (reverse fs)]
      (fn [& args]
        (reduce
          (fn [result f]
            (f result))
          (apply (first reverse-fs) args)
          (rest reverse-fs))))))

(defcheck solution-a6983015
  (fn [& fns]
    (fn [& xs]
      (reduce #(%2 %1) (apply (last fns) xs)
        (reverse (butlast fns))))))

(defcheck solution-a78e7713
  (fn ([f1 f2] (fn [& x] (f1 (apply f2 x))))
    ([f1 f2 f3] (fn [& x] (f1 (f2 (apply f3 x)))))
    ))

(defcheck solution-a7d5f478
  (fn [& funs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last funs) args) (reverse (butlast funs))))))

(defcheck solution-a7e436a4
  (fn [& fns]
    (fn [& args]
      (first (reduce
               (fn [args f]
                 #_(println args)
                 (list (apply f args)))
               args
               (reverse fns))))))

(defcheck solution-a816f17f
  (fn [& fs]
    (fn [& args]
      (first (reduce (fn [r f] [(apply f r)]) args (reverse fs))))))

(defcheck solution-a8278c36
  (fn [f & r]
    (fn [& args]
      ((fn af [[f & r] args]
         (if r
           (f (af r args))
           (apply f args)))
       (cons f r) args))))

(defcheck solution-a829210
  (fn [f & more]
    (let [fs (reverse (list* f more))]
      (fn [& args]
        (loop [res (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) res) (next fs))
            res))))))

(defcheck solution-a9002432
  (fn func [f & fs]

    (fn [& x]

      (if fs

        (f (apply (apply func fs) x))

        (apply f x)))))

(defcheck solution-a92dd98
  #(reduce (fn [f g] (fn [& x] (f (apply g x)))) %&))

(defcheck solution-a960b44d
  (fn [& fns] (fn [& args] (reduce #(%2 %1) (apply (last fns) args) (reverse (butlast fns))))))

(defcheck solution-a962963
  (fn newcomp
    [& fns]
    (fn
      [& args]
      (loop [fns (reverse fns)
             args args]
        (if (seq fns)
          (recur (next fns) (vector (apply (first fns) args)))
          (apply identity args))))))

(defcheck solution-a9701725
  (fn
    ([f g]
     (fn [& args]
       (f (apply g args))))
    ([f g h]
     (fn [& args]
       (f (g (apply h args)))))))

(defcheck solution-a996698a
  (fn [& f]
    (fn [& a]
      (reduce #(%2 %) (apply (last f) a) (rest (reverse f))))))

(defcheck solution-a9b04e55
  (fn [& f]
    (fn [& a]
      (nth
        (reduce
          #(list (apply %2 %))
          a
          (reverse f))
        0))))

(defcheck solution-a9f72f3b
  (fn [& fs] (fn [& args] (reduce (fn [v f] (f v)) (apply (last fs) args)
                            (reverse (butlast fs))))))

(defcheck solution-aa1acbc
  (fn iter [a & r]
    (if (empty? r)
      a
      #(a (apply (apply iter r) %&)))))

(defcheck solution-aa42b2ac
  (fn compose-it
    [& functions]
    (fn [& arg]
      (loop [[h & t] (reverse functions)
             accum arg]
        (if (nil? t)
          (apply h accum)
          (recur t [(apply h accum)]))))))

(defcheck solution-aa477d57
  (fn [& fns]
    (fn [& args]
      (let [plan (fn exec-plan [[f & fs]] (if (empty? fs) (apply f args) (f (exec-plan fs))))]
        (plan fns)))))

(defcheck solution-aa60ec9f
  (fn [& fs]
    (fn [& [h & _ :as vs]]
      (if (<= (count vs) 1)
        (let []
          (reduce #(%2 %1) h (reverse fs)))
        (let [[fh & ft] (reverse fs)
              i (apply fh vs)]
          (reduce #(%2 %1) i ft)
          )
        )
      )
    ))

(defcheck solution-aa9e361f
  (fn [& f] (fn [& a] (reduce #(%2 %) (apply (last f) a) (reverse (butlast f))))))

(defcheck solution-aacca7d9
  ;function is the first-class citizen of clojure
  (fn myComp
    ([f] f)
    ([f1 f2]
     (fn[& args]
       (f1 (apply f2 args))))
    ([f1 f2 & fs]
     (apply myComp (myComp f1 f2) fs))))

(defcheck solution-aad0aff3
  (fn [& v]
    (loop [fns  (rest v)
           res  (first v)]
      (if (empty? fns) res
                       (recur (rest fns)
                         (fn [ & a ] (res (apply (first fns) a))))))))

(defcheck solution-aad10b32
  (fn [& l] (fn [& z] (reduce #(%2 %) (apply (last l) z) (rest (reverse l))))))

(defcheck solution-aad1ce54
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply (first fs) args) (rest fs))))))

(defcheck solution-aafa5aa2
  (fn [& funs]
    (fn [& args]
      (first
        (reduce (fn [args fun]
                  (list
                    (apply fun args)))
          args
          (reverse funs))))))

(defcheck solution-ab182e3e
  (fn [& funcs]
    (fn f
      [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse funcs)))
      )))

(defcheck solution-ab435d86
  (fn
    ([f g]
     (fn
       ([](f (g)))
       ([x](f (g x)))
       ([x y](f (g x y)))
       ([x y z](f (g x y z)))
       ([x y z & args](f (apply g x y z args)))
       )
     )
    ([f g h]
     (fn
       ([](f (g (h))))
       ([x](f (g (h x))))
       ([x y](f (g (h x y))))
       ([x y z](f (g (h x y z))))
       ([x y z & args](f (g (apply h x y z args))))
       )
     )
    ))

(defcheck solution-ab46d44
  (fn meu-comp [& f]
    (fn [& p]
      (reduce #(%2 %1) (apply (last f) p) (reverse (drop-last 1 f))))))

(defcheck solution-ac25310c
  (fn [& fns]
    (fn [& args]
      (reduce
        (fn [f s] (s f))
        (apply (last fns) args)
        (reverse (butlast fns))))))

(defcheck solution-ac47410d
  (fn [& fns]
    (fn [& args]
      (let [rfns (reverse fns)]
        (loop [fs (rest rfns)
               x  (apply (first rfns) args)]
          (if-let [f (first fs)]
            (recur (rest fs) (f x))
            x))))))

(defcheck solution-ac75b1b2
  (fn my-comp [& fns]
    (fn [& args]
      (if (= (count fns) 1)
        (apply (first fns) args)
        ((apply my-comp (butlast fns))
         (apply (last fns) args))))))

(defcheck solution-aca5bce5
  (fn [& xs] (let [fs (reverse xs)] (fn [& args] (loop [acc (apply (first fs) args) f (rest fs)] (if (empty? f) acc (recur ((first f) acc) (rest f))))))))

(defcheck solution-acde52b5
  (fn [& fs]
    (fn [& args]
      (let [[ff & rf] (reverse fs), x (apply ff args)]
        (reduce #(%2 %1) x rf)))))

(defcheck solution-acdf2acd
  (fn my-comp [& fns]
    (if (= 1 (count fns))
      (first fns)
      (let [[cur-fn & rest-fns] fns
            rest-comp (apply my-comp rest-fns)]
        #(cur-fn (apply rest-comp %&))))))

(defcheck solution-ad6980f
  (fn [& f]
    #(loop [r  (apply (last f) %&)
            f (rest (reverse f))]
       (if (empty? f)
         r
         (recur ((first f) r)
           (rest f))))))

(defcheck solution-ada57c4a
  (fn cmp
    ([f g] (fn [& args] (f (apply g args))))
    ([f g & fs] (reduce cmp (concat [f g] fs)))))

(defcheck solution-adb0ea0d
  (fn [& ops]
    (fn [& args]
      (let [ops (reverse ops) result (apply (first ops) args)]
        (loop [ops (rest ops) result result]
          (if (empty? ops) result
                           (recur (rest ops) ((first ops) result))))))))

(defcheck solution-ae2a0404
  (fn [& s]
    (reduce
      (fn [f g]
        (fn [& a] (g (apply f a))))
      (reverse s))))

(defcheck solution-aee933f6
  (fn [& funcs] (reduce (fn [f1 f2] #(f1 (apply f2 %&))) funcs)))

(defcheck solution-af105fc7
  (fn [& fs] (fn [& args] (first (reduce #(list (apply %2 %)) args (reverse fs))))))

(defcheck solution-b0123065
  (fn co [ & f ] (fn [ & x ] (if (= 1 (count f)) (apply (first f) x) ((first f) (apply (apply co (rest f)) x ) )))))

(defcheck solution-b030fdd5
  (fn f [a & b]
    (if b
      #(a (apply (apply f b) %&)  )
      a )))

(defcheck solution-b06ddbe2
  (fn [& funcs]
    (fn [& args] (first (reduce #(list (apply %2 %)) args (reverse funcs))))))

(defcheck solution-b192088c
  (fn my-comp
    ([f] (fn [& args] (apply f args)))
    ([f & fs] (fn [& args] (f (apply (apply my-comp fs) args))))))

(defcheck solution-b1d7d5f5
  (fn [& f-list] (fn [& d] (reduce #(%2 %) (apply (last f-list) d) (rest (reverse f-list))))))

(defcheck solution-b252886a
  (fn [x & xs]
    (fn [& args]
      ((fn step [[f & fs] a]
         (if fs
           (f (step fs a))
           (apply f a)))
       (cons x xs) args))))

(defcheck solution-b26e4bc3
  (fn c
    ([f g]
     (fn [& args](f (apply g args))))
    ([f g h]
     (fn [& args] (f (g (apply h args)))))))

(defcheck solution-b2f031eb
  (fn z [& fs]
    (fn [& args]
      (loop [funcs (rest (reverse fs))
             res (apply (last fs) args)]
        (if (empty? funcs)
          res
          (recur (rest funcs) ((first funcs) res)))))))

(defcheck solution-b3ba048c
  (fn ! [f & fs]
    (if (empty? fs)
      (fn [& a]
        (apply f a))
      (fn [ & a]
        (f (apply (apply ! fs) a))))))

(defcheck solution-b40a6136
  (fn [& f] (reduce #(fn [& x] (% (apply %2 x))) f)))

(defcheck solution-b42a978c
  (fn [& f]
    (fn [& args]
      (reduce #(%2 %1) (apply (last f) args) (reverse (drop-last f)))
      )
    ))

(defcheck solution-b43fd6a
  (fn [& fs]
    (fn [& xs]
      (let [x (apply (last fs) xs)]
        (reduce #(%2 %1) x (reverse (butlast fs)))))))

(defcheck solution-b4b02745
  (fn comp1 [& fs]
    (fn [& arg]
      ((fn mf [a fss]
         (if (empty? fss) a (mf ((first fss) a) (next fss)))
         ) (apply (last fs) arg) (drop 1 (reverse fs)))
      )))

(defcheck solution-b4c52f75
  (fn [& f] (fn [& x]
              (let [fs (reverse f)]
                (reduce #(%2 %1) (apply (first fs) x) (rest fs))))))

(defcheck solution-b4d91f28
  (fn [ & args ] (fn [& x] (loop [ft (reverse args) fp x] (if (empty? ft) (first fp) (recur (rest ft) (vector (if (= 1 (count fp)) ((first ft) (last fp)) (apply (first ft) fp)))))))))

(defcheck solution-b5005821
  (fn comp1
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g & fs]
     (reduce comp1 (list* f g fs)))))

(defcheck solution-b525ac70
  (fn [& funs]
    (reduce  (fn [acc,f] (fn [& args] (acc (apply f args)))) funs)))

(defcheck solution-b537c02f
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %1)
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-b5e4944
  (fn [& fs]
    (let [rfs (reverse fs) f (first rfs) rs (rest rfs)]
      (fn [& v]
        (reduce #(%2 %1) (apply f v) rs)))))

(defcheck solution-b707a3aa
  (fn [& fs]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse fs))))))

(defcheck solution-b75df75e
  (let [f
        (fn [funs result]
          (if (nil? funs)
            result
            (recur (next funs) ((first funs) result))
            )
          )
        g
        (fn [z & x] (f (next z) (apply (first z) x)))]
    (fn [& p]
      (partial g (reverse p))
      )
    ))

(defcheck solution-b836138f
  (fn my-comp
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g & fs]
     (reduce my-comp (list* f g fs)))))

(defcheck solution-b8c62b26
  (fn outer [& params]
    (fn inner [& inputs]
      (loop [functions (reverse params)
             result inputs]
        (if (empty? functions)
          (first result)
          (recur (rest functions)  [(apply (first functions) result)])
          )))
    ))

(defcheck solution-b915bebc
  (fn [& x] (reduce (fn [f g] (fn [& a] (f (apply g a)))) x)))

(defcheck solution-b91b9f1a
  (fn
    ([f g] (fn [v] (-> v g f)))
    ([f g h] (fn [& v] (f (g (apply h v)))))))

(defcheck solution-b97c4387
  (fn [& fns]
    (let [fns (reverse fns)]
      (fn [& args]
        (loop [ret (apply (first fns) args)
               fns (next fns)]
          (if fns
            (recur ((first fns) ret) (next fns))
            ret))))))

(defcheck solution-b98d1404
  (fn [& a] (reduce #(fn [& x] (% (apply %2 x))) a)))

(defcheck solution-b9c99d27
  #_(fn [& fns]
      (fn [& xs]
        (loop [vs xs, gs (reverse fns)]
          (if (seq gs)
            (let [g (first gs)
                  vs' (list (apply g vs))]
              (recur vs' (rest gs)))
            (first vs)))))

  (fn [& fns]
    (fn [& xs]
      (->> (reduce (fn [acc f]
                     (list (apply f acc)))
             xs
             (reverse fns))
        first))))

(defcheck solution-ba74388b
  (fn [& fs]
    (fn [& args]
      (let [[f & fs] (reverse fs)]
        (reduce #(%2 %) (apply f args) fs)))))

(defcheck solution-ba7cb0fa
  (fn [& flist] (reduce (fn [f1 f2] (fn [& args] (f1 (apply f2 args)))) identity flist)))

(defcheck solution-ba947004
  (fn my-comp [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs  (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-bab47b26
  (fn [& fns]
    (fn [& x]
      ((fn call [fns]
         (if (empty? (rest fns))
           (apply (first fns) x)
           ((first fns) (call (rest fns)))))
       fns))))

(defcheck solution-bac909c7
  (fn com [& fs]
    (let [fs (reverse fs)]
      (fn [& b]
        (reduce #(%2 %1) (apply (first fs) b) (rest fs))))))

(defcheck solution-bb893e78
  (fn
    ([f g]
     (fn [& args]
       (f (apply g args))))
    ([e f g]
     (fn [& args]
       (e (f (apply g args)))))
    ))

(defcheck solution-bb9d0409
  (fn mycomp
    [f & r]
    (fn [& args]
      (if (empty? r)
        (apply f args)
        (f (apply (apply mycomp r) args))))))

(defcheck solution-bc339ce3
  (fn[& f]
    (let [fs (reverse f)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ( (first fs) ret)
              (next fs))
            ret))))))

(defcheck solution-bc59272e
  (fn myComposition
    [& fns]
    (let [butFirstFn (rest (reverse fns))]
      (fn [& args] (reduce #(%2 %1) (apply (last fns) args) butFirstFn)))))

(defcheck solution-bd485b3a
  (fn [& p]
    (reduce
      (fn [f g]
        #(f (apply g %&))) p)))

(defcheck solution-bd7e75b3
  (fn mc
    ([f]
     f)
    ([f g]
     (fn [& args] (f (apply g args))))
    ([f g & more]
     (fn [& args] (f (apply (apply mc g more) args))))
    ))

(defcheck solution-bdbfaccd
  (fn [& f] (fn [& x] (reduce #(%2 %1) (apply (last f) x) (rest (reverse f))))))

(defcheck solution-bdd770f8
  (fn my-comp
    ([f1] (fn [& xs] (apply f1 xs)))
    ([f1 f2] (fn [& xs] (f1 (apply f2 xs))))
    ([f1 f2 f3] (fn [& xs] (f1 (f2 (apply f3 xs)))))))

(defcheck solution-be00a6a8
  (fn cmp
    ([f] f)
    ([f & g] #(f (apply (apply cmp g) %&)))))

(defcheck solution-be088ef
  (fn [& fs]
    (fn [& a] (let [[f & r] (reverse fs)]
                (reduce #(%2 %1) (apply f a) r)))))

(defcheck solution-be33a003
  (fn [& fs] (apply (fn f' [f & fs]
                      (if (empty? fs) f
                                      (fn [& args] ((apply f' fs) (apply f args))))) (reverse fs))))

(defcheck solution-be863ae3
  (fn [& funs]
    (let [fl (reverse funs)]
      (fn [& args]
        (loop [ret (apply (first fl) args)
               ffl (next  fl)]
          (if ffl
            (recur ((first ffl) ret) (next ffl))
            ret))))))

(defcheck solution-be8b2654
  (fn compo [& funcs]
    (fn [& x]
      (let [y (apply (last funcs)  x)
            subcoll (drop-last funcs)]
        (if (first subcoll)
          ((apply compo subcoll) y)
          y) ))))

(defcheck solution-bec8e1d9
  (fn [& fs]
    (fn com
      ([s]
       (reduce #(%2 %1) s (reverse fs)))
      ([x & s]
       (reduce #(%2 %1) (apply (last fs) (concat [x] s))
         (reverse (butlast fs)))))))

(defcheck solution-beffce79
  (fn [& f]
    (fn [& x]
      (loop [f (reverse f) x x]
        (if (seq f)
          (recur (next f) [(apply (first f) x)])
          (first x))))))

(defcheck solution-bf2c96bf
  (fn [& s] (fn [& x]
              (reduce #(%2 %1) (apply (last s) x) (rest (reverse s))))))

(defcheck solution-bf8b7543
  (fn c
    ([f] f)
    ([f & g]
     #(f (apply (apply c g) %&)))))

(defcheck solution-bfd327cf
  (fn [& funcs]
    (fn [& args]
      ((fn comp-rec [fs arguments]
         (if (empty? (rest fs))
           (apply (first fs) args)
           (apply (first fs) [(comp-rec (rest fs) arguments)])))
       funcs args))))

(defcheck solution-c03df44d
  (fn [& sf]
    (let [fs (reverse sf)
          iscon (fn [testo] (= (type (cons 1 (list 1))) (type testo)))]
      (fn [parg & arg]
        (loop [f (first fs)
               fr (rest fs)
               zarg (if (nil? arg) parg (cons parg arg))]
          (if f
            (recur (first fr) (rest fr)
              (if-not (iscon zarg) (apply f zarg nil)
                                   (apply f zarg)))
            zarg))))))

(defcheck solution-c044b37
  (fn [& fs] (fn [& args] (first (reduce (fn [r f] [(apply f r)]) args (reverse fs))))))

(defcheck solution-c04f6454
  (fn my-comp
    ([x y]
     (fn [& ls]
       (x (apply y ls))))
    ([x y & xs]
     (apply my-comp (my-comp x y) xs))))

(defcheck solution-c0793465
  (fn compfs [& fs]
    (let [f (last fs), lf (butlast fs)]
      (if (empty? lf)
        (fn [& arg]
          (apply f arg)
          )
        (fn [& arg]
          ((apply compfs lf) (apply f arg))
          )
        )
      )
    ))

(defcheck solution-c07941ab
  (fn [& f]
    (fn [& a]
      (let [fs (reverse f)]
        (loop [r (apply (first fs) a)
               fs (next fs)]
          (if fs
            (recur ((first fs) r) (next fs))
            r))))))

(defcheck solution-c094e6e9
  (fn f ([x & l] (fn [& xs] (x (apply (apply f l) xs)))) ([x] x)))

(defcheck solution-c0b12929
  (fn [& funcs]
    (fn [& args]
      (loop [ n (dec (count funcs))
             result (apply (last funcs) args) ]
        (if (= 0 n) result
                    (recur (dec n) ((nth funcs (dec n)) result)))))))

(defcheck solution-c0b6e9ef
  (fn my_comp
    ([f] f)
    ([f & fs] #(f (apply (apply my_comp fs) %&)))))

(defcheck solution-c0debcf1
  (fn [& fns]
    (fn [& args]
      (let [[f & rfns] (reverse fns)]
        (reduce #(%2 %1) (apply f args) rfns)))))

(defcheck solution-c138d6f7
  (fn [ & fs ]
    #(first (reduce
              (fn [v f] [(apply f v)])
              %&
              (reverse fs)))))

(defcheck solution-c15a4ff
  (fn comb [& funcs]
    (fn [& args]
      (first
        (reduce #(vector (apply %2 %1)) args (reverse funcs ))))))

(defcheck solution-c168d9bf
  (fn my-comp [ & fns ]
    (if (empty? (rest fns)) (first fns)
                            (fn [ & args ] ( (apply my-comp (drop-last fns)) (apply (last fns) args))))))

(defcheck solution-c1f01031
  (fn [& fs] (fn [& x] (let [lf (last fs), init (if (next x) (apply lf x) (lf (first x))) ]  (reduce (fn [r f] (f r)) init (reverse (drop-last fs))) )) ))

(defcheck solution-c1fb2b45
  (fn [& fs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fs) args) (next (reverse fs))))))

(defcheck solution-c2779e8a
  (fn [& f]
    (fn [& x]
      (first
        (reduce #(list (apply %2 %1)) x (reverse f))))))

(defcheck solution-c2d0fee7
  (fn [& fns]
    (fn [& args]
      (first (reduce
               (fn [res f] (list (apply f res)))
               args
               (reverse fns))))))

(defcheck solution-c2fa46bd
  (fn [& f] (fn [& a]  ((reduce (fn [v g] [(apply g v)]) a (reverse f)) 0))))

(defcheck solution-c2fe881e
  (fn compose [first & more]
    (fn [& args]
      (if (empty? more)
        (apply first args)
        (first (apply (apply compose more) args))))))

(defcheck solution-c45ca6b2
  (letfn
   [(comp-2 [f g]
      (fn [& more]
        (f (apply g more))))

    (comp-n [ops]
      (reduce comp-2 ops))]

    #(comp-n %&)))

(defcheck solution-c48a985c
  (fn [& fns]
    (fn [& args]
      (loop [rfns (rest (reverse fns))
             v (apply (last fns) args)]
        (if (seq rfns) (recur (rest rfns) ((first rfns) v))
                       v)))))

(defcheck solution-c49206e7
  (fn [& fsi] (fn [ & args]
                (let [fs (reverse fsi)]
                  (loop [res (apply (first fs) args) fsrest (next fs)]
                    #_(println res, fsrest)
                    (if (nil? fsrest)
                      res
                      (recur ((first fsrest) res) (next fsrest))))))))

(defcheck solution-c49fef5b
  (fn [& fns]
    (fn [& args]
      (reduce
        (fn [x f] (f x))
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-c4c6efbb
  (fn [& fns] (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse fns))))))

(defcheck solution-c575e31e
  (fn [& fs]
    (letfn [(go-func [fs args]

              (if (empty? (rest fs))
                (apply (first fs) args)
                ((first fs)
                 (go-func (rest fs) args)))
              )]
      (fn [& par] (go-func fs par)))))

(defcheck solution-c59e7aec
  (fn [& f]
    (fn [& p]
      ((fn r [g q] (if (nil? g) q (r (butlast g) ((last g) q)))) (butlast f) (apply (last f) p)))))

(defcheck solution-c5d5c02a
  (fn [& functions] (fn [& args] (first (reduce #(vector (apply %2 %)) args (reverse functions))))))

(defcheck solution-c6ad58ff
  (fn [& f]
    (reduce
      (fn [a x]
        (fn [& y] (a (apply x y)))) f)))

(defcheck solution-c6d6ec7e
  (fn [& fs] (let [fs (reverse fs)]
               (fn [& args] (reduce #(%2 %)
                              (apply (first fs) args) (rest fs))))))

(defcheck solution-c7130b22
  (fn c [f & fs] (fn [& a] (if (seq fs) (f (apply (apply c fs) a)) (apply f a)))))

(defcheck solution-c7299c03
  (fn com
    ([] identity)
    ([& fs] (fn [& xs] ((apply com (butlast fs)) (apply (last fs) xs))))))

(defcheck solution-c72fe856
  (fn [& fs]
    (fn [& arg] (first (reduce #(vector (apply %2 %1))
                         arg
                         (reverse fs))))))

(defcheck solution-c75cc66c
  (fn [& xs] (fn [& ys] (reduce #(%2 %1) (apply (last xs) ys) (rest (reverse xs))))))

(defcheck solution-c75f75af
  (fn[& f]
    (let [[g & f] (reverse f)]
      (fn[& a] (reduce #(%2 %) (apply g a) f)))))

(defcheck solution-c77a24b6
  (fn [& fs]
    (fn [& args]
      (reduce #(apply %2 [%1]) (apply (last fs) args) (reverse (butlast fs))))))

(defcheck solution-c7c7ac44
  (fn my-comp [f & rem-funcs]
    (if (empty? rem-funcs)
      f
      (fn [& args] (f (apply (apply my-comp rem-funcs) args))))))

(defcheck solution-c7c7ea73
  (fn j [& fns]
    (fn [& fnargs]
      (first (reduce (fn [x y] [(apply y x)])
               fnargs
               (reverse fns)))
      )
    ))

(defcheck solution-c7e0c022
  (fn [& s]
    #(reduce (fn [c f] (f c))
       (apply (last s) %&)
       (rest (reverse s)))))

(defcheck solution-c7fd3244
  (fn [& funcs]
    (let [fs (reverse funcs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-c7fe1b37
  (fn [f & flist]
    (fn [x & y]
      (let [lf (into [f] flist)]
        (loop [i  (count flist) z (into [x] y)]
          #_(println "z" z "x" x)
          (cond
            (< i 0) z
            (vector? z) (recur (dec i) (apply (get lf i) z))
            :else (recur (dec i) ((get lf i) z))
            )
          )
        )
      )
    ))

(defcheck solution-c7fefb18
  (fn [& funcs] (fn [& args] (first (reduce #(list (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-c82098cc
  (letfn [(cmp [f & fs] (if (empty? fs) f (fn [& args] (f (apply (apply cmp fs) args)))))] cmp))

(defcheck solution-c8ea1bb
  (fn F[f1 & f2]
    (fn [& xs] (if (= nil f2) (apply f1 xs) (f1 (apply (apply F f2) xs))))
    ))

(defcheck solution-c94343c2
  (fn cmp
    ([f] f)
    ([f g] (fn ([& args] (f (apply g args)))))
    ([f g h] (fn ([& args] (f (g (apply h args))))))))

(defcheck solution-c9aa6e5b
  (fn [& fns]
    (let [last-fn (last fns)
          fns (drop-last fns)]
      (fn [& rest]
        (reduce (fn [args f] (f args)) (apply last-fn rest) (reverse fns))))))

(defcheck solution-ca0b388d
  (fn
    ([f1 f2] (fn [& args] (f1 (apply f2 args))))
    ([f1 f2 f3] (fn [& args] (f1 (f2 (apply f3 args)))))))

(defcheck solution-ca7f7350
  (fn f [& v]
    (let [[g & u] (reverse v)]
      (fn [& y]
        (reduce #(%2 %) (apply g y) u)))))

(defcheck solution-cabf08fc
  (fn
    ([f1 f2 ]
     (fn [& args]
       (f1 (apply f2 args))))
    ([f g & fs]
     (fn [& args]
       (let [fs (reverse (list* f g fs))]
         (loop [fss (rest fs) ret (apply (first fs) args)]
           (if fss
             (recur (next fss) ((first fss) ret))
             ret)))))))

(defcheck solution-caccd5d3
  (fn [& ff]
    (let [aa (fn  [fs  arg]
               (if (= fs [])
                 (first arg)
                 (recur (rest fs) (list (apply (first fs) arg)))))]
      (fn [& cc] ( aa (reverse ff) cc))
      )))

(defcheck solution-cb13a99c
  (fn [& g]
    (let [[f & r] (reverse g)]
      (fn [& a]
        (reduce
          #(%2 %1)
          (apply f a)
          r)))))

(defcheck solution-cb2977fc
  (fn [& fns]
    (fn [& args]
      (reduce (fn [v f] (f v))
        (apply (last fns) args)
        (rest (reverse fns))))))

(defcheck solution-cc667977
  (fn [& f]
    (fn [& coll]
      (first
        (reduce #(vector (apply %2 %1)) coll (reverse f))))))

(defcheck solution-cc7ac4b6
  (fn [& f]
    (let [fs (reverse f)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-ccffccae
  (fn [& fns]
    (fn [& args]
      (let [rfns (reverse fns)
            init-res (apply (first rfns) args)]
        (reduce #(%2 %1) init-res (rest rfns))))))

(defcheck solution-cd018dda
  (fn ([a b]
       #(->> % b a))
    ([a b c]
     (fn [& x](->> x (apply c) b a)))))

(defcheck solution-cd88872b
  (fn [& fs]
    (fn [& args]
      (first (reduce
               (fn [s e] [(apply e s)])
               args
               (reverse fs))))))

(defcheck solution-cdeff528
  (fn [& fns]
    (reduce (fn [agg f] #(agg (apply f %&))) fns)))

(defcheck solution-ce04cf6f
  (fn [& fns]
    (let [f (reverse fns)]
      (fn [& args]
        (reduce #(%2 %1) (apply (first f) args) (rest f))))))

(defcheck solution-ce0fdd16
  (fn [& fns]
    (reduce (fn [g f] (fn [& args] (f (apply g args)))) (reverse fns))))

(defcheck solution-cf189cc3
  (fn [& functions]
    (fn [& v]
      (first (reduce #(vector (apply %2 %1)) v (reverse functions))))))

(defcheck solution-cffb9394
  (fn [& fs]
    (let [[f & fs] (reverse fs)]
      (fn [& xs]
        (reduce #(%2 %) (apply f xs) fs)))))

(defcheck solution-d03b8a76
  (fn [& fns]
    (fn [& c]
      (first (reduce #(vector (apply %2 %))
               c (reverse fns))))))

(defcheck solution-d04824d1
  (fn c
    ([x y] #(x (apply y %&)))
    ([x y & xs] (reduce c (list* x y xs)))))

(defcheck solution-d050a9ee
  (fn [& fns]
    (let [[f & fns] (reverse fns)]
      (fn [& args]
        (reduce (fn [v f] (f v)) (apply f args) fns)))))

(defcheck solution-d0ac5ed9
  (fn compose ([] (fn [x] x)) ([f] f) ([f g & fs] (fn [& args] (f (g (apply (apply compose fs) args)))))))

(defcheck solution-d0e44448
  (fn [& f]
    (fn [& a]
      (reduce #(%2 %1) (apply (last f) a) (rest (reverse f))))))

(defcheck solution-d1c563cf
  (fn mcomp
    [& fns]
    (fn [& args]
      (first (reduce (fn [r e]
                       (list (apply e r)))
               args (reverse fns))))))

(defcheck solution-d2328855
  (fn prob-0058
    [& in-funs]
    (let [rfuns (reverse in-funs)]
      (fn [& args]
        (loop [ans (apply (first rfuns) args)
               rfs (next rfuns)]
          (if rfs
            (recur ((first rfs) ans) (next rfs))
            ans))))))

(defcheck solution-d27de6a
  (fn [& f]
    (let [r (reverse f) m #(list (apply %2 %))]
      #(first (reduce m %& r)))))

(defcheck solution-d2c214d0
  (fn [& f]
    (fn [& a]
      (first ((fn r [f]
                (if (empty? f)
                  a
                  [(apply (first f) (r (rest f)))]))
              f)))))

(defcheck solution-d2d7914f
  (fn [& a] (fn [& d] (first (reduce #(list (apply %2 %)) d (reverse a))))))

(defcheck solution-d3b26cc1
  (fn compf [ & funcol]
    (let [tmpcol (reverse funcol),
          fun1 (first tmpcol),
          fcol (rest tmpcol)
          ]
      (fn lambda
        ([col]  (reduce  #(%2 %1) (fun1 col) fcol) )
        ([a b]  (reduce #(%2 %1) (fun1 a b) fcol) )
        ([a b & other] (let
                        [cols (cons a (cons b other)) ]
                         (reduce #(%2 %1) (apply fun1 cols) fcol)
                         )
         )
        )
      )
    ))

(defcheck solution-d3cff6d3
  #(let [l (reverse %&)]
     (fn [& a]
       (loop [x (apply (first l) a), l (next l)]
         (if l
           (recur ((first l) x), (next l))
           x)))))

(defcheck solution-d42ce541
  (fn [& fns]
    (fn [& args]
      (loop [val (apply (last fns) args) fnl (reverse (butlast fns))]
        (if-let [f (first fnl)]
          (recur (f val) (next fnl))
          val)))))

(defcheck solution-d45c004f
  (fn [& fs]
    (reduce #(fn [& args]
               (% (apply %2 args)))
      fs)))

(defcheck solution-d4e58db
  (fn [& fs]
    (fn [& xs]
      (let [[f & fs] (reverse fs)]
        (reduce #(%2 %) (apply f xs) fs)))))

(defcheck solution-d5b4be8d
  (fn [& f] (fn [& s] (reduce #(%2 %) (#(apply (last f) %) s) (rest (reverse f))))))

(defcheck solution-d5beebe4
  (fn [& funcs]
    (fn ([arg]
         (reduce (fn [acc f]
                   (f acc))
           arg (reverse funcs)))
      ([a & args]
       (reduce (fn [acc f]
                 (f acc))
         (apply (first (reverse funcs)) (concat [a] args)) (rest (reverse funcs)))))))

(defcheck solution-d5ed41fb
  #(fn [& c]
     (letfn [(e [[f & fs]] (if fs (f (e fs)) (apply f c)))]
       (e %&))))

(defcheck solution-d60acf83
  (fn composition
    ([f g] #(f (apply g %&)))
    ([f g & more] (reduce composition (composition f g) more))))

(defcheck solution-d6536ae7
  (fn [& fs]
    (let [[f1 & fs] (reverse fs)]
      #(reduce (fn [res f] (f res)) (apply f1 %&) fs))))

(defcheck solution-d69820e3
  (fn [& fns]
    (reduce (fn [f g]
              (fn [& args] (f (apply g args))))
      fns)))

(defcheck solution-d6b701a4
  (fn [& l]
    (reduce #(fn [& x] (% (apply %2 x))) l)))

(defcheck solution-d6bddea8
  (fn compose [& args]
    (fn [& vars]
      (loop [f (reverse args) v vars]
        (if (empty? f) (first (reverse v))
                       (recur (rest f) (list (apply (first f) v))))))))

(defcheck solution-d7118797
  (fn func-comp
    [fun & fns] (if (nil? fns)
                  fun
                  (fn [& args] (fun (apply (apply func-comp fns) args)))
                  )))

(defcheck solution-d711be48
  (fn fcomp [& fns]
    (fn [& args]
      (let [[f & fns] (reverse fns)]
        (reduce #(%2 %1) (apply f args) fns)))))

(defcheck solution-d77ae409
  (fn
    ([f g] (fn [& x] (f (apply g x))))
    ([f g h] (fn [& y] (f (g (apply h y)))))))

(defcheck solution-d8615339
  (fn [& fns]
    (let [[f & fs] (reverse fns)]
      (fn [& args] (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-d89899b
  (fn fcomp [& funcs]
    (fn [& a]
      (loop [fs (butlast funcs)
             res (apply (last funcs) a)]
        (if (empty? fs)
          res
          (recur (butlast fs) ((last fs) res) ))))))

(defcheck solution-d8bd6d46
  (fn [f & fs]
    (let[g (reverse (conj fs f))]
      (fn[ & args] (reduce #(%2 %) (apply (first g) args) (next g))))))

(defcheck solution-d999c657
  (fn [& fs](letfn [(comp'
                      ([] identity)
                      ([f] f)
                      ([f g] (fn
                               ([] (f (g)))
                               ([& x] (f (apply g x)))))
                      ([f g & h] (reduce comp' f (list* g h))))]
              (apply comp' fs))))

(defcheck solution-da046877
  (fn [& f]
    (let [fs (reverse f)
          f1 (first fs)
          fr (rest fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply f1 args) fr)))))

(defcheck solution-da6d517c
  (fn [& functions]
    (let [reverseFunctions (reverse functions)
          zeroFunction (first reverseFunctions)
          restFunctions (rest reverseFunctions)]
      (fn [& parameters]
        (reduce #(%2 %1) (apply zeroFunction parameters) restFunctions)))))

(defcheck solution-dab4d9d7
  (fn [& fs] (fn a [& xs] (loop [fs (vec fs), res xs] (if (empty? fs) (first res) (recur (pop fs) (list (apply (peek fs) res))))))))

(defcheck solution-dacdf96d
  (fn my-comp [& functions]
    (let [fs (reverse functions)]
      (fn [& args]
        (loop [result (apply (first fs) args)
               fs (next fs)]
          (if
           fs
            (recur ((first fs) result) (next fs))
            result))))))

(defcheck solution-dad256a8
  (fn fun-comp
    ([f g]
     (fn[ & more]
       (f (apply g more))))
    ([f g h]
     (fn[ & more]
       #_(prn more)
       (f (g (apply h more)))))))

(defcheck solution-db182fdc
  (fn s [& fs] (fn [& x] (reduce (fn [r f] (f r)) (apply (last fs) x )(rest (reverse fs))))))

(defcheck solution-db373960
  (fn [& a]
    (let [[f & r] (reverse a)]
      (fn [& x] (reduce #(%2 %1) (apply f x) r)))))

(defcheck solution-db4d270f
  (fn compose [& xs]
    (if (empty? (rest xs))
      (first xs)
      (fn [& x]
        ((first xs)
         (apply (apply compose (rest xs)) x)
         )
        )
      )
    ))

(defcheck solution-dbb92b87
  (fn my-comp [f & fs]
    (letfn [(com [g k]
              (fn [x] (g (k x))))]
      (fn [& args]
        (loop [f f, fs fs]
          (if (empty? (rest fs))
            (f (apply (first fs) args))
            (recur (com f (first fs)) (rest fs))))))))

(defcheck solution-dc16958b
  (fn [& fs]
    (let [[rf & rfs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %) (apply rf args) rfs)))))

(defcheck solution-dc6dbfb5
  (fn [& fs] (fn [& args]
               (reduce #(%2 %1)
                 (apply (last fs) args)
                 (rest (reverse fs))))))

(defcheck solution-dcf54218
  (fn [& fs]
    (fn [& args]
      (let [r (rseq (vec fs))]
        (reduce (fn [i f] (f i))
          (apply (first r) args)
          (rest r))))))

(defcheck solution-dd438a
  (fn [& args] (reduce (fn [r c] (fn [& x] (r (apply c x)))) identity args)))

(defcheck solution-dd5bb944
  (fn my-comp [& fs]
    (fn [& args]
      (reduce (fn [a-val a-fun] (a-fun a-val))
        (apply (last fs) args)
        (reverse (butlast fs))))))

(defcheck solution-dd87dd35
  (fn my-comp2 [& funcs]
    (fn [& args]
      (loop [fun (reverse funcs)
             arg args
             begn true]
        (if (= fun [])
          arg
          (if begn
            (recur (rest fun) (apply (first fun) arg) false)
            (recur (rest fun) ((first fun) arg) false) ))))))

(defcheck solution-dda3a946
  (fn [& funcs]
    (fn [& args]
      (first (reduce
               (fn [acc fu]
                 [(apply fu acc)])
               args
               (reverse funcs))))))

(defcheck solution-ddd1ae1b
  (fn co [& fns]
    (if (empty? fns)
      identity
      (fn [& x] ((apply co (butlast fns)) (apply (last fns) x))))))

(defcheck solution-de6a82d6
  (fn [& s]
    (let [x (reverse s) f (first x) r (rest x)]
      (fn [& args] (reduce #(%2 %1) (apply f args) r)))))

(defcheck solution-de79d403
  (fn f [first-fn & fns]
    (fn [& args]
      (if (empty? fns)
        (apply first-fn args)
        (first-fn (apply (apply f fns) args))))))

(defcheck solution-deba1c17
  (fn [& funs] (fn [& argz] (reduce (fn [r f] (f r))
                              (apply (first (reverse funs)) argz)
                              (rest (reverse funs))))))

(defcheck solution-dee73368
  (fn compos [& fns]
    (if (= 1 (count fns))
      #(apply (last fns) %&)
      #((apply compos (take (dec (count fns)) fns)) (apply (last fns) %&)))))

(defcheck solution-deed05d5
  #(reduce (fn [f g] (fn [& a] (g (apply f a)))) (reverse %&)))

(defcheck solution-df4ea249
  ;;(fn [& s] (let [[f & r] (reverse s)] (fn [& v] (reduce #(%2 %1) (apply f v) r))))
  (fn [& s] (reduce #(fn [& v] (%2 (apply % v))) (reverse s))))

(defcheck solution-df6319ab
  (fn mycomp [& args]
    (if (= 1 (count args))
      (fn [& arg] (apply (last args) arg))
      (fn [& arg] ((apply mycomp (take (dec (count args)) args)) (apply (last args) arg))))))

(defcheck solution-dfd0aea8
  #(reduce % (reverse %&)) (fn [g f] #(f (apply g %&))))

(defcheck solution-dfd5e78
  (fn [& x]
    (let [[f & fs] (reverse x)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-dff59ab7
  (fn [& fs]
    (reduce (fn [f g]
              (fn [& args]
                (f (apply g args))))
      identity
      fs)))

(defcheck solution-e0165228
  (fn [& fs]
    (fn [& v]
      (let [fs (reverse fs)]
        (reduce #(%2 %1)
          (apply (first fs) v)
          (rest fs))))))

(defcheck solution-e085613
  (fn [& s]
    (fn [& args]
      (reduce
        (fn [r f] (f r))
        (apply (last s) args)
        (rest (reverse s))))))

(defcheck solution-e0a42adc
  (fn my-comp [& funcs]
    (reduce
      (fn [func1 func2]
        (fn [& args] (func1 (apply func2 args))))
      funcs)))

(defcheck solution-e0d8eabb
  (fn [& f]
    (fn [& args]
      (reduce #(%2 %) (apply (last f) args) (reverse (butlast f))))))

(defcheck solution-e0ed5b50
  (fn [& fns]
    (fn [& args]
      (first
        (reduce
          #(vector (apply %2 %))
          args
          (reverse fns))))))

(defcheck solution-e0fd6bc1
  (fn [& args]
    (fn [& a2]
      (let [v (into [] args)
            r1 (apply (last v) a2)]
        (loop [f (pop v), r r1]
          (if (empty? f) r
                         (recur (pop f) ((last f) r))))))))

(defcheck solution-e1622858
  (fn [& f]
    (fn [& args]
      (let [fs (reverse f)]
        (loop [r (apply (first fs) args) fs (rest fs)]
          (if-not (empty? fs) (recur ((first fs) r) (rest fs)) r))))))

(defcheck solution-e16e1673
  (fn [& fns]
    (fn [& args]
      (first
        (reduce (fn [xs f] [(apply f xs)]) args (reverse fns))))))

(defcheck solution-e174c91a
  (fn [ & funs ]
    (fn [ & inp ]
      (loop [
             fns (next (reverse funs))
             inp (apply (last funs) inp) ]
        (if (empty? fns) inp
                         (recur (next fns) ((first fns) inp
                                            )))))))

(defcheck solution-e1ceb5fd
  (fn [& fns]
    (letfn
     [(apply-fn-to-memo [m fun] (fun m))]
      (let
       [fns (reverse fns)]
        (fn [& args]
          (reduce apply-fn-to-memo (apply (first fns) args) (rest fns)))))))

(defcheck solution-e20b3806
  (fn c [& fns]
    (fn [& args]
      (first
        (loop [value args fns fns]
          (if (empty? fns)
            value
            (recur [(apply (last fns) value)] (butlast fns))
            )
          )
        )
      )
    ))

(defcheck solution-e230ff0
  (fn [& fs]
    (fn [& xs]
      (loop [remaining fs, args xs]
        (if (empty? remaining)
          (first args)
          (recur
            (drop-last remaining)
            [(apply (last remaining) args)]))))))

(defcheck solution-e38a1b6d
  (fn [& a] (fn [& x] (reduce #(%2 %1) (apply (last a) x) (reverse (butlast a))))))

(defcheck solution-e395a91f
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [ret (apply (first fs) args)
               fs  (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))

(defcheck solution-e4acef88
  (fn [& fs]
    (fn [& args]
      (loop [fx (apply (last fs) args) f1 (butlast fs)]
        (if f1
          (recur ((last f1) fx) (butlast f1))
          fx)))))

(defcheck solution-e4c42def
  (fn xcomp
    [& f]
    (fn [& args]
      (loop [rem (reverse f), res args]
        (if (empty? rem)
          (first res)
          (recur (rest rem) [(apply (first rem) res)])
          )
        ))))

(defcheck solution-e4d78411
  (fn [& fs]
    (let [[f & more] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply f args) more)))))

(defcheck solution-e556260f
  (fn [& f] (fn [& x] (reduce #(%2 %) (apply (last f) x) (reverse (butlast f))))))

(defcheck solution-e579f5fa
  (fn my-comp
    [& fns]
    (fn [x & more]
      (let [rfns (reverse fns)
            ffn (first rfns)
            fr (apply ffn (list* x more))]
        (loop [f (rest rfns)
               r fr]
          (if (empty? f)
            r
            (recur (rest f) ((first f) r))))))))

(defcheck solution-e60c0173
  (fn [& fs]
    (let [gs (reverse fs)]
      (fn [& args]
        (reduce
          (fn [result f] (f result))
          (apply (first gs) args)
          (rest gs))))))

(defcheck solution-e6f9c6ed
  (fn [& funcs]
    (letfn [(sup [x funcs] (let [[f & restf] funcs]
                             (if (empty? restf) (apply f x) (f (sup x restf) ) ) ))]
      (fn [& x] (sup x funcs)  ) )
    ))

(defcheck solution-e70ec0c5
  ;(fn [& funcargs]
  ;    (fn [& args]
  ;     (loop [f (rest (reverse funcargs)),
  ;            result (apply (last funcargs) args)]
  ;       (if (seq f)
  ;         (recur (rest f) ((first f) result))
  ;         result))));


  (fn outer [f & fs]
    (if fs
      (fn [& x] (f (apply (apply outer fs) x)))
      (fn [& xs] (apply f xs)))))

(defcheck solution-e739f56d
  (fn [& fs]
    (fn [& xs]
      (first (reduce #(vector (apply %2 %1)) xs (reverse fs))))))

(defcheck solution-e77ad9f6
  (fn [& fs]
    (reduce
      (fn [g f]
        (fn [& xs] (g (apply f xs))))
      identity fs)))

(defcheck solution-e78d61af
  (fn my-comp [f & fs]
    (if (empty? fs)
      f
      (fn [& args]
        (f (apply (apply my-comp fs) args))))))

(defcheck solution-e7ad682c
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-e7bd4108
  (fn [& funcs]
    (let [[func & funcs] (reverse funcs)]
      (fn [& x]
        (loop [[f & fs] funcs, result (apply func x)]
          (if f
            (recur fs (f result))
            result))))))

(defcheck solution-e81e1d9f
  (fn comp'
    [& fs]
    (let [[f & fs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1)
          (apply f args)
          fs)))))

(defcheck solution-e88e4ee7
  (fn [& fs] (fn [& vs] (reduce (fn [v f] (f v)) (apply (last fs) vs) (rest (reverse fs))))))

(defcheck solution-e8b0f8ea
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& xs]
        (reduce #(apply %2 [%1])
          (apply (first fs) xs)
          (rest fs))))))

(defcheck solution-e947a214
  (fn g [& fs]
    (fn [& args]
      (loop [fs (reverse fs) args args]
        (if-let [f (first fs)]
          (recur (next fs) (list (apply f args)))
          (first args))))))

(defcheck solution-e9d5cea7
  #_(fn comp->
      [& comp-funcs]
      (let [rev-funcs (reverse comp-funcs)]
        (fn ret-closure [& closure-args]
          (let [[rightmost-func & leftmost-funcs] rev-funcs
                init-result (apply rightmost-func closure-args)]
            (reduce (fn [acc each-fn] (each-fn acc)) init-result leftmost-funcs)))))

  ;; another version using low-level recursion might look like:
  (fn comp->
    [& comp-funcs]
    (let [rev-funcs (reverse comp-funcs)]
      (fn closure-recur [& args]
        (loop [[head-func & tail-funcs] rev-funcs
               result (apply head-func args)]
          (if (nil? tail-funcs)
            result
            (recur tail-funcs ((first tail-funcs) result))))))))

(defcheck solution-ea451e51
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& ps]
        (loop [r (apply (first fs) ps) fs (next fs)]
          (if fs
            (recur ((first fs) r) (next fs))
            r))))))

(defcheck solution-eaa6338
  (fn c [f & fs]
    (if (empty? fs)
      f
      (fn [& args] (f (apply (apply c fs) args))))))

(defcheck solution-eae06ec2
  (fn [& f]  (fn [& d]  (first (reduce (fn[acc el] (list(apply el acc))) d (reverse f))))))

(defcheck solution-eb2c9b06
  (fn cmp [& functions]
    (fn [& params]
      (let [fst-res (apply (last functions) params)]
        (reduce #(%2 %1) fst-res (rest (reverse functions)))))))

(defcheck solution-eb551371
  (fn [& fs]
    (fn [& args] (let [f_list (reverse fs) f_result (apply (first f_list) args)]
                   (loop [r f_result  rf (rest f_list)]
                     (if (empty? rf) r
                                     (recur ((first rf) r) (rest rf))
                                     )
                     ))
      )
    ))

(defcheck solution-eb5b48da
  (fn [& fns]
    (fn [& params]
      (reduce (fn [result f]
                (f result))
        (apply (last fns) params)
        (reverse (butlast fns))))))

(defcheck solution-eba8806c
  (fn [& fs]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse fs))))))

(defcheck solution-ec7021a4
  (fn [& funcs]
    (fn [& args]
      (reduce #(%2 %1) (apply (last funcs) args) (reverse (butlast funcs))))))

(defcheck solution-ec76177f
  (fn [& fs]
    (fn [& xs]
      (first (reduce (fn [acc f] [(apply f acc)]) xs (reverse fs)))
      )
    ))

(defcheck solution-ecdad695
  (fn f [& fs]
    (fn [& xs]
      (loop [fs-remaining fs
             result nil]
        (if (seq? fs-remaining)
          (recur (butlast fs-remaining) (cond
                                          (nil? result) (apply (last fs-remaining) xs)
                                          :else ((last fs-remaining) result)))
          result)))))

(defcheck solution-ed37da61
  (fn [& fs] (fn [& xs] (reduce #(%2 %1)
                          (apply (last fs) xs)
                          (rest (reverse fs))))))

(defcheck solution-ed471543
  (fn [& fs]
    (loop [f (last fs)
           r (butlast fs)]
      (if (empty? r)
        #(apply f %&)
        (recur (fn [& args]
                 ((last r) (apply f args)))
          (butlast r))))))

(defcheck solution-ed558991
  (fn [& fs] (let [rfs (reverse fs)] (fn [& args] (reduce (fn [r f] (apply f (list r))) (apply (first rfs) args) (rest rfs))))))

(defcheck solution-ed9e4d79
  (fn [& funcs]
    (let [funcs (reverse funcs)]
      (fn [& args] (reduce #(%2 %1) (apply (first funcs) args) (rest funcs))))))

(defcheck solution-edfeff84
  (fn [& funcs]
    (let [comp-fns (fn [f1 f2]
                     (fn [& args] (f1 (apply f2 args))))]
      (reduce comp-fns funcs))))

(defcheck solution-ee10919f
  (fn [& fs] (reduce #(fn [& x] (% (apply %2 x))) fs)))

(defcheck solution-ee1384aa
  (fn [& xs]
    (reduce #(fn [& more] (%2 (apply %1 more))) (reverse xs))))

(defcheck solution-ef450c4f
  (fn hof [& fs]
    (fn [& a]
      (reduce #(%2 %1) (apply (last fs) a) (rest (reverse fs))))))

(defcheck solution-ef516bf1
  (fn komp
    ([] (fn [n] (identity n)))
    ([fn1] (fn [& n] (apply fn1 n)) )
    ([fn1 fn2] (fn [& n]
                 (fn1 ((fn [m] (apply fn2 m)) n))))
    ([fn1 fn2 & fns] (fn [& n]
                       (fn1 ((fn [m] (apply (apply komp (conj fns fn2)) m)) n))))))

(defcheck solution-ef871c6e
  (fn compose
    [& funcs]
    (if (= 1 (count funcs))
      (first funcs)
      (fn [& args]
        ((first funcs) (apply (apply compose (rest funcs)) args))))))

(defcheck solution-efae594f
  (fn [& fns] (fn [& args] (let [fns_ (reverse fns)] (reduce (fn [r f] (f r)) (apply (first fns_) args) (rest fns_))))))

(defcheck solution-efb2ef39
  (fn mycomp [& funs]
    (fn [ & args ] (first (reduce #(vector (apply %2 %1)) args (reverse funs))))))

(defcheck solution-efd23394
  (fn [& args]
    (fn [& xs]
      (reduce
        (fn [a v] (v a))
        (apply (last args) xs)
        (reverse (butlast args))
        ))))

(defcheck solution-efd8fd38
  #(fn [& x]
     (loop [fs (drop-last %&) o (apply (last %&) x)]
       (if (empty? fs)
         o
         (recur (drop-last fs) ((last fs) o))
         )
       )
     ))

(defcheck solution-f020d7d3
  (fn mycomp
    ([f] f)
    ([f & fns] (fn [& args] (f (apply (apply mycomp fns) args))))))

(defcheck solution-f052b1c
  (fn [& funcs]
    (fn ([& seq]
         (loop [funcs (reverse funcs)
                seq seq]
           (if (empty? funcs)
             (first seq)
             (let [[head & tail] funcs]
               (recur tail [(apply head seq)]))))))))

(defcheck solution-f0e5a660
  (fn [& fs] (reduce
               (fn [m f]
                 (fn [& args] (m (apply f args))))
               fs)))

(defcheck solution-f13af903
  (fn my-comp
    [& fns]
    (fn x
      [& args]
      (reduce #(%2 %1) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-f1b5b35e
  (fn [& funs]
    (reduce (fn [a b]
              (fn [& args] (a (apply b args)))) funs)))

(defcheck solution-f1d1b953
  (fn [& funs]
    (fn [& args]
      (loop [r-fun  (reverse funs)
             c-args args]
        (let [i (apply (first r-fun) c-args)
              nr-fun (rest r-fun)]
          (if (empty? nr-fun)
            i
            (recur nr-fun [i])))))
    ))

(defcheck solution-f2118d5a
  (fn [& allFuncs]
    (let
     [funcList (reverse allFuncs),
      recurser
               (fn recurs
                 [funcs args]
                 (if
                  (empty? funcs)
                   args
                   (recurs (rest funcs) (list (apply (first funcs) args)))
                   ))]
      (fn
        [& argList]
        (first (recurser funcList argList))
        )
      )
    ))

(defcheck solution-f2d00675
  (fn [& functions]
    (letfn [(my-comp [f1 f2]
              (fn [& args] (f1 (apply f2 args))))]
      (let [composite-function (reduce my-comp (first functions) (rest functions))]
        (fn [& operands]
          (apply composite-function operands))))))

(defcheck solution-f2de9807
  (fn [& funcs]
    (fn [& v] (reduce #(%2 %1) (apply (last funcs) v) (rest (reverse funcs))) )
    ))

(defcheck solution-f34c6df2
  (fn
    ([] identity)
    ([f] f)
    ([f g & fns]
     (let [fns (reverse (list* f g fns))]
       (fn [& args]
         (let [first-res (apply (first fns) args)]
           (reduce (fn [res f] (f res)) first-res (rest fns))))))))

(defcheck solution-f36bb8f0
  (fn [& fs]
    (let [[lf & lfs] (reverse fs)]
      (fn [& args]
        (reduce #(%2 %1) (apply lf args) lfs)))))

(defcheck solution-f3b9de8d
  (fn
    ([x y] #(x (apply y %&)))
    ([x y z]
     #(x (y (apply z %&))))))

(defcheck solution-f3cb2ca
  (fn [& fns]
    (reduce (fn [f g]
              #(f (apply g %&))) fns)))

(defcheck solution-f3cb6af1
  (fn
    ([] identity)
    ([f] f)
    ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
    ([f g h]
     (fn
       ([] (f (g (h))))
       ([x] (f (g (h x))))
       ([x y] (f (g (h x y))))
       ([x y z] (f (g (h x y z))))
       ([x y z & args] (f (g (apply h x y z args))))))
    ([f1 f2 f3 & fs]
     (let [fs (reverse (list* f1 f2 f3 fs))]
       (fn [& args]
         (loop [ret (apply (first fs) args) fs (next fs)]
           (if fs
             (recur ((first fs) ret) (next fs))
             ret)))))))

(defcheck solution-f3de8d8e
  (fn [& fs]
    #(first
       (reduce (fn [res f]
                 [(apply f res)]) %& (reverse fs)))))

(defcheck solution-f43b4820
  (fn [& fns]
    (fn [& args]
      (let [[f & fs] (reverse fns)]
        (reduce #(%2 %1) (apply f args) fs)))))

(defcheck solution-f499c029
  #(let [rfns (reverse %&)]
     (fn [& args]
       (loop [val (apply (first rfns) args)
              rfns (next rfns)]
         (if rfns
           (recur ((first rfns) val) (next rfns))
           val)))))

(defcheck solution-f4d73f18
  (fn my-comp
    ([f g]
     (fn [x] (f (g x))))
    ([f g h]
     (fn
       ([x y] (f (g (h x y))))
       ([x y & args] (f (g (apply h x y args))))))))

(defcheck solution-f4f4d674
  (fn [& fs] (fn [& xs] (let [rfs (reverse fs)] (reduce #(%2 %1) (apply (first rfs) xs) (drop 1 rfs))))))

(defcheck solution-f50e222f
  (fn my-comp
    ([f] f)
    ([f & fs]
     (let [fcomp (apply my-comp fs)]
       (fn [& args] (f (apply fcomp args)))))))

(defcheck solution-f53204b7
  (fn [& fs]
    (fn [& xs]
      (let [[f & fs] (reverse fs)]
        (reduce #(%2 %1) (apply f xs) fs)))))

(defcheck solution-f55b9d13
  (fn c
    ([] identity)
    ([f] f)
    ([f g] (fn [& p] (f (apply g p))))
    ([f g & fs]
     (fn [& p] (f (apply (if (empty? fs)
                           (c g)
                           (apply c g (first fs) (rest fs)))
                    p))))))

(defcheck solution-f571e0c5
  (fn multipleFN
    [& args]
    (loop [xs args
           f (fn [x] x)]
      (if (empty? xs)
        f
        (recur (rest xs) (fn [& x] (f (apply (first xs) x))))))))

(defcheck solution-f5757a4c
  (fn [& fns]
    (fn [& args]
      (loop [fn (drop-last fns) result (apply (last fns) args)]
        (if (empty? fn)
          result
          (recur (drop-last fn) ((last fn) result)))))))

(defcheck solution-f581f6c6
  (fn [& fs]
    (fn [& fn-args]
      (loop [fs (reverse fs)
             fn-args fn-args]
        (if (empty? fs)
          (first fn-args)
          (recur (rest fs) [(apply (first fs) fn-args)]))))))

(defcheck solution-f58523ce
  (fn [& functions]
    (fn [& param]
      (first
        (reduce #(vector (apply %2 %1)) param (reverse functions))))))

(defcheck solution-f5aed22e
  (fn compose [& fns]
    (fn [& input]
      (reduce #(->> %1 (%2))
        (apply (last fns) input)
        (rest (reverse fns))))))

(defcheck solution-f5bdf032
  (fn mycomp [& fns]
    (if (= 1 (count fns))
      #(apply (first fns) %&)
      #((apply mycomp (butlast fns)) (apply (last fns) %&)))))

(defcheck solution-f5dedb73
  (fn [& fns]
    (fn [& args]
      (reduce #(%2 %1) (apply (last fns) args) (rest (reverse fns))))))

(defcheck solution-f5f6dc49
  (fn
    ([x y]
     (fn
       [& args]
       (x (apply y args))))
    ([x y z]
     (fn
       [& args]
       (x (y (apply z args)))))))

(defcheck solution-f639e2e9
  (fn [& fs]
    (fn [& xs]
      (let [revfs (reverse fs)]
        (loop [out (apply (first revfs) xs) in (rest revfs)]
          (if (empty? in) out (recur ((first in) out) (rest in))))))))

(defcheck solution-f6484380
  (fn [& fs] (fn [& xs] (first (reduce (fn [p f] (list (apply f p))) xs (reverse fs))))))

(defcheck solution-f66135e0
  (fn [& fs]
    (fn [& args]
      (first
        (reduce
          #(vector (apply %2 %1)) args (reverse fs))))))

(defcheck solution-f699cf79
  (fn [& funcs]
    (fn [& args]
      (first (reduce #(vector (apply %2 %1)) args (reverse funcs))))))

(defcheck solution-f7a6015c
  (fn comp* [f1 & fns]
    (if (nil? fns)
      f1
      (fn [& args]
        (f1 (apply (apply comp* fns)
              args ))))))

(defcheck solution-f7ad67a4
  (fn number58 [& fns]
    (fn [& xs]
      (first (reduce #(vector (apply %2 %)) xs (reverse fns))))))

(defcheck solution-f7e79fc2
  (fn [& fs]
    (fn [& args]
      (reduce (fn [arg f] (f arg))
        (apply (last fs) args)
        (reverse (butlast fs))))))

(defcheck solution-f8398e8c
  (fn compo [f & fs]
    (if (empty? fs) f
                    (fn [& xs]
                      (f (apply (apply compo fs)
                           xs))))))

(defcheck solution-f8abf870
  (fn mycomp [& fns]
    (let [f
          (fn [& args]
            (loop [fs (reverse (list* fns)) a args]
              (if (empty? fs) (first a)
                              (let [a1 (apply (first fs) a)]
                                (recur (rest fs) (list (apply (first fs) a))))))
            )] f)))

(defcheck solution-f901a290
  (fn compp [f & other-fs]
    (if (empty? other-fs) f
                          (fn [& args]
                            (f (apply (apply compp other-fs) args))))))

(defcheck solution-f945e8d6
  (fn [& input-funcs]
    (let [[first-func & funcs] (reverse input-funcs)]
      (fn [& args]
        (reduce (fn [acc f]
                  (f acc))
          (apply first-func args)
          funcs)))))

(defcheck solution-f99e7601
  (fn f-comp [f & fs]
    (if (seq fs)
      (fn [& args]
        (f (apply (apply f-comp fs) args)))
      f)))

(defcheck solution-f9a288cf
  (fn [& s]
    (fn [& z]
      (first (let [p (reverse s)]
               (loop [fun p res z]
                 (if (empty? fun)
                   res
                   (recur (rest fun)
                     (if (= 1 (count res))
                       [((first fun) (first res))]
                       [(apply (first fun) res)])))))))))

(defcheck solution-f9c2cf1a
  (fn [& f]
    (fn [& x]
      (reduce #(%2 %)
        (apply (last f) x) (rest (reverse f))))))

(defcheck solution-fa0af4da
  (fn [f & xs]
    (reduce (fn [a b]
              #(a (apply b %&)))
      f
      xs)))

(defcheck solution-fa1a1ec4
  (fn [& f]
    (fn [& a]
      (reduce #(%2 %) (apply (last f) a) (rest (reverse f))))))

(defcheck solution-fa3a4d58
  (fn compp [f & fns]
    (fn [& xs]
      (if (seq fns)
        (f (apply (apply compp fns) xs))
        (apply f xs)))))

(defcheck solution-fa6ef5f6
  (fn c [& fs]
    (fn [& args](reduce #(%2 %)
                  (apply (last fs) args)
                  (reverse (butlast fs))))))

(defcheck solution-fa90d941
  (fn [& fs]
    (fn [& args]
      (first (reduce #(list (apply %2 %1)) args (reverse fs))))))

(defcheck solution-facd5333
  (fn [& F] (let [f (reverse F)] (fn [& l] (loop [x (apply (first f) l) g (next f)] (if g (recur ((first g) x) (next g)) x))))))

(defcheck solution-fae6855d
  (fn [& fns]
    (fn [& args]
      (let [rfns (rest (reverse fns))
            f (first (reverse fns))
            seed (if (= 1 (count args))
                   (f (first args))
                   (apply f args))]
        (loop [lseq rfns acc seed]
          (if (empty? lseq)
            acc
            (recur (rest lseq) ((first lseq) acc))))))))

(defcheck solution-fb301cd9
  (fn [& fs]
    (let [fs (reverse fs)]
      (fn [& args]
        (loop [gs (next fs)
               res (apply (first fs) args)]
          (if (seq gs)
            (recur (next gs) ((first gs) res))
            res))))))

(defcheck solution-fb5a02dc
  (fn compose [& fs] (if (= (count fs) 1)
                       (first fs)
                       #((first fs) (apply (apply compose (rest fs)) %&)))))

(defcheck solution-fb814c62
  (fn [& fs]
    (fn [& args]
      ((fn _ [[f & r]]
         (if (empty? r)
           (apply f args)
           (f (_ r))))
       fs))))

(defcheck solution-fbe1f9c3
  (fn [& fs] (reduce (fn [g f] #(g (apply f %&))) identity fs)))

(defcheck solution-fc34e4b4
  (fn[& f]
    (fn [& args]
      (loop [ret (apply (last f) args) f1 (drop-last f)]
        (if (empty? f1)
          ret
          (recur ((last f1) ret) (drop-last f1))
          )))))

(defcheck solution-fc8a00f3
  (fn [& fs]
    (reduce (fn [acc f] (fn [& xs] (acc (apply f xs))))
      identity fs)))

(defcheck solution-fca3524c
  (fn [& fns]
    (fn [& arguments]
      (loop [funcs (reverse fns)
             args arguments]
        (if-let [s (seq funcs)]
          (recur (rest s) (list (apply (first s) args)))
          (first args))
        )
      )
    ))

(defcheck solution-fcc45bc1
  (fn c[& ffs]
    (let [[f & fs] ffs]
      (fn [& a]
        (if (nil? fs) (apply f a) (f (apply (apply c fs) a)))
        )
      )
    ))

(defcheck solution-fdbc1408
  (fn cmps [& fns]
    (letfn [(apply-rt-to-left [funlist arglist]
              (if (= (count funlist) 0)
                (first arglist)
                (apply-rt-to-left (butlast funlist)
                  (list (apply (last funlist) arglist)))))]
      (fn [& args]
        (apply-rt-to-left fns args)))))

(defcheck solution-fdcd31e7
  (fn [& f]
    (let [f (reverse f)]
      (fn [& a]
        (loop [r (apply (first f) a) f (next f)]
          (if f
            (recur ((first f) r) (next f))
            r))))))

(defcheck solution-fde54130
  (fn comp2 [& fns]
    (fn [& args]
      (loop [ret (apply (last fns) args)
             fns (rest (reverse fns))]
        (if (empty? fns)
          ret
          (recur
            ((first fns) ret)
            (rest fns)))))))

(defcheck solution-fdeb6584
  (fn [& fns]
    (fn [& args]
      (first
        (reduce
          #(vector (apply %2 %1))
          args
          (reverse fns))))))

(defcheck solution-fe4b768a
  (fn my-comp
    [& fns]
    (fn [& args]
      (let [ordered-fns (reverse fns)
            first-result (apply (first ordered-fns) args)
            remaining-fns (rest ordered-fns)]
        (reduce (fn [result fun] (fun result))
          first-result
          remaining-fns)))))

(defcheck solution-fe7b80ae
  (fn [& fns]
    (fn [& as]
      (loop [acc as fns (reverse fns)]
        (if (empty? fns)
          (first acc)
          (recur (list (apply (first fns) acc)) (rest fns))
          )
        ))
    ))

(defcheck solution-fe96796
  (fn [& fs]
    (fn [& xs]
      (loop [x (apply (last fs) xs), f (butlast fs)]
        (if (empty? f)
          x
          (recur ((last f) x) (butlast f)))))))

(defcheck solution-fe9f043
  (fn my[& f]
    (fn [& x]
      ((fn temp[flist]
         (if (empty? (rest flist)) (apply (first flist) x)
                                   ((first flist) (temp (rest flist))))) f))))

(defcheck solution-feb58bfd
  (fn [& f]
    (fn [& n]
      (reduce #(%2 %1)
        (apply (last f) n)
        (rest (reverse f))))))

(defcheck solution-fee1eff1
  (fn [& f]
    (fn [& a]
      (first (reduce #(vector (apply %2 %1)) a (reverse f))))))

(defcheck solution-feec5258
  #(fn [& x] ((reduce (fn [v f] [(apply f v)]) x (into () %&)) 0)))

(defcheck solution-ff01b8f0
  (fn compose
    ([] identity)
    ([f] #(apply f %&))
    ([f & args] #(f (apply (apply compose args) %&)))))

(defcheck solution-ff0e1a67
  (fn cmp [f & fs]
    (if fs
      #(f (apply (apply cmp fs) %&))
      f)))

(defcheck solution-ff223df7
  (fn mycomp [& fcts]
    (reduce #(fn [& args] (%1 (apply %2 args))) fcts)
    ))

(defcheck solution-ff540e3a
  (fn [& fs] (fn [& args] (let [g (apply (last fs) args)] (reduce #(%2 %1) g (reverse (drop-last fs)))))))

(defcheck solution-ff588fdd
  (fn co [& funs]
    (fn [& x]
      ((fn call[fs y]
         (if (empty? fs)
           y
           (call (butlast fs) ((last fs) y)))) (butlast funs) (apply (last funs) x)))))

(defcheck solution-ff8d3e48
  (fn _comp [& functions]
    (if (= 1 (count functions))
      (first functions)
      (fn [& args]
        ((first functions) (apply (apply _comp (rest functions)) args))))))

(defcheck solution-ffb50b07
  (fn new-comp
    [& fs]
    (fn [& args]
      (if (> (count fs) 1)
        ((apply new-comp (butlast fs)) (apply (last fs) args))
        (apply (last fs) args)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-58))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
