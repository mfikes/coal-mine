(ns coal-mine.problem-62
  (:require [coal-mine.checks :refer [defcheck-62] :rename {defcheck-62 defcheck}]
            [clojure.test]))

(defcheck solution-1095d3de
  (fn lazy [f x] (lazy-seq (cons x (lazy f (f x))))))

(defcheck solution-10bc76ae
  (fn g [f x]
    (cons x (lazy-seq (g f (f x))))))

(defcheck solution-10bc7a54
  (fn iter [f v]
    (let [inner (fn me [f v]
                  (lazy-seq
                    (let [step [(f v)]]
                      (concat step (me f (step 0))))))]
      (cons v (inner f v)))))

(defcheck solution-10d71740
  (fn x [f b](cons b (lazy-seq (x f (f b))))))

(defcheck solution-112ab6ec
  (fn my-iterate [f v]
    (lazy-seq (cons v (my-iterate f (f v))))))

(defcheck solution-1179eca7
  (fn do-iterate [f x]
    (cons x
      (lazy-seq
        (do-iterate f (f x))))))

(defcheck solution-122c514e
  (fn ![f x] (cons x (lazy-seq (! f (f x))))))

(defcheck solution-12593f10
  #(tree-seq % (juxt %) %2))

(defcheck solution-12ba6201
  (fn [f init]
    (reductions (fn [init _] (f init)) init (range))))

(defcheck solution-1313e4e3
  (fn new-iterate [x y]
    (if (number? y)
      (lazy-seq (cons y (new-iterate x (x y)))))))

(defcheck solution-1323e843
  ;(fn [f v] (map-indexed #((apply comp (repeat %1 f)) %2) (repeat v)))

  (fn i [f v]
    (lazy-cat [v] (i f (f v)))))

(defcheck solution-142c0bc6
  (fn it [f i]
    (cons i (lazy-seq (it f (f i))))))

(defcheck solution-1435cbb8
  (fn iter [f a] (cons a (lazy-seq (iter f (f a))))))

(defcheck solution-14a948db
  (fn iter [f n] (cons n (lazy-seq (iter f (f n))))))

(defcheck solution-14bd0f51
  (fn it [func i]
    (cons i (lazy-seq (it func (func i))))))

(defcheck solution-1545c684
  (fn it [f x]
    (cons x (lazy-seq (it f (f x))))
    ))

(defcheck solution-1548bb37
  (fn z [f x] (lazy-seq (cons x (z f (f x))))))

(defcheck solution-156496ad
  (fn my-func [f x]
    (lazy-seq
      (cons x (my-func f (f x))))))

(defcheck solution-158224bb
  (fn my-iterate [f initial-arg]
    (lazy-seq (cons initial-arg
                (my-iterate f (f initial-arg))))))

(defcheck solution-1600a7fc
  (fn foo [f s]
    (cons s (lazy-seq (foo f (f s))))))

(defcheck solution-16bfa5df
  (fn k [f x] (cons x (lazy-seq (k f (f x))))))

(defcheck solution-16ed85a1
  (fn iterate-clone [f x]
    (lazy-seq (cons x (iterate-clone f (f x))))))

(defcheck solution-1754bcb7
  (fn my-fn [f i]
    (lazy-seq (cons i (my-fn f (f i))))))

(defcheck solution-175635e3
  (fn re-implement-iterate [f i]
    (lazy-seq
      (cons i (re-implement-iterate f (f i))))))

(defcheck solution-175cbc18
  (fn myiter
    [f i]
    (cons i (lazy-seq (myiter f (f i))))))

(defcheck solution-17860e3c
  (fn foo [ f x] (cons x
                   (lazy-seq  (foo  f  (f x))))))

(defcheck solution-185f7eb3
  (fn ite
    [f v]
    (lazy-seq
      (cons v (ite f (f v))))))

(defcheck solution-18fef7bf
  #(reductions (fn [acc f] (f acc)) %2 (repeat %)))

(defcheck solution-19089d63
  (fn [f x]
    (reductions (fn [a b] (f a)) x (range))))

(defcheck solution-190dfaa0
  (fn myiterate [f x] (lazy-seq (cons x (myiterate f (f x))))))

(defcheck solution-1963feab
  (fn myit[f v] (cons v (lazy-seq (myit f (f v))))))

(defcheck solution-199aa65
  (fn f [g i] (lazy-seq (cons i (f g (g i))))))

(defcheck solution-1a4950a5
  (fn iter2 [f start]
    (lazy-seq (cons start (iter2 f (f start))))))

(defcheck solution-1a84e111
  (fn m [f x]
    (lazy-seq (cons x (m f (f x))))))

(defcheck solution-1c2f7133
  (fn f1 [f v] (cons v (lazy-seq (f1 f (f v))))))

(defcheck solution-1c5a7dab
  (fn iter [f x]
    (lazy-seq (cons x (iter f (f x))))
    ))

(defcheck solution-1caac565
  (fn myit [f x]
    (cons x (lazy-seq (myit f (f x))))))

(defcheck solution-1cc7b9bb
  (fn my-iterate [f x]
    (lazy-cat [x] (my-iterate f (f x)))))

(defcheck solution-1cd6dffa
  (fn hey [f x] (cons x (lazy-seq (hey f (f x))))))

(defcheck solution-1d27c754
  (fn [f n] (reductions (fn [x _] (f x)) (repeat n))))

(defcheck solution-1d481c
  (fn my-iter [f n] (cons n (lazy-seq (my-iter f (f n))))))

(defcheck solution-1d4a3adb
  (fn my-iterate
    [f x]
    (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-1d7ae497
  (fn cust-iter [f x]
    (cons x (lazy-seq (cust-iter f (f x))))))

(defcheck solution-1d89096d
  (fn it [f v] (lazy-seq (cons v (it f (f v) )))))

(defcheck solution-1e060d82
  (fn my-iterate [f initial-value]
    (lazy-seq (cons initial-value
                (my-iterate f (f initial-value))))))

(defcheck solution-1f0ecfd1
  (fn [f x]
    (reductions
      (fn [a b] (f a))
      x
      (range))))

(defcheck solution-1f63efdf
  (fn it [f x]
    (cons x
      (lazy-seq
        (it f (f x))))))

(defcheck solution-1f767a15
  (fn iterate* [f x]
    (lazy-seq
      (cons x (iterate* f (f x))))))

(defcheck solution-1f8bcc6e
  (fn itr [f v]
    (lazy-cat [v] (itr f (f v)))))

(defcheck solution-1fac693a
  (fn i [f x]
    (lazy-seq (cons x (i f (f x))))))

(defcheck solution-1fc6fae8
  (fn __ [f n]
    (lazy-seq
      (cons n (__ f (f n))))))

(defcheck solution-208cd77c
  (fn myit [f x]
    (lazy-seq
      (cons x (myit f (f x))))))

(defcheck solution-20cfffd
  (fn it [f init]
    (cons init
      (lazy-seq
        (it f
          (f init))))))

(defcheck solution-20d92a53
  (fn my-iter [f x]
    (cons x (lazy-seq (my-iter f (f x))))))

(defcheck solution-21520761
  #(reductions (fn [i _] (%1 i)) %2 (range)))

(defcheck solution-21ac9b66
  (fn self[f x] (lazy-seq (cons x (self f (f x))))))

(defcheck solution-22dcbfd
  (fn fero [f st]
    (lazy-seq (cons st (fero f (f st))))))

(defcheck solution-233f6193
  (fn my-iter
    [f x]
    (cons x
      (lazy-seq
        (my-iter f (f x))))))

(defcheck solution-23a7f5aa
  (fn bla [f x] (cons x (lazy-seq (bla f (f x))) ) ))

(defcheck solution-23f32595
  (fn this [f n] (cons n (lazy-seq (this f (f n))))))

(defcheck solution-23f3a0f
  (fn l [f b]
    (lazy-seq (cons b (l f (f b))))))

(defcheck solution-24aa8929
  (fn fp [f x]
    (lazy-seq
      (cons x (fp f (f x))))))

(defcheck solution-24aef568
  (fn iter [f x]
    (cons x (lazy-seq (iter f (f x))))
    ))

(defcheck solution-24dd39fb
  (fn iter[f init]
    (
     (fn seq [fun initval]
       (lazy-seq
         (cons initval
           (seq fun (fun initval))
           )
         )
       ) f init)
    ))

(defcheck solution-24f8d707
  (fn it
    [f in]
    (lazy-seq
      (cons in (it f (f in))))))

(defcheck solution-25081f1d
  (fn i [f x]
    (cons x
      (lazy-seq
        (i f (f x))))))

(defcheck solution-252443cc
  (fn g [f n] (lazy-seq (cons n (g f (f n))))))

(defcheck solution-2545684b
  (fn re-implement-iterate [f x]
    (cons x (lazy-seq (re-implement-iterate f (f x))))))

(defcheck solution-276969d1
  #(let [f %1 v (atom %2)]
     (cons %2 (repeatedly (fn [] (swap! v f))))))

(defcheck solution-28228728
  (fn name#
    [f n]
    (lazy-seq
      (cons n (name# f (f n))))))

(defcheck solution-286c7bb1
  (fn myIterate [f v]
    (lazy-seq
      (cons v (myIterate f (f v))))))

(defcheck solution-289e0fb8
  (fn i[f o] (cons o (lazy-seq (i f (f o))))))

(defcheck solution-28c364c5
  #(reductions (comp %1 (fn [a _] a)) %2 (range)))

(defcheck solution-2a6b0c6e
  (fn lazer [f i]
    (cons i (lazy-seq (lazer f (f i))))))

(defcheck solution-2a7d4c3a
  (fn itrt [f x]
    (cons x (lazy-seq (itrt f (f x))))))

(defcheck solution-2a8bf0d7
  (fn foo [f v] (lazy-seq (cons v (foo f (f v))))))

(defcheck solution-2a8ef6b3
  (fn r [f a]
    (cons a (lazy-seq (r f (f a))))))

(defcheck solution-2abacc22
  (fn i [f n] (cons n (lazy-seq (i f (f n))))))

(defcheck solution-2b8253a3
  (fn iterate* [f x]
    (cons x (lazy-seq (iterate* f (f x))))))

(defcheck solution-2bf06fe5
  (fn my-it [f n]
    (lazy-seq (cons n (my-it f (f n))))))

(defcheck solution-2c38a166
  (fn f [g n] (cons n (lazy-seq (f g (g n))))))

(defcheck solution-2c455fb9
  (fn iter [f x]
    (cons x (lazy-seq (iter f (f x))))
    ))

(defcheck solution-2cbd9be6
  (fn [f x] (reductions (fn [a b] (f a)) x (range))))

(defcheck solution-2cf4949e
  (fn g [f v] (cons v (lazy-seq (g f (f v))))))

(defcheck solution-2dc7959
  (fn iter [f start]
    (lazy-seq
      (cons start (iter f (f start))))))

(defcheck solution-2e7ec73a
  (fn i [f x] (lazy-seq (cons x (i f (f x))))))

(defcheck solution-2e7f23b0
  (fn it [f v] (cons v (lazy-seq (it f (f v))))))

(defcheck solution-2e88673c
  (fn it [f v](lazy-seq (cons v (it f (f v))))))

(defcheck solution-2e91e2c5
  (fn ffr ([f x] (cons x (ffr f x (list x))))
    ([f x res] (let [nres (cons (f (first res)) res)]
                 (lazy-seq (cons (f (first res)) (ffr f x nres)))))))

(defcheck solution-2eb808dd
  (fn i [f v]
    (lazy-seq (cons v (i f (f v))))))

(defcheck solution-2eec6bc6
  (fn my-iterate [f initial]
    (lazy-seq (cons initial
                (my-iterate f (f initial))))))

(defcheck solution-2eecba65
  (fn j [f x] (lazy-seq (cons x (j f (f x))))))

(defcheck solution-2f03bc07
  (fn [f x]
    (reductions (fn [a _] (f a))
      x
      (range))))

(defcheck solution-2f0cef50
  (fn iter [f start] (cons start (lazy-seq (iter f (f start))))))

(defcheck solution-2f4cdeae
  (fn iter [func x]
    (lazy-seq (cons x (iter func (func x))))))

(defcheck solution-2fb9d9ea
  (fn __ [f v] (lazy-seq (cons v (__ f (f v))))))

(defcheck solution-2fcbbafc
  (fn foo [fun val]
    (cons val (lazy-seq (foo fun (fun val))))))

(defcheck solution-3084bc96
  (fn myiter [f x]
    (cons x
      (lazy-seq (myiter f (f x))))))

(defcheck solution-308cd7b1
  (fn myiterate [f x]
    (cons x (lazy-seq (myiterate f (f x))))
    ))

(defcheck solution-30976b8c
  (fn _iterate
    [f x]
    (lazy-seq
      (cons x (_iterate f (f x))))))

(defcheck solution-30d79e9
  (fn foo [f x]
    (cons x (lazy-cat (foo f (f x))))))

(defcheck solution-30fe9bb5
  (fn it [f v]
    (cons v
      (lazy-seq (it f (f v))))))

(defcheck solution-3163032c
  (fn reiterate [f x] (cons x (lazy-seq (reiterate f (f x))))))

(defcheck solution-317157b4
  (fn it[f x] (lazy-seq (cons x (it f (f x))))))

(defcheck solution-31bddd
  (fn my-iterate [f x] (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-31c537a
  (fn it [f i]
    (lazy-seq (cons i (it f (f i))))
    ))

(defcheck solution-31e59936
  (fn gen-seq [f init] (lazy-seq (cons init (gen-seq f (f init))))))

(defcheck solution-327a63ab
  (fn __ [f n]
    (lazy-seq
      (cons n
        (__ f (f n))))))

(defcheck solution-32921e75
  (fn iterate' [f x]
    (cons x (lazy-seq (iterate' f (f x))))))

(defcheck solution-32a5f139
  (fn itr [f x] (cons x (lazy-seq (itr f (f x))))))

(defcheck solution-33461e29
  (fn my-iterate [f v]
    (cons
      v
      (lazy-seq (my-iterate f (f v))))))

(defcheck solution-33672ec7
  (fn it [f n]
    (lazy-cat [n (f n)] (map f (rest (it f n))))))

(defcheck solution-337b80d6
  (fn iter [f s]
    (cons s (lazy-seq (iter f (f s))))))

(defcheck solution-3399bcba
  (fn reiterate [f x]
    (cons x (lazy-seq (reiterate f (f x))))))

(defcheck solution-339bbb69
  (fn it [f x]
    (cons x (lazy-seq (map f (it f x))))))

(defcheck solution-33cf487
  (fn my-iterate [f x]
    (cons x
      (lazy-seq (my-iterate f (f x))))))

(defcheck solution-34462943
  (fn iterator
    [f v]
    (lazy-seq
      (cons v (iterator f (f v))))))

(defcheck solution-345ffc47
  (fn s [a b] (cons b (lazy-seq (s a (a b))))))

(defcheck solution-3473719a
  (fn own-iter [f x]
    (cons x (lazy-seq (own-iter f (f x))))))

(defcheck solution-348b1f0b
  (fn iter
    [f v]
    (cons v (lazy-seq (iter f (f v ))))))

(defcheck solution-348bea76
  (fn f [g x] (lazy-cat [x] (f g (g x)))))

(defcheck solution-34c1460a
  (fn it [f x]
    (reductions (fn [y dummy] (f y)) x (range)) ))

(defcheck solution-3509ca1
  (fn my [f x] (lazy-seq (cons x (my f (f x))))))

(defcheck solution-35458f6a
  (fn iterate--lazy [f x]
    (lazy-seq (cons x (iterate--lazy f (f x))))))

(defcheck solution-35886dca
  (fn my-iterate [fnn value]
    (lazy-cat [value]
      (my-iterate fnn (fnn value)))))

(defcheck solution-3621955a
  (fn myiterate [f v]
    (lazy-seq (cons v (myiterate f (f v))))))

(defcheck solution-36a6753c
  (fn ! [f v] (lazy-seq (cons v (! f (f v))))))

(defcheck solution-36fa4e67
  (fn it [f i] (lazy-seq (cons i (it f (f i))))))

(defcheck solution-370a8bc7
  #(reductions(fn[x _](% x))%2(range)))

(defcheck solution-37242516
  (fn itr [f v] (lazy-seq (cons v (itr f (f v))))))

(defcheck solution-378ab930
  (fn i [f x]
    (lazy-seq
      (cons x (i f (f x)))
      )
    ))

(defcheck solution-37df569b
  (fn my-iterate [f x] (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-382673a4
  (fn iter [f x](cons x (lazy-seq (iter f (f x))))))

(defcheck solution-3859e267
  (fn lz [f i]
    (cons i  (lazy-seq (lz f (f i))))))

(defcheck solution-39c40a9f
  (fn iter [fnc, elem]
    (lazy-seq (cons elem (iter fnc (fnc elem))))
    ))

(defcheck solution-3a4a6c1c
  (fn f [o x] (cons x (lazy-seq (f o (o x))))))

(defcheck solution-3a7857cb
  (fn i
    [f x]
    (lazy-seq
      (cons x (i f (f x))))))

(defcheck solution-3b820445
  (fn itter[f x]
    (lazy-seq (cons x (itter f (f x))))))

(defcheck solution-3b96839f
  (fn iter
    [fun init]
    (lazy-seq
      (cons init
        (iter fun (fun init))))))

(defcheck solution-3bd6c6b5
  (fn i [f s] (lazy-cat [s (f s)] (i f (f (f s))))))

(defcheck solution-3bfbecaa
  (fn y [f x] (lazy-seq (cons x (y f (f x))))))

(defcheck solution-3bfd9420
  (fn eka
    ([op x]
     (lazy-seq
       (cons x (eka op (op x)))))
    ))

(defcheck solution-3c6f0849
  (fn foo [f x]
    (cons x (lazy-seq (foo f (f x))))))

(defcheck solution-3cc85ccd
  (fn it [f i] (cons i (lazy-seq (it f (f i))))))

(defcheck solution-3d7c1234
  (fn reit [f x] (cons x (lazy-seq (reit f (f x))))))

(defcheck solution-3da7a861
  (fn foo [f x] (cons x (lazy-seq (foo f (f x))))))

(defcheck solution-3dadf332
  (fn myiterate [f s]
    (lazy-cat (list s) (myiterate f (f s)))))

(defcheck solution-3dc94d80
  (fn ITERATE [f x]
    (cons x
      (lazy-seq (ITERATE f (f x))))))

(defcheck solution-3dd9f3ba
  (fn l [f n] (cons n (lazy-seq (l f (f n))))))

(defcheck solution-3e59399
  (fn my-iterate [f x0]
    (lazy-seq
      (cons x0
        (my-iterate f (f x0))))))

(defcheck solution-3f26aa62
  (fn myiterate[f init]
    (cons init
      (lazy-seq (myiterate f (f init))))))

(defcheck solution-3f70d2ce
  (fn f [g x] (lazy-seq (cons x (f g (g x))))))

(defcheck solution-4005e0b3
  (fn r [f %]
    (cons % (lazy-seq (r f (f %))))))

(defcheck solution-401ade1d
  (fn iter [f x]
    (lazy-seq
      (cons x
        (iter f (f x))))))

(defcheck solution-4033b0f0
  (fn my-iter [f seed]
    (cons seed (lazy-seq (my-iter f (f seed))))))

(defcheck solution-403d037
  (fn r [f v]
    (lazy-cat (list v) (lazy-seq (r f (f v))))))

(defcheck solution-40fb4e72
  #(reductions (fn [c _] (% c)) %2 (range)))

(defcheck solution-4146aade
  (fn it [f el]
    (cons el (lazy-seq (it f (f el))))))

(defcheck solution-414bd72f
  (fn z [f i]
    (lazy-seq (cons i (z f (f i))))))

(defcheck solution-4172f7f7
  (fn i [f x]
    (lazy-seq
      (let [val (f x)]
        (cons x (i f val))))))

(defcheck solution-42b881a6
  #((fn f [n] (cons n (lazy-seq (f (% n))))) %2))

(defcheck solution-431cfa16
  (fn iter [f init]
    (cons init (lazy-seq (iter f (f init))))))

(defcheck solution-462ef53d
  (fn MI [f x]
    (lazy-seq (concat (list x) (MI f (f x))))))

(defcheck solution-46519a9e
  (fn iter [f init] (lazy-seq (cons init (iter f (f init))) ) ))

(defcheck solution-467c4fec
  (fn my-iterate [f i] (lazy-seq (cons i (my-iterate f (f i))))))

(defcheck solution-46ce9b5a
  (fn myiter [f x]
    (cons x (lazy-seq (myiter f (f x))))))

(defcheck solution-47bd54c5
  (fn f [func fe] (lazy-seq (cons fe (f func (func fe))))))

(defcheck solution-47e8ed74
  (fn apply-seq [fun value]
    (letfn [(apply-fun [fun value]
              (let [current (fun value)]
                (cons current (lazy-seq (apply-fun fun current)))))]
      (cons value (apply-fun fun value)))))

(defcheck solution-48013fc1
  (fn trte [f n]
    (cons n (lazy-seq (trte f (f n))))))

(defcheck solution-4805aae6
  (fn iterate' [f x]
    (lazy-seq (cons x (iterate' f (f x))))))

(defcheck solution-48a8864e
  (fn fx [f x]
    (lazy-seq
      (cons x (fx f (f x))))))

(defcheck solution-48ccf4c8
  (fn [f base]
    (let [aux
          (fn step [curr]
            (lazy-seq
              ((fn[]
                 (cons curr (step (f curr)))))))]
      (aux base))))

(defcheck solution-48d125df
  (fn iter' [f x] (lazy-seq (cons x (iter' f (f x))))))

(defcheck solution-49b82ca4
  (fn x [f s] (lazy-seq (cons s (x f (f s))))))

(defcheck solution-49e1c944
  (fn iterate* [f x]
    (lazy-cat [x] (iterate* f (f x)))))

(defcheck solution-49f41bc4
  (fn itr [f x]
    (lazy-seq (concat [x] (itr f (f x))))
    ))

(defcheck solution-4af68d5c
  (fn [f n]
    (letfn [(foo [f n]
              (lazy-seq (cons n
                          (foo f (f n)))))]
      (foo f n))))

(defcheck solution-4b9cd2fa
  (fn I [f x]
    (cons x (lazy-seq (I f (f x))))))

(defcheck solution-4bd7f01b
  (fn it[ f x]
    (lazy-seq (cons x (it f ( f x)))) ))

(defcheck solution-4c6470ad
  (fn myiter [f x]
    (lazy-seq
      (cons x (myiter f (f x))))))

(defcheck solution-4ca9a03d
  (fn iter [f x]
    (cons x
      (lazy-seq (iter f
                  (f x))))))

(defcheck solution-4ce7d648
  (fn my-iterate [f i]
    (lazy-seq (cons i (my-iterate f (f i))))))

(defcheck solution-4d0b45a9
  (fn[fnc arg]
    (letfn [(my-iterate [fnc arg]
              (cons arg
                (lazy-seq
                  (my-iterate fnc (fnc arg)))))]
      (my-iterate fnc arg))))

(defcheck solution-4d8eea8d
  (fn my-iterator [f x]
    (lazy-seq (cons x (my-iterator f (f x))))))

(defcheck solution-4e7a47e
  (letfn [(myiterate [f x]
            (lazy-seq
              (cons x (myiterate f (f x)))))]
    myiterate
    ))

(defcheck solution-4efa4d96
  (fn iter [f x] (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-4fc18d74
  (fn my-iterate [f seed]
    (lazy-seq (cons seed (my-iterate f (f seed))))))

(defcheck solution-4fe374b2
  (fn ls [f x] (cons x (lazy-seq (ls f (f x))))))

(defcheck solution-4ff34f8d
  (fn ff [f n]
    (lazy-seq
      (cons n (ff f (f n))))))

(defcheck solution-501d7a1e
  (fn ex62
    [f x]
    (cons x (lazy-seq (ex62 f (f x))))))

(defcheck solution-504de64b
  (fn itr [f x] (let [n (f x)] (lazy-seq (cons x (itr f n))))))

(defcheck solution-50624414
  (fn r [f x] (cons x (lazy-seq (r f (f x))))))

(defcheck solution-5159ec46
  (fn my-iterate [func x]
    (cons x (lazy-seq (my-iterate func (func x))))))

(defcheck solution-516183b6
  (fn c [f x]  (cons x (lazy-seq ( c f (f x) )))))

(defcheck solution-51f19d5f
  (fn itr [f init]
    (lazy-seq (cons init (itr f (f init))))))

(defcheck solution-521b23
  (fn my-iter [f x]
    (lazy-seq
      (cons x (my-iter f (f x))))))

(defcheck solution-52d206b3
  (fn ! [f x]
    (cons x (lazy-seq
              (! f (f x))))))

(defcheck solution-52de30f
  (fn [func startValue] (reductions #(%2 %1) startValue (repeat func))))

(defcheck solution-52e576d4
  (fn i [f x]
    (lazy-cat [x] (i f (f x)))))

(defcheck solution-549a2ebb
  (fn itit [f x0] (lazy-seq (cons x0 (itit f (f x0))))))

(defcheck solution-549c064
  (fn my-iter [f x]
    (lazy-seq (cons x (my-iter f (f x))))))

(defcheck solution-549f31a4
  (fn my_iterate
    ([f init]
     (cons init (lazy-seq (my_iterate f (f init)))))))

(defcheck solution-54a0b07c
  (fn my-iterate [f x]
    (lazy-seq
      (cons x (my-iterate f (f x))))))

(defcheck solution-54ca3c1e
  (fn r[f v](lazy-seq(cons v(r f(f v))))))

(defcheck solution-55b3aa12
  (fn [f x] (map #(reduce (fn [a b] (b a) ) x (repeat % f) ) (range) ) ))

(defcheck solution-5635d6e9
  (fn iterate* [f v]
    (cons v (lazy-seq (iterate* f (f v))))))

(defcheck solution-5674bc44
  (fn it [op seed]
    (lazy-seq (cons
                seed
                (it
                  op
                  (op seed))))))

(defcheck solution-5676a76d
  (fn itr [f x]
    (lazy-seq
      (cons x (itr f (f x))))))

(defcheck solution-568e1f2c
  (fn itr8 [f x] (lazy-seq (cons x (itr8 f (f x))))))

(defcheck solution-578c3bea
  (fn myint [f x] (lazy-seq (cons x (myint f (f x))) )))

(defcheck solution-589092d1
  (fn fun [f n]
    (cons n (lazy-seq (fun f (f n))))))

(defcheck solution-593e5828
  (fn iter1 [f x]
    (cons x (lazy-seq (iter1 f (f x))))))

(defcheck solution-5958f598
  (fn iter [f x]
    (lazy-seq (cons x (iter f (f x))))))

(defcheck solution-5a507be5
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-5a881996
  (fn ITER [f x]
    (cons x (lazy-seq (ITER f (f x))))))

(defcheck solution-5ac46e16
  (fn _iterate [f arg]
    (lazy-seq (cons arg (_iterate f (f arg))))))

(defcheck solution-5acfbd40
  (fn it [f x] (lazy-seq (cons x (it f (f x))))))

(defcheck solution-5aeadd1d
  (fn it [f init] (cons init (lazy-seq (it f (f init))))))

(defcheck solution-5b1468d8
  (fn my-iterate
    [f start]
    (cons start (lazy-seq (my-iterate f (f start))))
    ))

(defcheck solution-5b6082ff
  (fn my-iterate [f a]
    (lazy-seq
      (cons a (my-iterate f (f a))))))

(defcheck solution-5bc824ed
  (fn my-iterate [func initial]
    (lazy-seq (cons initial (my-iterate func (func initial))))))

(defcheck solution-5d035c92
  (fn itrt [f x] (cons x (lazy-seq (itrt f (f x))))))

(defcheck solution-5d36a88e
  (fn i [f a]
    (lazy-seq
      (cons a (i f (f a))))))

(defcheck solution-5d4958f3
  (fn my-iterate [f x]
    (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-5daa4956
  (fn rec [f x]
    (lazy-seq
      (cons x (rec f (f x))))))

(defcheck solution-5dda4293
  (fn iter [f n]
    (cons n (lazy-seq (iter f (f n))))))

(defcheck solution-5e02127
  (fn my-iterate [f ini]
    (lazy-seq (cons ini (my-iterate f (f ini))))))

(defcheck solution-5e95b21d
  (fn uj [f & s] (lazy-seq (cons (first s) (uj f (f (first s)))))))

(defcheck solution-5f0bae25
  (fn iter [f s]
    (cons s (lazy-seq (iter f (f s))))))

(defcheck solution-5f410ab7
  (fn iter [func in]
    (lazy-seq (cons in (iter func (func in))))))

(defcheck solution-5fac9914
  (fn my-iter [f x] (cons x (lazy-seq (my-iter f (f x))))))

(defcheck solution-5fcab2d0
  (fn [f x0]
    (cons
      x0
      (map
        (fn [n]
          ( (apply comp (take n (repeat f))) x0 )
          )
        (map #(inc %) (range))
        )
      )
    ))

(defcheck solution-5fe34aa8
  (fn ! [f v]
    (lazy-seq
      (cons v (! f (f v))))))

(defcheck solution-601e55b5
  (fn iter
    [f x] (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-6044777
  (fn iter [f init]
    (lazy-seq (cons init (iter f (f init))))))

(defcheck solution-60472e9e
  (fn iter [f initial]
    (lazy-seq (cons initial (iter f (f initial))))))

(defcheck solution-60a16537
  (fn [f x] (reductions #(%2 %) x (repeat f))))

(defcheck solution-61d444e
  (fn myiter [f init]
    (let [rslt (f init)]
      (cons init (lazy-seq (myiter f rslt))))))

(defcheck solution-61ffafde
  (fn iter-seq
    [f x]
    (lazy-seq (cons x (iter-seq f (f x))))))

(defcheck solution-62052e44
  (fn iterfun [f v]
    (lazy-seq (cons v (iterfun f (f v))))))

(defcheck solution-623301ef
  (fn f-iter [f x]
    (lazy-seq (cons x (f-iter f (f x))))))

(defcheck solution-62f90f68
  (fn l [f i]
    (lazy-seq
      (cons i (l f (f i))))))

(defcheck solution-636d207c
  (fn myiter [f v]
    (lazy-seq (cons v (myiter f (f v))))))

(defcheck solution-639e5bc9
  (fn it [f x]
    (cons x
      (lazy-seq (it f (f x))))))

(defcheck solution-63a546cc
  (fn f' [f x] (lazy-seq (cons x (f' f (f x))))))

(defcheck solution-64248311
  (letfn [(iter2 [f x]
            (cons x (lazy-seq (iter2 f (f x)))))]
    iter2))

(defcheck solution-644e1e9c
  (fn iter2 [f x]
    (cons x (lazy-seq (iter2 f (f x))))))

(defcheck solution-647f7c9
  (fn m [f x]
    (cons x (lazy-seq (m f (f x))))))

(defcheck solution-64c7f6dc
  (fn iter
    ([f x]
     (concat [x] (lazy-seq (iter f (f x)))))))

(defcheck solution-6578835
  (fn my-iterate [f x]
    (lazy-seq
      (cons x (my-iterate f (f x))))))

(defcheck solution-6598974a
  (fn
    [f x]
    (letfn [(r [v] (lazy-seq (cons v (r (f v)))))]
      (r x))))

(defcheck solution-65bcf70a
  (fn iterate*
    [f x]
    (cons x (lazy-seq (iterate* f (f x))))))

(defcheck solution-65d0c498
  (fn prob62
    [f x]
    (lazy-seq (cons x (prob62 f (f x))))))

(defcheck solution-6627143c
  (fn [f x]
    (cons x
      (let [a (atom x)]
        (repeatedly #(swap! a f))))))

(defcheck solution-668b8006
  (fn my-iterate [f x]
    (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-669c769b
  (fn [f v]
    (letfn [(g [a] (lazy-seq (cons a (g (f a)))))]
      (g v))))

(defcheck solution-67097ac
  (fn custom-iterate
    [f init]
    (cons init
      (lazy-seq (custom-iterate f (f init))))))

(defcheck solution-6817537b
  (fn [f x]
    (map (fn [n]
           (loop [n n
                  fx x]
             (if (zero? n) fx
                           (recur (dec n) (f fx))))
           )
      (range))))

(defcheck solution-69970113
  (fn myiterate [f n]
    (lazy-seq (cons n (myiterate f (f n))))))

(defcheck solution-699e7ecd
  (fn func [f x]
    (cons x (lazy-seq (func f (f x))))))

(defcheck solution-69afb2b0
  (fn iterate2 [f s]
    (cons s (lazy-seq (iterate2 f (f s))))))

(defcheck solution-69b00242
  (fn iter
    [f start]
    (->> start
      (f)
      (iter f)
      (cons start)
      (lazy-seq))))

(defcheck solution-69c5cc22
  (fn _iterate [f init]
    (lazy-seq (cons init (_iterate f (f init))))))

(defcheck solution-69f910d
  (fn iterat [f x] (cons x ((fn iterate* [f x] (lazy-seq (cons (f x) (iterate* f (f x))))) f x))))

(defcheck solution-6a7f44ac
  #(reductions (fn [a _] (% a)) (repeat %2)))

(defcheck solution-6a835111
  (fn my-iterate [f init]
    (lazy-seq
      (cons init (my-iterate f (f init))))))

(defcheck solution-6a8db733
  (fn ! [f i]
    (cons i (lazy-seq (! f (f i))))))

(defcheck solution-6a92e076
  (fn iter [f i]
    (reductions
      (fn [a _] (f a))
      i (repeat i))))

(defcheck solution-6b021233
  (fn f [g v] (lazy-seq (cons v (f g (g v))))))

(defcheck solution-6b3396e4
  (fn [func x]
    (letfn [(prod [f y]
              (lazy-seq (cons y (prod f (f y)))))]
      (prod func x))))

(defcheck solution-6c355d54
  (fn m [f x]
    (lazy-seq
      (cons x (m f (f x))))))

(defcheck solution-6cafc850
  (fn my-iterate [f x]
    (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-6ce4c215
  (fn it [f x] (lazy-cat [x] (it f (f x)))))

(defcheck solution-6d555c72
  (fn foo [f x]
    (lazy-seq
      (cons x (foo f (f x))))))

(defcheck solution-6dcaee05
  (fn [f n]
    (letfn ((sub [f n]
              (let [r (f n)]
                (lazy-seq
                  `(~r ~@(sub f r))))))
      (lazy-seq `(~n ~@(sub f n))))))

(defcheck solution-6e048f09
  (fn f [x y] (lazy-cat [y] (f x (x y)))))

(defcheck solution-6e311578
  (fn my-iterate [f seed] (lazy-seq (cons seed (my-iterate f (f seed))))))

(defcheck solution-6e385f55
  (fn my-i
    [f x]
    (cons x (lazy-seq (my-i f (f x))))))

(defcheck solution-6e557e2a
  (fn go [f v]
    (cons v
      (lazy-seq
        (go f (f v))))))

(defcheck solution-6f08ac57
  (fn x [f i]
    (cons i (lazy-seq
              (x f (f i))
              )
      )))

(defcheck solution-6f3d3d5e
  (fn ff [f x] (cons x (lazy-seq (ff f (f x))))))

(defcheck solution-6f82f6e7
  (fn reiter [f x]
    (cons x (lazy-seq (reiter f (f x))))))

(defcheck solution-6fe2cfe4
  (fn self [f x] (lazy-seq (cons x (self f (f x))))))

(defcheck solution-7020d61b
  (fn my-iter [f x]
    (lazy-seq
      (cons x (my-iter f (f x))))))

(defcheck solution-702d4597
  (fn it*
    [f x]
    (lazy-seq
      (cons x (it* f (f x))))))

(defcheck solution-702d4737
  (fn myiterate [f i] (cons i (lazy-seq (myiterate f (f i))))))

(defcheck solution-70eb0ca9
  (fn itr [f x]
    (lazy-seq
      (cons x (itr f (f x))))))

(defcheck solution-71092916
  (fn myiter [f x] (cons x (lazy-seq (myiter f (f x)))) ))

(defcheck solution-7110e5a6
  (fn p [f x] (lazy-seq (cons x (p f (f x))))))

(defcheck solution-7151e1ca
  (fn i [f x]
    (lazy-seq
      (cons x (i f (f x))))))

(defcheck solution-71fc08c6
  (fn my-iterate [f value]
    (cons value (lazy-seq (my-iterate f (f value))))))

(defcheck solution-72166bc8
  (fn my-i [func, param]
    (let [my-i-f (fn [p, c]
                   (if(zero? c)
                     p
                     (recur (func p), (dec c))))]
      (map #(my-i-f param %) (range)))))

(defcheck solution-72bca92b
  (fn it [f x]
    (lazy-seq (cons x (it f (f x))))))

(defcheck solution-73024074
  (fn my-iter [f x] (lazy-seq (cons x (my-iter f (f x))))))

(defcheck solution-733317b3
  (fn my-iterate [f x]
    (lazy-cat [x] (my-iterate f (f x)))))

(defcheck solution-739ddee3
  #(letfn [(g [f x] (cons x (lazy-seq (g f (f x)))))] (g % %2)))

(defcheck solution-74295b50
  (fn aa [x y] (lazy-seq (cons y (aa x (x y))))))

(defcheck solution-74361cdb
  (fn it [f x]
    (cons x (lazy-seq (it f (f x))))))

(defcheck solution-74f048d0
  (fn my-iterate [f z] (lazy-seq (cons z (my-iterate f (f z))))))

(defcheck solution-754f2149
  (fn it [f ini]
    (lazy-seq
      (cons ini (it f (f ini))))))

(defcheck solution-756ce0cf
  (fn [f x]
    (letfn [(step [f x]
              (lazy-seq
                (cons
                  x
                  (step f (f x)))))]
      (step f x))))

(defcheck solution-75b1c457
  (fn my-iter
    [fun seed]
    (cons seed
      (lazy-seq
        (my-iter fun (fun seed))))))

(defcheck solution-768acd17
  (fn self [f n]
    (cons n (lazy-seq (self f (f n))))))

(defcheck solution-76c0a93f
  (fn ! [f v] (cons v (lazy-seq (! f (f v))))))

(defcheck solution-76c78b3
  (fn itr
    [f x]
    (cons x (lazy-seq (itr f (f x))))))

(defcheck solution-76f2aefd
  (fn my-iterate [f x]
    (cons x (lazy-seq (my-iterate f (apply f [x]))))))

(defcheck solution-77153761
  (fn iterate* [f x] (lazy-seq  (cons x (iterate* f (f x))))))

(defcheck solution-77aa469d
  (fn i [f x] (lazy-cat [x] (i f (f x)))))

(defcheck solution-77b4ec0b
  (fn zz [f i]
    (lazy-seq (cons i (zz f (f i))))))

(defcheck solution-77d330c6
  (fn g [f x] (cons x (lazy-seq (g f (f x))))))

(defcheck solution-78092458
  (fn my-iterate [f x]
    (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-780bb970
  (fn myIterate
    [f x]
    (let [y (f x)]
      (lazy-seq
        (cons x (myIterate f y))))))

(defcheck solution-780f7800
  (fn iter [f init]
    (cons init (lazy-seq (iter f (f init))))))

(defcheck solution-781b3afd
  (fn[f x](
            map-indexed
            (fn[idx itm](
                          loop[iter 0 ex x](
                                             if(= iter idx) ex
                                                            (recur (inc iter) (f ex))
                                                            )
                                           ))
            (range)
            )))

(defcheck solution-781bf83
  (fn y [f x]
    (lazy-seq (cons x (y f (f x)) ))
    ))

(defcheck solution-78e21312
  (fn itr [f v]
    (cons v (lazy-seq (itr f (f v))))))

(defcheck solution-7940c1e
  (fn iter [f x]
    (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-795037a8
  (fn do-iterate [f i]
    (lazy-seq
      (cons i
        (do-iterate f (f i))))))

(defcheck solution-7964a1c5
  (fn iter [f x]
    (lazy-seq (cons x (iter f (f x))))))

(defcheck solution-79b1890f
  (fn iterate2 [f init]
    (lazy-seq
      (cons init
        (iterate2 f (f init))))))

(defcheck solution-79c6ba74
  (fn fx [f x]
    (cons x (lazy-seq (fx f (f x))))))

(defcheck solution-7a07c34d
  (fn i [f x]
    (cons x (lazy-seq (i f (f x))))))

(defcheck solution-7a6f1a62
  (fn it [f i]
    (cons i (lazy-seq ( it f (f i))))))

(defcheck solution-7b20a0c2
  (fn iter [f init]
    (lazy-seq
      (cons
        init
        (iter f (f init))))))

(defcheck solution-7b55bc36
  (fn it [f v]
    (lazy-seq
      (cons v (it f (f v))))))

(defcheck solution-7bf2d842
  (fn fun [f x](cons x (lazy-seq (fun f (f x))))))

(defcheck solution-7bff1d6c
  (fn f
    [func init]
    (cons init (lazy-seq (f func (func init))))))

(defcheck solution-7c8c9966
  ; interesting: (fn it[f init] (reductions (fn [res new] (f res)) init (repeat 1)))
  (fn it [f init] (lazy-seq (cons init (it f (f init))))))

(defcheck solution-7c97a1c1
  (fn iter1 [f n] (lazy-seq (cons n (iter1 f (f n))
                              ))))

(defcheck solution-7cb1f4cb
  #(map-indexed (fn [n x] ((apply comp (repeat n %1)) %2)) (repeat 1)))

(defcheck solution-7cc840e6
  (fn new-iter [f x] (lazy-seq (cons x (new-iter f (f x))))))

(defcheck solution-7d7e77c1
  (fn foo [f n] (lazy-seq (cons n (foo f (f n))))))

(defcheck solution-7dd2228a
  (fn iter [f v] (cons v (lazy-seq (iter f (f v))))))

(defcheck solution-7e345bc3
  (fn me [f x] (cons x (lazy-seq(me f (f x))))))

(defcheck solution-7e86a780
  (fn i [f v]
    (let [rec (fn ri [f v]
                (let [r (f v)]
                  (lazy-cat (list r) (ri f r))))]
      (cons v (rec f v)))))

(defcheck solution-7eb2ab02
  (fn iter-ate [f x] (cons x (lazy-seq (iter-ate f (f x))))))

(defcheck solution-7eb598aa
  (fn it [f i]
    (lazy-seq (cons i (it f (f i))))))

(defcheck solution-7ed1a4bd
  (fn myiter [f v]
    (lazy-seq (cons v (myiter f (f v))))))

(defcheck solution-7ef4be76
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-8071d4c2
  (fn my-iterate [fnc x]
    (cons x (lazy-seq (my-iterate fnc (fnc x))))))

(defcheck solution-80b1d5b6
  (fn iterate' [f init]
    (cons init
      (lazy-seq (iterate' f (f init))))))

(defcheck solution-80fee917
  (fn iter [f v]
    (cons v (lazy-seq (iter f (f v))))))

(defcheck solution-811de389
  (fn g [f n]
    (cons n (lazy-seq (g f (f n))))))

(defcheck solution-81269047
  (fn z [f start]
    (cons start (lazy-seq (z f (f start))))))

(defcheck solution-819e3002
  (fn k[f init](lazy-seq (cons init (k f (f init))))))

(defcheck solution-821e9a7b
  #(reductions (fn [i _] (% i)) %2 (range)))

(defcheck solution-822982cb
  (fn F [f x] (lazy-seq (cons x (F f (f x))))))

(defcheck solution-82762508
  (fn i [f v]
    (cons v
      (lazy-seq (i f (f v))))))

(defcheck solution-83fb7c2c
  (fn itr [f x] (lazy-cat [x] (itr f (f x) ) )))

(defcheck solution-843e121a
  (fn my-iterate
    [f init-val]
    (lazy-seq
      (cons init-val (my-iterate f (f init-val))))))

(defcheck solution-84a114e0
  (fn i [f val] (lazy-seq (cons val (i f (f val))))))

(defcheck solution-853409ca
  (fn f[g i]`(~i ~@(lazy-seq (f g (g i))))))

(defcheck solution-85c4c20a
  (fn [f x]
    (map (fn [i]
           (reduce (fn [v f] (f v))
             x
             (repeat i f)))
      (range))))

(defcheck solution-86357e2
  (fn it [f x]
    (cons x (lazy-seq (it f (f x))))))

(defcheck solution-866f2b97
  (fn o
    ([f v]
     (cons v (lazy-seq (o f (f v)))))))

(defcheck solution-86f75a14
  (fn it [f v]
    (cons v (lazy-seq (it f (f v))))))

(defcheck solution-876aed56
  (fn my-iterate [fun init] (cons init (lazy-seq (my-iterate fun (fun init))))))

(defcheck solution-87748536
  (fn iter [f x]
    (lazy-cat [x] (iter f (f x)))
    ))

(defcheck solution-8807c63f
  (fn it
    (
     [f n]
     (lazy-seq (cons n (it f n nil)))
     )
    (
     [f n _]
     (lazy-seq (cons (f n) (it f (f n) nil)))
     )
    ))

(defcheck solution-8812c55e
  (fn go [f x]
    (cons x (lazy-seq (go f (f x))))))

(defcheck solution-8859e8b0
  (fn __ [f x] (cons x (lazy-seq (__ f (f x))))))

(defcheck solution-896f643d
  #(letfn [(generate [init update-fn]
             (lazy-seq
               (cons init
                 (generate (update-fn init) update-fn))))]
     (generate %2 %1)))

(defcheck solution-8a3a2651
  (fn my-lazy-seq [f x] (lazy-seq (concat [ x] (my-lazy-seq f (f x))))))

(defcheck solution-8a46c8fb
  (fn new-iter [f x]
    (cons x (lazy-seq (new-iter f (f x))))))

(defcheck solution-8a5fd853
  #(reductions (fn [x _] (% x)) %2 (range)))

(defcheck solution-8a7cef8f
  (fn t[f x] (lazy-seq (cons x (t f (f x))))))

(defcheck solution-8a858f23
  (fn foo [f a]
    (lazy-seq (cons a  (foo  f (f a) )))
    ))

(defcheck solution-8ac1362d
  (fn myiterate [f v]
    (let [newval (f v)]
      (lazy-seq (cons v (myiterate f newval))))))

(defcheck solution-8b124752
  (fn iter [f, x] (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-8b6365bd
  (fn my-iterate [f val]
    (cons val (lazy-seq (my-iterate f (f val))))))

(defcheck solution-8b8920ed
  (fn iterate' [f x]
    (cons
      x
      (lazy-seq (iterate' f (f x))))))

(defcheck solution-8bb992a0
  (fn foo [a b] (lazy-seq (cons b (foo a (a b))))))

(defcheck solution-8bd1353c
  (fn funcseq [afunc val]
    (lazy-seq (cons val (funcseq afunc (afunc val))))))

(defcheck solution-8bd1663f
  (fn myIter
    [pred x]
    (cons x (lazy-seq (myIter pred (pred x))))))

(defcheck solution-8c1a930e
  (fn [fnc x]

    (cons x
      ((fn ite [fcn x]
         (cons (fcn x) (lazy-seq (ite fcn (fcn x)))))
       fnc x))))

(defcheck solution-8c3d3efe
  (fn re [f n]
    (cons n (lazy-seq (re f (f n))))

    ))

(defcheck solution-8d67549f
  (fn it [f val]
    (lazy-seq (cons val (it f (f val))))))

(defcheck solution-8d67c920
  (fn it [f,v]
    (lazy-seq (cons v (it f (f v))))
    ))

(defcheck solution-8d7def2b
  (fn it [f x] (cons x (lazy-seq (it f (f x))))))

(defcheck solution-8e4fd032
  (fn my-iterate [f x]
    (lazy-seq
      (cons
        x
        (my-iterate f (f x))))))

(defcheck solution-8eab3a54
  (fn [f n](letfn [(itr [x] (lazy-seq (cons x (itr (f x)))))] (itr n))))

(defcheck solution-8eedc5e
  (fn [f i] (cons i (map #((apply comp (repeat (inc %) f)) i) (range)))))

(defcheck solution-8f0b1ea7
  (fn f [g x]
    (lazy-seq (cons x
                (f g (g x))))))

(defcheck solution-8f227be1
  (fn f [g x] (cons x (lazy-seq (f g (g x))))))

(defcheck solution-8f44612
  (fn s[f i] (lazy-seq (cons i (s f (f i))))))

(defcheck solution-8f9bd787
  (fn iter [f arg] (cons arg (lazy-seq (iter f (f arg))))))

(defcheck solution-903e357f
  #(letfn [(ff
             ([] (ff %2))
             ([n] (lazy-seq (cons n (ff (%1 n))))))]
     (ff)))

(defcheck solution-905f99bf
  (fn iterate- [f init] (cons init (lazy-seq (iterate- f (f init))))))

(defcheck solution-906453d9
  #(reductions (fn [a _] (%1 a)) %2 (range)))

(defcheck solution-90f99757
  (fn [f x] (letfn [(itit [f x]
                      (cons x
                        (lazy-seq (itit f (f x)))))]
              (itit f x))))

(defcheck solution-91177a8a
  (fn t [f y] (cons y (lazy-seq (t f (f y))))))

(defcheck solution-9285c1dc
  (fn iter
    [f x]
    (lazy-seq (cons x (iter f (f x))))))

(defcheck solution-92bfacbd
  (fn fn2[f x]
    (cons x (lazy-seq (fn2 f (f x))))))

(defcheck solution-930fcf6
  (fn miterate [fun x]
    (cons x (lazy-seq (miterate fun (fun x))))))

(defcheck solution-93121831
  (fn iter [f i]
    (lazy-seq (cons i (iter  f (f i))))))

(defcheck solution-932587bd
  (fn my-iterate [f x]
    (lazy-seq
      (cons
        x
        (my-iterate f (f x))))))

(defcheck solution-9391887e
  (fn itr [f i] (cons i (lazy-seq (itr f (f i))))))

(defcheck solution-94363b14
  (fn a [f n] (lazy-seq (cons n (a f (f n))))))

(defcheck solution-948103fe
  (fn iter[op x]
    (lazy-seq
      (cons x (iter op (op x))))))

(defcheck solution-94d2c400
  (fn my-iterate [f prev]
    (cons prev
      (lazy-seq
        (my-iterate f (f prev))))))

(defcheck solution-95a00dd4
  (fn i [f n]
    (lazy-cat [n] (i f (f n)))))

(defcheck solution-95ea0977
  (fn itr [f, x]
    (cons x (lazy-seq (itr f (f x))))))

(defcheck solution-9649315f
  (fn iter [f x]
    (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-965f3fa2
  (fn iterate2 [f x] (cons x (lazy-seq (iterate2 f (f x))))))

(defcheck solution-96b9c65a
  (fn [f v]
    (map #(%1 v)
      (reductions (fn [fv _] (comp f fv)) identity (range)))))

(defcheck solution-96d49934
  (fn my-iter [f x] (cons x  (lazy-seq (my-iter f (f x))))))

(defcheck solution-96fa0bcf
  (fn my-iterate [f x]
    (lazy-seq
      (cons x (my-iterate f (f x))))))

(defcheck solution-975ac986
  (fn myiterate [f init]
    (cons init
      (lazy-seq
        (myiterate f (f init))))))

(defcheck solution-97878019
  (fn ls [f arg]
    (cons arg (lazy-seq (ls f (f arg))))))

(defcheck solution-97e0672a
  (fn i [f v]
    (cons
      v
      (lazy-seq (i f (f v))))))

(defcheck solution-988231d9
  (fn new-iterate [f x]
    (lazy-seq
      (cons x (new-iterate f (f x))))))

(defcheck solution-99facc44
  (fn [f i]
    (let [apply-f-n-times (fn [n x]
                            (if (= 0 n)
                              x
                              (recur (dec n) (f x))))]
      (map-indexed #(apply-f-n-times %1 %2) (repeat i)))))

(defcheck solution-9a2d0b8c
  (fn myit [f x] (cons x (lazy-seq (myit f (f x))))))

(defcheck solution-9a3b8a1b
  (fn this-f [func init]
    (cons init
      (lazy-seq (this-f func (func init))))
    ))

(defcheck solution-9a5696db
  #(reductions(fn[v x] (% v)) %2 (repeat[""] )))

(defcheck solution-9a5f886f
  (fn iter [fun start]
    (cons start (lazy-seq (iter fun (fun start))))))

(defcheck solution-9aebfef0
  (fn iter [f x]
    (cons x
      (lazy-seq
        (iter f (f x))))))

(defcheck solution-9b0b3c9f
  (fn [f x]
    (letfn [(go-seq [y]
              (lazy-seq
                (cons y (go-seq (f y)))
                ))] (go-seq x))))

(defcheck solution-9b0b5389
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-9b51dbb0
  (fn fun [f x] (cons x (lazy-seq (fun f (f x))))))

(defcheck solution-9b8a1248
  (fn foo [f v]
    (lazy-seq (cons v (foo f (f v))))))

(defcheck solution-9bb32d23
  (fn iterate' [f x] (cons x (lazy-seq (iterate' f (f x))))))

(defcheck solution-9c0a4eb4
  (fn [f s]
    (letfn [(my-seq [f s]
              (lazy-seq (cons s (my-seq f (f s)))))]
      (my-seq f s))))

(defcheck solution-9c908ed7
  (fn iter[f, i]
    (lazy-seq
      (cons i (iter f (f i))))))

(defcheck solution-9cd2ec53
  (fn foo [f x] (lazy-seq (cons x (foo f (f x))))))

(defcheck solution-9d8b5b4b
  (fn iter8 [f x] (cons x (lazy-seq (iter8 f (f x))))))

(defcheck solution-9da94fbc
  (fn i [f v0] (lazy-seq (cons v0 (i f (f v0))))))

(defcheck solution-9dde9864
  (fn iterate_ [f x]
    (lazy-seq (cons x (iterate_ f (f x))))))

(defcheck solution-9e8b6995
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x))))))

(defcheck solution-9ea0959d
  (fn -iterate [the-fn the-value]
    (let [result (the-fn the-value)] (cons the-value (lazy-seq (-iterate the-fn result)) ))))

(defcheck solution-9f90652c
  (fn soln [f init]
    (lazy-seq
      (cons init (soln f (f init))))))

(defcheck solution-9fd05596
  (fn myIterate
    [fun init]
    (lazy-seq
      (cons init (myIterate fun (fun init))))))

(defcheck solution-a05fec3c
  (fn lazy-f [f x]
    (lazy-seq (concat [x] (lazy-f f (f x))))))

(defcheck solution-a074f3cd
  (fn [f n] (reductions (fn [a _] (f a)) (repeat n))))

(defcheck solution-a08c426d
  (fn [f x] (letfn [(ite [le]
                      (lazy-seq(cons le (ite (f le) ) ))) ]
              (ite x) ) ))

(defcheck solution-a09c65d9
  (fn rec [f seed]
    (cons seed (lazy-seq (rec f (f seed))))))

(defcheck solution-a0f0ad60
  (fn iterate- [f init]
    (cons init
      (lazy-seq
        (iterate- f (f init))))))

(defcheck solution-a1928c42
  (fn
    [f x]
    (letfn [(iter
              [x]
              (lazy-seq
                (cons x (iter (f x)))))]
      (iter x))))

(defcheck solution-a1928c68
  (fn [f x]
    (letfn [(xx [y]
              (cons y (lazy-seq (xx (f y)))))]
      (xx x))))

(defcheck solution-a208932a
  (fn my-iterate [f x] (->> (my-iterate f (f x))
                         (cons x)
                         lazy-seq)))

(defcheck solution-a208e13a
  (fn self [f a] (lazy-cat [a] (self f (f a)))))

(defcheck solution-a245d2d9
  (fn my-it [f x] (cons x (lazy-seq (my-it f (f x))))))

(defcheck solution-a276414f
  (fn r [f s]
    (lazy-seq
      (cons s (r f (f s))))))

(defcheck solution-a2848dba
  (fn [f x] (reductions #(do %2 (f %1)) x (range))))

(defcheck solution-a28c3564
  (fn iter [f x]
    (cons x
      (lazy-seq (iter f (f x))))))

(defcheck solution-a3162a7a
  (fn t [f v]
    (cons v (lazy-seq (t f (f v))))))

(defcheck solution-a370063a
  (fn iter [func value]
    (cons value (lazy-seq (iter func (func value))))))

(defcheck solution-a406af7
  (fn my-iter [f init]
    (cons init
      (lazy-seq
        (my-iter f (f init))))))

(defcheck solution-a431ae94
  (fn i[f x] (cons x (lazy-seq (i f (f x))))))

(defcheck solution-a4378c86
  (fn f[ff v]
    (cons v (lazy-seq (f ff (ff v))))))

(defcheck solution-a4bb6eb0
  (fn iiterate [f x]
    (cons x (lazy-seq (iiterate f (f x))))))

(defcheck solution-a4e5d2cb
  (fn new-iterate
    [f n]
    (cons n (lazy-seq (new-iterate f (f n))))))

(defcheck solution-a52952f8
  (fn iter [fun initval]
    (lazy-cat [initval] (iter fun (fun initval)))))

(defcheck solution-a62a52c4
  (fn [fin xin]
    ((fn [f f1 x] (f f f1 x))
     (fn [f f1 x] (cons x (lazy-seq (f f f1 (f1 x)))))
     fin
     xin)))

(defcheck solution-a6f4546f
  (fn itt [f x]
    (cons x (lazy-seq (itt f (f x))))))

(defcheck solution-a7852ab1
  (fn [f a] (reductions (fn [b _] (f b)) (repeat a))))

(defcheck solution-a7a2b460
  (fn newIterate [f val]
    (cons val (lazy-seq (newIterate f (f val))))))

(defcheck solution-a80d37cf
  (fn my-iterate [f x]
    (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-a85cbd73
  (fn itr
    [f v]
    (cons v (lazy-seq (itr f (f v))))))

(defcheck solution-a8968de3
  (fn iter [fun seed]
    (lazy-seq (cons seed (iter fun (fun seed))))))

(defcheck solution-a90f0043
  (fn i [f p]
    (lazy-seq
      (cons p (i f (f p))))))

(defcheck solution-a916af3a
  (fn my-iterate
    [f x]
    (cons x
      (lazy-seq
        (my-iterate f (f x))))))

(defcheck solution-a91e33e3
  (fn iter [func x]
    (->> (repeat x)
      (map-indexed vector)
      (map (fn [[ind val]] ((apply comp (take ind (repeat func))) val))))
    ))

(defcheck solution-aa081c73
  (fn [f x] (reductions #(%2 %1) x (repeat f))))

(defcheck solution-aa43305a
  (fn i[f x] (cons x (lazy-seq (map f (i f x))))))

(defcheck solution-ab2f9c3f
  (fn my-iterate [func init]
    (lazy-seq
      (cons init (my-iterate func (func init))))))

(defcheck solution-ab7243c6
  (fn my_it [f x] (cons x (lazy-seq (my_it f (f x))))))

(defcheck solution-ab958ebe
  (fn f [g v] (cons v (lazy-seq (f g (g v))))))

(defcheck solution-ac076991
  (fn myit [f x]
    (lazy-seq
      (cons x (myit f (f x))))))

(defcheck solution-ac17534
  (fn ! [f x]
    (cons x (lazy-seq (! f (f x) ) ))
    ))

(defcheck solution-ac54dd5e
  (fn my-lz [f lv]
    (lazy-seq
      (let [nv (f lv)]
        (cons lv (my-lz f nv))))))

(defcheck solution-ac82b501
  (fn my_iterate [f x] (lazy-seq (cons x (my_iterate f (f x))))))

(defcheck solution-ac8e623b
  (fn myiterate [f x] (cons x (lazy-seq (myiterate f (f x))))))

(defcheck solution-ac934415
  (fn i [f n] (lazy-seq (cons n (i f (f n))))))

(defcheck solution-acbe0dc5
  (fn my-iter [f x]
    (lazy-seq
      (cons x
        (my-iter f (f x))))))

(defcheck solution-aceb26d
  (fn rng [f x]
    (lazy-seq (cons x (rng f (f x))))))

(defcheck solution-adb33a7c
  (fn iter [f init] (lazy-seq (cons init (iter f (f init))))))

(defcheck solution-adccb9c7
  (fn it [f v] (lazy-seq (cons v (it f (f v))))))

(defcheck solution-adf249a6
  (fn it
    [fun x]
    (lazy-seq (cons x (it fun (fun x))))))

(defcheck solution-afc8a1db
  (fn itrt [f x]
    (lazy-seq (cons x (itrt f (f x))))))

(defcheck solution-b062b862
  (fn [f iv]
    (let [tick (atom iv)]
      (cons iv (repeatedly #(swap! tick f))))))

(defcheck solution-b077147a
  (fn my-iterate [f v]
    (lazy-seq (cons v (my-iterate f (f v))))))

(defcheck solution-b14ab880
  (fn i [f a]
    (cons a (lazy-seq (i f (f a))))))

(defcheck solution-b158cb72
  (fn iter[f x] (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-b19bac41
  (fn my-iter [f n]
    (let [x n]
      (lazy-seq
        (cons x (my-iter f (f x)))))))

(defcheck solution-b1a2fa0b
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x))))))

(defcheck solution-b1d3ec00
  (fn [f x]
    (reductions #(%2 %1) x (repeat f))))

(defcheck solution-b1ebc8e0
  (fn iterate- [f x]
    (cons x
      (lazy-seq (iterate- f (f x))))))

(defcheck solution-b2244123
  (fn myiterate [f arg]
    (lazy-seq (cons arg (myiterate f (f arg))))))

(defcheck solution-b2a34cce
  (fn iter [f a]
    (cons a (lazy-seq (iter f (f a))))))

(defcheck solution-b35506a
  (fn iter [f x]
    (cons x (lazy-seq (iter f (f x))))))

(defcheck solution-b3b8eef5
  (fn my [f x]
    (cons x (lazy-seq (my f (f x))))))

(defcheck solution-b3f06118
  (fn myiterate [ f i ]
    (lazy-seq
      (cons i (myiterate f (f i))))))

(defcheck solution-b4b33e6e
  (fn r [f x]
    (lazy-seq (cons x (r f (f x))))))

(defcheck solution-b4cb3b02
  #(letfn [(i [x] (lazy-seq (cons x (i (%1 x)))))] (i %2)))

(defcheck solution-b4d6fc5a
  #(reductions (fn [i _] (%1 i)) (repeat %2)))

(defcheck solution-b5a2f2
  (fn it [f i]
    (cons i (lazy-seq (it f (f i))))))

(defcheck solution-b67eef26
  (fn iterate' [f val]
    (cons val (lazy-seq (iterate' f (f val))))))

(defcheck solution-b6aea4fe
  (fn intera [f x]
    (cons x (lazy-seq (intera f (f x))))))

(defcheck solution-b71d4ef7
  (fn i [f x]
    (lazy-seq
      (cons x (i f (f x))))))

(defcheck solution-b75e3d8d
  (fn my-iterate [f startval]
    (lazy-seq
      (cons startval (my-iterate f (f startval))))))

(defcheck solution-b7c54620
  (fn [f_ ini_]
    (letfn [(myite [f ini]
              (lazy-seq (cons ini (myite f (f ini)))))]
      (myite f_ ini_)
      )))

(defcheck solution-b7d28f33
  (fn m [f x] (
                lazy-seq
                (cons x (m f (f x))))))

(defcheck solution-b911231
  (fn re [f i]
    (cons i (lazy-seq (re f (f i))))))

(defcheck solution-ba3796e0
  #(reductions (fn [x f] (f x)) %2 (repeat %1)))

(defcheck solution-bae8f90c
  (fn iterate2 [f x]
    (lazy-seq (cons x (iterate2 f (f x))))
    ))

(defcheck solution-bb317d6b
  (fn ! [f x] (cons x (lazy-seq (! f (f x))))))

(defcheck solution-bb68cc22
  (letfn [(it [f x] (lazy-seq (cons x (it f (f x)))))] it))

(defcheck solution-bc254ce9
  (fn it [f x]
    (lazy-seq (cons x (it f (f x))))))

(defcheck solution-bc7cebc3
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x))))))

(defcheck solution-bcd835a8
  (fn iterateX [f x] (lazy-seq (cons x (iterateX f (f x))))))

(defcheck solution-bd064a51
  (fn re [f init]
    (cons init
      (lazy-seq (re f (f init))))))

(defcheck solution-bd1c9434
  (fn it [f n]
    (cons n (lazy-seq (it f (f n))))))

(defcheck solution-bd752343
  (fn iter
    [f x]
    (cons x, (lazy-seq (iter f (f x))))))

(defcheck solution-bd86a072
  (fn iter [f y] (cons y (lazy-seq (iter f (f y))))))

(defcheck solution-bd8f9aad
  (fn func [f x] (cons x (lazy-seq (func f (f x))))))

(defcheck solution-bdb88e37
  (fn my-iterate([f, n] (cons n (lazy-seq (my-iterate f (f n)))))))

(defcheck solution-bdd8b0b0
  (fn p [f x]
    (lazy-seq (cons x (p f (f x))))))

(defcheck solution-be6aa7bc
  (fn iter [f x]
    (lazy-seq (cons x (iter f (f x))))
    ))

(defcheck solution-bee78019
  (fn [f x]
    (let [that (atom x)]
      (repeatedly #(do (let [tmp @that] (swap! that f) tmp))))))

(defcheck solution-bf2aa4b6
  (fn iter [f x] (lazy-cat [x] (iter f (f x)))))

(defcheck solution-bf4d87f6
  (fn [x y] ((fn peu [z] (cons z (lazy-seq (peu (x z))))) y)))

(defcheck solution-bf943dff
  (fn iterate-1 [f init]
    (cons init
      (lazy-seq
        (iterate-1 f (f init))))))

(defcheck solution-bf9ed67f
  (fn my-iterate
    [f x]
    (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-bfd2055b
  (fn myiter [f x]
    (cons x (lazy-seq (myiter f (f x))))
    ))

(defcheck solution-c0440ffa
  (fn my-iter [f x]
    (lazy-seq (cons x
                (my-iter f (f x))))))

(defcheck solution-c0491168
  (fn ! [f arg]
    (lazy-seq (cons arg (! f (f arg))))))

(defcheck solution-c17a6bbc
  (fn foo [f v]
    (cons v (lazy-seq (map f (foo f v))))))

(defcheck solution-c1db8ef3
  (fn iter [f i]
    (cons i
      (lazy-seq (iter f (f i))))))

(defcheck solution-c1e398cf
  (fn myIterate [f x] (cons x (lazy-seq (myIterate f (f x))))))

(defcheck solution-c27a658a
  (fn myiter [f n]
    (cons n (lazy-seq (myiter f (f n))))))

(defcheck solution-c296421a
  (fn i [f n]
    (cons n (lazy-seq (i f (f n))))))

(defcheck solution-c3461b80
  (fn [f i] (reductions #(%2 %) i (repeat f))))

(defcheck solution-c361556
  (fn g [f x]
    (lazy-seq (cons x (g f (f x))))))

(defcheck solution-c4443c2c
  (fn x [f i] (lazy-seq (cons i (x f (f i))))))

(defcheck solution-c543ffa7
  (fn it [f arg] (lazy-seq (cons arg (it f (f arg))))))

(defcheck solution-c57ad7b8
  (fn my-iterate [f x]
    (cons x (lazy-seq (my-iterate f (f x))))))

(defcheck solution-c5b89b9b
  (fn my-iterate2
    [f x]
    (lazy-seq (cons x (my-iterate2 f (f x))))))

(defcheck solution-c60c2dd9
  (fn ite [fn1 val]
    (cons val (lazy-seq (ite fn1 (fn1 val))))))

(defcheck solution-c6287911
  (fn itr [f x]
    (cons x (lazy-seq (itr f (f x))))))

(defcheck solution-c68f91d6
  (fn itr [a b](cons b (lazy-seq (itr a (a b))))))

(defcheck solution-c69d6d9c
  (fn iter [f init]
    (lazy-seq
      (cons init (iter f (f init))))))

(defcheck solution-c6b9e059
  (fn s [f x] (cons x (lazy-seq (s f (f x))))))

(defcheck solution-c6fd0857
  (fn miterate
    [f x]
    (cons x (lazy-seq (miterate f (f x))))))

(defcheck solution-c70c145c
  (fn self [f x]
    (cons x (lazy-seq (self f (f x))))))

(defcheck solution-c748ce43
  (fn itr
    [f ini]
    (cons ini
      (lazy-seq (itr f (f ini))))))

(defcheck solution-c77a8367
  (fn myiter [f s] (cons s (lazy-seq (myiter f (f s))))))

(defcheck solution-c7bf79fd
  (fn foo [op start] (lazy-seq (cons start (foo op (op start))))))

(defcheck solution-c7f68351
  (fn my-iterate [func x]
    (cons x (lazy-seq (my-iterate func (func x))))))

(defcheck solution-c8204857
  #(letfn [(iter [f x] (cons x (lazy-seq (iter f (f x)))))]
     (iter %1 %2)))

(defcheck solution-c84f71e7
  (fn generator [f x] (lazy-seq (cons x (generator f (f x))))))

(defcheck solution-c90da997
  (fn my-iter [fun init]
    (cons init (lazy-seq (my-iter fun (fun init))))))

(defcheck solution-c929ba11
  (fn [f i]
    (letfn [(my-iterate [f i] (lazy-seq (cons i (my-iterate f (f i)))))]
      (my-iterate f i))))

(defcheck solution-c9d80f10
  (fn m [f n]
    (lazy-seq (cons n (m f (f n))))))

(defcheck solution-cae3c872
  (fn g [f x]
    (lazy-seq
      (cons x (g f (f x))))))

(defcheck solution-cb8fa383
  (fn [f x]
    (map
      (fn [n]
        (loop [n n f f x x]
          (if (zero? n)
            x
            (recur (dec n) f (f x)))))
      (range)
      )))

(defcheck solution-cbe089c8
  (fn my-iter [f x]
    (lazy-seq
      (cons x (my-iter f (f x))))))

(defcheck solution-cc6215ae
  (fn ii [f x]
    (cons x (lazy-seq (ii f (f x))))))

(defcheck solution-cc62aa42
  (fn my-iterate [a-fn seed]
    (lazy-seq (cons seed (my-iterate a-fn (a-fn seed))))))

(defcheck solution-cc8501fa
  (fn my-iterate [f init-value]
    (lazy-seq (cons init-value (my-iterate f (f init-value))))
    ))

(defcheck solution-cca82c06
  (fn i [f a] (cons a (lazy-seq (i f (f a))))))

(defcheck solution-ccbac7ef
  (fn re-implement-iterate [f first]
    (lazy-seq (cons first (re-implement-iterate f (f first))))))

(defcheck solution-cd1a1e6
  (fn my-iterate
    ([fun val]
     (cons val (lazy-seq (my-iterate fun (fun val)))))
    ))

(defcheck solution-cdffaa07
  (fn my-iterate [f init]
    (cons init
      (lazy-seq
        (my-iterate f (f init))))))

(defcheck solution-ce56a25
  (fn iter [f v]
    (cons v (lazy-seq (iter f (f v))))))

(defcheck solution-cf4b2472
  (fn iter [f i]
    (cons i (lazy-seq (iter f (f i))))))

(defcheck solution-cfce5c1d
  (fn itr [f x]
    (lazy-seq (cons x (itr f (f x))))))

(defcheck solution-cfe132d9
  (fn fun [f x]
    (lazy-cat [x] (fun f (f x)))))

(defcheck solution-d0017b8a
  (fn __ [f x]
    (lazy-seq
      (cons x (__ f (f x))))))

(defcheck solution-d0187c
  (fn iter [f s]
    (lazy-seq (list* s (lazy-seq (iter f (f s)))))))

(defcheck solution-d08031cc
  (fn reiterater [f x]
    (cons x (lazy-seq (reiterater f (f x))))
    ))

(defcheck solution-d0926b1f
  #(reductions (fn [agg x] (%1 agg)) %2 (range)))

(defcheck solution-d0ad4412
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-d15d9c00
  (fn func [f x]
    (cons
      x
      (lazy-seq
        (func f (f x))))))

(defcheck solution-d18d2899
  (fn [f init]
    (letfn [(xx [ff i]
              (cons i (lazy-seq (xx ff (ff i)))))]
      (xx f init))))

(defcheck solution-d1c2d4c1
  (fn f [a b] (lazy-seq (cons b (f a (a b))))))

(defcheck solution-d1c47bfe
  (fn afn [f x] (cons x (lazy-seq (afn f (f x))))))

(defcheck solution-d1f2e1cf
  (fn iterate* [f x] (cons x (lazy-seq (iterate* f (f x))))))

(defcheck solution-d23bd123
  (fn co [f x] (cons x (lazy-seq (co f (f x))))))

(defcheck solution-d23f85f5
  (fn itr [f x] (lazy-seq (cons x (itr f (f x))))))

(defcheck solution-d28c5083
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-d346c4f3
  (fn it [f a] (cons a (lazy-seq (it f (f a))))))

(defcheck solution-d4651085
  (fn [f init]
    (letfn [(myiterate
              ([f init]
               (cons init (lazy-seq (myiterate f (f init))))))]
      (myiterate f init))))

(defcheck solution-d4a676a
  (fn a[f i] (cons i (lazy-seq (a f (f i))))))

(defcheck solution-d4c35775
  (fn ! [f i]
    (lazy-seq
      (cons i (! f (f i)))
      )
    ))

(defcheck solution-d557c61c
  (fn my-iter [f val]
    (cons val (lazy-seq (my-iter f (f val))))))

(defcheck solution-d55ce6d6
  (fn iter [f arg] (lazy-seq (cons arg (iter f (f arg))))))

(defcheck solution-d56e9fb6
  (fn ! [f n]
    (cons n (lazy-seq (!  f (f n))))))

(defcheck solution-d59ac85
  (fn it [f init]
    (cons init (lazy-seq (it f (f init))))))

(defcheck solution-d5d15a4f
  (fn i
    [f x]
    (cons x (lazy-seq (i f (f x))))))

(defcheck solution-d667d6b9
  (fn i [f v]
    (cons v (lazy-seq (i f (f v))))))

(defcheck solution-d69ab004
  (fn [f x]
    (letfn [(f-your-x [x]
              (lazy-cat [x] (f-your-x (f x))))]
      (f-your-x x))))

(defcheck solution-d6d8d64a
  (fn this [f x]
    (cons x
      (lazy-seq
        (this f (f x))))))

(defcheck solution-d76197fd
  (fn myiterate
    [f start-val]
    (cons start-val (lazy-seq
                      (myiterate
                        f (f start-val))))))

(defcheck solution-d76da194
  (fn ! [m b]
    (cons b (lazy-seq (! m (m b))))
    ))

(defcheck solution-d7715fe2
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x))))))

(defcheck solution-d781fd46
  (fn it
    [f v]

    (lazy-seq
      (cons
        v
        (it f (f v))
        )
      )
    ))

(defcheck solution-d7b847ba
  (fn iterate* [f x] (lazy-seq (cons x (iterate* f (f x))))))

(defcheck solution-d7d8d617
  (fn itr [f x] (lazy-cat [x] (itr f (f x)))))

(defcheck solution-d84c726d
  (fn my-iterate [f x]
    (cons x
      (lazy-seq (my-iterate f (f x))))))

(defcheck solution-d87e27e4
  (fn myiterate [fun x]
    (cons x (lazy-seq (myiterate fun (fun x))))))

(defcheck solution-d89c1c17
  (fn iter [f init]
    (lazy-seq
      (cons init (iter f (f init))))))

(defcheck solution-d8d0bd7c
  (letfn [(my-iter [f x]
            (cons x (lazy-seq (my-iter f (f x)))))]
    my-iter))

(defcheck solution-d91963b1
  (fn iter [f v]
    (lazy-seq (cons v (iter f (f v))))))

(defcheck solution-d937850a
  (fn re-implement-iterate [f x]
    (lazy-seq
      (cons x
        (re-implement-iterate f
          (f x ))))))

(defcheck solution-d9c3b2af
  (fn [f n]
    (for [x (range)]
      (if (zero? x)
        n
        ((apply comp (repeat x f)) n)))))

(defcheck solution-d9d7f05
  (fn myiter [func a]
    (lazy-seq
      (cons a
        (myiter func (func a))))
    ))

(defcheck solution-da3ba338
  (fn my-iterate
    [f startval]
    (lazy-seq (cons startval (my-iterate f (f startval))))))

(defcheck solution-da712348
  (fn irate [f x]
    (cons x (lazy-seq (irate f (f x))))))

(defcheck solution-da721c8c
  (fn _iterate [function value]
    (cons value (lazy-seq (_iterate function (function value))))))

(defcheck solution-da726467
  #( (fn l[x] (lazy-seq (cons x (l (% x))))) %2))

(defcheck solution-dabd7307
  (fn my-iterate [f x]
    (lazy-seq
      (cons x (my-iterate f (f x))))))

(defcheck solution-dac88d21
  (fn myIterate [f x] (lazy-seq (cons x (myIterate f (f x))))))

(defcheck solution-db169426
  (fn itr[f x]
    (cons x (lazy-seq (itr f (f x))))))

(defcheck solution-db178355
  (fn itr8 [f x]
    (cons x (lazy-seq (itr8 f (f x))))))

(defcheck solution-db1a57c8
  (fn myseq [f n] (cons n (lazy-seq (myseq f (f n))))))

(defcheck solution-db1f9769
  (fn [f x]
    (letfn
     [(my-iterate [f x]
        (lazy-seq (cons x (my-iterate f (f x)))))]
      (my-iterate f x))))

(defcheck solution-dbc6fee6
  ;(fn iter [f x]
  ;  (lazy-cat [x (f x)]
  ;            (map f (rest (iter f x)))))

  (fn iter [f x]
    (cons x
      (lazy-seq (iter f (f x))))))

(defcheck solution-dc1be860
  (fn my-iterate [f x]
    (lazy-seq
      (cons x
        (my-iterate f (f x))))))

(defcheck solution-dc6fb94d
  (fn iterate* [f x]
    (lazy-seq (cons x (iterate* f (f x))))))

(defcheck solution-dc8de8d3
  (fn g [a b] (cons b (lazy-seq (g a (a b))))))

(defcheck solution-dc92ed1e
  (fn newiterate [f x]
    (lazy-seq (cons x (newiterate f (f x))))))

(defcheck solution-dc976f37
  (fn this [f x]
    (cons x (lazy-seq (this f (f x))))))

(defcheck solution-dca8abdc
  (fn iter [f v]
    (lazy-seq
      (cons v (iter f (f v))))))

(defcheck solution-dcdc9aeb
  #(reductions (fn [a _] (% a)) %2 (range)))

(defcheck solution-dd31426b
  (fn lazySeq [x y] (lazy-seq (cons y (lazySeq x (x y))))))

(defcheck solution-dd9dd9b2
  (fn iter [f v]
    (cons v
      (lazy-seq
        (iter f (f v))))))

(defcheck solution-ddbf086f
  (fn body [f a0]
    (lazy-seq (cons a0 (body f (f a0))))))

(defcheck solution-de05c591
  (fn myiter [f start] (lazy-cat [start] (myiter f (f start)))))

(defcheck solution-de6be3ae
  (letfn [(it
            [f x]
            (cons x
              (lazy-seq (it f (f x)))))]
    it))

(defcheck solution-debb2231
  (fn itr ([pred x] (lazy-cat [x] (map pred (itr pred x))))))

(defcheck solution-debc12ef
  (fn iter
    [op result]
    (lazy-seq
      (cons
        result
        (iter op (op result))))))

(defcheck solution-debc511
  (fn yo [f x]
    (cons x (lazy-seq (yo f (f x))))))

(defcheck solution-dec77170
  (fn iter [f n]
    (lazy-seq (cons n (iter f (f n))))))

(defcheck solution-decc07fe
  (fn iter [func init] (cons init (lazy-seq (iter func (func init))))))

(defcheck solution-df0bf321
  (fn t [f x] (lazy-seq (cons x (t f (f x))))))

(defcheck solution-df2e4b57
  (fn iter [f v]
    (lazy-seq (cons v (iter f (f v))))))

(defcheck solution-df61a59d
  (fn iterate' [f i]
    (cons i (lazy-seq (iterate' f (f i))))))

(defcheck solution-df7953ab
  (fn my-iterate [f x]
    (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-dfe15104
  (fn i [f x]
    (lazy-seq
      (cons x (i f (f x))))))

(defcheck solution-dfed4172
  (fn whaeva [f n]
    (cons n (lazy-seq (whaeva f (f n))))))

(defcheck solution-e08443b8
  (fn g [f x] (lazy-cat [x] (g f (f x)))))

(defcheck solution-e0daecae
  (fn iter [f n] (lazy-seq (cons n (iter f (f n))))))

(defcheck solution-e122f50d
  (fn it [f s] (lazy-seq (cons s (it f (f s))))))

(defcheck solution-e17d0c99
  (fn iter [f current] (lazy-seq (cons current (iter f (f current))))))

(defcheck solution-e186c8ec
  (fn iter-recur [func start]
    (lazy-seq
      (let [computed (func start)]
        (cons start (iter-recur func computed))))))

(defcheck solution-e1ecfd3c
  (fn iterate* [f i]
    (lazy-seq (cons i (iterate* f (f i))))))

(defcheck solution-e23f4185
  (fn f [a b] (cons b (lazy-seq (f a (a b))))))

(defcheck solution-e2d0f722
  (fn it [f x]
    (lazy-seq
      (cons x
        (it f (f x))))))

(defcheck solution-e2f82b85
  (fn g [f x] (lazy-seq (cons x (g f (f x))))))

(defcheck solution-e31c0d13
  (fn iter [f x] (lazy-seq (cons x (iter f (f x))))))

(defcheck solution-e3250623
  (fn myite [f x]
    (cons x (lazy-seq (myite f (f x))))))

(defcheck solution-e402b3ae
  (fn q [f v]
    (cons v (lazy-seq (q f (f v))))))

(defcheck solution-e4bc98d2
  (fn my-iterate [f init]
    (lazy-seq
      (cons init (my-iterate f (f init))))))

(defcheck solution-e536e914
  (fn nxt[f x]
    (cons x
      (lazy-seq
        (nxt f (f x))
        )
      )
    ))

(defcheck solution-e563ed77
  (fn g [f i]
    (lazy-seq (cons i (g f (f i))))))

(defcheck solution-e58a5d5e
  (fn m
    [f x]
    (lazy-seq (cons x (m f (f x))) )))

(defcheck solution-e604c1e7
  (fn iterate1 [f x] (cons x (lazy-seq (iterate1 f (f x))))))

(defcheck solution-e613d204
  (fn iter
    [f x]
    (lazy-seq (cons x (iter f (f x))))))

(defcheck solution-e62a8105
  (fn it [f s]
    (lazy-cat (list s) (it f (f s)))))

(defcheck solution-e63ca1da
  (fn iter [fun val]
    (lazy-cat
      [val]
      (iter fun (fun val)))))

(defcheck solution-e66e3804
  (fn [f s]
    (let [i (fn j [p] (lazy-seq (cons p (j (f p)))))]
      (i s))))

(defcheck solution-e676a6f
  (fn it [f n]
    (lazy-seq
      (cons n (it f (f n))))))

(defcheck solution-e67e243b
  (letfn [(step [f x] (cons x (lazy-seq (step f (f x)))))] step))

(defcheck solution-e7458670
  (fn mi [f v]
    (lazy-seq
      (let [c v]
        (cons v (mi f (f v)))))))

(defcheck solution-e76ad610
  (fn iter [f i] (cons i (lazy-seq (iter f (f i))))))

(defcheck solution-e7ffe2b5
  (fn iter [f x]
    (let [nxt (f x)]
      (cons x (lazy-seq (iter f nxt)))
      )
    ))

(defcheck solution-e845f65f
  (fn iter [f, x]
    (lazy-seq
      (cons x (iter f (f x))))))

(defcheck solution-e9eecb9
  (fn i [f seed] (cons seed (lazy-seq (i f (f seed))))))

(defcheck solution-ea079b77
  (fn g [f n]
    (cons n (lazy-seq (g f (f n))))))

(defcheck solution-ea185dec
  (fn re-iterate
    [f starter]
    (lazy-seq
      (cons starter
        (re-iterate f (f starter))))))

(defcheck solution-ea5d627e
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x)))
      )
    ))

(defcheck solution-eabec9f9
  (fn iiterate
    [f x]
    (cons x (lazy-seq (iiterate f (f x))))))

(defcheck solution-eac4c37b
  (fn f [y x] (lazy-seq (cons x (f y (y x))))))

(defcheck solution-ead20377
  (fn afn [f x](lazy-seq (cons x (afn f (f x))))))

(defcheck solution-eadc6891
  (fn f [g x]
    (lazy-seq (cons x (f g (g x))))))

(defcheck solution-eb4c95d3
  (fn i [f x] (cons x (lazy-seq (i f (f x))))))

(defcheck solution-eb5ff648
  (fn my-iterate [f ival]
    (cons ival (lazy-seq (my-iterate f (f ival))))))

(defcheck solution-ebf54fbe
  (fn iter [f v]
    (lazy-seq
      (cons v (iter f (f v))))))

(defcheck solution-ec630eb2
  (fn [f x] (map #(if (= 0 %2) %1 (recur (f %1) (dec %2))) (repeat x) (range))))

(defcheck solution-eca8a039
  (fn ff[f a]
    (lazy-seq (cons a (ff f (f a))))))

(defcheck solution-ecc240f2
  (fn myiterate [f x]
    (lazy-seq
      (cons x (myiterate f (f x))))))

(defcheck solution-edbde53a
  (fn ! [f n] (cons n (lazy-seq (! f (f n))))))

(defcheck solution-ee532a6a
  (fn iter [f x]
    (lazy-seq
      (cons x (iter f (f x)))
      )
    ))

(defcheck solution-ee9bed12
  (fn iter [f v]
    (cons v (lazy-seq (iter f (f v))))))

(defcheck solution-eebc224c
  (fn [f x]
    (for [i (range)]
      ((apply comp (repeat i f)) x))))

(defcheck solution-ef3a0e3d
  (fn ite [func x]
    (cons x (lazy-seq (ite func (func x) ) ) )
    ))

(defcheck solution-ef64f76e
  (fn r [f v] (lazy-seq (cons v (r f (f v))))))

(defcheck solution-ef9679c1
  ;(fn [f x] (reductions #(%2 %1) x (repeat f)))
  (fn it [f init]
    (lazy-seq
      (cons init (it f (f init))))))

(defcheck solution-f0b59f28
  (fn this [f x]
    (cons x (lazy-seq (this f (f x))))))

(defcheck solution-f0e3638e
  ( fn [f x]
    (for [n (range)]
      ((apply comp (repeat n f) )x )
      )
    ))

(defcheck solution-f110d818
  (fn i [f x]
    (cons x (lazy-seq (i f (f x))))))

(defcheck solution-f1293802
  (fn mylazy
    [myfn n]
    (cons n (lazy-seq (mylazy myfn (myfn n)))

      )))

(defcheck solution-f16050a
  (fn iterate' [f x]
    (lazy-seq (cons x (iterate' f (f x))))))

(defcheck solution-f1bfd3a1
  (fn* lazy [f x]
    (cons x (lazy-seq (lazy f (f x))))))

(defcheck solution-f2028aa0
  (fn iterate- [f value]
    "62. Given a side-effect free function f and an initial value x
     write a function which returns an infinite lazy sequence of x, (f
     x), (f (f x)), (f (f (f x))), etc."
    (lazy-seq
      (cons value (iterate- f (f value))))))

(defcheck solution-f286858c
  (fn m [f i] (cons i (lazy-seq (m f (f i))))))

(defcheck solution-f29d0e63
  (fn [x y] ((fn itr [z] (cons z (lazy-seq (itr (x z))))) y)))

(defcheck solution-f2c88490
  (fn i [f a] (lazy-seq (cons a (i f (f a))))))

(defcheck solution-f2f29d61
  (fn reimp [f n] (cons n (lazy-seq (reimp f (f n))))))

(defcheck solution-f45f54fb
  (fn self [f x]
    (lazy-seq (cons x (self f (f x))))))

(defcheck solution-f49bf21f
  (fn iterate' [f v] (lazy-seq (cons v (iterate' f (f v))))))

(defcheck solution-f4b1d71a
  (fn ! [f x]
    (cons x (lazy-seq (! f (f x))))
    ))

(defcheck solution-f5c29078
  (fn it [f x]
    (lazy-seq
      (cons x (it f (f x))
        )
      )
    ))

(defcheck solution-f60d5649
  (fn ! [f s] (lazy-cat [s] (! f (f s)))))

(defcheck solution-f6d5a997
  (fn [f x]
    (map
      #(% x)
      (map #(apply comp identity (repeat % f))
        (range)))))

(defcheck solution-f789a34b
  (fn a [f x]
    (lazy-seq (cons x (a f (f x))))))

(defcheck solution-f7963b6f
  (fn testt [f x]
    (lazy-seq (cons x (testt f (f x))))))

(defcheck solution-f7bde0e0
  (fn my-iterate
    [f init]
    (cons init (lazy-seq (my-iterate f (f init))))))

(defcheck solution-f870eec7
  (fn iter [f v] (lazy-seq (cons v (iter f (f v))))))

(defcheck solution-f8b4230a
  #(reductions (fn [y g] (g y)) %2 (repeat %1)))

(defcheck solution-f8fcbe8a
  (fn F [f x] (cons x (lazy-seq (F f (f x))))))

(defcheck solution-f8fe2c42
  (fn [f x]
    (letfn [(iter [g y] (cons y (lazy-seq (iter g (g y)))))]
      (iter f x))))

(defcheck solution-f92249e5
  (fn my-iterate [op n]
    (lazy-seq
      (cons n
        (my-iterate op (op n))))))

(defcheck solution-f9d2b38e
  (fn [fun init] ((fn iter [x] (cons x (lazy-seq (iter (fun x))))) init)))

(defcheck solution-f9e9d768
  (fn l[f x] (cons x (lazy-seq (l f (f x))))))

(defcheck solution-fa272ff6
  (fn it [f x]
    (cons x (lazy-seq (it f (f x))))))

(defcheck solution-fa924d5b
  (fn [f x]
    ((fn my-it [i]
       (cons i (lazy-seq (my-it (f i))))) x)))

(defcheck solution-fad3ca4d
  (fn reiterate [f i]
    (lazy-seq
      (cons i (reiterate f (f i))))))

(defcheck solution-fb5b9dbe
  (fn myiterate [f x]
    (cons x (lazy-seq (myiterate f (f x))))))

(defcheck solution-fb7b5b2d
  (fn r [f i]
    (cons i (lazy-seq (r f (f i))))))

(defcheck solution-fc65d60e
  (fn it [f z] (cons z (lazy-seq (it f (f z))))))

(defcheck solution-fc88fb58
  #(cons %2
     (repeatedly
       (let [x (atom %2)]
         (fn [] (swap! x %1))))))

(defcheck solution-fc94756e
  (fn re [x y]
    (cons y (lazy-seq (re x (x y))))
    ))

(defcheck solution-fc9b19c9
  (fn i [f x]
    (cons x (lazy-seq (i f (f x))))))

(defcheck solution-fca8c488
  (fn rit
    [f n]
    (cons n
      (lazy-seq (rit f (f n)))
      )))

(defcheck solution-fcb1240c
  (fn my-iterate
    [f x]
    (lazy-seq (cons x (my-iterate f (f x))))))

(defcheck solution-fcc7dfb3
  (fn iter [f n]
    (cons n (lazy-seq (iter f (f n))))))

(defcheck solution-fd3c3ad9
  (fn itr8 [f acc]
    (cons acc
      (lazy-seq
        (itr8 f (f acc))))))

(defcheck solution-fd47a627
  (fn my-fx [f, x]
    (lazy-seq (cons x (my-fx f (f x))))))

(defcheck solution-fd540cca
  (fn ite [f s] (lazy-seq (cons s (ite f (f s))))))

(defcheck solution-fd9cab59
  (fn g[f x] (lazy-seq (cons x (g f (f x))))))

(defcheck solution-fdf107a0
  (fn iter [f x]
    (lazy-seq (cons x (iter f (f x )
                        )))))

(defcheck solution-fe7cd9f4
  (fn myiterate [f ini]
    (cons ini (lazy-seq (myiterate f (f ini))))))

(defcheck solution-ff6265f4
  (fn again [func input]
    (lazy-seq (cons input (again func (func input))))))

(defcheck solution-ff7d0aeb
  (fn [f x]
    (let [t (atom x)]
      (lazy-cat [x] (repeatedly #(swap! t f))))))